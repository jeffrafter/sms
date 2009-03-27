require 'Win32API'

# http://www.rubyinside.com/cross-platform-ruby-serial-port-library-328.html

module Win32

  GENERIC_READ = 0x80000000
  GENERIC_WRITE = 0x40000000
  OPEN_EXISTING = 0x00000003
  FILE_FLAG_OVERLAPPED = 0x40000000
  NULL = 0x00000000
  EV_RXCHAR = 0x0001
  ERROR_IO_PENDING = 997
  ERROR_HANDLE_EOF = 38
  DCB_SIZE = 80

  class SmsError < RuntimeError; end

  class Sms

    attr_accessor :messages, :ignore_unknown_errors

    def initialize(port)
      @read_buffer = ""

      @CreateFile = Win32API.new('Kernel32', 'CreateFile', 'PLLLLLL', 'L')
      @CloseHandle = Win32API.new('Kernel32','CloseHandle', 'L', 'N')
      @ReadFile = Win32API.new('Kernel32','ReadFile','LPLPP','I')
      @WriteFile = Win32API.new('Kernel32','WriteFile','LPLPP','I')
      @SetCommState = Win32API.new('Kernel32','SetCommState','LP','N')
      @SetCommTimeouts = Win32API.new('Kernel32','SetCommTimeouts','LP','N')
      @BuildCommDCB = Win32API.new('Kernel32','BuildCommDCB', 'PP', 'N')
      @GetLastError = Win32API.new('Kernel32','GetLastError', 'V', 'N')

      # Open the device non-overlapped
      @device = create_file(port, GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL)

      # Set the speed
      set_comm_state(9600, 8, 'N', 1)

      # Need to be able to reset blocking
      set_comm_timeouts(2, 1, 1, 0, 0)

      # Keep multi-part messages until the last part is delivered
      @multipart = {}

      # Store incoming messages until they're dealt with by someone else
      @messages = []

      # Setup the command for texting and processing
      connect
    end

    def finalize
      close
    end

    def close
      hResult = @CloseHandle.call(@device) if @device
      raise "Could not close device (#{get_last_error})" if hResult == 0
      @device = nil
    end

    def command(data, term = "\r\n")
      write(data + "\r\n")
      response = wait(term)
      response.delete_if do |line|
        (line[0,6] == "+WIND:") or
        (line[0,6] == "+CREG:") or
        (line[0,7] == "+CGREG:")
      end
      response
    end

    def query(data)
      response = command(data)
      return response[0] if response.size == 2 and response[1] == "OK"
      raise "Invalid response: #{response.inspect}"
    end

    def encodings?
      command "AT+CSCS=?"
    end

    def encoding
      @encoding
    end

    def encoding=(enc)
      @encoding = enc
      case enc
        when :ascii
         command "AT+CSCS=\"ASCII\""
        when :utf8
         command "AT+CSCS=\"UTF8\""
        when :ucs2
         command "AT+CSCS=\"UCS2\""
        when :gsm
         command "AT+CSCS=\"GSM\""
        when :iso88591
         command "AT+CSCS=\"8859-1\""
      end
    end

    def sms(number, text)
      # initiate the sms, and wait for either the text prompt or an error
      command "AT+CMGS=\"#{number}\"", ["\r\n", "> "]
      begin
        # send the sms, and wait until it is accepted or rejected
        text = encode(text)
        write "#{text}#{26.chr}"
        response = wait
      rescue
        # Escape entry mode
        write 27.chr
        raise
      end
      response
    end

    def hardware
      {:manufacturer => query("AT+CGMI"),
       :model        => query("AT+CGMM"),
       :revision     => query("AT+CGMR"),
       :serial       => query("AT+CGSN") }
    end
    
    def process 
      @messages = []
      fetch_and_delete_stored_messages
    end
    
    def pin?
      not command("AT+CPIN?").include?("+CPIN: READY")
    end    
    
    def pin=(pin)
      return unless pin?
      command "AT+CPIN=#{pin}"
    end 
    
    def signal?
      query("AT+CSQ")
    end

  protected

    def wait(term = "\r\n")
      response = []
      loop do
        buffer = read_until(term)
        buffer.strip!
        next if buffer.nil? || buffer.empty?
        response << buffer

        # Check for formatted error
        if m = buffer.match(/^\+(CM[ES]) ERROR: (\d+)$/)
          number = m.captures[1].to_i rescue 0
          return response if number == 500 && @ignore_unknown_errors
          raise SmsError.new(buffer)
        end

        # Check for unformatted error
        if buffer == "ERROR"
          raise SmsError
        end

        # Check for 'OK' or prompt
        if (buffer == "OK") or (buffer == ">") or (buffer =~ /^\+CPIN: (.+)$/)
          return response
        end
      end
    end

    def parse_timestamp(data)
      # Extract the weirdo quarter-hour timezone into a regular hourly offset
      data.sub! /(\d+)$/ do |m| sprintf("%02d", (m.to_i/4)); end
      DateTime.strptime(data, "%d/%m/%Y %H:%M:%S %z")
    end

    # Encodes the message using the set encoding or, if no encoding is specified
    # returns the msg unchange
    def encode(msg)
      if (@encoding == :ascii)
        # TODO, use lucky sneaks here
        msg
      elsif (@encoding == :utf8)
        # Unpacking and repacking supposedly cleans out bad (non-UTF-8) stuff
        utf8 = msg.unpack("U*");
        packed = utf8.pack("U*");
        packed
      elsif (@encoding == :ucs2)
        ucs2 = Iconv.iconv("UCS-2", "UTF-8", msg).first
        ucs2 = ucs2.unpack("H*").join
        ucs2
      else
        msg
      end
    end

    def select_default_mailbox
      # Eventually we will select the first mailbox as the default
      result = query("AT+CPMS=?")
      boxes = result.scan(/\"(\w+)\"/).flatten #"
      mailbox = boxes.first
      command "AT+CPMS=\"#{mailbox}\""
      mailbox
    rescue
      raise RuntimeError.new("Could not select the default mailbox")
      nil
    end

    def fetch_and_delete_stored_messages
      # If there is no way to select a default mailbox we can't continue
      return unless select_default_mailbox

      # Try to read the first message from the box
      begin
        response = command("AT+CMGR=1")
      rescue
        return
      end

      # Did we find any messages
      return if response.nil? || response.empty?

      # Delete that message
      command("AT+CMGD=1")

      # Break that down
      header = response.first
      text = response[1..response.size-2].join("\n")
      validity = response.last

      # Read the header
      header = header.scan(/\"([^"]+)\"/).flatten #"
      status = header[0]
      from = header[1]
      timestamp = header[2]
      sent = DateTime.parse(timestamp)

      # Just in case it wasn't already obvious
      puts "Received message from #{from}: #{text}"
      @messages << {:from => from,
                    :text => text,
                    :created_at => sent,
                    :processed_at => Time.now}
    end

  private

    def get_last_error
      @GetLastError.Call
    end

    def create_file(file_name, desired_access, shared_mode, security_attributes,
      creation_distribution, flags_and_attributes, template_file)
      hResult = @CreateFile.Call(file_name, desired_access, shared_mode,
        security_attributes, creation_distribution, flags_and_attributes,
        template_file)
      raise "Could not open device (#{get_last_error})" if hResult < 1
      hResult
    end

    def set_comm_state(baud, data, parity, stop)
      args = "baud=#{baud} parity=#{parity} data=#{data} stop=#{stop}"
      dcb = " "*80
      hResult = @BuildCommDCB.Call(args, dcb)
      raise "Could not build dcb structure (#{get_last_error})" if hResult == 0
      hResult = @SetCommState.Call(@device, dcb)
      raise "Could not set comm state (#{get_last_error})" if hResult == 0
    end

    def set_comm_timeouts(read_interval_timeout = 3, read_total_timeout_multiplier = 3,
      read_total_timeout_constant = 2, write_total_timeout_multiplier = 3,
      write_total_timeout_constant = 2)
      hResult = @SetCommTimeouts.Call(@device, [read_interval_timeout,
        read_total_timeout_multiplier, read_total_timeout_constant,
        write_total_timeout_multiplier, write_total_timeout_constant].pack("L*"))
      raise "Could not set comm timeouts (#{get_last_error})" if hResult == 0
    end

    def read
      count = " "*4
      buffer = " "*1024
      hResult = @ReadFile.Call(@device, buffer, 1024, count, NULL)
      raise "Could not read data (#{get_last_error})" if hResult == 0
      count = count.unpack("L").first
      @read_buffer << buffer[0..(count-1)]
      hResult
    end

    def read_until(term)
      term = [term].flatten
      stop = nil
      loop do
        term.each {|t| stop = t and break if @read_buffer.index(t)}
        break if stop
        read
      end
      @read_buffer.slice!(0, @read_buffer.index(stop)+stop.size)
    end

    def write(data)
      count = " "*4
      hResult = @WriteFile.Call(@device, data, data.size, count, NULL)
      count = count.unpack("L").first
      raise "Could not write data (#{get_last_error})" if hResult == 0
      # raise "Not enough bytes written" if count != data.size
    end

    def connect
      # Echo off
      command "ATE0" rescue nil
      # Useful errors
      command "AT+CMEE=1" rescue nil
      # No notifications
      command "AT+WIND=0" rescue nil
      # Switch to text mode
      command "AT+CMGF=1"
    end
  end
end