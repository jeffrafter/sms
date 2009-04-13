require 'iconv'
require 'serial/win32' if RUBY_PLATFORM.downcase.include?("mswin")
require 'serial/generic' unless RUBY_PLATFORM.downcase.include?("mswin")

class SmsError < RuntimeError; end

class Sms

  attr_accessor :messages, :ignore_unknown_errors, :max_retry

  def initialize(port)

    # Create the right device for the platform
    if RUBY_PLATFORM.downcase.include?("mswin")
      @device = Win32::Serial.new(port)
    else  
      @device = Generic::Serial.new(port)
    end  

    raise "Could not open device" unless @device

    # Sometimes commands fail, we count on that
    @max_retry = 5

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
    @device = nil
  end

  def command(data, term = "\r", attempt = 1)
    @device.write(data + "\r")
    response = wait(term)
    response.delete_if do |line|
      (line == data) or
      (line[0,6] == "+WIND:") or
      (line[0,6] == "+CREG:") or
      (line[0,7] == "+CGREG:")
    end
    response
  rescue Exception => e
    # Allow retries for device errors or busy errors
    if attempt <= @max_retry and (e.message =~ /\+CMS ERROR/ or e.message =~ /515/)       
      puts "Retrying failed command '#{data}' (#{e.message})"
      sleep(1)
      command "AT+CMGF=1"
      command data, term, attempt + 1
      return
    end
    raise "Error running command '#{data}' (#{e.message})"
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

  def sms(number, text, attempt = 1)
    puts "Preparing message"
    # Switch to text mode (should already be there, but seems to get reset)
    command "AT+CMGF=1"
    position = nil
    # initiate the sms, and wait for either the text prompt or an error
    command "AT+CMGW=\"#{number}\"", ["\r", "> "]
    begin
      # send the sms, and wait until it is accepted or rejected
      text = encode(text)
      @device.write "#{text}#{26.chr}"
      response = wait
      position = "#{response}".scan(/\+CMGW\:\s*(\d*)/).first
    rescue Exception => e
      # Escape entry mode
      @device.write 27.chr

      # Allow retries for device errors or busy errors
      if attempt <= @max_retry and (e.message =~ /\+CMS ERROR/ or e.message =~ /515/)       
        puts "Retrying failed sms to '#{number}' (#{e.message})"
        sleep(1)
        sms number, text, attempt + 1
        return
      end
      raise
    end
    puts "Sending"
    command "AT+CMGF=1"
    command "AT+CMSS=#{position}" if position
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

  def connect
    # Echo off
    command "ATE0" rescue nil
    # Useful errors
    command "AT+CMEE=1" rescue nil
    # No notifications
    command "AT+WIND=0" rescue nil
    # In general we want to be in SMS mode, but we may need to reset this
    command "AT+CMGF=1"
  end

  def wait(term = "\r")
    response = []
    loop do
      buffer = @device.read_until(term)
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
    @encoding = @encoding.to_sym rescue nil
    if (@encoding == :ascii)
      require 'lucky_sneaks/unidecoder'
      msg = LuckySneaks::Unidecoder::decode(msg)
      msg
    elsif (@encoding == :utf8)
      # Unpacking and repacking supposedly cleans out bad (non-UTF-8) stuff
      utf8 = msg.unpack("U*")
      packed = utf8.pack("U*")
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
    # Make sure we are in the right mode
    command "AT+CMGF=1"
  
    # If there is no way to select a default mailbox we can't continue
    return unless select_default_mailbox

    # Try to read the messages from the box
    begin
      response = command("AT+CMGL")
    rescue
      return
    end

    # Did we find any messages
    return if response.nil? || response.empty?

    # Process those
    loop do
      # Get the header (or OK)
      header = response.shift      
      break unless header =~ /^\+CMGL/
    
      # Break that down
      text = response.shift

      # Read the header (+CMGL: 13,"REC READ","Movistar")
      header = header.scan(/^\+CMGL\:\s*(\d*),\"(.+)\",\"(.+)\"$/).flatten #"
      number = header.shift
      status = header.shift
      from = header.shift
      
      # A CGMR will return a timestamp as well. To reduce phone traffic we will skip that and punt on the time sent to now
      sent = Time.now

      # Delete the message
      command("AT+CMGD=#{number}")

      # We only want to worry about received messages
      next unless status =~ /^REC/
      
      # Just in case it wasn't already obvious
      puts "Received message from #{from}: #{text}"

      @messages << {:from => from,
                    :text => text,
                    :created_at => sent,
                    :processed_at => Time.now}

    end    
  end
end