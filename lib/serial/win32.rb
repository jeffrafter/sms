require 'Win32API'
require 'timeout'

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

  class Serial
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
    end

    def finalize
      close 
    end

    def read
      Timeout::timeout(60) do
        begin
          count = " "*4
          buffer = " "*1024
          hResult = @ReadFile.Call(@device, buffer, 1024, count, NULL)
          raise "Could not read data (#{get_last_error})" if hResult == 0
          count = count.unpack("L").first
          @read_buffer << buffer[0..(count-1)]
          hResult
        rescue Timeout::Error
          puts "Timed out reading"
        end  
      end
    end

    def read_until(term)
      Timeout::timeout(60) do
        begin
          term = [term].flatten
          stop = nil
          loop do
            term.each {|t| stop = t and break if @read_buffer.index(t)}
            break if stop
            read
          end
          @read_buffer.slice!(0, @read_buffer.index(stop)+stop.size)
        rescue Timeout::Error
          puts "Timed out reading until '#{term}'"
        end
      end
    end

    def write(data)
      count = " "*4
      hResult = @WriteFile.Call(@device, data, data.size, count, NULL)
      count = count.unpack("L").first
      raise "Could not write data (#{get_last_error})" if hResult == 0
      # raise "Not enough bytes written" if count != data.size
    end

    def close
      hResult = @CloseHandle.call(@device) if @device
      raise "Could not close device (#{get_last_error})" if hResult == 0
      @device = nil
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
  end
end