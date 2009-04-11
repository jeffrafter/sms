require 'rubygems'
require 'serialport'
require 'timeout'

module Generic

  # Simple generic wrapper around the serialport library
  class Serial

    def initialize(port)
      @read_buffer = ""
 			@device = SerialPort.new(port, 9600, 8, 1, SerialPort::NONE)
    end

    def finalize
      close 
    end

    def read
      # I don't want a timeout here, because the phone gets really busy with receipts
      char = @device.getc
      raise "Could not read data" if char.nil?
      @read_buffer << sprintf("%c", char)
      char        
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
      begin
        data.each_byte do |b|
          @device.putc(b.chr)
        end
      rescue Errno::EIO => e
        raise "Could not write data (IO: #{e.message})"
      rescue Exception => e
        raise "Could not write data (#{e.message})"
      end
    end

    def close
      @device = nil
    end
  end
end