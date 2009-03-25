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

  class Sms
    def initialize(port)    
      @read_buffer = ""
      
      @CreateFile = Win32API.new('Kernel32', 'CreateFile', 'PLLLLLL', 'L')
      @CloseHandle = Win32API.new('Kernel32','CloseHandle', 'L', 'N')
      @ReadFile = Win32API.new('Kernel32','ReadFile','LPLPP','I')
      @WriteFile = Win32API.new('Kernel32','WriteFile','LPLPP','I')
      @SetCommState = Win32API.new('Kernel32','SetCommState','LP','N')
      @SetCommTimeouts = Win32API.new('Kernel32','SetCommTimeouts','LP','N')
      @BuildCommDCB = Win32API.new('Kernel32','BuildCommDCB', 'LP', 'N')
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
      @incoming = []          
    end  
    
    def finalize
      hResult = @CloseHandle.call(@device) if @device
      raise "Could not close device (#{get_last_error})" if hResult == 0      
    end
      
    def command(data)
      write(data)
      response = read_until("\r\n")
      response.strip!    
      # Clean up status messages
      response.delete_if do |line|
        (line[0,6] == "+WIND:") or
        (line[0,6] == "+CREG:") or
        (line[0,7] == "+CGREG:")
      end
      response
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
      raise "Could not open device (#{get_last_error})" if hResult == INVALID_HANDLE_VALUE
      hResult   
    end
  
    def set_comm_state(baud, data, parity, stop)
      args = "baud=#{baud} parity=#{parity} data=#{data} stop=#{stop}"
      dcb = nil
      hResult = @BuildCommDCB.Call(args, dcb)
      raise "Could not build dcb structure (#{get_last_error})" if hResult == 0
      hResult = @SetCommState.Call(@device, dcb)
      raise "Could not set comm state (#{get_last_error})" if hResult == 0
    end

    def set_comm_timeouts(
      read_interval_timeout = 3, 
      read_total_timeout_multiplier = 3,
      read_total_timeout_constant = 2,
      write_total_timeout_multiplier = 3,
      write_total_timeout_constant = 2)
      hResult = @SetCommTimeouts.Call(@device, [read_interval_timeout,
                                      read_total_timeout_multiplier,
                                      read_total_timeout_constant,
                                      write_total_timeout_multiplier,
                                      write_total_timeout_constant].pack("L*"))
      raise "Could not set comm timeouts (#{get_last_error})" if hResult == 0
    end 
    
    def read
      count = nil
      buffer = nil
      hResult = @ReadFile.Call(@device, buffer, 1024, count, NULL)
      raise "Could not read data (#{get_last_error})" if hResult == 0
      count = [count].unpack("L")
      @read_buffer << buffer.slice[0..count-1]
      hResult
    end
    
    def read_until(term)
      loop do
        break if @read_buffer.index(term)
        read
      end
      @read_buffer.slice!(0, buf.index(term)+term.size)     
    end
            
    def write(data)
      count = nil
      hResult = @WriteFile.Call(@device, data, data.size, count, NULL)
      count = [count].unpack("L")
      raise "Could not write data (#{get_last_error})" if hResult == 0
      raise "Not enough bytes written" if count != data.size
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

=begin  

  
  private
  
  
  INCOMING_FMT = "%y/%m/%d,%H:%M:%S%Z" #:nodoc:
  CMGL_STATUS = "REC UNREAD" #:nodoc:


  def parse_incoming_timestamp(ts)
    # extract the weirdo quarter-hour timezone,
    # convert it into a regular hourly offset
    ts.sub! /(\d+)$/ do |m|
      sprintf("%02d", (m.to_i/4))
    end
    
    # parse the timestamp, and attempt to re-align
    # it according to the timezone we extracted
    DateTime.strptime(ts, INCOMING_FMT)
  end
  
  def parse_incoming_sms!(lines)
    n = 0
    
    # iterate the lines like it's 1984
    # (because we're patching the array,
    # which is hard work for iterators)
    while n < lines.length
      
      # not a CMT string? ignore it
      unless lines && lines[n] && lines[n][0,5] == "+CMT:"
        n += 1
        next
      end
      
      # since this line IS a CMT string (an incoming
      # SMS), parse it and store it to deal with later
      unless m = lines[n].match(/^\+CMT: "(.+?)",.*?,"(.+?)".*?$/)
        err = "Couldn't parse CMT data: #{lines[n]}"
        raise RuntimeError.new(err)
      end
      
      # extract the meta-info from the CMT line,
      # and the message from the FOLLOWING line
      from, timestamp = *m.captures
      msg_text = lines[n+1].strip
      
      # notify the network that we accepted
      # the incoming message (for read receipt)
      # BEFORE pushing it to the incoming queue
      # (to avoid really ugly race condition if
      # the message is grabbed from the queue
      # and responded to quickly, before we get
      # a chance to issue at+cnma)
      begin
        command "AT+CNMA"
        
      # not terribly important if it
      # fails, even though it shouldn't
      rescue Gsm::Error
        log "Receipt acknowledgement (CNMA) was rejected"
      end
      
      # we might abort if this part of a
      # multi-part message, but not the last
      catch :skip_processing do
      
        # multi-part messages begin with ASCII char 130
        if (msg_text[0] == 130) and (msg_text[1].chr == "@")
          text = msg_text[7,999]
          
          # ensure we have a place for the incoming
          # message part to live as they are delivered
          @multipart[from] = []\
            unless @multipart.has_key?(from)
          
          # append THIS PART
          @multipart[from].push(text)
          
          # add useless message to log
          part = @multipart[from].length
          log "Received part #{part} of message from: #{from}"
          
          # abort if this is not the last part
          throw :skip_processing\
            unless (msg_text[5] == 173)
          
          # last part, so switch out the received
          # part with the whole message, to be processed
          # below (the sender and timestamp are the same
          # for all parts, so no change needed there)
          msg_text = @multipart[from].join("")
          @multipart.delete(from)
        end
        
        # just in case it wasn't already obvious...
        log "Received message from #{from}: #{msg_text.inspect}"
      
        # store the incoming data to be picked up
        # from the attr_accessor as a tuple (this
        # is kind of ghetto, and WILL change later)
        sent = parse_incoming_timestamp(timestamp)
        msg = Gsm::Incoming.new(self, from, sent, msg_text)
        @incoming.push(msg)
      end
      
      # drop the two CMT lines (meta-info and message),
      # and patch the index to hit the next unchecked
      # line during the next iteration
      lines.slice!(n,2)
      n -= 1
    end
  end
  
  
  # write a string to the modem immediately,
  # without waiting for the lock
  def write(str)
    log "Write: #{str.inspect}", :traffic
    
    begin
      str.each_byte do |b|
        @device.putc(b.chr)
      end
    
    # the device couldn't be written to,
    # which probably means that it has
    # crashed or been unplugged
    rescue Errno::EIO
      raise Gsm::WriteError
    end
  end
  
  
  # read from the modem (blocking) until
  # the term character is hit, and return
  def read(term=nil)
    term = "\r\n" if term==nil
    term = [term] unless term.is_a? Array
    buf = ""
    
    # include the terminator in the traffic dump,
    # if it's anything other than the default
    #suffix = (term != ["\r\n"]) ? " (term=#{term.inspect})" : ""
    #log_incr "Read" + suffix, :traffic
    
    begin
      timeout(@read_timeout) do
        while true do
          char = @device.getc
          
          # die if we couldn't read
          # (nil signifies an error)
          raise Gsm::ReadError\
            if char.nil?
          
          # convert the character to ascii,
          # and append it to the tmp buffer
          buf << sprintf("%c", char)
        
          # if a terminator was just received,
          # then return the current buffer
          term.each do |t|
            len = t.length
            if buf[-len, len] == t
              log "Read: #{buf.inspect}", :traffic
              return buf.strip
            end
          end
        end
      end
    
    # reading took too long, so intercept
    # and raise a more specific exception
    rescue Timeout::Error
      log = "Read: Timed out", :warn
      raise TimeoutError
    end
  end
  
  
  # issue a single command, and wait for the response
  def command(cmd, resp_term=nil, write_term="\r")
    begin
      out = ""
      
      exclusive do
        log_incr "Command: #{cmd}"
        write(cmd + write_term)
        out = wait(resp_term)
      end
    
      # some hardware (my motorola phone) adds extra CRLFs
      # to some responses. i see no reason that we need them
      out.delete ""
    
      # for the time being, ignore any unsolicited
      # status messages. i can't seem to figure out
      # how to disable them (AT+WIND=0 doesn't work)
      out.delete_if do |line|
        (line[0,6] == "+WIND:") or
        (line[0,6] == "+CREG:") or
        (line[0,7] == "+CGREG:")
      end
    
      # parse out any incoming sms that were bundled
      # with this data (to be fetched later by an app)
      parse_incoming_sms!(out)
    
      # log the modified output
      log_decr "=#{out.inspect}"
    
      # rest up for a bit (modems are
      # slow, and get confused easily)
      sleep(@cmd_delay)
      return out
    
    # if the 515 (please wait) error was thrown,
    # then automatically re-try the command after
    # a short delay. for others, propagate
    rescue Error => err
      log "Rescued (in #command): #{err.desc}"
      
      if (err.type == "CMS") and (err.code == 515)
        sleep 2
        retry
      end
      
      log_decr
      raise
    end
  end
  
  
  # proxy a single command to #command, but catch any
  # Gsm::Error exceptions that are raised, and return
  # nil. This should be used to issue commands which
  # aren't vital - of which there are VERY FEW.
  def try_command(cmd, *args)
    begin
      log_incr "Trying Command: #{cmd}"
      out = command(cmd, *args)
      log_decr "=#{out}"
      return out
      
    rescue Error => err
      log_then_decr "Rescued (in #try_command): #{err.desc}"
      return nil
    end
  end
  
  
  def query(cmd)
    log_incr "Query: #{cmd}"
    out = command cmd
  
    # only very simple responses are supported
    # (on purpose!) here - [response, crlf, ok]
    if (out.length==2) and (out[1]=="OK")
      log_decr "=#{out[0].inspect}"
      return out[0]
    
    else
      err = "Invalid response: #{out.inspect}"
      raise RuntimeError.new(err)
    end
  end
  
  
  # just wait for a response, by reading
  # until an OK or ERROR terminator is hit
  def wait(term=nil)
    buffer = []
    log_incr "Waiting for response"
    
    while true do
      buf = read(term)
      buffer.push(buf)
      
      # some errors contain useful error codes,
      # so raise a proper error with a description
      if m = buf.match(/^\+(CM[ES]) ERROR: (\d+)$/)
        log_then_decr "!! Raising Gsm::Error #{$1} #{$2}"
        raise Error.new(*m.captures)
      end
    
      # some errors are not so useful :|
      if buf == "ERROR"
        log_then_decr "!! Raising Gsm::Error"
        raise Error
      end
    
      # most commands return OK upon success, except
      # for those which prompt for more data (CMGS)
      if (buf=="OK") or (buf==">")
        log_decr "=#{buffer.inspect}"
        return buffer
      end
    
      # some commands DO NOT respond with OK,
      # even when they're successful, so check
      # for those exceptions manually
      if m = buf.match(/^\+CPIN: (.+)$/)
        log_decr "=#{buffer.inspect}"
        return buffer
      end
    end
  end
  
  
  def exclusive &blk
    old_lock = nil
    
    begin
      
      # prevent other threads from issuing
      # commands TO THIS MODDEM while this
      # block is working. this does not lock
      # threads, just the gsm device
      if @locked_to and (@locked_to != Thread.current)
        log "Locked by #{@locked_to["name"]}, waiting..."
      
        # wait for the modem to become available,
        # so we can issue commands from threads
        while @locked_to
          sleep 0.05
        end
      end
      
      # we got the lock!
      old_lock = @locked_to
      @locked_to = Thread.current
      log_incr "Got lock"
    
      # perform the command while
      # we have exclusive access
      # to the modem device
      yield
      
    
    # something went bang, which happens, but
    # just pass it on (after unlocking...)
    rescue Gsm::Error
      raise
    
    
    # no message, but always un-
    # indent subsequent log messages
    # and RELEASE THE LOCK
    ensure
      @locked_to = old_lock
      Thread.pass
      log_decr
    end
  end
  
  
  
  
  public
  
  
  # call-seq:
  #   hardware => hash
  #
  # Returns a hash of containing information about the physical
  # modem. The contents of each value are entirely manufacturer
  # dependant, and vary wildly between devices.
  #
  #   modem.hardware => { :manufacturer => "Multitech".
  #                       :model        => "MTCBA-G-F4", 
  #                       :revision     => "123456789",
  #                       :serial       => "ABCD" }
  def hardware
    return {
      :manufacturer => query("AT+CGMI"),
      :model        => query("AT+CGMM"),
      :revision     => query("AT+CGMR"),
      :serial       => query("AT+CGSN") }
  end
  
  
  # The values accepted and returned by the AT+WMBS
  # command, mapped to frequency bands, in MHz. Copied
  # directly from the MultiTech AT command-set reference
  Bands = {
    0 => "850",
    1 => "900",
    2 => "1800",
    3 => "1900",
    4 => "850/1900",
    5 => "900E/1800",
    6 => "900E/1900"
  }
  
  # call-seq:
  #   bands_available => array
  #
  # Returns an array containing the bands supported by
  # the modem.
  def bands_available
    data = query("AT+WMBS=?")
    
    # wmbs data is returned as something like:
    #  +WMBS: (0,1,2,3,4,5,6),(0-1)
    #  +WMBS: (0,3,4),(0-1)
    # extract the numbers with a regex, and
    # iterate each to resolve it to a more
    # readable description
    if m = data.match(/^\+WMBS: \(([\d,]+)\),/)
      return m.captures[0].split(",").collect do |index|
        Bands[index.to_i]
      end
    
    else
      # Todo: Recover from this exception
      err = "Not WMBS data: #{data.inspect}"
      raise RuntimeError.new(err)
    end
  end
  
  # call-seq:
  #   band => string
  #
  # Returns a string containing the band
  # currently selected for use by the modem.
  def band
    data = query("AT+WMBS?")
    if m = data.match(/^\+WMBS: (\d+),/)
      return Bands[m.captures[0].to_i]
      
    else
      # Todo: Recover from this exception
      err = "Not WMBS data: #{data.inspect}"
      raise RuntimeError.new(err)
    end
  end
  
  BandAreas = {
    :usa     => 4,
    :africa  => 5,
    :europe  => 5,
    :asia    => 5,
    :mideast => 5
  }
  
  # call-seq:
  #   band=(_numeric_band_) => string
  #
  # Sets the band currently selected for use
  # by the modem, using either a literal band
  # number (passed directly to the modem, see
  # Gsm::Modem.Bands) or a named area from
  # Gsm::Modem.BandAreas:
  #
  #   m = Gsm::Modem.new
  #   m.band = :usa    => "850/1900"
  #   m.band = :africa => "900E/1800"
  #   m.band = :monkey => ArgumentError
  #
  # (Note that as usual, the United States of
  # America is wearing its ass backwards.)
  #
  # Raises ArgumentError if an unrecognized band was
  # given, or raises Gsm::Error if the modem does
  # not support the given band.
  def band=(new_band)
    
    # resolve named bands into numeric
    # (mhz values first, then band areas)
    unless new_band.is_a?(Numeric)
      
      if Bands.has_value?(new_band.to_s)
        new_band = Bands.index(new_band.to_s)
      
      elsif BandAreas.has_key?(new_band.to_sym)
        new_band = BandAreas[new_band.to_sym]
        
      else
        err = "Invalid band: #{new_band}"
        raise ArgumentError.new(err)
      end
    end
    
    # set the band right now (second wmbs
    # argument is: 0=NEXT-BOOT, 1=NOW). if it
    # fails, allow Gsm::Error to propagate
    command("AT+WMBS=#{new_band},1")
  end
  
  # call-seq:
  #   pin_required? => true or false
  #
  # Returns true if the modem is waiting for a SIM PIN. Some SIM cards will refuse
  # to work until the correct four-digit PIN is provided via the _use_pin_ method.
  def pin_required?
    not command("AT+CPIN?").include?("+CPIN: READY")
  end
  
  
  # call-seq:
  #   use_pin(pin) => true or false
  #
  # Provide a SIM PIN to the modem, and return true if it was accepted.
  def use_pin(pin)
    
    # if the sim is already ready,
    # this method isn't necessary
    if pin_required?
      begin
        command "AT+CPIN=#{pin}"
    
      # if the command failed, then
      # the pin was not accepted
      rescue Gsm::Error
        return false
      end
    end
    
    # no error = SIM
    # PIN accepted!
    true
  end
  
  
  # call-seq:
  #   signal => fixnum or nil
  #
  # Returns an fixnum between 1 and 99, representing the current
  # signal strength of the GSM network, or nil if we don't know.
  def signal_strength
    data = query("AT+CSQ")
    if m = data.match(/^\+CSQ: (\d+),/)
      
      # 99 represents "not known or not detectable",
      # but we'll use nil for that, since it's a bit
      # more ruby-ish to test for boolean equality
      csq = m.captures[0].to_i
      return (csq<99) ? csq : nil
      
    else
      # Todo: Recover from this exception
      err = "Not CSQ data: #{data.inspect}"
      raise RuntimeError.new(err)
    end
  end
  
  
  # call-seq:
  #   wait_for_network
  #
  # Blocks until the signal strength indicates that the
  # device is active on the GSM network. It's a good idea
  # to call this before trying to send or receive anything.
  def wait_for_network
    
    # keep retrying until the
    # network comes up (if ever)
    until csq = signal_strength
      sleep 1
    end
    
    # return the last
    # signal strength
    return csq
  end
  
  
  # call-seq:
  #   send_sms(message) => true or false
  #   send_sms(recipient, text) => true or false
  #
  # Sends an SMS message via _send_sms!_, but traps
  # any exceptions raised, and returns false instead.
  # Use this when you don't really care if the message
  # was sent, which is... never.
  def send_sms(*args)
    begin
      send_sms!(*args)
      return true
    
    # something went wrong
    rescue Gsm::Error
      return false
    end
  end
  
  
  # call-seq:
  #   send_sms!(message) => true or raises Gsm::Error
  #   send_sms!(receipt, text) => true or raises Gsm::Error
  #
  # Sends an SMS message, and returns true if the network
  # accepted it for delivery. We currently can't handle read
  # receipts, so have no way of confirming delivery. If the
  # device or network rejects the message, a Gsm::Error is
  # raised containing (hopefully) information about what went
  # wrong.
  #
  # Note: the recipient is passed directly to the modem, which
  # in turn passes it straight to the SMSC (sms message center).
  # For maximum compatibility, use phone numbers in international
  # format, including the *plus* and *country code*.
  def send_sms!(*args)
    
    # extract values from Outgoing object.
    # for now, this does not offer anything
    # in addition to the recipient/text pair,
    # but provides an upgrade path for future
    # features (like FLASH and VALIDITY TIME)
    if args.length == 1\
    and args[0].is_a? Gsm::Outgoing
      to = args[0].recipient
      msg = args[0].text
    
    # the < v0.4 arguments. maybe
    # deprecate this one day
    elsif args.length == 2
      to, msg = *args
    
    else
      raise ArgumentError,\
        "The Gsm::Modem#send_sms method accepts" +\
        "a single Gsm::Outgoing instance, " +\
        "or recipient and text strings"
    end
    
    # the number must be in the international
    # format for some SMSCs (notably, the one
    # i'm on right now) so maybe add a PLUS
    #to = "+#{to}" unless(to[0,1]=="+")
    
    # 1..9 is a special number which does notm
    # result in a real sms being sent (see inject.rb)
    if to == "+123456789"
      log "Not sending test message: #{msg}"
      return false
    end
    
    # block the receiving thread while
    # we're sending. it can take some time
    exclusive do
      log_incr "Sending SMS to #{to}: #{msg}"
      
      # initiate the sms, and wait for either
      # the text prompt or an error message
      command "AT+CMGS=\"#{to}\"", ["\r\n", "> "]
      
      # encode the message using the setup encoding or the default
      msg = encode(msg)
      
      begin
        # send the sms, and wait until
        # it is accepted or rejected
        write "#{msg}#{26.chr}"
        wait
        
      # if something went wrong, we are
      # be stuck in entry mode (which will
      # result in someone getting a bunch
      # of AT commands via sms!) so send
      # an escpae, to... escape
      rescue Exception, Timeout::Error => err
        log "Rescued #{err.desc}"
        write 27.chr
        
        # allow the error to propagate,
        # so the application can catch
        # it for more useful info
        raise
        
      ensure
        log_decr
      end
    end
        
    # if no error was raised,
    # then the message was sent
    return true
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
    
  # call-seq:
  #   receive(callback_method, interval=5, join_thread=false)
  #
  # Starts a new thread, which polls the device every _interval_
  # seconds to capture incoming SMS and call _callback_method_
  # for each, and polls the device's internal storage for incoming
  # SMS that we weren't notified about (some modems don't support
  # that).
  #
  #   class Receiver
  #     def incoming(msg)
  #       puts "From #{msg.from} at #{msg.sent}:", msg.text
  #     end
  #   end
  #   
  #   # create the instances,
  #   # and start receiving
  #   rcv = Receiver.new
  #   m = Gsm::Modem.new "/dev/ttyS0"
  #   m.receive rcv.method :incoming
  #   
  #   # block until ctrl+c
  #   while(true) { sleep 2 }
  #
  # Note: New messages may arrive at any time, even if this method's
  # receiver thread isn't waiting to process them. They are not lost,
  # but cached in @incoming until this method is called.
  def receive(callback, interval=5, join_thread=false)
    @polled = 0
    
    @thr = Thread.new do
      Thread.current["name"] = "receiver"
      
      # keep on receiving forever
      while true
        process(callback)        
        # re-poll every
        # five seconds
        sleep(interval)
        @polled += 1
      end
    end
    
    # it's sometimes handy to run single-
    # threaded (like debugging handsets)
    @thr.join if join_thread
  end
  
  def process(callback)
    command "AT"

    # check for messages in the default mailbox (wether read or not)
    # read them and then delete them       
    fetch_and_delete_stored_messages        
    
    # if there are any new incoming messages,
    # iterate, and pass each to the receiver
    # in the same format that they were built
    # back in _parse_incoming_sms!_
    unless @incoming.empty?
      @incoming.each do |msg|
        begin
          callback.call(msg)
          
        rescue StandardError => err
          log "Error in callback: #{err}"
        end
      end
      
      # we have dealt with all of the pending
      # messages. todo: this is a ridiculous
      # race condition, and i fail at ruby
      @incoming.clear
    end
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

  def select_default_mailbox
    # Eventually we will select the first mailbox as the default
    result = command("AT+CPMS=?")
    boxes = result.first.scan(/\"(\w+)\"/).flatten #"
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
      out = command("AT+CMGR=1")
    rescue
      return     
    end  
    return if out.nil? || out.empty?

    # Delete that message
    command("AT+CMGD=1")

    # Break that down
    header = out.first
    msg = out[1..out.size-2].join("\n")
    validity = out.last
               
    # Read the header
    header = header.scan(/\"([^"]+)\"/).flatten #"
    status = header[0]
    from = header[1]
    timestamp = header[2]
    # Parsing this using the incoming format failed, it need %Y instead of %y
    sent = DateTime.parse(timestamp)
                        
    # just in case it wasn't already obvious...
    log "Received message from #{from}: #{msg}"
  
    @incoming.push Gsm::Incoming.new(self, from, sent, msg)
  end
  
  def fetch_unread_messages
    
    # fetch all/unread (see constant) messages
    lines = command('AT+CMGL="%s"' % CMGL_STATUS)
    n = 0
    
    # if the last line returned is OK
    # (and it SHOULD BE), remove it
    lines.pop if lines[-1] == "OK"
    
    # keep on iterating the data we received,
    # until there's none left. if there were no
    # stored messages waiting, this done nothing!
    while n < lines.length
      
      # attempt to parse the CMGL line (we're skipping
      # two lines at a time in this loop, so we will
      # always land at a CMGL line here) - they look like:
      #   +CMGL: 0,"REC READ","+13364130840",,"09/03/04,21:59:31-20"
      unless m = lines[n].match(/^\+CMGL: (\d+),"(.+?)","(.+?)",*?,"(.+?)".*?$/)
        err = "Couldn't parse CMGL data: #{lines[n]}"
        raise RuntimeError.new(err)
      end
      
      # find the index of the next
      # CMGL line, or the end
      nn = n+1
      nn += 1 until\
        nn >= lines.length ||\
        lines[nn][0,6] == "+CMGL:"
      
      # extract the meta-info from the CMGL line, and the
      # message text from the lines between _n_ and _nn_
      index, status, from, timestamp = *m.captures
      msg_text = lines[(n+1)..(nn-1)].join("\n").strip
      
      # log the incoming message
      log "Fetched stored message from #{from}: #{msg_text.inspect}"
      
      # store the incoming data to be picked up
      # from the attr_accessor as a tuple (this
      # is kind of ghetto, and WILL change later)
      sent = parse_incoming_timestamp(timestamp)
      msg = Gsm::Incoming.new(self, from, sent, msg_text)
      @incoming.push(msg)
      
      # skip over the messge line(s),
      # on to the next CMGL line
      n = nn
    end
  end
end # Modem
end # Gsm
=end