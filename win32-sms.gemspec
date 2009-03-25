# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{win32-sms}
  s.version = "0.1.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Jeff Rafter"]
  s.date = %q{2009-03-25}
  s.description = %q{TODO}
  s.email = %q{jeff@baobabhealth.org}
  s.files = ["VERSION.yml", "lib/win32_sms.rb", "test/test_helper.rb", "test/win32_sms_test.rb"]
  s.homepage = %q{http://github.com/jeffrafter/win32-sms}
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.1}
  s.summary = %q{TODO}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 2

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
    else
    end
  else
  end
end
