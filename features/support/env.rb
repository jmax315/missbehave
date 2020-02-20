require 'aruba/cucumber'


Before do
 #make sure behave is in our path
 ENV['PATH'] = "#{ENV['PATH']}:#{File.expand_path(File.dirname(__FILE__))}"
 @aruba_timeout_seconds = 5
end
