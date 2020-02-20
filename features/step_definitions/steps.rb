# 
# %%HEADER%%
#
Given /^there are no feature files$/ do
  system("rm tmp/aruba/*.feature > /dev/null 2>&1")
end

Given /^there is a feature file "([^"]*)" with:$/ do |filename, string|
  File.open("tmp/aruba/#{filename}",'w') do |f|
    f.write(string)
  end
end
