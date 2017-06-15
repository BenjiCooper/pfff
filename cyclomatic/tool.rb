# !usr/bin/ruby
# Author: Benjamin Cooper
# It should be noted that this particular file is SUPER insecure

require 'optparse'

options = {}

options[:language] = 'c'
options[:dirname] = '.'

OptionParser.new do |opts|
	opts.banner = "Usage: ruby tool.rb [options]"

	opts.on('-l', '--language LANG', 'Language (default is c)') {|v| options[:language] = v}
	opts.on('-d', '--dirname DIR', 'Directory (default is .)') {|v| options[:dirname] = v}
	opts.on('-f', '--filename FILE', 'Filename (not defaulted)') {|v| options[:filename] = v}
	opts.on('-v', '--Verbose', 'Tag to trigger verbose mode') {|v| options[:verbose] = v}

end.parse!

options.each do |k,v|
	puts(k.to_s + " " + v.to_s)
end

def traverse
	Dir.chdir options[:dirname]

