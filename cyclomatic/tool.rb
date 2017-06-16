# !usr/bin/ruby
# Author: Benjamin Cooper
# I opted to use Ruby instead of python because of better native support for regex and system calls

require 'optparse'

$options = {}

$options[:language] = 'c'
$options[:dirname] = '.'
$options[:outfile] = 'result.txt'

$re_file = /^(\d|\w)(.)*\.c$/
$re_dir = /^(\d|\w)(.)*$/

OptionParser.new do |opts|
	opts.banner = "Usage: ruby tool.rb [options]"

	opts.on('-l', '--language LANG', 'Language (default is c)') {|v| $options[:language] = v}
	opts.on('-d', '--dirname DIR', 'Directory (default is .)') {|v| $options[:dirname] = v}
	opts.on('-f', '--filename FILE', 'Filename (not defaulted)') {|v| $options[:filename] = v}
	opts.on('-o', '--outfile FILE', 'Name of file in which to store results (default is result.txt)') {|v| $options[:outfile] = v}
	#opts.on('-v', '--verbose', 'Tag to trigger verbose mode') {|v| $options[:verbose] = v}

end.parse!

# Pretty print the given string
def pretty(st)
	n = st.length
	puts('='*(n+2))
	puts(' ' + st + ' ')
	puts('='*(n+2))
end

def cyclo(fn, lang)
	path = Dir.pwd
	Dir.chdir "/Users/benjamincooper/Desktop/BIBIFI/pfff/cyclomatic"
	if (lang == 'c') then
		pretty(path + "/" + fn)
		system("./c_cyclo.byte " + path + "/" + fn)
	end
	Dir.chdir path
end
 
def traverse(dir)
	Dir.chdir dir
	Dir.foreach('.') do |f|
		if(File.file?(f)) then
			if($options[:language] == 'c' and f =~ $re_file) then cyclo(f,$options[:language]) end
		elsif(File.directory?(f)) then
			if(f =~ $re_dir) then traverse(f) end
		end
	end
	Dir.chdir '..'
end
 
traverse($options[:dirname])

