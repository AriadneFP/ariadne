#!/usr/bin/ruby

require 'fileutils'
require 'open3'
require 'optparse'
require 'timeout' #debugging

# Inputs may be GMP rationals.  The -- terminates option parsing and allows 
# negative inputs.
$USAGE="logline [-hv] numeric_program_with_explicit_fpe -- <inputs>"


options = {}
optparse = OptionParser.new do |opts|
	opts.banner = $USAGE

	opts.on( '-h', '--help', 'Display this screen' ) do
		puts $USAGE
		puts "Inputs may be GMP rationals.  The -- terminates option parsing"
		puts "and allows negative inputs."
	    exit 0
	end

	opts.on( '-v', '--verbose', 'Output progress' ) do
	    options[:verbose] = true
	end
end
begin
	optparse.parse!
rescue OptionParser::InvalidOption => invalidOption
	message  = "#{invalidOption} is not a recognized option."
	$stderr.puts message
	$stderr.puts $USAGE
	exit 1
end

if not File.exists?("gdb.script") then
	gscript = new File("gdb.script")
	gscript.puts \
%q{
run
quit
}
end

if ARGV.length < 1 or not File.exists?(ARGV[0]) then
	$stderr.puts $USAGE
	exit 1
end

inputs = ARGV[1..-1].join(" ")
command = "gdb -x gdb.script --args \./#{ARGV[0]} #{inputs}"
kill = false
Open3.popen3(command) do |cmdin, cmdout, cmderr|
	cmdout.each do |line|
		if line =~ /^.*:(\d+)/ then
		  # ignore three lines added by the scripts
		  puts "#{$1.to_i - 3}"
		end
		kill = true if line =~ /A debugging session is active\./
	end
	cmderr.each do |line|
		kill = true if line =~ /A debugging session is active\./
	end
end
puts "kill = #{kill}"
`pkill f gdb -x gdb.script --args` if kill
exit 0
