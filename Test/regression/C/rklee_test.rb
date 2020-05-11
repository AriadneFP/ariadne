#!/usr/bin/ruby

require "fileutils"

$USAGE = "rklee_test <list of c files to test>"

if not (File.directory?("./EXE"))
  `mkdir EXE`
end 

FileUtils.rm Dir.glob("*.o")	# Remove existing .o files.

ARGV.each do |f|
	if not ( File.file? f )
		$stderr.puts "Error:  #{f} not found."
		$stderr.puts $USAGE
		exit 1
	end
	`ariadne.rb -km #{f}`
	if $? != 0
		$stderr.puts "Error:  call to llvm's opt failed on #{f}."
		exit 1
	end
	`ariadne.rb -kc #{f}`
end

Dir.glob("*main\.o") do |f|
  `rklee.rb #{f}`
end

exit 0

