#!/usr/bin/ruby

require 'open3'
require 'optparse'
require 'pp'

$USAGE="test.rb TODO options -d bitcodeFile"

$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"

$LLVMREL	=
	if File.exists?("#{$REPOROOT}/llvm-2.7/Release+Debug")
		"#{$REPOROOT}/llvm-2.7/Release+Debug"
	elsif File.exists?("#{$REPOROOT}/llvm-2.7/Release")
		"#{$REPOROOT}/llvm-2.7/Release"
	else
		$stderr.puts "No LLVM Release directory found."
		$stderr.puts "For build instructions, consult #{$REPOROOT}/Readme.txt."
		exit 1
	end
$OPT		= "#{$LLVMREL}/bin/opt"	
$OPTFLAGS	= "-load #{$LLVMREL}/lib/Print.so -Print"	
	
ARGV.each do |file|
	command = "#{$OPT} #{$OPTFLAGS} -o #{file} < #{file} 2> #{file}.log"
	`#{command}`  
end
