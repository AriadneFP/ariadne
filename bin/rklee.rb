#!/usr/bin/ruby

require 'fileutils'
require 'optparse'

$USAGE="rklee [-hpv] <llvm bitcode files>"
$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"

$KLEE		= `which klee 2>&1`.chomp

$GMPLIB		= "#{$REPOROOT}/gmp-4.3.2/lib"
$GSLLIB		= "#{$REPOROOT}/klee/gsl-1.14/lib"

$LLVMLD="llvm-ld"
$FP_env_dir = "FP_env/"
$EXE_dir = "EXE/"
$TESTFILE   = "test"
$TIMEOUT = "-watchdog -max-time=300 -ariadne-time=60"
$ARIADNE_OPTIONS = "-use-ariadne -use-cex-cache -use-cache -suppress-external-warnings"

options = {}
OptionParser.new do |opts|
	opts.banner = $USAGE

	opts.on( '-h', '--help', 'Display this screen' ) do
	     $stderr.puts $USAGE
	     exit 0
	end

	opts.on( '-p', '--pipe', 'Output to stdout for piping' ) do
		 options[:pipe] = true
	end

	opts.on( '-v', '--verbose', 'Verbosely output commands issued' ) do
		 options[:verbose] = true
	end
end.parse!

unless File.exists?($KLEE)
	$stderr.puts "Perhaps you have not built klee: it was not found."
	$stderr.puts "For build instructions, consult #{$REPOROOT}/Readme.txt."
	exit 1
end

if ARGV.size == 0
	$stderr.puts "At least one bitcode file to symbolically execute required."
	$stderr.puts $USAGE
	exit 1
end

STDOUT.sync = true	# Force a flush after each write to stdout.

ARGV.each do |f| 
	
	unless File.exists?(f)
		$stderr.puts "A list of bitcode files must end the command line."
		$stderr.puts $USAGE
		exit 1
	end
	
	command = "#{$LLVMLD} -native -disable-opt -L #{$GMPLIB} "
	command += "-lgmp -L #{$GSLLIB} -lgsl -lgslcblas -lm #{$EXE_dir}#{f} "
	command += "-o #{$TESTFILE}\n"
	puts command
	`#{command}`
	name = f.gsub(/\.\w$/,'')

	command = " #{$KLEE} #{$ARIADNE_OPTIONS} #{$TIMEOUT} #{name}.o 2>&1 |tee #{name}.out"
	command = " #{$KLEE} #{name}.o " if options[:pipe]
	puts command if options[:verbose]
	output = `#{command}`
	if $? != 0		#TODO:  More sophisticated error handling.
		puts "#{output}"
		exit 1
	end
end
