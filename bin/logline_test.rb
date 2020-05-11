#!/usr/bin/ruby

require 'fileutils'
require 'optparse'

require 'timeout' #debugging

$USAGE="exception_test [-hv] [-d func-dir]"

$LLVMLD="llvm-ld"
$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$GMPLIB		= "#{$REPOROOT}/gmp-4.3.2/lib"
$GSLLIB		= "#{$REPOROOT}/klee/gsl-1.14/lib"
$ARIADNE 		= "#{$REPOROOT}/bin/ariadne.rb"
$LLVMREL	= "#{$REPOROOT}/llvm-2.7/Release+Debug"
$OPT 		= "#{$LLVMREL}/bin/opt"
$LOGLINE 	= "#{$REPOROOT}/bin/logline.rb"
$OPTFLAGS	= "-load #{$LLVMREL}/lib/Ariadne.so -Ariadne -MainAdder"

options = {}
optparse = OptionParser.new do |opts|
	opts.banner = $USAGE

	options[:func_dir] = "./tests"
	opts.on(
		'-d', '--funcdir dir',
		'Directory in which .o gsl files reside')  do |dir|
		if not File.directory?( dir )
			$stderr.puts $USAGE
			exit 1
		end
		options[:func_dir] = dir
	end
	
	opts.on( '-h', '--help', 'Display this screen' ) do
	     puts $USAGE
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

unless ARGV.empty?
	$stderr.puts $USAGE 
	exit 0
end

def getSourceFiles (options)
	source_files = []	
	Dir.glob("*\.c") do |f|
		source_files.push(f) 		
	end	
	return source_files
end

def compile(options, source_files)
	source_files.each do |file|
		#Inject the FPE_handler code
		command = "#{$LOGLINE} #{file}"
		puts command if options[:verbose]
		`#{command}`
		command = "mv #{file.chomp('.c')}.linelog.c #{file}"
		puts command if options[:verbose]
		`#{command}`
	end
end

####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")
source_files = getSourceFiles(options)
if not source_files.empty?
    compile(options, source_files)	
end

exit 0
