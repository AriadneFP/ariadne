#!/usr/bin/ruby

require 'fileutils'
require 'optparse'

require 'timeout' #debugging

$USAGE="logline_gdb [-hvf] [-d func-dir]"

$LLVMLD="llvm-ld"
$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$GMPLIB		= "#{$REPOROOT}/gmp-4.3.2/lib"
$GSLLIB		= "#{$REPOROOT}/klee/gsl-1.14/lib"
$TESTFILE 	= "test"

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

def getBitcodeFiles (options)
	bitcode_files = []	
	Dir.glob("*\.o") do |f|
		if (f.match(/^main/)) 		  
		  bitcode_files.push(f) 
		end
	end	
	return bitcode_files
end


def checkException(fileName)
    outfilename = "#{fileName}.out"
    outFile = File.new("#{outfilename}", "w")
    if File.exist?(fileName)
	inFile = File.new(fileName, "r")
	while (inputs = inFile.gets)
	    inputs = inputs.chomp # These inputs are solutions from SMT solver.
	    begin 
	      Timeout::timeout(5) do
		command = "gdb -x gdb.script --args \./#{$TESTFILE} #{inputs} 2>&1 | grep \"^[0-9][0-9]*\""
		puts command
		result =  `#{command}`
		outFile.puts("#{result}")		
	      end
	    rescue Timeout::Error
		puts "#{fileName} too slow!"
	    end	    
	end
	inFile.close
	
    end
end

def run(options, bitcode_files)
	
	bitcode_files.each do |file|
		command = "#{$LLVMLD} -native -disable-opt -L #{$GMPLIB} "
		command += "-lgmp -L #{$GSLLIB} -lgsl -lgslcblas -lm #{file} "
		command += "-o #{$TESTFILE}\n"
		puts command
		`#{command}`
		function = file.gsub(".o","")
		function = function.gsub("main_","")
		puts function
		#Check Underflow inputs
		fileName = "Underflow/#{function}"
		checkException(fileName)
		#check Overflow inputs
		fileName = "Overflow/#{function}"
		checkException(fileName)
		#check Invalid inputs
		fileName = "Invalid/#{function}"
		checkException(fileName)  
		#check DivByZero inputs		
		fileName = "DivByZero/#{function}"
		checkException(fileName)
	end
end

####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")
bitcode_files = getBitcodeFiles(options)
if not bitcode_files.empty?
    run(options, bitcode_files)	
end

exit 0
