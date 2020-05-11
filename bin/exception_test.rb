#!/usr/bin/ruby

require 'fileutils'
require 'optparse'

require 'timeout' #debugging

$USAGE="exception_test [-hvf] [-d func-dir]"

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
	
	opts.on('-f', '--fp', 'Test with double format inputs')  do 		
		options[:float] = true
	end
	
	opts.on('-a', '--ap', 'Test inputs with AP transformed executable')  do 		
		options[:ap] = true
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


def checkException(options, fileName, exceptionFile, exception, gslErrorFile)
    outfilename = "#{fileName}.out"    
    if File.exist?(fileName)
	inFile = File.new(fileName, "r")
	detectedNum = 0
	confirmedNum = 0
	gslErrorNum = 0
	while (inputs = inFile.gets)
	    detectedNum += 1 
	    inputs = inputs.chomp # These inputs are solutions from SMT solver.
	    begin 
	      Timeout::timeout(5) do
		
		if options[:ap]
		  
		end
		command = "\./#{$TESTFILE} #{inputs} >> #{outfilename} 2>> #{outfilename}"
		puts command
		`#{command}`
		command = "\./#{$TESTFILE} #{inputs}"
		result =  `#{command}`
		if (result.match(/(.*)#{exception}(.*)/))
		  confirmedNum += 1
		end
		gsl = "gsl"
		if (result.match(/(.*)#{gsl}(.*)/))
		  gslErrorNum += 1
		end
	      end
	    rescue Timeout::Error
		puts "#{fileName} too slow!"
	    end	    
	end
	inFile.close
	fileName = fileName.gsub("#{exception}/","")
	fileName = fileName.gsub("DivByZero/","")
	exceptionFile.puts("#{fileName};#{detectedNum};#{confirmedNum}\n")
	gslErrorFile.puts("#{fileName};#{gslErrorNum}\n")
    end
end

def run(options, bitcode_files)
	underflowFile = File.new("Underflow.csv", "a")
	underflowFile.puts("Function;Detected;Confirmed\n")
	overflowFile = File.new("Overflow.csv", "a")
	overflowFile.puts("Function;Detected;Confirmed\n")
	invalidFile = File.new("Invalid.csv","a")
	invalidFile.puts("Function;Detected;Confirmed\n")
	divByZeroFile = File.new("DivByZero.csv", "a")
	gslErrorFile = File.new("GSL_ERROR.csv", "a")
	gslErrorFile.puts("Function;NumofErrors\n")
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
		checkException(options, fileName, underflowFile,"Underflow", gslErrorFile)
		#check Overflow inputs
		fileName = "Overflow/#{function}"
		checkException(options, fileName, overflowFile, "Overflow", gslErrorFile)
		#check Invalid inputs
		fileName = "Invalid/#{function}"
		checkException(options, fileName, invalidFile, "Invalid", gslErrorFile)  
		#check DivByZero inputs		
		fileName = "DivByZero/#{function}"
		checkException(options, fileName, divByZeroFile, "Divide by Zero", gslErrorFile)
	end
end

####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")
bitcode_files = getBitcodeFiles(options)
if not bitcode_files.empty?
    run(options, bitcode_files)	
end

exit 0
