#!/usr/bin/ruby

require 'fileutils'
require 'optparse'

require 'timeout' #debugging
require 'set'

$USAGE="exception_test [-hvf] [-d func-dir]"

$LLVMLD="llvm-ld"
$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$GMPLIB		= "#{$REPOROOT}/gmp-4.3.2/lib"
$GSLLIB		= "#{$REPOROOT}/klee/gsl-1.14/lib"
$TESTFILE 	= "test"
$LOGLINE	= "#{$REPOROOT}/bin/logline.rb"

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
	
	opts.on('-l', '--logline', 'Test inputs with logline transformed executable')  do 		
		options[:logline] = true
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


def checkException(options, fileName, exceptionFile, exception)
    outfilename = "#{fileName}.dat"
    outFile = File.new(outfilename, "a")
    if File.exist?(fileName)
	functionName = fileName.gsub("#{exception}/","")
	functionName = functionName.gsub("DivByZero/","")
	inFile = File.new(fileName, "r")
	exceptionNum = 0
	strSet = Set.new	
	#currentStr = ""
	while (inputs = inFile.gets)
	    exceptionNum += 1 
	    inputs = inputs.chomp # These inputs are solutions from SMT solver.
	    begin 
	      Timeout::timeout(5) do
		command = "#{$LOGLINE} #{$TESTFILE} -- #{inputs}"
		puts "command = #{command}"
		fred = `pgrep -fl gdb`
		puts "gdb is running #{fred}" if not (fred.nil? or fred.empty?)
		result =  `#{command}`
		puts result
		line = result.to_i 		
		#puts line
		dbl_inputs = `\./dbl_converter #{inputs}`
		outFile.puts("Input: \t #{dbl_inputs}")
		outFile.puts("Line: \t #{line}")		
		outFile.puts("\n")
		if line > 0 then
		  str = "#{functionName}#{line}#{dbl_inputs}"
		  if (strSet.add? str) then
		    puts str
		    #puts currentStr
		    exceptionFile.puts("<tr>")
		    exceptionFile.puts("<td>#{functionName}</td>")
		    exceptionFile.puts("<td>#{line}</td>")
		    exceptionFile.puts("<td>#{dbl_inputs}</td>")
		    exceptionFile.puts("</tr>")
		    #currentStr = str
		  end
		end
	      end
	    rescue Timeout::Error
		puts "#{fileName} too slow!"
	    end	    
	end
	inFile.close
	#exceptionFile.puts("#{fileName};#{exceptionNum}\n")	
    end
end

def insertHeader(file, exception) 
	file.puts("<HEAD>")
	file.puts("<TITLE>Basic HTML Sample Page</TITLE>")
	file.puts("</HEAD>")
	file.puts("<BODY>")
	#file.puts("<H1>Floating Point Exceptions in the GSL</H1>")
	file.puts("<H2>#{exception}</H2>")
	file.puts("<table border=\"1\">")
	file.puts("<tr>")
	file.puts("<th>Function</th>")
	file.puts("<th>Line Number</th>")
	file.puts("<th>Inputs</th>")
	file.puts("<\/tr>")	
end

def insertFooter(file) 
      file.puts("</table>")
      file.puts("</BODY>")      
end

def run(options, bitcode_files)
	underflowFile = File.new("Underflow.html", "a")
	insertHeader(underflowFile, "Underflow")	
	overflowFile = File.new("Overflow.html", "a")
	insertHeader(overflowFile, "Overflow")	
	invalidFile = File.new("Invalid.html","a")
	insertHeader(invalidFile, "Invalid")	
	divByZeroFile = File.new("DivByZero.html", "a")
	insertHeader(divByZeroFile, "Divide By Zero")	
	bitcode_files.each do |file|
		command = "#{$LLVMLD} -native -disable-opt -L #{$GMPLIB} "
		command += "-lgmp -L #{$GSLLIB} -lgsl -lgslcblas -lm #{file} "
		command += "-o #{$TESTFILE}\n"
		puts command
		#`#{command}`
		function = file.gsub(".o","")
		function = function.gsub("main_","")
		puts function
		#Check Underflow inputs
		fileName = "Underflow/#{function}"
		if File.exist?(fileName) then
		  checkException(options, fileName, underflowFile, "Underflow")
		end
		#check Overflow inputs
		fileName = "Overflow/#{function}"
		if File.exist?(fileName) then
		  checkException(options, fileName, overflowFile, "Overflow")
		end
		#check Invalid inputs
		fileName = "Invalid/#{function}"
		if File.exist?(fileName) then
		  checkException(options, fileName, invalidFile, "Invalid")
		end
		#check DivByZero inputs		
		fileName = "DivByZero/#{function}"
		if File.exist?(fileName) then
		  checkException(options, fileName, divByZeroFile, "DivByZero")
		end
	end
	insertFooter(underflowFile)
	insertFooter(overflowFile)
	insertFooter(invalidFile)
	insertFooter(divByZeroFile)
end

####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")
bitcode_files = getBitcodeFiles(options)
if not bitcode_files.empty?
    run(options, bitcode_files)	
end

exit 0
