#!/usr/bin/ruby

require 'fileutils'
require 'optparse'
require 'open3'

require 'timeout' #debugging
require 'set'

$USAGE="AP_logline_report.rb [-ahlv] [-d func-dir]"

$LLVMLD="llvm-ld"
$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$GMPLIB		= "#{$REPOROOT}/gmp-4.3.2/lib"
$GSLLIB		= "#{$REPOROOT}/klee/gsl-1.14/lib"
$TESTFILE 	= "test"
$LOGLINE	= "#{$REPOROOT}/bin/logline.rb"

$TOTAL_OVERFLOW = 0
$INTERESTING_OVERFLOW = 0
$TOTAL_UNDERFLOW = 0
$INTERESTING_UNDERFLOW = 0

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

	opts.on('-l', '--logline', 'Test inputs with logline transformed executable')  do
		options[:logline] = true
	end

	opts.on('-a', '--ap', 'Test inputs with AP transformed executable')  do
		if options[:logline] then
		      msg = "Can not run test one both AP and logline transformed code"
		      $stderr.puts msg
		      exit 1
		end
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


def checkException(options, fileName, exceptionFile, exception)
    outfilename = "#{fileName}.dat"
    outFile = File.new(outfilename, "a")
    if File.exist?(fileName)
	functionName = fileName.gsub("#{exception}/","")
	functionName = functionName.gsub("DivByZero/","")
	inFile = File.new(fileName, "r")
	exceptionNum = 0
	strSet = Set.new
	currentStr = ""
	while (inputs = inFile.gets)
	    exceptionNum += 1
	    inputs = inputs.chomp # These inputs are solutions from SMT solver.
	    begin
	      Timeout::timeout(5) do
		command = "#{$LOGLINE} #{$TESTFILE} -- #{inputs}"
		result =  `#{command}`
		line = result.to_i		
		dbl_inputs = `\./dbl_converter #{inputs}`
		outFile.puts("Input: \t #{dbl_inputs}")
		outFile.puts("Line: \t #{line}")
		outFile.puts("\n")
		if line > 0 then
		  str = "#{functionName}#{line}#{dbl_inputs}"
		  if (strSet.add? str) then
		    exceptionFile.puts("<tr>")
		    exceptionFile.puts("<td>#{functionName}</td>")
		    exceptionFile.puts("<td>#{line}</td>")
		    exceptionFile.puts("<td>#{dbl_inputs}</td>")
		    exceptionFile.puts("</tr>")
		    dbl_inputs = dbl_inputs.chomp(" ")
		    if ( exception.eql?("Invalid") ) then
		        invalidTex = File.new("invalid.logline.tex","a")
			invalidTex.puts("#{functionName.gsub("_","\\_")} & #{line} & #{dbl_inputs.gsub(" ",", ")}\\\\")
		    end
		    if ( exception.eql?("DivByZero") ) then
		        divTex = File.new("div.logline.tex","a")
			divTex.puts("#{functionName.gsub("_","\\_")} & #{line} & #{dbl_inputs.gsub(" ",", ")}\\\\")
		    end
		  end
		end
	    end
	    rescue Timeout::Error
		puts "#{fileName} too slow!"
	    end
	end
	inFile.close
    end
end

def distinguish (options, fileName, interestingExceptionFile, interestingHtml, exception, logFile)
  outfileName = "#{fileName}.inst"
  outFile = File.new(outfileName, "a")
  if File.exist?(fileName)
    functionName = fileName.gsub("#{exception}/","")
    inFile = File.new(fileName, "r")
    interesting = 0
    uninteresting = 0
    numOfFloatFunc = 0
    inputSet = Set.new
    while (inputs = inFile.gets)
      inputs = inputs.chomp
      begin Timeout::timeout(5) do
	  if (inputSet.add? inputs) then
	    command = "\./#{$TESTFILE} #{inputs} 2>&1"
	    result =  `#{command}`
	    Open3.popen3(command) do |cmdin, cmdout, cmderr|
	      cmdout.each do |line|
		logFile.puts(line)
	      end
	      cmderr.each do |line|
		logFile.puts(line)
	      end
	    end
	    val = result.to_i
	    puts val
	    if val == 1 then
	      numOfFloatFunc += 1
	      dbl_inputs = `\./dbl_converter #{inputs}`
	      outFile.puts(dbl_inputs)
	      interesting += 1
	      interestingHtml.puts("<tr>")
	      interestingHtml.puts("<td>#{functionName}</td>")
	      #FIXME: get the line from gdb here!
	      interestingHtml.puts("<td>1</td>")
	      interestingHtml.puts("<td>#{dbl_inputs}</td>")
	      interestingHtml.puts("</tr>")
	    elsif val == -1 then
	      numOfFloatFunc += 1
	      uninteresting += 1	    
	    end	    
	  end
      end # end begin
      rescue Timeout::Error
	  puts "#{fileName} too slow!"
      end
    end #end while
    inFile.close
    line = "#{functionName};#{interesting};#{uninteresting};#{numOfFloatFunc}"
    interestingExceptionFile.puts(line)
    if (exception.eql?("Overflow")) then
	$TOTAL_OVERFLOW = $TOTAL_OVERFLOW + interesting + uninteresting;
	$INTERESTING_OVERFLOW += interesting;
    end
    
    if (exception.eql?("Underflow")) then
	$TOTAL_UNDERFLOW = $TOTAL_UNDERFLOW + interesting + uninteresting;
	$INTERESTING_UNDERFLOW += interesting;
    end
    
  end #end if
end # end def

def insertHeader(file, exception)
	file.puts("<HEAD>")
	file.puts("<TITLE>Basic HTML Sample Page</TITLE>")
	file.puts("</HEAD>")
	file.puts("<BODY>")
	file.puts("<H1>Floating Point Exceptions in the GSL</H1>")
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

def insertTexHeader(file)
    file.puts("\\begin{tabular}{l l l }")
    file.puts("\\hline")
    file.puts("\\toprule")
    file.puts("Function & Line & Inputs\\\\")
    file.puts("\\midrule")
end

def insertTexFooter(file)
    file.puts("\\bottomrule")
    file.puts("\\hline")
    file.puts("\\end{tabular}")
end

def makeExecutable(file)
    command = "#{$LLVMLD} -native -disable-opt -L #{$GMPLIB} "
    command += "-lgmp -L #{$GSLLIB} -lgsl -lgslcblas -lm #{file} "
    command += "-o #{$TESTFILE}\n"
    `#{command}`
end

def run(options, bitcode_files)
	if (options[:ap]) then
	  interestingUnderflowHtml = File.new("Underflow.AP.html", "a")
	  interestingOverflowHtml = File.new("Overflow.AP.html", "a")
	  interestingUnderflowFile = File.new("Underflow.AP.csv","a")
	  interestingUnderflowFile.puts("Function;Interesting;Uninteresting;Float")
	  interestingOverflowFile = File.new("Overflow.AP.csv", "a")
	  interestingOverflowFile.puts("Function;Interesting;Uninteresting;Float")
	  insertHeader(interestingUnderflowHtml, "Underflow")
	  insertHeader(interestingOverflowHtml, "Overflow")
	  logFile = File.new("error_report.log","a")
	  bitcode_files.each do |file|
	      makeExecutable(file)
	      function = file.gsub(".o","")
	      function = function.gsub("main_","")
	      puts function
	      logFile.puts(function)
	      #distinguish interesting/uninteresting underflows
	      fileName = "Underflow/#{function}"
	      if File.exist?(fileName) then
		  distinguish(
		    options, fileName, interestingUnderflowFile,
		    interestingUnderflowHtml, "Underflow", logFile
		  )
	      end
	      #distinguish interesting/uninteresting overflows
	      fileName = "Overflow/#{function}"
	      if File.exist?(fileName) then
		  distinguish(
		    options, fileName, interestingOverflowFile,
		    interestingOverflowHtml, "Overflow", logFile
		  )
	      end
	  end
	  insertFooter(interestingOverflowHtml)
	  insertFooter(interestingUnderflowHtml)
	  interestingUnderflowHtml.close()
	  interestingOverflowHtml.close()
	  
	  # Print interesting/uninteresting exception info to a tex file
	  apInfoFile = File.new("AP.tex", "a")
	  apInfoFile.puts("Interesting & #{$INTERESTING_OVERFLOW} & #{$INTERESTING_UNDERFLOW} \\\\")
	  apInfoFile.puts("Total & #{$TOTAL_OVERFLOW} & #{$TOTAL_UNDERFLOW} \\\\")
	  overflowRatio = $INTERESTING_OVERFLOW.to_f/ $TOTAL_OVERFLOW.to_f
	  underflowRatio = $INTERESTING_UNDERFLOW.to_f / $TOTAL_UNDERFLOW.to_f
	  overflowRatioStr = "%0.2f" % overflowRatio
	  underflowRatioStr = "%0.2f" % underflowRatio
	  apInfoFile.puts("Ratio & #{overflowRatioStr} & #{underflowRatioStr} \\\\")
	  apInfoFile.close()
	  
	end

	if (options[:logline]) then
	  divTex = File.new("div.logline.tex","a")
	  invalidTex = File.new("invalid.logline.tex","a")
	  underflowFile = File.new("Underflow.logline.html", "a")
	  insertHeader(underflowFile, "Underflow")
	  overflowFile = File.new("Overflow.logline.html", "a")
	  insertHeader(overflowFile, "Overflow")
	  invalidFile = File.new("Invalid.logline.html","a")
	  insertHeader(invalidFile, "Invalid")
	  divByZeroFile = File.new("DivByZero.logline.html", "a")
	  insertHeader(divByZeroFile, "Divide By Zero")
	  bitcode_files.each do |file|
		makeExecutable(file)
		function = file.gsub(".o","")
		function = function.gsub("main_","")
		puts function
		#Check Underflow inputs
		fileName = "Underflow/#{function}"
		if File.exist?(fileName) then
		  checkException( options, fileName, underflowFile, "Underflow")
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
end

####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")
bitcode_files = getBitcodeFiles(options)
if not bitcode_files.empty?
    run(options, bitcode_files)
end

exit 0
