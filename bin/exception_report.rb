#!/usr/bin/ruby

require 'fileutils'
require 'optparse'
require 'open3'

require 'timeout' #debugging
require 'set'

$USAGE="exception_report.rb [-hv] [-d func-dir]"

$LLVMLD="llvm-ld"
$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$GMPLIB		= "#{$REPOROOT}/gmp-4.3.2/lib"
$GSLLIB		= "#{$REPOROOT}/klee/gsl-1.14/lib"
$TESTFILE 	= "test"
$LOGLINE	= "#{$REPOROOT}/bin/logline.rb"

$options = {}
$path
optparse = OptionParser.new do |opts|
	opts.banner = $USAGE

	$options[:func_dir] = "./tests"

	opts.on(
		'-d', '--funcdir dir',
		'Directory in which .o gsl files reside')  do |dir|
		if not File.directory?( dir )
			$stderr.puts $USAGE
			exit 1
		end
		Dir.chdir("#{dir}")
		$options[:func_dir] = Dir.getwd
	end


	opts.on( '-h', '--help', 'Display this screen' ) do
	     puts $USAGE
	     exit 0
	end

	opts.on( '-v', '--verbose', 'Output progress' ) do
	     $options[:verbose] = true
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

def insertTexHeader(file)
    file.puts("\\begin{tabular}{ l r }")
    file.puts("\\hline")
    file.puts("\\toprule")
    file.puts("Function & Inputs\\\\")
    file.puts("\\midrule")
end

def insertTexFooter(file)
    file.puts("\\bottomrule")
    file.puts("\\hline")
    file.puts("\\end{tabular}")
end

def getExceptionFiles ()
	exception_files = []
	Dir.glob("*") do |f|
		exception_files.push(f)
	end
	return exception_files
end

def printTex(texFile, exception)
    Dir.chdir("#{$options[:func_dir]}/#{exception}")
    Dir.glob("*") do |file|
      puts file
      strSet = Set.new
      inFile = File.new(file, "r")
      while (line = inFile.gets)
	inputs = `\./../dbl_converter #{line}`
	inputs = inputs.chomp(" ")
	str = "#{file}#{inputs}"
	if (strSet.add? str) then
	    texFile.puts("#{file.gsub("_","\\_")} & #{inputs.gsub(/ /,",")}\\\\")
	end
      end
    end
end

def countExceptions( exception )
  exceptionNum = 0
  Dir.chdir("#{$options[:func_dir]}/#{exception}")
  Dir.glob("*") do |file|
    puts file
    strSet = Set.new
    inFile = File.new(file, "r")
    while (line = inFile.gets)
      inputs = `\./../dbl_converter #{line}`
      inputs = inputs.chomp(" ")
      str = "#{file}#{inputs}"
      if (strSet.add? str) then
	exceptionNum += 1
      end
    end
  end
  exceptionNum
end

def countExcepPerFunction( functionName )

end

def summarize()
    Dir.chdir( "#{$options[:func_dir]}" )
    exceptionFile = File.new( "ex-by-type.csv", "a" )
    exceptionFile.puts( "Exception;Number" )

    invalidNum = countExceptions( "Invalid" )
    divNum = countExceptions( "DivByZero" )
    overflowNum = countExceptions( "Overflow" )
    underflowNum = countExceptions( "Underflow" )

    exceptionFile.puts( "Invalid;#{invalidNum}" )
    exceptionFile.puts( "DivByZero;#{divNum}" )
    exceptionFile.puts( "Underflow;#{underflowNum}" )
    exceptionFile.puts( "Overflow;#{overflowNum}" )
    exceptionFile.close()

end

def run()
    divTex = File.new( "div.tex", "a" )
    invalidTex = File.new( "invalid.tex", "a" )
    printTex( divTex, "DivByZero" )
    printTex( invalidTex, "Invalid" )
    summarize()
end

####### MAIN ##############################
run()
exit 0
