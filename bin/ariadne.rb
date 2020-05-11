#!/usr/bin/ruby

require 'fileutils'
require 'optparse'

$USAGE="ariadne [-acfhklmpv] [-- llvm-gcc switches] <source files>"
$EXCLUSION_ERROR = \
	"The Ariadne and arbitrary precision tranformations are mutually exclusive."

$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$GMP		= "#{$REPOROOT}/gmp-4.3.2/gmp.h"
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

$LLVMGCC	=
	if `uname -a` =~ /x86_64/
		"#{$REPOROOT}/llvm-gcc/llvm-gcc4.2-2.7-x86_64-linux/bin/llvm-gcc"
	else
		"#{$REPOROOT}/llvm-gcc/llvm-gcc-4.2-2.7-i686-linux/bin/llvm-gcc"
	end
unless File.exists?($LLVMGCC)
	$stderr.puts "Perhaps you have not built llvm-gcc: it was not found."
	$stderr.puts "For build instructions, consult #{$REPOROOT}/Readme.txt."
	exit 1
end

$OPT		= "#{$LLVMREL}/bin/opt"
unless File.exists?($OPT)
	$stderr.puts "Perhaps you have not built llvm: #{$OPT} was not found."
	$stderr.puts "For build instructions, consult #{$REPOROOT}/Readme.txt."
	exit 1
end


options = {}
options[:transform] = ""
options[:print] = false
optparse = OptionParser.new do |opts|
	# Set the enviroment variable __ARIADNE_NORMAL, 0 if we do not transform normal executables
	# 1 if we transform normal executables
	ENV['__FORTRAN'] = "0" 
	ENV['__ARIADNE_NORMAL'] = "0"
	ENV['__ARIADNE_FENV'] = "0"
	opts.banner = $USAGE
    
	opts.on('-a',
		'--arbitrary', 
		'Transform numeric code to use arbitrary precision'
	) do 
		if options[:transform] =~ /-Ariadne|-loopControl/
			$stderr.puts $EXCLUSION_ERROR
			exit 1
		end
		unless File.exists?($GMP)
			$stderr.puts "Perhaps you have not built GMP: gmp.h not found."
			$stderr.puts "Consult #{$REPOROOT}/Readme.txt."
			exit 1
		end
		options[:transform] += "-ArbitraryPrecision "
	end
 
	opts.on( '-h', '--help', 'Display this screen' ) do
	     $stderr.puts $USAGE
	     exit 0
	end

	# Add function call to module, do not create external functions as make_symbolic and captureExpr 
	opts.on('-c','--functionCall', 'Add function call to executable') do 
		if ( options[:transform] =~ /-ArbitraryPrecision|-MainAdder/ )		    
			$stderr.puts $EXCLUSION_ERROR
			$stderr.puts $USAGE
			exit 1
		end
		ENV['__ARIADNE_NORMAL'] = "1"
		options[:transform] += "-FunctionCall "
	end
	
	opts.on('-k','--ariadne', 'Perform base Ariadne transform') do 
		if (options[:transform] =~ /-ArbitraryPrecision/)
			$stderr.puts $EXCLUSION_ERROR
			$stderr.puts $USAGE
			exit 1
		end
		options[:transform] += "-Ariadne "
	end	
	
	
	opts.on('-l','--loopControl', 'Bound all loops transform') do 
		if options[:transform] =~ /-ArbitraryPrecision/
			$stderr.puts $EXCLUSION_ERROR
			$stderr.puts $USAGE
			exit 1
		end
		options[:transform] += "-loopControl " 
	end
	
	opts.on('--fortran-by-value', 'Fortran pass by value transform') do 
		if options[:transform] =~ /-ArbitraryPrecision/
			$stderr.puts $EXCLUSION_ERROR
			$stderr.puts $USAGE
			exit 1
		end
		options[:transform] += "-FortranByValue " 
	end
	
	opts.on('--fortran', 'Fortran pass by value transform') do 
		ENV['__FORTRAN']="1"
	end	
	
	opts.on('--logline', 'Logline pass to find the terminated line of program') do 
		if options[:transform] =~ /-ArbitraryPrecision/
			$stderr.puts $EXCLUSION_ERROR
			$stderr.puts $USAGE
			exit 1
		end
		ENV['__ARIADNE_FENV'] = "1"
		options[:transform] += "-FunctionCall " 
	end
	
	opts.on('-m','--mainAdder', 'main adder transform') do 
		if options[:transform] =~ /-ArbitraryPrecision/
			options[:transform] += "-APMainAdder "			
		else
		    options[:transform] += "-MainAdder " 
		end		
	end
	
	opts.on('-f','--fenv', 'Generate function calls enabling exception flags.') do 
		if options[:transform] =~ /-ArbitraryPrecision/
			$stderr.puts $EXCLUSION_ERROR
			$stderr.puts $USAGE
			exit 1
		end
		options[:transform] += "-FP_env "
	end

	opts.on('-p','--print', 'Print LLVM IR.') do 
		options[:print] = true
	end	
	
	
	opts.on( '-v', '--verbose', 'Verbosely output commands issued' ) do
		 options[:verbose] = true
	end
end
begin
	optparse.parse!
rescue OptionParser::InvalidOption => invalidOption
	message  = "#{invalidOption} is not a recognized option.  Perhaps you "
	message += "forgot to use '--' to delimit this script's option from "
	message += "those intended for llvm-gcc."
	$stderr.puts message
	$stderr.puts $USAGE
	exit 1
end
options[:transform] = "-Ariadne" if options[:transform].empty?

$OPTFLAGS	= "-load #{$LLVMREL}/lib/Ariadne.so #{options[:transform]}"
if options[:print]
	printlib = "#{$REPOROOT}/llvm-2.7/"
	if File.exists?("#{printlib}/Release+Debug/lib/Print.so")
		printlib += "Release+Debug/lib/Print.so"
	else
		printlib += "Release/lib/Print.so"
	end
	$OPTFLAGS = "-load #{printlib} -Print"
end

if ARGV.size == 0
	$stderr.puts "At least one source file to transform is required."
	$stderr.puts $USAGE
	exit 1
end

# Ariadne handles C, C++, and Fortran.
SRC_SUFFIXES = ["C", "cpp", "cc", "cxx", "F", "f", "f77", "f90", "c"]
SRC_SUFFIXES.collect! { |e| '\.' + e }

FILES = ARGV.select { |e| e =~ /#{SRC_SUFFIXES.join("|")}/ }
$LLVMGCCOPTS = ARGV - FILES

# This regex captures shared library object files *.l(a|o) and standard object 
# files put into .libs.
OBJECTFILES = /(-o  *(\w|-)*\.l(a|o)$|-o  *\.libs\/(\w|-)*\.o$)/
$EMITLLVM = 
	if $LLVMGCCOPTS.join(" ") =~ OBJECTFILES then
		""
	else
		'-emit-llvm'
	end

# There are convenient when using this script manually, as opposed to calling 
# it from a build system.
ADD_CSWITCH = $LLVMGCCOPTS.select {|e| e =~ /-c/}.empty? 
ADD_OSWITCH = $LLVMGCCOPTS.select {|e| e =~ /-o/}.empty? 
ADD_GSWITCH = $LLVMGCCOPTS.select {|e| e =~ /-g/}.empty?

STDOUT.sync = true	# Force a flush after each write to stdout.

FILES.each do |f| 
	unless File.exists?(f)
		$stderr.puts "A list of source files must end the command line."
		$stderr.puts $USAGE
		exit 1
	end

	name = f.gsub(/\.\w$/,'')
	# 1. Use llvm-gcc to compile f 
	command  = "#{$LLVMGCC} #{$LLVMGCCOPTS.join(" ")} #{$EMITLLVM}"
	command += " -c" if ADD_CSWITCH
	command += " -o #{name}.o" if ADD_OSWITCH
	command += " -g" if ADD_GSWITCH 
	command += " #{f} 2>&1" 
	if options[:transform] =~ /-ArbitraryPrecision|-FP_env|-FunctionCall|-LogLineMain/
		FileUtils.cp f, f + ".tmp"
		# TODO: May need to include stdio.h, see URL at end of this file.
		system "sed -i -e'1 i #include <#{$GMP}>' #{f}"
		system "sed -i -e'2 i #include <fenv.h>' #{f}"
		# Dead code is eliminated before our transformation runs.  We inject 
		# this global so that the mpq_t structure is not eliminated.
		system "sed -i -e'3 i mpq_t __ariadne_deleteme_;' #{f}"  
		puts command if options[:verbose]
		output = `#{command}`
		FileUtils.mv f + ".tmp", f
		if output =~ /error/
			FileUtils.rm f + ".o" if File.exists?(f + ".o")
			$stderr.puts output
			exit 1
		end
	else
		puts command if options[:verbose]
		output = `#{command}` 
		if output =~ /error/
			FileUtils.rm f + ".o" if File.exists?(f + ".o")
			$stderr.puts output
			exit 1
		end
	end

	# Do not run opt on shared library object files.
	next if $EMITLLVM.empty?

	# 2. Run our passes using opt.
	command = "#{$OPT} #{$OPTFLAGS} -o #{name}.o < #{name}.o 2> #{name}.log"
	puts command if options[:verbose]
	output = `#{command}`
	if $? != 0		#TODO:  More sophisticated error handling.
		puts "#{output}"
		exit 1
	end

end

#http://gmplib.org/manual-4.3.2/Headers-and-Libraries.html#Headers-and-Libraries