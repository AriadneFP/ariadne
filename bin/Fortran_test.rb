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
$OPTFLAGS	= "-load #{$LLVMREL}/lib/Ariadne.so "

options = {}
optparse = OptionParser.new do |opts|
	opts.banner = $USAGE

	options[:func_dir] = "./tests"
	options[:ariadne] = false
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
	
	opts.on( '-k', '--ariadne', 'Generate ariadne transformed code' ) do
	     options[:ariadne] = true
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

def getSourceFiles ()
	source_files = []	
	Dir.glob("*\.f") do |f|
		source_files.push(f) 		
	end	
	return source_files
end

def compile(options, source_files)
	source_files.each do |file|
		#change float to double
		command = "#{$ARIADNE} --fortran-by-value #{file}"		
		puts command if options[:verbose]
		`#{command}`
		#perform Ariadne transformation
		if options[:ariadne] then
		  name = file.gsub(".f","")
		  command = "#{$OPT} #{$OPTFLAGS} -Ariadne -o #{name}.o < #{name}.o 2> #{name}.log"
		  puts command if options[:verbose]
		  `#{command}`
		  command = "#{$OPT} #{$OPTFLAGS} -MainAdder -o #{name}.o < #{name}.o 2> #{name}.log"
		  puts command if options[:verbose]
		  `#{command}`
		end
	end
end

####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")
  #command = "export __ARIADNE_NORMAL=0"
  #`#{command}`
  source_files = getSourceFiles()
  if not source_files.empty?
      compile(options, source_files)	
  end
exit 0