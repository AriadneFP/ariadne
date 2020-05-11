#!/usr/bin/ruby

require 'fileutils'
require 'optparse'
require 'open3'

require 'timeout' #debugging

$USAGE="link [-hv] [-d func-dir] [-o outfile]"

$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$LLVMLD="llvm-ld -link-as-library -disable-opt"
$OUTFILE="specfunc.o"

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
	
	opts.on(
		'-o', '--outfile file',
		'output file name')  do |file|
		options[:outfile] = file
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
	Dir.glob("main*\.o") do |f|
		#if !(f.match(/^main/)) && !(f.match(/^mathieu_workspace/)) && !(f.match(/^specfunc/))		  
		  bitcode_files.push(f) 
		#end
	end	
	return bitcode_files
end

def link(options, bitcode_files)
	
	if (File.exists? "AP_error.log")
	  `rm AP_error.log`
	end
	
	errLog = File.new("AP_error.log","a")
	bitcode_files.each do |file|
	  command = "test.rb #{file}"
	  puts command if options[:verbose]
	  Open3.popen3(command) do |cmdin, cmdout, cmderr|
	    cmdout.each do |line|
	      errLog.puts(line)	       	
	    end
	    cmderr.each do |line|
	      errLog.puts(line)
	    end	    
	  end
	end	
end

####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")
bitcode_files = getBitcodeFiles(options)
if not bitcode_files.empty?
    link(options, bitcode_files)	
end

exit 0
