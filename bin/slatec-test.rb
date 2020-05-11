#!/usr/bin/ruby

require 'fileutils'
require 'optparse'

require 'timeout' #debugging

$USAGE="gsl-test [-hv] [-d func-dir]"

$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$RKLEE		= "#{$REPOROOT}/bin/rklee.rb"
$ARIADNE		= "#{$REPOROOT}/bin/ariadne.rb"
$KLEE		= `which klee 2>&1`.chomp

options = {}
optparse = OptionParser.new do |opts|
	opts.banner = $USAGE

	options[:func_dir] = "./tests"
	opts.on(
		'-d', '--funcdir dir',
		'Directory in which ariadne produced tests reside')  do |dir|
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



# This is the pattern we use to detect out of resource errors to trigger
# applying the loop control transformation.  Here we are assuming that this
# pattern is necessary and sufficient for detecting these errors.
RESERROR = /std::bad_alloc/

# return 0 if success
# return 1 if RESERROR
# return 2 if timeout
def run_klee( function )	
	
	outfilename = "#{function.gsub(".o","")}.out"
	start = Time.now

	#The following begin uses ruby's timeout to pass over .o files which take "too long"
	begin
		Timeout::timeout(600) do
			command = " #{$KLEE} #{function} 2>&1 |tee #{outfilename}"
			`#{command}`
		end 
	rescue Timeout::Error
		puts "#{function} too slow!"
		error = 2		
	end		
	
	#system "#{$RKLEE} -p #{function} > #{outfilename} 2>&1"
	#command = " #{$KLEE} #{function} 2>&1 |tee #{outfilename}"
	#`#{command}`
	elapsed = Time.now - start 
	outfile = File.new(outfilename, "a+")
	outfile.puts "\nElapsed = #{elapsed}"
	outfile.pos = 0 #set the position at the start of the file
	outfile.each do |line| 
		#puts "line = #{line}" #debugging
		if line =~ RESERROR
			error = 1
			break
		end
	end
	#puts error #debugging
	return error
end

# Run over the whole directory and get the list of failed files
def run(options)
	failed_files = []	
	Dir.glob("main*\.o") do |f|
		puts "#{f}" if options[:verbose]
		error = run_klee(f)
		failed_files.push(f) if error != 0
	end
	#puts failed_files.inspect #debugging
	return failed_files
end


####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")

failed_files = run(options)

exit 0