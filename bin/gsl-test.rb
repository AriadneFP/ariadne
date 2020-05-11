#!/usr/bin/ruby

require 'fileutils'
require 'optparse'

require 'timeout'
require 'open3'


$USAGE="gsl-test [-hv] [-d func-dir]"

$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$RKLEE		= "#{$REPOROOT}/bin/rklee.rb"
$ARIADNE		= "#{$REPOROOT}/bin/ariadne.rb"
$KLEE		= `which klee 2>&1`.chomp
$KLEE_OPTIONS 	= "-libc=klee -suppress-external-warnings -use-cex-cache -use-ariadne -watchdog -max-time=300 -ariadne-time=60"

$GMPLIB		= "#{$REPOROOT}/gmp-4.3.2/lib"
$GSLLIB		= "#{$REPOROOT}/klee/gsl-1.14/lib"

$LLVMLD="llvm-ld"
$FP_env_dir = "FP_env/"
$EXE_dir = "EXE/"
$TESTFILE = "test"
$LOGFILE = "gsl-test.log"
$SCRIPT_TIMEOUT = 600
$processID
$LOGFILE = "gsl-test.log"

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

# This is the pattern we use to detect KLEE timeout errors
KLEE_TIMEOUT_ERROR = /KLEE: WATCHDOG: time expired/

# This is the pattern we use to detect KLEE halting errors
KLEE_HALT_ERROR = /KLEE: HaltTimer invoked/

def makeDirs( function, bound )
  dirList = [
    "SAT", "UNSAT", "UNKNOWN", "IntensiveConcretization", 
    "IntensiveSuccessConstraints", "IntensiveFailConstraints",
    "Constraints", "Queries", "CurrentConstraints",
    "Constraints/#{function}.#{bound}", "Queries/#{function}.#{bound}",
    "CurrentConstraints/#{function}.#{bound}", 
    "IntensiveConcretization/#{function}.#{bound}"  
  ]
  
  dirList.each do |dirName|
    if not File.directory?(dirName)
      Dir.mkdir(dirName)
    end
  end
end

# return 0 if success
# return 1 if RESERROR
# return 2 if timeout
def run_klee( function, bound )
	logFile = File.new($LOGFILE, "a+")
	logFile.puts "Running function: #{function} with bound #{bound}"
	makeDirs(function, bound)
	logFile = File.new($LOGFILE, "a+")
	logFile.puts "Running function: #{function} with bound #{bound}"
	logFile.close()
	error = 0
	bound < 1 ? ENV['__ARIADNE_BOUNDMAX'] = "" : ENV['__ARIADNE_BOUNDMAX'] = "#{bound}"
	bound < 1 ? outfilename = "#{function.gsub(".o","")}.infinity.out" : outfilename = "#{function.gsub(".o","")}.#{bound}.out"
	start = Time.now

	#The following begin uses ruby's timeout to pass over .o files which take "too long"
	begin
		$timeout = true
		pid = Process.fork do
		      Process.setsid
		      command = "#{$LLVMLD} -native -disable-opt -L #{$GMPLIB} "
		      command += "-lgmp -L #{$GSLLIB} -lgsl -lgslcblas -lm #{$EXE_dir}#{function} "
		      command += "-o #{$TESTFILE}\n"
		      `#{command}`
		      command = "#{$KLEE} #{$KLEE_OPTIONS} #{function} 2>&1 |tee #{outfilename}"
		      `#{command}`
		end
		Timeout::timeout($SCRIPT_TIMEOUT) do
		      Process.waitpid(pid)
		end
	rescue Timeout::Error
	      Process.kill("HUP", pid)
	      puts "#{function} hung its internal timeout!"
	      error = 2
	end
	
	elapsed = Time.now - start 
	outfile = File.new(outfilename, "a+")
	outfile.puts "\nElapsed = #{elapsed}"
	outfile.pos = 0 #set the position at the start of the file
	outfile.each do |line| 
		if line =~ RESERROR
		    error = 1
		    break
		end
		if line =~ KLEE_TIMEOUT_ERROR
		    error = 2
		    break
		end
		if line =~ KLEE_HALT_ERROR
		    error = 2
		    break
		end

	end
	`mv SAT UNSAT UNKNOWN Constraints/#{function}.#{bound}`
	`mv IntensiveSuccessConstraints IntensiveFailConstraints IntensiveConcretization/#{function}.#{bound}`
	if File.exist?("currentConstraints.smt")
	  `mv currentConstraints.smt CurrentConstraints/#{function}.#{bound}`
	end
	return error
end

def build_with_loop_control
	# Modify the Makefile to rebuild with loop control
	# Find the line containing "CC = $(ROOT)/../bin/ariadne.rb -k -m --"
	# and replace it "CC = $(ROOT)/../bin/ariadne.rb -klm --"
	if not File.exists?("Makefile")
		stderr.puts "Error: No Makefile found."
		exit 0
	end
 	FileUtils.cp "Makefile", "Makefile.orig"
	cmd = 'sed -i -e\'s/\(.*\)ariadne\.rb -k -m\(.*\)/\1ariadne\.rb -klm\2/\' Makefile'
	`#{cmd}`
	if $?.exitstatus != 0
		$stderr.puts "sed failed"
		exit 1
	end
	`make clean`
	`make`
end

def getListOfFiles(options)
    fileList = []
    Dir.glob("main*\.o") do |f|
	fileList.push(f)
    end
    return fileList
end

def getUnknownPaths(fileName)
  unknownPaths = 0
  statFile = File.new("Unknown.stat")
  while (line = statFile.gets) 
    if (line.include? fileName)
      unknownStr = line.gsub("#{fileName}\t", "")
      unknownPaths = unknownStr.to_i
      break
    end
  end  
  unknownPaths
end

def test_infinity(options)
	failed_files = []	
	Dir.glob("main*\.o") do |f|
		puts "#{f}" if options[:verbose]
		error = run_klee(f, -1)
		failed_files.push(f) if error != 0
	end
	return failed_files
end

def test_bounded(options, fileList)
	error = 0
	bounds = [] 
	7.times { |i| bounds << 2**i }
	bounds.reverse!
	fileList.each do |f|
		puts "#{f}" if options[:verbose]
		bounds.each do |b|
			puts ".#{b}" if options[:verbose]
			run_klee( f, b )
		end
	end
end

# After running the script if changes Makefile, should rechange it to orginal. 
def restoreMakefile() 
      cmd = 'sed -i -e\'s/\(.*\)ariadne\.rb -klm \(.*\)/\1ariadne\.rb -k -m \2/\' Makefile'
      `#{cmd}`
end


####### MAIN ##############################
Dir.chdir("#{options[:func_dir]}")

failed_files = test_infinity(options)
fileList = getListOfFiles(options)
if not fileList.empty?
	build_with_loop_control()
	test_bounded(options, fileList)
	restoreMakefile()
end

exit 0
