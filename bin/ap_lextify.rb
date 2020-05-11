#!/usr/bin/ruby -w

# == Synopsis 
#   Parses outfiles, 
#		TODO: write this
#
# == Examples
#		TODO: write this
#
# == Usage 
#		TODO: write this
#		The following command 
#   	ap_lextify.rb [options]
#
#   For help use: ap_lextify.rb -h
#
# == Options
#   -h, --help          Displays help information
#		-V, --verbose				Run verbosely
#		-q, --quiet					Run quietly


require 'optparse'
require 'outfile' #defines an Outfile class


# This class define a ruby application which runs a named obfuscation engine.
class APLextify
	#TODO: attr_reader :field1, :field2
	
	# Initialization of this application requires the command line arguments.
	def initialize(arguments)
		@arguments = arguments		
		# Set defaults
		@opt_parser = nil
		@options = {:help=>false,:verbose=>false,:quiet=>false}
		#TODO: initialize class fields here		
	end

	# Parse options, check arguments, then process the command
	def run
		if parsed_options? && arguments_valid?
			output_options if @options[:verbose]
			output_arguments if @options[:verbose]
			process_arguments
			process_command
		else
			output_usage
		end
	end

	# Parse the options
	def parsed_options?
		#configure an OptionParser
		@opt_parser = OptionParser.new do |opts|		
			opts.banner = "Usage: #{__FILE__} [options] arguments_go_here"
			opts.separator ""
			opts.separator "Specific options:"
			
			opts.on('-h', '--help', 'displays help information') do
				@options[:help] = true
				output_help
			end
			opts.on('-V', '--verbose', 'Run verbosely') { @options[:verbose] = true }
			opts.on('-q', '--quiet', 'Run quietly') { @options[:quiet] = true }
		end
	
		@opt_parser.parse!(@arguments) rescue return false
	
		process_options
		true
	end

	# True if required arguments were provided
	def arguments_valid?
		num_required = 0
		return false if @arguments.length != num_required
		# TO DO - check if arguments are valid
		process_arguments
	end

	# Performs post-parse processing on options
	# For instance, some options may cancel others or have higher importance
	def process_options
		@options[:verbose] = false if @options[:quiet]
	end

	# Setup the arguments
	def process_arguments
		# store arguments in proper class fields
		# check for errors in arguments like existence of files/directories
		# return false if failed or raise exception
		return true
	end

	# Application logic
	def process_command		
		#~ Test run
		#~ filename = "gsl_sf_bessel_J0.out"
		#~ outfile = Outfile.new(filename)
		#~ puts outfile.underflow.inspect
		#~ puts outfile.overflow.inspect
		
		#~ Test run
		#~ filename = "gsl_sf_bessel_asymp_Mnu_e.out"
		#~ outfile = Outfile.new(filename)
		#~ outfile.underflow.each { |e| puts e.inspect}
		#~ outfile.overflow.each { |e| puts e.inspect}
		
		#ofiles = []
		# get ofiles from some directory
		# repo-path/Test/gsl-1.14 
		# files have extension .o
		
		#ofiles.each do |o|
			#inputfiles = []
			#ifstr = ""
			#E.each do |e|
				# inputfile = "write e.value to inputfilename"
				# inputfiles.push (inputfile)
			#inputfiles.each do |if|
				#ifstr = ifstr + of + " "
			#end
			# output = `lli *.o ifstr`
			# if($?.existatus == 0)
			#		out_value = output.chomp.to_f
			#	end
			# result = out_value < omega
			# how and where to store result?
		return 0
	end

	def output_help
		puts @opt_parser
		exit 1
	end
	
	def output_usage
		puts @opt_parser
		exit 1
	end

	def output_options
		puts "Options:"
		@options.each do |name, value|
			puts "\t#{name} = #{value}"
		end
	end
	
	def output_arguments
		puts "Arguments:"
		#print out arguments passed in
	end
end
		
		
#Create and run the application
if __FILE__ == $0
	app = APLextify.new(ARGV)
	exit app.run
end
