# This class defines a parsed .out file
# Useful properties of the parsed .out file can be provided if the parse method
# is modified.
class Outfile
	attr_reader :overflow, :underflow

	def initialize(filename)
		@filename = filename
		@underflow = {}
		@overflow = {}
		parse
	end
	
	# Parses a .out file
	# Underflow and overflow exceptions are stored in object fields
	def parse
		file = File.new(@filename, "r")
		line = file.gets.chomp
		while !line.nil?
			if line =~ /(Underflow|Overflow)/ #if exception
				line =~ /Overflow/ ? overflow = true : overflow = false
				identifier = line[/[0-9][0-9]*/] #get exception identifier
				line = file.gets.chomp					
				line = file.gets.chomp if line =~ /The constraints are unsat/				
				line = file.gets.chomp if line =~ /KLEE: WARNING:/				
				# For a given identifier, there can me multiple warnings of a potential 
				# overflow/underflow, store only the last one.
				# By creating a new array, previous values are overwritten
				values = []
				while line =~ /^arr[0-9]/	#if arr value
					line = file.gets.chomp
					#~ puts "#{identifier}: #{line}"	#debugging
					values.push line #store value as sring with no regard to type (rational, float, integer)					
					line = file.gets.chomp
				end
				if not values.empty? #if some arr values were found, store them					
					overflow ? @overflow["#{identifier}"] = values : @underflow["#{identifier}"] = values
				end
			else 
				line = file.gets #if not exception warning
			end
		end #iterate over lines in file
	end #parse
end #Outfile
