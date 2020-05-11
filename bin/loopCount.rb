#!/usr/bin/ruby

def hasLoop(functionName, fileName) 
  file = File.new(fileName)
  hasFunction = false
  while (line = file.gets )
    if ( line =~ /#{functionName}[\w]*/)
      hasFunction = true
    end
    if hasFunction
      if ( (line.include? 'for (') || (line.include? 'while') )
	return true
      end
      if ( line.chomp!.eql?('}') )
	return false
      end
    end
  end
  return false
end

def countLoop()
    loopFunctions = 0
    functionFile = File.new("functionList.log")
    while ( functionName = functionFile.gets )
	functionName.chomp!
	Dir.glob("*\.c") do |fileName|
	    if (hasLoop( functionName, fileName )) 
	      loopFunctions = loopFunctions + 1
	      puts functionName
	      break
	    end	    
	end
    end
    #puts loopFunctions    
end

#### MAIN ####
countLoop()

exit 0
