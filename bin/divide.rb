#!/usr/bin/ruby

DIRECT_NUM = 18
TOTAL = 480
FUNCTIONPERDIRECT = (TOTAL / DIRECT_NUM) + 1

$REPOROOT= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"

def removeDirectories()
  counter = 1
  while( counter <= DIRECT_NUM)
    `rm -rf ../Test#{counter}`
    counter = counter + 1
  end
end

def createDirectories()
  counter = 1
  while( counter <= DIRECT_NUM)
    `mkdir ../Test#{counter}`
    `cp -rf . ../Test#{counter}`
    `rm ../Test#{counter}/*.o`
    counter = counter + 1
  end
end

def cleanDirectories()
  counter = 1
  while( counter <= DIRECT_NUM)
    #`rm -rf ../Test#{counter}/klee*`
    #`rm ../Test#{counter}/*.exe`
    #`rm ../Test#{counter}/*.bc`
    #`rm ../Test#{counter}/*.ll`
    #`cp -rf EXE ../Test#{counter}`
    `rm ../Test#{counter}/*.o`
    counter = counter + 1
  end
end

def getNumofFunctions(functionList)
  file = File.new(functionList);
  counter = 0;
  while (line = file.gets)
    counter = counter + 1;
  end
  counter;
end

def copy_FunctionList(functionList)
  counter = 0
  numOfFunctions = getNumofFunctions(functionList)
  numPerDirect = (numOfFunctions / DIRECT_NUM) + 1
  file = File.new(functionList);
  while (line = file.gets)
    counter = counter + 1
    directID = (counter / numPerDirect)  + 1
    `cp #{line.chomp("\n")} ../Test#{directID}/#{line}`
    #puts "cp #{line} ../Test#{directID}/#{line}"
  end
end

def copyData()
  counter = 1
  while ( counter <= DIRECT_NUM )
    `cp #{$REPOROOT}/Test/gsl-1.14/Test#{counter}/gsl-test.log ./Logs/gsl-test#{counter}.log`
    `cp #{$REPOROOT}/Test/gsl-1.14/Test#{counter}/*.out ./Result`
    `cp -r #{$REPOROOT}/Test/gsl-1.14/Test#{counter}/Constraints ./Constraints/Test#{counter}`
    `cp -r #{$REPOROOT}/Test/gsl-1.14/Test#{counter}/CurrentConstraints ./CurrentConstraints/Test#{counter}`
    counter = counter + 1
  end
end

####### MAIN ##############################
#createDirectories();
cleanDirectories();
copy_FunctionList("longRunList.log");
copy_FunctionList("shortRunList.log");
#copyData();
