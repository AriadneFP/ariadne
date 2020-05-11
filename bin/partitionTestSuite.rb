#!/usr/bin/ruby

require 'fileutils'

#$REPOROOT= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$REPOROOT = "/home/thanh/kali/ndev"

$PARTITION_NUM = 18
$TOTAL = 480
$FUNCTIONPERPARTITION = ($TOTAL / $PARTITION_NUM) + 1

$NUM_OF_FUNCTIONS = 18
$TEST_ID = 1

$listFileName = "functionList.txt";

def exportFunctionList() 
  fileList = [];
  Dir.glob("main*\.o") do |f|
	fileList.push(f)
  end
  listFile = File.new( $listFileName, "w")
  for i in ($NUM_OF_FUNCTIONS * $TEST_ID ..($NUM_OF_FUNCTIONS * ($TEST_ID + 1) - 1) )
      listFile.puts(fileList[i]);
  end
  listFile.close()
end



def removeDirectories()
  counter = 1
  while( counter <= $PARTITION_NUM)
    Dir.rmdir("../Test#{counter}");
    counter = counter + 1
  end
end

def initializeDirectories()
  counter = 1
  while( counter <= $PARTITION_NUM)
    if not File.directory?("../Test#{counter}")
      Dir.mkdir("../Test#{counter}");
    end 
    FileUtils.cp_r(".", "../Test#{counter}");
    if File.exist?("../Test#{counter}/*.o")
      FileUtils.rm("../Test#{counter}/*.o");
    end
    counter = counter + 1
  end
end

def cleanDirectories()
  counter = 1
  while( counter <= $PARTITION_NUM)
    FileUtils.rm_rf("../Test#{counter}/klee*");
    FileUtils.rm("../Test#{counter}/*.exe");
    FileUtils.rm("rm ../Test#{counter}/*.bc");
    FileUtils.rm("../Test#{counter}/*.ll");
    FileUtils.cp_r("EXE", "../Test#{counter}");
    FileUtils.rm("../Test#{counter}/*.o");
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
  numPerDirect = numOfFunctions / $PARTITION_NUM + 1
  file = File.new(functionList);
  while (line = file.gets)
    counter = counter + 1
    directID = (counter / numPerDirect)  + 1
    if (directID > 18)
      puts "error!"
    end
    `cp #{line.chomp("\n")} ../Test#{directID}/#{line}`    
  end
end

def copyData()
  counter = 1
  while ( counter <= $PARTITION_NUM )
    FileUtils.cp("#{$REPOROOT}/Test/gsl-1.14/Test#{counter}/gsl-test.log", "./Logs/gsl-test#{counter}.log");
    #FileUtils.cp("#{$REPOROOT}/Test/gsl-1.14/Test#{counter}/*.out", "./Result");
    `cp #{$REPOROOT}/Test/gsl-1.14/Test#{counter}/*.out ./Result`;
    FileUtils.cp_r("#{$REPOROOT}/Test/gsl-1.14/Test#{counter}/Constraints", "./Constraints/Test#{counter}");
    FileUtils.cp_r("#{$REPOROOT}/Test/gsl-1.14/Test#{counter}/CurrentConstraints", "./CurrentConstraints/Test#{counter}");
    counter = counter + 1
  end
end

####### MAIN ##############################
#initializeDirectories();
#cleanDirectories();
#exportFunctionList();
#copy_FunctionList("longRunList.log");
#copy_FunctionList("shortRunList.log");
#copy_FunctionList($listFileName);
copyData();
