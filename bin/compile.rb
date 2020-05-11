#!/usr/bin/ruby

$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$LLVMREL 	= "#{$REPOROOT}/llvm-2.7/Release+Debug/"
$OPT 		= "#{$LLVMREL}/bin/opt"
def getBitcodeFiles ()
	bitcode_files = []	
	Dir.glob("main*\.o") do |f|
		bitcode_files.push(f)		
	end	
	return bitcode_files
end

def link(bitcode_files)
    bitcode_files.each do |file|
	command = "llvm-ld -link-as-library -disable-opt lib.o #{file} -o #{file}"
	`#{command}`
    end	
end

def generateLib()
  `rm *.rand`
  `rm *.o`
  `make clean;make`
  list = ""
  Dir.glob("*\.o") do |f|
    list += "#{f} " 
  end
  command = "llvm-ld #{list} -link-as-library -disable-opt -o lib.o"
  `#{command}`
end

####### MAIN ##############################
generateLib()
command = "#{$OPT} -load #{$LLVMREL}/lib/Ariadne.so -Ariadne -MainAdder -o libT.o < lib.o 2> lib.log"
`#{command}`
exit 0
