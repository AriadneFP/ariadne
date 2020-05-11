1. Build LLVM
	$ cd llvm-2.7 
	# Need to build llvm with path to llvm-gcc
	$./configure --with-llvmgccdir=<path to llvm-gcc parent dir> 
		--with-llvm-gcc=<path to llvm-gcc> (executable file) 
		--with-llvm-gxx=<path to llvm-g++> (executable file)	 
	$ make clean; make

	# To build LLVM in debugging mode, add these following options:
	$ ./configure --enable-expensive-checks 
		--enable-debug-runtime --enable-debug-symbols
	# For this option, we will change the building process of KLEE, 
	# Release -> Release+Debug
	$ make clean; make

2. Build GMP
	$ cd gmp-4.3.2 
	$ ./configure --prefix=<path to gmp>
	$ make install
	$ make check

4. Run LLVM transformations
	$ cd llvm-2.7/lib/Transforms/Kali
	$ make
	# To use the scripts, set the PATH variable to the bin directory.
	export PATH=$PATH:<path to ndev/bin>
	# To transform a single input file, issue this
	$ kali.rb -k <source file> 
 
5. Build GSL 
    $ Go to [klee,Test]/gsl*
	$ autoreconf --install
	$ ./configure --prefix=<path to gsl> 
	$ cd gsl ; make ; cd ..
	$ make install
  
6. Build KLEE
	# Modify Makefile.common to link to correct Z3 direction. With 64bit machine, 
	# use Z3_64, with 32bit machines, use Z3 as default.
	$ git submodule update --init klee
	$ cd klee
	# Follow the build GSL instructions above
	$ ./configure --with-llvm=../llvm-2.7
	$ cd fparser
	$ llvm-g++ -fPIC -c -g fparser.cc
	$ llvm-g++ -fPIC -c -g fpoptimizer.cc
	$ cd ..
	$ make clean; make

	# Note: To build fparser, we need to uncomment lines setting MPFR and GMP 
	#		in fpconfig.h.  todo:  Why 

	# If we build LLVM with debug mode, we need to copy Release+Debug -> Release
	$ cp -r Release+Debug Release 
	
	# To interact with our tool on the command line, add klee to your path. 
	export PATH=$PATH:/path/to/klee/Release/bin:/path/to/ndev/bin
	
7. Build GSL for testing
	$ Go to <repo-root>/Test/gsl*
	$ autoreconf --install
	$ ./configure --prefix=<path to gsl>
	$ make install

8. After above steps, run Klee on an object file as follows:
	$ klee <bitcodefile>
	or you can use the script rklee.rb to export the output 
	to files.
