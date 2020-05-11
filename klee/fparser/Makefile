SHELL=/bin/bash
ifeq ($(shell uname -m),x86_64)
	is64 = true
else
	is64 = false
endif
ifeq ($(is64),true)
	LLVMGCCDIR:=../../llvm-gcc/llvm-gcc4.2-2.7-x86_64-linux
else
	LLVMGCCDIR:=../../llvm-gcc/llvm-gcc-4.2-2.7-i686-linux
endif

LLVMGXX = $(LLVMGCCDIR)/bin/llvm-g++
all: fparser.o fpoptimizer.o
	if [ ! -d "lib" ]; then mkdir lib; fi
	$(LLVMGXX) -shared -Wl,-soname,libfparser.so \
	-o lib/libfparser.so.0.1.0 *.o >/dev/null 2>&1;
	ldconfig -n lib	
fparser.o: fparser.cc
	$(LLVMGXX) -fPIC -c -g fparser.cc;
fpoptimizer.o: fpoptimizer.cc
	$(LLVMGXX) -fPIC -c -g fpoptimizer.cc;
clean:
	rm *.o lib/*