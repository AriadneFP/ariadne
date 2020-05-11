SHELL=/bin/bash
TMP = $(shell readlink -f $(shell git rev-parse --git-dir))
REPOROOT := $(subst /.git,"",$(strip $(TMP)))

LLVM = llvm-2.7
GMP = gmp-4.3.2
SUBDIRS = $(LLVM) Ariadne $(GMP) klee Test 

ifeq ($(shell uname -m),x86_64)
	is64 = true
else
	is64 = false
endif
ifeq ($(is64),true)
	LLVMGCC4 := $(REPOROOT)/llvm-gcc/llvm-gcc4.2-2.7-x86_64-linux/
else
	LLVMGCC4 := $(REPOROOT)/llvm-gcc/llvm-gcc-4.2-2.7-i686-linux/
endif

KLEEMAKEFILES = klee/Makefile.config.in klee/Makefile.common.32 \
				klee/Makefile.common.64 


.PHONY: all
all: SANITY_CHECK $(SUBDIRS)
	if [[ ! "$(PATH)" =~ .*$(REPOROOT)/klee.* ]] ; then \
		echo "Your PATH does not include klee.  You should source "; \
		echo $(REPOROOT)/bin/ariadne_setenv; \
	fi


.PHONY: SANITY_CHECK
SANITY_CHECK:
	@which ruby &>/dev/null || ( echo "ruby not found."; exit 1 )
	@which m4 &>/dev/null || ( echo "m4 not found."; exit 1 )
	@which autoreconf &>/dev/null || \
		( echo "autoconf not installed."; exit 1 );
	@which libtool &>/dev/null || ( echo "libtool not found."; exit 1 )
	@which g++ &>/dev/null || ( echo "g++ not found."; exit 1 )
	@which curl &>/dev/null || ( echo "curl not found."; exit 1 )
	@which python &>/dev/null || ( echo "python not found."; exit 1 )
	@which bison &>/dev/null || ( echo "bison not found."; exit 1 )
	@which bc &>/dev/null || ( echo "bc not found."; exit 1 )

.PHONY: $(LLVM)
$(LLVM): $(LLVM)/Makefile.config
	$(MAKE) -C $@ $(PARAM)

$(LLVM)/Makefile.config: $(LLVM)/Makefile.config.in $(LLVM)/Makefile.rules
	@echo "Configuring $(LLVM)"
	cd $(LLVM); \
	./configure \
        --with-llvmgccdir=$(LLVMGCC4) \
        --with-llvm-gcc=$(LLVMGCC4)/bin/llvm-gcc \
        --with-llvm-gxx=$(LLVMGCC4)/bin/llvm-g++ \
        --enable-debug-runtime \
        --enable-debug-symbols; \
	make clean

Makefile.%: ;


.PHONY: $(GMP)
$(GMP): $(GMP)/Makefile
	$(MAKE) -C $@ $(PARAM)

$(GMP)/Makefile: $(GMP)/Makefile.am $(GMP)/Makefile.in
	@echo "Configuring $(GMP)"
	cd $(GMP); \
	./configure --prefix=$(REPOROOT)/gmp-4.3.2; \
    make clean; \
    make install; \
    make check



.PHONY: Ariadne
Ariadne: 
	$(MAKE) -C $(LLVM)/lib/Transforms/Ariadne $(PARAM)


.PHONY: GSL
GSL: $(GSLDIR)/Makefile
	cd $(GSLDIR); \
	if [[ "$(GSLDIR)" =~ .*Test.* ]] ; then \
		make; \
	else \
		LIBGSL = "$(REPOROOT)/$(GSLDIR)/lib/libgsl.so"; \
		LIBGSLCBLAS = "$(REPOROOT)/$(GSLDIR)/lib/libgslcblas.so"; \
		if [ ! -e $(LIBGSL) ] || [! -e $(LIBGSLCBLAS)]; then \
			make clean; \
			make install; \
		else \
			make install; \
		fi \
	fi

$(GSLDIR)/Makefile: $(GSLDIR)/Makefile.am
	if [ -z "$(GSLDIR)" ] ; then echo "GSLDIR not set"; exit 1; fi
	cd $(GSLDIR); \
	autoreconf --install; \
	if [[ "$(GSLDIR)" =~ .*Test.* ]] ; then \
		./configure --prefix=$(REPOROOT)/$(GSLDIR) --disable-shared; \
	else \
		./configure --prefix=$(REPOROOT)/$(GSLDIR); \
	fi
	cd $(GSLDIR); \
	make clean; \
	make -C gsl; \
	make install;

.PHONY: klee
klee: klee/.git klee/Makefile.in	
	$(MAKE) GSL GSLDIR=klee/gsl-1.14
	$(MAKE) -C $(REPOROOT)/klee/fparser	
	$(MAKE) -C $@ $(PARAM)

klee/.git:
	if [ ! -e klee/.git ] ; then \
		git submodule update --init klee; \
		cd klee; \
		git checkout master; \
		cd ..; \
	fi

klee/Makefile.in: $(KLEEMAKEFILES)
ifeq ($(is64),true)
	cd klee; cp Makefile.common.64 Makefile.common
else
	cd klee; cp Makefile.common.32 Makefile.common
endif
	cd klee; \
	./configure --with-llvm=../$(LLVM) --with-uclibc=uclibc

.PHONY: Test
Test:
	$(MAKE) GSL GSLDIR=Test/gsl-1.14

.PHONY: clean
clean: 
	$(MAKE) -C $(LLVM) clean
	$(MAKE) -C $(GMP) clean
	$(MAKE) -C $(LLVM)/lib/Transforms/Ariadne clean
	$(MAKE) -C klee/gsl-1.14 clean
	$(MAKE) -C klee clean
	$(MAKE) -C Test/gsl-1.14 clean	
