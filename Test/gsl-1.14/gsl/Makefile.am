AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --

header-links: remove-links
	HEADERLIST="$(top_srcdir)/gsl*.h $(top_srcdir)/*/gsl*.h"; \
	for h in $$HEADERLIST; do \
	  BASENAME=`basename $$h`; \
	  test -r $$BASENAME || $(LN_S) $$h $$BASENAME; \
	done

remove-links: 
	rm -f gsl*.h


all: all-am header-links

clean: clean-am remove-links
distclean: distclean-am remove-links
	-rm -f Makefile
