AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --
noinst_LTLIBRARIES = libgslcheb.la 

pkginclude_HEADERS = gsl_chebyshev.h

INCLUDES = -I$(top_srcdir)

libgslcheb_la_SOURCES =  deriv.c eval.c init.c integ.c

TESTS = $(check_PROGRAMS)

check_PROGRAMS = test

test_LDADD = libgslcheb.la ../ieee-utils/libgslieeeutils.la ../test/libgsltest.la ../sys/libgslsys.la ../err/libgslerr.la ../utils/libutils.la

test_SOURCES = test.c


