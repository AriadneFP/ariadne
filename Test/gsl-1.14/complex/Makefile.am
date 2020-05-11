AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --
noinst_LTLIBRARIES = libgslcomplex.la 

pkginclude_HEADERS = gsl_complex.h gsl_complex_math.h

INCLUDES = -I$(top_srcdir)

libgslcomplex_la_SOURCES = math.c inline.c

TESTS = $(check_PROGRAMS)
check_PROGRAMS = test

test_SOURCES = test.c results.h results1.h results2.h results_real.h
test_LDADD = libgslcomplex.la ../err/libgslerr.la ../ieee-utils/libgslieeeutils.la ../err/libgslerr.la ../test/libgsltest.la ../sys/libgslsys.la  ../utils/libutils.la

