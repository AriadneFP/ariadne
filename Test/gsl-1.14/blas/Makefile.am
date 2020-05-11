AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --
noinst_LTLIBRARIES = libgslblas.la

pkginclude_HEADERS = gsl_blas.h gsl_blas_types.h

INCLUDES = -I$(top_srcdir)

libgslblas_la_SOURCES = blas.c 

check_PROGRAMS = test
TESTS = test
test_LDADD = libgslblas.la ../ieee-utils/libgslieeeutils.la ../err/libgslerr.la ../test/libgsltest.la ../sys/libgslsys.la
test_SOURCES = test_blas_raw.c test_cases.c test_cases.h


