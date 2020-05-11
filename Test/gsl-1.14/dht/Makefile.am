AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --
noinst_LTLIBRARIES = libgsldht.la 

pkginclude_HEADERS = gsl_dht.h

INCLUDES = -I$(top_srcdir)

TESTS = $(check_PROGRAMS)

check_PROGRAMS = test

test_LDADD = libgsldht.la ../specfunc/libgslspecfunc.la ../complex/libgslcomplex.la ../ieee-utils/libgslieeeutils.la ../err/libgslerr.la ../test/libgsltest.la ../sys/libgslsys.la ../utils/libutils.la

test_SOURCES = test.c

libgsldht_la_SOURCES = dht.c
