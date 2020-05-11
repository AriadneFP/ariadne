AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --
noinst_LTLIBRARIES = libgsldiff.la

INCLUDES = -I$(top_srcdir)

libgsldiff_la_SOURCES = diff.c

pkginclude_HEADERS = gsl_diff.h

TESTS = $(check_PROGRAMS)
check_PROGRAMS = test #demo

test_SOURCES = test.c
test_LDADD =  libgsldiff.la  ../vector/libgslvector.la ../block/libgslblock.la  ../ieee-utils/libgslieeeutils.la ../err/libgslerr.la ../test/libgsltest.la ../sys/libgslsys.la

#demo_SOURCES = demo.c
#demo_LDADD =  libgsldiff.la  ../vector/libgslvector.la ../block/libgslblock.la  ../err/libgslerr.la ../test/libgsltest.la ../sys/libgslsys.la
