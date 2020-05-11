AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --
noinst_LTLIBRARIES = libgslerr.la

pkginclude_HEADERS = gsl_errno.h gsl_message.h

libgslerr_la_SOURCES = error.c stream.c message.c strerror.c

check_PROGRAMS = test

TESTS = $(check_PROGRAMS)

test_SOURCES = test.c
test_LDADD = libgslerr.la ../test/libgsltest.la ../sys/libgslsys.la ../utils/libutils.la
