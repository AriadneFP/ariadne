AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --
# -*-makefile-*-

noinst_LTLIBRARIES = libgslroots.la 

pkginclude_HEADERS = gsl_roots.h

noinst_HEADERS = roots.h

INCLUDES = -I$(top_srcdir)

libgslroots_la_SOURCES = bisection.c brent.c falsepos.c	newton.c  secant.c steffenson.c	convergence.c fsolver.c fdfsolver.c 

check_PROGRAMS = test

TESTS = $(check_PROGRAMS)

test_SOURCES = test.c test_funcs.c test.h
test_LDADD = libgslroots.la ../ieee-utils/libgslieeeutils.la  ../err/libgslerr.la ../test/libgsltest.la ../sys/libgslsys.la ../utils/libutils.la

