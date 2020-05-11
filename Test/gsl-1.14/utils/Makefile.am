AUTOMAKE_OPTIONS = -Wno-gnu
CFLAGS = -g -emit-llvm -O2
ROOT=`readlink -f \`git rev-parse --git-dir\``
CC=$(ROOT)/../bin/ariadne.rb -k -m --
noinst_LTLIBRARIES = libutils.la

# Don't need to list alloca.c, etc., Automake includes them.
libutils_la_SOURCES = system.h placeholder.c
libutils_la_LIBADD = @LIBOBJS@ 

EXTRA_DIST = README
