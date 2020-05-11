#!/usr/bin/ruby

require 'open3'
require 'optparse'
require 'pp'

$LLVMLD="llvm-ld"
$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
$GMPLIB		= "#{$REPOROOT}/gmp-4.3.2/lib"
$GSLLIB		= "#{$REPOROOT}/klee/gsl-1.14/lib"
$TESTFILE 	= "test"

ARGV.each do |file|
	command = "#{$LLVMLD} -native -disable-opt -L #{$GMPLIB} -lgmp -L #{$GSLLIB} -lgsl -lgslcblas -lm #{file} -o #{file.gsub(".o","")}.exe\n"
	#command += "\./#{$TESTFILE}"
	puts command
	`#{command}`
end
