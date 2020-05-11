#!/usr/bin/ruby

`llvm-gcc -c -g -emit-llvm lib_test.c`
`llvm-gcc -c -g -emit-llvm lib_test_main.c`
`llvm-ld lib_test.o lib_test_main.o -link-as-library -disable-opt -o lib_test_main.o`