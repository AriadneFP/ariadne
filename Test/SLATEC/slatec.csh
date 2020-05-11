#!/bin/csh
#
mkdir temp
cd temp
rm *
f90split ../slatec.f90
#
foreach FILE ( `ls -1 *.f90` )
  F90 -c -g $FILE >& compiler.txt
  if ( $status != 0 ) then
    echo "Errors compiling " $FILE
    exit
  endif
  rm compiler.txt
end
rm *.f90
#
ar qc libslatec.a *.o
rm *.o
#
mv libslatec.a ~/lib/$ARCH
cd ..
rmdir temp
#
echo "Library installed as ~/lib/$ARCH/libslatec.a"
