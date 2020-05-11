#!/usr/bin/ruby

$REPOROOT	= "#{`readlink -f \`git rev-parse --git-dir\``.chomp("/.git\n")}"
find / -name ./gsl-1.14/*Makefile.am | xargs sed -i -e '1 i =#{REPOROOT}/bin/ariadne.rb --' 

ROOT=`readlink -f \`git rev-parse --git-dir\``; \
find Test/gsl-1.14/ -name Makefile.am \
| xargs sed -i -e '1 i CC="${ROOT%"/.git"}/bin/ariadne.rb --"'

find Test/gsl-1.14/ -name Makefile.am \
| xargs sed -i -e '1 i CC=$(ROOT)/../bin/ariadne.rb --'

find Test/gsl-1.14/ -name Makefile.am \
| xargs sed -i -e '1 i ROOT=`readlink -f \\`git rev-parse --git-dir\\`\`'

ROOT=`readlink -f \`git rev-parse --git-dir\``
ROOT=`readlink -f `git rev-parse --git-dir\``