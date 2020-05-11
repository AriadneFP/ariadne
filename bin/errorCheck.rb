#!/usr/bin/ruby

$UNDERSTOODERRORS = [
      "external call",  # We cannot symbolically execute 3rd party binaries.
      "memory error:",	# Currently, we do not handle the array with unknown size.  
      #Error when using Poly solver to solve polynomial that failed to converge 
      "ERROR: root solving qr method failed to converge",
      # When running the excutable over inputs we generate, we can get the 
      # message that GSL handle OVERFLOW and UNDERFLOW cases.
      "ERROR: overflow", # an example at line 780 in airy.c   
      "ERROR: underflow", # an example at line 693 in airy.c
      # Or some inputs can lead to the location that cause GSL ERRORs
      "ERROR: domain error", # an example at line 719 in file zeta.c
      "ERROR: singularity", # an example at line 827 in file psi.c
      "ERROR: x too large to extract fraction part", # an example at line 1249 gamma.c
      "ERROR: x is greater than 1.0", # an example at line 265 airy.c
      "ERROR: error", # an example at line 552 trig.c
      "WARNING: undefined reference to function", # internal function calls
      "WARNING: Fail to concretize constraints", # Failed to concretize the constraints
      "WARNING: Fail to linearize constraints", # Failed to linearize the constraints
      # warning when converting double to int, we silently concretize the expressions
      "WARNING: silently concretizing",
      # We do not handle the symbolic integer paramater
      "WARNING: failing large alloc"     
]

###MAIN###
puts ( "find #{ARGV[1]} -type f | xargs egrep -e 'ERROR | Assertion' | grep -v '#{$UNDERSTOODERRORS.join("' | grep -v '")}")
system( "find #{ARGV[1]} -type f | xargs egrep -e 'ERROR | Assertion | WARNING' | grep -v '#{$UNDERSTOODERRORS.join("' | grep -v '")}'")