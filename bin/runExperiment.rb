#!/usr/bin/ruby

children_pipes = [] # children pipes
CPUS = 18
for i in 1..CPUS do
  children_pipes << IO.popen( "cd Test#{i}; gsl-test.rb -vd ." )  
end
children_pipes.each { |p| Process.waitpid( p.pid ) }