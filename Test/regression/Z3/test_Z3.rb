#!/usr/bin/ruby

Dir.glob("*\.smt") do |f|
    result = `z3 -m #{f}`
    puts result
end