DOUBLE PRECISION function average (x, y)
DOUBLE PRECISION x, y, zero, two, av1, av2, av3, av4 
logical samesign
parameter (zero = 0.0e+00, two = 2.0e+00)
av1(x,y) = (x + y) / two
av2(x,y) = (x / two) + (y / two)
av3(x,y) = x + ((y - x) / two)
av4(x,y) = y + ((x - y) / two)

if (x .ge. zero) then
if (y .ge. zero) then
samesign = .true.
else
samesign = .false.
endif
else
if (y .ge. zero) then
samesign = .false.
else
samesign = .true.
endif
endif

if (samesign) then
if (y .ge. x) then
average = av3(x,y)
else
average = av4(x,y)
endif
else
average = av1(x,y)
endif

return
end