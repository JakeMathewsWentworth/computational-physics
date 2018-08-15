program excersize1
	
	implicit none
	
	real :: x, y, z, results
	real, parameter :: pi = 4 * atan (1.0)

	print*, 'Enter in 3 real numbers'
	read*, x, y, z

	if (x < 0) then
		results = sqrt(x**3 + y**3 + z **3)
	elseif (x == 0) then
		results = sqrt(pi / 4)
	else
		results = sin(x * y) + cos(x * z)
	end if

	print*, 'Results', results 

stop
end program
