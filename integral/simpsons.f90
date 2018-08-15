program simpsonexact
	implicit none
	
	double precision :: a, b, h, f, x, integral, n

	print*, 'Enter the limits of integration seperated by a comman'
	read*, a, b

	n = 6.0
	h = (b - a) / n

	integral = h * (f(a) + 4.0 * f((a + b) / 2.0) + f(b))

	print*, 'The integral from ', a, ' to ', b, ' = ', integral

	stop
end program

function f(x)
	implicit none
	double precision :: f, x
	f = (x * x) + 1.0
	return
end function
