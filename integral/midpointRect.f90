program midpointrect
	implicit none
	
	real :: a, b, h, f, x, integral, dx
	integer :: i, n

	print*, 'Enter the limits of integration seperated by a comman'
	read*, a, b

	print*, 'Enter the number of rectangles'
	read*, n

	! Width of each rectangle
	h = (b - a) / n

	dx = 0.5
	! Intialize
	integral = 0.0
	do i = 0, n - 1
		x = (dx + i) * h
		integral = integral + f(x)
		write(*, *) x, f(x)
	end do

	integral = h * integral

	print*, 'width n = ', n, ' rectangles, the estimate'
	print*, 'of the integral from ', a, ' to ', b, ' = ', integral

	stop
end program

real function f(x)
	implicit none
	real :: x
	f = (x**2) + 1
	return
end function
