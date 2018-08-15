program midpointrect
	implicit none
	
	real :: a, b, h, f, x, integral, dx, sigma
	integer :: i, n

	real :: x_k, weight, sign, sum, simp ! New stuff
	real :: I_up, I_low, Iexact
	integer :: k ! New stuff

	print*, 'Enter the limits of integration seperated by a comman'
	read*, a, b

	print*, 'Enter the number of rectangles'
	read*, n

	! Width of each rectangle
	h = (b - a) / n

	dx = 0.5
	! Intialize
	integral = 0.0
	do i = 1, n - 1
		x = a + (h * i)
		sigma = sigma + f(x)
	end do

	integral = h * (((f(a) + f(b)) / 2.0) + sigma)

	! ##### Begin new stuff #####

	do k = 1, n - 1
		x_k = a + h * float(k)
		weight = 4.0
		sign = (-1)**k
		if (sign > 0) weight = 2.0
		sum = sum + weight * f(x_k)
	end do
	simp = h * (f(a) + f(b) + sum) / 3.0
	print*, 'simp = ', simp

	I_up  = (1 / 10.0) * exp(-3.0) * (10 * exp(3.0) * 3 - 6 * sin(2.0 * 3.0) + 3 * cos(2.0 * 3.0) - 15)
	I_low = (1 / 10.0) * exp(-0.0) * (10 * exp(0.0) * 0 - 6 * sin(2.0 * 0.0) + 3 * cos(2.0 * 0.0) - 15)

	Iexact = I_up - I_low
	print*, '********************************'
	print*, ''''
	write(*, 22) Iexact
22	format('Iexact = ', F7.4)

	! ##### End new stuff #####
	

	print*, 'width n = ', n, ' rectangles, the estimate'
	print*, 'of the integral from ', a, ' to ', b, ' = ', integral

	stop
end program

real function f(x)
	implicit none
	real :: x
	! f = (x**2) + 1 ! - Orginal
	f = 3.0 * exp(-x) * sin(x)**2 + 1.0
	return
end function
