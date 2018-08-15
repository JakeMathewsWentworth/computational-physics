program math
	implicit None
	! Variable declarations:
	Double Precision :: x, y, a
	Double Precision :: f1, f2, f3

	Integer :: b, c

	! Assign numerical values
	x = 1.1
	y = 2.5
	a = -5.5
	b = 10
	c = 3

	! Add, subtract, multiply, and divide some numbers
	f1 = (x + y) / y
	f2 = (a * b) + (c - a)
	f3 = (x - a) / (-a * y)

	! Print results to terminal
	print*, "**************************************"
	print*, "This simple FORTRAN 90 does some math"
	print*, "**************************************"
	print*, ""
	print*, "x=", x
	print*, "y=", y
	print*, "a=", a
	print*, "b=", b
	print*, "c=", c
	print*, "**************************************"
	print*, ""
	print*, "(x + y) / y = ", f1
	print*, "(a * b) + (c - a) = ", f2 
	print*, "(x - a) / (-a * y) = ", f3
	
	stop
end program math

