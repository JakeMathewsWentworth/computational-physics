program sinvderiv
	implicit none

	real :: xo, dx, F, dF, dFdx1, dFdx2
	integer :: i

	! xo is what we will evalutate the function at
	! dx is our stepsize
	! F is our function: F(x)=sin(x)
	! df is the 1st known derivative: F'(x)=cos(x)
	! dFdx us the numerical approximation

	dx = 1.0
	xo = 26.0
	! 1st known derivitive of our function
        dF = cos(xo)
	do i = 1, 5
		dx = dx/10.0
		F = sin(xo)

		! 1st Derivative approximation
		dFdx1 = (sin(xo + dx) - sin(xo))/dx
		dFdx2 = (sin(xo + dx) - sin(xo - dx)) / (2*dx)
		print*, 'Iteration', i, 'Method 1,2 Error: ', abs(dFdx1 - dF), ',',  abs(dFdx2 - dF)
	end do
stop
end program
