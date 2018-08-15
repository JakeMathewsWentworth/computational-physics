! ---------------------------------------------------------------------------------
! This simple FORTRAN 90 program will compute numerically the 2nd derivative of
! the function xsin(x) using the symmetric 3-point formula and the
! 5-point formula. The code will produce a plot of the absolute value of
! the difference between the numerical result and the exact result as a
! function of step size "dx". The plot will be a log-log scale.
! ---------------------------------------------------------------------------------

program secondDeriv
	! Defining variables
	implicit none
	real (kind = 16) :: xo, dx, F
	real (kind = 16) :: dF, dFdx,dFdx1

	integer :: i

	! Description of variables
    	! xo is what we will evaluate the function at
    	! dx is our step size
    	! F is our function ---> [sin(x)]
    	! fF is the 2nd known derivative of F ---> [2cost(x)-xsin(x)]
   	! dFdx is the 3-point symmetric approximation
    	! dFdx1 is the 5-point symmetric approximation
   	! i is the loop variable

	! Define the step size
	dx = 1.0

	! Open the data files...
	open(unit = 20, file = 'symmetric3.dat', status = 'unknown')
	open(unit = 30, file = 'symmetric5.dat', status = 'unknown')

	! start do loop to calculate numberically the 2nd derivative
	do i = 1,5
		dx = dx / 10.0
		xo = 26.0
		F = xo * sin(xo)

		! 3-point formula
		dFdx = ((xo + dx) * sin(xo + dx) - 2.0 * xo * sin(xo) + &
			(xo - dx) * sin(xo - dx)) / dx**2

		! 5-point formula
		dFdx1 = (-(xo + 2.0 * dx) * sin(xo + 2.0 * dx) + 16.0* &
			(xo + dx) * sin(xo + dx) - 30.0 * (xo) * sin(xo) + 16.0* &
			(xo - dx) * sin(xo - dx) - (xo - 2.0 * dx) * sin(xo - 2.0 * dx)) / &
			(12.0 * dx * dx)
		
		! 2n known derivative of the function
		dF = 2.0 * cos(xo) - xo * sin(xo)
		
		! write data to the data files
		write(20, *) dx, abs(dFdx - dF)
		write(30, *) dx, abs(dFdx1 - df)
	end do

	close(unit = 20)
	close(unit = 30)
	
	stop
end program
