program Deriv1
	implicit none

	! Define variables
	real, allocatable :: y(:), yprime(:)
	real :: x, dx
	integer :: i, grid_pts

	! Request the number of grid points from user
	print *, 'Enter the number of grid points'
	read *, grid_pts

	! Allocate enough memory in to 2 arrays
	allocate(y(grid_pts), yprime(grid_pts))

	! Define the spacing given the amount of grid points (step-size)
	dx = 10.0 / (grid_pts - 1)

	! Fill the y array with the result of the function at each point
	do i = 1, grid_pts
		x = (i - 1) * dx
		y(i) = cos(x)
	end do

	! Get the approximate derivative of the equation y
	call deriv(y, grid_pts, dx, yprime)

	! Compare the results of approximation with the exact derivation and show the error
	do i = 1, grid_pts
		x = (i - 1) * dx
		print *, yprime(i), -sin(x), -sin(x) - yprime(i)
	end do

	deallocate (y, yprime)
end program

! A subroutine to give an aproximate derivation
subroutine deriv (a, np, h, aprime)
	implicit none

	! Define variables
	integer, intent(in) :: np ! grid_pts

	real, intent(in) :: a(np), h
	real, intent(out) :: aprime(np)
	integer :: i

	! Fill the output array with the approximate 
	! derivation of the same point in the y array
	do i = 1, np - 1
		aprime(i) = (a(i + 1) - a(i)) / h
	end do

	aprime(np) = 0.0
end subroutine deriv
