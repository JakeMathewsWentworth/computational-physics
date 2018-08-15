program euler1
	implicit none

	double precision :: dxdt	! for function
	double precision :: dt		! time-step
	double precision :: xi		! intial conditions
	double precision :: ti		! intial conditions
	double precision :: xf 		! for re-declaration
	double precision :: tf 		! for re-declaration
	double precision :: tmax	! max time
	double precision :: x, t	! for exact solution

	integer :: i, n

	! using function dx
	external dxdt

	open(unit = 20, file = 'results-exact.dat', status = 'unknown')
	open(unit = 30, file = 'results-euler1.dat', status = 'unknown')

	! intial conditions
	xi = 1.00
	ti = 0.0

	! step-size and max time:
	print*, 'Enter a time-step:'
	read*, dt
	tmax = 2.0

	! exact solution:
	do i = 1, 20
		t = i * dt
		x = exp(-t)
		write(20, 100) t, x
	end do

	! loop to solve ode
	do while(ti .le. tmax)
		! increment step-size
		tf = ti + dt
		xf = xi + dxdt(ti, xi) * dt
		print*, xf
		write(30, 100) tf, xf
		
		! re-delcare
		ti = tf
		xi = xf
	end do

	! format output
	100 format (2f10.5)

	stop
end program

function dxdt(t, x)
	implicit none
	
	double precision :: dxdt
	double precision :: x
	double precision :: t

	dxdt = -(1.0) * x
end function









