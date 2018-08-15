program orbits
	implicit none
	
	real :: x, y
	real :: vx, vy
	real :: t, tmax, dt
	real :: xn, yn
	real :: vxn, vyn
	real :: gravity, mSun
	real :: energy, vtot, rvec

	integer :: flag

	x = 1.00		! x-position
	y = 0.0d0		! y-position
	vx = 0.0d0		! x-velocity
	vy = 6.29d0		! y-velocity
	t = 0.0d0		! intial time
	! dt = 1.0d0 / 365.25d0	! time-step (user overriden)

	gravity = 39.47d0	! gravitational constant (normalized)
	mSun = 1.0d0		! mass of the Sun (normalized)

	! open files for data to be written
	open(unit = 10, file = 'xy-pos.dat', status = 'unknown')

	! user input for max orbital period
	print*, 'Enter max orbital period in years (i.e. 1.0 is 1 year)'
	read*, tmax

	print*, 'Enter time step'
	read *, dt

	! loop to integrate over max orbital period
	do while (t <= tmax)
		! update x and y coordinates via Euler
		xn = x + (vx * dt)
		yn = y + (vy * dt)

		! update x-velocity
		vxn = vx - gravity * mSun * x * dt / (x**2 + y**2)**1.5d0

		! update y-velocity
		vyn = vy - gravity * mSun * y * dt / (x**2 + y**2)**1.5d0

		! re-declare (update position and velocity)
		x = xn
		y = yn
		vx = vxn
		vy = vyn

		! increment time
		t = t + dt

		! calculate r-vector
		rvec = (x**2)
		
		! write to file
		write(10, *) x, y, t
		write(*, *) x, y, t
	end do

	close(unit = 10)

	stop
end program
