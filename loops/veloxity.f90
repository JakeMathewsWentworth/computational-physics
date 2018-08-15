program velocity

	implicit none

	real :: Vfx, Vox, ax, dt, time
	integer :: tmax

	open(unit = 20, file = 'velocity.dat', status = 'Unknown')

	Vox = 0
	ax = 2.15
	dt = 0.5
	time = 0.0
	tmax = 5.0

	do while (time <= tmax)

		Vfx = Vox + (ax * time)
		write(20, *) time, Vfx
		time = time + dt
	end do

	close(unit = 20)
	stop
end program
