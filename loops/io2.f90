program io2

	implicit none
	
	real :: x, y, dx
	integer :: i
	dx = 0.5

	open(unit = 10, file = 'func.dat', status = 'Unknown')

	do i = -10, 10
		x = dx * i
		y = x**2
		print*, y
		write(10, *) x, y
	end do

	close(10)
	stop

end program
