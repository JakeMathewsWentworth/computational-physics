program average
	implicit none

	real :: test1, test2, test3
	real :: myAverage, val

	integer :: none

	print*, 'enter 3 numbers seperated by a comma'
	print*, ''
	read*, test1, test2, test3

	call myAverage(test1, test2, test3, val)

	print*, 'The average of those three umbers is:', val
	stop
end program

subroutine myAverage(x, y, z, val)
	implicit none
	real :: x, y, z
	real :: val

	val = (x + y + z) / 3.0

	return
end subroutine
