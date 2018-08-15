program average
	implicit none

	real :: test1, test2, test3
	real :: myAverage, val

	integer :: none

	print*, 'enter 3 numbers seperated by a comma'
	print*, ''
	read*, test1, test2, test3

	val = myAverage(test1, test2, test3)

	print*, 'The average of those three umbers is:', val
	stop
end program

function myAverage(x, y, z)
	implicit none
	real :: x, y, z
	real :: myAverage

	myAverage = (x + y + z) / 3.0

	return
end function
