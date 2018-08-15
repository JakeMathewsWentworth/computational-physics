! ***********************************
! Program: 	Sum
! Author: 	Jake Mathews
! Description: 	Sums up all integers
!		between 1 an 100 and
!		then displays result
! ***********************************

program sum

	integer :: sums = 0

	do n=1,100
		sums = sums + n
	end do

	print*, sums

	stop

end program
