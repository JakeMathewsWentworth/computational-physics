program loops

	implicit none

	integer :: n
	real :: dn = 0.5
	real :: f	

	do n=1, 20
		f = dn * n
		print*, f**2
	end do

	stop

end program
