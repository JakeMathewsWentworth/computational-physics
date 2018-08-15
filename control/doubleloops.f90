program doubleloops

	Implicit None

	Real :: none
	Integer :: i, j

	do i = 1, 10
		do j = 1, 10
			print*, i * j
		end do
	end do

stop
end program
