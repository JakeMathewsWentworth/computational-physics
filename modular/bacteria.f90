program bacteria
	implicit none

	real :: initialPopulation, newPopulation, time

	print*, 'Enter the initial population and simulation time in hours'
	print*, ''
	read*, initialPopulation, time

	call simulateColony(initialPopulation, time, newPopulation)

	print*, 'Inital Population', initialPopulation
	print*, 'Time Elapsed (Hours)', time
	print*, 'Predicted Population', newPopulation

	stop
end program

subroutine simulateColony(initialPopulation, time, newPopulation)
	implicit none

	real, intent(in) :: initialPopulation, time 
	real, intent(out) :: newPopulation
        real, parameter :: alpha = 1.386
	real, parameter :: dt = 0.5
	integer :: i
	
!	do i=0, int(time/dt)
!		newPopulation = initialPopulation * exp(alpha * (i * dt))
!	end do

	newPopulation = initialPopulation * exp(alpha * time)

	return
end subroutine
