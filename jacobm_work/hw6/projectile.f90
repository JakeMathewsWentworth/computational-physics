!--------------------------------------------------------
! Author: 	Jake Mathews
! Date: 	June 28th, 2018
! Program: 	projectile
! Description: 	plots the x and y position of a projectile 
!		to a file
!--------------------------------------------------------


program projectile
	implicit none

	real :: maxHeight, maxHeightTime ! projectile statistics
	logical :: writeToFile = .true.
	character(len = 10) :: fileName
	real :: theta, velocity 

	print*, 'Enter the inital lauch angle (degrees), and the initial launch velocity (meters/second) seperated by a comma:'
	read*, theta, velocity
	print*, ''

	if ((theta == 15) .and. (velocity == 50)) then
		fileName = 'traj15.dat'
	else if ((theta == 30) .and. (velocity == 50)) then
		fileName = 'traj30.dat'
	else if ((theta == 45) .and. (velocity == 25)) then
		fileName = 'traj45.dat'
	else if ((theta == 60) .and. (velocity == 25)) then
		fileName = 'traj60.dat'
	else
		writeToFile = .false.
		print*, 'Given the angle and velocity, results will not be written to files.'
		print*, '	Valid angles/velocties are 15deg & 30deg @ 50m/s as well as 45deg & 60deg @ 25m/s'
		print*, '	Example input would be: 15.0, 30.0'
		print*, ''
	endif
	
	call plotTrajectory(theta, velocity, fileName, writeToFile, maxHeight, maxHeightTime)
        call printStats(theta, velocity, maxHeight, maxHeightTime)
	
	stop
end program

subroutine printStats(theta, velocity, maxHeight, maxHeightTime)
	implicit none

	real , intent(in) :: theta, velocity, maxHeight, maxHeightTime

	print*, 'Trajectory Statstics'
	print 100, 'Angle (degrees): ', theta
        100 format (7x, a, 3(f6.2))
	print 150, 'Initial Velocity (meters/second): ', velocity
        150 format (7x, a, 3(f6.2))
	print 200, 'Max Height (meters): ', maxHeight
        200 format (7x, a, 3(f6.2))
        print 250, 'Time to Max Height (seconds): ', maxHeightTime
        250 format (7x, a, 3(f4.2))
	print *, ''
end subroutine

subroutine plotTrajectory(initialTheta, initialVelocity, fileName, writeToFile, maxHeight, maxHeightTime)
	implicit none

	real, parameter :: pi = acos(-1.0) ! pi - Mathematical constant
	real, parameter :: g = 9.81 ! m/s^2 - Gravitational acceleration on earth
	real, parameter :: dt = 0.01 ! loop parameters

	real, intent(in) :: initialTheta, initialVelocity ! input - angle (degrees) and intial velocity (m/s)
	character(len = *), intent(in) :: fileName ! input - output file name
	logical, intent(in) :: writeToFile ! input- logical condition on whether to write the results to a file
	
	real, intent(out) :: maxHeight, maxHeightTime ! output - projectile statistics

	real :: x, y, t, theta ! equation variables
	
	! Intialize equation variables
	x = 0.0
	y = 0.0
	t = 0.0
	theta = (pi / 180.0) * initialTheta
	maxHeight = 0.0
	maxHeightTime = 0.0

	! Open the file by name provided
	if (writeToFile) then
		print*, 'Writing to results to file: ', fileName
		print*, ''
		open(unit = 20, file = fileName, status = 'Unknown')
	endif

	do while (y >= 0)
		x = initialVelocity * cos(theta) * t
		y = (initialVelocity * sin(theta) * t) - (0.5 * g * t**2)

		if (y > maxHeight) then
			maxHeight = y
			maxHeightTime = t
		endif
		
		if (writeToFile) then
			write(20, *) x, y
		endif

		t = t + dt 
	end do

	if (writeToFile) then
		close(unit = 20)
	endif

end subroutine
	
