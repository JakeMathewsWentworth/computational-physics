!-------------------------------------------------------
! Program: 	harmonic
! Author: 	Jake Mathews
! Date: 	July 26th, 2018
! Description: 	Simulates a spring mass system to 
!		demonstrate the motion of a Simple
!		Harmonic Oscillator (HS0). The program
!		simulates an exact solution and a 
!		coupled Ordinary Differential Equation
!		(ODE).
!------------------------------------------------------
program harmonic
	implicit none

	! Delcarations
	double precision :: func		! general function
	double precision :: x, x1, xInitial		! x-position (m)
	double precision :: v, v1, velocityInitial	! x-velocity (m/s)
	double precision :: mass 		! block mass (kg)
	double precision :: k 			! spring constant (N/m)
	double precision :: omega		! calculated constant
	double precision :: t, t1		! time (s)
	double precision :: newTime		! time redeclaration (s)
	double precision :: deltaTime 		! time step (s)
	double precision :: maxTime		! max simulation time (s)
	double precision :: kineticEnergy	! kinetic energy (j)
	double precision :: potentialEnergy	! potential energy (j)
	double precision :: totalEnergy		! total energy
	integer :: i				! do loop iterator

	! Open data files
	open(unit = 10, file = 'exact.dat', status = 'unknown')
	open(unit = 20, file = 'approximation.dat', status = 'unknown')
	open(unit = 30, file = 'energy.dat', status = 'unknown')

	! Set inital conditions
	xInitial = 1.0d0
	velocityInitial = 0.0d0
	x = xInitial 		! x-position (m)
	v = velocityInitial	! x-velocity (m/s)
	mass = 1.0d0 		! block mass (kg)
	k = 1.2d0 		! spring constant (N/m)
	omega = sqrt(k / mass)	! calculated constant
	t = 0.0d0		! time (s)
	deltaTime = 0.001d0 	! time step (s)
	maxTime	= 20.0d0	! max simulation time (s)

	! Calculate exact solution
	do i = 1, int(maxTime / deltaTime)
		! Calculate exact solution
		newTime = i * deltaTime
		x1 = func(newTime, xInitial, velocityInitial, omega)

		! Write exact solution to file
		write(10, *) newTime, x1
	end do

	! Calculate aproximate solution
	do while (t <= maxTime)
		! Calculate aproximate solution
		t1 = t + deltaTime
		x1 = x + v * deltaTime
		v1 = v + ((-k * x) / mass) * deltaTime

		! Calculate energy
		kineticEnergy = 0.5d0 * mass * v**2
		potentialEnergy = 0.5d0 * k * x**2
		totalEnergy = potentialEnergy + kineticEnergy

		! Write approximated solution and energy to files
		write(20, *) t, x
		write(30, *) t, totalEnergy

		! Increment for iteration
		t = t1
		x = x1
		v = v1
	end do

	! Close data files
	close(unit = 10)
	close(unit = 20)
	close(unit = 30)

	stop
end program harmonic

! This function is the exact solution
function func(t, xInitial, velocityInitial, omega)
	implicit none

	double precision :: func
	double precision :: xInitial
	double precision :: t
	double precision :: velocityInitial
	double precision :: omega

	func = (xInitial * cos(omega * t)) + ((velocityInitial / omega) * sin(omega * t))

	return
end function func
