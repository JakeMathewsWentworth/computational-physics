! *****************************
! Program: 	mathfunc
! Author: 	Jake Mathews
! Description: 	A program to practice writing complex equations.
! 		Reads in two numbers and produces a result
! *****************************

program mathfunc

! Define user defined variables
Real :: x
Real :: y

! Define program variables
Real :: term1
Real :: term2
Real :: term3
Real :: results

! Populate user defined values
print*, "Enter two real numbers seperated by a comma"
read*, x, y

term1 = sin(x**2 - 3.0*y)**3
term2 = (1 + exp(-(x**2 + y**2) / 4.0)) / (1.0 - exp(-(x**2 + y**2) / 4.0))
term3 = cos(sqrt(abs(x)) + y**4)**2

results = term1 * term2 * term3 + (1.0/7.0)

print*, results

stop
end program
