! ************************************************
! Program: 	Swapped
! Author: 	Jake Mathews
! Description: 	Swappes two real numbers provided 
! 	       	by the user seperated by a comma
! ************************************************

program swap

! Define variables
Real :: x
Real :: y
Real :: temp ! Temp variable needed to perform swap

! Get numbers to be swapped from the user
print*, "Enter two numbers x & y separated by a comma:"
read*, x, y
print*, "" ! Blank line

! Print the intial parameters
print*, "=============================="
print*, "The numbers you entered are:"
print*, "x=:", x
print*, "y=:", y
print*, "=============================="
print*, "" ! Blank line

! Perform swap
temp = x
x = y
y = temp

! Print results
print*, "*********************************"
print*, "The numbers are now swapped:"
print*, "x=", x
print*, "y=", y
print*, "*********************************"

stop
end program 
