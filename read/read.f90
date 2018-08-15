program read_statement

	Implicit None

	Real :: x
	Integer :: y

	print*, "Enter a value for x"
	read*, x

	print*, "Enter a value for y"
	read*, y

	print*, "The value you entered for 'x' is", x
	print*, "The value you entered for 'y' is", y

Stop
end program
