program temp

	Real :: temp_f
	Real :: temp_c
	Real :: temp_k

	print*, "Enter the temperature in Fahrenheit"
	read*, temp_f

	temp_c = (temp_f - 32.0) * (5.0 / 9.0)
	temp_k = temp_c + 273.15

	print*, "The temperature in Fahrenheit is", temp_f
	print*, "The temperature in Celcius is", temp_c
	print*, "The temperature in Kelvin is", temp_k

Stop
end program
