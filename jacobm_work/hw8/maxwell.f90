Module constants
      	Implicit None
      	Real(kind=16), Parameter :: pi=acos(-1.0)
      	Real(kind=16), Parameter :: k_B=8.3145
      	Real(kind=16), Parameter :: m_N2=28.
      	Real(kind=16), Parameter :: m_O2=32.
      	Real(kind=16), Parameter :: m_Ar=40.
End Module constants

Program Boltzmann
	Use constants

	Implicit None

	Real(kind=16) :: nint, v_max, beta, sum
	Real(kind=16) :: a, b, y_a, y_b, h_y, h_v, y_k
	Real(kind=16) :: f, fvT, m, Tc, T
	Integer :: ivel, k, n
	Character(len = 256) :: fileName

	Write(*,*) 'Compute fraction of molecules whose speeds v < v_max:'
	Write(*,*) 'Enter temperature in Celsius: '
	Read(*,*) Tc

	Write(*,*) 'Enter upper velocity limit in m/sec: '
	Read(*,*) v_max

	Write(*,*) 'Enter integration step size in m/sec (e.g., 1.): '
	Read(*,*) h_v

	Write(*,*) 'Enter the output file name. Max length of 256 characters: '
	Read(*,*) fileName

	open(unit=100, file=fileName, status='unknown')

      	T = 273.15 + Tc

      	m = (m_N2*0.78 + m_O2*0.21 + m_Ar*0.01) / 1000.0

      	beta = sqrt(2.*k_B*T/m)

      	a = 0.0
      	b = v_max
	y_a = a/beta
	y_b = b/beta
      	h_y = h_v/beta
     	nint = (y_b-y_a)/h_y
 	n = int(nint,4)

  	Do ivel = 0, 5000, 100
        	b = real(ivel,16);
              	y_b = b/beta;
              	!h_y = (y_b-y_a)/real(n,16)
              	!sum = 0.0

		!Do k=1, n-1
		!	y_k= y_a+h_y*real(k,16)
              	!	sum = sum + f(y_k)
		!End Do

        	!fvT = h_y * (f(y_a)+f(y_b)+2.*sum) / 2.
		
		call simpsons(a, y_b, f, fvT)
        	fvT = 4.*pi * fvT / sqrt(pi)**3

        	Write (100, *) b, fvT*100.0
	End do

	Write(*,*) 'Results: '
	Write(*,*) '	Average molecular mass (g/mol):	', m*1000.
	Write(*,*) '	Temperatuer of gas (Celsius):	', Tc
	Write(*,*) '	Velocity (m/sec):		', v_max
	Write (*,20) fvT*100.
	20 Format('	f(v,T) (in %)			', 3x,f6.2)
	Stop
End Program Boltzmann

Function f(y)
      	Implicit None

      	Real(kind=16) :: f, y, y2

      	y2 = y*y
      	f = y2*exp(-y2)
	Return
End Function f

Subroutine simpsons(low, high, func, integral) 
	Implicit none
	
	Real(kind=16), intent(in):: low, high
	Real(kind=16) :: h, n
	Real(kind=16), External :: func
	Real(kind=16), intent(out) :: integral

	n = 6.0
	h = (high - low) / n

	integral = h * (func(low) + 2.0 * func((low + high) / 2.0 - 1.0) + 4.0 * (func((low + high) / 2.0)) + func(high)) / 3.0

	return
End Subroutine






