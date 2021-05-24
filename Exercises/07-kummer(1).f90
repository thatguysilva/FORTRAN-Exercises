PROGRAM kummer
!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 19/04/2019
!Modificado em: 04/05/2019

REAL*8 :: a, an
REAL*8 :: b, bn
REAL*8 :: cn
REAL*8 :: x
REAL*8 :: s0, s
REAL*8 :: f,fatorial
integer :: n
REAL*8 :: r

!Parâmetros da função de Kummer:
a = 1
b = 1
x = 7

s0 = 0
s = 0
n = 0

DO
	f = fatorial(n)
	an = gamma(a+n)/gamma(a)
	bn = gamma(b+n)/gamma(b)
	cn = (an*x**(n))/(bn*f)
	s = s0 + cn
	r = (s-s0)/s0
		IF (abs(r) < 1d-10) THEN
		EXIT
		END IF
	s0 = s
	n = n + 1
	

END DO

write(*,*) "a =",a
write(*,*) " "
write(*,*) "b =",b
write(*,*) " "
write(*,*) "x =",x
write(*,*) " "
write(*,*) "M =",s





END PROGRAM

	real*8 function fatorial(n)
		   integer, intent(in) :: n
		   integer :: i
		   real*8 :: f

		   f = 1
		   do i = 1, n
			  f = f * i
			  
		   end do
		
		fatorial = f
		return
		
		end function fatorial


