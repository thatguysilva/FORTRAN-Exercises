program hipergeometrica_2

!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 27/04/2019
!Modificado em: 25/06/2019

real*8 :: F21, hipergeometrica
real*8 :: f1,f2,f3,fatorial,f
real*8 :: a,b,c,x,n


a = 0.5d0
b = 1d0
c = 1.5d0
x = 0.3d0

!~ !Funções para comparação
!~ f1 = -log(1-x)/x
f2 = atan(x)/x
!~ f3 = 1/((1-x)**a)


F21 = hipergeometrica(a,b,c,-x*x)

write(*,*) "a       =",a
write(*,*) " "
write(*,*) "b       =",b
write(*,*) " "
write(*,*) "c       =",c
write(*,*) " "
write(*,*) "-x**2   = ",-x*x
write(*,*) " "
write(*,*) "F21     =",F21
write(*,*) " "
write(*,*) "f       =",f2


end program

real*8 function hipergeometrica(a,b,c,x)

REAL*8,intent(in) :: a
REAL*8,intent(in) :: b
REAL*8,intent(in) :: c
REAL*8 :: dn
REAL*8, intent(in) :: x
REAL*8 :: s0, s
real*8 :: n
REAL*8 :: f, fatorial
REAL*8 :: r
REAL*8 :: an,bn,cn



s0 = 0
s = 0
n = 0

DO
f = fatorial(n)
an = gamma(a+n)/gamma(a)
bn = gamma(b+n)/gamma(b)
cn = gamma(c+n)/gamma(c)
dn = (an*bn*(x)**(n))/(cn*f)
s = s0 + dn
r = (s-s0)/s0
IF (abs(r) < 1d-12) THEN
EXIT
END IF
s0 = s
n = n + 1

end do

hipergeometrica = s
return



END FUNCTION hipergeometrica


real*8 function fatorial(n)
real*8, intent(in) :: n
integer :: i
real*8 :: f

if (n == 0) then
fatorial = 1
return
end if

f = 1
do i = 1, n
f = f * i
end do

fatorial = f
return

end function fatorial
