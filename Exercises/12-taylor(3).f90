program taylor
!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 17/05/2019
!Modificado em: 24/06/2019

REAL*8 :: x = 0
REAL*8 :: y = 1
REAL*8 :: dy
REAL*8 :: d_xy
REAL*8 :: d_yy
REAL*8 :: staylor
REAL*8 :: intervalo, h, n,r

intervalo = 1
n = 1000
h = intervalo/n


WRITE(*,*)"-------------------------------------------------------------------------------------------------------"
WRITE(*,*) "         x			     y			 exp(-x^2/2)			r"
WRITE(*,*)"-------------------------------------------------------------------------------------------------------"

DO WHILE (x<=1)
dy = -x*y
d_xy = -y
d_yy = -x
staylor = y + dy*h + (d_xy + dy*d_yy)*(h**2)/2
r = abs(y-exp((-x**2)/2))

WRITE(*,*)x,y,exp(-x**2/2),r
y = staylor

x = x + h
END DO






end program
