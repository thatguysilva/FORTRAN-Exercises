PROGRAM euler_modificado
!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 17/05/2019
!Modificado em: 24/06/2019

REAL*8 :: x = 0
REAL*8 :: y = 1
REAL*8 :: xm,ym
REAL*8 :: dy
REAL*8 :: dym
REAL*8 :: intervalo, h, n,r

intervalo = 1
n = 1000
h = intervalo/n

WRITE(*,*)"-------------------------------------------------------------------------------------------------------"
WRITE(*,*) "         x			     y			 exp(-x^2/2)			r"
WRITE(*,*)"-------------------------------------------------------------------------------------------------------"

DO WHILE (x<=1)
dy = -x*y
xm = x + h/2
ym = y + h*dy/2
dym = -xm*ym
y = y + dym*h
r = abs(y-exp((-x**2)/2))
WRITE(*,*) x,y,exp(-x**2/2),r
x = x + h

END DO


END PROGRAM
