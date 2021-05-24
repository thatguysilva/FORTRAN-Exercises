PROGRAM euler
!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 17/05/2019
!Modificado em: 03/06/2019

REAL*8 :: x = 0
REAL*8 :: y = 1
REAL*8 :: dy
REAL*8 :: intervalo, h, n, r

intervalo = 1
n = 1000
h = intervalo/n

WRITE(*,*)"-------------------------------------------------------------------------------------------------------"
WRITE(*,*) "         x			     y			 exp(-x^2/2)			r"
WRITE(*,*)"-------------------------------------------------------------------------------------------------------"

DO WHILE (x<=1)
dy = -x*y
y = y + dy*h
r = abs(y-exp((-x**2)/2))
WRITE(*,*) x,y,exp(-x**2/2),r
x = x + h
END DO


END PROGRAM
