PROGRAM df_direto

!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 15/06/2019
!Modificado em: 20/06/2019

REAL*8, DIMENSION(11) :: a,b,c,y,x
REAL*8, DIMENSION(11) :: beta, rho
REAL*8 :: h = 0.1d0
REAL*8 :: d1, d2, d3
INTEGER :: j

!Definindo os valores nas diagonais:
d1 = 5/(2*h) + 1/(h**2)
d2 = -2/(h**2) + 10
d3 = 1/(h**2) - 5/(2*h)
DO j = 1,11
a(j) = d1
b(j) = d2
c(j) = d3
x(j) = (j-1)*h
END DO

!Resolvendo o sistema linear tridiagonal:
beta(1) = b(1)
rho(1) = x(1)

DO j = 2,11
beta(j) = b(j) - a(j)*c(j-1)/beta(j-1)
rho(j) = x(j) - a(j)*rho(j-1)/beta(j-1)
END DO

y(1) = 0
y(11) = 100


DO j = 1,10
y(11-j) = (rho(11-j)-c(11-j)*y(11-j+1))/beta(11-j)
END DO
y(1) = 0
y(11) = 100
WRITE(*,*) "h =",h
WRITE(*,*) " "
WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*) " 	  x			     y"
WRITE(*,*)"--------------------------------------------------------------------------------------------"

DO j = 1,11

WRITE(*,*) x(j),y(j)
END DO

WRITE(*,*)"--------------------------------------------------------------------------------------------"

END PROGRAM
