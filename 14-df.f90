PROGRAM df_indireto

!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 07/06/2019
!Modificado em: 11/06/2019

REAL*8 :: x0, h, yy, r
INTEGER :: i, iteracao
REAL*8, DIMENSION(11) :: x, y
LOGICAL :: fim

x0 = 0
h = 0.1

!condicoes de contorno
y(1)=0
y(11)=100

!variavel de controle
r = 1d-6

DO i=1,11
x(i) = x0 + (i-1)*h
END DO

DO i=2,10
y(i) = (i-1)*100
END DO

interacao=1
DO
fim=.true.
DO i=2,10
yy = 1/(2-10*h**2)*((1-5*h/2)*y(i+1)+(1+5*h/2)*y(i-1)-10*x(i)*h**2)
IF (abs((yy-y(i))/yy) > r) THEN
fim=.false.
END IF
y(i) = yy
END DO
IF (fim .eqv. .true.) THEN
EXIT
END IF
iteracao=iteracao+1
END DO


WRITE(*,*)'N de iteracoes:', iteracao
WRITE(*,*)" "
WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*) "          i		  x			y"
WRITE(*,*)"--------------------------------------------------------------------------------------------"

DO i=1,11
WRITE(*,*) i,x(i),y(i)
END DO

END PROGRAM
