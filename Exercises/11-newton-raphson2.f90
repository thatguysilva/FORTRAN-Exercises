program newton_raphson2
!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 09/05/2019
!Modificado em: 15/05/2019

REAL*8, DIMENSION(3,1) :: X0
REAL*8, DIMENSION(3,1) :: X
REAL*8, DIMENSION(3,3) :: J, Ji
REAL*8, DIMENSION(3,1) :: f, M
REAL*8 :: detJ


!valores iniciais:

X0(1,1) = 10
X0(2,1) = 5
X0(3,1) = 7


WRITE(*,*)'Valor inicial de x1 =',X0(1,1)
WRITE(*,*)'Valor inicial de x2 =',X0(2,1)
WRITE(*,*)'Valor inicial de x3 =',X0(3,1)
WRITE(*,*)" "
WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*) "         x1			     x2			      x3"
WRITE(*,*)"--------------------------------------------------------------------------------------------"



DO n = 1, 10

!Determinante da matriz Jacobiana:
detJ = 2*X0(1,1)*X0(2,1)+4*X0(1,1)*X0(3,1)**2+4*X0(2,1)*X0(3,1)**2+2*X0(3,1)**2

!Matriz transposta da matriz dos cofatores da Jacobiana:
J(1,1) = (2*X0(1,1)*X0(3,1)**2+X0(1,1)*X0(2,1))/(X0(1,1)-X0(2,1))
J(1,2) = (-2*(X0(2,1)*X0(3,1)+X0(3,1)))/(X0(1,1)-X0(2,1))
J(1,3) = -2*X0(1,1)*X0(2,1)**2+2*X0(1,1)*X0(3,1)**2/(X0(1,1)-X0(2,1))
J(2,1) = -(2*X0(2,1)*X0(3,1)**2+X0(1,1)*X0(2,1))/(X0(1,1)-X0(2,1))
J(2,2) = (2*(2*X0(1,1)*X0(3,1)+X0(3,1)))/(X0(1,1)-X0(2,1))
J(2,3) = (2*(X0(2,1)*X0(1,1)**2-X0(2,1)*X0(3,1)**2))/(X0(1,1)-X0(2,1))
J(3,1) = X0(3,1)
J(3,2) = 2
J(3,3) = -2*(X0(1,1)*X0(3,1)+X0(2,1)*X0(3,1))

!Inversa da matriz Jacobiana:
Ji = J/detJ

!Equações escritas como funções:
f(1,1) = X0(1,1)**2 + X0(2,1)**2 + X0(3,1)**2 - 9
f(2,1) = X0(1,1)*X0(2,1)*X0(3,1) - 1
f(3,1) = X0(1,1) + X0(2,1) - X(3,1)**2



call multmatriz(Ji,f,M)

X = X0 - M

X0 = X

write(*,*) X

END DO

write(*,*) "=================================================================================="
write(*,*) " "
write(*,*) "x1 =",X0(1,1)
write(*,*) "x2 =",X0(2,1)
write(*,*) "x3 =",X0(3,1)






END PROGRAM

subroutine multmatriz(A,B,C)

INTEGER :: i,k
INTEGER :: j = 1
REAL*8,DIMENSION(3,3),INTENT(IN) :: A 
REAL*8,DIMENSION(3,1),INTENT(IN) :: B
REAL*8, DIMENSION(3,1), INTENT(OUT) :: C

C = 0

DO i = 1,3

		DO k =1,3
			C(i,j) = C(i,j) + A(i,k)*B(k,j)
		END DO
	END DO





end subroutine multmatriz


