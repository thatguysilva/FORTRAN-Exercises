PROGRAM newton_raphson

!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 14/04/2019
!Modificado em: 25/04/2019

IMPLICIT NONE

REAL*8 :: x0,x, f, df, r

!Valor inicial de x
x0 = 30

WRITE(*,*)'Valor inicial de x =',x0
WRITE(*,*)" "
WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*) "         x0			     x			r"
WRITE(*,*)"--------------------------------------------------------------------------------------------"


   DO WHILE(abs(r) > 1d-12)

     f= x0**4 - 26*(x0**3) + 131*(x0**2) - 226*x0 + 120
     df = 4*(x0**3) - 3.0*26*(x0**2) + 2.0*131*x0 - 226
     x = x0-f/df
     r = (x-x0)/x0
     x0 = x
     write (*,*) x0,x,abs(r)

  END DO
WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*)" "
  WRITE(*,*) 'Raiz = ',x



END PROGRAM
