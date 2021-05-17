PROGRAM secante

!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 14/04/2019
!Modificado em: 25/04/2019

IMPLICIT NONE

REAL*8 :: x1, x2,x3,fx1,fx2


x1 = 18
x2 = 23

WRITE(*,*)'Valor inicial de x1 =',x1
WRITE(*,*)'Valor inicial de x2 =',x2
WRITE(*,*)" "
WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*) "         x1			     x2			r"
WRITE(*,*)"--------------------------------------------------------------------------------------------"
  
  DO WHILE(abs((x2-x1)/x1) > 1d-12)

     fx1 = x1**4 - 26*(x1**3) + 131*(x1**2) - 226*x1 + 120
     fx2 = x2**4 - 26*(x2**3) + 131*(x2**2) - 226*x2 + 120
     x3 = x2 - fx2*(x2 - x1)/(fx2 - fx1) 
     x1 = x2
     x2 = x3
     WRITE(*,*) x1,x2,abs((x2-x1)/x1)
     
    
  END DO

WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*)" "
WRITE(*,*) 'Raiz = ', x3






END PROGRAM
