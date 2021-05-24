PROGRAM aproximacoes_sucessivas
!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 11/05/2019
!Modificado em: 15/05/2019

REAL*8 :: x1, x10, x2, x20
REAL*8 :: r1,r2

!Valores iniciais:

x10 = -0.5

x20 = 1

WRITE(*,*)'Valor inicial de x1 =',x10
WRITE(*,*)'Valor inicial de x2 =',x20
WRITE(*,*)" "
WRITE(*,*) "-----------------------------------------------------------------------"
WRITE(*,*) "        r1                       r2                        "
WRITE(*,*) "-----------------------------------------------------------------------"


DO WHILE (abs(r1) >  1d-5 .and. abs(r2) > 1d-5)

write(*,*) r1,r2

x1 = sqrt((3.5d0 - x20)/3)
r1 = (x1-x10)/x10
x10 = x1


x2 = (1.625d0 - x10)**(1.0d0/3.0d0)
r2 = (x2-x20)/x20
x20 = x2


END DO





WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*)" "
WRITE(*,*) 'x1 = ',x1
WRITE(*,*) 'x2 = ',x2
  



END PROGRAM
