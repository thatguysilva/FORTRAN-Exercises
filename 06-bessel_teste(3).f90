program bessel_teste

real*8 :: J,jbessel
real*8 :: x,v
real*8 :: Y,ybessel

v = 1
x = 5

J = jbessel(v,x)
Y = ybessel(v,x)


write(*,*) "v = ",v
write(*,*) " "
write(*,*) "x = ",x
write(*,*) " "
write(*,*) "J = ",J
write(*,*) " "
write(*,*) "Y = ",Y





end program

real*8 function jbessel(v,x)

REAL*8 :: v
integer :: k                         !Variáveis dos loops
REAL*8 :: x
REAL*8 :: s01,s1,cn1   !Variáveis da função de Bessel de primeira espécie
REAL*8 :: r1                          !Variáveis para controle dos loops                             !Função de Bessel de segunda espécie
REAL*8 :: f1                          !Valores retornados pela subrotina fatorial
real*8 :: fatorial

r1 = 0
s01 = 0
s1 = 0
k = 0
DO
f1 = fatorial(k)
cn1 = (-x**(2)/4)**k/(f1*gamma(k+v+1))
s1 = s01 + cn1
r1 = (s1-s01)/s01
IF (abs(r1) < 1d-12) THEN
EXIT
END IF
s01 = s1
k = k + 1
END DO
jbessel=s1*(x/2)**v
return
end function jbessel

real*8 function ybessel(v,x)

real*8, intent(in) :: v,x
real*8 :: h
real*8 :: v1,v2,Y0,Y
real*8 :: jbessel,j1,j2,j3,j4
REAL*8 :: pi = 4.d0*datan(1.d0)
REAL*8 :: r


h=0.1d0

Y0 = 1000000




h=h*0.5d0
DO

 v1=v+h
   v2=v-h

j1 = jbessel(v1,x)
j2 = jbessel(-v1,x)
j3 = jbessel(v2,x)
j4 = jbessel(-v2,x)



   Y=((j1*cos(v1*pi)-j2)/sin(v1*pi)+(j3*cos(v2*pi)-j4)/sin(v2*pi))*0.5d0
   r = (Y-Y0)/Y0
   IF(abs(r) < 1d-8) THEN
     ybessel=Y
     return
   END IF
   Y0=Y
   h=h*0.5d0
END DO
end function ybessel


!Função para cálculo do fatorial:
real*8 function fatorial(n)
integer, intent(in) :: n
integer :: i
real*8 :: f
f = 1
do i = 1, n
f = f * i

end do
fatorial = f
return
end function fatorial
