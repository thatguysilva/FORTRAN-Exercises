program gama_func_final_2

real*8 :: x,v
real*8 :: gama 


v=1
x = 5.4d0

write(*,*)"x           =",x
write(*,*) " "
write(*,*)"valor exato =",gamma(x)
write(*,*) " "
write(*,*)"gama        =",gama(x)








end program




real*8 function gama(xx)

REAL*16,DIMENSION(0:19) :: w
REAL*16,DIMENSION(0:19) :: B, Tn
integer :: n
integer :: i
real*8 :: s, st,sb
real*8 :: x,v,g,xx

x = xx


w(0) = 0.94178559779549466571d0
w(1) = 0.00441538132484100676d0
w(2) = 0.05685043681599363379d0
w(3) = -0.00421983539641856050d0
w(4) = 0.00132680818121246022d0
w(5) = -0.00018930245297988804d0
w(6) = 0.00003606925327441245d0
w(7) = -0.00000605676190446086d0
w(8) = 0.00000105582954630228d0
w(9) = -0.00000018119673655424d0
w(10)= 0.00000003117724964715d0
w(11)= -0.00000000535421963902d0
w(12)= 0.00000000091932755199d0
w(13)= -0.00000000015779412803d0
w(14)= 0.00000000002707980623d0
w(15)= -0.00000000000464681865d0
w(16)= 0.00000000000079733502d0
w(17)= -0.00000000000013680782d0
w(18)= 0.00000000000002347319d0
w(19)= -0.00000000000000402743d0



st = 0

v = 1



B(0) = 1

Tn(0) = 1



if (x > 2.d0) then

do
x = x-1.d0
v = v*x
if (x < 2.d0) then
	exit
end if

end do


end if

if (x < 1.d0) then
do
v = v*x
x = x+1.d0
if (x > 1) then
v = 1/v
	exit
end if


end do


end if

DO n = 1,19

sb = 0

DO i = 1,n

B(i) = B(i-1)*(-n+i-1)*(n+i-1)*(2d0-x)/(i*(i-0.5d0))
sb = sb + B(i)

END DO



Tn(n) = 1 + sb

END DO



DO i = 0,19

s = w(i)*Tn(i)
st = st + s



end do


g = st

gama = v*g
return






end function

