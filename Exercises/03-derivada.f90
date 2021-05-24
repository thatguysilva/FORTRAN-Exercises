PROGRAM precision

!Autor: Álisson Matheus Araújo Silva
!Matrícula: 17/0135888
!Criado em: 23/03/2019

!IMPLICIT NONE

REAL*8 :: f
REAL*8 :: f1, f_1, f2, f_2
REAL*8 :: derivada14, derivada24, derivada34
REAL*8 :: derivada15, derivada25, derivada35, derivada45
REAL*8 :: h = 0.1
REAL*8 :: derivada, derivada2, derivada3, derivada4
REAL*8 :: erro_4P1, erro_4P2, erro_4P3
REAL*8 :: erro_5P1, erro_5P2, erro_5P3, erro_5P4

WRITE(*,*)"--------------------------------------------------------------------------------------------"
WRITE(*,*) "4P        h			erro_f'			 erro_f''			erro_f'''			erro_f'''' "
WRITE(*,*)"--------------------------------------------------------------------------------------------"
DO WHILE (h>=0.00000000000000000000001)
!4 pontos

!Função e valores reais de suas derivadas:
f = h**8 + h**5 + h**2 + 3
derivada = 8*h**7 + 5*h**4 + 2*h
derivada2 = 56*h**6 + 20*h**3 + 2
derivada3 = 336*h**5 + 60*h**2

!Valor avaliado nos pontos de interesse:
f_0 = 3
f1 = h**8 + h**5 + h**2 + 3
f_1 = (-h)**8 + (-h)**5 + (-h)**2 + 3

!Aproximações:
derivada14 = ((-2)*f_1 - 3*3 +6*f1 - f2)/(6*h)
derivada24 = (f_1 - 2*3 + f1)/(h**(2))
derivada34 = (- f_1 + 3*3 - 3*f1 + f2)/(h**(3))

!Erro nas aproximações: 
erro_4P1 = abs(derivada - derivada14)
erro_4P2 = abs(derivada2 - derivada24)
erro_4P3 = abs(derivada3 - derivada34)

WRITE(*,*) h, erro_4P1, erro_4P2, erro_4P3, erro_4P3
h = h*0.1

END DO

WRITE(*,*)" "
WRITE(*,*)"-----------------------------------------------------------------------------------------------"
WRITE(*,*) "5P        h			erro_f'			 erro_f''			erro_f'''			erro_f'''' "
WRITE(*,*)"-----------------------------------------------------------------------------------------------"

h = 0.1

DO WHILE (h>=0.00000000000000000000001)
!5 pontos

!Função e valores reais de suas derivadas:
f = h**8 + h**5 + h**2 + 3
derivada = 8*h**7 + 5*h**4 + 2*h
derivada2 = 56*h**6 + 20*h**3 + 2
derivada3 = 336*h**5 + 60*h**2
derivada4 = 5*336*h**(4) + 120*h

!Valor avaliado nos pontos de interesse:
f_0 = 3
f1 = h**8 + h**5 + h**2 + 3
f2 = (2*h)**8 + (2*h)**5 + (2*h)**(2) + 3
f_1 = (-h)**8 + (-h)**5 + (-h)**(2) + 3
f_2 = (-2*h)**8 + (-2*h)**5 + (-2*h)**(2) + 3

!Aproximações:
derivada15 = (f_2 - 8*f_1 + 8*f1 -f2)/(12*h)
derivada25 = (-f_2 + 16*f_1 -30*3 + 16*f1 - f2)/(12**h**(2))
derivada35 = (- f_2 + 2*f_1 - 2*f1 + f2)/(2*h**(3))
derivada45 = (f_2 - 4*f_1 + 6*3 - 4*f1 + f2)/(h**(4))

!Erro nas aproximações: 
erro_5P1 = abs(derivada - derivada15)
erro_5P2 = abs(derivada2 - derivada25)
erro_5P3 = abs(derivada3 - derivada35)
erro_5P4 = abs(derivada4 - derivada45)

WRITE(*,*) h, erro_5P1, erro_5P2, erro_5P3, erro_5P4
h = h*0.1



END DO









END PROGRAM
