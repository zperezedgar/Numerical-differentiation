PROGRAM Derivada_Datos_Uniform_Espaciados

IMPLICIT NONE
!Declarar variables
DOUBLE PRECISION :: x, Xim,  Xi, Xip, Yim,  Yi, Yip, derivada
INTEGER :: trabajar=1

WRITE (*,*) 'Calcular Derivadas para datos No Uniformemente Espaciados'
WRITE (*,*) 'Ingrese el primer punto X(i-1)'
READ (*,*) Xim
WRITE (*,*) 'Ingrese el primer punto Y(i-1)'
READ (*,*) Yim
WRITE (*,*) 'Ingrese el segundo punto Xi'
READ (*,*) Xi
WRITE (*,*) 'Ingrese el segundo punto Yi'
READ (*,*) Yi
WRITE (*,*) 'Ingrese el tercer punto X(i+1)'
READ (*,*) Xip
WRITE (*,*) 'Ingrese el tercer punto Y(i+1)'
READ (*,*) Yip

DO WHILE (trabajar == 1)

WRITE (*,*) 'Ingrese el punto X donde quiere calcular la derivada (interv [X(i-1),X(i+1)])'
READ (*,*) x

!Calculamos la derivada
derivada = Yim*(2*x-Xi-Xip)/((Xim-Xi)*(Xim-Xip))+Yi*(2*x-Xim-Xip)/((Xi-Xim)*(Xi-Xip))+Yip*(2*x-Xim-Xi)/((Xip-Xim)*(Xip-Xi))

WRITE (*,*) 'Derivada= ', derivada

WRITE (*,*) 'Ingrese 1 para trabajar de nuevo, ingrese otro Numero para SALIR '
READ (*,*) trabajar

END DO
STOP
END PROGRAM

