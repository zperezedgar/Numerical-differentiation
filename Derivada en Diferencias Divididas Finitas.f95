PROGRAM Metodos_Numericos_Para_Derivadas				!SOLO MODIFICAR FUNCION HASTA EL FINAL!

IMPLICIT NONE
!Declarar variables
DOUBLE PRECISION :: F, h, x, derivada
INTEGER :: trabajar=1
!WRITE(*,*)'Bienvenido al programa para Derivar'
WRITE(*,*)

DO WHILE (trabajar == 1)

WRITE (*,*)'Ingrese el punto x donde desea calcular la derivada'
READ (*,*) x
WRITE (*,*)'Ingrese el Tamanio de paso h'
READ (*,*) h


!Aquí DESCOMENTAMOSs dependiendo de lo que queramos hacer, Renglon por renglon!

!**************************************************************************************************************
!**************************************************************************************************************

!PRIMERA DERIVADA CENTRADA DE ORDEN 1
!CALL DER1CENTORD1( F(x+h), F(x-h), h, derivada)	
!WRITE(*,*)'Derivada=',derivada
!WRITE(*,*)
!WRITE(*,*) 'Ingrese 1 para trabajar de nuevo, Ingrese otro Numero para SALIR'
!READ (*,*) trabajar
!**************************************************************************************************************
!**************************************************************************************************************

!PRIMERA DERIVADA CENTRADA DE ORDEN 2
CALL DER1CENTORD2( F(x+h), F(x+2*h), F(x-h), F(x-2*h), h, derivada)	
WRITE(*,*)'Derivada=',derivada
WRITE(*,*)
WRITE(*,*) 'Ingrese 1 para trabajar de nuevo, Ingrese otro Numero para SALIR'
READ (*,*) trabajar
!**************************************************************************************************************
!**************************************************************************************************************

!SEGUNDA DERIVADA CENTRADA DE ORDEN 1
!CALL DER2CENTORD1( F(x), F(x+h), F(x-h), h, derivada)	
!WRITE (*,*) derivada
!WRITE(*,*) 'Ingrese 1 para trabajar de nuevo, Ingrese otro Numero para SALIR'
!READ (*,*) trabajar
!**************************************************************************************************************
!**************************************************************************************************************

!SEGUNDA DERIVADA CENTRADA DE ORDEN 2
!CALL DER2CENTORD2( F(x), F(x+h), F(X+2*h), F(X-h), F(X-2*h), h, derivada)	
!WRITE (*,*) derivada
!WRITE(*,*) 'Ingrese 1 para trabajar de nuevo, Ingrese otro Numero para SALIR'
!READ (*,*) trabajar
!**************************************************************************************************************
!**************************************************************************************************************

END DO
STOP
END PROGRAM


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE DER1CENTORD1( fxipp1, fximm1, h, derivate1)
IMPLICIT NONE

!Declaramos Parametros de llamado
DOUBLE PRECISION, INTENT(IN) :: fxipp1
DOUBLE PRECISION, INTENT(IN) :: fximm1
DOUBLE PRECISION, INTENT(IN) :: h
DOUBLE PRECISION, INTENT(OUT) :: derivate1

Derivate1 = ( fxipp1 - fximm1)/(2*h)

RETURN
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE DER1CENTORD2( fxipp1, fxipp2, fximm1, fximm2, h, derivate1)
IMPLICIT NONE

!Declaramos Parametros de llamado
DOUBLE PRECISION, INTENT(IN) :: fxipp1
DOUBLE PRECISION, INTENT(IN) :: fxipp2
DOUBLE PRECISION, INTENT(IN) :: fximm1
DOUBLE PRECISION, INTENT(IN) :: fximm2
DOUBLE PRECISION, INTENT(IN) :: h
DOUBLE PRECISION, INTENT(OUT) :: derivate1

Derivate1 = ( -fxipp2 + 8*fxipp1 - 8*fximm1 + fximm2)/(12*h)

RETURN
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE DER2CENTORD1( fxi, fxipp1, fximm1, h, derivate2)
IMPLICIT NONE

!Declaramos Parametros de llamado
DOUBLE PRECISION, INTENT(IN) :: fxi
DOUBLE PRECISION, INTENT(IN) :: fxipp1
DOUBLE PRECISION, INTENT(IN) :: fximm1
DOUBLE PRECISION, INTENT(IN) :: h
DOUBLE PRECISION, INTENT(OUT) :: derivate2

Derivate2 = ( fxipp1 - 2*fxi + fximm1)/(h**2)

RETURN
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE DER2CENTORD2( fxi, fxipp1, fxipp2, fximm1, fximm2, h, derivate2)
IMPLICIT NONE

!Declaramos Parametros de llamado
DOUBLE PRECISION, INTENT(IN) :: fxi
DOUBLE PRECISION, INTENT(IN) :: fxipp1
DOUBLE PRECISION, INTENT(IN) :: fxipp2
DOUBLE PRECISION, INTENT(IN) :: fximm1
DOUBLE PRECISION, INTENT(IN) :: fximm2
DOUBLE PRECISION, INTENT(IN) :: h
DOUBLE PRECISION, INTENT(OUT) :: derivate2

Derivate2 = ( -fxipp2 + 16*fxipp1 - 30*fxi + 16*fximm1 - fximm2)/(12*(h**2))

RETURN
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DOUBLE PRECISION FUNCTION F(x) 
IMPLICIT NONE
!Declaramos arumentos llamados
DOUBLE PRECISION, INTENT(IN) :: x 

!Evaluar Expresion
F=x*COS(x)*SIN((SQRT((x**2)+1))/2)											!!!!!!!!!!!!!!!!!!!!!!!!!!!!ESTO ES LO QUE SE DEBE CAMBIAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END FUNCTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!