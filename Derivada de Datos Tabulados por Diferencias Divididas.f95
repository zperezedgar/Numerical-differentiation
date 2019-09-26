PROGRAM derivada_datos_Tabulados

IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Declarar Variables
DOUBLE PRECISION :: derivada1, derivada2, derivada3, xmed
INTEGER :: i, j

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER, PARAMETER :: n=3
!INGRESAR EL NUMERO DE PUNTOS!!!!!!!!
!hacemos que n sea parametro para usarlo en arrays
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

DOUBLE PRECISION, DIMENSION(n)::x 
DOUBLE PRECISION, DIMENSION(n)::y
DOUBLE PRECISION, DIMENSION(n,4)::diferencias

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!TERMINA DECLARACION DE VARIABLES

WRITE(*,*)'Bienvenido al programa para Derivar Datos Tabulados'

DO i=1, n, 1
WRITE(*,*)'Ingrese el Punto X', i
READ (*,*) x(i)
WRITE(*,*)'Ingrese el Punto Y', i
READ (*,*) y(i)
END DO

!Construimos las diferenia divididas
!Inicializamos la matriz de diferencias con ceros
DO i=1, n, 1
DO j=1, 4, 1
diferencias(i,j)=0
END DO
END DO

DO i=1, n, 1
diferencias(i,1)=y(i)
END DO

DO i=2, n, 1
DO j=2, 4, 1
diferencias(i,j)=(diferencias(i,j-1)-diferencias(i-1,j-1))/(x(i)-x(i-1))
END DO
END DO
    
!Imprimimos Las derivadas, hAsta la tercera por practicidad
!Imprimimos los titulos de la tabla
WRITE (*,100)
100 FORMAT ('1', T3, 'Tabla de Derivadas')

!Imprimimos los encabezados
WRITE (*,110)
110 FORMAT ('0',T4,' X ',T10,'Primera Derivada',T32,'Segunda Derivada',T54,'Tercera Derivada')
WRITE(*,120)
120 FORMAT(1X,T4,'===',T10,'================',T32,'================',T54,'================='/)

!Imprimimos
DO i=2, n, 1
xmed=(x(i-1)+x(i))/2
derivada1=diferencias(i,2)
derivada2=diferencias(i,3)
derivada3=diferencias(i,4)

WRITE (*,*) xmed, derivada1, derivada2, derivada3
!130 FORMAT (T4, F10.6, T10, F10.6, T32, F10.6, T54, F10.6)
END DO
     
STOP
END PROGRAM

