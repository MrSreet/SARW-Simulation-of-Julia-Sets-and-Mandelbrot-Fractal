************************************************************************

* PROYECTO FINAL: CÓDIGO PARA JULIA SETS

CAYETANO HERNÁNDEZ SÁNCHEZ       02.03.2024


************************************************************************
        
        PROGRAM JULIA_SETS
        IMPLICIT NONE 
        
        INTEGER, PARAMETER :: NX = 1000, NY = 1000
        INTEGER :: I, J, K, ITER, MAX_ITER
        REAL*8 :: XMIN = -2.0, XMAX = 2.0, YMIN = -2.0, YMAX = 2.0
*        REAL*8 :: XMIN = -0.745, XMAX = -0.705,YMIN = 0.220, YMAX =0.260 !CAMBIAMOS EL TAMAÑO DE LA PARCELA X E Y PARA COMPROBAR AUTOSIMILITUD
        REAL*8 :: DX, DY, X, Y, ZR, ZI, CR, CI, TR, TI
        REAL*8, DIMENSION(nx,ny) :: JULIA_MATRIX
        REAL*8 :: CR_VAL(9), CI_VAL(9)
      CHARACTER(LEN=30) :: FILENAME
        
        MAX_ITER = 1000 !NUMERO MAXIMO ITERACIONES QUE DETERMINAN LA PERTENENCIA A LOS JULIA SETS 
        
*********************** ESCOGEMOS DIFERENTES C *************************

      CR_VAL=(/-1.0d0,0.0d0,-0.4d0,0.35d0,          
     &                    0.285d0,-0.123d0,-0.75d0,-0.391d0,0.0d0/)
        
      CI_VAL=(/0.0d0,1.0d0,0.6d0,0.35d0,
     &                 0.01d0,0.745d0,0.0d0,0.587d0,0.0d0/)
     
************************************************************************
************************************************************************
****************** BUCLE PARA DIFERENTES FRACTALES *********************
************************************************************************
************************************************************************
        DX = (XMAX - XMIN) / REAL(NX-1) !ESTABLECEMOS LOS DIFERENCIALES
        DY = (YMAX - YMIN) / REAL(NY-1)
        
        DO K = 1, 9
        
            CR = CR_VAL(K)
            CI = CI_VAL(K)
            
            X = XMIN 
            Y = YMIN

************************************************************************
***************** BUCLE ITERACIONES PARA CADA FRACTAL ******************
************************************************************************
            
            DO I = 1, NX 
                X = XMIN + REAL(I-1)*DX !RECORREMOX EN X
                DO J = 1, NY
                    Y = YMIN + REAL(J-1)*DY !RECORREMOS EN Y
                    ZR = X
                    ZI = Y
                    ITER = 0
                    DO WHILE (ITER < MAX_ITER) !BUCLE QUE SIGUE HASTA LLEGAR AL MAXIMO DE ITERACIONES...
                        TR = ZR * ZR - ZI * ZI + CR
                        TI = 2.0D0 * ZR * ZI + CI
                        ZR = TR
                        ZI = TI
                        ITER = ITER + 1
                        IF (ZR*ZR + ZI*ZI > 4.0D0) EXIT !... O HASTA QUE LA SUCESIÓN DIVERJA
                    END DO
                    JULIA_MATRIX(I,J) = REAL(ITER) !PARA CADA PUNTO (I,J) RECONTAMOS LAS ITERACIONES TOTALES LLEVADAS A CABO Y LAS COMPUTAMOS
                END DO
            END DO
            
************************************************************************
************************************************************************
************************************************************************

************************************************************************
*************** FABRICACIÓN MATRICES PARA CADA FRACTAL *****************
************************************************************************        
        
            WRITE(FILENAME,'(a,F5.2,a,F5.2,a)')'js', CR,'_', CI,'.dat' !GENERAMOS DATAFILE EN BASE DE CR Y CI 

*            WRITE(FILENAME,'(a,F5.2,a,F5.2,a)')'aujs',CR,'_',CI,'.dat' !PAR AUTOSIMILITUD GENERAMOS DATAFILE EN BASE DE CR Y CI
            
            OPEN(unit=10, file=filename, status='replace') !ABRIMOS DICHO DATAFILE Y ESCRIBIMOS
            
            DO I = 1, NX
            
                WRITE(10,'(1000(f8.3,1x))')(JULIA_MATRIX(I,J), J = 1,NY)
                
            ENDDO
            
            CLOSE(10)
            
        END DO
        
************************************************************************
************************************************************************
************************************************************************
        
************************************************************************
************************************************************************
************************************************************************
************************************************************************
************************************************************************

        END PROGRAM JULIA_SETS
