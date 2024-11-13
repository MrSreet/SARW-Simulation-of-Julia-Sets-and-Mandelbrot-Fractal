************************************************************************

* PROYECTO FINAL: CÓDIGO PARA MANDELBROT

CAYETANO HERNÁNDEZ SÁNCHEZ       02.03.2024


************************************************************************        
        
        
        
        PROGRAM MMANDELBROT
      PARAMETER (N=512,NMAX=100000)        ! tamanyo de matriz
      COMPLEX*16 I,C                   ! equiv a real*8
      INTEGER NITER
      REAL*8 X,Y,YMAX,YMIN,XMAX,XMIN,DY,DX
      INTEGER :: IT(N,N)
      
      
      I = COMPLEX(0.0D0,1.0D0)
      
*      XMIN =-0.745 
*      XMAX =-0.705  
*      YMIN = 0.220 
*      YMAX = 0.260 
      
      XMIN =-2.5
      XMAX =0.5 
      YMIN = -1.5
      YMAX =1.5
      
      DX=(XMAX-XMIN)/(N-1)
      DY=(YMAX-YMIN)/(N-1)
      
      X = XMIN
      Y = YMIN
      
      DO J=1,N
        DO K=1,N
          X=XMIN+J*DX
          Y=YMIN+k*DY
          C = X+I*Y
          call mandelbrot(C,NMAX,NITER)
          IT(J,K)= NITER    
        END DO
      END DO
      
      OPEN(10,FILE='mandelbrot.dat',STATUS='UNKNOWN')
*      OPEN(20,file='mauto.dat',status='UNKNOWN')
      
      DO J=1,N
        WRITE(10,*) IT(J,:)
*        WRITE(20,*) IT(J,:)
      END DO
      CLOSE(10)
*      CLOSE(20)
      
      WRITE(*,*) 'Programa finalizado'
      STOP
      END PROGRAM MMANDELBROT
      
      
      
      
      
      
      
      
************************************************************************      
      SUBROUTINE mandelbrot(C,NMAX,NITER)    !devuelve niter dado nmax y c
      COMPLEX*16 Z0,Z,C
      REAL*8 ABSZ
      INTEGER NITER,NMAX
      
      
      Z0   = COMPLEX(0.0D0,0.0D0)
      NITER= 0
      
      DO J=1,NMAX
      
        Z = Z0*Z0 + C
        
        NITER=NITER+1
        
        ABSZ = REAL(CONJG(Z) * Z)
        
        IF (ABSZ.GE.4.0D0) EXIT !SALIMOS DEL BUCLE SI..
        
        Z0=Z
        
      ENDDO
      
        END SUBROUTINE mandelbrot
        
!EXTRAS:


!relacion ecuacion logistica y conjunto de mandelbrot. No es 100 por cien autosimilar
!se puede estudair caminantealeatorio, programa que tenemos. Camino aleatorio 2d
!dimension fractal de mandelbroot( dependera mucho de discretizacion)
!julia sets
