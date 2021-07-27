c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dspmev.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:59
c**
c*****************************************************
C ***  FILE NAME: DSPMEV
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: DSPMEV                                         **
C **                                                                  **
C **  PURPOSE OF SUBROUTINE: EVALUATE MESH SURF EDGE.                 **
C **                                                                  **
C **   ARGUMENTS:
C **      INPUT:
C **        PE1 THRU PE4 - BEZIER POINTS OF EDGE
C **        PI1 THRU PI4 - INTERIOR POINTS PARALLEL TO EDGE
C **        U            - U VALUE AT WHICH TO EVALUATE EDGE
C **        OFF          - OFFSET VALUE
C **      OUTPUT:
C **        P            - EVALUATED POINT
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE DSPMEV (PE1,PE2,PE3,PE4,PI1,PI2,PI3,PI4,U,OFF,P)
 
      INCLUDE 'com4a.com'
 
      REAL*4 PE1(3),PE2(3),PE3(3),PE4(3),PI1(3),PI2(3),PI3(3),PI4(3)
      REAL*4 P(3),PU(3),PV(3)

C                            EVALUATE EDGE
      U1=(1.-U)**3
      U2=3.*U*(1.-U)**2
      U3=3.*U**2*(1.-U)
      U4=U**3
      DO 10 K=1,3
10      P(K)=U1*PE1(K)+U2*PE2(K)+U3*PE3(K)+U4*PE4(K)
      IF (OFF.EQ.0.) GOTO 999
C                                OFFSET SURFACE
C                       V VEC = (POINT ON INT CV) MINUS (PT ON EDGE)
      DO 20 K=1,3
20      PV(K)=U1*PI1(K)+U2*PI2(K)+U3*PI3(K)+U4*PI4(K)-P(K)
C                       CALC U VEC
      U4=U**2
      U1=-1.+2.*U-U4
      U2=1.-4.*U+3.*U4
      U3=2.*U-3.*U4
      DO 30 K=1,3
30      PU(K)=U1*PE1(K)+U2*PE2(K)+U3*PE3(K)+U4*PE4(K)
C                                 NORMAL VECTOR
      XN=PU(2)*PV(3)-PU(3)*PV(2)
      YN=PU(3)*PV(1)-PU(1)*PV(3)
      ZN=PU(1)*PV(2)-PU(2)*PV(1)
      SEC=SQRT(XN**2+YN**2+ZN**2)
C                                 OFFSET POINT NORMAL TO SF BY 'OFF'
      DIS=0.
      IF (SEC.GT.0.)DIS=OFF/SEC
      P(1)=P(1)+XN*DIS
      P(2)=P(2)+YN*DIS
      P(3)=P(3)+ZN*DIS
 
999   RETURN
      END
