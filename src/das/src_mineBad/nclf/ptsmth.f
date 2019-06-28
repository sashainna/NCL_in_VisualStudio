C*********************************************************************
C*    NAME         :  ptsmth.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       ptsmth.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:31
C********************************************************************/
C
C **********************************************************************
C **  SUBROUTINE NAME: PTSMTH
C **
C **  LAST REVISION:
C **  PURPOSE OF SUBROUTINE: SMOOTH AN ARRAY OF POINTS
C **  INPUT -
C **    BUF      - ARRAY OF POINTS
C **    NPTS     - NUMBER OF POINTS
C **    VS       - VECTOR IN GENERAL DIRECTION OF START OF POINTS
C **    TOL      - SMOOTHING TOLERANCE
C **  OUTPUT -
C **    BUF      - ARRAY OF POINTS SMOOTHED
C **    NPTS     - NUMBER OF SMOOTHED POINTS
C **
C **********************************************************************
C **********************************************************************
 
      subroutine ptsmth (BUF, NPTS, VS, TOL)
 
      INCLUDE 'com4a.com'

      REAL*8 BUF(120), VS(3), TOL
      INTEGER*2 NPTS

      REAL*4 VT(3)

      TOLSQ = TOL**2
      comin = -0.5
      if (sc(169).lt.8.2999) comin = 0.0
      N = NPTS*3
      IX = 0
      DO 12 I=2,NPTS
        D1 = (BUF(IX+4)-BUF(IX+1))**2+(BUF(IX+5)-BUF(IX+2))**2
     X          +(BUF(IX+6)-BUF(IX+3))**2
        IX = IX+3
        IF (D1.GT.TOLSQ) GOTO 12
        IF (I.EQ.NPTS) IX = IX-3
        DO 10 J=IX+1,N
10        BUF(J) = BUF(J+3)
        IX=IX-3
        N=N-3
12    CONTINUE
      NPTS = N/3
      D1 = (BUF(4)-BUF(1))*VS(1)+(BUF(5)-BUF(2))*VS(2)
     X      +(BUF(6)-BUF(3))*VS(3)
      IF (NPTS.GT.2) GOTO 25
      IF (NPTS.EQ.2) GOTO 20
C                                          ONE PT- FAKE UP 2 PTS
14    D1=DSQRT(VS(1)**2+VS(2)**2+VS(3)**2)
      IF (D1.EQ.0.)D1=1.
      D1=TOL/D1/5.
      DO 16 I=1,3
        VT(I)=VS(I)*D1
        BUF(I+3)=BUF(I)+VT(I)
16      BUF(I)=BUF(I)-VT(I)
      NPTS=2
      GOTO 999
C                             TWO PTS - ENSURE CORRECT DIRECTION
20    IF (D1.GT.0.) GOTO 999
      DO 22 I=1,3
        D1 = BUF(I)
        BUF(I) = BUF(I+3)
        BUF(I+3) = D1
22    CONTINUE
      GOTO 999

25    IX = 0
      I = 3
30    VX1 = BUF(IX+4)-BUF(IX+1)
      VY1 = BUF(IX+5)-BUF(IX+2)
      VZ1 = BUF(IX+6)-BUF(IX+3)
      sec1 = sqrt(vx1**2+vy1**2+vz1**2)
      if (sec1.eq.0.0d0) sec1 = 1.0d0
      VX2 = BUF(IX+7)-BUF(IX+4)
      VY2 = BUF(IX+8)-BUF(IX+5)
      VZ2 = BUF(IX+9)-BUF(IX+6)
      sec2 = sqrt(vx2**2+vy2**2+vz2**2)
      if (sec2.eq.0.0d0) sec2 = 1.0d0
      co =  (VX1*VX2+VY1*VY2+VZ1*VZ2)/sec1/sec2
      IF (co.LT.comin) GOTO 40
      IX = IX+3
      I = I+1
      IF (I.GT.NPTS) GOTO 100
      GOTO 30
C                   POINT ARRAY CONTAINS SWITCH BACKS
40    CONTINUE
      ISV = I-1
      IF ((BUF(4)-BUF(1))*VS(1)+(BUF(5)-BUF(2))*VS(2)
     X      +(BUF(6)-BUF(3))*VS(3).GT.0) GOTO 60
C                   POINT ARRAY STARTS IN WRONG DIRECTION - DISCARD FRONT
      N = N-IX
      NPTS = N/3
      DO 50 I=1,N
50      BUF(I) = BUF(I+IX)
      IF (NPTS.LT.3) GOTO 999
      IX = 0
      ISV = 2
60    CONTINUE
C                   DISCARD SWITCH BACKS
      DO 80 I=ISV,NPTS-1
        VX1 = BUF(IX+4)-BUF(IX+1)
        VY1 = BUF(IX+5)-BUF(IX+2)
        VZ1 = BUF(IX+6)-BUF(IX+3)
        VX2 = BUF(IX+7)-BUF(IX+4)
        VY2 = BUF(IX+8)-BUF(IX+5)
        VZ2 = BUF(IX+9)-BUF(IX+6)
        IX = IX+3
        IF (VX1*VX2+VY1*VY2+VZ1*VZ2.GT.0) GOTO 80
        DO 70 J=IX+4,N
70        BUF(J) = BUF(J+3)
        IX=IX-3
        N=N-3
80    CONTINUE
      NPTS = N/3
      IF (NPTS.EQ.1) GOTO 14
      GOTO 999
100   CONTINUE
C                   POINT ARRAY DOES NOT CONTAIN SWITCH BACKS
      IF((BUF(4)-BUF(1))*VS(1)+(BUF(5)-BUF(2))*VS(2)
     X      +(BUF(6)-BUF(3))*VS(3).GT.0) GOTO 999
C                   POINT ARRAY STARTS IN WRONG DIRECTION
      
999   RETURN
      END
