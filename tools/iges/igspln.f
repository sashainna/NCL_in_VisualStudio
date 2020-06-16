C *********************************************************************
C **                                                                 **
C **     COPYRIGHT (C) 1984 MILLS DATA SYSTEMS CORPORATION           **
C **                                                                 **
C *********************************************************************
C **  PROGRAM NAME: igspln                                           **
C **                                                                 **
C **  WRITTEN BY: I.DONALDSON                    ON: 07 MAY 84       **
C **  LAST REVISION BY: R. HERRING               ON: 29 MAY 89       **
C **                                                                 **
C **     MODULE NAME AND RELEASE LEVEL                               **
C **       igspln.f , 25.1                                                 **
C **    DATE AND TIME OF LAST  MODIFICATION                          **
C **       04/29/15 , 15:12:42                                                 **
C **                                                                 **
C **  PURPOSE OF MODULE:                                             **
C **                                                                 ** 
C **    THIS FILE CONTAINS THE FOLLOWING SUBROUTINES TO CONVERT      **
C **    AN IGES RECORD FOR A PARAMETRIC SPLINE INTO NCL CANONICAL    **
C **    FORM FOR A CURVE, TRANSFORMING IF NECESSARY.                 **
C **                                                                 **
C **       IGSPLN - HANDLE PARAMETRIC SPLINE                         **
C **                                                                 **
C **  The following routines are in support of IGSPLN:               **
C **                                                                 **
C **       IGTRNS                                                    **
C **       IGMAT                                                     **
C **       IGTRAN                                                    **
C **       IGTRAN (RD,RMX,NWDS,IT,IW)                                **
C **       CONENT                                                    **
C **       CRVFIT(RI,VE,NENT,NMP)                                    **
C **       FARSLP(INV,NUM,IVF,IRET)                                  **
C **       SEGCHK(XC,YC,ZC,I,K,ITSK)                                 **
c **       CRVPRE (W,NWDS,NMP)                                       **
C **                                                                 **
C *********************************************************************
  
C *********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: IGSPLN                                         **
C **                                                                  **
C **  PURPOSE OF PROGRAM: Convert an IGES record of a PARAMETRIC      **
C **       SPLINE into NCL canonical form.   This routine was taken   **
C **       from 501 IGES and modified to extract the IGES record      **
C **       information from UNIBASE data structures.  Some subroutines**
C **       from 501 IGES have been converted to C to serve this       **
C **       purpose.                                                   **
C **                                                                  **
C **********************************************************************
C **********************************************************************
      SUBROUTINE  IGSPLN(RDR)
C                               * HANDLE PARAMETRIC SPLINE
      INCLUDE 'iges.com'
 
      REAL*8 RDR(600)
 	 
      REAL*8 B(13)
      REAL*4 BB(24)
      EQUIVALENCE(B,BB)

      REAL*8 T(201), A(12), VE(6)
      INTEGER*4 I
      INTEGER*2 J, K, N, NPTS, NWDS

C HANDLE A MAXIMUM OF 200 PTS. IF THERE ARE 99 POINTS OR LESS USE ALL
C POINTS PLUS CALCULATED MID POINTS BETWEEN EACH OF THE SUPPLIED POINTS.
C ALSO CALCULATE THE VECTORS FOR THE FIRST AND LAST POINTS. PASS POINTS
C IN RDR AND VECTORS IN VE TO CRVFIT THEN CALL CRVPRE TO GET CANONICAL
C FORM OF CV BACK IN RDR.
C ARRAY T HOLDS THE SUPPLIED BREAK POINTS. ARRAY A HOLDS THE COEFFICIENTS
C SUPPLIED FOR THE X,Y AND Z POLYNOMIALS.

C Get number of segments:
      call igtseg(i)
      N = I
      IF (N.GT.199) THEN
        CALL IGEROR(3)
        ERRFLG=.TRUE.
        GO TO 99999       
      ENDIF

      IX = -2
      NPTS = 1
	  D=0.0

C Get the T array:
      call igett(t)
 
C For each segment, get the X, Y and Z coordinate polynomials
      DO 300 J=1,N
      call igtxyz(j,a)
C..                                                      10-JUN-87
C..             IF 1ST SEG OF A 2-PT CRV, ADD TO B-TBL FOR LATER USE
      IF(N.NE.1.OR.J.NE.1)GOTO 209
      DO 202 K=1,12
202   B(K)=A(K)
209   CONTINUE
C..
        RDR(IX+3) = A(1)
        RDR(IX+4) = A(5)
        RDR(IX+5) = A(9)
        CALL IGTRNS(RDR(IX+3),3,3)
210     IF (J.NE.1) D = DSQRT((RDR(IX)-RDR(IX+3))**2
     1                       +(RDR(IX+1)-RDR(IX+4))**2
     2                       +(RDR(IX+2)-RDR(IX+5))**2)
C                    IF THE LAST POINT IS TOO CLOSE TO 2ND LAST,
C                    BACK OUT 2ND LAST AND CHECK AGAIN.
C                     USE THIS POINT IF IT IS THE FIRST OR IT IS MORE THAN
C                     .001 FROM PREVIOUS POINT.
        IF (J.EQ.1.OR.D.GE..001) THEN
          IX = IX+3
        ENDIF
C                     GET THE MID POINT BETWEEN THIS POINT AND NEXT POINT.
        IF (N.LE.99) THEN
          U = (T(J)+T(J+1))/2 - T(J)
          RDR(IX+3) = A(1) + A(2)*U + A(3)*U**2 + A(4)*U**3
          RDR(IX+4) = A(5) + A(6)*U + A(7)*U**2 + A(8)*U**3
          RDR(IX+5) = A(9) + A(10)*U + A(11)*U**2 + A(12)*U**3
          CALL IGTRNS(RDR(IX+3),3,3)
          D = DSQRT((RDR(IX)-RDR(IX+3))**2+(RDR(IX+1)-RDR(IX+4))**2
     1                       +(RDR(IX+2)-RDR(IX+5))**2)
          IF (D.GE..001) THEN
            IX=IX+3
          ENDIF
        ENDIF

        IF (J.EQ.1) THEN
          VE(1) = A(2)
          VE(2) = A(6)
          VE(3) = A(10)
          CALL IGTRNS(VE,4,3)
        ENDIF
300   CONTINUE

      U = T(N+1) - T(N)
      RDR(IX+3) = A(1) + A(2)*U + A(3)*U**2 + A(4)*U**3
      RDR(IX+4) = A(5) + A(6)*U + A(7)*U**2 + A(8)*U**3
      RDR(IX+5) = A(9) + A(10)*U + A(11)*U**2 + A(12)*U**3
      CALL IGTRNS(RDR(IX+3),3,3)
301   D = DSQRT((RDR(IX)-RDR(IX+3))**2 +(RDR(IX+1)-RDR(IX+4))**2
     1                       +(RDR(IX+2)-RDR(IX+5))**2)
C                    IF THE LAST POINT IS TOO CLOSE TO 2ND LAST,
C                    BACK OUT 2ND LAST AND CHECK AGAIN.
      IF (D.LT..001) THEN
          RDR(IX)   = RDR(IX+3)
          RDR(IX+1) = RDR(IX+4)
          RDR(IX+2) = RDR(IX+5)
          IX=IX-3
          IF (IX.GT.0) GOTO 301
          CALL IGEROR(8)
          ERRFLG=.TRUE.
          GOTO 99999
      ELSE
          IX = IX+3
      ENDIF
C Calculate the last vector ...
      VE(4) = A(2) + 2*A(3)*U + 3*A(4)*U**2
      VE(5) = A(6) + 2*A(7)*U + 3*A(8)*U**2
      VE(6) = A(10) + 2*A(11)*U + 3*A(12)*U**2
      CALL IGTRNS(VE(4),4,3)

C NCL: for debugging...
C     call igtxyz(N+1,a)
C NCL: end debugging
      NPTS = (IX+2)/3

C..................  IF ONLY 1 SEG, PREPARE CANON LIST NOW.      10-JUN-87
      IF(N.NE.1)GOTO 400
C                       MOVE B-TBL BACK TO A-TBL 
      DO 310 K=1,12
310   A(K)=B(K)
C                       CORRECT EQ COEFS PER T(2)
      TT=T(2)-T(1)
      TT2=TT**2
      TT3=TT*TT2
      DO 315 I=2,10,4
      A(I)=A(I)*TT
      A(I+1)=A(I+1)*TT2
315   A(I+2)=A(I+2)*TT3
C                       ADD P0 , P1 TO B FROM RDR
      DO 320 K=1,3
      B(K+1)=RDR(K)
320   B(K+7)=RDR(K+6)
C                       CALC PTQ AND ADD TO RDR(4 ) FOR TRANSFORM
      RDR(4)=A(1)+A(2)/3.
      RDR(5)=A(5)+A(6)/3.
      RDR(6)=A(9)+A(10)/3.
      CALL IGTRNS(RDR(4),3,3)
C                        DITTO FOR PTR IN RDR(10)
      RDR(10)=A(1)+A(2)*2./3.+A(3)/3.
      RDR(11)=A(5)+A(6)*2./3.+A(7)/3.
      RDR(12)=A(9)+A(10)*2./3.+A(11)/3.
      CALL IGTRNS(RDR(10),3,3)
C                        PTQ DELTAS IN BB(9)
      BB(9)=RDR(4)-RDR(1)
      BB(10)=RDR(5)-RDR(2)
      BB(11)=RDR(6)-RDR(3)
C                        DU/DS,  DU/DS,  RHO
      BB(12)=1.
      BB(13)=0.
      BB(14)=1.
C                        PTR DELTAS
      BB(21)=RDR(7)-RDR(10)
      BB(22)=RDR(8)-RDR(11)
      BB(23)=RDR(9)-RDR(12)
C                        FINUP WITH 2.,1.   IN FIRST BWD
      BB(1)=2.
      BB(2)=1.
      B(13)=0.
      NWDS=13
C                          LOAD B-TBL IN RDR AND GO STO IT
      DO 330 K=1,13
330   RDR(K)=B(K)
      GOTO 500
C..........END OF CHG                                            10-JUN-87
400   CONTINUE

      CALL CRVFIT(RDR,VE,NPTS ,N)
      IF (.NOT.ERRFLG) CALL CRVPRE(RDR,NWDS,N)

500   CONTINUE

99999 RETURN
      END
C *********************************************************************
C **                                                                 ** 
C **  NOTE: The following routines were taken from various 501 IGES  ** 
C **        files.  These subroutine provide support to IGSPLN().    ** 
C **                                                                 ** 
C *********************************************************************
C *********************************************************************
C **  SUBROUTINE NAME: IGTRNS                                        **
C **                                                                 **
C **  PURPOSE OF SUBROUTINE:                                         **
C **                                                                 **
C **    Modified so that igmat() is now called from a C routine.     **
C **                                                                 **
C **    TO FIND CONVERSION MATRIX POINTED TO BY DIRECTORY RECORD     **
C **    IMATP AND CALL IGTRAN TO HANDLE THE CONVERSION. THE LAST     **
C **    MATRIX USED IS AT THE DIRECTORY RECORD POINTED TO BY LMATP   **
C **    AND IS IN ARRAY TRD. IF LMATP AND IMATP ARE THE SAME, JUST   **
C **    USE TRD. OTHERWISE CALL IGMAT TO PUT REQUIRED MATRIX INTO    **
C **    TRD AND SET LMATP TO IMATP.                                  **
C **                                                                 **
C *********************************************************************
  
      SUBROUTINE  IGTRNS (RDR,NT,NWDS)
      INCLUDE 'iges.com'

      REAL*8 RDR(600)
      INTEGER*2 NT, NWDS

      CALL IGTRAN(RDR,TRD,NWDS,NT,0)
	  call igvctmsc(rdr)

99999 RETURN
      END
C *********************************************************************
C *********************************************************************
C **  SUBROUTINE NAME: IGMAT                                         **
C **                                                                 **
C **  WRITTEN BY: I.DONALDSON                  ON: 18 APRIL 84       **
C **  LAST REVISION BY: R.HERRING              ON: 26 MAY 89         **
C **                                                                 **
C **  PURPOSE OF SUBROUTINE:                                         **
C **                                                                 ** 
C **    INITIALIZE TRANSFORMATION MATRIX                             **
C **                                                                 **
C *********************************************************************
C *******************************************************************
      SUBROUTINE IGMAT (RDR,CTOL)
      INCLUDE 'iges.com'
 
      REAL*8 RDR(12)
	  REAL*8 CTOL

      FTOL = CTOL
      ERRFLG=.FALSE.
      DO 100 I=1,12
100     TRD(I)=RDR(I)


99999 RETURN
      END
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: IGTRAN - SLIGTHLY ADAPTED FROM NCL TRANSF      **
C **                                                                  **
C **  LAST REVISION:                                                  **
C **                                                                  **
C **  PURPOSE OF PROGRAM: HANDLES TRANSFORMATION THROUGH A MATRIX FOR **
C **    REFSYS OR TRACUT.                                             **
C **                                                                  **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE IGTRAN (RD,RMX,NWDS,IT,IW)

      INCLUDE 'iges.com'
 
      REAL*8 RD(7),RMX(15),Q(3)
      REAL*8 TR8
      REAL*4 TR4(2)
      INTEGER*2 INP(3)
      EQUIVALENCE(TR4,TR8,INP)
      INTEGER*2 IT,IW
 
      TR8=RD(1)
      IF (IT.NE.3.AND.IT.NE.4) GO TO 100
          CALL CONENT(RD,RMX,IT,IW)
          GO TO 99999
100   IF (IT.NE.5) GO TO 200
          CALL CONENT(RD,RMX,3,IW)
          CALL CONENT(RD(4),RMX,4,IW)
          GO TO 99999
200   IF (IT.NE.6) GO TO 300
          IX=1
C  210        RD(IX+3)=RD(IX+3)-RD(IX)*RMX(4)-
C       1        RD(IX+1)*RMX(8)-RD(IX+2)*RMX(12)
C             PUT NPT IN Q
210   K=IX-1
      DIS=RD(K+4)
      Q(1)=RD(K+1)*DIS
      Q(2)=RD(K+2)*DIS
      Q(3)=RD(K+3)*DIS
C             TRANSFORM NPT IN PLACE
      CALL CONENT(Q,RMX,3,IW)
C             TRANFORM PL NORMAL
      CALL CONENT(RD(IX),RMX,4,IW)
C             CALC CONST AND UNITIZE THE ABCD EQ.
      RD(K+4)=RD(K+1)*Q(1)+RD(K+2)*Q(2)+RD(K+3)*Q(3)
      SEC=DSQRT(RD(K+1)**2+RD(K+2)**2+RD(K+3)**2)
C          IF SEC VERY SMALL, MUST BE CIRCLE UNBOUNDED CASE.  EXIT
      IF(SEC.LT..0001)GOTO 99999
      RD(K+1)=RD(K+1)/SEC
      RD(K+2)=RD(K+2)/SEC
      RD(K+3)=RD(K+3)/SEC
      RD(K+4)=RD(K+4)/SEC
           GO TO 99999
300   IF (IT.NE.8.AND.IT.NE.81) GO TO 400
          NW=1
          IF (IT.EQ.8) NW=(TR4(1)+1)/2+1
310       CALL CONENT(RD(NW),RMX,3,IW)
          CALL CONENT(RD(NW+3),RMX,41,IW)
          NW=NW+6
          IF (NW.LT.NWDS) GO TO 310
          GO TO 99999
400   IF (IT.NE.9.AND.IT.NE.91) GO TO 500
C                  HANDLE SURFACE PATCH
          IF (IT.EQ.9) GO TO 404
              INP(2)=0
              INP(3)=NWDS
              NW=1
              GO TO 407
C                  HANDLE SURFACE PANEL
404       INP(3)=8
          IF (INP(1).EQ.0) INP(3)=14
406       NW=3+(INP(2)+1)/2
407       DO 490,J=1,INP(2)+1
          CALL CONENT(RD(NW),RMX,3,IW)
          INP(1)=NW+INP(3)-1
          NW=NW+3
410       IX=0
420       CALL CONENT(RD(NW),RMX,41+IX,IW)
          NW=NW+1+IX
          IF (NW.LT.INP(1)) GO TO 480
              NW=NW+1
              IF (NW.GE.NWDS) GO TO 99999
              GO TO 490
480       IF (IX.EQ.1) GO TO 410
          IX=1
          GO TO 420
490       CONTINUE
          GO TO 99999
500   IF (IT.NE.7) GO TO 99999
          CALL CONENT(RD,RMX,3,IW)
          CALL CONENT(RD(4),RMX,4,IW)
C             FACTOR VEC TO UNITY AND RAD BY INVERSE OF SAME
      SEC=DSQRT(RD(4)**2+RD(5)**2+RD(6)**2)
C             IF SEC TOO SMALL, EXIT
      IF(SEC.GT..00001)GOTO 510
C     IFL(2)=163
      GOTO 99999
510   RD(4)=RD(4)/SEC
      RD(5)=RD(5)/SEC
      RD(6)=RD(6)/SEC
      RD(7)=RD(7)*SEC
          IX=8
          GO TO 210
99999 RETURN
      END
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: CONENT - SLIGHTLY ADAPTED FROM NCL CONENT      **
C **                                                                  **
C **  LAST REVISION:                                                  **
C **                                                                  **
C **  PURPOSE OF PROGRAM: DOES THE ACTUAL CONVERSION THROUGH A MATRIX **
C **    OF POINTS AND VECTORS.  POINTS ARE ALWAYS IN REAL*8 FORM.     **
C **    VECTORS CAN BE IN REAL*8 FORM OR REAL*4 FORM.  IF THEY ARE IN **
C **    REAL*4 FORM, THEY ARE CONTAINED IN 2 REAL*8'S, EITHER RIGHT   **
C **    OR LEFT JUSTIFIED.                                            **
C **                                                                  **
C **       RD      DATA TO BE TRANSFORMED                             **
C **       RMX     MATRIX TO BE USED                                  **
C **       IT      ITEM TYPE                                          **
C **                   3   POINT                                      **
C **                   4   VECTOR IN REAL*8 FORM                      **
C **                   41  VECTOR IN REAL*4 FORM, LEFT JUSTIFIED      **
C **                   42  VECTOR IN REAL*4 FORM, RIGHT JUSTIFIED     **
C **       IW      WHICH MATRIX FORM TO USE (PUTENT/TRACUT VS GETENT) **
C **                                                                  **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE CONENT (RDR,RMX,IT,IW)
 
      INCLUDE 'iges.com'

      INTEGER*2 IT,IW
      REAL*4 T4(4),TT(4)
      REAL*8 T8(2)
      EQUIVALENCE(T4,T8)
      REAL*8 RMX(15),RDR(3),T(3)
      EQUIVALENCE(T,TT)

      IX=0
      IF(IT.EQ.42) IX=1
      T8(1)=RDR(1)
      T8(2)=RDR(2)
      JJ=1
      IF (IT.NE.3.AND.IT.NE.4) GO TO 100
C                                                        ****** POINT
          DO 10,J=1,3
              T(J)=RDR(1)*RMX(JJ)+RDR(2)*RMX(JJ+1)+RDR(3)*RMX(JJ+2)
              IF (IT.EQ.3) T(J)=T(J)+RMX(JJ+3)
              JJ=JJ+4
10        CONTINUE
          GO TO 88888
C                                                        ****** VECTOR
100       DO 200,J=1,3
              TT(J+IX)=T4(1+IX)*RMX(JJ)+T4(2+IX)*RMX(JJ+1)+
     1                 T4(3+IX)*RMX(JJ+2)
              JJ=JJ+4
200       CONTINUE
      IF (IX.EQ.0) TT(4)=T4(4)
      IF (IX.EQ.1) TT(1)=T4(1)
88888 RDR(1)=T(1)
      RDR(2)=T(2)
      IF (IT.EQ.3.OR.IT.EQ.4) RDR(3)=T(3)
99999 RETURN
      END
C *********************************************************************
C **  SUBROUTINE NAME: CRVFIT                                        **
C **                                                                 **
C **  WRITTEN BY: I.DONALDSON                    ON: 07 MAY 84       **
C **  LAST REVISION BY: R. HERRING               ON: 31 MAY 89       **
C **                                                                 **
C **                                                                 **
C **  PURPOSE OF SUBROUTINE:                                         **
C **                                                                 ** 
C **                                                                 **
C *********************************************************************
      SUBROUTINE CRVFIT(RI,VE,NENT,NMP)
 
C          INPUT:   ANY STRING OF PTS (W/WO VECS)    NENT=NUM INPUT PTS
C          OUTPUT:  20 (MAX) PT-VECS FOR CRVPRE      NMP=NUM OUTPUT PTS
      INCLUDE 'iges.com'

      COMMON/PBLOK/X,Y,Z,A,B,C,DX,DY,DZ,CH,HX,HY,HZ,HA,HB,HC
C
      REAL*8 X(20),Y(20),Z(20)
      REAL*4 A(20),B(20),C(20),DX(20),DY(20),DZ(20),CH(20)
      REAL*8 HX(20),HY(20),HZ(20)
      REAL*4 HA(20),HB(20),HC(20)
C
      REAL*8 RI(600),VE(6)
      INTEGER*2 INV(20)
 
C          INIT
      KX=0
      LASTX=0
      KENT=0
      IPTK=0
      IVF=0
      IV20=0
      KASE18=0

      GOTO 20
 
C          UPSHIFT ANY LEFT-OVER PTVECS  + INV'S
10    I=0
      DO 15 J=JX,20
      I=I+1
      X(I)=X(J)
      Y(I)=Y(J)
      Z(I)=Z(J)
      A(I)=A(J)
      B(I)=B(J)
      C(I)=C(J)
15    INV(I)=INV(J)
C          TURN-ON INV(1) AND V20CHK=0
      INV(1)=1
      IV20=0
      IPTK=I
      IVF=1
C                       GET NEXT PT FROM INPUT
20    KENT=KENT+1
      IPTK=IPTK+1
      X(IPTK)=RI(KENT*3-2)
      Y(IPTK)=RI(KENT*3-1)
      Z(IPTK)=RI(KENT*3  )
      INV(IPTK)=0
      IF(KENT.GT.1)GOTO 25
      IIX=1
      GOTO 30
25    IF(KENT.LT.NENT)GOTO 35
      IIX=4
C              VECTOR
30    INV(IPTK)=1
      IVF=1
      A(IPTK)=VE(IIX)
      B(IPTK)=VE(IIX+1)
      C(IPTK)=VE(IIX+2)
 
35    IF(KENT.GE.NENT)GOTO 40
      IF(IPTK.LT.20)GOTO 20
 
C              FAIR SLOPES   (MODE 1)
40    IRET=1

C      WRITE(8,9100)
C9100  FORMAT(' CRVFIT PTS')
C      DO 36 I=1,20
C        WRITE(8,9110) X(I),Y(I),Z(I),A(I),B(I),C(I)
C9110    FORMAT(6(F12.7))
C36    CONTINUE
 
      CALL FARSLP(INV,IPTK,IVF,IRET)
      IF (IRET.NE.0) THEN
        CALL IGEROR(6+IRET)
        ERRFLG=.TRUE.
        GO TO 999
      ENDIF
C              IMPROVE PTS  2 THRU LAST VIA DELTAS. UNLESS INV ON.
C                  ( LAST=18 OR NPTS-1 )
      LST=18
      IF(IPTK.LT.20.OR.KENT.EQ.NENT) LST=IPTK-1
      DO 50 J=2,LST
      IF(INV(J).NE.0)GOTO 50
      I=J-1
      K=J+1
      CALL SEGCHK(XC,YC,ZC,I,K,1)
      IF(ERRFLG)GOTO 999
      DX(J)=XC-X(J)
      DY(J)=YC-Y(J)
      DZ(J)=ZC-Z(J)
50    CONTINUE
 
C          NOW FIX INDICATED POINTS
      DO 55 I=2,LST
      IF(INV(I).NE.0)GOTO 55
      HMOV=SQRT( DX(I)**2+DY(I)**2+DZ(I)**2 )
      IF(HMOV.LT.1.E-6)GOTO 55
      RO=FTOL/HMOV
      IF(RO.GT.1.)RO=1.
      X(I)=X(I)+DX(I)*RO
      Y(I)=Y(I)+DY(I)*RO
      Z(I)=Z(I)+DZ(I)*RO
55    CONTINUE
 
C          FAIR SLPS AGAIN    MODE=2
 
 
      IRET=2
      CALL FARSLP(INV,IPTK,IVF,IRET)
      IF (IRET.NE.0) THEN
        CALL IGEROR(6+IRET)
        ERRFLG=.TRUE.
        GO TO 999
      ENDIF
C          SLPFAIR OFF
 
C          SEGFIT THIS XYZTBL FOR SAVPTS
      JX=1
59    LX=JX
C          IF NO HPT(1), GO STO IT NOW
      IF(KX.EQ.0)GOTO 70
 
60    LX=LX+1
      IF(LX-JX.LT.2)LX=LX+1
      KASE18=0
      IF(KENT.GE.NENT)GOTO 65
      IF(LX.LT.19)GOTO 65
      KASE18=1
      LX=18
      IF(JX.NE.1)GOTO 10
      GOTO 70
 
65    IF(LX.LE.IPTK)GOTO 67
      LX=IPTK
      GOTO 70
67    CALL SEGCHK(XC,YC,ZC,JX,LX,2)
 
      IF (ERRFLG)GOTO 999
      IF(ABS(XC)+ABS(YC)+ABS(ZC).LT.FTOL)GOTO 68
      LX=LX-1
      GOTO 70
68    IF(INV(LX).EQ.0)GOTO 60
C              ADD PT TO HOLD TBL
70    KX=KX+1
      IF(KX.LT.21)GOTO 71
      CALL IGEROR(1)
      ERRFLG=.TRUE.
      GOTO 999
71    HX(KX)=X(LX)
      HY(KX)=Y(LX)
      HZ(KX)=Z(LX)
      HA(KX)=A(LX)
      HB(KX)=B(LX)
      HC(KX)=C(LX)
      JX=LX
      IF(LX.LT.IPTK.AND.KASE18.EQ.0)GOTO 60
      IF(KENT.LT.NENT)GOTO 10
 
C              ALL DONE.  MOVE HPTS TO XYZ AND CALC DX,DY,DZ,CH
      DO 80 I=1,KX
      X(I)=HX(I)
      Y(I)=HY(I)
      Z(I)=HZ(I)
      A(I)=HA(I)
      B(I)=HB(I)
      C(I)=HC(I)
      IF(I.EQ.KX) GOTO 80
      DX(I)=HX(I+1)-HX(I)
      DY(I)=HY(I+1)-HY(I)
      DZ(I)=HZ(I+1)-HZ(I)
      CH(I)=SQRT(DX(I)**2+DY(I)**2+DZ(I)**2)
80    CONTINUE
      NMP=KX

C      IF (ICV.EQ.1.OR.ICV.EQ.21) THEN
C      WRITE(8,9200)NMP,NENT
C9200  FORMAT(' END CRVFIT PTS,NMP=',I3,' NENT=',I3)
C      DO 90 I=1,20
C        WRITE(8,9210) X(I),Y(I),Z(I),A(I),B(I),C(I)
C9210    FORMAT(6(F12.7))
C        WRITE(8,9210) DX(I),DY(I),DZ(I),CH(I)
C90    CONTINUE
C      ENDIF

 
999   RETURN
      END
C *********************************************************************
C **  SUBROUTINE NAME: FARSLP                                        **
C **                                                                 **
C **  WRITTEN BY: I.DONALDSON                    ON: 07 MAY 84       **
C **  LAST REVISION BY: R. HERRING               ON: 31 MAY 89       **
C **                                                                 **
C **                                                                 ** 
C ** COPYRIGHT (C) 1981,1982 MILLS DATA SYSTEMS CORPORATION          **
C **                                                                 ** 
C **                                                                 **
C **  PURPOSE OF SUBROUTINE:                                         **
C **                                                                 **
C *********************************************************************
      SUBROUTINE FARSLP(INV,NUM,IVF,IRET)

      INCLUDE 'iges.com'

      COMMON/PBLOK/X,Y,Z,A,B,C,DX,DY,DZ,CH,HX,HY,HZ,HA,HB,HC
C
      REAL*8 X(20),Y(20),Z(20)
      REAL*4 A(20),B(20),C(20),DX(20),DY(20),DZ(20),CH(20)
      REAL*8 HX(20),HY(20),HZ(20)
      REAL*4 HA(20),HB(20),HC(20)
C
      INTEGER*2 INV(20)
 
      MODE=IRET
      STOL=1.E-5
      IF(MODE.EQ.1)STOL=1.E-4
       IRET=1
       IF (NUM.LT.2) GOTO 99
       IRET=0
       ISMALL = 0
       IF (NUM-3)7,4,8
4      IF (IVF)7,7,8
7      ISMALL=1
8      N=NUM
       M=N-1
       DO 10 I=1,M
       DX(I)=X(I+1)-X(I)
       DY(I)=Y(I+1)-Y(I)
       DZ(I)=Z(I+1)-Z(I)
       CH(I)=SQRT(DX(I)**2+DY(I)**2+DZ(I)**2)
       IF (CH(I).GE..001) GOTO 10
       IRET=2
       GOTO 99
10     CONTINUE
       DX(N)=DX(M)
       DY(N)=DY(M)
       DZ(N)=DZ(M)
       CH(N)=CH(M)
       IF(MODE.EQ.2)GOTO 21
       DO 20 I=1,N
       IF (INV(I).GT.0)GOTO 14
       IF (I.EQ.1.OR.I.EQ.N) GOTO 20
       RO=(CH(I-1)/CH(I))**2
       A(I)=DX(I-1)+RO*DX(I)
       B(I)=DY(I-1)+RO*DY(I)
       C(I)=DZ(I-1)+RO*DZ(I)
14     TL=SQRT(A(I)**2+B(I)**2+C(I)**2)
       IF (ISMALL.EQ.1) GOTO 15
       IF (A(I)*DX(I)+B(I)*DY(I)+C(I)*DZ(I).LT.0.)TL=-TL
15     IF (TL.NE.0) GOTO 16
       IRET=3
       GOTO 99
16     A(I)=A(I)/TL
       B(I)=B(I)/TL
       C(I)=C(I)/TL
20     CONTINUE
21     IKNT=0
       VCHG=1.
       GOTO 42
30     VCHG=0.
       DO 40 J=2,M
       IF (INV(J).GT.0) GOTO 40
       I=J-1
       K=J+1
       CAL=(A(I)*DX(I)+B(I)*DY(I)+C(I)*DZ(I))/CH(I)
       ADIS=.531*CH(I)/(.593+CAL)
       AL=CH(I)*(1.104+.13*CAL)/(.851+CAL)
       SA=CH(I)*(3.565+.24*CAL)/(2.805+CAL)
       DAX=DX(I)-A(I)*ADIS
       DAY=DY(I)-B(I)*ADIS
       DAZ=DZ(I)-C(I)*ADIS
       CBE=(DX(J)*A(K)+DY(J)*B(K)+DZ(J)*C(K))/CH(J)
       BDIS=.531*CH(J)/(.593+CBE)
       BL=CH(J)*(1.104+.13*CBE)/(.851+CBE)
       SB=CH(J)*(3.565+.24*CBE)/(2.805+CBE)
       RO=AL*SA/(BL*SB)
       DBX=(DX(J)-BDIS*A(K))*RO+DAX
       DBY=(DY(J)-BDIS*B(K))*RO+DAY
       DBZ=(DZ(J)-BDIS*C(K))*RO+DAZ
       TL=SQRT(DBX**2+DBY**2+DBZ**2)
       DBX=DBX/TL
       DBY=DBY/TL
       DBZ=DBZ/TL
       VDEL=ABS(DBX-A(J))+ABS(DBY-B(J))+ABS(DBZ-C(J))
       IF (VDEL.GT.VCHG)VCHG=VDEL
       A(J)=DBX
       B(J)=DBY
       C(J)=DBZ
40     CONTINUE
42     IF (INV(1).GT.0) GOTO 50
       CO=(DX(1)*A(2)+DY(1)*B(2)+DZ(1)*C(2))/CH(1)
       A(1)=2.*CO*DX(1)/CH(1)-A(2)
       B(1)=2.*CO*DY(1)/CH(1)-B(2)
       C(1)=2.*CO*DZ(1)/CH(1)-C(2)
50     IF (INV(N).GT.0) GOTO 60
       CO=(A(M)*DX(M)+B(M)*DY(M)+C(M)*DZ(M))/CH(M)
       A(N)=2.*CO*DX(M)/CH(M)-A(M)
       B(N)=2.*CO*DY(M)/CH(M)-B(M)
       C(N)=2.*CO*DZ(M)/CH(M)-C(M)
60     CONTINUE
       IF (ISMALL.GT.0) GOTO 99
       IF (VCHG.LT.STOL) GO TO 70
       IKNT=IKNT+1
       IF (IKNT.LT.16) GO TO 30
       IRET=4
70     DO 74 J=1,N
       I=J-1
       IF (J.EQ.1) GOTO 72
       IF (A(J)*DX(I)+B(J)*DY(I)+C(J)*DZ(I).LT.0.) GOTO 90
72     IF (A(J)*DX(J)+B(J)*DY(J)+C(J)*DZ(J).LT.0.) GOTO 90
74     CONTINUE
       GOTO 99
90     IRET=5
99     RETURN
       END
C *********************************************************************
C **  SUBROUTINE NAME: SEGCHK                                        **
C **                                                                 **
C **  WRITTEN BY: I.DONALDSON                    ON: 07 MAY 84       **
C **  LAST REVISION BY: R. HERRING               ON: 31 MAY 89       **
C **                                                                 **
C **                                                                 ** 
C ** COPYRIGHT (C) 1981,1982 MILLS DATA SYSTEMS CORPORATION          **
C **                                                                 ** 
C **                                                                 **
C **  PURPOSE OF SUBROUTINE:                                         **
C **       BUILD A BEZCUB PT(I) TO PT(K)                             **
C **         ITSK=1     RETURN XC,YC,ZC TO CALLING ROUTINE           **
C **         ITSK=2     STUDY ALL INTERMEDIATE PTS  (P/O TEMP)       **
C **                                                                 **
C *********************************************************************
      SUBROUTINE SEGCHK(XC,YC,ZC,I,K,ITSK)
 
      INCLUDE 'iges.com'

      COMMON/PBLOK/X,Y,Z,A,B,C,DX,DY,DZ,CH,HX,HY,HZ,HA,HB,HC
C
      REAL*8 X(20),Y(20),Z(20)
      REAL*4 A(20),B(20),C(20),DX(20),DY(20),DZ(20),CH(20)
      REAL*8 HX(20),HY(20),HZ(20)
      REAL*4 HA(20),HB(20),HC(20)
 
 
C      IF(K.LT.21)GOTO 6
C      IFL(2)=43
C      GOTO 99
6     CTAN=1.
      HDX=0.
      HDY=0.
      HDZ=0.
      J=I+1
      DELX=X(K)-X(I)
      DELY=Y(K)-Y(I)
      DELZ=Z(K)-Z(I)
      CC=SQRT(DELX**2+DELY**2+DELZ**2)
      CAL=(DELX*A(I)+DELY*B(I)+DELZ*C(I))/CC
      CBE=(DELX*A(K)+DELY*B(K)+DELZ*C(K))/CC
      ADIS=.666667*CC/(1.+CAL)
      BDIS=.666667*CC/(1.+CBE)
      IF(ADIS.GT.BDIS)ADIS=BDIS*(2.-BDIS/ADIS)
      IF(BDIS.GT.ADIS)BDIS=ADIS*(2.-ADIS/BDIS)
      XQ=X(I)+A(I)*ADIS
      YQ=Y(I)+B(I)*ADIS
      ZQ=Z(I)+C(I)*ADIS
      XR=X(K)-A(K)*BDIS
      YR=Y(K)-B(K)*BDIS
      ZR=Z(K)-C(K)*BDIS
30    ITIM=0
      U=.5
40    C1=(1.-U)**2
      C2=(1.-U)*U*2.
      C3=U**2
      ITIM=ITIM+1
      IF(ITIM.GT.10)GOTO 60
      XA=C1*X(I)+C2*XQ+C3*XR
      YA=C1*Y(I)+C2*YQ+C3*YR
      ZA=C1*Z(I)+C2*ZQ+C3*ZR
C              PTB DELTAS
      XB=C1*XQ+C2*XR+C3*X(K)-XA
      YB=C1*YQ+C2*YR+C3*Y(K)-YA
      ZB=C1*ZQ+C2*ZR+C3*Z(K)-ZA
      DEN = (XB**2+YB**2+ZB**2)
      IF (DEN.EQ.0.) THEN
        CALL IGEROR(2)
        ERRFLG=.TRUE.
        GO TO 99
      ENDIF
      UERR=(XB*(X(J)-XA)+YB*(Y(J)-YA)+ZB*(Z(J)-ZA))/
     1 (DEN)-U
      UERR=UERR/3.
      IF(ABS(UERR).LT.1.E-5)GOTO 60
      IF(ITIM.EQ.1)GOTO 45
      DEN=OERR-UERR
      IF(ABS(DEN).LT.1.E-5)GOTO 45
      CTAN=DU/DEN
      IF(CTAN.LT..1)CTAN=1.
45    DU=UERR*CTAN
 
50    IF(DU+U.GT.1.)DU=1.-U
      IF(DU+U.LT.0.)DU=-U
      U=U+DU
      OERR=UERR
      GOTO 40
 
60    XC=XA+U*XB
      YC=YA+U*YB
      ZC=ZA+U*ZB
      IF(ITSK.EQ.1)GOTO 99
      DLX=XC-X(J)
      DLY=YC-Y(J)
      DLZ=ZC-Z(J)
      IF(ABS(DLX).GT.ABS(HDX))HDX=DLX
      IF(ABS(DLY).GT.ABS(HDY))HDY=DLY
      IF(ABS(DLZ).GT.ABS(HDZ))HDZ=DLZ
 
      J=J+1
      IF(J.LT.K)GOTO 30
C          ITSK 2.  PUT DELTAS IN XC,YC,ZC
      XC=HDX
      YC=HDY
      ZC=HDZ
 
99    RETURN
      END
 
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: CRVPRE                                         **
C **                                                                  **
C **                                                                  **
C ** COPYRIGHT (C) 1984 MILLS DATA SYSTEMS CORPORATION                **
C **                                                                  **
C **  PURPOSE OF PROGRAM: BUILDS THE CURVE SEGMENTS USING             **
C **         X,Y,Z,A,B,C ETC. DATA FROM SLPSET.                       **
C **                                                                  **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE CRVPRE (W,NWDS,NMP)

      INCLUDE 'iges.com'

      COMMON/PBLOK/X,Y,Z,A,B,C,DX,DY,DZ,CH,HX,HY,HZ,HA,HB,HC
C
      REAL*8 X(20),Y(20),Z(20)
      REAL*4 A(20),B(20),C(20),DX(20),DY(20),DZ(20),CH(20)
      REAL*8 HX(20),HY(20),HZ(20)
      REAL*4 HA(20),HB(20),HC(20)
C
      REAL*8 W(150)
      REAL*4 V4(2)
      REAL*8 V
      EQUIVALENCE (V,V4)

10    N=NMP
      M=N-1
      ARCSUM=0.
      DO 30 I=1,M
      J=I+1
      CAL=(A(I)*DX(I)+B(I)*DY(I)+C(I)*DZ(I))/CH(I)
      CBE=(A(J)*DX(I)+B(J)*DY(I)+C(J)*DZ(I))/CH(I)
      ADIS=.666667*CH(I)/(1.+CAL)
      BDIS=.666667*CH(I)/(1.+CBE)
C          SUPPRESS THE LARGER FOR MORE PLEASING APPEARANCE IN
C          EXTREME CASES ( WHICH ARE DOUBTFUL ANYWAY ).
      IF(ADIS.GT.BDIS)ADIS=BDIS*(2.-BDIS/ADIS)
      IF(BDIS.GT.ADIS)BDIS=ADIS*(2.-ADIS/BDIS)
C          CONVERT A,B,C TO PT'Q' DELTAS.
      A(I)=A(I)*ADIS
      B(I)=B(I)*ADIS
      C(I)=C(I)*ADIS
C          IF NOT SEG1, GO BACK AND PUT PREV RO IN CH.
      IF(I.EQ.1)GOTO 20
      CH(I-1)=OBDIS/ADIS
20    OBDIS=BDIS
C          DO PRELIMINARY S,DS/DU,DS/DU WORK
C          SET PT'R' DELTAS
      DXR=DX(I)-A(J)*BDIS
      DYR=DY(I)-B(J)*BDIS
      DZR=DZ(I)-C(J)*BDIS
C          SET B1,B2 DELTAS AT U=.5 ( ACTUALLY 4*SIZE )
      DXC=DXR+DX(I)-A(I)
      DYC=DYR+DY(I)-B(I)
      DZC=DZR+DZ(I)-C(I)
      CDIS=SQRT(DXC**2+DYC**2+DZC**2)
C          THIS ARCLENGTH IS APPROX. ( IMPROVE IN FUTURE? )
      RO=1.62*(ADIS+BDIS)/CDIS-.81
      ARCL=(.5-RO)*(ADIS+BDIS)+(.5+.5*RO)*CDIS
      ARCSUM=ARCSUM+ARCL
C          HOLD FULL SCALE S, DS/DU(0), DS/DU(1)
      DX(I)=ARCL
      DY(I)=3.*ADIS
      DZ(I)=3.*BDIS
30    CONTINUE
C          FINUP ON LAST SEG
      CH(M)=1.
      A(N)=A(N)*BDIS
      B(N)=B(N)*BDIS
      C(N)=C(N)*BDIS
C          POLGONS ARE DONE. DO FINAL WORK ON S, ETC.
      DO 40 I=1,M
      DX(I)=DX(I)/ARCSUM
      HDX=DX(I)
      IF(I.EQ.1)GOTO 35
      DX(I)=DX(I)+DX(I-1)
C          NOTE THE SWITCH FROM DS/DU TO DU/DS, CONV TO UNIT FORM,
C          AND DZ BECOMES 'B' IN U=A*V+B*V**2+C*V**3
35    DY(I)=HDX*ARCSUM/DY(I)
      DZ(I)=HDX*ARCSUM/DZ(I)
40    DZ(I)=3.-2.*DY(I)-DZ(I)
C          DN-SHIFT S-VALUES, PUT NMP IN DX(1) AND 1. IN DX(NMP)
      J=N+1
      DO 50 I=2,N
      J=J-1
50    DX(J)=DX(J-1)
      DX(1)=NMP
      DX(N)=1.

      DO 60 I=1,(NMP+1)/2
        V4(1)=DX(2*I-1)
        V4(2)=DX(2*I)           
        W(I)=V
60    CONTINUE
C            ADD SEGS 1 BY 1 STARTING AT IWX = (NMP+1)/2
      IWX=(NMP+1)/2
      DO 70 ISEG=1,NMP
        I=6*(ISEG-1)+IWX
        W(I+1)=X(ISEG)
        W(I+2)=Y(ISEG)
        W(I+3)=Z(ISEG)
        V4(1)=A(ISEG)
        V4(2)=B(ISEG)
        W(I+4)=V
        V4(1)=C(ISEG)
        V4(2)=DY(ISEG)
        W(I+5)=V
        V4(1)=DZ(ISEG)
        V4(2)=CH(ISEG)
        W(I+6)=V
70    CONTINUE
      NWDS = IWX+6*NMP
99    RETURN
      END
