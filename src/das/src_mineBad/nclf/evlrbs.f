c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       evlrbs.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:02
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1991 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **  SUBROUTINE NAME: EVRBSF
C **
C **  LAST REVISION:
C **
C **  PURPOSE OF SUBROUTINE: TO EVALUATE A RATIONAL BSPLINE SURFACE.
C **
C **    INPUT ARGUMENTS -
C **        M1       - DEGREE OF SURFACE IN U DIRECTION
C **        M2       - DEGREE OF SURFACE IN V DIRECTION
C **        NK1      - NUMBER OF KNOT VALUES IN U DIRECTION
C **        NK2      - NUMBER OF KNOT VALUES IN V DIRECTION
C **        T1       - KNOT VALUES IN U DIRECTION
C **        T2       - KNOT VALUES IN V DIRECTION
C **        WT       - WEIGHT VALUES
C **        C        - COEFFICIENTS
C **        U        - PARAMETER TO EVALUATE (0 - 1)
C **        V        - PARAMETER TO EVALUATE (0 - 1)
C **        ND       - HIGHEST DERIVATIVE DESIRED (1ST MAX)
C **
C **    OUTPUT ARGUMENT -
C **        SV(1-3)   - POINT AT U,V
C **        SV(4-6)   - 1ST DERIVATIVE IN U
C **        SV(7-9)   - 1ST DERIVATIVE IN V
C **
C **********************************************************************
      SUBROUTINE EVRBSF(M1,M2,NK1,NK2,T1,T2,WT,C,U,V,ND,SV)

      INTEGER*2 MAXDRV, MAXORD
      PARAMETER (MAXDRV=3)
      PARAMETER (MAXORD=25)

      INTEGER*2 M1,M2,NK1,NK2,ND
      REAL*8 T1(8),T2(8),WT(8),C(48),U,V,SV(9)

      REAL*8 VT,Q(MAXORD*MAXDRV*3),Q1(MAXDRV*4),WL(MAXDRV*MAXORD),WTMP
      REAL*8 S0, S1
      INTEGER*2 I,J,K,IK2
      INTEGER*4 IXC,IXCH, IXW,IXWH

      VT=(T2(NK2-M2+1)-T2(M2+1))*V+T2(M2+1)

      DO 10 I=NK2-M2,M2+2,-1
10    IF (T2(I).LT.T2(I+1) .AND. T2(I).LE.VT .AND. VT.LE.T2(I+1))
     1     GOTO 20

20    IK2=I-M2
      IF (IK2.LT.1) IK2 = 1
      IXWH = NK1-M1
      IXCH = IXWH*3

      DO 30 I=IK2,IK2+M2
      IXW = (I-1)*IXWH+1
      IXC = (I-1)*IXCH+1
      S0 = T1(1)
      S1 = T1(NK1)
      CALL EVRBSP(M1,NK1,T1,WT(IXW),C(IXC),U,ND,S0,S1,Q1,WL(I-IK2+1))
      J=(I-IK2)*3
      DO 30 K=0,ND*3,3
      Q(J+(M2+1)*K+1)=Q1(K+1)
      Q(J+(M2+1)*K+2)=Q1(K+2)
      Q(J+(M2+1)*K+3)=Q1(K+3)
30    CONTINUE

      CALL RBSVAL(M2,T2(IK2),WL,Q,VT,ND,Q1,WTMP)
      DO 40 K=0,ND
      SV(K*6+1)=Q1(K*3+1)
      SV(K*6+2)=Q1(K*3+2)
      SV(K*6+3)=Q1(K*3+3)
40    CONTINUE

      IF (ND.EQ.0) GOTO 999

      J=0
      DO 50 I=1,ND
      CALL RBSVAL(M2,T2(IK2),WL,Q((M2+1)*I*3+1),VT,J,Q1,WTMP)
      DO 50 K=1,3
      SV(I*6+K-3)=Q1(K)
50    CONTINUE

999   RETURN
      END
C **********************************************************************
C **  SUBROUTINE NAME: EVRBSP
C **
C **  LAST REVISION:
C **
C **  PURPOSE OF SUBROUTINE: TO EVALUATE A RATIONAL BSPLINE. CONVERT U BASED
C **    ON KNOT VALUES, CHOOSE SPAN AND CALL RBSVAL
C **
C **    INPUT ARGUMENTS -
C **        M        - DEGREE OF BSPLINE
C **        NK       - NUMBER OF KNOT VALUES
C **        T        - KNOT VALUES
C **        WT       - WEIGHT VALUES
C **        C        - COEFFICIENTS
C **        U        - PARAMETER TO EVALUATE (0 - 1)
C **        ND       - HIGHEST DERIVATIVE DESIRED (1ST MAX)
C **
C **    OUTPUT ARGUMENT -
C **        Q(1-3)   - POINT AT U
C **        Q(4-6)   - 1ST DERIVATIVE AT U
C **        WR       - WEIGHT AT EVALUATED POINT
C **
C **********************************************************************
      SUBROUTINE EVRBSP(M,NK,T,WT,C,U,ND,T0,T1,Q,WR)

      INTEGER*2 M,NK,ND
      REAL*8 T(3),WT(3),C(3),U,T0,T1,Q(3),WR

      REAL*8 UT
      INTEGER*2 I

      UT=(T(NK)-T(1))*U+T(1)
      UT=(T(NK-M+1)-T(M+1))*U+T(M+1)
      UT=(T1-T0)*U+T0

C      DO 10 I=NK-1,M+2,-1
C10    IF (T(I).LT.T(I+1) .AND. T(I).LE.UT .AND. UT.LE.T(I+1)) GOTO 20
      DO 10 I=NK-M,M+2,-1
10    IF (T(I).LT.T(I+1) .AND. T(I).LE.UT) GOTO 20

20    I=I-M
      IF (I.LT.1) I=1
      CALL RBSVAL(M,T(I),WT(I),C(I*3-2),UT,ND,Q,WR)

999   RETURN
      END
C **********************************************************************
C **  SUBROUTINE NAME: RBSVAL
C **
C **  LAST REVISION:
C **
C **  PURPOSE OF SUBROUTINE: TO EVALUATE 1 SPAN OF A RATIONAL BSPLINE. 
C **    MODELED AFTER PASCAL CODE BY RICHARD D. FUHR OF BOEING
C **    AND UNICAD RBS EVALUATOR.
C **
C **    INPUT ARGUMENTS -
C **        M        - DEGREE OF BSPLINE
C **        T        - KNOT VALUES OF THIS SPAN
C **        WT       - WEIGHT VALUES OF THIS SPAN
C **        C        - COEFFICIENTS OF THIS SPAN
C **        U        - PARAMETER TO EVALUATE
C **        ND       - HIGHEST DERIVATIVE DESIRED (MAX 1ST)
C **
C **    OUTPUT ARGUMENT -
C **        Q(1-3)   - POINT AT U
C **        Q(4-6)   - 1ST DERIVATIVE AT U
C **        WR       - WEIGHT AT EVALUATED POINT
C **
C **********************************************************************
      SUBROUTINE RBSVAL(M,T,WT,C,U,ND,Q,WR)

      INTEGER*2 MAXKNT, MAXORD
      PARAMETER (MAXORD = 25)
      PARAMETER (MAXKNT = 7)

      INTEGER*2 M,ND
      REAL*8 T(MAXKNT),WT(MAXKNT),C(MAXKNT*3),U,Q(12),WR

      REAL*8 A(MAXORD),B(MAXORD),Q1(MAXORD*4),A1,B1,AB
      INTEGER*2 I,J,K

      IF (M.EQ.0) THEN
        Q(1)=C(1)*WT(1)
        Q(2)=C(2)*WT(2)
        Q(3)=C(3)*WT(3)
        GOTO 999
      ENDIF

      DO 10 I=1,M
      A(I)=U-T(I+1)
10    B(I)=T(M+I+1)-U

      IF (B(1).LT.A(M)) GOTO 200

      DO 120 I=0,M
      Q1(I*4+1)=C(I*3+1)*WT(I+1)
      Q1(I*4+2)=C(I*3+2)*WT(I+1)
      Q1(I*4+3)=C(I*3+3)*WT(I+1)
120   Q1(I*4+4)=WT(I+1)

      DO 130 J=1,M
      DO 130 I=0,M-J
      A1=A(I+J)
      B1=B(I+1)
      AB=A1+B1
      DO 130 K=I*4+1,I*4+4
      Q1(K)=(A1*Q1(K+4)+B1*Q1(K))/AB
130   CONTINUE

C      DO 140 J=1,ND
C      DO 140 I=ND,J,-1
C      B1=B(I-J+1)*(M-J+1)
C      DO 140 K=I*4+1,I*4+4
C      Q1(K)=(Q1(K)-Q1(K-4))/B1
C140   CONTINUE
      A1=M/B(1)
      GOTO 500

200   CONTINUE

      DO 220 I=0,M
      Q1(I*4+1)=C(M*3-I*3+1)*WT(I+1)
      Q1(I*4+2)=C(M*3-I*3+2)*WT(I+1)
      Q1(I*4+3)=C(M*3-I*3+3)*WT(I+1)
220   Q1(I*4+4)=WT(I+1)

      DO 230 J=1,M
      DO 230 I=0,M-J
      A1=A(M-I)
      B1=B(M-J+1-I)
      AB=A1+B1
      DO 230 K=I*4+1,I*4+4
      Q1(K)=(B1*Q1(K+4)+A1*Q1(K))/AB
230   CONTINUE

C      DO 240 J=1,ND
C      DO 240 I=ND,J,-1
C      A1=A(M-I+J)*(M-J+1)
C      DO 240 K=I*4+1,I*4+4
C      Q1(K)=(Q1(K-4)-Q1(K))/A1
C240   CONTINUE
      A1=-M/A(M)

500   CONTINUE
      DO 600 I=1,3
600   Q(I)=Q1(I)/Q1(4)
      WR=Q1(4)

      IF (ND.LT.1) GOTO 999

      A1=A1*Q1(8)/Q1(4)
      DO 700 I=1,3
700   Q(I+3)=A1*(Q1(I+4)/Q1(8)-Q(I))

999   RETURN
      END
