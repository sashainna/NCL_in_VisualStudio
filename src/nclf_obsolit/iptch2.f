c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       iptch2.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:12
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1981,1982 MILLS DATA SYSTEMS CORPORATION
C**
C
C       CHG  16 NOV 82    FIX STALL-OUT LOGIC AFTER O/B RECOGNIZED.
C       CHG   7-DEC-82    ADD CALLS TO DEBUGI
C
C          APPLIES THE EPT IN SRF(8-10) TO THE PATCH IN D(ISRF)
C          AND LOADS SRF P/N IN SRF(1-7).
C
C          U,V     INPUT:   1ST TRY LOC ON PATCH.
C                  OUTPUT:  LAST FOUND VALUES.
C
C          IRET =  0   ALL OK. INBOUNDS AND GOOD SOLUTION.
C               = -1   U(0) BDY VIOLATION -- THIS PATCH
C               = +1   U(1)  "     "
C               = -2   V(0) BDY VIOLATION -- THIS PANEL
C               = +2   V(1)  "     "
C               = +3   APPLICATION FAILED.
 
      SUBROUTINE IPTCH2(SFU,SFV,IRET)

      INCLUDE 'com4a.com'
      include 'mocom.com'
 
C          MOTION COMMON EQUIVALENCES
      real*4 srf(10,3)
      equivalence (srf,s)
 
      REAL*8 XO,YO,ZO
      REAL*4 AD(300),CO(11),RX(4),RY(4),RZ(4),X(4),Y(4),Z(4)
      REAL*4 QX(10),QY(10),QZ(10)
      INTEGER*2 KD(600)
      EQUIVALENCE (D,AD,KD),(IFL(54),ISRF)

C          INIT FOR CONVERGE STEPS.  ZERO IRET.
      IDX=50*(ISRF-1)
      JDX=2*IDX
      KDX=4*IDX
      ISTYP=KD(KDX+1)
      J=JDX
      IMID=0
      ITIM=0
      IRET=0
      UTAN=1.
      VTAN=1.
      den = 3.
C      IF(ISTYP.NE.25)GOTO 4
C      U=SFU
C      V=SFV
C      XO=AD(JDX+23)
C      YO=AD(JDX+24)
C      ZO=AD(JDX+25)
C      GOTO 6

CCC++                 GUARD AGAINST ZERO DIVIDE    9-3-82
C              CONVERT SFU,SFV TO PATCH U,V
4     U=SFU
      V=SFV
      IF (SFU.EQ..5.AND.SFV.EQ..5) GOTO 5
      UDEN= AD(JDX+16)-AD(JDX+15)
      VDEN= AD(JDX+18)-AD(JDX+17)
      IF (UDEN.EQ.0..OR.VDEN.EQ.0.)GOTO 91
      U=(SFU-AD(JDX+15))/UDEN
      V=(SFV-AD(JDX+17))/VDEN
5     XO=D(IDX+23)
      YO=D(IDX+24)
      ZO=D(IDX+25)
CCC++                  END OF CHG     9-3-82
C          DO NOT ALLOW O/B U,V
6     IF(U.LT.0.)U=0.
      IF(U.GT.1.)U=1.
      IF(V.GT.1.)V=1.
      IF(V.LT.0.)V=0.
C          DELTA THE EPT FROM PATCH ORIGIN
      TX=SRF(8,ISRF)-XO
      TY=SRF(9,ISRF)-YO
      TZ=SRF(10,ISRF)-ZO

C          SOLVE THIS U,V POSITION AND UERR, VERR
C           RPTS CALC DIFFERENTLY FOR DIFFERENT PATCH TYPES.
10    IF(ISTYP.NE.25) GOTO 14
C
C             PREP 4 RPTS FOR ROCKWELL 25PT PATCH
      VM=1.-V
      C1=VM**3
      C2=3.*V*VM**2
      C3=3.*V**2*VM
      C4=V**3
C             V-INTERP FOR 10 Q-PTS
      DO 11 I=1,10
      J=JDX+20+5*I
      IF(I.GT.5) J=J-24
      QX(I)=C1*AD(J+1)+C2*AD(J+2)+C3*AD(J+3)+C4*AD(J+4)
      J=J+25
      QY(I)=C1*AD(J+1)+C2*AD(J+2)+C3*AD(J+3)+C4*AD(J+4)
      J=J+25
11    QZ(I)=C1*AD(J+1)+C2*AD(J+2)+C3*AD(J+3)+C4*AD(J+4)
C             U-INTERP OVER Q FOR 4 RPTS
      UM=1.-U
      C1=UM**3
      C2=3.*U*UM**2
      C3=3.*U**2*UM
      C4=U**3
      DO 12 I=1,4
      J=5*I-5
      IF(I.GT.2)J=J-9
      RX(I)=C1*QX(J+1)+C2*QX(J+2)+C3*QX(J+3)+C4*QX(J+4)
      RY(I)=C1*QY(J+1)+C2*QY(J+2)+C3*QY(J+3)+C4*QY(J+4)
12    RZ(I)=C1*QZ(J+1)+C2*QZ(J+2)+C3*QZ(J+3)+C4*QZ(J+4)
      GOTO 32

14    IF(ISTYP.NE.21)GOTO 18
C*****************************   RLD PAT
      B1=(1.-U)**2
      B2=2.*U*(1.-U)
      B3=U**2
      DO 16 I=1,4
      J=49+JDX+3*I
      RX(I)=B1*AD(J)+B2*AD(J+6)+B3*AD(J+12)
      RY(I)=B1*AD(J+1)+B2*AD(J+7)+B3*AD(J+13)
16    RZ(I)=B1*AD(J+2)+B2*AD(J+8)+B3*AD(J+14)
      GOTO 32
C**********************************   FULL PAT
18    A=(1.-U)*(1.-V)
      B=(1.-U)*V
      C=U*(1.-V)
      E=U*V
      CO(1)=A**2
      CO(2)=2.*A*B
      CO(3)=B**2
      CO(4)=0.
      CO(5)=2.*A*C
      CO(6)=2.*(B*C+A*E)
      CO(7)=2.*B*E
      CO(8)=0.
      CO(9)=C**2
      CO(10)=2.*C*E
      CO(11)=E**2
C          DO 4 RPTS
      DO 30 I=1,4
      RX(I)=0.
      RY(I)=0.
      RZ(I)=0.
C          SET P-PTR
      IP=J+46+3*I
      IF(I.GT.2)IP=IP+6
C          DO THE 11 SUM
      DO 20 K=1,11
      IP=IP+3
      RX(I)=RX(I)+CO(K)*AD(IP)
      RY(I)=RY(I)+CO(K)*AD(IP+1)
20    RZ(I)=RZ(I)+CO(K)*AD(IP+2)
30    CONTINUE
C*****************************************
C          4 INT PTS  ( 2 & 4 ARE ACTUALLY DELTAS )
32    X(1)=RX(1)+V*(RX(2)-RX(1))
      Y(1)=RY(1)+V*(RY(2)-RY(1))
      Z(1)=RZ(1)+V*(RZ(2)-RZ(1))
      X(2)=RX(3)+V*(RX(4)-RX(3))-X(1)
      Y(2)=RY(3)+V*(RY(4)-RY(3))-Y(1)
      Z(2)=RZ(3)+V*(RZ(4)-RZ(3))-Z(1)
      X(3)=RX(1)+U*(RX(3)-RX(1))
      Y(3)=RY(1)+U*(RY(3)-RY(1))
      Z(3)=RZ(1)+U*(RZ(3)-RZ(1))
      X(4)=RX(2)+U*(RX(4)-RX(2))-X(3)
      Y(4)=RY(2)+U*(RY(4)-RY(2))-Y(3)
      Z(4)=RZ(2)+U*(RZ(4)-RZ(2))-Z(3)
C
      XN=Y(2)*Z(4)-Z(2)*Y(4)
      YN=Z(2)*X(4)-X(2)*Z(4)
      ZN=X(2)*Y(4)-Y(2)*X(4)
C----------
C      IF(IFL(85).LT.0)CALLDEBUGI(1,ITIM)     
C----------
C
      XU=Y(4)*ZN-Z(4)*YN
      YU=Z(4)*XN-X(4)*ZN
      ZU=X(4)*YN-Y(4)*XN
C
      XV=Z(2)*YN-Y(2)*ZN
      YV=X(2)*ZN-Z(2)*XN
      ZV=Y(2)*XN-X(2)*YN
CCC--          GUARD AGAINST ZERO DIVIDE    9-3-82
      UDEN= XU*X(2)+YU*Y(2)+ZU*Z(2)
      VDEN= XV*X(4)+YV*Y(4)+ZV*Z(4)
      IF(UDEN.EQ.0..OR.VDEN.EQ.0.)GOTO 91
      UERR=(XU*(TX-X(1))+YU*(TY-Y(1))+ZU*(TZ-Z(1))) /UDEN-U
      VERR=(XV*(TX-X(3))+YV*(TY-Y(3))+ZV*(TZ-Z(3))) /VDEN-V
CCC--              END OF CHG  9-3-82
      UERR=UERR/den
      IF(ISTYP.NE.21)VERR=VERR/den
      IF(ISTYP.NE.25) GOTO 38
      UERR=UERR*.75
      VERR=VERR*.75
38    CONTINUE
      IF(ITIM.GT.0)GOTO 40
      DU=UERR
      DV=VERR
      GOTO 50
40    UDEN=OUERR-UERR
      IF(ABS(UDEN).LT.1.E-5)GOTO 42
      UTAN=DU/UDEN
C          NEG TAN IS UNREAL
      IF(UTAN.LE.0.)UTAN=1.
42    DU=UERR*UTAN
      VDEN=OVERR-VERR
      IF(ABS(VDEN).LT.1.E-5)GOTO 44
      VTAN=DV/VDEN
C          SAME FOR VTAN
      IF(VTAN.LE.0.)VTAN=1.
44    DV=VERR*VTAN
50    HU=U+UERR
      HV=V+VERR
      IF(U+DU.LT.0.)DU=-U
      IF(U+DU.GT.1.)DU=1.-U
      IF(V+DV.LT.0.)DV=-V
      IF(V+DV.GT.1.)DV=1.-V
      U=U+DU
      V=V+DV
      IF(ABS(UERR).LT..0001.AND.ABS(VERR).LT..0001)GOTO 90
      ITIM=ITIM+1
      OUERR=UERR
      OVERR=VERR
      IF(ITIM.GT.120)GOTO 91
C          IF UV CHG REAL, GO AGAIN.
      IF(ABS(DU).GT.1.E-5.OR.ABS(DV).GT.1.E-5)GOTO 10
C
C          STALL-OUT.  (NO REAL CHG IN U,V)
C            SCAN PATCHES UNTIL BEST FOUND FOR CURRENT PANEL.
C            CHG PANELS ONLY AS INDICATED AFTER ABOVE SEARCH.
C
C            IF 25PT PATCH, SET IRET AND EXIT WITH TANPL
C              (ALLOW .002 U,V EXCESS WITHOUT SETTING IRET)
      UOFFM=ABS(HU-.5)
      VOFFM=ABS(HV-.5)
C      IF(ISTYP.NE.25) GOTO 70
      IF(UOFFM.LT.VOFFM)GOTO 65
C              IT IS APPARENTLY A U-VIOLATION
      IF(HU.LT.-.002) IRET=-1
      IF(HU.GT.1.002) IRET=1
      GOTO 90
C               APPARENTLY A V-VIOLATION
65    IF(HV.LT.-.002) IRET=-2
      IF(HV.GT.1.002) IRET=2
      GOTO 90

C70    NPATS=KD(KDX+38)
C      IPAT=KD(KDX+41)
C      IF(UOFFM.LT.VOFFM)GOTO 74
C      IF(HU.GE.-.002)GOTO 72
CC      IF(IPAT.LT.2)GOTO 74
C      IRET=-1
C      GOTO 99
C72    IF(HU.LE.1.002)GOTO 74
CC      IF(IPAT.GT.NPATS-1)GOTO 74
C      IRET=1
C      GOTO 99
C74    IF(HV.LT.-.002)IRET=-2
C      IF(HV.GT.1.002)IRET=2
C
C          CALC TANPL
90    A=XN
      B=YN
      C=ZN
      SEC=SQRT(A**2+B**2+C**2)*AD(JDX+2)
      IF(ABS(SEC).GT.1.E-6)GOTO 92

C          SOLUTION FAILED.  RE-TRY FROM MIDPT IF FEASIBLE.  15-NOV-84
91    IF(IMID.gt.1.OR.ITIM.EQ.0) GOTO 915
      IMID=imid+1
      if (imid.gt.1) den = 6.
      ITIM=0
      U=.5
      V=.5
      GOTO 10
C             COULD NOT RE-TRY.  ERROR
915   IRET=3
      IFL(2)=128
      GOTO 99
C          NORMAL EXIT.  LOAD VEC IN SRF(1-3)
92    SRF(1,ISRF)=A/SEC
      SRF(2,ISRF)=B/SEC
      SRF(3,ISRF)=C/SEC
C          SRF PT
      SRF(5,ISRF)=X(1)+X(2)*U+XO
      SRF(6,ISRF)=Y(1)+Y(2)*U+YO
      SRF(7,ISRF)=Z(1)+Z(2)*U+ZO
C                  IF OFFSET SURFACE, CHG SRF PT         20-JAN-87
      IF(KD(KDX+5).NE.7)GOTO 93
      DIS=AD(JDX+2)*AD(JDX+4)
      SRF(5,ISRF)=SRF(5,ISRF)+SRF(1,ISRF)*DIS
      SRF(6,ISRF)=SRF(6,ISRF)+SRF(2,ISRF)*DIS
      SRF(7,ISRF)=SRF(7,ISRF)+SRF(3,ISRF)*DIS
93    CONTINUE
C          TANPL CONST
      SRF(4,ISRF)=SRF(1,ISRF)*SRF(5,ISRF)+SRF(2,ISRF)*SRF(6,ISRF)+
     1 SRF(3,ISRF)*SRF(7,ISRF)
C          CONVERT BACK TO SFU,SFV
      IF(ISTYP.NE.25) GOTO 94
      SFU=U
      SFV=V
      GOTO 99
94    SFU=AD(JDX+15)+U*(AD(JDX+16)-AD(JDX+15))
      SFV=AD(JDX+17)+V*(AD(JDX+18)-AD(JDX+17))
C           IF DS AND TA 'PARELM', ADD V-VEC TO TCOL(22-24)      23-JUL-86
99    IF(ISRF.NE.2.OR.IFL(23).NE.6)GOTO 999
      IA=IFL(51)
      if (rldinu) then
        t(22,ia)=x(2)
        t(23,ia)=y(2)
        t(24,ia)=z(2)
      else
        t(22,ia)=x(4)
        t(23,ia)=y(4)
        t(24,ia)=z(4)
      endif
999   RETURN
      END
