C*********************************************************************
C*    NAME         :  dsbarl.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dsbarl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:58
C*
C*             THIS ROUTINE DOES DS WORK WHEN TOOL IS BARREL
C*                        PEM                  22-DEC-87
C*
C*********************************************************************
c
      SUBROUTINE DSBARL

      include 'com4a.com'
      include 'mocom.com'
 
      INTEGER*2 JD(600)
      REAL*4 AD(300),ASC(320)
      EQUIVALENCE (SC,ASC),(ASC(63),SBE),(ASC(64),CBE),(ASC(65),CUTN)
      EQUIVALENCE(D,AD,JD),(ITFL,IFL(49)),(IPTK,IFL(50))
      EQUIVALENCE(IFL(51),IA),(IFL(52),IB),(IFL(53),IC),(IFL(54),ISRF)
 
C          INIT  H, ITIM  AND CUTDIA/2
      H=T(21,IA)
      ITIM=0
      HDIA=TOOL(1)/2.
      IF(CBE.EQ.0.)CBE=1.
      TBE=-SBE/CBE
      SAL=ASC(305)
      CAL=SQRT(1.-SAL**2)
      TAL=-SAL/CAL
C          GET TA FROM T-TBL
      TI=T(4,IA)
      TJ=T(5,IA)
      TK=T(6,IA)
C                             IF TA/NORMAL,PS  USE SAME FOR IJK      14-DEC-82
      IF(IFL(23).NE.1)GOTO 10
      TI=S(1,1)
      TJ=S(2,1)
      TK=S(3,1)
C          EPT IS HDS ALONG TA FROM TE.
10    S(8,2)=T(1,IA)+T(4,IA)*H
      S(9,2)=T(2,IA)+T(5,IA)*H
      S(10,2)=T(3,IA)+T(6,IA)*H
      I=JD(201)
C          IF DS IS SF OR RLDSRF, GO SURFPN ROUTE.
      IF(I.EQ.9.OR.I.EQ.21.OR.I.EQ.25.OR.I.EQ.26.OR.I.EQ.27)GOTO 50
C          DSIS LN, CI, OR CV.  ERROR IF TL/LFT,RGT 
      IF(IFL(21).EQ.0)GOTO 20
      IFL(2)=28
      GOTO 99
C          BRA ON LN OR CV/CI
20    IF(JD(201).NE.5)GOTO 30
C
C          LINE. USE LINE PT-DELTAS FOR P-V
      PX=D(53)
      PY=D(54)
      PZ=D(55)
      VX=D(56)
      VY=D(57)
      VZ=D(58)
      GOTO 40
C          CV OR CI    ( EPT 1ST-TRY IS READY IN S-TBL)
30    IF(JD(201).EQ.8)GOTO 35
      IF(JD(201).EQ.7)GOTO 32
C          ERROR. INVALID DS TYPE.
      IFL(2)=28
      GOTO 99
32    CALL CIRCPV(PX,PY,PZ,VX,VY,VZ)
      GOTO 40
35    CALL CURVPV(PX,PY,PZ,VX,VY,VZ)
C          DSN IS CROSSF V & TA
40    RX=VY*TK-VZ*TJ
      RY=VZ*TI-VX*TK
      RZ=VX*TJ-VY*TI
      SEC=SQRT(RX**2+RY**2+RZ**2)
C          IF SEC ZERO, SOME SPECIAL CASE EXISTS.   TA//V?
      IF(SEC.GT.0.)GOTO 42
      IFL(2)=129
      GOTO 99
C          IF V-VEC OPPOSES OLD FWD, FLIP THIS RGT SENSE
42    IF(VX*T(7,IC)+VY*T(8,IC)+VZ*T(9,IC).LT.0.)SEC=-SEC
C          LOAD DSPL IN S-TBL.
      S(1,2)=RX/SEC
      S(2,2)=RY/SEC
      S(3,2)=RZ/SEC
      S(4,2)=PX*S(1,2)+PY*S(2,2)+PZ*S(3,2)
      GOTO 99
C          DS IS SURF OR RLDSRF
50    ITIM=ITIM+1
      U=T(19,IA)
      V=T(20,IA)
      CALL SURFPN(U,V,1)
C RLS - 03/27
      if (ifl(2) .eq. 466) go to 99
C RLS - END
      T(19,IA)=U
      T(20,IA)=V
C          CALC HDS IF TLLFT OR TLRGT
      IF(IFL(57).EQ.0)GOTO 99
C          IF O/B CASE, CALC DELTAS TO SFPNT PER TANPL EQ
      IF(U.GT.0..AND.U.LT.1..AND.V.GT.0..AND.V.LT.1.) GOTO 53
      DIS=S(1,2)*S(8,2)+S(2,2)*S(9,2)+S(3,2)*S(10,2)-S(4,2)
      DX=S(8,2)-S(1,2)*DIS-T(1,IA)
      DY=S(9,2)-S(2,2)*DIS-T(2,IA)
      DZ=S(10,2)-S(3,2)*DIS-T(3,IA)
      GOTO 54
C          DELTAS TO SFPNT FROM TE
53    DX=S(5,2)-T(1,IA)
      DY=S(6,2)-T(2,IA)
      DZ=S(7,2)-T(3,IA)
C          SFPNT IN TOOLSYS
54    ZT=TI*DX+TJ*DY+TK*DZ
C                                CHECK FOR NEG XT   29-MAY-85
      XT=DX**2+DY**2+DZ**2-ZT**2
      IF(XT.LT.0.)GOTO 98
      XT=SQRT(XT)
      TN=0.
      IF(XT.GT.0.)TN=(ZT-H)/XT
C          BRA ON ITIM
      IF(ITIM-2)55,60,90
C              ****  TIME 1
55    X1=XT
      Z1=ZT
      H1=H
      T1=TN
C          CHOOSE BETWEEN RCORN, R2, AND TOP INDICATED CONTACT
      IF(T1.GT.TBE) GOTO 56
C             RCORN CASE
      H=TOOL(2)-T1*TOOL(6)
      GOTO 58
56    IF(T1.GT.TAL)GOTO 57
C             R2 CASE         H=Z2-X2*TN
      H=ASC(68)-ASC(67)*T1
      GOTO 58
C             TOP CASE
57    H=TOOL(3)-T1*TOOL(1)*.5
      IFL(57)=3
C          IF H-CHG SMALL AND DS SEEMS CONVEX, EXIT
58    IF(ABS(H-H1).LT..01.AND.IFL(27).EQ.0)GOTO 90
      IF(ABS(H-H1).LT..01)H=H1+.01
      GOTO 10
C              ****  TIME 2
60    H2=H
C          CALC APPARENT R CTR
      DEN=T1-TN
C          IF NO REAL ANGLE CHG, ASSUME FLAT CASE.
      IF(ABS(DEN).LT.1.E-5)GOTO 90
      XC=(H2-H1)/DEN
      ZC=H1+T1*XC
C          CONVEX OR CONCAVE
      IF(XC.LT.XT) GOTO 75
C             CONVEX. SEE IF RCORN CASE OR OTHER.
      IFL(27)=0
      ZCN=SBE*(XC-TOOL(6))+CBE*(ZC-TOOL(2))
      IF(ZCN.GT.0.)GOTO 63
C             RCORN
      DEN=XC-TOOL(6)
      IF(DEN.EQ.0.)GOTO 98
      TN=(TOOL(2)-ZC)/DEN
      H=TOOL(2)+TN*TOOL(6)
      IFL(57)=1
      GOTO 80
C             SEE IF R2 CASE                   27-JAN-88
63    ZCN=SAL*(XC-ASC(67))+CAL*(ZC-ASC(68))
      IF(ZCN.GT.0.)GOTO 64
C             R2 CASE.
      DEN=XC-ASC(67)
      IF(DEN.EQ.0.)GOTO 98
      TN=(ZC-ASC(68))/DEN
      H=ASC(68)-TN*ASC(67)
      GOTO 80
C             SIDE OR TOP
64    ZCN=SAL*(XC-HDIA)+CAL*(ZC-TOOL(3))
      IF(ZCN.GE.0.)GOTO 68
C             SIDE CASE
      H=ZC-TAL*XC
      GOTO 80
C             TOP CASE
68    DEN=XC-HDIA
      IF(DEN.EQ.0.)GOTO 98
      TN=(ZC-TOOL(3))/DEN
      H=TOOL(3)-TN*HDIA
      GOTO 80
C             CONCAVE. DECIDE IF RCORN, R2, OR TOP CONTACT
75    RTOP=SQRT((HDIA-XC)**2+(TOOL(3)-ZC)**2)
      RSID=SQRT((ASC(67)-XC)**2+(ASC(68)-ZC)**2)+ASC(307)
      RBOT=SQRT((TOOL(6)-XC)**2+(ZC-TOOL(2))**2)+TOOL(2)
C             ELIM R1 OR R2 BY CTRPT VS. BETA LINE
      ZCN=SBE*(XC-TOOL(6))+CBE*(ZC-TOOL(2))      
      IF(ZCN.GE.0.)RSID=0.
      IF(ZCN.LT.0.)RBOT=0.
      IFL(27)=1
C             IF R2 IS LARGEST, DO SIDE CASE H         27-JAN-88
      IF(RTOP.GT.RSID.OR.RBOT.GT.RSID)GOTO 755
      DEN=ASC(67)-XC
      TN=0.
      IF(DEN.NE.0.)TN=(ASC(68)-ZC)/DEN
      H=ASC(68)-TN*ASC(67)
      GOTO 80
755   IF(RBOT.GE.RTOP)GOTO 77
C             TOP CASE
76    DEN=HDIA-XC
      IF(DEN.EQ.0.)GOTO 98
      TN=(TOOL(3)-ZC)/DEN
      H=ZC-TN*XC
      IFL(57)=3
      GOTO 80
C             BOT CASE
77    DX=TOOL(6)-XC
      IF(DX.EQ.0.)GOTO 98
      TN=(ZC-TOOL(2))/DX
      H=ZC+TN*XC
      IFL(57)=1
C          IF H-CHG REAL, GO AGAIN
80    IF(ABS(H-H2).GT..01)GOTO 10
C
C          RECORD THIS H-VAL IN T(21)
90    T(21,IA)=H
      GOTO 99
C          ERROR EXIT DUE TO ZERO DIVIDE.        15-JUN-84
98    IFL(2)=163
99    RETURN
      END
