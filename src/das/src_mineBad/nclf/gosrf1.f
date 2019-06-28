C*********************************************************************
C*    NAME         :  gosrf1.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       gosrf1.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:08
C********************************************************************
C                                                                   *
C            WRITTEN:  14-OCT-87    PEM                             *
C                                                                   *
C            THIS ROUTINE IS CALLED BY STRTUP WHEN GO/SF1 CMD       *
C            IS GIVEN.  BUILD SC(10) TBL TO LOOK LIKE 3SRF GO       *
C            AND CONTINUE AS IS.                                    *
C                                                                   *
C            NOTE:  NOPS AND/OR INDIRV AFFECT THIS TASK             *
C                                                                   *
C********************************************************************

      subroutine gosrf1
 
      include 'com4a.com'
      include 'mocom.com'
C
      INTEGER*2 JD(600),KGO(12),KSC(100)
      EQUIVALENCE (HGO(7),KGO),(SC,KSC)
      REAL*4 AD(300)
      EQUIVALENCE(D,AD,JD),(ITFL,IFL(49)),(IPTK,IFL(50)),(NTK,IFL(79))
      EQUIVALENCE(IFL(51),IA),(IFL(52),IB),(IFL(53),IC),(IFL(54),ISRF)
C

C              IF INDIRV FLG ON, USE FWD TO SET 'DSLN'
      IF(IFL(22).EQ.0) GOTO 20
      DSA=SC(7)
      DSB=SC(8)
      DSC=SC(9)
      GOTO 30
C            INDIRV IS OFF.  BUILD A GF/CMD TO PREMOV TO LOAD SF1 IN DS-TBL
C                 (SF1 IS ACTUALLY CS BUT ONLY DS IS LOADED BY PREMOV)
20    KSC(37)=704
      SC(11)=HGO(12)
      SC(12)=71.
C                 DUMMY SC(13) TO SEEM AN ALREADY LOADED PL CS TO PREMOV
      SC(13)=0.
      KSC(52)=6
C               IF NO FWD SENSE YET, FORCE ONE
      IF(SC(7)**2+SC(8)**2+SC(9)**2.GT.0.) GOTO 22
      SC(7)=.6
      SC(8)=.8
22    CALL PREMOV
C               IF SF1 IS PLANE PERPTO TA, CHANGE THE SC(10) COMMAND
C               TO BE A GD/DIS  AND EXIT.                  22-FEB-88
      IF(JD(201).NE.6)GOTO 23
      CO=S(1,2)*SC(4)+S(2,2)*SC(5)+S(3,2)*SC(6)
      IF(ABS(CO).LT..9999995)GOTO 23
      KSC(37)=710
      KSC(38)=2
      SC(11)=S(4,2)-S(1,2)*SC(1)-S(2,2)*SC(2)-S(3,2)*SC(3)
      IF(CO.LT.0.)SC(11)=-SC(11)
      if (sc(24).eq.0.)goto 99
c                                              Apply thick
      if (sc(11).ge.0.)sc(11)=sc(11)-sc(24)
      if (sc(11).lt.0.)sc(11)=sc(11)+sc(24)
      GOTO 99

C                 SPECIAL HANDLING IF SF1 IS LN OR CIRCLE
23    IF(JD(201).NE.5)GOTO 24
C                 SF1 IS LINE.  DSLN IS PERPTO SAME IN XYPLAN
      DSA=-D(57)
      DSB=D(56)
      DSC=0.
      DX=D(53)-SC(1)
      DY=D(54)-SC(2)
      DZ=0.
      DIREC=1.
      GOTO 28

24    IF(JD(201).NE.7)GOTO 26
C                 SF1 IS CIRCLE.   DSLN IS FROM TE TO NRPT ON CIRCUMF.
      DX=D(53)-SC(1)
      DY=D(54)-SC(2)
      TD=SQRT(DX**2+DY**2)
      IF(TD.LT..0001)GOTO 34
      RO=(TD-D(59))/TD
      DSA=DX*RO
      DSB=DY*RO
      DSC=0.
      DIREC=1.
      GOTO 29

C                 SFNRM IS 'DSLN'
26    DSA=S(1,2)
      DSB=S(2,2)
      DSC=S(3,2)
C                 SET FWD FROM TE TO DSPT ALONG DSLN
      DIREC=1.
      DX=S(5,2)-SC(1)
      DY=S(6,2)-SC(2)
      DZ=S(7,2)-SC(3)
28    IF(DSA*DX+DSB*DY+DSC*DZ.LT.0.)DIREC=-1.
29    SC(7)=DSA*DIREC
      SC(8)=DSB*DIREC
      SC(9)=DSC*DIREC

C                  DSPLN IS PERPTO DSLN AND TA
30    TI=SC(4)
      TJ=SC(5)
      TK=SC(6)

      DSX=DSB*TK-DSC*TJ
      DSY=DSC*TI-DSA*TK
      DSZ=DSA*TJ-DSB*TI
      SEC=SQRT(DSX**2+DSY**2+DSZ**2)
      IF(SEC.GT.0.)GOTO 32
C                  ERROR. DSNRM IS PARLEL TA
      IFL(2)=163
      GOTO 99
C                  ADD DSPLN TO D(51,,)
32    D(53)=DSX/SEC
      D(54)=DSY/SEC
      D(55)=DSZ/SEC
      D(56)=D(53)*SC(1)+D(54)*SC(2)+D(55)*SC(3)
      JD(201)=6
      AD(102)=1.

C                  IF NOPS, BUILD A PSPL AND ADD TO D(1-6)
      IF(IFL(276).EQ.0)GOTO 40
C                  PSPL CONTAINS DSABC AND IS APPROX PERPTO IJK
C                   RGT SENSE
      RX=DSB*TK-DSC*TJ
      RY=DSC*TI-DSA*TK
      RZ=DSA*TJ-DSB*TI
C                   UP
      PX=RY*DSC-RZ*DSB
      PY=RZ*DSA-RX*DSC
      PZ=RX*DSB-RY*DSA
      SEC=SQRT(PX**2+PY**2+PZ**2)
      IF(SEC.GT.0.)GOTO 36
C               ERROR. SEC=0
34    IFL(2)=163
      GOTO 99
C                   
36    PX=PX/SEC
      PY=PY/SEC
      PZ=PZ/SEC
      COB=TI*PX+TJ*PY+TK*PZ
      SIB=0.
      IF(ABS(COB).LT.1.)SIB=SQRT(1.-COB**2)
      PK=PX*SC(1)+PY*SC(2)+PZ*SC(3)+TOOL(2)*(COB-1.)-SIB*TOOL(6)-SC(23)
C               BUILD 'PSPL' IN D(1)
      JD(1)=6
      AD(2)=1.
      D(3)=PX
      D(4)=PY
      D(5)=PZ
      D(6)=PK

C               BUILD 3SRF GO CMD IN SC(10)
40    KSC(37)=702
      KSC(38)=1
      KSC(39)=3
      SC(11)=71.
      SC(12)=0.
      KSC(48)=6
      SC(13)=0.
      SC(14)=HGO(11)
      SC(15)=HGO(12)

99    RETURN
      END
