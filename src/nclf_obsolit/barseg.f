C*********************************************************************
C*    NAME         :  barseg.f
C*       CONTAINS:
C*         barseg   barcta
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       barseg.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       03/01/16 , 11:13:47
C          THIS ROUTINE ENTERED ON CUTTER/BARREL  COMMAND.
C          WRITTEN PEM 17-NOV-87
C
C          CALCULATES VARIOUS PARAMS FOR BARREL CUTTER AND STOS SAME.
C           IFL(282)=1  MEANS BARREL TOOL IN EFFECT.
C*********************************************************************
C
c***********************************************************************
c
c   SUBROUTINE:  barseg (idis,cparm,dcutr,nparms)
c
c   FUNCTION:  This routine parses a barrel CUTTER statement and stores
c              its definition.
c
c   INPUT:  idis    I*2  D1  -  0 = Actual cutter being defined.  1 =
c                               displayed cutter.  3 = Pseudo cutter.
c
c           cparm   R*8  D6  -  Cutter parameters.
c
c           nparms  I*2  D1  -  Number of parameters in 'cparm'.
c
c   OUTPUT: dcutr   R*8  D10 -  Output cutter parameters when defining a
c                               displayed cutter.  An actual cutter
c                               definition is stored in the SC table.
c
c
c***********************************************************************
c
      SUBROUTINE BARSEG (idis,cparm,dcutr,nparms)

      include 'com8a.com'
      include 'mocom.com'
c
      integer*2 idis,nparms
c
      real*8 cparm(6),dcutr(10)
c 
      REAL*4 ASC(310)
      INTEGER*2 KSN(4)
      EQUIVALENCE (ASN,KSN),(ASC,SC)

C      IFL(2)=12
      RADIAN=57.29577951D0

C                5-PARAMS IN SC(11-15)
      DIA=cparm(1)
      R1=cparm(2)
      HGT=cparm(3)
      R2=cparm(4)
      Z2=cparm(5)

      CUTE=DIA/2.-R1
C                PRE-CHECKS ON INPUT DATA
      IF(R1.LT.0..OR.R2.LE.0..OR.CUTE.LT.-.001)GOTO 98
      DZ=Z2-R1
      DR=R2-R1
C                SIDE RADIUS R2 MUST NOT EQUAL R1
      IF(DABS(DR).LT..001)GOTO 98
      SINB=DZ/DR
C                BETA LIMS ARE +89, -89
      IF(DABS(SINB).GT..99985)GOTO 98
      COSB=DSQRT(1.-SINB**2)
      X2=CUTE-DR*COSB
      ZTAN=R1-R1*SINB
C                BRA ON NPARMS
      IF(NPARMS.EQ.6)GOTO 20
C                ALPHA NOT GIVEN.  CALC SIN,COS PER HGT IF R2 REAL
      IF(R2.LT..002)GOTO 98
      SINA=(Z2-HGT)/R2
      GOTO 30
C                ALPHA GIVEN
20    ALF=cparm(6)/RADIAN
      SINA=DSIN(ALF)

C                ALPHA LIMS ALSO +- 89
30    IF(DABS(SINA).GT..99985)GOTO 98
      COSA=DSQRT(1.-SINA**2)
      X4=X2+R2*COSA
      Z4=Z2-R2*SINA
      CUTA=X4+SINA/COSA*(HGT-Z4)
      XTAN=CUTE+R1*COSB

C               ADDITIONAL ERROR CHECKS
      IF(CUTA.LT.0.)GOTO 98
      IF(HGT+.0001.LT.Z4)GOTO 98
      IF(Z4.LE.ZTAN)GOTO 98

C               CUTTER DEF SEEMS OK.  ADD DATA TO SC AND TOOL TABLES.
c
c...Store actual cutter parameters in SC table
c
      if (idis .eq. 0 .or. idis .eq. 3) then
          IFL(282)=1
          SC(28)=DIA
          SC(29)=R1
          SC(30)=HGT
          SC(31)=DASIN(SINB)*RADIAN
          ASC(63)=SINB
          ASC(64)=COSB
C               CUTN AND DISK INDICATOR DO NOT APPLY.  SET TO ZERO
          ASC(65)=0.
          ASC(66)=0.
          ASC(67)=X2
          ASC(68)=Z2
          ASC(305)=SINA
          ASC(306)=CUTA
          ASC(307)=R2
C                      MAX R  (FOR DISPLY)
          RMAX=X2+R2
          IF(SINB.LE.0.)RMAX=CUTE+R1
          IF(SINA.GE.0.)RMAX=CUTA
          ASC(308)=RMAX
C               'TOOL' TBL
          TOOL(1)=2.*CUTA
          TOOL(2)=R1
          TOOL(3)=HGT
          TOOL(4)=R1+.005
          TOOL(5)=(R1+HGT)/2.
          TOOL(6)=CUTE
          TOOL(7)=.5*DSQRT(DIA**2+HGT**2)
c
c...Store display cutter parameters in subroutine argument
c
      else
          dcutr(1) = dia
          dcutr(2) = r1
          dcutr(3) = hgt
          dcutr(4) = dasin(sinb) * radian
          dcutr(5) = x2
          dcutr(6) = z2
          dcutr(7) = r2
          dcutr(8) = sina
          dcutr(9) = 1
      endif

C           ZERO IFL(251) TO MAKE SUBROUTINE CDRAW RE-CALCULATE CUTTER DISPLY
      IFL(251)=0

      GOTO 99

C               ERROR EXIT.
98    IF(IFL(2).LT.1)IFL(2)=119

99    RETURN
      END
C
c***********************************************************************
c
c   SUBROUTINE:  barcta (gtanpt,gsan,gcuta)
c
c   FUNCTION:  This routine calculates the tangent angle (alf) and
c              diameter (cuta) at the provided height of a barrel cutter.
c              The global parameters of the barrel cutter are used.
c
c   INPUT:  gtanpt   R*4  D1  -  Contact point on barrel cutter to define
c                               tangent angle and diameter for.
c
c   OUTPUT: gsan     R*8  D1  -  Sine of tangent angle at 'gtanpt'.
c
c           gcuta    R*8  D1  -  Cutter diameter at 'gtanpt'.
c
c***********************************************************************
c
      subroutine barcta (gtanpt,gsan,gcuta)

      include 'com8a.com'
      include 'mocom.com'
c
      real*4 gtanpt,gsan,gcuta
c
      real*4 dia,r1,hgt,r2,z2,cute,dz,dr,x2,sinb,cosb,sina,cosa,x4,z4,
     1       z4a
c
      real*4 asc(310)
      equivalence (asc,sc)
c
c...Define necessary values
c
      dia    = sc(28)
      r1     = sc(29)
      hgt    = gtanpt
      r2     = asc(307)
      z2     = asc(68)
c
      cute   = dia/2. - r1
      dz     = z2 - r1
      dr     = r2 - r1
      sinb   = dz/dr
      cosb   = sqrt(1.-sinb**2)
      x2     = cute-dr*cosb
c
c....Calculate sine of tangent angle
c
      sina   = (z2-hgt)/r2
c
c...If tangent height is above flat angle of cutter
c...Then don't change angle or diameter
c
      cosa   = sqrt(1.-sina**2)
      x4     = x2 + r2*cosa
      z4     = z2 - r2*sina
      z4a    = z2 - r2*asc(305)
      if (gtanpt .ge. z4a) go to 8000
     
c
c...Calculate diameter at contact point
c
      gsan   = sina
      gcuta  = (x4 + sina/cosa * (hgt-z4)) * 2.
c
c...End of routine
c
 8000 return
      end
