C*********************************************************************
C*    NAME         :  nestpt.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       nestpt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:20
C********************************************************************/
C
C **********************************************************************
C **  SUBROUTINE NAME: NESTPT
C **
C **  LAST REVISION:
C **  PURPOSE OF SUBROUTINE: FIND POINT ON SURFACE IN PLANE FROM EXT POINT
C **  INPUT -
C **    ASW      - ASSOCIATED WORD OF SURFACE
C **    PTE      - EXTERNAL POINT
C **    PTX      - LAST POINT ON SURFACE
C **    ISF      - INDEX TO SURFACE TABLE
C **    R        - RADIUS
C **    DIR      - SIDE OF SURFACE
C **    U        - STARTING U VALUE
C **    V        - STARTING V VALUE
C **    CPL      - PLANE
C **  OUTPUT -
C **    U        - ENDING U VALUE
C **    V        - ENDING V VALUE
C **    PTX      - ENDING POINT ON SURFACE
C **    SS       - SURFACE TABLE
C **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE NESTPT (ASW, PTE, PTX, ISF, R, DIR, U, V, CPL, SS)

      INCLUDE 'com4a.com'

      REAL*8 ASW, PTE(3), PTX(3), CPL(4)
      REAL*4 R, U, V, DIR, SS(7)
      INTEGER*2 ISF

      INTEGER*2 ITIM
      REAL*8 VT(3), PS(3), VS(3),
     *        D1,D2, SEC, CO, DX,DY,DZ, VX,VY,VZ, dabs,dsqrt
c
c... aak 22-may-1998: made this routine real*8 (the above line)
c... and relaxed tolerance sc(27) instead sc(27)/10. below
c
      ITIM=0
10    CONTINUE
      ITIM=ITIM+1
      IF (ITIM.GT.60) GOTO 980
      CALL SFPT (ASW, PTX, ISF, DIR, U, V, SS)
      IF (U.GT.0..AND.U.LT.1..AND.V.GT.0..AND.V.LT.1.) THEN
        PS(1)=SS(5)
        PS(2)=SS(6)
        PS(3)=SS(7)
      ELSE
        D1=SS(1)*PTX(1)+SS(2)*PTX(2)+SS(3)*PTX(3)-SS(4)
        PS(1)=PTX(1)-SS(1)*D1
        PS(2)=PTX(2)-SS(2)*D1
        PS(3)=PTX(3)-SS(3)*D1
      ENDIF
      DX=PTE(1)-PS(1)
      DY=PTE(2)-PS(2)
      DZ=PTE(3)-PS(3)

      D1=dSQRT(DX**2+DY**2+DZ**2)
c      IF (D1.LT.SC(27)/10.) GOTO 100
      IF (D1.LT.SC(27)) GOTO 100
      DX=DX/D1
      DY=DY/D1
      DZ=DZ/D1

      VT(1)=SS(2)*CPL(3)-SS(3)*CPL(2)
      VT(2)=SS(3)*CPL(1)-SS(1)*CPL(3)
      VT(3)=SS(1)*CPL(2)-SS(2)*CPL(1)

      VS(1)=CPL(2)*VT(3)-CPL(3)*VT(2)
      VS(2)=CPL(3)*VT(1)-CPL(1)*VT(3)
      VS(3)=CPL(1)*VT(2)-CPL(2)*VT(1)

      SEC=dSQRT(VS(1)**2+VS(2)**2+VS(3)**2)
      IF(SEC.EQ.0)GOTO 980
      VS(1)=VS(1)/SEC
      VS(2)=VS(2)/SEC
      VS(3)=VS(3)/SEC
      CO=VS(1)*DX+VS(2)*DY+VS(3)*DZ
      IF(dABS(CO).GT..9999)GOTO 100
      D2=dSQRT(D1**2-(D1*CO)**2)

      VT(1)=VS(2)*DZ-VS(3)*DY
      VT(2)=VS(3)*DX-VS(1)*DZ
      VT(3)=VS(1)*DY-VS(2)*DX

      VX=VT(2)*VS(3)-VT(3)*VS(2)
      VY=VT(3)*VS(1)-VT(1)*VS(3)
      VZ=VT(1)*VS(2)-VT(2)*VS(1)

      SEC=dSQRT(VX**2+VY**2+VZ**2)
      IF(SEC.EQ.0.)GOTO 980
      D1=D2/SEC
      if (itim.gt.30) d1 = d1/2.0
      PTX(1)=PTX(1)+VX*D1
      PTX(2)=PTX(2)+VY*D1
      PTX(3)=PTX(3)+VZ*D1
      GOTO 10

100   PTX(1)=PS(1)
      PTX(2)=PS(2)
      PTX(3)=PS(3)
      GOTO 999

980   IFL(2)=163

999   RETURN
      END
