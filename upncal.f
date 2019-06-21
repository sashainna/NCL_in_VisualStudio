C*********************************************************************
C*    NAME         :  upncal.f
C*       CONTAINS:
C*    COPYRIGHT 1989 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       upncal.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:51
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine upncal
c*        calc srf point normal on unicad surface for scrub
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      SUBROUTINE UPNCAL
 
      include 'com4a.com'
      include 'mocom.com'
 
      COMMON/SCRBLK/E,QX,QY,QZ,DQX,DQY,DQZ,SFU,SFV,XSF,YSF,ZSF,
     1 VNA,VNB,VNC,UOLD
 
      REAL*8 U, V, SV(9), E(14)
      REAL*4 QX(4),QY(4),QZ(4),DQX(4),DQY(4),DQZ(4)
 
C             CONV SFUV TO PAT UV AND SOLVE PT
      U=SFU
      V=SFV
      CALL UEVSFT (U, V, 1, SV, ifl(2))
      XSF=SV(1)
      YSF=SV(2)
      ZSF=SV(3)
      DXDU=SV(4)
      DYDU=SV(5)
      DZDU=SV(6)
      DXDV=SV(7)
      DYDV=SV(8)
      DZDV=SV(9)
C                               SFNORM CALC.
      VNA=DYDU*DZDV-DZDU*DYDV
      VNB=DZDU*DXDV-DXDU*DZDV
      VNC=DXDU*DYDV-DYDU*DXDV
999   RETURN
      END
