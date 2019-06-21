
C*********************************************************************
C*    NAME         :  bscrub.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       bscrub.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:38
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine bscrub
C*        ******* now dummy *******
C*        will later handle different scrub type
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine bscrub
c
      include 'com4a.com'
      include 'mocom.com'

       common/scrblk/e,qx,qy,qz,dqx,dqy,dqz,sfu,sfv,xsf,ysf,zsf,
     1 vna,vnb,vnc,uold

      real*8 e(14)
      real*4 ad(300),ae(28),qx(4),qy(4),qz(4),dqx(4),dqy(4),dqz(4)
      integer*2 ksc(500),kd(600)
      equivalence (sc,ksc),(d,ad,kd),(e,ae)

cccccccccccc
      ifl(2)=5
cccccccccccc
      return
      end
