
C*********************************************************************
C*    NAME         :  filename
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       lthctl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:15
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine lthctl
c*        entered after  lathe/command                  10-aug-83
c*        call rufpre and rufcut   -- or --   finpre and fincut
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine lthctl

      include 'com4a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 e,f,a,b
      real*4 ad(300)
      integer*2 ksc(500),kd(600)
      equivalence (sc,ksc),(d,ad,kd)

      real*8 svd(6)
c
c... Save default part surface
c
      do 5 i=1,6
5     svd(i) = d(i)
c
c                      lathe/rough or finish
      if(ksc(38).eq.1) goto 10
c                    ********** finish ********
      call finpre
      if(ifl(2).le.0)call fincut
      goto 99
c                    ********** rough ********
10    call rufpre
      if(ifl(2).le.0)call rufcut
99    continue
c
c... Restore default part surface
c
      do 20 i=1,6
20    d(i) = svd(i)
      return
      end
