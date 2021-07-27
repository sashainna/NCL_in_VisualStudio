C*********************************************************************
C*    NAME         :  polfin.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       polfin.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:27
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine polfin(i,j,k,l)
c*       do pts(j,k) per input pts(i,l) and 2 vecs
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
      subroutine polfin(i,j,k,l)

      include 'com8a.com'

      common/wblok/w(600)
      common/blok/x(16),y(16),z(16),co(16),u,v,xpt,ypt,zpt
     1,vx(2),vy(2),vz(2)

      real*4 aw(1200),cal,cbe,ad,bd
      equivalence (w,aw)

      dx=x(l)-x(i)
      dy=y(l)-y(i)
      dz=z(l)-z(i)
      chd=dsqrt(dx**2+dy**2+dz**2)
      ad=.3345*chd
      bd=ad
c              ifl(30) on means rldsrf of 2 same cvs.
c                calc ad,bd as in curves
      if (ifl(30).eq.0)goto 30
      cal=(dx*vx(1)+dy*vy(1)+dz*vz(1))/chd
      cbe=(dx*vx(2)+dy*vy(2)+dz*vz(2))/chd
      ad=.666667*chd/(1.+cal)
      bd=.666667*chd/(1.+cbe)
      if(ad.gt.bd)ad=bd*(2.-bd/ad)
      if(bd.gt.ad)bd=ad*(2.-ad/bd)
30    x(j)=x(i)+ad*vx(1)
      y(j)=y(i)+ad*vy(1)
      z(j)=z(i)+ad*vz(1)
      x(k)=x(l)-bd*vx(2)
      y(k)=y(l)-bd*vy(2)
      z(k)=z(l)-bd*vz(2)
99    return
      end
