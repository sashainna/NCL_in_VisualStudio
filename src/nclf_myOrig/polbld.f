C*********************************************************************
C*    NAME         :  polbld.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       polbld.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:27
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine polbld(i,j,k,l)
c*       hold pts(i,l) and chg pts(j,k) to polgon state.
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
      subroutine polbld(i,j,k,l)

      include 'com8a.com'

      common/blok/x(16),y(16),z(16),co(16),u,v,xpt,ypt,zpt,
     * vx(2),vy(2),vz(2)

      logical lv84,dir

      lv84 = sc(169).lt.8.499d0

      sma = .001
      itim=0

2     dx=x(l)-x(i)
      dy=y(l)-y(i)
      dz=z(l)-z(i)
      chd=dsqrt(dx**2+dy**2+dz**2)
      if(chd.gt.sma)goto 9
      if(itim.ne.0)goto 25
      itim=1
c                     chd very small, make it real         29-jan-87
c                     ii,ll pt to adj set of points in patch
      ii=i+4
      ll=l+4
      if(ii.lt.14)goto 4
      ii=i-4
      ll=l-4
4     dx=x(ll)-x(ii)
      dy=y(ll)-y(ii)
      dz=z(ll)-z(ii)
      chd=dsqrt(dx**2+dy**2+dz**2)
      if(chd.lt.sma)goto 25
c                   pts ij backoff sma.    pts kl go ahead sma
      dis=sma/chd
      x(i)=x(i)-dx*dis
      y(i)=y(i)-dy*dis
      z(i)=z(i)-dz*dis

      x(j)=x(j)-dx*dis
      y(j)=y(j)-dy*dis
      z(j)=z(j)-dz*dis

      x(k)=x(k)+dx*dis
      y(k)=y(k)+dy*dis
      z(k)=z(k)+dz*dis

      x(l)=x(l)+dx*dis
      y(l)=y(l)+dy*dis
      z(l)=z(l)+dz*dis
      goto 2

9     vx(1)=x(j)-x(i)
      vy(1)=y(j)-y(i)
      vz(1)=z(j)-z(i)
c
c... aak 16-OCT-97:
c... chnaged criterium for inverting the sign;
c... fixes srf construction when middle crv is in the middle 
c... of the conic
c
      if(lv84) then
         dir = vx(1)*dx+vy(1)*dy+vz(1)*dz.le.0.
      else
         dir = vx(1)*dx+vy(1)*dy+vz(1)*dz.lt.0.
      endif
      if(dir) then
         vx(1)=-vx(1)
         vy(1)=-vy(1)
         vz(1)=-vz(1)
      endif
      sec1=dsqrt(vx(1)**2+vy(1)**2+vz(1)**2)
      vx(2)=x(l)-x(k)
      vy(2)=y(l)-y(k)
      vz(2)=z(l)-z(k)
c
c... aak 16-OCT-97:
c... same as above 
c
      if(lv84) then
         dir = vx(2)*dx+vy(2)*dy+vz(2)*dz.le.0.
      else
         dir = vx(2)*dx+vy(2)*dy+vz(2)*dz.lt.0.
      endif
      if(dir) then
         vx(2)=-vx(2)
         vy(2)=-vy(2)
         vz(2)=-vz(2)
      endif
      sec2=dsqrt(vx(2)**2+vy(2)**2+vz(2)**2)
c          chd,sec1,sec2 must be gt 0
      if(chd.gt.0..and.sec1.gt.0..and.sec2.gt.0.)goto 30
25       ifl(2)=143
         return
30    cal=(vx(1)*dx+vy(1)*dy+vz(1)*dz)/sec1/chd
      cbe=(vx(2)*dx+vy(2)*dy+vz(2)*dz)/sec2/chd
      adis=.666667*chd/(1.+cal)
      bdis=.666667*chd/(1.+cbe)
      if(adis.gt.bdis)adis=bdis*(2.-bdis/adis)
      if(bdis.gt.adis)bdis=adis*(2.-adis/bdis)
      x(j)=x(i)+adis*vx(1)/sec1
      y(j)=y(i)+adis*vy(1)/sec1
      z(j)=z(i)+adis*vz(1)/sec1
      x(k)=x(l)-bdis*vx(2)/sec2
      y(k)=y(l)-bdis*vy(2)/sec2
      z(k)=z(l)-bdis*vz(2)/sec2
99    return
      end
