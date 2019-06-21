C*********************************************************************
C*    NAME         :  circpv.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       circpv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine circpv(px,py,pz,vx,vy,vz)
C*       description
C*          solves a pv this circle per current tool loc.
C*          ci is in d-tbl and 1st-try ept is in s(8,,isrf).
C*
C*            chg  17-jan-83   correct ifl(2) error values to 163,-137
C*                             (were 203,-204)
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
      subroutine circpv(px,py,pz,vx,vy,vz)

      include 'com4a.com'
      include 'mocom.com'

      real*8 asn
      integer*2 kd(600),ksn(4)
      real*4 ad(300)
      equivalence(d,ad,kd),(itfl,ifl(49)),(iptk,ifl(50)),(asn,ksn)
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

c          index to d-tbl
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
      itim=0
      jretry=0
c          do ept deltas and project on zc=0 pl
      i=idx
      ihx=6*isrf+9
      h=t(ihx,ia)
30    dx=t(1,ia)+h*t(4,ia)-d(i+3)
      dy=t(2,ia)+h*t(5,ia)-d(i+4)
      dz=t(3,ia)+h*t(6,ia)-d(i+5)
      dis=d(i+6)*dx+d(i+7)*dy+d(i+8)*dz
      dx=dx-d(i+6)*dis
      dy=dy-d(i+7)*dis
      dz=dz-d(i+8)*dis
31    tl=sqrt(dx**2+dy**2+dz**2)
      if(tl.gt.0..or.jretry.gt.0)goto34
c               re-try 1 time with .001 move in fwd direction
      jretry=1
      dx=dx+.001*t(7,ia)
      dy=dy+.001*t(8,ia)
      dz=dz+.001*t(9,ia)
      goto 31
34    ro=0.
      jretry=0
      if(tl.gt.0.)ro=d(idx+9)/tl
c          point on circle
      px=d(idx+3)+ro*dx
      py=d(idx+4)+ro*dy
      pz=d(idx+5)+ro*dz
c          check the h-value. fix 5-times only (temp)
      dh=t(4,ia)*(px-t(1,ia))
     1  +t(5,ia)*(py-t(2,ia))
     2  +t(6,ia)*(pz-t(3,ia))-h
      h=h+dh
      if(dh.lt..001)goto 35
      if(itim.gt.4)goto 35
      itim=itim+1
      goto 30
c          now vxyz
35    vx=dz*d(idx+7)-dy*d(idx+8)
      vy=dx*d(idx+8)-dz*d(idx+6)
      vz=dy*d(idx+6)-dx*d(idx+7)
      sec=sqrt(vx**2+vy**2+vz**2)
      if(sec.gt.0.)goto 40
      ifl(2)=163
      goto 99
40    vx=vx/sec
      vy=vy/sec
      vz=vz/sec
c          store h-value
      t(ihx,ia)=h
c          if pt not in valid range, set warning
c      clr=d(i+10)*px+d(i+11)*py+d(i+12)*pz-d(i+13)
c      if(clr.lt.-.005) ifl(2)=-137
99    return
      end
