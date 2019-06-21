C*********************************************************************
C*    NAME         :  lnpv.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       lnpv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:14
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine lnpv(px,py,pz,vx,vy,vz)
C*       description
C*          solves a pv(u) for a line segment per current tool loc.
C*          ln is in d-tbl and 1st-try ept is in t-tbl.
C*
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
      subroutine lnpv(px,py,pz,vx,vy,vz)

      include 'com.com'
      include 'mocom.com'

      real*4 px,py,pz,vx,vy,vz

      real*8 xe,ye,ze,sx,sy,sz,dv,dx,dy,dz,rx,ry,rz,d1
      real*8 ti,tj,tk,co,si,sec,u
      integer*2 idx,iux,ihx
      real*8 asn
      integer*2 kd(600),ksn(4),ia,isrf
      real*4 ad(300)
      equivalence(d,ad,kd),(asn,ksn)
      equivalence(ifl(51),ia),(ifl(54),isrf)

c          index to d-tbl and t-tbl
      idx=50*(isrf-1)
      iux=6*isrf+7
      ihx = iux+2
      xe=t(1,ia)
      ye=t(2,ia)
      ze=t(3,ia)
      if (ifl(23).eq.1) then
        ti = s(1,1)
        tj = s(2,1)
        tk = s(3,1)
      else
        ti = t(4,ia)
        tj = t(5,ia)
        tk = t(6,ia)
      endif
      sx=d(idx+3)
      sy=d(idx+4)
      sz=d(idx+5)
      vx=d(idx+6)
      vy=d(idx+7)
      vz=d(idx+8)
      dv=sqrt(vx**2+vy**2+vz**2)
      if(dv.eq.0.0) goto 9163
      vx=vx/dv
      vy=vy/dv
      vz=vz/dv
c
c... Project external point into plane of line & tool axis
c
      rx = vy*tk-tj*vz
      ry = vz*ti-tk*vx
      rz = vx*tj-ti*vy
      sec = dsqrt(rx**2+ry**2+rz**2)
      if (sec.lt..00001) sec = 1.0d0
      rx = rx/sec
      ry = ry/sec
      rz = rz/sec
      d1 = rx*(xe-sx)+ry*(ye-sy)+rz*(ze-sz)
      xe = xe-rx*d1
      ye = ye-ry*d1
      ze = ze-rz*d1
c
c... Get vector from external point normal to line
c
      dx=xe-sx
      dy=ye-sy
      dz=ze-sz
      co=dx*vx+dy*vy+dz*vz
      dx=co*vx-dx
      dy=co*vy-dy
      dz=co*vz-dz
c
c... Get length of vector from external point to line in direction on tool axis
c
      co = vx*ti+vy*tj+vz*tk
      if (co.gt..99995) co = .99995
      if (co.lt.-.99995) co = -.99995
      si = dsqrt(1.0-co**2)
      d1 = dsqrt(dx**2+dy**2+dz**2)/si
      if (ti*dx+tj*dy+tk*dz .lt. 0.0) d1 = -d1
c
c... Get vector line start point to intersection point on line
c
      dx = xe+ti*d1-sx
      dy = ye+tj*d1-sy
      dz = ze+tk*d1-sz
      u  = (dx*vx+dy*vy+dz*vz)/dv
      if (u.lt.0.0) u = 0.0
      if (u.gt.1.0) u = 1.0
      sec = u*dv
      px = sx+vx*sec
      py = sy+vy*sec
      pz = sz+vz*sec
      t(iux,ia)=u
      t(ihx,ia)=(px-xe)*ti+(py-ye)*tj+(pz-ze)*tk

99    return

9163  ifl(2) = 163
      goto 99

      end
