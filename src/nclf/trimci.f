
C*********************************************************************
C*    NAME         :  trimci.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       trimci.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:48
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine trimci(cidesc,p1desc,p2desc,imod)
c*       purpose of this subroutine is to calculate the
c*       necessary information to trim a circle to two
c*       points.  this routine is has four input
c*       arguments:
c*
c*           cidesc - this is the associated word of the circle
c*           p1desc - this is the associated word of the strtpt
c*           p2desc - this is the associated word of the endpt
c*           imod   - this is the modifier (xs,xl etc)
c*
c*    *   the logical steps taken by this subroutine are:
c*
c*           1.  project the start point and end point onto the
c*               plane of the circle.
c*           2.  project the points onto the circle.
c*           3.  calc a midpt on circle between above pts per imod.  if
c*               imod=0, calc midpt for smaller arc strtpt to endpt.
c*           4.  build the sc table so that it can be passed to
c*               geogn2 as a circle definition type 1 (ci/pt1,pt2,pt3)
c*
c*    *   the format of the sc table at exit should be:
c*
c*           sc(10) = type 604, subtype 1
c*           sc(11) = pt1 x value
c*           sc(12) = pt1 y value
c*           sc(13) = pt1 z value
c*           sc(14) = pt2 x value
c*           sc(15) = pt2 y value
c*           sc(16) = pt2 z value
c*           sc(17) = pt3 x value
c*           sc(18) = pt3 y value
c*           sc(19) = pt3 z value
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
      subroutine trimci(cidesc,p1desc,p2desc,imod)

      include 'com8a.com'

      real*8 cidesc,p1desc,p2desc
      integer*2 imod

      real*8 asn,c(11),u(3),v(3)
      integer*2 ksn(4)
      integer*4 nclkey
      integer*2 ietype
      logical trflg
      equivalence (asn,ksn)

c                  get ci1 in c-tbl, pt1 in u, pt2 in v
      trflg = .true.
      call gtentt(cidesc, trflg, nclkey, ietype, c(1))
      call gtentt(p1desc, trflg, nclkey, ietype, u(1))
      call gtentt(p2desc, trflg, nclkey, ietype, v(1))

c                   nearpt to pt1 on axis
      dis=c(4)*(u(1)-c(1))+c(5)*(u(2)-c(2))+c(6)*(u(3)-c(3))
      xn=c(1)+dis*c(4)
      yn=c(2)+dis*c(5)
      zn=c(3)+dis*c(6)
c                   dxyz to pt1 from nrpt
      dx=u(1)-xn
      dy=u(2)-yn
      dz=u(3)-zn
      sec=dsqrt(dx**2+dy**2+dz**2)
      if(sec.gt..001)goto 20
c                   error. strtpt or endpt is on axis
15    ifl(2)=291
      goto 99
c                   cp1 is radius dist from circ center pt
20    rad=c(7)/sec
      x1=c(1)+rad*dx
      y1=c(2)+rad*dy
      z1=c(3)+rad*dz

c                   ditto pt2
      dis=c(4)*(v(1)-c(1))+c(5)*(v(2)-c(2))+c(6)*(v(3)-c(3))
      xn=c(1)+dis*c(4)
      yn=c(2)+dis*c(5)
      zn=c(3)+dis*c(6)
      dx=v(1)-xn
      dy=v(2)-yn
      dz=v(3)-zn
      sec=dsqrt(dx**2+dy**2+dz**2)
      if(sec.lt..001)goto 15
      rad=c(7)/sec
      x2=c(1)+rad*dx
      y2=c(2)+rad*dy
      z2=c(3)+rad*dz
c              if modifier given, set approx vec to midpt
      if(imod.ne.0) goto 23
c              modifier not given. rgt= circ center to cp1,cp2 ave
      dx=(x2+x1)/2.-c(1)
      dy=(y2+y1)/2.-c(2)
      dz=(z2+z1)/2.-c(3)
      goto 28
23    vx=0.
      vy=0.
      vz=0.
      if(imod.eq.638)vx=1.
      if(imod.eq.641)vx=-1.
      if(imod.eq.639)vy=1.
      if(imod.eq.642)vy=-1.
      if(imod.eq.640)vz=1.
      if(imod.eq.643)vz=-1.
      sec=(vx**2+vy**2+vz**2)
      if(sec.eq.1.)goto 25
c                error.  modifier ng
      ifl(2)=292
      goto 99
c                fwd sense p1 to p2
25    fx=x2-x1
      fy=y2-y1
      fz=z2-z1
c                 rgt sense
      dz=fx*c(5)-fy*c(4)
      dy=fz*c(4)-fx*c(6)
      dx=fy*c(6)-fz*c(5)
28    sec=dsqrt(dx**2+dy**2+dz**2)
      if(sec.gt..001)goto 30
c                 error. rgt did not develop.
29    ifl(2) = 293
      goto 99
30    if(imod.eq.0)goto 35
      rchk=dx*vx+dy*vy+dz*vz
      if(dabs(rchk).lt..0001)goto 29
      if(rchk.lt.0.)sec=-sec
35    rad=c(7)/sec
c                  calc midpt and put in sc(14-16)
      sc(14)=c(1)+dx*rad
      sc(15)=c(2)+dy*rad
      sc(16)=c(3)+dz*rad
c                  also cp1,cp2
      sc(11)=x1
      sc(12)=y1
      sc(13)=z1
      sc(17)=x2
      sc(18)=y2
      sc(19)=z2
c                   sc(10)     note:  ksn(3)=9876 signals redef to geogn2
      asn=0.
      ksn(1)=604
      ksn(2)=1
      ksn(3)=9876
      sc(10)=asn
99    return
      end
