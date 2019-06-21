c**
C*********************************************************************
C*    NAME         :  sfevl2.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sfevl2.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:41
C********************************************************************/
C
c **********************************************************************
c **********************************************************************
c **  subroutine name: sfevl2
c **
c **  last revision:
c **  purpose of subroutine: this routine evaluates a patch of a surface
c **      at u & v, and returns point, normal, slope in u direc,
c **      slope in v direc
c **  input -
c **          srfhed  = second word of surface header (for offset calc.)
c **          p1      = surface patch in real*8 array
c **          ap1     = surface patch in real*4 array
c **          u       = u value at which to evaluate surface
c **          v       = v value at which to evaluate surface
c **  output -
c **          pvs(1:3)   = point at u,v
c **          pvs(4:6)   = normal vector at u,v
c **          pvs(1:9)   = u vector at u,v
c **          pvs(10:12) = v vector at u,v
c **
c **********************************************************************
c **********************************************************************
 
      subroutine sfevl2 (srfhed, p1, ap1, u, v, pvs)

      include 'com4a.com'

      real*8 srfhed,p1(3),pvs(12)
      real*4 ap1(60),u,v

      real*4 co(11),rx(4),ry(4),rz(4),x(4),y(4),z(4)

      real*8 asn
      real*4 bsn(2)
      integer*2 ksn(4)
      equivalence (asn,bsn,ksn)

      a=(1.-u)*(1.-v)
      b=(1.-u)*v
      c=u*(1.-v)
      e=u*v
      co(1)=a**2
      co(2)=2.*a*b
      co(3)=b**2
      co(4)=0.
      co(5)=2.*a*c
      co(6)=2.*(b*c+a*e)
      co(7)=2.*b*e
      co(8)=0.
      co(9)=c**2
      co(10)=2.*c*e
      co(11)=e**2
c          do 4 rpts
      do 60 i=1,4
      rx(i)=0.
      ry(i)=0.
      rz(i)=0.
c          set p-ptr
      ip=3*i+2
      if(i.gt.2)ip=ip+6
c          do the 11 sum
      do 50 k=1,11
      ip=ip+3
      rx(i)=rx(i)+co(k)*ap1(ip)
      ry(i)=ry(i)+co(k)*ap1(ip+1)
50    rz(i)=rz(i)+co(k)*ap1(ip+2)
60    continue
c*****************************************
c          4 int pts  ( 2 & 4 are actually deltas )
      x(1)=rx(1)+v*(rx(2)-rx(1))
      y(1)=ry(1)+v*(ry(2)-ry(1))
      z(1)=rz(1)+v*(rz(2)-rz(1))
      x(2)=rx(3)+v*(rx(4)-rx(3))-x(1)
      y(2)=ry(3)+v*(ry(4)-ry(3))-y(1)
      z(2)=rz(3)+v*(rz(4)-rz(3))-z(1)
      x(3)=rx(1)+u*(rx(3)-rx(1))
      y(3)=ry(1)+u*(ry(3)-ry(1))
      z(3)=rz(1)+u*(rz(3)-rz(1))
      x(4)=rx(2)+u*(rx(4)-rx(2))-x(3)
      y(4)=ry(2)+u*(ry(4)-ry(2))-y(3)
      z(4)=rz(2)+u*(rz(4)-rz(2))-z(3)
c
      xn=y(2)*z(4)-z(2)*y(4)
      yn=z(2)*x(4)-x(2)*z(4)
      zn=x(2)*y(4)-y(2)*x(4)

      sec=sqrt(xn**2+yn**2+zn**2)
      if (sec.eq.0.)sec=1.
      pvs(4)=xn/sec
      pvs(5)=yn/sec
      pvs(6)=zn/sec
      sec=sqrt(x(2)**2+y(2)**2+z(2)**2)
      if (sec.eq.0.)sec=1.
      pvs(7)=x(2)/sec
      pvs(8)=y(2)/sec
      pvs(9)=z(2)/sec
      sec=sqrt(x(4)**2+y(4)**2+z(4)**2)
      if (sec.eq.0.)sec=1.
      pvs(10)=x(4)/sec
      pvs(11)=y(4)/sec
      pvs(12)=z(4)/sec
c          srf pt
      pvs(1)=x(1)+x(2)*u+p1(1)
      pvs(2)=y(1)+y(2)*u+p1(2)
      pvs(3)=z(1)+z(2)*u+p1(3)
c                  if offset surface, chg srf pt
      asn=srfhed
      if(ksn(1).ne.7)goto 100
      dis=bsn(2)
      pvs(1)=pvs(1)+pvs(4)*dis
      pvs(2)=pvs(2)+pvs(5)*dis
      pvs(3)=pvs(3)+pvs(6)*dis

100   continue

99999 return
      end
