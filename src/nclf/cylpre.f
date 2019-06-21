C*********************************************************************
C*    NAME         :  cylpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cylpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:49
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cylpre
C*                                                   written pem 9-may-83
C*             input:  surf/xyzls,pl1,xyzls,pl2,radius,r,pt1,pt2
C*            output:  2 circles in w-tbl
C*                     sc updated to look like 2 circle input
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
      subroutine cylpre

      include 'com8a.com'

      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/wblok/w(600)
      common/pblok/p(400)

      real*4 aw(1200),ap(800)
      equivalence(asn,ksn),(w,aw),(p,ap),(sc,lsc)
      integer*2 ksn(4),lsc(400)

      integer*2 prtyp,icoor
      real*8 prdat(16)
      common/primbl/prdat,prtyp,icoor

      integer*4  nclkey
      integer*2  ietype
      logical trflg
      real*8 a(7),b(7),c(12),d(12),x1,y1,z1,x2,y2,z2
c
c... pl1,pl2 in a,b
c
      trflg = .true.
      call gtplt(sc(12), ifl(72), a)
      call gtplt(sc(14), ifl(72), b)
c
c... pt1,2 in c,d   ( pts required at this 1st write )
c
      call gtentt(sc(17), trflg, nclkey, ietype, c)
      call gtentt(sc(18), trflg, nclkey, ietype, d)
      mod1 = sc(11)
      mod2 = sc(13)
c
c... pt pls toward cyl axis per modifier
c
      if(mod1.gt.640) goto 15
      i=mod1-637
      if(a(i))18,980,20
15    i=mod1-640
      if(a(i))20,980,18
18    do 19 i=1,4
19    a(i)=-a(i)
c
20    if(mod2.gt.640)goto 25
      i=mod2-637
      if(b(i))28,980,30
25    i=mod2-640
      if(b(i))30,980,28
28    do 29 i=1,4
29    b(i)=-b(i)

30    continue
c
c... cyl axis
c
      ax=a(2)*b(3)-a(3)*b(2)
      ay=a(3)*b(1)-a(1)*b(3)
      az=a(1)*b(2)-a(2)*b(1)
      sec=dsqrt(ax**2+ay**2+az**2)
      if(sec.lt..001)goto 980
      ax=ax/sec
      ay=ay/sec
      az=az/sec
c
c... fwd along circ arc
c
      fx=a(1)-b(1)
      fy=a(2)-b(2)
      fz=a(3)-b(3)
      sec=dsqrt(fx**2+fy**2+fz**2)
      if(sec.lt..001)goto 980
      fx=fx/sec
      fy=fy/sec
      fz=fz/sec
c
c... drop pt1 to intof pl1,pl2
c
      si = f_dot (a,b)
      if (si.gt..9999999d0) si=.9999999d0
      den = 1.-si*si

      p1 = - sc(15) - a(4) + f_dot (a,c) 
      p2 = - sc(15) - b(4) + f_dot (b,c)
      d1=(p1-p2*si)/den
      d2=(p2-p1*si)/den

      x1=c(1)-a(1)*d1-b(1)*d2
      y1=c(2)-a(2)*d1-b(2)*d2
      z1=c(3)-a(3)*d1-b(3)*d2

      gx=a(1)+b(1)
      gy=a(2)+b(2)
      gz=a(3)+b(3)
      sec=dsqrt(gx**2+gy**2+gz**2)
      if(sec.lt..001)goto 980
      gx=-gx/sec
      gy=-gy/sec
      gz=-gz/sec
      cosphi=-a(1)*gx-a(2)*gy-a(3)*gz
      phi = dacos(cosphi)
      gk=gx*x1+gy*y1+gz*z1+sc(15)*cosphi
c                 load ci1 in w(1)
      w(1)=x1
      w(2)=y1
      w(3)=z1
      w(4)=ax
      w(5)=ay
      w(6)=az
      w(7)=sc(15)
      w(8)=gx
      w(9)=gy
      w(10)=gz
      w(11)=gk
      w(12)=fx
      w(13)=fy
      w(14)=fz
      w(15)=phi
c
c... ci2 is same as ci1 with ctrpt & limpl chg
c
      p1 = -sc(16) - a(4) + f_dot (a,d)
      p2 = -sc(16) - b(4) + f_dot (b,d)
      d1=(p1-p2*si)/den
      d2=(p2-p1*si)/den

      x2 = d(1)-a(1)*d1-b(1)*d2
      y2 = d(2)-a(2)*d1-b(2)*d2
      z2 = d(3)-a(3)*d1-b(3)*d2

      gk=gx*x2+gy*y2+gz*z2+sc(16)*cosphi

      iwx=(maxwd+20)*2
      do 40 i=4,15
40    w(iwx+i)=w(i)

      w(iwx+1) = x2
      w(iwx+2) = y2
      w(iwx+3) = z2
      w(iwx+7) = sc(16)
      w(iwx+11)= gk
c
c... identify as circles to rldgen
c
      lsc(209) = CIRCLE
      lsc(211) = CIRCLE
      asn = sc(10)
c
c...For consistency
c...Use the surface analyze routine
c...to determine if this is a valid cylinder
c...Bobby - 3/6/03
c
cc      if (ksn(3).eq.0 .or. dabs(sc(15)-sc(16)).le.0.1*sc(27)) then
cc        prtyp = 5
cc        icoor = 1
cc        prdat(1)=x1
cc        prdat(2)=y1
cc        prdat(3)=z1
cc        prdat(4) = ax
cc        prdat(5) = ay
cc        prdat(6) = az
cc        prdat(7) = sc(15)
cc        prdat(8)=dsqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2))
cc      endif
      goto 999
c
c... error exit
c
980   ifl(2) = 163
990   if(ifl(2).lt.1) ifl(2) = 5

999   return
      end
