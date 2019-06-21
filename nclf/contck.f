
C*********************************************************************
C*    NAME         :  contck.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       contck.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:43
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine contck(kmx,kpat,ksid)
C*         called by quipre to see if ept(kmx) is contained in pat(kpat)
C*
C*          if containment found, set ksid to the side indicated.
C*          ksid=0 on return means nofind.
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
      subroutine contck(kmx,kpat,ksid)

      include 'com4a.com'

      common/pblok/p(400)
      common/wblok/w(600)

      real*8 p,w,asn,amap(12)
c      real*8 x(25),y(25),z(25)       (x,y,z are real*4 this subr.)
      real*4 x(25),y(25),z(25)
      real*4 xm(12),ym(12),zm(12),aw(1200),bsn(2)
      real*4 xe(48),ye(48),ze(48)
      integer*2 ival(67),ksn(4),kmap(48),ki(4),kj(4)

c               use pblok for misc array storage
      equivalence (p,x),(p(26),y),(p(51),z),(asn,bsn,ksn),(w,aw),
     1 (p(76),amap,kmap),(p(100),xm),(p(106),ym),(p(112),zm),
     2 (p(118),xe),(p(142),ye),(p(166),ze),(p(190),ival),
     3 (p(210),ki),(p(211),kj)

      ksid=0
      iwx=40*kpat-19
      jwx=2*iwx
      xo=aw(jwx+1)
      yo=aw(jwx+2)
      zo=aw(jwx+3)
c                   convert ept to patch sys
      tx=xe(kmx)-xo
      ty=ye(kmx)-yo
      tz=ze(kmx)-zo
c                   move patch pts to xyz ( for convenience)
      jwx=jwx+3
      do 20 i=1,25
      j=jwx+i
      x(i)=aw(j)
      y(i)=aw(j+25)
20    z(i)=aw(j+50)
c                   start at u,v=.5
      u=.5
      v=.5
      itim=0
c                    pts 2 and 4 are deltas
30    holdu=u
      holdv=v
      itim=itim+1
      if(itim.gt.10)goto 99
      x1=x(1)+v*(x(5)-x(1))
      y1=y(1)+v*(y(5)-y(1))
      z1=z(1)+v*(z(5)-z(1))
      x2=x(21)+v*(x(25)-x(21))-x1
      y2=y(21)+v*(y(25)-y(21))-y1
      z2=z(21)+v*(z(25)-z(21))-z1
      x3=x(1)+u*(x(21)-x(1))
      y3=y(1)+u*(y(21)-y(1))
      z3=z(1)+u*(z(21)-z(1))
      x4=x(5)+u*(x(25)-x(5))-x3
      y4=y(5)+u*(y(25)-y(5))-y3
      z4=z(5)+u*(z(25)-z(5))-z3
c                      normal
      xn=y2*z4-z2*y4
      yn=z2*x4-x2*z4
      zn=x2*y4-y2*x4
c                      u-vec
      xu=y4*zn-z4*yn
      yu=z4*xn-x4*zn
      zu=x4*yn-y4*xn
c                      v-vec
      xv=z2*yn-y2*zn
      yv=x2*zn-z2*xn
      zv=y2*xn-x2*yn
c
      uden=xu*x2+yu*y2+zu*z2
      vden=xv*x4+yv*y4+zv*z4
c                      zero denom means nofind
      if(uden.eq.0..or.vden.eq.0.)goto 99
      uerr=(xu*(tx-x1)+yu*(ty-y1)+zu*(tz-z1))/uden-u
      verr=(xv*(tx-x3)+yv*(ty-y3)+zv*(tz-z3))/vden-v
      if(abs(uerr).lt..001.and.abs(verr).lt..001) goto 40
c                      go again, if chg real.
      u=u+uerr
      v=v+verr
      if(u.lt.0.)u=0.
      if(u.gt.1.)u=1.
      if(v.lt.0.)v=0.
      if(v.gt.1.)v=1.
c
      if(abs(u-holdu).gt..001.or.abs(v-holdv).gt..001)goto 30
c
c                      pick side per u,v
40    derr=abs(u-.5)+abs(v-.5)
c                      if u,v near center or corner, no side indicated.
      if(derr.lt..15.or.derr.gt..99)goto 99
c                      also exit if uerr or verr gt 1.0
      if(abs(uerr).gt.1..or.abs(verr).gt.1.)goto 99
      ds1=u
      ds2=1.-u
      ds3=v
      ds4=1.-v
      ds=min(ds1,ds2,ds3,ds4)
      isid=1
      if(ds.eq.ds2)isid=2
      if(ds.eq.ds3)isid=3
      if(ds.eq.ds4)isid=4
c                     full chk ept vs this side.

c                       set i,j,k,l,m for this side
      i=1
      j=2
      k=3
      l=4
      m=5
      if(isid.eq.1)goto 50
      if(isid-3)42,43,44
42    i=21
      j=22
      k=23
      l=24
      m=25
      goto 50
43    i=1
      j=6
      k=11
      l=16
      m=21
      goto 50
44    i=5
      j=10
      k=15
      l=20
      m=25
c                   begin sch at cv midpt
50    ro=.5
      jtim=0
55    hro=ro
      c1=(1.-ro)**3
      c2=3.*ro*(1.-ro)**2
      c3=3.*ro**2*(1.-ro)
      c4=ro**3
      xa=c1*x(i)+c2*x(j)+c3*x(k)+c4*x(l)
      ya=c1*y(i)+c2*y(j)+c3*y(k)+c4*y(l)
      za=c1*z(i)+c2*z(j)+c3*z(k)+c4*z(l)

      dx=c1*x(j)+c2*x(k)+c3*x(l)+c4*x(m)-xa
      dy=c1*y(j)+c2*y(k)+c3*y(l)+c4*y(m)-ya
      dz=c1*z(j)+c2*z(k)+c3*z(l)+c4*z(m)-za
      den=dx**2+dy**2+dz**2
      if(den.eq.0.)goto 99
      roer=(dx*(tx-xa)+dy*(ty-ya)+dz*(tz-za))/den-ro
      roer=roer/4.
      if(abs(roer).lt..00001)goto 60
c          go again
      jtim=jtim+1
      if(jtim.gt.10)goto 99
      ro=ro+roer
      if(ro.lt.0.)ro=0.
      if(ro.gt.1.)ro=1.
      if(abs(hro-ro).gt..00001)goto 55
c                compare this cvpt to ept
60    xc=xa+ro*dx
      yc=ya+ro*dy
      zc=za+ro*dz
      dissq=(xc-tx)**2+(yc-ty)**2+(zc-tz)**2
      if(dissq.gt..0004) goto 99
c                 cv match found.  set ksid and exit.
      ksid=isid

99    return
      end
