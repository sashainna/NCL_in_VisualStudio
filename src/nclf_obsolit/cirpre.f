C*********************************************************************
C*    NAME         :  cirpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cirpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:41
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cirpre(inv,n,icirc)
C*          this routine checks the input data and solves circle
C*          11-param canon form. if all ok, stores it via putent
C*
C*          canon form is: x,y,z   ctr pt
C*                         a,b,c   'up'
C*                         radius
C*                         a,b,c,d  rgt pl
C*    PARAMETERS   
C*       INPUT  : 
C*          n  - number of points
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine cirpre(inv,n,icirc)

      include 'com8a.com'

      parameter (maxpt = 50)
      parameter (maxptx = 1000)

      common/wblok/w(600)
      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch

      real*8 x(maxptx),y(maxptx),z(maxptx)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx),dy(maxptx)
      real*4 dz(maxptx),ch(maxptx)
      integer*2 inv(maxptx)
      real*8 asn
      integer*2 ksn(4)
      equivalence (asn,ksn)
c     integer*2 ityp -- declared in com.com   
      integer*2 n
      real*8 chd,xf,yf,zf, secu,xu,yu,zu, secr,xr,yr,zr,
     *       sinb,cosb,sina,cosa,sinc,sincsq,cosc,cosd, h,rad,den,
     *       sec2,x2,y2,z2,sec3,x3,y3,z3,xm,ym,zm

      asn=sc(10)
      ityp=ksn(2)
c
c.... subtypes 1,2 are pt/ve input.
c............. 3-6 are geom constructs handled by cirprg
c............. 7-12  "   "       "         "    "  cirprh  
c............. 13-18 "   "       "         "    "  cirpri   
c............. 19-22 "   "       "         "    "  cirprj   
c
      if (ityp .lt. 3) then
          goto 20
      else if (ityp.ge.3 .and. ityp.le.6) then
          call cirprg
          goto 99
      else if (ityp.ge.7 .and. ityp.le.12) then
          call cirprh
          goto 99
      else if (ityp.ge.13 .and. ityp.le.18 .or. ityp.eq.26) then
          call cirpri
          goto 99
      else 
          call cirprj
          goto 99
      endif
c
c..... vf=(xf,yf,zf) is the vector from pt(1) to pt(n)
c
20    xf=x(n)-x(1)
      yf=y(n)-y(1)
      zf=z(n)-z(1)
      chd=dsqrt(xf**2+yf**2+zf**2)

      if (chd .lt. 1.E-6) then
        ifl(2)=163
        goto 99
      endif
c
c..... 'up' sense is v(1) x vf
c..... In the case of three pts, v(1) is the unit length vector perp. 
c..... to vf, is in the circle plane, and is looking toward pt(2) (rather
c..... than away from it).
c
      xu=b(1)*zf-c(1)*yf
      yu=c(1)*xf-a(1)*zf
      zu=a(1)*yf-b(1)*xf
      secu=dsqrt(xu**2+yu**2+zu**2)
c
c..... 'rgt' sense
c
      xr=yf*zu-zf*yu
      yr=zf*xu-xf*zu
      zr=xf*yu-yf*xu
      secr=dsqrt(xr**2+yr**2+zr**2)
      if (secr.gt.0.) goto 30
c
c..... error. p(1) to p(n) chd=0
c
      ifl(2)=163
      goto 99

30    xr=xr/secr
      yr=yr/secr
      zr=zr/secr
c
c..... sin beta=v1*fwd
c
      sinb=(a(1)*xf+b(1)*yf+c(1)*zf)/chd
      chd = chd/2.
      if (ksn(2).eq.1) goto 65
c
c..... if range 1 case, h = chd * tanb
c
      if (dabs(sinb).gt..70710678) goto 40
c
c..... range 1.
c
      cosb=dsqrt(1.-sinb*sinb)
      h=chd*(sinb/cosb)
      goto 60
c
c..... not range 1.
c
40    sina=a(1)*xr+b(1)*yr+c(1)*zr

      if (sina.le.0.) then
c
c..... error. st-ln case   (temp?)
c
        ifl(2)=200
        goto 99
      endif

      cosa=dsqrt(1.-sina**2)
      h=chd*(cosa/sina)
      if (sinb.lt.0.) h=-h
c
c..... finup and exit
c
60    rad=dsqrt(h*h+chd*chd)
      xctr=x(1)+xf/2.-h*xr
      yctr=y(1)+yf/2.-h*yr
      zctr=z(1)+zf/2.-h*zr
      goto 70

c
c..... Circle by 3 pts
c
65    x2=x(2)-x(1)
      y2=y(2)-y(1)
      z2=z(2)-z(1)
      sec2=dsqrt(x2*x2+y2*y2+z2*z2)
      x3=x(3)-x(2)
      y3=y(3)-y(2)
      z3=z(3)-z(2)
      sec3=dsqrt(x3*x3+y3*y3+z3*z3)

      if (sec2.lt.1.E-6 .or. sec3.lt.1.E-6) then
        ifl(2)=163
        goto 99
      endif

      den=sec2*sec3
      cosc=(x2*x3+y2*y3+z2*z3)/den
      sincsq=1.-cosc**2
      if (sincsq.le.0) then    
        ifl(2)=163
        goto 99
      endif

      sinc=dsqrt(sincsq)
      rad=chd/sinc
      h=dsqrt(rad*rad-chd*chd)

      xm = x(1)+xf/2.
      ym = y(1)+yf/2.
      zm = z(1)+zf/2.

      cosd = xr*(x(2)-xm) + yr*(y(2)-ym) + zr*(z(2)-zm)
      if (cosd .lt. 0.) then
        xr = -xr
        yr = -yr
        zr = -zr
      endif

      if (cosc .gt. 0.) h = -h

      xctr = xm + h*xr
      yctr = ym + h*yr
      zctr = zm + h*zr

c
c..... load 11 params in w   ( for putent )
c
70    continue
      w(11)=xr*x(1)+yr*y(1)+zr*z(1)
      w(1)=xctr
      w(2)=yctr
      w(3)=zctr
      w(4)=xu/secu
      w(5)=yu/secu
      w(6)=zu/secu
      w(7)=rad
      w(8)=xr
      w(9)=yr
      w(10)=zr

99    if (ifl(2).gt.0) goto 999
c
c..... radius min =.001           7-29-82
c
      if (w(7).lt..0009) ifl(2)=165

999   return
      end
