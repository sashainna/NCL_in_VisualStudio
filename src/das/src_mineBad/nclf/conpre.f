c**
C*********************************************************************
C*    NAME         :  conpre.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       conpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:43
C********************************************************************/
C
c **********************************************************************
c **********************************************************************
c **  program name: conpre                                            **
c **                                                                  **
c **  purpose of program:  convert the input data in xyzabc to be     **
c **         a 5 pt-vec special curve (subtyp=6) to crvpre            **
c **                  27-feb-89                                       **
c                                                                     **
c               4 conic definition kases are possible.                **
c                  kase=1   pvppv                                     **
c                       2   pvppp                                     **
c                       3   ppppv                                     **
c                       4   ppppp                                     **
c                                                                     **
c **********************************************************************
c **********************************************************************
 
      subroutine conpre (inv,nmp)
 
      include 'com8a.com'
 
      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxpn = 20)
      parameter (mxsfhd = (2+(maxpn+1)/2))

      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q,invx
c
      real*8 x(maxptx), y(maxptx), z(maxptx)
      real*4 a(maxptx), b(maxptx), c(maxptx)
      real*4 dx(maxptx), dy(maxptx), dz(maxptx)
      real*4 ch(maxptx), q(maxpt*36)

      integer*2 inv(20), invx(maxptx)

      small=1.e-6
      kase=isc10(3)
c               wrt cmat thru p1,pn plus 1 other pt
      n=nmp
c                       xc-axis
      qx=x(n)-x(1)
      qy=y(n)-y(1)
      qz=z(n)-z(1)
      chd=sqrt(qx**2+qy**2+qz**2)
      if(chd.lt.small)goto 98
      c1=qx/chd
      c2=qy/chd
      c3=qz/chd
c                pseudo yc-axis
      j=2
      if(nmp.gt.4)j=3
      qx=x(j)-x(1)
      qy=y(j)-y(1)
      qz=z(j)-z(1)
c                       zc-axis
            
      c9= c2*qz-c3*qy
      c10=c3*qx-c1*qz
      c11=c1*qy-c2*qx
      sec=sqrt(c9**2+c10**2+c11**2)
      if(sec.lt.small)goto 98
      c9=c9/sec
      c10=c10/sec
      c11=c11/sec
c                        yc-axis
      c5=c3*c10-c2*c11
      c6=c1*c11-c3*c9
      c7=c2*c9-c1*c10
c                         xyz orig in c-sys
      c4= -c1*x(1)-c2*y(1)-c3*z(1)
      c8= -c5*x(1)-c6*y(1)-c7*z(1)
      c12=-c9*x(1)-c10*y(1)-c11*z(1)
      c13=x(1)
      c14=y(1)
      c15=z(1)

c                     pt2 in c-sys
      x2=c1*x(2)+c2*y(2)+c3*z(2)+c4
      y2=c5*x(2)+c6*y(2)+c7*z(2)+c8
      z2=c9*x(2)+c10*y(2)+c11*z(2)+c12
      if(x2.eq.0.)x2=small
      if(x2.eq.chd)x2=chd-small
      xm=x2
      ym=y2
      if(y2.eq.0.)goto 98
      if(abs(z2).gt..002)goto 97

c                  bra on kase no.
      if(kase.gt.2)goto 20
      vax=c1*a(1)+c2*b(1)+c3*c(1)
      vay=c5*a(1)+c6*b(1)+c7*c(1)
      vaz=c9*a(1)+c10*b(1)+c11*c(1)
c                  vec1 must be in-plane and not parlel to chord
      if(abs(vaz).gt..002)goto 97
      if(abs(vay).lt..001)goto 98
      if(kase.gt.1)goto 20
c*************************************  kase=1     3pt2sl
      vbx=c1*a(3)+c2*b(3)+c3*c(3)
      vby=c5*a(3)+c6*b(3)+c7*c(3)
      vbz=c9*a(3)+c10*b(3)+c11*c(3)
c                   same for vec2
      if(abs(vbz).gt..002)goto 97
      if(abs(vby).lt..001)goto 98
      tan1=vax/vay
      tan2=vbx/vby
      den=tan1-tan2
      if(den.eq.0.)den=small
      yi=chd/den
      xi=tan1*yi
      goto 50
c                     rot pt3
20    x3=c1*x(3)+c2*y(3)+c3*z(3)+c4
      y3=c5*x(3)+c6*y(3)+c7*z(3)+c8
      z3=c9*x(3)+c10*y(3)+c11*z(3)+c12
      if(x3.eq.0.)x3=small
      if(x3.eq.chd)x3=chd-small
      if(y3.eq.0.)goto 98
      if(abs(z3).gt..002)goto 97
      if(kase-3)32,30,40
30    vbx=c1*a(4)+c2*b(4)+c3*c(4)
      vby=c5*a(4)+c6*b(4)+c7*c(4)
      vbz=c9*a(4)+c10*b(4)+c11*c(4)
      if(abs(vbz).gt..002)goto 97
      if(abs(vby).lt..001)goto 98
c*************************** kase 2 or 3   iters iln with vector
32    t12=y2/x2
      t13=y3/x3
      t25=y2/(chd-x2)
      t35=y3/(chd-x3)
      den=t13+t25
      if(den.eq.0.)goto 98
      xb=t25*chd/den
      yb=t13*xb
      den=t12+t35
      if(den.eq.0.)goto 98
      xt=t35*chd/den
      yt=xt*t12
      if(yt.eq.yb)goto 98
      tani=(xt-xb)/(yt-yb)
      coni=xb-tani*yb
c               intersect iln with vec1 or vec4
      if(kase.eq.3)goto 36
c                                 vec1
      t1=vax/vay
      if(t1.eq.tani)goto 98
      yi=coni/(t1-tani)
      xi=t1*yi
      goto 50
c                                  vec4
36    t1=vbx/vby
      if(tani.eq.t1)goto 98
      yi=(chd-coni)/(tani-t1)
      xi=coni+tani*yi
      goto 50
c********************* kase=5     rot p4 to c-sys and do iln
40    x4=c1*x(4)+c2*y(4)+c3*z(4)+c4
      y4=c5*x(4)+c6*y(4)+c7*z(4)+c8
      z4=c9*x(4)+c10*y(4)+c11*z(4)+c12
      if(x4.eq.0.)x4=small
      if(x4.eq.chd)x4=chd-small
      if(y4.eq.0.)goto 98
      if(abs(z4).gt..002)goto 97
c                                do leftln
      t12=y2/x2
      t13=y3/x3
      t25=y2/(chd-x2)
      t35=y3/(chd-x3)
      if(t13+t25.eq.0.)goto 98
      xb=t25*chd/(t13+t25)
      yb=t13*xb
      if(t12+t35.eq.0.)goto 98
      xt=t35*chd/(t12+t35)
      yt=xt*t12
      if(yt.eq.yb)goto 98
      tanl=(xt-xb)/(yt-yb)
      conl=xb-tanl*yb
c                                   now rgtlin
      t14=y4/x4
      t45=y4/(chd-x4)
      if(t14+t35.eq.0.)goto 98
      xb=t35*chd/(t14+t35)
      yb=xb*t14
      if(t13+t45.eq.0.)goto 98
      xt=chd*t45/(t13+t45)
      yt=t13*xt
      if(yt.eq.yb)goto 98
      tanr=(xt-xb)/(yt-yb)
      conr=xb-tanr*yb
      if(tanl.eq.tanr)goto 98
      yi=(conr-conl)/(tanl-tanr)
      xi=conl+tanl*yi
c                note:  use pt3 for rhoval in 5pt case
      xm=x3
      ym=y3
      
c                      set conv params and do unit conic
50    tal=(xi-chd/2.)/yi
      sec=sqrt(1.+tal**2)
      sal=tal/sec
      cal=1./sec
      xc3=chd*cal
c                       rhopt in unit sys
c                       unit conic eq is:  bb*v**2 + v + 2*(u**2-u)=0
      u=(cal*xm-sal*ym)/xc3
      v=ym/yi
      bb=(2.*(u-u**2)-v)/v**2
c                 further checks on interior pts.
c                  (must be within unit triangle)
      u2=(cal*x2-sal*y2)/xc3
      v2=y2/yi
      if(v2.le.0.)goto 98
      if(u2.le..5*v2.or.u2.ge.1.-.5*v2)goto 98
      if(nmp.lt.4)goto 59
      u3=(cal*x3-sal*y3)/xc3
      v3=y3/yi
      if(v3.le.0.)goto 98
      if(u3.le..5*v3.or.u3.ge.1.-.5*v3)goto 98
      if(nmp.lt.5)goto 59
      u4=(cal*x4-sal*y4)/xc3
      v4=y4/yi
      if(v4.le.0.)goto 98
      if(u4.le..5*v4.or.u4.ge.1.-.5*v4)goto 98
59    continue
c                       avoid bb=0 parab case
      if(abs(bb).lt..000001)bb=.000001
      bb8=8.*bb
      bb2=2.*bb
      ro=(sqrt(1.+bb2)-1.)/bb2
      rh=(1.-sqrt(2.-ro*2.))/(2.*ro-1.)
      rq=(1.-sqrt(2.-rh*2.))/(2.*rh-1.)
      u2=rh*(.5*ro-.25)+.25
      v2=.5*ro*(1.+rh)
c                    solve 5 ptvecs in unit sys.  convert to c-sys
c                     and trf to partsys.  this data is then input
c                     to crvpre to build the conic approximation cv.

70    du=.25
      au=-du
      do 80 i=1,5
      au=au+du
      u=au
      if(i.eq.2)u=u2
      if(i.eq.4)u=1.-u2
      v=(sqrt(1.+bb8*(u-u**2))-1.)/bb2
      dvdu=(1.-2.*u)/(bb*v+.5)
      yc=v*yi
      xc=u*chd+tal*yc
      vyc=yi*dvdu
      vxc=chd+vyc*tal
c                            now to partsys
      x(i)=c1*xc+c5*yc+c13
      y(i)=c2*xc+c6*yc+c14
      z(i)=c3*xc+c7*yc+c15
      a(i)=c1*vxc+c5*vyc
      b(i)=c2*vxc+c6*vyc
      c(i)=c3*vxc+c7*vyc
      inv(i)=1
80    continue
c                            pass rq in x(6) to crvpre
      x(6)=rq
      goto 99

c                       error exit.  input data is out-of-plane
97    ifl(2)=369
c                       catch-all error exit
98    if(ifl(2).le.0)ifl(2)=370

c                       normal exit
99    return
      end
