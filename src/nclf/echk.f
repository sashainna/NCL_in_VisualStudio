C*********************************************************************
C*    NAME         :  echk.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       echk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:00
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine echk
C           this routine checks the span from lastk to nextk this itcrv.
C            if intervening pts not intol, set allowable nextk.
C
C           method.  load pv's this itcrv in xyzabc for convenience.
C
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
      subroutine echk

      include 'com4a.com'

      parameter (maxpt=50)
      parameter (maxptx=1000)
      parameter (maxpn=20)
      parameter (maxcv=50)
      parameter (maxev=12)

      common/fitcom/scc(maxcv),r(maxcv*maxev*3),knv(maxcv)
     *  ,ncvs,lastk,nextk,nexk,itcrv
      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q,inv
      common/wblok/w(600)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx),dy(maxptx)
      real*4 dz(maxptx),ch(maxptx),q(maxcv*maxev*3),aw(1200),r
      real*8 x(maxptx),y(maxptx),z(maxptx),p(400),v(450),asn,w,
     1       scc,hj(36)
      integer*2 inv(maxptx),ksn(4),kscc(80)
      logical lv84,lv95
      equivalence (w,aw),(asn,ksn),(v,w(151)),(x,p),(jb,hj),(scc,kscc)

c
c... aak 16-OCT-97:
c... changed tolerance; the old one was: " tol=sc(27) ";
c... fixes srf construction when x1 is close to the middle of the conic
c
      lv84 = sc(169).lt.8.499d0
      lv95 = sc(169).lt.9.549d0
      if(lv84) then
         tol = sc(27)
      else
         tol = 1.e-6
      endif
c
c... load pv's this itcrv in xyzabc
c
      npts=nextk-lastk+1
      iqx=3*maxev*(lastk-1)+3*(itcrv-1)
c
c... retain this iqx for fut rgt sense
c
      kqx=iqx
      do 20 i=1,npts
         x(i)=q(iqx+1)
         y(i)=q(iqx+2)
         z(i)=q(iqx+3)
         a(i)=r(iqx+1)
         b(i)=r(iqx+2)
         c(i)=r(iqx+3)
         iqx=iqx+maxev*3
20    continue
c                   do rgt sense
      lqx=kqx+3
      if(itcrv.eq.maxev)lqx=kqx-3
      xr=q(lqx+1)
      yr=q(lqx+2)
      zr=q(lqx+3)
      xr=xr-x(1)
      yr=yr-y(1)
      zr=zr-z(1)
      if(itcrv.lt.maxev)goto 26
      xr=-xr
      yr=-yr
      zr=-zr
26    continue

c               write crv l thru n
      n=npts
      nexk=nextk
30    delx=x(n)-x(1)
      dely=y(n)-y(1)
      delz=z(n)-z(1)
      chd=sqrt(delx**2+dely**2+delz**2)
      if(chd.gt.0.) goto 32
      ifl(2)=163
      goto 99
32    cal=(delx*a(1)+dely*b(1)+delz*c(1))/chd
      cbe=(delx*a(n)+dely*b(n)+delz*c(n))/chd
      adis=.666667*chd/(1.+cal)
      bdis=.666667*chd/(1.+cbe)
      if(adis.gt.bdis)adis=bdis*(2.-bdis/adis)
      if(bdis.gt.adis)bdis=adis*(2.-adis/bdis)
      xj=x(1)+a(1)*adis
      yj=y(1)+b(1)*adis
      zj=z(1)+c(1)*adis
      xk=x(n)-bdis*a(n)
      yk=y(n)-bdis*b(n)
      zk=z(n)-bdis*c(n)

c             check intervening pts for intol
34    m=n-1
      do 70 i=2,m
      itim=0
      u=.5
      du=.01
      ctan=.5
36    ca=(1.-u)**2
      cb=2.*(1.-u)*u
      cc=u**2
      x1=ca*x(1)+cb*xj+cc*xk
      y1=ca*y(1)+cb*yj+cc*yk
      z1=ca*z(1)+cb*zj+cc*zk
      xd=ca*xj+cb*xk+cc*x(n)-x1
      yd=ca*yj+cb*yk+cc*y(n)-y1
      zd=ca*zj+cb*zk+cc*z(n)-z1
      uerr=(xd*(x(i)-x1)+yd*(y(i)-y1)+zd*(z(i)-z1))/
     * (xd**2+yd**2+zd**2)
      uerr=(uerr-u)/3.
      if(abs(uerr).lt.1.e-5)goto 60
      if(itim.ne.0) then
         if(abs(oerr-uerr).ge.1.e-5) ctan=du/(oerr-uerr)
         du = uerr*ctan
         if(u + du.gt.1.) du = 1.-u
         if(u + du.lt.0.)du  = -u
      endif
      itim=itim+1
      u=u+du
      oerr=uerr
      if(itim.lt.maxpt) goto 36
c
c... error: too many iters
c
      ifl(2)=51
      return

60    xup=yr*zd-zr*yd
      yup=zr*xd-xr*zd
      zup=xr*yd-yr*xd
      sec=sqrt(xup**2+yup**2+zup**2)
      perr=(xup*(x(i)-x1)+yup*(y(i)-y1)+zup*(z(i)-z1))/sec
      if(abs(perr).gt.tol)goto 80
c
c... aak 16-OCT-97:
c... added this line; fixes srf construction when x1 is
c... exactly in the middle of the conic
c
      if(.not.lv84.and. lv95 .and. abs(u-0.5).lt.1.e-5) goto 80
c
c...For evenly spaced odd number of curves
c...U will always hit .5 exactly at the middle curve
c...The above conditional causes these type of surfaces
c...to be parameterized incorrectly
c...(.5 from 1 to n-1 curve and .5 from n-1 to n curve)
c...Bobby - 3/7/07
c
      if (.not. lv84 .and. abs(u-.5) .lt. 1.e-5 .and. i .ne. (n+1)/2)
     1    goto 80
70    continue
c                all interior pts intol
      return
c                bump nexk down
80    nexk=nexk-1
      n=n-1
      if(n.gt.2)goto 30
99    return
      end

