C*********************************************************************
C*    NAME         :  segchk.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       segchk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:39
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine segchk(xc,yc,zc,i,k,itsk)
c*          build a bezcub pt(i) to pt(k)
c*          itsk=1     return xc,yc,zc to calling routine
c*          itsk=2     study all intermediate pts  (p/o temp)

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
      subroutine segchk(xc,yc,zc,i,k,itsk)

      include 'com4a.com'

      parameter (maxpt = 50)
      parameter (maxptx = 1000)

      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch

      real*8 x(maxptx),y(maxptx),z(maxptx),asn
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx)
      real*4 dy(maxptx),dz(maxptx),ch(maxptx)
      integer*2 ksn(4)
      equivalence(asn,ksn)

      if(k.le.maxpt)goto 6
      ifl(2)=43
      goto 99
6     ctan=1.
      hdx=0.
      hdy=0.
      hdz=0.
      j=i+1
      delx=x(k)-x(i)
      dely=y(k)-y(i)
      delz=z(k)-z(i)
      cc=sqrt(delx**2+dely**2+delz**2)
      cal=(delx*a(i)+dely*b(i)+delz*c(i))/cc
      cbe=(delx*a(k)+dely*b(k)+delz*c(k))/cc
      adis=.666667*cc/(1.+cal)
      bdis=.666667*cc/(1.+cbe)
      if(adis.gt.bdis)adis=bdis*(2.-bdis/adis)
      if(bdis.gt.adis)bdis=adis*(2.-adis/bdis)
      xq=x(i)+a(i)*adis
      yq=y(i)+b(i)*adis
      zq=z(i)+c(i)*adis
      xr=x(k)-a(k)*bdis
      yr=y(k)-b(k)*bdis
      zr=z(k)-c(k)*bdis
30    itim=0
      u=.5
40    c1=(1.-u)**2
      c2=(1.-u)*u*2.
      c3=u**2
      itim=itim+1
      if(itim.gt.10)goto 60
      xa=c1*x(i)+c2*xq+c3*xr
      ya=c1*y(i)+c2*yq+c3*yr
      za=c1*z(i)+c2*zq+c3*zr
c              ptb deltas
      xb=c1*xq+c2*xr+c3*x(k)-xa
      yb=c1*yq+c2*yr+c3*y(k)-ya
      zb=c1*zq+c2*zr+c3*z(k)-za

      uerr=(xb*(x(j)-xa)+yb*(y(j)-ya)+zb*(z(j)-za))/
     1 (xb**2+yb**2+zb**2)-u
      uerr=uerr/3.
      if(abs(uerr).lt.1.e-5)goto 60
      if(itim.eq.1)goto 45
      den=oerr-uerr
      if(abs(den).lt.1.e-5)goto 45
      ctan=du/den
      if(ctan.lt..1)ctan=1.
45    du=uerr*ctan

50    if(du+u.gt.1.)du=1.-u
      if(du+u.lt.0.)du=-u
      u=u+du
      oerr=uerr
      goto 40

60    xc=xa+u*xb
      yc=ya+u*yb
      zc=za+u*zb
      if(itsk.eq.1)goto 99
      dlx=xc-x(j)
      dly=yc-y(j)
      dlz=zc-z(j)
      if(abs(dlx).gt.abs(hdx))hdx=dlx
      if(abs(dly).gt.abs(hdy))hdy=dly
      if(abs(dlz).gt.abs(hdz))hdz=dlz

      j=j+1
      if(j.lt.k)goto 30
c          itsk 2.  put deltas in xc,yc,zc
      xc=hdx
      yc=hdy
      zc=hdz

99    return
      end


