c*****************************************************
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       crvlen.f , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 15:09:45
c**
c*****************************************************
      subroutine crvlen (asw, npts, clen, ierr)
 
      include 'com.com'

      real*8 asw, clen
      integer*2 npts, ierr
 
      real*8 a(10),x(4),y(4),z(4)
      integer*4 nclkey
      integer*2 i, nu, nseg, iseg, imid
      integer*2 ietype,nwds, iwf
      real*8 xx,yy,zz, ox,oy,oz, xm,ym,zm, dx,dy,dz, u, tu, du, um, hdu
      real*8 v, d1,d2,d3,d4, den, delx,dely,delz, sdel, ro
      real*4 a4(20)
      equivalence (a,a4)

      call gtdesc(asw,nclkey,nwds,ietype)
      call isitwf (nclkey, iwf)
      if (iwf.eq.1) then
        call wfcvln (nclkey, sc(27), clen, ierr)
        if (ierr.ne.0) ierr = 383
        goto 999
      endif

      if (ietype.eq.LINE) then
        call lnlnth (nclkey, clen, ierr)
        if (ierr.ne.0) ierr = 383
        goto 999
      endif

      if (ietype.eq.CIRCLE) then
        call cilnth (nclkey, clen, ierr)
        if (ierr.ne.0) ierr = 383
        goto 999
      endif
c
c...Use tolerance routine for NCL V9.0+
c
      if (sc(169) .gt. 8.499) then
        call wfcvln (nclkey, sc(27), clen, ierr)
        if (ierr.ne.0) ierr = 383
        goto 999
      endif
c
c...ncl curve in versions => 8.4 can be trimmed 
c
      if (sc(169) .gt. 8.399d0) then
         call ncl_ncrv_len (nclkey,clen)
         if (ifl(264) .eq. 1) clen = clen*25.4
         goto 999
      end if

10    clen=0.
      nu=npts
      if (nu.lt.10) nu=10
      if (nu.gt.999) nu=999

      call gtncsg (nclkey, nseg)

      call gtcseg(nclkey,1,a)
      x(1)=a(1)
      y(1)=a(2)
      z(1)=a(3)
      x(2)=x(1)+a4(7)
      y(2)=y(1)+a4(8)
      z(2)=z(1)+a4(9)
      ox=x(1)
      oy=y(1)
      oz=z(1)
      ro=a4(12)

      do 100 iseg=2,nseg
      call gtcseg(nclkey,iseg,a)
      x(4)=a(1)
      y(4)=a(2)
      z(4)=a(3)
      x(3)=x(4)-ro*a4(7)
      y(3)=y(4)-ro*a4(8)
      z(3)=z(4)-ro*a4(9)

      u=0.
      tu=nu-1
      du=1.d0/tu
      hdu=du/2.d0
      i=1
30    imid=1
      i=i+1
40    u=u+hdu
      um=1.-u
      d1=um**3
      d2=3.*u*um**2
      d3=3.*u**2*um
      d4=u**3
      xx=d1*x(1)+d2*x(2)+d3*x(3)+d4*x(4)
      yy=d1*y(1)+d2*y(2)+d3*y(3)+d4*y(4)
      zz=d1*z(1)+d2*z(2)+d3*z(3)+d4*z(4)
      if(imid.eq.0)goto 50
c        hold this midpt
      xm=xx
      ym=yy
      zm=zz
      imid=0
      goto 40
c               do this s amt and add to sum
50    dx=xx-ox
      dy=yy-oy
      dz=zz-oz
      den=dx**2+dy**2+dz**2
      v=(dx*(xm-ox)+dy*(ym-oy)+dz*(zm-oz))/den
      delx=ox+v*dx-xm
      dely=oy+v*dy-ym
      delz=oz+v*dz-zm
      sdel=dsqrt(dx**2+dy**2+dz**2+5.3334d0*(delx**2+dely**2+delz**2))
      clen=clen+sdel
      ox=xx
      oy=yy
      oz=zz
      if(i.lt.nu)goto 30

      x(1)=a(1)
      y(1)=a(2)
      z(1)=a(3)
      x(2)=x(1)+a4(7)
      y(2)=y(1)+a4(8)
      z(2)=z(1)+a4(9)
      ro=a4(12)

100   continue

999   return
      end
