C*********************************************************************
C*    NAME         :  dsrelx.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       dsrelx.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:00
C*********************************************************************
C **********************************************************************
C **  PROGRAM NAME: DSRELX
C **
C **  PURPOSE OF PROGRAM: CALL DSREL WITH DIFFERENT HEIGHTS IF GOUGCK 
C **                      IS ON AND DS IS A SURFACE.
C **
C **********************************************************************
C **********************************************************************
 
      subroutine dsrelx (ldsgck,tol)
 
      include 'com.com'
      include 'mocom.com'
      include 'drvcom.com'

      logical ldsgck
      real*4 tol

      integer*2 jd(600),ia,ic
      equivalence(d,jd)
      equivalence(ifl(51),ia),(ifl(53),ic)
      integer*2 isftyp
      equivalence (ksd(329),isftyp)

      logical lv92,lv95,lgck,lmod,ltry1
      integer*2 ier0,ktyp0,ktyp1
      real*8 dst0,dst1,dtol
      real*4 ds0(10),fv(3),ta(3),spt(3),vpos(3),xv(3)
      real*4 h0,h1,u0,v0,usav,vsav,u,v
      real*4 ti,tj,tk,fx,fy,fz,x1,y1,cute,crad
      real*4 rx,ry,rz,dx,dy,dz,sec,thk,dis,hsav,h,co,si,dh

      lv92 = sc(169).lt.9.249d0
      lv95 = sc(169).lt.9.549d0

      crad = tool(2)
      cute = tool(6)

      i = jd(201)
      if (i .ne. 9) goto 100
      hsav = t(21,ia)
      if (.not.ldsgck .and. hsav .le. crad+tol) goto 100

      lmod = (lmdnow.or.lcvgid).and.ifl(21).ne.0.and.cute.gt.tol
      lgck = (.not.lv92 .and. idsgck.ge.2 .and. ifl(21).ne.0 .and.
     *          cute.ge.2.*tol .and. (.not.lmod .or. ifl(57).ne.0))

      if (.not.lgck) goto 100

      dtol = sc(27)
      usav = t(19,ia)
      vsav = t(20,ia)
      imodsav = 0
      if (ifl(57).gt.0) imodsav = 1

      call dsrel
      if (ifl(2) .gt. 0) return

      ltry1 = .true.
      if (hsav .gt. crad+tol) then
        h1 = crad
      else if (ldsgck .and. hsav .lt. crad - tol) then
        h1 = crad
      else if (ldsgck .and. tool(5) .gt. hsav + tol) then
        h1 = tool(5)
      else
        ltry1 = .false.
      endif

      if (ltry1) then
        call dsdis (ktyp0,dst0,dtol)
        call conv4_4 (s(1,2),ds0,10)
        h0 = t(21,ia)
        u0 = t(19,ia)
        v0 = t(20,ia)
        ier0 = ifl(2)
        iamod0 = ifl(57)

        t(21,ia) = h1
        t(19,ia) = usav
        t(20,ia) = vsav
        call dsrel

        call dsdis (ktyp1,dst1,dtol)

        if (ktyp1 .gt. ktyp0 .or.
     x     (ktyp1.eq.ktyp0 .and. dst1.ge.dst0-dtol)) then
          call conv4_4 (ds0,s(1,2),10)
          t(21,ia) = h0
          t(19,ia) = u0
          t(20,ia) = v0
          ifl(2) = ier0
          ifl(57) = iamod0
        endif
      endif

      if (ifl(2) .gt. 0) return

      if (ldsgck .and. isftyp.ne.27 .and. crad.gt.2*tol) then
        thk = sc(24)

        call conv4_4 (s(1,2),ds0,10)
        h0 = t(21,ia)
        u0 = t(19,ia)
        v0 = t(20,ia)
        ier0 = ifl(2)
        iamod0 = ifl(57)

c     if ta/normal,ps  use same for ijk
        if (ifl(23).eq.1) then
          ti=s(1,1)
          tj=s(2,1)
          tk=s(3,1)
        else
          ti=t(4,ia)
          tj=t(5,ia)
          tk=t(6,ia)
        endif
        ta(1) = ti
        ta(2) = tj
        ta(3) = tk
        fx = tj*s(3,2)-tk*s(2,2)
        fy = tk*s(1,2)-ti*s(3,2)
        fz = ti*s(2,2)-tj*s(1,2)
        sec = sqrt(fx*fx+fy*fy+fz*fz)
        if (sec.le.1.e-3) goto 999
        if (lv95) then
        if (fx*t(7,ia) + fy*t(8,ia) + fz*t(9,ia) .lt. 0.) sec = -sec
        else
        if (fx*t(7,ic) + fy*t(8,ic) + fz*t(9,ic) .lt. 0.) sec = -sec
        endif
        fx = fx/sec
        fy = fy/sec
        fz = fz/sec

        rx = tj*fz - tk*fy
        ry = tk*fx - ti*fz
        rz = ti*fy - tj*fx
        sec = sqrt(rx*rx+ry*ry+rz*rz)
        if (sec.le.1.e-3) goto 999
        dx=s(8,2)-s(5,2)
        dy=s(9,2)-s(6,2)
        dz=s(10,2)-s(7,2)
        if (rx*dx + ry*dy + rz*dz .gt. 0.) sec = -sec
        rx = rx/sec
        ry = ry/sec
        rz = rz/sec
 
        s(8,2) = t(1,ia) + crad*ti + cute*rx
        s(9,2) = t(2,ia) + crad*tj + cute*ry
        s(10,2) = t(3,ia) + crad*tk + cute*rz

        u = usav
        v = vsav
        call surfpn(u,v,1)

        if (ifl(2) .gt. 0) goto 40

        call vctovc4(s(5,2),spt)
        call vcmnvc4(spt,t(1,ia),fv)

        y1 = f_dot4(ta,fv)
        x1 = sqrt (f_dot4(fv,fv) - y1*y1)
c
c..... x1,y1 are spt coordinates in the plane through the tool axis and spt.
c
        if (y1 .lt. -tol .or. y1 .ge. crad + tol) goto 40
        if (x1 .lt. cute - tol) goto 40

        call uvcplvc4(fv,ta,vpos,-y1)
        call unitvc4(vpos,vpos)
        call avcplbvc4 (crad,ta,cute,vpos,fv)
        call vcplvc4(t(1,ia),fv,xv)
c
c..... xv is the tool ring point closest to spt
c
        call vcmnvc4(spt,xv,fv)
        dis = f_mag4(fv) - crad

        if (dis .lt. thk + tol) then
          dh = 0
          co = f_dot4(ta,s(1,2))
          co = abs(co)
          if (co .gt. 1.e-3) then
            si = 1 - co*co
            if (si .gt. 1.e-6) then
              si = sqrt(si)
              dh = cute*co/si
              h = crad + dh
              s(8,2)=t(1,ia)+ta(1)*h
              s(9,2)=t(2,ia)+ta(2)*h
              s(10,2)=t(3,ia)+ta(3)*h
            endif
          endif

          t(21,ia) = crad + dh
          t(19,ia) = u
          t(20,ia) = v
          ifl(57) = imodsav
          goto 999
        endif

40      continue
        call conv4_4 (ds0,s(1,2),10)
        t(21,ia) = h0
        t(19,ia) = u0
        t(20,ia) = v0
        ifl(2) = ier0
        ifl(57) = iamod0

      endif

      goto 999

100   call dsrel

999   return
      end
