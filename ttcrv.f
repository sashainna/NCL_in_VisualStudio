C*********************************************************************
C*    NAME         :  ttcrv.f
C*       CONTAINS:
C*      subroutine ttcrv
C*      subroutine ttcsrl(dis)
C*      subroutine lnbox(buf, box)
C*      subroutine cibox(buf, box)
C*    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       ttcrv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:49
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ttcrv
C*      Initialize driving TANTO a curve check surface.
C*    PARAMETERS   
C*       INPUT  : 
C*          dsasw        - Associated word of first (drive) curve.
C*          csasw        - Associated word of second (check) curve.
C*       OUTPUT :  
C*          ss(1-3)      - Vector at point of tangency.
C*          ss(4)        - Constant of plane thru point of tangency perp
C*                         to vector at point of tangency.
C*          ss(5-7)      - Point of tangency.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine ttcrv (dsasw, csasw, ss)

      include 'com.com'
      include 'mocom.com'

      real*8 dsasw, csasw, ss(7),
     *       cos_tol,tol, ptol, co, hco, dir, dpmax, d1, d2,
     *       f_dist, f_mag, f_dot, zero/0.d0/, one/1.d0/,
     *       p(3), v(3), p1(3), v1(3), v2(3), vcv(3),
     *       u8, hu8, du, hdu, ust, cvlen, dumin,umin,umax,
     *       box1(6), box2(6), boxo(6), hldt(30)

      real*4 px,py,pz,vx,vy,vz

      integer*4 keycv1, keycv2, ispt, ipt, npts

      integer*2 ierr, irslt, i2pt, nwds, ietyp1, ietyp2,
     *          iend, ia, ib, ic, isrf, izro/0/

      real*4 srf(10,3)
      equivalence (srf,s)
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      d1   = one
      umin = zero
      umax = one

      npts = (icsgck+1)*50
      tol = sc(27)
      ptol = sc(168)
      dpmax = tol*100.d0
c
c... aak  09-12-97 : 
c... make the tolerance for the CS&DS to be tangent at the contact pt. to 
c... depend on global tolerance; tool(1) = tool diameter
c
      if (tool(1) .le. tol) then
          cos_tol = dabs(one-tol)
      else
          cos_tol = dabs(one - tol/tool(1))
      endif
      call gtdesc (dsasw, keycv1, nwds, ietyp1)
      call gtdesc (csasw, keycv2, nwds, ietyp2)
      isrf = 2
      ia = 3
      call conv4_8(t(1,ia),hldt,30)
      t(4,ia)  = zero
      t(5,ia)  = zero
      t(6,ia)  = zero
      t(21,ia) = zero
      t(27,ia) = zero
c
c...   Project tool end point onto drive curve (PREMOV put tool data in tcol 3)
c
      call acrvpv (px,py,pz,vx,vy,vz)
      if (ifl(2) .eq. 466) go to 999
C
C...   Determine which direction we are heading along the drive curve.
C
      dir = one
      if (t(7,3)*vx+t(8,3)*vy+t(9,3)*vz .lt. zero) dir = -one
C
C...   Calculate & intersect curve boxes.
C
      call cvbox (keycv1, box1, ierr)
         if (ierr.ne.0) goto 9172
      call cvbox (keycv2, box2, ierr)
         if (ierr.ne.0) goto 9172
      call cvxbox (box1, box2, irslt, boxo)
         if (irslt.eq.0) goto 9172
      u8 = t(19,ia)
      du = npts
      if (dir .lt.zero) then
        du = -u8/du
      else
        du = (one - u8)/du
      endif
c
c...   Move along drive curve from the current point until we find a
c...   point in the intersection box.
c
      ispt = 0
120   if (ispt.gt.npts) goto 9172
      call evstup (keycv1, isrf)
      call uevcvt (u8, isrf, izro, p, v, ifl(2))
      if (ifl(2) .eq. 466) go to 999

      do 150 ipt=ispt,npts

         call ptinbx (p, boxo, irslt)
         if (irslt.eq.1) goto 160
         if (ipt .lt. npts) then
            u8 = u8+du
            if (u8.lt.zero) u8 = zero
            if (u8.gt.one)  u8 = one
            call uevcvt (u8, isrf, izro, p, v, ifl(2))
            if (ifl(2) .eq. 466) go to 999
         endif

150   continue
C
C...   Did not find a point in the intersection box for some reason.
C...   Start at the beginning or end of the curve.
C
      if (dir .gt. zero) then
        u8 = umin
      else
        u8 = umax
      endif
      ipt = ispt

160   continue
C
C...   If first point is in box start from it, otherwise start iterating
C...   from last point outside box.
C
      if (ipt.gt.ispt) then
         ipt = ipt-1
         ust = u8-du
         if (ust.lt.zero) ust = zero
         if (ust.gt.one)  ust = one
         u8 = ust
      else
         ust = u8
      endif

      i2pt = 100
      call crvlen (sc(11), i2pt, cvlen, ifl(2))
      if (ifl(2).gt.0) goto 999
      if (dir .lt. zero) then
        du = ust
      else
        du = (one - ust)
      endif

      dumin = tol/cvlen
      npts = du/dumin/10.0d0
      if (npts.lt.8) npts = 8
      du = npts
      hdu = one/du
      du = hdu * dir
      npts = npts*10

      ipt = 0
      u8 = ust-du
c
c..... if at an end, step back on an extension, this way the
c..... current endpoint could be accepted: Dassault MFGNC254
c
      if (sc(169).ge.9.149 .and. u8 .gt. umax - 1.d-4) then
        call uevcvt (umax, isrf, izro, p1, v1, ifl(2))
        if (ifl(2) .eq. 466) go to 999
        call uvcplvc (p1,v1,p1,-du)
      else if (sc(169).ge.9.149 .and. u8 .lt. umin + 1.d-4) then
        call uevcvt (umin, isrf, izro, p1, v1, ifl(2))
        if (ifl(2) .eq. 466) go to 999
        call uvcplvc (p1,v1,p1,-du)
      else
        call uevcvt (u8, isrf, izro, p1, v1, ifl(2))
        if (ifl(2) .eq. 466) go to 999
      endif

180   continue
         ipt=ipt+1
c
c...break loop if curve end
c
         if (d1.eq.zero .and. (u8.eq.umax.and.du.gt.zero .or.
     *                u8.eq.umin.and.du.lt.zero)) go to 9172
         if (ipt.gt.npts) goto 9172

         u8 = u8+du
         if (u8.gt.umax) u8 = umax
         if (u8.lt.umin) u8 = umin
         isrf = 2
         call uevcvt (u8, isrf, izro, p, v, ifl(2))
            if (ifl(2) .eq. 466) go to 999
         call unitizevc(v)
         d1= f_dist(p,p1)
         call conv8_4(p,t(1,ia),3)
         isrf = 3
         call acrvpv (px,py,pz, vx,vy,vz)
         if (ifl(2).gt.0) goto 999
c aak:
         vcv(1)=vx
         vcv(2)=vy
         vcv(3)=vz
         
         v2(1) = px-t(1,ia)
         v2(2) = py-t(2,ia)
         v2(3) = pz-t(3,ia)
         d2 = f_mag(v2)
         co = dabs(f_dot(v,vcv))
         call vctovc(p,p1)
         call vctovc(v,v1)
c
c... aak 09-12-97: changed tolerance here; old way was:
c... if (d2.lt.ptol .and. co.gt.0.99999d0) goto 300
c
         if (d2.lt.ptol .and. co.gt.cos_tol) goto 300
         if (d1.gt.d2) goto 200
         if (d1.lt.d2/4.0d0 .and. dabs(du).lt.hdu/2.0d0) du = du*2.0d0
      goto 180

200   continue
      if (d2.gt.ptol .and. dabs(du).gt.dumin) then
        du = du/4.0d0
        if (dabs(du).lt.dumin) du = dumin*dir
        u8 = u8-du
        isrf = 2
        call uevcvt (u8, isrf, izro, p1, v1, ifl(2))
        if (ifl(2) .eq. 466) go to 999
      endif
      goto 180

300   continue
      hco = co
      hu8 = u8
      umin = u8 - 2.*dir*du
      umax = u8 + 2.*dir*du
      if (umin .lt. zero) umin = zero
      if (umax .gt. one) umax = one
      u8 = u8 - 2.0d0*du
c
c...refine point using smaller du
c
      du = du/4.0d0
      iend  = 0
      if (co .gt. .99999999) iend = dir

      do 320 ipt =1,64
         u8 = u8+du
         if (u8.gt.one) u8 = one
         if (u8.ge.umax) iend = 1
         if (u8.lt.zero) u8 = zero
         if (u8.le.umin) iend = -1
         isrf = 2
         call uevcvt (u8, isrf, izro, p, v, ifl(2))
         if (ifl(2) .eq. 466) go to 999
         call unitizevc(v)
         call conv8_4(p,t(1,ia),3)
         isrf = 3
         call acrvpv (px,py,pz, vx,vy,vz)
         if (ifl(2).gt.0) goto 999
         v2(1) = px-t(1,ia)
         v2(2) = py-t(2,ia)
         v2(3) = pz-t(3,ia)
         d2 = f_mag(v2)
         co = dabs(v(1)*vx+v(2)*vy+v(3)*vz)
         if (d2.lt.ptol) then
            if (co.gt.hco) then
               call vctovc(p,p1)
               call vctovc(v,v1)
               hco = co
               hu8 = u8
               if(co.gt. 0.99999999d0) iend = dir
            endif
         endif
         if (iend*dir .eq. 1) go to 500
320   continue

500   continue
C
C...   Create a plane at the end of the drive curve to be the check surface.
C...   (Additional checks are done in check routine to make sure we reach
C...   the end of the curve before stopping.)
C
      call vctmsc(v1,ss,dir)
      ss(4) = f_dot(p1,v1)*dir
      call vctovc(p1,ss(5))

      call conv8_4(hldt,t(1,ia),30)
      lttcrv = .true.

999   continue  
      return

c
c... error - invalid type for tanto ending
c
9172  ifl(2) = 172
      goto 999
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine ttcsrl
C*      Calculate distance to check surface when driving tanto a curve.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          dis       - Distance to check surface.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine ttcsrl(dis)

      include 'com.com'
      include 'mocom.com'
c      include 'com4.com'

      real*8 dis

      real*4 srf(10,3)
      equivalence (srf,s)

      real*8 xe,ye,ze, ti,tj,tk, rx,ry,rz, xp,yp,zp, h, rgt, co
      real*8 fx,fy,fz, sec, sfa,sfb,sfc,thk
      integer*2 ia, ib, ic, isrf, iptk, itfl
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence (itfl,ifl(49)),(iptk,ifl(50))

      h = t(21,ia)
      ti = t(4,ia)
      tj = t(5,ia)
      tk = t(6,ia)
      fx = t(7,ia)
      fy = t(8,ia)
      fz = t(9,ia)
      sfa = srf(1,3)
      sfb = srf(2,3)
      sfc = srf(3,3)

      xe = t(1,ia)+h*ti
      ye = t(2,ia)+h*tj
      ze = t(3,ia)+h*tk
      xp = srf(5,3)
      yp = srf(6,3)
      zp = srf(7,3)
      h  = (xp-xe)*ti+(yp-ye)*tj+(zp-ze)*tk
      xe = xe+h*ti
      ye = ye+h*tj
      ze = ze+h*tk
c
c..... added DS thick into consideration: Dassault MFGNC253
c
      if (sc(169) .lt. 9.149d0) then
        thk = 0.
      else
        thk = sc(24)
      endif
C
C...   If driving TLRGT or TLLFT, offset ending point
C
      if (ifl(21).ne.0) then
        rx = sfb*tk-sfc*tj
        ry = sfc*ti-sfa*tk
        rz = sfa*tj-sfb*ti
        sec = dsqrt(rx**2+ry**2+rz**2)
        if (sec.eq.0.0) sec = 1.0
        rgt = ifl(21) * ((tool(1)/2.0d0 + thk)/sec)
        xp = xp+rgt*rx
        yp = yp+rgt*ry
        zp = zp+rgt*rz
      endif
C
C...   Calculate distance from TE to curve tangent pt
C
      dis = dsqrt((xp-xe)**2+(yp-ye)**2+(zp-ze)**2)
C
C...   If this is the first motion point and we are very close to the check
C...   plane, exit with a negative distance so mover will make first step
C
c..... but not in a GO statement (possible only in OML, since NCL does not
c..... allow tanto conditions in a GO): Dassault MFGNC254
c
      if ((ifl(280).eq.0 .or. sc(169).lt.9.149d0) .and. 
     *  iptk.eq.0 .and. dis.lt.sc(27)*10.) then
        dis = -dis
        goto 200
      endif
C
C...   If we are not close to the end point, exit with positive distance so
C...   mover knows we are still heading towards it.
C
      if (itfl.eq.0 .and. dis.gt.2.0*t(10,ia)) goto 200
C
C...   If we are close to the tangent point on the drive curve
C...   and heading towards it, calculate distance to check plane.
C
      co = sfa*fx+sfb*fy+sfc*fz
      if (co.lt.0.0) goto 200
      rx = fy*tk-fz*tj
      ry = fz*ti-fx*tk
      rz = fx*tj-fy*ti
      fx = tj*rz-tk*ry
      fy = tk*rx-ti*rx
      fz = ti*ry-tj*rz
      sec = dsqrt(fx**2+fy**2+fz**2)
      if (sec.eq.0.0d0) sec = 1.0d0
      fx = fx/sec
      fy = fy/sec
      fz = fz/sec
      dis = (srf(5,3)-xe)*fx+(srf(6,3)-ye)*fy+(srf(7,3)-ze)*fz

200   t(12,ia) = dis

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine cibox (buf, u, p, v)
C*      Calculate a limit box around a circle.
C*    PARAMETERS   
C*       INPUT  : 
C*          buf       - Circle data.
C*       OUTPUT :  
C*          box       - Box.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine cibox(buf, box)

      include 'com.com'

      real*8 buf(11), box(6)

      real*8 vx,vy,vz, sx,sy,sz, ex,ey,ez, rx,ry,rz
      real*8 xx1,xy1,xz1, xx2,xy2,xz2, yx1,yy1,yz1, yx2,yy2,yz2
      real*8 zx1,zy1,zz1, zx2,zy2,zz2
      real*8 dis, h,hsq, xs,xl,ys,yl,zs,zl
      real*8 cx,cy,cz, ax,ay,az, rd, pi,pj,pk,pd
c
c...   Move circle center, axis, radius & limit plane into local variables.
c
      cx = buf(1)
      cy = buf(2)
      cz = buf(3)
      ax = buf(4)
      ay = buf(5)
      az = buf(6)
      rd = buf(7)
      pi = buf(8)
      pj = buf(9)
      pk = buf(10)
      pd = buf(11)
c
c...   Project X-axis onto plane of circle by crossing circle axis with X-axis
c...   and crossing reultant vector with circle axis. Following commented code
c...   simplifies as below
c
c      rx = ay*0.0-az*0.0 = 0.0
c      ry = az*1.0-ax*0.0 =  az
c      rz = ax*0.0-ay*1.0 = -ay
c      vx = ry*az-rz*ay
c      vy = rz*ax-rx*az
c      vz = rx*ay-ry*ax
      vx = az**2 + ay**2
      vy = -ay*ax
      vz = -az*ax
      dis = dsqrt(vx**2+vy**2+vz**2)
      if (dis.gt.0.d0) then
        dis = rd/dis
        if (vx.lt.0.0) dis=-dis
        vx = vx*dis
        vy = vy*dis
        vz = vz*dis
      endif
c
c...   Calculate xsmall & xlarge point on circle
c
      xx1 = cx-vx
      xx2 = cx+vx
      xy1 = cy-vy
      xy2 = cy+vy
      xz1 = cz-vz
      xz2 = cz+vz
c
c...   Same for Y
c
      vx = -ax*ay
      vy = ax**2+az**2
      vz = -az*ay
      dis = dsqrt(vx**2+vy**2+vz**2)
      if (dis.gt.0.d0) then
        dis = rd/dis
        if (vy.lt.0.0) dis=-dis
        vx = vx*dis
        vy = vy*dis
        vz = vz*dis
      endif
      yx1 = cx-vx
      yx2 = cx+vx
      yy1 = cy-vy
      yy2 = cy+vy
      yz1 = cz-vz
      yz2 = cz+vz

c
c...   Same for Z
c
      vx = -ax*az
      vy = -ay*az
      vz = ay**2+ax**2
      dis = dsqrt(vx**2+vy**2+vz**2)
      if (dis.gt.0.d0) then
        dis = rd/dis
        if (vz.lt.0.0) dis=-dis
        vx = vx*dis
        vy = vy*dis
        vz = vz*dis
      endif
      zx1 = cx-vx
      zx2 = cx+vx
      zy1 = cy-vy
      zy2 = cy+vy
      zz1 = cz-vz
      zz2 = cz+vz

      if (dabs(pi).gt..01.or.dabs(pj).gt..01 .or.dabs(pk).gt..01) then
c
c...   Circle is bounded, calculate end points
c
        rx=ay*pk-az*pj
        ry=az*pi-ax*pk
        rz=ax*pj-ay*pi
        dis=pd-pi*cx-pj*cy-pk*cz
        hsq=rd**2-dis**2
        h=0.
        if(hsq.gt.0.)h=dsqrt(hsq)
        sx=cx+pi*dis-h*rx
        sy=cy+pj*dis-h*ry
        sz=cz+pk*dis-h*rz
        ex=sx+2.*h*rx
        ey=sy+2.*h*ry
        ez=sz+2.*h*rz
        xs = sx
        if (ex.lt.xs) xs=ex
        ys = sy
        if (ey.lt.ys) ys=ey
        zs = sz
        if (ez.lt.zs) zs=ez
        xl = sx
        if (ex.gt.xl) xl=ex
        yl = sy
        if (ey.gt.yl) yl=ey
        zl = sz
        if (ez.gt.zl) zl=ez
c
c...   Determine if box points are on real part of circle
c
        if (pi*xx1+pj*xy1+pk*xz1-pd .lt. 0.0) xx1 = xs
        if (pi*xx2+pj*xy2+pk*xz2-pd .lt. 0.0) xx2 = xl
        if (pi*yx1+pj*yy1+pk*yz1-pd .lt. 0.0) yy1 = ys
        if (pi*yx2+pj*yy2+pk*yz2-pd .lt. 0.0) yy2 = yl
        if (pi*zx1+pj*zy1+pk*zz1-pd .lt. 0.0) zz1 = zs
        if (pi*zx2+pj*zy2+pk*zz2-pd .lt. 0.0) zz2 = zl

      endif

      box(1) = xx1
      box(4) = xx2
      box(2) = yy1
      box(5) = yy2
      box(3) = zz1
      box(6) = zz2

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine acrvpv (px,py,pz, vx,vy,vz)
C*      Project a point onto any curve.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          pxyz      - Point on curve.
C*          vxyz      - Slope of curve at pxyz.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine acrvpv(px,py,pz,vx,vy,vz)

      include 'com.com'
      include 'mocom.com'

      real*4 px,py,pz,vx,vy,vz

      integer*2 ia, ib, ic, isrf
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      real*8 u, du, dsq, svdsq, spx, spy, spz, svx, svy, svz, svu
      real*8 xe, ye, ze
      integer*4 nclkey
      integer*2 idx, nwds, ietype, itim, ntim, iux

      itim = 0

      if (isrf .eq. 1) then
        ntim = ifl(6)*8+1
      else if (isrf .eq. 2) then
        ntim = idsgck*8+1
      else if (isrf .eq. 3) then
        ntim = icsgck*8+1
      else
        ifl(2) = 466
        goto 999
      endif

      if (ntim .le. 1) then
        du = 1.
      else
        du = ntim-1
        du = 1.0d0/du
      endif
      u = 0.0d0
      svdsq = 1.e9
      xe = t(1,ia)
      ye = t(2,ia)
      ze = t(3,ia)
      iux = isrf*6+7
      idx = 50*(isrf-1)
      call gtdesc (d(idx+2), nclkey,nwds,ietype)
100   continue
      if (ietype.eq.LINE) then
        call lnpv (px,py,pz,vx,vy,vz)
      else if (ietype.eq.CIRCLE) then
        call circpv (px,py,pz,vx,vy,vz)
      else
        call curvpv (px,py,pz,vx,vy,vz)
C RLS - 03/27
        if (ifl(2) .eq. 466) go to 999
C RLS - END
      endif
      if (ntim.le.1) goto 999
      dsq = (px-xe)**2+(py-ye)**2+(pz-ze)**2
      if (svdsq.gt.dsq) then
        svdsq = dsq
        spx = px
        spy = py
        spz = pz
        svx = vx
        svy = vy
        svz = vz
        svu = t(iux,ia)
      endif
      t(iux,ia) = u
      u = u+du
      itim = itim+1
      if (itim.le.ntim) goto 100

      px = spx
      py = spy
      pz = spz
      vx = svx
      vy = svy
      vz = svz
      t(iux,ia) = svu

999   return
      end
