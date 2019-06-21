c*********************************************************************
c*    NAME         :  gencrv.f (see negeogn.c)
c*       CONTAINS:
c*     bspdef (nents, kfit, kxv, pts, crvp, gt, knum, kerr)
c*     ncl_crvfit (npts, inv, ptve, nt, kerr)
c*     ncl_crvgen (n, inv, ptve, s, pts)
c*     ncl_interp_rbsp (n,inv,ptve,itsk,pts,s,nt,kerr)
c*     ncl_segchk (cv,inv,ptve,pi,pk,kmode)
c*     ncl_slpset (n, inv, ptve, kmode, kerr)
c*
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        gencrv.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:07
c
c********************************************************************/
C*    E_FUNCTION: ncl_interp_rbsp (n,inv,ptve,itsk,pts,s,nt,kerr)
C*       Interpolate a rational B-spline curve thru a list of points &
C*       optional slope vectors.
C*    PARAMETERS
C*       INPUT  :
c*          n     I*4  - number of points in 'inv' & 'ptve' arrays.
c*          inv   I*4  - flags marking if point has defined slope
c*          ptve  R*8  - data structures defining points:
c*                        (x,y,z,a,b,c,dx,dy,dz,ch)
C*          itsk         = 0 - interpolate all points
C*                       = 1 - fit thru points.
C*       OUTPUT :
c*          s     R*8  - Bspline curve t values.
c*          pts   R*8  - Bspline curve control points.
c*          nt    I*4  - number of spans in created B-spline
c*          kerr  I*4  - error status.
c*
C*    SIDE EFFECTS : none
C********************************************************************/
      subroutine ncl_interp_rbsp (n,inv,ptve,itsk,pts,s,nt,kerr)
c
      include 'post.inc'
c
      integer*4 n, itsk, nt, inv(*), kerr
      real*8 ptve(10,*),pts(3,*),s(*)
c
      integer*4 i, ns, npts
c
c...set slope if not supplied
c
      nt    = n
      if (itsk .eq. 0 .or. n .lt. 3) then
        if (n .lt. 3 .and. inv(1) .eq. 0 .and. inv(2) .eq. 0) then
          call vcplvc (ptve(1,2),ptve(1,1),ptve(4,1),-1.d0)
          inv(1) = 1
        end if
        call ncl_slpset (n,inv,ptve,0,kerr)
      else
        call ncl_crvfit (n,inv, ptve, nt, kerr)
      end if
      if (kerr .ne. 0) go to 9000
c
c...create b-spline curve
c
      npts  = 3*nt - 2
      ns    = npts + 4
      call ncl_crvgen (nt, inv, ptve, s, pts)

 9000 return
      end
c*********************************************************************
c*    E_FUNCTION     : ncl_slpset (n, inv, ptve, kmode, kerr)
c*       Set slope vectors for array of curve generation points.
c*    PARAMETERS
c*       INPUT  :
c*          n     I*4  - number of points in 'inv' & 'ptve' arrays.
c*          inv   I*4  - flags marking if point has defined slope
c*          ptve  R*8  - data structures defining points:
c*                        (x,y,z,a,b,c,dx,dy,dz,ch)
c*          kmode I*4  - mode:  1 = first pass for curve fit
c*                              2 = 2nd    "    "    "    "
c*                              0 = all others
c*       OUTPUT :
c*          ptve  R*8  - contains slope vectors calculated
c*          kerr  I*4  - error status.
c*
c*    SIDE EFFECTS : none
c***********************************************************************
      subroutine ncl_slpset (n, inv, ptve, kmode, kerr)
c
      include 'post.inc'
c
      integer*4 n, kmode, kerr, inv(*)
      real*8 ptve(10,*)
c
      integer*4 i,m,ismall, iknt, jknt, pi, pj, pk
      real*8 stol,ro,co,tl,hvchg,vchg,vdel,cal,adis,al,sa,del(3)
      real*8 ndot,ndist,cbe,bdis,bl,sb,dav(3),dbv(3),zzer(3)
c
      data zzer /0.d0, 0.d0, 0.d0 /
c
      kerr   = 0
      if (n .lt. 2) then
         kerr = 1
         goto 8000
      end if
      if (kmode .eq. 1) then
         stol = 1.0d-4
      else
         stol = 1.0d-5
      end if
      ismall = 0
      if (n .lt. 3) ismall = 1
      m     = n - 1
      pi    = 1
      pj    = 2
      if (m .eq. 1 .and. inv(pi) .eq. 0 .and. inv(pj) .eq. 0) then
         call vcplvc (ptve(1,pj),ptve(1,pi),ptve(4,pi),-1.d0)
         call copyn (ptve(4,pi),ptve(4,pj),3)
         inv(pi) = 1
         inv(pj) = 1
      end if
      do 115 i=1,m,1
         call vcplvc (ptve(1,pj),ptve(1,pi),ptve(7,pi),-1.d0)
         ptve(10,pi) = dsqrt(ndot(ptve(7,pi),ptve(7,pi)))
         pi = pi + 1
         pj = pj + 1
  115 continue
      call copyn (ptve(7,m),ptve(7,n),4)
c
      if (kmode .lt. 2) then
         do 215 i=1,n,1
            pi = i - 1
            pj = i
            if (inv(pj) .eq. 0 .and. i .gt. 1 .and. i .lt. n) then
               ro = ptve(10,pi) / ptve(10,pj)
               ro = ro * ro
               call vcplvc (ptve(7,pi),ptve(7,pj),ptve(4,pj),ro)
            end if
            if (inv(pj) .gt. 0 .or. i .gt. 1 .and. i .lt. n) then
               tl = dsqrt(ndot(ptve(4,pj),ptve(4,pj)))
               if (tl .eq. 0.) go to 9000
               tl = 1.d0 / tl
               call vcplvc (zzer,ptve(4,pj),ptve(4,pj),tl)
            end if
  215    continue
      end if
c
      vchg   = 1.0
      hvchg  = 1.0d8
c
      do 505 jknt=1,3
        if (vchg .gt. stol) then
           do 405 iknt=1,100,1
              if (vchg .gt. stol) then
                 if (iknt .gt. 1) then
                    vchg = 0.0
                    pj = 2
                    do 355 i=1,m,1
                      if (inv(pj) .eq. 0) then
                        pi  = pj - 1
                        pk  = pj + 1
                        cal = ndot(ptve(4,pi),ptve(7,pi))/ptve(10,pi)
                        adis = .531*ptve(10,pi)/(.593+cal)
                        al   = ptve(10,pi)*(1.104+.13*cal)/(.851+cal)
                        sa   = ptve(10,pi)*(3.565+.24*cal)/(2.805+cal)
                        call vcplvc (ptve(7,pi),ptve(4,pi),dav,-adis)
                        cbe  = ndot(ptve(7,pj),ptve(4,pk))/ptve(10,pj)
                        bdis = .531*ptve(10,pj)/(.593+cbe)
                        bl   = ptve(10,pj)*(1.104+.13*cbe)/(.851+cbe)
                        sb   = ptve(10,pj)*(3.565+.24*cbe)/(2.805+cbe)
                        ro   = al*sa/(bl*sb)
                        call vcplvc(ptve(7,pj),ptve(4,pk),dbv,-bdis)
                        call vcplvc(dav,dbv,dbv,ro)
                        call unitvc(dbv,dbv)
                        call vcplvc(dbv,ptve(4,pj),del,-1.d0)
                        vdel = dabs(del(1))+dabs(del(2))+dabs(del(3))
                        if (vdel .gt. vchg) vchg = vdel
                        call copyn (dbv,ptve(4,pj),3)
                      end if
                      pj = pj + 1
  355               continue
                 end if
                 if (inv(1) .eq. 0) then
                    pi  = 1
                    pj  = 2
                    co  = 2.*ndot(ptve(7,pi),ptve(4,pj))/ptve(10,pi)**2
                    call vcplvc(zzer,ptve(7,pi),ptve(4,pi),co)
                    call vcplvc(ptve(4,pi),ptve(4,pj),ptve(4,pi),-1d0)
                 end if
                 pj  = m + 1
                 if (inv(pj)  .eq. 0) then
                    pi  = m
                    co  = 2.*ndot(ptve(7,pi),ptve(4,pi))/ptve(10,pi)**2
                    call vcplvc(zzer,ptve(7,pi),ptve(4,pj),co)
                    call vcplvc(ptve(4,pj),ptve(4,pi),ptve(4,pj),-1d0)
                 end if
                 if (ismall .eq. 1) vchg = 0.
                 if (hvchg .gt. vchg) hvchg = vchg
              end if
  405      continue
        end if
  505 continue
c
      do 605 i=2,n
         pj = i
         pi = pj - 1
         co = ndot(ptve(4,pj),ptve(7,pi)) / ptve(10,pi)
         if (co .lt. -.9999) then
            co = ndot(ptve(4,pj),ptve(4,pi)) / ptve(10,pi)
            if (co .lt. -.9999) then
               ptve(4,pj) = -ptve(4,pj)
               ptve(5,pj) = -ptve(5,pj)
               ptve(6,pj) = -ptve(6,pj)
            end if
         end if
  605 continue
      goto 8000
c
 9000 kerr = pi
 8000 return
      end
c*********************************************************************
c*    E_FUNCTION     : ncl_crvgen (n, inv, ptve, s, pts)
c*       Generate a rational B-spline curve.
c*    PARAMETERS
c*       INPUT  :
c*          n     I*4  - number of points in 'inv' & 'ptve' arrays.
c*          inv   I*4  - flags marking if point has defined slope
c*          ptve  R*8  - data structures defining points:
c*                        (x,y,z,a,b,c,dx,dy,dz,ch)
c*       OUTPUT :
c*          s     R*8  - curve t values.
c*          pts   R*8  - curve control points.
c*
c*    SIDE EFFECTS : none
c*********************************************************************
      subroutine ncl_crvgen (n, inv, ptve, s, pts)
c
      include 'post.inc'
c
      integer*4 n, inv(*)
      real*8 ptve(10,*), s(*), pts(3,*)
c
      integer*4  i, ix, m, pi, pj
      real*8 arcsum, arcl, cal, cbe, adis, bdis, cdis, ro
      real*8 ndot, rnum, drv(3), dcv(3), zzer(3)
c
      data zzer /0.d0, 0.d0, 0.d0/
c
      m      = n - 1
      arcsum = 0.0
      ix     = 1
      do 105 i=1,m
         pi   = i
         pj   = pi + 1
         cal  = ndot(ptve(4,pi),ptve(7,pi)) / ptve(10,pi)
         cbe  = ndot(ptve(4,pj),ptve(7,pi)) / ptve(10,pi)
         rnum = 2.d0/3.d0 * ptve(10,pi)
         adis = rnum / (1.d0 + cal)
         bdis = rnum / (1.d0 + cbe)
         if (adis .gt. bdis) adis = bdis*(2.d0 - bdis/adis)
         if (bdis .gt. adis) bdis = adis*(2.d0 - adis/bdis)
         call vcplvc(zzer,ptve(4,pi),ptve(4,pi),adis)
         call vcplvc(ptve(7,pi),ptve(4,pj),drv,-bdis)
         call vcplvc(ptve(7,pi),ptve(4,pi),dcv,-1.d0)
         call vcplvc(drv,dcv,dcv,1.d0)
         cdis = dsqrt(ndot(dcv,dcv))
         ro   = 1.62 * (adis + bdis)/cdis - .81
         arcl = (.5 - ro) * (adis + bdis) + (.5 + .5*ro) * cdis
         arcsum = arcsum + arcl
         s(i) = arcsum
         call copyn (ptve(1,pi),pts(1,ix),3)
         call vcplvc (ptve(1,pi),ptve(4,pi),pts(1,ix+1),1.d0)
         call vcplvc (ptve(1,pi),drv,pts(1,ix+2),1.d0)
         ix  = ix + 3
  105 continue
      call copyn (ptve(1,n),pts(1,ix),3)
c
      do 205 i=m,1,-1
         ix    = 3 * i + 2
         s(ix) = s(i)
         s(ix+1) = s(i)
         s(ix+2) = s(i)
  205 continue
      call copyn (zzer,s(1),3)
      s(4) = 0.d0
      s(3*n+2) = s(3*n+1)
c
      return
      end

c*********************************************************************
c*    E_FUNCTION     : ncl_crvfit (npts, inv, ptve, nt, kerr)
c*       Fit a rational B-spline curve through a set of points & optional
c*       slope vectors.
c*    PARAMETERS
c*       INPUT  :
c*          npts  I*4  - number of points in 'inv' & 'ptve' arrays.
c*          inv   I*4  - flags marking if point has defined slope
c*          ptve  R*8  - data structures defining points:
c*                        (x,y,z,a,b,c,dx,dy,dz,ch)
c*       OUTPUT :
c*          nt    I*4  - number of curve segments generated.
c*          kerr  I*4  - error status.
c*
c*    SIDE EFFECTS : none
c*********************************************************************
      subroutine ncl_crvfit (npts, inv, ptve, nt, kerr)
c
      include 'post.inc'
c
      equivalence (BSPTOL,POSMAP(4902))
c
      real*8 BSPTOL(2)
c
      integer*4 npts, kerr, nt, inv(*)
      real*8 ptve(10,*)
c
      integer*4 i, k, ih, m, n, mode, otol, pi, pj, ph
      real*8 hmov, htol, ftol, cv(3)
      real*8 ndot, ro, hldseg(10,1000)
      integer*4 hldinv(1000)
c
c      ftol   = .5 * BSPTOL(1)
      ftol   = BSPTOL(1)
      htol   = 1.d-6
      n      = npts
      m      = n - 1
      mode  = 1
      call ncl_slpset (n, inv, ptve, mode, kerr)
      if (kerr .eq. 0) then
         do 105 i=2,m,1
            pi = i - 1
            pj = i
            call ncl_segchk (cv,inv,ptve,pi,pi+2,mode)
            call vcplvc (cv,ptve(1,pj),ptve(7,pj),-1.d0)
  105    continue
      end if
      if (kerr .eq. 0) then
         do 205 i=2,m,1
            pi = i
            if (inv(pi) .eq. 0) then
               hmov = dsqrt(ndot(ptve(7,pi),ptve(7,pi)))
               if (hmov .gt. htol) then
                  ro = ftol / hmov
                  if (ro .gt. 1.d0) ro = 1.d0
                  call vcplvc(ptve(1,pi),ptve(7,pi),ptve(1,pi),-ro)
               end if
            end if
  205    continue
         mode = 2
         call ncl_slpset (n, inv, ptve, mode, kerr)
      end if
c
      if (kerr .eq. 0) then
         pi = 1
         call copyn (ptve(1,pi),hldseg(1,pi),10)
         hldinv(pi) = inv(pi)
         ph = 2
         ih = 1
         pj = pi + 2
         k  = 2
      end if

  305 if (k .lt. n .and. kerr .eq. 0) then
         call ncl_segchk (cv,inv,ptve,pi,pj,mode)
         otol = 0
         if (dabs(cv(1))+dabs(cv(2))+dabs(cv(3)) .gt. ftol) otol = 1
         if (otol .eq. 1 .or. inv(pj) .eq. 1) then
            if (otol .eq. 1) then
               pj = pj - 1
               k  = k - 1
            end if
            call copyn (ptve(1,pj),hldseg(1,ph),10)
            hldinv(ph) = inv(pj)
            ph   = ph + 1
            ih   = ih + 1
            pi   = pj
            pj   = pj + 2
            k    = k  + 2
         else
            pj   = pj + 1
            k    = k  + 1
         end if
         go to 305
      end if

      if (kerr .eq. 0) then
         if (k .lt. n+1) then
            call copyn (ptve(1,pj-1),hldseg(1,ph),10)
            hldinv(ph) = inv(pj-1)
            ih  = ih + 1
         end if
         pi   = 1
         ph   = 1
         do 405 i=1,ih,1
            call copyn (hldseg(1,ph),ptve(1,pi),6)
            if (i .lt. ih) then
               pj = ph + 1
               call vcplvc (hldseg(1,pj),hldseg(1,ph),ptve(7,pi),-1.d0)
               ptve(10,pi) = dsqrt(ndot(ptve(7,pi),ptve(7,pi)))
            end if
            pi = pi + 1
            ph = ph + 1
  405    continue
      end if
c
      nt    = ih
      return
      end

c*********************************************************************
c*    E_FUNCTION     : ncl_segchk (cv,inv,ptve,pi,pk,kmode)
c*       Check points lying between 2 curve segment points for tolerance.
c*    PARAMETERS
c*       INPUT  :
c*          inv   I*4  - flags marks if point has defined slope
c*          ptve  R*8  - data structures defining points:
c*                        (x,y,z,a,b,c,dx,dy,dz,ch)
c*          pi    I*4  - pointer to 1st curve segment.
c*          pk    I*4  - pointer to last curve segment
c*          kmode I*4  - =1 - return near point on segment
c*                       =2 - return max delta between interior
c*                       points & curve segment
c*       OUTPUT :
c*          cv    R*8  - output vector (see kmode)
c*
c*    SIDE EFFECTS : none
c********************************************************************/
      subroutine ncl_segchk (cv,inv,ptve,pi,pk,kmode)
c
      include 'post.inc'
c
      real*8  cv(3),ptve(10,*)
      integer*4 inv(*),pi,pk,kmode
c
      integer*4 itim,pj
c
      real*8 hdv(3),dlv(3)
      real*8 vcq(3), vcr(3), vcc(3), vca(3), vcb(3), del(3)
      real*8 ctan, c1, c2, c3, cc, cal, cbe, adis, bdis
      real*8 u, du, uerr, oerr, den, ndot
c
      ctan = 1.0
      hdv(1) = 0.0
      hdv(2) = 0.0
      hdv(3) = 0.0
      pj   = pi + 1
      call vcplvc (ptve(1,pk),ptve(1,pi),dlv,-1.d0)
      cc   = dsqrt(ndot(dlv,dlv))
      cal  = ndot(dlv,ptve(4,pi)) / cc
      cbe  = ndot(dlv,ptve(4,pk)) / cc
      adis = 2./3.d0 * cc / (1.d0 + cal)
      bdis = 2./3.d0 * cc / (1.d0 + cbe)
      if (adis .gt. bdis) adis = bdis * (2.0 - bdis/adis)
      if (bdis .gt. adis) bdis = adis * (2.0 - adis/bdis)
      call vcplvc (ptve(1,pi),ptve(4,pi),vcq,adis)
      call vcplvc (ptve(1,pk),ptve(4,pk),vcr,-bdis)
c
  205 if (pj .lt. pk) then
         u    = .5
         itim = 0
  305    if (itim .lt. 10) then
            c1   = 1.d0 - u
            c2   = 2.d0 * c1 * u
            c1   = c1 * c1
            c3   = u * u
            vca(1)   = c1*ptve(1,pi) + c2*vcq(1) + c3*vcr(1)
            vca(2)   = c1*ptve(2,pi) + c2*vcq(2) + c3*vcr(2)
            vca(3)   = c1*ptve(3,pi) + c2*vcq(3) + c3*vcr(3)
            vcb(1) = c1*vcq(1) + c2*vcr(1) + c3*ptve(1,pk) - vca(1)
            vcb(2) = c1*vcq(2) + c2*vcr(2) + c3*ptve(2,pk) - vca(2)
            vcb(3) = c1*vcq(3) + c2*vcr(3) + c3*ptve(3,pk) - vca(3)
            call vcplvc (ptve(1,pj),vca,del,-1.d0)
            uerr = ndot(vcb,del) / ndot(vcb,vcb) - u
            uerr = uerr / 3.d0
            if (dabs(uerr) .lt. 1.d-5) then
               itim = 10
            else
               if (itim .gt. 0) then
                  den = oerr - uerr
                  if (dabs(den) .gt. 1.d-5) then
                     ctan = du / den
                     if (ctan .lt. .1) ctan = 1.0
                  end if
               end if
               du  = uerr * ctan
               if (du+u .gt. 1.d0) du = 1.d0 - u
               if (du+u .lt. 0.d0) du = -u
               u   = u + du
               oerr = uerr
            end if
            itim = itim + 1
         end if
         if (itim .lt. 10) go to 305
         call vcplvc (vca,vcb,cv,u)
         if (kmode .eq. 2) then
            call vcplvc (cv,ptve(1,pj),dlv,-1.d0)
            if (dabs(dlv(1)) .gt. dabs(hdv(1))) hdv(1) = dlv(1)
            if (dabs(dlv(2)) .gt. dabs(hdv(2))) hdv(2) = dlv(2)
            if (dabs(dlv(3)) .gt. dabs(hdv(3))) hdv(3) = dlv(3)
         end if
         pj = pj + 1
      end if
      if (pj .lt. pk) go to 205
c
      if (kmode .eq. 2) call copyn (hdv,cv,3)
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION: bspdef (nents, kfit, kxv, pts, crvp, gt, knum, kerr)
c*       Creates cubic B-spline curve thru all points in 'pts'.
c*       The 'pts' and 'kxv' arrays are internaly buffered to preserve
c*       original values.
c*    PARAMETERS
c*       INPUT  :
c*          nents I*4  - number of points (and vectors) in 'pts' array.
c*          kfit  I*4  - 0 = interpolate, 1 = fit
c*          kxv   I*4  - slope vector included flags.
c*          pts   R*8  - data points (x,y,z & optional slope vector)
c*       OUTPUT :
c*          crvp  R*8  - curve control points.
c*          gt    R*8  - curve t values.
c*          knum  I*4  - number of curve segments generated.
c*          kerr  I*4  - error status.
c*
c*    SIDE EFFECTS : none
c***************************************fit******************************
      subroutine bspdef (npts,kfit,kxv,pts,crvp,gt,slv,knum,kerr)
c
      include 'post.inc'
c
      integer*4 npts, kfit, kerr, kxv(*), knum
      real*8 pts(10,*), crvp(3,*), gt(*), slv(6)
c
      equivalence (BSVCST,BSPMAP(01)), (BSVCND,BSPMAP(04))
c
      real*8 BSVCST(3),BSVCND(3)
c
      real*8 bpts(10,1000)
      integer*4 ixv(1000)
c
      kerr = 0
      call copynk (kxv,ixv,npts)
      call copyn (pts,bpts,npts*10)
c
c...create bspline curve
c
      call ncl_interp_rbsp (npts, ixv, bpts, kfit, crvp, gt, knum, kerr)
c
c...get end points slope vector
c
      if (kerr .eq. 0) then
         call unitvc (bpts(4,1),slv(1))
         call unitvc (bpts(4,knum),slv(4))
      end if
c
      return
      end
