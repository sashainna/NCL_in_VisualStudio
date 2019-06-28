C*********************************************************************
C*    NAME         :  nfuncs.f
C*       CONTAINS:
C*        nmxmnf
C*        nsignf
C*        nlnthf
C*        nanglf
C*        ndistf
C*        prsuv2 (lflg, u1, v1)
C*        distf (asw1, asw2)
C*        ntypef
C*        xtypef
C*        nvcabf
C*        findexf
C*        strcmpf
C*        strpars
C*        ntextf
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       nfuncs.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       05/01/17 , 13:09:18
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nmxmnf
C*      Function to determine the minimum or maximum of a list of scalars.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : Result in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine nmxmnf

      include 'com.com'

      integer*2 ilx
      equivalence (ifl(326),ilx)

      real*8 a1(20)
      logical lmin(20)
      integer*2 MIN1F
      parameter (MIN1F = 571)

      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      lmin(ilx) = ist.eq.MIN1F
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.3.and.ityp.ne.4.and.(ityp.ne.2.or.ist.ne.2)) goto 9007
      a1(ilx)=tv
      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9311
100   call parser
      call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.3.and.ityp.ne.4.and.(ityp.ne.2.or.ist.ne.2)) goto 9007
      if (lmin(ilx)) then
        a1(ilx)=dmin1(a1(ilx),tv)
      else
        a1(ilx)=dmax1(a1(ilx),tv)
      endif
      call parser
      if (ityp.eq.5.and.ist.eq.9) goto 100
      if (ityp.ne.5.or.ist.ne.7) goto 9310

      tv = a1(ilx)
      goto 999
c                   number or scalar expected
9007  ifl(2)=7
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999

c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999

999   ilx=ilx-1
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nsignf
C*      Function to return the absolute value of the first argument with the
C*      sign of the second argument.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : Result in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine nsignf

      include 'com.com'

      integer*2 ilx
      equivalence (ifl(326),ilx)

      real*8 a1(20)

      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.3.and.ityp.ne.4.and.(ityp.ne.2.or.ist.ne.2)) goto 9007
      a1(ilx)=tv
      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9311
      call parser
      call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.3.and.ityp.ne.4.and.(ityp.ne.2.or.ist.ne.2)) goto 9007
      a1(ilx)=dsign(a1(ilx),tv)
      call parser
      if (ityp.ne.5.or.ist.ne.7) goto 9310

      tv = a1(ilx)
      goto 999
c                   number or scalar expected
9007  ifl(2)=7
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999

c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999

999   ilx=ilx-1
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nlnthf
C*      Function to determine the length of a vector, line or point vector,
C*      circle, curve or text string.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : Length in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine nlnthf

      include 'com.com'

      integer*2 ilx
      equivalence (ifl(326),ilx)

      character*256 str
      real*8 w(6), tvhld
      integer*4 nclkey
      integer*2 ix, isthld, len, nwds, ietype
      logical trflg
      data trflg /.false./
      integer*2 STRINGV/9/
c
      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.eq.STRINGV .or. ityp.eq.2 .and. ist.eq.TEXTVAR) then
        call strpars(str,len)
        if (ifl(2).ne.0) goto 999
        if (ityp.ne.5.or.ist.ne.7) goto 9310
        tv = len
        goto 999
      endif
      if (ityp.ne.2) goto 9457
      if (ist.ne.VECTOR.and.ist.ne.LINE.and.ist.ne.PNTVEC.and.
     x    ist.ne.CIRCLE.and.ist.ne.CURVE) goto 9457
      tvhld = tv
      isthld = ist
      call parser
      if (ityp.ne.5.or.ist.ne.7) goto 9310
      if (isthld.eq.CIRCLE.or.isthld.eq.CURVE) then
        call crvlen (tvhld, 200, tv, ifl(2))
        if (ifl(264) .eq. 1 .and. isthld.eq.CIRCLE) then
            tv = tv * 25.4d0
        endif
        goto 999
      endif
      if (isthld.eq.TEXTVAR) then
        call gtdesc(tvhld,nclkey,nwds,ietype)
        call ncl_lntext(nclkey,len)
        tv = len
        goto 999
      endif
      call gtentt(tvhld,trflg,nclkey,isthld,w)
      ix = 3
      if (isthld.eq.VECTOR) ix = 0
      tv = dsqrt(w(ix+1)**2+w(ix+2)**2+w(ix+3)**2)
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   vector, line, circle or curve expected
9457  ifl(2)=457
      goto 999

999   ilx=ilx-1
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nanglf
C*      Function to determine the angle between any 2 of a vector, line,
C*      plane or point vector, or between a vector from the center of a
C*      to a point and positive x axis.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : Angle in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine nanglf

      include 'com.com'

      integer*2 ilx
      equivalence (ifl(326),ilx)

      real*8 a1(3,20), w(20), sec1, sec2, co, x, y, z
      integer*4 nclkey
      integer*2 nwds, ietype, ix, i
      integer*2 izro /0/
      logical trflg
      data trflg /.false./

      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.2) goto 9458
      ix = 0
      if (ist.eq.SURF) then
        call gtdesc(tv,nclkey,nwds,ietype)
        call ncl_get_sf_primtyp(nclkey,ietype)
        if (ietype.ne.3) goto 9458
        call gtplt(tv, izro, w)
      else
        if (ist.ne.CIRCLE.and.ist.ne.VECTOR.and.ist.ne.LINE.and.
     x      ist.ne.PLANE.and.ist.ne.PNTVEC) goto 9458
        call gtentt(tv,trflg,nclkey,ietype,w)
        if (ietype.eq.LINE.or.ietype.eq.PNTVEC) ix = 3
      endif
      do 10 i=1,3
10    a1(i,ilx) = w(i+ix)
      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9311
      call parser
      if (ietype.eq.CIRCLE) then
        if (ityp.eq.2.and.ist.eq.14) call exprs2
        if (ityp.ne.2 .or. ist.ne.POINT) goto 9020
        call gtentt(tv,trflg,nclkey,ietype,w)
        x = w(1)-a1(1,ilx)
        y = w(2)-a1(2,ilx)
        z = w(3)-a1(3,ilx)
        sec1 = dsqrt(x**2+y**2+z**2)
        if (sec1.eq.0.0d0) sec1 = 1.0d0
        co = x/sec1
      else
        if (ityp.eq.2.and.ist.eq.14) call exprs2
        if (ityp.ne.2) goto 9459
        ix = 0
        if (ist.eq.SURF) then
          call gtdesc(tv,nclkey,nwds,ietype)
          call ncl_get_sf_primtyp(nclkey,ietype)
          if (ietype.ne.3) goto 9459
          call gtplt(tv, izro, w)
        else
          if (ist.ne.VECTOR.and.ist.ne.LINE.and.ist.ne.PLANE.and.
     x        ist.ne.PNTVEC) goto 9459
          call gtentt(tv,trflg,nclkey,ietype,w)
          if (ietype.eq.LINE.or.ietype.eq.PNTVEC) ix = 3
        endif
        sec1 = dsqrt(a1(1,ilx)**2+a1(2,ilx)**2+a1(3,ilx)**2)
        if (sec1.eq.0.0d0) sec1 = 1.0d0
        sec2 = dsqrt(w(1+ix)**2+w(2+ix)**2+w(3+ix)**2)
        if (sec2.eq.0.0d0) sec2 = 1.0d0
        co = (a1(1,ilx)*w(1+ix)+a1(2,ilx)*w(2+ix)+a1(3,ilx)*w(3+ix))
     x        /sec1/sec2
        x = a1(2,ilx)*w(3+ix)-a1(3,ilx)*w(2+ix)
        y = a1(3,ilx)*w(1+ix)-a1(1,ilx)*w(3+ix)
        z = a1(1,ilx)*w(2+ix)-a1(2,ilx)*w(1+ix)
        if (dabs(z).ge.dabs(x).and.dabs(z).ge.dabs(y)) then
          y = z
        else if (dabs(x).gt.dabs(y)) then
          y = x
        endif
      endif
      call parser
      if (ityp.ne.5.or.ist.ne.7) goto 9310
      if (co.gt.1.0d0) co = 1.0d0
      if (co.lt.-1.0d0) co = -1.0d0
      tv = dacos(co)*57.295779513
      if (y.lt.0.0d0) tv = 360-tv
      goto 999
c                   number or scalar expected
9007  ifl(2)=7
      goto 999
c                   point expected
9020  ifl(2)=20
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999

c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999
c                   vector, line, circle or plane expected
9458  ifl(2)=458
      goto 999
c                   vector, line plane or pntvec expected
9459  ifl(2)=459
      goto 999

999   ilx=ilx-1
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ndistf
C*      Function to determine the distance between 2 entities.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : distance in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ndistf

      include 'com.com'
      if (sc(169).le.9.502) then
          call ndistfo
      else
          call ndistfn
      endif
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine ndistfn
C*      Function to determine the distance between 2 entities.
C*		function used after V9.5
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : distance in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ndistfn

      include 'com.com'
      include 'suvcom.com'

      integer*2 ilx
      equivalence (ifl(326),ilx)

      real*8 asw1(20), asw2(20)
      integer*4 nclkey
      integer*2 nwds, ietype

      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.2) goto 9087
      if (ist.eq.1) goto 9009
      asw1(ilx) = tv
      if (ist.eq.SURF.and.nextyp.eq.15) then
        call prsuv2 (dsuv, dsu, dsv)
        if (ifl(2).gt.0) goto 999
      endif
      call parser
      call gtdesc (asw1(ilx),nclkey,nwds,ietype)
      if (ietype.eq.CURVE.and.ityp.eq.5.and.ist.eq.7) then
         asw2(ilx) = 200
      else
        if (ityp.ne.5.or.ist.ne.9) goto 9311
        call parser
        if (ityp.eq.2.and.ist.eq.14) call exprs2
        call gtdesc (asw1(ilx),nclkey,nwds,ietype)
        if (ietype.eq.CURVE.and.((ityp.ne.2).or.
     x         (ityp.eq.2).and.(ist.eq.1) )) then
           call exprs2
           if (ifl(2).ne.0) goto 999
           if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.ityp.ne.4)
     x        goto 9007
        else  
          if (ityp.ne.2) goto 9087
          if (ist.eq.1) goto 9009
          asw2(ilx) = tv
          if (ist.eq.SURF.and.nextyp.eq.15) then
            call prsuv2 (dsuv, dsu, dsv)
            if (ifl(2).gt.0) goto 999
          endif
          if (ietype.eq.POINT.or.ietype.eq.PNTVEC) then
            if (ist.ne.POINT.and.ist.ne.PNTVEC.and.ist.ne.PLANE.and.
     x          ist.ne.LINE.and.ist.ne.SURF.and.
     x          ist.ne.CIRCLE.and.ist.ne.CURVE.and.ist.ne.VSOLID)
     x              goto 9345
            if (ist.eq.SURF.and.nextyp.eq.15) then
              call prsuv2 (dsuv, dsu, dsv)
              if (ifl(2).gt.0) goto 999
            endif
cc          else if (ietype.eq.SURF .or.ist.eq.SURF) then
cc            if (ietype.eq.SURF) then
cc              call gtdesc(asw1(ilx),nclkey,nwds,ietype)
cc              call ncl_get_sf_primtyp(nclkey,ietype)
cc              if (ietype.ne.3) goto 9345
cc            endif
cc            if (ist.eq.SURF) then
cc              call gtdesc(asw2(ilx),nclkey,nwds,ietype)
cc              call ncl_get_sf_primtyp(nclkey,ietype)
cc              if (ietype.ne.3) goto 9345
cc            endif
          endif
        endif
        call parser
        if (ityp.ne.5.or.ist.ne.7) goto 9310
      endif
      call distf (asw1(ilx),asw2(ilx))
      goto 999
c                   number or scalar expected
9007  ifl(2)=7
      goto 999
c                   Identifier not previously defined
9009  ifl(2)=9
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   Identifier expected
9087  ifl(2)=87
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999
9345  ifl(2)=345
      goto 999
999   ilx=ilx-1
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine prsuv2 (lflg, u1, v1)
C*      Function to parse surface u & v values.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          lflg      - values found.
C*          u1        - U value
C*          v1        - V value
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine prsuv2 (lflg, u1, v1)

      include 'com.com'

      logical lflg
      real*4 u1, v1

      call parser
      call parser
      call exprs2
      if (ifl(2).gt.0) goto 999
      if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.ityp.ne.4)
     x     goto 9007
      u1 = tv
      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9311
      call parser
      call exprs2
      if (ifl(2).gt.0) goto 999
      if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.ityp.ne.4)
     x     goto 9007
      if (nextyp.ne.16) goto 9422
      lflg = .true.
      v1 = tv
      call parser

      goto 999
c                   number or scalar expected
9007  ifl(2)=7
      goto 999
c                   comma expected
9311  ifl(2)=311
      goto 999
c                   Right bracket expected.
9422  ifl(2)=422
      goto 999

999   continue
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine distf (asw1, asw2)
C*      Function to calulate the distance between 2 geometric entities.
C*    PARAMETERS
C*       INPUT  :
C*          asw1      - Associated word of first entity.
C*          asw2      - Associated word of second entity.
C*       OUTPUT :
C*          none
C*    RETURNS      : Distance in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine distf (asw1, asw2)

      include 'com.com'

      real*8 asw1, asw2

      real*8 val
      integer*4 nclkey1, nclkey2, error
      integer*2 nwds, it1, it2
      real*8 a1(11), a2(11), sec1,sec2,co,den,dx,dy,dz
      logical trflg
      data trflg /.true./

      if (ifl(38) .eq. 1 .or. ifl(370) .eq. 1) goto 999
      call gtdesc (asw1, nclkey1, nwds, it1)
      call gtdesc (asw2, nclkey2, nwds, it2)

      if ((it1.eq.CURVE).and.(nclkey2.eq.0)) then
        nwds = asw2
        call crvlen (asw1, nwds, tv, ifl(2))      
      else if ((it1.eq.SURF.or.it1.eq.PLANE).and.
     x         (it2.eq.SURF.or.it2.eq.PLANE)) then
          call ncl_geom_distf(nclkey1, nclkey2, val, error)
          if (error .ne. 0) goto 9345
          tv = val
      else if (it1.eq.SURF.or.it2.eq.SURF) then
        if (it1.eq.SURF) then
          sc(11) = asw2
          sc(12) = asw1
        else
          sc(11) = asw1
          sc(12) = asw2
        endif
        isc10(2) = 1
        call sfptdo
        tv = sc(13)
      else
        call gtentt (asw1, trflg, nclkey1, it1, a1)
        call gtentt (asw2, trflg, nclkey2, it2, a2)
        if ((it1.eq.POINT.or.it1.eq.PNTVEC).and.
     x           (it2.eq.POINT.or.it2.eq.PNTVEC)) then
            tv=dsqrt((a1(1)-a2(1))**2+(a1(2)-a2(2))**2+(a1(3)-a2(3))**2)
        else if ((it1.eq.POINT.or.it1.eq.PNTVEC).and.it2.eq.PLANE.or.
     x          ((it2.eq.POINT.or.it2.eq.PNTVEC).and.it1.eq.PLANE)) then
           if (it1.eq.PLANE) then
             tv=dabs(a1(4)-(a1(1)*a2(1)+a1(2)*a2(2)+a1(3)*a2(3)))
           else
             tv=dabs(a2(4)-(a1(1)*a2(1)+a1(2)*a2(2)+a1(3)*a2(3)))
           endif
        else if ((it1.eq.POINT.or.it1.eq.PNTVEC).and.it2.eq.LINE.or.
     x          ((it2.eq.POINT.or.it2.eq.PNTVEC).and.it1.eq.LINE)) then
           dx = a1(1) - a2(1)
           dy = a1(2) - a2(2)
           dz = a1(3) - a2(3)
           sec1 = dx*dx + dy*dy + dz*dz
           if (it2.eq.LINE) then
             den = a2(4)*a2(4)+a2(5)*a2(5)+a2(6)*a2(6)
             co = dx*a2(4) + dy*a2(5) + dz*a2(6)
           else
             den = a1(4)*a1(4)+a1(5)*a1(5)+a1(6)*a1(6)
             co = dx*a1(4) + dy*a1(5) + dz*a1(6)
           endif
           if (den .lt. 1.d-12) goto 9345
           sec2 = (co*co)/den
           if (sec1 .lt. sec2) goto 9345
           tv = dsqrt (sec1 - sec2)
        else        
          call ncl_geom_distf(nclkey1, nclkey2, val, error)
          if (error .ne. 0) goto 9345
          tv = val
        endif
      endif
      goto 999
c           Invalid combination of geometry for dist.
9345  ifl(2)=345
      goto 999

999   continue
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine tndistf
C*      Function to calulate the closest/farthest/near-point distance
C*      between a point-vector and a geometric entity.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : Distance in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine tndistf

      include 'com.com'

      real*8 asw1(20), asw2(20), asw3(20), dis
      real*8 val, ptvc(6)
      integer*4 nclkey1, nclkey2, nclkey3, error, typ
      integer*2 nwds, it1, it2, it3, pverr
      integer*2 ilx
      equivalence (ifl(326),ilx)

      ilx = ilx + 1
      if (ilx.gt.20) goto 9059
      nclkey3 = 0
      typ = 0
      error = 0
      pverr = 0
      call parser
c     Check for left paren      
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.2) goto 9087
      if (ist.eq.1) goto 9009
      asw1(ilx) = tv   
      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9057
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      asw2(ilx) = tv
      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9057
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ityp.eq.3) then 
        typ = itv
        if (typ.lt.-2.or.typ.gt.2) goto 9445
c     Near point used
      else if (ityp.eq.2.and.ist.eq.3) then 
        typ = 3
        asw3 = tv
        call gtdesc (asw3, nclkey3, nwds, it3)
      else 
        goto 9033
      endif
c
c.....Get keys and types for entities
c
      call gtdesc (asw1(ilx), nclkey1, nwds, it1)
      if (it1.ne.PNTVEC) goto 9443   
      call gtdesc (asw2(ilx), nclkey2, nwds, it2)
        
      if (it2.eq.SURF.or.it2.eq.PLANE.or.it2.eq.CIRCLE.or.
     x    it2.eq.CURVE.or.it2.eq.LINE.or.it2.eq.33) then
     
          call parser
c     Check for right paren      
          if (ityp.ne.5.or.ist.ne.7) goto 9310
          call ncl_geom_tdistf(nclkey1,nclkey2,typ,nclkey3,val,error)
          if (error .ne. 0) goto 9502
      else 
        goto 9345
      endif
      tv = val
      goto 999
c                   Identifier not previously defined
9009  ifl(2)=9
      goto 999
c                   Point or scalar expected
9033  ifl(2)=33
      goto 999   
c                   ',' expected
9057  ifl(2)=57
      goto 999      
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   Identifier expected
9087  ifl(2)=87
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   Invalid combination of geometry for tdist.
9345  ifl(2)=345
      goto 999
c                   Point vector expected
9443  ifl(2)=443
      goto 999 
c                   Scalar out of range
9445  ifl(2)=445
      goto 999 
c                   No intersection found
9502  ifl(2)=502
      goto 999      
      
 999  ilx=ilx-1 
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ntypef
C*      Function to determine the type of an entity.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          gtv   = 'tv' value of entity.
C*    RETURNS      : Type in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ntypef (gtv)

      include 'com.com'
c      include 'com8a.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
c
      real*8 gtv
c
      integer*2 isym, dat, iret
      integer*4 nclkey
      real*4 u,v
      logical lflg
c
      integer*2 ilx
      equivalence (ifl(326),ilx)

      real*8 t1,origin(3)

      ilx = ilx+1
      dat = 0
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14.and.nextyp.eq.6) call exprs2
      if (ifl(2).ne.0) goto 999
      if (nextyp.ne.7) goto 9310
c
c.....Text string will be output as type 4
c      
      if (ityp.eq.9) then
        t1 = 4.0d0
      else if (ityp.eq.1) then
        t1 = 3.0d0
      else if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
C
C...I don't think t1 is what should be compared here, but tv
C...so changed all the t1 to tv in the if/else statement. 
C...and tooke out the bad logic statement. JLS 1/22/99
C
C       if (tv.eq.1.0d0 .or. (.not.(tv.gt.1.0d-14))) then
        if (tv.le.1.0d0) then
          t1 = 1.0d0
        else
          t1 = 2.0d0
        endif
      else if (ityp.eq.2) then
          gtv = tv
          if (ist .eq. 1) then
              call ub_symbol_name(token2,ivxsub,nclkey,origin,isym)
              if (isym .ne. 0) ist = isym + 30
          endif
          t1 = ist+4.0d0
      endif
      call parser
      tv = t1
      goto 999
c                   integer expected
9053  ifl(2)=53
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   right bracket expected
9462  ifl(2)=462
      goto 999
      
999   ilx=ilx-1
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine xtypef
C*      Function to determine the extended type of an entity.  It 
C*      first calls 'ntypef' and if the entity type is one of the
C*      following, it will routine and the exact type of entity.
C*
C*            Curve: NCL = 1201, Spline = 1202, S-spline = 1203,
C*                   Composite = 1204.
C*
C*            Pattern: Points = 2401, 2402 = Point-vectors.
C*
C*            Solid: Box = 3701, Cylinder = 3702, Torus = 3703,
C*                   Sphere = 3704, Cone = 3705, Extrusion = 3706,
C*                   Contour = 3707, Revolved = 3708, STL = 3709
C*
C*            Surface: NCL = 1301, Nurb = 1302, Revolved = 1303,
C*                     Mesh = 1304, Quilt = 1305, Trimmed = 1306,
C*                     NET = 1307
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : Type in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine xtypef
c
      include 'com.com'
c
      integer*4 nclkey,relnum,np,styp
      integer*2 nwds,ietype,npts
c
      real*8 parms(128),rtv
c
c...Get main type of entity
c
      call ntypef (rtv)
      if (ifl(2) .ne. 0 .or. tv .lt. 7.) go to 8000
c
c...Get entity's key
c
      call gtdesc (rtv,nclkey,nwds,ietype)
c
c...Entity has a sub-type
c
      itv = tv + .1
      if (itv .eq. 12 .or. itv .eq. 13) then
          call gtreln (nclkey,relnum)
c
c......Curves
c
          if (relnum .eq. 82) tv = 1201
          if (relnum .eq. 7) tv = 1202
          if (relnum .eq. 13) tv = 1203
          if (relnum .eq. 5) tv = 1204
c
c......Surfaces
c
          if (relnum .eq. 83) tv = 1301
          if (relnum .eq. 11) tv = 1302
          if (relnum .eq. 100) tv = 1303
          if (relnum .eq. 86) tv = 1304
          if (relnum .eq. 99) tv = 1306
          if (relnum .eq. 93) tv = 1307
c
c......Patterns
c
      else if (itv .eq. 24) then
          call gtpnnp (nclkey,npts,styp)
          tv = 2400 + styp
c
c......Solids
c
      else if (itv .eq. 37) then
          call nclf_get_solid (nclkey,styp,parms,np)
          tv = 3700 + styp
      endif
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nvcabf
C*      Function to determine the value of a vocabulary word.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : Type in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine nvcabf

      include 'com.com'

      integer*2 ilx
      equivalence (ifl(326),ilx)

      real*8 t1

      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ifl(2).ne.0) goto 999
      if (nextyp.ne.7) goto 9310
      if (ityp .eq. 1) then
          t1    = ist
      else
          t1    = 0
      endif
      call parser
      tv     = t1
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999

999   ilx=ilx-1
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine findexf
C*      Function to find the position of 1 string in another.
C*    PARAMETERS
C*       INPUT  :
C*          iflg    - =0,find from start, =1 find from end
C*       OUTPUT :
C*          none
C*    RETURNS      : Position of string or zero if not found.
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine findexf(iflg)

      include 'com.com'

      integer*2 iflg

      integer*2 ilx
      equivalence (ifl(326),ilx)

      character*256 str1(20), str2(20)
      integer*2 j, k, n1(20), n2(20)
      integer*2 LPARENV/6/,RPARENV/7/,COMMAV/9/

      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.LPARENV) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).ne.0) goto 999
      call strpars(str1(ilx),n1(ilx))
      if (ifl(2).gt.0) goto 999
      if (ityp.ne.5.or.ist.ne.COMMAV) goto 9311
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      call strpars(str2(ilx),n2(ilx))
      if (ifl(2).gt.0) goto 999
      if (ityp.ne.5.or.ist.ne.RPARENV) goto 9310

      if (.not.ltcase) then
          j = 1
          call nupper (str1(ilx),j,n1(ilx))
          call nupper (str2(ilx),j,n2(ilx))
      endif

      k =  index(str1(ilx)(1:n1(ilx)), str2(ilx)(1:n2(ilx)))
      if (iflg.eq.1 .and. k.gt.0) then
        j = k
        do while (j.gt.0 .and. k+n2(ilx).le.n1(ilx))
          j = index(str1(ilx)(k+1:n1(ilx)),str2(ilx)(1:n2(ilx)))
          k = k+j
        enddo
      endif
      tv = k
      goto 999

c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999

999   ilx=ilx-1
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine strcmpf
C*      Function to compare one string with another.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          TV      - =1,strings match, =-1, strings do not match.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine strcmpf

      include 'com.com'

      integer*2 ilx
      equivalence (ifl(326),ilx)

      character*256 str1(20), str2(20)
      integer*2 j, n1(20), n2(20)
      integer*2 LPARENV/6/,RPARENV/7/,COMMAV/9/

      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.LPARENV) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).ne.0) goto 999
      call strpars(str1(ilx),n1(ilx))
      if (ifl(2).gt.0) goto 999
      if (ityp.ne.5.or.ist.ne.COMMAV) goto 9311
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      call strpars(str2(ilx),n2(ilx))
      if (ifl(2).gt.0) goto 999
      if (ityp.ne.5.or.ist.ne.RPARENV) goto 9310
      tv = -1.0d0
      if (n1(ilx).eq.n2(ilx)) then
        if (.not.ltcase) then
          j = 1
          call nupper (str1(ilx),j,n1(ilx))
          call nupper (str2(ilx),j,n2(ilx))
        endif
        if (str1(ilx)(1:n1(ilx)).eq.
     x      str2(ilx)(1:n2(ilx))) then
          tv = 1.0d0
        endif
      endif
      goto 999

c                   expression too long
9059  ifl(2)=59
      goto 999
c                   positive scalar expected
9224  ifl(2)=224
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999
c                   Right bracket expected
9462  ifl(2)=462
      goto 999
c                   Colon expected.
9512  ifl(2)=512
      goto 999
c                   Text or text variable expected.
9513  ifl(2)=513
      goto 999

999   ilx=ilx-1
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine strpars
C*     Parse a string as an argument to a function.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          str     - String parsed.
C*          len     - Length of string.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine strpars(str,len)

      include 'com.com'

      character*256 str
      integer*2 len

      integer*2 ilx
      equivalence (ifl(326),ilx)

      integer*2 MXDPTH
      parameter (MXDPTH = 20)

      character*256 str1(MXDPTH)
      integer*4 nclkey
      integer*2 nwds, ietype, n1(MXDPTH), ix1(MXDPTH), ix2(MXDPTH)
      integer*2 STRINGV/9/
      integer*2 COLONV/13/, LBRCKTV/15/, RBRCKTV/16/

      ilx = ilx+1
      if (ilx.gt.20) goto 9059

      if (ityp.eq.STRINGV) then
        str1(ilx) = token2
        n1(ilx)   = length
      else if (ityp.eq.2 .and. ist.eq.TEXTVAR) then
        call gtdesc(tv,nclkey,nwds,ietype)
        n1(ilx) = 256
        call ncl_gttext(nclkey,str1(ilx),n1(ilx))
      else
        goto 9513
      endif
      call parser
      ix1(ilx) = 1
      ix2(ilx) = n1(ilx)
      if (ityp.eq.5.and.ist.eq.LBRCKTV) then
        call parser
        call exprs2
        if (ifl(2).gt.0) goto 999
        if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.ityp.ne.4.or.
     x       itv.lt.1) goto 9224
        if (nextyp.ne.COLONV) goto 9512
        if (itv.gt.n1(ilx)) goto 9085
        ix1(ilx) = itv
        call parser
        if (nextyp.ne.RBRCKTV) then
          call parser
          call exprs2
          if (ifl(2).gt.0) goto 999
          if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.ityp.ne.4.or.
     x         itv.lt.1) goto 9224
          if (nextyp.ne.RBRCKTV) goto 9462
          if (itv.gt.n1(ilx) .or. itv.lt.ix1(ilx)) goto 9085
          ix2(ilx) = itv
        endif
        call parser
        call parser
      endif
      str = str1(ilx)(ix1(ilx):ix2(ilx))
      len = ix2(ilx)-ix1(ilx)+1
      goto 999

c                   expression too long
9059  ifl(2)=59
      goto 999
c                   subscript out of range
9085  ifl(2)=85
      goto 999
c                   positive scalar expected
9224  ifl(2)=224
      goto 999
c                   Right bracket expected
9462  ifl(2)=462
      goto 999
c                   Colon expected.
9512  ifl(2)=512
      goto 999
c                   Text or text variable expected.
9513  ifl(2)=513
      goto 999

999   ilx=ilx-1
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine distfo (asw1, asw2)
C*      Function to calulate the distance between 2 geometric entities.
C*		function used before V9.6
C*    PARAMETERS
C*       INPUT  :
C*          asw1      - Associated word of first entity.
C*          asw2      - Associated word of second entity.
C*       OUTPUT :
C*          none
C*    RETURNS      : Distance in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine distfo (asw1, asw2)

      include 'com.com'

      real*8 asw1, asw2

      real*8 a1(11), a2(11), sec1,sec2,co,den,rho,dx,dy,dz
      integer*4 nclkey
      integer*2 nwds, it1, it2
      logical trflg
      data trflg /.true./

      call gtdesc (asw1, nclkey, nwds, it1)
      call gtdesc (asw2, nclkey, nwds, it2)

      if (it1.eq.CURVE) then
        nwds = asw2
        call crvlen (asw1, nwds, tv, ifl(2))
      else if ((it1.eq.SURF.or.it1.eq.PLANE).and.
     x         (it2.eq.SURF.or.it2.eq.PLANE)) then
        call gtplt(asw1, ifl(72), a1)
        call gtplt(asw2, ifl(72), a2)
        co=a1(1)*a2(1)+a1(2)*a2(2)+a1(3)*a2(3)
        if (dabs(co).le..9999995d0) goto 9250
c                 the planes are parallel
        if (co.le.0) a2(4)=-a2(4)
        tv=dabs(a1(4)-a2(4))
      else if (it1.eq.SURF.or.it2.eq.SURF) then
        if (it1.eq.SURF) then
          sc(11) = asw2
          sc(12) = asw1
        else
          sc(11) = asw1
          sc(12) = asw2
        endif
        isc10(2) = 1
        call sfptdo
        tv = sc(13)
      else
        call gtentt (asw1, trflg, nclkey, it1, a1)
        call gtentt (asw2, trflg, nclkey, it2, a2)
        if (it1.eq.LINE.and.it2.eq.LINE) then
c                its a dist between two lines
c                make sure they are parlel
           sec1=dsqrt(a1(4)**2+a1(5)**2+a1(6)**2)
           sec2=dsqrt(a2(4)**2+a2(5)**2+a2(6)**2)
           co=(a1(4)*a2(4)+a1(5)*a2(5)+a1(6)*a2(6))/sec1/sec2
           if (dabs(co).lt..999999999d0) goto 9344
           den=a2(4)**2+a2(5)**2+a2(6)**2
           rho=(a2(4)*(a1(1)-a2(1))+a2(5)*(a1(2)-a2(2))+
     1          a2(6)*(a1(3)-a2(3)))/den
           dx=a2(1)+rho*a2(4)-a1(1)
           dy=a2(2)+rho*a2(5)-a1(2)
           dz=a2(3)+rho*a2(6)-a1(3)
           tv=dsqrt(dx**2+dy**2+dz**2)
        else if ((it1.eq.POINT.or.it1.eq.PNTVEC).and.
     x           (it2.eq.POINT.or.it2.eq.PNTVEC)) then
            tv=dsqrt((a1(1)-a2(1))**2+(a1(2)-a2(2))**2+(a1(3)-a2(3))**2)
        else if ((it1.eq.POINT.or.it1.eq.PNTVEC).and.it2.eq.PLANE.or.
     x          ((it2.eq.POINT.or.it2.eq.PNTVEC).and.it1.eq.PLANE)) then
           if (it1.eq.PLANE) then
             tv=dabs(a1(4)-(a1(1)*a2(1)+a1(2)*a2(2)+a1(3)*a2(3)))
           else
             tv=dabs(a2(4)-(a1(1)*a2(1)+a1(2)*a2(2)+a1(3)*a2(3)))
           endif
        else if ((it1.eq.POINT.or.it1.eq.PNTVEC).and.it2.eq.LINE.or.
     x          ((it2.eq.POINT.or.it2.eq.PNTVEC).and.it1.eq.LINE)) then
           dx = a1(1) - a2(1)
           dy = a1(2) - a2(2)
           dz = a1(3) - a2(3)
           sec1 = dx*dx + dy*dy + dz*dz
           if (it2.eq.LINE) then
             den = a2(4)*a2(4)+a2(5)*a2(5)+a2(6)*a2(6)
             co = dx*a2(4) + dy*a2(5) + dz*a2(6)
           else
             den = a1(4)*a1(4)+a1(5)*a1(5)+a1(6)*a1(6)
             co = dx*a1(4) + dy*a1(5) + dz*a1(6)
           endif
           if (den .lt. 1.d-12) goto 9345
           sec2 = (co*co)/den
           if (sec1 .lt. sec2) goto 9345
           tv = dsqrt (sec1 - sec2)
        else
          goto 9345
        endif
      endif

      goto 999
c                   Plane must be parallel.
9250  ifl(2)=250
      goto 999
c                   Lines are not parallel.
9344  ifl(2)=344
      goto 999
c                   Invalid combination of geometry for dist.
9345  ifl(2)=345
      goto 999

999   continue
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine ndistfo
C*      Function to determine the distance between 2 entities.
C*		function used before V9.6
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : distance in tv
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ndistfo

      include 'com.com'
      include 'suvcom.com'

      integer*2 ilx
      equivalence (ifl(326),ilx)

      real*8 asw1(20), asw2(20)
      integer*4 nclkey
      integer*2 nwds, ietype

      ilx = ilx+1
      if (ilx.gt.20) goto 9059
      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.2) goto 9087
      if (ist.eq.1) goto 9009
      asw1(ilx) = tv
      if (ist.eq.SURF.and.nextyp.eq.15) then
        call prsuv2 (dsuv, dsu, dsv)
        if (ifl(2).gt.0) goto 999
      endif
      call parser
      call gtdesc (asw1(ilx),nclkey,nwds,ietype)
      if (ietype.eq.CURVE.and.ityp.eq.5.and.ist.eq.7) then
        asw2(ilx) = 200
      else
        if (ityp.ne.5.or.ist.ne.9) goto 9311
        call parser
        if (ityp.eq.2.and.ist.eq.14) call exprs2
        call gtdesc (asw1(ilx),nclkey,nwds,ietype)
        if (ietype.eq.CURVE) then
          call exprs2
          if (ifl(2).ne.0) goto 999
          if ((ityp.ne.2.or.ist.ne.2).and.ityp.ne.3.and.ityp.ne.4)
     x       goto 9007
        else
          if (ityp.ne.2) goto 9087
          if (ist.eq.1) goto 9009
          asw2(ilx) = tv
          if (ietype.eq.POINT.or.ietype.eq.PNTVEC) then
            if (ist.ne.POINT.and.ist.ne.PNTVEC.and.ist.ne.PLANE.and.
     x          ist.ne.LINE.and.ist.ne.SURF) goto 9345
            if (ist.eq.SURF.and.nextyp.eq.15) then
              call prsuv2 (dsuv, dsu, dsv)
              if (ifl(2).gt.0) goto 999
            endif
          else if (ietype.eq.LINE) then
            if (ist.ne.LINE.and.
     x          ist.ne.POINT.and.ist.ne.PNTVEC) goto 9345
          else if (ietype.eq.SURF .or.ist.eq.PLANE.and.
     x             ietype.eq.PLANE.or.ist.eq.SURF) then
            if (ietype.eq.SURF) then
              call gtdesc(asw1(ilx),nclkey,nwds,ietype)
              call ncl_get_sf_primtyp(nclkey,ietype)
              if (ietype.ne.3) goto 9345
            endif
            if (ist.eq.SURF) then
              call gtdesc(asw2(ilx),nclkey,nwds,ietype)
              call ncl_get_sf_primtyp(nclkey,ietype)
              if (ietype.ne.3) goto 9345
            endif
          else if (ietype.eq.PLANE) then
            if (ist.ne.POINT.and.ist.ne.PNTVEC.and.ist.ne.PLANE)
     x         goto 9345
          else if (ietype.eq.SURF) then
            if (ist.ne.POINT.and.ist.ne.PNTVEC) goto 9345
          else
            goto 9345
          endif
        endif
        call parser
        if (ityp.ne.5.or.ist.ne.7) goto 9310
      endif
      call distfo (asw1(ilx),asw2(ilx))
      goto 999
c                   number or scalar expected
9007  ifl(2)=7
      goto 999
c                   Identifier not previously defined
9009  ifl(2)=9
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   Identifier expected
9087  ifl(2)=87
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999
c                   Invalid combination of geometry for dist
9345  ifl(2)=345
      goto 999

999   ilx=ilx-1
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine ntextf
C*      Function to return the label string of an entity.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : label in token2
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ntextf

      include 'com.com'

      integer*2 ilx,i
      equivalence (ifl(326),ilx)

      character*64 label,subscr
      integer*4 len, len2
      integer*4 strlen1
      character*64 svtok2
      integer*2 n1, ix, jstat
      integer*4 keydt,sub,isub
      character*(24) cvoc

      ifl(379) = 1
      ilx = ilx+1
      if (ilx.gt.20) goto 9059

      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if ((ityp.eq.2).and.(ist.eq.DATAST)) then
c
c.....data statement, must followed by index [n]
c
          svtok2 = token2
          call gtdesc(tv, keydt, n1, ist)
          if (n1.lt.1) goto 9310
          call parser
          if (ityp.ne.5.or.ist.ne.15) goto 9309
          call parsit
          if ((ityp.eq.2.and.ist.eq.2) .or.
     1       (ityp.eq.3) .or. (ityp.eq.3)) then
             ix = tv
             call parser
             if (ityp.ne.5.or.ist.ne.16) goto 9310
          endif
          call dtgetv (keydt, ix, tv ,ityp, nextyp,label,sub)
          token2 = label
          ivxsub = sub         
      endif
c
c...need consider vocab word
c...yurong
c
      if (ityp.eq.1) then
          i = iabs(ist)
          call asvoc (i, 0, cvoc)
          token2 = cvoc
      endif
      len = strlen1(token2)
      label=token2
      if (ifl(2).ne.0) goto 999
      isub =ivxsub
      call itoc(isub,subscr,len2)
      call parser
      if (ityp.ne.5.or.ist.ne.7) goto 9310
      ityp = 9
      token2 = label
      length = len
      if (isub.gt.0) then
        len = len+1
        token2(len:len) ='('
        len = len+1
        token2(len:len+len2) = subscr
        len = len+len2
        token2(len:len) =')'
        length = len
      endif
      tokstr = token2
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999

999   ilx=ilx-1
      return
      end
