c **********************************************************************
c**    NAME           : razpk1.f
c**      CONTAINS:
c**   subroutine arrinp
c**   subroutine fedout (fout, fhld)
c**   subroutine poklst (istep, buf, last)
c**   subroutine pokcir (kk, istep, buf, trans, mx, ier)
c**   subroutine pklcou
c**   subroutine pklcyc
c**   subroutine pokerr (ier, errstr)
c**   subroutine scllan (istrt, iend, fact)
c**   subroutine trncnt (buf, mx, ietype)
c**   subroutine rrpsrf (trans, mx, ier)
c**   subroutine tlprof
c**   subroutine sfparm (asn, thk, buf, trans, mx, ier)
c**   subroutine surftl (itp, ind, buf, trans, mx, ier)
c**   subroutine tlaxsf (te, ta, tp, sn, ier)
c**   subroutine tlpnts (ipt, te, ta, tp, sn, vn)
c**   subroutine tlgoug (te, ta, tp, sn, ig)
c**   subroutine tlupta (rd, cn, tn, te, ta, tp, sn, ig)
c**   subroutine srfhgt (ta, dhi, dlo, trans, mx, ier)
c**   subroutine srfout (kk, istep, buf, trans, mx, ier)
c**   subroutine poksto (buf, mtype, trans, mx, ier)
c**   
c**    MODULE NAME AND RELEASE LEVEL
c**       razpk1.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:34
c **********************************************************************
c**
c** copyright (c) 2003 NCCS
c**
c **********************************************************************
c **********************************************************************
c **   subroutine name:  arcinp
c **                                                                  
c **  purpose of subroutine: input arc point.
c **
c **********************************************************************
c **********************************************************************
      subroutine arcinp(xrr,yrr,ixxy,x,y,r,ccw,xc,yc)

      include 'pokcom.com'

      real*4 xrr(3000),yrr(3000),ccw
      integer*2 ixxy
      real*8 x,y,xc,yc,r
      
          xrr(ixxy) = x
          yrr(ixxy) = y
          xrr(ixxy+1) = flarc
          yrr(ixxy+1) = 0.
          xrr(ixxy+2) = r
          yrr(ixxy+2) = ccw
          xrr(ixxy+3) = xc
          yrr(ixxy+3) = yc
          ixxy = ixxy + 4

      return
      end

c **********************************************************************
c **********************************************************************
c **  2. subroutine name:  fedout
c **                                                                  
c **  purpose of subroutine: Output feedrate commands.
c **
c **********************************************************************
c **********************************************************************

      subroutine fedout (fout, fhld)

      include 'com8a.com'

      real*8 fout, fhld

c... If the FOUT feedrate is greater than zero (0) and different
c... than the feedrate in effect (FHLD), put out a feedrate 
c... command with the FOUT value.
      if (fout.gt.0) then
cc        if (rpfron .and. ifl(270).gt.0) then
c... dump display buffer before a feedrate if rapid
cc          ifl(270) = 0
cc          call motend
cc        endif
        if (fout.ne.fhld) then
          call putcl (2000,1009,2,fout)
          fhld = fout
        endif
        rpfron = .false.
c... If the FOUT feedrate is zero (0) or less, put out a RAPID
c... feedrate command.
      else
cc        if (.not.rpfron .and. ifl(270).gt.0) then
c... dump display buffer before a rapid if feedrate
cc          ifl(270) = 0
cc          call motend
cc        endif
        call putcl (2000, 5, 1, 0.)
        rpfron = .true.
      endif

99999 return
      end

c **********************************************************************
c **********************************************************************
c **  3. subroutine name:  poklst
c **                                                                  
c **  purpose of subroutine: Print pocket motion
c **
c **********************************************************************
c **********************************************************************

      subroutine poklst (istep, buf, last)

      include 'com8a.com'

      integer*2 istep, knt, j
      real*8 buf(6)
      logical last

      real*8 bufl(6)

      if(motdfl) then
          if (istep.eq.0) then
            if (ifl(82).eq.0) then
              write(cout,1010)
1010          format(' step no.',5x,'x',11x,'y',11x,'z')
              knt = 39
            else
              write(cout,1015)
1015          format(' step no.',5x,'x',11x,'y',11x,'z',11x,'i',11x,'j',
     1               11x,'k')
              knt = 75
            endif
            call putmsg (cout,knt,1,0)
          endif
          if (ifl(154).eq.1.or.istep.eq.0.or.last) then
c                            first or last step or print large
            knt = 3
            if (ifl(82).eq.1) knt = 6
            do 100 j=1,6
  100       bufl(j) = buf(j)
            if (ifl(73).eq.1.and.ifl(267).eq.1) then
              call conent(bufl, sc(41), 3)
              call conent(bufl(4), sc(41), 4)
            endif
            istep=istep+1
            write(cout,1020) istep,(bufl(j),j=1,knt)
1020        format(i6,1x,6f12.4)
            call putmsg(cout,80,1,0)
          endif
      endif

99999 return
      end

c **********************************************************************
c **********************************************************************
c **  5. subroutine name:  pokcir
c **                                                                  
c **  purpose of subroutine: Output circular graphic motion and CL from 
c **                         pocket routines arrays.
c **
c **********************************************************************
c **********************************************************************

      subroutine pokcir (kk, istep, buf, trans, mx, ier)

      include 'com.com'
      include 'const.com'
      include 'pokcom.com'
      include 'pokpl.com'

      integer*2 istep
      integer*4 ier
      real*8 buf(6), mx(12)
      logical trans

      integer*2 i, l, n, npts, mtype
      real*8 bufl(7)
      real*8 rad, xc, yc, vsx, vsy, vex, vey, ang, dang, co
      real*8 cclw, si
      integer*2 i2v2 /2/, i2v3 /3/, i2v4 /4/

      integer*4 jj, kk
      real*8 xx,yy,x1,y1,x2,y2,x3,y3

      mtype = 0
      if (sfcut .and. botpl(4) .lt. botpl(1)) then
        call srfout (kk, istep, mtype, buf, trans, mx, ier)
      else
        jj = kk - 1
        call rrget(jj,xx,yy,ier)
        if (ier.gt.0) goto 999
        jj = kk + 1
        call rrget(jj,x1,y1,ier)
        if (ier.gt.0) goto 999
        jj = kk + 2
        call rrget(jj,x2,y2,ier)
        if (ier.gt.0) goto 999
        jj = kk + 3
        call rrget(jj,x3,y3,ier)
        if (ier.gt.0) goto 999

        rad = x1
        if (rad.lt.tol) goto 999

        xc = x2
        yc = y2
        vsx = (xx - xc)/rad
        vsy = (yy - yc)/rad
        vex = (x3 - xc)/rad
        vey = (y3 - yc)/rad
        co = vsx*vex+vsy*vey
        if (co.gt.1.) co = 1.
        if (co.lt.-1.) co = -1.
        ang = dacos(co)

        cclw = y1

        si = (vsx*vey-vsy*vex)*cclw
        if (si.lt.0.) ang = TWO_PI-ang
        dang = ang
        if (rad.gt.tol) dang = 2.*dacos((rad-tol)/rad)
        npts = ang/dang
        dang = npts+1
        dang = cclw*ang/dang
        co = vsx
        if (co.gt.1.) co = 1.
        if (co.lt.-1.) co = -1.
        ang = dacos(co)
        if (vsy.lt.0) ang = -ang
        if (ifl(94).eq.1 .and. ifl(144).eq.1 .and. .not. sfcut) then
c... circular interpolation
          bufl(1) = xc
          bufl(2) = yc
          bufl(3) = buf(3)
          if (trans) call conent (bufl, mx, i2v3)
          bufl(4) = buf(4)
          bufl(5) = buf(5)
          bufl(6) = buf(6)
          if (trans) call conent (bufl(4), mx, i2v4)
          bufl(7) = rad
          call pklsto (i2v2, mtype, bufl, ier)
          if (ier.gt.0) goto 999
        endif

        mtype = 0
        l = ifl(82)
        n = npts+1
        do 100 i=1,n
          if (i.lt.n) then
            ang = ang+dang
            buf(1) = xc+rad*dcos(ang)
            buf(2) = yc+rad*dsin(ang)
          else

            buf(1) = x3
            buf(2) = y3
          endif
          call poksto (buf, mtype, trans, mx, ier)
          if (ier .ne. 0) go to 999
100     continue
      endif

999   return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name:  pklcou
c **                                                                  
c **  purpose of subroutine: Output circular graphic motion and CL from 
c **                         pocket routines arrays.
c **
c **********************************************************************
c **********************************************************************

      subroutine pklcou (iramps,dz,xc,yc,rad,trans,mx,ier)

      include 'com.com'

      integer*2 iramps
      real*4 dz,rad,xc,yc
      real*8 mx(12)
      logical trans
      integer*4 ier

      integer*2 itype,mtype
      real*8 bufl(9)
      integer*2 i2v3 /3/, i2v4 /4/
      integer*4 j

      itype = 5
      mtype = 0

      j = 10000.*(dz/iramps) + 0.5
          bufl(1) = j/10000.
          bufl(2) = iramps
      j = 10000.*xc + 0.5
          bufl(3) = j/10000.
      j = 10000.*yc + 0.5
          bufl(4) = j/10000.
          bufl(5) = 0
          if (trans) call conent (bufl(3), mx, i2v3)
          bufl(6) = 0
          bufl(7) = 0
          bufl(8) = 1
          if (trans) call conent (bufl(6), mx, i2v4)
      j = 10000.*rad + 0.5
          bufl(9) = j/10000.

          call pklsto (itype, mtype, bufl, ier)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name:  pklcyc
c **                                                                  
c **  purpose of subroutine: Output circular graphic motion and CL from 
c **                         pocket routines arrays.
c **
c **********************************************************************
c **********************************************************************

      subroutine pklcyc (dz,rad,dzi,fent,rdis,ang,ier)

      include 'com.com'
      include 'const.com'
      include 'rrdm.com'

      real*4 dz,rad,dzi,rdis
      real*8 fent
      integer*4 ier

      integer*2 itype,mtype
      real*8 bufl(6)
      integer*4 j

      itype = 6
      mtype = 0

      j = 10000.*dz + 0.5
          bufl(1) = j/10000.
      if (keytxt .eq. 0) then
          j = 10000.*rad + 0.5
      else
          j = 10000.*(2.*rad + sc(28)) + 0.5
      endif
          bufl(2) = j/10000.
      j = 10000.*dzi + 0.5
          bufl(3) = j/10000.
          bufl(4) = fent
      j = 10000.*rdis + 0.5
          bufl(5) = j/10000.
      if (keytxt .eq. 0) then
          j = 10000.*ang*RADIAN + 0.5
          bufl(6) = j/10000.
      else
          bufl(6) = 45.
      endif

      call pklsto (itype, mtype, bufl, ier)

      return
      end

c **********************************************************************
c **********************************************************************
c **  6. subroutine name:  pokerr
c **                                                                  
c **  purpose of subroutine: Load proper error message in errcom area 
c **
c **********************************************************************
c **********************************************************************

      subroutine pokerr (ier)

      include 'com8a.com'

      integer*4 ier

      if (ier.eq.1) then
        if (ifl(2) .le. 0) ifl(2) = 488
c       errstr = 'system inners are not right'
c     else if (ier.eq.2) then
c       errstr = 'TOO MANY POINTS IN POCKET'
c     else if (ier.eq.3) then
c       errstr = 'TOO MANY POINTS IN ISLAND'
      else if (ier.eq.4) then
        ifl(2) = 489
c       errstr = 'cannot project tool to surface'
      else if (ier.eq.5) then
        ifl(2) = 490
c       errstr = 'cannot find tool surface pts'
      else if (ier.eq.6) then
        ifl(2) = 491
c       errstr = 'entry gouges surface bottom'
      else if (ier.eq.7) then
        ifl(2) = 492
c       errstr = 'entry gouges geometry'
      else if (ier.eq.8) then
        ifl(2) = 493
c       errstr = 'TOO MANY INPUT POINTS'
      else if (ier.eq.9) then
        ifl(2) = 494
c       errstr = 'ILLEGAL START POINT'
      else if (ier.eq.10) then
        ifl(2) = 495
c       errstr = 'TOO MANY LOOPS'
      else if (ier.eq.11) then
        ifl(2) = 496
c       errstr = 'TOO MANY POINTS IN LOOP CALC'
      else if (ier.eq.12) then
        ifl(2) = 497
c       errstr = 'ILLEGAL END POINT'
      else if (ier.eq.13) then
        ifl(2) = 498
c       errstr = 'END LOOP TOO SMALL'
      else if (ier.eq.14) then
        ifl(2) = 499
c       errstr = 'POCKET & ISLAND LOOPS DON`T CUT'
      else if (ier.eq.51) then
        ifl(2) = 500
c       errstr = 'CANNOT MOVE BETWEEN LOOPS'
      else if (ier.eq.15) then
        ifl(2) = 510
c       errstr = 'POCKET EXIT VIOLATES GEOMETRY'
      else if (ier.eq.16) then
        ifl(2) = 519
c       errstr = 'POCKET: BAD MAX/MIN STEP VALUES'
      else
        ifl(2) = 177
      endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  7. subroutine name:  scllan (istrt,iend,fact)
c **                                                                  
c **  purpose of subroutine: Scale pocket lanes by factor
c **
c **********************************************************************
c **********************************************************************

      subroutine scllan (istrt,iend,fact)

      include 'com4a.com'
      include 'pokcom.com'

      integer*2 istrt,iend
      real*4 fact

      integer*2 i, ier

      do 100 i=istrt,iend
        if (lane(i).eq.999) goto 100
        call rrscal(loca(i),loca(i+1),fact,ier)
  100 continue

  999 return
      end

c **********************************************************************
c **********************************************************************
c **  8. subroutine name:  trncnt (buf, mx, ietype)
c **                                                                  
c **  purpose of subroutine: transform composite curve entities
c **
c **********************************************************************
c **********************************************************************

      subroutine trncnt (buf, mx, ietype)

      include 'com4a.com'

      real*8 buf(14), mx(12)
      integer*2 ietype
      integer*2 it

      it = 3
      if (ietype.eq.5) then
        call conent (buf, mx, it)
        call conent (buf(4), mx, it)
      else if (ietype.eq.7) then
        call conent (buf, mx, it)
        call conent (buf(4), mx, it)
        call conent (buf(7), mx, it)
        it = 4
        call conent (buf(10), mx, it)
      endif

  999 return
      end

c **********************************************************************
c **********************************************************************
c **  12. subroutine name:  rrpsrf
c **                                                                  
c **  purpose of subroutine: initializes the scratch area one, surface
c **                         and point common parameters and builds the
c **                         tool offset to pocket bottom in scratch
c **                         area one.  surface high/low distances are
c **                         botpl(1) and botpl(2) respectively.
c **  parameters
c **     input  :  xyplane data from rr common output of rrpock.
c **
c **     output :  scratch area one is opened and xyplane points from
c **               rrpock are pierced along the tool axis to the surface
c **               as a 15 element array.
c **                 ier = 1 - out of scratch areas from scrall
c **                 ier = 1/4 - errors from sfparm and surftl
c **                 ier = 1/5 - errors from srfhgt
c **
c **     format :  a tool surface pt contains the following real*8 data
c **                 idp - tool pt id - two integer*4 values of:
c **                       point type and index ids
c **                 xpl - x xyplaner value, not translated
c **                 ypl - y xyplaner value, not translated
c **                 te  - tool end, xyz, offset to surface
c **                 ta  - tool axis, ijk
c **                 tp  - tangent point, xyz, on surface
c **                 sn  - surface normal, ijk, at tp.
c **        
c **********************************************************************
c **********************************************************************

      subroutine rrpsrf (idir, trans, mx, ltoppl, lsc1pt, sc1lev, ier)

      include 'com8a.com'
      include 'mocom.com'
      include 'const.com'
      include 'rrdm.com'
      include 'pokcom.com'
      include 'pokpl.com'

      common/rrs1/ idf1, idr1, msz1, dwa1, dwc1, dws1

      common/rrsf/ asf, usf, vsf, sfdr, sftk, sftb(7)

      common/rrpt/ tldr(3), bs(15), lnd

      real*8 sc1lev
      logical ltoppl,lsc1pt

      common/secpsf/ secdir,usec,vsec
      real*4 secdir,usec,vsec

      common/tavekt/ tavk,tainit
      integer*2 tainit
      real*8 tavk(3)

      real*8 tldr, bs
      real*8 asf, buf(6), mx(12)
      real*4 usf, vsf, sfdr, sftk, sftb
      real*4 rad, xc, yc, vs(2), ve(2), ang, dang, cs, si, ccw
      integer*4 idf1, idr1, msz1, dwa1, dwc1, dws1
      integer*4 lnd, ind, ltp, itp, ier
      integer*2 idir, istr, iend, i, j, n, ltlaxs
      logical trans

      integer*4 kk
      real*8 xx,yy,x0,y0,x1,y1,x2,y2,x3,y3,dis

      real*4 srf(10,4)
      equivalence (srf,s)
      integer*2 isf
      real*8 tbuf(6)
      integer*2 i2v3 /3/, i2v4 /4/

c... set up scratch area one
      call scrdea (idf1)
      call scrall (idf1,ier)
      if (ier .ne. 0) goto 999
      idr1 = 0
      msz1 = 0
      dwa1 = 1
      dwc1 = 15
      dws1 = 8
c... get tool profile
      call tlprof
c... get near mid pt and set up surface data
      ind = loca(locapo)
      itp = lane(locapo)
      tainit = 0
      kk = ind
      call rrget(kk,x0,y0,ier)
      if (ier .ne. 0) goto 999
      buf(1) = x0
      buf(2) = y0
      if (lsc1pt) then
c
c..... if both clpl and toppl are defined as a distance, use the starting
c..... point level for the first surface projection
c
        buf(3) = sc1lev
      else
        buf(3) = clpl(4)
      endif
      buf(4) = 0.0
      buf(5) = 0.0
      buf(6) = 1.0
      ltlaxs = tamode
      tamode = 0
      call sfparm (sc(11), sc(23), buf, trans, mx, ier)
      if (ier .ne. 0) goto 999
      tamode = ltlaxs

      if (ltoppl) then
        dis = buf(1)*toppl(1)+buf(2)*toppl(2)+buf(3)*toppl(3)
        toppl(4) = (toppl(4) - dis)/toppl(3)
      endif

      if (tamode .eq. 2) then
        isf = 4
        usec = 0.5
        vsec = 0.5
        call sfinit(sc(19),isf,usec,vsec)
        do i=1,6
          tbuf(i) = buf(i)
        enddo

        if (trans) then
          call conent (tbuf, mx, i2v3)
          call conent (tbuf(4), mx, i2v4)
        endif

        call sfptvc(sc(19),tbuf,tbuf(4),isf,usec,vsec,ierr)
      
        if (ierr.ne.0) then
          tamode = 1
        else
          secdir = 1
          cs = 0

          do i=1,3
            cs = cs + tbuf(3+i)*srf(i,isf)
          enddo

          if (cs .lt. 0) secdir = -1
        endif
      endif
c
c... loop through all xyplaner pts
c
      istr = locapo
      iend = lanes-1
      if (idir .lt. 0) then
c
c... loop backwards through pts
c
        istr = lanes-1
        iend = locapo
      endif
      ltp = 0
      do 300 i=istr,iend,idir
        itp = lane(i)
        if (itp .eq. 999) goto 250
        if (itp .ne. ltp) itp = -itp
        ind = loca(i)
100     kk = ind
        call rrget(kk,x0,y0,ier)
        if (ier .ne. 0) goto 999
        if (x0 .lt. 0.9*flarc) then
          buf(1) = x0
          buf(2) = y0
c
c... straight to pt
c
          call surftl (itp, ind, buf, trans, mx, ier)
          if (ier .ne. 0) goto 999
          itp = iabs (itp)
          ind = ind + 1
        else
          kk = ind+1
          call rrget(kk,x1,y1,ier)
          if (ier .ne. 0) goto 999
          rad = x1
          if (rad .ge. tol) then
c
c... circle to pt
c
            kk = ind-1
            call rrget(kk,xx,yy,ier)
            if (ier .ne. 0) goto 999
            kk = ind+2
            call rrget(kk,x2,y2,ier)
            if (ier .ne. 0) goto 999
            kk = ind+3
            call rrget(kk,x3,y3,ier)
            if (ier .ne. 0) goto 999

            ccw = y1
            xc = x2
            yc = y2
            vs(1) = (xx - xc)/rad
            vs(2) = (yy - yc)/rad
            ve(1) = (x3 - xc)/rad
            ve(2) = (y3 - yc)/rad
            cs = vs(1)*ve(1) + vs(2)*ve(2)
            if (cs .gt.  1.0) cs = 1.0
            if (cs .lt. -1.0) cs = -1.0
            ang = acos (cs)
            si = (vs(1)*ve(2) - vs(2)*ve(1))*ccw
            if (si .lt. 0.0) ang = TWO_PI - ang
            dang = ang
            if (rad .gt. tol) dang = 2.*acos ((rad-tol)/rad)
            n = ang/dang
            dang = n + 1
            dang = ccw*ang/dang
            cs = vs(1)
            if (cs .gt.  1.0) cs = 1.0
            if (cs .lt. -1.0) cs = -1.0
            ang = acos (cs)
            if (vs(2) .lt. 0.0) ang = -ang
            do 200 j=1,n+1
            ang = ang + dang
            buf(1) = xc + rad*cos (ang)
            buf(2) = yc + rad*sin (ang)
            call surftl (itp, ind, buf, trans, mx, ier)
            if (ier .ne. 0) go to 999
            itp = iabs (itp)
200         continue
          endif
          ind = ind + 4
        endif
        if (ind .lt. loca(i+1)) goto 100
250     ltp = itp
300   continue
c
c... find max/min height along tool axis
c
      call srfhgt (buf(4), botpl(1), botpl(2), trans, mx, ier)
c
c... reset read direction of scratch one
c
      idr1 = 0

999   tainit = 0
      return
      end

c **********************************************************************
c **********************************************************************
c **  13. subroutine name:  tlprof
c **                                                                  
c **  purpose of subroutine: to build the tool profile segment array
c **                         from the tool definition in the system.
c **                         The tool profile array is used to define
c **                         tool offset for pockets to a surface.
c **  parameters
c **     output :  array of tool segment are created with end points
c **               of a segment being the start point of the next. The
c **               segments are as follows:
c **                              1. zs
c **                              2. xs
c **                              3. cosine of start point normal
c **                              4. radius of arc or zero if straight
c **                              5. zc if arc
c **                              6. xc if arc
c **                              7. cosine of end point normal
c **                              8. ze
c **                              9. xe
c **        
c **********************************************************************
c **********************************************************************

      subroutine tlprof

      include 'com8a.com'

      common/rrpf/tlsegs(50), ntlsgs

      real*8 tlsegs
      real*8 dia, r1, hgt, bet, snb, csb
      real*8 x, r2, z2, x2, sna, csa
      real*4 asc(310)
      integer*2 ntlsgs, i
      equivalence (asc(1),sc(1))
      parameter (stol = .000001)

c... cutter parameters as created by cutseg or barseg routines
      dia = sc(28)
      r1 = sc(29)
      hgt = sc(30)
      bet = sc(31)
      snb = asc(63)
      csb = asc(64)
      if (ifl(282) .eq. 1) then
c... barrel cutter
        r2 = asc(307)
        z2 = asc(68)
        x2 = asc(67)
        sna = asc(305)
        csa = dsqrt(1.0-sna**2)
      else
c... non barrel cutter
        r2 = 0.0
        z2 = 0.0
        x2 = 0.0
        sna = snb
        csa = csb
      endif
      i = 0
      tlsegs(i+1) = 0.0
      tlsegs(i+2) = 0.0
      x = .5*dia - r1
      if (x .gt. 0.0) then
c... tool flat
        tlsegs(i+3) = 1.0
        tlsegs(i+4) = 0.0
        tlsegs(i+7) = 1.0
        tlsegs(i+8) = 0.0
        tlsegs(i+9) = x
        i = i+7
      endif
      if (r1 .gt. 0.0) then
c... corner radius
        tlsegs(i+3) = 1.0
        tlsegs(i+4) = r1
        tlsegs(i+5) = r1
        tlsegs(i+6) = tlsegs(i+2)
        tlsegs(i+7) = snb
        tlsegs(i+8) = r1-r1*snb
        tlsegs(i+9) = tlsegs(i+2) + r1*csb
        i = i+7
      endif
      if (r2 .gt. 0.0) then
c... side radius
        tlsegs(i+3) = snb
        tlsegs(i+4) = r2
        tlsegs(i+5) = z2
        tlsegs(i+6) = x2
        tlsegs(i+7) = sna
        tlsegs(i+8) = z2 - r2*sna
        tlsegs(i+9) = x2 + r2*csa
        i = i+7
      endif
      if (hgt .gt. tlsegs(i+1)) then
c... side straight
        tlsegs(i+3) = sna
        tlsegs(i+4) = 0.0
        tlsegs(i+7) = sna
        tlsegs(i+8) = hgt
        x = tlsegs(i+2)
        if (csa .gt. stol) x = x + (hgt - tlsegs(i+1))/csa*sna
        tlsegs(i+9) = x
        i = i+7
      endif
      ntlsgs = i/7

      return
      end

c **********************************************************************
c **********************************************************************
c **  14. subroutine name:  sfparm
c **                                                                  
c **  purpose of subroutine: to initialize the surface parameters and
c **                         to calculate a trial point to the surface
c **                         to verify the configuration.
c **  parameters
c **     input  :  buf contains a point near the centroid of the pocket.
c **
c **     output :  the surface and point commons are initialized and the
c **               surface is pierced along the tool axis from its cutter
c **               position in buf.
c **                  ier = .eq. 1 - zero surface id
c **                             4 - errors from surftl
c **        
c **********************************************************************
c **********************************************************************

      subroutine sfparm (asn, thk, buf, trans, mx, ier)

      include 'com.com'
      include 'suvcom.com'
      include 'pokcom.com'

      real*8 asn, thk, buf(7), mx(12)
      integer*4 ier
      logical trans

      common/rrsf/ asf, usf, vsf, sfdr, sftk, sftb(7)
      common/rrpt/ tldr(3), bs(15), lnd

      real*8 asf
      real*4 usf, vsf, sfdr, sftk, sftb
      real*8 tldr, bs
      integer*4 lnd
      
      real*8 tbuf(6), tol8
      integer*4 itp, ind, isf(2), kps
      equivalence (isf(1), asf)
      integer*2 i, nwds, ietype, imult, iscv
      integer*2 i2v3, i2v4
      data i2v3 /3/, i2v4 /4/

c... set surface common
      asf = asn
c... check for surface id
      if (isf(1) .eq. 0) then
        ier = 1
        go to 999
      endif
      if (sc(169).gt.8.229999d0) then
        call gettol(tol8)
        call gtdesc(asf,kps,nwds,ietype)
        if (ietype.eq.surf .and. sc(169).ge.9.349) then
          ifl(331) = 0
          psu = 0.5
          psv = 0.5
        endif
        iscv = 0
        ifl(2) = ncl_psmult_init(kps,tol8,imult,iscv)
        if (ifl(2).gt.0) then
          if (ifl(2).ne.466) ifl(2) = 122
          ier = 1
          goto 999
        endif
        psmult = imult.eq.1

          do 20 i=1,6
20        tbuf(i) = buf(i)

        if (trans) then
          call conent (tbuf, mx, i2v3)
          call conent (tbuf(4), mx, i2v4)
        endif
        if (ietype.eq.surf .and. .not.psmult .and. sc(169).ge.9.449)
     x                                                          then
          itp = 2
          call psini1 (itp,asf,tbuf,tbuf(4))
        else
          call psinit (asf, tbuf, tbuf(4))
        endif

        if (ifl(2).gt.0) then
          ier = 1
          goto 999
        endif
      endif
c
c..... the rrsf common block (in particular the four variables initialized
c..... below) are used in obsolete code only
c
      usf = .5
      vsf = .5
      sfdr = 0.0
      sftk = thk
c
c... pierce surface along ta with tool and initialize rrpt
c
      itp = -1
      ind = 0
      call surftl (itp, ind, buf, trans, mx, ier)

999   return
      end

c **********************************************************************
c **********************************************************************
c **  15. subroutine name:  surftl
c **                                                                  
c **  purpose of subroutine: generates tool positions along a surface
c **                         bottom by projecting the tool down its tool
c **                         axis and checking its linear move from the
c **                         last entry and surface contour.
c **  parameters
c **     input  :  rrpf, rrsf, rrpt and rrs1 common values are assumed
c **               initialized.  the inputs are:
c **                 itp - point type
c **                     .lt. 0 - initialize rrpt common
c **                     .ge. 0 - continuation pt
c **                 ind - point index
c **                     .le. 0 - pt return in BUF only
c **                     .gt. 0 - scratch output
c **                 buf - x,y,z, 0,0,1 pierced pt in xyplane system
c **
c **     output :  Output is stored directly into a scratch area one if
c **               ier .eq. 0. (see format below)
c **                 ier = 1 - scratch area not allocated from scrsto
c **                 ier = 4 - errors from tlaxsf
c **
c **     format :  a tool surface pt contains the following real*8 data
c **                 idp - tool pt id - two integer*4 values of:
c **                       point type and index ids
c **                 xpl - x xyplaner value, not translated
c **                 ypl - y xyplaner value, not translated
c **                 te  - tool end, xyz, offset to surface
c **                 ta  - tool axis, ijk
c **                 tp  - tangent point, xyz, on surface
c **                 sn  - surface normal, ijk, at tp.
c **
c **********************************************************************
c **********************************************************************

      subroutine surftl (itp, ind, buf, trans, mx, ier)

      include 'com8a.com'
      include 'rrdm.com'
      include 'pokcom.com'

      integer*4 itp, ind, ier
      real*8 buf(6), mx(12)
      logical trans

      common/rrs1/ idf1, idr1, msz1, dwa1, dwc1, dws1
      common/rrpt/ tldr(3), bs(15), lnd

      real*8 tldr, bs, tes(3), tas(3), tps(3), sns(3)
      real*8 b(15), te(3), ta(3), tp(3), sn(3)
      real*8 be(15), tee(3), tae(3), tpe(3), sne(3)
      real*8 bc(15), tec(3), tac(3), tpc(3), snc(3)
      real*8 vt(3), dst, v(3), dsn, dt, dn
      integer*4 idf1, idr1, msz1, dwa1, dwc1, dws1
      integer*4 lnd, idp(2)
      integer*2 i, imult
      logical lv82
      equivalence (b(1),idp(1))
      equivalence (b(4),te(1)), (b(7),ta(1)), (b(10),tp(1))
      equivalence (b(13),sn(1))
      equivalence (bs(4),tes(1)), (bs(7),tas(1)), (bs(10),tps(1))
      equivalence (bs(13),sns(1))
      equivalence (be(4),tee(1)), (be(7),tae(1)), (be(10),tpe(1))
      equivalence (be(13),sne(1))
      equivalence (bc(4),tec(1)), (bc(7),tac(1)), (bc(10),tpc(1))
      equivalence (bc(13),snc(1))
      real*8 dismin
      parameter (pih = 1.5/3.141592653589793)

      lv82 = sc(169).lt.8.229999d0
      dismin = tol*2.0
      if (lv82) dismin = stpmin
      imult = ifl(82)
c... set up input
      if (itp .lt. 0 ) lnd = 0
      idp(1) = iabs (itp)
      idp(2) = ind
      b(2) = buf(1)
      b(3) = buf(2)
      do 10 i=1,3
      b(i+3) = buf(i)
10    b(i+6) = buf(i+3)
      if (trans) then
        call conent (b(4), mx, 3)
        call conent (b(7), mx, 4)
      endif
      if (lnd .gt. 0) then
c... determine tool direction
        v(1) = b(4) - bs(4)
        v(2) = b(5) - bs(5)
        v(3) = b(6) - bs(6)
        dst = v(1)*ta(1) + v(2)*ta(2) + v(3)*ta(3)
        tldr(1) = v(1) - dst*ta(1)
        tldr(2) = v(2) - dst*ta(2)
        tldr(3) = v(3) - dst*ta(3)
        dst = dsqrt (tldr(1)**2 + tldr(2)**2 + tldr(3)**2)
      else
        dst = 0.0
      endif
      if (dst .eq. 0.0) then
c... pick a tool direction via ta
        if (dabs(ta(3)) .gt. tol) then
          tldr(1) = ta(3)
          tldr(2) = 0.0
          tldr(3) = -ta(1)
        else if (dabs(ta(2)) .gt. tol) then
          tldr(1) = 0.0
          tldr(2) = -ta(3)
          tldr(3) = ta(2)
        else
          tldr(1) = -ta(2)
          tldr(2) = ta(1)
          tldr(3) = 0.0
        endif
        dst = dsqrt(tldr(1)**2 + tldr(2)**2 + tldr(3)**2)
      endif
      tldr(1) = tldr(1)/dst
      tldr(2) = tldr(2)/dst
      tldr(3) = tldr(3)/dst
c... input pt to surface
      call tlaxsf (te, ta, tp, sn, ier)
      if (ier .ne. 0) goto 999
      if (lnd .eq. 0) then
        if (ind .gt. 0) then
c... output single pt
          call scrst0 (idf1, b, dwa1, dwc1, dws1, ier)
          if (ier .ne. 0) goto 999
          dwa1 = dwa1 + dwc1
          if (dwa1 .gt. msz1) msz1 = dwa1-1
        else if (tamode .ge. 1) then
          call tltos0 (te, ta, tp, sn, ier)
          if (ier .ne. 0) goto 999
        endif
c... store single pt for next entry
        do 20 i=1,15
20      bs(i) = b(i)
        goto 920
      endif
c... store end pt
      do 30 i=1,15
      bc(i) = b(i)
30    be(i) = b(i)
c... steps by tool ends and tolerance by tangent pts
100   v(1) = te(1)-tes(1)
      v(2) = te(2)-tes(2)
      v(3) = te(3)-tes(3)
      dst = ta(1)*v(1) + ta(2)*v(2) + ta(3)*v(3)
      vt(1) = v(1) - dst*ta(1)
      vt(2) = v(2) - dst*ta(2)
      vt(3) = v(3) - dst*ta(3)
      dst = dsqrt (vt(1)**2 + vt(2)**2 + vt(3)**2)
      if (dst .le. dismin) goto 200
c... check for tolerance around mid pt
      tec(1) = tes(1) + pih*vt(1)
      tec(2) = tes(2) + pih*vt(2)
      tec(3) = tes(3) + pih*vt(3)
      call tlaxsf (tec, tac, tpc, snc, ier)
      if (ier .ne. 0) goto 999
      v(1) = tp(1)-tps(1)
      v(2) = tp(2)-tps(2)
      v(3) = tp(3)-tps(3)
      dsn = ta(1)*v(1) + ta(2)*v(2) + ta(3)*v(3)
      v(1) = tpc(1)-tps(1)
      v(2) = tpc(2)-tps(2)
      v(3) = tpc(3)-tps(3)
      dn = ta(1)*v(1) + ta(2)*v(2) + ta(3)*v(3)
      if (dabs(dn - dsn*pih) .gt. tol) then
c... store mid in pt and redo
        do 110 i=1,15
110     b(i) = bc(i)
        go to 100
      endif
c... check for end
200   v(1) = tee(1)-te(1)
      v(2) = tee(2)-te(2)
      v(3) = tee(3)-te(3)
      dt = ta(1)*v(1) + ta(2)*v(2) + ta(3)*v(3)
      vt(1) = v(1) - dt*ta(1)
      vt(2) = v(2) - dt*ta(2)
      vt(3) = v(3) - dt*ta(3)
      dt = dsqrt (vt(1)**2 + vt(2)**2 + vt(3)**2)
      if (dt .lt. dismin) goto 900
c... output pt
      call scrst0 (idf1, b, dwa1, dwc1, dws1, ier)
      if (ier .ne. 0) goto 999
      dwa1 = dwa1 + dwc1
      if (dwa1 .gt. msz1) msz1 = dwa1-1
c... store pt in start
      do 220 i=1,15
220   bs(i) = b(i)
c... set up next pt
      dst = 1.25*dst
      if (dst .gt. dt) dst = dt
      dt = dst/dt
      te(1) = tes(1) + dt*vt(1)
      te(2) = tes(2) + dt*vt(2)
      te(3) = tes(3) + dt*vt(3)
      call tlaxsf (te, ta, tp, sn, ier)
      if (ier .ne. 0) goto 999
      goto 100
c... output end pt
900   continue
      call scrst0 (idf1, be, dwa1, dwc1, dws1, ier)
      if (ier .ne. 0) goto 999
      dwa1 = dwa1 + dwc1
      if (dwa1 .gt. msz1) msz1 = dwa1-1
c... store end pt in start pt
      do 910 i=1,15
910   bs(i) = be(i)
920   if (ind .gt. 0) then
        lnd = ind
      else
c... store output pt
        if (trans) then
c... back to tool axis referance
          do 930 i=1,3
          buf(i)   = bs(4)*mx(i) + bs(5)*mx(i+4) + bs(6)*mx(i+8)
930       buf(i+3) = bs(7)*mx(i) + bs(8)*mx(i+4) + bs(9)*mx(i+8)
        else
          do 940 i=1,6
940       buf(i) = bs(i+3)
        endif
      endif 
999   return
      end

c **********************************************************************
c **********************************************************************
c **  16. subroutine name:  tlaxsf
c **                                                                  
c **  purpose of subroutine: to define the tool end point, TE, the 
c **                         tool tangent point, TP, and the surface
c **                         normal, SN of the tool when its projected
c **                         along its tool axis to the surface.
c **  parameters
c **     input  :  The initial TE and TA are used as start values of
c **               TP and SN. Surface parmeters are already initialized.
c **
c **     output :  TE, TP and SN are defined to offset the tool on the
c **               surface by its thick when IER is zero.
c **                  ier = 4 - max iteration exceeded 1/TOL+10
c **        
c **********************************************************************
c **********************************************************************

      subroutine tlaxsf (te, ta, tp, sn, ier)

      include 'com8a.com'
      include 'rrdm.com'
      include 'pokcom.com'

      common/rrsf/ asf, usf, vsf, sfdr, sftk, sftb(7)

      real*8 asf, te(3), ta(3), tp(3), sn(3), vn(3)
      real*8 v(3), cs, d, ds(2)
      real*4 usf, vsf, sfdr, sftk, sftb
      integer*4 ier
      integer*2 nlp, ilp, ig
      parameter (stol = .000001)

      ier = 0
      if (sc(169).gt.8.229999d0) then
        call tltosf (te, ta, tp, sn,sc(11))
        if (ifl(2).gt.0) ier = 4
        goto 999
      endif

c... set up loop count and parmeters
      ig = ifl(6)
      nlp = 1.0/tol + 10
      ilp = 0
      sn(1) = ta(1)
      sn(2) = ta(2)
      sn(3) = ta(3)
100   ilp = ilp + 1
c... allign tp on tool and get sf pt and normal
      call tlpnts (2, te, ta, tp, sn, vn)
      call sfpt (asf, tp, 3, sfdr, usf, vsf, sftb)
c... normalize surface normal
      v(1) = sftb(1)
      v(2) = sftb(2)
      v(3) = sftb(3)
      d = dsqrt(v(1)**2 + v(2)**2 + v(3)**2)
      sn(1) = v(1)/d
      sn(2) = v(2)/d
      sn(3) = v(3)/d
c... add normal distance (thick) to sf pt
      v(1) = sftb(5) + sftk*sn(1)
      v(2) = sftb(6) + sftk*sn(2)
      v(3) = sftb(7) + sftk*sn(3)
c... check if we are there
      v(1) = tp(1) - v(1)
      v(2) = tp(2) - v(2)
      v(3) = tp(3) - v(3)
      d = ta(1)*v(1) + ta(2)*v(2) + ta(3)*v(3)
      if(dabs(d) .le. tol .and. ilp .ne. 1) go to 900
c... check for looping
      if (ilp .gt. nlp) then
        ier =  4
        go to 999
      endif
c... get a new tool end point
      cs = (ta(1)*sn(1) + ta(2)*sn(2) + ta(3)*sn(3))**2
      if (cs .le. stol) go to 900
      ds(1) = ds(2)
      ds(2) = d/cs
      if (ilp .ge. 2 .and. 
     x    ds(2)*ds(1).lt.0.0 .and. dabs(ds(2)).ge.dabs(ds(1))) then
c... divergency
        if (ier .ge. 3) then
          if (cs .ge. .5) then
c... gouge check test
            te(1) = te(1) - dabs(ds(1))*ta(1)
            te(2) = te(2) - dabs(ds(1))*ta(2)
            te(3) = te(3) - dabs(ds(1))*ta(3)
          endif
          go to 900
        endif
c... half interval for 3 tries
        ier = ier + 1
        ds(2) = -.5*ds(1)
      endif
      te(1) = te(1) - ds(2)*ta(1)
      te(2) = te(2) - ds(2)*ta(2)
      te(3) = te(3) - ds(2)*ta(3)
      go to 100
900   if (ier .gt. 0) ier = ier - 1
      if (ier .ne. 0 .or.
     x    (sn(1)*vn(1) + sn(2)*vn(2) + sn(3)*vn(3)) .lt. 0.0) then
c... check for gouge on halfing or sn on wrong side of normal
c... 0. could be set by user input (function of tool dia and curvature)
        ig = 3
        ier = 0
      endif
      if (ig .ne. 0) call tlgoug (te, ta, tp, sn, ig)
      if (ig .lt. 0) ier = ig

999   return
      end

c **********************************************************************
c **********************************************************************
c **  16. subroutine name:  scrst0
c **                                                                  
c **  purpose of subroutine: to define the tool end point, TE, the 
c **                         tool tangent point, TP, and the surface
c **                         normal, SN of the tool when its projected
c **                         along its tool axis to the surface.
c **  parameters
c **     input  :  The initial TE and TA are used as start values of
c **               TP and SN. Surface parmeters are already initialized.
c **
c **     output :  TE, TP and SN are defined to offset the tool on the
c **               surface by its thick when IER is zero.
c **                  ier = 4 - max iteration exceeded 1/TOL+10
c **        
c **********************************************************************
c **********************************************************************

      subroutine scrst0 (kfilp,b,kst,kcnt,kbyt,kerr)

      include 'com8a.com'
      include 'rrdm.com'
      include 'mocom.com'

      common/secpsf/ secdir,usec,vsec
      real*4 secdir,usec,vsec

      common/tavekt/ tavk,tainit
      integer*2 tainit
      real*8 tavk(3)

      integer*4 kfilp,kst,kcnt,kbyt,kerr,khow
      real*8 b(15),bta(3)
 
      real*8 te(3), ta(3), tp(3), sn(3), bb(15)
      integer*2 i,isf,ierr

      real*4 srf(10,4)
      equivalence (srf,s)

        if (tainit .eq. 1) then
          do i = 1,3
            bta(i) = tavk(i)
          enddo
        else
          do i = 1,3
            bta(i) = b(6+i)
          enddo
        endif

        if (tamode .ge. 1) then
          if (tamode .eq. 2) then
            isf = 4
            do i = 1,3
              te(i) = b(3+i)
              ta(i) = bta(i)
            enddo
            call sfptvc(sc(19),te,ta,isf,usec,vsec,ierr)
            if (ierr .gt. 0) then
              kerr = ierr
              return
            endif
          endif

          do i = 1,3
            te(i) = b(3+i)
            ta(i) = bta(i)
          enddo

          khow = 2
          call tltos1 (khow, te, ta, tp, sn, sc(11))

          if (tamode.eq.2 .and. ierr.eq.0) then
            do i = 1,3
              ta(i) = secdir*srf(i,isf)
            enddo
          else
            do i = 1,3
              ta(i) = sn(i)
            enddo
          endif

          khow = 1
          call tltos1 (khow, te, ta, tp, sn, sc(11))

          do i = 1,3
            bb(i) = b(i)
            bb(3+i) = te(i)
            bb(6+i) = ta(i)
            bb(9+i) = tp(i)
            bb(12+i) = sn(i)
          enddo

          tainit = 1
          do i = 1,3
            tavk(i) = ta(i)
          enddo

          call scrsto (kfilp,bb,kst,kcnt,kbyt,kerr)
        else
          call scrsto (kfilp,b,kst,kcnt,kbyt,kerr)
        endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  16. subroutine name:  tltos0
c **                                                                  
c **  purpose of subroutine: to define the tool end point, TE, the 
c **                         tool tangent point, TP, and the surface
c **                         normal, SN of the tool when its projected
c **                         along its tool axis to the surface.
c **  parameters
c **     input  :  The initial TE and TA are used as start values of
c **               TP and SN. Surface parmeters are already initialized.
c **
c **     output :  TE, TP and SN are defined to offset the tool on the
c **               surface by its thick when IER is zero.
c **                  ier = 4 - max iteration exceeded 1/TOL+10
c **        
c **********************************************************************
c **********************************************************************

      subroutine tltos0 (te, ta, tp, sn, kerr)

      include 'com8a.com'
      include 'rrdm.com'
      include 'mocom.com'

      common/secpsf/ secdir,usec,vsec
      real*4 secdir,usec,vsec

      real*8 te(3), ta(3), tp(3), sn(3)
      integer*4 kerr

      real*4 srf(10,4)
      equivalence (srf,s)
 
      integer*2 i,isf,ierr
      integer*4 khow

          if (tamode .eq. 2) then
            isf = 4
            call sfptvc(sc(19),te,ta,isf,usec,vsec,ierr)
            if (ierr .gt. 0) then
              kerr = ierr
              return
            endif
          endif

          khow = 2
          call tltos1 (khow, te, ta, tp, sn, sc(11))
          if (tamode.eq.2 .and. ierr.eq.0) then
            do i = 1,3
              ta(i) = secdir*srf(i,isf)
            enddo
          else
            do i = 1,3
              ta(i) = sn(i)
            enddo
          endif

          khow = 1
          call tltos1 (khow, te, ta, tp, sn, sc(11))

      return
      end

c **********************************************************************
c **********************************************************************
c **  17. subroutine name:  tlpnts
c **                                                                  
c **  purpose of subroutine: to define the tool end point, TE, or the 
c **                         tool tangent point, TP, from the tool axis,
c **                         TA, the surface normal, SN, the other tool
c **                         point and the tool profile segment array.
c **  parameters
c **     input  :  if IPT is 1, TE is calculated from TP, TA, and SN 
c **               else TP is calculated from TE, TA, and SN.
c **
c **     output :  if the profile segment arry is not defined, the tool 
c **               end, TE, and tool tangent point, TP, are set equal.
c **               VN, vector normal to TA, is not zero if SNXTA is not
c **               zero and the tool has a flat bottom.
c **********************************************************************
c **********************************************************************

      subroutine tlpnts (ipt, te, ta, tp, sn, vn)

      include 'com8a.com'

      common/rrpf/tlsegs(50), ntlsgs

      real*8 tlsegs, te(3), ta(3), tp(3), sn(3), vn(3)
      real*8 d, cs, sr, sx, sz
      integer*2 ntlsgs, ipt, isg, i
      parameter (stol = .000001)

c... calculate the ta.sn cosine valve and zero scalar values
      sr = 0.0
      sx = 0.0
      sz = 0.0
      cs = ta(1)*sn(1) + ta(2)*sn(2) + ta(3)*sn(3)
      if (ntlsgs .le. 0) go to 400
c... develop the normal vector as  sn - (ta.sn)ta
      vn(1) = sn(1) - cs*ta(1)
      vn(2) = sn(2) - cs*ta(2)
      vn(3) = sn(3) - cs*ta(3)
      d = dsqrt (vn(1)**2 + vn(2)**2 + vn(3)**2)
      if (d .gt. stol) then
        vn(1) = vn(1)/d
        vn(2) = vn(2)/d
        vn(3) = vn(3)/d
      else
        vn(1) = 0.0
        vn(2) = 0.0
        vn(3) = 0.0
      endif
c... find scalars from cosine and profile array
      do 100 isg=1,ntlsgs
      i = isg*7 -7 
      if (cs .ge. tlsegs(i+3)) go to 200
      if (cs .ge. tlsegs(i+7)) go to 300
100   continue
c... end of segments: see what end within last segment to offset
      if (tlsegs(i+3) .eq. tlsegs(i+7) .and.
     x    dabs(tlsegs(i+7)-cs) .lt. stol) then
        sx = tlsegs(i+2)
        sz = tlsegs(i+1)
      else
        sx = tlsegs(i+9)
        sz = tlsegs(i+8)
      endif
      go to 400
200   sx = tlsegs(i+2)
      sz = tlsegs(i+1)
      go to 400
300   sr = tlsegs(i+4)
      sx = tlsegs(i+6)
      sz = tlsegs(i+5)
c... calculate output point fron offsets
400   if(ipt .eq. 1) then
        te(1) = tp(1) + sr*sn(1) + sx*vn(1) - sz*ta(1)
        te(2) = tp(2) + sr*sn(2) + sx*vn(2) - sz*ta(2)
        te(3) = tp(3) + sr*sn(3) + sx*vn(3) - sz*ta(3)
      else
        tp(1) = te(1) + sz*ta(1) - sx*vn(1) - sr*sn(1)
        tp(2) = te(2) + sz*ta(2) - sx*vn(2) - sr*sn(2)
        tp(3) = te(3) + sz*ta(3) - sx*vn(3) - sr*sn(3)
      endif
      if (tlsegs(7) .ne. 1.0) then
c... not a flat tool null vn
        vn(1) = 0.0
        vn(2) = 0.0
        vn(3) = 0.0
      endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  18. subroutine name:  tlgoug
c **                                                                  
c **  purpose of subroutine: to determine whether the tool gouges the
c **                         surface and can move the up the tool axis
c **                         to be tangent again.  Tool slice planes are
c **                         defined by the normal direction, to its 
c **                         axis, of surface normals from the tool or
c **                         the tool direction.
c **  parameters
c **     input  :  The tool end, TE, tool axis, TA, and tangent point,
c **               TP, are used with sf and pt common values to define
c **               gouging.  The TE and TP define the sampling width to
c **               the surface.  If TP equals TE, the tool direction is
c **               used to define the effective tool radius for width.
c **               IG is number of tool slices for gouge testing 1 or 2.
c **
c **     output :  TE, TP and surface normal,SN, are redefined up the
c **               tool axis, TA, if IG is not zero.
c **
c **   warnings :  The routine assumes only one surface inflection under
c **               the width of the tool.. ie - concave or convex.
c **
c **********************************************************************
c **********************************************************************

      subroutine tlgoug (te, ta, tp, sn, ig)

      include 'com8a.com'
      include 'const.com'
      include 'pokcom.com'

      common/rrsf/ asf, usf, vsf, sfdr, sftk, sftb(7)
      common/rrpt/ tldr(3), bs(15), lnd

      real*8 tldr, bs, asf, te(3), ta(3), tp(3), sn(3)
      real*8 tn(3), v(3), d, vn(3), dsn, t(3), p(3), r
      real*8 st(3), vt(3), sp(3), vp(3), c(3), cs, si, cv
      real*4 usf, vsf, sfdr, sftk, sftb, sft(7), sfp(7)
      integer*4 lnd
      integer*2 ig, ngp, ilp, i
      equivalence (sft(1),sftb(1))
      parameter (stol = .000001)

      ngp = 2*((ig+1)/2)
      ig = 0
      ilp = 0
      do 10 i=1,3
      t(i) = te(i)
10    p(i) = tp(i)
c... set up tool normal distance and direction
      v(1) = p(1) - t(1)
      v(2) = p(2) - t(2)
      v(3) = p(3) - t(3)
      d = v(1)*ta(1) + v(2)*ta(2) + v(3)*ta(3)
      vn(1) = v(1) - d*ta(1)
      vn(2) = v(2) - d*ta(2)
      vn(3) = v(3) - d*ta(3)
      dsn = dsqrt (vn(1)**2 + vn(2)**2 + vn(3)**2)
      if (dsn .eq. 0.0) then
c... get a tool normal distance via tool direction
        v(1) = tldr(1)
        v(2) = tldr(2)
        v(3) = tldr(3)
        call tlpnts (2, t, ta, p, v, vn)
        v(1) = p(1) - t(1)
        v(2) = p(2) - t(2)
        v(3) = p(3) - t(3)
        d = v(1)*ta(1) + v(2)*ta(2) + v(3)*ta(3)
        vn(1) = v(1) - d*ta(1)
        vn(2) = v(2) - d*ta(2)
        vn(3) = v(3) - d*ta(3)
        dsn = dsqrt (vn(1)**2 + vn(2)**2 + vn(3)**2)
c... test for zero tool
        if (dsn .eq. 0.0) go to 999
      endif
      vn(1) = vn(1)/dsn
      vn(2) = vn(2)/dsn
      vn(3) = vn(3)/dsn
c... set pierce pt even with tip
100   p(1) = t(1) + dsn*vn(1)
      p(2) = t(2) + dsn*vn(2)
      p(3) = t(3) + dsn*vn(3)
c... surface curvature radius between te & tp
      call sfpt (asf, p, 3, sfdr, usf, vsf, sfp)
      v(1) = sfp(1)
      v(2) = sfp(2)
      v(3) = sfp(3)
      d = dsqrt (v(1)**2 + v(2)**2 + v(3)**2)
      vp(1) = v(1)/d
      vp(2) = v(2)/d
      vp(3) = v(3)/d
      sp(1) = sfp(5) + sftk*vp(1) - t(1)
      sp(2) = sfp(6) + sftk*vp(2) - t(2)
      sp(3) = sfp(7) + sftk*vp(3) - t(3)
      call sfpt (asf, t, 3, sfdr, usf, vsf, sft)
      v(1) = sft(1)
      v(2) = sft(2)
      v(3) = sft(3)
      d = dsqrt (v(1)**2 + v(2)**2 + v(3)**2)
      vt(1) = v(1)/d
      vt(2) = v(2)/d
      vt(3) = v(3)/d
      st(1) = sft(5) + sftk*vt(1) - t(1)
      st(2) = sft(6) + sftk*vt(2) - t(2)
      st(3) = sft(7) + sftk*vt(3) - t(3)
c... convex/concave indecator
      cv = 1.0
      if ((vp(1)-vt(1))*(sp(1)-st(1)) +
     x    (vp(2)-vt(2))*(sp(2)-st(2)) +
     x    (vp(3)-vt(3))*(sp(3)-st(3)) .lt. 0.0) cv = -cv
c... get the plane of the tool
      v(1) = vp(2)*vt(3) - vp(3)*vt(2)
      v(2) = vp(3)*vt(1) - vp(1)*vt(3)
      v(3) = vp(1)*vt(2) - vp(2)*vt(1)
      tn(1) = ta(2)*v(3) - ta(3)*v(2)
      tn(2) = ta(3)*v(1) - ta(1)*v(3)
      tn(3) = ta(1)*v(2) - ta(2)*v(1)
      d = dsqrt (tn(1)**2 + tn(2)**2 + tn(3)**2)
      if (d .gt. stol) then
        d = cv*d
        tn(1) = tn(1)/d
        tn(2) = tn(2)/d
        tn(3) = tn(3)/d
c... project surface data into tool plane
        v(1) = tn(2)*ta(3) - tn(3)*ta(2)
        v(2) = tn(3)*ta(1) - tn(1)*ta(3)
        v(3) = tn(1)*ta(2) - tn(2)*ta(1)
        d = vp(1)*v(1) + vp(2)*v(2) + vp(3)*v(3)
        vp(1) = vp(1) - d*v(1)
        vp(2) = vp(2) - d*v(2)
        vp(3) = vp(3) - d*v(3)
        d = dsqrt (vp(1)**2 + vp(2)**2 + vp(3)**2)
        if (d .lt. stol) go to 300
        vp(1) = vp(1)/d
        vp(2) = vp(2)/d
        vp(3) = vp(3)/d
        d = sp(1)*v(1) + sp(2)*v(2) + sp(3)*v(3)
        sp(1) = sp(1) - d*v(1)
        sp(2) = sp(2) - d*v(2)
        sp(3) = sp(3) - d*v(3)
        d = vt(1)*v(1) + vt(2)*v(2) + vt(3)*v(3)
        vt(1) = vt(1) - d*v(1)
        vt(2) = vt(2) - d*v(2)
        vt(3) = vt(3) - d*v(3)
        d = dsqrt (vt(1)**2 + vt(2)**2 + vt(3)**2)
        if (d .lt. stol)go to 300 
        vt(1) = vt(1)/d
        vt(2) = vt(2)/d
        vt(3) = vt(3)/d
        d = st(1)*v(1) + st(2)*v(2) + st(3)*v(3)
        st(1) = st(1) - d*v(1)
        st(2) = st(2) - d*v(2)
        st(3) = st(3) - d*v(3)
c... surface curvature in tool plane
        v(1) = sp(1) - st(1)
        v(2) = sp(2) - st(2)
        v(3) = sp(3) - st(3)
        d = dsqrt (v(1)**2 + v(2)**2 + v(3)**2)
        cs = vt(1)*vp(1) + vt(2)*vp(2) + vt(3)*vp(3)
        r = dsqrt (dabs(1.0 - cs)*.5)
        if (r .gt. stol) then
          r = cv*d/r*.5
c... center pt of curvature
          c(1) = t(1) + st(1) - r*vt(1)
          c(2) = t(2) + st(2) - r*vt(2)
          c(3) = t(3) + st(3) - r*vt(3)
c... tool up the tool axis from surface if needed
          call tlupta (r, c, tn, t, ta, sp, vp, i)
          if (i .ne. 0) then
c... replace tool pts and vt
            ig = ig + i
            do 200 i=1,3
            te(i) = t(i)
            tp(i) = sp(i)
200         sn(i) = vp(i)
          endif
        endif
      endif
300   ilp = ilp + 1
      if (ilp .lt. ngp) then
        if (ilp-ilp/2*2 .ne. 0) then
c... do back side 
          vn(1) = -vn(1)
          vn(2) = -vn(2)
          vn(3) = -vn(3)
          go to 100
        else
c... do rigth side of -vn
          v(1) = vn(2)*ta(3) - vn(3)*ta(2)
          v(2) = vn(3)*ta(1) - vn(1)*ta(3)
          v(3) = vn(1)*ta(2) - vn(2)*ta(1)
          r = TWO_PI/ngp
          cs = dcos (r)
          si = dsin (r)
          vn(1) = cs*vn(1) + si*v(1)
          vn(2) = cs*vn(2) + si*v(2)
          vn(3) = cs*vn(3) + si*v(3)
          go to 100
        endif
      endif
       
999   return
      end

c **********************************************************************
c **********************************************************************
c **  19. subroutine name:  tlupta
c **                                                                  
c **  purpose of subroutine: to move the tool end point, TE, up the tool
c **                         axis, TA, given a surface point normal
c **                         direction, TN, the surface centroid, CN,
c **                         its radius of curvature, RD, (plus - convex
c **                         minus - concave) and the tool profile 
c **                         segment array in common.
c **  parameters
c **     input  :  RD, CN, TN, TE, and TA are given.
c **
c **     output :  TE, TP, and SN will be changed up the tool if the
c **               surface gouge flag, IG, is not zero.
c **        
c **********************************************************************
c **********************************************************************

      subroutine tlupta (rd, cn, tn, te, ta, tp, sn, ig)

      include 'com8a.com'

      common/rrpf/tlsegs(50), ntlsgs

      real*8 tlsegs, rd, cn(3), tn(3), te(3), ta(3), tp(3), sn(3)
      real*8 sid, r, v(3), scz, scx
      real*8 dz1, dx1, ds1, cs1, si1, rt, dz2, dx2, ds2, cs2, si2
      real*8 dzc, dxc, s, z, x, d, cs, si
      integer*2 ntlsgs, ig, n, i
      parameter (stol = .000001)

      ig = 0
c... tool side of surface curvature
      sid = 1.0
c... or inside (concave side) of surface
      if (rd .lt. 0.0) sid = -1.0
      r = dabs (rd)
c... set up a 2d profile
      v(1) = cn(1) - te(1)
      v(2) = cn(2) - te(2)
      v(3) = cn(3) - te(3)
      scz = v(1)*ta(1) + v(2)*ta(2) + v(3)*ta(3)
      scx = v(1)*tn(1) + v(2)*tn(2) + v(3)*tn(3)
c... loop through tool segments
      do 100 n=1,ntlsgs
      i = 7*n - 7
c... surface center to segment end pts
      dz1 = tlsegs(i+1)-scz
      dx1 = tlsegs(i+2)-scx
      cs1 = tlsegs(i+3)
      ds1 = 0.0
      if (cs1 .gt. stol) then
c... botton tool segment
        si1 = dsqrt (1.0 - cs1**2)
        ds1 = sid*(dx1 + dz1*si1/cs1)
      endif
      if (ds1 .ge. 0.0) then
c... first segment pt - convex or concave
        x = dx1
        z = sid*dsqrt (r**2 - x**2)
        d = z - dz1
        go to 200
      endif
      rt = sid*tlsegs(i+4)
      dz2 = tlsegs(i+8)-scz
      dx2 = tlsegs(i+9)-scx
      cs2 = tlsegs(i+7)
      ds2 = 0.0
      if (cs2 .gt. stol) then
c... bottom tool segmment
        si2 = dsqrt (1.0 - cs2**2)
        ds2 = sid*(dx2 + dz2*si2/cs2)
      endif
      if (ds2 .ge. 0.0) then
c... between first and second segment pts
        if (rt .ne. 0.0) then
c... arc segment - convex or concave
          dzc = tlsegs(i+5) - scz
          dxc = tlsegs(i+6) - scx
          x = dxc
c
c... UNTESTED CHANGE TO FIX DOMAIN ERROR FSR 51610 with GOUGCK/ON,3
c... This code will only be executed if version is set below 8.21
c
          z = 0.0
          d = (r + rt)**2 - dxc**2
          if (d.gt.0.0) z = sid*dsqrt (d)
          d = z - dzc
          go to 200
        elseif (sid .gt. 0.0) then
c... line segment - convex
          if (ds2 .eq. 0.0) then
            cs2 = 0.0
            si2 = 1.0
          endif
          x = -r*si2
          z = r*cs2
          d = z - dz2
          go to 200
        else
c... line segment - concave
          x = dx2
          z = sid*dsqrt (r**2 - x**2)
          d = z - dz2
          go to 200
        endif
      endif
100   continue
      x = dx2
      z = sid*dsqrt (r**2 - x**2)
      d = z - dz2
200   if (d .gt. stol) then
c... move the tool up the axis
        ig = ig + 1
        s = dsqrt (z**2 + x**2)
        cs = z/s
        si = x/s
        sn(1) = cs*ta(1) + si*tn(1)
        sn(2) = cs*ta(2) + si*tn(2)
        sn(3) = cs*ta(3) + si*tn(3)
        x = scx + x
        z = scz + z
        tp(1) = te(1) + z*ta(1) + x*tn(1)
        tp(2) = te(2) + z*ta(2) + x*tn(2)
        tp(3) = te(3) + z*ta(3) + x*tn(3)
        te(1) = te(1) + d*ta(1)
        te(2) = te(2) + d*ta(2)
        te(3) = te(3) + d*ta(3)
      endif

999   return
      end

c **********************************************************************
c **********************************************************************
c **  20. subroutine name:  srfhgt
c **                                                                  
c **  purpose of subroutine: determines the high and low distance along
c **                         a given tool axis, TA, within the surface
c **                         tool point in scratch area one.
c **  parameters
c **     input  :  15 element surface tool pts in scratch area one and
c **               TA - tool axis vector
c **
c **     output :  DHI and DLO - distance high and low of tool end pts
c **               from the surface tool point if ier .ne. 0.
c **                 ier = 1 - scratch area not allocated from scrlod
c **                 ier = 5 - find tool surface data in scratch
c **
c **     format :  a tool surface pt contains the following real*8 data
c **                 idp - tool pt id - two integer*4 values of:
c **                       point type and index ids
c **                 xpl - x xyplaner value, not translated
c **                 ypl - y xyplaner value, not translated
c **                 te  - tool end, xyz, offset to surface
c **                 ta  - tool axis, ijk
c **                 tp  - tangent point, xyz, on surface
c **                 sn  - surface normal, ijk, at tp.
c **
c **********************************************************************
c **********************************************************************

      subroutine srfhgt (ta, dhi, dlo, trans, mx, ier)

      include 'com.com'

      real*8 ta(3), dhi, dlo, mx(12)
      integer*4 ier
      logical trans

      common/rrsf/ asf, usf, vsf, sfdr, sftk, sftb(7)
      common/rrs1/ idf1, idr1, msz1, dwa1, dwc1, dws1

      real*8 asf
      real*4 usf, vsf, sfdr, sftk, sftb
      integer*4 idf1, idr1, msz1, dwa1, dwc1, dws1

      real*8 vt(3), buf(15), d
      integer*2 i

c... set up tool vector and scratch parms
      do 10 i=1,3
10    vt(i) = ta(i)
      if (trans) call conent (vt, mx, 4)
      idr1 = 1
      dwa1 = 1
      dwc1 = 15
      dws1 = 8
      if (msz1 .lt. dwa1) then
        ier = 5
        go to 999
      endif
      ier = 0
c... get first pt
      call scrlod (idf1, buf, dwa1, dwc1, dws1, ier)
      if (ier .ne. 0) go to 999
      dwa1 = dwa1 + dwc1
      dhi = buf(4)*vt(1) + buf(5)*vt(2) + buf(6)*vt(3)
      dlo = dhi
100   if(msz1 .lt. dwa1) go to 900
c... do next pt
      call scrlod (idf1, buf, dwa1, dwc1, dws1, ier)
      if (ier .ne. 0) go to 999
      dwa1 = dwa1 + dwc1
      d = buf(4)*vt(1) + buf(5)*vt(2) + buf(6)*vt(3)
      if (d .gt. dhi) dhi = d
      if (d .lt. dlo) dlo = d
      go to 100

900   dlo = dlo-sftk

999   return
      end

c **********************************************************************
c **********************************************************************
c **  21. subroutine name:  srfout
c **                                                                  
c **  purpose of subroutine: outputs cutter points as a xyplane level
c **                         before the surface or tool surface points
c **                         from scatch area one.
c **  parameters
c **     input  :  15 element surface tool pts in scratch area one and
c **                 ixxy  - point index id and scratch pt read
c **                         direction if<0 read backwards
c **                 istep - output count value
c **                         if>0 output motion to i/o
c **                         if<0 output xyplane pt to buf
c **                 buf   - xyplane cut xyz values
c **
c **     output :  rrs1 common initialize if idr1=0 and output if ier=0
c **                 buf    - contains last point output.
c **                 ier = 1 - scratch area not allocated from scrlod
c **                 ier = 5 - cannot find record in scratch
c **
c **     format :  a tool surface pt contains the following real*8 data
c **                 idp - tool pt id - two integer*4 values of:
c **                       point type and index ids
c **                 xpl - x xyplaner value, not translated
c **                 ypl - y xyplaner value, not translated
c **                 te  - tool end, xyz, offset to surface
c **                 ta  - tool axis, ijk
c **                 tp  - tangent point, xyz, on surface
c **                 sn  - surface normal, ijk, at tp.
c **        
c **********************************************************************
c **********************************************************************

      subroutine srfout (kk, istep, mtype, buf, trans, mx, ier)

      include 'com8a.com'
      include 'rrdm.com'
      include 'pokpl.com'

      integer*2 istep, mtype
      real*8 buf(6), mx(12)
      logical trans
      integer*4 ier, kk, kkxy

      common/rrs1/ idf1, idr1, msz1, dwa1, dwc1, dws1
      integer*4 idf1, idr1, msz1, dwa1, dwc1, dws1

      integer*4 iocnt
      logical ifnd, lfalse
      data lfalse /.false./
      integer*4 ind, itp, ids(2)
      equivalence (itp, ids(1)), (ind, ids(2)), (ids(1), bs(1))
      real*8 bs(15)
      save bs
      data bs /15*0.0/

      ier = 0
      if (botpl(4) .gt. botpl(1)) then
c... cut level higher than surface bottom
        if (istep .ge. 0) then
          call poksto (buf, mtype, trans, mx, ier)
        else
          buf(3) = botpl(4)
        endif
        go to 999
      endif
      if (idr1 .eq. 0) then
c... set up scratch area index and count
        dwa1 = 1
        dwc1 = 15
        dws1 = 8
        ind = 0
      endif
c... set up loop parameters
      kkxy = iabs (kk)
      ifnd = .false.
      iocnt = msz1/dwc1 + 3
      idr1 = 1
      if (kk .lt. 0) idr1 = -1
100   if (kkxy .eq. ind) then
c... found tool surface pt in scratch
        ifnd = .true.
        d = bs(7)*bs(4) + bs(8)*bs(5) + bs(9)*bs(6)
        if (botpl(4) .gt. d .and. tamode.eq.0) then
c... cut level higher than surface bottom
          d = botpl(4) - d
          bs(4) = bs(4) + d*bs(7)
          bs(5) = bs(5) + d*bs(8)
          bs(6) = bs(6) + d*bs(9)
        endif
        if (istep .ge. 0) then
C   ------   FIX - Check if trans is necessary   ------
          call poksto (bs(4), mtype, lfalse, mx, ier)
          buf(1) = bs(4)
          buf(2) = bs(5)
          buf(3) = bs(6)
        else
          buf(3) = d
          go to 999
        endif
      else
        if (ifnd) goto 999
      endif
c... read next record
      call scrlod (idf1, bs, dwa1, dwc1, dws1, ier)
      if (ier .ne. 0) goto 999
      if (idr1 .lt. 0) then
c... index backwards
        dwa1 = dwa1 - dwc1
        if (dwa1 .lt. 1) dwa1 = msz1 - dwc1 + 1
      else
c... index forwards
        dwa1 = dwa1 + dwc1
        if (dwa1 .gt. msz1) dwa1 = 1
      endif
      iocnt = iocnt - 1
      if (iocnt .ge. 0) goto 100
      ier = 5 

999   return
      end

c **********************************************************************
c **********************************************************************
c **  22. subroutine name:  poksto
c **                                                                  
c **  purpose of subroutine: Store a point in the current pocket list
c **
c **********************************************************************
c **********************************************************************

      subroutine poksto (buf, mtype, trans, mx, ier)

      include 'com.com'
      include 'rrdm.com'
      include 'pokcom.com'

      real*8 buf(6), mx(12)
      integer*2 mtype
      logical trans
      integer*4 ier

      real*8 bufl(6)
      integer*2 i
      integer*2 i2v3 /3/, i2v4 /4/

      do 20 i = 1,6
20    bufl(i) = buf(i)

      if (tamode.ge.1 .and. .not.trans .and. bthk.gt.0) then
        bufl(1) = buf(1) + bthk*buf(4)
        bufl(2) = buf(2) + bthk*buf(5)
        bufl(3) = buf(3) + bthk*buf(6)
      else
        bufl(3) = buf(3) + bthk
      endif

      if (trans) then
        call conent (bufl(1), mx, i2v3)
        call conent (bufl(4), mx, i2v4)
      endif

      call pklsto (ifl(82), mtype, bufl, ier)

999   return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  pokstn
c **                                                                  
c **  purpose of subroutine: Store a point in the current pocket list
c **
c **********************************************************************
c **********************************************************************

      subroutine pokstn (buf, tainv, mtype, trans, mx, ier)

      include 'com.com'
      include 'pokcom.com'

      real*8 buf(6), mx(12), tainv(12)
      integer*2 mtype
      logical trans
      integer*4 ier

      real*8 bufl(6)
      integer*2 i
      integer*2 i2v3 /3/, i2v4 /4/

      do 20 i = 1,6
20    bufl(i) = buf(i)

c      bufl(3) = buf(3) + bthk

      call conent(bufl, tainv, i2v3)
      call conent(bufl(4), tainv, i2v4)

      if (trans) then
        call conent (bufl(1), mx, i2v3)
        call conent (bufl(4), mx, i2v4)
      endif

      call pklsto (ifl(82), mtype, bufl, ier)

999   return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  pokst0
c **                                                                  
c **  purpose of subroutine: Store a point in the current pocket list
c **
c **********************************************************************
c **********************************************************************

      subroutine pokst0 (buf, mtype, trans, mx, ier)

      include 'com.com'
      include 'pokcom.com'

      real*8 buf(6), mx(12)
      integer*2 mtype
      logical trans
      integer*4 ier

      real*8 bufl(6)
      integer*2 i
      integer*2 i2v3 /3/, i2v4 /4/

      do 20 i = 1,6
20    bufl(i) = buf(i)

      if (trans) then
        call conent (bufl(1), mx, i2v3)
        call conent (bufl(4), mx, i2v4)
      endif

      call pklsto (ifl(82), mtype, bufl, ier)

999   return
      end
