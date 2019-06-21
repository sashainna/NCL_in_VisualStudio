C***********************************************************************
C*    NAME         :  uvmove.f
C*       CONTAINS:
c*
C*                uvmove
C*                goto_edge
c*
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       uvmove.f , 25.1
C*     DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:52
C***********************************************************************
C
c **********************************************************************
c **********************************************************************
c **
c **  subroutine name: uvmove
c **                   this is a modified version of the mover routine
c **
c **  last revision:
c **  purpose of subroutine: generates points along intersection of a 
c **                         surf and a surf or plane.
c **  input:
c **     itsk = type of call being made
c **            1 = move to a point on the two surfaces near the point
c **                specified in p(1-3)
c **            2 = generate points along the intersection of the two
c **                surfaces
c **     isav = 1 - save XYZ, 2 - save UV, 0 - don't put anything into list
c **     isav = 3 - save both XYZ  and UV
c **     maxp = max size of p & uv arrays to return points back to 
c **            caller, this is not a limit of storage in the list.
c **  output:
c **     p    = array to store generated points
c **     pts  = last point stored during the single pass.
c **     vx   = 1-st and last point derivatives
c **
c **********************************************************************
c **********************************************************************

      subroutine uvmove (itsk,maxp,pts,p,vx,isav)

      include 'com4a.com'
      include 'mocom.com'
 
      integer*2 itsk,maxp,isav
      real*8 p(3000),vx(6),pts(3),uv(3)

      integer*2 jd(600)
      real*4 ad(300)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
c
c... local variables
c
      real*8 hh(10)
      real*8 uvc(2),uvcs(2),uvps1(4),uvps2(4),uvds1(4),uvds2(4),
     *       uvtmp(2),ptx(3),plast(3),
     *       dl,sec,si,pp,q,fx,fy,fz,co,tol,told,dpend,uu,vv,
     *       dp,dpmax,dpmin,hdp,hot,dp1,dp2,
     *       half/0.5d0/,zero/0.d0/,one/1.d0/,separ/1.d-6/,
     *       cot, min2_r8,max2_r8,um_dist_2d
      real*4 v(3),a,b,dis,ro, dp4,min2_r4, f_dot4,f_mag4,f_dist4
      real*4 a1,b1
      integer*4 nclkey
      integer*2 kfln,icrwnt,nwds,ietype,ierr,iwch,ifl331,ifl332
      logical psdone,dsdone,lv91,lv92,lv925,noedge,lcvsf
      save noedge
      real*4 pt1(3),uv1(2),vc1(3),vc(3),c
      integer*2 lll
      real*8 sv(9),xn(3),bestn1(3),bestn2(3),bestc1,bestc2

      
      lv91 = sc(169).lt.9.149d0
      lv92 = sc(169).lt.9.2d0
      lv925 = sc(169).lt.9.25d0

      if (itsk.eq.1) then
        if (isav .eq. 0) lv925 = .true.
        if (isav .eq. 1) isav = 0
      endif
           
      noedge = .false.
      lcvsf = .false.

      if (sc(169).lt.8.4999) then
        call uvmoveo(itsk,maxp,p,vx)
        n = ntk/3
        call ncl_free_uv
        call ncl_put_uv (p,n)
        call vctovc(p,pts)
        return
      endif

      if (sc(169) .lt. 9.249d0) then
        ifl331 = ifl(331)
        ifl332 = ifl(332)
        ifl(331) = 0
        ifl(332) = 0
      endif
      ix   = 0
      ntk  = 0
      if (lv91) call fill_array (p,3000,zero)
      call fill_array (hh,10,zero)
      sc(160) = zero
      kfln = 0
      kret = 0
      if (sc(169).lt.9.0999) then
        tol  = sc(27)
      else
        tol  = sc(27)*0.25d0
        if (tol .lt. 1.d-6) tol = 1.e-6
      endif
      told = tol
      ltsk = itsk
      if (itsk .gt. 2) ltsk = 2
      dpend = tol*10.
      dpmax = min2_r8 (tol*250.d0,sc(54))
      dpmin = tol

      if (ltsk.eq.2) then
          if (lv92) then
            told = .2*tol
          else
            tol = sc(27)*0.2d0
          endif
      endif
c
c.....unset the withold o/p and endpas re-try flags   8-3-82
c
      ifl(29)=0
      ifl(28)=0
c
c.....     min/max toler range for crwnh and jog tol
c.....         ( accept liberally at this time )
c
      atol=tol*.5
      btol=tol*1.4
c
c.....set probable iflat flg if ps,ds are in ln,pl class
c
      iflat = 0
      if(jd(1).lt.CIRCLE.and.jd(201).lt.CIRCLE) iflat=1

      if (jd(201).eq.CURVE .or.
     x    jd(201).eq.LINE .or. jd(201).eq.CIRCLE) lcvsf = .true.

      dp = 0.
      t(10,2) = 0.

      itfl = -2
      iptk = 0

      psdone = .false.
      dsdone = .false.
      call fill_array (uvps1,4,half)
      call fill_array (uvps2,4,half)
      call fill_array (uvds1,4,half)
      call fill_array (uvds2,4,half)
c
c... col headers
c
      ia = 1
      ib = 2
      ic = 3
c
c***********************************  basic step cycle startpoint
c          set extrap coefs.
c
100   idpk=0
      ireduc=0
      irejog=0
      lll = 0
      if (noedge) goto 999
110   ro=0.

      if(t(10,ib).ge.1.e-03) ro = min2_r4 (t(10,ic)/t(10,ib), 5.)

      q = ro*.5
      t(10,ia) = dp
      if(ifl(2).gt.0) goto 999
c      
c...extrap tend
c
      do 140 i=1,3
         k = i+6
         t(i,ia)=t(i,ic)+dp*(t(k,ic)+q*(t(k,ic)-t(k,ib)))
140   continue
c
c... do ps,ds srf pn's
c... ps *********
c
210   isrf=1
      call vctovc4 (t(1,ia),s(8,1))

      if(jd(1).eq.PLANE) then
        dis = f_dot4 (s(1,1), s(8,1)) - s(4,1)
        call uvcplvc4 (s(8,1),s(1,1),s(5,1), -dis)
      else
c
c...extrap u,v,hps  (unless rejog)
c
         if (irejog.eq.0) then
            uu = t(13,ic)+ro*(t(13,ic)-t(13,ib))
            vv = t(14,ic)+ro*(t(14,ic)-t(14,ib))
            t(13,ia) = min2_r8 (one, max2_r8(zero,uu))
            t(14,ia) = min2_r8 (one, max2_r8(zero,vv))
         endif
 
         call surfpn (t(13,ia),t(14,ia),1)
         if (ifl(2).gt.0) goto 999
      endif
c
c... ds *********
c
      isrf = 2
c
c...vp93.10.25 this 'if' is extracted from dsrfpn routine
c...and may be replaced in future by call dsrfpn.
c
      if (jd(201) .ne. PLANE) then
c
c...extrap u, v and h   ( unless rejog )
c
        if (ltsk.eq.1 .and. lll.gt.0) then
           t(1,ia) = s(5,1)
           t(2,ia) = s(6,1)
           t(3,ia) = s(7,1)
        endif
        if (irejog.eq.0) 
     *     call avcplbvc4 (1.+ro, t(19,ic),-ro,t(19,ib),t(19,ia))
        if (lcvsf) then
          call dsrel0
        else
          call vctovc4 (t(1,ia),s(8,2))
          call surfpn (t(19,ia),t(20,ia),1)
        endif
        if (ifl(2).gt.0) goto 999
      else
        call vctovc4 (t(1,ia),s(5,2))
      end if
c
c... fwd is crossf ps(up) and ds(rgt)
c
300   fx=s(3,2)*s(2,1)-s(2,2)*s(3,1)
      fy=s(1,2)*s(3,1)-s(3,2)*s(1,1)
      fz=s(2,2)*s(1,1)-s(1,2)*s(2,1)
      sec = dsqrt(fx**2+fy**2+fz**2)
c
c...error. fwd sense indefinite.
c
      if (sec.le.1.e-3) then
         if (ltsk.eq.2 .and. .not.lv925 .and. lll.lt.10) then
           lll = lll + 1
           t(7,ia) = t(7,ic)
           t(8,ia) = t(8,ic)
           t(9,ia) = t(9,ic)
           goto 321
         else if (ltsk.eq.1 .and. .not.lv925) then
           if (lll .eq. 0) then
             lll = 1
             t(1,ia) = s(5,2)
             t(2,ia) = s(6,2)
             t(3,ia) = s(7,2)
             goto 210
           else if (lll .eq. 1) then
             duv = 0.001
             call gtdesc (sc(35),nclkey,nwds,ietype)
             isrf = 1
             call evstup(nclkey,isrf)
             bestc1 = 1.
             do 301 i = 1,4
               if (i.eq.1) then
                 if (t(13,ia) .gt. 1.-duv) goto 301
                 uvc(1) = t(13,ia) + duv 
                 uvc(2) = t(14,ia)
               else if (i.eq.2) then
                 if (t(13,ia) .lt. duv) goto 301
                 uvc(1) = t(13,ia) - duv 
                 uvc(2) = t(14,ia)
               else if (i.eq.3) then
                 if (t(14,ia) .gt. 1.-duv) goto 301
                 uvc(1) = t(13,ia)
                 uvc(2) = t(14,ia) + duv
               else
                 if (t(14,ia) .lt. duv) goto 301
                 uvc(1) = t(13,ia)
                 uvc(2) = t(14,ia) - duv 
               endif
               call uevsft (uvc(1),uvc(2),isrf,sv,ierr)
               if (ierr .eq. 0) then
                 call f_cross(sv(4),sv(7),xn)
                 call unitvc(xn,xn)
                 co = xn(1)*s(1,1)+xn(2)*s(2,1)+xn(3)*s(3,1)
                 if (dabs(co) .lt. bestc1) then
                   if (co .lt. 0.) then
                     bestc1 = -co
                     bestn1(1) = -xn(1)
                     bestn1(2) = -xn(2)
                     bestn1(3) = -xn(3)
                   else
                     bestc1 = co
                     bestn1(1) = xn(1)
                     bestn1(2) = xn(2)
                     bestn1(3) = xn(3)
                   endif
                 endif
               endif
301          continue

             call gtdesc (sc(11),nclkey,nwds,ietype)
             isrf = 2
             call evstup(nclkey,isrf)
             bestc2 = 1.
             do 302 i = 1,4
               if (i.eq.1) then
                 if (t(19,ia) .gt. 1.-duv) goto 302
                 uvc(1) = t(19,ia) + duv 
                 uvc(2) = t(20,ia)
               else if (i.eq.2) then
                 if (t(19,ia) .lt. duv) goto 302
                 uvc(1) = t(19,ia) - duv 
                 uvc(2) = t(20,ia)
               else if (i.eq.3) then
                 if (t(20,ia) .gt. 1.-duv) goto 302
                 uvc(1) = t(19,ia)
                 uvc(2) = t(20,ia) + duv
               else
                 if (t(20,ia) .lt. duv) goto 302
                 uvc(1) = t(19,ia)
                 uvc(2) = t(20,ia) - duv 
               endif
               call uevsft (uvc(1),uvc(2),isrf,sv,ierr)
               if (ierr .eq. 0) then
                 call f_cross(sv(4),sv(7),xn)
                 call unitvc(xn,xn)
                 co = xn(1)*s(1,1)+xn(2)*s(2,1)+xn(3)*s(3,1)
                 if (dabs(co) .lt. bestc2) then
                   if (co .lt. 0.) then
                     bestc2 = -co
                     bestn2(1) = -xn(1)
                     bestn2(2) = -xn(2)
                     bestn2(3) = -xn(3)
                   else
                     bestc2 = co
                     bestn2(1) = xn(1)
                     bestn2(2) = xn(2)
                     bestn2(3) = xn(3)
                   endif
                 endif
               endif
302          continue
  
             fx=bestn2(3)*bestn1(2)-bestn2(2)*bestn1(3)
             fy=bestn2(1)*bestn1(3)-bestn2(3)*bestn1(1)
             fz=bestn2(2)*bestn1(1)-bestn2(1)*bestn1(2)
             sec = dsqrt(fx**2+fy**2+fz**2)
             if (sec.le.1.e-3) then
               ifl(2)=385
               goto 999
             endif
           endif
         else
           ifl(2)=385
           goto 999
         endif
      else
         lll = 0
      endif

      if (iptk.eq.0.and.ltsk.eq.1) goto 320
      if (fx*t(7,ic)+fy*t(8,ic)+fz*t(9,ic).lt.0.) sec=-sec
320   t(7,ia)=fx/sec
      t(8,ia)=fy/sec
      t(9,ia)=fz/sec
c
c... nest tool to ps,ds
c
321   si = f_dot4 (s(1,1), s(1,2))
      pp = f_dot4 (s(1,1), t(1,ia)) - s(4,1)
      q = f_dot4 (s(1,2), t(1,ia)) - s(4,2)
      if (lll .gt. 0 .and. dabs(si).gt.0.99999) then
        a = pp/2.
        b = q/2.
      else
        a = (pp - si*q)/(1.-si*si)
        b = q - a*si
      endif
c
c...get distance from point to i/o ps & ds
c
      call avcplbvc4 (a,s(1,1),b,s(1,2),v)
      dl = f_mag4 (v)
c
c...vp93.10.25 replaced tol check from a,b to dist of points.
c...Check both distance moved to planes (a and b) and distance between
c...old and new points (dl) - IJD 8APR99 QAR 9209
c
      if ((lv92 .and. abs(a).lt.told.and.abs(b).lt.told.and.dl.lt.told)
     *    .or. (dabs(pp).lt.tol.and.dabs(q).lt.tol.and.dl.lt.tol)) then
c
c..... to avoid stalling - replace told with tol (=0.2*sc(27)) and
c..... check pp and q - distances from current PS and DS planes
c
        call vcmnvc4 (t(1,ia),v,t(1,ia))
        goto 590
      endif

      if (irejog.gt.120) then
          ifl(2) = 163
          if (dabs(si).gt.0.99) ifl(2) = 385
          goto 999
      endif
      a1 = a
      b1 = b
      if (irejog.gt.30) then
        a1 = a/2.0
        b1 = b/2.0
        if (irejog.gt.60) then
          a1 = a/4.0
          b1 = b/4.0
        endif
      endif
      call avcplbvc4 (a1,s(1,1),b1,s(1,2),v)
      call vcmnvc4 (t(1,ia),v,t(1,ia))

      irejog = irejog+1
      goto 210

590   continue

      if (itfl.lt.0) then
         itfl = 0
         dp = min2_r8 (250.d0*tol,dpmax)
         if(iflat.eq.1 .and. ifl(23).ne.4 .and. 
     *      jd(401).ne.CIRCLE) dp = dpmax
         goto 660
      endif
c
c.....verify step tol (if in iter mode)
c
620   continue
      if (ltsk.eq.1) goto 660
      hdp = dp
      co = f_dot4 (t(7,ia),t(7,ic))
c
c.....EXTRA CRWN CHECK                23-MAY-90  IJD
c
      FX=T(1,IA)-T(1,IC)
      FY=T(2,IA)-T(2,IC)
      FZ=T(3,IA)-T(3,IC)
      SEC=dsqrt(FX**2+FY**2+FZ**2)
      IF (SEC.LT..00001) GOTO 6202
      ACO=(FX*T(7,IC)+FY*T(8,IC)+FZ*T(9,IC))/SEC
      IF (ACO.LT..1)ACO=.1
      IF (ACO.LT.CO) CO=ACO
6202  CONTINUE
c
C.....END EXTRA CRWN CHECK      
c
      if (co.gt..9999999995d0) then
        co = 1.
        si = 0.
        crwn = 0.
      else
        si = dsqrt(1.-co**2)
        crwn = dp*si/8.
      endif
c
c...extra crown check on uv curve
c
      icrwnt = 0
      if (itsk .eq. 2) then
         uvc(1) = t(13,ia) - t(13,ic)
         uvc(2) = t(14,ia) - t(14,ic)
         sec = dsqrt(uvc(1)**2 + uvc(2)**2)
         if ((lv92 .and. sec .gt. 1.d-5) .or. sec .gt. 1.d-4) then
            hot = dabs(uvc(1)*uvcs(2) - uvc(2)*uvcs(1))
c
c..... hot describes how far is current (u,v) from the line determined by the
c..... previous two (u,v)
c
            uvc(1) = uvc(1)/sec
            uvc(2) = uvc(2)/sec
            if (iptk .gt. 1 .and. kfln .eq. 1) then
               cot = uvc(1)*uvcs(1) + uvc(2)*uvcs(2)
c
c..... cot is cosine of the angle between previous and current UV vectors
c
               if (lv92) then
                 if (cot .lt. 0.999687) icrwnt = 1
                 if (cot .lt. 0.999387) icrwnt = 2
               else
                 if (cot .lt. 0.9848 .or. hot. gt. 1.d-4) icrwnt = 1
                 if (cot .lt. 0.7071 .and. hot. gt. 5.d-4) icrwnt = 2
               endif
            else
               uvcs(1) = uvc(1)
               uvcs(2) = uvc(2)
            endif
         endif
      endif
c
c.....if flat case, probable exit  (except when cs is circle)
c
621   if (crwn.gt.1.e-5 .or. icrwnt .gt. 0) goto 622

      if (iflat.eq.1.and.jd(401).ne.CIRCLE) goto 632
c
c.....this is a computed flat case. be conservative.
c.....if ps endflat contact, even more    "  "
c
      dp = hdp*1.5
      if (ifl(56).gt.1) dp = hdp*1.25
      goto 634
622   if (crwn.lt.btol .and. icrwnt .lt. 2) goto 624
c
c.....crwn too large. shorten dp ( but 1/4 hdp min )
c
      dp1 = dp
      if (crwn .ge. btol) dp1=dp*dsqrt(tol/crwn)

      if (itsk .eq. 2) then
         dp = dp1
         if (icrwnt .gt. 1) then
             dp2 = .25 * dp
             if (dp2 .lt. dp1) dp = dp2
         endif
      else
         dp = dp1
      endif

      ireduc=1
      dp = max2_r8 (dp, 0.25d0*hdp)
      if (dp.lt.dpmin) dp = dpmin
      goto 626
624   if (crwn.gt.atol.or.ireduc.gt.0.or.itfl.gt.0) goto 630
c
c.....crwn too small. increase dp ( to 2*hdp max ) unless
c.....ps endflat contact or unless a reduction has occured
c
      dp = min2_r8 (dp*dsqrt(tol/crwn), 2.d0*hdp)
      if (ifl(56).gt.1) dp = min2_r8 (dp, 1.5d0*hdp)
626   if (dp.lt.dpmax) goto 627
         dp = dpmax
         if(hdp.ge.dpmax) goto 660
627   idpk = idpk + 1
      if (idpk.eq.5) dp = min2_r8 (50.d0*tol, dpmax)
c
c.....trouble. set dp small and go one more time.
c
      if (idpk.le.5) goto 110
c
c.....calc next dp
c
630   if (si.gt.1.d-6) goto 635
632   dp = dpmax
634   t(11,ia) = 1000.
      goto 640
635   rc = dp/si
      t(11,ia) = rc
      dp = dsqrt(8.*rc*tol)
640   dp = min2_r8 (dp,dpmax)
c
c************************************ check endup
c
660   dp4 = dp
c
c..... Eduard 5/14/1999. Extra precaution needed against the situation
c..... when the move started at an edge and is going toward the opposite
c..... edge. This is a weak fix designed to change as little as possible). 
c..... The situation might cause more trouble. 
c
      if (ltsk.eq.2 .and. iptk.eq.0 .and. isav.ge.1) then
        kret = 1
      else
        call uvend(ltsk,kret,dp4)
      endif
c     if (.not.lv92.and.ltsk.eq.1.and.iptk.gt.1.and.ifl(2).eq.0) then
c       if (f_dist4 (t(1,ia),t(1,ic)) .le. tol) ifl(2) = 163
c     endif
661   dp = dp4
c
c.....if itfl=100, all done. otherwise bra on kret
c
      if (itfl.eq.100) goto 999
c
c.....kret = 0     re-calc this loc per new dp.
c.....     = 1     go output this loc and exit or cont.
c
      if (kret.eq.0 .and. iptk.gt.0) then
        dpmax = dpend
        goto 100
      endif
c
c..... noedge means the current curve segment has overlapped the starting 
c..... point (without ever reaching an edge);
c..... so we end here, with a closed curve as a result
c
      if (itsk.eq.2 .and. .not.lv92 .and. iptk.gt.2) then
        if (abs (uv1(1) - t(13,ia)) .gt. 0.5 .or.
     *      abs (uv1(2) - t(14,ia)) .gt. 0.5) goto 800
        co = f_dot4 (t(7,ia),vc1)
        if (co .gt. 0.866) then
          dis = f_dist4 (t(1,ia),pt1)
          if (dis .lt. 4.*tol) then
            noedge = .true.
            goto 800
          endif
          call vcmnvc4 (t(1,ia),t(1,ic),v)
          dis = f_dist4 (t(1,ia),t(1,ic))
          if (dis .gt. tol) then
            v(1) = v(1)/dis
            v(2) = v(2)/dis
            v(3) = v(3)/dis
            call vcmnvc4 (pt1,t(1,ic),vc)
            b = f_dot4 (v,vc)
            if (b.gt.-tol .and. b.lt. dis+tol) then
              c = f_mag4 (vc)
              if (c*c - b*b .lt. 16.*tol*tol) then
                noedge = .true.
                call vctovc4 (pt1,t(1,ia))
                t(13,ia) = uv1(1)
                t(14,ia) = uv1(2)
              endif
            endif
          endif
        endif
      endif
c
c************************************ rotate cols, go again.
c
800   iptk=iptk+1
      ia=ia+1
      ib=ib+1
      ic=ic+1
      if(ia.gt.3) ia=1
      if(ib.gt.3) ib=1
      if(ic.gt.3) ic=1
c
c... check curvature from last entered pt+fwd
c... add this pt to tdat
c
      dis=hh(7)*(t(1,ic)-hh(1))+hh(8)*(t(2,ic)-hh(2))+
     *    hh(9)*(t(3,ic)-hh(3))

      call conv4_8 (t(1,ic),hh,9)
c
c...if not in-line with former, issue error
c
      if (iptk.lt.2.or.ltsk.eq.1.or.dis.gt.-tol) goto 8075
         ifl(2) = 255
         goto 999
8075  continue
c
c... calculate uv-slope vector for PS and DS
c
      uvcs(1) = t(13,ic) - t(13,ib)
      uvcs(2) = t(14,ic) - t(14,ib)
      sec = dsqrt(uvcs(1)**2 + uvcs(2)**2)

      if ((lv92 .and. sec .gt. 1.d-5) .or. sec .gt. 1.d-4) then
         uvcs(1) = uvcs(1)/sec
         uvcs(2) = uvcs(2)/sec
         kfln = 1
      else
         kfln = 0
      end if

      if (itsk .eq. 2) then
        if (lv91.and.isav.ge.1.and.ix.eq.0) call conv4_8 (t(1,ic),pts,3)
c
c...store fwd vector for the 1-st and last point
c
        if (lv91 .or. isav.eq.1) call conv4_8 (t(7,ic),vx(ix+1),3)

        if (lv91) call conv4_8 (t(1,ic),p(ntk+1),3)
c
c..... save first point, forward and (u,v) for the 'noedge' logic
c
        if (.not.lv92 .and. itsk.gt.1 .and. iptk.eq.1) then
          call vctovc4 (t(1,ic),pt1)
          call vctovc4 (t(7,ic),vc1)
          uv1(1) = t(13,ic)
          uv1(2) = t(14,ic)
        endif
        if (isav .ge. 1) then
          if (isav.eq.2 .or .isav.eq.3) then
            uv(1) = t(13,ic)
            uv(2) = t(14,ic)
            uv(3) = 0.
            call ncl_push_uv (uv)
          endif
          if (isav.eq.1 .or .isav.eq.3) then
            call conv4_8 (t(1,ic),uv,3)
            call ncl_push_uv (uv)
          endif
        endif
        
        ix = 3
c
c... store the first 2 and the last 2 uv-points for PS and DS;
c... to be used in the end of the routine to extrapolate the curve
c... to the boundary of one of the surface
c
        if (isav.eq.1) then

           if (iptk.eq.1) then

              if (jd(1).ne.PLANE .and. jd(1).ne.NETSF) then
                 uvps1(1) = t(13,ic)
                 uvps1(2) = t(14,ic)
                 call um_vctovc_2d (uvps1(1),uvps2(3)) 
              endif

              if (jd(201).ne.PLANE .and. jd(201).ne.NETSF) then
                 uvds1(1) = t(19,ic)
                 uvds1(2) = t(20,ic)
                 call um_vctovc_2d (uvds1(1),uvds2(3)) 
              endif

           else
c
c... get the next uv-point at distance > separ from the first one
c
              if (jd(1).ne.PLANE .and. jd(1).ne.NETSF) then

                 if (.not.psdone) then
                    uvps1(3) = t(13,ic)
                    uvps1(4) = t(14,ic)
                    if (um_dist_2d (uvps1(1),uvps1(3))
     *                 .gt. separ) psdone = .true.
                 endif

                 uvtmp(1) = t(13,ic)
                 uvtmp(2) = t(14,ic)

                 if (um_dist_2d (uvtmp,uvps2(3)) .gt. separ) then
                    call um_vctovc_2d (uvps2(3),uvps2(1))
                    call um_vctovc_2d (uvtmp,uvps2(3))
                 endif

              endif

              if (jd(201).ne.PLANE .and. jd(201).ne.NETSF) then

                 if (.not.dsdone) then
                    uvds1(3) = t(19,ic)
                    uvds1(4) = t(20,ic)
                    if (um_dist_2d (uvds1(1),uvds1(3))
     *                 .gt. separ) dsdone = .true.
                 endif

                 uvtmp(1) = t(19,ic)
                 uvtmp(2) = t(20,ic)

                 if (um_dist_2d (uvtmp,uvds2(3)) .gt. separ) then
                    call um_vctovc_2d (uvds2(3),uvds2(1))
                    call um_vctovc_2d (uvtmp,uvds2(3))
                 endif

              endif

           endif

        endif

      else if (lv91) then
        p(ntk+1) = t(13,ic)
        p(ntk+2) = t(14,ic)
        p(ntk+3) = 0.0
      endif

      ntk=ntk+3
c
c ... aak 08-jan-1998: if searching for a single intersection pt (itsk=1),
c ... exit when 3*maxp exceeded; will retry a diff. nearpt from "cvptsn";
c ... otherwise (itsk=2), save points in list
c
      if (ntk .ge. maxp) then
        ifl(2) = 163
        goto 999
      endif

850   continue
      if (lv91) sc(160) = sc(160) + f_dist4 (t(1,ic),t(1,ib))
c
c...if interrupt switch on, abort motion
c...RAH: added call to set intr flag - effective on SGI ONLY
c
      call ckintr(ifl(86),ifl(35))
      if (ifl(86).ne.0) then
        ifl(2) = 149
        goto 999
      endif

      goto 100

999   continue
      if (sc(169) .lt. 9.249d0) then
        ifl(331) = ifl331
        ifl(332) = ifl332
      endif
c
c... aak 09-feb-1998:
c... extrapolate the first and last point of the curve to the
c... boundary of appropriate surface and add the 2 new points to the
c... curve list;
c
c... uvps1(1-2) = PS u-v coord. of the 1st curve end
c... uvps1(3-4) = PS u-v coord. of the next point along the curve
c... uvps2(3-4) = PS u-v coord. of the 2nd curve end on PS
c... uvps2(1-2) = PS u-v coord. of the previous point along the curve
c
c... if all points are far from boundary or evaluator error (ierr > 0),
c... don't add anything
c
      if (itsk.eq.2 .and. isav.eq.1 .and. .not.noedge) then
         call gtdesc (sc(35),nclkey,nwds,ietype)
         isrf = 1
         call evstup(nclkey,isrf)
         call gtdesc (sc(11),nclkey,nwds,ietype)
         isrf = 2
         call evstup(nclkey,isrf)

         call goto_edge (uvps1,uvps2,uvds1,uvds2,ptx,plast,iwch,ierr)
c
c... ptx,plast hold the first/last points of the curve; insert them 
c... into the list and also save in plast as the last element of p() -
c... to be used in "presfe"
c
         if (ierr.gt.0) return
         ntoins = 1
         indx = 0
         if (iwch .eq. 1 .or. iwch .eq. 3) then
            call ncl_insert_uv (ptx,ntoins,indx)
            ntk = ntk + 3
         else if (.not.lv92) then
            call ncl_replace_uv (ntoins,ptx)
         endif
         if (iwch .eq. 2 .or. iwch .eq. 3 .or. .not.lv92) then
            n = ntk/3
            call ncl_replace_uv (n,plast)
            if (lv91) call vctovc (plast,p(ntk-2))
         endif
      endif

      return
      end

c ******************************************************
c SUBROUTINE: goto_edge ((p1,q1,p2,q2,pt1,pt2,ierr)
c 
c Extrapolates 2 endpoints of a curve obtained as INTOF/SF1,SF2
c to the boundary of one of the surfaces SF1,SF2
c
c INPUT:
c       p1 -  uv-parameters of 2 close points on SF1 at 1st end of curve
c       q1 -  uv-parameters of 2 close points on SF1 at 2nd end of curve
c       p2,q2 -  uv-parameters of the same points on SF2
c
c OUTPUT:
c       pt1,pt2 - points on the boundary of one of the surfaces 
c                 SF1/SF2 near the 1st/2nd end of the curve
c       iwch - 1 = New 1st point calculated, 2 = New end point calculated,
c              3 = Both points calculated.
c       ierr - error flag; if ierr = 1000 - both sets of points are 
c              far from surface boundaries
c ******************************************************

      subroutine goto_edge (p1,q1,p2,q2,pt1,pt2,iwch,ierr)
      include 'com.com'

      real*8 p1(4),p2(4),q1(4),q2(4),pt1(3),pt2(3)
      integer*2 ierr,iwch
c
c... local variables
c
      real*8 uv(4),uv1(2),uv2(2),vec1(2),sv(9),
     *       d,dmin,zero/0.d0/,one/1.d0/,min2_r8,max2_r8
      integer*2 isf,i,imin
      logical lv92

      ierr = 0
      iwch = 0
      lv92 = sc(169).lt.9.2d0
c
c... decide to edge of which surface 1 or 2 to go:
c... find the point out of p1,q1,p2,q2 closest to the boundary
c... of the [0,1]x[0,1] square.
c..... Eduard 2/8/2001 - process each endpoint separately since one endpoint 
c..... could be at an edge of SF1 while the other one at an edge of SF2 - FSR 
c..... 60354
c
      do 2 i = 1,4
         p1(i) = min2_r8 (one, max2_r8 (zero,p1(i)))
         q1(i) = min2_r8 (one, max2_r8 (zero,q1(i)))
         p2(i) = min2_r8 (one, max2_r8 (zero,p2(i)))
         q2(i) = min2_r8 (one, max2_r8 (zero,q2(i)))
2     continue

      call conv8_8 (p1(1),uv(1),2)
      call conv8_8 (p2(1),uv(3),2)

      dmin = 1.e+09
      imin = 0

      do 1 i = 1,4
         d = min2_r8 (uv(i),one - uv(i))

         if (d.lt.dmin) then
            dmin = d
            imin = i
         endif

1     continue
c
c... if closest point far from boundary return error
c
      if (dmin .gt. .01) then
         ierr = 1000
         return
      endif

      if (imin.le.2) then
         isf = 1
         call um_vctovc_2d (p1(1),uv2)
         call um_vcmnvc_2d (p1(1),p1(3),vec1)
      else
         isf = 2
         call um_vctovc_2d (p2(1),uv2)
         call um_vcmnvc_2d (p2(1),p2(3),vec1)
      endif

      call um_unitvc_2d (vec1,vec1)

      call um_to_edge (uv2,vec1,uv1)
c
c...Make sure we don't return same point
c...as the input point
c...Bobby  -  7/13/99
c
      d = (uv1(1)-uv2(1))**2 + (uv1(2)-uv2(2))**2
      if ((lv92 .and. d .ne. 0.) .or. (d .gt. 1.d-10)) then
          call uevsft (uv1(1),uv1(2),isf,sv,ierr)
          if (ierr.gt.0) return
          call vctovc (sv,pt1)
          iwch = 1
      else if (.not.lv92) then
c
c..... if the points are same, still reproject in order to have exact match
c
          call uevsft (uv1(1),uv1(2),isf,sv,ierr)
          if (ierr.gt.0) return
          call vctovc (sv,pt1)
      endif

      if (lv92) then
      call conv8_8 (q1(1),uv(1),2)
      call conv8_8 (q2(1),uv(3),2)
      else
      call conv8_8 (q1(3),uv(1),2)
      call conv8_8 (q2(3),uv(3),2)
      endif

      dmin = 1.e+09
      imin = 0

      do 3 i = 1,4
         d = min2_r8 (uv(i),one - uv(i))

         if (d.lt.dmin) then
            dmin = d
            imin = i
         endif

3     continue
c
c... if closest point far from boundary return error
c
      if (dmin .gt. .01) then
         ierr = 1000
         return
      endif

      if (imin.le.2) then
         isf = 1
         if (lv92) then 
           call um_vctovc_2d (q1(1),uv2)
           call um_vcmnvc_2d (q1(1),q1(3),vec1)
         else
           call um_vctovc_2d (q1(3),uv2)
           call um_vcmnvc_2d (q1(3),q1(1),vec1)
         endif
      else
         isf = 2
         if (lv92) then 
           call um_vctovc_2d (q2(1),uv2)
           call um_vcmnvc_2d (q2(1),q2(3),vec1)
         else
           call um_vctovc_2d (q2(3),uv2)
           call um_vcmnvc_2d (q2(3),q2(1),vec1)
         endif
      endif

      call um_unitvc_2d (vec1,vec1)

      call um_to_edge (uv2,vec1,uv1)

      d = (uv1(1)-uv2(1))**2 + (uv1(2)-uv2(2))**2
      if ((lv92 .and. d .ne. 0.) .or. (d .gt. 1.d-10)) then
          call uevsft (uv1(1),uv1(2),isf,sv,ierr)
          if (ierr.gt.0) return
          call vctovc (sv,pt2)
          iwch = iwch + 2
      else if (.not.lv92) then
          call uevsft (uv1(1),uv1(2),isf,sv,ierr)
          if (ierr.gt.0) return
          call vctovc (sv,pt2)
      endif

      return
      end
c
c **********************************************************************
c **
c **  subroutine name: uvmoveo
c **                   this is a modified version of the mover routine
c **
c **  last revision:
c **  purpose of subroutine: generates points along intersection of a 
c **                         surf and a surf or plane.
c **  input:
c **     itsk = type of call being made
c **            1 = move to a point on the two surfaces near the point
c **                specified in p(1-3)
c **            2 = generate points along the intersection of the two
c **                surfaces
c **     maxp = size of array p
c **  output:
c **     p    = array to store generated points
c **
c **********************************************************************
c **********************************************************************

      subroutine uvmoveo (itsk,maxp,p,vx)

      include 'com4a.com'
      include 'mocom.com'
 
      integer*2 itsk,maxp
      real*8 p(600),vx(6)

      integer*2 jd(600)
      real*4 ad(300),asc(100)
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
c
      real*8 hh(10)
      real*8 v(3),dl,a,b,sec,si,pp,q,pcon,dcon,fx,fy,fz,ro,co,
     -       tol,told
c
      ix = 0
      ntk=0
      call fill_array (hh,10,zero)
      tol=sc(27)
      told=tol
      ltsk = itsk
      if (itsk .gt. 2) ltsk = 2
      if (ltsk.eq.2) told=.4*tol
      dpmax=tol*250.
      if (dpmax.gt.sc(54)) dpmax=sc(54)
c          unset the withold o/p and endpas re-try flags   8-3-82
      ifl(29)=0
      ifl(28)=0
c          min/max toler range for crwnh and jog tol
c              ( accept liberally at this time )
      atol=tol*.5
      btol=tol*1.4
      dtol=tol*10.
      dtolsq=4.*tol**2
      qtolsq=900.*dtolsq
c          set probable iflat flg if ps,ds are in ln,pl class
      iflat=0
      if(jd(1).lt.7.and.jd(201).lt.7)iflat=1
20    dp=0.
      t(10,2)=0.
      itfl=-2
      iptk=0
c              col headers
      ia=1
      ib=2
      ic=3
c
c***********************************  basic step cycle startpoint
c          set extrap coefs.
c
100   idpk=0
      ireduc=0
      irejog=0
110   ro=0.
      if(t(10,ib).lt..001)goto 120
      ro=t(10,ic)/t(10,ib)
      if(ro.gt.5.)ro=5.
120   q=ro*.5
      t(10,ia)=dp
c          extrap te,ta    ( if no error )
      if(ifl(2).gt.0) goto 999
      do 140 i=1,3
      j=i+3
      k=i+6
c              xyz and ijk
      t(i,ia)=t(i,ic)+dp*(t(k,ic)+q*(t(k,ic)-t(k,ib)))
      t(j,ia)=t(j,ic)+ro*(t(j,ic)-t(j,ib))
140   continue
c           do ps,ds srf pn's
c                                  ps *********
210   isrf=1
      s(8,1)=t(1,ia)
      s(9,1)=t(2,ia)
      s(10,1)=t(3,ia)
      if(jd(1).eq.6) then
        sec=s(1,1)*s(8,1)+s(2,1)*s(9,1)+s(3,1)*s(10,1)-s(4,1)
        s(5,1)=s(8,1)-sec*s(1,1)
        s(6,1)=s(9,1)-sec*s(2,1)
        s(7,1)=s(10,1)-sec*s(3,1)
        goto 220
      end if
c          extrap u,v,hps  (unless rejog)
      if(irejog.ne.0)goto 215
      t(13,ia)=t(13,ic)+ro*(t(13,ic)-t(13,ib))
      t(14,ia)=t(14,ic)+ro*(t(14,ic)-t(14,ib))
215   call surfpn(t(13,ia),t(14,ia),1)
      if(ifl(2).gt.0)goto 999
c                                  ds *********
220   isrf=2
c          extrap u,v  and h   ( unless rejog )
      if(irejog.ne.0)goto 225
      do 222 i=19,21
222      t(i,ia)=t(i,ic)+ro*(t(i,ic)-t(i,ib))
c
c...vp93.10.25 this 'if' is extracted from dsrfpn routine
c...and may be replaced in future by call dsrfpn.
c
225   if (jd(201) .ne. 6) then
        s(8,2)=t(1,ia)
        s(9,2)=t(2,ia)
        s(10,2)=t(3,ia)
        call surfpn(t(19,ia),t(20,ia),1)
        if(ifl(2).gt.0)goto 999
      else
        s(5,2)=t(1,ia)
        s(6,2)=t(2,ia)
        s(7,2)=t(3,ia)
      end if
c
c******************************* fwd is crossf ps(up) and ds(rgt)
c
300   fx=s(3,2)*s(2,1)-s(2,2)*s(3,1)
      fy=s(1,2)*s(3,1)-s(3,2)*s(1,1)
      fz=s(2,2)*s(1,1)-s(1,2)*s(2,1)
      sec = dsqrt(fx**2+fy**2+fz**2)
      if(sec.gt.1.e-3) goto 310
c          error. fwd sense indefinite.
      ifl(2)=385
      goto 999
c
310   continue
      if (iptk.eq.0.and.ltsk.eq.1) goto 320
      if(fx*t(7,ic)+fy*t(8,ic)+fz*t(9,ic).ge.0.)goto 320
      sec=-sec
c          error. fwd sense reversal.
c      ifl(2)=125
c315   continue
c      goto 999
c          fwd ok. add to col(ia)
320   t(7,ia)=fx/sec
      t(8,ia)=fy/sec
      t(9,ia)=fz/sec
      if (ifl(23) .eq. 0) goto 500
c
c***********************************  set tool axis vector
c
410   t(4,ia)=s(1,1)
      t(5,ia)=s(2,1)
      t(6,ia)=s(3,1)
c
c          nest tool to ps,ds
c
500   pcon=s(4,1)
510   dcon=s(4,2)
520   si=s(1,1)*s(1,2)+s(2,1)*s(2,2)+s(3,1)*s(3,2)
      pp=s(1,1)*t(1,ia)+s(2,1)*t(2,ia)+s(3,1)*t(3,ia)-pcon
      q=s(1,2)*t(1,ia)+s(2,2)*t(2,ia)+s(3,2)*t(3,ia)-dcon
      a=(pp-si*q)/(1.-si**2)
      b=q-a*si
c
c...get distance from point to i/o ps & ds
c
      do 522 i=1,3
          v(i) = a * s(i,1) + b * s(i,2)
          t(i,ia)=t(i,ia)-v(i)
522   continue
c
c...get distance between points
c
      dl = dsqrt(v(1)**2+v(2)**2+v(3)**2)
c
c...vp93.10.25 replaced tol check from a,b to dist of points.
c...check for large move (fix 1-time only)
c
      if (sc(169) .lt. 8.20799) then
          if(dabs(a).lt.told .and. dabs(b).lt.told)goto 590
      else
          if (dl .lt. told) go to 590
      end if
      if(irejog.gt.20) then
          ifl(2)=163
          if (abs(si).gt..99)ifl(2)=385
          goto 999
      endif
      irejog=irejog+1
      goto 210
590   continue

      if(itfl.ge.0) goto 620
      itfl=0
      dp=250.*tol
      if(dp.gt.dpmax)dp=dpmax
      if(iflat.eq.1.and.ifl(23).ne.4.and.jd(401).ne.7)dp=dpmax
      goto 660
c
c          verify step tol (if in iter mode)
620   continue
      if (ltsk.eq.1) goto 660
      hdp=dp
      co=t(7,ia)*t(7,ic)+t(8,ia)*t(8,ic)+t(9,ia)*t(9,ic)
C                         EXTRA CRWN CHECK                23-MAY-90  IJD
      FX=T(1,IA)-T(1,IC)
      FY=T(2,IA)-T(2,IC)
      FZ=T(3,IA)-T(3,IC)
      SEC=dsqrt(FX**2+FY**2+FZ**2)
      IF (SEC.LT..00001) GOTO 6202
      ACO=(FX*T(7,IC)+FY*T(8,IC)+FZ*T(9,IC))/SEC
      IF (ACO.LT..1)ACO=.1
      IF (ACO.LT.CO) CO=ACO
6202  CONTINUE
C                         END EXTRA CRWN CHECK      
      if(co.gt..9999999995d0)co=1.
      si=dsqrt(1.-co**2)
      crwn=dp*si/8.
c          if flat case, probable exit  (except when cs is circle)
621   if(crwn.gt.1.e-5)goto 622
c...
      if(iflat.eq.1.and.jd(401).ne.7)goto 632
c          this is a computed flat case. be conservative.
c           if ps endflat contact, even more    "  "
      dp=hdp*1.5
      if(ifl(56).gt.1) dp=hdp*1.25
      goto 634
622   if(crwn.lt.btol)goto 624
c          crwn too large. shorten dp ( but 1/4 hdp min )
      dp=dp*dsqrt(tol/crwn)
      ireduc=1
      if(dp.lt.hdp/4.)dp=hdp/4.
      goto 626
624   if(crwn.gt.atol.or.ireduc.gt.0.or.itfl.gt.0) goto 630
c          crwn too small. increase dp ( to 2*hdp max ) unless
c           ps endflat contact or unless a reduction has occured
      dp=dp*dsqrt(tol/crwn)
      if(dp.gt.2.*hdp)dp=2.*hdp
      if(dp.gt.1.5*hdp.and.ifl(56).gt.1) dp=1.5*hdp
626   if(dp.lt.dpmax) goto 627
      dp=dpmax
      if(hdp.ge.dpmax) goto 660
627   idpk=idpk+1
      if(idpk-5)6292,629,630
c          trouble. set dp small and go one more time.
629   dp=50.*tol
      if(dp.gt.dpmax)dp=dpmax
6292  continue
      goto 110
c          calc next dp
630   if(si.gt.1.e-6)goto 635
632   dp=dpmax
634   t(11,ia)=1000.
      goto 640
635   rc=dp/si
      t(11,ia)=rc
      dp=dsqrt(8.*rc*tol)
640   if(dp.gt.dpmax)dp=dpmax
c
c************************************ check endup
c
660   call uvend(ltsk,kret,dp)
c          if itfl=10, all done. otherwise bra on kret
      if(itfl.eq.10)goto 99999
c              kret=0     re-calc this loc per new dp.
c                  =1     go output this loc and exit or cont.
      if(kret.eq.0)goto 100
      goto 800
c
c************************************ rotate cols, go again.
c
800   iptk=iptk+1
      ia=ia+1
      ib=ib+1
      ic=ic+1
      if(ia.gt.3)ia=1
      if(ib.gt.3)ib=1
      if(ic.gt.3)ic=1
c---------
c
c    if indicated, add this pt to tdat.  (also ijk if multax on)
c
805   continue
c          check curvature from last entered pt+fwd
      dis=hh(7)*(t(1,ic)-hh(1))+hh(8)*(t(2,ic)-hh(2))+
     1    hh(9)*(t(3,ic)-hh(3))
      dx=hh(1)+hh(7)*dis-t(1,ic)
      dy=hh(2)+hh(8)*dis-t(2,ic)
      dz=hh(3)+hh(9)*dis-t(3,ic)
      esq=dx**2+dy**2+dz**2
c          if err large or logstpt, go hold this loc
c      if(esq.gt.dtolsq.or.iptk.eq.1)goto 806
c          check ta chg
c      acos=hh(4)*t(4,ic)+hh(5)*t(5,ic)+hh(6)*t(6,ic)
c      if(acos.lt..999992)goto 806
c          withold this loc from o/p and set ifl(29)
c      ifl(29)=1
c      goto 850
c          hold this loc being entered in tdat
806   do 807 i=1,9
807   hh(i)=t(i,ic)
c          if not in-line with former, issue error
      if(iptk.lt.2.or.ltsk.eq.1.or.dis.gt.-tol)goto 8075
      ifl(2)=255
      goto 99999
8075  continue
808   continue
c...                  save point
      if (ntk.lt.maxp) goto 820
      ifl(2)=410
      goto 999
820   continue
      if (itsk .eq. 2) then
        p(ntk+1)=t(1,ic)
        p(ntk+2)=t(2,ic)
        p(ntk+3)=t(3,ic)
c
c...store fwd vector for the 1-st and last point
c
        vx(1+ix) = t(7,ic)
        vx(2+ix) = t(8,ic)
        vx(3+ix) = t(9,ic)
        ix  = 3
      else
        p(ntk+1)=t(13,ic)
        p(ntk+2)=t(14,ic)
        p(ntk+3)=0.0
      end if
      ntk=ntk+3
850   continue
      sc(160)=sc(160)+sqrt((t(1,ic)-t(1,ib))**2+(t(2,ic)-t(2,ib))**2
     x                    +(t(3,ic)-t(3,ib))**2) 
c            if interrupt switch on, abort motion
c RAH: added call to set intr flag - effective on SGI ONLY
      call ckintr(ifl(86),ifl(35))
      if(ifl(86).ne.0) then
        ifl(2)=149
        goto 999
      endif
      goto 100
c          exit from mover when itfl=10
c875   if(iptk.lt.ifl(91)) goto 992
c  $$$$$   error.    numpts exhausted
c      ifl(2)=141
c      goto 999
c992   if(itfl.ne.10) goto 100
999   continue
99999 continue
      return
      end
