C*********************************************************************
C*    NAME         :  mover.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       mover.f , 25.2
C*    DATE AND TIME OF LAST MODIFICATION
C*       03/01/16 , 11:15:35
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mover
c*          this is the main 3d motion control subroutine.
c*          chg:  if ifl(298) on, ret to mocomb with pass
c*                 logical startpt            26-apr-89  pem
c*
c*     1.  premov has prepared 3 control srfs in d-tbl plus all
c*         req'd pass params.
c*
c*     2.  tool loc in sc(1-9) does not chg until pass succeeds.
c*
c*     3.  t-tbl is the main memory area during pass.
c*
c*     4.  error flag ifl(2) is checked throughout mover and
c*         causes exit if plus.
c*
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine mover

      include 'com4a.com'
      include 'comgt.com'
      include 'const.com'
      include 'mocom.com'
      include 'csrel.com'
      include 'gidcom.com'
      include 'shrpcv.com'
c
      common/hsccom/hsc
      real*8 hsc(9)
c
c...  note: mocom(272-288) used by wrt125,wrttek 7-sep-84
c.... (289-293) used by plotcc
c.... update mocom next real chg.!!!!!!!
c
      common/clccom/svtool(6),iflsv(3)
      real*4 svtool
      integer*2 iflsv

      common/dcpcom/svcpt(6)
      real*8 svcpt

      common/hptcom/hldctp
      real*8 hldctp(27)

      common/tpvcom/tavend
      real*8 tavend(3)

      common/reset/ reset_dp
      logical reset_dp

      common/tantocsl/llttcs

      common/lsetad2/setad2
      logical setad2

      common/psldcom/irejog,ro,idsjog
      integer*4 irejog,idsjog
      real*4 ro

      common/stdist/csdis0,itry,itfl0
      real*4 csdis0,csdis1,csdt
      integer*2 itry,sharp,itfl0

      common/gidcm2/ gidst,dst1,irejdp,gidmvd,gdatcr
      real*8 gidst,dst1
      logical gidmvd,gdatcr
      integer*2 irejdp

      common/fancmbcom/fan160,lfncmb
      real*8 fan160
      integer*2 lfncmb

      integer*2 jd(600),itfl,iptk,ntk,ia,ib,ic,isrf,isltim,fdeg
      real*4 ad(300),asc(400),sbe,cbe,cutn,alp,bet,rate,rate1
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      equivalence (ifl(295),fdeg)
      equivalence (asc(280),rate)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(ifl(243),isltim)
c
c... local variables
c
      real*8 tbn(24),hht(9),tdat(420),tb(24),otb(24),ptb(24),ds(3)
      real*8 tapt(3),vtmp(3),tavst(3),cstapt(3),ocstpt(3)
      real*8 vpsnrm(3),vdsnrm(3),vcsnrm(3),pte(3),hh(10)
      real*8 primdat(16),ccen(3),plx(4),cvec(3)
      real*8 psu0,psv0,dsu0,dsv0,mindis,sc160,sc176,asn,ctol
      real*8 ucv,totdis,tacvds,psthk,dsthk,csthk,diff,tdis
      real*8 dtmp,cdis,tcdis,dis0,dis1,sc20,sc21,sc81,sc82,sc83,dpfac
      real*8 min2_r8,f_dist, fslow, f_mag, f_dot

      real*4 dpmax,dpmin,dpm,reff,dpmax1,rval,dir,cute
      real*4 angmax,angdp,angfct,trgt,ddel,tol,atol,btol,dtol,dtolsq
      real*4 qtolsq,dp,co,si,q,fx,fy,fz,sec,dot1,dis,alx,aly,alz
      real*4 xu,yu,zu,tbe,sal,cal,rx,ry,rz,sisq,ti,tj,tk,xr,yr,zr
      real*4 pcon,pco,psi,dcon,dco,dsi,a,b,p,a1,b1,hdp,crwn,aco,ang,rc
      real*4 cutmax,csdis,hdis,rdp,x,y,z,d1,f1,f2,h,dx,dy,dz,esq,acos
      real*4 tonp,csu1,csv1,EPS,absa,absb,hcsdis,usp,vsp,hds0,del1
      real*4 vec(3),fwd0(3),oldcs(4),pte1(3),vta1(3),savta(3)
      real*4 f_dot4, f_dist4, f_mag4, max2_r4, min2_r4, min3_r4

      integer*4 keytcv,lpos,isv2(2),iept(2),mupt,ierr,nclkey

      integer*2 isv104,mod,numpts,lnk,kdntcu,isubcl,iflat,knttcv,jptk
      integer*2 idpk,ireduc,i,j,jret,kret,ifl29,numitm,ifl83,jd403
      integer*2 isv1,iclf,nwds,ietype,isf,iclass,svmps,izro,MAX_ITER
      integer*2 ips, dstype,primtyp,fanint,iflg,inidcv
      integer*2 initds,ksn(4),ifl289,lifted,icsgcksav,ntry51

      logical lv81,lv82,lv83,lv84,lv90,lv91,lv92,lv93,lv94,lv95,lv96
      logical lv97
      logical lcvrev,lbfst,go_slow,lfansf,tryd,llttcs,icck,lijk,lstald
      logical gougup,lnodisk,ltoolcon,ltoolcyl,ldsgck

      equivalence (ifl(67),initds),(asn,ksn),(sc(168),ctol)

      data izro/0/, MAX_ITER/5/
      real*8 PSMULT_FWD_FUZZ /1.d-08/, CURVE_TOL/0.01/
c
c...Debug variables
c
      integer*4 DEBUGX
      parameter (DEBUGX=1)
      character*80 dbuf
      byte dout(80)
c      if (sc(223) .eq. -10000) goto 1085
      
      if (DEBUGX .ne. 0) then
          write (dbuf,7000) sc(1),sc(2),sc(3),sc(4),sc(5),sc(6)
 7000     format ('Mover-I = ',6f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

      lv81 = sc(169).lt.8.19999d0
      lv82 = sc(169).lt.8.229d0
      lv83 = sc(169).lt.8.399d0
      lv84 = sc(169).lt.8.499d0
      lv90 = sc(169).lt.9.049d0
      lv91 = sc(169).lt.9.149d0
      lv92 = sc(169).lt.9.249d0
      lv93 = sc(169).lt.9.349d0
      lv94 = sc(169).lt.9.449d0
      lv95 = sc(169).lt.9.549d0
      lv96 = sc(169).lt.9.649d0
      lv97 = sc(169).lt.9.749d0

      if (mocom .and. .not.lmdnow .and. (sc(81) .ne. 0.0 .or. 
     x                sc(82) .ne. 0.0 .or. sc(83).ne. 0.0)) then
         if (.not.lapmov) then 
            dis1 = f_dist (tb, sc(1))
         else
            if (ifl(352) .eq. 1) dis1 = f_dist (savtb, sc(1))
         endif
      endif     

      call conv8_4 (sc(4),savta,3)
      call unitizevc4 (savta)

      lcsavd = .false.
      fanint = 0
      svmps  = 0
      lpos   = 0
      ifl289 = ifl(289)
      ntry51 = 0
c
c...UMR reported by Dassault 2/15/07
c
      totdis = 0.
      tdis   = 0.
c
      call gtdesc (sc(145),nclkey,nwds,dstype)
      if (dstype.eq.SURF) then
c
c..... Dassault'c NCL315: the 'indsuv' logic was not used for some calls
c..... from mocomb since not all of them go through domove
c
        if ((.not.lv93 .and. ifl(280).eq.0) .or. domov)
     x    call indsuv (nclkey,t(19,3),t(20,3))
      endif
C
C...If the expanded CL file option is set then set the motion record
C...number to 5200 and the number of elements per point to 21.
C
C...Internal clfile is always stored in expanded form
C...Bobby - 5/7/02
C
cc      if (lexpcl) then
      if (.not. lv92) then
        mupt = 21
        iclass = 5200
        if (ifl(42).eq.0) lpos = 1
        if (ifl(335) .eq. 1) lpos = 0
      else
        mupt = 3 * (ifl(82) + 1)
        iclass = 5000
      endif

      call fill_array (tb(10),12,ZERO)
      call fill_array (hh,10,ZERO)

      isv104  = ifl(104)
      llttcs = lttcsf
      setad2 = .true.

      csdis0 = 0.
      csdis1 = 0.
      l2csrf = .false.
      lhavecspl = .false.
      icsgcksav = icsgck
c
c..... Dassault's test case of 02/28/2008: when 'tanto,cs' the gouge
c..... check level 4 is wrong
c
      if (lttcsf .and. icsgck.gt.3) icsgck = 3

      iclf = 0
      mod = ifl(23)
      lfansf = mod.eq.TA_FAN .and. jd(401).ne.PLANE
      if ((ifl289.eq.1 .or. ifl289.eq.2) .and. (mod.ne.4 .or.
     *     lv91 .or. tool(2).lt.10.*sc(27))) ifl(289) = 0
c
c... jingrong 12/16/98 save u,v value at motion starting point.
c
      psu0 = t(13,3)
      psv0 = t(14,3)
      dsu0 = t(19,3)
      dsv0 = t(20,3)
      hds0 = t(21,3)
      if ( jd(201).eq.CURVE .or. jd(201).eq.CIRCLE
     *                        .or. jd(201).eq.LINE) then
        hds0 = tool(2)
      endif

      tol    = sc(27)
      psthk  = sc(23)
      dsthk  = sc(24)
      csthk  = sc(25)

      if (cntctp) then
        ifl(56)=0
        iflsv(1) = ifl(23)
        iflsv(2) = ifl(282)
        iflsv(3) = 0
        if (.not.lv91) then
          if (lmdnow) iflsv(3) = 1
          lmdnow = .false.
        endif
        mod = 1
        ifl(282) = 0
        call conv4_4 (tool(1),svtool(1),6)
        call fill_array4 (tool,6,0.)
      endif

      cute = tool(6)

      lnodisk = asc(66).eq.0

      ltoolcon = sc(31).ne.0 .and. ifl(282).eq.0 .and. lnodisk
      ltoolcyl = sc(31).eq.0 .and. ifl(282).eq.0 .and. lnodisk

      ldsgck = jd(201).eq.SURF .and. ifl(21).ne.0 .and. idsgck.ge.4
     x         .and. .not.lmdnow .and. .not.lcvgid .and. ltoolcyl
     x         .and. cute.gt.2*tol
c
c... secondary part surface.
c
      ips = 1
      if (lsecps .and. mod.ne.TA_FAN) then
        usp = .5
        vsp = .5
        dir = .0
        ips = 4
        call sfpt(sc(195),sc(1),ips,dir,usp,vsp,s(1,4))
        if (sc(4)*s(1,4)+sc(5)*s(2,4)+sc(6)*s(3,4).lt.0.) dir = -dir
      endif

      numpts = iabs(ifl(91))
c
c... init pass params and col pointers, etc
c... also cos cutter taper angle
c
      ntk = 0
      lnk = 1
      kdntcu = ifl(42)

      dpmax = sc(54)
      dpmin = 0.1*sc(27)
      dpmin = min2_r4 (dpmin, 0.1*dpmax)
      dpfac = 1.

      ifl(359) = 0
      sharp = 0

      if(sbe.eq.0.) cbe = 1.
c
c...Circular interpolation is active
c...and drive surface is a cylinder
c...Get cylinder attributes
c
      icck = .false.
      if (ifl(95) .eq. 1) then
        if (dstype .eq. SURF) then
          icck = .true.
          call gtprimt(nclkey,0,primtyp,primdat)
cc          plx(1) = s(1,1)
cc          plx(2) = s(2,1)
cc          plx(3) = s(3,1)
cc          plx(4) = s(4,1)
          call plnvpt (primdat(4),sc(1),plx,ierr)
          if (ierr .ne. 0) then
            ifl(95) = 0
            icck = .false.
          else
            call plnint (primdat,primdat(4),plx,ccen,ierr)
            cvec(1) = primdat(4)
            cvec(2) = primdat(5)
            cvec(3) = primdat(6)
            if (ierr .ne. 0 .or. (ifl(246) .ne. 0 .and. lv92)) then
              ifl(95) = 0
              icck = .false.
            else
              cdis =  f_dist(sc(1),ccen)
            endif
          endif
        else
            cvec(1) = d(56)
            cvec(2) = d(57)
            cvec(3) = d(58)
        endif
      endif
c
c... call csrel to load s(,3) tbl   calc eff rad ctr to te,
c... evaluate starting distance to CS
c
      isrf = 3
      ia   = 3
      csu1 = t(25,ia)
      csv1 = t(26,ia)
      h = t(27,ia)
      x = t(28,ia)
      y = t(29,ia)
      z = t(30,ia)
      itfl = -2
      iptk = -1
      ifl2 = ifl(2)
      ifl83 = ifl(83)
      jd403 = jd(403)
      call csrel

      hcsdis = t(12,ia)
      csdis0 = abs(hcsdis)
      if (ifl(2) .gt. 0) then
        ifl(2) = ifl2
        csdis0 = 1000000.
      endif
c
c... if cs is circle and tool inside same, reduce dpmax   15-mar-83
c
      if (jd(401).eq.CIRCLE) then
c
c... call csrel to load s(,3) tbl   calc eff rad ctr to te
c
         dpm = dpmax

         if (ifl(2) .eq. 466) go to 999

         reff=s(1,3)*(sc(1)-d(103))+s(2,3)*(sc(2)-d(104))+s(3,3)
     *      *(sc(3)-d(105))
c
c... if te inside circle cs, reduce dpmax to 1/5 radius
c
         if (abs(reff).lt.d(109)) dpm = d(109)/5.

         if (dpm.le.sc(54)) then
            if(dpm.lt.dpmin) dpm=dpmin
            dpmax=dpm
         endif
      else
         t(25,ia) = csu1
         t(26,ia) = csv1
         t(27,ia) = h
         t(28,ia) = x
         t(29,ia) = y
         t(30,ia) = z
         ifl(83) = ifl83
         jd(403) = jd403
      endif

      dpmax1 = 0.5*sc(28)
      dpmax1 = max2_r4 (dpmax1, dpmin)
c
c... chgs for ta angular chg dp control   26-jan-83
c
      angmax = sc(80)
      if (angmax.lt..01) angmax = .01
c
c... conv angle to sin for all later use.
c
      angmax = sin(angmax/57.29578)
      angdp  = dpmax

      angfct = .9
      if (lv81) angfct = 1.0

      isubcl = 5
      if (lmintf(1)) isubcl = 6
      trgt   = ifl(21)
      ddel   = tool(2)

c
c... unset the withold o/p and endpas re-try flags   8-3-82
c
      ifl(29) = 0
      ifl(28) = 0
c
c... save the dis travelled. jingrong 11/09/99.
c
      sc160 = sc(160)
      sc176 = sc(176)
c
c... save the current clfile pointers in case of error
c
      isv1 = istat(1)
      call ncl_setptr(imotp,isv2)
      call ncl_zroptr(iept)
c
c... min/max toler range for crwnh and jog tol
c... ( accept liberally at this time )
c
      EPS = tol*.001
      atol = tol*.5
      btol = tol*1.4
      dtol = sc(167)*2.
      if (lv81) dtol = tol*2.
      if (.not.lv93 .and. ifl(280).eq.0) then
        a = 0.002
        if (ifl(264).eq.1 .or. ifl(362).eq.1) a = 0.0508
        if (sc(167) .ge. 2*a) then
          dtol = sc(167)
        else if (sc(167) .ge. a) then
          dtol = 2*a
        endif
      endif
      dtolsq = 4.*tol**2
      qtolsq = 900.*dtolsq
c
c...set probable iflat flg if ps,ds are in ln,pl class
c
      iflat = 0
      if(jd(1).lt.CIRCLE .and. jd(201).lt.CIRCLE) iflat=1
c
c... set slowdn tim1 (if indicated)              4-jun-86
c
      isltim = 0
c
c...vp 1.14.94 old FR/AT if version < 8.209
c... if circ interp or dntcut, set to tim2                  5-feb-86
c
      if (lv82) then
         if(ifl(213).ne.0) isltim=1
         if(ifl(95).eq.1 .or. ifl(42).ne.0) isltim=2
      end if
c
c...   TA/THRU,pt
c
      if (mod .eq. 13) call conv4_8 (tool(18),tapt,3)
c
c...   TA/THRU,cv
c
      if (mod .eq. 14) then
        call gtdesc (tool(19), keytcv, nwds, ietype)
        isf = 4
        call evstup (keytcv, isf)
c
c...   Use closest end of curve as start point.
c
        ucv = ONE
        call uevcvt (ucv, isf, izro, tapt, vtmp, ifl(2))
        if (ifl(2) .eq. 466) go to 999
        call ptlnds (tapt, sc, totdis)
        ucv = ZERO
        call uevcvt (ucv, isf, izro, tapt, vtmp, ifl(2))
        if (ifl(2) .eq. 466) go to 999
        call ptlnds (tapt, sc, tacvds)
        lcvrev = totdis.lt.tacvds
c
c...   For 3 sf GO get closest end of curve for TA/THRU,pt
c
        if (ifl(280).eq.1) then
          mod = 13
          if (lcvrev) then
            ucv = ONE
            call uevcvt (ucv, isf, izro, tapt, vtmp, ifl(2))
            if (ifl(2) .eq. 466) go to 999
          endif
        else
          knttcv = 0
          totdis = ZERO
          tacvds = tool(18)
          if (tacvds.le.tol) then
            ifl(80) = 2
          else
            ifl(80) = 1
          endif
        endif
      endif
c
c... TA/INTERPOL
c
      if (mod .eq. 15) then
        knttcv = 0
        totdis = ZERO
        tacvds = ZERO
        call conv4_8(t(4,3), tavst, 3)
        ifl(80) = 2
      endif
c
c..... smooth interpolation
c
      if ((mod.eq.TA_FAN .or. mod.eq.15) .and. fdeg.gt.1) then
        if (lv93) fdeg = 0
        if (rate .gt. 1.) rate = 1.
        if (fdeg .eq. 3) then
          rate1 = 3.*rate
          alp = 4.*rate1 - 8.
          bet = 6. - 2.*rate1
        else if (fdeg .gt. 3) then
          rate1 = rate*((fdeg-1.)**2-1.)/((fdeg-2.)**2-1.)
          bet = ((fdeg - 1.)*rate1 - fdeg)/2.
          alp = rate1 - bet - 1.
        endif
      endif

      if (ifl(80).eq.0 .and. mod.eq.TA_FAN .and. fdeg.gt.0) ifl(80) = 1
c
c... if fan pass1, set dntcut and save fwd
c
      if (ifl(80).ne.0) then
         ifl(42) = 2
         call vctovc4 (t(7,3),fwd0)
      endif

      lijk = mod.eq.1 .and. ifl(280).eq.1 .and. psmult .and.
     x       .not.lv93 .and. tool(1).ge.10*tol
c
c... init dp and itfl,iptk, jptk for tdsply
c
20    continue

      ifl(146) = 0
c
c..... ifl(147) is used in psrel: if psmult and no gouge checking we initialize
c..... the tool look vector t(16,ia) as a forward; this happens only the first
c..... time in psrel - for Dassault NCL117 (motion 3)
c
      if (.not.lv91 .and. ifl(6).eq.0) ifl(147) = 1
      dp = 0.
      t(10,2) = 0.

      itfl = -2
      iptk = 0
      jptk = 0
      ijk = 0
      gougup = .false.

      ishrp = -1
      inidcv = 0
c
c... off the logical pass stpt goto flag
c
      ifl(90)=0
c
c... col headers
c
      ia = 1
      ib = 2
      ic = 3
      lbfst = .true.
      lifted = 0
c
c... basic step cycle startpoint
c... set extrap coefs.
c
100   idpk   = 0

      if (ishrp.eq.2 .and. klik.eq.1) ishrp = 1

      ireduc = 0
      irejog = 0
      idsjog = 0
      if (DEBUGX .eq. 1) then
          call ctob ("100 loop...",dout)
          call nclxostr(dout)
      endif
c
c... aak 19-jun-1998: go_slow flag is turned on for net PS if there are
c... surfaces besides the current PS near the tool; in this case,
c... try not to allow make too big steps; dpmax1 gives desirable
c... upper limit on the steps.
c
      if (ifl(289).eq.1 .and. lifted.eq.0) then
         t(1,ia) = t(1,ia) + tool(2)*t(4,ia)
         t(2,ia) = t(2,ia) + tool(2)*t(5,ia)
         t(3,ia) = t(3,ia) + tool(2)*t(6,ia)
         t(1,ib) = t(1,ib) + tool(2)*t(4,ib)
         t(2,ib) = t(2,ib) + tool(2)*t(5,ib)
         t(3,ib) = t(3,ib) + tool(2)*t(6,ib)
         t(1,ic) = t(1,ic) + tool(2)*t(4,ic)
         t(2,ic) = t(2,ic) + tool(2)*t(5,ic)
         t(3,ic) = t(3,ic) + tool(2)*t(6,ic)
         lifted = 1
      endif
      go_slow = .false.
c     tryd = .false.

110   continue
      
      if (DEBUGX .eq. 1) then
          call ctob ("Outer loop...",dout)
          call nclxostr(dout)
          write (dbuf,7002) 'ia',t(1,ia),t(2,ia),t(3,ia),t(4,ia),
     1                      t(5,ia),t(6,ia)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7002) 'ib',t(1,ib),t(2,ib),t(3,ib),t(4,ib),
     1                      t(5,ib),t(6,ib)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7002) 'ic',t(1,ic),t(2,ic),t(3,ic),t(4,ic),
     1                      t(5,ic),t(6,ic)
 7002     format ('t(',a2,') = ',6f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
      ro = 0.
      if (t(10,ib).ge.1.e-3) ro = min2_r4 (t(10,ic)/t(10,ib),5.)

      if (ishrp .eq. 2) then
        ro = 0.
        if (dp .gt. 250.*tol) dp = 250.*tol
      endif

120   q = ro*.5
      irejdp = 0
      dst1=0.
      idst1=0
      gidmvd = .false.
c
c... for multips, create estimated point in forward direction to fix problem
c... projecting back to wrong sf
c
      if (psmult) q = 0.
      if (iptk.gt.0 .and. dp.lt.dpmin) dp = dpmin
      t(10,ia) = dp
c
c... extrap te,ta    ( if no error )
c
      if (ifl(2).gt.0) goto 999

      call avcplbvc4 (dp*(1.+q),t(7,ic),-dp*q,t(7,ib),vec)
      call vcplvc4 (t(1,ic),vec,t(1,ia))
      call vcmnvc4 (t(4,ic),t(4,ib),vec)
      if (sharp .eq. 1) ro = 0.
      call uvcplvc4 (t(4,ic),vec,t(4,ia),ro)
      if (DEBUGX .eq. 2) then
          write (dbuf,7017) t(7,ib),t(8,ib),t(9,ib)
 7017     format ('Mover-fwd(b) = ',3f12.5)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7023) t(7,ic),t(8,ic),t(9,ic)
 7023     format ('Mover-fwd(c) = ',3f12.5)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7018) vec
 7018     format ('Mover-vec = ',3f12.5)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7019) dp,q,ro
 7019     format ('Mover-dp,q,ro = ',3f12.5)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7020) t(1,ia),t(2,ia),t(3,ia),t(4,ia),t(5,ia),
     1                      t(6,ia)
 7020     format ('Mover-t(ia) = ',6f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7021) t(1,ib),t(2,ib),t(3,ib),t(4,ib),t(5,ib),
     1                      t(6,ib)
 7021     format ('Mover-t(ib) = ',6f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7022) t(1,ic),t(2,ic),t(3,ic),t(4,ic),t(5,ic),
     1                      t(6,ic)
 7022     format ('Mover-t(ic) = ',6f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

      if (
     *     (mod.ge.2 .or. lblade)
     *     .or.
     *     (psmult. and. .not.lv90 .and.
     *         f_dot4 (t(7,ia),t(7,ic)).lt.PSMULT_FWD_FUZZ)
     *   ) then
         call vcmnvc4 (t(7,ic),t(7,ib),vec)
         call uvcplvc4 (t(7,ic),vec,t(7,ia),ro)
      endif

      if (.not.lv84) call unitizevc4 (t(4,ia))
c
c... do PS,DS srf pn's
c
c... ps
c
210   continue

      if (DEBUGX .eq. 1) then
          call ctob ("Inner loop...",dout)
          call nclxostr(dout)
          write (dbuf,7002) 'ia',t(1,ia),t(2,ia),t(3,ia),t(4,ia),
     1                      t(5,ia),t(6,ia)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7002) 'ib',t(1,ib),t(2,ib),t(3,ib),t(4,ib),
     1                      t(5,ib),t(6,ib)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7002) 'ic',t(1,ic),t(2,ic),t(3,ic),t(4,ic),
     1                      t(5,ic),t(6,ic)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
      if (ifl(289).eq.1 .and. lifted.eq.1) then
         t(1,ia) = t(1,ia) - tool(2)*t(4,ia)
         t(2,ia) = t(2,ia) - tool(2)*t(5,ia)
         t(3,ia) = t(3,ia) - tool(2)*t(6,ia)
         t(1,ib) = t(1,ib) - tool(2)*t(4,ib)
         t(2,ib) = t(2,ib) - tool(2)*t(5,ib)
         t(3,ib) = t(3,ib) - tool(2)*t(6,ib)
         t(1,ic) = t(1,ic) - tool(2)*t(4,ic)
         t(2,ic) = t(2,ic) - tool(2)*t(5,ic)
         t(3,ic) = t(3,ic) - tool(2)*t(6,ic)
         lifted = 0
      endif
      if (lsecps .and. mod.ne.TA_FAN) then
        pte(1) = t(1,ia)
        pte(2) = t(2,ia)
        pte(3) = t(3,ia)
        call sfpt(sc(195),pte(1),ips,dir,usp,vsp,s(1,4))
      endif
      isrf = 1
      if (jd(1).eq.PLANE) goto 220
c
c... extrap u,v,hps  (unless rejog)
c
      if (irejog.eq.0) then
         t(13,ia)=t(13,ic)+ro*(t(13,ic)-t(13,ib))
         t(14,ia)=t(14,ic)+ro*(t(14,ic)-t(14,ib))
c
c...jingrong 12/16/98 reassign initial uv value for FAN pass 2
c
      if (.not.lv90 .and. mod.eq.4 .and. ifl(80).eq.0
     *    .and. iptk.eq.0) then
         t(13,ia) = psu0
         t(14,ia) = psv0
      endif

c
c...extrap cpt if indicated
c
         if (ifl(56).ne.0)
     *      call avcplbvc4 (1.+ro,t(16,ic),-ro,t(16,ib),t(16,ia))
      endif

      call psrel
      if (ifl(147) .gt. 0) ifl(147) = 0

      if (sharp .eq. 0) sharp = ifl(359)
      if (ifl(2).gt.0) goto 999
c
c      if (reset_dp .and. tool(1).gt.2.*sc(27)
c     *    .and. dpmax1.lt.dpmax ) go_slow = .true.
c
      if (reset_dp .and. sc(28).gt.2.*sc(27)
     *    .and. dpmax1.lt.dpmax ) go_slow = .true.
c
c..... DS
c
220   isrf=2

      if (jd(201).ne.PLANE) then
c
c...extrap u,v  and h   ( unless rejog )
c
         if (irejog.eq.0 .and. inidcv.eq.0) then
           call avcplbvc4 (1.+ro,t(19,ic),-ro,t(19,ib),t(19,ia))
c
c......Don't allow the look point height to be under the cutter
c......FSR 61242  Bobby  - 6/25/09
c
           if (.not. lv96 .and. t(21,ia) .lt. 0.) t(21,ia) = t(21,ic)
           if (ishrp.eq.1 .and.
     x       fdshrp*(t(19,ic)-ushrp).lt.-0.0001 .and.
     x       fdshrp*(t(19,ia)-ushrp).gt.0.0001) then
               t(19,ia) = ushrp + fdshrp*0.0001
           endif
         endif
c
c...jingrong 12/16/98 reassign initial uv value for FAN pass 2
c...And the same for the look point height (h) - Bobby - 6/25/09
c
         if (.not.lv90 .and. mod.eq.4 .and. ifl(80).eq.0
     *                 .and. iptk.eq.0 .and. inidcv.eq.0) then
            t(19,ia) = dsu0
            t(20,ia) = dsv0
            if (.not. lv96) t(21,ia) = hds0
         endif

         icnt = 0
225      continue
         call dsrelx (ldsgck,tol)

         if (.not.lv94 .and. ifl(280).eq.0 .and. itfl.eq.-2 .and.
     x       jd(201).eq.CURVE .and. (ifl(2).eq.129 .or. ifl(2).eq.139)
     x       .and. icnt .lt. 100) then
           ifl(2) = 0
           call ucvini(jret1)
           icnt = icnt + 1
           if (jret1.eq.1) goto 225
         endif

         if (ifl(2).eq.139 .and. ishrp.gt.0 .and. dp.gt.dpmin) then
           ifl(2) = 0
           goto 230
         endif

         if (ifl(2).gt.0) goto 999
      endif
c
c... fwd is crossf ps(up) and ds(rgt)
c
      fx=s(3,2)*s(2,1)-s(2,2)*s(3,1)
      fy=s(1,2)*s(3,1)-s(3,2)*s(1,1)
      fz=s(2,2)*s(1,1)-s(1,2)*s(2,1)
      sec=sqrt(fx**2+fy**2+fz**2)
c
c... error. fwd sense indefinite.
c
      if (sec.le.1.e-3) then
         ifl(2) = 124
         goto 999
      endif

      dot1 = fx*t(7,ic)+fy*t(8,ic)+fz*t(9,ic)
      if (dot1.gt.0.) goto 320
c
c... fwd sense reversal
c
230   if (dp.gt.dpmin) then
         dp = max2_r4 (0.25*dp, dpmin)
         ro = 0.25 * ro
         ireduc = 1
         irejog = 0
         goto 120
      endif
c
c... error. fwd sense reversal.
c... aak 17-jul-98: allow vertical movements for net PS
c
      if (ifl(91).lt.0 .or.
     *    (psmult. and. ireduc.gt.0 .and. dot1.gt.-PSMULT_FWD_FUZZ)
     *   ) then
         ifl(2) = -125
      else
         if (itry.eq.2 .and. iptk.lt.1) goto 321

         if (.not.lv94 .and. ifl(280).eq.0 .and. itfl.eq.-2 .and.
     x                         inidcv.eq.0 .and. jd(201).eq.CURVE) then
           call ucvini(jret1)
           inidcv = 1
           if (jret1 .eq. 1) goto 220
         endif

         ifl(2) = 125
         ifl(130) = 0
         call conv4_8 (t(1,ia),tb,9)
cc         call plotm (tb,.false.)
         write(errcom,1020)t(1,ia),t(2,ia),t(3,ia)
         goto 999
      endif
c
c... fwd ok. add to col(ia)
c
320   t(7,ia)=fx/sec
      t(8,ia)=fy/sec
      t(9,ia)=fz/sec
c
c..... recalculate fx,fy,fz vector if there is a secondary PS, and not FAN
c
321   if (ips .eq. 4) then
        fx=s(3,2)*s(2,ips)-s(2,2)*s(3,ips)
        fy=s(1,2)*s(3,ips)-s(3,2)*s(1,ips)
        fz=s(2,2)*s(1,ips)-s(1,2)*s(2,ips)
        sec=sqrt(fx**2+fy**2+fz**2)
c
c... error. tlaxis fwd sense indefinite.
c
        if (sec.le.1.e-3) then
           ifl(2) = 123
           goto 999
        endif
      endif

      if (lblade) then

        if (lbfst) then
          ifl(104) = 1
          tool(12) = 0.0
          tool(13) = asc(307)
          tool(14) = 0.0
          tool(15) = 0.0
          tool(16) = -asc(305)
          tool(17) = sqrt(1.0-tool(16)**2)
        endif

        if (mod .eq. 0 .or. lbfst) then
          call conv8_8 (sc,tb,6)
          call conv4_8 (t(7,ia),tb(7),3)
          dis = tool(13)
          if (.not. lbfst) tool(13) = 0.0
          tool(16) = -tool(16)
          call modfy (tb,tb)
          tool(13) = dis
          tool(16) = -tool(16)
          if (lbfst) then
            tool(13) = -dis*tool(17)
            tool(14) = dis*tool(16)
            call conv8_4 (tb, t(1,1),6)
            call conv8_4 (tb, t(1,3),6)

            if (mod.eq.4) then
              alx = t(1,3)-t(4,3)*1.e6
              aly = t(2,3)-t(5,3)*1.e6
              alz = t(3,3)-t(6,3)*1.e6

              do 350 j=1,3
                 t(28,j) = alx
                 t(29,j) = aly
                 t(30,j) = alz
350           continue

            endif
            lbfst = .false.
          endif

          call conv8_4 (tb(4),t(4,ia),3)
        endif
      endif
c
c***********************************  set tool axis vector
c
      if (mod .eq. 0) goto 500
      if (mod .eq. 1) goto 410
      if (mod .eq. 2) goto 420
      if (mod .eq. 3) goto 430
      if (mod .eq. 4) goto 440
      if (mod .eq. 5) goto 450
      if (mod .eq. 6) goto 460
      if (mod .eq. 7) goto 410
      if (mod .ge.10 .and. mod.le.12) goto 420
      if (mod .eq. 13) goto 475
      if (mod .eq. 14) goto 470
      if (mod .eq. 15) goto 480
c
      ifl(2) = 126
      goto 99999
c
c...TA/NORMAL,PS        ta=psnrm
c
410   call vctovc4 (s(1,ips), t(4,ia))
      if (mod.eq.7) goto 490
      goto 500
c
c... TA/ATANGL,alf,PS     ta=psnrm*cos+fwd*sin
c
420   CO=TOOL(9)
      SI=TOOL(8)
      if(mod.gt.10)call tltang(si,co,t(1,ia),t(16,ia),t(13,ia),t(14,ia))
      T(4,IA)=S(1,ips)*CO+T(7,IA)*SI
      T(5,IA)=S(2,ips)*CO+T(8,IA)*SI
      T(6,IA)=S(3,ips)*CO+T(9,IA)*SI
      IF (MOD.EQ.10.OR.MOD.EQ.12) GOTO 490
      GOTO 500
c
c... TA/TANTO,ds,hin      ta=crossf(rln,fwd)
c... rln is +/- tanbeta (cutter angl) from dsrgt
c             'up'
430   xu=( s(2,2)*fz-s(3,2)*fy )/sec
      yu=( s(3,2)*fx-s(1,2)*fz )/sec
      zu=( s(1,2)*fy-s(2,2)*fx )/sec
      tbe=-sbe/cbe*trgt
c
c......if barrel tool, alpha pertains          22-DEC-87
c......Sal = Sine of flat angle
c......Cal = Cosine
c......Tbe = Tangent (with sign depending on Tllft or Tlrgt)
c
      if (ifl(282).ne.0) then
c
c......If 'hin' is on barrel (not on flat angle)
c......Then calculate new flat angle so that
c......It is tangent to barrel at 'hin'
c......Bobby - 02/18/16
c
         call barcta (t(21,ia),sal,bcuta)
         cal = sqrt(1.-sal*sal)
         tbe = -sal/cal*trgt
      endif

      rx=s(1,2)+tbe*xu
      ry=s(2,2)+tbe*yu
      rz=s(3,2)+tbe*zu
      xu=ry*fz-rz*fy
      yu=rz*fx-rx*fz
      zu=rx*fy-ry*fx
      sec=sqrt(xu**2+yu**2+zu**2)
      i = ic
      if (sc(169).lt.8.20799d0) i = ia
      if (xu*t(4,i)+yu*t(5,i)+zu*t(6,i).lt.0.) sec=-sec
      if (abs(sec).lt.1.e-4) goto 500
      t(4,ia)=xu/sec
      t(5,ia)=yu/sec
      t(6,ia)=zu/sec
      if (DEBUGX .eq. 1) then
          write (dbuf,7006) t(4,ia),t(5,ia),t(6,ia)
 7006     format ('Dstan-tlaxis = ',3f12.5)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
      goto 500
c     ta/fan          24-jan-83
440   continue
      if (lcvgid .and. irejdp.gt.2 .and. gdatcr .and. gidmvd) goto 540
      if (ifl(289).eq.1 .and. lifted.eq.0) then
         t(1,ia) = t(1,ia) + tool(2)*t(4,ia)
         t(2,ia) = t(2,ia) + tool(2)*t(5,ia)
         t(3,ia) = t(3,ia) + tool(2)*t(6,ia)
         t(1,ib) = t(1,ib) + tool(2)*t(4,ib)
         t(2,ib) = t(2,ib) + tool(2)*t(5,ib)
         t(3,ib) = t(3,ib) + tool(2)*t(6,ib)
         t(1,ic) = t(1,ic) + tool(2)*t(4,ic)
         t(2,ic) = t(2,ic) + tool(2)*t(5,ic)
         t(3,ic) = t(3,ic) + tool(2)*t(6,ic)
         lifted = 1
      endif
      alx=t(1,ia)-t(28,ic)
      aly=t(2,ia)-t(29,ic)
      alz=t(3,ia)-t(30,ic)

      if (fanint .eq. 2) then
        dis = f_dist4 (t(1,ia),t(1,ic))
        ucv = (sc(160) + dis - sc160)/fuldis
        if (ucv.gt.0. .and. ucv.lt.1.) then
          vec(1) = sc4 - sc(4)
          vec(2) = sc5 - sc(5)
          vec(3) = sc6 - sc(6)
          q = ucv
          if (q .gt. 0.5) q = 1.-q
          if (fdeg .eq. 1) then
            a = q
          else if (fdeg .eq. 2) then
            a = 2.*(q**2)
          else if (fdeg .eq. 3) then
            a = alp*(q**3) + bet*(q**2)
          else
            q = 2.*q - 1.
            p = exp((fdeg-2)*alog(-q))
            b = alp*(q**2)*p + bet*p + rate1*q
            a = (1. + b)/2.
          endif
          if (ucv .le. 0.5) then
            alx = sc(4) + a*vec(1)
            aly = sc(5) + a*vec(2)
            alz = sc(6) + a*vec(3)
          else
            alx = sc4 - a*vec(1)
            aly = sc5 - a*vec(2)
            alz = sc6 - a*vec(3)
          endif
          goto 445
        endif
      endif
c                  if apxpt all zero, set alx,y,z = current ta  3-sep-86
      if (t(28,ic)**2+t(29,ic)**2+t(30,ic)**2.gt..001) goto 445
      alx=t(4,ia)
      aly=t(5,ia)
      alz=t(6,ia)
445   fx=aly*s(3,2)-alz*s(2,2)
      fy=alz*s(1,2)-alx*s(3,2)
      fz=alx*s(2,2)-aly*s(1,2)
      sec=sqrt(fx**2+fy**2+fz**2)
c     a-line may not oppose ijk
      if (alx*t(4,ia)+aly*t(5,ia)+alz*t(6,ia).lt.0.) sec=-sec
      if (abs(sec).gt.0.) goto 430
c               if sec zero, drop thru
      goto 500
c
c... TA/TANTO,DS,hin,PERPTO,v1
c
450   fx=tool(18)
      fy=tool(19)
      fz=tool(20)
      sisq=(fx*s(1,2)+fy*s(2,2)+fz*s(3,2))**2
      if (sisq.gt.1.) sisq = 1.
      sec=sqrt(1-sisq)
      if (fx*t(7,ia)+fy*t(8,ia)+fz*t(9,ia).lt.0.) sec=-sec
      if (abs(sec).gt.0.) goto 430
c              sec=0  drop thru
      goto 500
c
c... TA/TANTO,DS,hin,PARELM
c... v-vec is in t(22-24,ia) per ipatch. go fan route
c
460   alx=t(22,ia)
      aly=t(23,ia)
      alz=t(24,ia)
      goto 445
c
c... TA/THRU,cv1
c
470   continue

      if(lv84) then
         tdis = totdis + dp
      else if (iptk .eq. 0) then
         tdis = ZERO
      else
         tdis = totdis + f_dist4 (t(1,ia), t(1,ic))
      endif

      if (ifl(80).eq.2) then
        ucv = 0.0
        if (iptk.gt.0) ucv = tdis/(abs(t(12,ic))+totdis)
      else
        ucv = tdis/tacvds
      endif

      if (lcvrev) ucv = 1. - ucv
      if (ucv.lt.0.0) ucv = 0.0
      if (ucv.gt.1.0) ucv = 1.0
      call uevcvt (ucv, isf, izro, tapt, vtmp, ifl(2))
      if (ifl(2) .eq. 466) go to 999
c
c... TA/THRU,pt1
c
475   ti=tapt(1)-t(1,ia)
      tj=tapt(2)-t(2,ia)
      tk=tapt(3)-t(3,ia)
      sec=sqrt(ti**2+tj**2+tk**2)
      if(sec.lt.1.e-4)goto 500
      if(ti*t(4,ia)+tj*t(5,ia)+tk*t(6,ia).lt.0.)sec=-sec
      t(4,ia)=ti/sec
      t(5,ia)=tj/sec
      t(6,ia)=tk/sec
      goto 500
c
c... TA/INTERP
c
480   continue
      if (iptk.eq.0) then
        call conv8_4 (tavst,t(4,ia),3)
      else if (itfl.gt.0 .and. itfl.ne.11) then
        call conv8_4 (tavend,t(4,ia),3)
      else
        tdis = totdis + f_dist4 (t(1,ia),t(1,ic))
        if (ifl(80).eq.2) then
          ucv = tdis/(abs(t(12,ic))+totdis)
        else
          ucv = tdis/tacvds
        endif
        if (ucv.lt.ZERO) ucv = ZERO
        if (ucv.gt.ONE)  ucv = ONE
        vtmp(1) = tavend(1)-tavst(1)
        vtmp(2) = tavend(2)-tavst(2)
        vtmp(3) = tavend(3)-tavst(3)
        if (fdeg .gt. 1) then
          q = ucv
          if (q .gt. 0.5) q = 1.-q
          if (fdeg .eq. 2) then
            a = 2.*(q**2)
          else if (fdeg .eq. 3) then
            a = alp*(q**3) + bet*(q**2)
          else
            q = 2.*q - 1.
            p = exp((fdeg-2)*alog(-q))
            b = alp*(q**2)*p + bet*p + rate1*q
            a = (1. + b)/2.
          endif
          if (ucv .le. 0.5) then
            ti = tavst(1) + a*vtmp(1)
            tj = tavst(2) + a*vtmp(2)
            tk = tavst(3) + a*vtmp(3)
          else
            ti = tavend(1) - a*vtmp(1)
            tj = tavend(2) - a*vtmp(2)
            tk = tavend(3) - a*vtmp(3)
          endif
        else
          if (knttcv.ge.1 .and. ucv .ge. ONE) then
            ucv = HALF*(totdis/tacvds + ONE)
          endif
          ti = tavst(1) + vtmp(1)*ucv
          tj = tavst(2) + vtmp(2)*ucv
          tk = tavst(3) + vtmp(3)*ucv
        endif
        sec=sqrt(ti**2+tj**2+tk**2)
        if(sec.lt.1.e-4)goto 500
        t(4,ia)=ti/sec
        t(5,ia)=tj/sec
        t(6,ia)=tk/sec
      endif

      goto 500
c
c... TA/...,PERPTO,v1
c... do rgt vec
c
490   XR=TOOL(19)*T(6,IA)-TOOL(20)*T(5,IA)
      YR=TOOL(20)*T(4,IA)-TOOL(18)*T(6,IA)
      ZR=TOOL(18)*T(5,IA)-TOOL(19)*T(4,IA)
c
c... up vec is  perpto v1 and rgt
c
      fx=yr*tool(20)-zr*tool(19)
      fy=zr*tool(18)-xr*tool(20)
      fz=xr*tool(19)-yr*tool(18)
      sec=sqrt(fx**2+fy**2+fz**2)
c
c... if sec zero, drop thru with old tlaxis
c
      if (sec.lt.1.e-4) goto 500
      t(4,ia)=fx/sec
      t(5,ia)=fy/sec
      t(6,ia)=fz/sec
500   continue

      if (DEBUGX .eq. 1 .and. mocom .and. lmdnow .and. (sc(81) .ne. 0.0
     1     .or. sc(82) .ne. 0.0 .or. sc(83) .ne. 0.0)) then
          call ctob ("Middle motion...",dout)
          call nclxostr(dout)
          
          write (dbuf,8001) ifl(298)
          call ctob (dbuf,dout)
 8001     format ('ifl(298)= ',i2)
          call ctob (dbuf,dout)
          call nclxostr(dout)
             
          write (dbuf,8003) 'ia',t(1,ia),t(2,ia),t(3,ia),t(4,ia),
     1                      t(5,ia),t(6,ia)
          call ctob (dbuf,dout)
 8003     format ('t(',a2,') = ',6f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
 
c
c......Added fanning interploation of RIGHT/FWD angle during tool axis combine
c......approach motion and leave motion. The middle motion keep the tilt angle
c......FSR60198(Change note NCLj_042810)
c 
      if (mocom .and. .not.lmdnow .and. (sc(81) .ne. 0.0 .or. 
     x      sc(82) .ne. 0.0 .or. sc(83).ne. 0.0)) then
          dis0 = (t(1,ia)-sc(1))**2 +  (t(2,ia)-sc(2))**2 +
     1          (t(3,ia)-sc(3))**2
          dis0 = sqrt(dis0)
     
          if (DEBUGX .eq. 1) then
            call ctob ("lmdnow...",dout)
            call nclxostr(dout)
            
            write (dbuf,8031) sc160,sc(160),dis0,dis1,csdis0
 8031       format ('sc160=',f8.4,' sc(160)=',f8.4,'dis0=',f8.4,
     1            ' dis1=',f8.4,' csdis0=',f8.4)
            call ctob (dbuf,dout)
            call nclxostr(dout)
          
            write (dbuf,8002) 'ia',t(1,ia),t(2,ia),t(3,ia),t(4,ia),
     1                      t(5,ia),t(6,ia)
            call ctob (dbuf,dout)
 8002       format ('t(',a2,') = ',6f11.4)
            call ctob (dbuf,dout)
            call nclxostr(dout)
          endif

          csdt = 0.0
          if (lapmov .and. ifl(352) .ne. 1) then
            csdt = dis0/csdis0
          else
            if (dis1 .eq. 0.) then
              csdt = 0.
            else
              csdt = dis0/dis1
            endif
          endif
          if ((lapmov .and. csdt .gt. 0.95) .or.
     1        (.not. lapmov .and. csdt .gt. 0.99)) then
              csdt = 1.0
          endif
          if (lapmov) then
             csdt = 1 - csdt
          endif
          
          if (DEBUGX .eq. 1) then
            write (dbuf,8032) csdt
 8032       format ('csdt =',f11.4)
            call ctob (dbuf,dout)
            call nclxostr(dout)
          endif
c
c...... The RIGHT angle(sc20) and FWD angle(sc21)      
c   
          sc20 = datan(sc(81)/sc(83))*RADIAN
          sc21 = datan(sc(82)/sc(83))*RADIAN
         
          if (.not. lapmov .and. csdt .eq. 1.0 .and.
     x        abs(sc21) .lt.0.01) then
            lmdnow = .TRUE.
            goto 530
          endif
 
          cosa=dcos(sc20/RADIAN*csdt)
          sina=dsin(sc20/RADIAN*csdt)
          cosb=dcos(sc21/RADIAN*csdt)
          sinb=dsin(sc21/RADIAN*csdt)
          dcx=sina*cosb
          dcy=sinb*cosa
          dcz=cosb*cosa
          sec=sqrt(dcx**2+dcy**2+dcz**2)
          sc81=dcx/sec
          sc82=dcy/sec
          sc83=dcz/sec
               
          xr=t(8,ia)*t(6,ia)-t(9,ia)*t(5,ia)
          yr=t(9,ia)*t(4,ia)-t(7,ia)*t(6,ia)
          zr=t(7,ia)*t(5,ia)-t(8,ia)*t(4,ia)
          sec = sqrt(xr**2+yr**2+zr**2)
          if (sec.eq.0.) sec = 1.0
          xr = xr/sec
          yr = yr/sec
          zr = zr/sec
          ti = xr*sc81+t(7,ia)*sc82+t(4,ia)*sc83
          tj = yr*sc81+t(8,ia)*sc82+t(5,ia)*sc83
          tk = zr*sc81+t(9,ia)*sc82+t(6,ia)*sc83
          sec=sqrt(ti**2+tj**2+tk**2)
          if(ti*t(4,ia)+tj*t(5,ia)+tk*t(6,ia).lt.0.)sec=-sec
          if (sec.ne.0.) then
            t(4,ia)=ti/sec
            t(5,ia)=tj/sec
            t(6,ia)=tk/sec
          endif
      endif
     
530   if (lmdnow) then
        xr=t(8,ia)*t(6,ia)-t(9,ia)*t(5,ia)
        yr=t(9,ia)*t(4,ia)-t(7,ia)*t(6,ia)
        zr=t(7,ia)*t(5,ia)-t(8,ia)*t(4,ia)
        sec = sqrt(xr**2+yr**2+zr**2)
        if (sec.eq.0.) sec = 1.0
        xr = xr/sec
        yr = yr/sec
        zr = zr/sec
        ti = xr*sc(81)+t(7,ia)*sc(82)+t(4,ia)*sc(83)
        tj = yr*sc(81)+t(8,ia)*sc(82)+t(5,ia)*sc(83)
        tk = zr*sc(81)+t(9,ia)*sc(82)+t(6,ia)*sc(83)
        sec=sqrt(ti**2+tj**2+tk**2)
        if(ti*t(4,ia)+tj*t(5,ia)+tk*t(6,ia).lt.0.)sec=-sec
        if (sec.ne.0.) then
          t(4,ia)=ti/sec
          t(5,ia)=tj/sec
          t(6,ia)=tk/sec
        endif
      endif
c
c ... ldschk = true iff DS gougck is on
c
c..... Eduard 07/14/2000: do not check the guide curve until the tool end
c..... is close to the drive surface. CATIA NCL186
c
540   continue
      if ((lcvgid .and. (irejog.gt.0 .or. iptk.gt.0 .or. lv91))
     *    .or. (ldschk .and. dstype.eq.SURF)) then
          if (lcvgid .and. irejdp.eq.0 .and. .not.lv93) goto 550
          if (irejdp.eq.1.and.idst1.eq.0) then
            call conv4_4 (t(1,ia),pte1,3)
            call conv4_4 (t(4,ia),vta1,3)
          endif
          call cvgide
          if (ifl(2) .gt. 0) goto 99998
      endif
550   continue
c
c... TA/...,PERPTO,v1,LAST
c
      if (prplst) then
          XR=TOOL(19)*T(6,IA)-TOOL(20)*T(5,IA)
          YR=TOOL(20)*T(4,IA)-TOOL(18)*T(6,IA)
          ZR=TOOL(18)*T(5,IA)-TOOL(19)*T(4,IA)
c
c... up vec is  perpto v1 and rgt
c
          fx=yr*tool(20)-zr*tool(19)
          fy=zr*tool(18)-xr*tool(20)
          fz=xr*tool(19)-yr*tool(18)
          sec=sqrt(fx**2+fy**2+fz**2)
c
c... if sec zero, drop thru with old tlaxis
c
          if (sec.ge.1.e-4) then
              t(4,ia)=fx/sec
              t(5,ia)=fy/sec
              t(6,ia)=fz/sec
          endif
      endif
c
c... nest tool to ps,ds
c
      pcon=s(4,1)+sc(23)
c
c...   For blade cutter, adjust pcon for rear contact
c
      if (lblade) then
        if (tool(13).gt.-tol) goto 510
        call conv4_8 (t(1,ia),tb,6)
        call modfy (tbn,tb)
        dis = (tb(1)-tbn(1))*s(1,1) + (tb(2)-tbn(2))*s(2,1) +
     *        (tb(3)-tbn(3))*s(3,1)
        if (dis.gt.0.0) pcon = pcon+2.0*dis
        goto 510
      endif
c
c... if ta/normps, no further offset reqd.
c
      if (mod.eq.1.and..not.lcvgid.and..not.ldschk.and..not.lmdnow
     *    .and..not.lsecps) goto 510
c
c... If tlonps, no further offset required.
c
      if (ifl(342).eq.1) goto 510

      pco = t(4,ia)*s(1,1)+t(5,ia)*s(2,1)+t(6,ia)*s(3,1)
      if (pco.gt.1.) pco=1.
      psi = sqrt(1.-pco**2)
      pcon = pcon+tool(2)*(1.-pco)+tool(6)*psi
c
c... if disk tool + edge contact, fix pcon          27-jun-84
c
      if (lnodisk .or. pco.ge.sbe) goto 510
      pcon = s(4,1)+sc(23)+psi*tool(1)/2.-pco*tool(3)
510   dcon = s(4,2)
c
c... if tlon, no ds offset reqd.
c
      if (ifl(21) .eq. 0) goto 520
c
c... this assumes rcorn contact (temp)
c
      dsi = (t(4,ia)*s(1,2)+t(5,ia)*s(2,2)+t(6,ia)*s(3,2))*trgt
      if (dsi.gt.1.) dsi=1.
      dco = sqrt (1.-dsi**2)
c
c... BARREL TOOL ADDITION                           15-DEC-87
c
      IF(IFL(282).EQ.0)GOTO 514
C          YES BARREL TOOL. DECIDE IF RCORN,RSIDE,TOP CONTACT
      IF(DSI.LE.ASC(305))GOTO 518
      IF(DSI.GE.SBE)GOTO 516
C          RSIDE CONTACT
      DCON=S(4,2)+TRGT*(SC(24)+ASC(307)+DCO*ASC(67)-DSI*ASC(68))
      GOTO 520
C          BRANCH ON TOP OR CORNER CONTACT
514   IF(DSI.LT.SBE-.001.OR.IFL(57).GT.2)GOTO 518
C           CORNER CONTACT
516   DCON=S(4,2)+TRGT*(TOOL(6)*DCO-TOOL(2)*DSI+DDEL+sc(24))
      GOTO 520
c           top contact
518   dcon=s(4,2)+trgt*(sc(24)+.5*dco*tool(1)-dsi*tool(3))

520   si = f_dot4 (s(1,1),s(1,2))
      if (ifl(289).eq.1 .and. lifted.eq.1) then
         t(1,ia) = t(1,ia) - tool(2)*t(4,ia)
         t(2,ia) = t(2,ia) - tool(2)*t(5,ia)
         t(3,ia) = t(3,ia) - tool(2)*t(6,ia)
         t(1,ib) = t(1,ib) - tool(2)*t(4,ib)
         t(2,ib) = t(2,ib) - tool(2)*t(5,ib)
         t(3,ib) = t(3,ib) - tool(2)*t(6,ib)
         t(1,ic) = t(1,ic) - tool(2)*t(4,ic)
         t(2,ic) = t(2,ic) - tool(2)*t(5,ic)
         t(3,ic) = t(3,ic) - tool(2)*t(6,ic)
         lifted = 0
      endif
      !if (pcon .le. 0.0) then
      !pcon = -pcon
      !endif
      p  = f_dot4 (s(1,1),t(1,ia)) - pcon
      q  = f_dot4 (s(1,2),t(1,ia)) - dcon

      if (gdatcr .and. irejdp.gt.2 .and. gidst .lt. -tol) then
        if (idst1.eq.0 .and. dst1.lt.0) then
          call conv4_4 (pte1,t(1,ia),3)
          call conv4_4 (vta1,t(4,ia),3)
          irejdp = 0
          idst1 = 1
        else
          q = q-gidst
        endif
      endif

      if (idst1.eq.1) q = q-dst1

      a  = (p-si*q)/(1.-si**2)
      b  = q - a*si
      absa = abs(a)
      absb = abs(b)
      a1 = a
      b1 = b
      if (irejog.gt.30 .and. .not.lv83) then
        a1 = a/2.0
        b1 = b/2.0
        if (irejog.gt.60) then
          a1 = a/4.0
          b1 = b/4.0
        endif
      endif

      do 522 i=1,3
522   t(i,ia)=t(i,ia)-a1*s(i,1)-b1*s(i,2)
c
c... check for large move (fix 1-time only)
c... lv92+, rejog at least once to get the correct ta.
c... jingrong 01/14/2000.
c
      irejdp = irejdp + 1
      if ( ((.not.lv91.and.irejog.eq.0) .or. abs(a).ge.dtol
     *     .or. abs(b).ge.dtol) .and.irejog.le.120) then
         irejog=irejog+1
c
c..... reproject on DS from the same point if DS is large and we are
c..... still far after dp has been reduced
c
      if (.not.lv91 .and. initds.eq.1 .and. irejog.ge.2 .and.
     *     ireduc.gt.0 .and. (absa.ge.5.*dtol.and.absa.gt.2.*dp .or.
     *      absb.ge.5.*dtol.and.absb.gt.2.*dp)) then
c..... the following line assures this is done no more than once per iptk
         initds = 2
         ro = 0.25 * ro
         irejog = 0
         goto 120
      endif

         goto 210
      endif
      if (lcvgid .and. irejdp.eq.1 .and. .not.lv93) goto 210
c
c..... MFGNC304. In a GO statement with a multiple part surface and
c..... 'ta/normal,PS' mode, when the tool is stuck in a 'bad spot', change
c..... the mode temporarily to 'ta/tanto,DS'
c
      if (irejog.ge.120 .and. lijk .and. ijk.eq.0 .and. itfl.eq.0
     x    .and. t(12,ic) .gt. 2.*tool(1)) then
        ijk = 1
        dijk = t(12,ic) - 0.4*tool(1)
        ifl(23) = 3
        goto 100
      endif
c
c... begpas controls init cycles
c
      if (itfl) 600,620,642
600   continue
      call begpas(jret)

      if (ifl(2).eq.-142 .and. ifl(6).eq.0 .and. .not.lv94 .and.
     x    jret.eq.0 .and. .not.gougup .and. .not.psmult .and.
     x    ifl(280).eq.0) then
        ifl(2) = 0
        ifl(6) = 3
        itfl = -2
        gougup = .true.
        goto 100
      else if (gougup .and. jret.lt.2) then
        gougup = .false.
        ifl(6) = 0
      endif
c
c... jret= ok,err,redo
c
      if (jret-1) 615,999,610
610   irejog = 1

      if (.not.lv94 .and. ishrp.eq.0) then
        call ucvini(jret1)
        if (jret1.eq.1) goto 120
      endif

      goto 210
c
c... ret from begpas ok.   set 1st step dp
c
615   irejog = 0
      if (ifl(90).eq.1) ifl(90) = 1-ifl(363)
c
c... if ifl(298) set, return to mocomb now            21-apr-89
c
      if (ifl(298).eq.1) goto 999
c
c... if first motion after genpts/chkpts statement & not modify
c... save current current tool point & vectors
c
      if (ifl(246).gt.0.and.ifl(247).eq.1.and.ifl(104).eq.0 .and.
     1    lv92) call savmot(sc(1),sc(7))

      call conv4_8 (t(7,ia),fromsv(7),3)
      if (lpos .eq. 1) call conv4_8 (t(1,ia),ptb,9)
c
c... push tool fwd .001 to clarify cs relationship
c
      if (itry .eq. 2) goto 642
      call uvcplvc4 (t(1,ia), t(7,ia), t(1,ia), 0.001)
      dp = 250.*tol
      if(dp.gt.dpmax) dp = dpmax
      if(iflat.eq.1 .and. mod.ne.4 .and. jd(401).ne.CIRCLE) dp = dpmax
c
c... if circ int mode, save pass startpt in hsc
c
      if(ifl(95).ne.0) call conv4_8 (t(1,ia),hsc,9)
      goto 642
c
c... verify step tol (if in iter mode)
c
620   hdp = dp
      co  = f_dot4 (t(7,ia),t(7,ic))
c
c.....extra crwn check                23-MAY-90  IJD
c
      call vcmnvc4 (t(1,ia),t(1,ic),vec)
      sec = f_mag4 (vec)

      if (sec.ge.0.00001)
     *   co = min2_r4 (co, max2_r4 (f_dot4 (vec,t(7,ic))/sec, 0.1))

      if (co.gt..999999) then
c
c..... changed since real*4 co is practically = 1. if >.999999
c..... this change allows to avoid running several loops if crwn>atol
c..... eduard 001013
c
        co = 1.
        si = 0.
      else
        si=sqrt(1.-co**2)
      endif
      crwn=dp*si/8.
c
c... angdp addition                 26-jan-83
c
      if (mod.eq.0.or.hdp.lt.dpmin) goto 621

      aco = f_dot4 (t(4,ia),t(4,ic))
      if (abs(aco).gt..999999) aco=.999999
      ang=sqrt(1.-aco**2)
      angdp=angmax*hdp/ang*angfct
      if (angdp.lt.dpmin) angdp=dpmin
      if (angdp.gt.dpmax) angdp=dpmax
      if (DEBUGX .eq. 1) then
          write (dbuf,7030) idpk,crwn,atol,btol
 7030     format ('Idpk = ',i2,'   Crwn = ',f12.4,'   Atol = ',f12.4,
     1            'Btol = ',f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7031) angdp,hdp,dpmin
 7031     format ('Angdp = ',f12.4,'   Hdp = ',f12.4,'Dpmin = ',f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
      if(angdp.ge.hdp-dpmin/4.)goto 621
      dp=angdp
      ireduc=1
      if (lv81) goto 626
      if (crwn.lt.btol) goto 626
      angdp=hdp*sqrt(tol/crwn)
      if (dp.gt.angdp) dp = angdp
      goto 626
c
c... if flat case, probable exit  (except when cs is circle)
c... the hard-coded condition below changed so that it is defined by
c... the current tolerance. eduard 001013
c
c621   if (crwn.gt.1.e-5) goto 622
621   if (crwn.gt.EPS) goto 622

      if (iflat.eq.1.and.jd(401).ne.CIRCLE.and.iptk.gt.0.and.
c
c... jingrong 4/15/99 if csdis<dpmax, do not set dp=dpmax.
c     *  .not.go_slow) goto 632
c
     *  .not.go_slow.and.t(12,ic).gt.dpmax) goto 632
c
c... this is a computed flat case. be conservative.
c... if ps endflat contact, even more    "  "
c
      dp=hdp*1.5
      if (ifl(56).gt.1..or.go_slow) dp=hdp*1.25

      goto 634
622   if (crwn.lt.btol) goto 624
c
c... crwn too large. shorten dp ( but 1/4 hdp min )
c
      dp = max2_r4 (dp*sqrt(tol/crwn), 0.25*hdp)
      ireduc=1
      goto 626
624   if (crwn.gt.atol .or. ireduc.gt.0 .or. sharp.ne.0) goto 630

c
c... crwn too small. increase dp ( to 2*hdp max ) unless
c... ps endflat contact or unless a reduction has occured
c
      dp = min2_r4 (dp*sqrt(tol/crwn), 2.*hdp)
      if (ifl(56).gt.1.or.go_slow) dp = min2_r4 (dp, 1.5*hdp)

      if (dp.gt.angdp) goto 630

626   continue
      if (dp.lt.dpmax) goto 627
      dp = dpmax
      if (hdp.ge.dpmax) then
        if (ishrp.lt.1 .or. itfl.gt.0) goto 642
      endif

627   idpk = idpk+1
c
c... trouble. set dp small and go one more time.
c
      if (ishrp.eq.2 .and. idpk.lt.5) then
        if (crwn.lt.atol .and. dp.gt.20.*tol) then
          if (hdp.gt.20.*tol) then
            dp = hdp
            goto 630
          else
            idpk = 5
          endif
        else if (ireduc.gt.0 .and. crwn.gt.btol) then
          irejog = 0
        endif
      endif

      if (idpk.eq.5) dp = min3_r4 (50.*tol, dpmax, angdp)
      if (idpk.le.5) goto 110
c
c.....calc next dp
c
630   if (si.gt.1.e-6) goto 635
632   dp=dpmax
634   t(11,ia)=1000.
      goto 640
635   rc=dp/si
      t(11,ia)=rc
      dp=sqrt(8.*rc*tol)

640   continue

      dp = min3_r4 (dp,dpmax,angdp)
      if (go_slow) dp = min2_r4 (dp,dpmax1)
c
c..... CS
c
642   continue

      if (.not.lv94 .and. ifl(280).eq.0 .and. jd(201).eq.CURVE) then

        if (ishrp.eq.1 .and. abs(t(19,ia)-ushrp) .lt. 1.e-4) then
          ishrp = 2
          if (ifl(21).eq.0) then

            ti=t(4,ia)
            tj=t(5,ia)
            tk=t(6,ia)

            vx=vxshrp(1)
            vy=vxshrp(2)
            vz=vxshrp(3)

            rx=vy*tk-vz*tj
            ry=vz*ti-vx*tk
            rz=vx*tj-vy*ti

            fx=rz*s(2,1)-ry*s(3,1)
            fy=rx*s(3,1)-rz*s(1,1)
            fz=ry*s(1,1)-rx*s(2,1)

            sec=sqrt(fx**2+fy**2+fz**2)
            if (fdshrp*(fx*vx+fy*vy+fz*vz) .lt. 0.) sec = -sec

            t(7,ia) = fx/sec
            t(8,ia) = fy/sec
            t(9,ia) = fz/sec

            t(19,ia) = ushrp + fdshrp*1.e-4
            klik = 1

          endif

        else if (ishrp.eq.1 .and. ifl(21).ne.0) then
          if (wedged) then

            ishrp = 2

            ti=t(4,ia)
            tj=t(5,ia)
            tk=t(6,ia)

            vx=fdshrp*vxshrp(1)
            vy=fdshrp*vxshrp(2)
            vz=fdshrp*vxshrp(3)

            rx=vy*tk-vz*tj
            ry=vz*ti-vx*tk
            rz=vx*tj-vy*ti

            fx=rz*s(2,1)-ry*s(3,1)
            fy=rx*s(3,1)-rz*s(1,1)
            fz=ry*s(1,1)-rx*s(2,1)

            sec=sqrt(fx**2+fy**2+fz**2)
            if (fx*vx+fy*vy+fz*vz .lt. 0.) sec = -sec

            t(7,ia) = fx/sec
            t(8,ia) = fy/sec
            t(9,ia) = fz/sec

            t(19,ia) = ushrp + fdshrp*1.e-4
            if (fdshrp.eq.1 .and. t(19,ia).gt.1) then
              t(19,ia) = 0.0001
            else if (fdshrp.eq.-1 .and. t(19,ia).lt.0) then
              t(19,ia) = 0.9999
            endif

            klik = 1
          else
            if (zercor.eq.0 .and. fdshrp*(t(19,ia)-ushrp).ge.0.0001 .or.
     x   zercor.eq.1 .and. t(19,ia).lt.0.2 .and. t(19,ia).gt.0.0001 .or.
     x   zercor.eq.-1 .and.t(19,ia).gt.0.8 .and. t(19,ia).lt.0.9999)
     x      then
            ishrp = 2
            klik = 1

            endif

          endif

        endif

      endif

      isrf=3

      if (jd(401).eq.PLANE) goto 645
c          extrap u,v  except at pass-end
      if(itfl.gt.1.or.ifl(83).gt.0)goto 645
c
c... aak:  don't extrapolate if gougck is on            AUG-18-97
c
      if (icsgck.gt.1..and..not.lv84) goto 645

      call avcplbvc4 (1.+ro,t(25,ic),-ro,t(25,ib),t(25,ia))

645   continue
      call vctovc4 (s(1,3),oldcs)
      oldcs(4) = s(4,3)

      if (l2csrf .and. icsgck.ge.4 .and. ltoolcyl) then
        call csrelx1 (tol)
      else
        call csrelx
      endif
c
c...It is possible for to fix a
c..."Conditions too severe" error
c...by shrinking the step size
c...Let's try it a couple of times
c...prior to just failing
c...Bobby - 05/05/10
c
      if (ifl(2) .eq. 51 .and. ntry51 .lt. 5) then
          ifl(2) = 0
          ntry51 = ntry51 + 1
      endif

      if (.not.lv93 .and. ifl(289).eq.1 .and. itfl.eq.-2 .and.
     *                                             lifted.eq.0) goto 20
      if (itry .eq. 2) then
        if (abs(t(12,ia)).lt.tol) call endps0(itfl0)
        ifl(2) = 141
        goto 99998
      endif

      if (lfncmb.eq.1 .and. itfl.eq.0 .and. sc(160).gt.50.*tol .and.
     x    t(12,ia).ge.0 .and. t(12,ic).lt.0) then
c
c..... this drive is just to determine an intermediate plane for the
c..... special two-fan logic. this position is acceptable, so exit
c
        itfl = 5
        lfncmb = 0
        goto 800
      endif

      if (ifl(2) .eq. 466) goto 999
      co = abs (f_dot4 (s(1,3),oldcs))
      dis = f_dist4 (t(1,ia),t(1,ic))
c
c..... if the tool is farther from the previous (clfile) location
c..... than the previously calculated distance from CS, and CS
c..... plane has significantly changed this time: try once to
c..... redo this step with a smaller dp and go_slow=.true.
c

      if (.not.lv91 .and. iptk.gt.0 .and. co.lt..99 .and. .not.tryd
     *  .and. dis.gt.t(12,ic) .and.  t(12,ic).gt.dpmin) then
          dp = min2_r4 (dp,dpmax1)
c
c..... Dassault'c NCL316: dpmax1 could be too big. In the test case the
c..... Check Surface was thin but not flat, so a little difference in
c..... a look point yielded different planes
c
          if (.not.lv93 .and.
     x        dp .gt. 2.*t(12,ic) .and. t(12,ic) .gt. 10.*dpmin) then
            dis = (t(25,ia)-t(25,ic))*(t(25,ia)-t(25,ic)) +
     x            (t(26,ia)-t(26,ic))*(t(26,ia)-t(26,ic))
            if (dis .gt. 0.25) dp = t(12,ic)
          endif
          go_slow = .true.
          tryd = .true.
          call vctovc4 (oldcs,s(1,3))
          s(4,3) = oldcs(4)
          goto 110
      endif
c
c..... if psmult and the motion is stalling while far from end,
c..... increase the step
c
      if (.not.lstald .and. .not.lv93 .and. iptk.gt.1 .and. itfl.eq.0
     x    .and. reset_dp .and. psthk.gt.sc(28)/4) then
        if (dis .ge. dpmin) then
          call vcmnvc4 (t(1,ia),t(1,ic),vec)
          vec(1) = vec(1)/dis
          vec(2) = vec(2)/dis
          vec(3) = vec(3)/dis
          co = f_dot4(t(7,ia),vec)
        else
          hdis = f_dist4 (t(1,ia),t(1,ib))
        endif
        if ( co.ge.-0.999 .and. (dis.ge.dpmin .or. hdis.ge.dpmin))
     *    goto 646
        if (t(12,ia).gt.20*dp .and. t(12,ia).gt.sc(28)) then
          dp = 10*dp
          if (dp .gt. dpmax1) dp = dpmax1
          lstald = .true.
          goto 110
        endif
      endif
c
c... if 1st time here in go/strtup, dpmax may have chgd 11-feb-88
c
      if(iptk.gt.0.or.ifl(280).eq.0.or.sc(54).gt.dpmax)goto 646
      dpmax=sc(54)
      if(dp.gt.dpmax) dp = dpmax

646   continue
c
c.....if 1st time here in slowdn f/r case, csdis must be +.  14-may-85
c.....if csdis.le.0, skip phase 1 entirely.
c
c...vp 1.14.94 old FR/AT if version < 8.209
c
      if (.not.lv82) go to 649
c
      if(ifl(213).eq.0.or.iptk.gt.0.or.isltim.ne.1)goto 649
      if(t(12,ia).gt.0..or.ifl(42).ne.0)goto 649
c**        *** void phase 1 of slowdn and do phase 2 only
      fslow=sc(126)
      if(iabs(ifl(213)).eq.2)fslow=sc(126)*sc(123)
c
      call putcl(2000,1009,2,fslow)
      t(12,ia)=t(12,ia)+sc(125)
      isltim=2
649   continue
c
c...   If checking to a point, reduce dp to dist to pt.
c
      if (lcspt) then
        cutmax = sc(28)/2.0d0
        if (cutmax.lt.tool(1)/2.0) cutmax = tool(1)/2.0
        csdis = t(12,ia)
        hdis = csdis - cutmax

        if (hdis.gt.0.0) then
          dp = min2_r4 (dp,csdis)
        else
          dp = min2_r4 (dp,0.5*abs(csdis))
        endif

        if (dp.lt.dpmin) dp = dpmin
      endif
c
c.....if iptk=1 and fan/mode, keep step reasonable 14-jun-84
c
      if (iptk.eq.1 .and. mod.eq.4) then
         hdis = abs(0.5*t(12,ia))
         if (dp.gt.hdis) dp = hdis
      endif
c
c.....endpas controls end steps
c
      if (itfl.lt.1) goto 700
660   continue
      call endpas(kret,rdp)
c
c... rdp=(-) is trouble                       7-16-82
c
      if (rdp.lt.0.) rdp = dpmin
c
c...If calculated step is more than 2x cs distance
c...Then we are probably not close to the cs yet
c...use the previously calculated dp
c
      if (DEBUGX .eq. 3) then
          write (dbuf,7035) t(12,ia),dp,rdp
 7035     format ('Csdis = ',f12.4,'   dp = ',f12.4,'   rdp = ',f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
      if (rdp .gt. t(12,ia)*4. .and. rdp .gt. dpmax*dpfac .and.
     1        .not. lv97) then
          rdp = dpmax * dpfac
          if (t(12,ia) .gt. dpmax) rdp = t(12,ia) * dpfac
          dpfac = dpfac + 1.
          if (DEBUGX .eq. 3) then
              write (dbuf,7036) rdp,dpfac
 7036         format ('rdp = ',f12.4,'   dpfac = ',f12.4)
              call ctob (dbuf,dout)
              call nclxostr(dout)
          endif
      endif
      dp = rdp
c
c... if itfl=5, all done. otherwise bra on kret
c
      if (itfl .eq. 5) then
c
c..... if fanning to a plane, adjust tool axis at the end - FSR 60289
c
        asn = sc(146)
c
c..... the adjustment is restricted to a "simple" tool (nonbarrel, nondisk,
c..... not angular); and to a "real" CS plane - Dassault MFGNC243
c
        if (.not.lv91 .and. ltoolcyl .and.
     *      .not.lcvgid.and..not.ldschk.and..not.lmdnow .and.
     *      mod.eq.TA_FAN) then
          if (ksn(4).eq.PLANE) then
            call f_cross4 (s(1,2),s(1,3),vec)
            call unitizevc4 (vec)
            pcon=s(4,1)+sc(23)
            pco = vec(1)*s(1,1)+vec(2)*s(2,1)+vec(3)*s(3,1)
            if (pco .gt. 1.) pco = 1.
            psi = sqrt(1.-pco**2)
            pcon = pcon+tool(2)*(1.-pco)+tool(6)*psi
            p = (pcon - f_dot4 (s(1,1),t(1,ia))) / pco
c
c..... also, we don't do it if it moves the tool far from PS - MFGNC246
c
            if (abs(p) .le. tol) then
              call vctovc4 (vec,t(4,ia))
              call uvcplvc4 (t(1,ia),vec,t(1,ia),p)
            endif
          else if (.not.lv92 .and. ksn(4).eq.SURF .and. llttcs .and.
     *        ifl(80).eq.0 .and. .not.lcsavd .and. icsgck.gt.2) then
            jd(401) = SURF
            call csavoid
            if (lcsavd) then
              ifl(80) = 1
              call ncl_eqlptr(isv2,imotp,iflg)
              if (iflg .eq. 0)
     *          call cldel (iclf,isv2,iept,imotp)
              call ptclos
            else
              jd(401) = PLANE
            endif
          endif
        endif
        goto 800
      endif
c
c... kret = 0     re-calc this loc per new dp.
c...      = 1     go output this loc and exit or cont.
c
      if (kret.eq.0) goto 100
      goto 800
c
c... check csdis for end of pass
c... (( to be improved ))           do not check 1st pt
c
700   csdis=t(12,ia)
      if (iptk.eq.0) then
        if (ifl(102).ne.0 .and. ifl(102).ne.4) then
c
c... set if(102) to 0 for first tanto circle intersection, 4 for second.
c
          co = f_dot4(t(7,ia),s(1,3))
          if (abs(co).gt..7 .and. hcsdis.ge.0. .and. csdis.lt.0.) then
c
c... Tool passed through the check plane, Set ifl(102) to 4 if it near
c... tanto postition, 0 if it is on opposite side.
c
            if (ifl(102).gt.0) then
              ifl(102) = 4
            else
              ifl(102) = 0
            endif
          else
c
c... Otherwise set ifl(102) to 4 if tool is moving away from
c... tanto postition, 0 if it is moving towards..
c
            if (ifl(102).eq.1 .or. ifl(102).eq.-1) then
              ifl(102) = 4
            else
              ifl(102) = 0
            endif
          endif
        endif
        goto 800
      endif
c
c...   If check to a point flag is set, output point.
c
      if (ifl(327).gt.0) then
        ifl(327) = 0
        goto 800
      endif

      if (csdis.gt.0.) goto 720
c     if(t(12,ic).lt.0.)goto 800
c
c..... do not go to endpas if this is the first step, dsthk is
c..... large (more than tool diam), and we were on CS at start
c..... (after the previous motion)
c..... extra condition - if this is not a 'go' statement
c
      if (t(12,ic).lt.0. .or.
     *    (.not.lv91. and. iptk.eq.1 .and. ifl(280).eq.0 .and.
     *      sc(24).ge.sc(28) .and. csdis0.lt.tol)) then
      goto 800
      endif

      if (itfl.eq.0 .and. lfncmb.eq.1 .and.
     x    (sc(160)+tool(1)+50*tol) .lt. fan160) then
      goto 800
      endif

      if (abs(csdis).le.t(10,ia)) goto 660
      if (abs(csdis).lt.tol*10.) goto 660
      if (lcspt .and. ifl(28).gt.0) goto 660
      t(12,ic)=-t(12,ic)
      goto 800
720   if (csdis.lt.t(12,ic)/2.) then
        co = f_dot4 (s(1,3),oldcs)
c
c..... fsr 60861 - if the motion goes along CS and the CS normal
c..... just flipped, do not go to endpas
c
        if (itfl.eq.0 .and. co.lt.-0.999 .and. csdis.gt.10.*tol) then
          co = abs(f_dot4 (s(1,3),t(7,ia)))
          if (.not.lv93 .and. co.lt.0.001) then
          goto 800
          endif
        endif
        goto 660
      endif
c
c..... rotate cols, go again.
c
800   iptk = iptk+1

      if (lfncmb.eq.-1 .and. itfl.eq.0) then
c
c..... Dassault'c NCL344:
c..... abandon this fan if tool axis points inside PS
c
        si = f_dot4 (s(1,1),t(4,ia))
        if (si .lt. -0.05 .and. sc(160).gt.50.*tol) then
          ifl(2) = 9596
          goto 999
        endif
      endif

      if (.not.lv94 .and. ifl(280).eq.0 .and. jd(201).eq.CURVE) then

        if (ishrp.eq.1 .or. (ishrp.eq.2 .and. klik.eq.0)) then

          if (zercor .eq. 0) then
            if (fdshrp*(t(19,ia)-ushrp) .ge. 0.0001) ishrp = 0

          else if (zercor .eq. 1) then
            if (t(19,ia).lt.0.2 .and. t(19,ia).gt.0.0001) ishrp = 0

          else if (zercor .eq. -1) then
            if (t(19,ia).gt.0.8 .and. t(19,ia).lt.0.9999) ishrp = 0

          endif

        endif

        if (ishrp.eq.0) then
          isrf = 2
          call fndcor
        endif
        klik = 0
      endif

      if (lcvgid .and. .not.lv92) then
        gidu = giduia
        gidh = gidhia
        gidpt(1) = gdptia(1)
        gidpt(2) = gdptia(2)
        gidpt(3) = gdptia(3)
      endif

      inidcv = 0
      tryd = .false.
      lstald = .false.
      if (ijk .gt. 0 .and. t(12,ia) .lt. dijk) then
         ijk=0
         ifl(23) = 1
      endif
      if (initds .gt. 1) initds = 1
c
c..... ifl(146) is used in csrel to decide whether to calculate a new
c..... apex point. the idea is that during the second fan, once the
c..... 'abandoned' plane is cleared, an apex point should be
c..... recalculated while we are moving away from the CS plane (and once
c..... we start again moving toward it, the standard logic should work)
c..... DASSAULT MFGNC229
c
      if (abs(t(12,ia)).gt.tol) ifl(146) = 0
      if (ifl(146).eq.1 .and. t(12,ia) .lt. 0.
     *   .and. t(12,ic).lt.tol) ifl(146) = 2

      ia = ia+1
      ib = ib+1
      ic = ic+1
      if (ia.gt.3) ia = 1
      if (ib.gt.3) ib = 1
      if (ic.gt.3) ic = 1

      if (ifl(85).lt.0) goto 875
c
c..... if indicated, add this pt to tdat.  (also ijk if multax on)
c..... ifl(80)=1 means fan pass1.  skip o/p        14-jun-84
c..... jingrong calulculate the dis travelled for fan pass1 11/09/99.
c..... was  if (ifl(80).gt.0) goto 852
c
      if (ifl(80).gt.1) goto 852

      call conv8_8 (tb,otb,21)
      call conv4_8 (t(1,ic),tb,9)

      if (ifl(80).eq.1) goto 851

      if (iptk.eq.1) call uvcplvc (tb(1),tb(7),tb(1), -1.0d-03)
      x = tb(1)
      y = tb(2)
      z = tb(3)
      call conv4_8 (s(1,1),tb(13),3)

      if (jd(1).eq.SURF.or.jd(1).eq.21) then
        d1 = f_dot4 (s(1,1),s(8,1)) - s(4,1) - psthk
        tb(10) = s(8,1)  - s(1,1)*d1
        tb(11) = s(9,1)  - s(2,1)*d1
        tb(12) = s(10,1) - s(3,1)*d1
      else
        pco = tb(13)*tb(4)+tb(14)*tb(5)+tb(15)*tb(6)
        if (pco.gt.1.0) pco = 1.0
        psi = sqrt(1-pco**2)

        if (lnodisk .or. pco.ge.sbe) then
          f1 = tool(6)+tool(2)*psi
          f2 = tool(2)*(1-pco)
        else
          f1 = tool(1)/2.0
          f2 = tool(3)
        endif

        call f_cross(tb(4),tb(13), vtmp)
        call f_cross(tb(4),vtmp, vtmp)
        call unitvc (vtmp, vtmp)
        tb(10) = x+vtmp(1)*f1+tb(4)*f2
        tb(11) = y+vtmp(2)*f1+tb(5)*f2
        tb(12) = z+vtmp(3)*f1+tb(6)*f2
      endif

      tb(19) = s(1,2)*trgt
      tb(20) = s(2,2)*trgt
      tb(21) = s(3,2)*trgt
      if (jd(201).eq.PLANE) then
        d1 = tb(19)*x+tb(20)*y+tb(21)*z-s(4,2)*trgt
        tb(16) = x-tb(19)*d1
        tb(17) = y-tb(20)*d1
        tb(18) = z-tb(21)*d1
      else
        tb(16) = s(5,2)+dsthk*tb(19)
        tb(17) = s(6,2)+dsthk*tb(20)
        tb(18) = s(7,2)+dsthk*tb(21)
        if ( jd(201).eq.CURVE .or. jd(201).eq.CIRCLE
     *                        .or. jd(201).eq.LINE) then
          h = hds0
          if (lv96) h = t(21,ic)
          s(8,2)  = x+tb(4)*h
          s(9,2)  = y+tb(5)*h
          s(10,2) = z+tb(6)*h
        endif
        d1 = f_dot4 (s(1,2),s(8,2)) - s(4,2)
c
c..... qar 97096: put DS points at the uniform height if possible
c
        if (ifl(42).eq.0 .and. dstype.eq.SURF .and. .not.lv96) then
          call conv8_4 (tb(1),pte1,3)
          call conv8_4 (tb(4),vta1,3)
          call vcmnvc4 (s(8,2),pte1,vec)
          del1 = f_dot4 (vec,vta1) - hds0
          call uvcplvc4 (s(8,2),vta1,pte1, -del1)
          f1 = f_dot4 (s(1,2),pte1) - s(4,2)
          f2 = abs (f1 - d1)
          if (f2 .lt. tol) then
            call vctovc4 (pte1,s(8,2))
          endif
        endif
        
        tb(16) = s(8,2)  - s(1,2)*d1
        tb(17) = s(9,2)  - s(2,2)*d1
        tb(18) = s(10,2) - s(3,2)*d1
      endif

      if (lpos .eq. 1) then
        call conv8_8 (tb(10),fromsv(10),12)
        call conv8_8 (tb(10),ptb(10),12)
      endif

      if (.not.cntctp) goto 803
      if (iptk.lt.2) goto 8025
      dis=hht(7)*(t(1,ic)-hht(1))+hht(8)*(t(2,ic)-hht(2))+
     1    hht(9)*(t(3,ic)-hht(3))
      dx=hht(1)+hht(7)*dis-t(1,ic)
      dy=hht(2)+hht(8)*dis-t(2,ic)
      dz=hht(3)+hht(9)*dis-t(3,ic)
      esq=dx**2+dy**2+dz**2
      rval = qtolsq
      if (sharp .ne. 0) rval = qtolsq * 4
      if (dis.ge.0..and.esq.lt.rval) goto 8025

      if (ifl(2).eq.0) then
         ifl(2) = 255
         if ( psmult .and. svmps.ne.ifl(341) ) ifl(2) = -255
         if (ifl(91).lt.0) ifl(2) = -255
         write(errcom,1020) t(1,ic),t(2,ic),t(3,ic)
         if (DEBUGX .eq. 1) then
             call ctob ("ERROR -255 #1",dout)
             call nclxostr(dout)
             write (dbuf,7032) dis,esq,qtolsq
 7032        format ('Dis = ',f12.4,'   Esq = ',f12.4,'   Qtolsq = ',
     1               f12.4)
             call ctob (dbuf,dout)
             call nclxostr(dout)
         endif
      endif

8025  call clcept(tb)
803   continue
      if (ifl(293).eq.0) goto 804
c
c... ta/fixed,normal - ouput ps normal as tlaxis
c
      call conv4_8 (s(1,1), tb(4), 3)
c
c... ifl(95)=1 means this is circ int pass. no action until end.
c
804   if (ifl(95).eq.1) then
        if (lv91) goto 8075
c
c..... calculate the distances between the position on PS and
c..... the circle (DS) plane. If the distances are not stable,
c..... unset the circular interpolation flag, get out of the mover,
c..... then come back and do the usual motion.
c
        ddd = cvec(1)*t(1,ic)+cvec(2)*t(2,ic)+cvec(3)*t(3,ic)
        if (iptk .eq. 1) then
          dddmin = ddd
          dddmax = ddd
        else
          if (ddd .gt. dddmax) dddmax = ddd
          if (ddd .lt. dddmin) dddmin = ddd
          if (dddmax - dddmin .gt. tol) then
            ifl(95) = 0
            ifl(2) = 9595
            goto 999
          else
            if (.not. icck) goto 8075
          endif
        endif
c
c......Circular interpolation while driving a cylinder
c......make sure we do not drive the surface extension
c
        if (icck) then
          plx(1) = t(1,ic)
          plx(2) = t(2,ic)
          plx(3) = t(3,ic)
          tcdis =  f_dist(plx,ccen)
          if (dabs(tcdis-cdis) .gt. tol*2) then
            ifl(95) = 0
            ifl(2) = 9595
            goto 999
          else if (iptk .ne. 1) then
            goto 8075
          endif
        endif
      endif
c
c... check curvature from last entered pt+fwd
c
      ifl29 = ifl(29)
      dis = hh(7)*(tb(1)-hh(1))+hh(8)*(tb(2)-hh(2))+
     *      hh(9)*(tb(3)-hh(3))
      dx = hh(1)+hh(7)*dis-tb(1)
      dy = hh(2)+hh(8)*dis-tb(2)
      dz = hh(3)+hh(9)*dis-tb(3)
      esq = dx**2+dy**2+dz**2
      if (lstep) goto 805
c          if err large or logstpt, go hold this loc
      if (esq.gt.dtolsq .or. iptk.eq.1 .or. ishrp.eq.2) goto 805
c          check ta chg
      acos=hh(4)*tb(4)+hh(5)*tb(5)+hh(6)*tb(6)
      if(acos.lt..999992)goto 805
c
c... if final point is in-line, don't output prev pt
c
      ifl(29) = 0
      if(itfl.eq.5)goto 808
c          withold this loc from o/p and set ifl(29)
      ifl(29)=1
      goto 850
805   continue
c
c... final pt not in-line - output prev if it was in-line
c
      if (itfl.ge.5) then
         ifl(29) = ifl29
         goto 808
      endif
c
c... hold this loc being entered in tdat
c
806   continue

      call conv8_8 (tb,hh,9)
      if (cntctp) call conv4_8 (t(1,ic),hht,9)
c
c...if not in-line with former, issue error
c
      rval = qtolsq
      if (sharp .ne. 0) rval = qtolsq * 4
c
c..... QAR 92390: added ifl(280) condition, so that no checks are made
c..... for a GO statement
c
      if ( iptk.lt.2 .or. mod.eq.4 .or. cntctp .or. ifl(280).eq.1 .or.
     *     (dis.ge.0. .and. esq.lt.rval) ) goto 8075
      if (esq.lt.dtolsq .and. ishrp.eq.2) goto 8075

      if (ifl(2).eq.0) then
         ifl(2) = 255
         if ( psmult .and. svmps.ne.ifl(341) ) ifl(2) = -255
         if (ifl(91).lt.0) ifl(2) = -255
         write(errcom,1020) tb(1),tb(2),tb(3)
         if (DEBUGX .eq. 1) then
             call ctob ("ERROR -255 #2",dout)
             call nclxostr(dout)
             write (dbuf,7032) dis,esq,qtolsq
             call ctob (dbuf,dout)
             call nclxostr(dout)
         endif
      endif

8075  continue
c
c... do not output pass logical startpt unless forced by begpas
c
      if(iptk.gt.1 ) goto 808

      if (lpos .eq. 1) then
c
c... aak 12-mar-1998: modfy before writing to 5210 cl-rec.
c
          if (ifl(104).eq.1) call modfy (ptb,ptb)
c
c...If the normal to DS is a null vector use the normal vector from the motion array
c
          if (ptb(19).eq.0 .and. ptb(20).eq.0 .and.
     1      ptb(21).eq.0) then
            ptb(19) = s(1,2)
            ptb(20) = s(2,2)
            ptb(21) = s(3,2)
          endif
          if (.not. lmintf(1)) call putcl (5210,3,1,ptb)
          lpos = 0
      endif

      if(ifl(90).eq.0) goto 850
808   continue
      ds(1) = s(1,2)
      ds(2) = s(2,2)
      ds(3) = s(3,2)
      if (ifl(29).gt.0) call outmot (tdat,otb,ds,lnk,jptk,iclass,isubcl)
      ifl(29) = 0
      call outmot (tdat,tb,ds,lnk,jptk,iclass,isubcl)
      dpfac  = 1.
850   continue
      svmps = ifl(341)
c
c......on-line plot (unless dntcut)       2-4-82
c
      if (iptk.gt.numpts.and.ifl(91).lt.0.or.itfl.eq.5) ifl(130)=0
c
851   continue
c
c... add up distance moved for muliple check surfaces
c... ian 04-jun-1998: for multiple CS, sum up
c... as well the distances the tool top travels
c
      call vctmsc (tb(4), vtmp, sc(30))
      call vcplvc (tb, vtmp, cstapt)

      if (iptk.gt.1) then
        sc(160) = sc(160) + f_dist (tb,otb)
        call vcmnvc (cstapt, ocstpt, vtmp)
        dtmp = f_mag (vtmp)
        if (f_dot(vtmp, tb(7)) .lt. ZERO) dtmp = -dtmp
        sc(176) = sc(176) + dtmp
C
C...When ifl(349) is set to 1, we are here just checking to see
C...which of the multiple check surfaces should be used.
C...If the minimum of sc(160) or sc(176) is already larger than
C...the value in sc(181), then we are already further away then
C...one of the previous check surfaces, so exit. JLS 6/18/99
c
        mindis = min2_r8(sc(160),sc(176))
        if (ifl(80).eq.0.and.mindis.gt.sc(181)+10.d0*tol
     *      .and.ifl(349).eq.1) then
c
c..... ifl(352) = 2 signals to mocomb to do the motion in fan.
c..... DASSAULT MFGNC242 - midmotion is too long because tool(18),tool(19)
c..... are wrong.
c
          if (ifl(352).eq.1) ifl(352) = 2
          goto 99999
        endif
c
C...for fan pass1 if mindis>sc(181)+2*tooldiameter
c...exit. jingrong 11/09/99.
c
        if (ifl(80).eq.1.and.mindis.gt.(sc(181)+2.*sc(28))
     *      .and.ifl(349).eq.1) goto 99999
c
c...jingrong 06/24/99 if traveled dis in any sub motion of motion combine
c...exceeds the 1.5*tot dis, give up and return to mocomb.
c
        if (ifl(352).eq.1.and.svdism.gt.0.and.
     x   sc(160).gt.1.5*svdism) then
            ifl(352) = 2
            goto 99999
        end if
c
c..... if this is the second try after error 141, stop if the travelled
c..... distance is already too large
c
        if (.not.lv91 .and. domov .and. itry.gt.0
     *      .and. ((sc(160)-sc160).gt.(2.5*csdis0))) then
           ifl(2) = 141
           goto 99998
        endif
      endif

      call vctovc (cstapt, ocstpt)

852   continue
c
c... if interrupt switch on, abort motion
c... RAH: added call to set intr flag - effective on SGI ONLY
c
      call ckintr(ifl(86),ifl(35))
      if(ifl(86).ne.0) then
        ifl(2) = 149
        goto 999
      endif
      if (iptk.gt.1) totdis = tdis

875   continue
c
c.....error.    numpts exhausted
c
      if (iptk.gt.numpts) then
         if (ifl(91).gt.0) then
           ifl(2) = 141
           goto 999
         else
           ifl(2) = -141
           ds(1) = s(1,2)
           ds(2) = s(2,2)
           ds(3) = s(3,2)
           if (ifl(29).eq.1)
     *        call outmot (tdat,tb,ds,lnk,jptk,iclass,isubcl)
           goto 994
         endif
      endif
c
c...exit from mover when itfl=5
c
      if (itfl.ne.5) goto 100
c
c.....normal exit route; if no error, load tcol(c) in sc(1-9)
c
      if (ifl(2).gt.0) goto 999
c
c.....if fan pass1, reset for plane cs and go again
c
      if (fanint .eq. 2) fanint = 0
      if (ifl(80).eq.0) goto 994
      lnk = 1
      if (mod.eq.TA_FAN .and. fdeg.gt.1 .and. fanint.eq.0) then
        fanint = 1
      else
        if (fanint .eq. 1) then
          fanint = 2
          fuldis = sc(160) - sc160
          sc4 = t(4,ic)
          sc5 = t(5,ic)
          sc6 = t(6,ic)
        endif
        ifl(80) = 0
        ifl(42) = kdntcu
      endif
c
c.... Reset the distance traveled jingrong 11/09/99.
c
      sc(160) = sc160
      sc(176) = sc176
c
c..... make cs a pl from s(,3) tbl
c
      if (.not.lv91 .and. lcsavd .and. ltoolcon) then
c
c..... if "tanto CS" ending would gouge, readjust the final plane for an
c..... angular tool - DASSAULT MFGNC243
c
        dot1 = f_dot4(t(4,ic),s(1,3))
        if (abs((dot1 + sbe)*tool(3)) .gt. tol) then
          call uvcplvc4 (s(1,3),t(4,ic),vec,-dot1)
          call unitizevc4 (vec)
          call avcplbvc4 (cbe,vec,-sbe,t(4,ic),s(1,3))
          s(4,3) = f_dot4(s(1,3),s(5,3))
        endif
      endif
      do 993 i=1,6
      tb(i) = t(i,ic)
      t(i,3)=sc(i)
      if (mod.eq.4) d(i+101)=s(i,3)
993   continue

      call conv4_8 (t(7,ic), tb(7), 3)
      call vctovc4 (fwd0,t(7,3))
c     if (lttcsf .and. (lv91 .or. .not.mocom)) call ncl_2sfbnry_free
      lttcsf = .false.

      if (mod.eq.14 .or. mod.eq.15) then
        if(lv84) then
           if (abs(tacvds - totdis) .gt. tol) then
             if (knttcv .gt. 10) then
               ifl(2) = 163
               goto 999
             endif
             ifl(80)=1
             knttcv = knttcv+1
           endif
           tacvds = totdis
           totdis = ZERO
           goto 20
        else

          diff = dabs(tacvds - totdis)/totdis
          if(diff.gt.CURVE_TOL.and.knttcv.le.MAX_ITER) then
             ifl(80) = 1
             knttcv = knttcv+1
          endif

          if(knttcv.le.1) then
             tacvds = totdis
          else
             tacvds = HALF*(tacvds + totdis)
          endif
          totdis = ZERO
          jd(401) = PLANE
          goto 20
        endif

      endif

      jd(401) = PLANE
      lavoid  = .false.
      if (lcsavd) sc(36) = -1.0d0
      csu1 = t(25,ic)
      csv1 = t(26,ic)
c
c... aak 21-sep-1998: increased the factor here; was 100.
c
      if (lv90) then
        call uvcplvc4 (t(1,3), t(4,3), t(28,3), 100.)
      else
        call uvcplvc4 (t(1,3), t(4,3), t(28,3), 1.0e9)
      endif
      goto 20
c
c... if slowdn f/r tim1, do not update sc(1-9)
c
994   if(ifl(213).ne.0.and.isltim.eq.1)goto 996

      call conv4_8 (t(1,ic),sc,3)
c
c..... Dassault's test case of 03/05/2008: do not overwrite tool axis
c..... if unchanged - to avoid real*4 to real*8 conversion problems
c
      if (lv95 .or. t(4,ic) .ne. savta(1) .or. t(5,ic) .ne. savta(2)
     x         .or. t(6,ic) .ne. savta(3)) then
        call conv4_8 (t(4,ic),sc(4),3)
      endif
      call conv4_8 (t(7,ic),sc(7),3)

      if (lblade) call modfy (sc, sc)
      cptsvd=.false.
c
c... restore last cs u & v values if ta/fan to sf cs
c
      if (lfansf) then
        t(25,ic) = csu1
        t(26,ic) = csv1
      endif
c
c... if motion TANTO the check surface stopped TO the surface,
c... change the forward vector slightly if necessary to make a subsequent
c... GOFWD work.
c
      if (lcsavd) then
        call conv4_8(s(1,1),vpsnrm,3)
        call conv4_8(s(1,2),vdsnrm,3)
        if (ifl(21).eq.-1) call mnvc(vdsnrm)
        call conv4_8(s(1,3),vtmp,3)
        call triple_cross(vpsnrm,vtmp,vpsnrm,vcsnrm)
        dtmp = f_dot(vdsnrm,vcsnrm)
        if (dtmp.gt.-.02d0) then
          dtmp = .01d0
          call intrvec(vcsnrm,vdsnrm,sc(7),dtmp)
        endif
      endif

996   continue
c
c...if anything left in tdat, wrt it now
c...if dntcut, don't call putcl    epm 1-24-84
c
      if(ntk.ge.3) then
         numitm = ntk/mupt
         if (ifl(42).eq.0) then
             call putcl5 (iclass,isubcl,numitm,tdat(1))
         else if (ifl(42) .eq. 1 .and. ifl(246) .ne. 0) then
             call putcl5 (iclass+100,isubcl,numitm,tdat(1))
         endif
         ntk = 0
      endif

999   continue
      if (domov .and. ifl(2).gt.0 .and. itry.eq.0 .and. ifl(330).eq.1
     *    .and. csdis0.lt.ctol) itfl0 = 1
c
c... slowdn   logic                13-may-85
c
      if (ifl(2) .eq. 466) go to 99998
      if(ifl(213).eq.0) goto 9998
c
c...vp 1.14.94 old FR/AT if version < 8.209
c
      if (.not.lv82) go to 9998
      if(isltim.ne.1) goto 9994
c           slow tim1. issue f/r slow and prep for last move.
      isltim=2
      fslow=sc(126)
      if(iabs(ifl(213)).eq.2)fslow=sc(126)*sc(123)
c
      if(ifl(42).eq.0.and.ifl(95).eq.0) call putcl(2000,1009,2,fslow)
      itfl=0
      dp=sc(125)*.6
c                also reduce dpmax              28-mar-86
      if(dpmax.gt.dp)dpmax=dp
      goto 100
c           slowdn tim2. issue primary f/r and resto flags if 'once'
c
9994  if(ifl(42).eq.0.and.ifl(95).eq.0)call putcl(2000,1009,2,sc(123))
      if(sc(123).eq.0.)ifl(2)=274
      if(ifl(213).ge.0)goto 9998
      if(ifl(95).ne.0)goto 9998
      ifl(213)=ifl(214)
      sc(125)=sc(127)
      sc(126)=sc(128)
9998  continue
c
c          if print/small, disply last loc
c
      if (ifl(154).eq.0) then
         if (ifl(104).eq.1) then
              call modfy(tbn,tb)
              call tdsply(lnk,tbn,jptk)
         else
              call tdsply(lnk,tb,jptk)
         endif
      endif
c
c... if genpts/chkpts & circ interp, save last point
c
      if (ifl(246).gt.0.and.ifl(95).eq.1 .and. lv92)
     1      call savmot(sc,sc(7))
99998 if (ifl(2).lt.1) goto 99999
c
c      if (ifl(42).ne.0) goto 99999
c..... qar 96259: always remove the failed motion points from the CL-file
c
c
c...Changed to new clfile storage logic
c...Bobby  -  12/18/92
c
      call ncl_eqlptr(isv2,imotp,iflg)
      if (iflg .eq. 0) call cldel (iclf,isv2,iept,imotp)
      call conv8_8 (sc,tb,9)
cc      call plotm (tb,.false.)
      call ptclos
cc      call moters
cc      call motbgn
c
c... restore dntcut to entry state
c
99999 ifl(42)=kdntcu
      ifl(104) = isv104

      if (lttcsf .or. llttcs) then
c
c..... normally, mocomb drives several times toward the same common
c..... boundary - so we reuse it, and free it only at the end of mocomb
c
         if (lv91 .or. .not.mocom) call ncl_2sfbnry_free
         lttcsf = .false.
      endif

      if (cntctp) then
        ifl(23)  = iflsv(1)
        ifl(282) = iflsv(2)
        if (iflsv(3) .eq. 1) lmdnow = .true.
        call conv4_4 (svtool,tool,6)

        if (ifl(2).lt.1) then
          call conv8_8 (tb,sc,6)
          call conv4_8 (t(1,ic),svcpt,6)
          cptsvd = .true.
        endif
      endif

cc      if (lexpcl) then
      if (.not. lv92) then
        tonp = sc(36)
        hldctp(25) = s(1,3)*tonp
        hldctp(26) = s(2,3)*tonp
        hldctp(27) = s(3,3)*tonp

        if (jd(401).eq.PLANE) then
          d1 = (s(1,3)*tb(1)+s(2,3)*tb(2)+s(3,3)*tb(3)-s(4,3))*tonp
          hldctp(22) = tb(1)-hldctp(25)*d1
          hldctp(23) = tb(2)-hldctp(26)*d1
          hldctp(24) = tb(3)-hldctp(27)*d1
        else

          if (jd(401).eq.CURVE .or. jd(401).eq.CIRCLE
     *    .or.jd(401).eq.LINE) then
            h = t(27,ic)
            s(8,3)  = tb(1)+tb(4)*h
            s(9,3)  = tb(2)+tb(5)*h
            s(10,3) = tb(3)+tb(6)*h
          endif

          d1 = f_dot4 (s(1,3),s(8,3)) - s(4,3)
          hldctp(22) = s(8,3)-s(1,3)*d1
          hldctp(23) = s(9,3)-s(2,3)*d1
          hldctp(24) = s(10,3)-s(3,3)*d1
        endif

        call conv8_8 (tb,hldctp,21)
      endif

1020  format(3f10.3)

      if (DEBUGX .ne. 0) then
          write (dbuf,7001) ifl(2),sc(1),sc(2),sc(3),sc(4),sc(5),sc(6)
 7001     format ('Mover-O = ',i3,' - ',6f10.3)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

1085  initds = 0
      ifl(289) = ifl289
      ishrp = -1
      l2csrf = .false.
      lhavecspl = .false.
      icsgck = icsgcksav
c      sc(223)=0

      return
      end
