C*********************************************************************
C*    NAME         :  premov.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       premov.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       03/01/16 , 11:16:03
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine premov
c*          This routine is called by mocntl to prepare for basic move.         
c*                                                                 
c*          1.  calc init fwd from prior fwd-up senses plus        
c*              --fwd,lft,rgt,back-- in go***/ command. 
c*                                                                 
c*          2.  load ds and cs in d-tbl (ps already there)         
c*                                                                 
c*          3.  set ps 'up' and ds 'rgt'                           
c*                                                                 
c*          4.  set calc depth levels for ps,ds,cs  ifl(56,57,58)  
c*                                                                 
c*          5.  prepare tcol(3) for mover entry.                   
c*                                                                 
c*                                                                 
c*         chg 12-dec-84  1)  set cbe =1. if sbe 0                    
c*                        2)  error if non-cyl cutter and to/past     
c*                            a ln,ci,cv check surface                
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
      subroutine premov

      include 'com8a.com'
      include 'const.com'
      include 'mocom.com'
      include 'drvcom.com'
      include 'suvcom.com'

c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)

      common/savcuv/cuv
      real*4 cuv(10)

      common/dcpcom/svcpt(6)
      real*8 svcpt

      real*4 ac(2),ad(300),asc(400),sbe,cbe,cutn,u,v
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      integer*2 ksn(4),kc(4),jd(600)
      equivalence(c,ac,kc),(d,ad,jd),(ifl(54),isrf),(kcps,ifl(337))
      integer*4 ka(2),kb(2),kcps
      equivalence(asn,ksn),(b,kb),(asn,ka)
      real*8 ss(7)
      real*4 fx,fy,fz,fl, hds, f_dot4,tx,px,py,pz,dot1
      real*8 co,ad2, tol1, tol2,primdat(16)
      integer*4 locn,nclkey, kcs,strlen1
      integer*2 nwds,ietype, itcs,primtyp
      logical lv84,lv91,lv96

      lv84 = sc(169).ge.8.3999
      lv91 = sc(169).lt.9.149
      lv96 = sc(169).lt.9.649
c
c... get NCL tolerance; gettol, not getsct
c
      call gettol (tol1)

      c = 0.
      lttcrv  = .false.
      lttcsf  = .false.
      tantods = .false.
c
c... unset the cir int flag      11-10-81
c
      ifl(95)=0
c
c... unset the endpas re-try flag  7-30-82
c
      ifl(28)=0
c
c... also spec case endup backout flag   3-may-88
c
      ifl(291)=0
      ifl(102)=0
c
c... unset fan pass1 flag        14-jun-84
c
      ifl(80)=0
c
c... if psis quilt srf , set itfl=-3    3-oct-84
c
      if(jd(1).eq.QUILTSF) ifl(49)=-3
c
c... circle draw iter count =0          5-oct-84
c
      ifl(130)=0
c
c... make sure cbe is ok               12-dec-84
c
      if(sbe.eq.0.) cbe=1.
c
c... prepare init fwd sense.
c
      asn=sc(10)
c
c... GOUP or GODOWN, set fwd to tool axis
c
      if (ksn(1).eq.GOUP.or.ksn(1).eq.GODOWN) then
        fx = sc(4)
        fy = sc(5)
        fz = sc(6)
        if(ksn(1).eq.GOUP) goto 20
        goto 15
      endif

      if(ksn(1).eq.GOLFT.or.ksn(1).eq.GORGT) goto 10
c
c... if gf or gb, pickup last fwd from sc.
c
      fx=sc(7)
      fy=sc(8)
      fz=sc(9)
c
c... if command was 'gofwd', fxyz holds fwd sense.
c... if 'goback', reverse it.
c
      if(ksn(1).eq.GOFWD) goto 20
      goto 15
c              gl or gr.   calc a rgt sense
10    fx=sc(6)*sc(8)-sc(5)*sc(9)
      fy=sc(4)*sc(9)-sc(6)*sc(7)
      fz=sc(5)*sc(7)-sc(4)*sc(8)
      if(ksn(1).eq.GORGT)goto 20
c              reverse the current fwd  (for gl and gb)
15    fx=-fx
      fy=-fy
      fz=-fz
c          check for real and unitize
20    fl=sqrt(fx**2+fy**2+fz**2)
c
c.....error.  initl move direction indefinite
c
      if(fl.le.0.001) then
         err    = .true.
         ifl(2) = 117
         return
      endif
c              add fwd sense to t-tbl(col3)
25    t(7,3)=fx/fl
      t(8,3)=fy/fl
      t(9,3)=fz/fl
c
c...also load te and ta from sc
c
      if (cptsvd.and.cntctp) then
         call conv8_4 (svcpt,t(1,3),6)
      else
         call conv8_4 (sc,t(1,3),6)
      endif
c
c... set to,on,past cs  per sc(12): sc(36) = -1, 0, +1
c... also tancs   11-9-81
c
      if(sc(12).eq.TNTO.or.sc(12).eq.PSTAN) goto 30

      if (sc(12).eq.CS_TO)  then 
         sc(36) = -ONE
      else if (sc(12).eq.CS_ON) then
         sc(36) = ZERO
      else if (sc(12).eq.CS_PAST) then
         sc(36) = ONE
      else
c
c...error. invalid endup modifier
c
         ifl(2) = 29
         return
      endif
c
c... get ds & cs (if not already in place)
c... point to sc word, index to d-tbl
c
30    ixx=1
      lcspt = .false.

32    ix=50*ixx
      kdx=ix*4
      isx=ixx*mxsfhd
      iscx=ixx*2+9
      asn=sc(iscx)
      sc(144+ixx)=asn
c
c... note.  must always get entity.  may have redefined.
c... bra on  pl-ln , ci-cv , surf
c
      nw=ksn(3)
      if(ksn(4).eq.PLANE  .or. ksn(4).eq.LINE)  goto 40
      if(ksn(4).eq.CIRCLE .or. ksn(4).eq.CURVE) goto 44
      if(ksn(4).eq.SURF .or. ksn(4) .eq. NETSF) goto 34

      if (ixx.eq.2 .and. ksn(4).eq.POINT) then
        lcspt = .true.
        jd(kdx+1)=ksn(4)
        call gtgeom(asn,d(ix+15),nclkey,nwds,ietype)
        goto 50
      endif
c
c... illegal entity type for ds, cs
c
      ifl(2) = 118
      err    = .true.
      return
c
c...surface.
c
34    continue
      jd(kdx+1)=ksn(4)
c
c.....if cs, init u,v=.5 nin t-tbl        29-apr-86
c
39    if(ixx.eq.1) goto 50
      t(25,3)=csu
      t(26,3)=csv
      isrf=3
      call sfinit (sc(146), isrf, t(25,3), t(26,3))
      goto 50
c
c              pl/ln
40    continue
c
c... note:  if ksn(1)=0, this is a call from regmil and
c...        pl is already in d-tbl.             17-mar-87
c
      if(ksn(1).ne.0.or.ksn(2).ne.0)
     *    call gtgeom(asn,d(ix+3),nclkey,nwds,ietype)
c
c...add this asn to d(2) for ident purposes
c
      d(ix+2)=asn
c
c...add ityp to d(1)
c
      kc(1)=ksn(4)
      kc(2)=0
      d(ix+1)=c
c
c... also load cs pl in srf(_,3)
c
      if(iscx.eq.11.or.ksn(4).eq.5) goto 50
      do 42 m=1,4
      n=ix+2+m
42    srf(m,3)=d(n)
      goto 50
c
c... if not circle, must be curve
c
44    if(ksn(4).eq.CURVE) goto 48
c
c...circle. get it now.
c
      call gtgeom(asn,d(ix+3),nclkey,nwds,ietype)
      kc(1)=7
      kc(2)=0
c              go use cv finup logic.
      goto 49
c              curve.  set ityp,nmp and iseg=0 but do not get
c                       item now. segs will activate later.
48    kc(1)=ksn(4)
c
c... get nsegs for NCL curve.
c
      call gtdesc(asn,nclkey,nwds,ietype)
      call isitwf (nclkey, iwf)
      if (iwf.eq.0) call gtncsg(nclkey,kc(2))

49    kc(3)=0
      d(ix+1)=c
      d(ix+2)=asn

50    continue

      ixx=ixx+1
      if(ixx.lt.3)goto 32
      asn = sc(14)
c
c...vp 11/10/97 near point specified, set cs parameters
c...using projection of this point on cs.
c... aak aug-14-98: only if CS is a surface
c
      if (ksn(4).eq.POINT) then
        lcspt = .true.
        call gtgeom(asn,d(ix+15),nclkey,nwds,ietype)
        if (jd(401).eq.SURF) call set_cspt
      endif
c
c... Initialize driving trimmed surface as trimmed.
c
      do 502 i = 1,3
          ifl(330+i) = 0
          if (ifl(361).eq.1) goto 502
          if (ifl(340).eq.2) goto 502

          call gtdesc(sc(143+i),nclkey,nwds,ietype)

          if (ietype.eq.SURF) then
            ifl(330+i) = ifl(340)
            call tbdini (nclkey, i, tol1, ifl(330+i))
            if (ifl(2).eq.466) return
          endif

502   continue
c
c... TANTO,CS or TANTO,PS
c
      if(sc(12).ne.TNTO .and. sc(12).ne.PSTAN) goto 51
      call gtdesc(sc(146),kcs,nwds,itcs)

      if (sc(12).eq.TNTO) then
        tantods = .true.
        call gtdesc (sc(145),nclkey,nwds,ietype)
      else
        call gtdesc(sc(144),nclkey,nwds,ietype)
        if (ietype.ne.SURF.or.itcs.ne.SURF) goto 9172
      endif

      if (ietype.eq.SURF.or.itcs.eq.SURF) then
        if (ietype.ne.SURF.or.itcs.ne.SURF) goto 9172
        lttcsf = .true.
        sc(36) = ZERO
c
c...Tolerance * 5 is too large
c...(especially when the surface is only tol*4 wide)
c...so changed it, though it may still be tool large
c...Catia - NCL171 3rd motion
c...Bobby  -  3/6/00
c
        if (lv91) then
          tol2 = tol1*5.0
        else
          tol2 = tol1*3.0
        endif
        call ncl_2sfbnry (nclkey, kcs, tol1, tol2, ifl(2))
        if (ifl(2).eq.466) go to 99
      else if (jd(201).eq.CURVE .or. jd(401).eq.CURVE) then
        if (jd(201) .eq. PLANE .or. jd(401) .eq. PLANE) then
          ifl(2) = 173
          err    = .true.
        else
          call ttcrv (sc(11), sc(13), ss)
          call conv8_4 (ss,srf(1,3),7)
        endif
      else
        call tanto
      endif

      if(ifl(2).gt.0) return
c
c... set direc's for ps and ds  (if surfs)
c... ps first
c
51    if(jd(1).ne.PLANE) goto 60
c
c... psis pl ( direct it up )
c
      if (lv84 .and. ad(2).ne.0.0) goto 54
      ad(2) = 1.0
      co = t(4,3)*d(3)+t(5,3)*d(4)+t(6,3)*d(5)
      if (.not.lv84) then
        if (co.gt.0.0) goto 54
      else
c
c...   If ta is within 6 degrees of ps, keep te pt on current side,
c...   otherwise ta points away from ps.
c
        if (co.gt.0.1) goto 54
        if (co.gt.-0.1) then
          if (t(1,3)*d(3)+t(2,3)*d(4)+t(3,3)*d(5)-d(6).gt.0.) goto 54
        endif
      endif

      do 52 i=3,6
52    d(i)=-d(i)
c
c...add this pl to srf(isrf)
c
54    continue

      call conv8_4 (d(3),srf(1,1),4)
      goto 70
c
c... ps is a surf
c... ( psis/ ln,cv,ci is now a syntax error. chg in future )
c
60    continue
      ad2 = ad(2)
      isrf=1
      ifl(51)=3
c
c... use ps 1st-look pt
c
		call uvcplvc4 (t(1,3),t(4,3),srf(8,1),tool(4))
      u = psu
      v = psv
c
c...vp 97.06.17
c...cv on ds is part surface
c
      if (kcps .eq. 0) then
         call sfinit (sc(144), isrf, u, v)
         if (lv84.and.ad2.ne.ZERO) ad(2) = ad2
         call surfpn (u,v,1)
      else
         call cvonsf_init (kcps,isrf) 
         if (ifl(2) .ne. 0) return
         call surfpn (u,v,1)
      end if

      if (ifl(2) .eq. 466) return

      t(13,3)=u
      t(14,3)=v
c
c...   Initialize ps u & v for gougck.
c
      if (ifl(6).gt.0) then
        do 61 i=1,9,2
        cuv(i)   = u
61      cuv(i+1) = v
      endif
c
c... if psnorm not up, reverse direc(1) and ps tanpl.
c
      co = f_dot4 (t(4,3), srf(1,1))

      if (.not.lv84) then
        if (co.gt.ZERO) goto 67
      else
        if (ad2.ne.ZERO) goto 67
        if (co.gt.0.1) goto 67
        if (co.gt.-0.1 .and.
     *      f_dot4(t(1,3),srf(1,1))-srf(4,1).gt.ZERO ) goto 67
      endif

      ad(2)=-ad(2)

      call mnvc4 (srf(1,1))
      srf(4,1) = - srf(4,1)

67    continue
c
c... ditto ds.   ( no action if ln,cv )
c
70    i=jd(201)
      if(i.eq.LINE .or. i.eq.CURVE) goto 80
c
c... ds is circle. call cir int check routine and skip rgt chk
c
      if(i.eq.CIRCLE) then
         call cintck (d(56))
         goto 80
c
c...Allow for circular interplotion
c...when driving a cylinder
c
      else if (i .eq. SURF .and. .not. lv91) then
        call gtdesc(sc(145),nclkey,nwds,ietype)
        call gtprimt(nclkey,0,primtyp,primdat)
        if (primtyp .eq. 5) then
          call cintck (primdat(4))
        endif
      endif
c
c... do a rgt sense
c
      rx=t(8,3)*t(6,3)-t(9,3)*t(5,3)
      ry=t(9,3)*t(4,3)-t(7,3)*t(6,3)
      rz=t(7,3)*t(5,3)-t(8,3)*t(4,3)

      if (rx**2+ry**2+rz**2 .lt. .01) then
        rx = t(8,3)*srf(3,1)-t(9,3)*srf(2,1)
        ry = t(9,3)*srf(1,1)-t(7,3)*srf(3,1)
        rz = t(7,3)*srf(2,1)-t(8,3)*srf(1,1)
      endif

      if(jd(201).ne.PLANE) goto 75
c          dsis pl
      if(rx*d(53)+ry*d(54)+rz*d(55).gt.0.)goto 73
c          flip ds pl
      do 72 i=53,56
72    d(i)=-d(i)
c          also in srf tbl
73    continue
      call conv8_4 (d(53),srf(1,2),4)
      goto 80
c
c... dsis sf
c
75    ad(102)=1.
      isrf=2
      ifl(51)=3
      rldinu = .false.
c
c.....use ds 1st-look pt
c
c..... added ifl(288): while ifl(21) ne 0 means DS attribute is not TLON in a
c..... gf (gb,gr,gl,...) statement, ifl(288) ne 0 means same for a GO 
c..... statement (set in strtup) - Technology Answers
c
      IF (lv91 .or. ((IFL(21).NE.0 .or. ifl(288).ne.0) .and. 
     *     TOOL(1).gt.0. .and. (ifl(23).le.2.or.ifl(23).ge.7))) then
        hds = tool(5)
      else if (ifl(23).gt.2.and.ifl(23).lt.7) then
        hds = tool(10)
      else
        hds = 0.
      endif
      call uvcplvc4 (t(1,3),t(4,3),srf(8,2),hds)
c     call uvcplvc4 (t(1,3),t(4,3),srf(8,2),tool(5))
      u = dsu
      v = dsv
      call sfinit (sc(145), isrf, u, v)
      call surfpn(u,v,1)
      if (ifl(2) .eq. 466) return
      t(19,3)=u
      t(20,3)=v

      sfa=srf(1,2)
      sfb=srf(2,2)
      sfc=srf(3,2)
c
c.....flip direc(2) and vec in srf( ,2))
c
c
c...There is a problem when the PS & DS
c...are almost parallel
c...So check the forward vector with the
c...calculated forward vector (cross(ps,ds))
c...to determine if we need to flip the DS normal
c...QAR 97309
c...Bobby - 10/19/09
c
      tx = rx*sfa+ry*sfb+rz*sfc
      if (lv96) then
          dot1 = 1.
      else
          px = srf(3,2)*srf(2,1)-srf(2,2)*srf(3,2)
          py = srf(1,2)*srf(3,1)-srf(3,2)*srf(1,2)
          pz = srf(2,2)*srf(1,1)-srf(1,2)*srf(2,2)
          dot1 = px*t(7,3)+py*t(8,3)+pz*t(9,3)
      endif
      if(tx .le. 0. .or. (tx .le .1.e-8 .and. dot1 .le. 0.)) then
         ad(102) = -ad(102)
         call mnvc4 (srf(1,2))
         srf(4,2) = -srf(4,2)
      endif

80    continue
c          init zero dp
      t(10,3)=0.
      t(10,2)=0.
c          set ps,ds,cs calc depths    ifl(56,57,58)
c              ps. if ta/normal,ps or ball-end cutter, pslev=0
      ifl(56) = 0
      t(15,3) = tool(2)
c
c... If tlonps, use level zero.
c
      if (ifl(342).eq.1) goto 82
      if (asc(66).eq.1.) goto 81
      if(ifl(23).eq.1.or.tool(6).lt..002)goto 82
81    t(16,3)=0.
      t(17,3)=0.
      t(18,3)=0.
      ifl(56)=1
c              ds. if tlon or cutter/0, dslev=0
82    ifl(57)=0
c              if cutmode=6 ds must be rldsrf        23-jul-86
      if (ifl(23).ne.6) goto 829
      if (jd(201).eq.NETSF) goto 829
      call gtdesc(sc(145), nclkey, nwds, ietype)
c
c... jingrong 5/4/99 if ta/tanto,ds,parelm & ds=pl,change to ta/tanto,ds.
c
      if (ietype.eq.PLANE) then
          ifl(23)=3
          goto 829
      end if

      if (ietype.ne.SURF) then
         ifl(2) = 308
         return
      endif

      call gtrld (nclkey, i)
      if (i.ge.0.or..not.lv84) then
        rldinu = i.eq.1
      else
        fx = t(22,3)
        fy = t(23,3)
        fz = t(24,3)
        fl = sqrt(fx**2+fy**2+fz**2)
        if (fl.eq.0.) fl = 1.
        co1 = (fx*t(4,3)+fy*t(5,3)+fz*t(6,3))/fl
        rldinu = .true.
        call surfpn(u,v,1)
        if (ifl(2) .eq. 466) go to 99
        fx = t(22,3)
        fy = t(23,3)
        fz = t(24,3)
        fl = sqrt(fx**2+fy**2+fz**2)
        if (fl.eq.0.) fl = 1.
        co2 = (fx*t(4,3)+fy*t(5,3)+fz*t(6,3))/fl
        rldinu = dabs(co1).lt.dabs(co2)
      endif
829   continue 
c              fan + ln,ci,cv ds is ng               29-may-85
      if(ifl(23).ne.4)goto 83
C              DO NOT ALLOW FAN + BARREL TOOL AT THIS TIME   08-FEB-88
CCCCCCC      IF(IFL(282).NE.0) IFL(2)=354
      IF(IFL(2).GT.0)GOTO 99
      k=jd(201)
      if(k.eq.LINE .or. k.eq.CIRCLE .or. k.eq.CURVE) ifl(2)=279
83    continue
c           ifl(27)=0,1  for ds convex/concave to cutter.
c             convex assumed at pass start.
      ifl(27)=0
      t(21,3)=0.
C                IF TANDS MODE AND BARREL TOOL, HIN MUST BE ABOVE OR AT
C                R2 TO FLAT SIDE CONTACT PT.                     16-DEC-87
      IF(IFL(23).LT.3.OR.IFL(23).GT.6)GOTO 832
      T(21,3)=TOOL(10)
      IF(IFL(282).EQ.0.OR.IFL(21).EQ.0)GOTO 832
      SAL=ASC(305)
      CAL=DSQRT(1.-SAL**2)
C                ERROR. HGT IN TA/TANTO,DS CMD TOO SMALL FOR THIS CUTTER.
      HCALC=sc(29) - sc(29)*asc(63)
      IF(TOOL(10).lt.HCALC-.001) then
        IFL(2)=353
        GOTO 99
      endif

832   IF (IFL(21).NE.0 .and. TOOL(1).gt.0. .and.
     *     (ifl(23).le.2.or.ifl(23).ge.7)) then
         t(21,3)=tool(5)
         ifl(57)=1
      endif

c              cs. if 'on' cs or cutter/0, cslev=0
      if (sc(36).eq.0..or.tool(1).eq.0.) then
         ifl(58)=0
         t(27,3)=0.
      else
         ifl(58)=1
         t(27,3)=tool(5)
         if (csmult) t(27,3)=hu(1)
      endif
c              set nearflg off           9-15-81
      ifl(83)=0
ccc      if ta/fan, simple cs apply and hcs=hin     24-jan-83
c     init a far apxpt
      if(ifl(23).ne.4)goto 865
C               IF BARREL TOOL, FAN, AND NOT 'ON' CS, HIN MUST
C               CORRESPOND TO STRAIGHT SIDE OF TOOL.         10-FEB-88
      IF(IFL(282).EQ.0.OR.SC(36).EQ.0.)GOTO 862
      SAL=ASC(305)
      CAL=DSQRT(1.-SAL**2)
C                ERROR. HGT IN TA/TANTO,DS CMD TOO SMALL FOR THIS CUTTER.
      HCALC=sc(29) - sc(29)*asc(63)
      IF(TOOL(10).lt.HCALC-.001) then
        IFL(2)=353
        GOTO 99
      endif
      HCALC=ASC(68)+ASC(67)*SAL/CAL
862   CONTINUE
c               ln,ci,cv cs ng with fan                    29-may-85
      k=jd(401)
      if(k.eq.LINE .or. k.eq.CIRCLE .or. k.eq.CURVE) ifl(2)=279
c               if cs not a plane, set for 2-pass run      14-jun-84
      if(jd(401).ne.PLANE) ifl(80)=1
      ifl(58)=0
      t(27,3)=tool(10)
      t(28,3)=t(1,3)-t(4,3)*1.e6
      t(29,3)=t(2,3)-t(5,3)*1.e6
      t(30,3)=t(3,3)-t(6,3)*1.e6
cxyz                      apxpt also in tcols 1 and 2            4-may-87
      do 864 l=1,2
      do 863 m=27,30
863   t(m,l)=t(m,3)
864   continue

865   continue
c                 add checks for disk cutter + tanto,ds and fan  4-jun-84
      if(asc(66).ne.1.)goto 875
      if(ifl(23).lt.3.or.ifl(23).gt.5)goto 875
c                 disk cutter + tanto/ds reqs tlon
      if(ifl(21).eq.0)goto 867
c                   error.
         ifl(2)=262
         return
867   if(ifl(23).ne.4.or.sc(36).eq.0.)goto 875
c                 error. disk tool + fan reqs 'on', cs
         ifl(2)=263
         return
875   continue
ccc
c             error if non-cyl barrel tool and to or past a ln,ci,cv
      if(sbe.eq.0.or.ifl(282).eq.0.or.sc(36).eq.0.)goto 877
      k=jd(401)
      if(k.ne.5.and.k.ne.7.and.k.ne.8)goto 877
      ifl(2)=30
      goto 99
877   continue
c------------   debugm.  allow display to start at loc(n)
      if(ifl(85).eq.0.or.ifl(2).gt.0) return
      write(cout,88)
      call swinpt()
      call opnwin()
      call putmsg(cout,80,23,0)
88    format(' *** premov end.  give loc num to start debugm')
      nccout = strlen1(cout)
      call nclpmt(cout, nccout, cin, nccin)
      call clswin()
      l=0

      do 92 kk=1,6
         if (ain(kk).ge.'0'.and.ain(kk).le.'9') then
             l=l+1
         else
             go to 94
         endif
92    continue

94    call chr2int (cin,l,locn)
      ifl(85)=locn
      if(locn.lt.1)ifl(85)=-1
c------------
99    return
9172  ifl(2) = 172
      goto 99
      end
      
C*********************************************************************
C*    E_SUBROUTINE     : subroutine isdswf
c*         Verify whether the current DS is a wireframe.
c*
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          iwf
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine isdswf (iwf)

      include 'com.com'
      include 'mocom.com'

      integer*2 jd(600)
      real*4 ad(300)
      equivalence (d,ad,jd)
      
      integer*2 iwf
      
      iwf = 0
      if (jd(201).eq.CURVE .or. jd(201).eq.CIRCLE
     *                     .or. jd(201).eq.LINE) then
        iwf  = 1
      endif

      return
      end
