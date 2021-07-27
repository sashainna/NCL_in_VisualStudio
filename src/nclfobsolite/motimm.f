C*********************************************************************
C*    NAME         :  motimm.f
c*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       motimm.f , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*       10/27/16 , 13:54:08
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine motimm
c*     1.   this routine does the immediate motion related tasks.
c*
c*     2.   task commands are:
c*
c*        1)    601  tlon,l,r    no action. ifl(21) already set.
c*        2)    701  from
c*        3)    703  goto
c*        4)    710  godlta
c*        5)    711  iv
c*        6)    712  ip
c*        7)    713  psis
c*        8)    717  thick
c*        9)    721  ta
c*       10)    731  toler
c*       11)    803  cut     xeq even if zero move. (may be tracut)
c*
c*              575  new dist or vec def per subr sfptdo        1-jul-85
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
      subroutine motimm

      include 'com8a.com'
      include 'comgt.com'
      include 'const.com'
      include 'mocom.com'
      include 'drvcom.com'
c          mocom equivs
      real*4 srf(10,3)
      equivalence (srf,s)

      common/tpvcom/tavend
      real*8 tavend(3)

      integer*2 i2sc18(4)
      integer*4 jsc(200)
      real*4 ad(300),bsn(2),u,v
      real*8 bj(35),e(7),
     *       tol,tcvasw,dis,f_mag,f_dot
      integer*2 ksn(4),kc(4),kd(600),ift(10)
      equivalence (asn,bsn,ksn),(jb,bj),(sc,jsc),(c,kc),(d,ad,kd)
      equivalence (i2sc18,sc(108))
      logical lflg,lv84,lv91
      integer*4 nclkey, kcps, ncl_psmult_init
      integer*2 nwds,ietype,imult,iscv,iprim
      equivalence (tool(19),tcvasw)
      equivalence (ifl(337),kcps)
      real*4 asc(400)
      equivalence (sc,asc)

      data ift/601,701,703,710,711,712,713,717,721,731/
      integer*2 izro /0/
      integer*2 ifl331

      lv84 = sc(169).lt.8.4999d00
      lv91 = sc(169).lt.9.149d0
      ifl331 = ifl(331)

      asn=sc(10)
      mtsk=ksn(1)
      msub=ksn(2)
      nw=ksn(3)
c
c... Autops
c
      if (mtsk.eq.745) then
        sc(35) = ZERO
        d(1)   = ZERO
        d(2)   = ZERO
        kd(1)  = 6
        call vctovc(sc(4),d(3))
        d(6) = f_dot(d(3),sc(1))
        goto 999
      endif

c          assume clfile subclass=5  (only exception is 3 for from)
      isubcl=5
      if(mtsk.ne.803) goto 2
c          if cut, go write in clfile. do not update fwd.
      itask = 11
      if (ifl(213).eq.0) goto 160
c          set up values for secondary feedrate.
      xd=sc(1)-sc(141)
      yd=sc(2)-sc(142)
      zd=sc(3)-sc(143)
      dis=dsqrt(xd**2+yd**2+zd**2)
      if (dis.lt.0.01d0) goto 160
      xd=xd/dis
      yd=yd/dis
      zd=zd/dis
      goto 154

c 29-jan-85                     slowdn fedrat commands
c                  ifl(213) is islow indicator
c                          = -2    slo/once/scale
c                          = -1    slo/once
c                          =  0    no slowdn
c                          =  1    slo/modal
c                          =  2    slo/modal/scale
c                  ifl(214) holds mem of ifl(213) for restore after once.
c
c                  sc(123) holds primary fedrat
c                  sc(125)   "   dslow
c                  sc(126)   "   fslow (or scale)
c                  sc(127)   "   memory of dslow  (for once restores)
c                  sc(128)   "     "    "  fslow    "   "      "
c
2     if(mtsk.ne.1009)goto 9
      if(sc(11).gt.0.)goto 3
c                   fr/at,0       turn islow off
      ifl(213)=0
      goto 999
c                   slowdn f/r reqs a real primary f/r  28-mar-86
3     if(sc(123).gt.0.)goto 4
      ifl(2)=274
      goto 999

4     i=ifl(213)
      ifl(213)=1
c                   nw is a misnomer and means 'once' if real.
      if(nw.eq.0)goto 5
c                   set islow=-1 and sto old data
      ifl(214)=i
      ifl(213)=-1
      sc(127)=sc(125)
      sc(128)=sc(126)
5     sc(125)=sc(11)
      sc(126)=sc(12)
c                   if scale, set ifl(213) to +/- 2
      if(msub.eq.1) ifl(213)=ifl(213)*2
      goto 999
9     continue
c 29-jan-85 end

c++ 1-jul-85          new dist and vec defs per pt1,sf1   1-jul-85
      if(mtsk.ne.575) goto 11
      call sfptdo (e)
c                     if no error, set ifl(1)=1 for prcntl branch
      if(ifl(2).lt.1)ifl(1)=1
      goto 999
11    continue
c++ 1-jul-85 end

c          find task/branch
      do 10 i=1,10
      itask=i
      if(mtsk.eq.ift(i))goto 20
10    continue

c          unknown task
      ifl(2)=120
15    err=.true.
      goto 999

c          tn  fr  gt  gd  iv  ip  ps  th  ta  tol
c20    goto(999,100,200,300,400,500,600,700,800,900),itask
20     if (itask .eq. 1) goto 999
       if (itask .eq. 2) goto 100
       if (itask .eq. 3) goto 200
       if (itask .eq. 4) goto 300
       if (itask .eq. 5) goto 400
       if (itask .eq. 6) goto 500
       if (itask .eq. 7) goto 600
       if (itask .eq. 8) goto 700
       if (itask .eq. 9) goto 800
       if (itask .eq. 10) goto 900
c
c***************************************************** from (701)
100   isubcl=3
c          turn on first from flag if this is first motion
      if (i2sc18(2).ne.0) go to 110
      i2sc18(2)=2
c              fr/x,y  ,   fr/x,y,z    or   from/pt1
 110  continue
      if (msub.eq.1) go to 140
      dis = f_mag(sc(14))
c          do not accept very small values in ta/a,b,c
      if(dis.gt..01d0)goto 120
c          error. tlaxis i,j,k values too small
      ifl(2)=123
      goto 15

120   call vctmsc(sc(14),sc(4),ONE/dis)
140   call vctovc(sc(11),sc(1))
      cptsvd = cntctp .and. ifl(280).eq.1
      autouv = auvset .and. ifl(280).eq.1
c
c          display ending point to window 3
150   if (motdfl) then
          write (cout,151)
151       format (12x,'x',11x,'y',11x,'z',11x,'i',11x,'j',11x,'k')
          call putmsg (cout,80,15,0)
          call conv8_8(sc,bj,6)
          if (ifl(73).eq.1.and.ifl(267).eq.1) then
            call conent(bj,sc(41),3)
            call conent(bj(4),sc(41),4)
          endif
          write (cout,1511) (bj(i),i=1,6)
1511      format (4x,3f12.4,3f12.6)
          call putmsg (cout,80,16,0)
          cout=' '
c   
c...Added check for NCL-VT mode    
c...Paul  -  10/3/91    
c...Old version was:    
c   if (.not.ksr.and.ifl(35).eq.0) call ersw3 (17,1)
c
          if (.not.ksr .and. (ifl(35) .eq. 0 .or. ifl(35) .eq. 2) 
     *       .and. ifl(131) .eq. 0)   call ersw3 (17,1)
      endif
c          output to clfile unless dntcut is in effect (using bj array)
c
c...DNTCUT in effect
c
      if (ifl(42) .eq. 1 .and. ifl(246) .ne. 0) then
          itask = 2
          go to 160
      else if (ifl(42).ne.0) then
          go to 999
      endif
c
c...Version 8.209+ FEDRAT/AT...OUT logic
c
      if (sc(169) .gt. 8.229) go to 160 
c           check for slowdn                        30-jan-85
      if(ifl(213).eq.0)goto 160
c           also skip slowdn action if 'from'
      if(itask.eq.2)goto 160
154   dslow=sc(125)
c          if dis gt dslow, calc intpt and goto at primary f/r
      if(dis.le.dslow+.002) goto 155
      bj(1)=sc(1)-xd*dslow
      bj(2)=sc(2)-yd*dslow
      bj(3)=sc(3)-zd*dslow
      call vctovc(sc(4),bj(4))
      call putcl(5000,isubcl,1,bj)
c          issue slowdn fedrat
155   fslow=sc(126)
      if(iabs(ifl(213)).eq.2)fslow=sc(126)*sc(123)
      if(isubcl.ne.3) call putcl(2000,1009,2,fslow)

c             load ndpt in bj( ) and output to clfile
160   if (sc(169) .gt. 8.20899) then
          call ssdist (itask)
cc          call plotm (sc,.false.)
          go to 999
      end if
c
      call conv8_8(sc,bj,6)
cc      call plotm (bj,.false.)
      call putcl(5000,isubcl,1,bj(1))
c             if this was slowdn, issue primary f/r record
      if(ifl(213).eq.0)goto 999
165   if(sc(123).gt.0.)goto 167
c             error. zero fedrate will not be output.
      ifl(2)=274
      goto 999
167   if(isubcl.ne.3) call putcl(2000,1009,2,sc(123))
c              if 'once', restore to prior state
      if(ifl(213).ge.0)goto 999
      if(itask.eq.2)goto 999
      ifl(213)=ifl(214)
      sc(125)=sc(127)
      sc(126)=sc(128)
c
c...End of version 8.209- (old FEDRAT/AT logic).
c
      goto 999

c **************************************************** goto (703)
c          set direc nums
200   xd=sc(11)-sc(1)
      yd=sc(12)-sc(2)
      zd=sc(13)-sc(3)
c          update fwd unless small move or parallel tlaxis
      dis=dsqrt(xd**2+yd**2+zd**2)
      if(dis.gt..001)goto 260
c          move too small to chg fwd
      goto 270
260   xd=xd/dis
      yd=yd/dis
      zd=zd/dis
      co=dabs(xd*sc(4)+yd*sc(5)+zd*sc(6))
      if(co.gt..99999d0)goto 270
      sc(7)=xd
      sc(8)=yd
      sc(9)=zd
c          turn off iv flag  ( fwd is now corrupted)    27-may-86
      ifl(22)=0
c          goto  clfile write  area
270   goto 110

c****************************************************  godlta (710)
300   if(msub.eq.2)goto 320
      if(msub.eq.3)goto 330
      if(msub.eq.4)goto 340
      if(nw.eq.2)sc(13)=0.
      call vcplvc(sc(11),sc(1),sc(11))
      goto 200

c          gd/a
320   dis=sc(11)
321   continue

      if (lblade) then
        sv16 = tool(16)
        sv17 = tool(17)
        tool(16) = asc(305)
        tool(17) = sqrt(1.0-tool(16)**2)
        call modfy (bj,sc)
        tool(16) = sv16
        tool(17) = sv17
        ti = bj(4)
        tj = bj(5)
        tk = bj(6)
      else
        ti = sc(4)
        tj = sc(5)
        tk = sc(6)
      endif

      sc(11)=sc(1)+dis*ti
      sc(12)=sc(2)+dis*tj
      sc(13)=sc(3)+dis*tk
c
c... if dis is neg, reverse direction vector for sec feed rate.
c
      td=1
      if (dis.lt.ZERO) td = -1
      xd=ti*td
      yd=tj*td
      zd=tk*td
      dis = dabs(dis)
      goto 140

330   call gtdesc(sc(11),nclkey,nwds,ietype)
      if (ietype.eq.PLANE) goto 331
c
c... gd/sf1                    9-Sep-93
c... Move tool down ta and go gt/pt route.
c
      call vctovc(sc,e)
      psmult = .false.
      ifl(331) = 0
c
c... Use multiple part surface code for versions 8.5 & later
c
      if (.not.lv84) then
c
c... aak 05-dec-1997: initialize multi-PS as net surface;
c
        call gettol (tol)
        iscv = 0
        ifl(2) = ncl_psmult_init (nclkey,tol,imult,iscv)

        if (ifl(2).ne.0 .and. ifl(2).ne.466) ifl(2) = 122
        if (ifl(2).gt.0) then
           err = .true.
           return
        endif

        psmult = imult.eq.1
      endif

      call psinit (sc(11), e, sc(4))
c
c..... Eduard 5/21/1999. Changed the call below for compatibility with
c..... razpok.f
c     call tltosf (e, sc(4), e(4), e(4))
c
      call tltosf (e, sc(4), e(4), e(4),sc(144))
      if (ifl(2).gt.0) goto 999
c
c... if sf primitive is a plane, set ps to plane.
c
      if (ifl(46).le.0 .and. .not.lv91) then
        call ncl_get_sf_primtyp(nclkey,iprim)
        if (iprim.eq.3) then
          call gtplt (sc(11),izro,d(3))
          c     = 0.0d0
          kc(1) = PLANE
          d(1)  = c
          d(2)  = sc(11)
        endif
      endif
      xd = e(1)-sc(1)
      yd = e(2)-sc(2)
      zd = e(3)-sc(3)
      sc(11) = sc(1)+xd
      sc(12) = sc(2)+yd
      sc(13) = sc(3)+zd
      dis = dsqrt(xd**2+yd**2+zd**2)
      goto 140
c          gd/pl1                          12-jan-84
c            calc dis and go gd/a route
331   call gtgeom(sc(11),e,nclkey,nwds,ietype)
      co = f_dot(e,sc(4))
      if(dabs(co).gt.0.1) goto 332
c          error. pl is nearly parlel ta
      ifl(2)=254
      goto 999

332   td=e(4)-f_dot(e,sc)
      dis=td/co
      goto 321
c                                  godlta/ve    24-sep-86
c340   call gtgeom (sc(11),e,nclkey,nwds,ietype)
c
c...vp 21-apr-93 PV added, vector part only
c
340   call gtgeve(sc(11),e,nclkey,nwds,ietype)
      xd=e(1)
      yd=e(2)
      zd=e(3)
      sc(11)=sc(1)+xd
      sc(12)=sc(2)+yd
      sc(13)=sc(3)+zd
      dis=dsqrt(xd**2+yd**2+zd**2)
      goto 140

c****************************************************  iv (711)
400   dis= f_mag (sc(11))
c          input vector must have length
      if(dis.gt.0.001d0)goto 402
      ifl(2) = 121
      goto 15

402   call vctmsc (sc(11),sc(7),ONE/dis)
c          turn on ivflg. will be turned off after use.
      ifl(22) = 1
      goto 999

c****************************************************  ip (712)
500   call vcmnvc (sc(11),sc(1),sc(11))
      goto 400
c
c****************************  psis (713)
c
600   continue
c
c... get ps into d-tbl.   record this ps in sc(35)
c
      fautops = .false.
      sc(35) = sc(11)
      c      = sc(11)
      call gtdesc (sc(11), nclkey, nwds, ietype)
      ifl4(17) = nclkey
      if (ifl(46).le.0 .and. ietype.eq.SURF .and. .not.lv91) then
        call ncl_get_sf_primtyp(nclkey,ietype)
        if (ietype.eq.3) kc(4) = PLANE
      endif
c
c... bra on pl or sf
c
      if(kc(4).eq.PLANE) then
         call gtplt (sc(11),izro,d(3))
         kc(1) = PLANE
         d(1)  = c
         d(2)  = sc(11)
         ad(2) = ZERO
         ifl4(17) = 0
         kcps  = 0
         return
      endif
c
c...must be surf or curve on surface
c
      if(kc(4).ne.SURF .and. kc(4).ne.CURVE .and. kc(4).ne.NETSF) then
         ifl(2) = 122
         err    = .true.
         return
      endif

c     **************************** surf
c
c              get surf hed into d(1)
660   continue
c
c...Put asn in sc(144) instead of d(2). d(2) used for offset surfaces.
c
      sc(144) = sc(11)
      i = 1
      u = .5
      v = .5
      psmult = .false.
c
c......vp 97.06.17
c......ps is defined by cv on ds, now sc(11) must contain ds id 
c
      call gtdesc (sc(11), nclkey, nwds, ietype)
c
c... aak 04-feb-1998:
c... if ps.key = 0 -> error: "Illegal geometry type for part surface";
c... may be useful for OpenNCL
c
      if (nclkey.eq.0) then
         ifl(2) = 122
         err = .true.
         return
      endif

      call gettol (tol)

      if (ietype.eq.SURF .or. ietype.eq.NETSF) then
c
c... Use multiple part surface code for versions 8.5 & later
c
         if (.not.lv84) then
c
c... aak 05-dec-1997: initialize multi-PS as net surface;
c
           iscv = 0
           ifl(2) = ncl_psmult_init (nclkey,tol,imult,iscv)

           if (ifl(2).ne.0 .and. ifl(2).ne.466) ifl(2) = 122
           if (ifl(2).gt.0) then
              err = .true.
              return
           endif

           psmult = imult.eq.1
         endif
         call sfinit (sc(11), i, u, v)
         kcps = 0
      else
c
c... aak 18-nov-1997: decide if to drive a CVonSf as multiple PS
c... cvoneps = lfl(104): 
c... if false(default), we want to drive it as one (not multiple) PS.
c
c... aak 05-dec-1997: initialize multi-PS as composite SSPLINE on DS
c
         iscv = 1
         ifl(2) = ncl_psmult_init (nclkey,tol,imult,iscv)

         if (ifl(2).ne.0 .and. ifl(2).ne.466) ifl(2) = 122
         if (ifl(2).gt.0) then
            err = .true.
            return
         endif

         psmult = imult.eq.1.and.cvoneps
         if(psmult) call ncl_cvonsf_free
         kcps     = nclkey
         c        = ZERO
         kc(1)    = SURF
         d(1)     = c
      end if

      ad(2) = ZERO
      return

c****************************************************  th (717)
700   continue

      if(msub.eq.2) then
c
c... th/off
c
         call xyzvc (ZERO,ZERO,ZERO,sc(23))
      else
c
c... repeat the last to sc(14) and move all to sc(23+)
c
         call fill_array (sc(11+nw),4-nw,sc(10+nw))
         call conv8_8 (sc(11),sc(23),4)
      endif
      return

c****************************************************  ta (721)
800   lflg = .false.
c              if ta/ , , ,modify   set modfy flag and put
c                  sc(15-18) in tool(12-14). calc ijk in t-sys
c                  tool(15-17)
      ifl(104)=0
      ifl(293)=0
c      lmdnow = .false.
      if(nw.eq.0)goto 802
      
      if (nw.gt.1) then
          if ((sc(20).eq.0.0).and.(sc(21).eq.0.0)) then
              lmdnow = .false.
              goto 802
          endif
        cosa=dcos(sc(20)/RADIAN)
        sina=dsin(sc(20)/RADIAN)
        cosb=dcos(sc(21)/RADIAN)
        sinb=dsin(sc(21)/RADIAN)
        dcx=sina*cosb
        dcy=sinb*cosa
        dcz=cosb*cosa
        sec=dsqrt(dcx**2+dcy**2+dcz**2)
        if(sec.lt.1.d-6) then
          ifl(2) = 121
          goto 15
        endif
        sc(81)=dcx/sec
        sc(82)=dcy/sec
        sc(83)=dcz/sec
        lmdnow = .true.
        goto 802
      endif
      
      if((sc(15).eq.0.0).and.(sc(16).eq.0.0).and.(sc(17).eq.0.0)
     x         .and.(sc(18).eq.0.0).and.(sc(19).eq.0.0)) then
              ifl(104) = 0
      else
          ifl(104)=1
          call conv8_4(sc(15),tool(12),3)
          cosa=dcos(sc(18)/RADIAN)
          sina=dsin(sc(18)/RADIAN)
          cosb=dcos(sc(19)/RADIAN)
          sinb=dsin(sc(19)/RADIAN)
          dcx=sina*cosb
          dcy=sinb*cosa
          dcz=cosb*cosa
          sec=dsqrt(dcx**2+dcy**2+dcz**2)
          if(sec.gt.1.d-6)goto 801
c          error. degen case.   (rpl-fpl do not intersect)
c              turn modfy off, error on and exit.
          ifl(2)=121
          goto 15

801       tool(15)=dcx/sec
          tool(16)=dcy/sec
          tool(17)=dcz/sec
      endif

802   if (msub.eq.1) goto 810
      if (msub.eq.2) goto 820
      if (msub.eq.3) goto 830
      if (msub.eq.4) goto 840
      if (msub.eq.5) goto 850
      if (msub.eq.6) goto 860
      if (msub.eq.7) goto 870
      if (msub.eq.8) goto 880
      if (msub.eq.9) goto 890
      if (msub.eq.10) goto 890
      if (msub.eq.11) goto 830
      if (msub.eq.12) goto 830
      if (msub.eq.13) goto 830
      if (msub.eq.14) goto 891
      if (msub.eq.15) goto 892
      if (msub.eq.0)  return
c          error. unknown ta/mode
      ifl(2)=126
      goto 15

c********* ta/i,j,k  or  ta/ve1   ( note -- means ta/same ) 7-20-81

810   dis=dsqrt(sc(11)**2+sc(12)**2+sc(13)**2)
c          do not accept very small values in ta/a,b,c
      if(dis.gt..01d0)goto 812
c          error. tlaxis i,j,k values too small
      ifl(2)=123
      goto 15

812   call vctmsc(sc(11),sc(4),1.d0/dis)
c      *** note ***   ta/same is now in effect
      ifl(23)=0
c                     set ta/ve1,normal flag
      ifl(293)=ksn(4)
      goto 895
c
c... ta/interp,ve1
c
892   ifl(23) = 15
      call unitvc (sc(11), tavend)
      goto 895
c
c... ta/same or ta/normal,ps
c
820   ifl(23)=sc(11)
c                     set ta/same,normal flag
      ifl(293)=ksn(4)
      goto 895

c*********  ta/atangl,alf,ps  (ta leans alf degrees fwd from psnorm)
c...vp 22-apr-93 get VE or PV in e(1-3)
c
830   IF (MSUB.NE.11.AND.MSUB.NE.13) GOTO 835
c
c...Bobby
c
      if (ifl(330) .eq. 1) then
          call vctovc(sc(87),e(1))
      else
          call gtgeve(sc(12),e,nclkey,nwds,ietype)
      endif

      SEC = f_mag(e)
      IF(SEC.LT..001)GOTO 861
835   IFL(23)=MSUB-1
      TOOL(8)=DSIN(SC(11)/RADIAN)
      TOOL(9)=DCOS(SC(11)/RADIAN)
      IF (MSUB.GT.11) TOOL(10) = SC(13)
      if (sc(14).eq.1.) lflg = .true.
      IF (MSUB.NE.11.AND.MSUB.NE.13) GOTO 895
         call vctmsc(e,e,ONE/sec)
         call conv8_4(e,tool(18),3)
      GOTO 895

c*********  ta/tanto,ds,hin
840   ifl(23)=3
      tool(10)=sc(11)
c              unset l/r indicator.  (this points to ps contact pt)
      tool(11)=0.
      goto 895

c*********  ta/,,,fan                24-jan-83
850   ifl(23)  = 4
      tool(10) = sc(11)
      tool(11) = 0.
      goto 895

c********   ta/tanto,ds,hin,perpto,v1      29-feb-84
c...vp 22-apr-93 get VE or PV in e(1-3)
c
860   asn=sc(12)
c
c...get v1 and unitize
c...Bobby
c
      if (ifl(330) .eq. 1) then
          call vctovc(sc(87),e)
      else
          call gtgeve(asn,e,nclkey,nwds,ietype)
      endif

      sec = f_mag(e)
      if(sec.gt.1.d-6)goto 862
c
c... error.  v1 numbers too small
c
861   ifl(2) = 121
      goto 999
c                add hin and v1 to tool tbl
862   ifl(23)  = 5
      tool(10) = sc(11)
      goto 885
c**********      ta/tanto,ds,hin,parelm                     23-jul-86
870   ifl(23)  = 6
      tool(10) = sc(11)
      goto 895

880   asn=sc(12)
c
c...vp 22-apr-93 get VE or PV in e(1-3)
c...Bobby
c
      if (ifl(330) .eq. 1) then
          call vctovc(sc(87),e)
      else
          call gtgeve(asn,e,nclkey,nwds,ietype)
      endif

      sec=f_mag(e)
      if(sec.lt..001)goto 861
      ifl(23) = 7
 885  continue
      call vctmsc(e,e,ONE/sec)
      call conv8_4(e,tool(18),3)
      goto 895
C*********        TA/COMBIN,HIN,ADIS,BDIS                 21-APR-89
C                   MSUB =9  MIDRANGE IS TANDS
C                     "  =10     "     " PARELM
890   ifl(23)  = msub-1
      tool(10) = sc(11)
C                   STO ADIS,BDIS IN TOOL(18,19)
      tool(18) = sc(12)
      tool(19) = sc(13)
      goto 895
c
c...              ta/THRU,[pt1|cv1]
c
891   if (isc10(4).eq.0) then
       ifl(23) = 13
       call conv8_4(sc(11),tool(18),3)
      else
       ifl(23)  = 14
       tcvasw   = sc(11)
       tool(18) = sc(12)
      endif

895   cntctp = lflg
      goto 999
c***************************************************** toler (731)
c          min toler is .000001
900   sc(27) = sc(11)
      if(sc(27).lt.1.d-6) sc(27) = 1.d-6
      sc(167) = sc(27)
      sc(168) = .0001
      if (isc10(2).eq.1) then
        sc(167) = sc(12)
        if (sc(167).lt.1.e-8)  sc(167) = 1.e-8
        if (sc(167).gt.sc(27)) sc(167) = sc(27)
        sc(168) = sc(167)
      endif

999   continue
      ifl(331) = ifl331
    
      return
      end
