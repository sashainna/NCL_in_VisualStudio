C*********************************************************************
C*    NAME         :  endpas.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       endpas.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:01
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : endpas
C*       this routine controls the passend steps
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
      subroutine endpas(kret,rdp)

      include 'com.com'
      include 'mocom.com'
      include 'csrel.com'

      integer*2 kret
      real*4 rdp

      integer*2 jd(600)
      real*4 ad(300),asc63
      integer*2 itfl,iptk,ia,ib,ic
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic)
      real*4 asc(100)
      equivalence (sc,asc),(sc(168),ctol),(asc(63),asc63)

      integer*2 ierr,i,nwds,ietype,mod,n
      real*4 prj(3),ve(3)
      real*4 dp,cdis,odis,co,cal,sal,crwn,cutmax,u,v,bar,sec
      real*4 f_dot4,f_mag4

      integer*4 kcs
      real*8 ctol,ftol,tlrad,dis,dis1,dis2,dsthk
      real*8 ta(9),s8(10),s8sav(10),ht(4),tasav(6),plfwd(4)
      real*8 vec(3),pt(3),pt1(3)
      real*8 dt1,dt2,sbe,cbe,tbe,crad,thgt,cute
      real*8 f_dot, f_dist, f_mag

      common/tantocsl/llttcs
      logical lv90,lv91,lv93,lv97,lsrf,llttcs
c
c...Debug Variables
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)

      lv90 = sc(169).lt.9.049d0
      lv91 = sc(169).lt.9.149d0
      lv93 = sc(169).lt.9.349d0
      lv97 = sc(169).lt.9.749d0
      mod = ifl(23)
      lavdd = .false.
      dp = t(10,ia)
      if (sc(169).lt.8.20799) then
        ftol = sc(27)
      else
        ftol = ctol
        t(25,ib)=t(25,ia)
        t(26,ib)=t(26,ia)
        t(27,ib)=t(27,ia)
      endif
c            include tanto cs 2nd find of cs'pl             17-mar-83
c             itfl is held on 3 more times and tool then released.
      if(ifl(102).eq.0)goto 9
      itfl=1
      kret=1
      rdp=dp
      ifl(102)=ifl(102)-1
      if (ifl(102).eq.0) itfl=0
      goto 99
9     continue
c          turn cs nearfl on.  (no harm if not used)
      if (ifl(58).ne.0) ifl(83)=1
c          bump itfl at entry, set kret go, p/u cdis & odis
      itfl=itfl+1
      kret=1
      cdis=t(12,ia)
      odis=t(12,ic)
      co=t(7,ia)*s(1,3)+t(8,ia)*s(2,3)+t(9,ia)*s(3,3)
      if (abs(co).lt..001) co=.001
c          if itfl large this is re-entry
      if (itfl.gt.10) goto 20
c          check crown ht for all passes except 1st (checked in mover).
c          prev only checked crwn ht on second pass.         ijd 17-jun-88
      if (itfl.gt.1) goto 15
c          first entry.  see if ok to o/p this loc
10    if (cdis.gt..25.or.cdis.gt.odis/4.) goto 25
c          nogo.  shorten dp and re-calc.
      if (cdis.lt.0.0 .and. lcspt) goto 25
      kret=0
      rdp=(dp+cdis/co)/2.
      if (rdp.gt.dp*.75) rdp=dp*.75
      itfl=11
      goto 99
c
c          this is first look at end location from midpt.  if the
c          step exceeds crownh due to unexpected curvature, try a
c          1-time backout from endup.                 7-30-82
c          allow backout 20 times                 ijd 17-jun-88
15    if (ifl(28).gt.19) goto 30
      ifl(28)=ifl(28)+1
      cal=t(7,ia)*t(7,ic)+t(8,ia)*t(8,ic)+t(9,ia)*t(9,ic)
      if (abs(cal).gt.1.) cal=1.
      sal=sqrt(1.-cal**2)
      crwn=dp*sal/8.
c          allow 2*toler this final step.
      if (crwn.le.2.*sc(27)) goto 30
c***********   abandon this endup try.  (itfl = 0)
      itfl=0
      kret=0
c              reduce dp some.  ( mover will recalc dp as nec )
      rdp=dp/4.
      goto 99
20    itfl=1
c          calc dp to endloc
25    rdp=cdis/co
      if (rdp.gt.0.) goto 99
c          improve final step length as nec.
30    if (abs(cdis).gt.ctol) goto 32
c
c... jingrong 10/13/99 if fan and tlaxis not // cs, backout once
c
      if (.not.lv90 .and. mod.eq.4 .and. ifl(291).eq.0
     *       .and. sc(36).ne.0) then
c
c..... the check above makes sense only if CS condition is not 'ON'
c..... DASSAULT MFGNC229
c
         cal = asc63 - sc(36)* f_dot4(t(4,ia),s(1,3))
         if (cal.gt.0.001) goto 32
      endif
      itfl=5
      goto 60
c               if this end case is unreal, backout 1-time   3-may-88
32    continue
      if (ifl(291).ne.0) goto 40
      if (itfl.ne.4) goto 40
      if (abs(co).gt..50) goto 40
      if (sc(36).ne.1.) goto 40
      ifl(291)=1
      itfl=0
      kret=1
      if (cdis.lt.0.) kret=0
      rdp=dp
      goto 99
40    rdp=cdis/co+dp
      if (rdp.lt.0..and.itfl.lt.3) rdp=sc(27)/10.d0
      kret=0
      if(itfl.lt.5.or.abs(cdis).lt.ftol) goto 99
      if (ifl(291).gt.20) goto 50
      ifl(291)=ifl(291)+1
      itfl=4
      goto 99
50    if(abs(cdis).gt.sc(27)*3..and..not.lcspt) ifl(2)=256
c
c... jingrong 11/22/99 if fan and tlaxis not // cs, raise err.
c
      if (.not.lv90 .and. mod.eq.4 .and. .not.lcspt
     *       .and. sc(36).ne.0) then
c
c..... the check above makes sense only if CS condition is not 'ON'
c..... DASSAULT MFGNC229
c
         cal = asc63 - sc(36)* f_dot4(t(4,ia),s(1,3))
         if (abs(cal).gt.0.001) ifl(2) = 256
      end if
c
c...   If checking to a point, abandon this endup try if point is not
c...   within the cutter envelope.
c
60    if (lcspt) then
        cutmax = sc(28)/2.0d0
        if (cutmax.lt.tool(1)/2.0) cutmax = tool(1)/2.0
        if (sc(174).gt.cutmax+sc(27)*3.0) then
          itfl = 0
          kret = 1
          rdp = dp
          ifl(327) = 1
          ifl(28)  = 0
          ifl(291) = 0
        endif
      endif
c
c...jingrong 01/15/99 Add additional end check to allow U-turn
c...in TANTO CS mode. If pt1 not in cutter twice diameter envelop,
c...abadon this endup try.
c
      if (lv90) goto 99
c
c..... the flag llttcs is used here since lttcsf is unset after the
c..... first pass of fan and the wrong stop can still occur during the 
c..... second pass. DASSAULT MFGNC229
c
      if (llttcs .and. (lv91 .or. .not.lcsavd)) then
c
c..... do not do this check if tool stops at a real CS, and not at a 
c..... CS-DS common boundary. the flag lcsavd is set in csrelx
c
          labandon = .false.
          dis = 0
          dis1 = 0
          call conv4_8 (t(1,ia),ta,9)
          call ncl_ttcssf(ta,dis1,pt1,vec,ierr,ta(7))
          pt(1)=ta(1)+tool(3)*ta(4)
          pt(2)=ta(2)+tool(3)*ta(5)
          pt(3)=ta(3)+tool(3)*ta(6)
          call ncl_ttcssf(pt,dis,pt1,vec,ierr,ta(7))
          if (dis1 .lt. dis) dis = dis1
c
c..... take DS thickness into account: tool should be ON CS plane, but
c..... about dsthk away from the common boundary
c
          dsthk = sc(24)
          if (.not.lv93 .and. (2*sc(24)+tool(1)).lt.0)
     *      dsthk = - sc(24) - tool(1)
          dis = dis - dsthk
          if (mod.eq.3 .or. mod.eq.6 .and. mocom .and. .not.lv93) then
            dis1 = dis + dsthk - sc(25)
            if (dis1 .lt. dis) dis = dis1
          endif
c
c..... Replaced the "abandon" condition below with a much less
c..... restrictive one: if we do a fan tanto CS, with the common
c..... boundary on a straight line (ierr=-1). This fixes CATIA's
c..... NCL166. The number 0.55 is an arbitrary guess: in NCL166
c..... the dis was 0.71*tool(1), and it should be about
c..... equal to 0.5*tool(1) at the "real" end
c         if (dis .gt. 2.*tool(1)) labandon=.true.
c
c...Calculate diameter at tool axis contact point
c...Otherwise when driving small diameter surfaces
c...tool could stop prematureley - NCCS-27-09-2010-1.cpp
c...Bobby  -  8/31/10
c
          if (tool(3) .le. tool(2) .or. lv97) then
              dia = tool(1)
          else
              dia = sc(28) + ((tool(1)-sc(28)) *
     1              (tool(10)-tool(2)) / (tool(3)-tool(2)))
              if (dia .le. 0) dia = tool(1)
          endif
          if (ierr.eq.-1 .and. ifl(282).eq.0 .and.
     *       mod.eq.4 .and..not.lv91) then
c            bar = 0.55*dia
c..... changed from 0.55 to 0.6 - CATIA NCL199
c..... it had tool(1)=25.,sc(24)=4.,dis=18.11
c
             bar = 0.6*dia
          else
             bar = 2.*dia
          endif

          if (.not.lv93 .and. ierr.eq.-1 .and. ifl(282).eq.0 .and.
     *        jd(1).eq.9 .and. .not.psmult .and. dis.le.bar) then
            sec = 0.5*tool(1) - 5.*sc(27)
            if (sec .lt. 5*sc(27)) sec = 5*sc(27)
            if (dis.gt.sec) then
              dis1 = dis
c
c..... dis1 is less than bar, i.e., at this distance we would not abandon
c..... to check if we should:
c..... is the tool far below or above the common boundary?
c
              call cbdist(ta,pt,dis,pt1,bar,ta(7))
              if (dis .gt. bar) then
                call conv4_8(s(1,1),s8sav,10)

                vec(1) = s(5,1) - pt1(1)
                vec(2) = s(6,1) - pt1(2)
                vec(3) = s(7,1) - pt1(3)
                call unitvc(vec,vec)

                i = 1
                call sfptvc(sc(144),pt1,vec,i,t(13,ia),t(14,ia),ierr)
                if (ierr .gt. 0) then
                  dis = dis1
                else
                  vec(1) = s(5,1) - s8sav(5)
                  vec(2) = s(6,1) - s8sav(6)
                  vec(3) = s(7,1) - s8sav(7)
c
c..... is the common boundary point PS projection far from the current PS tool
c..... projection?
c..... abandon if both conditions hold
c
                  dis2 = f_mag(vec)
                  if (dis2 .lt. bar) dis = dis1
                endif
                call conv8_4(s8sav,s(1,1),10)
              endif
            endif
          endif

          if (dis .gt. bar) labandon=.true.
          if (labandon) then
c
c..... the flag ifl(146) is used in csrel in the apex point logic
c..... DASSAULT MFGNC229
c
               if (ifl(80).eq.0 .and. mod.eq.4) ifl(146) = 1
               kret = 1
               itfl = 0
               rdp = dp
               ifl(28) = 0
               ifl(291) = 0
          endif
          goto 99
      endif
c
c... Ignore check surface extension if AVOID is set
c
      if (.not. lavoid) goto 99
c
c... jingrong if TNTO or ON should not be avoid
c
      if (sc(36).eq.0.) goto 99
c
c... Save off tool and surface values
c
      call conv4_8(t(1,ia),tasav,6)
      call conv4_8(t(7,ia),plfwd,3)
      call conv4_8(s(1,3),s8sav,10)
      call conv4_8(t(1,ia),ta,6)
      sbe  = asc(63)
      cbe  = asc(64)
      if (cbe.eq.0.0d0) cbe = 1.0d0
      tbe  = sbe/cbe
c
c... Expand tool by 2 times tolerance
c
      ftol = sc(27)*2.0d0
      crad = tool(2)+ftol
      thgt = tool(3)+ftol*2
      cute = tool(6)
      if (.not. lv91) then
        cute = cute+ftol
        if (lautgf) cute = cute+sc(24)
      endif
      call uvcplvc (ta,ta(4),ta,-ftol)
c
c... Create 3 heights up tool to project onto cs
c
      ht(1) = ftol
      ht(2) = tool(3)/2.0d0
      ht(3) = tool(3)
c
c... Zero tool axis to stay out of corner logic in usurfpn
c
      lsrf = jd(401).eq.SURF .or. jd(401).eq.21
      if (lsrf) then
        t(4,ia) = 0.0
        t(5,ia) = 0.0
        t(6,ia) = 0.0
      endif
c
c... dt1 is const of plane through te perpto ta
c... dt2 is height up tool axis where corner radius ends
c
      dt1 = f_dot(ta,ta(4))
      dt2 = crad*(1-sbe)
c
c... Initialize variables.
c
      ifl(54) = 3
      u = t(25,ia)
      v = t(26,ia)
      labandon = .true.
      call gtdesc (sc(13),kcs,nwds,ietype)
      n = 3
      if (.not.lsrf) n = 1
c
c... Try points up tool axis until we find tool contacting real cs
c
      do 80 i=1,n
        if (labandon) then
           call uvcplvc (ta,ta(4),s8(8),ht(i))
           call conv8_4(s8(8),s(8,3),3)
c
c... If cs is a sf.
c
          if (lsrf) then
             call surfpn(u,v,1)
             call conv4_8(s(1,3),s8,7)
             goto 100
c
c... cs is a cv
c
          else if (jd(401).eq.CURVE) then
             call curvpv(s(5,3),s(6,3),s(7,3),ve(1),ve(2),ve(3))
             if (ifl(2).gt.0) ifl(2) = 256
c
c... cs is a circle
c
          else if (jd(401).eq.CIRCLE) then
             call circpv(s(5,3),s(6,3),s(7,3),ve(1),ve(2),ve(3))
             call ncl_reproj_on_lnci (s(5,3),kcs)
c
c... cs is a line
c
          else if (jd(401).eq.LINE) then
             call conv8_4(d(106),ve,3)
             sec = f_mag4(ve)
             if (sec.eq.0) then
                ifl(2) = 256
             else
                call unitvc4(ve,ve)
                call vctovc4(s(8,3),prj)
                call ncl_reproj_on_lnci (prj,kcs)
                call vctovc4(prj,s(5,3))
             end if
          else
             labandon = .false.
             goto 80
          end if
c
c...csn is crossf v & ta
c
            call f_cross4(ve,t(4,ia),ve)
c
c... if sec zero, some special case exists.   ta//v?
c
            sec = f_mag4(ve)
            if(sec.eq.0.) then
                 ifl(2) = 256
                 goto 80
            end if
            if (f_dot4(ve,t(7,ia)).lt.0) call mnvc4(ve)
            call unitvc4(ve,s(1,3))
            s(4,3) = f_dot4(s(1,3),s(5,3))
            call conv4_8(s(1,3),s8,7)
100   continue
c
c... Project surface point onto tool axis
c
          dis1 = f_dot(s8(8),s8)-s8(4)
          call uvcplvc (s8(5),s8,pt1,dis1)
c
c... Determine if surface pt projects onto tool between top and bottom.
c
          dis1 = f_dot(pt1,ta(4))-dt1
          if (dis1.ge.0.0d0 .and. dis1.le.thgt) then
c
c... Calculate radius of tool at point on axis that surface point projects to
c
            if (dis1.ge.dt2.or.(.not.lsrf.and..not.lcntct)) then
              tlrad = cute+crad*cbe+(dis1-dt2)*tbe
            else
              dis  = crad**2 - (dt2-dis1+crad*sbe)**2
              tlrad = 0.0d0
              if (dis.gt.0.0d0) tlrad = dsqrt(dis)+cute
            endif
c
c... jingrong if tool is away from cs, on extension.
c
            dis2 = f_dist(s8(5),pt1)
            labandon = dis2.gt.(dsqrt(2.d0)*tlrad)
            if (.not.labandon) then
c
c... Project surface point onto plane through external point perpto forward
c
               plfwd(4) = f_dot(s8(8),plfwd)
               call plndis(plfwd,s8(5),dis)
               call uvcplvc (s8(5),plfwd,s8(5),-dis)
c
c... Calc distance from projected point to tool axis & compare to tool radius
c
               call ptlnds (s8(5),ta,dis)
               labandon = dis.gt.tlrad
            endif
          endif
        endif
80    continue
c
c... If tool does not intersect real surface, abandon this endup try.
c
      if (labandon) then
          kret = 1
          itfl = 0
          rdp = dp
          if (cdis.gt.-ftol) t(12,ia) = -ftol
          ifl(28) = 0
          ifl(291) = 0
          lavdd    = .true.
      end if
c
c... Restore tool and surface tables.
c
      call conv8_4(tasav,t(1,ia),6)
      call conv8_4(s8sav,s(1,3),10)
c
99    continue
      if (DEBUGX .eq. 1) then
          write (dbuf,7000) dis,dis1,labandon,itfl,kret
 7000     format ('dis = ',f11.4,'  dis1 = ',f11.4,'  labandon = ',i2
     1            '  itfl = ',i3,'  kret = ',i2)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : endps0(ifl0)
C*       Short version of endpas - just tells if the tool is within
C*       tolerance to CS.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          itfl0 - 5 iff within tolerance
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine endps0(itfl0)

      include 'com.com'
      include 'mocom.com'

      real*4 asc63
      integer*2 ia,ib,ic
      real*4 asc(100)
      equivalence (sc,asc),(sc(168),ctol),(asc(63),asc63)
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic)

      integer*2 ierr,mod
      real*4 cdis,cal,cutmax,bar
      real*4 f_dot4

      real*8 ctol,dis,dis1
      real*8 ta(9)
      real*8 vec(3),pt(3),pt1(3)

      common/tantocsl/llttcs
      logical llttcs

      if (lavoid) return
      mod = ifl(23)

      cdis=t(12,ia)
      if (abs(cdis).gt.ctol) return

c
c... if fan and tlaxis not // cs and CS condition is not 'ON' - exit
c
      if (mod.eq.4 .and. sc(36).ne.0) then
         cal = asc63 - sc(36)* f_dot4(t(4,ia),s(1,3))
         if (cal.gt.0.001) return
      endif
      itfl0 = 5
c
c...   If checking to a point, abandon this endup try if point is not
c...   within the cutter envelope.
c
      if (lcspt) then
        cutmax = sc(28)/2.0d0
        if (cutmax.lt.tool(1)/2.0) cutmax = tool(1)/2.0
        if (sc(174).gt.cutmax+sc(27)*3.0) then
          itfl0 = 0
          return
        endif
      endif
c
c...jingrong 01/15/99 Add additional end check to allow U-turn
c...in TANTO CS mode. If pt1 not in cutter twice diameter envelop,
c...abadon this endup try.
c
c
c..... the flag llttcs is used here since lttcsf is unset after the
c..... first pass of fan and the wrong stop can still occur during the 
c..... second pass. DASSAULT MFGNC229
c
      if (llttcs .and. .not.lcsavd) then
c
c..... do not do this check if tool stops at a real CS, and not at a 
c..... CS-DS common boundary. the flag lcsavd is set in csrelx
c
          dis = 0
          dis1 = 0
          call conv4_8 (t(1,ia),ta,9)
          call ncl_ttcssf(ta,dis1,pt1,vec,ierr,ta(7))
          pt(1)=ta(1)+tool(3)*ta(4)
          pt(2)=ta(2)+tool(3)*ta(5)
          pt(3)=ta(3)+tool(3)*ta(6)
          call ncl_ttcssf(pt,dis,pt1,vec,ierr,ta(7))
          if (dis1 .lt. dis) dis = dis1
c
c..... take DS thickness into account: tool should be ON CS plane, but
c..... about dsthk away from the common boundary
c
          dis = dis - sc(24)
          if (mod.eq.3 .or. mod.eq.6 .and. mocom) then
            dis1 = dis + sc(24) - sc(25)
            if (dis1 .lt. dis) dis = dis1
          endif
c
c..... Replaced the "abandon" condition below with a much less
c..... restrictive one: if we do a fan tanto CS, with the common
c..... boundary on a straight line (ierr=-1). This fixes CATIA's
c..... NCL166. The number 0.55 is an arbitrary guess: in NCL166
c..... the dis was 0.71*tool(1), and it should be about
c..... equal to 0.5*tool(1) at the "real" end
c         if (dis .gt. 2.*tool(1)) labandon=.true.
c
          if (ierr.eq.-1 .and. ifl(282).eq.0 .and. mod.eq.4) then
             bar = 0.6*tool(1)
          else
             bar = 2.*tool(1)
          endif
          if (dis .gt. bar) itfl0 = 0
      endif

      return
      end
