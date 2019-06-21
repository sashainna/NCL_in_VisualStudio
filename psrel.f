c*********************************************************************
C*    NAME         :  psrel.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       psrel.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:29
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine psrel
C*
C*    PURPOSE OF PROGRAM: CALL PSRELA OR PSMOD3. IF GOUGCK IS IN EFFECT
C*                      WITH FLAT BOTTOM CUTTER, CALL PSRELA MULTIPLE
C*                      TIMES & TAKE BEST ONE.
C*
c*       this routine now calls:
c*             psmod3 for cutmode 3
c*             psrela for all others.         5-oct-84
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
      subroutine psrel

      include 'com4a.com'
      include 'const.com'
      include 'mocom.com'
 
      integer*2 itfl, iptk, ia, ib, ic, tanorm
      equivalence (itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic)

      integer*4 kcps
      equivalence (kcps,ifl(337))

      common/savcuv/cuv
      real*4 cuv(10)

      common/lsetad2/setad2
      logical setad2
      
      logical noflip

      common/reset/ reset_dp
      logical reset_dp

      common/lfl6/lfl6
      integer*2 lfl6
c
c... local variables
c
      integer*2 NTOOL,NSRF
      parameter (NTOOL=50, NSRF=20)
      real*8 
     *   tool_struct(NTOOL),surf_struct(NSRF),
     *   sc35,sc144, dtol, dist, psthk,toler,tcn,rad
      real*4 
     *   taxis(3),fd(3),fwd(3),svuv(2),current_ps(7),psrf(3),
     *   vec(3), co,svdis,svco,dis,f_mag4,f_dot4,f_dist4,s1(10),
     *   tsav(3),svsn(3),dis1,svdis1,plfwd(3),co1,svco1, tvec(3)
      integer*4 
     *   ncl_psmult_select, status,ncl_toolstruct_size
      integer*2 
     *   isf,nsf,iuse,iext,isv23,isv6,isv56,icp,icp0,iuv,ncp,
     *   ifl56,iflat,nb,iext2,nsfusd,lttck
      logical 
     *   best,lv84,lv90,lv91,lv92,lv93,lnogck,lnorm,lpnorm
c
c..... hard-coded constants:
c
      real*4 ONE_MIN_EPS/0.99/, EPS1/1.e-04/, EPS2/1.e-02/
c
c...Debug variables
c
      integer*4 DEBUGX
cc      parameter (DEBUGX=3)
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)

      lv84 = sc(169).lt.8.4999d0
      lv90 = sc(169).lt.9.049d0
      lv91 = sc(169).lt.9.149d0
      lv92 = sc(169).lt.9.249d0
      lv93 = sc(169).lt.9.349d0
      ifl56 = 0

      call getsct (toler)

      call vctovc4 (t(4,ia),taxis)

      isv56 = ifl(56)
      isv6  = ifl(6)
      lfl6 = 0
      isv23 = ifl(23)
      lnorm = isv23 .eq. TA_NORMAL_PS .and. .not. lsecps
      lpnorm = isv23 .eq. TA_NORMAL_PS_PERPTO .and. .not. lsecps

      psthk = sc(23)
      if (lnorm .or. (.not.lv91 .and. lpnorm)) psthk = ZERO
      sc35 = sc(35)
      sc144 = sc(144)

      reset_dp = .false.

      nsf  = 1
      dtol = ZERO
      tcn = psthk + tool(2)
c
c...Special circumstances when tool axis is
c...reliant upon part surface normal and
c...multiple part surfaces
c...Bobby  -  3/20/00
c
      if (.not. lv91 .and. psmult .and. tanorm(isv23) .eq. 1 .and.
     1    ifl(6) .gt. 1) then
          lttck = 1
          lnogck = ifl(6).le.1 .or. ifl(342).eq.1
c
c..... ifl(342) = tool on/off PS flag
c
      else
          lttck = 0
          lnogck = ifl(6).le.1 .or. tool(6).lt.2.*toler .or.
     1             ifl(342).eq.1
      endif
c
c... prepare stuff for gouge checking;
c... get fwd vec perpto tlaxis: fwd = [[taxis,t(7,ia)],taxis]
c
c..... nsf = # of part surfaces; 
c
      if (psmult) call ncl_psmult_nsf (nsf)
      if (lv90) then
         lnogck = (ifl(6).le.1 .and. nsf.eq.1) .or. tool(6).lt.2.*toler
      endif

      if (.not.lnogck .or. nsf.gt.1) then
         call triple_cross4 (taxis,t(7,ia),taxis,fwd)
         if ( f_mag4(fwd).ge.EPS1) call unitizevc4 (fwd)
         call f_cross4 (t(7,ia),taxis,plfwd)
         if (f_mag4(plfwd).ge.EPS1) call unitizevc4 (plfwd)
      endif
c
c..... Set up to drive multi PS
c..... turn on the "use" flag for PS's crossing tool cyl
c
      if (nsf.gt.1) then

         if (NTOOL .lt. ncl_toolstruct_size ()) then
            ifl(2) = 199
            return
         endif

         dtol = 5.*toler
         call conv4_4 (s(1,1),current_ps,7)

         call ncl_create_tool (tool_struct,t(1,ia),taxis,t(7,ia),fwd)

         rad = tool(1) + 2.*toler
         if (cntctp) rad = sc(28) + 2.*toler
         if (lv90) then
             rad = sc(28)
             if (rad.lt.dtol) rad = dtol
         endif
         if (psthk.gt.ZERO) rad = rad + psthk

         call ncl_create_tcyl (tool_struct, rad)
c
c......Calculate distance of current step
c......Used to determine if we are skipping over
c......any surfaces
c
         tdelt = f_dist4(t(1,ia),t(1,ic)) / 2.
         call ncl_psmult_chk (tdelt)
      endif

   70 nsfusd = 0
      if (DEBUGX .eq. 3) then
          write (dbuf,7010) t(1,ia),t(2,ia),t(3,ia)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
	if (t(1,ia) .lt. -1.9560 .and. t(1,ia) .gt. -1.9561) then
		i = 10
	endif
      do 100 isf=1,nsf
c
c..... load next multi ps sf, if any
c..... aak 19-nov-1997: kcps = ifl(337); if ! = 0 -> multi-PS is a CVonSF
c
        iuse = 1
        if (kcps.ne.0 .and.psmult) then
           nb = 0
           call ncl_load_curve (isf,iuse,1)
        else if (nsf.gt.1) then
           call psload (isf, iuse, nb)
        endif

        if (iuse.eq.0) goto 100
        nsfusd = nsfusd + 1
c
c..... cutmod 3: call psmod3
c
        if (ifl(23).eq.TA_TANTO_DS .and. .not.lsecps) then
           call psmod3
           goto 95
c
c..... no gougck
c
c        else if (lnogck) then
       else if (lnogck.and.(lv91.or.(.not. lpnorm .and. .not. lnorm)))
     *         then
           if (ifl(147).eq.1 .and. nsf.gt.1) then
c
c..... this initialization (only the first time in psrel) is done for some
c..... consistency; otherwise, e.g., during the second fan we start with the
c..... look vector from the end of the first fan - Dassault NCL117
c
             call vctovc4 (fwd,t(16,ia))
             if (.not.lv92 .and. ifl(342).eq.0 .and.
     1           tool(6).ge.2.*toler) goto 20
           endif
           call psrela
           goto 95
        endif
c
c..... gougck with flat bottom cutter
c
20      call conv4_4 (t(13,ia),svuv,2)
        if (svuv(1).eq.HALF .and. svuv(2).eq.HALF) 
     *      call fill_array4 (cuv,10,0.5)

        ifl(6) = 0
        call vctovc4 (t(16,ia),fd)
c
c..... qar 94121. for an atangl tlaxis mode and gouge check on,
c..... go into psrela with different look points, and pick the best
c..... projection. (otherwise psrela would always use the front
c..... of the cutter)
c
        if (.not.lv93 .and. isv6.ge.3 .and.
     x      (ifl(23).eq.2.or.ifl(23).eq.10.or.
     x       ifl(23).eq.11.or.ifl(23).eq.12)) then
          ifl(23) = 0
        endif
c
c..... if not 'TA/NORMAL,PS', call PSRELA with saved tool bottom vector
c..... for 'TA/NORMAL,PS', call SURFPN with center point
c..... of tool & fix tlaxis to this SF normal
c
      if (lnorm .or. (.not.lv91 .and. lpnorm)) then
         call vctovc4 (t(1,ia),s(8,1))
         call surfpn (cuv(3),cuv(4),0)
         if (ifl(2) .eq. 466) return
         ifl(56) = 1
         ifl(23) = 0
         if (lpnorm) then
            call triple_cross4(tool(18),s(1,1),tool(18),taxis)
            call unitvc4(taxis, taxis)
            call vctovc4(s(1,1),svsn)
         else
            call vctovc4 (s(1,1),taxis)
         end if
         svdis = - HUGE_VAL
         svdis1 = - HUGE_VAL
         svco = ONE
         svco1 = ZERO
         if (lnogck) then
            best = .false.
            goto 201
         end if
      else
         call psrela
         if (ifl(2) .eq. 466) return
         co = f_dot4 (taxis,s(1,1))
         svco = co
         co1 = f_dot4 (plfwd,s(1,1))
         svco1 = abs(co1)
c
c..... correct for thick & corner contact
c
         if (lv84) then
             svdis = f_dot4(taxis,s(5,1)) + (psthk+(1.-co)*tool(2))*co
          else
             svdis1 = co * (s(4,1) + tcn - f_dot4 (s(1,1),s(8,1)))
             svdis = svdis1 - dtol*ifl(336)
         endif
      endif
c
c..... there are 5 different tool contact points to try;
c..... icp - contact point number:
c.....    1 - forward contact point
c.....    2 - center  ...          
c.....    3 - back    ...
c.....    4 - right   ...
c.....    5 - left    ...
c.....
c..... if 'GOUGCK/ON,3', do all 5; if gougck level < 3, do first 3.
c
      ncp = 3
      if (isv6.ge.3 .or. (lv90.and.nsf.gt.1)) ncp = 5
c
c..... decide whether to do forward contact point
c
      if (f_dot4(t(16,ia),fwd).le.ONE_MIN_EPS) then
         icp0 = 1
         iuv  = 1
         call uvcplvc4 (t(1,ia),fwd,s(8,1),tool(1))
         call surfpn (cuv(1),cuv(2),0)
         if (ifl(2).eq.466) return
      else
         icp0 = 2
         iuv  = 3
         call conv4_4 (t(13,ia),cuv(1),2)
      endif

c      lfl6 = isv6.ge.3 .and. .not.lv92
      if (isv6.ge.2 .and. .not.lv92) lfl6 = isv6
      iflat = 0
      iext = 1
      do 200 icp = icp0,ncp

         call conv4_4 (cuv(iuv),t(13,ia),2)
c
c..... set t(16-18,ia) vector:
c
         if (icp.eq.1) then
            call vctovc4 (fwd,t(16,ia))
         else if (icp.eq.2) then
            call xyzvc4 (0.,0.,0.,t(16,ia))
         else if (icp.eq.3) then
            call vctovc4 (fwd,t(16,ia))
            call mnvc4 (t(16,ia))
         else if (icp.eq.4) then
            call f_cross4 (fwd,taxis,t(16,ia))
         else if (icp.eq.5) then
            call f_cross4 (taxis,fwd,t(16,ia))
         endif

         call psrela
      if (DEBUGX .eq. 2) then
          write (dbuf,7010) s(5,1),s(6,1),s(7,1)
 7010     format ('Psrel endpt = ',3f12.5)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7011) t(16,ia),t(17,ia),t(18,ia)
 7011     format ('Psrel vec = ',3f12.5)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
         if (ifl(2).eq.466) return
c
c...Simply store the s-array
c...If checking for multi-ps gouging
c
         if (lttck .eq. 2) then
             call ncl_update_tool (tool_struct,s(8,1),t(16,ia))
             call load_surface (surf_struct)

             call ncl_tool_ps_rel (noflip,isf,ifl(331),tool_struct,
     1       ifl(23),surf_struct,toler,dtol,iext,dist,lttck)
             call ncl_psmult_store_s (isf,icp,icp0,ncp,iext,s(1,1),
     1           dist)
         else
             co  = f_dot4 (taxis,s(1,1))
             co1 = abs(f_dot4 (plfwd,s(1,1)))

             if (lv84) then
                dis = f_dot4(taxis,s(5,1)) + (psthk+(1.-co)*tool(2))*co
             else
                dis1 = co * (s(4,1) + tcn - f_dot4(s(1,1),s(8,1)))
                dis = dis1 - dtol*ifl(336)
             endif

             if (lv90 .and. nsf.gt.1 .and. co.gt..999) then
               iflat = iflat+1
               if (iext.eq.1) then
                 call point_on_plane4(s(8,1),s(5,1),s(1,1),psrf)
                 dist = f_dist4(psrf,s(5,1))
                 if (dist.le.dtol) then
                   if (nb .gt. 0) then
                     call ncl_psmult_insf (isf, t(13,ia), t(14,ia),
     1                   iext2)
                     if (iext2.eq.0) iext = 0
                   else
                     iext = 0
                   endif
                 endif
               endif
             endif

        if (lv91) then
          best = dis.gt.svdis
        else
c
c..... this change is for DASSAULT MFGNC234, MFGNC235 - when a Part Surface has
c..... both a horizontal and a vertical (parallel to DS) extensions.
c..... select the horizontal one if the other one is at an angle to both
c..... the horizontal (perp to tlaxis) plane and the forward plane,
c..... and either the distances are within dtol, or the tilted plane is 
c..... almost vertical
c
          if (co.gt.0.999.and.svco.lt.0.8.and.svco1.gt.0.5) then
             best = svco.lt.0.01 .or. dis1 .gt. (svdis1 - dtol)
          else if (svco.gt.0.999.and.co.lt.0.8.and.co1.gt.0.5) then
             best = co.gt.0.01 .and. dis1 .gt. (svdis1 + dtol)
          else
             best = dis.gt.svdis
          endif
        endif

             if (best) then
                svdis = dis
                svdis1 = dis1
                svco1 = co1
                call conv4_4 (cuv(iuv),svuv,2)
                call vctovc4 (t(16,ia),fd)
                svco = co
             endif

             call conv4_4 (t(13,ia),cuv(iuv),2)
             iuv = iuv + 2
         endif

200   continue
c
c.....Get projection type
c.....when performing TANTO,DS gouge checking
c
      if (lttck .eq. 2) go to 100
c
c..... now do the best one (if the last was not the best)
c..... fd,svco are saved already for the best one
c
201   if (.not.best) then
         call conv4_4 (svuv,t(13,ia),2)
         call vctovc4 (fd,t(16,ia))
         if (.not.lv93) lfl6 = 0
         call psrela
         if (ifl(2) .eq. 466) return
      endif
c
c..... for 'TA/NORMAL,PS', restore SF normal & re-calc
c..... plane D thru highest SF contact point.
c
      if (lnorm .or. (.not.lv91 .and. lpnorm)) then
         if (lpnorm) then
            call vctovc4(svsn,s(1,1))
         else
            call vctovc4 (taxis,s(1,1))
         end if
         s(4,1) = f_dot4 (s(1,1),s(5,1)) + svco*(1.-svco)*tool(2)
         call fill_array4 (t(16,ia),3,0.)
         ifl(56) = isv56
         ifl(23) = isv23
      endif

      ifl(6) = isv6
      ifl(23) = isv23
c
c..... Save info for this multi ps sf
c
95    continue

      if (lv90) goto 96

      if (ifl(2).eq.466) return

96    if (nsf.gt.1) then

c
c..... Here we use 9.0 logic exactly, instead of ncl_tool_ps_rel call 
c
        if (lv90) then
          if (lnogck .or. iflat .lt. ncp-icp0+1) then
            co = f_dot4(taxis,s(1,1))
c
c... get point on sf or extension: psrf = proj. of the ext.point
c... on the PS plane
c
            call point_on_plane4(s(8,1),s(5,1),s(1,1),psrf)
c
c... Determine if point is on extension
c
            iext = 0
            dist = f_dist4(psrf,s(5,1))
            if (dist.gt.dtol) iext = 1

            if (iext .eq. 0 .and. nb .gt. 0) then
              call ncl_psmult_insf (isf, t(13,ia), t(14,ia), iext2)
              if (iext2.eq.1) iext = 2
            endif
          endif

          if (iext.eq.1) then
c
c...Point is on extension
c
            if (abs(co).gt.EPS2) then
              dist = f_dist4(s(5,1),s(8,1))
            else
c
c...If point is on extension and surface at a steep angle to
c...the tool axis, create a plane through sf pt normal to vec from sf
c...pt to tool look pt
c
              call vcmnvc4(s(8,1),s(5,1),vec)
              if (f_dot4(taxis,vec) .gt. zero) then
                dist = f_mag4(vec)
                if (dist.gt.zero) then
                  call unitvc4(vec,s(1,1))
                  call vctovc4(s(5,1),psrf)
                  s(4,1) = f_dot4(s(1,1),psrf)
                  co = f_dot4(taxis,s(1,1))
                  iext = 0
                endif
              endif
            endif
          endif

          if (iext.ne.1) then
c
c... Calculate distance tool must move down tool axis to contact sf
c
            call vcmnvc4(psrf,s(8,1),vec)
            dist = f_dot4(vec,taxis) + co*tcn
          endif

          goto 97
        endif
c
c..... end of 9.0 logic
c
        call ncl_update_tool (tool_struct,s(8,1),t(16,ia))
        call load_surface (surf_struct)
        
c......check flip, Sasha, Sep11, 2018        
          tvec(1) = surf_struct(1)
          tvec(2) = surf_struct(2)
          tvec(3) = surf_struct(3)
          if(f_dot(tvec,sc(220)) .lt. 0.0) noflip = .true.

        call ncl_tool_ps_rel (noflip, isf,ifl(331),tool_struct,ifl(23),
     *     surf_struct,toler,dtol,iext,dist,lttck)
c
c..... the first time the tool is 'ON' a surface, set 'normal reversed'
c..... flags on the connected surfaces. DASSAULT MFGNC228
c
      noflip = .false.
            
       if (setad2 .and. nsf.gt.1 .and. iext.eq.0) then
         call ncl_psmult_setad2 (isf,dtol)
         setad2 = .false.
       endif
c
c...Check for surface to surface transition
c...and possibly modify surface normal if 
c...tool contacts 2 or more surfaces at one time
c...Bobby  -  3/14/00
c
cc        call ncl_tool_ps_multi (isf,tool_struct,ta_mode,surf_struct,
cc     1                          tol,iext,dist)
c
c... surface plane, tool look pt, and look vector 
c... can be changed by ncl_tool_ps_rel
c
        call conv8_4 (surf_struct,s(1,1),7)
        call ncl_update_look (tool_struct,s(8,1),t(16,ia))

97      call pssave (isf, dist, iext)
        if (ifl(56).gt.ifl56) ifl56 = ifl(56)

      endif

100   continue
c
c... If multiple PS, select best sf and save plane & point of
c... selected sf in s table.
c... reset_dp flag tells mover that there are other surfaces around
c... (to be careful with increasing dp)
c... for the same reason, return max ifl(56) flag encountered
c
c... Initialize the current PS only at the beginning of the motion.
c... so ifl(341) will be the last PS selected. 06/08/99
c      ifl(341) = 1
      if (iptk.eq.0) ifl(341) = 1

      if (nsf.gt.1) then
         status = 0
         if (lttck.ne.2 .or. lv93) then
           status = ncl_psmult_select (tool_struct,ifl(23),s(1,1),
     *      ifl(341),reset_dp,ifl(2))
         endif
c
c...Make sure that the tool does not violate
c...any part surface which was chosen
c...We do this by going thru this routine
c...one more time in a fixed tool axis mode and
c...saving off each tool look point's surface data
c...Bobby  -  3/20/00
c
         if (nsfusd .le. 1) ifl(359) = 0
         if (lttck .eq. 1 .and. .not. lnogck .and. nsfusd .gt. 1) then
             ifl(23) = TA_SAME
             lttck = 2
             call conv4_4 (s(1,1),s1,10)
             call ncl_psmult_init_s (nsfusd)
             tsav(1) = t(4,ia)
             tsav(2) = t(5,ia)
             tsav(3) = t(6,ia)
      if (DEBUGX .eq. 1) then
          write (dbuf,7000) t(4,ia),t(5,ia),t(6,ia)
 7000     format ('Psrel tool axis = ',3f12.5)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
             goto 70
         endif
c
c...Check for multi-part surface tool violation
c...If it violates, then calculate new plane and point
c

         if (lttck .eq. 2) then
             ifl(23) = isv23
             ifl(6) = isv6
             t(4,ia) = tsav(1)
             t(5,ia) = tsav(2)
             t(6,ia) = tsav(3)

             if (ifl(2) .le. 0) then
                call ncl_psmult_gougck (tool_struct,s(1,1),s1,
     2               toler,ifl(341),ifl(359))
             endif

             reset_dp = .false.
         endif
         if (ifl(2) .gt. 0) ifl(2) = 138
         if (status.ne.0) call conv4_4 (current_ps,s(1,1),7)
         if (.not.lv90) ifl(56) = ifl56
      endif

      sc(35) = sc35
      sc(144) = sc144

      return
      end
