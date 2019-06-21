C*********************************************************************
C*    NAME         :  csrel.f
C*       CONTAINS:
c*
C*                csrel
C*                set_cspt
C*                cs_nearpt
c*
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       csrel.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       03/01/16 , 11:14:22
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine csrel
C*          this routine relates the tool to cs in all cases except     
C*          surf.  if surf, call cssurf and exit.                       
C*                                                                      
C*              chg   barrel tool addition  pem         2-feb-88        
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
      subroutine csrel

      include 'com.com'
      include 'const.com'
      include 'mocom.com'
      include 'csrel.com'
      include 'gidcm1.com'

      integer*2 jd(600),itfl,iptk,ia,ib,ic,isrf,mod
      real*4 ad(300),asc(350),sbe,cbe,cutn
      equivalence(sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
c
c..... cbe (resp. sbe) is cos (resp. sin) of the angle beta between
c..... the tool side and axis. In the case of a cylindrical tool,
c..... beta=0, and so cbe=1, sbe=0
c
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50)),(mod,ifl(23))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      common/reset/ reset_dp
      logical reset_dp
c
c... Local variables
c
      real*8 tb(9),ve(3),vta(3),vfd(3),vup(3),vr(3),vds(3),vcs(3),
     *       pt(3),pt1(3),ve1(3),
     *       tx,ty,tz,cr,dst,d1,d2,thk,cutmax,hrad,tonp,toler,
     *       co8,si8,ddis,
     *       f_dot,max2_r8

      real*4 pnt1(3),pnt2(3),pnt3(3),asc305,
     *       dfwd,hdis,sgam,cgam,dmov,sfi,cfi,dang,sth,cth,sbb,cbb,sbc,
     *       dup,dupsq,
     *       cal,ted,ux,uy,uz,f,a,b,co,tcon,sal,absal,com,som,r,
     *       ti,tj,tk,dis,cute,r2dis,dx,dy,dz,tlrgt,tti,ttj,ttk,
     *       rx,ry,rz,h,h1,sec,px,py,pz,vx,vy,vz,fx,fy,fz

      real*4 f_dist4, f_dot4, f_mag4
      real*4 cotol, sitol

      integer*2 ifl2, iflip, igofix, itim, ierr, noflip
      save iflip

      logical isfar,lv84,lv90,lv91,lv93,lv95,lv97,lpivot,lmdcs,tstdis
c
c...Debug variables
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)

      lv84 = sc(169).lt.8.499d0
      lv90 = sc(169).lt.9.049d0
      lv91 = sc(169).lt.9.149d0
      lv93 = sc(169).lt.9.349d0
      lv95 = sc(169).lt.9.549d0
      lv97 = sc(169).lt.9.749d0
      lpivot = .false.
      llookfd = .false.

      cotol = .001
      sitol = .999999
      noflip = 0

      call getsct (toler)

      if (lttcrv) then
        call ttcsrl (ddis)
        return
      endif

      igofix=0

      asc305 = asc(305)
      bcuta = tool(1)
      bhgt  = tool(3)
      if (ifl(282) .ne. 0 .and. ifl(23) .eq. TA_FAN) then
        bhgt = t(27,ia)
        call barcta (bhgt,asc305,bcuta)
      endif
c
c... get tept & tijk from t(,ia)
c
      tx = t(1,ia)
      ty = t(2,ia)
      tz = t(3,ia)
      ti = t(4,ia)
      tj = t(5,ia)
      tk = t(6,ia)
      if (ifl(289).eq.1) then
         tx = tx + tool(2)*ti
         ty = ty + tool(2)*tj
         tz = tz + tool(2)*tk
      endif
      if (DEBUGX .eq. 1) then
          write (dbuf,7000) tx,ty,tz,ti,tj,tk
 7000     format ('End point = ',6f11.4) 
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

      tonp = sc(36)

      if (iptk.eq.0) then 
         iflip = 0
         ifl2 = 0
      endif

      if (lblade .and. tonp.ge.ZERO) then
        call conv4_8 (t(1,ia),tb,9)

        if (tonp.eq.ONE) then
          d1 = tool(13)
          d2 = tool(14)
          tool(13) = d1*2.0
          tool(14) = d2*2.0
        endif

        call modfy (tb,tb)

        if (tonp.eq.ONE) then
          tool(13) = 0.
          tool(14) = 0.
          call modfy (tb,tb)
          tool(13) = d1
          tool(14) = d2
        endif

        tx = tb(1)
        ty = tb(2)
        tz = tb(3)
        ti = tb(4)
        tj = tb(5)
        tk = tb(6)
      endif

      cute = tool(6)
      thk  = sc(25)
      hrad = 0.5*tool(1)
c
c...   Check TANTO a surface
c
      if (lttcsf) then
        call conv4_8(t(1,ia),tb,9)
        dst = t(21,ia)
        t(27,ia) = dst
c
c... find projection of the look point: tb = tend +  dst*taxis
c... on the curve of tangency of DS and CS:
c... pt = projection point, ve = tan vector of the curve at pt
c... For PSTAN, move tool end point to tool ring above PS contact
c... point. If tool axis is parallel to PS normal, use plus or
c... minus fwd vector as contact point for greater accuracy.
c 
        if (tantods) then
          call avcplbvc (ONE,tb,dst,tb(4),pt1)
        else
          call uvcplvc(tb,tb(4),pt1,sc(29))
          if (cute.gt.sc(27)) then
            co = f_dot4(t(4,ia),s(1,1))
            if (co.lt.0.99998) then
              call conv4_8(t(16,ia),ve1,3)
            else
              call conv4_8(t(7,ia),ve1,3)
              co = f_dot4(t(7,ia),t(16,ia))
              if (co.lt.0.0) call mnvc(ve1)
            endif
            d1 = cute
            call uvcplvc(pt1,ve1,pt1,d1)
          endif
          tx = pt1(1)
          ty = pt1(2)
          tz = pt1(3)
        endif
        dst = 0
c
c..... send a 'large distance' parameter to ncl_ttcssf, so that when the
c..... calculated distance is less, a better common boundary projection is
c..... returned. MFGNC303.
c
        if (.not.lv93 .and. (ifl(23).eq.3 .or. ifl(23).eq.6))
     *    dst = sc(25) + 2.*tool(1) + toler
        call ncl_ttcssf (pt1, dst, pt, ve, ierr,tb(7))
        if (ierr.gt.0) goto 9256
c
c... aak OCT-09-97: if'd this out: may cause
c... "Ending position failure" if tool is tilted
c... as in "TA/...,RIGHT,ang" and tilt angle is big
c
         if (lv84) then
            t(12,ia) = dst-thk
            cutmax = sc(28)
            if (cutmax.lt.tool(6))     cutmax = tool(6)
            if (cutmax.lt.t(10,ia))    cutmax = t(10,ia)
            if (cutmax.lt.10.0*toler) cutmax = 10.0*toler
            if (t(12,ia).gt.cutmax .and. itfl.eq.0) return
         endif
c
c...   We are close. Calculate check plane.
c
        call conv8_4 (pt,s(8,3),3)
        call surfpn (t(25,ia),t(26,ia),1)
        if (ifl(2).gt.0) goto 9256
        if (.not.lv93) call bndprj (pt,t(25,ia),t(26,ia),s(1,3))
        fx = ve(2)*s(3,3)-ve(3)*s(2,3)
        fy = ve(3)*s(1,3)-ve(1)*s(3,3)
        fz = ve(1)*s(2,3)-ve(2)*s(1,3)
        sec = sqrt(fx**2+fy**2+fz**2)

      if (DEBUGX .eq. 1) then
          write (dbuf,7001) fx,fy,fz,sec
 7001     format ('fxyz = ',4f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
c
c... aak 02-jun-1998: save tend proj. on real CS plane;
c... to be used later to determine whether to check top of the
c... cutter to the CS curve
c
        call point_on_plane4 (t(1,ia),s(8,3),s(1,3),pnt1)
c
c... jingrong 04/30/99 use tool top projection when ta not tantods.
c
        pnt2(1)=t(1,ia)+tool(3)*t(4,ia)
        pnt2(2)=t(2,ia)+tool(3)*t(5,ia)
        pnt2(3)=t(3,ia)+tool(3)*t(6,ia)
        call point_on_plane4 (pnt2,s(8,3),s(1,3),pnt3)

      if (DEBUGX .eq. 1) then
          write (dbuf,7002) pnt1
 7002     format ('Plane point = ',3f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7002) pnt3
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

        if (sec .gt. 0.0) then
          s(1,3) = fx/sec
          s(2,3) = fy/sec
          s(3,3) = fz/sec
        endif

        s(4,3) = s(1,3)*pt(1)+s(2,3)*pt(2)+s(3,3)*pt(3)

        goto 82
      endif
c
c... Check to a point or near point.
c... aak 20-may-1998: made it a separate routine
c
      if (lcspt) then
         call cs_nearpt (isfar)

         if (isfar) then
            if (mod.eq.4) goto 82
            return
         endif

      endif
 
5     itim = 0
c
c... cssurf handles surf cases.            9-11-81
c
      if(jd(401).eq.SURF .or. jd(401).eq.PNTVEC .or. jd(401).eq.QUILTSF
     *   .or. jd(401).eq.MESHSF .or. jd(401).eq.NETSF) then

         if (.not.l2csrf .and. jd(401).eq.SURF .and. tonp.eq.-1)
     x     l2csrf = .true.
         if (.not.lhavecspl)
     x     call cssurf
         if (ifl(2) .eq. 466) go to 9256
         goto 82
      endif
c
c...if cs is pl, go finup
c
      if(jd(401).eq.PLANE) goto 82
c
c...csis ln, ci, or cv.  error if not 2d case (temp)
c
      if(ifl(70).ne.0) then
         ifl(2) = 5
         goto 99
      endif
c
c... bra on ln or cv/ci
c
20    if(jd(401).eq.LINE) then
c
c...line. use line pt-deltas for p-v
c
         px=d(103)
         py=d(104)
         pz=d(105)
         vx=d(106)
         vy=d(107)
         vz=d(108)
c
c...cv or ci    ( ept 1st-try is ready in s-tbl)
c
      else if (jd(401).eq.CURVE) then
         call curvpv(px,py,pz,vx,vy,vz)
         if (ifl(2).gt.0) ifl(2) = 256
      else if (jd(401).eq.CIRCLE) then 
         call circpv(px,py,pz,vx,vy,vz)
c
c...error. invalid cs type.
c
      else
         ifl(2) = 30
         goto 99
      endif
c
c...csn is crossf v & ta
c
      rx=vy*tk-vz*tj
      ry=vz*ti-vx*tk
      rz=vx*tj-vy*ti
      sec=sqrt(rx**2+ry**2+rz**2)
c          if sec zero, some special case exists.   ta//v?
      if(sec.eq.0.)goto 9256
      if (rx*t(7,ic)+ry*t(8,ic)+rz*t(9,ic).lt.0.)sec=-sec
      rx = rx/sec
      ry = ry/sec
      rz = rz/sec
      if (tonp.eq.ZERO) goto 42
      if (sbe.eq.0. .and. .not.lcntct) goto 42
c
c...   Handle line, circle or curve with angled tool or CONTACT/ON
c
      h = t(27,ia)
c
c...   Unitize line vector
c
      if (jd(401).eq.LINE) then
        dst = sqrt(vx**2+vy**2+vz**2)
        if (dst.eq.0.0) goto 9256
        vx = vx/dst
        vy = vy/dst
        vz = vz/dst
      else
c
c...   Adjust height
c
        h1 = (px-tx)*ti+(py-ty)*tj+(pz-tz)*tk

        if (abs(h-h1).gt.toler .and. itim.lt.16) then
          t(27,ia) = h1
          itim = itim+1
          goto 20
        endif

      endif
c
c...   Project point to extension of curve or to correct place on line.
c
      d1 = (tx+ti*h-px)*vx+(ty+tj*h-py)*vy+(tz+tk*h-pz)*vz
      px = px+vx*d1
      py = py+vy*d1
      pz = pz+vz*d1
c
c...   handle angled tool with no contact
c
      if (.not.lcntct) then
        d1 = ((px-tx)*ti+(py-ty)*tj+(pz-tz)*tk-sc(30))*sbe/cbe
        if (sbe.lt.0) d1 = d1+(tool(1)-sc(28))/2.0d0
        goto 41
      endif
c
c...   Adjust te & cr for thick
c
      tx = tx-ti*thk
      ty = ty-tj*thk
      tz = tz-tk*thk
      cr = tool(2)+thk
      if (cr.lt.ZERO) cr = ZERO
c
c...   Calculate point on tool axis at height where cr ends
c
      h = cr*(1.0-sbe)
      dx = tx+h*ti
      dy = ty+h*tj
      dz = tz+h*tk
c
c...   calc dist of curve contact point from max width of cutter &
c...   move contact point by that amount. This will create plane parallel
c...   to ta at correct distance. (Signs are all reversed because final result
c...   is multiplied by tool to or past factor)
c
      d1 = cute - thk - max2_r8 (0.5d0*sc(28),hrad)
c
c...   If side contact, add in dist above cr * tan beta.
c
      dst = (px-dx)*ti+(py-dy)*tj+(pz-dz)*tk
      if (dst.gt.ZERO) d1 = d1+dst*sbe/cbe
c
c...   If negative thick comes inside cr, reduce flat.
c...   (Uses tan(a/2)=(1-cos(a))/sin(a), cos(b)=sin(90-a) ).
c
      d2 = tool(2)+thk
      if (d2.lt.ZERO) d1 = d1+d2*(1-sbe)/cbe
c
c...   If corner radius contact or above, add in correction
c
      d2 = (px-tx)*ti+(py-ty)*tj+(pz-tz)*tk

      if (d2.gt.ZERO) then
        if (d2.gt.h) d2 = h
        d2 = cr**2-(cr-d2)**2
        if (d2.gt.ZERO) d1 = d1 + dsqrt(d2)
      endif

41    d1 = d1*tonp
      px = px+d1*rx
      py = py+d1*ry
      pz = pz+d1*rz

42    continue
c
c... load cspl in s-tbl.
c
      s(1,3)=rx
      s(2,3)=ry
      s(3,3)=rz
      s(4,3)=px*s(1,3)+py*s(2,3)+pz*s(3,3)
c
c...normal exit route; cspl must point fwd
c...if not fwd, flip cspl (and ad(202) for next time)
c
82    co = f_dot4 (t(7,ia),s(1,3))
825   continue

      if (DEBUGX .eq. 1) then
          write (dbuf,7003) co
 7003     format ('Co = ',f12.6)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

      if (co.lt.0.) then
         if (.not.lv91 .and. iptk.gt.0 .and. noflip.eq.0 
     *       .and. co.gt.-0.017) then
c
c..... change: if the angle is less than 1 degree, first try to calculate 
c..... the distance without reversing - if it is already less than toler
c..... the motion should stop here
c 
           noflip = 1
         else
           call mnvc4(s(1,3))
           s(4,3) = - s(4,3)
           co = -co
           if (co.lt.0.001 .and. iptk.ge.1) then 
             iflip = 1
c
c..... fix for MFGNC284, when a PS plane was almost parallel to the
c..... CS plane, but the motion was far from finished
c
             if (noflip.eq.1 .and. itfl.eq.0 .and. tonp.eq.ONE .and.
     *         .not.lv93 .and. co.lt.0.0004363) noflip = 2
           endif
           if (noflip.lt.2) noflip = 0
           if (jd(401).ne.CURVE) ad(202)=-ad(202)
         endif
      else
         if (itfl.eq.0) iflip = 0
      endif

      if (DEBUGX .eq. 1) then
          write (dbuf,7005) iflip
 7005     format ('Iflip = ',i4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
c
c..... Eduard 6-28-1999. The flag iflip is set to 1 when the current 
c..... cspl is flipped while fwd is almost parallel to it. It is unset 
c..... if the current cspl has not been flipped this time, and we are 
c..... not going through endpas (itfl=0).
c

83    tcon = s(1,3)*tx+s(2,3)*ty+s(3,3)*tz
      ted  = s(4,3) - tcon + tonp*thk
c
c..... aak 27-mar-1998: ifl(298) = 2 flag is set in mocomb for middle 
c..... motion in TA/COMBIN in order to take into account the 
c..... approach-dist as CS thick
c
      if (lttcsf.and. (lv84 .or. ifl(298).eq.2)) ted=ted-thk

      if (.not.lv95 .and. .not.lttcsf .and. tonp.eq.0 .and.
     x                            ifl(298).eq.2) ted=ted-thk
c
c ************************************
c... aak 05-Feb-1998: the following "if" takes into account
c...  position of the tool top with respect to CS in "TANTO,DS" mode.
c...  another version which seems to work as well is 

c      if (tantods) then
c... instead of 
c      if (tantods .and. ted.gt.toler) then

c... the difference: if tool end is close to check surface at the 
c... beginning of a move, it does not move in the above version; it 
c... does in the current one (makes a step back); may produce a strange 
c... motion for "TA/..., RIGHT,ang,...". At the moment don't know which 
c... one is better.
c... aak 02-jun-1998: don't check the top if tool is not touching
c... the "real" CS plane: to enable motion around U-turns
c
      sal = ti*s(1,3)+tj*s(2,3)+tk*s(3,3)
      if (.not.lv90 .and.
     x       tantods .and. ted.gt.toler .and. sal.gt.0.) then
c
c..... pnt1,pnt2,pnt3 are only computed when lttcsf, so shouldn't be here
c..... otherwise - in particular during the second stage of fan. MFGNC306.
c
         if (.not.lttcsf .and. .not.lv93) goto 835
         if (lttcsf.and.ifl(298).eq.2) then
c
c.... change: always check the top if this is the midmotion and tanto,cs
c.... to solve CS collision in CATIA NCL139
c
             ted=ted-tool(3)*sal
         else 
            cutmax = 0.5*tool(1)+5.*toler
c
c... jingrong 7/30/99. If fan, use tool end proj.,otherwise use tool top.
c
            if (ifl(23).eq.4) then
               dst = f_dist4 (pnt1,t(1,ia))
               if (.not.lv93 .and. ifl(280).eq.1 .and. reset_dp)
     x           cutmax = 0.5*tool(1)+3*toler
            else
               dst = f_dist4 (pnt2,pnt3)
            endif
            dis = ted
            if (dst .lt. cutmax) then
              dis = ted-tool(3)*sal
c
c..... pnt1 is projection of tend to CS tangent plane at the CS-DS common
c..... boundary. so, at a large distance from the end dst could be any
c..... small number. I do not understand why this distance should be
c..... used to decide when to switch to the distance from top formula.
c..... a result of this in Dassault's MFGNC288  was a premature endpas
c..... attempt, which resulted in freaky tool axis changes and guide
c..... curve projection problems.
c
              if (lv93 .or. .not. (ifl(80).eq.1 .and.  tonp.eq.0 .and.
     *          dis.lt.t(12,ic)/2 .and. dis.gt.cutmax)) ted = dis
            else if (ifl(289).eq.2 .and. iptk.eq.0 .and. itfl.eq.0 .and.
     x               ifl(23).eq.4 .and. ifl(280).eq.1 .and. icsgck.le.1
     x               .and. dis.gt.2*toler .and. dis.lt.10*toler) then
              if (.not.lv93 .and.
     *            (ted - tool(2)*sal .lt. -10*toler)) then
c
c..... special case for changing center/auto to center/on and restarting
c
                ifl(289) = 1
                itfl = -2
                return
              endif
            else if (itfl.eq.11 .and. .not.lsamta .and. lcvrg1 .and.
     x               t(12,ia).eq.0 .and. ted.gt.0.75*t(12,ic)) then
c
c..... MFGNC306: fixed premature ending of fan1 with a guide curve
c
              itfl = 0
            endif
         endif
         if (ted.le.toler) ted = ZERO
      endif

c ************************************
c
c... 'ON' CS.
c
835   if (tonp.eq.ZERO) then
         dis = ted
         goto 98
      endif

      sal   = ti*s(1,3)+tj*s(2,3)+tk*s(3,3)
      absal = abs(sal)
      if(absal.gt.1.) absal = 1.
      cal = 0.
      if(absal.lt.1.) cal = sqrt(1.-absal*absal)
c
c... add barrel tool chgs          2-feb-88
c
      if(ifl(282).eq.0) goto 84

      sfi = tonp*sal
c
c...if rcorn case, go use existing logic.
c
      if(sfi.gt.sbe) then
         dis = ted + tonp*(tool(2) + tool(6)*cal)-tool(2)*sal
c
c...ditto if top case.
c
      else if (sfi.lt.asc305) then
cc         dis = ted + tonp*tool(1)/2.*cal - tool(3)*sal
         dis = ted + tonp*bcuta/2.*cal - bhgt*sal
c
c...r2case.
c
      else
         cfi = 0.
         if(abs(sfi).lt.1.) cfi = sqrt(1.-sfi*sfi)
         r2dis = cfi*asc(67) - sfi*asc(68) + asc(307)
         dis = ted + r2dis*tonp
      endif

      goto 98

84    dang = sbe - tonp*sal
c
c...tool side is parlel cs.   use cutn for csdis calc
c
      if (abs(dang).le.0.001 .or.
     *    (ifl(23).eq.4 .and. ifl(68).eq.0)) then
c
c..... ifl(68)=1 if we get here from csavoid - to check for gouging during
c..... "TANTO CS" motion - Dassault MFGNC243
c     if (abs(dang).le.0.001 .or. ifl(23).eq.4) then
         dis = ted + tonp*cutn
         if (ifl(289).eq.2 .and. itfl.eq.-2 .and. dis.lt.-toler) then
           tx1 = tx + tool(2)*ti
           ty1 = ty + tool(2)*tj
           tz1 = tz + tool(2)*tk
           tcon1 = s(1,3)*tx1+s(2,3)*ty1+s(3,3)*tz1
           dis1  = dis + tcon - tcon1
           if (dis1 .gt. toler) then
             ifl(289) = 1
             tx = tx1
             ty = ty1
             tz = tz1
             dis = dis1
           endif
         endif
c
c...tool strikes at top or rcorn
c
c...rcorn case
c
      else if (sal*tonp.ge.sbe) then
         dis = ted + tonp*(tool(2) + tool(6)*cal) - tool(2)*sal
c
c... ifl(298)=3, set leaving fan pl in mocomb.
c... jingrong 02/25/2000.
c
         if (ifl(298).eq.3.and..not.lv91) dis = ted +tonp*tool(1)/2.0
c
c...tool top contact
c
      else
         dis = ted + tonp*tool(1)/2.*cal - tool(3)*sal
c
c..... Eduard 8/5/1999. Block the top contact if CS plane
c..... intersects tool and motion is almost parallel to the CS plane:
c..... try changing the CS plane normal direction.
c
         if (.not.lv90 .and. ifl(282).eq.0 .and. co.lt.cotol) then
           if ( (dis.gt.2*abs(ted) .and. sal.lt.-sitol) .or.
     x       (dis.lt.-0.5*tool(3) .and. sal.gt.sitol) ) then 
              co = -0.5
              goto 825
           endif
         endif

      endif

98    t(12,ia) = dis
      if (abs(dis).gt.toler .and. noflip.eq.1) goto 825
c
c..... fix for MFGNC284
c
      if (noflip.eq.2 .and.  dis.lt.t(12,ic)/2. and. dis.gt.toler) then
        noflip = 0
        iflip = 0
        call mnvc4(s(1,3))
        s(4,3) = - s(4,3)
       if (jd(401).ne.CURVE) ad(202)=-ad(202)
       goto 83
      endif
c
c... if slowdn tim1 and not fanpas1, alter dis per dslow  13-may-85
c
      if(ifl(213).eq.0.or.ifl(243).ne.1.or.ifl(80).eq.1) goto 985
      t(12,ia) = dis - sc(125)

985   continue
c
c...if cs is surf, see if time to turn nearfl on.
c
      if (jd(401).ne.SURF .and. jd(401).ne.PNTVEC) goto 99
      if (ifl(83).gt.0.or.iptk.eq.0) goto 99
      if (dis.le.0.) goto 99
c
c... ok to test for near cs.  turn on nearfl if indicated.
c
      if (dis.lt.t(12,ic)*.6.and.ifl(58).gt.0) ifl(83)=1

99    if (ifl(23).ne.4) goto 999
c
c... ta/fan    calc apxpt, etc.
c... if this loc indicates endup start, leave apxpt as is  18-feb-85
c... unless cs is plane.  in plane case, calc apxpt if cdis
c... is reasonable.
c
c...Don't calculate new apex point if dis csdis are both 0
c...Causes bad apex point to be calculated in
c...Dassault test case NCCS-27-08-2010-1.cpp
c...Bobby  -  09/03/10
c
      if (DEBUGX .eq. 1) then
          write (dbuf,7010) itfl,labandon,dis,t(12,ic),toler
 7010     format ('itfl = ',i2,'  labandon = ',i2,'  dis = ',f8.3,
     1            '  t(12) = ',f8.3,'  toler = ',f8.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

      tstdis = (dis-toler) .le. t(12,ic)/2.
      if (lv97) tstdis = dis .lt. t(12,ic)/2.
      if (itfl.eq.0 .and. tstdis .and.
     x    (lv91.or..not.labandon.or.(dis+toler.gt.0.))) goto 9900
c
c..... do calculate a new apex point if during the previous step
c..... endpas decided to abandon this intersection, and we are 
c..... moving away from it (so that dis < 0)
c
      if (DEBUGX .eq. 1) then
          call ctob ("Inside Csrel-if",dout)
          call nclxostr(dout)
      endif
      if (labandon) then
         labandon = .false.
c
c..... but only during the first pass of fan - DASSAULT MFGNC229
c
         if (ifl(80).eq.0 .and. dis.lt.0.5*t(12,ic)) goto 9900
      endif
      goto 9902
c              start of endup.  if cs is srf, do not re-calc apxpt
9900  continue
c
c... jingrong 5/6/99 use the infinite apex pt for the 1st pt of FAN2.
c
      if (.not.lv84.and.iptk.eq.0.and.ifl(80).eq.0) then
         hdis = 1.e+9
         goto 992
      endif
c
c..... if cs is plane.  unreasonable if csdis lt 1/8 old cdis
c..... or lt .03
c
      if (.not.lv93 .and. ifl(146).eq.1 .and. jd(401).eq.PLANE
     *    .and. (dis+toler).lt.0 .and. t(12,ic).lt.toler) then
        hdis = f_mag4(t(28,ia))
        if (2.*hdis .lt. HUGE_VAL) goto 9902
      endif
      if (ifl(146).eq.2 .and. jd(401).eq.PLANE) goto 9902
c
c..... the flag ifl(146) is set in mover for some special case of
c..... an abandoned tanto palne - DASSAULT MFGNC229
c
      if (jd(401).ne.PLANE .or.dis.lt.t(12,ic)/8.
     *                             .or. dis.lt..03) goto 993

9902  continue
      sth = f_dot4 (s(1,2),s(1,3))
      if (abs(sth).gt..999999) sth=.999999
      cth=sqrt(1.-sth**2)
      ux=(s(2,2)*s(3,3)-s(3,2)*s(2,3))/cth
      uy=(s(3,2)*s(1,3)-s(1,2)*s(3,3))/cth
      uz=(s(1,2)*s(2,3)-s(2,2)*s(1,3))/cth

      if (lblade) then
        a = 0.0
        b = 0.0

        if (tonp.eq.ZERO) then
          b = asc(305)
        else if (tonp.eq.ONE) then
          b = sin(asin(asc(305))*2)
        endif

        tx = t(1,ia)
        ty = t(2,ia)
        tz = t(3,ia)
        ti = t(4,ia)
        tj = t(5,ia)
        tk = t(6,ia)
      else
        tlrgt =ifl(21)
        sbb = sbe
        if (ifl(282).ne.0) sbb = asc305
        sbc = sbb

        if (lcvgid .and. (co.le.0.5 .or. lsamta)) goto 9903
c
c..... Do not use lmdnow for internal CS plane with TANTO case
c        
cc      if (lmdnow.or.lcvgid.or.ldschk) then
        lmdcs = lmdnow
        if (lmdnow .and. mocom .and. ifl(23) .eq. TA_FAN
     x      .and. ltanto)then
         lmdcs = .FALSE.
        endif
        if (lmdcs.or.lcvgid.or.ldschk) then
          call conv4_8 (s(1,2),vds,3) 
          call conv4_8 (s(1,3),vcs,3) 
          call conv4_8 (t(7,ia),vfd,3) 
          call conv4_8 (t(4,ia),vta,3) 

          call f_cross(vds,vfd,vup)
          call f_cross(vfd,vta,vr)
          call unitvc(vr,vr)
          co8 = f_dot(vr,vds)
          si8 = ZERO
          if (dabs(co8).lt.ONE) si8 = dsqrt(ONE - co8**2)
          d1 = f_dot(vr,vup)
          if (d1.gt.ZERO) si8 = -si8
          cbb = cbe
          if (ifl(282).ne.0) cbb = sqrt(1. -sbb*sbb)
          sbb = sbb*co8 + cbb*si8
        endif

9903    a = sbb*tlrgt
        b = sbc*tonp
      endif
c
c...vector (tti, ...) is the tool axis in the end position
c...sth = cos(angle between n_drive & n_check)
c...sbe = cos(angle between  (tti, ...) and n_drive or n_check)
c
      f = (b-a*sth)/(1.-sth**2)
      r = a-f*sth
      dup = 0.
      dupsq = 1.- a*a - (f*cth)**2
      if(dupsq.gt.0.) dup = sqrt(dupsq)

      tti=s(1,2)*r+s(1,3)*f+ux*dup
      ttj=s(2,2)*r+s(2,3)*f+uy*dup
      ttk=s(3,2)*r+s(3,3)*f+uz*dup
c
c...if not clearly approaching cs or cutter solution unreal,
c...leave apxpt as-is and exit.
c... aak 21-sep-1998: use infinite apxpt rather than old one
c
      if (co.le.0.1 .or. dupsq.le.0.) then
         if (lv90) then
           call conv4_4 (t(28,ic),t(28,ia),3)
         else
           call uvcplvc4 (t(1,ia),t(4,ia),t(28,ia),1.e+09)
         endif

      if (DEBUGX .eq. 1) then
          write (dbuf,7004) itfl
 7004     format ('Itfl = ',i4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

         ifl2 = ifl(2)
         if (itfl.gt.0) ifl(2) = 163
c
c..... Eduard 6-28-1999. We unset the Error 163 flag if there has
c..... been only one pass through endpas, and the iflip flag is set 
c..... to one.
c
         if (itfl.ge.11 .and. iflip.eq.1 .and. .not.lv90) ifl(2)=ifl2
         goto 993
      endif
c
c... normal route.
c
c...com = cos(angle between the forward vector and current tool axis)
c
      com = t(7,ia)*ti+t(8,ia)*tj+t(9,ia)*tk
      som = 0.
      if(abs(com).lt.1.) som = sqrt(1.-com*com)
c
c...dfwd = distance between current and final position of
c...the tool end ; dis = distance between the current pos. of tool end
c...and the check surface; co = cos(angle between n_check & forward vect)
c
      dfwd = 0.
      if (co.gt.0.) dfwd = dis/co
      hdis = HUGE_VAL
c
c...vp 11/12/97 removed to make always apex point,
c...hdis remains huge only if (t,tt) is close to 0. 
c
      if (lv84.and.itfl.gt.0.and.ifl(243).ne.1) goto 992
c
c...fi = angle between the forward vector and the final direction
c...of tool axis; cfi = cos(fi)
c
      cfi = t(7,ia)*tti + t(8,ia)*ttj + t(9,ia)*ttk
      sfi = 0.
      if (abs(cfi).lt.1.) sfi = sqrt(1.-cfi*cfi)
c
c... gam = fi - omega
c
      cgam = som*sfi + com*cfi
      sgam=0.
      if (abs(cgam).lt.1.) sgam = sqrt(1.-cgam*cgam)
      if (com.lt.cfi) sgam=-sgam
c
c... aak 7-may-97:
c...corrected apex point calculation in the case when initial and final
c...direction of the tool axis do not lie on the same plane (tti and ti)
c...hdis is the distance from the tool end to the apex point along the
c...tool axis
c
      if (abs(sgam).gt.1.e-3) then
         hdis = dfwd*sfi/sgam
         if (lv84) hdis=dfwd*som/sgam
      endif
c
c... jingrong if the tool has been very close to the CS and ta almost
c... parellel to the CS pl(0.5 degree), do not change apex pt. 02/03/2000.
c
      if (.not.lv90 .and. itfl.gt.0 .and. abs(dfwd).lt.0.03
     *  .and.abs(sgam).lt.0.0174524) then
        if (abs(sgam).lt.0.0087) then
          hdis = HUGE_VAL
        else if (.not.lv93 .and. itfl.lt.5 .and. co.lt.0.95) then
          lpivot = .true.
          hdis = HUGE_VAL
        endif
      endif
c
c..... changed to 0.75 degrees - to stabilize DASSAULT models per07,per08
c    *  .and.abs(sgam).lt.0.0087) hdis = HUGE_VAL
c..... had to change back to 0.5 degrees because of QAR 92347 - also note
c..... that the change from 0.5 to 0.75 was not version flag compatible!!!
c
c... aak 16-dec-1997: if apex pt. is inside tool in the iteration mode,
c... make it infinite. This change restores in a weaker form
c... the using infinite apex pt. commented out by vp above. 
c... At the moment it's phenomenological, for we don't have true 
c... understanding
c      if (itfl.gt.0 .and. hdis.lt.tool(3) .and. hdis.gt.0.) 
c
c... jingrong 5/6/99  cause tool ending in a different position on 
c... U-shaped CS. removed again.
c
      if (.not.lv84 .and. itfl.gt.0 .and. hdis.lt.tool(2)
     *    .and. hdis.gt.0.) hdis = HUGE_VAL
      if (itfl.eq.0 .and. (.not.lsamta.and.lcvrgt.and.
     *    dis.lt.t(12,ic)/2)) hdis = HUGE_VAL
c
c... apex point
c
992   continue
      if (lv84 .or. lpivot) then 
        t(28,ia)=tx+hdis*tti+dfwd*t(7,ia)
        t(29,ia)=ty+hdis*ttj+dfwd*t(8,ia)
        t(30,ia)=tz+hdis*ttk+dfwd*t(9,ia)
      else 
        t(28,ia) = tx + hdis*ti
        t(29,ia) = ty + hdis*tj
        t(30,ia) = tz + hdis*tk
      endif
      if (DEBUGX .eq. 1) then
          write (dbuf,7011) lpivot,hdis
 7011     format ('lpivot = ',i2,'  hdis = ',f20.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7012) t(28,ia),t(29,ia),t(30,ia)
 7012     format ('t(28) = ',3f20.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
c
c... also add to t(,ic)
c... vp 8-mar-95 update 'apx(ib)' so the 3-th point will not use old
c... apex point but the previous one.
c
993   continue

c     call conv4_4 (t(28,ia),t(28,ic),3)
      if (iptk .ge. 0) call conv4_4 (t(28,ia),t(28,ic),3)
      if (iptk .eq. 0) call conv4_4 (t(28,ia),t(28,ib),3)

999   CONTINUE
c
c... if this is pass logical startpt of a go/strtup and csdis
c... is (-), tool has crossed CS.  move back (csdis+.1)/co
c... and try one more time.                 10-feb-88
c
      if(ifl(280).eq.0.or.iptk.ne.0.or.jd(401).eq.CIRCLE) return
      if(dis.gt.0.01 .or. igofix.gt.0) return
      if (.not.lv93 .and. co.lt.0.001 .and. (-10.*dis) .gt. tool(1))
     *  return

      if (lv84) then
        if(co.eq.0.) return
      else
        if (co.lt.0.01) co = 0.01
      endif

      igofix = 1
      dmov   = (-dis+.1)/co

      tx=tx-T(7,IA)*DMOV
      ty=ty-T(8,IA)*DMOV
      tz=tz-T(9,IA)*DMOV
      T(1,IA)=tx
      T(2,IA)=ty
      T(3,IA)=tz
c
c... set maxdp small.  will be reset at strtup exit
c
      sc(54) = 0.1
      goto 83
c
c...Error - tool to cs apply failure
c
9256  if (ifl(2) .ne. 466) ifl(2) = 256

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     :  set_cspt
C*       This routine sets cs plane for the passend steps using
C*       near point coordinates.
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
      subroutine set_cspt

      include 'com4a.com'
      include 'mocom.com'
c
c... aak 20-may-1998: common to hold initial CS plane
c... obtained via projecting nearpt on CS
c
      common /csnpt/ csnpt_pl
      real*8 csnpt_pl(7)
c
      real*4 ad(300)
      equivalence(d,ad)

      real*4 f_dot4

      call conv8_4 (d(115),s(8,3),3)
      call surfpn(t(25,3),t(26,3),1)

      if ( f_dot4(t(7,3),s(1,3)) .lt. 0.) then
         ad(202) = -ad(202)
         call mnvc4 (s(1,3))
         s(4,3) = - s(4,3)
      endif

      call conv4_8 (s(1,3),csnpt_pl,7)

      return
      end

C
C*********************************************************************
C*    E_SUBROUTINE     :  cs_nearpt (isfar)
C*       Decides if tool is close enough to CS nearpt to do full
C*       CS calculations in "csrel"
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*               isfar  - L D1  -  if true, tool is far from CS nearpt
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cs_nearpt (isfar)

      include 'com.com'
      include 'mocom.com'
c
c... aak 20-may-1998: common to hold initial CS plane
c... obtained via projecting nearpt on CS
c
      common /csnpt/ csnpt_pl
      real*8 csnpt_pl(7)

      logical isfar,lv94,lv100

      integer*2 jd(600)
      equivalence(d,jd)
      equivalence(ifl(51),ia)
c
c...Debug variables
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)
c
c
c... local var's
c
      real*8 ta(6),pt(3),vec(3),fwd(3),vc1(3),
     *       dis,co, diam, tonp,
     *       f_mag, f_dot,max2_r8


      lv94 = sc(169).lt.9.449d0
      lv100 = sc(169).lt.10.049d0

      diam = 0.5*tool(1)
      if (.not. lv100 .and. diam .lt. 10.*sc(27)) diam = 10.*sc(27)
      thk  = sc(25)
      tonp = sc(36)

      call conv4_8 (t(1,ia),ta,6)
      call conv4_8 (t(7,ia),fwd,3)
c
c... calculate dist. from nearpt to the taxis line
c
      call point_on_line (d(115),ta, pt)
      call vcmnvc (d(115),pt,vec)
      dis = f_mag (vec)
c
c... if tend is past nearpt, make it < 0
c
      call unitvc (vec,vc1)
      co = f_dot(vc1,fwd)
      if (co .le. -.9848 .or. (lv100 .and. co .lt. 0.)) then
        dis = -dis
      endif
c
c... corrections to THICK and tool tilt towards the nearpt
c
      dis = dis + tonp*(thk + tool(1)*.5)

      if (jd(401).eq.SURF) then
         co = f_dot (ta(4),csnpt_pl)
         if (co.gt.0.) dis = dis - co*tool(3)
      endif

      if (lv100 .and. .not. lv94) then
          sc(174)  = dabs(dis)
          isfar = sc(174).gt.max2_r8 (0.5*sc(28),diam)
      else
          sc(174)  = dis
          isfar = dis.gt.max2_r8 (0.5*sc(28),diam)
      endif
      if (DEBUGX .eq. 1) then
          call ctob (' ',dout)
          call nclxostr(dout)
          write (dbuf,7000) d(115),d(116),d(117)
 7000     format ('ckpt = ',2(f9.3,','),f9.3)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7001) ta(1),ta(2),ta(3),ta(4),ta(5),ta(6)
 7001     format ('tend = ',5(f9.3,','),f9.3)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7002) pt(1),pt(2),pt(3)
 7002     format ('pt = ',2(f9.3,','),f9.3)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7003) fwd(1),fwd(2),fwd(3)
 7003     format ('fwd = ',2(f9.3,','),f9.3)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7004) vc1(1),vc1(2),vc1(3)
 7004     format ('vc1 = ',2(f9.3,','),f9.3)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7005) isfar,dis,co
 7005     format ('isfar = ',i2,'    dis = ',f9.3,'   co = ',f9.6)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
c
c... if isfar && TA/...,FAN : pass up the projection plane of nearpt
c... on CS as the CS plane
c
      if (isfar) then
         if (ifl(23).eq.4) call conv8_4 (csnpt_pl,s(1,3),4)
      else if (jd(401).eq.POINT) then
          isfar = .true.

          if (ifl(291).lt.11) then
            call vctovc4 (t(7,ia),s(1,3))
          else if (ifl(291).lt.18) then
            call vcplvc4 (s(1,3),t(7,ia),s(1,3))
            call unitizevc4 (s(1,3))
          endif

          dis = s(1,3)*vec(1) + s(2,3)*vec(2) + s(3,3)*vec(3)
      endif

      t(12,ia) = dis

      return
      end

