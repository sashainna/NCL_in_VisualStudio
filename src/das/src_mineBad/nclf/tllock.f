C*********************************************************************
C*    NAME         :  tllock.f
C*       CONTAINS:
C*    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       tllock.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:47
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine tllock
C*       Handle motion with tlaxis locked at start and end of move.
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
      subroutine tllock

      include 'com.com'
      include 'mocom.com'

      common/tpvcom/tavend
      real*8 tavend(3)
c
      real*8 sch(22),fwd(3),dis1,dis2,dis3,dis4,tdis1,tdis2,tdis
      real*8 endple(4),endvta(3),vtt(3),pl1(4),pl2(4),ptt(3)
      real*8 rgt(3),pte(3),pt1(3),vt1(3),pt2(3),vt2(3)
      real*8 svdis,svd51(6),hsc(9), plasw, cs1thk, cs2thk
      real*8 co, si, tn
      real*8 f_mag,f_dot
      real*4 svuv(6), f_dot4
      integer*4 isv2(2),iept(2)
      integer*2 isv1,iclf,ifl21,ifl23,ifl42,ifl154,ifl213
      integer*2 mod, iflg
      logical lsvuv(3), lmodhld, svmdfl
      integer*2 ksn(4)
      real*8 asn
      equivalence (asn,ksn)
      integer*2 ic
      equivalence (ifl(53),ic)

      call conv8_8(sc,sch,22)
      asn    = 0.0d0
      ksn(4) = PLANE
      plasw = asn

      call uv_save (lsvuv,svuv)

      lmodhld = lmdnow
      svmdfl  = motdfl
      iclf = 0
      ifl21  = ifl(21)
      ifl23  = ifl(23)
      mod    = ifl23
      if (mod.eq.TA_COMBINE .or. mod.eq.TA_COMBINE_PARELM) mod = TA_FAN
      ifl213 = ifl(213)
      ifl42  = ifl(42)
      ifl154 = ifl(154)
      cs1thk = sc(25)
      cs2thk = sc(26)
      isv1   = istat(1)
      call ncl_setptr(imotp,isv2)
      call ncl_zroptr(iept)
      sc(14) = 0.0d0
c
c... do whole move in user tool axis mode to calculate the total
c... distance travelled.
c
      motdfl   = .false.
      svdis    = 0.0d0
      ifl(42)  = 2
      ifl(213) = 0
      ifl(335) = 1
      ifl(23)  = mod
      sc(160)  = 0.0d0
      sc(176)  = 0.0d0
      call domove
      if (ifl(2).gt.0 .or. lavdd) goto 98
      lavoid = .false.
      dis1 = dabs(sc(196))
      dis2 = dabs(sc(197))
      dis3 = dabs(sc(198))
c
c... calculate locked distance if radius is in effect
c
      if (ifl(366).eq.1 .and. ifl(21).ne.0 .and.
     x   (sch(12).eq.CS_TO.or.sch(12).eq.CS_PAST.or.ifl(365).eq.3))then
        co = f_dot4(s(1,2),s(1,3))
        if (co.gt.1.d0) co = 1.0d0
        if (co.lt.-1.d0) co = -1.0d0
        if (ifl(21).eq. 1 .and. sch(12).eq.CS_TO .or.
     x      ifl(21).eq.-1 .and. sch(12).eq.CS_PAST) co = -co
        si = dsqrt(1-co**2)
        tn = 1.0d0
        if (dabs(si).gt.1.0d-6) tn = (1.0d0-co)/si
        dis4 = dis1*tn
        if (ifl(365).eq.3) then
          tdis = dis2 + dis3
        else
          tdis = dis2*2.0d0 + dis3 + dis4
        endif
        dis1 = dis3
        tdis1 = dis1+dis2
        tdis2 = dis4+dis2
      else
        tdis1 = dis1+dis2
        tdis2 = tdis1
        tdis = 2.d0*tdis1
        if (ifl(365).eq.3) tdis = tdis1
      endif
      if (sc(160).lt.tdis) then
         goto 50
      end if
c
c... Calculate end plane.
c
      call f_cross(sc(7),sc(4),rgt)
      call f_cross(sc(4),rgt,fwd)
      if (f_mag(fwd) .eq. 0.0d0) then
         ifl(2) = 117
         goto 98
      endif
      call unitvc(fwd,endple)
      call vctovc(sc(4),endvta)
      endple(4) = f_dot(sc,endple)
c
c... restore tool to start point.
c
      call uv_reset (lsvuv,svuv)
      call conv8_8 (sch,sc,13)
      ifl(2)   = 0
      ifl(335) = 0
c
c... if omit was specified, don't do first leave distance 
c
      if (ifl(365).eq.1) then
        ifl(365) = 2
        ifl(42)  = ifl42
        ifl(154) = ifl154
        call conv8_8 (sch,sc,10)
        goto 30
      endif
c
c... set ifl(298)=1 to return from mover at startpt
c
      ifl(298) = 1
      call premov
      if (ifl(2).gt.0) goto 98
      call do_mover

      ifl(298) = 0
      if (ifl(2).gt.0) goto 98
c
c... Create fwd vector perpto tool axis
c
      call conv4_8 (t(7,1),fwd,3)
      call f_cross(fwd,sc(4),rgt)
      call f_cross(sc(4),rgt,fwd)
      if (f_mag(fwd) .eq. 0.0d0) then
         ifl(2) = 117
         goto 98
      endif
      call unitizevc(fwd)
c
c... create planes at locked dis & locked dis + interp dis from tool
c
      call conv4_8 (t(1,1),pte,3)
      call uvcplvc(pte,fwd,ptt,dis1)
      call vctovc(fwd,pl1)
      pl1(4) = f_dot(fwd,ptt)
      call uvcplvc(pte,fwd,ptt,tdis1)
      call vctovc(fwd,pl2)
      pl2(4) = f_dot(fwd,ptt)
c
c... Add first plane to cs dtbl and drive on it with fixed ta.
c
      call conv8_8 (pl1,d(103),4)
      sc(13) = plasw
      asn    = 0.0d0
      ksn(1) = PLANE
      d(101) = asn
      ifl(23) = TA_SAME
      ifl(154) = 2
      sc(12) = CS_ON
      lmdnow  = .false.
      call domove
      lmdnow  = lmodhld
      if (ifl(2).gt.0) goto 50
      call vctovc(sc,pt1)
c
c... Add second plane to cs dtbl and drive on it with user specified
c... tool axis.
c
      if (dis2.gt.sc(27)) then
        call conv8_8 (pl2,d(103),4)
        isc10(1) = GOFWD
        sc(12) = CS_ON
        ifl(23) = mod
        call domove
        if (ifl(2).gt.0) goto 50
        call vctovc(sc,pt2)
        call vctovc(sc(4),vt2)
        call vctovc(sc(7),pl2)
        pl2(4) = f_dot(sc(7),pt2)
      endif
c
c... Start of real motion
c
      motdfl   = svmdfl
      ifl(42)  = ifl42
      ifl(154) = ifl154
      call conv8_8 (sch,sc,10)
      call uv_reset (lsvuv,svuv)
c
c... Add first plane to cs dtbl and drive on it with fixed ta.
c
      call conv8_8 (pl1,d(103),4)
      asn    = 0.0d0
      ksn(1) = PLANE
      d(101) = asn
      ifl(23) = TA_SAME
      lmdnow  = .false.
      call domove
      lmdnow  = lmodhld
      if (ifl(2).eq.142) goto 98
      if (ifl(2).gt.0) goto 50
      asn    = 0.0d0
      ksn(1) = GOFWD
      ksn(2) = 1
      sc(10) = asn
      if (dis2.lt.sc(27)) goto 30
c
c... Add second plane to cs dtbl
c
      call conv8_8 (pl2,d(103),4)
      asn    = 0.0d0
      ksn(4) = PLANE
      sc(13) = asn
      asn    = 0.0d0
      ksn(1) = PLANE
      d(101) = asn
c
c... If fan transition mode, drive to second plane in fan mode
c
      if (ifl(364).eq.1) then
        ifl(23) = TA_FAN
        call domove
        if (ifl(2).eq.142) goto 98
        if (ifl(2).gt.0) goto 50
        goto 30
      endif
c
c    Interpolate transition mode
c... create drive plane
c
      call conv8_8(d(51),svd51,6)
      call vcmnvc(pt1,pt2,vt1)
      call f_cross(vt1,sc(4),vtt)
      call unitvc(vtt,d(53))
      d(56)   = f_dot(pt1,d(53))
      asn     = 0.0d0
      ksn(4)  = PLANE
      sc(11)  = asn
      asn     = 0.0d0
      ksn(1)  = PLANE
      d(101)  = asn
      ifl(21) = 0
c
c... drive on created plane to 2nd check plane with ta interp
c
      ifl(23) = TA_INTERPOL
      call vctovc(vt2, tavend)
c      ifl(42) = ifl42
c      ifl(154) = ifl154
      lmdnow  = .false.
      call domove
      lmdnow  = lmodhld
      if (ifl(2).eq.142) goto 98
      if (ifl(2).gt.0) goto 50
      call conv8_8(svd51,d(51),6)

30    continue
      if (ifl(366).eq.1) dis1 = dis4
      sc(11)  = sch(11)
      ifl(21) = ifl21
c
c... restore plane at totdis from original check surface
c
      call vctovc (endple,d(103))
      if (ifl(365).eq.3) tdis2 = 0.0d0
      d(106) = endple(4) - tdis2
      asn    = 0.0d0
      ksn(1) = PLANE
      d(101) = asn
      sc(12) = CS_ON
      sc(13) = plasw
c
c... Drive to totdis from check sf using user specified tlaxis.
c
      
      ifl(23) = ifl23
      if (ifl23.eq.TA_COMBINE .or. ifl23.eq.TA_COMBINE_PARELM) then
        call mocomb
      else
        call domove
      endif
      if (ifl(2).eq.142) goto 98
      if (ifl(2).gt.0) goto 50
      if (ifl(365).eq.3) then
        ifl(365) = 0
        goto 99
      endif
      call vctovc(sc,pt1)
      isc10(1) = GOFWD
      if (dis2 .lt. sc(27)) goto 40
      d(106) = endple(4) - dis1
c
c... Fan interpolate mode
c
      if (ifl(364).eq.1) then
        ifl(23) = TA_FAN
        call domove
        if (ifl(2).eq.142) goto 98
        if (ifl(2).gt.0) goto 50
        goto 40
      endif
c
c... In dntcut, drive to fixed dis from cs, create drive plane
c... from start point to end point.
c
      motdfl  = .false.
      ifl(42) = 2
      ifl(154) = 2
      ifl(23) = TA_SAME
      call conv8_8(sc,hsc,9)
      call vctovc(endvta, sc(4))
      call premov
      if (ifl(2).gt.0) goto 50
      lmdnow  = .false.
      call mover
      lmdnow  = lmodhld
      if (ifl(2).gt.0) goto 50
      call vctovc(sc,pt2)
      call conv8_8(d(51),svd51,6)
      call vcmnvc(pt1,pt2,vt1)
      call f_cross(vt1,sc(4),vtt)
      call unitvc(vtt,d(53))
      d(56)   = f_dot(pt1,d(53))
      sc(11)  = plasw
      asn     = 0.0d0
      ksn(1)  = PLANE
      d(101)  = asn
      ifl(21) = 0
      call conv8_8(hsc,sc,9)
      motdfl  = svmdfl
      ifl(42) = ifl42
      ifl(154) = ifl154
c
c... Drive on created drive plane to fixed dis from check surface
c... interpolating tool axis to end vector.
c
      d(106) = endple(4) - dis1
      ifl(23) = TA_INTERPOL
      call vctovc(endvta, tavend)
      lmdnow  = .false.
      call domove
      lmdnow  = lmodhld
      if (ifl(2).eq.142) goto 98
      if (ifl(2).gt.0) goto 50
      call conv8_8(svd51,d(51),6)
      sc(11)  = sch(11)
      ifl(21) = ifl21
c
c... Drive to end position with fixed tool axis
c
40    continue
      d(106) = endple(4)
      ifl(23) = TA_SAME
      lmdnow  = .false.
      call domove
      lmdnow  = lmodhld
      if (ifl(2).eq.142) goto 98
      if (ifl(2).gt.0) goto 50
      sc(198) = dis4
      goto 99
50    continue
c
c... Error, do whole move in user tool axis mode
c
      ifl(2)   = 0
      ifl(21)  = ifl21
      motdfl   = svmdfl
      ifl(42)  = ifl42
      ifl(154) = ifl154
      ifl(213) = ifl213
      ifl(335) = 0
      ifl(352) = 0
      if (ifl(42) .eq. 0) then
        call ncl_eqlptr(isv2,imotp,iflg)
        if (iflg .eq. 0) call cldel (iclf,isv2,iept,imotp)
        call ptclos
      endif
      sc(25)   = cs1thk
      sc(160)  = 0.
      sc(176)  = 0.
      call conv8_8 (sch,sc,13)
      ifl(23) = ifl23
      call uv_reset (lsvuv,svuv)
      if (ifl23.eq.TA_COMBINE .or. ifl23.eq.TA_COMBINE_PARELM) then
        call mocomb
      else
        call domove
      endif
      if (ifl(2).gt.0) goto 98
      goto 99

98    continue
      if (ifl(42) .eq. 0) then
        call ncl_eqlptr(isv2,imotp,iflg)
        if (iflg .eq. 0) call cldel (iclf,isv2,iept,imotp)
        call ptclos
      endif
      call conv8_8 (sch,sc,9)

99    continue
      ifl(21)  = ifl21
      ifl(23)  = ifl23
      ifl(213) = ifl213
      motdfl   = svmdfl
      ifl(42)  = ifl42
      ifl(154) = ifl154
      sc(25)   = cs1thk
      sc(26)   = cs2thk 
      return
      end
