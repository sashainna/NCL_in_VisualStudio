C************************************************************************
c** copyright (c) 1988,1989   mills data systems corporation
c*  CONTAINS:
c*
c*           mocomb
c*           do_mover
c*           uv_save
c*           uv_reset
c*           fancmb
c*
C*     MODULE NAME AND RELEASE LEVEL
C*       mocomb.f , 25.2
C*    DATE AND TIME OF LAST MODIFICATION
C*       09/09/15 , 13:32:03
c **********************************************************************
c **********************************************************************
c **  program name: mocomb                                            **
c **                                                                  **
c **  written 17-apr-89   pem                                         **
c **                                                                  **
c **  purpose of program:    controls the combination fan and         **
c **                         parelm/tands motion sequence.  is        **
c **                         called by mocntl and calls premov,       **
c **                         mover, etc.                              **
C **********************************************************************
C
      subroutine mocomb

      include 'com8a.com'
      include 'mocom.com'
c
c...Local variables
c
      real*8 asn
      integer*2 ksn(4),ksn1
      real*4 bsn(2)
      equivalence (asn,bsn,ksn)
      real*8 sch(22),fwd(3),fv(3),dhld(6)
      real*8 tool_rad,tool_diam,sch54,zero,f_mag,f_dot
      parameter (zero = 0.d0)
      real*4 dpmin,tool18,tool19,max2_r4
      real*4 svuv(6)
      integer*4 isv2(2),iept(2)
      integer*2 isv1,iclf,ifl94, iflg
      logical lv84,lv90,lv91,lv93,lv95,lsvuv(3),do_fan
c
c...Debug variables
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)

      mocom = .true.
c
c...init auto maxdp loop counter and save maxdp,distance moved,numpts
c
c
      ASN = SC(162)
      dpmin = bsn(2)
      ifl94 = ifl(94)

      lv84 = sc(169).lt.8.499d0
      lv90 = sc(169).lt.9.049d0
      lv91 = sc(169).lt.9.149d0
      lv93 = sc(169).lt.9.349d0
      lv95 = sc(169).lt.9.549d0
c
c... store sc tbl,params,flgs for reset at exit
c... leave-dist = tool(18), approach-dist = tool(19)
c
      call conv8_8(sc,sch,22)
      call conv8_8(d(101),dhld,6)

      tool_diam = tool(1)
      tool_rad  = 0.5*tool_diam
      tool18 = tool(18)
      tool19 = tool(19)

1     continue

      do_fan = .false.
      call uv_save (lsvuv,svuv)

      iclf = 0
      ifl23  = ifl(23)
      ifl213 = ifl(213)
      ifl42  = ifl(42)
      ifl154 = ifl(154)
      cs1thk = sc(25)
      cs2thk = sc(26)
      isv1   = istat(1)
      call ncl_setptr(imotp, isv2)
      call ncl_zroptr(iept)
      sc(14) = zero
      midmod = 3
      if (ifl23.eq.9) midmod = 6
      if (lv90) goto 30
c
c... jingrong 06/24/99 do fan for the whole motion to calculate the total 
c... distance travelled.
c...Call domove instead of mover so that
c...Auto-Maxdp is in effect
c...Bobby  -  3/2/00
c
      svdism = 0.
      ifl(42) = 2
      ifl(213) = 0
      ifl(335) = 1
      ifl(23) = 4
      sc(160) = 0.
      sc(176) = 0.
c
c... TANTO flag used to check if title angle need to be turned off
c
      ltanto = .FALSE.
      if (sch(12).eq.646) ltanto = .TRUE.

      if (DEBUGX .eq. 1) then
          write (dbuf,7000) sc(1),sc(2),sc(3),sc(4),sc(5),sc(6)
 7000     format ('Mocomb-I = ',6f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
c
c...Use AUTO-MAXDP with fan moves
c...Bobby  -  3/6/00
c
      if (lv91) then
          call premov
          if(ifl(2).gt.0) goto 10
          call mover
c         if (ifl(2).eq.141) goto 98
      else
          call vctovc(sc(1),sc(217))
          call vctovc(sc(4),sc(220))
          call domove
      endif
      if (ifl(2).eq.141 .or. lavdd) goto 98
      lavoid = .false.
      if(ifl(2).gt.0) goto 10
c
c... if the total travelled dis is smaller than leaving dis, exit.
c
      if (sc(160).lt.1.5*tool(18)) then
         ifl(42) = ifl42
         do_fan = .true.
         goto 50
      end if
      svdism = sc(160)
c
c...Store the last motion point for combined motion
c
      call conv8_8 (sc,savtb,3)
c
c... ifl(352) flag for motion combined. ifl(352) = 0 means do sub motion
c... without checking totdis; ifl(352)= 1 means we have a valid totdis,
c... check traveled dis of sub motion; ifl(352)=2 means sub motion has
c... exceeded 1.5* totdis. do the whole motion in fan.
c
      ifl(352) = 1	
10    continue
c
c... restore tool loc to entry values and reset all to
c... start motion combine
c... set dntcut on, slowdn off and sc(12) to 'on'
c
      call uv_reset (lsvuv,svuv)
      call conv8_8 (sch,sc,13)
      ifl(2) = 0
      ifl(335) = 0
      if (ifl(42).eq.0) then
        call ncl_eqlptr(isv2, imotp, iflg)
        if (iflg.eq.0) call cldel (iclf,isv2,iept,imotp)
        call ptclos
      endif

30    continue
      sc(160) = 0.
      sc(176) = 0.
      ifl(42) = 2
      ifl(213) = 0
      ifl(23) = midmod
      sc(12)  = CS_ON
c      call uv_save (lsvuv,svuv)
c
c... set ifl(298)=1 to ret from mover at startpt
c
      ifl(298) = 1
      ifl(154) = 2
      call premov
      if (ifl(2).lt.1) goto 12
c
c...vp 10/17/97 make sure ifl(298) is reset if error occur
c
      if (ifl(2).ne.308) then
         ifl(298) = 0
         ifl(154) = ifl154
         goto 98
      end if
c
c... if parelm & not ruled ds, set ta/tt,ds
c
      ifl(2)  = 0
      ifl(23) = 3
      midmod  = 3

12    call do_mover

      ifl(298) = 0
      ifl(154) = ifl154
      if (ifl(2).eq.466) goto 98
c
c... turn off ifl(298) and proj init fwd on pl perpto ta
c... init fwd is in t(7-9,1)
c
      call conv4_8 (t(7,1),fwd,3)
      dis = f_dot (fwd,sch(4))
      call uvcplvc (fwd,sch(4),fv,-dis)
c
c... error. init fwd vs tool ijk is ng
c
      if (f_mag(fv) .eq. zero) then
         ifl(2) = 117
         goto 98
      endif
c
c... build pl thru entry tool cl and add to dtbl (cs)
c
      call unitvc (fv,d(103))
      d(106) = f_dot (d(103),sch(1))
      asn    = zero
      ksn(1) = PLANE
      d(101) = asn
c
c... save this fwd sense
c
      call vctovc (d(103),sc(7))
c
c... csthk=adis minus half tool diameter
c
      sc(25) = tool(18) - 0.5*sc(28)
c
c... build sc message for past the cspl already in dtbl
c
      asn    = zero
      ksn(1) = GOFWD
      ksn(2) = 1
      sc(10) = asn
      sc(12) = CS_PAST
      asn    = zero
      ksn(4) = PLANE
      sc(13) = asn

      call premov
      if (ifl(2).gt.0) goto 98
      ifl(154) = 2
c
c...set ifl(298)=3 to indicate a special CS_PAST in csrel.
c...Only when driving TANTO a surface
c...jingrong 02/25/2000.
c
      if (sch(12).eq.646. .and. .not.lv91) ifl(298) = 3
      call do_mover
      ifl(298) = 0
      ifl(154) = ifl154

      if (ifl(2).eq.466) goto 98
c
c... build a pl thru tool cl and fan 'on' same
c... project fwd on pl perpto ta
c
      dis = f_dot (sc(7),sc(4))
      call uvcplvc (sc(7),sc(4),fv,-dis)
c
c... error. init fwd vs tool ijk is ng
c
      if (f_mag(fv) .eq. zero) then
         ifl(2) = 117
         goto 98
      endif
c
c... load pl directly in dtbl
c
      call unitvc (fv,d(103))
      d(106) = f_dot (d(103),sc(1))
c
c... restore tool loc to entry values and reset all to
c... do fan1 motion
c
      call uv_reset (lsvuv,svuv)
      call conv8_8 (sch,sc,6)
      ifl(42) = ifl42
      ifl(23) = 4
      sc(12)  = CS_ON
      ksn(1)  = 0
      sc(13)  = asn
      
      sc160 = sc(160)
c
c... aak 18-feb-1998: reset travelled distance (used in "motgxx"
c... in the case of several CS's.
c
      sc(160) = zero
      sc(176) = zero
c
c... turn off tilt angle and appoach move flag for leave motion if available
c
      if (.not. ltanto) then
         lmdnow = .FALSE.
      endif
      lapmov = .FALSE.
         
      ifl(2) = 0
C   
C...If the leaving distance is less than or equal to 0 then
C...skip the initial fanning. JLS 6/10/99
C   
      if (tool(18).le.0.0) then
          ifl(352) = 1
c
c..... set the correct forward since the the command now is GF and the
c..... original vector set for GL or GR could be wrong
c
          if (.not.lv93) then
            sc(7) = fwd(1)
            sc(8) = fwd(2)
            sc(9) = fwd(3)
          endif
          goto 88
      endif

13    call premov
      if (ifl(2).gt.0) goto 98
      call mover
      if (ifl(2) .eq. 9595) then
c
c..... don't do circular interpolation for this motion
c
        ifl(94) = 0
        ifl(2) = 0
        goto 13
      endif
      ifl(94)=ifl94

      if (ifl(42) .eq. 0) ifl(335) = 1
c
c... aak OCT-01-97: new error handling for fan1 motion:
c... if conditions too severe for Fan1 motion because leave-dist too small, 
c... try leave-dist = tool(1)/2 and tool(1) ; if neither is O.K., give 
c... error ifl(2) = 467 and get out
c... changed 19-feb-1998: if neither is O.K., redo the whole move in fan.
c
      if (ifl(2).gt.0) then
         if(lv84) goto 98

         if (tool(18).ge.tool_diam) then
            do_fan = .true.
         else if (tool(18).ge.tool_rad) then
            tool(18) = tool_diam + 1.e-5
         else
            tool(18) = tool_rad  + 1.e-5
         endif

         goto 50
      endif
c
c... jingrong 06/24/99 if the dis traveled in fan1 already exceeds the tot dis
c... give up and do the whole motion in fan.
c
      if (ifl(352).eq.2) then
         do_fan = .true.
         goto 50
      end if

      if (.not.lv84) call uv_set 
c
c... now do mid range motion.  this pass ends 'to' the entry
c... cs where  csthk=bdis-tonp* cs1thk. 
c
c....The above holds for lv91 - 08/15/2000, eduard. Now the midmotion
c....respects the real CS condition, and the approach distance figures
c....in the thickness parameter - except when the real condition is
c....CS_ON we drive 'to CS'
c
88    continue
      ifl(23) = midmod
      sc(12)  = CS_TO
      ksn1 = sch(12)

c
c...Respect thick and CS condition
c...If approach distance is 0
c...Bobby  -  5/9/00
c
      if (tool(19) .eq. 0) ifl(213) = ifl213
c
c... aak 27-mar-1998: for TANTO,DS/PS set ifl(298) = 2 flag to 
c... take into account leave dist. as CS thick in csrel for middle motion
c
      if (ksn1 .eq. TNTO) then
         sc(12) = sch(12)
         ifl(298) = 2
         sc(25) = tool(19)
      else if (ksn1.eq.CS_ON) then
         if (tool(19) .eq. 0) then
            sc(12) = sch(12)
            sc(25) = 0.
         else if (lv91) then
            sc(25) = tool(19)
         else
            if (lv95) then
            sc(25) = tool(19) - tool_rad
            else
            sc(12) = sch(12)
            ifl(298) = 2
            sc(25) = tool(19)
            endif
         endif
      else if (ksn1.eq.CS_PAST) then
         if (tool(19) .eq. 0) then
            sc(12) = sch(12)
            sc(25) = cs1thk
         else if (lv91) then
            sc(25) = tool(19) - cs1thk
         else
            sc(12) = sch(12)
            sc(25) = cs1thk - tool(19)
         endif
c..... the rest is when sch(12) is CS_TO
      else 
         sc(25) = tool(19) + cs1thk
      endif

      sc(13) = sch(13)
      call conv8_8(dhld,d(101),6)
c
c... aak 20-may-1998: if using CS nearpt, restore the nearpt modifier
c... before doing the middle motion.
c
      sc(14) = sch(14)
c
c.....turn on tilt angle for middle motion if available
c
      if (sc(81) .ne. 0.0 .or. sc(82) .ne. 0.0 .or. sc(83).ne. 0.0) then
         lmdnow = .TRUE.
      endif 
      
      ifl(2) = 0

14    call premov
      if (ifl(2).gt.0) goto 98
      call mover
      if (ifl(2) .eq. 9595) then
c
c..... don't do circular interpolation for this motion
c
        ifl(94) = 0
        ifl(2) = 0
        goto 14
      endif
      ifl(94)=ifl94
c
c... aak 27-mar-1998: reset the ifl(298) flag which could 
c... have been changed for TANTO,DS/PS.
c
      ifl(298) = 0
      if (ifl(42) .eq. 0) ifl(335) = 1
c
c... if middle motion failed due to NUMPTS error, redo the whole move in FAN
c... same if it failed with 'Aborted Due to Discontinuity' error.
c... jingrong 06/24/99 if the dis traveled in midmod already exceeds the
c... tot dis, give up and do the whole motion in fan.
c
      if (ifl(2) .eq. 125 .or.
     1    ifl(2) .eq. 141 .or.
     2    ifl(2) .eq. 255 .or.
     2    ifl(2) .eq. 256 .or.
     3    ifl(352).eq.2) then
         do_fan = .true.
         goto 50
      endif

      if (ifl(2) .gt. 0)   goto 98
      if (.not.lv84) call uv_set 
c
c... final fan motion t,o,p entry cs
c... restore slowdn
c
      ifl(213) = ifl213
      sc(25)   = cs1thk
      sc(12)   = sch(12)

      ifl(23) = 4
      ifl(2)  = 0
C
C...If the approach distance (tool(19)) is equal to 0, then we can
C...exit, skipping the final fanning motion.  JLS 6/10/99
C
      if (tool(19).le.0.0) goto 99
c
c.....turn off tilt angle and turn on approach move flag for approach motion
c
      if (.not.ltanto) then
        lmdnow = .FALSE.
      endif
      lapmov = .TRUE.
      
      sch54 = sc(54)
      if (.not.lv91 .and. tool(19).lt.sch54) 
     *   sc(54) = max2_r4(tool(19),dpmin)
15    call premov
      if (ifl(2) .gt. 0) goto 98
      call mover
      if (ifl(2) .eq. 9595) then
c
c..... don't do circular interpolation for this motion
c
        ifl(94) = 0
        ifl(2) = 0
        goto 15
      endif
      sc(54) = sch54
      ifl(94) = ifl94

      if (ifl(352).eq.2) then
         do_fan = .true.
         goto 50
      endif

      if (ifl(2) .le. 0) goto 99
c
c... aak OCT-01-97: new error handling for fan2 motion:
c... if conditions too severe for Fan2 motion because approach-dist too small, 
c... try approach-dist = tool(1)/2 and tool(1) ; if neither is O.K., give 
c... error ifl(2) = 467 and get out
c... changed 19-feb-1998: if neither is O.K., redo the whole move in fan.
c
      if (ifl(2).gt.0 .and. .not.lv84) then

         if (tool(19) .ge. tool_diam) then
            do_fan = .true.
         else if (tool(19) .ge. tool_rad) then
            tool(19) = tool_diam + 1.e-5
         else
            tool(19) = tool_rad  + 1.e-5
         endif

      endif
c
c... old behaviour if fan2 failed: 
c... come here for numpts errs - do whole move in fan (for NCL <= 8.5)
c
50    call uv_reset (lsvuv,svuv)
      call conv8_8 (sch,sc,13)
      call conv8_8 (dhld,d(101),6)
      ifl(2) = 0
      ifl(352) = 0
      if (.not.lv93) mocom = .false.
c
c...enable 5210 record again since erroneous motion is erased
c
      ifl(335) = 0
c
c...Changed to new clfile storage method
c...Bobby  -  12/18/92
c
      call ncl_eqlptr(isv2, imotp, iflg)
      if (iflg .eq. 0) call cldel (iclf,isv2,iept,imotp)
      call ptclos

      ifl(213) = ifl213
      sc(25)   = cs1thk
c
c... if the middle motion failed or FAN1/FAN2 motions failed after all
c... attempts, redo the whole move in FAN 
c
      if (.not.lv84 .and. .not.do_fan) then
        if (.not.lv93) ifl(23) = ifl23
        goto 1
      endif
c
c... aak 18-feb-1998: reset travelled distance (used in "motgxx"
c... in the case of several CS's.
c
      sc(160) = zero
      sc(176) = zero

      ifl(23) = 4
c
c...Call domove instead of mover so that
c...Auto-Maxdp is in effect for whole move in fan
c...Bobby  -  3/2/00
c
      if (lv91) then
          call premov
          if (ifl(2) .gt. 0) goto 98
          call mover
          if (ifl(2) .lt. 1) goto 99
      else
          call vctovc(sc(1),sc(217))
          call vctovc(sc(4),sc(220))
          call domove
      endif
      if (ifl(2) .gt. 0) goto 98
      goto 99
c
c... error exit.  restore sc(1-9) to entry values
c... (may have chgd during mover calls)
c... restore flags, params at exit
c
98    call conv8_8 (sch,sc,9)
99    ifl(42)  = ifl42
      ifl(213) = ifl213
      ifl(23)  = ifl23
      ifl(335) = 0
      sc(25)   = cs1thk
      sc(26)   = cs2thk
      if (.not.lv84) call uv_set 
      ifl(352) = 0
      call conv8_8 (dhld,d(101),6)
      if (.not.lv93) then
        tool(18) = tool18
        tool(19) = tool19
      endif
c
c.....restore tilt angle if availble
c
      if (sc(81) .ne. 0.0 .or. sc(82) .ne. 0.0 .or. sc(83) .ne.0.0) then
         lmdnow = .TRUE.
      endif 
      lapmov = .FALSE.
      
      if (sch(12).eq.646.) call ncl_2sfbnry_free
      mocom = .false.

      return
      end
c
c**********************************************************************
c   SUBROUTINE: do_mover
c
c   FUNCTION:  Calls mover and checks error status after motion.
c              Used in mocomb to perform probing move.
c
c     INPUT:  none
c
c     OUTPUT: none
c
c**********************************************************************
      subroutine do_mover
c
      include 'com8a.com'
c
      logical lv84
c
      lv84 = sc(169) .lt. 8.499d0
      call mover
      if (ifl(2) .lt. 1) then
        if (ifl(42).eq.0) then
cc          call moters
cc          call motbgn
        endif
      else if (ifl(2).eq.466) then
          goto 900
      else
c
c... aak 31-OCT-1997: reset error flag (if this motion failed)
c
          ifl(2) = 0
      endif
c
  900 if (.not.lv84) call uv_set 
      return
      end
c
c**********************************************************************
c   SUBROUTINE: uv_save (lsuv,gsuv)
c
c   FUNCTION:  Store SFs parameters saved in the common block,
c              so it can be used lately if the next motion is dummy.   
c
c     INPUT:  none
c
c     OUTPUT: lsuv   L*2  D3  -  saved psuv, dsuv, csuv flags.
c
c             gsuv   R*4  D6  -  saved PS, DS and CS parameters.
c
c**********************************************************************
      subroutine uv_save (lsuv,gsuv) 
c
      include 'com8a.com'
c
      logical lsuv(3)
      real*4 gsuv(6)
c
      include 'suvcom.com'
c
c... aak 20-may-1998: if not csuv, set csu=csv=0.5 as it's done
c... in domove
c
      if (.not.csuv) call getsuv(sc(13),3,csu,csv)

      gsuv(1) = psu
      gsuv(2) = psv
      gsuv(3) = dsu
      gsuv(4) = dsv
      gsuv(5) = csu
      gsuv(6) = csv

      lsuv(1) = psuv
      lsuv(2) = dsuv
      lsuv(3) = csuv
c
      return
      end
c
c**********************************************************************
c   SUBROUTINE: uv_reset (lsuv,gsuv)
c
c   FUNCTION:  Sets SFs parameters in the common block to values
c              specified in lsuv, gsuv.   
c
c     INPUT:   lsuv   L*2  D3  -  saved psuv, dsuv, csuv flags.
c
c              gsuv   R*4  D6  -  saved PS, DS and CS parameters.
c
c     OUTPUT:  none
c
c**********************************************************************
      subroutine uv_reset (lsuv,gsuv) 
c
      include 'com8a.com'
c
      logical lsuv(3)
      real*4 gsuv(6)
c
      include 'suvcom.com'
c
      psu  = gsuv(1)
      psv  = gsuv(2)
      dsu  = gsuv(3)
      dsv  = gsuv(4)
      csu  = gsuv(5)
      csv  = gsuv(6)

      psuv = lsuv(1)
      psuv = lsuv(2)
      psuv = lsuv(3)
c
      return
      end

c **********************************************************************
c **********************************************************************
c **  program name: fancmb                                            **
c **                                                                  **
c **  purpose of program:    runs a fan motion as two fans, by        **
c **                         constructing an intermediate plane.      **
c **                         called by domove after the original      **
c **                         fan failed.                              **
C **********************************************************************
C
      subroutine fancmb

      include 'com8a.com'
      include 'mocom.com'

c
c...Local variables
c
      real*8 asn
      integer*2 ksn(4)
      equivalence (asn,ksn)
      real*8 sch(22),dhld(6)
      real*8 f_dot
      real*4 svuv(6)
      logical lsvuv(3)

      common/fancmbcom/fan160,lfncmb
      real*8 fan160
      integer*2 lfncmb

c
c... store sc tbl,params,flgs for reset at exit
c
      call conv8_8(sc,sch,22)
      call conv8_8(d(101),dhld,6)


      lfncmb = 1
      fan160 = sc(160)
      call uv_save (lsvuv,svuv)

      ifl23  = ifl(23)
      ifl213 = ifl(213)
      ifl42  = ifl(42)
      sc(14) = 0

      ifl(42) = 2
      ifl(213) = 0
      ifl(335) = 1
      ifl(23) = 4
      sc(160) = 0.
      sc(176) = 0.

      ifl(23) = 3
      call premov
      if (ifl(2).gt.0) goto 98
      call mover


      if (lfncmb.eq.1) ifl(2) = 255
      if (ifl(2).gt.0) goto 98

      asn    = 0
      ksn(1) = PLANE
      d(101) = asn
c
c... build a pl thru toolend perpendicular to PS and DS
c... load pl directly in dtbl
c
      call unitvc (sc(7),d(103))
      d(106) = f_dot (d(103),sc(1))
c
c... restore tool loc to entry values and reset all to
c... do fan1 motion
c
      call uv_reset (lsvuv,svuv)
      call conv8_8 (sch,sc,11)
      ifl(42) = ifl42
      ifl(23) = 4
      sc(12)  = CS_ON
      asn = 0
      ksn(4) = PLANE
      sc(13) = asn
c
c.....reset travelled distances
c
      sc(160) = 0
      sc(176) = 0
c
c..... first fan to the middle plane
c
      call premov
      if (ifl(2).gt.0) goto 98
      call mover
      if (ifl(2).gt.0) goto 98

      call uv_set 

c
c..... restore the original fan parameters
c
      asn    = 0
      ksn(1) = GOFWD
      ksn(2) = 1
      sc(10) = asn

      ifl(213) = ifl213
      sc(12)   = sch(12)
      sc(13)   = sch(13)
      call conv8_8(dhld,d(101),6)
c
c... if using CS nearpt, restore the nearpt modifier
c... before doing the second fan.
c
      sc(14) = sch(14)

      call premov
      if (ifl(2) .gt. 0) goto 98
      call mover
      if (ifl(2) .le. 0) goto 99
c
c... error exit.  restore sc(1-9) to entry values
c... restore flags, params at exit
c
98    call conv8_8 (sch,sc,9)
99    ifl(42)  = ifl42
      ifl(213) = ifl213
      ifl(23)  = ifl23
      ifl(335) = 0
      call uv_set 
      call conv8_8 (dhld,d(101),6)

      lfncmb = 0
      fan160 = 0

      return
      end
