c*********************************************************************
c*    NAME         :  yfmotsup.f
c*       CONTAINS:
c*
c*          ymtool
c*          ymfwd
c*          yindpt
c*          yindvc
c*          ygchk
c*          ygchk1
c*          ymaxan
c*          ymaxdp
c*          ynumpt
c*          ythick
c*          ytlaxs
c*          ytoler
c*          ycntct
c*          yfdrat
c*          ysfvct
c*          yautst
c*          yautst1
c*          ypscnd
c*          yfilet
c*          yfnint
c*
c*    COPYRIGHT 1996 (c) Numerical Control Computer Sciences Inc.
c*              All Rights Reserved.
c*    MODULE NAME AND RELEASE LEVEL
c*       yfmotsup.f , 26.3
c*    DATE AND TIME OF LAST  MODIFICATION
c*       05/22/18 , 10:29:07
c*********************************************************************
c
c*********************************************************************
c*    E_FUNCTION     : ymtool(pt)
c*       This function returns the current tool position.
c*    PARAMETERS
c*    INPUT  :
c*       none
c*    OUTPUT :
c*       pt           Coordinates and vector of current tool position.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c*********************************************************************
c
      subroutine ymtool(pt)
c
      include 'com8a.com'
c
      real*8 pt(6)

      call conv8_8 (sc,pt,6)
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ymfwd(vec)
c*       This function returns the current forward direction.
c*    PARAMETERS
c*    INPUT  :
c*       none
c*    OUTPUT :
c*       vec          Current forward direction.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c*********************************************************************
c
      subroutine ymfwd (vec)
c
      include 'com8a.com'
c
      real*8 vec(3)

      call vctovc (sc(7),vec)
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yindpt(pt,ierr)
c*       This function sets the forward direction using a point.
c*    PARAMETERS
c*    INPUT  :
c*       pt           Forward direction.
c*    OUTPUT :
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c*********************************************************************
c
      subroutine yindpt (pt,ierr)
c
      include 'com8a.com'
c
      real*8 pt(3)
      integer*4 ierr
c
      real*8 vec(3)

      ierr = 0
      vec(1) = pt(1) - sc(1)
      vec(2) = pt(2) - sc(1)
      vec(3) = pt(3) - sc(1)
      call yindvc (vec,ierr)

      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yindvc(vec,ierr)
c*       This function sets the forward direction using a vector.
c*    PARAMETERS
c*    INPUT  :
c*       vec          Forward direction.
c*    OUTPUT :
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yindvc (vec,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr
      real*8 vec(3)
c
      real*8 f_mag
c
      if (f_mag(vec) .eq. 0.) Then
          ierr = 121
      else
          ierr = 0
          call unitvc(vec,sc(7))
          ifl(22) = 1
      endif
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ygchk(ig,iway,ierr)
c*       This function sets/returns the GOUGCK level.
c*    PARAMETERS
c*    INPUT  :
c*       ig           Current GOUGCK level.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       ig           Current GOUGCK level.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ygchk (ig,iway,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr,ig,iway
c
      if (iway .eq. 0) then
          ig = ifl(6)
          if (idsgck .gt. ig) ig = idsgck
          if (icsgck .gt. ig) ig = icsgck
      else
          ierr = 0
          if (ig .lt. 0) ig = 0
          if (ig .gt. 4) ig = 4
          ifl(6) = ig
          if (ig .gt. 3) ifl(6) = 3
          idsgck = ig
          icsgck = ig
      endif
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ygchk1(igps,igds,igcs,iway,ierr)
c*       This function sets/returns the GOUGCK level.
c*    PARAMETERS
c*    INPUT  :
c*       igps         Current PS GOUGCK level.
c*       igds         Current DS GOUGCK level.
c*       igcs         Current CS GOUGCK level.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       ig           Current GOUGCK level.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ygchk1 (igps,igds,igcs,iway,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr,igps,igds,igcs,iway
c
      if (iway .eq. 0) then
          igps = ifl(6)
          igds = idsgck
          igcs = icsgck
      else
          ierr   = 0
          if (igps .lt. 0) igps = 0
          if (igds .lt. 0) igds = 0
          if (igcs .lt. 0) igcs = 0

          if (igps .gt. 3) igps = 3
          if (igds .gt. 4) igds = 4
          if (igcs .gt. 4) igcs = 4

          ifl(6) = igps
          idsgck = igds
          icsgck = igcs
      endif
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ymaxan(ang,iway,ierr)
c*       This function sets/returns the MAXANG value.
c*    PARAMETERS
c*    INPUT  :
c*       ang         Current MAXANG value.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       ang          Current MAXANG value.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ymaxan (ang,iway,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr,iway
      real*8 ang
c
      if (iway .eq. 0) then
          ang = sc(80)
      else
          ierr = 0
          sc(80) = ang
          sc(201) = ang
      endif
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ymaxdp(imx,rmx,iway,ierr)
c*       This function sets/returns the MAXDP value.
c*    PARAMETERS
c*    INPUT  :
c*       imx         Current MAXDP integer values.
c*       rmx         Current MAXDP real values.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       imx         Current MAXDP integer values.
c*       rmx         Current MAXDP real values.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ymaxdp (imx,rmx,iway,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr,imx(4),iway
      real*8 rmx(2)
c
      integer*2 ksn(4),istep
      real*4 bsn(2)
      real*8 asn
c
      equivalence (asn,bsn,ksn)
      equivalence (lstep,istep)
c
c...Process command
c
      if (iway .eq. 0) then
          if (sc(162) .eq. 0.) then
              imx(1) = 0
              imx(3) = 10
              imx(4) = 0
              rmx(1) = .01
          else
              asn = sc(162)
              imx(1) = 1
              imx(3) = ksn(1)
              imx(4) = ksn(2)
              rmx(1) = bsn(2)
          endif
c
c...strangely, when lstep is true, istep = -1
c...          imx(2) = istep
c
          if (lstep.eq..TRUE.) then
              imx(2) = 1
          else
              imx(2) = 0
          endif
          rmx(2) = sc(54)
      else
          ierr = 0
          if (imx(1) .eq. 0) then
              sc(162) = 0
          else
              ksn(1)  = imx(3)
              ksn(2)  = 0
              bsn(2)  = rmx(1)
              sc(162) = asn
          endif
          sc(54) = rmx(2)
          sc(105) = rmx(2)
          sc(214) = sc(162)
          istep  = imx(2)
          lstepv = lstep
      endif
c
c...End of routine
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ynumpt (numpts,iway,ierr)
c*       This function sets/returns the NUMPTS value.
c*    PARAMETERS
c*    INPUT  :
c*       numpts       Current NUMPTS values.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       numpts       Current NUMPTS values.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ynumpt (numpts,iway,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr,numpts,iway
c
      if (iway .eq. 0) then
          numpts = ifl(91)
      else
          ierr = 0
          ifl(91) = numpts
          ifl(368) = numpts
      endif
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ythick(thk,iway,ierr)
c*       This function sets/returns the THICK value.
c*    PARAMETERS
c*    INPUT  :
c*       thk          Current THICK values.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       thk          Current THICK values.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ythick (thk,iway,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr,iway
      real*8 thk(7)
c
c...Process command
c
      if (iway .eq. 0) then
          call vctovc (sc(23),thk)
	  thk(3) = sc(132)
          do 10 i =4,7
             thk(i) = sc(173+i)
10        continue
      else
          ierr = 0
          call vctovc (thk,sc(23))
          sc(132) = thk(3)
          do 20 i =1,4
             sc(176+i) = thk(3+i)
20        continue
      endif
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ytlaxs(ktl,gtl,iway,ierr)
c*       This function sets/returns the TLAXIS mode.
c*    PARAMETERS
c*    INPUT  :
c*       ktl          Current TLAXIS integer values.
c*       gtl          Current TLAXIS real values.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       ktl          Current TLAXIS integer values.
c*       gtl          Current TLAXIS real values.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c*********************************************************************
c
      subroutine ytlaxs(ktl,gtl,iway,ierr)
c
      include 'com8a.com'
      include 'const.com'
      include 'mocom.com'
      include 'gidcom.com'
c
      common /tpvcom/ tavend
      real*8 tavend(3)
c
c...MOCOM equivs
c
      integer*4 ierr,iway,ktl(25)
      real*8 gtl(30)
c
      integer*2 i1,i2
      real*8 sx,sy,sz,sec,spshld
c
c...ktl(1)  = Tlaxis mode.
c...ktl(2)  = Normal flag for fixed tlaxis.
c...ktl(3)  = Parelm flag for TANTO ds.
c...ktl(4)  = Perpto flag for TANTO ds.
c...ktl(5)  = Contact flag for ATANGL ps.
c...ktl(6)  = Thru curve key.
c...ktl(7)  = Thru curve relation number.
c...ktl(8)  = Guide curve flag.
c...ktl(9)  = Guide curve key.
c...ktl(10) = Guide curve relation number.
c...ktl(11) = Guide curve contact flag.
c...ktl(12) = Guide curve tool condition. (0=SAME,1=TLLFT,2=TLON,3=TLRGT)
c...ktl(13) = Gouge avoidance flag.
c...ktl(14) = Adjust (MODIFY) flag.
c...ktl(15) = Right/Fwd Tilt angles.
c...ktl(16) = Secondary PS flag.
c...ktl(17) = Secondary PS key.
c...ktl(18) = Secondary PS relation number.
c...ktl(19) = lock mode
c...ktl(20) = lock transition
c...ktl(21) = lock radius mode
c...ktl(22) = Center condition (used for CS distance and apex point
c...          calculation with FAN (OFF,ON,AUTO)
c...ktl(23) = Perpto Last flag.
c
c...gtl(1:3) = Tlaxis vector.
c...      4  = TANTO height.
c...      5  = COMBIN departure distance.
c...      6  = COMBIN approach distance.
c...      7  = ATANGL angle.
c...      8  = CLDIST heel clearance.
c...   9:11  = PERPTO vector.
c...  12:14  = THRU point.
c...     15  = THRU curve distance.
c...     16  = Modify Right tilt angle.
c...     17  = Modify Forward tilt angle.
c...     18  = Guide curve offset.
c...     19  = MODIFY right offset.
c...     20  = MODIFY forward offset.
c...     21  = MODIFY up offset.
c...     22  = MODIFY right angle.
c...     23  = MODIFY forward angle.
c...     24  = Guide curve initial u value
c...     25  = Tool axis lock distance.
c...     26  = Tool axis lock interpolation distance.
c
c...Get TLAXIS Mode
c
      if (iway .eq. 0) then
c
c......TLAXIS/mode
c
          ktl(1) = ifl(23)
c
c......TLAXIS/SAME,NORMAL
c
          ktl(2) = 0
          if (ifl(23) .eq. 0) ktl(2) = ifl(293)
c
c......TLAXIS/vec
c
          call vctovc(sc(4),gtl)
c
c......TLAXIS/TANTO,DS,height
c
          if (ifl(23) .eq. 3 .or. ifl(23) .eq. 4 .or.
     1        ifl(23) .eq. 5 .or. ifl(23) .eq. 6 .or.
     2        ifl(23) .eq. 8 .or. ifl(23) .eq. 9) gtl(4) = tool(10)
c
c......TLAXIS/COMBIN,height,depart,approach
c
          if (ifl(23) .eq. 8 .or. ifl(23) .eq. 9) then
              gtl(5) = tool(18)
              gtl(6) = tool(19)
          endif
c
c......TLAXIS/ATANGL
c
          if (ifl(23) .eq. 2 .or. ifl(23) .eq. 10 .or.
     1        ifl(23) .eq. 11 .or. ifl(23) .eq. 12) then
              gtl(7) = asin(tool(8)) * RADIAN
c
c.........CLDIST
c
              if (ifl(23) .eq. 11 .or. ifl(23) .eq. 12) then
                  gtl(8) = tool(10)
              else
                  gtl(8) = 0
              endif
c
c.........CONTCT
c
              ktl(5) = 0
              if (cntctp) ktl(5) = 1
          endif
c
c......TLAXIS/PARELM
c
          ktl(3) = 0
          if (ifl(23) .eq. 6 .or. ifl(23) .eq. 9) ktl(3) = 1
c
c......TLAXIS/PERPTO
c
          ktl(4) = 0
          if (ifl(23) .eq. 5 .or. ifl(23) .eq. 7 .or.
     1        ifl(23) .eq. 10 .or. ifl(23) .eq. 12) then
              ktl(4) = 1
              ktl(23) = 0
              if (prplst) ktl(23) = 1
              call conv4_8(tool(18),gtl(9),3)
          endif
c
c......TLAXIS/THRU,PT
c
          if (ifl(23) .eq. 13) call conv4_8(tool(18),gtl(12),3)
c
c......TLAXIS/THRU,CURVE
c
          if (ifl(23) .eq. 14) then
              call gtdesc (tool(19),ktl(6),i1,i2)
              ktl(7) = i2
              gtl(15) = tool(18)
          endif
c
c......TLAXIS/INTERP,ve
c
          if (ktl(1) .eq. 15) then
              call vctovc (tavend,gtl(1))
          endif
c
c......TLAXIS/...RIGHT,FWD
c
          ktl(15) = 0
          if (lmdnow) then
              ktl(15) = 1
              sx = sc(81)
              sy = sc(82)
              sz = sc(83)
c
              gtl(16) = ZERO
              sec = dsqrt(sx**2+sz**2)
              if (sec.gt.ZERO) gtl(16) = dacos(sz/sec) * RADIAN
              if (sx.lt.ZERO)  gtl(16) = -gtl(16)
c
              gtl(17) = ZERO
              sec = dsqrt(sy**2+sz**2)
              if (sec.gt.ZERO) gtl(17) = dacos(sz/sec) * RADIAN
              if (sy.lt.ZERO)  gtl(17) = -gtl(17)
          endif
c
c..... CENTER
c
          ktl(22) = ifl(289)
c
c......TLAXIS/...GUIDE,CURVE
c
          ktl(8) = 0
          if (lcvgid) then
              ktl(8) = 1
              call gtdesc (gidasw,ktl(9),i1,i2)
              ktl(10) = i2
              if (ltltct) then
                ktl(11) = 1
              else
                ktl(11) = 0
              endif
              ktl(12) = gidsid
              gtl(18) = gidthk
          endif
c
c.......PS,sf1 - secondary PS
c
          ktl(16) = 0
          if (lsecps) then
              ktl(16) = 1
              spshld = sc(195)
              call gtdesc (spshld,ktl(17),i1,i2)
              ktl(18) = i2
          endif
c
c......TLAXIS/...,GOUGCK
c
          if (ldschk) then
            ktl(13) = 1
          else
            ktl(13) = 0
          endif
c
c......TLAXIS/...,LOCK
c
          ktl(19) = ifl(365)
          ktl(20) = ifl(364)
          ktl(21) = ifl(366)
          gtl(25) = sc(196)
          gtl(26) = sc(197)
c
c......TLAXIS/MODIFY
c
         ktl(14) = ifl(104)
         if (ifl(104) .eq. 1) then
             call conv4_8 (tool(12),gtl(19),3)
c
             sx = tool(15)
             sy = tool(16)
             sz = tool(17)
c
             gtl(22) = ZERO
             sec = dsqrt(sx**2+sz**2)
             if (sec.gt.ZERO) gtl(22) = dacos(sz/sec) * RADIAN
             if (sx.lt.ZERO)  gtl(22) = -gtl(22)
c
             gtl(23) = ZERO
             sec = dsqrt(sy**2+sz**2)
             if (sec.gt.ZERO) gtl(23) = dacos(sz/sec) * RADIAN
             if (sy.lt.ZERO)  gtl(23) = -gtl(23)
         endif
c
c...Set tool axis parameters
c
      else
          ierr     = 0
          isc10(3) = 0
          isc10(4) = 0
          prplst = .false.
c
c......SAME
c
          if (ktl(1) .eq. -1) then
              isc10(2) = 2
              sc(11)   = 0.
              isc10(4) = ktl(2) 
c
c......FIXED
c
          else if (ktl(1) .eq. 0) then
              isc10(2) = 1
              isc10(4) = ktl(2)
              call vctovc(gtl,sc(11))
c
c......NORMAL
c
          else if (ktl(1) .eq. 1 .or. ktl(1) .eq. 7) then
              isc10(2) = ktl(1) + 1
              sc(11)   = ONE
c
c......ATANGL
c
          else if (ktl(1) .eq. 2 .or. ktl(1) .eq. 10 .or.
     *             ktl(1) .eq. 11 .or. ktl(1) .eq. 12) then
              isc10(2) = ktl(1) + 1
              sc(11)   = gtl(7)
              if (ktl(1) .eq. 11 .or. ktl(1) .eq. 12) sc(13) = gtl(8)
              sc(14) = ktl(5)
c
c......TANTO/FAN
c
          else if (ktl(1) .ge. 3 .and. ktl(1) .le. 6) then
              isc10(2) = ktl(1) + 1
              sc(11)   = gtl(4)
c
c......COMBIN
c
          else if (ktl(1) .eq. 8 .or. ktl(1) .eq. 9) then
              isc10(2) = ktl(1) + 1
              call vctovc(gtl(4),sc(11))
c
c......THRU,PT
c
          else if (ktl(1) .eq. 13) then
              isc10(2) = ktl(1) + 1
c
c... aak 08-dec-1997: added the next line 
c
              isc10(4) = 0
              call vctovc(gtl(12),sc(11))
c
c......THRU,CV
c
          else if (ktl(1) .eq. 14) then
              isc10(2) = 14
              isc10(4) = 1
              i1       = ktl(7)
              call ptdesc (ktl(6),i1,sc(11))
              sc(12) = gtl(15)
c
c......INTERP,ve
c
          else if (ktl(1) .eq. 15) then
              isc10(2) = 15
              call vctovc (gtl(1),sc(11))
          endif
c
c......PERPTO vector
c
          if (ktl(1) .eq. 5 .or. ktl(1) .eq. 7 .or. ktl(1) .eq. 10 .or.
     1        ktl(1) .eq. 12) then
              if (ktl(23) .eq. 1) prplst = .true.
              call vctovc (gtl(9),sc(87))
          endif
c
c.......RIGHT - FWD angles
c
          isc10(3) = 0
          sc(20) = 0
          sc(21) = 0
          if (ktl(15) .eq. 1) then
              isc10(3) = 2
              call conv8_8 (gtl(16),sc(20),2)
          endif
c
c..... CENTER
c
          ifl(289) = ktl(22)
c
c.......GUIDE,CV
c
          lcvgid = .false.
          if (ktl(8) .eq. 1) then
              lcvgid = .true.
              i1 = ktl(10)
              call ptdesc(ktl(9),i1,gidasw)
              ltltct = ktl(11) .gt. 0
              gidsid = ktl(12)
              gidthk = gtl(18)
              gthkon = 0
              if (gidthk .lt. -.0001 .or. gidthk .gt. .0001) gthkon = 1
              giduon = 1
              gidu0 = gtl(24)
              gidh = ZERO
          endif
c
c.......PS,sf1 - secondary PS
c
          lsecps = .false.
          if (ktl(16) .eq. 1 .and.
     x        (ktl(18).eq.plane .or. ktl(18).eq.surf)) then
              lsecps = .true.
              i1 = ktl(18)
              call ptdesc(ktl(17),i1,spshld)
              sc(195) = spshld
          endif
c
c.......GOUGCK
c
        ldschk = ktl(13) .gt. 0
c
c......TLAXIS/...,LOCK
c
          ifl(365) = ktl(19)
          ifl(364) = ktl(20)
          ifl(366) = ktl(21)
          sc(196)  = gtl(25)
          sc(197)  = gtl(26)
c
c......TLAXIS/MODIFY
c
         if (ktl(14) .eq. 1) then
             isc10(3) = isc10(3) + 1
             call conv8_8 (gtl(19),sc(15),5)
         endif
c
c......Process TLAXIS statement
c
         isc10(1) = 721
         ifl(2)   = 0
         call motimm
         ierr = ifl(2)
      endif
c
c...End of routine
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : subroutine ytoler(tol,iway,ierr)
c*       This function sets/returns the TOLER value.
c*    PARAMETERS
c*    INPUT  :
c*       tol          Current TOLER values.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       tol          Current TOLER values.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ytoler (tol,iway,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr,iway
      real*8 tol(5)
c
      if (iway .eq. 0) then
          tol(1) = sc(27)
          tol(2) = sc(167)
          tol(3) = sc(168)
          tol(4) = sc(91)
          tol(5) = sc(92)
      else
          ierr = 0
          sc(27)  = tol(1)
          sc(167) = tol(2)
          sc(168) = tol(3)
c FIXXES
          sc(91) = tol(4)
          sc(92) = tol(5)
      endif
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ycntct(icntct,iway,ierr)
c*       This function sets/returns the CONTCT mode.
c*    PARAMETERS
c*    INPUT  :
c*       icntct       Current CONTCT mode.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       icntct       Current CONTCT mode.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ycntct (icntct,iway,ierr)
c
      include 'com8a.com'
c
      integer*4 icntct, ierr, iway
c
      ierr = 0
      if (iway .eq. 0) then
          icntct = 0
          if (lcntct) icntct = 1
      else if (icntct.eq.1.or.icntct.eq.2) then
          lcntct = icntct.eq.1
      else
          ierr = 1
      endif
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yfdrat (ivals,dvals,iway,ierr)
c*       This function sets/returns the FEDRAT mode.
c*    PARAMETERS
c*    INPUT  :
c*       ivals        Current FEDRAT integer values.
c*       dvals        Current FEDRAT real values.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       ivals        Current FEDRAT integer values.
c*       dvals        Current FEDRAT real values.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfdrat (ivals,dvals,iway,ierr)
c
      include 'com8a.com'
      include 'comgt.com'
      include 'cutter.com'
c
      integer*4 ivals(5), ierr, iway
      real*8 dvals(6)

      real*8 FEEDR
      equivalence (FEEDR,MOTMAP(24))
c
      if (iway .eq. 0) then
          ivals(1) = ifl(315)
          ivals(2) = ifl(314)
          ivals(3) = FITYP
          ivals(4) = 0
          ivals(5) = 0
          if (sclout) ivals(4) = 1
          if (sclat) ivals(5) = 1
          dvals(1) = FEEDC(3)
          dvals(2) = FEDIS(2)
          dvals(3) = FEEDC(2+3*ivals(4))
          dvals(4) = FEDIS(1)
          dvals(5) = FEEDC(1+3*ivals(5))
          dvals(6) = FHIGT
      else
          ierr = 0
          ifl(315) = ivals(1)
          ifl(314) = ivals(2)
          FITYP = ivals(3)
          sclout = .false.
          sclat  = .false.
          if (ivals(4) .eq. 1) sclout = .true.
          if (ivals(5) .eq. 1) sclat = .true.
          FEEDC(FITYP) = dvals(1)
          FEDIS(2) = dvals(2)
          FEEDC(2+3*ivals(4)) = dvals(3)
          FEDIS(1) = dvals(4)
          FEEDC(1+3*ivals(5)) = dvals(5)
          FHIGT    = dvals(6)
      endif
c
      if (iway.eq.1) rpfron = .false.
c...      rpfron = .false.
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ysfvct (dsflag, dsvec, csflag, csvec ,ierr)
c*       This function sets the SRFVCT statement.
c*    PARAMETERS
c*    INPUT  :
c*       dsflag       1 = Set DS SRFVCT.
c*       dsvec        DS Surface vector.
c*       csflag       1 = Set DS SRFVCT.
c*       csvec        DS Surface vector.
c*    OUTPUT :
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine ysfvct (dsflag, dsvec, csflag, csvec ,ierr)

      include 'com8a.com'

      integer*4 dsflag, csflag, ierr
      real*8 dsvec(3), csvec(3)

      ierr = 0
      if (dsflag .eq. 1) then
        ldssvc = .true.
        call vctovc(dsvec,dssfvc)
      else
        ldssvc = .false.
      endif

      if (csflag .eq. 1) then
        lcssvc = .true.
        call vctovc(csvec,cssfvc)
      else
        lcssvc = .false.
      endif

      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yautst (iauto, iway, ierr)
c*       This function sets/returns the AUTO START mode.
c*    PARAMETERS
c*    INPUT  :
c*       iauto        1 = Auto Start is on.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       iauto        1 = Auto Start is on.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yautst (iauto, iway, ierr)

      include 'com.com'

      integer*4 iauto, iway, ierr
c
c...Get or set autostart
c
      if (iway .eq. 0) then
          iauto = 0
          if (autost) iauto = 1
      else
          ierr = 0
          autost = iauto.ne.0
      endif

      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yautst1 (iauto, omit, iway, ierr)
c*       This function sets/returns the AUTO START parameters.
c*    PARAMETERS
c*    INPUT  :
c*       iauto        1 = Auto Start is on.
c*       omit         1 iff on.
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       iauto        1 = Auto Start is on.
c*       omit         1 iff on.
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c*********************************************************************
c
      subroutine yautst1 (iauto, omit, iway, ierr)

      include 'com.com'
      include 'const.com'

      integer*4 iauto, omit, iway, ierr
c
c...Get or set autostart
c
      if (iway .eq. 0) then
          iauto = 0
          if (autost) iauto = 1
          omit = 0
          if (ifl(363) .eq. 1) omit = 1
      else
          ierr = 0
          autost = iauto.ne.0
          ifl(363) = 0
          if (omit .eq. 1) ifl(363) = 1
      endif

      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ypsis(nclkey,pl)
c*       This function returns the current part surface.  
c*    PARAMETERS
c*    INPUT  : none
c*    OUTPUT :
c*       nclkey       Key of part surface.  UU_NULL if the part surface
c*                    is a plane.
c*       pl           Canonical form (i,j,k,d) of planar part surface.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c*********************************************************************
c
      subroutine ypsis (nclkey,pl)

      include 'com.com'
      include 'mocom.com'

      integer*4 nclkey
c
      real*8 pl(4)
c
      integer*2 nwds,ietype
c
c...Get the part surface key
c
      nclkey = ifl4(17)
c
c...The part surface is planar
c
      if (nclkey .eq. 0) then
          pl(1) = d(3)
          pl(2) = d(4)
          pl(3) = d(5)
          pl(4) = d(6)
      endif

      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : ypscnd(icond,iway,ierr)
c*       This function sets/returns the part surface condition mode.
c*    PARAMETERS
c*    INPUT  :
c*       icond        1 = TLONPS, 0 = TLOFPS
c*       iway         0 = Return value , 1 = Set value
c*    OUTPUT :
c*       icond        1 = TLONPS, 0 = TLOFPS
c*       ierr         Non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c*********************************************************************
c
      subroutine ypscnd (icond,iway,ierr)

      include 'com.com'

      integer*4 icond, iway, ierr
      ierr = 0
c
c...Get or set Part surface condition, TLONPS or TLOFPS.
c
      if (iway .eq. 0) then
         icond = ifl(342)
      else
         ifl(342) = icond
      endif

      return
      end
c
C*********************************************************
C**
C**   ROUTINE: yfilet
C** 
C**   PURPOSE: Sets the variables for fillet
C**            Added 4/14/99 JLS
C**
C*********************************************************

      subroutine yfilet(ival,rval,iway)

      include 'com8a.com'
      include 'const.com'
      include 'fillet.com'

      real*8 rval(10)
      real*8 cutrad
      integer*2 ival(10)
C
C...Get Fillet parameters
C
      if (iway .eq. 0) then
          rval(1) = filrad
          rval(2) = filtol
          if (rval(2) .eq. 0.) rval(2) = sc(27)
c
c...Feed rate control
c
          ival(1) = IFEDCO
C
C...Tool axis same flag
C
          ival(2) = 0
          if (lflsam) ival(2) = 1
          rval(6) = acos(filang) * RADIAN
C
C...Combine motion flag
C
          ival(3) = 0
          if (lflcom) ival(3) = 1
C
C...Direction
C
          ival(4) = 1
          if (ifedco .eq. 1) ival(4) = -1
C
C...Warning
C
          ival(5) = 0
          if (lflwrn) ival(5) = 1
C
C...Set up low and high feed rates.
C...fedlow and fedhig are global variables
C
          rval(3) = SFLFED
          if (SFLFED .eq. 0.) rval(3) = sc(123)
          rval(4) = SFLFMX
          if (SFLFMX .eq. 0.) rval(4) = 999.
          rval(5) = SFLDIA
          if (SFLDIA .eq. 0.) rval(5) = sc(28)
C
C...Set the radius and the tolerance
C
      else
          filrad = rval(1)
          if (rval(2).gt.0) then
             filtol = rval(2)
          else
             filtol = sc(27)
          endif
          lflwrn = .true.
          ifl(347) = 0
          ifl(348) = 0
C
C...Tool axis same flag
C
          if (ival(2) .eq. 0) then
            lflsam = .false.
          else
            lflsam = .true.
          endif
c         lflsam = ival(2)
          filang = rval(6)
C
C...Combine motion flag
C
          if (ival(3) .eq. 0) then
            lflcom = .false.
          else
            lflcom = .true.
          endif
c         lflcom = ival(3)
          FILFLG(1) = 0
C
C...Warning
C
          lflwrn = ival(5)
C
C...If fnum is zero then we are done here and can exit.
C
          ifedco = 0
          if (ival(1) .eq. 0 .or. rval(3) .le. 0.) go to 99999

          if (ival(4) .eq. 8) then
             ifedco = 1
          else
             ifedco = 2
          endif
C
C...Divide the cutter diameter by two to get the cutter radius
C
          SFLFED = rval(3)
          SFLFMX = rval(4)
          SFLDIA = rval(5)
          cutrad = rval(5)/2.
C
C...Set up low and high feed rates.
C...fedlow and fedhig are global variables
C
          fedlow = rval(3) * filrad/(filrad+cutrad)
          tl = filrad - cutrad
          if (tl. lt. .001) then
             fedhig = rval(4)
             goto 99999
          endif
          fedhig = rval(3) * filrad/tl
          if (fedhig.gt.rval(4)) fedhig = rval(4)
      endif
c
c...End of routine
c
99999 return
      end

C*********************************************************
C**
C**   ROUTINE:yfnint(ival,rval,iway,ierr)
C** 
C**   PURPOSE: Sets the variables for fillet
C**            Added 4/14/99 JLS
C**
C*********************************************************

      subroutine yfnint(ival,rval,iway,ierr)

      include 'com8a.com'

      real*4 rval
      integer*4 iway, ierr

      real*4 asc(400)
      equivalence (sc,asc)

      ierr = 0
      if (iway .eq. 0) then
        ival = ifl(295)
        rval = asc(280)
c
c...this rate of fan value is only allow from 0 to 1, so to me, it is not make sense to
c...set to 0 when less then 1, then it only have 0 and 1 two value
c...temp remove Yurong
c
        if (ival .le. 1) rval = 0
      else
        ifl(295) = ival
        asc(280) = rval
      endif

      return
      end
