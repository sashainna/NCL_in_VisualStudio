C*********************************************************************
C*    NAME         :  gocntl.f
C*
C*       CONTAINS:
C*         gocntl  goposn  gt3swp
C*
C*    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
C*
C*     MODULE NAME AND RELEASE LEVEL
C*       gocntl.f , 25.2
C*     DATE AND TIME OF LAST MODIFICATION
C*       07/05/16 , 12:42:17
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gocntl
c*
c*     Controlling routine for the GO statement.
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
      subroutine gocntl

      include 'com8a.com'
      include 'comgt.com'
      include 'mocom.com'
      include 'suvcom.com'
c
      real*8 FEEDR
      equivalence (FEEDR ,MOTMAP(24))
c
      integer*2 isc19(4)
      integer*4 svrec(2)
c
      logical g3more,lrap(4),lrev,rapsav
c
      real*8 scsav(13),tvec(3),sc23,sc24,sc25,tend(3),sc35,tpos(6,4)
c
      equivalence (sc(19),isc19)
c
c.....Save GO parameters
c
      g3more = .false.
      lrev   = .false.
      inc = 0
      if (sc(16) .ne. 0. .or. sc(17) .ne. 0. .or. sc(18) .ne. 0. .or.
     1    isc19(2) .ne. 0 .or. isc19(3) .ne. 0) then
        g3more = .true.
        rapsav = rpfron
        do i=1,13,1
          scsav(i) = sc(i+9)
        enddo
        if (isc19(1) .eq. 0) then
          ifl42 = ifl(42)
          ifl(42) = 2
          lrev = .true.
        endif
      endif
c
c.....Position to surfaces
c
      call ncl_setptr (imotp,svrec)
      call goposn
      if (ifl(2) .ne. 0) go to 8000
      call vctovc (sc(1),tend)
      call vctovc (sc(4),tvec)
      sc35 = sc(35)
      if (lrev) then
        inc = inc + 1
        call vctovc (sc(1),tpos(1,inc))
        call vctovc (sc(4),tpos(4,inc))
        lrap(inc) = .false.
      endif
c
c.....Position off of surfaces
c
      if (g3more) then
        do i=1,13,1
          sc(i+9) = scsav(i)
        enddo
        if (sc(16) .ne. 0. .or. sc(17) .ne. 0. .or. sc(18) .ne. 0) then
          sc23 = sc(23)
          sc24 = sc(24)
          sc25 = sc(25)
          sc(23) = sc(23) + sc(17)
          sc(24) = sc(24) + sc(16)
          sc(25) = sc(25) + sc(18)
          if (sc(11) .ne. 71) sc(11) = 714
          if (sc(14) .ne. 71) sc(14) = 714
          call goposn
          call vctovc (sc(1),tend)
          call vctovc (sc(4),tvec)
          sc(23) = sc23
          sc(24) = sc24
          sc(25) = sc25
          if (lrev) then
            inc = inc + 1
            call vctovc (sc(1),tpos(1,inc))
            call vctovc (sc(4),tpos(4,inc))
            lrap(inc) = .false.
          endif
        endif
c
c......Position to RAPTO plane
c
        if (isc19(3) .ne. 0) then
c
c.........RAPTO distance
c
          isc10(2) = 0
          if (isc19(3) .eq. 1) then
            isc10(1) = 703
            sc(11) = sc(1) + sc(4)*sc(202)
            sc(12) = sc(2) + sc(5)*sc(202)
            sc(13) = sc(3) + sc(6)*sc(202)
            call vctovc (sc(4),sc(14))
c
c.........RAPTO vector
c
          else if (isc19(3) .eq. 2) then
            isc10(1) = 703
            sc(11) = sc(1) + sc(202)
            sc(12) = sc(2) + sc(203)
            sc(13) = sc(3) + sc(204)
            call vctovc (tvec,sc(14))
c
c.........RAPTO geometry
c
          else
            isc10(1) = 710
            isc10(2) = 3
            sc(11) = sc(202)
          endif
          call motimm
          if (lrev) then
            inc = inc + 1
            call vctovc (sc(1),tpos(1,inc))
            call vctovc (sc(4),tpos(4,inc))
            lrap(inc) = .true.
          endif
        endif
c
c......Position to RETRCT plane
c
        if (isc19(2) .ne. 0) then
c
c.........RETRCT distance
c
          isc10(2) = 0
          if (isc19(2) .eq. 1) then
            isc10(1) = 703
            sc(11) = tend(1) + sc(4)*sc(20)
            sc(12) = tend(2) + sc(5)*sc(20)
            sc(13) = tend(3) + sc(6)*sc(20)
            call vctovc (tvec,sc(14))
c
c.........RETRCT vector
c
          else if (isc19(2) .eq. 2) then
            isc10(1) = 703
            sc(11) = tend(1) + sc(20)
            sc(12) = tend(2) + sc(21)
            sc(13) = tend(3) + sc(22)
            call vctovc (tvec,sc(14))
c
c.........RETRCT geometry
c
          else
            isc10(1) = 710
            isc10(2) = 3
            sc(11) = sc(20)
            call vctovc (tvec,sc(4))
          endif
          if (.not. lrev) then
            rpfron = .true.
            call putcl (2000,5,1,0.d0)
          endif
          call motimm
          if (lrev) then
            inc = inc + 1
            call vctovc (sc(1),tpos(1,inc))
            call vctovc (sc(4),tpos(4,inc))
            lrap(inc) = .true.
          endif
        endif
c
c......Reverse positioning moves if
c......Entering part
c
        iclf   = 0
        if (lrev) then
          ifl(42) = ifl42
          do i=inc,2,-1
            rpfron = lrap(inc)
            if (rpfron) call putcl (2000,5,1,0.d0)
            isc10(1) = 703
            call vctovc (tpos(1,i),sc(11))
            call vctovc (tpos(4,i),sc(14))
            call motimm
          enddo
c
c......Position to part
c
          do i=1,13,1
            sc(i+9) = scsav(i)
          enddo
          if (sc(11) .ne. 71) sc(11) = 714
          if (sc(14) .ne. 71) sc(14) = 714
          rpfron = rapsav
          call goposn
        endif
      endif
      sc(35) = sc35
c      rpfron = .false.
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine goposn
c*
c*     Processes the GO statement.
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
      subroutine goposn

      include 'com8a.com'
      include 'comgt.com'
      include 'mocom.com'
      include 'suvcom.com'

      real*8 tmpsfvc(3)

      integer*2 iflg,ietype,nwds,lvgt3
      real*8 eps,temp,fw(3),pnt(3)

      logical lv92,lv93,lv97,g3more
c
c...Initialize routine
c
      lvgt3  = 0
      eps = sc(27)
      lv92 = sc(169) .lt. 9.249d0
      lv93 = sc(169) .lt. 9.349d0
      lv97 = sc(169) .lt. 9.749d0
c
c...Save CS Thick
c...It may change in GO/sf or GO/sf1,sf2
c
      HCSTHK=SC(25)
c
c...Process GO statement
c......lvgt3 is flag to show that 3-surface GO is effect
c
    5 if (isc10(3) .eq. 3 .and. lvgt3.eq.0) lvgt3 = 1
      call gtdesc (sc(12),ifl4(15),nwds,ietype)
      if (lvgt3 .gt. 0) call gtdesc (sc(15),ifl4(16),nwds,ietype)
      psmult = .false.
      IFL(280)=1
      iflg = 1
      call strtup(iflg)
      if (ifl(2).gt.0) goto 99
c
c...If command has changed to GODLTA
c...Call MOTIMM routine
c
      if (ISC10(1).EQ.710) then
        call motimm
        iflg = 3
c
c...Otherwise call MOVER to process GO statement
c
      else
        call mover
        iflg = 2
      endif
      call strtup(iflg)
C
C...Keep track of ARCSLP/FILLET
C
      if (ifl(347).ge.1 .and. ifl(2).lt.1 .and. (ifl(42).eq.0 .or.
     1    ifl(372) .eq. 1)) then
         ifl(347) = ifl(347) +1
      endif
c
c...Restore parameters after second attempt at 3-surface GO
c
      g3more = (lvgt3.gt.0 .and. (ifl(2).eq.125 .or. ifl(2).eq.256))
c
c......Restore thick if we switched DS and CS on 3sf GO
c......Bobby  -  3/24/99
c
      if (lvgt3 .ge .2 .and. lvgt3 .le. 3) then
        if (lv93) then
          temp = sc(24)
          sc(24) = sc(25)
          sc(25) = temp
c
c......Swap ending u-v for CS and DS
c
          if (sc(169) .ge. 9.149) then
            temp = hu(2)
            hu(2) = hu(3)
            hu(3) = temp
            temp = hv(2)
            hv(2) = hv(3)
            hv(3) = temp
          endif
        else if (lvgt3.eq.2 .and. .not.g3more) then
          call gt3swp
        endif
        if (lvgt3 .eq. 3) then
          sc(7) = fw(1)
          sc(8) = fw(2)
          sc(9) = fw(3)
        endif
c
c......Restore starting position as it was shifted.
c......Eduard 8/30/99
c
      else if (lvgt3.eq.4 .and. ifl(2).gt.0) then
        sc(1) = pnt(1)
        sc(2) = pnt(2)
        sc(3) = pnt(3)
      endif
c
c......Restore NUMPTS, MAXDP and MAXANG ONCE
c
      if (ifl(2).le.0) then
        ifl(91) = ifl(368)
        sc(54)  = sc(105)
        if (.not. lv97) then
            sc(162) = sc(214)
            lstep = lstepv
        endif
        sc(80)  = sc(201)
      endif
c
c...End of GO motion
c
   99 continue
      if (ifl(2).ne.0) err=.true.
c
C...Turn off GO Flag
c
      IFL(280)=0
c
C...Restore Check Surface thick
c
      if (lv93) SC(25)=HCSTHK
c
c...If Forward Sense has reversed or
c...Tool to CS apply failure
c...Try again with reversed DS and CS
c
      if ((lvgt3.eq.1 .or. lvgt3.eq.2) .and. g3more) then
        lvgt3  = lvgt3 + 1
c
c......NCL V9.3 and previous
c
        if (lv93) then
          sc(11) = hgo(14)
          sc(12) = hgo(15)
          sc(13) = hgo(13)
          sc(14) = hgo(11)
          sc(15) = hgo(12)
          isc10(1) = 702
          isc10(2) = 1
          isc10(3) = 3
c
c.........Swap UY_ds & UY_cs[0] for NCLX geometries.
c
          call ncl_dscs_swap
c
c.........Swap thicks
c
          temp = sc(24)
          sc(24) = sc(25)
          sc(25) = temp
c
c.........Swap SRFVCT
c
          if (ldssvc .or. lcssvc) then
            call vctovc (dssfvc, tmpsfvc)
            call vctovc (cssfvc, dssfvc)
            call vctovc (tmpsfvc, cssfvc)
          endif
c
c......Swap DS & CS Settings
c......Modern version
c
        else
          call gt3swp
        endif
c
c......Try inverting forward vector
c......just in case tool is already at location
c......Bobby  -  6/30/99
c
        if (lvgt3.eq.3) then
          fw(1) = sc(7)
          fw(2) = sc(8)
          fw(3) = sc(9)
          call vctmsc (sc(7),sc(7),-1.0d0)
          ifl(22) = 1
        endif
c
c...Go try GO again
c
        ifl(2) = 0
        err = .false.
        goto 5
      endif
c
c...Try moving the tool a little along the 
c...current forward vector.
c...Eduard 8/27/99
c
      if (lvgt3.eq.3 .and. g3more) then
        lvgt3  = lvgt3 + 1
        sc(11) = hgo(11)
        sc(12) = hgo(12)
        sc(13) = hgo(13)
        sc(14) = hgo(14)
        sc(15) = hgo(15)
c
c......Restore UY_ds & UY_cs[0] for NCLX geometries.
c 
        call ncl_dscs_restore
        isc10(1) = 702
        isc10(2) = 1
        isc10(3) = 3
        call vctovc (sc(1),pnt)
        call uvcplvc (sc(1),sc(7),fw,eps)
        call vctovc (fw,sc(1))
        ifl(22) = 0
        ifl(2) = 0
        err = .false.
        goto 5
      endif
99999 continue
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine gt3swp
C*       Swap DS-CS data. Currently we swap for the second attempt
C*       at GO3, and swap back after it.
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
      subroutine gt3swp

      include 'com.com'
      include 'mocom.com'
      include 'suvcom.com'

      real*8 asn1,asn2,tmp
      integer*2 ksn1(4),ksn2(4)
      equivalence (ksn1,asn1), (ksn2,asn2)

      logical ltmp
      real*4 temp4
      real*8 temp8,tmpsfvc(3)

      sc(11) = hgo(14)
      sc(12) = hgo(15)
      sc(13) = hgo(13)
      sc(14) = hgo(11)
      sc(15) = hgo(12)
      isc10(1) = 702
      isc10(2) = 1
      isc10(3) = 3
c
c... Swap UY_ds & UY_cs[0] for NCLX geometries
c
      call ncl_dscs_swap
c
c......Swap thicks
c
      temp8 = sc(24)
      sc(24) = sc(25)
      sc(25) = temp8
c
c... Swap surface vectors
c
      if (ldssvc .or. lcssvc) then
        call vctovc (dssfvc, tmpsfvc)
        call vctovc (cssfvc, dssfvc)
        call vctovc (tmpsfvc, cssfvc)
      endif
c
c... Swap initial surface parameters
c
      ltmp = dsuv
      dsuv = csuv
      csuv = ltmp
      temp4 = dsu
      dsu = csu
      csu = temp4
      temp4 = dsv
      dsv = csv
      csv = temp4
c
c..... swap ending u-v for CS and DS
c
      if (ifl(2) .eq. 0) then
        temp4 = hu(2)
        hu(2) = hu(3)
        hu(3) = temp4
        temp4 = hv(2)
        hv(2) = hv(3)
        hv(3) = temp4
      endif
c
c...In case DS or CS is a line
c...that has been converted to a plane
c
      asn1 = hgo(12)
      asn2 = hgo(15)
      if ((ksn1(4) .eq. 6 .and. ksn1(1) .eq. 0) .or.
     1    (ksn2(4) .eq. 6 .and. ksn2(1) .eq. 0)) then
        do 100 i=1,4,1
            tmp = d(50+i+2)
            d(50+i+2) = d(100+i+2)
            d(100+i+2) = tmp
  100   continue
      endif
c
c...End of routine
c
      return
      end
