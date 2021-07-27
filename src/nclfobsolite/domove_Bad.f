C***********************************************************************
C*    NAME         :  domove.f
C*       CONTAINS:
C*    COPYRIGHT 1989 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       domove.f , 26.8
C*    DATE AND TIME OF LAST  MODIFICATION
C*       05/28/19 , 11:56:45
C***********************************************************************
C **********************************************************************
C **  PROGRAM NAME: DOMOVE
C **
C **  PURPOSE OF PROGRAM: HANDLE ADJUSTMENT OF MAXDP ON ERROR IN MOVER.
C **
C **********************************************************************
C **********************************************************************
 

      SUBROUTINE DOMOVE
 
      include 'com8a.com'
      include 'mocom.com'
      include 'suvcom.com'

      common/fancmbcom/fan160,lfncmb
      real*8 fan160
      integer*2 lfncmb

      common/stdist/csdis0,itry,itfl0
      real*4 csdis0
      integer*2 itry,ifl6,itfl0

      integer*2 ic
      equivalence (ifl(53),ic)
      REAL*8 sc54, sc160, sc176, DPMIN, FCTR
      REAL*4 dpi
      INTEGER*2 LCNT, NPTSV, MXLOOP, isc162(4)
      REAL*8 ASN
      REAL*4 BSN(2)
      INTEGER*2 KSN(4)
      EQUIVALENCE (ASN,BSN,KSN), (isc162, sc(162))
      integer*4 kcps
      equivalence (kcps,ifl(337))
      logical lv91,lv93,lv98

      domov = .true.
      itry = 0
      itfl0 = 0
      lv91 = sc(169).lt.9.149d0
      lv93 = sc(169).lt.9.349d0
      lv98 = sc(169).lt.9.849d0
      lfncmb = 0

C           SET MAX TRIES, MIN STEP, REDUC FACTOR
      ASN=SC(162)
      MXLOOP = KSN(1)
      DPMIN = BSN(2)
      FCTR = 4.D0
C                  INIT LOOP COUNTER
      LCNT = 0
C                  SAVE MAXDP, DISTANCE MOVED, NUMPTS
      sc54 = SC(54)
      sc160 = SC(160)
      sc176 = SC(176)
      NPTSV = IFL(91)
c
c..... save circular interpolation flag
c
      ifl94 = ifl(94)
      ifl6 = ifl(6)
      
c     init pars for flip check Sasha Aug. 2018     
      !sc(217) = 0.0
      !sc(218) = 0.0
      !sc(219) = 0.0
      !sc(220) = 0.0
      !sc(221) = 0.0
      !sc(222) = 0.0
      !sc(223) = 0.0
      !
      if (.not.csuv .and. .not.csmult) call getsuv(sc(13),3,csu,csv)

10    CONTINUE
      CALL PREMOV
      IF (IFL(2).GT.0) then
        if (lttcsf .and. .not.mocom) then
          call ncl_2sfbnry_free
          lttcsf = .false.
        endif
        GOTO 999
      endif
      !psmult = 0
      if (.not.lv93 .and. .not. psmult .and. .not.mocom .and.
     x    ifl(42).eq.0 .and. ifl(23).eq.4 .and. kcps.eq.0) lfncmb = -1
      
c     save to check flip later in psrel, Sasha Aug. 2018           
      !call vctovc(sc(1),sc(217))
      !call vctovc(sc(4),sc(220))
      CALL MOVER
      
      !IF (IFL(2).EQ.-255) IFL(2) = 255
      
      if(f_dot(sc(4),sc(220)) .lt. 0.0) then 
          call vctovc(sc(217), sc(1))
          call vctovc(sc(220), sc(4))
      endif

      IF (IFL(2).EQ.9596) then
        ifl(2) = 0
        call fancmb
        if (ifl(2) .gt. 0) ifl(2) = 255
      endif

      IF (IFL(2).EQ.0) GOTO 100
      IF (IFL(2).EQ.124) GOTO 20
      IF (IFL(2).EQ.125) GOTO 20
      IF (IFL(2).EQ.128) GOTO 20
      IF (IFL(2).EQ.129) GOTO 20
      IF (IFL(2).EQ.134) GOTO 20
      IF (IFL(2).EQ.138) GOTO 20
      IF (IFL(2).EQ.141 .and. .not.lv91) GOTO 20
      IF (IFL(2).EQ.163) GOTO 20
      IF (IFL(2).EQ.255) GOTO 20
      !IF (IFL(2).EQ.-255) GOTO 20
      IF (IFL(2).EQ.256) GOTO 20
      IF (IFL(2).EQ.9595) GOTO 20
C...                             ADD MORE ERRORS HERE IF NEEDED
      GOTO 999
20    CONTINUE
C                     FOR SELECTED ERRORS, TRY MXLOOP TIMES WITH REDUCED MAXDP
c
c...Erase motion from last attempt  &
c...start new motion segment
c...Bobby  -  11/18/93
c
c      if (ifl(42).eq.0) then
c        call moters
c        call motbgn
c      endif

      if (ifl(2) .eq. 9595) then
c
c..... don't do circular interpolation for this motion
c
        ifl(94) = 0
        ifl(2) = 0
        goto 10
      endif

      if (ifl(2).eq.141 .and. .not.lv91 .and. (.not. lcspt .or. lv98))
     1        then
        if (itry.gt.1 .or. (itry.gt.0 .and. itfl0.ne.1)) goto 999
        itry = itry + 1
        dpi = csdis0/2.5
        if (dpi .gt. sc(54)) goto 999
        if (itry .eq. 1) SC(54) = dpi
      else
        if (ifl(2).eq.255 .and. ifl(6).lt.3) ifl(6) = 3
        LCNT = LCNT + 1
        IF (LCNT .GT. MXLOOP) then
          if (lv93 .or. itry.gt.0 .or. itfl0.ne.1) goto 999
          itry = 2
        else
          SC(54) = SC(54)/FCTR
          IF (IFL(91)*FCTR.LT.32000) IFL(91) = IFL(91)*FCTR
        endif
      endif
      IF (SC(54) .LT. DPMIN) then
        if (itfl0 .ne. 1) GOTO 999
        sc(54) = dpmin
        itry = 2
      endif
      SC(160)=sc160
      SC(176)=sc176
      IFL(2)=0
      GOTO 10

100   CONTINUE
C       MADE MOVE WITHOUT ERROR. ISSUE WARNING IF MAXDP WAS REDUCED
      IF (LCNT.EQ.0 .or. isc162(2) .eq. 0) GOTO 999
      IFL(2)=-375
      ERR = .TRUE.
      WRITE(ERRCOM,1000) SC(54)
1000  FORMAT(F10.4)

999   CONTINUE
      SC(54)=sc54
      IFL(91)=NPTSV
      ifl(94)=ifl94
      ifl(6) = ifl6
c
c...save uv parameters of PS, DS and CS
c
      call uv_set
      itry = 0
      domov = .false.
      lfncmb = 0

99999 RETURN
      END
c
c**********************************************************************
c   SUBROUTINE: uv_set 
c
c   FUNCTION:  Store the last motion SFs parameters in the common
c              block to use lately (in following motion).   
c
c     INPUT:  none
c
c     OUTPUT: none
c
c**********************************************************************
      subroutine uv_set 
c
      include 'com8a.com'
      include 'mocom.com'
      include 'suvcom.com'
c
      integer*2 ic
      equivalence (ifl(53),ic)
c
      psuv = .false.
      dsuv = .false.
      csuv = .false.
      if (autouv) then
         psu = t(13,ic)
         psv = t(14,ic)
         dsu = t(19,ic)
         dsv = t(20,ic)
         csu = t(25,ic)
         csv = t(26,ic)
      end if
      if (csmult) then
         csu = t(25,ic)
         csv = t(26,ic)
         hu(1) = t(27,ic)
      end if
c
      return
      end
c
