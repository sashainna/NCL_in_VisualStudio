C***********************************************************************
c**    NAME           : off11.f
c**
c**    CONTAINS:
c**
C**   SUBROUTINE RRPOCK (FSTOFF,STEP,LOOPS,KLIMB,IERR)
C**   SUBROUTINE CONTOUR (FSTOFF, STEP,LOOPS,IERR)
C**   SUBROUTINE STOLAP (LUP,LANET,MOM,XC,YC,NPPC,IERR)
C**   SUBROUTINE wrilap
C**   SUBROUTINE GETLUP (LAN,LUP,INDEX,XA,YA,NPPA,ierr)
C**   SUBROUTINE GETLAN (INDEX,XA,YA,NPPA)
C**   SUBROUTINE COMLUP (XA,YA,NPPA,XB,YB,NPPB,
C**                        MOM,LUPOK,LANEP,INTILS,LANEX,NEWLUP)
C**   SUBROUTINE LUPLUP (XA,YA,NPPA,XB,YB,NPPB,XL1,YL1,XL2,YL2,LLT,
C**                        NBOX,INGO,INTO,XING,YING,INGOS)
C**   SUBROUTINE ARRINP (IERR)
C**   SUBROUTINE LANCOM (LUPOK,LUPIL,LANEX,ISON,NEWLUP)
C**   SUBROUTINE ILSCOM (LUPIL,ISON,NEWLUP)
C**   SUBROUTINE LANPOK (RADIUS,LUPOK,LANEX,NOLUP,IERR)
C**   SUBROUTINE LANILE (FSTOFF,STEP,LUPIL,LANEX,IERR)
C**   SUBROUTINE LOOKPT (XA,YA,NPPA,LSEG,UEG,XN,YN,IERR)
C**   SUBROUTINE CLOSEST (XA,YA,NPPA,LSEG,UEG,XN,YN,DIST)
C**   SUBROUTINE ISLOOK (INDEX1,INDEX2)
C**   SUBROUTINE XSHARP (xa, ya, nppa, step, stepit)
C**
C**    MODULE NAME AND RELEASE LEVEL
C**       off11.f , 25.1
C**    DATE AND TIME OF LAST  MODIFICATION
C**       04/29/15 , 15:10:22
C***********************************************************************
c**
c** copyright (c) 1990 MILLS DATA SYSTEMS CORPORATION
c**
c **********************************************************************
c **********************************************************************
c **  1. SUBROUTINE RRPOCK
C **  purpose of subroutine:  EXECUTE POCKET WITH ISLANDS
c **    input:
C **      FSTOFF...FIRST POCKET & ISLAND OFFSETS  -------- INPUT
C **      STEP.....MAX DISTANCE BETWEEN LOOPS
C **      LOOPS....MAX NUMBER OF LOOPS
C **      KLIMB....MACHINING DIRECTION 1- CCW, -1- CW
c **    output:
C **      IERR.....0-OK, 2 - TOO MANY POINTS IN POCKET
C **                     3 - TOO MANY POINTS IN ISLAND - INPUT GEOMETRY
C **                     8 - TOO MANY INPUT POINTS - INPUT GEOMETRY
C **                     9 - ILLEGAL START POINT FOR POCKET
c **                    10 - TOO MANY LOOPS
C **                    11 - TOO MANY POINTS IN LOOP COMPUTATION
C **                    12 - CANNOT HANDLE GIVEN END POINT
C **                    13 - END LOOP TOO SMALL
C **                    14 - Pocket loop contained within island loop
C **                    51 - CANNOT MOVE BETWEEN LOOPS
c **********************************************************************
c **********************************************************************

      SUBROUTINE RRPOCK (FSTOFF,STEP,LOOPS,KLIMB,IERR)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 fstoff(120), step
      integer*4 loops, klimb, ierr
      logical llook0
      integer*2 lanes0

      STEPL = STEP
      LKLIMB = KLIMB
      LLOOPS = LOOPS
      LERR = 0
c
c..... look = 2 is set in pokntr as a part of AVOID logic; here it is treated
c..... as a user-set end point
c
      llook0 = .false.
      if (look .eq. 2) then
        llook0 = .true.
        lanes0 = lanes
        look = 1
      else
        CALL ARRINP (LERR)
        IF (LERR.NE.0) GOTO 99
        islnds = LANES-1
c
c..... Check that the number of islands does not exceed maximum
c
        if (islnds .gt. 100) then
          lerr = 11
          goto 99
        endif
      endif
C --- CALCULATE POCKET LOOPS
10    CALL CONTOUR (FSTOFF,STEPL,LLOOPS,LERR)
      IF (LERR.NE.0 .or. lacefl.gt.0) GOTO 99

C --- CALCULATE ORDER OF PATH
      CALL CUTPOCK (STEPL,LKLIMB,LERR)

   99 CONTINUE
c
c..... if look = 2 set in pokntr as a part of AVOID logic results in a
c..... new error the pocket is again recalculated with look = 1
c
      if (llook0) then
        if (look .eq. 0) then
          look = 1
        else if (look.eq.1 .and. lerr.gt.1) then
          look = 0
          lanes = lanes0
          lerr = 0
          goto 10
        endif
      endif

      IERR = LERR
      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  2. SUBROUTINE CONTOUR
C **  purpose of subroutine: CALCULATE POCKET CONTOUR
c **    input:
C **      FSTOFF.........FIRST OFFSET
C **      STEP...........DISTANCE BETWEEN LOOPS
C **      LOOPS..........NUMBER OF LOOPS
c **    output:
C **      IERR...........0 -OK, OTHER- ERROR
c **********************************************************************
c **********************************************************************

      SUBROUTINE CONTOUR (FSTOFF, STEP,LOOPS,IERR)

      include 'com.com'
      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid

      real*4 FSTOFF(120)
      integer*2 loops,ierr,lanex,nolup,lupil,ison,lanes1,lupok,itim
      integer*2 lansav,lanxsv,isonsv,k1,k2,k,i,i1,ild,indx,indma
      integer*2 lupma,lanma,loopi
      integer*2 lupsv(20),lansv(20),momsv(20)
      real*4 step,r,rx,radsv,pm

      IERR = 0
      LANEX = 1
      NOLUP = 0
      LUPIL = 0
      ISON = islnds
      itim = 0
      pm = 1.
      LANES1 = 1
      STPMAX = abs(step)
      stplst = stpmax
      cutin = 1.0
      do i = 1,100
        insid(i) = 0
      enddo

c
c... if cutting outside profile change sense of cutin side
c
      if (fstoff(1) .lt. 0.0) cutin = - cutin

C--- RUN POCKET, ISLANDS AND COMMON LOOPS
      DO 75 LUPOK = 1, LOOPS
        LANSAV = LANES
        LANXSV = LANEX
        ISONSV = ISON
        R = stplst * cutin
        IF (LUPOK.EQ.1) R = FSTOFF(1)
c
c..... changed 10 to 20 to resolve fsr 60982 (letter H)
c
        if (lupok.ge.2 .and. lansav.le.20) then
          radsv = r
          do i = 1,lansav
            lupsv(i) = loop(i)
            lansv(i) = lane(i)
            momsv(i) = mama(i)
          enddo
        endif

C--- RUN POCKET LOOPS
 10     CALL LANPOK (R, LUPOK, LANEX, NOLUP, IERR)
        IF (IERR.GT.1) GOTO 99
        IF (NOLUP.EQ.1) then
          if (lupok.eq.1) then
            ierr = 13
            goto 99
          endif
          GOTO 90
        endif
        IF (ISON.EQ.0) then
          if (lacefl .gt. 0) then
            call lacer(step,lanex,ierr)
            return
          endif
          GOTO 75
        endif
        IF (LUPOK.GT.1) GOTO 27

C--- RUN ISLANDS LOOPS
        LUPIL = 1
        rx = stpmax
        CALL LANILE (FSTOFF, rx, LUPIL, LANEX, IERR)
        IF (IERR.GT.1) GOTO 99

C--- RUN COMMON LOOPS FOR ISLAND LOOPS
        call ilscom (lupil, ison, lanex, ierr)
        if (ierr.gt.1) goto 99
        CALL LANCOM (LUPOK, LUPIL, LANEX, ISON, IERR)
        if (ierr.eq.13) GOTO 99

        if (lacefl .gt. 0) then
          call lacer(step,lanex,ierr)
          return
        endif

C--- SECOND LOOP AROUND ISLANDS
        lupil = 2
        rx = rx*.2
        CALL LANILE (FSTOFF, RX, LUPIL, LANEX, IERR)
        IF (IERR.GT.1) GOTO 99

C--- RUN COMMON LOOPS FOR ISLAND LOOPS
        call ilscom (lupil, ison, lanex, ierr)
        if (ierr.gt.1) goto 99

C--- RUN COMMON LOOPS
   27   CALL LANCOM (LUPOK, LUPIL, LANEX, ISON, IERR)

        if (ierr.eq.14 .and. lupok.ge.2 .and. lupil.eq.2 .and.
     x                           lansav.le.20 .and. itim.lt.4) then
          ison = isonsv
          LANEX = LANXSV
          itim = itim+1
          pm = -pm
          r = radsv + itim*pm*tol
          ierr = 0
          do i = 1,lansav
            loop(i) = lupsv(i)
            lane(i) = lansv(i)
            mama(i) = momsv(i)
          enddo
          do i = lansav+1,lanes
            loop(i) = 0
            lane(i) = 0
          enddo
          goto 10
        endif

        IF (IERR.GT.1) GOTO 99
c
c..... the fix below is for QAR 92323 - ierr = 1 from lancom is not an error,
c..... so if the loop ends here it should have ierr = 0
c
        IF (IERR.eq.1 .and. lupok.ge.loops .and.
     +      sc(169).gt.9.2) ierr = 0
        if (sc(169).gt.8.229999) stplst = stpcur
   75 CONTINUE

C... For limited number of lanes set last lane with negative loop.
      k1 = 1
      k2 = lanex
81    do 82 k=k1,k2
        do 78 i=lanes,1,-1
          if (lane(i).eq.k) then
            i1 = loop(i)
            if (i1.gt.0) then
              lanes = lanes+1
              lane(lanes) = k
              loop(lanes) = -i1-1
              loca(lanes+1) = loca(lanes)
              mama(lanes) = i
            endif
            goto 82
          endif
   78   continue
   82 continue

C... CORRECT MAMA FOR ISLANDS THAT HAD THEIR MAMA LOOP KILLED
   90 DO 85 ILD = 101, islnds+100
        INDX = 0
        DO 84 K = 1, LANES
          IF (LANE(K).EQ.ILD.AND.LOOP(K).EQ.1) THEN
            INDX = K
            INDMA = MAMA(INDX)
            LUPMA = LOOP(INDMA)
            IF (LUPMA.NE.0) GOTO 85
            LANMA = LANE(INDMA)
            DO 83 I = INDMA+1,LANES
              IF (LANE(I).EQ.LANMA.AND.LOOP(I).GT.0) THEN
                loopi = loop(i)
                if (sc(169) .ge. 9.549d0)
     x          call fostermom (indx,i,loopi)
                MAMA(INDX) = I
                GOTO 85
              ENDIF
   83       CONTINUE
          ENDIF
   84   CONTINUE
   85 CONTINUE

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  4.SUBROUTINE STOLAP
C **  purpose of subroutine: STORE ONE LANE/LOOP IN COMMONS RRXY and RRLN
c **    input:
C **      LUP..........CURRENT LOOP
C **      LANET........LANE NUMBER
C **      MOM..........INDEX IN RRLN OF MOTHER LOOP
C **      XC,YC,NPPC...ARRAYS OF PATH AND NO OF POINTS
c **    output:
C **      XRR,YRR......ARRAYS OF PATH
C **      LANE.........NUMBER OF LANE IN PATH
C **      LOOP.........LOOP IN LANE
C **      LOCA.........LOCATION OF LANE/LOOP IN XRR,YRR
C **      IERR.........0 - OK, 10 - TOO MANY LOOPS
c **********************************************************************
c **********************************************************************

      SUBROUTINE STOLAP (LUP,LANET,MOM,XC,YC,NPPC,IERR)

      include 'com.com'
      include 'pokcom.com'

      real*4 XC(maxpts),YC(maxpts)
      integer*2 lup,lanet,mom,nppc,ierr

      integer*2 i
      integer*4 kk
      real*8 xx,yy
      real*8 xx1,yy1,xx2,yy2,xxi,yyi,fmm
      real*4 xi,yi

      LANES = LANES+1
      IF (LANES+1.GE.mxln) THEN
        IERR = 10
        GOTO 99
      ENDIF

      if (lwr) then
         write(19,*)'$$'
         write(19,*)'%lanes = ',lanes
         write(19,*)'$$'
      endif

100   format('ln/',f11.4,',',f11.4,',',f11.4,',',f11.4)
101   format('ci/(pt/',f11.4,',',f11.4,'),(pt/',f11.4,',',f11.4,'),$')
102   format('(pt/',f11.4,',',f11.4,')')
103   format('pt/',f11.4,',',f11.4)

      LANE(LANES) = LANET
      LOOP(LANES) = LUP
      MAMA(LANES) = MOM
      LOCA(LANES+1) = LOCA(LANES) + NPPC
      kk = LOCA(LANES)-1

      if (lwr) then
       fmm = 1.
       if (ifl(264).eq.0) fmm = 1./25.4
       ifla = 0
       icirc = 0
      endif

      DO 15 I = 1, NPPC
        kk = kk + 1
        xx = XC(I)
        yy = YC(I)
        call rrput(kk,xx,yy,ierr)

      if (lwr) then
        if (xx .gt. 0.9*flarc) then
          icirc = 1
          ifla = 3
          call midarc (xc,yc,i-1,xi,yi)
          xxi = xi*fmm
          yyi = yi*fmm
        endif

        if (ifla .eq. 0) then
          xx1 = xx*fmm
          yy1 = yy*fmm
          if (i.gt.1) then
            if (icirc.eq.0) then
        xxi = xx2-xx1
        yyi = yy2-yy1
              if (xxi*xxi + yyi*yyi .lt. 0.00001) then
                write(19,103) xx2,yy2
        else
                write(19,100) xx2,yy2,xx1,yy1
              endif
            else
              write(19,101) xx2,yy2,xxi,yyi
              write(19,102) xx1,yy1
            endif
          endif
          xx2 = xx1
          yy2 = yy1
          icirc = 0
        else
          ifla = ifla-1
        endif
      endif

   15 CONTINUE
      if (sc(169).gt.8.229999.and.lup.gt.0 .and. lanet.lt.100 .and.
     x    lacefl.eq.0) call xsharp (xc, yc, nppc, stpmax, stpcur)

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
      SUBROUTINE wrilap (XC,YC,NPPC)

      include 'com.com'
      include 'pokcom.com'

      real*4 XC(maxpts),YC(maxpts)
      integer*2 nppc

      integer*2 i
      real*8 xx,yy
      real*8 xx1,yy1,xx2,yy2,xxi,yyi,fmm
      real*4 xi,yi

         write(19,*)'$$'
         write(19,*)'%nppa = ',nppc
         write(19,*)'$$'

100   format('ln/',f11.4,',',f11.4,',',f11.4,',',f11.4)
101   format('ci/(pt/',f11.4,',',f11.4,'),(pt/',f11.4,',',f11.4,'),$')
102   format('(pt/',f11.4,',',f11.4,')')
103   format('pt/',f11.4,',',f11.4)
104   format('$$ln/',f11.4,',',f11.4,',',f11.4,',',f11.4)

       fmm = 1.
       if (ifl(264).eq.0) fmm = 1./25.4
       ifla = 0
       icirc = 0

      DO 15 I = 1, NPPC
        xx = XC(I)
        yy = YC(I)
        if (xx .gt. 0.9*flarc) then
          icirc = 1
          ifla = 3
          call midarc (xc,yc,i-1,xi,yi)
          xxi = xi*fmm
          yyi = yi*fmm
        endif

        if (ifla .eq. 0) then
          xx1 = xx*fmm
          yy1 = yy*fmm
          if (i.gt.1) then
            if (icirc.eq.0) then
              xxi = xx2-xx1
              yyi = yy2-yy1
              if (xxi*xxi + yyi*yyi .lt. 0.00001) then
                write(19,103) xx2,yy2
                write(19,104) xx2,yy2,xx1,yy1
              else
                write(19,100) xx2,yy2,xx1,yy1
              endif
            else
              write(19,101) xx2,yy2,xxi,yyi
              write(19,102) xx1,yy1
            endif
          endif
          xx2 = xx1
          yy2 = yy1
          icirc = 0
        else
          ifla = ifla-1
        endif

   15 CONTINUE

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  5. SUBROUTINE GETLUP
C **  purpose of subroutine: GET VALUES OF A LANE/LOOP INTO ARRAY
c **    input:
C **      LAN..........LANE NUMBER OF LOOP
C **      LUP..........LOOP NUMBER OF LOOP
C **    output:
c **      INDEX........INDEX OF LANE/LOOP IN RRLN
C **      XA,YA,NPPA...STRING OF LANE/LOOP
C **      IERR.....0-OK, 2 or more - error
c **********************************************************************
c **********************************************************************

      SUBROUTINE GETLUP (LAN,LUP,INDEX,XA,YA,NPPA,ierr)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      integer*2 ierr
      integer*4 k1,kk,ier
      real*8 xx,yy

      ierr = 0

C --- FIND LANE/LOOP LOCATION
      INDEX = 0
      DO 19 K = LANES, 1, -1
        IF (LANE(K).EQ.LAN) THEN
          IF (LOOP(K).EQ.LUP) GOTO 27
          GOTO 99
        ENDIF
   19 CONTINUE
      GOTO 99

C --- GET VALUES INTO XA,YA
   27 K1 = LOCA(K)
      INDEX = K
      NPPA = LOCA(K+1)-K1
      if (nppa .gt. maxpts) then
        ierr = 11
        goto 99
      endif
      DO 37 K = 1, NPPA
        kk = k1+k-1
        call rrget(kk,xx,yy,ier)
        if (ier .gt. 1) then
           ierr = ier
           return
        endif
        xa(k) = xx
        ya(k) = yy
   37 CONTINUE

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  6. SUBROUTINE GETLAN (INDEX,XA,YA,NPPA)
C **  purpose of subroutine: GET VALUES OF LANE/LOOP INTO ARRAYS
c **    input:
C **      INDEX........INDEX OF LANE IN COMMON RRLN
C **    output:
c **      XA,YA,NPPA...STRING OF LANE/LOOP
c **********************************************************************
c **********************************************************************

      SUBROUTINE GETLAN (INDEX,XA,YA,NPPA)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      integer*4 ier
      integer*4 k1, kk
      real*8 xx,yy

      ier = 0

C --- GET VALUES INTO XA,YA
      K1 = LOCA(INDEX)
      NPPA = LOCA(INDEX+1)-K1
      DO 37 K = 1, NPPA
        kk = k1+k-1
        call rrget(kk,xx,yy,ier)
        if (ier .gt. 0) return
        xa(k) = xx
        ya(k) = yy
   37 CONTINUE

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  7. SUBROUTINE COMLUP
C **  purpose of subroutine: FIND RESULTING COMMON LOOP FROM TWO LOOPS
c **    input:
C **      XA,YA,NPPA...POCKET LANE/LOOP
C **      XB,YB,NPPB...ISLAND LANE/LOOP
C **      MOM..........INDEX IN RRLN OF MOTHER LOOP
C **      LUPOK........LOOP NUMBER OF POCKET MOTHER LOOP
C **      LANEP........LANE NUMBER OF POCKET MOTHER LOOP
c **      INTILS.......Intersecting islands=1, pocket/island=0
C **      LANEX........MAX LANE NUMBER
c **    output:
C **      LANEX........MAX LANE NUMBER if intils=0 only - plus -
C **      NEWLUP....... 1- NEW LOOPS FORMED, 0- NONE FORMED --OUTPUT
C **                   11- NEW LOOP FORMED WITH MORE THAN maxpts POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE COMLUP (XA,YA,NPPA,XB,YB,NPPB,lupil,
     x                   MOM,LUPOK,LANEP,INTILS,LANEX,NEWLUP)

      include 'com4a.com'
      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)

      integer*2 nppb0,nppc
      real*4 XC(maxpts),YC(maxpts),XB0(maxpts),YB0(maxpts)

      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)

      real*4 XING(25),YING(25)
      integer*4 INGO(25),INTO(25)
      real*4 XING1(25),YING1(25)
      integer*4 INGO1(25),INTO1(25),INTEMP(25),intmp(25),intmp1(25)
      real*4 eps,eps1
      integer*2 lan1,lann1,itim

      logical lchged,lv94,lv95,lv96

      lv94  = sc(169).lt.9.449d0
      lv95  = sc(169).lt.9.549d0
      lv96  = sc(169).lt.9.649d0
      lchged = .false.

      eps = tol/25.
      eps1 = tol/2.54
      NEWLUP = 0
      lann1 = 1
      IADD = -1
      INDEX1 = LANES+1
      INDEX2 = LANES
      if (intils.eq.1) then
        CALL XCLOCK (XA,YA,NPPA,AREA1,KLOK)
        CALL XCLOCK (XB,YB,NPPB,AREA2,KLOK)
        area1 = abs(area1)
        area2 = abs(area2)
        arebig = area1
        if (area2.gt.area1) arebig = area2
      else if (.not.lv94) then
        call cpylap (xb,yb,nppb,xb0,yb0,nppb0)
c
c..... qar 95178 - if the two contours have overlapping segments
c..... try to fix it by offsetting the island
c
        call xross2 (XA,YA,NPPA,XB,YB,NPPB,lchged)
      endif

      itim = -1
      if (.not.lv95 .and. .not.lchged .and. lupil.eq.2 .and.
     x        intils.eq.0 .and. lupok.ge.2) itim = 0
      if (.not.lv96 .and. .not.lchged .and. lupil.eq.1 .and.
     x        intils.eq.1 .and. lupok.eq.1) itim = 0

10    continue

C --- FIND INTERSECTIONS BETWEEN FIRST LOOP TO SECOND LOOP
      CALL BOXES(NPPB,XB,YB,XL1,YL1,XL2,YL2,LLT,NBOX)
      CALL LUPLUP (XA,YA,NPPA,XB,YB,NPPB,XL1,YL1,XL2,YL2,LLT,NBOX,
     +             INGO ,INTO,intmp,XING,YING,INGOS)
      IF (INGOS.EQ.0) GOTO 99

C --- FIND INTERSECTIONS BETWEEN SECOND LOOP TO FIRST LOOP
      CALL BOXES(NPPA,XA,YA,XL1,YL1,XL2,YL2,LLT,NBOX)
      CALL LUPLUP (XB,YB,NPPB,XA,YA,NPPA,XL1,YL1,XL2,YL2,LLT,NBOX,
     +             INTO1 ,INGO1,intmp1,XING1,YING1,INGOS1)
      IF (INGOS.NE.INGOS1) THEN
        INGOS = 0
        GOTO 99
      ENDIF
c
c.... Eduard 4/28/1999. If the number of intersections is odd, one of
c.... them is a tangency. We are trying to eliminate it here.
c
      if (ingos .ne. (ingos/2)*2) then
        if (abs(xing(ingos)-xing1(ingos)).le.eps .and.
     +      abs(ying(ingos)-ying1(ingos)).le.eps) then
              ingos = ingos - 1
        else
          if (itim.ge.0 .and. itim.lt.4) then
            itim = itim + 1
            call smloffs (itim,XB,YB,NPPB,lchged,ierr)
            if (ierr .eq. 0) goto 10
          endif
          newlup = 14
          goto 99
        endif
      endif
      newlup = 1

C --- SET INTO1 TO DESIGNATE ORDER OF B INTERSECTIONS
      DO 22 I = 1, INGOS
      intemp(i) = 0
        DO 20 K = 1, INGOS
          IF (INGO(I).NE.INGO1(K) .and. INGO(I).NE.intmp1(K)) GOTO 20
          IF (INTO(I).NE.INTO1(K) .and. intmp(I).NE.INTO1(K)) GOTO 20
          IF (ABS(XING(I)-XING1(K)).GT.eps1) GOTO 20
          IF (ABS(YING(I)-YING1(K)).GT.eps1) GOTO 20
          if (INTO(I).NE.INTO1(K)) INTO(I) = INTO1(K)
          INTEMP(I) = K
   20   CONTINUE
   22 CONTINUE
      DO 23 i = 1, INGOS
        INGO1(i) = 0
        INTO1(i) = INTEMP(i)
   23 CONTINUE

C --- GET COMMON LOOP, A-INTERSECTIONS FIRST
      NPPC = 0
      ii = 0
   27 ii = ii+1
c... set up control indices
      i = ii
      IF (i.GT.INGOS) GOTO 90
      IF (INGO1(i).EQ.-1) GOTO 27
      if (into1(i) .eq. 0) goto 27
      ic = 1
   29 i1 = i+ic
      IF (i1.GT.INGOS) i1 = i1 - ingos
      if (into1(i1) .eq. 0) then
        ic = ic + 1
        goto 29
      endif
c... set up start, intersect, and end segments ids
      K1 = INGO(i)
      KI2 = INGO(i1)
      K2 = KI2
      IF (XA(K2+1).EQ.FLARC) K2 = K2+3
      NPPC = NPPC+1
      IF (NPPC.GT.maxpts) THEN
        NEWLUP = 11
        GOTO 99
      ENDIF
c... start intersection of A/B
      XC(NPPC) = XING(I)
      YC(NPPC) = YING(I)
      IF (KI2.GT.K1) THEN
c... segments inside of A to end intersection
        DO 41 K = K1+1, K2
          NPPC = NPPC+1
          IF (NPPC.GT.maxpts) THEN
            NEWLUP = 11
            GOTO 99
          ENDIF
          XC(NPPC) = XA(K)
          YC(NPPC) = YA(K)
   41   CONTINUE
      ENDIF
      IF (KI2.EQ.K1.AND.I1.GT.I) GOTO 47
      IF (KI2.LE.K1) THEN
        IF (K1+1.GT.NPPA-1) GOTO 44
c... segments backside of A
        DO 43 K = K1+1, NPPA-1
          NPPC = NPPC+1
          IF (NPPC.GT.maxpts) THEN
            NEWLUP = 11
            GOTO 99
          ENDIF
          XC(NPPC) = XA(K)
          YC(NPPC) = YA(K)
   43   CONTINUE
c... segments start of A to end intersection
   44   DO 45 K = 1, K2
          NPPC = NPPC+1
          IF (NPPC.GT.maxpts) THEN
            NEWLUP = 11
            GOTO 99
          ENDIF
          XC(NPPC) = XA(K)
          YC(NPPC) = YA(K)
   45   CONTINUE
      ENDIF
      GOTO 49
   47 IF (K1.GE.K2) GOTO 49
c... segments frontside of A to end intersection
      DO 48 K = K1+1, K2
        NPPC = NPPC+1
        IF (NPPC.GT.maxpts) THEN
          NEWLUP = 11
          GOTO 99
        ENDIF
        XC(NPPC) = XA(K)
        YC(NPPC) = YA(K)
   48 CONTINUE

   49 NPPC = NPPC+1
      IF (NPPC.GT.maxpts) THEN
        NEWLUP = 11
        GOTO 99
      ENDIF
c... end intersection of A/B
      XC(NPPC) = XING(I1)
      YC(NPPC) = YING(I1)

C--- GET COMMON LOOP, B INTERSECTIONS NEXT
      i2 = 0
      K1 = INTO1(I1)
      IF (K1.EQ.INGOS) THEN
        DO 52 K = 1, INGOS
          IF (INTO1(K).EQ.ic) THEN
            I2 = K
            KI2 = INTO(I2)
            GOTO 54
          ENDIF
   52   CONTINUE
      else
        DO 53 K = 1, INGOS
          IF (INTO1(K)-K1.EQ.ic) THEN
            I2 = K
            KI2 = INTO(I2)
            GOTO 54
          ENDIF
   53   CONTINUE
      ENDIF
      if (i2 .eq. 0) then
c... common intersections not found into B
        nppc = 0
        goto 27
      endif
   54 K1 = INTO(I1)
      K2 = KI2
      IF (XB(K2+1).EQ.FLARC) K2 = K2+3
      IF (KI2.GT.K1) THEN
c... segments inside of B to start intersection
        DO 58 K = K1+1, K2
          NPPC = NPPC+1
          IF (NPPC.GT.maxpts) THEN
            NEWLUP = 11
            GOTO 99
          ENDIF
          XC(NPPC) = XB(K)
          YC(NPPC) = YB(K)
   58   CONTINUE
      ENDIF
      IF (KI2.EQ.K1.AND.INTO1(I2).GT.INTO1(I1)) GOTO 65
      IF (KI2.LE.K1) THEN
        IF (K1+1.GT.NPPB-1) GOTO 60
c... segments backside of B
        DO 59 K = K1+1, NPPB-1
          NPPC = NPPC+1
          IF (NPPC.GT.maxpts) THEN
            NEWLUP = 11
            GOTO 99
          ENDIF
          XC(NPPC) = XB(K)
          YC(NPPC) = YB(K)
   59   CONTINUE
c... segments start of B to start intersection
   60   DO 61 K = 1, K2
          NPPC = NPPC+1
          IF (NPPC.GT.maxpts) THEN
            NEWLUP = 11
            GOTO 99
          ENDIF
          XC(NPPC) = XB(K)
          YC(NPPC) = YB(K)
   61   CONTINUE
      ENDIF
      GOTO 69
   65 IF (K1.GE.K2) GOTO 69
c... segments frontside of B to start intersection
      DO 68 K = K1+1, K2
        NPPC = NPPC+1
        IF (NPPC.GT.maxpts) THEN
          NEWLUP = 11
          GOTO 99
        ENDIF
        XC(NPPC) = XB(K)
        YC(NPPC) = YB(K)
   68 CONTINUE
   69 IF (I2.EQ.II) THEN
        NPPC = NPPC+1
        IF (NPPC.GT.maxpts) THEN
          NEWLUP = 11
          GOTO 99
        ENDIF
c
c... start intersection A/B
c
        XC(NPPC) = XING(I2)
        YC(NPPC) = YING(I2)
        CALL DOUBLE (XC,YC,NPPC)
        CALL LITARC (XC,YC,NPPC)
        CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)
        if (intils.eq.0) then
          IF (KLOK.EQ.-1.OR.AREA.LT..25*stplst**2) THEN
c
c..... Note: The inequality above seems arbitrary (why .25 and not .2 ?),
c..... and sometimes causes the tool to miss some small pieces. In a test
c..... case, with the tool diameter of 0.3, some gaps of a size slightly
c..... less than 0.1 were left out. Eduard 4/8/1999
c
            NPPC = 0
            GOTO 27
          ENDIF
          IADD = IADD+1
          LAN1 = LANEP
          IF (IADD.GT.0) THEN
            LANEX = LANEX+1
            LAN1 = LANEX
          ENDIF
        else
          area = abs(area)
          if (area.lt.arebig) then
            if (insid(lann1+1).eq.0 .and. klok.eq.-1 .and.
     x           sc(169).ge.9.349 .and. area.gt.stplst**2) then
              newlup = -lann1
              lann1 = lann1+1
              lan1 = lann1
              insid(lan1) = lanep
            else
              nppc = 0
              goto 27
            endif
          else
            lan1 = lanep
          endif
          call flipo (xc,yc,nppc)
        endif

      IF (NPPC.GT.maxpts) THEN
        NEWLUP = 11
        GOTO 99
      ENDIF

      CALL STOLAP (LUPOK,LAN1,MOM,XC,YC,NPPC,IERR)
      IF (IERR.EQ.10) GOTO 99
        INDEX2 = INDEX2+1
        NPPC = 0
        GOTO 27
      ELSE
        INGO1(I2) = -1
        I = I2
        GOTO 29
      ENDIF
c
C --- FOR END POINT REARRANGE INDICES
c
   90 IF (LOOK.EQ.1) CALL ISLOOK (INDEX1,INDEX2)
c
c..... If no new loop data is stored for some reason (e.g., the area of
c..... new loops is too small), in the case this routine is called from
c..... LANCOM (and not from ILSCOM) the newlup flag is reset from 1 to 0.
c..... This corrects a erroneous behavior for some combinations of
c..... geometry and tool size.  Eduard 4/8/1999
c
   99 if (intils.eq.0.and.iadd.eq.-1.and.newlup.eq.1) newlup = 0

      if (lchged) call cpylap (xb0,yb0,nppb0,xb,yb,nppb)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  8. SUBROUTINE LUPLUP
C **  purpose of subroutine: FIND INTERSECTIONS BETWEEN FIRST LOOP TO
c **                         SECOND LOOP
c **    input:
C **      XA,YA,NPPA...FIRST LOOP
C **      XB,YB,NPPB...SECOND LOOP
C **      XL1,YL1
c **      XL2,YL2
c **      LLT,NBOX.....ARRAYS OF BOXES FOR SECOND LOOP
C **    output:
c **      INGO.........INTERsect SEGMENT values ON FIRST LOOP
C **      INTO.........INTERsect SEGMENT values ON SECOND LOOP
c **      xing,ying....x,y intersect values
C **      INGOS........NO OF INTERSECTIONS
c **********************************************************************
c **********************************************************************

      SUBROUTINE LUPLUP (XA,YA,NPPA,XB,YB,NPPB,XL1,YL1,XL2,YL2,LLT,NBOX,
     x                   INGO,INTO,intmp,XING,YING,INGOS)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)
      real*4 XING(25),YING(25)
      integer*4 INGO(25),INTO(25),intmp(25)

      integer*2 karc0, karc1
      real*4 dx, dy, eps, xi1, yi1

c     eps = tol/25.
c
c..... Eduard 4/30/1999. Changed the value of EPS.
c
      eps = tol/2.
      KLOSED = 1
      INGOS = 0
      ILOOP = 0
      MM1 = 1
      MM2 = NPPB-1
      XMM = XB(1)
      YMM = YB(1)
      XINT = XMM
      YINT = YMM
      xi1 = xint
      yi1 = yint
      LSEG = 1
      karc0 = 0
      karc1 = 0

      K = 0
   17 K = K+1
      LEG = -2
      karc0 = karc1
      IF (K.GE.NPPA) GOTO 99

   19 IF (ILOOP.EQ.1) THEN
        X1 = XINT
        Y1 = YINT
        XMM = XINT
        YMM = YINT
      ELSE
        X1 = XA(K)
        Y1 = YA(K)
      ENDIF

      IF (XA(K+1).EQ.FLARC) THEN
        KARC1 = 1
        R1 = XA(K+2)
        KLOK1 = YA(K+2)
        XC1 = XA(K+3)
        YC1 = YA(K+3)
        X2 = XA(K+4)
        Y2 = YA(K+4)
        K1 = 3
      ELSE
        KARC1 = 0
        X2 = XA(K+1)
        Y2 = YA(K+1)
        K1 = 0
      ENDIF
      dx = x2 - x1
      dy = y2 - y1
21    if (abs(dx).le.eps .and. abs(dy).le.eps) then
        iloop = 0
      else
        CALL SEEK (LEG,1,NBOX,KARC1,XC1,YC1,R1,KLOK1,
     +          X1,Y1,X2,Y2,XB,YB,MM1,MM2,XMM,YMM,LLT,XL1,YL1,XL2,YL2,
     +          KLOSED,LSEG,NBOX1,XINT,YINT,ILOOP)
      endif
c
C --- ADD INTERSECTION POINT
c
      IF (ILOOP.EQ.1) THEN
c
c..... Eduard 4/30/1999. If two straight intervals intersect on a
c..... whole subinterval (and SEEK typically finds its two endpoints),
c..... disregard all but the (subinterval's) starting point.
c
        if (ingos.ge.1. and. leg.eq.-2) then
          if (karc0.eq.0 .and. karc1.eq.0 .and.
     +        xb(lseg+1).ne.flarc .and. ingo(ingos).eq.k-1) then
            if ((abs(xi1-x1).le.eps .and. abs(yi1-y1).le.eps) .or.
     +          (abs(xint-x1).le.eps .and. abs(yint-y1).le.eps)) then
               iloop = 0
               x1 = xint + 1.42*eps*dx
               y1 = yint + 1.42*eps*dy
               goto 21
            endif
          endif
        endif
        LEG = -3
        INGOS = INGOS+1
        if (ingos .gt. maxing) then
          ingos = maxing
          goto 99
        endif
        INGO(INGOS) = K
        INTO(INGOS) = LSEG
        intmp(INGOS) = LSEG
        dx = xint - xb(lseg)
        dy = yint - yb(lseg)
c
c..... Eduard 4/30/1999. For consistence, if the intersection point
c..... coincides with the LSEG point, put in the INTO array the previous
c..... interval (so that, if there is a choice, an intersection is at an
c..... endpoint of an interval).
c
        if (abs(dx).le.eps .and. abs(dy).le.eps) then
          lprev = lseg - 3
          if (lprev.gt.0 .and. xb(lprev).eq.flarc) then
            into(ingos) = lprev - 1
          else if (lseg.gt.1) then
            into(ingos) = lseg - 1
          else
            into(ingos) = nppb - 1
          endif
        endif
        xi1 = xint
        yi1 = yint
        XING(INGOS) = XINT
        YING(INGOS) = YINT

c
C --- CHECK FOR DOUBLES
c
        IDUBL = 0
        DO 74 J = 1, INGOS-1
          IF (INGO(INGOS).NE.INGO(J)) GOTO 74
          IF (INTO(INGOS).NE.INTO(J)) GOTO 74
          IF (ABS(XING(INGOS)-XING(J)).GT.0.1) GOTO 74
          IF (ABS(YING(INGOS)-YING(J)).GT.0.1) GOTO 74
          IDUBL = 1
   74   CONTINUE
        IF (IDUBL.EQ.1) INGOS = INGOS-1
        GOTO 19
      ENDIF
c
c..... Increment k if no intersection
c
      K = K+K1
      GOTO 17

99    continue
c
c..... Eduard 4/16/1999. I found it better to eliminate
c..... single (i.e., tangential) intersections.
c
      if (ingos .eq. 1) ingos = 0

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  9. SUBROUTINE ARRINP
C **  purpose of subroutine: ARRANGE INPUT DATA IN COMMON ARRAYS
c **    output:
C **      IERR...0- OK,  1- TOO MANY POINTS IN POCKET
C **                     2- TOO MANY POINTS IN ISLAND
C **                     3- POCKET INTERSECTS ITSELF
C **                     4- ISLAND INTERSECTS ITSELF
C **                     5- POCKET INTERSECTS ISLAND
C **                     6- ISLAND INTERSECTS ISLAND
C **                     7- ISLAND OUTSIDE OF POCKET
C **                     8- TOO MANY INPUT POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE ARRINP (IERR)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*8 xx, yy
      real*4 XA(maxpts),YA(maxpts)
      integer*2 k,k1,k2
      integer*4 kk,ier

      IERR = 0
      LANES1 = LANES
      LANES = 0
      LUP = 0
      MOM = 0
      ier = 0
c
C --- CHECK IF TOTAL NUMBER OF INPUT POINTS EXCEEDS 3000
c
      IF (LOCA(LANES1+1).GT.3000) THEN
        IERR = 8
        GOTO 99
      ENDIF
      DO 16 K = 1, LOCA(LANES1+1)-1
        kk = k
        call rrget(kk,xx,yy,ier)
        if (ier .gt. 0) goto 99
        call rrput(kk+3000,xx,yy,ierr)
   16 CONTINUE
c
c... Start of next loop is saved in K2 because STOLAP updates LOCA array
c... if DOUBLE has removed points. - FSR 53360  IJD 26 Sep 1997
c
      k2 = loca(1) + 3000
      DO 26 LAN = 1, LANES1
        k1 = k2
        k2 = LOCA(LAN+1)+3000
        NPPA = k2-k1
        IF (NPPA.GT.maxpts) THEN
          IERR = 2
          IF (LAN.GT.1) IERR = 3
          GOTO 99
        ENDIF
        DO 37 K = 1, NPPA
          kk = k1+k-1
          call rrget(kk,xx,yy,ier)
          if (ier .gt. 0) goto 99
          XA(K) = xx
          YA(K) = yy
c         XA(K) = XRR(K1+K-1)
c         YA(K) = YRR(K1+K-1)
   37   CONTINUE
        CALL DOUBLE (XA,YA,NPPA)
c
C --- POCKET MUST BE CCW, ISLANDS CW
c
        CALL xccw (XA,YA,NPPA,KLOK)
        IF (KLOK.EQ.-1.AND.LAN.EQ.1) THEN
          CALL FLIPO (XA,YA,NPPA)
        ENDIF
        IF (KLOK.EQ.1.AND.LAN.GT.1) THEN
          CALL FLIPO (XA,YA,NPPA)
        ENDIF
        LANE1 = LAN
        IF (LAN.GT.1) LANE1 = 100+LAN-1
        CALL STOLAP (LUP,LANE1,MOM,XA,YA,NPPA,IERR)
   26 CONTINUE

   99 if (ierr.eq.0 .and. ier.gt.0) ierr = ier
      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  10. SUBROUTINE LANCOM
c **  purpose of subroutine: COMPUTE COMMON LOOPS FOR LOOPS between
c **                         POCKET AND ISLANDS
c **    input:
c **      LUPOK....LOOP NUMBER OF POCKET LOOP
c **      LUPIL....LOOP NUMBER OF ISLANDS LOOPS
c **      LANEX....MAX LANE NUMBER, EXCLUDING ISLANDS
c **      ISON.....NO OF REMAINING ACTIVE ISLANDS
c **    output:
c **      LANEX....MAX LANE NUMBER, EXCLUDING ISLANDS
c **      ISON.....NO OF REMAINING ACTIVE ISLANDS
c **      NEWLUP... 0- NO error or loop formed
c **                1- NEW LOOP WITH ISLAND FORMED
c **               10- TOO MANY LOOPS
c **               11- Max points exceeded
c **               14- Pocket loop contained within island loop
c **********************************************************************
c **********************************************************************

      SUBROUTINE LANCOM (LUPOK,LUPIL,LANEX,ISON,NEWLUP)

      include 'com4a.com'
      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid

      integer*2 lanex,lupok,lupil,ison,newlup,lan,lan1

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      real*4 rad,aremin,area
      integer*2 nppa,nppb,lanex0,intils,klok,klokb,iside,nc,mompola
      integer*2 lanes0,lanes1
      logical lv95,lhavepola,ladded

      lv95  = sc(169).lt.9.549d0
      mompola = 0
      lanes0 = islnds + 2

      lhavepola = .false.
      rad = stpcov
      aremin = 25*tol*tol

C --- CUT EACH POCKET LANE AGAINST EACH ISLAND LANE
      NEWLUP = 0
      LAN = 0
      lanex0 = lanex
   25 LAN = LAN+1
      IF (LAN.GT.LANEX) GOTO 99
      IF (ISON.EQ.0) GOTO 99

   27 CALL GETLUP (LAN,LUPOK,LN,XA,YA,NPPA,ierr)
      if (ierr.gt.1) then
         newlup = 11
         goto 99
      endif
      IF (LN.EQ.0) GOTO 25
      MOM = MAMA(LN)

       if (lhavepola .and. mom.ne.mompola) then
         do i = lanes0,lanes
           if (mama(i).eq.mompola .and. loop(i).eq.lupok) then
             call getlan (i,xb,yb,nppb)
             call addpolb (xb,yb,nppb,0,ladded)
             if (.not.ladded) goto 29
           endif
         enddo

         call subpokpols (aremin,nc)
         if (nc .gt. 0) then
           lup = loop(mompola) + 1
           do i = 1,nc
             call getpolb (i,xb,yb,nppb)
             LANEX = LANEX+1
             lan1 = lanex
             call stolap (lup, lan1, mompola, xb, yb, nppb, ierr)
             if (ierr .eq. 10) then
               newlup = ierr
               goto 99
             endif
           enddo
         endif

29       continue
         mompola = 0
         lhavepola = .false.
       endif

      DO 32 ILE = 101, islnds+100
        if (lan.gt.1 .and. insid(lan).eq.ile) goto 32
        CALL GETLUP (ILE,LUPIL,IL,XB,YB,NPPB,ierr)
        if (ierr.gt.1) then
           newlup = 11
           goto 99
        endif
        IF (IL.EQ.0) GOTO 32
        LANES1 = LANES+1
        intils = 0
        CALL COMLUP (XA,YA,NPPA,XB,YB,NPPB,lupil,MOM,LUPOK,LAN,intils,
     +               LANEX,NEWLUP)
        IF (NEWLUP.ge.11) GOTO 99
        IF (NEWLUP.EQ.0) THEN
          xpt = xa(1)
          ypt = ya(1)
          call inloop (xpt, ypt, xb, yb, nppb, in, ierr)
          if (ierr.eq.1) then
            newlup = 11
            goto 99
          endif
          if (in.eq.1) then
c... pocket loop completely in island loop
            if (lanes .le. 4) then
              newlup = 13
              return
            endif
            newlup = 1
            call stolap (lupok, lan, mom, xb, yb, nppb, ierr)
            if (ierr .eq. 10) then
              newlup = ierr
              goto 99
            endif
          endif
        endif
        IF (NEWLUP.EQ.1) THEN

        if (LUPIL.EQ.2 .and. .not.lhavepola .and. .not.lv95 .and.
     x                                      loop(mom).gt.0) then
            call rstpokpol (1)
            iside = -1
            CALL GETLAN (mom,XA,YA,NPPA)
            CALL XCLOCK (XA,YA,NPPA,AREA,KLOK)
            call createpola (xa,ya,nppa,iside,area,lhavepola)
            if (lhavepola) mompola = mom
        endif

c... set common arrays for commom loop merge
          ISON = ISON-1
          LOOP(LN) = 0
          LOOP(IL) = 0
          IF (LUPIL.EQ.2) THEN
            LOOP(IL) = -2
            DO 28 I = 1, LANES
              IF (LANE(I).EQ.ILE.AND.LOOP(I).EQ.1) THEN
                MAMA(IL) = I
                MAMA(I) = LANES1
                if (lhavepola) then
                  CALL GETLAN (i,xb,yb,nppb)
                  call addpolb (xb,yb,nppb,1,ladded)
                  if (.not.ladded) then
                    mompola = 0
                    lhavepola = .false.
                  endif
                endif
              else IF (LANE(I).gt.100 .AND. LOOP(I).EQ.1) THEN
c
c..... qar 96162: add other active first island loops which are inside
c..... the loop IL even if their lane number does not match ILE
c
                if (lhavepola) then
                  call lpinside (i,xb,yb,nppb,il,in)
                  if (in .eq. 1) then
                    call addpolb (xb,yb,nppb,1,ladded)
                    if (.not.ladded) then
                      mompola = 0
                      lhavepola = .false.
                    endif
                  endif
                endif
              ENDIF
   28       CONTINUE
          ENDIF
          IF (ISON.EQ.0) GOTO 40
          GOTO 27
        ENDIF
   32 CONTINUE
   40 CONTINUE
      IF (ISON.GT.0) GOTO 25
c   75 CONTINUE

       if (lhavepola) then
         do i = lanes0,lanes
           if (mama(i).eq.mompola .and. loop(i).eq.lupok) then
             call getlan (i,xb,yb,nppb)
             call xccw (xb,yb,nppb,klokb)
             if (klok .ne. klokb) CALL FLIPO (xb,yb,nppb)
             call addpolb (xb,yb,nppb,0,ladded)
             if (.not.ladded) goto 79
           endif
         enddo

         call subpokpols (aremin,nc)
         if (nc .gt. 0) then
           lup = loop(mompola) + 1
           do i = 1,nc
             call getpolb (i,xb,yb,nppb)
             LANEX = LANEX+1
             lan1 = lanex
             call stolap (lup, lan1, mompola, xb, yb, nppb, ierr)
             if (ierr .eq. 10) then
               newlup = ierr
               goto 99
             endif
           enddo
         endif

79       continue
         mompola = 0
         lhavepola = .false.

       endif

   99 if (lanex.gt.lanex0 .and. newlup.eq.0) newlup = 1
c
c..... Eduard 7/15/1999 Make sure adding new loops does set the flag
c..... newlup to one. Needed in LANPOK, to reassign the lane() numbers
c..... if necessary.
c
      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  11. SUBROUTINE ILSCOM
C **  purpose of subroutine: COMPUTE COMMON LOOPS FOR LOOPS between
c **                         ISLANDS
c **    input:
C **      LUPIL....LOOP NUMBER OF ISLANDS LOOPS
C **      ISON.....NO OF REMAINING ACTIVE ISLANDS
C **    output:
C **      ISON.....NO OF REMAINING ACTIVE ISLANDS
c **      NEWLUP...1- NEW LOOP WITH ISLAND FORMED, 0- NONE, >1 - error
c **********************************************************************
c **********************************************************************

      SUBROUTINE ILSCOM (LUPIL,ISON,lanex,NEWLUP)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)

      integer*2 ierr,il1,i1,il2,i2,intils,idx
      real*8 ver
      
      idx = 169
      call getsc(idx,ver)

      ierr = 0

C --- CUT EACH island LANE AGAINST every other ISLAND LANE
      NEWLUP = 0
      il1 = 100

      IF (islnds .LT. 2) GOTO 99
      LAN = 0
      KIL = 0
c     DO 72 IL1=101,islnds+99
25      il1 = il1 + 1
        if (il1 .gt. islnds+99) goto 99
27      IF (ISON.EQ.0) GOTO 99
        i1 = 0
        CALL GETLUP (IL1,LUPIL,I1,XA,YA,NPPA,ierr)
        IF (I1.EQ.0) GOTO 25
        if (ierr.gt.1) goto 99
        call flipo(xa,ya,nppa)
        MOM = MAMA(I1)
        DO 32 IL2 = IL1+1, islnds+100
          i2 = 0
          CALL GETLUP (IL2,LUPIL,I2,XB,YB,NPPB,ierr)
          IF (I2.EQ.0) GOTO 32
          if (ierr.gt.1) goto 99
          call flipo(xb,yb,nppb)
          LANES1 = LANES+1
          intils = 1
          CALL COMLUP (XA,YA,NPPA,XB,YB,NPPB,lupil,MOM,LUPIL,IL1,
     +                 intils,LUPIL,NEWLUP)
          IF (NEWLUP.EQ.11) GOTO 99
          if (newlup.lt.0) then
            lanex = lanex - newlup
            newlup = 1
          endif
          IF (NEWLUP.EQ.1) THEN
            ISON = ISON-1
            LOOP(I1) = 0
            LOOP(I2) = 0
            IF (LUPIL.EQ.2) THEN
c
c.....Switched LOOP(I1) to 0 so the loop being replaced is not seen
c.....a second time with the changes made in CUTPOCK - ASF 10/2/13
c
              IF (VER.GT.9.949) THEN
                LOOP(I1) = 0
              ELSE
                LOOP(I1) = -2
              ENDIF
              LOOP(I2) = -2
              DO 28 I = 1, LANES
                IF (LANE(I).EQ.IL1.AND.LOOP(I).EQ.1) THEN
                  MAMA(I1) = I
                  MAMA(I) = LANES1
                ENDIF
   28         CONTINUE
              DO 29 I = 1, LANES
                IF (LANE(I).EQ.IL2.AND.LOOP(I).EQ.1) THEN
                  MAMA(I2) = I
                  MAMA(I) = LANES1
                ENDIF
   29         CONTINUE
            ENDIF
            goto 27
          ENDIF
   32   CONTINUE
        if (ison .gt. 0) goto 25

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  12. SUBROUTINE LANPOK
C **  purpose of subroutine: COMPUTE all  POCKET LANES for each loop
c **    input:
C **      RADIUS...TOOL RADIUS of 1st loop
C **      LUPOK....LOOP NUMBER OF CREATED POCKET LOOP
c **      LANEX....MAX LANE NUMBER, EXCLUDING ISLANDS
C **    output:
C **      NOLUP....0- MORE LOOPS CREATED, 1- NO MORE LOOPS
C **      IERR.....O- OK,  9- END PROBLEM
c **********************************************************************
c **********************************************************************

      SUBROUTINE LANPOK (RADIUS,LUPOK,LANEX,NOLUP,IERR)

      include 'com4a.com'
      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),rad

      integer*2 lanbig,indbig,indbg1,index,index1,isplit
      integer*2 lastlup
      integer*4 lanes1
      real*4 arebig
      logical lv95,lhavepola

      lv95  = sc(169).lt.9.549d0
      if (ierr.eq.0 .or. lanex.le.1 .or. look.eq.1) then
         isplit = 0
      else
         isplit = 1
         lanes1 = lanes
         lanbig = 1
         indbig = lanes
         arebig = 0.0
      endif

      IERR = 0
      NOLUP = 1
      LANEX1 = LANEX
      ISLE = 0
      KIVUN = 1
      LASTLUP = 0
      stpcur = stpmax
      DO 45 LAN = 1, LANEX1
        rad = abs (radius)
        if (stpcur .lt. rad .and. lupok .gt. 1) rad = stpcur
        CALL GETLUP (LAN,LUPOK-1,LN,XA,YA,NPPA,ierr)
        if (ierr.gt.1) goto 99
        IF (LN.EQ.0) GOTO 45
        RR = RAD*RAD
        CALL XCLOCK (XA,YA,NPPA,AREA,KLOK)
        area = abs(area)

        IF (AREA.LE.(1.44*rr)) THEN
          if (.not.lv95 .and.
     x        radius.gt.0 .and. area.ge.4.*stpcov*stpcov) then
            iside = -1
            call createpola (xa,ya,nppa,iside,area,lhavepola)
            if (lhavepola) goto 33
          endif
          LANES = LANES+1
          LANE(LANES) = LAN
          LOOP(LANES) = -LUPOK
          LOCA(LANES+1) = LOCA(LANES)
          MAMA(LANES) = LN
          GOTO 45
        ENDIF

   33   continue
        iside = -1
        if (radius.lt.0) iside = 1
        CALL ONEOFFS (XA,YA,NPPA,ISLE,KIVUN,ISIDE,RAD,LN,LUPOK,
     +                LAN,LANEX,LASTLUP,IERR)
        IF (NPPA.GT.maxpts) IERR = 11
        IF (IERR.EQ.9.OR.IERR.EQ.11) GOTO 99
        IF (IERR.EQ.1) THEN
c... no lane/loop output into xy arrays
          IF (LASTLUP .eq. 0 .and. lupok .gt. 1) THEN
            rad = .5*stpmax
            LASTLUP = 1
c
c..... the line below was added in 18.7 to do the second attempt with the
c..... same contour as the first. unfortunately it made many test cases
c..... quite different. commented out since it was not a necessary fix.
c
c            CALL GETLUP (LAN,LUPOK-1,LN,XA,YA,NPPA,ierr)
            GOTO 33
          ENDIF
          LANES = LANES+1
          LANE(LANES) = LAN
          LOOP(LANES) = -LUPOK
          LOCA(LANES+1) = LOCA(LANES)
          MAMA(LANES) = LN
          if (.not.lv95) LASTLUP = 0
          IERR = 0
          goto 45
        ENDIF

        IF (isplit.eq.1 .and. area.gt.arebig) THEN
           lanbig = lan
           arebig = area
           do 44 indbig = lanes, lanes1+1, -1
             if (lane(indbig) .eq. lan) goto 445
44         continue
445        continue
        endif

        LASTLUP = 0
        NOLUP = 0
   45 CONTINUE
      stplst = stpcur

      if (nolup.eq.0 .and .isplit.eq.1) then
        index = mama(lanes1)
        lan1 = lane(index)
        if (lanbig .eq. lan1) goto 99
        if (loop(indbig) .le. 0) goto 99

        do 50 index = lanes1+1, lanes
          if (lane(index) .eq. lan1) goto 51
50      continue

51      if (sc(169).lt.9.049d0 .or. lane(index).ne.lan1) goto 99
c
c..... FSR 60746,60752 - this switch was introduced to make cutpath
c..... work, but it sometimes messes up - for instance at the end of
c..... contour, when the mama array is fixed up. Also the switch does not
c..... guarantee cutpath will work. So decided to deal with cutting across
c..... problems in cutpath, starting with 9.3
c
        if (sc(169) .ge. 9.3) goto 99
        index1 = mama(index)
        indbg1 = mama(indbig)
        if (mama(index1) .ne. mama(indbg1)) goto 99
        lane(index) = lanbig
        lane(index1) = lanbig
        lane(indbig) = lan1
        lane(indbg1) = lan1
      endif

99    RETURN
      END

c **********************************************************************
c **********************************************************************
c **  13. SUBROUTINE LANILE
c **  purpose of subroutine: COMPUTE ISLANDS LOOPS
c **    input:
C **      FSTOFF...FIRST OFSET
C **      STEP.....MAX STEP AFTER FIRST
C **      LUPIL....LOOP NUMBER OF CREATED ISLAND LOOP
C **      LANEX....MAX LANE NUMBER, EXCLUDING ISLANDS
C **    output:
c **      IERR.....O- OK, 1- NO NEW LOOP, 9- END PROBLEM
c **********************************************************************
c **********************************************************************

      SUBROUTINE LANILE (FSTOFF,STEP,LUPIL,LANEX,IERR)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 FSTOFF(120)
      real*4 XA(maxpts),YA(maxpts)
      integer*2 lastlup

      ISLE = 1
      IERR = 0
      lastlup = 0
      KIVUN = 1
      DO 55 ILE = 101, islnds+100
        CALL GETLUP (ILE,LUPIL-1,LN,XA,YA,NPPA,ierr)
        if (ierr.gt.1) goto 99
        IF (LN.EQ.0) GOTO 55
        rad = fstoff(ile-99)
        if (lupil.gt.1) rad = step
        r = abs(rad)
        iside = -1
        if (rad.lt.0) iside = 1
        CALL ONEOFFS (XA,YA,NPPA,ISLE,KIVUN,ISIDE,R,LN,LUPIL,
     +                ILE,LANEX,lastlup,IERR)
        IF (IERR.EQ.1) IERR = 0
        IF (NPPA.GT.maxpts) IERR = 11
        IF (IERR.NE.0) GOTO 99
   55 CONTINUE

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  14. SUBROUTINE LOOKPT
C **  purpose of subroutine: FIND MANUAL ENDPOINT ON CONTOUR
c **    input:
C **      XA,YA,NPPA...POCKET
C **    output:
c **      LSEG.........SEGMENT OF ENDPOINT
C **      UEG..........RELATIVE LOCATION OF ENDPOINT
C **      XN,YN........COORDINATES OF ENDPOINT
C **      IERR.........0 - OK, 9 - ERROR - ENDPOINT TOO FAR
c **********************************************************************
c **********************************************************************

      SUBROUTINE LOOKPT (XA,YA,NPPA,LSEG,UEG,XN,YN,IERR)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      IERR = 0
      LSEG = 0
      UEG = 0.0
      XN = 0.0
      YN = 0.0
      INDEX = 1
      DIST = 10000000.0
      K = 0
   21 K = K+1
      IF (K.GE.NPPA) GOTO 90
      IF (XA(K+1).EQ.FLARC) THEN
        R = XA(K+2)
        KLOK = YA(K+2)
        CALL DSPTARC (XLOOK,YLOOK,R,KLOK,XA(K+3),YA(K+3),
     +        XA(K),YA(K),XA(K+4),YA(K+4),DIST1,XN1,YN1,U)
c
c..... Eduard 5/19/1999. Trying to find the closest point on the segment,
c..... even if the endpoint does not project inside the interval.
c
        if (u.ge.0.0 .and. u.lt.tol) u = tol
        if (u.le.1.0 .and. u.gt.(1.-tol)) u = 1. - tol
        IF (U.GT.0.0.AND.U.LT.1.0.AND.DIST1.LT.DIST) THEN
          DIST = DIST1
          LSEG = K
          UEG = U
          XN = XN1
          YN = YN1
        ENDIF
        K = K+3
      ELSE
        CALL DSPTLN (XLOOK,YLOOK,XA(K),YA(K),XA(K+1),YA(K+1),
     +        DIST1,XN1,YN1)
        CALL USEG (XA(K),YA(K),XA(K+1),YA(K+1),XN1,YN1,U)
c
c..... Eduard 5/19/1999. See comment above.
c
        if (u.ge.0.0 .and. u.lt.tol) u = tol
        if (u.le.1.0 .and. u.gt.(1.-tol)) u = 1. - tol
        IF (U.GT.0.0.AND.U.LT.1.0.AND.DIST1.LT.DIST) THEN
          DIST = DIST1
          LSEG = K
          UEG = U
          XN = XN1
          YN = YN1
        ENDIF
      ENDIF
      GOTO 21
   90 IF (DIST.GT.1000000.0) IERR = 9

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  15. SUBROUTINE CLOSEST
C **  purpose of subroutine: FIND CLOSEST POINT ON CONTOUR TO A POINT
c **    input:
C **      XA,YA,NPPA...POCKET contour
C **    output:
c **      LSEG.........SEGMENT OF CLOSEST POINT
C **      UEG..........RELATIVE LOCATION OF CLOSEST POINT
C **      XN,YN........COORDINATES OF CLOSEST POINT
C **      DIST.........DISTANCE OF CLOSEST POINT
c **********************************************************************
c **********************************************************************

      SUBROUTINE CLOSEST (XA,YA,NPPA,LSEG,UEG,XN,YN,DIST)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),dist,ueg,xn,yn
      integer*2 nppa,lseg

      call dsptlap (XLOOK,YLOOK,XA,YA,NPPA,LSEG,UEG,XN,YN,DIST)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  16. SUBROUTINE ISLOOK
C **  purpose of subroutine: DETERMINE LANE 1 IN POCKET ISLAND
c **                         INTERSECTION
c **    input:
C **      INDEX1,INDEX2...RANGE OF INDICES CREATED IN INTERSECTION
c **********************************************************************
c **********************************************************************

      SUBROUTINE ISLOOK (INDEX1,INDEX2)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      IF (INDEX1.EQ.INDEX2) GOTO 99
      DO 12 K = INDEX1, INDEX2
        IF (LANE(K).EQ.1) GOTO 21
   12 CONTINUE
      GOTO 99

C --- FIND CLOSEST LOOP TO ENDPOINT
   21 K1 = K
      DIST = 10000000.0
      DO 37 K = INDEX1,INDEX2
        CALL GETLAN (K,XA,YA,NPPA)
        CALL CLOSEST (XA,YA,NPPA,LSEG,USEG,X1,Y1,DIST1)
        IF (DIST1.LT.DIST) THEN
          KL = K
          DIST = DIST1
        ENDIF
   37 CONTINUE
      IF (LANE(KL).EQ.1) GOTO 99

C --- SWITCH AROUND
      LANE(K1) = LANE(KL)
      LANE(KL) = 1

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  17. SUBROUTINE XSHARP
c **  purpose of subroutine: computes the maximum lane/loop step over
c **    input:
c **      xa,ya,nppa...lane/loop point arrays and size value
c **      step.........effective tool diameter value
c **    output:
c **      stepok.......maxium step over for lane/loop geometry
c **********************************************************************
c **********************************************************************

      SUBROUTINE XSHARP (xa, ya, nppa, step, stepok)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 xa(maxpts), ya(maxpts)

      stepit = step
      k = -1
5     k = k+1
      if (k.ge.nppa) goto 90
      if (nppa.ge.5 .and. xa(nppa-3).eq.flarc) then
        n = nppa-4
        r = xa(n+2)
        klok = ya(n+2)
        xc = xa(n+3)
        yc = ya(n+3)
        call tanarc(2, xc, yc, r, klok, xa(n), ya(n), xa(n+4), ya(n+4),
     x               dx1, dy1)
      else
        dx1 = xa(k+1)-xa(nppa-1)
        dy1 = ya(k+1)-ya(nppa-1)
        dd = sqrt(dx1**2+dy1**2)
        if (dd.lt.tol) goto 5
        dx1 = dx1/dd
        dy1 = dy1/dd
      endif
10    k = k+1
      if (k.ge.nppa) goto 90
      if (xa(k+1).eq.flarc) then
        r = xa(k+2)
        klok = ya(k+2)
        xc = xa(k+3)
        yc = ya(k+3)
        call tanarc(1, xc, yc, r, klok, xa(k), ya(k), xa(k+4), ya(k+4),
     x               dx2, dy2)
        call turned (dx1, dy1, dx2, dy2, step, stepit)
        call tanarc(2, xc, yc, r, klok, xa(k), ya(k), xa(k+4), ya(k+4),
     x               dx1, dy1)
        k = k+3
        goto 10
      else
        dx2 = xa(k+1)-xa(k)
        dy2 = ya(k+1)-ya(k)
        dd = sqrt(dx2**2+dy2**2)
        if (dd.lt.tol) goto 10
        dx2 = dx2/dd
        dy2 = dy2/dd
        call turned (dx1, dy1, dx2, dy2, step, stepit)
        dx1 = dx2
        dy1 = dy2
        goto 10
      endif
90    if (stepit.lt.stpmin) stepit = stpmin
      if (stepit.lt.stepok) stepok = stepit

99    return
      end
