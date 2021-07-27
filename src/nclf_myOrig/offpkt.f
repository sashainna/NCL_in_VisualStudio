C***********************************************************************
c**    NAME           : offpkt.f
c**    CONTAINS:
C**      SUBROUTINE ONEOFFS
C**      SUBROUTINE ONEOFF1
C**      SUBROUTINE chkcover
C**      SUBROUTINE createpola
C**      SUBROUTINE addpola
C**      SUBROUTINE addpolb
C**      SUBROUTINE dsptlap
C**      SUBROUTINE dslplp
C**      SUBROUTINE XROSS
C**      SUBROUTINE XROSS1
C**      SUBROUTINE XROSS2
C**      SUBROUTINE overlaps
C**      SUBROUTINE smloffs
C**      SUBROUTINE cpylap
C**      SUBROUTINE offslp
C**      SUBROUTINE fostermom
C**      SUBROUTINE evolvlp
C**
C**    MODULE NAME AND RELEASE LEVEL
C**       offpkt.f , 25.1
C**    DATE AND TIME OF LAST  MODIFICATION
C**       04/29/15 , 15:10:23
C***********************************************************************
c**
c** copyright (c) 1990 MILLS DATA SYSTEMS CORPORATION
c**
c **********************************************************************
c **********************************************************************
c **  SUBROUTINE ONEOFFS
C **  purpose of subroutine: COMPUTES OFFSET FOR RADIUS RAD
C **    INPUT:
c **      XA,YA.....INPUT STRING ARRAYS
C **      NPPA......NO OF ELEMENTS IN INPUT ARRAY
C **      ISLE......0- POCKET, 1- ISLAND
C **      KIVUN.....1/-1 DIRECTION FORWARD/BACKWARD
C **      kside.....1/-1 SIDE RIGHT/LEFT
C **      RADIUS....current step-over
C **      MOM.......INDEX OF MOTHER LOOP
C **      LUP.......CURRENT LOOP
C **      LANEP.....PRESENT LANE
C **      LANEX.....MAX LANE NUMBER
C **    OUTPUT:
C **      LANEX.....MAX LANE NUMBER
C **      IERR......0- OK, new lane/loop stored
c **                1- OK, NO new lane/loop stored
c **                9- ERROR IN OFFSET END
c **********************************************************************
c **********************************************************************

      SUBROUTINE ONEOFFS (XA,YA,NPPA,ISLE,KIVUN,kside,RADIUS,
     x                    MOM,LUP,LANEP,LANEX,LASTLUP,IERR)
      include 'com.com'
      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      real*4 XC(maxpts),YC(maxpts)
      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)
      integer*4 INGO(mxingos),INTO(mxingos)
      real*4 XING(mxingos),YING(mxingos)
      real*4 HLDARE,area,x1,y1,xmm,ymm,useg,radius,rad,r
      integer*2 lastlup
      integer*2 nppa,nppb,nppc,iadd,klosed,newlup,index1,index2,is
      integer*2 lseg,klok,lann,isle,kivun,kside,mom,lup,lanep,lanex
      integer*2 ierr,iside,icen,noboxes,ingos,ing,mm1,mm2
      logical lv93,lv94,lv95,lperim,lisle

c---  CHECK FOR DOUBLE POINTS, FLIP FOR POSITIVE DIRECTION
      IERR = 0
      IADD = 0
      KLOSED = 1
      LSEG = 1
      NEWLUP = 0
      XMM = 0.0
      YMM = 0.0
      INDEX1 = LANES+1
      INDEX2 = LANES
      rad = abs(radius)
      iside = kside
      IF (KIVUN.EQ.-1) THEN
        CALL FLIPO (XA,YA,NPPA)
        KIVUN = 1
        ISIDE = -ISIDE
      ENDIF

      lv93  = sc(169).lt.9.349d0
      lv94  = sc(169).lt.9.449d0
      lv95  = sc(169).lt.9.549d0

C --- ARRANGE CLOSED OFFSET
C... remove like points
      CALL DOUBLE (XA,YA,NPPA)
C... remove in-line points, arcs and overlap
      nppb = nppa
      CALL STRAIT (XA,YA,NPPA)
c
c..... fix fo qar 95289: STRAIT could remove a 'fold' and create
c..... another double point
c
      if (.not.lv95 .and. nppb.gt.nppa .and. isle.eq.1) then
      CALL DOUBLE (XA,YA,NPPA)
      endif

C --- END POINT - FIRST LOOP ON POINT, REST ON FIRST SEG
C     NO END POINT OR LANE>1 - START AT MID LARGEST
      IF (LOOK.EQ.1.AND.LANEP.EQ.1) THEN
        IF (LUP.EQ.1) THEN
          CALL LOOKPT (XA,YA,NPPA,LSEG,USEG,X1,Y1,IERR)
          XLOOK = X1
          YLOOK = Y1
          IF (IERR.EQ.9) GOTO 99
          ICEN = 1
          ULOOK = USEG
          CALL SHUFFLE (XA,YA,NPPA,LSEG,ICEN,USEG,XMM,YMM)
        ELSE
          ICEN = 1
          LSEG = 1
          USEG = ULOOK
          CALL SHUFFLE (XA,YA,NPPA,LSEG,ICEN,USEG,XMM,YMM)
        ENDIF
      ELSE
        if (lv95) then
        CALL LARGEST (XA,YA,NPPA,LSEG)
        else
        CALL LARGES1 (XA,YA,NPPA,LSEG,lastlup)
        endif
        ICEN = 1
        USEG = 0.5
        CALL SHUFFLE (XA,YA,NPPA,LSEG,ICEN,USEG,XMM,YMM)
      ENDIF

C... remove in-line points, arcs and overlap
      CALL STRAIT (XA,YA,NPPA)
c
c..... qar 95170 - handle zero offset
c
      if (.not.lv94 .and. rad.lt.0.5*tol) then
        call cpylap (xa,ya,nppa,xc,yc,nppc)
        CALL DOUBLE (XC,YC,NPPC)
        NEWLUP = 1
        CALL STOLAP (LUP,LANEP,MOM,XC,YC,NPPC,IERR)
        IF (IERR.EQ.10) GOTO 99
        INDEX2 = INDEX2+1
        IADD = 1
        GOTO 95
      endif

      CALL XCLOCK (XA,YA,NPPA,HLDARE,KLOK)

      lperim = (isle.eq.0 .and. klok*iside.eq.-1 .and. .not.lv94)

      lisle = (.not.lv95 .and. isle.eq.1 .and. iside.eq.-1 .and.
     x                                 abs(HLDARE).gt.25*tol*tol)

      if (lperim .and. rad.lt.20*tol) then
        call defold (xa,ya,nppa)
      endif

      if (lperim .and. lup.gt.1 .and. .not.lv95) then
        call oneoff1 (XA,YA,NPPA,iside,rad,hldare,
     x                    MOM,LUP,LANEP,LANEX,lastlup,newlup,IERR)
        index2 = lanes
        if (ierr .le. 1) goto 95
        goto 99
      endif

      lanex0 = lanex
C---  COMPUTES FIRST OFFSET
10    CALL OFF1ST (KLOSED,ISIDE,ISLE,RAD,NPPA,XA,YA,XB,YB,NPPB,IERR)
      IF (IERR.GT.1) GOTO 99
      IF (IERR.EQ.1) GOTO 95
C      
C.....FSR 61324 handle special case of offset 
C     
      if (lisle .and. lup .eq. 1) then
        CALL LARGES2 (RAD,XA,YA,NPPA,XB,YB,NPPB,LSEG)
        ICEN = 1
        USEG = 0.5
        if (LSEG .GT. 1) then
            CALL SHUFFLE (XB,YB,NPPB,LSEG,ICEN,USEG,XMM,YMM)
        endif
      endif    
      
      if (lv95) then
        CALL DOUBLE (XB,YB,NPPB)
      else
        CALL DOUBL1 (XB,YB,NPPB)
      endif

C     CHANGE TO LUP.GT.1 WITHOUT GPR ********************************
      IF (LUP.GT.0) CALL LITARC (XB,YB,NPPB)
      IF (NPPB.LT.3) THEN
        IERR = 13
        if (lup.gt.1 .and. isle.eq.0) goto 95
        GOTO 99
      ENDIF

C---  REMOVES LOOPS FROM FIRST OFFSET TO CREATE TRUE OFFSET
      CALL BOXES(NPPB,XB,YB,XL1,YL1,XL2,YL2,LLT,NOBOXES)

      if (lisle) then
          call offisle (KLOSED,XB,YB,NPPB,XL1,YL1,XL2,YL2,LLT,NOBOXES,
     x                  LUP,LANEP,MOM,hldare,rad,newlup,ierr)
      
        index2 = lanes
        if (ierr .le. 1) goto 95
        goto 99
      endif
      
      INGOS = 0
      ING = 0
      MM1 = 1
      MM2 = NPPB-1
      XMM = XB(1)
      YMM = YB(1)
      CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     +  XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IERR)
      IF (NPPC.GT.maxpts) THEN
        IERR = 11
        GOTO 99
      ENDIF
      IF (IERR.EQ.11) GOTO 99
 11   CALL DOUBLE (XC,YC,NPPC)

C... CHANGE TO LUP.GT.1 WITHOUT GPR ***
      IF (LUP.GT.0) CALL LITARC (XC,YC,NPPC)
      CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)
      IS = 1
      IF (ISLE.GT.0) IS = -1
      IF (KLOK.EQ.IS.AND.ABS(AREA).Ge.rad*rad*.25) THEN
        if (lanep.le.100) then
c... check if step-over is small enough - if NOT redo
          r = rad-tol
          if (sc(169).lt.8.229999.and.ingos.gt.0 .and. lup.gt.1 .and.
     x        .not.(r.lt.stpmin .or. r.lt..5*stpmax)) then
c... loop/lane is breaking up, insure minimum step over
            rad = .5*stpmax
            if (rad.lt.stpmin) rad = stpmin
            goto 10
          endif
          call xsharp (xc, yc, nppc, stpmax, stpcur)
          if (stpcur.lt.r .and. lup.gt.1) then
            rad = stpcur
            goto 10
          endif
          if (isle.eq.0 .and. klok*iside.eq.-1 .and. .not.lv93) then
            if (area/hldare.gt.2) goto 40
            if (.not.lv94) then
c
c..... added another check if this is the second attempt to offset this
c..... loop, with the half offset parameter - qar 95170
c
              call xross (XA,YA,NPPA,xc,yc,nppc,icen,ierr)
              if (ierr.gt.0) then
                ierr = 0
                goto 40
              endif
c
c.... if xross changed the contour repeat the testing
c
              if (icen .eq. 1) then
                goto 11
              endif

              if (nppa.lt.8 .and. area.lt.0.8*rad*rad) then
                call xross1 (XA,YA,NPPA,xc,yc,nppc,rad,icen)
c
c.... if xross1 changed the contour repeat the testing
c
                if (icen .eq. 1) then
                  goto 11
                endif
              endif
            endif
          endif
        else if (isle*iside.eq.-1 .and. area/hldare.lt.0.5) then
c
c..... Qar 94122. An island outside offset should not be less than the island.
c..... When the current offset contour is too small, try to find a better one.
c
          if (lv93) goto 30
   20     ING = ING+1
          IF (ING.GT.INGOS) GOTO 95

          NPPC = 0
          MM1 = INGO(ING)
          MM2 = INTO(ING)
          XMM = XING(ING)
          YMM = YING(ING)
          CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     +                    XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,
     x                    XB,YB,XC,YC,NPPC,IERR)
          IF (NPPC.GT.maxpts) THEN
            IERR = 11
            GOTO 99
          ENDIF
          IF (IERR.EQ.11) GOTO 99
          CALL DOUBLE (XC,YC,NPPC)

          IF (LUP.GT.0) CALL LITARC (XC,YC,NPPC)
          CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)
          IF (KLOK.EQ.IS.AND.ABS(AREA).Ge.rad*rad*.25 .and.
     x                                  area/hldare.gt.0.5) goto 30
          goto 20
        endif
   30   NEWLUP = 1
C--- STORE MAIN LOOP
        CALL STOLAP (LUP,LANEP,MOM,XC,YC,NPPC,IERR)
        IF (IERR.EQ.10) GOTO 99
        INDEX2 = INDEX2+1
        IADD = 1
      ENDIF
      IF (INGOS.EQ.0.OR.ISLE.GT.0) GOTO 95
      ING = 0
   40 ING = ING+1
      IF (ING.GT.INGOS) GOTO 95
      NPPC = 0
      MM1 = INGO(ING)
      MM2 = INTO(ING)
      XMM = XING(ING)
      YMM = YING(ING)
      CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     + XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IERR)
      IF (NPPC.GT.maxpts) THEN
        IERR = 11
        GOTO 99
      ENDIF
      IF (IERR.EQ.11) GOTO 99
 41   CALL DOUBLE (XC,YC,NPPC)

C     CHANGE TO LUP.GT.1 WITHOUT GPR ********************************
      IF (LUP.GT.0) CALL LITARC (XC,YC,NPPC)
      CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)
      IF (KLOK.EQ.-1.OR.ABS(AREA).LT.rad*rad*.25) GO TO 40
      IF (AREA.GT.HLDARE) GO TO 40
      IF (IADD.EQ.0) THEN
        LANN = LANEP
      ELSE
        LANEX = LANEX+1
        LANN = LANEX
      ENDIF
      if (lann.le.100) then
c... check if step-over is small enough - if NOT redo
        r = rad-tol
        if (sc(169).lt.8.229999 .and. ingos.gt.0 .and. lup.gt.1 .and.
     x      .not.(r.lt.stpmin .or. r.lt..5*stpmax)) then
c... loop/lane is breaking up insure minimum step over
          rad = .5*stpmax
          if (rad.lt.stpmin) rad = stpmin
          goto 10
        endif
        call xsharp (xc, yc, nppc, stpmax, stpcur)
        if (stpcur.lt.r .and. lup.gt.1) then
          rad = stpcur
          if (sc(169) .ge. 9.3) then
c..... FSR 60746 fix - the same area was cut several times with different steps
            iadd = 0
            lanex = lanex0
            do i=index1,lanes
              loop(i)=0
            enddo
          endif
          goto 10
        endif
      endif

      if (isle.eq.0 .and. klok*iside.eq.-1 .and. .not.lv93) then
        call xross (XA,YA,NPPA,xc,yc,nppc,icen,ierr)
        if (ierr.gt.0) then
          ierr = 0
          goto 40
        endif
c
c.... if xross changed the contour repeat the testing
c
        if (icen .eq. 1) then
          goto 41
        endif
      endif

      NEWLUP = 1
C--- STORE SECONDARY LOOPS
      CALL STOLAP (LUP,LANN,MOM,XC,YC,NPPC,IERR)
      IF (IERR.EQ.10) GOTO 99
      INDEX2 = INDEX2+1
      IADD = 1
      GOTO 40
   95 IF (NEWLUP.EQ.0) IERR = 1

C --- FOR END POINT REARRANGE INDICES
      IF (LOOK.EQ.1) CALL ISLOOK (INDEX1,INDEX2)

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE ONEOFF1
C **  purpose of subroutine: COMPUTES OFFSET FOR RADIUS RAD
C **    INPUT:
c **      XA,YA.....INPUT STRING ARRAYS
C **      NPPA......NO OF ELEMENTS IN INPUT ARRAY
C **      ISLE......0- POCKET, 1- ISLAND
C **      KIVUN.....1/-1 DIRECTION FORWARD/BACKWARD
C **      kside.....1/-1 SIDE RIGHT/LEFT
C **      RADIUS....current step-over
C **      MOM.......INDEX OF MOTHER LOOP
C **      LUP.......CURRENT LOOP
C **      LANEP.....PRESENT LANE
C **      LANEX.....MAX LANE NUMBER
C **    OUTPUT:
C **      LANEX.....MAX LANE NUMBER
C **      IERR......0- OK, new lane/loop stored
c **                1- OK, NO new lane/loop stored
c **                9- ERROR IN OFFSET END
c **********************************************************************
c **********************************************************************

      SUBROUTINE oneoff1 (XA,YA,NPPA,iside,rad,hldare,
     x                    MOM,LUP,LANEP,LANEX,lastlup,newlup,ierr)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      real*4 XC(maxpts),YC(maxpts)
      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)
      integer*4 INGO(mxingos),INTO(mxingos)
      real*4 XING(mxingos),YING(mxingos)
      real*4 hldare,area,xmm,ymm,rad,r,shalf
      real*4 delrad
      integer*2 lastlup,irad,nrad
      integer*2 nppa,nppb,nppc,iadd,isle,klosed,newlup,index1,is
      integer*2 lseg,klok,lann,mom,lup,lanep,lanex
      integer*2 ierr,iside,noboxes,ingos,ing,mm1,mm2
      logical lcovergap,lhavepola

      IERR = 0
      IADD = 0
      KLOSED = 1
      LSEG = 1
      isle = 0
      IS = 1
      NEWLUP = 0
      XMM = 0.0
      YMM = 0.0
      INDEX1 = LANES+1

      lanex0 = lanex
      nrad = 0
      irad = 0
      delrad = 0.
      lcovergap = .false.
      lhavepola = .false.

      shalf = 0.5*stpmax

      if (lastlup.eq.0 .and. rad .ge. (shalf + 5.*tol)) then
        nrad = (rad - shalf)/(5.*tol)
        if (nrad .gt. 8) nrad = 8
        delrad = (rad - shalf)/nrad
      endif

      if (lastlup .gt. 0) then
        call createpola (xa,ya,nppa,iside,hldare,lhavepola)
      endif

C---  COMPUTES FIRST OFFSET
10    CALL OFF1ST (KLOSED,ISIDE,ISLE,RAD,NPPA,XA,YA,XB,YB,NPPB,IERR)
      IF (IERR.ge.1) GOTO 99
      CALL DOUBLE (XB,YB,NPPB)

C     CHANGE TO LUP.GT.1 WITHOUT GPR ********************************
      IF (LUP.GT.0) CALL LITARC (XB,YB,NPPB)
      IF (NPPB.LT.3) THEN
        IERR = 1
        GOTO 99
      ENDIF

      radsq = rad*rad
      aremin = 0.25*radsq
      if (lcovergap .or. (lastlup.gt.0 .and. lhavepola)) then
        aremin = 25*tol*tol
      endif
      r = rad-tol

C---  REMOVES LOOPS FROM FIRST OFFSET TO CREATE TRUE OFFSET
      CALL BOXES(NPPB,XB,YB,XL1,YL1,XL2,YL2,LLT,NOBOXES)
      INGOS = 0
      ING = 0
      MM1 = 1
      MM2 = NPPB-1
      XMM = XB(1)
      YMM = YB(1)
      CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     +  XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IERR)
      IF (NPPC.GT.maxpts) IERR = 11
      IF (IERR.EQ.11) GOTO 99

 11   CALL DOUBLE (XC,YC,NPPC)
      CALL LITARC (XC,YC,NPPC)
      CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)

      IF (KLOK.EQ.IS .AND. ABS(AREA).ge.aremin) THEN
c... check if step-over is small enough - if NOT redo
        if (lanep.le.100) then
          if (.not.lcovergap) call xsharp (xc, yc, nppc, stpmax, stpcur)
          if (stpcur.lt.r .and. irad.lt.nrad) then
            irad = irad + 1
            rad = rad - delrad
            if (rad .lt. stpcur) rad = stpcur
            if (rad .lt. stpmin) then
              rad = stpmin
              irad = nrad
            endif
            stpcur = rad
            goto 10
          endif

          if (area/hldare.gt.2) goto 40
c
c..... added another check if this is the second attempt to offset this
c..... loop, with the half offset parameter - qar 95170
c
          call xross (XA,YA,NPPA,xc,yc,nppc,lchg,ierr)
          if (ierr .gt. 0) then
            ierr = 0
            goto 40
          endif
c
c.... if xross changed the contour repeat the testing
c
          if (lchg .eq. 1) goto 11
          if (nppa.lt.8 .and. area.lt.0.8*radsq) then
            call xross1 (XA,YA,NPPA,xc,yc,nppc,rad,lchg)
c
c.... if xross1 changed the contour repeat the testing
c
            if (lchg .eq. 1) goto 11
          endif
        endif

        NEWLUP = 1
C--- STORE MAIN LOOP
        CALL STOLAP (LUP,LANEP,MOM,XC,YC,NPPC,IERR)
        IF (IERR.EQ.10) GOTO 99
        IADD = 1
      ENDIF

      IF (INGOS.EQ.0) GOTO 99

      ING = 0
   40 ING = ING+1
      IF (ING.GT.INGOS) GOTO 98
      NPPC = 0
      MM1 = INGO(ING)
      MM2 = INTO(ING)
      XMM = XING(ING)
      YMM = YING(ING)
      CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     + XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IERR)
      IF (NPPC.GT.maxpts) IERR = 11
      IF (IERR.EQ.11) GOTO 99

 41   CALL DOUBLE (XC,YC,NPPC)
      CALL LITARC (XC,YC,NPPC)
      CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)

      IF (KLOK.EQ.-1.OR.ABS(AREA).LT.aremin) GOTO 40
      IF (AREA.GT.HLDARE) GOTO 40
      IF (IADD.EQ.0) THEN
        LANN = LANEP
      ELSE
        LANEX = LANEX+1
        LANN = LANEX
      ENDIF

      if (lann.le.100) then
c... check if step-over is small enough - if NOT redo
        if (.not.lcovergap) call xsharp (xc, yc, nppc, stpmax, stpcur)
        if (stpcur .lt. r .and. irad.lt.nrad) then

          irad = irad + 1
          rad = rad - delrad
          if (rad .lt. stpcur) rad = stpcur
          if (rad .lt. stpmin) then
            rad = stpmin
            irad = nrad
          endif
          stpcur = rad
c
c..... FSR 60746 fix - the same area was cut several times with different steps
c
          iadd = 0
          newlup = 0
          lanex = lanex0
          do i = index1, lanes
            loop(i) = 0
            lane(i) = 0
            mama(i) = 0
            loca(i+1) = 0
          enddo
          lanes = index1 - 1
          goto 10

        endif
      endif

      call xross (XA,YA,NPPA,xc,yc,nppc,lchg,ierr)
      if (ierr.gt.0) then
        ierr = 0
        goto 40
      endif
c
c.... if xross changed the contour repeat the testing
c
      if (lchg .eq. 1) goto 41

      NEWLUP = 1
C--- STORE SECONDARY LOOPS
      CALL STOLAP (LUP,LANN,MOM,XC,YC,NPPC,IERR)
      IF (IERR.EQ.10) GOTO 99
      IADD = 1
      GOTO 40

 98   continue

      if (ierr.eq.0 .and. iadd.eq.1) then
        if (irad .ge.nrad .and. .not.lcovergap .and. rad.ge.shalf) then
          nrad = 1
          irad = 0
          delrad = rad - shalf
        endif

        if (irad .ge. nrad) goto 99

        if (.not.lhavepola) then
          call createpola (xa,ya,nppa,iside,hldare,lhavepola)
        endif

        lcovergap = .false.
        if (lhavepola) call chkcover (index1,lcovergap)

        if (lcovergap) then
          irad = irad + 1
          rad = rad - delrad
          if (rad .lt. stpmin) then
            rad = stpmin
            irad = nrad
          endif
          iadd = 0
          newlup = 0
          lanex = lanex0
          do i = index1, lanes
            loop(i) = 0
            lane(i) = 0
            mama(i) = 0
            loca(i+1) = 0
          enddo
          lanes = index1 - 1
          goto 10
        endif
      endif

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE offisle
C **  purpose of subroutine: calculate a good contour from a off1st loop for an
C **                         outside offset of in island
C **    INPUT:
C **      KLOSED...........1/0  STRING CLOSED/OPEN
C **      XB,YB............INPUT STRING
C **      NPPB.............NUMBER OF ELEMENTS IN INPUT STRING
C **    current boxes data:
C **    {
C **      NOBOXES..........NUMBER OF BOXES
C **      LLT..............ARRAY OF BOX ALLOCATION NUMBERS
C **      XL1,YL1,XL2,YL2..BOX ARRAYS
C **    }
C **      MOM.......INDEX OF MOTHER LOOP
C **      LUP.......CURRENT LOOP
C **      LANEP.....PRESENT LANE
C **      hldare....original contour area
C **      rad   ....offset distance
C **    OUTPUT:
C **      newlup....1 if new loop is stored, else 0
C **      ierr......error number
c **********************************************************************
c **********************************************************************

      SUBROUTINE offisle (KLOSED,XB,YB,NPPB,XL1,YL1,XL2,YL2,LLT,NOBOXES,
     x                    LUP,LANEP,MOM,hldare,rad,newlup,ierr)

      include 'com.com'
      include 'pokcom.com'
      
      real*4 XB(maxpts),YB(maxpts)
      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      real*4 hldare,rad
      integer*2 LUP,LANEP,MOM,KLOSED,NOBOXES,NPPB
      integer*2 ierr,newlup
      integer*4 LLT(30)
      
      real*4 xc(maxpts),yc(maxpts)
      real*4 area,xmm,ymm,aremin
      integer*2 INGOS,ING
      integer*2 nppc,mm1,mm2,klok,is
      integer*4 INGO(mxingos),INTO(mxingos)
      real*4 XING(mxingos),YING(mxingos)
            
      aremin = rad*rad*.25

      IS = -1
       
      INGOS = 0
      ING = 0
      MM1 = 1
      MM2 = NPPB-1
      XMM = XB(1)
      YMM = YB(1)
      
   20 CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     +  XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IERR)
      IF (NPPC.GT.maxpts) IERR = 11
       
      IF (IERR.EQ.11) GOTO 99
      CALL DOUBLE (XC,YC,NPPC)

      IF (LUP.GT.0) CALL LITARC (XC,YC,NPPC)
      CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)

      IF (KLOK.EQ.IS .AND. ABS(AREA).ge.aremin .and.
     x                            area/hldare.ge.0.5) then

        NEWLUP = 1
C--- STORE LOOP and exit
        CALL STOLAP (LUP,LANEP,MOM,XC,YC,NPPC,IERR)
      else  
        ING = ING+1
        IF (ING.GT.INGOS) GOTO 99
        NPPC = 0
        MM1 = INGO(ING)
        MM2 = INTO(ING)
        XMM = XING(ING)
        YMM = YING(ING)
        goto 20
      endif
      
   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE evolvlp
c **  purpose of subroutine: evolve a loop into a polyline (break arcs)
c **    input:
c **      xa,ya,nppa...lane/loop array and size
c **    output:
c **      xb,yb,nppb...lane/loop array and size
c **      ierr.........0- ok 1- over maximum points
c **********************************************************************
c **********************************************************************

      SUBROUTINE evolvlp (xa,ya,nppa,xb,yb,nppb,ierr)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 xa(maxpts), ya(maxpts), xb(maxpts), yb(maxpts)
      real*4 xt(30), yt(30)
      integer*2 nppa,nppb,k,i,npt,ierr

      nppb = 1
      xb(nppb) = xa(1)
      yb(nppb) = ya(1)
      ierr = 0

      k = 0
15    k=k+1
      if (k.ge.nppa) goto 30
      if (xa(k+1).eq.flarc) then
        r = xa(k+2)
        klok = ya(k+2)
        xc = xa(k+3)
        yc = ya(k+3)
        x1 = xa(k)
        y1 = ya(k)
        x2 = xa(k+4)
        y2 = ya(k+4)
        call brkarc (r,klok,xc,yc,x1,y1,x2,y2,xt,yt,npt)
        do 20 i=1,npt
          if (i .gt. 1) then
            nppb = nppb + 1
            if (nppb.gt.maxpts) then
              ierr = 1
              goto 99
            endif
            xb(nppb) = xt(i)
            yb(nppb) = yt(i)
          endif
20      continue
        k = k+3
      else
        nppb=nppb+1
        if (nppb.gt.maxpts) then
          ierr = 1
          goto 99
        endif
        xb(nppb) = xa(k+1)
        yb(nppb) = ya(k+1)
      endif
      goto 15
30    if (xb(1).ne.xb(nppb) .or. yb(1).ne.yb(nppb)) then
        nppb = nppb + 1
        if (nppb.gt.maxpts) then
          ierr = 1
          goto 99
        endif
        xb(nppb) = xb(1)
        yb(nppb) = yb(1)
      endif

99    return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE cornat1(xa,ya,nppa,lcorn)
C **  purpose of subroutine: DETERMINE if a CLOSED LOOP starts at a corner.
c **    input:
C **      XA,YA...CLOSED STRING
C **      NPPA....NO OF STRING POINTS
C **    output:
C **      lcorn.... true/false
c **********************************************************************
c **********************************************************************

      SUBROUTINE cornat1(xa,ya,nppa,lcorn)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa
      logical lcorn

      real*4 co,dx,dy,dx1,dy1,sec
      integer*2 i, k

      lcorn = .false.
      if (nppa.lt.4) goto 99
c
c... Get start vector
c
      if (xa(2).eq.FLARC) then
        call tanarc (1,xa(4),ya(4),xa(3),ya(3),xa(1),ya(1),xa(5),ya(5),
     x               dx1,dy1)
      else
        dx1 = xa(2)-xa(1)
        dy1 = ya(2)-ya(1)
      endif
      sec = sqrt(dx1**2+dy1**2)
      if (sec.gt.0.0) then
        dx1 = dx1/sec
        dy1 = dy1/sec
      endif
c
c... Get end vector
c
      i = nppa-4
      if (xa(i+1).eq.FLARC) then
        k = ya(i+2)

        call tanarc (2,xa(i+3),ya(i+3),xa(i+2),k,xa(i),ya(i),
     x               xa(i+4),ya(i+4),dx,dy)
      else
        i = nppa - 1
        dx = xa(i+1)-xa(i)
        dy = ya(i+1)-ya(i)
      endif
      sec = sqrt(dx**2+dy**2)
      if (sec.gt.0.0) then
        dx = dx/sec
        dy = dy/sec
      endif
c
c... Look at the angle between last element & first element.
c
      co = dx1*dx+dy1*dy
      if (co .lt. 0.866) lcorn = .true.

   99 return
      end

c **********************************************************************
c **  SUBROUTINE createpola
C **  offset a contour inside and add to polygon A
c **********************************************************************
c **********************************************************************

      SUBROUTINE createpola (xa,ya,nppa,iside,hldare,lcreated)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts),xaa(maxpts),yaa(maxpts)
      real*4 xmm,ymm,useg,hldare
      integer*2 nppa,npaa,lseg,icen,iside
      logical lcreated

      LSEG = 1
      XMM = 0.0
      YMM = 0.0

      call cpylap (xa,ya,nppa,xaa,yaa,npaa)
      CALL LARGEST (xaa,yaa,npaa,LSEG)
      ICEN = 1
      USEG = 0.5
      CALL SHUFFLE (xaa,yaa,npaa,LSEG,ICEN,USEG,XMM,YMM)
C... remove like points
      CALL DOUBLE (xaa,yaa,npaa)
C... remove in-line points, arcs and overlap
      CALL STRAIT (xaa,yaa,npaa)

      call createpola1 (xaa,yaa,npaa,iside,hldare,lcreated)

      RETURN
      END

c **********************************************************************
c **  SUBROUTINE createpola1
C **  offset a contour inside and add to polygon A
c **********************************************************************
c **********************************************************************

      SUBROUTINE createpola1 (xa,ya,nppa,iside,hldare,lcreated)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      real*4 XC(maxpts),YC(maxpts)
      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)
      integer*4 INGO(mxingos),INTO(mxingos)
      real*4 XING(mxingos),YING(mxingos)
      real*4 HLDARE,area,xmm,ymm,rad,radsq
      integer*2 nppa,nppb,nppc,iadd,isle,klosed,is
      integer*2 klok
      integer*2 ierr,iside,noboxes,ingos,ing,mm1,mm2,larcsv
      logical lcreated

      call rstpokpol (0)
      lcreated = .false.
      rad = stpcov
      radsq = rad*rad

      aremin = 25*tol*tol

      IERR = 0
      IADD = 0
      KLOSED = 1
      isle = 0
      IS = 1
      nc = 0
      XMM = 0.0
      YMM = 0.0

      larcsv = larcs
      larcs = 1
C---  COMPUTES FIRST OFFSET
      CALL OFF1ST (KLOSED,ISIDE,ISLE,RAD,NPPA,XA,YA,XB,YB,NPPB,IERR)
      larcs = larcsv
      IF (IERR.ge.1) GOTO 99
      CALL DOUBLE (XB,YB,NPPB)

      CALL LITARC (XB,YB,NPPB)
      IF (NPPB.LT.5) GOTO 99

C---  REMOVES LOOPS FROM FIRST OFFSET TO CREATE TRUE OFFSET
      CALL BOXES(NPPB,XB,YB,XL1,YL1,XL2,YL2,LLT,NOBOXES)
      INGOS = 0
      ING = 0
      MM1 = 1
      MM2 = NPPB-1
      XMM = XB(1)
      YMM = YB(1)
      CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     +  XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IERR)
      IF (NPPC.GT.maxpts) IERR = 11
      IF (IERR.EQ.11) GOTO 99

 11   CALL DOUBLE (XC,YC,NPPC)
      CALL LITARC (XC,YC,NPPC)
      CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)

      IF (KLOK.EQ.IS .AND. ABS(AREA).ge.aremin) THEN

          if (area/hldare.gt.2) goto 40
c
c..... added another check if this is the second attempt to offset this
c..... loop, with the half offset parameter - qar 95170
c
          call xross (XA,YA,NPPA,xc,yc,nppc,lchg,ierr)
          if (ierr .gt. 0) then
            ierr = 0
            goto 40
          endif
c
c.... if xross changed the contour repeat the testing
c
          if (lchg .eq. 1) goto 11
          if (nppa.lt.8 .and. area.lt.0.8*radsq) then
            call xross1 (XA,YA,NPPA,xc,yc,nppc,rad,lchg)
c
c.... if xross1 changed the contour repeat the testing
c
            if (lchg .eq. 1) goto 11
          endif

C--- STORE MAIN LOOP
        lcreated = .true.
        call addpola (XC,YC,NPPC)
      ENDIF

      IF (INGOS.EQ.0) GOTO 99

      ING = 0
   40 ING = ING+1
      IF (ING.GT.INGOS) GOTO 99
      NPPC = 0
      MM1 = INGO(ING)
      MM2 = INTO(ING)
      XMM = XING(ING)
      YMM = YING(ING)
      CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     + XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IERR)
      IF (NPPC.GT.maxpts) IERR = 11
      IF (IERR.EQ.11) GOTO 99

 41   CALL DOUBLE (XC,YC,NPPC)
      CALL LITARC (XC,YC,NPPC)
      CALL XCLOCK (XC,YC,NPPC,AREA,KLOK)

      IF (KLOK.EQ.-1.OR.ABS(AREA).LT.aremin) GOTO 40
      IF (AREA.GT.HLDARE) GOTO 40

      call xross (XA,YA,NPPA,xc,yc,nppc,lchg,ierr)
      if (ierr.gt.0) then
        ierr = 0
        goto 40
      endif
c
c.... if xross changed the contour repeat the testing
c
      if (lchg .eq. 1) goto 41

C--- STORE SECONDARY LOOPS
      lcreated = .true.
      call addpola (XC,YC,NPPC)
      GOTO 40

   99 if (ierr.gt.0) lcreated = .false.
      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE addpola
C **  evolve contour arcs and add to polygon A
c **********************************************************************
c **********************************************************************

      SUBROUTINE addpola (xc,yc,nppc)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)
      real*4 XC(maxpts),YC(maxpts)

      integer*2 nppa,ierr,nppc

      ierr = 0
      call evolvlp (xc,yc,nppc,xa,ya,nppa,ierr)
      if (ierr .ne. 0) return

      call addpokpol (0,xa,ya,nppa)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE addpolb
C **  offset a contour outside and add to polygon B
c **********************************************************************
c **********************************************************************

      SUBROUTINE addpolb (xb,yb,nppb,is,ladded)

      include 'pokcom.com'

      real*4 XB(maxpts),YB(maxpts)
      integer*2 nppb,is
      logical ladded

      real*4 XA(maxpts),YA(maxpts)
      real*4 XC(maxpts),YC(maxpts)
      real*4 rad
      integer*2 nppa,nppc,ier,larcsv
      logical lxross

      ladded = .false.
      lxross = .false.

      rad = stpcov

      larcsv = larcs
      larcs = 1

      if (is.eq.0) call flipo (xb,yb,nppb)
      call offslp (lxross,xb,yb,nppb,xc,yc,nppc,-rad,ier)
      larcs = larcsv
      if (ier .ne. 0) return

      call evolvlp (xc,yc,nppc,xa,ya,nppa,ier)
      if (ier .ne. 0) return

      ladded = .true.
      call addpokpol (1,xa,ya,nppa)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE chkcover
c **  check if there is a coverage gap by offsetting contours inside
c **  the current lane to form polygon B, and subtracting it from
c **  polygon A.
c **    input:
c **      index1   -  number of the first new lane
c **    output:
c **      lgap     - true iff there is a gap
c **********************************************************************
c **********************************************************************

      SUBROUTINE chkcover (index1,lgap)

      include 'pokcom.com'

      integer*2 index1
      logical lgap

      real*4 XA(maxpts),YA(maxpts)
      real*4 aremin
      integer*2 i,nc,nppa
      logical ladded

      lgap = .false.

      call rstpokpol (1)

      do i = index1,lanes
        call getlan (i,xa,ya,nppa)
        call addpolb (xa,ya,nppa,0,ladded)
        if (.not.ladded) return
      enddo

      aremin = 25*tol*tol

      call subpokpols (aremin,nc)

      if (nc .gt. 0) lgap = .true.

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **     SUBROUTINE xross
c **  purpose of subroutine: remove points of loop B not inside loop A
c **    input:
c **      XA,YA,NPPA...POCKET LANE/LOOP
c **      XB,YB,NPPB...ISLAND LANE/LOOP
c **    output:
c **      ichg........ 1 iff loop B changed
c **********************************************************************
c **********************************************************************

      SUBROUTINE xross (XA,YA,NPPA,XB,YB,NPPB,ichg,ierr)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      integer*2 nppa,nppb,ichg,ierr

      real*4 xaa(maxpts), yaa(maxpts)
      integer*2 j,k,karc0,karc1,karc,jdel,npaa

      ichg = 0
      call evolvlp (xa,ya,nppa,xaa,yaa,npaa,ierr)
      if (ierr .ne. 0) return

      k = 0
10    k = k+1

      karc0 = 0
      if (k .gt. 3 .and. xa (k-3) .eq. flarc) karc0 = 1
      karc1 = 0
      if (xb(k+1) .eq. flarc) karc1 = 1
c
c..... remove points on contour B which are not inside contour A
c..... (this routine is only called for offsets inside)
c
      call inlup1 (xb(k),yb(k),xaa,yaa,npaa,in)

      if (in .ne. 1) then
        ichg = 1
        if (karc1 .eq. 1) k = k+3
        karc = karc0+karc1
        jdel = 0
        if (karc .eq. 0) then
          jdel = 1
        else if (karc .eq. 1) then
          jdel = 4
        else if (karc .eq. 2) then
          jdel = 7
        endif

        do j = k+1,nppb
          xb(j-jdel) = xb(j)
          yb(j-jdel) = yb(j)
        enddo

        k = k-jdel
        nppb = nppb-jdel
      else
        if (karc1 .eq. 1) k = k+3
      endif

      if (k .lt. nppb-1) goto 10

      return
      end

c **********************************************************************
c *********************************************************************
c **  SUBROUTINE dsptlap
C **  purpose of subroutine: FIND CLOSEST POINT ON CONTOUR TO A POINT
c **    input:
C **      XL,YL........POINT
C **      XA,YA,NPPA...contour
C **    output:
C **      DIST.........DISTANCE OF CLOSEST POINT
c **********************************************************************
c **********************************************************************

      SUBROUTINE dsptlap (XL,YL,XA,YA,NPPA,LSEG,UEG,XN,YN,DIST)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XL,YL,UEG,XN,YN,DIST
      integer*2 NPPA,LSEG

      real*4 dx,dy,dist1,r,XN1,YN1,U
      integer*2 ierr,index,k,klok

      IERR = 0
      LSEG = 0
      UEG = 0.0
      XN = 0.0
      YN = 0.0
      INDEX = 1
      DIST = 10000000.0
      K = 0
   21 K = K+1
      IF (K.GT.NPPA) GO TO 99
      DX = XL-XA(K)
      DY = YL-YA(K)
      DIST1 = DX*DX+DY*DY
      DIST1 = SQRT(DIST1)
      IF (DIST1.LT.DIST) THEN
        DIST = DIST1
        LSEG = K
        UEG = 0.0
        XN = XA(K)
        YN = YA(K)
      END IF
      IF (K.GE.NPPA) GO TO 99
      IF (XA(K+1).EQ.FLARC) THEN
        R = XA(K+2)
        KLOK = YA(K+2)
        CALL DSPTARC (XL,YL,R,KLOK,XA(K+3),YA(K+3),
     +        XA(K),YA(K),XA(K+4),YA(K+4),DIST1,XN1,YN1,U)
        IF (U.GT.0.0.AND.U.LT.1.0.AND.DIST1.LT.DIST) THEN
          DIST = DIST1
          LSEG = K
          UEG = U
          XN = XN1
          YN = YN1
        END IF
        K = K+3
      ELSE
        CALL DSPTLN (Xl,Yl,XA(K),YA(K),XA(K+1),YA(K+1),
     +        DIST1,XN1,YN1)
        CALL USEG (XA(K),YA(K),XA(K+1),YA(K+1),XN1,YN1,U)
        IF (U.GT.0.0.AND.U.LT.1.0.AND.DIST1.LT.DIST) THEN
          DIST = DIST1
          LSEG = K
          UEG = U
          XN = XN1
          YN = YN1
        END IF
      END IF
      GO TO 21

   99 RETURN
      END

c **********************************************************************
c *********************************************************************
c **  SUBROUTINE dsptlp0
C **  purpose of subroutine: find distance between a point and a contour.
c **    input:
C **      XL,YL........POINT
C **      XA,YA,NPPA...contour
C **    output:
C **      DIST.........DISTANCE OF CLOSEST POINT
c **********************************************************************
c **********************************************************************

      SUBROUTINE dsptlp0 (XL,YL,XA,YA,NPPA,DIST)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XL,YL,DIST
      integer*2 NPPA

      real*4 UEG,XN,YN
      integer*2 LSEG

      call dsptlap (XL,YL,XA,YA,NPPA,LSEG,UEG,XN,YN,DIST)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE xross1
c **  purpose of subroutine: remove points of loop B too close to loop A
c **    input:
c **      XA,YA,NPPA...POCKET LANE/LOOP
c **      XB,YB,NPPB...ISLAND LANE/LOOP
c **    output:
c **      ichg........ 1 iff loop B changed
c **********************************************************************
c **********************************************************************

      SUBROUTINE xross1 (XA,YA,NPPA,XB,YB,NPPB,rad,ichg)

      include 'pokcom.com'

      real*4 rad
      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      integer*2 nppa,nppb,ichg

      real*4 dmin,dis
      integer*2 j,k,karc0,karc1,karc,jdel

      ichg = 0
      dmin = rad - 4*tol

      k = 0
10    k = k+1

      karc0 = 0
      if (k .gt. 3 .and. xa (k-3) .eq. flarc) karc0 = 1
      karc1 = 0
      if (xb(k+1) .eq. flarc) karc1 = 1
c
c..... remove points on contour B which are at less than minimal
c..... distance from contour A
c..... (this routine is only called for offsets inside)
c
      call dsptlp0 (xb(k),yb(k),xa,ya,nppa,dis)

      if (dis .lt. dmin) then
        ichg = 1
        if (karc1 .eq. 1) k = k+3
        karc = karc0+karc1
        jdel = 0
        if (karc .eq. 0) then
          jdel = 1
        else if (karc .eq. 1) then
          jdel = 4
        else if (karc .eq. 2) then
          jdel = 7
        endif

        do j = k+1,nppb
          xb(j-jdel) = xb(j)
          yb(j-jdel) = yb(j)
        enddo

        k = k-jdel
        nppb = nppb-jdel
      else
        if (karc1 .eq. 1) k = k+3
      endif

      if (k .lt. nppb-1) goto 10

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE xross2
c **  purpose of subroutine: offset loop B by a small distance (inside or
c **  or outside) so that it does not overlap loop A
c **    input:
c **      XA,YA,NPPA...POCKET LANE/LOOP
c **      XB,YB,NPPB...ISLAND LANE/LOOP
c **    output:
c **      lchged...... true loop B changed
c **********************************************************************
c **********************************************************************

      SUBROUTINE xross2 (XA,YA,NPPA,XB,YB,NPPB,lchged)

      include 'pokcom.com'

      real*4 rad
      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      integer*2 nppa,nppb
      logical lchged

      real*4 xc(maxpts),yc(maxpts)
      integer*2 nppc,itim,j,ier
      logical lclose,lxross

      call overlaps (XA,YA,NPPA,XB,YB,NPPB,lclose)
      if (.not.lclose) return

      itim = 0
      rad = 0.8*tol
      pm = 1
      lxross = .true.

5     itim = itim+1
      pm = -pm
      j = (itim+1)/2

      r = j*pm*rad

      call offslp (lxross,xb,yb,nppb,xc,yc,nppc,r,ier)
      if (ier .gt. 0) return

      call overlaps (XA,YA,NPPA,xc,yc,nppc,lclose)

      if (lclose .and. itim.lt.10) goto 5

      if (.not.lclose) then
        call cpylap (xc,yc,nppc,xb,yb,nppb)
        lchged = .true.
      endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE smloffs
c **  purpose of subroutine: create a small offset of a contour
c **    input:
c **      itim........ counter - determines offset direction and distance
c **      XB,YB,NPPB...contour to offset
c **    output:
c **      XB,YB,NPPB...offset contour
c **      lchged...... true loop B changed
c **      ier......... >0 iff error
c **********************************************************************
c **********************************************************************

      SUBROUTINE smloffs (itim,XB,YB,NPPB,lchged,ier)
      include 'pokcom.com'

      real*4 rad,pm
      real*4 XB(maxpts),YB(maxpts)
      integer*2 itim,nppb,ier
      logical lchged

      real*4 xc(maxpts),yc(maxpts)
      integer*2 nppc,j
      logical lxross

      lxross = .true.
      rad = 0.8*tol
      pm = 1
      j = (itim+1)/2
      if (2*j .eq. itim) pm = -1

      r = j*pm*rad
      call offslp (lxross,xb,yb,nppb,xc,yc,nppc,r,ier)
      if (ier .gt. 0) return

      call cpylap (xc,yc,nppc,xb,yb,nppb)
      lchged = .true.

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE overlaps
c **  purpose of subroutine: determine if a line segment of loop B lies
c **                         on to loop A
c **    input:
c **      XA,YA,NPPA...POCKET LANE/LOOP
c **      XB,YB,NPPB...ISLAND LANE/LOOP
c **    output:
c **      lclose...... true iff loops overlap
c **********************************************************************
c **********************************************************************

      SUBROUTINE overlaps (XA,YA,NPPA,XB,YB,NPPB,lclose)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      integer*2 nppa,nppb
      logical lclose

      real*4 dmin,dis
      integer*2 k,karc1
      logical lpton,lpton0

      lclose = .false.
      dmin = tol/2.54

      lpton = .false.
      lpton0 = .false.
      nklos = 0

      k = 0
10    k = k+1
      karc1 = 0
      if (xb(k+1) .eq. flarc) karc1 = 1

      call dsptlp0 (xb(k),yb(k),xa,ya,nppa,dis)

      lpton = (dis .lt. dmin)

      if (lpton .and. lpton0) then
        nklos = nklos+1
      endif

      if (karc1 .eq. 1) then
        k = k+3
        lpton0 = .false.
      else
        lpton0 = lpton
      endif

      if (k .lt. nppb) goto 10

      lclose = (nklos .gt. 0)

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE fnpinlp
C **  purpose of subroutine: compute the number of elements in a contour,
C **  when each arc and each segment count as one element
C **    INPUT:
c **      XA,YA.....input array
C **      NPPA......number of elements in input array
C **    OUTPUT:
C **      fa......adjusted number of elements
c **********************************************************************
c **********************************************************************

      SUBROUTINE fnpinlp(xa,ya,nppa,fa)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts),fa
      integer*2 nppa

      integer*2 k,nrc

      nrc = 0
      k = 0
10    k = k+1
      if (xa(k+1) .eq. flarc) then
        k = k+3
        nrc = nrc + 1
      endif
      if (k .lt. nppa-1) goto 10

      fa = nppa - 4*nrc

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE offslp
C **  purpose of subroutine: COMPUTES OFFSET FOR RADIUS RAD
C **    INPUT:
c **      lxross....true iff called from xross2
c **      XA,YA.....input array
C **      NPPA......number of elements in input array
C **      RADIUS....offset parameter
C **    OUTPUT:
C **      IER......0 - valid offset computed, else error
c **********************************************************************
c **********************************************************************

      SUBROUTINE offslp (lxross,xp,yp,npp,xc,yc,nppc,radius,ier)

      include 'pokcom.com'

      real*4 xp(maxpts),yp(maxpts),xc(maxpts),yc(maxpts),radius
      integer*2 npp,nppc,ier
      logical lxross

      real*4 xa(maxpts),ya(maxpts)
      real*4 hldare,area,xmm,ymm,rad,tolsq,useg,fac,fa,fc,facnp,fmin
      real*8 ver
      integer*2 nppa,is,klok,iside,lseg,icen,idx

c
c..... copy the input contour and shuffle
c
      ier = 0
      rad = abs(radius)
      iside = 1
      klok = 0
      tolsq = tol*tol
      IF (radius .lt. 0) ISIDE = -1

      call cpylap (xp,yp,npp,xa,ya,nppa)

      CALL LARGEST (XA,YA,NPPA,LSEG)
      ICEN = 1
      USEG = 0.5
      CALL SHUFFLE (XA,YA,NPPA,LSEG,ICEN,USEG,XMM,YMM)

C... remove like points
      CALL DOUBLE (XA,YA,NPPA)
C... remove in-line points, arcs and overlap
      CALL STRAIT (XA,YA,NPPA)

      CALL XCLOCK (XA,YA,NPPA,HLDARE,IS)
      IF (nppa.lt.3 .or. abs(hldare).lt.tolsq) then
        ier = 1
        GOTO 99
      endif

      call offslp1 (iside,rad,xa,ya,nppa,xc,yc,nppc,klok,area,ier)
      if (ier .ne. 0) goto 99
c
c..... check if C is valid
c
      if (klok .eq. is) then
        fac = area/hldare
        call fnpinlp(xa,ya,nppa,fa)
        call fnpinlp(xc,yc,nppc,fc)
        facnp = 0
        if (fa .gt. 0) facnp = fc/fa
c
c.....Reduced the minimum value for facnp for FSR61570 - ASF 9/23/13
c
        idx = 169
        call getsc(idx,ver)
        if (ver.lt.9.949) then
            fmin = 0.8
        else
            fmin = 0.75
        endif
c
c..... if called from xross2 (lxross is true), the offset is small and can be
c..... positive or negative, so the area is expected not to change much.
c..... if called form addpolb (lxross is false), the offset is stpcov, so the
c..... area should increase
c
        if (lxross) then
          if (fac.lt.0.8 .or. fac.gt.1.25 .or.
     x       facnp.lt.0.8 .or. facnp.gt.1.25) ier = 1
        else
          if (fac.lt.0.9 .or.
     x       facnp.lt.fmin .or. facnp.gt.1.25) ier = 1
        endif
      else
        ier = 1
      endif

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE offslp1
C **  purpose of subroutine: COMPUTES OFFSET FOR RADIUS RAD
C **    INPUT:
c **      XA,YA.....INPUT STRING ARRAYS
C **      NPPA......NO OF ELEMENTS IN INPUT ARRAY
C **      RADIUS....current step-over
C **    OUTPUT:
C **      IER......0- OK, new lane/loop stored
c **********************************************************************
c **********************************************************************

      SUBROUTINE offslp1 (iside,rad,xa,ya,nppa,xc,yc,nppc,klok,area,ier)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts),xc(maxpts),yc(maxpts),rad,area
      integer*2 nppa,nppc,iside,ier,klok

      real*4 XB(maxpts),YB(maxpts)
      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)
      integer*4 INGO(mxingos),INTO(mxingos)
      real*4 XING(mxingos),YING(mxingos)
      real*4 xmm,ymm
      integer*2 nppb,klosed
      integer*2 isle
      integer*2 noboxes,ingos,ing,mm1,mm2

      IER = 0
      klok = 0
      isle = 1
      KLOSED = 1

      CALL OFF1ST (KLOSED,ISIDE,ISLE,RAD,NPPA,XA,YA,XB,YB,NPPB,IER)

      CALL DOUBLE (XB,YB,NPPB)

      IF (NPPB.LT.3) ier = 1
      IF (IER.GT.0) GOTO 99

C---  REMOVES LOOPS FROM FIRST OFFSET TO CREATE TRUE OFFSET
      CALL BOXES(NPPB,XB,YB,XL1,YL1,XL2,YL2,LLT,NOBOXES)
      INGOS = 0
      ING = 0
      MM1 = 1
      MM2 = NPPB-1
      XMM = XB(1)
      YMM = YB(1)
      CALL DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     +  XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IER)

      IF (NPPC.GT.maxpts) IER = 11

      IF (IER.GT.0) GOTO 99

      CALL DOUBLE (XC,YC,NPPC)

      CALL XCLOCK (XC,YC,NPPC,area,klok)


   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE cpylap
C **  purpose of subroutine: copy a contour
c **********************************************************************
c **********************************************************************

      subroutine cpylap (xa,ya,nppa,xc,yc,nppc)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),xc(maxpts),yc(maxpts)
      integer*2 nppa,nppc

      integer*2 k

        nppc = nppa
        do k = 1,nppa
          xc(k) = xa(k)
          yc(k) = ya(k)
        enddo

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE dslplp
c **  purpose of subroutine: calculate distance from loop A to loop B
c **  loop A does not have arcs.
c **    input:
c **      XA,YA,NPPA...POCKET LANE/LOOP
c **      XB,YB,NPPB...ISLAND LANE/LOOP
c **    output:
c **      dist
c **********************************************************************
c **********************************************************************

      SUBROUTINE dslplp (XA,YA,NPPA,XB,YB,NPPB,dist)

      include 'pokcom.com'

      real*4 dist
      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      integer*2 nppa,nppb

      real*4 dis
      integer*2 k

      dist = flarc

      do k = 1,nppa-1
        call dsptlp0 (xa(k),ya(k),xb,yb,nppb,dis)
        if (dis .lt. dist) dist = dis
      enddo

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE fostermom
c **  purpose of subroutine: find a mama for a contour indx. the current
c **  choice is imom, it is changed if a closer contour is found with
c **  the same loop number
c **    input:
c **      indx, imom
c **      lup - imom loop number
c **    output:
c **      imom - maybe changed
c **********************************************************************
c **********************************************************************

      SUBROUTINE fostermom (indx,imom,lup)

      include 'pokcom.com'

      integer*2 indx,imom,lup

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      real*4 xaa(maxpts), yaa(maxpts)
      integer*2 nppa,nppb,npaa
      integer*2 i,k,ierr,npop,mgran
      integer*2 ipop(50)
      real*4 dis,dbest

      mgran = mama(imom)
      npop = 0
      do i = 1,lanes
        if (i.ne.imom .and. lane(i).lt.100 .and. mama(i).eq.mgran .and.
     x      loop(i).eq.lup .and. npop.lt.50) then
          npop = npop+1
          ipop(npop) = i
        endif
      enddo

      if (npop .eq. 0) return

      call getlan (indx,xa,ya,nppa)
      call evolvlp (xa,ya,nppa,xaa,yaa,npaa,ierr)
      if (ierr .ne. 0) return

      call getlan (imom,xb,yb,nppb)
      call dslplp(xaa,yaa,npaa,xb,yb,nppb,dis)
      if (dis .le. stpmax) return

      dbest = dis

      do k = 1,npop
        i = ipop(k)
        call getlan (i,xb,yb,nppb)
        call dslplp(xaa,yaa,npaa,xb,yb,nppb,dis)

        if (dis .lt. dbest) then
          imom = i
          dbest = dis
        endif

      enddo

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE doubl1
C **  purpose of subroutine: REMOVE DOUBLE POINTS
c **    input/output:
C **      XA,YA...STRING OF POINTS
C **      NPPA....NUMBER OF POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE DOUBL1 (XA,YA,NPPA)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      integer*2 nppa
      real*4 XA(maxpts),YA(maxpts)

      integer*2 npp,i,j,j0,k1
      real*4 ee,dlarge,dx,dy,dist

      NPP = NPPA
      EE = tol/.254
      dlarge = 2500*tol*tol

C --- REMOVE DOUBLE POINTS
      I = 0
   11 I = I + 1
      IF (I.GE.NPP) GO TO 35
      K1 = 1
      IF (XA(I+1).EQ.FLARC) THEN
        K1 = 4
      ENDIF

      dx = XA(I)-XA(I+K1)
      dy = YA(I)-YA(I+K1)
c
c..... fsr 61101 - do not weed out points at the end of long segments
c
      IF (ABS(dx).LT.EE .AND. ABS(dy).LT.EE) THEN
        j0 = i
        if (dist .gt. dlarge .and. i+k1 .lt. npp) then
          j0 = i+1
        endif
        DO 19 J = j0, NPP - K1
          XA(J) = XA(J+K1)
          YA(J) = YA(J+K1)
   19   CONTINUE
        I = I-1
        NPP = NPP - K1
      ELSE
        DIST = dx*dx + dy*dy
        I = I+K1-1
      END IF
      GO TO 11
   35 continue
      NPPA = NPP

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE hlentr
c **  find a helix entry center point.
c **********************************************************************
c **********************************************************************

      SUBROUTINE hlentr (ix,xx,yy,xt,yt,rad,fact,ier)

      include 'pokcom.com'

      real*8 xx,yy
      real*4 xt(2),yt(2),rad,fact
      integer*2 ix,ier

      real*4 xb(maxpts),yb(maxpts),xc(maxpts),yc(maxpts)
      real*4 ueg,xn,yn,x1,y1,dist,areb,arec,vx,vy,d
      integer*2 nppb,nppc,larcsv,mom,lseg,iside,klb,klc

        mom = mama(ix)
        call getlan (mom,xb,yb,nppb)
c
c..... qar 96246: added missing transformation
c
        if (mom.lt.locapo .and. xlace .ne. 1.) then
          call lctran (xb,yb,nppb,0)
        endif

        call xclock (xb,yb,nppb,areb,klb)

        larcsv = larcs
        larcs = 1

        iside = -1

        call offslp1 (iside,rad*fact,xb,yb,nppb,xc,yc,nppc,klc,arec,ier)

        larcs = larcsv

        if (klc.ne.klb .or. arec/areb.gt.1) ier = 1

        if (ier .gt. 0) return

        x1 = xx*fact
        y1 = yy*fact

        call dsptlap (x1,y1,xc,yc,nppc,lseg,ueg,xn,yn,dist)

        dist = dist/fact
        xn = xn/fact
        yn = yn/fact

        if (dist .gt. rad + tol) then
          call getlan (ix,xb,yb,nppb)
          call dsptlap (xn,yn,xb,yb,nppb,lseg,ueg,x1,y1,dist)
        else
          x1 = x1/fact
          y1 = y1/fact
        endif

        if (dist .lt. rad-tol) ier = 1
c
c..... qar 96246: adjust the entry point if too far from the center
c
        if (dist .gt. rad+tol) then
          vx = x1 - xn
          vy = y1 - yn
          d = sqrt(vx**2+vy**2)
          if (rad .gt. 0.0) then
            vx = vx/d
            vy = vy/d
            x1 = xn + rad*vx
            y1 = yn + rad*vy
          endif
        endif

        xt(1) = x1
        yt(1) = y1

        xt(2) = xn
        yt(2) = yn

      RETURN
      END
