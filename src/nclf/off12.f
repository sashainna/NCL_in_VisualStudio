c**    NAME           : off12.f
c**       CONTAINS:
c**
c**  1. SUBROUTINE CUTPOCK (STEP,KLIMB,IERR)
c**  2. SUBROUTINE CUTPATH (STEP,KLIMB,ISEG,XSEG,YSEG,USEG,IERR)
c**  3. SUBROUTINE CUT1ST (LAN,LSEG,USEG,XB,YB,NPPB,X1,Y1,X2,Y2)
c**  4. SUBROUTINE CUTREST (INDEX,X1,Y1,X2,Y2,MANOR
c**    x                  ,NORMAL,XINT,YINT,UINT,LSEG)
c**  5. SUBROUTINE NOTLIFT (ISEG,XSEG,YSEG,USEG
c**    x                  ,INDEX,KLIMB,INDEX1,INDEX2,ierr)
c**  6. SUBROUTINE NEXT (LAN1ST,INDEX,INDEX1,INDEX2,INDEX3,INDEX4)
c**  7. SUBROUTINE TOOLIFT (ISEG,XSEG,YSEG,USEG,INDEX,KLIMB,INDEX1,ierr)
c**  8. SUBROUTINE CUTLOOK (STEP,KLIMB,ISEG,XSEG,YSEG,USEG,IERR)
c**  9. subroutine findu (indem,xa,ya,nppa,npa,ipapa,step,useg,lseg)
c**  10. SUBROUTINE mvbyj
c**  11. SUBROUTINE filcor
c**  12. subroutine chkgou
c**  13. subroutine pastrt
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       off12.f , 25.2
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 17:22:29
c **********************************************************************
c**
c** copyright (c) 1990 MILLS DATA SYSTEMS CORPORATION
c**
c **********************************************************************
c **********************************************************************
c **    SUBROUTINE CUTPOCK
C **  purpose of subroutine: CUT FINAL ROUTING FOR POCKET
c **    input:
C **      STEP....STEP BETWEEN LOOPS
C **      KLIMB...-1 - CLIMB MILLING, LEFT, +1 - REGULAR, RIGHT
C **    output:
c **      IERR....0 - OK, ELSE - ERROR
c **********************************************************************
c **********************************************************************

      SUBROUTINE CUTPOCK (STEP,KLIMB,IERR)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'
      include 'pokseg.com'

      integer*2 idx
      real*8 ver
      
      idx = 169
      call getsc(idx,ver)

      LOCAPO = LANES+1

C --- FIND INTERSECTION POINTS FOR PATH
      CALL CUTPATH (STEP,KLIMB,IERR)
      IF (IERR.NE.0) GOTO 90

C --- FOR END POINT RECOMPUTE LANE 1 INTERSECTIONS
      IF (LOOK.EQ.1) THEN
        CALL CUTLOOK (STEP,KLIMB,IERR)
        IF (IERR.NE.0) GOTO 90
      ENDIF

C --- CUT PATH WITHOUT LIFT OF TOOL -----------------------------------
      IF (LIFTS.EQ.1) GOTO 60

C --- FIND FIRST LANE TO COMPUTE PATH TO
      LAN1ST = 1
      K = LOCAPO
   15 K = K-1
      IF (K.EQ.0) THEN
        IF (VER.GT.9.949) THEN
          GOTO 60
        ELSE
          GOTO 90
        ENDIF
      ENDIF
      IF (LANE(K).EQ.LAN1ST.AND.LOOP(K).LT.0) THEN
        INDEX1 = K
        INDEX = MAMA(K)
        MAMA(K) = -MAMA(K)
        GOTO 20
      ENDIF
      GOTO 15
   20 continue
   21 CALL NOTLIFT (INDEX,KLIMB,INDEX1,INDEX2,ierr)
c
c..... Eduard 4/28/1999. Added ierr parameter to signal when the
c..... maximum number of lanes is exceeded.
c
      IF (IERR.NE.0) GOTO 90
      CALL NEXT (LAN1ST,INDEX,INDEX1,INDEX2,INDEX3,INDEX4)
      IF (INDEX4.EQ.999) THEN
        if (ltrarc .eq. 1) CALL NOTLIFT (INDEX3,KLIMB,INDEX4,INDEX2,
     x                                                         ierr)
        LANES = LANES+1
        LANE(LANES) = 999
        LOOP(LANES) = 0
        LOCA(LANES+1) = LOCA(LANES)
        MAMA(LANES) = 0
        IF (VER.GT.9.949) THEN
          GOTO 60
        ELSE
          GOTO 90
        ENDIF
      ENDIF
      INDEX = INDEX3
      INDEX1 = INDEX4
      GOTO 21

C --- CUT PATH WITH LIFT OF TOOL -------------------------------------
   60 continue
      LAN1ST = 0
      DO 62 K = 1, LOCAPO-1
      IF (LAN1ST.LT.LANE(K).AND.LANE(K).LT.101) LAN1ST = LANE(K)
   62 CONTINUE
c
c...   If any lanes have been dropped, contract lane numbers
c...   so there are no gaps. This fixes a problem where only
c...   the innermost lanes are processed.  IJD 23-Jan-95
c
      k = 1
  621 k = k+1
      if (k.ge.lan1st) goto 63
      j = 0
  622 j = j+1
      if (j.gt.lanes) goto 623
      if (lane(j).eq.k) goto 621
      goto 622
  623 do 624 j=1,lanes
  624 if (lane(j).gt.k.and.lane(j).lt.101) lane(j) = lane(j)-1
      lan1st = lan1st-1
c     goto 621
c
c..... Eduard 4/28/1999. Have to check again for the same k, until
c..... the gap is closed completely.
c
      j = 0
      goto 622
   63 K = LOCAPO
   65 K = K-1
      IF (K.EQ.0) THEN
        LAN1ST = LAN1ST-1
        IF (LAN1ST.LE.100) GOTO 90
        GOTO 63
      ENDIF
c
c.....Added to NOTLIFT case to make sure all loops have been
c.....considered - ASF 10/2/13
c
      IF (((VER.GT.9.949.AND.LIFTS.EQ.0).OR.LANE(K).EQ.LAN1ST).AND.
     x     LOOP(K).LT.0.AND.MAMA(K).GT.0) THEN
        INDEX = K
        LAN1 = LANE(K)
        MAMA(K) = -MAMA(K)
        GOTO 70
      ENDIF
      GOTO 65
   70 INDEX1 = INDEX
      INDEX = ABS(MAMA(INDEX))
      IF (LANE(INDEX).NE.LAN1.OR.LOOP(INDEX).EQ.0) THEN
        LAN1ST = LAN1ST-1
        IF (LAN1ST.EQ.0) LAN1ST = 100+islnds
        if (loop(lanes) .eq. 1) then
          index = lanes
          call pastrt (index)
        endif
        LANES = LANES+1
        LANE(LANES) = 999
        LOOP(LANES) = 0
        LOCA(LANES+1) = LOCA(LANES)
        MAMA(LANES) = 0
        GOTO 63
      ENDIF
      index2 = ABS(mama(index))
      if (ltrarc.ne.1 .or.
     x    lane(index2).ne.lan1 .or. loop(index2).eq.0) index2 = 0
      CALL TOOLIFT(INDEX,KLIMB,INDEX1,index2,ierr)
c
c..... Eduard 4/28/1999. Added ierr parameter to signal when the
c..... maximum number of lanes is exceeded.
c
      IF (IERR.NE.0) GOTO 90
      GOTO 70

   90 continue

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE CUTPATH
C **  Purpose of subroutine: find intersection path between lane/loops
c **    Input:
C **      STEP........STEP BETWEEN LOOPS
C **      KLIMB.......-1- CLIMB MILLING, LEFT, +1- REGULAR, RIGHT
C **    Output:
c **      ISEG........INTERSECTION SEGMENT NUMBER
C **      XSEG,YSEG...INTERSECTION COORDINATES
C **      USEG........INTERSECTION RELATIVE POSITION
C **      IERR........0 - OK, ELSE - ERROR
c **
c **********************************************************************
c **********************************************************************

      SUBROUTINE CUTPATH (STEP,KLIMB,IERR)

      include 'com4a.com'
      include 'const.com'
      include 'pokcom.com'
      include 'pokseg.com'

      real*4 XA(maxpts),YA(maxpts)
      integer*4 IPAPA(50)
      integer*2 jseg(maxseg)

      integer*2 iangle, IANGMX, ierr, ind, j, j1, j2, mom, lupk
      integer*4 kk, lanes1
      real*4 ddmax,ddmx0,ddmx1,dx,dy,dd,dx1,dy1,rl,uu,dst1,d1min,uintmin
      real*8 xx,yy
      integer*2 lastry,ntry,lan2,index2,mamhld,norcut,norct1
      logical lv94,lnoperp

      lv94 = sc(169).lt.9.449d0

      if (lv94) then
        IANGMX = 36
      else
        IANGMX = 54
      endif

      IERR = 0
      lastry = 0
      ntry = 0
c
c..... FSR 60752 - replaced previous logic for the case the cutting has
c..... problems: first (ntry = 1) try to cut to the outer loop without
c..... any time-consuming tricks, this should work in most cases: then try
c..... to cut disregarding lane numbers, i.e., any cut from a loop to its
c..... mama is good (ntry = 2). If this works, renumber the lanes so that the
c..... successful chain has constant lane number. In any event, reset and do
c..... the work from start (ntry = 0), with the tricks if needed.
c
      if (sc(169) .ge. 9.3) ntry = 1
      ddmx0 = 4.01*step*step
      ddmx1 = 2.*ddmx0
      ddmax = ddmx0

      IF (LANES.GT.maxseg) THEN
        IERR = 10
        GOTO 99
      ENDIF

      lanes1 = lanes
      if (lanes .le. maxseg-10) lanes1 = lanes + 10
    5 DO 10 I = 1, LANES1
        ISEG(I) = 0
        JSEG(I) = 0
        XSEG(I) = 0.0
        YSEG(I) = 0.0
        USEG(I) = 0.0
   10 CONTINUE
c
C... FIND LANE/loop TO COMPUTE PATH INTERSECTIONS from inside out
c
   12 K = LANES+1
   15 K = K-1
      if (ntry .eq. 2) then
        if (k .eq. 0) then
          ntry = 0
          lastry = 0
          goto 5
        else if (lane(mama(k)).eq.lan2 .or. lane(mama(k)).gt.100) then
          goto 15
        endif
      endif
      IF (K.EQ.0) GOTO 99
1525  IF (LOOP(K).LT.0.AND.ISEG(K).EQ.0) THEN
        LUPK = LOOP(k)
        INDEX1 = K
        INDEM = MAMA(K)
        LUP1 = LOOP(INDEM)
        LAN1 = LANE(INDEM)
        mamhld = mama(indem)
c
c..... Eduard 4/28/1999. Here IND is the cousin of the lane K, the one
c..... with the same value of lane(ind) as their common grandmother's.
c..... In other words, ind is such that mama(mama(ind))=mamhld, and
c..... lane(ind)=lane(mamhld).
c
        lan = lane(mamhld)
        ind = 0
        if (lan.ne.lan1 .and. look.ne.1 .and. sc(169).lt.9.3) then
          do 155 l=lanes,1,-1
            if (loop(l).eq.lupk .and. lane(l).lt.100 .and.
     x          lane(l).eq.lan .and. iseg(l).eq.0) then
              mom = mama(l)
              if (mama(mom).eq.mamhld .and. loop(mom).eq.lup1
     x            .and.lane(mom).eq.lan) then
                ind = l
                goto 16
              endif
            endif
155       continue
        else if (lan1.gt.100 .and. .not.lv94) then
c
c..... fsr 61078: prevent an infinite loop
c
          mom = mama(mamhld)
          if (mom .eq. indem) goto 15
        endif
      ELSE
        GOTO 15
      ENDIF

   16 CALL GETLAN (INDEM,XA,YA,NPPA)
      if (lan1.gt.100 .and. ntry.eq.1) ntry = 0
c
c... check if path is a profile or hidden center island
c
      call xclock (xa, ya, nppa, area, klok)
      if (lan1.lt.100 .and. klok*cutin.lt.0.0) then
        call flipo (xa, ya, nppa)
        kk = loca(indem)-1
        do 17 i=1,nppa
          kk = kk+1
          xx = xa(i)
          yy = ya(i)
c
c..... linked list storage
c
          call rrput(kk,xx,yy,ierr)
          IF (IERR.NE.0) GOTO 99
   17   continue
      endif
c
c... look for different lanes with same loop id
c
      indmil = indem
      kpa = 0
      npa = 0
      lnoperp = .false.
c
c..... qar 95223 - at least for island lanes the IPAPA array should
c..... contain aunts of the lane indmil=mama(k). Possibly it should be
c..... for all lanes.
c
      if (lan1.gt.100 .and. .not.lv94) then
        do l=1,lanes
          if (loop(l).eq.loop(mamhld) .and. lane(l).lt.100
     x        .and. mamhld.ne.l .and. npa.lt.50) then
            npa=npa+1
            ipapa(npa) = l
          endif
        enddo
        goto 21
      endif
c
c..... Eduard 4/28/1999. The array IPAPA in fact consists of "aunts"
c..... (i.e., mama's sisters) of the lane K.
c
      do 20 l=1,lanes
        if (loop(l).eq.lup1 .and. lane(l).lt.100
     x      .and. indem.ne.l .and. npa.lt.50) then
          npa=npa+1
          ipapa(npa) = l
        endif
   20 continue
c
C... FIND BEST CUT LINE: TRY UP TO 10 NORMALS ON EACH SEGMENT
c... OF CENTER LOOP
c
   21 LEUINT = 0

      n1min = 10000
      d1min = flarc
      lsmin = 0
      leumin = 0
      iamin = 0
      uintmin = 0.5
c
c..... Here the code is trying to find a (difficult)
c..... cut by varying the parameter UINT over the interval (0,1).
c
   22 LEUINT = LEUINT+1
      iangle = 0
      if (.not.lv94) ddmax = ddmx0
      UINT = 0.5+0.05*(LEUINT-1)
      IF (UINT.GT.1.0) UINT = UINT-1.0
   23 LSEG = 0
      MANOR = 0
c
c... try next segment of center loop
c
   25 LSEG = LSEG+1
      IF (XA(LSEG).EQ.FLARC) THEN
        LSEG = LSEG+3
      ENDIF
      IF (LSEG.GE.NPPA) THEN
c
c..... Eduard 4/28/1999. If a cut could not be found, try to slant
c..... the cutting line from the normal to the central lane.
c
        IF (iangle.lt.IANGMX .and. ntry.eq.0) then
          iangle = iangle+1
          if (lv94) then
            ddmax = ddmx1
          else
            j = (iangle + 1)/2
            alp = j*(PI/72.)
            co = cos(alp)
            ddmax = ddmx0 / (co*co)
            if (ddmax .gt. ddmx1) ddmax = ddmx1
          endif
          goto 23
        ENDIF
        IF (LEUINT.lt.20 .and. ntry.eq.0) GOTO 22
        IF (KPA.LT.NPA .and. ntry.eq.0) THEN
          KPA = KPA+1
          INDEMP = IPAPA(KPA)
          MAMA(INDMIL) = INDEMP
          GOTO 21
        ENDIF
        if (lastry.eq.0) then
          lastry = 1
c
c..... qar 60985 - prevent memory problem
c
          if (.not.lv94 .and. kpa .gt. 0) then
            kpa = 0
            npa = 0
          endif
          mama(indmil) = mamhld
          indemp = mamhld
          call findu (indemp,xa,ya,nppa,npa,ipapa,step,uint,lseg)
          if (lseg.gt.0) goto 27
        endif
c
c..... Error exit: cannot cut through loops
c
        if (ntry.eq.1) then
          lan2 = lan1
          ntry = 2
          lastry = 0

          do k = islnds+2, lanes
            if (jseg(k) .eq. 0) iseg(k) = 0
          enddo

          goto 12
        else if (ntry.eq.2) then
          goto 12
        endif
        if (.not.lnoperp .and. n1min.lt.10000) then
          lnoperp = .true.
          iangle = iamin
          leuint = leumin
          lseg = lsmin
          uint = uintmin
          goto 27
        endif
        IERR = 51
        GOTO 99
      ENDIF
   27 NORCUT = 0
      norct1 = 0
      dst1 = 0
      MANOR = 1 + iangle
c
C... CUT THROUGH FIRST LOOP FOR EACH LANE and FIND CUT LINE
c
      UINTS = UINT
      CALL CUT1ST (lv94,iangle,LAN1,LSEG,UINTS,XA,YA,NPPA,X1,Y1,X2,Y2)
      if (lv94 .and. iangle .gt. 0) then
        dx = x1 - xa(lseg)
        dy = y1 - ya(lseg)
        dx1 = x1 - x2
        dy1 = y1 - y2
        rl = 1.0
        uu = 0.0
        if ( dx*dy1-dy*dx1 .lt. 0.) rl = -1.0
        uu = iangle/45.0
        x2 = x2 + uu*(dx1+rl*dy1)
        y2 = y2 + uu*(dy1-rl*dx1)
      endif
      ISEG(INDEX1) = LSEG
      XSEG(INDEX1) = X1
      YSEG(INDEX1) = Y1
      USEG(INDEX1) = UINT
      KONNECT = 0
      KOUNT = 0
      INDEX = INDEX1
c
C... CUT THROUGH REST OF LOOPS FOR EACH LANE
c
   30 INDEXO = INDEX
      INDEX = MAMA(INDEX)
      INDEM = MAMA(INDEX)
      IF (KPA.GT.0) INDEM = INDEMP
      LAN = LANE(INDEM)
      IF (LAN.NE.LAN1) KONNECT = 1
      IF (LOOP(INDEM).EQ.0) THEN
        if (ntry.eq.2) then
          ntry = 1
          lastry = 0
          if (konnect .eq. 1) then
c
c..... exchange lane numbers between lan and lan1
c
          k = index1
31        if (k.gt.indem .and. lane(k).ne.lan) then
            lane(k) = -lane(k)
            k = mama(k)
            goto 31
          else
            index2 = k
          endif
          do k = index2+1,lanes
            if (lane(k).eq.lan .and. loop(k).ne.0) lane(k) = lan1
          enddo
          k = index1
32        if (k.gt.index2 .and. lane(k).lt.0) then
            lane(k) = lan
            k = mama(k)
            goto 32
          endif
          endif

          do k = islnds+2, lanes
            if (jseg(k) .eq. 0) iseg(k) = 0
          enddo

          k = index1
          goto 1525
        endif
c
c... all done with this lane/loop
c
        ISEG(INDEX) = 0
        XSEG(INDEX) = 0.0
        YSEG(INDEX) = 0.0
        USEG(INDEX) = 0.0

        do 33 l=lanes,1,-1
          if (loop(l).lt.0 .and. iseg(l).gt.0 .and. jseg(l).eq.0) then
            indem = l
            do k=1,lanes
              indem = mama(indem)
              if (indem .eq. index) then
                jseg(l) = 1
                jseg(index) = 1
                indem = l
                do m=1,lanes
                  indem = mama(indem)
                  if (indem.eq.index .or. loop(indem).le.0 .or.
     x                                    iseg(indem).eq.0) goto 33
                    jseg(indem) = 1
                enddo
                goto 33
              endif
              if (loop(indem).le.0 .or. iseg(indem).eq.0) then
                goto 33
              endif
           enddo
          endif
  33    continue
c        ntry = 0

        GOTO 12
      ENDIF
      UINTS = UINT
      CALL CUTREST (INDEM,X1,Y1,X2,Y2,MANOR,NORM,XINT,YINT,UINTS,KSEG)
      ISEG(INDEX) = KSEG
      XSEG(INDEX) = XINT
      YSEG(INDEX) = YINT
c
c..... Fsr 61027 - uints should be put in useg array, since uint is
c..... constant for this leuint value. The useg array is used in notlift.
c
      if (lv94) then
        USEG(INDEX) = UINT
      else
        USEG(INDEX) = uints
      endif
      KOUNT = KOUNT+1
      IF (NORM.EQ.1) NORCUT = NORCUT+1

      if (norm.eq.-1 .and. iangle.gt.4) then
        if (.not.lnoperp) then
          norct1 = norct1+1
        else
          norcut = norcut+1
        endif
      endif

      DX = XSEG(INDEX)-XSEG(INDEXO)
      DY = YSEG(INDEX)-YSEG(INDEXO)
      DD = DX*DX+DY*DY
      IF (DD .GT. ddmax) THEN
c
c..... a fix for 95202: if this lane already ended, allow a longer cut
c
        if (.not.lv94 .and. ntry.eq.0 .and. norm.eq.1 .and. kount.ge.2
     x       .and. konnect.eq.1 .and. kount.eq.norcut .and. dd.lt.ddmx1)
     c                                                              then
          goto 40
        endif
c
c... too far away to be next loop
c
        NORCUT = NORCUT-100
        GOTO 25
      ENDIF
      IF (NORM.EQ.0 .or. (iangle.gt.0.and.norm.eq.-2)) THEN
c
c... NORM = 0 means there is no intersection with the current
c... lane INDEM. If the cutting line is slanted, NORM = -2 means we
c... have a double intersection or the line gets too close to an island.
c
        NORCUT = NORCUT-200
        GOTO 25
      ENDIF
 40   continue

      if (.not.lnoperp .and. norm.eq.-1 .and. iangle.gt.4)
     x   dst1 = dst1 + dd

      IF (KONNECT.EQ.1) THEN
c
c... may have a island/pocket loop exit
c
        KONNECT = 0
        IF (KOUNT.EQ.NORCUT .or. MANOR.EQ.0) then
           if (ntry.eq.2) goto 30
c
c..... the if-block below is for NCL93 or older
c
           if (kount.le.1 .and. ind.gt.0 .and. iseg(ind).eq.0 .and.
     X       sc(169).ge.9.05d0 .and. sc(169).lt.9.3) then
c
c..... If the cut starts well but the lane number is wrong, we try to
c..... exchange the lane number between this lane (and her mama) and her
c..... cousin IND (and her mama).
c
             j1 = ind
             j2 = k
             do 80 j=1,2
                lane(j2) = lan
                lane(j1) = lan1
                j2 = mama(j2)
                j1 = mama(j1)
80           continue
             lan1 = lan
             goto 30
           endif

           if (lnoperp) lastry = 0

           if (jseg(indem) .ne. 0) then
             jseg(index) = 1
             indem = index1
   81        jseg(indem) = 1
             indem = mama(indem)
             if (indem .ne. index) goto 81
           endif

           GOTO 12

        else
           if (ntry.eq.0 .and. kount.eq.(norcut+norct1)) then
c
c..... this would be a possible cut if lnoperp were in effect;
c..... remember it if it is the best such
c
             if (norct1.lt.n1min .or.
     x           (norct1.eq.n1min .and. dst1.lt.d1min)) then
               n1min = norct1
               d1min = dst1
               lsmin = lseg
               iamin = iangle
               leumin = leuint
               uintmin = uint
             endif
           endif
           GOTO 25
        endif
      ENDIF
      GOTO 30

99    RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE CUT1ST
C **  purpose of subroutine: CUT FIRST CENTER LOOP - FIND LINE OF CUT
c **    input:
C **      LAN...........LANE NUMBER OF CENTER LOOP
C **      LSEG..........CUT SEGMENT
C **      uint..........U OF CUT ON SEGMENT
C **      XB,YB,NPPB....CENTER LOOP
C **    output:
c **      X1,Y1,X2,Y2...CUT LINE FOR LANE
c **********************************************************************
c **********************************************************************

      SUBROUTINE CUT1ST (lv94,iangle,LAN,LSEG,uint,XB,YB,NPPB,X1,Y1,
     *                                                        X2,Y2)
      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'const.com'
      include 'pokcom.com'

      real*4 XB(maxpts),YB(maxpts),uint,X1,Y1,X2,Y2
      integer*2 iangle,lan,lseg,nppb
      logical lv94

      real*4 XA(maxpts),YA(maxpts),dx,dy,dd,co,si,alp,ux,uy
      integer*2 i,j,nppa,klok


       if (.not.lv94 .and. iangle .gt. 0) then
         j = (iangle + 1)/2
         alp = j*(PI/72.)
         if (2*j .eq. iangle) alp = -alp
         co = cos(alp)
         si = sin(alp)
       endif

      NPPA = NPPB
      DO 10 I = 1, NPPA
        XA(I) = XB(I)
        YA(I) = YB(I)
   10 CONTINUE
      ICEN = 1
      X2 = 0.0
      Y2 = 0.0
      CALL SHUFFLE (XA,YA,NPPA,LSEG,ICEN,uint,X2,Y2)
      X1 = XA(1)
      Y1 = YA(1)
      IF (XA(2).EQ.FLARC) THEN
        DD = XA(3)
        DX = XA(1)-XA(4)
        DY = YA(1)-YA(4)
        KLOK = YA(3)
        IF (KLOK.EQ.-1) DX = -DX
        IF (KLOK.EQ.-1) DY = -DY
        DX = DX/DD
        DY = DY/DD
        IF (LAN.GT.100) THEN
          DX = -DX
          DY = -DY
        END IF

         if (.not.lv94 .and. iangle .gt. 0) then
           ux = dx*co - dy*si
           uy = dx*si + dy*co
           dx = ux
           dy = uy
         endif

        X2 = X1+DX*10000.0
        Y2 = Y1+DY*10000.0
      ELSE
        DX = XA(2)-XA(1)
        DY = YA(2)-YA(1)
        DD = SQRT(DX*DX+DY*DY)
        DX = DX/DD
        DY = DY/DD
        IF (LAN.GT.100) THEN
          DX = -DX
          DY = -DY
        END IF

         if (.not.lv94 .and. iangle .gt. 0) then
           ux = dx*co - dy*si
           uy = dx*si + dy*co
           dx = ux
           dy = uy
         endif

        X2 = X1+DY*10000.0
        Y2 = Y1-DX*10000.0
      END IF

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE CUTREST
C **  purpose of subroutine: CUT ALL LOOPS EXCEPT FIRST LOOP
c **    input:
C **      INDEX.........SEQUENCE NUMBER IN COMMON RRLN
C **      X1,Y1,X2,Y2...CUT LINE FOR LANE
C **      MANOR.........1- MUST BE NORMAL, 0- DOES NOT HAVE TO BE NORMAL,
C **                    >1 - the cut is slanted w.r.t. the central lane.
c **    output:
C **      NORMAL........0- NO CUT, 1- NORMAL CUT, -1- NOT NORMAL- OUTPUT
C **                    -2 - bad cut (checked if MANOR>1)
C **      XINT,YINT.....INTERSECTION POINT WITH LOOP
c **      UINT..........RELATIVE LOCATION OF INTERSECTION
C **      LSEG..........INTERSECTION SEGMENT
c **********************************************************************
c **********************************************************************

      SUBROUTINE CUTREST (INDEX,X1,Y1,X2,Y2,MANOR,NORMAL,XINT,YINT,
     x                                                   UINT,LSEG)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)
      integer*2 nppa
      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)

      integer*2 ierr, iloop1, k, lseg1, nobox1
      integer*4 kk
      real*8 xx,yy
      real*4 xint1, yint1

      NORMAL = 1
c     EPS = tol/25.
c
c..... Eduard 4/28/1999. Replaced the old value of EPS.
c
      eps = tol/2.
      CALL GETLAN (INDEX,XA,YA,NPPA)
c... check if the path is a profile or a hidden center island
      call xclock (xa, ya, nppa, area, klok)
      if (lane(index).lt.100 .and. klok*cutin.lt.0.0) then
        call flipo (xa, ya, nppa)
        kk = loca(index)-1
        do 17 i=1,nppa
          kk = kk+1
          xx = xa(i)
          yy = ya(i)
          call rrput(kk,xx,yy,ierr)
   17   continue
      endif
      CALL BOXES (NPPA,XA,YA,XL1,YL1,XL2,YL2,LLT,NBOX)
      N = -1
      KARC1 = 0
      NOBOX = 1
      MM1 = 1
      MM2 = NPPA-1
      XMM = XA(1)
      YMM = YA(1)
      KLOSED = 1
      XINT = XMM
      YINT = YMM
      NX = MM1
      NOBOXX = 1
      ILOOP = 0
      CALL SEEK (N,NOBOX,NBOX,KARC1,XC1,YC1,R1,KLOK1,X1,Y1,
     x          X2,Y2,XA,YA,MM1,MM2,XMM,YMM,LLT,XL1,YL1,XL2,YL2,
     x          KLOSED,LSEG,NOBOXX,XINT,YINT,ILOOP)
      IF (ILOOP.EQ.0) THEN
        NORMAL = 0
        XINT = 0.0
        YINT = 0.0
        GOTO 99
      ENDIF
      IF (MANOR.EQ.0) GOTO 99

C --- CHECK IF LINE IS NORMAL TO CUT
      IF (XA(LSEG+1).EQ.FLARC) THEN
        R = XA(LSEG+2)
        KLOK = YA(LSEG+2)
        XC = XA(LSEG+3)
        YC = YA(LSEG+3)
        CALL UARC (R,KLOK,XC,YC,XA(LSEG),YA(LSEG),
     x             XA(LSEG+4),YA(LSEG+4),XINT,YINT,UINT)
        DX1 = X2-X1
        DY1 = Y2-Y1
        DD = SQRT (DX1*DX1+DY1*DY1)
        DX1 = DX1/DD
        DY1 = DY1/DD
        DX2 = XINT-XC
        DY2 = YINT-YC
        DD = SQRT (DX2*DX2+DY2*DY2)
        DX2 = DX2/DD
        DY2 = DY2/DD
        DD = DX1*DY2-DY1*DX2
        IF (ABS(DD).GT.EPS) NORMAL = -1
      ELSE
        DX2 = XINT-XA(LSEG)
        DY2 = YINT-YA(LSEG)
        IF (ABS(DX2).LT.EPS.AND.ABS(DY2).LT.EPS) THEN
          DX2 = XA(LSEG+1)-XINT
          DY2 = YA(LSEG+1)-YINT
        ENDIF
        CALL USEG (XA(LSEG),YA(LSEG),XA(LSEG+1),YA(LSEG+1),
     +             XINT,YINT,UINT)
        DX1 = X2-X1
        DY1 = Y2-Y1
        DD = SQRT (DX1*DX1+DY1*DY1)
        DX1 = DX1/DD
        DY1 = DY1/DD
        DD = SQRT (DX2*DX2+DY2*DY2)
        DX2 = DX2/DD
        DY2 = DY2/DD
        DD = DX1*DY2-DY1*DX2
        EE1 = 1.0-EPS
        IF (ABS(DD).LT.EE1) NORMAL = -1
      ENDIF

      if (manor .gt. 1) then
c
c..... Eduard 4/28/1999. Check if the cut is not double.
c
         iloop1 = 0
         nobox1 = 1
         xi = xint + eps*dx1
         yi = yint + eps*dy1
         CALL SEEK (N,NOBOX,NBOX,KARC1,XC1,YC1,R1,KLOK1,xi,yi,
     x          X2,Y2,XA,YA,MM1,MM2,XMM,YMM,LLT,XL1,YL1,XL2,YL2,
     x          KLOSED,LSEG1,NOBOX1,XINT1,YINT1,ILOOP1)
         if (iloop1 .ne. 0) then
           dx1 = xint1-xi
           dy1 = yint1-yi
           DD = SQRT (DX1*DX1+DY1*DY1)
           IF (DD.GT.EPS) then
             normal = -2
             goto 99
           endif
         endif
c
c..... Eduard 4/28/1999. Check if the cut intersects any first loop
c..... around an island.
c
         do 80 k = islnds+2, index-1
           if (mama(k).eq.index .and. loop(k).eq.1) then
             iloop1 = 0
             CALL GETLAN (k,XA,YA,NPPA)
             CALL BOXES (NPPA,XA,YA,XL1,YL1,XL2,YL2,LLT,NBOX)
             N = -1
             KARC1 = 0
             NOBOX = 1
             KLOSED = 1
             XINT1 = XMM
             YINT1 = YMM
             NX = MM1
             NOBOX1 = 1
             MM1 = 1
             MM2 = NPPA-1
             XMM = XA(1)
             YMM = YA(1)
             CALL SEEK (N,NOBOX,NBOX,KARC1,XC1,YC1,R1,KLOK1,X1,Y1,
     x          X2,Y2,XA,YA,MM1,MM2,XMM,YMM,LLT,XL1,YL1,XL2,YL2,
     x          KLOSED,LSEG1,NOBOX1,XINT1,YINT1,ILOOP1)
             if (iloop1 .ne. 0) then
                 normal = -2
                 goto 99
             endif
           endif
80       continue
      endif

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE addarc
C **  Purpose of subroutine: Add a two-arc (S-shaped) transition from the
C **                         end of loop C to the start of loop B.
c **    input:
C **      lwall...... loop B is near the wall if true
C **      XC,YC...... previous loop
C **      NPPC....... Number of points in loop.
C **      XB,YB...... next loop
C **      NPPB....... Number of points in loop.
C **      rcorn...... radius of corner fillet.
C **    output:
C **      XB,YB,xc,yc...... New loops
C **      iexb - number of points cut off from loop B
c **********************************************************************
c **********************************************************************

      subroutine addarc (lwall,xb,yb,nppb,iexb,xc,yc,nppc)

      include 'com4a.com'
      include 'rrdm.com'
      include 'pokcom.com'

      integer*2 nppb,nppc,iexb
      real*4 xb(maxpts),yb(maxpts),xc(maxpts),yc(maxpts)

      real*4 vax,vay,da,vbx,vby,db,vcx,vcy,dc,vhx,vhy,dh,r,rad
      real*4 dang,ang,tolsq,brx,bry,crx,cry,arx,ary,rb,rc
      real*4 vx1,vy1,vx2,vy2,cex,cey,dbh,dch,del
      integer*2 lcr,lbr,iexc
      logical lwall

          if (nppb.le.4 .or. xb(2).eq.flarc) return
          if (nppc.le.4 .or. xc(nppc-3).eq.flarc) return

          tolsq = 16*tol*tol

          del = hrtdis
          if (ifl(264).eq.0) del = del*25.4
          if (del .eq. 0) del = 10*tol
          del = del + 6*tol
          if (del .lt. 11*tol) del = 11*tol

          brx = xb(1)
          bry = yb(1)
          crx = xc(nppc)
          cry = yc(nppc)
c
c..... [cr,br] is the transition segment between the C and B loops
c
          vhx = brx - crx
          vhy = bry - cry
          dh = vhx**2 + vhy**2
          if (dh.lt.tolsq) return
c
c..... the arcs will be tangent to the transition segment; if there is enough
c..... room, the point of tangency will be the middle of it.
c
          arx = (brx + crx)/2.
          ary = (bry + cry)/2.
          rc = dh/4.
          rb = dh/4.
          lbr = 0
          lcr = 0
          iexc = 0

          vcx = crx - xc(nppc-1)
          vcy = cry - yc(nppc-1)
          dc = vcx**2 + vcy**2
c
c..... If the last leg of loop C is too short, we try to extend it by joining
c..... it with the previous leg (we try it twice), if the previous leg goes
c..... in roughly the same direction as the last.
c..... If it does not work we make the first arc smaller.
c
          if (4.*dc.lt.dh) then
            if (nppc.le.5 .or. xc(nppc-4).eq.flarc) return
            vax = xc(nppc-1) - xc(nppc-2)
            vay = yc(nppc-1) - yc(nppc-2)
            da = vax**2 + vay**2
            r = vcx*vax + vcy*vay
            if (r.lt.0 .or. r*r.lt.0.5*dc*da) then
              if (dc .lt. tolsq) return
              rc = dc
              lcr = 1
              goto 50
            endif
            vcx = crx - xc(nppc-2)
            vcy = cry - yc(nppc-2)
            dc = vcx**2 + vcy**2
            iexc = 1
            if (4.*dc .lt. dh) then
              if (nppc.le.6 .or. xc(nppc-5).eq.flarc) return
              vax = xc(nppc-2) - xc(nppc-3)
              vay = yc(nppc-2) - yc(nppc-3)
              da = vax**2 + vay**2
              r = vcx*vax + vcy*vay
              if (r.lt.0 .or. r*r.lt.0.5*dc*da) then
                if (dc .lt. tolsq) return
                rc = dc
                lcr = 1
                goto 50
              endif
              vcx = crx - xc(nppc-3)
              vcy = cry - yc(nppc-3)
              dc = vcx**2 + vcy**2
              iexc = 2
              if (4.*dc.lt.dh) then
                if (dc .lt. tolsq) return
                lcr = 1
                rc = dc
                goto 50
              endif
            endif
          endif

50        vbx = xb(2) - brx
          vby = yb(2) - bry
          db = vbx**2 + vby**2
c
c..... If the first leg of the loop B is too short, we try to extend it by
c..... joining it with the next leg (we try it twice), provided the next leg
c..... goes in roughly the same direction as the first one.
c..... Maybe we should also make the second arc smaller if extending fails,
c..... but so far it has not.
c
          if (4.*db.lt.dh) then
            if (lwall) then
c
c..... if loop B is near a wall then the arc transition should not end at a
c..... corner, since we have to allow some room for an exit.
c
              rb = sqrt(db)
              rb = rb - del
              if (rb .lt. 4*tol) return
              rb = rb*rb
              lbr = 1
              goto 60
            endif
     
            if (nppb.le.5 .or. xb(3).eq.flarc) return
            vax = xb(3) - xb(2)
            vay = yb(3) - yb(2)
            da = vax**2 + vay**2
            r = vbx*vax + vby*vay
            if (r.lt.0 .or. r*r.lt.0.5*db*da) return
            vbx = xb(3) - brx
            vby = yb(3) - bry
            db = vbx**2 + vby**2
            iexb = 1
            if (4.*db.lt.dh) then
              if (nppb.le.6 .or. xb(4).eq.flarc) return
              vax = xb(4) - xb(3)
              vay = yb(4) - yb(3)
              da = vax**2 + vay**2
              r = vbx*vax + vby*vay
              if (r.lt.0 .or. r*r.lt.0.5*db*da) return

              vbx = xb(4) - brx
              vby = yb(4) - bry
              db = vbx**2 + vby**2
              r = vbx*vax + vby*vay
              if (4.*db.lt.dh) return
              iexb = 2
            endif
          endif

60        continue
          dc = sqrt(dc)
          vcx = vcx/dc
          vcy = vcy/dc
          db = sqrt(db)
          vbx = vbx/db
          vby = vby/db
          dh = sqrt(dh)
          vhx = vhx/dh
          vhy = vhy/dh
c
c..... the first leg of loop B and the last leg of loop C should be in roughly
c..... direction; they also should be roughly orthogonal to the transition
c..... segment
c
          if (vcx*vbx + vcy*vby .lt. 0.7071) return
          dch = vcx*vhx + vcy*vhy
          dbh = vbx*vhx + vby*vhy
          if (dch.gt.0.7071 .or. dch.lt.-0.7071) return
          if (dbh.gt.0.7071 .or. dbh.lt.-0.7071) return
c
c..... create the first arc - between loop C and the transition segment
c
          r = sqrt(rc)
          if (iexc .gt. 0) nppc = nppc - iexc
c
c..... lcr = 1 means the arc is made smaller, and so will touch loop C at
c..... the point nppc-1
c
          if (lcr .eq. 0) then
            xc(nppc) = crx - vcx*r
            yc(nppc) = cry - vcy*r
          else
            nppc = nppc-1
          endif

          ang = acos(dch)
          rad = r/tan(ang/2)
          dang = 2*acos(1. - tol/rad)
          npts = ang/dang
          dang = ang/npts

          vx1 = -vcy
          vy1 = vcx
          if (vx1*vhx + vy1*vhy .gt. 0) then
            vx1 = -vx1
            vy1 = -vy1
          endif
          cex = xc(nppc) - rad*vx1
          cey = yc(nppc) - rad*vy1

          ang = acos(vx1)
          if (vy1 .lt. 0) ang = -ang

          vx2 = arx - cex
          vy2 = ary - cey
          dch = vx1*vy2 - vx2*vy1
          if (dch .lt. 0) dang = -dang
c
c..... lcr = 1 means the arc will touch the transition segment before its
c..... middle point
c
          npts = npts + lcr
          do k = 1, npts-1
            nppc = nppc + 1
            ang = ang + dang
            xc(nppc) = cex + rad*cos(ang)
            yc(nppc) = cey + rad*sin(ang)
          enddo

          nppc = nppc + 1
          xc(nppc) = arx
          yc(nppc) = ary
c
c..... create the second arc - between  the transition segment and loop B
c
          r = sqrt(rb)
          if (lbr .eq. 1) then

            del = dh/2 - r
            arx = arx + vhx*del
            ary = ary + vhy*del
 
            nppc = nppc + 1
            xc(nppc) = arx
            yc(nppc) = ary

          endif

          ang = acos(dbh)
          rad = r/tan(ang/2)
          dang = 2*acos(1. - tol/rad)
          npts = ang/dang
          dang = ang/npts

          vx1 = -vhy
          vy1 = vhx
          if (vx1*vbx + vy1*vby .gt. 0) then
            vx1 = -vx1
            vy1 = -vy1
          endif
          cex = arx - rad*vx1
          cey = ary - rad*vy1

          ang = acos(vx1)
          if (vy1 .lt. 0) ang = -ang

          vx2 = brx + r*vbx - cex
          vy2 = bry + r*vby - cey
          dbh = vx1*vy2 - vx2*vy1
          if (dbh .lt. 0) dang = -dang

          do k = 1, npts-1
            nppc = nppc + 1
            ang = ang + dang
            xc(nppc) = cex + rad*cos(ang)
            yc(nppc) = cey + rad*sin(ang)
          enddo
c
c.... adjust the first point of loop B to be at the end of the second arc
c
          r = r/db
          xb(1) = xb(1)*(1 - r) + xb(2+iexb)*r
          yb(1) = yb(1)*(1 - r) + yb(2+iexb)*r

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE NOTLIFT
C **  purpose of subroutine: FOR NO LIFT FIND NEXT LANE POINT AND
c **                         STORE PART OR ALL LOOP
c **    input:
C **      ISEG........INTERSECTION SEGMENT NUMBER
C **      XSEG,YSEG...INTERSECTION COORDINATES
C **      USEG........INTERSECTION RELATIVE POSITION
C **      INDEX.......INDEX OF  LOOP
C **      KLIMB.......1- CLIMB MILLING, -1- REGULAR MILLING
C **      INDEX1......CURRENT LANE POINT
C **    output:
c **      INDEX2......NEXT LANE POINT, 0- NO NEXT POINT
C **      IERR.........0 - OK, 10 - TOO MANY LOOPS
c **********************************************************************
c **********************************************************************

      SUBROUTINE NOTLIFT (INDEX,KLIMB,INDEX1,INDEX2,ierr)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'const.com'
      include 'pokcom.com'
      include 'pokseg.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts),
     x       xc(maxpts),yc(maxpts)
      real*4 dx,dy,dd,xst,yst

      integer*2 nppa,nppb,nppc,lupc,lanc,momc,laneb,lanec
      integer*2 ind1,ind2,ierr,iexb
      logical lclosd
      save xc,yc,nppc,lupc,lanc,momc,lanec,lclosd
      logical lwall
	character*80 dbuf
	byte dout(80)

      ierr = 0

      if (index1.eq.999) then
        laneb = lanes
        lanes = lanec

        if (ltrarc .eq. 1) then
c
c..... qar 95275: close the final path for arc transitions,
c..... like in toolift
c..... qar 95283: if loop C is not full, find the appropriate closing
c
          if (lclosd) then
            xst = xc(1)
            yst = yc(1)
          else
            do k = locapo,lanes
              if (lane(k).eq.lanc .and. loop(k).eq.lupc .and.
     x                                  mama(k).eq.momc) then
                CALL GETLAN (k,xa,ya,nppa)
                xst = xa(1)
                yst = ya(1)
                lclosd = .true.
                goto 10
              endif
            enddo
          endif

   10     if (lclosd) then
            dx = xst - xc(nppc)
            dy = yst - yc(nppc)
            dd = dx**2+dy**2
            if (dd .gt.1.e-6) then
              nppc = nppc+1
              xc(nppc) = xst
              yc(nppc) = yst
            endif
          endif
        endif

        CALL STOLAP (lupc,lanc,momc,xc,yc,nppc,IERR)
        lanes = laneb
        return
      endif
c
C ---- GET LOOP AND FIND POINTS ON IT
c
      CALL GETLAN (INDEX,XA,YA,NPPA)
      RPPA = NPPA
      NPPB = 0
      INDEX2 = INDEX1
      DIFF = 1000.0
      MOM = 0
      U1 = ISEG(INDEX1)+USEG(INDEX1)
      DO 27 K = 1, LOCAPO-1
        IF (ABS(MAMA(K)).EQ.INDEX .AND. LOOP(K).NE.0 .and.
     x      iseg(k).ne.0) THEN
          MOM = MOM+1
          U22 = ISEG(K)+USEG(K)
          IF (KLIMB.EQ.1) THEN
            IF (U22.GT.U1) DIFF1 = U22-U1
            IF (U22.LE.U1) DIFF1 = RPPA-U1+U22-1.0
          ELSE
            IF (U22.LT.U1) DIFF1 = -U22+U1
            IF (U22.GE.U1) DIFF1 = RPPA+U1-U22-1.0
          ENDIF
          IF (DIFF1.LT.DIFF) THEN
            DIFF = DIFF1
            U2 = U22
            INDEX2 = K
          ENDIF
        ENDIF
   27 CONTINUE

C ---- FOR REGULAR MILLING, SWITCH DIRECTION
      IF (KLIMB.EQ.1) THEN
        IND1 = INDEX1
        IND2 = INDEX2
      ELSE
        IND1 = INDEX2
        IND2 = INDEX1
      END IF

C ---- STORE LOOP OR PART OF LOOP
      ISEG1 = ISEG(IND1)
      ISEG2 = ISEG(IND2)
      U1 = ISEG(IND1)+USEG(IND1)
      U2 = ISEG(IND2)+USEG(IND2)
      NPPB = NPPB+1

      XB(NPPB) = XSEG(ind1)
      YB(NPPB) = YSEG(ind1)

      IF (U2.GT.U1) THEN
        IF (XA(ISEG2+1).EQ.FLARC) ISEG2 = ISEG2+3
        IF (ISEG1.EQ.ISEG2) GOTO 49
        DO 31 K = ISEG1+1,ISEG2
          NPPB = NPPB+1
          XB(NPPB) =  XA(K)
          YB(NPPB) =  YA(K)
   31   CONTINUE
      ENDIF
      IF (U2.LE.U1) THEN
        IF (XA(ISEG2+1).EQ.FLARC) ISEG2 = ISEG2+3
        IF (ISEG1.EQ.NPPA-1) GO TO 43
        DO 41 K = ISEG1+1,NPPA-1
          NPPB = NPPB+1
          XB(NPPB) =  XA(K)
          YB(NPPB) =  YA(K)
   41   CONTINUE
   43   DO 45 K = 1, ISEG2
          NPPB = NPPB+1
          XB(NPPB) =  XA(K)
          YB(NPPB) =  YA(K)
   45   CONTINUE
      ENDIF
   49 NPPB = NPPB+1
      XB(NPPB) = XSEG(IND2)
      YB(NPPB) = YSEG(IND2)
      IF (KLIMB.EQ.1) THEN
        INDEX1 = IND1
        INDEX2 = IND2
      ELSE
        INDEX1 = IND2
        INDEX2 = IND1
        CALL FLIPO (XB,YB,NPPB)
      ENDIF
      CALL DOUBLE (XB,YB,NPPB)

      lclosd = (ind1 .eq. ind2)

      if (rcorn.gt.0 .and. nppb+4.le.maxpts)
     x  call filcor(xb,yb,nppb,rcorn,klimb)

C ---- STORE LOOP IN COMMONS RRLN AND RRXY
      LUP = LOOP(INDEX)
      LAN = LANE(INDEX)

C ---- CHANGE TO LUP.GT.1 WITHOUT GPR ********************************
      IF (LUP.GT.0) CALL LITARC (XB,YB,NPPB)
      if (ltrarc .eq. 1) then
        iexb = 0
        lwall = .false.
        if (lanes .ge. locapo) then
          if (lane(lanes) .lt. 999) then
            npc = nppc
            call addarc (lwall,xb,yb,nppb,iexb,xc,yc,nppc)
            narcpt(lanes) = nppc - npc
	write (dbuf,7001) lanes,narcpt(lanes)
7001	format('narcpt(',i4,') = ',i4)
	call ctob (dbuf,dout)
	call nclxostr(dout)
	do i=npc+1,nppc
	write (dbuf,7002) xc(i),yc(i)
7002	format ('point/',f12.4,',',f12.4)
	call ctob (dbuf,dout)
	call nclxostr(dout)
	enddo
	call ctob ('*STOP',dout)
	call nclxostr(dout)
          endif
          laneb = lanes
          lanes = lanec
          CALL STOLAP (lupc,lanc,momc,xc,yc,nppc,IERR)
          lanes = laneb
        endif
        nppc = nppb - iexb
        lupc = lup
        lanc = lan
        momc = mom
        xc(1) = xb(1)
        yc(1) = yb(1)
        do k = 2,nppc
          xc(k) = xb(k+iexb)
          yc(k) = yb(k+iexb)
        enddo
        lanec = lanes
        lanes = lanes+1
        loca(lanes+1) = loca(lanes)+nppb
      else
        CALL STOLAP (LUP,LAN,MOM,XB,YB,NPPB,IERR)
      endif

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE NEXT
C **  purpose of subroutine: DETERMINE NEXT LOOP IN NO LIFT CUTTING
c **    input:
C **      LAN1ST...CURRENT LANE OF INDEPENDENT PATH
C **      INDEX....INDEX OF  LOOP
C **      INDEX1...PREVIOUS LANE POINT
C **      INDEX2...CURRENT LANE POINT
C **    output:
C **      LAN1ST...CURRENT LANE OF INDEPENDENT PATH
c **      INDEX3...INDEX OF NEXT LOOP
C **      INDEX4...NEW LANE POINT, 999- END OF PATH
c **********************************************************************
c **********************************************************************

      SUBROUTINE NEXT (LAN1ST,INDEX,INDEX1,INDEX2,INDEX3,INDEX4)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

C --- FOR EQUAL INDICES GO TO MAMA
      IF (INDEX1.EQ.INDEX2.OR.LOOP(INDEX2).LT.0) THEN
        INDEX3 = ABS(MAMA(INDEX))
        INDEX4 = INDEX
        IF (LOOP(INDEX3).EQ.0) GO TO 21
        GO TO 99
      END IF
      GO TO 30

C --- GOT TO LAST LOOP, LOOK FOR INDEPENDENT PATH
   21 LAN1ST = LAN1ST+1
      DO 25 K = LOCAPO, islnds+1, -1
        IF (LANE(K).EQ.LAN1ST.AND.LOOP(K).LT.0.AND.MAMA(K).GT.0) THEN
          INDEX4 = K
          MAMA(K) = -MAMA(K)
          INDEX3 = ABS(MAMA(K))
          LANES = LANES+1
          LANE(LANES) = 999
          LOOP(LANES) = 0
          MAMA(LANES) = 0
          LOCA(LANES+1) = LOCA(LANES)
          GO TO 99
        END IF
   25 CONTINUE
      INDEX4 = 999
      GO TO 99

C --- FOR UNEQUAL INDICES GO TO START OF LANE
   30 LAN = LANE(INDEX2)
      DO 35 K = LOCAPO, islnds+1, -1
        IF (LANE(K).EQ.LAN.AND.LOOP(K).LT.0.AND.MAMA(K).GT.0) THEN
          INDEX4 = K
          MAMA(K) = -MAMA(K)
          INDEX3 = ABS(MAMA(K))
          GO TO 99
        END IF
   35 CONTINUE
      INDEX3 = ABS(MAMA(INDEX))
      IF (LOOP(INDEX3).EQ.0) GO TO 21
      INDEX4 = INDEX
      IF (LOOP(INDEX).EQ.0) GO TO 21

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE TOOLF0
C **  purpose of subroutine: FOR LIFT TOOL FIND NEXT LANE POINT ON
c **                         LOOP AND calculate the LOOP
c **    input:
C **      ISEG........INTERSECTION SEGMENT NUMBER
C **      XSEG,YSEG...INTERSECTION COORDINATES
C **      USEG........INTERSECTION RELATIVE POSITION
C **      INDEX.......INDEX OF  LOOP
C **      KLIMB.......1- CLIMB MILLING, -1- REGULAR MILLING
C **      INDEX1......CURRENT LANE POINT
C **    output: none
c **********************************************************************
c **********************************************************************

      SUBROUTINE toolf0 (xb,yb,nppb,INDEX,KLIMB,INDEX1)

      include 'pokcom.com'
      include 'pokseg.com'

      real*4 XB(maxpts),YB(maxpts)
      integer*2 klimb,nppb,index,index1

      real*4 XA(maxpts),YA(maxpts)
      integer*2 k,nppa,iseg1,iseg2

        CALL GETLAN (INDEX,XA,YA,NPPA)
        NPPB = 0
c
C --- STORE LOOP
c
        ISEG1 = ISEG(INDEX1)
        ISEG2 = ISEG(INDEX1)

        NPPB = NPPB+1

        XB(NPPB) = XSEG(INDEX1)
        YB(NPPB) = YSEG(INDEX1)

        IF (XA(ISEG2+1).EQ.FLARC) ISEG2 = ISEG2+3
        IF (ISEG1.EQ.NPPA-1) GO TO 43
        DO 41 K = ISEG1+1,NPPA-1
          NPPB = NPPB+1
          XB(NPPB) =  XA(K)
          YB(NPPB) =  YA(K)
   41   CONTINUE
   43   DO 45 K = 1, ISEG2
          NPPB = NPPB+1
          XB(NPPB) =  XA(K)
          YB(NPPB) =  YA(K)
   45   CONTINUE
        NPPB = NPPB+1
        XB(NPPB) = XSEG(INDEX1)
        YB(NPPB) = YSEG(INDEX1)

        IF (KLIMB.EQ.-1) THEN
          CALL FLIPO (XB,YB,NPPB)
        ENDIF
c
C..... STORE the LOOP IN COMMONS RRLN AND RRXY
c
        CALL DOUBLE (XB,YB,NPPB)

        if (rcorn.gt.0 .and. nppb+4.le.maxpts)
     x    call filcor(xb,yb,nppb,rcorn,klimb)

       return
       end

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE TOOLIFT
C **  purpose of subroutine: FOR LIFT TOOL calculate AND STORE the current LOOP
c **    input:
C **      ISEG........INTERSECTION SEGMENT NUMBER
C **      XSEG,YSEG...INTERSECTION COORDINATES
C **      USEG........INTERSECTION RELATIVE POSITION
C **      INDEX.......INDEX OF LOOP
C **      KLIMB.......1- CLIMB MILLING, -1- REGULAR MILLING
C **      INDEX1......previous loop index
C **      INDEX2......next loop index if current-to-next transition is DOWN,
C **                  else zero
C **    output:
C **      IERR.........0 - OK, 10 - TOO MANY LOOPS
c **********************************************************************
c **********************************************************************

      SUBROUTINE TOOLIFT (INDEX,KLIMB,INDEX1,index2,ierr)

      include 'pokcom.com'
      include 'pokseg.com'

      integer*2 index,index1,index2,klimb,ierr,nppb,nppc

      real*4 XB(maxpts),YB(maxpts),xc(maxpts),yc(maxpts)

      integer*2 k,indc,iexc,lup,lan,mom,mindex2
      real*4 dx,dy,dd
      logical lwall

      data nppb /0/, nppc /0/, iexc /0/, indc /0/

      ierr = 0
      if (lanes .lt. locapo) indc = 0
c
C --- GET current LOOP
c
      if (indc .eq. index) then
        nppb = nppc - iexc
        xb(1) = xc(1)
        yb(1) = yc(1)
        do k = 2,nppb
          xb(k) = xc(k+iexc)
          yb(k) = yc(k+iexc)
        enddo
      else
        call toolf0 (xb,yb,nppb,INDEX,KLIMB,INDEX1)
      endif

      if (index2 .gt. 0) then
        call toolf0 (xc,yc,nppc,INDEX2,KLIMB,INDEX)
        indc = index2
        iexc = 0

        lwall = .false.

c
c..... set a flag if loop at index2 is near the wall
c
        mindex2 = abs(mama(index2))
        lwall = (loop(index2).eq.1 .or. loop(mindex2).eq.0)

        npc = nppb
        call addarc (lwall,xc,yc,nppc,iexc,xb,yb,nppb)
        narcpt(lanes+1) = nppb - npc

      else if (indc .eq. index) then
        dx = xb(1) - xb(nppb)
        dy = yb(1) - yb(nppb)
        dd = dx**2+dy**2
        if (dd .gt.1.e-6) then
          nppb = nppb+1
          xb(nppb) = xb(1)
          yb(nppb) = yb(1)
        endif
      endif

      LUP = LOOP(INDEX)
      LAN = LANE(INDEX)
      MOM = 1
c
C..... CHANGE TO LUP.GT.1 WITHOUT GPR
c
      IF (LUP.GT.0) CALL LITARC (XB,YB,NPPB)
      CALL STOLAP (LUP,LAN,MOM,XB,YB,NPPB,IERR)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE pastrt(index)
C **  purpose of subroutine: for an exit loop move the final point forward
C **    to create a small overlap (in order to avoid dwell marks)
c **    input:
C **      INDEX......CURRENT LANE number
C **    output: none
c **********************************************************************
c **********************************************************************

      SUBROUTINE pastrt(index)

      include 'com.com'
      include 'pokcom.com'

      real*4 dx,dy,d1
      integer*4 ier
      integer*4 kk
      real*8 xxs,yys,xx1,yy1,xxe,yye


        kk = loca(index)
        call rrget(kk,xxs,yys,ier)
        if (ier .ne. 0) return
        kk = kk+1
        call rrget(kk,xx1,yy1,ier)
        if (ier.ne.0 .or. xx1.gt.0.9*flarc) return

        dx = xx1 - xxs
        dy = yy1 - yys
        D1 = SQRT(dx*dx+dy*dy)
        if (d1 .lt. 10.*tol) return

        d1 = (5.*tol)/d1

        kk = loca(index+1) - 1
        call rrget(kk,xxe,yye,ier)
        if (ier .ne. 0) return

        kk = kk
        xxe = xxe + dx*d1
        yye = yye + dy*d1
        call rrplce (kk,xxe,yye,ier)
        if (ier .ne. 0) return

       if (ifl(264).eq.0) then
          xxs = xxs/25.4
          yys = yys/25.4
          xxe = xxe/25.4
          yye = yye/25.4
       endif

       return
       end

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE CUTLOOK
C **  purpose of subroutine: FIND INTERSECTION POINTS ON LANE ONE FOR
c **                         END POINT
c **    input:
C **      STEP........STEP BETWEEN LOOPS
C **      KLIMB.......-1 - CLIMB MILLING, LEFT, +1 - REGULAR, RIGHT
C **    output:
c **      ISEG........INTERSECTION SEGMENT NUMBER
C **      XSEG,YSEG...INTERSECTION COORDINATES
C **      USEG........INTERSECTION RELATIVE POSITION
C **      IERR........0 - OK, ELSE - ERROR
c **********************************************************************
c **********************************************************************

      SUBROUTINE CUTLOOK (STEP,KLIMB,IERR)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'
      include 'pokseg.com'

      real*4 XA(maxpts),YA(maxpts)

      IERR = 0
      MANOR = 0
      IF (LANES.GT.maxseg) THEN
        IERR = 10
        GOTO 90
      ENDIF

C --- FIND LANE TO COMPUTE PATH INTERSECTIONS TO
      K = LANES+1
   15 K = K-1
      IF (K.EQ.0) GO TO 90
      IF (LANE(K).EQ.1.AND.LOOP(K).LT.0) THEN
        INDEX1 = K
        INDEM = MAMA(K)
        LAN1 = LANE(INDEM)
        LUP1 = LOOP(INDEM)
        GOTO 20
      ENDIF
      GOTO 15
   20 CALL GETLAN (INDEM,XA,YA,NPPA)

C --- FIND LINE CONNECTING END POINT TO CENTER LOOP
      CALL CLOSEST (XA,YA,NPPA,LSEG,UINT,X1,Y1,DIS)
      ISEG(INDEX1) = LSEG
      XSEG(INDEX1) = X1
      YSEG(INDEX1) = Y1
      USEG(INDEX1) = UINT
      X2 = XLOOK
      Y2 = YLOOK

C --- FOR LANE 1 CUT THROUGH REST OF LOOPS
      INDEX = INDEX1
   30 INDEXO = INDEX
      INDEX = MAMA(INDEX)
      INDEM = MAMA(INDEX)
      LAN = LANE(INDEM)
      IF (LOOP(INDEM).EQ.0) THEN
        ISEG(INDEX) = 0
        XSEG(INDEX) = 0.0
        YSEG(INDEX) = 0.0
        USEG(INDEX) = 0.0
        GOTO 90
      ENDIF
      CALL CUTREST (INDEM,X1,Y1,X2,Y2,MANOR,NORM,XINT,YINT,UINT,KSEG)
      ISEG(INDEX) = KSEG
      XSEG(INDEX) = XINT
      YSEG(INDEX) = YINT
      USEG(INDEX) = UINT
      DX = XSEG(INDEX)-XSEG(INDEXO)
      DY = YSEG(INDEX)-YSEG(INDEXO)
      DD = DX*DX+DY*DY
      IF (DD.GT.25.01*STEP*STEP.OR.NORM.EQ.0) THEN
        IERR = 12
        GOTO 90
      ENDIF
      GOTO 30
   90 continue

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE findu
C **  Purpose of subroutine: Find a segment and u value to connect a
C **                        loop with its MAMA.
c **    input:
C **      INDEM...... Index to first mama
C **      XA,YA...... Loop to find segment and u value in.
C **      NPPA....... Number of points in loop.
C **      NPA........ Number of loops in 'PAPA' array.
C **      IPAPA...... Additional candidates to be mama.
C **      STEP....... Step over distance between loops.
C **    output:
C **      USEG........Intersection relative position
c **      LSEG........Intersection segment number, or zero if none
C **
c **********************************************************************
c **********************************************************************

      subroutine findu (indem,xa,ya,nppa,npa,ipapa,step,useg,lseg)

      include 'pokcom.com'

      integer*2 indem,nppa,npa,lseg
      real*4 XA(maxpts),YA(maxpts)
      integer*4 IPAPA(50)
      real*4 step, useg

      integer*2 kpa, ixa, ida, ixb, idb
      real*8 x1,x2,x3,x4,y1,y2,y3,y4,dx1,dx3,dy1,dy3
      real*8 u1,u2,u3,u4,eps,s1,s3,d1,d2,co
      logical l1,l2,l3,l4
      integer*2 nppb
      real*4 xb(maxpts),yb(maxpts)

c     EPS = tol/25400.
c
c..... Eduard 4/28/1999. Replaced the old value of EPS.
c
      eps = tol/2.0
      lseg = 0
      do 100 kpa=0,npa
c
c...  Get the first or next mama
c
        if (kpa.gt.0) indem=ipapa(kpa)
        call getlan (indem,xb,yb,nppb)
c
c... Loop through each segment of input loop.
c
        ixa = 1
        ida = 0
20      ixa = ixa+ida
        if (ixa.ge.nppa) goto 100
        ida = 1
        if (xa(ixa+1).eq.flarc) ida=4
        x1 = xa(ixa)
        y1 = ya(ixa)
        x2 = xa(ixa+ida)
        y2 = ya(ixa+ida)
        dx1 = x2-x1
        dy1 = y2-y1
        s1 = dx1**2+dy1**2
        if (s1.lt.1.e-7) goto 20
        d1 = dsqrt(s1)
c
c...  For this segment, loop through each segment of mama to find one
c...  which is parallel, close and whose points project onto this
c...  segment.
c
        ixb = 1
        idb = 0
30      ixb = ixb+idb
        if (ixb.ge.nppb) goto 20
        idb = 1
        if (xb(ixb+1).eq.flarc) idb=4
        x3 = xb(ixb)
        y3 = yb(ixb)
        x4 = xb(ixb+idb)
        y4 = yb(ixb+idb)
        dx3 = x4-x3
        dy3 = y4-y3
        s3 = dx3**2+dy3**2
        if (s3.lt.1.e-7) goto 30
        co = (dx1*dx3+dy1*dy3)/dsqrt(s3)/d1
        if (dabs(co).lt.1.0-EPS) goto 30
c
c...  Parallel
c
        d2 = dabs((x3-x1)*dy1-(y3-y1)*dx1)/d1
        if (d2.gt.2.01*step) goto 30
c
c...  CLose
c
        u1 = ((x3-x1)*dx1+(y3-y1)*dy1)/s1
        u2 = ((x4-x1)*dx1+(y4-y1)*dy1)/s1
        u3 = ((x1-x3)*dx3+(y1-y3)*dy3)/s3
        u4 = ((x2-x3)*dx3+(y2-y3)*dy3)/s3
        l1 = u1.ge.0.0 .and. u1.le.1.0
        l2 = u2.ge.0.0 .and. u2.le.1.0
        l3 = u3.ge.0.0 .and. u3.le.1.0
        l4 = u4.ge.0.0 .and. u4.le.1.0
        if (.not.l1 .and. .not.l2 .and. .not.l3 .and. .not.l4) goto 30
c
c...  End points within range of input segment. Calculate best u value
c...  on inner segment. (Only the first case - both end points of the
c...  outer (mama) segment lie within the inner segment - seems possible
c...  but test for them all just in case.
c
        if (l1) then
          if (l2) then
            useg = (u1+u2)/2.0
            goto 200
          endif
          if (l3) then
            useg = u1/2.0
            goto 200
          endif
          if (l4) then
            useg = (u1+1.0)/2.0
            goto 200
          endif
          goto 30
        endif
        if (l2) then
          if (l3) then
            useg = u2/2.0
            goto 200
          endif
          if (l4) then
            useg = (u2+1.0)/2.0
            goto 200
          endif
          goto 30
        endif
        goto 30
100     continue
        goto 999
200     continue
        if (useg.lt.0.0) useg = 0.0
        if (useg.gt.1.0) useg = 1.0
        lseg = ixa
999     return
        end

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE mvbyj
C **  Purpose of subroutine: Move the loop points forward by j
c **    input:
C **      INDEX...... Index to start
C **      XA,YA...... Loop
C **      NPPA....... Number of points in loop.
C **    output:
C **      XA,YA...... New loop
c **********************************************************************
c **********************************************************************

      subroutine mvbyj (xa,ya,index,nppa,j)

      include 'pokcom.com'

      integer*2 index,nppa,j
      real*4 XA(maxpts),YA(maxpts),XT(maxpts),YT(maxpts)
      integer*2 k

      if (j .le. 0) return

      do k = index,nppa
        xt(k-index+1) = xa(k)
        yt(k-index+1) = ya(k)
      enddo

      do k = index,nppa
        xa(k+j) = xt(k-index+1)
        ya(k+j) = yt(k-index+1)
      enddo

      return
      end

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE arcrange
C **  Purpose of subroutine: check if an arcpoint is inside the arc
c **    input:
C **      vx0,vy0.... arc start vector
C **      vx1,vy1.... arc end vector
C **      vxc,vyc.... vector from the center to the arc point
C **      rad    .... arc radius
C **      klok    ... arc direction (1 if CCW, -1 if CW)
C **    output:
C **      in......... 0 if the point is outside the arc; 1 if inside
c **********************************************************************
c **********************************************************************

      subroutine arcrange (vx0,vy0,vx1,vy1,vxc,vyc,klok,rad,in)

      include 'pokcom.com'

      integer*2 klok
      real*4 vx0,vy0,vx1,vy1,vxc,vyc,rad

      real*4 d0,d1
      integer*2 in

      d  = (vx0*vy1 - vy0*vx1)*klok
      d0 = (vx0*vyc - vy0*vxc)*klok
      d1 = (vxc*vy1 - vyc*vx1)*klok

      in = 0
      if (d - d0 .gt. 0 .and.
     x    d0 .ge. rad*tol .and. d1 .ge. rad*tol) in = 1

      return
      end

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE getnxtleg
C **  Purpose of subroutine: Get the current arc/segment data for filcor;
C **  get the unit vector at an endpoint
c **    input:
C **      XB,YB...... Loop
C **      NPPB....... Number of points in loop.
C **      it..........1- 1ST endpoint (for the second leg of a corner),
C **                  2- 2ND endpoint (for the first leg of a corner)
C **      k.......... starting index.
C **    output:
C **      k1.................. ending index.
C **      XC1,YC1,R1,KLOK..... CENTER POINTS, RADIUS AND DIRECTION for an arc
C **      vx1,vy1..............VECTOR AT END
C **      d1 ................. length for a segment.
c **********************************************************************
c **********************************************************************

      subroutine getnxtleg (it,xb,yb,nppb,k,k1,klok,xc1,yc1,r1,
     x                      vx1,vy1,d1)

      include 'pokcom.com'

      integer*2 it,nppb,k,k1,klok
      real*4 xb(maxpts),yb(maxpts),xc1,yc1,r1,vx1,vy1,d1

      klok = 0
      d1 = 0
      k1 = k+1
      if (k1 .gt. nppb) return

      if (xb(k1) .eq. flarc) then
        k1 = k+4
        if (k1 .gt. nppb) return
        r1 = xb(k+2)
        klok = yb(k+2)
        xc1 = xb(k+3)
        yc1 = yb(k+3)
        call tanarc (it,xc1,yc1,r1,klok,xb(k),yb(k),xb(k1),yb(k1),
     x               vx1,vy1)
      else
        vx1 = xb(k1) - xb(k)
        vy1 = yb(k1) - yb(k)
        d1 = sqrt(vx1**2 + vy1**2)
        if (d1 .gt. tol) then
          vx1 = vx1/d1
          vy1 = vy1/d1
        else
          vx1 = 0
          vy1 = 0
        endif
      endif

      return
      end

c **********************************************************************
c **********************************************************************
c **    SUBROUTINE filcor
C **  Purpose of subroutine: Put fillets on inside corners
c **    input:
C **      XB,YB...... Loop
C **      NPPB....... Number of points in loop.
C **      klimb...... 1 if ccw, -1 if cw; 0 iff lace pocketing
C **      rcorn...... radius of corner fillet.
C **    output:
C **      XB,YB...... New loop
c **********************************************************************
c **********************************************************************

      subroutine filcor(xb,yb,nppb,rcr,klimb)

      include 'com.com'
      include 'pokcom.com'

      integer*2 klimb,nppb
      real*4 xb(maxpts),yb(maxpts),rcr

      integer*2 j,k,k1,k11,k2,kl1,kl2,ifil,nint,in1,in2,k0
      real*4 vx0,vy0,vx1,vy1,vxc,vyc,r1,r2,rad
      real*4 co,si,t,tsq,cx,cy,xc1,yc1,xc2,yc2,tolsq
      real*4 vx2,vy2,d1,d2,vxe,vye,vxt,vyt,xa,ya
      real*4 dsq,vxca,vyca,adrad
      logical lback,lv97
     
        if (nppb .lt. 3) return
        tolsq = tol*tol
        k0 = 0
        k = 1   
        
        lv97 = sc(169) .lt. 9.75d0
c
c.....calcuate stpcur for adjusting arc corner
c
        if (.not. lv97 .and. rcr .gt. 0.0 .and. lacefl.eq.0) then
            call xsharp (xb,yb,nppb,stpmax,stpcur)
            if (rcr .gt. stpmin) rcr = stpmin
        endif     
c
c..... notation: [k...k1] is the first leg, [k11...k2] is the second leg
c
10      ifil = 0
        kccw = klimb
        rad = rcr
        lback = .false.
        call getnxtleg (2,xb,yb,nppb,k,k1,kl1,xc1,yc1,r1,vx1,vy1,d1)
        if (k1 .ge. nppb) return
        k11 = k1
20      call getnxtleg (1,xb,yb,nppb,k11,k2,kl2,xc2,yc2,r2,vx2,vy2,d2)
        if (k2 .gt. nppb) return
25      si = -vy1*vx2 + vx1*vy2
        if (klimb .eq. 0) then
          kccw = 1
          if (si. lt. 0) kccw = -1
        endif
        si = si*kccw
        co = vx1*vx2 + vy1*vy2     
        if (si.gt.0 .and. abs(co).lt.0.866 ) then
          in1 = 1
          in2 = 1
c
c..... have an inside corner. try to fit an arc
c
          if (kl1.ne.0 .and. kl2.eq.0) then
c
c..... arc - segment corner
c
            cx = xb(k2) - kccw*vy2*rcr
            cy = yb(k2) + kccw*vx2*rcr
            vxt = cx - xc1
            vyt = cy - yc1
            co = vxt*vx2 + vyt*vy2
            si = vxt*vy2 - vyt*vx2
            d1 = r1 + rcr
            t = co - sqrt(d1**2 - si**2)
            cx = cx - t*vx2
            cy = cy - t*vy2
            vx0 = xb(k) - xc1
            vy0 = yb(k) - yc1
            vxe = xb(k1) - xc1
            vye = yb(k1) - yc1
            vxc = (cx - xc1)*r1/d1
            vyc = (cy - yc1)*r1/d1
c
c..... check that the new end point on the 1st arc is in the range
c
            call arcrange (vx0,vy0,vxe,vye,vxc,vyc,kl1,r1,in1)
            if (t .le. tol) in2 = 0
            if (in1.eq.1 .and. in2.eq.1) then
            x1 = xc1 + vxc
            y1 = yc1 + vyc
            x2 = xb(k2) - t*vx2
            y2 = yb(k2) - t*vy2
            ifil = 1
            endif
          else if (kl1.eq.0 .and. kl2.ne.0) then
c
c..... segment - arc corner
c
            cx = xb(k) - kccw*vy1*rcr
            cy = yb(k) + kccw*vx1*rcr
            vxt = cx - xc2
            vyt = cy - yc2
            co = vx1*vxt + vy1*vyt
            si = vx1*vyt - vy1*vxt
            d2 = r2 + rcr
            t = sqrt(d2**2 - si**2)
            t = -co - t
            if (t .le. tol) in1 = 0

              cx = cx + t*vx1
              cy = cy + t*vy1
              vx0 = xb(k11) - xc2
              vy0 = yb(k11) - yc2
              vxe = xb(k2) - xc2
              vye = yb(k2) - yc2
              vxc = (cx - xc2)*r2/d2
              vyc = (cy - yc2)*r2/d2
c
c..... check that the new end point on the 2nd arc is in the range
c
            call arcrange (vx0,vy0,vxe,vye,vxc,vyc,kl2,r2,in2)
            if (in1.eq.1 .and. in2.eq.1) then
              x1 = cx + kccw*vy1*rcr
              y1 = cy - kccw*vx1*rcr
              x2 = xc2 + vxc
              y2 = yc2 + vyc
              ifil = 1
            endif
          else if (kl1.ne.0 .and. kl2.ne.0) then
c
c..... arc - arc corner
c
            d1 = r1 + rcr
            d2 = r2 + rcr
            vxc = xc2 - xc1
            vyc = yc2 - yc1
            tsq = vxc**2 + vyc**2
            t = sqrt(tsq)
            vxc = vxc/t
            vyc = vyc/t
            t = (tsq + d1**2 - d2**2)/(2*t)
            cx = xc1 + t*vxc
            cy = yc1 + t*vyc
            vxt = -vyc
            vyt = vxc
            if (vxt*(xb(k1)-cx)  + vyt*(yb(k1)-cy) .lt. 0) then
              vxt = -vxt
              vyt = -vyt
            endif
            t = sqrt(d1**2 - t**2)
            cx = cx + t*vxt
            cy = cy + t*vyt
            vx0 = xb(k) - xc1
            vy0 = yb(k) - yc1
            vxe = xb(k1) - xc1
            vye = yb(k1) - yc1
            vxc = (cx - xc1)*r1/d1
            vyc = (cy - yc1)*r1/d1
c
c..... check that the new end point on the 1st arc is in the range
c
            call arcrange (vx0,vy0,vxe,vye,vxc,vyc,kl1,r1,in1)

            x1 = xc1 + vxc
            y1 = yc1 + vyc
            vx0 = xb(k11) - xc2
            vy0 = yb(k11) - yc2
            vxe = xb(k2) - xc2
            vye = yb(k2) - yc2
            vxc = (cx - xc2)*r2/d2
            vyc = (cy - yc2)*r2/d2
c
c..... check that the new end point on the 2nd arc is in the range
c
            call arcrange (vx0,vy0,vxe,vye,vxc,vyc,kl2,r2,in2)
            if (in1.eq.1 .and. in2.eq.1) then
            x2 = xc2 + vxc
            y2 = yc2 + vyc
            ifil = 1
            endif
          else
c
c..... segment - segment corner
c
            co = sqrt((1-co)/(1+co))
            t = rcr*co
            if (klimb .eq. 0) then
c
c..... if scrub pocketing and the specified radius is too big
c..... we use a radius that fits
c
              r1 = d1
              if (d2 .lt. d1) r1 = d2
              if (t.ge.(r1-tol) .and. r1.gt.5*tol) then
                t = r1
                rad = t/co
              endif
            endif

            if (lv97) then
               if (t .ge. (d1-tol)) in1 = 0
               if (t .ge. (d2-tol)) in2 = 0
            else
               if (t .ge. (d1-tol)) then
                  rad = rad * d1 * 0.25 / t
                  t = rad * co
               endif
               if (t .ge.(d2-tol)) then
                  rad = rad * d2 * 0.25 / t
                  t = rad * co
               endif
            endif

            if (in1.eq.1 .and. in2.eq.1) then
c
c..... get corner point (xa,ya)
c
              xa = xb(k1)
              ya = yb(k1)
              if (k1 .ne. k11) then
                CALL LINEX (1,xb(k),yb(k),xb(k1),yb(k1),
     x                      xb(k11),yb(k11),xb(k2),yb(k2),xa,ya,nint)
                if (nint .ne. 1) goto 30
              endif
c
c..... get fillet arc center (cx,cy) and tangencies (x1,y1), (x2,y2)
c
              x1 = xa - vx1*t
              y1 = ya - vy1*t
              xc1 = x1 - kccw*vy1*rad
              yc1 = y1 + kccw*vx1*rad
              x2 = xa + vx2*t
              y2 = ya + vy2*t
              xc2 = x2 - kccw*vy2*rad
              yc2 = y2 + kccw*vx2*rad
              vxc = xc2 - xc1
              vyc = yc2 - yc1
              tsq = vxc**2 + vyc**2
              if (tsq .lt. tolsq) then 
                cx = (xc2 + xc1)/2
                cy = (yc2 + yc1)/2
                disca = sqrt((cx - xa)**2 + (cy - ya)**2)
c
c.....Check the corner point and arc center distance                
c
                if (.not. lv97 .and. rad .lt. stpcov) then
c
c.....Move arc center to adjust arc radius for full converage
c
                    vxca = xa - cx
                    vyca = ya - cy
                    vxca = vxca / disca
                    vyca = vyca / disca
                    
                    if (rad .lt. stpcur) then
c
c.....Calculate distance between corner and next corner point
c
                        discnc = disca * stpcur / rad
c
c....Adjusted radius
c 
                        adrad = (stpmin - (discnc-2.0*stpmin))/2.0
                        if (adrad .lt. 0.0) adrad = 10.5 * tol
                        if (adrad .gt. rad) goto 50
c
c....Adjusted arc
c          
                        x1 = xa - vx1*adrad*co
                        y1 = ya - vy1*adrad*co        
                        xc1 = x1 - kccw*vy1*adrad
                        yc1 = y1 + kccw*vx1*adrad
                        x2 = xa + vx2*adrad*co  
                        y2 = ya + vy2*adrad*co
                        xc2 = x2 - kccw*vy2*adrad
                        yc2 = y2 + kccw*vx2*adrad
                        vxc = xc2 - xc1
                        vyc = yc2 - yc1
                        tsq = vxc**2 + vyc**2
                        if (tsq .lt. tolsq) then 
                          cx = (xc2 + xc1)/2
                          cy = (yc2 + yc1)/2
                          rad = adrad
                        endif          
                    endif
                endif
50              ifil = 1
              endif
            endif
          endif
          if (in1.eq.0 .or. in2.eq.0) then
            if (k11 .ne. k1) goto 30
            if (in2 .eq. 0) then
c
c..... if the second leg is short, try to use the next leg
c
              k11 = k2
              goto 20
            else if (in1 .eq. 0) then
c
c..... if the first leg is short, try to use the previous leg
c
              lback = .true.
              k = k0
              call getnxtleg (2,xb,yb,nppb,k,k1,kl1,xc1,yc1,r1,
     x                                                vx1,vy1,d1)
              if (k1 .ge. nppb) return
              goto 25
            endif
          endif
        endif
        if (ifil .eq. 1) then
          j = 4 - k11 + k1
          call mvbyj (xb,yb,k11+1,nppb,j)
          xb(k1) = x1
          yb(k1) = y1
          xb(k1+1) = flarc
          yb(k1+1) = 0
          xb(k1+2) = rad
          yb(k1+2) = kccw
          xb(k1+3) = cx
          yb(k1+3) = cy
          xb(k1+4) = x2
          yb(k1+4) = y2
          k0 = k1
          k = k1+4
          nppb = nppb+j
          goto 40
        endif
30      k0 = k
        k = k1
        if (lback) k = k11
40      if (k+1.lt.nppb .and. nppb+4.le.maxpts) goto 10

      return
      end

c **********************************************************************
c **********************************************************************
c **    subroutine name:  chkgou
c **  purpose of subroutine: check if a helix or a ramp move gouges
c **                         pocket geometry.
c **    input:
c **      ientry          - ramp if 0, arc or helix else
c **      a0,b0,a1,b1     - start and end points
c **      ix              - number of pocket geometry element to check
c **      ro              - arc radius
c **      idbx            - output debug info
c **    output:
c **      jcut = 0, if no intersections, else jcut = 1
c **      ier - errors from rrget
c **
c **********************************************************************
c **********************************************************************

      subroutine chkgou (ientry,ix,ro,a0,b0,a1,b1,jcut,ier,idbx)

      include 'com4a.com'
      include 'rrdm.com'
      include 'pokcom.com'

      integer*2 ientry,ix,jcut,idbx
      integer*4 ier
      real*4 a0,b0,a1,b1,ro

      integer*4 jj
      integer*2 klok
      real*8 x0,y0,x1,y1
      real*4 rr,xc,yc,tolsv,aa0,bb0,aa1,bb1

      character*80 dbuf
      byte dout(80)

      tolsv = tol
      tol = sc(27)
      if (ifl(264).eq.0) tol = tol*25.4

      aa0 = a0
      bb0 = b0
      aa1 = a1
      bb1 = b1
      if (lacefl.gt.0 .and. lctflg) then
        call lctrans (aa0,bb0,-1)
        call lctrans (aa1,bb1,-1)
      endif

      jj = loca(ix)
      call rrget (jj,x0,y0,ier)
      if (ier .ne. 0) goto 99

215     if (jj .gt. loca(ix)) then
          x0 = x1
          y0 = y1
        endif
        jj = jj + 1
        call rrget (jj,x1,y1,ier)
        if (ier .ne. 0) goto 99
        if (x1 .lt. 0.9*flarc) then
          if (ientry .gt. 0) then
            call chksg1(aa0,bb0,ro,x0,y0,x1,y1,jcut)
          else
            call chkseg(aa0,bb0,aa1,bb1,ro,x0,y0,x1,y1,jcut)
          endif
        else
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 99
          rr = x1
          klok = y1
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 99
          xc = x1
          yc = y1
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 99
          if (ientry .gt. 0) then
            call chkrc1(aa0,bb0,ro,x0,y0,x1,y1,rr,xc,yc,klok,jcut)
          else
            call chkarc(aa0,bb0,aa1,bb1,ro,x0,y0,x1,y1,
     x                                       rr,xc,yc,klok,jcut)
          endif
        endif

        if (jcut .eq. 1) goto 99
        if (jj+1 .lt. loca(ix+1)) goto 215

99    tol = tolsv
c
c...Output debug information on gouge
c...The violated entity is output in orange
c
      if (idbx .eq. 1 .and. jcut .eq. 1) then
        kk = jj
        jj = loca(ix)
        call rrget (jj,x0,y0,ier)
        if (ier .ne. 0) goto 999
        call ctob ('ERASE/all',dout)
        call nclxostr(dout)
        call ctob ('DRAFT/MODIFY,COLOR=CYAN',dout)
        call nclxostr(dout)
        if (ientry .gt. 0) then
          write (dbuf,7002) aa0,bb0
 7002     format ('POINT/',2(f12.4,','),f12.4)
        else
          write (dbuf,7000) aa0,bb0,aa1,bb1
 7000     format ('LINE/',3(f12.4,','),f12.4)
        endif
        call ctob (dbuf,dout)
        call nclxostr(dout)
c
        call ctob ('DRAFT/MODIFY,COLOR=CYAN',dout)
        call nclxostr(dout)
        write (dbuf,7001) aa0,bb0,ro
 7001   format ('CIRCLE/',2(f12.4,','),f12.4)
        call ctob (dbuf,dout)
        call nclxostr(dout)
c
        if (ientry .le. 0) then
          call ctob ('DRAFT/MODIFY,COLOR=MAGNTA',dout)
          call nclxostr(dout)
          write (dbuf,7001) aa1,bb1,ro
          call ctob (dbuf,dout)
          call nclxostr(dout)
        endif
c
        call ctob ('DRAFT/MODIFY,COLOR=GREEN',dout)
        call nclxostr(dout)
c
  991   if (jj .gt. loca(ix)) then
          x0 = x1
          y0 = y1
        endif
        jj = jj + 1
        call rrget (jj,x1,y1,ier)
        if (ier .ne. 0) goto 999
        if (jj .eq. kk) then
          call ctob ('DRAFT/MODIFY,COLOR=ORANGE',dout)
          call nclxostr(dout)
        endif
        if (x1 .lt. 0.9*flarc) then
          write (dbuf,7000) x0,y0,x1,y1
          call ctob (dbuf,dout)
          call nclxostr(dout)
        else
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 999
          rr = x1
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 999
          xc = x1
          yc = y1
          jj = jj + 1
          call rrget (jj,x1,y1,ier)
          if (ier .ne. 0) goto 999
          if (jj .ne. kk) then
            call ctob ('DRAFT/MODIFY,COLOR=BLUE',dout)
            call nclxostr(dout)
          endif
          write (dbuf,7001) xc,yc,rr
          call ctob (dbuf,dout)
          call nclxostr(dout)
          call ctob ('DRAFT/MODIFY,COLOR=GREEN',dout)
          call nclxostr(dout)
        endif
        if (jj .eq. kk) then
          call ctob ('DRAFT/MODIFY,COLOR=GREEN',dout)
          call nclxostr(dout)
        endif
  999   continue
        if (ier .ne. 0) then
          call ctob ('ERR=1',dout)
          call nclxostr(dout)
        endif

        if (jj+1 .lt. loca(ix+1)) goto 991
        call ctob ('*STOP',dout)
        call nclxostr(dout)
      endif

      return
      end
