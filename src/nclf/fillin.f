C*********************************************************************
C*    NAME         :  fillin.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*        fillin.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:03
C********************************************************************

C*************************************************************
C*
C*     SUBROUTINE:  fillin
C*
C*     PURPOSE:     Determines the number of points needed
C*                  in the fillet and puts the points into
C*                  the clpt array.
C*
C*
C************************************************************
      SUBROUTINE FILLIN (SANG,FANG,RAD1,CBUF,SPOINT,FPOINT,WRKPTS)
 
      include 'com8a.com'
      include 'const.com'
      INCLUDE 'fillet.com'
 
      REAL*8 AXSPAN,SANG,FANG,CBUF(7),dang,thgt,dtol,
     1       RAD1,DLTTAB,SANG1,ISPAN,JSPAN,KSPAN,SPOINT(6),
     2       FPOINT(6),WRKPTS(6,4),IVAL,JVAL,KVAL,VL
      REAL*8 PT(3), U, DU, OFFD, maxd, fp(6), sp(6)
      INTEGER*2 IFLG, INEG, MMSAV, ITIMES, I2, J, ICNT, IISAV
c
      logical idid,lv91
 
      MMSAV  = MXCL
      INEG   = 1
      lv91 = sc(169) .lt. 9.15d0
C
C...DECIDE IF MOVE SHOULD BE LINEARIZED
C
      DLTTAB = FANG-SANG
      IF (DABS(DLTTAB) .LE. 179.) GO TO 100
      INEG   = -1
      IF (DLTTAB .LT. 0) INEG = 1
      DLTTAB = 360. - DABS (DLTTAB)
  100 call pchkpts (sang,fang,filrad,itimes)
C
C...  IF NO LINEARIZATION THEN OUTPUT FPOINT
C
 9990 IF (ITIMES .LE. 0) THEN
          DO 9995 I2=1,6,1
              CLPT(I2,1) = FPOINT(I2)
 9995     CONTINUE
          MXCL  = 1
          GO TO 400
      ENDIF
C
C...GET ANGLE SPAN
C
      AXSPAN = (DLTTAB / ITIMES) * INEG
      ISPAN  = (FPOINT(4)-SPOINT(4))/ITIMES
      JSPAN  = (FPOINT(5)-SPOINT(5))/ITIMES
      KSPAN  = (FPOINT(6)-SPOINT(6))/ITIMES
C
      IVAL   = SPOINT(4)
      JVAL   = SPOINT(5)
      KVAL   = SPOINT(6)
C
C...GET NEW COORDINATES
C
      ICNT = ITIMES
      U    = 0.d0
      DU   = ICNT
      IF (DU .LE. 0.D0) DU = 1.D0
      DU   = 1.D0 / DU
      dang = AXSPAN/RADIAN
      SANG1 = SANG/RADIAN
      DO 150,J=1,ICNT,1
          SANG1 = SANG1 + dang
          CLPT(1,J)  = CBUF(1) + (DCOS(SANG1) * RAD1)
          CLPT(2,J)  = CBUF(2) + (DSIN(SANG1) * RAD1)
          CLPT(3,J)  = CBUF(3)
  150 CONTINUE
C
C... Create points on a B-Spline curve above the fillet plane for
c... the tool axis to pass through. Choose arbitrary distance above plane
c... big enough to get reasonable accuracy and small enough to not run
c... into problems with tool axis vectors crossing
C
      iflg = 1
      if (ifl(82).eq.1 .and. .not. lflsam .and. IFILTA .ne. 1 .and.
     1    .not. lv91) then
        offd = cbuf(7) / 2.D0
cc        if (offd .gt. SC(27)*100) offd = sc(27)*100
        maxd = .1
        if (ifl(264) .eq. 1 .or. ifl(362) .eq. 1) maxd = 2.54
        dtol = 0.01 * maxd
        if (offd .gt. maxd) offd = maxd

        thgt = sc(30)

        call ncl_filpts(spoint,wrkpts(1,1),wrkpts(1,2),wrkpts(1,3),
     1      wrkpts(1,4),fpoint,offd,thgt,clpt,icnt,dtol,iflg)
      endif


      DO 300,J=1,ICNT,1
          if (iflg .eq. 0) then
c
c... create vector through curve above plane of fillet
c
             u = u + du
             call ncl_filcvpt(u,pt)
             CLPT(4,j)  = pt(1) - CLPT(1,j)
             CLPT(5,j)  = pt(2) - CLPT(2,j)
             CLPT(6,j)  = pt(3) - CLPT(3,j)
          else
             CLPT(4,J)  = IVAL + ISPAN
             CLPT(5,J)  = JVAL + JSPAN
             CLPT(6,J)  = KVAL + KSPAN
          endif
C
C... UNITIZE VECTORS
C
          VL   = DSQRT(CLPT(4,J)**2 + CLPT(5,J)**2 + CLPT(6,J)**2)
          CLPT(4,J)  =  CLPT(4,J)  /  VL
          CLPT(5,J)  =  CLPT(5,J)  /  VL
          CLPT(6,J)  =  CLPT(6,J)  /  VL
C
          IVAL = IVAL + ISPAN
          JVAL = JVAL + JSPAN
          KVAL = KVAL + KSPAN
  300 CONTINUE
C
      MXCL  = ICNT
C
  400 IISAV  = IROTPT
      IF (IROTPT .NE. 99) IROTPT = 0
C
C...Rotate Circular points back to part system
C
  900 CALL FILADJ
c
c...Fix any round off problems
c
      if (iflg .eq. 0) then
          do 950 i=1,6,1
              sp(i) = spoint(i)
              fp(i) = fpoint(i)
  950     continue
          call rotpts (sp,ierr)
          call rotpts (fp,ierr)
          idid = .false.
          do 970 i=4,6,1
              if (dabs(sp(i)-fp(i)) .lt. .000001) then
                  idid = .true.
                  do 960 j=1,mxcl,1
                      clpt(i,j) = sp(i)
  960             continue
              endif
  970     continue
          if (idid) then
              do 980 j=1,mxcl,1
                  VL = DSQRT(CLPT(4,J)**2 + CLPT(5,J)**2 + CLPT(6,J)**2)
                  CLPT(4,J)  =  CLPT(4,J)  /  VL
                  CLPT(5,J)  =  CLPT(5,J)  /  VL
                  CLPT(6,J)  =  CLPT(6,J)  /  VL
  980         continue
          endif
      endif
c
c...ARCSLP/FILLET,LOCK
c...Keep fixed tool axis
c
      if (lflsam .and. ifl(82) .eq. 1) then
         do 990 j=1,icnt,1
             CLPT(4,j) = FILAXS(1)
             CLPT(5,j) = FILAXS(2)
             CLPT(6,j) = FILAXS(3)
  990    continue
      endif
      IROTPT = IISAV
C
C...RESET FEDRAT IF FORWARD
C
 9999 MXCL   = MMSAV
      IROTPT = 99
      RETURN
      END
