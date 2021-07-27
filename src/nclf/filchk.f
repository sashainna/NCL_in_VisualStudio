C*********************************************************************
C*    NAME         :  filchk.f
C*       CONTAINS:
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
C*    All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*        filchk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:02
C*********************************************************************
C********************************************************
C*
C*   SUBROUTINE:   filchk
C*
C*   Determines which points should be put into
C*   the wrkpts array, to be used to create the
C*   fillet and calls rotpts to rotate the points
C*   if neccessary.
C*
C*
C*******************************************************
      subroutine filchk (nxrc,wrkpts,iend,ifless,mxtmp,
     -                   mxcl1,mxcl2,filsav,npts,cirsto,
     -                   icrnbf,npts3)
 
      include 'com8a.com'
      include 'fillet.com'
C
C...filsave is the array that has the point's coordinates.
C
      real*8 wrkpts(6,4),filsav(420),tmp1,cirsto(7)
      real*8 sectol,distsq,pl(4)
      parameter (sectol=1.d-12)
 
      integer*4 nxrc(2),nxrc2(2),imltgo,savken(2),iclw(6),iclwtm(6)
 
      integer*2 IEND,IFLESS,MXTMP,mxcl1,mxcl2,NPTS,npts3
      integer*2 iclf, jerr
      integer*4 icrnbf(5)
      logical ismrec
c
c...Debug variables
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)
C
C...RESTORE POINTS BEFORE PROCESSING IF NEEDED
C...IROTPT - 1= ROTATE POINTS
C...IROTPT - 0= RESTORE POINTS
C
      iclf = 0
      iend = 0
      call ncl_setptr(nxrc, savken)
      IACY = 4
      IF (IROTPT .NE. 3) GO TO 90
      IERR  = 0
      IROTPT  = 0
 
      DO 80 I=1,4,1
          CALL ROTPTS (WRKPTS(1,I),IERR)
   80 CONTINUE
 
      IROTPT = 99
   90 IF (IFLESS .GT. 0) GO TO 1250
C
C...Save in case this is the last motion record
C
      call ncl_setptr(nxrc, nxrc2)
      NREC2  = NREC
      INC    = 0
C
C...IMLTSV  = 1 If both records are single points
C...IMLTSV  = 2 If both records are multi point
C...IMLTSV  = 3 If 1st rec is single and second is multi
C...IMLTSV  = 4 if 1st rec is multi and second is single
C
C... WAS THE LAST RECORD A SINGLE POINT
C
      IMLTSV = 1
      IF (MXCL1 .EQ. 1) GO TO 100
C
C... THE LAST RECORD WAS MULTIPLE POINT
C
      IMLTSV = 2
 
C
C...get the record for the second motion of the fillet.
C
100   call ncl_setptr(nxrc, savken)
      call clread (iclf,nxrc,iclw,dnxbuf,jerr)
      if (jerr.eq.1) goto 5000
C
C...Check for circular record - store if it is...
C
      if (iclw(3) .eq. 3000) then
          do 150 i=1,7,1
              cirbuf(i) = dnxbuf(i)
              cirsto(i) = cirbuf(i)
              if (i .gt. 5) goto 150
              icrnbf(i) = iclw(i)
  150     continue
          icrsto = 1
          goto 100
      endif
c
c...Get next record if not motion type
c
      if (.not. ismrec(iclw)) then
          call ncl_setptr(nxrc, nxrc2)
          goto 100
      endif
C
C...mxcl is the number of points in dnxbuf.
C
      call setnpt(iclw, npts3)
      call calmxc (iclw,mxcl)
      mxcl2  =  mxcl
c
c...ARCSLP/FILLET,COMBIN in effect
c...Get next record and append to 1st
c
      if (FILFLG(4) .eq. 1) then
  200     call ncl_setptr(nxrc, savken)
          call clread (iclf,nxrc,iclwtm,dclbuf,jerr)
          if (jerr .ne. 1 .and. ismrec(iclwtm)) then
              do 300 i=1,iclwtm(5),1
                  dnxbuf(iclw(5)+i) = dclbuf(i)
  300         continue
              call calmxc (iclwtm,mxcl)
              IFADJ2 = mxcl2
              istrpt(2) = mxcl2 + 1
              mxcl2 = mxcl2 + mxcl
          else
              go to 200
          endif
      endif
C
C... IS THE NEXT RECORD MULTIPLE POINT
C
 1200 IF (MXCL1 .EQ. 1 .AND. MXCL2 .GT. 1) IMLTSV = 3
      IF (MXCL1 .GT. 1 .AND. MXCL2 .EQ. 1) IMLTSV = 4
C
C... SET UP X,Y,Z POINTS
C
 1250 IMLTGO = IMLTSV
      GO TO (1300,1400,1500,1600), IMLTGO
C
C... BOTH RECORDS ARE SINGLE POINT
C...wrkpts(x,1) should have the beginning point of the first motion.
C...wrkpts(x,2) should have the ending point of the first motion.
C...wrkpts(x,3) should have the same point as wrkpts(x,2) this is the
C...starting point of the second motion.
C...wrkpts(x,4) should have the ending point of the second motion.
C

 1300 IPT    =  ((MXCL1-1)*NPTS) + 1
      DO 1310 I=1,NPTS,1
          WRKPTS(I,1) = OLDPTS(I)
          WRKPTS(I,2) = FILSAV(IPT+I-1)
          WRKPTS(I,3) = FILSAV(IPT+I-1)
          WRKPTS(I,4) = dnxbuf(i)
 1310 CONTINUE
C
      call ncl_setptr(nxrc2, nxrc)
      NREC   = NREC2
      GO TO 1700
C
C... BOTH RECORDS ARE MUTILPLE POINT
C...wrkpts(x,1) has the second to last point in the first motion record.
C...wrkpts(x,2) has the last point in the first motion record.
C...wrkpts(x,3) has the same point as (x,2).
C...wrkpts(x,4) has the first point in the second poiton record.
C...If the start point of the fillet is before the second to last point in
C...the motion record, the program will return here and get the third to last
C...point and put it in wrkpts(x,1)  
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CCC        I THINK SOMETHING IS WRONG HERE
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1400 IPT    =  ((MXCL1-1)*NPTS) + 1
      if (mxcl1.le.1) goto 5000
c
c..... if start of the array FILSAV has been reached, return iret=1
c
      DO 1410 I=1,NPTS,1
          WRKPTS(I,1) = FILSAV(IPT-NPTS+I-1)
          WRKPTS(I,2) = FILSAV(IPT+I-1)
 1410 CONTINUE
      IF (ISTRPT(2) .GT. 1) GO TO 1450
      IPT    =  ((MXTMP-1)*NPTS) + 1
      DO 1420 I=1,NPTS,1
          WRKPTS(I,3) = FILSAV(IPT+I-1)
          WRKPTS(I,4) = dnxbuf(i)
 1420 CONTINUE
C
      GO TO 1700
C
C...If we get to this point, then the fillet end point was further
C...along then the previous point, shift down the record to the next 
C...two points.  The previous (x,4) is now in (x,3) and the next point
C...in the record ins in (x,4).
C   
 1450 DO 1460 I=1,NPTS,1
          WRKPTS(I,3) = dnxbuf(i+((istrpt(2)-2)*npts3))
          WRKPTS(I,4) = dnxbuf(i+(npts3*(istrpt(2)-1)))
 1460 CONTINUE
C
      GO TO 1700
C
C... 1ST RECORD IS SINGLE POINT, 2ND IS MULTIPLE POINT
C...Again, wrkpts(x,1) and wrkpts(x,2) have the start and end points 
C...of the first motion.  wrkpts(x,3) is the same as wrkpts(x,2)
C...and wrkpts(x,4) has the first point in the second motion record.
 1500 IPT    = 1
      DO 1510 I=1,NPTS,1
          WRKPTS(I,1) = OLDPTS(I)
          WRKPTS(I,2) = FILSAV(IPT+I-1)
 1510 CONTINUE
C
      IF (ISTRPT(2) .GT. 1) GO TO 1550
      DO 1520 I=1,NPTS,1
          WRKPTS(I,3) = FILSAV(IPT+I-1)
          WRKPTS(I,4) = dnxbuf(i)
 1520 CONTINUE
C
      GO TO 1700
C
C...The fillet endpoint was further along than the first
C...point in the motion record so put the previous wrkpts(x,4)
C...into wrkpts(x,3) and get the next point in the motion record
C...into wrkpts(x,4).
C
 1550 DO 1560 I=1,NPTS,1
          WRKPTS(I,3) = dnxbuf(i+((istrpt(2)-2)*npts3))
          WRKPTS(I,4) = dnxbuf(i+(npts3*(istrpt(2)-1)))
 1560 CONTINUE
C
      GO TO 1700
C
C...1st record is multiple point and second is single point.
C...wrkpts(x,1) should have the second to last point of the 
C...first record in it, and wrkpts(x,2) the last point, if
C...the start point of the fillet came before this point then
C...put the third to last point into wrkpts(x,1) and the 
C...the second to last into wrkpts(x,2)
C
 1600 IPT    =  ((MXCL1-1)*NPTS) + 1
      DO 1610 I=1,NPTS,1
          WRKPTS(I,1) = FILSAV(IPT-NPTS+I-1)
          WRKPTS(I,2) = FILSAV(IPT+I-1)
 1610 CONTINUE
C
      IF (IFLESS .EQ. 1) GO TO 1700
      IPT    =  ((MXTMP-1)*NPTS) + 1
C
C...Again, wrkpts(x,3) should have the endpoint of the first motion record
C...and wrkpts(x,4) the endpoint of the second motion.
C
      DO 1620 I=1,NPTS,1
          WRKPTS(I,3) = FILSAV(IPT+I-1)
          WRKPTS(I,4) = dnxbuf(i)     
 1620 CONTINUE
C
C...Debug output
C
 1700 continue

      if (istrpt(2) .lt. mxcl2) then
        distsq = (WRKPTS(1,4)-WRKPTS(1,3))**2 +
     -           (WRKPTS(2,4)-WRKPTS(2,3))**2 +
     -           (WRKPTS(3,4)-WRKPTS(3,3))**2
        if (distsq .le. sectol) then
          IFLESS = 1
          ISTRPT(2) = ISTRPT(2) + 1
          goto 90
        endif
      endif

      if (DEBUGX .eq. 1) then
          do 7002 i=1,4,1
            write (dbuf,7001) i,wrkpts(1,i),wrkpts(2,i),wrkpts(3,i),
     1        wrkpts(4,i),wrkpts(5,i),wrkpts(6,i)
 7001       format ('Wrkpts(',i1,') = ',6f11.4)
            call ctob (dbuf,dout)
            call nclxostr(dout)
 7002     continue
      endif
c
c...Determine if tool axis is fixed
c
      iacy1 = 5
      IFILTA = 0
      do 1750 i1=1,3,1
          if (NPTS .eq. 3) then
              FILTAX(i1) = sc(i1+3)
          else
              FILTAX(i1) = wrkpts(i1+3,1)
          endif
          do 1740 i=4,6,1
              tmp1 = wrkpts(i,i1+1)-wrkpts(i,i1)
              call dpont2(tmp1,iacy1)
              if (tmp1 .ne. 0.) go to 1760
 1740     continue
 1750 continue
      IFILTA = 1
C
C...Make sure points are on tool axis plane
C
 1760 pl(1) = FILTAX(1)
      pl(2) = FILTAX(2)
      pl(3) = FILTAX(3)
      pl(4) = pl(1)*wrkpts(1,1)+pl(2)*wrkpts(2,1)+pl(3)*wrkpts(3,1)
      DO 1800 I1=2,4,1
          TMP1 = dabs(pl(4) - (pl(1)*wrkpts(1,i1) + pl(2)*wrkpts(2,i1) +
     1                         pl(3)*wrkpts(3,i1)))
          call dpont2 (TMP1,IACY)
          if (tmp1 .lt. -.00005 .or.
     -        tmp1 .gt. .00005) goto 6000
1800  continue
C
      CIRBUF(4) = FILTAX(1)
      CIRBUF(5) = FILTAX(2)
      CIRBUF(6) = FILTAX(3)
      ICROUT = ifl(94)
      GOTO 6000
C
C... ENCOUNTERED A RAPID OR FINI
C
 5000 IEND = 1
      RETURN
C
C... ZAXIS MOVEMENT - ROTATE POINTS TO XYPLAN
C
 6000 NOPTS = 4
      IERR  = 0
C
C... 1 = rotate points
C... 0 = restore points
C
      IROTPT  = 1
      call ncl_setptr(savken, nxrc)

        distsq = (WRKPTS(1,2)-WRKPTS(1,1))**2 +
     -           (WRKPTS(2,2)-WRKPTS(2,1))**2 +
     -           (WRKPTS(3,2)-WRKPTS(3,1))**2
        if (distsq .le. sectol) then
          call rtprst
          iend = 3
          return
        endif

      CALL ROTPTS (WRKPTS,IERR)
      if (lcirvc .eq. 0) then
        lcirvc = 1
        cirvc0(1) = cirbuf(4)
        cirvc0(2) = cirbuf(5)
        cirvc0(3) = cirbuf(6)
      endif
      IF (IERR .EQ. 1) GO TO 7000
       
      RETURN
 7000 IEND = 2
      RETURN
C
      END
