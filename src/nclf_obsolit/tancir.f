C*********************************************************************
C*    NAME         :  tancir.f
C*
C*    CONTAINS:     Routines to handle the ARCSLP/FILLET command
C*
C*                  tancir
C*                  pcirout
C*
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        tancir.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:46
C********************************************************************/
C**************************************************************
C*
C*       Subroutine tancir
C*
C*       This routine is a modified version of the PREPST
C*       routine tancir. 
C*
C*       Purpose:  Calculate a circle tangent to two lines
C*
C*       WRKPTS = Are the line endpoints.
C*       VEC    = The unitized vectors of the lines created
C*                from the motion clpts.
C*       IERR   = Returns 1 if an error in building the circle
C*       IMOD   = Is the modifier 1=XL,2=YL,3=XS,4=YS
C*
C**************************************************************
      subroutine tancir (wrkpts,vec,ierr,imod)
      include 'com8a.com'
      include 'fillet.com'
 
      real*8 wrkpts(6,4),r1,r2,s1
      real*8 s2,t1,t2,q1,den
      real*8 vec(3,3),RADS
 
      integer*2 imod(2),ierr
 
      PI     = 3.1415926535897932D0
      ierr   = 0
      RADS   = (180.0/PI)
 
      r1=-vec(1,2)
      s1=vec(1,1)
      t1=r1*wrkpts(1,1)+s1*wrkpts(2,1)
C
C...Length of vector.
C
      q1=DSQRT(r1**2+s1**2)
      if (q1 .gt. 1.D-6) goto 100 
      goto 9000
C
C...Flip Normal if indicated.
C
  100 if(imod(1) .eq. 1 .and. r1 .lt. 0.) q1 = -q1
      if(imod(1) .eq. 2 .and. s1 .lt. 0.) q1 = -q1
      if(imod(1) .eq. 3 .and. r1 .gt. 0.) q1 = -q1
      if(imod(1) .eq. 4 .and. s1 .gt. 0.) q1 = -q1
C
C...Give an error if x modifier relates to line parallel to x axis
C...or y modifier relates to line parallel to y axis.
C
      if ((imod(1).eq.1.or.imod(1).eq.3).and.r1.eq.0) goto 9000
      if ((imod(1).eq.2.or.imod(1).eq.4).and.s1.eq.0) goto 9000
 
      r1=r1/q1
      s1=s1/q1
      t1=t1/q1+filrad
C
C...Repeat the process with the next vector.
C
      r2=-vec(2,2)
      s2=vec(2,1)
      t2=r2*wrkpts(1,3)+s2*wrkpts(2,3)
      q1=DSQRT(r2**2+s2**2)
 
      if (q1 .lt. 1.D-6) goto 9000
 
      if(imod(2) .eq. 1 .and. r2 .lt. 0.) q1 = -q1
      if(imod(2) .eq. 2 .and. s2 .lt. 0.) q1 = -q1
      if(imod(2) .eq. 3 .and. r2 .gt. 0.) q1 = -q1
      if(imod(2) .eq. 4 .and. s2 .gt. 0.) q1 = -q1
C
C...Give an error if x modifier relates to line parallel to x axis
C...or y modifier relates to line parallel to y axis.
C
      if ((imod(2).eq.1.or.imod(2).eq.3).and.r2.eq.0) goto 9000
      if ((imod(2).eq.2.or.imod(2).eq.4).and.s2.eq.0) goto 9000
      r2=r2/q1
      s2=s2/q1
      t2=t2/q1+filrad
      den  =  r1*s2-r2*s1
C
C...If lines look parallel
C
      if (DABS(den) .gt. 1.D-6) goto 200
      goto 9000
C
C...Calculate circle center point
C
  200 cirbuf(1)  =  (s2*t1-s1*t2)/den
      cirbuf(2)  =  (r1*t2-r2*t1)/den
      cirbuf(3)  =  (wrkpts(3,1)+wrkpts(3,4))/2
      cirbuf(7)  =  filrad
      goto 9100
 
 9000 ierr = 1
 
 9100 continue
 
      return
 
      end
C***********************************************************
C*
C*    SUBROUTINE: pchkpts
C*
C*    This routine is the same as PREPST's chkpts with just
C*    different include files
C*
C**********************************************************
      SUBROUTINE PCHKPTS (SANG,FANG,RAD1,INOPTS)

      include 'com8a.com'
      include 'fillet.com'

      REAL*8 LINSID,LINANG,TRATIO,SANG,FANG,
     1       RAD1,DLTTAB,SANGSV,TIMES

      PI     = 3.1415926535897932D0
      RADS   = (180. / PI)
      SANGSV = SANG
      MMSAV  = MXCL
      INEG   = 1
      INOPTS = 0
C
C...DECIDE IF MOVE SHOULD BE LINEARIZED
C
      DLTTAB = FANG-SANG
      IF (DABS(DLTTAB) .LE. 179.) GO TO 100
      INEG  = (0-1)
      IF (DLTTAB .LT. 0) INEG = 1
      DLTTAB  =  360. - DABS (DLTTAB)
C
C...CALCULATE MAXIMUM ANGULAR MOVE
C
  100 TRATIO =  0.
C
C... CIRCULAR TOLERANCE
C
C                    FILTOL  ! 12/02/87
      LINSID = DABS (RAD1)   - FILTOL
      IF (LINSID .LE. 0.) GO TO 9999
      LINANG = (DACOS (LINSID/DABS (RAD1)) * (180./PI)) * 2.
C
C...GET INCREMENT
C
      TIMES  = DABS (DLTTAB) / LINANG
      TIMES  = TIMES  + .49999
      ITIMES = IDNINT (TIMES)
      ITIMES = ITIMES - 1
      IF (ITIMES .LE. 0) GO TO 9999
      if (itimes .gt. 80) itimes = 80
      INOPTS = ITIMES
      RETURN
 9999 INOPTS = 0
      RETURN
      END


