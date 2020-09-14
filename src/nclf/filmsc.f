C*********************************************************************
C*    NAME         :  filmsc.f
C*    CONTAINS:   Miscellaneous routines to handle the 
C*                ARCSLP/FILLET command in NCL instead of PREPST
C*
C*                calmxc  ftsave  fedadj  filadj  filprs  fedset  setnpt
C*                filseg  filsp2
C*
C*    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*        filmsc.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*        05/27/15 , 17:19:11
C********************************************************************/
C******************************************************************
C*
C*    SUBROUTINE: calmxc
C*
C*    PURPOSE:    Determine the number of clpoints in a record.
C*
C*****************************************************************
      subroutine calmxc (iclw,mxclp)
 
      include 'com8a.com'
      include 'fillet.com'
 
      integer*2 mxclp
      integer*4 iclw(5)
c
c...Calculate number of cl points based on record type
c...5000 - 6 reals per location, or 3 if multax is off.
c...5200 - 21 reals per location
c
      if ((iclw(3) .eq. 5000 .or. iclw(3) .eq. 5100) .and.
     1    (multax.eq.1)) then
          mxclp   = iclw(5) / 6
      else if ((iclw(3) .eq. 5000 .or. iclw(3) .eq. 5100) .and.
     1         (multax.eq.0)) then
          mxclp   = iclw(5) / 3
      else if ((iclw(3).eq.5200).or.(iclw(3).eq.5210) .or.
     1         iclw(3) .eq. 5300) then
          mxclp   = iclw(5) / 21
      else if (iclw(3).eq.5220) then
          mxclp   = iclw(5) / 12
      endif
c
c...End of routine
c
 8000 return
      end
C****************************************************************
C*
C*    SUBROUTINE:  ftsave
C*
C*    PURPOSE:  Saves an array and returns the number of points
C*              in the array.
C*
C****************************************************************
      subroutine ftsave (kst,iclw,clbuf,filsav,mxcl,npts)
 
      integer*2 kst,mxcl,npts
      integer*4 iclw(5)
 
      real*8 clbuf(640),filsav(640)
 
      integer*4 is
      integer*2 i
c
c...Save off cl points
c
      if (iclw(3).eq.5000 .or. iclw(3) .eq. 5100) then
         is     = 0
         do 200 i=kst,iclw(5)
            is     = is     + 1
            filsav(i) = clbuf(i)
200      continue
c
c...Define number of points
c
         mxcl   = is     /npts 
      else
         is = 0
         do 300 i = 1,(iclw(5)/21)
             do 250 j=1,npts,1
                 filsav((is*npts) + j) = clbuf((is*21) + j)
  250        continue
             is = is +1
300      continue
         mxcl = is
      endif
c
c...End of routine
c
 8000 return
      end

C*******************************************************
C*
C*    SUBROUTINE: fedadj
C*
C*    PURPOSE:    Adjust feed rate.
C*
C******************************************************
      subroutine fedadj (vec,ierr)

      include 'com8a.com'
      include 'fillet.com'
      include 'const.com'

      real*8 vec(3,3),angl(2),del

      ierr   = 0
      if (fedout .lt. 0) goto 990
      idir   = 0
C
C...Define thru angles of vectors.
C
      do 110 i=1,2,1
          angl(i) = RADIAN * DACOS (vec(i,1))
          if (vec(i,2) .lt. 0.) angl(i) = 360. - angl(i)
          if (angl(i) .lt. 0.) angl(i) = angl(i) + 360.
          if (angl(i) .gt. 360.) angl(i) = angl(i) - 360.
  110 continue
C
C...Set dircetion of motion
C...idir = 1 - left
C...idir = 2 - right
C
      del    = angl(2) - angl(1)
      call dpont2 (del,4)
      if (del .gt. 0.) idir = 1
      if (del .lt. 0.) idir = 2
      if (DABS(del) .gt. 180.) idir = 3 - idir
 
      if (idir .eq. 0 .or. idir .eq. 3) goto 900
C
C...Select feed rate
C
      if (idir .eq. ifedco) then
          fedout = fedlow
      else
          fedout = fedhig
      end if
      return
C
C...error - Same or opposite direction
C
  900 ierr   = 1
  990 fedout = 0.
      return
      end
C*****************************************************
C*
C*    SUBROUTINE: filadj
C*
C*    PURPOSE:  Rotates the points in the clpt array, back
C*              to the location they were originally in.
C*
C********************************************************
      subroutine filadj

      include 'com8a.com'
      include 'fillet.com'

      if (irotpt .eq. 0) then
          do 100 i=1,mxcl,1
              call rotpts (clpt(1,i),ierr)
  100     continue
      endif

      return
      end

C***********************************************************************
C*
C*       SUBROUTINE:   fedset
C*       PURPOSE:      This routine checks the syntax and sets the
C*                     FEDRAT variables for the ARCSLP/FILLET command.
C*
C***********************************************************************

      subroutine fedset (frad)

      include 'com8a.com'
      include 'fillet.com'

      real*8 frad

      real*8 tl,cutrad
      integer*2 ifed

C
C...Right now we need the feed rate to maintain on the side of the cutter
C
      call parsit
      if ((ityp.eq.2.and.ist.eq.2).or.(ityp.eq.3).or.
     x          (ityp.eq.4)) then 
         SFLFED=tv
C
C...If just FEDRAT was in the command, use the current values
C...for fedrat, cutter diameter, direction, and set fedmax to 999.
C...JLS 7/6/99
C
      else if (ityp.eq.7) then
         SFLFED = sc(123)
         SFLFMX = 999 
         cutrad= sc(28)/2
         if (ifl(21).eq.1) then
C...Right
            ifed = 2
         else
C...Left
            ifed = 1
         endif
C
C...Now go down to figure out the high and the low fedrat.
C
         goto 99997
      endif
C
C...Now get the maximum feed rate that can be output with the fillet motion.
C
      call parsit
      if ((ityp.eq.2.and.ist.eq.2).or.(ityp.eq.3).or.
     x          (ityp.eq.4)) then 
         SFLFMX = tv
      else
         goto 99998
      endif
C
C...Now get LEFT or RIGHT, ifedco is a global variable
C
      call parsit
      if (.not.vocab) goto 99998
      if (ist.eq.8) then
         ifed=1
      else if (ist.eq.24) then 
         ifed=2
      else
         goto 99998 
      endif
C
C...Get the cutter diameter
C
      call parsit
      if ((ityp.eq.2.and.ist.eq.2).or.(ityp.eq.3).or.
     x          (ityp.eq.4)) then
         cutrad=tv/2
      else
         goto 99998
      endif

C
C...Set up low and high feed rates.
C...fedlow and fedhig are global variables
C
99997 continue
      SFLDIA = cutrad * 2.
      IFEDCO = ifed
      FEDLOW = SFLFED * frad/(frad+cutrad)
      tl = frad - cutrad
      if (tl. lt. .001) then
         FEDHIG=SFLFMX 
         goto 99999
      endif
      FEDHIG = SFLFED * frad/tl 
      if (FEDHIG.gt.SFLFMX) FEDHIG = SFLFMX
      goto 99999
C
C...If everything is okay, exit, if not set ifl(2)
C
99998 ifl(2)=61

99999 return
      end 
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine filprs
c*       This routine parses the variable portion of the ARCSLP/FILLET
c*       command.
c*
c*          ARCSLP/FILLET,...[,tol] [,WARN/NOWARN] [,FEDRAT,fed,fmax, $
c*                           LEFT/RIGHT,cdia] [,ONCE/COMBIN] [,FAN/SAME]
C*    PARAMETERS   
C*       INPUT  : 
C*          tflrad   R*8   Fillet radius.
C*       OUTPUT :  
C*          tfltol   R*8   Fillet tolerance.
C*          tlfed    L*2   TRUE if feedrate set.
C*          tflwrn   L*2   TRUE if WARN set.  FALSE if NOWARN set.
C*          tflcom   L*2   TRUE if COMBIN set.  FALSE if ONCE set.
C*          tflsam   L*2   TRUE if SAME set.  FALSE if FAN set.
C*    RETURNS      :
C*          ifl(2)   Non-zero if an error occured parsing statement.
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine filprs (tflrad,tfltol,tlfed,tflwrn,tflcom,tflsam,tmang)
c
      include 'com8a.com'
      include 'const.com'
c
      logical tlfed,tflwrn,tflcom,tflsam
c
      real*8 tflrad,tfltol,tmang
c
      integer*2 WARN,NOWRN,FEDRAT,ONCE,COMBIN,LOCK,INTERP
      parameter (WARN=852,NOWRN=853,FEDRAT=1009,ONCE=325,COMBIN=1071)
      parameter (LOCK=114,INTERP=752)
c
c...Loop to parse command
c
      do while (nextyp .ne. 11)
c
c...Parse command
c
          call parsit
c
c...Tolerance
c
          if (scalar) then
              tfltol = tv
c
c...WARN/NOWARN
c
          else if (ityp .eq. 1 .and.
     1             (ist .eq. WARN .or. ist .eq. NOWRN)) then
              tflwrn = ist .eq. WARN
c
c...FEDRAT
c
          else if (ityp .eq. 1 .and. ist .eq. FEDRAT) then
              call fedset (tflrad)
              tlfed = .true.
c
c...COMBIN/ONCE
c
          else if (ityp .eq. 1 .and.
     1             (ist .eq. COMBIN .or. ist .eq. ONCE)) then
              tflcom = ist .eq. COMBIN
c
c...LOCK/INTERP
c
          else if (ityp .eq. 1 .and.
     1             (ist .eq. LOCK .or. ist .eq. INTERP)) then
              tflsam = ist .eq. LOCK
c
c......LOCK,maxang
c
              if (tflsam) then
                  call svpars
                  call parsit
                  if (scalar) then
                      tmang = dcos(tv/RADIAN)
                  else
                      call rtpars
                  endif
              endif
c
c...Unrecognized parameter
c
          else
              ifl(2) = 61
          endif
c
c...Break on error
c
          if (ifl(2) .ne. 0) go to 8000
      enddo
c
c...End of routine
c
 8000 return
      end
C
C*****************************************************************
C*
C*    SUBROUTINE: setnpt
C*
C*    PURPOSE:    To return the number of spaces each clpnt 
C*                occupies in the motion record.
C*
C*****************************************************************

      subroutine setnpt (iclw,npts)
 
      include 'com8a.com'
      include 'fillet.com'

      integer*2 npts
      integer*4 iclw(5)

      if ((iclw(3) .eq. 5000 .or. iclw(3) .eq. 5100).and.
     1    (multax.eq.1)) then
          npts = 6
      else if ((iclw(3) .eq. 5000 .or. iclw(3) .eq. 5100).and.
     1         (multax.eq.0)) then
          npts = 3
      else if ((iclw(3).eq.5200).or.(iclw(3).eq.5210) .or.
     1         iclw(3) .eq. 5300) then
          npts = 21
      else if (iclw(3).eq.5220) then
          npts = 12
      else
          npts = 6
      endif
c
c...End of routine
c
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine filseg
c*       This routine determines if a point lies within a line segment
c*       as defined by 2 points.  It is assumed that all points lie on the
c*       same XY-plane.
C*    PARAMETERS   
C*       INPUT  : 
C*          gstrt    R*8   D3    Start point of line segment.
C*          gend     R*8   D3    End point of line segment.
C*          gpt      R*8   D3    Point to check.
C*       OUTPUT :  
C*          kfl      I*2   D1    1 = Point is within line segment.
C*                               0 = Not.
C*    RETURNS      :
C*          none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine filseg (gstrt,gend,gpt,kfl)
c
      include 'com8a.com'
c
      integer*2 kfl
c
      real*8 gstrt(3),gend(3),gpt(3)
c
      real*8 vs(3),v1(3),v2(3),f_mag,tol,d1,d2
c
      integer*2 i
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)
c
c...Initialize routine
c
      kfl = 0
      tol = .0001
      if (ifl(264) .eq. 1 .or. ifl(362) .eq. 1) tol = tol * 25.4
      if (DEBUGX .eq. 1) then
          write (dbuf,7001) gstrt(1),gstrt(2)
 7001     format ('Start  = ',f12.4,f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7002) gend(1),gend(2)
 7002     format ('End    = ',f12.4,f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7003) gpt(1),gpt(2)
 7003     format ('Point  = ',f12.4,f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif

c
c...Calculate line segment vectors
c
      vs(3) = 0.
      v1(3) = 0.
      v2(3) = 0.
      do 100 i=1,2,1
          vs(i) = gend(i) - gstrt(i)
          v1(i) = gpt(i) - gstrt(i)
          v2(i) = gend(i) - gpt(i)
  100 continue
c
c...If points almost match
c...Then assume on line segment
c
      if (f_mag(v1) .lt. tol .or.  f_mag(v2) .lt. tol) goto 8000
      d1 = dsqrt((gstrt(1)-gend(1))**2 + (gstrt(2)-gend(2))**2)
      d2 = dsqrt((gpt(1)-gstrt(1))**2 + (gpt(2)-gstrt(2))**2)
      d3 = dsqrt((gpt(1)-gend(1))**2 + (gpt(2)-gend(2))**2)
      if (d2-tol .gt. d1) kfl = 1
      if (d3-tol .gt. d1) kfl = 1
c
c...Make sure vectors match
c
      call unitvc(vs,vs)
      call unitvc(v1,v1)
      call unitvc(v2,v2)
      if (DEBUGX .eq. 1) then
          write (dbuf,7004) d1,d2,d3
 7004     format ('Line   = ',f8.4,'  Start = ',f8.4,'  End = ',f8.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7005) v1(1),v1(2)
 7005     format ('V1     = ',f12.4,f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7006) v2(1),v2(2)
 7006     format ('V2     = ',f12.4,f12.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7007) kfl
 7007     format ('KFL    = ',i2)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
cc      if (dabs(vs(1)-v1(1)) .gt. .001 .or.
cc     1    dabs(vs(2)-v1(2)) .gt. .001 .or.
cc     2    dabs(vs(3)-v1(3)) .gt. .001) kfl = 1
cc      if (dabs(vs(1)-v2(1)) .gt. .001 .or.
cc     1    dabs(vs(2)-v2(2)) .gt. .001 .or.
cc     2    dabs(vs(3)-v2(3)) .gt. .001) kfl = 1
c
c...End of routine
c
 8000 return
      end
C
C*****************************************************************
C*
C*    FUNCTION: ismrec
C*
C*    PURPOSE:    Determines if clfile record is a valid motion
C*                record during ARCSLP/FILLET.
C*
C*****************************************************************
C
      logical function ismrec (iclw)
c 
      include 'com8a.com'
      include 'fillet.com'
c
      integer*4 iclw(5)
c
      if (iclw(3) .eq. 5000 .or. iclw(3) .eq. 5200 .or.
     1   ((iclw(3) .eq. 5100 .or. iclw(3) .eq. 5300) .and.
     2    ifl(372) .eq. 1)) then
          ismrec = .true.
      else
          ismrec = .false.
      endif
c
c...End of routine
c
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine filsp2 (kpt,kmxcl,krec,kref,kfl)
c*       This routine increments the pointer within the second leg
c*       of motion.  A new clfile record will be read if the pointer is
c*       at the end of the clfile record and the pointer will be set
c*       to the beginning of the new record if it is a continuation
c*       record.
C*    PARAMETERS   
C*       INPUT  : 
C*          kpt      I*2   D1    Pointer within last motion record.
C*          kmxcl    I*2   D1    Number of points in motion record.
C*          krec     I*4   D2    Current clfile record.
C*          kref     I*2   D1    1 = Check to see if another point is
C*                               available only.  Do not increment
C*                               pointer and load next record.
C*       OUTPUT :  
C*          kpt      I*2   D1    Updated pointer for motion record.
C*          kmxcl    I*2   D1    Updated number of points in motion record.
C*          krec     I*4   D2    Updated clfile record number.
C*          kfl      I*2   D1    0 = No more points available, 1 = Another
C*                               point is available from current array,
C*                               2 = A continuation record was read to obtain
C*                               the next point.
C*    RETURNS      :
C*          none
C*    SIDE EFFECTS :
C*       Reads the next clfile record to determine if it is a
C*       continuation record when the end of the current record is
C*       reached.  Updates the clpoint data to the next record if so.
C*       The last point of the current record will be added as the
C*       first point of the new record.
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine filsp2 (kpt,kmxcl,krec,kref,kfl)
c
      include 'com8a.com'
      include 'fillet.com'
c
      integer*2 kpt,kmxcl,kref,kfl
      integer*4 krec(2)
c
      integer*2 npts,jerr,iclf,ipt
      integer*4 svrec(2),svrec1(2),i,iclw(6)
c
      logical ismrec
c
      real*8 dbuf(640)
c
c...Increment pointer
c
      ipt    = kpt
      kpt    = kpt    + 1
      kfl    = 1
c
c...At end of current record
c...See if next record is continuation record
c...First record read is actually current clfile record
c...so we need to read two records
c
      if (kpt .gt. kmxcl) then
c
c...This logic is not supported with ARCSLP/FILLET,COMBIN
c
          if (FILFLG(4) .eq. 1) then
              kfl    = 0
              go to 8000
          endif
          iclf   = 0
          call ncl_setptr (krec,svrec)
          call clread (iclf,krec,iclw,dbuf,jerr)
          call ncl_setptr (krec,svrec1)
          call clread (iclf,krec,iclw,dbuf,jerr)
c
c......Next record is not a continuation record
c
          if (jerr .eq. 1 .or. .not. ismrec(iclw) .or. iclw(4) .ne. 6)
     1            then
              call ncl_setptr (svrec,krec)
              kfl    = 0
c
c......Next record is a continuation record
c
          else
              kfl    = 2
c
c.........Calling routine is for reference only
c
              if (kref .eq. 1) then
                  call ncl_setptr (svrec,krec)
c
c.........Store as current motion,
c.........Starting with last point of current record
c
              else
                  call setnpt (iclw,npts)
                  do 100 i=1,npts,1
                      dnxbuf(i) = dnxbuf((kmxcl*npts)-npts+i)
  100             continue
                  do 200 i=1,iclw(5),1
                      dnxbuf(npts+i) = dbuf(i)
  200             continue
                  iclw(5) = iclw(5) + npts
                  call calmxc (iclw,kmxcl)
                  call ncl_setptr (svrec1,krec)
                  kpt    = 2
              endif
          endif
      endif
c
c...End of routine
c
 8000 if (kref .eq. 1) kpt = ipt
      return
      end
