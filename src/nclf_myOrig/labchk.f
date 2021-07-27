C*********************************************************************
C*    NAME         :  labchk.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) Numerical Control Computer Sciences
C*                       All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       labchk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:13
C********************************************************************/
C
C **********************************************************************
C **  PROGRAM NAME: labchk (kst,ken,clab,ksub,klin)
C **
C **  PURPOSE OF PROGRAM: Checks for a specific label in the current
C **                      looping region.
c **
c **  Input:  kst     I*2   D1   Contains the starting line number for
c **                             the current looping region.
c **
c **          ken     I*2   D1   Contains the ending line number for the
c **                             current looping region.
c **
c **          clab    C*n   D1   Contains the label to search for.
c **
c **          ksub    I*4   D1   Contains the label subscript to search for.
c **
c **  Output: klin    I*2   D1   Returns the number of the line in the
c **                             source file which contains the label.
c **                             Returns 0 if the label was not found.
C **
C **********************************************************************
C **********************************************************************
 
      subroutine labchk (kst,ken,clab,ksub,klin)
 
      include 'com4a.com'
 
      integer*4 kst,ken,klin,ksub
 
      character*(*) clab
 
      integer*2 svinx,j,k
      integer*4 i,nc,svncin
 
      character*(MAX_LEN) lbuf
      character*1536 svcin
C
C...Save current input line
C
      svcin  = cin
      svinx  = inx
      svncin = nccin
C
C...Search for the requested label
C
      do 100 i=kst,ken,1
C
C...Read next source record, deblank and uppercase.
C
          call getsrc (lbuf,nc,i,0)
          k = 1
          do j=1,nc,1
            if (lbuf(j:j).ne.' ') then
              lbuf(k:k)  = lbuf(j:j)
              k = k+1
            endif
          enddo
          lbuf(k:k)  = ' '
          call convrt (lbuf,cin,k)
          nccin = k
C
C...Parse first token
C
          inx    = 1
          call parsit
C
C...This is the label we were
C...looking for
C
          if ((nextyp .eq. 13 .or. nextyp .eq. 7) .and. (ist .eq. 13
     1        .or. ityp .eq. 3) .and. (token2 .eq. clab .and.
     2        ivxsub .eq. ksub)) then
              klin   = i      + 1
              go to 8000
          endif
  100 continue
C
C...Label was not found
C
      klin   = 0
C
C...End of routine
C...Restore current input line
C
 8000 cin    = svcin
      nccin  = svncin
      inx    = svinx
C
C...Reset the error flags as they
C...may have been set while looking for
C...the labels in the source file
C
      ifl(2) = 0
      err    = .false.
      return
      end
