c
c***********************************************************************
c
c   FILE NAME: helpi.for
c   CONTAINS:
c               helpi
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        helpi.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:56
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  helpi (cbuf,kindx,kerr)
c
c   FUNCTION:  This routine displays the help text requested by the sub-
c              ject 'cbuf'.
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing the subject
c                               of the help text requested.
c
c           kindx   I*4  D1  -  Value of first level.  This is used as
c                               the number of the index record that con-
c                               tains the pointer to the help text.
c
c   OUTPUT: kerr    I*4  D1  -  Returns 1 if the help subject is not
c                               present in the help file.  Returns -1
c                               if the user interrupted the display with
c                               a ^C.
c
c***********************************************************************
c
      subroutine helpi (cbuf,kindx,kerr)
c
      include 'menu.inc'
c
      common /CHELPI/ irec  , indx  , itxtpt
c
      integer*4 indx(128),irec,itxtpt
c
      character*512 lindx
c
      equivalence (indx,lindx)
c
      integer*4 kerr,kindx
c
      character*(*) cbuf
c
      integer*4 ipt,ic,i,il,icnt,nc,ie,itxt
c
      character*32 sbuf
c
      kerr   = 0
      ic     = 0
      sbuf   = cbuf // ' '
c
c...Multiple time thru Motif lookup
c...Go get next record
c
      if (IMSCAN .ne. 0 .and. IMHFLG .eq. 1) then
          if (indx(itxtpt) .eq. 0) then
              kerr   = -1
              go to 8000
          endif
          if (indx(itxtpt) .gt. 0) go to 1150
          go to 2000
      endif
c
c...Find help index
c
      irec   = kindx
      if (irec .le. 0) irec = 1
      if (irec .gt. 36) irec = 36
  100 read (LUNHLP,rec=irec) indx
      do 500 i=1,indx(2),1
          ipt    = (i-1) * 10 + 3
          itxt   = (ipt-1) * 4 + 9
          ie     = itxt    + 31
          if (sbuf .eq. lindx(itxt:ie)) go to 800
  500 continue
      if (indx(1) .eq. 0) go to 9000
      irec   = indx(1)
      go to 100
c
c...Found index
c...Display help text
c
  800 irec   = indx(ipt)
      itxtpt = indx(ipt+1)
      il     = IMENBL - 1
      icnt   = il
c
c......Erase screen
c
      if (IMSCAN .eq. 0) then
          call plott (IMENBL,1)
          call clreos
      endif
c
c......Read help text record
c
 1100 read (LUNHLP,rec=irec) indx
      if (indx(itxtpt) .eq. -1) then
          if (indx(1) .eq. 0) then
              if (IMSCAN .ne. 0) kerr = 2
              go to 8000
          endif
          irec   = indx(1)
          itxtpt = 2
          go to 1100
      endif
c
c......Page is full
c......Get key from user
c
 1150 if (IMSCAN .eq. 0) then
          icnt   = icnt   + 1
          if (icnt .gt. IMENEL) then
              call errmsg ('HITKEY',1,2)
              call gtch (ic)
              call errmsg (' ',2,2)
              if (ic .eq. 3) then
                  kerr   = -1
                  go to 8000
              endif
              icnt   = IMENBL + 1
          endif
c
c......Position cursor &
c......Scroll page if necessary
c
          if (il .lt. IMENEL) then
              il     = il     + 1
              call plott (il,1)
          else
              call scrol (IMENBL,IMENEL,1,1)
              call plott (il,1)
              call clreol
          endif
      endif
c
c......Set text indices
c
      nc     = indx(itxtpt)
      itxt   = (itxtpt-1) * 4 + 5
      ie     = itxt   + nc     - 1
      itxtpt = itxtpt + ((nc-1) / 4 + 2)
c
c......Display help text
c
      if (IMSCAN .eq. 0) then
          call dmpbuf (lindx(itxt:ie),nc)
      else
          MHLPBF = lindx(itxt:ie)
          MHLPNC = nc
          go to 8000
      endif
c
c......Increment text pointers
c
      if (indx(itxtpt) .eq. 0) go to 8000
      if (indx(itxtpt) .gt. 0) go to 1150
c
c......Last text on this record
c......Point to next record
c
 2000 if (indx(1) .eq. 0) then
          if (IMSCAN .ne. 0) kerr = -1
          go to 8000
      endif
      irec   = indx(1)
      itxtpt = 2
      go to 1100
c
c...End of routine
c
 8000 IMHFLG = 0
      return
c
c...Help index not found
c
 9000 call errmsg ('NOHELP',1,2)
      kerr   = 1
      return
      end
