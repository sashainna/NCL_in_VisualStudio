C*********************************************************************
C*    NAME         :  cutool.f
C*       CONTAINS:
C*          cutool 
C*
C*    COPYRIGHT 1993 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cutool.f , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*       12/08/15 , 14:00:17
C********************************************************************/
C
c***********************************************************************
c
c   SUBROUTINE:  cutool (kent,cbuf,knc)
c
c   FUNCTION:  Loads the requested tool number and the user defined
c              parameters from the NCL Tool Library.
c
c   INPUT:  kent    I*4  D1  -  Should be set to 0 upon first call (when
c                               parsing the original CUTTER/TOOL command)
c                               and not changed after that.
c
c   OUTPUT: kent    I*4  D1  -  Set to the entry type for the next call
c                               to this routine and can have the
c                               following values.
c
c                                  0 = Initial call or end of calls.
c                                  1 = Issue CUTTER/parms.
c                                  2 = Issue CUTTER/DISPLY,parms
c                                  3 = Issue CUTTER/DISPLY,(PART,ALL)
c                                  4 = Issue CUTTER/DISPLY,MOVE
c                                  5 = Issue CUTTER/DISPLY,SHADE
c                                  6 = Issue CUTTER/DISPLY,symbol
c                                  7 = Issue CUTTER/DISPLY,SHANK command.
c                                  8 = Issue CUTTER/DISPLY,HOLDER command.
c                                  9 = Issue Load Tool command.
c                                 10 = Load Tool continuation line.
c                                 11+= Issue Operator Commands
c                                 -1 = Ran out of commands.  No command
c                                      was issued.
c
c           cbuf    C*n  D1  -  Text of command when 'kent'.
c
c           knc     I*4  D1  -  Number of chars in 'cbuf'.
c
c***********************************************************************
c
      subroutine cutool (kent,cbuf,knc)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 kent,knc
c
      character*(*) cbuf
c
      integer*2 ksn(4)
      equivalence (asn,ksn)
c
      integer*4 it,np,i,ierr,nc,strlen1,lpc(20)
      integer*2 len,il
      real*8 itool
      character*256 txt, fulldir
c
      data lpc /305,313,321,329,337,345,353,361,369,377,385,393,401,
     1          409,417,425,433,441,449,457/
c
      integer*4 tfbuf(128)
      equivalence (fbuf,tfbuf)
c
c...Initial Entry
c...Parse CUTTER/TOOL statement
c
      if (kent .eq. 0) then
          asn    = sc(10)
          it     = 12
          il     = 0
          np     = 2
c
c......CUTTER/TOOL,toolib
c
          len = 0
          if (ksn(3) .gt. 2 .and. sc(12) .lt. 0) then
             il     = 2
             call nclf_getlsc(il, txt, len) 
             call ulf_get_full_dir(txt,fulldir,len)
             if (len.eq.0) then
                  it     = 13
                  np     = 3
              else
                  it     = 14
                  np     = 4
              endif
          endif
c
c......CUTTER/TOOL [,toolib_path] [,toolib], tool
c
          itool  = sc(it)
          if (itool .le. 0) go to 9100
          if (il .ne. 0) then
              if (len.gt.0) then
                 toolib(1:) = txt(1:len) 
     1                     // '\' // lsc(il+1)(1:64)
              else
                 toolib = lsc(il)
                 call nclf_getlsc(il, txt, len)
                 if (toolib(78:79).eq.'&&') then
                    toolib(1:) = txt(1:len) 
                 endif
              endif
          endif
c
c......Store user parameters
c......CUTTER/TOOL,tool, parms
c
          if (ksn(3) .gt. np) then
              nctool = ksn(3) - np
              if (nctool .gt. 20) nctool = 20
              do 80 i=1,nctool,1
                  lctool(i) = lsc(i+np)
   80         continue
          else
              nctool = 0
          endif
c
c......Open tool library and load in toolib if not load in yet
c
          nc = strlen1(toolib)
          call ncl_load_toolf(toolib, nc, ierr)
          if (ierr.eq.1) goto 9200
          if (ierr.eq.2) goto 9300
          call ncl_sel_toolf(itool, ierr)
          if (ierr .ne. 0) goto 9400
          kent   = 1
          ictlin = nline
          fromct = ifl(123)
          go to 8000
      endif

      call ncl_cutool(kent,cbuf,knc)
c
c......the last character cbuf here in C is '\0',
c......but need to be space " " in Fortran
c
      cbuf(knc+1:) = ' '
c
c...End of routine
c
 8000 return
c
c...Invalid tool number
c
 9100 ifl(2) = 483
      kent   = -1
      go to 8000
c
c...Could not open tool library
c
 9200 ifl(2) = 484
      kent   = -1
      go to 8000
c
c...Error reading from library
c
 9300 ifl(2) = 485
      kent   = -1
      close (unit=scrlun)
      go to 8000
c
c...Could not find requested tool
c
 9400 ifl(2) = 486
      kent   = -1
      close (unit=scrlun)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtllib (clib,knc)
c
c   FUNCTION:  Interface to C routines which returns the name of the
c              NCL Tool Library.
c
c   INPUT:  none.
c
c   OUTPUT: clib    C*n  D1  -  Name of NCL Tool Library.
c
c           knc     I*4  D1  -  Number of chars in 'clib'.
c
c***********************************************************************
c
      subroutine gtllib (clib,knc)
c
      include 'com.com'
      include 'cutter.com'
c
      integer*4 knc
c
      character*(MAX_PATH) clib
c
      integer*4 strlen1
c
      clib = toolib
      knc = strlen1(toolib)
      return
      end
