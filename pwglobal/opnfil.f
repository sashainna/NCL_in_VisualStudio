c
c***********************************************************************
c
c   FILE NAME:  opnfil.for
c   CONTAINS:
c               adftyp  fbpost  fbreak  fparse  opnfil  shfile
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        opnfil.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:18
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  adftyp (cfnami,cfnamo,cext)
c
c   FUNCTION:  This routine removes the existing file extension within a
c              filename and replaces it with specified extension.
c
c   INPUT:  cfnami  C*n  D1  -  Input filename.
c
c           cext    C*n  D1  -  Requested new file extension for
c                               'cfnami'.
c
c   OUTPUT: cfnamo  C*n  D1  -  The input filename with the new exten-
c                               sion.
c
c***********************************************************************
c
      subroutine adftyp (cfnami,cfnamo,cext)
c
      character*(*) cfnami,cfnamo,cext
c
      integer*4 inc1,inc2,rindex,strlen1,nc
c
c...Determine if a file extension
c...is specified
c
      nc     = strlen1(cfnami)
      inc1   = rindex (cfnami,'.')
C VAX-START
C     inc2   = rindex (cfnami,']')
C VAX-END
C SUN-SGI-IBM-HPX-DEC-START
C      inc2   = rindex (cfnami,'/')
C SUN-SGI-IBM-HPX-DEC-END
C WNT-DOS-START
      inc2   = rindex (cfnami,'\')
C WNT-DOS-END
c
c...Append new file extension
c
      if (inc1 .eq. 0 .or. inc1 .lt. inc2) then
          cfnamo = cfnami(1:nc) // cext
      else
          cfnamo = cfnami(1:inc1-1) // cext
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fbpost (cfnam,cdev,cfil)
c
c   FUNCTION:  This routine accepts a filename as input and breaks it
c              up into 'device/directory' and post-processor name, such as
c              PWORKS_'cfil'.MDF.
c
c   INPUT:  cfnam   C*n  D1  -  Input filename.
c
c   OUTPUT: cdev    C*n  D1  -  Device & directory specification.
c
c           cfil    C*n  D1  -  Post-processor name.
c
c***********************************************************************
c
      subroutine fbpost (cfnam,cdev,cfil)
c
      include 'menu.inc'
c
      character*(*) cfnam,cdev,cfil
c
      integer*4 index,inc
c
      character*(MAX_PATH) lfil,lext
c
c...Break filename
c
      call fbreak (cfnam,cdev,lfil,lext)
c
c...Get post-processor name from filename
c
      inc    = index(lfil,'PWORKS_')
      if (inc .eq. 0) inc = index(lfil,'pworks_')
      if (inc .ne. 0) inc = index(lfil,'_')
      cfil   = lfil(inc+1:)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fbreak (cfnam,cdev,cfil,cext)
c
c   FUNCTION:  This routine accepts a filename as input and breaks it
c              up into 'device/directory', 'filename' and 'extension'.
c
c   INPUT:  cfnam   C*n  D1  -  Input filename.
c
c   OUTPUT: cdev    C*n  D1  -  Device & directory specification.
c
c           cfil    C*n  D1  -  File name specification.
c
c           cfil    C*n  D1  -  File extension specification.
c
c***********************************************************************
c
      subroutine fbreak (cfnam,cdev,cfil,cext)
c
      character*(*) cfnam,cdev,cfil,cext
c
      integer*4 rindex,strlen1,is,nc,inc
      character*1 slash
c
C SUN-SGI-IBM-HPX-DEC-START
C      data slash /'/'/
C SUN-SGI-IBM-HPX-DEC-END
C WNT-DOS-START
      data slash /'\'/
C WNT-DOS-END
c
c...Get device & directory
c
      is     = 1
      nc     = strlen1(cfnam)
C VAX-START
C     is     = index(cfnam,']')
C     if (is .eq. 0) then
C         is     = index(cfnam,':')
C         if (is .eq. 0) then
C             cdev   = ' '
C             is     = 1
C         else
C             cdev   = cfnam(1:is)
C             is     = is     + 1
C         endif
C     else
C         cdev   = cfnam(1:is)
C         is     = is     + 1
C     endif
C VAX-END
C SUN-SGI-IBM-HPX-DOS-DEC-WNT-START
      is     = rindex(cfnam,slash)
      if (is .le. 1) then
          is     = 1
          cdev   = ' '
      else
          cdev   = cfnam(1:is)
          is     = is     + 1
      endif
C SUN-SGI-IBM-HPX-DOS-DEC-WNT-END
c
      if (is .gt. nc) then
          cfil = ' '
          cext = ' '
          go to 8000
      endif
c
c...Get file name
c
      inc    = rindex(cfnam(is:nc),'.')
      if (inc .eq. 1) then
          cfil   = ' '
      else if (inc .eq. 0) then
          cfil   = cfnam(is:nc)
          cext   = ' '
          go to 8000
      else
          cfil   = cfnam(is:is+inc-2)
          is     = is     + inc    - 1
      endif
c
c...Get file extension
c
      cext   = cfnam(is:nc)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fparse (cfnami,cfnamo,cdev,cext)
c
c   FUNCTION:  This routine adds the default device/directory and file
c              extension to a filename if it does not already contain
c              them.
c
c   INPUT:  cfnami  C*n  D1  -  Input filename.
c
c           cdev    C*n  D1  -  Default device/directory string.
c
c           cext    C*n  D1  -  Default file extension.
c
c   OUTPUT: cfnamo  C*n  D1  -  The input filename with the default
c                               device/directory and file extension
c                               appended (if necessary).
c
c***********************************************************************
c
      subroutine fparse (cfnami,cfnamo,cdev,cext)
c
      include 'menu.inc'
c
      character*(*) cfnami,cfnamo,cdev,cext
c
      integer*4 nc,ifdot,idir,i,inc,strlen1
c
      character*1 space,tab,brak(2),slash,bs,colon,dot
      character*(MAX_PATH) fnam,dev,ldat
      byte bb,bt
      integer*4 envlen
C WNT-START
      integer*4 getenvc
C WNT-END
c
      equivalence (bs,bb), (tab,bt)
c
C VAX-START
C     integer*4 ibrak
C VAX-END
c
C SUN-SGI-VAX-IBM-HPX-DEC-START
C      data slash /'/'/, bb /8/
C SUN-SGI-VAX-IBM-HPX-DEC-END
C WNT-DOS-START
      data slash /'\'/, bb /8/
C WNT-DOS-END
      data space /' '/, bt /9/, dot /'.'/
      data brak /'[',']'/, colon /':'/
c
c...Determine if a device and/or file extension
c...is specified
c
      fnam   = cfnami
      cfnamo = ' '
      nc     = 0
      ifdot  = 0
      idir   = 0
c
      do 500 i=1,strlen1(fnam),1

cc          if (fnam(i:i) .eq. space .or. fnam(i:i) .eq. tab .and.
cc     1        nc .eq. 0) go to 500
          if (fnam(i:i) .eq. bs) then
              if (nc .ne. 0) nc = nc - 1
              go to 500
          endif
          if (fnam(i:i) .eq. dot) ifdot = 1
          if (fnam(i:i) .eq. slash) then
              ifdot  = 0
              idir   = 1
          endif
          nc     = nc     + 1
          fnam(nc:nc) = fnam(i:i)
  500 continue
c
c...Add default device
c
      if (idir .eq. 0) then
          cfnamo = cdev
          envlen = strlen1(cfnamo)
C WNT-START
          inc = getenvc(cfnamo, dev, envlen)
C WNT-END
C SUN-SGI-IBM-HPX-DOS-DEC-START
C         call getenv(cfnamo(1:envlen), dev)
C         inc = strlen1(dev)
C SUN-SGI-IBM-HPX-DOS-DEC-END
          if (inc .eq. 0) then
              cfnamo = cdev
              inc    = strlen1(cfnamo)
          else
              cfnamo = dev
              if (cfnamo(1:1) .eq. '"') then
                  ldat = cfnamo(2:)
                  cfnamo = ldat
                  inc = inc - 1
              endif
              if (cfnamo(inc:inc) .eq. '"') then
                  cfnamo(inc:inc) = ' '
                  inc = inc - 1
              endif
          endif
          if (inc .ne. 0 .and. cfnamo(inc:inc) .ne. slash) then
              ldat   = cfnamo(1:inc) // slash
              cfnamo = ldat
          endif
      endif
c
c...Add filename
c
      inc    = strlen1(cfnamo)
      if (inc .eq. 0) then
          cfnamo = fnam(1:nc)
      else
          ldat   = cfnamo(1:inc) // fnam(1:nc)
          cfnamo = ldat
      endif
c
c...Add default file type
c
      if (ifdot .eq. 0) then
          inc    = strlen1(cfnamo)
          ldat   = cfnamo(1:inc) // cext
          cfnamo = ldat
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  opnfil (kunit,cfnam,catt,krecl,cmsg,kerr)
c
c   FUNCTION:  This routine opens a file for input and/or output.
c
c   INPUT:  kunit   I*4  D1  -  Unit number of file to open.
c
c           cfnam   I*4  D1  -  Name of file to open.
c
c           catt    I*4  D4  -  Attributes of file to open.  'catt(1)' =
c                               ACCESS, 'catt(2)' = CARRIAGECONTROL,
c                               'catt(3)' = FORM, 'catt(4)' = STATUS.
c
c           krecl   I*4  D1  -  Record length of file.
c
c   OUTPUT: cmsg    C*n  D1  -  Error text when an error occurred trying
c                               to open the file.
c
c           kerr    I*4  D1  -  Returns -1 when an error occurred trying
c                               to open the file.
c
c***********************************************************************
c
      subroutine opnfil (kunit,cfnam,catt,krecl,cmsg,kerr)
c
      include 'menu.inc'
c
      character*(*) catt(*),cfnam,cmsg
c
      integer*4 kunit,krecl,kerr
C WNT-START
      integer*4 acslen, ctllen, fmtlen, stalen
C WNT-END
c
      integer*4 strlen1,nc
c
C VAX-SGI-IBM-HPX-DOS-DEC-START
C      integer*4 irecl,istat,nr
C VAX-SGI-IBM-HPX-DOS-DEC-END
c
C WNT-SUN-SGI-IBM-HPX-DOS-DEC-START
      character*80 lerr
C WNT-SUN-SGI-IBM-HPX-DOS-DEC-END
c
      character*20 rdonly,formt
      character*80 sbuf
	
c
      nc     = strlen1(cfnam)
c
c...Open file with specified attributes
c
C VAX-WNT-START



c...      open (unit=kunit,file=cfnam, access='sequential',
c...     1 	   form = 'FORMATTED', status = 'old', err=9000)

      call touppr (catt(4),rdonly)
      if (rdonly .eq. 'WRITE') catt(4) = 'OLD'
      irecl   = krecl
      call touppr (catt(3),formt)
      if (formt .eq. 'UNFORMATTED') irecl = (krecl-1) / 4 + 1
C VAX-WNT-END
C WNT-START
      if (rdonly .eq. 'NEW') catt(4) = 'UNKNOWN'
      acslen = strlen1(catt(1))
      ctllen = strlen1(catt(2))
      fmtlen = strlen1(catt(3))
      stalen = strlen1(catt(4))

      if (rdonly .eq. 'OLD') then
          open (unit=kunit,file=cfnam(1:nc),access=catt(1)(1:acslen),
     1      carriagecontrol=catt(2)(1:ctllen),form=catt(3)(1:fmtlen),
     2      status=catt(4)(1:stalen),recl=irecl,iostat=istat,
     3      err=9000, readonly,shared)
      else
           open (unit=kunit,file=cfnam(1:nc),access=catt(1)(1:acslen),
     1      carriagecontrol=catt(2)(1:ctllen),form=catt(3)(1:fmtlen),
     2      status=catt(4)(1:stalen),recl=irecl,iostat=istat,
     3      err=9000)
      endif
C WNT-END
C VAX-START
C      if (rdonly .eq. 'OLD') then
C          open (unit=kunit,file=cfnam,access=catt(1),
C    1          carriagecontrol=catt(2),form=catt(3),status=catt(4),
C    2          recl=irecl,iostat=istat,err=9000,readonly)
C     else
C         open (unit=kunit,file=cfnam,access=catt(1),
C    1          carriagecontrol=catt(2),form=catt(3),status=catt(4),
C    2          recl=irecl,iostat=istat,err=9000)
C     endif
C VAX-END
c
C SUN-START
C     call touppr (catt(4),formt)
C     if (formt .eq. 'NEW') formt = 'UNKNOWN'
C     if (formt .eq. 'WRITE') formt = 'OLD'
C     call touppr (catt(1),rdonly)
c
C     if (rdonly .eq. 'SEQUENTIAL') then
C         open (unit=kunit,file=cfnam(1:nc),access=catt(1),form=catt(3),
C    1          status=formt,err=9000)
C     else
C         open (unit=kunit,file=cfnam(1:nc),access=catt(1),form=catt(3),
C    1          status=formt,recl=krecl,err=9000)
C     endif
C SUN-END
C SGI-IBM-HPX-DEC-START
C      irecl   = krecl
C      call touppr (catt(3),formt)
C IBM-HPX-END
C     if (formt .eq. 'UNFORMATTED') irecl = (krecl-1) / 4 + 1
C IBM-HPX-START
C      call touppr (catt(4),formt)
C      if (formt .eq. 'NEW') formt = 'UNKNOWN'
C      if (formt .eq. 'WRITE') formt = 'OLD'
C      call touppr (catt(1),rdonly)
c
C      if (rdonly .eq. 'SEQUENTIAL') then
C IBM-HPX-END
C         open (unit=kunit,file=cfnam(1:nc),access=catt(1),form=catt(3),
C    1          status=formt,iostat=istat,err=9000)
C SGI-DEC-END
C IBM-HPX-START
C          if (formt .eq. 'SCRATCH') then
C             open (unit=kunit,access=catt(1),form=catt(3),status=formt,
C    1             iostat=istat,err=9000)
C         else
C             open (unit=kunit,file=cfnam(1:nc),access=catt(1),
C    1             form=catt(3),status=formt,iostat=istat,err=9000)
C          end if
C SGI-DEC-START
C      else
C SGI-DEC-END
C          if (formt .eq. 'SCRATCH') then
C             open (unit=kunit,access=catt(1),form=catt(3),status=formt,
C    1             recl=irecl,iostat=istat,err=9000)
C          else
C             open (unit=kunit,file=cfnam(1:nc),access=catt(1),
C    1             form=catt(3),status=formt,recl=irecl,iostat=istat,
C    2             err=9000)
C          endif
C IBM-HPX-END
C SGI-DEC-START
C         open (unit=kunit,file=cfnam(1:nc),access=catt(1),form=catt(3),
C    1          status=formt,recl=irecl,iostat=istat,err=9000)
C IBM-HPX-START
C      endif
C SGI-IBM-HPX-DEC-END
C DOS-START
C      irecl   = krecl
C      call touppr (catt(1),formt)
C      call touppr (catt(2),clist)
C      call touppr (catt(4),rdonly)
C     call touppr (catt(3),cacces)
C     if (rdonly(1:5) .eq. 'WRITE') catt(4) = 'OLD'
C      if (rdonly(1:3) .eq. 'NEW') catt(4) = 'UNKNOWN'
C
C      if (formt .eq. 'SEQUENTIAL') then
C        if (clist .eq. 'LIST') then
C          open (unit=kunit,file=cfnam(1:nc),access=catt(1),
C    -            form=catt(3),status=catt(4),iostat=istat,
C    -            carriagecontrol='list',err=9000)
C        else
C          open (unit=kunit,file=cfnam(1:nc),access=catt(1),
C    -            form=catt(3),status=catt(4),iostat=istat,err=9000)
C        end if
C      else if (formt .eq. 'TRANSPARENT') then
C          open (unit=kunit,file=cfnam(1:nc),access=catt(1),
C    -            status=catt(4),iostat=istat,err=9000)
C      else
C        if (clist .eq. 'LIST') then
C          open (unit=kunit,file=cfnam(1:nc),access=catt(1),
C    -            form=catt(3),recl=irecl,status=catt(4),
C    -            iostat=istat,carriagecontrol='list',err=9000)
C        else
C          open (unit=kunit,file=cfnam(1:nc),access=catt(1),
C    -            form=catt(3),status=catt(4),recl=irecl,
C    -            iostat=istat,err=9000)
C        end if
C      endif
C DOS-END
c
      kerr   = 0
      return
c
c...Error trying to open file
c...Check for no such file on old file
c
 9000 kerr   = -1

C WNT-VAX-DEC-START
      if (istat .eq. 29) kerr = -2
C WNT-VAX-DEC-END
C WNT-START
      if (istat .eq. 602) kerr = -2
C WNT-END
C SUN-CIM-START
C     call gerror (lerr)
C     if (lerr .eq. 'can''t find ''old'' file') kerr = -2
C SUN-CIM-END
C SGI-START
C     if (istat .eq. 2) kerr = -2
C SGI-END
C DOS-START
C      IBMERR = istat
C      nr     = iand (istat,4095)
C      if (nr .eq. 71) kerr = -2
C DOS-END
C IBM-START
C     IBMERR = istat
C     if (istat .eq. 6) kerr = -2
C IBM-END
C HPX-START
C      IBMERR = istat
C      if (istat .eq. 908) kerr = -2
C HPX-END
c
      call shfile (cfnam,sbuf,40)
      nc     = strlen1(sbuf)
      cmsg   = '*FATAL*  Error trying to open "' //
     1         sbuf(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  shfile (cin,cout,kmaxc)
c
c   FUNCTION:  This routine shortens a filename if it is more than
c              'kmaxc' characters.  Used to output filenames in error
c              messages, print files, etc.
c
c   INPUT:  cin     C*n  D1  -  Input filename.
c
c           kmaxc   I*4  D1  -  Maximum number of chars in 'fout'.
c
c   OUTPUT: cout    C*n  D1  -  Output filename.
c
c***********************************************************************
c
      subroutine shfile (cin,cout,kmaxc)
c
      include 'menu.inc'
c
      integer*4 kmaxc
c
      character*(*) cin,cout
c
      integer*4 inc,nc1,nc2,strlen1,index,nc11
c
      character*(MAX_PATH) fdev
      character*(MAX_FILE) fnam,fext,ftmp
      character*1 slash
c
C SUN-SGI-IBM-HPX-DEC-START
C      data slash /'/'/
C SUN-SGI-IBM-HPX-DEC-END
C WNT-DOS-START
      data slash /'\'/
C WNT-DOS-END
c
c...Filename is short enough
c
      if (strlen1(cin) .le. kmaxc) then
          cout   = cin
c
c...Shorten filename
c
      else
          call fbreak (cin,fdev,ftmp,fext)
          nc1    = strlen1(ftmp)
          fnam   = ftmp(1:nc1) // fext
          nc1    = strlen1(fdev)
          nc2    = strlen1(fnam)
c
c......Filename by itself is too long
c......Use first part of directory with filename
c
c...          if (nc2 .gt. kmaxc) then
          if (nc2+5 .ge. kmaxc) then
c
c...To consistance with shfile function
c...Yurong
c...              inc = index(fdev,slash)
              inc = index(fdev(5:),slash)
              if (inc .ne. 0) then
                  inc = 5 + inc - 1
                  if (inc .gt. kmaxc/4) then
                      inc = kmaxc / 4
                  endif
                  cout = fdev(1:inc)
              endif
              cout(inc+1:) = '{...}'
              nc11    = inc    + 5
              inc    = nc2 - kmaxc  + nc1 + 1 + nc11
              cout(nc11+1:) = cin(inc:)
c
c......Use partial directory and filename
c
          else
              nc1    = kmaxc  - nc2    - 5
              cout   = fdev(1:nc1)
              cout(nc1+1:) = '{...}'
              cout(nc1+6:) = fnam
          endif
      endif
c
c...End of routine
c
 8000 return
      end
