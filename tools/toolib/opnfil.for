c
C*********************************************************************
C*    NAME         :  opnfil.for
C*       CONTAINS:
C*                  adftyp  fbreak  fparse  opnfil
C*
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       opnfil.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:13:29
C********************************************************************/
C
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
=VMS
      inc2   = rindex (cfnami,']')
=UNX
      inc2   = rindex (cfnami,'/')
=W2K
      inc2   = rindex (cfnami,'\\')
=ALL
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
      integer*4 rindex,index,strlen1,is,nc,inc
      character*1 slash
c
=UNX
      data slash /'/'/
=W2K
      data slash /'\\'/
=ALL
c
c...Get device & directory
c
      is     = 1
      nc     = strlen1(cfnam)
=VMS
      is     = index(cfnam,']')
      if (is .eq. 0) then
          is     = index(cfnam,':')
          if (is .eq. 0) then
              cdev   = ' '
              is     = 1
          else
              cdev   = cfnam(1:is)
              is     = is     + 1
          endif
      else
          cdev   = cfnam(1:is)
          is     = is     + 1
      endif
=UNX,W2K
      is     = rindex(cfnam,slash)
      if (is .le. 1) then
          is     = 1
          cdev   = ' '
      else
          cdev   = cfnam(1:is)
          is     = is     + 1
      endif
=ALL
c
      if (is .gt. nc) then
          cfil = ' '
          cext = ' '
          go to 8000
      endif
c
c...Get file name
c
      inc    = index(cfnam(is:nc),'.')
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
      include 'toolib.com'
c
      character*(*) cfnami,cfnamo,cdev,cext
c
      integer*4 nc,nc1,ifdot,idir,i,inc,strlen1, envlen
c
      character*1 space,tab,brak(2),slash,bs,colon,dot
      character*(MAX_PATH) ddir,fnam,dev,ldat
      byte bb,bt
c
      equivalence (bs,bb), (tab,bt)
c
=VMS
      integer*4 ibrak
=UNX
      data slash /'/'/, bb /8/
=W2K
      data slash /'\'/, bb /8/
=ALL
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
cc      if (fnam(i:i) .eq. space .or. fnam(i:i) .eq. tab)
cc     1        go to 500
cc      if (fnam(i:i) .eq. bs) then
cc          if (nc .ne. 0) nc = nc - 1
cc          go to 500
cc      endif
=VMS
          if (fnam(i:i) .eq. brak(1)) ibrak = 1
          if (fnam(i:i) .eq. brak(2)) ibrak = 0
          if (fnam(i:i) .eq. dot .and. ibrak .eq. 0) ifdot = 1
          if (fnam(i:i) .eq. brak(1) .or. fnam(i:i) .eq. colon)
     1            idir = 1
=UNX,W2K
      if (fnam(i:i) .eq. dot) ifdot = 1
      if (fnam(i:i) .eq. slash) then
          ifdot  = 0
          idir   = 1
      endif
=ALL
      nc     = nc     + 1
      fnam(nc:nc) = fnam(i:i)
  500 continue
c
c...Add default device
c
      if (idir .eq. 0) then
          cfnamo = cdev
c
c...need get exactly enviroment name no space
c...yurong changed
c
          envlen = strlen1(cfnamo)
          if (envlen .ne. 0) then
              call fgetenv (cfnamo,dev, envlen)
          endif
          inc = envlen
          if (inc.eq.0) then
              cfnamo = cdev
          else
              inc = index(dev(1:envlen),slash)
              if (inc .ne. 0 .and. inc .ne. 1) then
                  nc1   = inc - 1
                  cfnamo = dev(1:nc1)
                  call fgetenv (cfnamo,ldat,nc1)
                  if (nc1 .eq. 0) then
                      cfnamo = dev(1:envlen)
                  else
                      cfnamo = ldat(1:nc1) // dev(inc:envlen)
                  endif
              else
                  cfnamo = dev(1:envlen)
              endif
          endif
          inc = strlen1(cfnamo)
          if (inc .ne. 0 .and. cfnamo(inc:inc) .ne. slash) then
              ldat   = cfnamo(1:inc) // slash
              cfnamo = ldat
=W2K
          elseif (inc.eq.0) then
c
c...added current directory
c
              call tool_getwd(ddir)
              inc = strlen1(ddir)
              if (ddir(inc:inc) .ne. slash) then
                  cfnamo = ddir (1:inc) // slash
              else
                  cfnamo = ddir (1:inc)
              endif
=ALL
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
      integer*4 kunit,krecl,kerr
c
      character*(*) catt(*),cfnam,cmsg
c
      include 'toolib.com'
c
      integer*4 strlen1,nc,nco
c
      integer*4 irecl,istat
c
      character*80 lbuf
cc=UNX
cc      character*80 lerr
cc=ALL
c
      character*20 rdonly,formt
c
      nc     = strlen1(cfnam)
c
c...Open file with specified attributes
c
=VMS
      call touppr (catt(4),rdonly)
      if (rdonly .eq. 'WRITE') catt(4) = 'OLD'
      irecl   = krecl
      call touppr (catt(3),formt)
      if (formt .eq. 'UNFORMATTED') irecl = (krecl-1) / 4 + 1
      if (rdonly .eq. 'OLD') then
          open (unit=kunit,file=cfnam,access=catt(1),
     1          carriagecontrol=catt(2),form=catt(3),status=catt(4),
     2          recl=irecl,iostat=istat,err=9000,readonly)
      else
          open (unit=kunit,file=cfnam,access=catt(1),
     1          carriagecontrol=catt(2),form=catt(3),status=catt(4),
     2          recl=irecl,iostat=istat,err=9000)
      endif
c
=UNX,W2K
      irecl   = krecl
=IRS,WNT,W2K
      call touppr (catt(3),formt)
      if (formt .eq. 'UNFORMATTED') irecl = (krecl-1) / 4 + 1
=UNX,W2K
      call touppr (catt(4),formt)
      if (formt .eq. 'NEW') formt = 'UNKNOWN'
      if (formt .eq. 'WRITE') formt = 'OLD'
      call touppr (catt(1),rdonly)
c
      if (rdonly .eq. 'SEQUENTIAL') then
          open (unit=kunit,file=cfnam(1:nc),access=catt(1),form=catt(3),
     1          status=formt,iostat=istat,err=9000)
      else
          if (formt .eq. 'SCRATCH') then
              open (unit=kunit,access=catt(1),form=catt(3),status=formt,
     1              recl=irecl,iostat=istat,err=9000)
          else
              open (unit=kunit,file=cfnam(1:nc),access=catt(1),
     1              form=catt(3),status=formt,recl=irecl,iostat=istat,
     2              err=9000)
          endif
      endif
=ALL
c
c...End of routine
c
      kerr   = 0
      return
c
c...Error trying to open file
c...Check for no such file on old file
c
 9000 kerr   = -1
c=UNX
c      call gerror (lerr)
c      if (lerr .eq. 'can''t find ''old'' file') kerr = -2
      if (istat .eq. 29) kerr = -2
      if (istat .eq. 2) kerr = -2
      if (istat .eq. 118) kerr = -2
      if (istat .eq. 602) kerr = -2
      if (istat .eq. 908) kerr = -2
      if (istat .eq. 1018) kerr = -2
      IBMERR = istat
      if (istat .eq. 6) kerr = -2
      nco = 50
      call shfile (cfnam,nc,lbuf,nco)
      nco = strlen1(lbuf)
      cmsg   = '*FATAL*  Error trying to open "' //
     1         lbuf(1:nco) // '".'
=ALL
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine flopen(lun, fname, fstat,
C*                            facces, fform, frecl, fblank, stati2)
C*       Open a file for input/output.
C*    PARAMETERS
C*       INPUT  :
C*          lun                  logical unit number to assign file
C*          fname                file name
C*          fstat                'OLD' => existing file
C*                               'NEW' => new file (does not exist yet)
c*          facces               'DIRECT' => direct access file
C*                               'SEQUENTIAL' => sequential access file
C*          fform                'FORMATTED' => formated file
C*                               'UNFORMATTED' => unformated file
C*          frecl                file record length (meaningfull only
C*                               for direct access files)
C*          fblank               'NULL' => ignore blanks on formatted file
C*                               'ZERO' => zero fill blanks on formatted file
C*                               (ignored for unformatted files)
C*       OUTPUT :
C*          stati2               0 iff no error
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine flopen(lun, fname, fstat, facces, fform, frecl,
     x          fblank, stati2)
 
      include 'com8a.com'
 
      integer*2 frecl, stati2
      integer*4 lun
c
      character*(*) fname, fstat, facces, fform, fblank
 
      character*20 tstat
      integer*4 flun, stati4, i4recl
      logical vms
c
c     check arguments
 
      stati2 = -1
 
c     inquire the status of the requested unit
      flun = lun
      i4recl = frecl
      tstat = fstat
c
      if (fform .eq. 'UNFORMATTED') i4recl = i4recl/4
c
      if (facces .eq. 'DIRECT') then
         if (fstat .eq. 'SCRATCH') then
             open(IOSTAT=stati4, ERR=9999, UNIT=flun,
     1       STATUS=tstat, ACCESS=facces, FORM=fform, RECL=i4recl)
         else
             open(IOSTAT=stati4, ERR=9999, UNIT=flun, FILE=fname,
     1       STATUS=tstat, ACCESS=facces, FORM=fform, RECL=i4recl)
         endif
      else
         if (fstat .eq. 'SCRATCH') then
             open(IOSTAT=stati4, ERR=9999, UNIT=flun,
     1          STATUS=tstat, ACCESS=facces, FORM=fform)
         else
             open(IOSTAT=stati4, ERR=9999, UNIT=flun, FILE=fname,
     1          STATUS=tstat, ACCESS=facces, FORM=fform)
         endif
      endif
      if (stati4 .ne. 0) goto 9999
      stati2 = 0
 
9000  continue
      if (stati2 .ne. 0) goto 9999
      return
 
9999  continue
 
      stati2 = stati4
 
99999 return
      end
 
