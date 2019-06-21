C*********************************************************************
C*    NAME         :  fio.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*			fio.for , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*			06/17/15 , 17:59:22
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine flname(ftype, name, fname)
C*       Determine the complete pathname (FNAME) for the file
C*       of the specified type (FTYPE) which has the given
C*       root name (NAME) terminated by { with no embedded blanks.
C*    PARAMETERS
C*       INPUT  :
C*          ftype
C*                               1 => vocabulary file
C*                               2 => dochelp file
C*                               3 => que file
C*                               4 => error file
C*                               5 => part program file
C*                               6 => clfile
C*                               7 => apt source file
C*                               8 => batch listing file
C*                               9 => ranfile file
C*                              10 => scratch file
C*                              11 => synonim vocabulary file
C*                              12 => Macro file
C*                              13 => NCL_INCDIR file
C*          name                 root name of file terminated by {
C*       OUTPUT :
C*         fname                 complete file name
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine flname(ftype, tnam, fname)
 
      include 'com4a.com'
 
      integer*2 ftype
      character*(*) tnam, fname
 
      character*(MAX_PATH) temp, temp2
      integer*4 nc,mxc
      character*(MAX_PATH) ldir
      character*(MAX_PATH) name,fullname
      integer*2 lname,i,len
      integer*4 nco,nci
      logical brafnd,suffnd
      character*4 suffix(8)
      integer*4 strlen1,knc
c
c...Added for working on RS6000. Paul
c...11/22/91
c
      character*80 nclinc,nc00,nc15
      data nc00 /'nc00'/, nc15 /'nc15'/
      data nclinc /'NCL_INCDIR'/
 
      data suffix/'.pp','.cl','.as','.pr','.rf','.dat','.syn','.mac'/
 
c
c     Implemented use of logical names. kathy
c
      if (ftype .le. 4) then
          name = tnam
          nco = strlen1(tnam)
      else
          temp2 = tnam
          nci = strlen1(temp2)
          call getfnm(temp2, nci, name, nco)
      endif
      lname = nco
 
      brafnd=.false.
      suffnd=.false.
c
c...remove trailing spaces
c
      do 500 i=1,lname
         if (name(i:i).eq.'.') suffnd=.true.
         if (name(i:i) .eq. '{' .or.
     1       ichar(name(i:i)) .eq. 0) goto 600
500   continue
600   continue
      i = i - 1
 
      if (ftype .lt. 3 .or. ftype .eq. 4) then
 
c             system files: vocabulary, dochelp, error
c...             call gtenv(nc00,temp,lname)
c...             call ulf_get_full_dir(temp, fullname, lname)
c...             fname = fullname(1:lname) // name(1:i) // '.ncl'
          lname = 4
          name = name(1:i) // '.ncl'
          i = i+4
          call ulf_get_mod_file(nc00, lname, name, i, fname, len)
c.....
c..... Added by Paul to return full file name for synonim voc. file.
c..... 08/06/92.
c.....
	  else if (ftype .eq. 11) then
c             system file: synonim vocabulary file
c...              call gtenv(nc00,temp,lname)
c...              call ulf_get_full_dir(temp, fullname, lname)
c...              fname = fullname(1:lname) // name(1:i) // '.syn'
          lname = 4
          name = name(1:i) // '.syn'
          i = i+4
          call ulf_get_mod_file(nc00, lname, name, i, fname, len)
c.....
c..... End of code for synonim file. Paul.
c.....
 
      else if (ftype .eq. 3) then
c             batch queue file
c
c...Changed for RS6000 using only. Paul
c...11/22/91
c
c...Old version:
c   call gtenv('nc15',temp,lname)
c
c...             call gtenv(nc15,temp,lname)
c...             fname = temp(1:lname) // name(1:i)
c...why add '.ncl', ncl.que file should not have '.ncl' after it
          lname = 4
c          name = name(1:i) // '.ncl'
c         i = i+4
          call ulf_get_mod_file(nc15, lname, name, i, fname, len)
c      fname = '/users/nccs/nc15/' // name(1:i) // '.ncl'
c
c....Ranfil on the HP requires a directory name
c
      else if (ftype .eq. 9) then
          mxc = MAX_PATH
          call getdef(ldir,nc,mxc)
          fname = ldir(1:nc) // name(1:i) // suffix(ftype-4)
 
      else if (ftype .eq. 13) then
c
c...allow multi path for NCL_INCLDIR now
c...so check if the exist, only return the one exist
c
c...
c...          call gtenv(nclinc,temp,lname)
c...          call ulf_get_full_dir(temp, fullname, lname)
c...          fname = fullname(1:lname) // '\\' // name(1:i)
          lname = 10
          call ulf_get_mod_file(nclinc, lname, name, i, fname, len)
          if (.not. suffnd) then
              name = fname(1:strlen1(fname)) // '.mac'
              fname = name
          endif
c             user files: part program, clfile, apt source, batch listing,
c             ranfil, scratch
      else
        if (suffnd) then
          fname = name(1:i)
        else
          fname = name(1:i) // suffix(ftype-4)
        endif
      endif
 
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
 
c
c...Changed for RS6000. Paul
c...11/19/91
c
c     integer*2 lun, frecl, stati2
c
      integer*2 frecl, stati2
      integer*4 lun
c
      character*(*) fname, fstat, facces, fform, fblank
 
      character*20 tstat
      integer*4 flun, stati4, i4recl
c
c...Added   for RS6000. Paul
c...11/19/91
c
c
c     check arguments
 
      stati2 = -1
c      if ((fstat .ne. 'OLD') .and.
c     x    (fstat .ne. 'NEW') .and.
c     x    (fstat .ne. 'SCRATCH')) then
c         write(cout,1000) fstat
c1000     format('ERROR: ',a3,' not in (OLD,NEW,SCRATCH)')
c         call putmsg(cout, 80,1, 0)
c         goto 9000
c      endif
 
c      if ((facces .ne. 'DIRECT') .and. (facces .ne. 'SEQUENTIAL')) then
c         write(cout,1100) facces
c1100     format('ERROR: ',a10,' not in (DIRECT, SEQUENTIAL)')
c         call putmsg(cout, 80,1, 0)
c         goto 9000
c      endif
 
c      if ((fform .ne. 'FORMATTED') .and. (fform .ne. 'UNFORMATTED'))
c     x then
c         write(cout,1200) fform
c1200     format('ERROR: ',a11,' not in (FORMATTED, UNFORMATTED)')
c         call putmsg(cout, 80,1, 0)
c         goto 9000
c      endif
 
c     inquire the status of the requested unit
      flun = lun
      i4recl = frecl
      tstat = fstat
c
c...Commented for RS6000 using only. Paul
c...11/22/91
c
c
      if (fform .eq. 'UNFORMATTED') i4recl = (i4recl+3)/4
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
         if (flun.eq.voclun) then
c                       ***  open the vocabulary file 'shared' (vms)
             open(IOSTAT=stati4, ERR=9999, UNIT=flun, FILE=fname,
     1       STATUS=tstat, ACCESS=facces, FORM=fform)
         else if (flun .eq. scrlun .or.
     1            flun .eq. prtlun .or.
     1            flun .eq. aslun) then
c
c                        ***  open the part program file without fortran
c                             carriage control
c                             also open the apt source file without
c                             fortran carriage control (kathy)
c
             if (fstat .eq. 'SCRATCH') then
                 open(IOSTAT=stati4, ERR=9999, UNIT=flun,
     1               STATUS=tstat, ACCESS=facces)
             else
                 open(IOSTAT=stati4, ERR=9999, UNIT=flun, FILE=fname,
     1               STATUS=tstat, ACCESS=facces, FORM=fform)
             endif
         else
             if (fstat .eq. 'SCRATCH') then
                 open(IOSTAT=stati4, ERR=9999, UNIT=flun,
     1               STATUS=tstat, ACCESS=facces, FORM=fform)
             else
                 open(IOSTAT=stati4, ERR=9999, UNIT=flun, FILE=fname,
     1               STATUS=tstat, ACCESS=facces, FORM=fform)
             endif
         endif
      endif
      if (stati4 .ne. 0) goto 9999
      stati2 = 0
 
9000  continue
      if (stati2 .ne. 0) goto 9999
      return
 
9999  continue
 
      stati2 = stati4
      call flopnv(stati2,stati2)
c     SUN: (stati2.eq.118) make no file on unix look like nofile on vax
c     CIMLINC: (stati2.eq.669) make no file on unix look like nofile on vax
c     PI: (stati2.eq.127) make no file on unix look like nofile on vax
c      if ((stati2.eq.118).or.(stati2.eq.669)
c     x                   .or.(stati2.eq.127)) stati2=29
 
99999 return
      end
 
C*********************************************************************
C*    E_SUBROUTINE     : subroutine flopnw(lun, fname,
C*                            facces, fform, frecl, fblank, stati2)
C*       Open a new file. On unix, if the file already exists, remove it.
C*    PARAMETERS
C*       INPUT  :
C*          lun                  logical unit number to assign file
C*          fname                file name
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
 
      subroutine flopnw(lun, fname, facces, fform, frecl,
     x          fblank, stati2)
 
      include 'com8a.com'
c
c...Changed for RS6000 .  Paul
c...11/19/91
c
c     integer*2 lun, frecl, stati2
c
      integer*2 frecl, stati2
      integer*4 lun
c
      character*(*) fname, facces, fform, fblank
c
      call flopen(lun,fname,'NEW',facces,fform,frecl,fblank,stati2)
c
c...601 is for old file on WIN2K
c...Yurong 3/16/01
c
      if ((stati2.eq.117).or.(stati2.eq.668)
     x    .or.(stati2.eq.126) .or. (stati2.eq.10)
     x    .or. (stati2.eq.601)) then
        call flopen(lun, fname, 'OLD', facces, fform, frecl,
     x        fblank, stati2)
        if (stati2.eq.0) then
            close(unit=lun,status='DELETE')
            call flopen(lun, fname, 'NEW', facces, fform, frecl,
     x        fblank, stati2)
        endif
      endif
      return
      end
 
C*********************************************************************
C*    E_SUBROUTINE     : subroutine flopnm(lun, fname,
C*                            facces, fform, frecl, fblank, stati2)
C*       Open a new file. On unix, if the file already exists, prompt
C*       the user and if it's ok remove it.
C*    PARAMETERS
C*       INPUT  :
C*          lun                  logical unit number to assign file
C*          fname                file name
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
 
      subroutine flopnm(lun, fname, facces, fform, frecl,
     x          fblank, stati2)
 
      include 'com8a.com'
 
c
c...Changed for RS6000. Paul
c...11/19/91
c
c     integer*2 lun, frecl, stati2
c
      integer*2 frecl, stati2
      integer*4 lun, ud_fyesno, nc1, nc2, parent
c
      character*(*) fname, facces, fform, fblank
      character*80  title
c
      call flopen(lun,fname,'NEW',facces,fform,frecl,fblank,stati2)
c
c...601 is for old file on WIN2K
c...Yurong 3/16/01
c
      if ((stati2.ne.117) .and. (stati2.ne.668) .and.
     x    (stati2.ne.126) .and. (stati2.ne.10) .and .
     x    (stati2.ne.601)) go to 99999
      call flopen(lun, fname, 'OLD', facces, fform, frecl,
     x        fblank, stati2)
      if (stati2.ne.0) goto 99999
c
c....close the stream first if open an old file because
c....we could eject from the overwrite question and longjump without
c....close the file, then when we try to open again, the iostate
c...will return a wrong number
c
      close(unit=lun,status='KEEP')
c
c        added a message in case the file already exist. by: kathy
c
      cout = 'Is it ok to overwrite the existing file?'
c
c...Added check for NCL-VT mode
c...Paul  -  10/14/91
c...Old version was:
c   call nclpmt (cout,cin)
c
100   continue
      parent = 0
      nc1 = 42
      title = "File exists, Overwrite?"
      nc2 = 23
      ansint = ud_fyesno(parent, cout, nc1, title, nc2)
      if (ansint .eq. 1) then
c
c....open again and close as DELETE
c
             call flopen(lun, fname, 'OLD', facces, fform, frecl,
     x               fblank, stati2)
             if (stati2.ne.0) goto 99999
             close(unit=lun,status='DELETE')
             call flopen(lun, fname, 'NEW', facces, fform, frecl,
     x                   fblank, stati2)
             goto 99999
      endif
      stati2=-30
99999 return
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
      include 'com.com'
c
      character*(*) cfnami,cfnamo,cdev,cext
c
      integer*4 nc,ifdot,idir,i,inc,strlen1,nci,nco
      integer*2 lname, ierr, len1, len2, len3, len
c
      character*1 space,tab,brak(2),slash,bs,colon,dot
      character*(MAX_PATH) fnam,dev,ldat,path
      byte bb,bt
c
      equivalence (bs,bb), (tab,bt)
c
      data slash /'\\'/, bb /8/
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
c....since we added multi-path support for
c....the diretory could be multi-path, so we can't simply
c....use gtenv to get the path
c....we will check if we can get the path with exist files first
c....if failed, then use the old function below which just add dev and filename
c....but not consider file existance
c
      err = 0
      len1 = strlen1(cfnami)
      len2 = strlen1(cdev)
      len3 = strlen1(cext)
      call ul_fparse(cfnami, len1, path, len, cdev, len2,
     1               cext, len3, ierr)
      if (ierr.eq.0) then
           cfnamo = path
           return
      endif
c
      do 500 i=1,strlen1(fnam),1
          if ((fnam(i:i) .eq. space .or. fnam(i:i) .eq. tab) .and.
     1        nc .eq. 0) go to 500
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
      nc    = strlen1(fnam)
c
c...Add default device
c
      if (idir .eq. 0) then
      cfnamo = cdev
      call gtenv (cdev,cfnamo,lname)
      if (lname .eq. 0) then
          dev = ' '
      else
          nci = lname
          call getfnm(cfnamo(1:lname),nci,dev,nco)
          lname = nco
      endif
      if (lname .ne. 0 .and. dev(1:lname) .ne. space) then
          cfnamo = dev(1:lname)
      else
          cfnamo = cdev
      endif
      inc    = strlen1(cfnamo)
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
