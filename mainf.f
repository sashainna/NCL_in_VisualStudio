c
c***********************************************************************
c
c   FILE NAME: mainf.for
c   CONTAINS:
c               mninit  master  getopc  prsopt  prmtop  prmfil  lodmch
c               savmch
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mainf.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        09/26/17 , 11:59:15
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  mninit (cmsg,kerr)
c
c   FUNCTION:  This routine initializes the MakePost application.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine mninit (cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087))
c
      integer*4 IUNIT
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-START
      integer*4 kerr
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-END
c
      character*(*) cmsg
c
      integer*4 i,ierr,ivnc,isav,nc,irop(8),nrop,iero,iwrn
c
      character*8  crop(20)
      character*12 dat,pgm
      character*40 lmach
      character*80 msg,sopt,wmsg
c
      byte lopt(80),lpgm(12),lmsg(80),ldat(12)
      equivalence (sopt,lopt), (pgm,lpgm), (dat,ldat)
c
      data sopt /'LATHE2,MLATHE,MILL3,MILL4,MILL5,MILL7,RUNTIME'/
c
c...Initialize program
c
c...we use a function to sets version, revdate and copyright dates
c...then we can just change version information in one files version.f
c...for all the Postworks Apps
c...Yurong 4/11/02
c
c...      PGMNAM = 'MakePost'
c...      REVDAT = '08.31.00'
      call mpost_version
      call init
      NCFN = 0
      LOADED = 0
c
c...Initialize PostWorks variables
c
      call pstini
c
c...Attach terminal screen
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
c...not for WNT
c
C      if (MOTIF .eq. 0) call trmatt (CURSES)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c...Verify runtime license
c
      pgm    = 'POSTWORKS'
      call pwdctb (pgm,lpgm)
c
      call pwdctb (sopt,lopt)
c
      call datcnv (REVDAT,dat)
      call pwdctb (dat,ldat)
c
cc temp
      call pwdaut (lpgm,lopt,ldat,LICOPT,lmsg,ierr)
      call pwdbtc (lmsg,msg,nc)
      if (ierr .ne. 0) go to 9000
c
      pgm    = 'MAKEPOST'
      call pwdctb (pgm,lpgm)
      call pwdall (lpgm,lmsg,ierr)
      call pwdbtc (lmsg,msg,nc)
      if (ierr .ne. 0) go to 9000
c
      if (LICOPT(7) .eq. 1) then
          iero   = 1
          go to 60
      endif

c
c...Check if custom makepost
c
      call pwdopt (msg,nc,crop,irop,nrop)
      iero   = 0
      do 55 i=1,nrop
          call ctoi (crop(i)(1:irop(i)),ivnc,ierr)
          if (ierr .eq. 0) then
              iero = 1
              go to 60
          end if
   55 continue
c
c...Open data files &
c...Load Standalone Prompts
c
   60 call loddat (1,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (iero .ne. 0) then
          ierr = 1
          call errtxt ('INVLICO',msg)
          go to 9000
      endif
c
c...Get input command line & options
c

      isav   = IUNIT
      call getopc (lmach,msg,ierr)
      if (ierr .ne. 0) go to 9000
      call trmrst
      CURSES = 1
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
c...not for WNT
c
C      if (MOTIF .eq. 0) call trmatt (CURSES)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c...Convert default Units
c
      if (IUNIT .ne. isav) call metini (IUNIT)
c
c...Load input file
c
      iwrn   = 0
      call lodmch (lmach,msg,ierr)
      if (ierr .eq. -1) then
          iwrn   = 1
          wmsg   = msg
          ierr   = 0
      endif
      if (ierr .ne. 0) go to 9000
c
c...Display banner
c
      call disban
c
c...End of routine
c
 8000 if (ierr .eq. 0 .and. iwrn .eq. 1) then
          ierr   = -1
          msg    = wmsg
      endif
c
c...Error
c
 9000 cmsg   = msg
      kerr   = ierr
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  master
c
c   FUNCTION:  Controlling routine for MakePost.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine master
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087))
      equivalence (MACHTP,KPOSMP(1201)), (MACUST,KPOSMP(3980))
c
      integer*4 IUNIT,MACUST,MACHTP
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 ifl,igo,ierr,ivnc,irnc
c
      character*80 msg,vmsg,rmsg
      character*(40) lname
c
c...Specific level requested
c
      ifl    = NLEVL
      NLEVL  = 1
      if (MLEVL(1) .ne. 0 .or. ifl .ne. 1) go to 200
c
c...Call level 0 menu
c
      SMLEVL(1) = -1
  100 NLEVL  = 1
      MLEVL(NLEVL) = 0
      call menu (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
c
c...Go to appropriate section
c
  200 igo    = MLEVL(NLEVL) + 1
      NLEVL  = NLEVL  + 1
      ierr   = 0
      if (ifl .eq. 0) MLEVL(NLEVL) = 0
      go to (1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     1       2100,2200,2300,2400,2500,2600,2700,8100), igo
c
c...Walk through all levels
c
 1000 IWALK  = 1
c
c...Miscellaneous setup section
c
 1100 MLEVL(NLEVL-1) = 1
      lname  = LMNAME
      call miscsu (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (LMNAME .ne. lname .and. IWALK .eq. 0 .and. MOTIF .eq. 0)then
          NLEVL = 1
          MLEVL(NLEVL) = 0
          call dissec
      endif
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Machine configuration section
c
 1200 MLEVL(NLEVL-1) = 2
      call machcf (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Control tape format section
c
 1300 MLEVL(NLEVL-1) = 3
      call tapfmt (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Print file description
c
 1400 MLEVL(NLEVL-1) = 4
      call pdfdes (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Motion block parameters
c
 1500 MLEVL(NLEVL-1) = 5
      call motblk (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Circular interpolation format
c
 1600 MLEVL(NLEVL-1) = 6
      call cirblk (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Motion adjustments
c
 1700 MLEVL(NLEVL-1) = 7
      call motadj (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Milling cycles
c
 1800 MLEVL(NLEVL-1) = 8
      call cycmil (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Lathe cycles
c
 1900 MLEVL(NLEVL-1) = 9
      call cyclth (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Sequence numbers/Alignment blocks
c
 2000 MLEVL(NLEVL-1) = 10
      call seqaln (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Spindle & Coolant
c
 2100 MLEVL(NLEVL-1) = 11
      call spncol (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Feedrate & Rapid
c
 2200 MLEVL(NLEVL-1) = 12
      call fedrap (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Tool changes
c
 2300 MLEVL(NLEVL-1) = 13
      call tlchg (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Cutter/Fixture compensation
c
 2400 MLEVL(NLEVL-1) = 14
      call cutcmp (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Miscellaneous Post commands
c
 2500 MLEVL(NLEVL-1) = 15
      call mscpst (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Blade Cutter set-up
c
 2600 if (MACHTP .ne. 3) then
          if (IWALK .eq. 1) go to 100
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          if (MOTIF .ne. 1) then
              go to 100
          else
              goto 8000
          endif
      endif
      MLEVL(NLEVL-1) = 16
      call bladcf (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
c...Stringer Mill/Drill set-up
c
 2700 if (MACHTP .ne. 5) then
          if (IWALK .eq. 1) go to 100
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          if (MOTIF .ne. 1) then
              go to 100
          else
              goto 8000
          endif
      endif
      MLEVL(NLEVL-1) = 17
      call strgcf (ifl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      if (MOTIF .eq. 1) go to 8000
      if (IWALK .eq. 0) go to 100
c
      go to 100
c
c...End of routine
c
 8000 return
c
c...Save Machine description file
c
cc 8100 if (IOPFL(3) .ne. 0) then
cc          MACUST = (MACH - 715827882) * 3
cc      else
cc          MACUST = 0
cc      end if
 8100 MACUST = 0
      if (MOTIF .ne. 1) then
          call savmch (0,msg,ierr)
          if (ierr .eq. 2) go to 100
          if (ierr .ne. 0) go to 9000
          if (NCFN.eq.0) goto 8000
      endif
      call docmen (DCFNAM, msg,ierr)
      if (ierr.gt.0) goto 9000
      go to 8000
c
c...An error occurred during
c...execution of program
c
 9000 call trmmsg (' ')
      call trmmsg (msg)
      if (ierr .lt. 0) then
          call errhnd (vmsg,ivnc,rmsg,irnc)
          if (ivnc .ne. 0) call trmmsg (vmsg)
          if (irnc .ne. 0) call trmmsg (rmsg)
      endif
c
      call trmmsg (' ')
      go to 8000
cc      call exit
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getopc (cmach,cmsg,kerr)
c
c   FUNCTION:  This routine processes the initialization file and the
c              runtime command line for the input filename and any op-
c              tions.  The user is prompted for replys if necessary.
c
c   INPUT:  none.
c
c   OUTPUT: cmach   C*n  D1  -  Returns the input Machine descriptor
c                               file name.  The output file name is
c                               stored in LMNAME.  A value of " " means
c                               that a file number was not specified.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine getopc (cmach,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kerr
c
      character*(*) cmach,cmsg
c
      equivalence (MACH  ,KPOSMP(0086))
c
      integer*4 MACH
c
      equivalence (LMNAME,CPOSMP(0086))
c
      character*40 LMNAME
c
      integer*4 irecl,iopn,nc,strlen1,ierr
c
      character*20 att(4)
      character*(MAX_PATH+80) ldat
      character*(MAX_PATH) fnam
c
c...Initialize routine
c
      IOPFL(1) = 0
      IOPFL(2) = 0
      IOPFL(3) = 0
      IOPFL(4) = 60
      cmach  = " "
      LMNAME = " "
      MACH = 0
c
c...Open the initialization file
c
c...Open MPost.ini first
c
      fnam   = 'mpost.ini'
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
      if (kerr .eq.0) then
          iopn   = 1
      else
          call fparse (fnam,fnam,DVDATA,'.ini')
          call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
          if (kerr.eq.0) then
              iopn   = 1
          else
              fnam   = 'makepost.ini'
              att(1) = 'sequential'
              att(2) = 'list'
              att(3) = 'formatted'
              att(4) = 'old'
              irecl  = 80
              call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
              if (kerr .eq. -2) then
                  call fparse (fnam,fnam,DVDATA,'.ini')
                  call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
                  if (kerr .eq. 0) then
                      iopn   = 1
                  else if (kerr .eq. -2) then
                      iopn   = 0
                  else
                      go to 8000
                  endif
              else if (kerr .eq. 0) then
                  iopn   = 1
              else
                  go to 8000
              endif
          endif
      endif
c
c......Read next record from initialization file
c
      kerr   = 0
      if (iopn .eq. 1) then
  100     call rdtxt (LUNSC1,ldat,cmsg,kerr)
          if (kerr .eq. 1) go to 200
          if (kerr .ne. 0) go to 8000
          nc     = strlen1(ldat)
c
c.........Parse this line
c
          if (nc .eq. 0) go to 100
          call prsopt (ldat,nc,cmach,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          go to 100
      endif
c
c...Get the command line from
c...the operating system
c
  200 kerr   = 0
C
C...added command line in WinNT version
c
C WNT-VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
      call getmcr (ldat,nc)
C WNT-VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C... WNT-START
C...      nc = 0
C... WNT-END
c
c......Parse the runtime command line
c
      if (nc .ne. 0) then
          call prsopt (ldat,nc,cmach,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Prompt the user for options
c
      if (IOPFL(1) .eq. 1) then
          call prmtop (1,cmach,cmsg,kerr)
      endif
      if (LMNAME .eq. " ") LMNAME = cmach
      if (LMNAME .eq. " ") LMNAME = "0"
      MACH = 0
      call ctoi (LMNAME,MACH,ierr)
c
c...End of routine
c
 8000 call clsfil (LUNSC1)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prsopt (cdat,knc,cmach,cmsg,kerr)
c
c   FUNCTION:  This routine parses the runtime command line and process-
c              es all options and filenames.
c
c   INPUT:  cdat    C*n  D1  -  Character string to parse.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: cmach   C*n  D1  -  Returns the input Machine descripter
c                               filename.  The output file name is
c                               stored in LMNAME.  A value of " " means
c                               that a file name was not specified.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine prsopt (cdat,knc,cmach,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      integer*4 knc,kerr
c
      character*(*) cdat,cmsg,cmach
c
      equivalence (MACH  ,KPOSMP(0086)), (IUNIT ,KPOSMP(0087))
      equivalence (MCHOPT,KPOSMP(0308)), (MACUST,KPOSMP(3980))
c
      integer*4 IUNIT,MCHOPT(20),MACUST,MACH
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 ino,nonc,is,is1,ie,ie1,iopval(4),iop,nop,index,nindex,
     1          ncpm,ncop,ierr,inum,iuval(3),ifl
c
      character*1 ldlm
      character*24 lno,lnum
      character*80 lparm
c
      data nop /4/
      data iopval /1001,1009,1004,1003/, iuval /14,15,71/
c
      data ldlm /'-'/
c
c...Get value for NO
c
      call getvwd (12,lno,nonc,0,MENWRD,MENWVL,NMENWD)
c
c...Get next parameter
c
      kerr   = 0
      is     = 1
      ifl    = 0
  100 if (is .gt. knc) go to 8000
c
c......Get start of parameter
c
          if (cdat(is:is) .eq. ' ') then
              is1    = nindex(cdat(is:knc),' ')
              if (is1 .eq. 0) go to 8000
              is     = is     + is1    - 1
          endif
c
c......Get end of parameter
c
          if (is .eq. knc) then
              ie     = knc
          else if (cdat(is:is) .eq. '"') then
              is     = is     + 1
              ie     = index(cdat(is:knc),'"')
              if (is1 .ne. 0) then
                  ie     = is     + ie     - 1
              else
                  ie     = knc
              endif
          else
              ie     = index(cdat(is+1:knc),ldlm)
              ie1    = index(cdat(is+1:knc),' ')
              if ((ie1 .ne. 0 .and. ie1 .lt. ie) .or. ie .eq. 0)
     1                ie = ie1
              ie     = is     + ie     - 1
              if (ie .lt. is) ie = knc
          endif
c
c......Machine name
c
          if (cdat(is:is) .ne. ldlm) then
              if (ifl .eq. 2) go to 9000
              ifl    = ifl    + 1
              if (ifl .eq. 1) then
                  call fbpost (cdat(is:ie),DEFDIR,cmach)
              else
                  if (ie-is+1 .gt. 40) go to 9200
                  LMNAME = cdat(is:ie)
                  MACH = 0
                  call ctoi (LMNAME,MACH,ierr)
              endif
c
c......Option
c
          else
c
c.........Remove option's parameter (option:parameter)
c
              is1    = index (cdat(is:ie),':')
              if (is1 .ne. 0) then
                  if (is1 .eq. 2) go to 9100
                  is1    = is     + is1    - 1
                  ie1    = is1    - 1
                  lparm  = cdat(is1+1:ie)
                  ncpm   = ie     - is1
              else
                  lparm  = ' '
                  ie1    = ie
                  ncpm   = 0
              endif
c
c.........Check for NO specifier
c
              call touppr (cdat(is+1:ie1),lnum)
              ncop   = ie1    - is
              if (ncop .gt. 8) then
                  if (lnum(1:nonc) .ne. lno(1:nonc)) go to 9100
                  ino    = 1
                  lnum   = lnum(nonc+1:ncop)
                  ncop   = ncop   - nonc
              else
                  ino    = 0
              endif
c
c.........Get option
c
              if (ncop .gt. 8) go to 9100
  200         call getvnr (lnum(1:ncop),iopval,nop,iop,MENWRD,MENWVL,
     1                     NMENWD)
c
c............Unrecognized option
c............Check for NO specifier
c
              if (iop .eq. 0) then
                  if (ino .ne. 0) go to 9100
                  if (lnum(1:nonc) .ne. lno(1:nonc)) go to 9100
                  ino    = 1
                  lnum   = lnum(nonc+1:ncop)
                  ncop   = ncop   - nonc
                  go to 200
c
c.........PROMPT
c
              else if (iop .eq. 1) then
                  if (ncpm .ne. 0) go to 9100
                  IOPFL(1) = 1
                  if (ino .eq. 1) IOPFL(1) = 0
c
c.........UNITS
c
              else if (iop .eq. 2) then
                  if (ino .eq. 1 .or. ncpm .eq. 0) go to 9100
                  call getvnr (lparm(1:ncpm),iuval,3,inum,MENWRD,MENWVL,
     1                         NMENWD)
                  if (inum .eq. 0) then
                      go to 9100
                  else if (inum .ne. 3) then
                      IUNIT = inum
                      MCHOPT(1) = inum
                      MCHOPT(2) = inum
                  end if
                  IOPFL(2) = inum
c
c.........OBJECT
c
              else if (iop .eq. 3) then
                  if (ino .eq. 1 .or. ncpm .eq. 0) go to 9100
                  call ctoi (lparm(1:ncpm),inum,ierr)
                  if (ierr .ne. 0 .or. inum .ne. 1957) go to 9200
                  IOPFL(3) = inum
c
c.........PAGE_LEN
c
              else if (iop .eq. 4) then
                  if (ino .eq. 1 .or. ncpm .eq. 0) go to 9100
                  call ctoi (lparm(1:ncpm),IOPFL(4),ierr)
                  if (ierr .ne. 0 .or. IOPFL(4) .le. 0) go to 9100
c
c.........Unrecognized option
c
              else
                  go to 9100
              endif
c
c......Go get next parameter
c
          endif
      is     = ie     + 1
      go to 100
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 call errtxt ('SYNOPT',cmsg)
      call errstr (cmsg,cdat(is:ie),1)
      kerr   = 1
      go to 8000
c
c...Invalid option
c
 9100 call errtxt ('INVOPT',cmsg)
      call errstr (cmsg,cdat(is:ie),1)
      kerr   = 1
      go to 8000
c
c...Invalid machine number
c
 9200 call errtxt ('SYNMACH',cmsg)
      call errstr (cmsg,cdat(is:ie),1)
      kerr   = 1
      go to 8000
c
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmtop (kfl,cmach,cmsg,kerr)
c
c   FUNCTION:  This routine prompts the user for each of the recognized
c              runtime options (if kfl=1) and input filename.
c
c   INPUT:  kfl     I*4  D1  -  0 = Prompt for input filename only.
c                               1 = Prompt for options & input filename.
c
c   OUTPUT: cmach   C*n  D1  -  Returns the input Machine descripter
c                               file name.  The output file name is
c                               stored in LMNAME.  A value of " " means
c                               that a file name was not specified.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine prmtop (kfl,cmach,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg,cmach
c
      equivalence (MACH  ,KPOSMP(0086)), (IUNIT ,KPOSMP(0087))
      equivalence (MCHOPT,KPOSMP(0308))
c
      integer*4 IUNIT,MCHOPT(20),MACH
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 nca,inum,ierr,nce,strlen1,iuval(3),ians
c
      character*80 lans,lerr
c
      data iuval /14,15,71/
c
c...Prompt for options
c
      if (kfl .eq. 1) then
c
c...Input Machine Descriptor File
c
          lans   = cmach
          nca    = strlen1(lans)
  200     call prmfil (' ','PRMMCHI',ians,lans,nca,1,kerr)
          if (kerr .ne. 0) go to 8000
          if (nca .eq. 0) then
              cmach  = ' '
          else
              cmach  = lans(1:nca)
          endif
c
c...Output Machine Descriptor File
c
          if (LMNAME .eq. ' ') then
              if (cmach .eq. ' ') then
                  lans   = ' '
                  nca    = 0
              else
                  lans   = cmach
                  nca    = strlen1(lans)
              endif
          else
              lans    = LMNAME
              nca    = strlen1(lans)
          endif
  300     call prmfil (' ','PRMMCHO',ians,lans,nca,1,kerr)
          if (kerr .ne. 0) go to 8000
          if (nca .ne. 0) then
              LMNAME = lans(1:nca)
              MACH = 0
              call ctoi (LMNAME,MACH,ierr)
          endif
c
c...Units
c
          if (IUNIT .eq. 1) then
              call getvwd (14,lans,nca,0,MENWRD,MENWVL,NMENWD)
          else
              call getvwd (15,lans,nca,0,MENWRD,MENWVL,NMENWD)
          endif
  400     call prmfil (' ','PRMUNITS',ians,lans,nca,1,kerr)
          if (kerr .ne. 0) go to 8000
          call getvnr (lans(1:nca),iuval,3,inum,MENWRD,MENWVL,NMENWD)
c
          if (inum .eq. 0) then
              call errtxt ('INVRESP',lerr)
              call trmnl (1)
              nce    = strlen1(lerr)
              call dmpbuf (lerr,nce)
              call trmnl (1)
              go to 400
          else if (inum .ne. 3) then
              IUNIT = inum
              MCHOPT(1) = inum
              MCHOPT(2) = inum
          else
              IOPFL(2) = inum
          end if
c
c...Page length
c
          call itoc (IOPFL(4),lans,nca,0)
  500     call prmfil (' ','PRMPAGL',ians,lans,nca,1,kerr)
          if (kerr .ne. 0) go to 8000
          if (nca .ne. 0) then
              call ctoi (lans(1:nca),inum,ierr)
c
              if (inum .le. 0 .or. ierr .ne. 0) then
                  call errtxt ('INVRESP',lerr)
                  call trmnl (1)
                  nce    = strlen1(lerr)
                  call dmpbuf (lerr,nce)
                  call trmnl (1)
                  go to 500
              else
                  IOPFL(4) = inum
              endif
          endif
      endif
c
c...End of routine
c
 8000 cmsg   = ' '
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmfil (cask,caskf,kask,cfil,kfl,kerr)
c
c   FUNCTION:  This routine prompts the user for a YES/NO answer and if
c              the answer is YES, then prompts the user for a text
c              string associated with this prompt.
c
c   INPUT:  cask    C*n  D1  -  Label of YES/NO prompt.
c
c           caskf   C*n  D1  -  Label of secondary string prompt.
c
c           kfl     I*4  D1  -  0 = Perform YES/NO prompt prior to text
c                               prompt.  1 = Prompt only for the text
c                               string.  Do not perform the YES/NO
c                               prompt.  3 = Perform YES/NO prompt only.
c
c   OUTPUT: kask    I*4  D1  -  0 = YES/NO prompt returned NO.  1 = YES.
c
c           cfil    C*n  D1  -  Text string of secondary prompt.
c
c           knc     I*4  D1  -  Number of chars in 'cfil'.
c
c           kerr    I*4  D1  -  Returns 1 when the user interupted input
c                               via a ^C.
c
c***********************************************************************
c
      subroutine prmfil (cask,caskf,kask,cfil,knc,kfl,kerr)
c
      include 'menu.inc'
c
      integer*4 kask,kerr,kfl,knc
c
      character*(*) cask,caskf,cfil
c
      integer*4 iyesno(2),ipt,nc,nca,ncyes,ncno,inum,strlen1,nce
c
      character*24 lyes,lno
      character*80 ldat,lans,lerr
c
      data iyesno /13,12/
c
c...Check for filename prompt only
c
      kerr   = 0
      if (kfl .eq. 1) then
          inum   = 1
          go to 200
      endif
c
c...Get NO & YES values
c
      call getvwd (iyesno(1),lyes,ncyes,0,MENWRD,MENWVL,NMENWD)
      call getvwd (iyesno(2),lno,ncno,0,MENWRD,MENWVL,NMENWD)
c
c...Ask if the user wants to create this file
c
       call getsap (cask,ipt,IPRMDS,SALABL)
       ldat   = SAPRM(ipt)
       nc     = SAPNC(ipt) + 1
c
  100 lans   = lno
      nca    = ncno
      if (kask .eq. 1) then
          lans   = lyes
          nca    = ncyes
      else
          lans   = lno
          nca    = ncno
      endif
      call dmpbuf (ldat,nc)
      call getlin (lans,nca,8,-1,nc+2)
      call trmnl (1)
      if (nca .eq. -1) go to 8000
      call getvnr (lans,iyesno,2,inum,MENWRD,MENWVL,NMENWD)
c
      if (inum .eq. 0) then
          call errtxt ('INVRESP',lerr)
          call trmnl (1)
          nce    = strlen1(lerr)
          call dmpbuf (lerr,nce)
          call trmnl (1)
          go to 100
      else
          kask   = 0
          if (inum .eq. 1) kask = 1
      endif
c
c......Get name of file
c
  200 if (inum .eq. 1 .and. kfl .ne. 3) then
          call getsap (caskf,ipt,IPRMDS,SALABL)
          ldat   = SAPRM(ipt)
          nc     = SAPNC(ipt) + 1
c
          lans   = cfil
          knc    = strlen1(lans)
          call dmpbuf (ldat,nc)
          call getlin (lans,knc,80,-1,nc+2)
          call trmnl (1)
          if (knc .eq. -1) go to 9000
c
          if (knc .ne. 0) then
              cfil = lans(1:knc)
          else
              cfil = ' '
          endif
      endif
c
c...End of routine
c
 8000 return
c
c...Input interrupted with ^C
c
 9000 kerr   = 1
      go to 8000
      end
c
      subroutine npw_load_machine (cfil,cmsg1,cmsg2,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr
c
      byte cfil(MAX_PATH),cmsg1(80),cmsg2(80)
c
      integer*4 nc,ivnc,irnc
c
      character*80 msg1,rmsg,vmsg
c
c...Load machine descriptor
c
      call pwdbtc (cfil,LCMPFI,nc)
      call lodmch ('~',msg1,kerr)
c
c.........Error loading Machine Descriptor File
c
      if (kerr .ne. 0) then
          call pwdctb (msg1,cmsg1)
          if (kerr .eq. -1) then
              call errtxt ('SETTINGS',rmsg)
              call pwdctb (rmsg,cmsg2)
          else
              call errhnd (vmsg,ivnc,rmsg,irnc)
              if (irnc .ne. 0) then
                  call pwdctb (rmsg,cmsg2)
              else if (ivnc .ne. 0) then
                  call pwdctb (vmsg,cmsg2)
              else
                  cmsg2(1) = 0
              endif
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodmch (cmach,cmsg,kerr)
c
c   FUNCTION:  This routine loads the input Machine descriptor file.
c
c   INPUT:  cmach   C*n  D1  -  Name of Machine descriptor file to
c                               load.  ' ' = No descriptor file specified.
c                               '~' = Filename specified in LCMPFI.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodmch (cmach,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kerr
c
      character*(*) cmsg,cmach
c
      equivalence (IUNIT ,KPOSMP(0087))
      equivalence (NKPOSM,KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM,KPOSMP(0173))
c
      equivalence (MPOSMP,CPOSMP(0001))
c
      integer*4 IUNIT,MPOSMP(2000),NKPOSM,NCPOSM,NPOSMA
c
      equivalence (LMNAME,CPOSMP(0141)), (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
      character*40 LMNAME
c
      integer*4 irecl,nc,i,iuns,mkp,mpo,mcp,is,inkp,inpo,incp,
     1          strlen1,nupd,nrev
c
      character*20 att(4)
      character*40 lsav
      character*(MAX_PATH) lfil
c
      mkp    = 2
      mpo    = 4
      mcp    = 1
c
c...Input Machine was not specified
c
      iuns   = IUNIT
      if (cmach .eq. ' ') then
          if (LMNAME .eq. ' ') LMNAME = '0'
c
c...Set up input file name
c
      else
          lsav   = LMNAME
          if (cmach .eq. '~') then
              call fbpost (LCMPFI,DEFDIR,lfil)
          else
             nc     = strlen1(cmach)
             LCMPFI = 'PWORKS_' // cmach(1:nc) // '.MDF'
          endif
c
c...Open input file
c
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'old'
          irecl  = 8000
          call fparse (LCMPFI,lfil,DEFDIR,'.MDF')
          call opnfil (LUNSC1,lfil,att,irecl,cmsg,kerr)
          if (kerr .eq. -2) then
              call fparse (LCMPFI,LCMPFI,DVDATA,'.MDF')
              call opnfil (LUNSC1,LCMPFI,att,irecl,cmsg,kerr)
          endif
          if (kerr .ne. 0) go to 8000
c
c...Load the first record of Common KPOSMP
c
          inkp   = NKPOSM
          inpo   = NPOSMA
          incp   = NCPOSM
          call rdcom (LUNSC1,1,KPOSMP(1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...Check version of Common arrays
c
          if (NKPOSM .gt. mkp) mkp = NKPOSM
          if (NPOSMA .gt. mpo) mpo = NPOSMA
          if (NCPOSM .gt. mcp) mcp = NCPOSM
          NKPOSM = inkp
          NPOSMA = inpo
          NCPOSM = incp
c
c...Load the Common arrays
c
          do 205 i=2,mkp
              call rdcom (LUNSC1,i,KPOSMP((i-1)*2000+1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  205     continue
          is     = mkp + 1
          do 215 i=is,mkp+mcp
              call rdcom (LUNSC1,i,MPOSMP((i-is)*2000+1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  215     continue
          is     = is + mcp
          do 300 i=is,mkp+mcp+mpo,1
              call rdcom (LUNSC1,i,POSMAP((i-is)*1000+1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  300     continue
c
c...Update MDF data if old file version
c
          call update
          if (LMNAME .eq. ' ') LMNAME = lsav
          if (IOPFL(2) .ne. 3) then
             if (iuns .ne. IUNIT) call metini (iuns)
             IUNIT  = iuns
          end if
          LOADED = 1
      endif
c
c...End of routine
c
 8000 call clsfil (LUNSC1)
c
c...Make sure MDF file is not from
c...a future version of Mpost
c
      if (kerr .eq. 0) then
          call cnvday (LUPDAT,nupd)
          call cnvday (REVDAT,nrev)
          if (nupd .gt. nrev) then
              kerr   = -1
              call errtxt ('MDFVER',cmsg)
              call errstr (cmsg,LUPDAT,0)
              call errstr (cmsg,REVDAT,0)
          endif
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  npw_was_loaded (cfname,klod)
c
c   FUNCTION:  This routine returns whether an external MDF file was
c              loaded and the default name of the active MDF file.
c
c   INPUT:  none
c
c   OUTPUT: cmach   B*1  Dn  -  Default name of Machine descriptor file.
c
c           klod    I*4  D1  -  Returns 1 when an MDF file has been
c                               loaded.
c
c***********************************************************************
c
      subroutine npw_was_loaded (cfname,klod)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 klod
c
      byte cfname(*)
c
      character*(MAX_PATH) fnam,lfil
c
      integer*4 nc,strlen1
c
c...Set loaded flag
c
      klod = LOADED
c
c...Define default filename
c
      if (LMNAME .eq. " ") LMNAME = "0"
      nc = strlen1(LMNAME)
      fnam = 'PWORKS_' // LMNAME(1:nc) // '.MDF'
      call fparse (fnam,lfil,DEFDIR,'.MDF')
      call pwdctb (lfil,cfname)
c
c...End of routine
c
 8000 return
      end
c
c...kerr = 0  -  Normal return.
c...       1  -  File exists, ask to replace it.
c...       2  -  File I/O error.
c
c...kask = 0  -  Need to ask about overwriting file.
c...       1  -  Already been asked.
c
      subroutine npw_save_machine (kask,cmsg1,cmsg2,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr,kask
c
      byte cmsg1(80),cmsg2(80)
c
      integer*4 ivnc,irnc
c
      character*80 msg1,rmsg,vmsg
c
c...Save under current Machine number
c
      kerr   = 0
      call savmch (kask,msg1,kerr)
      call pwdctb (msg1,cmsg1)
c
c.........Error loading Machine Descriptor File
c
      if (kerr .lt. 0) then
          call errhnd (vmsg,ivnc,rmsg,irnc)
          if (irnc .ne. 0) then
              call pwdctb (rmsg,cmsg2)
          else if (ivnc .ne. 0) then
              call pwdctb (vmsg,cmsg2)
          else
              cmsg2(1) = 0
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
      subroutine npw_sim_view (fnam,cmsg1,cmsg2,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr
c
      byte cmsg1(80),cmsg2(80),fnam(MAX_PATH)
c
      integer*4 ivnc,irnc
c
      character*80 msg1,rmsg,vmsg,ldir
      character*(MAX_PATH) lfil
c
      data ldir /'PWORKS_MACH'/
c
c...Load current Machine number
c
      kerr   = 0
      call simviw (ldir,lfil,msg1,kerr)
      call pwdctb (msg1,cmsg1)
      call pwdctb (lfil,fnam)
c
c.........Error loading Machine Simulation File
c
      if (kerr .lt. 0) then
          call errhnd (vmsg,ivnc,rmsg,irnc)
          if (irnc .ne. 0) then
              call pwdctb (rmsg,cmsg2)
          else if (ivnc .ne. 0) then
              call pwdctb (vmsg,cmsg2)
          else
              cmsg2(1) = 0
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c...kerr = 0  -  Normal return.
c...       1  -  File exists, ask to replace it.
c...       2  -  File I/O error.
c
c...kask = 0  -  Need to ask about overwriting file.
c...       1  -  Already been asked.
c
      subroutine npw_mchsim (kask,cmsg1,cmsg2,kerr)
c
      include 'menu.inc'
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-START
      integer*4 kerr,kask
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-END
c
      byte cmsg1(80),cmsg2(80)
c
      integer*4 ivnc,irnc
c
      character*80 msg1,rmsg,vmsg
      character*(MAX_PATH) ldir
c
      data ldir /'PWORKS_MACH'/
c
c...Save under current Machine number
c
      kerr   = 0
      call mchsim (kask,ldir,msg1,kerr)
      call pwdctb (msg1,cmsg1)
c
c.........Error saving Machine Simulation File
c
      if (kerr .lt. 0) then
          call errhnd (vmsg,ivnc,rmsg,irnc)
          if (irnc .ne. 0) then
              call pwdctb (rmsg,cmsg2)
          else if (ivnc .ne. 0) then
              call pwdctb (vmsg,cmsg2)
          else
              cmsg2(1) = 0
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  savmch (kask,cmsg,kerr)
c
c   FUNCTION:  This routine saves the input Machine descriptor file.
c
c   INPUT:  kask    I*4  D1  -  0 = Check to see if file already exists
c                               and notify user of it.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 2 to go back to main menu.
c                               Otherwise, returns non-zero when an
c                               error occurred.
c
c***********************************************************************
c
      subroutine savmch (kask,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kerr,kask
c
      character*(*) cmsg
c
      equivalence (NKPOSM,KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM,KPOSMP(0173))
c
      equivalence (MPOSMP,CPOSMP(0001))
c
      integer*4 MPOSMP(2000),NKPOSM,NPOSMA,NCPOSM
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 irecl,nc,i,inc,iyesno(2),inum,ipt,ivnc,irnc,strlen1,nca
c
      character*20 att(4),sbuf
      character*80 rmsg,vmsg,sdat
c
      data iyesno /13,12/
c
c...Ask user if they want to save
c...the Machine descriptor file
c
      if (MOTIF .eq. 0) then
          call getsap ('Mchsav',ipt,IPRMDS,SALABL)
c
  100     call plott (ISELIN,1)
          call dmpbuf (SAPRM(ipt),SAPNC(ipt))
          call clreol
c
          nc     = 0
  120     call getlin (sbuf,nc,10,ISELIN,SAPNC(ipt)+2)
          if (nc .eq. 0) go to 120
c
c......^C entered
c......Return to Main menu
c
          if (nc .eq. -1) go to 9000
c
c......Check validity of answer
c
          call getvnr (sbuf,iyesno,2,inum,MENWRD,MENWVL,NMENWD)
          if (inum .eq. 0) then
              call errmsg ('INVRSP',1,2)
              go to 120
          endif
c
          if (inum .eq. 2) go to 8000
      endif
c
c...Set up output file name
c
      nc = strlen1(LMNAME)
      LCMPFI = 'PWORKS_' // LMNAME(1:nc) // '.MDF'
      call fparse (LCMPFI,LCMPFI,DEFDIR,'.MDF')
c
c...Check to see if file exists
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 8000
      if (kask .eq. 0) then
          call opnfil (LUNSC1,LCMPFI,att,irecl,cmsg,kerr)
          call clsfil (LUNSC1)
c
c...File already exists
c...Ask user to for permission
c...to overwrite it
c
          if (kerr .ne. -2) then
              if (MOTIF .eq. 1) then
                  call shfile (LCMPFI,cmsg,60)
                  kerr   = 1
                  go to 8000
              else
                  call getsap ('Mchexist',ipt,IPRMDS,SALABL)
                  sdat   = SAPRM(ipt)
                  call shfile (LCMPFI,rmsg,60)
                  call errstr (sdat,rmsg,1)
c
  200             call plott (ISELIN+1,1)
                  nca    = strlen1(sdat)
                  call dmpbuf (sdat,nca)
                  call clreol
c
                  nc     = 0
  220             call getlin (sbuf,nc,10,ISELIN+1,nca+2)
                  if (nc .eq. 0) go to 220
                  call plott (ISELIN+1,1)
                  call clreol
c
c......^C entered
c......Return to Main menu
c
                  if (nc .eq. -1) go to 9000
c
c......Check validity of answer
c
                  call getvnr (sbuf,iyesno,2,inum,MENWRD,MENWVL,NMENWD)
                  if (inum .eq. 0) then
                      call errmsg ('INVRSP',1,2)
                      go to 220
                  endif
c
                  if (inum .eq. 2) go to 9000
              endif
          endif
      endif
c
c...Open output file
c
      att(4) = 'new'
      irecl  = 8000
      call opnfil (LUNSC1,LCMPFI,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 9100
c
c...Store the Common arrays
c
      do 205 i=1,NKPOSM
          call wrcom (LUNSC1,i,KPOSMP((i-1)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 9100
  205 continue
      inc    = NKPOSM + 1
      do 215 i=inc,NKPOSM+NCPOSM
          call wrcom (LUNSC1,i,MPOSMP((i-inc)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 9100
  215 continue
      inc    = inc + NCPOSM
      do 300 i=inc,NKPOSM+NCPOSM+NPOSMA,1
          call wrcom (LUNSC1,i,POSMAP((i-inc)*1000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 9100
  300 continue
      call clsfil (LUNSC1)
c
c...End of routine
c
 8000 if (MOTIF .eq. 0) then
          call plott (ISELIN,1)
          call clreol
          call plott (ISELIN+1,1)
          call clreol
          call plott (ISELIN+2,1)
          call clreol
      endif
      return
c
c...Return to Main menu
c
 9000 kerr   = 2
      go to 8000
c
c...File I/O occurred
c
 9100 call clsfil (LUNSC1)
      if (MOTIF .eq. 0) then
          call plott (ISELIN+1,1)
          call dmpbuf (cmsg,strlen1(cmsg))
          call errhnd (vmsg,ivnc,rmsg,irnc)
          if (irnc .ne. 0) then
              call plott (ISELIN+2,1)
              call dmpbuf (rmsg,irnc)
          else if (ivnc .ne. 0) then
              call plott (ISELIN+2,1)
              call dmpbuf (vmsg,ivnc)
          endif
          call getchr (inum)
      endif
      go to 9000
      end
c
      subroutine npw_create_doc (cfil,cmsg1,cmsg2,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr
c
      byte cfil(MAX_PATH),cmsg1(80),cmsg2(80)
c
      integer*4 nc,ivnc,irnc
c
      character*80 msg1,vmsg,rmsg
      character*(MAX_PATH) filn
c
c...Create document file
c
      call pwdbtc (cfil,filn,nc)
      call docmen (filn,msg1,kerr)
c
c...Error creating documentation file
c
      if (kerr .ne. 0) then
          call pwdctb (msg1,cmsg1)
          call errhnd (vmsg,ivnc,rmsg,irnc)
          if (irnc .ne. 0) then
              call pwdctb (rmsg,cmsg2)
          else if (ivnc .ne. 0) then
              call pwdctb (vmsg,cmsg2)
          else
              cmsg2(1) = 0
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
      subroutine npw_set_default (cfil)
c
      include 'menu.inc'
c
      byte cfil(MAX_PATH)
c
      integer*4 nc
c
      character*(MAX_PATH) filn,lfil
c
c...Set the default directory
c
      call pwdbtc (cfil,filn,nc)
      call fbpost (filn,DEFDIR,lfil)
c
c...End of routine
c
 8000 return
      end
