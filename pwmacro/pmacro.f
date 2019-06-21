c
c***********************************************************************
c
c   FILE NAME: pmacro.for
c   CONTAINS:
c               pmacro  getopc  prsopt  prmtop  prmfil  opnout clsout
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pmacro.f , 24.4
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/14 , 12:57:19
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM:  pmacro
c
c   FUNCTION:  This is the main routine for processing clfiles and in-
c              terpretting Post Macros.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      program pmacro
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      subroutine pmacro
      include "postworks_nt.inc"
C WNT-END
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (NUMERR,KPOSMP(0082))
c
      integer*4 NUMERR, flag
c
      integer*4 ierr,iend,ivnc,irnc,nc,i,strlen1,iero
c
      character*12 dat,pgm,mpgm(2)
      character*80 msg,vmsg,rmsg,sopt
      character*2 nln
      character*(MAX_PATH) ldev
      character*(MAX_FILE) lfil,lext
      byte nlnb(2)
      equivalence (nln, nlnb)
c
      byte lopt(80),lpgm(10),lmsg(80),ldat(12)
      equivalence (sopt,lopt), (pgm,lpgm), (dat,ldat)
c
      data mpgm /'POSTWORKS','POSTMACRO'/
      data sopt /'LATHE2,MLATHE,MILL3,MILL4,MILL5,MILL7,VMX,RUNTIME'/
c
c...Initialize routine
c
c...W2K will need those information before runs here
c...so we use a function to sets version, revdate and copyright dates
c...then we can just call that one function (in avoiding changes made
c...in 2 places, W2K will call this function in pmacro_ntinit function)
c...Yurong 4/11/02
c...      PGMNAM = 'PostMacro'
c...      REVDAT = '07.21.97'
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C      call pmacro_version
C      call init
C      call pstini
C      PREPT  = 0
C      MEMPT(2) = 0
c
c...Open terminal
c
C      CURSES = 0
C      call trmatt (CURSES)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c...Verify runtime license
c
      iero    = 0
      flag = 1
      do 100 i=1,2,1
          pgm    = mpgm(i)
          nc     = strlen1(pgm)
          lpgm(nc+1) = 0
c
          nc     = strlen1(sopt)
          lopt(nc+1) = 0
c
          call datcnv (REVDAT,dat)
          nc     = strlen1(dat)
          ldat(nc+1) = 0
c
          call pwdaut (lpgm,lopt,ldat,LICOPT,lmsg,ierr)
          if (ierr .eq. 0) go to 150
          if (ierr .ne. 0 .and. i .eq. 2) then
              call pwdbtc (lmsg,msg,nc)
              go to 9000
          endif
  100 continue
  150 continue
c
C WNT-START
      pgm    = mpgm(1)
      nc     = strlen1(pgm)
      lpgm(nc+1) = 0
      call pwdall (lpgm,lmsg,ierr)
      call pwdbtc (lmsg,msg,nc)
      if (ierr .ne. 0) go to 9000
C WNT-END
c
c...Load data files
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C.....WinNT load these data before open window to run PWork
C     call loddat (0,msg,ierr)
C      if (ierr .ne. 0) go to 9000
C      if (LICOPT(8) .eq. 1) then
C          ierr = 1
C          call errtxt ('INVLICO',msg)
C          go to 9000
C      endif
c
c...Get input command line & options
c
C      call getopc (msg,ierr)
C      call trmrst
C      if (ierr .ne. 0) go to 9000
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      if (LICOPT(8) .eq. 1) then
          ierr = 1
          call errtxt ('INVLICO',msg)
          go to 9000
      endif
c
c...Pmacro start
c
      nlnb(1) = 13
      nlnb(2) = 10
      call fbreak (LCMPFI,ldev,lfil,lext)
      nc = strlen1(ldev)
      if (ldev .ne. ' ') call pw_chdir(ldev,nc)
      call shfile (LCMPFI,rmsg,60)
      msg(1:80) = nln(1:2) // 'Pmacro - ' // rmsg
      nc = strlen1 (msg)
      flag = 1;
      call add1dispmsg (msg, nc, flag)
C WNT-END

c
c...Open the input file
c
      call opninp (msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Get MACHIN card from clfile
c
      if (PREPT .eq. 0) then
          call fndmch (msg,ierr)
          if (ierr .ne. 0) go to 9000
      else
          call itoc (PRENUM(PREPT),PSTNAM(PREPT),nc,0)
      endif
c
c...Process clfile for each
c...Machine object file
c
      iend   = PREPT
      do 1000 PREPT=1,iend,1
c
c......Initialize routine
c
          call preini
c
c......Open output files &
c......Rewind clfile
c
          call opnout (msg,ierr)
          if (ierr .ne. 0) go to 9000
          call clrew
c
c......Process clfile
c
          call postp (msg,ierr)
          if (ierr .ne. 0) go to 9000
          if (NUMERR .ne. 0) call erronc
c
c......Run requested post-processors
c
          if (MCHPT .ne. 0) then
              call clsfil (LUNSC3)
              call postit (msg,ierr)
              if (ierr .ne. 0) then
                  ierr   = 0
                  call lsterr (msg,msg,ierr)
                  if (ierr .ne. 0) go to 9000
              endif
          endif
c
c......Close output files
c
          call clsout
 1000 continue
c
c...End of routine
c
 8000 IOPFL(2) = 0
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C      if (ierr .ge. 0 .and. NUMERR .ne. 0) call errsum	
C      call trmrst
C      call clsfil (0)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      if (ierr .ge. 0 .and. NUMERR .ne. 0) then
          call errsum
      else if (ierr .eq. 0 ) then
          msg = "PostMacro completed"
          nc = strlen1(msg)
          call add1dispmsg (msg, nc, flag)
      endif	
      call clsfil (0)
      pgm    = mpgm(1)
      nc     = strlen1(pgm)
      lpgm(nc+1) = 0
      call pwddea (lpgm)
      lpgm(1) = 0
      call pwddea (lpgm)
      return
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C      call exit (0)
c
c...An error occurred during
c...execution of program
C9000 call trmmsg (' ')
C      call trmmsg (msg)
C      if (ierr .lt. 0) then
C          call errhnd (vmsg,ivnc,rmsg,irnc)
C          if (ivnc .ne. 0) then
C              call trmmsg (vmsg)
C          endif
C          if (irnc .ne. 0) then
C              call trmmsg (rmsg)
C          endif
C      endif
c
C      call trmmsg (' ')
C      go to 8000
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 9000 nc = strlen1(msg)
      call add1dispmsg (msg, nc, flag)
c...      if (ierr .lt. 0) then
c...          call errhnd (vmsg,ivnc,rmsg,irnc)
c...          if (ivnc .ne. 0) then
c...              call add1dispmsg (vmsg, ivnc, flag)
c...          endif
c...          if (irnc .ne. 0) then
c...              call add1dispmsg (rmsg, irnc, flag)
c...          endif
c...      endif
      go to 8000
C WNT-END
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getopc (cmsg,kerr)
c
c   FUNCTION:  This routine processes the initialization file and the
c              runtime command line for the input filename and any op-
c              tions.  The user is prompted for replys if necessary.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine getopc (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 kerr
c
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-START
C      character*(*) cmsg
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-END
C WNT-START
      character*80 cmsg
C WNT-END
      integer*4 ifil,irecl,iopn,nc,strlen1
c
      character*20 att(4),lnum
      character*(MAX_PATH+80) ldat
      character*(MAX_PATH) fnam
c
c...Initialize routine
c
      RUNQUIET = 0
      ifil   = 0
      IOPFL(1) = 0
      IOPFL(2) = 0
      LOPFL(2) = '.lis'
      IOPFL(3) = 60
      IOPFL(4) = 1
      LOPFL(4) = '.cln'
      IOPFL(5) = 0
      LOPFL(5) = ' '
      IOPFL(6) = 0
      IOPFL(7) = 0
      IOPFL(11) = 1
      PRENUM(1) = 0
      IOPFL(12) = 0
      LOPFL(12) = ' '
      FILVAL = 0.
c
c...Open the initialization file
c
      fnam   = 'pmacro.ini'
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
c
c......Read next record from initialization file
c
      kerr   = 0
      if (iopn .eq. 1) then
  100     call rdtxt (LUNSC1,ldat,cmsg,kerr)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C          if (kerr .eq. 1) go to 200
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
          if (kerr .eq. 1) then
c
c...we can get filename from interface
c
              kerr = 0
              go to 8000
          endif
C WNT-END
          if (kerr .ne. 0) go to 8000
          nc     = strlen1(ldat)
c
c.........Parse this line
c
          if (nc .eq. 0) go to 100
          call prsopt (ldat,nc,ifil,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          go to 100
      endif
c
c...Get the command line from
c...the operating system
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C 200 call getmcr (ldat,nc)
c
c......Parse the runtime command line
c
C      if (nc .ne. 0) then
C          call prsopt (ldat,nc,ifil,cmsg,kerr)
C          if (kerr .ne. 0) go to 8000
C      endif
c
c...Prompt the user for options
c
C      if (IOPFL(1) .eq. 1) then
C          call prmtop (1,cmsg,kerr)
C      else if (ifil .eq. 0) then
C          call prmtop (0,cmsg,kerr)
C      endif
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C      if (ICLF .eq. 1 .and. LICOPT(7) .eq. 1) go to 9100
c
c...End of routine
c
 8000 call clsfil (LUNSC1)
      return
c
c...End of routine
c
 9100 call errtxt ('INVOPT',cmsg)
      call getvwd (1020,ldat,nc,0,MENWRD,MENWVL,NMENWD)
      call errstr (ldat,'=',1)
      call itoc (ICLF,lnum,nc,0)
      call errstr (ldat,lnum(1:nc),1)
      call errstr (cmsg,ldat,1)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prsopt (cdat,knc,kfil,cmsg,kerr)
c
c   FUNCTION:  This routine parses the runtime command line and process-
c              es all options and filenames.
c
c   INPUT:  cdat    C*n  D1  -  Character string to parse.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: kfil    I*4  D1  -  Returns 1 when a filename was processed.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine prsopt (cdat,knc,kfil,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 knc,kerr,kfil
c
      character*(MAX_PATH+80) cdat
      character*80 cmsg
c
      integer*4 ino,nonc,is,is1,ie,ie1,iopval(11),iop,nop,index,nindex,
     1          ncpm,ncop,ierr,quote1, quote2
      logical lstop
c
      character*1 ldlm
      character*24 lno,lnum
      character*(MAX_PATH) lparm
c
      data nop /11/
      data iopval /1001,1002,1003,1004,1006,1007,1008,1020,1028,1029,
     1             1030/
c
C VAX-DOS-START
C     data ldlm /'/'/
C VAX-DOS-END
C WNT-SUN-SGI-IBM-HPX-DEC-START
      data ldlm /'-'/
C WNT-SUN-SGI-IBM-HPX-DEC-END
c
c...Get value for NO
c
      call getvwd (12,lno,nonc,0,MENWRD,MENWVL,NMENWD)
c
c...Get next parameter
c
      kerr   = 0
      is     = 1
      quote1 = 0
      quote2 = 0
      lstop  = .false.
  100 if (is .gt. knc) go to 8000
c
c......Get start of parameter
c
          if (cdat(is:is) .eq. ' ') then
              is1    = nindex(cdat(is:knc),' ')
              if (is1 .eq. 0) go to 8000
              is     = is     + is1    - 1
          endif
          if (cdat(is:is) .eq. '''') then
              quote1 = 1
          else if (cdat(is:is) .eq. '"') then
              quote2 = 1
          endif
c
c......Get end of parameter
c
          if (is .eq. knc) then
              ie     = knc
              if ((quote1.eq.1).and.(quote2.eq.1)) goto 9000
          else
              if (quote1.eq.1) then
                  ie1 = index(cdat(is+1:knc),'''')
                  if (ie1.eq.0) goto 9000
                  ie = ie1+1
                  quote1 = 0
              else if (quote2.eq.1) then
                  ie1 = index(cdat(is+1:knc),'"')
                  if (ie1.eq.0) goto 9000
                  ie = ie1+1
                  quote2 = 0
              else
cc                  ie     = index(cdat(is+1:knc),ldlm)
                  ie     = index(cdat(is+1:knc),' ')
cc                  if ((ie1 .ne. 0 .and. ie1 .lt. ie) .or. ie .eq. 0 .or.
cc     1                kfil .eq. 0) ie = ie1
              endif
              ie     = is     + ie     - 1
              if (ie .lt. is) ie = knc
          endif
c
c......Filename
c
          if (cdat(is:is) .ne. ldlm) then
              if (kfil .eq. 1) then
                  if (lstop) go to 9000
                  LCMPFI(NCCMPF+1:NCCMPF+1) = ' '
                  LCMPFI(NCCMPF+2:) = cdat(is:ie)
                  NCCMPF = NCCMPF + ie     - is     + 2
              else if ((cdat(is:is) .ne. '''').and.
     1             (cdat(is:is) .ne. '"')) then
                   kfil   = 1
                   LCMPFI = cdat(is:ie)
                   NCCMPF = ie     - is     + 1
              else
                   kfil   = 1
                   LCMPFI = cdat(is+1:ie-1)
                   NCCMPF = ie     - is     - 1
              endif
              lstop  = .false.
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
              lstop  = .true.
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
c.........LISTING
c
              else if (iop .eq. 2) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      IOPFL(2) = 0
                  else
                      IOPFL(2) = 1
                      if (ncpm .ne. 0) LOPFL(2) = lparm
                  endif
c
c.........PAGE_LEN
c
              else if (iop .eq. 3) then
                  if (ino .eq. 1 .or. ncpm .eq. 0) go to 9100
                  call ctoi (lparm(1:ncpm),IOPFL(3),ierr)
                  if (IOPFL(3) .le. 0 .or. ierr .ne. 0) go to 9100
c
c.........OBJECT
c
              else if (iop .eq. 4) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      IOPFL(4) = 0
                  else
                      IOPFL(4) = 1
                      if (ncpm .ne. 0) LOPFL(4) = lparm
                  endif
c
c.........MACHINE
c
              else if (iop .eq. 5) then
                  if (ino .eq. 1 .or. ncpm .eq. 0) go to 9100
                  IOPFL(5) = 1
                  call ctoi (lparm(1:ncpm),PRENUM(1),ierr)
                  if (ierr .ne. 0) go to 9000
                  PREPT  = 1
c
c.........IDENT
c
              else if (iop .eq. 6) then
                  if (ncpm .ne. 0) go to 9100
                  IOPFL(6) = 1
                  if (ino .eq. 1) IOPFL(6) = 0
c
c.........POST
c
              else if (iop .eq. 7) then
                  if (ncpm .ne. 0) go to 9100
                  IOPFL(7) = 1
                  if (ino .eq. 1) IOPFL(7) = 0
c
c.........CLFILE
c
              else if (iop .eq. 8) then
                  is1    = index (lparm(1:ncpm),',')
                  if (is1 .eq. 0) is1 = ncpm + 1
                  if (ino .eq. 1 .or. is1 .lt. 2) go to 9100
                  call ctoi (lparm(1:is1-1),ICLF,ierr)
                  if (ierr .ne. 0 .or. ICLF .lt. 0 .or. ICLF .gt. 6)
     1                    go to 9000
                  if (is1 .lt. ncpm) then
                      call ctoi (lparm(is1+1:ncpm),MCNSRC,ierr)
                      if (ierr .ne. 0 .or. MCNSRC .lt. 1 .or.
     1                MCNSRC .gt. 132) go to 9100
                  end if
c
c...if (iop .eq. 9)
c...QUIET for running batch
c
              else if (iop .eq. 9) then
                  if (ncpm .ne. 0) go to 9100
                  RUNQUIET = 1
                  if (ino .eq. 1) RUNQUIET = 0
c
c.........APTERR
c
              else if (iop .eq. 10) then
                  if (ncpm .ne. 0) go to 9100
                  IOPFL(11) = 1
                  if (ino .eq. 1) IOPFL(11) = 0
c
c.........FILL
c
              else if (iop .eq. 11) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      IOPFL(12) = 0
c
c.........FILL:val
c
                  else
                      IOPFL(12) = -1
                      LOPFL(12) = lparm(1:ncpm)
                      call ctor (lparm(1:ncpm),FILVAL,ierr)
c
c.........FILL:word
c
                      if (ierr .eq. 1) then
                          call touppr (lparm(1:ncpm),lnum)
                          call getvnm (lnum,IOPFL(12),PSTWRD,PSTWVL,
     1                                 NPSTWD)
                          if (IOPFL(12) .eq. 0) go to 9100
                      endif
                  endif
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
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmtop (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine prompts the user for each of the recognized
c              runtime options (if kfl=1) and input filename.
c
c   INPUT:  kfl     I*4  D1  -  0 = Prompt for input filename only.
c                               1 = Prompt for options & input filename.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      subroutine prmtop (kfl,cmsg,kerr)
C
C      include 'menu.inc'
C      include 'pregen.inc'
C
C      integer*4 kfl,kerr
C
C      character*(*) cmsg
C
C      integer*4 nca,inum,ierr,nce,strlen1,ifl
C
C      character*80 lans,lerr
C
C...Prompt for options
C
C      if (kfl .eq. 1) then
c
c...Listing file
c
C          call prmfil ('PRMLISQ','PRMLIS',IOPFL(2),LOPFL(2),0,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Page length
c
C          call itoc (IOPFL(3),lans,nca,0)
C 200     call prmfil (' ','PRMPAGL',nca,lans,1,kerr)
C          if (kerr .ne. 0) go to 8000
C          call ctoi (lans(1:nca),inum,ierr)
c
C          if (inum .le. 0 .or. ierr .ne. 0) then
C              call errtxt ('INVRESP',lerr)
C              call trmnl (1)
C              nce    = strlen1(lerr)
C              call dmpbuf (lerr,nce)
C              call trmnl (1)
C              go to 200
C          else
C              IOPFL(3) = inum
C          endif
c
c...Object file
c
C          call prmfil ('PRMOBJQ','PRMOBJ',IOPFL(4),LOPFL(4),0,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Machine card
c...(Post macro filename)
c
C          ifl    = 0
C 300     call prmfil ('PRMMCHQ','PRMMCH',IOPFL(5),LOPFL(5),ifl,kerr)
C          if (kerr .ne. 0) go to 8000
c
C          if (IOPFL(5) .eq. 1) then
C              ifl    = 1
C              call remspc (LOPFL(5),lans,nca)
C              if (nca .eq. 0) go to 300
c
C              call ctoi (lans(1:nca),PRENUM(1),ierr)
C              if (ierr .ne. 0 .or. PRENUM(1) .le. 0) then
C                  call errtxt ('INVRESP',lerr)
C                  call trmnl (1)
C                  nce    = strlen1(lerr)
C                  call dmpbuf (lerr,nce)
C                  call trmnl (1)
C                  go to 300
C              endif
C              PREPT  = 1
C          endif
c
c...Identify output file
c
C          call prmfil ('PRMIDENT',' ',IOPFL(6),LOPFL(6),3,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Run post-processor
c
C          call prmfil ('PRMPOST',' ',IOPFL(7),LOPFL(7),3,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Cl file type
c
C          call itoc (ICLF,lans,nca,0)
C 350     call prmfil (' ','PRMCLTYP',nca,lans,1,kerr)
C          if (kerr .ne. 0) go to 8000
C          call ctoi (lans(1:nca),inum,ierr)
C
C          if (inum .lt. 1+LICOPT(7) .or. inum .gt. 6 .or.
C    -                                   ierr .ne. 0) then
C              call errtxt ('INVRESP',lerr)
C              call trmnl (1)
C              nce    = strlen1(lerr)
C              call dmpbuf (lerr,nce)
C              call trmnl (1)
C              go to 350
C          else
C              ICLF    = inum
C              IOPFL(8) = ICLF
C          endif
C      endif
c
c...Input file name
c
C 400 call prmfil (' ','PRMFILE',nca,LCMPFI,1,kerr)
C      if (kerr .ne. 0) go to 8000
c...      call remspc (LCMPFI,LCMPFI,nca)
C      nca    = strlen1(LCMPFI)
C      if (nca .eq. 0) go to 400
c
c...End of routine
c
C8000 cmsg   = ' '
C      return
C      end
C
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
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
c           kerr    I*4  D1  -  Returns 1 when the user interupted input
c                               via a ^C.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      subroutine prmfil (cask,caskf,kask,cfil,kfl,kerr)
C
C      include 'menu.inc'
C
C      integer*4 kask,kerr,kfl
C
C      character*(*) cask,caskf,cfil
C
C      integer*4 iyesno(2),ipt,nc,nca,ncyes,ncno,inum,strlen1,nce
C
C      character*8 lyes,lno
C      character*80 ldat,lans,lerr
c
C      data iyesno /13,12/
c
c...Check for filename prompt only
c
C      kerr   = 0
C      if (kfl .eq. 1) then
C          inum   = 1
C          go to 200
C      endif
c
c...Get NO & YES values
c
C      call getvwd (iyesno(1),lyes,ncyes,0,MENWRD,MENWVL,NMENWD)
C      call getvwd (iyesno(2),lno,ncno,0,MENWRD,MENWVL,NMENWD)
c
c...Ask if the user wants to create this file
c
C       call getsap (cask,ipt,IPRMDS,SALABL)
C       ldat   = SAPRM(ipt)
C       nc     = SAPNC(ipt) + 1
c
C 100 lans   = lno
C      nca    = ncno
C      if (kask .eq. 1) then
C          lans   = lyes
C          nca    = ncyes
C      else
C          lans   = lno
C          nca    = ncno
C      endif
C      call dmpbuf (ldat,nc)
C      call getlin (lans,nca,8,-1,nc+2)
C      call trmnl (1)
C      if (nca .eq. -1) go to 8000
C      call getvnr (lans,iyesno,2,inum,MENWRD,MENWVL,NMENWD)
c
C      if (inum .eq. 0) then
C          call errtxt ('INVRESP',lerr)
C          call trmnl (1)
C          nce    = strlen1(lerr)
C          call dmpbuf (lerr,nce)
C          call trmnl (1)
C          go to 100
C      else
C          kask   = 0
C          if (inum .eq. 1) kask = 1
C      endif
c
c......Get name of file
c
C 200 if (inum .eq. 1 .and. kfl .ne. 3) then
C          call getsap (caskf,ipt,IPRMDS,SALABL)
C          ldat   = SAPRM(ipt)
C          nc     = SAPNC(ipt) + 1
C
C          lans   = cfil
C          nca    = strlen1(lans)
C          call dmpbuf (ldat,nc)
C          call getlin (lans,nca,80,-1,nc+2)
C          call trmnl (1)
C          if (nca .eq. -1) go to 9000
C
C          if (nca .ne. 0) then
C              cfil = lans(1:nca)
C          else
C              cfil = ' '
C          endif
C      endif
c
c...End of routine
c
C8000 return
C
C...Input interrupted with ^C
C
C9000 kerr   = 1
C      go to 8000
C      end
C
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c***********************************************************************
c
c   SUBROUTINE:  opnout (cmsg,kerr)
c
c   FUNCTION:  This routine opens some of the input/output files used by
c              this program, for the current Machine number; the Post
c              macro, listing and object files.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine opnout (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 irecl,nc,ncf,strlen1
c
      character*20 att(4),lnum
      character*(MAX_PATH) ldev,fnam,sdev
      character*(MAX_FILE) lfil,lext,sfil,sext
c
c...Get Machine number extension
c
      call itoc (PRENUM(PREPT),lnum,nc,0)
c
c...Break apart input filename
c
      call fbreak (LCMPFI,ldev,lfil,lext)
c
c...Open Post macro file
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 512
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-START
      fnam   = 'pmacro_' // lnum(1:nc) // '.OBJ'
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C     if (nc .gt. 5) nc = 5
C     fnam   = 'p' // lnum(1:nc) // '.OBJ'
C DOS-END
      call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
      if (kerr .eq. -2) then
          call fparse (fnam,fnam,DVDATA,'.OBJ')
          call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
c
c......Load macro file
c
      call lodcmp (LUNSC4,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Open listing file
c
      if (IOPFL(2) .eq. 1) then
          att(1) = 'sequential'
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'new'
          irecl  = 132
          call fbreak (LOPFL(2),sdev,sfil,sext)
          if (sfil .eq. ' ') sfil = lfil
          if (sdev .eq. ' ') sdev = ldev
          if (sext .eq. ' ') sext = '.lis'
          if (IOPFL(6) .eq. 1) then
              ncf    = strlen1(sfil)
              fnam   = sfil(1:ncf) // '_' // lnum(1:nc)
              sfil   = fnam
          endif
          call fparse (sfil,fnam,sdev,sext)
          call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Open object file
c
      if (IOPFL(4) .eq. 1) then
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'new'
          irecl  = 288
          call fbreak (LOPFL(4),sdev,sfil,sext)
          if (sfil .eq. ' ') sfil = lfil
          if (sdev .eq. ' ') sdev = ldev
          if (sext .eq. ' ') sext = '.OBJ'
          if (IOPFL(6) .eq. 1) then
              ncf    = strlen1(sfil)
              fnam   = sfil(1:ncf) // '_' // lnum(1:nc)
              sfil   = fnam
          endif
          call fparse (sfil,LOBJFI,sdev,sext)
          NOBJFI = strlen1(LOBJFI)
          call opnfil (LUNSC3,LOBJFI,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clsout
c
c   FUNCTION:  This routine closes all of the output files used by
c              this program.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clsout
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 i
c
c...Close the output files
c
c...this is a direct-access file, the endfile command cause the segmention error
c
c...      endfile (LUNSC3)
      call clsfil (LUNSC2)
      call clsfil (LUNSC3)
c
c...Close user files
c
      do 100 i=1,5,1
          if (FIOPND(i) .eq. 1) then
              call clsfil (i+20)
              FIOPND(i) = 0
          endif
  100 continue
c
c...Delete any temporary user files
c
      do 200 i=1,FNTEMP,1
          call opnfil (21,FLFNAM(i),att,irecl,msg,ierr)
          if (ierr .eq. 0) call clsdel (21)
  200 continue
      FNTEMP = 0
c
c...Deallocate Macro memory
c
      call memdea
c
c...End of routine
c
 8000 return
      end
