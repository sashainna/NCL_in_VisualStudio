c
c***********************************************************************
c
c   FILE NAME: pworks.for
c   CONTAINS:
c               pworks  getopc  prsopt  prmtop  prmfil  opnout clsout
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pworks.f , 25.5
c     DATE AND TIME OF LAST  MODIFICATION
c        05/20/16 , 16:28:26
c
c***********************************************************************

c***********************************************************************
c
c   PROGRAM:  pworks
c
c   FUNCTION:  This is the main routine for post-processing clfiles.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      program pworks
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      subroutine pworks
      include "postworks_nt.inc"
C WNT-END
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087)), (MCHOPT,KPOSMP(0308))
      equivalence (MACUST,KPOSMP(3980))
c
      integer*4 IUNIT,MCHOPT(20),MACUST
c
      equivalence (METCNV,POSMAP(0004)), (FTUNT, POSMAP(0005))
      equivalence (RUSER ,POSMAP(0006)), (FTMCNV,POSMAP(0056))
c
      real*8 METCNV, FTUNT, FTMCNV, RUSER(50)
c
      equivalence (LPARTN,CPOSMP(0067)), (PHFILE,CPOSMP(2535))
      equivalence (PDFFIL,CPOSMP(3249))
c
      character*40 PDFFIL,PHFILE
      character*66 LPARTN
c
      integer*4 ierr,iend,j,iop(20),nc,strlen1,irop(20),nrop,i,inum,
     1          iop4,iop7,iwrn,flag
c
      character*80 sbuf,wmsg
c
      character*8 crop(20)
      character*12 dat,pgm
      character*66 lpart
      character*80 msg,sopt
      character*2 nln
      character*80 logfil
      character*(MAX_PATH) ldev
      character*(MAX_FILE) lfil,lext
      byte nlnb(2)
      equivalence (nln, nlnb)
c
      byte lopt(80),lpgm(10),lmsg(80),ldat(12)
      equivalence (sopt,lopt), (pgm,lpgm), (dat,ldat)
c
      data sopt /'LATHE2,MLATHE,MILL3,MILL4,MILL5,MILL7,VMX'/
ccc   call pwdopt (msg,nc,crop,irop,nrop)
c
c...Initialize routine
c
c...W2K will need those information before runs here
c...so we use a function to sets version, revdate and copyright dates
c...then we can just call that one function (in avoiding changes made
c...in 2 places, W2K will call this function in pworks_ntinit function)
c...Yurong 4/11/02
c
c...      PGMNAM = 'PostWorks'
c...      REVDAT = '07.10.98'
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-START
c...only not for WNT
C      call pworks_version
C      call init
C      PREPT  = 0
C      NCCMPF = 0
c
c...Open terminal
c
C      CURSES = 0
C      call trmatt (CURSES)
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-END
c
c...Verify runtime license
c
      call touppr (PGMNAM,pgm)
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
c...Get authorization license
c
      call pwdaut (lpgm,lopt,ldat,LICOPT,lmsg,ierr)
      call pwdbtc (lmsg,msg,nc)
      if (ierr .ne. 0) go to 9000
      call pwdall (lpgm,lmsg,ierr)
      call pwdbtc (lmsg,msg,nc)
      if (ierr .ne. 0) go to 9000
c
c...Get all custom licensed MDF files
c
      call pwdopt (msg,nc,crop,irop,nrop)
      NLICMC = 0
      do 55 i=1,nrop
          call ctoi (crop(i)(1:irop(i)),inum,ierr)
          if (ierr .eq. 0) then
              NLICMC = NLICMC + 1
              LICMCH(NLICMC) = (inum - 715827882) * 3
          end if
   55 continue
c
c...Load data files
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C.....WinNT load these data before open window to run PWork
C      call loddat (0,msg,ierr)
C      if (ierr .ne. 0) go to 9000
c
c...Get input command line & option
C      call getopc (msg,ierr)
C      call trmrst
C      if (ierr .ne. 0) go to 9000
C      if ((NCCMPF.ne.0) .and. RUNQUIET .eq. 1) then
c
c...quiet mode, output error message in pworks.log file
c
C          logfil = 'pworks.log'
C          nc = 10
C          call opnlogfil (logfil, nc)
C      endif
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c...Pworks start
c
      nlnb(1) = 13
      nlnb(2) = 10
      call shfile (LCMPFI,sbuf,60)
      flag = 1
C WNT-START
      call fbreak (LCMPFI,ldev,lfil,lext)
      nc = strlen1(ldev)
      if (ldev .ne. ' ') call pw_chdir(ldev,nc)
      msg(1:80) = nln(1:2) // 'Pworks - ' // sbuf
      nc = strlen1 (msg)
      call add1dispmsg (msg, nc, flag)
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     msg(1:80) = 'Pworks - ' // sbuf
C     call trmnl (1)
C     call trmmsg (msg)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c...Open the input file
c
      call opninp (msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Get MACHIN & PARTNO card from clfile
c
      call machin (iop,msg,ierr)
      if (ierr .ne. 0) go to 9000
      lpart  = LPARTN
c
c...Process clfile for each
c...Machine object file
c
      iend   = PREPT
      iop4   = IOPFL(4)
      iop7   = IOPFL(7)
      do 1000 PREPT=1,iend,1
c
c......Prepare for simulation file
c......Run simulation file on second pass or
c......if this is the only output file requested
c
  100     if ((IOPFL(10) .eq. 2 .and. IOPFL(4) .eq. 0 .and.
     1         IOPFL(7) .eq. 0) .or. IOPFL(10) .eq. 1) then
c
c.........Set simulation file settings
c............Turn off listing, print, and punch files
c............Post generated circular moves
c............Post generated cycles
c............Hold back motion
c
               if (IOPFL(10) .eq. 1) IOPFL(2) = 0
               IOPFL(10) = 3
               IOPFL(4) = 0
               IOPFL(7) = 0
          endif
c
c......Open output files &
c......Rewind clfile
c
          iwrn   = 0
          call opnout (msg,ierr)
          if (ierr .eq. 3) then
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-START
C              call trmnl (1)
C              call trmmsg (msg)
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-END
C WNT-START
              nc = strlen1 (msg)
              flag = 1;
              call add1dispmsg (msg, nc, flag)
C WNT-END
              go to 1000
          endif
          if (ierr .eq. -1) then
              iwrn   = 1
              wmsg   = msg
              ierr   = 0
          endif
          if (ierr .ne. 0) go to 9000
          call clrew
          LPARTN = lpart
c
c......Set required simulation flags
c
          if (IOPFL(10) .eq. 3) then
              call simini
          endif
c
c......Initialize routine
c
          call preini
c
c......Override MACHIN options
c......Reset PARTNO card
c
          do 300 j=1,20,1
              if (iop(j) .ne. -999999) then
                  MCHOPT(j) = iop(j)
                  if (j .eq. 3) call itoc (MCHOPT(j),PDFFIL,nc,0)
              endif
              if (OPTOVR(j) .ne. -987654321) then
                  MCHOPT(j) = OPTOVR(j)
                  if (j .eq. 3) call itoc (MCHOPT(j),PDFFIL,nc,0)
              endif
  300     continue
c
c......Override %USER variables
c
          do 320 j=1,50,1
              if (USROVR(j) .ne. -987654321) then
                  RUSER(j) = USROVR(j)
              endif
  320     continue
c
c......Change default units
c
          if (IUNIT .eq. 1) then
              if (MCHOPT(2) .eq. 2) call metini (2)
          else
              if (MCHOPT(2) .eq. 1) call metini (1)
          endif
c
          if (MCHOPT(1) .eq. MCHOPT(2)) then
              METCNV = 1.0
              FTMCNV = 1.0
          else if (MCHOPT(2) .eq. 2) then
              METCNV = 25.4
              FTMCNV = 12.0d0 * 25.4d0 / 1000.0d0
          else
              METCNV = 1.0D0 / 25.4D0
              FTMCNV = 1000.0d0 / (12.0d0 * 25.4d0)
          endif
c
          if (MCHOPT(2) .eq. 2) FTUNT = 1000.
c
c......Load print file descriptor
c
          if (IOPFL(10) .ne. 3) then
              call cmpprt (msg,ierr)
              if (ierr .ne. 0) go to 9000
          else
              call simhdr (msg,ierr)
              if (ierr .ne. 0) go to 9000
          endif
c
c......Process clfile
c
          if (iwrn .eq. 1) call psterr (1,wmsg,'OUTNOT',-1)
          call postp (msg,ierr)
          if (ierr .ne. 0) go to 9000
          call pstsum
c
c......Write Punch File Header if necessary
c
      if (PHFILE .ne. ' ' .and. IOPFL(7) .ne. 0) then
          call pchhdr (1,msg,ierr)
          if (ierr .ne. 0) go to 9000
          call finrst
      endif
c
c......Close output files
c
          call clsout
c
c......Check for 2nd pass Simulation
c
          if (IOPFL(10) .eq. 2) then
              IOPFL(10) = 1
              go to 100
          else if (IOPFL(10) .eq. 3) then
              IOPFL(10) = 2
              IOPFL(4) = iop4
              IOPFL(7) = iop7
          endif
 1000 continue
c
c...End of routine
c
 8000 IOPFL(2) = 0
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call trmrst
C      call clslogfil
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      call clsfil (0)
	
C WNT-START
      call pwddea (lpgm)
      lpgm(1) = 0
      call pwddea (lpgm)
      return
C WNT-END
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-START
c...only not for WNT
C      call exit (0)
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-END
c
c...An error occurred during
c...execution of program
c
 9000 continue
C WNT-START
      lpgm(1) = 0
      call pwddea (lpgm)
C WNT-END
      if (ierr .ne. -10) call errkil (msg,ierr)
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
C     character*(*) cmsg
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-END
C WNT-START
      character*80 cmsg
C WNT-END
c
      integer*4 ifil,irecl,iopn,nc,strlen1
c
      character*20 att(4),lnum
c
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
      LOPFL(4) = '.pr1'
      IOPFL(5) = 0
      LOPFL(5) = ' '
      IOPFL(6) = 0
      IOPFL(7) = 1
      LOPFL(7) = ' '
      IOPFL(9) = 0
      LOPFL(9) = '.maf'
      IOPFL(10) = 0
      LOPFL(10) = ' '
      IOPFL(11) = 2
      LOPFL(11) = ' '
      IOPFL(12) = 0
      LOPFL(12) = ' '
      FILVAL = 0.
      PSTNAM(1) = ' '
c
c...Open the initialization file
c
      fnam   = 'pworks.ini'
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
c...not for window NT, which we use WINAPI to get option
c
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-START
C 200 call getmcr (ldat,nc)

c......Parse the runtime command line
c
C      if (nc .ne. 0) then
C          call prsopt (ldat,nc,ifil,cmsg,kerr)
C          if (kerr .ne. 0) go to 8000
C      endif

c...Prompt the user for options
c
C      if (IOPFL(1) .eq. 1) then
C          call prmtop (1,cmsg,kerr)
C      else if (ifil .eq. 0) then
C          call prmtop (0,cmsg,kerr)
C      endif

C       if (ICLF .eq. 1 .and. LICOPT(7) .eq. 1) go to 9100
C VAX-SUN-SGI-IBM-HPX-DEC-DOS-END
c
c...End of routine
c
 8000 call clsfil (LUNSC1)
      return
c
c...Invalid option
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
      integer*4 ino,nonc,is,is1,ie,ie1,iopval(18),iop,nop,index,
     1          ncpm,ncop,ierr,i,iary(40),nwds,mop,opmn(8),opmx(8),
     2          quote1, quote2, nindex,inum
      logical lstop
c
      real*8 rary(50)
c
      character*1 ldlm
      character*24 lno,lnum
      character*(MAX_PATH) lparm
c
      data nop /18/
      data iopval /1001,1002,1003,1010,1006,1007,1011,1016,1017,1018,
     1             1019,1020,1021,1026,1028,1029,1030,1031/
c
      data mop /8/
      data opmn /1,1,0,-999999999,10,1,0,0/
      data opmx /2,2,999999999,999999999,512,2,3,1/
c
C VAX-DOS-START
C      data ldlm /'/'/
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
  150     if (cdat(is:is) .ne. ldlm) then
              if (kfil .eq. 1) then
                  if (lstop) goto 9000
                  LCMPFI(NCCMPF+1:NCCMPF+1) = ' '
                  LCMPFI(NCCMPF+2:) = cdat(is:ie)
                  NCCMPF = NCCMPF + ie     - is     + 2
              else if ((cdat(is:is) .ne. '''').and.
     1            (cdat(is:is) .ne. '"')) then
                  kfil   = 1
                  LCMPFI = cdat(is:ie)
                  NCCMPF = ie     - is     + 1
              else
                  kfil   = 1
                  LCMPFI = cdat(is+1:ie-1)
                  NCCMPF = ie     - is     - 1
              endif
              lstop = .false.
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
              lstop = .true.
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
c.........PRINT
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
                  PSTNAM(1) = lparm(1:ncpm)
                  PREPT  = 1
c
c.........IDENT
c
              else if (iop .eq. 6) then
                  if (ncpm .ne. 0) go to 9100
                  IOPFL(6) = 1
                  if (ino .eq. 1) IOPFL(6) = 0
c
c.........PUNCH
c
              else if (iop .eq. 7) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      IOPFL(7) = 0
                  else
                      IOPFL(7) = 1
                      if (ncpm .ne. 0) then
                          LOPFL(7) = lparm
                          IOPFL(7) = 2
                      endif
                  endif
c
c.........WARNING
c
              else if (iop .eq. 8) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      MAXWRN = -1
                  else
                      if (ncpm .ne. 0) then
                          call ctoi (lparm(1:ncpm),MAXWRN,ierr)
                          if (ierr .ne. 0 .or. MAXWRN .lt. 0) go to 9100
                      else
                          MAXWRN = 9999
                      endif
                  endif
c
c.........ERROR
c
              else if (iop .eq. 9) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      MAXPER = -1
                  else
                      if (ncpm .ne. 0) then
                          call ctoi (lparm(1:ncpm),MAXPER,ierr)
                          if (ierr .ne. 0 .or. MAXPER .lt. 0) go to 9100
                      else
                          MAXPER = 9999
                      endif
                  endif
c
c.........FATAL
c
              else if (iop .eq. 10) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      MAXFAT = -1
                  else
                      if (ncpm .ne. 0) then
                          call ctoi (lparm(1:ncpm),MAXFAT,ierr)
                          if (ierr .ne. 0 .or. MAXFAT .lt. 0) go to 9100
                      else
                          MAXFAT = 9999
                      endif
                  endif
c
c.........OPTION
c
              else if (iop .eq. 11) then
                  if (ino .eq. 1 .or. ncpm .eq. 0) go to 9100
                  call geti4n (lparm,ncpm,iary,nwds,ierr)
                  if (ierr .eq. 1 .or. nwds .eq. 0 .or.
     1                nwds/2*2 .ne. nwds) go to 9100
                  do 1000 i=1,nwds,2
                      if (iary(i) .le. 0 .or. iary(i) .gt. mop)
     1                    go to 9100
                      if (iary(i+1) .lt. opmn(iary(i)) .or.
     1                    iary(i+1) .gt. opmx(iary(i))) go to 9100
                      OPTOVR(iary(i)) = iary(i+1)
 1000             continue
c
c.........USERVAR
c
              else if (iop .eq. 18) then
                  if (ino .eq. 1 .or. ncpm .eq. 0) go to 9100
                  call getr8n (lparm,ncpm,rary,nwds,ierr)
                  if (ierr .eq. 1 .or. nwds .eq. 0 .or.
     1                nwds/2*2 .ne. nwds) go to 9100
                  do 1100 i=1,nwds,2
                      inum   = rary(i)
                      if (inum .le. 0 .or. inum .gt. 50) go to 9100
                      USROVR(inum) = rary(i+1)
 1100             continue
c
c.........CLFILE
c
              else if (iop .eq. 12) then
                  is1    = index (lparm(1:ncpm),',')
                  if (is1 .eq. 0) is1 = ncpm + 1
                  if (ino .eq. 1 .or. is1 .lt. 2) go to 9100
                  call ctoi (lparm(1:is1-1),ICLF,ierr)
                  if (ierr .ne. 0 .or. ICLF .lt. 0 .or. ICLF .gt. 6)
     1                    go to 9000
                  if (is1 .lt. ncpm) then
                      call ctoi (lparm(is1+1:ncpm),MCNSRC,ierr)
                      if (ierr .ne. 0 .or. MCNSRC .lt. 1 .or.
     1                MCNSRC .gt. 256) go to 9100
                  end if
c
c.........ADJUST
c
              else if (iop .eq. 13) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      IOPFL(9) = 0
                  else
                      if (ino .eq. 1 .or. ncpm .eq. 0) go to 9100
                      IOPFL(9) = 1
                      LOPFL(9) = lparm
                  end if
c
c.........SIMULATE
c
              else if (iop .eq. 14) then
                  if (ino .eq. 1) then
                      if (ncpm .ne. 0) go to 9100
                      IOPFL(10) = 0
                  else
                      IOPFL(10) = 2
                      if (ncpm .ne. 0) LOPFL(10) = lparm
                  endif
c
c.........QUIET
c
              else if (iop .eq. 15) then
                  if (ncpm .ne. 0) go to 9100
                  RUNQUIET = 1
                  if (ino .eq. 1) RUNQUIET = 0
c
c.........APTERR
c
              else if (iop .eq. 16) then
                  if (ncpm .ne. 0) go to 9100
                  IOPFL(11) = 2
                  if (ino .eq. 1) IOPFL(11) = 0
c
c.........FILL
c
              else if (iop .eq. 17) then
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
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
c...This routine not used by WNT
C
c
C      subroutine prmtop (kfl,cmsg,kerr)
C
C      include 'menu.inc'
C      include 'pregen.inc'
c
C      integer*4 kfl,kerr
c
C      character*(*) cmsg
c
C      integer*4 nca,inum,ierr,nce,strlen1,ifl,inc,iary(40),mop,opmn(4),
C    1          opmx(4),i
c
C      character*80 lans,lerr
c
C      data mop /4/
C      data opmn /1,1,0,-999999999/, opmx /2,2,999999999,999999999/
c
c...Prompt for options
c
C      if (kfl .eq. 1) then
c
c...Listing file
c
C          call prmfil ('PRMLISQ','PRMLIS',IOPFL(2),LOPFL(2),0,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Print file
c
C          call prmfil ('PRMPRTQ','PRMPRT',IOPFL(4),LOPFL(4),0,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Punch file
c
C          call prmfil ('PRMPCHQ','PRMPCH',IOPFL(7),LOPFL(7),0,kerr)
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
c...Identify output file
c
C          call prmfil ('PRMIDENT',' ',IOPFL(6),LOPFL(6),3,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Maximum warnings
c
C          ifl    = 0
C          if (MAXWRN .eq. -1) then
C              inum   = 2
C              LOPFL(8) = ' '
C          else
C              inum   = 1
C              call itoc (MAXWRN,LOPFL(8),nca,0)
C          endif
C 300     call prmfil ('PRMWRNQ','PRMWRN',inum,LOPFL(8),ifl,kerr)
C          if (kerr .ne. 0) go to 8000
c
C          if (inum .eq. 1) then
C              ifl    = 1
C              call remspc (LOPFL(8),lans,nca)
C              if (nca .eq. 0) go to 300
c
C              call ctoi (lans(1:nca),MAXWRN,ierr)
C              if (ierr .ne. 0 .or. MAXWRN .le. 0) then
C                  call errtxt ('INVRESP',lerr)
C                  call trmnl (1)
C                  nce    = strlen1(lerr)
C                  call dmpbuf (lerr,nce)
C                  call trmnl (1)
C                  go to 300
C              endif
C          else
C              MAXWRN = -1
C          endif
c
c...Maximum errors
c
C          ifl    = 0
C          if (MAXPER .eq. -1) then
C              inum   = 2
C              LOPFL(8) = ' '
C          else
C              inum   = 1
C              call itoc (MAXPER,LOPFL(8),nca,0)
C          endif
C 400     call prmfil ('PRMERRQ','PRMERR',inum,LOPFL(8),ifl,kerr)
C          if (kerr .ne. 0) go to 8000
c
C          if (inum .eq. 1) then
C              ifl    = 1
C              call remspc (LOPFL(8),lans,nca)
C              if (nca .eq. 0) go to 400
c
C              call ctoi (lans(1:nca),MAXPER,ierr)
C              if (ierr .ne. 0 .or. MAXPER .le. 0) then
C                  call errtxt ('INVRESP',lerr)
C                  call trmnl (1)
C                  nce    = strlen1(lerr)
C                  call dmpbuf (lerr,nce)
C                  call trmnl (1)
C                  go to 400
C              endif
C          else
C              MAXPER = -1
C          endif
c
c...Maximum fatals
c
C          ifl    = 0
C          if (MAXFAT .eq. -1) then
C              inum   = 2
C              LOPFL(8) = ' '
C          else
C              inum   = 1
C              call itoc (MAXFAT,LOPFL(8),nca,0)
C          endif
C 500     call prmfil ('PRMFATQ','PRMFAT',inum,LOPFL(8),ifl,kerr)
C          if (kerr .ne. 0) go to 8000
c
C          if (inum .eq. 1) then
C              ifl    = 1
C              call remspc (LOPFL(8),lans,nca)
C              if (nca .eq. 0) go to 500
c
C              call ctoi (lans(1:nca),MAXFAT,ierr)
C              if (ierr .ne. 0 .or. MAXFAT .le. 0) then
C                  call errtxt ('INVRESP',lerr)
C                  call trmnl (1)
C                  nce    = strlen1(lerr)
C                  call dmpbuf (lerr,nce)
C                  call trmnl (1)
C                  go to 500
C              endif
C          else
C              MAXFAT = -1
C          endif
c
c...Machine card
c...(Post macro filename)
c
C          ifl    = 0
C 600     call prmfil ('PRMMCHQ','PRMMCH',IOPFL(5),LOPFL(5),ifl,kerr)
C          if (kerr .ne. 0) go to 8000
c
C          if (IOPFL(5) .eq. 1) then
C              ifl    = 1
C              call remspc (LOPFL(5),lans,nca)
C              if (nca .eq. 0) go to 600
c
C              if (nca .eq. 0) then
C                  call errtxt ('INVRESP',lerr)
C                  call trmnl (1)
C                  nce    = strlen1(lerr)
C                  call dmpbuf (lerr,nce)
C                  call trmnl (1)
C                  go to 600
C              endif
C              PSTNAM(1) = lans(1:nca)
C              PREPT  = 1
C          endif
c
c...Machine options
c
C          inc    = 0
C          do 650 i=1,20,1
C              if (OPTOVR(i) .ne. -987654321) then
C                  inc    = inc    + 1
C                  iary(inc) = i
C                  inc    = inc    + 1
C                  iary(inc) = OPTOVR(i)
C              endif
C 650     continue
C          if (inc .eq. 0) then
C              inum   = 2
C              lans   = ' '
C              nca    = 0
C          else
C              inum   = 1
C              call geti4s (iary,inc,lans,nca)
C          endif
C          ifl    = 0
C 700     call prmfil ('PRMOPTQ','PRMOPT',inum,lans,ifl,kerr)
C          if (kerr .ne. 0) go to 8000
C          do 720 i=1,mop,1
C              OPTOVR(i) = -987654321
C 720     continue
c
C          if (inum .eq. 1) then
C              call geti4n (lans,nca,iary,inc,ierr)
c
C              if (inc/2*2 .ne. inc) ierr = 1
C              if (ierr .eq. 0) then
C                  do 740 i=1,inc,2
C                      if (iary(i) .le. 0 .or. iary(i) .gt. mop) then
C                          ierr   = 1
C                          go to 750
C                      endif
C                      if (iary(i+1) .lt. opmn(iary(i)) .or.
C    1                    iary(i+1) .gt. opmx(iary(i))) then
C                          ierr   = 1
C                          go to 750
C                      endif
C                      OPTOVR(iary(i)) = iary(i+1)
C 740             continue
C              endif
c
C 750         if (ierr .ne. 0) then
C                  call errtxt ('INVRESP',lerr)
C                  call trmnl (1)
C                  nce    = strlen1(lerr)
C                  call dmpbuf (lerr,nce)
C                  call trmnl (1)
C                  go to 700
C              endif
C          endif
c
c...Adjust file
c
C          call prmfil ('PRMADJSQ','PRMADJS',IOPFL(9),LOPFL(9),0,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Machine simulation file
c
C          call prmfil ('PRMSIMQ','PRMSIM',IOPFL(10),LOPFL(10),0,kerr)
C          if (kerr .ne. 0) go to 8000
c
c...Input cl file type
c
C          call itoc (ICLF,lans,nca,0)
C 800     call prmfil (' ','PRMCLTYP',nca,lans,1,kerr)
C          if (kerr .ne. 0) go to 8000
C          call ctoi (lans(1:nca),inum,ierr)
c
C          if (inum .lt. 1+LICOPT(7) .or. inum .gt. 6 .or.
C    -                                   ierr .ne. 0) then
C              call errtxt ('INVRESP',lerr)
C              call trmnl (1)
C              nce    = strlen1(lerr)
C              call dmpbuf (lerr,nce)
C              call trmnl (1)
C              go to 800
C          else
C              ICLF   = inum
C          endif
C      endif
c
c...Input file name
c
C2000 call prmfil (' ','PRMFILE',nca,LCMPFI,1,kerr)
C      if (kerr .ne. 0) go to 8000
c...      call remspc (LCMPFI,LCMPFI,nca)
C      nca    = strlen1(LCMPFI)
C      if (nca .eq. 0) go to 2000
c
c...End of routine
c
C8000 cmsg   = ' '
C      return
C      end
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
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
c           kerr    I*4  D1  -  Returns 1 when the user interupted input
c                               via a ^C.
c
c***********************************************************************
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
c...This routine not used by WNT
c
C      subroutine prmfil (cask,caskf,kask,cfil,kfl,kerr)
c
C      include 'menu.inc'
c
C      integer*4 kask,kerr,kfl
c
C      character*(*) cask,caskf,cfil
c
C      integer*4 iyesno(2),ipt,nc,nca,ncyes,ncno,inum,strlen1,nce
c
C      character*24 lyes,lno
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
C
c......Get name of file
c
C 200 if (inum .eq. 1 .and. kfl .ne. 3) then
C          call getsap (caskf,ipt,IPRMDS,SALABL)
C          ldat   = SAPRM(ipt)
C          nc     = SAPNC(ipt) + 1
c
C          lans   = cfil
C          nca    = strlen1(lans)
C
C          call dmpbuf (ldat,nc)
C          call getlin (lans,nca,80,-1,nc+2)
C          call trmnl (1)
C          if (nca .eq. -1) go to 9000
c
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
c
c...Input interrupted with ^C
c
C9000 kerr   = 1
C      go to 8000
C      end
c
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
      include 'post.inc'
c
      equivalence (IPHFLO,KPOSMP(0369))
c
      integer*4 IPHFLO
c
      equivalence (LPARTN,CPOSMP(0067)), (PHFILE,CPOSMP(2535))
      equivalence (PU1EXT,CPOSMP(4067))
c
      character*40 PHFILE
      character*66 LPARTN
      character*80 PU1EXT
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 irecl,nc,ncf,strlen1,iwrn
c
      character*20 att(4)
      character*66 lpart
      character*80 wmsg
      character*(MAX_PATH) ldev,sdev,fnam
      character*(MAX_FILE) lfil,lext,sfil,sext
c
c...Save the PARTNO card
c
      lpart  = LPARTN
c
c...Get Machine number extension
c
      nc     = strlen1(PSTNAM(PREPT))
c
c...Break apart input filename
c
      call fbreak (LCMPFI,ldev,lfil,lext)
c
c...Open Machine Definition File
c
      call opnmch (LUNSC4,PSTNAM(PREPT),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Load the MDF file
c
      iwrn   = 0
      call lodmch (LUNSC4,cmsg,kerr)
      if (kerr .eq. -1) then
          iwrn   = 1
          wmsg   = cmsg
          kerr   = 0
      endif
      if (kerr .ne. 0) go to 8000
c
c...Open Post macro file
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 512
      fnam   = 'pmacro_' // PSTNAM(PREPT)(1:nc) // '.OBJ'
      call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
      if (kerr .eq. -2) then
          call fparse (fnam,fnam,DVDATA,'.OBJ')
          call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
      endif
c
c......Load macro file
c
      if (kerr .eq. 0) then
          call lodcmp (LUNSC4,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      else if (kerr .eq. -2) then
          JHED(1) = 0
          JHED(7) = 0
      else
          go to 8000
      endif
c
c...Open machine adjust file
c
      if (IOPFL(9) .eq. 1) then
          att(1) = 'sequential'
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'old'
          irecl  = 80
          call fbreak (LOPFL(9),sdev,sfil,sext)
          if (sfil .eq. ' ') sfil = lfil
          if (sdev .eq. ' ') sdev = ldev
          if (sext .eq. ' ') sext = '.maf'
          ncf    = strlen1(sfil)
          fnam   = sfil(1:ncf) // sext
          sfil   = fnam
          call opnfil (LUNSC2,sfil,att,irecl,cmsg,kerr)
          if (kerr .eq. -2) then
              call fparse (fnam,fnam,DVDATA,'.maf')
              call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
              if (kerr .ne. 0) IOPFL(9) = 0
          endif
          if (kerr .ne. 0) go to 8000
          call lodmaf (LUNSC2,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
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
              fnam   = sfil(1:ncf) // '_' // PSTNAM(PREPT)(1:nc)
              sfil   = fnam
          endif
          call fparse (sfil,fnam,sdev,sext)
          call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Open print file
c
      if (IOPFL(4) .eq. 1) then
          att(1) = 'sequential'
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'new'
          irecl  = 132
          call fbreak (LOPFL(4),sdev,sfil,sext)
          if (sfil .eq. ' ') sfil = lfil
          if (sdev .eq. ' ') sdev = ldev
          if (sext .eq. ' ') sext = '.pr1'
          if (IOPFL(6) .eq. 1) then
              ncf    = strlen1(sfil)
              fnam   = sfil(1:ncf) // '_' // PSTNAM(PREPT)(1:nc)
              sfil   = fnam
          endif
          call fparse (sfil,fnam,sdev,sext)
          call opnfil (LUNSC3,fnam,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call fparse (fnam,PRTFIL,sdev,sext)
      endif
c
c...Open Punch Header File (.phf)
c
      if (PHFILE .ne. ' ' .and. IOPFL(7) .gt. 0) then
cc          nc     = strlen1(PHFILE)
cc          att(1) = 'sequential'
cc          att(2) = 'list'
cc          att(3) = 'formatted'
cc          att(4) = 'old'
cc          irecl  = 80
cc          fnam   = 'pworks_' // PHFILE(1:nc) // '.phf'
cc          call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
cc          if (kerr .eq. -2) then
cc              call fparse (fnam,fnam,DVDATA,'.phf')
cc              call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
cc          endif
cc          if (kerr .ne. 0) go to 8000
          PHFFIL = fnam
          IPHFLO = 1
cc          call clsfil (LUNSC4)
      endif
c
c...Open punch file
c
      PCHFIL = ' '
      if (IOPFL(7) .gt. 0) then
          att(1) = 'sequential'
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'new'
          irecl  = 512
          if (IOPFL(7) .eq. 1 .and. PU1EXT .ne. ' ') LOPFL(7) = PU1EXT
          call fbreak (LOPFL(7),sdev,sfil,sext)
          if (sfil .eq. ' ') sfil = lfil
          if (sdev .eq. ' ') sdev = ldev
          if (sext .eq. ' ') sext = '.pu1'
          if (IOPFL(6) .eq. 1) then
              ncf    = strlen1(sfil)
              fnam   = sfil(1:ncf) // '_' // PSTNAM(PREPT)(1:nc)
              sfil   = fnam
          endif
          if (PHFILE .ne. ' ') then
              att(4) = 'scratch'
              call fparse (sfil,fnam,sdev,'.tmp')
          else
              call fparse (sfil,fnam,sdev,sext)
          endif
          call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call fparse (sfil,PCHFIL,sdev,sext)
      endif
c
c..........Open simulation file
c
      LPARTN = lpart
      if (IOPFL(10) .eq. 3) call simopn (cmsg,kerr)
c
c...End of routine
c
 8000 if (kerr .eq. 0 .and. iwrn .eq. 1) then
          kerr   = -1
          cmsg   = wmsg
      endif
      return
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
      integer*4 i,irecl
c
      character*20 att(4)
c
      data irecl /512/
      data att /'sequential', 'list', 'formatted', 'write'/
c
c...Close the output files
c
C WNT-DOS-START
      if (IOPFL(2) .eq. 1) endfile (LUNSC2)
      if (IOPFL(4) .eq. 1 .or. IOPFL(10) .gt. 0) endfile (LUNSC3)
      if (IOPFL(7) .gt. 0) endfile (LUNSC4)
C WNT-DOS-END
      call clsfil (LUNSC2)
      call clsfil (LUNSC3)
      call clsfil (LUNSC4)
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
          att(1) = 'sequential'
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'write'
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
