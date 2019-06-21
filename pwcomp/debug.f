c
c***********************************************************************
c
c   FILE NAME:  debug
c   CONTAINS:
c               wrtstm  wrtprn  wrttok  wrtps1  wrtps2  wrtreq  wrtteq
c               wrtpwd  wrtfnc  wrtsyn  wrtif   wrtfmt  dbgtyp
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        debug.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:13
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  wrtstm (cbuf,knc,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine formats an internally stored statement and
c              writes it out to a file.
c
c   INPUT:  cbuf    C*n  D1  -  Statement character string when kfl = 1.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kfl     I*4  D1  -  Defines the level of compiling that we
c                               are currently on.
c
c                               Levels:  1 = Parenthesis parsing.
c                                        2 = Token parsing.
c                                        3 = 1st pass of compiling.
c                                        4 = 2nd pass of compiling.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine wrtstm (cbuf,knc,kfl,cmsg,kerr)
c
      integer*4 knc,kfl,kerr
c
      character*(*) cbuf,cmsg
c
c...Go to appropriate section
c
      go to (100,200,300,400,8000), kfl
c
c...Parenthesis level
c
  100 call wrtprn (cbuf,knc,cmsg,kerr)
      go to 8000
c
c...Token level
c
  200 call wrttok (cmsg,kerr)
      go to 8000
c
c...1st pass Complilation level
c
  300 call wrtps1 (cmsg,kerr)
      go to 8000
c
c...2nd pass Compilation level
c
  400 call wrtps2 (cmsg,kerr)
      go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtprn (cbuf,knc,cmsg,kerr)
c
c   FUNCTION:  This routine formats a parenthesis parsed statement and
c              writes it out to a file.
c
c   INPUT:  cbuf    C*n  D1  -  Statement character string.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine wrtprn (cbuf,knc,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 knc,kerr
c
      character*(*) cbuf,cmsg
c
      integer*4 ist,i,ic,nci,strlen1,nc
c
      character*1 lc
      character*20 lnum
      character*512 ldat
c
c...Initialize routine
c
      ist    = 1
      ldat   = ' '
      nc     = 0
c
c...Search for Parenthesis variable
c
      i      = 0
  100 i      = i      + 1
      if (i .gt. knc) go to 300
      lc     = cbuf(i:i)
c
c......Found parenthesis variable
c......Convert it to text
c
      if (ichar(lc) .eq. 1) then
	  i      = i      + 1
	  ic     = ichar(cbuf(i:i))
	  call itoc (ic,lnum,nci,0)
	  ldat   = ldat(1:nc) // cbuf(ist:i-2) // '^' // lnum(1:nci)
	  ist    = i      + 1
	  nc     = strlen1(ldat)
      endif
c
      go to 100
c
c...Append last of text to output buffer
c
  300 ldat   = ldat(1:nc) // cbuf(ist:knc)
      nc     = strlen1(ldat)
c
c...Write formatted statement
c
      call lstout (ldat,nc,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrttok (cmsg,kerr)
c
c   FUNCTION:  This routine formats a token parsed statement and writes
c              it out to a file.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine wrttok (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i,ist,nci,nc,iflw
c
      character*2 ldelim(18)
      character*20 lnum
      character*512 ldat
c
      data ldelim /'= ',', ',': ','+ ','- ','* ','/ ','**','==','< ',
     1             '> ','<>','<=','>=','&','|','(',')'/
c
c...Initialize routine
c
      ist    = 1
      ldat   = ' '
      nc     = 0
      iflw   = 1
c
c...Parse token array
c
      do 500 i=1,NTOK,1
c
c......Vocabulary word
c
	  if (ICTYP(i) .eq. 1) then
	      ist    = RCSUB(i)
	      call getvwd (ist,lnum,nci,iflw,PSTWRD,PSTWVL,NPSTWD)
	      ldat   = ldat(1:nc) // lnum(1:nci) // ' '
	      nc     = nc     + nci    + 1
c
c......Operator
c
	  else if (ICTYP(i) .eq. 2) then
	      ist    = RCSUB(i)
	      ldat   = ldat(1:nc) // ldelim(ist) // ' '
	      nc     = nc     + 3
c
c......Number
c
	  else if (ICTYP(i) .eq. 3) then
	      call rtoc (RCSUB(i),lnum,nci)
	      ldat   = ldat(1:nc) // lnum(1:nci) // ' '
	      nc     = nc     + nci    + 1
c
c......Text String
c
	  else if (ICTYP(i) .eq. 4) then
	      ist    = RCSUB(i)
	      ldat   = ldat(1:nc) // LCTXT(ist:ICNC(i)) // ' '
	      nc     = nc     + (ICNC(i)-ist+1) + 1
c
c......Real scalar variable
c
	  else if (ICTYP(i) .eq. 5) then
	      ist    = RCSUB(i)
	      call itoc (ist,lnum,nci,0)
	      ldat   = ldat(1:nc) // '^' // lnum(1:nci) // ' '
	      nc     = nc     + nci    + 2
c
c......Text variable
c
	  else if (ICTYP(i) .eq. 6) then
	      ist    = RCSUB(i)
	      call itoc (ist,lnum,nci,0)
	      ldat   = ldat(1:nc) // '@' // lnum(1:nci) // ' '
	      nc     = nc     + nci    + 2
c
c......Quoted Text String
c
	  else if (ICTYP(i) .eq. 7) then
	      ist    = RCSUB(i)
	      ldat   = ldat(1:nc) // '"' // LCTXT(ist:ICNC(i)) // '" '
	      nc     = nc     + (ICNC(i)-ist+1) + 3
c
c......Format number
c
	  else if (ICTYP(i) .eq. 8) then
	      ist    = RCSUB(i)
	      call itoc (ist,lnum,nci,0)
	      ldat   = ldat(1:nc) // '#' // lnum(1:nci) // ' '
	      nc     = nc     + nci    + 2
c
c......Unrecognized type
c
	  else
	      ldat   = ldat(1:nc) // '? '
	      nc     = nc     + 2
	  endif
	  iflw   = 2
  500 continue
c
c...Write formatted statement
c
      call lstout (ldat,nc,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtps1 (cmsg,kerr)
c
c   FUNCTION:  This routine reads the Macro work file, formats it's re-
c              cords and writes them out to a file.  It should be called
c              after the 1st compilation pass.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message when an error oc-
c
c           kerr    I*4  D1  -  Returns 1 when an error occurs.
c
c***********************************************************************
c
      subroutine wrtps1 (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i,inc,nc,j,ibuf(2),nci,strlen1,igo,is1,is2,is3,ityp,iwd,
     1          jpc,nc1,m,ipt,ist,ilab,inum,ii
c
      character*20 lnum
      character*24 lbuf
      character*80 ldat,ltemp
c
      equivalence (lbuf,ibuf)
c
c...Write header
c
      ldat   = char(12) // '** 1st Pass Compilation **'
      nc     = strlen1(ldat)
      call lstout (ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      ldat   = ' '
      call lstout (ldat,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Prepare to read Macro Index record
c
      MFDAT(2,1) = 1
      MFDAT(2,1) = MFPT(1,1)
      inc    = 128
      jpc    = 0
      do 1000 i=1,NMACRO,1
c
c...Read Macro Index Record
c
	  if (inc .gt. 120) then
	      call lodwrk (MFDAT(2,1),MFDAT(1,1),cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	      inc    = MNFIX(1) + 1
	  endif
c
c......Get Macro name
c
	  call getvwd (MFDAT(inc,1),ldat,nc,1,PSTWRD,PSTWVL,NPSTWD)
c
c......Write Macro record
c
	  lbuf   = ' '
	  call lstout (lbuf,1,cmsg,kerr)
	  if (kerr .ne. 0) go to 8000
	  call getvwd (5006,lbuf,nci,0,PSTWRD,PSTWVL,NPSTWD)
	  call itoc (MFDAT(inc+11,1),lnum,nc1,0)
	  ldat   = ldat(1:nc) // '/' // lbuf(1:nci) // ',' //
     1             lnum(1:nc1)
	  nc     = nc     + nci    + nc1    + 2
	  call lstout (ldat,nc,cmsg,kerr)
	  if (kerr .ne. 0) go to 8000
c
c......Write allocated arguments
c
	  if (MFDAT(inc+11,1) .ne. 0) then
	      call getvwd (4008,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      ldat   = ldat(1:nc) // ' '
	      nc     = nc     + 1
c
c.........%NARG
c
	      call getvwd (6002,lbuf,nci,0,PSTWRD,PSTWVL,NPSTWD)
	      call itoc (MFDAT(inc+9,1),lnum,nc1,0)
	      ldat   = ldat(1:nc) // lbuf(1:nci) // ':' //
     1                 lnum(1:nc1) // ' '
	      nc     = nc     + nci    + nc1    + 2
c
c.........REAL %ARG
c
	      call getvwd (6000,lbuf,nci,0,PSTWRD,PSTWVL,NPSTWD)
	      call itoc (MFDAT(inc+10,1),lnum,nc1,0)
	      ldat   = ldat(1:nc) // lbuf(1:nci) // ':' //
     1                 lnum(1:nc1)
	      nc     = nc     + nci    + nc1    + 1
	      call itoc (MFDAT(inc+11,1),lnum,nc1,0)
	      ldat   = ldat(1:nc) // '(' // lnum(1:nc1) // ')'
	      nc     = nc     + nc1    + 3
c
	      call lstout (ldat,nc,cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
c
c.........CHAR %ARG
c
	      call getvwd (4007,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      ldat   = ldat(1:nc) // ' '
	      nc     = nc     + 1
c
	      call itoc (MFDAT(inc+12,1),lnum,nc1,0)
	      ldat   = ldat(1:nc) // lbuf(1:nci) // ':' //
     1                 lnum(1:nc1)
	      nc     = nc     + nci    + nc1    + 1
	      call itoc (MFDAT(inc+13,1),lnum,nc1,0)
	      ldat   = ldat(1:nc) // '(' // lnum(1:nc1) // ')'
	      nc     = nc     + nc1    + 3
c
	      call lstout (ldat,nc,cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	  endif
c
c......Write Allocated Variables
c
	  iwd    = 4008
	  is1    = inc    + 3
	  is2    = inc    + 4
	  is3    = 3
	  ityp   = 1
c
	  do 140 m=1,2,1
	      call getvwd (iwd,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      ldat   = ldat(1:nc) // ' '
	      nc     = nc     + 1
	      do 100 j=MFDAT(is1,1),MFDAT(is2,1),1
		  call lodscl (ityp,j,ipt,cmsg,kerr)
		  if (kerr .ne. 0) go to 8000
c
        do 80 ii=1,6,1
		      ibuf(ii) = MFDAT(ii+ipt-1,is3)
   80   continue
		  if (lbuf .eq. ' ') go to 100
		  nci    = strlen1(lbuf)
		  ltemp  = lbuf(1:nci)
c
		  call itoc (MFDAT(ipt+3,is3),lnum,nc1,0)
		  ltemp  = ltemp(1:nci) // ':' // lnum(1:nc1)
		  nci    = nci    + nc1    + 1
c
		  call itoc (MFDAT(ipt+6,is3),lnum,nc1,0)
		  ltemp  = ltemp(1:nci) // '(' // lnum(1:nc1) // ') '
		  nci    = nci    + nc1    + 3
c
		  if (nc+nci .gt. 80) then
		      call lstout (ldat,nc,cmsg,kerr)
		      if (kerr .ne. 0) go to 8000
		      nc     = 5
		  endif
		  ldat   = ldat(1:nc) // ltemp(1:nci)
		  nc     = nc     + nci
  100         continue
	      if (nc .gt. 5) then
		  call lstout (ldat,nc,cmsg,kerr)
		  if (kerr .ne. 0) go to 8000
	      endif
c
	      iwd    = 4007
	      is1    = inc    + 5
	      is2    = inc    + 6
	      is3    = 4
	      ityp   = 2
  140     continue
c
c......Write compiled macro statements
c......Load 1st record
c
	  if (MFDAT(1,5) .ne. MFDAT(inc+7,1)) then
	      call lodwrk (MFDAT(inc+7,1),MFDAT(1,5),cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	  endif
	  ist    = MFDAT(inc+8,1)
c
c.........Load Label record
c
	   if (MFDAT(inc+2,1) .ge. MFDAT(inc+1,1)) then
	       ilab   = MFDAT(inc+1,1)
	       call lodlab (ilab,ipt,cmsg,kerr)
	       if (kerr .ne. 0) go to 8000
	   endif
c
c.........Check for Label at this location
c
  200     if (ilab .le. MFDAT(inc+2,1)) then
	      if (MFDAT(ipt+2,2) .eq. jpc) then
        do 201 ii=1,6,1
		      ibuf(ii) = MFDAT(ii+ipt-1,2)
  201   continue
		  nci    = strlen1(lbuf)
		  ldat   = lbuf(1:nci) // ':'
		  nc     = nci    + 1
		  call lstout (ldat,nc,cmsg,kerr)
		  if (kerr .ne. 0) go to 8000
		  ilab   = ilab   + 1
		  if (ilab .le. MFDAT(inc+2,1)) then
		      call lodlab (ilab,ipt,cmsg,kerr)
		      if (kerr .ne. 0) go to 8000
		  endif
c
c............Go check for another
c............Label at this location
c
		  go to 200
	      endif
	  endif
c
c.........Load compiled record
c
	  call lodcmp (ist,cmsg,kerr)
	  if (kerr .ne. 0) go to 8000
	  jpc    = jpc    + ICMPL(1)
c
c.........Go to appropriate section
c
	  igo    = ICMPL(2) + 1
	  go to (205,210,220,230,240,250,260,270,270,290,295,297), igo
c
c............Continue
c
  205         call getvwd (4005,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      go to 300
c
c............Real equation
c
  210         call wrtreq (ldat,nc)
	      go to 300
c
c............Text equation
c
  220         call wrtteq (ldat,nc)
	      go to 300
c
c............Post word
c
  230         call wrtpwd (ldat,nc)
	      go to 300
c
c............JUMPTO
c
  240         call getvwd (4014,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      ldat   = ldat(1:nc) // '/ ' // LCMPL(5:28)
	      nc     = nc     + 10
	      go to 300
c
c............IF
c
  250         call wrtif (ldat,nc)
	      go to 300
c
c...........TERMAC
c
  260         call getvwd (4010,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      call lstout (ldat,nc,cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	      go to 900
c
c............Mult-arg function
c
  270         call wrtfnc (ldat,nc)
	      go to 300
c
c............SYNTAX
c
  290         call wrtsyn (ldat,nc)
	      go to 300
c
c............ENABLE/DISABLE
c
  295         if (ICMPL(3) .eq. 0) then
		  call getvwd (4016,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      else
		  call getvwd (4015,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      endif
	      inum   = ICMPL(4)
	      call getvwd (inum,lnum,nci,0,PSTWRD,PSTWVL,NPSTWD)
	      ldat   = ldat(1:nc) // '/ ' // lnum(1:nci)
	      nc     = nc     + nci    + 2
	      go to 300
c
c............FORMAT specification
c
  297         call wrtfmt (ldat,nc)
	      go to 300
c
c.........Write out formatted compiler record
c
  300     call lstout (ldat,nc,cmsg,kerr)
	  if (kerr .ne. 0) go to 8000
c          ist    = ist    + ICMPL(ist)
	  go to 200
c
c.........Point to next MACRO
c
  900     inc    = inc    + MNREC(1)
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtps2 (cmsg,kerr)
c
c   FUNCTION:  This routine reads the Object file, formats its records
c              and writes them out to the listing file.  It should be
c              called after the 2nd compilation pass.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message when an error oc-
c
c           kerr    I*4  D1  -  Returns 1 when an error occurs.
c
c***********************************************************************
c
      subroutine wrtps2 (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 ihed(16),imac(8)
c
      integer*4 i,j,irec,inc,nc,nc1,nc2,strlen1,igo,jhed(8),jmac(4),ipt,
     1          nmac,inum
c
      character*20 lnum
      character*24 lbuf
      character*80 ldat
c
      equivalence (jhed,ihed), (jmac,imac)
c
c...Write header
c
      ldat   = char(12) // '** 2nd Pass Compilation **'
      nc     = strlen1(ldat)
      call lstout (ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      ldat   = ' '
      call lstout (ldat,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Read header record
c
      call rdprm (LUNSC3,1,PFDAT(1,1),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      do 100 i=1,7,1
	  jhed(i) = PFDAT(i,1)
  100 continue
c
c...Process Macro records
c
      irec   = jhed(1)
      inc    = ihed(14) - 1
      nmac   = ihed(13)
      do 400 i=1,nmac,1
c
c......Store Macro header
c
	  do 200 j=1,8,1
c
c.........Read next Macro record
c
	      if (inc .ge. 256) then
		  irec   = irec   + 1
		  call rdprm (LUNSC3,irec,PFDAT(1,1),cmsg,kerr)
		  if (kerr .ne. 0) go to 8000
		  inc    = 0
	      endif
	      inc    = inc    + 1
	      imac(j) = PFIDAT(inc,1)
  200     continue
c
c......Write out Macro header
c.........Macro name
c
	  call getvwd (jmac(1),ldat(1:24),nc,1,PSTWRD,PSTWVL,NPSTWD)
c
c.........ENABLED/DISABLED
c
	  if (imac(3) .eq. 0) then
	      call getvwd (4016,lbuf,nc,1,PSTWRD,PSTWVL,NPSTWD)
	  else
	      call getvwd (4015,lbuf,nc,1,PSTWRD,PSTWVL,NPSTWD)
	  endif
	  ldat   = ldat(1:24) // '   ' // lbuf(1:8)
	  nc     = 19
c
c.........Start of code
c
	  inum   = imac(4)
	  call itoc (inum,lnum,nc1,6)
	  ldat   = ldat(1:nc) // '   PC: ' // lnum(1:nc1)
	  nc     = nc     + 7 + nc1
c
c.........# of Arguments
c
	  inum   = imac(5)
	  call itoc (inum,lnum,nc1,2)
	  ldat   = ldat(1:nc) // '   Args: ' // lnum(1:nc1)
	  nc     = nc     + 9 + nc1
c
c.........Start of REAL args
c
	  inum   = imac(6)
	  call itoc (inum,lnum,nc1,5)
	  ldat   = ldat(1:nc) // '   Real Arg: ' // lnum(1:nc1)
	  nc     = nc     + 13  + nc1
c
c.........Start of CHAR args
c
	  inum   = imac(7)
	  call itoc (inum,lnum,nc1,5)
	  ldat   = ldat(1:nc) // '   Char Arg: ' // lnum(1:nc1)
	  nc     = nc     + 13 + nc1
c
	  call lstout (ldat,nc,cmsg,kerr)
	  if (kerr .ne. 0) go to 8000
  400 continue
      call lstout (' ',1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Process REAL variables
c
      inc    = 256
      irec   = jhed(2) - 1
      call getvwd (4008,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
      ldat   = ldat(1:nc) // ' '
      nc     = nc     + 1
      ipt    = MAXPRN + 1
      inum   = 1
      do 800 i=1,NSCAL(1),1
c
c......Read next REAL record
c
	  inc    = inc    + inum
	  if (inc .gt. 256) then
	      irec   = irec   + 1
	      call rdprm (LUNSC3,irec,PFDAT(1,2),cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	      inc    = inc    - 256
	  endif
c
c......Write REAL variables
c
	  inum   = PFIDAT(inc,2)
	  call itoc (ipt,lbuf,nc1,0)
	  call itoc (inum,lnum,nc2,0)
	  ipt    = ipt    + inum
c
	  if (nc+nc1+nc2+3 .gt. 80) then
	      nc     = nc     - 1
	      call lstout (ldat,nc,cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	      nc    = 5
	  endif
c
	  ldat   = ldat(1:nc) // lbuf(1:nc1) // '(' // lnum(1:nc2) //
     1             '),'
	  nc     = nc     + nc1    + nc2    + 3
  800 continue
      nc     = nc     - 1
      call lstout (ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call lstout (' ',1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Process CHAR variables
c
      inc    = 257
      irec   = jhed(4) - 1
      call getvwd (4007,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
      ldat   = ldat(1:nc) // ' '
      nc     = nc     + 1
      ipt    = MAXPRN + 1
      do 1200 i=1,NSCAL(2),1
c
c......Read next CHAR record
c
	  if (inc .ge. 256) then
	      irec   = irec   + 1
	      call rdprm (LUNSC3,irec,PFDAT(1,4),cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	      inc    = inc    - 256
	  endif
c
c......Write CHAR variables
c
	  inum   = PFIDAT(inc,4)
	  call itoc (ipt,lbuf,nc1,0)
	  call itoc (inum,lnum,nc2,0)
	  ipt    = ipt    + inum   + DESCHR
c
	  if (nc+nc1+nc2+3 .gt. 80) then
	      nc     = nc     - 1
	      call lstout (ldat,nc,cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	      nc    = 5
	  endif
c
	  ldat   = ldat(1:nc) // lbuf(1:nc1) // '(' // lnum(1:nc2) //
     1             '),'
	  nc     = nc     + nc1    + nc2    + 3
c
	  inc    = inc    + 2 + inum/2
 1200 continue
      nc     = nc     - 1
      call lstout (ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call lstout (' ',1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Process code records
c
      irec   = jhed(5) - 1
      inc    = 1000
      IPC    = 0
      do 2000 i=1,nmac,1
c
c......Get next compiled command
c
 1250     if (inc .gt. 256) then
	      irec   = irec   + 1
	      call rdprm (LUNSC3,irec,PFDAT(1,5),cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	      inc    = 1
	  endif
c
c.........Store command in separate array
c
	  do 1300 j=1,PFIDAT(inc,5),1
	      if (inc .gt. 256) then
		  irec   = irec   + 1
		  call rdprm (LUNSC3,irec,PFDAT(1,5),cmsg,kerr)
		  if (kerr .ne. 0) go to 8000
		  inc    = 1
	      endif
	      ICMPL(j) = PFIDAT(inc,5)
	      inc    = inc    + 1
 1300     continue
c
c.........Go to appropriate section
c
	  igo    = ICMPL(2) + 1
	  go to (1500,1510,1520,1530,1540,1550,1560,1570,1570,1590,1600,
     1           1610), igo
c
c............Continue
c
 1500         call getvwd (4005,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      go to 1700
c
c............Real equation
c
 1510         call wrtreq (ldat,nc)
	      go to 1700
c
c............Text equation
c
 1520         call wrtteq (ldat,nc)
	      go to 1700
c
c............Post word
c
 1530         call wrtpwd (ldat,nc)
	      go to 1700
c
c............JUMPTO
c
 1540         call getvwd (4014,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      call itoc (JCMPL(2),lnum,nc1,0)
	      ldat   = ldat(1:nc) // '/' // lnum(1:nc1)
	      nc     = nc     + 1 + nc1
	      go to 1700
c
c............IF
c
 1550         call wrtif (ldat,nc)
	      go to 1700
c
c...........TERMAC
c
 1560         call getvwd (4010,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      go to 1700
c
c............Mult-arg function
c
 1570         call wrtfnc (ldat,nc)
	      go to 1700
c
c............SYNTAX
c
 1590         call wrtsyn (ldat,nc)
	      go to 1700
c
c............ENABLE/DISABLE
c
 1600         if (ICMPL(3) .eq. 0) then
		  call getvwd (4016,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      else
		  call getvwd (4015,ldat,nc,0,PSTWRD,PSTWVL,NPSTWD)
	      endif
	      inum   = ICMPL(4)
	      call getvwd (inum,lnum,nc1,0,PSTWRD,PSTWVL,NPSTWD)
	      ldat   = ldat(1:nc) // '/ ' // lnum(1:nc1)
	      nc     = nc     + nc1    + 2
	      go to 1700
c
c............FORMAT specification
c
 1610         call wrtfmt (ldat,nc)
	      go to 1700
c
c.........Write out formatted compiler record
c
 1700     call itoc (IPC,lnum,nc1,6)
	  ldat   = lnum(1:nc1) // '   ' // ldat(1:nc)
	  nc     = nc     + nc1    + 3
	  call lstout (ldat,nc,cmsg,kerr)
	  if (kerr .ne. 0) go to 8000
	  IPC    = IPC    + ICMPL(1)
c
c.........TERMAC
c
	  if (ICMPL(2) .eq. 6) then
	      call lstout (' ',1,cmsg,kerr)
	      if (kerr .ne. 0) go to 8000
	  else
	      go to 1250
	  endif
 2000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtreq (cdat,knc)
c
c   FUNCTION:  This routine formats a compiled numeric equation for
c              printing.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Text of formatted numeric equation.
c
c           knc     I*4  D1  -  Number of characters in cdat.
c
c***********************************************************************
c
      subroutine wrtreq (cdat,knc)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc,inum,ifnc(9),ipt
c
      character*2 ldelim(16)
      character*20 lnum
c
      data ifnc /5004,5001,5002,5003,5007,5008,5009,5010,5011/
c
      data ldelim /'= ',', ',': ','+ ','- ','* ','/ ','**','==','< ',
     1             '> ','<>','<=','>=','&','|'/
c
c...Initialize routine
c
      knc    = 0
c
c...Store variable name
c
      inum   = ICMPL(7)
      call itoc (inum,lnum,nc,0)
      cdat   = '^' // lnum
      knc    = nc     + 1
c
c......Store subscript value
c
      if (ICMPL(4) .eq. 2) then
	  inum   = ICMPL(8)
	  call itoc (inum,lnum,nc,0)
	  cdat   = cdat(1:knc) // '(^' // lnum(1:nc) // ')'
	  knc    = knc    + nc     + 3
      endif
c
      cdat   = cdat(1:knc) // ' = '
      knc    = knc    + 3
c
c...Function
c
      if (ICMPL(3) .ge. 19 .and. ICMPL(3) .le. 27) then
	  inum   = ifnc(ICMPL(3)-18)
	  call getvwd (inum,lnum,nc,0,PSTWRD,PSTWVL,NPSTWD)
	  cdat   = cdat(1:knc) // lnum(1:nc) // '('
	  knc    = knc    + nc     + 1
      endif
c
c...Store 1st variable or number
c
      inum   = ICMPL(5)
      ipt    = 9
      call dbgtyp (inum,ipt,ipt,lnum,nc)
      cdat   = cdat(1:knc) // lnum(1:nc)
      knc    = knc     + nc
c
c...Store operator
c
      if (ICMPL(3) .ge. 3 .and. ICMPL(3) .le. 16) then
	  cdat   = cdat(1:knc) // ' ' // ldelim(ICMPL(3)) // ' '
	  knc    = knc    + 4
      else if (ICMPL(3) .ge. 17 .and. ICMPL(3) .le. 27) then
	  cdat   = cdat(1:knc) // ')'
	  knc    = knc    + 1
	  go to 8000
      else
	 go to 8000
      endif
c
c...Store 2nd variable or number
c
      inum   = ICMPL(6)
      ipt    = 13
      call dbgtyp (inum,ipt,ipt,lnum,nc)
      cdat   = cdat(1:knc) // lnum(1:nc)
      knc    = knc     + nc
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtteq (cdat,knc)
c
c   FUNCTION:  This routine formats a compiled numeric text for
c              printing.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Text of formatted text equation.
c
c           knc     I*4  D1  -  Number of characters in cdat.
c
c***********************************************************************
c
      subroutine wrtteq (cdat,knc)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc,inum,ist,ipt,ifnc(4)
c
      character*2 lc
      character*512 lnum
c
      data ifnc /5016,5017,5018,5019/
c
c...Initialize routine
c
      knc    = 0
c
c...Store variable name
c
      inum   = ICMPL(4)
      ipt    = 7
      call dbgtyp (inum,ipt,ist,cdat,knc)
      cdat   = cdat(1:knc) // ' = '
      knc    = knc    + 3
c
c...Function
c
      if (ICMPL(3) .ge. 32 .and. ICMPL(3) .le. 35) then
	  inum   = ifnc(ICMPL(3)-31)
	  call getvwd (inum,lnum,nc,0,PSTWRD,PSTWVL,NPSTWD)
	  cdat   = cdat(1:knc) // lnum(1:nc)
	  knc    = knc    + nc
	  if (ICMPL(3) .eq. 35) then
	      cdat   = cdat(1:knc)   // '('
	      knc    = knc    + 1
	  else
	      go to 8000
	  endif
      endif
c
c...Store 1st variable or text
c...Store Variable name
c
      ist    = 16
      inum   = ICMPL(5)
      ipt    = 10
      call dbgtyp (inum,ipt,ist,lnum,nc)
      cdat   = cdat(1:knc) // lnum(1:nc)
      knc    = knc    + nc
c
c...Store operator
c
      if (ICMPL(3) .ge. 3 .and. ICMPL(3) .le. 16) then
	  if (ICMPL(3) .eq. 4) then
	      lc     = '+ '
	  else if (ICMPL(3) .eq. 9) then
	      lc = '=='
	  else if (ICMPL(3) .eq. 15) then
	      lc = '&'
	  else if (ICMPL(3) .eq. 16) then
	      lc = '|'
	  else if (ICMPL(3) .eq. 12) then
	      lc = '<>'
	  endif
	  cdat   = cdat(1:knc) // ' ' // lc // ' '
	  knc    = knc    + 4
      else if (ICMPL(3) .ge. 32 .and. ICMPL(3) .le. 35) then
	  cdat   = cdat(1:knc) // ')'
	  knc    = knc    + 1
	  go to 8000
      else
	 go to 8000
      endif
c
c...Store 2nd variable or number
c...Store Variable name
c
      inum   = ICMPL(6)
      ipt    = 13
      call dbgtyp (inum,ipt,ist,lnum,nc)
      cdat   = cdat(1:knc) // lnum(1:nc)
      knc    = knc    + nc
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtpwd (cdat,knc)
c
c   FUNCTION:  This routine formats a compiled post-processor command
c              for printing.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Text of formatted post command.
c
c           knc     I*4  D1  -  Number of characters in cdat.
c
c***********************************************************************
c
      subroutine wrtpwd (cdat,knc)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc,inum,i,ipt,ist
c
      character*2 ldlm
      character*72 lnum
c
c...Initialize routine
c
      knc    = 0
      ldlm   = '/ '
c
c...Store major word
c
      inum   = ICMPL(3)
      call getvwd (inum,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
      if (ICMPL(4) .eq. 0) go to 8000
c
c...Store minor word(s)/value(s)
c
      ist    = 5
      ipt    = 4 + ((ICMPL(4)-1)/4 + 1) * 4 + 1
      do 1000 i=1,ICMPL(4),1
	  inum   = ICMPL(ist)
	  call dbgtyp (inum,ipt,ipt,lnum,nc)
	  cdat   = cdat(1:knc) // ldlm // lnum(1:nc)
	  knc    = knc    + nc     + 2
	  ist    = ist    + 1
	  ldlm   = ', '
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtfnc (cdat,knc)
c
c   FUNCTION:  This routine formats a compiled multi-argument function.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Text of formatted post command.
c
c           knc     I*4  D1  -  Number of characters in cdat.
c
c***********************************************************************
c
      subroutine wrtfnc (cdat,knc)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc,inum,i,ipt,ist,ifnc(4),is
c
      character*2 ldlm
      character*512 lnum
c
      data ifnc /5012,5013,5014,5015/
c
c...Initialize routine
c
      knc    = 0
      ldlm   = '( '
c
c...Store real variable name
c
      if (ICMPL(2) .eq. 7) then
	  inum   = ICMPL(6)
	  call itoc (inum,lnum,nc,0)
	  cdat   = '^' // lnum
	  knc    = nc     + 1
c
c......Store subscript value
c
	  if (ICMPL(5) .EQ. 2) then
	      inum   = ICMPL(7)
	      call itoc (inum,lnum,nc,0)
	      cdat   = cdat(1:knc) // '(^' // lnum(1:nc) // ')'
	      knc    = knc    + nc     + 3
	  endif
c
c...Store text variable name
c
      else
	  inum   = ICMPL(5)
	  ipt    = 6
	  call dbgtyp (inum,ipt,ipt,cdat,knc)
      endif
c
      cdat   = cdat(1:knc) // ' = '
      knc    = knc    + 3
c
c...Store Function name
c
      inum   = ifnc(ICMPL(3)-27)
      call getvwd (inum,lnum,nc,0,PSTWRD,PSTWVL,NPSTWD)
      cdat   = cdat(1:knc) // lnum(1:nc)
      knc    = knc    + nc
c
c...Store arguments
c
      is     = 9
      ipt    = 8 + ((ICMPL(4)-1)/4 + 1) * 4 + 1
      do 1000 i=1,ICMPL(4),1
	  inum   = ICMPL(is)
	  ist    = ipt
	  call dbgtyp (inum,ipt,ist,lnum,nc)
	  cdat   = cdat(1:knc) // ldlm // lnum(1:nc)
	  knc    = knc    + nc     + 2
	  is     = is     + 1
	  ldlm   = ', '
 1000 continue
      cdat   = cdat(1:knc) // ')'
      knc    = knc    + 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtsyn (cdat,knc)
c
c   FUNCTION:  This routine formats a compiled SYNTAX command for print-
c              ing.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Text of formatted post command.
c
c           knc     I*4  D1  -  Number of characters in cdat.
c
c***********************************************************************
c
      subroutine wrtsyn (cdat,knc)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc,inum,i,ipt,ist,j,nprm
c
      character*2 ldlm
      character*20 lnum
c
c...Initialize routine
c
      knc    = 0
      ldlm   = '/ '
c
c...Store major word
c
      inum   = 4011
      call getvwd (inum,cdat,knc,0,PSTWRD,PSTWVL,NPSTWD)
c
c...Store parameter bundles
c
      ist    = 5
      do 1000 i=1,ICMPL(3),1
	  cdat   = cdat(1:knc) // ldlm
	  knc    = knc    + 2
	  ldlm   = ', '
c
c......Get next parameter
c
	  ipt    = ((ist+ICMPL(ist)-1)/4 + 1) * 4 + 1
	  nprm   = ICMPL(ist)
	  if (nprm .gt. 1) then
	      cdat   = cdat(1:knc) // '('
	      knc    = knc    + 1
	  endif
	  do 800 j=1,nprm,1
	      if (j .ne. 1) then
		  cdat   = cdat(1:knc) // ', '
		  knc    = knc    + 2
	      endif
	      ist    = ist    + 1
	      inum   = ICMPL(ist)
	      call dbgtyp (inum,ipt,ipt,lnum,nc)
	      cdat   = cdat(1:knc) // lnum(1:nc)
	      knc    = knc    + nc
  800     continue
	  if (nprm .gt. 1) then
	      cdat   = cdat(1:knc) // ')'
	      knc    = knc    + 1
	  endif
	  ist    = ipt
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtif (cdat,knc)
c
c   FUNCTION:  This routine formats an IF statment for printing.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Text of formatted post command.
c
c           knc     I*4  D1  -  Number of characters in cdat.
c
c***********************************************************************
c
      subroutine wrtif (cdat,knc)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc,inum
c
      character*1 ldlm
      character*24 lnum
c
c...Initialize routine
c
      knc    = 0
c
c...Store IF word
c
      inum   = 4001
      call getvwd (inum,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
c
c...Store parenthesis variable
c
      ldlm   = '^'
      if (ICMPL(3) .eq. 2) ldlm = '@'
      inum   = ICMPL(4)
      call itoc (inum,lnum,nc,0)
      cdat   = cdat(1:knc) // ' (' // ldlm // lnum(1:nc) // ')'
      knc    = knc    + nc     + 4
c
c...Store label
c
      if (IPASS .eq. 1) then
	  cdat   = cdat(1:knc) // LCMPL(9:32)
	  knc    = knc    + 24
      else
	  call itoc (JCMPL(3),lnum,nc,0)
	  cdat   = cdat(1:knc) // lnum(1:nc)
	  knc    = knc    + nc
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtfmt (cdat,knc)
c
c   FUNCTION:  This routine formats a FORMAT specification for printing.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Text of formatted post command.
c
c           knc     I*4  D1  -  Number of characters in cdat.
c
c***********************************************************************
c
      subroutine wrtfmt (cdat,knc)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc,inum
c
      character*1 ltyp(4)
      character*24 lnum
c
      data ltyp /'L','T','F','D'/
c
c...Initialize routine
c
      knc    = 0
c
c...Store Format number
c
      inum   = ICMPL(3)
      call itoc (inum,lnum,nc,0)
      cdat   = '#' // lnum(1:nc) // ' = '
      knc    = nc     + 4
c
c...#F = #F
c
      if (ICMPL(1) .eq. 4) then
	   inum   = ICMPL(4)
	   call itoc (inum,lnum,nc,0)
	   cdat   = cdat(1:knc) // '#' // lnum(1:nc)
	   knc    = knc    + nc     + 1
c
c...#F = Fm.n, Mm.n, S+-
c
      else
c
c......Fm.n
c
	  inum   = ICMPL(5)
	  call itoc (inum,lnum,nc,0)
	  cdat   = cdat(1:knc) // ltyp(ICMPL(4)) // lnum(1:nc) // '.'
	  knc    = knc    + nc     + 2
	  inum   = ICMPL(6)
	  call itoc (inum,lnum,nc,0)
	  cdat   = cdat(1:knc) // lnum(1:nc) // ', '
	  knc    = knc    + nc     + 2
c
c......Mm.n
c
	  inum   = ICMPL(7)
	  call itoc (inum,lnum,nc,0)
	  cdat   = cdat(1:knc) // 'M' // lnum(1:nc) // '.'
	  knc    = knc    + nc     + 2
	  inum   = ICMPL(8)
	  call itoc (inum,lnum,nc,0)
	  cdat   = cdat(1:knc) // lnum(1:nc) // ', '
	  knc    = knc    + nc     + 2
c
c......S+-
c
	  cdat   = cdat(1:knc) // 'S'
	  knc    = knc    + 1
	  if (ICMPL(9) .eq. 1) then
	      cdat   = cdat(1:knc) // '+'
	      knc    = knc    + 1
	  endif
	  if (ICMPL(10) .eq. 1) then
	      cdat   = cdat(1:knc) // '-'
	      knc    = knc    + 1
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
c   SUBROUTINE:  dbgtyp (ktyp,kpt,kst,cdat,knc)
c
c   FUNCTION:  This routine determines the type of variable, using the
c              ICMPL global array, in a 1st pass compiled statement and
c              returns a text string representation of the variable.
c              Recognized variable types are as follows.
c
c                 0  = Vocabulary Word (Value(I*4))
c
c                 1  = Real Variable (Var(I*4))
c
c                 2  = Subscripted Real Variable (Var(I*4) + Sub(I*4))
c
c                 3  = Real Number (Number(R*8))
c
c                 4  = Text Variable (Var(I*2) + Start(I*2) + End(I*2))
c
c                 5  = Subscripted Text Variable (Var(I*2) + Sub(I*2))
c
c                 6  = Text String (#Chars(I*2) + String(C*#Chars))
c
c                 7  = Format Number (Number(I*2))
c
c                12  = Macro Argument (Word(I*4) + Sub(I*4))
c
c                13  = Post Variable (Word(I*4))
c
c                14  = Subscripted Post Variable (Word(I*4) + Sub(I*4))
c
c                14  = Clfile Record Data (Word(I*4) + Sub(I*4))
c
c   INPUT:  ktyp    I*4  D1  Type of current variable.
c
c           kpt     I*4  D1  Pointer to the start of the variable de-
c                            scriptor (I*2) within global ICMPL array.
c                            'kpt' will be modified to point to the next
c                            I*2 (on an even 8-byte boundary) following
c                            the current variable descriptor.
c
c           kst     I*4  D1  Pointer to the start of the text data (I*2)
c                            within the global ICMPL array, when the
c                            variable is a text string.  'kst' will be
c                            modified to point to the next I*2 (on an
c                            even 2-byte boundary) following the current
c                            text string.  'kst' is not used when the
c                            variable is not a text string.
c
c   OUTPUT: cdat    C*n  D1  Text representation of variable.
c
c           knc    I*4  D1  Number of characters in 'cdat'.
c
c
c***********************************************************************
c
      subroutine dbgtyp (ktyp,kpt,kst,cdat,knc)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kpt,knc,ktyp,kst
c
      character*(*) cdat
c
      integer*4 jpt,irt,nc,inum,is,ie
c
      character*20 lnum
c
c...Initialize routine
c
      jpt    = kpt    / 2 + 1
      irt    = jpt    / 2 + 1
c
c...Post word
c
      if (ktyp .eq. 0) then
	  inum   = JCMPL(jpt)
	  call getvwd (inum,cdat,knc,2,PSTWRD,PSTWVL,NPSTWD)
	  kpt    = kpt    + 4
c
c...Real variable
c
      else if (ktyp .eq. 1 .or. ktyp .eq. 2) then
	  call itoc (JCMPL(jpt),lnum,nc,0)
	  cdat   = '^' // lnum(1:nc)
	  knc    = nc     + 1
	  if (ktyp .eq. 2) then
	      call itoc (JCMPL(jpt+1),lnum,nc,0)
	      cdat   = cdat(1:knc) // '(^' // lnum(1:nc) // ')'
	      knc    = knc    + nc     + 3
	  endif
	  kpt    = kpt    + 4
c
c...Real number
c
      else if (ktyp .eq. 3) then
	  call rtoc (RCMPL(irt),cdat,knc)
	  kpt    = kpt    + 4
c
c...Text variable
c
      else if (ktyp .eq. 4 .or. ktyp .eq. 5) then
	  inum   = ICMPL(kpt)
	  call itoc (inum,lnum,nc,0)
	  cdat   = '@' // lnum(1:nc)
	  knc    = nc     + 1
c
c......Store start & end values
c
	  if (ktyp .eq. 4) then
	      if (ICMPL(kpt) .le. MAXPRN) then
		  ICMPL(kpt+1) = 0
		  ICMPL(kpt+2) = 0
	      endif
	      inum   = ICMPL(kpt+1)
	      call itoc (inum,lnum,nc,0)
	      cdat   = cdat(1:knc) // '(' // lnum(1:nc)
	      knc    = knc    + nc     + 1
	      inum   = ICMPL(kpt+2)
	      call itoc (inum,lnum,nc,0)
	      cdat   = cdat(1:knc) // ':' // lnum(1:nc) // ')'
	      knc    = knc    + nc     + 2
c
c...Store subscript value
c
	  else if (ktyp .eq. 5) then
	      inum   = ICMPL(kpt+1)
	      call itoc (inum,lnum,nc,0)
	      cdat   = cdat(1:knc) // '(^' // lnum(1:nc) // ')'
	      knc    = knc    + nc     + 3
	  endif
	  kpt    = kpt    + 4
c
c...Text string
c
      else if (ktyp .eq. 6) then
	  nc     = ICMPL(kst)
	  is     = kst    * 2      + 1
	  ie     = is     + nc     - 1
	  cdat   = '"' // LCMPL(is:ie) // '"'
	  knc    = nc     + 2
c
	  is     = (nc-1) / 2 + 2
	  is     = (is/4 + 1) * 4
	  kpt    = kpt    + is
	  kst    = kst    + (nc-1) / 2 + 1
c
c...Format number
c
      else if (ktyp .eq. 7) then
	  inum   = ICMPL(kpt)
	  call itoc (inum,lnum,nc,0)
	  cdat   = '#' // lnum(1:nc)
	  knc    = nc     + 1
	  kpt    = kpt    + 4
c
c...Range
c
      else if (ktyp .eq. 11) then
	  call rtoc (RCMPL(irt),cdat,knc)
	  call rtoc (RCMPL(irt+1),lnum,nc)
	  cdat   = cdat(1:knc) // '-' // lnum(1:nc)
	  knc    = knc    + nc     + 1
	  kpt    = kpt    + 8
c
c...Post Argument/Variable
c
      else if (ktyp .eq. 12 .or. ktyp .eq. 13 .or. ktyp .eq. 14 .or.
     1         ktyp .eq. 15 .or. ktyp .eq. 18) then
          inum   = JCMPL(jpt) + 6000.
          if (ICMPL(2) .eq. 2) inum   = ICMPL(kpt) + 6000.
          call getvwd (inum,cdat,knc,0,PSTWRD,PSTWVL,NPSTWD)
          if (ktyp .eq. 12 .or. ktyp .eq. 14 .or. ktyp .eq. 15 .or.
     1        ktyp .eq. 18) then
	      inum   = JCMPL(jpt+1)
	      if (ICMPL(2) .eq. 2) inum = ICMPL(kpt+1)
	      if (inum .lt. 0.) then
		  inum   = - inum
		  call itoc (inum,lnum,nc,0)
		  cdat   = cdat(1:knc) // '(^' // lnum(1:nc) // ')'
		  knc    = knc    + nc     + 3
	      else
		  call itoc (inum,lnum,nc,0)
		  cdat   = cdat(1:knc) // '(' // lnum(1:nc) // ')'
		  knc    = knc    + nc     + 2
	      endif
	  endif
	  kpt    = kpt    + 4
c
c...Unknown type
c
      else
	  cdat   = ' ? '
	  knc    = 3
      endif
c
c...End of routine
c
 8000 return
      end
