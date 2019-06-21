c
c***********************************************************************
c
c   FILE NAME:  macro
c   CONTAINS:
c               lodmac  inimac  savmac  incfil
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        macro.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:13
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  lodmac (cmsg,kerr)
c
c   FUNCTION:  This routine is the controlling routine for loading &
c              compiling macros.  The input file should be opened with
c              LUNSC1, the listing file with LUNSC2, the object file
c              with LUNSC3 and the document file with LUNSC4.  This rou-
c              tine will open the output file with LUNMAC.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns -1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodmac (cmsg,kerr)
c
      include 'compile.inc'
      include 'menu.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,iparen,inc,iend,iend1,icmn
c
      character*512 ldat,sdat
      character*80 cword
c
      character*1 ctab
      byte tab
c
      equivalence (ctab,tab)
c
      data tab / 9/
c
c...Initialize routine
c
      ISTMNC = 0
      kerr   = 0
c
c...Initialize Macro data base
c
      call inimac (cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Initialize G$MAIN Macro
c
      ldat   = 'G$MAIN/MACRO,0'
      sdat   = ldat
      nc     = 14
      call stpars (ldat,nc,0,cmsg,kerr,icmn)
      if (kerr .ne. 0) go to 9000
      call compil (cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Get a statement from the input file
c
  100 call getstm (ldat,nc,cmsg,kerr)
      sdat   = ldat
      ITXTPT = 1
c
c...Check for include file on stack before exiting
c
      if (kerr .eq. 2) then
          if (INCFLN .eq. 0) go to 8100
          call clsfil (LUNSC1)
          LUNSC1 = INCLUN(INCFLN)
          INCFLN = INCFLN - 1
          go to 100
      end if
      if (kerr .ne. 0) go to 9000
      iparen = 0
c
c...Check for ELSE in IF-THEN-ELSE structure
c...If so, then compile previous "TRUE"'s JUMPTO
c
      if (IFPT .ne. 0) then
          do 200 inc=1,nc,1
              if (ldat(inc:inc) .ne. ' ' .and.
     1            ldat(inc:inc) .ne. ctab) go to 220
  200     continue
          inc    = 0
  220     if (inc .ne. 0) then
              iend   = index(ldat(inc:nc),' ')
              iend1  = index(ldat(inc:nc),ctab)
              if ((iend1 .ne. 0 .and. iend1 .lt. iend) .or.
     1            iend .eq. 0) iend = iend1
              if (iend .eq. 0) then
                  iend   = nc
              else
                  iend   = inc    + iend   - 2
              endif
              call touppr (ldat(inc:iend),cword)
              if (cword(1:iend-inc+1) .eq. LELSE) then
                  call elsjmp (cmsg,kerr)
                  if (kerr .ne. 0) go to 9000
              endif
          endif
      endif
c
c...Store internal label pointer
c
      LABPC  = IPC
c
c...Label
c
      if (ILABEL .eq. 1) then
          call cmplab (ldat,nc,IPC,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          go to 100
      endif
c
c...Text only command
c......Check if include command
c......or other text command
c
      if (ITXCMD .ne. 0) then
          if (ITXCMD .eq. 4041) then
             call incfil (ldat,nc,cmsg,kerr)
          else
             call cmptpw (ldat,nc,cmsg,kerr)
          end if
          if (kerr .ne. 0) go to 9000
          go to 100
      endif
c
c...Get a single parenthesis level
c
  400 call getprn (ldat,nc,iparen,cmsg,kerr)
      if (kerr .eq. 1) go to 9000
c
c......Write listing file record
c
c      call wrtstm (ldat,nc,1,cmsg,kerr)
c      if (kerr .ne. 0) go to 8000
c
c...Break out tokens
c
      call stpars (ldat,nc,0,cmsg,kerr,icmn)
      if (kerr .eq. 1) go to 9000
      if (RCSUB(1) .eq. 4041) then
          call incfil (ldat,nc,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          go to 100
      end if
c
c......Write listing file record
c
c      call wrtstm (ldat,nc,2,cmsg,kerr)
c      if (kerr .ne. 0) go to 8000
c
c...Compile statement
c
      call compil (cmsg,kerr)
      if (kerr .eq. 1) go to 9000
c
c...Get next parenthesis level
c
      if (iparen .eq. 1) go to 400
c
c...Check for end of DO loop
c
      if (IDOLPT .ne. 0) then
          call doend (cmsg,kerr)
          if (kerr .ne. 0) go to 9000
      endif
c
c...Get next statement
c
      go to 100
c
c...End of routine
c
 8000 return
c
c...Save macro index records
c
 8100 call savmac (cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Write compiled code
c
c      call wrtstm (ldat,nc,3,cmsg,kerr)
c      if (kerr .ne. 0) go to 8000
c
c...Compile 2nd pass
c
      call cmpps2 (cmsg,kerr)
      if (kerr .gt. 0) then
          kerr   = 0
          call lsterr ('Compilation Pass 2.',cmsg,cmsg,kerr)
      endif
c      call wrtstm (ldat,nc,4,cmsg,kerr)
      go to 8000
c
c...Error parsing statement
c
 9000 if (kerr .eq. -1) go to 8000
      ISTMNC = 0
      kerr   = 0
      call lsterr (sdat,cmsg,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 100
      end
c
c***********************************************************************
c
c   SUBROUTINE:  inimac (cmsg,kerr)
c
c   FUNCTION:  This routine initializes the Macro compiling routines.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine inimac (cmsg,kerr)
c
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 idat(128),i,nc
c
c...Initialize routine
c
      kerr   = 0
c
c...Open Macro scratch file
c
c      att(1) = 'direct'
c      att(2) = 'none'
c      att(3) = 'unformatted'
c      att(4) = 'scratch'
c      irecl  = 512
c      fnam   = 'pre_comp.wrk'
c      call opnfil (LUNMAC,fnam,att,irecl,cmsg,kerr)
c      if (kerr .ne. 0) go to 8000
c
c...Initialize Macro variables
c
      DESCHR = 4
      DOCLIN = IOPFL(3) + 1
c
      ICURLN = 0
      IMACDF = 0
      IPASS  = 1
      IPC    = 0
c
      LISLIN = IOPFL(3) + 1
      LISPAG = 0
c
      MAXIF  = 10
      MAXMEM = 20
      MAXPRN = 50
      MAXTOK = 100
      MEMREC = 0
      MEMSIZ = 100
c
      NMACRO = 0
      NLABEL = 0
      NSCAL(1) = 0
      NSCAL(2) = 0
c
      ISCAST(1) = 51
      ISCAST(2) = 51
c
c...Initialize file pointers
c
      MFPT(1,1) = 1
      MFPT(2,1) = 1
      MFPT(3,1) = 1
c
      MFPT(1,2) = 2
      MFPT(2,2) = 2
      MFPT(3,2) = 1
c
      MFPT(1,3) = 3
      MFPT(2,3) = 3
      MFPT(3,3) = 1
c
      MFPT(1,4) = 4
      MFPT(2,4) = 4
      MFPT(3,4) = 1
c
      MFPT(1,5) = 5
      MFPT(2,5) = 5
c                    MFPT(3,5) points to I*2's
      MFPT(3,5) = 4
c
      ICREC(1) = 1
      ICREC(2) = 1
      ICREC(3) = 1
      ICREC(4) = 1
      ICREC(5) = 1
c
      MFNXT  = 6
c
c...Initialize # of items per record
c
      MFREC(1) = 9
      MFREC(2) = 18
      MFREC(3) = 10
      MFREC(4) = 10
      MFREC(5) = 1
c
c...Initialize length of fixed data &
c...logical record data per record
c
      MNFIX(1) = 2
      MNREC(1) = 14
c
      MNFIX(2) = 2
      MNREC(2) = 7
c
      MNFIX(3) = 2
      MNREC(3) = 12
c
      MNFIX(4) = 2
      MNREC(4) = 12
c
      MNFIX(5) = 2
      MNREC(5) = 0
c
c...Initialize  record buffers
c
      MFDAT(1,1) = MFPT(1,1)
      MFDAT(1,2) = MFPT(1,2)
      MFDAT(1,3) = MFPT(1,3)
      MFDAT(1,4) = MFPT(1,4)
      MFDAT(1,5) = MFPT(1,5)
c
      MFDAT(2,1) = 0
      MFDAT(2,2) = 0
      MFDAT(2,3) = 0
      MFDAT(2,4) = 0
      MFDAT(2,5) = 0
c
c...Initialize scratch file
c
      do 100 i=1,128,1
          idat(i) = 0
  100 continue
c
      do 200 i=1,5,1
          call stowrk (i,idat,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  200 continue
c
c...Initialize text records
c...LETTER  PPRINT  PARTNO  INSERT  PRINT  REMARK  ERROR
c...INCLUD, APPEND
c
      call getvwd (1043,LCOMTX(1),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1044,LCOMTX(2),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1045,LCOMTX(3),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1046,LCOMTX(4),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1102,LCOMTX(5),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1103,LCOMTX(6),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (4009,LCOMTX(7),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (4041,LCOMTX(8),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1199,LCOMTX(9),nc,1,PSTWRD,PSTWVL,NPSTWD)
c
c...Initialize 'ELSE' string
c
      call getvwd (4004,LELSE,nc,1,PSTWRD,PSTWVL,NPSTWD)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  savmac (cmsg,kerr)
c
c   FUNCTION:  This routine saves the compiler index records.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine savmac (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i
c
c...Initialze routine
c
      kerr   = 0
c
c...Save compiler index records
c
      do 200 i=1,5,1
          call stowrk (MFDAT(1,i),MFDAT(1,i),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  200 continue
c
c...End of routine
c
 8000 return
      end
c***********************************************************************
c
c   SUBROUTINE:  incfil (cbuf,knc,cmsg,kerr)
c
c   FUNCTION:  This routine process include files.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to parse.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine incfil (cbuf,knc,cmsg,kerr)
c
      include 'compile.inc'
      include 'menu.inc'
c
      integer*4 knc,kerr
c
      character*(*) cbuf,cmsg
c
      integer*4 lu,i,n,irecl
      character*20 att(4)
      character*(MAX_PATH) ldev,fnam,cname
      character*(MAX_FILE) lfil,lext
c
      kerr   = 0
c
      i      = 8
      if (IMINST .ne. 0) i = IMINST + 1
      if (knc .lt. i) go to 9000
      cname  = cbuf(i:knc)
c
c...Check stack for overflow
c
      if (INCFLN .gt. 9) go to 9100
      lu     = 11 + INCFLN
c
c...Open include file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      call fbreak (cname,ldev,lfil,lext)
      if (lext .eq. ' ') lext = '.inc'
      call fparse (lfil,fnam,ldev,lext)
      call opnfil (lu,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Push on stack current file
c...and switch logic unit to new file
c
      INCFLN = INCFLN + 1
      INCLUN(INCFLN) = LUNSC1
      LUNSC1 = lu
c
c...End of routine
c
 8000 return
c
c...Errors
c
 9000 call errtxt ('INVPSYN',cmsg)
      kerr   = 1
      go to 8000
c
 9100 call errtxt ('INCOVL',cmsg)
      kerr   = 1
      go to 8000
      end
