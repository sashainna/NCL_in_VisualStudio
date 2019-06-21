c
c***********************************************************************
c
c   FILE NAME:  docmen.for
c   CONTAINS:
c               docmen  docgen  pdfhed  dcenhd  docprm  docsap  docrgf
c               docout  docclr
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        docmen.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:24:04
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  docmen (cfil,kerr)
c
c   FUNCTION:  Creates the MakePost automatic documentation file.
c
c   INPUT:  cfil    C*n  D1  -  Name of documentation file to create.
c
c   OUTPUT: kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docmen (cfil,cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 kerr
c
      character*(*) cfil,cmsg
c
      integer*4 irecl,nc, inum,strlen1
c
      character*20 att(4)
      character*80 lbuf
      character*(MAX_PATH) fnam,ldev
      character*(MAX_FILE) lfil,lext
c
c...Initialize routine
c
      MAXDOC = 700
      NDOCBF = 0
c
c...Open documentation prompt file
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 512
      fnam   = 'postdoc.MSG'
      call fparse (fnam,fnam,DVDSUP,'.MSG')
      call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Read prompt main record
c
      call rdprm (LUNSC1,1,IDOCDS,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Load Standalone prompts
c
      call lodspr (LUNSC1,IDOCDS,MAXDOC,DCLAB,DCPRM,DCNC,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Load Header text from .PDF file
c
      call pdfhed
c
c...Construct the document file's name
c
      call fbreak (cfil,ldev,lfil,lext)
      if (lfil .eq. ' ') then
          nc     = strlen1(LMNAME)
          lfil   = 'pworks_' // LMNAME(1:nc)
      endif
      if (lext .eq. ' ') lext = '.doc'
      call fparse (lfil,fnam,ldev,lext)
c
c...Open the document file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 132
      call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Generate the automatic documentation
c
      call docgen (cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...End of routine
c
 8000 call clsfil (LUNSC1)
      call clsfil (LUNSC2)
      return
c
c...Error processing file
c
 9000 if (MOTIF .ne. 1) then
          call errtxt ('NODOCUM',lbuf)
          call shfile (fnam,lfil,50)
          call errstr (lbuf,lfil,1)
          call errmsg (lbuf,2,1)
          call errmsg (cmsg,2,2)
          call getchr (inum)
      endif
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docgen (cmsg,kerr)
c
c   FUNCTION:  Controlling routine which calls each of the automatic
c              documentation routines.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docgen (cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Get page prompt
c
      call docsap ('PAGE',LDPAG,NCPAG)
c
c...Generate Machine Configuration doc
c
      call docmcf (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Generate Spindle & Feed rate tables
c
      call doctab (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Generate Register Description doc
c
      call docreg (1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Generate G-code Description doc
c
      call docreg (2,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Generate M-code Description doc
c
      call docreg (3,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Generate User defined block doc
c
      call docusr (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Generate Default Command Summary
c
      call doccmd (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pdfhed
c
c   FUNCTION:  Obtains the print file header record from the supported
c              .PDF file which will be used as the automatic documenta
c              tion page header.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pdfhed
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (LMNAME,CPOSMP(0141)), (MDESC ,CPOSMP(3853))
c
      character*40 LMNAME
      character*80 MDESC
c
      integer*4 nc,nc1,strlen1
c
      character*80 lbuf
c
c...Store blank lines and separator line
c
      DHED(1) = ' '
      DHED(2) = ' '
      DHED(4) = ' '
      DHED(6) = '-------------------------------------------------------
     1-------------------------'
      NCHED(1) = 0
      NCHED(2) = 0
      NCHED(4) = 0
      NCHED(6) = 80
c
c...Store MACHIN header line
c
      call getvwd (1015,lbuf,nc,0,PSTWRD,PSTWVL,NPSTWD)
      nc1    = strlen1(LMNAME)
      lbuf(nc+1:80) = '/PWORKS,' // LMNAME(1:nc1)
      nc     = nc     + 8      + nc1
      call dcenhd (lbuf,nc,DHED(3),NCHED(3))
c
c...Store MACHIN description line
c
      nc     = strlen1(MDESC)
      call dcenhd (MDESC,nc,DHED(5),NCHED(5))
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  dcenhd (cinp,knci,cout,knco)
c
c   FUNCTION:  Centers a text string within an 80 character buffer.
c
c   INPUT:  cinp    C*n  D1  -  Text string to center.
c
c           knci    I*4  D1  -  Number of chars in 'cinp'.
c
c   OUTPUT: cout    C*80 D1  -  80 character buffer to receive centered
c                               text string (cinp).
c
c           knco    I*4  D1  -  Number of chars in 'cout'.
c
c***********************************************************************
c
      subroutine dcenhd (cinp,knci,cout,knco)
c
      integer*4 knci,knco
c
      character*(*) cinp,cout
c
      integer*4 ist
c
      character*80 ltxt
c
c...No characters in text
c
      if (knci .le. 0 .or. knci .ge. 80) then
          cout   = cinp
          knco   = knci
c
c...Center input text in output string
c
      else
          ltxt   = cinp
          ist    = 40 - ((knci-1) / 2)
          cout   = ' '
          cout(ist:) = ltxt
          knco   = ist    + knci   - 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docprm (cbuf,knc,klev,knlev)
c
c   FUNCTION:  This routine returns the documentation menu text stored
c              at the requested level (example: 10.4.1).
c
c   INPUT:  klev    I*4  D10 -  An array containing the current level
c                               values.
c
c           knlev   I*4  D1  -  Number of levels in 'klevl'.
c
c   OUTPUT: cbuf    C*n  D1  -  Text of menu text.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine docprm (cbuf,knc,klev,knlev)
c
      integer*4 knc,klev(10),knlev
c
      character*(*) cbuf
c
      character*20 lpr
c
c...Convert level values to text string
c...in the format '1.2.3'
c
      call levasc (klev,knlev,lpr)
c
c...Load the standalone prompt
c
      call docsap (lpr,cbuf,knc)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docsap (cpr,cbuf,knc)
c
c   FUNCTION:  This routine returns the documentation standalone prompt
c              pointed to by the label 'cpr'.
c
c   INPUT:  cpr     C*n  D1  -  The label to search for.
c
c   OUTPUT: cbuf    C*n  D1  -  Text of standalone prompt.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine docsap (cpr,cbuf,knc)
c
      include 'menu.inc'
      include 'docum.inc'
c
      integer*4 knc
c
      character*(*) cpr
      character*80 cbuf
c
      integer*4 ipt
c
c...Get Documentation prompt
c
      call getsap (cpr,ipt,IDOCDS,DCLAB)
      cbuf   = DCPRM(ipt)
      knc    = DCNC(ipt)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docrgf (kreg,gval,cbuf,knc,kfl)
c
c   FUNCTION:  This routine formats a register and optionally its value
c              for output by the automatic documentation routines.
c
c   INPUT:  kreg    I*4  D1  -  Register to use for formatting the
c                               output.
c
c           gval    R*8  D1  -  Value to format with register when
c                               kfl = 3.
c
c           kfl     I*4  D1  -  Type of output to format register for.
c
c                                  1 = X          (Beg & End id only)
c                                  2 = X-xxx.oooo (1 + Numeric format)
c                                  3 = X1.0       (1 + Value)
c
c   OUTPUT: cbuf    C*n  D1  -  Formatted register text.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine docrgf (kreg,gval,cbuf,knc,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087)), (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT),IUNIT
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*4 kreg,knc,kfl
c
      real*8 gval
c
      character*(*) cbuf
c
      integer*4 inc,nc,nc1
c
      character*10 lft,lrgt
      character*20 lbuf
c
      data lft /'xxxxxxxxxx'/, lrgt /'oooooooooo'/
c
c...No register
c
      if (kreg .eq. 0) then
          cbuf   = ' '
          knc    = 0
          go to 8000
      endif
c
c...Get register's starting string
c
      knc    = REGBNC(kreg)
      if (REGBNC(kreg) .gt. 0) then
          cbuf   = REGST(kreg)
      else
          cbuf   = ' '
      endif
c
c...Format for Register Description
c...Get register's floating point format
c......Sign
c
      if (kfl .eq. 2) then
          if (FMTDES(2,kreg) .eq. 3) then
              cbuf(knc+1:knc+2) = 'P('
              knc    = knc    + 2
          else
              if (FMTDES(2,kreg) .eq. 1) then
                  knc    = knc    + 1
                  cbuf(knc:knc) = '+'
              endif
c
              if (FMTDES(3,kreg) .ne. 0) then
                  knc    = knc    + 1
                  cbuf(knc:knc) = '-'
              endif
          endif
c
c......Digits to the left
c
          inc    = IUNIT  - 1
          nc     = FMTDES(4+inc,kreg)
          if (nc .ne. 0) then
              cbuf(knc+1:knc+nc) = lft(1:nc)
              knc    = knc    + nc
          endif
c
c......Decimal point
c
          nc1    = FMTDES(6+inc,kreg)
          if ((nc1 .gt. 0 .or. nc .gt. 0) .and. (FMTDES(1,kreg) .eq. 3
     1        .or. (FMTDES(1,kreg) .eq. 4 .and. nc1 .gt. 0))) then
              knc    = knc    + 1
              cbuf(knc:knc) = '.'
          endif
c
c......Digits to the right
c
          inc    = IUNIT  - 1
          nc     = FMTDES(6+inc,kreg)
          if (nc .ne. 0) then
              cbuf(knc+1:knc+nc) = lrgt(1:nc)
              knc    = knc    + nc
          endif
c
c...Format register value
c
      else if (kfl .eq. 3) then
          call ftoc (gval,lbuf,nc,FMTDES(1,kreg))
          if (nc .ne. 0) then
              cbuf(knc+1:) = lbuf(1:nc)
              knc    = knc    + nc
          endif
      endif
c
c...Get register's ending string
c
      if (REGENC(kreg) .gt. 0) then
          cbuf(knc+1:) = REGEN(kreg)
          knc    = knc    + REGENC(kreg)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docout (cbuf,knc,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine outputs a character string to the auto-
c              matic documentation file.  It is intelligent enough to
c              buffer output in logical groups so that related output
c              records will not be broken up on page breaks.
c
c              A maximum of 20 records can be buffered.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to output.
c
c           knc     I*4  D1  -  Number of chars in 'cbuf'.
c
c           kfl     I*4  D1  -  1 = Starts a new logical output record.
c                               The current buffer will be flushed and
c                               'cbuf' will become the first line in the
c                               new buffer.  A blank line is always
c                               output prior to a new logical record.
c                               0 = 'cbuf' is part of the current logical
c                               record and will be buffered for now.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docout (cbuf,knc,kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
c
      integer*4 knc,kerr,kfl
c
      character*(*) cbuf,cmsg
c
c...Buffer text line
c
      if (kfl .eq. 0) then
          NDOCBF = NDOCBF + 1
          LDOCBF(NDOCBF) = cbuf
          IDOCNC(NDOCBF) = knc
          if (NDOCBF .eq. 20) call docclr (cmsg,kerr)
c
c...Write out buffered lines
c
      else if (kfl .eq. 1) then
          call docclr (cmsg,kerr)
          NDOCBF = 1
          LDOCBF(NDOCBF) = cbuf
          IDOCNC(NDOCBF) = knc
c
c...Clear buffer
c
      else
          call docclr (cmsg,kerr)
          NDOCBF = 0
      endif

c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docclr (cmsg,kerr)
c
c   FUNCTION:  Clears the automatic documentation output buffer main-
c              tained by 'docout'.  This routine should be called at
c              the end of each separate documentation section (MACHINE
c              CONFIGURATION, REGISTER FORMAT, etc.).
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docclr (cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i,nc,ifl
c
      character*1 ff
      character*20 lnum
c
      ff     = char(12)
c
      if (NDOCBF .eq. 0) go to 8000
c
c...Output header
c
      ifl    = 0
      if (DLIN+NDOCBF .gt. IOPFL(4)) then
          DPAG   = DPAG   + 1
          if (DPAG .gt. 1) then
              call wrtxt (LUNSC2,ff,1,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          call itoc (DPAG,lnum,nc,-4)
          DHED(1)(71:78) = LDPAG(1:NCPAG) // lnum(1:nc)
          NCHED(1) = 78
          do 100 i=1,NDHED,1
              call wrtxt (LUNSC2,DHED(i),NCHED(i),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  100     continue
          call wrtxt (LUNSC2,' ',0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          DLIN   = NDHED  + 1
          ifl    = 1
      endif
c
c...Output document buffer
c
      if (ifl .eq. 0) then
          call wrtxt (LUNSC2,' ',0,cmsg,kerr)
          DLIN   = DLIN   + 1
      endif
c
      do 300 i=1,NDOCBF,1
          DLIN   = DLIN   + 1
          call wrtxt (LUNSC2,LDOCBF(i),IDOCNC(i),cmsg,kerr)
  300 continue
      NDOCBF = 0
c
c...End of routine
c
 8000 return
      end
c***********************************************************************
c
c   SUBROUTINE:  docchk (cfil,kerr)
c
c   FUNCTION:  Checks to see if the MakePost automatic documentation file
c              can be opened.
c
c   INPUT:  cfil    C*n  D1  -  Name of documentation file to check.
c
c   OUTPUT: kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docchk (cfil,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 kerr
c
      character*(*) cfil
c
      integer*4 irecl,nc, strlen1
c
      character*20 att(4)
      character*80 msg,lbuf
      character*(MAX_PATH) fnam,ldev
      character*(MAX_FILE) lfil,lext
c
c...Construct the document file's name
c
      call fbreak (cfil,ldev,lfil,lext)
      if (lfil .eq. ' ') then
          nc     = strlen1(LMNAME)
          lfil   = 'pworks_' // LMNAME(1:nc)
      endif
      if (lext .eq. ' ') lext = '.doc'
      call fparse (lfil,fnam,ldev,lext)
c
c...Open the document file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 132
      call opnfil (LUNSC2,fnam,att,irecl,msg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Save the file name
c
      NCFN = strlen1(lfil)
      DCFNAM = lfil
c
c...End of routine
c
 8000 continue
      call clsfil (LUNSC2)
      return
c
c...Error processing file
c
 9000 call errtxt ('NODOCUM',lbuf)
      call shfile (fnam,lfil,50)
      call errstr (lbuf,lfil,1)
      call errmsg (lbuf,2,1)
      call errmsg (msg,2,2)
      kerr   = 1
      ncfn = 0
      go to 8000
      end
