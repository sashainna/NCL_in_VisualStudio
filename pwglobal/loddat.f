c
c***********************************************************************
c
c   FILE NAME: loddat.for
c   CONTAINS:
c               getsap  loddat  lodspr  lodwrd  lodsyn  wparse  cloddat
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        loddat.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:17
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  getsap (clab,kpt,kdesc,clabl)
c
c   FUNCTION:  This routine returns the pointer inside the label array
c              'clabl' for the entry 'clab'.  If 'clab' is not found the
c              pointer to the 'DEFAULT' label will be returned.
c
c   INPUT:  clab    C*n  D1  -  The label to search for in the 'clabl'
c                               array.
c
c           kdesc   I*4  D1  -  A descriptor array of the 'clabl' array.
c                               'kdesc(5)' is used and contains the num-
c                               ber of labels in 'clabl'.
c
c           clabl   C*n  Dn  -  An array of recognized labels.
c
c   OUTPUT: kpt     I*4  D1  -  The pointer in the 'clabl' label array
c                               for the entry 'clab'.
c
c***********************************************************************
c
      subroutine getsap (clab,kpt,kdesc,clabl)
c
      include 'menu.inc'
c
      integer*4 kpt,kdesc(5)
c
      character*(*) clab,clabl(100)
c
      integer*4 ilo,ihi,inc
c
      real*8 rbuf,rnum
c
      character*8 sbuf,snum
c
      equivalence (rbuf,sbuf), (rnum,snum)
c
c...Prepare for label search
c
      snum   = clab
      call touppr (snum,sbuf)
   50 ilo    = 1
      ihi    = kdesc(5)
c
c...Check for label match
c
  100 if (ihi .lt. ilo) go to 500
      inc    = ilo    + (ihi-ilo) / 2
      snum   = clabl(inc)
      if (rbuf-rnum) 200,400,300
c
c...Label is less than Standalone prompt label
c
  200 ihi    = inc    - 1
      go to 100
c
c...Label is greater than Standalone prompt label
c
  300 ilo    = inc    + 1
      go to 100
c
c...Found label
c
  400 kpt    = inc
      return
c
c...Label not found
c...Use 'DEFAULT' label
c
  500 if (sbuf .eq. 'DEFAULT') go to 600
      sbuf   = 'DEFAULT'
      go to 50
c
c...No 'DEFAULT' label
c...Use 1st prompt
c
  600 kpt    = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  loddat (kfl,cmsg,kerr)
c
c   FUNCTION:  This function can not be called by C++ routine, called
c              cloddat by C++ routine
c              This routine opens the Error, Menu/Prompt, Vocabulary,
c              and help files, and loads the Error & Standalone Prompt
c              text and Vocabulary words.  It also parses the Status
c              Banner text to determine the areas for the current sec-
c              tion and level text.
c
c   INPUT:  kfl     I*4  D1  -  0 = LODDAT is not called from a menu
c                               navigator (make_post), the prompt file
c                               will be closed and the help file will
c                               not be opened.  1 = LODDAT is called
c                               from a menu navigator, the prompt file
c                               will be left opened and the help file
c                               will be opened.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine loddat (kfl,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 irecl
c
      character*20 att(4)
      character*80 sbuf
      character*(MAX_PATH) fnam
c
c...Open Error Message file
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 512
      fnam   = 'posterror.MSG'
      call fparse (fnam,fnam,DVDSUP,'.MSG')
      call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Read error main record
c
      call rdprm (LUNSC1,1,IERRDS,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Load Error Messages
c
      call lodspr (LUNSC1,IERRDS,MAXERR,ERLABL,SAERR,ERRNC,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call clsfil (LUNSC1)
c
c...Open prompt file
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 512
      fnam   = 'postmenu.MSG'
      call fparse (fnam,fnam,DVDSUP,'.MSG')
      call opnfil (LUNPRM,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Read prompt main record
c
      call rdprm (LUNPRM,1,IPRMDS,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Load Standalone prompts
c
      call lodspr (LUNPRM,IPRMDS,MAXSAP,SALABL,SAPRM,SAPNC,cmsg,kerr)
      if (kfl .eq. 0) call clsfil (LUNPRM)
c
c......Determine Areas of Banner for
c......Sect & Sub-sect messages
c
      ISECP1 = 0
      ISECP2 = 0
      ISECP3 = 0
      call getsap ('Banner',IBANPT,IPRMDS,SALABL)
      sbuf   = SAPRM(IBANPT)
      call strspt (sbuf,':',ISECP1,ISECE1)
      if (ISECP1 .ne. 0) then
          ISECP1 = ISECP1 + 1
          call strspt (sbuf(ISECE1+1:80),'/',ISECP2,ISECE2)
          if (ISECP2 .ne. 0) then
              ISECP2 = ISECP2 + ISECE1 + 1
              ISECE2 = ISECE2 + ISECE1
              call strspt (sbuf(ISECE2+1:80),':',ISECP3,ISECE3)
              if (ISECP3 .ne. 0) then
                  ISECP3 = ISECP3 + ISECE2 + 1
                  ISECE3 = ISECE3 + ISECE2
              endif
          endif
      endif
c
c...Open Vocabulary file
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 512
      fnam   = 'postword.WRD'
      call fparse (fnam,fnam,DVDSUP,'.WRD')
      call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Load Vocabulary words
c
      call lodwrd (LUNSC1,MENWRD,MENWVL,NMENWD,MAXMWD,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call clsfil (LUNSC1)
c
c...Open Post Vocabulary word file
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 512
      fnam   = 'postvocab.WRD'
      call fparse (fnam,fnam,DVDSUP,'.WRD')
      call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Load Post words
c
      call lodwrd (LUNSC1,PSTWRD,PSTWVL,NPSTWD,MAXPWD,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call clsfil (LUNSC1)
c
c...Open Post Synonym word file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      fnam   = 'postvocab.syn'
      call fparse (fnam,fnam,DVDATA,'.WRD')
      call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
c
c......Load Synonym words
c
      if (kerr .eq. 0) then
          call lodsyn (LUNSC1,PSTWRD,PSTWVL,NPSTWD,MAXPWD,cmsg,kerr)
          call clsfil (LUNSC1)
          if (kerr .ne. 0) go to 8000
      endif
      kerr   = 0
c
c...Open Help file
c
      if (kfl .eq. 1) then
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'old'
          irecl  = 512
          fnam   = 'posthelp.HLP'
          call fparse (fnam,fnam,DVDSUP,'.HLP')
          call opnfil (LUNHLP,fnam,att,irecl,cmsg,kerr)
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
c   SUBROUTINE:  lodspr (klun,kdesc,kmxsap,clabl,cprm,knc,cmsg,kerr)
c
c   FUNCTION:  This routine loads the Standalone Prompt type messages
c              from the specified file.
c
c   INPUT:  klun    I*4  D1  -  Unit number of file to load prompts
c                               from.
c
c           kdesc   I*4  D1  -  A descriptor array of the prompt file.
c                               'kdesc(5)' is used and contains the num-
c                               ber of standalone prompts in the file.
c
c           kmxsap  I*4  D1  -  Maximum number of standalone prompts to
c                               load.
c
c   OUTPUT: clabl   C*n  Dn  -  An array of character strings to receive
c                               the labels of the standalone prompts.
c
c           cprm    C*n  Dn  -  An array of character strings to receive
c                               the text of the standalone prompts.
c
c           knc     I*4  Dn  -  An array of values that contain the num-
c                               ber of characters in each prompt text.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lodspr (klun,kdesc,kmxsap,clabl,cprm,knc,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kdesc(5),kmxsap,klun,knc(*),kerr
c
      character*(*) clabl(*),cprm(*),cmsg
c
      integer*4 nrec,nent,lsttxt,ibase,idat(128),tdat(128),iend,ie,
     1          ipt,i,inc,itxt,irec
c
      character*512 tcdat,icdat
c
      equivalence (tdat,tcdat), (idat,icdat)
c
c...Get ready to load Standalone Prompts
c
      nrec   = IPRMIX
      if (kdesc(5) .gt. kmxsap) kdesc(5) = kmxsap
      nent   = kdesc(5)
      lsttxt = 0
      ibase  = 0
c
c...Get # of prompts to process in this record
c
  100 iend   = nent   - ibase
      if (iend .le. 0) go to 8000
      if (iend .gt. SAPREC) iend = SAPREC
      inc    = -1
c
c...Read Standalone Prompt record
c
      call rdprm (klun,nrec,idat,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Loop through prompts
c
      do 500 i=1,iend,1
c
c......Get text pointers for this prompt
c
          inc    = inc    + 4
          irec   = idat(inc+2)
          itxt   = idat(inc+3)
c
c......Store prompt label
c
          ipt    = (inc-1) * 4 + 1
          clabl(ibase+i) = icdat(ipt:ipt+7)
c
c......Read the text record
c......if it is not the current record
c
          if (irec .ne. lsttxt) call rdprm (klun,irec,tdat,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          lsttxt = irec
c
c......Store Standalone Prompt
c
          ipt    = (itxt-1) * 4 + 5
          knc(ibase+i) = tdat(itxt)
          ie     = ipt    + knc(ibase+i) - 1
          cprm(ibase+i) = tcdat(ipt:ie)
  500 continue
c
c...Is there another Standalone Prompt record ?
c
      ibase  = ibase  + SAPREC
      nrec   = idat(1)
      if (nrec .ne. 0) go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodwrd (klun,cword,kword,knword,kmax,cmsg,kerr)
c
c   FUNCTION:  This routine loads the Vocabulary words and values from
c              the specified file.
c
c   INPUT:  klun    I*4  D1  -  Unit number of file to load vocabulary
c                               from.
c
c           kmax    I*4  D1  -  Maximum number of vocabulary words to
c                               load.
c
c   OUTPUT: cword   C*n  Dn  -  An array of character strings to receive
c                               the vocabulary words.
c
c           kword   I*4  Dn  -  An array to receive the vocabulary word
c                               values.
c
c           knword  I*4  D1  -  The actual number of vocabulary words
c                               loaded.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lodwrd (klun,cword,kword,knword,kmax,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,kword(*),kmax,knword,kerr
c
      character*(*) cword(*),cmsg
c
      integer*4 nrec,idat(128),nent,ibase,iend,i,inc,itxt
c
      character*512 icdat
c
      equivalence (idat,icdat)
c
c...Read 1st vocabulary word record
c
      nrec   = 1
      call rdprm (klun,nrec,idat,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      knword = idat(128)
      if (knword .gt. kmax) knword = kmax
      nent   = knword
      ibase  = 0
      go to 200
c
c...Read Standalone Prompt record
c
  100 call rdprm (klun,nrec,idat,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Get # of vocabulary words to process in this record
c
  200 iend   = nent
      if (iend .gt. WRDREC) iend = WRDREC
      inc    = 0
      itxt   = -27
c
c...Loop through words
c
      do 500 i=1,iend,1
c
c......Get text pointers for this prompt
c
          inc    = inc    + 7
          itxt   = itxt   + 28
c
c......Store word text & value
c
          cword(ibase+i) = icdat(itxt:itxt+23)
          kword(ibase+i) = idat(inc)
  500 continue
c
c...Is there another Vocabulary word record ?
c
      nent   = nent   - WRDREC
      if (nent .le. 0) go to 8000
      ibase  = ibase  + WRDREC
      nrec   = nrec   + 1
      go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodsyn (klun,cword,kword,knword,kmax,cmsg,kerr)
c
c   FUNCTION:  This routine loads the Synonym words and values from
c              the specified file.
c
c   INPUT:  klun    I*4  D1  -  Unit number of file to load vocabulary
c                               from.
c
c           cword   C*n  Dn  -  An array of character strings to merge
c                               the synonym words into.
c
c           kword   I*4  Dn  -  An array to merge the synonym word
c                               values into.
c
c           knword  I*4  D1  -  The number of vocabulary words already
c                               stored.
c
c           kmax    I*4  D1  -  Maximum number of vocabulary words to
c                               load.
c
c   OUTPUT: cword   C*n  Dn  -  Vocabulary array updated with synonym words.
c
c           kword   I*4  Dn  -  Word value array updated with synonym values.
c
c           knword  I*4  D1  -  The actual number of Vocabulary and synonym
c                               words loaded.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lodsyn (klun,cword,kword,knword,kmax,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,kword(*),kmax,knword,kerr
c
      character*(*) cword(*),cmsg
c
      integer*4 i,inc1,inc2,iwrdpt,iwrd,iword(720),iswapi,ipt,nc,
     1          strlen1,isw,tkword(720),idid
c
      real*8 rword(3,720),rcword(3,720)
c
      character*24 lwrd,lword(720),swapw,tcword(720)
      character*80 buf
c
      equivalence (rword,lword), (rcword,tcword)
c
c...Initialize routine
c
      iwrdpt = 0
c
c...Read text record
c
  500 call rdtxt (LUNSC1,buf,cmsg,kerr)
      if (kerr .eq. 1) go to 7000
      if (kerr .ne. 0) go to 8000
c
c...Parse record
c
      call wparse (buf,lwrd,iwrd,cmsg,kerr)
      if (kerr .ne. 0) go to 9200
      if (iwrd .eq. 0) go to 500
c
c...Store word & value in buffer
c
      if (iwrdpt .gt. kmax) go to 9100
      iwrdpt = iwrdpt + 1
      lword(iwrdpt) = lwrd
      iword(iwrdpt) = iwrd
      go to 500
c
c...End of text file
c...Sort words
c
 7000 isw   = 0
      kerr  = 0
      do 7100 i=1,iwrdpt-1,1
          idid = 0
          if (rword(1,i) .gt. rword(1,i+1)) then
              idid = 1
          else if (rword(1,i) .eq. rword(1,i+1)) then
              if (rword(2,i) .gt. rword(2,i+1)) then
                  idid = 1
              else if (rword(2,i) .eq. rword(2,i+1)) then
                  if (rword(3,i) .gt. rword(3,i+1)) idid = 1
              endif
          endif
          if (idid .eq. 1) then
              swapw  = lword(i)
              lword(i) = lword(i+1)
              lword(i+1) = swapw
c
              iswapi = iword(i)
              iword(i) = iword(i+1)
              iword(i+1) = iswapi
c
              isw    = 1
          endif
 7100 continue
      if (isw .eq. 1) go to 7000
c
c...Merge synonyms into vocabulary arrays
c
      do 7200 i=1,knword,1
          tcword(i) = cword(i)
          tkword(i) = kword(i)
 7200 continue
c
      inc1   = 1
      inc2   = 1
      ipt    = 0
c
      do while (inc1 .le. iwrdpt .or. inc2 .le. knword)
          ipt    = ipt    + 1
c
          idid = 0
          if (inc1 .le. iwrdpt .and. inc2 .le. knword) then
              if (rcword(1,inc2) .gt. rword(1,inc1)) then
                  idid = 1
              else if (rcword(1,inc2) .eq. rword(1,inc1)) then
                  if (rcword(2,inc2) .gt. rword(2,inc1)) then
                      idid = 1
                  else if (rcword(2,inc2) .eq. rword(2,inc1)) then
                      if (rcword(3,inc2) .gt. rword(3,inc1)) idid = 1
                  endif
              endif
          endif
c
          if (inc1 .le. iwrdpt .and. (inc2 .gt. knword .or.
     1            idid .eq. 1)) then
              cword(ipt) = lword(inc1)
              kword(ipt) = iword(inc1)
              inc1   = inc1   + 1
          else
              cword(ipt) = tcword(inc2)
              kword(ipt) = tkword(inc2)
              inc2   = inc2   + 1
          endif
      enddo
      knword = ipt
c
c...End of routine
c
 8000 return
c
c...Too many vocabulary words
c
 9100 write (cmsg,9101) kmax
 9101 format ('Too many vocabulary words.  Maximum = ',i4,'.')
      kerr   = 1
      go to 8000
c
c...Invalid word descriptor
c
 9200 nc     = strlen1(buf)
      write (cmsg,9201) buf(1:nc)
 9201 format ('Invalid synonym descriptor: ',a,'.')
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wparse (cbuf,cwrd,kwrd,cmsg,kerr)
c
c   FUNCTION:  This routine parses a character string that should be in
c              the following format:
c
c                  word    MAJOR/MINOR    value
c
c   INPUT:  cbuf    C*n  D1  -  Character string.
c
c   OUTPUT: cwrd    C*n  D1  -  Contains 'word' text.  Can be 1-8 chars
c                               in length.
c
c           kwrd    I*4  D1  -  Returns the value for 'word', negative
c                               if MAJOR and positive if MINOR.
c
c           cmsg    C*n  D1  -  Returns the text of the error message
c                               when an error occurred.
c
c           kerr    I*4  D1  -  Returns 1 for an invalid character
c                               string.
c
c***********************************************************************
c
      subroutine wparse (cbuf,cwrd,kwrd,cmsg,kerr)
c
      integer*4 kwrd,kerr
c
      character*(*) cbuf,cwrd,cmsg
c
      integer*4 nc,nc1,ipt,strlen1,jpt,ipt1,jpt1
c
      character*5 sbuf
c
      character*1 ctab,cspc
      byte tab
c
      equivalence (ctab,tab)
c
      data tab / 9/, cspc/' '/
c
c...Initialize routine
c
      kerr   = 0
      nc     = strlen1(cbuf)
c
c...Check for blank line
c
      if (nc .eq. 0) then
          kwrd   = 0
          return
      endif
c
c...Get word text
c
      ipt    = index(cbuf,ctab)
      ipt1   = index(cbuf,cspc)
      if ((ipt1 .ne. 0 .and. ipt1 .lt. ipt) .or. ipt .eq. 0) ipt = ipt1
      if (ipt .eq. 0) go to 9000
      if (ipt .gt. 25) go to 9100
      call touppr (cbuf(1:ipt-1),cwrd)
      nc1 = strlen1(cwrd)
      call remspc (cwrd,cwrd,nc1)
c
c...Get MAJOR/MINOR parameter
c
      jpt    = index(cbuf(ipt+1:nc),ctab)
      jpt1   = index(cbuf(ipt+1:nc),cspc)
      if ((jpt1 .ne. 0 .and. jpt1 .lt. jpt) .or. jpt .eq. 0) jpt = jpt1
      if (jpt .eq. 0) go to 9000
      if (jpt .ne. 6) go to 9300
      call touppr (cbuf(ipt+1:ipt+5),sbuf)
      nc1 = strlen1(sbuf)
      call remspc (sbuf,sbuf,nc1)
      if (sbuf .ne. 'MINOR' .and. sbuf .ne. 'MAJOR') go to 9300
c
c...Get word value
c
      call ctoi (cbuf(ipt+jpt+1:nc),kwrd,kerr)
      if (kerr .ne. 0) go to 9200
      if (kwrd .le. 0 .or. kwrd .gt. 9999) go to 9200
      if (sbuf .eq. 'MAJOR') kwrd = kwrd * (0-1)
      return
c
c...Invalid syntax
c
 9000 cmsg   = 'A <TAB> character is required.'
      kerr   = 1
      return
c
c...Too many chars in word text
c
 9100 cmsg   = 'Word names cannot contain more than 8 characters.'
      kerr   = 1
      return
c
c...Invalid word value
c
 9200 cmsg   = 'Invalid number.'
      kerr   = 1
      return
c
c...No MAJOR/MINOR spec
c
 9300 cmsg   = 'MAJOR or MINOR specification required.'
      kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cloddat (kfl,cmsg,kerr)
c
c   FUNCTION:  This function can be called by C++ routine
c			 This routine opens the Error, Menu/Prompt, Vocabulary,
c              and help files, and loads the Error & Standalone Prompt
c              text and Vocabulary words.  It also parses the Status
c              Banner text to determine the areas for the current sec-
c              tion and level text.
c
c   INPUT:  kfl     I*4  D1  -  0 = LODDAT is not called from a menu
c                               navigator (make_post), the prompt file
c                               will be closed and the help file will
c                               not be opened.  1 = LODDAT is called
c                               from a menu navigator, the prompt file
c                               will be left opened and the help file
c                               will be opened.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine cloddat (kfl,cmsg,kerr)
c
      integer*4 kfl,kerr
c
      character*80 cmsg
c
      call loddat (kfl,cmsg,kerr)
      return
      end
