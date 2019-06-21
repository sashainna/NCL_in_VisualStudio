c
c***********************************************************************
c
c   FILE NAME:  statmt
c   CONTAINS:
c               getdlm  getprn  getstm  gettok  stpars
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        statmt.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/12/15 , 17:50:28
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  getdlm (cbuf,knc,kdlm,kinc,kerr)
c
c   FUNCTION:  This routine receives a single delimeter value and deter-
c              mines the real delimiter value.
c
c              Input values:  =1 ,2 :3 +4 -5 *6 /7 <8 >9 (10 )11
c
c              Real values:  =1 ,2 :3 +4 -5 *6 /7 *8 ==9 <10 >11 <>12
c                              <=13 >=14 &=15 |=16 (17 )18
c
c   INPUT:  cbuf    C*n  D1  -  Character string currently being parsed.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kdlm    I*4  D1  -  Input delimeter value.
c
c           kinc    I*4  D1  -  Pointer within 'cbuf' to the delimiter
c                               currently being parsed.
c
c   OUTPUT: kerr    I*4  D1  -  Returns 1 when the input delimiter is
c                               considered part of a number and not a
c                               delimiter.
c
c***********************************************************************
c
      subroutine getdlm (cbuf,knc,kdlm,kinc,kerr)
c
      include 'compile.inc'
c
      integer*4 knc,kdlm,kinc,kerr
c
      character*(*) cbuf
c
      character*1 lc
c
c...Initialize routine
c
      kerr   = 0
      if (kinc .lt. knc) lc = cbuf(kinc+1:kinc+1)
c
c...Go to appropriate section
c
      NTOK   = NTOK   + 1
      ICTYP(NTOK) = 2
      go to (600,400,400,500,500,700,400,800,900,1000,1100,1200,1300),
     1       kdlm
c
c...", : /"
c
  400 RCSUB(NTOK) = kdlm
      go to 8000
c
c..."+ -"
c
  500 if (NTOK .eq. 1 .or. (ICTYP(NTOK-1) .eq. 2 .and.
     1    RCSUB(NTOK-1) .ne. 18.))
     2        go to 9000
c     1    RCSUB(NTOK-1) .ne. 17. .and. RCSUB(NTOK-1) .ne. 18.))
      RCSUB(NTOK) = kdlm
      go to 8000
c
c..."= =="
c
  600 if (kinc .lt. knc .and. lc .eq. '=') then
          kinc   = kinc   + 1
          RCSUB(NTOK) = 9
      else
          RCSUB(NTOK) = 1
      endif
      go to 8000
c
c..."* **"
c
  700 if (kinc .lt. knc .and. lc .eq. '*') then
          kinc   = kinc   + 1
          RCSUB(NTOK) = 8
      else
          RCSUB(NTOK) = 6
      endif
      go to 8000
c
c..."< <> <="
c
  800 RCSUB(NTOK) = 10
      if (kinc .lt. knc) then
          if (lc .eq. '>') then
              kinc   = kinc   + 1
              RCSUB(NTOK) = 12
          else if (lc .eq. '=') then
              kinc   = kinc   + 1
              RCSUB(NTOK) = 13
          endif
      endif
      go to 8000
c
c..."> >="
c
  900 if (kinc .lt. knc .and. lc .eq. '=') then
          kinc   = kinc   + 1
          RCSUB(NTOK) = 14
      else
          RCSUB(NTOK) = 11
      endif
      go to 8000
c
c..."&"
c
 1000 RCSUB(NTOK) = 15
      go to 8000
c
c..."|"
c
 1100 RCSUB(NTOK) = 16
      go to 8000
c
c..."("
c
 1200 RCSUB(NTOK) = 17
      go to 8000
c
c...")"
c
 1300 RCSUB(NTOK) = 18
      go to 8000
c
c...End of routine
c
 8000 return
c
c...Delimiter is part of number
c
 9000 NTOK   = NTOK   - 1
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getprn (cbuf,knc,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine parses an input statement and returns the
c              innermost parenthesis level portion of the statement.
c              Parenthesis levels that include tokens separated by
c              commas will not be broken out and the parenthesis will
c              be left in the statement.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to parse.  1st call
c                               only.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.  1st
c                               call only.
c
c           kfl     I*4  D1  -  0 = 1st call for this statement.  1 =
c                               subsequent calls.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string to receive parenthesis
c                               level of statement.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kfl     I*4  D1  -  Returns 1 when another call to this
c                               routine is required.  Returns 0 when the
c                               current statement has been fully parsed.
c
c           cmsg    C*n  D1  -  Error text when 'kerr' = 1.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred parsing
c                               the statement.
c
c***********************************************************************
c
      subroutine getprn (cbuf,knc,kfl,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 knc,kerr,kfl
c
      character*(*) cbuf,cmsg
c
      integer*4 i,iparen(10),icomma(10),ipnpt
c
      character*1 lqot,lc
      character*512 tbuf
c
c...Initialize routine
c
      kerr   = 0
      lqot   = ' '
      do 100 i=1,5,1
          iparen(i) = 0
  100 continue
      ipnpt  = 0
c
c...If this is the first call
c...to this routine then store
c...original statement
c
      if (kfl .eq. 0) then
          PRNBUF = cbuf
          NCPRN  = knc
          NPNVAR = 0
      endif
c
c...Get inner most level of parenthesis
c
      i      = 1
  200 do 500 i=i,NCPRN,1
          lc     = PRNBUF(i:i)
          if (lqot .eq. ' ') then
c
c......Found beginning parenthesis
c......Save position & increment count
c
              if (lc .eq. '(') then
                  if (ipnpt .eq. 10) go to 9300
                  ipnpt  = ipnpt  + 1
                  iparen(ipnpt) = i
                  icomma(ipnpt) = 0
              endif
c
c......Check for ending parenthesis
c
              if (lc .eq. ')') then
                  if (ipnpt .eq. 0) go to 9000
                  if (icomma(ipnpt) .eq. 0 .or. iparen(ipnpt) .eq. 0)
     1                    go to 600
c
c.........Comma found within parenthesis
c.........Do not remove parenthesis
c
                  ipnpt  = ipnpt  - 1
              endif
c
c......Check for comma
c......Commas within Parenthesis will cause
c......the parenthesis to be replaced with ='s
c......not to be broken out of the statement
c
              if (lc .eq. ',' .and. ipnpt .ne. 0) icomma(ipnpt) = 1
c
c......Check for quoted string
c
              if (lc .eq. '"' .or. lc .eq. '''') lqot = lc
c
c......Check for end of quoted string
c
          else if (lc .eq. lqot) then
              lqot = ' '
          endif
  500 continue
c
c...No parenthesis on line
c...return entire line
c
      if (ipnpt .ne. 0) go to 9100
      cbuf   = PRNBUF
      knc    = NCPRN
      kfl    = 0
      go to 8000
c
c...Found internal parenthesis level
c...Return that level only
c
  600 if (i .eq. iparen(ipnpt)+1 .or. NPNVAR .eq. MAXPRN) go to 9200
      NPNVAR = NPNVAR + 1
      IPNSMP(NPNVAR) = 0
      cbuf   = char(1) // char(NPNVAR) // '=' //
     1         PRNBUF(iparen(ipnpt)+1:i-1)
      knc    = (i-iparen(ipnpt)-1) + 3
      tbuf   = PRNBUF(1:iparen(ipnpt)-1) // cbuf(1:2) //
     1         PRNBUF(i+1:NCPRN)
      PRNBUF = tbuf
      NCPRN  = NCPRN  - (i-iparen(ipnpt)+1) + 2
      kfl    = 1
      go to 8000
c
c...End of routine
c
 8000 return
c
c...Right parenthesis without left parenthesis
c
 9000 call errtxt ('PARENRT',cmsg)
      kerr   = 1
      go to 8000
c
c...Left parenthesis without right parenthesis
c
 9100 call errtxt ('PARENLF',cmsg)
      kerr   = 1
      go to 8000
c
c...Invalid parenthesis count
c
 9200 call errtxt ('PARENIV',cmsg)
      kerr   = 1
      go to 8000
c
c...Too many nested parenthesis
c
 9300 call errtxt ('COMPLXEQ',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getstm (cbuf,knc,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a MACRO definition file.
c              It also parses the record in the following manner.  A ';'
c              delimits multiple statements on a single input line.
c              Only 1 statement will be returned at a time.  A '$' is a
c              continuation mark, meaning the statement is continued on
c              the next line.  This routine will read all continuation
c              lines and return a single statement.  A '!' specifies the
c              rest of the line is a comment.  A ':' is a label identi-
c              fier.  Labels are returned on a line by themself.
c
c   INPUT:  none.
c
c   OUTPUT: cbuf    C*512  D1  -  Character string to recieve statement.
c
c           knc     I*4    D1  -  Number of characters in 'cbuf'.
c
c           cmsg    C*n    D1  -  Error text when 'kerr' = 1.
c
c           kerr    I*4    D1  -  Returns 1 when an error occurred pars-
c                                 ing the statement.  Returns 2 when
c                                 the EOF was reached.
c
c***********************************************************************
c
      subroutine getstm (cbuf,knc,cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc,kerr
c
      character*(*) cbuf,cmsg
c
      integer*4 i,strlen1,iprn,nindex,nosp,notb
c
      byte tab
      character*1 lqot,lc,ctab
      character*6 lrem
      character*80 cword
c
      equivalence (tab,ctab)
      data tab /9/
c
c...Initialize routine
c
      kerr   = 0
      knc    = 0
      lqot   = ' '
      cbuf   = ' '
      iprn   = 0
      ILABEL = 0
      IMINST = 0
c
c...Check to see if we are in the middle
c...of parsing multiple statements on a
c...single line
c
      if (ISTMNC .gt. 0) go to 200
c
c...Read from text file
c
  100 call rdtxt (LUNSC1,LSTMT,cmsg,kerr)
      if (kerr .eq. 1) go to 8100
      if (kerr .ne. 0) go to 8000
      ISTMNC = strlen1(LSTMT)
c
c...Write listing record
c
      ICURLN = ICURLN + 1
      call lstdat (LSTMT,ISTMNC,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (ISTMNC .eq. 0) go to 100
c
c...Check for character string commands
c...LETTER  PPRINT  PARTNO  INSERT
c
      ITXCMD = 0
c
      if (ISTMNC .ge. 6) then
          nosp = nindex (LSTMT,' ')
          notb = nindex (LSTMT,ctab)
          if (notb .gt. nosp) nosp = notb
          cword = LSTMT(nosp:nosp+6)
          notb  = nosp + 6
          call touppr (cword(1:6),lrem)
          if (lrem .eq. LCOMTX(7) .and. LSTMT(notb:notb) .eq. '/')
     1          LSTMT(notb:notb) = ' '
c
          if (LSTMT(notb:notb) .ne. '/') then
             do 115 i=1,9
                if (lrem .eq. LCOMTX(i)) then
                   call getvnm (lrem,ITXCMD,PSTWRD,PSTWVL,NPSTWD)
                   cbuf   = LSTMT(nosp:ISTMNC)
                   knc    = ISTMNC - nosp + 1
                   go to 500
                endif
  115        continue
          end if
c         if (LSTMT(7:7) .ne. '/' .and. (lrem .eq. LCOMTX(1) .or.
c    1        lrem .eq. LCOMTX(2) .or. lrem .eq. LCOMTX(3)
c    2        .or. lrem .eq. LCOMTX(4) .or. lrem .eq. LCOMTX(5) .or.
c    3        lrem .eq. LCOMTX(6) .or. lrem .eq. LCOMTX(7))) then
c             call getvnm (lrem,ITXCMD,PSTWRD,PSTWVL,NPSTWD)
c             cbuf   = LSTMT(1:ISTMNC)
c             knc    = ISTMNC
c             go to 500
c         endif
      endif
c
c...Break out single command
c
  200 do 400 i=1,ISTMNC,1
          lc     = LSTMT(i:i)
c
c......Check for recognized special characters
c
          if (lqot .eq. ' ') then
              if (lc .eq. '/' .and. IMINST .eq. 0) IMINST = i
              if (lc .eq. '(') iprn = iprn + 1
              if (lc .eq. ')') iprn = iprn - 1
              if (lc .eq. ';') go to 600
              if (lc .eq. '$') then
                  if (i .eq. ISTMNC) go to 100
                  if (LSTMT(i+1:i+1) .eq. '$') go to 500
                  go to 100
              endif
              if (lc .eq. '!') go to 500
              if (lc .eq. ':' .and. iprn .eq. 0) go to 700
              if (lc .eq. '"' .or. lc .eq. '''') lqot = lc
c
c......Check for end of character string
c
          else if (lc .eq. lqot) then
              lqot = ' '
          endif
c
c......Store character in output buffer
c
          if (knc .eq. 512) go to 9000
          knc    = knc    + 1
          cbuf(knc:knc) = lc
  400 continue
c
c...Entire single command was on a line
c
  500 ISTMNC = 0
      if (knc .eq. 0) go to 100
      go to 8000
c
c...Command was terminated by a ';'
c
  600 LSTMT  = LSTMT(i+1:ISTMNC)
      ISTMNC = ISTMNC - i
      go to 8000
c
c...Command was terminated by a ':' (Label)
c
  700 call remspc (cbuf,cbuf,knc)
      if (knc .gt. 24) go to 9200
      ILABEL = 1
      go to 600
c
c...End of routine
c
 8000 return
c
c...End of file
c
 8100 if (ISTMNC .ne. 0) go to 9100
      kerr   = 2
      go to 8000
c
c...Too many characters in statement
c
 9000 call errtxt ('STLONG',cmsg)
      kerr   = 1
      go to 8000
c
c...End of file with
c...continuation record active
c
 9100 call errtxt ('STENDCON',cmsg)
      kerr   = 1
      go to 8000
c
c...Too many characters in label
c
 9200 call errtxt ('LABMAX',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gettok (cbuf,knc,ktyp,gnum)
c
c   FUNCTION:  This routine determines the token type of a character
c              string.
c
c              Token types:  1=Vocabulary word,  3=number, 4=text string
c                            5=real scalar, 6=text scalar,
c                            7=quoted text string, 8=format number
c
c   INPUT:  cbuf    C*n  D1  -  Character string to parse.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: ktyp    I*4  D1  -  Token type.
c
c           gnum    R*8  D1  -  Sub-type of token (vocabulary word val-
c                               ue, number, pointer to variable, etc.)
c
c***********************************************************************
c
      subroutine gettok (cbuf,knc,ktyp,gnum)
c
      include 'menu.inc'
c
      integer*4 knc,ktyp
c
      real*8 gnum
c
      character*(*) cbuf
c
      integer*4 inum,ierr,nc,strlen1,i,index
c
      real*8 rnum
c
      character*2 preg(MAXFMT),ltmp
      character*14 lreg
      character*80 lbuf
c
      data preg /'A1','A2','A3','B1','B2','B3','C1','C2','C3','C4','C5',
     1           'C6','D ','E ','F1','F2','F3','G0','G1','G2','G3','G4',
     2           'G5','G6','G7','G8','G9','GA','H ','I1','I2','J1','J2',
     3           'K1','K2','L ','M0','M1','M2','M3','M4','M5','M6','M7',
     4           'M8','M9','MA','N ','O ','P ','Q ','R ','S ','T ','U1',
     5           'U2','V1','V2','W1','W2','X1','X2','Y1','Y2','Z1','Z2',
     6           'AA','AB','AC','AD','AE','AF','AG','AH','AI','AJ','AK',
     7           'AL','AM','AN','AO','AP','AQ','AR','AS','AT','AU','AV',
     8           'AW','AX','AY','AZ'/
c
      data lreg /'GM XUYVZWABC@F'/
c
c...Check for vocabulary word
c
      call getvnm (cbuf,inum,PSTWRD,PSTWVL,NPSTWD)
      if (inum .ne. 0) then
          ktyp   = 1
          gnum   = inum
          go to 8000
      endif
c
c...Check for format number
c
      if (cbuf(1:1) .eq. '#') then
          lbuf   = cbuf(2:)
c
c......Physical register descriptor
c......#A1, #G0, etc.
c
          nc     = strlen1(lbuf)
          if (nc .eq. 1 .or. nc .eq. 2) then
              do 100 i=1,MAXFMT,1
                  call touppr(lbuf,ltmp)
                  if (ltmp .eq. preg(i)) then
                      ktyp   = 8
                      gnum   = i
                      go to 8000
                  endif
  100         continue
          endif
c
c......Logical register descriptor
c......#G, #X, etc.
c......Which will be converted into a numeric descriptor
c
          if (nc .eq. 1) then
              call touppr(lbuf,ltmp)
              i      = index(lreg,ltmp(1:1))
              if (i .ne. 0) then
                  ktyp   = 3
                  gnum   = i      * (-1)
                  go to 8000
              endif
          endif
c
c......Numeric register descriptor
c
          call ctor (lbuf,rnum,ierr)
          if (ierr .eq. 0 .and. rnum .ge. 1 .and. rnum .le. MAXFMT) then
              ktyp   = 8
              gnum   = rnum
              go to 8000
          endif
      endif
c
c...Check for +- sign only
c
      if (knc .eq. 1 .and. (cbuf(1:1) .eq. '+' .or. cbuf(1:1) .eq. '-'))
     1    then
          ktyp   = 2
          gnum   = 4.
          if (cbuf(1:1) .eq. '-') gnum = 5.
          go to 8000
      endif
c
c...Check for real scalar
c
      call ctor (cbuf,rnum,ierr)
      if (ierr .eq. 0) then
          ktyp   = 3
          gnum   = rnum
          go to 8000
      endif
c
c...Text
c
      ktyp   = 4
      go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stpars (cbuf,knc,kpc,cmsg,kerr,kfcom)
c
c   FUNCTION:  This routine parses an input character string & sets up
c              common arrays that contain the type(s) of tokens and
c              their values.
c
c              Token types:  1=Vocabulary word,  2=delimiter(operator)
c                            3=number, 4=text string, 5=real scalar,
c                            6=text scalar, 7=quoted text string,
c                            8=format number
c
c   INPUT:  cbuf    C*n  D1  -  Character string to parse.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kpc     I*4  D1  -  1 = % is a comment character,
c                               2 = &() are not delimeters.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred parsing
c                               'cbuf'.
c
c           kfcom   I*4  D1  -  Returns 1 when string contains comment.
c
c***********************************************************************
c
      subroutine stpars (cbuf,knc,kpc,cmsg,kerr,kfcom)
c
      include 'compile.inc'
c
      integer*4 knc,kpc,kfcom,kerr
c
      character*(*) cbuf,cmsg
c
      integer*4 inc,i,nc,ierr,nnum
c
      character*1 lc,lqot
      character*16 ldel
      character*512 lbuf
      byte tab
c
      equivalence (tab,ldel(15:15))
c
      data ldel /'=,:+-*/<>&|() 	'/
      character*11 lnum /'0123456789.'/
c
      tab    = 9
c
c...Initialize routine
c
      kerr   = 0
      kfcom  = 0
      lbuf   = ' '
      lqot   = ' '
      nc     = 0
      nnum   = 0
      NTOK   = 0
c
c...Parse input buffer
c
      inc    = 0
  100 inc    = inc    + 1
      if (inc .gt. knc) go to 8000
          lc     = cbuf(inc:inc)
          if (lqot .eq. ' ') then
              if (cbuf(inc:inc+1) .eq. '$$' .or.
     -            (lc .eq. '%'.and.kpc.ne.0)) then
                 if (NTOK .gt. 0 .and. ICTYP(NTOK) .ne. 4 .or.
     -               NTOK .eq. 0) then
                     knc = inc - 1
                     kfcom = 1
                     go to 8000
                 endif
              endif
c
c......Search for number
c
cc              if (nc .eq. 0 .or. nnum .gt. 0) then
                if (nc .eq. 0) nnum = 0
                if (nnum.eq.1 .and. (lc.eq.'E' .or. lc.eq.'e')) then
                  nnum = 2
                else
                  i = index (lnum,lc)
                  if (i .eq. 0) then
                    if (nnum.eq.2 .and. (lc.eq.'+' .or. lc.eq.'-')) then
                      nnum = 3
                    else
                      nnum = 0
                    endif
                  else
                    nnum = 1
                  endif
                endif
cc              endif
c
c......Search for delimiting character
c
              i      = index(ldel,lc)
              if (i .eq. 0 .or. (kpc .eq. 2 .and. (i .eq. 10 .or.
     1            i .eq. 12 .or. i .eq. 13))) go to 1500
              if (nnum.eq.3) goto 1500
c
c.........Delimiting character found
c.........Store current variable
c
              if (nc .ne. 0) then
                  if (NTOK .ge. MAXTOK) go to 9000
                  NTOK   = NTOK   + 1
                  call gettok (lbuf,nc,ICTYP(NTOK),RCSUB(NTOK))
                  if (ICTYP(NTOK) .eq. 4) then
                      RCSUB(NTOK) = ITXTPT
                      ICNC(NTOK) = ITXTPT + nc     - 1
                      LCTXT(ITXTPT:ICNC(NTOK)) = lbuf(1:nc)
                      ITXTPT = ITXTPT + nc
                  endif
                  lbuf   = ' '
                  nc     = 0
              endif
c
c.........Determine operator from delimiter
c
              if (i .eq. 14 .or. i .eq. 15) go to 1000
              call getdlm (cbuf,knc,i,inc,ierr)
              if (ierr .eq. 1) go to 1500
              go to 100
c
c.........Space or TAB
c
 1000         if (nc .ne. 0) then
                  if (NTOK .ge. MAXTOK) go to 9000
                  NTOK   = NTOK   + 1
                  call gettok (lbuf,nc,ICTYP(NTOK),RCSUB(NTOK))
                  if (ICTYP(NTOK) .eq. 4) then
                      RCSUB(NTOK) = ITXTPT
                      ICNC(NTOK) = ITXTPT + nc     - 1
                      LCTXT(ITXTPT:ICNC(NTOK)) = lbuf(1:nc)
                      ITXTPT = ITXTPT + nc
                  endif
                  lbuf   = ' '
                  nc     = 0
              endif
              go to 100
c
c......Check for parenthesis variable
c
 1500         if (lc .eq. char(1)) then
                  if (nc .ne. 0) then
                      if (NTOK .ge. MAXTOK) go to 9000
                      NTOK   = NTOK   + 1
                      call gettok (lbuf,nc,ICTYP(NTOK),RCSUB(NTOK))
                      if (ICTYP(NTOK) .eq. 4) then
                          RCSUB(NTOK) = ITXTPT
                          ICNC(NTOK) = ITXTPT + nc     - 1
                          LCTXT(ITXTPT:ICNC(NTOK)) = lbuf(1:nc)
                          ITXTPT = ITXTPT + nc
                      endif
                      lbuf   = ' '
                      nc     = 0
                  endif
c
                  NTOK   = NTOK   + 1
                  ICTYP(NTOK) = 5
                  inc    = inc    + 1
                  RCSUB(NTOK) = ichar(cbuf(inc:inc))
                  go to 100
              endif
c
c......Quote character
c
              if (lc .eq. '"' .or. lc .eq. '''') then
                  if (nc .ne. 0) go to 9100
                  lqot   = lc
                  go to 100
              endif
c
c......Store character in token buffer
c
              nc     = nc     + 1
              lbuf(nc:nc) = cbuf(inc:inc)
              go to 100
c
c......Quoted character string in effect
c
          else
              if (lc .eq. lqot) then
                  NTOK   = NTOK   + 1
                  ICTYP(NTOK) = 7
                  RCSUB(NTOK) = ITXTPT
                  ICNC(NTOK) = ITXTPT + nc     - 1
                  LCTXT(ITXTPT:ICNC(NTOK)) = lbuf(1:nc)
                  ITXTPT = ITXTPT + nc
                  lbuf   = ' '
                  nc     = 0
                  lqot   = ' '
                  go to 100
              else
                  nc     = nc     + 1
                  lbuf(nc:nc) = lc
                  go to 100
              endif
          endif
c
c...End of routine
c...Parse last token
c
 8000 if (nc .ne. 0) then
          if (NTOK .ge. MAXTOK) go to 9000
          NTOK   = NTOK   + 1
          call gettok (lbuf,nc,ICTYP(NTOK),RCSUB(NTOK))
          if (ICTYP(NTOK) .eq. 4) then
              RCSUB(NTOK) = ITXTPT
              ICNC(NTOK) = ITXTPT + nc     - 1
              LCTXT(ITXTPT:ICNC(NTOK)) = lbuf(1:nc)
              ITXTPT = ITXTPT + nc
          endif
          lbuf   = ' '
          nc     = 0
      endif
c
 8100 return
c
c...Equation is too complex
c
 9000 call errtxt ('COMPLXEQ',cmsg)
      kerr   = 1
      go to 8100
c
c...Invalid syntax
c
 9100 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8100
      end
