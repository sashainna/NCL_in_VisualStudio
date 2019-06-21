c
c***********************************************************************
c
c   FILE NAME: char.for
c   CONTAINS:
c               ctoi    ctof    ctor    ftoc    itoc    justr   nindex
c               nrndex  remspc  rindex  rtoc    strlen1  strspt  tolowr
c               touppr
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        char.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/18/15 , 08:27:04
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ctoi (cdat,knum,kerr)
c
c   FUNCTION:  This routine converts a character string to an integer
c              number.
c
c   INPUT:  cdat    C*n  D1  -  Character string to be converted to
c                               integer number.
c
c   OUTPUT: knum    I*4  D1  -  Integer value from 'cdat'.
c
c           kerr    I*4  D1  -  Returns 1 on error.
c
c***********************************************************************
c
      subroutine ctoi (cdat,knum,kerr)
c
      integer*4 knum,kerr
c
      character*(*) cdat
c

      character*20 lnum
c
      integer*4 nc,strlen1
c
C IBM-HPX-START
C      integer*4 i,ndig,isgn,inc
C      character*12 spc
C      data spc /'+-1234567890'/
C IBM-HPX-END
c
c...Convert character data to integer data
c
      kerr   = 0
      nc     = strlen1(cdat)
C IBM-HPX-START
C      ndig   = 0
C      isgn   = 0
C      do 100 i=1,nc,1
C          inc    = index(spc,cdat(i:i))
C          if (inc .eq. 0) then
C              go to 9000
C          else if (inc .le. 2) then
C              if (isgn .ne. 0 .or. ndig .ne. 0) go to 9000
C              isgn   = 1
C          else
C              ndig   = 1
C          endif
C 100 continue
C IBM-HPX-END
      lnum   = ' '
      lnum(21-nc:20) = cdat(1:nc)
c      lnum(21-nc:20) = cdat(1:nc)

      read (lnum,10,err=9000) knum
   10 format (i20)
      return
c
c...Invalid numeric data
c
 9000 kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ctof (cbuf,gnum,kfmt,kerr,cmsg)
c
c   FUNCTION:  This routine converts a character string into a real num-
c              number using a predefined format array.
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing ascii num-
c                               number.
c
c           kfmt    I*2  D10 -  Format specifier.
c                               kfmt(1) = Floating point format.
c                                         1 = Leading zero suppression.
c                                         2 = Trailing zero suppression.
c                                         3 = Floating point with deci-
c                                             mal point in whole #'s.
c                                         4 = Floating point without
c                                             decimal point in whole #'s
c                               kfmt(2) = Plus sign flag.
c                               kfmt(3) = Minus sign flag.
c                               kfmt(4) = # of digits to left of decimal
c                                         point (INCH).
c                               kfmt(5) = # of digits to left of decimal
c                                         point (MM).
c                               kfmt(6) = # of digits to right of deci-
c                                         mal point (INCH).
c                               kfmt(7) = # of digits to right of deci-
c                                         mal point (MM).
c                               kfmt(8) = Min # of digits to left of
c                                         decimal point.
c                               kfmt(9) = Min # of digits to right of
c                                         decimal point.
c                               kfmt(10)  Not used.
c
c   OUTPUT: gnum    R*8  D1  -  Returns numberic value of 'cbuf' number.
c
c           cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ctof (cbuf,gnum,kfmt,kerr,cmsg)
c
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
c
      integer*4 MCHOPT(20)
c
      integer*2 kfmt(10)
      integer*4 kerr
c
      real*8 gnum
c
      character*(*) cbuf,cmsg
c
      integer*4 nc,nlft,nrgt,idot,index,imet
c
      character*1 lsgn
      character*20 lbuf,lzero,tbuf,lnum
c
      data lzero /'00000000000000000000'/

      kerr = 0
c
c...Set metric flag
c
      imet   = 0
      if (MCHOPT(1) .eq. 2 .or. MCHOPT(1) .eq. 3) imet = 1
c
c...Check for empty string
c
      call remspc (cbuf,lbuf,nc)
      if (nc .eq. 0) go to 8100
c
c...Check for decimal point
c
      idot   = index(lbuf(1:nc),'.')
c
c...Leading/Trailing zero suppression
c...Remove sign
c
      if ((kfmt(1) .eq. 1 .or. kfmt(1) .eq. 2) .and. idot .eq. 0) then
          lsgn   = '+'
          if (lbuf(1:1) .eq. '+' .or. lbuf(1:1) .eq. '-') then
              if (nc .eq. 1) go to 8100
              lsgn = lbuf(1:1)
              lbuf   = lbuf(2:nc)
              nc     = nc     - 1
          else if (kfmt(2) .ne. 0 .and. kfmt(3) .eq. 0) then
              lsgn   = '-'
          endif
c
c......Leading zero suppression
c......Add decimal point & sign
c
          if (kfmt(1) .eq. 1) then
              nrgt   = kfmt(6+imet)
              if (nrgt .gt. nc) then
                  tbuf   = lsgn // '.' // lzero(1:nrgt-nc) // lbuf(1:nc)
                  lbuf   = tbuf
                  nc     = nrgt   + 2
              else if (nrgt .lt. nc) then
                  tbuf   = lsgn // lbuf(1:nc-nrgt) // '.' //
     1                     lbuf(nc-nrgt+1:nc)
                  lbuf   = tbuf
                  nc     = nc     + 2
              else
                  tbuf   = lsgn // '.' // lbuf(1:nc)
                  lbuf   = tbuf
                  nc     = nc     + 2
              endif
c
c......Trailing zero suppression
c......Add decimal point & sign
c
          else if (kfmt(1) .eq. 2) then
              nlft   = kfmt(4+imet)
              if (nlft .gt. nc) then
                  tbuf   = lsgn // lbuf(1:nc) // lzero(1:nlft-nc) // '.'
                  lbuf   = tbuf
                  nc     = nlft   + 2
              else if (nlft .lt. nc) then
                  tbuf   = lsgn // lbuf(1:nlft) // '.' //
     1                     lbuf(nlft+1:nc)
                  lbuf   = tbuf
                  nc     = nc     + 2
              else
                  tbuf   = lsgn // lbuf(1:nc) // '.'
                  lbuf   = tbuf
                  nc     = nc     + 2
              endif
          endif
c
c...Floating point
c...Add - sign if needed
c
      else if (kfmt(2) .ne. 0 .and. kfmt(3) .eq. 0) then
          if (lbuf(1:1) .ne. '+' .and. lbuf(1:1) .ne. '-') then
              tbuf   = '-' // lbuf(1:nc)
              lbuf   = tbuf
              nc     = nc     + 1
          endif
      endif
c
c...Convert character string
c...to real number
c
      lnum   = ' '
      lnum(21-nc:20) = lbuf(1:nc)
      read (lnum,310,err=9000) gnum
  310 format (f20.0)
c
c...End of routine
c
 8000 return
c
c...Blank character string
c
 8100 gnum   = 0.
      go to 8000
c
c...Invalid number
c
 9000 call errtxt ('INVARG',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ctor (cdat,gnum,kerr)
c
c   FUNCTION:  This routine converts a character string to a real
c              number.
c
c   INPUT:  cdat    C*n  D1  -  Character string to be converted to real
c                               number.
c
c   OUTPUT: gnum    I*4  D1  -  Real value from 'cdat'.
c
c           kerr    I*4  D1  -  Returns 1 on error.
c
c***********************************************************************
c
      subroutine ctor (cdat,gnum,kerr)
c
      integer*4 kerr
c
      real*8 gnum
c
      character*(*) cdat
c
      integer*4 nc,strlen1,index,inc
c
      character*40 lnum
      character*26 low,upp
c
C IBM-HPX-START
C      integer*4 i,ndig,idot,isgn,iexp
C      character*15 spc
C      data spc /'+-.1234567890Ee'/
C IBM-HPX-END
c
      data low /'#abcdfghijklmnopqrstuvwxyz'/
      data upp /'$ABCDFGHIJKLMNOPQRSTUVWXYZ'/
c
c...Convert character data to real data
c
      kerr   = 0
      nc     = strlen1(cdat)
      inc    = index(low,cdat(1:1))
      if (inc .ne. 0) go to 9000
      inc    = index(upp,cdat(1:1))
      if (inc .ne. 0) go to 9000
C IBM-HPX-START
C      ndig   = 0
C      idot   = 0
C      isgn   = 0
C      iexp   = 0
C      do 100 i=1,nc,1
C          inc    = index(spc,cdat(i:i))
C          if (inc .eq. 0) go to 9000
C          if (inc .eq. 14 .or. inc .eq. 15) then
C             if (ndig .eq. 0 .or. iexp .eq. 1) goto 9000
C             iexp = 1
C             isgn = 0
C             idot = 0
C             ndig = 0
C          else if (inc .le. 2) then
C             if (isgn .ne. 0 .or. idot .ne. 0 .or. ndig .ne. 0)
C    1                go to 9000
C              isgn   = 1
C          else if (inc .eq. 3) then
C              if (idot .ne. 0 .or. iexp .eq. 1) go to 9000
C              idot   = 1
C          else
C              ndig   = 1
C          endif
C 100 continue
C IBM-HPX-END
cc      lnum   = ' '
cc      lnum(41-nc:40) = cdat(1:nc)
cc	lnum='3.166d-2'
cc	nc = strlen1(lnum)
cc	read (lnum(1:nc),10,err=9000) gnum
      read (cdat(1:nc),10,err=9000) gnum
   10 format (f<nc>.0)
      return
c
c...Invalid numeric data
c
 9000 kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ftoc (gnum,cbuf,knc,kfmt)
c
c   FUNCTION:  This routine converts a real number to a character
c              string using a predefined format array.
c
c   INPUT:  gnum    R*8  D1  -  Real value to be converted to charac-
c                               ter string.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string from 'gnum'.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kfmt    I*2  D10 -  Format specifier.
c                               kfmt(1) = Floating point format.
c                                         1 = Leading zero suppression.
c                                         2 = Trailing zero suppression.
c                                         3 = Floating point with deci-
c                                             mal point in whole #'s.
c                                         4 = Floating point without
c                                             decimal point in whole #'s
c                               kfmt(2) = Plus sign flag.
c                               kfmt(3) = Minus sign flag.
c                               kfmt(4) = # of digits to left of decimal
c                                         point (INCH).
c                               kfmt(5) = # of digits to left of decimal
c                                         point (MM).
c                               kfmt(6) = # of digits to right of deci-
c                                         mal point (INCH).
c                               kfmt(7) = # of digits to right of deci-
c                                         mal point (MM).
c                               kfmt(8) = Min # of digits to left of
c                                         decimal point.
c                               kfmt(9) = Min # of digits to right of
c                                         decimal point.
c                               kfmt(10)  Not used.
c
c***********************************************************************
c
      subroutine ftoc (gnum,cbuf,knc,kfmt)
c
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
c
      integer*4 MCHOPT(20)
c
      integer*2 kfmt(10)
      integer*4 knc
c
      real*8 gnum
c
      character*(*) cbuf
c
      integer*4 nindex,nrndex,ist,ineg,iacy,nlft,nrgt,imet
c
      real*8 rnum
c
      character*10 llft,lrgt
      character*20 snum,lzero,tbuf
c
      data lzero /'00000000000000000000'/
c
c...Set metric flag
c
      imet   = MCHOPT(2)
c
c...Null format specifier
c
      if (kfmt(3+imet) .eq. 0 .and. kfmt(5+imet) .eq. 0) then
          if (kfmt(2) .eq. 1 .and. gnum .gt. 0) then
              cbuf   = '+'
              knc    = 1
          else if (kfmt(3) .eq. 1 .or. gnum .lt. 0) then
              cbuf   = '-'
              knc    = 1
          else
              cbuf   = ' '
              knc    = 0
          endif
          go to 8000
      endif
c
c...Convert integer number to character string
c
      iacy   = kfmt(5+imet)
      call dpoint (gnum,rnum,iacy)
      ineg   = 0
      if (rnum .lt. 0.) ineg = 1
      rnum   = dabs(rnum)
      write (snum,10) rnum
   10 format (f20.9)
      llft   = snum(1:10)
      lrgt   = snum(12:20)
c
c...Set pointers to beginning of number,
c...ending of number & decimal point location
c
      if (llft(10:10) .eq. ' ') llft(10:10) = '0'
      ist    = nindex(llft,' ')
      if (llft(ist:ist) .eq. '0') then
          nlft   = 0
      else
          nlft   = 10     - ist    + 1
          llft   = llft(ist:10)
      endif
c
      ist    = kfmt(5+imet)
      nrgt   = nrndex(lrgt(1:ist),'0')
c
c...Leading zero suppression
c
      if (kfmt(1) .eq. 1) then
          nrgt   = kfmt(5+imet)
          if (nlft .lt. kfmt(8)) then
              if (nrgt .ne. kfmt(5+imet)) then
                  tbuf   = lrgt(1:nrgt) // lzero(1:kfmt(5+imet)-nrgt)
                  lrgt   = tbuf
                  nrgt   = kfmt(5+imet)
              endif
              tbuf   = lzero(1:kfmt(8)-nlft) // llft(1:nlft)
              llft   = tbuf
              nlft   = kfmt(8)
          endif
c
          if (nrgt .lt. kfmt(9)) then
              tbuf   = lrgt(1:nrgt) // lzero(1:kfmt(9)-nrgt)
              lrgt   = tbuf
              nrgt   = kfmt(9)
          endif
c
          if (nlft .eq. 0 .and. nrgt .gt. kfmt(9)) then
              ist    = nindex(lrgt(1:nrgt),'0')
              if (ist .eq. 0) ist = nrgt + 1
              if (nrgt-ist+1 .lt. kfmt(9)) ist = nrgt - kfmt(9) + 1
              lrgt   = lrgt(ist:nrgt)
              nrgt   = nrgt   - ist    + 1
          endif
c
c...Trailing zero suppression
c
      else if (kfmt(1) .eq. 2) then
          if (nlft .lt. kfmt(3+imet)) then
              tbuf   = lzero(1:kfmt(3+imet)-nlft) // llft(1:nlft)
              llft   = tbuf
              nlft   = kfmt(3+imet)
          endif
c
          if (nrgt .lt. kfmt(9)) then
              tbuf   = lrgt(1:nrgt) // lzero(1:kfmt(9)-nrgt)
              lrgt   = tbuf
              nrgt   = kfmt(9)
          endif
c
          if (nlft .lt. kfmt(8)) then
              tbuf   = lzero(1:kfmt(8)-nlft) // llft(1:nlft)
              llft   = tbuf
              nlft   = kfmt(8)
          endif
c
          if (nrgt .eq. 0 .and. nlft .ne. 0) then
              nlft   = nrndex(llft(1:nlft),'0')
              if (nlft .lt. kfmt(8)) nlft = kfmt(8)
          endif
c
c...Floating point & whole number
c
      else
          if (nlft .lt. kfmt(8)) then
              tbuf   = lzero(1:kfmt(8)-nlft) // llft(1:nlft)
              llft   = tbuf
              nlft   = kfmt(8)
          endif
c
          if (nrgt .lt. kfmt(9)) then
              tbuf   = lrgt(1:nrgt) // lzero(1:kfmt(9)-nrgt)
              lrgt   = tbuf
              nrgt   = kfmt(9)
          endif
c
          if (kfmt(1) .eq. 3 .or. nrgt .ne. 0) then
              if (nrgt .eq. 0) then
                  lrgt   = '.'
                  nrgt   = 1
              else
                  tbuf   = '.' // lrgt(1:nrgt)
                  lrgt   = tbuf
                  nrgt   = nrgt   + 1
              endif
          endif
      endif
c
c...Join left & right of number
c
      if (nlft .eq. 0) then
          if (nrgt .ne. 0) cbuf = lrgt(1:nrgt)
      else
          if (nrgt .eq. 0) then
              cbuf   = llft(1:nlft)
          else
              cbuf   = llft(1:nlft) // lrgt(1:nrgt)
          endif
      endif
      knc    = nlft    + nrgt
c
c...Add sign
c
      if ((ineg .eq. 1 .and. kfmt(3) .ne. 0) .or.
     1    (rnum .eq. 0. .and. (kfmt(3) .eq. 2 .or. kfmt(3) .eq. 13)))
     2        then
          if (knc .eq. 0) then
              if (kfmt(3) .eq. 3 .or. kfmt(3) .eq. 13) then
                  cbuf   = 'N('
                  knc    = 2
              else
                  cbuf   = '-'
                  knc    = 1
              endif
          else
              if (kfmt(3) .eq. 3 .or. kfmt(3) .eq. 13) then
                  tbuf   = 'N(' // cbuf(1:knc)
                  cbuf   = tbuf
                  knc    = knc    + 2
              else
                  tbuf   = '-' // cbuf(1:knc)
                  cbuf   = tbuf
                  knc    = knc    + 1
              endif
          endif
      else if (ineg .eq. 0 .and. kfmt(2) .eq. 1) then
          if (knc .eq. 0) then
              cbuf   = '+'
              knc    = 1
          else
              tbuf   = '+' // cbuf(1:knc)
              cbuf   = tbuf
              knc    = knc    + 1
          endif
      else if (ineg .eq. 0 .and. kfmt(2) .eq. 3) then
          if (knc .eq. 0) then
              cbuf   = 'P('
              knc    = 2
          else
              tbuf   = 'P(' // cbuf(1:knc)
              cbuf   = tbuf
              knc    = knc    + 2
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
c   SUBROUTINE:  itoc (knum,cbuf,knc,kdig)
c
c   FUNCTION:  This routine converts an integer number to a character
c              string.
c
c   INPUT:  knum    I*4  D1  -  Integer value to be converted to charac-
c                               ter string.
c
c           kdig    I*4  D1  -  Minimum number of digits for character
c                               string.  Preceding zeros will be used to
c                               fill the string.  A negative value spe-
c                               cifies that preceding spaces will be
c                               used to fill the string.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string from 'knum'.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine itoc (knum,cbuf,knc,kdig)
c
      integer*4 knum,knc,kdig
c
      character*(*) cbuf
c
      integer*4 ipt,nindex,idig
c
      character*1 ldlm
      character*20 sbuf
c
c...Convert integer number to character string
c
      if (kdig .le. 0) then
          write (sbuf,8) knum
    8     format (i20)
          idig   = 0 - kdig
          ldlm   = ' '
      else
          write (sbuf,10) knum
   10     format (i20.20)
          idig   = kdig
          ldlm   = '0'
      endif
c
c...Store significant digits in output string
c
      ipt    = nindex(sbuf,ldlm)
      if (ipt .eq. 0) ipt = 20
      if (20-ipt+1 .lt. idig) ipt = 20 - idig + 1
      cbuf   = sbuf(ipt:20)
      knc    = 20     - ipt    + 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  justr (cbuf,knc,kmax)
c
c   FUNCTION:  This routine right justifies a character string.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to right justify.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kmax    I*4  D1  -  Maximum number of characters in 'cbuf'.
c                               The resultant string will be padded with
c                               preceding spaces and will be 'kmax' chars
c                               long.
c
c   OUTPUT: cbuf    C*n  D1  -  Right justified character string.
c
c           knc     I*4  D1  -  Returns 'kmax'.
c
c***********************************************************************
c
      subroutine justr (cbuf,knc,kmax)
c
      integer*4 knc,kmax
c
      character*(*) cbuf
c
      integer*4 inc
c
      character*80 tbuf
c
c...Right justify the string
c
      if (knc .ge. kmax) go to 8000
      tbuf   = ' '
      inc    = kmax    - knc    + 1
      tbuf(inc:) = cbuf(1:knc)
      cbuf   = tbuf(1:kmax)
      knc    = kmax
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   E-FUNCTION:  nindex (cstr1,cstr2)
c
c   FUNCTION:  This routine returns the first location of 'cstr1' that
c              does not contain the string 'cstr2'.
c
c   INPUT:  cstr1   C*n  D1  -  Character string to search in.
c
c           cstr2   C*n  D1  -  Character string to search for.
c
c   OUTPUT: nindex  I*4  D1  -  Pointer to 1st non-occurrence of 'cstr2'
c                               within 'cstr1'.  Returns 0 if 'cstr1'
c                               consists solely of 'cstr2' strings.
c
c***********************************************************************
c
      integer*4 function nindex (cstr1,cstr2)
c
      character*(*) cstr1,cstr2
c
      integer*4 inc,nc,nc1,strlen1
c
      nindex = 0
c
c...Search for 1st non-occurrence of 'cstr2' in 'cstr1'
c
      nc     = strlen1(cstr1)
      nc1    = strlen1(cstr2)
      if (nc1 .eq. 0) nc1 = 1
      inc    = 1
  100 if (inc .gt. nc-nc1+1) go to 300
      if (cstr1(inc:inc+nc1-1) .ne. cstr2) go to 200
      inc    = inc    + nc1
      go to 100
c
c...Found non-occurrence of string
c
  200 nindex = inc
      return
c
c...All characters match the search string
c
  300 nindex = 0
      return
      end
c
c***********************************************************************
c
c   E-FUNCTION:  nrndex (cstr1,cstr2)
c
c   FUNCTION:  This routine returns the last location of 'cstr1' that
c              does not contain the string 'cstr2'.
c
c   INPUT:  cstr1   C*n  D1  -  Character string to search in.
c
c           cstr2   C*n  D1  -  Character string to search for.
c
c   OUTPUT: nrndex  I*4  D1  -  Pointer to last non-occurrence of
c                               'cstr2' within 'cstr1'.  Returns 0 if
c                               'cstr1' consists solely of 'cstr2'
c                               strings.
c
c***********************************************************************
c
      integer*4 function nrndex (cstr1,cstr2)
c
      character*(*) cstr1,cstr2
c
      integer*4 inc,nc,nc1,strlen1
c
      nrndex = 0
c
c...Search for last non-occurrence of 'cstr2' in 'cstr1'
c
      nc     = strlen1(cstr1)
      nc1    = strlen1(cstr2)
      if (nc1 .eq. 0) nc1 = 1
      inc    = nc     - nc1    + 1
  100 if (inc .le. 0) go to 300
      if (cstr1(inc:inc+nc1-1) .ne. cstr2) go to 200
      inc    = inc    - 1
      go to 100
c
c...Found non-occurence of string
c
  200 nrndex = inc
      return
c
c...All characters match the search string
c
  300 nrndex = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  remspc (cstr1,cstr2,knc)
c
c   FUNCTION:  This routine removes all of the spaces in a character
c              string, except for characters within quotes.
c
c   INPUT:  cstr1   C*n  D1  -  Original character string.
c
c   OUTPUT: cstr2   C*n  D1  -  Character string minus spaces.
c
c           knc     I*4  D1  -  Number of characters in 'cstr2'.
c
c***********************************************************************
c
      subroutine remspc (cstr1,cstr2,knc)
c
      integer*4 knc
c
      character*(*) cstr1,cstr2
c
      integer*4 i,strlen1
c
      character*1 lqot,lc
      character*512 str
c
c...Remove spaces from string
c
      str    = cstr1
      cstr2  = ' '
      lqot   = ' '
      knc    = 0
      do 100 i=1,strlen1(str),1
          lc     = str(i:i)
          if (lqot .eq. ' ') then
              if (lc .eq. ' ') go to 100
              if (lc .eq. '''' .or. lc .eq. '"') lqot = lc
          else
              if (lc .eq. lqot) lqot = ' '
          endif
          knc    = knc    + 1
          cstr2(knc:knc) = lc
  100 continue
      cstr2  = cstr2(1:knc)
      return
      end
c
c***********************************************************************
c
c   E-FUNCTION:  rindex (cstr1,cstr2)
c
c   FUNCTION:  This routine returns the last location of 'cstr2' within
c              'cstr1'.
c
c   INPUT:  cstr1   C*n  D1  -  Character string to search in.
c           cstr2   C*n  D1  -  Character string to search for.
c
c   OUTPUT: nindex  I*4  D1  -  Pointer to last occurrence of 'cstr2'
c                               within 'cstr1'.  Returns 0 if 'cstr1'
c                               does not contain the string 'cstr2'.
c
c***********************************************************************
c
      integer*4 function rindex (cstr1,cstr2)
c
      character*(*) cstr1,cstr2
c
      integer*4 inc,nc,strlen1,is
c
      rindex = 0
c
c...Find every occurrence of 'cstr2' in 'cstr1'
c
      nc     = strlen1(cstr1)
      is     = 1
  100 inc    = index(cstr1(is:nc),cstr2)
      if (inc .eq. 0) go to 200
      is     = is     + inc
      rindex = is     - 1
      go to 100
c
c...Last occurrence found
c...return pointer
c
  200 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rtoc (gnum,cbuf,knc)
c
c   FUNCTION:  This routine converts a real number to a character
c              string.
c
c   INPUT:  gnum    R*8  D1  -  Real value to be converted to charac-
c                               ter string.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string from 'gnum'.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine rtoc (gnum,cbuf,knc)
c
      integer*4 knc
c
      real*8 gnum
c
      character*(*) cbuf
c
      integer*4 nindex,nrndex,ist,ien,strlen1
c
      character*20 snum
c
c...Convert integer number to character string
c
      write (snum,10) gnum
   10 format (f20.9)
c
c...Return significant digits only
c
      ist    = nindex(snum,' ')
      if (snum(ist:ist) .eq. '.' .and. ist .ne. 1) then
          ist    = ist    - 1
          snum(ist:ist) = '0'
      endif
      ien    = nrndex(snum,'0')
      cbuf   = snum(ist:ien)
      knc    = strlen1(cbuf)
      return
      end
c
c***********************************************************************
c
c   E-FUNCTION:  strlen1 (cstr)
c
c   FUNCTION:  This routine returns the number of characters in 'cstr'.
c              The last non-space character is considered the last char-
c              acter in the string.
c
c   INPUT:  cstr    C*n  D1  -  Character string to determine the length
c                               of.
c
c   OUTPUT: strlen1  I*4  D1  -  Length of 'cstr'.
c
c***********************************************************************
c
      integer*4 function strlen1 (cstr)
c
c...added for function call lnblnk
c...Yurong 10/15/97
c
c...remove for use MFC
c...Yurong 3/24/98
c
C WNT-START
c...      USE DFPORT
C WNT-END
c
      character*(*) cstr
c
      integer*4 nc
c
C VAX-START
C     integer*4 istat,str$trim
C
C     character*256 str
C VAX-END
c
C SUN-SGI-CIM-DEC-START
C     integer*4 lnblnk,nc1
C SUN-SGI-CIM-DEC-END
C WNT-IBM-HPX-DOS-START
      integer*4 i,nc1
C WNT-IBM-HPX-DOS-END
c
c...Find the last non-blank
c...character in string
c
      if (cstr .eq. ' ') then
           strlen1 = 0
      else
C VAX-START
C         istat  = str$trim (str,cstr,nc)
C VAX-END
c
c...changed from call lnblnk to call LEN_TRIM for WNT
c...because DFPORT lib is conflicks with MFC lib
c...when we use this routine for mpost
c...Yurong 3/13/98
c
C SUN-SGI-CIM-DEC-START
C        nc     = lnblnk (cstr)
C        nc1    = len (cstr)
C
C        if (nc1 .lt. nc) nc = nc1
C SUN-SGI-CIM-DEC-END
C WNT-START
         nc = LEN_TRIM(cstr)
         nc1    = len (cstr)
         if (nc1 .lt. nc) nc = nc1
C WNT-END
c

C DOS-START
C         nc     = nblank (cstr)
C         nc1    = len (cstr)
C         if (nc1 .lt. nc) nc = nc1
C DOS-END
C IBM-HPX-START
C          nc1    = len (cstr)
C          do 100 i=nc1,1,-1
C             if (cstr(i:i) .ne. '      ' .and. cstr(i:i) .ne. ' ' .and.
C    1            cstr(i:i) .ne. '\0') go to 200
C 100     continue
C          i      = 0
C 200     nc     = i
C IBM-HPX-END

      strlen1 = nc
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  strspt (cstr,csub,kst,ken)
c
c   FUNCTION:  This routine returns the pointers to a blank section in
c              character string 'cstr' following the first occurrence of
c              the character string 'csub'.  The start of this blank
c              section will be the 1st character after 'csub'.  The end
c              will be 2 spaces before the next non-space character.
c
c   INPUT:  cstr    C*n  D1  -  Character string to search in.
c
c           csub    C*n  D1  -  Character string to search for that de-
c                               fines the beginning of the blank sec-
c                               tion.
c
c   OUTPUT: kst     I*4  D1  -  Beginning of blank section.  This will
c                               be 0 if a blank section was not found.
c
c           ken     I*4  D1  -  End of blank section.  This will be 0 if
c                               a blank section was not found.
c
c***********************************************************************
c
      subroutine strspt (cstr,csub,kst,ken)
c
      integer*4 kst,ken
c
      character*(*) cstr,csub
c
      integer*4 strlen1,nc,index,nindex
c
c...Find sub-string within string
c
      kst    = index(cstr,csub) + 1
      if (kst .eq. 1) go to 9000
c
c...Find blank area within string
c
      nc     = strlen1(cstr)
      ken    = nindex(cstr(kst:nc),' ')
      if (ken .eq. 0) then
          ken = len(cstr)
      else
          ken    = kst    + ken    - 3
      endif
      if (ken .lt. kst) go to 9000
      return
c
c...A blank area was not found
c
 9000 kst    = 0
      ken    = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tolowr (cinp,cout)
c
c   FUNCTION:  This routine converts a character string to all lower
c              case.
c
c   INPUT:  cinp    C*n  D1  -  Original character string.
c
c   OUTPUT: cout    C*n  D1  -  All lower case character string.
c
c***********************************************************************
c
      subroutine tolowr (cinp,cout)
c
      character*(*) cinp,cout
c
      integer*4 i,strlen1,nc,ic
c
      character*512 str
c
c...Convert uppercase letters
c...to lowercase letters
c
      str    = cinp
      nc     = strlen1(str)
      do 100 i=1,nc,1
          ic     = ichar(str(i:i))
          if (ic .ge. ichar('A') .and. ic .le. ichar('Z'))
     2            str(i:i) = char(ic+32)
  100 continue
      cout   = str
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  touppr (cinp,cout)
c
c   FUNCTION:  This routine converts a character string to all upper
c              case.
c
c   INPUT:  cinp    C*n  D1  -  Original character string.
c
c   OUTPUT: cout    C*n  D1  -  All upper case character string.
c
c***********************************************************************
c
      subroutine touppr (cinp,cout)
c
      character*(*) cinp,cout
c
      integer*4 i,strlen1,nc,ic
c
      character*512 str
c
c...Convert lower case letters
c...to uppercase letters
c
      str    = cinp
      nc     = strlen1(str)
      do 100 i=1,nc,1
          ic     = ichar(str(i:i))
          if (ic .ge. ichar('a') .and. ic .le. ichar('z'))
     2            str(i:i) = char(ic-32)
  100 continue
      cout   = str
      return
      end
