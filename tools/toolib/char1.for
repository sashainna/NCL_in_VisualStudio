C*********************************************************************
C*    NAME         :  char1.f
C*       CONTAINS:
C*                  ctoi    ctor    itoc    remspc  rindex
C*                  strspt  tolowr strlen1 touppr nindex
C*					nrndex 
C*
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
c*       char1.for , 25.1
c*    DATE AND TIME OF LAST  MODIFICATION
c*       04/29/15 , 15:13:29
C********************************************************************/
C
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
      character*(*) cstr
c
      integer*4 nc
c
=VMS
      integer*4 istat,str$trim
c 
      character*256 str
=UNX,W2K
      integer*4 nc1
      byte l
      character*1 b
      equivalence (b,l)
=ALL
c
c...Find the last non-blank
c...character in string
c
      if (cstr .eq. ' ') then
          strlen1 = 0
      else
=VMS
          istat  = str$trim (str,cstr,nc)
=UNX,W2K
          nc1 = len(cstr)
          do 1234 i=nc1,1,-1
            b = cstr(i:i)
            if (cstr(i:i) .ne. ' ' .and. cstr(i:i) .ne. '	' .and.
     1          l .ne. 0) go to 2345
1234      continue
          i = 0
2345      nc = i
c
c.. Now for SGI ncl = 80 instead of nc1    = len (cstr)
c
=ALL
          strlen1 = nc
      endif
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
      character*256 str
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
c
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
=UNX,W2K
      integer*4 i,ndig,isgn,inc
      character*12 spc
      data spc /'+-1234567890'/
=ALL
c
c...Convert character data to integer data
c
      kerr   = 0
      nc     = strlen1(cdat)
=UNX,W2K
      ndig   = 0
      isgn   = 0
      do 100 i=1,nc,1
          inc    = index(spc,cdat(i:i))
          if (inc .eq. 0) then
              go to 9000
          else if (inc .le. 2) then
              if (isgn .ne. 0 .or. ndig .ne. 0) go to 9000
              isgn   = 1
          else
              ndig   = 1
          endif
  100 continue
=ALL
      lnum   = ' '
      lnum(21-nc:20) = cdat(1:nc)
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
      character*20 lnum
      character*27 low,upp
c
=UNX,W2K
      integer*4 i,ndig,idot,isgn
      character*13 spc
      data spc /'+-.1234567890'/
=ALL
c
      data low /'#abcdefghijklmnopqrstuvwxyz'/
      data upp /'$ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
c
c...Convert character data to real data
c
      kerr   = 0
      nc     = strlen1(cdat)
      inc    = index(low,cdat(1:1))
      if (inc .ne. 0) go to 9000
      inc    = index(upp,cdat(1:1))
      if (inc .ne. 0) go to 9000
=UNX,W2K
      ndig   = 0
      idot   = 0
      isgn   = 0
      do 100 i=1,nc,1
          inc    = index(spc,cdat(i:i))
          if (inc .eq. 0) then
              go to 9000
          else if (inc .le. 2) then
              if (isgn .ne. 0 .or. idot .ne. 0 .or. ndig .ne. 0)
     1                go to 9000
              isgn   = 1
          else if (inc .eq. 3) then
              if (idot .ne. 0) go to 9000
              idot   = 1
          else
              ndig   = 1
          endif
  100 continue
=ALL
      lnum   = ' '
      lnum(21-nc:20) = cdat(1:nc)
      read (lnum,10,err=9000) gnum
   10 format (f20.0)
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
      character*256 str
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
