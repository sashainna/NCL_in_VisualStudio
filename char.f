C*********************************************************************
C*    NAME         :  char.for
C*       CONTAINS:
C*					jindex  nindex  rindex1  nrndex  rtoc    rtoi
C*             strlen1 touppr  remspc  itoc    ctob    btoc
C*             ctor
C*
C*    COPYRIGHT 1993 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       char.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:32
C*********************************************************************
C
c
c***********************************************************************
c
c   E-FUNCTION:  jindex (kary,knum,ksiz)
c
c   FUNCTION:  This routine returns the first location of 'kary' that
c              contains the number 'knum'.
c
c   INPUT:  kary    I*2  Dn  -  Array of values to search.
c
c           knum    I*2  D1  -  Number to search for.
c
c           ksiz    I*2  D1  -  Size of array 'kary'.
c
c   OUTPUT: jindex  I*2  D1  -  Pointer within 'kary' to position that
c                               contains the value 'knum'.  Returns 0 if
c                               the array does not contain the value
c                               'knum'.
c
c***********************************************************************
c
      integer*2 function jindex (kary,knum,ksiz)
c
      integer*2 kary(*),knum,ksiz
c
      integer*2 i
c
      jindex = 0
c
c...Search for 1st occurrence of 'knum' in 'kary'
c
      do 100 i=1,ksiz,1
          if (knum .eq. kary(i)) go to 200
  100 continue
      go to 8000
c
c...Found value
c
  200 jindex = i
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
c   E-FUNCTION:  rindex1 (cstr1,cstr2)
c
c   FUNCTION:  This routine returns the last location of 'cstr1' that
c              matches the string 'cstr2'.
c
c   INPUT:  cstr1   C*n  D1  -  Character string to search in.
c
c           cstr2   C*n  D1  -  Character string to search for.
c
c   OUTPUT: rindex1 I*4  D1  -  Pointer to last occurrence of
c                               'cstr2' within 'cstr1'.  Returns 0 if
c                               'cstr1' does not contain 'cstr2'
c                               string.
c
c***********************************************************************
c
      integer*4 function rindex1 (cstr1,cstr2)
c
      character*(*) cstr1,cstr2
c
      integer*4 inc,nc,nc1,strlen1
c
      rindex1 = 0
c
c...Search for last non-occurrence of 'cstr2' in 'cstr1'
c
      nc     = strlen1(cstr1)
      nc1    = strlen1(cstr2)
      if (nc1 .eq. 0) nc1 = 1
      inc    = nc     - nc1    + 1
  100 if (inc .le. 0) go to 300
      if (cstr1(inc:inc+nc1-1) .eq. cstr2) go to 200
      inc    = inc    - 1
      go to 100
c
c...Found non-occurence of string
c
  200 rindex1 = inc
      return
c
c...All characters match the search string
c
  300 rindex1 = 0
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
c   SUBROUTINE:  itoc (gnum,cbuf,knc)
c
c   FUNCTION:  This routine converts an interger number to a character
c              string.
c
c   INPUT:  gnum    I*8  D1  -  Integer value to be converted to charac-
c                               ter string.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string from 'gnum'.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine itoc (gnum,cbuf,knc)
c
      integer*4 knc
c
      integer*4 gnum
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
   10 format (i10)
c
c...Return significant digits only
c
      ist    = nindex(snum,' ')
      ien    = nrndex(snum,' ')
      cbuf   = snum(ist:ien)
      knc    = strlen1(cbuf)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rtoi (gnum,knum)
c
c   FUNCTION:  This routine converts a real number to an integer number.
c              It makes sure that the conversion does not cause an
c              integer overflow.
c
c   INPUT:  gnum    R*8  D1  -  Real value to be converted to integer.
c
c   OUTPUT: kbuf    I*4  D1  -  Integer number from 'gnum'.
c
c***********************************************************************
c
      subroutine rtoi (gnum,knum)
c
      integer*4 knum
c
      real*8 gnum
c
c...Make sure the input value
c...is not too large
c
      if (gnum .gt. 2147483647) then
          knum   = 2147483647
      else if (gnum .lt. -2147483648) then
          knum   = -2147483648
      else if (gnum .gt. -.0001 .and. gnum .lt. .0001) then
          knum = 0
      else
          knum   = (gnum + (4.9d-8*(dabs(gnum)/gnum)))
      endif
c
c...End of routine
c
 8000 return
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
      character*(*) cstr
c
      integer*4 nc
c
      integer*4 nc1
      byte l
      character*1 b
      equivalence (b,l)
c
c...Find the last non-blank
c...character in string
c
      if (cstr .eq. ' ') then
          strlen1 = 0
      else
          nc1 = len(cstr)
          do 1234 i=nc1,1,-1
            b = cstr(i:i)
            if (cstr(i:i) .ne. ' ' .and. cstr(i:i) .ne. '	' .and.
     1          l .ne. 0) go to 2345
1234      continue
          i = 0
2345      nc = i
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
c...Convert lower case letters
c...to uppercase letters
c
      nc     = strlen1(cinp)
      do 100 i=1,nc,1
          ic     = ichar(cinp(i:i))
          if (ic .ge. ichar('a') .and. ic .le. ichar('z')) then
              cout(i:i) = char(ic-32)
          else
              cout(i:i) = char(ic)
          endif
  100 continue
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  remspc (cinp,cout)
c
c   FUNCTION:  This routine removes spaces from a character string.
c
c   INPUT:  cinp    C*n  D1  -  Original character string.
c
c   OUTPUT: cout    C*n  D1  -  Compacted character string.
c
c***********************************************************************
c
      subroutine remspc (cinp,cout)
c
      character*(*) cinp,cout
c
      integer*4 i,strlen1,nci,nco
c
      character*256 str
c
c...Convert lower case letters
c...to uppercase letters
c
      str    = cinp
      nci    = strlen1(str)
      nco    = 0
      cout   = ' '
      do 100 i=1,nci,1
          if (str(i:i) .ne. ' ' .and. str(i:i) .ne. '	') then
              nco = nco + 1
              cout(nco:nco) = str(i:i)
          endif
  100 continue
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ctob (cinp,cout)
c
c   FUNCTION:  This routine converts a character string to a NULL
c              terminated byte string.
c
c   INPUT:  cinp    C*n  D1  -  Original character string.
c
c   OUTPUT: cout    B*1  Dn  -  Null terminated byte string.
c
c***********************************************************************
c
      subroutine ctob (cinp,cout)
c
      character*(*) cinp
      byte cout(*)
c
      integer*4 i,nc,strlen1
c
      byte snum(1024)
      character*1024 sbuf
c
      equivalence (sbuf,snum)
c
c...Convert character data to text string
c
      sbuf   = cinp
      nc     = strlen1(sbuf)
      if (nc .ne. 1024) snum(nc+1) = 0
      do 100 i=1,nc+1,1
          cout(i) = snum(i)
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  btoc (cinp,cout,knc)
c
c   FUNCTION:  This routine converts a NULL terminated byte string to a
c              character string.
c
c   INPUT:  cinp    B*1  Dn  -  Null terminated byte string.
c
c   OUTPUT: cout    C*n  D1  -  Output character string.
c
c           knc     I*4  D1  -  Number of characters in 'cout'.
c
c***********************************************************************
c
      subroutine btoc (cinp,cout,knc)
c
      byte cinp(*)
      character*(*) cout
c
      integer*4 knc
c
      integer*4 i
c
      byte snum(1024)
      character*1024 sbuf
c
      equivalence (sbuf,snum)
c
c...Convert text string to character string
c
      i = 1
      sbuf = ' '
      do while (cinp(i) .ne. 0)
          snum(i) = cinp(i)
          i = i + 1
      enddo
      cout = sbuf
      knc = i - 1
c
c...End of routine
c
 8000 return
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
c   OUTPUT: gnum    R*8  D1  -  Real value from 'cdat'.
c
c           kerr    I*2  D1  -  Returns 1 on error.
c
c***********************************************************************
c
      subroutine ctor (cdat,gnum,kerr)
c
      integer*2 kerr
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
