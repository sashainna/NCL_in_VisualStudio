c
c***********************************************************************
c
c   FILE NAME:  vocab.for
c   CONTAINS:
c               getnms  geti4n  geti4s  getr8n  getr8s  getvnr  getvnm
c               getvwd  getwds
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        vocab.f , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:40:37
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  getnms (cbuf,knc,kwds,knwds)
c
c   FUNCTION:  This routine parses a string of vocabulary words separat-
c              ed by commas and returns the value for each word and the
c              number of words parsed.
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing vocabulary
c                               word(s) separated by commas.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: kwds    I*4  Dn  -  Array to receive vocabulary word values.
c
c           knwds   I*4  D1  -  Number of vocabulary words parsed.
c
c***********************************************************************
c
      subroutine getnms (cbuf,knc,kwds,knwds)
c
      include 'menu.inc'
c
      integer*4 knc,kwds(10),knwds
c
      character*(*) cbuf
c
      integer*4 nc,strlen1,inc,index
c
      character*24 swrd
      character*80 sbuf
c
c...Initialize routine
c
      call remspc (cbuf,sbuf,nc)
      knwds  = 0
c
c...Get values for multiple vocabulary words
c...entered on a single line
c
  100 nc     = strlen1(sbuf)
      if (nc .eq. 0) go to 8000
c
c......Break out single word
c
      inc    = index(sbuf,',')
      if (inc .eq. 0) then
          swrd   = sbuf
          sbuf   =  ' '
      else if (inc .gt. 25) then
          knwds  = knwds  + 1
          kwds(knwds) = 0
          sbuf   = sbuf(inc+1:nc)
          go to 100
      else
          swrd   = sbuf(1:inc-1)
          sbuf   = sbuf(inc+1:nc)
      endif
c
c......Get vocabulary word value
c
      knwds  = knwds  + 1
      call getvnm (swrd,kwds(knwds),MENWRD,MENWVL,NMENWD)
      go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  geti4n (cbuf,knc,knum,knwds,kerr)
c
c   FUNCTION:  This routine parses a string of integer numbers separated
c              by commas and returns the value for each number and the
c              number of values parsed.
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing real numbers
c                               separated by commas.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: knum    I*4  Dn  -  Array to receive integer values.
c
c           knwds   I*4  D1  -  Number of values parsed.
c
c           kerr    I*4  D1  -  Returns 1 if an error occurred.
c
c***********************************************************************
c
      subroutine geti4n (cbuf,knc,knum,knwds,kerr)
c
      integer*4 knc,knwds,kerr,knum(20)
c
      character*(*) cbuf
c
      integer*4 nc,strlen1,inc,index
c
      character*20 swrd
      character*80 sbuf
c
c...Initialize routine
c
      kerr   = 0
      knwds  = 0
      call remspc (cbuf,sbuf,nc)
c
c...Get values for multiple integer numbers
c...entered on a single line
c
  100 nc     = strlen1(sbuf)
      if (nc .eq. 0) go to 8000
c
c......Break out single value
c
      inc    = index(sbuf,',')
      if (inc .eq. 0) then
          swrd   = sbuf
          sbuf   =  ' '
      else if (inc .gt. 20) then
          kerr   = 1
          go to 8000
      else
          swrd   = sbuf(1:inc-1)
          sbuf   = sbuf(inc+1:nc)
          nc     = inc    - 1
      endif
c
c......Get integer value
c
      knwds  = knwds  + 1
      call ctoi (swrd(1:nc),knum(knwds),kerr)
      if (kerr .eq. 1) go to 8000
      go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  geti4s (gnum,knwds,cbuf,knc)
c
c   FUNCTION:  This routine creates a character string of integer num-
c              bers separated by commas, using the input arrary.
c
c   INPUT:  knum    I*4  Dn  -  Array of real numbers.
c
c           knwds   I*4  D1  -  Number of values in 'knum'.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string containing numbers se-
c                               parated by commas.
c
c           knc     C*n  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine geti4s (knum,knwds,cbuf,knc)
c
      integer*4 knum(10),knwds,knc
c
      character*(*) cbuf
c
      integer*4 nc,i
c
      character*8 sbuf
      character*256 tbuf
c
c...Initialize routine
c
      cbuf   = ' '
      knc    = 0
c
c...Convert array of values to a string
c
      do 500 i=1,knwds,1
c
c......Convert value to vocabulary word &
c......Append to text string
c
          call itoc (knum(i),sbuf,nc,0)
          if (i .eq. 1) then
              cbuf   = sbuf
              knc    = nc
          else
              tbuf   = cbuf(1:knc) // ',' // sbuf(1:nc)
              cbuf   = tbuf
              knc    = knc    + nc     + 1
          endif
  500 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getr8n (cbuf,knc,gnum,knwds,kerr)
c
c   FUNCTION:  This routine parses a string of real numbers separated by
c              commas and returns the value for each number and the num-
c              ber of values parsed.
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing real numbers
c                               separated by commas.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: kwds    R*8  Dn  -  Array to receive real values.
c
c           knwds   I*4  D1  -  Number of values parsed.
c
c***********************************************************************
c
      subroutine getr8n (cbuf,knc,gnum,knwds,kerr)
c
      integer*4 knc,knwds,kerr
c
      real*8 gnum(10)
c
      character*(*) cbuf
c
      integer*4 nc,strlen1,inc,index
c
      character*20 swrd
      character*80 sbuf
c
c...Initialize routine
c
      kerr   = 0
      knwds  = 0
      call remspc (cbuf,sbuf,nc)
c
c...Get values for multiple real numbers
c...entered on a single line
c
  100 nc     = strlen1(sbuf)
      if (nc .eq. 0) go to 8000
c
c......Break out single value
c
      inc    = index(sbuf,',')
      if (inc .eq. 0) then
          swrd   = sbuf
          sbuf   =  ' '
      else if (inc .gt. 20) then
          kerr   = 1
          go to 8000
      else
          swrd   = sbuf(1:inc-1)
          sbuf   = sbuf(inc+1:nc)
      endif
c
c......Get real value
c
      knwds  = knwds  + 1
      call ctor (swrd,gnum(knwds),kerr)
      if (kerr .eq. 1) go to 8000
      go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getr8s (gnum,knwds,cbuf,knc)
c
c   FUNCTION:  This routine creates a character string of real numbers
c              separated by commas, using the input arrary.
c
c   INPUT:  gnum    R*8  Dn  -  Array of real numbers.
c
c           knwds   I*4  D1  -  Number of values in 'gnum'.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string containing numbers se-
c                               parated by commas.
c
c           knc     C*n  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine getr8s (gnum,knwds,cbuf,knc)
c
      integer*4 knwds,knc
c
      real*8 gnum(10)
c
      character*(*) cbuf
c
      integer*4 nc,i
c
      character*8 sbuf
      character*256 tbuf
c
c...Initialize routine
c
      cbuf   = ' '
      knc    = 0
c
c...Convert array of values to a string
c
      do 500 i=1,knwds,1
c
c......Convert value to vocabulary word &
c......Append to text string
c
          call rtoc (gnum(i),sbuf,nc)
          if (i .eq. 1) then
              cbuf   = sbuf
              knc    = nc
          else
              tbuf   = cbuf(1:knc) // ',' // sbuf(1:nc)
              cbuf   = tbuf
              knc    = knc    + nc     + 1
          endif
  500 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getvnr (cbuf,ktab,kntab,knum,cword,kwval,knwrds)
c
c   FUNCTION:  This routine receives as input a character string and
c              finds the vocabulary word that (partially) matches it,
c              from a list of furnished accepted vocabulary word values.
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing (partial)
c                               vocabulary word.
c
c           ktab    I*4  Dn  -  Array of acceptable vocabulary word
c                               values.  Only the words with these val-
c                               ues will be checked for match.
c
c           kntab   I*4  D1  -  Number of values in 'ktab'.
c
c           cword   C*24 Dn  -  Array of recognized vocabulary words.
c
c           kwval   I*4  Dn  -  Array of vocabulary word values.
c
c           knwrds  I*4  D1  -  Number of recognized vocabulary words &
c                               values.
c
c   OUTPUT: knum    I*4  D1  -  A pointer within the array 'ktab' that
c                               matches the (partial) vocabulary word
c                               supplied in 'cbuf'.  Returns 0 if a
c                               match was not found.
c
c***********************************************************************
c
      subroutine getvnr (cbuf,ktab,kntab,knum,cword,kwval,knwrds)
c
      integer*4 ktab(10),kntab,knum,kwval(100),knwrds
c
      character*(*) cbuf,cword(100)
c
      integer*4 nc,nc1,i,strlen1
c
      character*24 sbuf,str
c
      call touppr (cbuf,str)
      nc1    = strlen1(str)
      if (nc1 .eq. 0) go to 9000
c
c...Check for closest match
c
      knum   = 0
      do 200 i=1,kntab,1
          call getvwd (ktab(i),sbuf,nc,0,cword,kwval,knwrds)
          if (nc1 .le. nc .and. str .eq. sbuf(1:nc1)) then
              if (nc .eq. nc1) then
                  knum   = i
                  go to 8000
              else
                  if (knum .ne. 0) go to 9000
                  knum   = i
              endif
          endif
  200 continue
c
c...End of routine
c
 8000 return
c
c...No match found
c
 9000 knum   = 0
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getvnm (cbuf,knum,cword,kwval,knwrds)
c
c   FUNCTION:  This routine searches the vocabulary word list for an ex-
c              act match of the input string and returns the word's val-
c              ue.
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing vocabulary
c                               word to search for.
c
c           cword   C*24 Dn  -  Array of recognized vocabulary words.
c
c           kwval   I*4  Dn  -  Array of vocabulary word values.
c
c           knwrds  I*4  D1  -  Number of recognized vocabulary words &
c                               values.
c
c   OUTPUT: knum    I*4  D1  -  Value of vocabulary word.  Returns 0 if
c                               the word was not found.
c
c***********************************************************************
c
      subroutine getvnm (cbuf,knum,cword,kwval,knwrds)
c
      include 'menu.inc'
c
      integer*4 knum,kwval(100),knwrds
c
      character*(*) cbuf,cword(100)
c
      integer*4 ilo,ihi,inc,isgn
c
      real*8 rwrd(3),rnum(3)
c
      character*24 sbuf,swrd,snum
c
      equivalence (rwrd,swrd), (rnum,snum)
c
c...Initialize routine
c
      sbuf   = cbuf
      call touppr (sbuf,swrd)
      ilo    = 1
      ihi    = knwrds
c
c...Check for word match
c
  100 if (ihi .lt. ilo) go to 9000
      inc    = ilo    + (ihi-ilo) / 2
      snum   = cword(inc)
      if (rwrd(1) .lt. rnum(1)) then
          isgn = -1
      else if (rwrd(1) .gt. rnum(1)) then
          isgn = 1
      else if (rwrd(2) .lt. rnum(2)) then
          isgn = -1
      else if (rwrd(2) .gt. rnum(2)) then
          isgn = 1
      else if (rwrd(3) .lt. rnum(3)) then
          isgn = -1
      else if (rwrd(3) .gt. rnum(3)) then
          isgn = 1
      else
          isgn = 0
      endif
      if (isgn) 200,400,300
c
c...Word is less than Vocabulary word
c
  200 ihi    = inc    - 1
      go to 100
c
c...Word is greater than Vocabulary word
c
  300 ilo    = inc    + 1
      go to 100
c
c...Found vocabulary word
c
  400 knum   = abs(kwval(inc))
      go to 8000
c
c...End of routine
c
 8000 return
c
c...Vocabulary word was not found
c
 9000 knum   = 0
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getvwd (kwd,cbuf,knc,kfl,cword,kwval,knwrds)
c
c   FUNCTION:  This routine returns the vocabulary word that has the
c              value 'kwd'.  It will search for a Major and/or Minor
c              word as specified by 'kfl'.  If the input value does not
c              exist in the requested word type array and does exist in
c              as the other type, then that word will be returned.
c
c   INPUT:  kwd     I*4  D1  -  Value of vocabulary word to find.
c
c           cword   C*24 Dn  -  Array of recognized vocabulary words.
c
c           kwval   I*4  Dn  -  Array of vocabulary word values.
c
c           knwrds  I*4  D1  -  Number of recognized vocabulary words &
c                               values.
c
c           kfl     I*4  D1  -  Flag that specifies which type of vo-
c                               cabulary word to search for.  0 = Don't
c                               care (Major/Minor).  1 = Major word.
c                               2 = Minor word.
c
c   OUTPUT: cbuf    C*n  D1  -  Vocabulary word with the value 'kwd'.
c                               Returns value string if the vocabulary
c                               word was not found.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine getvwd (kwd,cbuf,knc,kfl,cword,kwval,knwrds)
c
      include 'menu.inc'
c
      integer*4 kwd,knc,kwval(100),knwrds,kfl
c
      character*(*) cbuf,cword(100)
c
      integer*4 i,strlen1,inum,ifnd
c
      character*10 sbuf
c
c...Find vocabulary word value
c......Search for Major word
c
      ifnd   = 0
      if (kfl .eq. 1) then
          inum   = 0 - kwd
          do 100 i=1,knwrds,1
              if (inum .eq. kwval(i)) go to 300
              if (kwd .eq. kwval(i)) ifnd = i
  100     continue
          if (ifnd .ne. 0) then
              i      = ifnd
              go to 300
          endif
c
c......Search for Minor word
c
      else if (kfl .eq. 2) then
          inum   = 0 - kwd
          do 150 i=1,knwrds,1
              if (kwd .eq. kwval(i)) go to 300
              if (inum .eq. kwval(i)) ifnd = i
  150     continue
          if (ifnd .ne. 0) then
              i      = ifnd
              go to 300
          endif
c
c......Search for either Major or Minor word
c
      else
          do 200 i=1,knwrds,1
              if (kwd .eq. abs(kwval(i))) go to 300
  200     continue
      endif
c
c...Value was not found
c
      call itoc (kwd,sbuf,knc,0)
      cbuf   = '*' // sbuf(1:knc) // '*'
      knc    = knc    + 2
      go to 8000
c
c...Value was found
c...Return vocabulary word
c
  300 cbuf   = cword(i)
      knc    = strlen1(cbuf)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getwds (kwds,knwds,cbuf,knc,cword,kwval,knwrds)
c
c   FUNCTION:  This routine accepts as input an array of vocabulary word
c              values and returns a character string containing the ac-
c              tual word(s) separated by commas.
c
c   INPUT:  kwds    I*4  Dn  -  Array of vocabulary word values.
c
c           knwds   I*4  D1  -  Number of values in 'kwds'.
c
c           cword   C*24 Dn  -  Array of recognized vocabulary words.
c
c           kwval   I*4  Dn  -  Array of vocabulary word values.
c
c           knwrds  I*4  D1  -  Number of recognized vocabulary words &
c                               values.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string containing vocabulary
c                               word(s) separated by commas.
c
c           knc     C*n  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine getwds (kwds,knwds,cbuf,knc,cword,kwval,knwrds)
c
      integer*4 kwds(10),knwds,knc,kwval(100),knwrds
c
      character*(*) cbuf,cword(100)
c
      integer*4 nc,i
c
      character*24 sbuf
      character*80 tbuf
c
c...Initialize routine
c
      cbuf   = ' '
      knc    = 0
c
c...Convert array of values
c...to a string containing the appropriate
c...vocabulary words
c
      do 500 i=1,knwds,1
c
c......Convert value to vocabulary word &
c......Append to text string
c
          call getvwd (kwds(i),sbuf,nc,0,cword,kwval,knwrds)
          if (i .eq. 1) then
              cbuf   = sbuf
              knc    = nc
          else
              tbuf   = cbuf(1:knc) // ',' // sbuf(1:nc)
              cbuf   = tbuf
              knc    = knc    + nc     + 1
          endif
  500 continue
c
c...End of routine
c
 8000 return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  getr8nl (cbuf,knc,gnum,knwds,kerr)
c
c   FUNCTION:  This routine parses a string of real numbers separated by
c              commas and returns the value for each number and the num-
c              ber of values parsed. cbuf can be up to 1000 chars
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing real numbers
c                               separated by commas.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: kwds    R*8  Dn  -  Array to receive real values.
c
c           knwds   I*4  D1  -  Number of values parsed.
c
c***********************************************************************
c
      subroutine getr8nl (cbuf,knc,gnum,knwds,kerr)
c
      integer*4 knc,knwds,kerr
c
      real*8 gnum(100)
c
      character*(*) cbuf
c
      integer*4 nc,strlen1,inc,index
c
      character*20 swrd
      character*1000 sbuf
      character*1 lc,lqot
c
c...Initialize routine
c
      kerr   = 0
      knwds  = 0
c
c...Remove spaces from string
c
      lqot   = ' '
      nc    = 0
      do 50 i=1,knc,1
          lc     = cbuf(i:i)
          if (lqot .eq. ' ') then
              if (lc .eq. ' ') go to 50
              if (lc .eq. '''' .or. lc .eq. '"') lqot = lc
          else
              if (lc .eq. lqot) lqot = ' '
          endif
          nc    = nc    + 1
          sbuf(nc:nc) = lc
   50 continue
      sbuf  = sbuf(1:nc)
c
c...Get values for multiple real numbers
c...entered on a single line
c
  100 nc     = strlen1(sbuf)
      if (nc .eq. 0) go to 8000
c
c......Break out single value
c
      inc    = index(sbuf,',')
      if (inc .eq. 0) then
          swrd   = sbuf
          sbuf   =  ' '
      else if (inc .gt. 20) then
          kerr   = 1
          go to 8000
      else
          swrd   = sbuf(1:inc-1)
          sbuf   = sbuf(inc+1:nc)
      endif
c
c......Get real value
c
      knwds  = knwds  + 1
      call ctor (swrd,gnum(knwds),kerr)
      if (kerr .eq. 1) go to 8000
      go to 100
c
c...End of routine
c
 8000 return
      end
