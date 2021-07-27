c*********************************************************************
c*    NAME         :  getkey.for
c*    Returns the key if you know the label of entity.
c*       CONTAINS:
c*    COPYRIGHT 1992 (c) NCCS   Inc.  All Rights Reserved.
c*    MODULE NAME AND RELEASE LEVEL
c*       getkey.for , 25.1
c*    DATE AND TIME OF LAST  MODIFICATION
c*       04/29/15 , 15:09:33
c********************************************************************/
		subroutine getkeyf(label, key)
 
		include 'com.com'
 
		character*64 label
		character*64 tmp
		integer*4 key,len,strlen1,kerr
		integer*2 dummy1,dummy2
c
		tmp = token2
		token2 = ' '
		len = strlen1(label)
c
c If label is not subscripted
c
		if (label(len:len) .ne. ')') then
			if (len .gt. 6) len = 64
			token2(1:len) = label
			if (len .lt. 64) token2(len + 1:64) = ' '
              ivxsub = 0
		endif
c
c If label is subscripted
c
        if (label(len:len) .eq. ')') then
			do 10 i = 1,len,1
				if (label(i:i) .eq. '(') then
					go to 20
				else if (i .le. 64) then
					token2(i:i) = label(i:i)
				endif
10			continue
20			call ctoi(label(i+1:len-1),ivxsub,kerr)
 
        endif
 
	
		call vstchk
c
		if (ist .ne. 1 .and. ist .ne. 14) then
			call gtdesc(tv, key, dummy1, dummy2)
		else
			key = 0
		endif
c
		token2 = tmp
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
c
      integer*4 kerr
      integer*2 knum
c
      character*(*) cdat
c
      character*20 lnum
c
      integer*4 nc
ccc
ccc
      integer*4 i,ndig,isgn,inc
      character*13 spc
      data spc /'+-1234567890 '/
c
c...Convert character data to integer data
c
      kerr   = 0
      ndig   = 0
      isgn   = 0
ccc
      if (cdat .eq. ' ') goto 9000
      nc1    = len (cdat)
      do 100 i=nc1,1,-1
          if (cdat(i:i) .gt. ' ' .and. cdat(i:i) .ne. '\0') go to 200
 100  continue
 200  nc     = i
ccc
      do 110 i=1,nc,1
          inc    = index(spc,cdat(i:i))
          if (inc .eq. 0) then
              go to 9000
          else if (inc .le. 2) then
              if (isgn .ne. 0 .or. ndig .ne. 0) go to 9000
              isgn   = 1
          else
              ndig   = 1
          endif
  110 continue
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
