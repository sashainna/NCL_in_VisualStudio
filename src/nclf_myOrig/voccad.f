C********************************************************************/
C*    NAME         :  voccad
C*       CONTAINS:
C*					voccad (name,inum)
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       voccad.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:53
C********************************************************************/
C
c***********************************************************************
c
c   SUBROUTINE:  voccad (name,inum)
c
c   FUNCTION:  C callable routine which searches the NCL vocabulary
c              array for a vocabulary word.
c
c   INPUT:  name    C*8  D1  -  Vocabulary word to search for.
c
c   OUTPUT: inum    I*2  D1  -  Numeric value for vocabulary word.
c                               Returns 0 if word is not found.
c
c***********************************************************************
c
      subroutine voccad(name,inum)

      include 'com4a.com'


      integer*2 inum
      character*8 name
      integer*4 strlen1, nc
c
c...Convert NULL terminated C string to
c...FORTRAN character string
c
      nc = strlen1 (name)
      token2(1:nc) = name
      do 100 i=nc+1,64,1
		token2(i:i) = ' '
  100 continue
c
c...Search for vocabulary word
c
      inum = 0
      call vocsch
      if (ityp .eq. 1) inum = ist
      return
      end
