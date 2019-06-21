c
c***********************************************************************
c
c   FILE NAME: gensupport.for
c   CONTAINS:
c               pparse  getlvl  lstout
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        gensupport.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:31
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM:  gencrypt
c
c   FUNCTION:  This routine encrypts the text string entered by user
c              and includes this string to the posterror.txt file at
c              LOGG1 label.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  pparse (cbuf,klevl,knlevl,ktyp,cdat,cmsg,kerr)
c
c   FUNCTION:  This routine parses a character string that should con-
c              tain either a standalone prompt or menu level.
c
c   INPUT:  cbuf    C*n  D1  -  Character string.
c
c   OUTPUT: klevl   I*4  D10 -  An array containing the level values.
c
c           knlevl  I*4  D1  -  Number of levels entered (i.e. 1.2.4 =
c                               3 levels).
c
c           ktyp    I*4  D1  -  1 = Low level menu prompt, 2 = High
c                               level menu item (w/prompts underneath),
c                               3 = Extra text record for prompt, 4 =
c                               Standalone prompt.
c
c           cdat    C*n  D1  -  Text of prompt or menu item.
c
c           cmsg    C*n  D1  -  Returns label of standalone prompt or
c                               the error message text when an error
c                               occurred.
c
c           kerr    I*4  D1  -  Returns 1 for an invalid character
c                               string.
c
c***********************************************************************
c
      subroutine pparse (cbuf,klevl,knlevl,ktyp,cdat,cmsg,kerr)
c
      integer*4 klevl(10),knlevl,ktyp,kerr
c
      character*(*) cbuf,cdat,cmsg
c
      integer*4 strlen1,i,j,k,nc,nc1
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
      kerr   = 0
c
c...Check for blank line
c
      if (strlen1(cbuf) .eq. 0) then
          ktyp   = 0
          return
      endif
c
c...Text only record
c
      if (cbuf(1:1) .eq. ctab) then
          ktyp   = 3
          cdat   = cbuf(2:strlen1(cbuf))
          go to 8000
       endif
c
c...Menu/Prompt section
c...Determine if prompt sub-section
c...or Menu sub-section
c
      i      = index (cbuf,ctab)
      j      = index (cbuf,'#')
      k      = index (cbuf,'$')
      if (i .eq. 0) go to 9200
      if (j .lt. i .and. j .ne. 0) then
          ktyp   = 2
          nc     = j      - 1
      else if (k .lt. i .and. k .ne. 0) then
          ktyp   = 4
          nc     = k      - 1
      else
          ktyp   = 1
          nc     = i      - 1
      endif
c
c...Get level #'s
c
      if (ktyp .ne. 4) then
          call getlvl (cbuf,nc,klevl,knlevl,kerr)
          if (kerr .ne. 0) go to 9100
c
c...Get label for standalone prompt
c
      else
          if (nc .gt. 8) go to 9300
          call touppr(cbuf(1:nc),cmsg)
          if (nc .lt. 8) cmsg(nc+1:8) = ' '
      endif
c
c...Store text data
c
      nc     = strlen1(cbuf)
      if (nc .gt. i) then
          cdat   = cbuf(i+1:nc)
      else
          cdat   = ' '
      endif
c      nc1    = strlen1(cbuf(i+1:nc))
c      cdat   = cbuf(i+1:nc)
c
c...End of routine
c
 8000 return
c
c...Invalid text record
c
 9000 cmsg   = 'Standalone text record is only valid with prompt ' //
     1         'records.'
      kerr   = 1
      go to 8000
c
c...Invalid level number
c
 9100 cmsg   = 'Invalid level number.'
      kerr   = 1
      go to 8000
c
c...Invalid syntax
c
 9200 cmsg   = 'Invalid syntax.'
      kerr   = 1
      go to 8000
c
c...Standalone label is too long
c
 9300 cmsg   = 'Labels can be a maximum of 8 characters.'
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getlvl (cbuf,knc,klevl,knlevl,kerr)
c
c   FUNCTION:  This routine converts a character string (i.e. 1.2.4) to
c              an array of level values.
c
c   INPUT:  cbuf    C*n  D1  -  Character string.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: klevl   I*4  D10 -  An array containing the level values.
c
c           knlevl  I*4  D1  -  Number of levels entered (i.e. 1.2.4 =
c                               3 levels).
c
c           kerr    I*4  D1  -  Returns 1 for an invalid character
c                               string.
c
c***********************************************************************
c
      subroutine getlvl (cbuf,knc,klevl,knlevl,kerr)
c
      integer*4 knc,klevl(10),knlevl,kerr
c
      character*(*) cbuf
c
      integer*4 nc,i
c
      character*1 buf1(80),ldat1(80)
      character*80 buf,ldat
c
      equivalence (buf,buf1), (ldat,ldat1)
c
c...Initialize routine
c
      knlevl = 0
      kerr   = 0
      nc     = 0
c
c...Get level number
c
      buf    = cbuf(1:knc)
      do 500 i=1,knc+1,1
          if (buf1(i) .eq. '.' .or. i .eq. knc+1) then
              knlevl = knlevl + 1
              call ctoi (ldat(1:nc),klevl(knlevl),kerr)
              if (kerr .ne. 0) go to 9000
              if (klevl(knlevl) .lt. 0. .or. klevl(knlevl) .gt. 41)
     1            go to 9000
              nc     = 0
          else
              nc     = nc     + 1
              ldat1(nc) = buf1(i)
          endif
  500 continue
c
c...End of routine
c
 8000 return
c
c...Bad level number
c
 9000 kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstout
c
c   FUNCTION:  This is a dummy routine that is required because it is
c              referenced in some of the Subrouitines in the global
c              library.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
c
c...added parameters because WNT already asked exactly parameters
c...Yurong
c
      subroutine lstout (cdat,knc,cmsg,kerr)
      integer*4 knc,kerr
c
      character*(*) cdat,cmsg

      return
      end
