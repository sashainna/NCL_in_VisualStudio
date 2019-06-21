C*********************************************************************
C*    NAME         :  pthru.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       pthru.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:30
C********************************************************************/
c
c***********************************************************************
c
c   SUBROUTINE:  pthsav (iary,rary,tok2sv,hldsv)
c
c   FUNCTION:  This routine saves 'parser' variables used by the 'parsit'
c              routine.  It is used by routines which look ahead in
c              the command, such as the 'GET/ln1,AS,ln2' command.  It
c              must be called prior to the 'parsit' routine when need it.
c
c   INPUT:  none.
c
c   OUTPUT: iary    I*2  D20 - Array of integer values to save.
c
c           rary    R*8  D10 - Array of real values to save.
c
c           tok2sv  C*64 D1  - Saved 'token2'.
c
c           hldsv   C*8  D1  - Saved 'hldtok'.
c
c***********************************************************************
c
      subroutine pthsav (iary,rary,tok2sv,hldsv)
c
      include 'com8a.com'
c
      common /subcom/ hldtok, iend, incr, icur, nxthld, idelim
      integer*2 iend,incr,icur,nxthld,idelim
      character*64 hldtok
c
      integer*4 iary(20)
c
      real*8 rary(10)
c
      character*64 hldsv
      character*64 tok2sv
c
c...Save PTHRU variables
c
      call svpars
      iary(1) = ityp
      iary(2) = ist
      iary(3) = itv
      iary(4) = ifl(105)
      iary(5) = ifl4(106)
      iary(6) = ifl4(107)
      iary(7) = ifl4(108)
      iary(8) = inx
      iary(9) = ivxsub
      iary(10) = nextyp
      iary(11) = iend
      iary(12) = incr
      iary(13) = icur
      iary(14) = nxthld
      iary(15) = idelim
      iary(16) = ifl(325)
      rary(1)  = tv
      tok2sv  = token2
      hldsv   = hldtok
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pthrst (iary,rary,tok2sv,hldsv)
c
c   FUNCTION:  This routine restores 'parser' variables used by the
c              'parsit' routine.  It is used by routines which look ahead
c              in the command, such as the 'GET/ln1,AS,ln2' command for'thru'
c
c   INPUT:  iary    I*2  D20 - Array of integer values to restore.
c
c           rary    R*8  D10 - Array of real values to restore.
c
c           tok2sv  C*64 D1  - Saved 'token2'.
c
c           hldsv   C*8  D1  - Saved 'hldtok'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pthrst (iary,rary,tok2sv,hldsv)
c
      include 'com8a.com'
c
      common /subcom/ hldtok, iend, incr, icur, nxthld, idelim
      integer*2 iend,incr,icur,nxthld,idelim
      character*64 hldtok
c
      integer*4 iary(20)
c
      real*8 rary(10)
c
      character*64 hldsv
      character*64 tok2sv
c
c...Restore PTHRU variables
c
      call rtpars
      ityp     = iary(1)
      ist      = iary(2)
      itv      = iary(3)
      ifl(105) = iary(4)
      ifl4(106) = iary(5)
      ifl4(107) = iary(6)
      ifl4(108) = iary(7)
      inx      = iary(8)
      ivxsub   = iary(9)
      nextyp   = iary(10)
      iend     = iary(11)
      incr     = iary(12)
      icur     = iary(13)
      nxthld   = iary(14)
      idelim   = iary(15)
      ifl(325) = iary(16)
      tv       = rary(1)
      token2   = tok2sv
      hldtok   = hldsv
c
c...End of routine
c
 8000 return
      end
