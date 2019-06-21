C********************************************************************/
C*    NAME         :  mdinit.f
C*       CONTAINS:
C*					mdinit
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       mdinit.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:16
C********************************************************************/
C
c***********************************************************************
c
c   SUBROUTINE:  mdinit
c
c   FUNCTION:  This routine intializes the cutter display and motion
c              display variables.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mdinit
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*2 nc
c
      character*80 ltl
c
      data ltl /'NCL_TOOLIB'/
c
c...Initialize cutter display
c
      call nclf_get_tool_symlib(cutlib)
      call gtenv(ltl,toolib,nc)
      cutsym(1) = ' '
      cutsym(2) = ' '
      cutsym(3) = ' '
      icutfl(1) = 0
      icutfl(2) = 0
      icutfl(3) = 1
      icutfl(4) = 0
      icutfl(5) = 0
      icutfl(6) = 0
      icutfl(7) = 0
      icutfl(8) = 0
C
      do 100 i=1,20,1
          dcutr(i) = 0
  100 continue
      dcutr(18) = 1
cc      call rscsym
c
c...Initialize motion playback
c
      call rsmply
      itrafl = 0
      lcumov = .false.
c
c...Initialize the Tool Library
c
      ictent = 0
      ictsav = 0
c
c...Initialize clfile variables
c
      do 500 i=1,10,1
          iciprm(i) = 0
          rciprm(i) = 0.
  500 continue
      cycret = 2
      fityp  = 1
      cfeed(1) = 0.
      cfeed(2) = 0.
      crpm   = 0.
      pstnum = 0.
c
c...End of routine
c
 8000 return
      end
