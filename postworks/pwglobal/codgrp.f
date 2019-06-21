c
c***********************************************************************
c
c   FILE NAME: codgrp.for
c   CONTAINS:
c               gcdgrp  mcdgrp
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        codgrp.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  gcdgrp (kreg,gval)
c
c   FUNCTION:  This routine gets the actual register of a G-code de-
c              pending on which group it belongs to.  If the G-code does
c              not belong to any group, then the G0 register will be
c              used.
c
c   INPUT:  gval    R*8  D1  -  G-code value.
c
c   OUTPUT: kreg    I*4  D1  -  Actual register for G-code.
c
c***********************************************************************
c
      subroutine gcdgrp (kreg,gval)
c
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003)), (GRANGE,POSMAP(0801))
c
      real*8 DUMMY,GRANGE(14,11)
c
      integer*4 kreg
c
      real*8 gval
c
      integer*4 i,j
c
c...Find the G-code group
c...the input value belongs to
c
      do 200 i=1,11,1
          do 100 j=1,14,1
              if (GRANGE(j,i) .eq. DUMMY) go to 200
              if (gval .eq. GRANGE(j,i)) go to 300
  100     continue
  200 continue
      i     = 1
c
  300 kreg  = i      + 17
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mcdgrp (kreg,gval)
c
c   FUNCTION:  This routine gets the actual register of a M-code de-
c              pending on which group it belongs to.  If the M-code does
c              not belong to any group, then the M0 register will be
c              used.
c
c   INPUT:  gval    R*8  D1  -  M-code value.
c
c   OUTPUT: kreg    I*4  D1  -  Actual register for M-code.
c
c***********************************************************************
c
      subroutine mcdgrp (kreg,gval)
c
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003)), (MRANGE,POSMAP(0955))
c
      real*8 DUMMY,MRANGE(7,11)
c
      integer*4 kreg
c
      real*8 gval
c
      integer*4 i,j
c
c...Find the M-code group
c...the input value belongs to
c
      do 200 i=1,11,1
          do 100 j=1,7,1
              if (MRANGE(j,i) .eq. DUMMY) go to 200
              if (gval .eq. MRANGE(j,i)) go to 300
  100     continue
  200 continue
      i     = 1
c
  300 kreg  = i      + 36
c
c...End of routine
c
 8000 return
      end
