c
c***********************************************************************
c
c   FILE NAME:  support
c   CONTAINS:
c               preadj  prtout  pstsum
c
c   FUNCTION:   Contains dummy routines for calls when standalone PMACRO
c               is built instead of DECADE.
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       suppmac.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:58:14
c
c***********************************************************************
c
c...PREADJ
c
      subroutine preadj (glin,gout,gtvi,gtvo)
c
      real*8 glin(3),gout(3),gtvi(3),gtvo(3)
c
      gout(1) = glin(1)
      gout(2) = glin(2)
      gout(3) = glin(3)
      gtvo(1) = gtvi(1)
      gtvo(2) = gtvi(2)
      gtvo(3) = gtvi(3)
c
      return
      end
c
c...PRTOUT
c
      subroutine prtout (cdat,knc)
c
      integer*4 knc
c
      character*(*) cdat
c
      return
      end
c
c...PSTSUM
c
      subroutine pstsum
c
      return
      end
