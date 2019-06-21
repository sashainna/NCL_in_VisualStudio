c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       retlbl.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:37
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1988 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: retlbl                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Return the display label flags for pts, lns, cis & cv.
C          Contains:
C              ptlbl(ival)
C              lnlbl(ival)
C              cilbl(ival)
C              cvlbl(ival)
C              sflbl(ival)
C **********************************************************************
C **********************************************************************
 
      subroutine ptlbl(ival)
 
      include 'com4a.com'
 
      integer*2 ival

      ival = 0
      if (lablpt) ival = 1

99    return
      end
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      subroutine lnlbl(ival)
 
      include 'com4a.com'
 
      integer*2 ival

      ival = 0
      if (lablln) ival = 1

99    return
      end
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      subroutine cilbl(ival)
 
      include 'com4a.com'
 
      integer*2 ival

      ival = 0
      if (lablci) ival = 1

99    return
      end
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      subroutine cvlbl(ival)
 
      include 'com4a.com'
 
      integer*2 ival

      ival = 0
      if (lablcv) ival = 1

99    return
      end
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      subroutine sflbl(ival)
 
      include 'com4a.com'
 
      integer*2 ival

      ival = 0
      if (lablsf) ival = 1

99    return
      end
