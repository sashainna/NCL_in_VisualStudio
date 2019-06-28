
C***********************************************************************
C*    NAME         :  uvend.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       uvend.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:52
C***********************************************************************
C
c **********************************************************************
c **********************************************************************
c **  subroutine name: uvend
c **
c **  last revision:
c **  purpose of subroutine: this routine handles endup when driving to
c **      the edge of a surface.
c **  input -
c **    itsk = 1 - positioning, 2 - generating points 
c **    rdp  = size of step just made
c **  output -
c **    kret = 1 - use the last calculated point
c **         = 0 - don't use the last calculated point
c **    rdp  = new step size
c **
c **********************************************************************
c **********************************************************************
 
      subroutine uvend (itsk,kret,rdp)
 
      include 'com4a.com'

      integer*2 itsk,kret
      real*4 rdp

      if (itsk.eq.1) then    
        call uvend1(kret,rdp)
      else
        call uvend2(kret,rdp)
      endif


      return
      end
