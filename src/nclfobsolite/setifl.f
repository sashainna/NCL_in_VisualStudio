C*********************************************************************
C*    NAME         :  setifl.f
C*       CONTAINS:
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       setifl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:40
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setifl
c*          C callable routine to set an ifl
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setifl(idx,ival)

      include 'com4a.com'

      integer*2 idx,ival

      ifl(idx)=ival

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setjfl
c*          C callable routine to set an i*4 ifl
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setjfl(idx,ival)

      include 'com4a.com'

      integer*2 idx
      integer*4 ival
c
      ifl4(idx) = ival

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setlfl
c*          C callable routine to set an lfl (logical flag).
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setlfl(idx,ival)

      include 'com4a.com'

      integer*2 idx
      logical*2 ival
c
      lfl(idx) = ival

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setscv
c*          C callable routine to set an sc value
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setscv(idx,val)

      include 'com4a.com'

      integer*2 idx
c
      real*8 val

      sc(idx)=val

      return
      end
