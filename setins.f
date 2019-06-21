C*********************************************************************
C*    NAME         :  setins.f
C*       CONTAINS:  setins   getins  setinp   getinp
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       setins.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:40
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setins
c*          C callable routine to set INSERT mode
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setins(ivl1,ivl2)

      include 'com4a.com'

      integer*2 ivl1,ivl2

      ifl(37)=ivl1
      ifl(25)=ivl2

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getins
c*          C callable routine to get the active INSERT mode
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getins(ivl1,ivl2)

      include 'com4a.com'

      integer*2 ivl1,ivl2

      ivl1 = ifl(37)
      ivl2 = ifl(25)

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setinp
c*          C callable routine to get the active INSERT mode
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setinp(ivl1,ivl2)

      include 'com4a.com'

      integer*2 ivl1,ivl2

      ifl(37) = ivl1
      ifl(150) = ivl2

      return
      end  
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getinp
c*          C callable routine to get ifl values for input mode flags
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getinp(ivl1,ivl2)

      include 'com4a.com'

      integer*2 ivl1,ivl2

      ivl1 = ifl(37)
      ivl2 = ifl(150)

      return
      end      
