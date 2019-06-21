C*********************************************************************
C*    NAME         :  sfpkg.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sfpkgy.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sfpkg (lok)
c*       allows the use of the surface package 
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine sfpkg (lok)

      include 'com8a.com'

      logical lok

      lok = .true.

99999 return
      end
