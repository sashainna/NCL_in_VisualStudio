C*********************************************************************
C*    NAME         :  setstp.f
C*       CONTAINS:
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       setstp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:40
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setstp
c*          C callable routine to set the stpflg for *stop command.
C*       INPUT  : 
C*          ival -   0 = set inch values
c*                   1 = set mm values
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setstp(ival)

      include 'com4a.com'

      integer*2 ival

      if (ival .eq. 0) then
		  stpflg = .false.
      else
		  stpflg = .true.
      endif

      return
      end
