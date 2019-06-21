C*********************************************************************
C*    NAME         :  rsttrm.f
C*       CONTAINS:
C*                rsttrm
C*                rstint
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       rsttrm.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:35
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rsttrm
c*       This routine sets flags that the operator has
c*       the execution of statements and motion generation should be
c*       stopped.
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
      subroutine rsttrm
 
      include 'com8a.com'
 
      pmode=1
      ifl(78)=1
      ifl(86)=1
      ifl(37)=1
 
c=VMS
c      call unsol
c=ALL
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rstint
c*       This routine resets flags after an interrupt was received
c*       and handled.  See interface/irp.c:uz_zplayback() on SUN.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine rstint
 
      include 'com8a.com'
 
      ifl(78)=0
      ifl(86)=0
 
      return
      end
