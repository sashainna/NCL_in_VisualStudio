C*********************************************************************
C*    NAME         :gpgmnm.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       gpgmnm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:09
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gpgmnm(pgmname)
C*       Return part program name
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine gpgmnm(pgmname,knc)

      include 'com4a.com'
 
      integer*4 knc,strlen1
      character*(MAX_FILE) pgmname
 
      pgmname = defnam
      knc = strlen1(pgmname)
 
      return
      end
