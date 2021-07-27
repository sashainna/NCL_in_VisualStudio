C*********************************************************************
C*    NAME         :  dnarrw.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dnarrw.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:56
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dnarrw
c*          to update nline if necessary when user hits down arrow
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine dnarrw

      include 'com4a.com'

      if (nline.le.ifl4(1)) nline=nline+1

c         Update status line in error message area.
      call statln
c
c...if it is at INSERT mode, save the insert line
c
      if (IFL(37)==2) call set_insline(nline)
      return
      end
