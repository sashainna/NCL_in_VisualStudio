C*********************************************************************
C*    NAME         :curnam.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       curnam.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:46
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine curnam(name, isubsr)
C*       Return current name and subscript in common.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine curnam (name, isubsr)

      include 'com4a.com'

      character*64 name
      integer*4 isubsr

c... check for (internal) unknown label logic
      if (unklbl) then
        name = '@UN'
        isubsr = 0
        goto 99
      endif
 
      name = savid2
      isubsr = isvsub
      if (ldtflg) then
        isubsr = -isubsr-1
        ldtflg = .false.
      endif

99    return
      end
