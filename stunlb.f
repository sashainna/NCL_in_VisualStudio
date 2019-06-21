C*********************************************************************
C*    NAME         :stunlb.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       stunlb.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:46
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine stunlb (lfset)
C*       Sets unknown temporary label flag - unklbl, lfl(77)
C*    PARAMETERS   
C*       INPUT  : 
C*          lfset    - =1, set unkbl to true, =0, set unklbl to false
C*       OUTPUT :  
C*          lfset    - =1, unkbl was true, =0, unklbl was false
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine stunlb (lfset)

      include 'com4a.com'

      integer*2 lfset
      integer*2 ihld

c
c...Set return value of lfset based on current value of unklbl
c
      ihld = lfset
      if (unklbl) then
        lfset = 1
      else
        lfset = 0
      endif
c
c...Set unklbl from value of lfset
c
      if (ihld.eq.0) then
        unklbl=.false.
      else
        unklbl=.true.
      endif
 
99    return
      end
