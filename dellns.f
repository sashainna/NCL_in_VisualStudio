C*********************************************************************
C*    NAME         :  dellns.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dellns.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:55
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dellns(istart, istop)
C*      this routine deletes lines from a part program file
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
      subroutine dellns(istart, istop)
c
      include 'com4a.com'
c
      integer*4 istart,istop
c
      integer*4 inrec,nclf_delsrc,ierr,nlin
c
c...Delete the lines
c
      ierr = nclf_delsrc (istart-1,istop-1)
      if (ierr .eq. 0) then
          nlin = istop - istart + 1
          ifl4(1) = ifl4(1) - nlin
      endif
c
c...Fix Label and Macro pointers
c
cc      inrec = istop - istop + 1
cc      call fixpp (-inrec,istart)

99999 return
      end
