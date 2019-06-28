c*********************************************************************
C*    NAME         :  tanorm.f
C*       CONTAINS:
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sciences
C*       All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
C*       tanorm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:47
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : integer*2 function tanorm(imode)
C*
C*    PURPOSE OF PROGRAM: Determine if tool axis mode is reliant upon
C*                        part surface (TA_NORMAL_PS, TA_TANTO_DS,
C*                        TA_TANTO_DS_PERPTO, or TA_NORMAL_PS_PERPTO.
C*
C*    PARAMETERS
C*       INPUT  :
C*          imode  -  Current tool axis mode.
C*       OUTPUT :
C*          none
C*    RETURNS      : 1 if tool axis is dependant on part surface.
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      integer*2 function tanorm(imode)
c
      include 'com.com'
c
      integer*2 imode
c
c...Check to see if the tool axis is
c...reliant upon the part surface normal
c
      tanorm = 0
      if (imode .eq. TA_NORMAL_PS .or.
     2    imode .eq. TA_TANTO_DS .or.
     3    imode .eq. TA_TANTO_DS_PERPTO .or.
     4    imode .eq. TA_NORMAL_PS_PERPTO) tanorm = 1
cc     1    imode .eq. TA_ATANGL_PS .or.
cc     5    imode .eq. TA_ATANGL_PS_PERPTO .or.
cc     6    imode .eq. TA_ATANGL_PS_DIST .or.
cc     7    imode .eq. TA_ATANGL_PS_DIST_PERPTO
c
      return
      end
