C*********************************************************************
C*    NAME         :  flshbf.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       flshbf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:04
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine flshbf
C*       flush the plotting buffer by calling
C*       the appropriate plotting routine.
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
      subroutine flshbf

      include 'com8a.com'

C      integer*2 ipb,maxipb
C      equivalence (ifl(185),ipb),(ifl(186),maxipb)
C
C      if (ipb.gt.0) then
C        if (ifl(127).eq.0) then
C          call pltwrt
C        else if (ifl(127).eq.1) then
C          call wrt125
C        else if (ifl(127).gt.1.and.ifl(127).lt.7) then
C          call wrttek
C        endif
C      endif

99999 return
      end
