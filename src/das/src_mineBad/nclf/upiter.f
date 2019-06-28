
C*********************************************************************
C*    NAME         :  upiter.f
C*       CONTAINS: routine to retrieve tool parameters
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       upiter.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:50
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine upiter(iter)
C*       set number of iterations btween cutter display.
C*    PARAMETERS   
C*       INPUT  : 
C*          iter         number of iterations between cutter display
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine upiter(iter)

      include 'com8a.com'
c
      integer*2 iter
c
      ifl(129) = iter 

      return
      end
