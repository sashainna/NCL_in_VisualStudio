C*********************************************************************
C*    NAME         :  putapt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putapt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:32
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putapt (lbuf,nc)
c*       performs the actual i/o to the apt source file **
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
      subroutine putapt (lbuf,nc)

      include 'com8a.com'

      character*1 lbuf(80)

      if (nc.eq.0) return
      nc1 = nc
      if (nc1 .gt. 80) nc1 = 80
c
c...VX
c
      if (ifl(35) .eq. 2 .and. ifl(322) .eq. 1) then
          call vx_putapt(lbuf,nc1)
      else
          write (aslun,10) (lbuf(i),i=1,nc1)
   10     format (80a1)
      endif
      nc=0

      return
      end
