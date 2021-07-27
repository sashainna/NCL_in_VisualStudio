C*********************************************************************
C*    NAME         :  clput.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       clput.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:43
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine clput (ipg,buff)
C*     write to the cl file.  it writes the      
C*     of the buff rry to the record number contained in ipg.  
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine clput (ipg,buff)

      include 'com4a.com'
      include 'mocom.com'

      integer*4 ipg
      real*8 buff(36)

c                                ipg + 1    for rsx-11m
cuni      write(2,rec=ipg+1) buff
      write(cllun,rec=ipg+1) buff

      return
      end
