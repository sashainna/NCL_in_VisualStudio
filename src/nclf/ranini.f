C*********************************************************************
C*    NAME         :  ranini.f
C*       CONTAINS: initialize RANFIL
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ranini.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:34
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ranini
c*       intializes the frst 40 records of the ranfil
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
      subroutine ranini

      include 'com8a.com'

      do 100 i = 1, 143
100       jb(i) = 0

c          init recs 1-40 ranfil for init vst setting
      do 200 i = 1, 100
200       call putran(jb, i)
c
c.....Reset ifl(66) so it points to the start of the now reset
c.....ranfile - ASF 1/9/14
c
      ifl(66) = 0
      call vxlfin
c
c....vxlfin delete all name list, but we still need initialize the
c....list, otherwise, the pointer is all NULL and will get the error
c....Yurong 6/23/05
c
      call vxlini
      return
      end
