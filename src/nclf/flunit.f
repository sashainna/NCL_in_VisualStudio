
C*********************************************************************
C*    NAME         :  flunit.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       flunit.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:04
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     :
C*       description
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
      subroutine flunit()
c
      include 'com8a.com'
c
c          file luno assignments
      pplun=1
      cllun=2
      pltlun=3
      lun125=3
      aslun=4
c     roclun=4       rockwell surface input file unit
      cinlun=5
      conlun=6
      hlplun=7
      quelun=7
      errlun=8
      scrlun=12
      voclun=8
      prtlun=9
      gsklun=9
      ranlun=10
      ppslun=11
      dblun=11
      iscrln(1) = 13
      iscrln(2) = 14
      iscrln(3) = 15
      iscrln(4) = 16
      templun=17
      return
      end
