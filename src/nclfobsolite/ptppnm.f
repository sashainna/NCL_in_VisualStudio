C*********************************************************************
C*    NAME         :ptppnm.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ptppnm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:31
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptppnm(nam)
C*       Store part program name in NCL common 
C*    PARAMETERS   
C*       INPUT  : 
C*          nam      name of part progam
C*          ncf      Number of chars in 'nam'.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine ptppnm(nam,ncf)

      include 'com4a.com'
c
      integer*4 ncf
c 
      character*(*) nam
 
      ppfnam = nam(1:ncf)
c     ifl(148)=1
 
      return
      end
