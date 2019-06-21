C*********************************************************************
C*    NAME         :  getnln.f
C*       CONTAINS:
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       getnln.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:07
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getnln
c*          C callable routine to get an value of nline
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          ival   -  value of nline
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getnln(ival)

      include 'com4a.com'

      integer*4 ival

      ival=nline

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setnln
c*          C callable routine to set an value of nline
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          ival   -  value of nline
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setnln(ival)

      include 'com4a.com'

      integer*4 ival

      nline=ival
      if (nline .gt. ifl4(1)+1) nline = ifl4(1)+1
      return
      end
