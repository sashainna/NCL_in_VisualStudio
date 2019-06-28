C*********************************************************************
C*    NAME         :  getdb.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       getdb.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:07
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getdb (buf,ipg)
C*       read a record from a data base file.
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
      subroutine getdb (buf,ipg)

      include 'com4a.com'
      include 'comdb.com'

      real*8      buf(36),buf1
      character*8 cbuf
      equivalence (buf1,cbuf)

c      read (cdbio,rec=ipg+1) buf
      read (unit=dblun, rec=ipg+1, err= 9999) buf
      go to 100
c
 9999 cbuf = ' '
      buf(1) = buf1
c
  100 return
      end
