C*********************************************************************
C*    NAME         :  putdb.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putdb.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:32
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putdb (buf,ipg)
c*       This routine writes a record to a data base file.   
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
      subroutine putdb (buf,ipg)

      include 'com4a.com'
      include 'comdb.com'

      real*8 buf(36)

      if (cdbio.ne.wdbio) go to 99999

c      write (cdbio,rec=ipg+1) buf
      write (unit=dblun, rec=ipg+1) buf

99999 return
      end
