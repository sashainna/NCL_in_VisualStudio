C*********************************************************************
C*    NAME         :randlk.f
C*       CONTAINS:
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
c*     MODULE NAME AND RELEASE LEVEL
c*       randlk.f , 25.1
c*    DATE AND TIME OF LAST MODIFICATION
c*       04/29/15 , 15:10:34
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine randel(nclkey,is)
C*       This routine deletes an entry from the ranfil hash table using key.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey -    key of entity to delete from ranfil hash table
C*       OUTPUT :  
C*          is -     0 if name was found and deleted
c*                   1 if name not found
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine randlk (nclkey, ncltype, is)

      include 'com8a.com'

      integer*4 nclkey
      integer*2 is, ncltype

      call vxdlk (nclkey, ncltype, is)

999   return
      end
