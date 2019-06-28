C*********************************************************************
C*    NAME         :randel.f
C*       CONTAINS:
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       randel.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:33
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine randel(name,isub,is)
C*       This routine deletes an entry from the ranfil hash tabel.
C*    PARAMETERS   
C*       INPUT  : 
C*          name - character*6
c*                   name of entity to delete from ranfil hash table
c*          isub - integer*2
c*                   subscript of entity name (0 if not subscripted) 
C*       OUTPUT :  
C*          is - integer*3
c*                   0 if name was found and deleted
c*                   1 if name not found
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine randel (name, isub, is)

      include 'com8a.com'

      character*64 name
      integer*4 isub
      integer*2 is
      real*8 vs(35)
      character*8 cvs(35)
      equivalence (vs, cvs, jb)

      is = 0
      call vxdel (name, isub, is)

      return
      end
