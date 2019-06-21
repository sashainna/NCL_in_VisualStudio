C
C*********************************************************************
C*    NAME         :  renam.f
C*       CONTAINS:
C*    COPYRIGHT 1998 (c) Numerical Control Computer Sciences.
C*       All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       renam.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:37
C********************************************************************/
C*********************************************************************
C*		NAME		:renam.f
C*
C*
C*    This subroutine allows for the command rename, it takes
C*    the element and gives it the new specified name.
C*    For example, given the command   
C*    rename/sf1,sfA,...,sfn, sfAn 
C*    it will rename sf1 with sfA and so on.
C*
C*********************************************************************
C
C

      subroutine renam

      include 'com4a.com'
      include 'comdb.com'

C
C...Declarations
C...Declare arrays, ones for the old name and new name,
C...declare integers old subscript, new subscript, and 
C...new key.
C
      character*64 oldlab, newlab   
      integer*2 ierr
      integer*4 olscpt
      integer*2 nwds, ietype
      integer*4 nwscpt
      integer*4 nclkey
c
      real*8 origin(3)
c
c...Get the 'old' name
c
   10 call parsit
      if (ityp .eq. 2 .and. ist .eq. 1) then
          call ub_symbol_name (token2,ivxsub,nclkey,origin,i)
          if (i .eq. 1) then
              ist = 31
              call ptdesc (nclkey,ist,tv)
          else if (i .eq. 2) then
              ist = 32
              call ptdesc (nclkey,ist,tv)
          endif
      endif
c        
      if (ityp .ne. 2 .or. ist .eq. 1 .or. ist.eq.TEXTVAR) then
          call error (9)
          go to 8000
      endif
      call gtdesc(tv, nclkey, nwds, ietype)
      oldlab = token2
      olscpt = ivxsub
c
c...Ignore the comma
c
      if (nextyp .eq. 11) go to 9000
      call parsit
      if (ityp .ne. 5 .or. ist .ne. 9) then
          call error (57)
          go to 8000
      endif
c
c...Get the 'new' name
c
      if (nextyp .eq. 11) go to 9000
      call parsit
      if (ityp .ne. 2) go to 9000
cc      if (ist .ne. 1) then
cc          call error (8)
cc          go to 8000
cc      endif
      newlab = token2
      nwscpt = ivxsub
c
c...Rename the geometry
c
      if (ietype .eq. 31 .or. ietype .eq. 32) then
          call ub_symbol_rename (nclkey,newlab,nwscpt,ierr)
          if (ierr .ne. 0) ierr = 316
      else
          call ncl_rename_geom (nclkey,oldlab,olscpt,newlab,nwscpt,ierr)
      endif
c
c...Geometry already exists
c
      if (ierr .eq. 316) then
          call error (8)
          go to 8000
c
c...Geometry is of different type
c
      else if (ierr .eq. 318) then
          call error (89)
          go to 8000
      endif
c
c...Loop again
c...Ignoring the comma
c
      if (nextyp .ne. 11) then
          call parsit
          if (ityp .ne. 5 .or. ist .ne. 9) then
              call error (57)
              go to 8000
          endif
          go to 10
      endif
c
c...End of routine
c
 8000 return
c
c...End of statment reached
c
 9000 call error (82)
      go to 8000
      end
