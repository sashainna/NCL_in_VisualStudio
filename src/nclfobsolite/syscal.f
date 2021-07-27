C*********************************************************************
C*    NAME         :  syscal.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       syscal.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:46
C********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine syscal
C*        Calls an OpenNCL user defined routine.
C*                                                                    
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
      subroutine syscal

      include 'com8a.com'
      include 'status.com'

c
      integer*2 narg,ierr
      integer*4 nc,strlen1
c
      byte larg(64,35)
      character*64 lrtn,carg(35)
      equivalence (carg,larg)
c
c...Next character must be a slash (/)
c
      if (nextyp.ne.5) then
          ierr   = 22
          go to 9000
      endif
c
c...Get name of Open/NCL routine
c
      call parsit
      lrtn   = token2
c
c...Verify that this routine exists
c
      call sysvfy (lrtn,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Get argument list
c
      narg   = 0
  100 if (nxteos) go to 200
      call parsit
cc      if (ityp .ne. 5 .or. ist .ne. 9) then
cc          ierr   = 297
cc          go to 9000
cc      endif
      call parsit
      narg   = narg   + 1
      carg(narg) = token2
C*   IJD - Handle subscripts.
      if (ityp.eq.2 .and. ivxsub.gt.0) then
        nc = strlen1(token2)
        write (carg(narg)(nc+1:64),1000) ivxsub
1000    format ('(',i5,')')
      endif
      larg(64,narg) = 0
      go to 100
c
c...Call Open/NCL routine
c
  200 call ylanch (lrtn,narg,larg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...End of routine
c
 8000 return
c
c...Error trying to call Open/NCL routine
c
 9000 call error (ierr)
      go to 8000
      end
