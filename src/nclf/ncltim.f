 
C*********************************************************************
C*    NAME         :  ncltim.f
C*       CONTAINS:
C*    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       ncltim.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:34
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ncltim
c*       purpose of subroutine: display elapsed time
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
      subroutine ncltim
c
      include 'com8a.com'
      integer*4 isec
      real*4 qtime,qtime2
c
c     added for time function on the sun.
c
      common /ntmcom/it,qtime2
      integer*4  it2(3),it(3)
      character*12 rc(3)
c...we set default -i2 for fortran compile
c...so value pass in DATE_AND_TIME must be i2
c...Yurong
c
c...    integer*4 vals(8)
      integer*2 vals(8)
c
c
c...get elapsed time
c
c
c...use DATE_AND_TIME for WNT to avoid using port library
c
      CALL DATE_AND_TIME (rc(1),rc(2),rc(3),vals)
      it2(1) = vals(5)
      it2(2) = vals(6)
      it2(3) = vals(7)
      qtime = (it2(1)*60 + it2(2))*60 + it2(3)
      isec = qtime - qtime2
      qtime2 =qtime
      imin  = 0
      ihr   = 0
210   if (isec .lt. 60) go to 220
      isec   = isec   - 60
      imin   = imin   + 1
      go to 210
 
220   if (imin .lt. 60) go to 230
      imin   = imin   - 60
      ihr  = ihr  + 1
      go to 220
 
230   write (cout,270) ihr,imin,isec
270   format ('elapsed time since last *time = ',i2,' hrs ',i2,
     x' mins ',i2,' secs')
      call putmsg (cout,80,15,0)
      qtime = (it(1)*60 + it(2))*60 + it(3)
	isec = qtime2 - qtime
      imin  = 0
      ihr   = 0
240   if (isec .lt. 60) go to 250
      isec   = isec   - 60
      imin   = imin   + 1
      go to 240
 
250   if (imin .lt. 60) go to 260
      imin   = imin   - 60
      ihr  = ihr  + 1
      go to 250
 
260   write (cout,280) ihr,imin,isec
280   format ('total elapsed time this run   = ',i2,' hrs ',i2,
     x' mins ',i2,' secs')
      call putmsg (cout,80,16,0)
 
c
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (.not. ksr .and. ifl(35) .eq. 0) then
c
      if (.not. ksr .and.( ifl(35) .eq. 0 .or. ifl(35) .eq. 2)) then
      call clreos
c         cout = ' '
c          do 290 i=17,24,1
c             call putmsg (cout,80,i,0)
c290       continue
      endif
      return
      end
 
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclf_get_etime(etime, nc)
c*       purpose of subroutine: return elapsed time in 'etime' string
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          etime: the elapsed time writing in a string
C*                  the length of etime
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine nclf_get_etime(etime, nc)
c
      include 'com8a.com'
      character*100 etime
      integer*4 nc
      integer*4 isec, strlen1
      real*4 qtime,qtime3
      common /ntmcom/it
      integer*4  it3(3),it(3)
      character*12 rc(3)
      integer*2 vals(8)
c
c
c...get elapsed time
c
      CALL DATE_AND_TIME (rc(1),rc(2),rc(3),vals)
      it3(1) = vals(5)
      it3(2) = vals(6)
      it3(3) = vals(7)
      qtime3 = (it3(1)*60 + it3(2))*60 + it3(3)
      imin  = 0
      ihr   = 0
      qtime = (it(1)*60 + it(2))*60 + it(3)
	isec = qtime3 - qtime
240   if (isec .lt. 60) go to 250
      isec   = isec   - 60
      imin   = imin   + 1
      go to 240
 
250   if (imin .lt. 60) go to 260
      imin   = imin   - 60
      ihr  = ihr  + 1
      go to 250
 
260   write (etime,270) ihr,imin,isec
270   format (i2,' hrs ',i2,' mins ',i2,' secs')
      nc = strlen1(etime)
      return
      end
