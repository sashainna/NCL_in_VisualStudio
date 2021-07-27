C*********************************************************************
C*    NAME         :  gtmx.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*       gtmx.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:10
c*********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtmx()
C*       purpose of subroutine: to parse all commands not handled in the 
C*       driver.
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
      subroutine gtmx(cbuf,rsorig,rszvec,rsyvec,ierr)

      include 'com8a.com'

      real*8 rsorig(3), rszvec(3), rsyvec(3)
      character*6 c6
      character*16 cbuf
	  integer*2  nwds, ietype
	  integer*4  nclkey
	  real*8     tmpmx(12)
	  equivalence (cin,c6)
	  logical     ierr

      call convrt(cbuf, c6, 6)
      inx=1
      call parsit
c                                               ***  matrix
      if (ityp.eq.2.and.ist.eq.10) then
           call gtdesc(tv,nclkey,nwds,ietype)
		   call gtgeo(nclkey,tmpmx)

          rsorig(1) = tmpmx(4)
          rsorig(2) = tmpmx(8)
          rsorig(3) = tmpmx(12)

          rszvec(1) = tmpmx(3)
          rszvec(2) = tmpmx(7)
          rszvec(3) = tmpmx(11)

          rsyvec(1) = tmpmx(2)
          rsyvec(2) = tmpmx(6)
          rsyvec(3) = tmpmx(10)
      else
          call error (94)
		  ierr=.true.
		  goto 99999
      endif

99999 return
      end
