c
c***********************************************************************
c
c   FILE NAME: license_nt.f
C			This file only for winNT
c   CONTAINS:
c                  savinput(buf, parm, flag)
c                  license_ntinit()
c                  auth_ntinit()
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        license_nt.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:08
c
c***********************************************************************

c***********************************************************************
c
c   SUBROUTINE: savinput(buf, flag)
c
c   FUNCTION: save the user input value into global value
c
c   INPUT:  buf: value to be saved
c 			flag: 1: store for add&delete
c 				  2: store for search	
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine savinput(buf, parm, flag)
c
      byte buf(132, 10)
      integer*4 parm(10)
      integer*4 flag
      include "menu.inc"

      integer*4 i,nc, fnum
c
      if (flag .eq. 1) then
c
c...save for add & delete
c
          do 100 i=1, FRMREC, 1
              call pwdbtc (buf(1,i), FRMBUF(i), nc)
  100     continue
      else
c
c...save for search
c
          do 200 i=1, FRMREC, 1
              call pwdbtc (buf(1,i), FRMBUF(i+20), nc)
              MLEVL(i) = parm(i)
  200     continue
      endif

      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE: license_ntinit()
c
c   FUNCTION: initialize nccs_license on WinNT
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine license_ntinit(msg, ierr)
c
      character*80 msg
      integer*4 ierr

      ierr = 0
      call init
      call autini
      call opndat (msg,ierr)
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE: auth_ntinit()
c
c   FUNCTION: initialize nccs_auth on WinNT
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine auth_ntinit(msg, ierr)
c
      character*256 msg
      integer*4 ierr

      ierr = 0
      call init
      call na_autini
      call opndat (1, msg,ierr)
      call clrrec()
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE: onfexit()
c
c   FUNCTION: handle exit routine
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine onfexit()
c
      include "menu.inc"
      call clsfil (LUNSC1)
      return
      end
C WNT-END

c***********************************************************************
c
c   SUBROUTINE: onfsuper()
c
c   FUNCTION: setup for super user
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
      subroutine onfsuper(msg, nc, ierr)
c
      include "menu.inc"
      character*256 msg
      integer*4 ierr,nc, strlen1
c
      NLEVL  = 2
      FRMREC = 10
      ierr = 0
c
c......Open data base for read/write
c
      call clsfil (LUNSC1)
      call opndat (3,msg,ierr)
      if (ierr .ne. 0) goto 9000
c
c......Open print control file
c......LSP.COM
c
c...      call opnprt (msg,ierr)
c...      if (ierr .ne. 0) goto 9000
c
c......Redisplay form
c
      call showsuper
      call disfld (9)
      call disfld (10)
      return
 9000 nc = strlen1(msg)
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE: getsparm(buf, parm)
c
c   FUNCTION: get the last search parameters value
c
c   INPUT:  buf: parameter value (string)
c 		parm: parameter flag
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine getsparm(buf, parm)
c
      byte buf(132, 10)
      integer*4 parm(10)
      include "menu.inc"

      integer*4 i,nc, fnum
c
      do 200 i=1, 10, 1
          call pwdctb (FRMBUF(i+20), buf(1,i))
          parm(i) = MLEVL(i)
  200 continue

      return
      end
c***********************************************************************
c
c   SUBROUTINE: savsparm(parm, buf, item)
c
c   FUNCTION: save the search  parameter
c
c   INPUT:  parm: value to be saved
c			buf: string value to be saved
c 		  item: search item number
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine savsparm(parm, buf, item)
c
      byte buf(132)
      integer*4 parm
      integer*4 item
      include "menu.inc"

      integer*4 i,nc, fnum

      if (parm.eq.0) then
          MLEVL(item) = 0
      else
          MLEVL(item) = 1
          call pwdbtc (buf, FRMBUF(item+20), nc)
      endif
      return
      end
c***********************************************************************
c
c   SUBROUTINE: checkpass(err)
c
c   FUNCTION: check if password match
c
c   INPUT:  err: !=0 not match
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine checkpass(ierr)
c
      integer*4 ierr
      include "menu.inc"

      call pwdpas (FRMBUF,SAPNC,FRMBUF(51),ierr)
      if (ierr .eq. 0) then
          if (FRMBUF(51) .ne. FRMBUF(9)) ierr = 9
      endif
      if (ierr .ne. 0) then
          call messag (ierr)
          return
      endif
      return
      end
