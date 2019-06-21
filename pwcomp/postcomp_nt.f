c
c***********************************************************************
c
c   FILE NAME: postcomp_nt.f
C        This file only for WinNT
c   CONTAINS:
c              pc_saveopt
c              pc_reinit
c              pc_getoptions
c              pc_saveoptions
c              pc_putpfile
c              pcomp_ntinit
c              getopcc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        postcomp_nt.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:14
c
c***********************************************************************
c**********************************************************************
c   Subroutine      pc_reinit
c
c        reinitialize postworks data, copy the save data back
c	   to option data( because we need keep option data is
c	   same as before running
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pc_reinit

      include "postworks_nt.inc"
      include "menu.inc"
      include "pregen.inc"
      character*80 msg
      integer*4 ierr, i

      call init
      call pstini
      call getopc (msg,ierr)

      i = 1
      do while(i.ne.6)
          IOPFL(i) = SIOPFL(i)
          LOPFL(i) = SLOPFL(i)
      i = i + 1
      enddo

      return
      end
C WNT-END

c**********************************************************************
c   Subroutine         pc_saveopt                                         *
c            save option data into tmp variables
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pc_saveopt

      include "postworks_nt.inc"
      include "menu.inc"
      include "pregen.inc"
      integer*4  i
      i = 1
      do while(i.ne.6)
          SIOPFL(i) = IOPFL(i)
          SLOPFL(i) = LOPFL(i)
          i = i + 1
      enddo

      return
      end



C WNT-END
c**********************************************************************
c   Subroutine    pcomp_ntinit                                  *
c        initialize Pwmacro on WInNT
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pcomp_ntinit

      include "pregen.inc"

      call pcomp_version
      call init
      call pstini
      PREPT  = 0
      MEMPT(2) = 0

      return
      end

C WNT-END
c***********************************************************************
c
c   SUBROUTINE: pc_getoptions(iopt, copt)
c
c   FUNCTION: get the option values
c
c   INPUT:  none.
c
c   OUTPUT: iopt: integer option values
c			copt" string option values
c
c***********************************************************************
C WNT-START
c
      subroutine pc_getoptions(iopt, copt)
c
      byte copt(256, 5)
      integer*4 iopt(5)
      include "menu.inc"
      include "pregen.inc"

      integer*4 i
c
      do 100 i=1, 5, 1
          iopt(i) = IOPFL(i)
  100 continue

      do 300 i=1, 5, 1
          call pwdctb (LOPFL(i), copt(1,i))
  300 continue

      return
      end
C WNT-END

c***********************************************************************
c
c   SUBROUTINE: pc_saveoptions(iopt, copt)
c
c   FUNCTION: save the option values
c
c   INPUT:  iopt: integer option values
c			copt: string option values
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine pc_saveoptions(iopt, copt)
c
      byte copt(256, 4)
      integer*4 iopt(4)
      include "menu.inc"
      include "pregen.inc"

      integer*4 i,nc
c
      do 100 i=1, 5, 1
          IOPFL(i) = iopt(i)
  100 continue

      do 300 i=1, 5, 1
          call pwdbtc (copt(1,i), LOPFL(i), nc)
  300 continue

      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE: pc_putpfile(filename)
c
c   FUNCTION: save Postcomp file name into global value
c
c   INPUT:  filename: Postcomp filename
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine pc_putpfile(filename)
c
      include "menu.inc"
c
      byte filename(MAX_PATH)
      integer*4 nc

      call pwdbtc (filename, LCMPFI, nc)
      NCCMPF = nc
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE:  getopcc (cmsg,kerr)
c
c   FUNCTION:  This routine called by C++
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
C WNT-START
      subroutine getopcc (cmsg,kerr)

      byte cmsg(80)
      integer*4 kerr,nc,pwdlen
      character*80 smsg
      byte snum(80)
c
      equivalence (smsg,snum)

      call getopc (smsg,kerr)
      nc     = pwdlen(smsg)
      if (nc .ne. 80) snum(nc+1) = 0
      do 100 i=1,80,1
          cmsg(i) = snum(i)
  100 continue

      return
      end

C WNT-END
