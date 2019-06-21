c
c***********************************************************************
c
c   FILE NAME: pmacro_win.f
C		This file only for WinNT
c   CONTAINS:
c              pm_saveopt
c              pm_reinit
c              pm_getoptions
c              pm_saveoptions
c              pm_putpfile
c              pmacro_ntinit
c              getopcc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pmacro_nt.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:52
c
c***********************************************************************
c***********************************************************************
c**********************************************************************
c   Subroutine      pm_reinit
c
c        reinitialize postworks data, copy the save data back
c	   to option data( because we need keep option data is
c	   same as before running
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pm_reinit

      include "postworks_nt.inc"
      include "menu.inc"
      include "pregen.inc"
      character*80 msg
      integer*4 ierr, i

      call init
      call pstini
      call getopc (msg,ierr)

      ICLF = SICLF
      MCNSRC = SMCNSRC
      PRENUM(1) = SPRENUM
      PREPT = SPREPT

      i = 2
      do while(i.ne.20)
          IOPFL(i) = SIOPFL(i)
          LOPFL(i) = SLOPFL(i)
      i = i + 1
      enddo

      return
      end
C WNT-END

c**********************************************************************
c   Subroutine         pm_saveopt                                         *
c            save option data into tmp variables
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pm_saveopt

      include "postworks_nt.inc"
      include "menu.inc"
      include "pregen.inc"
      integer*4  i
      SPRENUM = PRENUM(1)
      SPREPT = PREPT
      SICLF = ICLF
      SMCNSRC = MCNSRC

      i = 1
      do while(i.ne.20)
          SIOPFL(i) = IOPFL(i)
          SLOPFL(i) = LOPFL(i)
          i = i + 1
      enddo

      return
      end



C WNT-END
c**********************************************************************
c   Subroutine    pmacro_ntinit                                  *
c        initialize Pwmacro on WInNT
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pmacro_ntinit

      include "pregen.inc"

      call pmacro_version
      call init
      call pstini
      PREPT  = 0
      MEMPT(2) = 0

      return
      end

C WNT-END
c***********************************************************************
c
c   SUBROUTINE: pm_getoptions(iopt, copt)
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
      subroutine pm_getoptions(iopt, copt)
c
      byte copt(256, 20)
      integer*4 iopt(21)
      include "menu.inc"
      include "pregen.inc"

      integer*4 i
c
      do 100 i=1, 20, 1
          iopt(i) = IOPFL(i)
  100 continue
      if (iopt(12) .ne. 0) iopt(12) = 1

      do 300 i=1, 20, 1
          call pwdctb (LOPFL(i), copt(1,i))
  300 continue
      iopt(8) = ICLF
      iopt(9) = MCNSRC
      iopt(20) = PRENUM(1)
      iopt(21) = PREPT

      return
      end
C WNT-END

c***********************************************************************
c
c   SUBROUTINE: pm_saveoptions(iopt, copt)
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
      subroutine pm_saveoptions(iopt, copt)
c
      byte copt(256, 20)
      integer*4 iopt(21)
      include "menu.inc"
      include "pregen.inc"

      integer*4 i,nc
c
      character*24 lnum
c
      do 100 i=1, 20, 1
          IOPFL(i) = iopt(i)
  100 continue

      do 300 i=1, 20, 1
          call pwdbtc (copt(1,i), LOPFL(i), nc)
  300 continue
      ICLF = iopt(8)
      MCNSRC = iopt(9)
      PRENUM(1) = iopt(20)
      PREPT = iopt(21)
c
c...Setup FILL option
c
      if (IOPFL(12) .eq. 1) then
          IOPFL(12) = -1
          call ctor (LOPFL(12),FILVAL,ierr)
          if (ierr .eq. 1) then
              call touppr (LOPFL(12),lnum)
              call getvnm (lnum,IOPFL(12),PSTWRD,PSTWVL,NPSTWD)
          endif
      endif

      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE: pm_putpfile(filename)
c
c   FUNCTION: save PMacro file name into global value
c
c   INPUT:  filename: PMacro filename
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine pm_putpfile(filename)
c
      include 'menu.inc'
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
