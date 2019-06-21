c
c***********************************************************************
c
c   FILE NAME: pwconv_nt.f
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
c     COPYRIGHT 2011 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pwconv_nt.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        09/26/17 , 12:34:14
c
c***********************************************************************
c***********************************************************************
c**********************************************************************
c   Subroutine      pv_reinit
c
c        reinitialize postworks data, copy the save data back
c	   to option data( because we need keep option data is
c	   same as before running
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pv_reinit

      include "postworks_nt.inc"
      include "menu.inc"
c
      character*80 msg
      integer*4 ierr, i

      call init
      call pstini
      call getopc (msg,ierr)

      ICLF = SICLF
      MCNSRC = SMCNSRC

      i = 2
      do while(i.le.20)
          IOPFL(i) = SIOPFL(i)
          LOPFL(i) = SLOPFL(i)
          i = i + 1
      enddo

      return
      end
C WNT-END

c**********************************************************************
c   Subroutine         pv_saveopt                                         *
c            save option data into tmp variables
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pv_saveopt

      include "postworks_nt.inc"
      include "menu.inc"
      integer*4  i

      SICLF = ICLF
      SMCNSRC = MCNSRC

      i = 1
      do while(i.le.20)
          SIOPFL(i) = IOPFL(i)
          SLOPFL(i) = LOPFL(i)
          i = i + 1
      enddo

      return
      end



C WNT-END
c**********************************************************************
c   Subroutine    pwconv_ntinit                                  *
c        initialize Pwmacro on WInNT
c   Input: none
c   Output: nine
c   return: None
c**********************************************************************

C WNT-START
      subroutine pwconv_ntinit

      include "pregen.inc"

      call pwconv_version
      call init
      call pstini

      return
      end

C WNT-END
c***********************************************************************
c
c   SUBROUTINE: pv_getoptions(iopt, copt)
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
      subroutine pv_getoptions(iopt, copt)
c
      byte copt(256, 20)
      integer*4 iopt(20)
      include "menu.inc"
c
c...correspond LOPFL(i) length of 256
c
      byte snum(256)
      character*256 sbuf
      equivalence (sbuf,snum)

      integer*4 i,j,nc,pwdlen
c
      do 100 i=1, 20, 1
          iopt(i) = IOPFL(i)
  100 continue
      if (iopt(12) .ne. 0) iopt(12) = 1

      do 300 i=1, 20, 1
          sbuf(1:) = LOPFL(i)(1:)
          nc  = pwdlen(sbuf)
          if (nc .ge. 256) nc = 255
          snum(nc+1) = 0
          snum(256) = 0
          do 200 j=1,256,1
              copt(j,i) = snum(j)
  200     continue
  300 continue

      iopt(8) = ICLF
      iopt(9) = MCNSRC

      return
      end
C WNT-END

c***********************************************************************
c
c   SUBROUTINE: pv_saveoptions(iopt, copt)
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
      subroutine pv_saveoptions(iopt, copt)
c
      byte copt(256, 20)
      integer*4 iopt(20)
      include "menu.inc"

      byte snum(256)
      character*256 sbuf
      equivalence (sbuf,snum)
c
      integer*4 i,j,nc
c
      character*24 lnum
c
      do 100 i=1, 20, 1
          IOPFL(i) = iopt(i)
  100 continue
c
c....don't use following since pwdbtc assume the pass in string is 1024 chars
c....but here is only 256 (define LOPFL(i) as 256 in *.inc file
c....yurong
c.......      do 300 i=1, 20, 1
c.......          call pwdbtc (copt(1,i), LOPFL(i), nc)
c.......  300 continue
      do 300 i=1, 20, 1
          do 150 j=1,256,1
              if (copt(j,i).eq. 0) go to 200
              snum(j) = copt(j,i)
  150     continue
  200     nc    = j      - 1
          LOPFL(i)   = sbuf(1:nc)
  300 continue
      ICLF = iopt(8)
      MCNSRC = iopt(9)
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
c   SUBROUTINE: pv_putpfile(filename)
c
c   FUNCTION: save Pwconv file name into global value
c
c   INPUT:  filename: Pwconv filename
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine pv_putpfile(filename)
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
