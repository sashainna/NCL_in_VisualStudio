c
c***********************************************************************
c
c   FILE NAME: pworks_nt.f
c   CONTAINS:
c              pw_saveopt
c              pw_reinit
c              pw_getoptions
c              pw_saveoptions
c              pw_putpfile
c              pworks_ntinit
c              getopcc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pworks_nt.f , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:50:55
c
c***********************************************************************
c
c**********************************************************************
c   Subroutine pw_saveopt                                         *
c     save option data into tmp variables
c   Input: none
c   Output: nine
c   return: None
c!**********************************************************************
C WNT-START
      subroutine pw_saveopt

      include "postworks_nt.inc"
      include "menu.inc"
      include "pregen.inc"
c
      integer*4  i

      SICLF = ICLF
      SMCNSRC = MCNSRC
      SMAXWRN = MAXWRN
      SMAXPER = MAXPER
      SMAXFAT = MAXFAT
      SPSTNAM = PSTNAM(1)
      SPREPT = PREPT

      do 100 i=1,20,1
          SIOPFL(i) = IOPFL(i)
          SLOPFL(i) = LOPFL(i)
          SOPTOVR(i) = OPTOVR(i)
  100 continue
c
      do 200 i=1,50,1
          SUSROVR(i) = USROVR(i)
  200 continue
      return
      end
C WNT-END
c**********************************************************************
c   Subroutine pw_reinit                                         *
c     reinitialize postworks data, copy the save data back
c	to option data( because we need keep option data is
c	same as before running
c   Input: none
c   Output: nine
c   return: None
c!**********************************************************************
C WNT-START
      subroutine pw_reinit

      include "postworks_nt.inc"
      include "menu.inc"
      include "pregen.inc"
      character*80 msg
      integer*4 ierr, i

      call init
      call getopc (msg,ierr)
      ICLF = SICLF
      MCNSRC = SMCNSRC
      MAXWRN = SMAXWRN
      MAXPER = SMAXPER
      MAXFAT = SMAXFAT
      PSTNAM(1) = SPSTNAM
      PREPT = SPREPT

      do 100 i=1,20,1
          IOPFL(i) = SIOPFL(i)
          LOPFL(i) = SLOPFL(i)
          OPTOVR(i) = SOPTOVR(i)
  100 continue
c
      do 200 i=1,50,1
          USROVR(i) = SUSROVR(i)
  200 continue
      return
      end
C WNT-END
	
c***********************************************************************
c
c   SUBROUTINE: pw_getoptions(iopt, rusr, copt)
c
c   FUNCTION: get the option values
c
c   INPUT:  none.
c
c   OUTPUT:
c       iopt    =  integer option values
c       rusr    =  user variable values
c       copt    =  string option values
c
c***********************************************************************
C WNT-START
c
      subroutine pw_getoptions(iopt, rusr, copt)
c
      byte copt(256, 11)
      integer*4 iopt(40)
      real*8 rusr(50)
c
      include "menu.inc"
      include "pregen.inc"
c
      integer*4 i,j
c
      do 100 i=1, 20, 1
          iopt(i) = IOPFL(i)
  100 continue
      if (iopt(7) .eq. 2) iopt(7) = 1
      if (iopt(12) .ne. 0) iopt(12) = 1
      j = 1
      do 200 i=21, 40, 1
          iopt(i) = OPTOVR(j)
          j = j+1
  200 continue
c
      do 250 i=1, 50, 1
          rusr(i) = USROVR(i)
  250 continue

      do 300 i=1, 20, 1
          call pwdctb (LOPFL(i), copt(1,i))
  300 continue
      iopt(42) = ICLF
      iopt(43) = MCNSRC
      iopt(44) = MAXWRN
      iopt(45) = MAXPER
      iopt(46) = MAXFAT
      iopt(48) = PREPT
      call pwdctb (PSTNAM(1),copt(1,21))

      return
      end
C WNT-END

c***********************************************************************
c
c   SUBROUTINE: pw_saveoptions(iopt, rusr, copt)
c
c   FUNCTION: save the option values
c
c   INPUT:
c       iopt    =  integer option values
c       rusr    =  user variable values
c       copt    =  string option values
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine pw_saveoptions(iopt, rusr, copt)
c
      byte copt(256, 11)
      integer*4 iopt(38)
      real*8 rusr(50)
c
      include "menu.inc"
      include "pregen.inc"
c
      integer*4 i,j,nc,strlen1
c
      character*24 lnum
c
      do 100 i=1, 20, 1
          IOPFL(i) = iopt(i)
  100 continue

      j  = 1
      do 200 i=21, 41, 1
          OPTOVR(j) = iopt(i)
          j = j+1
  200 continue

      do 250 i=1, 50, 1
          USROVR(i) = rusr(i)
  250 continue

      do 300 i=1, 20, 1
          call pwdbtc (copt(1,i), LOPFL(i), nc)
  300 continue
      call pwdbtc (copt(1,21), PSTNAM(1), nc)
      ICLF = iopt(42)
      MCNSRC = iopt(43)
      MAXWRN  = iopt(44)
      MAXPER = iopt(45)
      MAXFAT = iopt(46)
      PREPT = iopt(48)
c
c...Punch file extension can be set in MDF file
c...If it is specified here, then it overrides it
c
      nc     = strlen1(LOPFL(7))
      if (IOPFL(7) .eq. 1 .and. nc .gt. 0) IOPFL(7) = 2
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
c   SUBROUTINE: pw_putpfile(filename)
c
c   FUNCTION: save pworks file name into global value
c
c   INPUT:  filename: Pworks filename
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine pw_putpfile(filename)
c
      include "menu.inc"
c
      byte filename(MAX_PATH)
      integer*4 nc

      call pwdbtc (filename, LCMPFI, nc)
      return
      end
C WNT-END
c***********************************************************************
c
c   SUBROUTINE: pworks_ntinit()
c
c   FUNCTION: initialize pworks on WinNT
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
C WNT-START
c
      subroutine pworks_ntinit
      include "pregen.inc"
c
      call pworks_version
      call init
 	REPT  = 0
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
c***********************************************************************
c
c   SUBROUTINE:  geti4nc (cbuf, knc, knum, knwds, kerr)
c
c   FUNCTION:  This routine called by C++
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing real numbers
c                               separated by commas.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: knum    I*4  Dn  -  Array to receive integer values.
c
c           knwds   I*4  D1  -  Number of values parsed.
c
c           kerr    I*4  D1  -  Returns 1 if an error occurred.
c
c***********************************************************************
c
C WNT-START
      subroutine geti4nc (cbuf,knc,knum,knwds,kerr)
c
      integer*4 knc,knwds,kerr,knum(20)
c
      character*256 cbuf
c
      call geti4n (cbuf,knc,knum,knwds,kerr)
      return
      end

C WNT-END

c***********************************************************************
c
c   SUBROUTINE:  getr8nc (cbuf, knc, gnum, knwds, kerr)
c
c   FUNCTION:  This routine called by C++
c               get real number sep by ','
c               it will allow long string cbuf to 1000 char
c
c   INPUT:  cbuf    C*n  D1  -  Character string containing real numbers
c                               separated by commas. string upto 1000 chars
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: gnum    I*4  Dn  -  Array to receive real values.
c
c           knwds   I*4  D1  -  Number of values parsed.
c
c           kerr    I*4  D1  -  Returns 1 if an error occurred.
c
c***********************************************************************
c
C WNT-START
      subroutine getr8nc (cbuf,knc,gnum,knwds,kerr)
c
      integer*4 knc,knwds,kerr
c
      real*8 gnum(100)
c
      character*1000 cbuf
c
      call getr8nl (cbuf,knc,gnum,knwds,kerr)
      return
      end

C WNT-END

