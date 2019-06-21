c
c***********************************************************************
c
c   FILE NAME:  support.for
c   CONTAINS:
c               ncdate  fmtdat  exit    getmcr  trmmsg  ftim  fmttim
c               gerror
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        support.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:18
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ncdate (cd)
c
c   FUNCTION:  This routine returns the current date in the form:
c              dd-mmm-yyyy.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Character string to receive the current
c                               date.
c
c***********************************************************************
c
      subroutine ncdate (cd)
c
      character*(*) cd
c
      integer*4 imon,iday,iyear
C WNT-START
      integer*4 values(8)
      character*12 rc(3)
C WNT-END
C SUN-CIM-DOS-START
C     integer*4 idat(3)
C SUN-CIM-DOS-END
c
c...Get binary date
c
C HPX-START
C      call idate (iday,imon,iyear)
C HPX-END
C VAX-SGI-IBM-OSF-START
C     call idate (imon,iday,iyear)
C VAX-SGI-IBM-OSF-END
c
c...Before, WNT use same format as SGI, use idate, but
c...Two digit year return value may cause problem after 2000,
c...so change it
C WNT-START
      CALL DATE_AND_TIME(rc(1),rc(2),rc(3),values);
      iday = values(3)
      imon = values(2)
      iyear = values(1)
C WNT-END
C SUN-CIM-START
C     call idate (idat)
C     imon   = idat(2)
C     iday   = idat(1)
C     iyear  = idat(3)
C SUN-CIM-END
C DOS-START
C     call idatep (idat)
C     imon   = idat(2)
C     iday   = idat(1)
C     iyear  = idat(3)
C DOS-END
c
c...Convert to text date
c
      call fmtdat (imon,iday,iyear,cd)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fmtdat (kmon,kday,kyear,cd)
c
c   FUNCTION:  This routine formats the date in the form: dd-mmm-yyyy.
c
c   INPUT:  kmon    I*4  D1  -  Month.
c
c           kday    I*4  D1  -  Day.
c
c           kyear   I*4  D1  -  Year.
c
c   OUTPUT: cdat    C*n  D1  -  Character string to receive the date
c                               string.
c
c***********************************************************************
c
      subroutine fmtdat (kmon,kday,kyear,cd)
c
      integer*4 kmon,kday,kyear
c
      character*(*) cd
c
      character*2 ld
      character*3 mon(12)
      character*4 ly
c
      data mon /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1          'OCT','NOV','DEC'/
c
c...Convert to text date
c
      if (kyear .lt. 1900) kyear  = kyear  + 1900
      if (kyear .lt. 1970) kyear  = kyear  + 100
      write (ld,10) kday
   10 format (i2.2)
      write (ly,20) kyear
   20 format (i4.4)
      cd     = ld  // '-' // mon(kmon) // '-' // ly
c
c...End of routine
c
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  exit
c
c   FUNCTION:  This is the EXIT routine for Unix systems.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C SUN-SGI-START
C     subroutine exit
C
C     call wflush
C     stop
C     end
C SUN-SGI-END
c
c***********************************************************************
c
c   SUBROUTINE:  getmcr (cdat,knc)
c
c   FUNCTION:  This routine returns the runtime command line.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Character string to receive the runtime
c                               command line.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine getmcr (cdat,knc)
c
      include 'menu.inc'
	
c
      integer*4 knc
c
      character*(*) cdat
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      integer*4 nc,inum,strlen1,i,iargc
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      integer*4 nc,inum,strlen1,i
C WNT-END
c...      character*80 ldat,tbuf
      character*(MAX_PATH+80) ldat,tbuf
C WNT-START
      cdat   = MCRBUF
      knc    = MCRNC
C WNT-END
c
c...Command line supplied by Motif
c
C SUN-SGI-IBM-HPX-DEC-START
C     if (MOTIF .eq. 1) then
C         cdat   = MCRBUF
C         knc    = MCRNC
C         go to 8000
C     endif

c...Get number of Arguments
c...in command line
c
C     inum   = iargc()

c...No arguments specified
c
C     if (inum .eq. 0) then
C	         cdat   = ' '
C         knc    = 0
c
c...Build command line from arguments
c
C     else
C         call getarg (1,cdat)

C         knc    = strlen1(cdat)
C         if (inum .eq. 1) go to 8000
C         do 100 i=2,inum,1
C             call getarg (i,ldat)
C             nc     = strlen1(ldat)
C             tbuf   = cdat(1:knc) // ' ' // ldat(1:nc)
C            cdat   = tbuf
C             knc    = knc    + nc     + 1
C 100     continue
C     endif
C SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C      call getcl (cdat)
C      call remspc (cdat,cdat,knc)
C DOS-END
c VAX-START
C     integer*4 istat
C     istat  = lib$get_foreign (cdat,,knc)
c VAX-END
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ftim (cd)
c
c   FUNCTION:  This routine returns the current time in the form:
c              hh:mm:ss.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  -  Character string to receive the current
c                               time.
c
c***********************************************************************
c
      subroutine ftim (cd)
c
      character*(*) cd
c
C WNT-START
      integer*4 values(8)
      character*12 rc(3)
C WNT-END
C SUN-WNT-HPX-START
      integer*4 it(3)
C SUN-WNT-HPX-END
C IBM-START
C     integer*4 it,ctime
C     character*26 ltim
c
C     pointer (p_time,ltim)
C IBM-END
C VAX-SGI-DOS-DEC-START
C     call time (cd)
C VAX-SGI-DOS-DEC-END
C WNT-START
      call DATE_AND_TIME (rc(1),rc(2),rc(3),values)
      it(1) = values(5)
      it(2) = values(6)
      it(3) = values(7)
C WNT-END
C SUN-HPX-START
c
c...Get the integer time
c
C     call itime (it)
c
c...Convert to character format
c...hh:mm:ss
c
C WNT-START
      call fmttim (it(1),it(2),it(3),cd)
C SUN-HPX-WNT-END
C IBM-START
c
c...Get the time since 'Year 0'
c
C     call time (it)
c
c...Convert to character format
c...hh:mm:ss
c
C     p_time = ctime(it)
C     cd    = ltim(12:19)
C IBM-END
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fmttim (khr,kmin,ksec,cd)
c
c   FUNCTION:  This routine formats the time in the form: hh:mm:ss.
c
c   INPUT:  khr     I*4  D1  -  Hour.
c
c           kmin    I*4  D1  -  Minutes.
c
c           ksec    I*4  D1  -  Seconds.
c
c   OUTPUT: cdat    C*n  D1  -  Character string to receive the time
c                               string.
c
c***********************************************************************
c
      subroutine fmttim (khr,kmin,ksec,cd)
c
      integer*4 khr,kmin,ksec
c
      character*(*) cd
c
c...Convert to character format
c...hh:mm:ss
c
      write (cd,10) khr,kmin,ksec
   10 format (i2,':',i2.2,':',i2.2)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gerror (cbuf)
c
c   FUNCTION:  This routine returns an IBM error message string.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to receive the error
c                               message text.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C IBM-HPX-START
C      subroutine gerror (cbuf)
c
C      include 'menu.inc'
c
C      character*(*) cbuf
c
c...Don't know how to get the error message
c
C      write (cbuf,10) IBMERR
C  10 format ('Fortran Error #',i3)
C      return
C      end
C IBM-HPX-END
c
C DOS-START
C      subroutine idate (imo,iday,iyr)
C      integer*4 imo,iday,iyr
C
C      character*10 cdat
C
C      call date (cdat)
C      read (cdat(4:5),10) iday
C      read (cdat(1:2),10) imo
C      read (cdat(7:8),10) iyr
C      if (iyr .lt. 1900) iyr = iyr + 1900
C  10  format (i2)
C      return
C      end
C
C      subroutine idatep (knum)
C      integer*4 knum(3)
C      call idate (knum(2),knum(1),knum(3))
C      return
C      end
C DOS-END
c***********************************************************************
c
c   SUBROUTINE:  trmmsg (cbuf)
c
c   FUNCTION:  This routine writes a character string to the screen.
c              Use this routine instead of 'dmpbuf' when the terminal
c              has not been opened.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to write to the screen.
c
c   OUTPUT: none.
c
c***********************************************************************
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
C      subroutine trmmsg (cbuf)
c
C      character*(*) cbuf
c
C      integer*4 nc,strlen1
c
c...Write a message to the terminal screen
c
C       nc     = strlen1(cbuf)
C      if (nc .eq. 0) nc = 1
C      if (nc .gt. 80) nc = 80
C      call dmpbuf (cbuf,nc)
C      call trmnl (1)
C      return
C      end
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
