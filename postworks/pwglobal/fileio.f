c
c***********************************************************************
c
c   FILE NAME: fileio.for
c   CONTAINS:
c               clsdel  clsfil  rdcom   rdcat   rdcat5  rdncl   rdmac
c               rdprm   rdtxt   drtxt   wrcom   wrmac   wrncl   wrprm
c               wrtxt   getfnm
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        fileio.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:17
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  clsdel (klun)
c
c   FUNCTION:  This routine closes and deletes an open file.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to close and
c                                 delete.
c
c   OUTPUT: none.
c
c***********************************************************************
c
c
      subroutine clsdel (klun)
c
      include 'menu.inc'
c
      integer*4 klun
c
c...Close and delete the specific file
c
      close (unit=klun,status='DELETE')
c
      return
      end
c
c
c***********************************************************************
c
c   SUBROUTINE:  clsfil (klun)
c
c   FUNCTION:  This routine closes an open file or all of the open
c              files.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to close.  If
c                                 'klun'=0, then all of the open files
c                                 will be closed.
c
c   OUTPUT: none.
c
c***********************************************************************
c
c
      subroutine clsfil (klun)
c
      include 'menu.inc'
c
      integer*4 klun
c
c...Close all open files
c
      if (klun .eq. 0) then
          close (unit=LUNHLP)
          close (unit=LUNPRM)
          close (unit=LUNSC1)
          close (unit=LUNSC2)
          close (unit=LUNSC3)
          close (unit=LUNSC4)
          close (unit=LUNTI)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...not for Wnt
C          close (unit=LUNTO)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c...Close the specific file
c
      else
          close (unit=klun)
      endif
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdcom (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a direct access file
c              containing 8000 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c           krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kdat    I*4  D72   -  Data read from file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing read.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rdcom (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(2000),kerr
c
      character*(*) cmsg
c
      character*80 ldat
c
      integer*4 nc
c
c...Read from prompt or error message file
c
      kerr   = 0
      read (klun,rec=krec,err=9000) kdat
      return
c
c...Error reading from file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error reading file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdcat (klun,krec,kswap,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a direct access file
c              containing 3228 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c           krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kdat    I*4  D72   -  Data read from file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing read.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rdcat (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(807),kerr
c
      character*(*) cmsg
c
      character*80 ldat
c
      integer*4 nc
C WNT-DOS-START
      integer*4 jbbb,i,j
      byte bbuf(3228),bbb(4)
      equivalence (bbb,jbbb)
C WNT-DOS-END
c
c...Read from prompt or error message file
c
      kerr   = 0
C SUN-SGI-IBM-HPX-DEC-START
C      read (klun,rec=krec,err=9000) kdat
C SUN-SGI-IBM-HPX-DEC-END
C WNT-DOS-START
      read (klun,rec=krec,err=9000) bbuf
      do 105 i=1,807
          j   = i * 4
          bbb(1) = bbuf(j-2)
          bbb(2) = bbuf(j-3)
          bbb(3) = bbuf(j)
          bbb(4) = bbuf(j-1)
          kdat(i) = jbbb
  105 continue
C WNT-DOS-END
      return
c
c...Error reading from file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error reading file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdcat5 (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a Catia V5 file
c              containing 264 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c           krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kdat    I*4  D72   -  Data read from file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing read.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rdcat5 (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(66),kerr
c
      character*(*) cmsg
c
      character*80 ldat
c
      integer*4 nc
C WNT-DOS-START
      integer*4 jbbb(2),i,j
      byte bbuf(264),bbb(8)
      equivalence (bbb,jbbb)
C WNT-DOS-END
c
c...Read from prompt or error message file
c
      kerr   = 0
C SUN-SGI-IBM-HPX-DEC-START
C      kdat(1) = -98765
C      read (klun,rec=krec,err=9000) kdat
C SUN-SGI-IBM-HPX-DEC-END
C WNT-DOS-START
      read (klun,rec=krec,err=9000) bbuf
      do 106 i=1,264,2
          j   = (i+1) * 4
          bbb(1) = bbuf(j)
          bbb(2) = bbuf(j-1)
          bbb(3) = bbuf(j-2)
          bbb(4) = bbuf(j-3)
          bbb(5) = bbuf(j-4)
          bbb(6) = bbuf(j-5)
          bbb(7) = bbuf(j-6)
          bbb(8) = bbuf(j-7)
          kdat(i) = jbbb(1)
          kdat(i+1) = jbbb(2)
  106 continue
C WNT-DOS-END
      return
c
c...Error reading from file
c
 9000 if (kdat(1) .eq. -98765) then
          kerr   = -1
          call getfnm (klun,ldat,nc,50)
          cmsg   = '*FATAL*  Error reading file "' // ldat(1:nc) // '".'
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdncl (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a direct access file
c              containing 288 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c           krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kdat    I*4  D72   -  Data read from file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing read.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rdncl (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(72),kerr
c
      character*(*) cmsg
c
      character*80 ldat
c
      integer*4 nc
c
c...Read from prompt or error message file
c
      kerr   = 0
      read (klun,rec=krec,err=9000) kdat
      return
c
c...Error reading from file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error reading file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdmac (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a direct access file
c              containing 400 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c           krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kdat    I*4  D100  -  Data read from file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing read.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rdmac (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(100),kerr
c
      character*(*) cmsg
c
      character*80 ldat
c
      integer*4 nc
c
c...Read from prompt or error message file
c
      kerr   = 0
      read (klun,rec=krec,err=9000) kdat
      return
c
c...Error reading from file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error reading file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdprm (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a direct access file
c              containing 512 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c           krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kdat    I*4  D128  -  Data read from file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing read.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rdprm (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(128),kerr
c
      character*(*) cmsg
c
      character*80 ldat
c
      integer*4 nc
c
c...Read from prompt or error message file
c
      kerr   = 0
      read (klun,rec=krec,err=9000) kdat
      return
c
c...Error reading from file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error reading file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdtxt (klun,cdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a formatted ascii file.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c   OUTPUT: cdat    C*n  D1    -  Data read from file.
c
c           cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns 1 when the EOF is reached.
c                                 Returns -1 when an error occurred
c                                 during the read.
c
c***********************************************************************
c
      subroutine rdtxt (klun,cdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 nc
c
      character*80 ldat
c
c...Read from text file
c
      read (klun,10,end=9000,err=9100) cdat
   10 format (a)
      kerr   = 0
      return
c
c...End of file
c
 9000 kerr   = 1
      return
c
c...Error reading file
c
 9100 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error reading file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  drtxt (klun,cdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a formatted ascii file
c              on PC/DOS machines without "unix2dos" conversion since
c              convertor removes some characters (DEL).  The input file
c              must opened with TRANSPARENT access.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c   OUTPUT: cdat    C*n  D1    -  Data read from file.
c
c           cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns 1 when the EOF is reached.
c                                 Returns -1 when an error occurred
c                                 during the read.
c
c***********************************************************************
C WNT-DOS-START
      subroutine drtxt (klun,cdat,cmsg,kerr)

      integer*4 klun,kerr

      character*(*) cdat,cmsg

      character*80 ldat

      integer*4 i,nc
      integer*1 lbuf(160)
      character*160 cbuf

      equivalence (lbuf,cbuf)

c...Read from text file

   10 format (A)
      do 505 i=1,160
          read (klun,10,end=900,err=9100) lbuf(i)
          if (lbuf(i) .eq. 10) go to 600
  505 continue
  600 if (i .gt. 1) cdat  = cbuf(1:i-1)
      kerr   = 0
      return

c...See if EOF with empty buffer
c
  900 if (i .gt. 1) go to 600

c...End of file
c
 9000 kerr   = 1
      return
c
c...Error reading file
c
 9100 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error reading file "' // ldat(1:nc) // '".'
      return
      end
C WNT-DOS-END
c
c***********************************************************************
c
c   SUBROUTINE:  wrcom (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine writes a record to a direct access file
c              containing 8000 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to write to.
c
c           krec    I*4  D1    -  Record number to write.
c
c   OUTPUT: kdat    I*4  D2000 -  Data to write to file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing write.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine wrcom (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(2000),kerr
c
      character*(*) cmsg
c
      integer*4 nc
c
      character*80 ldat
c
c...Write data to file
c
      kerr   = 0
      write (klun,rec=krec,err=9000) kdat
      return
c
c...Error writing to file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error writing file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrmac (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine writes a record to a direct access file
c              containing 400 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to write to.
c
c           krec    I*4  D1    -  Record number to write.
c
c   OUTPUT: kdat    I*4  D100  -  Data to write to file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing write.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine wrmac (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(100),kerr
c
      character*(*) cmsg
c
      integer*4 nc
c
      character*80 ldat
c
c...Write data to file
c
      kerr   = 0
      write (klun,rec=krec,err=9000) kdat
      return
c
c...Error writing to file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error writing file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrncl (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine writes a record to a direct access file
c              containing 288 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to write to.
c
c           krec    I*4  D1    -  Record number to write.
c
c   OUTPUT: kdat    I*4  D72   -  Data to write to file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing write.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine wrncl (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(72),kerr
c
      character*(*) cmsg
c
      integer*4 nc
c
      character*80 ldat
c
c...Write data to file
c
      kerr   = 0
      write (klun,rec=krec,err=9000) kdat
      return
c
c...Error writing to file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error writing file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrprm (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine writes a record to a direct access file
c              containing 512 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to write to.
c
c           krec    I*4  D1    -  Record number to write.
c
c   OUTPUT: kdat    I*4  D128  -  Data to write to file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing write.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine wrprm (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(128),kerr
c
      character*(*) cmsg
c
      integer*4 nc
c
      character*80 ldat
c
c...Write data to file
c
      kerr   = 0
      write (klun,rec=krec,err=9000) kdat
      return
c
c...Error writing to file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error writing file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrtxt (klun,cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine writes a record to a text file.
c
c   INPUT:  klun    I*4  D1  -  Unit number of file to write to.
c
c           cdat    C*n  D1  -  Data to write to file.
c
c           knc     I*4  D1  -  Number of chars in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing write.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine wrtxt (klun,cdat,knc,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,knc,kerr
c
      character*(*) cdat,cmsg
      integer stat
c
      integer*4 nc
c
      character*80 ldat
c
c...Write to text file
c
      kerr   = 0
      write (klun,10,iostat = stat, err=9000) cdat(1:knc)
   10 format (a)
      return
c
c...Error writing file
c
 9000 kerr   = -1
      call getfnm (klun,ldat,nc,50)
      cmsg   = '*FATAL*  Error writing file "' // ldat(1:nc) // '".'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getfnm (kunit,cfile,knc,kmaxc)
c
c   FUNCTION:  This routine returns the file name associated with a unit
c              number.
c
c   INPUT:  kunit   I*4  D1  -  Unit number of file.
c
c           kmaxc   I*4  D1  -  Maximum number of chars to place in 'cfile'.
c
c   OUTPUT: cfile   C*n  D1  -  Name of file associated with 'kunit'.
c
c           knc     I*4  D1  -  Number of characters in 'cfile'.
c
c***********************************************************************
c
      subroutine getfnm (kunit,cfile,knc,kmaxc)
c
      include 'menu.inc'
c
      integer*4 kunit,knc,kmaxc
c
      character*(*) cfile
c
      integer*4 strlen1
      logical iname
c
      character*(MAX_PATH) fnam
c
c...Does this file have a name?
c
      inquire (UNIT=kunit,NAMED=iname)
      if (iname) then
c
c...Get the filename
c
          inquire (UNIT=kunit,NAME=fnam)
          call shfile (fnam,cfile,kmaxc)
          knc    = strlen1(cfile)
c
c...No filename
c
      else
          cfile = ' '
          knc   = 0
      endif
c
c...End of routine
c
 8000 return
      end
