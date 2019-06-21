c
c***********************************************************************
c
c   FILE NAME: dbasio.for
c   CONTAINS:
c               rddbas  wrdbas
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        lbasio.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:08
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  rddbas (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from a direct access file
c              containing 204 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to read from.
c
c           krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kdat    I*4  D51   -  Data read from file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing read.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rddbas (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(51),kerr
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
c   SUBROUTINE:  wrdbas (klun,krec,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine writes a record to a direct access file
c              containing 204 byte records.
c
c   INPUT:  klun    I*4  D1    -  Unit number of file to write to.
c
c           krec    I*4  D1    -  Record number to write.
c
c   OUTPUT: kdat    I*4  D51   -  Data to write to file.
c
c           cmsg    C*n  D1    -  Error text when an error occurred dur-
c                                 ing write.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine wrdbas (klun,krec,kdat,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klun,krec,kdat(51),kerr
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
