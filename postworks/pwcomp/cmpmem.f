c
c***********************************************************************
c
c   FILE NAME: cmpmem.for
c   CONTAINS:
c               memall  rdmem   wrmem   lodmem  stomem
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmpmem.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:12
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  memall (krec,cmsg,kerr)
c
c   FUNCTION:  This routine allocates a chunk of memory.
c
c   INPUT:  krec    I*4  D1    -  Chunk of memory to allocate.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred try-
c                                 ing to allocate memory.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine memall (krec,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 krec,kerr
c
      character*(*) cmsg
c
      integer*4 i, pt
C DOS-START
C     integer*4 ipt,idat(128)
C DOS-END
c
C VAX-START
C     integer*4 istat,sys$expreg
C VAX-END
c
c...Allocate memory from the current
c...chunk to the requested chunk
c
C VAX-START
C     do 100 i=MEMREC+1,krec,1
C         istat  = sys$expreg (%VAL(MEMSIZ),MEMPT(1,i),,)
C         if (.not. istat) go to 9000
C 100 continue
C VAX-END
C SUN-SGI-IBM-HPX-DEC-START
C     do 100 i=MEMREC+1,krec,1
C         call cmall (MEMSIZ*512,MEMPT(1,i))
C         if (MEMPT(1,i) .eq. 0) go to 9000
C 100 continue
C SUN-SGI-IBM-HPX-DEC-END
C WNT-START
      do 100 i=MEMREC+1,krec,1
c...Yurong changed 8/14/98
c
C...          pt = LOC(MEMPT(1,i))
C...          call cmall (MEMSIZ*512,pt)
          MEMPT(1,i) = MALLOC(MEMSIZ*512)
          if (MEMPT(1,i) .eq. 0) go to 9000
  100 continue

C WNT-END  		
C DOS-START
C     do 100 i=MEMREC+1,krec,1
C         call memmen (1,i,MEMPT(1,i),idat,ipt,MEMSIZ,4,kerr)
C         if (kerr .ne. 0) go to 9000
C 100 continue
C DOS-END
      MEMREC = krec
	krec = MEMREC
c
c...End of routine
c
 8000 return
c
c...Error trying to allocate memory
c
 9000 kerr   = -1
      call errtxt ('MEMALL',cmsg)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdmem (kpt,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a section of the work file memory
c              storage.
c
c   INPUT:  kpt     I*4  D1    -  Section of memory to read from.
c
c   OUTPUT: kdat    I*4  D128  -  Data read from memory.
c
c           cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rdmem (kpt,kdat,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kpt,kdat(128),kerr
	
c
      character*(*) cmsg
c
      integer*4 irec,ipt
c
c...Calculate memory chunk & location
c
C WNT-VAX-SUN-SGI-DOS-IBM-HPX-DEC-START
      irec   = (kpt-1) / MEMSIZ + 1
      ipt    = kpt    - ((irec-1)*MEMSIZ)
      ipt    = (ipt-1) * 128 + 1
C WNT-VAX-SUN-SGI-DOS-IBM-HPX-DEC-END
c
c...Chunk of memory has not been allocated
c
c
c...added check for ipt
c...Yurong 3/4/02
c
c...      if (irec .gt. MEMREC .or. irec .le. 0) go to 9000
      if (irec .gt. MEMREC .or. irec .le. 0 .or. ipt .le. 0) go to 9000
c
c...Read data from memory
c
C VAX-START
C     call lodmem (kdat,ipt,%VAL(MEMPT(1,irec)))
C VAX-END
C WNT-SUN-SGI-IBM-HPX-DEC-START
      call cldmem (kdat,ipt,MEMPT(1,irec))
C WNT-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C     call memmen (2,irec,MEMPT(1,irec),kdat,ipt,128,4,kerr)
C DOS-END
c
c...End of routine
c
 8000 return
c
c...No such memory
c
 9000 call errtxt ('NOMEMALL',cmsg)
      kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrmem (kpt,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine writes a section of the work file memory
c              storage.
c
c   INPUT:  kpt     I*4  D1    -  Section of memory to write to.
c
c           kdat    I*4  D128  -  Data to write to memory.
c
c   OUTPUT: cmsg    C*n  D1    -  Text of error message.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine wrmem (kpt,kdat,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kpt,kdat(128),kerr
c
      character*(*) cmsg
c
      integer*4 irec,ipt
c
c...Calculate memory chunk & location
c
C WNT-VAX-SUN-SGI-DOS-IBM-HPX-DEC-START
      irec   = (kpt-1) / MEMSIZ + 1
      ipt    = kpt    - ((irec-1)*MEMSIZ)
      ipt    = (ipt-1) * 128 + 1
C WNT-VAX-SUN-SGI-DOS-IBM-HPX-DEC-END
c
c...Chunk of memory has not been allocated
c...So allocate another chunk
c
      if (irec .gt. MEMREC) then
	  if (irec .gt. MAXMEM) go to 9000
	  call memall (irec,cmsg,kerr)
	  if (kerr .ne. 0) go to 8000
      endif
c
c...Write data to memory
c
C VAX-START
C     call stomem (kdat,ipt,%VAL(MEMPT(1,irec)))
C VAX-END
C WNT-SUN-SGI-IBM-HPX-DEC-START
      call cstmem (kdat,ipt,MEMPT(1,irec))
C WNT-SUN-SGI-IBM-HPX-DEC-END

C DOS-START
C     call memmen (3,irec,MEMPT(1,irec),kdat,ipt,128,4,kerr)
C DOS-END
c
c...End of routine
c
 8000 return
c
c...Ran out of memory
c
 9000 call errtxt ('PGMLARG',cmsg)
      kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodmem (kdat,kpt,kdest)
c
c   FUNCTION:  This routine loads a page of memory to a data buffer.
c
c   INPUT:  kpt     I*4  D1    -  Section of memory to read from.
c
c           kdest   I*4  Dn    -  Allocated memory pointer.
c
c   OUTPUT: kdat    I*4  D128  -  Data read from memory.
c
c***********************************************************************
c
      subroutine lodmem (kdat,kpt,kdest)
c
      integer*4 kdat(128),kpt,kdest(128)
c
      integer*4 i,ipt
c
      ipt    = kpt    - 1
      do 100 i=1,128,1
	  kdat(i) = kdest(ipt+i)
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stomem (kdat,kpt,kdest)
c
c   FUNCTION:  This routine loads a data buffer to a page of memory.
c
c   INPUT:  kpt     I*4  D1    -  Section of memory to write to.
c
c           kdat    I*4  D128  -  Data to store in memory.
c
c           kdest   I*4  Dn    -  Allocated memory pointer.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine stomem (kdat,kpt,kdest)
c
      integer*4 kdat(128),kpt,kdest(128)
c
      integer*4 i,ipt
c
      ipt    = kpt    - 1
      do 100 i=1,128,1
	  kdest(ipt+i) = kdat(i)
  100 continue
c
c...End of routine
c
 8000 return
      end
