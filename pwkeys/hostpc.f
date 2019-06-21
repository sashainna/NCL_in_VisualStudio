c****************************************************************
c
c   FILE NAME:  hostid.for
c
c   CONTAINS:  hostid  getkey
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        hostpc.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:07
c
c****************************************************************
c
c****************************************************************
c
c   SUBROUTINE:  hostid (knum)
c
c   FUNCTION:  This routine is used to read the SCRIBE key
c              device inside the licensed software.
c
c   INPUT:  none
c
c   OUTPUT:  knum    I*4  D1  -  host ID number.  -1 if not
c                                not avilable.
c
c****************************************************************
c
C DOS-START
C     subroutine hostid (knum)
c
C     integer*4 knum
c
C     integer*2 istat,sk_command
c
C     character*40 commd
C     character*18 pasw1
C     character*120 cscbf
C     character*35 nccs
c
C     integer*2 iscbf(60)
C     integer*4 idnum
c
C     equivalence (iscbf,cscbf), (iscbf(59),idnum)
c
C     data pasw1 /'E/mqJTkdevdxEbhyZh'/
C     data nccs /'Numerical Control Computer Sciences'/
c
c...Initialize SCRIBE device
c
C     commd  = 'I/1/0' // char(0)
C     istat  = sk_command (commd)
C     if (istat .ne. 0) go to 8100
c
c...Enable read only the key
c
C     commd  = pasw1 // char(0)
C     istat  = sk_command (commd)
C     if (istat .ne. 0) go to 8100
c
c...Read the key
c
C     call getkey (iscbf)
C     if (cscbf(1:35) .ne. nccs) go to 8100
C     if (cscbf(36:49) .ne. 'Licensed for: ') go to 8100
C     knum   = idnum
C     go to 9000
c
C8100 knum   = -1
c
c...End of routine
c
C9000 return
C     end
C DOS-END
c
c****************************************************************
c
c   SUBROUTINE:  getkey (ibuf)
c
c   FUNCTION:  This routine is used to read the contents of the
c              key device.
c
c   INPUT:  none
c
c   OUTPUT:  ibuf    I*2  D60  -  output buffer containing the
c                                 full information stored in key.
c
c****************************************************************
c
      subroutine getkey (ibuf)
c
      integer*2 ibuf(60)
      integer*2 sk_command
      integer*4 i
      character*10 commd
c
c...Read key to the buffer (i*2)
c
      commd(1:2) = 'R/'
      commd(5:) =  char(0)
      do 205 i=1,60
	  write (commd(3:4),'(i2)') i-1
	  ibuf(i) = sk_command (commd)
  205 continue
c
      return
      end
