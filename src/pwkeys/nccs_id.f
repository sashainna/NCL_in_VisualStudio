c****************************************************************
c
c   FILE NAME:  nccs_id.for
c
c   CONTAINS:  nccs_id  getkey
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccs_id.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:09
c
c****************************************************************
c
c****************************************************************
c
c   PROGRAM:  nccs_id
c
c   FUNCTION:  This utility is used to read the SCRIBE key
c              device
c
c****************************************************************
c
      program nccs_id
c
C SUN-SGI-IBM-HPX-VAX-START
c
C     integer*4 nc,ierr
c
C     character*20 ldat
c
c...Open the user's terminal
c
C SUN-SGI-IBM-HPX-END
C     open (unit=6,name='TT:',type='old',carriagecontrol='list',
C    1      err=9000)
C VAX-END
C SUN-SGI-IBM-HPX-VAX-START
c
c...Get the Host System ID
c
C     call pwdsid (ldat,nc,0,ierr)
C     if (ierr .ne. 0) go to 9000
C     write (6,10) ldat(1:nc)
C  10 format ('System ID = ',a)
c
c...End of routine
c
C8000 continue
C SUN-SGI-IBM-HPX-END
C     call exit
C VAX-END
C SUN-SGI-IBM-HPX-START
C     stop
C SUN-SGI-IBM-HPX-END
C SUN-SGI-IBM-HPX-VAX-START
c
c...Could not obtain the System ID
c
C9000 write (6,9001)
C9001 format ('*FATAL*  Could not obtain the System ID.')
C     go to 8000
C SUN-SGI-IBM-HPX-VAX-END
      end
C DOS-START
C     integer*2 istat,sk_command
c
C     character*40 commd
C     character*18 pasw1
C     character*120 cscbf
c
C     integer*2 iscbf(60)
C     integer*4 idnum
c
C     equivalence (iscbf,cscbf), (iscbf(59),idnum)
C
C     data pasw1 /'E/mqJTkdevdxEbhyZh'/
c
c...Initialize SCRIBE device
c
C     commd  = 'I/1/0' // char(0)
C     istat  = sk_command (commd)
C     if (istat .ne. 0) go to 8100
C
c...Enable read only the key
c
C     commd  = pasw1 // char(0)
C     istat  = sk_command (commd)
C     if (istat .ne. 0) go to 8200
c
c...Read the key
c
C     call getkey (iscbf)
C     write (6,10)
C  10 format (' The key device contents is:',/)
C     write (6,51) cscbf(1:35)
C     write (6,51) cscbf(36:89)
C     write (6,*) 'Host ID:',idnum
c
C  51 format (' ',A)
C     go to 9000
c
C8100 write (6,*) ' * Error *  Key not initialized  - ',istat
C     stop
C8200 write (6,*) ' * Error *  Wrong key passwords  - ',istat
C     stop
c
c...End of program
c
C9000 stop
C     end
c
C****************************************************************
C
C   SUBROUTINE:  getkey (ibuf)
c
C   FUNCTION:  This routine is used to read the contents of the
C              key device.
c
C   INPUT:  none
c
C   OUTPUT:  ibuf    I*2  D60  -  output buffer containing the
C                                 full information stored in key.
c
C****************************************************************
c
C     subroutine getkey (ibuf)
c
C     integer*2 ibuf(60)
C     integer*2 sk_command
C     integer*4 i
C     character*10 commd
C
c...Read key to the buffer (i*2)
c
C     commd(1:2) = 'R/'
C     commd(5:) =  char(0)
C     do 205 i=1,60
C         write (commd(3:4),'(i2)') i-1
C         ibuf(i) = sk_command (commd)
C 205 continue
c
C     return
C     end
C DOS-END
