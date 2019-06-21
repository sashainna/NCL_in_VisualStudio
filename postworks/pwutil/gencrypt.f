c
c***********************************************************************
c
c   FILE NAME: gencrypt.for
c   CONTAINS:
c               gencrypt
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        gencrypt.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:31
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM:  gencrypt
c
c   FUNCTION:  This routine encrypts the text string entered by user
c              and includes this string to the posterror.txt file at
c              LOGG1 label.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     program gencrypt
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      subroutine gencrypt(opmsg,nca,crypt_err)
C WNT-END
c
      include 'menu.inc'
c
      integer*4 jlevl(10),irec,irecl,ierr,nca,ityp,i,inc,nc,
     2          strlen1,lbrk,ivnc,irnc,nrc,nc1,nc2,flag,crypt_err
c
      character*5 copyrh
      character*20 att(4)
      character*80 opmsg,msg,ldat,vmsg,rmsg
      character*100 buf,crmsg,carry(600)
      character*256 msg1
      character*(MAX_PATH) fnam,fnamo
c
      data copyrh /'LOGG1'/

      flag = 0
      crypt_err = 0
c
c...Initialize program
c
      call init
c
c...Initialize level structure
c
      snlevl = 1
      nlevl  = 0
      iprmix = 2
      imenix = 3
      lbrk   = 0
      irec   = 0
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
c...Open screen for reading & writing
c
C     CURSES = 0
C     call trmatt (CURSES)
c
c...Get input text filename
c
C     call getmcr (fnam,nca)
C     if (nca .eq. 0) then
C         ldat   = 'Enter text message: '
C         nc     = strlen1(ldat) + 1
C         call dmpbuf (ldat,nc)
C         nca    = 0
C         call getlin (opmsg,nca,80,-1,nc+2)
C         call trmnl (1)
C         if (nca .eq. -1) go to 8000
C     endif
C     call trmrst
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c...Open text file
c
      fnam = 'posterror'
      call fparse (fnam,fnamo,' ','.txt')
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 100
      call opnfil (LUNSC4,fnamo,att,irecl,msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Rewrite input file to temporary file
c
  100 irec   = irec + 1
      call rdtxt (LUNSC4,carry(irec),msg,ierr)
      if (ierr .eq. 1) go to 200
      if (ierr .ne. 0) go to 9000
      go to 100
c
  200 call clsfil (LUNSC4)
      att(4) = 'new'
      call opnfil (LUNSC2,fnamo,att,irecl,msg,ierr)
      if (ierr .ne. 0) go to 9000
      rewind(LUNSC2)
      nrc   = 0
      irec  = irec - 1
c
c...Get Text Record
c
  500 nrc   = nrc + 1
      if (nrc .gt. irec) go to 8100
      buf   = carry(nrc)
c
c...Parse record
c
      call pparse (buf,jlevl,snlevl,ityp,ldat,msg,ierr)
      if (ierr .ne. 0) go to 9200
      if (ityp .eq. 0) go to 500
c
c...Standalone prompt record
c
      if (ityp .eq. 4) then
          nc     = strlen1(msg)
          if (msg .eq. copyrh) then
             lbrk = 1
             go to 2000
          endif
      endif
      nc     = strlen1(buf)
      call wrtxt (LUNSC2,buf,nc,msg,ierr)
      go to 500
c
c...Update record with encrypted text
c
 2000 call crptmsg (nca,opmsg,crmsg)
      buf = copyrh // '$	' // crmsg(1:nca)
      nc      = nca + 7
      call wrtxt (LUNSC2,buf,nc,msg,ierr)
      if (ierr .ne. 0) go to 9000
c
      if (lbrk .eq. 1) then
         lbrk = 2
         go to 500
      end if
c
c...End of routine
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C8000 call trmrst
C     call clsfil (LUNSC2)
C     call exit
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 8000 call clsfil (LUNSC2)
      RETURN
C WNT-END
c
c...End of file on text file
c
 8100 if (lbrk .eq. 2) go to 8000
      lbrk  =  2
      go to 2000
c
c...An error occurred during
c...execution of program
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C9000 call trmmsg (' ')
C     call trmmsg (msg)
C     if (ierr .lt. 0) then
C         call errhnd (vmsg,ivnc,rmsg,irnc)
C         if (ivnc .ne. 0) call trmmsg (vmsg)
C         if (irnc .ne. 0) call trmmsg (rmsg)
C     endif
C     go to 8000
c
c...Invalid prompt record
c
C9200 call trmmsg (' ')
C     call trmmsg ('*FATAL*  Invalid prompt record.')
C     call trmmsg (msg)
C     call trmmsg (buf)
C     go to 8000
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 9000 nc = strlen1(msg)
      call add1dispmsg (msg, nc, flag)
      crypt_err = 1
      go to 8000
 9200 nc1 = strlen(buf)
      nc2 = strlen(msg)
      msg1 = '*FATAL*  Invalid prompt record: ' // buf (1:nc1)
     x         // msg(1:nc2)
      nc = strlen(msg)
      call add1dispmsg (msg, nc, flag)
      crypt_err = 1
      go to 8000
C WNT-END
      end
