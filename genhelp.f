c
c***********************************************************************
c
c   FILE NAME: genhelp.for
c   CONTAINS:
c               genhelp
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        genhelp.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:31
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM:  genhelp
c
c   FUNCTION:  This routine converts an input text file into a binary
c              Help file, with indices, that can be referenced by inter-
c              active programs such as MakePost.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      program genhelp
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      subroutine genhelp(fnam)
      include "postworks_nt.inc"
C WNT-END
c
      include 'menu.inc'
c
      integer*4 irecl,ierr,irec,nrec,indx(128),ipx,nc,strlen1,itxtpt,
     1          ltxtpt,itrec,ie,ilevl(10),idxp(2000),idxt(2000),i,
     2          iswapi,isw,lstix,inc,ix,ipt,nca,ivnc,irnc,lpt
c
      character*(MAX_PATH) fnam
c
      integer*4 help_err, len, flag,batch
      character*20 att(4)
      character*32 lidx(2000),swapw
      character*80 msg,buf,vmsg,rmsg,ldat, msg1
      character*512 lindx
      character*(MAX_PATH) fnamo
c
      character*2 nln
      byte nlnb(2)
      equivalence (nln, nlnb)
c
      equivalence (indx,lindx)
c
c...Initialize routine
c
      call ghelp_version
      call init
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
c
c...Open screen for reading & writing
c
C      CURSES = 0
C      call trmatt (CURSES)
c
c...Get input text filename
c
C      call getmcr (fnam,nca)
C      if (nca .eq. 0) then
C          ldat   = 'Enter text filename: '
C          nc     = strlen1(ldat) + 1
C          call dmpbuf (ldat,nc)
C          nca    = 0
C          call getlin (fnam,nca,80,-1,nc+2)
C          call trmnl (1)
C          if (nca .eq. -1) go to 8000
C      endif
C      call trmrst
c
c...Open text file
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      help_err = 0
      flag = 1
C WNT-START
c
c...GenHelp start
c
c...if running batch, don't display those message
c
      flag = 1;
      call ifrunbatch(batch);
      if (batch.eq.0) then
          nlnb(1) = 13
          nlnb(2) = 10
          call shfile (fnam,buf,60)
          msg(1:80) = nln(1:2) // 'Genhelp - ' // buf
          nc = strlen1 (msg)
          call add1dispmsg (msg, nc, flag)
      endif
C WNT-END
      call fparse (fnam,fnamo,' ','.txt')
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      call opnfil (LUNSC1,fnamo,att,irecl,msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Ouput help file
c
      call adftyp (fnamo,fnam,'.HLP')
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'new'
      irecl  = 512
      call opnfil (LUNSC2,fnam,att,irecl,msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Initialize help file
c
      ipx    = 0
      indx(1) = 0
      indx(2) = 0
      do 450 i=1,36,1
          call wrprm (LUNSC2,i,indx,msg,ierr)
          if (ierr .ne. 0) go to 8000
  450 continue
      irec   = nrec   - 1
      itxtpt = 2
      ltxtpt = 9
      itrec  = 37
      nrec   = 38
c
c...Read text record
c
  500 call rdtxt (LUNSC1,buf,msg,ierr)
      if (ierr .eq. 1) go to 750
      if (ierr .ne. 0) go to 9000
      nc     = strlen1(buf)
      if (nc .eq. 0) nc = 1
c
c...Check for index record
c
      if (buf(1:1) .eq. '~') go to 600
c
c...Text record
c
c......Record is full write current record &
c......Allocate another record
c
      if (ltxtpt+nc+4 .gt. 512) then
          indx(itxtpt) = -1
          indx(1) = nrec
          nrec   = nrec   + 1
          call wrprm (LUNSC2,itrec,indx,msg,ierr)
          if (ierr .ne. 0) go to 8000
          itrec   = indx(1)
          indx(1) = 0
          itxtpt = 2
          ltxtpt = 9
      endif
c
c......Store text data
c
      indx(itxtpt) = nc
      ie     = ltxtpt + nc     - 1
      lindx(ltxtpt:ie) = buf(1:nc)
      itxtpt = itxtpt + ((nc-1) / 4 + 2)
      ltxtpt = (itxtpt-1) * 4 + 5
      go to 500
c
c...Index Record
c
c......Check for valid index
c
  600 call getlvl (buf(2:nc-1),nc-1,ilevl,nlevl,ierr)
      if (ierr .ne. 0) go to 9200
c
c......Set "End-of-Text" mark
c......For previous help text
c
      if (ipx .ne. 0) then
          indx(itxtpt) = 0
          itxtpt = itxtpt + 1
          ltxtpt = ltxtpt + 4
c
c.........Record is full write current record &
c.........Allocate another record
c
          if (ltxtpt .gt. 512) then
C              indx(itxtpt) = -1
              indx(1) = nrec
              nrec   = nrec   + 1
              call wrprm (LUNSC2,itrec,indx,msg,ierr)
              if (ierr .ne. 0) go to 8000
              itrec   = indx(1)
              indx(1) = 0
              itxtpt = 2
              ltxtpt = 9
          endif
      endif
c
c......Store index data
c
      ipx    = ipx    + 1
      lidx(ipx) = buf(2:33)
      idxp(ipx) = itrec
      idxt(ipx) = itxtpt
      go to 500
c
c...End of input file
c
c......Set "End-of-Text" mark
c......For previous help text
c
  750 if (ipx .ne. 0) then
          indx(itxtpt) = 0
          itxtpt = itxtpt + 1
          ltxtpt = ltxtpt + 4
C
C          indx(itxtpt) = -1
          indx(1) = nrec
          nrec   = nrec   + 1
          call wrprm (LUNSC2,itrec,indx,msg,ierr)
          if (ierr .ne. 0) go to 8000
          itrec   = indx(1)
      endif
      go to 8050
c
c...End of program
c	
C WNT-START
 8000 call clsfil (0)
      if ((help_err .eq. 0).and.(batch.eq.0)) then
          call shfile (fnam,msg1,60)
          len = len_trim(msg1)
          msg = "Genhelp - "// msg1(1:len)//" completed"
          nc = strlen1(msg)
          call add1dispmsg (msg, nc, flag)
      endif
      return
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C8000 call trmrst
C      call clsfil (0)
C      call exit
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c......Sort indices
c
 8050 isw    = 0
      do 8100 i=1,ipx-1,1
          if (lidx(i) .gt. lidx(i+1)) then
              swapw  = lidx(i)
              lidx(i) = lidx(i+1)
              lidx(i+1) = swapw
c
              iswapi = idxp(i)
              idxp(i) = idxp(i+1)
              idxp(i+1) = iswapi
c
              iswapi = idxt(i)
              idxt(i) = idxt(i+1)
              idxt(i+1) = iswapi
c
              isw    = 1
          endif
 8100 continue
      if (isw .eq. 1) go to 8050
c
c......Store indices
c
      irec   = 1
      lstix  = 1
      indx(1) = 0
      inc    = 0
      do 8400 i=1,ipx,1
          call getlvl (lidx(i),strlen1(lidx(i)),ilevl,nlevl,ierr)
          ix     = ilevl(1)
          if (ix .eq. 0) ix = 1
          if (ix .gt. 36) ix = 36
c
c.........New index record
c.........Save last record
c
          if (ix .ne. lstix) then
              indx(1) = 0
              indx(2) = inc
              call wrprm (LUNSC2,irec,indx,msg,ierr)
              if (ierr .ne. 0) go to 8000
              irec   = ix
              lstix  = ix
              indx(1) = 0
              inc    = 0
          endif
c
c.........Current record is full
c.........Allocate another record
c
          if (inc .eq. 12) then
              indx(1) = nrec
              indx(2) = inc
              call wrprm (LUNSC2,irec,indx,msg,ierr)
              if (ierr .ne. 0) go to 8000
              irec   = nrec
              nrec   = nrec   + 1
              inc    = 0
          endif
c
c.........Store index in record
c
          inc    = inc    + 1
          ipt    = (inc-1) * 10 + 3
          lpt    = (ipt-1) * 4 + 9
          indx(ipt) = idxp(i)
          indx(ipt+1) = idxt(i)
          lindx(lpt:lpt+31) = lidx(i)
 8400 continue
c
c.........Store last index
c
      if (inc .ne. 0) then
          indx(1) = 0
          indx(2) = inc
          call wrprm (LUNSC2,irec,indx,msg,ierr)
          if (ierr .ne. 0) go to 8000
      endif
      go to 8000
c
c...An error occurred during
c...execution of program
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C9000 call trmmsg (' ')
C      call trmmsg (msg)
C      if (ierr .lt. 0) then
C          call errhnd(vmsg, ivnc,rmsg,irnc)
C          if (ivnc .ne. 0) call trmmsg (vmsg)
C          if (irnc .ne. 0) call trmmsg (rmsg)
C      endif
C      go to 8000
c
c...Invalid word descriptor
c
C9200 call trmmsg (' ')
C      call trmmsg ('*FATAL*  Invalid help index.')
C      call trmmsg (msg)
C      call trmmsg (buf)
C      go to 8000
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 9000	nc = strlen1(msg)
      call add1dispmsg (msg, nc, flag)
c...      if (ierr .lt. 0) then
c...          call errhnd(vmsg, ivnc,rmsg,irnc)
c...          if (ivnc .ne. 0) call add1dispmsg (vmsg,ivnc, flag)
c...          if (irnc .ne. 0) call add1dispmsg (rmsg,irnc, flag)
c...      endif
      help_err = 1
      goto 8000
c
c...Invalid word descriptor
c
 9200 msg1 = '*FATAL*  Invalid help index.'
      nc = strlen1(msg1)
      call add1dispmsg (msg1,nc, flag)
      nc = strlen1(msg)
      call add1dispmsg (msg, nc, flag)
      nc = strlen1(buf)
      call add1dispmsg (buf, nc, flag)
      help_err = 1
      go to 8000
C WNT-END
      end
