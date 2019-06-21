c
c***********************************************************************
c
c   FILE NAME: genxfer.for
c   CONTAINS:
c               genxfer  bintxt  txtbin
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        genxfer.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:31
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM:  genxfer
c
c   FUNCTION:  This routine converts both an input binary Machine
c              Descriptor File (MDF) to an Ascii (MDA) file and vice
c              versa.  If the input file extension is '.MDF', then a
c              binary to text conversion will take place.  If the input
c              file extension is '.MDA' then a text to binary conversion
c              will take place.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      program genxfer
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      subroutine genxfer(fnam)
      include "postworks_nt.inc"
C WNT-END
c
      include 'menu.inc'
c
      integer*4 irecl,ierr,nc,nca,nc1,ivnc,irnc,strlen1,inum
c
      integer*4 flag
      integer*4 xfer_err,batch
      character*20 att(4),lnum
      character*80 msg,msg1,vmsg,rmsg,ldat
      character*(MAX_PATH) fnam,ldev
      character*(MAX_FILE) lfil,lext
c
      character*2 nln
      byte nlnb(2)
      equivalence (nln, nlnb)
c
c...Initialize routine
c
      call gxfer_version
      call init
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
c...Open screen
c
C      CURSES = 0
C      call trmatt (CURSES)
c
c...Get input filename
c
C      call getmcr (fnam,nca)
C      if (nca .eq. 0) then
C          ldat   = 'Enter filename: '
C          nc     = strlen1(ldat) + 1
C          call dmpbuf (ldat,nc)
C          nca    = 0
C          call getlin (fnam,nca,80,-1,nc+2)
C          call trmnl (1)
C          if (nca .eq. -1) go to 8000
C      endif
C      call trmrst
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      xfer_err = 0
      flag = 1
C WNT-START
c
c...GenXfer start
c
c
c...if running batch, don't display those message
c
      flag = 1;
      call ifrunbatch(batch);
      if (batch.eq.0) then
          nlnb(1) = 13
          nlnb(2) = 10
          call shfile (fnam,msg1,60)
          msg(1:80) = nln(1:2) // 'Genxfer - ' // msg1
          nc = strlen1 (msg)
          call add1dispmsg (msg, nc, flag)
      endif
C WNT-END
c...Determine if binary or text file
c
      call fbreak (fnam,ldev,lfil,lext)
      call touppr (lext,lext)
c
c...Convert binary file to text file
c
      if (lext(1:4) .eq. '.MDF') then
c
c.....Open input binary file
c
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'old'
          irecl  = 8000
          nc     = strlen1(lfil)
          call ctoi (lfil(1:nc),inum,ierr)
          if (ierr .ne. 1 .and. inum .ge. 0) then
              call itoc (inum,lnum,nc,0)
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-START
              lfil   = 'PWORKS_' // lnum(1:nc)
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C             lfil   = 'P' // lnum(1:nc)
C DOS-END
              nc     = strlen1(lfil)
              if (ldev .eq. ' ') then
                  fnam   = lfil(1:nc) // '.MDF'
              else
                  nc1    = strlen1(ldev)
                  fnam   = ldev(1:nc1) // lfil(1:nc) // '.MDF'
              endif
          endif
          call opnfil (LUNSC1,fnam,att,irecl,msg,ierr)
          if (ierr .ne. 0) then
              nc = strlen1(msg)
 		    go to 9000
          endif
c
c......Open output text file
c
          call fparse (lfil,fnam,ldev,'.mda')
          att(1) = 'sequential'
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'new'
          irecl  = 80
          call opnfil (LUNSC2,fnam,att,irecl,msg,ierr)
          if (ierr .ne. 0) then
              nc = strlen1(msg)
 		    go to 9000
          endif
c
c......Convert binary file to text file
c
         call bintxt (msg,ierr)
         if (ierr .ne. 0) then
              nc = strlen1(msg)
 		    go to 9000
         endif
c
c...Convert text file to binary file
c
      else if (lext(1:4) .eq. '.MDA') then
c
c.....Open input text file
c
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-START
          att(1) = 'sequential'
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C         att(1) = 'transparent'
C DOS-END
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'old'
          irecl  = 80
          nc     = strlen1(lfil)
          call ctoi (lfil(1:nc),inum,ierr)
          if (ierr .ne. 1 .and. inum .ge. 0) then
              call itoc (inum,lnum,nc,0)
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-START
              lfil   = 'PWORKS_' // lnum(1:nc)
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C             lfil   = 'P' // lnum(1:nc)
C DOS-END
              nc     = strlen1(lfil)
c...changed because ldev may = '    ' but will not equel ' '
c...use non-blank length
c...Yurong
			nc1    = strlen1(ldev)
			if (nc1 .eq.0) then
c...orig code is
c...              if (ldev .eq. ' ') then
                  fnam   = lfil(1:nc) // '.mda'
              else
c...				nc1    = strlen1(ldev)
                  fnam   = ldev(1:nc1) // lfil(1:nc) // '.mda'
              endif
          endif
          call opnfil (LUNSC1,fnam,att,irecl,msg,ierr)
          nc = strlen1(msg)
          if (ierr .ne. 0) then
              nc = strlen1(msg)
 		    go to 9000
          endif
c
c......Open output binary file
c
          call touppr (lfil,lfil)
          call fparse (lfil,fnam,ldev,'.MDF')
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'new'
          irecl  = 8000
          call opnfil (LUNSC2,fnam,att,irecl,msg,ierr)
          if (ierr .ne. 0) then
              nc = strlen1(msg)
 		    go to 9000
          endif
c
c......Convert text file to binary file
c
          call txtbin (msg,ierr)
          if (ierr .ne. 0) then
              nc = strlen1(msg)
 		    go to 9000
          endif
c
c...Unrecognized file extension
c
      else
           ierr   = 1
           nc     = strlen1(lext)
           msg    = 'Unrecognized file extension' // lext(1:nc)
           nc = nc + 28
           go to 9000
      endif
c
c...End of routine
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C8000 call trmrst
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C DOS-START
C     if (lext .eq. 'MDF') endfile (LUNSC2)
C DOS-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call clsfil (0)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 8000 call clsfil (0)
      flag = 1
      if ((xfer_err .eq. 0).and.(batch.eq.0))  then
          call shfile (fnam,msg1,60)
          len = len_trim(msg1)
          msg = "Genxfer - "// msg1(1:len)//" completed"
          nc = len + 20
 	call add1dispmsg(msg, nc, flag)
      endif
      return
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c...only not for WNT
C      call exit
c
c...An error occurred during
c...execution of program
c
c...only not for WNT
C9000 call trmmsg (' ')
C      call trmmsg (msg)
C      if (ierr .lt. 0) then
C          call errhnd (vmsg,ivnc,rmsg,irnc)
C          if (ivnc .ne. 0) call trmmsg (vmsg)
C          if (irnc .ne. 0) call trmmsg (rmsg)
C      endif
C      go to 8000
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 9000 flag = 1
      call add1dispmsg (msg, nc, flag)
c...      if (ierr .lt. 0) then
c...          call errhnd (vmsg,ivnc,rmsg,irnc)
c...          if (ivnc .ne. 0) call add1dispmsg (vmsg, ivnc, flag)
c...          if (irnc .ne. 0) call add1dispmsg (rmsg, irnc, flag)
c...      endif
      xfer_err = 1
      go to 8000
C WNT-END
      end

c***********************************************************************
c
c   SUBROUTINE:  bintxt (cmsg,kerr)
c
c   FUNCTION:  This routine performs the binary to text conversion for
c              the Post comman arrays.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Returns the text of the error message
c                               when an error occurred.
c
c           kerr    I*4  D1  -  Returns 1 for an invalid character
c                               string.
c
c***********************************************************************
c
      subroutine bintxt (cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (FMTDES,KPOSMP(2133))
      equivalence (NKPOSM,KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM,KPOSMP(0173))
c
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (MPOSMP,CPOSMP(0001))
c
      integer*4 MPOSMP(2000),NKPOSM,NPOSMA,NCPOSM
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 iswp
      integer*4 i,j,inc,nc,nci,ncc,iend,inum,icnt,mkp,mpo,mcp,is
c
      character*1 lc
      character*80 sbuf,ldat,lnum,lcnt
c
      real*8 rnum
c
      mkp    = 2
      mpo    = 4
      mcp    = 1
c
c...Load the first I*4 record
c
      call rdcom (LUNSC1,1,KPOSMP(1),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Check version of Common arrays
c
      if (NKPOSM .gt. mkp) mkp = NKPOSM
      if (NPOSMA .gt. mpo) mpo = NPOSMA
      if (NCPOSM .gt. mcp) mcp = NCPOSM
c
      do 205 i=2,mkp
          call rdcom (LUNSC1,i,KPOSMP((i-1)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  205 continue
      is     = mkp + 1
      do 215 i=is,mkp+mcp
          call rdcom (LUNSC1,i,MPOSMP((i-is)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  215 continue
      is     = is + mcp
      do 300 i=is,mkp+mcp+mpo,1
          call rdcom (LUNSC1,i,POSMAP((i-is)*1000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  300 continue
c
c...Output as extended size
c
      mkp    = 3
      mpo    = 6
      NKPOSM = mkp
      NCPOSM = mcp
      NPOSMA = mpo
c
c...Write out KPOSMP data
c
      nc     = 0
      ldat   = ' '
      iend   = 2000*mkp + 1
      icnt   = 0
c
c.........Swap integer*2 variables
c
C SUN-SGI-IBM-HPX-START
C      do 700 i=1,MAXFMT,1
C          do 600 j=1,10,2
C              iswp  = FMTDES(j,i)
C              FMTDES(j,i) = FMTDES(j+1,i)
C              FMTDES(j+1,i) = iswp
C 600     continue
C 700 continue
C SUN-SGI-IBM-HPX-END
      do 1000 i=1,iend,1
c
c......Number is same as previous
c......Increment count
c
          if (i .ne. iend .and. (KPOSMP(i) .eq. inum .or. i .eq. 1))
     1            then
              icnt   = icnt   + 1
              inum   = KPOSMP(i)
c
c.......Number is different
c.......Output number
c
          else
              call itoc (inum,lnum,nci,0)
c
c.........Multiple values that are the same
c.........Add 'n*' to number
c
              if (icnt .gt. 1) then
                  call itoc (icnt,lcnt,ncc,0)
                  sbuf   = lcnt(1:ncc) // '*' // lnum(1:nci)
                  lnum   = sbuf
                  nci    = ncc    + 1 + nci
              endif
c
c.........Buffer is full, output it
c
              if (nc+nci+1 .gt. 80) then
                  call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  ldat   = lnum(1:nci) // ','
                  nc     = nci    + 1
c
c.........Append new number to output buffer
c
              else
                  sbuf   = ldat(1:nc) // lnum(1:nci) // ','
                  ldat   = sbuf
                  nc     = nc     + nci    + 1
              endif
c
c.........Store current value
c
              if (i .ne. iend) then
                  inum   = KPOSMP(i)
                  icnt   = 1
c
c.........End of array, output buffer
c
              else
                  if (nc .ne. 0) call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
          endif
 1000 continue
c
c...Write out POSMAP data
c
      nc     = 0
      ldat   = ' '
      iend   = 1000*mpo + 1
      icnt   = 0
      do 2000 i=1,iend,1
c
c......Number is same as previous
c......Increment count
c
          if (i .ne. iend .and. (POSMAP(i) .eq. rnum .or. i .eq. 1))
     1            then
              icnt   = icnt   + 1
              rnum   = POSMAP(i)
c
c.......Number is different
c.......Output number
c
          else
              call rtoc (rnum,lnum,nci)
c
c.........Multiple values that are the same
c.........Add 'n*' to number
c
              if (icnt .gt. 1) then
                  call itoc (icnt,lcnt,ncc,0)
                  sbuf   = lcnt(1:ncc) // '*' // lnum(1:nci)
                  lnum   = sbuf
                  nci    = ncc    + 1 + nci
              endif
c
c.........Buffer is full, output it
c
              if (nc+nci+1 .gt. 80) then
                  call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  ldat   = lnum(1:nci) // ','
                  nc     = nci    + 1
c
c.........Append new number to output buffer
c
              else
                  sbuf   = ldat(1:nc) // lnum(1:nci) // ','
                  ldat   = sbuf
                  nc     = nc     + nci    + 1
              endif
c
c.........Store current value
c
              if (i .ne. iend) then
                  rnum   = POSMAP(i)
                  icnt   = 1
c
c.........End of array, output buffer
c
              else
                  if (nc .ne. 0) call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
          endif
 2000 continue
c
c...Write out CPOSMP data
c
      nc     = 0
      ldat   = ' '
      iend   = 8000*mcp + 1
      icnt   = 0
      do 3000 i=1,iend,1
c
c......Character is same as previous
c......Increment count
c
          if (CPOSMP(i) .eq. char(0)) CPOSMP(i) = ' '
          if (i .ne. iend .and. (CPOSMP(i) .eq. lc .or. i .eq. 1))
     1            then
              icnt   = icnt   + 1
              lc     = CPOSMP(i)
c
c.......Number is different
c.......Output number
c
          else
c
c.........Multiple characters that are the same
c.........Add 'n*' to number
c
              if (icnt .gt. 1) then
                  call itoc (icnt,lcnt,ncc,0)
                  nc     = nc     + 1
                  ldat(nc:nc) = char(11)
                  lnum   = lcnt(1:ncc) // '*' // lc
                  nci    = ncc     + 2
c
c............Buffer is full, output it
c
                  if (nc+nci .gt. 80) then
                      call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
                      if (kerr .ne. 0) go to 8000
                      nc     = 0
                  endif
c
c............Append multiple char to string
c
                  sbuf   = ldat(1:nc) // lnum(1:nci)
                  ldat   = sbuf
                  nc     = nc     + nci
c
c............Buffer is full, output it
c
                  if (nc .eq. 80) then
                      call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
                      if (kerr .ne. 0) go to 8000
                      nc     = 0
                  endif
c
c.........Append character to string
c
              else
                   nc     = nc     + 1
                   ldat(nc:nc) = lc
c
c............Buffer is full, output it
c
                  if (nc .eq. 80) then
                      call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
                      if (kerr .ne. 0) go to 8000
                      nc     = 0
                  endif
              endif
c
c.........Store current character
c
              if (i .ne. iend) then
                  lc     = CPOSMP(i)
                  icnt   = 1
c
c.........End of array, output buffer
c
              else
                  if (nc .ne. 0) call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
          endif
 3000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  txtbin (cmsg,kerr)
c
c   FUNCTION:  This routine performs the text to binary conversion for
c              the Post comman arrays.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Returns the text of the error message
c                               when an error occurred.
c
c           kerr    I*4  D1  -  Returns 1 for an invalid character
c                               string.
c
c***********************************************************************
c
      subroutine txtbin (cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NKPOSM,KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM,KPOSMP(0173))
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (MPOSMP,CPOSMP(0001))
c
      integer*4 MPOSMP(2000),NKPOSM,NPOSMA,NCPOSM
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 iswp
      integer*4 i,j,inc,nc,iend,inum,icnt,ist,ien,ipt,index
c
      character*80 ldat
c
      real*8 rnum
c
c...Read in KPOSMP data
c
      ipt    = 0
      iend   = 4000
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-START
  100 call rdtxt (LUNSC1,ldat,cmsg,kerr)
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C 100 call drtxt (LUNSC1,ldat,cmsg,kerr)
C DOS-END
      if (kerr .ne. 0) go to 8000
      ist    = 1
      nc     = 80
c
c......Get next parameter
c
  200 if (ist .gt. nc) go to 100
      ien    = index(ldat(ist:nc),',')
      if (ien .eq. 0) go to 100
      ien    = ist    + ien    - 2
      if (ien .lt. ist) go to 9000
c
c.........Check for multiple values 'n*n'
c
      inc    = index(ldat(ist:ien),'*')
      if (inc .ne. 0) then
          call ctoi (ldat(ist:ist+inc-2),icnt,kerr)
          if (kerr .ne. 0) go to 9000
          ist    = ist    + inc
      else
          icnt   = 1
      endif
c
c.........Parse value
c
      call ctoi (ldat(ist:ien),inum,kerr)
      if (kerr .ne. 0) go to 9000
c
c.........Store values
c
      if (ipt+icnt .gt. iend) go to 9000
      do 500 i=1,icnt,1
          ipt    = ipt    + 1
          KPOSMP(ipt) = inum
  500 continue
      ist    = ien    + 2
      if (ipt .gt. 173) then
          if (NKPOSM .gt. 1 .and. NKPOSM .lt. 7) iend = NKPOSM * 2000
      end if
      if (ipt .lt. iend) go to 200
c
c.........Swap integer*2 variables
c
C SUN-SGI-IBM-HPX-START
C      do 700 i=1,MAXFMT,1
C          do 600 j=1,10,2
C              iswp  = FMTDES(j,i)
C              FMTDES(j,i) = FMTDES(j+1,i)
C              FMTDES(j+1,i) = iswp
C 600     continue
C 700 continue
C SUN-SGI-IBM-HPX-END
c
c...Read in POSMAP data
c
      ipt    = 0
      iend   = 4000
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-START
 1100 call rdtxt (LUNSC1,ldat,cmsg,kerr)
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C1100 call drtxt (LUNSC1,ldat,cmsg,kerr)
C DOS-END
      if (kerr .ne. 0) go to 8000
      ist    = 1
      nc     = 80
c
c......Get next parameter
c
 1200 if (ist .gt. nc) go to 1100
      ien    = index(ldat(ist:nc),',')
      if (ien .eq. 0) go to 1100
      ien    = ist    + ien    - 2
      if (ien .lt. ist) go to 9000
c
c.........Check for multiple values 'n*n'
c
      inc    = index(ldat(ist:ien),'*')
      if (inc .ne. 0) then
          call ctoi (ldat(ist:ist+inc-2),icnt,kerr)
          if (kerr .ne. 0) go to 9000
          ist    = ist    + inc
      else
          icnt   = 1
      endif
c
c.........Parse value
c
      call ctor (ldat(ist:ien),rnum,kerr)
      if (kerr .ne. 0) go to 9000
c
c.........Store values
c
      if (ipt+icnt .gt. iend) go to 9000
      do 1500 i=1,icnt,1
          ipt    = ipt    + 1
          POSMAP(ipt) = rnum
 1500 continue
      ist    = ien    + 2
      if (ipt .gt. 173) then
          if (NPOSMA .gt. 3) iend = NPOSMA * 1000
      end if
      if (ipt .lt. iend) go to 1200
c
c...Read in CPOSMP data
c
      ipt    = 0
      iend   = 8000
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-START
 2100 call rdtxt (LUNSC1,ldat,cmsg,kerr)
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C2100 call drtxt (LUNSC1,ldat,cmsg,kerr)
C DOS-END
      if (kerr .ne. 0) go to 8000
      ist    = 1
      nc     = 80
c
c......Get next parameter
c
 2200 if (ist .gt. nc) go to 2100
      ien    = index(ldat(ist:nc),char(11))
      if (ien .eq. 0) ien = index(ldat(ist:nc),char(128))
      ien    = ist    + ien    - 1
c
c.........Check for multiple characters 'n*n'
c
      if (ien .eq. ist) then
          inc    = index(ldat(ist+1:nc),'*')
          if (inc .eq. 0) then
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-START
              call rdtxt (LUNSC1,ldat,cmsg,kerr)
C WNT-VAX-SUN-SGI-IBM-HPX-DEC-END
C DOS-START
C             call drtxt (LUNSC1,ldat,cmsg,kerr)
C DOS-END
              if (kerr .ne. 0) go to 8000
              ist    = 1
              nc     = 80
              inc    = index(ldat(ist:nc),'*')
              if (inc .eq. 0) go to 9000
          else
              ist    = ist    + 1
          endif
          call ctoi (ldat(ist:ist+inc-2),icnt,kerr)
          if (kerr .ne. 0) go to 9000
          ist    = ist    + inc
      else
          icnt   = 1
      endif
c
c.........Store character(s)
c
      if (ipt+icnt .gt. iend) go to 9000
      do 2500 i=1,icnt,1
          ipt    = ipt    + 1
          CPOSMP(ipt) = ldat(ist:ist)
 2500 continue
      ist    = ist    + 1
      if (ipt .gt. 173) then
          if (NCPOSM .gt. 0 .and. NCPOSM .lt. 7) iend = NCPOSM * 8000
      end if
      if (ipt .lt. iend) go to 2200
c
c...Store the Common arrays
c
      NKPOSM = 3
      NCPOSM = 1
      NPOSMA = 6
      do 3205 i=1,NKPOSM
          call wrcom (LUNSC2,i,KPOSMP((i-1)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
 3205 continue
      inc    = NKPOSM + 1
      do 3215 i=inc,NKPOSM+NCPOSM
          call wrcom (LUNSC2,i,MPOSMP((i-inc)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
 3215 continue
      inc    = inc + NCPOSM
      do 3300 i=inc,NKPOSM+NCPOSM+NPOSMA,1
          call wrcom (LUNSC2,i,POSMAP((i-inc)*1000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
 3300 continue
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 cmsg = '*ERROR* Parsing ".mda" file.  Text = ' // ldat(ist:ien)
      go to 8000
      end
