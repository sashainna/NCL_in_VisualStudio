c
c***********************************************************************
c
c   FILE NAME: genword.for
c   CONTAINS:
c               genword  wpars
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        genword.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:31
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM:  genword
c
c   FUNCTION:  This routine converts an input text file containing
c              vocabulary words and values into a binary file with the
c              extension .WRD and that is sorted in alphabetic order.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      program genword
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      subroutine genword(fnam)
      include "postworks_nt.inc"
C WNT-END
c
      include 'menu.inc'
c
      character*(MAX_PATH) fnam
c
      integer*4 irec,iwrdpt,irecl,ierr,iwrd,iword(720),iswapi,idat(128),
     1          i,j,ie,iend,ipt,itxt,nc,nca,ivnc,irnc,strlen1,isw,maxwrd
c
      integer*4 word_err, flag, batch,idid
      real*8 rword(3,720)
c
      character*24 lwrd,lword(720),swapw
      character*20 att(4)
      character*80 msg,buf,vmsg,rmsg,ldat,msg1
      character*512 icdat
      character*(MAX_PATH) fnamo
c
      character*2 nln
      byte nlnb(2)
      equivalence (nln, nlnb)
c
      equivalence (idat,icdat), (rword,lword)
      flag = 1
c
c...Initialize routine
c
      call init
      call gword_version
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
c
c...Open screen
c
C      CURSES = 0
C      call trmatt (CURSES)
c
c...Get input text filename
c
C      call getmcr (fnam,nca)
C	
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
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      iwrdpt = 0
      maxwrd = 720
      word_err = 0
C WNT-START
c
c...GenWord start
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
          msg(1:80) = nln(1:2) // 'Genword - ' // msg1
          nc = strlen1 (msg)
          call add1dispmsg (msg, nc, flag)
      endif
C WNT-END
c
c...Open text file
c
      call fparse (fnam,fnamo,' ','.txt')
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      call opnfil (LUNSC1,fnamo,att,irecl,msg,ierr)
      if (ierr .ne. 0) then
          nc = strlen1(msg)
          go to 9000
      endif
c
c...Open output word file
c
      call adftyp (fnamo,fnam,'.WRD')
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'new'
      irecl  = 512
      call opnfil (2,fnam,att,irecl,msg,ierr)
      if (ierr .ne. 0) then
          nc = strlen1(msg)
          go to 9000
      endif
c
c...Read text record
c
  500 call rdtxt (LUNSC1,buf,msg,ierr)
      if (ierr .eq. 1) go to 8100
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
c
c...Parse record
c
      call wpars (buf,lwrd,iwrd,msg,ierr)
      if (ierr .ne. 0) go to 9200
      if (iwrd .eq. 0) go to 500
c
c...Store word & value in buffer
c
      if (iwrdpt .gt. maxwrd) go to 9100
      iwrdpt = iwrdpt + 1
      lword(iwrdpt) = lwrd
      iword(iwrdpt) = iwrd
      go to 500
c
c...End of routine
c
C WNT-START
 8000 call clsfil (0)
      if ((word_err .eq. 0) .and.(batch.eq.0)) then
          call shfile (fnam,msg1,60)
          len = len_trim(msg1)
          msg = "Genword - "// msg1(1:len)//" completed"
          nc = 20 + len
          call add1dispmsg(msg, nc, flag)
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
c...End of text file
c...Sort words
c
 8100 isw   = 0
      do 8150 i=1,iwrdpt-1,1
          idid = 0
          if (rword(1,i) .gt. rword(1,i+1)) then
              idid = 1
          else if (rword(1,i) .eq. rword(1,i+1)) then
              if (rword(2,i) .gt. rword(2,i+1)) then
                  idid = 1
              else if (rword(2,i) .eq. rword(2,i+1)) then
                  if (rword(3,i) .gt. rword(3,i+1)) idid = 1
              endif
          endif
          if (idid .eq. 1) then
              swapw  = lword(i)
              lword(i) = lword(i+1)
              lword(i+1) = swapw
c
              iswapi = iword(i)
              iword(i) = iword(i+1)
              iword(i+1) = iswapi
c
              isw    = 1
          endif
 8150 continue
      if (isw .eq. 1) go to 8100
c
c...Save word records
c
      idat(128) = iwrdpt
      iend   = iwrdpt
      irec   = 0
      do 8300 i=0,iwrdpt-1,18
          ie     = iend
          if (ie .gt. 18) ie = 18
          iend   = iend   - ie
          ipt    = 7
          itxt   = 1
          do 8200 j=1,ie,1
              icdat(itxt:itxt+23) = lword(i+j)
              idat(ipt) = iword(i+j)
              itxt   = itxt   + 28
              ipt    = ipt    + 7
 8200     continue
          irec   = irec   + 1
          call wrprm (LUNSC2,irec,idat,msg,ierr)
          if (ierr .ne. 0) then
              nc = strlen1(msg)
              go to 9000
          endif
 8300 continue
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
C          call errhnd (vmsg,ivnc,rmsg,irnc)
C          if (ivnc .ne. 0) call trmmsg (vmsg)
C          if (irnc .ne. 0) call trmmsg (rmsg)
C      endif
C      go to 8000
c
c...Too many vocabulary words
c
C9100 write (msg,9101) maxwrd
C9101 format ('*FATAL*  Too many vocabulary words.  Maximum = ',i4,'.')
C      call trmmsg (' ')
C      call trmmsg (msg)
C      go to 8000
c
c...Invalid word descriptor
c
C9200 call trmmsg (' ')
C      call trmmsg ('*FATAL*  Invalid word descriptor.')
C      call trmmsg (msg)
C      call trmmsg (buf)
C      go to 8000
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 9000 call add1dispmsg (msg, nc, flag)
c...      if (ierr .lt. 0) then
c...          call errhnd (vmsg,ivnc,rmsg,irnc)
c...          if (ivnc .ne. 0) call add1dispmsg (vmsg, ivnc, flag)
c...          if (irnc .ne. 0) call add1dispmsg (rmsg, irnc, flag)
c...      endif
      word_err = 1
      go to 8000
c
c...Too many vocabulary words
c
 9100 write (msg,9101) maxwrd
 9101 format ('*FATAL*  Too many vocabulary words.  Maximum = ',i4,'.')
      nc = 54
      call add1dispmsg (msg, nc, flag)
      word_err = 1
      go to 8000
c
c...Invalid word descriptor
c
 9200 flag = 1
      msg1 = '*FATAL*  Invalid word descriptor.'
      nc = strlen1(msg1)
      call add1dispmsg (msg1, nc, flag)
      nc = strlen1 (msg)
      call add1dispmsg (msg, nc, flag)
      nc = strlen1(buf)
      call add1dispmsg (buf, nc, flag)
      word_err = 1
      go to 8000
C WNT-END
      end

c
c***********************************************************************
c
c   SUBROUTINE:  wpars (cbuf,cwrd,kwrd,cmsg,kerr)
c
c   FUNCTION:  This routine parses a character string that should be in
c              the following format:
c
c                  word    MAJOR/MINOR    value
c
c   INPUT:  cbuf    C*n  D1  -  Character string.
c
c   OUTPUT: cwrd    C*n  D1  -  Contains 'word' text.  Can be 1-24 chars
c                               in length.
c
c           kwrd    I*4  D1  -  Returns the value for 'word', negative
c                               if MAJOR and positive if MINOR.
c
c           cmsg    C*n  D1  -  Returns the text of the error message
c                               when an error occurred.
c
c           kerr    I*4  D1  -  Returns 1 for an invalid character
c                               string.
c
c***********************************************************************
c
      subroutine wpars (cbuf,cwrd,kwrd,cmsg,kerr)
c
      integer*4 kwrd,kerr
c
      character*(*) cbuf,cwrd,cmsg
c
      integer*4 nc,ipt,strlen1,jpt
c
      character*5 sbuf
c
      character*1 ctab
      byte tab
c
      equivalence (ctab,tab)
c
      data tab / 9/
c
c...Initialize routine
c
      kerr   = 0
      nc     = strlen1(cbuf)
c
c...Check for blank line
c
      if (nc .eq. 0) then
          kwrd   = 0
          return
      endif
c
c...Get word text
c
      ipt    = index(cbuf,ctab)
      if (ipt .eq. 0) go to 9000
      if (ipt .gt. 25) go to 9100
      call touppr (cbuf(1:ipt-1),cwrd)
c
c...Get MAJOR/MINOR parameter
c
      jpt    = index(cbuf(ipt+1:nc),ctab)
      if (jpt .eq. 0) go to 9000
      if (jpt .ne. 6) go to 9300
      call touppr (cbuf(ipt+1:ipt+5),sbuf)
      if (sbuf .ne. 'MINOR' .and. sbuf .ne. 'MAJOR') go to 9300
c
c...Get word value
c
      call ctoi (cbuf(ipt+jpt+1:nc),kwrd,kerr)
      if (kerr .ne. 0) go to 9200
      if (kwrd .le. 0 .or. kwrd .gt. 9999) go to 9200
      if (sbuf .eq. 'MAJOR') kwrd = kwrd * (0-1)
      return
c
c...Invalid syntax
c
 9000 cmsg   = 'A <TAB> character is required.'
      kerr   = 1
      return
c
c...Too many chars in word text
c
 9100 cmsg   = 'Word names cannot contain more than 24 characters.'
      kerr   = 1
      return
c
c...Invalid word value
c
 9200 cmsg   = 'Invalid number.'
      kerr   = 1
      return
c
c...No MAJOR/MINOR spec
c
 9300 cmsg   = 'MAJOR or MINOR specification required.'
      kerr   = 1
      return
      end
