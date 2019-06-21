c
c***********************************************************************
c
c   FILE NAME: genprompt.for
c   CONTAINS:
c               genprompt
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        genprompt.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:31
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM:  genprompt
c
c   FUNCTION:  This routine converts an input text file into a binary
c              Menu/Prompt file.  The input file can contain both stand-
c              alone prompts and menu levels.  The output file will have
c              the extension .MSG and can used as menu text and error
c              text.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      program genprompt
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      subroutine genprompt(fnam)
      include "postworks_nt.inc"
C WNT-END
c
      include 'menu.inc'
c
      character*(MAX_PATH) fnam
c
      integer*4 ilevl(10),jlevl(10),irec,nrec,irecl,ierr,nca,
     1          frec(5),idat(128),ityp,i,inc,nc,tdat(128),ipnt,
     2          jdat(128),typlev(10),strlen1,itxtix,itxtrc,itxt,
     3          sprix(2,700),isw,ivnc,irnc,itxtpt
c
      integer*4 prompt_err, flag,batch
      real*8 rpr(700)
c
      character*8 spr(700),sprswp
      character*20 att(4)
      character*80 msg,ldat,vmsg,rmsg,msg1
      character*100 buf
      character*512 tcdat,icdat
      character*(MAX_PATH) fnamo
c
      character*2 nln
      byte nlnb(2)
      equivalence (nln, nlnb)
c
      equivalence (tdat,tcdat), (idat,icdat)
      equivalence (spr,rpr)
c
c...Initialize program
c
      call gprompt_version
      call init

C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C
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
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
c
c...Initialize level structure
c
      snlevl = 1
      nlevl  = 0
      ilevl(1) = 0
      iprmix = 2
      imenix = 3
      itxtix = 4
      prompt_err = 0
      flag = 1
C WNT-START
c
c...GenPrompt start
c
c
c...if running batch, don't display those message
c
      flag = 1;
      call ifrunbatch(batch);
      if (batch.eq.0) then
          nlnb(1) = 13
          nlnb(2) = 10
          call shfile (fnam,msg1,50)
          msg(1:80) = nln(1:2) // 'Genprompt - ' // msg1
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
      irecl  = 100
      call opnfil (LUNSC1,fnamo,att,irecl,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
c
c...Open output prompt file
c
      call adftyp (fnamo,fnam,'.MSG')
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'new'
      irecl  = 512
      call opnfil (LUNSC2,fnam,att,irecl,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
c
c...Initialize prompt file
c
      frec(1) = 5
      frec(2) = 4
      frec(3) = 2
      frec(4) = 0
      frec(5) = 0
c
      idat(1) = 0
      idat(2) = 0
      call wrprm (LUNSC2,iprmix,idat,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
c
      idat(1) = 0
      idat(2) = 0
      idat(3) = 0
      call wrprm (LUNSC2,imenix,idat,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
c
      idat(1) = 0
      call wrprm (LUNSC2,itxtix,idat,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
c
c...Read Text Record
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
      call pparse (buf,jlevl,snlevl,ityp,ldat,msg,ierr)
      if (ierr .ne. 0) go to 9200
      if (ityp .eq. 0) go to 500
c
c...Standalone prompt record
c
      if (ityp .eq. 4) then
c          if (snlevl .ne. 1 .or. jlevl(1) .ne. frec(5)+1) go to 9600
c
c...Prompt text will not fit in current text record
c...Allocate another text record &
c...Update main record & previous text record
c
          nc     = strlen1(ldat)
          if (frec(3)+((nc-1)/4+2) .gt. 128) then
              tdat(1) = frec(1)
              tdat(frec(3)) = 0
              call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
              if (ierr .ne. 0) then
                 nc = strlen1(msg)
                 go to 9000
              endif
c
              frec(2) = frec(1)
              frec(3) = 2
              frec(1) = frec(1) + 1
              tdat(1) = 0
          endif
c
c...Store text data
c
          tdat(frec(3)) = nc
          ipnt   = (frec(3)-1) * 4 + 5
          tcdat(ipnt:ipnt+nc-1) = ldat(1:nc)
          call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
          if (ierr .ne. 0) then
             nc = strlen1(msg)
             go to 9000
          endif
c
c...Store prompt label
c
          frec(5) = frec(5) + 1
          spr(frec(5)) = msg(1:8)
          sprix(1,frec(5)) = frec(2)
          sprix(2,frec(5)) = frec(3)
c
c...Update next text record pointer
c
          frec(3) = frec(3) + ((nc-1) / 4 + 2)
          if (frec(3) .gt. 128) then
              tdat(1) = frec(3)
              call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
              if (ierr .ne. 0) go to 9000
c
              frec(2) = frec(1)
              frec(3) = 2
              frec(1) = frec(1) + 1
              tdat(1) = 0
          endif
          go to 500
      endif
c
c...Extra text record for prompt
c
      if (ityp .eq. 3) then
          if (nlevl .le. 1 .or. typlev(nlevl-1) .ne. 2) go to 9500
c
c...Update main text record
c
          if (frec(2) .ne. itxtrc) then
              call rdprm (LUNSC2,itxtrc,tdat,msg,ierr)
              if (ierr .ne. 0) go to 9000
              tdat(itxtpt) = tdat(itxtpt) + 1
              call wrprm (LUNSC2,itxtrc,tdat,msg,ierr)
              if (ierr .ne. 0) go to 9000
              call rdprm (LUNSC2,frec(2),tdat,msg,ierr)
              if (ierr .ne. 0) go to 9000
          else
              tdat(itxtpt) = tdat(itxtpt) + 1
          endif
c
c...Extra text will not fit in current text record
c...Allocate another text record &
c...Update main record & previous text record
c
          nc     = strlen1(ldat)
          if (frec(3)+((nc-1)/4+2) .gt. 128) then
              tdat(1) = frec(1)
              tdat(frec(3)) = 0
              call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
              if (ierr .ne. 0) then
                  nc = strlen1(msg)
                  go to 9000
              endif
c
              frec(2) = frec(1)
              frec(3) = 2
              frec(1) = frec(1) + 1
              tdat(1) = 0
          endif
c
c...Store text data
c
          tdat(frec(3)) = nc
          ipnt   = (frec(3)-1) * 4 + 5
          tcdat(ipnt:ipnt+nc-1) = ldat(1:nc)
          call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
          if (ierr .ne. 0) then
             nc = strlen1(msg)
             go to 9000
          endif
c
c...Update next text record pointer
c
          frec(3) = frec(3) + ((nc-1) / 4 + 2)
          if (frec(3) .gt. 128) then
              tdat(1) = frec(3)
              call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
              if (ierr .ne. 0) then
                  nc = strlen1(msg)
                  go to 9000
              endif
c
              frec(2) = frec(1)
              frec(3) = 2
              frec(1) = frec(1) + 1
              tdat(1) = 0
          endif
          go to 500
      endif
c
c...Check for proper level structure
c
      if (snlevl .eq. nlevl) then
          if (jlevl(snlevl) .ne. ilevl(snlevl)+1) go to 9300
          do 600 i=1,nlevl-1,1
              if (jlevl(i) .ne. ilevl(i)) go to 9300
  600     continue
c
      else if (snlevl .lt. nlevl) then
          if (jlevl(snlevl) .ne. ilevl(snlevl)+1) go to 9300
          do 620 i=1,snlevl-1,1
              if (jlevl(i) .ne. ilevl(i)) go to 9300
  620     continue
c
      else if (snlevl .eq. nlevl+1) then
          if (jlevl(snlevl) .ne. 0 .and. jlevl(snlevl) .ne. 1)
     1        go to 9300
          do 650 i=1,nlevl,1
              if (jlevl(i) .ne. ilevl(i)) go to 9300
  650     continue
c
      else
          go to 9300
      endif
c
c...Set current level number
c
      nlevl  = snlevl
      do 700 i=1,nlevl,1
          ilevl(i) = jlevl(i)
  700 continue
      typlev(nlevl) = ityp
c
c...Read levels of  prompt
c
      nrec  = imenix
      do 1000 i=1,nlevl,1
          call rdprm (LUNSC2,nrec,idat,msg,ierr)
          if (ierr .ne. 0) then
             nc = strlen1(msg)
             go to 9000
          endif
c
c...Get next level's record
c
          inc    = ilevl(i) * 3 + 4
          if (i .ne. nlevl) nrec = idat(inc)
 1000 continue
c
c...Update Menu item record
c
      idat(1) = ilevl(nlevl) + 1
      idat(2) = nlevl
      idat(3) = 1
      if (nlevl .ne. 1) idat(3) = typlev(nlevl-1)
c
c...Update Sub-option pointer
c
      if (idat(3) .eq. 1) then
          idat(inc) = frec(1)
          do 1200 i=1,6,1
              jdat(i) = 0
 1200     continue
          call wrprm (LUNSC2,frec(1),jdat,msg,ierr)
          if (ierr .ne. 0) go to 9000
          frec(1) = frec(1) + 1
      else
          idat(inc) = 0
      endif
c
c...Read current text record
c
      nc     = strlen1(ldat)
      call rdprm (LUNSC2,frec(2),tdat,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
c
c...Menu text will not fit in current text record
c...Allocate another text record &
c...Update Main record & previous text record
c
      if (frec(3)+((nc-1)/4+3) .gt. 128) then
          tdat(1) = frec(1)
          tdat(frec(3)) = 0
          call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
          if (ierr .ne. 0) then
              nc = strlen1(msg)
              go to 9000
          endif
c
          frec(2) = frec(1)
          frec(3) = 2
          frec(1) = frec(1) + 1
          tdat(1) = 0
      endif
c
c...Update text pointers
c
      idat(inc+1) = frec(2)
      idat(inc+2) = frec(3)
c
c...Store text data
c
      tdat(frec(3)) = 1
      tdat(frec(3)+1) = nc
      ipnt    = (frec(3)-1) * 4 + 9
      tcdat(ipnt:ipnt+nc-1) = ldat(1:nc)
c
c...Save last text pointer
c
      itxtrc = frec(2)
      itxtpt = frec(3)
c
c...Adjust next text pointers
c
      frec(3) = frec(3) + ((nc-1) / 4 + 3)
      if (frec(3) .gt. 128) then
          tdat(1) = frec(3)
          call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
          if (ierr .ne. 0) then
             nc = strlen1(msg)
             go to 9000
          endif
c
          frec(2) = frec(1)
          frec(3) = 2
          frec(1) = frec(1) + 1
          tdat(1) = 0
      endif
c
c...Store Menu & Text records
c
      call wrprm (LUNSC2,nrec,idat,msg,ierr)
         if (ierr .ne. 0) then
             nc = strlen1(msg)
             go to 9000
         endif
      call wrprm (LUNSC2,frec(2),tdat,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
      go to 500
c
c...End of routine
c
C WNT-START
 8000 call clsfil (0)
      if ((prompt_err .eq. 0).and.(batch.eq.0))  then
          call shfile (fnam,msg1,50)
          len = len_trim(msg1)
          msg = "Genprompt - "// msg1(1:len)//" completed"
          nc = len + 22
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
c...End of file on text file
c...Sort standalone prompts
c
 8100 isw    = 0
      do 8150 i=1,frec(5)-1,1
          if (rpr(i) .gt. rpr(i+1)) then
              sprswp = spr(i)
              spr(i) = spr(i+1)
              spr(i+1) = sprswp
c
              ipnt   = sprix(1,i)
              inc    = sprix(2,i)
              sprix(1,i) = sprix(1,i+1)
              sprix(2,i) = sprix(2,i+1)
              sprix(1,i+1) = ipnt
              sprix(2,i+1) = inc
c
              isw    = 1
          endif
 8150 continue
      if (isw .eq. 1) go to 8100
c
c...Check for duplicate labels
c
      do 8160 i=1,frec(5)-1,1
          if (spr(i) .eq. spr(i+1)) then
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C       call trmmsg ('*ERROR*  Duplicate standalone prompt label')
C              call trmmsg (spr(i))
C              call trmmsg (' ')
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
              nc = 42
              call add1dispmsg (
     1			'*ERROR*  Duplicate standalone prompt label',nc, flag)
              nc = strlen1(spr(i))
              call add1dispmsg (spr(i), nc, flag)
C WNT-END
          endif
 8160 continue
c
c...Save prompt records
c
      irec   = iprmix
      idat(1) = 0
      ipnt    = 3
      do 8170 i=1,frec(5),1
          itxt   = (ipnt-1) * 4 + 1
          icdat(itxt:itxt+7) = spr(i)
          idat(ipnt+2) = sprix(1,i)
          idat(ipnt+3) = sprix(2,i)
          ipnt   = ipnt    + 4
          if (ipnt .gt. 125) then
              idat(1) = frec(1)
              call wrprm (LUNSC2,irec,idat,msg,ierr)
              if (ierr .ne. 0) go to 9000
              idat(1) = 0
              irec   = frec(1)
              frec(1) = frec(1) + 1
              ipnt   = 3
          endif
 8170 continue
      call wrprm (LUNSC2,irec,idat,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
      endif
c
c...Save Descriptor record
c
      call wrprm (LUNSC2,1,frec,msg,ierr)
      if (ierr .ne. 0) then
         nc = strlen1(msg)
         go to 9000
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
C          call errhnd (vmsg,ivnc,rmsg,irnc)
C          if (ivnc .ne. 0) call trmmsg (vmsg)
C          if (irnc .ne. 0) call trmmsg (rmsg)
C      endif
C      go to 8000
c
c...Invalid prompt record
c
C9200 call trmmsg (' ')
C      call trmmsg ('*FATAL*  Invalid prompt record.')
C      call trmmsg (msg)
C      call trmmsg (buf)
C      go to 8000
c
c...Invalid leveling of prompts
c
C9300 call trmmsg (' ')
C      call trmmsg ('*FATAL*  Invalid layering of prompts.')
C      call trmmsg (buf)
C      go to 8000
c
c...Invalid text record
c
C9500 call trmmsg (' ')
C      msg    = '*FATAL*  Text record only valid after prompt field.'
C      call trmmsg (msg)
C      call trmmsg (buf)
C      go to 8000
c
c...Invalid Standalone prompt
c
C9600 call trmmsg (' ')
C      call trmmsg ('*FATAL*  Invalid standalone prompt number.')
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
      prompt_err = 1
      go to 8000
c
c...Invalid prompt record
c
 9200 msg1 = '*FATAL*  Invalid prompt record.'
      nc = strlen1(msg1)
      call add1dispmsg (msg1,nc, flag)
      nc = strlen1(msg)
      call add1dispmsg (msg, nc, flag)
      nc = strlen1(buf)
      call add1dispmsg (buf, nc, flag)
      prompt_err = 1
      go to 8000
c
c...Invalid leveling of prompts
c
 9300 msg1 = '*FATAL*  Invalid layering of prompts.'
      nc = strlen1(msg1)
      call add1dispmsg (msg1,nc, flag)
      nc = strlen1(buf)
      call add1dispmsg (buf, pwMtxt)
      prompt_err = 1
      go to 8000
c
c...Invalid text record
c
 9500 msg    = '*FATAL*  Text record only valid after prompt field.'
      nc = strlen1(msg)
      call add1dispmsg (msg, nc, flag)
      nc = strlen1(buf)
      call add1dispmsg (buf, nc, flag)
      prompt_err = 1
      go to 8000
c
c...Invalid Standalone prompt
c
 9600 msg1 = '*FATAL*  Invalid word descriptor.'
      nc = strlen1(msg1)
      call add1dispmsg (msg1, nc, flag)
      nc = strlen1(buf)
      call add1dispmsg (buf, nc, flag)
      prompt_err = 1
      go to 8000
C WNT-END
      end
