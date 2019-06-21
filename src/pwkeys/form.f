c
c***********************************************************************
c
c   FILE NAME: form.for
c   CONTAINS:
c               clrfrm  disfld  disprm  form  selfrm
c               opninp  opnout  clsout
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        form.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:07
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  clrfrm
c
c   FUNCTION:  This routine clears out all fields in the form and resets
c              the search pointer to the beginning of the file.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clrfrm
c
      include 'menu.inc'
c
      integer*4 i
c
c...Clear form data
c
      do 100 i=1,10,1
          FRMBUF(i) = ' '
          SAPNC(i) = 0
  100 continue
c
c...Initialize search pointers
c
c...SMREC(1) = Pointer to top level company index record. (1-26)
c...SMREC(2) = Pointer to current company index record.
c...SMREC(3) = Pointer to current data record.
c...SMREC(4) = Pointer within SMREC(2) for current company
c
      SMREC(1) = IERRDS(2)
      SMREC(2) = IERRDS(2)
      SMREC(3) = 0
      SMREC(4) = 0
c
c...Display selection fields
c
      call disfld (-1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  disfld (kfl)
c
c   FUNCTION:  This routine displays the selected form field(s).
c
c   INPUT:  kfl     I*4  D1    -  0 = Display all fields.  +n = Display
c                                 only this field.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine disfld (kfl)
c
      include 'menu.inc'
c
      integer*4 kfl
c
      integer*4 i,ist,ien,nc
c
      byte bbuf(132)
      character*132 cbuf
      equivalence (bbuf,cbuf)
c
      character*80 spac
c
      data spac /' '/

c
c...Set up field range
c
      if (kfl .lt. 0) then
          ist    = 1
          ien    = FRMREC
      else
          ist    = kfl
          ien    = kfl
      endif
c      if (ien .eq. 10 .and. NLEVL .ne. 2) ien = 8
c
c...Display field data
c
C WNT-START
      call disfldC (FRMBUF,ist,ien)
C WNT-END
C SUN-SGI-IBM-HPX-START
C     do 100 i=ist,ien,1
C         if (MOTIF.eq.1) then
C             nc = SAPNC(i)
C             cbuf(1:nc) = FRMBUF(i)(1:nc)
C             call mfdisfld(i, bbuf,nc)
C         else
C             call plott (IPRMDS(i),FRMST(i))
C             if (SAPNC(i) .ne. 0) call dmpbuf (FRMBUF(i),SAPNC(i))
C             nc     = FRMEN(i) - FRMST(i) + 1 - SAPNC(i)
C             if (nc .gt. 0) call dmpbuf (spac,nc)
C         endif
C 100 continue
C SUN-SGI-IBM-HPX-END
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  disprm (kfl)
c
c   FUNCTION:  This routine displays the selected form prompt(s).
c
c   INPUT:  kfl     I*4  D1    -  0 = Display all prompts.  +n = Display
c                                 only this prompt.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine disprm (kfl)
c
      include 'menu.inc'
c
      integer*4 kfl
c
      integer*4 ipr(10),ist,ien,i,ipst(10)
c
      character*20 lpr(10)
c
      data lpr /'Company:','Hardware:','Software:',
     1          'Options:','Number of Users:','License Termination:',
     2          'Version:','System ID:','Password:','License:'/
c
      data ipr /8,9,9,8,16,20,8,10,9,8/
      data ipst /1,1,1,1,1,1,1,1,1,55/
c
c...Set up range
c
      if (kfl .lt. 0) then
          ist    = 1
          ien    = FRMREC
      else
          ist    = kfl
          ien    = kfl
      endif
c
c...Display prompts
c
c      if (ien .eq. 10 .and. NLEVL .ne. 2) ien = 8
      do 100 i=ist,ien,1
          call plott (IPRMDS(i),ipst(i))
          if (MLEVL(i) .eq. 1) call hilit (1)
          call dmpbuf (lpr(i),ipr(i))
          if (MLEVL(i) .eq. 1) call hilit (0)
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  form (krec,kfl)
c
c   FUNCTION:  This routine controls the input of the entire form.  It
c              allows the user a certain amount of editing features
c              when inputing the data.  The form record is stored in
c              the FORM common variables.
c
c   INPUT:  krec    I*4  D1  -  Record in form to position at.
c
c   OUTPUT: krec    I*4  D1  -  See INPUT.
c
c           kfl     I*4  D1  -  Flag that shows which key terminated
c                               this record.
c
c                               0 = ^C - Exit program.
c                               1 = PF1 A - Add record.
c                               2 = PF2 - Help.
c                               4 = PF1 PF3 - Search for next record.
c                               5 = PF4 - Clear form.
c                               6 = PF1 D - Delete record.
c                               7 = PF1 P - Print record.
c                               8 = PF1 E - Start Password Generation.
c                              10 = ^W - Redisplay form.
c                              11 = PF1 U - Purge data base.
c                              12 = PF1 PF4 - Show field selections.
c
c***********************************************************************
c
      subroutine form (krec,kfl)
c
      include 'menu.inc'
c
      integer*4 krec,kfl
c
      integer*4 ic,is,insmod,mxc,nca,isw
c
      character*80 spac
      character*80 sbuf,obuf
c
      data spac /' '/
c
c...Load current field in working string
c
   50 is     = 0
      insmod = 0
      isw    = 0
      mxc    = FRMEN(krec) - FRMST(krec) + 1
      nca    = SAPNC(krec)
      if (nca .eq. 0) then
          sbuf   = ' '
      else
          sbuf   = FRMBUF(krec)
      endif
      call plott (IPRMDS(krec),FRMST(krec))
c
c...Get character from keyboard
c
  100 call getchr (ic)
c
c...^X
c...Execute special command
c
      if (ic .eq. 24 .and. isw .ne. 0) then
          if (isw .eq. 1 .and. NLEVL .ne. 2) then
              FRMBUF(krec) = sbuf
              SAPNC(krec) = nca
              kfl    = 3
              go to 8000
          else if (isw .eq. 2 .and. NLEVL .eq. 2) then
              FRMBUF(krec) = sbuf
              SAPNC(krec) = nca
              kfl    = 8
              go to 8000
          else
              isw   = 0
              go to 100
          endif
      endif
      isw    = 0
c
c...Left arrow
c...Move cursor left
c
      if (ic .eq. -3) then
          if (is .eq. 0) go to 100
          is     = is     - 1
          call plott (IPRMDS(krec),is+FRMST(krec))
c
c...Right arrow
c...Move cursor right
c
      else if (ic .eq. -4) then
          if (is .ge. nca) go to 100
          is     = is     + 1
          call plott (IPRMDS(krec),is+FRMST(krec))
c
c...Backspace
c...Go to beginning of line
c
      else if (ic .eq. 8) then
          is     = 0
          call plott (IPRMDS(krec),FRMST(krec))
c
c...Line feed
c...Go to end of field
c
      else if (ic .eq. 10) then
          is     = nca
          call plott (IPRMDS(krec),is+FRMST(krec))
c
c...^A
c...Toggle insert mode
c
      else if (ic .eq. 1) then
          insmod = insmod + 1
          if (insmod .eq. 2) insmod = 0
c
c...Delete
c...Delete char to left
c
      else if (ic .eq. 127) then
          if (is .eq. 0) go to 100
          if (is .eq. 1) then
              obuf   = sbuf(is+1:nca)
          else
              obuf = sbuf(1:is-1) // sbuf(is+1:nca) // ' '
          endif
          sbuf   = obuf
          is     = is     - 1
          call plott (IPRMDS(krec),is+FRMST(krec))
          call dmpbuf (sbuf(is+1:nca),nca-is+1)
          nca    = nca    - 1
          call plott (IPRMDS(krec),is+FRMST(krec))
c
c...^D
c...Delete char under cursor
c
      else if (ic .eq. 4) then
          if (is .ge. nca) go to 100
          obuf   = sbuf(1:is) // sbuf(is+2:nca) // ' '
          sbuf   = obuf
          call dmpbuf (sbuf(is+1:nca),nca-is)
          nca    = nca    - 1
          call plott (IPRMDS(krec),is+FRMST(krec))
c
c...2nd left arrow
c...Delete to beginning of line
c
      else if (ic .eq. -99) then
          if (is .eq. 0) go to 100
          sbuf   = sbuf(is+1:nca)
          nca    = nca    - is
          call plott (IPRMDS(krec),FRMST(krec))
          is     = 0
          call dmpbuf (sbuf,FRMEN(krec)-FRMST(krec)+1)
          call plott (IPRMDS(krec),FRMST(krec))
c
c...2nd right arrow
c...Delete to end of line
c
      else if (ic .eq. -100) then
          if (is .ge. nca) go to 100
          nca    = is
          sbuf(nca+1:mxc) = ' '
          call dmpbuf (spac,FRMEN(krec)-FRMST(krec)+1-is)
          call plott (IPRMDS(krec),FRMST(krec)+is)
c
c...DNA / CR
c...Go to next record
c
      else if (ic .eq. -2 .or. ic .eq. 13) then
          if (krec .eq. FRMREC) go to 100
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          krec   = krec   + 1
          go to 50
c
c...UPA
c...Go to previous record
c
      else if (ic .eq. -1) then
          if (krec .eq. 1) go to 100
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          krec   = krec   - 1
          go to 50
c
c...^C
c...End of form
c
      else if (ic .eq. 3) then
          kfl    = 0
          go to 8000
c
c...PF1 A
c...Add record
c
      else if (ic .eq. -65) then
          if (NLEVL .ne. 2) go to 100
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          kfl    = 1
          go to 8000
c
c...PF2
c...Help
c
      else if (ic .eq. -6) then
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          kfl    = 2
          go to 8000
c
c...PF1 PF3
c...Toggle select
c
      else if (ic .eq. -103) then
          MLEVL(krec) = 1 - MLEVL(krec)
          call disprm (krec)
          call plott (IPRMDS(krec),FRMST(krec)+is)
          if (MLEVL(krec) .eq. 1) then
              FRMBUF(krec+20) = sbuf
              SAPNC(krec+20) = nca
          endif
          go to 100
c
c...PF1 PF4
c...Show select
c
      else if (ic .eq. -104) then
          kfl    = 12
          go to 8000
c
c...PF3
c...Find next match
c
      else if (ic .eq. -7) then
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          kfl    = 4
          go to 8000
c
c...PF4
c...Clear form
c
      else if (ic .eq. -8) then
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          kfl    = 5
          go to 8000
c
c...PF1 D
c...Delete record
c
      else if (ic .eq. -68) then
          if (NLEVL .ne. 2) go to 100
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          kfl    = 6
          go to 8000
c
c...PF1 E
c...Password calculation
c
      else if (ic .eq. -69) then
          isw    = 2
          go to 100
c
c...PF1 P
c...Print record
c
      else if (ic .eq. -80) then
          if (NLEVL .ne. 2) go to 100
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          kfl    = 7
          go to 8000
c
c...PF1 S
c...Super user
c
      else if (ic .eq. -83) then
          isw    = 1
          go to 100
c
c...PF1 U
c...Purge data base
c
      else if (ic .eq. -85) then
          if (NLEVL .ne. 2) go to 100
          kfl    = 11
          go to 8000
c
c...^W
c...Redisplay page
c
      else if (ic .eq. 23) then
          FRMBUF(krec) = sbuf
          SAPNC(krec) = nca
          kfl    = 10
          go to 8000
c
c...Standard character
c
      else
          if (ic .lt. 32) go to 100
          if (ic .ge. 97 .and. ic .le. 122) ic = ic - 32
c
c......Insert mode is on
c
          if (insmod .eq. 1 .and. is .lt. nca) then
              if (nca .eq. mxc) go to 100
              obuf   = sbuf(1:is) // char(ic) // sbuf(is+1:nca)
              sbuf   = obuf
              nca    = nca    + 1
              call dmpbuf (sbuf(is+1:nca),nca-is)
              is     = is     + 1
              call plott (IPRMDS(krec),is+FRMST(krec))
c
c......Insert mode is off
c
          else
              if (is .ge. mxc) go to 100
              sbuf(is+1:is+1) = char(ic)
              if (is .ge. nca) nca = nca + 1
              call dmpbuf (sbuf(is+1:is+1),1)
              is     = is     + 1
          endif
      endif
      go to 100
c
c...End of routine
c
 8000 call plott (IPRLIN,1)
      call clreol
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  selfrm
c
c   FUNCTION:  This routine loads the selection fields into the active
c              form.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine selfrm
c
      include 'menu.inc'
c
      integer*4 i
c
c...Load selection fields into form
c
      do 100 i=1,10,1
          FRMBUF(i) = FRMBUF(i+20)
          SAPNC(i) = SAPNC(i+20)
  100 continue
c
c...Display selection fields
c
      call disfld (-1)
c
c...End of routine
c
 8000 return
      end
