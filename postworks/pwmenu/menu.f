c
c***********************************************************************
c
c   FILE NAME:  menu.f
c   CONTAINS:
c               dismen  form    frminp  gtflab  gtflab1  lodmen  menu
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        menu.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:24:33
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  dismen
c
c   FUNCTION:  This routine clears the screen and displays the menu
c              choices.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine getcpr (cmsg)
c
      include 'menu.inc'
c
      character*(*) cmsg
c
      character*10 sb1,sb2
      integer*4 nc1,nc2
c
c...Store Copyright notice
c
      call itoc (PWVER1,sb1,nc1,1)
      call itoc (PWVER2,sb2,nc2,1)
      cmsg   = PGMNAM // '  (Version ' // sb1(1:nc1) // '.' //
     1         sb2(1:nc2) // ')  ' // 'Copyright (C) ' // CPYDATE
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  dismen
c
c   FUNCTION:  This routine clears the screen and displays the menu
c              choices.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine dismen
c
      include 'menu.inc'
c
      integer*4 ilin,icol,ipt,i,itxt,nc
c
      character*80 msg
c
c...Initialize routine
c
      ilin   = IMENBL
      icol   = 2
c
c......Clear screen
c
      call plott (IMENBL,1)
      call clreos
c
c......Display the Copyright disclosure
c......if this is the top level menu
c
      if (NLEVL .eq. 1 .and. MLEVL(NLEVL) .eq. 0) then
          call getcpr (msg)
          call errmsg (msg,2,2)
          call errmsg (msg,2,2)
      endif
c
c......Display active sections
c
      call dissec
c
c......Get the menu item text &
c......Display it
c
      ipt    = 1
      do 300 i=1,IMENDT(1,NLEVL),1
          ipt    = IPRDAT(2,i)
          itxt   = (ipt-1) * 4 + 5
          nc     = IPRTXT(ipt)
          call plott (ilin,icol)
          call dmpbuf (LPRTXT(itxt:itxt+nc-1),nc)
          if (ilin .lt. IMENEL) then
              ilin   = ilin   + 1
          else
              ilin   = IMENBL
              icol   = 42
          endif
  300 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  form (kbeg,kerr)
c
c   FUNCTION:  This routine displays a form with multiple records that
c              each have the same format and allows the user to edit
c              each record.  The actual form is described in the follow-
c              ing global variables.
c
c              FRMBEG = Beginning line number of first record in form on
c                       the screen.
c
c              FRMWID = Width of form on screen.
c
c              FRMREC = Number of records in form.
c
c              FRMNFD = Number of fields in each record.
c
c              FRMST  = An array holding the starting position of each
c                       field in a single record.
c
c              FRMEN  = An array holding the ending position of each
c                       field in a single record.
c
c              FRMFNC = A 2 dimensional array that holds the number of
c                       characters in each field in every record.
c
c              FRMBUF = An array of character strings, pre-formatted for
c                       each record.
c
c
c   INPUT:  kbeg    I*4  D1  -  First record to display in form.  This
c                               should be set to 1 on the original call
c                               and should be set to the record that an
c                               error occurred on in subsequent calls.
c
c   OUTPUT: kerr    I*4  D1  -  Returns 1 when the user does not want to
c                               save the changes made to the form.
c
c***********************************************************************
c
      subroutine form (kbeg,kerr)
c
      include 'menu.inc'
c
      integer*4 kbeg,kerr
c
      integer*4 ielin,ifsz,ilin,ifd,i,ifl,irec,ibrec,ierec,ierr,isav
c
      character*80 sbuf
c
c...Display the current section
c
      call dissec
      SNLEVL = 0
c
c...Set form size
c
      ielin  = FRMBEG + FRMREC - 1
      if (ielin .gt. ISELIN-1) then
          ielin = ISELIN - 1
      endif
      ifsz   = ielin  - FRMBEG + 1
      ibrec  = kbeg
      ierec  = ibrec  + ifsz   - 1
      if (ierec .gt. FRMREC) ierec = FRMREC
c
c...Display the form header
c
   50 ilin   = IMENBL
      do 60 i=1,NFRMHD,1
          call plott (ilin,1)
          call dmpbuf (SAPRM(FRMHPT(i)),SAPNC(FRMHPT(i)))
          ilin   = ilin   + 1
   60 continue
c
c...Display the form
c
  100 ilin   = FRMBEG
      do 200 i=ibrec,ierec,1
          call plott (ilin,1)
          call dmpbuf (FRMBUF(i),FRMWID)
          ilin   = ilin   + 1
  200 continue
c
c...Get the users input
c
      irec   = ibrec
      ifd    = 1
      ilin   = FRMBEG
      kerr   = 0
  500 call frminp (irec,ifd,ilin,ifl)
c
c...End of form input
c...Do not save
c
      if (ifl .eq. -1) then
          kerr   = 1
          go to 8000
c
c...End of form input
c...Save changes
c
      else if (ifl .eq. 0) then
          go to 8000
c
c...Help
c
      else if (ifl .eq. 7) then
          isav   = IMENBL
          IMENBL = FRMBEG
          call levasc (MLEVL,NLEVL,sbuf)
          call helpi (sbuf,MLEVL(1),ierr)
          IMENBL = isav
          if (ierr .eq. 1) go to 500
          if (ierr .eq. 0) then
              call errmsg ('HITKEY',1,2)
              call gtch (isav)
          endif
          call plott (FRMBEG,1)
          call clreos
          go to 100
c
c...^W
c...Redisplay form
c
      else if (ifl .eq. 8) then
          call disban
          call dissec
          go to 50
c
c...Go to next record
c
      else if (ifl .eq. 1 .or. ifl .eq. 2) then
          if (irec .eq. FRMREC) go to 500
          irec   = irec   + 1
          ilin   = ilin   + 1
          if (ifl .eq. 2) ifd = 1
          if (FRMFNC(ifd,irec) .lt. 0) ifd = 1
          if (ilin .le. ielin) go to 500
          ilin   = ielin
          ibrec  = ibrec  + 1
          ierec  = ierec  + 1
          call scrol (FRMBEG,ielin,1,1)
          call plott (ilin,1)
          call clreol
          call dmpbuf (FRMBUF(irec),FRMWID)
          go to 500
c
c...Go to previous record
c
      else if (ifl .eq. 3 .or. ifl .eq. 4) then
          if (irec .eq. 1) go to 500
          irec   = irec   - 1
          ilin   = ilin   - 1
          if (ifl .eq. 4) ifd = 1
          if (ilin .ge. FRMBEG) go to 500
          ilin   = FRMBEG
          ibrec  = ibrec  - 1
          ierec  = ibrec  + ifsz   - 1
          call scrol (FRMBEG,ielin,1,2)
          call plott (ilin,1)
          call clreol
          call dmpbuf (FRMBUF(irec),FRMWID)
          go to 500
c
c...Go down 1 page
c
      else if (ifl .eq. 5) then
          if (ierec .ge. FRMREC) go to 500
          ibrec  = ierec  + 1
          ierec  = ierec  + ifsz
          if (ierec .gt. FRMREC) ierec = FRMREC
          call plott (FRMBEG,1)
          call clreos
          go to 100
c
c...Go up 1 page
c
      else if (ifl .eq. 6) then
          if (ibrec .eq. 1) go to 500
          ibrec  = ibrec  - ifsz
          if (ibrec .lt. 1) ibrec = 1
          ierec  = ibrec  + ifsz   - 1
          go to 100
      endif

c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  frminp (krec,kfd,klin,kfl)
c
c   FUNCTION:  This routine accepts a form record the keyboard.  It al-
c              lows the user a certain amount of editing features when
c              inputting the line.  The form record is stored in the
c              FORM common variables.
c
c   INPUT:  krec    I*4  D1  -  Current record in form that is being
c                               edited.
c
c           kfd     I*4  D1  -  Field within record to position at.
c
c           klin    I*4  D1  -  Cursor line # for this record.
c
c   OUTPUT: kfl     I*4  D1  -  Flag that shows which key terminated
c                               this record.
c
c                               -1 = PF1 K - End of form. Don't save.
c                                0 = ^C - End of form.
c                                1 = DNA - Next record same field.
c                                2 = CR  - Next record 1st field.
c                                3 = UPA - Previous record same field.
c                                4 = ^U - Previous record 1st field.
c                                5 = PF1 DNA - Next page.
c                                6 = PF1 UPA - Previous page.
c                                7 = PF1 H - Help.
c                                8 = ^W - Redisplay page.
c
c***********************************************************************
c
      subroutine frminp (krec,kfd,klin,kfl)
c
      include 'menu.inc'
c
      integer*4 krec,kfd,klin,kfl
c
      integer*4 strlen1,ic,is,insmod,mxc,ist,ien,nca
c
      character*40 spac
      character*132 sbuf,obuf
c
      data spac /'                                        '/
c
c...Load current field in working string
c
c  50 if (FRMTYP(kfd) .eq. 3) then
c         ic     = 9
c         go to 150
c     endif
   50 nca    = FRMFNC(kfd,krec)
      if (nca .eq. 0) then
          sbuf   = ' '
      else
          ist    = FRMST(kfd)
          ien    = FRMST(kfd) + nca    - 1
          obuf   = FRMBUF(krec)
          sbuf   = obuf(ist:ien)
      endif
      if (FRMTYP(kfd) .eq. 3) then
          ic     = 9
          go to 150
      endif
      is     = 0
      insmod = 0
      mxc    = FRMEN(kfd) - FRMST(kfd) + 1
      call plott (klin,FRMST(kfd))
c
c...Get character from keyboard
c
  100 call getchr (ic)
c
c...Left arrow
c...Move cursor left
c
  150 if (ic .eq. -3) then
          if (is .eq. 0) then
              FRMFNC(kfd,krec) = strlen1(sbuf)
              FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) =
     1            sbuf(1:FRMFNC(kfd,krec))
              kfd    = kfd    - 1
              if (kfd .lt. 1) kfd = FRMNFD
              go to 50
          else
              is     = is     - 1
              call plott (klin,is+FRMST(kfd))
          endif
c
c...Right arrow
c...Move cursor right
c
      else if (ic .eq. -4) then
          if (is .ge. nca) then
              FRMFNC(kfd,krec) = strlen1(sbuf)
              FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) =
     1            sbuf(1:FRMFNC(kfd,krec))
              kfd    = kfd    + 1
              if (kfd .gt. FRMNFD) kfd = 1
              if (FRMFNC(kfd,krec) .lt. 0) kfd = 1
              go to 50
          else
              is     = is     + 1
              call plott (klin,is+FRMST(kfd))
          endif
c
c...Line feed
c...Go to end of field
c
      else if (ic .eq. 10) then
          is     = nca
          call plott (klin,is+FRMST(kfd))
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
              obuf  = sbuf(is+1:nca)
          else
              obuf  = sbuf(1:is-1) // sbuf(is+1:nca) // ' '
          endif
          sbuf   = obuf
          is     = is     - 1
          call plott (klin,is+FRMST(kfd))
          call dmpbuf (sbuf(is+1:nca),nca-is)
          nca    = nca    - 1
          call plott (klin,is+FRMST(kfd))
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
          call plott (klin,is+FRMST(kfd))
c
c...2nd left arrow
c...Delete to beginning of line
c
      else if (ic .eq. -99) then
          if (is .eq. 0) go to 100
          sbuf   = sbuf(is+1:nca)
          nca    = nca    - is
          call plott (klin,FRMST(kfd))
          is     = 0
          sbuf   = sbuf(1:nca)
          call dmpbuf (sbuf,FRMEN(kfd)-FRMST(kfd)+1)
          call plott (klin,FRMST(kfd))
c
c...2nd right arrow
c...Delete to end of line
c
      else if (ic .eq. -100) then
          if (is .ge. nca) go to 100
          nca    = is
          sbuf(nca+1:mxc) = ' '
          call dmpbuf (spac,FRMEN(kfd)-FRMST(kfd)+1-is)
          call plott (klin,FRMST(kfd)+is)
c
c...Tab
c...Go to next field
c
      else if (ic .eq. 9) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfd    = kfd    + 1
          if (kfd .gt. FRMNFD) kfd = 1
          if (FRMFNC(kfd,krec) .lt. 0) kfd = 1
          go to 50
c
c...Backspace
c...Go to previous field
c
      else if (ic .eq. 8) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfd    = kfd    - 1
          if (kfd .lt. 1) kfd = FRMNFD
          go to 50
c
c...DNA
c...Go to next record
c
      else if (ic .eq. -2) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 1
          go to 8000
c
c...CR
c...Go to beginning of next record
c
      else if (ic .eq. 13) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 2
          go to 8000
c
c...UPA
c...Go to previous record
c
      else if (ic .eq. -1) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 3
          go to 8000
c
c...^U
c...Go to beginning of previous record
c
      else if (ic .eq. 21) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 4
          go to 8000
c
c...PF1 DNA
c...Go to next page
c
      else if (ic .eq. -98) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 5
          go to 8000
c
c...PF1 UPA
c...Go to previous page
c
      else if (ic .eq. -97) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 6
          go to 8000
c
c...^C
c...End of form
c
      else if (ic .eq. 3) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 0
          go to 8000
c
c...PF1 K
c...End of form - Do not save data
c
      else if (ic .eq. -75) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = -1
          go to 8000
c
c...PF1 H
c...Help
c
      else if (ic .eq. -72) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 7
          go to 8000

c
c...^W
c...Redisplay page
c
      else if (ic .eq. 23) then
          FRMFNC(kfd,krec) = strlen1(sbuf)
          FRMBUF(krec)(FRMST(kfd):FRMEN(kfd)) = sbuf(1:FRMFNC(kfd,krec))
          kfl    = 8
          go to 8000

c
c...Standard character
c
      else
          if (ic .lt. 32) go to 100
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
              call plott (klin,is+FRMST(kfd))
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
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtflab (chead,clab,knc,kst,ken,kcol)
c
c   FUNCTION:  This routine returns an individual label from a prompt
c              line based on a specified slot in the prompt line.
c
c   INPUT:  chead   C*n  D1  -  Input prompt line to retrieve label from.
c
c           kst     I*4  D1  -  Starting position in prompt line to look
c                               for the label.  The first non-blank
c                               character will mark the start of the label.
c
c           ken     I*4  D1  -  Ending position in prompt line to look
c                               for the label.
c
c           kcol    I*4  D1  -  1 = Add ':' to end of label.
c
c   OUTPUT: clab    C*n  D1  -  Label from the specified slot.
c
c           knc     I*4  D1  -  Number of chars in 'chead'.
c
c***********************************************************************
c
      subroutine gtflab (chead,clab,knc,kst,ken,kcol)
c
      include 'menu.inc'
c
      integer*4 knc,kst,ken,kcol,nindex
c
      character*(*) chead,clab
c
      integer*4 ist,ien
c
c...Find first non-blank character
c
      ist    = nindex(chead(kst:ken),' ')
      if (ist .eq. 0) then
          clab   = ' '
          knc    = 0
          go to 8000
      endif
      ist    = kst    + ist    - 1
c
c...Find end of string
c
      ien    = index(chead(ist:ken),' ') - 1
      if (ien .lt. 0) then
          ien    = ken
      else
          ien    = ist    + ien    - 1
      endif
c
c...Set label
c
      if (kcol .eq. 0) then
          clab   = chead(ist:ien)
          knc    = ien    - ist    + 1
      else
          clab   = chead(ist:ien) // ':'
          knc    = ien    - ist    + 2
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtflab1 (chead,clab,knc,kst,kcol)
c
c   FUNCTION:  This routine returns an individual label from a prompt
c              line based on the starting position within the prompt
c              line.
c
c   INPUT:  chead   C*n  D1  -  Input prompt line to retrieve label from.
c
c           kst     I*4  D1  -  Starting position in prompt line to look
c                               for the label.  Should be set to 1 on the
c                               first call and left alone after that.
c
c           kcol    I*4  D1  -  1 = Add ':' to end of label.
c
c   OUTPUT: clab    C*n  D1  -  Label from the specified slot.
c
c           knc     I*4  D1  -  Number of chars in 'chead'.
c
c           kst     I*4  D1  -  Starting position for next label.
c
c***********************************************************************
c
      subroutine gtflab1 (chead,clab,knc,kst,kcol)
c
      include 'menu.inc'
c
      integer*4 knc,kst,ken,kcol,nindex
c
      character*(*) chead,clab
c
      integer*4 ist,ien,nc,strlen1
c
c...Initialize routine
c
      clab   = ' '
      knc    = 0
      nc     = strlen1(chead)
      if (kst .gt. nc) go to 8000
c
c...Find first non-blank character
c
      ist    = nindex(chead(kst:),' ')
      if (ist .eq. 0) then
          kst    = nc     + 1
          go to 8000
      endif
      ist    = kst    + ist    - 1
c
c...Find end of string
c
      ien    = index(chead(ist:),' ') - 1
      if (ien .le. 0) then
          ien    = ken
      else
          ien    = ist    + ien    - 1
      endif
c
c...Set label
c
      if (kcol .eq. 0) then
          clab   = chead(ist:ien)
          knc    = ien    - ist    + 1
      else
          clab   = chead(ist:ien) // ':'
          knc    = ien    - ist    + 2
      endif
c
c...Increment starting index
c
      kst    = ien    + 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodmen (klevl,knlevl,cmsg,kerr)
c
c   FUNCTION:  This routine loads in the requested level of menu/prompt
c              text.  It also appends the prompt number to the beginning
c              of the text.  Only 1 level of menu/prompt text can be in
c              memory at 1 time.
c
c   INPUT:  klevl   I*4  D10  -  An array containing the current level
c                                values.
c
c           knlevl  I*4  D10  -  Number of levels in 'klevl'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lodmen (klevl,knlevl,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klevl(10),knlevl,kerr
c
      character*(*) cmsg
c
      integer*4 i,irec,inc,itxt,lsttxt,itxtpt,itrec,ntxt,tdat(128),
     1          ipt,nc,ie,nct,iend,strlen1,itpt,j
c
      character*20 lnum
      character*80 sbuf
      character*512 tcdat
c
      equivalence (tdat,tcdat)
c
c...Read in requested levels
c
      do 500 i=1,knlevl,1
          if (i .eq. 1) then
              irec   = IMENIX
          else
              inc    = klevl(i-1) * 3 + 4
              irec   = IMENDT(inc,i-1)
          endif
c
          if (irec .ne. SMREC(I) .or. IMENDT(1,i) .le. 1) then
              call rdprm (LUNPRM,irec,IMENDT(1,i),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              SMREC(i) = irec
          endif
  500 continue
c
c...Read in menu text
c
  600 lsttxt = 0
      itxtpt = 1
      do 800 i=1,IMENDT(1,knlevl),1
          inc    = (i-1) * 3 + 4
          itrec  = IMENDT(inc+1,knlevl)
          itxt   = IMENDT(inc+2,knlevl)
          if (itrec .eq. 0) go to 800
c
c......Read text record
c
          if (itrec .ne. lsttxt) then
              call rdprm (LUNPRM,itrec,tdat,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          lsttxt = itrec
c
c......Set up text data
c
          ipt    = (itxt-1) * 4 + 9
          nc     = tdat(itxt+1)
          ie     = ipt    + nc     - 1
          call itoc (i-1,lnum,nct,1)
          if (i-1 .ge. 10) then
              sbuf    = lnum(1:nct) // '.  '  // tcdat(ipt:ie)
          else
              sbuf    = lnum(1:nct) // '.   '  // tcdat(ipt:ie)
          endif
c
c......Store prompt data
c
          IPRDAT(1,i) = tdat(itxt)
          IPRDAT(2,i) = itxtpt
          nc     = strlen1(sbuf)
          itpt   = (itxtpt-1) * 4 + 5
          ie     = itpt   + nc     - 1
          IPRTXT(itxtpt) = nc
          LPRTXT(itpt:ie) = sbuf
          itxtpt = itxtpt + (nc-1) / 4 + 2
c
c......Store extra text data
c
          ntxt   = IPRDAT(1,i)
          if (ntxt .ne. 1) then
              itxt   = itxt   + ((tdat(itxt+1)-1) / 4 + 3)
              do 700 j=2,ntxt,1
                  if (itxt .gt. 128 .or. tdat(itxt) .eq. 0) then
                      lsttxt = tdat(1)
                      call rdprm (LUNPRM,lsttxt,tdat,cmsg,kerr)
                      if (kerr .ne. 0) go to 8000
                      itxt   = 2
                  endif
c
                  nc     = tdat(itxt)
                  ipt    = (itxt-1) * 4 + 5
                  ie     = ipt    + nc     - 1
                  itpt   = (itxtpt-1) * 4 + 5
                  iend   = itpt   + nc     - 1
                  IPRTXT(itxtpt) = nc
                  LPRTXT(itpt:iend) = tcdat(ipt:ie)
                  itxtpt = itxtpt + (nc-1) / 4 + 2
                  itxt   = itxt   + (nc-1) / 4 + 2
  700         continue
          endif
  800 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  menu (kfl,cmsg,kerr)
c
c   FUNCTION:  This is the menu handling routine.  It displays the menu
c              text and gets the menu selection from the keyboard.  It
c              also displays the current section and level.
c
c   INPUT:  none.
c
c   OUTPUT: kfl     I*4  D1  -  0 = Single value was entered.  Other-
c                                   wise it contains the level number
c                                   that was entered.  'MLEVL' contains
c                                   the level values.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine menu (kfl,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 i,jlevl(10),jnlevl,ilev
c
c...Initialize routine
c
      IWALK  = 0
      IMTYP  = 1
      ilev   = PRESEL(NLEVL)
c
c...Load the levels of menu items
c
      call lodmen (MLEVL,NLEVL,cmsg,kerr)
      if (kerr .ne. 0 .or. MOTIF .eq. 1) go to 8000
c
c...See if this screen is already displayed
c
      if (NLEVL .eq. SNLEVL) then
          do 100 i=1,SNLEVL,1
              if (MLEVL(i) .ne. SMLEVL(i)) go to 200
  100     continue
          go to 400
      endif
c
c......Save current screen
c
  200 SNLEVL = NLEVL
      do 250 i=1,SNLEVL,1
          SMLEVL(i) = MLEVL(i)
          PRESEL(i) = MLEVL(i)
  250 continue
c
c...Display the menu
c
      call dismen
c
c...Get menu selection
c
      IPRELV = -1
  400 if (NLEVL .eq. 0) then
          jlevl(1) = 0
      else
          jlevl(1) = ilev   + 1
          if (jlevl(1) .ge. IMENDT(1,NLEVL)) jlevl(1) = 0
      endif
      call getsel (jlevl,jnlevl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Set up current level number
c
      if (jnlevl .eq. 1) then
          MLEVL(NLEVL) = jlevl(1)
          PRESEL(NLEVL) = MLEVL(NLEVL)
          if (PRESEL(NLEVL) .eq. 0) PRESEL(NLEVL) = IMENDT(1,NLEVL) - 2
          kfl    = 0
      else
          do 500 i=1,jnlevl,1
              MLEVL(NLEVL+i-1) = jlevl(i)
  500     continue
          kfl    = NLEVL  + jnlevl - 1
      endif
c
c......Erase error messages
c
      call errmsg (' ',2,1)
      call errmsg (' ',2,2)
c
c...End of routine
c
 8000 return
      end
