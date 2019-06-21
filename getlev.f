c
c***********************************************************************
c
c   FILE NAME: getlev.for
c   CONTAINS:
c               asclev  chklev  dissel  getsel  levasc  selinp
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        getlev.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:56
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  asclev (cbuf,knc,klevl,knlevl,kerr)
c
c   FUNCTION:  This routine converts a character string (i.e. 1.2.4) to
c              an array of level values.
c
c   INPUT:  cbuf    C*n  D1  -  Character string.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: klevl   I*4  D10 -  An array containing the level values.
c
c           knlevl  I*4  D1  -  Number of levels entered (i.e. 1.2.4 =
c                               3 levels).
c
c           kerr    I*4  D1  -  Returns 1 for an invalid character
c                               string.
c
c***********************************************************************
c
      subroutine asclev (cbuf,knc,klevl,knlevl,kerr)
c
      include 'menu.inc'
c
      integer*4 knc,klevl(10),knlevl,kerr
c
      character*(*) cbuf
c
      integer*4 i,nc,ilev(10),nlev
c
      character*80 ldat
c
c...Initialize routine
c
      knlevl = 0
      kerr   = 0
      nc     = 0
      nlev   = 0
c
c...Get level number
c
      do 500 i=1,knc+1,1
          if (cbuf(i:i) .eq. '.' .or. i .eq. knc+1) then
              if (nc .eq. 0) go to 9000
              nlev   = nlev   + 1
              call ctoi (ldat(1:nc),ilev(nlev),kerr)
              if (kerr .ne. 0) go to 9000
              if (ilev(nlev) .lt. 0. .or. ilev(nlev) .gt. MAXMEN)
     1            go to 9000
              nc     = 0
          else
              nc     = nc     + 1
              ldat(nc:nc) = cbuf(i:i)
          endif
  500 continue
c
      knlevl = nlev
      do 600 i=1,nlev,1
          klevl(i) = ilev(i)
  600 continue
c
c...End of routine
c
 8000 return
c
c...Bad level number
c
 9000 kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  chklev (klevl,knlevl,cmsg,kerr)
c
c   FUNCTION:  This routine verifys that the prompt specified by the in-
c              put level actually exists in the prompt file.
c
c   INPUT:  klevl   I*4  D10 -  An array containing the level values.
c
c           knlevl  I*4  D1  -  Number of levels in klevl.
c
c   OUTPUT: cmsg    C*n  Dn  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 for a non-existent prompt
c                               level.
c
c***********************************************************************
c
      subroutine chklev (klevl,knlevl,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klevl(10),knlevl,kerr
c
      character*(*) cmsg
c
      integer*4 idat(128),i,inc,irec,jlevl(10),jnlevl
c
c...Set-up actual menu level
c...Based on current menu level &
c...requested level
c
      do 100 i=1,NLEVL,1
          jlevl(i) = MLEVL(i)
  100 continue
c
      do 200 i=1,knlevl,1
          jlevl(NLEVL+i-1) = klevl(i)
  200 continue
c
      jnlevl = NLEVL  + knlevl - 1
c
c...Read in requested levels
c
      call rdprm (LUNPRM,IMENIX,idat,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      do 500 i=1,jnlevl-1,1
          if (jlevl(i) .ge. idat(1)) go to 9100
          inc    = jlevl(i) * 3 + 4
          irec   = idat(inc)
          if (irec .eq. 0) go to 9100
c
          call rdprm (LUNPRM,irec,idat,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  500 continue
      if (jlevl(jnlevl) .ge. idat(1)) go to 9100
c
c...End of routine
c
 8000 return
c
c...No such level number
c
 9100 kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  dissel (klevl)
c
c   FUNCTION:  This routine displays the marker character '>' at the
c              currently active choice.
c
c   INPUT:  klevl   I*4  D1  -  The currently active menu choice.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine dissel (klevl)
c
      include 'menu.inc'
c
      integer*4 klevl
c
      integer*4 ilin,icol
c
c...Erases previous menu choice
c
      if (IPRELV .eq. klevl) go to 8000
      if (IPRELV .lt. 0) go to 100
      if (IPRELV .ge. (IMENEL-IMENBL+1)) then
          ilin   = IMENBL + IPRELV - (IMENEL-IMENBL+1)
          icol   = 41
      else
          ilin   = IMENBL + IPRELV
          icol   = 1
      endif
c
      call plott (ilin,icol)
      call dmpbuf (' ',1)
c
c...Display current choice
c
  100 if (klevl .ge. (IMENEL-IMENBL+1)) then
          ilin   = IMENBL + klevl  + (IMENEL-IMENBL+1)
          icol   = 41
      else
          ilin   = IMENBL + klevl
          icol   = 1
      endif
c
      call plott (ilin,icol)
      call hilit (1)
      call dmpbuf ('>',1)
      call plott (ISELIN,SAPNC(ISELPT)+2)
      call hilit (0)
c
c...End of routine
c
 8000 IPRELV = klevl
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getsel (klevl,knlevl,cmsg,kerr)
c
c   FUNCTION:  This routine inputs a menu selection from the keyboard
c              and returns the level values entered along with the num-
c              ber of levels.
c
c   INPUT:  none.
c
c   OUTPUT: klevl   I*4  D10 -  An array containing the level values en-
c                               tered.
c
c           knlevl  I*4  D1  -  Number of levels entered (i.e. 1.2.4 =
c                               3 levels).
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine getsel (klevl,knlevl,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 klevl(10),knlevl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,ifl,ic,ierr
c
      character*40 sbuf,hbuf
c
c...Display Selection prompt
c
      if (ISELPT .eq. 0) call getsap ('Select',ISELPT,IPRMDS,SALABL)
c
   80 call plott (ISELIN,1)
      call dmpbuf (SAPRM(ISELPT),SAPNC(ISELPT))
      call clreol
c
c...Get command line
c
      nc     = 0
      sbuf   = ' '
  100 call dissel (klevl(1))
      call selinp (sbuf,nc,ifl)
c
c...^C entered
c...Pick last menu option
c
      if (ifl .eq. -1) then
          knlevl = 1
          klevl(1) = IMENDT(1,NLEVL) - 1
          go to 8000
c
c...CR
c...Pick current menu option
c
      else if (ifl .eq. 0 .and. nc .eq. 0) then
          knlevl = 1
          go to 8000
c
c...Down Arrow
c...Go to next selection
c
      else if (ifl .eq. 1) then
          if (klevl(1) .eq. IMENDT(1,NLEVL)-1) go to 100
          klevl(1) = klevl(1) + 1
          go to 100
c
c...Up Arrow
c...Go to next selection
c
      else if (ifl .eq. 2) then
          if (klevl(1) .eq. 0) go to 100
          klevl(1) = klevl(1) - 1
          go to 100
c
c...Right Arrow
c...Go to next row
c
      else if (ifl .eq. 3) then
          if (klevl(1)+(IMENEL-IMENBL+1) .ge. IMENDT(1,NLEVL)) go to 100
          klevl(1) = klevl(1) + (IMENEL-IMENBL+1)
c
c...Left Arrow
c...Go to previous row
c
      else if (ifl .eq. 4) then
          if (klevl(1)-(IMENEL-IMENBL+1) .lt. 0) go to 100
          klevl(1) = klevl(1) - (IMENEL-IMENBL+1)
c
c...^W
c...Redisplay page
c
      else if (ifl .eq. 5) then
          call disban
          call dismen
          call plott (ISELIN,1)
          call dmpbuf (SAPRM(ISELPT),SAPNC(ISELPT))
          IPRELV = -1
          go to 100
c
c...?
c...Display help text
c
      else if (nc .eq. 1 .and. sbuf .eq. '?') then
          call levasc (MLEVL,NLEVL,hbuf)
          call helpi (hbuf,MLEVL(1),ierr)
          if (ierr .eq. 0) then
              call errmsg ('HITKEY',1,2)
              call gtch (ic)
          endif
          if (ierr .ne. 1) then
              call dismen
              IPRELV = -1
          endif
          go to 80
      endif
c
c...Parse command line to get level numbers
c
      call asclev (sbuf,nc,klevl,knlevl,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Make sure this is a proper level
c
      if (knlevl .eq. 1) then
          if (klevl(1) .ge. IMENDT(1,NLEVL)) go to 9100
      else
          call chklev (klevl,knlevl,cmsg,kerr)
          if (kerr .lt. 0) go to 8000
          if (kerr .ne. 0) go to 9100
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid Level Spec
c
 9000 call errmsg ('INVLEV',1,2)
      kerr   = 0
      go to 100
c
c...No such level
c
 9100 call errmsg ('NOSCHLEV',1,2)
      kerr   = 0
      go to 100
      end
c
c***********************************************************************
c
c   SUBROUTINE:  levasc (klevl,knlevl,cbuf)
c
c   FUNCTION:  This routine converts an array of level values to a char-
c              acter string.
c
c   INPUT:  klevl   I*4  D10 -  An array containing the level values.
c
c           knlevl  I*4  D1  -  Number of levels entered (i.e. 1.2.4 =
c                               3 levels).
c
c   OUTPUT: cbuf    C*n  D1  -  Returned character string.
c
c***********************************************************************
c
      subroutine levasc (klevl,knlevl,cbuf)
c
      integer*4 klevl(10),knlevl
c
      character*(*) cbuf
c
      integer*4 i,inc,nc
c
      character*20 sbuf
      character*80 lbuf
c
c...Convert level array to character string
c
      do 500 i=1,knlevl,1
          call itoc (klevl(i),sbuf,inc,1)
          if (i .eq. 1) then
              cbuf   = sbuf(1:inc)
              nc     = inc
          else
              lbuf   = cbuf(1:nc) // '.' // sbuf(1:inc)
              cbuf   = lbuf
              nc     = nc     + inc    + 1
          endif
  500 continue
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  selinp (cbuf,knc,kfl)
c
c   FUNCTION:  This routine accepts a menu selection from the keyboard.
c              It allows the user a certain amount of editing features
c              when inputting the line.
c
c   INPUT:  none.
c
c   OUTPUT: cbuf    C*n  D1  -  Input character string from keyboard.
c                               'cbuf' is also used as the default
c                               string when 'knc' is greater than 0.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kfl     I*4  D1  -  Flag that shows which key terminated
c                               this record.
c
c                               -1 = ^C - Select last option.
c                                0 = CR - Choose current selection or
c                                         typed in selection.
c                                1 = DNA - Go down 1 selection.
c                                2 = UPA - Go up 1 selection.
c                                3 = RTA - Go right 1 row.
c                                4 = LFA - Go left 1 row.
c                                5 = ^W - Redisplay page.
c
c***********************************************************************
c
      subroutine selinp (cbuf,knc,kfl)
c
      include 'menu.inc'
c
      integer*4 knc,kfl
c
      character*(*) cbuf
c
      integer*4 strlen1,ic,is,insmod,mxc,nca,ilin,icol
c
      character*80 sbuf,obuf
c
      ilin   = ISELIN
      icol   = SAPNC(ISELPT) + 2
c
c...Load current field in working string
c
   50 is     = 0
      insmod = 0
      mxc    = 40
      nca    = knc
      call plott (ilin,icol)
      if (knc .eq. 0) then
          sbuf = ' '
      else
          sbuf   = cbuf(1:knc)
          call dmpbuf (sbuf,knc)
          call plott (ilin,icol)
      endif
c
c...Get character from keyboard
c
  100 call getchr (ic)
c
c...Left arrow
c...Move cursor left
c
      if (ic .eq. -3) then
          if (is .eq. 0) then
              if (nca .eq. 0) then
                  kfl    = 4
                  go to 8000
              else
                  go to 100
              endif
          else
              is     = is     - 1
              call plott (ilin,is+icol)
          endif
c
c...Right arrow
c...Move cursor right
c
      else if (ic .eq. -4) then
          if (is .eq. 0 .and. nca .eq. 0) then
              kfl    = 3
              go to 8000
          else if (is .ge. nca) then
              go to 100
          else
              is     = is     + 1
              call plott (ilin,is+icol)
          endif
c
c...Back space
c...Go to end of line
c
      else if (ic .eq. 8) then
          is     = 0
          call plott (ilin,icol)
c
c...Line feed
c...Go to end of line
c
      else if (ic .eq. 10) then
          is     = nca
          call plott (ilin,is+icol)
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
          obuf = sbuf(1:is-1) // sbuf(is+1:nca) // ' '
          sbuf   = obuf
          is     = is     - 1
          call plott (ilin,is+icol)
          call dmpbuf (sbuf(is+1:nca),nca-is+1)
          nca    = nca    - 1
          call plott (ilin,is+icol)
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
          call plott (ilin,is+icol)
c
c...2nd left arrow
c...Delete to beginning of line
c
      else if (ic .eq. -99) then
          if (is .eq. 0) go to 100
          sbuf   = sbuf(is+1:nca)
          nca    = nca    - is
          call plott (ilin,icol)
          is     = 0
          sbuf   = sbuf(1:nca)
          call dmpbuf (sbuf,nca)
          call clreol
          call plott (ilin,icol)
c
c...2nd right arrow
c...Delete to end of line
c
      else if (ic .eq. -100) then
          if (is .ge. nca) go to 100
          nca    = is
          sbuf(nca+1:mxc) = ' '
          call clreol
c
c...DNA
c...Go to next selection
c
      else if (ic .eq. -2) then
          if (nca .eq. 0) then
              knc    = strlen1(sbuf)
              cbuf   = sbuf
              kfl    = 1
              go to 8000
          else
              go to 100
          endif
c
c...UPA
c...Go to previous selection
c
      else if (ic .eq. -1) then
          if (nca .eq. 0) then
              knc    = strlen1(sbuf)
              cbuf   = sbuf
              kfl    = 2
              go to 8000
          else
              go to 100
          endif
c
c...CR
c...Choose current selection or
c...typed in selection
c
      else if (ic .eq. 13) then
          knc    = strlen1(sbuf)
          cbuf   = sbuf
          kfl    = 0
          go to 8000
c
c...^C
c...Choose last selection
c
      else if (ic .eq. 3) then
          kfl    = -1
          go to 8000
c
c...^W
c...Redisplay page
c
      else if (ic .eq. 23) then
          knc    = strlen1(sbuf)
          cbuf   = sbuf
          kfl    = 5
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
              call plott (ilin,is+icol)
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
