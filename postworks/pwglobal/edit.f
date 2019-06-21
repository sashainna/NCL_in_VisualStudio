c
c***********************************************************************
c
c   FILE NAME: edit.for
c   CONTAINS:
c               fnc2nd  getchr  getesc  getlin
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        edit.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  fnc2nd (k2nd)
c
c   FUNCTION:  This routine toggles the 2nd Function banner.
c
c   INPUT:  k2nd    I*4  D1  -  1 = Display banner.  2 = Erase banner.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine fnc2nd (k2nd)
c
      include 'menu.inc'
c
      integer*4 k2nd
c
      character*12 lsf
c
      data lsf /'2nd Function'/
c
      if (CURSES .eq. 0) go to 8000
      call savcur
      call plott (ISELIN,68)
c
c...Enable 2nd Function
c
      if (k2nd .eq. 1) then
          call hilit (1)
          call dmpbuf (lsf,12)
          call hilit (0)
c
c...Disable 2nd Function
c
      else
          call clreol
      endif
      call rstcur
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getchr (kchar)
c
c   FUNCTION:  This routine gets a character from the keyboard and de-
c              termines if it is an escape sequence or a 2nd Function
c              key.
c
c   INPUT:  none.
c
c   OUTPUT: kchar   I*4  D1  -  Numeric value of character entered.
c                               Recognized escape sequences & 2nd Func-
c                               tion keys are returned as a single
c                               value.
c
c***********************************************************************
c
C SUN-SGI-IBM-HPX-START
C     subroutine getchr (kchar)
c
C     include 'menu.inc'
c
C     integer*4 kchar
c
C     integer*4 i2nd,ic,ic1,ierr,igo,iesc,iescfl
c
C     integer*4 i,ifun(4),jindex
c
C     data ifun / 6, 16, 14, 21/
c
c...Initialize routine
c
C     i2nd   = 0
c
c...Are we dumping an escape sequence ?
c
C  50 if (IESCPT .lt. NESC) go to 700
C     iesc   = 0
C     NESC   = 0
C     IESCPT = 0
c
c...Read input character
c
C 100 call gtch (ic)
c
c...Remove parity bit
c
C SUN-SGI-IBM-HPX-END
C IBM-START
C     ic     = and(ic,127)
C IBM-END
C VAX-SUN-SGI-DEC-HPX-START
C     ic     = ic     .and. 127
C IBM-START
c
c...Check for escape sequence
c
C     if (iesc .eq. 0) then
C         if (ic .ne. 27) go to 800
C         iesc   = 1
C         go to 600
C     endif
C VAX-SUN-SGI-IBM-DEC-HPX-END
C DOS-START
C     if (ic .lt. 1000) go to 800
C     if (ic .eq. 1008) then
C         ic   = 8
C         go to 800
C     end if
C DOS-END
c
c......In the midst of an escape sequence
c
C SUN-SGI-IBM-HPX-START
C     call getesc (ic,kchar,ic1,i2nd,iesc,iescfl,ierr)
C     igo    = ierr   + 1
C     go to (1000,700,100), igo
c
c.........Buffer up escape sequence
c
C 600 NESC   = NESC   + 1
C     ESCBUF(NESC) = ic
C     go to 100
c
c......Invalid escape sequence
c......Issue buffered escape sequence
c
C 700 IESCPT = IESCPT + 1
C     kchar  = ESCBUF(IESCPT)
C     go to 1000
c
c...Check for function key (^*)
c
C 800 if (ic .eq. 8) ic = 127
C     kchar  = ic
C     i      = jindex (ifun,ic,4) + 4
C     if (i .gt. 4) then
C         if (i .eq. 5) then
C             i2nd = i2nd + 1
C             if (i2nd .eq. 2) i2nd = 0
C             call fnc2nd (i2nd)
C             if (i2nd .eq. 1) go to 100
C         else
C             kchar = 0 - i - i2nd * 96
C             go to 1000
C         end if
C      end if
c
c...Standard character
c
C     if (ic .eq. 127 .or. i2nd .eq. 0) go to 8000
C     if (ic .lt. 32) go to 1000
C     if (ic .ge. 97 .and. ic .le. 122) ic = ic - 32
C     kchar  = 0 - ic
C     go to 1000
c
c...Erase 2nd function display
c
C1000 if (i2nd .eq. 0) go to 8000
C     i2nd   = 0
C     call fnc2nd (i2nd)
c
c...End of routine
c
C8000 return
C     end
C SUN-SGI-IBM-HPX-END
c
c***********************************************************************
c
c   SUBROUTINE:  getesc (kc,kchar,kc1,k2nd,kesc,kescfl,kerr)
c
c   FUNCTION:  This routine receives 1 character at a time and puts them
c              together to determine if a valid escape sequence was en-
c              tered.  If so it returns the single numeric value of the
c              escape sequence.  This routine should be called at the
c              1st character after an escape character was entered and
c              until it returns an error value of 0.
c
c   INPUT:  kc      I*4  D1  -  Character entered from the keyboard.
c
c           k2nd    I*4  D1  -  1 = The 2nd Function mode is active.
c                               The 2nd Function value of the escape
c                               sequence will be returned when 'k2nd'
c                               is set to 1.
c
c           kesc    I*4  D1     Contains the call number to 'getesc'
c                               and should be set to 1 on the first
c                               call.  After that 'getesc' will incre-
c                               ment this value each time it is called.
c
c   OUTPUT: kchar   I*4  D1  -  Numeric value of escape sequence.  This
c                               value is only returned when 'kerr'=0.
c
c           kc1     I*4  D1  -  Saves 'kc' for the next call to 'getesc'
c                               and is not used by the calling routine.
c
c           kescfl  I*4  D1  -  Saves the value of the escape sequence
c                               when a fixed character is needed to ter-
c                               minate the escape sequence (for example
c                               on the SUN workstation) and is not used
c                               by the calling routine.
c
c           kerr    I*4  D1  -  0 = Escape sequence was fully parsed and
c                               it's value is being returned.  1 = In-
c                               valid escape sequence.  2 = Escape se-
c                               quence is being parsed but more charac-
c                               ters are needed.
c
c***********************************************************************
c
      subroutine getesc (kc,kchar,kc1,k2nd,kesc,kescfl,kerr)
c
      include 'menu.inc'
c
      integer*4 kc,kchar,kc1,k2nd,kesc,kescfl,kerr,i
c
      character*1 lc,lc1
c
C VAX-START
C     character*1 lesc(22),lc2
C VAX-END
c
C SUN-START
C     character*1 lesc(36),lesc1(10)
C SUN-END
c
C SGI-IBM-HPX-START
C     character*1 lesc(54),lesc1(9),lc2
C SGI-IBM-HPX-END
C DOS-START
C     integer*4 lesc(22)
C DOS-END
c
C VAX-START
C     data lesc /'A','B','D','C','P','Q','R','S','w','x','y','m','t',
C    1           'u','v','l','q','r','s','M','p','n'/
C VAX-END
c
C SUN-START
C     data lesc1 /'A','B','D','C','2','z','0','1','2','3'/
C
C     data lesc /'0','8','0','9','1','0','1','3','2','5','1','8',
C    1           '2','6','1','2','1','4','1','6','2','2','1','1',
C    2           '2','9','3','0','3','1','2','8','2','7','2','0'/
C SUN-END
C SGI-IBM-HPX-START
C     data lesc1 /'A','B','D','C','[','q','0','1','2'/
c
C     data lesc /'1','3','9', 'H',' ',' ', '1','5','0', '0','1','1',
C    1           '0','0','2', '0','0','8', '0','0','3', '0','1','0',
C    2           '0','0','6', '0','0','7', '0','1','2', '0','0','9',
C    3           '2','0','9', '2','1','3', '2','1','7', '0','0','5',
C    4           '0','0','4', '0','0','1'/
C SGI-IBM-HPX-END
C DOS-START
C     data lesc /1072,1080,1075,1077,1082,1071,1073,1133,1071,1072,
C    -           1073,1010,1006,1007,1012,1009,0,0,0,1005,1004,1001/
C DOS-END
c
      lc     = char(kc)
      i      = 0
c
c...check for escape sequence
c
C VAX-START
C     if (kesc .ne. 1) go to 200
C     if (lc .ne. '[' .and. lc .ne. 'O') go to 650
C     kesc   = 2
C     go to 600
C
C 200 do 300 i=1,22,1
C         if (lc .eq. lesc(i)) go to 440
C 300 continue
C     go to 650
C VAX-END
C SUN-START
C     if (kesc .eq. 1) then
C         if (lc .ne. '[') go to 650
C         kesc   = 2
C         go to 600
C     endif
C
C 200 if (kesc .eq. 2) then
C         if (lc .ne. lesc1(5)) then
C             do 250 i=1,4,1
C                 if (lc .eq. lesc1(i)) go to 440
C 250         continue
C             go to 650
C         endif
C         kesc = 3
C         go to 600
C     endif
C
C     if (kesc .eq. 3) then
C         do 300 i=7,10,1
C             if (lc .eq. lesc1(i)) go to 320
C 300     continue
C         go to 650
C 320     kc1    = kc
C         kesc   = 4
C         go to 600
C     endif
C
C     if (kesc .eq. 4) then
C         i      = 1
C         lc1    = char(kc1)
C 400     if (lc1 .eq. lesc(i) .and. lc .eq. lesc(i+1)) go to 420
C         i      = i      + 2
C         if (i .gt. 36) go to 650
C         go to 400
C 420     kescfl = (i-1) / 2 + 1 + 4
C         kesc   = 5
C         go to 600
C     endif
C
C     if (kesc .eq. 5) then
C         if (lc .ne. lesc1(6)) go to 650
C         i      = kescfl
C         go to 440
C     endif
C     go to 1200
C SUN-END
C SGI-IBM-HPX-START
C     if (kesc .eq. 1) then
C         if (lc .ne. lesc1(5)) go to 650
C         kesc   = 2
C         go to 600
C     endif
c
C 200 if (kesc .eq. 2) then
C         if (lc .eq. lesc(4)) then
C             i      = (4-1) / 3 + 1 + 4
C             go to 440
C         endif
C         do 250 i=1,4,1
C             if (lc .eq. lesc1(i)) go to 440
C 250     continue
C         do 300 i=7,9,1
C             if (lc .eq. lesc1(i)) go to 320
C 300     continue
C         go to 650
C 320     lc1    = lc
C         kesc   = 3
C         go to 600
C     endif
c
C     if (kesc .eq. 3) then
C         lc2    = lc
C         kesc   = 4
C         go to 600
C     endif
C
C     if (kesc .eq. 4) then
C         i      = 1
C 400     if (lc1 .eq. lesc(i) .and. lc2 .eq. lesc(i+1) .and.
C    1        lc .eq. lesc(i+2)) go to 420
C         i      = i      + 3
C         if (i .gt. 54) go to 650
C         go to 400
C 420     kescfl = (i-1) / 3 + 1 + 4
C         kesc   = 5
C         go to 600
C     endif
c
C     if (kesc .eq. 5) then
C         if (lc .ne. lesc1(6)) go to 650
C         i      = kescfl
C         go to 440
C     endif
C     go to 1200
C SGI-IBM-HPX-END
C DOS-START
c
c...Delete key
c
C     if (kc .eq. 1083) then
C         kchar   = 127
C         go to 1000
C     endif
c
c...Find supported key
c
C     do 100 i=1,22,1
C         if (kc .eq. lesc(i)) go to 440
C 100 continue
C     go to 1100
C DOS-END
c
c...Recognized escape sequence
c
  440 if (i .eq. 5) go to 450
      kchar  = 0 - i
      if (k2nd .eq. 1) kchar = kchar - 96
      NESC   = 0
      go to 1000
c
c......2nd Function key
c
  450 k2nd   = k2nd   + 1
      if (k2nd .eq. 2) k2nd = 0
      call fnc2nd (k2nd)
      kesc   = 0
      NESC   = 0
      go to 1200
c
c...Buffer up escape sequence
c
  600 NESC   = NESC   + 1
      ESCBUF(NESC) = kc
      go to 1200
  650 NESC   = NESC   + 1
      ESCBUF(NESC) = kc
      go to 1100
c
c...Return escape char
c
 1000 kerr   = 0
      return
c
c...Bad escape sequence
c
 1100 kerr   = 1
      return
c
c...Get another character
c
 1200 kerr   = 2
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getlin (cbuf,knc,kmxc,klin,kcol)
c
c   FUNCTION:  This routine gets a character string from the keyboard.
c              It allows the user a certain amount of editing features
c              when inputting the line.  If 'knc' is not equal to 0 when
c              'getlin' is called, then 'cbuf' will be displayed and
c              used as the default input.
c
c   INPUT:  kmxc    I*4  D1  -  Maximum number of characters allowed in
c                               input string.
c
c           klin    I*4  D1  -  Cursor line # for start of string.  A
c                               value of -1 specifies that the terminal
c                               is not under cursor control and all
c                               editing will be done on the current
c                               line.
c
c           kcol    I*4  D1  -  Cursor column # for start of string.
c
c   OUTPUT: cbuf    C*n  D1  -  Input character string from keyboard.
c                               'cbuf' is also used as the default
c                               string when 'knc' is greater than 0.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.  Returns
c                               -1 if the input was interrupted with a
c                               ^C.  When CURSES is set to 1, -2 is re-
c                               turnced if an Up arrow was entered, -3
c                               for a ^W.
c
c***********************************************************************
c
C SUN-SGI-IBM-HPX-START
C     subroutine getlin (cbuf,knc,kmxc,klin,kcol)
c
C     include 'menu.inc'
c
C     integer*4 knc,kmxc,klin,kcol
c
C     character*(*) cbuf
c
C     integer*4 strlen1,ic,is,insmod
c
C     character*80 sbuf,obuf
c
c...Initialize routine
c
C     is     = 1
C     insmod = 0
C     sbuf   = cbuf
C     if (knc .lt. 0) knc = 0
c
c...Display current line
c
C     if (klin .ne. -1) call plott (klin,kcol)
C     call dmpbuf (cbuf,knc)
C     call clreol
C     if (knc .ne. 0) then
C         if (klin .ne. -1) then
C             call plott (klin,kcol)
C         else
C             call crslft (knc)
C         endif
C     endif
c
c...Get character from keyboard
c
C 100 call getchr (ic)
C     if (ic .eq. -3) go to 200
C     if (ic .eq. -4) go to 300
C     if (ic .eq. -99) go to 900
C     if (ic .eq. -100) go to 1000
C     if (ic .eq. 1) go to 600
C     if (ic .eq. 127) go to 700
C     if (ic .eq. 4) go to 800
C      if (ic .eq. 8) go to 400
C     if (ic .eq. 8) go to 700
C     if (ic .eq. 10) go to 500
C     if (ic .eq. 13) go to 1100
C     if (ic .eq. 3) go to 1200
C     if (ic .eq. -1) go to 1300
C     if (ic .eq. 23) go to 1400
C     go to 1500
c
c...Left arrow
c...Move cursor left
c
C 200 if (is .eq. 1) go to 100
C     is     = is     - 1
C     if (klin .ne. -1) then
C         call plott (klin,is+kcol-1)
C     else
C         call crslft (1)
C     endif
C     go to 100
c
c...Right arrow
c...Move cursor right
c
C 300 if (is .gt. knc) go to 100
C     is     = is     + 1
C     if (klin .ne. -1) then
C         call plott (klin,is+kcol-1)
C     else
C         call crsrgt (1)
C     endif
C     go to 100
c
c...Backspace
c...Move cursor to beginning of line
c
C 400 if (klin .ne. -1) then
C         call plott (klin,kcol)
C     else
C         call crslft (is-1)
C     endif
C     is     = 1
C     go to 100
c
c...Line feed
c...Move cursor to End of line
c
C 500 if (klin .ne. -1) then
C         call plott (klin,kcol+knc)
C     else
C         call crsrgt (knc-is+1)
C     endif
C     is     = knc    + 1
C     go to 100
c
c...^A
c...Toggle insert mode
c
C 600 insmod = insmod + 1
C     if (insmod .eq. 2) insmod = 0
C     go to 100
c
c...Delete
c...Delete char to left
c
C 700 if (is .eq. 1) go to 100
C     obuf = sbuf(1:is-2) // sbuf(is:knc) // ' '
C     sbuf   = obuf
C     is     = is     - 1
C     if (klin .ne. -1) then
C         call plott (klin,is+kcol-1)
C     else
C         call crslft (1)
C     endif
C     call dmpbuf (sbuf(is:knc),knc-is+1)
C     knc    = knc    - 1
C     if (klin .ne. -1) then
C         call plott (klin,is+kcol-1)
C     else
C         call crslft (knc-is+2)
C     endif
C     go to 100
c
c...^D
c...Delete char under cursor
c
C 800 if (is .gt. knc) go to 100
C     obuf   = sbuf(1:is-1) // sbuf(is+1:knc)
C     sbuf   = obuf
C     call dmpbuf (sbuf(is:knc),knc-is+1)
C     knc    = knc    - 1
C     if (klin .ne. -1) then
C         call plott (klin,is+kcol-1)
C     else
C         call crslft (knc-is+2)
C     endif
C     go to 100
c
c...2nd left arrow
c...Delete to beginning of line
c
C 900 if (is .eq. 1) go to 100
C     sbuf   = sbuf(is:knc)
C     knc    = knc    - is     + 1
C     if (klin .ne. -1) then
C         call plott (klin,kcol)
C     else
C         call crslft (is-1)
C     endif
C     is     = 1
C     call dmpbuf (sbuf,knc)
C     call clreol
C     if (klin .ne. -1) then
C         call plott (klin,kcol)
C     else
C         call crslft (knc)
C     endif
C     go to 100
c
c...2nd right arrow
c...Delete to end of line
c
C1000 if (is .gt. knc) go to 100
C     knc    = is     - 1
C     call clreol
C     go to 100
c
c...CR
c...End of input
c
C1100 cbuf   = sbuf(1:knc)
C     knc    = strlen1(cbuf)
C     return
c
c...^C
c...Cancel input line
c
C1200 knc    = -1
C     return
c
c...Up arrow
c...Go to previous line
c
C1300 if (CURSES .eq. 0) go to 100
C     knc    = -2
C     return
c
c...^W
c...Redisplay page
c
C1400 if (CURSES .eq. 0) go to 100
C     knc    = -3
C     return
c
c...Standard character
c
C1500 if (ic .lt. 32) go to 100
C     if (insmod .eq. 0 .or. is .gt. knc) go to 1550
c
c......Insert mode is on
c
C     if (knc .eq. kmxc) go to 100
C     obuf   = sbuf(1:is-1) // char(ic) // sbuf(is:knc)
C     sbuf   = obuf
C     knc    = knc    + 1
C     call dmpbuf (sbuf(is:knc),knc-is+1)
C     is     = is     + 1
C     if (klin .ne. -1) then
C         call plott (klin,is+kcol-1)
C     else
C         call crslft (knc-is+1)
C     endif
C     go to 100
c
c......Insert mode is off
c
C1550 if (is .gt. kmxc) go to 100
C     sbuf(is:is) = char(ic)
C     if (is .gt. knc) knc = knc + 1
C     call dmpbuf (sbuf(is:is),1)
C     is     = is     + 1
C     go to 100
C     end
C SUN-SGI-IBM-HPX-END
