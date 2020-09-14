
C*********************************************************************
C*    NAME         :  gvtlin.f
C*       CONTAINS:
C*    COPYRIGHT 1992 (c) Numerical Control Computer Sciences
C*                       All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       gvtlin.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:33
C********************************************************************/
C
c
c***********************************************************************
c
c   FILE NAME: gvtlin.for
c   CONTAINS:
c               fnc2nd  gvtchr  gvtesc  gvtlin
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
      include 'menu.com'
c
      integer*4 k2nd
c
      character*12 lsf
c
      data lsf /'2nd Function'/
c
      if (CURSES .eq. 0) go to 8000
      call savcur
      call plot (ISELIN,68)
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
c   SUBROUTINE:  gvtchr (kchar)
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
      subroutine gvtchr (kchar)
c
      include 'com4a.com'
      include 'menu.com'
c
      integer*4 kchar,tmp
c
      integer*4 i2nd,ic,ic1,ierr,igo,iesc,iescfl
c
c...Initialize routine
c
      i2nd   = 0
c
c...Are we dumping an escape sequence ?
c
   50 if (IESCPT .lt. NESC) go to 700
      iesc   = 0
      NESC   = 0
      IESCPT = 0
c
c...Read input character
c
  100 call getc1 (ic)
c
c...Remove parity bit
c
c
c....01/22/92
c....This logic was changed to work correctly on RS6000. Paul
c....Old version:
c.... ic     = ic     .and. 127
c
!=SUN,IRS,IBM
!      tmp    = 127 
!      ic     = and(ic,tmp) 
!=VAX,HPX,WNT
!      ic     = ic     .and. 127 
!=ALL 
c 
c...Check for escape sequence
c
      if (iesc .eq. 0) then
          if (ic .ne. 27) go to 800
          iesc   = 1
          go to 600
      endif
c
c......In the midst of an escape sequence
c
      call gvtesc (ic,kchar,ic1,i2nd,iesc,iescfl,ierr)
      igo    = ierr   + 1
      go to (1000,700,100), igo
c
c
c.........Buffer up escape sequence
c
  600 NESC   = NESC   + 1
      ESCBUF(NESC) = ic
      go to 100
c
c......Invalid escape sequence
c......Issue buffered escape sequence
c
  700 IESCPT = IESCPT + 1
      kchar  = ESCBUF(IESCPT)
      go to 1000
c
c...Standard character
c
  800 kchar  = ic
      if (ic .eq. 127) return
      if (i2nd .eq. 0) go to 1000
      if (ic .lt. 32) go to 1000
      if (ic .ge. 97 .and. ic .le. 122) ic = ic - 32
      kchar  = ic     * (0-1)
      go to 1000
c
c...Erase 2nd function display
c
 1000 if (i2nd .eq. 0) go to 8000
      i2nd   = 0
      call fnc2nd (i2nd)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gvtesc (kc,kchar,kc1,k2nd,kesc,kescfl,kerr)
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
      subroutine gvtesc (kc,kchar,kc1,k2nd,kesc,kescfl,kerr)
c
      include 'menu.com'
c
      integer*4 kc,kchar,kc1,k2nd,kesc,kescfl,kerr,i
c
      character*1 lc,lc1
c
C VAX-START
      character*1 lesc(22),lc2
C VAX-END
c
C SUN-START
C     character*1 lesc(36),lesc1(10)
C SUN-END
c
C SGI-START
C     character*1 lesc(54),lesc1(9),lc2
C SGI-END
c
C VAX-START
      data lesc /'A','B','D','C','P','Q','R','S','w','x','y','m','t',
     1           'u','v','l','q','r','s','M','p','n'/
C VAX-END
c
C SUN-START
C     data lesc1 /'A','B','D','C','2','z','0','1','2','3'/
C
C     data lesc /'0','8','0','9','1','0','1','3','2','5','1','8',
C    1           '2','6','1','2','1','4','1','6','2','2','1','1',
C    2           '2','9','3','0','3','1','2','8','2','7','2','0'/
C SUN-END
C SGI-START
C     data lesc1 /'A','B','D','C','[','q','0','1','2'/
c
C     data lesc /'1','3','9', 'H',' ',' ', '1','5','0', '0','1','1',
C    1           '0','0','2', '0','0','8', '0','0','3', '0','1','0',
C    2           '0','0','6', '0','0','7', '0','1','2', '0','0','9',
C    3           '2','0','9', '2','1','3', '2','1','7', '0','0','5',
C    4           '0','0','4', '0','0','1'/
C SGI-END
c
      lc     = char(kc)
c
c...check for escape sequence
c
C VAX-START
      if (kesc .ne. 1) go to 200
      if (lc .ne. '[' .and. lc .ne. 'O') go to 650
      kesc   = 2
      go to 600
C
  200 do 300 i=1,22,1
          if (lc .eq. lesc(i)) go to 440
  300 continue
      go to 650
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
C 400     if (lc1 .eq. lesc(i) .and. lc .eq. lesc(i+1)) go to 640
C         i      = i      + 2
C         if (i .gt. 36) go to 650
C         go to 400
C 640     kescfl = (i-1) / 2 + 1 + 4
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
C SGI-START
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
c
C     if (kesc .eq. 4) then
C         i      = 1
C 400     if (lc1 .eq. lesc(i) .and. lc2 .eq. lesc(i+1) .and.
C    1        lc .eq. lesc(i+2)) go to 640
C         i      = i      + 3
C         if (i .gt. 54) go to 650
C         go to 400
C 640     kescfl = (i-1) / 3 + 1 + 4
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
C SGI-END
c
c...Recognized escape sequence
c
  440 if (i .eq. 5) go to 450
      kchar  = i      * (0-1)
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
c   SUBROUTINE:  gvtlin (cbuf,knc,kmxc,klin,kcol)
c
c   FUNCTION:  This routine gets a character string from the keyboard.
c              It allows the user a certain amount of editing features
c              when inputting the line.  If 'knc' is not equal to 0 when
c              'gvtlin' is called, then 'cbuf' will be displayed and
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
      subroutine gvtlin (cbuf,knc,kmxc,klin,kcol)
c
      include 'menu.com'
      include 'com.com'
c
      integer*4 knc,kmxc,klin,kcol,mode
c
      character*(*) cbuf
c
      integer*4 strlen1,ic,is,insmod,tmp
c
      character*80 sbuf,obuf
c
c...Initialize routine
c
c
c...Added for NCL-VT by Paul
c...12/04/91
c
c     if(ifl(35) .eq. 2) then
c         mode = 1
c         call termsw(mode) 
c     endif

      is     = 1
      insmod = 0
      sbuf   = cbuf
      if (knc .lt. 0) knc = 0
c
c...Display current line
c
101   if (klin .ne. -1) call plot (klin,kcol)
      call dmpbuf (cbuf,knc)
      call clreol
      if (knc .ne. 0) then
          if (klin .ne. -1) then
              call plot (klin,kcol)
          else
              call crslft (knc)
          endif
      endif
      call wflush
c
c...Get character from keyboard
c
  100 call gvtchr (ic)
      if (ic .eq. -3) go to 200
      if (ic .eq. -4) go to 300
      if (ic .eq. 1) go to 600
      if (ic .eq. 127) go to 700
      if (ic .eq. 4) go to 800
      if (ic .eq. 8) go to 400
      if (ic .eq. 10) go to 500
      if (ic .eq. 13) go to 1100
      if (ic .eq. -1) go to 1300
      if (ic .eq. 23) go to 1400
      if( ic.eq.11) go to 1410
      if( ic.eq.18) go to 1415
      if( ic.eq.16) go to 1420
      if( ic.eq.21) go to 1425
      if( ic.eq.24) go to 1430
      if( ic.eq.-2) go to 1435
      if( ic.eq.12 .and. ifl(35).eq.2 .and. ifl(350).eq.1) goto 1445
      go to 1500

c
c...Left arrow
c...Move cursor left
c
  200 if (is .eq. 1) go to 100
      is     = is     - 1
      if (klin .ne. -1) then
          call plot (klin,is+kcol-1)
          call wflush
      else
          call crslft (1)
          call wflush
      endif
      go to 100
c
c...Right arrow
c...Move cursor right
c
  300 if (is .gt. knc) go to 100
      is     = is     + 1
      if (klin .ne. -1) then
          call plot (klin,is+kcol-1)
          call wflush
      else
          call crsrgt (1)
          call wflush
      endif
      go to 100
c
c...Backspace
c...Move cursor to beginning of line
c
  400 if (klin .ne. -1) then
          call plot (klin,kcol)
      else
          call crslft (is-1)
      endif
      is     = 1
      go to 100
c
c...Line feed
c...Move cursor to End of line
c
  500 if (klin .ne. -1) then
          call plot (klin,kcol+knc)
      else
          call crsrgt (knc-is+1)
      endif
      is     = knc    + 1
      go to 100
c
c...^A
c...Toggle insert mode
c
  600 insmod = insmod + 1
      if (insmod .eq. 2) insmod = 0
      go to 100
c
c...Delete
c...Delete char to left
c
  700 if (is .eq. 1) go to 100
      obuf = sbuf(1:is-2) // sbuf(is:knc) // ' '
      sbuf   = obuf
      is     = is     - 1
      if (klin .ne. -1) then
          call plot (klin,is+kcol-1)
      else
          call crslft (1)
      endif
      call dmpbuf (sbuf(is:knc),knc-is+1)
      knc    = knc    - 1
      if (klin .ne. -1) then
          call plot (klin,is+kcol-1)
      else
          call crslft (knc-is+2)
      endif
      go to 100
c
c...^D
c...Delete char under cursor
c
  800 if (is .gt. knc) go to 100
      obuf   = sbuf(1:is-1) // sbuf(is+1:knc)
      sbuf   = obuf
      call dmpbuf (sbuf(is:knc),knc-is+1)
      knc    = knc    - 1
      if (klin .ne. -1) then
          call plot (klin,is+kcol-1)
      else
          call crslft (knc-is+2)
      endif
      go to 100
c
c...CR
c...End of input
c
 1100 cbuf   = sbuf(1:knc)
      knc    = strlen1(cbuf)
      return
c
c...Up arrow
c...Go to previous line
c
 1300 if (ifl(35) .eq. 2) then
         cbuf   = sbuf(1:knc)
         call wrtpp(cbuf,knc,nline-1,0)
         cbuf = '*sk/-1'
         knc = 6
      else
         if (CURSES .eq. 0) go to 100
         knc    = -2
      endif
      return
c
c...Down arrow
c...Go to next line  
c
 1435 if (ifl(35) .eq. 2) then   
         cbuf   = sbuf(1:knc)
         call wrtpp(cbuf,knc,nline-1,0)
         cbuf = '*sk/+1'   
         knc = 6 
         return
      endif
c
c...^L
c...*menu command.
c...Added for NCL501+ mode. Paul 03/30/92
c...for NCL501+ mode only.
c
1445     cbuf = '*menu'
         knc = 5
         return
c
c...^W
c...Redisplay page
c
 1400 if (ifl(35) .eq. 2 .and. ifl(141) .eq. 0) then
         knc    = -3

         return
      else
         if (CURSES .eq. 0) go to 100
         knc    = -3

         return
      endif
c
c...^U
c...Delete to begining of line.
c
 1425 if (ifl(35) .eq. 2) then
          if (is .gt. 1) then
c            do 45  i=is-1,1,-1
c45          sbuf(i:i) = ' '
             do 45 i=1,80-is,1
45           sbuf(i:i) = sbuf(i+is-1:i+is-1)
             do 46 i=80-is,80,1
46           sbuf(i:i) = ' '
     
          endif
         knc    = strlen1(sbuf)
         cbuf   = sbuf(1:knc)
         is = 1
         goto 101
      endif

c 
c...^X 
c...Delete to end of line.
c 
 1430 knc    = is - 1
      call clreol
      go to 100

c
c...^K
c... quit (if pp file was not open at the begining of session))
c
1410  cbuf = '*quit'
      knc = 5
      return
c
c...^R
c... *run  ( in VT mode only)                                  
c
1415  cbuf = '*run'
      knc = 4
      return

c
c...^P
c... *sh/src,s ( in VT mode only)
c
1420  cbuf = '*sh/src,s'
      knc = 9
      return

c
c...Standard character
c
 1500 if (ic .lt. 32) go to 100
      if (insmod .eq. 0 .or. is .gt. knc) go to 1550
c
c......Insert mode is on
c
      if (knc .eq. kmxc) go to 100
      obuf   = sbuf(1:is-1) // char(ic) // sbuf(is:knc)
      sbuf   = obuf
      knc    = knc    + 1
      call dmpbuf (sbuf(is:knc),knc-is+1)
      is     = is     + 1
      if (klin .ne. -1) then
          call plot (klin,is+kcol-1)
      else
          call crslft (knc-is+1)
      endif
      go to 100
c
c......Insert mode is off
c
 1550 if (is .gt. kmxc) go to 100
      sbuf(is:is) = char(ic)
      if (is .gt. knc) knc = knc + 1
      tmp = 1
      call dmpbuf (sbuf(is:is),tmp)
      is     = is     + 1
      go to 100
      end
