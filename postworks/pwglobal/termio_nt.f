c
C***********************************************************************
c
C   FILE NAME:  termiont.for
C   CONTAINS:
C               clreol  clreos  clrscr  crslft  crsrgt  dmpbuf  gtch
C               getmcr  hilit   plott   rstcur  savcur  scrol   trmatt
C               trmmsg  trmnl   trmrst
c   Most function do nothing (jusk dum function for Win NT
C
C     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
C           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        termio_nt.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:18
c
C***********************************************************************
c
C WNT-START


      subroutine  iostat_msg(flag, crms)
      character*(*) crms
      integer*4 flag

      return
      end
c
      subroutine trmnl (knt)
c
      integer*4 knt
      return
      end
C WNT-END

c***********************************************************************
c
c   SUBROUTINE:  dmpbuf (cbuf,knc)
c
c   FUNCTION:  This routine writes a character string to the screen.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to write to the screen.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: none.
c
c***********************************************************************/
c
c...not use now
C WNT-START
      subroutine dmpbuf (cbuf,knc)

      include 'postworks_nt.inc'

      character*200 cbuf
      integer*4 knc
      call displn(cbuf,knc)
      return
      end

      subroutine crsrgt (knt)
      integer*4 knt
      return
      end


      subroutine hilit (kfl)
c
      integer*4 kfl

      return
      end


      subroutine plott (klin,kcol)
c
      integer*4 klin,kcol
      return
      end

      subroutine rstcur
      return
      end

      subroutine savcur

      return
      end

      subroutine scrol (kbeg,kend,klin,kdir)
c
      integer*4 kbeg,kend,klin,kdir

      return
      end


      subroutine trmrst
      return
      end

      subroutine gtch (kch)
c
      integer*4 kch
      return
      end

      subroutine clreol
      return
      end
C WNT-END
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
C WNT-START
      subroutine getchr (kchar)
c
      integer*4 kchar, ret

      call getchrc(kchar)
      return
      end

      subroutine clreos
      return
      end
      subroutine clrsrc
      return
      end

      subroutine trmmsg(msg)
      include "postworks_nt.inc"
      character*80 msg
      integer*4 nc, flag, strlen1
      flag = 1
      nc = strlen1(msg)
      call add1dispmsg(msg, nc, flag)
       return
           end
C WNT-END
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
C WNT-START
      subroutine getlin (cbuf,knc,kmxc,klin,kcol)
      integer*4 knc,kmxc,klin,kcol, status
c
      character*(*) cbuf

c
      include 'menu.inc'
c
c
      integer*4 strlen1,ic,is,insmod

c
      character*80 sbuf,obuf

      call getlnc(cbuf, LOC(knc))
      return
      end

      subroutine clrscr
      return
      end
C WNT-END
