C*********************************************************************
C*    NAME         :  putw2.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putw2.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:33
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putw2(iline)
c*       this routine writes a line to window 2   
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine putw2(iline)

      include 'com8a.com'

      character*1 sep,win(23)
      integer*4 kbeg, kend, klin, kdir, iline, ncc

C                                          SET SCROLL AREA TO LINES 6 THRU 13
      DATA WIN   /' ','[','6',';','1','3','r',
C                                          SET CURSOR AT LINE 13 COLUMN 1
     1            ' ','[','1','3',';','1','H',
C                                          SCROLL UP 1 LINE
     2            ' ','E',
C                                          SET SCROLL AREA TO LINES 1 THRU 24
     3            ' ','[','1',';','2','4','r'/

      if (SIMCOM) return

      win(1) = char(27)
      win(8) = char(27)
      win(15) = char(27)
      win(17) = char(27)

      if (.not.echo.or.ifl(35).eq.1) go to 88888
      if (.not.ksr .and. ifl(131).eq.0) then

c              handle writing to tek 4115 and 4125 window 2 differently
          if (ifl(187) .eq. 6 .or.
     x       (ifl(187) .eq. 5 .and. ifl(259) .eq. 2)) then

c                  reset line number pointer if outside lines 6-13 range
              if (ifl(188).lt.6 .or. ifl(188).gt.13) ifl(188)=6

c                  move cursor to line 6 if line 13 is going to be written on
              if (ifl(188).eq.13) then
                  call crsplt (6,1)

c                  move cursor to line after line to be written if line is 6-12
              else
                  call crsplt (ifl(188)+1,1)
              endif

c                          erase line after line to be written
              call erslin
          else
cuni              write (5,1020) win
c                             set up scrolling window in 501
c
c...Added check for NCL-VT mode
c...Paul  -  10/9/91
c...Old version was:
c   if (ifl(268).eq.1) write (conlun,1020) win
c
              if (ifl(268).eq.1 .and. ifl(35) .ne. 2)
     X write (conlun,1020) win
1020          format ('+',23a1,$)
          endif
      endif
      sep=':'
      if (IRCTYP .eq. 1) sep = '#'
      ncc = nccimg
      if (ncc .gt. 72) ncc = 72
      write (cout,150)iline,sep, cimage(1:ncc)

c          on vax the format should only print 71 characters since a 72
c          character write will wrap a non-blank 72nd character to the
c          next line and wipe out the window 2/3 separator line.
150   format(i6,a1,' ',a)
      if (ifl(187) .eq. 6 .or.
     x   (ifl(187) .eq. 5 .and. ifl(259) .eq. 2)) then
          call putmsg (cout,80,ifl(188),0)
          ifl(188)=ifl(188)+1
cc
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c     else
c         call putmsg (cout,80,13,0)
c     endif
c
c
      else if (ifl(35) .eq. 2) then
c
c....VX mode
c
         if (ifl(322) .eq. 1) then
             call vx_putw2 (cout,80)
         else if (echo .and. ifl(131) .eq. 0) then
             kbeg = 6
             kend = 13
             klin = 1
             kdir = 1
             call scroll1 (kbeg,kend,klin,kdir)
             call putmsg_w2 (cout,80,8,0)
         endif
      else
          call putmsg (cout,80,13,0)
      endif
88888 ifl(47)=0

99999 continue
      end
