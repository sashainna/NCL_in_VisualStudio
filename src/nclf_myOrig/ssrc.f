C*********************************************************************
C*    NAME         :  ssrc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        ssrc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:45
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ssrc (stat)
C **  last revision: to change alternate file i/o unit from 44 to 45  **
C **                                                                  **
C **  purpose of subroutine: to display 10 records of the source file **
C **   to window three on the terminal.                               **
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
      subroutine ssrc (stat)

      include 'com4a.com'

      integer*2 stat, toplin, nmln,tolin,nc1
      integer*4 beglin, istart, iend, numlin, find, istsav,
     1          i
      integer*2 svidir
       character*1 afname(44),sep
       character*6 tkn
       equivalence (tkn, token2)
       character*44 fname
       equivalence (fname,afname)
       character*(MAX_LEN) cbuf
       character*80 scopt1, scopt2, scopt3, scopt4
       logical sveof, scroll, fn
       integer*4 strlen1, kmxc, klin, kcol, nc

      data scopt1
     x /' f = forward   n = next occurrence      s = 10 lines  <num> = <
     xnum> lines       '/
      data scopt2
     x /' b = backward  p = previous occurrence  l = 19 lines  sk = *skip
     x/to  q = exit    '/

      data scopt3
     x /'  f = forward     n = forward     s = 10 lines    <num> = <num>
     x lines           '/
      data scopt4
     x /'  b = backward    p = backward    l = 19 lines    sk = *skip/to
     x    q = exit     '/
c
c...Added by Paul for VT MODE
c...11/07/91
c
c...VX
c     if (ifl(35) .eq. 2 .and. ifl(322) .eq. 1) return

      if (ifl(35) .eq. 2) call ersw3(15,1)
c
      numlin = 9
      if (ifl(268).eq.0) then
          call nclwsz(nmln)
          numlin = nmln - 2
      endif

c          override numlin if this call was from a *find command
      if (findss) numlin = 0

      toplin = 15
       if (ifl(253) .eq. 0 .or. ifl(253) .eq. 4) then
         if (nline .lt. ifl4(1)-5) then
           beglin = nline
         else
           nc     = 1
           beglin = max(nc,ifl4(1)-5)
         endif
         ifl(253) = 4
       else
         beglin =  ifl4(3)
       endif
       sveof=srceof
       srceof=.false.
       scroll=.false.
       fn=.false.
       fname=' '
       tolin = 0
       if (nextyp.eq.11) then
c
c         changed (beglin-5) to (beglin-1) by: kathy
c
           istart=beglin-1
           if (istart.lt.0) istart=0
           iend=istart + numlin
       else
           if (ain(inx).ne.'*') go to 10
5              istart=beglin-1
               iend=istart + numlin
               go to 20
10         call parsit
           if (ityp.eq.3) then
               istart=itv-1
           else if (tkn.eq.'S') then
               fname='this file '
               idir=1
               scroll=.true.
                write (cout,2011)
2011            format('Listing current file.')
c
c...Added check for NCL-VT mode
c...Paul  -  10/14/91
c...Old version was:
c   call puterm(cout)
c
                if (ifl(35) .eq. 2) then
                   call putmsg(cout,79,2,53)
                else
                   call puterm(cout)
                endif

                goto 5

           else if (tkn.eq.'FN') then
               icnt=0
12             icnt=icnt+1
               inx=inx+1
               afname(icnt)=ain(inx)
               if (ain(inx).eq.' ') go to 13
               go to 12

13             icnt=icnt-1
              open (unit=quelun,file=fname,status='old',
     x              err=115)
              open (unit=scrlun,file='partpgm.tmp',recl=20,
     x              access='direct',status='new')
              ir=0
131           read (quelun,1070,end=14) cbuf
1070          format (a80)
              ir=ir+1
              nc = strlen1(cbuf)
              write (scrlun,rec=ir) cbuf(1:nc)
              go to 131

115           ist=2
              call error (145)
              go to 99999

14             fn=.true.
               pmode=0
               idir=1
               scroll=.true.
               istart=0
               iend=9
          		write (cout,2012) fname
2012      		format('Listing file:',a44)
c 
c...Added check for NCL-VT mode 
c...Paul  -  10/14/91 
c...Old version was: 
c   call puterm(cout) 
c 
                if (ifl(35) .eq. 2) then 
                   call putmsg(cout,79,2,53)
                else 
                   call puterm(cout)
                endif 

               go to 20
           else
               call error(37)
               go to 99999
           endif
           if (nextyp.eq.11) then
               iend=istart + numlin
           else
               call parsit
               if (ityp.eq.3) then
                   iend=itv-1
                   call parsit
               else if (tkn.eq.'END'.and.length.eq.3) then
                   iend=1000000
                   call parsit
               else
                   call error(37)
                   go to 99999
               endif
           endif
           if (nextyp.ne.11) then
               call error(4)
               go to 99999
           endif
       endif
20     if (.not.ksr) then
           iend = numlin
       else
           iend=iend-istart
       endif
       istsav=istart 

       if (numlin .eq. 9)  call ersw3(15,1)
       if (numlin .eq. 18) call ersw3(6,1)

       cout = '  '
       call putmsg (cout, 2, 16, 0)
       do 100 i=0,iend
           if (fn) then
               read (scrlun,rec=istart+1,err=51) cbuf
2000           format (a)
               srceof=.false.
               go to 54

51             srceof=.true.
               go to 54
           endif

53         call getsrc (cbuf, nc, istart,0)

c                      it's a line that has been deleted
c           if (ceof .eq. 'eof ') srceof = .true.
cc           if (cbuf(1:3) .eq. 'eof') srceof = .true.
54         if (srceof) then
               cout='* * *   e n d   o f   f i l e   * * *'
               call putmsg (cout, 80, (tolin + i), 0)
               if (i.lt.iend) then
                 cout=' '
                 call putmsg (cout, 80, (tolin + i + 1), 0)
               endif
               go to 110
           endif

55         istart=istart+1
           sep=':'
           if (IRCTYP .eq. 1) sep = '#'
           write(cout,60)istart,sep,cbuf(1:nc)
60         format(i6,a1,' ',a)
           nc1    = nc     + 8
           call putmsg (cout, nc1, (toplin + i), 0)
100    continue

110    continue
c          now set number of lines to proper value.  this only has any real
c          effect if this was a *find ssrc call.  see findss logical set in
c          find.for
      if (ifl(268).eq.0) then
          call nclwsz(nmln)
          numlin = nmln - 2
      else
          if (numlin.eq.0) numlin = 9
      endif
120   if (scroll) then
          call swinpt()
          if (ifl(268) .ne. 0) then
              if (ifl(253) .eq. 4) then
                  call putmsg (scopt3, 80, 3, 0)
                  call putmsg (scopt4, 80, 4, 0)
              else
                  call putmsg (scopt1, 80, 3, 0)
                  call putmsg (scopt2, 80, 4, 0)
              endif
          endif
          write (cout,2010)
2010      format('ENTER SCROLL COMMAND: (f=forward,b=back,q=quit)')
c
c...Added check for NCL-VT mode
c...Paul  -  10/14/91
c...Old version was:
c   call nclpmt (cout, cin)
c
          if (ifl(35) .eq. 2) then
             call putmsg(cout,49,2,2)
             nccin = 0
             kmxc =2
             kcol = 9
             klin = 1
             call gvtlin(cin,nccin,kmxc,klin,kcol)
          else
             nccout = strlen1(cout)
             call nclpmt (cout, nccout, cin, nccin)
          endif
          call touppr (cin(1:nccin),cin)
          inx = 1
          call parsit
          if (nextyp .eq. 0) nextyp = 11
          iadd = numlin
          svidir = idir
          if (ityp.eq.3) then
              idir=1
              iadd=itv
          else if (((tkn .eq. 'F') .or.
     x              (tkn .eq. 'f') .or.
     x              (tkn .eq. 'N') .or.
     x              (tkn .eq. 'n')) .and.
     x             nextyp .eq. 11) then
              idir=1
          else if (((tkn .eq. 'B') .or.
     x              (tkn .eq. 'b') .or. 
     x              (tkn .eq. 'P') .or.
     x              (tkn .eq. 'p')) .and.
     x             nextyp .eq. 11) then
              idir=-1
          else if (((tkn .eq. 'L') .or.
     x              (tkn .eq. 'l')) .and. 
     x             nextyp .eq. 11) then
              toplin = 6
              numlin = 18
              istsav = istsav - (iadd * idir)
          else if (((tkn .eq. 'S') .or.
     x              (tkn .eq. 's')) .and. 
     x             nextyp .eq. 11) then
              toplin = 15
              numlin = 9
              istsav = istsav - (iadd * idir)
          else if ((((tkn .eq. 'Q') .or.
     x               (tkn .eq. 'q')) .and.
     x              nextyp .eq. 11) .or. 
     x             ifl(256).eq.1) then
              go to 900

          else if (((tkn .eq. 'SK') .or.
     x              (tkn .eq. 'sk')) .and. 
     x             nextyp .eq. 11) then
              svll = istsav
              if (ifl(37) .ne. 3) svll = svll + 1
              go to 900

          else if (tkn.eq.'*') then
              stat=1
              ifl(44)=5
              go to 900

          else if (ityp.ne.7) then
              call nclf_putw2 (1,cin,nccin,irctyp)
              stat=2
              go to 900

          endif
          if (ifl(253) .eq. 4 .or. tkn .eq. 'F' .or. tkn .eq. 'f' .or.
     x                             tkn .eq. 'B' .or. tkn .eq. 'b' .or. 
     x                             tkn .eq. ' ' .or. ityp .eq. 3) then
              istart=istsav+(iadd*idir)
          else if (ifl(253) .eq. 2 .or. ifl(253) .eq. 3) then
              cout=' '
              if (ifl(253) .eq. 2) then
                  ifl4(3) = find(cout, 0, idir, ifl4(3))
              else
                  ifl4(3) = find(cout, 1, idir, ifl4(3))
              endif
              idir = svidir
              istart = ifl4(3)-1
          endif
          if (istart .ne. ifl4(3)-1 .and.
     x        ifl4(1)-numlin .lt. istart) istart = ifl4(1) - numlin
          if (istart.lt.0) istart=0
          iend=istart + numlin
          srceof=.false.
          go to 20

      else
          srceof=sveof
      endif
900   if (ifl(35).eq.1) goto 99999
c
c...Added check for NCL-VT mode
c...Paul  -  10/16/91
c...Old version was:
c   call crsplt (i, 1)
c   call erslin
c
      do 910 i = 2, 4
          if(ifl(35) .eq. 2) then
             klin = i
             kcol = 1
             call plot(klin,kcol)
             call erslin
          else
             call crsplt (i, 1)
             call erslin
          endif
910   continue

99999 if (fn) then
          close (unit=quelun)
          close (unit=scrlun,status='delete')
      endif

      return
      end
