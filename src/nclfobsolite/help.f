C*********************************************************************
C*    NAME         :  help.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       help.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:11
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine help (stat)
C*       display portions of the documentation help file at the terminal.
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
      subroutine help (stat)

      include 'com4a.com'

      integer*2 stat, helprn, helpel, helpi2(40), tkindx
      integer*2 tknptr(50), toplin, numlin, offset, icnt, sv131
      character*1 sign
      character*2 c2hlpi(40)
      character*6 tkn, testix, svtkn, svtok
      character*8 helpix(10)
      character*(MAX_PATH) hfile
      real*8 r8hlpi(10)
      equivalence (c2hlpi, helpix, leq1), (leq1, r8hlpi, helpi2)
      equivalence (tkn, token2)
      equivalence (lastrn, ifl(36))
      logical leq1, eof, eofind, first
      character*80 scopt1, scopt2, scopt3, scopt4
      integer*4 kmxc, klin, kcol, strlen1

c      data scopt1
c     x /'  f = forward    n = next entry       s = 10 lines   <num> = <n
c     xum> lines        '/
c      data scopt2
c     x /'  b = backward   p = previous entry   l = 19 lines   q = quit
c     x                 '/
c23456789-123456789-123456789-123456789-123456789-123456789-123456789-12
      data scopt1
     x /'  f = forward    n = next entry       <num> = <num> lines      
     x  '/
      data scopt2
     x /'  b = backward   p = previous entry   q = quit
     x                 '/
      data scopt3
     x /'enter scroll command or index entry:  help index displayed
     x                 '/
      data scopt4
     x /'  f = scroll forward    b = scroll backward    q = quit
     x                 '/

c
c...VX
c
      if (ifl(35) .eq. 2 .and. ifl(322) .eq. 1) return

      sv131 = ifl(131)
      ifl(131) = 0
      stat = 0
      idir = 1
      toplin = 14
      if (ifl(35).eq.0) call opnwin()
      call nclwsz (numlin)
      numlin = numlin - 1
c      numlin = 9
      first = .true.

      close (unit = hlplun)
      call flname(2, 'dochelp{', hfile)
      call flopen(hlplun, hfile, 'OLD', 'DIRECT', 'UNFORMATTED', 84,
     x          'NULL',  ierr)
c      open (unit = 6, file = 'nc00:dochelp.ncl' ,status = 'old',
c     1       access = 'direct')
c          find end of file record number in index
      eofind = .true.
      svtok = tkn
      tkn = 'ENDFIL'
      go to 50

5     eofind = .false.
      tkn = svtok
      lastrn = tknptr(tkindx)
10    if (.not.(ityp .ne. 7)) go to 4199

c **********************************************************************
c **********************************************************************
c     read index records and load array of record pointers for the
c     requested keyword.
c **********************************************************************
c **********************************************************************

c              check index for keyword   '*help/keyword'
          if (.not.(ityp .eq. 1 .or. ityp .eq. 2)) go to 3199
50            write (svtkn, 2010) tkn

c                  load up array of record numbers that point to index
c                  occurances of the keyword in the help file
              tkindx = 0
              do 100 helprn = 0, 99
cuni                  read (6, rec = helprn+1) (r8hlpi(j), j = 1, 10)
                  read (hlplun, rec = helprn+1) (r8hlpi(j), j = 1, 10)
                  do 100 helpel = 1, 10
                      write (testix, 2010) helpix(helpel)
2010                  format (a6)
                      if (tkn .eq. testix) then
                          tkindx = tkindx + 1
                          if (tkindx .gt. 50) then
                              ifl(2) = 296
                              go to 99999
                          endif

c                                  load help file record number into token
c                                  pointer array
                          tknptr(tkindx) = helpi2(helpel*4)
                      else
                          if (tkindx .gt. 0) then

c                                  mark the end of the token pointer entries
                              tknptr(tkindx + 1) = 0
                              go to 150
                          endif
                      endif
100               continue
              ifl(2) = 160
              go to 99999

c **********************************************************************
c **********************************************************************
c     check if other than the 1st occurance of the keyword is requested
c **********************************************************************
c **********************************************************************

150           if (eofind) go to 5
160           if (nextyp .eq. 9) then
                  call parsit
                  if (ityp .ne. 7) then
                      if ((ityp .eq. 2 .and. ist .eq. 2) .or.
     x                     ityp .eq. 3) then

c                              get help file record number from token pointer
c                              array
                          icnt = itv
                          if (ityp .eq. 2) icnt = tv
                          if (icnt .le. 1) icnt = 1
                          if (icnt .le. tkindx) then
                              helprn = tknptr(icnt)
                          else
                              ifl(2) = 160
                              go to 99999
                          endif
                      else
                          ifl(2) = 7
                          go to 99999
                      endif
                  else
                      ifl(2) = 7
                      go to 99999
                  endif

c                      default to first index occurance
              else if (nextyp .eq. 11) then
                  icnt = 1
                  helprn = tknptr(1)
              else
                  ifl(2) = 297
                  go to 99999
              endif

c **********************************************************************
c **********************************************************************
c     display the help file records for the selected index to the screen
c **********************************************************************
c **********************************************************************


510           eof = .false.
              iline = toplin
c
c...Added check for NCL-VT mode
c...Paul  -  10/15/91
c
              if (ifl(35) .eq. 2) numlin = 9
              call ersw3(15,1)

              do 600 i = helprn, helprn + numlin
                  if (.not.(eof)) go to 560
540                   do 550 j = 1, 10
550                       helpix(j) = ' '
                      go to 580
560               continue
cuni                      read (6, rec = i+1, err = 570)
                      read (hlplun, rec = i+1, err = 570)
     x                     (r8hlpi(j), j = 1, 10)
                      go to 580
570                   eof = .true.
                      go to 540
580               continue
                  iline = iline + 1
600               call putmsg (helpix, 80, iline, 0)
c              if (first) then
c                  first = .false.
                  offset = helprn - tknptr(icnt)
                  sign = ' '
                  if (offset .gt. -1) sign = '+'

                  write (cout, 1020) svtok, icnt, sign, offset
1020              format ('enter scroll command?  help index entry ',
     x                    'displayed: ', a6, '(', i2, ') ', a,
     x                    i6, ' lines' )

c                     read response to scroll message
c
c...Added check for NCL-VT mode
c...Paul  -  10/15/91
c...Old version was:
c   call nclpmt (cout, cin)
c                if (ifl(268) .eq. 1) then
c                    call putmsg (scopt1, 80, 3, 0)
c                    call putmsg (scopt2, 80, 4, 0)
c                endif
c
          if (ifl(268) .eq. 1) then
             call putmsg (scopt1, 80, 3, 0)
             call putmsg (scopt2, 80, 4, 0)
          endif

          if (ifl(35) .eq. 2) then
             call putmsg(cout,78,2,2)
             nccin = 0
             kmxc =6
             kcol = 9
             klin = 1
             call gvtlin(cin,nccin,kmxc,klin,kcol)
          else
             nccout = strlen1(cout)
             call nclpmt (cout, nccout, cin, nccin)
          endif

c              endif
              inx = 1
              call parsit
              if (ityp .eq. 3) then
                  idir = 1
                  helprn = helprn + itv - (numlin*idir)
              else if ((tkn .eq. 'F' .or. tkn .eq. 'f') .and. 
     x                  nextyp .eq. 11) then
                  idir = 1
              else if ((tkn .eq. 'B' .or. tkn .eq. 'b') .and. 
     x                  nextyp .eq. 11) then
                  idir = -1
              else if ((tkn .eq. 'N' .or. tkn .eq. 'n') .and. 
     x                  nextyp .eq. 11) then
                  idir = 1
                  icnt = icnt + 1
                  if (icnt .gt. tkindx) icnt = 1
                  helprn = tknptr(icnt) - (numlin*idir)
              else if ((tkn .eq. 'P' .or. tkn .eq. 'p') .and. 
     x                 nextyp .eq. 11) then
                  idir = 1
                  icnt = icnt - 1
                  if (icnt .lt. 1) icnt = tkindx
                  helprn = tknptr(icnt) - (numlin*idir)
c              else if ((tkn .eq. 'l' .or. tkn .eq. 'L') .and.
c                        nextyp .eq. 11) then
c                  toplin = 5
c                  numlin = 18
c                  helprn = helprn - (numlin*idir)
c              else if (tkn .eq. 's' .and. nextyp .eq. 11) then
c                  if (toplin .eq. 5) then
c                      do 720 i = 6, 13
c                          call crsplt (i, 1)
c720                       call erslin
c                      call putmsg('***************************  interact
c     xive processing  ***************************', 80, 14, 0)
c                  endif
c                  toplin = 14
c                  numlin = 9
              else if (((tkn .eq. 'Q' .or. tkn .eq. 'q') .and. 
     x                nextyp .eq. 11) .or. ifl(256) .eq. 1) then
                  go to 900

              else if (tkn .eq. '*') then
                  stat = 1
                  go to 900

              else if (ityp .ne. 7) then
c          if there is non-blank input assume an exit from the help
c          routine
                  stat = 2 
                  go to 900

              endif
              offset = helprn - tknptr(icnt) + (numlin*idir)
              sign = ' '
              if (offset .gt. -1) sign = '+'
              helprn = helprn + (numlin*idir)
              if (helprn .lt. 100) helprn = 100
              if (helprn .gt. lastrn) helprn = lastrn
              go to 510
3199      continue
              ifl(2) = 160
              go to 99999
3299      continue

          go to 4299
c **********************************************************************
c **********************************************************************
c     display the help document index in window 3
c **********************************************************************
c **********************************************************************

4199  continue
          istart = 0
1050      iline = 14
c
c...Added check for NCL-VT mode
c...Paul  -  10/15/91
c 
          if (ifl(35) .eq. 2) numlin = 9
          call ersw3(15,1)

1100      do 1200 i = istart, istart + numlin
              iline = iline + 1
cuni              read (6, rec = i+1) (r8hlpi(j), j = 1, 10)
              read (hlplun, rec = i+1) (r8hlpi(j), j = 1, 10)
              do 1150 k = 4, 40, 4
1150              c2hlpi(k) = '  '
1200      call putmsg (helpix, 80, iline, 0)
          if (ifl(268).eq.1) then
c
c...Added check for NCL-VT mode
c...Paul  -  10/15/91
c...Old version was:
c   call putmsg (scopt3, 80, 2, 0)
c   call putmsg (scopt4, 80, 3, 0)

            if (ifl(35) .eq. 2) then
               call putmsg (scopt3, 80, 3, 0)
               call putmsg (scopt4, 80, 4, 0)
            else
               call putmsg (scopt3, 80, 2, 0)
               call putmsg (scopt4, 80, 3, 0)
            endif
          endif
          cout='ENTER SCROLL COMMAND: (f = forward, b = back, q = quit)'
c
c...Added check for NCL-VT mode
c...Paul  -  10/15/91
c...Old version was:
c   call nclpmt (cout, cin)
c
          if (ifl(35) .eq. 2) then
             call putmsg(cout,55,2,3)
             nccin = 0
             kmxc =6
             kcol = 9
             klin = 1
             call gvtlin(cin,nccin,kmxc,klin,kcol)
          else
             nccout = strlen1(cout)
             call nclpmt (cout, nccout, cin, nccin)
          endif

          inx = 1
          call parsit
          svtok = tkn
          if (((tkn .eq. 'Q' .or. tkn .eq. 'q') .and. 
     x         nextyp .eq. 11) .or. ifl(256) .eq. 1) go to 900
          if (tkn .eq. '*') then
              stat = 1
              go to 900
          endif
          if ((tkn .ne. 'F' .and. tkn .ne. 'f') .and. 
     x         (tkn .ne. 'B' .and. tkn .ne. 'b') .and.
     x          tkn .ne. ' ') go to 10
          if (tkn .eq. 'F' .or. tkn .eq. 'f') idir = 1
          if (tkn .eq. 'B' .or. tkn .eq. 'b') idir = -1
          istart = istart + (10*idir)
          if (istart .gt. 90) istart = 90
          if (istart .lt. 0) istart = 0
          go to 1050

4299  continue
c
c...Added check for NCL-VT mode
c...Paul  -  10/16/91
c...Old version was:
c   call crsplt (i, 1)
c   call erslin
c
900   do 910 i = 2, 4
          if(ifl(35) .eq. 2) then
             klin = i
             kcol = 1
             call plot(klin,kcol)
             call clreol
          else
             call crsplt (i, 1)
             call erslin
          endif
910   continue
99999 close (unit = hlplun)
      if (ifl(2) .ne. 0) then
          ist = 2
          call error (ifl(2))
      endif

      ifl(131) = sv131
      return
      end
