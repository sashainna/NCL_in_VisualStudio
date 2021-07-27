C*********************************************************************
C*    NAME         :  urest.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*        urest.for , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*        10/27/16 , 13:59:38
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine urest (svtkn,isvl,stat)
c*       this routine parses all control commands other than show and set/reset
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
      subroutine urest (svtkn,isvl,stat)
 
      include 'com8a.com'
      include 'mocom.com'
      include 'ifcom.com'
 
      integer*2 stat,isvl
      integer*4 find, opened
      character*6 svtkn
 
      equivalence (iquit,quit)
      character*6 tkn,qimage
      character*1 aimage(MAX_LEN)
      character*2 svtkn2
      character*3 svtkn3
      character*256 comstr
      equivalence (svtkn2,svtkn3)
      character*(MAX_PATH) temp,temp2
      character*80 msg1, msg2
      character*(MAX_LEN) cbuf,msg
      integer*2 j,ifini(2),ipos,ipos2,numf
	  character*1 endl
	  data endl /0/
 
      integer*4 i4fini(2),savepp,iret,isavep,newnln,savepm,isrc,ii
      integer*4 istart,istop,tnum
c
      integer*4 larg
      data larg /1000000/
      equivalence (aimage,cimage),(qimage,cin)
      equivalence (fini,ifini,i4fini)
      equivalence (isvmod,ifl(48))
 
      integer*4 strlen1, klin, kcol, knc, kmxc,nc1,nc2
      integer*4 curses
 
      svtkn3=svtkn
      tkn = token2
      if (.not.((svtkn.eq.'RUN'.and.isvl.eq.3).or.
     1    (svtkn2.eq.'RU'.and.isvl.eq.2))) go to 199
c                                                      ******* run
          ifl(37)=3
          if (ityp.eq.7.or.(tkn.eq.'END'.and.length.eq.3)) then
              pmode=larg
          else if (ityp.eq.3) then
              pmode=itv
          else if (tkn.eq.'STEP') then
              ifl(101)=1
              ifl(37)=6
              pmode=larg
c
c         pass control to command line
c
              ifl(275) = 2
c
c.....RUN to macro call
c
          else if (tkn.eq.'MACRO') then
              ifl(391) = 1
              pmode=larg
c
c.....RUN to macro end
c
          else if (tkn.eq.'TERMAC') then
              ifl(391) = 2
              pmode=larg
c
c.....RUN to loop start
c
          else if (tkn.eq.'LOOPST') then
              ifl(391) = 3
              pmode=larg
c
c.....RUN to loop end
c
          else if (tkn.eq.'LOOPND') then
              ifl(391) = 4
              pmode=larg
          else
              call error(25)
             ifl(37)=isvmod
              go to 99999
          endif
          ifl(25)=0
          go to 2499
199   if (.not.((svtkn.eq.'DELETE'.and.isvl.eq.6).or.
     1         (svtkn2.eq.'DE'.and.isvl.eq.2))) go to 299
c                                                ********* delete
          if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3) then
              istart=itv
          else
              call error(53)
              go to 99999
          endif
          call parsit
          if (ityp.eq.7) then
              istop=istart
          else if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3) then
              istop=itv
              if (nextyp.ne.11) then
                  call error(4)
                  go to 99999
              endif
          else
              call error(53)
              go to 99999
          endif
          if (istop.lt.istart.or.istart.lt.0) then
              call error(37)
              go to 99999
          endif
 
      	  call dellns(istart, istop)
		  go to 2499
 
 
299   IF (.not.(SVTKN.EQ.'QUIT'.AND.ISVL.EQ.4)) go to 399
C                                                ********* QUIT
          IQUIT=1
200       continue
c200       IF (IFL(69).EQ.1) THEN
c              IFINI(1)=1
c              IFINI(2)=14000
c              I4FINI(2)=NREC+1
c              CLBUFF(ISTAT(1))=FINI
c              CALL CLPUT (I4STAT(2),CLBUFF)
c          ENDIF
          go to 2499
399   IF (.not.((SVTKN.EQ.'START'.AND.ISVL.EQ.5).OR.
     1         (SVTKN3.EQ.'STA'.AND.ISVL.EQ.3))) go to 499
C                                                ********* START
         call vxtest(i)
         if (i.eq.1) then
           call clswin()
           iquit = 1
           call vxjump(iquit)
           goto 2499
         endif
         IQUIT=2
         GO TO 200
499   IF (.not.((SVTKN.EQ.'HELP'.AND.ISVL.EQ.4).OR.
     1         (SVTKN2.EQ.'HE'.AND.ISVL.EQ.2))) go to 599
C                                                *********  HELP
c
c...Added check for NCL-VT mode
c...Paul  -  10/15/91
c...Old version was:
c   IF (IFL(35).EQ.0) THEN
c
c...VX
c
 
        IF (IFL(35).EQ.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 0))
     1          THEN
          SAVEPM=PMODE
          PMODE=0
          call ersw3(15,1)
          CALL HELP (STAT)
          IF (STAT.EQ.1) THEN
              IFL(44)=5
          ENDIF
          PMODE=SAVEPM
        ENDIF
        go to 2499
599   if (.not.(svtkn.eq.'STOP'.and.isvl.eq.4)) go to 699
c                                                ********* stop
c
c        added for *commands in the macro and loops. kathy
c
          if (.not.fromt.and.(ifl(152).eq.1.or.ifl(38).eq.1
     x		   .or.ifl(152).eq.3.or. ifl(45).eq.1))goto 2499
          if (.not. onstop) goto 2499
          if ((fromt.or.strcmd).and.ityp.eq.5
     x            .and.ist.eq.10) goto 2499
 
          pmode=0
          if (ifl(101).eq.1) then
              cout=' '
              call putmsg (cout,80,2,0)
              ifl(101)=0
              if (ifl(25).eq.2) then
                  ifl(37)=2
              else
                  ifl(37)=1
              endif
		  else
c              svll=nline
c          	  nline=nline+1
c
c          command mode. kahty
c
              if (stpflg) then
                 ifl(275) = 2
              else
                 ifl(275) = 0
              endif
          endif
c
c.....Enter command mode if *SET/STPCMD is set
c
          if (ifl(393).eq.1) then
            call putw2(nline)
            svll = svll + 1
            nline = nline + 1
            call ncl_cmd_mode()
          endif
		  go to 2499
 
699   if (.not.((svtkn.eq.'PAUSE'.and.isvl.eq.5).or.
     1         (svtkn2.eq.'PA'.and.isvl.eq.2))) go to 799
c                                                ********* pause
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(35).eq.0) then
c
c...VX
c
        if (.not. onpaus) goto 2499
        IF (IFL(35).EQ.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 0))
     1          THEN
c
c        Added for *commands in the macro and loops. kathy
c
          if (.not.fromt.and.(ifl(152).eq.1.or.ifl(38).eq.1
     x	           .or.ifl(152).eq.3.or. ifl(45).eq.1))then
 
              if (nline.lt.pmode) then
c				  savepm=pmode
c				  pmode=0
c				  call putw2(nline)
                  goto 2499
              endif
           else
              if (nline.lt.pmode) then
                  savepm=pmode
                  pmode=0
                  call putw2(nline)
                  ifl(257) = 1
                  cout = 'Press return to continue or *STOP'
c
c...Added check for NCL-VT mode
c...Paul  -  10/18/91
c...Old version was:
c   call nclpmt (cout, cin)
c
          if (ifl(35) .eq. 2) then
             call putmsg(cout,33,2,3)
             nccin = 0
             kmxc =5
             kcol = 9
             klin = 1
             call gvtlin(cin,nccin,kmxc,klin,kcol)
             klin = 3
             kcol = 1
             call plot(klin,kcol)
             call clreol
             call wflush
          else
             nccout = strlen1(cout)
             call nclpmt (cout, nccout, cin, nccin)
          endif
 
c
c         *pause should not close the scrolling window. kathy
c                  call clswin()
cuni                  call getmsg
                  ifl(257) = 0
                  if (qimage.eq.'*QUIT ') iquit=1
                  if (qimage.eq.'*START') iquit=2
                  fromt=.false.
                  if (ifl(256).eq.1) then
                      cin = '*STOP'
                      nccin = 5
                  endif
                  if (qimage.eq.'*STOP '.or.qimage.eq.'*stop') go to 390
                  cimage='* processing resumed * '
                  nccimg = strlen1(cimage)
                  pmode=savepm
390               continue
              else
                  call error(96)
              endif
          endif
 
          endif
          go to 2499
799   if (.not.(svtkn.eq.'VER' .and. isvl.eq.3)) go to 899
c                                                ********* version
cuni          call lnkinf (0)
          call nclver
		  go to 2499
 
899   if (.not.(svtkn .eq. 'FIND' .and. isvl .eq. 4)) go to 999
c                                                ********* search for pattern
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(35) .eq. 0) then
c
c...VX
c
        IF (IFL(35).EQ.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 0))
     1       then
          if (ifl(35) .eq. 2) call ersw3(15,1)
              savepm = pmode
              pmode = 0
c
c             open window and display source. kathy
              if (ifl(35).eq.0) call opnwin()
              ifl4(3) = find (cin(7:nccin), 0, 1, nline)
              if (savepm .ge. nline) call putw2 (nline)
              if (ifl4(3) .ne. 0) then
                  cin = 'S'
                  nccin = 1
                  inx = 1
                  nextyp = 1
                  ifl(253) = 2
                  findss = .true.
                  call ssrc (stat)
                  findss = .false.
                  ifl(253) = 0
              endif
              pmode = savepm
          endif
          go to 2499
999   if (.not.(svtkn .eq. 'FINDTK' .and. isvl .eq. 6)) go to 1099
c
c                                                ********* search for id-pattern
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(35) .eq. 0) then
c
c...VX
c
        IF (IFL(35).EQ.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 0))
     1       then
              savepm = pmode
              pmode = 0
c
c             open window and display source. kathy
              if (ifl(35).eq.0) call opnwin()
              ifl4(3) = find (cin(9:nccin), 1, 1, nline)
              if (savepm .ge. nline) call putw2 (nline)
              if (ifl4(3) .ne. 0) then
                  cin = 'S'
                  nccin = 1
                  inx = 1
                  nextyp = 1
                  ifl(253) = 3
                  findss = .true.
                  call ssrc (stat)
                  findss = .false.
                  ifl(253) = 0
              endif
              pmode = savepm
          endif
          go to 2499
1099  if (.not.(svtkn .eq. 'SAVEPP')) go to 1199
c                                                ********* save part program
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(35) .eq. 0) then
c
c...VX
c
        IF (IFL(35).EQ.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 0))
     1       then
            if (lstrng) then
              temp = ' '
              j = 0
              call gttext(temp,j)
              ipos = 0
            else
              err  = .false.
              temp = cimage(1:nccimg)
              ipos = index (temp(1:nccimg), '/')
              j = nccimg
            endif
404         ipos = ipos+1
            if (ipos.lt.j.and.temp(ipos:ipos).eq.' ') goto 404
c
c          modefied the partpgm files will not be overwriten unless the user
c          requests to.
c
              nc1 = strlen1(temp(ipos:))
              isavep = savepp (temp(ipos:),nc1,0)
              if (isavep .eq.1) then
                call error(145)
              else if (isavep .ne. -30) then
                if (ifl(37) .ne. 1) call putw2 (nline)
              endif
          endif
          go to 2499
1199  if (.not.(svtkn .eq. 'LOADPP')) go to 1299
c                                                ********* reload part program
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(35) .eq. 0) then
c
c...VX
c
        IF (IFL(35).EQ.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 0))
     1       then
            if (lstrng) then
              temp = ' '
              j = 0
              call gttext(temp,j)
              ipos = 0
            else
              err  = .false.
              temp = cimage(1:nccimg)
              ipos = index (temp(1:nccimg), '/')
              j = nccimg
            endif
406         ipos = ipos+1
            if (ipos.lt.j.and.temp(ipos:ipos).eq.' ') goto 406
            i=1
            nc1 = strlen1(temp(ipos:))
            call loadpp (temp(ipos:), nc1, i, 0,'')
            if (i.ne.0) then
              call error(145)
            else
              if (ifl(37) .ne. 1) call putw2 (nline)
              call domode
            endif
          endif
          go to 2499
c
c...*EDT
c
1299  if (.not.(svtkn .eq. 'EDT')) go to 1399
        IF (IFL(35).EQ.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 0))
     1       then
            call clswin()
            temp='nclwrk1.tmp'
c
c......Open NCL work file
c......Base filename is 'nclwrk1.tmp'
c......Loop until unique name is found
c
            numf = 1
            ipos = index (temp, ' ')
            if (numf .le. 9) then
                write (temp(ipos:),1021) numf
1021            format (i1)
            else
                write (temp(ipos:),1022) numf
1022            format (i2)
            endif
c
1020        call flopen(scrlun, temp, 'NEW','SEQUENTIAL',
     x                  'FORMATTED', MAX_LEN, 'NULL', ioerr)
 
            if (ioerr .ne. 0 .and. numf .lt. 99) then
                numf = numf +1
                if (numf .le.9) then
                    write (temp(ipos:),1021) numf
                else
                    write (temp(ipos:),1022) numf
                endif
                goto 1020
            endif
            close (scrlun,status='delete')
c
c......Copy the part program file
c......into the temporary file
c
            nc1 = strlen1(temp)
            iret = savepp (temp, nc1, 1)
            temp2 = temp
            nc2 = nc1
c
c......Let the user edit the part program
c
            call uledit(temp,nc1,temp2,nc2)
c            call ultname(temp,nc1,temp2,nc2)
c            tt = temp
c
c......Load the part program file back in
c
            call loadpp (temp2, nc2, i, 1,'')
c
c......Delete the temporary file
c
            open (unit = scrlun, file = temp, status = 'old',
     x            err = 430)
            close(scrlun,status='delete')
430         continue
        endif
        go to 2499
1399  if (.not.(svtkn .eq. 'SYSTEM')) go to 1491
c                                                ********* *SYSTEM
c              spawn dcl
c
c...VX
c
          if (ifl(35) .eq. 0
     x       .or. (ifl(35) .eq. 2 .and. ifl(350) .eq. 1) .or.
     x            (ifl(35) .eq. 2 .and. ifl(322) .eq. 1)) then
c...text string
              if ((ityp.ne.9).and.
     1              (.not.(ityp.eq.2.and.ist.eq.24))) then
                   if (nextyp.eq.11) then
                      call spwnit
                      go to 2499
                   endif
                   call error(4)
                   go to 99999
              endif
              comstr = ' '
              if (ityp.eq.9) then
                 comstr = token2
              endif
              if (ityp.eq.2.and.ist.eq.24) then
                 j = 0
                 call gttext(comstr,j)
              endif
              opened = 0
              call parsit
              if (ityp.eq.1.and.ist.eq.71) then
                  opened = 1
              else if (ityp.eq.1.and.ist.eq.72) then
                  opened = 0
              else if (ityp.ne.7) then
                  call error(4)
                  go to 99999
              endif
              if (nextyp.ne.11) then
                  call error(4)
                  go to 99999
              endif
              call ulf_spawn(comstr, opened)
c                      restore screen to previous state
c                      force domode to write line 5
c
c...Added for NCL501+ mode only
c...Paul 03/18/92
c
cc            if(ifl(35) .eq. 2 .and. ifl(350) .eq. 1) then
cc               flag = 1
cc               call pass_to_lplus(flag)
cc
cc               call uw_newline_flush()
cc
cc               call plot(flag,flag)
cc               call clreos
cc               ifl(303) = 2
cc               call statln
cc               call ssrc(stat)
cc               call domode
cc            endif
c
          endif
 
c
c...Added check for NCL-VT mode
c...Paul  -  11/01/91
c...And changed for NCL501+ mode
c...Paul - 03/18/92
c
          if (ifl(35) .eq. 2 .and. ifl(350) .eq. 0) then
              call savescr
              call clrscr
              call wflush
              call trmrst
              write(*,1011)
1011          format(//,' Enter "exit" to return to NCL',/)
              call system("sh")
              call trmatt(CURSES)
              call restscr
          endif
 
          go to 2499
c
c....Added for NCL501+ mode. Paul
c....02/12/92
c
1491    continue
cc1491    if (ifl(35).ne.2.or.ifl(350).eq.0) go to 1499
cc        if (.not.(svtkn .eq. 'MENU'))      go to 1499
cc           call clrscr()
cc
cc           ifl(35) = 0
cc           ifl(350) = 2
cc           ifl(268) = 0
cc
cc           flag = 0
cc           call pass_to_lplus(flag)
cc           call nclu_clean_ln()
cc
cc           ifl(35) = 2
cc           ifl(350) = 1
cc           ifl(268) = 1
cc           call uw_newline_flush()
cc           flag = 1
cc           call pass_to_lplus(flag)
c
c restore text part of screen
c
cc           tmp2 = ifl(253)
cc           ifl(253) = 0
cc           ifl(303) = 2
cc           call statln
cc           call ssrc(stat)
cc           ifl(253) = tmp2
cc           call domode
cc
cc           write(cout,2222)nline
cc2222       format (' ',i5,':  ')
cc           call putmsg(cout,8,1,0)
cc
cc           if (cimage(1:7) .eq. '*return') then
cc              call getsrc (cimage,nline-1)
cc           endif
cc
cc         go to 2499
1499  continue
1599  if (.not.(svtkn.eq.'WWY'. and.isvl.eq.3)) go to 1699
      cout='Copyright (C) 1991-2009 Numerical Control Computer Sciences'
          call putmsg(cout,60,3,0)
          write (cout,1010) sc(119)
1010      format ('NCL Processor Release',f6.3)
          call putmsg(cout,30,4,0)
          go to 2499
1699  if (.not.((svtkn.eq.'INSERT'.and.isvl.eq.6) .or.
     1         (svtkn3.eq.'INS'.and.isvl.eq.3))) go to 1799
c                                                ********* insert
          ifl(37)=2
          ifl(25)=2
c
c         go to command mode. kathy
c
          ifl(275) = 2
          go to 2499
1799  if (.not.((svtkn.eq.'CONSOL'.and.isvl.eq.6).or.
     1         (svtkn2.eq.'CO'.and.isvl.eq.2))) go to 1899
c                                                ********  consol input
          ifl(37)=1
          ifl(25)=0
c
c          command mode. kahty
c
          ifl(275) = 2
          go to 2499
1899  if (.not.((svtkn.eq.'INPUT'.and.isvl.eq.5) .or.
     x         (svtkn3.eq.'INP' .and. isvl.eq.3))) go to 1999
c                                                ********  input (non-process)
          ifl(150)=1
          ifl(37)=4
          go to 2499
1999  if (.not.((svtkn.eq.'SKIP'.and.isvl.eq.4).or.
     1         (svtkn2.eq.'SK'.and.isvl.eq.2))) go to 2099
c                                                ********* skip
          if (ityp.eq.7) then
c                end of statement, skip to next line
              newnln=nline+1
          else if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
c                number, skip that many lines
     1        then
              newnln=nline+tv
              if (newnln.lt.1) newnln=1
          else if (ityp.eq.1.and.ist.eq.714) then
c                "TO", look for number next
              call parsit
 
              if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4)
c                    number found
     1            then
                  if (tv.gt.0) then
c                        good number, skip to that line
c   Date: Sept-09-2016: Yurong
c   Added logic to check the specified jump to line number to make
c   sure it is not beyond the end of file. Output message to notify
c   user and modify jump to end of file instead.
c
                      if (tv.le.ifl4(1)) then
                          newnln=tv
                      else
                          tnum = tv
                          newnln=ifl4(1)
                          write(msg1,4010) tnum, newnln
                          write(msg2,4020) newnln
4010    format('Skip to Line "',i0.1,'" is bigger than the maxinum',
     x                    ' Line "',i0.1,'".')
4020    format('Jump to Line "',i0.1,'" instead.')
                           msg = msg1(1:60) // NEW_LINE("A")
     x                          //msg2(1:60)//endl
                           call ud_printmsg(msg)
                      endif
                  else
c                        bad number, issue error
                      call error(112)
                      go to 99999
                  endif
c
c   added for skip/to,end. kathy
c
			  else if (ist .eq. 499) then
				  newnln=ifl4(1)
 
              else
c                    no number, issue error
                  call error(7)
                  go to 99999
              endif
          else
c                illegal input,issue error
c                  changing ist to 0 forces error.f to report correct error
c                  instead of default error 9
              ist = 0
              call error(361)
              go to 99999
          endif
          if (nextyp.ne.11) then
c                end of input found
              call error(4)
              go to 99999
          endif
c             line number larger than current highest record number in part
c             program file, write null records to pad file
          if (newnln.gt.ifl4(1)) then
            cout=' '
            nc1 = 0
            do 500 ii=ifl4(1)+1,newnln
500            call wrtpp(cout,nc1,ii-1,0,0)
          endif
c               if requested line is less than 1, issue error
          if (newnln .lt. 1) then
              call error(362)
              goto 99999
          endif
c            set nline to requested line and exit
          nline=newnln
c            To correct the line number in case of *skip/to, num.
		  lskip = .true.
c          svll=nline
c
c          command mode. kahty
c
          ifl(275) = 2
          go to 2499
c                                                *********  time
2099  if (.not.((svtkn.eq.'TIME'.and.isvl.eq.4).or.
     1         (svtkn2.eq.'TI'.and.isvl.eq.2))) go to 2199
          call ncltim
		  go to 2499
 
c                                                ********** edit
2199  if (.not.((svtkn.eq.'EDIT' .and. isvl.eq.4) .or.
     x         (svtkn2.eq.'ED'  .and. isvl.eq.2))) go to 2599
c
c...VX
c
      if (ifl(35) .ne. 2 .or. ifl(322) .eq. 0) then
          if (ityp.eq.3 .and. (ist.eq.0 .or. ist.eq.2)) then
              isrc=itv
              if (isrc.lt.1) then
                call error(217)
                goto 99999
              endif
          else if (ityp.ne.7) then
              call error (240)
              goto 99999
          else
              isrc=nline
          endif
c
c...Added check for NCL-VT mode
c...Paul  -  10/22/91
c...Old version was:
c   call edtlin(isrc)
c
          if (ifl(35) .eq. 2) then
             call getsrc(cbuf,knc,(isrc-1),0)
             if(srceof) then
                  call error(241)
                  goto 99999
             endif
             write (cout,1122) isrc
1122         format(i6,': ')
             call putmsg(cout,8,1,1)
             call clreol
             klin = 1
             kcol = 9
             kmxc = ifl(106)
             call gvtlin(cbuf,knc,kmxc,klin,kcol)
             ifl(257) = 0
             ifl4(2) = isrc
             cimage = cbuf(1:knc)
             nccimg = knc
             call putsrc
          else
             call edtlin(isrc)
          endif
c
c...VX
c
      endif
          go to 2499
c
c     New command *window/open, *window/close.  kathy
c
2599  if (.not.(svtkn2 .eq.'WI' .or. svtkn .eq. 'WINDOW')) goto 2299
          if (ityp .eq. 1 .and.ist .eq. 50) then
             if (ifl(35).eq.0) call opnwin()
             goto 2499
          else if (ityp .eq.1.and.ist .eq. 831) then
             if (ifl(35).eq.0) call clswin()
             goto 2499
          else if (token2 .eq. ' ') then
             if (ifl(35).eq.0) call opnwin()
             goto 2499
          else
             call error (380)
             goto 99999
          endif
 
2299  if (.not.(fromt .or. ifl(38).eq.1 .or. ifl(45).eq.1)) go to 2399
c              if it's a non-recognizable control command preceeded
c              by an asterisk (*) that came from the console or a
c              macro or loop is being defined either from the console
c              or the part program file, point to the second character
c              and process the statement again
          inx=2
          stat=4
          if (cin(2:2).ne.'*') ifl4(10) = svll
          ifl(2)=0
          errcom=' '
          fromt=.false.
          err=.false.
          go to 2499
2399  continue
          call error(26)
2499  continue
 
99999 return
      end

      