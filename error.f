C*********************************************************************
C*    NAME         :  error.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       error.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:01
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine error (ierrno)
C*       display error messages 
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
 
      subroutine error (ierrno)

      include 'com8a.com'
      include 'dtcom.com'
      include 'cutter.com'

      character*(MAX_LEN) cin80
      equivalence (cin,cin80)
      character aout(MAX_LEN)
      equivalence (cout,aout)
      character aimage(MAX_LEN)
      equivalence (aimage,cimage)
      integer errno,ier
c
c...svln already declared in com.com
c...WINNT will hit syntax error.
c
c...      integer*4 ii, strlen1, svln
      integer*4 ii, strlen1,nc
      character*1 aw2(MAX_LEN),acerno(8)
      character*8 cerrno
      equivalence (aw2,w2),(acerno,cerrno)
      character*(MAX_PATH) efile
      integer ierr,ifl2
      character*1 atok(64)
      character*64 atoken
      equivalence (atok, atoken)
c
      logical lerout,lercnt
      character*64 errlab
      integer*4 errsub
      equivalence (sc182,errlab)
      equivalence (inx182,errsub)

      if (SIMCOM) return
c...
      if (debug) then
        write(cout,9010)ierrno,ifl(2),pmode
9010    format(' error: ierrno=',i4,',ifl(2)=',i4,',pmode=',i2)
        call putmsg(cout,50,15,0)
      endif


c         Turn off the flag that says an NCL status line is displayed
c         in the error message line.
      ifl(301) = 0

c
c...Check for ON/ERROR flag being set
c
    9 lerout = .true.
      lercnt = .true.
      if (ifl(254) .ne. 0 .and. isvinx .le. 0) then
          lercnt = .false.
          if (ifl(255) .ne. 0) lerout = .false.
      endif

c     Do not issue more than one error per processed statement. kathy
c 
      if ((nline .eq. svln) .and. (svmsg .eq. 0)) then
         goto 99999
      else if (lercnt) then
         svln = nline
         svmsg=0
      endif
              
c          reset *run/step if error occurs unless it is a warning and
c          the warning bypass flag is on
      if ((ierrno.gt.0 .or.
     x   (ierrno.lt.0 .and. .not.nowarn)) .and. lercnt) then
          ifl(101)=0
      endif
      errno=ierrno
      if (lercnt) then
          if (ifl(25).eq.2) then
              ifl(37)=3
          else
               ifl(37)=1
          endif
      endif

c          turn on data tablet error flag so all data tablet input is
c          ignored except for clear line button
      ldterr = .true.

c          set gettrm re-write flag ifl(141) to number of characters to
c          display on line 1
      if (ierrno.gt.0) then
          call nclf_getw2 (1,w2,ncw2,i)
          ifl(141) = ncw2
      endif

c      isvpmd=pmode
      runerr = pmode.ne.0
      if ((pmode.gt.0 .and.
     x    (errno.gt.0 .or. (errno.lt.0.and..not.nowarn))) .and.
     2    lercnt) pmode=0
      if (ifl(2).ne.0) errno=ifl(2)
      if (errno.lt.0) then
          ifl(2)=errno
          errno=-errno
          assign 110 to ii
          err=.false.
c              bump warning count
          ifl(34)=ifl(34)+1
      else
          if (lercnt) then
              assign 100 to ii
              err=.true.
              isvtyp=0
c
c...Added check for NCL-VT mode 
c...Paul  -  10/3/91 
c...Old version was: 
c   if (ifl(35) .eq. 0) nline=svll
c
              if (ifl(35) .eq. 0 .or. ifl(35) .eq. 2) nline = svll

c              bump error count
              ifl(33)=ifl(33)+1
          else
              assign 110 to ii
              if (lerout) ifl(34)=ifl(34)+1
              err=.false.
          endif
      endif
c
c...Internally generated *Command
c...Point to statement which generated
c...this command
c...Bobby  -  9/14/93
c
      if (lercnt) then
          if (ictlin .gt. 0 .and.
     1        (ifl(35) .eq. 0 .or. ifl(35) .eq. 2)) then
              nline   = ictlin - fromct + 1
              ifl(123) = fromct
              ictlin = 0
              ifl4(10) = 0
          endif
c
c...Reset CUTTER/TOOL *commands
c
          ictent = 0
          if (ifl(35) .eq. 1 .and. err) then
              nline=nline+1
              svll=nline
          endif
      endif
c
c...Bypass display of error message
c
      if (.not. lerout) go to 99999

      if (ifl(35) .eq. 1) then
          if (ifl(149).lt.1) ifl(149)=-2
          write (cout,109)svll,ifl(33)
109       format ('@@@@@@@@@@@@@ statement:',i6,' - error total:',i6)
          call putprt(cout,80,1)
      endif
c
c.....Skip changing error number if error came from GET command - Andrew 3/5/13
c
      if (ityp.eq.2.and.ist.eq.1.and.ifl(299).eq.0) errno=9
      k=0
      do 250 i=1,8
          call nclf_getw2 (i,w2,ncw2,j)
          do 200 j=1,ncw2,1
              if (aw2(j).ne.' ') then
                  if (aw2(j).eq.'$') go to 250
                  k=k+1
                  if (k.ge.isvinx-1) go to 260
              endif
200       continue
250   continue
260   continue
      if (ifl(35).eq.1) then
        write (cout,ii) w2(1:ncw2)
110     format ('warning:',a)
100     format ('error:  ',a)
        i      = ncw2   + 8
        call putprt(cout,i,1)
      endif
      if (errno.ne.4) go to 410
          if (ityp.ne.7) isvinx=inx
410   if (isvinx.gt.0) then
          isvinx=isvinx+ifl(121)
          cout='syntax: '

c              figure how long token is then write to error comment area
          if (errcom.eq.' ') then
            atoken = token2
            do 412 i = 64, 1, -1
                if (atok(i) .ne. ' ') goto 413
412         continue
413         atok(i+1)=']'
            if (ierrno.ne. 561) then
            errcom=' ERROR NEAR TOKEN ['
            errcom(20:32)=atoken
            endif
c413         write (errcom, 411) (atok(j), j=1,i)
c411         format (' ERROR NEAR TOKEN [',<i>a1,']')
          endif
      else
          if (lercnt) then
              cout='generation: '
          else
              cout='generation: continuing ...'
          endif
      endif
      if (ifl(35).eq.1) then
        if (isvinx.gt.0.and.ain(isvinx).ne.'@') aout(j+9)='$'
        nc = strlen1(cout)
        i = nc
        call putprt(cout,i,1)
      endif

c...if error file can't open at the beginning
c...do not tried to open again
c...Yurong
      if (errlun.eq.-1) then
         w2 = ' '
         ncw2 = 0
      else
         call flname(4, 'nclerrs{', efile)
         call flopen(errlun, efile, 'OLD', 'DIRECT', 'UNFORMATTED',
     x         80,'NULL',  ierr)
         read(errlun,rec=errno+1)w2(1:80)
         ncw2 = 80
         close (unit=errlun)
      endif
c
c     if ierrno is negative issue a warning. kathy
c
      if (ierrno .lt. 0 .or. .not. lercnt) then
          write (cerrno,295) errno
      else
          write (cerrno,290) errno
      endif
290   format ('ERR',i3.3,': ')
295   format ('WRN',i3.3,': ')
c      if (acerno(4).eq.' ') acerno(4)='0'
c      if (acerno(5).eq.' ') acerno(5)='0'
      w2(ncw2+1:) = ' '
      if (ifl(343) .eq. 1) then
          write (cout,300) cerrno,w2(1:40)
      else
          write (cout,300) cerrno,w2(1:40),errcom
      endif
300   format (a8,a40,a32)
      errcom=' '
      if (ifl(35) .eq. 1) then
        call putprt(cout,80,1)
        cout='@@@@@@@@@@@@@@@@@@@@ '
        call putprt(cout,20,1)
        if (ifl(149).eq.-2) ifl(149)=-1
        goto 99999
      endif
c
c...Added check for NCL-VT mode
c...Paul  -  10/8/91
c...Old version was:
c   call puterm(cout)
c
      if (ifl(35) .eq. 0) then
          if (ifl(343) .eq. 0) then
              call puterm(cout)
          else
              nc = strlen1(cout)
              call puterr(cout,nc)
          endif
      endif
      if (ifl(35) .eq. 2) then
c
c...VX
c
          if (ifl(322) .eq. 1) then
              call vx_error(errno,cout)
          else
c
c         klin = 3
c         kcol = 1
c         call plot(klin,kcol)
c         call dmpbuf(cout,strlen1(cout))
              call putmsg(cout,strlen1(cout),3,1)
              call wflush
          endif
      endif


c              if we were in run mode and not in command mode (ie the command
c              window is not open), turn off runerr so we will go into 
c              command mode to allow user to correct error.
      ifl(275)=1
      if (runerr) then
        ifl(275)=0
c
c...the winopn is for status window
c
c...        call winopn(i)
c...        if (i.eq.0) ifl(275)=2
        ifl(275)=2
      endif
c
c...Reset ON/ERROR conditions
c
99999 if (.not. lercnt) then
          ifl254 = ifl(254)
          ifl(254) = 0
          ifl(255) = 0
c
c...Jumpto user specified label
c
          if (ifl254 .eq. 2) then
              token2 = errlab
              ivxsub = errsub
              call vstchk
              ier = 0
              ifl2 = ifl(2)
              call jmplab (ier)
              ifl(2) = ifl2
              if (ier .ne. 0) go to 9
          endif
      endif
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine uwarn (ierrno)
C*       display warning
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine uwarn (ierrno)

      include 'com8a.com'

      character aout(MAX_LEN)
      equivalence (cout,aout)
      character*(MAX_LEN) cin80
      equivalence (cin,cin80)
      character aimage(MAX_LEN)
      equivalence (aimage,cimage)
      integer errno,ier
c
c...svln already declared in com.com
c...WINNT will hit syntax error.
c
c...      integer*4 ii, strlen1, svln
      integer*4 ii, strlen1
      character*1 aw2(MAX_LEN),acerno(8)
      character*8 cerrno
      equivalence (aw2,w2),(acerno,cerrno)
      character*(MAX_PATH) efile
      integer ierr
      character*1 atok(64)
      character*64 atoken
      equivalence (atok, atoken)
c
      logical lerout,lercnt
      character*64 errlab
      integer*4 errsub
      equivalence (sc182,errlab), (inx182,errsub)


      if (ierrno.ge.0) goto 99999

c         Turn off the flag that says an NCL status line is displayed
c         in the error message line.
      ifl(301) = 0

c
c...Check for ON/ERROR flag being set
c
    9 lerout = .true.
      lercnt = .true.
      if (ifl(254) .ne. 0 .and. isvinx .le. 0) then
          lercnt = .false.
          if (ifl(255) .ne. 0) lerout = .false.
      endif

c     Do not issue more than one error per processed statement. kathy
c 
      if ((nline .eq. svln) .and. (svmsg .eq. 0)) then
         goto 99999
      else if (lercnt) then
         svln = nline
         svmsg=0
      endif

c          reset *run/step if error occurs unless it is a warning and
c          the warning bypass flag is on
      if (.not.nowarn .and. lercnt) then
          ifl(101)=0
      endif
      errno=ierrno
      if (lercnt) then
          if (ifl(25).eq.2) then
              ifl(37)=3
          else
               ifl(37)=1
          endif
      endif

c          turn on data tablet error flag so all data tablet input is
c          ignored except for clear line button
      ldterr = .true.

c          set gettrm re-write flag ifl(141) to number of characters to
c          display on line 1

      runerr = pmode.ne.0
      if (pmode.gt.0 .and. errno.lt.0.and..not.nowarn .and.
     2    lercnt) pmode=0
      if (ifl(2).ne.0) errno=ifl(2)

      ifl(2)=errno
      errno=-errno
      assign 110 to ii
      err=.false.
c           bump warning count
      ifl(34)=ifl(34)+1

c
c...Internally generated *Command
c...Point to statement which generated
c...this command
c...Bobby  -  9/14/93
c
      if (lercnt) then
c
c...Reset CUTTER/TOOL *commands
c
          ictent = 0
      endif
c
c...Bypass display of error message
c
      if (.not. lerout) go to 99999

      if (ityp.eq.2.and.ist.eq.1) errno=9
      k=0
      do 250 i=1,8,1
          call nclf_getw2 (i,w2,ncw2,j)
          do 200 j=1,ncw2,1
              if (aw2(j).ne.' ') then
                  if (aw2(j).eq.'$') go to 250
                  k=k+1
                  if (k.ge.isvinx-1) go to 260
              endif
200       continue
250   continue
260   continue
      if (ifl(35).eq.1) then
        write (cout,ii) w2(1:ncw2)
110     format ('warning:',a)
        i = ncw2 + 8
        call putprt(cout,i,1)
      endif
      if (errno.ne.4) go to 410
          if (ityp.ne.7) isvinx=inx
410   if (isvinx.gt.0) then
          isvinx=isvinx+ifl(121)
          cout='syntax: '

c              figure how long token is then write to error comment area
      else
          if (lercnt) then
              cout='generation: '
          else
              cout='generation: continuing ...'
          endif
      endif
      if (ifl(35).eq.1) then
        if (isvinx.gt.0.and.ain(isvinx).ne.'@') aout(j+9)='$'
        nc = strlen1(cout)
        i = nc
        call putprt(cout,i,1)
      endif
c...if error file can't open at the beginning
c...do not tried to open again
c...Yurong
      if (errlun.eq.-1) then
         w2 = ' '
         ncw2 = 0
      else
         call flname(4, 'nclerrs{', efile)
         call flopen(errlun, efile, 'OLD', 'DIRECT', 'UNFORMATTED',
     x        80,'NULL',  ierr)
         read(errlun,rec=errno+1) w2(1:80)
         ncw2 = 80
         close (unit=errlun)
      endif

      write (cerrno,295) errno
295   format ('WRN',i3.3,': ')
      write (cout,300) cerrno,w2(1:40)
300   format (a8,a40)
c
c...Added check for NCL-VT mode
c...Paul  -  10/8/91
c...Old version was:
c   call puterm(cout)
c
      if (ifl(35) .eq. 0) then
          if (ifl(343) .eq. 0) then
              call puterm(cout)
          else
              nc = strlen1(cout)
              call puterr(cout,nc)
          endif
      endif
      if (ifl(35) .eq. 2) then
c
c...VX
c
          if (ifl(322) .eq. 1) then
              call vx_error(errno,cout)
          else
              call putmsg(cout,strlen1(cout),3,1)
              call wflush
          endif
      endif


c              if we were in run mode and not in command mode (ie the command
c              window is not open), turn off runerr so we will go into 
c              command mode to allow user to correct error.
      ifl(275)=1
      if (runerr) then
        ifl(275)=0
c
c...the winopn is for status window
c
c...        call winopn(i)
c...        if (i.eq.0) ifl(275)=2
        ifl(275)=2
      endif
c
c...Reset ON/ERROR conditions
c
99999 if (.not. lercnt) then
          ifl254 = ifl(254)
          ifl(254) = 0
          ifl(255) = 0
c
c...Jumpto user specified label
c
          if (ifl254 .eq. 2) then
              token2 = errlab
              ivxsub = errsub
              call vstchk
              ier = 0
              call jmplab (ier)
              if (ier .ne. 0) go to 9
          endif
      endif
      return
      end

