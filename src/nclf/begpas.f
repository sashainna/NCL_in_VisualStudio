C*********************************************************************
C*    NAME         :  begpas.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       begpas.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       05/01/17 , 13:05:25
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine begpas(jret)
C*          this routine controls the init move logic             *
C*              jret = 0    all ok
C*                   = 1    error.  invalid start loc
C*                   = 2    retry 1-time
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine begpas(jret)

      include 'com4a.com'
      include 'mocom.com'

      common /psldcom/ irejog,ro,idsjog
      integer*4 irejog,idsjog
      real*4 ro
      character*80 enpt, calpt
      character*500 msg
      integer*4 knc, kmxc, kcol, klin, strlen1, ud_yesnof, yesno
      equivalence (itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      real*8 xlast, ylast, zlast;
      equivalence (xlast, sc(171)), (ylast, sc(172))
      equivalence (zlast, sc(173))
      real*8 tolsq, tolco, hld(9)
c      real*4 hld(9)
c
c...Debug variables
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)

      if (DEBUGX .ne. 0) then
          write (dbuf,7001) sc(1),sc(2),sc(3),sc(4),sc(5),sc(6)
 7001     format ('Begpas = ',6f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7104) t(19,ic),t(20,ic),t(21,ic)
 7104     format ('t(ds,ic) = ',3f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7105) t(19,ia),t(20,ia),t(21,ia)
 7105     format ('t(ds,ia) = ',3f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7106) t(25,ic),t(26,ic),t(27,ic)
 7106     format ('t(cs,ic) = ',3f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7107) t(25,ia),t(26,ia),t(27,ia)
 7107     format ('t(cs,ia) = ',3f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
c
          write (dbuf,7108) s(1,2),s(2,2),s(3,2),s(4,2)
 7108     format ('DS Normal = ',4f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7109) s(5,2),s(6,2),s(7,2)
 7109     format ('DS Sfpt = ',3f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7110) s(8,2),s(9,2),s(10,2)
 7110     format ('DS Lookpt = ',3f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
c
          write (dbuf,7111) s(1,3),s(2,3),s(3,3),s(4,3)
 7111     format ('CS Normal = ',4f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7112) s(5,3),s(6,3),s(7,3)
 7112     format ('CS Sfpt = ',3f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
          write (dbuf,7113) s(8,3),s(9,3),s(10,3)
 7113     format ('CS Lookpt = ',3f11.4)
          call ctob (dbuf,dout)
          call nclxostr(dout)
      endif
c
c...Set "Out of Tolerance"
c...Point Distance to 2 * Chordal Tolerance
c...Angular Distance to 1 degree
c
      if (sc(169).lt.8.4999) then
          tolsq = 4.0d-6
          tolco = .99995
      else
          tolsq = sc(91)**2
          tolco = sc(92)
      endif
c
c          bump itfl at entry  and unset ifl(90)
       kfl2=ifl(2)
      itfl=itfl+1
c          actually set ifl(90) = ifl(104) to force log stpt
c          output in ta/,,modify mode          10-19-82
      ifl(90)=ifl(104)
c          check this loc vs. sc(1,2,3)
      call conv4_8(t(1,ia),hld,9)
c      call conv4_4(t(1,ia),hld,9)
      co = 1.0
      if (cntctp) then
        call clcept(hld)
        dsq=(sc(1)-hld(1))**2+(sc(2)-hld(2))**2+(sc(3)-hld(3))**2
        if (ifl(23).gt.0) co = sc(4)*hld(4)+sc(5)*hld(5)+sc(6)*hld(6)
      else
        dsq=(t(1,ic)-t(1,ia))**2+(t(2,ic)-t(2,ia))**2
     1     +(t(3,ic)-t(3,ia))**2
        if (ifl(23).gt.0)
     1    co = t(4,ic)*t(4,ia)+t(5,ic)*t(5,ia)+t(6,ic)*t(6,ia)
      endif
c
c...If distance and angle are ok
c...Go to normal exit
c
      if(dsq.le.tolsq.and.co.ge.tolco) goto 90
c      goto 90
c          if itfl still neg, go re-try
      if(itfl)92,91,91
      !if(itfl)91,91,92
c              all ok exit
90    itfl=0
      jret=0
      goto 99
c              error exit. pass start loc did not resolve
c
c...First try at dsrel shows tool is within
c...tolerance of DS on extenstion at startup
c...So set automatic move to next location
c...QAR FSR 61061 & QAR 98133
c...Bobby - 08/05/11
c
91    if (idsjog .eq. 1) goto 40
      jret=1
      ifl(2)=140
c              if autostart or dntcut, all ok
355   if (autost.or.ifl(42).ne.0) go to 40
c
c...If running under machining library & not auto start, give an error.
c
       if (ifl(330).eq.1) goto 38
c
c...Added check for NCL-VT mode
c...Paul  -  10/18/91
c...Old version was:
c   if (ifl(35).eq.0) go to 356
c
       if (ifl(35).eq.0 .or. ifl(35) .eq. 2) go to 356


c  **********  add Unicad code here to open a window, put message out and
c  **********  accept input character

c          if batch, move it
c          issue warning that there is a mismatch in tool position
       call putmsg('warning: tool automatically moved from entry locatio
     1n to calculated location    ',80,2,0)
       ifl(34)=ifl(34)+1
       go to 357
c
c...Added check for NCL-VT mode
c...Paul  -  10/18/91
c...Old version was:
c356  call swinpt()
c     call motend
c     call opnwin()
c
c...Changed for NCL501+ mode
c...Paul - 03/17/92
c
356   if (ifl(35) .eq. 0 .and. ifl(350) .ne. 2) then
        call swinpt()
cc        call motend
        call opnwin()
      endif
c
c...put this info in Yes/No Box instead of status window per Ken
c...Yurong
c
      write(cout,3520)
3520  format('note:  tool not in correct position')
c...      call putmsg(cout,80,2,3)
c...357   write(cout,34)(sc(i),i=1,6)
c...34    format('  entry loc  ',3f10.4,4x,3f9.6)
c...      call putmsg(cout,80,3,0)
c       ensure number(s) are not too large so as to overflow output format
c...      if (t(1,ia) .gt. 9999.9999) t(1,ia) = 9999.0
c...      if (t(1,ia) .lt.-9999.9999) t(1,ia) = -9999.0
c...      if (t(2,ia) .gt. 9999.9999) t(2,ia) = 9999.0
c...      if (t(2,ia) .lt.-9999.9999) t(2,ia) = -9999.0
c...      if (t(3,ia) .gt. 9999.9999) t(3,ia) = 9999.0
c...      if (t(3,ia) .lt.-9999.9999) t(3,ia) = -9999.0
c...      if (lblade) call modfy (hld, hld)
c...      write(cout,35)(hld(j),j=1,6)
c...35    format('  calcd loc  ',3f10.4,4x,3f9.6)
c...      call putmsg(cout,80,4,0)
cccccccccccccccccccccccccccccccc      
357   write(enpt,34)(sc(i),i=1,6)
34    format('  entry loc  ',3f10.4,4x,3f9.6)
      if (t(1,ia) .gt. 9999.9999) t(1,ia) = 9999.0
      if (t(1,ia) .lt.-9999.9999) t(1,ia) = -9999.0
      if (t(2,ia) .gt. 9999.9999) t(2,ia) = 9999.0
      if (t(2,ia) .lt.-9999.9999) t(2,ia) = -9999.0
      if (t(3,ia) .gt. 9999.9999) t(3,ia) = 9999.0
      if (t(3,ia) .lt.-9999.9999) t(3,ia) = -9999.0
      if (lblade) call modfy (hld, hld)
      write(calpt,35)(hld(j),j=1,6)
35    format('  calcd loc  ',3f10.4,4x,3f9.6)      
ccccccccccccccccccccccccccccccc   temp keep-going route
c          if batch, don't ask
c
c...VX
c
c
c...when preview, still need get Yes/No answer
c....When Batch, need post info to the print file
c...Yurong
c
c...       if (ifl(35).eq.1 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 1) .or.
c...     1     ifl(216) .eq. 1) go to 40
      if (ifl(35).eq.1 .or. (ifl(35) .eq. 2 .and. ifl(322) .eq. 1)) then
          call putmsg(cout,80,2,3)
          call putmsg(enpt,80,3,0)
          call putmsg(calpt,80,4,0)
          go to 40
      endif
c
c...change to use MessageBox Yes/No
c...YUrong
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c...      ifl(257) = 1
c         read response to warning message
c...      cout =
c...     x 'note:  tool not in correct position. Accept mismatch [y or n]?'
c
c...Added check for NCL-VT mode
c...Paul  -  10/17/91
c...Old version was:
c   call nclpmt (cout, cin)
c
c...          if (ifl(35) .eq. 2) then
c...             call putmsg(cout,62,2,3)
c...            nccin = 0
c...             kmxc =1 
c...             kcol = 64
c...             klin = 2
c...             call gvtlin(cin,nccin,kmxc,klin,kcol)
c
c...             do  348 i =2,4,1
c...             klin = i
c...             kcol = 1
c...             call plot(klin,kcol)
c...348          call erslin
c...          else
c...             knc = strlen1(cout)
c...             call nclpmt (cout, knc, cin, nccin)
c...          endif

c          check if in command mode or not
c...      call cmdmod(i)

c          if not in command mode, close window
c
c...Added check for NCL-VT mode
c...Paul  -  10/18/91
c...Old version was:
c   if (i .eq. 1) call clswin()
c
c...Changed for NCL501+ mode
c...Paul - 03/17/92
c
c...      if (i.eq.1.and.ifl(35).eq.0.and.ifl(350).ne.2) call clswin()
c...      ifl(257) = 0

c  ***********  end of area for Unicad code change for window and answer
c  ***********  technique


c...      if (debug) then
c...          write (cout,9010) cin(1:10),ain(1)
c...9010      format ('begpas cin=',a10,' ain(1)=',a1)
c...          call putmsg (cout,80,21,0)
c...      endif

c...       cout=' '
c...       do 36,j=2,4
c...36     call putmsg(cout,80,j,0)
c...
c...      if  (ain(1) .eq. 'y' .or. ain(1) .eq. 'Y') goto 40
c...      if ((ain(1) .ne. 'n' .and. ain(1) .ne. 'N')) goto 355
c     x     .and. ifl(256).ne.1) goto 355
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      msg = enpt//NEW_LINE("A")//calpt//NEW_LINE("A")
     x  //'Tool not in correct position. Accept mismatch?'      
      yesno = ud_yesnof(msg,
     x  "init move Question");
      if (yesno.eq.1) goto 40
38    jret=1
      ifl(2)=142
      goto 99
c          user responded y to mismatch ok. turn on ifl(90)
40    jret=0
      ifl(90)=1
      ifl(2)=kfl2
      if (ifl(330).eq.1 .and. ifl(42).eq.0 .and. kfl2.eq.0 .and.
     x   idsjog .eq. 0) ifl(2) = -142
      goto 99
c              re-calc start loc
92    jret=2

99    return
      end
