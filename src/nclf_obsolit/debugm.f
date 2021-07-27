C*********************************************************************
C*    NAME         :  debugm.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       debugm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:51
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine debugm (itsk)
C*       display debug info
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
      subroutine debugm (itsk)

c     called by mover to display debug info      7-dec-82

      include 'com4a.com'
      include 'mocom.com'
c
      integer*4 strlen1
c
      equivalence (itfl,ifl(49)),(iptk,ifl(50)),(ia,ifl(51))
      equivalence (ic,ifl(53)),(ib,ifl(52))
      character*1 cdum
      equivalence(cdum,cin)

      call swinpt()
      call opnwin()
      goto(10,20,30,40,50,60,70,80),itsk

c     itsk 1.  if ifl(85) plus, see if ready to resume.
10    if(ifl(85).lt.0)goto 14
      if(iptk.lt.ifl(85))goto 99
      ifl(85)=-1
14    write(cout,15)iptk,itfl
      call putmsg(cout,80,15,0)
15    format(' *** debugm - mover ***    loc(',i3,')     itfl=',i3,10x)
      goto 90

c     itsk 2.   display te,ta
20    write(cout,22)(t(m,ia),m=1,6)
      call putmsg(cout,80,16,0)
22    format(' te,ta  ',3f10.4,2x,3f10.6)
      goto 90

c     itsk 3.  display fwd per ps-ds
30    write(cout,32)(t(m,ia),m=7,9)
      call putmsg(cout,80,19,0)
32    format(' fwd (per ps-ds)',24x,3f10.6)
      goto 90

c     itsk 4.   display nest te,ta
40    write(cout,22)(t(m,ia),m=1,6)
      call putmsg(cout,80,20,0)
      goto 90

c     itsk 5.  end of loc
50    jptk=iptk-1
      write(cout,52)jptk,t(12,ia)
      call putmsg(cout,80,23,0)
52    format(' end of location(',i3,')         csdis=',f8.4)
      goto 90

c     itsk 6.  stepl chg.
60    write(cout,62)t(10,ia)
      call putmsg(cout,80,21,0)
62    format(' stepl chgd to ',f9.5,'       new te,ta  etc.')
      goto 90

c     itsk 7.  stepl accepted.
70    write(cout,72)t(10,ia)
      call putmsg(cout,80,21,0)
72    format(' stepl accepted =',f9.5,'                          ')
      goto 90

c     itsk 8.  fwd is ng
80    write(cout,82)
      call putmsg(cout,80,20,0)
82    format(' error.  computed fwd sense (ds-rgt vs. ps-up) is ng.')
      write(cout,84)t(7,ia),t(8,ia),t(9,ia)
      call putmsg(cout,80,21,0)
84    format('  computed fwd:',3f10.6,'     ')
86    format('      last fwd:',3f10.6,'     ')
88    format('     prior fwd:',3f10.6,'     ')
      write(cout,86)t(7,ic),t(8,ic),t(9,ic)
      call putmsg(cout,80,22,0)
      write(cout,88)t(7,ib),t(8,ib),t(9,ib)
      call putmsg(cout,80,23,0)

c     pause      honor 'q'
90    nccout = strlen1(cout)
      call nclpmt(cout, nccout, cin, nccin)
      call clswin()
cuni 90    call gettrm(cin,4,1)  
      if(cdum.eq.'q')ifl(85)=0

99    return
      end
