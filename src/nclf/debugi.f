C*********************************************************************
C*    NAME         :  debugi.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       debugi.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:51
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine debugi(itsk,itim)
C*    called by ipatch to disply surf data     7-dec-82
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
      subroutine debugi(itsk,itim)

      include 'com4a.com'
      include 'mocom.com'

      integer*4 knc,strlen1
c
      character*2 ss(5)

      data ss(1)/'ps'/,ss(2)/'ds'/,ss(3)/'cs'/,ss(4)/'  '/,ss(5)/'??'/

c  isrf=ifl(54)
c--
      kk=4
      if(itsk.gt.2)kk=5
c--
      j=ifl(54)
      lin=j+16
      if(j.eq.3)lin=22
      if(itsk.gt.1)goto 20
c     display itim and exit.
      write(cout,11)ss(j),itim
      call putmsg(cout,80,lin,0)
11    format(2x,a2,2x,i2,3f10.4,2x,3f10.6,2x,a2)
      goto 99
c
c     display final xyz,abc
20    write(cout,11)ss(j),itim,s(5,j),s(6,j),s(7,j),
     1 s(1,j),s(2,j),s(3,j),ss(kk)
      call swinpt()
      call opnwin()
      call putmsg(cout,80,lin,0)
c
c     pause
      cout = 'hit any key to continue'
      knc = strlen1(cout)
      call nclpmt(cout, knc, cin, nccin)
      call clswin()
cuni     call gettrm(cin,1,1)

99    return
      end
