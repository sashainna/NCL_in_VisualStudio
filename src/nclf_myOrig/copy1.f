C*********************************************************************
C*    NAME         :  copy1.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        copy1.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:44
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine copy1 (copdat,mxtr,copymx,numcpy)
C*       purpose of subroutine: to handle the copy command              
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
       subroutine copy1 (copdat,mxtr,copymx,numcpy,nocop)

      include 'com8a.com'

       integer*2 ktv(4),numcpy,nocop
       integer*4 iclad1(2),iclad2(2)
       equivalence (ktv,tv)
       equivalence (iclad1,claddr(1)),(iclad2,claddr(2))
       character*64 xtoken
       equivalence (xtoken,token2)
       real*8 copymx(16),claddr(2),angle,angler,angles,anglec
       real*8 copdat(2)
       logical mxtr
c
c      added by: kathy
c 
       integer*2  ietype
       integer*4  nclkey
c
       nocop = 1
       mxtr=.true.
       ifl(44)=9
       call parsit
       if ((ityp.eq.2.and.ist.eq.2).or.
     1     (ityp.eq.3.and.itv.gt.0)) then
           if (ityp.eq.2) itv=tv
           write (xtoken,2010) itv
2010       format ('ix',i6.6)
           call vstchk
           if (ist.eq.15) then
               do 50 i=2,16
50                 copymx(i)=0
               call getent (claddr,2,ktv(1),ktv(2),0)
c               if (ipgend.eq.0.and.ielend.eq.0) then
c               call getent (claddr,1,ktv(1),ktv(2),0)
c              if (icladr(2) .eq. 0) then
               call ncl_tstptr (iclad2,iflg)
               if (iflg .eq. 0) then
                   call error(152)
                   go to 88888
               else
                   copdat(1)=claddr(1)
                   copdat(2)=claddr(2)
                   if (nextyp.ne.11) then
c                          check for 'transl'
                       call parsit
                       if (ityp.eq.1.and.ist.eq.649) then
c                              get x, y and z translation values
                           copymx(1)=1
                           copymx(6)=1
                           copymx(11)=1
                           do 100 i=1,3
                               if (nextyp.ne.11) then
                                   call parsit
                                   if ((ityp.eq.2.and.ist.eq.2).or.
     1                                 (ityp.eq.3).or.(ityp.eq.4)) then
                                       copymx(i*4)=tv
                                       if (ityp.eq.3) copymx(i*4)=itv
                                   else
                                       call error(7)
                                       go to 88888
                                   endif
                               else
                                   call error(7)
                                   go to 88888
                               endif
100                        continue
c                          check for 'modify'
                       else if (ityp.eq.1.and.ist.eq.732) then
                           call parsit
                               if (ityp.eq.2.and.ist.eq.10) then
c
c          changed to gtentt by: kathy
c
c                                   call getent (copymx,12,ktv(1),
c     1                                          ktv(2),0)
c
                                    call gtentt (tv,mxtr,nclkey,
     1                                          ietype,copymx)
c
                               else
                                   call error(94)
                                   go to 88888
                               endif
c                          check for 'xy, yz, zx rot'
                       else if (ityp.eq.1.and.ist.gt.732
     1                                   .and.ist.lt.736) then
                           if (nextyp.ne.11) then
                               istsv=ist
                               call parsit
                               if ((ityp.eq.2.and.ist.eq.2).or.
     1                              ityp.eq.3.or.ityp.eq.4) then
                                   angle=tv
                                   if (ityp.eq.3) angle=itv
c                                      1 radian = 57.2957795 degrees
                                   angler=angle/57.2957795
                                   angles=dsin(angler)
                                   anglec=dcos(angler)
                                   if (istsv.eq.733) then
                                       copymx(1)=anglec
                                       copymx(2)=-angles
                                       copymx(5)=angles
                                       copymx(6)=anglec
                                       copymx(11)=1
                                   else if (istsv.eq.734) then
                                       copymx(6)=anglec
                                       copymx(7)=-angles
                                       copymx(10)=angles
                                       copymx(11)=anglec
                                       copymx(1)=1
                                   else if (istsv.eq.735) then
                                       copymx(1)=anglec
                                       copymx(3)=-angles
                                       copymx(9)=angles
                                       copymx(11)=anglec
                                       copymx(6)=1
                                   endif
                               else
                                   call error(7)
                                   go to 88888
                               endif
                           else
                               isvinx=inx
                               call error(7)
                               go to 88888
                           endif
c                              check for 'same'
                       else
                           mxtr=.false.
                           if ((ityp.eq.1.and.ist.ne.730).or.ityp.ne.1)
     1                         then
                               call error(154)
                               go to 88888
                           endif
                       endif
                       if (nextyp.ne.11) then
c                              get number of copies value
                           call parsit
                           if ((ityp.eq.2.and.ist.eq.2).or.
     1                         (ityp.eq.3.and.itv.gt.0)) then
                               numcpy=itv
                               if (ityp.eq.2) numcpy=tv
                           else
                               call error(7)
                               go to 88888
                           endif
                       else
                           numcpy=1
                       endif
                   else
                       numcpy=1
                       mxtr=.false.
                   endif
               endif
           else
               call error (155)
               go to 88888
           endif
       else
           call error(7)
           go to 88888
       endif
       go to 99999
88888  nocop = 0
99999  return
       end
