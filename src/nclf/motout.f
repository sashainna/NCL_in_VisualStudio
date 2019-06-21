
C*********************************************************************
C*    NAME         :  motout.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       motout.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:18
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine motout
c*         output this loc.   (x,y are in tdat)
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
      subroutine motout

      include 'com4a.com'
      include 'mocom.com'

      common/pokom/tdat,bu,x(20),y(20),a(20),b(20),c(20)
     1,hx(20),hy(20),rgt,amt,finamt,dist,salmin,psk,dnat
     2,npts,inpts,npold,ibux,stepno, first
      real*8 asn,tdat(3),bj(35),abuf(6),bbuf(6)
      real*4 ad(300),bu(900)
      integer*2 ksn(4),kd(600),stepno
      logical first
      equivalence(asn,bsn,ksn),(jb,bj),(d,ad,kd)
      equivalence (ad(291),ox),(ad(292),oy)

c               if this move very small, exit now
c                  use ad(291,292) for old loc
      dx=tdat(1)-ox
403          format(' step no.',5x,'x',11x,'y',11x,'z')
1010      format(2x,i4,3x,3(f10.4,2x))

      dy=tdat(2)-oy
      if(dx**2+dy**2.lt.1.e-6)goto 99
      ox=tdat(1)
      oy=tdat(2)
c
      tdat(3)=(psk-d(3)*tdat(1)-d(4)*tdat(2))/d(5)
c             if cant case, rot xyz to partsys        1-nov-84
      if(kd(280).eq.0) goto 19
      xi=d(71)*tdat(1)+d(74)*tdat(2)+d(77)*tdat(3)
      yi=d(72)*tdat(1)+d(75)*tdat(2)+d(78)*tdat(3)
      zi=d(73)*tdat(1)+d(76)*tdat(2)+d(79)*tdat(3)
      xhld=tdat(1)
      yhld=tdat(2)
      zhld=tdat(3)
      tdat(1)=xi
      tdat(2)=yi
      tdat(3)=zi
19    continue

      ad(293)=tdat(3)
      abuf(1)=tdat(1)
      abuf(2)=tdat(2)
      abuf(3)=tdat(3)
      abuf(4)=sc(4)
      abuf(5)=sc(5)
      abuf(6)=sc(6)
c               if multax off, write tdat to clfile.
      if(ifl(82).ne.0) goto 30
      call putcl(5000,5,1,tdat)
      goto 40
c               multax on
30    do 32 i=1,6
32    bbuf(i)=abuf(i)
      call putcl(5000,5,1,bbuf)
40    continue
      if(motdfl) then
c    
c...Added check for NCL-VT mode     
c...Paul  -  10/3/91     
c...Old version was:     
c     if((ifl(139).gt.24.or.ifl(139).eq.0).and.
c    x       (ifl(35).eq.0.or.(ifl(35).eq.1.and.first))) then
c 
          if((ifl(139) .gt. 24 .or. ifl(139) .eq. 0) .and.
     x       (ifl(35) .eq. 0 .or. ifl(35) .eq. 2 .or.
     *       (ifl(35) .eq. 1 .and. first))) then
             write(cout,403)
             call putmsg (cout,39,15,0)
             ifl(139)=16
c     
c...Added check for NCL-VT mode      
c...Paul  -  10/3/91      
c...Old version was:      
c   if(ifl(35).eq.0) call ersw3(16,1)
c
             if(ifl(35).eq.0 .or. ifl(35) .eq. 2) call ersw3(16,1)
          endif
          if (ifl(154).eq.1.or.first) then
c                            print large
            first=.false.
            stepno=stepno+1
            write(cout,1010)stepno,(abuf(ii),ii=1,3)
            call putmsg(cout,45,ifl(139),0)
            ifl(139)=ifl(139)+1
          endif
      endif
c RAH: added call to set intr flag - effective on SGI ONLY
      call ckintr(ifl(86),ifl(35))
      if(ifl(86).ne.0) then
          ifl(2)=149
          goto 99
      endif

cc      if(ifl(100).eq.1)call plotm(abuf,.false.)
99    continue
c              re-store tdat if tipped case
      if(kd(280).eq.0)goto 999
      tdat(1)=xhld
      tdat(2)=yhld
      tdat(3)=zhld
999   return
      end
