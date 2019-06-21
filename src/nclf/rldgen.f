C*********************************************************************
C*    NAME         :  rldgen.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       rldgen.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:37
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rldgen
c*       builds a set of patches for the 2 curves in the w-table  
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
      subroutine rldgen

      include 'com4a.com'

      integer*2 maxpt, maxwd
      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

      common/wblok/w(4*(maxwd+20))
      common/pblok/p(400)

      common/twblok/ktwist(2*maxpt)
      integer*2 ktwist

      common/blok/x(16),y(16),z(16),co(16),u,v,xpt,ypt,zpt
     1,vx(2),vy(2),vz(2)
      real*8 x,y,z

      real*8 w,p,asn
      real*4 aw(8*(maxwd+20))
      real*4 s(maxpt),ap(800),sa,sb
      equivalence (asn,ksn),(p,ap),(sc,lsc),(aw,w)
      integer*2 i,iwx,itwist,itim,itwis(2)
      integer*2 ksn(4),lsc(100)

c
c..... The default value of ifl(346) is zero, it means reverse boundary
c..... curves to avoid twisting. The value ifl(346)=1 means do not reverse.
c
c
c..... if 2 curves of equal npts, s-gen may be unnecessary. 7-28-82
c
      if (lsc(44).ne.8.or.lsc(48).ne.8.or.
     1  aw((maxwd+20)*2).ne.aw((maxwd+20)*6)) goto 9

CMAR89          IF EITHER IS CONIC CV, GO FULL ROUTE  17-MAR-89
C                CONIC/CVS HAVE 5 PTS AND RHOVALS=1.
      IF(AW((maxwd+20)*2).NE.5.)GOTO 6
      IF(ABS(AW(18)-1.).LT..0002.OR.
     1  ABS(AW((maxwd+20)*4+18)-1.).LT..0002)GOTO 9
6     CONTINUE
CMAR89 END OF CHG

C.C.C           IF 1-SEG CVS FROM IGES EQUATIONS, GO FULL ROUTE  11-JUN-87
C                < 2PTS AND DU/DS(0)=1. , DU/DS(1)=0. >
      IF(AW((maxwd+20)*2).NE.2.)GOTO 7
      IF (AW(12).EQ.1..AND.AW(13).EQ.0.)GOTO 9
      IF(AW((maxwd+20)*4+12).EQ.1.
     1  .AND.AW((maxwd+20)*4+13).EQ.0.)GOTO 9
7     CONTINUE
C.C.C                                              END OF CHG    11-JUN-87
c
c..... compare s-values
c
      ns=aw((maxwd+20)*2)
      do 8 i=1,ns
      if (abs(aw(i)-aw(i+(maxwd+20)*4)).gt..005) goto 9
8     s(i)=aw(i)
c
c..... s-values match. use same for patch def.
c..... set ifl(30) for polfin
c
      ifl(30)=1
      do 81 i=1,ns
81    aw(i+(maxwd+20)*4)=aw(i)
      goto 181
c
c..... start at s=0, and gen s-list along cv1, cv3
c
9     i=1
      s(1)=0.
10    sa=s(i)
c
c..... do cv1 and cv3 sb values.
c
      call sbsolv(sa,sb,1,ktwist(1))
      sb1=sb
      call sbsolv(sa,sb,3,ktwist(3))
      sb3=sb
      i=i+1
      if(i .gt. maxpt) goto 998
      s(i)=sb3
      if(sb1 .lt. sb3) s(i)=sb1

      if (geodsp) then
          write(cout,18)s(i)
18        format(14x,'rldgen: s=',f6.4)
          call putmsg(cout,30,15,0)
      endif

      if(s(i) .lt. .9999) goto 10
      ns=i
181   npat=ns-1
c
c..... init the p-sto array (wd1)
c
      ksn(2)=npat
      ksn(1)=2+(npat+1)/2+(npat+1)*8
      p(1)=asn
c
c..... avoid very small last patch.
c
      if(npat.lt.2 .or. ifl(30).eq.1) goto 19
      rho=(1.-s(npat))/(1.-s(npat-1))
      if(rho .gt. .2) goto 19
      s(npat)=1.-(1.-s(npat-1))*.2
19    continue
c
c..... add s-list
c
      do 20 i=1,npat
20    ap(i+4)=s(i)
c
c..... build patches per this s-list
c
      if (geodsp) call putmsg
     x                 ('  patches are now being solved',30,15,0)
c
c..... Check patches for twisting. First check the endpoints.
c
      itwist = 0
      if (ifl(346) .eq. 1) goto 50
      itwis(1) = 0   
      itwis(2) = 0      
   
      itim = 1

30    iwx=0
      call crvpnt(0.,iwx,0,0)
      x(1)=w(maxwd+12)
      y(1)=w(maxwd+13)
      z(1)=w(maxwd+14)
      call crvpnt(1.0,iwx,0,0)
      x(13)=w(maxwd+12)
      y(13)=w(maxwd+13)
      z(13)=w(maxwd+14)
      iwx=(maxwd+20)*2
      if (itim .eq. 1) then
        call crvpnt(0.,iwx,0,0)
      else
        call crvpnt(1.,iwx,0,0)
      endif
      x(4)=w(iwx+maxwd+12)
      y(4)=w(iwx+maxwd+13)
      z(4)=w(iwx+maxwd+14)
      if (itim .eq. 1) then
        call crvpnt(1.,iwx,0,0)
      else
        call crvpnt(0.,iwx,0,0)
      endif
      x(16)=w(iwx+maxwd+12)
      y(16)=w(iwx+maxwd+13)
      z(16)=w(iwx+maxwd+14)
      call chkpat(itwist)
      itwis(itim) = itwis(itim) + itwist

      do 40 i=1,npat
         ipat=i
         sa=s(i)
         sb=s(i+1)
c
c..... cv1, load pt1, pt13
c
         iwx=0
         call crvpnt(sa,iwx,0,0)
         x(1)=w(maxwd+12)
         y(1)=w(maxwd+13)
         z(1)=w(maxwd+14)
         call crvpnt(sb,iwx,0,0)
         x(13)=w(maxwd+12)
         y(13)=w(maxwd+13)
         z(13)=w(maxwd+14)
c
c..... repeat for cv2, pt4,pt16
c
         iwx=(maxwd+20)*2
         if (itim .eq. 1) then
           call crvpnt(sa,iwx,0,0)
         else
           call crvpnt(1.-sa,iwx,0,0)
         endif
         x(4)=w(iwx+maxwd+12)
         y(4)=w(iwx+maxwd+13)
         z(4)=w(iwx+maxwd+14)
         if (itim .eq. 1) then
           call crvpnt(sb,iwx,0,0)
         else
           call crvpnt(1.-sb,iwx,0,0)
         endif
         x(16)=w(iwx+maxwd+12)
         y(16)=w(iwx+maxwd+13)
         z(16)=w(iwx+maxwd+14)
c
c..... check whether the patch is twisted
c
         call chkpat(itwist)
         itwis(itim) = itwis(itim) + itwist
40    continue
      if (itim.eq.1) then
        if (itwis(1).eq.0 .or. itwis(1).gt.npat) goto 50
        itim = 2
        goto 30
      endif
      if (itwis(1).gt.itwis(2)) then
        itwist = 1
      else
        itwist = 0
      endif

50    do i=1,npat

        if (geodsp) then
          write (cout,24) i
24        format(8x,'patch no.',i3)
          call putmsg (cout,20,16,0)
        endif

        ipat=i
        sa=s(i)
        sb=s(i+1)
        call rldpat(sa,sb,ipat,itwist)
      enddo

c*************************     6-29-81
      call fixrld
c************************
c          blank-out window three
      cout=' '
      if (geodsp) call ersw3(15,1)
      goto 999
998   ifl(2) = 156
999   ifl(30)=0

      return
      end
