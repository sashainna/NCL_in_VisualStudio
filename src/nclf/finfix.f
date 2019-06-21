
C*********************************************************************
C*    NAME         :  finfix.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       finfix.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:03
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine finfix
C*        called by finpre to fix the shape inplace in f-tbl for
C*        lth finish operation.  (the lathe input list is in d-tbl)
C*
C*        only lines, circles and pp's are recognized.
C*        modifiers have already been acted on.
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
      subroutine finfix
c
      include 'com8a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)
      common/lthdum/dum(80)

      real*8 e,f,a,b,dum,asn,ssv
      real*4 ad(300)
      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40),kf(4000)
      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,ksn),(f,kf)
c
c      sdel=d(4)-d(5)
c      if(abs(sdel).lt..001) goto 10
c                error. stock(m,n) not allowed in fincut
c      ifl(2)=251
c      goto 99
      xdel = d(4)
      ydel = d(5)
      if (xdel .gt. ydel) then
         rxy  = ydel
      else
         rxy  = xdel
      end if
      xdel = xdel - rxy
      ydel = ydel - rxy
      ssv  = sc(28)
10    rtool=sc(28)/2. + rxy
c
c...Fake cutter diameter to get correct solution from aCIbxx
c...and axxbCI calls, restore at 99. vp 11/09/94.
c
      sc(28) = rtool * 2.0
      numf=kf(3)
c      offset=d(4)+rtool
c
c             init the read-fix process.  put lim1 in b-tbl
      iexit=0
      irx=10
      ilx=11
c            b(1,2,3,4,5,6) from f(8,9,10,2,3,4)
      do 22 i=1,6
      j=i+7
      if(i.gt.3)j=i-2
22    b(i)=f(j)
c
c ****************  basic read-fix loop start ***************
c            move b to a
30    do 32 i=1,10
32    a(i)=b(i)
c            find next entity, load in b
33    irx=irx+1
      if(irx.lt.numf)goto 331
      if(iexit.eq.1)goto 80
c            put lim2 in b and do final pt (build ln ident in a(1))
      iexit=1
      b(4)=f(5)
      b(5)=f(6)
      b(6)=f(7)
      goto 43
331   asn=f(irx)
      if(ksn(3).ne.2.or.ksn(4).ne.5) goto 34
c            line.  add to b
      b(1)=f(irx)
      b(2)=f(irx+1)
      b(3)=f(irx+2)
      goto 40
34    if(ksn(3).ne.6.or.ksn(4).ne.7) goto 38
c            circle. add to b, fix r per stock
      j=irx-1
      do 36 i=1,7
36    b(i)=f(j+i)
c      bsig=kb(2)
c      b(6)=b(6)+bsig*d(4)
      goto 60
c
38    if(ksn(4).ne.2000) goto 33
c                pp command. add to b/u list
      npp=ksn(3)+1
      do 39 i=1,npp
      f(ilx)=f(irx)
      ilx=ilx+1
39    irx=irx+1
      irx=irx-1
      goto 33
c               fix b-ln per rtool
40    vx=b(2)-a(2)
      vy=b(3)-a(3)
      sec=sqrt(vx**2+vy**2)
      if(sec.gt.0.) goto 42
c               error.  line has no length.
41    ifl(2)=31
      goto 99
42    b(4)=vy/sec
      b(5)=-vx/sec
c      b(6)=b(4)*b(2)+b(5)*b(3) + offset
      b(6)=b(4)*b(2)+b(5)*b(3) + rtool 
c               a-ent ln or ci
43    if(ka(4).eq.7) goto 50
c               a is ln.  inters 2 lns
      den=a(4)*b(5)-a(5)*b(4)
      if(den.ne.0.)goto 44
c               error.  lines are parlel.
      ifl(2)=167
      goto 99
44    xi=(b(5)*a(6)-a(5)*b(6))/den
      yi=(a(4)*b(6)-b(4)*a(6))/den
c               add modified a-ln to b/u list
      f(ilx)=a(1)
      f(ilx+1)=xi + xdel
      f(ilx+2)=yi + ydel
c               hold endpt in case next ent is circle (4 plcs.)
       dum(79)=xi
       dum(80)=yi
      ilx=ilx+3
      irx=irx+2
      goto 30
c               inters a-ci and b-line
50    call acibln
      if(ifl(2).gt.0)goto 99
c               add a-ci to b/u list
      f(ilx)=a(1)
      f(ilx+1)=a(8) + xdel 
      f(ilx+2)=a(9) + ydel
      f(ilx+3)=a(4) + xdel
      f(ilx+4)=a(5) + ydel
      asig=ka(2)
      f(ilx+5)=a(6)+asig*rtool
      call angcal
      f(ilx+6)=a(10)
       dum(79)=a(8)
       dum(80)=a(9)
      ilx=ilx+7
      irx=irx+2
      goto 30
c               b is circle.   if concave case and rtool too big, skip it.
c
60    if(b(6).le.0.)goto 33
c               bra on a-ln or a-ci
      if(ka(4).eq.7)goto 65
c               intersect a-ln b-ci
      call alnbci
      if(ifl(2).gt.0)goto 99
c               add a-ln to b/u list
      f(ilx)=a(1)
      f(ilx+1)=a(7) + xdel 
      f(ilx+2)=a(8) + ydel
       dum(79)=a(7)
       dum(80)=a(8)
      ilx=ilx+3
      irx=irx+6
      goto 30
c               a-ci b-ci
65    call acibci
      if(ifl(2).gt.0)goto 99
c               add a-ci to b/u
      f(ilx)=a(1)
      f(ilx+1)=a(8) + xdel
      f(ilx+2)=a(9) + ydel
      f(ilx+3)=a(4) + xdel
      f(ilx+4)=a(5) + ydel
      asig=ka(2)
      f(ilx+5)=a(6)+asig*rtool
      call angcal
      f(ilx+6)=a(10)
       dum(79)=a(8)
       dum(80)=a(9)
      ilx=ilx+7
      irx=irx+6
      goto 30
c
c                fin-up machine area and misc.
80    kf(3)=ilx-1

99    sc(28) = ssv
      return
      end
