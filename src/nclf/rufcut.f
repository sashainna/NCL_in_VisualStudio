
C*********************************************************************
C*    NAME         :  rufcut.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       rufcut.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:38
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rufcut
C*       description
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
      subroutine rufcut
c
      include 'com4a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 e,f,a,b,asn,x,y
      real*4 ad(300)
      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40)
      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,ksn)
c
      equivalence  (idfr,kd(1)),(nds,kd(3)), (idx,kd(5)),  (icx,kd(6)),
     1            (ibx,kd(7)),(iax,kd(8)), (idpp,kd(9)), (icpp,kd(10)),
     2             (ibpp,kd(11)),(iapp,kd(12))
c        data for lthmot in a-tbl
      equivalence (a(2),x),(a(3),y),(ka(1),kcl),(ka(2),kpl)
      integer*4 nclkey
      integer*2 ietype
      logical trflg
c
c                 zero a(1-4)     lthcom data region
      do 20 i=1,4
20    a(i)=0.
c                 init for output if clfile, aptsrc, or tapecount 'on'
c      kcl=0
      kcl=1
c                 init for plot if plotting and 'cut'
c      kpl=0
      if(ifl(100).eq.1.and.ifl(42).eq.0) kpl=1
c                 init for cutter motion.
      xmin=ad(7)
      ymin=ad(8)
      xmax=ad(9)
      ymax=ad(10)
      depth=ad(2*idx)
c                 calc drx,dry if retrct given
      if(ibx.eq.0)goto 30
      k=2*ibx+2
      alf=ad(k-1)/57.29578
      dis=ad(k)
      drx=cos(alf)*dis
      dry=sin(alf)*dis
30    continue
c
c                init x,y tool ctr
      ycut=ymax
      y=ymax
c
c                issue any pp's prior to depth (except f/r)
      if(idx.gt.6)call ppwrt(0)
c
c    *****************************************************  goback
c                                                   (to make depth move)
100   oycut=ycut
      oy=y
      ycut=ycut-depth
c                if error flag set, exit now.
      if (ifl(2).gt.0) goto 999
      if (ycut.lt.ymin) ycut=ymin
c             sch e-tbl for x-value at new ycut  (no chg in y for goback)
      y=ycut
      call lathex(e,x,y,oldx,oldy,iii)
c                 goto pre-depth loc at rapid f/r
      call ppwrt(5)
      y=oy
       call lthmot
c
c   *******************************************************   depth
c                 issue any depth pp's
200   if (idpp.ne.0) call ppwrt(1)
c                 if no depth f/r, issue rapid
      if (idfr.eq.0) call ppwrt(5)
      y=ycut
       call lthmot
c
c    ******************************************************   cutang
c
300   if (icpp.ne.0) call ppwrt(2)
c                    sch sh2 to find x on part shape this ycut
c                      (xmin for now)
      y=ycut
      call lathex(f,x,y,oldx,oldy,ifx)
 
c                    sto data for lthfol
      a(5)=oldx
      a(6)=oldy
      ka(4)=ifx
      call lthmot
c                follow thru to old ycut
      call lthfol (oycut)
c    ******************************************************   retrct
400   if(ibx.eq.0)goto 490
c                issue pp recs, if any.  otherwise f/r rapid.
      if(ibpp.ne.0)call ppwrt(3)
      if(ibpp.eq.0)call ppwrt(5)
      x=x+drx
      y=y+dry
       call lthmot
c
c               check for more roughing passes
c
490   if(ycut.gt.ymin) goto 100

c    ******************************************************   return
500   if(iax.eq.0)goto 900
      if(iapp.ne.0)call ppwrt(4)
      if(iapp.eq.0)call ppwrt(5)
c              set xy for return step
      asn=d(iax+1)
      k=ksn(4)
      if(k.ne.0)goto 510
c              ksn(4)=0   return to entry tool loc
      x=sc(1)
      y=sc(2)
      goto 600
510   if(k.ne.3) goto 520
c              get return pt into a(2) and goto
      trflg = .true.
      call gtentt(asn, trflg, nclkey, ietype, a(2))
      goto 600
c              off,xaxis,yaxis
520   if(k-84) 900, 530, 540
c                       xaxis    x=xin,y=same
530   x=sc(1)
      goto 600
c                       yaxis    x=same,y=yin
540   y=sc(2)
c               make this move
600   call lthmot
c      -----  at normal exit, update sc(1-3) to last tool loc.
900   sc(1)=x
      sc(2)=y
c
999   return
      end
