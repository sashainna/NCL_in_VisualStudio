C*********************************************************************
C*    NAME         :  scrub.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       scrub.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:39
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine scrub
c*           broken into scrub,ascrub,bscrub,pmcalc       8-apr-83
c*
c*               ascrub runs m*n pattern
c*               bscrub handles toler control pattern
c*
c*
c*          scrub/sf1,m,n          ( m-passes, n-pts per pass )
c*          scrub/sf1,m,n,pt1,pt2,pt3,pt4
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
      subroutine scrub

      include 'com4a.com'
      include 'mocom.com'
      include 'drvcom.com'

      common/scrblk/e,qx,qy,qz,dqx,dqy,dqz,sfu,sfv,xsf,ysf,zsf,
     1 vna,vnb,vnc,uold

      real*8 e(14)
      real*4 ad(300),ae(28),qx(4),qy(4),qz(4),dqx(4),dqy(4),dqz(4)
      integer*2 ksc(500),kd(600)
      equivalence (sc,ksc),(d,ad,kd),(e,ae)
      integer*4 nclkey,srfkey,pankey
      integer*2 nwds,ietype
      integer*2 isrf,ifl331
      equivalence (ifl(54),isrf)

c        scrub now requires ta/same, no refsys, ball-end cutter  3-feb-83
c        ta/normal,ps & flat bottom normal,ps added              15-jan-86
c                                  tlaxis/same or normal,ps
      if (ifl(23).gt.1) then
          ifl(2)=226
          go to 998
c                                  refsys/off
      else if (ifl(72).ne.0) then
          ifl(2)=227
          go to 998
c                                  ball-end cutter or flat bottom normal to ps
      else if (abs(tool(6)).gt..002 .and. ifl(23).ne.1) then
          ifl(2)=228
          go to 998
      endif
c
      call gtdesc(sc(11),srfkey,nwds,ietype)
      call sftype (srfkey,isftyp)
      if (isftyp.ne.25.and.isftyp.ne.27) goto 5
c                                    scrub not implemented for quilt sf
      ifl(2)=290
      goto 998

5     continue
      ifl331 = ifl(331)
      ifl(331) = 0
      uold=2.2
      isrf=1
12    continue
c     init scrub region.   full or partial
      if(ksc(38).eq.2) goto 15
      if(ksc(38).ne.1) goto 998
c     scrub type 1.   surf corners set region.
      ad(101)=0.
      ad(102)=0.
      ad(103)=0.
      ad(104)=1.
      ad(105)=1.
      ad(106)=0.
      ad(107)=1.
      ad(108)=1.
      goto 20
c     scrub type 2.  get 4 pts and project on sf1
15    continue
      do 17 i=1,4
      k=4*i+49
c     get pt, load in s(,1),  call surfpn.
      call gtgeom(sc(13+i),d(55),nclkey,nwds,ietype)
      s(8,1)=d(55)
      s(9,1)=d(56)
      s(10,1)=d(57)
c               begin search at sf center pt         7-mar-83
      uret=.5
      vret=.5
      call sfinit (sc(11), isrf, uret, vret)
      call surfpn(uret,vret,0)
      j=2*i+99
      ad(j)=uret
      ad(j+1)=vret
17    continue

20    continue
      uret=.1
      vret=.1
      call sfinit (sc(11), isrf, uret, vret)
      if (isftyp.eq.91) call gtspa1 (srfkey,kd(42),panhed,pankey)
      if(sc(12).lt.1..or.sc(13).lt.1.)goto 30
      call ascrub(srfkey,pankey)
      if(ifl(2).gt.0) goto 998
      autouv = .false.
      goto 999
30    call bscrub
      if(ifl(2).gt.0) goto 998
      autouv = .false.
      goto 999
998   if(ifl(2).lt.1)ifl(2)=5
999   continue
      ifl(331) = ifl331

      return
      end
