C*********************************************************************
C*    NAME         :  cirrec.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       cirrec.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:42
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cirrec
C*      called after basic move along circle ds to
C*      write a 3000-2 and 5000-5 record pair to clfile. 
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
      subroutine cirrec

      include 'com4a.com'
      include 'mocom.com'
c
      common/hsccom/hsc
      real*8 hsc(9)

      real*8 fslow
      real*8 a1,a2,b1,b2,c3

      integer*2 kdat
      real*8 tdat(120),rdat(15),hhh(9),hsav(9)
      integer*2 jd(600)
      real*4 ad(300)
      integer*2 itfl, iptk, ntk, ia, ib, ic, isrf
      equivalence (d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence (ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence (rdat,kdat) 
      real*4 tl,xca,xcb,xcc,xc,yc,zc,r,dx,dy,dz,dis,alf,chd,h,hfwd,sec
      real*4 totang,remang,sbe,bet,dtrav,ang1,tol,dalf,anpts,xcyl,ycyl
      integer*2 i, n, iphase, npts, nsub, nitem

      integer*2 primtyp,ietype,nwds
      integer*4 nclkey
      real*8 ci(7),primdat(16)
c
c...Get circle parameters
c
      call gtdesc (sc(145),nclkey,nwds,ietype)
      if (ietype .eq. CIRCLE) then
          do 10 i=1,7,1
              ci(i) = d(52+i)
   10     continue
      else
          call gtprimt(nclkey,0,primtyp,primdat)
          do 15 i=1,7,1
              ci(i) = primdat(i)
   15     continue
      endif
c
c.....stpt is in hsc(1-9); ndpt is in sc(1-9); ds circle is in d(53-59)
c.....iphase=1 for possible slowdn fedrat use   4-feb-86
c
      iphase=1
      alf=0.
c
c.....  14-jan-83   if ifl(90) on, issue 5000-5 record to startpt.
c..... (this means a mis-match was accepted)
c..... move back the .001 from mover             31-jan-86
c
      call conv8_8(hsc(4),hhh(4),6)
      call uvcplvc(hsc(1),hsc(7),hhh,-1.d-3)
      if(ifl(90).ne.0) then
c
c..... aak 26-sep-97: hhh is changed by putcl if TRACUT is on;
c..... need to restore data afterwards 
c
         call conv8_8(hhh,hsav,9)
c
c.....Adjust circle for TLAXIS/,MODIFY
c
         if (ifl(104) .eq. 1) call modfy(hhh,hhh)
         call putcl(5000,5,1,hhh)
         call conv8_8(hsav,hhh,9)
      endif
c
c.....vp 2-FEB-94 FR/AT...OUT 
c
      if (sc(169) .gt. 8.229) then
         call cidist (hhh,sc,ci)
         go to 99
      end if
c+++
c     solve point on circle axis per stpt.
      dx=hsc(1)-ci(1)
      dy=hsc(2)-ci(2)
      dz=hsc(3)-ci(3)
      dis=ci(4)*dx+ci(5)*dy+ci(6)*dz
      xc=ci(1)+dis*ci(4)
      yc=ci(2)+dis*ci(5)
      zc=ci(3)+dis*ci(6)
c     effective r  ctrpt to stpt.  note backoff .001 from mover
      dx=hsc(1)-xc-.001*hsc(7)
      dy=hsc(2)-yc-.001*hsc(8)
      dz=hsc(3)-zc-.001*hsc(9)
      r=sqrt(dx**2+dy**2+dz**2)
      tl=sqrt(dx**2+dy**2+dz**2)
      xca=dx/tl
      xcb=dy/tl
      xcc=dz/tl
c
c     h and chd to endpt
      dx=sc(1)-hsc(1)+.001*hsc(7)
      dy=sc(2)-hsc(2)+.001*hsc(8)
      dz=sc(3)-hsc(3)+.001*hsc(9)
      h=dx*hsc(7)+dy*hsc(8)+dz*hsc(9)
      chd=sqrt(dx**2+dy**2+dz**2)
c     assumed hfwd = 5 degrees arc.
      hfwd=.0875*r
      if(h.lt.0.)goto 20
      if(chd.gt.2.*hfwd)goto 20
c     subtended angle appears small. reduce hfwd to chd/2
      hfwd=chd/2.
20    dx=hsc(1)+hsc(7)*hfwd-xc
      dy=hsc(2)+hsc(8)*hfwd-yc
      dz=hsc(3)+hsc(9)*hfwd-zc
      sec=sqrt(dx**2+dy**2+dz**2)
      if(sec.gt..001)goto 30
c     error.  nrpt too close to ctrpt
      ifl(2)=165
      goto 99
c
c********************   issue 3000-2 rec for circle info.
c
30    if (ifl(144).ne.0)go to 33
      rdat(1)=0.
      kdat=5
c     get circle data from d(53-59)
      do 32 i=2,8
32    rdat(i)=ci(i-1)
c
c.....Adjust circle for TLAXIS/,MODIFY
c
      if (ifl(104) .eq. 1) call modfy(rdat,rdat)
      if (.not. lmintf(1)) call putcl(3000,2,9,rdat)

      go to 35
33    rdat(1)=5.
      rdat(2)=0.
      rdat(3)=0.
      rdat(4)=0.
      rdat(5)=0.
c     get circle data from d(53-59)
      do 34 i=6,12
34    rdat(i)=ci(i-5)

C
C...Calculate CW=-/CCW=+ for sign
c...of k component of circle
c...For VERICUT support
C... Bobby - 30-OCT-91
C
      if (abs(rdat(11)) .gt. .9999999) then
          a1 = hsc(1) - xc
          a2 = hsc(2) - yc
          b1 = a2
          b2 = (a1*(-1.))
          c3 = sqrt(b1**2+b2**2)
          b1 = b1 / c3
          b2 = b2 / c3
          a1 = hsc(7) * b1 + hsc(8) * b2
          if (a1 .gt. 0.) then
              rdat(11) = abs(rdat(11)) * (-1.)
          else
              rdat(11) = abs(rdat(11))
          endif
      endif
C
C... calculate CW=-/CCW=+ from sign of k component of
C... cross-product of 2 vectors in plane of circle
C... Sharon - 11JUL91
C
c     a1 = hsc(1) - xc
c     a2 = hsc(2) - yc
c     b1 = hsc(1) + hsc(7) - xc
c     b2 = hsc(2) + hsc(8) - yc
c     c3 = a1*b2 - a2*b1
c     if (c3 .lt. 0.0) then
c        rdat(11) = -1.0
c     endif
C

c
c.....Adjust circle for TLAXIS/,MODIFY
c
      if (ifl(104) .eq. 1) call modfy(rdat,rdat)
      if (.not. lmintf(1)) call putcl(3000,2,13,rdat)
35    continue
c
c********************  issue 5000-5 rec for motion.
c                        2-pts for westinghouse
c                        many pts for nccs.
      if(ifl(144).ne.0)goto 55
c
      rdat(1)=xc+dx/sec*r
      rdat(2)=yc+dy/sec*r
      rdat(3)=zc+dz/sec*r
      rdat(4)=sc(1)
      rdat(5)=sc(2)
      rdat(6)=sc(3)
c     if multax on, add ijk values.
      if(ifl(82).eq.0)goto 50
      rdat(4)=hsc(4)
      rdat(5)=hsc(5)
      rdat(6)=hsc(6)
      rdat(7)=sc(1)
      rdat(8)=sc(2)
      rdat(9)=sc(3)
      rdat(10)=sc(4)
      rdat(11)=sc(5)
      rdat(12)=sc(6)
50    if (.not. lmintf(1)) call putcl(5000,5,2,rdat)
      goto 99
c                  nccs o/p    (init for many pts)

c                  if slowdn phase 2, totang=remang
55    if(iphase.eq.1)goto 60
      totang=remang
      goto 63

60    if(chd.gt..0005) goto 62
c                   chd very small.  assume 360 case      11-apr-85
      totang=6.2831853
      goto 63
62    sbe=h/chd
      bet=asin(sbe)
      totang=3.1415927-2.*bet
c                if no slowdn or iphase=2, go calc pts
63    if(ifl(213).eq.0.or.iphase.eq.2)goto 69
      dtrav=totang*r
c               if dtrav .le. dslow, go dirctly to phase 2
      if(dtrav.gt.sc(125))goto 67
c                 dtrav too small. issue slowdn f/r and goto phase 2
65    iphase=2
      fslow=sc(126)
c                 chk for 'scale' fedrat
      if(iabs(ifl(213)).eq.2)fslow=sc(126)*sc(123)
      call putcl(2000,1009,2,fslow)
      goto 69
c                 set 1st and 2nd angular ranges
67    remang=sc(125)/r
      ang1=totang-remang
      if(ang1.le.0.)goto 65
      totang=ang1
69    tol=sc(27)
      dalf=totang
      if(r.gt.tol)dalf=acos((r-tol)/r)*2.
      npts=totang/dalf+1
      if (npts.lt.1) npts=1
      if (npts.eq.1.and.dalf.gt.3.) npts=2
      anpts=npts
      dalf=totang/anpts
      if(iphase.eq.1)alf=0.
      ntk=0
      n=0
      nsub=5
      if (lmintf(1)) nsub = 6
c                loop thru for pts
70    n=n+1
      alf=alf+dalf
      if(n.lt.npts)goto 75
c                 last pt = sc(1,2,3)
      if(ifl(213).eq.0)goto 72
      if(iphase.ne.2)goto 75
72    tdat(ntk+1)=sc(1)
      tdat(ntk+2)=sc(2)
      tdat(ntk+3)=sc(3)
      goto 76
75    xcyl=r*cos(alf)
      ycyl=r*sin(alf)
      tdat(ntk+1)=xcyl*xca+hsc(7)*ycyl+xc
      tdat(ntk+2)=xcyl*xcb+hsc(8)*ycyl+yc
      tdat(ntk+3)=xcyl*xcc+hsc(9)*ycyl+zc
76    nts = ntk + 1
      ntk=ntk+3
c                 multax?
      if(ifl(82).eq.0 .and. sc(169) .lt. 8.229)goto 80
      tdat(ntk+1)=sc(4)
      tdat(ntk+2)=sc(5)
      tdat(ntk+3)=sc(6)
c
c.....Adjust circle for TLAXIS/,MODIFY
c
      if(ifl(82).eq.0) ntk=ntk+3
   80 if (ifl(104) .eq. 1) call modfy(tdat(nts),tdat(nts))
      if(ntk.lt.120)goto 85
c                  tdat full.  dump on clfile
82    nitem=ntk/(3*(ifl(82)+1))
      call putcl(5000,nsub,nitem,tdat)
      nsub=6
      ntk=0
85    if(n.lt.npts)goto 70
      if(ntk.gt.0)goto 82

      if(ifl(213).eq.0)goto 99
      if(iphase.eq.2)goto 90
c              issue slow f/r
      fslow=sc(126)
      if(iabs(ifl(213)).eq.2)fslow=sc(126)*sc(123)
      call putcl(2000,1009,2,fslow)
      iphase=2
      goto 30
c              issue primary f/r
90    call putcl(2000,1009,2,sc(123))
c               if 'once', revert to prior
      if(ifl(213).ge.0)goto 99
      ifl(213)=ifl(214)
      sc(125)=sc(127)
      sc(126)=sc(128)
99    continue
      ifl(95)=0
999   return
      end
