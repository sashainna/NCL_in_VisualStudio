C*********************************************************************
C*    NAME         :  crvfit.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       crvfit.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:44
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crvfit (inv,nmp)
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
      subroutine crvfit (inv,nmp)

      include 'com4a.com'

      parameter (maxpt = 50)
      parameter (maxptx = 1000)

      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,hx,hy,hz,ha,hb,hc

      real*8 hx(maxptx),hy(maxptx),hz(maxptx),hj(maxptx+1)
      real*4 ha(maxptx),hb(maxptx),hc(maxptx)

      real*8 x(maxptx),y(maxptx),z(maxptx),asn,dd(6)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx)
      real*4 dy(maxptx),dz(maxptx),ch(maxptx)
      integer*2 inv(maxptx),ksn(4),ixv,kent,ibeg
      integer*2 ietype,isub,kase18
      equivalence(asn,ksn), (isub,ksn(4))
      integer*4 nclkey
      logical trflg
      real*8 dsq, dtolsq
c
c...init
c...initialize kase18          14-feb-85
c
      kase18 = 0
      trflg  = .true.
      asn    = sc(10)
      nent   = ksn(3)
      irec   = ifl(4)
      kx     = 0
      lastx  = 0
      kent   = 0
      iptk   = 0
      ivf    = 0
      iv20   = 0
c
c...zero the inv-tbl             8-nov-84
c
      do 6 i=1,maxpt
6        inv(i)=0

c
c...fittol = toler/2   (if very small, use 3/4 toler)
c
      ftol=sc(27)/2.
      if (ftol.le..0005) ftol=ftol*1.5
      dtolsq = 4.*sc(27)*sc(27)
      ibeg   = 0
      if (ifl(212).eq.0) then
        if (ranpts) ibeg = nent
        call ncl_get_asn (ibeg,1,hj(1),maxpt+1)
      endif
      iha   = 0
      ihb   = maxpt
      goto 20
c
c...upshift any left-over ptvecs  + inv's
c
10    i=0
      do 15 j=jx,maxpt
         i=i+1
         x(i)=x(j)
         y(i)=y(j)
         z(i)=z(j)
         a(i)=a(j)
         b(i)=b(j)
         c(i)=c(j)
15    inv(i)=inv(j)
c
c...turn-on inv(1) and v20chk=0
c
      inv(1) = 1
      iv20   = 0
      iptk   = i
      ivf    = 1
c
c...get entities via hj tbl until stopped by nent or maxpt kount
c...( hj tbl refreshes after maxpt)
c
20    kent = kent+1
         iha = iha+1
         ixv = 0
         if (iha .le. maxpt) goto 25
         if (ifl(212).eq.0) call ncl_get_asn (ibeg,kent,hj(1),maxpt+1)
         iha = 1
c
c...get this entity
c
25       asn=hj(iha)
         if (ranpts) then
           if (ifl(212).gt.0) then
c
c..... ifl(212) > 0 means we are from cvio
c
             isub = 3
c
c..... if ifl(212) > 1, we do not use vectors; otherwise the first
c..... and last points come with vectors
c
             if (ifl(212).eq.1 .and. 
     *           (kent.eq.1 .or. kent.eq.nent)) isub = 21
             call getent_list (kent,isub,dd(1))
           else
             call getent_list (ksn(2),ksn(4),dd(1))
           endif
         else
             call gtentt(asn, trflg, nclkey, ietype, dd(1))
         endif
         if (isub .eq. 4) goto 30
c
c...point or PV.
c
         if (iptk.eq.0) goto 28
         dsq=(x(iptk)-dd(1))**2+(y(iptk)-dd(2))**2+(z(iptk)-dd(3))**2
         if (dsq.gt.dtolsq) goto 28
         if (kent.eq.nent) goto 26
         asn=hj(iha+1)
         if (isub .eq. 4) goto 26
      goto 20
26       continue
         if (inv(iptk).eq.1) goto 28
         if (iptk.eq.1) goto 28
         iptk=iptk-1
28       continue
         iptk=iptk+1
         x(iptk)=dd(1)
         y(iptk)=dd(2)
         z(iptk)=dd(3)
         inv(iptk)=0
         if (isub .eq. 3) goto 35
c
c...vector
c
30       inv(iptk)=1
         ivf=1
         if (isub .eq. 21) ixv = 3
         a(iptk)=dd(ixv+1)
         b(iptk)=dd(ixv+2)
         c(iptk)=dd(ixv+3)

35       if (kent.ge.nent.or.iv20.eq.1) goto 40
         if (iptk.lt.maxpt) goto 20
c
c...if next entity a vec, go get it
c
         if (ifl(212) .gt. 0) then
           isub = 3
           if (ifl(212).eq.1 .and. (kent+1).eq.nent) isub = 21
         else
           asn=hj(iha+1)
         endif
         if (isub .eq. 3) goto 40
         iv20=1
      goto 20

40    continue
      if (iptk.eq.2.and.inv(1).eq.0.and.inv(2).eq.0) then
         a(1)=x(2)-x(1)
         b(1)=y(2)-y(1)
         c(1)=z(2)-z(1)
         inv(1)=1
      endif
c
c...fair slopes   (mode 1)
c
      iret=1
      call farslp(inv,iptk,ivf,iret)
      if (iret.eq.0) goto 45
42    ifl(2)=iret+46
      goto 999
45    continue
c
c...improve pts  2 thru last via deltas. unless inv on.
c...( last=18 or npts-1 )
c
      lst=maxpt-2
      if (iptk.lt.maxpt.or.kent.eq.nent) lst=iptk-1
      do 50 j=2,lst
         if(inv(j).ne.0) goto 50
         i=j-1
         k=j+1
         call segchk(xc,yc,zc,i,k,1)
         if(ifl(2).gt.0) goto 999
         dx(j)=xc-x(j)
         dy(j)=yc-y(j)
         dz(j)=zc-z(j)
50    continue
c
c...now fix indicated points
c
      do 55 i=2,lst
      if (inv(i).ne.0) goto 55
      hmov=sqrt( dx(i)**2+dy(i)**2+dz(i)**2 )
      if (hmov.lt.1.e-6) goto 55
      ro=ftol/hmov
      if(ro.gt.1.)ro=1.
      x(i)=x(i)+dx(i)*ro
      y(i)=y(i)+dy(i)*ro
      z(i)=z(i)+dz(i)*ro
55    continue
c
c...fair slps again    mode=2
c...display 'slpfair 2'
c
      if (geodsp) call putmsg (' slope fair 2',13,15,0)

      iret=2
      call farslp(inv,iptk,ivf,iret)
      if (iret.ne.0) goto 42
c
c...slpfair off
c...segfit this xyztbl for savpts
c
      if (geodsp) call putmsg (' ',1,15,0)
      jx=1
59    lx=jx
c
c...if no hpt(1), go sto it now
c
      if (kx.eq.0) goto 70

60    lx=lx+1
      if (lx-jx.lt.2)lx=lx+1
      kase18=0
      if (kent.ge.nent) goto 65
      if (lx.lt.maxpt-1) goto 65
      kase18=1
      lx=maxpt-2
      if (jx.ne.1) goto 10
      goto 70

65    if (lx.le.iptk) goto 67
      lx=iptk
      goto 70
67    call segchk(xc,yc,zc,jx,lx,2)
c
c...display segchk result
c
      if (geodsp) then
          write(cout,672)x(lx),y(lx),z(lx),xc,yc,zc
672       format(' try pt  ',3f10.5,4x,3f9.5)
          call putmsg(cout,70,16,0)
      endif

      if (ifl(2).gt.0) goto 999
      if (abs(xc)+abs(yc)+abs(zc).lt.ftol) goto 68
      lx=lx-1
      goto 70
68    if (inv(lx).eq.0) goto 60
c
c...add pt to hold tbl
c
70    kx=kx+1
      if (kx.le.maxpt) goto 71
      ifl(2)=156
      goto 999
71    hx(kx)=x(lx)
      hy(kx)=y(lx)
      hz(kx)=z(lx)
      ha(kx)=a(lx)
      hb(kx)=b(lx)
      hc(kx)=c(lx)
c
c...display hold pt
c
      if (geodsp) then
          write(cout,72)kx,hx(kx),hy(kx),hz(kx)
72        format(' hold pt ',i3,2x,3f10.5)
          call putmsg(cout,44,15,0)
      endif

      jx=lx
      if (lx.lt.iptk.and.kase18.eq.0) goto 60
      if (kent.lt.nent) goto 10
c
c...all done.  move hpts to xyz and calc dx,dy,dz,ch
c
      do 80 i=1,kx
         x(i)=hx(i)
         y(i)=hy(i)
         z(i)=hz(i)
         a(i)=ha(i)
         b(i)=hb(i)
         c(i)=hc(i)
         if (i.eq.kx) goto 80
         dx(i)=hx(i+1)-hx(i)
         dy(i)=hy(i+1)-hy(i)
         dz(i)=hz(i+1)-hz(i)
         ch(i)=sqrt(dx(i)**2+dy(i)**2+dz(i)**2)
80    continue
      nmp=kx

999   call ncl_free_uv
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : getent_list (indx,pts,itype)
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

      subroutine getent_list (indx,itype,pts)

      include 'com8a.com'

      integer*2 indx,itype,j
      real*8 pts(6)

      j = indx
      if (ifl(212).eq.1 .and. indx.ge.2) j=j+1
      call ncl_get_ent (j,pts)
      if (ifl(212).eq.1.and.itype.eq.21) call ncl_get_ent(j+1,pts(4))
c
c...translate thru refsys if required
c
      if (ifl(72) .eq. 1 .and. itype .ne. 0) then
         if (itype .eq. 4) then
           call transf(pts,sc(68),3,4)
         else
           call transf(pts,sc(68),3,3)
         endif
         if (ifl(212).eq.1 .and. itype.eq.21)
     *     call transf(pts(4),sc(68),3,4)
      endif

      return
      end
