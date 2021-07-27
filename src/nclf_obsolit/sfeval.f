C*********************************************************************
C*    NAME         :  sfeval.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sfeval.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sfeval (nclkey,max,npts,buf,iret)
C*      Evaluate a surface into polylines for drawing. 
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey   - Unicad key of surface to evaluate.
C*          max      - max number of points to return.
C*       OUTPUT :  
C*          npts     - Number of points returned.
C*          buf      - Polyline points.
C*          iret     - Has value 0 for first call, set to 1 for subsequent 
C*                     calls, set to -1 for last call.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine sfeval (nclkey, max, npts, buf, iret) 

      include 'com4a.com'
      include 'mocom.com'

      integer*4 nclkey
      integer*2 max,npts,iret
      real*8 buf(450)

c      real*8 dd(50),asn,dbuf(3),e(14)
      real*8 dd(50),asn,e(14)
      real*8 sfu,sfv,du,dv
      real*4 ad(100),ae(28),qx(4),qy(4),qz(4),x(400),y(400),z(400)
	  save x,y,z

      integer*2 ksn(4),ksc(500),kd(200)
c      integer*2 ietype
      integer*2 kw(8)
      equivalence (w,kw)


      equivalence (iu,kd(5)),(nu,kd(6)),(iv,kd(7)),(nv,kd(8))
      equivalence (asn,ksn),(sc,ksc),(d(51),dd,ad,kd),(e,ae)

      if (iret.eq.1) goto 320

c
      call gtgeo(nclkey, dd(1))

c      nu=ifl(137)
c      nv=ifl(138)
c                      get the U and V from unibase
c
	   nu = kd(9)
	   nv = kd(10)
      if(nu.lt.2)nu=2
      if(nv.lt.2)nv=2
      if(nu.gt.max)nu=max
      if(nv.gt.max)nv=max
8     if(nu*nv.lt.401)goto 10
      if (nu.gt.10)nu=nu-nu/5
      if (nv.gt.10)nv=nv-nv/5
      goto 8
10    du=nu
      dv=nv
      du=1./(du-1.)
      dv=1./(dv-1.)
c          get sf1 into d. ( treat roughly as ps )

12    continue

c     .... mesh surface

      if (kd(1) .eq. 26) then
         iret=-1 
         goto 99999
      endif

c     .... quilt surface
      if (kd(1) .eq. 25) then
         iret=-1  
         goto 99999
      endif

c     .... regular surface
c      dd(2)=sc(11)
      kd(1)=9
c          get panel 1 header into dd(10).  actpat=0, actpan=1
c             (if rldsrf, this is entire srf)

      call gtspa1(nclkey, 1, dd(10), pankey)
      kd(41)=0
      kd(42)=1
c          vmin/max this panel
      fnpans=kd(2)
      ad(17)=0.
      ad(18)=1./fnpans
c          if rldsrf, sftype=21     (not 9)
      if(kd(37).eq.1) kd(1)=21
      istyp=kd(1)
      npans=kd(2)
      npats=kd(38)

c          init the ptcal loop
      sfu=-du
      sfv=0.
      nux=0
      nvx=1
      ipat=0
      ipx=0
      iipx=0
20    nux=nux+1
      sfu=sfu+du
c          if no active patch, go search.
      if(ipat.eq.0) goto 30
c          allow some small o/b before activating a patch
      if(sfu+.001.lt.ad(15).or.sfu-.001.gt.ad(16))goto 30
      goto 300
c          scan s-list this panel for indicated patch
30    do 32 i=2,npats
          kkx=i+22
          if(ad(kkx).ge.sfu)goto 40
32    continue
c          must be last pat
      i=npats+1
40    ipat=i-1
c          set u-min/max
      ad(15)=ad(ipat+22)
      ad(16)=ad(ipat+23)
      if(ipat.eq.npats) ad(16)=1.
c          point to activ panel in ranfil
      ipan=kd(42)
      if(istyp.eq.9)goto 200
c*********************************************  rldsrf
120   continue
c
c     changed to fix the metric surface on drawing. kathy
c
c     call gtpptt(pankey, ipat, dd(25),8)
c
      call gtppat(pankey, ipat, dd(25))
      rho=ad(64)
c          read 8 wds next patch into e
c
c     changed to fix the metric surface on drawing. kathy
c
      call gtppat(pankey, ipat+1, e)
c      call gtpptt(pankey, ipat+1, e,8)
      dx=e(1)-dd(25)
      dy=e(2)-dd(26)
      dz=e(3)-dd(27)
c          p7,p8 from next pat
      ad(70)=dx
      ad(71)=dy
      ad(72)=dz
      ad(73)=dx+ae(7)
      ad(74)=dy+ae(8)
      ad(75)=dz+ae(9)
c          zer0 e(2,3) and extrap back for pts 5,6
      e(2)=0.
      e(3)=0.
      do 145 i=64,69
      j=i+6
      k=i-54
      l=k-6
145   ad(i)=ad(j)-rho*(ae(k)-ae(l))
      goto 246
c*******************************************  full srf
200   continue
c
c     changed to fix the metric surface on drawing. kathy
c
      call gtppat(pankey, ipat, dd(25))
c      call gtpptt(pankey, ipat, dd(25),8)
      rho=ad(76)
c          read 14 wds next patch into e
c
c     changed to fix the metric surface on drawing. kathy
c
      call gtppat(pankey, ipat+1, e)
c      call gtpptt(pankey, ipat+1, e,8)
      dx=e(1)-dd(25)
      dy=e(2)-dd(26)
      dz=e(3)-dd(27)
      ad(88)=dx
      ad(89)=dy
      ad(90)=dz
      ad(91)=dx+ae(7)
      ad(92)=dy+ae(8)
      ad(93)=dz+ae(9)
      ad(94)=dx+ae(10)
      ad(95)=dy+ae(11)
      ad(96)=dz+ae(12)
      ad(97)=dx+ae(13)
      ad(98)=dy+ae(14)
      ad(99)=dz+ae(15)
c          zer0 e(2,3) and extrap back for pts 9-12
      e(2)=0.
      e(3)=0.
      do 245 i=76,87
          j=i+12
          k=i-60
          l=k-12
245       ad(i)=ad(j)-rho*(ae(k)-ae(l))
c          upshift origpt 2 locs and zero pt1
246   dd(23)=dd(25)
      dd(24)=dd(26)
      dd(25)=dd(27)
      ad(52)=0.
      ad(53)=0.
      ad(54)=0.
c              do 4 qpts this pat, this v   (rld or srf)
      if(istyp.ne.21)goto 280
c          rld qpts
      do 270 i=1,4
          j=46+6*i
          qx(i)=ad(j)+sfv*(ad(j+3)-ad(j))
          qy(i)=ad(j+1)+sfv*(ad(j+4)-ad(j+1))
270       qz(i)=ad(j+2)+sfv*(ad(j+5)-ad(j+2))
      goto 300
c          full srf qpts
280   v=(sfv-ad(17))/(ad(18)-ad(17))
      c1=(1.-v)**3
      c2=3.*v*(1.-v)**2
      c3=3.*v**2*(1.-v)
      c4=v**3
      do 290 i=1,4
          j=40+12*i
          qx(i)=c1*ad(j)+c2*ad(j+3)+c3*ad(j+6)+c4*ad(j+9)
          j=j+1
          qy(i)=c1*ad(j)+c2*ad(j+3)+c3*ad(j+6)+c4*ad(j+9)
          j=j+1
290       qz(i)=c1*ad(j)+c2*ad(j+3)+c3*ad(j+6)+c4*ad(j+9)

c          conv sfu to pat u and solve pt
300   u=(sfu-ad(15))/(ad(16)-ad(15))
      c1=(1.-u)**3
      c2=3.*u*(1.-u)**2
      c3=3.*u**2*(1.-u)
      c4=u**3
      ipx=ipx+1
      x(ipx)=c1*qx(1)+c2*qx(2)+c3*qx(3)+c4*qx(4)+dd(23)
      y(ipx)=c1*qy(1)+c2*qy(2)+c3*qy(3)+c4*qy(4)+dd(24)
      z(ipx)=c1*qz(1)+c2*qz(2)+c3*qz(3)+c4*qz(4)+dd(25)

c          if more u's this run, go do
      if(nux.lt.nu)goto 20

c          any more v's ?
      nvx=nvx+1
      if(nvx.gt.nv) goto 310
c          prep for next u-run. force pat sch
      sfv=sfv+dv
      sfu=-du
      nux=0
      ipat=0
c          switch panels if indicated
      if(sfv.gt.ad(17)-.002.and.sfv.lt.ad(18)+.002)goto 20
c          get panel for sfv
      ipan=sfv*fnpans
      ipan=ipan+1
      if(ipan.gt.kd(2)) ipan=kd(2)

      call gtspa1(nclkey, ipan, dd(10), pankey)
      kd(41)=0
      kd(42)=ipan
      npats=kd(38)
      fipan=ipan
      ad(18)=fipan/fnpans
      ad(17)=ad(18)-1./fnpans
      goto 20

310   continue

      iret=1
      iv=0
      iu=0

320   if (iv.ge.nv) goto 350

      iv=iv+1
      ipx=(iv-1)*nu
c...
c      write(cout,9010)iu,nu,iv,nv,ipx
c9010  format('sfeval: iu,nu,iv,nv,ipx'5i4)
c      call putmsg(cout,80,1,0)
c...
      do 330 i=1,nu
      ipx=ipx+1
      buf(i*3-2)=x(ipx)
      buf(i*3-1)=y(ipx)
      buf(i*3)=z(ipx)
c...
c      write(cout,9020)buf(i*3-2),buf(i*3-1),buf(i*3)
c9020  format('sfeval: buf = '3f16.4)
c      call putmsg(cout,80,1,0)
c...
330   continue

      npts=nu
      goto 99999

350   if (iu.ge.nu) goto 400

      iu=iu+1
      ipx=iu

      do 360 i=1,nv
      buf(i*3-2)=x(ipx)
      buf(i*3-1)=y(ipx)
      buf(i*3)=z(ipx)
      ipx=ipx+nu
360   continue

      npts=nv
      goto 99999

400   iret=-1

99999 return
      end
