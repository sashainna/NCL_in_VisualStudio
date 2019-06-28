C*********************************************************************
C*    NAME         :  dsplsf.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dsplsf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:59
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dsplsf (nclkey, dbuf)
C*       display surface type geometry on plotter
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey    - key of surface to display
C*       OUTPUT :  
C*          dbuf      - last point displayed (used for labelling net sfs)
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
       subroutine dsplsf (nclkey, dbuf)

c  dtype   type of display
c
c    9      disply/sf1,m,n                 (m*n pts + stln segs full srf)
c   10      disply/sf1                     (plot curved outline )
c   11      disply/sf1,m,n,pt1,pt2,pt3,pt4  ("   "      "    "   partial)
c
c      note:    only case 9 at this first writing    4-1-82

      include 'com4a.com'
      include 'mocom.com'

      parameter (maxpt=50)
      parameter (maxpn=20)
      parameter (mxsfhd=(2+(maxpn+1)/2))
      parameter (mxpnhd=(2+(maxpt+1)/2))

      integer*4 nclkey
      real*8 dbuf(3)

      real*8 srfhed(mxsfhd),panhed(mxpnhd)
      real*4 asfhed(4),apnhed(2)
      integer*2 ksfhed(12),kpnhed(6)
      equivalence (srfhed,asfhed,ksfhed),(panhed,apnhed,kpnhed)

      real*8 dd(50),asn,e(14)
      real*8 sfu,sfv,du,dv
      real*4 rx(4),ry(4),rz(4)
      !real*4 ad(100),ae(28),qx(4),qy(4),qz(4),x(400),y(400),z(400)
      real*4 ad(100),ae(28),qx(4),qy(4),qz(4),x(600),y(600),z(600)
      integer*2 ksn(4),ksc(500),kd(200)
      equivalence (asn,ksn),(sc,ksc),(d(51),dd,ad,kd),(e,ae)
c      character*20 pass


c          must be m*n (type 9) now         4-2-82
cuni      if(ksc(38).ne.9)goto 998
c          nu,nv=2 min
c      if(sc(12).lt.2.)sc(12)=10.
c      if(sc(13).lt.2.)sc(13)=10.
c
c
c            Get the u and v values from unibase.
c
      call rstint
      call gtgeo(nclkey, srfhed)
      nu = ksfhed(9)
	  nv = ksfhed(10)

c      nu=ifl(137)
c      nv=ifl(138)
c
      if(nu.lt.2)nu=2
      if(nv.lt.2)nv=2
8     if(nu*nv.lt.601)goto 10
      if (nu.gt.10)nu=nu-nu/5
      if (nv.gt.10)nv=nv-nv/5
      goto 8
10    du=nu
      dv=nv
      du=1./(du-1.)
      dv=1./(dv-1.)
      npts=nu*nv
      if(npts.lt.601)goto 12
c          error. too many pts
      ifl(2)=141
      goto 998
c          get sf1 into d. ( treat roughly as ps )
12    continue

c
c            Get the u and v values from unibase.
c
c     call gtgeo(nclkey, srfhed)
c      nu = ksfhed(9)
c	  nv = ksfhed(10)

c                              do not set wd2           20-jan-87
c      dd(2)=sc(11)
      ksfhed(1)=9
c          get panel 1 header into dd(10).  actpat=0, actpan=1
c             (if rldsrf, this is entire srf)

c          get data from ranfil or data base file
      if (ifl(190).eq.0) then
          call gtspa1(nclkey, 1, panhed, pankey)
      else
cuni          call dbgent (dd(10),12,kd(9),kd(10),0)
      endif
      kpnhed(5)=0
      kpnhed(6)=1
c          vmin/max this panel
      fnpans=ksfhed(2)
      ad(17)=0.
      ad(18)=1./fnpans
c          if rldsrf, sftype=21     (not 9)
      if(kpnhed(1).eq.1) ksfhed(1)=21
      istyp=ksfhed(1)
      npans=ksfhed(2)
      npats=kpnhed(2)

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
          if(apnhed(i+4).ge.sfu)goto 40
32    continue
c          must be last pat
      i=npats+1
40    ipat=i-1
c          set u-min/max
      ad(15)=apnhed(ipat+4)
      ad(16)=apnhed(ipat+5)
      if(ipat.eq.npats) ad(16)=1.
c          point to activ panel in ranfil
      ipan=kpnhed(6)
      if(istyp.eq.9)goto 200
c*********************************************  rldsrf
120   continue
c          get data from ranfil or data base file
      if (ifl(190).eq.0) then
          call gtppat(pankey, ipat, dd(25))
      else
cuni          call dbgent (dd(25),8,ipg,iel,0)
      endif
      rho=ad(64)
c          read 8 wds next patch into e

c          get data from ranfil or data base file
      if (ifl(190).eq.0) then
          call gtppat(pankey, ipat+1, e)
      else
cuni          call dbgent (e,8,ipg,iel,0)
      endif
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
c          move pointer to ipat in ranfil
200   continue

c          get data from ranfil or data base file
      if (ifl(190).eq.0) then
          call gtppat(pankey, ipat, dd(25))
      else
cuni          call dbgent (dd(25),14,ipg,iel,0)
      endif
      rho=ad(76)
c          read 14 wds next patch into e

c          get data from ranfil or data base file
      if (ifl(190).eq.0) then
          call gtppat(pankey, ipat+1, e)
      else
cuni          call dbgent (e,14,ipg,iel,0)
      endif
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

c-c-c                    if offset srf, chg xyz per dist         20-jan-87
      if(ksfhed(5).ne.7)goto 304
c                      calc u-vec from 4 qpts
      usq=u**2
      d1=-1.+2.*u-usq
      d2=1.-4.*u+3.*usq
      d3=2.*u-3.*usq
      d4=usq
      xu=d1*qx(1)+d2*qx(2)+d3*qx(3)+d4*qx(4)
      yu=d1*qy(1)+d2*qy(2)+d3*qy(3)+d4*qy(4)
      zu=d1*qz(1)+d2*qz(2)+d3*qz(3)+d4*qz(4)

      if(istyp.eq.9) goto 302
c                       v-vec for rldsrf per 2 pts
      do 301 i=1,2
      j=49+3*i
      rx(i)=c1*ad(j)+c2*ad(j+6)+c3*ad(j+12)+c4*ad(j+18)
      j=j+1
      ry(i)=c1*ad(j)+c2*ad(j+6)+c3*ad(j+12)+c4*ad(j+18)
      j=j+1
301   rz(i)=c1*ad(j)+c2*ad(j+6)+c3*ad(j+12)+c4*ad(j+18)
      xv=rx(2)-rx(1)
      yv=ry(2)-ry(1)
      zv=rz(2)-rz(1)
      goto 303
c                       v-vec for full sf.  calc 4 rpts and then vvec
302   do 3022 i=1,4
      j=49+3*i
      rx(i)=c1*ad(j)+c2*ad(j+12)+c3*ad(j+24)+c4*ad(j+36)
      j=j+1
      ry(i)=c1*ad(j)+c2*ad(j+12)+c3*ad(j+24)+c4*ad(j+36)
      j=j+1
3022  rz(i)=c1*ad(j)+c2*ad(j+12)+c3*ad(j+24)+c4*ad(j+36)
      vsq=v**2
      e1=-1.+2.*v-vsq
      e2=1.-4.*v+3.*vsq
      e3=2.*v-3.*vsq
      e4=vsq
      xv=e1*rx(1)+e2*rx(2)+e3*rx(3)+e4*rx(4)
      yv=e1*ry(1)+e2*ry(2)+e3*ry(3)+e4*ry(4)
      zv=e1*rz(1)+e2*rz(2)+e3*rz(3)+e4*rz(4)

c                         calc normal and move dist along same
303   xn=yu*zv-zu*yv
      yn=zu*xv-xu*zv
      zn=xu*yv-yu*xv
      sec=sqrt(xn**2+yn**2+zn**2)
      dis=0.
      if(sec.gt.0.)dis=asfhed(4)/sec
      x(ipx)=x(ipx)+xn*dis
      y(ipx)=y(ipx)+yn*dis
      z(ipx)=z(ipx)+zn*dis
304   continue 
c-c-c                   end of chg                20-jan-87

c          if more u's this run, go do
      if(nux.lt.nu)goto 20

c          write u-run pass number to terminal and check for abort
c      write (pass,305) nvx
c305   format ('plotting u-pass:',i4)
c      call putmsg (pass,20,4,0)
c RAH: added call to set intr flag - effective on SGI ONLY
      call ckintr(ifl(86),ifl(35))
      if (ifl(86).eq.1) then
          ifl(2)=-212
          go to 99999
      endif

c          plot u-run points
      iipx=iipx+1
      dbuf(1)=x(iipx)
      dbuf(2)=y(iipx)
      dbuf(3)=z(iipx)
      call gmova3(dbuf(1), dbuf(2), dbuf(3))
c              go p2 thru pn
      do 319 j=2,nu
          iipx=iipx+1
          dbuf(1)=x(iipx)
          dbuf(2)=y(iipx)
          dbuf(3)=z(iipx)
          call glina3(dbuf(1), dbuf(2), dbuf(3))
319   continue

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
      if(ipan.gt.ksfhed(2)) ipan=ksfhed(2)

c          get data from ranfil or data base file
      if (ifl(190).eq.0) then
          call gtspa1(nclkey, ipan, panhed, pankey)
      else
cuni          call dbgent (dd(10),12,ipg,iel,0)
      endif
      kpnhed(5)=0
      kpnhed(6)=ipan
      npats=kpnhed(2)
      fipan=ipan
      ad(18)=fipan/fnpans
      ad(17)=ad(18)-1./fnpans
      goto 20

c          write v-run pass number to terminal and check for abort
310   continue
      do 330 i=1,nu
c          write (pass,325) i
c325       format ('plotting v-pass:',i4)
c          call putmsg (pass,20,4,0)
c RAH: added call to set intr flag - effective on SGI ONLY
          call ckintr(ifl(86),ifl(35))
          if (ifl(86).eq.1) then
              ifl(2)=-212
              go to 99999
          endif
          ipx=i
          dbuf(1)=x(ipx)
          dbuf(2)=y(ipx)
          dbuf(3)=z(ipx)
          call gmova3(dbuf(1), dbuf(2), dbuf(3))
          do 329 j=2,nv
              ipx=ipx+nu
              dbuf(1)=x(ipx)
              dbuf(2)=y(ipx)
              dbuf(3)=z(ipx)
              call glina3(dbuf(1), dbuf(2), dbuf(3))
329       continue
330   continue
      call gdraw
      call drwlab(dbuf(1), dbuf(2), dbuf(3), nclkey)
      go to 99999

c          temp error exit
998   if(ifl(2).lt.1) ifl(2)=5
99999 return
      end
