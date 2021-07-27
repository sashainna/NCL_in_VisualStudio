C*********************************************************************
C*    NAME         :  pncalc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       pncalc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:25
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pncalc
C*       description
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
      subroutine pncalc(srfkey,pankey)

      include 'com4a.com'
      include 'mocom.com'
      include 'drvcom.com'

      Integer*4 srfkey,pankey

      common/scrblk/e,qx,qy,qz,dqx,dqy,dqz,sfu,sfv,xsf,ysf,zsf,
     1 vna,vnb,vnc,uold

      real*8 e(14)
      real*4 ad(300),ae(28),qx(4),qy(4),qz(4),dqx(4),dqy(4),dqz(4)
      integer*2 ksc(500),kd(600), junk
      equivalence (sc,ksc),(d,ad,kd),(e,ae)

      call isitwf (srfkey, i)
      if (i.eq.1) then
        call upncal
        goto 999
      endif
c
      istyp=kd(1)
      if (istyp.ne.26) goto 20
c                                mesh - see if right patch is loaded
      mmax=kd(38)
      nmax=kd(37)
      um=mmax
      do 4 m=1,mmax
        u=m
        if (u/um.gt.sfu) goto 6
4     continue
      m=mmax
6     vm=nmax
      do 8 n=1,nmax
        v=n
        if (v/vm.gt.sfv) goto 10
8     continue
      n=nmax
10    ipat=(n-1)*mmax+m
      if (ipat.eq.kd(39)) goto 300
      kd(39)=ipat

cuni      ipg=kd(5)+(ipat-1)*26/35
cuni      iel=kd(6)+(ipat-1)*26-((ipat-1)*26/35*35)+1
cuni      if (iel.lt.36) goto 12
cuni        iel=iel-35
cuni        ipg=ipg+1
cuni12    call getent(d(25),26,ipg,iel,0)

12    call gtmptt(srfkey,ipat,d(25))
      ad(15)=(u-1.)/um
      ad(16)=u/um
      ad(17)=(v-1.)/vm
      ad(18)=v/vm
      goto 246

20    npats=kd(38)
      npans=kd(2)
      fnpans=npans
      ipat=kd(41)
      ipan=kd(42)
c     if sfv not in this panl, activate one
      if(sfv.gt.ad(17)-.002.and.sfv.lt.ad(18)+.002) goto 22
c     get panel for this sfv
      ipan=sfv*fnpans
      ipan=ipan+1
      if(ipan.gt.kd(2)) ipan=kd(2)
c      j=2*ipan+7
c      ipg=kd(j)
c      iel=kd(j+1)
cuni      call getent(d(10),12,ipg,iel,0)
cuni     call gtspan(srfkey,ipan,d(10)) 
      call gtspa1(srfkey,ipan,panhed,pankey) 
      d(10)=panhed(1)
      kd(41)=0
      kd(42)=ipan
      npats=kd(38)
      fipan=ipan
      ad(18)=fipan/fnpans
      ad(17)=ad(18)-1./fnpans
c     force patch get
      ipat=0
22    continue
c     if no activ patch, go search
      if(ipat.eq.0) goto 30
c     allow small o/b before activating a patch
      if(sfu+.001.lt.ad(15).or.sfu-.001.gt.ad(16)) goto 30
      goto 300
c     scan s-list this panl for patch
30    do 32 i=2,npats
      if(apnhed(i+4).ge.sfu)goto 40
32    continue
c     must be last patch
      i=npats+1
40    ipat=i-1
      kd(41)=ipat
c     set u min/max
      ad(15)=apnhed(ipat+4)
      ad(16)=apnhed(ipat+5)
      if(ipat.eq.npats) ad(16)=1.
c     point to activ panel in ranfil
      ipan=kd(42)
c      isfx=7+2*ipan
c      ipg=kd(isfx)
c      iel=kd(isfx+1)
      if(istyp.eq.9)goto 200
c     *********************************   rldsrf   **************
c      iel=iel-6+8*ipat+(npats+1)/2
c     fix per 35 r8's per page
c120   if(iel.lt.36) goto 130
c      iel=iel-35
c      ipg=ipg+1
c      goto 120
c     read 8 wds this patch into d-tbl
130   continue
cuni     call gtspat(srfkey,ipan,ipat,d(25))
      call gtpptt(pankey,ipat,d(25),8)
cuni     write(cout,1510) ipan,ipat,d(25),d(26),d(27)
cuni1510  format(2x,'pncalc ipan,ipat,x,y,z = ',i2,i2,3f10.5)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1511) ad(55),ad(56),ad(57),ad(58),ad(59)
cuni1511  format(2x,'pncalc dels(ad) = :',5f10.5)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1511) ad(60),ad(61),ad(62),ad(63),ad(64)
cuni      call putmsg(cout,80,1,0)
      rho=ad(64)
c     read 8 wds next patch into e
      iel=iel+8
135   if(iel.lt.36)goto 140
      ipg=ipg+1
      iel=iel-35
      goto 135
140   continue
cuni     call gtspat(srfkey,ipan,ipat+1,e)
      call gtpptt(pankey,ipat+1,e,8)
      junk = ipat + 1
cuni     write(cout,1510) ipan,junk,e(1),e(2),e(3)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1512) ae(7),ae(8),ae(9),ae(10),ae(11)
cuni1512  format(2x,'pncalc dels(ae) = :',5f10.5)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1512) ae(12),ae(13),ae(14),ae(15),ae(16)
cuni      call putmsg(cout,80,1,0)
      dx=e(1)-d(25)
      dy=e(2)-d(26)
      dz=e(3)-d(27)
c     p7,p8 from next pa
      ad(70)=dx
      ad(71)=dy
      ad(72)=dz
      ad(73)=dx+ae(7)
      ad(74)=dy+ae(8)
      ad(75)=dz+ae(9)
c     zero 2(2,3) and extrap back for pts 5,6
      e(2)=0.
      e(3)=0.
      do 145 i=64,69
      j=i+6
      k=i-54
      l=k-6
145   ad(i)=ad(j)-rho*(ae(k)-ae(l))
      goto 246
c  *************************************  full srf  **********
c     move pointer to ipat in ranfil
200   iel=iel-12+14*ipat+(npats+1)/2
c     fix per 35 r8's per page
220   if(iel.lt.36) goto 230
      iel=iel-35
      ipg=ipg+1
      goto 220
c     read 14 wds this pat into d
230   continue
cuni     call gtspat(srfkey,ipan,ipat,d(25))
      call gtpptt(pankey,ipat,d(25),14)
cuni     write(cout,1510) ipan,ipat,d(25),d(26),d(27)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1511) ad(55),ad(56),ad(57),ad(58),ad(59)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1511) ad(60),ad(61),ad(62),ad(63),ad(64)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1511) ad(65),ad(66),ad(67),ad(68),ad(69)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1511) ad(70),ad(71),ad(72),ad(73),ad(74)
cuni      call putmsg(cout,80,1,0)
      rho=ad(76)
c     read 14 wds next pat into e
      iel=iel+14
235   if(iel.lt.36) goto 240
      ipg=ipg+1
      iel=iel-35
      goto 235
240   continue
cuni     call gtspat(srfkey,ipan,ipat+1,e)
      call gtpptt(pankey,ipat+1,e,14)
cuni     junk = ipat + 1
cuni     write(cout,1510) ipan,junk,e(1),e(2),e(3)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1512) ae(7),ae(8),ae(9),ae(10),ae(11)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1512) ae(12),ae(13),ae(14),ae(15),ae(16)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1512) ae(17),ae(18),ae(19),ae(20),ae(21)
cuni      call putmsg(cout,80,1,0)
cuni     write(cout,1512) ae(22),ae(23),ae(24),ae(25),ae(26)
cuni      call putmsg(cout,80,1,0)
      dx=e(1)-d(25)
      dy=e(2)-d(26)
      dz=e(3)-d(27)
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
c     zero e(2,3) and extrap back for pts 9-12
      e(2)=0.
      e(3)=0.
      do 245 i=76,87
      j=i+12
      k=i-60
      l=k-12
245   ad(i)=ad(j)-rho*(ae(k)-ae(l))
c     upshift origpt 2 locs and zero pt1
246   d(23)=d(25)
      d(24)=d(26)
      d(25)=d(27)
      ad(52)=0.
      ad(53)=0.
      ad(54)=0.
c     void u memory
      uold=2.2
c     conv sfuv to pat uv and solve pt
300   u=(sfu-ad(15))/(ad(16)-ad(15))
      v=(sfv-ad(17))/(ad(18)-ad(17))
c     if no u chg, go v-coef
      if(abs(u-uold).lt.1.e-5) goto 330
      uold=u
      usq=u**2
      um=1.-u
      umsq=um**2
      u3=3.*u
      uc1=um*umsq
      uc2=u3*umsq
      uc3=3.*usq*um
      uc4=u*usq
c     up1=umsq
      up2=um*(1.-u3)
      up3=u*(2.-u3)
c     up4=usq
      if(kd(1).ne.21) goto 310
c     rldpat reqs 2 qpts only
      do 309 i=1,2
      j=3*i+49
      qx(i)=uc1*ad(j)+uc2*ad(j+6)+uc3*ad(j+12)+uc4*ad(j+18)
      dqx(i)=-umsq*ad(j)+up2*ad(j+6)+up3*ad(j+12)+usq*ad(j+18)
      j=j+1
      qy(i)=uc1*ad(j)+uc2*ad(j+6)+uc3*ad(j+12)+uc4*ad(j+18)
      dqy(i)=-umsq*ad(j)+up2*ad(j+6)+up3*ad(j+12)+usq*ad(j+18)
      j=j+1
      qz(i)=uc1*ad(j)+uc2*ad(j+6)+uc3*ad(j+12)+uc4*ad(j+18)
      dqz(i)=-umsq*ad(j)+up2*ad(j+6)+up3*ad(j+12)+usq*ad(j+18)
309   continue
      goto 330
c     full pat   4 qpts and dq's
310   do 319 i=1,4
      j=3*i+49
      qx(i)=uc1*ad(j)+uc2*ad(j+12)+uc3*ad(j+24)+uc4*ad(j+36)
      dqx(i)=-umsq*ad(j)+up2*ad(j+12)+up3*ad(j+24)+usq*ad(j+36)
      j=j+1
      qy(i)=uc1*ad(j)+uc2*ad(j+12)+uc3*ad(j+24)+uc4*ad(j+36)
      dqy(i)=-umsq*ad(j)+up2*ad(j+12)+up3*ad(j+24)+usq*ad(j+36)
      j=j+1
      qz(i)=uc1*ad(j)+uc2*ad(j+12)+uc3*ad(j+24)+uc4*ad(j+36)
      dqz(i)=-umsq*ad(j)+up2*ad(j+12)+up3*ad(j+24)+usq*ad(j+36)
319   continue
c     now v.  if rld linear interp only.
330   if(kd(1).ne.21)goto 340
      dxdv=qx(2)-qx(1)
      dydv=qy(2)-qy(1)
      dzdv=qz(2)-qz(1)
      xsf=qx(1)+v*dxdv+d(23)
      ysf=qy(1)+v*dydv+d(24)
      zsf=qz(1)+v*dzdv+d(25)
      dxdu=dqx(1)+v*(dqx(2)-dqx(1))
      dydu=dqy(1)+v*(dqy(2)-dqy(1))
      dzdu=dqz(1)+v*(dqz(2)-dqz(1))
      goto 350
c     full patch
340   vm=1.-v
      vsq=v**2
      vmsq=vm**2
      v3=3.*v
      vc1=vm*vmsq
      vc2=v3*vmsq
      vc3=3.*vsq*vm
      vc4=v*vsq
      vp2=vm*(1.-v3)
      vp3=v*(2.-v3)
      xsf=vc1*qx(1)+vc2*qx(2)+vc3*qx(3)+vc4*qx(4)+d(23)
      ysf=vc1*qy(1)+vc2*qy(2)+vc3*qy(3)+vc4*qy(4)+d(24)
      zsf=vc1*qz(1)+vc2*qz(2)+vc3*qz(3)+vc4*qz(4)+d(25)
      dxdu=vc1*dqx(1)+vc2*dqx(2)+vc3*dqx(3)+vc4*dqx(4)
      dydu=vc1*dqy(1)+vc2*dqy(2)+vc3*dqy(3)+vc4*dqy(4)
      dzdu=vc1*dqz(1)+vc2*dqz(2)+vc3*dqz(3)+vc4*dqz(4)
      dxdv=-vmsq*qx(1)+vp2*qx(2)+vp3*qx(3)+vsq*qx(4)
      dydv=-vmsq*qy(1)+vp2*qy(2)+vp3*qy(3)+vsq*qy(4)
      dzdv=-vmsq*qz(1)+vp2*qz(2)+vp3*qz(3)+vsq*qz(4)
c     sfnorm calc.
350   continue
      vna=dydu*dzdv-dzdu*dydv
      vnb=dzdu*dxdv-dxdu*dzdv
      vnc=dxdu*dydv-dydu*dxdv
c                 if offsrf, chg xyz of sfpt            20-jan-87
      if (kd(5).ne.7)goto 999
      sec=sqrt(vna**2+vnb**2+vnc**2)
      dis=0.
      if(sec.ne.0.) dis=ad(4)/sec
      xsf=xsf+dis*vna
      ysf=ysf+dis*vnb
      zsf=zsf+dis*vnc
999   return
      end
