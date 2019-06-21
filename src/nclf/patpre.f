C*********************************************************************
C*    NAME         :  patpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       patpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:25
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine patpre(sa,sb,ipat)
c*       build patch(ipat) for this sa,sb pair
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
      subroutine patpre(sa,sb,ipat)

      include 'com8a.com'

      integer*2 maxpn, maxpt, maxwd
      parameter (maxpt = 50)
      parameter (maxpn = 20)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

      common/wblok/w(4*(maxwd+20))
      common/pblok/p(400)
      common/blok/x(16),y(16),z(16),co(16),u,v,xpt,ypt,zpt
     1,vx(2),vy(2),vz(2)
      common/twblok/ktwist(2*maxpt)

      real*8 asn, p, x,y,z, w
      real*4 aw(8*(maxwd+20))
      real*4 ap(800), pm, sa,sb, ta,tb
      integer*2 iap(8),ksn(4),ksc(500)
      integer*2 iwx,jwx,ktwist

      equivalence (asn,ksn),(p,ap,iap),(sc,ksc),(w,aw)

c
c..... The default value of ifl(346) is zero, it means reverse boundary
c..... and slope curves to avoid twisting. The value ifl(346)=1 means
c..... do not reverse.
c
c
c..... cv1, point-vector at pt1
c
      pm=1.0
      ta=sa
      tb=sb
      if (ktwist(1).eq.1 .and. ifl(346).eq.0) then
         ta=1.0-sa
         tb=1.0-sb
         pm=-1.0
      endif

      iwx=0
      jwx=0
      call crvpnt(ta,iwx,0,1)
      x(1)=w(maxwd+12)
      y(1)=w(maxwd+13)
      z(1)=w(maxwd+14)
      vx(1)=pm*aw(maxwd*2+29)
      vy(1)=pm*aw(maxwd*2+30)
      vz(1)=pm*aw(maxwd*2+31)
c
c..... cv1, point-vector at pt13
c
      call crvpnt(tb,iwx,0,1)
      x(13)=w(maxwd+12)
      y(13)=w(maxwd+13)
      z(13)=w(maxwd+14)
      vx(2)=pm*aw(maxwd*2+29)
      vy(2)=pm*aw(maxwd*2+30)
      vz(2)=pm*aw(maxwd*2+31)

      call polfin(1,5,9,13)
c
c..... repeat for cv3, pt4,pt16
c
      pm=1.0
      ta=sa
      tb=sb
      if (ktwist(3).eq.1 .and. ifl(346).eq.0) then
         ta=1.0-sa
         tb=1.0-sb
         pm=-1.0
      endif

      iwx=(maxwd+20)*2
      jwx=iwx*2
      call crvpnt(ta,iwx,0,1)
      x(4)=w(iwx+maxwd+12)
      y(4)=w(iwx+maxwd+13)
      z(4)=w(iwx+maxwd+14)
      vx(1)=pm*aw(jwx+maxwd*2+29)
      vy(1)=pm*aw(jwx+maxwd*2+30)
      vz(1)=pm*aw(jwx+maxwd*2+31)

      call crvpnt(tb,iwx,0,1)
      x(16)=w(iwx+maxwd+12)
      y(16)=w(iwx+maxwd+13)
      z(16)=w(iwx+maxwd+14)
      vx(2)=pm*aw(jwx+maxwd*2+29)
      vy(2)=pm*aw(jwx+maxwd*2+30)
      vz(2)=pm*aw(jwx+maxwd*2+31)

      call polfin(4,8,12,16)
c
c..... likewise cv2 and cv4  (turn-off cv2,cv4 active segs)
c
      pm=1.0
      ta=sa
      tb=sb
      if (ktwist(2).eq.1 .and. ifl(346).eq.0) then
         ta=1.0-sa
         tb=1.0-sb
         pm=-1.0
      endif

      aw((maxwd+20)*4-1)=0.
      aw((maxwd+20)*8-1)=0.
      iwx=maxwd+20
      if(ksc(48).ne.23) goto 43
c
c..... entity is a vvcv 
c
      call vvcvpt(ta,tb,2)
      if(ifl(2).gt.0) goto 99
      goto 49

43    if(ksc(48).ne.22) goto 45
c
c..... entity is a vecv
c
      ivx=ksc(45)
      call vcvpnt(ta,tb,ivx,2)
      goto 50

45    call crvpnt(ta,iwx,0,1)
      x(2)=w(maxwd*2+32)
      y(2)=w(maxwd*2+33)
      z(2)=w(maxwd*2+34)
      vx(1)=pm*aw(maxwd*4+69)
      vy(1)=pm*aw(maxwd*4+70)
      vz(1)=pm*aw(maxwd*4+71)

      call crvpnt(tb,iwx,0,1)
      x(14)=w(maxwd*2+32)
      y(14)=w(maxwd*2+33)
      z(14)=w(maxwd*2+34)
      vx(2)=pm*aw(maxwd*4+69)
      vy(2)=pm*aw(maxwd*4+70)
      vz(2)=pm*aw(maxwd*4+71)
49    call polfin(2,6,10,14)

50    pm=1.0
      ta=sa
      tb=sb
      if (ktwist(4).eq.1 .and. ifl(346).eq.0) then
         ta=1.0-sa
         tb=1.0-sb
         pm=-1.0
      endif

      iwx=(maxwd+20)*3
      jwx=iwx*2
      if(ksc(56).ne.23) goto 53
      call vvcvpt(ta,tb,4)
      if(ifl(2).gt.0) goto 99
      goto 59

53    if(ksc(56).ne.22) goto 55
      ivx=ksc(53)
      call vcvpnt(ta,tb,ivx,4)
      goto 60

55    call crvpnt(ta,iwx,0,1)
      x(3)=w(maxwd*4+72)
      y(3)=w(maxwd*4+73)
      z(3)=w(maxwd*4+74)
      vx(1)=pm*aw(maxwd*8+149)
      vy(1)=pm*aw(maxwd*8+150)
      vz(1)=pm*aw(maxwd*8+151)

      call crvpnt(tb,iwx,0,1)
      x(15)=w(maxwd*4+72)
      y(15)=w(maxwd*4+73)
      z(15)=w(maxwd*4+74)
      vx(2)=pm*aw(maxwd*8+149)
      vy(2)=pm*aw(maxwd*8+150)
      vz(2)=pm*aw(maxwd*8+151)
59    call polfin(3,7,11,15)
c
c..... now finalize into a 16-point patch
c
60    call polbld(1,2,3,4)
      call polbld(5,6,7,8)
      call polbld(9,10,11,12)
      call polbld(13,14,15,16)
c
c..... add this patch to p-storage table
c
      ipx=2+(iap(2)+1)/2+20*(ipat-1)
      jpx=2*ipx+6
      p(ipx+1)=x(1)
      p(ipx+2)=y(1)
      p(ipx+3)=z(1)

      do 70 k=2,12
      ap(jpx+1)=x(k)-x(1)
      ap(jpx+2)=y(k)-y(1)
      ap(jpx+3)=z(k)-z(1)
      jpx=jpx+3
70    continue
c
c..... if this is the last patch, add pts 13-16
c
      if(ipat.lt.iap(2)) goto 99
      ipx=ipx+20
      p(ipx+1)=x(13)
      p(ipx+2)=y(13)
      p(ipx+3)=z(13)
      jpx=2*ipx+6

      do 80 k=14,16
      ap(jpx+1)=x(k)-x(13)
      ap(jpx+2)=y(k)-y(13)
      ap(jpx+3)=z(k)-z(13)
      jpx=jpx+3
80    continue

99    return
      end
