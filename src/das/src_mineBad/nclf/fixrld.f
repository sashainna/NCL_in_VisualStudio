C*********************************************************************
C*    NAME         :  fixrld.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       fixrld.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:04
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fixrld
C*       correct rldpats for any c1 discontinuity 
C*       and convert to new storage form (4 point per patch)
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
      subroutine fixrld

      include 'com4a.com'

      common/blok/xx(16),yy(16),zz(16),co(16),u,v,xpt,ypt,zpt
     1,vx(2),vy(2),vz(2)
      common/wblok/w(400)
      common/pblok/p(400)

      real*8 w,p,asn,x(12),y(12),z(12),ch(4)
      real*8 xx,yy,zz,co,u,v,xpt,ypt,zpt,vx,vy,vz
      real*4 aw(800),ap(800)
      equivalence(asn,ksn),(w,aw),(p,ap)
      integer*2 ksn(4)
c     integer*4 dghj, dghk
c
      asn=p(1)
      npat=ksn(2)
c          init index to p and w
      ipat=1
      ipx=2+(npat+1)/2
      jpx=2*ipx
      jwx=0
c          move p to w (from wd1 thru patch1-pt4)
      lst=jpx+15
      do 15 i=1,lst
      jwx=jwx+1
15    aw(jwx)=ap(i)
c          init xyz(1)=0.  (never chgs)
      x(1)=0.
      y(1)=0.
      z(1)=0.
c          index ahead to next patch in p-tbl
17    ipat=ipat+1
      ippx=ipx
      ipx=ipx+11
      jpx=jpx+22
c          load xyz-tbl this patch   (pt1 xyz is 0,0,0 always)
      iend=4
      if (ipat.eq.npat+1) iend=2
      do 20 i=2,iend
      j=jpx+3*i
      x(i)=ap(j+1)
      y(i)=ap(j+2)
20    z(i)=ap(j+3)
c          get pts(5,6) prior patch into xyz
      dx=p(ipx+1)-p(ippx+1)
      dy=p(ipx+2)-p(ippx+2)
      dz=p(ipx+3)-p(ippx+3)
      do 25 i=5,6
      j=jpx+3*i-22
      x(i)=ap(j+1)-dx
      y(i)=ap(j+2)-dy
25    z(i)=ap(j+3)-dz
c          if npat+1, reflect 5,6 into 3,4
      if(ipat.le.npat)goto 259
      do 255 i=3,4
      j=i+2
      k=i-2
      x(i)=2*x(k)-x(j)
      y(i)=2*y(k)-y(j)
255   z(i)=2*z(k)-z(j)
259   continue
c          begin poly pts alignment
      do 28 i=1,2
      j=i+2
      k=i+4
c     dghj = j
c     dghk = k
c     write(conlun,2288) dghj, x(j), y(j), z(j), dghk, x(k), y(k), z(k)
c2288 format(i3, f12.6, 1x, f12.6, 1x, f12.6, 1x, /)
28    ch(i)=sqrt((x(j)-x(k))**2+(y(j)-y(k))**2+(z(j)-z(k))**2)
c          do weighted average rho
      ro1=sqrt(x(3)**2+y(3)**2+z(3)**2)/ch(1)
      ro2=sqrt((x(2)-x(4))**2+(y(2)-y(4))**2+(z(2)-z(4))**2)/ch(2)
      wt1=ch(1)/(ch(1)+ch(2))
      wt2=1.-wt1
      avro=wt1*ro1+wt2*ro2
c          correct pts(3,4) in xyz
      do 30 i=1,2
      j=i+2
      k=i+4
      x(j)=x(i)+avro*(x(j)-x(k))
      y(j)=y(i)+avro*(y(j)-y(k))
30    z(j)=z(i)+avro*(z(j)-z(k))
c          move these back into p
      do 40 i=3,4
      j=3*i
      ap(jpx+j+1)=x(i)
      ap(jpx+j+2)=y(i)
40    ap(jpx+j+3)=z(i)
c          move this patch to w  (+ rho of prior)
      lst=jpx+15
      if(ipat.gt.1) ap(jpx)=(1.-avro)/avro
      do 50 i=jpx,lst
      jwx=jwx+1
50    aw(jwx)=ap(i)
      if(ipat.le.npat)goto 17
c          all done.
ccccccccccccccccccccccccccccccccccccccccc
c              p/o in rldgen
ccccccccccccccccccccccccccccccccccccccccc
c          fix nwds, move w to p
95    nwds=2+(npat+1)/2+(npat+1)*8
      ksn(1)=nwds
c          rldsrf sets iap(3)=1
      ksn(3)=1
      p(1)=asn
      do 96 i=2,nwds
96    p(i)=w(i)

99    return
      end
