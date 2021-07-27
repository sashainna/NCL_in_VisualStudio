C*********************************************************************
C*    NAME         :  rectfy.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       rectfy.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:35
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rectfy
c*        correct patches for any c1 discontinuity and convert to
c*        new storage form (8 pts per pat).
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
      subroutine rectfy

      include 'com4a.com'

      common/blok/xx(16),yy(16),zz(16),co(16),u,v,xpt,ypt,zpt
     1,vx(2),vy(2),vz(2)
      common/wblok/w(400)
      common/pblok/p(400)

      real*8 w,p,asn
      real*8 x(12),y(12),z(12),ch(4)
      real*8 xx,yy,zz,co,u,v,xpt,ypt,zpt,vx,vy,vz
      real*4 aw(800),ap(800)
      equivalence(asn,ksn),(w,aw),(p,ap)
      integer*2 ksn(4)
c
      asn=p(1)
      nps=ksn(1)
      npat=ksn(2)
c          init index to p and w
      ipat=1
      ipx=2+(npat+1)/2
      jpx=2*ipx
      jwx=0
c          move p to w (from wd1 thru patch1-pt8)
      lst=jpx+27
      do 15 i=1,lst
      jwx=jwx+1
15    aw(jwx)=ap(i)
      x(1)=0.
      y(1)=0.
      z(1)=0.
c          index ahead to next patch in p-tbl
17    ipat=ipat+1
      ippx=ipx
      ipx=ipx+20
      jpx=jpx+40
      lst=8
      if (ipat.gt.npat) lst=4
c          load xyz-tbl this patch   (pt1 xyz is 0,0,0 always)
      do 20 i=2,lst
      j=jpx+3*i
      x(i)=ap(j+1)
      y(i)=ap(j+2)
20    z(i)=ap(j+3)
c          get pts(9-12) prior patch into xyz
      dx=p(ipx+1)-p(ippx+1)
      dy=p(ipx+2)-p(ippx+2)
      dz=p(ipx+3)-p(ippx+3)
      do 25 i=9,12
      j=jpx+3*i-40
      x(i)=ap(j+1)-dx
      y(i)=ap(j+2)-dy
25    z(i)=ap(j+3)-dz
c          if npat+1, reflect 9-12 into 5-8
      if(ipat.le.npat)goto 259
      do 255 i=5,8
      j=i+4
      k=i-4
      x(i)=2*x(k)-x(j)
      y(i)=2*y(k)-y(j)
255   z(i)=2*z(k)-z(j)
259   continue
c          begin poly pts alignment
      do 28 i=1,4
      j=i+4
      k=i+8
cuni28    ch(i)=sqrt((x(j)-x(k))**2+(y(j)-y(k))**2+(z(j)-z(k))**2)
      ch(i)=((x(j)-x(k))**2+(y(j)-y(k))**2+(z(j)-z(k))**2)
      ch(i) = sqrt(ch(i))
28    continue
c          do weighted average rho
      ro1=sqrt(x(5)**2+y(5)**2+z(5)**2)/ch(1)
      ro4=sqrt((x(4)-x(8))**2+(y(4)-y(8))**2+(z(4)-z(8))**2)/ch(4)
      wt1=ch(1)/(ch(1)+ch(4))
      wt4=1.-wt1
      avro=wt1*ro1+wt4*ro4
c          correct pts(5-8) in xyz
      do 30 i=1,4
      j=i+4
      k=i+8
      x(j)=x(i)+avro*(x(j)-x(k))
      y(j)=y(i)+avro*(y(j)-y(k))
30    z(j)=z(i)+avro*(z(j)-z(k))
c          move these back into p
      do 40 i=5,8
      j=3*i
      ap(jpx+j+1)=x(i)
      ap(jpx+j+2)=y(i)
40    ap(jpx+j+3)=z(i)
c          move this patch to w  (+ rho of prior)
      lst=jpx+27
      if(ipat.gt.1) ap(jpx)=(1.-avro)/avro
      do 50 i=jpx,lst
      jwx=jwx+1
50    aw(jwx)=ap(i)
      if(ipat.le.npat)goto 17
c          fix nwds, move w to p
95    nwds=2+(npat+1)/2+(npat+1)*14
      ksn(1)=nwds
c          ksn(3) returns sftype to geogn2
c              ksn(3)=0 full sf
c              ksn(3)=1 rldsf
      ksn(3)=0
      p(1)=asn
      do 96 i=2,nwds
96    p(i)=w(i)
99    return
      end
