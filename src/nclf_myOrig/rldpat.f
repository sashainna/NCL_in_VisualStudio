C*********************************************************************
C*    NAME         :  rldpat.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       rldpat.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:37
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rldpat(sa,sb,ipat,itwist)
c*       Build patch(ipat) for this sa,sb pair.
c*       Copied from patpre (note the pt numbering).
C*       If itwist=0 at the beginning: calculate whether the patch 
C*       is twisted, return itwist=1 if so.
C*    PARAMETERS   
C*       INPUT  : 
C*          sa, sb               - two consecutive (sa=s(i), sb=s(i+1))
C*                                 s-points for boundary curves
C*          ipat                 - patch number
C*          itwist               - a (logical) flag parameter
C*                                 if itwist=1, cv3 is reversed,
C*       OUTPUT :  
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine rldpat(sa,sb,ipat,itwist)

      include 'com8a.com'

      integer*2 maxpt, maxwd
      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))
  
      common/blok/x(16),y(16),z(16),co(16),u,v,xpt,ypt,zpt
     1,vx(2),vy(2),vz(2)
      common/wblok/w(4*(maxwd+20))
      common/pblok/p(400)

      real*8 x,y,z
      real*4 aw(8*(maxwd+20))
      real*4 ap(800), pm, sa,sb
      equivalence (w,aw),(p,ap,iap)
      integer*2 iap(8),itwist, iwx,jwx, k,l

c
c..... cv1, point-vector at pt1
c
      iwx=0
      jwx=0
      call crvpnt(sa,iwx,0,1)
      x(1)=w(maxwd+12)
      y(1)=w(maxwd+13)
      z(1)=w(maxwd+14)
      vx(1)=aw(maxwd*2+29)
      vy(1)=aw(maxwd*2+30)
      vz(1)=aw(maxwd*2+31)
c
c..... cv1, point-vector at pt13
c
      call crvpnt(sb,iwx,0,1)
      x(13)=w(maxwd+12)
      y(13)=w(maxwd+13)
      z(13)=w(maxwd+14)
      vx(2)=aw(maxwd*2+29)
      vy(2)=aw(maxwd*2+30)
      vz(2)=aw(maxwd*2+31)

      call polfin(1,5,9,13)
c
c..... repeat for cv3, pt4,pt16
c
      pm=1.0
c
c..... The default value of ifl(346) is zero, it means reverse boundary
c..... curves to avoid twisting. The value ifl(346)=1 means do not reverse.
c
      if (itwist.eq.1 .and. ifl(346).eq.0) then
         sa=1.0-sa
         sb=1.0-sb
         pm=-1.0
      endif
 
      iwx=(maxwd+20)*2
      jwx=iwx*2
      call crvpnt(sa,iwx,0,1)
      x(4)=w(iwx+maxwd+12)
      y(4)=w(iwx+maxwd+13)
      z(4)=w(iwx+maxwd+14)
      vx(1)=pm*aw(jwx+maxwd*2+29)
      vy(1)=pm*aw(jwx+maxwd*2+30)
      vz(1)=pm*aw(jwx+maxwd*2+31)

      call crvpnt(sb,iwx,0,1)
      x(16)=w(iwx+maxwd+12)
      y(16)=w(iwx+maxwd+13)
      z(16)=w(iwx+maxwd+14)
      vx(2)=pm*aw(jwx+maxwd*2+29)
      vy(2)=pm*aw(jwx+maxwd*2+30)
      vz(2)=pm*aw(jwx+maxwd*2+31)

      call polfin(4,8,12,16)
c
c..... add this patch to p-storage table
c
      ipx=2+(iap(2)+1)/2+11*(ipat-1)
      jpx=2*ipx+6
      p(ipx+1)=x(1)
      p(ipx+2)=y(1)
      p(ipx+3)=z(1)

      do 70 k=2,6
      l=2*k
c
c..... if k odd, l=l-1
c
      if(k/2*2.ne.k) l=l-1
      ap(jpx+1)=x(l)-x(1)
      ap(jpx+2)=y(l)-y(1)
      ap(jpx+3)=z(l)-z(1)
      jpx=jpx+3
70    continue
c
c..... if this is the last patch, add pts 7,8
c
      if(ipat.lt.iap(2)) goto 99
      ipx=ipx+11
      p(ipx+1)=x(13)
      p(ipx+2)=y(13)
      p(ipx+3)=z(13)
      jpx=2*ipx+6
      k=16
      ap(jpx+1)=x(k)-x(13)
      ap(jpx+2)=y(k)-y(13)
      ap(jpx+3)=z(k)-z(13)

99    return
      end
