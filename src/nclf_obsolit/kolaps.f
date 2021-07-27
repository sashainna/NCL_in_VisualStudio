
C*********************************************************************
C*    NAME         :  kolaps.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       kolaps.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:13
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine kolaps(icase)
C*            collapse this point set by dist
C*
C*            output a new set of xyabc's + dnat,salmin,npts
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
      subroutine kolaps(icase)
c

      include 'com4a.com'
      include 'mocom.com'

      common/pokom/tdat,bu,x(20),y(20),a(20),b(20),c(20)
     1,hx(20),hy(20),rgt,amt,finamt,dist,salmin,psk,dnat
     2,npts,inpts,npold,ibux,stepno 

      real*8 asn,tdat(3),bj(35)
      real*4 ad(300),bu(900)
      integer*2 ksn(4),kd(600),stepno,ireal(20)
      equivalence(asn,bsn,ksn),(jb,bj),(d,ad,kd),(d(50),ireal)

      itim=0
c            bump c-vals by dist and set all sides to non-real
      do 20 i=1,npts
      ireal(i)=0
20    c(i)=c(i)+dist
c            new x,y intersections
      do 30 j=1,npts
      i=j-1
      if(i.lt.1) i=npts
      den=a(i)*b(j)-b(i)*a(j)
      if(abs(den).lt.1.e-5)goto 98
      x(j)=(b(j)*c(i)-b(i)*c(j))/den
30    y(j)=(a(i)*c(j)-a(j)*c(i))/den
      if(icase.gt.1)goto 32
c            icase 1.   these are good cl points.
      dnat=dnat-dist
      goto 99
c            check sides for real
31    continue
32    nreal=0
      do 35 j=1,npts
      k=j+1
      if(k.gt.npts) k=1
      dx=x(k)-x(j)
      dy=y(k)-y(j)
c             if length=0, side is unreal
      tlsq=dx**2+dy**2
      if(tlsq.lt..00001) goto 34
      rx=dy*rgt
      ry=-dx*rgt
c             ditto if fwd reversal
      rco=rx*a(j)+ry*b(j)
      if(rco.lt.0.) goto 34
c             also imaginary if parallel to a prior real side
      if(nreal.eq.0)goto 33
      den=a(j)*b(ilj)-a(ilj)*b(j)
      if(abs(den).lt.1.e-6)goto 34
33    ireal(j)=1
      ilj=j
      nreal=nreal+1
34    continue
35    continue
c
      if(icase.ne.2) goto 40
c               icase 2.  if nreals .lt.3, island is imaginary
      if(nreal.gt.2)goto 70
      npts=0
      goto 99
c               case 3. if .lt.3 reals, exit with 1 or 2 pts
40    if(nreal.gt.0) goto 45
      npts=1
      goto 99
45    if(nreal-2)50,55,70
c               nreal=1.   find it and exit with that xy
50    do 52 i=1,npts
      if(ireal(i).eq.0)goto 53
52    continue
53    x(1)=x(i)
      y(1)=y(i)
      npts=1
      goto 99
c               nreal=2.   output those two pts
55    j=0
      do 56 i=1,npts
      if(ireal(i).eq.0)goto 56
      j=j+1
      x(j)=x(i)
      y(j)=y(i)
56    continue
      npts=2
      goto 99
c             build new list of real sides
70    j=0
      do 72 i=1,npts
      if(ireal(i).eq.0)goto 72
      j=j+1
      a(j)=a(i)
      b(j)=b(i)
      c(j)=c(i)
72    continue

c            new xy pts per abc list
      npts=nreal
      do 75 j=1,npts
      i=j-1
      if(i.lt.1)i=npts
      den=a(i)*b(j)-b(i)*a(j)
      if(abs(den).gt.1.e-5)goto 74
c               no intersection possible.  assume imaginary island
c          case and exit.                        pem      29-jan-86
      npts=0
      goto 99
74    continue
      x(j)=(b(j)*c(i)-b(i)*c(j))/den
75    y(j)=(a(i)*c(j)-a(j)*c(i))/den

c               calc salmin and dnat this pt set
      dnat=1.e5
      salmin=dnat
      do 80 j=1,npts
      i=j-1
      k=j+1
      if(i.lt.1) i=npts
      if(k.gt.npts) k=1
      aa=-b(i)-b(j)
      bb=a(i)+a(j)
      cc=aa*x(j)+bb*y(j)
      dd=-b(j)-b(k)
      ee=a(j)+a(k)
      ff=dd*x(k)+ee*y(k)
      den=aa*ee-bb*dd
      if(abs(den).lt.1.e-5)goto 98
      xi=(ee*cc-bb*ff)/den
      yi=(aa*ff-dd*cc)/den
      dis=a(j)*xi+b(j)*yi-c(j)
      if(dis.lt.dnat) dnat=dis

c               sin of half-angle
      co=a(i)*a(j)+b(i)*b(j)
      if(co.gt.1.)co=1.
      si=.707107*sqrt(1+co)
      if(si.lt.salmin) salmin=si
80    continue
c               if island case, more work may be reqd.
      if(icase.ne.2.or.dnat.gt.0.)goto 99
      itim=itim+1
      if(itim.gt.5)goto 98
c               zero ireals and go chk sides for real again
      do 86 i=1,npts
86    ireal(i)=0
      goto 31
c               error exit
98    if(ifl(2).lt.1) ifl(2)=163
99    continue
999   return
      end
