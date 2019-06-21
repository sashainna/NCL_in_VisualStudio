c*****************************************************
c**
C**    MODULE NAME AND RELEASE LEVEL 
C**       trnshd.f , 25.1
C**    DATE AND TIME OF LAST  MODIFICATION
C**       04/29/15 , 15:10:49
c**
c*****************************************************
c**
c** copyright (c) 1986 mills data systems corporation
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: trnshd(nclkey, sfhead, rmx)
C **
C **  PURPOSE OF PROGRAM: transform a mesh or ncl surface header through
C **    a matrix. Only needed for offset surfaces.
C **
C **   Input arguments - 
C **                      nclkey - key of surface
C **                      sfhead - header of surface
C **                      rmx    - matrix to transform header through
C **   Output arguments -
C **                      sfhead - Transformed surface head
C **
C **********************************************************************
C **********************************************************************

      subroutine trnshd (nclkey, sfhead, rmx)

      include 'com4a.com'

      integer*4 nclkey
      real*8 sfhead(2), rmx(12) 

      real*8 buf(26), ofstwd, asn
      real*4 abuf(52),aoffst(2)
      integer*2 ibuf(104), ioffst(4), ksn(4)
      integer*4 ipnkey
      equivalence (ofstwd, aoffst, ioffst), (ibuf, buf, abuf)
      equivalence (asn,ksn)
      real*4 vn(3),vu(3),vv(3),xn,yn,zn,sec

      ofstwd=sfhead(2)
      if (ioffst(1).ne.7) goto 99999
      ix=16
c           determine surface type & get middle patch
      call sftype(nclkey,isftyp)
      if (isftyp .eq. 26) then 
        asn=sfhead(1)
        npats=ksn(2)
        ipat=(npats+1)/2
        call gtmptt(nclkey,ipat,buf)
      else
        asn=sfhead(1)
        npans=ksn(2)
        ipan=(npans+1)/2
        call gtspa1(nclkey,ipan,buf,ipnkey)
        npats=ibuf(2)
        ipat=(npats+1)/2
        if(ibuf(1).eq.1) ix=10
        call gtpptt(ipnkey,ipat,buf,nwds)
      endif
c                                      reverse offset distance if necessary
      vu(1)=abuf(7)
      vu(2)=abuf(8)
      vu(3)=abuf(9)
      vv(1)=abuf(IX)
      vv(2)=abuf(ix+1)
      vv(3)=abuf(ix+2)
      xn=vu(2)*vv(3)-vu(3)*vv(2)
      yn=vu(3)*vv(1)-vu(1)*vv(3)
      zn=vu(1)*vv(2)-vu(2)*vv(1)
      sec=sqrt(xn**2+yn**2+zn**2)
      if (sec.lt.1.e-6)sec=1.
      vn(1)=xn/sec
      vn(2)=yn/sec
      vn(3)=zn/sec
      call conent(vn,rmx,41)
      sec=sqrt(vn(1)**2+vn(2)**2+vn(3)**2)
      call conent(vu,rmx,41)
      call conent(vv,rmx,41)
      xn=vu(2)*vv(3)-vu(3)*vv(2)
      yn=vu(3)*vv(1)-vu(1)*vv(3)
      zn=vu(1)*vv(2)-vu(2)*vv(1)
      if (vn(1)*xn+vn(2)*yn+vn(3)*zn.lt.0.)sec=-sec
      aoffst(2)=aoffst(2)*sec
      sfhead(2)=ofstwd
      
99999 return
      end
