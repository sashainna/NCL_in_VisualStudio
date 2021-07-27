c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbgnsf.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:50
c**
c*****************************************************
c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbgnsf
c **
c **  purpose of subroutine: to get an ncl surface from the database.
c **
c **********************************************************************
c **********************************************************************


      subroutine dbgnsf (asw,nclkey)

      include 'com4a.com'

      parameter (maxpt=50)
      parameter (maxpn=20)
      parameter (mxsfhd=(3+(maxpn+1)/2))
      parameter (mxpnhd=(2+(maxpt+1)/2))
      parameter (mxpnsz=(2+(maxpt+1)/2)+14*maxpt)

      real*8 asw
      integer*4 nclkey

      real*8 srfhed(mxsfhd),asn,panel(mxpnsz), ofstwd
      real*4 abuf(320),aoffst(2)
      real*4 vn(3),vu(3),vv(3),xn,yn,zn,sec
      integer*2 isfhed(28),ksn(4),ipanel(4),nw,ipan,npans,idbpg,idbel
      integer*2 ioffst(4)
      equivalence (srfhed,isfhed),(asn,ksn),(panel,abuf,ipanel)
      equivalence (ofstwd, aoffst, ioffst)
      real*8 tsfhed(3)
      integer*2 itsfhd(12)
      integer*4  nup,nupt,nvp,nvpt 
      equivalence (tsfhed,itsfhd)
      real*8 dbver,dbver1
      equivalence (dbver,sc(166))
      parameter (dbver1=8.1)
      
      asn=asw
      call dbgent(srfhed,2,ksn(1),ksn(2))
      npans=isfhed(2)
      nw=2+(npans+1)/2
      ipgoff=7
      if (dbver.ge.dbver1) then
        nw=nw+1
        ipgoff=11
      endif
      call dbgent(srfhed,nw,ksn(1),ksn(2))
      if (ifl(72).eq.1.and.isfhed(5).eq.7) then
        ofstwd=srfhed(2)
        ipan=(npans+1)/2
        idbpg=isfhed(ipan*2+ipgoff)
        idbel=isfhed(ipan*2+ipgoff+1)
        call dbgent(panel,1,idbpg,idbel)
        npats=ipanel(2)
        ipat=(npats+1)/2
        if (ipanel(1).eq.1) then
          ix=10
          nw=8
          idbel=idbel-6+8*ipat+(npats+1)/2
        else
          ix=16
          nw=14
          idbel=idbel-12+14*ipat+(npats+1)/2
        endif
10      if (idbel.gt.35) then
          idbel=idbel-35
          idbpg=idbpg+1
          goto 10
        endif
        call dbgent(panel,nw,idbpg,idbel)
c                               reverse & scale offset distance if necessary
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
        call conent(vn,sc(56),41)
        sec=sqrt(vn(1)**2+vn(2)**2+vn(3)**2)
        call conent(vu,sc(56),41)
        call conent(vv,sc(56),41)
        xn=vu(2)*vv(3)-vu(3)*vv(2)
        yn=vu(3)*vv(1)-vu(1)*vv(3)
        zn=vu(1)*vv(2)-vu(2)*vv(1)
        if (vn(1)*xn+vn(2)*yn+vn(3)*zn.lt.0.)sec=-sec
        aoffst(2)=aoffst(2)*sec
        srfhed(2)=ofstwd
  
      endif
      tsfhed(1)=srfhed(1)
      tsfhed(2)=srfhed(2)
      call gtsfdp (nup,nupt,nvp,nvpt)
c     itsfhd(9)=ifl(137)
c     itsfhd(10)=ifl(138)
c     itsfhd(11)=ifl(137)
c     itsfhd(12)=ifl(138)
      itsfhd(9) = nup
      itsfhd(10) = nvp
      itsfhd(11) = nupt
      itsfhd(12) = nvpt
      call ptshed(tsfhed,nclkey)
      if (dbver.ge.dbver1) then
        if (isfhed(9).eq.1) call ptclsd(nclkey,0,1)
        if (isfhed(10).eq.1) call ptclsd(nclkey,1,1)
      endif
c                           get and store panels
      do 100 ipan=1,npans
c                           get panel header
      idbpg=isfhed(ipan*2+ipgoff)
      idbel=isfhed(ipan*2+ipgoff+1)
      call dbgent(panel,12,idbpg,idbel)
      npats=ipanel(2)+1
      iw=14
      if (ipanel(1).eq.1)iw=8
      nw=2+npats/2+npats*iw
      call dbgent(panel,nw,idbpg,idbel)
c                        zero unused rho of last patch in case junk in 
c                        it kills unibase storage code.    27-Aug-87
      abuf(nw*2)=0.
      call ptspnt(nclkey,ipan,panel)
100   continue

      return
      end
