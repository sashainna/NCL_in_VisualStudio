c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbpnsf.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:50
c**
c*****************************************************
c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbpnsf
c **
c **  purpose of subroutine: to put an ncl surface into the database.
c **
c **********************************************************************
c **********************************************************************


      subroutine dbpnsf (asw,iretpg,iretel)

      include 'com4a.com'

      parameter (maxpt=50)
      parameter (maxpn=20)
      parameter (mxsfhd=(3+(maxpn+1)/2))
      parameter (mxpnhd=(2+(maxpt+1)/2))

      real*8 asw
      integer*2 iretpg,iretel

      real*8 srfhed(mxsfhd),panel(mxpnhd),asn
      integer*4 nclkey,ipnkey
      integer*2 isfhed(28),ipanel(4),nw,idbpg,idbel,ietype,ksn(4)
      integer*2 ipat,npats,ipan,npans
      equivalence (srfhed,isfhed),(panel,ipanel),(asn,ksn)
      real*8 dbver,dbver1
      equivalence (dbver,sc(166))
      parameter (dbver1=8.1)
      logical redef

      redef=.false.
c                           get sf header
      asn=asw
      ksn(3)=1
      call gtgeom(asn,srfhed,nclkey,nw,ietype)
      npans=isfhed(2)
      ipgoff=7
      if (dbver.ge.dbver1) then
        srfhed(3)=0.d0
        call gtclsd(nclkey,0,isfhed(9))
        call gtclsd(nclkey,1,isfhed(10))
        ipgoff=11
      endif
      if (ifl(72).eq.1) call trnshd (nclkey, srfhed, sc(68))
c                           get and store panels      
      do 100 ipan=1,npans
c                           get panel header
      call gtspa1(nclkey,ipan,panel,ipnkey)
      npats=ipanel(2)+1
      nw=2+npats/2
c                           store panel header and update sf header with
c                           page and element.
      call dbpent(panel,nw,idbpg,idbel,redef)
      isfhed(ipan*2+ipgoff)=idbpg
      isfhed(ipan*2+ipgoff+1)=idbel

      nw=14
      if (ipanel(1).eq.1)nw=8
c                                get and store patches
      do 100 ipat=1,npats
      call gtpptt(ipnkey,ipat,panel,nw)
      if (ifl(72).eq.1) call transf(panel,sc(68),nw,91)
      call dbpent(panel,nw,idbpg,idbel,redef)
100   continue
c                                store surface header & return pg & el
      call dbpent(srfhed,mxsfhd,iretpg,iretel,redef)

      return
      end

