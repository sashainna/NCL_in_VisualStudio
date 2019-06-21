c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbpssf.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:51
c**
c*****************************************************
c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbpssf
c **
c **  purpose of subroutine: to put a super surface into the database.
c **
c **********************************************************************
c **********************************************************************


      subroutine dbpssf (asw,idbpg,idbel,idbnw)

      include 'com4a.com'

      real*8 asw
      integer*2 idbpg,idbel,idbnw

      real*8 ssfhed(61),srfhed(81),asn
      integer*4 nclkey,ikey,jsn(2),i4sshd(2)
      integer*2 isshed(2),isfhed(4),ksn(4),nw,isf,nsf
      equivalence (ssfhed,i4sshd,isshed),(srfhed,isfhed),(asn,jsn,ksn)
      logical redef

      redef=.false.
c                        get header
      asn=asw
      nclkey=jsn(1)
      call gtgeo(nclkey,ssfhed)
      nsf=isshed(2)
      nw=nsf*2+1
      idbnw=nw
      srfhed(1)=ssfhed(1)

      do 100 isf=1,nsf
c      asn=ssfhed(isf+1)
c      ikey=isn4(1)
c      call gtgeo(ikey,srfhed)
c      isftyp=isfhed(1)
      ikey=i4sshd(isf+2)
      call sftype(ikey,isftyp)
      call ptdesc(ikey,9,asn)
      if (isftyp.eq.26) then
        call dbpmsh(asn,ipg,iel)
      else
        call dbpnsf(asn,ipg,iel)
      endif
      ksn(1)=ipg
      ksn(2)=iel
      srfhed(isf+1)=asn
100   continue
c                          move in map
      do 120 i=1,nsf
120   srfhed(i+nsf+1)=ssfhed(i+21)

      call dbpent(srfhed,nw,idbpg,idbel,redef)

      return
      end
