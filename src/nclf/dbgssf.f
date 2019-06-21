c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbgssf.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:50
c**
c*****************************************************
c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbgssf
c **
c **  purpose of subroutine: to get a super surface from the database.
c **
c **********************************************************************
c **********************************************************************


      subroutine dbgssf (asw,nclkey)

      include 'com4a.com'

      real*8 asw
      integer*4 nclkey

      real*8 ssfhed(81),buf(61),srfhed,asn
      integer*4 ikey,i4sshd(160)
      integer*2 isshed(320),isfhed(4),ksn(4),nw,isf,nsf
      equivalence (ssfhed,i4sshd,isshed),(srfhed,isfhed),(asn,ksn)

c                        get header
      asn=asw
      nw=ksn(3)
      call dbgent(ssfhed,nw,ksn(1),ksn(2))
      buf(1)=ssfhed(1)
      nsf=isshed(2)

      do 10 i=1,nsf
10    buf(i+21)=ssfhed(i+nsf+1)

      call ssfcre(buf, nclkey)

      do 100 isf=1,nsf
      asn=ssfhed(isf+1)
      call dbgent(srfhed,1,ksn(1),ksn(2))
      isftyp=isfhed(1)
      if (isftyp.eq.26) then
        call dbgmsh(asn,ikey)
      else
        call dbgnsf(asn,ikey)
      endif
      call nevdsp(ikey)
      call ssfupd(nclkey, isf, ikey)
100   continue

c      call ptntsf(ssfhed,nclkey)

99999 return
      end
