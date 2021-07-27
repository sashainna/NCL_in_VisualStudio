c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbgqlt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:50
c**
c*****************************************************

c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbgqlt
c **
c **  purpose of subroutine: to get a quilt surface from the database.
c **
c **********************************************************************
c **********************************************************************

      subroutine dbgqlt (asw,nclkey)

      include 'com4a.com'

      real*8 asw
      integer*4 nclkey

      real*8 srfhed(20),asn,qltpat(40)
      real*4 asfhed(40)
      integer*2 isfhed(4),ksn(4),ipat,npats,ipg,iel
      equivalence (qltpat,srfhed,asfhed,isfhed),(asn,ksn)

      asn=asw
      ipg=ksn(1)
      iel=ksn(2)
      call dbgent(srfhed,20,ksn(1),ksn(2))
      if (ifl(72).eq.1) call trnqhd(asfhed,sc(56))
      call ptqhed(srfhed,nclkey)
      npats=isfhed(2)
      iel=iel+20
      if (iel.gt.35) then
        iel=iel-35
        ipg=ipg+1
      endif
      
      do 100 ipat=1,npats
      call dbgent(qltpat,40,ipg,iel)
      if (ifl(72).eq.1) call trnqlt(qltpat,sc(56))
      call ptqpat(nclkey,ipat,qltpat)
      iel=iel+40
80    if (iel.lt.36) goto 100
      iel=iel-35
      ipg=ipg+1
      goto 80
100   continue

      return
      end
