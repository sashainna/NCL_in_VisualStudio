c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbpqlt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:51
c**
c*****************************************************
c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbpqlt
c **
c **  purpose of subroutine: to put a quilt surface into the database.
c **
c **********************************************************************
c **********************************************************************

      subroutine dbpqlt (asw,iretpg,iretel)

      include 'com4a.com'

      real*8 asw
      integer*2 iretpg,iretel

      real*8 srfhed(20),qltpat(40)
      integer*4 nclkey
      integer*2 isfhed(4),nw,ipat,npats,ietype
      equivalence (srfhed,isfhed)
      logical redef

      redef=.false.

      call gtgeom(asw,srfhed,nclkey,nw,ietype)
      call dbpent(srfhed,20,iretpg,iretel,redef)
      npats=isfhed(2)
      
      do 100 ipat=1,npats
      call gtqpat(nclkey,ipat,qltpat)
      if (ifl(72).eq.1) call trnqlt(qltpat,sc(68))
      call dbpent(qltpat,40,idbpg,idbel,redef)
100   continue

      return
      end
