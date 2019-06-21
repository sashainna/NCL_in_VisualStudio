c*****************************************************
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       dbpmsh.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:50
c**
c*****************************************************
c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbpmsh
c **
c **  purpose of subroutine: to put a mesh surface into the database.
c **
c **********************************************************************
c **********************************************************************

      subroutine dbpmsh (asw,iretpg,iretel)

      include 'com4a.com'

      real*8 asw
      integer*2 iretpg,iretel

      real*8 srfhed(3),mshpat(26)
      integer*4 nclkey
      integer*2 isfhed(12),nw,ipat,npats,ietype
      equivalence (srfhed,isfhed)
      real*8 dbver,dbver1
      equivalence (dbver,sc(166))
      parameter (dbver1=8.1)
      logical redef

      redef=.false.

      call gtgeom(asw,srfhed,nclkey,nw,ietype)
      iclos = 0
      if (dbver.ge.dbver1) then
        iclos=1
        srfhed(3)=0.d0
        call gtclsd(nclkey,0,isfhed(9))
        call gtclsd(nclkey,1,isfhed(10))
      endif
      if (ifl(72).eq.1) call trnshd(nclkey, srfhed, sc(68))
      nw = 2
      if (ldb501) then
        if (isfhed(5).eq.7) then
          iretpg = 0
          goto 999
        endif
        nw = 1
      endif
      call dbpent(srfhed,iclos+nw,iretpg,iretel,redef)
      npats=isfhed(2)
      
      do 100 ipat=1,npats
      call gtmptt(nclkey,ipat,mshpat)
      if (ifl(72).eq.1) call trnmsh(mshpat,sc(68))
      call dbpent(mshpat,26,idbpg,idbel,redef)
100   continue

999   return
      end
