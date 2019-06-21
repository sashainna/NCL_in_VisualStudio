c*****************************************************
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       dbgmsh.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:50
c**
c*****************************************************

c**
c** copyright (c) 1987 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbgmsh
c **
c **  purpose of subroutine: to get a mesh surface from the database.
c **
c **********************************************************************
c **********************************************************************


      subroutine dbgmsh (asw,nclkey)

      include 'com4a.com'

      real*8 asw
      integer*4 nclkey

      real*8 srfhed(3),asn,mshpat(26), ofstwd
      real*4 abuf(52),aoffst(2)
      real*4 vn(3),vu(3),vv(3),xn,yn,zn,sec
      integer*2 isfhed(12),ksn(4),ipat,npats,ipg,iel
      integer*2 ibuf(104), ioffst(4)
      integer*4 nup,nupt,nvp,nvpt 
      equivalence (srfhed,isfhed),(asn,ksn)
      equivalence (ofstwd, aoffst, ioffst), (mshpat, ibuf, abuf)
      real*8 dbver,dbver1
      equivalence (dbver,sc(166))
      parameter (dbver1=8.1)

      iclsu=0
      iclsv=0
      asn=asw
      ipg=ksn(1)
      iel=ksn(2)
      call dbgent(srfhed,3,ksn(1),ksn(2))
      if (dbver.ge.dbver1) then
        iclsu=isfhed(9)
        iclsv=isfhed(10)
        iel=iel+1
      endif
      call gtsfdp (nup,nupt,nvp,nvpt)
c     isfhed(9)=ifl(137)
c     isfhed(10)=ifl(138)
c     isfhed(11)=ifl(137)
c     isfhed(12)=ifl(138)
      isfhed(9) = nup
      isfhed(10) = nvp
      isfhed(11) = nupt
      isfhed(12) = nvpt
      npats=isfhed(2)
      if (ldb501) then
        iel=iel+1
        srfhed(2)=0.0d0
      else
        iel=iel+2
      endif
      if (iel.gt.35) then
       iel=iel-35
       ipg=ipg+1
      endif

      if (ifl(72).eq.1.and.isfhed(5).eq.7) then
c                               reverse & scale offset distance if necessary
        ipat=(npats+1)/2
        itpg=ipg
        itel=iel+(ipat-1)*26
10      if (itel.gt.35) then
          itel=itel-35
          itpg=itpg+1
          goto 10
        endif
        call dbgent(mshpat,26,itpg,itel)
        ofstwd=srfhed(2)
c                               reverse & scale offset distance if necessary
        ix=16
        vu(1)=abuf(7)
        vu(2)=abuf(8)
        vu(3)=abuf(9)
        vv(1)=abuf(ix)
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
    
      call ptmhdt(srfhed,nclkey)
      if (iclsu.eq.1) call ptclsd(nclkey,0,1)
      if (iclsv.eq.1) call ptclsd(nclkey,1,1)

c                      write out dummy last patch to allocate mem.
      call ptmptt(nclkey,npats,mshpat)
      
      do 100 ipat=1,npats
      call dbgent(mshpat,26,ipg,iel)
      if (ifl(72).eq.1) call trnmsh(mshpat,sc(56))
      call ptmptt(nclkey,ipat,mshpat)
      iel=iel+26
      if (iel.gt.35) then
        iel=iel-35
        ipg=ipg+1
      endif
100   continue

      return
      end
