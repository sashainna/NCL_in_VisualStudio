c*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL
C*       entcpy.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:01
c**
c*****************************************************
c**
c** copyright (c) 1993 NCCS
C**
C **********************************************************************
C **  PROGRAM NAME: ENTCPY
C **
C **  PURPOSE OF PROGRAM: CREATE an entity BY COPYING ANOTHER ENTITY
C **
C **********************************************************************

      subroutine entcpy

      include 'com.com'

      integer*2 maxpt, maxwd
      parameter (maxpt=50)
      parameter (maxwd=(maxpt*14+(maxpt+1)/2)+2)

      real*8 buf(maxwd), rmx(12), asn
      integer*2 ksn(4),nwds,ietype, isv72,nw,ibuf(1200)
      integer*2 iwf, iev, npts, i, isftyp, npat, ipat, npan, ipan, ix
      integer*4 nclkey,pankey,srfkey,pntyp
      real*4 abuf(600)
      equivalence (asn, ksn), (nw, ksn(3)), (ibuf, buf, abuf)
      logical trflg
      integer*2 i2v1

      data rmx /1.,0.,0.,0., 0.,1.,0.,0., 0.,0.,1.,0./
      data i2v1 /1/

c
c...WinNT
c
cc      integer*2 PNTVEC
cc      parameter (PNTVEC=21)
c
c
c...These parameters (ist & tv) are set in the calling routine.
c
      idst = ist
      asn = tv
      call gtdesc (asn,nclkey,nwds,ietype)
c                             wf
      call isitwf (nclkey, iwf)
      if (iwf.eq.1) then
        call isitev (nclkey, iev)
        if (iev.eq.1) then
          call clnent (nclkey, rmx, srfkey)
        else
          defwf = .true.
c
c...vp 9/23/97 set copy flag to copy
c
          srfkey = 1
          call clnwf (nclkey, rmx, srfkey)
          if (srfkey.eq.0) goto 9163
        endif
        call ptdesc(srfkey,ietype,asn)
        ksn(3)=nwds
        nclkey = srfkey
        goto 400
      endif
c                             patern
      if (idst.eq.20) then
         call gtpnnp (nclkey,npts,pntyp)
         ibuf(1)=npts
         call ptpnhd (buf,pntyp,pankey,1)
         call ptdesc (pankey,ietype,asn)
         do 30 i=1,npts
           call gtpnpt (buf,pntyp,nclkey,i)
           call ptpnpt (buf,pankey,i)
30       continue
         nclkey = pankey
         if (.not. ldsppn) call blkgeo (nclkey, i2v1)
c
c         if entity is not a surface, do a straight tranformation
      else if (idst .ne. 9) then
c
c           get geom canonical data
          call gtentt (asn, trflg, nclkey, ietype, buf)
c              if entity is a plane, get display point
          if (ietype.eq.6) then
            call gtdpt (nclkey, buf(5))
            nw = 7
          endif

c              store generated canonical data to unibase
          isv72 = ifl(72)
          ifl(72) = 0
          call ptentt (ietype, buf, nclkey, asn)
          ifl(72) = isv72
      else
c           determine surface tpye
          call gtdesc(asn,nclkey,nw,ietype)
          call sftype(nclkey,isftyp)
          if (isftyp .eq. 28) then
              call clnent (nclkey, rmx, srfkey)
          else if (isftyp .eq. 27) then
              call clnssf(nclkey, rmx, srfkey)
          else if (isftyp .eq. 26) then 
c                                       mesh surface
              call gtentt (asn, trflg, nclkey, ietype, buf)
              npat=ibuf(2)
              call ptmhdt(buf,srfkey)
              call ptmptt(srfkey,npat,buf)
              do 210 ipat=1,npat
                  call gtmptt(nclkey,ipat,buf)
210               call ptmptt(srfkey,ipat,buf)
          else if (isftyp .eq. 25) then
c                                        quilt surface
              call gtentt (asn, trflg, nclkey, ietype, buf)
              npat=ibuf(2)
              call ptqhed(buf,srfkey)
              do 310 ipat=1,npat
                  call gtqpat(nclkey,ipat,buf)
310               call ptqpat(srfkey,ipat,buf)
          else if (isftyp .eq. 91) then
c                                      standard surface
              call gtentt (asn, trflg, nclkey, ietype, buf)
              npan=ibuf(2)
              call ptshed (buf, srfkey)
              do 130 ipan=1,npan
c                                      get panel ipan header
                  call gtspa1(nclkey,ipan,buf,pankey)
                  npat=ibuf(2)+1
                  ix=npat/2+3
                  nwds=14
                  if (ibuf(1).eq.1) nwds=8
c                                      get patches of panel ipan
                  do 120 ipat=1,npat
                      call gtpptt(pankey,ipat,buf(ix),nwds)
120                   ix=ix+nwds
130               call ptspnt(srfkey,ipan,buf)
          else
            goto 9321
          endif
          ietype=9
          call ptdesc(srfkey,ietype,asn)
          nclkey = srfkey
      endif

400   rest = asn
      if (ietype.eq.SURF) then
        call srfcls (nclkey)
        if (.not. ldspsf) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.CURVE) then
        call crvcls (nclkey)
        if (.not. dsplcv) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.CIRCLE) then
        if (.not. dsplci) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.PLANE) then
        if (.not. dsplpl) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.LINE) then
        if (.not. dsplln) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.VECTOR) then
        if (.not. dsplve) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.POINT) then
        if (.not. dsplpt) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.MATRIX) then
        if (.not. dsplmx) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.PATERN) then
        if (.not. ldsppn) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.PNTVEC) then
        if (.not. ldsppv) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.VANOTE) then
        if (.not. dsplan) call blkgeo (nclkey, i2v1)
      else if (ietype.eq.VSOLID) then
        if (.not. dsplso) call blkgeo (nclkey, i2v1)
      endif
      call dspent (nclkey,ietype)
      goto 99999

c **********************************************************************
c                  error conditions code
c **********************************************************************
c                             error - Impossible geometry case
9163  ifl(2) = 163
      goto 9999
c                             error - not valid for this sf type
9321  ifl(2) = 321
      goto 9999

9999  continue
99999 return
      end
