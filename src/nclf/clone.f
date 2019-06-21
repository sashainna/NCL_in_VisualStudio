c*****************************************************
c**
C**    MODULE NAME AND RELEASE LEVEL
C**       clone.f , 25.2
C**    DATE AND TIME OF LAST  MODIFICATION
C**       10/13/15 , 10:59:59
c**
c*****************************************************
c**
c** copyright (c) 1986 mills data systems corporation
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: CLONE
C **
C **  PURPOSE OF PROGRAM: CREATE GEOMETRY BY TRANSFORMING ANOTHER ENTITY
C **    THROUGH A MATRIX.
C **
C **    ARGUMENT ITSK = 1  CLONE WITH NO NAMGEN
C **                  = 2  CLONE WITH NAMGEN
C **                  = 3  MOVE
C **
C **********************************************************************
C **********************************************************************
C **********************************************************************

      subroutine clone (itsk)

      include 'com4a.com'
      include 'wrksys.com'
      parameter (maxpt=50)
      parameter (maxwd=(maxpt*14+(maxpt+1)/2)+2)

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold, calls

      real*8 buf(maxwd), rmx(12), asn, tbuf(3), trmx(12)
      integer*2 ksn(4),itsk,ietype, isv72,nw,ibuf(1200),istsv
      integer*4 nclkey,pankey,srfkey,bskey,pntyp,isub,ier,ngeo,igeo
      real*4 abuf(600)
      equivalence (asn, ksn), (nw, ksn(3)), (ibuf, buf, abuf)
      logical trflg
      character*64  tempid
      integer*2  irest(4), idum, ndx
      equivalence (rest, irest)
      integer*2 POINTV, VECV
      real*8 origin(3)
      data POINTV /3/, VECV /4/

      if (nextyp .ne. 5) goto 9022
      ifl(44) = 9
      calls = 0
c
c...Get the matrix to use
c
      call svpars
      inc = 0
      trflg = .false.
    4 call parsit
      if (ityp .eq. 2 .and. ist .eq. matrix .and. 
     1    (nextyp .eq. 11 .or. (ifl(111) .eq. 1 .and. nextyp .eq. 7)))
     2        then
          call gtentt (tv, trflg, nclkey, ietype, rmx)
          go to 5
      else if (nextyp .eq. 11) then
          go to 9094
      else if (itsk .ne. 3 .and. inc .eq. 1) then
          go to 9094
      endif
      inc = 1
      go to 4
c
c...Loop through entities
c...Check for end of command
c
    5 call rtpars
    7 call parsit
      if (nextyp .eq. 11) then
          if (itsk .ne. 3) go to 400
          call addskg (asn,99,ngeo)
          igeo = 1
          go to 15
      endif
      call idtovc (idum,ist,ndx)
c
c...Check for symbol placement
c
      if (ityp .eq. 2 .and. ist .eq. 1) then
          call ub_symbol_name (token2,ivxsub,nclkey,origin,i)
          if (i .eq. 2) then
              ist = VPLACE
              call ptdesc (nclkey,ist,tv)
              geom = .true.
          endif
      endif
c          if type of token parsed is not valid geometry, report error
      if (ityp .eq. 2 .and. (.not. geom .or. (geom .and. 
     -    (ist .eq. 10 .or. ist .eq. 18) .and. ist .ne. 1))) then
          if (itsk .eq. 3) goto 7
          go to 9244
      endif
      if (ityp .ne. 2 .or. .not. geom) goto 9244
      asn = tv
      istsv = ist
      if (itsk .eq. 3) then
          call addskg (asn,ist,ngeo)
          if (nextyp .ne. 11) go to 7
          call addskg (asn,99,ngeo)
          igeo = 1
      endif
c
      if (itsk .eq. 1) goto 9
      if (itsk .eq. 2) goto 10
      if (itsk .eq. 3) goto 15

c                              *****  clone & name was supplied
9     if (idst .eq. 1) goto 20
      if (ifl(41) .eq. 0) goto 9008
      if (idst .ne. ist) goto 9089
      goto 20

c                              *****  clone & no name supplied - generate one
10    isvtyp = ifl(ndx) + 1
      idst = ist

c             the 0 passed to namgen means get the next name but don't
c             bump the unibase counter until after the call to vstore.
12    if (ist .eq. VPLACE) then
          call ubf_instance_namgen (nclkey,tempid,isub,ier)
          isvsub = isub
          if (ier .ne. 0) goto 9425
      else
          call namgen (0, idst, tempid, isub)
          if (isub.gt.999999) then
               ifl(2) = 85
               goto 99999
          endif
      endif
      token2 = tempid
      ivxsub = 0
      call vstchk
      token2=tempid
      ivxsub = isub
      savid2 = token2
      isvsub = ivxsub
c		For entities with subscripted labels check if the label is 
c		used, if yes then bump the unibase counter.
      if (ist .eq. 14 .and. isub .gt. 0) then
		call vstchk
	endif
c			Removed this condition so as not to use reserved entity labels.
c      if (ist .ne. 1 .and. ist .ne. 14) then
      if (ist .ne. 1) then
c         if (idst .eq. 18) then
c             ifl(143) = ifl(143) + 1
c             isvtyp = ifl(143) + 1
c         else if (idst.eq.20) then
c             ifl(277) = ifl(277) + 1
c             isvtyp = ifl(277) + 1
c         else
c             ifl(idst+10) = ifl(idst+10) + 1
c             isvtyp = ifl(idst+10) +  1
c         endif
          isvtyp = ifl(ndx) + 1
c              force namgen to update label counter and get next name
         call namgen (1, idst, tempid, isub)
         if (isub.gt.999999) then
           ifl(2) = 85
           goto 99999
         endif
         go to 12
      endif
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
c
c            if subscripted save reseve id in ifl(9) , ifl(10)
c            location the first time
      if (ivxsub .gt. 0.and. ist.eq.1 .and. istsv .ne. VPLACE) then
         isvsub=0
         isvst = idst
         idst=14
         rest=0.
         irest(3)=32767
         irest(4)=14
         call vstore
         idst = isvst
         token2 = tempid
         ivxsub = isub
         savid2 = token2
         isvsub = ivxsub
      endif
c             if subscripted get the page and element number to save the
c             geometry.
      if (ivxsub .gt. 0 .and. ist .ne. VPLACE) then
         call vstchk
         if (ist .ne. 1) then
c           if (idst .eq. 18) then
c              ifl(143) = ifl(143) + 1
c              isvtyp = ifl(143) + 1
c           else if (idst .eq. 20) then
c              ifl(277) = ifl(277) + 1
c              isvtyp = ifl(277) + 1
c           else
c              ifl(idst+10) = ifl(idst+10) + 1
c              isvtyp = ifl(idst+10) +  1
c           endif
            isvtyp = ifl(ndx) + 1
c                  force namgen to update label counter and get next name
            call namgen (1, idst, tempid, isub)
            if (isub.gt.999999) then
                 ifl(2) = 85
                 goto 99999
            endif
            go to 12
         endif
         ifl(9) = ifl(11)
         ifl(10) = ifl(12)
      endif
      goto 25

c                              *****  move - use name in statement
   15 call gtskg (igeo, asn, ist)
      igeo = igeo + 1
      if (ist .eq. 99) go to 400
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
20    idst = ist
25    continue

      call gtdesc (asn,nclkey,nwds,ietype)
      if (itsk .eq. 3) then
          call nclf_getlabel (nclkey,token2,ivxsub)
          savid2 = token2
          isvsub = ivxsub
          keyold = nclkey
          istold = ist
      endif

c
c... aak 10-apr-1998: disallow copying curves on surfaces or composite
c... curves composed of curves on surfaces
c.......Use temporary matrix for surfaces so entries in transformation
c.......matrix stay consistent after possible unit conversion
c
      call ncl_cvonsf_get_bskey (nclkey,bskey)
      if (bskey.gt.0) goto 9383
c
c...check if copying of surface with uv curves on it
c
      call ncl_gtssnum (nclkey,nss)
c     if (itsk .ne. 3 .and. nss .gt. 0) call error (-468)
      call isitwf (nclkey, iwf)
      if (iwf.eq.1) then
        do 27 i=1,12
          trmx(i) = rmx(i)
27      continue
        call isitev (nclkey, iev)
        if (iev.eq.1) then
          call clnent (nclkey, trmx, srfkey)
        else
          call cvtype1 (nclkey,nss)
          if (nss .eq. 13) go to 9468
          call clnwff (itsk, nclkey, ietype, trmx, srfkey)
          if (srfkey.eq.0) goto 9163
        endif
        if (istsv .eq. VPLACE .and. itsk .ne. 3)
     1      call ubf_instance_label (srfkey,savid2,isvsub)
        call ptdesc(srfkey,ietype,asn)
        ksn(3)=nwds
        goto 330
      endif
c                             patern
      if (idst.eq.20) then
         call gtpnnp (nclkey,npts,pntyp)
         ibuf(1)=npts
         call ptpnhd (buf,pntyp,pankey,1)
         call ptdesc (pankey,ietype,asn)
         do 30 i=1,npts
           call gpnptt (buf, nclkey, i, trflg)
           call conent (buf,rmx,POINTV)
           if (pntyp.eq.2) call conent (buf(4),rmx,VECV)
           call ppnptt (buf, pntyp, pankey, i, trflg)
30       continue

c         if entity is not a surface, do a straight tranformation
      else if (idst .ne. 9) then
c           get geom canonical data
          call gtentt (asn, trflg, nclkey, ietype, buf)
c              if entity is a plane, get display point
          if (ietype.eq.6) then
            call gtdpt (nclkey, buf(5))
c
c.....keeps unit conversion conistent for transform - Andrew 10/18/12
c
            if (ifl(264).eq.1) then
                buf(5) = buf(5)*25.4d0
                buf(6) = buf(6)*25.4d0
                buf(7) = buf(7)*25.4d0
            endif
            nw = 7
          else if (ietype .eq. 8) then
            call ncl_get_tpar (nclkey,tbuf)
          endif
c              transform it
          call transf (buf, rmx, nw, ietype)
          if (ifl(2) .gt. 0) goto 9999

c              store generated canonical data to unibase
          isv72 = ifl(72)
          ifl(72) = 0
          call ptentt (ietype, buf, nclkey, asn)
          if (ietype .eq. 8) then
            call ncl_put_tpar (nclkey,tbuf)
          endif
          ifl(72) = isv72
      else
          do 32 i=1,12
            trmx(i) = rmx(i)
32        continue
c           determine surface tpye
          call gtdesc(asn,nclkey,nw,ietype)
          call sftype(nclkey,isftyp)
          if (isftyp .eq. 28) then
              call clnent (nclkey, trmx, srfkey)
          else if (isftyp .eq. 27) then
              call isnswf (nclkey, idum)
              if (idum.eq.1) then
                call clnwff (itsk, nclkey, ietype, trmx, srfkey)
              else
                call clnssf(nclkey, trmx, srfkey)
              endif
          else if (isftyp .eq. 26) then 
c                                       mesh surface
              call gtentt (asn, trflg, nclkey, ietype, buf)
              npat=ibuf(2)
              call trnshd(nclkey, buf, trmx)
              call ptmhdt(buf,srfkey)
              call ptmptt(srfkey,npat,buf)
              do 210 ipat=1,npat
                  call gtmptt(nclkey,ipat,buf)
                  call trnmsh(buf,trmx)
                  if (ifl(2) .gt. 0) goto 9999
210               call ptmptt(srfkey,ipat,buf)
              call ncl_displst_delete (nclkey)
          else if (isftyp .eq. 25) then
c                                        quilt surface
              call gtentt (asn, trflg, nclkey, ietype, buf)
              npat=ibuf(2)
              call trnqhd(buf,trmx)
              if (ifl(2) .gt. 0) goto 9999
              call ptqhed(buf,srfkey)
              do 310 ipat=1,npat
                  call gtqpat(nclkey,ipat,buf)
                  call trnqlt(buf,trmx)
                  if (ifl(2) .gt. 0) goto 9999
310               call ptqpat(srfkey,ipat,buf)
          else if (isftyp .eq. 91) then
c                                      standard surface
              call gtentt (asn, trflg, nclkey, ietype, buf)
c
c...vp 9/23/97 use clnwf to translate ncl surf since uc_transform
c...has method for it, itsk is passed in input srfkey
c
              call clnwff (itsk, nclkey, ietype, trmx, srfkey)
          else
            goto 9321
          endif
          ietype=9
          call ptdesc(srfkey,ietype,asn)
      endif
c
c...Display entity with
c...Moved entities only
c
  330 continue
      if (itsk .eq. 3) then
          rest = asn
          call vstore
          call dspent (nclkey,ietype)
          go to 15
      endif

400   if (itsk .eq. 3) then
          ifl(262) = 1
          ifl(329) = 1
          defwf = .true.
          call delskg
      else
          rest = asn
          ifl(262) = 1
      endif
      goto 99999

c **********************************************************************
c                  error conditions code
c **********************************************************************
c                            error - end of statement expected
9004  ifl(2) = 4
      goto 9999
c                            error - identifier previously defined
9008  ifl(2) = 8
      goto 9999
c                            error - '/' expected
9022  isvinx = inx
      ifl(2) = 22
      goto 9999
c                             error - identifier currently has different type
9089  ifl(2) = 89
      goto 9999
c                             error - matrix expected
9094  ifl(2) = 94
      goto 9999
c                             error - Impossible geometry case
9163  ifl(2) = 163
      goto 9999
c                             error - valid geometry name expected
9244  ifl(2) = 244
      ityp = 1
      goto 9999
c                             error - not valid for this sf type
9321  ifl(2) = 321
      goto 9999
c                             error - not valid for this cv type
9383  ifl(2) = 383
      goto 9999
c                             error - could not generate name
9425  ifl(2) = 425
      goto 9999
c                             error - not valid for this surface 
9468  ifl(2) = 468
      goto 9999

9999  continue
99999 return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine clnwff (nclkey, rmx, srfkey)
C*      Clone entity after adjusting for modsys & units
C*    PARAMETERS
C*       INPUT  :
C*          nclkey   - Key of entity to clone
C*          rmx      - Matrix through which to transform entity
C*          srfkey   - type of clone
C*       OUTPUT :
C*          rmx      - matrix updated for units & modsys
C*          srfkey   - Key of cloned entity
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine clnwff (itsk, nclkey, ietype, rmx, srfkey)

      include 'com.com'
      include 'wrksys.com'

      integer*2 itsk,ietype
      integer*4 nclkey, srfkey
      real*8 rmx(12)
c
      integer*4 itype
c
c...Cannot clone non-standard annotation
c
      if (ietype .eq. VANOTE) then
          call uaf_get_note_type(nclkey,itype)
          if (itype .ne. 1) then
              srfkey = 0
              go to 99999
          endif
      endif

      srfkey = itsk      
      if (ifl(264).eq.1) then
        rmx(4) = rmx(4)/25.4d0
        rmx(8) = rmx(8)/25.4d0
        rmx(12) = rmx(12)/25.4d0
      endif
      if (lwrk) then
        call mxmult (rmx, invwrk, rmx)
        call mxmult (wrkmx, rmx,  rmx)
      endif
      defwf = .true.
      call clnwf (nclkey, rmx, srfkey)
c
c...Set blanked attribute
c
      if ((ietype .eq. POINT .and. dsplpt) .or.
     x    (ietype .eq. VECTOR .and. dsplve) .or.
     x    (ietype .eq. LINE .and. dsplln) .or.
     x    (ietype .eq. PLANE .and. dsplpl) .or.
     x    (ietype .eq. PNTVEC .and. ldsppv) .or.
     x    (ietype .eq. CIRCLE .and. dsplci) .or.
     x    (ietype .eq. CURVE .and. dsplcv) .or.
     x    (ietype .eq. SURF .and. ldspsf) .or.
     x    (ietype .eq. SHAPE .and. ldspsh) .or.
     x    (ietype .eq. MATRIX .and. dsplmx) .or.
     x    (ietype .eq. VANOTE .and. dsplan) .or.
     x    (ietype .eq. PATERN .and. ldsppn) .or.
     x    (ietype .eq. VSOLID .and. dsplso) .or.
     x    (ietype .eq. VPLACE .and. dsplsy)) then
      else
          call blkgeo (srfkey, 1)
      endif

99999 return
      end
