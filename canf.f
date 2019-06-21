c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       canf.f , 25.2
c**    DATE AND TIME OF LAST MODIFICATION
c**       08/01/17 , 13:47:56
c**
c*****************************************************
C ***  FILE NAME: CANF
C**
C** COPYRIGHT (C) 1986 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **  SUBROUTINE NAME: CANF
C **
C **  LAST REVISION:
C **
C **  PURPOSE OF SUBROUTINE: RETURN A SPECIFIED ELEMENT OF A CANONICAL
C **   IN VARIABLE 'TV' FOR SUBROUTINE 'EXPRES' TO USE.
C **
C **********************************************************************

      SUBROUTINE CANF

      include 'com8a.com'
      include 'const.com'

      common /cancom/ icx
      integer*2 icx
      character*64 label

      real*8 a(16),val(6)
      real*8 htv(20)
      integer*4 nclkey,ic, sub,ipg,iel,ival(2)
      integer*2 nwds,ix,ietype, ivoc
      logical trflg
      integer*2 FEDRATV, NUMPTSV, MAXDPV, TOLERV, CUTTERV, THICKV
      integer*2 UNITSV, MAXANGV, CONTCTV, PSEUDOV, SHANKV, HOLDERV
      parameter (FEDRATV=1009)
      parameter (NUMPTSV=737)
      parameter (MAXDPV=736)
      parameter (TOLERV=731)
      parameter (CUTTERV=716)
      parameter (PSEUDOV=762)
      parameter (SHANKV=192)
      parameter (HOLDERV=157)
      parameter (THICKV=717)
      parameter (UNITSV=841)
      parameter (MAXANGV=741,CONTCTV=856)

      trflg = .true.
      icx=icx+1
      if (icx.gt.20) goto 9059

      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp.eq.2.and.ist.eq.14) call exprs2
      if (ifl(2).gt.0) goto 999
      if (ityp.eq.2.and.ist.eq.1) goto 9009
      if (ityp.eq.1) then
        htv(icx) = -ist
      else
        if (ityp.ne.2) goto 9001
        if (ist.lt.3.or.(ist.gt.7.and.ist.ne.SURF.and.ist.ne.PNTVEC.and.
     x      ist.ne.MATRIX.and.ist.ne.DATAST.and.ist.ne.VSOLID))
     x          goto 9313
        htv(icx) = tv
      endif

      call parser
      if (ityp.ne.5.or.ist.ne.9) goto 9311
      call parser
      call exprs2
      if (ifl(2).gt.0) goto 999
      if (ityp.ne.3.and.ityp.ne.4.and.(ityp.ne.2.or.ist.ne.2)) goto 9007
      ix=tv
      call parser
      if (ityp.ne.5.or.ist.ne.7) goto 9310

      if (htv(icx).lt.0.0d0) then
        ist = -htv(icx)
        if (ix.lt.1) goto 9312
        if (ist.eq.FEDRATV) then
          if (ix.gt.1) goto 9312
          tv = sc(123)
        else if (ist.eq.NUMPTSV) then
          if (ix.gt.1) goto 9312
          tv = ifl(91)
        else if (ist.eq.MAXDPV) then
          if (ix.gt.1) goto 9312
          tv = sc(54)
        else if (ist.eq.TOLERV) then
          if (ix.gt.4) goto 9312
          if (ix.eq.1) then
            tv = sc(27)
          else if (ix.eq.2) then
            tv = sc(168)
          else if (ix.eq.3) then
            tv = sc(91)
          else if (ix.eq.4) then
            tv = dacos(sc(92))*RADIAN
          endif
        else if (ist.eq.CUTTERV) then
          if (ix.gt.6) goto 9312
          call obcutr (val,ic)
          tv = val(ix)
        else if (ist.eq.PSEUDOV) then
          if (ix.gt.5) goto 9312
          call obcuds (val,ic)
          tv = val(ix)
        else if (ist.eq.SHANKV) then
          if (ix.gt.5) goto 9312
          call obhold (val,1)
          tv = val(ix)
        else if (ist.eq.HOLDERV) then
          if (ix.gt.6) goto 9312
          call obhold (val,2)
          tv = val(ix)
        else if (ist.eq.THICKV) then
          if (ix.gt.7) goto 9312
          if (ix.eq.1) then
            tv = sc(23)
          else if (ix.eq.2) then
            tv = sc(24)
          else if (ix.eq.3) then
            tv = sc(132)
          else if (ix.eq.4) then
            tv = sc(177)
          else if (ix.eq.5) then
            tv = sc(178)
          else if (ix.eq.6) then
            tv = sc(179)
          else if (ix.eq.7) then
            tv = sc(180)
          endif
        else if (ist.eq.UNITSV) then
          if (ix.gt.1) goto 9312
          tv = ifl(264)+1
        else if (ist.eq.MAXANGV) then
          if (ix.gt.1) goto 9312
          tv = sc(80)
        else if (ist.eq.CONTCTV) then
          if (ix.gt.1) goto 9312
          if (lcntct) then
            tv = 1.0d0
          else
            tv = -1.0d0
          endif
        else
          goto 9372
        endif
        goto 999
      endif

      call gtdesc (htv(icx), nclkey, nwds, ietype)
      if (ietype.eq.SURF) nwds = 9
      if (ietype .eq. VSOLID) then
          call nclf_get_solid (nclkey,ival(1),a(2),ival(2))
          a(1) = ival(1)
          nwds = ival(2)
      endif
      if (ix.lt.1.or.ix.gt.nwds) goto 9312
      if (ietype.eq.DATAST) then
   30    call dtgetv (nclkey, ix, tv, ivoc, nwds,label, sub) 
         if (ivoc.eq.2) then
             token2 = label
             ivxsub = sub
             call vstchk
             if ((ityp.eq.2).and.(ist.eq.DATAST)) then                  
                 call vxchk (token2, ivxsub,nclkey,ipg,iel,nwds, ist)
                 goto 30
             endif       
         else if (ivoc.eq.24) then
c
c...text string
c
            tv = 0.0
         endif    
      else
        if (ietype.eq.SURF) then
          call gtprimt(nclkey,ifl(72),ietype,a)
          if (ietype.eq.6) a(7) = a(7) * RADIAN
        else if (ietype .ne. VSOLID) then
          call gtentt(htv(icx),trflg,nclkey,ietype,a)
        endif
        tv=a(ix)
      endif
      goto 999

c                   geometry type expected
9001  ifl(2)=1
      goto 999
c                   number or scalar expected
9007  ifl(2)=7
      goto 999
c                   identifier not previously defined
9009  ifl(2)=9
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   comma expected
9311  ifl(2)=311
      isvinx=inx
      goto 999
c                   index scalar to big for geometry type
9312  ifl(2)=312
      goto 999
c                   invalid geo type for canf
9313  ifl(2)=313
      goto 999
c                   Can't OBTAIN this vocabulary word.
9372  ifl(2)=372
      goto 999

999   icx=icx-1
      return
      end
