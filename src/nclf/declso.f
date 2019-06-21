C*********************************************************************
C*    NAME         :  declso.f
C*       CONTAINS:
C*             declso  boxcnv  showso  prinso
C*
C*    COPYRIGHT 2008 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       declso.f , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*       11/22/17 , 11:21:23
C********************************************************************/
c
c***********************************************************************
c
c   SUBROUTINE:  declso
c
c   FUNCTION:  This routine processes all SOLID definitions.
c
c                   SOLID/BOX
c                         CONE
c                         CYLNDR
c                         EXTRUD
c                         PART
c                         SPHERE
c                         TORUS
c
c                   SOLID/LOAD
c                         STL
c
c                   SOLID/COMPOS
c
c                   SOLID/OUT
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine declso
c
      include 'com8a.com'
      include 'wrksys.com'
c
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold
c
      integer*2 ietype,ierr,nwds,nsf,i,j,iret,isca,parslb,itr,itrflg
      integer*4 nclkey,isub,iftyp,nparms,icalc,ssub,nent,asub,iparam(2)
c
      logical trflg
c
      real*8 param(20),ufct,pbuf(20),origin(3),pt1(3),pt2(3)
c
      byte lsym(64),lfnam(MAX_LEN)
c
      character*64 sym,stok,asym
      character*(MAX_LEN) fnam
c
      equivalence (param,iparam), (param(15),lfnam)
c
      integer*2 ALLV,ATV,BOUNDV,BOXV,CENTERV,CONEV,CYLNDRV,EXTRUDV,
     1          HEIGHTV,INCHESV,LAYERV,LEVELV,LOADV,MMV,MOTIONV,OFFSETV,
     2          PARTV,PROFILV,RADIUSV,REVOLVV,SPHEREV,STLV,TORUSV,
     3          TRFORMV,EDGEV,ONV,OFFV,COMPOSV,CLOSEV,OPENV,INVISV,
     4          RETAINV,REMOVEV,OUTV
      parameter (ALLV=816)
      parameter (ATV=189)
      parameter (BOUNDV=624)
      parameter (BOXV=340)
      parameter (CENTERV=634)
      parameter (CLOSEV=831)
      parameter (COMPOSV=612)
      parameter (CONEV=632)
      parameter (CYLNDRV=620)
      parameter (EDGEV=860)
      parameter (EXTRUDV=633)
      parameter (HEIGHTV=754)
      parameter (INCHESV=303)
      parameter (INVISV=1096)
      parameter (LAYERV=902)
      parameter (LEVELV=759)
      parameter (LOADV=1075)
      parameter (MMV=296)
      parameter (MOTIONV=836)
      parameter (OFFV=72)
      parameter (OFFSETV=666)
      parameter (ONV=71)
      parameter (OPENV=50)
      parameter (OUTV=653)
      parameter (PARTV=260)
      parameter (PROFILV=761)
      parameter (RADIUSV=23)
      parameter (REMOVEV=843)
      parameter (RETAINV=329)
      parameter (REVOLVV=861)
      parameter (SPHEREV=631)
      parameter (STLV=330)
      parameter (TORUSV=627)
      parameter (TRFORMV=110)
c
c...Initialize routine
c
      call curnam (sym,isub)
      nparms = 0
      trflg = .false.
      itrflg = 2
      ufct = 1.d0
      if (ifl(264) .eq. 1) ufct = 1.d0 / 25.4d0
c
c...SOLID/BOX
c
      if (ityp .eq. 1 .and. ist .eq. BOXV) then
          iftyp = 1
          nparms = 12
          call svpars
          call parsit
c
c......SOLID/BOX,CENTER,pt1,l,w,h
c
          if (ityp .eq. 1 .and. ist .eq. CENTERV) then
              call gtpt (itrflg,param(7),ierr)
              if (ierr .ne. 0) go to 9020
              do 100 i=1,3,1
                  call parsit
                  if (.not. scalar) go to 9010
                  param(i+9) = tv * ufct
  100         continue
c
c.........Calculate corner points of box
c
              pt1(1) = param(7) - param(10)/2.
              pt1(2) = param(8) - param(11)/2.
              pt1(3) = param(9) - param(12)/2.
              pt2(1) = param(7) + param(10)/2.
              pt2(2) = param(8) + param(11)/2.
              pt2(3) = param(9) + param(12)/2.
              call boxcnv (pt1,pt2,param)
c
c......SOLID/BOX,BOUND,...
c
          else if (ityp .eq. 1 .and. ist .eq. BOUNDV) then
              nsf = 0
              nparms = 4
              call parsit
c
c.........SOLID/BOX,BOUND,expansion
c
              param(1) = 0.
              param(2) = 0.
              param(3) = 0.
              if (scalar) then
                  param(1) = tv
                  call parsit
                  if (scalar) then
                      param(2) = tv
                      call parsit
                      if (.not. scalar) go to 9010
                      param(3) = tv
                      call parsit
                  else
                      param(2) = param(1)
                      param(3) = param(1)
                  endif
c
c.....Metric Conversion
c
                  if (ifl(264).eq.1) then
                      param(1) = param(1)/25.4d0
                      param(2) = param(2)/25.4d0
                      param(3) = param(3)/25.4d0
                  endif
              endif
c
c.........SOLID/BOX,BOUND,MOTION
c
              if (ityp .eq. 1 .and. ist .eq. MOTIONV) then
                  nsf = -1
c
c.........SOLID/BOX,BOUND,LAYER
c
              else if (ityp .eq. 1 .and. ist .eq. LAYERV) then
                  call parsit
                  if (ityp .ne. 5 .or. ist .ne. 1) go to 9170
                  call parsit
                  if (.not. scalar) go to 9010
                  nclkey = tv
                  call addsky (1,nclkey,0,nsf)
                  call parsit
c
c.........SOLID/BOX,BOUND,ALL
c
              else if (ityp .eq. 1 .and. ist .eq. ALLV) then
                  call addsky (3,nclkey,0,nsf)
                  call parsit
c
c.........SOLID/BOX,BOUND,geo-list
c
              else if (ityp .eq. 2) then
                  do while (ityp .eq. 2 .and. (ist .eq. POINT .or.
     1                      ist .eq. LINE .or. ist .eq. CIRCLE .or.
     2                      ist .eq. CURVE .or. ist .eq. SURF .or.
     3                      ist .eq. PATERN .or. ist .eq. PNTVEC .or.
     4                      ist .eq. VANOTE .or. ist .eq. VSYMBOL .or.
     5                      ist .eq. VSOLID .or. ist .eq. 1))
                      if (ist .eq. 1) then
                          call ub_symbol_name (token2,ivxsub,nclkey,
     1                        origin,i)
                          if (i .eq. 0) go to 9000
                      else
                          call gtentt (tv,trflg,nclkey,ietype,pbuf)
                      endif
                      call addsky (0,nclkey,1,nsf)
                      call parsit
                  enddo
c
c......Geometry expected
c
              else
                  go to 9000
              endif
c
c......No geometry specified
c
              if (nsf .eq. 0) go to 9000
              param(4) = nsf
c
c......SOLID/BOX,1,pt,pt
c
          else
              call rtpars
              call gtpt (itrflg,pt1,ierr)
              if (ierr .ne. 0) go to 9020
              call gtpt (itrflg,pt2,ierr)
              if (ierr .ne. 0) go to 9020
c
c.........SOLID/BOX,1,pt,pt,zmin,zmax
c
              if (nextyp .ne. 11) then
                  call parsit
                  if (.not. scalar) go to 9010
                  pt1(3) = tv * ufct
                  call parsit
                  if (.not. scalar) go to 9010
                  pt2(3) = tv * ufct
              endif
              call boxcnv (pt1,pt2,param)
         endif
c
c...SOLID/CONE
c
      else if (ityp .eq. 1 .and. ist .eq. CONEV) then
          iftyp = 5
          call svpars
          call parsit
c
c......SOLID/CONE,CENTER,pv1,hgt,RADIUS,r1,r2
c
          if (ityp .eq. 1 .and. ist .eq. CENTERV) then
              call gtpv (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9120
              call unitvc (param(4),param(4))
              call parsit
              if (.not. scalar) go to 9010
              param(12) = tv * ufct
              call parsit
              if (ityp .ne. 1 .or. ist .ne. RADIUSV) go to 9110
              call parsit
              if (.not. scalar) go to 9010
              param(7) = tv * ufct
              call parsit
              if (.not. scalar) go to 9010
              param(8) = tv * ufct
              call vctmsc (param(4),param(4),param(12))
c
c......SOLID/CONE,pt,pt,RADIUS,r1,r2
c
          else
              call rtpars
              call gtpt (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9020
              call gtpt (itrflg,param(4),ierr)
              if (ierr .ne. 0) go to 9020
              call parsit
              if (ityp .ne. 1 .or. ist .ne. RADIUSV) go to 9110
              call parsit
              if (.not. scalar) go to 9010
              param(7) = tv * ufct
              call parsit
              if (.not. scalar) go to 9010
              param(8) = tv * ufct
              call vcmnvc (param(4),param(1),param(4))
         endif
         nparms = 8
c
c...SOLID/CYLNDR
c
      else if (ityp .eq. 1 .and. ist .eq. CYLNDRV) then
          iftyp = 2
          call svpars
          call parsit
c
c......SOLID/CYLNDR,CENTER,pv1,hgt,RADIUS,r1
c
          if (ityp .eq. 1 .and. ist .eq. CENTERV) then
              call gtpv (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9120
              call parsit
              call unitvc (param(4),param(4))
              if (.not. scalar) go to 9010
              param(12) = tv * ufct
              call parsit
              if (ityp .ne. 1 .or. ist .ne. RADIUSV) go to 9110
              call parsit
              if (.not. scalar) go to 9010
              param(7) = tv * ufct
              call vctmsc (param(4),param(4),param(12))
c
c......SOLID/CYLNDR,ci,len
c
          else if (ityp .eq. 2 .and. ist .eq. CIRCLE) then
              call gtentt (tv,trflg,nclkey,ietype,pbuf)
              pbuf(1) = pbuf(1) * ufct
              pbuf(2) = pbuf(2) * ufct
              pbuf(3) = pbuf(3) * ufct
              pbuf(7) = pbuf(7) * ufct
              call parsit
              if (.not. scalar) go to 9010
              call vctovc (pbuf(1),param(1))
              call vctmsc (pbuf(4),param(4),tv*ufct)
              param(7) = pbuf(7)
c
c......SOLID/CYLNDR,pt,pt,RADIUS,r1,r2
c
          else
              call rtpars
              call gtpt (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9020
              call gtpt (itrflg,param(4),ierr)
              if (ierr .ne. 0) go to 9020
              call parsit
              if (ityp .ne. 1 .or. ist .ne. RADIUSV) go to 9110
              call parsit
              if (.not. scalar) go to 9010
              param(7) = tv * ufct
              call vcmnvc (param(4),param(1),param(4))
         endif
         nparms = 7
c
c...SOLID/EXTRUD
c
      else if (ityp .eq. 1 .and. ist .eq. EXTRUDV) then
          iftyp = 6
c
c......SOLID/EXTRUD,cv,ve,dis
c
          call parsit
          if (ityp .ne. 2 .or. (ist .ne. CURVE .and. ist .ne. CIRCLE))
     1        go to 9140
          call gtdesc (tv,nclkey,nwds,ietype)
          param(1) = nclkey
          call gtvect (itrflg,param(2),ierr)
          if (ierr .ne. 0) go to 9100
          call unitvc (param(2),param(2))
          call parsit
          if (.not. scalar) go to 9010
          call vctmsc (param(2),param(2),tv*ufct)
          nparms = 4
c
c...SOLID/PART
c
      else if (ityp .eq. 1 .and. ist .eq. PARTV) then
          iftyp = 7
          param(1) = 0
          param(3) = 1
          param(4) = 0.
          param(5) = 0.
          param(6) = 1.
          call conref (param(4),VECTOR)
          param(7) = 0.
          param(8) = 0.
          param(9) = 0.
          param(10) = 1.
          call conref (param(8),VECTOR)
          param(11) = 0.
          param(12) = 0.
          param(13) = 0
          call parsit
c
c......SOLID/PART,exp
c
          if (.not. scalar) go to 9010
          param(2) = tv
c
c......SOLID/PART,LAYER
c
          call parsit
          if (ityp .eq. 1 .and. ist .eq. LAYERV) then
              call parsit
              if (ityp .ne. 5 .or. ist .ne. 1) go to 9170
              call parsit
              if (.not. scalar) go to 9010
              nclkey = tv
              call addsky (1,nclkey,1,nsf)
              call parsit
c
c......SOLID/PART,ALL
c
          else if (ityp .eq. 1 .and. ist .eq. ALLV) then
              call addsky (3,nclkey,1,nsf)
              call parsit
c
c......SOLID/PART,sf-list
c
          else if (ityp .eq. 2 .and. ist .eq. 9) then
              do while (ityp .eq. 2 .and. ist .eq. 9)
                  call gtentt (tv,trflg,nclkey,ietype,pbuf)
                  call addsky (0,nclkey,1,nsf)
                  call parsit
              enddo
c
c......Surface expected
c
          else
              go to 9070
          endif
c
c......No surfaces specified
c
          if (nsf .eq. 0) go to 9070
          param(1) = nsf
c
c......SOLID/PART,AT
c
          if (ityp .eq. 1 .and. ist .eq. ATV) then
              call parsit
c
c.........SOLID/PART,AT,LEVEL,bofs
c
              if (ityp .eq. 1 .and. ist .eq. LEVELV) then
                  param(3) = 1
                  param(11) = 0.
                  param(12) = 0.
                  call parsit
                  if (scalar) then
                      param(11) = tv
                      call parsit
                  endif
c
c.........SOLID/PART,AT,LEVEL,zlev
c
              else if (scalar) then
                  param(3) = 0
                  param(7) = tv
                  call parsit
c
c.........SOLID/PART,AT,LEVEL,plane
c
              else if (ityp .eq. 2 .and. ist .eq. 6) then
                  call gtentt (tv,trflg,nclkey,ietype,pbuf)
                  param(3) = 0
                  param(4) = pbuf(1)
                  param(5) = pbuf(2)
                  param(6) = pbuf(3)
                  param(7) = pbuf(4)
c
                  param(8) = pbuf(1)
                  param(9) = pbuf(2)
                  param(10) = pbuf(3)
c
                  call parsit
              else
                  go to 9080
              endif
c
c.........SOLID/PART,AT,LEVEL,hgt
c
              if (scalar) then
                  param(12) = tv
                  call parsit
              endif
          endif
c
c......SOLID/PART,OFFSET
c
          if (ityp .eq. 1 .and. ist .eq. OFFSETV) then
              call gtvect (itrflg,param(8),ierr)
              if (ierr .ne. 0) go to 9100
              call parsit
          endif
c
c......SOLID/PART,PROFIL-BOX
c
          if (ityp .eq. 1 .and. ist .eq. PROFILV) then
              param(13) = 0
          else if (ityp .eq. 1 .and. ist .eq. BOXV) then
              param(13) = 1
          else if (ityp .ne. 7) then
              go to 9090
          endif
          nparams = 13
c
c...SOLID/REVOLV
c
      else if (ityp .eq. 1 .and. ist .eq. REVOLVV) then
          iftyp = 8
c
c......SOLID/REVOLV,cv,pv
c
          call parsit
          if (ityp .ne. 2 .or. (ist .ne. CURVE .and. ist .ne. LINE .and.
     1        ist .ne. CIRCLE)) go to 9140
          call gtdesc (tv,nclkey,nwds,ietype)
          param(1) = nclkey
          call gtpvln (itrflg,param(2),ierr)
          if (ierr .ne. 0) go to 9100
c
c......SOLID/REVOLV,cv,pv,sang,eang
c
          param(8) = 0.
          param(9) = 360.
          if (nextyp .ne. 11) then
              call parsit
              if (.not. scalar) go to 9010
              param(8) = tv
              call parsit
              if (.not. scalar) go to 9010
              param(9) = tv
          endif
          nparms = 9
c
c...SOLID/SPHERE
c
      else if (ityp .eq. 1 .and. ist .eq. SPHEREV) then
          iftyp = 4
          call parsit
c
c......SOLID/SPHERE,CENTER,pt1,RADIUS,r1
c
          if (ityp .eq. 1 .and. ist .eq. CENTERV) then
              call gtpt (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9120
              call parsit
              if (ityp .ne. 1 .or. ist .ne. RADIUSV) go to 9110
              call parsit
              if (.not. scalar) go to 9010
              param(4) = tv * ufct
c
c......SOLID/SPHERE,ci
c
          else if (ityp .eq. 2 .and. ist .eq. CIRCLE) then
              call gtentt (tv,trflg,nclkey,ietype,param)
              param(1) = param(1) * ufct
              param(2) = param(2) * ufct
              param(3) = param(3) * ufct
              param(7) = param(7) * ufct
              param(4) = param(7)
c
c......Unrecognized command
c
          else
              go to 9060
          endif
          nparms = 4
c
c...SOLID/TORUS
c
      else if (ityp .eq. 1 .and. ist .eq. TORUSV) then
          iftyp = 3
          call parsit
c
c......SOLID/TORUS,CENTER,pv1,RADIUS,r1,r2
c
          if (ityp .eq. 1 .and. ist .eq. CENTERV) then
              call gtpv (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9120
              call parsit
              if (ityp .ne. 1 .or. ist .ne. RADIUSV) go to 9110
              call parsit
              if (.not. scalar) go to 9010
              param(7) = tv * ufct
              call parsit
              if (.not. scalar) go to 9010
              param(8) = tv * ufct
c
c......SOLID/TORUS,ci
c
          else if (ityp .eq. 2 .and. ist .eq. CIRCLE) then
              call gtentt (tv,trflg,nclkey,ietype,param)
              param(1) = param(1) * ufct
              param(2) = param(2) * ufct
              param(3) = param(3) * ufct
              param(7) = param(7) * ufct
              call parsit
c
c.........SOLID/TORUS,ci,RADIUS,r1
c
              if (ityp .eq. 1 .and. ist .eq. RADIUSV) then
                  call parsit
                  param(8) = tv * ufct
c
c.........SOLID/TORUS,ci1,ci2
c
              else if (ityp .eq. 2 .and. ist .eq. CIRCLE) then
                  call gtentt (tv,trflg,nclkey,ietype,pbuf)
                  pbuf(7) = pbuf(7) * ufct
                  param(8) = dabs(pbuf(7)-param(7)) / 2.
                  if (param(7) .lt. pbuf(7)) then
                      param(7) = param(7) + param(8)
                  else
                      param(7) = pbuf(7) + param(8)
                  endif
c
c.........Unrecognized command
c
              else
                  go to 9110
              endif
          else
              go to 9060
          endif
          nparms = 8
c
c...SOLID/STL
c
      else if (ityp .eq. 1 .and. ist .eq. STLV) then
          iftyp = 9
c
c......SOLID/STL,[units]
c
          iparam(1) = ifl(264)
          iparam(2) = 0
          iparam(3) = 0
          iparam(4) = 1
          do 500 i=3,14,1
              param(i) = 0.
  500     continue
          param(3) = 1.
          param(7) = 1.
          param(11) = 1.
  550     call svpars
          call parsit
          if (ityp .eq. 1 .and. ist .eq. INCHESV) then
              iparam(1) = 0
              go to 550
          else if (ityp .eq. 1 .and. ist .eq. MMV) then
              iparam(1) = 1
              go to 550
          else if (ityp .eq. 1 .and. ist .eq. EDGEV) then
              call parsit
              if (ityp .eq. 1 .and. ist .eq. ONV) then
                  iparam(4) = 1
              else if (ityp .eq. 1 .and. ist .eq. OFFV) then
                  iparam(4) = 0
              else
                  go to 9200
              endif
              go to 550
          else
              call rtpars
          endif
c
c......SOLID/STL,file
c
          ldtext = .true.
          call parsit
          ldtext = .false.
          if (.not. lstrng) go to 9130
          j = 0
          fnam = ' '
          call gttext (fnam,j)
          call ctob (fnam,lfnam)
cc          iparam(4) = j
          nparms = 14 + (j/8+1)
c
c...SOLID/COMPOS
c
      else if (ityp .eq. 1 .and. ist .eq. COMPOSV) then
          iftyp = 10
          call parsit
c
c......SOLID/COMPOS,OPEN-CLOSE
c
          param(1) = 1
          if (ityp .eq. 1 .and. ist .eq. OPENV) then
              param(1) = 0
              call parsit
          else if (ityp .eq. 1 .and. ist .eq. CLOSEV) then
              param(1) = 1
              call parsit
          endif
c
c......SOLID/COMPOS,INVIS-RETAIN-REMOVE
c
          param(2) = 0
          if (ityp .eq. 1 .and. ist .eq. INVISV) then
              param(2) = 0
              call parsit
          else if (ityp .eq. 1 .and. ist .eq. RETAINV) then
              param(2) = 1
              call parsit
          else if (ityp .eq. 1 .and. ist .eq. REMOVEV) then
              param(2) = 2
              call parsit
          endif
c
c.........Get surface list
c
          call sflist (2,1,nsf,ierr)
          if (ierr .ne. 0) go to 9210
          param(3) = nsf
          nparms = 3
c
c...SOLID/LOAD,solid
c
      else if (ityp .eq. 1 .and. ist .eq. LOADV) then
          ldtext = .true.
          call parsit
          ldtext = .false.
          if (.not. lstrng) go to 9130
          j = 0
          fnam = ' '
          call gttext (fnam,j)
          asym = sym
          asub = isub
c
c...when isub=0, it mean no sub number
c...should not change isub. yurong 
c
c...      if (isub .lt. 1) then
          if (isub .lt. 0) then
              iret = parslb(sym,isub)
c
c...there is no reason there is '-' there
c
c...              isub = -iret
              isub = iret
          endif
c
c......SOLID/LOAD,solid,scalar
c
          isca = 0
          call parsit
          if (ityp .eq. 2 .and. (ist .eq. 1 .or. ist .eq. 2)) then
              stok = token2
              ssub = ivxsub
              isca = 1
              call parsit
          endif
c
c......SOLID/LOAD,solid,TRFORM,mx
c
          itr = 0
          if (ityp .eq. 1 .and. ist .eq. TRFORMV) then
              call parsit
              if (ityp .ne. 2 .or. ist .ne. MATRIX) go to 9160
              call gtentt (tv,trflag,nclkey,ietype,pbuf)
              itr = 1
          else if (ityp .ne. 7) then
              go to 9150
          endif
c
c......Define solid(s)
c
          call ctob (sym,lsym)
          call ctob (fnam,lfnam)
          call nclf_load_solid (lfnam,lsym,isub,nent,itr,pbuf,ierr)
          if (ierr .eq. 2) go to 9180
          if (ierr .ne. 0) go to 9030
c
c......Store number of entities scalar
c
          if (isca .eq. 1) then
              idst = 2
              savid2 = stok
              isvsub = ssub
              rest = nent
              call vstore
          endif
c
c......Restore assignment label
c
          sym = asym
          isub = asub
c
c......Disable driver calls of geognx, vstore, & dspent
c
          defwf = .true.
          ifl(262) = 1
          ifl(329) = 1
          go to 8000
c
c...SOLID/OUT
c
      else if (ityp .eq. 1 .and. ist .eq. OUTV) then
          call parsit
          if (ityp .eq. 2 .and. ist .eq. VSOLID) then
              call gtdesc (tv,nclkey,nwds,itype)
              call nclf_get_solid (nclkey,istyp,params,nparams)
              if (istyp .ne. 10) go to 9220
              sc(11) = tv
              asn    = tv
              isc10(1) = 607
              isc10(2) = 15
              idst   = 9
              call prsout (nclkey,2)
              go to 8200
          else
              go to 9220
          endif
c
c...Unrecognized word
c
      else
          go to 9000
      endif
c
c...Define Solid
c
 1000 call ctob (sym,lsym)
      icalc = 1
      call nclf_define_solid (iftyp,lsym,isub,param,nparms,icalc,nclkey,
     1                        ierr)
      if (ierr .eq. 2) go to 9180
      if (ierr .ne. 0) go to 9030
c
c...Set visible flag
c
      if (.not. dsplso) call blkgeo(nclkey,1)
      idst   = VSOLID
      call ptdesc (nclkey,idst,rest)
c
c...End of routine
c
 8000 savid2 = sym
      ivxsub = isub
      ifl(262) = 1
 8100 call delsky
      call delskw
 8200 return
c
c...Stock type expected
c
 9000 ifl(2) = 1
      err = .true.
      go to 8100
c
c...Number expected
c
 9010 ifl(2) = 7
      err = .true.
      go to 8100
c
c...Point expected
c
 9020 ifl(2) = ierr
      err = .true.
      go to 8100
c
c...Could not create solid
c
 9030 ifl(2) = 163
      err = .true.
      go to 8100
c
c...Input value out of range
c
 9050 ifl(2) = 445
      err = .true.
      go to 8100
c
c...Circle expected
c
 9060 ifl(2) = 159
      err = .true.
      go to 8100
c
c...Surface expected
c
 9070 ifl(2) = 186
      err = .true.
      go to 8100
c
c...Plane expected
c
 9080 ifl(2) = 19
      err = .true.
      go to 8100
c
c...End of command expected
c
 9090 ifl(2) = 4
      err = .true.
      go to 8100
c
c...Vector expected
c
 9100 ifl(2) = 11
      err = .true.
      go to 8100
c
c...RADIUS expected
c
 9110 ifl(2) = 197
      err = .true.
      go to 8100
c
c...Point-Vector expected
c
 9120 ifl(2) = 526
      err = .true.
      go to 8100
c
c...Text string (filename) expected
c
 9130 ifl(2) = 513
      err = .true.
      go to 8100
c
c...Curve expected
c
 9140 ifl(2) = 21
      err = .true.
      go to 8100
c
c...Scalar expected
c
 9150 ifl(2) = 282
      err = .true.
      go to 8100
c
c...Matrix expected
c
 9160 ifl(2) = 94
      err = .true.
      go to 8100
c
c...'=' expected
c
 9170 ifl(2) = 6
      err = .true.
      go to 8100
c
c...Could not open file
c
 9180 ifl(2) = 145
      err = .true.
      go to 8100
c
c...ON or OFF expected
c
 9200 ifl(2) = 56
      err = .true.
      go to 8100
c
c...Error supplied by called routine
c
 9210 ifl(2) = ierr
      err = .true.
      go to 8100
c
c...Composite SOLID expected
c
 9220 ifl(2) = 553
      err = .true.
      go to 8100
      end
c
c***********************************************************************
c
c   SUBROUTINE:  boxcnv (gpt1,gpt2,gparm)
c
c   FUNCTION:  This routine converts the lower left and upper right
c              coordinates of a box to a standard XYZ box containing
c              its lower left and XYZ length vectors.
c
c   INPUT:  gpt1     R*8   D3  -  Lower left of box.
c           gpt2     R*8   D3  -  Upper right of box.
c
c   OUTPUT: gparm    R*8   D12 -  1:3 = Lower Left, 4:6 = X-axis vector,
c                                 7:9 = Y-axis vector, 10:12 = Z-axis
c                                 vector.  The vectors contain the length
c                                 of each side.
c
c***********************************************************************
c
      subroutine boxcnv (gpt1,gpt2,gparm)
c
      include 'com8a.com'
c
      real*8 gpt1(3),gpt2(3),gparm(12)
c
c...Store lower left of box
c
      gparm(1) = gpt1(1)
      gparm(2) = gpt1(2)
      gparm(3) = gpt1(3)
c
c...X-axis
c
      gparm(4) = gpt2(1) - gpt1(1)
      gparm(5) = 0.
      gparm(6) = 0.
c
c...X-axis
c
      gparm(7) = 0.
      gparm(8) = gpt2(2) - gpt1(2)
      gparm(9) = 0.
c
c...Z-axis
c
      gparm(10) = 0.
      gparm(11) = 0.
      gparm(12) = gpt2(3) - gpt1(3)
c
c...End of routien
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  showso (knl)
c
c   FUNCTION:  This routine displays the canonical data of a visual solid.
c
c   INPUT:  none.
c
c   OUTPUT: knl      I*2   D1  -  Last line number written to.
c
c***********************************************************************
c
      subroutine showso (knl)
c
      include 'com8a.com'
c
      integer*4 knl
c
      integer*2 nwds,itype
      integer*4 nclkey,np,styp,iary(4),nc1,nc2
c
      real*8 parms(128)
c
      character*60 ofile
      character*(MAX_PATH) lfile
      byte bfile(MAX_PATH)
c
      equivalence (parms,iary,bfile)
c
c...Get key of solid
c
      call gtdesc (tv,nclkey,nwds,itype)
c
c...Get solid canonical data
c
      call nclf_get_solid (nclkey,styp,parms,np)
c
c...Output solid data
c
      knl = 17
   10 format (23x,'x',18x,'y',18x,'z')
   11 format ('lower left:   ',3(f18.7,1x))
   12 format ('upper right:  ',3(f18.7,1x))
   13 format ('center:       ',3(f18.7,1x))
   14 format ('axis:         ',3(f18.7,1x))
   15 format ('height:       ',f18.7)
   16 format ('radius:       ',f18.7)
   17 format ('axial radius: ',f18.7)
   18 format ('circle radius:',f18.7)
   19 format ('lower radius: ',f18.7)
   20 format ('upper radius: ',f18.7)
   21 format ('direction:    ',3(f18.7,1x))
   22 format ('length:       ',f18.7)
   23 format ('expansion:    ',f18.7)
   24 format ('curve points: ',i4)
   25 format ('first point:  ',3(f18.7,1x))
   26 format ('start angle:  ',f18.7)
   27 format ('end angle:    ',f18.7)
   28 format ('length vector:',3(f18.7,1x))
   29 format ('width vector: ',3(f18.7,1x))
   30 format ('height vector:',3(f18.7,1x))
   31 format ('units: inches')
   32 format ('units: millimeters')
   33 format ('format: ascii')
   34 format ('format: binary')
   35 format ('file: ',a)
   36 format ('# of surfaces: ',i4)
   37 format ('# of solids:   ',i4)
c
c......Box
c
		if (styp .eq. 1) then
          cout = 'Box'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,11) parms(1),parms(2),parms(3)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,12) parms(4),parms(5),parms(6)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,28) parms(7),parms(8),parms(9)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,29) parms(10),parms(11),parms(12)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,30) parms(13),parms(14),parms(15)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......Cylinder
c
		else if (styp .eq. 2) then
          cout = 'Cylinder'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,13) parms(1),parms(2),parms(3)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,14) parms(4),parms(5),parms(6)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,15) parms(7)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,16) parms(8)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......Torus
c
		else if (styp .eq. 3) then
          cout = 'Torus'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,13) parms(1),parms(2),parms(3)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,14) parms(4),parms(5),parms(6)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,17) parms(7)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,18) parms(8)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......Sphere
c
		else if (styp .eq. 4) then
          cout = 'Sphere'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,13) parms(1),parms(2),parms(3)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,16) parms(4)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......Cone
c
		else if (styp .eq. 5) then
          cout = 'Cone'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,13) parms(1),parms(2),parms(3)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,14) parms(4),parms(5),parms(6)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,15) parms(7)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,19) parms(8)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,20) parms(9)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......Extrusion
c
		else if (styp .eq. 6) then
          cout = 'Extruded'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,21) parms(1),parms(2),parms(3)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,22) parms(4)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          np = parms(5)
          write (cout,24) np
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,25) parms(6),parms(7),parms(8)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......Contour
c
		else if (styp .eq. 7) then
          cout = 'Contour'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,23) parms(1)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          if (parms(1) .eq. 1) then
              call putmsg ('Box',80,knl,0)
          else
              call putmsg ('Profile',80,knl,0)
          endif
          knl = knl + 1
          write (cout,21) parms(3),parms(4),parms(5)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,22) parms(6)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          np = parms(7)
          write (cout,24) np
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,25) parms(8),parms(9),parms(10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......Revolved
c
		else if (styp .eq. 8) then
          cout = 'Revolved'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,10)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,13) parms(1),parms(2),parms(3)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,14) parms(4),parms(5),parms(6)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,26) parms(7)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,27) parms(8)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          np = parms(9)
          write (cout,24) np
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          write (cout,25) parms(10),parms(11),parms(12)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......STL
c
		else if (styp .eq. 9) then
          cout = 'STL'
          call putmsg (cout,80,knl,0)
          knl = knl + 1

          if (iary(1) .eq. 0) then
              write (cout,31)
          else
              write (cout,32)
          endif
          call putmsg (cout,80,knl,0)
          knl = knl + 1

          if (iary(2) .eq. 0) then
              write (cout,33)
          else
              write (cout,34)
          endif
          call putmsg (cout,80,knl,0)
          knl = knl + 1

          call btoc (bfile(113),lfile,nc1)
          nc1 = 60
          call shortname (lfile,ofile,nc1,nc2)
          write (cout,35) ofile(1:nc2)
          call putmsg (cout,80,knl,0)
          knl = knl + 1
c
c......Composite
c
		else if (styp .eq. 10) then
          cout = 'Composite'
          call putmsg (cout,80,knl,0)
          knl = knl + 1
          if (parms(1) .ne. 0) then
              i = parms(1)
              write (cout,36) i
              call putmsg (cout,80,knl,0)
              knl = knl + 1
          endif
          if (parms(2) .ne. 0) then
              i = parms(2)
              write (cout,37) i
              call putmsg (cout,80,knl,0)
              knl = knl + 1
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prinso (kkey,labstr,nc,cref)
c
c   FUNCTION:  This routine prints the canonical data of a visual solid.
c
c   INPUT:  kkey     I*4   D1  -  Key of solid entity.
c
c           labstr     C*n   D1  -  Label of solid. include subscript string
c
c           nc     I*4   D1  -  length of labstr.
c
c           cref     C*n   D1  -  Refsys string.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine prinso (kkey,labstr,nc,cref)
c
      include 'com8a.com'
c
      integer*4 kkey
c
      character*(*) labstr,cref
c
      integer*4 np,np1,styp,iary(4),nc,strlen1,nc1,nc2
c
      real*8 parms(128)
      character*11 numstr(128)
c
      character*9 lsol(10)
      character*60 ofile
      character*(MAX_PATH) lfile
      byte bfile(MAX_PATH)
c
      equivalence (parms,iary,bfile)
c
      data lsol /'box','cylinder','torus','sphere','cone','extrusion',
     1           'contour','revolved','STL','composite'/
c
c...Get solid canonical data
c
      call nclf_get_solid (kkey,styp,parms,np)
      if (styp .ne. 9 .and. styp .ne. 10) then
          np1 = np
          if (np1 .gt. 4) np1 = 4
          do 1111 k=1, np1
              call ul_format_data11(parms(k), numstr(k))
1111      continue
      endif
c
c...STL solid
c
      if (styp .eq. 9) then
          call btoc (bfile(113),lfile,nc1)
          nc1 = 60
          call shortname (lfile,ofile,nc1,nc2)
          if (nc.le.14) then
              write (cout,10) labstr(1:14), 
     1             lsol(styp), cref, ofile(1:nc2)
          else
              call prtge1 (labstr)          
              write (cout,11) lsol(styp),cref,ofile(1:nc2)
          endif
   10     format (a14,1x,a9,2x,a5,1x,a)
   11     format (15x,a9,2x,a5,1x,a)
          call prtge1 (cout)
c
c...Composite solid
c
      else if (styp .eq. 10) then
          nc1    = parms(1)
          nc2    = parms(2)
          write (cout,15) labstr(1:14),lsol(styp),nc1,nc2
   15     format (a14,1x,a9,1x,i4,' surfaces   ',i4,' solids')
          call prtge1 (cout)
c
c...All other solids
c
      else
          if (nc.le.14) then
              write (cout,20) labstr(1:14),lsol(styp),cref,
     1                    (numstr(i),i=1,np1)
          else
              call prtge1 (labstr)          
              write (cout,21) lsol(styp),cref,
     1                    (numstr(i),i=1,np1)
          endif
   20     format (a14,1x,a9,2x,a5,1x,a11,1x,a11,1x,a11,1x,a11)
   21     format (15x,a9,2x,a5,1x,a11,1x,a11,1x,a11,1x,a11)
          call prtge1 (cout)
c
c......Print out rest of data
c
          if (np .gt.4) then
              do 1112 k=5, np
                 call ul_format_data11(parms(k), numstr(k))
1112          continue
              do 100 i=5,np,4
                  nc = i + 3
                  if (np-i+1 .lt. 4) nc = np
                  write (cout,30) (numstr(j),j=i,nc)
   30             format (32x,a11,1x,a11,1x,a11,1x,a11)
                  call prtge1 (cout)
  100         continue
          endif
      endif
c
c...End of routine
c
 8000 return
      end
