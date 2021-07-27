C*********************************************************************
C*    NAME         :  stock.f
C*       CONTAINS:
C*             crestk  stksol  prsstk  stkout
C*
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       stock.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:45
C********************************************************************/
c
c***********************************************************************
c
c   SUBROUTINE:  crestk (kwhch)
c
c   FUNCTION:  This routine defines/manipulates an NCLIPV stock/fixture using
c              the following command syntacies.
c
c                 STOCK /BOX   ,num-id,params
c                 FIXTUR CONE
c                        CYLNDR
c                        SPHERE
c                        TORUS
c                        LOAD
c                        STL
c                        CLONE
c                        SOLID
c
c                 STOCK /MOVE,num-id1,...,num-idm,THRU,num-idn,AT,mx
c                 FIXTUR
c
c                 STOCK /REMOVE,num-id1,...,num-idm,THRU,num-idn
c                 FIXTUR
c
c                 STOCK /REMOVE,CHIPS,pv1,...,pvn
c
c                 STOCK /MODIFY,num-id...,COLOR=col,VISIBL=n,TOLER=tol, $
c                 FIXTUR        TRANS=tr,ACTIVE=n
c
c   INPUT:  kwhch    I*2    D1   321 = STOCK, 898 = FIXTUR.
c
c   OUTPUT: none.
c
c***********************************************************************
c

      subroutine crestk (kwhch)
c
      include 'com8a.com'
c
      integer*2 ietype,ierr,isav,ifl44,ipos,icltyp,inc,nc2,isca,
     1          icldat(320),itrflg,j
      integer*4 nclkey,numid,ifxt,iftyp,kerr,iunit,idns(1000),
     1          nidns,icol,ivis,itrans,iact,nc,strlen1,jcldat(160),
     2          idsav,nci,nindex,ssub,ncp,indx,isub,incr,xfl
c
      logical trflg
c
      real*8 param(12),pbuf(12),wid,len,hgt,tol,rcldat(640),ufct,cnv
c
      character*(MAX_LEN) temp,fnam
      character*64 stok
      character*640 lcldat,ptxt
      character*256 comstr
c
      equivalence (rcldat,jcldat,icldat,lcldat)
c
      integer*2 STOCKV,FIXTV,ATV,BOXV,CYLV,LOADV,STLV,INCHV,MMV,CLONEV,
     1          MOVEV,REMOVV,MODV,CHIPV,CONEV,SPHERV,TORUSV,SOLIDV,
     2          INCRV
c
      parameter (ATV=189)
      parameter (BOXV=340)
      parameter (CHIPV=331)
      parameter (CLONEV=576)
      parameter (CONEV=632)
      parameter (CYLV=620)
      parameter (FIXTV=898)
      parameter (INCHV=303)
      parameter (INCRV=66)
      parameter (LOADV=1075)
      parameter (MMV=296)
      parameter (MODV=732)
      parameter (MOVEV=577)
      parameter (REMOVV=843)
      parameter (SOLIDV=123)
      parameter (SPHERV=631)
      parameter (STLV=330)
      parameter (STOCKV=321)
      parameter (TORUSV=627)
c
      integer*2 COLV,VISV,TOLERV,TRANSV,ACTV
c
      integer*2 BLACKV,WHITEV,BLUEV,REDV,GREENV,MGNTAV,YELLOV,CYANV,
     1          BROWNV,TANV,LTBLUV,SEAGRV,ORANGV,PINKV,PURPLV,GREYV,
     2          ONV,OFFV
c
      parameter (COLV=900)
      parameter (VISV=1095)
      parameter (TOLERV=731)
      parameter (TRANSV=1037)
      parameter (ACTV=341)
c
      parameter (BLACKV=904)
      parameter (WHITEV=905)
      parameter (BLUEV =906)
      parameter (REDV  =907)
      parameter (GREENV=908)
      parameter (MGNTAV=909)
      parameter (YELLOV=910)
      parameter (CYANV =911)
      parameter (BROWNV=912)
      parameter (TANV  =913)
      parameter (LTBLUV=914)
      parameter (SEAGRV=915)
      parameter (ORANGV=1101)
      parameter (PINKV=1102)
      parameter (PURPLV=1103)
      parameter (GREYV =1104)
c
      parameter (ONV=71)
      parameter (OFFV=72)
c
c...Initialize routine
c
      ifl44 = ifl(44)
      ifl(44) = 9
      if (IST .eq. FIXTV) then
          ifxt = 1
          ptxt = 'IPV FIXTUR'
          ncp  = 10
      else
          ifxt = 0
          ptxt = 'IPV STOCK'
          ncp  = 9
      endif
cc      icltyp = 2600 + ifxt
      trflg = .true.
      itrflg = 1
      kerr = 0
      ufct = 1.d0
      if (ifl(264).eq.1) ufct = 1.d0 / 25.4d0
      cnv = 1.d0 / ufct
c
c...STOCK/type
c
      call parsit
      if (ITYP .ne. 1) go to 9000
      isav = IST
      call parsit
      if (.not. scalar .and. (isav .ne. REMOVV .or. ITYP .ne. 1 .or.
     1    IST .ne. CHIPV)) go to 9100
      if (scalar) then
          numid = TV
          if (numid .le. 0.) go to 9400
      endif
c
c...STOCK/BOX
c
      if (isav .eq. BOXV) then
          call parsit
          if (.not. scalar) go to 9100
          iftyp = ITV
c
c......STOCK/BOX,pt,pt
c
          if (ITV .eq. 1) then
              call gtpt (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9200
              call gtpt (itrflg,param(4),ierr)
              if (ierr .ne. 0) go to 9200
c
c.........STOCK/BOX,pt,pt,minz,maxz
c
              if (nextyp .ne. 11) then
                  call parsit
                  if (.not. scalar) go to 9100
                  param(3) = TV * ufct
                  call parsit
                  if (.not. scalar) go to 9100
                  param(6) = TV * ufct
              endif
c
c......STOCK/BOX,2,pt,wid,len,hgt
c
          else if (ITV .eq. 2) then
              call gtpt (itrflg,param,ierr)
              if (ierr .ne. 0) go to 9200
              call parsit
              if (.not. scalar) go to 9100
              wid = TV * ufct
              if (wid .le. .001) go to 9500
              call parsit
              if (.not. scalar) go to 9100
              len = TV * ufct
              if (len .le. .001) go to 9500
              call parsit
              if (.not. scalar) go to 9100
              hgt = TV * ufct
              if (hgt .le. .001) go to 9500
              param(1) = param(1) - wid/2.
              param(2) = param(2) - len/2.
              param(3) = param(3) - hgt/2.
              param(4) = param(1) + wid
              param(5) = param(2) + len
              param(6) = param(3) + hgt
c
c......STOCK/BOX,3,expand
c
cc          else if (ITV .eq. 3) then
cc              if (nextyp .eq. 11) then
cc                  param(1) = 0.
cc              else
cc                  call parsit
cc                  if (.not. scalar) go to 9100
cc                  param(1) = TV
cc              endif
c
c......Unrecognized stock type
c
          else
              go to 9500
          endif
c
c......Create the stock
c
          idsav = numid
          if (ifl(35) .eq. 0) then
              call ulf_verify_box(ifxt,iftyp,param,numid,kerr)
              if (kerr .ne. 0) go to 9300
          endif
c
c......Create clfile record
c
          ptxt = ptxt(1:ncp) // ' BOX'
          ncp  = ncp + 4
          call vctmsc (param,param,cnv)
          call vctmsc (param(4),param(4),cnv)
          call stkout (ptxt,ncp,idsav,param,6,idns,0,temp,0)
cc          jcldat(1) = iftyp
cc          jcldat(2) = idsav
cc          do 110 i=1,6,1
cc              rcldat(i+1) = param(i)
cc  110     continue
cc          call putcl (icltyp,BOXV,8,rcldat)
c
c...STOCK/CYLNDR
c
      else if (isav .eq. CYLV) then
          call parsit
          if (.not. scalar) go to 9100
          iftyp = ITV
c
c......STOCK/CYLNDR,1,cir,len
c
          if (ITV .eq. 1) then
              call parsit
              if (ityp .ne. 2 .or. ist .ne. CIRCLE) go to 9600
              call gtentt (TV,trflg,nclkey,ietype,param)
              param(1) = param(1) * ufct
              param(2) = param(2) * ufct
              param(3) = param(3) * ufct
              param(7) = param(7) * ufct
              call parsit
              if (.not. scalar) go to 9100
              param(8) = TV * ufct
cc              if (param(8) .le. .001) go to 9500
c
c......STOCK/CYLNDR,2,pt,pt,rad
c
          else if (ITV .eq. 2) then
              call gtpt (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9200
              call gtpt (itrflg,param(4),ierr)
              if (ierr .ne. 0) go to 9200
              call parsit
              if (.not. scalar) go to 9100
              param(7) = TV * ufct
              if (param(7) .le. .001) go to 9500
              call vcmnvc (param(4),param(1),param(4))
              param(8) = f_mag(param(4))
              call unitvc (param(4),param(4))
c
c......STOCK/CYLNDR,3,pv,hgt,rad
c
          else if (ITV .eq. 3) then
              call gtpv (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9200
              call parsit
              if (.not. scalar) go to 9100
              param(8) = TV * ufct
              call parsit
              if (.not. scalar) go to 9100
              param(7) = TV * ufct
              if (param(7) .le. .001) go to 9500
              call unitvc (param(4),param(4))
c
c......Unrecognized stock type
c
          else
              go to 9500
          endif
c
c......Create the stock
c
          idsav = numid
          if (ifl(35) .eq. 0) then
              call ulf_verify_cyl (ifxt,param,numid,kerr)
              if (kerr .ne. 0) go to 9300
          endif
c
c......Create clfile record
c
          ptxt = ptxt(1:ncp) // ' CYLNDR'
          ncp  = ncp + 7
          call vctmsc (param,param,cnv)
          param(7) = param(7) * cnv
          param(8) = param(8) * cnv
          call stkout (ptxt,ncp,idsav,param,8,idns,0,temp,0)
cc          jcldat(1) = iftyp
cc          jcldat(2) = idsav
cc          do 210 i=1,8,1
cc              rcldat(i+1) = param(i)
cc  210     continue
cc          call putcl (icltyp,CYLV,10,rcldat)
c
c...STOCK/CONE
c
      else if (isav .eq. CONEV) then
          call parsit
          if (.not. scalar) go to 9100
          iftyp = ITV
c
c......STOCK/CONE,2,pt,pt,rad1,rad2
c
          if (ITV .eq. 2) then
              call gtpt (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9200
              call gtpt (itrflg,param(4),ierr)
              if (ierr .ne. 0) go to 9200
              call parsit
              if (.not. scalar) go to 9100
              param(7) = TV * ufct
              if (param(7) .le. .001) go to 9500
              call parsit
              if (.not. scalar) go to 9100
              param(8) = TV * ufct
              if (param(8) .le. .001) go to 9500
              call vcmnvc (param(4),param(1),param(4))
              param(9) = f_mag(param(4))
              call unitvc (param(4),param(4))
c
c......STOCK/CONE,3,pv,hgt,rad1,rad2
c
          else if (ITV .eq. 3) then
              call gtpv (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9200
              call parsit
              if (.not. scalar) go to 9100
              param(9) = TV * ufct
              call parsit
              if (.not. scalar) go to 9100
              param(7) = TV * ufct
              if (param(7) .le. .001) go to 9500
              call parsit
              if (.not. scalar) go to 9100
              param(8) = TV * ufct
              if (param(8) .le. .001) go to 9500
              call unitvc (param(4),param(4))
c
c......Unrecognized stock type
c
          else
              go to 9500
          endif
c
c......Create the stock
c
          idsav = numid
          if (ifl(35) .eq. 0) then
              call ulf_verify_cone (ifxt,param,numid,kerr)
              if (kerr .ne. 0) go to 9300
          endif
c
c......Create clfile record
c
          ptxt = ptxt(1:ncp) // ' CONE'
          ncp  = ncp + 5
          call vctmsc (param,param,cnv)
          call vctmsc (param(7),param(7),cnv)
          call stkout (ptxt,ncp,idsav,param,9,idns,0,temp,0)
cc          jcldat(1) = iftyp
cc          jcldat(2) = idsav
cc          do 260 i=1,9,1
cc              rcldat(i+1) = param(i)
cc  260     continue
cc          call putcl (icltyp,CONEV,11,rcldat)
c
c...STOCK/SPHERE
c
      else if (isav .eq. SPHERV) then
          call parsit
          if (.not. scalar) go to 9100
          iftyp = ITV
c
c......STOCK/SPHERE,1,cir
c
          if (ITV .eq. 1) then
              call parsit
              if (ityp .ne. 2 .or. ist .ne. CIRCLE) go to 9600
              call gtentt (TV,trflg,nclkey,ietype,param)
              param(1) = param(1) * ufct
              param(2) = param(2) * ufct
              param(3) = param(3) * ufct
              param(4) = param(7) * ufct
c
c......STOCK/SPHERE,2,pt,rad
c
          else if (ITV .eq. 2) then
              call gtpt (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9200
              call parsit
              if (.not. scalar) go to 9100
              param(4) = TV * ufct
              if (param(4) .le. .001) go to 9500
c
c......Unrecognized stock type
c
          else
              go to 9500
          endif
c
c......Create the stock
c
          idsav = numid
          if (ifl(35) .eq. 0) then
              call ulf_verify_sphere (ifxt,param,numid,kerr)
              if (kerr .ne. 0) go to 9300
          endif
c
c......Create clfile record
c
          ptxt = ptxt(1:ncp) // ' SPHERE'
          ncp  = ncp + 7
          call vctmsc (param,param,cnv)
          param(4) = param(4) * cnv
          call stkout (ptxt,ncp,idsav,param,4,idns,0,temp,0)
cc          jcldat(1) = iftyp
cc          jcldat(2) = idsav
cc          do 310 i=1,4,1
cc              rcldat(i+1) = param(i)
cc  310     continue
cc          call putcl (icltyp,SPHERV,6,rcldat)
c
c...STOCK/TORUS
c
      else if (isav .eq. TORUSV) then
          call parsit
          if (.not. scalar) go to 9100
          iftyp = ITV
c
c......STOCK/TORUS,1,cir,cir
c
          if (ITV .eq. 1) then
              call parsit
              if (ityp .ne. 2 .or. ist .ne. CIRCLE) go to 9600
              call gtentt (TV,trflg,nclkey,ietype,param)
              param(1) = param(1) * ufct
              param(2) = param(2) * ufct
              param(3) = param(3) * ufct
              param(7) = param(7) * ufct
              call parsit
              if (ityp .ne. 2 .or. ist .ne. CIRCLE) go to 9600
              call gtentt (TV,trflg,nclkey,ietype,pbuf)
              pbuf(7) = pbuf(7) * ufct
              param(8) = dabs(pbuf(7)-param(7)) / 2.
              if (param(7) .lt. pbuf(7)) then
                  param(7) = param(7) + param(8)
              else
                  param(7) = pbuf(7) + param(8)
              endif
c
c......STOCK/TORUS,2,ci,rad
c
          else if (ITV .eq. 2) then
              call parsit
              if (ityp .ne. 2 .or. ist .ne. CIRCLE) go to 9600
              call gtentt (TV,trflg,nclkey,ietype,param)
              param(1) = param(1) * ufct
              param(2) = param(2) * ufct
              param(3) = param(3) * ufct
              param(7) = param(7) * ufct
              call parsit
              if (.not. scalar) go to 9100
              param(8) = TV * ufct
              if (param(8) .le. .001 .or. param(7) .le. param(8))
     1            go to 9500
c
c......STOCK/TORUS,3,pv,rad1,rad2
c
          else if (ITV .eq. 3) then
              call gtpv (itrflg,param(1),ierr)
              if (ierr .ne. 0) go to 9200
              call parsit
              if (.not. scalar) go to 9100
              param(7) = TV * ufct
              if (param(7) .le. .001) go to 9500
              call parsit
              if (.not. scalar) go to 9100
              param(8) = TV * ufct
              if (param(8) .le. .001) go to 9500
              call unitvc (param(4),param(4))
c
c......Unrecognized stock type
c
          else
              go to 9500
          endif
c
c......Create the stock
c
          idsav = numid
          if (ifl(35) .eq. 0) then
              call ulf_verify_torus (ifxt,param,numid,kerr)
              if (kerr .ne. 0) go to 9300
          endif
c
c......Create clfile record
c
          ptxt = ptxt(1:ncp) // ' TORUS'
          ncp  = ncp + 6
          call vctmsc (param,param,cnv)
          param(7) = param(7) * cnv
          param(8) = param(8) * cnv
          call stkout (ptxt,ncp,idsav,param,8,idns,0,temp,0)
cc          jcldat(1) = iftyp
cc          jcldat(2) = idsav
cc          do 360 i=1,8,1
cc              rcldat(i+1) = param(i)
cc  360     continue
cc          call putcl (icltyp,TORUSV,10,rcldat)
c
c...STOCK/LOAD,file
c
      else if (isav .eq. LOADV) then
          ldtext = .true.
          call parsit
          ldtext = .false.
          isca = 0
          if (lstrng) then
            j = 0
            fnam = ' '
            call gttext(fnam,j)
            call parsit
            if (ityp .eq. 2 .and. (ist .eq. 1 .or. ist .eq. 2)) then
                stok = token2
                ssub = ivxsub
                isca = 1
            else if (ityp .ne. 7) then
                go to 9150
            endif
          else
            err  = .false.
            ipos = index(cimage(1:nccimg),',')
            if (ipos .eq. 0) go to 9700
            temp = cimage(ipos+1:nccimg)
            ipos = index(temp,',')
            if (ipos .eq. 0) go to 9700
            j = nindex(temp(ipos+1:)," ")
            fnam = temp(ipos+j:)
          endif
c
          idsav = numid
          if (ifl(35) .eq. 0) then
              nci = strlen1(fnam)
              call ulf_verify_load (fnam,nci,numid,kerr)
              if (kerr .ne. 0) go to 9800
          endif
c
c......Store number of stocks loaded
c
          if (isca .eq. 1) then
              idst   = 2
              savid2 = stok
              isvsub = ssub
              rest   = numid
              call vstore
          endif
c
c......Create clfile record
c
          nc = strlen1(fnam)
          ptxt = ptxt(1:ncp) // ' LOAD'
          ncp  = ncp + 5
          temp = '"' // fnam(1:nc) // '"'
          nc2  = nc + 2
          call stkout (ptxt,ncp,idsav,param,0,idns,0,temp,nc2)
cc          jcldat(1) = idsav
cc          nc = strlen1(fnam)
cc          jcldat(2) = nc
cc          lcldat(9:nc+8) = fnam(1:nc)
cc          nc2 = (nc+7) / 8 + 2
cc          call putcl (icltyp,LOADV,nc2,rcldat)
c
c...STOCK/STL,units,file
c
      else if (isav .eq. STLV) then
          call parsit
          if (ITYP .eq. 1 .and. IST .eq. INCHV) then
              iunit = 0
          else if (ITYP .eq. 1 .and. IST .eq. MMV) then
              iunit = 1
          else
              go to 9900
          endif
          ldtext = .true.
          call parsit
          ldtext = .false.
          if (lstrng) then
            j = 0
            fnam = ' '
            call gttext(fnam,j)
          else
            err  = .false.
            ipos = index(cimage(1:nccimg),',')
            if (ipos .eq. 0) go to 9700
            fnam = cimage(ipos+1:nccimg)
            ipos = index(fnam,',')
            if (ipos .eq. 0) go to 9700
            temp = fnam(ipos+1:)
            ipos = index(temp,',')
            if (ipos .eq. 0) go to 9700
            j = nindex(temp(ipos+1:)," ")
            fnam = temp(ipos+j:)
          endif
          idsav = numid
          if (ifl(35) .eq. 0) then
              nci = strlen1(fnam)
              call ulf_verify_stl (ifxt,fnam,nci,iunit,numid,kerr)
              if (kerr .ne. 0) go to 9800
          endif
c
c......Create clfile record
c
          if (iunit .eq. 0) then
              temp = "INCHES"
              nc2 = 6
          else
              temp = "MM"
              nc2 = 2
          endif
          nc = strlen1(fnam)
          ptxt = ptxt(1:ncp) // ' STL'
          ncp  = ncp + 4
          temp(nc2+1:) = ' "' // fnam(1:nc) // '"'
          nc2  = nc2 + nc + 3
          call stkout (ptxt,ncp,idsav,param,0,idns,0,temp,nc2)
cc          jcldat(1) = idsav
cc          nc = strlen1(fnam)
cc          jcldat(2) = nc
cc          jcldat(3) = iunit
cc          lcldat(17:nc+16) = fnam(1:nc)
cc          nc2 = (nc+15) / 8 + 2
cc          call putcl (icltyp,STLV,nc2,rcldat)
c
c...STOCK/CLONE,stock[,ncopy]
c
      else if (isav .eq. CLONEV) then
          call parsit
          if (.not. scalar) go to 9100
          icol = ITV
          nc = 1
          if (nextyp .ne. 11) then
              call parsit
              if (.not. scalar) go to 9100
              if (ITV .le. 0) go to 9400
              nc = ITV
          endif
          idsav = numid
          if (ifl(35) .eq. 0) then
              xfl    = 0
              call ulf_verify_copy (ifxt,icol,numid,nc,xfl,param,kerr)
              if (kerr .ne. 0) go to 9010
          endif
c
c......Create clfile record
c
          ptxt = ptxt(1:ncp) // ' CLONE'
          ncp  = ncp + 6
          idns(1) = icol
          idns(2) = nc
          call stkout (ptxt,ncp,idsav,param,0,idns,2,temp,0)
cc          jcldat(1) = idsav
cc          jcldat(2) = icol
cc          jcldat(3) = nc
cc          call putcl (icltyp,CLONEV,3,rcldat)
c
c...STOCK/SOLID
c
      else if (isav .eq. SOLIDV) then
          call stksol (ifxt,numid,kerr)
          if (kerr .gt. 1) go to 9090
          if (kerr .ne. 0) go to 9080
c
c...STOCK/MOVE,stock
c
      else if (isav .eq. MOVEV) then
c
c......Get the list of stocks to move
c
          call prsstk (idns,nidns,ierr)
          if (ierr .ne. 0) go to 9200
          if (nidns .eq. 0) go to 9100
c
c......Get matrix
c
          if (ITYP .ne. 1 .or. (IST .ne. ATV .and. IST .ne. INCRV))
     1        go to 9020
          incr   = 0
          if (IST .eq. INCRV) incr = 1
          call parsit
          if (ITYP .ne. 2 .or. IST .ne. 10) go to 9030
          call gtentt (TV,trflg,nclkey,ietype,param)
          param(4)  = param(4) * ufct
          param(8)  = param(8) * ufct
          param(12) = param(12) * ufct
c
c......Move stock
c
          if (ifl(35) .eq. 0) then
              nc = strlen1 (token2)
              call ulf_verify_move (ifxt,idns,nidns,param,
     x              TOKEN2,nc,incr,kerr)
              if (kerr .eq. -1) go to 9035
              if (kerr .ne. 0) then
                  write (TOKEN2(1:6),10) kerr
                  go to 9010
              endif
          endif
c
c......Create clfile record
c
          ptxt = ptxt(1:ncp) // ' MOVE'
          ncp  = ncp + 5
          nc2 = nidns
          idsav = incr
          param(4) = param(4) * cnv
          param(8) = param(8) * cnv
          param(12) = param(12) * cnv
          call stkout (ptxt,ncp,idsav,param,12,idns,nc2,temp,0)
cc          jcldat(1) = nidns
cc          do 410 i=1,12,1
cc              rcldat(i+1) = param(i)
cc  410     continue
cc          lcldat(105:112) = TOKEN2(1:8)
cc          inc = 28
cc          do 640 i=1,nidns,1
cc              inc = inc + 1
cc              jcldat(inc) = idns(i)
cc              if (inc .eq. 100) then
cc                  jcldat(1) = inc - 28
cc                  nc2 = (inc+1) / 2 + 1
cc                  call putcl (icltyp,MOVEV,nc2,rcldat)
cc                  inc = 28
cc              endif
cc  640     continue
cc          if (inc .gt. 28) then
cc              jcldat(1) = inc - 28
cc              nc2 = (inc+1) / 2 + 1
cc              call putcl (icltyp,MOVEV,nc2,rcldat)
cc          endif
c
c...STOCK/REMOVE
c
      else if (isav .eq. REMOVV) then
c
c......STOCK/REMOVE,CHIPS
c......NOTE: Chips are only removed during simulation
c
          if (ITYP .eq. 1 .and. IST .eq. 331) then
              if (ifxt .eq. 1) go to 9020
              nidns  = 0
              inc    = 1
  450         if (inc .eq. 50) go to 9070
              call gtpv (itrflg,rcldat(inc),ierr)
              if (ierr .ne. 0) go to 9200
              call vctmsc (rcldat(inc),rcldat(inc),cnv)
              nidns  = nidns  + 1
              inc    = inc    + 6
              if (NEXTYP .ne. 11) go to 450
c
c.........Store the clfile record
c
              ptxt = ptxt(1:ncp) // ' REMOVE_CHIPS'
              ncp  = ncp + 13
              nc2 = inc - 1
              idsav = 0
              call stkout (ptxt,ncp,idsav,rcldat,nc2,idns,0,temp,0)
cc              nc2    = inc    + 1
cc              call putcl (icltyp,CHIPV,nc2,rcldat)
c
c......STOCK/REMOVE
c.........Get the list of stocks to remove
c
          else
              call prsstk (idns,nidns,ierr)
              if (ierr .ne. 0) go to 9200
              if (nidns .eq. 0) go to 9100
c
c.........Remove stocks
c
              if (ifl(35) .eq. 0) then
                  call ulf_verify_remove (ifxt,idns,nidns,kerr)
                  if (kerr .ne. 0) then
                      write (TOKEN2(1:6),10) kerr
   10                 format (i6)
                      go to 9010
                  endif
              endif
c
c.........Create clfile record
c
              ptxt = ptxt(1:ncp) // ' REMOVE'
              ncp  = ncp + 7
              nc2 = nidns
              idsav = 0
              call stkout (ptxt,ncp,idsav,param,0,idns,nc2,temp,0)
cc              jcldat(1) = nidns
cc              inc = 2
cc              do 510 i=1,nidns,1
cc                  inc = inc + 1
cc                  jcldat(inc) = idns(i)
cc                  if (inc .eq. 100) then
cc                      jcldat(1) = inc - 2
cc                      nc2 = (inc+1) / 2 + 1
cc                      call putcl (icltyp,REMOVV,nc2,rcldat)
cc                      inc = 2
cc                  endif
cc  510         continue
cc              if (inc .gt. 2) then
cc                  jcldat(1) = inc - 2
cc                  nc2 = (inc+1) / 2 + 1
cc                  call putcl (icltyp,REMOVV,nc2,rcldat)
cc              endif
          endif
c
c...STOCK/MODIFY,stock
c
      else if (isav .eq. MODV) then
          icol   = -1
          ivis   = -1
          tol    = -1.
          itrans = -1
          iact   = -1
c
c......Get the list of stocks to modify
c
          call prsstk (idns,nidns,ierr)
          if (ierr .ne. 0) go to 9200
          if (nidns .eq. 0) go to 9100
          go to 650
c
c......Get the next attribute
c
  600     call parsit
  650     if (ITYP .ne. 1) go to 9020
          isav = IST
          call parsit
          if (ITYP .ne. 5 .or. IST .ne. 1) go to 9040
          call parsit
c
c......STOCK/MODIFY,COLOR=col
c
          if (isav .eq. COLV) then
              if (scalar) then
                  if (ITV .lt. 0 .or. ITV .gt. 63) go to 9050
                  icol = itv
              else if (ITYP .eq. 1) then
                  if (IST .eq. BLACKV) icol = 0
                  if (IST .eq. WHITEV) icol = 1
                  if (IST .eq. BLUEV ) icol = 2
                  if (IST .eq. REDV  ) icol = 3
                  if (IST .eq. GREENV) icol = 4
                  if (IST .eq. MGNTAV) icol = 5
                  if (IST .eq. YELLOV) icol = 6
                  if (IST .eq. CYANV ) icol = 7
                  if (IST .eq. BROWNV) icol = 8
                  if (IST .eq. TANV  ) icol = 9
                  if (IST .eq. LTBLUV) icol = 10
                  if (IST .eq. SEAGRV) icol = 11
                  if (IST .eq. ORANGV) icol = 12
                  if (IST .eq. PINKV ) icol = 13
                  if (IST .eq. PURPLV) icol = 14
                  if (IST .eq. GREYV ) icol = 15
                  if (icol .eq. -1) go to 9050
              else if ((ITYP .eq. 2 .and. IST .eq. 1) .or.
     1            (ITYP .eq. 2 .and. IST .eq. 24) .or.
     2            (ITYP .eq. 9)) then
                  if (ITYP .eq. 9 .or. (ITYP .eq. 2 .and. IST .eq. 1))
     1                    then
                      comstr = token2
                      j = strlen1(token2)
                      isub = ivxsub
                  endif
                  if (ITYP .eq. 2 .and. IST .eq. 24) then
                     j = 0
                     call gttext(comstr,j)
                     isub = 0
                  endif
                  call ncl_getclr_inx(comstr, j, isub, indx)
                  if (indx .ge. 16 .and. indx .le. 63) then
                      icol = indx
                  else
                      go to 9020
                  endif
              else
                  go to 9020
              endif
c
c......STOCK/MODIFY,VISIBL=ON/OFF
c
          else if (isav .eq. VISV) then
              if (ITYP .ne. 1) go to 9020
              if (IST .eq. ONV) then
                  ivis = 1
              else if (IST .eq. OFFV) then
                  ivis = 0
              else
                  go to 9060
              endif
c
c......STOCK/MODIFY,TOLER=tol
c
          else if (isav .eq. TOLERV) then
              if (.not. scalar) go to 9100
              if (TV .le. 0.) go to 9400
              tol = TV * ufct
c
c......STOCK/MODIFY,TRANS=tr
c
          else if (isav .eq. TRANSV) then
              if (.not. scalar) go to 9100
              if (ITV .le. 0 .or. ITV .gt. 100) go to 9400
              itrans = ITV
c
c......STOCK/MODIFY,ACTIVE=ON/OFF
c
          else if (isav .eq. ACTV) then
              if (ITYP .ne. 1) go to 9020
              if (IST .eq. ONV) then
                  iact = 1
              else if (IST .eq. OFFV) then
                  iact = 0
              else
                  go to 9060
              endif
          endif
c
c......Get the next attribute
c
          if (nextyp .ne. 11) go to 600
c
c......Modify the stock attributes
c
          if (ifl(35) .eq. 0) then
              call ulf_verify_modify (ifxt,idns,nidns,icol,ivis,tol,
     1                                itrans,iact,kerr)
              if (kerr .ne. 0) go to 9010
          endif
c
c......Create clfile record
c
          ptxt = ptxt(1:ncp) // ' MODIFY'
          ncp  = ncp + 7
          nc2 = nidns
          param(1) = icol
          param(2) = ivis
          param(3) = itrans
          param(4) = iact
          param(5) = tol * cnv
          idsav = 0
          call stkout (ptxt,ncp,idsav,param,5,idns,nc2,temp,0)
cc          jcldat(1) = nidns
cc          jcldat(2) = icol
cc          jcldat(3) = ivis
cc          jcldat(4) = itrans
cc          jcldat(5) = iact
cc          rcldat(4) = tol
cc          inc = 8
cc          do 680 i=1,nidns,1
cc              inc = inc + 1
cc              jcldat(inc) = idns(i)
cc              if (inc .eq. 100) then
cc                  jcldat(1) = inc - 8
cc                  nc2 = (inc+1) / 2 + 1
cc                  call putcl (icltyp,MODV,nc2,rcldat)
cc                  inc = 8
cc              endif
cc  680     continue
cc          if (inc .gt. 8) then
cc              jcldat(1) = inc - 8
cc              nc2 = (inc+1) / 2 + 1
cc              call putcl (icltyp,MODV,nc2,rcldat)
cc          endif
c
c...Unrecognized word
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 ifl(44) = ifl44
      return
c
c...Stock type expected
c
 9000 call error (1)
      err = .true.
      go to 8000
c
c...Stock not previously defined
c
 9010 call error (9)
      err = .true.
      go to 8000
c
c...Invalid syntax
c
 9020 call error (61)
      err = .true.
      go to 8000
c
c...Matrix expected
c
 9030 call error (94)
      err = .true.
      go to 8000
c
c...Orthogonal Matrix expected
c
 9035 call error (535)
      err = .true.
      go to 8000
c
c...Equals expected
c
 9040 call error (6)
      err = .true.
      go to 8000
c
c...Invalid color parameter
c
 9050 call error (428)
      err = .true.
      go to 8000
c
c...ON or OFF expected
c
 9060 call error (56)
      err = .true.
      go to 8000
c
c...Too many parameters
c
 9070 call error (93)
      err = .true.
      go to 8000
c
c...Solid expected
c
 9080 call error (531)
      err = .true.
      go to 8000
c
c...Use provided error message
c
 9090 call error (kerr)
      err = .true.
      go to 8000
c
c...Number expected
c
 9100 call error (7)
      err = .true.
      go to 8000
c
c...Variable expected
c
 9150 call error (282)
      err = .true.
      go to 8000
c
c...Point expected
c
 9200 call error (ierr)
      err = .true.
      go to 8000
c
c...Could not create box
c
 9300 call error (163)
      err = .true.
      go to 8000
c
c...Identifier must be positive
c
 9400 call error(112)
      err = .true.
      go to 8000
c
c...Input value out of range
c
 9500 call error (445)
      err = .true.
      go to 8000
c
c...Circle expected
c
 9600 call error (159)
      err = .true.
      go to 8000
c
c...Filename not found
c
 9700 call error (57)
      err = .true.
      go to 8000
c
c...Could not process file
c
 9800 call error (271)
      err = .true.
      go to 8000
c
c...Inches or MM expected
c
 9900 call error (318)
      err = .true.
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stksol (kwhch,kidn,kerr)
c
c   FUNCTION:  This routine defines an NCLIPV stock/fixture from a
c              predefined Visual Solid.
c
c   INPUT:  kwhch    I*4    D1   321 = STOCK, 898 = FIXTUR.
c
c           kidn     I*4    D1   Stock ID number.
c
c   OUTPUT: kerr     I*4    D1   Returns non-zero if an error occurred
c                                creating the stock.
c
c***********************************************************************
c
      subroutine stksol (kwhch,kidn,kerr)
c
      include 'com8a.com'
c
      integer*4 kwhch,kerr
c
      integer*2 nc2
      integer*4 nclkey,nc,ncp,iary(4),idsav,styp,np
c
      real*8 param(128),rnum
c
      character*(MAX_LEN) temp,fnam
      character*640 ptxt
      byte bfil(MAX_PATH)
c
      equivalence (param,iary,bfil)
c
c...Initialize routine
c
      if (kwhch .eq. 1) then
          ptxt = 'IPV FIXTUR'
          ncp  = 10
      else
          ptxt = 'IPV STOCK'
          ncp  = 9
      endif
      kerr = 0
      idsav  = kidn
c
c...Create the solid
c
      call parsit
      if (ITYP .ne. 2 .or. IST .ne. 33) go to 9000
      call gtdesc (tv,nclkey,nwds,itype)
      call ulf_verify_solid (kwhch,nclkey,kidn,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Get the solid parameters
c
      call nclf_get_solid (nclkey,styp,param,np)
c
c...Output clfile record
c......Box
c
      if (styp .eq. 1) then
          ptxt = ptxt(1:ncp) // ' BOX'
          ncp  = ncp + 4
          call stkout (ptxt,ncp,idsav,param,6,idns,0,temp,0)
c
c......Cylinder
c
      else if (styp .eq. 2) then
          ptxt = ptxt(1:ncp) // ' CYLNDR'
          ncp  = ncp + 7
          rnum = param(7)
          param(7) = param(8)
          param(8) = rnum
          call stkout (ptxt,ncp,idsav,param,8,idns,0,temp,0)
c
c......Cone
c
      else if (styp .eq. 5) then
          ptxt = ptxt(1:ncp) // ' CONE'
          ncp  = ncp + 5
          rnum = param(7)
          param(7) = param(8)
          param(8) = param(9)
          param(9) = rnum
          call stkout (ptxt,ncp,idsav,param,9,idns,0,temp,0)
c
c......Sphere
c
      else if (styp .eq. 4) then
          ptxt = ptxt(1:ncp) // ' SPHERE'
          ncp  = ncp + 7
          call stkout (ptxt,ncp,idsav,param,4,idns,0,temp,0)
c
c......Torus
c
      else if (styp .eq. 3) then
          ptxt = ptxt(1:ncp) // ' TORUS'
          ncp  = ncp + 6
          call stkout (ptxt,ncp,idsav,param,8,idns,0,temp,0)
c
c......STL
c
      else if (styp .eq. 9) then
          if (iary(1) .eq. 0) then
              temp = "INCHES"
              nc2 = 6
          else
              temp = "MM"
              nc2 = 2
          endif
          call btoc (bfil(113),fnam,nc)
          ptxt = ptxt(1:ncp) // ' STL'
          ncp  = ncp + 4
          temp(nc2+1:) = ' "' // fnam(1:nc) // '"'
          nc2  = nc2 + nc + 3
          call stkout (ptxt,ncp,idsav,param,0,idns,0,temp,nc2)
      endif
c
c...End of routine
c
 8000 return
c
c...Solid expected
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prsstk (idns,nidns,ierr)
c
c   FUNCTION:  This routine parses the input command for the following
c              parameters.
c
c                 num-id1,...,num-idm,THRU,num-idn
c
c              There can be a maximum of 1000 values.
c
c   INPUT:  kwhch    I*2    D1   321 = STOCK, 898 = FIXTUR.
c
c   OUTPUT: idns     I*4    D1000   Values contained in statement.
c
c           nidns    I*4    D1      Number of values in 'idns'. 
c
c           ierr     I*2    D1      Non-zero if an error occurs.
c
c***********************************************************************
c
      subroutine prsstk (idns,nidns,ierr)
c
      include 'com8a.com'
c
      integer*2 ierr
      integer*4 idns(1000),nidns
c
      logical ithru
c
      integer*2 THRUV
      parameter (THRUV=152)
c
c...Initialize routine
c
      nidns = 0
      ierr  = 0
      ithru = .false.
      go to 150
c
c...Get next token
c
  100 idtype = -1
      call parsit
c
c...m,n,THRU,o,p
c
  150 if (ITYP .eq. 1 .and. IST .eq. THRUV) then
          if (ithru .or. nidns .eq. 0) go to 9000
          ithru = .true.
      else if (scalar) then
          if (ITV .le. 0) go to 9200
          if (ithru) then
              if (ITV .lt. idns(nidns)) go to 9200
              ist = idns(nidns) + 1
              ien = ITV
              do 200 i=ist,ien,1
                  if (nidns .eq. 1000) go to 8000
                  nidns = nidns + 1
                  idns(nidns) = i
  200         continue
              ithru = .false.
          else
              if (nidns .eq. 1000) go to 8000
              nidns = nidns + 1
              idns(nidns) = ITV
          endif
      else
          if (ithru) go to 9100
          go to 8000
      endif
      go to 100
c
c...End of routine
c
 8000 return
c
c...Invalid THRU clause
c
 9000 ierr = 114
      go to 8000
c
c...Scalar expected
c
 9100 ierr = 7
      go to 8000
c
c...Input value out of range
c
 9200 ierr = 445
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stkout (ctxt,knc,kidn,gparm,knprm,kidns,knid,cstr,knc2)
c
c   FUNCTION:  This routine outputs a STOCK/FIXTUR command as a PPRINT
c              statement to the clfile.
c
c   INPUT:  ctxt     C*n    D1      Current text to output in PPRINT
c                                   statement.
c
c           knc      I*4    Dn      Number of chars in 'ctxt'.
c
c           kidn     I*4    D1      ID number of stock.
c
c           gparm    R*8    Dn      Real parameters to output in PPRINT
c                                   statement.
c
c           knprm    I*2    D1      Number of values in 'gparm'.
c
c           kidns    I*4    Dn      Integer parameters to output in PPRINT
c                                   statement.
c
c           knid     I*2    D1      Number of values in 'kidns'.
c
c           cstr     C*n    D1      Text to append to command.
c
c           knc2     I*2    Dn      Number of chars in 'cstr'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine stkout (ctxt,knc,kidn,gparm,knprm,kidns,knid,cstr,knc2)
c
      include 'com8a.com'
c
      integer*2 knprm,knid,knc2
      integer*4 knc,kidn,kidns(*)
c
      real*8 gparm(*)
c
      character*(*) ctxt,cstr
c
      integer*2 icldat(320)
      integer*4 inc,nc,nc4,jcldat(160)
c
      real*8 rcldat(640)
c
      character*66 ltxt
      character*640 lcldat
c
      equivalence (rcldat,jcldat,icldat,lcldat)
c
c...Determine if input text needs to be
c...broken up into multiple PPRINT statements
c
      nc  = knc
      inc = 1
      do while (nc .gt. 65)
          lcldat(1:66) = ctxt(inc:inc+64) // '~'
          call putcl (2000,1044,10,rcldat)
          inc = inc + 65
          nc  = nc  - 65
      enddo
      lcldat(1:66) = ctxt(inc:nc)
      nc = nc - inc + 1
c
c...Add stock ID to command
c
      call itoc (kidn,ltxt,nc4)
      if (nc+nc4+1 .ge. 66) then
          lcldat(nc+1:66) = '~'
          call putcl (2000,1044,10,rcldat)
          nc = 0
      endif
      lcldat(nc+1:66) = ' ' // ltxt(1:nc4) // ' '
      nc = nc + nc4 + 2
c
c...Add real values to command
c
      do 100 i=1,knprm,1
          call rtoc (gparm(i),ltxt,nc4)
          if (nc+nc4+1 .ge. 66) then
              lcldat(nc+1:66) = '~'
              call putcl (2000,1044,10,rcldat)
              nc = 0
          endif
          lcldat(nc+1:66) = ltxt(1:nc4) // ','
          nc = nc + nc4 + 1
  100 continue
c
c...Add integer values to command
c
      do 200 i=1,knid,1
          call itoc (kidns(i),ltxt,nc4)
          if (nc+nc4+1 .ge. 66) then
              lcldat(nc+1:66) = '~'
              call putcl (2000,1044,10,rcldat)
              nc = 0
          endif
          lcldat(nc+1:66) = ltxt(1:nc4) // ','
          nc = nc + nc4 + 1
  200 continue
c
c...Add filename to command
c
      if (knc2 .eq. 0) then
          lcldat(nc:66) = ' '
          nc = nc - 1
      else
          if (nc+knc2+1 .lt. 66) then
              lcldat(nc+1:66) = ' ' // cstr(1:knc2)
              nc = nc + knc2 + 1
          else
              nc = nc + 1
              do 300 i=1,knc2,1
                  if (nc .eq. 65) then
                      lcldat(nc+1:66) = '~'
                      call putcl (2000,1044,10,rcldat)
                      lcldat = ' '
                      nc = 0
                  endif
                  nc = nc + 1
                  lcldat(nc:nc) = cstr(i:i)
  300         continue
          endif
      endif
c
c...Output final PPRINT command
c
      if (nc .ne. 0) call putcl (2000,1044,10,rcldat)
c
c...End of routine
c
 8000 return
      end
