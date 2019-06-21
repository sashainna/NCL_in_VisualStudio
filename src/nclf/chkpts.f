C*********************************************************************
C*    NAME         :  chkpts.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
c*    MODULE NAME AND RELEASE LEVEL 
C*        chkpts.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:40
c*********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine chkpts (iwhich)
C*       purpose of subroutine: to parse chkpts command.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          iwhich     - = 3 if macro call initiated.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine chkpts (iwhich)
c
      include 'com.com'
      include 'cmm.com'
c
      integer*2 ichk,iwhich
c
c     common /cmmcom/ cmmtol, cmmmxd, cmmmxa, cmmmnm, cmminm, cmmnpt
c     real*8 cmmtol, cmmmxd, cmmmxa, cmmmnm
c     integer*2 cmminm, cmmnpt
c
      real*8 hldtol, hldmxd, hldmxa
      integer*2 ihld
      integer*4 j,k

      integer*2 i, irest(4), ksn(4), ipg, iel, nwds
      real*8 r8parm(35), asn
      character*64  cmmcnm, parms(35)
      equivalence (rest,irest),(r8parm,parms)
      equivalence (cmmmnm,cmmcnm),(asn,ksn,ipg),(ksn(2),iel)
      equivalence (ichk,ifl(246))

      character*40 label
      character*3 vnms(4)
      data vnms(1)/'@P '/,vnms(2)/'@V1'/,vnms(3)/'@V2'/,vnms(4)/'@V3'/

      integer*2 izro
      data izro /0/

      integer*2 NOMORE,CANCEL,SLASH,MACRO,PS,DS
      integer*4 strlen1
      parameter (NOMORE=53)
      parameter (CANCEL=843)
      parameter (SLASH=5)
      parameter (MACRO=806)
      parameter (PS=728)
      parameter (DS=729)

      iwhich = 0
      ichk = 0

      if (nextyp.ne.SLASH) goto 9022

      ifl(44) = 9
      call parsit
      if (ityp.ne.1) goto 100
      if (nextyp.ne.11) goto 9004
      if (ist.ne.NOMORE) goto 20
c
c...  CHKPTS/NOMORE
c
      lcmm = .false.
      if (cmmcnm.eq.' ') goto 999
c
c... Call user supplied macro
c
      token2 = cmmmnm
      call vstchk
      if (ist.ne.11) goto 9054
      asn = tv
      if (ksn(3).ne.5) goto 9423
      nwds = 22
      i = strlen1(cmmcnm)
      cin(1:i) = cmmcnm(1:i)
      cin(i+1:i+1) = ','
      i = i+2
      k = 1
      call ncl_getmac_plabel(k, label, j)
      cin(i:) = label(1:j)
      i = i+j
      cin(i:i+3) = '=@N,'
      i = i+4
      k = 2
      call ncl_getmac_plabel(k, label, j)
      cin(i:) = label(1:j)
cyu      call expnam (r8parm(15), j, cin(i:))
      i = i+j
      cin(i:i+3) = '=@P,'
      i = i+4
      k = 3
      call ncl_getmac_plabel(k, label, j)
      cin(i:) = label(1:j)
cyu      call expnam (r8parm(17), j, cin(i:))
      i = i+j
      cin(i:i+4) = '=@V1,'
      i = i+5
      k = 4
      call ncl_getmac_plabel(k, label, j)
      cin(i:) = label(1:j)
cyu      call expnam (r8parm(19), j, cin(i:))
      i = i+j
      cin(i:i+4) = '=@V2,'
      i = i+5
      k = 5
      call ncl_getmac_plabel(k, label, j)
      cin(i:) = label(1:j)
cyu      call expnam (r8parm(21), j, cin(i:))
      i = i+j
      cin(i:i+4) = '=@V3 '
      nccin = i + 4
      inx = 1
      nextyp = 5
      call callit
      if (ifl(2).ne.0) goto 999
      iwhich = 3
      lcpmac = .true.
      goto 999

20    continue
      if (ist.ne.CANCEL) goto 9424
      lcmm = .false.
      if (cmmcnm.eq.' ') goto 999
      call delcmm
      goto 999

100   continue
c
c...  CHKPTS/toler,maxdp,maxang [,MACRO,m1] [,PS/DS]
c
      if (.not.scalar) goto 9424
      hldtol = tv
      call parsit
      if (.not.scalar) goto 9007
      hldmxd = tv
      call parsit
      if (.not.scalar) goto 9007
      hldmxa = tv
      ihld = 5
      cmmcnm = ' '
      if (.not.nxteos) then
        call parsit
        if (ityp.ne.1.or.ist.ne.MACRO.and.ist.ne.PS.and.ist.ne.DS)
     x    goto 9004
        if (ist.eq.MACRO) then
          call parsit
          if (ityp.ne.2.or.ist.ne.11) goto 9054
          cmmmnm = token2
          if (nxteos) goto 120
          call parsit
        endif
        if (.not.nxteos) goto 9004
        if (ityp.ne.1.or.ist.ne.PS.and.ist.ne.DS) goto 9004
        if (ist.eq.DS) ihld = 6
      endif
120   cmmtol = hldtol
      cmmmxd = hldmxd
      cmmmxa = hldmxa
      cmminm = ihld
      ichk = cmminm
      cmmnpt = 0
      lcmm = .true.

      ivxsub = 0

      do 200,i=1,4
        token2(1:64) = vnms(i)
        call vstchk
        if (ist.eq.14) goto 200
        isvsub = ivxsub
        savid2=token2
        ifl(9)=ifl(11)
        ifl(10)=ifl(12)
        idst=14
        rest=0.
        irest(3)=32767
        irest(4)=14
        call vstore
200   continue

999   return

9004  ifl(2) = 4
      goto 999

9007  ifl(2) = 7
      goto 999

9022  ifl(2) = 22
      goto 999

9054  ifl(2) = 54
      goto 999

9061  ifl(2) = 61
      goto 999

9070  ifl(2) = 70
      goto 999

9423  ifl(2) = 423
      goto 999

9424  ifl(2) = 424
      goto 999

      end
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *|* * *
      subroutine delcmm

      include 'com.com'
      include 'cmm.com'
c
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*4 nclkey,i
      integer*2 nwds, ietype

      do 100 i=1,cmmnpt
          ivxsub = i
          token2(1:64) = '@P'
      call vstchk
      if (ityp.ne.2.or.ist.ne.3) goto 10
      call gtdesc(tv,nclkey,nwds,ietype)
      call dlgeom(nclkey)
   10 token2(1:64) = '@V1'
      call vstchk
      if (ityp.ne.2.or.ist.ne.4) goto 20
      call gtdesc(tv,nclkey,nwds,ietype)
      call dlgeom(nclkey)
20    token2(1:64) = '@V2'
      call vstchk
      if (ityp.ne.2.or.ist.ne.4) goto 30
      call gtdesc(tv,nclkey,nwds,ietype)
      call dlgeom(nclkey)
30    token2(1:64) = '@V3'
      call vstchk
      if (ityp.ne.2.or.ist.ne.4) goto 100
      call gtdesc(tv,nclkey,nwds,ietype)
      call dlgeom(nclkey)
100   continue

      cmmnpt = 0
      lcpmac = .false.

      token2(1:64) = '@N'
      ivxsub = 0
      call vstchk
      if (ityp.ne.2.or.ist.ne.2) goto 999
      rest = 0
      idst = 2
      savid2 = token2
      isvsub = ivxsub
      keyold = keyhld
      istold = ist
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
      call vstore

999   return
      end
