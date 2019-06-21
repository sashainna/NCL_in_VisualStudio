c
c***********************************************************************
c
c     FILE NAME: mchlin
c
c     CONTAINS:  compnt  genlpt  getnpt  gtlpos  iterlm  limpnt  dirchk
c                linfed  mchlin  redefp  retpnt  savpos  getdvt  vecdad
c                secchk  gtfrdv  ovrlim
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mchlin.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:35:13
c
c***********************************************************************
c
c***********************************************************************
c
c     SUBROUTINE: mchlin (kflg,kerr,klin,kfln)
c
c     FUNCTION:  This routine generates points for multiaxis motion when
c                linearization is turned on.
c
c     INPUT:   kflg     I*4  D1  -  input status
c
c              kerr     I*4  D1  -  tlaxis call error status
c
c              klin     I*4  D1  -  1 = Move should be linearized.
c                                   2 = Don't linearize move, just check
c                                       for longest route retraction.
c
c     OUTPUT:  kfln     I*4  D1  -  output status
c
c***********************************************************************
c
      subroutine mchlin (kflg,kerr,klin,kfln)
c
      include 'post.inc'
c
      integer*4 kflg,kerr,kfln,klin
c
      equivalence (IJKROT,KPOSMP(1739)), (IRAP  ,KPOSMP(3199))
c
      integer*4 IJKROT,IRAP
c
      equivalence (LNRTOL,POSMAP(2251))
c
      real*8 LNRTOL(3)
c
      LNRTOL(3) = LNRTOL(1)
      if (IRAP .eq. 1 .or. IRAP .eq. 2) LNRTOL(3) = LNRTOL(2)
      if (LNRTOL(3) .eq. 0. .and. klin .ne. 2) go to 8000
c
      if (IJKROT .eq. 1) then
          call mchlin_tv (kflg,kfln)
      else
          kfln   = 0
          call mchlin_new (kflg,kerr,klin)
      end if
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mchlin_tv (kflg,kfln)
c
c     FUNCTION:  This routine generates points for motion with tool axis
c                changing its orientation without support of rotary axes
c                when linearization is turned on.
c
c     INPUT:   kflg     I*4  D1  -  input status
c
c     OUTPUT:  kfln     I*4  D1  -  output status
c
c***********************************************************************
c
      subroutine mchlin_tv (kflg,kfln)
c
      include 'post.inc'
c
      integer*4 kflg,kfln
c
      equivalence (NUMIJK,KPOSMP(1277)), (HLDFLG,KPOSMP(1622))
      equivalence (ITP   ,KPOSMP(1801)), (IRAP  ,KPOSMP(3199))
c
      integer*4 NUMIJK,ITP,HLDFLG,IRAP
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (IJKPTS,POSMAP(0318)), (IJKVEC,POSMAP(0321))
      equivalence (IJKAST,POSMAP(0324)), (IJKAFI,POSMAP(0325))
      equivalence (IJKADL,POSMAP(0326)), (IJKFI ,POSMAP(0327))
      equivalence (IJKTHT,POSMAP(0328)), (LINAXS,POSMAP(1299))
      equivalence (IJKVC ,POSMAP(0329))
      equivalence (MCHNUM,POSMAP(1287)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (HLDMCH,POSMAP(2141)), (HLDVEC,POSMAP(2177))
      equivalence (LNRTOL,POSMAP(2251)), (TL    ,POSMAP(3601))
      equivalence (FUZZ4 ,POSMAP(4912)), (ROTANG,POSMAP(5173))
c
      real*8 TL(120),MCHNUM(3,4),STONUM(3,4),TLVEC(3),VECSAV(3),
     -       HLDMCH(3,4),HLDVEC(3),LNRTOL(3),IJKFI,IJKTHT,IJKAFI,
     -       IJKAST,IJKADL,FUZZ4,IJKPTS(3),IJKVEC(3),LINAXS(6),
     -       AXSOUT(10),ROTANG(20,2),RAD,IJKVC(3)
c
      real*8 mlax(6),mxout(10),tvs(3),ang,tvm(4),ratio,d,amin
c
      integer*4 num,iary(10),icnt,ifir
c
      ifir   = 0
      if (LNRTOL(3) .eq. 0.) go to 8000
      if (TL(ITP) .le. LNRTOL(3)) go to 8000
c
c...Initial call, check is linearization is required
c...calculate some common constants
c
      if (kflg .eq. 0) then
         kfln   = 0
         if (HLDFLG .eq. 1) then
            call copyn (HLDMCH(1,2),IJKPTS,3)
            call copyn (HLDVEC(1),tvs,3)
         else
            call copyn (STONUM(1,2),IJKPTS,3)
            call copyn (VECSAV(1),tvs,3)
         end if
         call getlan (tvs,TLVEC,IJKAST,IJKAFI,IJKADL,
     -                IJKFI,IJKTHT,tvm)
         if (IJKADL .lt. .01) go to 8000
         d = TL(ITP) - LNRTOL(3)
         if (d .lt. FUZZ4) then
            amin = 180.d0
         else
            amin = 2.0*RAD*dacos (d/TL(ITP))
         end if
         num    = IJKADL / amin + 1
         if (num .eq. 1) go to 8000
         call vcplvc (MCHNUM(1,2),IJKPTS,IJKVEC,-1.d0)
         NUMIJK = num
         kflg   = NUMIJK
         ifir   = 1
         kfln   = NUMIJK
      end if
c
c...following calls to interpolate point
c...and tool vector
c
 1000 if (kfln .ne. 0) then
         d      = NUMIJK
         kfln   = kfln - 1
         ratio  = (NUMIJK - kfln) / d
         call vcplvc (IJKPTS,IJKVEC,MCHNUM(1,2),ratio)
         ang    = IJKAST + ratio * IJKADL
         call setijk (ang,IJKVC,IJKFI,IJKTHT)
         call alladj (MCHNUM,mlax,mxout,ROTANG,1,5)
c
c...put tool axis vector in mxout to save it on stack
c...this is OK since rotary axes are not used in IJK mode
c
         call copyn (IJKVC,mxout(7),3)
c
         if (ifir .eq. 1) then
            ifir   = 0
            call pshaxs (mxout,IJKVC)
            go to 1000
         else
            call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
            call copyn (TLVEC,IJKVC,3)
            call pshaxs (mxout,IJKVC)
         end if
      else
         call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
         call copyn (TLVEC,IJKVC,3)
         kflg = 0
      end if

 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mchlin_rot (kflg,kerr,kfln)
c
c     FUNCTION:  This routine generates points for multiaxis motion when
c                linearization is turned on.
c
c     INPUT:   kflg     I*4  D1  -  input status: 0 - initial call,
c                                   n < 0 - retract procedure (1,2,3,4),
c                                   n - following calls.
c
c              kerr     I*4  D1  -  tlaxis call error status: 3 - not the
c                                   shortest rotary motion, retract tool
c                                   when limits are reached. n - normal
c                                   linearization.
c
c     OUTPUT:  kfln     I*4  D1  -  output status:  0 - last point has
c                                   been generated, n - genarated point
c                                   is not the last one.
c
c***********************************************************************
c
      subroutine mchlin_rot (kflg,kerr,kfln)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (IFWDFL,KPOSMP(0832))
      equivalence (IRTNXF,KPOSMP(1365)), (IRTNXT,KPOSMP(1361))
      equivalence (LRTRCT,KPOSMP(1278)), (NROT  ,KPOSMP(1366))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRTNLS,KPOSMP(1641)), (IRFNLS,KPOSMP(1640))
      equivalence (IFLNRS,KPOSMP(1731)), (IRAP  ,KPOSMP(3199))
      equivalence (KERRSV,KPOSMP(0109)), (IRTSAM,KPOSMP(1724))
      equivalence (IMAXFL,KPOSMP(1729)), (IRTACT,KPOSMP(1256))
      equivalence (HLDFLG,KPOSMP(1622)), (MDTLIN,KPOSMP(4032))
c
      integer*4 LRTRCT,NROT,IRTNXT(4),IFLNRS,IRTINC(4),
     -          IRTNXF,IRFNLS,IRTNLS(4),IRAP,KERRSV,IRTSAM,
     -          IRTACT(2),IMAXFL,HLDFLG,MACHTP,
     -          IFWDFL,MDTLIN
c
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435)), (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387))
      equivalence (ROTBSV,POSMAP(2275)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340))
      equivalence (LNRTOL,POSMAP(2251)), (RETFED,POSMAP(2254))
      equivalence (PTLPLU,POSMAP(2301))
      equivalence (FWDDIR,POSMAP(4580)), (VTOLER,POSMAP(4911))
      equivalence (FUZZ4 ,POSMAP(4912)), (FUZZ8 ,POSMAP(4913))
      equivalence (FUZZM ,POSMAP(4914))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (HLDROT,POSMAP(5253))
c
      real*8 MCHNUM(3,4),VECSAV(3),ROTANG(20,2),STONUM(3,4),
     -       ROTSTO(20,2),TLVEC(3),ROTBAS(4),
     -       LINAXS(6),ROTBSV(4),AXSOUT(10),RETFED(4),
     -       PTLPLU(3),HLDROT(20,2),FWDDIR(3),VTOLER,FUZZ4,FUZZ8,
     -       FUZZM,LNRTOL(3)
c
      integer*4 kerr,kflg,kfln
c
      integer*4 ierr,iary(10),icnt,iaxs(10),ifir,irro,i,nr,lhsav,
     -          ifl,nuax,irnx(4),irfx,ir1,ir2,nerr,nvrt,nper,lfln,nlop,
     2          iadj,ip1,ip2
c
      equivalence (IRTACT(1),ir1), (IRTACT(2),ir2)
c
      real*8 mlax(6),mxout(10),anewd(4),tasav,rots(20,2),dan1,dan2,
     -       bass(4),dan
c
c...set flags
c
      ifir   = 0
      irro   = 1
      ifl    = 1
      nper   = 0
      lfln   = 1
      ip1    = IRTINC(ir1)
      ip2    = IRTINC(ir2)
      lhsav  = HLDFLG
      ierr   = 0
      if (kflg .eq. 0) then
          LNRTOL(3) = LNRTOL(1)
          if (IRAP .eq. 1 .or. IRAP .eq. 2) LNRTOL(3) = LNRTOL(2)
          if (LNRTOL(3) .eq. 0.) go to 7000
          ifir = 1
          kfln = 1
          IFLNRS = 0
          MTVEXC = 0
          NROLNM = 0
          KERRSV = kerr
          if (kerr .ne. 0) irro = 0
      end if
      if (KERRSV .eq. 3) irro = 0
c
c...reset feed rate if changed
c...remove test of HLDFLG for FSR 60834 - IJD 26-JUL-2004
c     if (kflg .gt. 0 .and. HLDFLG .eq. 0) then
c
      if (kflg .gt. 0) then
          if (IFLNRS .eq. 1) call raprst
          if (IFLNRS .eq. 2) call linfed (5,RETFED)
      end if
c
c...save ROTSTO for motion
c
      if (HLDFLG .eq. 1) then
          call cpyrot (HLDROT,rots)
      else
          call cpyrot (ROTSTO,rots)
      end if
c
c...see if any rotary axis is active
c
      if (NROT .eq. 0) go to 7000
c
c...switch entry if subsequent call or in retract procedure
c
      if (kflg .lt. 0) go to 3000
      if (kflg .eq. 0) go to 200
      if (kfln .eq. 1) go to 500
      if (kfln .ge. 5) go to 710
c
c...get last point from stack
c
      call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
      call copyn (ROTBAL,ROTBAS,3)
      if (MACHTP .ne. 3) ROTBAS(4) = ROTBAL(4)
      go to 7000
c
c...Initial call, get current point vars
c
  200 call gtlpos
      if (MACHTP .eq. 3) call gtfwvc (MCHNUM(1,2),TLVEC,
     -                  STONUL(1,2),VECSAL,FWDDIR,IFWDFL)
      LINERR = 0
      call tvr1ck (VECSAL,iadj)
      call tvr1ck (TLVECL,iadj)
      call getlan (VECSAL,TLVECL,ASTART,AFINAL,ADELTA,FIANG,THETA,
     -             TVMPVC)
      ALNSTO = ASTART
c
c...See if move needs linearization, if not make sure that
c...tlaxis routine returns values for the adjusted TV, since
c...original might cause error.
c
      if (dabs (ADELTA) .gt. .5*VTOLER) go to 220
  210    call copyn (ROTBSV,ROTBAS,4)
         if (MDTLIN .ne. 0) go to 7000
         call tlaxis (MCHNUM,TLVECL,LINAXS,AXSOUT,ROTANG,ROTBAS,1,0,
     -                ierr)
         go to 7000
c
c...Store last point to move to
c...and user defined direction
c
  220 irfx   = IRFNLS
      HLDFLG = 0
      call copyn (MCHNUL(1,2),PTNXLN(1,2),3)
      call setijk (AFINAL,TVNXLN,FIANG,THETA)
      call copyn (ROTBSV,ROTBAL,4)
      call copyn (ROTBAS,RBNXLN,4)
      call copynk (IRTNLS,irnx,4)
c
c...Get maximum angle increment for linearized move
c...and spans
c
  500 call getnpt (STONUL(1,2),ROTSTL,ANGMAX)
      DLTANG(ir1) = ANGMAX(ir1)
      if (NROT .eq. 2) DLTANG(ir2) = ANGMAX(ir2)
c
      XSPAN  = (PTNXLN(1,2) - STONUL(1,2))
      YSPAN  = (PTNXLN(2,2) - STONUL(2,2))
      ZSPAN  = (PTNXLN(3,2) - STONUL(3,2))
      ASPAN  = ADELTA
      tasav  = ALNSTO
c
c...Check if 1st rotary axis must move first
c...14-apr-96 this fix problem where 1st axis rotates due to
c...limit error but tlaxis doesn't set kerr=3 when short route
c...is based on one rotary axis movement.
c
      if (ifir .eq. 1 .or. IMAXFL .eq. 1) then
         call tvprtb (ROTSTL,nper)
         call chktvmp (ip2,TVMPVC,i)
         if (i .eq. 0) nper = 0
      end if
      if (nper .eq. ip1) then
          if (IMAXFL .eq. 1) then
             call cpyrot (ROTSTL,ROTSTO)
             call getdvt (IDRSAV,0,RATIT)
             call tlaxis (MCHNUL,TLVECL,mlax,mxout,
     -                    ROTCUR,ROTBAL,0,irro,ierr)
          end if
          dan = dabs(ROTCUR(ip1,2) - ROTSTL(ip1,2))
          if (dan .gt. .001 .and. dan .gt. ANGMAX(ir1)) then
             call setrot1 (ROTSTL,RTOVLN,BAOVLN,TVOVLN,TVLMLN)
c
c...next statement was not in any release, and I dont know why I put
c...it here. It was used in Sep 96 and makes problem when rotating
c...first axis before moving second rotary axis.
cccc             if (kflg .eq. 0) call copyn (ROTCUR,ROTANL,8)
             nvrt = 6 + ir1
             go to 1300
          else
             nper = 0
          end if
      end if
c
c...Check change of rotary axis direction
c...and set up point if not last
c
      nlop   = 0
  700 if (ADELTA .lt. FUZZM) then
          ifl    = 0
          RATIT  = 1.d0
      else
          call dirchk (IDRSAV,ifl,RATIT,nerr)
          if (ifir .eq. 1 .and. ifl .eq. 0) call copyn (ROTBSV,ROTBAL,4)
          if (nerr .ne. 0) irfx = 0
      end if
      if (ifir .eq. 1 .and. ifl .eq. 0 .and.
     -    KERRSV .eq. 0) go to 210
c    -    KERRSV .ne. 3 .and. LRTRCT .eq. 0) go to 7000
c
c...Restore user difined direction and
c...last rotary position
c
      if (irfx .eq. 1 .and. ifir .eq. 1) then
          IRTNXF = 1
          IRTNXT(ir1) = irnx(ir1)
          if (NROT .eq. 2) IRTNXT(ir2) = irnx(ir2)
      end if
  710 kflg   = 1
      if (ADELTA .lt. FUZZM) then
          ifl    = 0
          RATIT  = 1.d0
      end if
      call cpyrot (ROTSTL,ROTSTO)
      call copyn (ROTBAL,bass,4)
c
c...Process Tool vector on the I-st axis
c
      if (INFRST .eq. 1 .and. MPVSEC .ne. 0) then
          call setbas (ROTSTL(1,2),ROTBAL)
          call gtfrdv (IDRSAV,ifl,kfln)
          call tlaxis (MCHNUL,TLVECL,mlax,mxout,ROTANL,ROTBAL,1,
     -                 irro,ierr)
c
c......Restore faked old rotary axes
c
          call cpyrot (ROTSTL,ROTSTO)
      else
c
c...Process normal moving tool vector
c
          call getdvt (IDRSAV,ifl,RATIT)
          ADELTA = ADELTA * (1.d0 - RATIT)
          if (MTVEXC .eq. 1 .and. ADELTA .eq. 0.)
     -         call copyn (TVMAXL,TLVECL,3)
          call tlaxis (MCHNUL,TLVECL,mlax,mxout,ROTANL,ROTBAL,1,
     -                 irro,ierr)
      end if
      if (ierr .eq. 3) then
         call psterr (2,'LONGRTE','NOTDESIR',-1)
         go to 800
      endif
c
c...Check if change of angles at the new point
c...is not worse than at the old
c
      call getnpt (MCHNUL(1,2),ROTANL,anewd)
      dan1   = dabs (ROTSTO(ip1,2) - ROTANL(ip1,2))
      dan2   = dabs (ROTSTO(ip2,2) - ROTANL(ip2,2))
c
c...vp 10-Jun-94 replaced ANGMAX by DLTANG to solve loop
c...when iterating (test: ence2.pp)
c
      if (anewd(ir1) .lt. ANGMAX(ir1)) DLTANG(ir1) = anewd(ir1)
      if (dan1 .gt. 1.1 * ANGMAX(ir1) .and. nlop .lt. 10) then
          ROTBAL(ir1) = bass(ir1)
          if (NROT .eq. 2) then
             if (anewd(ir2) .lt. ANGMAX(ir2)) ANGMAX(ir2) = anewd(ir2)
             DLTANG(ir1) = DLTANG(ir1) * ANGMAX(ir1) / dan1
             if (dan2 .gt. 1.1*ANGMAX(ir2))
     -            DLTANG(ir2) = DLTANG(ir2) * ANGMAX(ir2) / dan2
             ROTBAL(ir2) = bass(ir2)
          end if
          IMAXFL = 0
          ALNSTO = tasav
          ADELTA = ASPAN
          nlop   = nlop   + 1
          go to 700
      end if
c
c...handling MTVEXC: end of first TVMP move, get end point
c...and tool vector and redifine TVMP
c
      if (MTVEXC .eq. 1 .and. ADELTA .eq. 0.) then
          call copyn (PTNXLM,PTNXLN(1,2),3)
          call copyn (TVNXLM,TVNXLN,3)
          MTVEXC = 0
          call getlan (TLVECL,TVNXLN,ASTART,AFINAL,ADELTA,FIANG,THETA,
     -             TVMPVC)
          ALNSTO = ASTART
      end if
      go to 2000
c
  800 LINERR = LINERR + 1
c
c...second error, output last point &
c...stop linearization
c
      if (LINERR .lt. 2) go to 1200
      call copyn (AXSNXT,mxout,10)
      go to 2000
c
c...a limit has been passed.
c...get point exactly on limit,
c...process depending on retract requirement
c
 1200 call limpnt (tasav,RTOVLN,BAOVLN,TVOVLN,TVLMLN,mxout,nvrt)
 1300 if (LRTRCT .ne. 0) then
          kfln   = 1
          IFLNRS = 1
      else
          ROLMAX = 360.
          nr     = 0
          if (nvrt .gt. 6) then
              call tvprtb (ROTANL(1,1),nr)
              if (nr .eq. ip1) then
                  call getnpt (MCHNUL(1,2),ROTANL,anewd)
                  ROLMAX = anewd(ir1)
                  call copyn (ROTBSV,ROLBSV,4)
              end if
          end if
          if (nr .eq. 0) call psterr (3,'RETRDISA','DEMPART',-1)
          kfln   = 11
      end if
c
      kflg   = -1
      ifl    = 1
      call whchax (mxout,iaxs,nuax)
      if (ierr .eq. 3 .and. nuax .eq. 0) go to 3100
      if (nper .ne. 0) then
         kflg = -kfln
         go to 3000
      end if
c
c...Copy 1-st point axes to AXSOUT
c......before calculating 2-nd point
c
 2000 if (ifir .eq. 1) then
          call copyn (mxout,AXSOUT,10)
          call savpos
          ifir   = 2
          IRFNLS = 0
          if (kflg .gt. 0) then
              if (kfln .eq. 5) go to 710
              go to 500
          else
              kflg = -kfln
              go to 3000
          end if
      else
c
c...get previous point axes
c...and put on stack current point if not last,
c
          if (ifir .eq. 0) then
             call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
          else
             call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
          end if
          call pshaxs (mxout,TLVEC)
          ifir = 0
      end if
c
c...Set reentry flag
c
 2200 if (kflg .lt. 0) then
          kflg = -kfln
      else
          kflg = 1
      end if
      go to 8000
c
c...Retract procedure active (ifl: 1,2,3,4)
c
 3000 ifl    = - kflg
 3100 if (ifl .eq. 1) then
c
c......generate point at retract plane
c......and save plunge point
c
          call retpnt (PTLPLU,mxout)
      else if (ifl .eq. 2) then
c
c......rotate rotary axis at complementary angles
c
          call compnt (RTOVLN,BAOVLN,TVLMLN,mxout)
          call linfed (1,RETFED)
      else if (ifl .eq. 3) then
c
c......plunge back to point
c
          call copyn (PTLPLU,MCHNUL(1,2),3)
          call alladj (MCHNUL,LINAXS,mxout,ROTSTL,2,5)
          call linfed (3,RETFED)
      else if (ifl .eq. 4) then
c
c......end of retract sequence, reset all
c
          call redefp
          call raprst
          call linfed (2,RETFED)
          ifir   = 0
          kfln   = 1
          go to 500
c
c...Linearization without retract (ifl: 11,12)
c
      else if (ifl .eq. 11) then
          if (NROLNM .eq. 0) then
              call compnt (RTOVLN,BAOVLN,TVLMLN,mxout)
              call compnt1 (0,kfln,mxout)
          end if
          call compnt1 (1,kfln,mxout)
      else if (ifl .eq. 12) then
c
c......Reset after rotation of the 1-st axis
c
          call redefp
          kfln   = 1
          go to 500
      end if
      kfln   = kfln + 1
      go to 2000
c
c...Linearization not required
c...or last point of linearization
c
 7000 kflg   = 0
      go to 8800
c
c...save current point as old for next call
c...restore ROTSTO for motion
c
 8000 if (ifl .eq. 0) then
          kfln = 0
      else
          call savpos
      end if
      call cpyrot (rots,ROTSTO)
c
 8800 HLDFLG = lhsav
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: getnpt (glsto,gasto,gdlt)
c
c     FUNCTION:  This routine defines the maximum rotation of each
c                rotary axis to generate multiaxis motion with tool end
c                point not exceeding the required tolerance
c                (linearization is on)
c
c     INPUT:  glsto   R*8  D3    -  last cl point
c
c             gasto   R*8  D20.2 -  last rotary position
c
c     OUTPUT: gdlt    R*8  D4    -  maximum delta angle for each axis
c
c***********************************************************************
c
      subroutine getnpt (glsto,gasto,gdlt)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (NOPIVS,KPOSMP(1282))
      equivalence (IRTACT,KPOSMP(1256))
      equivalence (IFHEAD,KPOSMP(1281)), (NOTABS,KPOSMP(1283))
      equivalence (LASTAB,KPOSMP(1260)), (NCONTB,KPOSMP(1347))
      equivalence (NROT  ,KPOSMP(1366))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTDEF,KPOSMP(1485))
      equivalence (IRTWRK,KPOSMP(1506)), (NROTA ,KPOSMP(1725))
      equivalence (ITP   ,KPOSMP(1801)), (NOTOOL,KPOSMP(1802))
c
      integer*4 IRTYPE(20),IRTWRK(20),IRTNUM,ITP,NOTOOL,NOPIVS,IFHEAD,
     -          NOTABS,IRDEAD(20),LASTAB,NROTA(4),NCONTB,IRTDEF,
     2          IRTACT(2),NROT
c
      equivalence (RAD   ,POSMAP(0002)), (PI    ,POSMAP(0001))
      equivalence (TL    ,POSMAP(3601)), (LNRTOL,POSMAP(2251))
      equivalence (SPIVEC,POSMAP(3583)), (TABORG,POSMAP(5374))
c
      real*8 TL(120),PI,RAD,TABORG(3,20),LNRTOL(3),SPIVEC(3)
c
      integer*4 inx(6),kax,ktimes,is1,is2,i,j,inc
c
      real*8 glsto(3),gasto(20,2),gdlt(4),newsid,cosa,rad1,tang,
     -       pvr1(3),prtps(3)
c
      data inx /3,2,3,1,1,2/
c
      ktimes = 1
c      NROTA(1) = 1
      do 110 i=1,3
          pvr1(i) = TL(ITP) * SPIVEC(i)
cc          if (NOTOOL .eq. 1) then
cc              pvr1(i) = TL(ITP) * SPIVEC(i)
cc          else
cc              pvr1(i) = 0.d0
cc          end if
  110 continue
      do 115 i= 1,IRTDEF,1
          gdlt(i) = 360.0
  115 continue
c
c...select method according to rotary type
c
cc      if (IFHEAD .ne. 1 .or. NOPIVS .ne. 1) go to 500
      if (IFHEAD .ne. 1) go to 500
c
c...Heads
c......Walk thru all heads from rider to carrier
c
      inc = NROT + 1
      do 290 i= IRTNUM,LASTAB+1,-1
          kax = IRTWRK(i)
          is1 = inx(kax+kax-1)
          is2 = inx(kax+kax)
c
c......Add center point components
c
          do 220 j=1,3
              pvr1(j) = pvr1(j) + TABORG(j,i)
  220     continue
c
c......Get median head rotation radius
c......using last point and current point rads
c
          if (IRDEAD(i) .ne. 0) go to 280
c
c......Get rotation radius for the starting point
c
          rad1 = dsqrt (pvr1(is1)**2 + pvr1(is2)**2)
c
c...Get the maximum delta angle when under/over-cut
c...still is in tolerance
c
          newsid = rad1 - LNRTOL(3)
          tang   = 360.
          if (newsid .gt. 0.) tang = 2. * RAD * dacos (newsid/rad1)
          inc = inc - 1
          gdlt(IRTACT(inc)) = tang
c
c...adjust backward pivot radius for this axis
c...to use with carier if any
c
  280     if (i .ne. LASTAB+1) then
              cosa = 360.d0 - gasto(i,1)
              call vecadj (pvr1,pvr1,cosa,kax)
          end if
  290 continue
c
c...Tables from rider to carrier
c
  500 if (NOTABS .eq. 2 .or. LASTAB .eq. 0) go to 8000
      prtps(1) = glsto(1)
      prtps(2) = glsto(2)
      prtps(3) = glsto(3)
      inc = 0
      do 390 i=1,LASTAB,1
c
c...select indexes for rotary plane & origin
c
          kax = IRTWRK(i)
          is1 = inx(kax+kax-1)
          is2 = inx(kax+kax)
          if (IRDEAD(i) .ne. 0) then
              if (NCONTB .eq. 2) then
                  go to 390
              else
                  go to 380
              end if
          end if
c
c......Get table rotation radius for last point
c
          rad1 = dsqrt ((prtps(is1) - TABORG(is1,i))**2 +
     -                  (prtps(is2) - TABORG(is2,i))**2)
c
c...get maximum delta rotation for specified tolerance
c
          newsid = rad1 - LNRTOL(3)
          tang   = 360.
          if (newsid .gt. 0.) tang = 2. * RAD * dacos (newsid/rad1)
          inc    = inc    + 1
          gdlt(IRTACT(inc)) = tang
c
c...adjust points' position and control point offset for this axis
c...to use with carrier if any
c
  380     if (i .ne. LASTAB) then
              cosa = 360.d0 - gasto(i,1)
              call axadj (prtps,prtps,cosa,TABORG(1,i),kax)
          end if
  390 continue
c
c...end of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: genlpt (knum,glstp,glstv,gratio)
c
c     FUNCTION:  This routine generates cl point for linearized motion.
c
c     INPUT:   knum     I*4  D1  -  intermediate point counter.
c
c              glstp    R*8  D3  -  destination cl point coordinates.
c
c              glstv    R*8  D3  -  destination tool vector.
c
c              gratio   R*8  D3  -  factor to apply with spans.
c
c     OUTPUT:  none              -  in common block
c
c***********************************************************************
c
      subroutine genlpt (knum,glstp,glstv,gratio)
c
      include 'lintol.inc'
c
      integer*4 knum
c
      real*8 glstp(3),glstv(3),gratio
c
c...last counter,
c.......get destination point
c
      if (knum .eq. 2) then
          call copyn (glstp,MCHNUL(1,2),3)
          call copyn (glstv,TLVECL,3)
c
c...calculate linearized point
c
      else
          MCHNUL(1,2) = STONUL(1,2) + gratio * XSPAN
          MCHNUL(2,2) = STONUL(2,2) + gratio * YSPAN
          MCHNUL(3,2) = STONUL(3,2) + gratio * ZSPAN
          ALNSTO = ALNSTO + gratio * ASPAN
          call setijk (ALNSTO,TLVECL,FIANG,THETA)
      end if
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  limpnt (gangl,grotov,gbanov,gvecov,gveclm,gxout,kraxs)
c
c     FUNCTION:  This routine generates cl point and machine output axes
c                at limit when moving toward it.
c
c     INPUT:   ganlg    R*8  D1  -  last tool vector position on TVMP
c
c     OUTPUT:  gbanov   R*8  D4  -  rotary axes base angles at the point
c                                   over a limit.
c
c              gvecov   R*8  D3  -  tool vector at the point over limit.
c
c              gveclm   R*8  D3  -  tool vector at the point on limit.
c
c              gxout    R*8  D10 -  output axes at the point on limit.
c
c***********************************************************************
c
      subroutine limpnt (gangl,grotov,gbanov,gvecov,gveclm,gxout,kraxs)
c
      include 'lintol.inc'
c
      real*8 gangl,grotov(20,2),gbanov(4),gvecov(3),gveclm(3),gxout(10)
c
      integer*4 kraxs
c
c...get base angles for the correct
c...point over limit
c
      call copyn (ROTBAL,gbanov,4)
      call cpyrot (ROTANL,grotov)
c
c...recover previous tool vector angle on TVMP,
c...get tool vector at the point over limit
c
      ALNSTO = gangl
      call copyn (TLVECL,gvecov,3)
c
c...get exact point and tool vector on limit
c...vp 12.21.93 make sure RATMAX is set
c
      if (RATMAX .eq. 0.0) RATMAX = RATIT
      call iterlm (gxout,kraxs)
      call copyn (TLVECL,gveclm,3)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  setrot1 (grotov,gbanov,gvecov,gveclm,gxout,kraxs)
c
c     FUNCTION:  This routine generates cl point and machine output axes
c                at limit when moving toward it.
c
c     INPUT:   ganlg    R*8  D1  -  last tool vector position on TVMP
c
c     OUTPUT:  gbanov   R*8  D4  -  rotary axes base angles at the point
c                                   over a limit.
c
c              gvecov   R*8  D3  -  tool vector at the point over limit.
c
c              gveclm   R*8  D3  -  tool vector at the point on limit.
c
c              gxout    R*8  D10 -  output axes at the point on limit.
c
c***********************************************************************
c
      subroutine setrot1 (grsav,grotov,gbanov,gvecov,gveclm)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTACT,KPOSMP(1256)), (IRTINC,KPOSMP(1461))
c
      integer*4 IRTACT(2),IRTINC(4)
c
      equivalence (ROTBAS,POSMAP(1435)), (ROTANG,POSMAP(5173))
c
      real*8 ROTANG(20,2),ROTBAS(4)
c
      real*8 grsav(20,2),grotov(20,2),gbanov(4),gvecov(3),gveclm(3)
c
      integer*4 i
c
c...get base angle of the 1st axis for the next point
c
      call copyn (ROTBAL,gbanov,4)
      call cpyrot (grsav,grotov)
      i      = IRTINC(IRTACT(1))
      grotov(i,1) = ROTCUR(i,1)
      grotov(i,2) = ROTCUR(i,2)
c
c...recover previous tool vector angle on TVMP,
c...get tool vector at the point over limit
c
      call copyn (TLVECL,gvecov,3)
      call copyn (VECSAL,TLVECL,3)
      call copyn (STONUL(1,2),MCHNUL(1,2),3)
c
c...get exact point and tool vector on limit
c
      call copyn (VECSAL,gveclm,3)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  retpnt (gptplu,gxout)
c
c     FUNCTION:  This routine generates cl point and output axes on
c                retract plane at the limit point.
c
c     INPUT:   none
c
c     OUTPUT:  gptplu   R*8  D3  -  plunge point cl coordinates.
c
c              gxout    R*8  D10 -  output axes at the retract point.
c
c***********************************************************************
c
      subroutine retpnt (gptplu,gxout)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (ITP   ,KPOSMP(1801))
c
      integer*4 ITP
c
      equivalence (TL    ,POSMAP(3601))
c
      real*8 TL(120)
c
      real*8 gptplu(3),gxout(10),rdis,mlax(6)
c
      integer*4 i
c
c...get retract distance
c
      call mwrdis (STONUL,ROTSTL,rdis)
c
c...save plunge cl point first
c...get point at retract distance
c
      do 120 i=1,3
         gptplu(i) = STONUL(i,2)
         MCHNUL(i,2) = STONUL(i,2) + rdis * VECSAL(i)
  120 continue
c
c...get output axes
c
      call alladj (MCHNUL,mlax,gxout,ROTSTL,2,5)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  compnt (grotov,gbanov,gveclm,gxout)
c
c     FUNCTION:  This routine unwinds rotary axes at the retract point
c                to continue motion in limits.
c
c     INPUT:   grotov   R*8  D20.2-  rotaty axes at the over limit point.
c
c              gbanov   R*8  D4   -  base angles at the over limit point.
c
c              gveclm   R*8  D3   -  tool vector at the limit point.
c
c     OUTPUT:  gxout    R*8  D10  -  output axes at the retract point with
c                                    new rotary axes position.
c
c***********************************************************************
c
      subroutine compnt (grotov,gbanov,gveclm,gxout)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (HLDFLG,KPOSMP(1622))
c
      integer*4 HLDFLG
c
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2)
c
      real*8 grotov(20,2),gveclm(3),gbanov(4),gxout(10),mlax(6)
c
      integer*4 ierr,i
c
c...use angles for over limit point
c...with rotary axes satisfying limits
c
      call cpyrot (ROTSTO,ROLSTO)
c
c...make sure that held back is not ON so
c...ROTSTO will be used
c
      i      = HLDFLG
      HLDFLG = 0
      call cpyrot (grotov,ROTSTO)
      call cpyrot (grotov,ROTSTL)
      call copyn (gbanov,ROTBAL,4)
c
c...get point on limit moving from right
c
      call tlaxis (MCHNUL,gveclm,mlax,gxout,ROTANL,ROTBAL,1,0,ierr)
      HLDFLG = i
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  redefp
c
c     FUNCTION:  This routine redefines Tool Vector Motion Plane after
c                tool has plunged at a limit point.
c
c     INPUT:   none
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine redefp
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IMAXFL,KPOSMP(1729))
c
      integer*4 IMAXFL
c
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 ROTANG(20,2),ROTSTO(20,2)
c
      integer*4 ierr,iadj
c
      real*8 mlax(10),mxout(10)
c
c...set variables for the last point of linearization
c
      call cpyrot (ROTSTL,ROTSTO)
      call copyn (BAOVLN,ROTBAL,4)
      call copyn (TVNXLN,TLVECL,3)
      call copyn (PTNXLN,MCHNUL(1,2),3)
      call tlaxis (MCHNUL,TLVECL,mlax,mxout,RTNXLN,ROTBAL,0,0,ierr)
c
c...Redefine TVMP to get starting angle for tool vector
c
      call tvr1ck (VECSAL,iadj)
      call tvr1ck (TLVECL,iadj)
      call getlan (VECSAL,TLVECL,ASTART,AFINAL,ADELTA,FIANG,THETA,
     -             TVMPVC)
      ALNSTO = ASTART
      IMAXFL = 0
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  gtlpos
c
c     FUNCTION:  This routine stores point variables in local arrays for
c                use in linearization routines.
c
c     INPUT:   none
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine gtlpos
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (HLDFLG,KPOSMP(1622))
c
      integer*4 HLDFLG
c
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435)), (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387)), (ROTBSV,POSMAP(2275))
      equivalence (AXSOUT,POSMAP(1340)), (HLDMCH,POSMAP(2141))
      equivalence (HLDVEC,POSMAP(2177)), (HLDAXS,POSMAP(2167))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (HLDROT,POSMAP(5253))
c
      real*8 MCHNUM(3,4),VECSAV(3),ROTANG(20,2),STONUM(3,4),ROTBSV(4),
     -       ROTSTO(20,2),TLVEC(3),ROTBAS(4),AXSOUT(10),HLDAXS(10),
     -       HLDROT(20,2),HLDMCH(3,4),HLDVEC(3)
c
c...Store last point variables
c
      call copyn (ROTBAS,ROTBAL,4)
      call cpyrot (ROTANG,ROTCUR)
      call cpyrot (RTNXLN,ROTANL)
c
c...Store machine axis
c
      call copyn (AXSOUT,AXSNXT,10)
c
c...Store held back position variables
c
      if (HLDFLG .eq. 1) then
c         call copyn (HLDVEC,VECSAL,3)
          call copyn (TLVEC,TLVECL,3)
          call copyn (MCHNUM(1,2),MCHNUL(1,2),6)
          call copyn (HLDMCH(1,2),STONUL(1,2),6)
          call cpyrot (HLDROT,ROTSTL)
          call cpyrot (HLDROT,ROTSTO)
      else
c
c...Store current position variables
c
c         call copyn (VECSAV,VECSAL,3)
          call copyn (TLVEC,TLVECL,3)
          call copyn (MCHNUM(1,2),MCHNUL(1,2),6)
          call copyn (STONUM(1,2),STONUL(1,2),6)
          call cpyrot (ROTSTO,ROTSTL)
      end if
      call getijk (ROTSTL(1,2),VECSAL)
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: iterlm (gxout,kraxs)
c
c     FUNCTION:  This routine defines cl point which machine coordinates
c                do not violate any limit but is exactly on limit.
c                To approach limit there are used iterations (half division
c                method).
c
c     INPUT:   none              -  in common block
c
c     OUTPUT:  gxout    R*8  D10 -  output axis adjusted for TRANS/-AXIS.
c
c***********************************************************************
c
      subroutine iterlm (gxout,kraxs)
c
      include 'post.inc'
      include 'lintol.inc'
c
      integer*4 kraxs
c
      equivalence (MOTREG,KPOSMP(0381)), (IRTNXF,KPOSMP(1365))
c
      integer*4 MOTREG(24),IRTNXF
c
      equivalence (LIMITS,POSMAP(1254))
      equivalence (ROTBSV,POSMAP(2275)), (FUZZM ,POSMAP(4914))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 ROTBSV(4),ROTSTO(20,2),LIMITS(2,10),FUZZM
c
      integer*4 kaxs(10),iout,ierr,lerx(10),ibord(10),iax(10),
     -          lerr,lern(10),inum
c
      real*8 glin(6),gxout(10),ratio,mult,rsig,border(10),angl,
     1       rnum,rtp,rout(10)
c
      integer*4 i,j,iflg,inxtsv
c
      data iax /1,3,5,7,9,11,13,16,19,22/
c
c...recover erroneous point moving over limits
c
      inxtsv = IRTNXF
      call cpyrot (ROTSTL,ROTSTO)
      IRTNXF = 2
      call tlaxis (MCHNUL,TLVECL,glin,gxout,ROTANL,ROTBSV,0,0,ierr)
c
c...store all axis over limits and its limit values
c
      call lmtchk (gxout,iout,kaxs,0)
      lerr   = 0
      kraxs  = 0
      do 110 i=1,10,1
          lerx(i) = kaxs(i)
          if (kaxs(i) .ne. 0) then
              lerr = lerr + 1
              call codint (MOTREG(iax(i)),LIMITS(kaxs(i),i),
     -                     border(i),ibord(i))
              lern(lerr) = i
          end if
  110 continue
      mult   = 1.d0
      rsig   = -1.d0
      ratio  = 1.d0
c
c...iterate until it makes sense
c
  200 mult   = mult * 2.d0
      if (ratio/mult .lt. FUZZM) go to 600
      ratio  = ratio + rsig / mult
      rtp    = ratio * RATMAX
c
c...set up cl point and generate its machine immage
c
      MCHNUL(1,2) = STONUL(1,2) + rtp * XSPAN
      MCHNUL(2,2) = STONUL(2,2) + rtp * YSPAN
      MCHNUL(3,2) = STONUL(3,2) + rtp * ZSPAN
      angl   = ALNSTO + rtp * ASPAN
      call setijk (angl,TLVECL,FIANG,THETA)
      IRTNXF = 2
      call tlaxis (MCHNUL,TLVECL,glin,gxout,ROTANL,ROTBSV,0,0,ierr)
c
c...check if still out of limits
c...set up direction of change
c...turn off flag for axis which is OK now
c
      call lmtchk (gxout,iout,kaxs,0)
      if (iout .ne. 0) then
          if (lerr .eq. 1 .and. lern(1) .gt. 6) kraxs = lern(1)
          rsig = -1.d0
          do 210 j=1,lerr
              i = lern(j)
              if (i .ne. 0) then
                  if (kaxs(i) .eq. 0) lern(j) = 0
              end if
  210     continue
      else
          rsig = 1.d0
      end if
      iflg = 1
c
c...if in limits make sure that
c...point is not already exactly on limit
c
      call axsxfr (gxout,rout)
      if (iout .eq. 0) then
          iflg = 0
          do 220 j=1,lerr
              i = lern(j)
              if (i .ne. 0) then
                  call codint (MOTREG(iax(i)),rout(i),rnum,inum)
                  if (inum .ne. ibord(i)) iflg = iflg + 1
              end if
  220     continue
      end if
c
c...improve accuracy if insuficient
c
      if (iflg .ne. 0) go to 200
      go to 8000
c
c...iteration stoped before accuracy is reached
c...set limit for out of limit output axis
c
  600 do 620 j=1,lerr
          i = lern(j)
          if (i .ne. 0) rout(i) = border(i)
  620 continue
      call axsxfm (rout,gxout)
c
c...end of routine
c
 8000 IRTNXF = inxtsv
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  savpos
c
c     FUNCTION:  This routine saves point variables in local arrays
c                AFTER move for use in linearization routines.
c
c     INPUT:   none
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine savpos
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (ROTBSV,POSMAP(2275))
c
      real*8 ROTBSV(4)
c
c...save current point variables
c
      call copyn (MCHNUL(1,2),STONUL(1,2),6)
      call copyn (TLVECL,VECSAL,3)
      call copyn (ROTBAL,ROTBSV,4)
      call cpyrot (ROTANL,ROTSTL)
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  linfed (kflg,gfed)
c
c     FUNCTION:  This routine sets feed rate to retract/plunge tool
c                when a limit is reached in linearized move.
c
c     INPUT:   kflg     I*4  D1  -  1 = retract feed, 2 = plunge feed,
c                                   3 = set rapid,    4 = reset rapid
c                                   5 = reset feed rate.
c                                   6 = Blade positioning feed.
c                                   7 = Tool shift feed.
c
c              gfed     R*8  D2  -  Feed rates to applay: 1 - retract,
c                                   2 - plunge feed rate.
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine linfed (kflg,gfed)
c
      include 'post.inc'
c
      integer*4 kflg
c
      real*8 gfed(3)
c
      equivalence (IRAPDO,KPOSMP(3220)), (IRPSAV,KPOSMP(3204))
      equivalence (IRPSUP,KPOSMP(3189)), (INCR  ,KPOSMP(1226))
      equivalence (IFITYP,KPOSMP(3150)), (IFSVLN,KPOSMP(3207))
      equivalence (IRAP  ,KPOSMP(3199)), (IFLNRS,KPOSMP(1731))
c
      integer*4 IRPSAV,IRAP,IRAPDO(8),IRPSUP,IFITYP,INCR,IFSVLN,IFLNRS
c
      equivalence (FDSVLN,POSMAP(2377))
      equivalence (RAPDIS,POSMAP(3582)), (PFEED ,POSMAP(3540))
c
      real*8 FDSVLN,RAPDIS,PFEED(4)
c
      integer*4 inx
c
c...set index for retract or plunge feed
c
      if (kflg .lt. 3) then
          inx = kflg
      else if (kflg .eq. 6) then
          inx = 3
      else if (kflg .eq. 7) then
          inx = 4
      else
          inx = 2
      end if
c
c...Set up rapid or feed rate
c
      if (kflg .ne. 4. .and. kflg .ne. 5) then
          if (kflg .eq. 1) then
              IFSVLN = IFITYP
              FDSVLN = PFEED(1)
          endif
          if (kflg .eq. 3 .or. gfed(inx) .eq. 0.) then
              IRPSAV = INCR
              IRAP   = IRPSUP
              IRAPDO(1) = 1
              IRAPDO(2) = 2
              IRAPDO(3) = 2
              IRAPDO(4) = 1
              IRAPDO(5) = 2
              RAPDIS = 0.
          else
              IFSVLN = IFITYP
              IFITYP = 1
              PFEED(1) = gfed(inx)
              IFLNRS = 2
          endif
      else
c
c......Reset rapid or feed rate
c
          if (kflg .eq. 4 .or. gfed(inx) .eq. 0.) then
              call raprst
          else
              IFITYP = IFSVLN
              PFEED(1) = FDSVLN
          endif
          IFLNRS = 0
      endif
c
c...end of routine
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE:  setrod (kflg,krot,kdirf,kerr)
c
c     FUNCTION:  This routine sets direrection flags to rotate axis
c                in user defined direction.
c
c     INPUT:   kflg     I*4  D1  -  Flag specifying if tool vector is
c                                   at the I-st rotary axis vector:
c                                   0 - different tool vector,
c                                   1 - tool vector is on the I-st
c                                   rotary axis.
c
c              krot     I*4  D4  -  Preset rotary directions based on the
c                                   shortest route: 1 - CLW, 2 - CCLW.
c
c     OUTPUT:  kdirf    I*4  D4  -  Output rotary directions.
c
c              kerr     I*4  D1  -  Error status: 0 - O.K., 1 - user
c                                   defined direction can not be fulfilled
c                                   to move tool on the TVMP.
c
c***********************************************************************
c
      subroutine setrod (kflg,krot,kdirf,kerr)
c
      include 'post.inc'
c
      equivalence (NROT  ,KPOSMP(1366)), (IRTACT,KPOSMP(1256))
      equivalence (IRTNLS,KPOSMP(1641)), (IRFNLS,KPOSMP(1640))
c
      integer*4 NROT,IRTACT(2),IRTNLS(4),IRFNLS
c
      integer*4 kflg,kdirf(4),krot(4),kerr
      integer*4 i,j
c
c...Check if forced direction is correct
c...otherwise set error flag
c
      do 1110 i=1,NROT
          j = IRTACT(i)
          kdirf(j) = krot(j)
          if (IRFNLS .eq. 1 .and. IRTNLS(j) .ne. 0) then
              if (kflg .eq. 0) then
                  if (krot(j) .ne. IRTNLS(j)) kerr = 1
              else
                  kdirf(j) = IRTNLS(j)
              end if
          end if
 1110 continue
c
 9000 return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  dirchk (krot,kfl,gratio,kerr)
c
c  FUNCTION:  This routine checks if the second rotary axis is
c             changing its rotary direction when moving to the last
c             point; if so then the position of the tool vector for
c             its turning value is defined.  The rotary movement for
c             each axis is checked against its limit required by
c             tolerance.
c
c             *NOTE* This routine is disabled for dual rotary heads.
c
c     INPUT:  none
c
c     OUTPUT: gratio   R*8  D1  -  Relative position of the tool vector
c                                  on the TVMP.
c
c             krot     I*4  D4  -  Rotary axis direction required for
c                                  continues cutting.
c
c             kfl      I*4  D1  -  Flag: 0 - no intermediate points are
c                                  required, 1 - needs generate interme-
c                                  diate point(s), 2 - turning point for
c                                  the second axis has been defined.
c
c             keer     I*4  D1  -  Error status: 0 - O.K., 1 - User
c                                  defined direction is can not satisfy
c                                  continues cutting motion.
c
c************************************************************************
c
      subroutine dirchk (krot,kfl,gratio,kerr)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (LASTAB,KPOSMP(1260))
      equivalence (IRTNXF,KPOSMP(1365)), (IRTNXT,KPOSMP(1361))
      equivalence (NROT  ,KPOSMP(1366)), (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461)), (IRTWRK,KPOSMP(1506))
      equivalence (IRTNLS,KPOSMP(1641)), (IRFNLS,KPOSMP(1640))
      equivalence (NROTA ,KPOSMP(1725)), (IMAXFL,KPOSMP(1729))
      equivalence (IRTSAV,KPOSMP(1730))
c
      integer*4 IMAXFL,NROTA(4),IRTSAV,IRTWRK(20),NROT,IRTACT(2),
     -          IRTNLS(4),IRFNLS,IRTNXF,IRTNXT(4),LASTAB,IRTINC(4)
c
      equivalence (VECTOL,POSMAP(1247))
      equivalence (ROTBSV,POSMAP(2275)), (ROTBAS,POSMAP(1435))
      equivalence (VECSAV,POSMAP(1372)), (VTOLER,POSMAP(4911))
      equivalence (FUZZ4 ,POSMAP(4912)), (FUZZ8 ,POSMAP(4913))
      equivalence (ROTSTO,POSMAP(5213)), (RTMAXN,POSMAP(5414))
c
      real*8 ROTBSV(4),ROTSTO(20,2),ROTBAS(4),RTMAXN(20,2),VECSAV(3),
     -       FUZZ4,FUZZ8,VTOLER,VECTOL
c
      integer*4 krot(4),kfl,kerr
c
      real*8 gratio
c
      integer*4 i,irot(4),ierr,ifl,ir1,ir2,nxt,irfx,irnx(4),ista1,
     -          ista2,iadj,ip1
c
      real*8 angd(4),basa(4),ratio,dlts,alnsav,ratm,r,da,tol,
     -       per(3),dst1,dst2,vec(3,2),firx(3),cosa(2),mlax(6),
     -       mxout(10),dlt,vec1(3),rang1,rang2
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
c...Initialize routine
c
      ifl    = 1
      alnsav = ALNSTO
      irfx   = IRFNLS
      nxt    = 0
      kerr   = 0
      ip1    = IRTINC(ir1)
      call cpyrot (ROTSTO,ROTSTL)
      do 110 i=1,4
          NROTA(i) = 0
          irot(i) = 3
          irnx(i) = IRTNLS(i)
          basa(i) = ROTBSV(i)
  110 continue
      da      = 90.
      tol     = 0.
c
c...handling MTVEXC: following calls of dirchk, redefine
c...RATMAX
c
      if (MTVEXC .eq. 1) then
          if (kfl .ne. 2) then
             RATMAX = (RATMAX - gratio) / (1.d0 - gratio)
          end if
          go to 1000
      end if
c
c...See if extremity was setup
c
      if (IMAXFL .ne. 0) then
          if (kfl .ne. 2 .and. IMAXFL .eq. 2) then
              RATMAX = (RATMAX - gratio) / (1.d0 - gratio)
              ratio  = RATMAX
          else
              ratio  = 1.d0
              call genlpt (1,PTNXLN(1,2),TVNXLN,ratio)
c
c...vp 7-apr-93 - force continous direction of the 1-st axis
c
              IRTNXF = 1
              IRTNXT(ir1) = IDRSAV(ir1)
              IRTNXT(ir2) = 0
              call tlaxis (MCHNUL,TLVECL,mlax,mxout,RTNXLN,ROTBAL,
     -                     0,0,ierr)
              if (ierr .eq. 3) call ovrlim (RTNXLN,ROTBAL)
          end if
          if (IMAXFL .eq. 1) then
              call rotlin (RTNXLN,ROTBAL,0,irot,angd)
          else
              call rotlin (RTMAXN,ROTBAL,0,irot,angd)
          end if
          if (angd(ir1) .eq. 0.d0) irot(ir1) = IDRSAV(ir1)
          go to 1000
      end if
c
c...Check if extremum exists
c
      firx(1) = 0.d0
      firx(2) = 0.d0
      firx(3) = 0.d0
      firx(IRTWRK(ip1)) = 1.d0
      call vecdad (firx,firx,1)
      ratm   = 1.d0
      dst1   = 0.
      dst2   = 0.
      call betvec (VECSAL,firx,dlts)
      dlt    = dlts
      if (dlts .gt. 90.0) dlts = 180.0 - dlts
c
c...vp 6-sep-96 make sure that tool vector is on 1-st rotary
c...axis if flag is set
c
      if (dlts .lt. VTOLER .or. dlts .lt. FUZZ4) then
          INFRST = 1
          call secchk (TVMPAN)
          if (MPVSEC .eq. 0) INFRST = 2
          if (INFRST .eq. 1) then
              call tvfix1 (firx,VECSAL,dlt,VTOLER,iadj)
ccc              call copyn (firx,VECSAL,3)
              go to 500
          end if
      else
          INFRST = 2
      end if
c
      call betvec (TVMPVC,firx,dlts)
      dlts = dabs (90.d0-dlts)
      if (dlts .lt. FUZZ4/2.) go to 500
c
c...Get plane perpto to TVPM and
c...going through the first rotary axis
c
      per(1) = firx(2)*TVMPVC(3) - firx(3)*TVMPVC(2)
      per(2) = firx(3)*TVMPVC(1) - firx(1)*TVMPVC(3)
      per(3) = firx(1)*TVMPVC(2) - firx(2)*TVMPVC(1)
c
c...Find the vector created by intersection of TVMP
c...and that plane. This vector is on the TVMP where
c...the second rotary axis has its minimum
c...vp 12/08/97 if planes are parallel exit gracefully
c
      call getpnt (TVMPVC,per,dst1,dst2,vec(1,1),vec(1,2),VECTOL,ierr)
      if (ierr .ne. 0) go to 500
c
c...Do not perform check tool is near apex point
c...Because this can cause the C-axis on an AC style head
c...to swing violently and also cause the rotary axes
c...to move in the wrong direction
c
      vec1(1) = 0.d0
      vec1(2) = 0.d0
      vec1(3) = 0.d0
      vec1(IRTWRK(1)) = 1.d0
      call betvec (VECSAL,vec1,rang1)
      call betvec (TLVECL,vec1,rang2)
      if ((rang1 .ge. -1. .and. rang1 .le. 1.) .or.
     1    (rang2 .ge. -1. .and. rang2 .le. 1.)) go to 500
c
c...Check if one of vectors is in motion area
c
      call intvec (vec(1,1),ALNSTO,AFINAL,FIANG,THETA,ista1)
      call intvec (vec(1,2),ALNSTO,AFINAL,FIANG,THETA,ista2)
      if (ista1+ista2 .eq. 0) go to 500
      if (ista1 .eq. 1) i = 1
      if (ista2 .eq. 1) i = 2
      call betvec (VECSAL,vec(1,i),cosa(i))
      if (cosa(i) .lt. .01*FUZZ4) nxt = 1
      ratm = cosa(i) / ADELTA
c
c...Check if TVMP is in vecinity of the first rotary
c...axis vector to set MTVEXC flag
c
      call betvec (vec(1,i),firx,da)
      r   = da
      if (da .gt. 90.) r = 180.0 - da
      tol = .5*FUZZ4
      if (r .lt. tol) MTVEXC = 1
c
c...Get tool vector for the extremum point
c
  500 ratio  = ratm
      call genlpt (1,PTNXLN(1,2),TVNXLN,ratio)

      if (irfx .eq. 1) then
          IRTNXF = irfx
          do 510 i=1,NROT
              IRTNXT(IRTACT(i)) = irnx(IRTACT(i))
  510     continue
      end if
c
c...Get rotary axis position and rotary delta motion
c...up to the extremity point and up to the last point
c
      if (ratio .lt. 1.) then
c
c...handling MTVEXC: TVMP is replace by 2 plane, 1-st from old TV
c...to the 1-st rotary axis vec, and 2-nd from this vector to the
c...next point TV, hance TVMP is redefined now.
c
          if (MTVEXC .eq. 1) then
              call tvfix1 (firx,TLVECL,da,tol,iadj)
              call getlan (VECSAL,TLVECL,ASTART,AFINAL,r,
     -                     FIANG,THETA,TVMPVC)
              ratio  = r / ADELTA
              ADELTA = r
              ASPAN  = r
              RATMAX = ratio
              call vcplvc (MCHNUL(1,2),STONUL(1,2),PSPAN,-1.d0)
              ratio  = 1.0
              alnsav = ASTART
              call copyn (TLVECL,TVMAXL,3)
          end if
c
          call tlaxis (MCHNUL,TLVECL,mlax,mxout,RTMAXN,basa,0,0,ierr)
          if (ierr .eq. 6) then
              kerr   = 1
              call tlaxis (MCHNUL,TLVECL,mlax,mxout,RTMAXN,basa,0,0,
     -                     ierr)
          end if
c
c...handling MTVEXC: save variables at the final point where 2-nd
c...TVMP ends
c
          if (MTVEXC .eq. 1) then
              call copyn (PTNXLN(1,2),PTNXLM,3)
              call copyn (TVNXLN,TVNXLM,3)
              call copyn (MCHNUL(1,2),PTNXLN(1,2),3)
              call copyn (TLVECL,TVNXLN,3)
          end if
c
          if (ierr .eq. 3 .and. INFRST .eq. 2)
     -          call ovrlim (RTMAXN,basa)
          call rotlin (RTMAXN,basa,1,irot,angd)
          ALNSTO = alnsav
          if (NROT .eq. 2 .and. (dabs(angd(ir1)) .gt. .0005 .or.
     -                           dabs(angd(ir2)) .gt. .0005)) then
              if (MTVEXC .ne. 1) IMAXFL = 2
          else
              ratio = 1.d0
              call genlpt (1,PTNXLN(1,2),TVNXLN,ratio)
              if (irfx .eq. 1) then
                  IRTNXF = irfx
                  do 520 i=1,NROT
                      IRTNXT(IRTACT(i)) = irnx(IRTACT(i))
  520             continue
              end if
          end if
      end if
c
c...Get rotary axis direction and rotary delta motion
c...up to the last point
c
      if (ratio .eq. 1.) then
          basa(ir1) = ROTBSV(ir1)
          irot(ir1) = 3
          if (NROT .eq. 2) then
              basa(ir2) = ROTBSV(ir2)
              irot(ir2) = 3
          end if
          call tlaxis (MCHNUL,TLVECL,mlax,mxout,RTNXLN,basa,0,0,ierr)
          if (ierr .eq. 6) then
              kerr   = 1
              call tlaxis (MCHNUL,TLVECL,mlax,mxout,RTNXLN,basa,0,0,
     -                     ierr)
          end if
          if (ierr .eq. 3 .and. INFRST .eq. 2)
     -          call ovrlim (RTNXLN,basa)
          call rotlin (RTNXLN,basa,1,irot,angd)
c
c...fix TVMPAN if limit error on 2-nd rotary axis does not allow
c...rotate 1-st axis the shortest way
c...vp 30-oct-96
c...before fix TVMPAN make sure that 1-st axis rotates so it will not
c...just rotate 180 degree without any reason.
c...The old code is following:
cc           if (MPVSEC .ne. irot(ir1)) then
c
          if (INFRST .eq. 1) then
             if (MPVSEC .ne. irot(ir1) .and. dabs(angd(ir1)) .gt. FUZZ4)
     -       then
                MPVSEC = irot(ir1)
                TVMPAN = 180. - TVMPAN
             end if
          end if
      end if
      IRTSAV = irot(ir1)
c
c...Check which axis moves over tolerance limit
c
 1000 if (dabs(angd(ir1)) .gt. ANGMAX(ir1)) NROTA(ir1) = 1
      if (NROT .eq. 2) then
          if (dabs(angd(ir2)) .gt. ANGMAX(ir2)) NROTA(ir2) = 1
      end if
c
c...Set up output flag and reentry flag
c
      if (NROTA(1)+NROTA(2)+NROTA(3)+NROTA(4) .eq. 0) then
          if (ratio .ne. 1.d0) then
              ifl    = 2
          else
              ifl    = 0
              ROTBAL(ir1) = basa(ir1)
              if (NROT .eq. 2) ROTBAL(ir2) = basa(ir2)
          end if
          if (IMAXFL .eq. 2) then
              IMAXFL = 1
          else
              IMAXFL = 0
          end if
      else
          if (ratio .ne. 1.d0) then
              IMAXFL = 2
          else
              IMAXFL = 1
          end if
          if (MTVEXC .eq. 1) then
              IMAXFL = 0
          else
              RATMAX = ratio
          end if
      end if
c
c...Reset variables and set direction
c
 1100 ALNSTO = alnsav
      gratio = ratio
      kfl    = ifl
      call setrod (nxt,irot,krot,kerr)
      if (kerr .eq. 0) go to 9000
c
c...Error,
c...forced direction cut is not on the TVMP
c
 8000 kerr   = 1
      call psterr (2,'ROTNOSAT','NOTDESIR',-1)
c
c...End of routine
c
 9000 return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  getdvt (krot,kflg,grat)
c
c  FUNCTION:  This routine defines a vector located on TVMP and at
c             specified angle from the last rotary position.
c
c     INPUT:  krot     I*4  D4  -  Rotary axis direction.
c
c             grat     R*8  D1  -  Max relative position available for
c                                  the tool vector move.
c
c             kflg     I*4  D1  -  Entry flag: 0 - generate the last
c                                  point, 1 - generate intermediate
c                                  point at specified angular distance,
c                                  2 - generate turning point using
c                                  specified relative position.
c
c     OUTPUT: grat     R*8  D1  -  Relative position of the tool vector
c                                  on the TVMP.
c
c
c************************************************************************
c
      subroutine getdvt (krot,kflg,grat)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (NROTA ,KPOSMP(1725)), (IMAXFL,KPOSMP(1729))
      equivalence (NROT  ,KPOSMP(1366)), (IRTACT,KPOSMP(1256))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTINC,KPOSMP(1461))
      equivalence (IRTWRK,KPOSMP(1506))
c
      integer*4 IMAXFL,NROTA(4),IRTWRK(20),NROT,IRTACT(2),IRTNUM,
     1          IRTINC(4)
c
      equivalence (VECTOL,POSMAP(1247)), (VECSAV,POSMAP(1372))
      equivalence (RAD   ,POSMAP(0002)), (SPLDIS,POSMAP(1353))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 RAD,ROTSTO(20,2),VECSAV(3),SPLDIS,VECTOL
c
      integer*4 krot(4),kflg
c
      real*8 grat
c
      integer*4 i,ir1,ir2,ierr,ista1,ista2,ip1,ip2
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
      real*8 angd(4),tvn(3),cosa(2),rnum,anum,ndot,ax(3),
     -       cosb(2),rotn(20),vec1(3),vec2(3),dst1,dst2,amax,rat(2)
c
c......Get tool vector for last
c......or extremity point
c
      if (kflg .eq. 1) go to 100
      if (kflg .eq. 0) then
          call genlpt (2,PTNXLN(1,2),TVNXLN,1.0)
      else
          call genlpt (1,PTNXLN(1,2),TVNXLN,grat)
      end if
      go to 8000
c
c......Find tool vector at specified rotary axis deviation
c
  100 amax   = ADELTA
      rat(1) = 1.d0
      rat(2) = 1.d0
      ip1    = IRTINC(ir1)
      ip2    = IRTINC(ir2)
      call copyn (ROTSTO,rotn,IRTNUM)
      if (NROTA(ir1) .eq. 0) go to 1000
c
c...One rotary axis active
c
      if (NROT .eq. 2) go to 500
      rat(1) = DLTANG(ir1) / ADELTA
      go to 2000
c
c...Two rotary axis active
c...Rotate I-st axis starting vector at new angle
c
  500 if (krot(ir1) .ne. 2) then
          angd(ir1) = DLTANG(ir1)
      else
          angd(ir1) = - DLTANG(ir1)
      end if
      rotn(ip1) = ROTSTO(ip1,1) + angd(ir1)
      if (rotn(ip1) .gt. 360.) rotn(ip1) = rotn(ip1) - 360.
      if (rotn(ip1) .lt. 0.) rotn(ip1) = rotn(ip1) + 360.
      rnum   = rotn(ip1)
      call getijk (rotn,tvn)
c
c...Get II-nd rotary axis rotation vector
c...adjusted for any dead axis & carier
c
      ax(1) = 0.d0
      ax(2) = 0.d0
      ax(3) = 0.d0
      ax(IRTWRK(ip2)) = 1.d0
      do 510 i=ip2-1,ip1,-1
c         anum = ROTSTO(i,1)
          anum = rotn(i)
          call vecadj (ax,ax,anum,IRTWRK(i))
  510 continue
      call vecdad (ax,ax,1)
c
c...Get intersection of the TVMP with the plane
c...defined above
c
      dst1   = 0.d0
c      dst2   = SPLDIS
      dst2   = ndot(tvn,ax)
      call getpnt (TVMPVC,ax,dst1,dst2,vec1,vec2,VECTOL,ierr)
      if (ierr .ne. 0) go to 1000
c
c...Get angle between old TV and solutions
c
      call intvec (vec1,ALNSTO,AFINAL,FIANG,THETA,ista1)
      call intvec (vec2,ALNSTO,AFINAL,FIANG,THETA,ista2)
      if (ista1+ista2 .eq. 0) go to 1000
      cosa(1) = 360.
      cosa(2) = 360.
      if (ista1 .eq. 1) call betvec (VECSAL,vec1,cosa(1))
      if (ista2 .eq. 1) call betvec (VECSAL,vec2,cosa(2))
      if (cosa(2) .lt. cosa(1)) cosa(1) = cosa(2)
c
c...Get ratio for I-st axis
c...Added a check to make sure that too many points
c...are not generated at apex of rotary move
c...for AC-style head.  Originally implemented for FSR 60859
c...Some problems have been found where not enough points
c...are being generated (60905) and this logic has been
c...modified.  This logic is duplicated once below and should
c...be changed in both places.
c...Bobby  -  6/23/05
c
      if (cosa(1) .eq. 0. .or. cosa(1) .eq. 180.) go to 1000
      if ((cosa(1) .lt. 1. .or. cosa(1) .gt. 179.) .and.
     1    cosa(1)*10. .lt. amax) cosa(1) = cosa(1)*5.
cc      if (cosa(1) .lt. 1 .or. cosa(1) .gt. 179.) go to 1000
      if (cosa(1) .lt. amax) rat(1) = cosa(1) / ADELTA
c
c...Fix I-st rotary axis angle &
c...get change of the II-nd axis angle
c
 1000 if (NROTA(ir2) .eq. 0) go to 2000
      if (krot(ir2) .eq. 1) then
          angd(ir2) = DLTANG(ir2)
      else
          angd(ir2) = - DLTANG(ir2)
      end if
      rotn(ip1) = ROTSTO(ip1,1)
      rotn(ip2) = ROTSTO(ip2,1) + angd(ir2)
      if (rotn(ip2) .gt. 360.) rotn(ip2) = rotn(ip2) - 360.
      if (rotn(ip2) .lt. 0.) rotn(ip2) = rotn(ip2) + 360.
c
c...Obtain its vector
c
      call getijk (rotn,tvn)
c
c...Get intersecting vectors on the TVMP
c
      dst1   = 0.d0
      ax(1) = 0.d0
      ax(2) = 0.d0
      ax(3) = 0.d0
      ax(IRTWRK(ip1)) = 1.d0
      call vecdad (ax,ax,1)
      dst2   = ndot (ax,tvn)
      call getpnt (TVMPVC,ax,dst1,dst2,vec1,vec2,VECTOL,ierr)
      if (ierr .ne. 0) go to 2000
c
c...Check if any vector is in the solution area
c
      call intvec (vec1,ALNSTO,AFINAL,FIANG,THETA,ista1)
      call intvec (vec2,ALNSTO,AFINAL,FIANG,THETA,ista2)
      if (ista1+ista2 .eq. 0) go to 2000
      cosb(1) = 360.
      cosb(2) = 360.
      if (ista1 .eq. 1) call betvec (VECSAL,vec1,cosb(1))
      if (ista2 .eq. 1) call betvec (VECSAL,vec2,cosb(2))
c
c...Use the closest solution if both are possible
c
      if (cosb(2) .lt. cosb(1)) cosb(1) = cosb(2)
c
c...Second instance of code mentioned above
c
cc      if (cosb(1) .lt. 1 .or. cosb(1) .gt. 179.) go to 2000
      if ((cosb(1) .lt. 1. .or. cosb(1) .gt. 179.) .and.
     1    cosb(1)*10. .lt. amax) cosb(1) = cosb(1)*5.
ctest     1    cosb(1) .lt. amax/60.) cosb(1) = amax/60.
      if (cosb(1) .lt. amax) rat(2) = cosb(1) / ADELTA
c
c...Get the worse case ratio
c
 2000 if (rat(1) .lt. rat(2)) then
          grat = rat(1)
      else
          grat = rat(2)
      end if
      call genlpt (1,PTNXLN(1,2),TVNXLN,grat)
c
 8000 return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  vecdad (gtvin,gtvo,kdir)
c
c  FUNCTION:  This routine adjust vector for dead axes specified
c             before the first active axis.
c
c     INPUT:  gtvin   R*8  D3  -  Input vector
c
c             kdir    I*4  D1  -  Flag: 1 - adjust forward, 2 - adjust
c                                 revers.
c
c     OUTPUT: gtvo    R*8  D3  -  Ouput vector
c
c*********************************************************************
c
      subroutine vecdad (gtvin,gtvo,kdir)
c
      real*8 gtvin(3),gtvo(3)
      integer*4 kdir
c
      include 'post.inc'
c
      equivalence (IRTWRK,KPOSMP(1506))
      equivalence (IRTACT,KPOSMP(1256)), (NCONTB,KPOSMP(1347))
      equivalence (NROT  ,KPOSMP(1366)), (IRTINC,KPOSMP(1461))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1244))
c
      integer*4 NROT,IRTACT(2),IRTWRK(20),IRDEAD(20),IRTYPE(20),NCONTB,
     1          IRTINC(4)
c
      equivalence (RAD   ,POSMAP(0002)), (ROTSTO,POSMAP(5213))
c
      real*8 RAD,ROTSTO(20,2)
c
      real*8 anum
c
      integer*4 i,ip1,ip2
c
c...Initialize routine
c
      ip1    = IRTINC(IRTACT(1))
      ip2    = IRTINC(IRTACT(2))
      gtvo(1) = gtvin(1)
      gtvo(2) = gtvin(2)
      gtvo(3) = gtvin(3)
c
c...Check if dead axis exists
c
      if (ip1 .eq. 1) go to 8000
      do 110 i=1,ip1-1,1
          if (NCONTB .eq. 2) then
              if (IRDEAD(i) .ne. 0 .and.
     -            IRTYPE(i) .eq. 1) go to 110
          end if
          if (kdir .eq. 1) then
              anum = 360.d0 - ROTSTO(i,1)
          else
              anum = ROTSTO(i,1)
          end if
          call vecadj (gtvo,gtvo,anum,IRTWRK(i))
  110 continue
c
 8000 return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  ovrlim (gang,gbas)
c
c  FUNCTION:  This routine obtains rotary axes position behind the
c             limit(s) when the error from tlaxis call was
c             equal 3.  The position is forced the shortest way.
c
c     INPUT:  gbas    R*8  D4   -  The base angles at the previous
c                                  position.
c
c     OUTPUT: gang    R*8  D20.2 -  The rotary angle position over the
c                                  limit.
c
c************************************************************************
c
      subroutine ovrlim (gang,gbas)
c
      real*8 gang(20,2),gbas(4)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTNXF,KPOSMP(1365))
c
      integer*4 IRTNXF
c
      integer*4 ierr
c
      real*8 mlax(6),mxout(10)
c
c...Force the rotary axes rotation shortest way
c...despite limit error
c
      IRTNXF = 2
      call tlaxis (MCHNUL,TLVECL,mlax,mxout,gang,gbas,0,0,ierr)
      return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  secchk (gang)
c
c  FUNCTION:  This routine defines the delta angle and rotary direction
c             required to bring the I-st rotary axis to the position
c             where the tool axis vector will move on the TVMP if the
c             II-nd rotary axis moves.
c
c     INPUT:  none
c
c     OUTPUT: gang    R*8  D1   -  Angle change of the I-st rotary axis.
c
c
c************************************************************************
c
      subroutine secchk (gang)
c
      real*8 gang
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (NROTA ,KPOSMP(1725))
      equivalence (NROT  ,KPOSMP(1366)), (IRTACT,KPOSMP(1256))
      equivalence (NCONTB,KPOSMP(1347)), (IRTINC,KPOSMP(1461))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTWRK,KPOSMP(1506))
      equivalence (IRTNLS,KPOSMP(1641)), (IRFNLS,KPOSMP(1640))
      equivalence (IRTYPE,KPOSMP(1486))
c
      integer*4 NROTA(4),IRTWRK(20),IRTACT(2),NROT,IRTNLS(4),
     -          IRFNLS,IRTYPE(20),IRDEAD(20),NCONTB,IRTINC(4)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (VECSAV,POSMAP(1372)), (VTOLER,POSMAP(4911))
      equivalence (FUZZ4 ,POSMAP(4912)), (FUZZ8 ,POSMAP(4913))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 RAD,ROTSTO(20,2),VECSAV(3),FUZZ4,FUZZ8,VTOLER
c
      real*8 tvs(3),anum,ang1,ang2,dlta,dlt
c
      integer*4 i,ir1,ir2,inx(6),is1,is2,irt,ip1,ip2
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
      data inx /3,2,3,1,1,2/
c
c...Get I-axis rotary indexes
c
      ip1    = IRTINC(ir1)
      ip2    = IRTINC(ir2)
      is1    = inx(IRTWRK(ip1)*2-1)
      is2    = inx(IRTWRK(ip1)*2)
c
c...Adjust II-nd axis to get spindle plane vector
c
      tvs(1) = 0.d0
      tvs(2) = 0.d0
      tvs(3) = 0.d0
      tvs(IRTWRK(ip1)) = 1.d0
      do 110 i=ip2-1,ip1,-1
          if (NCONTB .eq. 2) then
              if (IRDEAD(i) .ne. 0 .and.
     -            IRTYPE(i) .eq. 1) go to 110
          end if
          anum = ROTSTO(i,1)
          call vecadj (tvs,tvs,anum,IRTWRK(i))
  110 continue
      call vecang (tvs,IRTWRK(ip1),ang1)
c
c...Get angle between II-axis and TVMP
c
      call vecang (TVMPVC,IRTWRK(ip1),ang2)
c
c...vp 30-oct-96
c...now we check if vectors are parallel using tolerance.
c...Added absolute delta angle 'dlt' to set direction
c...of rotation at shortest way with more clear logic.  Note that
c...rotation is always <=90 deg. Following is the old stuff:
c
cc    if (ang2 .lt. ang1) ang2 = ang2 + 360.
cc    dlta   = ang2 - ang1
cc    irt  = 2
cc    if (dlta .gt. 180.) then
cc        dlta = 360. - dlta
cc        if (dlta .gt. 90.) then
cc            irt = 1
cc            dlta = 180. - dlta
cc        end if
cc    else
cc        if (dlta .gt. 90.) then
cc            dlta = 180. - dlta
cc        else
cc            irt = 1
cc        end if
cc    end if
cc
      dlt   = ang2 - ang1
      if (dlt .lt. 0.) dlt = dlt + 360.0
      if (dlt .gt. 180.0) dlt = 360.0 - dlt
c
      if (dlt .lt. VTOLER .or. dlt .gt. 180.-VTOLER) then
          dlt  = 0.
          dlta = dlt
      else
          if (ang2 .lt. ang1) ang2 = ang2 + 360.
          dlta   = ang2 - ang1
      end if
      irt  = 2
      if (dlt .le. 90.0) then
          if (dlta .le. 90.0) irt = 1
          dlta = dlt
      else
          if (dlta .gt. 180.0) irt = 1
          dlta = 180.0 - dlt
      end if
c
c...Check if vectors are parallel or
c...direction of rotation is forced
c
      if (dlta .lt. FUZZ4 .or. dlta .gt. 180.-FUZZ4) then
          MPVSEC = 0
          gang   = 0.
      else
          if (IRFNLS .eq. 1) then
              if (IRTNLS(ir1) .ne. irt) then
                  irt  = IRTNLS(ir1)
                  dlta = 180. - dlta
              end if
          end if
          MPVSEC = irt
          gang   = dlta
      end if
c
      return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  gtfrdv (krot,kfls,kfln)
c
c  FUNCTION:  This routine defines the I-st rotary axis position at the
c             specified angle from its last position when the tool
c             vector rotates around itself (I-st rotary axis).
c
c     INPUT:  krot    I*4  D4   -  Rotary axis directions.
c
c             kfls    I*4  D1   -  Flag from dirchk call: 0 - no intermediate
c                                  points are required, 1 - needs generate
c                                  intermediate point(s)
c
c     OUTPUT: kfls    I*4  D1   -  See input.
c
c             kfln    I*4  D1   -  Entry switch in mchlin: 1 - end of I-st
c                                  axis rotation. 5 - I-st axis rotation
c                                  process active.
c
c************************************************************************
c
      subroutine gtfrdv (krot,kfls,kfln)
c
      integer*4 krot(4),kfls,kfln
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTACT,KPOSMP(1256)), (IRTINC,KPOSMP(1461))
      equivalence (IMAXFL,KPOSMP(1729))
c
      integer*4 IRTACT(2),IMAXFL,IRTINC(4)
c
      equivalence (ROTSTO,POSMAP(5213)), (HLDROT,POSMAP(5253))
c
      real*8 ROTSTO(20,2),HLDROT(20,2)
c
      real*8 ratio,dan,rnum,trot(20,2),dlt(4)
c
      integer*4 irt,ir1,ip1
c
      equivalence (ir1,IRTACT(1))
c
c...Initialize routine
c
      irt    = MPVSEC
      kfln   = 5
      ip1    = IRTINC(ir1)
c
c...Check if linearization for the I-axis is required
c
      if (ANGMAX(ir1) .lt. TVMPAN) then
          ratio  = ANGMAX(ir1) / TVMPAN
          kfls   = 1
      else
          ratio  = 1.
          kfln   = 1
          MPVSEC = 0
          IMAXFL = 0
      end if
c
c...Get delta rotation of the I-st axis
c
      dan    = ratio * TVMPAN
      if (irt .eq. 2) dan = -dan
      rnum   = ROTSTO(ip1,1) + dan
      if (rnum .ge. 360.) rnum = rnum - 360.
      if (rnum .lt.   0.) rnum = rnum + 360.
      TVMPAN = TVMPAN * (1.d0 - ratio)
c
c...Get rotary position on linear scale
c
      call cpyrot (ROTSTO,trot)
      trot(ip1,1) = rnum
      krot(ir1) = irt
      call rotlin (trot,ROTBAL,1,krot,dlt)
c
c...Fake rotation putting new position in ROTSTO
c...to force 'tlaxis' rotate around the tool vector
c
      call cpyrot (trot,ROTSTO)
c
c......Added new position to HLDROT, because
c......this is what 'tlaxis' uses when an axis
c......is held back.
c......Bobby  -  12/6/94
c
      call cpyrot (trot,HLDROT)
c
c...Set variables for tlaxis call
c
      call copyn (VECSAL,TLVECL,3)
      call copyn (STONUL(1,2),MCHNUL(1,2),3)
c
      return
      end
c*********************************************************************
c
c  SUBROUTINE:  compnt1 (kent,kfln,gxout)
c
c  FUNCTION:  This routine defines the I-st rotary axis position at the
c             specified angle from its last position when the tool
c             vector rotates around itself (I-st rotary axis).
c
c     INPUT:  kent    I*4  D1   -  Entry flag: 0 - initialize process,
c                                  1 - generate point in tolerance while
c                                  rotating 1-st axis.
c
c     OUTPUT: kfln    I*4  D1   -  Entry switch in mchlin: 12 - end of I-st
c                                  axis rotation. 11 - I-st axis rotation
c                                  process active.
c
c             gxout   R*8  D10  -  Output axes adjusted for TRANS/-AXIS.
c
c************************************************************************
c
      subroutine compnt1 (kent,kfln,gxout)
c
      include 'post.inc'
      include 'lintol.inc'
c
      integer*4 kfln,kent
      real*8 gxout(10)
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (IRTACT,KPOSMP(1256)), (IRTINC,KPOSMP(1461))
      equivalence (IMAXFL,KPOSMP(1729))
c
      integer*4 IRTACT(2),IMAXFL,IRTINC(4),IRTNUM
c
      equivalence (VECSAV,POSMAP(1372))
      equivalence (ROTSTO,POSMAP(5213)), (HLDROT,POSMAP(5253))
c
      real*8 ROTSTO(20,2),VECSAV(3),HLDROT(20,2)
c
      real*8 rnum,trot(20,2),dlt(4),mlax(6)
c
      integer*4 ir1,ierr,krot(4),ip1
c
      equivalence (ir1,IRTACT(1))
c
c...Initialize linearization
c
      ip1    = IRTINC(ir1)
      if (kent .eq. 0) then
         ROLAST = ROTANL(ip1,1)
         NROLNM = dabs(ROTANL(ip1,2)-ROLSTO(ip1,2))/ROLMAX + 1
         if (NROLNM .eq. 1) go to 8000
         IROLDR = 1
         if (ROLSTO(ip1,2) .gt. ROTANL(ip1,2)) IROLDR = 2
         ROLMAX = (ROTANL(ip1,2) - ROLSTO(ip1,2)) / NROLNM
         call cpyrot (ROLSTO,ROTSTL)
         call copyn (ROLBSV,ROTBAL,4)
         call getijk (ROTSTL(1,2),TLVECL)
         kfln = 10
         goto 8000
      end if
      NROLNM = NROLNM - 1
c
c...Get position of the I-st axis
c
      if (NROLNM .eq. 0) then
          kfln = 11
          rnum = ROLAST
      else
          kfln = 10
          rnum = ROTSTL(ip1,1) + ROLMAX
      end if
      if (rnum .ge. 360.) rnum = rnum - 360.
      if (rnum .lt.   0.) rnum = rnum + 360.
c
c...Get rotary position on linear/rotary scale
c
      call copyn (ROTSTL,trot,20)
      trot(ip1,1) = rnum
      krot(ir1) = IROLDR
      call cpyrot (ROTSTL,ROTSTO)
      call rotlin (trot,ROTBAL,1,krot,dlt)
c
c...Fake rotation putting new position in ROTSTO
c...to force 'tlaxis' rotate around the tool vector
c
      call cpyrot (trot,ROTSTO)
c
c......Added new position to HLDROT, because
c......this is what 'tlaxis' uses when an axis
c......is held back.
c......Bobby  -  12/6/94
c
      call cpyrot (trot,HLDROT)
c
c...Call tlaxis
c
      call tlaxis (MCHNUL,TLVECL,mlax,gxout,ROTANL,ROTBAL,1,0,ierr)
c
 8000 return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  chktvmp (kaxs,gtvc,kflg)
c
c  FUNCTION:  This routine checks if specified rotary axis vector is
c             parallel to the specified vector.
c
c     INPUT:  kaxs    I*4  D1   -  Rotary axis number
c
c             gtvc    R*8  D3   -  Vector to check
c
c     OUTPUT: kflg    I*4  D1   -  Check result: 0 - not parallel,
c                                  1 - parallel.
c
c************************************************************************
c
      subroutine chktvmp (kaxs,gtvc,kflg)
c
      include 'post.inc'
c
      integer*4 kaxs,kflg
      real*8 gtvc(3)
c
      equivalence (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTWRK(20)
c
      equivalence (FUZZ4 ,POSMAP(4912))
c
      real*8 FUZZ4
c
      real*8 vec(3),dlta
c
      kflg   = 0
c
      vec(1) = 0.
      vec(2) = 0.
      vec(3) = 0.
      vec(IRTWRK(kaxs)) = 1.d0
      call betvec (vec,gtvc,dlta)
      if (dlta .gt. 90.) dlta = 180.d0 - dlta
      if (dlta .lt. FUZZ4) kflg = 1
c
      return
      end
c******************************************************************
c
c  SUBROUTINE:  tvr1ck (gvc,kfl)
c
c  FUNCTION:  This routine checks if the 1-st rotary axis vector is
c             within tolerance of the head carrier/table rider axis
c             and replaces it with the rotary axis vector if true.
c
c     INPUT:  gvc     R*8  D3   -  Vector to check.
c
c     OUTPUT: kfl     I*1  D1   -  1 = Vector was adjusted.
c
c******************************************************************
c
      subroutine tvr1ck (gvc,kfl)
c
      include 'post.inc'
c
      equivalence (IRTACT,KPOSMP(1256))
      equivalence (LNRADJ,KPOSMP(1277)), (IRTINC,KPOSMP(1461))
      equivalence (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTWRK(20),IRTACT(2),LNRADJ,IRTINC(4)
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))

      equivalence (RAD   ,POSMAP(0002))
      equivalence (LNRATL,POSMAP(2286)), (VTOLER,POSMAP(4911))
c
      real*8 VTOLER,RAD,LNRATL(5)
c
      integer*4 kfl
c
      real*8 gvc(3)
c
      integer*4 ir1,ir2,ip1,ip2
c
      real*8 firx(3),dlt,tlen,tol,ang
c
      ip1    = IRTINC(ir1)
      ip2    = IRTINC(ir2)
c
c...If angular tolerance based on LINTOL
c...is greater than the user defined tolerance
c...then use the LINTOL tolerance
c
      if (LNRADJ .eq. 1) then
          call mwgtln (gvc,tlen,ang)
          tol    = RAD * dacos((tlen**2+tlen**2-LNRATL(1)**2) /
     1                         (2*tlen*tlen))
      else
          tol    = 0.
      endif
      if (VTOLER .gt. tol) tol = VTOLER
c
c...Determine if tool axis is within tolerance
c...of rider carrier axis.  Adjust it if it is.
c
      firx(1) = 0.d0
      firx(2) = 0.d0
      firx(3) = 0.d0
      firx(IRTWRK(ip1)) = 1.d0
      call vecdad (firx,firx,1)
      call betvec (firx,gvc,dlt)
      call tvfix1 (firx,gvc,dlt,tol,kfl)
c
      return
      end
c
c*****************************************************************
c
c  SUBROUTINE:  tvfix1 (gvin,gvo,gdlt,gtol,kfl)
c
c  FUNCTION:  This routine raplaces gvo vector by gvin if specified
c             gvin (1-st rotary axis vector) is parallel to the
c             to the input vector within tolerance.
c
c     INPUT:  gvin    R*8  D3   -  Input vector.
c
c             gdlt    R*8  D1   -  angle betwen vectors.
c
c             gtol    R*8  D1   -  angular tolerance.
c
c     OUTPUT: gvo     R*8  D3   -  Output vector.
c
c             kfl     I*1  D1   -  1 = Vector was adjusted.
c
c******************************************************************
c
      subroutine tvfix1 (gvin,gvo,gdlt,gtol,kfl)
c
      integer*4 kfl
c
      real*8 gvo(3),gvin(3),gdlt,gtol
c
      kfl    = 0
      if (gdlt .lt. gtol .and. gdlt .ne. 0.) then
         kfl    = 1
         call copyn (gvin,gvo,3)
      else if (gdlt .gt. 180.d0-gtol .and. gdlt .ne. 180.d0) then
         kfl    = 1
         gvo(1) = -gvin(1)
         gvo(2) = -gvin(2)
         gvo(3) = -gvin(3)
      end if
c
      return
      end
