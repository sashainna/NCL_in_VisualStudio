c
c***********************************************************************
c
c   FILE NAME:  mottap
c   CONTAINS:
c               mottap  fedcnv
c
c     COPYRIGHT 2005 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c       mottap.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:59:17
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  mottap (kregs,gvals,knreg)
c
c   FUNCTION: Converts a motion block from one format to another during
c             tape conversion.
c
c   INPUT:  kregs    I*4  DMAXFMT - List of registers in current block.
c
c           gvals    R*8  DMAXFMT - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  DMAXFMT - Updated list of registers.
c
c           gvals    R*8  DMAXFMT - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine mottap (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (ICYCSW,KPOSMP(0271)), (MCHOPT,KPOSMP(0308))
      equivalence (ICIPR2,KPOSMP(1266))
      equivalence (CIRCOD,KPOSMP(1378)), (IC3AXF,KPOSMP(1747))
      equivalence (FMTDES,KPOSMP(2133))
      equivalence (IFITYP,KPOSMP(3150)), (IFOTYP,KPOSMP(3151))
      equivalence (IRAP  ,KPOSMP(3199))
c
      integer*4 MCHOPT(28),CIRCOD(6),IFOTYP,IFITYP,IRAP,ICYCSW(5),
     1          IC3AXF(10),ICIPR2(8),ICYCFL(30)
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (MOVDIS,POSMAP(1570))
      equivalence (CIRCDV,POSMAP(2206)), (C3XCDV,POSMAP(2244))
      equivalence (RPM   ,POSMAP(3307))
      equivalence (FEDRNG,POSMAP(3512)), (PFEED ,POSMAP(3540))
      equivalence (MOVTIM,POSMAP(3546))
      equivalence (FEED  ,POSMAP(3547)), (OFEED ,POSMAP(3560))
      equivalence (RAPLMT,POSMAP(3567)), (ROTANG,POSMAP(5173))
c
      real*8 FEDRNG(2,8),FEED(4),MCHNUM(3,4),LINAXS(6),AXSOUT(10),
     1       TLVEC(3),ROTANG(20,2),DUMMY,CIRCDV(2),OFEED(6),PFEED(4),
     2       RAPLMT(10),MOVDIS(4),MOVTIM,RPM,C3XCDV(2)
c
      integer*4 i,nco,frreg,inc,iftyp
      integer*4 vfedrt,vfmod(4),vrapid,vgoto,vwds(4),vtyp(4)
c
      real*8 frval,rnum
      real*8 vval(4)
c
      data vfedrt /1009/, vfmod /73,74,73,73/, vgoto /4013/, vrapid /5/
c
c...Calculate all motion positions
c
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c...Circular move
c
      if (MINTER .eq. PMOT_CIRCUL) call cirted
c
c...Rapid move
c
      if (IRAP .eq. 1 .and. MINTER .ne. PMOT_CIRCUL .and.
     1    MINTER .ne. PMOT_CIR3AX .and. ICYCSW(1) .eq. 0) then
          call mdistm
          MOVTIM = MOVDIS(3) / RAPLMT(1)
c
c...Calculate and output feedrate
c
      else if (MINTER .ne. PMOT_CYCLE .or. ICYCFL(30) .eq. 1) then
          call fedcal (FEED(1),iftyp,frval,frreg)
          call fedcnv (frval,frreg)
      endif
c
c...Output motion block
c
      call motota
c
c...Save current tool position
c
      call svtmot
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fedcnv (gval,kreg)
c
c   FUNCTION:  This routine calculates the output feedrate value
c              dependant on the feedrate mode and input feedrate during
c              tape conversions.
c
c   INPUT:    gval    R*8  D1   -  Feedrate register value.
c
c             kreg    I*4  D1   -  Register that holds current feed rate.
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine fedcnv (gval,kreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
      include 'ptedpost.inc'
c
      integer*4 kreg
c
      real*8 gval
c
      equivalence (ICYCSW ,KPOSMP(0271)), (REGFRC,KPOSMP(0603))
      equivalence (ICIPRM ,KPOSMP(1230))
      equivalence (ISCIRC ,KPOSMP(1238)), (IFOTYPI,KPOSMP(3151))
      equivalence (IFDOUTI,KPOSMP(3158)), (FEDCDI ,KPOSMP(3162))
      equivalence (IFDUNTI,KPOSMP(3182)), (IRAP   ,KPOSMP(3199))
c
      integer*4 IFOTYPI,IFDUNTI,IFDEXD(4),ISCIRC,ICIPRM(8),ICYCSW(5),
     1          IFDOUTI(4),FEDCDI(8),IRAP,REGFRC(MAXFMT)
c
      equivalence (PI    ,POSMAP(0001))
      equivalence (AXSOUT,POSMAP(1340)), (STONUM,POSMAP(1387))
      equivalence (AXSSTO,POSMAP(1425)), (MOVDIS,POSMAP(1570))
      equivalence (RCIPRM,POSMAP(2049)), (MOVTIM,POSMAP(3546))
      equivalence (OFEED ,POSMAP(3560))
c
      real*8 MOVDIS(4),MOVTIM,OFEED(6),AXSOUT(10),AXSSTO(10),PI,
     1       STONUM(3,4),RCIPRM(25)
c
      equivalence (MCHOPT,OKPOSMP(0308)), (REGSW ,OKPOSMP(0405))
      equivalence (FMTDES,OKPOSMP(2133))
      equivalence (IROTFT,OKPOSMP(3110)), (IFDTYP,OKPOSMP(3148))
      equivalence (IFOTYP,OKPOSMP(3151)), (IFDCTP,OKPOSMP(3157))
      equivalence (IFDOUT,OKPOSMP(3158)), (FEDCD ,OKPOSMP(3162))
      equivalence (FEDMCD,OKPOSMP(3170)), (IFDEXD,OKPOSMP(3178))
      equivalence (IFDUNT,OKPOSMP(3182)), (NFEDTB,OKPOSMP(3183))
      equivalence (IFDSPC,OKPOSMP(3185)), (IFDCIR,OKPOSMP(3206))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 IROTFT,IFDTYP(2),IFOTYP,IFDCTP,MCHOPT(20),IFDOUT(4),
     1          FEDCD(8),IFDUNT,NFEDTB(2),IFDSPC(2),IFDCIR,
     2          REGSW(MAXFMT),FEDMCD(8)
c
      equivalence (REGSTO,OPOSMAP(1032))
      equivalence (FEDCNS,OPOSMAP(3003)), (FDIVCO,OPOSMAP(3328))
      equivalence (FEDTCD,OPOSMAP(3330)), (FEDTVL,OPOSMAP(3420))
      equivalence (FEDSPC,OPOSMAP(3510)), (FEDRNG,OPOSMAP(3512))
      equivalence (DPMBPW,OPOSMAP(3551))
c
      real*8 FEDCNS(4),FDIVCO(2),FEDTCD(90),FEDTVL(90),FEDSPC(2),
     1       FEDRNG(2,8),DPMBPW(2),REGSTO(MAXFMT)
c
      integer*4 inc,iyx,imod,i,inum,nc,index,ifty
c
      real*8 rnum,snum,r2n,rmax,dis,rdis,tnum,rfed,rrad(4)
c
      character*20 sbuf
c
c...Don't perform conversion with Rapid
c
      if (IRAP .eq. 1) go to 8000
c
c...Determine output feed rate mode
c
      imod   = IFOTYPI
      IFOTYP = IFOTYPI
      ifty = 0
c
c......Rotary axes moves
c
      if (MINTER .eq. PMOT_ROTARY .and. ICYCSW(1) .ne. 1 .and.
     1    (IROTFT .ne. 4 .or. FEDMCD(4) .eq. 0)) then
          IFOTYP = IROTFT
c
c......FPM input mode
c
      else if (imod .eq. 1) then
          IFOTYP = IFDOUT(1)
c
c......FPR input mode
c
      else if (imod .eq. 2) then
          IFOTYP = IFDOUT(2)
c
c......Inverse time mode
c
      else if (imod .eq. 4) then
          IFOTYP = 4
      endif
c
c......Force FPR when Z-axis moves
c
cc      if (IFDSPC(1) .eq. 1) then
cc          if (kfl(5) .eq. 1) then
cc              IFOTYP = IFDOUT(2)
cc          else
cc              IFOTYP = IFDOUT(1)
cc          endif
cc      endif
c
c......Force FPR when feed is less than ...
c
      call dpoint (OFEED(1),rnum,5)
      if (IFOTYP .eq. IFDOUT(1) .and. rnum .lt. FEDSPC(1))
     1        IFOTYP = IFDOUT(2)
c
c...Calculate output feed rate numbers
c......FPM when linear movement is greater
c......DPM when rotary movement is greater
c
      if (IFOTYP .eq. 6) then
          rnum   = MOVDIS(3) / DPMBPW(1)
          snum   = MOVDIS(4) / DPMBPW(2)
          IFOTYP = 1
          if (snum .gt. rnum) IFOTYP = 3
      endif
c
c......FPM mode
c
      if (IFOTYP .eq. 1 .or. IFOTYP .eq. 7) then
          ifty   = IFOTYP
c
c.........Feed control point active
c
          if (IFOTYP .eq. 7 .or. IFDCTP .eq. 1) then
              IFOTYP = 1
              rfed   = gval
c
c.........Axes slides active
c
          else
              rfed   = OFEED(3)
          endif
c
c.........Determine if Extended Prec feed required
c
          if (IFDEXD(1) .eq. 1 .and. FEDCD(1) .ne. 0 .and.
     1        FEDCD(5) .ne. 0) then
              rnum   = rfed   * FEDCNS(1)
              call codint (FEDCD(5),rnum,snum,inum)
              tnum   = snum   / FEDCNS(1)
              call codint (FEDCD(1),rfed,rnum,inum)
              if (rnum .ne. tnum) then
                  call rtoc (snum,sbuf,nc)
                  nc     = index(sbuf,'.')
                  if (nc-1 .le. FMTDES(MCHOPT(2)+3,FEDCD(5)))
     1                    IFOTYP = 5
              endif
          endif
c
c.........Standard Precision FPM
c
         if (IFOTYP .eq. 1) then
              if (rfed .lt. FEDRNG(1,1)) rfed = FEDRNG(1,1)
              if (rfed .gt. FEDRNG(2,1)) rfed = FEDRNG(2,1)
              if (IFDTYP(1) .eq. 1) then
                  rnum   = rfed
              else
                  call tabfnd (FEDTVL(1),NFEDTB(1),rfed,inc)
                  rnum   = FEDTCD(inc)
              endif
c
c.........Extended precision FPM
c
          else
              rnum   = rfed   * FEDCNS(1)
              if (rnum .gt. FEDRNG(2,5)) then
                  rnum = FEDRNG(2,5)
                  if (ifty .eq. 1) then
                      OFEED(3) = rnum   / FEDCNS(1)
                  else
                      OFEED(2) = rnum   / FEDCNS(1)
                  endif
              endif
          endif
c
c......FPR mode
c
      else if (IFOTYP .eq. 2) then
          if (OFEED(6) .lt. FEDRNG(1,2)) OFEED(6) = FEDRNG(1,2)
          if (OFEED(6) .gt. FEDRNG(2,2)) OFEED(6) = FEDRNG(2,2)
              if (IFDTYP(2) .eq. 1) then
                  rnum   = OFEED(6)
              else
                  call tabfnd (FEDTVL(46),NFEDTB(2),OFEED(6),inc)
                  rnum   = FEDTCD(45+inc)
              endif
c
c......DPM mode
c
      else if (IFOTYP .eq. 3) then
          if (OFEED(4) .lt. FEDRNG(1,3)) OFEED(4) = FEDRNG(1,3)
          if (OFEED(4) .gt. FEDRNG(2,3)) OFEED(4) = FEDRNG(2,3)
          rnum   = OFEED(4)
c
c......Inverse time mode
c
      else if (IFOTYP .eq. 4) then
          if (MOVTIM .eq. 0.) then
              rnum    = FEDRNG(2,4)
              if (IFDEXD(3) .eq. 1) rnum = FEDRNG(2,8)
          else
              if (ISCIRC .eq. 1) then
                  if (IFDCIR .eq. 2) then
                      rnum   = OFEED(3) / RCIPRM(4)
                  else if (IFDCIR .eq. 3) then
                      rnum   = MOVTIM
                  else
                      rnum   = 1. / MOVTIM
                  endif
              else
                  if (IFDOUT(4) .eq. 3) then
                      rnum   = MOVTIM
                  else
                      rnum   = 1. / MOVTIM
                  endif
              endif
          endif
c
c.........Standard inverse time
c
          if (IFDOUT(4) .eq. 1 .or. IFDOUT(4) .eq. 3) then
              if (IFDUNT .eq. 2) then
                  if (ISCIRC .eq. 1) then
                      if (IFDCIR .eq. 2 .or. IFDCIR .eq. 3) then
                          rnum   = rnum   * 60.
                      else
                          rnum   = rnum   / 60.
                      endif
                  else
                      if (IFDOUT(4) .eq. 3) then
                          rnum   = rnum   * 60.
                      else
                          rnum   = rnum   / 60.
                      endif
                  endif
              endif
              if (IFDEXD(4) .eq. 1 .and. rnum .lt. FEDRNG(1,4))
     1                IFOTYP = 8
              if (IFOTYP .eq. 4) then
                  if (rnum .lt. FEDRNG(1,4)) rnum = FEDRNG(1,4)
                  if (rnum .gt. FEDRNG(2,4)) rnum = FEDRNG(2,4)
c
c............Extended precision inverse time
c
              else
                  rnum   = rnum   * FEDCNS(4)
                  if (rnum .lt. FEDRNG(1,8)) rnum = FEDRNG(1,8)
                  if (rnum .gt. FEDRNG(2,8)) rnum = FEDRNG(2,8)
              endif
c
c.........Toshiba vector speed
c
          else if (IFDOUT(4) .eq. 4) then
              call rtfrad (rrad)
              rdis = dsqrt(rrad(1)**2 + rrad(2)**2 + rrad(3)**2 +
     1                     rrad(4)**2)
              rnum = OFEED(2) * dsqrt((MOVDIS(3)**2 + MOVDIS(4)**2) /
     1               (MOVDIS(3)**2 + (PI/180.*rdis*MOVDIS(4))**2))
              if (rnum .lt. FEDRNG(1,4)) rnum = FEDRNG(1,4)
              if (rnum .gt. FEDRNG(2,4)) rnum = FEDRNG(2,4)
c
c.........Bendix inverse time
c
          else
c
c............Get maximum axis movement
c
              if (ISCIRC .eq. 1) then
                  rdis   = dabs(STONUM(ICIPRM(1),3)-RCIPRM(ICIPRM(1)))
                  dis    = dabs(STONUM(ICIPRM(2),3)-RCIPRM(ICIPRM(2)))
                  if (dis .gt. rdis) rdis = dis
              else
                  rdis   = -1.
                  do 400 i=1,6,1
                      dis    = dabs(AXSOUT(i)-AXSSTO(i))
                      if (dis .gt. rdis) rdis = dis
  400             continue
              endif
c
c............Get 2N
c
              iyx    = 0
              rmax   = rdis   / FDIVCO(2)
  500         r2n    = 2.d0**iyx
              if(r2n .gt. rmax) go to 600
              iyx    = iyx    + 1
              go to 500
c
c............Calculate FRN
c
  600         if (ISCIRC .eq. 1) then
                  r2n    = 2.d0**(iyx+1)
                  rnum   = (FDIVCO(1)*r2n*OFEED(3)) / (60.d0*RCIPRM(4))
              else
                  rnum   = (FDIVCO(1)*r2n*OFEED(3)) / (60.d0*MOVDIS(3))
              endif
              if (rnum .lt. FEDRNG(1,4)) rnum = FEDRNG(1,4)
              if (rnum .gt. FEDRNG(2,4)) rnum = FEDRNG(2,4)
          endif
c
c......Combination FPM/DPM
c
      else
          IFOTYP = 3
          if (OFEED(5) .lt. FEDRNG(1,3)) OFEED(5) = FEDRNG(1,3)
          if (OFEED(5) .gt. FEDRNG(2,3)) OFEED(5) = FEDRNG(2,3)
          rnum   = OFEED(5)
      endif
c
c...If feed rate mode and calculation
c...has not changed, then don't modify FRN
c
      ifl = 1
      if (IFOTYPI .eq. IFOTYP) then
          ifl = 0
          if (IFOTYP .eq. 4) then
              if (IFDUNTI .ne. IFDUNT .or. IFDOUTI(4) .ne. IFDOUT(4))
     1            ifl = 1
          endif
      endif
c
c...Output feed rate
c
      if (ifl .eq. 1) then
          REGSW(FEDCDI(IFOTYPI)) = 0
          REGFRC(FEDCDI(IFOTYPI)) = 0
          REGSW(FEDCD(IFOTYP)) = 1
          REGSTO(FEDCD(IFOTYP)) = rnum
      endif
c
c...End of routine
c
 8000 return
      end
