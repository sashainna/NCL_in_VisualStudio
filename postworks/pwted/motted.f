c
c***********************************************************************
c
c   FILE NAME:  motted
c   CONTAINS:
c               motted  stomot  fedcal  fedinv  mdistm  svtmot
c               motota
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c       %M% , %I%
c     DATE AND TIME OF LAST  MODIFICATION
c       %G% , %U%
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  motted (kregs,gvals,knreg)
c
c   FUNCTION: Outputs an APT source motion block.
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
      subroutine motted (kregs,gvals,knreg)
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
      equivalence (ICIPR2,KPOSMP(1266)), (ICRFMT,KPOSMP(1368))
      equivalence (CIRCOD,KPOSMP(1378)), (IC3AXF,KPOSMP(1747))
      equivalence (FMTDES,KPOSMP(2133))
      equivalence (IFITYP,KPOSMP(3150)), (IFOTYP,KPOSMP(3151))
      equivalence (IRAP  ,KPOSMP(3199)), (CIRCON,KPOSMP(4242))
c
      integer*4 MCHOPT(28),CIRCOD(6),IFOTYP,IFITYP,IRAP,ICYCSW(5),
     1          IC3AXF(10),ICIPR2(8),ICYCFL(30),CIRCON(8),ICRFMT
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (MOVDIS,POSMAP(1570))
      equivalence (CIRCDV,POSMAP(2206)), (CIRCOV,POSMAP(2215))
      equivalence (C3XCDV,POSMAP(2244)), (RPM   ,POSMAP(3307))
      equivalence (FEDRNG,POSMAP(3512)), (PFEED ,POSMAP(3540))
      equivalence (MOVTIM,POSMAP(3546))
      equivalence (FEED  ,POSMAP(3547)), (OFEED ,POSMAP(3560))
      equivalence (RAPLMT,POSMAP(3567)), (ROTANG,POSMAP(5173))
c
      real*8 FEDRNG(2,8),FEED(4),MCHNUM(3,4),LINAXS(6),AXSOUT(10),
     1       TLVEC(3),ROTANG(20,2),DUMMY,CIRCDV(2),OFEED(6),PFEED(4),
     2       RAPLMT(10),MOVDIS(4),MOVTIM,RPM,C3XCDV(2),CIRCOV(8)
c
      integer*4 i,nco,frreg,inc,iftyp
      integer*4 vfedrt,vfmod(4),vrapid,vgoto,vwds(4),vtyp(4)
c
      real*8 frval,rnum
      real*8 vval(4)
c
      character*80 vtxt
c
      data vfedrt /1009/, vfmod /73,74,73,73/, vgoto /4013/, vrapid /5/
c
c...Calculate all motion positions
c
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c...Rapid move
c
      if (IRAP .eq. 1 .and. MINTER .ne. PMOT_CIRCUL .and.
     1    MINTER .ne. PMOT_CIR3AX .and. ICYCSW(1) .eq. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vwds(1) = vrapid
              call ptdf_aptstmt (1,vtyp,vwds,vval,vtxt)
          endif
          call mdistm
          MOVTIM = MOVDIS(3) / RAPLMT(1)
c
c...Calculate and output feedrate
c
      else if (MINTER .ne. PMOT_CYCLE .or. ICYCFL(30) .eq. 1) then
          call fedcal (FEED(1),iftyp,frval,frreg)
          if (frval .lt. FEDRNG(1,iftyp)) frval = FEDRNG(1,iftyp)
          if (frval .gt. FEDRNG(2,iftyp)) frval = FEDRNG(2,iftyp)
          rnum = dabs(frval-PFEED(iftyp))
          if (rnum .gt. FEDRNG(1,iftyp)) then
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vwds(1) = vfedrt
                  nco    = 1
                  if (iftyp .ne. IFITYP) then
                      nco    = nco    + 1
                      vtyp(nco) = 1
                      vwds(nco) = vfmod(iftyp)
                  endif
                  nco    = nco    + 1
                  vtyp(nco) = 2
                  vval(nco) = frval
                  call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
              endif
              PFEED(iftyp) = frval
              IFITYP = iftyp
              if(ICYCFL(30) .ne. 1) IRAP   = 0
          endif
      endif
c
c...CYCLE
c
      if (MINTER .eq. PMOT_CYCLE) then
          if (IFOTYP .eq. 2) then
              OFEED(2) = FEED(1) * RPM
          else
              OFEED(2) = FEED(1)
          endif
          PFEED(1) = FEED(1)
          call cycted (kregs,gvals,knreg)
c
c...Circular record
c
      else if (MINTER .eq. PMOT_CIRCUL) then
          call cirted
c
c......Remove all circular registers
c
          do 1000 i=1,6,1
              rnum   = DUMMY
              if (i .le. 2 .and. ICRFMT .ne. 10) rnum = CIRCDV(i)
              call fndreg (kregs,gvals,knreg,CIRCOD(i),rnum,inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
 1000     continue
          if (ICRFMT .eq. 10) then
              call fndreg (kregs,gvals,knreg,CIRCON(2),DUMMY,inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
              call fndreg (kregs,gvals,knreg,CIRCON(3),DUMMY,inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
              call fndreg (kregs,gvals,knreg,CIRCON(3),DUMMY,inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
          endif
c
c...3-axis circular record
c
      else if (MINTER .eq. PMOT_CIR3AX) then
          call cir3ax
c
c......Remove all circular registers
c
          do 1100 i=3,7,1
              rnum   = DUMMY
              if (i .le. 4) rnum = C3XCDV(i-2)
              call fndreg (kregs,gvals,knreg,IC3AXF(i),rnum,inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
 1100     continue
c
c...Conversational circular
c
      else if (MINTER .eq. PMOT_CIRCEN) then
          call circen
c
c......Remove all circle center registers
c
          do 1200 i=3,6,1
              call fndreg (kregs,gvals,knreg,CIRCOD(i),DUMMY,inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
 1200     continue
c
c......Remove circular center register
c
          call fndreg (kregs,gvals,knreg,CIRCON(1),CIRCOV(1),inc)
          if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
c
c...Motion block
c
      else
          call motota
      endif
c
c...Save current tool position
c
      if (ICIPR2(1) .eq. 0) call svtmot
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stomot (kreg,gval)
c
c   FUNCTION:  This routine stores an axis value in the appropriate
c              position in the AXSOUT array.
c
c   INPUT:    kreg    I*4  D1   -  register to store.
c
c             gval    R*8  D1   -  Register value.
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine  stomot (kreg,gval)
c
      include 'pted.inc'
      include 'post.inc'
      include 'menu.inc'
c
      integer*4 kreg
      real*8 gval
c
      equivalence (MOTREG,KPOSMP(0381)), (ACTLIN,KPOSMP(1205))
      equivalence (IRTSCL,KPOSMP(1288)), (IRTCLW,KPOSMP(1292))
c
      integer*4 MOTREG(24),IRTCLW(4),IRTSCL(4),ACTLIN(3)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (AXSSTO,POSMAP(1425))
      equivalence (ROTCRM,POSMAP(1439))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),AXSSTO(10),ROTCRM(4)

      integer*4 j,iaxs(24),ipt,inc,idir,ibas,ilin(12),ixpt(12)
c
      real*8 rval,bas
c
      data iaxs /1,1,2,2,3,3,4,4,5,5,6,6,
     1           0,7,7, 0,8,8, 0,9,9, 0,10,10/
c
      data ilin /1,1,2,2,1,1,2,2,1,1,2,2/
c
      data ixpt /1,1,1,1, 2,2,2,2, 3,3,3,3/
c
c...Goto appropriate section
c
      do 10 j=1,24
        if (MOTREG(j).eq.kreg) goto 20
   10 continue
      goto 8000
c
   20 continue
      goto (100,200,100,200,100,200,100,200,100,200,100,200,
     1      8000,300,400,8000,300,400,8000,300,400,8000,300,400), j
c
c...Absolute linear address
c
  100 AXSOUT(iaxs(j)) = gval
      ACTLIN(ixpt(j)) = ilin(j)
      goto 8000
c
c...Incremental linear address
c
  200 AXSOUT(iaxs(j)) = AXSSTO(iaxs(j)) + gval
      ACTLIN(ixpt(j)) = ilin(j)
      goto 8000
c
c...Absolute rotary axis
c
  300 ipt    = iaxs(j)
      inc    = ipt    - 6
      rval   = gval
      if (IRTCLW(inc) .eq. 2) rval = -rval
      if (IRTSCL(inc) .eq. 1) rval = rval / ROTCRM(inc)
c
c......Axis uses rotary scale
c
      if (IRTSCL(inc) .eq. 2) then
          idir   = 1
          if (rval .lt. 0.) then
              idir   = -1
              rval   = dabs(rval)
          endif
          bas    = AXSSTO(ipt) / 360.
          ibas   = bas
          if (bas .lt. 0.) ibas = ibas - 1
          bas    = ibas   * 360.
          rval   = bas    + rval
          if (idir .eq. -1 .and. rval .gt. AXSSTO(ipt))
     1            rval = rval - 360.
          if (idir .eq. 1 .and. rval .lt. AXSSTO(ipt))
     1            rval = rval + 360.
      endif
c
c......Store value
c
      AXSOUT(ipt) = rval
      if (AXSOUT(ipt) .ne. AXSSTO(ipt) .and. MINTER .eq. PMOT_LINEAR)
     1    MINTER = PMOT_ROTARY
      goto 8000
c
c...Incremental rotary axis
c
  400 ipt    = iaxs(j)
      inc    = ipt    - 6
      rval   = gval
      if (IRTCLW(inc) .eq. 2) rval = -rval
      if (IRTSCL(inc) .eq. 1) rval = rval / ROTCRM(inc)
      AXSOUT(ipt) = AXSSTO(ipt) + rval
      if (AXSOUT(ipt) .ne. AXSSTO(ipt)) MINTER = PMOT_ROTARY
      goto 8000
c
c...End of routine
c
 8000 continue
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fedcal (gval,komod,gfed,kreg)
c
c   FUNCTION:  This routine calculates the programmed feedrate value
c              dependant on the feedrate mode and register value.
c
c   INPUT:    gval    R*8  D1   -  Feedrate register value.
c
c   OUTPUT:   komod   I*4  D1   -  Output feerate mode.
c
c             gfed    R*8  D1   -  Output Feedrate value.
c
c             kreg    I*4  D1   -  Register to use for feedrate output.
c
c***********************************************************************
c
      subroutine fedcal (gval,komod,gfed,kreg)
c
      include 'post.inc'
      include 'pted.inc'
c
      equivalence (IROTFT,KPOSMP(3110)), (IFOTYP,KPOSMP(3151))
      equivalence (IFDCTP,KPOSMP(3157))
      equivalence (IFDOUT,KPOSMP(3158)), (FEDCD ,KPOSMP(3162))
      equivalence (FEDMCD,KPOSMP(3170))
c
      integer*4 IROTFT,FEDCD(8),IFOTYP,IFDOUT(4),IFDCTP,FEDMCD(8)
c
      equivalence (MOVDIS,POSMAP(1570)), (RPM   ,POSMAP(3307))
      equivalence (MOVTIM,POSMAP(3546)), (DPMBPW,POSMAP(3551))
      equivalence (OFEED ,POSMAP(3560))
c
      real*8 MOVTIM,DPMBPW(2),MOVDIS(4),RPM,OFEED(6)
c
      integer*4 komod,kreg
c
      real*8 gval,gfed
c
      integer*4 ifmod,jfmod
c
      real*8 rnum,snum
c
c......Calculate machining distances
c
      jfmod  = abs(IFOTYP)
      if (jfmod .le. 2) jfmod = IFDOUT(jfmod)
      call mdistm
c
c...No rotary movement
c...Feed rate should be as programmed
c
cc      if (MINTER .ne. PMOT_ROTARY .or. (MOVDIS(4) .eq. 0. .and.
cc     1    FEDMCD(jfmod) .ne. 0)) then
      if (MINTER .ne. PMOT_ROTARY .or. (IROTFT .eq. 4 .and.
     1    FEDMCD(IROTFT) .ne. 0)) then
c
c......Inverse time
c
          if (jfmod .eq. 4) then
              call fedinv (gval,gfed)
              komod  = 1
              kreg   = FEDCD(4)
c
c......Standard feedrate
c
          else
              komod  = jfmod
              gfed   = gval
              kreg   = FEDCD(jfmod)
          endif
c
c......Calculate FPM
c
          if (komod .eq. 1) then
              OFEED(2) = gfed
          else if (komod .eq. 2) then
              OFEED(2) = gfed * RPM
          endif
          MOVTIM = 0.
          if (OFEED(2) .ne. 0.) MOVTIM = MOVDIS(3) / OFEED(2)
c
c...Rotary axis movement
c...Determine correct feedrate
c...(MODE is always set to IPM)
c
      else
c
c......FPM when linear movement is greater
c......DPM when rotary movement is greater
c
          ifmod  = IROTFT
          if (IROTFT .eq. 6) then
              rnum   = MOVDIS(3) / DPMBPW(1)
              snum   = MOVDIS(4) / DPMBPW(2)
              ifmod  = 1
              if (snum .gt. rnum) ifmod = 3
          endif
c
c......FPM
c
          if (ifmod .eq. 1) then
              MOVTIM = MOVDIS(3) / gval
              kreg   = FEDCD(1)
c
c......FPR
c
          else if (ifmod .eq. 2) then
              MOVTIM = MOVDIS(3) / (gval*RPM)
              kreg   = FEDCD(2)
c
c......DPM
c
          else if (ifmod .eq. 3) then
              MOVTIM = MOVDIS(4) / gval
              kreg   = FEDCD(3)
c
c......Inverse Time
c
          else if (ifmod .eq. 4) then
              call fedinv (gval,gfed)
              kreg   = FEDCD(4)
c
c......IPM/DPM combined
c
          else if (ifmod .eq. 5) then
              rnum   = dsqrt(MOVDIS(3)**2 + (MOVDIS(4) *
     1                       (DPMBPW(1)/DPMBPW(2)))**2)
              MOVTIM = rnum   / gval
              kreg   = FEDCD(5)
c
c......Feedrate control point
c
          else if (ifmod .eq. 7) then
              MOVTIM = MOVDIS(2) / gval
              kreg   = FEDCD(1)
          endif
c
c......Calculate actual feedrate
c
          komod  = 1
          gfed   = MOVDIS(1) / MOVTIM
          OFEED(1) = gfed
          OFEED(2) = gfed
          OFEED(3) = MOVDIS(3) / MOVTIM
          OFEED(4) = MOVDIS(4) / MOVTIM
          rfed   = dsqrt (MOVDIS(3)**2 + (MOVDIS(4) *
     1        (DPMBPW(1)/DPMBPW(2)))**2)
          OFEED(5) = rfed   / MOVTIM
          if (RPM .eq. 0.) then
              OFEED(6) = 0.
          else
              if (IFDCTP .eq. 1) then
                  OFEED(6) = OFEED(2) / RPM
              else
                  OFEED(6) = OFEED(3) / RPM
              endif
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
c   SUBROUTINE:  fedinv (gval,gfed)
c
c   FUNCTION:  This routine calculates the programmed feedrate value
c              for Inverse Time (1/T) feedrate mode.
c
c   INPUT:    gval    R*8  D1   -  Feedrate register value.
c
c   OUTPUT:   gfed    R*8  D1   -  Output Feedrate value.
c
c***********************************************************************
c
      subroutine fedinv (gval,gfed)
c
      include 'post.inc'
c
      real*8 gval,gfed
c
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (IFDOUT,KPOSMP(3158)), (IFDUNT,KPOSMP(3182))
c
      integer*4 IFDUNT,IFDOUT(4),MCHOPT(20)
c
      equivalence (PI    ,POSMAP(0001))
      equivalence (MOVDIS,POSMAP(1570)), (FRTVAL,POSMAP(3014))
      equivalence (MOVTIM,POSMAP(3546))
c
      real*8 MOVTIM,MOVDIS(4),PI,FRTVAL(2)
c
      real*8 rrad(4),rnum,rdis
c
c...Straight inverse time
c
      if (IFDOUT(4) .eq. 1) then
          MOVTIM = 1.d0 / gval
          if (IFDUNT .eq. 2) MOVTIM = MOVTIM / 60.d0
c
c...(Constant * 2n * Feed) / (60 * Distance)
c...Not supported yet
c
      else if (IFDOUT(4) .eq. 2) then
          MOVTIM = 1.d0 / gval
          if (IFDUNT .eq. 2) MOVTIM = MOVTIM / 60.d0
c
c...Straight time
c
      else if (IFDOUT(4) .eq. 3) then
          MOVTIM = gval
          if (IFDUNT .eq. 2) MOVTIM = MOVTIM / 60.d0
c
c...Vector speed
c
      else if (IFDOUT(4) .eq. 4) then
          call rtfrad (rrad)
          rdis = dsqrt(rrad(1)**2 + rrad(2)**2 + rrad(3)**2 +
     1                 rrad(4)**2)
          rval = MOVDIS(4) / FRTVAL(MCHOPT(2))
          rnum = gval / dsqrt((MOVDIS(3)**2 + rval**2) /
     1               (MOVDIS(3)**2 + (PI/180.*rdis*rval)**2))
          MOVTIM = MOVDIS(2) / rnum
c
c...Okuma Delta / Time
c
      else if (IFDOUT(4) .eq. 5) then
          rval = MOVDIS(4) / FRTVAL(MCHOPT(2))
          MOVTIM = dsqrt(MOVDIS(3)**2 + rval**2) / gval
          if (IFDUNT .eq. 2) MOVTIM = MOVTIM / 60.d0
c
c...Secondary Okuma
c
      else if (IFDOUT(4) .eq. 6) then
          rval = MOVDIS(4) / FRTVAL(MCHOPT(2))
          rnum = gval * dsqrt(MOVDIS(3)**2 + rval**2)
          MOVTIM = MOVDIS(2) / rnum
      endif
c
c...Calculate actual feedrate
c
      gfed   = MOVDIS(1) / MOVTIM
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mdistm
c
c   FUNCTION:  This routine calculates the delta distances for the
c              various linear and rotary axes.
c
c   INPUT:    none
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine mdistm
c
      include 'post.inc'
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399)), (AXSSTO,POSMAP(1425))
      equivalence (MOVDIS,POSMAP(1570))
c
      real*8 MOVDIS(4),MCHNUM(3,4),LINAXS(6),AXSOUT(10),STONUM(3,4),
     1       LINSTO(6),AXSSTO(10)
c
c...Tool end point movement
c
      MOVDIS(1) = dsqrt((MCHNUM(1,1)-STONUM(1,1))**2 +
     1                  (MCHNUM(2,1)-STONUM(2,1))**2 +
     2                  (MCHNUM(3,1)-STONUM(3,1))**2)
c
c...Feedrate control point movement
c
      MOVDIS(2) = MOVDIS(1)
c
c...Linear axes movement
c
      MOVDIS(3) = dsqrt((LINAXS(1)-LINSTO(1))**2 +
     1                  (LINAXS(2)-LINSTO(2))**2 +
     2                  (LINAXS(3)-LINSTO(3))**2 +
     3                  (LINAXS(4)-LINSTO(4))**2 +
     4                  (LINAXS(5)-LINSTO(5))**2 +
     5                  (LINAXS(6)-LINSTO(6))**2)
c
c...Rotary axes movement
c
      MOVDIS(4) = dsqrt((AXSOUT(7)-AXSSTO(7))**2 +
     1                  (AXSOUT(8)-AXSSTO(8))**2 +
     2                  (AXSOUT(9)-AXSSTO(9))**2 +
     3                  (AXSOUT(10)-AXSSTO(10))**2)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  svtmot
c
c   FUNCTION:  This routine calculates the delta distances for the
c              various linear and rotary axes.
c
c   INPUT:    none
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine svtmot
c
      include 'post.inc'
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399)), (AXSSTO,POSMAP(1425))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),STONUM(3,4),VECSAV(3),
     1       LINSTO(6),AXSSTO(10),ROTANG(20,2),ROTSTO(20,2),TLVEC(3)
c
      integer*4 i,j
c
c...General machine positions
c
      do 120 i=1,4,1
          do 100 j=1,3,1
              STONUM(j,i) = MCHNUM(j,i)
  100     continue
  120 continue
c
c...Tool axis vector
c
      do 200 i=1,3,1
          VECSAV(i) = TLVEC(i)
  200 continue
c
c...Linear axes
c
      do 220 i=1,6,1
          LINSTO(i) = LINAXS(i)
  220 continue
c
c...Rotary axes
c
      call cpyrot (ROTANG,ROTSTO)
c
c...Output axes
c
      do 420 i=1,10,1
          AXSSTO(i) = AXSOUT(i)
  420 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  motota
c
c   FUNCTION: Outputs an APT source motion record.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine motota
c
      include 'post.inc'
      include 'pted.inc'
c
      equivalence (MULTAX,KPOSMP(0056))
c
      integer*4 MULTAX
c
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (MOVDIS,POSMAP(1570))
      equivalence (MOVTIM,POSMAP(3546))
c
      real*8 MCHNUM(3,4),TLVEC(3),VECSAV(3),MOVTIM,MOVDIS(4)
c
      integer*4 nco,vtyp(2),vwds(2),i,nc,ityp,ierr
      integer*4 vmultx,von
c
      real*8 vval(2),vpz(3),pt(3),vec(3)
c
      character*80 aptdat,msg
      byte bdat(80)
c
      equivalence (aptdat, bdat)
c
      data vmultx /1105/, von /71/
      data vpz /0.,0.,1.d0/
c
c...APT Source output
c
      if (PCNV_TYPE .eq. PCNV_APTSRC) then
c
c...Determine if MULTAX
c
          if (MULTAX .ne. 1) then
              do 100 i=1,3,1
                  if (TLVEC(i) .ne. vpz(i)) MULTAX = 1
  100         continue
              if (MULTAX .eq. 1) then
                  vwds(1) = vmultx
                  vtyp(2) = 1
                  vwds(2) = von
                  nc     = 2
                  call ptdf_aptstmt (nc,vtyp,vwds,vval,msg)
              endif
          endif
c
c...Multax record
c
          call dpoint (MCHNUM(1,1),pt(1),4)
          call dpoint (MCHNUM(2,1),pt(2),4)
          call dpoint (MCHNUM(3,1),pt(3),4)
          if (MULTAX .eq. 1) then
              call dpoint (TLVEC(1),vec(1),6)
              call dpoint (TLVEC(2),vec(2),6)
              call dpoint (TLVEC(3),vec(3),6)
              write (aptdat,10) pt(1),pt(2),pt(3),vec(1),vec(2),vec(3)
   10         format ('GOTO  /',3(f11.4,','),2(f9.6,','),f9.6)
              nco = 72
              call ptd_addapt (bdat,nco)
c
c...Non-Multax record
c
          else
              write (aptdat,20) MCHNUM(1,1),MCHNUM(2,1),MCHNUM(3,1)
   20         format ('GOTO  /',2(f11.4,','),f11.4)
              nco = 42
              call ptd_addapt (bdat,nco)
          endif
c
c...Simulation record
c
      else
          ityp    = 2
          if (MINTER .eq. PMOT_CIRCUL) ityp = 3
          if (MINTER .eq. PMOT_CYCLE) ityp = 4
          call simmot (ityp,msg,ierr)
      endif
c
c...End of routine
c
 8000 return
      end
