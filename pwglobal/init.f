c
c***********************************************************************
c
c   FILE NAME: init.for
c   CONTAINS:
c               init    metini
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        init.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        08/11/16 , 09:49:54
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  init
c
c   FUNCTION:  This routine initializes the common variables.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine init
c
      include 'menu.inc'
      include 'post.inc'
      include 'compile.inc'
c
      equivalence (IUNIT ,KPOSMP(0087)), (ICYLFL,KPOSMP(1901))
c
      integer*4 IUNIT,ICYLFL(20)
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
      equivalence (CIRTOS,POSMAP(2241))
c
      real*8 PI,RAD,CIRTOS
c
C VAX-START
C     include '($iodef)'
C VAX-END
c
      integer*4 i,mkvr,kpvr(25),kpvl(25),kpsb(25),mrvr,rpvr(50),
     1          rpvl(50),rpsb(50)
c
      character*2 rid(MAXFMT)
c
      data rid /'A1','A2','A3','B1','B2','B3','C1','C2','C3','C4','C5',
     1          'C6','D','E','F1','F2','F3','G0','G1','G2','G3','G4',
     2          'G5','G6','G7','G8','G9','GA','H','I1','I2','J1','J2',
     3          'K1','K2','L','M0','M1','M2','M3','M4','M5','M6','M7',
     4          'M8','M9','MA','N','O','P','Q','R','S','T','U1','U2',
     5          'V1','V2','W1','W2','X1','X2','Y1','Y2','Z1','Z2',
     6          'AA','AB','AC','AD','AE','AF','AG','AH','AI','AJ',
     7          'AK','AL','AM','AN','AO','AP','AQ','AR','AS','AT',
     8          'AU','AV','AW','AX','AY','AZ'/
c
      data mkvr /25/
      data kpvr /   1,    3,    4,    5,    6,    8,   12,   41,   42,
     1             43,   44,   45,   50,   51,   55,   58,   61,   60,
     2             62,   65,   66,   67,   68,   70,   73/
      data kpvl /0081, 0085, 0001, 0084, 0082, 0056, 0001, 0308, 1151,
     1           1152, 1801, 1803, 0089, 0090, 0083, 0086, 0174, 0405,
     2           0075, 0076, 0057, 0058, 1230, 0097, 0996/
      data kpsb /   1,    1,    1,    1,    1,    1, 6000,    2,    1,
     1              1,    1,    1,    1,    1,    1,    1,    1,MAXFMT,
     2              1,    1,    1,    1,    8,    1,    2/
c
      data mrvr /50/
      data rpvr /   7,    9,   10,   11,   13,   14,   15,   16,   17,
     1             18,   19,   20,   21,   22,   23,   24,   25,   26,
     2             27,   28,   29,   30,   31,   32,   33,   34,   35,
     3             36,   37,   38,   39,   40,   46,   47,   48,   49,
     4             52,   53,   54,   56,   57,   59,   63,   69,   74,
     5             75,   76,   77,   78,   79/
      data rpvl /0491, 0201, 0001, 0001, 0744, 1287, 1474, 1506, 1518,
     1           1299, 1490, 1538, 1544, 1405, 1486, 1530, 1534, 1340,
     2           1496, 1550, 1560, 1570, 3546, 3558, 3559, 1204, 3560,
     3           1369, 3841, 3601, 3721, 0006, 1221, 1594, 1233, 1237,
     4           3307, 3308, 3566, 1032, 1215, 0151, 3989, 2049, 5454,
     5           5101, 5472, 5490, 5508, 5526/
      data rpsb / 240,    6,    1, 6000,    7,   12,   12,   12,   12,
     1              6,    6,    6,    6,    8,    4,    4,    4,   10,
     2             10,   10,   10,    4,    1,    1,    1,    1,    6,
     3              3,  120,  120,  120,   50,   12,    6,    4,   10,
     4              1,    1,    1,MAXFMT,   3,   10,    1,   25,   18,
     5             18,   18,   18,    18,  18/
c
c...Initialize program variables
c
      PI     = dacos (-1.d0)
      RAD    = 180.0d0 / PI
      ICLF   = 0
      ICYLFL(20) = ICYLFL(1)
      MCNSRC = 72
      IERRLN = 22
      IMENBL = 3
      IMENEL = 20
      IMENIX = 3
C VAX-START
C     IORLB  = io$_ttyreadall .or. io$m_noecho
C     IOWLB  = io$_writevblk .or. io$m_noformat
C VAX-END
      IPRMIX = 2
      ISELIN = 22
      ISELPT = 0
      ISGINC = 1
      ISGLST = 1
      IWALK  = 0
c
      LCMPFI = ' '
C VAX-START
C     DVDATA = 'PWORKS$DATA'
C     DVDSUP = 'PWORKS$DSUP'
C VAX-END
C WNT-SUN-SGI-DOS-IBM-HPX-DEC-START
      DVDATA = 'PWORKS_DATA'
      DVDSUP = 'PWORKS_DSUP'
C WNT-SUN-SGI-DOS-IBM-HPX-DEC-END
      LFORMF = char(12)
      LUNHLP = 7
      LUNMAC = 8
      LUNPRM = 8
      LUNPHD = 9
      LUNSC1 = 1
      LUNSC2 = 2
      LUNSC3 = 3
      LUNSC4 = 4
      LUNTI  = 5
      LUNTO  = 6
      INCFLN = 0
      IMINST = 0
c
      MAXERR = 512
      MAXMEN = 40
      MAXMWD = 200
      MAXPWD = 720
      MAXSAP = 512
      do 500 i=1,10,1
          MLEVL(i) = 0
          PRESEL(i) = 0
  500 continue
c
      NERR   = 0
      NLEVL  = 1
c
      PRMSP  = 0
c
      do 700 i=1,MAXFMT,1
          REGID(i) = rid(i)
  700 continue
c
      SAPREC = 31
      do 800 i=1,10,1
          SMREC(i) = 0
  800 continue
      SNLEVL = 0
c
      WRDREC = 18
c
c...Initialize PREGEN variables
c
      MAXKVR = mkvr
      do 2000 i=1,MAXKVR,1
          KPSTVR(i) = kpvr(i)
          KPSTVL(i) = kpvl(i)
          KPSTSB(i) = kpsb(i)
 2000 continue
c
      MAXRVR = mrvr
      do 2100 i=1,MAXRVR,1
          RPSTVR(i) = rpvr(i)
          RPSTVL(i) = rpvl(i)
          RPSTSB(i) = rpsb(i)
 2100 continue
c
c...Initialize PWORKS variables
c
      MAXFAT = 1
      MAXPER = 9999
      MAXWRN = 9999
      do 4000 i=1,20,1
          OPTOVR(i) = -987654321
 4000 continue
      CIRTOS = .001
      if (IUNIT .eq. 2) CIRTOS = .025
c
c...Initialize clfile variables
c
      CLNAME = ' '
      CLDATE = ' '
      CLTIME = ' '
      CAMNAM = ' '
      CAMREV = ' '
      DEFDIR = ' '
      NCLVER = 9.550
c
c...Get the current Date & Time
c
      call ncdate (LDATE)
      call ftim (LTIME)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  metini (kfl)
c
c   FUNCTION:  This routine changes the Units of real variables that
c              were initialized in 'pstini'.
c
c   INPUT:  kfl     I*4  D1  -  1 = convert to Inch.  2 = MM.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine metini (kfl)
c
      include 'post.inc'
c
      equivalence (NUMINF,KPOSMP(1396))
c
      integer*4 NUMINF
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (FTUNT ,POSMAP(0005)), (INFZON,POSMAP(0071))
      equivalence (HOME  ,POSMAP(0151))
      equivalence (CLRPLN,POSMAP(0161))
      equivalence (CLSAV ,POSMAP(0201)), (LEDBEG,POSMAP(1201))
      equivalence (LEDEND,POSMAP(1202)), (BRKVR ,POSMAP(1205))
      equivalence (LNDIST,POSMAP(1251)), (LIMITS,POSMAP(1254))
      equivalence (PPTOLR,POSMAP(1274)), (ORGIN ,POSMAP(1305))
      equivalence (TRANZ ,POSMAP(1308)), (TRANAX,POSMAP(1320))
      equivalence (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399)), (AXSSTO,POSMAP(1425))
      equivalence (PPINCR,POSMAP(1443))
      equivalence (PPMAXD,POSMAP(1584)), (CIRTOL,POSMAP(2201))
      equivalence (CIRTOS,POSMAP(2241))
      equivalence (LNRTOL,POSMAP(2251)), (RETDIS,POSMAP(2279))
      equivalence (LNRATL,POSMAP(2286)), (SLWVR ,POSMAP(2451))
      equivalence (CYCVR ,POSMAP(2926)), (CYLVR ,POSMAP(2993))
      equivalence (FDLCNV,POSMAP(3013)), (RETFED,POSMAP(3239))
      equivalence (FEDTVL,POSMAP(3420)), (FEDSPC,POSMAP(3510))
      equivalence (FEDRNG,POSMAP(3512))
      equivalence (FEDLMT,POSMAP(3528)), (PFEED ,POSMAP(3540))
      equivalence (MAXIPM,POSMAP(3544)), (DPMBPW,POSMAP(3551))
      equivalence (RAPLMT,POSMAP(3567)), (TL    ,POSMAP(3601))
      equivalence (TURDIS,POSMAP(3961)), (TRZX  ,POSMAP(3993))
      equivalence (MAXVEL,POSMAP(4293)), (AXSVEL,POSMAP(4294))
      equivalence (ACCSTP,POSMAP(4295))
      equivalence (ALNDIS,POSMAP(4444)), (ALNMPS,POSMAP(4461))
      equivalence (SMODIS,POSMAP(4465)), (BSPTOL,POSMAP(4902))
      equivalence (RTSHFD,POSMAP(4947)), (SGMOFS,POSMAP(5099))
      equivalence (SGMSTO,POSMAP(5101)), (SGMHOM,POSMAP(5119))
      equivalence (SGMLIM,POSMAP(5137)), (TABORG,POSMAP(5374))
c
      real*8 CLSAV(6),LEDBEG,LEDEND,LIMITS(2,10),FTUNT,CIRTOL(5),
     1       PPTOLR(10),ORGIN(3),TRANZ(12),TRANAX(20),TABORG(3,20),
     2       STONUM(3,4),LINSTO(6),AXSSTO(10),TL(120),PPMAXD(10),
     3       LNDIST(3),FEDTVL(80),FEDSPC(2),FEDLMT(10),MAXIPM,
     4       RAPLMT(10),TURDIS,LNRTOL,RETDIS,RETFED(4),SLWVR(5)
      real*8 CYCVR(5),HOME(10),CLRPLN(4),INFZON(2,10,4),DUMMY,BRKVR(10),
     1       PPINCR(10),CYLVR(5),PFEED(4),DPMBPW(2),TRZX(2),FEDRNG(2,8),
     2       ALNDIS(3),ALNMPS(3),SMODIS,CIRTOS,BSPTOL(2),FDLCNV,MAXVEL,
     3       AXSVEL,ACCSTP(2),LNRATL(5),RTSHFD,SGMOFS(2),SGMSTO(6,3),
     4       SGMHOM(6,3),SGMLIM(2,6,3)
c
      integer*4 kfl
c
      integer*4 i,j
c
      real*8 rcnv,dcnv,fcnv,lcnv
c
c...Set up conversion value
c
      rcnv   = 25.4
      dcnv   = 10.0
      lcnv   = FDLCNV
      fcnv   = 12.0D0 * 25.4D0 / 1000.0D0
      if (kfl .eq. 1) then
          rcnv = 1.D0 / 25.4D0
          dcnv = 1.d-1
          fcnv = 1.D0 / fcnv
          lcnv = 1.D0 / lcnv
      end if
c
c...1 dimension arrays
c
      ACCSTP(1) = ACCSTP(1) * rcnv
      ACCSTP(2) = ACCSTP(2) * rcnv
      AXSVEL = AXSVEL * rcnv
      MAXVEL = MAXVEL * rcnv
c
      BSPTOL(1) = BSPTOL(1) * rcnv
      BSPTOL(2) = BSPTOL(2) * rcnv
      CLRPLN(4) = CLRPLN(4) * rcnv
      CYLVR(1) = CYLVR(1) * rcnv
c
      DPMBPW(1) = DPMBPW(1) * dcnv
      FEDSPC(1) = FEDSPC(1) * rcnv
      FEDSPC(2) = FEDSPC(2) * rcnv
      FEDRNG(1,1) = FEDRNG(1,1) * lcnv
      FEDRNG(1,2) = FEDRNG(1,2) * lcnv
      FEDRNG(1,5) = FEDRNG(1,5) * lcnv
      FEDRNG(2,1) = FEDRNG(2,1) * lcnv
      FEDRNG(2,2) = FEDRNG(2,2) * lcnv
      FEDRNG(2,5) = FEDRNG(2,5) * lcnv
c
      LEDBEG = LEDBEG * rcnv
      LEDEND = LEDEND * rcnv
      LNRATL(1) = LNRATL(1) * rcnv
      LNRATL(4) = LNRATL(4) * rcnv
      LNRTOL = LNRTOL * rcnv
c
      MAXIPM = MAXIPM * rcnv
c
      RETDIS = RETDIS * rcnv
      RTSHFD = RTSHFD * rcnv
c
      SLWVR(2) = SLWVR(2) * rcnv
      SLWVR(3) = SLWVR(3) * rcnv
      SLWVR(4) = SLWVR(4) * rcnv
      SLWVR(5) = SLWVR(5) * rcnv
c
      TRZX(1) = TRZX(1) * rcnv
      TRZX(2) = TRZX(2) * rcnv
      TURDIS = TURDIS * rcnv
c
      BRKVR(1) = BRKVR(1) * fcnv
      BRKVR(2) = BRKVR(2) * fcnv
      BRKVR(4) = BRKVR(4) * rcnv
      BRKVR(5) = BRKVR(5) * rcnv
c
      CIRTOS = CIRTOS * 25.4
c
      SGMOFS(1) = SGMOFS(1) * rcnv
      SGMOFS(2) = SGMOFS(2) * rcnv
c
c...3 dimension arrays
c
      do 30 i=1,3,1
          CLSAV(i) = CLSAV(i) * rcnv
          CYCVR(i) = CYCVR(i) * rcnv
          LNDIST(i) = LNDIST(i) * rcnv
          ORGIN(i) = ORGIN(i) * rcnv
          ALNMPS(i) = ALNMPS(i) * rcnv
   30 continue
      ALNDIS(1) = ALNDIS(1) * rcnv
      ALNDIS(2) = ALNDIS(2) * rcnv
      SMODIS = SMODIS * rcnv
c
c...4 dimension arrays
c
      do 42 i=1,4,1
          PFEED(i) = PFEED(i) * rcnv
          RETFED(i) = RETFED(i) * rcnv
          do 40 j=1,3,1
              STONUM(j,i) = STONUM(j,i) * rcnv
   40     continue
   42 continue
c
c...5 dimension arrays
c
      do 50 i=1,5,1
          CIRTOL(i) = CIRTOL(i) * rcnv
   50 continue
c
c...6 dimension arrays
c
      do 60 i=1,6,1
          AXSSTO(i) = AXSSTO(i) * rcnv
          LINSTO(i) = LINSTO(i) * rcnv
          PPINCR(i) = PPINCR(i) * rcnv
          TRANAX(i) = TRANAX(i) * rcnv
          TRANZ(i) = TRANZ(i) * rcnv
c
          if (LIMITS(1,i) .ne. -100000.)
     1            LIMITS(1,i) = LIMITS(1,i) * rcnv
          if (LIMITS(2,i) .ne. 100000.) LIMITS(2,i) = LIMITS(2,i) * rcnv
c
          if (kfl .eq. 1) then
              if (PPTOLR(i) .eq. .001) then
                  PPTOLR(i) = .0001
              else
                  PPTOLR(i) = PPTOLR(i) * rcnv
              endif
          else
              if (PPTOLR(i) .eq. .0001) then
                  PPTOLR(i) = .001
              else
                  PPTOLR(i) = PPTOLR(i) * rcnv
              endif
          endif
c
          if (PPMAXD(i) .ne. 99999999.) PPMAXD(i) = PPMAXD(i) * rcnv
          FEDLMT(i) = FEDLMT(i) * rcnv
          RAPLMT(i) = RAPLMT(i) * rcnv
c
          do 58 j=1,3,1
              SGMSTO(i,j) = SGMSTO(i,j) * rcnv
              SGMHOM(i,j) = SGMHOM(i,j) * rcnv
              if (SGMLIM(1,i,j) .ne. -100000.)
     1                SGMLIM(1,i,j) = SGMLIM(1,i,j) * rcnv
              if (SGMLIM(2,i,j) .ne. 100000.)
     1                SGMLIM(2,i,j) = SGMLIM(2,i,j) * rcnv
   58     continue
c
   60 continue
c
c...10 dimension arrays, Vp 7/28/94 now is 6 enough.
c
      do 100 i=1,6,1
          HOME(i) = HOME(i) * rcnv
          do 95 j=1,NUMINF,1
              if (INFZON(1,i,j) .ne. DUMMY)
     1                INFZON(1,i,j) = INFZON(1,i,j) * rcnv
              if (INFZON(2,i,j) .ne. DUMMY)
     1                INFZON(2,i,j) = INFZON(2,i,j) * rcnv
   95     continue
  100 continue
c
c...20 Dimension array
c
      do 200 i=1,20,1
          TABORG(1,i) = TABORG(1,i) * rcnv
          TABORG(2,i) = TABORG(2,i) * rcnv
          TABORG(3,i) = TABORG(3,i) * rcnv
  200 continue
c
c...80 dimension arrays
c
      do 800 i=1,80,1
          FEDTVL(i) = FEDTVL(i) * rcnv
  800 continue
c
c...120 dimension arrays
c
      do 1200 i=1,120,1
          TL(i) = TL(i) * rcnv
 1200 continue
c
c...End of routine
c
 8000 return
      end
