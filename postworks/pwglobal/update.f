c
c***********************************************************************
c
c   FILE NAME: update.for
c   CONTAINS:
c               cnvday  update  u03291  m03291  u10292  u02293
c               u10293  u12294  u08025  u01396  u03296  u07396
c               u01087  u01177  u04248  u09149  u08310  u10112
c               u05193  u02034  u06094  u09134  u10294  u12144
c               u03155  u07135  u01256  u04196  u06126  u10276
c               u02027  u11137  u01309  u05139  u06179  u02020
c               u10180  u02161  u05121  u08072  u09052  u04303
c               u12053  u05125  u10055  u03106
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        update.f , 25.5
c     DATE AND TIME OF LAST  MODIFICATION
c        03/11/16 , 11:10:37
c
c***********************************************************************
c***********************************************************************
c
c   SUBROUTINE:  update
c
c   FUNCTION:  This is the controlling routine for updating previous
c              versions of MDF files to the current version.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine update
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT,KPOSMP(0087))
c
      integer*4 IUNIT
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 NUPD
      parameter (NUPD=48)
c
      character*8 chkdat(NUPD)
c
      integer*4 i,ndmdf,nchk(NUPD),ichk
c
      data chkdat /'08.03.90','11.06.92','02.12.93','06.30.93',
     2             '10.13.93','12.08.93','08.26.94','12.23.94',
     3             '08.04.95','11.27.95','02.06.96','03.23.96',
     4             '10.16.96','01.08.97','04.11.97','04.24.98',
     5             '09.14.99','08.31.00','09.10.02','05.19.03',
     6             '02.03.04','06.09.04','07.26.04','10.29.04',
     7             '12.14.04','03.15.05','07.13.05','01.25.06',
     8             '03.30.06','06.12.06','11.13.06','02.02.07',
     9             '11.13.07','11.06.08','05.13.09','06.17.09',
     -             '02.02.10','10.18.10','02.16.11','05.12.11',
     1             '08.07.12','09.05.12','04.30.13','12.05.13',
     2             '05.12.15','10.05.15','03.10.16','        '/
c
c...Convert revision's date to number of days
c
      call cnvday (LUPDAT,ndmdf)
      ichk   = 0
      do 110 i=1,NUPD,1
          if (chkdat(i) .eq. ' ') go to 200
          call cnvday (chkdat(i),nchk(i))
          ichk = i
  110 continue
  200 continue
c
c...Update from the oldest version
c
      do 3000 i=1,ichk,1
          if (ndmdf .ge. nchk(i)) go to 3000
          go to (300,400,500,700,800,900,1000,1100,1200,1300,1400,
     1           1500,1600,1700,1800,1900,2000,2010,2020,2030,2040,
     2           2050,2060,2070,2080,2090,2100,2200,2210,2220,2230,
     3           2240,2250,2260,2270,2280,2290,2300,2310,2320,2330,
     4           2340,2350,2360,2370,2380,2390) i
c
c...Update to 02.28.91 Version
c
  300     call u03291
          if (IUNIT .eq. 2) call m03291
          go to 3000
c
c...Update to 11.06.92 Version
c
  400     call u11092
          go to 3000
c
c...Update to 02.12.93 Version
c
  500     call u02293
          go to 3000
c
c...Update to 06.01.93 Version
c
  700     call u06093
          go to 3000
c
c...Update to 10.12.93 Version
c
  800     call u10293
          go to 3000
c
c...Update to 12.08.93 Version
c
  900     call u12193
          go to 3000
c
c...Update to 08.11.94 Version
c
 1000     call u08394
          go to 3000
c
c...Update to 12.23.94 Version
c
 1100     call u12294
          go to 3000
c
c...Update to 08.02.95 Version
c
 1200     call u08025
          go to 3000
c
c...Update to 11.27.95 Version
c
 1300     call u11275
          go to 3000
c
c...Update to 02.06.96 Version
c
 1400     call u01396
          go to 3000
c
c...Update to 03.23.96 Version
c
 1500     call u03296
          go to 3000
c
c...Update to 07.30.96 Version
c
 1600     call u07396
          go to 3000
c
c...Update to 01.08.97 Version
c
 1700     call u01087
          go to 3000
c
c...Update to 01.16.97 Version
c
 1800     call u01177
          go to 3000
c
c...Update to 04.24.98 Version
c
 1900     call u04248
          go to 3000
c
c...Update to 09.14.99 Version
c
 2000     call u09149
          go to 3000
c
c...Update to 08.31.00 version
c
 2010     call u08310
          go to 3000
c
c...Update to 10.11.02 version
c
 2020     call u10112
          go to 3000
c
c...Update to 05.19.03 version
c
 2030     call u05193
          go to 3000
c
c...Update to 01.24.04 version
c
 2040     call u02034
          go to 3000
c
c...Update to 06.04.04 version
c
 2050     call u06094
          go to 3000
c
c...Update to 09.13.04 version
c
 2060     call u09134
          go to 3000
c
c...Update to 10.29.04 version
c
 2070     call u10294
          go to 3000
c
c...Update to 12.14.04 version
c
 2080     call u12144
          go to 3000
c
c...Update to 03.15.05 version
c
 2090     call u03155
          go to 3000
c
c...Update to 07.13.05 version
c
 2100     call u07135
          go to 3000
c
c...Update to 01.17.06 version
c
 2200     call u01256
          go to 3000
c
c...Update to 04.19.06 version
c
 2210     call u04196
          go to 3000
c
c...Update to 06.12.06 version
c
 2220     call u06126
          go to 3000
c
c...Update to 11.13.06 version
c
 2230     call u11136
          go to 3000
c
c...Update to 02.02.07 version
c
 2240     call u02027
          go to 3000
c
c...Update to 11.13.07 version
c
 2250     call u11137
          go to 3000
c
c...Update to 01.30.09 version
c
 2260     call u01309
          go to 3000
c
c...Update to 05.13.09 version
c
 2270     call u05139
          go to 3000
c
c...Update to 06.17.09 version
c
 2280     call u06179
          go to 3000
c
c...Update to 02.02.10 version
c
 2290     call u02020
          go to 3000
c
c...Update to 10.18.10 version
c
 2300     call u10180
          go to 3000
c
c...Update to 02.16.11 version
c
 2310     call u02161
          go to 3000
c
c...Update to 05.12.11 version
c
 2320     call u05121
          go to 3000
c
c...Update to 08.07.12 version
c
 2330     call u08072
          go to 3000
c
c...Update to 09.05.12 version
c
 2340     call u09052
          go to 3000
c
c...Update to 04.30.13 version
c
 2350     call u04303
          go to 3000
c
c...Update to 12.05.13 version
c
 2360     call u12053
          go to 3000
c
c...Update to 05.12.15 version
c
 2370     call u05125
          go to 3000
c
c...Update to 10.05.15 version
c
 2380     call u10055
          go to 3000
c
c...Update to 03.10.16 version
c
 2390     call u03106
          go to 3000
c
 3000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u03291
c
c   FUNCTION:  This routine updates the MDF file from the version
c              08.03.90 to the version 03.29.91.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u03291
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IDUMMY,KPOSMP(0088)), (ICYCFL,KPOSMP(0181))
      equivalence (CYCCOD,KPOSMP(0211)), (CYCREG,KPOSMP(0231))
      equivalence (ICYTIM,KPOSMP(0251)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (MCHOPT,KPOSMP(0308))
      equivalence (ISQBKV,KPOSMP(0853)), (AL3BLK,KPOSMP(0854))
      equivalence (NTAXCD,KPOSMP(1090)), (ICSMRG,KPOSMP(1132))
      equivalence (ICSMFL,KPOSMP(1133)), (MXCKSM,KPOSMP(1134))
      equivalence (IRWEOB,KPOSMP(1135)), (NBOT  ,KPOSMP(1136))
      equivalence (BRKOP ,KPOSMP(1137)), (IPRDES,KPOSMP(1154))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (LRTRCT,KPOSMP(1278)), (SPITAX,KPOSMP(1279))
      equivalence (RSIZCD,KPOSMP(1328)), (RSIZFL,KPOSMP(1333))
      equivalence (ICRHEL,KPOSMP(1373))
      equivalence (ICRDIM,KPOSMP(1389)), (ICRDIR,KPOSMP(1390))
      equivalence (NUMINF,KPOSMP(1396)), (SLWFL ,KPOSMP(1701))
      equivalence (SLWCD ,KPOSMP(1706)), (ISLWDN,KPOSMP(1721))
      equivalence (ISLWFD,KPOSMP(1723)), (NOTOOL,KPOSMP(1802))
      equivalence (TOOLFL,KPOSMP(1804)), (TURFL ,KPOSMP(1824))
      equivalence (TLDIG ,KPOSMP(1829)), (TLCCD ,KPOSMP(1839))
      equivalence (TLCBLK,KPOSMP(1859)), (TLOCD ,KPOSMP(1860))
      equivalence (TLOBLK,KPOSMP(1868)), (TLOSGN,KPOSMP(1869))
      equivalence (ITSELF,KPOSMP(1870))
      equivalence (LSTGRP,KPOSMP(1871)), (LSTTSP,KPOSMP(1872))
      equivalence (TLOFPL,KPOSMP(1875)), (TLOFDR,KPOSMP(1876))
      equivalence (TOFNXT,KPOSMP(1877)), (ICYLFL,KPOSMP(1901))
      equivalence (CYLCOD,KPOSMP(1921)), (CYLREG,KPOSMP(1946))
      equivalence (ICYLTM,KPOSMP(1971))
      equivalence (ICYBLK,KPOSMP(3060)), (ICYLBK,KPOSMP(3080))
      equivalence (POSFDF,KPOSMP(3208)), (POSFED,KPOSMP(3209))
      equivalence (IFDRAD,KPOSMP(3210)), (IFDRCD,KPOSMP(3211))
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
      equivalence (ICUTDO,KPOSMP(3301)), (DELYCD,KPOSMP(3351))
      equivalence (DELYFL,KPOSMP(3356)), (HOMCOD,KPOSMP(3361))
      equivalence (HOMREG,KPOSMP(3366)), (PSTNCD,KPOSMP(3376))
      equivalence (PSTNRG,KPOSMP(3381)), (PSTNOF,KPOSMP(3396))
      equivalence (IPSTNF,KPOSMP(3397)), (EXCLPS,KPOSMP(3421))
      equivalence (EXCLNE,KPOSMP(3469)), (NUMEXC,KPOSMP(3517))
      equivalence (EXCLNM,KPOSMP(3518))
c
      integer*4 MCHOPT(20),IDUMMY,NOTOOL,MACHTP,ICRHEL(5),IPRDES(2,10),
     1          TOOLFL(20),TURFL(5),TLDIG(2,5),TLCCD(20),TLCBLK,ISLWDN,
     2          TLOCD(8),TLOBLK,TLOSGN,ITSELF,LSTGRP,LSTTSP,TLOFPL,
     3          TLOFDR,SPITAX,TOFNXT,LRTRCT,NTAXCD(3),POSFDF,POSFED,
     4          CUTCFL(20),CUTCCD(30),ICUTDO(15),SLWFL(5),SLWCD(15)
      integer*4 ISLWFD,ICYCFL(30),CYCCOD(20),CYCREG(20),ICYBLK(20),
     1          ICYTIM(20),ICYCSW(5),ISQBKV,DELYCD(5),DELYFL(5),
     2          HOMCOD(5),HOMREG(10),AL3BLK,ICYLFL(20),CYLCOD(25),
     3          CYLREG(25),ICYLBK(20),ICYLTM(20),PSTNCD(5),PSTNRG(15),
     4          PSTNOF,ICSMRG,ICSMFL,MXCKSM,IPSTNF,IRWEOB,NBOT
      integer*4 BRKOP(10),NUMEXC,EXCLNM(4),NUMINF,RSIZCD(5),ICRDIM,
     1          EXCLPS(12,4),EXCLNE(12,4),RSIZFL,IFDRAD,IFDRCD,
     2          ICYCDO(15),ICRDIR
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (INFZON,POSMAP(0071)), (HOME  ,POSMAP(0151))
      equivalence (CLRPLN,POSMAP(0161)), (DELYVL,POSMAP(0165))
      equivalence (DELYTM,POSMAP(0166)), (HOMCDV,POSMAP(0167))
      equivalence (PSTNCV,POSMAP(0172)), (RETPOS,POSMAP(0177))
      equivalence (ISEQSV,POSMAP(1182)), (BRKVR ,POSMAP(1205))
      equivalence (RSIZCV,POSMAP(1600)), (LNRTOL,POSMAP(2251))
      equivalence (RETDIS,POSMAP(2279)), (RETFED,POSMAP(2280))
      equivalence (CTOFRG,POSMAP(2401)), (CUTCVR,POSMAP(2402))
      equivalence (CUTCVL,POSMAP(2410)), (SLWVR ,POSMAP(2451))
      equivalence (SLWVL ,POSMAP(2456))
      equivalence (CYCCDV,POSMAP(2901)), (CYCVR ,POSMAP(2926))
      equivalence (CYCPSV,POSMAP(2951)), (CYCJVC,POSMAP(2961))
      equivalence (CYCPLN,POSMAP(2964)), (CYLCDV,POSMAP(2968))
      equivalence (CYLVR ,POSMAP(2993)), (SPIVEC,POSMAP(3583))
      equivalence (TURDIS,POSMAP(3961)), (TLCTIM,POSMAP(3962))
      equivalence (TLCVL ,POSMAP(3963)), (TLOVL ,POSMAP(3983))
      equivalence (TLOBAS,POSMAP(3988)), (LSTTN ,POSMAP(3990))
      equivalence (TLOFRG,POSMAP(3991)), (TSELTM,POSMAP(3992))
      equivalence (TRZX  ,POSMAP(3993)), (MAXTN ,POSMAP(3995))
c
      real*8 TURDIS,TLCTIM,TLCVL(20),TLOVL(5),TLOBAS,DUMMY,
     1       LSTTN,TLOFRG,CTOFRG,TSELTM,TRZX(2),MAXTN,SPIVEC(3),LNRTOL,
     2       RETDIS,RETFED(2),CUTCVR(8),CUTCVL(30),SLWVR(5),SLWVL(15),
     3       CYCCDV(25),CYCVR(5),CYCPSV(10),ISEQSV,HOME(10),CLRPLN(4),
     4       DELYVL,DELYTM,HOMCDV(5),CYCJVC(3),CYCPLN(4),CYLCDV(25)
      real*8 CYLVR(5),PSTNCV(5),BRKVR(10),RETPOS(10),INFZON(2,10,4),
     1       RSIZCV
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i,j,itlc(12),itlv(12),itloc(8),bko(5),
     1          itlov(5),cucf(20),cucd(24),cudo(10),swfl(4),swcd(15),
     2          cmfl(30),cmcd(15),cmrg(15),cmtm(14),hmcd(4),hmrg(10),
     3          clfl(12),clcd(25),clrg(20),cltm(14),pncd(5),pnrg(15)

c
      real*8 cucr(4),cucv(24),swvl(15),cmcv(15),clpl(4),hmvl(4),
     1       clcv(25),pncv(5)
c
      data bko /0,1,1,1, 0/
      data clpl /0.,0.,1.,0./
      data clcd /-1, 0, 0, 0, 0,-1, 0, 0,-1, 0, 0, 0,-1,-1, 0,-1,-1, 0,
     1           0,0,0,0,0,-2,-2/
      data clcv /33, 0, 0, 0, 0,75, 0, 0,94, 0, 0, 0,92,76, 0,74,90, 0,
     2           0,0,0,0,0,23,24/
      data clfl /1,1,1,1,2,2,1,1,2,2,1,0/
      data clrg /17,0,0,0,0,0,35,31,0,0,0,0,52,35,31,13,0,0,0,0/
      data cltm /1,1,2,2,3,3,4,4,5,5,6,6,7,7/
      data cmcd /-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0/
      data cmcv /80,86,87,88,89,83,81,82,87,85,74,76,84,87,0/
      data cmfl /1,2,1,1,1,1,1,1,1,2,2,1,1,1,2,2,1,1,2,1,1,1,2,2,1,
     1          1,1,2,0,0/
      data cmrg /0,52,14,50,0,35,0,51,31,33,35,60,0,36,0/
      data cmtm /1,2,3,4,5,6,7,8,9,10,11,12,13,0/
      data cucf /0,1,2,2,2,1,1,2,2,1,2,2,2,1, 0,0,0,0,0,0/
      data cucd /-1,-1,-1,13,-1,50,51,52,50,14,14,-1,-1,-1,50,29,0,0,-1,
     1           -1,55,57,59,0/
      data cucr /180.d0, 3.2767d0, 0., 0./
      data cucv /41.,42.,40.,0.,39,0,0,0,0,2,3,45,46,47,0,0,0,0,45,46,0,
     1           0,0,0/
      data cudo /0,0,1,1,1,0,0,0,0,0/
c
      data hmcd /-1,-1,-1,-1/, hmvl /27.,28.,29.,30./
      data hmrg /62,56,64,58,66,60,3,6,9,12/
c
      data itlc /0,0,54,54,-2,-2,0,0,0,0,-2,-2/
      data itlv /0,0, 0,0,  6, 0,0,0,0,0,17,18/
c
      data itloc /29,0,-1,-1,-1,55,57,59/
      data itlov / 0,0,49,43,44/
c
      data pncd /-1,0,0, 0,0/, pncv /92,0,0, 0,0/
      data pnrg /61,55,63,57,65,59,2,5,8,11, 49,0, 0,0,0/
c
      data swfl /1,2,2,1/
      data swcd /-1,0,0,0, -1,-1,0,0,0,0,0,0, -1,0,0/
      data swvl /8,0,0,0, 8,8,0,0,0,0,0,0, 9,0,0/
c
c...Move MCHOPT
c
      do 100 i=1,20,1
          MCHOPT(i) = KPOSMP(280+i)
  100 continue
      MCHOPT(4) = IDUMMY
c
c...Initialize PPRINT record number
c
      IPRDES(1,10) = 0
      do 200 i=1,9,1
          if (IPRDES(1,i) .gt. IPRDES(1,10))
     1            IPRDES(1,10) = IPRDES(1,i) + 1
          if (IPRDES(2,i) .gt. IPRDES(1,10))
     1            IPRDES(1,10) = IPRDES(2,i) + 1
  200 continue
      if (IPRDES(1,10) .gt. 10) IPRDES(1,10) = 0
c
c...Initialize user defined blocks
c
      AL3BLK = 0
c
c...Initialize DIM(3) arrays
c
      do 550 i=1,3,1
          CYCVR(i) = 0.
          NTAXCD(i) = 0
          TLDIG(1,i) = 0
          TLDIG(2,i) = 0
  550 continue
c
c...Initialize DIM(4) arrays
c
      do 600 i=1,4,1
          CLRPLN(i) = clpl(i)
          CUTCVR(i) = cucr(i)
          EXCLNE(1,i) = IDUMMY
          EXCLNM(i) = 0
          EXCLPS(1,i) = IDUMMY
          HOMCOD(i) = hmcd(i)
          HOMCDV(i) = hmvl(i)
          SLWFL(i) = swfl(i)
  600 continue
c
c...Initialize DIM(5) arrays
c
      do 620 i=1,5,1
          BRKOP(i) = bko(i)
          BRKVR(i) = 0.
          PSTNCD(i) = pncd(i)
          PSTNCV(i) = pncv(i)
          RSIZCD(i) = 0
          TLOVL(i) = itlov(i)
  620 continue
      TLOVL(1) = DUMMY
c
c...Initialize DIM(8) arrays
c
      do 700 i=1,8,1
          TLOCD(i) = itloc(i)
  700 continue
c
c...Initialize DIM(10) arrays
c
      do 880 i=1,10,1
          CYCPSV(i) = DUMMY
          HOME(i) = 0.
          HOMREG(i) = hmrg(i)
          ICUTDO(i) = cudo(i)
          do 820 j=1,4,1
              INFZON(1,i,j) = DUMMY
              INFZON(2,i,j) = DUMMY
  820     continue
  880 continue
c
c...Initialize DIM(12) arrays
c
      do 900 i=1,12,1
          ICYLFL(i) = clfl(i)
          TLCCD(i) = itlc(i)
          TLCVL(i) = itlv(i)
  900 continue
      TLCVL(3) = DUMMY
      TLCVL(4) = DUMMY
c
c...Initialize DIM(14) arrays
c
      do 980 i=1,14,1
          ICYBLK(i) = 0
          ICYLTM(i) = cltm(i)
          ICYTIM(i) = cmtm(i)
 980  continue
c
c...Initialize DIM(15) arrays
c
      do 1000 i=1,15,1
          CYCCOD(i) = cmcd(i)
          CYCCDV(i) = cmcv(i)
          CYCREG(i) = cmrg(i)
          PSTNRG(i) = pnrg(i)
          SLWCD(i) = swcd(i)
          SLWVL(i) = swvl(i)
 1000 continue
      CYCCOD(19) = -1
      CYCCOD(20) = -1
      CYCCDV(19) = 98
      CYCCDV(20) = 99
c
c...Initialize DIM(20) arrays
c
      do 1300 i=1,20,1
          CUTCFL(i) = cucf(i)
          CYLREG(i) = clrg(i)
          ICYLBK(i) = 0
          TOOLFL(i) = 2
 1300 continue
      TOOLFL(10) = 1
c
c...Initialize DIM(24) arrays
c
      do 1500 i=1,24,1
          CUTCCD(i) = cucd(i)
          CUTCVL(i) = cucv(i)
 1500 continue
      CUTCVL(4) = DUMMY
c
c...Initialize DIM(25) arrays
c
      do 1600 i=1,25,1
          CYLCOD(i) = clcd(i)
          CYLCDV(i) = clcv(i)
 1600 continue
c
c...Initialize DIM(30) arrays
c
      do 1700 i=1,30,1
          ICYCFL(i) = cmfl(i)
 1700 continue
c
c...Initialize KPOSMP variables
c
      DELYCD(1) = -1
      DELYCD(2) = 50
      DELYCD(3) = 0
      DELYFL(1) = 1
      DELYFL(2) = 2
      DELYFL(3) = 2
      DELYFL(4) = 2
c
      ICRDIM = 1
      ICRDIR = 2
      if (ICRHEL(3) .eq. 2) ICRHEL(3) = 0
      ICSMFL = 2
      ICSMRG = 14
      ICYCDO(1) = -1
      ICYCSW(1) = 0
      ICYCSW(3) = 1
      ICYCSW(4) = 0
      IFDRAD = 2
      IFDRCD = 52
      IPSTNF = 2
      IRWEOB = 1
      ISQBKV = 99999999
      ISLWDN = 2
      ISLWFD = 0
      ITSELF = 0
c
      LRTRCT = 1
      LSTGRP = 1
      LSTTSP = 1
c
      MCHOPT(4) = IDUMMY
      MXCKSM = 999
c
      NBOT   = 0
      if (MACHTP .eq. 2) NOTOOL = 2
      NUMEXC = 0
      NUMINF = 0
c
      POSFDF = 1
      POSFED = 1
      PSTNOF = 2
c
      RSIZFL = 3
c
      SPITAX = 1
c
      TLCBLK = 0
      TLOBLK = 0
      TLOFDR = 1
      TLOFPL = 1
      TLOSGN = 2
      TOFNXT = 0
      TURFL(1) = 1
      TURFL(2) = 1
c
c...Initialize POSMAP variables
c
      CTOFRG = 0.
      CYCJVC(1) = 1.
      CYCJVC(2) = 0.
      CYCJVC(3) = 0.
      CYCPLN(4) = DUMMY
      CYLVR(1) = 0.
c
      DELYVL = 4.
      DELYTM = .5
c
      ISEQSV = 1.
c
      LNRTOL = 0.
      LSTTN = 0.
c
      MAXTN  = 99999999.
c
      RETDIS = 0.
      RETFED(1) = 0.
      RETFED(2) = 10.
      RETPOS(1) = DUMMY
c
      SLWVR(1) = 0.
      SLWVR(2) = .001
      SLWVR(3) = 7.00
      SLWVR(4) = 7.25
      SLWVR(5) = .020
      SPIVEC(1) = 0.
      SPIVEC(2) = 0.
      SPIVEC(3) = 1.
c
      TLCTIM = 0.
      TLOBAS = 0.
      TLOFRG = 0.
      TRZX(1) = 0.
      TRZX(2) = 0.
      TSELTM = 0.
      TURDIS = 0.
c
c...Initialize CPOSMP variables
c
      LUPDAT = '03.29.91'
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  m03291
c
c   FUNCTION:  This routine changes the Units of real variables that
c              were initialized in 'u02281' to Metric.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine m03291
c
      include 'post.inc'
c
      equivalence (NUMINF,KPOSMP(1396))
c
      integer*4 NUMINF
c
      equivalence (DUMMY ,POSMAP(0003)), (INFZON,POSMAP(0071))
      equivalence (HOME  ,POSMAP(0151)), (CLRPLN,POSMAP(0161))
      equivalence (BRKVR ,POSMAP(1205))
      equivalence (LNRTOL,POSMAP(2251)), (RETDIS,POSMAP(2279))
      equivalence (RETFED,POSMAP(2280)), (SLWVR ,POSMAP(2451))
      equivalence (CYCVR ,POSMAP(2926)), (CYLVR ,POSMAP(2993))
      equivalence (TURDIS,POSMAP(3961)), (TRZX  ,POSMAP(3993))
c
      real*8 TURDIS,LNRTOL,RETDIS,RETFED(2),SLWVR(5),CYCVR(5),HOME(10),
     1       CLRPLN(4),INFZON(2,10,4),DUMMY,BRKVR(10),
     2       CYLVR(5),TRZX(2)
c
      integer*4 i,j
c
      real*8 rcnv, fcnv
c
c...Set up conversion value
c
      rcnv   = 25.4
      fcnv   = 12.0D0 * 25.4D0 / 1000.0D0
c
c...1 dimension arrays
c
      CLRPLN(4) = CLRPLN(4) * rcnv
      CYLVR(1) = CYLVR(1) * rcnv
c
      LNRTOL = LNRTOL * rcnv
c
      RETDIS = RETDIS * rcnv
      RETFED(1) = RETFED(1) * rcnv
      RETFED(2) = RETFED(2) * rcnv
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
c...3 dimension arrays
c
      do 30 i=1,3,1
          CYCVR(i) = CYCVR(i) * rcnv
   30 continue
c
c...5 dimension arrays
c
c      do 50 i=1,5,1
c          if (i .ne. 2) BRKVR(i) = BRKVR(i) * rcnv
c   50 continue
c
c...10 dimension arrays
c
      do 100 i=1,10,1
          HOME(i) = HOME(i) * rcnv
          do 95 j=1,NUMINF,1
              if (INFZON(1,i,j) .ne. DUMMY)
     1                INFZON(1,i,j) = INFZON(1,i,j) * rcnv
              if (INFZON(2,i,j) .ne. DUMMY)
     1                INFZON(2,i,j) = INFZON(2,i,j) * rcnv
   95     continue
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cnvday (cdat,kdays)
c
c   FUNCTION:  This routine converts date from 'mm.dd.yy' format
c              to approximate number of days from the year 0.
c
c   INPUT:  cdat   C*8  D1  -  Date string 'mm.dd.yy'.
c
c   OUTPUT: kdays  I*4  D1  -  Number of days.
c
c***********************************************************************
c
      subroutine cnvday (cdat,kdays)
c
      character*(*) cdat
      integer*4 kdays
c
      integer*4 inc,ipt,nd,nm,ny,ierr
c
      kdays  = 10000000
c
c...Get month
c
      inc    = index (cdat,'.')
      if (inc .eq. 0) go to 9000
      call ctoi (cdat(1:inc-1),nm,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Get day
c
      ipt    = inc + index (cdat(inc+1:),'.')
      if (inc .eq. ipt) go to 9000
      call ctoi (cdat(inc+1:ipt-1),nd,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Get year
c
      call ctoi (cdat(ipt+1:ipt+2),ny,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Fix for year greater than 1999
c
      if (ny.lt.90) ny = ny + 100
c
c...Get circa number of days
c
      kdays  = ny * 365 + (nm-1) * 31 + nd
c
c...End of routine
c
 9000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u10292
c
c   FUNCTION:  This routine updates the MDF file from the version
c              03.29.91 to the version 10.28.92.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u11092
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (KERRSV,KPOSMP(0109)), (LIMERR,KPOSMP(0110))
      equivalence (LNCRAP,KPOSMP(3212)), (ICYCFL,KPOSMP(0181))
c
      integer*4 KERRSV,LIMERR,LNCRAP,ICYCFL(30)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      KERRSV = 0
      LIMERR = 0
      LNCRAP = 2
      ICYCFL(29) = 2
c
c...Update update date
c
      LUPDAT = '11.06.92'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u02293
c
c   FUNCTION:  This routine updates the MDF file from the version
c              11.06.92 to the version 01.20.93.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u02293
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IFDRCD,KPOSMP(3213)), (IFDRCN,KPOSMP(3211))
      equivalence (MROTTV,KPOSMP(1334)), (PLCYCD,KPOSMP(0296))
c
      integer*4 IFDRCD(4),IFDRCN,MROTTV,PLCYCD(3)
c
      equivalence (FINCDV,POSMAP(1203)), (CYPCDV,POSMAP(2998))
c
      real*8 FINCDV,CYPCDV(3)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      IFDRCN = 1
      IFDRCD(1) = 52
      IFDRCD(2) = 0
      IFDRCD(3) = 0
      IFDRCD(4) = 0
      MROTTV = 1
c
c     FINCDV = 2.0
      PLCYCD(1) = 0
      PLCYCD(2) = 0
      PLCYCD(3) = 0
      CYPCDV(1) = 17
      CYPCDV(2) = 18
      CYPCDV(3) = 19
c
c...Update update date
c
      LUPDAT = '02.12.93'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u06093
c
c   FUNCTION:  This routine updates the MDF file from the version
c              04.07.93 to the version 06.01.93.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u06093
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IRETBL,KPOSMP(0817)), (IRTMAX,KPOSMP(0811))
      equivalence (IRTOUT,KPOSMP(0813)), (ISIDBL,KPOSMP(0812))
      equivalence (LRTTAD,KPOSMP(1335)), (IRTNUM,KPOSMP(1243))
c
      integer*4 IRTOUT(4),IRETBL,IRTMAX,ISIDBL,LRTTAD,IRTNUM
c
      equivalence (BLDELT,POSMAP(3593)), (BLDSTO,POSMAP(3592))
      equivalence (BLDVEC,POSMAP(3589)), (MAXIPM,POSMAP(3544))
      equivalence (MAXDPM,POSMAP(3545))
c
      real*8 BLDELT,BLDVEC(3),BLDSTO,MAXIPM,MAXDPM
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      integer*4 i
      character*8 LUPDAT
c
      IRTMAX = 4
      do 100 i=1,4
         IRTOUT(i) = 1
         if (i .gt. IRTNUM) IRTOUT(i) = 0
  100 continue
      LRTTAD = 1
      IRETBL = 0
      ISIDBL = 1
c
      BLDVEC(1) = 1.0
      BLDVEC(2) = .0
      BLDVEC(3) = .0
      BLDSTO = .0
      BLDELT = 90.01
      MAXIPM = 99999999.
      MAXDPM = 99999999.
c
c...Update update date
c
      LUPDAT = '06.30.93'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u10293
c
c   FUNCTION:  This routine updates the MDF file from the version
c              06.30.93 to the version 10.13.93.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u10293
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (PODCCD,KPOSMP(0379)), (PODACD,KPOSMP(0372))
      equivalence (PODSCD,KPOSMP(0818)), (IZIGON,KPOSMP(0370))
      equivalence (NKPOSM,KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM,KPOSMP(0173)), (IZIGAD,KPOSMP(0371))
c
      integer*4 PODCCD(2),PODACD(7),PODSCD(6),IZIGON,IZIGAD,
     -          NKPOSM,NCPOSM,NPOSMA
c
      equivalence (PODAVL,POSMAP(2494)), (PODSVL,POSMAP(3594))
c
      real*8 PODAVL(7),PODSVL(6)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      integer*4 i
      character*8 LUPDAT
c
      IZIGON = 0
      IZIGAD = 2
c
      NKPOSM = 3
      NPOSMA = 5
      NCPOSM = 1
c
      PODCCD(1) = 0
      PODCCD(2) = 0
      do 115 i=1,6
         PODACD(i) = -2
         PODSCD(i) = -2
  115 continue
      PODACD(7) = -2
      PODSCD(1) = -1
c
      PODAVL(1) = 148.
      PODAVL(2) = 149.
      PODAVL(3) = 141.
      PODAVL(4) = 142.
      PODAVL(5) = 41.
      PODAVL(6) = 40.
      PODAVL(7) = 140.
c
      PODSVL(1) = 140.
      PODSVL(2) = 146.
      PODSVL(3) = 147.
      PODSVL(4) = 144.
      PODSVL(5) = 143.
      PODSVL(6) = 145.
c
c...Update update date
c
      LUPDAT = '10.13.93'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u12193
c
c   FUNCTION:  This routine updates the MDF file from the version
c              10.13.93 to the version 12.08.93.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u12193
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ACTBLN,KPOSMP(1336)), (LTHXD ,KPOSMP(1339))
      equivalence (IFDFRC,KPOSMP(3153)), (FEDOCD,KPOSMP(3217))
      equivalence (MXGTOL,KPOSMP(4001)), (MXFLAG,KPOSMP(4002))
      equivalence (MXTRAN,KPOSMP(4003)), (ICY2FL,KPOSMP(4004))
c
      integer*4 FEDOCD(2),IFDFRC(4),ACTBLN(3),MXGTOL,MXFLAG,MXTRAN,
     -          ICY2FL(20),LTHXD
c
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
c
      real*8 TRAMAT(12),REFMAT(12)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      integer*4 i
      character*8 LUPDAT
c
      FEDOCD(1) = KPOSMP(3156)
      FEDOCD(2) = KPOSMP(3157)
c
      IFDFRC(4) = 2
      LTHXD  = 2
c
      MXGTOL = 0
      MXFLAG = 0
      MXTRAN = 0
      ACTBLN(1) = 0
      ACTBLN(2) = 0
      ACTBLN(3) = 0
      do 105 i=1,20,1
          ICY2FL(i) = 1
  105 continue
      do 115 i=1,12,1
          TRAMAT(i) = 0.d0
          REFMAT(i) = 0.d0
  115 continue
      do 125 i=4,12,4
          TRAMAT(i) = 1.d0
          REFMAT(i) = 1.d0
  125 continue
c
c...Update update date
c
      LUPDAT = '12.08.93'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u08394
c
c   FUNCTION:  This routine updates the MDF file from the version
c              12.08.93 to the version 08.26.94.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u08394
c
      include 'post.inc'
c
      equivalence (MODROT,KPOSMP(4031)), (MDTLIN,KPOSMP(4032))
      equivalence (NOPSON,KPOSMP(4050))
c
      integer*4 MODROT,MDTLIN,NOPSON
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      NOPSON = 0
      MODROT = 0
      MDTLIN = 0
c
c...Update update date
c
      LUPDAT = '08.26.94'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u12294
c
c   FUNCTION:  This routine updates the MDF file from the version
c              08.26.94 to the version 12.23.94.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u12294
c
      include 'post.inc'
c
      equivalence (IPHFIL,KPOSMP(0369))
      equivalence (IRETMD,KPOSMP(1340)), (IRTRTE,KPOSMP(1343))
      equivalence (IRETFL,KPOSMP(1345)), (IRPRET,KPOSMP(3219))
c
      integer*4 IRETMD(2),IRPRET,IPHFIL,IRTRTE(2),IRETFL
c
      equivalence (CUTCVR,POSMAP(2402))
c
      real*8 CUTCVR(8)
c
      equivalence (LUPDAT,CPOSMP(3001)), (LSTBLK,CPOSMP(3089))
c
      character*8 LUPDAT
      character*132 LSTBLK
c
c...Udate KPOSMP variables
c
      IPHFIL = -1
      IRETFL = 0
      IRETMD(1) = 1
      IRETMD(2) = 4
      IRPRET = 2
      IRTRTE(1) = 1
      IRTRTE(2) = 1
c
c...Update CPOSMP variables
c
      CUTCVR(5) = 0
c
c...Update CPOSMP variables
c
      LSTBLK = ' '
c
c...Update revision date
c
      LUPDAT = '12.23.94'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u08025
c
c   FUNCTION:  This routine updates the MDF file from the version
c              12.23.94 to the version 08.02.95.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u08025
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NCUT  ,KPOSMP(0062)), (NCUTDS,KPOSMP(0063))
      equivalence (ISACIR,KPOSMP(1346)), (ICRPLF,KPOSMP(1397))
      equivalence (ICOLSW,KPOSMP(3146))
c
      integer*4 NCUT,NCUTDS,ISACIR,ICRPLF,ICOLSW
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update revision date
c
      LUPDAT = '08.04.95'
c
c...Udate KPOSMP variables
c
      ICRPLF = 1
      ISACIR = 0
      NCUT   = 0
      NCUTDS = 0
      ICOLSW = 2
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u11275
c
c   FUNCTION:  This routine updates the MDF file from the version
c              08.04.95 to the version 11.27.95.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u11275
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NCONTB,KPOSMP(1347))
c
      integer*4 NCONTB
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update revision date
c
      LUPDAT = '11.27.95'
c
c...Udate KPOSMP variables
c
      NCONTB = 1
c
c...End of routine
c
 8000 return
      end
c***********************************************************************
c
c   SUBROUTINE:  u01396
c
c   FUNCTION:  This routine updates the MDF file from the version
c              11.27.95 to the version 01.24.96.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u01396
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICUTYP,KPOSMP(0064)), (IUNIT ,KPOSMP(0087))
      equivalence (ALNCUT,KPOSMP(4040)), (ALNMOV,KPOSMP(4045))
      equivalence (SMOKON,KPOSMP(4071)), (SMOKAN,KPOSMP(4072))
      equivalence (SMOCCD,KPOSMP(4073)), (SMONRN,KPOSMP(4075))
      equivalence (SMONCD,KPOSMP(4076)), (SMOORN,KPOSMP(4079))
      equivalence (SMOOCD,KPOSMP(4080)), (SMOSEL,KPOSMP(4084))
c
      integer*4 ICUTYP,ALNCUT,ALNMOV,SMOKON,SMOKAN,SMOCCD(2),
     -          SMONRN,SMOORN,SMONCD(3),SMOOCD(3),SMOSEL,IUNIT
c
      equivalence (CUTOFS,POSMAP(0758))
      equivalence (ALNDIS,POSMAP(4444)), (SMOCVL,POSMAP(4448))
      equivalence (SMONVL,POSMAP(4450)), (SMOOVL,POSMAP(4453))
      equivalence (ALNMPS,POSMAP(4461)), (SMORAN,POSMAP(4464))
      equivalence (SMODIS,POSMAP(4465)), (FWDDIR,POSMAP(4580))
c
      real*8 ALNDIS(3),ALNMPS(3),SMOCVL(2),SMORAN,SMODIS,SMONVL(3),
     -       SMOOVL(3),CUTOFS,FWDDIR(3)
c
      equivalence (LUPDAT,CPOSMP(3001)), (CUTSYM,CPOSMP(3221))
c
      character*8 LUPDAT
      character*20 CUTSYM
c
      real*8 rprm
c
      rprm   = 1.d0
      if (IUNIT .eq. 2) rprm = 25.4
c
c...Update revision date
c
      LUPDAT = '02.06.96'
c
c...Udate KPOSMP variables
c
      ALNMOV = 0
      ALNDIS(1) = .059055119 * rprm
      ALNDIS(2) = 4. * ALNDIS(1)
      ALNDIS(3) = 1.0
      ALNCUT = 0
      ALNMPS(1) = .984251969 * rprm
      ALNMPS(2) = 196.8503938 * rprm
      ALNMPS(3) = .25 * ALNMPS(2)
      SMOKON = 2
      SMOKAN = 0
      SMOCCD(1) = -1
      SMOCCD(2) = -1
      SMOCVL(1) = 36.0
      SMOCVL(2) = 37.0
      SMONRN = 1
      SMOORN = 2
      SMONCD(1) = 10
      SMONCD(3) = 11
      SMOOCD(1) = 10
      SMOOCD(2) = 12
      SMORAN = 15.0
      SMONVL(1) = 0.
      SMONVL(3) = 0.
      SMONVL(3) = SMORAN
      SMOOVL(1) = 0.
      SMOOVL(2) = 0.
      SMODIS = .00393700788 * rprm
      FWDDIR(1) = 1.
      FWDDIR(2) = 0.
      FWDDIR(3) = 0.
c
      ICUTYP = 0
      CUTOFS = 0.
      CUTSYM = ' '
c
c...End of routine
c
 8000 return
      end
c***********************************************************************
c
c   SUBROUTINE:  u03296
c
c   FUNCTION:  This routine updates the MDF file from the version
c              02.06.96 to the version 03.23.96.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u03296
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087)), (LTHPCL,KPOSMP(1899))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (LCRFMT,KPOSMP(4103)), (LFLMCD,KPOSMP(4104))
      equivalence (LDLMCD,KPOSMP(4105)), (MILREG,KPOSMP(4106))
      equivalence (LFDLCD,KPOSMP(4107)), (LPCREG,KPOSMP(4108))
      equivalence (LTHREG,KPOSMP(4110))
      equivalence (LFIRCD,KPOSMP(4112)), (LDIRCD,KPOSMP(4118))
      equivalence (MTPDYN,KPOSMP(4126))
      equivalence (IRFSUP,KPOSMP(4136)), (IRDSUP,KPOSMP(4137))
      equivalence (FRAPCD,KPOSMP(4138)), (DRAPCD,KPOSMP(4139))
      equivalence (LTCREG,KPOSMP(4195)), (LTPREG,KPOSMP(4201))
c
      integer*4 LTHPCL(2),LCRFMT,LDLMCD,MILREG,LFLMCD,LFDLCD,IUNIT,
     -          LFIRCD(6),LDIRCD(6),LPCREG(2),LTHREG,LTCREG(3),
     -          MACHTP,LTPREG(2),FRAPCD,DRAPCD,IRFSUP,IRDSUP,MTPDYN
c
      equivalence (CFRCDV,POSMAP(4595)), (CDRCDV,POSMAP(4597))
      equivalence (FLMCDV,POSMAP(4600)), (DLMCDV,POSMAP(4601))
      equivalence (GLMCDV,POSMAP(4602)), (MILTOL,POSMAP(4879))
      equivalence (TLMCDV,POSMAP(4880)), (TLLCDV,POSMAP(4881))
      equivalence (FRAPVL,POSMAP(4882)), (DRAPVL,POSMAP(4883))
c
      real*8 CFRCDV(2),CDRCDV(2),FLMCDV,DLMCDV,GLMCDV(2),MILTOL,
     -       TLMCDV,TLLCDV,FRAPVL,DRAPVL
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      real*8 rprm
c
      rprm   = 1.d0
      if (IUNIT .eq. 2) rprm = 25.4
c
      call gtmach (MTPDYN)
      if (MACHTP .eq. 4) then
         LTHPCL(1) = 1
         LTHPCL(2) = 1
         IRFSUP = 1
      else
         LTHPCL(1) = 2
         LTHPCL(2) = 2
         IRFSUP = 2
      end if
      IRDSUP = 2
      LCRFMT = 2
      LFLMCD = 28
      LDLMCD = 28
      LPCREG(1) = -1
      LPCREG(2) = -1
      MILREG = -2
      LTHREG = -2
      LTPREG(1) = 63
      LTPREG(2) = 64
      LTCREG(1) = 4
      LTCREG(2) = 5
      LTCREG(3) = 6
      LFDLCD = 36
      LFIRCD(1) = 28
      LFIRCD(2) = 28
      LFIRCD(3) = 30
      LFIRCD(4) = 32
      LFIRCD(5) = 34
      LFIRCD(6) = 52
      LDIRCD(1) = 28
      LDIRCD(2) = 28
      LDIRCD(3) = 30
      LDIRCD(4) = 32
      LDIRCD(5) = 34
      LDIRCD(6) = 52
      FRAPCD = 28
      DRAPCD = 28
c
      CFRCDV(1) = 12.
      CFRCDV(2) = 13.
      CDRCDV(1) = 22.
      CDRCDV(2) = 23.
      FLMCDV = 11.
      DLMCDV = 21.
      TLMCDV = 19.
      TLLCDV = 20.
      GLMCDV(1) = 79.
      GLMCDV(2) = 79.
      FRAPVL = 10.
      DRAPVL = 20.
      MILTOL = .001 * rprm
c
c...Update revision date
c
      LUPDAT = '03.23.96'
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  u07396
c
c   FUNCTION:  This routine updates the MDF file from the version
c              03.23.96 to the version 07.30.96.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u07396
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICRPFL,KPOSMP(1370)), (ICRPLS,KPOSMP(1398))
      equivalence (BSPLSW,KPOSMP(4084)), (ISBSPL,KPOSMP(4085))
      equivalence (BSPLFL,KPOSMP(4088)), (BSPCOD,KPOSMP(4089))
      equivalence (BSPPCD,KPOSMP(4090)), (BSPFRC,KPOSMP(4091))
c
      integer*4 BSPCOD,ISBSPL,BSPLSW,BSPLFL,BSPPCD,BSPFRC(2),
     -          ICRPFL,ICRPLS(3)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (BSPCDV,POSMAP(4901)), (BSPTOL,POSMAP(4902))
      equivalence (VTOLER,POSMAP(4911))
c
      real*8 BSPCDV,BSPTOL(2),DUMMY,VTOLER
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      ISBSPL = 0
      BSPLSW = 2
      BSPLFL = 2
      BSPCOD = 12
      BSPCDV = DUMMY
      BSPPCD = 50
      BSPFRC(1) = 1
      BSPFRC(2) = 1
      BSPTOL(1) = .001
      BSPTOL(2) = .01
c
      if (ICRPFL .eq. 1) then
         ICRPLS(1) = 1
         ICRPLS(2) = 1
         ICRPLS(3) = 1
      else
         ICRPLS(1) = 1
         ICRPLS(2) = 2
         ICRPLS(3) = 2
      end if
      VTOLER = .0001
c
c...Update revision date
c
      LUPDAT = '10.16.96'
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  u12126
c
c   FUNCTION:  This routine updates the MDF file from the version
c              03.23.96 to the version 07.30.96.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u01087
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IFWDFL,KPOSMP(0832)), (IFWSFL,KPOSMP(0833))
      equivalence (ALNMOD,KPOSMP(4049)), (MOTEXP,KPOSMP(4211))
c
      integer*4 ALNMOD,MOTEXP,IFWDFL,IFWSFL
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      ALNMOD = 1
      MOTEXP = 0
      IFWSFL = 0
      IFWDFL = 0
c
c...Update revision date
c
      LUPDAT = '01.08.97'
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  u01177
c
c   FUNCTION:  This routine updates the MDF file from the version
c              00.00.96 to the version 01.17.97.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u01177
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (LTHXY ,KPOSMP(1225)), (LTHXYS,KPOSMP(4129))
      equivalence (MODCYL,KPOSMP(4204)), (LUNREG,KPOSMP(4205))
      equivalence (ISRCYL,KPOSMP(4206)), (MCYNUM,KPOSMP(4207))
      equivalence (ISRVAL,KPOSMP(4208)), (ISXVAL,KPOSMP(4209))
      equivalence (PLNCOD,KPOSMP(4222)), (NCPLAN,KPOSMP(4226))
c
      integer*4 MODCYL,LUNREG,ISRCYL,MCYNUM,PLNCOD(4),ISRVAL,
     -          NCPLAN(3),ISXVAL,LTHXY,LTHXYS
c
      equivalence (PLNDWL,POSMAP(2212))
c
      real*8 PLNDWL
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      MODCYL = 0
      LUNREG = 0
      ISRCYL = 2
      ISRVAL = 2
      ISXVAL = 2
      MCYNUM = 1
      LTHXYS = LTHXY
c
c...move old addresses to new in KPOSMP & POSMAP
c
      PLNCOD(1) = KPOSMP(1385)
      PLNCOD(2) = KPOSMP(1386)
      PLNCOD(3) = KPOSMP(1387)
      PLNCOD(4) = 0
      NCPLAN(1) = 6
      NCPLAN(2) = 3
      NCPLAN(3) = 1
      PLNDWL = POSMAP(2211)
c
c...Update revision date
c
      LUPDAT = '04.11.97'
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  u04248
c
c   FUNCTION:  This routine updates the MDF file from the version
c              06.02.97 to the version 04.24.98.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u04248
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087))
      equivalence (ICLIPS,KPOSMP(0112)), (NPLANE,KPOSMP(0113))
      equivalence (LINTSW,KPOSMP(0114)), (ICRHEL,KPOSMP(1373))
      equivalence (CIRCOD,KPOSMP(1378))
      equivalence (ICMBSW,KPOSMP(1385)), (ICSPRE,KPOSMP(1386))
      equivalence (IJKROT,KPOSMP(1739))
c
      integer*4 IUNIT,ICMBSW,ICSPRE,LINTSW,ICLIPS,NPLANE,IJKROT,
     -          ICRHEL(5),CIRCOD(6)
c
      equivalence (CIRCDV,POSMAP(2206))
      equivalence (CIRTOS,POSMAP(2241)), (HELCDV,POSMAP(2242))
c
      real*8 CIRTOS,HELCDV(2),CIRCDV(2)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      ICSPRE = 0
      ICMBSW = 0
      LINTSW = 0
      ICLIPS = 0
      NPLANE = 0
      IJKROT = 2
      ICRHEL(4) = CIRCOD(1)
      ICRHEL(5) = CIRCOD(2)
      HELCDV(1) = CIRCDV(1)
      HELCDV(2) = CIRCDV(2)
c
      CIRTOS = .001
      if (IUNIT .eq. 2) CIRTOS = .025
c
c...Update revision date
c
      LUPDAT = '04.24.98'
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  u09149
c
c   FUNCTION:  This routine updates the MDF file from the version
c              04.23.99 to the version 09.14.99.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u09149
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087))
c
      integer*4 IUNIT
c
      equivalence (CIRTOS,POSMAP(2241)), (CUTCFL,KPOSMP(3251))
c
      integer*4 CUTCFL(20),CIRTOS
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      CUTCFL(16) = 2
      if (CIRTOS .eq. 0.) then
          CIRTOS = .001
          if (IUNIT .eq. 2) CIRTOS = .025
      endif
c
c...Update revision date
c
      LUPDAT = '09.14.99'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u08310
c
c   FUNCTION:  This routine updates the MDF file from the version
c              09.14.99 to the version 08.04.00.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u08310
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087))
c
      integer*4 IUNIT
c
      equivalence (MCHOPT,KPOSMP(0308)), (ICIDLT,KPOSMP(1741))
      equivalence (ICYDLT,KPOSMP(1742)), (BSPFRC,KPOSMP(4091))
c
      integer*4 MCHOPT(20),ICIDLT,ICYDLT,BSPFRC(4)
c
      equivalence (CIRTOS,POSMAP(2241))
      equivalence (SPNLMT,POSMAP(3309)), (SPMLMT,POSMAP(4930))
c
      real*8 SPNLMT(2,4),SPMLMT(2,3),CIRTOS
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i
c
      ICIDLT = 2
      ICYDLT = 2
      BSPFRC(3) = 2
      BSPFRC(4) = 2
      MCHOPT(5) = 72
c
      do 100 i=1,3,1
          SPMLMT(1,i) = SPNLMT(1,i)
          SPMLMT(2,i) = SPNLMT(2,i)
  100 continue
      if (CIRTOS .eq. 0.) then
          CIRTOS = .001
          if (IUNIT .eq. 2) CIRTOS = .025
      endif
c
c...Update revision date
c
      LUPDAT = '08.31.00'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u10112
c
c   FUNCTION:  This routine updates the MDF file from the version
c              08.31.00 to the version 10.11.02.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u10112
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ILINRP,KPOSMP(1348)), (IMXDRP,KPOSMP(1743))
c
      integer*4 ILINRP,IMXDRP
c
      equivalence (REGST ,CPOSMP(0988)), (OREGST,CPOSMP(0988))
      equivalence (OREGEN,CPOSMP(1318)), (REGEN ,CPOSMP(3407))
      equivalence (LUPDAT,CPOSMP(3001)), (PU1EXT,CPOSMP(4067))
c
      character*5 OREGST(66),OREGEN(66)
      character*8 LUPDAT
      character*10 REGST(66),REGEN(66)
      character*80 PU1EXT
c
      integer*4 i
c
      character*10 lbuf(66)
c
      ILINRP = 2
      IMXDRP = 1
c
      PU1EXT = ' '
c
      do 100 i=1,66,1
          REGEN(i) = OREGEN(i)
          lbuf(i)   = OREGST(i)
  100 continue
      do 110 i=1,66,1
          REGST(i) = lbuf(i)
  110 continue
c
c...Update revision date
c
      LUPDAT = '10.11.02'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u05193
c
c   FUNCTION:  This routine updates the MDF file from the version
c              10.11.02 to the version 05.19.03.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u05193
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ROTDCD,KPOSMP(1405))
c
      integer*4 ROTDCD(2,4)
c
      equivalence (ROTDVL,POSMAP(2041)), (FDLCNV,POSMAP(3013))
c
      real*8 ROTDVL(2,4),FDLCNV
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i
c
      FDLCNV = 10.
c
      do 100 i=1,4,1
          ROTDCD(1,i) = 0
          ROTDCD(2,i) = 0
          ROTDVL(1,i) = 0.
          ROTDVL(2,i) = 0.
  100 continue
c
c...Update revision date
c
      LUPDAT = '05.19.03'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u012404
c
c   FUNCTION:  This routine updates the MDF file from the version
c              05.19.03 to the version 01.24.04.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u02034
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NSHANK,KPOSMP(0065)), (IUNIT ,KPOSMP(0087))
      equivalence (IRTDUP,KPOSMP(1413))
      equivalence (IRTDAC,KPOSMP(1417)), (IACCFD,KPOSMP(1744))
      equivalence (IACCFL,KPOSMP(1745))
c
      integer*4 NSHANK,IRTDUP(4),IRTDAC(9,4),IACCFD,IACCFL(2),IUNIT
c
      equivalence (ACCFED,POSMAP(4291)), (MAXVEL,POSMAP(4293))
      equivalence (AXSVEL,POSMAP(4294)), (ACCSTP,POSMAP(4295))
c
      real*8 ACCFED,MAXVEL,AXSVEL,ACCSTP(2)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i,j
c
      real*8 cnv
c
      cnv    = 1.0
      if (IUNIT .eq. 2) cnv = 25.4
c
      NSHANK = 0
c
      do 200 i=1,4,1
          IRTDUP(i) = 1
          IRTDAC(1,i) = 1
          do 100 j=2,9,1
              IRTDAC(j,i) = 0
  100     continue
  200 continue
c
c...Acceleration blocks
c
      IACCFD = 0
      IACCFL(1) = 2
      IACCFL(2) = 2
      ACCFED = 0.
      MAXVEL = 20. * cnv
      AXSVEL = 8. * cnv
      ACCSTP(1) = 7. * cnv
      ACCSTP(2) = 25. * cnv
c
c...Update revision date
c
      LUPDAT = '02.03.04'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u06094
c
c   FUNCTION:  This routine updates the MDF file from the version
c              02.03.04 to the version 06.09.04.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u06094
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (LUPDAT,CPOSMP(3001)), (PU1EXT,CPOSMP(4067))
c
      character*8 LUPDAT
      character*80 PU1EXT
c
      PU1EXT = PU1EXT(1:20)
c
c...Update revision date
c
      LUPDAT = '06.09.04'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u09134
c
c   FUNCTION:  This routine updates the MDF file from the version
c              07.26.04 to the version 09.13.04.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u09134
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (MACCOD,KPOSMP(0801))
      equivalence (LSTREG,KPOSMP(0857)), (MACPRG,KPOSMP(0858))
      equivalence (MACPSW,KPOSMP(0859)), (IC3AXF,KPOSMP(1747))
      equivalence (REGBNC,KPOSMP(2001)), (REGENC,KPOSMP(2067))
c
      integer*4 IC3AXF(10),ICYCFL(30),MACCOD(5),LSTREG,MACPRG,
     1          MACPSW(30),REGBNC(66),REGENC(66)
c
      equivalence (MACCDV,POSMAP(1184)), (C3XCDV,POSMAP(2244))
c
      real*8 C3XCDV(2),MACCDV(4)
c
      equivalence (REGST ,CPOSMP(0988)), (LUPDAT,CPOSMP(3001))
      equivalence (REGEN ,CPOSMP(3407))
c
      character*8 LUPDAT
      character*10 REGST(66),REGEN(66)
c
      integer*4 i
c
      IC3AXF(1) = 2
      IC3AXF(2) = 2
      IC3AXF(3) = 10
      IC3AXF(4) = 10
      IC3AXF(5) = 31
      IC3AXF(6) = 33
      IC3AXF(7) = 35
c
      ICYCFL(30) = 2
c
      C3XCDV(1) = 0
      C3XCDV(2) = 0
c
      LSTREG = 0
c
      MACCOD(1) = 47
      MACCDV(1) = 0
      MACPRG = -1
c
      do 100 i=1,30,1
          MACPSW(i) = 0
  100 continue
c
      do 200 i=1,66,1
C WNT-START
          if (REGST(i)(REGBNC(i):REGBNC(i)) .eq. '\') then
              REGBNC(i) = REGBNC(i) + 1
              REGST(i)(REGBNC(i):REGBNC(i)) = '\'
          endif
          if (REGEN(i)(REGENC(i):REGENC(i)) .eq. '\') then
              REGENC(i) = REGENC(i) + 1
              REGEN(i)(REGENC(i):REGENC(i)) = '\'
          endif
C WNT-END
C SGI-SUN-HPX-IBM-START
C         if (REGST(i)(REGBNC(i):REGBNC(i)) .eq. '\\') then
C             REGBNC(i) = REGBNC(i) + 1
C             REGST(i)(REGBNC(i):REGBNC(i)) = '\\'
C         endif
C         if (REGEN(i)(REGENC(i):REGENC(i)) .eq. '\\') then
C             REGENC(i) = REGENC(i) + 1
C             REGEN(i)(REGENC(i):REGENC(i)) = '\\'
C         endif
C SGI-SUN-HPX-IBM-END
  200 continue
c
c...Update revision date
c
      LUPDAT = '09.13.04'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u10294
c
c   FUNCTION:  This routine updates the MDF file from the version
c              09.13.04 to the version 10.29.04.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u10294
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGSW ,KPOSMP(0405))
      equivalence (IREGST,KPOSMP(0497)), (REGFRC,KPOSMP(0603))
      equivalence (IRGOUT,KPOSMP(0695))
      equivalence (XFMFL ,KPOSMP(0921)), (XFMCD ,KPOSMP(0926))
      equivalence (XFMREG,KPOSMP(0931)), (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (IFDCTP,KPOSMP(3157))
      equivalence (REGENC,KPOSMP(3522))
      equivalence (IREGVL,KPOSMP(3614)), (REGORD,KPOSMP(3707))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 XFMFL(5),XFMCD(5),XFMREG(10),IREGST(MAXFMT),
     1          IREGVL(MAXFMT),IRGOUT(MAXFMT),REGBNC(MAXFMT),
     2          REGENC(MAXFMT),REGFRC(MAXFMT),REGORD(MAXFMT),
     3          REGSW(MAXFMT),IFDCTP
c
      equivalence (OIREGVL,KPOSMP(0471)), (OIREGST,KPOSMP(0537))
      equivalence (OREGORD,KPOSMP(0669)), (OIRGOUT,KPOSMP(0735))
      equivalence (OREGENC,KPOSMP(2067))
c
      integer*4 OIREGST(66),OIREGVL(66),OIRGOUT(66),OREGENC(66),
     1          OREGORD(66)
c
      equivalence (REGSTO,POSMAP(1032)), (XFMVL ,POSMAP(1189))
      equivalence (REGVAL,POSMAP(5000))
c
      real*8 XFMVL(5),REGSTO(MAXFMT),REGVAL(MAXFMT)
c
      equivalence (OREGVAL,POSMAP(1098))
c
      real*8 OREGVAL(66)
c
      equivalence (LUPDAT,CPOSMP(3001)), (REGST ,CPOSMP(4147))
      equivalence (REGEN ,CPOSMP(5067))
c
      character*8 LUPDAT
      character*10 REGST(MAXFMT),REGEN(MAXFMT)
c
      equivalence (OREGST ,CPOSMP(0988)), (OREGEN ,CPOSMP(3407))
c
      character*10 OREGST(66),OREGEN(66)
c
      integer*2 ifmt(10)
      integer*4 i,j,iord(66),ivl(66),ienc(66),ist(66),iout(66)
c
      real*8 rval(66)
c
      character*10 lreg(26),lst(66),len(66)
c
      data ifmt /3,0,1,3,4,4,3,0,1,0/
      data lreg /'AA','AB','AC','AD','AE','AF','AG','AH','AI','AJ','AK',
     1           'AL','AM','AN','AO','AP','AQ','AR','AS','AT','AU','AV',
     2           'AW','AX','AY','AZ'/
c
      IFDCTP = 2
c
      XFMFL(1) = 2
      XFMFL(2) = 1
      XFMFL(3) = 2
      XFMFL(4) = 0
      XFMFL(5) = 0
c
      XFMCD(1) = 0
      XFMCD(2) = 0
      XFMCD(3) = 0
      XFMCD(4) = 0
      XFMCD(5) = 0
      XFMVL(1) = 0
      XFMVL(2) = 0
      XFMVL(3) = 0
      XFMVL(4) = 0
      XFMVL(5) = 0
c
      XFMREG(1) = 62
      XFMREG(2) = 64
      XFMREG(3) = 66
      XFMREG(4) = 62
      XFMREG(5) = 64
      XFMREG(6) = 66
      XFMREG(7) = 30
      XFMREG(8) = 32
      XFMREG(9) = 34
      XFMREG(10) = 52
c
      do 100 i=1,66,1
          iord(i) = OREGORD(i)
          ivl(i) = OIREGVL(i)
          ienc(i) = OREGENC(i)
          ist(i) = OIREGST(i)
          iout(i) = OIRGOUT(i)
          rval(i) = OREGVAL(i)
          lst(i) = OREGST(i)
          len(i) = OREGEN(i)
  100 continue
c
      do 200 i=1,66,1
          REGORD(i) = iord(i)
          IREGVL(i) = ivl(i)
          REGENC(i) = ienc(i)
          IREGST(i) = ist(i)
          IRGOUT(i) = iout(i)
          REGVAL(i) = rval(i)
          REGST(i) = lst(i)
          REGEN(i) = len(i)
  200 continue
c
      do 500 i=67,MAXFMT,1
          do 480 j=1,10,1
              FMTDES(j,i) = ifmt(j)
  480     continue
          IREGST(i) = 0
          IREGVL(i) = 0
          REGBNC(i) = 2
          REGENC(i) = 0
          REGFRC(i) = 0
          REGSW(i)  = 0
          REGSTO(i) = 0
          REGVAL(i) = 0
          REGST(i) = lreg(i-66)
          REGEN(i) = ' '
          REGORD(i) = i
  500 continue
c
c...Update revision date
c
      LUPDAT = '10.29.04'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u12144
c
c   FUNCTION:  This routine updates the MDF file from the version
c              11.17.04 to the version 12.14.04.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u12144
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MRTFRC,KPOSMP(1387))
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
      equivalence (INMSFL,KPOSMP(3322)), (INMDEF,KPOSMP(3323))
      equivalence (INMLFL,KPOSMP(3324))
c
      integer*4 CUTCFL(20),CUTCCD(30),INMSFL,INMDEF,INMLFL,MRTFRC
c
      equivalence (CUTCVL,POSMAP(2410)), (NMLVEC,POSMAP(4492))
      equivalence (NMLSAV,POSMAP(4495))
c
      real*8 CUTCVL(30),NMLVEC(3),NMLSAV(3)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      CUTCFL(17) = 2
c
      CUTCCD(24) = -1
      CUTCCD(25) = 52
      CUTCCD(26) = 67
      CUTCCD(27) = 68
      CUTCCD(28) = 69
c
      INMDEF = 0
      INMLFL = 0
      INMSFL = 0
c
      MRTFRC = 2
c
      CUTCVL(24) = 141
c
      NMLVEC(1) = 0.
      NMLVEC(2) = 0.
      NMLVEC(3) = 1.
      NMLSAV(1) = 0.
      NMLSAV(2) = 0.
      NMLSAV(3) = 1.
c
c...Update revision date
c
      LUPDAT = '12.14.04'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u03155
c
c   FUNCTION:  This routine updates the MDF file from the version
c              12.14.04 to the version 03.15.05.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u03155
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (CLMPUB,KPOSMP(1757))
c
      integer*4 CLMPUB(2,10)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i
c
      do 100 i=1,10,1
          CLMPUB(1,i) = 0
          CLMPUB(2,i) = 0
  100 continue
c
c...Update revision date
c
      LUPDAT = '03.15.05'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u07135
c
c   FUNCTION:  This routine updates the MDF file from the version
c              03.15.05 to the version 06.23.05.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u07135
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICY2FL,KPOSMP(4004))
c
      integer*4 ICY2FL(20)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      ICY2FL(3) = 2
c
c...Update revision date
c
      LUPDAT = '07.13.05'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u01256
c
c   FUNCTION:  This routine updates the MDF file from the version
c              07.13.05 to the version 01.25.06.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u01256
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (OCRHEL,KPOSMP(1373)), (ICRHEL,KPOSMP(4250))
c
      integer*4 OCRHEL(5),ICRHEL(10),MCHOPT(20)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i
c
      do 100 i=1,5,1
          ICRHEL(i) = OCRHEL(i)
          ICRHEL(i+5) = 0
  100 continue
c
      MCHOPT(6) = 1
c
c...Update revision date
c
      LUPDAT = '01.25.06'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u04196
c
c   FUNCTION:  This routine updates the MDF file from the version
c              01.25.05 to the version 04.19.06.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u04196
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087)), (LNRADJ,KPOSMP(1277))
      equivalence (IRTSHF,KPOSMP(1374)), (IRSRTE,KPOSMP(1375))
      equivalence (ACHEAD,KPOSMP(1645)), (IACFLG,KPOSMP(1648))
c
      integer*4 ACHEAD,IACFLG(5),IRTSHF,IRSRTE(3),LNRADJ,IUNIT
c
      equivalence (ORTFED,POSMAP(2280)), (LNRATL,POSMAP(2286))
      equivalence (RETFED,POSMAP(3239)), (RTSHFD,POSMAP(4947))
c
      real*8 RTSHFD,ORTFED(3),RETFED(4),LNRATL(5)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      real*8 cnv
c
      cnv    = 1.0
      if (IUNIT .eq. 2) cnv = 25.4
c
      ACHEAD = 3
      IACFLG(1) = 1
      IACFLG(2) = 1
      IACFLG(3) = 1
      IACFLG(4) = 1
c
      IRSRTE(3) = 0
      IRTSHF = 1
      RTSHFD = 0.
c
      RETFED(1) = ORTFED(1)
      RETFED(2) = ORTFED(2)
      RETFED(3) = ORTFED(3)
      RETFED(4) = 10. * cnv
c
      LNRADJ = 2
      LNRATL(1) = .001 * cnv
      LNRATL(2) = 1.
      LNRATL(3) = 30.
      LNRATL(4) = 3. * cnv
c
c...Update revision date
c
      LUPDAT = '04.19.06'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u06126
c
c   FUNCTION:  This routine updates the MDF file to the version
c              06.12.06.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u06126
c
      include 'post.inc'
c
      equivalence (RTRMCD,KPOSMP(0093))
c
      integer*4 RTRMCD(4)
c
      equivalence (RTRMMX,POSMAP(4360)), (RTRMVL,POSMAP(4364))
c
      real*8 RTRMMX(4),RTRMVL(4)
c
      equivalence (LSTBLK,CPOSMP(0988)), (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
      character*512 LSTBLK
c
      integer*4 i
c
c...Update Maximum rotary axis register values
c
      do 100 i=1,4,1
          RTRMCD(i) = 0
          RTRMVL(i) = 0.
          RTRMMX(i) = 99999999.0d0
  100 continue
c
c...Update CPOSMP variables
c
      LSTBLK = ' '
c
c...Update revision date
c
      LUPDAT = '06.12.06'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u11136
c
c   FUNCTION:  This routine updates the MDF file to the version
c              11.13.06.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u11136
c
      include 'post.inc'
c
      equivalence (ICUTYP,KPOSMP(0064)), (NSHANK,KPOSMP(0067))
c
      integer*4 ICUTYP(3),NSHANK
c
      equivalence (CUTOFS,POSMAP(0766))
c
      real*8 CUTOFS(4,3)
c
      equivalence (LUPDAT,CPOSMP(3001)), (CUTSYM,CPOSMP(3221))
c
      character*8 LUPDAT
      character*20 CUTSYM(3)
c
      integer*4 i
c
c...Update Cutter parameters
c
      do 50 i=1,3,1
          ICUTYP(i) = 0.
          CUTSYM(i) = ' '
   50 continue
c
      do 100 i=1,4,1
          CUTOFS(i,1) = 0.
          CUTOFS(i,2) = 0.
          CUTOFS(i,3) = 0.
  100 continue
c
c...Update revision date
c
      LUPDAT = '11.13.06'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u02027
c
c   FUNCTION:  This routine updates the MDF file to the version
c              02.02.07.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u02027
c
      include 'post.inc'
c
      equivalence (XFMFL ,KPOSMP(0921))
      equivalence (XFMCD ,KPOSMP(0946)), (OXFMCD,KPOSMP(0926))
c
      integer*4 XFMFL(10),XFMCD(5),OXFMCD(5)
c
      equivalence (FRTVAL,POSMAP(3014))
c
      real*8 FRTVAL(2)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i
c
c...Update Xform parameters
c
      do 50 i=1,5,1
          XFMCD(i) = OXFMCD(i)
          XFMFL(i+5) = 2
   50 continue
c
c...Update feed rate constants
c
      FRTVAL(1) = 1.
      FRTVAL(2) = 1.
c
c...Update revision date
c
      LUPDAT = '02.02.07'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u11137
c
c   FUNCTION:  This routine updates the MDF file to the version
c              11.13.07.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u11137
c
      include 'post.inc'
c
      equivalence (IFDADJ,KPOSMP(3243))
c
      integer*4 IFDADJ
c
      equivalence (LUPDAT,CPOSMP(3001)), (CUTSYM,CPOSMP(3009))
c
      character*8 LUPDAT
      character*80 CUTSYM(3)
c
c...Update new/moved variables
c
      IFDADJ = 1
      CUTSYM(1) = ' '
      CUTSYM(2) = ' '
      CUTSYM(3) = ' '
c
c...Update revision date
c
      LUPDAT = '11.13.07'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u01309
c
c   FUNCTION:  This routine updates the MDF file to the version
c              01.30.09.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u01309
c
      include 'post.inc'
c
      equivalence (MLNFRC,KPOSMP(1373)), (IC3AXF,KPOSMP(1747))
c
      integer*4 MLNFRC,IC3AXF(10)
c
      equivalence (MSGST ,CPOSMP(0976)), (OMSGST,CPOSMP(0976))
      equivalence (MSGEN ,CPOSMP(1500)), (OMSGEN,CPOSMP(0981))
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*5 OMSGST,OMSGEN
      character*8 LUPDAT
      character*10 MSGST,MSGEN
c
c...Update new/moved variables
c
      IC3AXF(9) = 1
      MLNFRC = 2
      MSGEN  = OMSGEN
      MSGST  = OMSGST
c
c...Update revision date
c
      LUPDAT = '01.30.09'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u05139
c
c   FUNCTION:  This routine updates the MDF file to the version
c              05.13.09.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u05139
c
      include 'post.inc'
c
      equivalence (MACH  ,KPOSMP(0086)), (MCHOPT,KPOSMP(0308))
      equivalence (IPRDES,KPOSMP(1154))
c
      integer*4 MACH,MCHOPT(20),IPRDES(2,10)
c
      equivalence (LMNAME,CPOSMP(0141)), (LUPDAT,CPOSMP(3001))
      equivalence (PDFFIL,CPOSMP(3249)), (MDESC ,CPOSMP(3853))
c
      character*8 LUPDAT
      character*40 LMNAME,PDFFIL
      character*80 MDESC
c
      integer*4 nc
c
c...Update new/moved variables
c
      call itoc (MACH,LMNAME,nc,0)
      call pdfdsc (MCHOPT(3),IPRDES(1,1),MDESC,nc)
      call itoc (MCHOPT(3),PDFFIL,nc,0)
c
c...Update revision date
c
      LUPDAT = '05.13.09'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u06179
c
c   FUNCTION:  This routine updates the MDF file to the version
c              06.17.09.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u06179
c
      include 'post.inc'
c
      equivalence (IPHFIL,KPOSMP(0369))
      equivalence (LRTRCT,KPOSMP(1278)), (ILINRP,KPOSMP(1348))
c
      integer*4 ILINRP,LRTRCT,IPHFIL
c
      equivalence (LNRTOL,POSMAP(2251)), (RETFED,POSMAP(2254))
      equivalence (RETPL ,POSMAP(2258)), (TAXTOL,POSMAP(2262))
c
      equivalence (ORETFED,POSMAP(3239))
c
      real*8 LNRTOL(2),RETFED(4),RETPL(4),ORETFED(4),TAXTOL
c
      equivalence (PHFILE,CPOSMP(2535)), (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
      character*40 PHFILE
c
      integer*4 nc
c
c...Update new/moved variables
c
      if (ILINRP .eq. 1) then
          LNRTOL(2) = LNRTOL(1)
      else
          LNRTOL(2) = 0.
      endif
c
      if (LRTRCT .eq. 2) LRTRCT = 0
c
      RETFED(1) = ORETFED(1)
      RETFED(2) = ORETFED(2)
      RETFED(3) = ORETFED(3)
      RETFED(4) = ORETFED(4)
c
      RETPL(1) = 0.
      RETPL(2) = 0.
      RETPL(3) = 1.
      RETPL(4) = 0.
c
      TAXTOL = 0.
c
      if (IPHFIL .eq. -1) then
          PHFILE = ' '
      else
          call itoc (IPHFIL,PHFILE,nc,0)
      endif
c
c...Update revision date
c
      LUPDAT = '06.17.09'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u02020
c
c   FUNCTION:  This routine updates the MDF file to the version
c              02.02.10.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u02020
c
      include 'post.inc'
c
      equivalence (TOOLFL,KPOSMP(1804))
c
      integer*4 TOOLFL(20)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update new/moved variables
c
      TOOLFL(18) = 2
      TOOLFL(19) = 1
c
c...Update revision date
c
      LUPDAT = '02.02.10'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u10180
c
c   FUNCTION:  This routine updates the MDF file to the version
c              10.18.10.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u10180
c
      include 'post.inc'
c
      equivalence (MSGALN,KPOSMP(0349)), (IACLFL,KPOSMP(1732))
c
      integer*4 IACLFL,MSGALN
c
      equivalence (ACLDCL,POSMAP(2099))
c
      real*8 ACLDCL(10)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i
c
c...Update new/moved variables
c
      IACLFL = 2
      MSGALN = 0
c
      do 100 i=1,10,1
          ACLDCL(i) = 0.
  100 continue
c
c...Update revision date
c
      LUPDAT = '10.18.10'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u02161
c
c   FUNCTION:  This routine updates the MDF file to the version
c              02.16.11.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u02161
c
      include 'post.inc'
c
      equivalence (PPRIPV,KPOSMP(0175))
c
      integer*4 PPRIPV
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update new/moved variables
c
      PPRIPV = 2
c
c...Update revision date
c
      LUPDAT = '02.16.11'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u05121
c
c   FUNCTION:  This routine updates the MDF file to the version
c              05.12.11.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u05121
c
      include 'post.inc'
c
      equivalence (LRTRFL,KPOSMP(1348)), (IRPSMD,KPOSMP(3242))
c
      integer*4 LRTRFL,IRPSMD
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update new/moved variables
c
      IRPSMD = 2
      LRTRFL = 2
c
c...Update revision date
c
      LUPDAT = '05.12.11'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u08072
c
c   FUNCTION:  This routine updates the MDF file to the version
c              08.07.12.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u08072
c
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308)), (MACSPC,KPOSMP(1110))
c
      integer*4 MCHOPT(20),MACSPC
c
      equivalence (OROTZE,POSMAP(1453)), (ROTZER,POSMAP(4555))
c
      real*8 OROTZE,ROTZER(2)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update new/moved variables
c
      MACSPC = 1
      MCHOPT(7) = 0
c
      ROTZER(1) = OROTZE
      ROTZER(2) = 0.
c
c...Update revision date
c
      LUPDAT = '08.07.12'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u09052
c
c   FUNCTION:  This routine updates the MDF file to the version
c              09.05.12.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u09052
c
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (XFMFL ,KPOSMP(0921)), (ICY2FL,KPOSMP(4004))
c
      integer*4 ICY2FL(20),XFMFL(10),MCHOPT(20)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update new/moved variables
c
      ICY2FL(4) = 5
      ICY2FL(5) = 1
c
      MCHOPT(8) = 0
c
      XFMFL(7) = 2
      XFMFL(8) = 1
      XFMFL(9) = 2
c
c...Update revision date
c
      LUPDAT = '09.05.12'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u04303
c
c   FUNCTION:  This routine updates the MDF file to the version
c              04.30.13.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u04303
c
      include 'post.inc'
c
      equivalence (OIRTYP,KPOSMP(1244)), (IRTYPE,KPOSMP(1486))
      equivalence (OIRDED,KPOSMP(1248)), (IRDEAD,KPOSMP(1465))
      equivalence (OIRWRK,KPOSMP(1252)), (IRTWRK,KPOSMP(1506))
      equivalence (ISCWRK,KPOSMP(1453))
c
      integer*4 OIRDED(4),IRDEAD(20),ISCWRK(2,4),OIRTYP(4),IRTYPE(20),
     1          OIRWRK(4),IRTWRK(20)
c
      equivalence (OTBORG,POSMAP(1375)), (TABORG,POSMAP(5374))
      equivalence (OROTST,POSMAP(1413)), (ROTSTO,POSMAP(5213))
c
      real*8 OROTST(4,2),ROTSTO(20,2),OTBORG(3,4),TABORG(3,20)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
      integer*4 i
c
c...Update new/moved variables
c
      do 100 i=1,4,1
          IRDEAD(i) = OIRDED(i)
          IRTWRK(i) = OIRWRK(i)
          IRTYPE(i) = OIRTYP(i)
          ISCWRK(1,i) = 0
          ROTSTO(i,1) = OROTST(i,1)
          ROTSTO(i,2) = OROTST(i,2)
          TABORG(1,i) = OTBORG(1,i)
          TABORG(2,i) = OTBORG(2,i)
          TABORG(3,i) = OTBORG(3,i)
  100 continue
      do 120 i=5,20,1
          IRDEAD(i) = -1
          ROTSTO(i,1) = 0.
          ROTSTO(i,2) = 0.
          TABORG(1,i) = 0.
          TABORG(2,i) = 0.
          TABORG(3,i) = 0.
  120 continue
c
c...Update revision date
c
      LUPDAT = '04.30.13'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u12053
c
c   FUNCTION:  This routine updates the MDF file to the version
c              12.05.13.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u12053
c
      include 'post.inc'
c
      equivalence (NCPOSM,KPOSMP(0173))
      equivalence (OXFMFL,KPOSMP(0921)), (XFMFL ,KPOSMP(0969))
      equivalence (IXFMFL,KPOSMP(0989))
c
      integer*4 OXFMFL(10),XFMFL(20),IXFMFL(5),NCPOSM
c
      equivalence (LUPDAT,CPOSMP(3001))
      equivalence (OREGST,CPOSMP(4147)), (REGST ,CPOSMP(7011))
      equivalence (OREGEN,CPOSMP(5067)), (REGEN ,CPOSMP(9219))
c
      character*8 LUPDAT
      character*10 OREGST(92),OREGEN(92)
      character*24 REGST(92),REGEN(92)
c
      integer*4 i
c
c...Update new/moved variables
c
      do 100 i=1,10,1
          XFMFL(i) = OXFMFL(i)
          XFMFL(i+10) = 0
  100 continue
      XFMFL(11) = 2
      XFMFL(12) = 2
      XFMFL(13) = 2
      XFMFL(14) = 1
      IXFMFL(1) = 2
      IXFMFL(2) = 2
c
      do 200 i=1,92,1
          REGST(i) = OREGST(i)
          REGEN(i) = OREGEN(i)
  200 continue
c
      NCPOSM = 2
c
c...Update revision date
c
      LUPDAT = '12.05.13'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u05125
c
c   FUNCTION:  This routine updates the MDF file to the version
c              05.12.15.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u05125
c
      include 'post.inc'
c
      equivalence (NCPOSM,KPOSMP(0173))
      equivalence (XFMFL ,KPOSMP(0969))
      equivalence (IXFMFL,KPOSMP(0989))
c
      integer*4 XFMFL(20),IXFMFL(5),NCPOSM
c
      equivalence (LUPDAT,CPOSMP(3001))
      equivalence (OREGST,CPOSMP(4147)), (REGST ,CPOSMP(7011))
      equivalence (OREGEN,CPOSMP(5067)), (REGEN ,CPOSMP(9219))
c
      character*8 LUPDAT
      character*10 OREGST(92),OREGEN(92)
      character*24 REGST(92),REGEN(92)
c
      integer*4 i
c
c...Update new/moved variables
c
      XFMFL(16) = 1
      XFMFL(17) = 2
      if (XFMFL(3) .eq. 3) XFMFL(16) = 2
      if (XFMFL(3) .eq. 6) XFMFL(17) = 1
      if (XFMFL(3) .eq. 3) then
          XFMFL(3) = 2
      else if (XFMFL(3) .eq. 4) then
          XFMFL(3) = 3
      else if (XFMFL(3) .eq. 5 .or. XFMFL(3) .eq. 6) then
          XFMFL(3) = 4
      endif
c
c...Update revision date
c
      LUPDAT = '05.12.15'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u10055
c
c   FUNCTION:  This routine updates the MDF file to the version
c              10.05.15.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u10055
c
      include 'post.inc'
c
      equivalence (CUTCFL,KPOSMP(3251))
c
      integer*4 CUTCFL(20)
c
      equivalence (OFTMCNV,POSMAP(0026)), (FTMCNV,POSMAP(0056))
      equivalence (OTLATOL,POSMAP(0027)), (TLATOL,POSMAP(0057))
      equivalence (BRKVR ,POSMAP(1205))
c
      real*8 OFTMCNV,FTMCNV,OTLATOL,TLATOL,BRKVR(10)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update new/moved variables
c
      if (CUTCFL(1) .eq. 1) CUTCFL(14) = 2
		FTMCNV = OFTMCNV
      TLATOL = OTLATOL
      do 100 i=6,10,1
          BRKVR(i) = 0.
  100 continue
c
c...Update revision date
c
      LUPDAT = '10.05.15'
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  u03106
c
c   FUNCTION:  This routine updates the MDF file to the version
c              03.10.16.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine u03106
c
      include 'post.inc'
c
      equivalence (BRKOP ,KPOSMP(1137))
c
      integer*4 BRKOP(10)
c
      equivalence (LUPDAT,CPOSMP(3001))
c
      character*8 LUPDAT
c
c...Update new/moved variables
c
      BRKOP(5) = 2
c
c...Update revision date
c
      LUPDAT = '03.10.16'
c
      return
      end
