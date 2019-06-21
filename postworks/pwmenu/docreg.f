c
c***********************************************************************
c
c   FILE NAME:  docreg.for
c   CONTAINS:
c               docreg  docrgn  docusr  doctyp  docgcd  docrgo
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        docreg.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:55
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  docreg (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine outputs the automatic documentation
c              Register Format pages.
c
c   INPUT:  kfl     I*4  D1  -  1 = Output Register Description section.
c                               2 = Output G-code Description section.
c                               3 = Output M-code Description section.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docreg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (NRGORD,KPOSMP(0837))
      equivalence (NEOB  ,KPOSMP(0841)), (NCBMSG,KPOSMP(0843))
      equivalence (NCEMSG,KPOSMP(0844)), (XFMFL ,KPOSMP(0921))
      equivalence (NOPSKP,KPOSMP(1077))
      equivalence (NTAXCD,KPOSMP(1090)), (NRWSTP,KPOSMP(1104))
      equivalence (NEOT  ,KPOSMP(1106)), (NBOT  ,KPOSMP(1136))
      equivalence (MACHTP,KPOSMP(1201)), (NUMLIN,KPOSMP(1202))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTCLM,KPOSMP(1306))
      equivalence (RSIZCD,KPOSMP(1328)), (ICIRFL,KPOSMP(1367))
      equivalence (ICRFMT,KPOSMP(1368)), (SLWFL ,KPOSMP(1701))
      equivalence (IC3AXF,KPOSMP(1747))
      equivalence (TOOLFL,KPOSMP(1804)), (ICRHEL,KPOSMP(1373))
      equivalence (LTHPCL,KPOSMP(1899)), (ICYLFL,KPOSMP(1901))
      equivalence (FMTDES,KPOSMP(2133)), (NSPRG ,KPOSMP(3101))
      equivalence (SPNTYP,KPOSMP(3102)), (IJKROT,KPOSMP(1739))
      equivalence (SPNSFM,KPOSMP(3103)), (IFDSUP,KPOSMP(3111))
      equivalence (IFDEXD,KPOSMP(3178)), (NRAPCD,KPOSMP(3190))
      equivalence (IFDRAD,KPOSMP(3210))
      equivalence (CUTCFL,KPOSMP(3251)), (HOMCOD,KPOSMP(3361))
      equivalence (PSTNCD,KPOSMP(3376)), (REGORD,KPOSMP(3707))
      equivalence (SMONRN,KPOSMP(4075))
      equivalence (SMOORN,KPOSMP(4079)), (BSPLFL,KPOSMP(4088))
      equivalence (MTPDYN,KPOSMP(4126))
      equivalence (IRFSUP,KPOSMP(4136)), (IRDSUP,KPOSMP(4137))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 NEOB,NCBMSG,NCEMSG,NOPSKP,NEOT,NBOT,NRWSTP,NRGORD,
     1          REGORD(MAXFMT),ICYCFL(30),MACHTP,ICYLFL(20),ICIRFL,
     2          ICRFMT,SLWFL(5),SPNTYP,SPNSFM,IFDSUP(4),NSPRG,
     3          IFDEXD(4),NRAPCD,CUTCFL(20),HOMCOD(5),PSTNCD(5),
     4          TOOLFL(20),NUMLIN(3),IRTNUM,NTAXCD(3),IRTCLM,IJKROT,
     5          RSIZCD(5),SMONRN,SMOORN,IC3AXF(10),XFMFL(5),
     6          MTPDYN,LTHPCL(2),IRFSUP,IRDSUP,BSPLFL,ICRHEL(5),
     7          IFDRAD
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (GRANGE,KPOSMP(0801)), (MRANGE,KPOSMP(0955))
      equivalence (REGSTO,POSMAP(1032))
c
      real*8 DUMMY,REGSTO(MAXFMT),GRANGE(14,11),MRANGE(7,11)
c
      equivalence (LEOB  ,CPOSMP(0971)), (MSGST ,CPOSMP(0976))
      equivalence (LEDCHR,CPOSMP(0986)), (MSGEN ,CPOSMP(1500))
      equivalence (LOPSKP,CPOSMP(1648)), (LRWSTP,CPOSMP(2201))
      equivalence (LEOT  ,CPOSMP(2211)), (LBOT  ,CPOSMP(2515))
c
      character*1 LEDCHR
      character*5 LEOB,LOPSKP,LRWSTP
      character*10 MSGST,MSGEN
      character*20 LEOT,LBOT
c
      integer*4 kerr,kfl
c
      character*(*) cmsg
c
      integer*4 ilev(10),nlev,ncs,nct,ireg(MAXFMT),nreg,j,imcyc(35),
     1          nmcyc,nct1,nct2,inc,jindex,i,ifl,iblad(23),nblad,
     2          igen(67),ngen,imbld(28),nmbld,ispin(30),ispint(30),
     3          nlcyc,ilcyc(37),icirc(20),ncirc,islw(13),nslw,nspin,
     4          ilodt(12),ilod(12),nlod,idum,ifedt(25),ifed(25),nfed,
     5          icut(20),icutt(20),ncut,imisc(28),imisct(28),nmisc
      integer*4 rmcyc(35),rblad(23),rgen(67),rmbld(28),rspin(30),
     1          rlcyc(37),rcirc(20),rslw(13),rlod(12),rfed(25),rcut(20),
     2          rmisc(28),imbldt(28),k,nc,ncl,ibladt(23),icirc1(20),
     3          idumn(32),nn,igen1(67),nbspl,ibspl(12),rbspl(12),
     4          nsp1,ist,imcyc1(35),nxfrm,ixfrm(14),rxfrm(14),
     5          ixfrmt(14),nstrg,istrg(19),rstrg(19)
c
      character*20 lnum
      character*80 sbuf,tbuf,t1buf,t2buf
c
      data idum /88/, idumn /32*88/
c
      data ngen /67/
      data igen / 381,  382,  383,  384,  385,  386,  387,  388,
     2            389,  390,  391,  392,  393,  394,  395,  396,
     3            397,  398,  399,  400,  401,  402,  403,  404,
     4            847,  848, 1075, 1076, 1078, 1079, 1080, 1105,
     5           1132, 1240, 1241, 1242, 1860, 1861, 1862, 1863,
     6           1864, 1865, 1867, 3128, 3129, 3130, 3131, 3286,
     7           3287, 3288, 3289, 3290, 3291, 3293, 3351, 3352,
     8           3353, 3391, 4104, 4105,  827,  828,  829,   93,
     9             94,   95,   96/
      data rgen /   0,    0,    0,    0,    0,    0,    0,    0,
     2              0,    0,    0,    0,    0,    0,    0,    0,
     3              0,    0,    0,    0,    0,    0,    0,    0,
     4              0,    0, 1168, 1169, 1170, 1171, 1172, 1203,
     5              0, 1284, 1285, 1286, 3983, 3984, 3985, 3986,
     6           3987,    0,    0, 3294, 3295, 3296, 3297, 2425,
     7           2426, 2427, 2428, 2429,    0,    0, 0165,    0,
     8              0,    0, 4600, 4601,    0,    0,    0, 4364,
     9           4365, 4366, 4367/
c
      data nmbld /28/
      data imbldt /1081, 1082, 1083, 1084, 1085, 1086, 1087, 1088,
     1             1089, 1319, 1320, 1321, 1322, 1323, 1324, 1325,
     2             1326, 1328, 1329, 1330, 1331, 1332, 4222, 4223,
     3             4224, 4225, 1866, 3292/
      data rmbld  /1173, 1174, 1175, 1176, 1177, 1178, 1179, 1180,
     1             1181, 1466, 1467, 1468, 1469, 1470, 1471, 1472,
     2             1473, 1600,    0,    0,    0,    0, 2208, 2209,
     3             2210, 2211,    0,    0/
c
      data nmcyc /35/
      data imcyc / 211,  212,  213,  214,  215,  216,  217,  218,
     1             219,  220,  221,  222,  223,  224,  231,  232,
     2             233,  234,  235,  236,  237,  238,  239,  240,
     3             241,  242,  243,  244,  296,  297,  298,  225,
     4             229,  230,  801/
      data rmcyc /2901, 2902, 2903, 2904, 2905, 2906, 2907, 2908,
     1            2909, 2910, 2911, 2912, 2913, 2914,    0,    0,
     2               0,    0,    0,    0,    0,    0,    0,    0,
     3               0,    0,    0,    0, 2998, 2999, 3000, 2915,
     4            2919, 2920, 1184/
c
      data nlcyc /37/
      data ilcyc /1921, 1922, 1923, 1924, 1925, 1926, 1927, 1928,
     1            1929, 1930, 1931, 1932, 1933, 1934, 1935, 1936,
     2            1937, 1938, 1944, 1945, 1946, 1947, 1948, 1949,
     3            1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957,
     4            1958, 1959, 1960, 1961, 1962/
      data rlcyc /2968, 2969, 2970, 2971, 2972, 2973, 2974, 2975,
     1            2976, 2977, 2978, 2979, 2980, 2981, 2982, 283,
     2            2984, 2985, 2991, 2992,    0,    0,    0,   0,
     3               0,    0,    0,    0,    0,    0,    0,   0,
     4               0,    0,    0,    0,    0/
c
      data nblad /23/
      data iblad / 372,  373,  374,  375,  376,  377,  378,  379,
     1             380,  818,  819,  820,  821,  822,  823, 4076,
     2            4077, 4078, 4080, 4081, 4082, 4073, 4074/
      data rblad /2494, 2495, 2496, 2497, 2498, 2499, 2500,    0,
     1               0, 3594, 3595, 3596, 3597, 3598, 3599, 4450,
     2            4451, 4452, 4453, 4454, 4455, 4448, 4449/
c
      data ncirc /20/
      data icirc /1378, 1379, 1380, 1381, 1382, 1383, 1262, 1263,
     1            1264, 4112, 4113, 4118, 4119, 1376, 1377, 1749,
     2            1750, 1751, 1752, 1753/
      data rcirc /2206, 2207,    0,    0,    0,    0, 2227, 2228,
     1               0, 4595, 4596, 4597, 4598, 2242, 2243, 2244,
     2            2245,    0,    0,    0/
c
      data nbspl /12/
      data ibspl /4089, 4090, 4231, 4232, 4233, 4234, 4235, 4237,
     1            4238, 4239, 4240, 4241/
      data rbspl /4901,    0, 4936, 4937, 4938, 4939, 4940, 4941,
     1            4942, 4943, 4944, 4945/
c
      data islw /1718, 1706, 1710, 1711, 1707, 1712, 1713, 1708,
     1           1714, 1715, 1709, 1716, 1717/
      data rslw /2468, 2456, 2460, 2461, 2457, 2462, 2463, 2458,
     1           2464, 2465, 2459, 2466, 2467/
c
      data nlod /12/
      data ilodt /1839, 1840, 1841, 1842, 1843, 1844, 1845, 1846,
     1            1847, 1848, 1849, 1850/
      data rlod  /3963, 3964, 3965, 3966, 3967, 3968, 3969, 3970,
     1            3971, 3972, 3973, 3974/
c
      data nspin /30/
      data ispint /3105, 3106, 3107, 3108, 3109, 3144, 3145, 3115,
     1             3116, 3117, 3118, 3119, 3120, 3121, 3122, 3132,
     2             3133, 3134, 3135, 3136, 3137, 4195, 4196, 4197,
     3             4201, 4202, 4106, 4110, 4108, 4109/
      data rspin /3007, 3008, 3009, 3010, 3011,    0,    0, 3016,
     1            3017, 3018, 3019, 3020, 3021, 3022, 3023, 3298,
     2            3299, 3300, 3301, 3302, 3303,    0,    0,    0,
     3               0,    0, 4880, 4881, 4602, 4603/
c
      data nfed /25/
      data ifedt /3217, 3218, 3162, 3163, 3164, 3165, 3166, 3169,
     1            3170, 3171, 3172, 3173, 3174, 3177, 3191, 3192,
     2            3193, 3194, 3195, 4138, 4139, 3213, 3214, 3215,
     3            3216/
      data rfed /3001, 3002,    0,    0,    0,    0,    0,    0,
     1           3320, 3321, 3322, 3323, 3324, 3327, 3577, 3578,
     2           3579, 3580, 3581, 4882, 4883,    0,    0,    0,
     3              0/
c
      data ncut /20/
      data icutt /3271, 3272, 3273, 3274, 3275, 3276, 3277, 3278,
     1            3279, 3280, 3281, 3282, 3283, 3284, 3285, 3294,
     2            3295, 3296, 3297, 3298/
      data rcut /2410, 2411, 2412, 2413, 2414, 2415, 2416, 2417,
     1           2418, 2419, 2420, 2421, 2422, 2423, 2424, 2433,
     2              0,    0,    0,    0/
c
      data nmisc /28/
      data imisct /3361, 3362, 3363, 3364, 3366, 3367, 3368, 3369,
     1             3370, 3371, 3372, 3373, 3374, 3375, 3376, 3377,
     2             3378, 3381, 3382, 3383, 3384, 3385, 3386, 3387,
     3             3388, 3389, 3390, 4107/
      data rmisc / 167,  168,  169,  170,    0,    0,    0,    0,
     1               0,    0,    0,    0,    0,    0,  172,  173,
     2             174,    0,    0,    0,    0,    0,    0,    0,
     3               0,    0,    0,    0/
c
      data nxfrm /14/
      data ixfrm / 926,  927,  928,  929,  931,  932,  933,  934,
     1             935,  936,  937,  938,  939,  940/
      data rxfrm /1189, 1190, 1191, 1192,    0,    0,    0,    0,
     1               0,    0,    0,    0,    0,    0/
c
      data nstrg /19/
      data istrg / 951,  952,  953,  954,  955,  956,  957,  958,  959,
     1             963,  964,  965, 1018, 1019, 1020, 1021, 1022, 1023,
     2            1024/
      data rstrg /   0,    0,    0,    0,    0,    0,    0,    0,    0,
     1               0,    0,    0, 5552, 5553, 5554, 5555, 5556, 5557,
     2            5558/
c
c...Initialize routine
c
      DLIN   = 10000
      NDHED  = 9
      IGDES  = 0
c
c...Get Header lines
c
      nlev   = 1
      DHED(7) = ' '
      NCHED(7) = 0
      ilev(1) = 2
      if (kfl .eq. 2) ilev(1) = 3
      if (kfl .eq. 3) ilev(1) = 4
      call docprm (DHED(1),NCHED(1),ilev,nlev)
      nlev   = 2
      ilev(2) = 1
      call docprm (DHED(8),NCHED(8),ilev,nlev)
      ilev(2) = 2
      call docprm (DHED(9),NCHED(9),ilev,nlev)
      call dcenhd (DHED(1),NCHED(1),DHED(1),NCHED(1))
      NDHED  = 9
      ilev(1) = 2
c
c...Start of tape characters
c
      if (kfl .eq. 1) then
          ilev(2) = 3
          nlev   = 3
          if (NBOT .gt. 0) then
              ilev(nlev) = 1
              call docprm (sbuf,ncs,ilev,nlev)
              call docrgo (LBOT,NBOT,sbuf,ncs,' ',0,1,cmsg,kerr)
          endif
c
c...Rewind Stop Code
c
          if (NRWSTP .gt. 0) then
              ilev(nlev) = 2
              call docprm (sbuf,ncs,ilev,nlev)
              call docrgo (LRWSTP,NRWSTP,sbuf,ncs,' ',0,1,cmsg,kerr)
          endif
c
c...Leader character
c
          if (LEDCHR .ne. ' ') then
              ilev(nlev) = 3
              call docprm (sbuf,ncs,ilev,nlev)
              call docrgo (LEDCHR,1,sbuf,ncs,' ',0,1,cmsg,kerr)
          endif
c
c...Optional skip code
c
          if (NOPSKP .gt. 0) then
              ilev(nlev) = 4
              call docprm (sbuf,ncs,ilev,nlev)
              call docrgo (LOPSKP,NOPSKP,sbuf,ncs,' ',0,1,cmsg,kerr)
          endif
c
c...Message characters
c
          if (NCBMSG .gt. 0 .or. NCEMSG .gt. 0) then
              ilev(nlev) = 5
              if (NCBMSG .gt. 0) tbuf   = MSGST(1:NCBMSG)
              if (NCEMSG .gt. 0) tbuf(NCBMSG+1:) = MSGEN(1:NCEMSG)
              nct   = NCBMSG + NCEMSG
              call docprm (sbuf,ncs,ilev,nlev)
              call docrgo (tbuf,nct,sbuf,ncs,' ',0,1,cmsg,kerr)
          endif
c
c...End of Block characters
c
          if (NEOB .gt. 0) then
              ilev(nlev) = 6
              call docprm (sbuf,ncs,ilev,nlev)
              call docrgo (LEOB,NEOB,sbuf,ncs,' ',0,1,cmsg,kerr)
          endif
c
c...End of tape characters
c
          if (NEOT .gt. 0) then
              ilev(nlev) = 7
              call docprm (sbuf,ncs,ilev,nlev)
              call docrgo (LEOT,NEOT,sbuf,ncs,' ',0,1,cmsg,kerr)
          endif
          if (kerr .ne. 0) go to 8000
      endif
c
c...Register assignments
c......Determine output order for registers
c
      ilev(2) = 4
      nlev   = 3
      call copynk (imisct,imisc,nmisc)
      call copynk (ispint,ispin,nspin)
      call copynk (igen,igen1,ngen)
      call copynk (icirc,icirc1,ncirc)
      call copynk (ifedt,ifed,nfed)
      if (MACHTP .eq. 4) then
          nsp1 = 30
          if (LTHPCL(1) .ne. 1) then
             call copynk (idumn,icirc1(10),2)
             call copynk (idumn,ispin(25),2)
             ispin(29) = idum
             igen1(59) = idum
          end if
          if (LTHPCL(2) .ne. 1) then
             call copynk (idumn,icirc1(12),2)
             call copynk (idumn,ispin(22),3)
             ispin(30) = idum
             igen1(60) = idum
          end if
          if (LTHPCL(2) .ne. 1 .and. LTHPCL(2) .ne. 1) then
             imisc(28) = idum
          end if
      else
          call copynk (idumn,ifed(20),2)
          call copynk (idumn,ispin(22),9)
          nsp1   = 21
          call copynk (idumn,icirc1(10),4)
          if (ICRHEL(1) .ne. 1) call copynk (idumn,icirc1(14),2)
          if (IC3AXF(1) .ne. 1) call copynk (idumn,icirc1(15),5)
          call copynk (idumn,igen1(59),2)
          imisc(28) = idum
          if (IJKROT .ne. 1) call copynk (idumn,igen1(61),3)
      end if
      if (kfl .eq. 1) then
          nreg   = 0
          do 1000 i=1,NRGORD,1
              if (FMTDES(10,REGORD(i)) .ne. 0) then
                  nreg   = nreg   + 1
                  ireg(nreg) = REGORD(i)
              endif
 1000     continue
c
          do 1100 i=1,MAXFMT,1
              inc    = jindex(ireg,i,nreg)
              if (inc .eq. 0 .and. FMTDES(10,i) .ne. 0) then
                  nreg   = nreg   + 1
                  ireg(nreg) = i
              endif
 1100     continue
      else
          nreg   = 1
      endif
c
c......Find all descriptions for registers
c
      do 1500 i=1,nreg,1
          ifl    = 0
c
c.........G-code
c
          if (kfl .eq. 1 .and. ireg(i) .ge. 18 .and. ireg(i) .le. 28)
     1            then
              if (GRANGE(1,ireg(i)-17) .ne. DUMMY) then
                  ilev(nlev-1) = 3
                  ilev(nlev) = 8
                  call docprm (t2buf,nct2,ilev,nlev)
                  call docrgf (ireg(i),0.d0,t1buf,nct1,2)
                  if (REGSTO(ireg(i)) .ne. 0.) then
                      call rtoc (REGSTO(ireg(i)),lnum,ncl)
                      sbuf   = REGID(ireg(i)) // ' / ' // lnum(1:ncl)
                      nc     = ncl    + 5
                  else
                      sbuf   = REGID(ireg(i))
                      nc     = 2
                  endif
                  call docrgo (t1buf,nct1,t2buf,nct2,sbuf,nc,1,cmsg,
     1                         kerr)
              endif
              go to 1500
          endif
c
c.........M-code
c
          if (kfl .eq. 1 .and. ireg(i) .ge. 37 .and. ireg(i) .le. 47)
     1            then
              if (MRANGE(1,ireg(i)-36) .ne. DUMMY) then
                  ilev(nlev-1) = 3
                  ilev(nlev) = 9
                  call docprm (t2buf,nct2,ilev,nlev)
                  call docrgf (ireg(i),0.d0,t1buf,nct1,2)
                  if (REGSTO(ireg(i)) .ne. 0.) then
                      call rtoc (REGSTO(ireg(i)),lnum,ncl)
                      sbuf   = REGID(ireg(i)) // ' / ' // lnum(1:ncl)
                      nc     = ncl    + 5
                  else
                      sbuf   = REGID(ireg(i))
                      nc     = 2
                  endif
                  call docrgo (t1buf,nct1,t2buf,nct2,sbuf,nc,1,cmsg,
     1                         kerr)
              endif
              go to 1500
          endif
c
c.........Common registers
c
          ilev(nlev-1) = 4
          call docrgn (ireg(i),igen1,rgen,ngen,ilev,nlev,ifl,kfl,cmsg,
     1                 kerr)
c
c.........Circular registers
c
          if (ICIRFL .eq. 1) then
              if (MACHTP .eq. 4) then
                 nn  = ncirc
                 if (LTHPCL(2) .ne. 1) nn = 11
              else
                 nn  = ncirc
              end if
              if (ICRFMT .ne. 7) call copynk (idumn,icirc1(7),3)
              ilev(nlev-1) = 11
              call docrgn (ireg(i),icirc1,rcirc,nn,ilev,nlev,ifl,
     1                     kfl,cmsg,kerr)
          endif
c
c........Bspline registers
c
          if (BSPLFL .eq. 1) then
              ilev(nlev-1) = 7
              nn     = 2
              call docrgn (ireg(i),ibspl,rbspl,nbspl,ilev,nlev,ifl,
     1                     kfl,cmsg,kerr)
          endif
c
c.........Mill cycles
c
          if (MACHTP .ne. 2 .and. ICYCFL(1) .eq. 1) then
              ilev(nlev-1) = 5
              call copynk (imcyc,imcyc1,nmcyc)
              if (ICYCFL(30) .eq. 1) then
                  call copynk (idumn,imcyc1(15),14)
              else
                  imcyc1(35) = idumn(1)
              endif
              call docrgn (ireg(i),imcyc1,rmcyc,nmcyc,ilev,nlev,ifl,
     1                     kfl,cmsg,kerr)
          endif
c
c.........Mill/Blade registers
c
          do 1160 j=1,nmbld,1
              imbld(j) = imbldt(j)
 1160     continue
          if (MACHTP .ne. 2) then
              do 1162 j=1,3,1
                  inc    = (j-1) * 3
                  do 1161 k=NTAXCD(j)+1,3,1
                      imbld(inc+k) = idum
 1161             continue
 1162         continue
              if (IRTCLM .eq. 1) then
                  do 1163 j=10,17,1
                      imbld(j) = idum
 1163             continue
              endif
              if (RSIZCD(1) .eq. 0) then
                  do 1164 j=19,22,1
                      imbld(j) = idum
 1164             continue
              endif
              do 1165 j=IRTNUM+1,4,1
                  imbld(9+j*2-1) = idum
                  imbld(9+j*2) = idum
                  imbld(18+j) = idum
 1165         continue
              ilev(nlev-1) = 6
              call docrgn (ireg(i),imbld,rmbld,nmbld,ilev,nlev,ifl,
     1                     kfl,cmsg,kerr)
          endif
c
c.........Loadtl registers
c
          do 1200 j=1,nlod,1
              ilod(j) = ilodt(j)
 1200     continue
          if (TOOLFL(1) .ne. 1) then
              ilod(1) = idum
              ilod(7) = idum
              ilod(8) = idum
              ilod(9) = idum
              ilod(10) = idum
          endif
          if (MACHTP .eq. 2) then
              ilod(1) = idum
              ilod(2) = idum
              ilod(3) = idum
              ilod(7) = idum
              ilod(8) = idum
              ilod(9) = idum
              ilod(10) = idum
          else if (MACHTP .eq. 3) then
              ilod(2) = idum
              ilod(8) = idum
              ilod(9) = idum
              ilod(10) = idum
              ilod(11) = idum
              ilod(12) = idum
          else
              if (TOOLFL(2) .ne. 1) then
                  ilod(3) = idum
                  ilod(9) = idum
                  ilod(10) = idum
              endif
              if (TOOLFL(3) .ne. 1) then
                  ilod(8) = idum
                  ilod(10) = idum
              endif
              ilod(11) = idum
              ilod(12) = idum
         endif
         ilev(nlev-1) = 13
         call docrgn (ireg(i),ilod,rlod,nlod,ilev,nlev,ifl,kfl,cmsg,
     1                kerr)
c
c.........Spindle registers
c
c         do 1300 j=1,nspin,1
c             ispin(j) = ispint(j)
c1300     continue
          if (SPNTYP .eq. 1) then
              ispin(6) = idum
              ispin(7) = idum
              ispin(8) = idum
              ispin(9) = idum
              ispin(10) = idum
              ispin(14) = idum
              ispin(15) = idum
          endif
          if (SPNSFM .ne. 1) then
              ispin(7) = idum
              ispin(11) = idum
              ispin(12) = idum
              ispin(13) = idum
          endif
          if (NSPRG .eq. 1) then
              ispin(8) = idum
              ispin(9) = idum
              ispin(10) = idum
          else if (NSPRG .eq. 2) then
              ispin(9) = idum
          endif
          ilev(nlev-1) = 14
          call docrgn (ireg(i),ispin,rspin,nsp1,ilev,nlev,ifl,kfl,
     1                 cmsg,kerr)
c
c.........Feed rate registers
c
          if (IRFSUP .ne. 1) ifed(20) = idum
          if (IRDSUP .ne. 1) ifed(21) = idum
          if (IFDSUP(1) .ne. 1) then
              ifed(3) = idum
              ifed(7) = idum
              ifed(9) = idum
              ifed(13) = idum
          else if (IFDEXD(1) .ne. 1) then
              ifed(7) = idum
              ifed(13) = idum
          endif
          if (IFDSUP(2) .ne. 1) then
              ifed(4) = idum
              ifed(10) = idum
          endif
          if (IFDSUP(3) .ne. 1) then
              ifed(5) = idum
              ifed(11) = idum
          endif
          if (IFDSUP(4) .ne. 1) then
              ifed(6) = idum
              ifed(8) = idum
              ifed(12) = idum
              ifed(14) = idum
          else if (IFDEXD(4) .ne. 1) then
              ifed(8) = idum
              ifed(14) = idum
          endif
          do 1360 j=NRAPCD+1,5,1
              ifed(j+14) = idum
 1360     continue
          ist = 1
          if (IFDRAD .eq. 1) ist = IRTNUM + 1
          do 1365 j=ist,4,1
              ifed(j+21) = idum
 1365     continue
          ilev(nlev-1) = 15
          call docrgn (ireg(i),ifed,rfed,nfed,ilev,nlev,ifl,kfl,
     1                 cmsg,kerr)
c
c.........Cutcom registers
c
          do 1380 j=1,ncut,1
              icut(j) = icutt(j)
 1380     continue
          if (CUTCFL(1) .ne. 3) icut(5) = idum
          if ((CUTCFL(1) .lt. 1 .or. CUTCFL(1) .gt. 3) .and.
     1        CUTCFL(17) .ne. 1) then
              icut(6) = idum
              icut(7) = idum
              icut(8) = idum
          endif
          if (CUTCFL(1) .ne. 4) then
              icut(9) = idum
              icut(10) = idum
              icut(11) = idum
          endif
          if (CUTCFL(3) .ne. 1) then
              icut(12) = idum
              icut(13) = idum
              icut(14) = idum
              icut(15) = idum
          endif
          if (CUTCFL(17) .ne. 1) then
              icut(16) = idum
              icut(17) = idum
              icut(18) = idum
              icut(19) = idum
              icut(20) = idum
          endif
          if (MACHTP .eq. 2) icut(7) = idum
          ilev(nlev-1) = 16
          call docrgn (ireg(i),icut,rcut,ncut,ilev,nlev,ifl,kfl,cmsg,
     1                 kerr)
c
c.........Blade registers
c
          if (MACHTP .eq. 3) then
              ilev(nlev-1) = 9
              do 1389 j=1,nblad,1
                  ibladt(j) = iblad(j)
 1389         continue
              do 1390 j=SMONRN+1,2,1
                  ibladt(15+j) = idum
 1390         continue
              do 1395 j=SMOORN+1,3,1
                  ibladt(18+j) = idum
 1395         continue
              call docrgn (ireg(i),ibladt,rblad,nblad,ilev,nlev,ifl,
     1                     kfl,cmsg,kerr)
          endif
c
c.........Lathe cycles
c
          if (MTPDYN .eq. 2 .and. ICYLFL(1) .eq. 1) then
              ilev(nlev-1) = 10
              call docrgn (ireg(i),ilcyc,rlcyc,nlcyc,ilev,nlev,ifl,
     1                     kfl,cmsg,kerr)
          endif
c
c.........Slowdown blocks
c
          if (SLWFL(1) .eq. 1) then
              nslw   = 1 + SLWFL(4) * 3
              ilev(nlev-1) = 12
              call docrgn (ireg(i),islw,rslw,nslw,ilev,nlev,ifl,kfl,
     1                     cmsg,kerr)
          endif
c
c.........GOHOME/POSTN registers
c
c         do 1400 j=1,nmisc,1
c             imisc(j) = imisct(j)
c1400     continue
c
c............GOHOME
c
          if (HOMCOD(1) .eq. 0 .and. HOMCOD(2) .eq. 0 .and.
     1        HOMCOD(3) .eq. 0 .and. HOMCOD(4) .eq. 0) then
              do 1410 j=5,14,1
                  imisc(j) = idum
 1410         continue
          else
              do 1420 j=1,3,1
                  if (NUMLIN(j) .eq. 0) then
                      imisc(j*2-1+4) = idum
                      imisc(j*2+4) = idum
                  else if (NUMLIN(j) .eq. 1) then
                      imisc(j*2+4) = idum
                  endif
 1420         continue
              do 1425 j=IRTNUM+1,4,1
                  imisc(j+10) = idum
 1425         continue
          endif
c
c............POSTN
c
          if (PSTNCD(1) .eq. 0 .and. PSTNCD(2) .eq. 0) then
              do 1430 j=18,27,1
                  imisc(j) = idum
 1430         continue
          else
              do 1440 j=1,3,1
                  if (NUMLIN(j) .eq. 0) then
                      imisc(j*2-1+17) = idum
                      imisc(j*2+17) = idum
                  else if (NUMLIN(j) .eq. 1) then
                      imisc(j*2+17) = idum
                  endif
 1440         continue
              do 1445 j=IRTNUM+1,4,1
                  imisc(j+23) = idum
 1445         continue
          endif
          ilev(nlev-1) = 17
          call docrgn (ireg(i),imisc,rmisc,nmisc,ilev,nlev,ifl,kfl,
     1                 cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c.........Transformation registers
c
          if (XFMFL(1) .eq. 1) then
              ilev(nlev-1) = 18
              do 1450 j=1,nxfrm,1
                  ixfrmt(j) = ixfrm(j)
 1450         continue
              if (XFMFL(3) .eq. 1) then
                  ixfrmt(8) = idum
                  ixfrmt(9) = idum
                  ixfrmt(10) = idum
              else
                  ixfrmt(11) = idum
                  ixfrmt(12) = idum
                  ixfrmt(13) = idum
                  ixfrmt(14) = idum
              endif
              call docrgn (ireg(i),ixfrmt,rxfrm,nxfrm,ilev,nlev,ifl,
     1                     kfl,cmsg,kerr)
          endif
c
c.........Stringer machine
c
          if (MACHTP .eq. 5) then
              ilev(nlev-1) = 19
              call docrgn (ireg(i),istrg,rstrg,nstrg,ilev,nlev,ifl,kfl,
     1                     cmsg,kerr)
          endif
 1500 continue
c
c...Output G-codes or M-codes
c
      if (kfl .ne. 1) then
          call docgcd (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
      call docclr (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docrgn (kreg,kary,kval,knary,klev,knlev,kfl,ktyp,cmsg,
c                        kerr)
c
c   FUNCTION:  This routine finds all descriptions/definitions for a
c              specific register and will either output them if this is
c              a standard register, or will buffer them for sorting if
c              it is a G-code or M-code.
c
c   INPUT:  kreg    I*4  D1  -  Register to find definitions for.
c
c           kary    I*4  Dn  -  Pointers to within KPOSMP array of vari-
c                               ables to check for matches with 'kreg'.
c
c           kval    I*4  Dn  -  Pointers to within POSMAP array of vari-
c                               ables which contain values which go with
c                               the KPOSMP register variables.
c
c           knary   I*4  D1  -  Number of variables in 'kary' and 'kval'.
c
c           klev    I*4  D10 -  Level values of documentation menu text
c                               which apply to the 'kval' array.
c
c           knlev   I*4  D1  -  Current level depth for 'klev'.
c
c           kfl     I*4  D1  -  0 = A description has not yet been
c                               output for this register.  1 = A
c                               description (including the register
c                               format and label) has been output for
c                               this register.
c
c           ktyp    I*4  D1  -  Type of descriptions to find.  1 =
c                               Register, 2 = G-code, 3 = M-code.
c
c   OUTPUT: kfl     I*4  D1  -  Returns 1 if at least one description
c                               was found.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docrgn (kreg,kary,kval,knary,klev,knlev,kfl,ktyp,
     1                   cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (REGSTO,POSMAP(1032))
c
      real*8 REGSTO(MAXFMT)
c
      integer*4 kreg,kary(10),kval(10),knary,kerr,kfl,klev(10),knlev,
     1          ktyp
c
      character*(*) cmsg
c
      integer*4 i,nct1,nct2,icod,ival,ityp,ncl,nc
c
      character*20 lnum
      character*80 t1buf,t2buf,sbuf
c
c...Find all descriptions for input register
c
      do 1200 i=1,knary,1
c
c......Determine if this is a register,
c......G-code, or an M-code
c
          icod   = KPOSMP(kary(i))
          ival   = kval(i)
          call doctyp (icod,ival,ityp)
          if (ityp .ne. ktyp) go to 1200
c
c.........Register
c
          if (ityp .eq. 1) then
              if (kreg .eq. KPOSMP(kary(i))) then
                  klev(knlev) = i
                  call docprm (t2buf,nct2,klev,knlev)
                  if (kfl .eq. 0) then
                      call docrgf (kreg,0.d0,t1buf,nct1,2)
                      if (REGSTO(kreg) .ne. 0.) then
                          call rtoc (REGSTO(kreg),lnum,ncl)
                          sbuf   = REGID(kreg) // ' / ' // lnum(1:ncl)
                          nc     = ncl    + 5
                      else
                          sbuf   = REGID(kreg)
                          nc     = 2
                      endif
                      call docrgo (t1buf,nct1,t2buf,nct2,sbuf,nc,1,
     1                             cmsg,kerr)
                      kfl    = 1
                  else
                      call docrgo (' ',0,t2buf,nct2,' ',0,0,cmsg,kerr)
                  endif
              endif
c
c.........G-code
c.........M-code
c
          else
              if (IGDES .eq. 200) go to 8000
              klev(knlev) = i
              call docprm (t2buf,nct2,klev,knlev)
              IGDES  = IGDES  + 1
              LGDES(IGDES) = t2buf
              NCGDES(IGDES) = nct2
              IGID(IGDES) = icod
              RGVAL(IGDES) = POSMAP(ival)
         endif
 1200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docusr (cmsg,kerr)
c
c   FUNCTION:  This routine outputs the automatic documentation User
c              Defined Blocks pages.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docusr (cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
      include 'docum.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (NEOB  ,KPOSMP(0841)), (MACHTP,KPOSMP(1201))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTCLM,KPOSMP(1306))
      equivalence (IJKROT,KPOSMP(1739)), (ICYLFL,KPOSMP(1901))
      equivalence (CYLCOD,KPOSMP(1921)), (USRBRG,KPOSMP(2600))
      equivalence (MUSRBK,KPOSMP(3022)), (NCUSRB,KPOSMP(3025))
      equivalence (ICYBLK,KPOSMP(3060)), (ICYLBK,KPOSMP(3080))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 ICYCFL(30),CYCCOD(20),NEOB,ICYLFL(20),CYLCOD(25),
     1          ICYBLK(20),ICYLBK(20),USRBRG(20,20),MUSRBK,NCUSRB(20),
     2          MACHTP,MTPDYN,IRTNUM,IRTCLM,IJKROT
c
      equivalence (DUMMY ,POSMAP(0003)), (USRBVL,POSMAP(2501))
c
      real*8 DUMMY,USRBVL(20,20)
c
      equivalence (LEOB  ,CPOSMP(0971))
c
      character*5 LEOB
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 iusr(12),niusr,musr(14),nmusr,lusr(15),i,j,nco,inc,nc,
     1          nlusr,iout,ist1,iusg(50),ilev(10),nlev,icod,ityp,rusr,
     2          ncusr,cusr(8),nsusr,susr(5)
c
      character*80 sbuf,obuf
c
      data niusr /12/
      data iusr / 849,  850,  854, 1859, 1868, 3023, 3024, 3045, 3046,
     1           3047, 3048, 3123/
c
      data nmusr /14/
      data musr /3060, 3061, 3062, 3063, 3064, 3065, 3066, 3067, 3068,
     1           3069, 3070, 3071, 3072, 3073/
c
      data nlusr /15/
      data lusr /3080, 3081, 3082, 3083, 3084, 3085, 3086, 3087, 3088,
     1           3089, 3090, 3091, 3092, 3093, 3094/
c
      data ncusr /8/
      data cusr /1769,1770,1771,1772,1773,1774,1775,1776/
c
      data rusr /2501/
c
      data nsusr /5/
      data susr /1008, 1009, 1010, 1011, 1012/
c
      data ist1 /9/
c
c...Initialize routine
c
      DLIN   = 10000
      NDHED  = 9
c
c...Get header lines
c
      nlev   = 1
      DHED(7) = ' '
      NCHED(7) = 0
      ilev(1) = 6
      call docprm (DHED(1),NCHED(1),ilev,nlev)
      nlev   = 2
      ilev(2) = 1
      call docprm (DHED(8),NCHED(8),ilev,nlev)
      ilev(2) = 2
      call docprm (DHED(9),NCHED(9),ilev,nlev)
      call dcenhd (DHED(1),NCHED(1),DHED(1),NCHED(1))
      nlev   = 3
c
c...Find all blocks which are actually used
c
      do 100 i=1,MUSRBK,1
          iusg(i) = 0
  100 continue
c
      do 120 i=1,niusr,1
          inc    = KPOSMP(iusr(i))
          if (inc .ne. 0) iusg(inc) = 1
  120 continue
c
      if (MACHTP .ne. 2 .and. ICYCFL(1) .eq. 1) then
          do 140 i=1,nmusr,1
              if (CYCCOD(i) .ne. 0) then
                  inc    = KPOSMP(musr(i))
                  if (inc .ne. 0) iusg(inc) = 1
              endif
  140     continue
      endif
c
      if (MTPDYN .eq. 2 .and. ICYLFL(1) .eq. 1) then
          do 160 i=1,nlusr,1
              if (CYLCOD(i) .ne. 0) then
                  inc    = KPOSMP(lusr(i))
                  if (inc .ne. 0) iusg(inc) = 1
              endif
  160     continue
      endif
c
      if (IRTNUM .ne. 0 .and. IRTCLM .ne. 1 .and. IJKROT .ne. 1) then
          do 170 i=1,IRTNUM*2,1
              inc    = KPOSMP(cusr(i))
              if (inc .ne. 0) iusg(inc) = 1
  170     continue
      endif
c
      if (MACHTP .eq. 5) then
          do 180 i=1,nsusr,1
              inc    = KPOSMP(susr(i))
              if (inc .ne. 0) iusg(inc) = 1
  180     continue
      endif
c
c...Loop through all user defined blocks
c
      do 1000 i=1,MUSRBK,1
          if (NCUSRB(i) .eq. 0 .or. iusg(i) .eq. 0) go to 1000
c
c......Block has been defined
c......Format it
c
          call itoc (i,sbuf,nc,-2)
          obuf   = ' ' // sbuf(1:2)
          nco    = ist1
          iout   = 1
          do 300 j=1,NCUSRB(i),1
c
c.........EOB encountered
c.........output this block
c
              if (USRBRG(j,i) .eq. -3) then
                  if (NEOB .ne. 0) then
                      obuf(nco+1:) = LEOB(1:NEOB)
                      nco    = nco    + NEOB
                  endif
                  call docout (obuf,nco,iout,cmsg,kerr)
                  obuf   = ' '
                  nco    = ist1
                  iout   = 0
                  go to 300
c
c.........Macro Parameter
c
              else if (USRBRG(j,i) .gt. 1000) then
                  icod   = USRBRG(j,i) - 1000
                  call itoc (icod,sbuf,nc,0)
                  nc     = nc     + 1
                  sbuf(nc:) = ','
c
c.........Register with a value
c
              else if (USRBVL(j,i) .ne. DUMMY) then
                  icod   = USRBRG(j,i)
                  inc    = rusr   + (20 * (i-1)) + j - 1
                  call doctyp (icod,inc,ityp)
                  call docrgf (icod,USRBVL(j,i),sbuf,nc,3)
c
c.........Register without a value
c
              else
                  call cdtoc (USRBRG(j,i),USRBVL(j,i),sbuf,nc)
                  sbuf(nc+1:) = '..'
                  nc     = nc     + 2
              endif
              obuf(nco+1:) = ' ' // sbuf(1:nc)
              nco    = nco    + nc     + 1
  300     continue
          if (nco .gt. ist1) call docout (obuf,nco,iout,cmsg,kerr)
c
c......Find all appropriate uses for
c......this User defined block
c
          call docout (' ',0,0,cmsg,kerr)
          ilev(nlev-1) = 3
          do 400 j=1,niusr,1
              if (KPOSMP(iusr(j)) .eq. i) then
                  ilev(nlev) = j
                  call docprm (sbuf,nc,ilev,nlev)
                  obuf   = '               ' // sbuf(1:nc)
                  nco    = nc     + 15
                  call docout (obuf,nco,0,cmsg,kerr)
              endif
  400     continue
c
          if (MACHTP .ne. 2 .and. ICYCFL(1) .eq. 1) then
              ilev(nlev-1) = 4
              do 500 j=1,nmusr,1
                  if (CYCCOD(j) .ne. 0 .and. KPOSMP(musr(j)) .eq. i)
     1                    then
                      ilev(nlev) = j
                      call docprm (sbuf,nc,ilev,nlev)
                      obuf   = '               ' // sbuf(1:nc)
                      nco    = nc     + 15
                      call docout (obuf,nco,0,cmsg,kerr)
                  endif
  500         continue
          endif
c
          if (MTPDYN .eq. 2 .and. ICYLFL(1) .eq. 1) then
              ilev(nlev-1) = 5
              do 600 j=1,nlusr,1
                  if (CYLCOD(j) .ne. 0 .and. KPOSMP(lusr(j)) .eq. i)
     1                    then
                      ilev(nlev) = j
                      call docprm (sbuf,nc,ilev,nlev)
                      obuf   = '               ' // sbuf(1:nc)
                      nco    = nc     + 15
                      call docout (obuf,nco,0,cmsg,kerr)
                  endif
  600         continue
          endif
c
          if (IRTNUM .ne. 0 .and. IRTCLM .ne. 1 .and. IJKROT .ne. 1)
     1            then
              ilev(nlev-1) = 6
              do 700 j=1,IRTNUM*2,1
                  if (KPOSMP(cusr(j)) .eq. i) then
                      ilev(nlev) = j
                      call docprm (sbuf,nc,ilev,nlev)
                      obuf   = '               ' // sbuf(1:nc)
                      nco    = nc     + 15
                      call docout (obuf,nco,0,cmsg,kerr)
                  endif
  700         continue
          endif
c
          if (MACHTP .eq. 5) then
              ilev(nlev-1) = 7
              do 710 j=1,nsusr,1
                  if (KPOSMP(susr(j)) .eq. i) then
                      ilev(nlev) = j
                      call docprm (sbuf,nc,ilev,nlev)
                      obuf   = '               ' // sbuf(1:nc)
                      nco    = nc     + 15
                      call docout (obuf,nco,0,cmsg,kerr)
                  endif
  710         continue
          endif
 1000 continue
      call docclr (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  doctyp (kcod,kval,ktyp)
c
c   FUNCTION:  This routine returns the type of register code for the
c              register 'kcod' with a value of 'kval'.
c
c   INPUT:  kcod    I*4  D1  -  Register number.
c
c           kval    I*4  D1  -  Pointer within POSMAP to value
c                               associated with 'kcod'.
c
c   OUTPUT: ktyp    I*4  D1  -  1 = Register, 2 = G-code, 3 = M-code.
c
c***********************************************************************
c
      subroutine doctyp (kcod,kval,ktyp)
c
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 kcod,kval,ktyp
c
c...G-code
c
      if ((kcod .eq. -1 .or. (kcod .ge. 18 .and. kcod .le. 28)) .and.
     1    kval .ne. 0 .and. POSMAP(kval) .ne. DUMMY) then
          ktyp   = 2
          call gcdgrp (kcod,POSMAP(kval))
c
c...M-code
c
      else if ((kcod .eq. -2 .or. (kcod .ge. 37 .and. kcod .le. 47))
     1         .and. kval .ne. 0 .and. POSMAP(kval) .ne. DUMMY) then
          ktyp   = 3
          call mcdgrp (kcod,POSMAP(kval))
c
c...Standard register
c
      else
          ktyp   = 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docgcd (cmsg,kerr)
c
c   FUNCTION:  This routine outputs the buffered G and M-code descrip-
c              tions.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docgcd (cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i,isw,ncdes,id,nct
c
      real*8 rval
c
      character*80 ldes,tbuf
c
c...Sort the G/M-codes
c
      if (IGDES .eq. 0) go to 8000
  100 isw    = 0
      do 200 i=1,IGDES-1,1
          if (RGVAL(i) .gt. RGVAL(i+1)) then
              ldes   = LGDES(i)
              ncdes  = NCGDES(i)
              id     = IGID(i)
              rval   = RGVAL(i)
c
              LGDES(i) = LGDES(i+1)
              NCGDES(i) = NCGDES(i+1)
              IGID(i) = IGID(i+1)
              RGVAL(i) = RGVAL(i+1)
c
              LGDES(i+1) = ldes
              NCGDES(i+1) = ncdes
              IGID(i+1) = id
              RGVAL(i+1) = rval
              isw    = 1
          endif
  200 continue
      if (isw .eq. 1) go to 100
c
c...Output the G/M-codes
c
      do 300 i=1,IGDES,1
          if (i .ne. 1 .and. RGVAL(i) .eq. RGVAL(i-1)) then
              call docrgo (' ',0,LGDES(i),NCGDES(i),' ',0,0,cmsg,kerr)
          else
              call docrgf (IGID(i),RGVAL(i),tbuf,nct,3)
              call docrgo (tbuf,nct,LGDES(i),NCGDES(i),REGID(IGID(i)),
     1                     2,1,cmsg,kerr)
          endif
  300 continue
      if (kerr .ne. 0) go to 8000
      call docclr (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docrgo (cfrm,kfrm,cdef,kdef,creg,kreg,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine formats and outputs a Register/G-code/M-code
c              Description record.
c
c   INPUT:  cfrm    C*n  D1  -  Register format field text.
c
c           kfrm    I*4  D1  -  Number of characters in 'cfrm'.
c
c           cdef    C*n  D1  -  Definition field text.
c
c           kdef    I*4  D1  -  Number of characters in 'cdef'.
c
c           creg    C*n  D1  -  Register ID field text.
c
c           kreg    I*4  D1  -  Number of characters in 'creg'.
c
c           kfl     I*4  D1  -  1 = Starting a new register description
c                               record.  0 = nth description for this
c                               register.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docrgo (cfrm,kfrm,cdef,kdef,creg,kreg,kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
c
      integer*4 kfrm,kdef,kreg,kfl,kerr
c
      character*(*) cfrm,cdef,creg,cmsg
c
      integer*4 nco,ibg(4),ien(4)
c
      character*80 obuf
c
      data ibg /1,28,71,68/, ien /20,59,72,80/
c
c...Build register output record
c
      obuf   = ' '
      nco    = 0
c
c......Register format
c
      if (kfrm .ne. 0) then
          obuf(ibg(1):ien(1)) = cfrm(1:kfrm)
          nco    = ibg(1) + kfrm   - 1
      endif
c
c......Register definition
c
      if (kdef .ne. 0) then
          obuf(ibg(2):ien(2)) = cdef(1:kdef)
          nco    = ibg(2) + kdef   - 1
      endif
c
c......Actual register
c
      if (kreg .ne. 0) then
          if (kreg .gt. 2) then
              obuf(ibg(4):ien(4)) = creg(1:kreg)
              nco    = ibg(4) + kreg   - 1
          else
              obuf(ibg(3):ien(3)) = creg(1:kreg)
              nco    = ibg(3) + kreg   - 1
          endif
      endif
c
c...Output register record
c
      if (nco .ne. 0) then
          call docout (obuf,nco,kfl,cmsg,kerr)
      endif
c
c...End of routine
c
 8000 return
      end
