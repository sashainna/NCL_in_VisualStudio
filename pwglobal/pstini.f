c
c***********************************************************************
c
c   FILE NAME: pstini.for
c   CONTAINS:
c               pstini
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pstini.f , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:38:14
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  pstini
c
c   FUNCTION:  This routine initializes the post-processor common
c              variables.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pstini
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MULTAX,KPOSMP(0056)), (NPT   ,KPOSMP(0059))
      equivalence (NCUT  ,KPOSMP(0062)), (NCUTDS,KPOSMP(0063))
      equivalence (ICUTYP,KPOSMP(0064)), (NSHANK,KPOSMP(0067))
      equivalence (ICUCHG,KPOSMP(0068)), (ICLSMP,KPOSMP(0077))
      equivalence (NUMERR,KPOSMP(0082))
      equivalence (IUNIT ,KPOSMP(0087)), (IDUMMY,KPOSMP(0088))
      equivalence (NUMWRN,KPOSMP(0089)), (NUMFAT,KPOSMP(0090))
      equivalence (IDSPLY,KPOSMP(0091)), (IOPSKP,KPOSMP(0092))
      equivalence (RTRMCD,KPOSMP(0093)), (NUMAER,KPOSMP(0097))
      equivalence (KERRSV,KPOSMP(0109))
      equivalence (LIMERR,KPOSMP(0110)), (ICLIPS,KPOSMP(0112))
      equivalence (NPLANE,KPOSMP(0113)), (LINTSW,KPOSMP(0114))
      equivalence (ICLMSV,KPOSMP(0151)), (NKPOSM,KPOSMP(0171))
      equivalence (NPOSMA,KPOSMP(0172)), (NCPOSM,KPOSMP(0173))
      equivalence (SIMACT,KPOSMP(0174)), (PPRIPV,KPOSMP(0175))
      equivalence (IPVCTR,KPOSMP(0177)), (ICYCFL,KPOSMP(0181))
      equivalence (CYCCOD,KPOSMP(0211)), (CYCREG,KPOSMP(0231))
      equivalence (ICYTIM,KPOSMP(0251)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (MSGSEQ,KPOSMP(0301))
      equivalence (MSGEOB,KPOSMP(0302)), (MSGTAB,KPOSMP(0303))
      equivalence (INSSEQ,KPOSMP(0304)), (INSEOB,KPOSMP(0305))
      equivalence (OPSSEQ,KPOSMP(0306)), (STPSEQ,KPOSMP(0307))
      equivalence (MCHOPT,KPOSMP(0308)), (MSGALN,KPOSMP(0349))
      equivalence (PODACD,KPOSMP(0372))
      equivalence (MOTREG,KPOSMP(0381)), (REGSW ,KPOSMP(0405))
      equivalence (IREGST,KPOSMP(0497)), (REGFRC,KPOSMP(0603))
      equivalence (MACCOD,KPOSMP(0801)), (IRTMAX,KPOSMP(0811))
      equivalence (PODSCD,KPOSMP(0818)), (IJKREG,KPOSMP(0827))
      equivalence (IFWDFL,KPOSMP(0832)), (IFWSFL,KPOSMP(0833))
      equivalence (NRGORD,KPOSMP(0837)), (MULTGC,KPOSMP(0838))
      equivalence (MULTMC,KPOSMP(0839)), (MGCD  ,KPOSMP(0840))
      equivalence (NEOB  ,KPOSMP(0841)), (TABSEQ,KPOSMP(0842))
      equivalence (NCBMSG,KPOSMP(0843)), (NCEMSG,KPOSMP(0844))
      equivalence (ISEQSW,KPOSMP(0845)), (SEQFRQ,KPOSMP(0846))
      equivalence (SEQCOD,KPOSMP(0847)), (ALNCOD,KPOSMP(0848))
      equivalence (AL1BLK,KPOSMP(0849)), (AL2BLK,KPOSMP(0850))
      equivalence (ISEQBK,KPOSMP(0852)), (ISQBKV,KPOSMP(0853))
      equivalence (AL3BLK,KPOSMP(0854)), (LSTREG,KPOSMP(0857))
      equivalence (MACPRG,KPOSMP(0858)), (MACPSW,KPOSMP(0859))
      equivalence (XFMREG,KPOSMP(0931))
      equivalence (XFMCD ,KPOSMP(0946)), (SGMREG,KPOSMP(0951))
      equivalence (XFMFL ,KPOSMP(0969)), (IXFMFL,KPOSMP(0989))
      equivalence (SGMACT,KPOSMP(0996)), (SGMHFL,KPOSMP(0998))
      equivalence (SGMBLK,KPOSMP(1008)), (SGMCOD,KPOSMP(1018))
      equivalence (MOTFLG,KPOSMP(1073)), (NOUTSW,KPOSMP(1074))
      equivalence (OPSTCD,KPOSMP(1075)), (STOPCD,KPOSMP(1076))
      equivalence (NOPSKP,KPOSMP(1077)), (UNTCOD,KPOSMP(1078))
      equivalence (REWCOD,KPOSMP(1080)), (NTAXCD,KPOSMP(1090))
      equivalence (IPACKD,KPOSMP(1101)), (IPARTO,KPOSMP(1102))
      equivalence (PCHSPC,KPOSMP(1103)), (NRWSTP,KPOSMP(1104))
      equivalence (FINCOD,KPOSMP(1105)), (NEOT  ,KPOSMP(1106))
      equivalence (IPCHPN,KPOSMP(1107)), (IPCHSQ,KPOSMP(1108))
      equivalence (PCHBNC,KPOSMP(1109)), (MACSPC,KPOSMP(1110))
      equivalence (MROTTV,KPOSMP(1334))
      equivalence (IFIRST,KPOSMP(1131)), (ICSMRG,KPOSMP(1132))
      equivalence (ICSMFL,KPOSMP(1133)), (MXCKSM,KPOSMP(1134))
      equivalence (IRWEOB,KPOSMP(1135)), (NBOT  ,KPOSMP(1136))
      equivalence (BRKOP ,KPOSMP(1137)), (PLCYCD,KPOSMP(0296))
      equivalence (IPAGE ,KPOSMP(1151)), (IPRDES,KPOSMP(1154))
      equivalence (IPLINE,KPOSMP(1152)), (PRTBNC,KPOSMP(1153))
      equivalence (IPRTOP,KPOSMP(1176)), (IRTOUT,KPOSMP(0813))
      equivalence (MACHTP,KPOSMP(1201)), (NUMLIN,KPOSMP(1202))
      equivalence (ACTLIN,KPOSMP(1205)), (LNCALC,KPOSMP(1208))
      equivalence (FRMFLG,KPOSMP(1211)), (MAXAXS,KPOSMP(1212))
      equivalence (AXSPRI,KPOSMP(1213)), (HLDBCK,KPOSMP(1223))
      equivalence (IZSWIV,KPOSMP(1224)), (LTHXY ,KPOSMP(1225))
      equivalence (INCR  ,KPOSMP(1226)), (INCRFC,KPOSMP(1227))
      equivalence (LTHDIA,KPOSMP(1228)), (ISCIRC,KPOSMP(1238))
      equivalence (ICRXYZ,KPOSMP(1239)), (MOTCOD,KPOSMP(1240))
      equivalence (ABSCOD,KPOSMP(1241)), (INCCOD,KPOSMP(1242))
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (MCHPLN,KPOSMP(1261)), (POLCOD,KPOSMP(1262))
      equivalence (IPOLIN,KPOSMP(1265)), (LNRADJ,KPOSMP(1277))
      equivalence (LRTRCT,KPOSMP(1278))
      equivalence (SPITAX,KPOSMP(1279)), (NOPIVS,KPOSMP(1282))
      equivalence (NOTABS,KPOSMP(1283)), (IRTMOD,KPOSMP(1284))
      equivalence (IRTSCL,KPOSMP(1288)), (IRTCLW,KPOSMP(1292))
      equivalence (AXSINC,KPOSMP(1296)), (IRTCLM,KPOSMP(1306))
      equivalence (CLMPCD,KPOSMP(1307)), (CLMPBK,KPOSMP(1327))
      equivalence (RSIZCD,KPOSMP(1328)), (RSIZFL,KPOSMP(1333))
      equivalence (LRTTAD,KPOSMP(1335)), (ACTBLN,KPOSMP(1336))
      equivalence (IRETMD,KPOSMP(1340)), (IRTRTE,KPOSMP(1343))
      equivalence (IRETFL,KPOSMP(1345)), (ISACIR,KPOSMP(1346))
      equivalence (NCONTB,KPOSMP(1347)), (LRTRFL,KPOSMP(1348))
      equivalence (MOT1ST,KPOSMP(1349)), (ICLMPF,KPOSMP(1351))
      equivalence (NROT  ,KPOSMP(1366)), (ICIRFL,KPOSMP(1367))
      equivalence (ICRFMT,KPOSMP(1368)), (ICRSGN,KPOSMP(1369))
      equivalence (ICRPFL,KPOSMP(1370)), (ICRADJ,KPOSMP(1371))
      equivalence (ICRBRK,KPOSMP(1372)), (MLNFRC,KPOSMP(1373))
      equivalence (IRTSHF,KPOSMP(1374)), (IRSRTE,KPOSMP(1375))
      equivalence (CIRCOD,KPOSMP(1378)), (ICRIJP,KPOSMP(1384))
      equivalence (ICMBSW,KPOSMP(1385)), (ICSPRE,KPOSMP(1386))
      equivalence (MRTFRC,KPOSMP(1387))
      equivalence (PLNCOD,KPOSMP(4222)), (IPLNDF,KPOSMP(1388))
      equivalence (ICRDIM,KPOSMP(1389)), (ICRDIR,KPOSMP(1390))
      equivalence (ICIRSW,KPOSMP(1391)), (IHELIX,KPOSMP(1392))
      equivalence (NUMINF,KPOSMP(1396)), (ICRPLF,KPOSMP(1397))
      equivalence (ICRPLS,KPOSMP(1398))
      equivalence (LTHXD ,KPOSMP(1339))
      equivalence (AXSSPT,KPOSMP(1401)), (ROTDCD,KPOSMP(1405))
      equivalence (IRTDUP,KPOSMP(1413)), (IRTDAC,KPOSMP(1417))
      equivalence (ISCWRK,KPOSMP(1453)), (IRDEAD,KPOSMP(1465))
      equivalence (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506)), (HLDFLG,KPOSMP(1622))
      equivalence (LOOKFL,KPOSMP(1628)), (ACHEAD,KPOSMP(1645))
      equivalence (IACFLG,KPOSMP(1648)), (SLWFL ,KPOSMP(1701))
      equivalence (SLWCD ,KPOSMP(1706)), (ISLWDN,KPOSMP(1721))
      equivalence (ISLWFD,KPOSMP(1723)), (IACLFL,KPOSMP(1732))
      equivalence (ICIDLT,KPOSMP(1741))
      equivalence (ICYDLT,KPOSMP(1742)), (IMXDRP,KPOSMP(1743))
      equivalence (IACCFD,KPOSMP(1744)), (IACCFL,KPOSMP(1745))
      equivalence (IC3AXF,KPOSMP(1747)), (CLMPUB,KPOSMP(1757))
      equivalence (IRTFRC,KPOSMP(1777))
      equivalence (ITP   ,KPOSMP(1801)), (NOTOOL,KPOSMP(1802))
      equivalence (TOOLFL,KPOSMP(1804)), (TURFL ,KPOSMP(1824))
      equivalence (TLDIG ,KPOSMP(1829)), (TLCCD ,KPOSMP(1839))
      equivalence (TLCBLK,KPOSMP(1859)), (TLOCD ,KPOSMP(1860))
      equivalence (TLOBLK,KPOSMP(1868)), (TLOSGN,KPOSMP(1869))
      equivalence (ITSELF,KPOSMP(1870)), (PODCCD,KPOSMP(0379))
      equivalence (LSTGRP,KPOSMP(1871)), (LSTTSP,KPOSMP(1872))
      equivalence (TLOFPL,KPOSMP(1875)), (TLOFDR,KPOSMP(1876))
      equivalence (TOFNXT,KPOSMP(1877)), (LTHPCL,KPOSMP(1899))
      equivalence (ICYLFL,KPOSMP(1901))
      equivalence (IJKROT,KPOSMP(1739))
      equivalence (CYLCOD,KPOSMP(1921)), (CYLREG,KPOSMP(1946))
      equivalence (ICYLTM,KPOSMP(1971)), (IZIGON,KPOSMP(0370))
      equivalence (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (USRBRG,KPOSMP(2600))
      equivalence (NUSRBK,KPOSMP(3021)), (MUSRBK,KPOSMP(3022))
      equivalence (MOBLK ,KPOSMP(3023)), (STBLK ,KPOSMP(3024))
      equivalence (NCUSRB,KPOSMP(3025)), (NMOBLK,KPOSMP(3026))
      equivalence (OPKBLK,KPOSMP(3046)), (OPSBLK,KPOSMP(3047))
      equivalence (STPBLK,KPOSMP(3048)), (NUSRST,KPOSMP(3059))
      equivalence (ICYBLK,KPOSMP(3060)), (ICYLBK,KPOSMP(3080))
      equivalence (NSPRG ,KPOSMP(3101)), (IZIGAD,KPOSMP(0371))
      equivalence (SPNTYP,KPOSMP(3102)), (SPNSFM,KPOSMP(3103))
      equivalence (IFEDSW,KPOSMP(3104)), (SPNDCD,KPOSMP(3105))
      equivalence (SPNBCD,KPOSMP(3109)), (IROTFT,KPOSMP(3110))
      equivalence (IFDSUP,KPOSMP(3111)), (SPNRCD,KPOSMP(3115))
      equivalence (SPNFCD,KPOSMP(3118)), (SPNOCD,KPOSMP(3121))
      equivalence (SPNBLK,KPOSMP(3123)), (SPNOFF,KPOSMP(3124))
      equivalence (NSPNTB,KPOSMP(3125)), (COOLCD,KPOSMP(3128))
      equivalence (SPCOCD,KPOSMP(3132)), (COLBLK,KPOSMP(3138))
      equivalence (SPNSCD,KPOSMP(3144)), (LSTCOL,KPOSMP(3140))
      equivalence (SPNDIR,KPOSMP(3141)), (SPNMOD,KPOSMP(3142))
      equivalence (SPNRNG,KPOSMP(3143)), (ICOLSW,KPOSMP(3146))
      equivalence (SPNXCD,KPOSMP(3147)), (IFDTYP,KPOSMP(3148))
      equivalence (IFITYP,KPOSMP(3150)), (IFOTYP,KPOSMP(3151))
      equivalence (NFDSUP,KPOSMP(3152)), (LNCRAP,KPOSMP(3212))
      equivalence (IFDFRC,KPOSMP(3153)), (FEDOCD,KPOSMP(3217))
      equivalence (IFDCTP,KPOSMP(3157))
      equivalence (IFDOUT,KPOSMP(3158)), (FEDCD ,KPOSMP(3162))
      equivalence (FEDMCD,KPOSMP(3170)), (IFDEXD,KPOSMP(3178))
      equivalence (IFDUNT,KPOSMP(3182)), (NFEDTB,KPOSMP(3183))
      equivalence (IFDSPC,KPOSMP(3185)), (IFDLTP,KPOSMP(3187))
      equivalence (IDPMOV,KPOSMP(3188)), (IRPSUP,KPOSMP(3189))
      equivalence (NRAPCD,KPOSMP(3190)), (RAPCD ,KPOSMP(3191))
      equivalence (IRPALT,KPOSMP(3196)), (IRPTAX,KPOSMP(3197))
      equivalence (IRPMOD,KPOSMP(3198)), (IRAP  ,KPOSMP(3199))
      equivalence (IRAPSW,KPOSMP(3205)), (IFDCIR,KPOSMP(3206))
      equivalence (POSFDF,KPOSMP(3208)), (POSFED,KPOSMP(3209))
      equivalence (IFDRAD,KPOSMP(3210)), (IFDRCN,KPOSMP(3211))
      equivalence (IFDRCD,KPOSMP(3213))
      equivalence (IRPRET,KPOSMP(3219)), (IFDADJ,KPOSMP(3243))
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
      equivalence (ICUTDO,KPOSMP(3301)), (INMSFL,KPOSMP(3322))
      equivalence (INMDEF,KPOSMP(3323)), (INMLFL,KPOSMP(3324))
      equivalence (DELYCD,KPOSMP(3351))
      equivalence (DELYFL,KPOSMP(3356)), (HOMCOD,KPOSMP(3361))
      equivalence (HOMREG,KPOSMP(3366)), (PSTNCD,KPOSMP(3376))
      equivalence (PSTNRG,KPOSMP(3381)), (PSTNOF,KPOSMP(3396))
      equivalence (IPSTNF,KPOSMP(3397)), (EXCLPS,KPOSMP(3421))
      equivalence (IRPSMD,KPOSMP(3242))
      equivalence (EXCLNE,KPOSMP(3469)), (NUMEXC,KPOSMP(3517))
      equivalence (EXCLNM,KPOSMP(3518)), (REGENC,KPOSMP(3522))
      equivalence (IREGVL,KPOSMP(3614)), (REGORD,KPOSMP(3707))
      equivalence (ICY2FL,KPOSMP(4004))
      equivalence (MXTRAN,KPOSMP(4003)), (MXFLAG,KPOSMP(4002))
      equivalence (MXGTOL,KPOSMP(4001)), (MODROT,KPOSMP(4031))
      equivalence (MDTLIN,KPOSMP(4032)), (ALNCUT,KPOSMP(4040))
      equivalence (ALNCSK,KPOSMP(4041)), (ALNMOV,KPOSMP(4045))
      equivalence (ALNMSK,KPOSMP(4046)), (ALNMRP,KPOSMP(4047))
      equivalence (ALNREN,KPOSMP(4048)), (ALNMOD,KPOSMP(4049))
      equivalence (NOPSON,KPOSMP(4050))
      equivalence (SMOKON,KPOSMP(4071)), (SMOKAN,KPOSMP(4072))
      equivalence (SMOCCD,KPOSMP(4073)), (SMONRN,KPOSMP(4075))
      equivalence (SMONCD,KPOSMP(4076)), (SMOORN,KPOSMP(4079))
      equivalence (SMOOCD,KPOSMP(4080)), (BSPLSW,KPOSMP(4084))
      equivalence (ISBSPL,KPOSMP(4085)), (BSPLFL,KPOSMP(4088))
      equivalence (BSPCOD,KPOSMP(4089)), (BSPPCD,KPOSMP(4090))
      equivalence (BSPFRC,KPOSMP(4091))
      equivalence (LCRFMT,KPOSMP(4103)), (LFLMCD,KPOSMP(4104))
      equivalence (LDLMCD,KPOSMP(4105)), (MILREG,KPOSMP(4106))
      equivalence (LFDLCD,KPOSMP(4107)), (LPCREG,KPOSMP(4108))
      equivalence (LTHREG,KPOSMP(4110))
      equivalence (LFIRCD,KPOSMP(4112)), (LDIRCD,KPOSMP(4118))
      equivalence (MTPDYN,KPOSMP(4126))
      equivalence (IRFSUP,KPOSMP(4136)), (IRDSUP,KPOSMP(4137))
      equivalence (FRAPCD,KPOSMP(4138)), (DRAPCD,KPOSMP(4139))
      equivalence (LTCREG,KPOSMP(4195)), (LTPREG,KPOSMP(4201))
      equivalence (MODCYL,KPOSMP(4204)), (LUNREG,KPOSMP(4205))
      equivalence (MCYNUM,KPOSMP(4207))
      equivalence (ISRCYL,KPOSMP(4205)), (MOTEXP,KPOSMP(4211))
      equivalence (ICRHEL,KPOSMP(4250))
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
      equivalence (DUMMY ,POSMAP(0003)), (METCNV,POSMAP(0004))
      equivalence (FTUNT ,POSMAP(0005)), (INFZON,POSMAP(0071))
      equivalence (HOME  ,POSMAP(0151)), (PODAVL,POSMAP(2494))
      equivalence (CLRPLN,POSMAP(0161)), (DELYVL,POSMAP(0165))
      equivalence (DELYTM,POSMAP(0166)), (HOMCDV,POSMAP(0167))
      equivalence (PSTNCV,POSMAP(0172)), (RETPOS,POSMAP(0177))
      equivalence (CLSAV ,POSMAP(0201)), (CUTTER,POSMAP(0744))
      equivalence (CUTOFS,POSMAP(0766))
      equivalence (GRANGE,POSMAP(0801)), (PODSVL,POSMAP(3594))
      equivalence (MRANGE,POSMAP(0955)), (REGSTO,POSMAP(1032))
      equivalence (ISEQ  ,POSMAP(1164))
      equivalence (SEQINC,POSMAP(1165)), (SEQMAX,POSMAP(1167))
      equivalence (OPSTVL,POSMAP(1168)), (STOPVL,POSMAP(1169))
      equivalence (UNTCDV,POSMAP(1170)), (REWCDV,POSMAP(1172))
      equivalence (ISEQSV,POSMAP(1182)), (MACCDV,POSMAP(1184))
      equivalence (XFMVL ,POSMAP(1189)),  (LEDBEG,POSMAP(1201))
      equivalence (LEDEND,POSMAP(1202)), (FINCDV,POSMAP(1203))
      equivalence (TAPLEN,POSMAP(1204)), (BRKVR ,POSMAP(1205))
      equivalence (MCHDLS,POSMAP(1221)), (ROTDLS,POSMAP(1233))
      equivalence (AXSDLS,POSMAP(1237))
      equivalence (LNDIST,POSMAP(1251)), (LIMITS,POSMAP(1254))
      equivalence (PPTOLR,POSMAP(1274)), (MOTCDV,POSMAP(1284))
      equivalence (ABSCDV,POSMAP(1285)), (INCCDV,POSMAP(1286))
      equivalence (ORGIN ,POSMAP(1305)), (TRANZ ,POSMAP(1308))
      equivalence (TRANAX,POSMAP(1320))
      equivalence (STONUM,POSMAP(1387)), (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425))
      equivalence (ROTBAS,POSMAP(1435)), (ROTCRM,POSMAP(1439))
      equivalence (PPINCR,POSMAP(1443))
      equivalence (CLMPVL,POSMAP(1454)), (ZIGDEP,POSMAP(0190))
      equivalence (MCHDLT,POSMAP(1474)), (ROTDLT,POSMAP(1486))
      equivalence (LINDLT,POSMAP(1490)), (AXSDLT,POSMAP(1496))
      equivalence (MCHMIN,POSMAP(1506)), (MCHMAX,POSMAP(1518))
      equivalence (ROTMIN,POSMAP(1530)), (ROTMAX,POSMAP(1534))
      equivalence (LINMIN,POSMAP(1538)), (LINMAX,POSMAP(1544))
      equivalence (AXSMIN,POSMAP(1550)), (AXSMAX,POSMAP(1560))
      equivalence (PPMAXD,POSMAP(1584)), (LINDLS,POSMAP(1594))
      equivalence (RSIZCV,POSMAP(1600)), (ROTDVL,POSMAP(2041))
      equivalence (ACLDCL,POSMAP(2099)), (CURSPD,POSMAP(2109))
      equivalence (CIRTOL,POSMAP(2201)), (CIRCDV,POSMAP(2206))
      equivalence (PLNCDV,POSMAP(2208)), (PLNDWL,POSMAP(2212))
      equivalence (POLCDV,POSMAP(2227))
      equivalence (CIRTOS,POSMAP(2241)), (HELCDV,POSMAP(2242))
      equivalence (C3XCDV,POSMAP(2244)), (LNRTOL,POSMAP(2251))
      equivalence (RETFED,POSMAP(2254)), (RETPL ,POSMAP(2258))
      equivalence (TAXTOL,POSMAP(2262))
      equivalence (RETDIS,POSMAP(2279)), (LNRATL,POSMAP(2286))
      equivalence (CTOFRG,POSMAP(2401)), (CUTCVR,POSMAP(2402))
      equivalence (CUTCVL,POSMAP(2410)), (SLWVR ,POSMAP(2451))
      equivalence (SLWVL ,POSMAP(2456)), (USRBVL,POSMAP(2501))
      equivalence (CYCCDV,POSMAP(2901)), (CYCVR ,POSMAP(2926))
      equivalence (CYCPSV,POSMAP(2951)), (CYCJVC,POSMAP(2961))
      equivalence (CYCPLN,POSMAP(2964)), (CYLCDV,POSMAP(2968))
      equivalence (CYLVR ,POSMAP(2993)), (CYPCDV,POSMAP(2998))
      equivalence (FEDOVL,POSMAP(3001)), (FEDCNS,POSMAP(3003))
      equivalence (SPNDVL,POSMAP(3007)), (FDLCNV,POSMAP(3013))
      equivalence (FRTVAL,POSMAP(3014)), (SPNTCD,POSMAP(3024))
      equivalence (SPNTVL,POSMAP(3159))
      equivalence (COOLVL,POSMAP(3294))
      equivalence (SPCOVL,POSMAP(3298)), (MAXSFM,POSMAP(3304))
      equivalence (RPMSAV,POSMAP(3305)), (SFMSAV,POSMAP(3306))
      equivalence (RPM   ,POSMAP(3306)), (SFM   ,POSMAP(3307))
      equivalence (SPNLMT,POSMAP(3309)), (SFMTOL,POSMAP(3317))
      equivalence (SPNRAD,POSMAP(3318)), (FEDMVL,POSMAP(3320))
      equivalence (FDIVCO,POSMAP(3328)), (FEDTCD,POSMAP(3330))
      equivalence (FEDTVL,POSMAP(3420)), (FEDSPC,POSMAP(3510))
      equivalence (FEDRNG,POSMAP(3512)), (FEDLMT,POSMAP(3528))
      equivalence (RDRLMT,POSMAP(3538)), (PFEED ,POSMAP(3540))
      equivalence (MAXIPM,POSMAP(3544)), (MAXDPM,POSMAP(3545))
      equivalence (DPMBPW,POSMAP(3551)), (SEQTIM,POSMAP(3558))
      equivalence (CUTTIM,POSMAP(3559)), (TIMSCL,POSMAP(3566))
      equivalence (RAPLMT,POSMAP(3567)), (RAPVL ,POSMAP(3577))
      equivalence (SPIVEC,POSMAP(3583)), (FEDLEN,POSMAP(3539))
      equivalence (BLDVEC,POSMAP(3589))
      equivalence (BLDELT,POSMAP(3593)), (RETFSV,POSMAP(3600))
      equivalence (TL    ,POSMAP(3601)), (TLTIM ,POSMAP(3721))
      equivalence (TURDIS,POSMAP(3961)), (TLCTIM,POSMAP(3962))
      equivalence (TLCVL ,POSMAP(3963)), (TLOVL ,POSMAP(3983))
      equivalence (TLOBAS,POSMAP(3988)), (LSTTN ,POSMAP(3990))
      equivalence (TLOFRG,POSMAP(3991)), (TSELTM,POSMAP(3992))
      equivalence (TRZX  ,POSMAP(3993)), (MAXTN ,POSMAP(3995))
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
      equivalence (ACCFED,POSMAP(4291)), (MAXVEL,POSMAP(4293))
      equivalence (AXSVEL,POSMAP(4294)), (ACCSTP,POSMAP(4295))
      equivalence (RTRMMX,POSMAP(4360)), (RTRMVL,POSMAP(4364))
      equivalence (ALNDIS,POSMAP(4444)), (SMOCVL,POSMAP(4448))
      equivalence (SMONVL,POSMAP(4454)), (ROTZER,POSMAP(4555))
      equivalence (SMOOVL,POSMAP(4453))
      equivalence (ALNMPS,POSMAP(4461)), (SMORAN,POSMAP(4464))
      equivalence (SMODIS,POSMAP(4465)), (NMLVEC,POSMAP(4492))
      equivalence (NMLSAV,POSMAP(4495)), (CFRCDV,POSMAP(4595))
      equivalence (CDRCDV,POSMAP(4597)), (FLMCDV,POSMAP(4600))
      equivalence (DLMCDV,POSMAP(4601)), (GLMCDV,POSMAP(4602))
      equivalence (MILTOL,POSMAP(4879)), (TLMCDV,POSMAP(4880))
      equivalence (TLLCDV,POSMAP(4881)), (FRAPVL,POSMAP(4882))
      equivalence (DRAPVL,POSMAP(4883)), (BSPCDV,POSMAP(4901))
      equivalence (BSPTOL,POSMAP(4902)), (VTOLER,POSMAP(4911))
      equivalence (RTSHFD,POSMAP(1645)), (REGVAL,POSMAP(5000))
      equivalence (SGMOFS,POSMAP(5099))
      equivalence (SGMSTO,POSMAP(5101)), (SGMHOM,POSMAP(5119))
      equivalence (SGMLMT,POSMAP(5137))
      equivalence (ROTSTO,POSMAP(5213)), (TABORG,POSMAP(5374))
      equivalence (SGMTL ,POSMAP(5550)), (SGMCDV,POSMAP(5552))
c
      equivalence (LMNAME,CPOSMP(0141))
      equivalence (LEOB  ,CPOSMP(0971)), (MSGST ,CPOSMP(0976))
      equivalence (LEDCHR,CPOSMP(0986))
      equivalence (TABCHR,CPOSMP(0987)), (LSTBLK,CPOSMP(0988))
      equivalence (MSGEN ,CPOSMP(1500)), (LOPSKP,CPOSMP(1648))
      equivalence (LRWSTP,CPOSMP(2201)), (LEOT  ,CPOSMP(2211))
      equivalence (PHFILE,CPOSMP(2535)), (LUPDAT,CPOSMP(3001))
      equivalence (CUTSYM,CPOSMP(3009)), (PDFFIL,CPOSMP(3249))
      equivalence (MDESC ,CPOSMP(3853))
      equivalence (PU1EXT,CPOSMP(4067))
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*1 LEDCHR,TABCHR
      character*5 LEOB,LOPSKP
      character*8 LUPDAT
      character*10 LRWSTP,MSGST,MSGEN
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
      character*40 LMNAME,PDFFIL,PHFILE
      character*80 LEOT,PU1EXT,CUTSYM(3),MDESC
      character*512 LSTBLK
c
      integer*4 MULTAX,NUMERR,IUNIT,IDUMMY,NUMWRN,NUMFAT,NROT,NUMAER,
     1          MCHOPT(20),REGORD(MAXFMT),NRGORD,MOT1ST,ICLMPF(10),
     2          ISEQSW,MULTGC,MULTMC,MGCD,NEOB,TABSEQ,NCBMSG,NCEMSG,
     3          REGSW(MAXFMT),IREGVL(MAXFMT),IREGST(MAXFMT),RTRMCD(4),
     4          REGFRC(MAXFMT),MOTFLG,NOUTSW,IPACKD,IPARTO,PCHSPC,
     5          NRWSTP,FINCOD,NEOT,IPCHPN,IPCHSQ,PCHBNC,CLMPUB(2,10)
      integer*4 IFIRST,IPAGE,IPLINE,PRTBNC,MACHTP,NUMLIN(3),LNCRAP,
     1          ACTLIN(3),LNCALC(3),FRMFLG,MAXAXS,AXSPRI(10),HLDBCK,
     2          LTHDIA(2),LTHXY,INCR,INCRFC,MOTCOD,ABSCOD,INCCOD,IRTNUM,
     3          IRTYPE(20),IRDEAD(20),IRTWRK(20),MOTREG(24),NOPIVS,
     4          IRTMOD(4),IRTSCL(4),IRTCLW(4),AXSINC(10),IRTCLM,IZIGON
      integer*4 CLMPCD(2,10),CLMPBK,AXSSPT,KERRSV,LIMERR,MROTTV,IZIGAD,
     2          SEQFRQ,SEQCOD,ALNCOD,AL1BLK,AL2BLK,ISEQBK,IRTOUT(4)
      integer*4 OPSTCD,STOPCD,OPSSEQ,STPSEQ,MSGSEQ,IRTMAX,PODCCD(2),
     1          MSGEOB,MSGTAB,INSSEQ,INSEOB,NOPSKP,IPRDES(2,10),MSGALN,
     2          IDSPLY,IOPSKP,IPRTOP,ICIRFL,ICRFMT,ICRSGN,ICRPFL,
     3          ICRADJ,ICRBRK,ICRHEL(10),CIRCOD(6),ICRIJP,PLNCOD(4),
     4          IPLNDF,POLCOD(3),ICIRSW,IHELIX,LOOKFL(12),ICLMSV(20),NPT
      integer*4 ISCIRC,ICRXYZ,MCHPLN,IPOLIN,IZSWIV,UNTCOD(2),REWCOD,
     1          ISLWDN,SPITAX,LRTRCT,NTAXCD(3),SLWFL(5),SLWCD(15),
     2          ISLWFD,ICYCFL(30),CYCCOD(20),CYCREG(20),PLCYCD(3),
     3          ICYTIM(20),ICYCSW(5),ISQBKV,AL3BLK,ICSMRG,ICSMFL,MILREG,
     4          MXCKSM,IRWEOB,NBOT,BRKOP(10),LRTTAD,PODACD(7),PODSCD(6)
      integer*4 NUMINF,RSIZCD(5),ICRDIM,RSIZFL,ICYCDO(15),ICRDIR,LTHXD,
     1          IRPRET,IRETMD(2),IRTRTE(2),IRETFL,ISACIR,NCUT,NOTABS,
     2          NCUTDS,NCONTB,ALNCUT,ALNCSK,ALNMOV,ALNMSK,ALNMRP,ALNREN,
     3          ALNMOD,IFWSFL,IFWDFL,MOTEXP,ISRCYL,MODCYL,LUNREG,MCYNUM,
     4          NSHANK,IRTDUP(4),IRTDAC(9,4),IACCFL(2),IACCFD,IFDCTP
      integer*4 ICUTYP(3),ICUCHG,LRTRFL,ICLSMP,MACSPC,ISCWRK(2,4)
      integer*2 FMTDES(10,MAXFMT)
      integer*4 MOBLK,STBLK,NMOBLK,HLDFLG,ITP,NUSRBK,MUSRBK,ACTBLN(3),
     1          NOTOOL,REGBNC(MAXFMT),REGENC(MAXFMT),USRBRG(20,20),
     2          NCUSRB(20),NSPRG,SPNTYP,SPNSFM,ICY2FL(20),MXGTOL,MXFLAG,
     3          MXTRAN,SPNSCD(2),SPNDCD(4),SPNBCD,SPNRCD(3),SPNFCD(3),
     4          IRAP,SPNOCD(2),SPNBLK,SPNOFF,NSPNTB(3),COOLCD(4),
     5          SPCOCD(6)
      integer*4 COLBLK(2),LSTCOL,SPNDIR,SPNMOD,SPNRNG,ICOLSW,SPNXCD,
     1          IROTFT,IFDSUP(4),IFDTYP(2),IFITYP,NFDSUP,IFDFRC(4),
     2          FEDOCD(2),IFDOUT(4),FEDCD(8),FEDMCD(8),IFDEXD(4),IFDUNT,
     3          NFEDTB(2),IFDSPC(2),IFDLTP,IFEDSW,IDPMOV,IRPSUP,NRAPCD,
     4          RAPCD(5),IRPALT,IRPTAX,IRPMOD,IFOTYP,IRAPSW,NOPSON
      integer*4 OPKBLK,OPSBLK,STPBLK,NUSRST,IFDCIR,MDTLIN,MODROT,
     1          TOOLFL(20),TURFL(5),TLDIG(2,5),TLCCD(20),TLCBLK,
     2          TLOCD(8),TLOBLK,TLOSGN,ITSELF,LSTGRP,LSTTSP,TLOFPL,
     3          TLOFDR,TOFNXT,POSFDF,POSFED,NKPOSM,NCPOSM,NPOSMA,
     4          CUTCFL(20),CUTCCD(30),ICUTDO(15),SMOKON,SMOKAN,
     5          ICMBSW,ICSPRE,SMOCCD(2),IJKROT,IJKREG(3),IFDADJ
      integer*4 DELYCD(5),DELYFL(5),ICYBLK(20),SMONCD(3),SMOOCD(3),
     2          HOMCOD(5),HOMREG(10),ICYLFL(20),CYLCOD(25),SMONRN,
     3          CYLREG(25),ICYLBK(20),ICYLTM(20),PSTNCD(5),PSTNRG(15),
     4          SMOORN,PSTNOF,IPSTNF,NUMEXC,EXCLNM(4),ICRPLF,LFDLCD,
     5          EXCLPS(12,4),EXCLNE(12,4),IFDRAD,IFDRCD(4),IFDRCN,
     6          LCRFMT,LFLMCD,LDLMCD,LFIRCD(6),LDIRCD(6),LTHPCL(2),
     7          LPCREG(2),LTHREG,LTCREG(3),LTPREG(2),IRFSUP,IRDSUP,
     8          FRAPCD,DRAPCD,MTPDYN,ICRPLS(3),ISBSPL,BSPLSW,BSPLFL,
     9          BSPCOD,BSPPCD,BSPFRC(4),ICLIPS,NPLANE,LINTSW,ICYDLT
      integer*4 ICIDLT,IMXDRP,ROTDCD(2,4),SIMACT,IC3AXF(10),
     1          MACCOD(5),LSTREG,MACPRG,MACPSW(30),XFMFL(20),XFMCD(5),
     2          XFMREG(10),INMSFL,INMDEF,INMLFL,MRTFRC,IRTFRC,ACHEAD,
     3          IACFLG(5),IRTSHF,IRSRTE(3),LNRADJ,MLNFRC,IACLFL,PPRIPV,
     4          IRPSMD,IPVCTR,SGMREG(6,3),SGMACT(2),SGMBLK(10),
     5          SGMHFL(10),SGMCOD(10),IXFMFL(5)
c
      real*8 PI,RAD,DUMMY,METCNV,CLSAV(21),CUTTER(7),GRANGE(14,11),
     1       MRANGE(7,11),REGSTO(MAXFMT),REGVAL(MAXFMT),LEDBEG,LEDEND,
     2       FINCDV,LNDIST(3),LIMITS(2,10),PPTOLR(10),MOTCDV,ABSCDV,
     3       INCCDV,ORGIN(3),TRANZ(12),TRANAX(20),TABORG(3,20),
     4       STONUM(3,4),LINSTO(6),ROTSTO(20,2),AXSSTO(10),ROTBAS(4),
     5       ROTCRM(4),ISEQ,RTRMMX(4),RTRMVL(4)
      real*8 PPINCR(10),ROTZER(2),CLMPVL(2,10),USRBVL(20,20),TL(120),
     1       SEQINC,SPNDVL(4),SPNTCD(135),SPNTVL(135),COOLVL(4),FTUNT,
     2       SPCOVL(6),SPNLMT(2,4),MAXSFM,RPMSAV,SFMSAV,RPM,SFM,SFMTOL,
     3       SPNRAD,FEDOVL(2),FEDMVL(8),FEDCNS(4),FDIVCO(2),FEDTCD(90),
     4       FEDTVL(90),FEDSPC(2),FEDRNG(2,8),FEDLMT(10),RDRLMT,PFEED(4)
      real*8 MAXIPM,MAXDPM,MCHDLT(3,4),ROTDLT(4),LINDLT(6),AXSDLT(10),
     1       MCHMIN(3,4),MCHMAX(3,4),ROTMIN(4),ROTMAX(4),LINMIN(6),
     2       LINMAX(6),AXSMIN(10),AXSMAX(10),DPMBPW(2),SEQTIM,CUTTIM,
     3       TLTIM(120),TIMSCL,PPMAXD(10),RAPLMT(10),RAPVL(5),SEQMAX,
     4       OPSTVL,STOPVL,MCHDLS(3,4),ROTDLS(4),AXSDLS(10),LINDLS(6)
      real*8 CIRTOL(5),CIRCDV(2),PLNCDV(4),PLNDWL,POLCDV(2),UNTCDV(2),
     1       REWCDV,TAPLEN,TURDIS,TLCTIM,TLCVL(20),TLOVL(5),TLOBAS,
     2       LSTTN,TLOFRG,CTOFRG,TSELTM,TRZX(2),MAXTN,SPIVEC(3),
     3       RETDIS,RETFED(4),CUTCVR(8),CUTCVL(30),SLWVR(5),SLWVL(15),
     4       CYCCDV(25),CYCVR(5),CYCPSV(10),ISEQSV,HOME(10),CLRPLN(4)
      real*8 DELYVL,DELYTM,HOMCDV(5),CYCJVC(3),CYCPLN(4),CYLCDV(25),
     1       CYLVR(5),PSTNCV(5),BRKVR(10),RETPOS(10),INFZON(2,10,4),
     2       RSIZCV,CYPCDV(3),FEDLEN,BLDVEC(3),BLDELT,ZIGDEP,PODAVL(7),
     3       PODSVL(6),REFMAT(12),TRAMAT(12),RETFSV,ALNDIS(3),ALNMPS(3),
     4       SMORAN,SMODIS,SMOCVL(2),SMONVL(3),SMOOVL(3),CUTOFS(4,3)
      real*8 CFRCDV(2),CDRCDV(2),FLMCDV,DLMCDV,GLMCDV(2),TLMCDV,TLLCDV,
     1       FRAPVL,DRAPVL,VTOLER,BSPCDV,BSPTOL(2),HELCDV(2),CIRTOS,
     2       FDLCNV,ROTDVL(2,4),MILTOL,ACCFED,MAXVEL,AXSVEL,ACCSTP(2),
     3       C3XCDV(2),MACCDV(5),XFMVL(5),NMLVEC(3),NMLSAV(3),RTSHFD,
     4       LNRATL(5),FRTVAL(2),LNRTOL(2),RETPL(4),TAXTOL,ACLDCL(10)
      real*8 CURSPD(10),SGMSTO(6,3),SGMHOM(6,3),SGMLMT(2,6,3),SGMOFS(2),
     1       SGMCDV(10),SGMTL(2)
c
      integer*2 ifmt(10,MAXFMT)
      integer*4 i,j,imbrg(7),itmrg(8),ifcd(8),ic3ax(10),
     1          ifmcd(8),itlc(12),itloc(8),
     2          cucf(20),cucd(30),cudo(10),hmcd(4),hmrg(10),
     4          clfl(20),clcd(25),clrg(20),cltm(14),pncd(5),pnrg(15)
      integer*4 iord(MAXFMT),imotr(24),ipds(2,10),cicd(6),swfl(4),
     1          swcd(15),cmfl(30),cmcd(15),cmrg(15),cmtm(14),bko(5),
     2          itlov(5),itlv(12),mcd(5),xffl(20),sgreg(6,3),sgcod(10)
c
      real*8 igrg(14,11),imrg(7,11),sptc(135),sptv(135),fmvl(8),
     1       fdtc(90),fdtv(90),frg(2,8),flmt(10),ctol(5),cucr(5),
     2       cucv(30),swvl(15),cmcv(15),clpl(4),hmvl(4),clcv(25),
     3       pncv(5),mcdv(5),sgcdv(10)
c
      character*10 lreg(MAXFMT)
c
      data bko /0,1,1,1,2/
      data cicd /-1,-1,30,32,34,52/
      data ic3ax /2,2,10,10,31,33,35,0,1,0/
      data cmcd /-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0/
      data cmfl /1,2,1,1,1,1,1,1,1,2,2,1,1,1,2,2,1,1,2,1,1,1,2,2,1,
     1          1,1,2,2,2/
      data cmrg /0,52,14,50,0,35,0,51,31,33,35,60,0,36,0/
      data cmtm /1,2,3,4,5,6,7,8,9,10,11,12,13,0/
c
      data imotr /61,62,0,0, 63,64,0,0, 65,66,0,0, 4,5,6, 1,2,3, 0,0,0,
     1            0,0,0/
c
      data iord /48,49,18,19,20,21,22,23,24,25,26,27,61,62,63,64,65,66,
     1           55,57,59,02,03,05,06,30,31,32,33,34,35,15,16,17,52,13,
     2           29,53,54,37,38,39,40,41,42,01,04,07,08,09,10,11,12,14,
     3           28,36,43,44,45,46,47,50,51,56,58,60,67,68,69,70,71,72,
     4           73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,
     5           91,92/
c
      data ipds /1,0, 0,0, 2,0, 3,0, 4,0, 5,0, 5,0, 5,0, 6,7, 8,0/
c
      data swfl /1,2,2,1/
      data swcd /-1,0,0,0, -1,-1,0,0,0,0,0,0, -1,0,0/
      data clcd /-1, 0, 0, 0, 0,-1, 0, 0,-1, 0, 0, 0,-1,-1, 0,-1,-1, 0,
     1           0,0,0,0,0,-2,-2/
      data clfl /1,1,1,1,2,2,1,1,2,2,1,0, 0,0,0,0,0,0,0,1/
      data clrg /17,0,0,0,0,0,35,31,0,0,0,0,52,35,31,13,0,0,0,0/
      data cltm /1,1,2,2,3,3,4,4,5,5,6,6,7,7/
      data cucf /0,1,2,2,2,1,1,2,2,1,2,2,2,1,0,2,2, 0,0,0/
      data cucd /-1,-1,-1,13,-1,50,51,52,50,14,14,-1,-1,-1,50,29,0,0,-1,
     1           -1,55,57,59,-1,52,67,68,69, 0,0/
      data cudo /0,0,1,1,1,0,0,0,0,0/
c
      data hmcd /-1,-1,-1,-1/
      data hmrg /62,56,64,58,66,60,3,6,9,12/
c
      data ifcd /15,0,0,16,0,0,0,0/
c
      data ifmcd /-1,0,0,-1,0,0,0,0/
c
      data ifmt /3,0,1,4,4,3,3,0,1,3,  3,0,1,4,4,3,3,0,1,3,
     1           3,0,1,4,4,3,3,0,1,4,  3,0,1,4,4,3,3,0,1,3,
     2           3,0,1,4,4,3,3,0,1,3,  3,0,1,4,4,3,3,0,1,4,
     3           4,0,1,5,5,0,0,1,0,0,  3,0,1,4,5,4,3,0,4,0,
     4           3,0,1,4,4,3,3,0,3,0,  3,0,1,4,4,1,1,0,1,0,
     5           3,0,1,4,4,2,2,0,2,0,  3,0,1,4,4,3,3,0,1,0,
     6           4,0,0,2,2,0,0,2,0,2,  3,0,1,3,3,6,6,0,1,0,
     7           3,0,0,3,4,1,0,0,1,3,  3,0,0,2,2,3,3,0,1,2,
     8           3,0,0,2,2,2,2,0,1,3,  4,0,0,2,2,0,0,2,0,2,
     9           4,0,0,2,2,0,0,2,0,3,  4,0,0,2,2,0,0,2,0,3,
     *           4,0,0,2,2,0,0,2,0,3,  4,0,0,2,2,0,0,2,0,3,
     1           4,0,0,2,2,0,0,2,0,3,  4,0,0,2,2,0,0,2,0,3,
     2           4,0,0,2,2,0,0,2,0,3,  4,0,0,2,2,0,0,2,0,3,
     3           4,0,0,2,2,0,0,2,0,3,  4,0,0,2,2,0,0,2,0,0,
     4           4,0,0,2,2,0,0,2,0,2,  3,0,1,3,4,4,3,0,1,2,
     5           3,0,1,3,4,4,3,0,1,2,  3,0,1,3,4,4,3,0,1,2,
     6           3,0,1,3,4,4,3,0,1,2,  3,0,1,3,4,4,3,0,1,2,
     7           3,0,1,3,4,4,3,0,1,2,  4,0,0,4,4,0,0,1,0,0,
     8           4,0,0,2,2,0,0,2,0,2,  4,0,0,2,2,0,0,2,0,3,
     9           4,0,0,2,2,0,0,2,0,3,  4,0,0,2,2,0,0,2,0,3,
     *           4,0,0,2,2,0,0,2,0,3,  4,0,0,2,2,0,0,2,0,3,
     1           4,0,0,2,2,0,0,2,0,0,  4,0,0,2,2,0,0,2,0,0,
     2           4,0,0,2,2,0,0,2,0,0,  4,0,0,2,2,0,0,2,0,0,
     3           4,0,0,2,2,0,0,2,0,0,
     4           4,0,0,6,6,0,0,1,0,3,  3,0,0,0,0,0,0,0,0,2,
     5           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     6           3,0,1,3,4,4,3,0,1,2,  4,0,0,4,4,0,0,1,0,2,
     7           4,0,0,8,8,0,0,1,0,2,  3,0,1,3,4,4,3,0,1,2,
     8           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,2,
     9           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,2,
     *           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,3,
     1           3,0,1,3,4,4,3,0,1,4,  3,0,1,3,4,4,3,0,1,3,
     2           3,0,1,3,4,4,3,0,1,4,  3,0,1,3,4,4,3,0,1,3,
     3           3,0,1,3,4,4,3,0,1,4,
     4           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     5           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     6           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     7           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     8           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     9           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     *           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     1           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     2           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     3           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     4           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     5           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0,
     6           3,0,1,3,4,4,3,0,1,0,  3,0,1,3,4,4,3,0,1,0/
c
      data imbrg /19,-4,-6,-8,-10,-11,-14/
      data itmrg /-1,-1,61,63,65,15,53,38/
c
      data itlc /0,0,54,54,-2,-2,0,0,0,0,-2,-2/
c
      data itloc /29,0,-1,-1,-1,55,57,59/
c
      data pncd /-1,0,0, 0,0/

      data pnrg /61,55,63,57,65,59,2,5,8,11, 49,0, 0,0,0/
c
      data clpl /0.,0.,1.,0./
      data clcv /33, 0, 0, 0, 0,75, 0, 0,94, 0, 0, 0,92,76, 0,74,90, 0,
     2           0,0,0,0,0,23,24/
      data cmcv /80,86,87,88,89,83,81,82,87,85,74,76,84,87,0/
      data ctol /.002,.100,.0001,419.4303,.002/
      data cucr /180.d0, 3.2767d0, 0., 0., 0./
      data cucv /41.,42.,40.,0.,39,0,0,0,0,2,3,45,46,47,0,0,0,0,45,46,0,
     1           0,0,141,0,0,0,0, 0,0/
c
      data fdtc /01,02,03,04,05,06,07,08,09,11,12,13,14,15,16,
     1           17,18,19,21,22,23,24,25,26,27,28,29,31,32,33,
     2           34,35,36,37,38,39,00,00,00,00,00,00,00,00,00,
     3           01,02,03,04,05,06,21,22,23,24,25,26,00,00,00,
     4           00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
     5           00,00,00,00,00,00,00,00,00,00,00,00,00,00,00/
c
      data fdtv / .05, .06, .07, .10, .13, .18, .23, .29, .36, .45, .63,
     1            .83, 1.0, 1.2, 1.4, 1.7, 2.1, 2.6, 3.2, 3.8, 4.8, 6.1,
     2            7.5, 9.5,10.5,13.2,17.4, 21., 25., 30., 37., 45., 55.,
     3            67., 80.,100.,0,0,0,0,0,0,0,0,0,
     4           .002,.003,.005,.007,.009,.014,.021,.028,.041,.062,.083,
     5           .125,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     6           0,0,0,0,0,0,0,0/
c
      data flmt /400., 400., 400., 400., 200., 200., 240., 240., 240.,
     1           240./
c
      data frg /.1,999.9, 0.,0., 0.,0., .001,99.999, 0.,0., 0.,0.,
     1          0.,0., 0.,0./
c
      data hmvl /27.,28.,29.,30./
c
      data itlov / 0,0,49,43,44/
      data itlv /0,0, 0,0,  6, 0,0,0,0,0,17,18/
c
      data fmvl /94.,0.,0.,93.,0.,0.,0.,0./
c
      data igrg /04,10,11,12,92,98,99,-1,-1,-1,-1,-1,-1,-1,
     1           00,01,02,03,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     2           17,18,19,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     3           40,41,42,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     4           60,61,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     5           70,71,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     6           80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,
     7           90,91,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     8           93,94,95,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     9           96,97,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     *           -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
c
      data imrg /00,01,02,06,30,-1,-1,
     1           03,04,05,-1,-1,-1,-1,
     2           07,08,09,-1,-1,-1,-1,
     3           13,14,17,18,-1,-1,-1,
     4           40,41,-1,-1,-1,-1,-1,
     5           48,49,-1,-1,-1,-1,-1,
     6           -1,-1,-1,-1,-1,-1,-1,
     7           -1,-1,-1,-1,-1,-1,-1,
     8           -1,-1,-1,-1,-1,-1,-1,
     9           -1,-1,-1,-1,-1,-1,-1,
     *           -1,-1,-1,-1,-1,-1,-1/
c
      data mcd /47,47,47,47,47/
      data mcdv /0,0,0,0,0/
c
      data xffl /2,1,2,0,0,2,2,1,2,2,2,2,2,1,1,1,2,0,0,0/
c
      data pncv /92,0,0, 0,0/
c
      data sptc /00,01,02,03,04,05,06,07,10,11,12,13,14,15,16,
     1           17,20,21,22,23,24,25,26,27,30,31,32,33,34,35,
     2           36,37,00,00,00,00,00,00,00,00,00,00,00,00,00,
     3           01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,
     4           16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
     5           31,32,33,34,35,36,37,38,39,40,41,00,00,00,00,
     6           40,41,42,43,44,45,46,47,50,51,52,53,54,55,56,
     7           57,60,61,62,63,64,65,66,67,70,71,72,73,74,75,
     8           76,77,00,00,00,00,00,00,00,00,00,00,00,00,00/
c
      data sptv /22.4,  25,  28,31.5,35.5,  40,  45,  50,  56,  63,  71,
     1             80,  90, 100, 112, 125, 140, 160, 180, 200, 224, 250,
     2            280, 315, 355, 400, 450 ,500, 560, 630, 710, 800,   0,
     3            0,0,0,0,0,0,0,0,0,0,0,0,
     4             40,  45,  50,  57,  64,  72,  81,  91, 103, 115, 130,
     5            146, 164, 185, 208, 234, 263, 296, 333, 375, 421, 475,
     6            534, 600, 675, 760, 855, 962,1030,1210,1362,1532,1724,
     7           1940,2180,2452,2757,3104,3492,4000,4300,0,0,0,0,
     8             90, 100, 112, 125, 140, 160, 180, 200, 224, 250, 280,
     9            315, 355, 400, 450, 500, 560, 630, 710, 800, 900,1000,
     *           1120,1250,1400,1600,1800,2000,2240,2500,2800,3150,   0,
     1            0,0,0,0,0,0,0,0,0,0,0,0/
c
      data swvl /8,0,0,0, 8,8,0,0,0,0,0,0, 9,0,0/
c
      data lreg /'A','A','A','B','B','B','C','C','C','C','C','C','D',
     1           'E','F','F','F','G','G','G','G','G','G','G','G','G',
     2           'G','G','H','I','I','J','J','K','K','L','M','M','M',
     3           'M','M','M','M','M','M','M','M','N',':','P','Q','R',
     4           'S','T','U','U','V','V','W','W','X','X','Y','Y','Z',
     5           'Z',
     6           'AA','AB','AC','AD','AE','AF','AG','AH','AI','AJ','AK',
     7           'AL','AM','AN','AO','AP','AQ','AR','AS','AT','AU','AV',
     8           'AW','AX','AY','AZ'/
c
      data sgreg /61,63,65, 67,68,69, 30,32,34, 0,0,0, 55,57,59, 0,0,0/
      data sgcod /83,-2,-2,-2,-2,-2,-2, 0,0,0/
      data sgcdv /7.2465d0,45,46,10,11,38,35, 0,0,0/
c
c...Initialize the dummy variables
c
      NKPOSM = 3
      NCPOSM = 2
      NPOSMA = 6
      IDUMMY = -9912345
      DUMMY  = -9912345.54321D0
c
c...Initialize Register Formats
c
      NRGORD = 45
      do 100 i=1,MAXFMT,1
          IREGVL(i) = 0
          IREGST(i) = 0
          REGFRC(i) = 0
          REGORD(i) = iord(i)
          REGSW(i) = 0
  100 continue
c
c...Initialize User defined blocks
c
      AL1BLK = 2
      AL2BLK = 2
      AL3BLK = 0
c
c...Initialize DIM(3) arrays
c
      do 150 i=1,3,1
          ACTLIN(i) = 1
          LNCALC(i) = 1
          NTAXCD(i) = 0
          NUMLIN(i) = 1
          PLNCOD(i) = -1
          ACTBLN(i) = 0
  150 continue
      PLNCOD(4) = 0
      ICMBSW = 0
      ICSPRE = 0
c
c...Initialize DIM(4) arrays
c
      IRTMAX = 4
      do 200 i=1,4,1
          IACFLG(i) = 1
          IRTCLW(i) = 1
          IRTDUP(i) = 1
          IRTDAC(1,i) = 1
          do 180 j=2,9,1
              IRTDAC(j,i) = 0
  180     continue
          IRTMOD(i) = 1
          IRTOUT(i) = 0
          IRTSCL(i) = 1
          ROTDCD(1,i) = 0
          ROTDCD(2,i) = 0
          ROTDVL(1,i) = 0.
          ROTDVL(2,i) = 0.
          RTRMCD(i) = 0
          RTRMVL(i) = 0.
          RTRMMX(i) = 99999999.0d0
          SLWFL(i) = swfl(i)
          LFIRCD(i+2) = cicd(i+2)
          LDIRCD(i+2) = cicd(i+2)
  200 continue
c
c...Initialize DIM(5) arrays
c
      do 220 i=1,5,1
          BRKOP(i) = bko(i)
          RSIZCD(i) = 0
          MACCOD(i) = mcd(i)
          MACCDV(i) = mcdv(i)
          XFMCD(i) = 0
          XFMVL(i) = 0
  220 continue
c
c...Initialize DIM(6) arrays
c
      do 250 i=1,6,1
          CIRCOD(i) = cicd(i)
          PODACD(i) = -2
          PODSCD(i) = -2
  250 continue
      PODACD(7) = -2
      PODSCD(1) = -1
c
c...Initialize DIM(10) arrays
c
      do 280 i=1,10,1
          AXSINC(i) = 1
          AXSPRI(i) = i
          CLMPCD(1,i) = 0
          CLMPCD(2,i) = 0
          CLMPUB(1,i) = 0
          CLMPUB(2,i) = 0
          IC3AXF(i) = ic3ax(i)
          ICLMPF(i) = 1
          IPRDES(1,i) = ipds(1,i)
          IPRDES(2,i) = ipds(2,i)
          XFMREG(i) = 0
          ACLDCL(i) = 0.
          CURSPD(i) = 0.
  280 continue
c
c...Initialize DIM(12) arrays
c
      do 300 i=1,12,1
          LOOKFL(i) = 0
          REFMAT(i) = 0.0
          TRAMAT(i) = 0.0
  300 continue
c
c...Initialize DIM(14) arrays
c
      do 380 i=1,14,1
          ICYTIM(i) = cmtm(i)
 380  continue
c
c...Initialize DIM(15) arrays
c
      do 400 i=1,15,1
          CYCCOD(i) = cmcd(i)
          CYCREG(i) = cmrg(i)
          SLWCD(i) = swcd(i)
  400 continue
      CYCCOD(19) = -1
      CYCCOD(20) = -1
c
c...Initialize DIM(20) arrays
c
      do 420 i=1,20,1
          ICY2FL(i) = 1
          XFMFL(i) = xffl(i)
  420 continue
      ICY2FL(3) = 2
      ICY2FL(4) = 5
      IXFMFL(1) = 2
      IXFMFL(2) = 2
c
c...Initialize DIM(24) arrays
c
      do 450 i=1,24,1
          MOTREG(i) = imotr(i)
  450 continue
c
c...Initialize DIM(30) arrays
c
      do 500 i=1,30,1
          ICYCFL(i) = cmfl(i)
          MACPSW(i) = 0
  500 continue
c
c...Initialize KPOSMP variables
c
      ABSCOD = -1
      ACHEAD = 3
      ALNCOD = 49
      AXSSPT = 0
c
      BSPLSW = 2
      BSPLFL = 2
      BSPCDV = DUMMY
      BSPPCD = 50
      BSPFRC(1) = 1
      BSPFRC(2) = 1
      BSPFRC(3) = 2
      BSPFRC(4) = 2
      BSPTOL(1) = .001
      BSPTOL(2) = .01
c
      C3XCDV(1) = 0
      C3XCDV(2) = 0
      CLMPBK = 1
c
      FINCOD = -2
      FRMFLG = 2
c
      HLDBCK = 1
      HLDFLG = 0
c
      IACCFD = 0
      IACCFL(1) = 2
      IACCFL(2) = 2
      IACLFL = 2
      ICIDLT = 2
      ICIRFL = 1
      ICIRSW = 1
      ICLMSV(1) = -1
      ICLSMP = 0
      ICRADJ = 1
      ICRBRK = 3
      ICRDIM = 1
      ICRDIR = 2
      ICRFMT = 1
      ICRHEL(1) = 1
      ICRHEL(2) = 1
      ICRHEL(3) = 0
      ICRHEL(4) = CIRCOD(1)
      ICRHEL(5) = CIRCOD(2)
      ICRHEL(6) = 0
      ICRIJP = 2
      ICRPFL = 1
      ICRPLF = 1
      ICRPLS(1) = 1
      ICRPLS(2) = 1
      ICRPLS(3) = 1
      ICRSGN = 1
      ICRXYZ = 1
      ICSMFL = 2
      ICSMRG = 14
      ICYCDO(1) = -1
      ICYCSW(1) = 0
      ICYCSW(3) = 1
      ICYCSW(4) = 0
      ICYDLT = 2
      IDSPLY = 2
      IFDADJ = 1
      IFIRST = 1
      IHELIX = 0
      IMXDRP = 1
      INCCOD = -1
      INCR   = 1
      INCRFC = 1
      INMDEF = 0
      INMLFL = 0
      INMSFL = 0
      INSSEQ = 1
      INSEOB = 1
      IOPSKP = 2
      NOPSON = 0
      IPACKD = 2
      IPAGE  = 0
      IPARTO = 1
      IPCHPN = 0
      IPCHSQ = 0
      IPLINE = 20000
      IPLNDF = 2
      IPOLIN = 0
      IPRTOP = 1
      IPVCTR = 0
      IRETFL = 0
      IRETMD(1) = 1
      IRETMD(2) = 4
      IRPRET = 2
      IRPSMD = 2
      IRTCLM = 1
      IRTFRC = 0
      IRTNUM = 2
      NCONTB = 1
      IRTRTE(1) = 1
      IRTRTE(2) = 1
      IRSRTE(3) = 0
      IRTWRK(1) = 2
      IRTWRK(2) = 1
      IRTYPE(1) = 2
      IRTYPE(2) = 2
      IRTSHF = 1
      IRWEOB = 1
      ISACIR = 0
      ISCIRC = 0
      ISBSPL = 0
      ISEQBK = 99999999
      ISQBKV = 99999999
      ISEQSW = 1
      ISLWDN = 2
      ISLWFD = 0
      IUNIT  = 1
      IZSWIV = 2
      KERRSV = 0
c
      LIMERR = 0
      MDTLIN = 0
      MODROT = 0
      LRTRCT = 1
      LRTRFL = 2
      LRTTAD = 1
      LTHDIA(1) = 1
      LTHDIA(2) = 2
      LTHXY  = 1
      LTHXD  = 2
      LTHPCL(1) = 1
      LTHPCL(2) = 1
      LCRFMT = 2
      LFDLCD = 36
      LFIRCD(1) = 28
      LFIRCD(2) = 28
      LDIRCD(1) = 28
      LDIRCD(2) = 28
      MILREG = -2
      TLMCDV = 19.
      LTHREG = -2
      TLLCDV = 20.
      LPCREG(1) = -1
      LPCREG(2) = -1
      GLMCDV(1) = 79.
      GLMCDV(2) = 79.
      LSTREG = 0
      LTCREG(1) = 4
      LTCREG(2) = 5
      LTCREG(3) = 6
      LTPREG(1) = 63
      LTPREG(2) = 64
      IRFSUP = 1
      IRDSUP = 2
      FRAPCD = 28
      DRAPCD = 28
      FRAPVL = 10.
      DRAPVL = 20.
      MODCYL = 0
      MCYNUM = 1
      LUNREG = 0
      ISRCYL = 2
c
      MACHTP = 1
      MACPRG = -1
      MTPDYN = 1
      MAXAXS = 10
      MCHOPT(1) = 1
      MCHOPT(2) = 1
      MCHOPT(3) = 0
      MCHOPT(4) = IDUMMY
      MCHOPT(5) = 72
      MCHOPT(6) = 1
      MCHOPT(7) = 0
      MCHOPT(8) = 0
      MCHPLN = 3
      MGCD   = 1
      MOTCOD = -1
      MLNFRC = 2
      BSPCOD = 12
      LFLMCD = 28
      LDLMCD = 28
      MACSPC = 2
      MOTFLG = 0
      MOT1ST = 1
      MROTTV = 1
      MRTFRC = 2
      MSGALN = 0
      MSGSEQ = 1
      MSGEOB = 1
      MSGTAB = 2
      MULTAX = 0
      MULTGC = 1
      MULTMC = 1
      MXCKSM = 999
      MXFLAG = 0
      MXTRAN = 0
      MXGTOL = 0
c
      NBOT   = 0
      NCUT   = 0
      NCUTDS = 0
      NOPIVS = 1
      NOTABS = 1
      NOUTSW = 0
      NPT    = 3
      NROT   = 2
      NUMAER = 0
      NUMERR = 0
      NUMFAT = 0
      NUMINF = 0
      NUMWRN = 0
c
      OPSSEQ = 1
      OPSTCD = -2
c
      PCHBNC = 0
      PCHSPC = 2
      PODCCD(1) = 0
      PODCCD(2) = 0
      POLCOD(1) = -1
      POLCOD(2) = -1
      POLCOD(3) = 7
      PPRIPV = 2
      PRTBNC = 0
c
      REWCOD = -2
      RSIZFL = 3
      RTSHFD = 0.
c
      SEQCOD = 48
      SEQFRQ = 1
      SIMACT = 0
      SPITAX = 1
      STOPCD = -2
      STPSEQ = 1
c
      TABSEQ = 1
c
      UNTCOD(1) = -1
      UNTCOD(2) = -1
c
      ALNMOV = 0
      ALNMOD = 1
      MOTEXP = 0
      IFWSFL = 0
      IFWDFL = 0
      ALNDIS(1) = .059055119
      ALNDIS(2) = 4. * ALNDIS(1)
      ALNDIS(3) = 1.0
      ALNCUT = 0
      ALNMPS(1) = .984251969
      ALNMPS(2) = 196.8503938
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
      SMOOVL(2) = 0.
      SMONVL(3) = SMORAN
      SMOOVL(2) = 0.
      SMODIS = .00393700788
c
c...Initialize CPOSMP variables
c
      NEOB   = 1
      NEOT   = 0
      NOPSKP = 1
      NRWSTP = 1
c
      NCBMSG = 5
      NCEMSG = 1
c
c...Initialize Register Formats
c
      do 600 i=1,MAXFMT,1
          if (i .gt. 66) then
              REGBNC(i) = 2
          else
              REGBNC(i) = 1
          endif
          REGENC(i) = 0
          do 80 j=1,10,1
              FMTDES(j,i) = ifmt(j,i)
   80     continue
  600 continue
c
c...Initialize User defined blocks
c
      MUSRBK = 20
      NUSRBK = 0
      DO 630 i=20,MUSRBK,1
          NCUSRB(i) = 0
  630 continue
      NCUSRB(1) = 7
      do 650 i=1,7,1
          USRBRG(i,1) = imbrg(i)
  650 continue
c
      NCUSRB(2) = 8
      do 660 i=1,8,1
          USRBRG(i,2) = itmrg(i)
  660 continue
c
      MOBLK  = 1
      NMOBLK = 0
      STBLK  = 0
c
c...Initialize DIM(3) arrays
c
      do 750 i=1,3,1
          IFDFRC(i) = 1
          SPNFCD(i) = 0
          SPNRCD(i) = 0
          TLDIG(1,i) = 0
          TLDIG(2,i) = 0
          PLCYCD(i) = 0
  750 continue
      IFDFRC(4) = 2
c
c...Initialize DIM(4) arrays
c
      do 760 i=1,4,1
          EXCLNE(1,i) = IDUMMY
          EXCLNM(i) = 0
          EXCLPS(1,i) = IDUMMY
          HOMCOD(i) = hmcd(i)
          IFDEXD(i) = 2
          IFDOUT(i) = 1
          ISCWRK(1,i) = 0
          ISCWRK(2,i) = 0
          SPCOCD(i) = -2
          SPNDCD(i) = -2
  760 continue
c
c...Initialize DIM(5) arrays
c
      do 820 i=1,5,1
          PSTNCD(i) = pncd(i)
  820 continue
c
c...Initialize DIM(8) arrays
c
      do 870 i=1,8,1
          FEDCD(i) = ifcd(i)
          FEDMCD(i) = ifmcd(i)
          TLOCD(i) = itloc(i)
  870 continue
c
c...Initialize DIM(10) arrays
c
      do 880 i=1,10,1
          HOMREG(i) = hmrg(i)
          ICUTDO(i) = cudo(i)
  880 continue
c
c...Initialize DIM(12) arrays
c
      do 900 i=1,12,1
          TLCCD(i) = itlc(i)
  900 continue
c
c...Initialize DIM(14) arrays
c
      do 980 i=1,14,1
          ICYBLK(i) = 0
          ICYLTM(i) = cltm(i)
 980  continue
c
c...Initialize DIM(15) arrays
c
      do 1000 i=1,15,1
          PSTNRG(i) = pnrg(i)
 1000 continue
c
c...Initialize DIM(20) arrays
c
      do 1300 i=1,20,1
          CUTCFL(i) = cucf(i)
          CYLREG(i) = clrg(i)
          ICYLBK(i) = 0
          ICYLFL(i) = clfl(i)
          IRDEAD(i) = -1
          TOOLFL(i) = 2
 1300 continue
      IRDEAD(1) = 0
      IRDEAD(2) = 0
      TOOLFL(10) = 1
c
c...Initialize DIM(30) arrays
c
      do 1500 i=1,30,1
          CUTCCD(i) = cucd(i)
 1500 continue
c
c...Initialize DIM(25) arrays
c
      do 1600 i=1,25,1
          CYLCOD(i) = clcd(i)
 1600 continue
c
c...Initialize DIM(90) arrays
c
      NFEDTB(1) = 36
      NFEDTB(2) = 12
c
c...Initialize DIM(135) arrays
c
      NSPNTB(1) = 32
      NSPNTB(2) = 41
      NSPNTB(3) = 32
c
c...Initialize PREPST commands
c
      LINTSW = 0
      ICLIPS = 0
      NPLANE = 0
      CIRTOS = .001
c
c...Initialize KPOSMP variables
c
      COLBLK(1) = 2
      COLBLK(2) = 1
      COOLCD(1) = -2
      COOLCD(2) = -2
      COOLCD(3) = 0
      COOLCD(4) = -2
c
      DELYCD(1) = -1
      DELYCD(2) = 50
      DELYCD(3) = 0
      DELYFL(1) = 1
      DELYFL(2) = 2
      DELYFL(3) = 2
      DELYFL(4) = 2
c
      FEDOCD(1) = -2
      FEDOCD(2) = -2
c
      ICOLSW = 2
      ICUCHG = 0
      IDPMOV = 2
      IFDCIR = 1
      IFDCTP = 2
      IFDLTP = 1
      IFDRAD = 2
      IFDRCN = 1
      IFDRCD(1) = 52
      IFDSPC(1) = 2
      IFDSUP(1) = 1
      IFDSUP(2) = 2
      IFDSUP(3) = 2
      IFDSUP(4) = 1
      IFDTYP(1) = 1
      IFDTYP(2) = 1
      IFDUNT = 2
      IFEDSW = 0
      IFITYP = 1
      IFOTYP = 1
      IPSTNF = 2
      IRAP   = 0
      IRAPSW = 0
      IZIGON = 0
      IZIGAD = 2
      LNCRAP = 2
      IROTFT = 4
      IJKROT = 2
      IJKREG(1) = 31
      IJKREG(2) = 33
      IJKREG(3) = 35
      IRPALT = 4
      IRPMOD = 1
      IRPSUP = 1
      IRPTAX = 2
      ITP    = 1
      ITSELF = 0
c
      LSTCOL = 2
      LSTGRP = 1
      LSTTSP = 1
c
      NFDSUP = 2
      NOTOOL = 1
      NRAPCD = 1
      NSHANK = 0
      NSPRG  = 1
      NUMEXC = 0
      NUSRST = 0
c
      OPKBLK = 1
      OPSBLK = 1
c
      POSFDF = 1
      POSFED = 1
      PSTNOF = 2
c
      RAPCD(1) = -1
c
      SPNBCD = 0
      SPNBLK = 0
      SPNDIR = 1
      SPNMOD = 1
      SPNOCD(1) = 0
      SPNOCD(2) = 0
      SPNOFF = 1
      SPNRNG = 4
      SPNSCD(1) = 53
      SPNSCD(2) = 0
      SPNSFM = 2
      SPNTYP = 2
      SPNXCD = 0
      STPBLK = 1
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
c...Initialize Register Formats
c
      do 2000 i=1,MAXFMT,1
          REGSTO(i) = 0.
          REGVAL(i) = 0.
 2000 continue
c
c...Initialize Grange & Mrange
c
      do 2100 i=1,11,1
          do 1180 j=1,14,1
              GRANGE(j,i) = igrg(j,i)
              if (GRANGE(j,i) .eq. -1) GRANGE(j,i) = DUMMY
 1180     continue
          do 1190 j=1,7,1
              MRANGE(j,i) = imrg(j,i)
              if (MRANGE(j,i) .eq. -1) MRANGE(j,i) = DUMMY
 1190     continue
 2100 continue
c
c...Initialize User defined blocks
c
      do 2350 i=1,7,1
          USRBVL(i,1) = DUMMY
 2350 continue
c
      do 2360 i=1,8,1
          USRBVL(i,2) = DUMMY
 2360 continue
      USRBVL(1,2) = 0
      USRBVL(2,2) = 94
c
c...Initialize DIM(3) arrays
c
      do 2550 i=1,3,1
          CUTSYM(i) = ' '
          CYCVR(i) = 0.
          ICUTYP(i) = 0
          LNDIST(i) = 0.
          REFMAT(i*5-4) = 1.0
          TRAMAT(i*5-4) = 1.0
 2550 continue
c
c...Initialize DIM(4) arrays
c
      do 2600 i=1,4,1
          CLRPLN(i) = clpl(i)
          FEDCNS(i) = 1.
          HOMCDV(i) = hmvl(i)
          ROTBAS(1) = 0.
          ROTCRM(i) = 1.
          ROTDLS(i) = 0.
          ROTDLT(i) = 0.
          ROTMAX(i) = -100000.
          ROTMIN(i) = 100000.
          do 2570 j=1,3,1
              CUTOFS(i,j) = 0.
              MCHDLS(j,i) = 0.
              MCHDLT(j,i) = 0.
              MCHMAX(j,i) = -100000.
              MCHMIN(j,i) = 100000.
              STONUM(j,i) = 0.
 2570     continue
 2600 continue
c....      TABORG(3,2) = 5.25
c
c...Initialize DIM(5) arrays
c
      do 2620 i=1,5,1
          CIRTOL(i) = ctol(i)
          CUTCVR(i) = cucr(i)
          PSTNCV(i) = pncv(i)
          TLOVL(i) = itlov(i)
 2620 continue
      TLOVL(1) = DUMMY
c
c...Initialize DIM(6) arrays
c
      do 2650 i=1,6,1
          CLSAV(i) = 0.
          LINDLS(i) = 0.
          LINDLT(i) = 0.
          LINMAX(i) = -100000.
          LINMIN(i) = 100000.
          LINSTO(i) = 0.
          TRANZ(i) = 0.
          TRANZ(i+6) = 1.
 2650 continue
c
c...Initialize DIM(7) arrays
c
      do 2680 i=1,7,1
          CUTTER(i) = 0.
 2680 continue
c
c...Initialize DIM(8) arrays
c
      do 2700 i=1,8,1
          FEDMVL(i) = fmvl(i)
          FEDRNG(1,i) = frg(1,i)
          FEDRNG(2,i) = frg(2,i)
 2700 continue
c
c...Initialize DIM(10) arrays
c
      do 2880 i=1,10,1
          AXSDLS(i) = 0.
          AXSDLT(i) = 0.
          AXSMAX(i) = -100000.
          AXSMIN(i) = 100000.
          AXSSTO(i) = 0.
          BRKVR(i) = 0.
          CLMPVL(1,i) = DUMMY
          CLMPVL(2,i) = DUMMY
          CYCPSV(i) = DUMMY
          FEDLMT(i) = flmt(i)
          HOME(i) = 0.
          do 2820 j=1,4,1
              INFZON(1,i,j) = DUMMY
              INFZON(2,i,j) = DUMMY
 2820     continue
          LIMITS(1,i) = -100000.
          LIMITS(2,i) = 100000.
          PPINCR(i) = 0.
          PPMAXD(i) = 99999999.
          PPTOLR(i) = .0001
          RAPLMT(i) = flmt(i)
          TRANAX(i) = 0.
          TRANAX(i+10) = 1.
 2880 continue
c
c...Initialize DIM(12) arrays
c
      do 2900 i=1,12,1
          TLCVL(i) = itlv(i)
 2900 continue
      TLCVL(3) = DUMMY
      TLCVL(4) = DUMMY
c
c...Initialize DIM(15) arrays
c
      do 3000 i=1,15,1
          CYCCDV(i) = cmcv(i)
          SLWVL(i) = swvl(i)
 3000 continue
      CYCCDV(19) = 98
      CYCCDV(20) = 99
c
      CYPCDV(1) = 17
      CYPCDV(2) = 18
      CYPCDV(3) = 19
c
c...Initialize DIM(20) arrays
c
      do 3050 i=1,20,1
          ROTSTO(i,1) = 0.
          ROTSTO(i,2) = 0.
          TABORG(1,i) = 0.
          TABORG(2,i) = 0.
          TABORG(3,i) = 0.
 3050 continue
      TABORG(3,2) = 5.25
c
c...Initialize DIM(30) arrays
c
      do 3100 i=1,30,1
          CUTCVL(i) = cucv(i)
 3100 continue
      CUTCVL(4) = DUMMY
c
c...Initialize DIM(25) arrays
c
      do 3200 i=1,25,1
          CYLCDV(i) = clcv(i)
 3200 continue
c
c...Initialize DIM(90) arrays
c
      do 3300 i=1,90,1
          FEDTCD(i) = fdtc(i)
          FEDTVL(i) = fdtv(i)
 3300 continue
c
c...Initialize DIM(120) arrays
c
      do 3400 i=1,120,1
          TL(i) = 0.
          TLTIM(i) = 0.
 3400 continue
c
c...Initialize DIM(135) arrays
c
      do 4000 i=1,135,1
          SPNTCD(i) = sptc(i)
          SPNTVL(i) = sptv(i)
 4000 continue
c
c...Initialize Stringer Drill Machine
c
      SGMACT(1) = 1
      SGMACT(2) = 0
      SGMOFS(1) = 25.629d0
      SGMOFS(2) = 5.4211d0
      do 4110 i=1,3,1
          do 4100 j=1,6,1
               SGMSTO(j,i) = 0.
               SGMHOM(j,i) = 0.
               SGMLMT(1,j,i) = -100000.
               SGMLMT(2,j,i) = 100000.
 4100     continue
 4110 continue
c
      do 4130 i=1,3,1
          do 4120 j=1,6,1
              SGMREG(j,i) = sgreg(j,i)
 4120     continue
 4130 continue
c
      do 4140 i=1,10,1
          SGMBLK(i) = 0
          SGMCOD(i) = sgcod(i)
          SGMHFL(i) = 0
          SGMCDV(i) = sgcdv(i)
 4140 continue
c
c...Initialize POSMAP variables
c
      ABSCDV = 90
      ACCFED = 0.
      ACCSTP(1) = 7.
      ACCSTP(2) = 25.
      AXSVEL = 8.
c
      CIRCDV(1) = 2.
      CIRCDV(2) = 3.
      HELCDV(1) = CIRCDV(1)
      HELCDV(2) = CIRCDV(2)
      CFRCDV(1) = 12.
      CFRCDV(2) = 13.
      CDRCDV(1) = 22.
      CDRCDV(2) = 23.
      COOLVL(1) = 7.
      COOLVL(2) = 8.
      COOLVL(4) = 9.
      CTOFRG = 0.
      CUTTIM = 0.
      CYCJVC(1) = 1.
      CYCJVC(2) = 0.
      CYCJVC(3) = 0.
      CYCPLN(4) = DUMMY
      CYLVR(1) = 0.
c
      DELYVL = 4.
      DELYTM = .5
      DPMBPW(1) = .0001
      DPMBPW(2) = .001
c
      FDLCNV = 10.
      FEDOVL(1) = 49.
      FEDOVL(2) = 48.
      FEDSPC(1) = 0.
      FRTVAL(1) = 1.
      FRTVAL(2) = 1.
      FTUNT  = 12.
c
      INCCDV = 91.
      ISEQ   = 1.
      ISEQSV = 1.
c
      LEDBEG = 36.
      LEDEND = 36.
c
      LIMITS(1,7) = -25.
      LIMITS(2,7) = 25.
      LIMITS(1,8) = -25.
      LIMITS(2,8) = 25.
      LNRTOL(1) = 0.
      LNRTOL(2) = 0.
      VTOLER = .0001
      MILTOL = .001
      LSTTN  = 0.
      LNRADJ = 1
      LNRATL(1) = .001
      LNRATL(2) = 1.
      LNRATL(3) = 30.
      LNRATL(4) = 3.
c
      MAXDPM = 99999999.
      MAXIPM = 99999999.
      MAXSFM = 4000.
      MAXTN  = 99999999.
      MAXVEL = 20.
      METCNV = 1.0
      MOTCDV = 1.
      FLMCDV = 11.
      DLMCDV = 21.
c
      NMLVEC(1) = 0.
      NMLVEC(2) = 0.
      NMLVEC(3) = 1.
      NMLSAV(1) = 0.
      NMLSAV(2) = 0.
      NMLSAV(3) = 1.
c
      OPSTVL = 1
      ORGIN(1) = 0.
      ORGIN(2) = 0.
      ORGIN(3) = 0.
c
      PFEED(1) = 10.
      PFEED(2) = .01
      PFEED(4) = 10.
      PI     = 3.141592653589793d0
      PLNCDV(1) = 17.
      PLNCDV(2) = 18.
      PLNCDV(3) = 19.
      PLNCDV(4) = 16.
      PLNDWL = 1.
      PODAVL(1) = 148.
      PODAVL(2) = 149.
      PODAVL(3) = 141.
      PODAVL(4) = 142.
      PODAVL(5) = 41.
      PODAVL(6) = 40.
      PODAVL(7) = 140.
      PODSVL(1) = 140.
      PODSVL(2) = 146.
      PODSVL(3) = 147.
      PODSVL(4) = 144.
      PODSVL(5) = 143.
      PODSVL(6) = 145.
      POLCDV(1) = 11.
      POLCDV(2) = 10.
c
      RAD    = 180.0d0 / PI
      RAPVL(1) = 0.
      RDRLMT = 0.
      RETDIS = 0.
      RETFED(1) = 0.
      RETFED(2) = 10.
      RETFED(3) = 0.
      RETFED(4) = 10.
      RETPL(1) = 0.
      RETPL(2) = 0.
      RETPL(3) = 1.
      RETPL(4) = 0.
      FEDLEN = 0.
      RETPOS(1) = DUMMY
      REWCDV = 30.
      FINCDV = 2.
      RETFSV = 0.
      ROTZER(1) = 0.
      ROTZER(2) = 360.
      RPM    = 0.
      RPMSAV = 0.
      RSIZCV = DUMMY
c
      SEQINC = 1.
      SEQMAX = 999999
      SEQTIM = 0.
      SFM    = 0.
      SFMSAV = 0.
      SFMTOL = 0.
      SLWVR(1) = 0.
      SLWVR(2) = .001
      SLWVR(3) = 7.00
      SLWVR(4) = 7.25
      SLWVR(5) = .020
      SPCOVL(1) = 13.
      SPCOVL(2) = 14.
      SPCOVL(3) = 17.
      SPIVEC(1) = 0.
      SPIVEC(2) = 0.
      SPIVEC(3) = 1.
      BLDVEC(1) = 1.
      BLDVEC(2) = 0.
      BLDVEC(3) = 0.
      BLDELT = 90.01
      SPCOVL(4) = 18.
      SPNDVL(1) = 3.
      SPNDVL(2) = 4.
      SPNDVL(3) = 5.
      SPNDVL(4) = 5.
      SPNLMT(1,1) = 20.
      SPNLMT(2,1) = 4000.
      SPNLMT(1,2) = 20.
      SPNLMT(2,2) = 4000.
      SPNLMT(1,3) = 20.
      SPNLMT(2,3) = 4000.
      SPNLMT(1,4) = .1
      SPNLMT(2,4) = 10000.
      SPNRAD = 0.
      STOPVL = 0.
c
      TAPLEN = 0.
      TAXTOL = 0.
      TIMSCL = 1.
      TLCTIM = 0.
      TLOBAS = 0.
      TLOFRG = 0.
      TRZX(1) = 0.
      TRZX(2) = 0.
      TSELTM = 0.
      TURDIS = 0.
c
      UNTCDV(1) = 70.
      UNTCDV(2) = 71.
      ZIGDEP = 0.d0
c
c...Initialize Register Formats
c
      do 4200 i=1,MAXFMT,1
          REGEN(i) = ' '
          REGST(i) = lreg(i)
 4200 continue
c
c...Initialize CPOSMP variables
c
      LEDCHR = ' '
      LEOB   = '$'
      LEOT   = ' '
      LMNAME = ' '
      LOPSKP = '/'
      LRWSTP = '='
      LSTBLK = ' '
      LUPDAT = REVDAT
c
      MDESC  = ' '
      MSGST  = '(MSG,'
      MSGEN  = ')'
c
      PDFFIL = '0'
      PHFILE = ' '
      PU1EXT = ' '
c
      TABCHR = '*'
c
      return
      end
