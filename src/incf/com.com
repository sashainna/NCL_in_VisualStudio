c*********************************************************************
c**
c**    NAME         :  com.com 
c**
c**    CONTAINS:
c**     the nclcom is the shared common region that all ncl subroutines
c**     have read/write access to.  
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**        com.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**        04/29/15 , 15:07:29
c*********************************************************************
C
      integer*2 MAX_PATH,MAX_LEN,MAX_LINE,MAX_FILE,N_INCLMP,N_KNCLMP,
     1          N_RNCLMP,N_GNCLMP
      integer*4 N_CNCLMP
c
      parameter (MAX_PATH = 1024)
      parameter (MAX_LEN = 1024)
      parameter (MAX_LINE = 50)
      parameter (MAX_FILE = 520)
      parameter (N_INCLMP = 800)
      parameter (N_KNCLMP = 300)
      parameter (N_RNCLMP = 50)
      parameter (N_GNCLMP = 1600)
      parameter (N_CNCLMP = 12000)
c
      common /inclmp/ INCLMP
      integer*2 INCLMP(N_INCLMP)
c
      equivalence (IFL   ,INCLMP(0001)), (INX   ,INCLMP(0501))
      equivalence (ITYP  ,INCLMP(0502)), (IST   ,INCLMP(0503))
      equivalence (LENGTH,INCLMP(0504)), (ISVINX,INCLMP(0505))
      equivalence (ISRCIX,INCLMP(0506)), (IW2INX,INCLMP(0507))
      equivalence (LLINE ,INCLMP(0508)), (LBLTBL,INCLMP(0509))
      equivalence (IDST  ,INCLMP(0510)), (IEOF  ,INCLMP(0511))
      equivalence (IAPTIX,INCLMP(0512)), (ISVTYP,INCLMP(0513))
      equivalence (MPAGE ,INCLMP(0514)), (PRLINE,INCLMP(0515))
      equivalence (NCSFL ,INCLMP(0516)), (NCLNAM,INCLMP(0526))
      equivalence (NUMIFL,INCLMP(0528)), (NUMSC ,INCLMP(0529))
      equivalence (NUMLFL,INCLMP(0530)), (NUMDTB,INCLMP(0531))
      equivalence (PLTLUN,INCLMP(0532)), (CINLUN,INCLMP(0533))
      equivalence (LUN125,INCLMP(0534)), (X0000 ,INCLMP(0535))
      equivalence (XFFFF ,INCLMP(0536)), (X00FF ,INCLMP(0537))
      equivalence (XFF00 ,INCLMP(0538)), (X0FF0 ,INCLMP(0539))
      equivalence (XF00F ,INCLMP(0540)), (XETX  ,INCLMP(0541))
      equivalence (SVMSG ,INCLMP(0542)), (ISCRLN,INCLMP(0543))
      equivalence (EDTEXE,INCLMP(0547)), (IRCTYP,INCLMP(0548))
      equivalence (NUMIF4,INCLMP(0549)), (LFL   ,INCLMP(0550))
      equivalence (IDSGCK,INCLMP(0750)), (ICSGCK,INCLMP(0751))
      equivalence (LDSSVC,INCLMP(0752)), (LCSSVC,INCLMP(0753))
      equivalence (SIMCOM,INCLMP(0754)), (IDTYPE,INCLMP(0755))
      equivalence (S116  ,INCLMP(0756)), (WDTYPE,INCLMP(0757))
      equivalence (WILDWD,INCLMP(0758)), (FILTWD,INCLMP(0759))
      equivalence (FILTGEO,INCLMP(0760)), (FILTYPE, INCLMP(0761))
c
      integer*2 IFL(500),INX,ITYP,IST,LENGTH,ISVINX,ISRCIX,IW2INX,LLINE,
     1          LBLTBL,IDST,IEOF,IAPTIX,ISVTYP,MPAGE,PRLINE,NCSFL(10),
     2          NCLNAM(2),NUMIFL,NUMSC,NUMLFL,NUMDTB,PLTLUN,CINLUN,
     3          LUN125,X0000,XFFFF,X00FF,XFF00,X0FF0,XF00F,XETX,SVMSG,
     4          ISCRLN(4),NUMIF4,IDSGCK,ICSGCK,IDTYPE,S116,WDTYPE,
     5          IRCTYP,FILTYPE
c
      logical*2 LFL(200),LDSSVC,LCSSVC, SIMCOM,WILDWD, EDTEXE,
     1          FILTWD, FILTGEO
c
      common /knclmp/ KNCLMP
      integer*4 KNCLMP(N_KNCLMP)
c
      equivalence (ITV   ,KNCLMP(0001)), (NREC  ,KNCLMP(0002))
      equivalence (ASLUN ,KNCLMP(0003)), (VOCLUN,KNCLMP(0004))
      equivalence (PPLUN ,KNCLMP(0005)), (CLLUN ,KNCLMP(0006))
      equivalence (QUELUN,KNCLMP(0007)), (SCRLUN,KNCLMP(0008))
      equivalence (PPSLUN,KNCLMP(0009)), (DBLUN ,KNCLMP(0010))
      equivalence (ERRLUN,KNCLMP(0011)), (HLPLUN,KNCLMP(0012))
      equivalence (RANLUN,KNCLMP(0013)), (PRTLUN,KNCLMP(0014))
      equivalence (CONLUN,KNCLMP(0015)), (SVLL  ,KNCLMP(0016))
      equivalence (NLINE ,KNCLMP(0017)), (PMODE ,KNCLMP(0018))
      equivalence (IFL4  ,KNCLMP(0019)), (SVLN  ,KNCLMP(0219))
      equivalence (NHLINE,KNCLMP(0220)), (NCLINE,KNCLMP(0221))
      equivalence (NCW2  ,KNCLMP(0222)), (ISREC ,KNCLMP(0223))
      equivalence (IVXSUB,KNCLMP(0226)), (ISVSUB,KNCLMP(0227))
      equivalence (TEMPLUN,KNCLMP(0228)), (GEOMNUM,KNCLMP(0229))
      equivalence (WILDNUM,KNCLMP(0230)), (NCCIMG,KNCLMP(0231))
      equivalence (NCCOUT,KNCLMP(0232)), (NCCIN ,KNCLMP(0233))
      equivalence (INX135,KNCLMP(0234)), (INX136,KNCLMP(0235))
      equivalence (INX137,KNCLMP(0236)), (INX138,KNCLMP(0237))
      equivalence (INX155,KNCLMP(0238)), (INX156,KNCLMP(0239))
      equivalence (INX159,KNCLMP(0240)), (INX182,KNCLMP(0241))
      equivalence (BLINE ,KNCLMP(0242)), (ISREC2,KNCLMP(0243))
      equivalence (ISREC3,KNCLMP(0245))
c
      integer*4 ITV,NREC,ASLUN,VOCLUN,PPLUN,CLLUN,QUELUN,SCRLUN,PPSLUN,
     1          DBLUN,ERRLUN,HLPLUN,RANLUN,PRTLUN,CONLUN,SVLL,NLINE,
     2          PMODE,IFL4(200),SVLN,NHLINE,NCLINE,ISREC(2),ISREC2(2),
     3          ISREC3(2),IVXSUB,ISVSUB,TEMPLUN,GEOMNUM,WILDNUM,NCCIMG,
     4          NCCOUT,NCCIN,NCW2,BLINE,INX135,INX136,INX137,INX138
      integer*4 INX155,INX156,INX159,INX182
c
      common /IJOB/ IJOB
      integer*4 IJOB
c
      common /RNCLMP/ RNCLMP
      real*4 RNCLMP(N_RNCLMP)
c
      equivalence (STIME ,RNCLMP(0001)), (RTIME ,RNCLMP(0002))
c
      real*4 STIME,RTIME
c
      common /GNCLMP/ GNCLMP
      real*8 GNCLMP(N_GNCLMP)
c
      equivalence (SC    ,GNCLMP(0001)), (TV    ,GNCLMP(0301))
      equivalence (REST  ,GNCLMP(0302)), (IJ    ,GNCLMP(0303))
      equivalence (JBR   ,GNCLMP(0304)), (DSSFVC,GNCLMP(0340))
      equivalence (CSSFVC,GNCLMP(0343))
c
      real*8 SC(300),TV,REST,IJ,JBR(36),DSSFVC(3),CSSFVC(3)
c
      common /CNCLMP/ CNCLMP
      character*1 CNCLMP(N_CNCLMP)
c
      equivalence (CIN   ,CNCLMP(0001)), (COUT  ,CNCLMP(1537))
      equivalence (CIMAGE,CNCLMP(2561)), (WILDCHR, CNCLMP(4585))
      equivalence (CURMAC,CNCLMP(5609)), (SC135 ,CNCLMP(5673))
      equivalence (SAVEID,CNCLMP(5753)), (CIR125,CNCLMP(5761))
      equivalence (SAVTYP,CNCLMP(5769)), (PPFNAM,CNCLMP(5777))
      equivalence (ASFNAM,CNCLMP(6297)), (CLFNAM,CNCLMP(6817))
      equivalence (DEFNAM,CNCLMP(7337)), (PLTBUF,CNCLMP(7857))
      equivalence (TOKEN2,CNCLMP(7993)), (SAVID2,CNCLMP(8057))
      equivalence (SVMACI,CNCLMP(8121)), (ERRCOM,CNCLMP(8185))
      equivalence (W2    ,CNCLMP(8265)), (TOKSTR,CNCLMP(9289))
      equivalence (SC136 ,CNCLMP(9809)), (SC137 ,CNCLMP(9873))
      equivalence (SC138 ,CNCLMP(9937)), (SC155 ,CNCLMP(10001))
      equivalence (SC156 ,CNCLMP(10065)), (SC159 ,CNCLMP(10129))
      equivalence (SC163 ,CNCLMP(10193)), (SC164 ,CNCLMP(10257))
      equivalence (SC165 ,CNCLMP(10321)), (SC182 ,CNCLMP(10385))
      equivalence (SC116 ,CNCLMP(10449)), (THRLBL,CNCLMP(10513))
c
      character*1 PLTBUF(136)
      character*8 SAVEID,CIR125,SAVTYP
      character*80 ERRCOM
      character*64 TOKEN2,SAVID2,SVMACI,WILDCHR,CURMAC,SC135,SC136,
     1             SC137,SC138,SC155,SC156,SC159,SC163,SC164,SC165,
     2             SC182,SC116,THRLBL
      character*(MAX_LEN) COUT,CIMAGE,W2
      character*1536 CIN
      character*(MAX_FILE) PPFNAM,ASFNAM,CLFNAM,DEFNAM,TOKSTR
c
      common/ptrcom/PTRS(32)
      integer*4 PTRS
      integer*4 NRCN(2),IMOTP(2)
      equivalence (NRCN,PTRS(1)),(IMOTP,PTRS(3))
c
c ***********************************************************************
c
c        variable type declarations
c
c **********************************************************************
c
      logical*2 aptcom,aptmark,autost,motdfl,ldebgm,geodsp,dtverf,
     1          lablpt,lablve,lablln,lablci,lablcv,lablsf,lablsh,lablpl,
     2          dsplmx,dsplpt,dsplve,dsplln,dsplci,dsplcv,ldspsf,dsplpl,
     3          ldspsh,oldout,ldterr,ginena,lmpu,letrng,autol1,geover,
     4          err,fromt,quit,srceof,ksr,echo,init,debug,genfst,runerr,
     5          svlbpt,svlbve,svlbln,svlbpl,svlbci,svlbcv,svlbsf,svlbsh
      logical*2 findss,lincld,ldsppn,lablpn,svlbpn,rpfron,ranpts,incflg,
     1          stpflg,strcmd,lskip,vocab,geom,scalar,nxteos,compos,
     2          lablpv,svlbpv,cntctp,cptsvd,psuv,dsuv,csuv,autouv,
     3          noclos,auvset,defwf,ldsppv,unklbl,lstep,lwrk,lcmm,
     4          lcpmac,lmlt,ldb501,sclat,sclout,sclats,sclous,rldinu,
     5          ldtflg,lcspt,lcntct,ltltct,ldschk,lcvgid,lmdnow,lttcrv
      logical*2 lttcsf,lblade,lablmx,svlbmx,lexpcl,psmult,cvoneps,
     1          tantods,lavoid,lavdd,lcsavd,lflwrn,lincfi,gfadbg,lautgf,
     2          csmult,domov,mocom,lsecps,lrmill,lflcom,lflsam,lcumov,
     3          onpaus,onstop,lexpr,lstrng,ldtext,ltcase,ltxtsb,
     4          lmintf(2),nowarn,lbldpt,lbldve,lbldln,lbldci,lbldcv,
     5          lbldsf,lbldsh,lbldpl,lbldpv,lbldmx,lbldpn,lablan,lbldan
      logical*2 svldpt,svldve,svldln,svldpl,svldci,svldcv,svldsf,svldsh,
     1          svldpv,svldmx,svldpn,pscalar,svlban,svldan,dsplan,
     2          lablsy,lbldsy,svlbsy,svldsy,dsplsy,rightp,dsplso,lablso,
     3          svlbso,lbldso,aptrem,lstepv,ipvcom,prplst

      equivalence (aptcom,LFL(001)), (autost,LFL(002))
      equivalence (motdfl,LFL(003)), (ldebgm,LFL(004))
      equivalence (geodsp,LFL(005)), (dtverf,LFL(006))
      equivalence (nowarn,LFL(007)), (lablpt,LFL(008))
      equivalence (lablve,LFL(009)), (lablln,LFL(010))
      equivalence (lablci,LFL(011)), (lablcv,LFL(012))
      equivalence (lablsf,LFL(013)), (lablsh,LFL(014))
      equivalence (dsplpt,LFL(015)), (dsplve,LFL(016))
      equivalence (dsplln,LFL(017)), (dsplci,LFL(018))
      equivalence (dsplcv,LFL(019)), (ldspsf,LFL(020))
      equivalence (ldspsh,LFL(021)), (oldout,LFL(022))
      equivalence (ldterr,LFL(023)), (ginena,LFL(024))
      equivalence (lmpu  ,LFL(025)), (letrng,LFL(026))
      equivalence (autol1,LFL(027)), (geover,LFL(028))
      equivalence (err   ,LFL(029)), (fromt ,LFL(030))
      equivalence (quit  ,LFL(031)), (srceof,LFL(032))
      equivalence (ksr   ,LFL(033)), (echo  ,LFL(034))
      equivalence (init  ,LFL(035)), (debug ,LFL(036))
      equivalence (genfst,LFL(037)), (lablpl,LFL(038))
      equivalence (dsplpl,LFL(039)), (runerr,LFL(040))
      equivalence (svlbpt,LFL(041)), (svlbve,LFL(042))
      equivalence (svlbln,LFL(043)), (svlbpl,LFL(044))
      equivalence (svlbci,LFL(045)), (svlbcv,LFL(046))
      equivalence (svlbsf,LFL(047)), (svlbsh,LFL(048))
      equivalence (findss,LFL(049)), (lincld,LFL(050))
      equivalence (ldsppn,LFL(051)), (lablpn,LFL(052))
      equivalence (svlbpn,LFL(053)), (rpfron,LFL(054))
      equivalence (ranpts,LFL(055)), (incflg,LFL(056))
      equivalence (stpflg,LFL(057)), (strcmd,LFL(058))
      equivalence (lskip ,LFL(059)), (vocab ,LFL(060))
      equivalence (geom  ,LFL(061)), (scalar,LFL(062))
      equivalence (nxteos,LFL(063)), (compos,LFL(064))
      equivalence (cntctp,LFL(065)), (cptsvd,LFL(066))
      equivalence (psuv  ,LFL(067)), (dsuv  ,LFL(068))
      equivalence (csuv  ,LFL(069)), (autouv,LFL(070))
      equivalence (noclos,LFL(071)), (auvset,LFL(072))
      equivalence (defwf ,LFL(073)), (ldsppv,LFL(074))
      equivalence (lablpv,LFL(075)), (svlbpv,LFL(076))
      equivalence (unklbl,LFL(077)), (lstep ,LFL(078))
      equivalence (lwrk  ,LFL(079)), (lcmm  ,LFL(080))
      equivalence (lcpmac,LFL(081)), (lmlt  ,LFL(082))
      equivalence (ldb501,LFL(083)), (sclat ,LFL(084))
      equivalence (sclout,LFL(085)), (sclats,LFL(086))
      equivalence (sclous,LFL(087)), (rldinu,LFL(088))
      equivalence (dsplmx,LFL(089)), (ldtflg,LFL(090))
      equivalence (lcspt ,LFL(091)), (lcntct,LFL(092))
      equivalence (ltltct,LFL(093)), (ldschk,LFL(094))
      equivalence (lcvgid,LFL(095)), (lmdnow,LFL(096))
      equivalence (lttcrv,LFL(097)), (lttcsf,LFL(098))
      equivalence (lblade,LFL(099)), (lablmx,LFL(100))
      equivalence (svlbmx,LFL(101)), (lexpcl,LFL(102))
      equivalence (psmult,LFL(103)), (cvoneps,LFL(104))
      equivalence (tantods,LFL(105)),(lavoid,LFL(106))
      equivalence (lavdd ,LFL(107)), (lcsavd,LFL(108))
      equivalence (lflwrn,LFL(109)), (lincfi,LFL(110))
      equivalence (gfadbg,LFL(111)), (lautgf,LFL(112))
      equivalence (csmult,LFL(113)), (domov ,LFL(114))
      equivalence (mocom ,LFL(115)), (lsecps,LFL(116))
      equivalence (lrmill,LFL(117)), (lflcom,LFL(118))
      equivalence (lflsam,LFL(119)), (lcumov,LFL(120))
      equivalence (onpaus,LFL(121)), (onstop,LFL(122))
      equivalence (lexpr ,LFL(123)), (lstrng,LFL(124))
      equivalence (ldtext,LFL(125)), (ltcase,LFL(126))
      equivalence (ltxtsb,LFL(127)), (lmintf,LFL(128))
      equivalence (aptmark,LFL(129)),(lbldpt,LFL(130))
      equivalence (lbldve,LFL(131)), (lbldln,LFL(132))
      equivalence (lbldci,LFL(133)), (lbldcv,LFL(134))
      equivalence (lbldsf,LFL(135)), (lbldsh,LFL(136))
      equivalence (lbldpl,LFL(137)), (lbldpv,LFL(138))
      equivalence (lbldmx,LFL(139)), (lbldpn,LFL(140))
      equivalence (svldpt,LFL(141)), (svldve,LFL(142))
      equivalence (svldln,LFL(143)), (svldpl,LFL(144))
      equivalence (svldci,LFL(145)), (svldcv,LFL(146))
      equivalence (svldsf,LFL(147)), (svldsh,LFL(148))
      equivalence (svldpv,LFL(149)), (svldmx,LFL(150))
      equivalence (svldpn,LFL(151)), (pscalar,LFL(152))
      equivalence (lablan,LFL(153)), (lbldan,LFL(154))
      equivalence (svlban,LFL(155)), (svldan,LFL(156))
      equivalence (dsplan,LFL(157)), (lablsy,LFL(158))
      equivalence (lbldsy,LFL(159)), (svlbsy,LFL(160))
      equivalence (svldsy,LFL(161)), (dsplsy,LFL(162))
      equivalence (rightp,LFL(163)), (dsplso,LFL(164))
      equivalence (lablso,LFL(165)), (svlbso,LFL(166))
      equivalence (lbldso,LFL(167)), (aptrem,LFL(168))
      equivalence (lstepv,LFL(169)), (ipvcom,LFL(170))
      equivalence (prplst,LFL(171))
c
      integer*2 jb(144)
      integer*4 jb4(72)
      logical leqbuf
      character*288 jbc  
      equivalence (jb,JBR,jb4,leqbuf), (leqbuf,jbc)
c
      integer*2 voc,geotyp
      equivalence (voc,IST), (geotyp,IST)
c
      integer*2 isc10(20)
      equivalence (SC(10),isc10)
c
      integer*2 nextyp
      equivalence (nextyp,IFL(43))
c
      character*1 ain(1536)
      equivalence (CIN,ain)
c
c...Geometry types
c
      integer*2 POINT,VECTOR,LINE,PLANE,CIRCLE,CURVE,SURF,MATRIX,SHAPE,
     1          PATERN,PNTVEC,UVCVONSF,DATAST,TEXTVAR,QUILTSF,MESHSF,
     2          NETSF,RBSPLSF,MXNAME,REGSF,TRIMSF,VREVSF,VANOTE,VSYMBOL,
     3          VPLACE,VMACRO,VSOLID
      parameter (POINT    =  3)
      parameter (VECTOR   =  4)
      parameter (LINE     =  5)
      parameter (PLANE    =  6)
      parameter (CIRCLE   =  7)
      parameter (CURVE    =  8)
      parameter (SURF     =  9)
      parameter (MATRIX   = 10)
      parameter (VMACRO   = 11)
      parameter (SHAPE    = 18)
      parameter (PATERN   = 20)
      parameter (PNTVEC   = 21)
      parameter (UVCVONSF = 22)
      parameter (DATAST   = 23)
      parameter (TEXTVAR  = 24)
      parameter (QUILTSF  = 25)
      parameter (MESHSF   = 26)
      parameter (NETSF    = 27)
      parameter (RBSPLSF  = 29)
      parameter (VANOTE   = 30)
      parameter (VSYMBOL  = 31)
      parameter (VPLACE   = 32)
      parameter (VSOLID   = 33)
      parameter (MXNAME   = 64)
      parameter (REGSF    = 91)
      parameter (TRIMSF   = 99)
      parameter (VREVSF   = 100)
c
c...primitive types
c
      integer*2 PLANAR,SPHERE,CYLINDER,CONE,REVOLV
c
      parameter (PLANAR   = 3)
      parameter (SPHERE   = 4)
      parameter (CYLINDER = 5)
      parameter (CONE     = 6)
      parameter (REVOLV   = 7)
      parameter (TORUS    = 8)
c
c...Motion modifiers
c
      integer*2 GO,GOTO,GOFWD,GOLFT,GORGT,GOBACK,GOUP,GODOWN,GODLTA,
     1          TNTO,PSTAN,CS_ON,CS_TO,CS_PAST
c
      parameter (GO     = 702)
      parameter (GOTO   = 703)
      parameter (GOFWD  = 704)
      parameter (GOLFT  = 705)
      parameter (GORGT  = 706)
      parameter (GOBACK = 707)
      parameter (GOUP   = 708)
      parameter (GODOWN = 709)
      parameter (GODLTA = 710)
      parameter (TNTO   = 646)
      parameter (PSTAN  = 729)
      parameter (CS_ON  =  71)
      parameter (CS_TO  = 714)
      parameter (CS_PAST= 715)
c
c... Tool axis modes
c
      integer*2 TA_SAME,TA_NORMAL_PS,TA_ATANGL_PS,TA_TANTO_DS,TA_FAN,
     1          TA_TANTO_DS_PERPTO,TA_TANTO_DS_PARELM,
     2          TA_NORMAL_PS_PERPTO,TA_COMBINE,TA_COMBINE_PARELM,
     3          TA_ATANGL_PS_PERPTO,TA_ATANGL_PS_DIST,
     4          TA_ATANGL_PS_DIST_PERPTO,TA_THRU_PT
c
      parameter (TA_SAME                  =  0)
      parameter (TA_NORMAL_PS             =  1)
      parameter (TA_ATANGL_PS             =  2)
      parameter (TA_TANTO_DS              =  3)
      parameter (TA_FAN                   =  4)
      parameter (TA_TANTO_DS_PERPTO       =  5)
      parameter (TA_TANTO_DS_PARELM       =  6)
      parameter (TA_NORMAL_PS_PERPTO      =  7)
      parameter (TA_COMBINE               =  8)
      parameter (TA_COMBINE_PARELM        =  9)
      parameter (TA_ATANGL_PS_PERPTO      = 10)
      parameter (TA_ATANGL_PS_DIST        = 11)
      parameter (TA_ATANGL_PS_DIST_PERPTO = 12)
      parameter (TA_THRU_PT               = 13)
      parameter (TA_THRU_CV               = 14)
      parameter (TA_INTERPOL              = 15)
