c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        compile.inc , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:24
c
      integer*4 MAXCLD
c
      parameter (MAXCLD=244)
c
      common /kcomp / NCPRN , NPNVAR, ISTMNC, ICTYP , NTOK  , ICNC  ,
     1                MAXTOK, LUNMAC, NMACRO, NLABEL, NSCAL , MNREC ,
     2                MFPT  , MFREC , MFNXT , MFDAT , IMACDF, ISCAST,
     3                MNFIX , ICREC , IMACPT, IPNSMP, MFMAIN, ITXTPT,
     4                ITXCMD, MAXARG, MAXPRN, ILABEL, PRNNC , IFFLAG,
     5                MAXIF , NC1LAB, NC2LAB, IFPT  , IFELSF, IPASS
      common /kcomp / LABPC , IDOTYP, IDOSUB, IDOCNC, IDOPRN, IDOPT1,
     1                IDOPT2, IDOLPT, IFLAB , ICURLN, IPC   , MAXMEM,
     2                MEMREC, NCMPH1, NCMPH2, NCMPH3, NCMPH4, DOCLIN,
     3                PFPT  , NPMAC , DESCHR, MEMSIZ, MEMPT , INCLUN,
     4                INCFLN, IMINST, ICOMCI, IDPT  , IDARY
c
      integer*2 ICMPL(200),MFIDAT(256,5),PFIDAT(256,5)
c
      integer*4 MAXPRN,NCPRN,NTOK,NPNVAR,ISTMNC,ICTYP(100),ICNC(100),
     1          MAXTOK,LUNMAC,NMACRO,NLABEL,NSCAL(2),MFPT(3,5),MFREC(5),
     2          MFNXT,MFDAT(128,5),IMACDF,ISCAST(2),ICREC(5),IMACPT,
     3          JCMPL(100),IPNSMP(50),MFMAIN(8),ITXTPT,MNFIX(5),IFFLAG,
     4          MNREC(5),ITXCMD,MAXARG,ILABEL,PRNNC(50),MAXIF,IFLAB,
     5          NC1LAB(10),NC2LAB(10),IFPT,IFELSF(10),LABPC,IPASS
      integer*4 IDOTYP(10),IDOSUB(10),IDOCNC(10),IDOPRN(10),IDOPT1(10),
     1          IDOPT2(10),IDOLPT,ICURLN,IPC,MEMSIZ,MEMREC,NCMPH1,
     2          NCMPH2,NCMPH3,NCMPH4,DOCLIN,PFPT(2,5),PFDAT(128,5),
     3          NPMAC,DESCHR,MEMPT(2,20),MAXMEM,INCLUN(10),INCFLN,
     4          IMINST,ICOMCI,IDPT,IDARY(10)
c
      common /gcomp / RCSUB , PRNVAR, RCMPL, PFRDAT
c
      real*8 RCSUB(100),RCMPL(50),PRNVAR(50),PFRDAT(64,5)
c
      common /ccomp / PRNBUF, LSTMT , LCTXT , LCOMTX, LF1LAB, LF2LAB,
     1                LELSE , IDOLAB, LCMPH1, LCMPH2, LCMPH3, LCMPH4,
     2                LMACRO
c
      character*6 LCOMTX(10),LELSE
      character*24 LF1LAB(10),LF2LAB(10),IDOLAB(10),LMACRO
      character*132 LCMPH1,LCMPH2,LCMPH3,LCMPH4
      character*400 LCMPL
      character*512 PRNBUF,LSTMT,LCTXT
c
      equivalence (ICMPL,JCMPL,RCMPL,LCMPL)
      equivalence (MFDAT,MFIDAT)
      equivalence (PFRDAT,PFDAT,PFIDAT)
