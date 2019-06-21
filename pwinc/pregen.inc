c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pregen.inc , 24.3
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/14 , 12:58:39
c
      integer*4 MAXCLD
c
      PARAMETER (MAXCLD=244)
c
      common /kpre  / PRENUM, PREPT , IPC   , JHED  , MEMPT , MEMSIZ,
     1                NPREH1, NPREH2, NPREH3, NPREH4, INEREC, INEPT ,
     2                MCHPT , DUMNFI, IMACPT, IMACHD, MAXMAC, MAXPRN,
     3                IPNTX , ILSTOP, IMULTO, NMACS , MCRLST, MCNPAR,
     4                LUNMAC, SCRECN, MACSTA, SCRECM, MCRBSF, MAXMCN
      common /kpre  / INCMAC, INCCNT, PLOOK , SAVIRL, NSAVRL, MSAVRL,
     1                FIOFN , FIOPND, MSAVPX, ISAVPX, FNTEMP
c
      integer*4 PRENUM(10),PREPT,JHED(8),MEMPT(2),MEMSIZ,NPREH1,NPREH2,
     1          NPREH3,NPREH4,INEREC,INEPT,MCHPT,DUMNFI,IMACHD(6,20),
     2          IMACPT,MAXMAC,MAXPRN,IPNTX,IPC,ILSTOP,IMULTO,NMACS,
     3          MCRLST(51),MCNPAR(50),LUNMAC,SCRECN,MACSTA,MCRBSF(50),
     4          SCRECM,MAXMCN,INCCNT,INCMAC(10),PLOOK,SAVIRL(5,500)
      integer*4 NSAVRL,MSAVRL,FIOFN,FIOPND(5),MSAVPX,ISAVPX,FNTEMP
c
      integer*2 IHED(16)
      equivalence (JHED,IHED)
c
      common /gpre  / RCOBUF, RCMPL , PRNVAR, IFTOL, SCMBUF, SAVGRL,
     1                CLDATA
c
      real*8 RCOBUF(64),RCMPL(50),PRNVAR(2,50),IFTOL,SCMBUF(50),
     1       SAVGRL(500),CLDATA(MAXCLD)
c
      integer*2 ICOBUF(256)
      integer*4 JCOBUF(128)
      character*512 LCOBUF
      equivalence (RCOBUF,ICOBUF,JCOBUF,LCOBUF)
c
      integer*2 ICMPL(200),ICMBUF(200)
      integer*4 JCMPL(100),JCMBUF(100)
      character*400 LCMPL
      character*8 LCMBUF(50)
      character*80 LCMREC(5)
      equivalence (RCMPL,ICMPL,JCMPL,LCMPL)
      equivalence (SCMBUF,ICMBUF,JCMBUF,LCMREC,LCMBUF)
c
      common /cpre  / LPREH1, LPREH2, LPREH3, LPREH4, MCHNAM, DUMOBJ,
     1                LPRNTX, MCLST , PSTNAM, CLTEXT, SAVGTX, OBJUPD,
     2                FLFNAM
c
      character*6 MCHNAM(10)
      character*8 MCLST(50),OBJUPD
      character*40 PSTNAM(10)
      character*80 LPREH1,LPREH2,LPREH3,LPREH4,DUMOBJ
      character*512 LPRNTX,FLFNAM(5)
      character*1952 CLTEXT
      character*4000 SAVGTX
