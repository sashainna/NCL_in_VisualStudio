c*********************************************************************
c**
c**    NAME         :  toolib.com 
c**
c**    CONTAINS:
c**     The main common for the toolib routine.
c**
c**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c**    MODULE NAME AND RELEASE LEVEL 
c**       toolib.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:33
c*********************************************************************
c
      parameter (MAX_PATH=1024)
      parameter (MAX_FILE=520)
c
      common /kmenu / SAPNC , IBANPT, MLEVL , NLEVL , ISECP1, ISECP2,
     1                ISECE1, ISECE2, IERRLN, LUNSC1, LUNSC2, IRUNIT,
     2                IPRMDS, SAPREC, IPRMIX, MAXSAP, FRMNFD, FRMNC ,
     3                IMENDT, IPRDAT, IPRTXT, SMREC , WRDREC, IMENIX,
     4                LUNTI , LUNTO , JUNTI , JUNTO , IOWLB , IMENBL
      common /kmenu / MAXMEN, ISELPT, ISELIN, NESC  , IESCPT, ESCBUF,
     1                IORLB , IMENEL, IPRLIN, MENWVL, NMENWD, MAXMWD,
     2                SMLEVL, SNLEVL, LUNHLP, LUNSC3, LUNSC4, IOPFL ,
     3                CURSES, LUNPRM, FRMBEG, FRMEND, FRMFNC, IPRELV,
     4                PRMSTK, PRMSP , NFRMHD, PRESEL, IBMERR, LUNLIB
      common /kmenu / TOOLIX, FRMFLN, FRMFST, FRMFEN, FRMNCH, FRMLIN,
     1                FRMLNF, FRMNLN, FLIBEG, FLIEND, FRMHLN, IXPT  ,
     2                PAGLEN, IPAGE , ILINE , LHEDNC, LHEDPT, I0    ,
     3                I1    , I2    , I3    , I4,     I5    , I7    ,
     4                I8    , I75   , I80   , FRMNTG, FRMTGF, FRMTOG
      common /kmenu / FRMTTG
      common /kmenu / MOTIF
      common /kmenu / LSTRD, RCPOS, RCTOOL
      common /kmenu / MFSTAT, MFSNUM
	  common /kmenu / CURCUT, TCUTTYP, VERSION, NDBTOOL
c
      integer*4 SAPNC(200),IBANPT,MLEVL(10),NLEVL,ISECP1,ISECP2,ISECE1,
     1          ISECE2,IERRLN,LUNSC1,LUNSC2,LUNLIB,FRMNC(100),FRMNCH(5),
     2          IPRMDS(128),SAPREC,MAXSAP,IMENDT(128,10),IMENIX,
     3          IPRDAT(2,50),IPRTXT(1500),SMREC(10),WRDREC,LUNTI,LUNTO,
     4          JUNTI,JUNTO,IPRMIX,IOWLB,IMENBL,MAXMEN,ISELPT,ISELIN
      integer*4 NESC,IESCPT,ESCBUF(5),IORLB,IMENEL,IPRLIN,NMENWD,
     1          MENWVL(100),MAXMWD,SMLEVL(10),SNLEVL,LUNHLP,LUNSC3,
     2          LUNSC4,IOPFL(10),CURSES,LUNPRM,FRMBEG,FRMNFD,IPRELV,
     3          PRMSTK(50),PRMSP,NFRMHD,FRMFNC(100),PRTLDS(1280),
     4          PRESEL(10),TOOLIX(128),IBMERR,FRMEND,FRMNLN,FRMFLN(100)
      integer*4 FRMFST(100),FRMFEN(100),FRMLIN(10,50),FRMLNF(50),FLIBEG,
     1          FLIEND,FRMHLN(3),IXPT,IRUNIT,PAGLEN,IPAGE,LHEDNC,LHEDPT,
     2          ILINE,I0,I1,I2,I3,I4,I5,I7,I8,I75,I80,FRMNTG,FRMTTG,
     3          FRMTGF(4,20),FRMTOG(60), MOTIF, MFSNUM(6)
c
      common /gmenu / FGREC  , FGBUF , NCLVER
c
      integer*2 FIREC(256),FIBUF(512)
      integer*4 FREC(128),FBUF(256)
      integer*4 RCPOS(10000), RCTOOL(10000)
      real*8 FGREC(64),FGBUF(128),NCLVER
      character*512 FCREC
      character*1024 FCBUF
c
      equivalence (FGREC,FREC,FIREC,FCREC), (FGBUF,FBUF,FIBUF,FCBUF)
c
      integer*4 IXREC,IUNIT,IDBCHG,NDBENT,IDBLOW,IDBHGH,NDBTOOL,I2NDRC,
     1          I2NDPT,I2NDMX
      character*22 SYMLIB
      character*40 TOOLDS
      character*256 LIBNAM
c
      equivalence (IXREC ,FREC(1))
      equivalence (IUNIT ,FREC(25)), (NDBENT,FREC(26))
      equivalence (IDBCHG,FREC(27)), (IDBLOW,FREC(28))
      equivalence (IDBHGH,FREC(29))
      equivalence (I2NDRC,FREC(115)), (I2NDPT,FREC(116))
      equivalence (I2NDMX,FREC(117))
c
      equivalence (SYMLIB,FIREC(38))
      equivalence (TOOLDS,FREC(31)), (LIBNAM,FREC(41))
c
      common /cmenu / SAPRM , SALABL, MENWRD, FRMHD , LHEDBF,
     1                PGMNAM, LDATE , LTIME , LFORMF, LDEVIC,
     2                DVDATA, REVDAT, FRMBUF, FRMFBF, LBATCH,
     3                LLOAD , LBRIEF, LFULL , LODFNM
c
      character*1 LFORMF
      character*8 SALABL(200),MENWRD(100),LTIME
c      character*9 LDATE
c...as of 11-FEB-2000
      character*12 LDATE
      character*10 PGMNAM
      character*11 REVDAT
      character*80 SAPRM(200),DVDATA,LHEDBF,LDEVIC,FRMFBF(100)
      character*256 LLOAD, LBATCH,LBRIEF, LFULL, LODFNM
      character*132 FRMBUF(100),FRMHD(5)
      character*6000 LPRTXT
      character*80 LSTRD(10000)
      character*200 MFSTAT(6)
      integer*4 CURCUT, TCUTTYP(10000), VERSION
c
      equivalence (IPRTXT,LPRTXT), (IMENDT,PRTLDS)
