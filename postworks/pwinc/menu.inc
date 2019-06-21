c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        menu.inc , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        03/11/16 , 11:12:04
c
      integer*4 MAX_PATH,MAX_FILE,MAXFMT
c
      parameter (MAX_PATH=1024)
      parameter (MAX_FILE=520)
      parameter (MAXFMT=92)
c
      common /kmenu / SAPNC , IBANPT, MLEVL , NLEVL , ISECP1, ISECP2,
     1                ISECE1, ISECE2, ERRNC , IERRLN, LUNSC1, LUNSC2,
     2                IERRDS, IPRMDS, SAPREC, MAXERR, IPRMIX, MAXSAP,
     3                IMENDT, IPRDAT, IPRTXT, SMREC , WRDREC, IMENIX,
     4                LUNTI , LUNTO , JUNTI , JUNTO , IOWLB , IMENBL
      common /kmenu / MAXMEN, ISELPT, ISELIN, NESC  , IESCPT, ESCBUF,
     1                IORLB , IMENEL, IPRLIN, MENWVL, NMENWD,
     2                MAXMWD, SMLEVL, SNLEVL, IWALK , LUNHLP, LUNSC3,
     3                LUNSC4, PSTWVL, NPSTWD, MAXPWD, IOPFL , NERR  ,
     4                NCCMPF, LISPAG, LISLIN, CURSES, LUNPRM, FRMBEG
      common /kmenu / FRMNFD, FRMREC, FRMWID, FRMST , FRMEN , FRMFNC,
     1                IPRELV, PRMSTK, PRMSP , FRMHPT, NFRMHD, MAXKVR,
     2                KPSTVR, KPSTVL, KPSTSB, MAXRVR, RPSTVR, RPSTVL,
     3                RPSTSB, OPTOVR, MAXWRN, MAXPER, MAXFAT, LICOPT,
     4                PRESEL, ISECP3, ISECE3, ISCRLN, ICLF  , IBMERR
      common /kmenu / LICMCH, NLICMC, MCNSRC, MOTIF , IMTYP , JBTACT,
     1                JBTYPE, JBNCHC, JBDCHC, IMSCAN, MFKNC , MFERR ,
     2                FRMTYP, FRMDCH, FRMNCH, MFERPT, IMHFLG, MHLPNC,
     3                MCRNC , LUNPHD, NOBJFI, IPROGM, PWVER1,
     4                PWVER2, LOADED, ISGINC, ISGLST

      common /frmcom/ SSMREC, SFRMBUF, SSAPNC, LUNBAT, RUNQUIET
c
      integer*4 SAPNC(512),IBANPT,MLEVL(10),NLEVL,ISECP1,ISECP2,ISECE1,
     1          ISECE2,ERRNC(512),IERRLN,IERRDS(128),LUNSC1,LUNSC2,
     2          IPRMDS(128),SAPREC,MAXERR,MAXSAP,IMENDT(128,10),IMENIX,
     3          IPRDAT(2,50),IPRTXT(1500),SMREC(10),WRDREC,LUNTI,LUNTO,
     4          JUNTI,JUNTO,IPRMIX,IOWLB,IMENBL,MAXMEN,ISELPT,ISELIN
      integer*4 NESC,IESCPT,ESCBUF(5),IORLB,IMENEL,IPRLIN,NMENWD,NERR,
     1          MENWVL(200),MAXMWD,SMLEVL(10),SNLEVL,LUNHLP,LUNSC3,
     2          LUNSC4,PSTWVL(720),NPSTWD,MAXPWD,IOPFL(20),IPROGM,
     3          NCCMPF,LISPAG,LISLIN,IWALK,CURSES,LUNPRM,FRMBEG,FRMNFD,
     4          FRMREC,FRMWID,FRMST(20),FRMEN(20),FRMFNC(20,100),IPRELV
      integer*4 PRMSTK(50),PRMSP,FRMHPT(5),NFRMHD,MAXKVR,KPSTVR(50),
     1          KPSTVL(50),KPSTSB(50),MAXRVR,RPSTVR(50),RPSTVL(50),
     2          RPSTSB(50),PRTLDS(1280),OPTOVR(20),MAXWRN,MAXPER,MAXFAT,
     3          LICOPT(8),PRESEL(10),ISECP3,ISECE3,ISCRLN(4),ICLF,
     4          LICMCH(8),NLICMC,IBMERR,MCNSRC,MOTIF,IMTYP,JBTACT(60)
      integer*4 JBTYPE(60),JBNCHC(60),JBDCHC(60),IMSCAN,MFKNC,MFERR,
     1          FRMTYP(20),FRMDCH(20,MAXFMT),FRMNCH(20),MFERPT,IMHFLG,
     2          MHLPNC,MCRNC,LUNPHD,NOBJFI,PWVER1,PWVER2,LOADED,ISGINC,
     3          ISGLST
c
      common /gmenu / NCLVER, FILVAL, USROVR
c
      real*8 NCLVER,FILVAL,USROVR(50)
c
      common /cmenu / SAPRM , SAERR , ERLABL, SALABL, MENWRD, PSTWRD,
     1                LOPFL , LCMPFI, PGMNAM, LDATE , LTIME , LFORMF,
     2                DVDATA, REVDAT, ERRLAB, FRMBUF, REGID , DVDSUP,
     3                LDEVIC, CLNAME, CLDATE, CLTIME, CAMNAM, CAMREV,
     4                SBDEF , SBCDEF, MFANS , MFERTX, FRMLAB, FRMCHC
      common /cmenu / MHLPBF, MCRBUF, CPYDATE,PCHFIL, PHFFIL,
     1                LOBJFI, DEFDIR, PRTFIL
c
      character*1 LFORMF
      character*2 REGID(MAXFMT)
      character*8 ERLABL(512),SALABL(512),LTIME,REVDAT,ERRLAB,CLTIME,
     1            CAMNAM,CAMREV
      character*9 CPYDATE
      character*20  SBCDEF(20,60),FRMCHC(20,60)
      character*11 LDATE,CLDATE
      character*10 PGMNAM
      character*20 FRMLAB(20)
      character*24 MENWRD(200),PSTWRD(720)
      character*80 SAPRM(512),SAERR(512),SBDEF(60),MFANS,
     1             MFERTX,MHLPBF,DVDATA,DVDSUP,LDEVIC
      character*132 FRMBUF(100)
      character*256 LOPFL(20)
      character*6000 LPRTXT
      character*(MAX_PATH) LCMPFI,PCHFIL,PHFFIL,CLNAME,LOBJFI,DEFDIR,
     1                     PRTFIL
      character*(MAX_PATH+80) MCRBUF
c
      equivalence (IPRTXT,LPRTXT), (IMENDT,PRTLDS)

      integer*4 SSMREC(10)
      character*132 SFRMBUF(100)
      integer*4 SSAPNC(512)
      integer*4 LUNBAT, RUNQUIET

      common /klic/ ghMainDlg, ghWndMain
      common /klic/ repnum, addnum, BATCHFILE
      integer*4 ghMainDlg , ghWndMain
      integer*4 repnum, addnum, BATCHFILE
c
c...value to see if the reading is from the file or the text buffer
c...and value of the buffer end line
c
      common /ptdbf/ PTD_RDBUF, BUFEND
      integer*4 PTD_RDBUF, BUFEND
