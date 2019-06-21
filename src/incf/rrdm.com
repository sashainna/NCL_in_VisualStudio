c*********************************************************************
c**
c**    NAME         :  rrdm.com 
c**
c**    CONTAINS:
c**     Common block of data used by POKMOD and RAFPOK Fortran routines.
c**     These routines process data for the advanced POCKET feature.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       rrdm.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:32
c*********************************************************************
c
      integer*2 N_IRRDM,N_KRRDM,N_RRRDM,N_GRRDM,N_CRRDM
c
      parameter (N_IRRDM = 50)
      parameter (N_KRRDM = 10)
      parameter (N_RRRDM = 30)
      parameter (N_GRRDM = 300)
      parameter (N_CRRDM = 80)
c
      common /irrdm/ IRRDM(N_IRRDM)
c
      equivalence (NRAMPS,IRRDM(001)), (ENTRY ,IRRDM(002))
      equivalence (CLTYP ,IRRDM(003)), (PDIR  ,IRRDM(004))
      equivalence (LCTYP ,IRRDM(005)), (ISPIRL,IRRDM(006))
      equivalence (SLIFT ,IRRDM(007)), (CORNER,IRRDM(008))
      equivalence (NLOOPS,IRRDM(009)), (RTRACT,IRRDM(010))
      equivalence (NWRN  ,IRRDM(011)), (JCTCOM,IRRDM(012))
      equivalence (JCTCM1,IRRDM(013)), (JCTCM2,IRRDM(014))
      equivalence (LVDEP ,IRRDM(015)), (TRANST,IRRDM(016))
      equivalence (ISLE  ,IRRDM(017)), (PERIM ,IRRDM(018))
      equivalence (LCDIR ,IRRDM(019)), (PCLIK ,IRRDM(020))
      equivalence (DIRP  ,IRRDM(021)), (DIRI  ,IRRDM(022))
      equivalence (TTYP  ,IRRDM(023)), (ETYP  ,IRRDM(024))
      equivalence (EELM  ,IRRDM(025)), (LTYP  ,IRRDM(026))
      equivalence (LOPS  ,IRRDM(027)), (STTYP ,IRRDM(028))
      equivalence (WCLIK ,IRRDM(029)), (WLAYN ,IRRDM(030))
      equivalence (WBTYP ,IRRDM(031)), (WTTYP ,IRRDM(032))
      equivalence (WSTOCK,IRRDM(033)), (WLTYP ,IRRDM(034))
      equivalence (WLOPS ,IRRDM(035)), (WFROM ,IRRDM(036))
      equivalence (OFFPRT,IRRDM(037)), (WZTYP ,IRRDM(038))
      equivalence (WZLST ,IRRDM(039)), (WDEEP ,IRRDM(040))
      equivalence (TAMODE,IRRDM(041)), (SCRDIR,IRRDM(042))
      equivalence (HLDIR, IRRDM(043)), (LCFIN ,IRRDM(044))
      equivalence (WFINIS,IRRDM(045)), (WMETH ,IRRDM(046))
      equivalence (OPENFL,IRRDM(047))
c
      integer*2 NRAMPS,ENTRY,CLTYP,PDIR,ISPIRL,SLIFT,CORNER,TRANST,
     1          NLOOPS,RTRACT,NWRN,JCTCOM,JCTCM1,JCTCM2,LVDEP,ISLE,
     2          PERIM,PCLIK,DIRP,DIRI,TTYP,ETYP,EELM,LTYP,LOPS,STTYP,
     3          OFFPRT,TAMODE,SCRDIR,HLDIR,WMETH,OPENFL
      integer*2 LCDIR,LCTYP,LCFIN
      integer*2 WCLIK,WLAYN,WBTYP,WTTYP,WSTOCK,WLTYP,WLOPS,WFROM,WZTYP,
     x          WZLST,WDEEP,WFINIS
c
      common /krrdm/ KRRDM(N_KRRDM)
c
      equivalence (KEYTXT,KRRDM(001))
c
      integer*4 KEYTXT
c
      common /rrrdm/ RRRDM
c
      real*4 RRRDM(N_RRRDM)
c
      equivalence (RMPDIS,RRRDM(001)), (NUMLVL,RRRDM(002))
      equivalence (RETDIS,RRRDM(003)), (SLWANG,RRRDM(004))
      equivalence (MAXSTP,RRRDM(005)), (MINSTP,RRRDM(006))
      equivalence (GENFR ,RRRDM(007)), (POSFR ,RRRDM(008))
      equivalence (RETRFR,RRRDM(009)), (ENTRFR,RRRDM(010))
      equivalence (TRANFR,RRRDM(011)), (FINFR ,RRRDM(012))
      equivalence (FRSFR ,RRRDM(013)), (MAXANG,RRRDM(014))
      equivalence (ARCRAD,RRRDM(015)), (TDIS  ,RRRDM(016))
      equivalence (WBOTP ,RRRDM(017)), (WTOPP ,RRRDM(018))
      equivalence (WSTKP ,RRRDM(019)), (WGAP  ,RRRDM(020))
      equivalence (FINTHK,RRRDM(021)), (HRTDIS,RRRDM(022))
      equivalence (VRTDIS,RRRDM(023)), (HSCALP,RRRDM(024))
      equivalence (PSTHK, RRRDM(025)), (DSTHK, RRRDM(026))
      equivalence (ADJDN, RRRDM(027)), (ADJUP, RRRDM(028))
	   equivalence (OFFDIS,RRRDM(029)), (OFFTHK,RRRDM(030))
c
      real*4 RMPDIS,NUMLVL,RETDIS,SLWANG,MAXSTP,MINSTP,MAXANG,GENFR,
     1       POSFR,RETRFR,ENTRFR,TRANFR,FINFR,FRSFR,ARCRAD,TDIS,WBOTP,
     2       WTOPP,WSTKP,WGAP,FINTHK,HRTDIS,VRTDIS,HSCALP,PSTHK,DSTHK
      real*4 ADJDN,ADJUP,OFFDIS,OFFTHK
c
      common /grrdm/ GRRDM, RSVTOK, RSVSUB
c
      real*8 GRRDM(N_GRRDM)
      character*64 RSVTOK(120)
      integer*4 RSVSUB(120)
c
      equivalence (CLRLVL,GRRDM(001)), (DIRVEC,GRRDM(002))
      equivalence (RSVASW,GRRDM(003))
      equivalence (RCT   ,GRRDM(123)), (clrlv_lab,GRRDM(138))
      equivalence (clrlv_inx   ,GRRDM(139))
	  equivalence (WSTEP, GRRDM(140))
c
      real*8 CLRLVL,DIRVEC,RSVASW(120),RCT(15)
      character*64 clrlv_lab
      integer*4 clrlv_inx
c
      common /crrdm/ CRRDM
c
      character*1 CRRDM(N_CRRDM)
c
      equivalence (CUTCM ,CRRDM(001))
c
      character*80 CUTCM
c
      integer*2 bshrt,bsame,bccw,bcw
      parameter (bshrt=0,bsame=1,bccw=2,bcw=3)
c
      integer*2 fpsam,fpccw,fpclw,fpnon,fprev
      parameter (fpsam=0,fpccw=1,fpclw=2,fpnon=3,fprev=4)
 
