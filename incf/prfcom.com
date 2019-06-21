c*********************************************************************
c**
c**    NAME         :  prfcom.com 
c**
c**    CONTAINS:
c**     Common block of data used by the PROFIL routines.
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       prfcom.com , 26.2
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/10/18 , 15:01:34
c*********************************************************************
c
      integer*2 N_IPROF,N_KPROF,N_RPROF,N_GPROF,N_CPROF
c
      parameter (N_IPROF = 50)
      parameter (N_KPROF = 10)
      parameter (N_RPROF = 10)
      parameter (N_GPROF = 150)
      parameter (N_CPROF = 80)
c
      common /iprfcm/ IPROF(N_IPROF)
c
      equivalence (IPRFFL,IPROF(001)), (IPRFFD,IPROF(026))
      equivalence (IPTCOM,IPROF(032))
c
      integer*2 IPRFFL(25),IPRFFD(6),IPTCOM
c
      common /kprfcm/ KPROF(N_KPROF)
c
      equivalence (KDUMMY,KPROF(001))
c
      integer*4 KDUMMY
c
      common /rprfcm/ RPROF
c
      real*4 RPROF(N_RPROF)
c
      equivalence (RDUMMY,RPROF(001))
c
      common /gprfcm/ GPROF
c
      real*8 GPROF(N_GPROF)
c
      equivalence (PRFTV ,GPROF(001)), (PRFIVC,GPROF(011))
      equivalence (PRFSPT,GPROF(014)), (PRFPAS,GPROF(017))
      equivalence (PRFTHK,GPROF(021)), (PRFPLN,GPROF(025))
      equivalence (PRFTPL,GPROF(029)), (PRFCLF,GPROF(033))
      equivalence (PRFDEP,GPROF(038)), (PRFENT,GPROF(042))
      equivalence (PRFRPL,GPROF(048)), (PRFEXI,GPROF(052))
      equivalence (PRFEPT,GPROF(058)), (PRFLAP,GPROF(061))
      equivalence (PRFANG,GPROF(063)), (PRFFED,GPROF(064))
      equivalence (PRFFSV,GPROF(074)), (PRFNVC,GPROF(075)) 
      equivalence (PRFPPL,GPROF(078)), (PRRCT ,GPROF(082))
      equivalence (PRFMXD,GPROF(097)), (PRFTLT,GPROF(098))
c
      real*8 PRFTV(10),PRFIVC(3),PRFSPT(3),PRFPAS(4),PRFTHK(4),
     1       PRFPLN(4),PRFTPL(4),PRFCLF(4),PRFDEP(4),PRFENT(6),
     2       PRFRPL(5),PRFEXI(6),PRFEPT(3),PRFLAP(2),PRFANG,PRFFED(10),
     3       PRFNVC(3),PRFFSV,PRFPPL(5),PRRCT(15),PRFMXD,PRFTLT(4)
	 
	 
c
      common /cprfcm/ CPROF
	  

c

      character*1 CPROF(N_CPROF)
c
      equivalence (CDUMMY ,CPROF(001))
c
      character*80 CDUMMY
c
