c*********************************************************************
c**
c**    NAME         :  cmm.com
c**
c**    CONTAINS:  cmm.com  
c**     The ccmcom is the shared common region used by CMM logic,
c**     PODDEF and PODPTS commands.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       cmm.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:29
c*********************************************************************
c
      integer*2 N_ICMCOM,N_GCMCOM,N_CCMCOM
c
      PARAMETER (N_ICMCOM = 25)
      PARAMETER (N_I4CMCOM = 25)
      PARAMETER (N_GCMCOM = 150)
      PARAMETER (N_CCMCOM = 100)
c
c...CMM and PODDEF common variables
c
      common /ICMCOM/ ICMCOM
      integer*2 ICMCOM(N_ICMCOM)
c
      equivalence (CMMINM,ICMCOM(001)), (CMMNPT,ICMCOM(002))
      equivalence (NPODPN,ICMCOM(003))
      integer*2 CMMINM,CMMNPT,NPODPN
	  
      common /I4CMCOM/ ICM4COM
      integer*4 I4CMCOM(N_I4CMCOM)

	  equivalence (PMACSUB,I4CMCOM(001)),(PVARSUB,I4CMCOM(002))
      integer*4 PMACSUB, PVARSUB(8)
c
      common /gcmcom/ GCMCOM
      real*8 GCMCOM(N_GCMCOM)
c
      equivalence (CMMTOL,GCMCOM(001)), (CMMMXD,GCMCOM(002))
      equivalence (CMMMXA,GCMCOM(003))
      equivalence (PODPID,GCMCOM(004)), (PODHIT,GCMCOM(039))
      equivalence (PODPAR,GCMCOM(064)), (PODMAC,GCMCOM(069))
      equivalence (PODVAR,GCMCOM(077)), (CMMMNM,GCMCOM(141))
c
      real*8 CMMTOL,CMMMXD,CMMMXA,PODPID(25),PODHIT(25),
     1       PODPAR(5)
      character*64 CMMMNM, PODMAC,PODVAR(8)
c
      common /ccmcom/ CCMCOM
      character*1 CCMCOM(N_CCMCOM)
c
      equivalence (COMPOD,CCMCOM(001))
c
      character*100 COMPOD


