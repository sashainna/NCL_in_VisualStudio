c*********************************************************************
c**
c**    NAME         :  suvcom.com
c**
c**    CONTAINS:  
c**      Part, Drive, and Check surface UV settings.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       suvcom.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:33
c*********************************************************************
c
      integer*2 N_ISUVCM,N_KSUVCM,N_RSUVCM
c
      PARAMETER (N_ISUVCM = 5)
      PARAMETER (N_KSUVCM = 5)
      PARAMETER (N_RSUVCM = 25)
c
      common /isuvcm/ ISUVCM
      integer*2 ISUVCM(N_ISUVCM)
c
      equivalence (CSFLGS,ISUVCM(001))
c
      logical*2 CSFLGS(5)
c
      common /ksuvcm/ KSUVCM
      integer*4 KSUVCM(N_KSUVCM)
c
      equivalence (KUV   ,KSUVCM(001))
c
      integer*4 KUV(3)
c
      common /rsuvcm/ RSUVCM
      real*4 RSUVCM(N_RSUVCM)
c
      equivalence (UVSV  ,RSUVCM(001))
      equivalence (HU    ,RSUVCM(007)), (HV    ,RSUVCM(010))
      equivalence (MCSU  ,RSUVCM(013)), (MCSV  ,RSUVCM(018))
c
      real*4 UVSV(6),HU(3),HV(3),MCSU(5),MCSV(5)
c
      real*4 psu,psv,dsu,dsv,csu,csv
      equivalence (psu,UVSV(1)), (psv,UVSV(2))
      equivalence (dsu,UVSV(3)), (dsv,UVSV(4))
      equivalence (csu,UVSV(5)), (csv,UVSV(6))
