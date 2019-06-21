c*********************************************************************
c**
c**    NAME         :  ifcom.com
c**
c**    CONTAINS:  
c**      The common area for IF-THEN-ELSE.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       ifcom.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:31
c*********************************************************************
c
      integer*2 N_IIFCOM,N_KIFCOM,MAXIF
c
      PARAMETER (N_IIFCOM = 100)
      PARAMETER (N_KIFCOM = 320)
      PARAMETER (MAXIF = 100)
c
      common /iifcom/ IIFCOM
      integer*2 IIFCOM(N_IIFCOM)
c
      equivalence (JMPFLG,IIFCOM(001))
c
      logical*2 JMPFLG(MAXIF)
c
      common /kifcom/ KIFCOM
      integer*4 KIFCOM(N_KIFCOM)
c
      equivalence (IFINDX,KIFCOM(001)), (IFIDEC,KIFCOM(002))
      equivalence (IFA   ,KIFCOM(003)), (IFSTAT,KIFCOM(103))
      equivalence (IFDEC ,KIFCOM(203))
c
      integer*4 IFINDX,IFIDEC,IFA(MAXIF),IFSTAT(MAXIF),IFDEC(MAXIF)
