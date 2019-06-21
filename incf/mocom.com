c*********************************************************************
c**
c**    NAME         :  mocom.com 
c**
c**    CONTAINS:
c**     Motion common
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       mocom.com , 25.2
c**    DATE AND TIME OF LAST MODIFICATION
c**       10/27/16 , 12:03:33
c*********************************************************************
C
      integer*2 N_MOCCOM
c
      parameter (N_MOCCOM = 450)
c
      common /moccom/ MOCCOM
c
      real*8 MOCCOM(N_MOCCOM)
c
      common /mocombcom/ svdism,savtb,lapmov,ltanto,fautops, sautops
c
      logical*2 lapmov,ltanto,fautops, sautops
c
      real*8 svdism,savtb(3)      
c
      equivalence (TCOL  ,MOCCOM(001)), (DTBL  ,MOCCOM(091))
      equivalence (DUM1MO,MOCCOM(291)), (STATIX,MOCCOM(351))
      equivalence (CLBUFF,MOCCOM(352)), (DUM2MO,MOCCOM(388))
      equivalence (SCHLD ,MOCCOM(431))
c
      real*8 tcol(90),dtbl(200),dum1mo(60),statix,clbuff(36)
      real*8 dum2mo(43), schld(6)
c
      integer*2 NMTOOL
c
      parameter (NMTOOL = 20)
c
      real*4 t(30,3),s(10,4),tool(20)
      real*8 d(200), hgo(17)
      integer*2 istat(2)
      integer*4 i4stat(2)
      equivalence (t,tcol),(d,dtbl),(s,dum1mo),(tool,dum1mo(31))
      equivalence (hgo,dum2mo(20))
      equivalence (statix,istat,i4stat)
