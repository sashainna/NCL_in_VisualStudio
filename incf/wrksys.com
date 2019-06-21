c*********************************************************************
c**
c**    NAME         :  wrksys.com
c**
c**    CONTAINS:  
c**      The Working Coordinate System matrices.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       wrksys.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:34
c*********************************************************************
c
      integer*2 N_WRKSYS,N_CWKSYS
c
      PARAMETER (N_WRKSYS = 25)
      PARAMETER (N_CWKSYS = 252)
c
      common /wrksys/ WRKSYS
      real*8 WRKSYS(N_WRKSYS)
c
      equivalence (WRKMX ,WRKSYS(001)), (INVWRK,WRKSYS(013))
      equivalence (WRKSCL,WRKSYS(025))
c
      real*8 WRKMX(12),INVWRK(12),WRKSCL
c
      common /cwksys/ CWKSYS
      character*1 CWKSYS(N_CWKSYS)
c
      equivalence (MODLAB,CWKSYS(001)), (RSNAME,CWKSYS(081))
      equivalence (TCNAME,CWKSYS(161))
c
      character*80 MODLAB,RSNAME,TCNAME
