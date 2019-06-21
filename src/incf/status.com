c*********************************************************************
c**
c**    NAME         :  status.com
c**
c**    CONTAINS:  
c**      Status area variables.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       status.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:33
c*********************************************************************
c
      integer*2 N_ISTMAP
c
      PARAMETER (N_ISTMAP = 40)
c
      common /istmap/ ISTMAP
      integer*2 ISTMAP(N_ISTMAP)
c
      equivalence (PGINDX,ISTMAP(001)), (PGMODE,ISTMAP(002))
c
      integer*2 PGINDX,PGMODE(30)
