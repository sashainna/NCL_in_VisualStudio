c**********************************************************************
c**
c**    NAME         :  vaxvoc.com 
c**
c**    CONTAINS:
c**          contains vocabulary common for vax version
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       vaxvoc.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:33
c**
c*********************************************************************
c
      integer*2 N_VOC
c
      parameter (N_VOC = 1024)
c
      common /ivxvoc/ vvcnum
c
      integer*2 vvcnum(N_VOC)
c
      common /cvxvoc/ vvcnam
c
      character*24 vvcnam(N_VOC)
