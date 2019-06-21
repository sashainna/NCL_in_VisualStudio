
c*********************************************************************
c**
c**    NAME         :  gpts.com
c**
c**    CONTAINS:
c**     Storage to save GENPTS variables.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       gpts.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:31
c*********************************************************************
c
      common /kgpts/ igprec
c
      integer*4 igprec(2)
c
      common /ggpts/ gptend
c
      real*8 gptend(10)
