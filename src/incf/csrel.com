c*********************************************************************
c**
c**    NAME         :  csrel.com
c**
c**    CONTAINS:  
c**      A common block for CSREL.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       csrel.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:30
c*********************************************************************
c
      common/endab/labandon,l2csrf,lhavecspl,llookfd
      logical labandon,l2csrf,lhavecspl,llookfd

      integer*2 AHEAD,EXTEN,BOTTM,IGNOR
      parameter (AHEAD = 0)
      parameter (EXTEN = 1)
      parameter (BOTTM = 2)
      parameter (IGNOR = 3)
