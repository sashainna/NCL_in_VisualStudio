c*********************************************************************
c**
c**    NAME         :  pokpl.com
c**
c**    CONTAINS:  
c**      A common block for pocketing.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       pokpl.com , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 15:07:32
c*********************************************************************
c
      common/rrpl/ botpl(4), toppl(4), clpl(4), sfcut
      real*8 botpl, toppl, clpl
      logical sfcut
