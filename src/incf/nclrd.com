c**********************************************************************
c**
c**    NAME         :  nclrd.com 
c**
c**    CONTAINS:
c**          virtual ranfil and vocabulary common
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       nclrd.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:32
c**
c*********************************************************************
c      common /window/ iwndw(4032)               !    for rsx
      common /wndow/ iwndw(282240)
      common /reg/ irdb,iwdb,vrdb,vwdb
      integer*2 iwndw,irdb(8),iwdb(8),vrdb(8),vwdb(8),vocnum(1024),
     1          ncrb(40)
      logical leq99
      character*6 vocnam(1024)
      equivalence (vocnum(1),iwndw(1))
      equivalence (vocnam(1),leq99),(leq99,iwndw(513))
      equivalence (nccr,iwndw(2049)),(nccq,iwndw(2050))
      equivalence (ncrb(1),iwndw(2051))

