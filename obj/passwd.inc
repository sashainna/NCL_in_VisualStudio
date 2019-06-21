c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        passwd.inc , 12.1
c     DATE AND TIME OF LAST  MODIFICATION
c        31 Jul 1997 , 10:50:38
c
      common /kpwd  / RTADR , NTRM  , LICNC
c
      integer*4 RTADR(2,2),NTRM(2),LICNC(9)
c
      common /cpwd  / LICDAT
c
      character*60 LICDAT(9)
