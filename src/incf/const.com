c*********************************************************************
c**
c**    NAME         :  const.com 
c**
c**    CONTAINS:
c**     various useful constants 
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**      const.com , 25.1
c**   DATE AND TIME OF LAST  MODIFICATION
c**      04/29/15 , 15:07:30
c*********************************************************************

      real*8 ZERO,HALF,ONE,TWO,PI,HALF_PI,PI_32,TWO_PI,RADIAN,INCH,
     1       HUGE_VAL
c
      parameter (ZERO    = 0.d0)
      parameter (HALF    = .5d0)
      parameter (ONE     = 1.d0)
      parameter (TWO     = 2.d0)
      parameter (PI      = 3.1415926535897932d0)
      parameter (HALF_PI = 1.5707963267948966d0)
      parameter (PI_32   = 4.7123889803846898d0)
      parameter (TWO_PI  = 6.2831853071795864d0)
      parameter (RADIAN  = 57.2957795130823215d0)
      parameter (INCH    = 25.4d0)
      parameter (HUGE_VAL= 1.d09)
