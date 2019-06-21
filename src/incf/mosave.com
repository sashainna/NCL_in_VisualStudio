c*********************************************************************
c**
c**    NAME         :  mosave.com
c**
c**    CONTAINS:
c**     Storage to save off the 'ifl' and 'sc' arrays.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       mosave.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:32
c*********************************************************************
c
      common /imosav/ iflsv
c
      integer*2 iflsv(100)
c
      common /kmosav/ i4stsv, i4rec , i4rec2, i4rec3, i4fsav
c
      integer*4 i4stsv(2),i4rec(2),i4rec2(2),i4rec3(2),i4fsav(10)
c
      common /gmosav/ scsv  , sppsav, odptsv
c
      real*8 scsv(100),sppsav(42),odptsv(6)
