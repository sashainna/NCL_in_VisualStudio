c**********************************************************************
c**
c**    NAME         :  com8a.com 
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       com8a.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:30
c**
c*********************************************************************
      implicit real*8 (a-h,o-z),  integer*2 (i-n)
      include 'com.com'
