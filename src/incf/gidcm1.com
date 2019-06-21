c*********************************************************************
c**
c**    NAME         :  gidcm1.com
c**
c**    CONTAINS:  
c**      A common block for Guide Curves.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       gidcm1.com , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 15:07:31
c*********************************************************************
c
      common/gidcm1/ gdfw,gdkey,igdfwd,lcvpv,lcvrgt,lsamta,lcvrg1
      real*4 gdfw(3)
      integer*4 gdkey
      integer*2 igdfwd
      logical lcvpv,lcvrgt,lsamta,lcvrg1
