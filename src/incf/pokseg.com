c*********************************************************************
c**
c**    NAME         :  pokseg.com
c**
c**    CONTAINS:  
c**      Common blocks for loop cutting pocket routines.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       pokseg.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:32
c*********************************************************************

      integer*2 maxseg
      parameter (maxseg=600)

      common/segarr/XSEG(maxseg),YSEG(maxseg),USEG(maxseg),ISEG(maxseg)

      real*4 xseg,yseg,useg
      integer*2 iseg
