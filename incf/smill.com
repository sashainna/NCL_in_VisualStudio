c*********************************************************************
c**
c**    NAME         :  smill.com
c**
c**    CONTAINS:  
c**      The common area for SMILL
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       smill.com	25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15	15:07:33
c*********************************************************************
 
      integer*2 max_avpsf
      parameter (max_avpsf=400)

      common/smlcmi/navpsf,navend,navd,navbnd,navstep,navtype,navlrpto
      integer*2 navpsf,navend,navd,navbnd,navstep,navtype,navlrpto

      common/smlcmr/avpsf,avdpl,avbnd,avlasn,avlfr
      real*8 avpsf(max_avpsf),avdpl(2),avbnd,avlasn,avlfr
