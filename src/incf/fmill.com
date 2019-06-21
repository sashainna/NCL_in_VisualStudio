c*********************************************************************
c**
c**    NAME         :  fmill.com
c**
c**    CONTAINS:  
c**      The common area for FMILL
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       fmill.com , 26.2
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/10/18 , 14:59:28
c*********************************************************************
 
      integer*2 max_avcsf
      parameter (max_avcsf=40)
	  
	  
      common/fmlcmi/avflg,avrpto,avlst,avdir,navcsf,javcsf,delflg
      integer*2 avflg,avrpto,avlst,avdir,navcsf,javcsf,delflg

      common/fmlcml/lpssec
      logical lpssec

      common/fmlcmr/avasn1,avasn2,avfr1,avfr2,avcsf,avthk,tadir
      real*8 avfr1,avfr2,tadir
      real*8 avasn1,avasn2,avcsf(max_avcsf),avthk(max_avcsf)
	  
	  integer*2 max_sc
      parameter (max_sc=20)
        
	  common /scs / scsav
	  real*8 scsav(max_sc)
      
