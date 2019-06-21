c*********************************************************************
c**
c**    NAME         :  pmill.com
c**
c**    CONTAINS:  
c**      The common area for PMILL
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       pmill.com	24.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       09/11/13	13:01:57
c*********************************************************************
 
      integer*2 max_appsf
      parameter (max_appsf=400)

      common/pmlcmi/pavpsf,pavend,pavd,pavbnd,pavstep,pavtype,pavlrpto
      integer*2 pavpsf,pavend,pavd,pavbnd,pavstep,pavtype,pavlrpto

      common/pmlcmr/appsf,apdpl,apbnd,aplasn,aplfr
      real*8 appsf(max_appsf),apdpl(2),apbnd,aplasn,aplfr

	  common/pmlent/pavtran
	  integer*2 pavtran
	  common/pmldir/pavdir
	  integer*2 pavdir
	  common/pmlenr/atrad
	  real*8 atrad