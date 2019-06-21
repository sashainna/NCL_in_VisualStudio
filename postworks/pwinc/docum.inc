c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        docum.inc , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:24
c
      common /kdoc  / IDOCDS, MAXDOC, DCNC  , NCHED , DLIN  , DPAG  ,
     1                NDHED , IGDES , NCGDES, IGID  , IDOCNC, NCPAG ,
     2                NDOCBF, NCFN
c
      integer*4 IDOCDS(128),MAXDOC,DCNC(700),NCHED(12),DLIN,DPAG,NDHED,
     1          IGDES,NCGDES(200),IGID(200),IDOCNC(20),NCPAG,NDOCBF,
     2          NCFN
c
      common /gdoc  / RGVAL
c
      real*8 RGVAL(200)
c
      common /cdoc  / DCLAB , DCPRM , DHED  , LGDES , LDOCBF, LDPAG,
     1                DCFNAM
c
      character*8 DCLAB(700)
      character*80 DHED(12),DCPRM(700),LGDES(200),LDOCBF(20),LDPAG
      character*(MAX_PATH) DCFNAM
