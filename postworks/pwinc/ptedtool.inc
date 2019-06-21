c     File: ptedtool.inc
c
c     COPYRIGHT 2006 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			ptedtool.inc , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:58:25
c        
      integer*4 MAXTL
      parameter (MAXTL=120)
c
      common /KPTOOL/ ITPX  , NTOOL , ICTYPE, NCUTR , ISHKFL
c
      integer*4 ITPX,NTOOL,ICTYPE(MAXTL,3),NCUTR(MAXTL),ISHKFL(MAXTL)
c
      common /GPTOOL/ CUTR  , CUPARM, TLNUM , TLLEN
c
      real*8 CUTR(MAXTL,7),CUPARM(MAXTL,4,3),TLNUM(MAXTL),TLLEN(MAXTL)
c
      common /CPTOOL/ CUSYM
c
      character*80 CUSYM(MAXTL,3)
