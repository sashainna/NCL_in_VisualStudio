c*********************************************************************
c**
c**    NAME         :  comgt.com
c**
c**    CONTAINS:
c**     FEDRAT/AT variables.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       comgt.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:30
c*********************************************************************
c
      integer*2 N_MOTMAP
c
      parameter (N_MOTMAP = 200)
c
      common /NCLMAP/ MOTMAP
      real*8 MOTMAP(200)
c
      real*8 FROMSV(21),FEEDC(6),FEEDS(6),FEDIS(4),FHIGT
c
      equivalence (FROMSV,MOTMAP(31)), (FEEDC,MOTMAP(7))
      equivalence (FEEDS,MOTMAP(13)), (FEDIS,MOTMAP(19))
      equivalence (FHIGT,MOTMAP(23))
c NOTE:
c      MOTMAP(24) eqivalenced to FEEDR
c      MOTMAP(25) eqivalenced to RADIAN ( not used )
c      MOTMAP(26) eqivalenced to PI     ( not useful )
