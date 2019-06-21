c*********************************************************************
c**
c**    NAME         :  gidcom.com
c**
c**    CONTAINS:  
c**      The common area for Guide Curves.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       gidcom.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:31
c*********************************************************************
c
      integer*2 N_IGIDCM,N_RGIDCM,N_GGIDCM
c
      PARAMETER (N_IGIDCM = 5)
      PARAMETER (N_RGIDCM = 15)
      PARAMETER (N_GGIDCM = 5)
c
      common /igidcm/ IGIDCM
      integer*2 IGIDCM(N_IGIDCM)
c
      equivalence (GIDSID,IGIDCM(001)), (GTHKON,IGIDCM(002))
      equivalence (GIDUON,IGIDCM(003))
c
      integer*2 GIDSID,GTHKON,GIDUON
c
      common /rgidcm/ RGIDCM
      real*4 RGIDCM(N_RGIDCM)
c
      equivalence (GIDUIA,RGIDCM(001)), (GIDHIA,RGIDCM(002))
      equivalence (GIDU  ,RGIDCM(003)), (GIDH  ,RGIDCM(004))
      equivalence (GIDPT ,RGIDCM(005)), (GDPTIA,RGIDCM(008))
c
      real*4 GIDUIA,GIDHIA,GIDU,GIDH,GIDPT(3),GDPTIA(3)
c
      common /ggidcm/ GGIDCM
      real*8 GGIDCM(N_GGIDCM)
c
      equivalence (GIDASW,GGIDCM(001)), (GIDTHK,GGIDCM(002))
      equivalence (GIDU0 ,GGIDCM(003))
c
      real*8 GIDASW,GIDTHK,GIDU0
