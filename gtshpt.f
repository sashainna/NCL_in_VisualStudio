C*********************************************************************
C*    NAME         :  gtshpt.f
C*       CONTAINS:
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       gtshpt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:11
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtshpt
C*      Retrieve a shape from the unibase & tranform for modsys & units 
C*      as necessary.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey    - Key of shape
C*       OUTPUT :  
C*          p         - Shape.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine gtshpt (p, nclkey)

      include 'com.com'
      include 'wrksys.com'

      real*8 p(1000)
      integer*4 nclkey

      real*8 umx(12)
      data umx / 25.4d0,  0.0d0,  0.0d0,  0.0d0,
     y           0.0d0,   25.4d0, 0.0d0,  0.0d0,
     z           0.0d0,   0.0d0,  25.4d0, 0.0d0/
 
      call gtshap (p, nclkey)
c
c..     working system
c
      if (lwrk) call trnshp (p,invwrk)
c
c..     Units
c
      if (ifl(264).eq.1) call trnshp (p,umx)

999   return
      end
