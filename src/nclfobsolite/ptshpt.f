C*********************************************************************
C*    NAME         :  ptshpt.f
C*       CONTAINS:
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       ptshpt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:31
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptshpt
C*      Transform and store a shape.
C*    PARAMETERS   
C*       INPUT  : 
C*          p         - Array holding shape.
C*       OUTPUT :  
C*          p         - Transformed shape.
C*          nclkey    - Key of stored shape
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ptshpt (p, nclkey)

      include 'com.com'
      include 'wrksys.com'

      real*8 p(144)
      integer*4 nclkey

      real*8 umx(12)
      data umx / 0.0d0,  0.0d0, 0.0d0, 0.0d0,
     y           0.0d0,  0.0d0, 0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 0.0d0, 0.0d0/

      real*8 asn
      integer*2 ksn(4),nwds
      equivalence (asn,ksn)
 
      asn = p(1)
      nwds = ksn(3)
c
c...    Metric
c
      if (ifl(264).eq.1) then
        umx(1)  = 1.0d0/25.4d0
        umx(6)  = 1.0d0/25.4d0
        umx(11) = 1.0d0/25.4d0
        call trnshp (p,umx)
      endif
c
c..     working system
c
      if (lwrk) call trnshp (p,wrkmx)
c
c...    Store shape
c
      call ptshap (p, nclkey, nwds)

999   return
      end
