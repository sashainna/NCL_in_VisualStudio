C*********************************************************************
C*    NAME         :  ncvevl.f
C*       CONTAINS:
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ncvevl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:20
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ncvevl(nclkey,u,pt,ve)
C*     Take the NCL curve pointed to by nclkey and return a point and
C*     vector (pt, ve) at the specified u value.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey - Unibase key of curve to evaluate.
C*          u      - u value
C*       OUTPUT :  
C*          pt   - point on curve
C*          ve   - slope vector
C*    RETURNS      : none
C*    SIDE EFFECTS : Calls dcrvpt which uses common area dspcom.
C*    WARNINGS     : none
C********************************************************************/

      subroutine ncvevl (nclkey, u, pt, ve)

      include 'com4a.com'

      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/dspcom/ws(20),w(maxwd)

      real*8 ws,w
      real*4 aws(40), aw(300)
      equivalence (ws,aws),(w,aw)
c
c...subroutine args
c
      integer*4 nclkey
      real*8 u
      real*8 pt(3),ve(3)
c
c...local vars
c
      integer*2 itype
      real*4 s,vt(3)
c
c... initialize variables
c
      s=u
      itype=8
c
c... load curve into common buffer w
c
      call gtgeo(nclkey, w(1))

      ws(1)=0
      aws(39)=0.
      aws(40)=aw(1)
      aw(1)=0.
c
c... get point and vector at s
c... pass real*4 array vt to dcrvpt since thats what it expects...
c
      call dcrvpt (s, itype, pt, vt)
      ve(1)=vt(1)
      ve(2)=vt(2)
      ve(3)=vt(3)

      return
      end
