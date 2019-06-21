C*********************************************************************
C*    NAME         :  crvevl.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       crvevl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:44
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crvevl
c*          Evaluate a NCL curve.
c*                                                               
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey    - key of curve
C*          u         - parameter at which to evaluate
C*       OUTPUT :  
C*          pt        - point at u
C*          vc        - slope at u
C*    RETURNS      : none
C*    SIDE EFFECTS : none 
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine crvevl (nclkey, u, pt, vc)

      include 'com4a.com'

      integer*4 nclkey
      real*4 u
      real*8 pt(3), vc(3)

      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/dspcom/ws(20),w(maxwd)

      real*8 ws,w
      real*4 aws(40), aw(300)
      equivalence (ws,aws),(w,aw)

      real*8 u8, asw
      real*4 vt(3)
      integer*4 nkey
      integer*2 iwf, isf, itype, ksw(4)
      equivalence (asw,ksw)
      logical trflg

      call isitwf (nclkey, iwf)
      if (iwf.eq.0) goto 20
      isf = 3
      call evstup (nclkey, isf)
      u8 = u
      call uevcvt (u8, isf, ifl(72), pt, vc, ifl(2))
      goto 999

20    itype = 8
      call ptdesc(nclkey, itype, asw)
      ksw(3) = 1
      trflg = .true.
      call gtentt (asw, trflg, nkey, itype, w(1))

      ws(1) = 0.
      aws(39) = 0.
      aws(40) = aw(1)
      aw(1) = 0.

      if (u.lt.0.) u = 0.
      if (u.gt.1.) u = 1.

      call dcrvpt (u, itype, pt, vt)

      vc(1) = vt(1)
      vc(2) = vt(2)
      vc(3) = vt(3)

999   return
      end
