C*********************************************************************
C*    NAME         :  cvp.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cvp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:48
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvp (buf,cbuf)
C*       convert pts on buf to current plotting plan
C*       and return in cbuf
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cvp (buf,cbuf)

      include 'com8a.com'

      real*8 buf(3),cbuf(2)

      integer*2 plan
      equivalence (plan,isc108,sc(108))
      real*4 f1,f2,f3,f4,f5,f6
      equivalence (f1,sc(109))
      equivalence (f2,sc(110))
      equivalence (f3,sc(111))
      equivalence (f4,sc(112))
      equivalence (f5,sc(113))
      equivalence (f6,sc(114))

      cbuf(1)=buf(1)
      cbuf(2)=buf(2)

      if (plan.lt.2) goto 500
      if(plan-3)102,103,104

c                                  ** zx
102       cbuf(2)=-buf(3)
          goto 500
c                                  ** yz
103       cbuf(1)=-buf(3)
          goto 500
c                                  ** xyz
104       cbuf(1)=buf(1)*f1+buf(2)*f2+buf(3)*f3
          cbuf(2)=buf(1)*f4+buf(2)*f5+buf(3)*f6

500   continue

99999 return
      end
