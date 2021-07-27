C*********************************************************************
C*    NAME         :  convpt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       convpt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:43
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine convpt(dbuf,cbuf)
C*     to convert x,y & z coordinates in dbuf to x & y coordinates  
C*     using the current plotting plan and return converted data in 
C*     in cbuf.                                                   
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
      subroutine convpt(dbuf,cbuf)
      include 'com8a.com'

      real*8 dbuf(3),cbuf(2)
      real*8 f1,f2,f3,f4,f5,f6
      equivalence (f1,sc(109)),(f2,sc(110)),(f3,sc(111))
      equivalence (f4,sc(112)),(f5,sc(113)),(f6,sc(114))
      integer*2 plan,isc108(4)
      equivalence (plan,isc108,sc(108))

      cbuf(1)=dbuf(1)
      cbuf(2)=dbuf(2)
      if (plan.gt.1) then
        if (plan.eq.2) then
          cbuf(2)=-dbuf(3)
        else if (plan.eq.3) then
          cbuf(1)=-dbuf(3)
        else if (plan.eq.4) then
          cbuf(1)=dbuf(1)*f1 + dbuf(2)*f2 + dbuf(3)*f3
          cbuf(2)=dbuf(1)*f4 + dbuf(2)*f5 + dbuf(3)*f6
        endif
      endif

      if (debug) then
        write(cout,9000) dbuf(1),dbuf(2),dbuf(3),cbuf(1),cbuf(2)
9000    format('convpt: dbuf'3f7.3' cbuf'2f7.3)
        call putmsg(cout,80,23,0)
      endif

99999 return
      end
c *********************************************************************
