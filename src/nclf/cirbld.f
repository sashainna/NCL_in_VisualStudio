C*********************************************************************
C*    NAME         :  cirbld.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       cirbld.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cirbld
c*          Complete circle data for circpt in for surf definitions.
C*    PARAMETERS   
C*       INPUT  : 
C*          w         - circle data
C*       OUTPUT :  
C*          w(12:14)  - forward vector
C*          w(15)     - angle circle subtends
C*    RETURNS      : none
C*    SIDE EFFECTS : if circle is unbounded, dummy ok pl stored in w(8:11)
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cirbld (w)

      include 'com8a.com'

      real*8 w(15)

      real*8 sec, cosphi
c
      if (dabs(w(8)).gt.0..or.dabs(w(9)).gt.0..or.dabs(w(10)).gt.0.)
     x    goto 100
c
c... Circle is unbounded. Create ok plane that cuts circle a little.
c
      if (dabs(w(5)).gt..9999.or.dabs(w(6)).gt..9999) then
        w(8)=-1.
        if (w(5).lt.-.999.or.w(6).lt.-.999) w(8)=1.
        goto 60
      endif
      if (dabs(w(4)).gt..9999) then
        w(9)=-1.
        if (w(4).lt.0.) w(9)=1.
        goto 60
      endif
      if (w(4).lt..9.and.w(4).ge.0.) then
        w(8)=-1.
      else if (w(4).gt.-.9) then
        w(8)=1.
      else if (w(4).lt.0.) then
        w(9)=1.
      else
        w(9)=-1.
      endif
      w(12)=w(5)*w(10)-w(6)*w(9)
      w(13)=w(6)*w(8)-w(4)*w(10)
      w(14)=w(4)*w(9)-w(5)*w(8)
      w(8)=w(13)*w(6)-w(14)*w(5)
      w(9)=w(14)*w(4)-w(12)*w(6)
      w(10)=w(12)*w(5)-w(13)*w(4)
      sec=dsqrt(w(8)**2+w(9)**2+w(10)**2)
      if (sec.eq.0.)sec=1.
      w(8)=w(8)/sec
      w(9)=w(9)/sec
      w(10)=w(10)/sec

60    w(11)=w(8)*w(1)+w(9)*w(2)+w(10)*w(3)-w(7)

100   w(12)=w(10)*w(5)-w(9)*w(6)
      w(13)=w(8)*w(6)-w(10)*w(4)
      w(14)=w(9)*w(4)-w(8)*w(5)

      cosphi=(w(11)-w(8)*w(1)-w(9)*w(2)-w(10)*w(3))/w(7)
      if (cosphi.gt.1.d0) cosphi=1.d0
      if (cosphi.lt.-1.d0) cosphi=-1.d0
      w(15)=acos(cosphi)

999   return
      end
