c**
C*********************************************************************
C*    NAME         :  circpt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       circpt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine circpt(s,iwx,iex,itsk)
C*       this routine called by crvpnt when entity is a circle
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
      subroutine circpt(s,iwx,iex,itsk)

      include 'com8a.com'

      common/wblok/w(600)

      integer*2 ient(4)
      real*4 aw(1200),s,alf
      equivalence (w,aw),(sc(53),ient)

c          itsk=2 not allowed. (see crvpnt for meaning)
      if(itsk.lt.2)goto 20
c          error. itsk=2 not included.
      ifl(2)=5
      goto 99
20    i=iwx
      alf=w(iwx+15)*(2.*s-1.)
c          sin,cos are real*4 to keep dsin,dcos utils out of root seg.
c          unitize for accuracy where radius is large.
      si=sin(alf)
      co=cos(alf)
      sec=dsqrt(si**2+co**2)
      si=si/sec
      co=co/sec
      xc=co*w(i+7)
      yc=si*w(i+7)
c     zc=0.
      w(i+142)=w(i+8)*xc+w(i+12)*yc+w(i+1)
      w(i+143)=w(i+9)*xc+w(i+13)*yc+w(i+2)
      w(i+144)=w(i+10)*xc+w(i+14)*yc+w(i+3)
      j=2*i
      aw(j+289)=-w(i+8)*si+w(i+12)*co
      aw(j+290)=-w(i+9)*si+w(i+13)*co
      aw(j+291)=-w(i+10)*si+w(i+14)*co

99    return
      end
