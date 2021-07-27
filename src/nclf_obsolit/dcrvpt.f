C*********************************************************************
C*    NAME         :  dcrvpt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dcrvpt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:51
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dcrvpt (sss, dtype)
C*    calculate a point at any percentage along a curve or circle
C*                                                              
C*     sss has the input location on the curve (percentage along
C*        curve or circle                                      
C*    PARAMETERS   
C*       INPUT  : 
C*          sss           percent along curve
C*          dtype         curve type
C*       OUTPUT :  
C*          p             point at sss
C*          vs            slope at sss
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine dcrvpt (sss, dtype,p,vs)

      include 'com8a.com'
c      include '../incf/dspcom.com'

      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

      common/dspcom/ws(20),w(maxwd)

      real*8 w,ws
      real*4 aw(300),aws(40)
      equivalence (w,aw),(ws,aws)

      real*4 sss
      real*8 p(3)
      real*4 vs(3)
      integer*2 dtype

      real*4 h(5),xe,ye,ze,ro,ai,u,ca,cb,cc,x1,y1,z1
      real*4 x2,y2,z2,den,v

c          make sure 0>sss>1 and sss(1)=0.
      if(sss.lt.0.)sss=0.
      if(sss.gt.1.)sss=1.
      iwx=0

c                                                  **** circle
      if (dtype.eq.8) goto 4
      i=0
      alf=w(iwx+15)*(2.*sss-1.)
      si=dsin(alf)
      co=dcos(alf)
      sec=dsqrt(si**2+co**2)
      si=si/sec
      co=co/sec
      xc=co*w(i+7)
      yc=si*w(i+7)
      p(1)=w(i+8)*xc+w(i+12)*yc+w(i+1)
      p(2)=w(i+9)*xc+w(i+13)*yc+w(i+2)
      p(3)=w(i+10)*xc+w(i+14)*yc+w(i+3)
      j=2*i
      vs(1)=-w(i+8)*si+w(i+12)*co
      vs(2)=-w(i+9)*si+w(i+13)*co
      vs(3)=-w(i+10)*si+w(i+14)*co
      goto 99

c                                                  **** curve
4     jwx=0
      j=0
      nmp=aws(40)

c          if active seg, go try it.
9     if(aws(30).eq.0)goto 10
      if(aws(j+2).eq.0..or.aws(j+3).eq.0.)goto 10
      if(sss.lt.aws(j+1).or.sss.gt.aws(j+2))goto 10
      goto 80

c          scan for seg
10    do 12 i=2,nmp
12    if(aw(i+jwx).ge.sss)goto 20
20    continue
c          save active seg no. in aws(39)
      aws(39)=i-1
c          set ptrs and activate seg ( i pts to wanted seg + 1 )
      ix=iwx+(nmp+1)/2+6*(i-2)
      jx=2*ix
      ro=aw(jx+12)
      aws(j+13)=w(ix+7)-w(ix+1)
      aws(j+14)=w(ix+8)-w(ix+2)
      aws(j+15)=w(ix+9)-w(ix+3)
      aws(j+10)=aws(j+13)-ro*aw(jx+19)
      aws(j+11)=aws(j+14)-ro*aw(jx+20)
      aws(j+12)=aws(j+15)-ro*aw(jx+21)
      aws(j+7)=aw(jx+7)
      aws(j+8)=aw(jx+8)
      aws(j+9)=aw(jx+9)
      aws(j+1)=aw(jwx+i-1)
      aws(j+2)=aw(jwx+i)
      aws(j+3)=aws(j+2)-aws(j+1)
      aws(j+4)=aw(jx+10)
      aws(j+5)=aw(jx+11)
      aws(j+6)=1.-aws(j+4)-aws(j+5)
cccccccccccccccccccccccccccccccccccc
c          added correction for sss-calc  (in aw(j+16)  6-17-81
      do 50 i=1,5
      ai=i
      u=(ai-1.)/4.
      ca=(1.-u)**2
      cb=2.*u*(1.-u)
      cc=u**2
c          point b1
      x1=cb*aws(j+7)+cc*aws(j+10)
      y1=cb*aws(j+8)+cc*aws(j+11)
      z1=cb*aws(j+9)+cc*aws(j+12)
c          b2 deltas
      x2=ca*aws(j+7)+cb*aws(j+10)+cc*aws(j+13)-x1
      y2=ca*aws(j+8)+cb*aws(j+11)+cc*aws(j+14)-y1
      z2=ca*aws(j+9)+cb*aws(j+12)+cc*aws(j+15)-z1
50    h(i)=sqrt(x2**2+y2**2+z2**2)
c              calc proportional arc dis at u=.5
      den=h(1)+h(5)+2.*h(3)+4.*(h(2)+h(4))
      if (den.ne.0.) then
          v=(h(1)+h(3)+4.*h(2))/den
          u=v*(aws(j+4)+v*(aws(j+5)+v*aws(j+6)))
          aws(j+16)=(.5-u)/(v*(1.-v))**2
      endif
c          end of correction
cccccccccccccccccccccccccccccccccc
c          now pt(i)
      xe=w(ix+1)
      ye=w(ix+2)
      ze=w(ix+3)

c ***********************************  solve sss in active segment
c          solve u=f(sss)
80    v=(sss-aws(j+1))/aws(j+3)
      u=v*(aws(j+4)+v*(aws(j+5)+v*aws(j+6)))
ccccccccccc        u+correction    6-17-81
      u=u+aws(j+16)*(v*(1.-v))**2
      ca=(1.-u)**2
      cb=2.*u*(1.-u)
      cc=u**2
c          point b1
      x1=cb*aws(j+7)+cc*aws(j+10)
      y1=cb*aws(j+8)+cc*aws(j+11)
      z1=cb*aws(j+9)+cc*aws(j+12)
c          b2 deltas
      x2=ca*aws(j+7)+cb*aws(j+10)+cc*aws(j+13)-x1
      y2=ca*aws(j+8)+cb*aws(j+11)+cc*aws(j+14)-y1
      z2=ca*aws(j+9)+cb*aws(j+12)+cc*aws(j+15)-z1
90    p(1)=x1+u*x2+xe
      p(2)=y1+u*y2+ye
      p(3)=z1+u*z2+ze
      vs(1)=x2
      vs(2)=y2
      vs(3)=z2
99    return
      end
