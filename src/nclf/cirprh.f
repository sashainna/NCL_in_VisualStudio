C*********************************************************************
C*    NAME         :  cirprh
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cirprh.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cirprh
C*          this routine handles circle cases 7,8,9,,,,
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
      subroutine cirprh

      include 'com8a.com'

      common/wblok/w(600)

      real*8 u(11),v(11)
      integer*2 ksn(4)
      integer*4 nclkey
      integer*2 ietype
      logical trflg
      equivalence (asn,ksn),(w(20),u),(w(40),v)

c          zero w(4-11)      ( these are all xy view circles )
      do 20 i=4,11
20    w(i)=0.
c          get subtype and branch
      trflg = .true.
      asn=sc(10)
      isub=ksn(2)
      if(isub-8)70,80,89
c******************************  circle/center,pt1,pt2            -- 7 --
c              get pt1 in w(1), pt2 in u(1)
70    continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
c              set k = 1. (in i,j,k),  z=0.,  and calc radius.
      w(7)=dsqrt((w(1)-u(1))**2+(w(2)-u(2))**2)
75    w(4)=0.
      w(5)=0.
      w(6)=1.
      goto 999
c******************************  circle/center,pt1,tanto,ln1      -- 8 --
c              get pt1 in w(1), ln1 in u(1)
80    continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
      sec=dsqrt(u(4)**2+u(5)**2)
      if(sec.gt.1.d-6) goto 84
c          error.  line points in z-direction
      ifl(2)=163
      goto 990
84    w(7)=  dabs(  (u(5)*(u(1)-w(1))+u(4)*(w(2)-u(2)))/sec  )
      goto 75
c
89    if(isub-10)90,100,109
c***************************  circle/center,pt1,xl,tanto,ci1      -- 9 --
90    continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(13), trflg, nclkey, ietype, u(1))
c              input circle must be in xy-plane
      if(dabs(u(4)).lt.1.d-3.and.dabs(u(5)).lt.1.d-3)goto 94
c          error.  ci1 is tipped from xy-plane
      ifl(2)=161
      goto 990
94    dis=dsqrt((w(1)-u(1))**2+(w(2)-u(2))**2)
      if(sc(12).eq.663.) dis=-dis
      w(7)=dabs(u(7)+dis)
      goto 75 
c**************************  ci/xyls,pt1,pt2,ra,r               -- 10 --
100   continue
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
      call gtentt(sc(13), trflg, nclkey, ietype, v(1))
      dx=v(1)-u(1)
      dy=v(2)-u(2)
      sec=dsqrt(dx**2+dy**2)
c                 dist bet pts must be real and not .gt. 2*radius
      if(sec.gt.2.*sc(14))goto 1005
      if(sec.gt.0.)goto 101
c                 error.  coincident pts.
1005  ifl(2)=163
      goto 990
c
c..... Added by Eduard: Error message changed for the case
c..... when the direction modifier does not determine which 
c..... circle to choose
c
1006  ifl(2)=258
      goto 990
101   a=-dy/sec
      b=dx/sec
      if(sc(11).ne.638.)goto 102
      if(a)106,1006,107
102   if(sc(11).ne.641.)goto 103
      if(a)107,1006,106
103   if(sc(11).ne.639.)goto 104
      if(b)106,1006,107
104   if(sc(11).eq.642.)goto 105
c                  error. modifier xyls reqd.
      ifl(2)=12
      goto 990
105   if(b)107,1006,106
c                 flip nrm
106   a=-a
      b=-b
107   continue
      dis=dsqrt(sc(14)**2-sec**2/4.)
      w(1)=(u(1)+v(1))/2.+a*dis
      w(2)=(u(2)+v(2))/2.+b*dis
      w(3)=0.
      w(7)=sc(14)
      goto 75
109   if(isub-12)110,120,129
c*******************************  ci/center,pt1,radius,r        -- 11 --
110   continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
c     w(6)=1.
      w(7)=sc(12)
      if(w(7).gt..0005) goto 75
c                  error.  radius must be real
112   ifl(2)=165
      goto 990
c*******************************  ci/tanto,ln1,xyls,pt1,radius,r  -- 12 --
120   if(sc(14).lt..001)goto 112
      call gtentt(sc(11), trflg, nclkey, ietype, u(1))
      call gtentt(sc(13), trflg, nclkey, ietype, v(1))
c                  unitize ln vec
      sec=dsqrt(u(4)**2+u(5)**2)
      if(sec.gt.0.)goto 122
c                  error. ln has no x,y extent  or some other error.
121   ifl(2)=163
      goto 990
122   u(4)=u(4)/sec
      u(5)=u(5)/sec
      a=-u(5)
      b=u(4)
      dis=a*(v(1)-u(1))+b*(v(2)-u(2))
      if(dis)123,121,124
123   a=-a
      b=-b
      dis=-dis
124   offsq=sc(14)**2-(sc(14)-dis)**2
      if(offsq.lt.-sc(27)**2)goto 121
      if(offsq.lt.0.) offsq = 0.
      off=dsqrt(offsq)
      if(sc(12).ne.638.)goto 1242
      if(u(4))125,121,126
1242  if(sc(12).ne.641.)goto 1244
      if(u(4))126,121,125
1244  if(sc(12).ne.639.)goto 1246
      if(u(5))125,121,126
1246  if(sc(12).ne.642.)goto 121
      if(u(5))126,121,125
125   off=-off
126   h=sc(14)-dis
      w(1)=v(1)+u(4)*off+a*h
      w(2)=v(2)+u(5)*off+b*h
      w(3)=0.
c     w(6)=1.
      w(7)=sc(14)
      goto 75
129   continue
c          error exit.
990   if(ifl(2).lt.1)ifl(2)=5
c
999   return
      end
