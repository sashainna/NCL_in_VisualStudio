C*********************************************************************
C*    NAME         :  crvpnt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       crvpnt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:45
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     :
C*     calculate a point and alternately a   
C*     point and the slope on a curve.      
C*                                         
C*     if itsk =            then the routine does 
C*         0           calculates point at s-value 
C*         1           calculates point and slope  
C*         2           calculates point and slope on curve for ept
C*                      in w(iex)                                
C*                                                              
C*     s has the input location on the curve and the return value if
C*        if itsk=2                                                 
C*      nmp = number of points on the curve                        
C*      data is returned in s(iwx+142)...                         
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
c **********************************************************************
c **********************************************************************

      subroutine crvpnt (s,iwx,iex,itsk)

      include 'com8a.com'

      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))
      common/wblok/w(600)

      real*4 aw(1200),h(5),xe,ye,ze,dis,ro,ai,u,ca,cb,cc,x1,y1,z1
      real*4 x2,y2,z2,den,v,ds,ctan,uerr,oerr,tl,s
      real*8 w
      equivalence (w,aw)
      logical noeras
      integer*2 ient(4)
      equivalence (sc(53),ient)

      noeras=.false.
c          make sure 0>s>1 and s(1)=0.
      if(s.lt.0.)s=0.
      if(s.gt.1.)s=1.
c          if circle, go cirpnt route
      jentx=iwx/(maxwd+20)+1
      if(ient(jentx).ne.7)goto 4
      call cirpnt(s,iwx,iex,itsk)
      goto 99
4     jwx=2*iwx
      j=jwx+maxwd*2
      itx=iwx+maxwd
      nmp=aw(j+40)
c          if itsk=2. get ept and scan for seg
      if(itsk.ne.2)goto 9
      xe=w(iex+1)
      ye=w(iex+2)
      ze=w(iex+3)
      ix=iwx+(nmp+1)/2-6
      do 5 i=1,nmp
      ix=ix+6
      kx=2*ix+6
c          find pt/vec on curve past ept
      dis=aw(kx+1)*(xe-w(ix+1))+aw(kx+2)*(ye-w(ix+2))+
     1   aw(kx+3)*(ze-w(ix+3))
cuni      write (cout,1010) i,w(ix+1),w(ix+2),w(ix+3),dis
cuni1010  format(' segsch: 'i4,3f14.7)
cuni      call putmsg (cout,60,i,noeras)
      if(dis.le.0.)goto 7
5     continue
c          error - point is not on curve
6     ifl(2)=132
      goto 99
c          containing seg found. if now active, go do crvpnt
7     iacseg=aw(j+39)
c          min i is 2
      if(i.eq.1.and.dis.lt.-.001)goto 6
      if(i.lt.2)i=2
      if(iacseg.eq.i-1)goto 78
      goto 20
c          if active seg, go try it.
9     if(aw(j+39).eq.0)goto 10
      if(aw(j+2).eq.0..or.aw(j+3).eq.0.)goto 10
      if(s.lt.aw(j+1).or.s.gt.aw(j+2))goto 10
      goto 80
c          scan for seg
10    do 12 i=2,nmp
      if(aw(i+jwx).ge.s)goto 20
12    continue
c          set ptrs and activate seg ( i pts to wanted seg + 1 )
20    ix=iwx+(nmp+1)/2+6*(i-2)
      jx=2*ix
      ro=aw(jx+12)
      aw(j+13)=w(ix+7)-w(ix+1)
      aw(j+14)=w(ix+8)-w(ix+2)
      aw(j+15)=w(ix+9)-w(ix+3)
      aw(j+10)=aw(j+13)-ro*aw(jx+19)
      aw(j+11)=aw(j+14)-ro*aw(jx+20)
      aw(j+12)=aw(j+15)-ro*aw(jx+21)
      aw(j+7)=aw(jx+7)
      aw(j+8)=aw(jx+8)
      aw(j+9)=aw(jx+9)
      aw(j+1)=aw(jwx+i-1)
      aw(j+2)=aw(jwx+i)
      aw(j+3)=aw(j+2)-aw(j+1)
      aw(j+4)=aw(jx+10)
      aw(j+5)=aw(jx+11)
      aw(j+6)=1.-aw(j+4)-aw(j+5)
cccccccccccccccccccccccccccccccccccc
c          added correction for s-calc  (in aw(j+16)  6-17-81
      do 50 i=1,5
      ai=i
      u=(ai-1.)/4.
      ca=(1.-u)**2
      cb=2.*u*(1.-u)
      cc=u**2
c          point b1
      x1=cb*aw(j+7)+cc*aw(j+10)
      y1=cb*aw(j+8)+cc*aw(j+11)
      z1=cb*aw(j+9)+cc*aw(j+12)
c          b2 deltas
      x2=ca*aw(j+7)+cb*aw(j+10)+cc*aw(j+13)-x1
      y2=ca*aw(j+8)+cb*aw(j+11)+cc*aw(j+14)-y1
      z2=ca*aw(j+9)+cb*aw(j+12)+cc*aw(j+15)-z1
50    h(i)=sqrt(x2**2+y2**2+z2**2)
c              calc proportional arc dis at u=.5
      den=h(1)+h(5)+2.*h(3)+4.*(h(2)+h(4))
      v=(h(1)+h(3)+4.*h(2))/den
      u=v*(aw(j+4)+v*(aw(j+5)+v*aw(j+6)))
      aw(j+16)=(.5-u)/(v*(1.-v))**2
c          end of correction
cccccccccccccccccccccccccccccccccc
c          now pt(i)
      w(itx+9)=w(ix+1)
      w(itx+10)=w(ix+2)
      w(itx+11)=w(ix+3)
c          save active seg no. in aw(jwx+299)
      aw(j+39)=i-1
c          if itsk2, init for s-iter and do ept deltas
      if(itsk.ne.2)goto 80
78    s=(aw(j+1)+aw(j+2))/2.
      xe=w(iex+1)-w(itx+9)
      ye=w(iex+2)-w(itx+10)
      ze=w(iex+3)-w(itx+11)
      ds=.01
      itim=0
      ctan=.5
c ***********************************  solve s in active segment
c          solve u=f(s)
80    v=(s-aw(j+1))/aw(j+3)
      u=v*(aw(j+4)+v*(aw(j+5)+v*aw(j+6)))
ccccccccccc        u+correction    6-17-81
      u=u+aw(j+16)*(v*(1.-v))**2
      ca=(1.-u)**2
      cb=2.*u*(1.-u)
      cc=u**2
c          point b1
      x1=cb*aw(j+7)+cc*aw(j+10)
      y1=cb*aw(j+8)+cc*aw(j+11)
      z1=cb*aw(j+9)+cc*aw(j+12)
c          b2 deltas
      x2=ca*aw(j+7)+cb*aw(j+10)+cc*aw(j+13)-x1
      y2=ca*aw(j+8)+cb*aw(j+11)+cc*aw(j+14)-y1
      z2=ca*aw(j+9)+cb*aw(j+12)+cc*aw(j+15)-z1
      if(itsk.ne.2)goto 90
      uerr=(x2*(xe-x1)+y2*(ye-y1)+z2*(ze-z1))/(x2**2+y2**2+z2**2)-u
      if(abs(uerr).lt.1.e-5)goto 90
      if(itim.eq.0)goto 88
      if(abs(oerr-uerr).lt.1.e-5)goto 87
      ctan=ds/(oerr-uerr)
87    ds=uerr*ctan
88    itim=itim+1
cuni      write (cout,1020)itim,s,u,uerr
cuni1020  format(' crvpnt itsk2: 'i6,3f13.7)
cuni      call putmsg (cout,70,itim,noeras)
      s=s+ds
      oerr=uerr
      if(itim.lt.15)goto 80
      goto 6
90    w(itx+12)=x1+u*x2+w(itx+9)
      w(itx+13)=y1+u*y2+w(itx+10)
      w(itx+14)=z1+u*z2+w(itx+11)
c          if itsk=1, do vector
      if(itsk.ne.1)goto 99
      tl=sqrt(x2**2+y2**2+z2**2)
      aw(j+29)=x2/tl
      aw(j+30)=y2/tl
      aw(j+31)=z2/tl

99    return
      end
