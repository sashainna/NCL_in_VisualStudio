C*********************************************************************
C*    NAME         :  crvpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       crvpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:45
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crvpre (inv,nmp,icirc)
C*       builds the curve segments using            
C*       x,y,z,a,b,c etc. data from slpset.        
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
      subroutine crvpre (inv,nmp,icirc)

      include 'com8a.com'

      parameter (maxpt=50)
      parameter (maxptx=1000)

      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch

c      common/pblok/p
c     equivalence (p(1),x(1)),    (p(21),y(1)),   (p(41),z(1))
c     equivalence (p(61),a(1)),   (p(71),b(1)),   (p(81),c(1))
c     equivalence (p(91),dx(1)),  (p(101),dy(1)), (p(111),dz(1))
c     equivalence (p(121),ch(1))
c
      real*8 x(maxptx),y(maxptx),z(maxptx)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx)
      real*4 dy(maxptx),dz(maxptx),ch(maxptx)
      integer*2 inv(20)

cccccccccccccccccccc   rid the circle from crvpre   7-28-81
cccc       if this is circle, insert pts as nec for intol
cccc      if(icirc.eq.0)goto 10
cccc      call circhk(x,y,z,a,b,c,dx,dy,dz,ch,inv,nmp,icirc)
c          convert each segment to bezier cubic polgon form
10    n=nmp
      m=n-1
      RQ=X(6)*4./3.
      SIX9S=.999999
      arcsum=0.
      do 30 i=1,m
      j=i+1
      cal=(a(i)*dx(i)+b(i)*dy(i)+c(i)*dz(i))/ch(i)
      cbe=(a(j)*dx(i)+b(j)*dy(i)+c(j)*dz(i))/ch(i)
CF28         CHECK FOR CONIC (SUBTYP=6)
      IF(ISC10(2).EQ.6)GOTO 12
      adis=.666667*ch(i)/(1.+cal)
      bdis=.666667*ch(i)/(1.+cbe)
c          suppress the larger for more pleasing appearance in
c          extreme cases ( which are doubtful anyway ).
      if(adis.gt.bdis)adis=bdis*(2.-bdis/adis)
      if(bdis.gt.adis)bdis=adis*(2.-adis/bdis)
      GOTO 14
CF28       SPECIAL ADIS, BDIS CALC FOR CONICS      28-FEB-89
12    IF(CAL.GT.SIX9S)CAL=SIX9S
      IF(CBE.GT.SIX9S)CBE=SIX9S
      SAL=SQRT(1.-CAL**2)
      SBE=SQRT(1.-CBE**2)
      DEN=SBE*CAL+CBE*SAL
      BDIS=CH(I)*SAL*RQ/DEN
      ADIS=CH(I)*SBE*RQ/DEN
14    CONTINUE
c          convert a,b,c to pt'q' deltas.
      a(i)=a(i)*adis
      b(i)=b(i)*adis
      c(i)=c(i)*adis
c          if not seg1, go back and put prev ro in ch.
      if(i.eq.1)goto 20
      ch(i-1)=obdis/adis
20    obdis=bdis
c          do preliminary s,ds/du,ds/du work
c          set pt'r' deltas
      dxr=dx(i)-a(j)*bdis
      dyr=dy(i)-b(j)*bdis
      dzr=dz(i)-c(j)*bdis
c          set b1,b2 deltas at u=.5 ( actually 4*size )
      dxc=dxr+dx(i)-a(i)
      dyc=dyr+dy(i)-b(i)
      dzc=dzr+dz(i)-c(i)
      cdis=sqrt(dxc**2+dyc**2+dzc**2)
c          this arclength is approx. ( improve in future? )
      ro=1.62*(adis+bdis)/cdis-.81
      arcl=(.5-ro)*(adis+bdis)+(.5+.5*ro)*cdis
      arcsum=arcsum+arcl
c          hold full scale s, ds/du(0), ds/du(1)
      dx(i)=arcl
      dy(i)=3.*adis
      dz(i)=3.*bdis
30    continue
c          finup on last seg
      ch(m)=1.
      a(n)=a(n)*bdis
      b(n)=b(n)*bdis
      c(n)=c(n)*bdis
c          polgons are done. do final work on s, etc.
      do 40 i=1,m
      dx(i)=dx(i)/arcsum
      hdx=dx(i)
      if(i.eq.1)goto 35
      dx(i)=dx(i)+dx(i-1)
c          note the switch from ds/du to du/ds, conv to unit form,
c          and dz becomes 'b' in u=a*v+b*v**2+c*v**3
35    dy(i)=hdx*arcsum/dy(i)
      dz(i)=hdx*arcsum/dz(i)
40    dz(i)=3.-2.*dy(i)-dz(i)
c          dn-shift s-values, put nmp in dx(1) and 1. in dx(nmp)
      j=n+1
      do 50 i=2,n
      j=j-1
50    dx(j)=dx(j-1)
      dx(1)=nmp
      dx(n)=1.

99    return
      end
