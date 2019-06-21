c*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL 
C*       regdir.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:35
c**
c*****************************************************
C****  FILE NAME: REGDIR               30-NOV-88            PEM
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C             THIS ROUTINE BUILDS THE STEP AND FORWARD DIRECTION VECTORS
C             FOR THE MILLING REGION AND DETERMAINS THE TOOL RELATIONS OF
C             FOUR SIDES OF THE REGION AS DRIVE SURFACES.
C
C*************************************************************************


      subroutine regdir
       
c----------------------------------  reg.com ------------------
       
       
      include 'com4a.com'
      include 'mocom.com'

      real*8 asn,pla(4),plb(4),pt(3),pl(4)
      real*8 pf(3),st(3),tpin(6),tp1(6),tp2(6),tp3(6),tp4(6)
      real*8 sp1(7),sp2(7),apla,aplb,acsa,acsb,aps
      real*8 f1,f2,f3
      real*4 bsn(2)
      integer*2 mpla,mplb,mcsa,mcsb,lt12,lt34,lt13,lt24
      integer*2 kpla(4),kplb(4),kcsa(4),kcsb(4)
      equivalence (kpla,apla),(kplb,aplb),(kcsa,acsa),(kcsb,acsb)

      common/regblk/pl,pt,pla,plb,aps,apla,aplb,acsa,acsb,rho,dro
     1 ,pf,st,tpin,tp1,tp2,tp3,tp4,sp1,sp2,f1,f2,f3,step,istep
     2 ,mpla,mplb,mcsa,mcsb,lt12,lt34,lt13,lt24,lseg

      real*8 aclp,artp
      real*4 ad(300),asc(100)
      integer*2 jd(600),ksn(4),ksc(100)
      integer*2 kclp(4),krtp(4),lseg(2)
      equivalence (sc,asc,ksc),(asc(63),sbe),(asc(64),cbe),(asn,bsn,ksn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(aclp,kclp),(aclp,cdis),(artp,krtp),(artp,rdis)

      real*8 sec
      real*4 v1(3),v2(3),v3(3)

c                            step fwd is pla to plb
      st(1)=pla(1)+plb(1)
      st(2)=pla(2)+plb(2)
      st(3)=pla(3)+plb(3)
      sec=dsqrt(st(1)**2+st(2)**2+st(3)**2)
      if(sec.eq.0.d0)go to 999
      st(1)=st(1)/sec
      st(2)=st(2)/sec
      st(3)=st(3)/sec

c                            pasfwd is perpto ta and step fwd
c                            and same genl direc as p1 to p2
      pf(1)=st(2)*tpin(6)-st(3)*tpin(5)
      pf(2)=st(3)*tpin(4)-st(1)*tpin(6)
      pf(3)=st(1)*tpin(5)-st(2)*tpin(4)
      dis=pf(1)*(tp2(1)-tp1(1))+pf(2)*(tp2(2)-tp1(2))
     1   +pf(3)*(tp2(3)-tp1(3))
      sec=dsqrt(pf(1)**2+pf(2)**2+pf(3)**2)
      if(sec.eq.0.d0)go to 999
      if(dis.lt.0.0)sec=-sec
      pf(1)=pf(1)/sec
      pf(2)=pf(2)/sec
      pf(3)=pf(3)/sec

c                            lay st and pf into the 4 pt plane
      v1(1)=tp4(1)-tp1(1)
      v1(2)=tp4(2)-tp1(2)
      v1(3)=tp4(3)-tp1(3)
      v2(1)=tp3(1)-tp2(1)
      v2(2)=tp3(2)-tp2(2)
      v2(3)=tp3(3)-tp2(3)
      v3(1)=v1(2)*v2(3)-v1(3)*v2(2)
      v3(2)=v1(3)*v2(1)-v1(1)*v2(3)
      v3(3)=v1(1)*v2(2)-v1(2)*v2(1)
      sec=sqrt(v3(1)**2+v3(2)**2+v3(3)**2)
      if(sec.eq.0.d0)go to 999
      v3(1)=v3(1)/sec
      v3(2)=v3(2)/sec
      v3(3)=v3(3)/sec
      dis=st(1)*v3(1)+st(2)*v3(2)+st(3)*v3(3)
      st(1)=st(1)-dis*v3(1)
      st(2)=st(2)-dis*v3(2)
      st(3)=st(3)-dis*v3(3)
      sec=dsqrt(st(1)**2+st(2)**2+st(3)**2)
      if(sec.eq.0.d0)go to 999
      st(1)=st(1)/sec
      st(2)=st(2)/sec
      st(3)=st(3)/sec
      dis=pf(1)*v3(1)+pf(2)*v3(2)+pf(3)*v3(3)
      pf(1)=pf(1)-dis*v3(1)
      pf(2)=pf(2)-dis*v3(2)
      pf(3)=pf(3)-dis*v3(3)
      sec=dsqrt(pf(1)**2+pf(2)**2+pf(3)**2)
      if(sec.eq.0.d0)go to 999
      pf(1)=pf(1)/sec
      pf(2)=pf(2)/sec
      pf(3)=pf(3)/sec

c                            get location of tool to drive edges     
      dis=tpin(4)*(st(2)*pf(3)-st(3)*pf(2))
     1   +tpin(5)*(st(3)*pf(1)-st(1)*pf(3))
     2   +tpin(6)*(st(1)*pf(2)-st(2)*pf(1))
      i=1
      if(dis.lt.0.)i=-i
      if(mpla.eq.71)then
          lt12=0
      elseif(mpla.eq.714)then
          lt12=i
      else
          lt12=-i
      endif
      if(mplb.eq.71)then
          lt34=0
      elseif(mplb.eq.715)then
          lt34=i
      else
          lt34=-i
      endif
      if(mcsb.eq.71)then
          lt24=0
      elseif(mcsb.eq.714)then
          lt24=i
      else
          lt24=-i
      endif
      if(mcsa.eq.71)then
          lt13=0
      elseif(mcsa.eq.715)then
          lt13=i
      else
          lt13=-i
      endif
999   if(sec.eq.0.d0)ifl(2)=5
      return
      end
