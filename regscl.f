c*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL 
C*       regscl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:36
c**
c*****************************************************
C****  FILE NAME: REGSCL               01-DEC-88            PEM
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C             THIS ROUTINE CALCULATES THE STEP OVER BETWEEN PASSES FROM
C             THE TOOL AND SURFACE DATA AT THE TWO END POINTS OF THE
C             CURRENT PASS. THE TOOL DATA IS IN TP1 AND TP2 THE SURFACE
C             DATA IS IN SP1 AND SP2 RESPECTIVELY. THE STEP OVER RETURNED
C             IS THE SMALLER BETWEEN THE TWO END POINTS.
C
C*************************************************************************


      subroutine regscl(stpov)
       
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

      real*4 v1(3),v2(3)
      
c                     get the projected sn1 into ta1,ds normal
c                     cos1 = psn1.ta1  sin1 = (1-psn1.dsn**2)**.5
      v1(1)=pl(2)*tp1(6)-pl(3)*tp1(5)
      v1(2)=pl(3)*tp1(4)-pl(1)*tp1(6)
      v1(3)=pl(1)*tp1(5)-pl(2)*tp1(4)
      dis=sqrt(v1(1)**2+v1(2)**2+v1(3)**2)
      if(abs(dis).lt..001)then
c                     ta and ds vectors are parallel
          v1(1)=sp1(1)
          v1(2)=sp1(2)
          v1(3)=sp1(3)
          go to 10
      endif
      v1(1)=v1(1)/dis
      v1(2)=v1(2)/dis
      v1(3)=v1(3)/dis
      dis=sp1(1)*v1(1)+sp1(2)*v1(2)+sp1(3)*v1(3)
      if(abs(dis).lt..001)then
c                     sn is in ta,ds plane
          v1(1)=sp1(1)
          v1(2)=sp1(2)
          v1(3)=sp1(3)
          go to 10
      endif
      v1(1)=sp1(1)-dis*v1(1)
      v1(2)=sp1(2)-dis*v1(2)
      v1(3)=sp1(3)-dis*v1(3)
      dis=sqrt(v1(1)**2+v1(2)**2+v1(3)**2)
      if(abs(dis).lt..001)then
c                     sn perpto ta,ds plane
          v1(1)=tp1(4)
          v1(2)=tp1(5)
          v1(3)=tp1(6)
          go to 10
      endif
      v1(1)=v1(1)/dis
      v1(2)=v1(2)/dis
      v1(3)=v1(3)/dis
10    cs1=v1(1)*tp1(4)+v1(2)*tp1(5)+v1(3)*tp1(6)
      sn1=sqrt(1.0-(v1(1)*pl(1)+v1(2)*pl(2)+v1(3)*pl(3))**2)

c                     get the projected sn2 into ta2,ds normal
c                     cos2 = psn2.ta2  sin2 = (1-psn2.dsn**2)**.5
      v2(1)=pl(2)*tp2(6)-pl(3)*tp2(5)
      v2(2)=pl(3)*tp2(4)-pl(1)*tp2(6)
      v2(3)=pl(1)*tp2(5)-pl(2)*tp2(4)
      dis=sqrt(v2(1)**2+v2(2)**2+v2(3)**2)
      if(abs(dis).lt..001)then
c                     ta and ds vectors are parallel
          v2(1)=sp2(1)
          v2(2)=sp2(2)
          v2(3)=sp2(3)
          go to 20
      endif
      v2(1)=v2(1)/dis
      v2(2)=v2(2)/dis
      v2(3)=v2(3)/dis
      dis=sp2(1)*v2(1)+sp2(2)*v2(2)+sp2(3)*v2(3)
      if(abs(dis).lt..001)then
c                     sn is in ta,ds plane
          v2(1)=sp2(1)
          v2(2)=sp2(2)
          v2(3)=sp2(3)
          go to 20
      endif
      v2(1)=sp2(1)-dis*v2(1)
      v2(2)=sp2(2)-dis*v2(2)
      v2(3)=sp2(3)-dis*v2(3)
      dis=sqrt(v2(1)**2+v2(2)**2+v2(3)**2)
      if(abs(dis).lt..001)then
c                     sn perpto ta,ds plane
          v2(1)=tp2(4)
          v2(2)=tp2(5)
          v2(3)=tp2(6)
          go to 20
      endif
      v2(1)=v2(1)/dis
      v2(2)=v2(2)/dis
      v2(3)=v2(3)/dis
20    cs2=v2(1)*tp2(4)+v2(2)*tp2(5)+v2(3)*tp2(6)
      sn2=sqrt(1.0-(v2(1)*pl(1)+v2(2)*pl(2)+v2(3)*pl(3))**2)

c                     get the smallest cos and sin
      csa=cs1
      if(cs2.lt.csa)csa=cs2
      snd=sn1
      if(sn2.lt.snd)snd=sn2
      if(istep.ne.2)then
c                     stepover is the step value
          stpov=step
          go to 999
      endif
      cnr=tool(2)
      flt=tool(6)
      snb=asc(63)
      csb=asc(64)
      hgt=step
      if(csa.gt..999)then
c                     calc max step per scallop
          if(hgt.gt.tool(3))hgt=tool(3)
          stpov=2.0*flt
          hgt1=cnr*(1.0-snb)
          if(hgt.gt.hgt1)then
              stpov=stpov+2.0*cnr*csb+(hgt-hgt1)*snb
          else
              stpov=stpov+2.0*sqrt(2.0*cnr*hgt-hgt**2)
          endif
          go to 999
      endif
      sna=sqrt(1.0-csa**2)
c                     set hgt max as function of tool hgt and wdth
      hgt1=cnr*(1.0-csa)
      hgtm=tool(3)*csa+hgt1-tool(1)/2.0*sna
      if(hgt.gt.hgtm)hgt=hgtm
c                     setup heights of lower corner rad
      if(hgt.le.hgt1)then
c                     scallop hgt within lower corner rad
          hsq=hgt*(2.*cnr-hgt)
          if(hsq.lt.0.0)hsq=0.0
          stpov=sqrt(hsq)
          go to 90
      endif
      stpov=cnr*sna
      hgt2=2.0*flt*sna
      hgtm=hgt1+hgt2
      if(hgt.le.hgtm.and.sna.gt..001)then
c                     scallop hgt within flat of tool
          stpov=stpov+(hgt-hgt1)*csa/sna
          go to 90
      endif
      stpov=stpov+2.0*flt*csa
      hgt3=cnr*csa
      hgtm=hgtm+hgt3
      if(hgt.lt.hgtm)then
c                     scallop hgt within upper coner rad
          hsq=hgtm-hgt
          hsq=cnr**2-hsq**2
          if(hsq.lt.0.0)hsq=0.0
          stpov=stpov-cnr*sna+sqrt(hsq)
          go to 90
      endif
      stpov=stpov+cnr-cnr*sna
      hgt=hgtm
c                     back side of next tool
90    continue
c                     hgt4 = cnr*(1-sin(a+b))
      hgt4=cnr*(1.0-sna*csb-csa*snb)
      if(hgt.ge.hgt4)then
          stpov=stpov+(hgt-hgt4)*(sna*csb+csa*snb)/(csa*csb-sna*snb)
     1         +cnr*(csa*csb-sna*snb)
      else
          hsq=hgt*(2.*cnr-hgt)
          if(hsq.lt.0.0)hsq=0.0
          stpov=stpov+sqrt(hsq)
      endif
      stpov=stpov*snd
c                     error if stepover not positive and gt zero
      if(stpov.le.0.0)stpov=step
999   return
      end
