c*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL 
C*       regpas.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:36
c**
c*****************************************************
****  FILE NAME: REGPAS               13-MAR-87            PEM
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C         THIS ROUTINE CAUSES A PASS ALONG THE CURRENT DSPL ON CSPL
C         CREATED BY LIMA OR LIMB INTERSECTION.  
C
C              DIR=+1.  PASS IS TOWARD LIMB.
C                  -1.   "    "    "   LIMA.
C
C*************************************************************************


      subroutine regpas(dir)
       
c-------------------------------------   reg.com ---------------
       
       
      include 'com4a.com'
      include 'mocom.com'
c
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

      common/regthk/ rthk

      real*8 rthk(8)
      real*8 dsthk1,dsthk2,csthk1,csthk2,dsptk1,dsptk2,csptk1,csptk2
      equivalence (rthk(1),csthk1),(rthk(2),csthk2)
      equivalence (rthk(3),dsthk1),(rthk(4),dsthk2)
      equivalence (rthk(5),csptk1),(rthk(6),csptk2)
      equivalence (rthk(7),dsptk1),(rthk(8),dsptk2)

      real*8 aclp,artp
      real*4 ad(300),asc(100)
      integer*2 jd(600),ksn(4),ksc(100),iflg
      integer*2 kclp(4),krtp(4),lseg(2)
      equivalence (sc,asc,ksc),(asc(63),sbe),(asc(64),cbe),(asn,bsn,ksn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(aclp,kclp),(aclp,cdis),(artp,krtp),(artp,rdis)

c                     INDRV fwd in sc(7-9)
      ifl(22)=1
      sc(7)=pf(1)*dir
      sc(8)=pf(2)*dir
      sc(9)=pf(3)*dir
c                     TLxxx,GOFWD/ds,<mod>,cs
      sc(10)=0.
      ksc(37)=704
      ksc(38)=1
      if(rho.eq.0.)then
c                         1st pass, ds is pla w/ tool rel mod
          ifl(21)=lt12
          sc(11)=apla
          sc(24) = dsthk1
      elseif(rho.gt..9999)then
c                         last pass ds is plb w/ tool rel mod*dir
          ifl(21)=lt34*dir
          sc(11)=aplb
          sc(24) = dsthk2
      else
c                         med pass ds is last cs w/ tool rel mod ON
          ifl(21)=0
          do 20 i=1,4
20        d(52+i)=d(102+i)
          sc(11)=0.
          ksc(43)=4
          ksc(44)=6
      endif
c                     setup surf rel mod and cs
      if(dir.gt.0.)then
          sc(12)=0.
          ksc(45)=mcsb
          ksc(46)=1
          sc(13)=acsb
          sc(25) = csthk2
      else
          sc(12)=0.
          ksc(45)=mcsa
          ksc(46)=1
          sc(13)=acsa
          sc(25) = csthk1
      endif
c                     do the pass
      call motgxx
c
c...vp 16-feb-94 FR/AT...OUT in RMILL
c
      call ncl_tstptr(nrcn,iflg)
      if (iflg .ne. 0) call regdis

99    return
      end
