*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL 
C*       regprf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:36
c**
c*****************************************************
****  FILE NAME: REGPRF               28-DEC-88            PEM
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C             THIS ROUTINE BUILDS MOTION COMMANDS IN SC(10) TO PROFILE
C             THE BOUNDARIES OF REGION MILL IN THE DIRECTION (CLW OR CCLW)
C             FROM CS1 ALONG DS1 TO CS2 STARTING FROM THE CORNER POINT IN
C             CS(1) TO SC(3) BACK TO THAT POINT.
C
C*************************************************************************


      subroutine regprf
       
c----------------------------------  reg.com ------------------
       
       
      include 'com4a.com'
      include 'mocom.com'
c
      real*8 asn,pla(4),plb(4),pt(3),pl(4)
      real*8 pf(3),st(3),tpin(6),tp1(6),tp2(6),tp3(6),tp4(6)
      real*8 sp1(7),sp2(7),apla,aplb,acsa,acsb,aps
      real*8 f1,f2,f3
      real*4 bsn(2)
      integer*2 mpla,mplb,mcsa,mcsb,lt12,lt34,lt13,lt24,itsk
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
      logical autsav

      autsav = autost
      autost = .true.
c                     find nearest corner pt (p3 or p4)
      d3=(sc(1)-tp3(1))**2+(sc(2)-tp3(2))**2+(sc(3)-tp3(3))**2
      d4=(sc(1)-tp4(1))**2+(sc(2)-tp4(2))**2+(sc(3)-tp4(3))**2
      idpt=3
      if(d4.lt.d3)idpt=4
c                     setup GOFWD
      sc(10)=0.
      ksc(37)=704
      ksc(38)=1
c                     if p4 profile p4 to p3
      if(idpt.eq.4)then
          ifl(22)=1
          sc(7)=-pf(1)
          sc(8)=-pf(2)
          sc(9)=-pf(3)
          ifl(21)=-lt34
          sc(11)=aplb
          sc(12)=0.
          ksc(45)=mcsa
          ksc(46)=1
          sc(13)=acsa
          sc(24) = dsptk2
          sc(25) = csptk1
          call motgxx
          call ncl_tstptr(nrcn,iflg)
          if (iflg .ne. 0) call regdis
          if(ifl(2).gt.0) go to 999
      endif
c                     profile p3 to p1
      ifl(22)=1
      sc(7)=-st(1)
      sc(8)=-st(2)
      sc(9)=-st(3)
      ifl(21)=-lt13
      sc(11)=acsa
      sc(12)=0.
      ksc(45)=mpla
      ksc(46)=1
      sc(13)=apla
      sc(24) = csptk1
      sc(25) = dsptk1
      itsk = 1
      call nclx_rmill_swap(itsk)
      call motgxx
      call nclx_rmill_swap(itsk)
      call ncl_tstptr(nrcn,iflg)
      if (iflg .ne. 0) call regdis
      if(ifl(2).gt.0) go to 999
c                     profile p1 to p2
      ifl(22)=1
      sc(7)=pf(1)
      sc(8)=pf(2)
      sc(9)=pf(3)
      ifl(21)=lt12
      sc(11)=apla
      sc(12)=0.
      ksc(45)=mcsb
      ksc(46)=1
      sc(13)=acsb
      sc(24) = dsptk1
      sc(25) = csptk2
      call motgxx
      call ncl_tstptr(nrcn,iflg)
      if (iflg .ne. 0) call regdis
      if(ifl(2).gt.0) go to 999
c                     profile p2 to p4
      ifl(22)=1
      sc(7)=st(1)
      sc(8)=st(2)
      sc(9)=st(3)
      ifl(21)=lt24
      sc(11)=acsb
      sc(12)=0.
      ksc(45)=mplb
      ksc(46)=1
      sc(13)=aplb
      sc(24) = csptk2
      sc(25) = dsptk2
      itsk = 2
      call nclx_rmill_swap(itsk)
      call motgxx
      call nclx_rmill_swap(itsk)
      call ncl_tstptr(nrcn,iflg)
      if (iflg .ne. 0) call regdis
      if(ifl(2).gt.0) go to 999
c                     if p3 profile p4 to p3
      if(idpt.eq.3)then
          ifl(22)=1
          sc(7)=-pf(1)
          sc(8)=-pf(2)
          sc(9)=-pf(3)
          ifl(21)=-lt34
          sc(11)=aplb
          sc(12)=0.
          ksc(45)=mcsa
          ksc(46)=1
          sc(13)=acsa
          sc(24) = dsptk2
          sc(25) = csptk1
          call motgxx
          call ncl_tstptr(nrcn,iflg)
          if (iflg .ne. 0) call regdis
      endif
999   continue
      autost = autsav
      return
      end
