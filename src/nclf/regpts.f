c*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL 
C*       regpts.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:36
c**
c*****************************************************
C****  FILE NAME: REGPTS               30-NOV-88            PEM
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C             THIS ROUTINE BUILDS THE FOUR CORNER POINTS P1, P2, P3 AND
C             P4 OFFSET TO THE TWO CHECK SURFACES AND TWO DRIVE PLANES
C             DEFINED FOR REGION MILL.  THIS IS DONE BY GENERATING A SET
C             OF MOTION COMMANDS AFTER A ONE SUFACE STARTUP.
C
C*************************************************************************


      subroutine regpts
       
c----------------------------------  reg.com ------------------
       
       
      include 'com4a.com'
      include 'mocom.com'

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
      integer*2 jd(600),ksn(4),ksc(100)
      integer*2 kclp(4),krtp(4),lseg(2)
      equivalence (sc,asc,ksc),(asc(63),sbe),(asc(64),cbe),(asn,bsn,ksn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(aclp,kclp),(aclp,cdis),(artp,krtp),(artp,rdis)

      real*4 vls(3),vnx(3),snx
      integer*2 mls,ier

c                     PSIS/aps
      sc(10)=0.d0
      isc10(1)=713
      isc10(2)=1
      sc(11)=aps
      call motimm
      if(ifl(2).gt.0)go to 999
c                     FROM/ point near p1 and inside region
      call regfrm
      if(ifl(2).gt.0)go to 999
c                     INDIRV/ta x pla normal
      vnx(1)=sc(5)*pla(3)-sc(6)*pla(2)
      vnx(2)=sc(6)*pla(1)-sc(4)*pla(3)
      vnx(3)=sc(4)*pla(2)-sc(5)*pla(1)
      snx=sqrt(vnx(1)**2+vnx(2)**2+vnx(3)**2)
      if(abs(snx).lt..001)then
          vnx(1)=sc(4)
          vnx(2)=sc(5)
          vnx(3)=sc(6)
      else
          vnx(1)=vnx(1)/snx
          vnx(2)=vnx(2)/snx
          vnx(3)=vnx(3)/snx
      endif
      ier=0
10    ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     GO/<mod-cs1>,cs1
      ifl(280)=1
      isc10(1)=702
      isc10(3)=1
      sc(11)=mcsa
      sc(12)=acsa
      sc(24) = csthk1
      call strtup(1)
      sc(25) = dsthk1
C                     if chgd to GODLTA, do motimm
      if(isc10(1).eq.710)then
          call motimm
      else
          call mover
          ifl(22)=0
          call strtup(2)
      endif
      ifl(280)=0
      if(ifl(2).gt.0)then
          if(ier.gt.0)go to 998
          ier=ifl(2)
          ifl(2)=0
c                     INDIRV/-vdr
          vnx(1)=-vnx(1)
          vnx(2)=-vnx(2)
          vnx(3)=-vnx(3)
          go to 10
      endif
c                     save last direction vector
      vls(1)=sc(7)
      vls(2)=sc(8)
      vls(3)=sc(9)
c                     check direction with p1-p2
      if((tp1(1)-tp2(1))*vls(1)+(tp1(2)-tp2(2))*vls(2)
     1  +(tp1(3)-tp2(3))*vls(3).lt.0.0)then
          vls(1)=-vls(1)
          vls(2)=-vls(2)
          vls(3)=-vls(3)
      endif
      mls=mcsa
c                     INDIRV/vnx
      vnx(1)=sc(5)*s(3,3)-sc(6)*s(2,3)
      vnx(2)=sc(6)*s(1,3)-sc(4)*s(3,3)
      vnx(3)=sc(4)*s(2,3)-sc(5)*s(1,3)
      snx=sqrt(vnx(1)**2+vnx(2)**2+vnx(3)**2)
      if(abs(snx).lt..001)then
          vnx(1)=sc(4)
          vnx(2)=sc(5)
          vnx(3)=sc(6)
      else
          vnx(1)=vnx(1)/snx
          vnx(2)=vnx(2)/snx
          vnx(3)=vnx(3)/snx
      endif
      ier=0
100   ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     get TLxxx of cs1
      if(mls.eq.71)then
c                     TLON
          ifl(21)=0
      else
          snx=(vnx(2)*vls(3)-vnx(3)*vls(2))*sc(4)
     1       +(vnx(3)*vls(1)-vnx(1)*vls(3))*sc(5)
     2       +(vnx(1)*vls(2)-vnx(2)*vls(1))*sc(6)
          if(mls.eq.715)snx=-snx
          if(snx.gt.0.0)then
c                     TLRGT
              ifl(21)=1
          elseif(snx.lt.0)then
c                     TLLFT
              ifl(21)=-1
          elseif(snx.eq.0)then
c                     tangent dont change
          endif
      endif
c                     GOFWD/cs1,<mod-ds1>,ds1
      sc(10)=0.d0
      ksc(37)=704
      ksc(38)=1
      sc(11)=acsa
      sc(12)=0.d0
      ksc(45)=mpla
      ksc(46)=1
      sc(13)=apla
      sc(24) = csthk1
      sc(25) = dsthk1
      itsk = 1
      call nclx_rmill_swap(itsk)
      call motgxx
      call nclx_rmill_swap(itsk)
      if(ifl(2).gt.0)then
          if(ier.gt.0)go to 998
          ier=ifl(2)
          ifl(2)=0
c                     INDIRV/-vdr
          vnx(1)=-vnx(1)
          vnx(2)=-vnx(2)
          vnx(3)=-vnx(3)
          go to 100
      endif
c                     save last direction vector
      vls(1)=sc(7)
      vls(2)=sc(8)
      vls(3)=sc(9)
c                     check direction with p1-p3
      if((tp1(1)-tp3(1))*vls(1)+(tp1(2)-tp3(2))*vls(2)
     1  +(tp1(3)-tp3(3))*vls(3).lt.0.0)then
          vls(1)=-vls(1)
          vls(2)=-vls(2)
          vls(3)=-vls(3)
      endif
c                     p1=POINT/TE
      do 110 i=1,6
110   tp1(i)=sc(i)
c                     store surface pt and normal for p1
      do 120 i=1,7
120   sp1(i)=s(i,1)
      mls=mpla
c                     INDIRV/vnx
      vnx(1)=sc(5)*s(3,3)-sc(6)*s(2,3)
      vnx(2)=sc(6)*s(1,3)-sc(4)*s(3,3)
      vnx(3)=sc(4)*s(2,3)-sc(5)*s(1,3)
      snx=sqrt(vnx(1)**2+vnx(2)**2+vnx(3)**2)
      if(abs(snx).lt..001)then
          vnx(1)=sc(4)
          vnx(2)=sc(5)
          vnx(3)=sc(6)
      else
          vnx(1)=vnx(1)/snx
          vnx(2)=vnx(2)/snx
          vnx(3)=vnx(3)/snx
      endif
      ier=0
200   ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     get TLxxx of ds1
      if(mls.eq.71)then
c                     TLON
          ifl(21)=0
      else
          snx=(vnx(2)*vls(3)-vnx(3)*vls(2))*sc(4)
     1       +(vnx(3)*vls(1)-vnx(1)*vls(3))*sc(5)
     2       +(vnx(1)*vls(2)-vnx(2)*vls(1))*sc(6)
          if(mls.eq.715)snx=-snx
          if(snx.gt.0.0)then
c                     TLRGT
              ifl(21)=1
          elseif(snx.lt.0)then
c                     TLLFT
              ifl(21)=-1
          elseif(snx.eq.0)then
c                     tangent dont change
          endif
      endif
c                     GOFWD/ds1,<mod-cs2>,cs2
      sc(10)=0.d0
      ksc(37)=704
      ksc(38)=1
      sc(11)=apla
      sc(12)=0.d0
      ksc(45)=mcsb
      ksc(46)=1
      sc(13)=acsb
      sc(24) = dsthk1
      sc(25) = csthk2
      call motgxx
      if(ifl(2).gt.0)then
          if(ier.gt.0)go to 998
          ier=ifl(2)
          ifl(2)=0
c                     INDIRV/-vdr
          vnx(1)=-vnx(1)
          vnx(2)=-vnx(2)
          vnx(3)=-vnx(3)
          go to 200
      endif
c                     p2=POINT/TE
      do 210 i=1,6
210   tp2(i)=sc(i)
c                     store surface pt and normal for p2
      do 220 i=1,7
220   sp2(i)=s(i,1)
c                     save last direction vector
      vls(1)=sc(7)
      vls(2)=sc(8)
      vls(3)=sc(9)
c                     check direction with p2-p1
      if((tp2(1)-tp1(1))*vls(1)+(tp2(2)-tp1(2))*vls(2)
     1  +(tp2(3)-tp1(3))*vls(3).lt.0.0)then
          vls(1)=-vls(1)
          vls(2)=-vls(2)
          vls(3)=-vls(3)
      endif
      mls=mcsb
c                     INDIRV/vnx
      vnx(1)=sc(5)*s(3,3)-sc(6)*s(2,3)
      vnx(2)=sc(6)*s(1,3)-sc(4)*s(3,3)
      vnx(3)=sc(4)*s(2,3)-sc(5)*s(1,3)
      snx=sqrt(vnx(1)**2+vnx(2)**2+vnx(3)**2)
      if(abs(snx).lt..001)then
          vnx(1)=sc(4)
          vnx(2)=sc(5)
          vnx(3)=sc(6)
      else
          vnx(1)=vnx(1)/snx
          vnx(2)=vnx(2)/snx
          vnx(3)=vnx(3)/snx
      endif
      ier=0
300   ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     get TLxxx of cs2
      if(mls.eq.71)then
c                     TLON
          ifl(21)=0
      else
          snx=(vnx(2)*vls(3)-vnx(3)*vls(2))*sc(4)
     1       +(vnx(3)*vls(1)-vnx(1)*vls(3))*sc(5)
     2       +(vnx(1)*vls(2)-vnx(2)*vls(1))*sc(6)
          if(mls.eq.715)snx=-snx
          if(snx.gt.0.0)then
c                     TLRGT
              ifl(21)=1
          elseif(snx.lt.0)then
c                     TLLFT
              ifl(21)=-1
          elseif(snx.eq.0)then
c                     tangent dont change
          endif
      endif
c                     GOFWD/cs2,<mod-ds2>,ds2
      sc(10)=0.d0
      ksc(37)=704
      ksc(38)=1
      sc(11)=acsb
      sc(12)=0.d0
      ksc(45)=mplb
      ksc(46)=1
      sc(13)=aplb
      sc(24) = csthk2
      sc(25) = dsthk2
      itsk = 2
      call nclx_rmill_swap(itsk)
      call motgxx
      call nclx_rmill_swap(itsk)
      if(ifl(2).gt.0)then
          if(ier.gt.0)go to 998
          ier=ifl(2)
          ifl(2)=0
c                     INDIRV/-vdr
          vnx(1)=-vnx(1)
          vnx(2)=-vnx(2)
          vnx(3)=-vnx(3)
          go to 300
      endif
c                     p4=POINT/TE
      do 310 i=1,6
310   tp4(i)=sc(i)
c                     save last direction vector
      vls(1)=sc(7)
      vls(2)=sc(8)
      vls(3)=sc(9)
c                     check direction with p4-p2
      if((tp4(1)-tp2(1))*vls(1)+(tp4(2)-tp2(2))*vls(2)
     1  +(tp4(3)-tp2(3))*vls(3).lt.0.0)then
          vls(1)=-vls(1)
          vls(2)=-vls(2)
          vls(3)=-vls(3)
      endif
      mls=mplb
c                     INDIRV/vnx
      vnx(1)=sc(5)*s(3,3)-sc(6)*s(2,3)
      vnx(2)=sc(6)*s(1,3)-sc(4)*s(3,3)
      vnx(3)=sc(4)*s(2,3)-sc(5)*s(1,3)
      snx=sqrt(vnx(1)**2+vnx(2)**2+vnx(3)**2)
      if(abs(snx).lt..001)then
          vnx(1)=sc(4)
          vnx(2)=sc(5)
          vnx(3)=sc(6)
      else
          vnx(1)=vnx(1)/snx
          vnx(2)=vnx(2)/snx
          vnx(3)=vnx(3)/snx
      endif
      ier=0
400   ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     get TLxxx of ds2
      if(mls.eq.71)then
c                     TLON
          ifl(21)=0
      else
          snx=(vnx(2)*vls(3)-vnx(3)*vls(2))*sc(4)
     1       +(vnx(3)*vls(1)-vnx(1)*vls(3))*sc(5)
     2       +(vnx(1)*vls(2)-vnx(2)*vls(1))*sc(6)
          if(mls.eq.715)snx=-snx
          if(snx.gt.0.0)then
c                     TLRGT
              ifl(21)=1
          elseif(snx.lt.0)then
c                     TLLFT
              ifl(21)=-1
          elseif(snx.eq.0)then
c                     tangent dont change
          endif
      endif
c                     GOFWD/ds2,<mod-cs1>,cs1
      sc(10)=0.d0
      ksc(37)=704
      ksc(38)=1
      sc(11)=aplb
      sc(12)=0.d0
      ksc(45)=mcsa
      ksc(46)=1
      sc(13)=acsa
      sc(24) = dsthk2
      sc(25) = csthk1
      call motgxx
      if(ifl(2).gt.0)then
          if(ier.gt.0)go to 998
          ier=ifl(2)
          ifl(2)=0
c                     INDIRV/-vdr
          vnx(1)=-vnx(1)
          vnx(2)=-vnx(2)
          vnx(3)=-vnx(3)
          go to 400
      endif
c                     p3=POINT/TE
      do 410 i=1,6
410   tp3(i)=sc(i)
      go to 999

998   ifl(2)=ier
999   continue
      ifl(22)=0
      return
      end
