c*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL 
C*       regfrm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:36
c**
c*****************************************************
C****  FILE NAME: REGFRM               30-NOV-88            PEM
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C             THIS ROUTINE BUILDS A FROM POSITION INSIDE THE REGION AND
C             OFFSET TO PLANE ONE AND CHECK ONE NEAR POINT P1  THIS IS
C             DONE BY GENERATING TOOL-ON END POINTS WITH MOTION COMMANDS
c             AT THE CORNERS OF THE REGION.
C
C*************************************************************************


      subroutine regfrm
       
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

      real*4 vd2(3),vd3(3),vnx(3),snx,ofs,dis
      integer*2 ics,ier
      real*8 of1, of2

      ics=1
c                     INDIRV/ta X pla normal
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
1     ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     GO/ON,cs1
      ifl(280)=1
      isc10(1)=702
      isc10(3)=1
      sc(11)=71
      sc(12)=acsa
      call strtup(1)
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
          if(ier.gt.0)go to 10
          ier=ifl(2)
          ifl(2)=0
c                     INDIRV/-vdr
          vnx(1)=-vnx(1)
          vnx(2)=-vnx(2)
          vnx(3)=-vnx(3)
          go to 1
      endif
c                     starting on cs1 towards ds1
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
      go to 100

c                     error try other check surface
10    ics=2
      ifl(2)=0
      ier=0
11    ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     GO/ON,cs2
      ifl(280)=1
      isc10(1)=702
      isc10(3)=1
      sc(11)=71
      sc(12)=acsb
      call strtup(1)
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
          go to 11
      endif
c                     starting on cs2 towards ds1
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
20    ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     TLON
      ifl(21)=0
c                     GOFWD/cs2,ON,ds1
      sc(10)=0.d0
      ksc(37)=704
      ksc(38)=1
      sc(11)=acsb
      sc(12)=0.d0
      ksc(45)=71
      ksc(46)=1
      sc(13)=apla
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
          go to 20
      endif
c                     p2=POINT/TE
      do 30 i=1,6
30    tp2(i)=sc(i)
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
c                     TLON
      ifl(21)=0
c                     GOFWD/cs1,ON,ds1(or ds1,ON,cs1)
      sc(10)=0.d0
      ksc(37)=704
      ksc(38)=1
      sc(12)=0.d0
      ksc(45)=71
      ksc(46)=1
      if(ics.eq.2)then
        sc(11)=apla
        sc(13)=acsa
        call motgxx
      else
        sc(11)=acsa
        sc(13)=apla
        itsk = 1
        call nclx_rmill_swap(itsk)
        call motgxx
        call nclx_rmill_swap(itsk)
      endif
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
c                     p1=POINT/TE
      do 110 i=1,6
110   tp1(i)=sc(i)
c                     save direction from p1 to p3 (or p2)
      if(ics.eq.2)then
          vd2(1)=-sc(7)
          vd2(2)=-sc(8)
          vd2(3)=-sc(9)
      else
          vd3(1)=-sc(7)
          vd3(2)=-sc(8)
          vd3(3)=-sc(9)
      endif
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
      if(ics.eq.2)go to 300

200   ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     TLON
      ifl(21)=0
c                     GOFWD/ds1,ON,cs2
      sc(10)=0.d0
      ksc(37)=704
      ksc(38)=1
      sc(11)=apla
      sc(12)=0.d0
      ksc(45)=71
      ksc(46)=1
      sc(13)=acsb
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
c                     FROM/p1 INDIRV/vd3
      do 220 i=1,6
220   sc(i)=tp1(i)
c                     save direction from p1 to p2 first
      vd2(1)=vnx(1)
      vd2(2)=vnx(2)
      vd2(3)=vnx(3)
      vnx(1)=vd3(1)
      vnx(2)=vd3(2)
      vnx(3)=vd3(3)
      ier=0

300   ifl(22)=1
      sc(7)=vnx(1)
      sc(8)=vnx(2)
      sc(9)=vnx(3)
c                     TLON
      ifl(21)=0
c                     GOFWD/cs1,ON,ds2
      sc(10)=0.d0
      ksc(37)=704
      ksc(38)=1
      sc(11)=acsa
      sc(12)=0.d0
      ksc(45)=71
      ksc(46)=1
      sc(13)=aplb
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
          go to 300
      endif
c                     p3=POINT/TE
      do 310 i=1,6
310   tp3(i)=sc(i)
c                     save direction from p1 to p3
      vd3(1)=vnx(1)
      vd3(2)=vnx(2)
      vd3(3)=vnx(3)
c                     build region's from point
      do 500 i=1,6
500   sc(i)=tp1(i)
c                     get max dist between pts
      vnx(1)=tp2(1)-tp1(1)
      vnx(2)=tp2(2)-tp1(2)
      vnx(3)=tp2(3)-tp1(3)
      ofs=sqrt(vnx(1)**2+vnx(2)**2+vnx(3)**2)
c                     check direction from p1 to p2
      if(vnx(1)*vd2(1)+vnx(2)*vd2(2)+vnx(3)*vd2(3).lt.0.0)then
          vd2(1)=-vd2(1)
          vd2(2)=-vd2(2)
          vd2(3)=-vd2(3)
      endif
      vnx(1)=tp3(1)-tp1(1)
      vnx(2)=tp3(2)-tp1(2)
      vnx(3)=tp3(3)-tp1(3)
      dis=sqrt(vnx(1)**2+vnx(2)**2+vnx(3)**2)
c                     check direction from p1 to p3
      if(vnx(1)*vd3(1)+vnx(2)*vd3(2)+vnx(3)*vd3(3).lt.0.0)then
          vd3(1)=-vd3(1)
          vd3(2)=-vd3(2)
          vd3(3)=-vd3(3)
      endif
      if(ofs.gt.dis)dis=ofs
c                     get sine between direction vectors
      vnx(1)=vd3(2)*vd2(3)-vd3(3)*vd2(2)
      vnx(2)=vd3(3)*vd2(1)-vd3(1)*vd2(3)
      vnx(3)=vd3(1)*vd2(2)-vd3(2)*vd2(1)
      snx=sqrt(vnx(1)**2+vnx(2)**2+vnx(3)**2)
      if(snx.lt..001)then
c                     error ds1 and cs1 are near tangent
          ier=5
          go to 998
      endif
c                     get offset values and add them in
      of1=(tool(1)+csthk1)/snx
      of2=(tool(1)+dsthk1)/snx
c      if(ofs.eq.0.0)ofs=.25*snx
c      ofh=dis*ofs-ofs**2
c      if(ofh.gt.0.0)then
c          ofh=sqrt(ofh)
c      else    
c          ofh=dis
c      endif
c                     see which way is up
c      if(vnx(1)*sc(4)+vnx(2)*sc(5)+vnx(3)*sc(6).lt.0.d0)snx=-snx
c      ofh=ofh/snx
c      sc(1)=sc(1)+ofs*(vd2(1)+vd3(1))+ofh*vnx(1)
c      sc(2)=sc(2)+ofs*(vd2(2)+vd3(2))+ofh*vnx(2)
c      sc(3)=sc(3)+ofs*(vd2(3)+vd3(3))+ofh*vnx(3)
      sc(1)=sc(1)+of1*vd2(1)+of2*vd3(1)
      sc(2)=sc(2)+of1*vd2(2)+of2*vd3(2)
      sc(3)=sc(3)+of1*vd2(3)+of2*vd3(3)
      go to 999

998   ifl(2)=ier
999   continue
      ifl(22)=0
      return
      end
