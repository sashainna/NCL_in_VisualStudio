c*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL 
C*       regstp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:37
c**
c*****************************************************
****  FILE NAME: REGSTP               13-MAR-87            PEM
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C            THIS ROUTINE CAUSES GOFWD ALONG LIMA OR LIMB ONTO NEXT PASPL.
C
C              LTSK=1 MEANS LIMA IS THE DS.
C                  =2   "   LIMB  "  "   "

C*************************************************************************


      subroutine regstp(ltsk)
       
c-----------------------------------  reg.com ---------------------
       
       
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
      integer*2 kclp(4),krtp(4),lseg(2), iflg
      equivalence (sc,asc,ksc),(asc(63),sbe),(asc(64),cbe),(asn,bsn,ksn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(aclp,kclp),(aclp,cdis),(artp,krtp),(artp,rdis)

      real*8 dpmax, dsthk, csthk

      itsk = 0
      dsthk = sc(24)
      csthk = sc(25)
c                      save maxdp of motion commands
      dpmax=sc(54)
c                      calculate stepover
      call regscl(stpov)
      if(dpmax.gt.stpov)sc(54)=.5*stpov
c                      calc nexpl.    use old dro for first try
      orho=rho
      remrho=1.-rho
      itim=0
20    if(dro.lt.remrho*.95)goto 22
      dro=remrho
      goto 24
22    if(dro.lt.remrho*.6)goto 24
      dro=remrho*.6
24    rho=orho+dro
      if(rho.gt..9999)rho=1.
      do 25 i=1,4
25    pl(i)=pla(i)+plb(i)*rho
c                      normalize pl
      sec=dsqrt(pl(1)**2+pl(2)**2+pl(3)**2)
      do 27 i=1,4
27    pl(i)=pl(i)/sec

c                      check pl vs. p1,p2 and step
      d1=pl(4)-pl(1)*tp1(1)-pl(2)*tp1(2)-pl(3)*tp1(3)
      d2=pl(4)-pl(1)*tp2(1)-pl(2)*tp2(2)-pl(3)*tp2(3)
      dmax=d1
      if(d2.gt.d1)dmax=d2
      rat=dmax/stpov
      if(rat.lt.1.1.and.rat.gt..9)goto 30
      dro=dro/rat
      itim=itim+1
      if(itim.gt.5)goto 30
      goto 20
30    continue
c                      INDRV fwd in sc(7-9)
      ifl(22)=1
      sc(7)=st(1)
      sc(8)=st(2)
      sc(9)=st(3)
c                      TLxxx,GOFWD/csx,<mod>,pl
      sc(10)=0.
      ksc(37)=704
      ksc(38)=1
      if(ltsk.eq.1)then
c                          csa is ds
          ifl(21)=lt13
          sc(11)=acsa
          itsk = 1
          call nclx_rmill_swap(itsk)
          sc(24) = csthk1
      else
c                          csb is ds
          ifl(21)=lt24
          sc(11)=acsb
          itsk = 2
          call nclx_rmill_swap(itsk)
          sc(24) = csthk2
      endif
c                      setup surf rel mod and cs
      if(rho.gt..9999)then
          sc(12)=mplb
          sc(13)=aplb
          sc(25) = dsthk2
      else
          sc(12)=71
          sc(13)=0.
          ksc(51)=4
          ksc(52)=6
c                          load pl in d(103) as cs
          do 32 i=1,4
32        d(102+i)=pl(i)
      endif
c                      do the step
      call premov
c                      if curve ds restore last seg
c     if(ksc(44).eq.8)jd(204)=lseg(ltsk)
      call mover
      if (ifl(95).eq.1 .and. ifl(42).eq.0) call cirrec
      call nclx_rmill_swap(itsk)
      sc(24) = dsthk
      sc(25) = csthk
      call ncl_tstptr(nrcn,iflg)
      if (iflg .ne. 0) call regdis
c
c                      if curve ds save last seg
c     if(ksc(44).eq.8)lseg(ltsk)=jd(203)
c                      restore maxdp of motion commands
      sc(54)=dpmax
      return
      end
