*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL 
C*       regmil.f , 26.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/12/18 , 10:17:04
c**
c*****************************************************
****  FILE NAME: REGMIL               13-MAR-87            PEM
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C*************************************************************************
C
C             THIS ROUTINE BUILDS MOTION COMMANDS IN SC(10) TO REGION MILL
C             A GIVEN AREA AND CALLS EXISTING ROUTINES TO EXECUTE SAME.
C             COMMANDS GENERATED ARE  PSIS, FEDRAT, GOFWD, ETC.  
C
C             WHEN COMPUTED PLANES ARE THE DS AND/OR CS OF THE MOVE, THEY
C             ARE LOADED DIRECTLY IN D-TBL AND NOT FETCHED BY PREMOV.
C
C             INITIAL REQUIREMENTS ARE:
C                 5. CENTER PHILOSOPHY ONLY.
C
C*************************************************************************


      subroutine regmil
       
c----------------------------------  reg.com ------------------
       
       
      include 'com4a.com'
      include 'comgt.com'
      include 'mocom.com'

      real*8 asn,pla(4),plb(4),pt(3),pl(4)
      real*8 pf(3),st(3),tpin(6),tp1(6),tp2(6),tp3(6),tp4(6), tsc(3)
      real*8 sp1(7),sp2(7),apla,aplb,acsa,acsb,aps
      real*8 f1,f2,f3
      real*4 bsn(2)
      integer*2 mpla,mplb,mcsa,mcsb,lt12,lt34,lt13,lt24
      integer*2 kpla(4),kplb(4),kcsa(4),kcsb(4)
      equivalence (kpla,apla),(kplb,aplb),(kcsa,acsa),(kcsb,acsb)

      common/regblk/pl,pt,pla,plb,aps,apla,aplb,acsa,acsb,rho,dro
     1 ,pf,st,tpin,tp1,tp2,tp3,tp4,sp1,sp2,f1,f2,f3,step,istep
     2 ,mpla,mplb,mcsa,mcsb,lt12,lt34,lt13,lt24,lseg
c
      real*8 aclp,artp
      real*4 ad(300),asc(100)
      integer*2 jd(600),ksn(4),ksc(100)
      integer*2 kclp(4),krtp(4),lseg(2)
      equivalence (sc,asc,ksc),(asc(63),sbe),(asc(64),cbe),(asn,bsn,ksn)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(aclp,kclp),(aclp,cdis),(artp,krtp),(artp,rdis)
c...the following line make the tsc/sc(11), sc(12), sc(13) 
c...value save and reset useless
c...should remove here
c...      equivalence(sc(11),tsc(1)),(sc(12),tsc(2)),(sc(13),tsc(3))

      real*8 cler(4), dsthk, csthk,fsav
      integer*4 nclkey
      integer*4 isv2(2),iept(2)
      integer*2 isv1,kfl42,kfl213,iclf, iflg
      integer*2 nwds,ietype
      integer*2 izro /0/
      logical svmdfl
      
      dsthk = sc(24)
      csthk = sc(25)
      fsav  = FEEDC(3)
      iclf = 0
      
c                     save and turn off slowdn fedrat
      kfl213=ifl(213)
      ifl(213)=0
c
c...vp 16-feb-94 FR/AT...OUT, turn off for local handling
c
      ifl(317) = ifl(314)
      ifl(318) = ifl(315)
      ifl(314) = 0
      ifl(315) = 0
c                     save and set cutmod to dntcut
      kfl42=ifl(42)
      ifl(42)=2
      
      
c                     save the current clfile pointers in case of error
      isv1=istat(1)
      call ncl_setptr(imotp,isv2)
      call ncl_zroptr(iept)
c                    save & turn off printing during init
      svmdfl = motdfl
      motdfl = .false.
c                     init misc params
      nway=ksc(38)
      istep=ksc(39)
      mpla=ksc(85)
      mplb=ksc(86)
      mcsa=ksc(87)
      mcsb=ksc(88)
      aps=sc(11)
      acsa=sc(12)
      acsb=sc(13)
      apla=sc(14)
      aplb=sc(15)
      aclp=sc(16)
      step=sc(18)
      f1=asc(37)
      f2=asc(38)
      f3=asc(39)
      artp=sc(21)
c                     if rapid for f2, set irapid flg
      irapid=0

c.... The block of code below uses the values sc(215), sc(216), sc(217) 
c.... passed from rmill to move cutter to "nearpt"
c
c.... Sasha, July 24 2017
c
       if (sc(215) .ne. 0.0 .or. sc(216) .ne. 0.0 .or. 
     x sc(217). ne. 0.0) then
          asn=0.0
          ksn(1)=703
          ksn(2)=1
          sc(10)=asn
          
          !ifl(42) = 2    This dont cut not necessary since we are in code section with ifl(42) = 2
          
c         Temporary saving
          tsc(1) = sc(11)
          tsc(2) = sc(12)
          tsc(3) = sc(13)

c         Coordinates of nearpt            
          sc(11)=sc(215)
          sc(12)=sc(216)
          sc(13)=sc(217)
          
          call motimm
                   
          !ifl(42) = 0    The same as some lines above, dont cut no more is done below
          
          sc(11) = tsc(1) 
          sc(12) = tsc(2) 
          sc(13) = tsc(3)    
          
                              
          sc(215) = 0.0
          sc(216) = 0.0
          sc(217) = 0.0
                  
      endif
c end of new functionality
            
c
c...Causes floating point exception on Alpha
c...Bobby  -  1/17/95
c
c      if(f2.lt..1.and.ksc(76).eq.5)irapid=1
      if(ksc(75) .eq. 0 .and. ksc(76) .eq. 5)irapid=1
      if(kclp(4).eq.0) goto 20
c                     cler plane or point store values
      asn=aclp
      call gtdesc(asn,nclkey,nwds,ietype)
      if (ietype.eq.PLANE .or. ietype.eq.SURF) then
        call gtplt(asn, izro, cler)
        coscl=sc(4)*cler(1)+sc(5)*cler(2)+sc(6)*cler(3)
      else
        call gtgeom(asn,cler,nclkey,nwds,ietype)
      endif
20    plungd=sc(17)

c                     get ds1, ds2
      asn=apla
      call gtdesc(asn,nclkey,nwds,ietype)
      if (ietype.eq.PLANE .or. ietype.eq.SURF) then
c                     ds1 is a plane
        call gtplt(asn, izro, pla)
      else
c                     ds1 is a line make a plane
          call gtgeom(asn,tp1,nclkey,nwds,ietype)
          pla(1)=tp1(5)*sc(6)-tp1(6)*sc(5)
          pla(2)=tp1(6)*sc(4)-tp1(4)*sc(6)
          pla(3)=tp1(4)*sc(5)-tp1(5)*sc(4)
          dist=dsqrt(pla(1)**2+pla(2)**2+pla(3)**2)
          if(abs(dist).lt..005)then
c                     drive line parallel to cutter axis error
              ifl(2)=-1
              go to 998
          endif
          pla(1)=pla(1)/dist
          pla(2)=pla(2)/dist
          pla(3)=pla(3)/dist
          pla(4)=tp1(1)*pla(1)+tp1(2)*pla(2)+tp1(3)*pla(3)
      endif
      asn=aplb
      call gtdesc(asn,nclkey,nwds,ietype)
      if (ietype.eq.PLANE .or. ietype.eq.SURF) then
c                     ds2 is a plane
        call gtplt(asn, izro, plb)
      else
c                     ds2 is a line make a plane
          call gtgeom(asn,tp2,nclkey,nwds,ietype)
          plb(1)=tp2(5)*sc(6)-tp2(6)*sc(5)
          plb(2)=tp2(6)*sc(4)-tp2(4)*sc(6)
          plb(3)=tp2(4)*sc(5)-tp2(5)*sc(4)
          dist=dsqrt(plb(1)**2+plb(2)**2+plb(3)**2)
          if(abs(dist).lt..005)then
c                     drive line parallel to cutter axis error
              ifl(2)=-1
              go to 998
          endif
          plb(1)=plb(1)/dist
          plb(2)=plb(2)/dist
          plb(3)=plb(3)/dist
          plb(4)=tp2(1)*plb(1)+tp2(2)*plb(2)+tp2(3)*plb(3)
      endif

c                     entry xyzijk is in sc(1-6)
      do 22 i=1,6
22    tpin(i)=sc(i)

c                     get the four corner pts - p1,p2,p3,and p4
      call regpts
      if(ifl(2).gt.0)goto 998

c                     check pla against p3,p4.   flip if nec
      d3=pla(1)*tp3(1)+pla(2)*tp3(2)+pla(3)*tp3(3)-pla(4)
      d4=pla(1)*tp4(1)+pla(2)*tp4(2)+pla(3)*tp4(3)-pla(4)
c                     p3 and p4 must be on same side of pla
      if(d3*d4.ge.0.)goto 30
28    ifl(2)=163
      goto 998
30    if(d3.ge.0.)goto 32
      do 31 i=1,4
31    pla(i)=-pla(i)

32    coa=pla(1)*plb(1)+pla(2)*plb(2)+pla(3)*plb(3)
      if(coa.gt.0.)goto 34
c                     flip plb to agree with pla
      do 33 i=1,4
33    plb(i)=-plb(i)

c                     p1,p2 must now be minus from plb
34    d1=plb(1)*tp1(1)+plb(2)*tp1(2)+plb(3)*tp1(3)-plb(4)
      d2=plb(1)*tp2(1)+plb(2)*tp2(2)+plb(3)*tp2(3)-plb(4)
      if(d1.gt.0..or.d2.gt.0.)goto 28

c                     adjust pla and plb to TLON
      d1=pla(1)*tp1(1)+pla(2)*tp1(2)+pla(3)*tp1(3)-pla(4)
      d2=pla(1)*tp2(1)+pla(2)*tp2(2)+pla(3)*tp2(3)-pla(4)
c     if(abs(d1-d2).gt..002)then   warning about tool offsets different
      dist=d1
      if(abs(dist).lt.abs(d2)) dist=d2
      pla(4)=pla(4)+dist
      d3=plb(1)*tp3(1)+plb(2)*tp3(2)+plb(3)*tp3(3)-plb(4)
      d4=plb(1)*tp4(1)+plb(2)*tp4(2)+plb(3)*tp4(3)-plb(4)
c     if(abs(d3-d4).gt..002)then   warning about tool offsets different
      dist=d3
      if(abs(dist).lt.abs(d4)) dist=d4
      plb(4)=plb(4)+dist

c                     get step and cut direction vectors and
c                     tool relation to sides of region as ds
      call regdir
      if(ifl(2).gt.0)goto 998

c                     restore dntcut flag and do entry into part
      ifl(42)=kfl42
c      if(cdis.eq.0.)then
c          if(f1.gt.0.)call putcl(2000,1009,2,f1)
c          go to 38
c      endif
c                     goto clearance position
c      if(kclp(4).eq.0)then
c                         by axis distance
c          if(cdis.eq.0.) goto 36
c          sc(1)=tp1(1)+cdis*tp1(4)
c          sc(2)=tp1(2)+cdis*tp1(5)
c          sc(3)=tp1(3)+cdis*tp1(6)
c      elseif (kclp(4).eq.3) then
c                         to point
c          sc(1)=cler(1)
c          sc(2)=cler(2)
c          sc(3)=cler(3)
c      elseif (kclp(4).eq.6) then
c                         along init axis to plane
c          ddis=(cler(4)-cler(1)*tp1(1)-cler(2)*tp1(2)-cler(3)*tp1(3))
c     1         /coscl
c                         ddis must be positive
c          if(ddis.lt.0.)then
c              ifl(2)=332
c              goto 998
c          endif
c          sc(1)=tp1(1)+ddis*tpin(4)
c          sc(2)=tp1(2)+ddis*tpin(5)
c          sc(3)=tp1(3)+ddis*tpin(6)
c      endif
c      sc(4)=tp1(4)
c      sc(5)=tp1(5)
c      sc(6)=tp1(6)
c      sc(10)=0.
c      ksc(37)=803
c                     issue fedrat/f2 or RAPID (if any)
c      if(irapid.eq.1) then
c          call ppwrt(5)
c      else
c          if(f2.gt.0.)call putcl(2000,1009,2,f2)
c      endif
c      call motimm
c                     dist clear pt to stpt
c      dx=tp1(1)-sc(1)
c      dy=tp1(2)-sc(2)
c      dz=tp1(3)-sc(3)
c      dis=sqrt(dx**2+dy**2+dz**2)
c      if(dis.le.plungd)goto 36
c                     goto plung pt via 'cut'
c      ro=plungd/dis
c      sc(1)=tp1(1)-ro*dx
c      sc(2)=tp1(2)-ro*dy
c      sc(3)=tp1(3)-ro*dz
c      sc(10)=0.
c      ksc(37)=803
c                     issue RAPID (if nec)
c      if(irapid.eq.1) call ppwrt(5)
c      call motimm
c                     issue plung or entry fedrat
c36    if(f3.gt.0.)call putcl(2000,1009,2,f3)
c
c.....Added to allow plunge distance with SCRUB for RMILL.  The cutter
c.....is moved to the plunge distance above the starting point using
c.....a rapid move. - Andrew 11/19/12
c
      if (iabs(nway).ne.1.and.plungd.gt.0.) then
        call ppwrt(5)
        do 35 i=1,3
          sc(i)=tp1(i)+plungd*tp1(i+3)
35        sc(i+3)=tp1(i+3)
        sc(10)=0.
        ksc(37)=803
        call motimm
        motdfl = svmdfl
        i=1
        j=0
        call tdsply (i, sc, j)
      endif
36    if(f3.gt.0.) call fedmut (f3)
      FEEDC(3) = f3
      
c      
c                     goto stpt and issue fedrat/f1
38    do 39 i=1,6
39    sc(i)=tp1(i)
      sc(10)=0.
      ksc(37)=803
      call motimm
      motdfl = svmdfl
      i=1
      j=0
      call tdsply (i, tp1, j)
c     if(f1.gt.0.)call putcl(2000,1009,2,f1)
      FEEDC(3) = f1
      if(f1.gt.0.) call fedmut (f1)
c                     set pl and plb to deltas from pla
      do 40 i=1,4
      pl(i)=pla(i)
40    plb(i)=plb(i)-pla(i)
c                     init rho and dro
      rho=0.
      dro=.05

c*******************  pass motion *************************
50    direc=1.
      call regpas(direc)
      if(ifl(2).gt.0)goto 998
      do 51 i=1,6
51    tp2(i)=sc(i)
c                     store surface pt and normal for p2
      do 52 i=1,7
52    sp2(i)=s(i,1)
      if(rho.gt..9999)goto 80
      if(iabs(nway).ne.1)goto 60

c                     goto/clear pt from part
      if(kclp(4).eq.0)then
c                         by axis distance
          sc(11)=tp2(1)+cdis*tp2(4)
          sc(12)=tp2(2)+cdis*tp2(5)
          sc(13)=tp2(3)+cdis*tp2(6)
      elseif (kclp(4).eq.3) then
c                         to point
          sc(11)=cler(1)
          sc(12)=cler(2)
          sc(13)=cler(3)
      elseif (kclp(4).eq.PLANE .or. kclp(4).eq.SURF) then
c                         along init axis to plane
          coscl=cler(1)*tp2(4)+cler(2)*tp2(5)+cler(3)*tp2(6)
          ddis=0.
          if(coscl.ne.0.)then
            ddis=(cler(4)-cler(1)*tp2(1)-cler(2)*tp2(2)-cler(3)*tp2(3))
     1             /coscl
          endif
c                         ddis must be positive
          if(ddis.lt.0.)then
              ifl(2)=332
              goto 998
          endif
          sc(11)=tp2(1)+ddis*tp2(4)
          sc(12)=tp2(2)+ddis*tp2(5)
          sc(13)=tp2(3)+ddis*tp2(6)
      endif
      asn=0.
      ksn(1)=703
      ksn(2)=1
      sc(10)=asn
c                     issue fedrat/f2 or rapid
      if(irapid.eq.1)then
          call ppwrt(5)
      else
c         if(f2.gt.0.)call putcl(2000,1009,2,f2)
          FEEDC(3) = f2
          if(f2.gt.0.) call fedmut (f2)
      endif
      call motimm
c                     now goto/pt on clerpl above p1
      
      if(kclp(4).eq.0)then
c                         by axis distance
          sc(11)=tp1(1)+cdis*tp1(4)
          sc(12)=tp1(2)+cdis*tp1(5)
          sc(13)=tp1(3)+cdis*tp1(6)
      elseif (kclp(4).eq.3) then
c                         to point
          go to 55
      elseif (kclp(4).eq.PLANE .or. kclp(4).eq.SURF) then
c                         along init axis to plane
          coscl=cler(1)*tp1(4)+cler(2)*tp1(5)+cler(3)*tp1(6)
          ddis=0.
          if(coscl.ne.0.)then
            ddis=(cler(4)-cler(1)*tp1(1)-cler(2)*tp1(2)-cler(3)*tp1(3))
     1             /coscl
          endif
c                         ddis must be positive
          if(ddis.lt.0.)then
              ifl(2)=332
              goto 998
          endif
          sc(11)=tp1(1)+ddis*tp1(4)
          sc(12)=tp1(2)+ddis*tp1(5)
          sc(13)=tp1(3)+ddis*tp1(6)
      endif
      sc(14)=tp1(4)
      sc(15)=tp1(5)
      sc(16)=tp1(6)
      ksn(1)=703
      ksn(2)=2
      sc(10)=asn
      

      
      
c                     fedrat 'rapid' if nec
      if(irapid.eq.1)call ppwrt(5)
      call motimm
c                     now goto/pt plungd above p1
55    dx=tp1(1)-sc(1)
      dy=tp1(2)-sc(2)
      dz=tp1(3)-sc(3)
      dis=sqrt(dx**2+dy**2+dz**2)
      if(dis.le.plungd)goto 57
      ro=plungd/dis
      sc(11)=tp1(1)-ro*dx
      sc(12)=tp1(2)-ro*dy
      sc(13)=tp1(3)-ro*dz
      if(irapid.eq.1)call ppwrt(5)
      call motimm
c                     issue plung or entry fedrat
c57    if(f3.gt.0.)call putcl(2000,1009,2,f3)
57    if(f3.gt.0.) call fedmut (f3)
      FEEDC(3) = f3
c                     goto p1
58    sc(11)=tp1(1)
      sc(12)=tp1(2)
      sc(13)=tp1(3)
      call motimm

      
c     if(f1.gt.0.)call putcl(2000,1009,2,f1)
      FEEDC(3) = f1
      if(f1.gt.0.) call fedmut (f1)
      goto 70
c                     step along limb to nexpl
60    call regstp(2)
      if(ifl(2).gt.0)goto 998
      do 61 i=1,6
61    tp2(i)=sc(i)
c                     store surface pt and normal for p2
      do 62 i=1,7
62    sp2(i)=s(i,1)
c                     (-)pass to lima
      direc=-1.
      call regpas(direc)
      if(ifl(2).gt.0)goto 998
      if(rho.gt..9999)goto 80
      do 63 i=1,6
63    tp1(i)=sc(i)
c                     store surface pt and normal for p1
      do 64 i=1,7
64    sp1(i)=s(i,1)
c                     step along lima to nexpl
70    call regstp(1)
      if(ifl(2).gt.0)goto 998
      do 71 i=1,6
71    tp1(i)=sc(i)
c                     store surface pt and normal for p1
      do 72 i=1,7
72    sp1(i)=s(i,1)
      goto 50
c                     done - test for profile cut and retract
80    if (nway.lt.0) then
          call regprf
          if(ifl(2).gt.0)goto 998
      endif
c                     do retract cutter
      if (krtp(4).eq.0) then
          if (rdis.eq.0.0) go to 999
c                         by axis distance
          sc(1)=sc(1)+rdis*sc(4)
          sc(2)=sc(2)+rdis*sc(5)
          sc(3)=sc(3)+rdis*sc(6)
      elseif (krtp(4).eq.3) then
c                         to point
          asn=artp
          call gtgeom(asn,sc(1),nclkey,nwds,ietype)
      elseif (krtp(4).eq.PLANE.or.krtp(4).eq.SURF) then
c                         along init axis to plane
          asn=artp
          call gtplt(asn,izro,cler)
          coscl=cler(1)*sc(4)+cler(2)*sc(5)+cler(3)*sc(6)
          if(coscl.ne.0.)then
              ddis=(cler(4)-cler(1)*sc(1)-cler(2)*sc(2)-cler(3)*sc(3))
     1             /coscl
          else
              ddis=0.
          endif
c                         ddis must be positive
          if(ddis.lt.0.)then
              ifl(2)=332
              goto 998
          endif
          sc(1)=sc(1)+ddis*sc(4)
          sc(2)=sc(2)+ddis*sc(5)
          sc(3)=sc(3)+ddis*sc(6)
      endif
c                     issue fedrat/f2 or rapid
      if(irapid.eq.1) then
          call ppwrt(5)
      else
c         if(f2.gt.0.)call putcl(2000,1009,2,f2)
          FEEDC(3) = f2
          if(f2.gt.0.) call fedmut (f2)
      endif
c                     goto rtpt
      sc(10)=0.
      ksc(37)=803
      call motimm
      go to 999
c                     error exit
998   if(ifl(2).lt.1) ifl(2)=5
c                     set initial cutter and clfile positions
      do 90 i=1,6
90    sc(i)=tpin(i)
c
      call ncl_eqlptr(isv2,imotp,iflg)
      if (iflg .eq. 0) call cldel (iclf,isv2,iept,imotp)
      call ptclos

999   continue
c                     restore sldn fedrat
      ifl(213)=kfl213
c
c...vp 16-feb-94 restore FR/AT...OUT and reset general feed
c
      ifl(314) = ifl(317)
      ifl(315) = ifl(318)
      call setfed (sc(123))
c                     restore cut/dntcut cutmod
      ifl(42)=kfl42
c                     restore printing in case of error
      motdfl = svmdfl

      sc(24) = dsthk
      sc(25) = csthk
      FEEDC(3) = fsav
      
     
      return
      end
