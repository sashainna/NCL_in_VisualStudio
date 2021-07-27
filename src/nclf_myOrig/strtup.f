C*********************************************************************
C*    NAME         :  strtup.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       strtup.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:46
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine strtup(itim)
c*       this routine handles the go/startup command.                
c*                                                                   
c*       method:  run thru the premov,mover sequence in dntcut mode. 
c*                if all ok, issue 5000-5 command to end location.   
c*                re-store all flags,params to input state.          
c*                                                                   
c*       note:   this routine will abort at any sign of difficulty      
C*    PARAMETERS   
C*       INPUT  : 
C*          itim     - = 1, first call
C*                     = 2, second call (after call to mover)
C*                     = 3, second call (after 1 sf GO & call to motimm 
C*                                                           for godlta)
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine strtup(itim)

      include 'com4a.com'
      include 'const.com'
      include 'mocom.com'
      include 'suvcom.com'

      common/hptcom/hldctp
      real*8 hldctp(27)

      integer*2 kgo(12),ksc(100), jd(600)
c      integer*4 jd(600)
      equivalence (hgo(7),kgo),(sc,ksc)
      real*4 ad(300)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      integer*2 ipschg
      save ipschg
      real*8 asn, tdat(21), x8, y8, z8,bj(6)
      integer*2 ksn(4)
      equivalence (asn,ksn)

      real*8 f_mag, f_dot, did, a
      real*4 p4(3), t4(3), fw4(3), f_mag4, ds(3)
      real*4 px,py,pz,vx,vy,vz
      integer*4 nclkey, nn
      integer*2 nwds,ietype,knops,k,isf
      logical lv84,lv91,lv92,lv93,lv96,lfalse,lcsp
      data lfalse /.false./
c
c... jingrong 08/13/99 initialize lcsp
c
      lcsp = .false.
c
      lv84 = sc(169).lt.8.499d0
      lv91 = sc(169).le.9.05d0
      lv92 = sc(169).lt.9.249d0
      lv93 = sc(169).lt.9.349d0
      lv96 = sc(169).lt.9.649d0

      knops = isc10(4)

      if (itim.gt.1) goto 50
      insrfs=ksc(39)
      ipschg=0
c
c... put sc(1-15) in hgo(1-15)
c
      call conv8_8 (sc,hgo,15)
c
c... if 1 or 2 srfs, call gosrf1 or gosrf2 to create 3 sf cmd
c
      if (ksc(39).ne.1) goto 2
c
c... isrf.  if nops, sto d(1-6) in schld and set actpat=0
c
      if (ifl(276).eq.1) then
         ipschg = 1
         jd(41) = 0
         call conv8_8 (d,schld,6)
      endif

      call gosrf1
c                       if sc(10) is now godlta, exit       22-feb-88
c                       (means sf1 is a pl perpto ta)
      if (ksc(37).eq.710) goto 999
c              csthk=dsthk. will reset at mocntl exit       23-feb-88
      sc(25)=sc(24)
      if(ifl(2).gt.0) goto 96
c                   update hgo(10,) per sc(10,)
      goto 6
2     if (ksc(39).ne.2) goto 3
      call gosrf2
c                       csthk=dsthk                          23-feb-88
      sc(25)=sc(24)
      if (ifl(2).ne.0) goto 99
c                   update hgo(10,) per sc(10,)
6     continue
      call conv8_8 (sc(10),hgo(10),6)
c
c*******      3 srf strtup only  (temp)
3     if(ksc(39).eq.3)goto 10
5     ifl(2)=5
      goto 99
c*******
c              store and reset dntcut,toler,dpmax
10    kgo(1)=ifl(42)
      kgo(2)=ifl(21)
      ifl(42)=2
c
c... aak 13-feb-1998: transferred changing tolerance to after the call to
c... premov; surface boundaries are evaluated in premov and should be done
c... with the original tolerance
c
c      hgo(16)=sc(27)
c      sc(27)=4.*sc(27)
c
      hgo(17)=sc(54)
c               keep dpmax small. large values cause trouble.
      sc(54)=sc(54)/20.

      dpmin=.01
      if (ifl(264).eq.1) dpmin=.25

      if (sc(54).lt.dpmin) sc(54)=dpmin
      kgo(3)=ifl(91)
c
c..... QAR 92390: ifl(91) is integer*2, so must be less than 2^15-1
c
      if (ifl(91) .lt. 3275) then
        ifl(91)=ifl(91)*10
      else
        ifl(91) = 32766
      endif
      kgo(5)=ifl(154)
      ifl(154)=2
      kgo(6)=ifl(23)
      if (ifl(23).eq.8) ifl(23)=4
      if (ifl(23).eq.9) ifl(23)=4
c               stup indicator (for premov)
      kgo(4)=1
c
c...if insrfs gt 1 and not 3srf NOPS, build a psis command 
c...in sc(10), call motimm
c
      if (insrfs.lt.2 .or. knops.eq.1) goto 11
c
c...vp 9/97 reset sc[10]
c
      sc(10) = 0.
      ksc(37)=713
      sc(11)=sc(13)
      call motimm
      if(ifl(2).gt.0)goto 90
c               build a gofwd command in sc(10), call premov
11    continue
c
C...CONVERT LINE DS OR CS TO PLANE IF TA SAME except with CONTCT/ON
C...or angled cutter.
c
      IF (IFL(23).NE.0 .or. lcntct) GOTO 12
      if (sc(31) .ne. 0.0d0 .and. .not.lv84) goto 12
      DO 115 I=1,2
      ASN=HGO(I*3+9)
c
c..... qar 97142 - initialize surface plane if a curve entity, and
c..... there is a surface vector. For a curve, a surface plane is
c..... not computed in premov.
c
      if (.not.lv96 .and. ksn(4).eq.CURVE) then
        if (i.eq.1 .and. ldssvc .or. i.eq.2 .and. lcssvc) then
          isf = i+1
          do k = 1,4
            s(k,isf) = 0
          enddo
        endif
      endif
      
      IF (KSN(4).NE.5) GOTO 115
      IX=I*50
c      CALL GETENT(D(IX+3),KSN(3),KSN(1),KSN(2),0)
      call gtgeom(asn,d(ix+3),nclkey,nwds,ietype)
      VX=D(IX+7)*SC(6)-D(IX+8)*SC(5)
      VY=D(IX+8)*SC(4)-D(IX+6)*SC(6)
      VZ=D(IX+6)*SC(5)-D(IX+7)*SC(4)
      SEC=SQRT(VX**2+VY**2+VZ**2)
      IF(SEC.GT.1.E-6)GOTO 112
C               ERROR. DS LINE IS VERTICAL
      IFL(2)=304
      GOTO 90
112   VX=VX/SEC
      VY=VY/SEC
      VZ=VZ/SEC
      D(IX+6)=VX*D(IX+3)+VY*D(IX+4)+VZ*D(IX+5)
      D(IX+3)=VX
      D(IX+4)=VY
      D(IX+5)=VZ
      ASN=0.
      KSN(3)=4
      KSN(4)=6
      HGO(I*3+9)=ASN
115   CONTINUE
C               BUILD A GOFWD COMMAND IN SC(10), CALL PREMOV
12    ksc(37)=704
      sc(11)=hgo(12)
      sc(12)=hgo(14)
      sc(13)=hgo(15)
      if (.not.psuv) call getsuv(hgo(13),1,psu,psv)
      if (.not.dsuv) call getsuv(sc(11),2,dsu,dsv)
      if (.not.csuv) call getsuv(sc(13),3,csu,csv)
c
c... if no fwd sense yet, force one.
c
      if(f_mag(sc(7)).eq.0.) then
         sc(7) = 0.6
         sc(8) = 0.8
      endif
c
c..... the flag is used in premov for determining the DS look point.
c..... Immediate reason: Technology Answers' request about a GO-statement
c..... which worked in 9.1 but does not in 9.2
c
      if (hgo(11).ne.CS_ON) ifl(288) = 1
      call premov
      ifl(288) = 0

      hgo(16) = sc(27)
      sc(27)  = 4.*sc(27)
c
c... if error in premov, exit
c
      if(ifl(2).gt.0) goto 90
c
c... ds must be ln,pl,sf.  if ln, put dspl in s(,2)
c
      kds = jd(201)
      if(kds.ne.CIRCLE) goto 133

c               ds is circle. put a dsn in s(,2)    ta must be parlel axis
      co=sc(4)*d(56)+sc(5)*d(57)+sc(6)*d(58)
      if(abs(co).ge..9999)goto 132
131   ifl(2)=307
      goto 90
c               dsn is cctr to te proj on tepl
132   dis=sc(4)*(sc(1)-d(53))+sc(5)*(sc(2)-d(54))+sc(6)*(sc(3)-d(55))
      s(1,2)=sc(1)-dis*sc(4)-d(53)
      s(2,2)=sc(2)-dis*sc(5)-d(54)
      s(3,2)=sc(3)-dis*sc(6)-d(55)
      goto 134

133   if(kds.ne.LINE) goto 137
c               ds is line.  put a unit pl in s(,2)
      s(1,2)=d(57)*sc(6)-d(58)*sc(5)
      s(2,2)=d(58)*sc(4)-d(56)*sc(6)
      s(3,2)=d(56)*sc(5)-d(57)*sc(4)
134   sec=sqrt(s(1,2)**2+s(2,2)**2+s(3,2)**2)
      if(sec.gt.0.)goto 136

      if (kds.eq.CIRCLE) goto 131
c               error. ds line is vertical
      ifl(2)=304
      goto 90

136   s(1,2)=s(1,2)/sec
      s(2,2)=s(2,2)/sec
      s(3,2)=s(3,2)/sec
      s(4,2)=s(1,2)*d(53)+s(2,2)*d(54)+s(3,2)*d(55)
      if(kds.eq.7) s(4,2)=s(4,2)+d(59)
      goto 138
c                    do stuff for curves not done in premov
137   if(kds.ne.CURVE) goto 138
      ia=3
c
c..... Eduard 11/9/99. Added ic=3 for the direction check in dsrel.
c
      ic=3
      isrf=2
      call dsrel
      if (ifl(2) .eq. 466) goto 90
138   continue
c               if indirv flag off, calc fwd to cs
      kcs=jd(401)
      if(ifl(22).eq.0) goto 14
c
c... Always calc fwd sense for lines or planes
c
      if ((kcs.eq.LINE .or. kcs.eq.PLANE) .and. 
     *    (kds.eq.LINE .or. kds.eq.PLANE)) goto 14
c
c... Also calc fwd sense if ds is a circle and cs is not a curve
c
      if (sc(169).lt.8.20799d0) goto 22
      if (kds.eq.CIRCLE .and. kcs.ne.CURVE) goto 152
      goto 22
14    continue
      if(kcs.ne.CURVE) goto 152
c               error temp.  cs may not be ci or cv unless indirv given.
      if (lv93) then
        ifl(2)=306
        goto 90
      else
        s(8,3) = sc(1)
        s(9,3) = sc(2)
        s(10,3) = sc(3)
        isrf = 3
        call curvpv(px,py,pz,vx,vy,vz)
        if (ifl(2). gt. 0) goto 90
        s(5,3) = px
        s(6,3) = py
        s(7,3) = pz
        goto 20
      endif
c
c               solve a pt on cs per te pt. put it in s( ,3) tbl.
152   if(kcs.ne.CIRCLE) goto 16
c
c... csis circle.  cspt is nearest te
c
      dis=d(106)*(sc(1)-d(103))+d(107)*(sc(2)-d(104))+d(108)*(sc(3)-
     1 d(105))
      fx=sc(1)-dis*d(106)-d(103)
      fy=sc(2)-dis*d(107)-d(104)
      fz=sc(3)-dis*d(108)-d(105)
      sec=sqrt(fx**2+fy**2+fz**2)
      if(sec.ge.1.e-4)goto 153
c                       tool is on circ axis
      ifl(2)=305
      goto 90
153   s(5,3)=d(103)+d(109)*fx/sec
      s(6,3)=d(104)+d(109)*fy/sec
      s(7,3)=d(105)+d(109)*fz/sec
c               reduce maxdp unless otherwise indicated.
      if(sc(54).lt.1.)goto 20
      if(sc(54).gt.d(109)/2.) sc(54)=d(109)/2.
      if(sc(54).lt..25) sc(54)=.25
      goto 20
16    if (kcs-PLANE) 17,18,19
17    continue
      if (kcs.eq.POINT) then
        s(5,3)=d(115)
        s(6,3)=d(116)
        s(7,3)=d(117)
        goto 20
      endif
c
c.....          cs is line.
c
      ro=(d(106)*(sc(1)-d(103))+d(107)*(sc(2)-d(104))+d(108)*(sc(3)-
     * d(105)))/(d(106)**2+d(107)**2+d(108)**2)
      s(5,3)=d(103)+ro*d(106)
      s(6,3)=d(104)+ro*d(107)
      s(7,3)=d(105)+ro*d(108)
      goto 20
c
c...cs is pl
c
18    did=d(106)-f_dot(d(103),sc(1))
      s(5,3)=sc(1)+did*d(103)
      s(6,3)=sc(2)+did*d(104)
      s(7,3)=sc(3)+did*d(105)
c
c...vp 4/1/98 test if tool is perp to cs and sitting on it
c
      a = d(103)*t(4,3) + d(104)*t(5,3) + d(105)*t(6,3)
      if (dabs(a) .gt. .9999 .and. dabs(did) .lt. sc(27)) then
         call conv8_4 (sc(1),p4,3)
         call point_on_plane4 (s(5,1),p4,t(4,3),t4)
         call point_on_plane4 (s(5,2),p4,t(4,3),fw4)
         call vcmnvc4 (t4,p4,t4)
         call vcmnvc4 (fw4,p4,fw4)
         call vcplvc4 (t4,fw4,fw4)
         call unitvc4 (fw4,fw4)
         lcsp = .true.
      end if

      goto 20
c
c...cs is srf
c
19    s(8,3)=sc(1)
      s(9,3)=sc(2)
      s(10,3)=sc(3)
      isrf=3
      u=.5
      v=.5
      call surfpn(u,v,1)
c               do fwd te to cspt and compare to tfwd
20    fx=s(5,3)-sc(1)
      fy=s(6,3)-sc(2)
      fz=s(7,3)-sc(3)
c               add to tfwd (if dist real)           12-feb-88
      sec=sqrt(fx**2+fy**2+fz**2)
      if (sec.gt..001) goto 206
c
c...dist=0. means tool is already on cs. simply add csnorm
c...to tfwd to give some fwd sense       19-feb-88
c
c...vp 4/3/98 apply calculated above fwd sense using ds and ps
c...see above where lcsp flag is set
c
      if (lcsp) then
        call vctovc4 (fw4,t(7,3))
        goto 22
      else
         if (f_mag4(s(1,3)).le.0.1 .and. .not.lv91) then
c
c..... Eduard 11/9/99.
c..... if there is a SRFVCT direction, and the fwd is zero at the moment,
c..... set fwd as +/- that direction, depending on CS modifier.
c
            if (lcssvc) then
              call unitizevc(cssfvc)
              call conv8_4 (cssfvc,t(7,3),3)
              if (sc(36) .eq. -1) call mnvc4(t(7,3))
            endif
         else
            call vctovc4 (s(1,3),t(7,3))
         endif
         goto 21
      endif

206   t(7,3)=fx/sec
      t(8,3)=fy/sec
      t(9,3)=fz/sec
21    continue
c               proj tfwd on dspl
      si=t(7,3)*s(1,2)+t(8,3)*s(2,2)+t(9,3)*s(3,2)
      t(7,3)=t(7,3)-si*s(1,2)
      t(8,3)=t(8,3)-si*s(2,2)
      t(9,3)=t(9,3)-si*s(3,2)
      co=fx*t(7,3)+fy*t(8,3)+fz*t(9,3)
c
c... fwd opposes tfwd.  chg tfwd, ds rgt sense, and s(,2)
c
      if(co.le.0.) call mnvc4 (t(7,3))
 
22    continue
C
C...   If check surface SRFVCT opposes fwd, reverse TO,PAST.
c
      if (lcssvc) then
        co = t(7,3)*cssfvc(1)+t(8,3)*cssfvc(2)+t(9,3)*cssfvc(3)
        if (co .lt. 0.0d0) sc(36) = -sc(36)
      endif
c               verify ds rgt
      rx=t(8,3)*sc(6)-t(9,3)*sc(5)
      ry=t(9,3)*sc(4)-t(7,3)*sc(6)
      rz=t(7,3)*sc(5)-t(8,3)*sc(4)
      if(rx*s(1,2)+ry*s(2,2)+rz*s(3,2).ge.0.)goto 26
      if (kds.ne.CURVE) ad(102)=-ad(102)
      do 24 i=1,4
24    s(i,2)=-s(i,2)
c               set tlrgt,on,lft   per to,on,past ds in go/command.
c                assume tlrgt and correct as nec
26    ifl(21)=1
      if(hgo(11).ne.CS_ON) goto 28
         ifl(21)=0
         goto 39
28    rdis=s(1,2)*sc(1)+s(2,2)*sc(2)+s(3,2)*sc(3)-s(4,2)

C...   If fwd calculated from drive surface SRFVCT opposes fwd, reverse
C...   TLLFT, TLRGT.
c
      if (ldssvc) then
        fx=sc(5)*dssfvc(3)-sc(6)*dssfvc(2)
        fy=sc(6)*dssfvc(1)-sc(4)*dssfvc(3)
        fz=sc(4)*dssfvc(2)-sc(5)*dssfvc(1)
        rdis = 1.0
        if (t(7,3)*fx+t(8,3)*fy+t(9,3)*fz .gt. 0.0) rdis = -1.0
      endif

      if(rdis.ge.0.)goto 30
      if(hgo(11).eq.CS_PAST) goto 39
      goto 32
30    if(hgo(11).eq.CS_TO) goto 39
32    ifl(21)=-1
39    continue
c                if tlrgt or tllft, set ifl(57) ds calc flag    29-apr-86
c                unless one of the tands modes                  28-ju-86
      if (ifl(21).ne.0 .and. (ifl(23).le.2 .or. ifl(23).ge.7))
     *  ifl(57) = 1
c
c.... if error here we need to reset ifl(21) and ifl(42) since the routine
c.... will not be reentered with itim = 2
c
      if (ifl(2) .gt. 0) goto 90
      goto 99
c              time 2.
c
50    continue
      if (itim.eq.3)goto 96
      if(ifl(2).gt.0)goto 90
c              restore flags to input state
      ifl(42)=kgo(1)
      ifl(21)=kgo(2)
cc      if (lexpcl) then
      if (.not. lv92) then
        x8 = sc(1)-hgo(1)
        y8 = sc(2)-hgo(2)
        z8 = sc(3)-hgo(3)
        sec = dsqrt(x8**2+y8**2+z8**2)
        if (sec.gt..001)then
          ifl(22) = 0
          sc(7) = x8/sec
          sc(8) = y8/sec
          sc(9) = z8/sec
          call vctovc (sc(7),hldctp(7))
        else
          call xyzvc (ZERO,ZERO,ZERO,hldctp(7))
        endif

        if (ifl(42).eq.0 .or. (ifl(42) .eq. 1 .and. ifl(246) .ne. 0))
     1          then
cc          call plotm (hldctp,lfalse)
          lnk = 1
          jptk = 1
          iclass = 5200
cc          if (ifl(42) .eq. 1) iclass = 5300
          isubcl = 1
          ds(1) = hldctp(19)
          ds(2) = hldctp(20)
          ds(3) = hldctp(21)
          call outmot (tdat,hldctp,ds,lnk,jptk,iclass,isubcl)
          if (ntk.gt.0) then
            numitm = 1
            if (ifl(42) .eq. 0) then
                call putcl5 (iclass,isubcl,numitm,tdat)
            else
                call putcl5 (iclass+100,isubcl,numitm,tdat)
            endif
            ntk = 0
          endif
          call ptpnum (nn)

          if (nn .gt. 0) then
            call csdist (iclass, isubcl)
            call ptclos
          endif

          do 55 i=1,6
55        hldctp(i+6) = hldctp(i+21)

          if (ifl(42) .eq. 0) then
              iclass = 5220
              isubcl = 3
              numitm = 13
c
c... jingrong 01/18/00: if OPENNCL modfy before writing to 5220 cl-rec.
c
              if (ifl(330).eq.1) call modfy (hldctp,hldctp)
              call putcl (iclass,isubcl,numitm,hldctp)
            endif
        endif
c
c......Display ending point to window 3
c
        if (motdfl) then
            write (cout,151)
151         format (12x,'x',11x,'y',11x,'z',11x,'i',11x,'j',11x,'k')
            call putmsg (cout,80,15,0)
            call conv8_8(sc,bj,6)
            if (ifl(73).eq.1.and.ifl(267).eq.1) then
              call conent(bj,sc(41),3)
              call conent(bj(4),sc(41),4)
            endif
            write (cout,1511) (bj(i),i=1,6)
1511        format (4x,3f12.4,3f12.6)
            call putmsg (cout,80,16,0)
            cout=' '
c   
c...Added check for NCL-VT mode    
c...Paul  -  10/3/91    
c...Old version was:    
c   if (.not.ksr.and.ifl(35).eq.0) call ersw3 (17,1)
c
            if (.not.ksr .and. (ifl(35) .eq. 0 .or. ifl(35) .eq. 2) 
     *         .and. ifl(131) .eq. 0)   call ersw3 (17,1)
        endif
      else

c              build a goto/xyzijk  in sc(10),restore sc(1-6), and call motimm
        ksc(37)=703
        ksc(38)=2
        do 60 i=1,6
        sc(i+10)=sc(i)
60      sc(i)=hgo(i)
        call motimm
      endif
c                            save off ps,ds,cs keys & ending u & v
      call gtdesc (hgo(13),kuv(1),nwds,ietype)
      call gtdesc (hgo(12),kuv(2),nwds,ietype)
      call gtdesc (hgo(15),kuv(3),nwds,ietype)

      hu(1)=t(13,ic)
      hv(1)=t(14,ic)
      hu(2)=t(19,ic)
      hv(2)=t(20,ic)
      hu(3)=t(25,ic)
      hv(3)=t(26,ic)
      autouv = auvset
      ldssvc = .false.
      lcssvc = .false.

      goto 95
c              re-store params, flags to input state.
90    ifl(42)=kgo(1)
      ifl(21)=kgo(2)
95    sc(27)=hgo(16)
      sc(54)=hgo(17)
      ifl(91)=kgo(3)
      ifl(154)=kgo(5)
      ifl(23)=kgo(6)
96    if(ipschg.eq.0) goto 99
c
c... restore d(1-6) to input stat and set actpat=0
c
      call conv8_8 (schld,d,6)
      jd(41)=0
      ipschg=0
c                  turn off nops flag now
99    ifl(276)=0
c                  do not error exit with chgd ps
      if(ifl(2).gt.0.and.ipschg.ne.0)goto 96
999   return
      end
