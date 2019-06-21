c***********************************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
C*       curvpv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:46
c**
c***********************************************************************
C**
C** COPYRIGHT (C) 1984 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: CURVPV                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C **       SOLVES A PV THIS CV OR CI PER CURRENT TOOL LOC.            **
C **       CV IS IN D-TBL AND 1ST-TRY EPT IS IN S(8,,ISRF).           **
C **********************************************************************
C **********************************************************************

      subroutine curvpv (px,py,pz,vx,vy,vz)

      include 'com4a.com'
      include 'mocom.com'
      include 'gidcm1.com'
      include 'suvcom.com'

      parameter (maxpt=50)
      parameter (mxcvhd=(2+(maxpt+1)/2))

      real*8 asn
      integer*4 nclkey
      integer*2 kd(600),ksn(4),ietype,nwds
      real*4 ad(300),a(3),b(3)
      equivalence(d,ad,kd),(itfl,ifl(49)),(iptk,ifl(50)),(asn,ksn)
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      real*8 crvhed(mxcvhd),tpar(3)
      real*4 r4cvhd(mxcvhd*2)
      equivalence (crvhed,r4cvhd)
      real*4 h,herr,u,su,slmx,slmn,smid
      integer*2 iwf

      real*8 aa(3),bb(3),pp(3), f_dist, du,uu,vv,adis,dis

c..... index to d-tbl
c
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
c
c..... pt to h-val in tcol(ia) and set misc params
c
      jtim=0
      iknt=0
      ihx=6*isrf+9
      iux=ihx-2
      h=t(ihx,ia)
      asn=d(idx+2)
      call gtdesc(asn,nclkey,nwds,ietype)
c
c..... call ucrvpv for unicad curve
c
      call isitwf(nclkey,iwf)
c
c..... Eduard, 3/16/99. Call ucrvpv anyway for versions newer than 9.05.
c..... 11/23/99. Do it only when there is no user defined initial parameter.
c
      if (sc(169).gt.9.05d0 .and.
     x    .not.csuv .and. isrf.eq.3 .and. csmult) then
        call evstup(nclkey, isrf)
c
c... find a rough value for the curve parameter near the current tool location
c
        dis=1.d12
        du = 0.11111111
        vv = 0.d0
        uu = 0.d0
        call conv4_8 (t(1,ia),pp,3)
        do 5 j=1,10
          if (vv.gt.1.0d0) vv = 1.0d0
          call uevcvt (vv,isrf,ifl(72),aa,bb,ifl(2))
          if (ifl(2) .gt. 0) goto 99
          adis = f_dist(pp,aa)
          if (adis.lt.dis) then
            dis = adis
            uu = vv
          endif
          vv=vv+du
5       continue
        if (dis .lt. abs(t(12,ic))) t(25,ia) = uu
      endif

      if (iwf.eq.1) then
        call ucrvpv(px,py,pz,vx,vy,vz)
        goto 99
      endif

      nsegs=kd(kdx+2)
      iseg=kd(kdx+3)
      call gtcvhd (nclkey, crvhed)
      call ncl_get1_tpar (nclkey,tpar(1))
      r4cvhd(1) = 0.
c
c..... if a seg is active, go try it
c
      if (iseg.eq.0) goto 10
c
c...vp 28-apr-97 code has been modified to evaluate trimmed ncl curves.
c...Added variables slmn, slmx and smid for segment limits and medium
c...u value. Input u is converted to u' which is related to untrimmed
c...curve. Curve is evaluted for input u=(0,1) as it was before change.
c...slmn, slmx replace limits 0,1; smid replaces .5 in all applicable ifs.
c...This is a temporary solution and should be replaced by a generic
c...evaluator (see above call ucrvpv(px,py,pz,vx,vy,vz)!.
c
c
c...fix u for trim
c
      call lmsgst (tpar,r4cvhd,iseg,slmn,slmx)
      smid = .5*(slmn+slmx)
      su = tpar(1) + (t(iux,ia) * (tpar(2) - tpar(1)))
c------- end fix ------
      if (su .lt. 0.) su = 0.
      if (su .gt. r4cvhd(nsegs)) su = 1.
      su = (su-r4cvhd(iseg))/(r4cvhd(iseg+1)-r4cvhd(iseg))

      goto 40
c+-          if this is ds in reg mill stepover process, last
c+-          segno is in kd(204)                              21-apr-87
10    if(isrf.ne.2.or.kd(kdx+4).eq.0) goto 20
      iseg=kd(kdx+4)
      su = smid
      goto 30
c+-
c          insert mid-seg   ( future search? )
20    iseg=(nsegs+1)/2
      if (isrf.eq.2) then
        if (.not.dsuv.and..not.autouv) goto 30
        su = dsu
      else if (isrf.eq.3) then
        if (lcvpv) then
          su = t(25,ia)
        else
          if (.not.csuv.and..not.autouv) goto 30
          su = csu
        endif
      endif
c
c...fix u for trim
c
      su = tpar(1) + (su * (tpar(2) - tpar(1)))

      do 22 i=2,nsegs
22    if (su.lt.r4cvhd(i)) goto 24
24    if (i.gt.nsegs) i = nsegs
      iseg=i-1
      su = (su-r4cvhd(iseg))/(r4cvhd(iseg+1)-r4cvhd(iseg))
c*****************************    activate segment(iseg)
30    continue
c          read this seg + next into d-tbl
32    call gtcseg(nclkey,iseg,d(idx+3))
      call gtcseg(nclkey,iseg+1,d(idx+9))
c
c...get limits for curent segment
c
c------- fix --------
      call lmsgst (tpar,r4cvhd,iseg,slmn,slmx)
      smid = .5*(slmn+slmx)
c------- end fix -------
      rho=ad(jdx+16)
      xj=d(idx+9)-d(idx+3)
      yj=d(idx+10)-d(idx+4)
      zj=d(idx+11)-d(idx+5)
c          extrap back for r-pt
      ad(jdx+14)=xj-rho*ad(jdx+23)
      ad(jdx+15)=yj-rho*ad(jdx+24)
      ad(jdx+16)=zj-rho*ad(jdx+25)
      ad(jdx+17)=xj
      ad(jdx+18)=yj
      ad(jdx+19)=zj
c          record iseg and start at u=.5
      su = smid
      kd(kdx+3)=iseg
c******************************  apply hpt to seg in delta form
40    xe=t(1,ia)+t(4,ia)*h-d(idx+3)
      ye=t(2,ia)+t(5,ia)*h-d(idx+4)
      ze=t(3,ia)+t(6,ia)*h-d(idx+5)
      u=su
      if(u.lt.slmn) u=slmn
      if(u.gt.slmx) u=slmx
      itim=0
      ctan=1.
42    c1=(1.-u)**2
      c2=2.*u*(1.-u)
      c3=u**2
c          if here 1000 times, get out     ( safety exit )
      iknt=iknt+1
      if(iknt.gt.1000) goto 58
      do 44 i=1,3
      j=i+10+jdx
      k=j+3
      l=j+6
      a(i)=c2*ad(j)+c3*ad(k)
44    b(i)=c1*ad(j)+c2*ad(k)+c3*ad(l)-a(i)
      den= b(1)**2+b(2)**2+b(3)**2
c              guard against zero divide       7-27-82
      if(abs(den).lt.1.e-8) den=.001
      uerr=((b(1)*(xe-a(1))+b(2)*(ye-a(2))+b(3)*(ze-a(3)))/den-u)/3.
      if(abs(uerr).lt.1.e-4) goto 60
      if(itim.eq.0) goto 50
c          calc du
      den=oerr-uerr
c          if den small, use old ctan
      if(abs(den).gt.1.e-3) ctan=du/den
c          neg ctan is unreal
      if(ctan.le.0.) ctan=1.
50    du=uerr*ctan
c          make sure next u does not exceed 0,1
      if(u+du.gt.slmx) du=slmx-u
      if(u+du.lt.slmn) du=slmn-u
c          update for next iter
      itim=itim+1
      u=u+du
      oerr=uerr
      if(itim.lt.120) goto 52
c          iterk failure. funny seg?
      ifl(2)=139
      goto 99
c          if u-chg real, go again.
52    if(abs(du).gt.1.e-5) goto 42
c          stall-out.  probably 0,1 excess case this seg
c           if uerr small, accept it and go finup       11-12-81
      if(abs(uerr).le..002) goto 60
c          set next iseg per this u-case
      iseg=iseg+1
      if(u.lt.smid) iseg=iseg-2
c
c...finish if step on limit
c
c------- fix ------
      if (u .eq. slmn .and. slmn .gt. 0. .or.
     -    u .eq. slmx .and. slmx .lt. 1.) goto 60
c------- end fix -------
      if(iseg.gt.0.and.iseg.lt.nsegs) goto 30
cccccccccccccccccccccccccc
c          if cv is closed switch to other end, otherwise exit now.
      call gtclsd (nclkey, 0, iclsd)
      if (iclsd.eq.0) goto 60
      if (iseg.gt.0) goto 56
      iseg=nsegs-1
      if (sc(169).lt.8.2) iseg = nsegs
      su=1.
      goto 30
56    iseg=1
      su=0.
      goto 30
c          curvpv mystery exit
58    ifl(2)=134
      goto 99
c          u found this ept.  check h-value.
60    px=a(1)+u*b(1)
      py=a(2)+u*b(2)
      pz=a(3)+u*b(3)
      herr=t(4,ia)*(px-xe)+t(5,ia)*(py-ye)+t(6,ia)*(pz-ze)
      h=h+herr
      if (sc(169).ge.9.25) herr = abs(herr)
      if (.not.lcvpv .and. herr.lt.sc(27)*50.d0) goto 70
      if (jtim.gt.9 .or. herr.lt.sc(27)) goto 70
c     if(herr.lt.sc(27)*50.d0) goto 70
c          allow 10 h-fix
c     if(jtim.gt.9) goto 70
      jtim=jtim+1
      goto 40
c************************  normal exit path. fixup all data
70    px=px+d(idx+3)
      py=py+d(idx+4)
      pz=pz+d(idx+5)
      sec=sqrt(b(1)**2+b(2)**2+b(3)**2)
      if(sec.gt.0.) goto 75
c          error.  sec=0.
      ifl(2)=135
      goto 99
75    vx=b(1)/sec
      vy=b(2)/sec
      vz=b(3)/sec
      if (iseg.lt.1) iseg=1
      i = nsegs-1
      if (sc(169).lt.8.2) i = nsegs
      if (iseg.gt.i) iseg=i
      if (u.lt.0.) u=0.
      if (u.gt.1.) u=1.
      u=r4cvhd(iseg)+u*(r4cvhd(iseg+1)-r4cvhd(iseg))
c
c...redefine u back to trimmed curve
c
c------- fix ---------
      t(iux,ia)=(u - tpar(1))/(tpar(2) - tpar(1))
c------- end fix --------
      t(ihx,ia)=h

99    return
      end

c*****************************************************************
c     SUBROUTINE: lmsgst (tpar,r4cvhd,iseg,slmn,slmx)
c
c     FUNCTION:  Defines parameter limits for specified segment of
c                NCL curve when it is trimmed.
c
c     INPUT:  tpar   R*8  D3 - lower and upper curve limit prms.
c
c             r4cvhd R*4  D* - curve segments parametrization
c
c             iseg   I*2  D1 - segment number
c
c     OUTPUT: slmn   R*4  D1 - lower segment limit
c
c             slmx   R*4  D1 - upper segment limit
c
c*****************************************************************

      subroutine lmsgst (tpar,r4cvhd,iseg,slmn,slmx)

      parameter (maxpt=50)
      parameter (mxcvhd=(2+(maxpt+1)/2))

      real*8 tpar(2)
      real*4 r4cvhd(mxcvhd*2)
      real*4 slmn,slmx
      integer*2 iseg
c
c...Check if current segment contains upper trim limit
c...and calculate limit in local units
c
      if (tpar(2) .gt. r4cvhd(iseg) .and.
     -    tpar(2) .le. r4cvhd(iseg+1)) then
         slmx = (tpar(2) - r4cvhd(iseg))/(r4cvhd(iseg+1)-r4cvhd(iseg))
      else
         slmx = 1.0
      endif
c
c...Check if current segment contains lower trim limit
c...and calculate limit in local units
c
      if (tpar(1) .ge. r4cvhd(iseg) .and.
     -    tpar(1) .lt. r4cvhd(iseg+1)) then
         slmn = (tpar(1) - r4cvhd(iseg))/(r4cvhd(iseg+1)-r4cvhd(iseg))
      else
         slmn = .0
      endif

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvallpv(nclkey, ietype, p1, pv1)
C*      Project a point onto any curve.
C*    PARAMETERS
C*       INPUT  :
C*          nclkey       - Key of curve.
C*          ietype       - NCL type of curve.
C*          p1           - Point to project.
C*       OUTPUT :
C*          pv1          - Point on cirve and tangent vector.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cvallpv(nclkey, ietype, p1, pv1)

      include 'com.com'
      include 'mocom.com'

      integer*4 nclkey
      integer*2 ietype
      real*8 p1(3), pv1(6)
      

      integer*2 nwds, iwf, ksn(4)
      real*4 px,py,pz,vx,vy,vz
      real*8 hldt(30), asn, asw
      equivalence (asn,ksn)

      real*4 srf(10,3)
      equivalence (srf,s)
      integer*2 ia,ib,ic, isrf
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      isrf = 3
      ia   = 3
      call conv4_8(t(1,ia),hldt,30)
      asn = 0.0d0
      ksn(1) = ietype
      if (ietype .eq. line) then
        nwds = 6
      else if (ietype .eq. circle) then
        nwds = 11
      else
        nwds = 1
      endif
      call ptdsc3(nclkey,nwds,ietype,asw)
      sc(146) = asw
      d(102)  = asw
      if (ietype.eq.CURVE) then
        call isitwf(nclkey,iwf)
        if (iwf.eq.0) call gtncsg(nclkey,ksn(2))
      else
        call gtgeom(asw,d(103),nclkey,nwds,ietype)
      endif
      d(101) = asn

      t(1,ia)  = p1(1)
      t(2,ia)  = p1(2)
      t(3,ia)  = p1(3)
      t(4,ia)  = 0
      t(5,ia)  = 0
      t(6,ia)  = 0
      t(21,ia) = 0
      t(27,ia) = 0
      call acrvpv (px,py,pz,vx,vy,vz)
      pv1(1) = px
      pv1(2) = py
      pv1(3) = pz
      pv1(4) = vx
      pv1(5) = vy
      pv1(6) = vz
      call conv8_4(hldt,t(1,ia),30)

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvpv2(nclkey, pe, iflg, u8, pcv, vcv, ierr)
C*      Project a point onto a curve.
C*    PARAMETERS
C*       INPUT  :
C*          nclkey       - Key of curve.
C*          iwf          - Iff 1, do not call isitwf, and call ucrvpv
C*          pe           - Point to project.
C*          iflg         - 0=start at u value provided, >0, search for starting u.
C*          u8           - Starting u value
C*          unflg        - 0=Return point in current units, 1=return in inches
C*       OUTPUT :
C*          pcv          - Point on curve.
C*          vcv          - Tangent vector.
C*          ierr         - 0 = no error, < 0 = warning, > 0 error.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cvpv2(nclkey, iwf, pe, iflg, u8, unflg, pcv, vcv, ierr)

      include 'com.com'
      include 'mocom.com'

      integer*2 iwf
      integer*4 nclkey,iflg,unflg,ierr
      real*8 u8, pe(3), pcv(3), vcv(3)

      integer*2 n, nwds, ksn(4), iux, ihx, idx, kdx
      integer*2 ifl264
      real*4 px,py,pz,vx,vy,vz,hldt(30),u,du
      real*4 dsq, svdsq, spx, spy, spz, svx, svy, svz, svu
      real*8 asn, asw
      equivalence (asn,ksn)

      real*4 srf(10,3)
      equivalence (srf,s)
      integer*2 ia,ib,ic, isrf
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      isrf = 3
      ia   = 3
      iux = isrf*6+7
      ihx = iux+2
      idx = 50*(isrf-1)
      kdx = 4*idx
      call conv4_4(t(1,ia),hldt,30)
      nwds = 1
      call ptdsc3(nclkey,nwds,CURVE,asw)
      sc(146)  = asw
      d(idx+2) = asw
      if (iwf .eq. 0) call isitwf(nclkey,iwf)

      asn = 0.0d0
      ksn(1) = CURVE
      if (iwf .eq. 0) then
        call gtncsg(nclkey,ksn(2))
      else if (sc(169).ge.9.649) then
        ksn(3) = 1
      endif
      d(idx+1) = asn

      if (unflg.eq.1) then
       ifl264 = ifl(264)
       ifl(264) = 0
      endif

      svdsq = 1.e9
      t(1,ia)  = pe(1)
      t(2,ia)  = pe(2)
      t(3,ia)  = pe(3)
      t(4,ia)  = 0
      t(5,ia)  = 0
      t(6,ia)  = 0
      t(21,ia) = 0
      t(ihx,ia) = 0
      u = u8
      n = 1
      if (iflg .gt. 0) then
        n = iflg
        if (n.lt.3) n = 3
        du = n-1
        du = 1./du
        u  = 0.
      endif
      do i=1,n
        t(iux,ia) = u
        ifl(2) = 0
        if (iwf .eq. 0) then
          call curvpv (px,py,pz,vx,vy,vz)
        else
          call ucrvpv (px,py,pz,vx,vy,vz)
        endif
        if (ifl(2).lt.1) then
          dsq = (px-pe(1))**2+(py-pe(2))**2+(pz-pe(3))**2
          if (svdsq.gt.dsq) then
            svdsq = dsq
            spx = px
            spy = py
            spz = pz
            svx = vx
            svy = vy
            svz = vz
            svu = t(iux,ia)
          endif
        endif
        u = u+du
      enddo
      if (unflg.eq.1) ifl(264) = ifl264
      pcv(1) = spx
      pcv(2) = spy
      pcv(3) = spz
      vcv(1) = svx
      vcv(2) = svy
      vcv(3) = svz
      u8 = svu
      call conv4_4(hldt,t(1,ia),30)
      ierr = ifl(2)

999   return
      end
