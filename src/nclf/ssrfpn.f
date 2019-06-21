c*****************************************************
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       ssrfpn.f , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 15:10:45
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1986 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: SSRFPN
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C **       THIS ROUTINE IS CALLED WHEN A NET (FORMERLY SUPER) SURFACE 
C **       POINT NORMAL IS REQD. MESHPN & SURFPN DO THE ACTUAL WORK.
C **
C **    INPUT:
C **       u     = U-parameter to evaluate surface at.
C **       v     = V-parameter to evaluate surface at.
C **       itrfl = 1 - Determine if evaluated point is within the
C **                   boundaries of the selected trimmed surface.
C **
C **********************************************************************
C **********************************************************************
      subroutine ssrfpn (u,v,itrfl)

      include 'com.com'
      include 'mocom.com'
      include 'drvcom.com'
c
      integer*2 itrfl
c
c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)

      integer*2 kd(600)
      real*4 ad(300)
      integer*2 ia,isrf,itfl
      equivalence (d,ad,kd),(ifl(54),isrf)
      equivalence (itfl,ifl(49)),(ia,ifl(51))

      real*8 asn, svasw, vx,vy,vz, svx,svy,svz, sec, co1, co2
      real*4 svsrf(7),svdir,dis,svdis,u,v
      real*4 sva,svb,svc, fpans, svu,svv
      integer*4 nclkey,ikey,ipnkey,ikeysv
      integer*2 i, j, isf, nsf, isvsf, isftyp, iloop, nwds, ietype
      integer*2 ipan, ipx, iret, isid,itypsv
      integer*2 iextsf,isx, idx,jdx,kdx, isdx,jsdx,ksdx
      logical lused(40),newsf,ldsgck,lv97
      save isvsf

      lv97 = sc(169).lt.9.749d0

      iloop = 0
      newsf=.false.
      svdir=1.
      svdis=1.e7
      iextsf=0
      if (itfl .le. 0) isvsf = 0
      svasw=sc(143+isrf)
c          set ptrs to d-tbl
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
      isx=mxsfhd*(isrf-1)
c          set ptrs to srf in super d-tbl
      isdx=82*(isrf-1)
      jsdx=2*isdx
      ksdx=4*isdx

      ldsgck = isrf.eq.2 .and. ksd(ksdx+100).eq.1

      nsf=ksd(ksdx+2)
      sva=srf(1,isrf)
      svb=srf(2,isrf)
      svc=srf(3,isrf)
      do 10 i=1,nsf
10      lused(i)=.false.
      isf=ksd(ksdx+3)
      isftyp=ksd(ksdx+4)
      if (isf.eq.0) goto 15
      kd(kdx+1)=isftyp
      ikey=jsd(jsdx+isf+4)
      call ptdesc(ikey,9,asn)
      sc(143+isrf)=asn
      goto 40
15    newsf=.true.
      asn=sd(isdx+2)
      call gtdesc(asn,nclkey,i,j)
      call gtgeo(nclkey,sd(isdx+2))
      sd(isdx+2)=asn

25    isf=isf+1
      if (isf.gt.nsf) goto 110
      if (lused(isf)) goto 25

30    ksd(ksdx+3)=isf
      lused(isf)=.true.
      ikey=jsd(jsdx+isf+4)
      call ptdesc(ikey,9,asn)
      sc(143+isrf)=asn
c
c...   If tlaxis is PARELM, set rldinu from surface or set up to 
c...   get it from closest u or v surface vector.
c
      if (isrf.eq.2 .and. ifl(23).eq.6) then
        call gtrld (ikey, i)
        if (i.ge.0) then
          rldinu = i.eq.1
        else
          rldinu = .true.
          iloop = 2
        endif
      endif
      call gtgeom(asn,srfhed(isx+1),ikey,nwds,ietype)
c
c...Probably OML surface
c
      if (srfhed(isx+1) .eq. 0) then
          d(idx+1) = 0.
          d(idx+2) = 0.
          ad(jdx+2) = 1.
          kd(kdx+1) = 9
      else
        d(idx+1)=srfhed(isx+1)
        d(idx+2)=srfhed(isx+2)
      endif
      isftyp=kd(kdx+1)
      kd(kdx+1)=27
      call isitwf (ikey, i)
      if (i.eq.1) isftyp = 1
      ksd(ksdx+4)=isftyp
      if (isftyp.eq.1) goto 38
      if (isftyp.eq.26) goto 36
      ipan=1
      ipx=mxpnhd*(isrf-1)
      call gtspa1(ikey,ipan,panhed(ipx+1),ipnkey)
      d(idx+10)=panhed(ipx+1)
      kd(kdx+41)=0
      kd(kdx+42)=1
      fpans=kd(kdx+2)
      ad(jdx+17)=0.
      ad(jdx+18)=1./fpans
      ksd(ksdx+4)=9
      if(kd(kdx+37).eq.1)ksd(ksdx+4)=21
      kd(kdx+1)=ksd(ksdx+4)
      goto 38

36    kd(kdx+37)=kd(kdx+4)
      kd(kdx+38)=kd(kdx+3)
      kd(kdx+39)=0
      kd(kdx+1)=26
      sc(143+isrf)=asn
38    ad(jdx+2)=svdir
c
c..... if CS and itfl>0, save selected isf even if extension or edge
c..... if this isf = saved, use the passed U,V instead of the center
c
      if (isrf.eq.3 .and. isvsf.eq.isf) then
        u = t(25,ia)
        v = t(26,ia)
      else
        u=.5
        v=.5
      endif
40    if (isftyp.eq.26) then
        call meshpn (u,v,iret)
      else if (isftyp.eq.1) then
        call evstup (ikey, isrf)
        call usrfpn(u,v,iret)
        if (iret .eq. 0 .and. itrfl .eq. 1 .and. .not. lv97) then
            call ncl_pt_inside_trimsf (ikey,u,v,sc(27),iret)
        endif
      else
        call nsrfpn (u,v,iret)
      endif
      if (iret.eq.3) then
        ifl(2)=0
        isf=0
        goto 25
      endif
c
c...   Set rldinu from u or v surface vector closest to tool axis.
c
      if (iloop.gt.0) then
        iloop = iloop-1
        vx = t(22,ia)
        vy = t(23,ia)
        vz = t(24,ia)
        sec = dsqrt(vx**2+vy**2+vz**2)
        if (sec.eq.0.) sec=1.
        if (iloop.eq.1) then
          co1 = (vx*t(4,ia)+vy*t(5,ia)+vz*t(6,ia))/sec
          rldinu = .false.
          svx = vx
          svy = vy
          svz = vz
          goto 40
        endif
        co2 = (vx*t(4,ia)+vy*t(5,ia)+vz*t(6,ia))/sec
        rldinu = dabs(co1) .ge. dabs(co2)
        if (rldinu) then
          t(22,ia) = svx
          t(23,ia) = svy
          t(24,ia) = svz
        endif
      endif
      if (iret.eq.0) then
        isvsf = isf
        goto 120
      endif
      newsf=.true.
      dis=sqrt((srf(5,isrf)-srf(8,isrf))**2
     1        +(srf(6,isrf)-srf(9,isrf))**2
     2        +(srf(7,isrf)-srf(10,isrf))**2)
      if (dis.gt.svdis) goto 60
      iextsf=isf
      svdis=dis
      svu=u
      svv=v
      if (ldsgck) then
        itypsv=ksd(ksdx+4)
        ikeysv=jsd(jsdx+isf+4)
      endif
      do 50 i=1,7
50    svsrf(i)=srf(i,isrf)
60    continue
      isid=iret*2
      if (iret.lt.0)isid=-1-isid
      isf=ksd(ksdx+isf*4+isid+84)
      if (isf.eq.0) goto 25
      if (.not.lused(isf)) goto 30
      isf=0
      goto 25

110   ksd(ksdx+3)=0
      if (iextsf.eq.0) then
        ifl(2)=128
        goto 999
      endif
      isvsf = iextsf
c
c..... if DS gouge checking: save data even if on extension
c
      if (ldsgck) then
        ksd(ksdx+3)=iextsf
        ksd(ksdx+4)=itypsv
        jsd(jsdx+iextsf+4)=ikeysv
      endif
      u=svu
      v=svv
      do 112 i=1,7
112   srf(i,isrf)=svsrf(i)

120   continue
      if (.not.newsf) goto 999
      if (sva*srf(1,isrf)+svb*srf(2,isrf)+svc*srf(3,isrf).gt.0.)
     x  goto 200
      do 130 i=1,4
130   srf(i,isrf)=-srf(i,isrf)
      ad(jdx+2)=-1.

200   continue

999   kd(kdx+1)=27
      sc(143+isrf)=svasw
      return
      end
