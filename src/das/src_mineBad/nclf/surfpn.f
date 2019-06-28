C*********************************************************************
C*    NAME         :  surfpn.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       surfpn.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:46
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine surfpn (u,v,itrfl)
c*        this routine is called when a srf p/n is reqd.
c*        activate patch/panel as nec and call ipatch to calc
c*        the actual srf data.
C*    PARAMETERS   
C*       INPUT  : 
C*          u     = U-parameter to evaluate surface at.
C*          v     = V-parameter to evaluate surface at.
C*          itrfl = 1 - Determine if evaluated point is within the
C*                      boundaries of a trimmed surface when a net
C*                      surface is being evaluated.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine surfpn (u,v,itrfl)
c
      include 'com4a.com'
      include 'mocom.com'
      include 'drvcom.com'
c
      integer*2 itrfl
c

c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)

      real*4 ad(300)
      integer*2 kd(600)
      equivalence (d,ad,kd)
      equivalence (ifl(54),isrf)
      integer*2 nwds,ietype
      integer*4 srfkey
      logical lloopd,lv91,lv100
      real*8 data(16),s8(7),tmp(3),tmp1(6),f_dot,co,si,npt(3),v1(3)
      integer*2 izro /0/
      integer*4 kcps
      equivalence (kcps,ifl(337))

      lv91 = sc(169).lt.9.149d0
      lv100 = sc(169).lt.10.103d0
      ifl(336) = 0
      idx=(isrf-1)*50
      jdx=idx*2
      kdx=idx*4
      lloopd=noclos
c             get surface unibase key
      call gtdesc (sc(isrf+143), srfkey, nwds, ietype)
      call sftype (srfkey, isftyp)
c+++                                    super mesh surface
      if (isftyp.eq.27) then
        call ssrfpn(u,v,itrfl)
        go to 999
      endif
c+++                                    quilt srf
      if(isftyp.eq.25) then
        call qsrfpn(u,v)
        goto 999
      endif
c+++                                    wf surface
      if (ifl(270). eq. isrf) then
        iwf = 1
      else
        call isitwf (srfkey, iwf)
      endif
10    if (iwf.eq.1) then
        call usrfpn (u,v,iret)
c+++                                    mesh surface
      else if (isftyp.eq.26) then
        call meshpn (u,v,iret)
c+++                                    ncl surface
      else
        call nsrfpn (u,v,iret)
      endif

      if (iret.eq.0.or.iret.eq.3) goto 999
      if (lloopd) goto 999
      lloopd = .true.
      if (iabs(iret).eq.2) goto 200
      call gtclsd (srfkey, 0, iclsd)
      if (iclsd.eq.0) goto 900
      u=1.-u
      if (isftyp.eq.26) then
        kd(kdx+39)=0
      else
        kd(kdx+41)=0
      endif
      goto 10

200   continue
      call gtclsd (srfkey, 1, iclsd)
      if (iclsd.eq.0) goto 900
      v=1.-v
      if (isftyp.eq.26) then
        kd(kdx+39)=0
        goto 10
      endif
      if (isftyp.ne.91) goto 10
c++                               for NCL sf, ensure correct panel is loaded
      npans=kd(kdx+2)
      if (npans.eq.1) goto 10
      ipan=npans-kd(kdx+42)+1
c          get panel into panhed
      ipnx=mxpnhd*(isrf-1)+1
      call gtspan(srfkey,ipan,panhed(ipnx))
      d(idx+10)=panhed(ipnx)
      kd(kdx+42)=ipan
      fnpans=npans
c          set v min/max
      fipan=ipan
      ad(jdx+18)=fipan/fnpans
      ad(jdx+17)=ad(jdx+18)-1./fnpans
c          if v o/b, set it halfway
      if(v.lt.ad(jdx+17).or.v.gt.ad(jdx+18))
     1  v=(ad(jdx+17)+ad(jdx+18))/2.
c          force a new pat sch
      kd(kdx+41)=0
      goto 10

900   continue
      if (iret.ne.0) ifl(336) = 1
999   continue
c
c... jingrong if a special primitive, get exact data.
c
      if (lv91 .or. ifl(2).gt.0) goto 9999
      if (isftyp.ne.28 .and. isftyp.ne.29 .and. isftyp.ne.91 .and.
     *    isftyp.ne.99 .and. isftyp.ne.100 ) goto 9999 
      if (isrf .eq. 1 .and. kcps .ne. 0) goto 9999
c
c..... isftyp = 28 means NCL_EVALSF_REL, in particular any NCLX single (not a net) 
c..... surface (note NCLX_MDL_COMPOSITE also gets isftyp = 28)
c..... isftyp = 29 means UM_RBSPLSRF_REL
c..... isftyp = 91 means NCL_SURF_REL
c..... isftyp = 99 means NCL_TRIMSF_REL
c..... isftyp =100 means NCL_REVSURF_REL
c
      call gtprimt (srfkey, izro, ietype, data)
      if (ietype.le.2) goto 9999
c
      call conv4_8 (srf(5,isrf),s8(5),3)
c
      if (ietype .eq. PLANAR) then
         call vctovc(data(1),s8(1))
         call point_on_plane(s8(5),data(5),data(1),s8(5))
c
      else if (ietype .eq. SPHERE) then
         call vcmnvc(s8(5),data(1),s8(1))
         call unitvc(s8(1),s8(1))
         call uvcplvc(data(1),s8(1),s8(5),data(4))
c
      else if (ietype .eq. CYLINDER) then
         call point_on_line(s8(5),data(1),tmp)
         call vcmnvc(s8(5),tmp,s8(1))
         call unitvc(s8(1),s8(1))
         call uvcplvc(tmp,s8(1),s8(5),data(7))
c
      else if (ietype .eq. CONE) then
         call vcmnvc(s8(5),data(1),tmp)	
         co = f_dot(tmp,data(4))
         if (co .lt. 0.d0) then
            co = -co
            tmp(1) = -tmp(1)
            tmp(2) = -tmp(2)
            tmp(3) = -tmp(3)
         endif
         tmp1(1) = tmp(1) - co*data(4)
         tmp1(2) = tmp(2) - co*data(5)
         tmp1(3) = tmp(3) - co*data(6)
         call unitvc(tmp1,tmp1)
         co = dcos(data(7))
         si = dsin(data(7))
         tmp1(4) = co*data(4) + si*tmp1(1)
         tmp1(5) = co*data(5) + si*tmp1(2)
         tmp1(6) = co*data(6) + si*tmp1(3)
         call unitvc(tmp1(4),tmp1(4))
         call vctovc(data(1),tmp1(1))
         call point_on_line(s8(5),tmp1(1),s8(5))
         call f_cross(data(4),tmp,tmp)
         call f_cross(tmp1(4),tmp,s8(1))
         call unitvc(s8(1),s8(1))
c
      else if (ietype .eq. TORUS) then
         if (lv100) goto 9999
         call point_on_line(s8(5),data(1),tmp)
         call vcmnvc(s8(5),tmp,v1)
         call unitvc(v1,v1)
         call uvcplvc(data(1),v1,npt,data(7))
         call vcmnvc(s8(5),npt,s8(1))
         call unitvc(s8(1),s8(1))
         call uvcplvc(npt,s8(1),s8(5),data(8))
c
c... Unrecognized primitive
c
      else
          go to 9999
      end if
      co = srf(1,isrf)*s8(1) + srf(2,isrf)*s8(2) + srf(3,isrf)*s8(3)
      if (co .lt. 0.d0) call mnvc(s8(1))
      s8(4) = f_dot(s8(1),s8(5))
      call conv8_4(s8(1),srf(1,isrf),7)
9999  return
      end
