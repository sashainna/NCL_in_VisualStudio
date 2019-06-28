C*********************************************************************
C*    NAME         :  psproj.f
C*       CONTAINS:
C*           psini1 psreset fwdini tltos1
C*    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       psproj.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 17:16:52
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine psini1 (khow,asw,te,ta)
C*      Initialize the part surface. Copied form psinit, with the added
C*      search for good initial (u,v). Here real units (inches or MM)
C*      are used.
C*    PARAMETERS
C*       INPUT  :
C*          khow     = 1 = Project tool to surface, 2 = Project point
C*                     onto surface
C*          asw      - ASW of part surface
C*          te       - Tool end point.
C*          ta       - Tool axis.
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine psini1 (khow,asw,te,ta)

      include 'com.com'
      include 'mocom.com'
      include 'suvcom.com'

      integer*4 khow
      real*8 asw, te(3), ta(3)
c
c..... motion common equivalences
c
      real*4 ad(300)
      integer*2 kd(600)
      equivalence (d,ad,kd)

      real*4 asc(100), dsk
      equivalence (sc,asc),(asc(66),dsk)

      integer*2 ia, isrf
      equivalence (ifl(51),ia),(ifl(54),isrf)
c
c..... local variables
c
      real*8 f_dot
      real*8 svc(3),d0,dij,tol,tolsq
      real*4 u,v,u0,v0

      call getsct (tol)

      ia = 3
      isrf = 1

      call conv8_4 (te,t(1,ia),3)
      call conv8_4 (ta,t(4,ia),3)
      call xyzvc4  (0.,0.,0.,t(7,ia))

      if (khow .eq. 1) then
        ifl(56) = 0
c
c...  If disk or flat bottom cutter, set ps calc level to 1 for psrela
c
        if (dsk.ne.0. .or. tool(6).ge.2.*tol) then
          ifl(56) = 1
          call xyzvc4 (0.,0.,0.,t(16,ia))
        endif
c
c..... use ps 1st-look pt
c
        s(8,1)  = te(1)+ta(1)*tool(4)
        s(9,1)  = te(2)+ta(2)*tool(4)
        s(10,1) = te(3)+ta(3)*tool(4)
      else
        s(8,1)  = te(1)
        s(9,1)  = te(2)
        s(10,1) = te(3)
      endif
c
c..... get ps into d-tbl.   record this ps in sc(35)
c
      sc(35) = asw
      sc(144) = asw
      ad(2) = 1.

      u = psu
      v = psv

      call sfinit (asw, isrf, u, v)

      call surfpn (u,v,1)
      if (ifl(2).gt.0) return

      tolsq = tol*tol

      svc(1) = s(8,1) - s(5,1)
      svc(2) = s(9,1) - s(6,1)
      svc(3) = s(10,1) - s(7,1)
      d0 = f_dot(svc,svc)
      u0 = u
      v0 = v
      if (d0 .lt. tolsq) goto 50

      do i = 1,3
        do j = 1,3
          u = 0.5*(i-1)
          v = 0.5*(j-1)
          if (abs(u-psu).gt.0.001 .or. abs(v-psv).gt.0.001) then
            call surfpn (u,v,1)
            if (ifl(2).eq.0) then
               svc(1) = s(8,1) - s(5,1)
               svc(2) = s(9,1) - s(6,1)
               svc(3) = s(10,1) - s(7,1)
               dij = f_dot(svc,svc)
               if (dij .lt. tolsq) goto 50
               if (dij .lt. d0) then
                 u0 = u
                 v0 = v
                 d0 = dij
               endif
            endif
          endif
        enddo
      enddo

      u = u0
      v = v0

50    continue
      t(13,3) = u
      t(14,3) = v
      sfa = s(1,1)
      sfb = s(2,1)
      sfc = s(3,1)
c
c..... if psnorm not up, reverse direc(1) and ps tanpl.
c
      if (ta(1)*sfa+ta(2)*sfb+ta(3)*sfc .gt. 0.) return

      ad(2)=-ad(2)
      call mnvc4 (s(1,1))
      s(4,1) = -s(4,1)

999   return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : psreset ()
C*      Initializes certain part surface variables. Typically used when
C*      switching between CVonSF composite curve components.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine psreset
c
      include 'com.com'
      include 'mocom.com'
c
      real*4 ad(300)
c
      equivalence (d,ad)
c
c...Initialize Part Surface variables
c
      ad(2) = 0.
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fwdini
C*      Initialize the fwd direction, based on the tool axis only
C*********************************************************************
      subroutine fwdini

      include 'com.com'
      include 'mocom.com'

      integer*2 ia
      equivalence (ifl(51),ia)

      real*8 vec(3),nvec(3)
      real*8 rx,ry,rz,sec

      vec(1) = t(4,ia)
      vec(2) = t(5,ia)
      vec(3) = t(6,ia)

      nvec(1) = 0.0
      nvec(2) = 0.0
      nvec(3) = 0.0

      if (dabs(vec(2)).gt..9999 .or. dabs(vec(3)).gt..9999) then
c
c...  Vector is a Y or Z vector, use X vector
c
        nvec(1) = -1.
        if (vec(2).lt.-.999.or.vec(3).lt.-.999) nvec(1) = 1.
        goto 50
      endif
      if (dabs(vec(1)).gt..9999) then
c
c...  Vector is a X vector, use Y vector
c
        nvec(2)=-1.
        if (vec(1).lt.0.) nvec(2)=1.
        goto 50
      endif
c
c...  Vector is not pure X, Y or Z vector. Get an approximate vector,
c...  cross twice to get a vector in the plane of the circle & unitize.
c
      if (vec(1).lt..9 .and. vec(1).ge.0.) then
        nvec(1)=-1.
      else if (vec(1).gt.-.9) then
        nvec(1)=1.
      else if (vec(1).lt.0.) then
        nvec(2)=1.
      else
        nvec(2)=-1.
      endif

      rx=vec(2)*nvec(3)-vec(3)*nvec(2)
      ry=vec(3)*nvec(1)-vec(1)*nvec(3)
      rz=vec(1)*nvec(2)-vec(2)*nvec(1)
      nvec(1)=ry*vec(3)-rz*vec(2)
      nvec(2)=rz*vec(1)-rx*vec(3)
      nvec(3)=rx*vec(2)-ry*vec(1)

      sec = dsqrt(nvec(1)**2+nvec(2)**2+nvec(3)**2)
      if (sec.eq.0.) sec=1.
      nvec(1)=nvec(1)/sec
      nvec(2)=nvec(2)/sec
      nvec(3)=nvec(3)/sec

50    t(7,ia) = nvec(1)
      t(8,ia) = nvec(2)
      t(9,ia) = nvec(3)

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine tltos1 (khow, te, ta, tp, sn, partsf)
C*      This routine either projects a point onto the part surface or
C*      moves the tool down the tool axis to the part surface, depending
C*      on the value of 'khow'.
C*    PARAMETERS
C*       INPUT  :
C*          khow     = 1 = Project tool to surface, 2 = Project point
C*                     onto surface, normally used to obtain the surface
C*                     normal to use for future projections of the tool to
C*                     the surface (khow=1).
C*          te       - Tool end point.
C*          ta       - Tool axis.  This vector is ignored when 'khow' = 2.
C*          partsf   - Part Surface ID
C*       OUTPUT :
C*          te       - New tool end point.
C*          tp       - Tool contact point on surface.
C*          sn       - Surface normal at contact point.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
      subroutine tltos1 (khow, te, ta, tp, sn, partsf)
c
      include 'mocom.com'
c
      integer*4 khow
      real*8 te(3), ta(3), tp(3), sn(3), partsf
c
c..... motion common equivalences
c
      real*4 ad(300)
      integer*2 kd(600)
      equivalence (d,ad,kd)
c
      integer*2 isrf
c
      real*4 ss(9),dir
c
c...Project point ON part surface
c...Projects normal to PS rather than along a vector
c...Normally used to obtain PS normal vector for
c...further projection of tool along this vector
c
      if (khow .eq. 2) then
          isrf = 1
          dir = 1.
          call sfpt (partsf,te,isrf,dir,t(13,3),t(14,3),ss)
          te(1) = ss(5)
          te(2) = ss(6)
          te(3) = ss(7)
          tp(1) = ss(5)
          tp(2) = ss(6)
          tp(3) = ss(7)
          sn(1) = ss(1)
          sn(2) = ss(2)
          sn(3) = ss(3)
c
c...Project tool TO part surface
c
      else
          call tltosf (te, ta, tp, sn, partsf)
      endif
c
c...End of routine
c
      return
      end
