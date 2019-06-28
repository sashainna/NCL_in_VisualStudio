c********************************************************************
C*    NAME         :  usrfpn.f
C*    CONTAINS:
C*                    
c*            usrfpn
c*            uv_interpol
c*            resolve_corner
c*            near_corner
c*            push_uv
c*            get_surface_param
c*            indsuv
c*            usrfpno
C*                    
C*    COPYRIGHT 1989 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       usrfpn.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:51
c*********************************************************************
C
c ********************* 72 *********************************************
C*    SUBROUTINE     : subroutine usrfpn (u,v,iret)
c*        this routine is called when a srf p/n is reqd on a non-NCL
c*        surface.
C*    PARAMETERS   
C*       INPUT  : 
C*          sfu        - u value to start at
C*          sfv        - v value to start at
C*          iret       - return value
C*                Common array SRF(8-10,isrf) contains external point
C*       OUTPUT :  
C*          sfu        - u value of calc'd point & normal
C*          sfv        - v value of calc'd point & normal
C*                Common array SRF(1:4,isrf) - normal plane
C*                Common array SRF(5:7,isrf) - point on surface
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : includes modifications to detect a corner; 
c*                   to switch them off, set do_corner = .false. 
c ********************* 72 *********************************************
C
      subroutine usrfpn (sfu, sfv, iret)

      include 'com4a.com'
      include 'const.com'
      include 'mocom.com'

      integer*4 kcps
 
      real*8 
     *       pc(3),psrf1(3),pext1(3),der(3),
     *       vec1(3),vec2(3),vec(3),psrf(3),pext(3),taxis(3),
     *       pu(3),pv(3),ddu(3),ddv(3),xn(3),xu(3),xv(3),p1(3),
     *       p2(3),n1(3),n2(3),ddu1(3),ddu2(3),ddv1(3),ddv2(3)
      real*8 
     *       u1,v1,u2,v2,u,du,v,dv,sec,delta,f_dot,f_mag,f_dist, 
     *       hu,hv,d12,d10,d20,uc,vc,sign,dis,dmin,
     *       umin,vmin,umax,vmax,uden,vden,uerr,verr,ouerr,utan,vtan,
     *       halfu,halfv,uv_tol,duv_tol,ad2,toler,dtol,del,stol 

      real*8 small/1.d-06/
c
c... iteration parameters
c
      real*8
     *       UV_ERR/1.d-4/,DUV_MIN/1.d-5/,STALL_OUT/2.d-03/,
     *       D_UV0/1.d-02/,DUV_MAX/1.d-1/
      integer*2 MAX_ITER/60/, MAX_TRY/20/

      real*8 D_U0,D_V0
      common/ds_uv0/D_U0,D_V0

      common/cviofl/cvion
      logical cvion
c
c... corner treatment parameters
c
      real*8 COR_FLAG,STEP_BACK/5.d-05/
 
      logical done,fail,corner,do_corner,lv84,lv91,lv94,uext,vext
      logical lumin,lumax,lvmin,lvmax,lv100
      integer*2 ii,n_try,cycle,initds,imoved
c
c... motion common equivalences
c
      real*4 srf(10,3)
      equivalence (srf,s)
      real*4 ad(300)
      integer*2 kd(600)
      equivalence (d,ad,kd),(ifl(54),isrf), (kcps,ifl(337))
      equivalence (ifl(67),initds)
c
c...Call old USRFPN if V8.4 or less
c
      lv84 = sc(169).lt.8.499d00 
      lv91 = sc(169).lt.9.149d00 
      lv94 = sc(169).lt.9.449d0
      lv100 = sc(169).lt.10.049d0
      if (lv84) then
          call usrfpo (sfu,sfv,iret)
          return
      endif
      uext = .false.
      vext = .false.
c
c... ia = # of current mover iteration for t(.,ia)
c
      ia = ifl(51)
      do_corner = .true.
      call conv4_8 (t(4,ia),taxis,3)
      if (f_mag(taxis) .lt. .5) do_corner = .false.

      call getsct (toler)
      dtol = 5.*toler
 
      if (ifl(330+isrf).eq.1) then
        call gtmmuv(isrf,umin,vmin,umax,vmax)
      else
        umin = ZERO
        vmin = ZERO
        umax = ONE
        vmax = ONE
      endif

      halfu = HALF*(umin + umax)
      halfv = HALF*(vmin + vmax)

      idx   = 50*(isrf-1)
      jdx   = 2*idx
      ad2   = ad(jdx+2)
      kdx   = 4*idx
      istyp = kd(kdx+1)
      iret  = 0
      if (ifl(385) .lt. 0) ifl(385) = -ifl(385)
      imid  = 0
c
c...vp 10/8/97 psis = spline on ds 
c...corner problem is solved in norm_to_cvonsf1(in simsurf)
c
      if (isrf .eq. 1 .and. kcps .ne. 0) then
         call simsurf (sfu,iret)
         go to 99
      end if
c
c... aak 15-OCT-97:
c... There are 2 iteration modes (cycle = 0 and cycle = 1);
c... mode "0" is the standard old way  of restricting utan,vtan <= 1);
c... mode "1" is the new way: utan,vtan are not restricted, but du,dv 
c... are < DUV_MAX;
c... if it fails after MAX_ITER iterations, calc. restart in the second mode;
c... itim = iteration count;
c
c... jingrong 04/30/99 added one cycle: cycle = 2, with normalized ddu and ddv.
c
      cycle   = 0
      imoved = 0
      uv_tol  = UV_ERR
      duv_tol = DUV_MIN
      COR_FLAG = 1.d-1
c
c... read in external point
c
      call conv4_8(srf(8,isrf),pext, 3)

5     itim = 0
      utan = ONE
      vtan = ONE
      if (imid .eq. 0) then
        u = sfu
        v = sfv
      else
        u = halfu
        v = halfv
      endif
c
c... jingrong 4/30/99 do not allow o/b u,v
c
      if (u.lt.umin) u=umin
      if (u.gt.umax) u=umax
      if (v.lt.vmin) v=vmin
      if (v.gt.vmax) v=vmax

c
c... aak 17-jul-1998:
c... this 'if' block is for bended PS. It finds starting
c... uv-param closer to the 'right' side of the surface
c
      if (.not.lv84 .and. psmult .and. isrf.eq.1) then

         call get_surface_param(isrf,u,v,psrf,xn,ddu,ddv,
     *                          fail,ifl(2),cycle)
         dmin = f_dist (pext,psrf)

         nu = 4
         nv = 4

         du = (umax-umin)/(nu-1)
         dv = (vmax-vmin)/(nv-1)

         do i = 2,nu-1
            u1 = umin + (i-1)*du
            u1 = dmin1(umax,dmax1(umin,u1))
            do j = 2,nv-1
               v1 = vmin + (j-1)*dv
               v1 = dmin1(vmax,dmax1(vmin,v1))

               call get_surface_param(isrf,u1,v1,
     *                 psrf,xn,ddu,ddv,fail,ifl(2),cycle)

		         dis = f_dist (pext,psrf)

               if (dis.lt.dmin) then
                  dmin = dis
                  u = u1
                  v = v1
               endif
            enddo
         enddo

c
c...Secondary IF block handles cases where the surface
c...is long and skinny (tire molds for example).
c...Bobby - 3/31/15
c
         if (.not.lv100) then
            nu = 4
            nv = 4

            du = (umax-umin)/nu
            dv = (vmax-vmin)/nv
            v1 = v

            do i = 1,nu
               u1 = umin + (i-1)*du
               call get_surface_param(isrf,u1,v1,
     *                 psrf,xn,ddu,ddv,fail,ifl(2),cycle)

		         dis = f_dist (pext,psrf)

               if (dis.lt.dmin) then
                  dmin = dis
                  u = u1
               endif
            enddo

            u1 = u
            do i = 1,nv
               v1 = vmin + (i-1)*dv
               call get_surface_param(isrf,u1,v1,
     *                 psrf,xn,ddu,ddv,fail,ifl(2),cycle)

		         dis = f_dist (pext,psrf)

               if (dis.lt.dmin) then
                  dmin = dis
                  v = v1
               endif
            enddo

         endif
      endif
c
c... initial steps in u,v
c
      if (isrf.eq.2 .and. initds.ge.1) then
     	  du = D_U0
     	  dv = D_V0
      else
        du = D_UV0
        dv = D_UV0
      endif
c
c... init. corner parameters: corner = true --> corner situation occurs 
c... during iteration;
c... delta = diff. in normal vectors of current and previous iter.;
c... n_try is a count: if corner, iterations continue until n_try = MAX_TRY 
c
      corner = .false.
      n_try  = 0
      ii = 0
      delta  = ZERO
c
c... u1,v1 = u,v from the previous iteration
c
      u1 = u
      v1 = v
c
c... main loop of iterations
c... psrf = r(u,v); ddu = dr/du(u,v); ddv = dr/dv(u,v); xn = unit([ddu,ddv])
c
10    continue

      call get_surface_param(isrf,u,v,psrf,xn,ddu,ddv,fail,ifl(2),cycle)

      if (ifl(2) .eq. 466) then
          iret = 3
          go to 99
      endif

      if (fail) go to 91
c
c... pu = r(u,v) - u*dr/du; pv = r(u,v) - v*dr/dv
c
      call uvcplvc(psrf,ddu,pu, -u)
      call uvcplvc(psrf,ddv,pv, -v)
c
c... check for sudden change of normal vector: indicates a corner
c..... Eduard 2/8/2001. start only after 5th iteration since the first steps
c..... could have large du,dv. Also unset corner flag if the normals stay 
c..... close for 10 iterations - FSR 60354
c
      if (do_corner) call vctovc(xn,vec2)
      if (do_corner .and. itim.gt.5) then
         delta = dabs( ONE - f_dot(vec1,vec2) )
         if (corner) then
           if (delta.lt.COR_FLAG) ii = ii+1
           if (ii.ge.10) then
             corner = .false.
             n_try = 0
             u1 = u
             v1 = v
           endif
         else
           corner = delta.gt.COR_FLAG
           if (corner) ii = 0
         endif
      endif

      if (corner) then
         n_try = n_try + 1
c
c... points 1,2 are supposed to be on different sides of corner;
c... during each iter. while n_try < MAX_TRY, select a pair 
c... of such points maximally separated in u,v-space
c
         d12 = dsqrt( (u1-u2)**2 + (v1-v2)**2 )
         d10 = dsqrt( (u1-u)**2  + (v1-v)**2  )
         d20 = dsqrt( (u2-u)**2  + (v2-v)**2  )
         if (d10.gt.d12) then
            u2 = u
            v2 = v
         else if (d20.gt.d12) then
            u1 = u
            v1 = v
         endif
      else if (do_corner) then
         u2 = u1
         v2 = v1
         u1 = u
         v1 = v
      endif

      if (corner.and.n_try.gt.MAX_TRY) then
c
c... iterations did not converge because of the corner;
c... push points 1,2 on different corner sides from each other
c... by distance STEP_BACK each (along line 1-2 in uv-space)
c... to avoid micro structure very close to corner
c
         call push_uv(u1,v1,u2,v2,STEP_BACK)
c
c... get surface param. at new points
c
         call get_surface_param(isrf,u1,v1,p1,n1,ddu1,ddv1,
     *                          fail,ifl(2),cycle)
            if (fail) go to 91
         call get_surface_param(isrf,u2,v2,p2,n2,ddu2,ddv2,
     *                          fail,ifl(2),cycle)
            if (fail) go to 91
c
c... get projection of pext on corner, 'normal' vector and ddu,ddv
c
         call near_corner(isrf,pext,u1,v1,u2,v2,
     *          p1,n1,ddu1,ddv1,p2,n2,ddu2,ddv2, 
     *                 psrf,u,v,xn,ddu,ddv,fail)
         if(fail) go to 91

      else
c
c... continue regular algorithm; if saw corner, do MAX_TRY more iterations
c... before starting to evaluate corner
c
         call f_cross(ddv,xn,xu)
         call f_cross(xn,ddu,xv)

         uden = f_dot(xu,ddu)
         vden = f_dot(xv,ddv)
         if (uden.eq.ZERO.or.vden.eq.ZERO) goto 91

         call vcmnvc(pext,pu,vec)
         uerr = f_dot(xu,vec)/uden - u
         call vcmnvc(pext,pv,vec)
         verr = f_dot(xv,vec)/vden - v
c
c..... Non-standard convergence: try going the opposite way.
c..... Used for projecting with a near point.
c
         if (ifl(385) .eq. 1 .or. ifl(385) .eq. 3) uerr = -uerr
         if (ifl(385) .eq. 2 .or. ifl(385) .eq. 3) verr = -verr

         if (itim.eq.0) then
            if (uerr.lt.ZERO) du = - du
            if (verr.lt.ZERO) dv = - dv
         else
            uden = ouerr - uerr
            vden = overr - verr
c
c... neg tan is unreal
c
           if (dabs(uden).ge.DUV_MIN) utan = dabs(du/uden)
           if (dabs(vden).ge.DUV_MIN) vtan = dabs(dv/vden)
c
c... aak 16-sep-98: added this to prevent utan=0 all the time
c... when du was zero during previous steps
c
           if (utan.le.ZERO .and. ((u.ge.umax-UV_ERR .and. uerr.lt.ZERO)
     *         .or. (u.le.umin+UV_ERR .and. uerr.gt.ZERO) ) ) utan = ONE
           if (vtan.le.ZERO .and. ((v.ge.vmax-UV_ERR .and. verr.lt.ZERO)
     *         .or. (v.le.vmin+UV_ERR .and. verr.gt.ZERO) ) ) vtan = ONE
c
c... mode = "0": restrict utan,vtan
c... mode = "1,2": restrict |du|,|dv|
c
           if (cycle.eq.0) then
              if (utan.gt.one) utan = one
              if (vtan.gt.one) vtan = one
              du = uerr*utan
              dv = verr*vtan
           else
              du = uerr*utan
              dv = verr*vtan
              if (dabs(du).gt.DUV_MAX) du = DUV_MAX*du/dabs(du)
              if (dabs(dv).gt.DUV_MAX) dv = DUV_MAX*dv/dabs(dv)
           endif

        endif

        done = (itim.gt.1).and.(dabs(uerr).lt.uv_tol)
     *                    .and.(dabs(verr).lt.uv_tol) 
c
c... jingrong 03/31/99
c
        if (done) done = dabs(du).le.duv_tol.and.dabs(dv).le.duv_tol

        if (done.and.corner) then
c
c... special case when iterations converged to a point very close to corner;
c... important for flat tool and zero-diameter tool in TA/NORMAL,PS mode:
c... avoid micro structure if pext is very close to corner.
c... also important for a smooth (no corner) but very curved surface:
c... in this case try to avoid corner logic as much as possible
c
            call push_uv(u1,v1,u2,v2,STEP_BACK)
c
c... 1 & 2 are 2 points on different corner sides
c
            call get_surface_param(isrf,u1,v1,p1,n1,ddu1,ddv1,
     *                             fail,ifl(2),cycle)
                 if (fail) go to 91
            call get_surface_param(isrf,u2,v2,p2,n2,ddu2,ddv2,
     *                             fail,ifl(2),cycle)
                 if (fail) go to 91
            call near_corner(isrf,pext,u1,v1,u2,v2,
     *               p1,n1,ddu1,ddv1,p2,n2,ddu2,ddv2, 
     *                      pc,uc,vc,vec,vec1,vec2,fail)
            if (fail) then 
c
c..... Dassault's NCL315.cpp: the corner is defined by far away
c..... projections, and cannot be resolved, but the algorithm 
c..... converged OK - we pretend there is no corner
c
              if (.not.lv94 .and. isrf.eq.2) then
                d12 = dsqrt( (u1-u2)**2 + (v1-v2)**2 )
                fail = (d12 .lt. DUV_MAX)
                if (.not.fail) goto 15
              endif
              goto 91
            endif

            if (f_dist (psrf,pc).lt.1.5*toler) then

               if (f_dist (pext,psrf).ge.dtol) then
                  call vctovc (vec,xn) 
               else if (isrf.eq.1) then
                  sign = f_dot (taxis,xn)
                  call vctovc (taxis,xn)
                  if (sign.lt.ZERO) call mnvc (xn)
               endif

            endif
             
        endif

        if (.not.done) then
           itim = itim+1
c 
c... if cycle = 0 iteration mode failed, restart in the second mode
c
           if (itim.gt.MAX_ITER) then
c
c..... if failing, project extermal point ont the latest surface plane,
c..... and compare with the latest surface point; if they are close, use
c..... these latest values and return. eduard 11/9/2000
c
              call vcmnvc(pext,psrf,vec)
              call f_cross(xn,vec,vec1)
              del = f_mag(vec1)
              if (del .lt. toler) goto 15
              if (uext .or. vext) then
                del = f_dot (xn,vec)
                call uvcplvc(pext,xn,pext1, -del)
                if (vext .and. dabs(dv).lt.dabs(du)) then
                  call unitvc(ddv,der)
                else
                  call unitvc(ddu,der)
                endif
                del = f_dot (der,vec)
                call uvcplvc(psrf,der,psrf1, del)
                del = f_dist (pext1,psrf1)
                if (del .lt. toler) goto 15
              endif
              
              
              if (cycle.ge.3) goto 91
              cycle = cycle + 1
c
c... Added cycle 3 to deal with "not_so_sharped" cornered sf, but "sharp" enough
c... to make usrfpn fail to converge. jingrong 11/30/99.
c
              if (cycle.eq.3) COR_FLAG = 1.d-3
              goto 5
           endif

           if(.not.corner.and.do_corner) then
              u1 = u
              v1 = v  
              call vctovc(vec2,vec1)
           endif

           ouerr = uerr
           overr = verr

           hu = u + uerr
           hv = v + verr

           if (u+du.lt.umin) then
             uext = .true.
             du = umin - u
           else if (u+du.gt.umax) then
             uext = .true.
             du = umax - u
           else
             uext = .false.
           endif

           if (v+dv.lt.vmin) then
             vext = .true.
             dv = vmin - v
           else if (v+dv.gt.vmax) then
             vext = .true.
             dv = vmax - v
           else
             vext = .false.
           endif
c
c..... QAR 92440: if du is small because it got very small several steps before
c..... and never recovered, but uerr is large, and we are about to exit since
c..... both du and dv are too small now - boost du once. (same for dv and verr).
c
        if (.not.lv91 .and. imoved.eq.0 .and. cycle.eq.0 .and.
     *       dabs(du).le.duv_tol .and. dabs(dv).le.duv_tol) then
          if (dabs(uerr) .gt. 10.*uv_tol) then
            if (du .ge. 0)  then
              if ((u+2.*duv_tol).lt.umax) du = 2.*duv_tol
            else
              if ((u-2.*duv_tol).gt.umin) du = -2.*duv_tol
            endif
            imoved = 1
          endif
          if (dabs(verr) .gt. 10.*uv_tol) then
            if (dv .ge. 0)  then
              if ((v+2.*duv_tol).lt.vmax) dv = 2.*duv_tol
            else
              if ((v-2.*duv_tol).gt.vmin) dv = -2.*duv_tol
            endif
            imoved = 1
          endif
        endif

           u = u + du
           v = v + dv
c 
c... go to the next iteration of the main loop
c 
           if (dabs(du).gt.duv_tol.or.dabs(dv).gt.duv_tol) goto 10
c 
c... stall-out.  (no real chg in u,v)
c
           stol = STALL_OUT
           if (cvion) stol = 2*DUV_MIN
           if (dabs(hu-halfu).ge.dabs(hv-halfv)) then
              if (hu.lt.umin - stol) iret = -1
              if (hu.gt.umax + stol) iret =  1
           else
              if (hv.lt.vmin - stol) iret = -2
              if (hv.gt.vmax + stol) iret =  2
           endif
c
c..... Check if doing a non-standard convergence results in a wrong extension.
c
           if (ifl(385) .gt. 0) then

             call vcmnvc(pext,psrf,vec)
             uden = f_dot(vec,ddu)
             vden = f_dot(vec,ddv)

             lumin = (u .lt. umin + duv_tol)
             lvmin = (v .lt. vmin + duv_tol)
             lumax = (u .gt. umax - duv_tol)
             lvmax = (v .gt. vmax - duv_tol)

             if ((lumin .and. uden.gt.0) .or.
     x           (lumax .and. uden.lt.0) .or.
     x           (lvmin .and. vden.gt.0) .or.
     x           (lvmax .and. vden.lt.0)) then
               ifl(385) = -ifl(385)
               if (iret .eq. 0) then
                 if (lumin .and. uden.gt.uv_tol) then
                   iret = -1
                 else if (lumax .and. uden.lt.-uv_tol) then
                   iret = 1
                 else if (lvmin .and. vden.gt.uv_tol) then
                   iret = -2
                 else if (lvmax .and. vden.lt.-uv_tol) then
                   iret = 2
                 endif
               endif 
             endif

           endif
c
c... end of if(.not.done)
c
        endif
c
c... end of if(corner.and.n_try > max_try)  ... else ....
c
      endif
c
c... When done: CALC TANPL
c
15    sec = ad2*f_mag(xn)
      if (dabs(sec).lt.small) go to 91
c
c... LOAD VEC IN SRF(1-3)
c
         call vctmsc(xn, vec1, one/sec)
         call conv8_4(vec1,srf(1,isrf),3)
c
c... CSRF PT
c
         call conv8_4(psrf, srf(5,isrf),3)
c 
c... TANPL CONST
c 
         srf(4,isrf) = f_dot(vec1,psrf)
c        
c... CONVERT BACK TO SFU,SFV
c        
         SFU = u
         SFV = v
         go to 99
c
c... solution failed.  re-try from midpt if feasible.  15-NOV-84
c
91    if (ifl(2) .eq. 466) then
          iret = 3
          go to 99
      endif
      if (.not.(imid.eq.1.or.itim.eq.0)) then
         imid = 1
         cycle = 0
         if (.not.lv94) then
           imoved = 0
           goto 5
         endif
         itim = 0
         u = halfu
         v = halfv
         delta = ZERO
         go to 10
      endif
c
c... could not re-try.  error
c
      IFL(2)=128
      iret = 3
c
c... if DS and TA 'PARELM', add v-vec to TCOL(22-24)   23-JUL-86
c
99    if (isrf.ne.2.or.ifl(23).ne.6) return
      if (rldinu) then
c
c... load dr/du
c
        call conv8_4(ddu,t(22,ia),3)
      else
c
c... load dr/dv
c
        call conv8_4(ddv,t(22,ia),3)
      endif

      RETURN
      END

c ********************* 72 *********************************************
c  SUBROUTINE: uv_interpol(p0,u0,v0,ddu,ddv,n,p,u,v,fail)
c
c  FUNCTION:   given a point, finds coordinates of its projection on tangent 
c              plane to surface with respect to the frame (dr/du, dr/dv)
c
c  INPUT   :   p0  R*8   D3  - surface point
c              u0  R*8   D1  - u-parameter of p0
c              v0  R*8   D1  - v-parameter of p0
c              ddu R*8   D3  - dr/du at p0
c              ddv R*8   D3  - dr/dv at p0
c              n   R*8   D3  - normal to surface at p0
c              p   R*8   D3  - given point  
c
c  OUTPUT  :   u   R*8   D1  - u-parameter of p
c              v   R*8   D1  - v-parameter of p
c              fail L        - false if O.K., true if dr/du,dr/dv are almost ||              
c
c ********************* 72 *********************************************
      subroutine uv_interpol(p0,u0,v0,ddu,ddv,n,p,u,v,fail)

      implicit real*8 (a-h,o-z)
      real*8 p0(3),ddu(3),ddv(3),p(3),vec(3),n(3),vp(3),
     *       small/1.d-6/, zero/0.d00/, one/1.d00/
      logical fail
c
c... vp = projection of p on plane (p0,n)
c
      call point_on_plane(p,p0,n,vp)
c
c... vp = p0 + (u-u0)*ddu + (v-v0)*ddv
c
      du_dv = f_dot(ddu,ddv)
      du2   = f_dot(ddu,ddu)
      dv2   = f_dot(ddv,ddv)
      det   = du2*dv2 - du_dv*du_dv
c
c... if ddu, ddv are ||, fail   
c
      fail = dabs(det).le.small
      if(fail) return

      call vcmnvc(vp,p0,vec)
      vecu = f_dot(vec,ddu)
      vecv = f_dot(vec,ddv)

      du = (dv2*vecu - du_dv*vecv)/det
      dv = (du2*vecv - du_dv*vecu)/det

      u = u0 + du
      v = v0 + dv
      u = dmin1(one,dmax1(zero,u))
      v = dmin1(one,dmax1(zero,v))

      return   
      end
c ********************* 72 *********************************************
c  SUBROUTINE: resolve_corner(p1,n1,p2,n2,line,p,case)
c
c  FUNCTION:   given 2 half-planes (PL1, PL2) intersecting along
c              a line and an arbitrary point, finds on which half-plane
c              the point can be projected
c
c  INPUT   :   p1  R*8   D3  -  a point on PL1
c              n1  R*8   D3  -  normal vector to PL1
c              p2  R*8   D3  -  a point on PL2
c              n2  R*8   D3  -  normal vector to PL2
c              line R*8  D6  -  line of intersection of PL1&PL2
c              p   R*8   D3  -  arbitrary point 
c
c  OUTPUT  :  case I*2 :  
c             PROJ_ON_PL1  /1/ iff p is projected on PL1 only
c             PROJ_ON_PL2  /2/ iff p is projected on PL2 only
c             PROJ_ON_BOTH /3/ iff p is projected on both PL1&PL2 
c                                   (i.e. p is inside the corner)
c             PROJ_NONE    /0/ iff p cannot be projected on either of them
c
c ********************* 72 *********************************************
      subroutine resolve_corner(p1,n1,p2,n2,line,p,case)

      implicit real*8 (a-h,o-z)
      real*8 p1(3),n1(3),p2(3),n2(3),line(6),p(3),a(3),b(3),
     *       zero/0.d00/, one/1.d00/
      integer*2 case,
     *          PROJ_ON_PL1/1/,PROJ_ON_PL2/2/,PROJ_ON_BOTH/3/,
     *          PROJ_NONE/0/
c
c... construct vector a(): on PL1, perp. to line, goes from line towards p1
c
      call f_cross(line(4),n1,b)
      call vcmnvc(p1,line(1),a)
      if(f_dot(a,b).lt.zero) call vctmsc(b,a,-one)
c
c... if proj1 < 0, projection of p on PL1 is on other side from p1 vis. line  
c
      call vcmnvc(p,line(1),b)
      proj1 = f_dot(a,b)
c
c... same for plane2
c
      call f_cross(line(4),n2,b)
      call vcmnvc(p2,line(1),a)
      if(f_dot(a,b).lt.zero) call vctmsc(b,a,-one)
      call vcmnvc(p,line(1),b)
      proj2 = f_dot(a,b)

      if(proj1.ge.zero.and.proj2.le.zero) then
         case = PROJ_ON_PL1 
      else if(proj1.le.zero.and.proj2.ge.zero) then
         case = PROJ_ON_PL2 
      else if(proj1.gt.zero.and.proj2.gt.zero) then
         case = PROJ_ON_BOTH 
      else
         case = PROJ_NONE
      endif

      return   
      end
c ********************* 72 *********************************************
c  SUBROUTINE: near_corner(isrf,pext,u1,v1,u2,v2,p1,n1,ddu1,ddv1,
c                       p2,n2,ddu2,ddv2,psrf,u,v,xn,ddu,ddv,fail)
c
c  FUNCTION:   finds projection of a point on a surface with sharp corner
c              approximated by 2 half-planes intersecting along a line
c
c  INPUT   :   
c              isrf  I*2  D1   - number of the surface (ps,ds,cs)
c              pext  R*8  D3   - point to be projected 
c              u1    R*8   D1  - u-parameter of 1st surface point p1  
c              v1    R*8   D1  - v-parameter of 1st surface point p1
c              u2    R*8   D1  - u-parameter of 2nd surface point p2 
c              v2    R*8   D1  - v-parameter of 2nd surface point p2 
c                                (corner is between p1 & p2)
c              p1    R*8   D3  - 1st surface point
c              n1    R*8   D3  - normal to the surface at p1
c              ddu1  R*8   D3  - dr/du at p1
c              ddv1  R*8   D3  - dr/dv at p1
c              p2    R*8   D3  - 2nd surface point
c              n2    R*8   D3  - normal to the surface at p2
c              ddu2  R*8   D3  - dr/du at p2
c              ddv2  R*8   D3  - dr/dv at p2
c
c  OUTPUT  : 
c              psrf R*8  D3  - point of projection of pext on surface
c              u    R*8  D1  - u-parameter of psrf 
c              v    R*8  D1  - v-parameter of psrf 
c              xn   R*8  D3  - normal to surface at psrf 
c              ddu  R*8  D3  - dr/du at psrf 
c              ddv  R*8  D3  - dr/dv at psrf 
c              fail L        - true iff n1 || n2 , false otherwise
c
c ********************* 72 *********************************************
      subroutine near_corner(isrf,pext,u1,v1,u2,v2,p1,n1,ddu1,ddv1,
     *                    p2,n2,ddu2,ddv2,psrf,u,v,xn,ddu,ddv,fail)

      implicit real*8 (a-h,o-z)
      real*8 pext(3),psrf(3),xn(3),ddu(3),ddv(3),vec(3),n1(3),n2(3),
     *       ddu1(3),ddu2(3),ddv1(3),ddv2(3),ln(6),p1(3),p2(3),
     *       half/0.5d00/,small/1.d-06/
      integer*2 case,isrf,
     *          PROJ_ON_PL1/1/,PROJ_ON_PL2/2/,PROJ_ON_BOTH/3/,
     *          PROJ_NONE/0/
      logical fail
c
c... get line of intersection of 2 planes  (p1,n1)&(p2,n2)
c... if planes are ||, fail
c
      call plplcross(p1,n1,p2,n2,ln,fail)
      if(fail) return
c
c... find on which side of the corner pext is projected
c... and take the projection for the resulting point;
c
      call resolve_corner(p1,n1,p2,n2,ln,pext,case)
      
      if(case.eq.PROJ_ON_PL1) then
         call point_on_plane(pext,p1,n1,psrf)
         call vctovc(ddu1,ddu)
         call vctovc(ddv1,ddv)
         call vctovc(n1,xn)
      else if(case.eq.PROJ_ON_PL2) then
         call point_on_plane(pext,p2,n2,psrf)
         call vctovc(ddu2,ddu)
         call vctovc(ddv2,ddv)
         call vctovc(n2,xn)
c
c...         case = PROJ_NONE
c... if pext is not projected on either side, project pext on corner line,
c... average ddu,ddv and take for normal vector connecting the projection 
c... with pext
c
      else
         call point_on_line(pext,ln,psrf)
         call vcplvc(ddu1,ddu2,vec)
         call vctmsc(vec,ddu,half)
         call vcplvc(ddv1,ddv2,vec)
         call vctmsc(vec,ddv,half)
         call vcmnvc(pext,psrf,xn)
      endif
c
c... now find u,v parameters of psrf via linear interpol.
c
      call uv_interpol(p1,u1,v1,ddu1,ddv1,n1,psrf,uu1,vv1,fail)
         if(fail) return
      call uv_interpol(p2,u2,v2,ddu2,ddv2,n2,psrf,uu2,vv2,fail)
         if(fail) return
      u = half*(uu1+uu2)
      v = half*(vv1+vv2)

      return   
      end
c ********************* 72 *********************************************
c  SUBROUTINE: push_uv(u1,v1,u2,v2,step)
c
c  FUNCTION:  moves points (u1,v1) & (u2,v2) from each other 
c             along the line connecting them 
c
c  INPUT   :  
c              u1   R*8   D1  -  u-parameter of a surface point
c              v1   R*8   D1  -  v-parameter of a surface point
c              u2   R*8   D1  -  u-parameter of a surface point
c              v2   R*8   D1  -  v-parameter of a surface point
c              step R*8   D1  -  distance to move the points
c  OUTPUT  :   
c              modified values of u1,v1,u2,v2
c
c ********************* 72 *********************************************
      subroutine push_uv(u1,v1,u2,v2,step)
      implicit real*8 (a-h,o-z)
      real*8 small/1.d-06/, zero/0.d00/,one/1.d00/
      
      du = u2 - u1
      dv = v2 - v1
      d = dsqrt(du*du + dv*dv)
      if(d.lt.small) then
c
c... if points are almost the same, move them apart along vector (1,1)
c
         u0 = 0.5*(u1+u2)
         v0 = 0.5*(v1+v2)
         u1 = u0 - step
         u2 = u0 + step
         v1 = v0 - step
         v2 = v0 + step
      else
c
c... (du,dv) is unit vector of the line connecting 2 points
c
         du = du/d
         dv = dv/d
         u1 = u1 - du*step
         v1 = v1 - dv*step
         u2 = u2 + du*step
         v2 = v2 + dv*step
      endif
c
c... make sure new points are within [0,1]
c
      u1 = dmin1(one,dmax1(zero,u1))
      v1 = dmin1(one,dmax1(zero,v1))
      u2 = dmin1(one,dmax1(zero,u2))
      v2 = dmin1(one,dmax1(zero,v2))
     
      return   
      end
c ********************* 72 *********************************************
c  SUBROUTINE:  get_surface_param(isrf,u,v,p,n,ddu,ddv,fail,ierr,cycle)
c
c  FUNCTION:    provides an interface with routine uevsft(u,v,isrf,sv)
c
c  INPUT   :  
c              isrf  I*2   D1  - number of the surface (ps,ds,cs)
c              u     R*8   D1  - u-parameter of a surface point
c              v     R*8   D1  - v-parameter of a surface point
c              cycle           - flag, if cycle=2 the output derivatives are
c                                unitized
c  OUTPUT  :   
c              p     R*8   D3  - surface point r(u,v)
c              n     R*8   D3  - normal to surface at p, unitized
c              ddu   R*8   D3  - dr/du at p
c              ddv   R*8   D3  - dr/dv at p
c              fail  L     D1  - true if [ddu,ddv] = 0, false otherwise
c              ierr  I*2   D1  - 0 or 466 if evaluator failure
c
c ********************* 72 *********************************************
      subroutine get_surface_param(isrf,u,v,p,n,ddu,ddv,fail,ierr,cycle)
      implicit real*8 (a-h,o-z)
      integer*2 isrf,ierr,cycle
      real*8 sv(9),n(3),p(3),ddu(3),ddv(3),xn(3),
     *       small/1.d-06/
      logical fail
c
c... for u,v, get parameters of surface: p= r(u,v),
c... sv(4-6)= dr/du; sv(7-9)= dv/du; n = normal vector
c
      call uevsft(u,v,isrf,sv,ierr)
      if (ierr .eq. 466) return
      call vctovc(sv(4),ddu)
c
c... jingrong 4/30/99 if cycle=2, use normalized ddu & ddv.
c
      if (cycle.eq.2) call unitvc(sv(4),ddu)

      call vctovc(sv(7),ddv)

      if (cycle.eq.2) call unitvc(sv(7),ddv)

      call f_cross(ddu,ddv,xn)
      fail = f_mag(xn).lt.small
      if (fail) return
      call unitvc(xn,n)
      call vctovc(sv(1),p)

      return   
      end

c **********************************************************************
c  SUBROUTINE:  indsuv (sfrkey,u,v)
c
c  FUNCTION:    calculate a smaller initial step for a projection onto
c               a large DS
c
c  INPUT   :  
c            srfkey  I*4   - surface key
c              u     R*4   - u-parameter of a surface point
c              v     R*4   - v-parameter of a surface point
c  OUTPUT  :   
c              D_U0  R*8   initial u-increment
c              D_V0  R*8   initial u-increment
c              initds      set to 1 if D_U0, D_V0 initialized
c
c **********************************************************************
      subroutine indsuv (srfkey,u,v)

      include 'com.com'
      include 'const.com'

      integer*4 srfkey
      real*4 u,v
      real*8 sv(9),sv0(3),f_dist
      real*8 D_U0,D_V0
      common/ds_uv0/D_U0,D_V0
      real*8 u1,v1,uu0,vv0,del,umin,vmin,umax,vmax,du0,dv0
      real*8 D_UV0/1.d-02/,UV_ERR/1.d-4/
      integer*2 isrf,nuv,j,ierr,initds,it,ipr
      equivalence (ifl(67),initds)
      integer*2 isftyp


      D_U0 = D_UV0
      D_V0 = D_UV0
      it = 0
      ipr = 0

      if (sc(169) .ge. 9.149d0) then

        isrf = 2
        if (ifl(330+isrf).eq.1) then
          call gtmmuv(isrf,umin,vmin,umax,vmax)
        else
          umin = ZERO
          vmin = ZERO
          umax = ONE
          vmax = ONE
        endif

        call sftype (srfkey, isftyp)
        if (isftyp.eq.QUILTSF .or. isftyp.eq.MESHSF .or.
     x      isftyp.eq.NETSF) return

        nuv = 10
        del = D_UV0 / (nuv-1)
        if (u .gt. umin + UV_ERR + 0.5*D_UV0 .and. 
     *      u + UV_ERR + 0.5*D_UV0 .lt. umax) then
          uu0 = u - 0.5*D_UV0 
          v1 = v
          du0 = 0.
          j = 1
          if (isftyp .eq. 91) then
            call ncl_pt_on_sf (it,srfkey,ipr,uu0,v1,sv,ierr)
            if (ierr .ne. 0) ierr = 466
          else
            call uevsft(uu0,v1,isrf,sv,ierr)
          endif
          if (ierr .eq. 466) return
          do while (j.lt.nuv .and. ierr.ne.466)
            sv0(1) = sv(1)
            sv0(2) = sv(2)
            sv0(3) = sv(3)
            u1 = uu0+j*del
            if (isftyp .eq. 91) then
              call ncl_pt_on_sf (it,srfkey,ipr,u1,v1,sv,ierr)
              if (ierr .ne. 0) ierr = 466
            else
              call uevsft(u1,v1,isrf,sv,ierr)
            endif
            if (ierr.ne.466) du0 = du0 + f_dist(sv,sv0)
            j = j+1
          enddo
          if (ierr .eq. 466) return
          if (du0.gt.sc(54)) then
            D_U0 = 0.25*D_UV0
            initds = 1
          endif
        endif
        if (v .gt. vmin + UV_ERR + 0.5*D_UV0 .and. 
     *      v + UV_ERR + 0.5*D_UV0 .lt. vmax) then
          vv0 = v - 0.5*D_UV0 
          u1 = u
          dv0 = 0.
          j = 1
          if (isftyp .eq. 91) then
            call ncl_pt_on_sf (it,srfkey,ipr,u1,vv0,sv,ierr)
            if (ierr .ne. 0) ierr = 466
          else
            call uevsft(u1,vv0,isrf,sv,ierr)
          endif
          if (ierr .eq. 466) return
          do while (j.lt.nuv .and. ierr.ne.466)
            sv0(1) = sv(1)
            sv0(2) = sv(2)
            sv0(3) = sv(3)
            v1 = vv0+j*del
            if (isftyp .eq. 91) then
              call ncl_pt_on_sf (it,srfkey,ipr,u1,v1,sv,ierr)
              if (ierr .ne. 0) ierr = 466
            else
              call uevsft(u1,v1,isrf,sv,ierr)
            endif
            if (ierr.ne.466) dv0 = dv0 + f_dist(sv,sv0)
            j = j+1
          enddo
          if (ierr .eq. 466) return
          if (dv0.gt.sc(54)) then
            D_V0 = 0.25*D_UV0
            initds = 1
          endif
        endif
      endif

      return   
      end

c ********************* 72 *********************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine usrfpo (u,v,iret)
c*        this routine is called when a srf p/n is reqd on a non-NCL
c*        surface.  For NCL V8.4 and below.
C*    PARAMETERS   
C*       INPUT  : 
C*          sfu        - u value to start at
C*          sfv        - v value to start at
C*          iret       - return value
C*                Common array SRF(8-10,ISRF) contains external point
C*       OUTPUT :  
C*          sfu        - u value of calc'd point & normal
C*          sfv        - v value of calc'd point & normal
C*                Common array SRF(1:4,ISRF) - normal plane
C*                Common array SRF(5:7,ISRF) - point on surface
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      SUBROUTINE USRFPO (SFU, SFV, IRET)

      include 'com4a.com'
      include 'mocom.com'
 
C          MOTION COMMON EQUIVALENCES
      REAL*4 SRF(10,3)
      EQUIVALENCE (SRF,S)

      REAL*8 SV(9), U, DU, V, DV
      REAL*4 AD(300),X(4),Y(4),Z(4)
      INTEGER*2 KD(600)
      EQUIVALENCE (D,AD,KD),(IFL(54),ISRF)

C          INIT FOR CONVERGE STEPS.
      IDX=50*(ISRF-1)
      JDX=2*IDX
      KDX=4*IDX
      ISTYP=KD(KDX+1)
      IRET=0
      IMID=0
      ITIM=0
      UTAN=1.
      VTAN=1.
      U=SFU
      V=SFV
C          DO NOT ALLOW O/B U,V
      IF(U.LT.0.)U=0.
      IF(U.GT.1.)U=1.
      IF(V.GT.1.)V=1.
      IF(V.LT.0.)V=0.
      TX=SRF(8,ISRF)
      TY=SRF(9,ISRF)
      TZ=SRF(10,ISRF)
C          SOLVE THIS U,V POSITION AND UERR, VERR
C          4 INT PTS  ( 2 & 4 ARE ACTUALLY DELTAS )
10    CALL UEVSFT (U, V, ISRF, SV, ifl(2))
      X(1)=SV(1)-U*SV(4)
      Y(1)=SV(2)-U*SV(5)
      Z(1)=SV(3)-U*SV(6)
      X(3)=SV(1)-V*SV(7)
      Y(3)=SV(2)-V*SV(8)
      Z(3)=SV(3)-V*SV(9)
      X(2)=SV(4)
      Y(2)=SV(5)
      Z(2)=SV(6)
      X(4)=SV(7)
      Y(4)=SV(8)
      Z(4)=SV(9)
C
      XN=Y(2)*Z(4)-Z(2)*Y(4)
      YN=Z(2)*X(4)-X(2)*Z(4)
      ZN=X(2)*Y(4)-Y(2)*X(4)
C
      XU=Y(4)*ZN-Z(4)*YN
      YU=Z(4)*XN-X(4)*ZN
      ZU=X(4)*YN-Y(4)*XN
C
      XV=Z(2)*YN-Y(2)*ZN
      YV=X(2)*ZN-Z(2)*XN
      ZV=Y(2)*XN-X(2)*YN
CCC--          GUARD AGAINST ZERO DIVIDE    9-3-82
      UDEN= XU*X(2)+YU*Y(2)+ZU*Z(2)
      VDEN= XV*X(4)+YV*Y(4)+ZV*Z(4)
      IF(UDEN.EQ.0..OR.VDEN.EQ.0.)GOTO 91
      UERR=(XU*(TX-X(1))+YU*(TY-Y(1))+ZU*(TZ-Z(1))) /UDEN-U
      VERR=(XV*(TX-X(3))+YV*(TY-Y(3))+ZV*(TZ-Z(3))) /VDEN-V
C
      IF(ITIM.GT.0)GOTO 40
      DU=.01
      DV=.01
      IF (UERR.LT.0.)DU=-.01
      IF (VERR.LT.0.)DV=-.01
      GOTO 50
40    UDEN=OUERR-UERR
      IF(ABS(UDEN).LT.1.E-5)GOTO 42
      UTAN=DU/UDEN
C          NEG TAN IS UNREAL
      IF(UTAN.GT.0.) GOTO 42
      UTAN = -UTAN
42    IF(UTAN.GT.1.)UTAN=1.
      DU=UERR*UTAN
      VDEN=OVERR-VERR
      IF(ABS(VDEN).LT.1.E-5)GOTO 44
      VTAN=DV/VDEN
C          SAME FOR VTAN
      IF(VTAN.GT.0.) GOTO 44
      VTAN = -VTAN
44    IF(VTAN.GT.1.)VTAN=1.
      DV=VERR*VTAN
50    HU=U+UERR
      HV=V+VERR
      IF(U+DU.LT.0.)DU=-U
      IF(U+DU.GT.1.)DU=1.-U
      IF(V+DV.LT.0.)DV=-V
      IF(V+DV.GT.1.)DV=1.-V
      U=U+DU
      V=V+DV
      IF(ITIM.GT.1.AND.ABS(UERR).LT..0001.AND.ABS(VERR).LT..0001)GOTO 90
      ITIM=ITIM+1
      OUERR=UERR
      OVERR=VERR
      IF(ITIM.GT.120)GOTO 91
C          IF UV CHG REAL, GO AGAIN.
      IF(ABS(DU).GT.1.E-5.OR.ABS(DV).GT.1.E-5)GOTO 10
C
C          STALL-OUT.  (NO REAL CHG IN U,V)
C
      IF (ABS(HU-.5).LT.ABS(HV-.5)) GOTO 65
      IF (HU.LT.-.002) IRET = -1
      IF (HU.GT.1.002) IRET = 1
      GOTO 90
65    IF (HV.LT.-.002) IRET = -2
      IF (HV.GT.1.002) IRET = 2
90    CONTINUE
C          CALC TANPL
      A=XN
      B=YN
      C=ZN
      SEC=SQRT(A**2+B**2+C**2)*AD(JDX+2)
      IF(ABS(SEC).GT.1.E-6)GOTO 92

C          SOLUTION FAILED.  RE-TRY FROM MIDPT IF FEASIBLE.  15-NOV-84
91    IF(IMID.EQ.1.OR.ITIM.EQ.0) GOTO 915
      IMID=1
      ITIM=0
      U=.5
      V=.5
      GOTO 10
C             COULD NOT RE-TRY.  ERROR
915   CONTINUE
      IFL(2)=128
      iret = 3
      GOTO 99
C          NORMAL EXIT.  LOAD VEC IN SRF(1-3)
92    SRF(1,ISRF)=A/SEC
      SRF(2,ISRF)=B/SEC
      SRF(3,ISRF)=C/SEC
C          SRF PT
      SRF(5,ISRF)=X(1)+X(2)*U
      SRF(6,ISRF)=Y(1)+Y(2)*U
      SRF(7,ISRF)=Z(1)+Z(2)*U
C          TANPL CONST
      SRF(4,ISRF)=SRF(1,ISRF)*SRF(5,ISRF)+SRF(2,ISRF)*SRF(6,ISRF)+
     1 SRF(3,ISRF)*SRF(7,ISRF)
C          CONVERT BACK TO SFU,SFV
94    SFU=U
      SFV=V
C           IF DS AND TA 'PARELM', ADD V-VEC TO TCOL(22-24)      23-JUL-86
99    IF(ISRF.NE.2.OR.IFL(23).NE.6)GOTO 999
      IA=IFL(51)
      if (rldinu) then
        t(22,ia)=x(2)
        t(23,ia)=y(2)
        t(24,ia)=z(2)
      else
        t(22,ia)=x(4)
        t(23,ia)=y(4)
        t(24,ia)=z(4)
      endif
999   RETURN
      END
