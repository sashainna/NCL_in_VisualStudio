C*********************************************************************
C*    NAME         :  cidist.f
C*       CONTAINS:  cidist  circlo
C*    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
C*
C*    MODULE NAME AND RELEASE LEVEL 
C*        cidist.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:40
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cidist (gstp,gend,gcir) 
c*    FUNCTION :  Finds a point at specified distance from/to check 
c*                surface and outputs feed rate slowdown if applicable
c*                and goto points. 
C*    PARAMETERS   
C*       INPUT  : 
C*          gstp(9) r*8 - Circle arc start point & FWD vector. 
C*          gend(9) r*8 - Circle arc end point & FWD vector. 
C*          gcir(7) r*8 - Circle canonical data. 
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cidist (gstp,gend,gcir)
c
      include 'com8a.com'
      include 'comgt.com'
c
      real*8 gstp(9),gend(9),gcir(7),mconv(4,3),aconv(2)
c
      logical lf1f2,lv92
      integer*2 modac,modsl,isub
      integer*4 nu,nrp,i,nrf

      real*8 PI,RADIAN,FEEDR,zvec(3)
      real*8 rr,atot,ccen(3),cfwd(3),cstr(3),cend(3),dis,s1,s2,alps
      real*8 ang2dp,xax(3),cid,vs(3),ve(3),c1,al1,al2
c
      equivalence (FEEDR,MOTMAP(24))
      equivalence (RADIAN,MOTMAP(25)), (PI,MOTMAP(26))
c
      data xax /1.0,.0,.0/
c
      PI     = dacos (-1.0d0)
      RADIAN = 180.d0 / PI
      lv92 = sc(169) .lt. 9.249d0
c
c...Initialize
c
      modsl  = 0
      modac  = 0
      if (ifl(314) .ne. 0) modsl = 1
      if (ifl(315) .ne. 0) modac = 1
      isub   = 5
c
c...Here check all ifl's from cirrec routine to disable modsl & modac
c

c
c...Convert circle parameters to Z-plane
c
      nu     = 3 * (ifl(82) + 1)
      call gtpola (gcir(4),aconv,mconv) 
      call ptmatr (gcir(4),zvec,mconv,2)
      call ptmatr (gstp,cstr,mconv,2)
      call ptmatr (gend,cend,mconv,2)
      call ptmatr (gcir,ccen,mconv,2)
      call ptmatr (gstp(7),cfwd,mconv,2)
c
c...Get arc parameters on Z-plane
c
      vs(1) = cstr(1) - ccen(1)
      ve(1) = cend(1) - ccen(1)
      vs(2) = cstr(2) - ccen(2)
      ve(2) = cend(2) - ccen(2)
c
c......Circular is always calculated in XY-plane
c......Therefore, do not use Z's in calculations
c......Bobby  -  10/5/94
c
      vs(3) = 0.
      ve(3) = 0.

      rr     = dsqrt (vs(1)**2 + vs(2)**2)
      alps   = ang2dp (xax,vs,zvec)
      atot   = ang2dp (vs,ve,zvec)
c
c...Check CLW or CCLW motion
c
      cid    = 1.0
      c1     = vs(2) * cfwd(1) - vs(1) * cfwd(2)
      if (c1 .gt. 0.) then
          cid = 0. - 1.0
          atot = 2.0*PI - atot
      end if
c
c...If move too small, it must be full circle
c
      if (rr*atot .le. .0005) atot = 2.*PI
      dis    = rr * atot 
c     
c...Check point distance 
c
      if (dis .lt. .001 .or. modsl+modac .eq. 0) go to 1000
      lf1f2  = FEEDC(2) .lt. FEEDC(1) 
      nrf    = 0
      nrp    = 0
      nor    = 3
      s2     = 0.0
      s1     = 1.0
      is     = 0
c
c...For slowdown OUT find point forward
c
      if (modac .ne. 0) then
         is   = 2
         if (FEDIS(2) .lt. dis-.001) then
            nrf = 1 
            s2  = FEDIS(2) / dis               
            if (FEDIS(2) .lt. .001) then
               nrf = 0
            else
               al2 = s2 * atot
            end if
         else
            nrf = 2
         end if
      end if
c
c...For slwdown AT find point from CS
c
      if (modsl .ne. 0) then
         if (FEDIS(1) .le. dis) then
            nrp = 1 
            s1  = 1.0 - FEDIS(1) / dis               
            if (dis-FEDIS(1) .lt. .001) then
               nrp = -1
               is  = 1
            else if (FEDIS(1) .lt. .001) then
               nrp = 0
            else
               al1 = s1 * atot
            end if       
         else
            nrp = -1
            is  = 1
         end if       
      end if
c
c...Set all flags according to conditions
c
      if (modsl+modac .gt. 0) then
         if (nrp .lt. nrf .and. modac .eq. 1) then
            if (is .eq. 1 .and. lf1f2) is = 2
            if (nrp .gt. 0 .and. lf1f2) nrp = 0
            if (nrp .ne. 0) nor = 1
         else if (nrp .eq. nrf) then
            if (s1 .lt. s2+.002) then
               nor = 1
               if (lf1f2) then
                  nrp = 0
               else if (FEEDC(2) .gt. FEEDC(1)) then
                  nrf = 0
               else
                  nrp = 0
                  nrf = 0
               end if
            end if
         end if
      end if
c
c...Output accelerate feed or slow down
c...if slow has to start immediately  
c
      if (is .ne. 0) then
         call fedmut (FEEDC(is))
         if (is .eq. 1) isub = 4
         if (is .eq. 2) isub = 2
      end if
c
c...FR/OUT...
c...Output intermediate points & feed rate 
c
      if (nrf .eq. 1 .and. FEEDR .ne. FEEDC(nor)) then
         call circlo (rr,cstr,alps,al2,cid,ccen,gcir,mconv,isub)
         alps = alps + cid*al2 
         al1  = al1 - al2 
         atot = atot - al2
         call fedmut (FEEDC(nor))
         if (nor .eq. 3) isub = 5
         if (nor .eq. 1) isub = 4
      end if           
c
c...FR/AT...
c
      if (nrp .eq. 1 .and. FEEDR .ne. FEEDC(1)) then
         call circlo (rr,cstr,alps,al1,cid,ccen,gcir,mconv,isub)
         isub = 4
         alps = alps + cid*al1 
         atot = atot - al1
         call fedmut (FEEDC(1))
      end if
c
c...Output last points & restore feed
c
 1000 call circlo (rr,cstr,alps,atot,cid,ccen,gcir,mconv,isub)
      call fedmut (FEEDC(3))
c
c...If ONCE turn off appropriate flag 
c...unless it is FROM statement
c
      if (ifl(314) .eq. 2) call resfed (1)
      if (ifl(315) .eq. 2) call resfed (2)
      do 1005 i=1,nu
         FROMSV(i) = gend(i)
 1005 continue
c
c...End of routine
c
 8000 return 
      end
c
C*********************************************************************
C*    E_SUBROUTINE: subroutine circlo (gra,gpt,gast,gdel,gid,gcen,gmat)
c*    FUNCTION :  Outputs circle record and following GOTO record with 
c*                points generated according to required tolerance.
C*    PARAMETERS   
C*       INPUT  : gra    r8 - Arc radius 
C*                gpt(3) r8 - Start point in Z-plan 
C*                gast   r8 - Start angle of arc (rad) 
C*                gdel   r8 - Angular length of arc (rad) 
C*                gid    r8 - Direction of arc: 1. = CCLW, -1. = CLW.
C*                gcen(3) r8 - Arc center coordinates.
C*                gcir(7) r8 - Circle canonical data.
C*                gmat(12) r8 - Comversion matrix.
C*                ksub   I*2 - GOTO record subclass (for 5200 type)
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine circlo (gra,gpt,gast,gdel,gid,gcen,gcir,gmat,ksub) 
c
      include 'com8a.com'
      include 'comgt.com'
c
      integer*2 ksub
      real*8 gra,gpt(3),gast,gdel,gid,gcen(3),gcir(7),gmat(4,3) 
c
      real*8 PI,RADIAN,tol,a1,s1,cdt(15),csdt(5),t(420),pt(3),vc(3)
      real*8 d1,md(9)
c
      integer*4 i, n, max
      integer*2 npts,isub,mxc, iclass
      logical mult,lv92
c
      equivalence (csdt,cdt)
      equivalence (RADIAN,MOTMAP(25)), (PI,MOTMAP(26))
c
      integer*2 iwf
c
      data csdt /5.0, .0, .0, .0, .0/
c
      lv92 = sc(169) .lt. 9.249d0
      cdt(13) = 0.
      cdt(12) = 0.
      cdt(15) = 0.
c
c...   Initiialize for expanded cl
c
cc      if (lexpcl) then
      if (.not. lv92) then
        iclass = 5200
        mult = .true.
        max  = 20
        isub = ksub
      else
        iclass = 5000
        isub = 5
        mult = ifl(82).eq.1
        if (mult) then
          max = 20
        else
          max = 40
        endif
      endif
      if (lmintf(1)) isub = 6
c
c...Output circular record first
c
      do 105 i=6,12
         cdt(i) = gcir(i-5)
  105 continue
c
c...Reverse circul plan vector if CLW
c
      if (gid .lt. 0.) then
         cdt(9) = gid * gcir(4)
         cdt(10) = gid * gcir(5)
         cdt(11) = gid * gcir(6)
      end if
c
c.....Adjust circle for TLAXIS/,MODIFY
c
      if (ifl(104) .eq. 1) call modfy(cdt(6),cdt(6))
      if (.not. lmintf(1)) call putcl (3000,2,13,cdt)
c
c...Get number of points in arc
c
      tol   = sc(27)
      d1    = gdel
      if (gra .gt. tol) d1 = dacos((gra-tol)/gra)*2
      n     = gdel / d1 + 1
      a1    = gast
      s1    = gdel / n
c
c...Get DS type - wireframe or not
c
      call isdswf (iwf)
c
c...Generate points on arc
c
      mxc   = 0
      npts  = 0
      pt(3) = gpt(3)
      do 205 i=1,n 
          a1 = gast + i*gid*s1
          pt(1) = gcen(1) + gra * dcos(a1)
          pt(2) = gcen(2) + gra * dsin(a1)
          call ptmatb (pt,t(mxc+1),gmat,2)
          ms  = mxc + 1
          mxc = mxc + 3
c
c...If multax add TV
c
          t(mxc+1) = sc(4)
          t(mxc+2) = sc(5)
          t(mxc+3) = sc(6)
          if (mult) mxc = mxc + 3
c
c.....Adjust circle for TLAXIS/,MODIFY
c
          if (ifl(104) .eq. 1) then
              md(1) = t(ms)
              md(2) = t(ms+1)
              md(3) = t(ms+2)
              md(4) = t(ms+3)
              md(5) = t(ms+4)
              md(6) = t(ms+5)
              md(7) = 0.
              md(8) = 0.
              md(9) = 0.
              call modfy(t(ms),md)
          endif
c
c...Expanded cl, calc FWD vector etc...
c...to avoid confusion set mxc back to 1 (point index)
c
cc          if (lexpcl) then
            if (.not. lv92) then
             mxc = mxc - 6
             a2 = a1 + gid * .5d0 * PI
             vc(1) = dcos(a2)
             vc(2) = dsin(a2)
             vc(3) = 0.0
             call ptmatb (vc,t(mxc+7),gmat,2)
c
c......PSIS contact point is same as position point,
c......and PSIS normal vector is same as TA vector
c
             call vctovc (t(mxc+1),t(mxc+10))
             call vctovc (t(mxc+4),t(mxc+13))
c
c.....Drive sf point is offset from tool center point
c
             call f_cross(t(mxc+7),t(mxc+4),vc)
             vc(1) = ifl(21)*vc(1)
             vc(2) = ifl(21)*vc(2)
             vc(3) = ifl(21)*vc(3)
             call uvcplvc (t(mxc+1),vc,t(mxc+16),-sc(28)*.5d0) 
c
c..... qar 97096: put DS points at the corner radius height if wireframe
c
             if (iwf .eq. 1) then
               call uvcplvc (t(mxc+16),t(mxc+4),t(mxc+16),sc(29))
             endif

             call vctovc (vc,t(mxc+19))
             mxc = mxc + 21
          endif
          npts = npts+1
c
c...Output motion record if buffer is full
c
          if (npts .eq. max) then 
              call putcl (iclass,isub,npts,t)
              mxc   = 0
              isub  = 6
              npts  = 0
          end if
  205 continue
      if (npts .ne. 0) call putcl (iclass,isub,npts,t)
c
c...End of routine
c
      return
      end
