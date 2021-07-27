C*********************************************************************
C*    NAME         :  getpod.f
C*       CONTAINS: gtpodh  sfnorm  piersf  pirnsf  hompod  circsf
C*                 sssave 
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        getpod.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:08
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : gtpodh (kpn,pn,ph,ksf,sf,podid,gpar)
C*      Calculates pod hights, stores points, PVs of initial patern,
C*      PVs of swivel and number of generated points in the work vars. 
C*    PARAMETERS   
C*       INPUT  : 
C*          kpn     - # of paterns to process
C*          pn      - array of patern asws 
C*          ph      - array of patern higths (if PN of points). 
C*          ksf     - # of surfaces to process
C*          sf      - array of surface asws 
C*          podid   - array (4) of reserved names for work vars 
C*                       1 - # of pods (stored points) 
C*                       2 - array of pod' points
C*                       3 - array of initial pointvectors
C*                       4 - array of PVs for swivel axes. 
C*         gpar     - parameters (3): 1 - pod diam, 2 - pod higth,
C*                                    3 - max tilt angle.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine gtpodh (kpn,pn,ph,ksf,sf,podid,pinx,gpar)
c
      include 'com8a.com'
c
      real*8 pn(*),ph(*),sf(*),gpar(3)
      character*64 podid(4)
      integer*4 pinx(4)
      integer*4 kpn,ksf
c
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold
c
      integer*2 npt,i,j,k,mtv(4),mtyp,jtrim
      integer*4 ntyp,iflg,ifl1,n,mf(9),ls,lt,nclky,sfky(2)
      real*8 pv(6),pts(6),pte(3),pss(3),rs(6),
     -       ve(3),d,u,v,ss(6),uv(2),dof,ra,dlow,dhi,r1,asfw,ndot,
     -       tmp(6),tol,a
c
      logical trflg
c
      equivalence (uv(1),u), (uv(2),v), (TV,mtv)
      equivalence (mtv(1),nclky), (sfky,asfw)
c
      tol    = sc(27)*2.0d0
      r1     = 180.0 / dacos(0.-1.0d0)
      dof    = -9999. 
      trflg  = .true.

      ra     = gpar(1) * .55
      n      = 8
c
c...Get all patern points
c
      jtrim  = 0
      k      = 0
  100 k      = k + 1
      call gtpnnp (pn(k),npt,ntyp)
      do 955 i=1,npt
          call gpnptt (pv,pn(k),i,trflg)
          if (ntyp .eq. 1) then
              pv(4) = 0.0
              pv(5) = 0.0
              pv(6) = 1.0
          end if
          do 405 j=1,6
              pts(j) = pv(j)
  405     continue
          d    = dsqrt(pv(4)**2 + pv(5)**2 + pv(6)**2)
          if (ntyp .eq. 1) then
              pte(1) = pts(1)
              pte(2) = pts(2)
              pte(3) = pts(3) + ph(k)
          else
              pte(1) = pts(1) + pv(4)
              pte(2) = pts(2) + pv(5)
              pte(3) = pts(3) + pv(6)
          end if
          ve(1) = pv(4) / d
          ve(2) = pv(5) / d
          ve(3) = pv(6) / d
          dlow  = ndot (ve,pts)
          dhi   = ndot (ve,pte)
c
c...Check all SFs if pod is under one of them 
c
          ls    = 0
          do 850 j=1,ksf
             if (sf(j) .eq. 0.0) go to 500
             asfw = sf(j)
c
c...If trimmed surface, initialize boundry
c
             call sftype (sfky(1),mtyp)
             if (mtyp .eq. 99 .and. jtrim .ne. j) then
                 if (jtrim .ne. 0) call resmem
                 call tbncrv (sfky(1))
                 jtrim = j
             end if
c
c...Get IO point of pod axis and surface SF
c
             u   = .5
             v   = .5
             iflg = 0
             call piersf (sf(j),pts,uv,pss,tol,iflg)
c
c...Get projection point at specified 'ghig' from surface
c
             pts(1) = pss(1)
             pts(2) = pss(2)
             pts(3) = pss(3)
             ifl1   = 0
             call pirnsf (sf(j),pts,gpar(2),uv,ss,ifl1)
             if (mtyp .eq. 99 .and. ifl1 .eq. 0)
     -                 call intrim (uv(1),uv(2),ifl1)
c
c...Check if pod' higth is in available range
c
             if (ifl1 .eq. 0) then
                 d      = ndot (ve,ss) 
                 if (d .lt. dlow) go to 1100
                 if (d .gt. dhi) ifl1 = 1
                 ss(4) = ss(4) - ss(1)
                 ss(5) = ss(5) - ss(2)
                 ss(6) = ss(6) - ss(3)
c
c...Check if pod' swivel angle in the range
c
                 a = ndot(ss(4),ve) / dsqrt(ndot(ss(4),ss(4)))
                 a = dacos(a) * r1
                 if (a .gt. gpar(3)) ifl1 = 1
             end if
c
c...If center of pod is on SF, check circumference of pod
c
             if (ifl1 .eq. 0) then
                 call circsf (ksf,sf,jtrim,ss,ra,uv,n,mf)
                 call mfchk (n,mf,lt)
                 if (lt .gt. ls) then
                     ls = lt 
                     call sssave (ss,rs)
                     if (lt .eq. n) go to 500
                 end if     
             end if              
  850    continue
  500    if (ls .lt. n) call hompod (pv,gpar(2),rs)
c
c...Store pod tilt axis as PV entity
c...Store initial pod axis as PV entity
c...Store calculated pod position as PT entity
c
         len = strlen1(podid(4))
         if (len .gt. 0) then
             pinx(4)    =  pinx(4) + 1
             do 915 j=1,6
               tmp(j) = rs(j)
  915        continue
             call storit (tmp,podid(4),pinx(4), 21,1)
             if (ifl(2) .ne. 0) go to 9000
         end if
         pinx(2)   = pinx(2) + 1
         rs(1) = rs(1) + ve(1) * gpar(2)
         rs(2) = rs(2) + ve(2) * gpar(2)
         rs(3) = rs(3) + ve(3) * gpar(2)
         call storit (rs,podid(2),pinx(2),3,1)
         if (ifl(2) .ne. 0) go to 9000
         pinx(3)   = pinx(2)
         call storit (pv,podid(3), pinx(3),21,1)
         if (ifl(2) .ne. 0) go to 9000
  955 continue
      if (k .lt. kpn) go to 100
c
c...Store number of points
c
      token2   = podid(1)
      ivxsub = pinx(1)
      call vstchk
      REST   = pinx(2)
      IDST   = 2
      SAVID2 = TOKEN2
      isvsub = ivxsub
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
      keyold = keyhld
      istold = ist
      call vstore 
      kpn    = pinx(2)
      go to 9000
c
c...Error, pod is too high for surface
c
 1100 ifl(310) = pinx(2) 
      call delpod (podid,1)
      call error (488)
c
 9000 if (jtrim .ne. 0) call resmem
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : sfnorm (sf,pts,uu,vv,ss,kflg) 
C*      Projects the point on SF perpendicular to SF.  Uses sfpt routine
C*      but returns point on SF extension if input point is outside the
C*      SF edge. 
C*    PARAMETERS   
C*       INPUT  : 
C*          sf     - R*8 SF id
C*          pts(3) - input point to project
C*          uu,vv  - R*8 u & v initial values (if kflg = 0)
C*          kflg   - I*4 flag: 0 - use initial u,v; 1 - start from the
C*                   center of surface (.5,.5)
C*       OUTPUT :  
C*          uu,vv  - R*8 u & v values of output point
C*          ss(10) - R*8 output parameters of tangent plane & projected
C*                   point (5-7), closest point on boundry (8-10) if
C*                   point is outside SF (kflg = 1). 
C*          kflg   - I*4 flag: 0 - output point is on SF, 1 - output
C*                   point is outside SF.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine sfnorm (sf,pts,uu,vv,ss,kflg)
c
      include 'com8a.com'
c
      real*8 sf,pts(3),ss(10),uu,vv
      integer*4 kflg
c 
      real*8 pd(3),d,dist,d1
      real*4 sl(7),dir,u,v
      integer*4 i
c
      dir    = 0.
      if (kflg .eq. 0) then
          u = uu
          v = vv
      else
          u = .5
          v = .5
      end if
      kflg   = 0
      call sfpt (sf,pts,3,dir,u,v,sl)      
      ifl(2) = 0
c
c...Check if projected point is on surface
c                 
      if (u*v .eq. 0.0 .or. u .gt. .9995 .or. v .gt. .9995) then
          d     = sl(1) * pts(1) +  sl(2) * pts(2) + sl(3) * pts(3) 
          d1    = sl(4) - d
          pd(1) = pts(1) + d1 * sl(1) 
          pd(2) = pts(2) + d1 * sl(2) 
          pd(3) = pts(3) + d1 * sl(3) 
          dist  = dsqrt((pd(1)-sl(5))**2 + 
     -                 (pd(2)-sl(6))**2 + (pd(3)-sl(7))**2)
          if (dist .gt. .002) kflg = 1 
      end if
      uu     = u
      vv     = v
      do 105 i=1,7
          ss(i)  = sl(i)
  105 continue
c
c...Replace point on SF' edge by point on extension plane
c
      if (kflg .eq. 1) then
          ss(8)  = sl(5)
          ss(9)  = sl(6)
          ss(10) = sl(7)
          ss(5)  = pd(1)
          ss(6)  = pd(2)
          ss(7)  = pd(3)
      end if
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : piersf (sf,pts,uv,pss,gtol,kflg) 
C*      Projects the point on SF at the direction specified by vector
C*      from the point using PV definition.
C*    PARAMETERS   
C*       INPUT  : 
C*          sf     - R*8 SF id
C*          pts(6) - input pointvector
C*          uv(2)  - R*8 u & v initial values (if kflg = 0)
C*          kflg   - I*4 flag: 0 - use initial u,v; 1 - start from the
C*                   center of surface (.5,.5)
C*          gtol   - R*8 tolerance defined as distance from previous
C*                   step or from the surface edge.
C*       OUTPUT :  
C*          uv(2)  - R*8 u & v values of the calculated point on SF
C*          pss(3) - R*8 coordinates of the output point 
C*          kflg   - I*4 flag: 0 - pointvector intersects with SF,
C*                   1 - intersection point is outside SF. 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c*******************************************************************
c
      subroutine piersf (sf,pts,uv,pss,gtol,kflg)
c
      include 'com8a.com'
c
      real*8 sf,pts(6),pss(3),uv(2),gtol
      integer*4 kflg
c 
      real*8 pd(3),pvs(10),uu,vv,dd,ds,vec(3),d1,d2,d,pt(3)
      real*8 ndist
      integer*4 i,iflg,ir1,if1 
c
      kflg   = 0
c
c...Unitize direction vector of PV
c
      d      = dsqrt (pts(4)**2 + pts(5)**2 + pts(6)**2)
      do 55 i=1,3
          vec(i) = pts(i+3) / d
          pt(i) = pts(i)
   55 continue
c
c...Initialize misces
c
      ds     = .0d0
      uu     = .5
      vv     = .5
      ir1    = 0
      iflg   = 0
c
c...Get SF point at normal to SF from given point
c
  100 if1   = 0 
      ir1   = ir1 + 1
      call sfnorm (sf,pt,uu,vv,pvs,if1)
      d1   = vec(1)*pvs(5) + vec(2)*pvs(6) + vec(3)*pvs(7)
      d2   = vec(1)*pt(1) + vec(2)*pt(2) + vec(3)*pt(3) 
      d1   = d1 - d2
      pd(1) = pt(1) + d1 * vec(1)
      pd(2) = pt(2) + d1 * vec(2)
      pd(3) = pt(3) + d1 * vec(3)
      dd   = dsqrt ((pd(1)-pvs(5))**2 + (pd(2)-pvs(6))**2 +
     -              (pd(3)-pvs(7))**2)
      pt(1) = pd(1) 
      pt(2) = pd(2) 
      pt(3) = pd(3)
      ds    = dabs (d1)
      if (dd .gt. 0.) then
          if (dd/(ds+1.d-9) .gt. 100. .or. ir1 .ge. 100) iflg = 1
          if (dd .gt. gtol .and. iflg .eq. 0) go to 100
      end if
c
c...Check if projected point is on surface
c                 
      if (if1 .eq. 1) then
          d1 = ndist (pt,pvs(8))
          if (d1 .gt. 2.*gtol) iflg = 1
      end if 
      kflg  = iflg
      pss(1) = pt(1)
      pss(2) = pt(2)
      pss(3) = pt(3)
      uv(1)  = uu
      uv(2)  = vv
c
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : pirnsf (sf,pts,ghig,uv,ss,kflg) 
C*      Calculates the point on the input line (PV) which is at the
C*      specified distance from SF at normal direction.
C*    PARAMETERS   
C*       INPUT  : 
C*          sf     - R*8 SF id
C*          pts(6) - input pointvector
C*          uv(2)  - R*8 u & v initial values (if kflg = 0)
C*          kflg   - I*4 flag: 0 - use initial u,v; 1 - start from the
C*                   center of surface (.5,.5)
C*          ghig   - R*8 distance from SF where the desired point has 
C*                   to be located on the input line.
C*       OUTPUT :  
C*          uv(2)  - R*8 u & v values of the calculated point on SF
C*          ss(6)  - R*8 coordinates of the output point (1-3) and
C*                   its projection point on SF (4-6) 
C*          kflg   - I*4 flag: 0 - point exist, 1 - solution does not
C*                   exist. 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c*******************************************************************
c
      subroutine pirnsf (sf,pts,ghig,uv,ss,kflg)
c
      include 'com8a.com'
c
      real*8 sf,pts(6),ss(6),uv(2),ghig
      integer*4 kflg
c 
      real*8 pvs(10),uu,vv,h,ds,vec(3),d,pt(3)
      real*8 ndist
      integer*4 i,ir1,iflg 
c
      if (kflg .eq. 0) then
          uu  = uv(1) 
          vv  = uv(2)
      else
          uu  = .5
          vv  = .5
      end if
c
c...Unitize direction vector of PV
c
      d      = dsqrt (pts(4)**2 + pts(5)**2 + pts(6)**2)
      do 55 i=1,3
          vec(i) = pts(i+3) / d
          pt(i)  = pts(i) - ghig * vec(i)
   55 continue
c
c...Initialize misces
c
      h      = ghig
      d      = h
      ir1    = 0
c
c...Get SF point at normal to SF from given point
c
  100 iflg  = 0 
      ir1   = ir1 + 1
      call sfnorm (sf,pt,uu,vv,pvs,iflg)
c
c...Check if projected point is on surface
c
      if (iflg .ne. 0) then
          d = ndist (pvs(5),pvs(8))
          if (d .gt. sc(27)*50.0d0) go to 200 
          iflg = 0 
      end if
      d    = dsqrt ((pvs(5)-pt(1))**2 + (pvs(6)-pt(2))**2 + 
     -              (pvs(7)-pt(3))**2) 
      h    = ghig * h / d
      ds   = dabs (d-ghig)
c    
c...Check tolerance and convergence
c
      if (ds .gt. sc(27)) then
          pt(1) = pts(1) - h * vec(1)
          pt(2) = pts(2) - h * vec(2)
          pt(3) = pts(3) - h * vec(3)
          if (ir1 .lt. 100 .and. ds/(h+1.d-9) .lt. 100.) then
              go to 100
          else
              iflg = 1
          end if
      end if
c
c...Setup output points
c                 
  200 kflg  = iflg
      ss(1) = pt(1)
      ss(2) = pt(2)
      ss(3) = pt(3)
      ss(4) = pvs(5)
      ss(5) = pvs(6)
      ss(6) = pvs(7)
      uv(1)  = uu
      uv(2)  = vv
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine hompod (gpts,ghig,gout)
C*      sets the pod at home position 
C*    PARAMETERS   
C*       INPUT  : 
C*          gpts(6) - pointvector representing the pod
C*          ghig    - swivel height of pod
C*       OUTPUT :  
C*          gout(6) - pointvector representing the home position of pod 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine hompod (gpts,ghig,gout)
c
      real*8 gpts(6),gout(6),ghig
c
      real*8 dd
      integer*4 i
c
c...Set pod at home position (at ghig from low point)
c
      dd = dsqrt(gpts(4)**2 + gpts(5)**2 + gpts(6)**2)
      do 105 i=1,3
          gout(i) = gpts(i) 
          gout(i+3) = ghig * gpts(i+3) / dd
  105 continue
      return 
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : circsf (ksf,sfid,ktrim,gss,gra,guv,knum,kfl)
C*      Projects circle simulated by 'knum' points to the set of SFs  
C*      checking if all the points of circle have its equivalent on
C*      surface(s).
C*    PARAMETERS   
C*       INPUT  : 
C*          ksf    - # of SFs to check 
C*          sfid   - R8 array of asw of SFs 
C*          ktrim  - index of the last initialized trim SF if any. 
C*          gss    - pointvector specifying CI center & CI plane 
C*                   ie. directiom of projection.
C*          gra    - radius of the circle
C*          guv    - u,v values of the circle center 
C*          knum   - number of points to generate on the CI circumf.
C*       OUTPUT :  
C*          ktrim  - index of the last initialized trim SF if changed. 
C*          kfl(knum) - array of flags assigned to each point of CI,
C*                   0 - point is projectable on a SF, 1 - none of SFs  
C*                   can not satisfy this point & direction. 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c*******************************************************************
      subroutine circsf (ksf,sfid,ktrim,gss,gra,guv,knum,kfl)
c
      include 'com.com'
c
      real*8 sfid(*),gss(6),gra,guv(2)
      integer*4 ksf,knum,ktrim,kfl(*)
c
      real*8 pv(6),v(3),d,r,ab(2),ptsf(3),p(3),asfw,tol
      integer*4 i,j,n3,ifl1,nclky(2)
      integer*2 mtyp
c
      equivalence (nclky,asfw)
c
      tol = sc(27)*10.0d0
c
c...Unitize vector of circle plane
c
      d     = dsqrt (gss(4)**2 + gss(5)**2 + gss(6)**2)
      pv(4) = gss(4)/d
      pv(5) = gss(5)/d
      pv(6) = gss(6)/d
c
c...Get starting point on circle in XY (circle plan) 
c
      call getang (ab(1),ab(2),pv(4))
      v(1) = gra
      v(2) = 0.
      v(3) = 0.
      r    = 360.0 / knum
      n3   = 3
c
c...Adjust point back to original coordinates
c...and check if it points to surface 
c
      do 505 i=1,knum
          call ptadjr (v,p,ab)
          pv(1) = gss(1) + p(1)
          pv(2) = gss(2) + p(2)
          pv(3) = gss(3) + p(3) 
          do 105 j=1,ksf
              if (sfid(j) .ne. 0.0) then
c
c...Check if trimmed surface, initialize boundry
c
                  asfw = sfid(j)
                  call sftype (nclky(1),mtyp)
                  if (mtyp .eq. 99 .and. ktrim .ne. j) then
                     if (ktrim .ne. 0) call resmem
                     call tbncrv (nclky(1))
                     ktrim = j
                  end if
c
c...Project point on SF
c
                  ifl1 = 0
                  guv(1) = .5
                  guv(2) = .5
                  call piersf (sfid(j),pv,guv,ptsf,tol,ifl1)
                  if (mtyp .eq. 99 .and. ifl1 .eq. 0)
     -                     call intrim (guv(1),guv(2),ifl1)
                  if (ifl1 .eq. 0) go to 300
              end if
  105     continue
          ifl1 = 1
  300     call vecadj (v,v,r,n3)
          kfl(i) = 1 - ifl1
  505 continue
c
      return
      end 
c
C*********************************************************************
C*    E_SUBROUTINE     : sssave (ss,rs)
C*      Moves 6 r8 array to output array.
C*    PARAMETERS   
C*       INPUT  : 
C*          ss(6) - input buffer 
C*       OUTPUT :  
C*          rs(6) - output buffer 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine sssave (ss,rs)
c
      real*8 ss(6),rs(6)
c
      integer*4 i
c
      do 105 i=1,6
         rs(i) = ss(i)
  105 continue
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     :  mfchk (knum,kin,ktot)
C*      Counts number of ones' in the input array.
C*    PARAMETERS   
C*       INPUT  : 
C*          knum   - # of entries in the input array 
C*          kin    - input array with flags (0 or 1) 
C*       OUTPUT :  
C*          ktot   - number of flags (set to 1). 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine mfchk (knum,kin,ktot)
c
      integer*4 knum,kin(*),ktot 
c
      integer*4 i,m
c
      m   = 0
      do 105 i=1,knum
         if (kin(i) .eq. 1) m = m + 1 
  105 continue
      ktot = m
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     :  delpod (gids,kfla)
C*      Deletes temporary variables assigned for POD macro from run file
C*      and activated by TERMAC. ifl(310) holds the number of items to
C*      delete.
C*    PARAMETERS   
C*       INPUT  : 
C*          gids(4) - tokens of the variables to delete 
C*          kfla    - flag: 0 - use default names (tokens) assigned for
C*                    POD macro, 1 - use input tokens (gids), this is
C*                    used where error occured during calculations and 
C*                    variables already stored are deleted.  
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine delpod (gids,kfla) 
c
      include 'com.com'
c
      integer*2 kfla
      character*64 gids(4)
c
      integer*4 nclkey
      integer*2 i, nwds, ietype
      character*6 vams(4)
      character*64 cnms(4)
c
      data vams /'@K1','@K2','@K3','@K4'/
c
      if (kfla .eq. 0) then
          cnms(1) = vams(1)
          cnms(2) = vams(2)
          cnms(3) = vams(3)
          cnms(4) = vams(4)
      else
          cnms(1) = gids(1)
          cnms(2) = gids(2)
          cnms(3) = gids(3)
          cnms(4) = gids(4)
      end if
c
c...Delete all array of PT (and PV if used)
c
      do 100 i=1,ifl(310)
          ivxsub = i
c
c...Delete reserved point for pod output
c
          TOKEN2 = cnms(2)
          call vstchk
          if (ITYP .eq. 2 .and. IST .eq. 3) goto 90
c
c...Delete reserved initial pod' point-vector 
c
          TOKEN2 = cnms(3)
          call vstchk
          if (ITYP .eq. 2 .and. IST .eq. 21) goto 90
c
c...Delete reserved point-vector of the pod' tilt axis 
c
          TOKEN2 = cnms(4)
          call vstchk
          if (ITYP .ne. 2 .or. IST .ne. 21) goto 100
   90     call gtdesc(tv,nclkey,nwds,ietype)
          call dlgeom(nclkey)
  100 continue
c
c...Clear number of items in arrays
c
      ifl(310) = 0
      TOKEN2 = cnms(1)
      ivxsub = 0
      call vstchk
      if (ITYP .eq. 2 .and. IST .eq. 2) then
          REST = 0.0
          IDST = 2
          SAVID2 = TOKEN2
          isvsub = ivxsub
          ifl(9) = ifl(11)
          ifl(10) = ifl(12)
          keyold = keyhld
          call vstore
      end if
c
      return
      end
          
