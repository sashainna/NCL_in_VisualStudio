c
c***********************************************************************
c
c   FILE NAME:  math
c   CONTAINS:
c               axadj   betvec  getlan  getpnt  intvec  plndis  plnint
c               plnvpt  setijk  vecrot  veadja  veadjr  vecadj  vecang
c               split_bspl  casteljau   bezevl  curvpv  ptlnds  plnlnp
c               nptln   vctmsc  vcplvc  vcmnvc  crosvc  islnln  isciln
c               unitvc  intvec1 ndot    ndist   nmag    angl2p  isparl
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        math.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/12/15 , 17:41:59
c
c***********************************************************************
c
c***************************************************************
c
c   SUBROUTINE:  axadj (gixyz,goxyz,gaxis,gorg,kaxis)
c
c   FUNCTION:  This routine rotates an input point around
c              a specified axis at given angle.
c
c   INPUT:  gixyz   R*8  D3  -  The current point.
c           gaxis   R*8  D1  -  The angle to rotate.
c           gorg    R*8  D3  -  The rotary table origin and center.
c           kaxis   I*4  D1  -  Which axis to adjust for.
c
c   OUTPUT: goxyz   R*8  D3  -  The adjusted point.
c
c***************************************************************
c
      subroutine axadj (gixyz,goxyz,gaxis,gorg,kaxis)
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      integer*4 kaxis
c
      real*8 gixyz(3),goxyz(3),gaxis,gorg(3)
c
      integer*4 is1,is2,inx(6)
c
      real*8 pt(3),ti,tk,angl
c
      data inx /3,2,3,1,1,2/
c
c...Set up for which axis we are
c...rotating around
c
      is1    = inx(kaxis*2-1)
      is2    = inx(kaxis*2)
c
c...Set up the angle to rotate around
c
      angl   = gaxis
      if (kaxis .eq. 1) angl = 360.d0 - gaxis
c
      pt(is1) = gixyz(is1) - gorg(is1)
      pt(is2) = gixyz(is2) - gorg(is2)
c
c...Rotate the ijk position
c...around the rotary axis
c
      ti     = dsin (angl/RAD)
      tk     = dcos (angl/RAD)
      goxyz(is1) = pt(is1)*tk - pt(is2)*ti + gorg(is1)
      goxyz(is2) = pt(is2)*tk + pt(is1)*ti + gorg(is2)
      goxyz(kaxis) = gixyz(kaxis)
c
      return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  betvec (gvec1,gvec2,gang)
c
c  FUNCTION:  This routine defines true angle between two vectors.
c
c     INPUT:  gvec1   R*8  D3  -  Vector 1
c
c             gvec2   R*8  D3  -  Vector 2
c
c     OUTPUT: gang    R*8  D1  -  Angle between vectors (0-180)
c
c*********************************************************************
c
      subroutine betvec (gvec1,gvec2,gang)
c
      real*8 gvec1(3),gvec2(3),gang
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD,cosb,vec1(3),vec2(3),vn(3),ndist
c
      call unitvc (gvec1,vec1)
      call unitvc (gvec2,vec2)
      cosb   = vec1(1) * vec2(1) + vec1(2) * vec2(2) +
     -         vec1(3) * vec2(3)
c
c...Use direct angle for very small angles
c
      if (cosb .gt. .9999d0) then
          gang   = ndist(vec1,vec2) * RAD
      else if (cosb .lt. -.9999d0) then
          vn(1) = - vec2(1)
          vn(2) = - vec2(2)
          vn(3) = - vec2(3)
          gang   = 180.d0 - ndist(vec1,vn) * RAD
c
c...Use arccos in other cases
c
      else
          gang   = RAD * dacos (cosb)
      end if
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getlan (gtv1,gtv2,gan1,gan2,gdlt,gfi,gtheta,gtvmp)
c
c   FUNCTION:  This routine defines plane between two vectors and
c              vectors' angles on this plane (Tool Vector Motion Plane).
c
c   INPUT:  gtv1    R*8  D3  -  first vector
c
c           gtv2    R*8  D3  -  second vector
c
c   OUTPUT: gan1    R*8  D1  -  angle of the first vector on the TVMP
c
c           gan2    R*8  D1  -  angle of the second vector on the TVMP
c
c           gdlt    R*8  D1  -  delta angle between vectors
c
c           gfi     R*8  D1  -  angle from XY-plane to the TVMP
c
c           gtheta  R*8  D1  -  angle from X-axis to XY trace of plane
c
c           gtvmp   R*8  D3  -  vector defining TVMP
c
c*********************************************************************
c
      subroutine getlan (gtv1,gtv2,gan1,gan2,gdlt,gfi,gtheta,gtvmp)
c
      include 'post.inc'
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 PI,RAD
c
      integer*4 ifl
c
      real*8 gtv1(3),gtv2(3),gan1,gan2,gdlt,gfi,gtheta,gtvmp(3),
     -       p,vec,pln(3),alp,tvlin(3),bet,tl(3)
c
      ifl    = 0
c
c...get plane on vectors
c
      pln(1) = gtv1(2) * gtv2(3) - gtv1(3) * gtv2(2)
      pln(2) = gtv1(3) * gtv2(1) - gtv1(1) * gtv2(3)
      pln(3) = gtv1(1) * gtv2(2) - gtv1(2) * gtv2(1)
c
      vec    = dsqrt (pln(1)**2 + pln(2)**2 + pln(3)**2)
      p      = dabs (gtv1(1) * gtv2(1) + gtv1(2) * gtv2(2) +
     -               gtv1(3) * gtv2(3))
      if (p .gt. 1.d0-1.d-15 .or. vec .lt. 1.d-14) ifl = 1
c
c...check if vectors are parallel
c
      if (ifl .eq. 0) go to 400
c
c...parallel - use vertical plane
c
      pln(3) = 0.
      pln(1) = gtv1(2)
      pln(2) = - gtv1(1)
      vec    = dsqrt (pln(1)**2 + pln(2)**2)
      if (vec .lt. 1.d-14) then
          pln(1) = 0.
          pln(2) = 1.d0
          vec    = 1.d0
      end if
c
  400 pln(1) = pln(1) / vec
      pln(2) = pln(2) / vec
      pln(3) = pln(3) / vec
      p      = pln(1)**2 + pln(2)**2
      p      = dsqrt (p)
      gtvmp(1) = pln(1)
      gtvmp(2) = pln(2)
      gtvmp(3) = pln(3)
c
c...get intersection line on xy plane
c
      tvlin(3)  = 0.
      if (p .gt. 1.d-15) then
          tvlin(1)  = - pln(2) / p
          tvlin(2)  = pln(1) / p
      else
          tvlin(1)  = 1.
          tvlin(2)  = 0.
      end if
c
c...get line angle (from X-axis)
c...and plane angle (from XY-plane)
c
      call vecang (tvlin,3,gtheta)
      alp    = 360. - gtheta
      call vecadj (pln,tl,alp,3)
      call vecang (tl,1,gfi)
      bet    = 360. - gfi
c
c...get start angle (1-st vector)
c
      call vecadj (gtv1,tl,alp,3)
      call vecadj (tl,tl,bet,1)
      call vecang (tl,3,gan1)
c
c...get delta angle,
c...rotate shortest way
c
      call betvec (gtv1,gtv2,gdlt)
c
c...get end angle (2-nd vector)
c
      gan2   = gan1 + gdlt
      return
      end
c
c***************************************************************
c
c   SUBROUTINE:  getpnt (gpl1,gpl2,gd1,gd2,gvec1,gvec2,gtol,kerr)
c
c   FUNCTION:  This routine defines two vectors beeing obtained by
c              intersecting of two cones created by vectors
c              from 0,0,0 point and base planes gpl1 and gpl2.
c
c   INPUT:  gpl1    R*8  D3  -  vector of plane 1.
c
c           gpl2    R*8  D3  -  vector of plane 2.
c
c           gd1     R*8  D1  -  distance from 0,0,0 to the plane 1.
c
c           gd2     R*8  D1  -  distance from 0,0,0 to the plane 2.
c
c           gtol    R*8  D1  -  Tolerance for determining if a solution
c                               is available.
c
c   OUTPUT: gvec1   R*8  D3  -  intersection vector 1.
c
c           gvec2   R*8  D3  -  intersection vector 2.
c
c           kerr    I*4  D1  -  error status
c
c***************************************************************
c
      subroutine getpnt (gpl1,gpl2,gd1,gd2,gvec1,gvec2,gtol,kerr)
c
      integer*4 kerr
c
      real*8 gpl1(3),gpl2(3),gd1,gd2,gvec1(3),gvec2(3),gtol
c
      integer*4 i
c
      real*8 det1,det2,det3,det,col4(3),rnum,dist,gxyz(3),perp(3),
     -       prmt,ndist,vnul(3)
c
      data vnul /0.d0, 0.d0, 0.d0/
c
      kerr   = 0
c
c...get plane perpto to the intersection line
c
      perp(1) = gpl1(2) * gpl2(3) - gpl1(3) * gpl2(2)
      perp(2) = gpl1(3) * gpl2(1) - gpl1(1) * gpl2(3)
      perp(3) = gpl1(1) * gpl2(2) - gpl1(2) * gpl2(1)
      rnum   = dsqrt (perp(1)**2 + perp(2)**2 + perp(3)**2)
      if (rnum .eq. 0.) go to 900
      perp(1) = perp(1) / rnum
      perp(2) = perp(2) / rnum
      perp(3) = perp(3) / rnum
c
c...get main determinant
c
      det    = perp(1) * gpl1(2) * gpl2(3) + perp(2) * gpl1(3) * gpl2(1)
     -       + perp(3) * gpl1(1) * gpl2(2) - perp(3) * gpl1(2) * gpl2(1)
     -       - perp(1) * gpl1(3) * gpl2(2) - perp(2) * gpl1(1) * gpl2(3)
c
c...check if planes are parallel
c
      rnum   = dnint(det*10.0d0**12) / 10.0d0**12
      if (rnum .eq. 0.) go to 900
      col4(1) = 0.d0
      col4(2) = gd1
      col4(3) = gd2
c
c...get ijk determinants
c
      det1   = perp(2) * gpl1(3) * col4(3) + perp(3) * col4(2) * gpl2(2)
     -       - perp(3) * gpl1(2) * col4(3) - perp(2) * col4(2) * gpl2(3)
c
      det2   = perp(1) * col4(2) * gpl2(3) + perp(3) * gpl1(1) * col4(3)
     -       - perp(3) * col4(2) * gpl2(1) - perp(1) * gpl1(3) * col4(3)
c
      det3   = perp(1) * gpl1(2) * col4(3) + perp(2) * col4(2) * gpl2(1)
     -       - perp(1) * col4(2) * gpl2(2) - perp(2) * gpl1(1) * col4(3)
c
c...get intersection point
c
      gxyz(1) = det1 / det
      gxyz(2) = det2 / det
      gxyz(3) = det3 / det
c
c...see if solution exist
c
      dist   = gxyz(1)**2 + gxyz(2)**2 + gxyz(3)**2
cc      rnum   = dnint (dist*10.d0**8) / 10.d0**8
cc      if (rnum .gt. 1.d0) kerr = 2
      if (dist-1.d0 .gt. gtol) kerr = 2
      if (dist .gt. 1.d0) dist = 1.d0
      prmt   = dsqrt (1.d0 - dist)
c
c...get crossing vectors
c
      do 550 i=1,3
         gvec1(i) = gxyz(i) - prmt * perp(i)
         gvec2(i) = gxyz(i) + prmt * perp(i)
  550 continue
c
c...Fix any flow in vector length (vp 10-Jun-94)
c
      det1 = ndist (gvec1,vnul)
      det2 = ndist (gvec2,vnul)
      do 580 i=1,3
         gvec1(i) = gvec1(i) / det1
         gvec2(i) = gvec2(i) / det2
  580 continue
c
      return
c
c...planes are parallel
c
  900 kerr   = 1
      return
      end
c
c*********************************************************************
c
c  SUBROUTINE:  intvec (gvec,gang1,gang2,gfee,gtheta,kstat)
c
c  FUNCTION:  This routine checks if specified vector is lacated
c             between two vectors described by theirs angles from X-ax
c             in sector < 180.
c
c     INPUT:  gvec    R*8  D3  -  Vector on TVMP
c
c             gang1   R*8  D1  -  Position of the first vector
c
c             gang2   R*8  D1  -  Position of the second vector
c                                 (gang2 >= gang1 and can be > 360).
c
c             gfee    R*8  D1  -  Vertical tilt angle of TVMP
c
c             gtheta  R*8  D1  -  Horizontal rotation angle of TVMP
c
c     OUTPUT: kstat   I*4  D1  -  Ouput flag: 0 - vector is outside
c                                 the specfied sector, 1 - vector is
c                                 inside specfied sector.
c
c*********************************************************************
c
      subroutine intvec (gvec,gang1,gang2,gfee,gtheta,kstat)
c
      real*8 gvec(3),gang1,gang2,gfee,gtheta
      integer*4 kstat
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002)), (FUZZ8, POSMAP(4913))
c
      real*8 RAD,FUZZ8,alp,bet,ang,tl(3)
c
      alp    = 360. - gtheta
      bet    = 360. - gfee
c
c...Adjust vector to the XY plane
c...and get its angle from X-axis
c
      call vecadj (gvec,tl,alp,3)
      call vecadj (tl,tl,bet,1)
      call vecang (tl,3,ang)
c
c...See if base angle applies
c
      if (gang2 .gt. 360.d0 .and. ang .lt. gang1) ang = ang + 360.d0
c
c...Check if input vector is between vectors
c
      kstat  = 0
      if (ang .gt. gang1+FUZZ8 .and.
     -    ang .lt. gang2-FUZZ8) kstat = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  plndis (gpl,gpt,gdis)
c
c   FUNCTION:  This routine calculates the distance of a point to a
c              plane.
c
c   INPUT:  gpl     R*8  D4  -  Input plane.
c
c           gpt     R*8  D3  -  Input point.
c
c   OUTPUT: gdis    R*8  D1  -  Output distance.
c
c***********************************************************************
c
      subroutine plndis (gpl,gpt,gdis)
c
      real*8 gpl(4),gpt(3),gdis
c
c     integer*4 is
c
      real*8 rdis
c
c...Calculate distance of point to plane
c
      rdis   = gpl(1)*gpt(1) + gpl(2)*gpt(2) + gpl(3)*gpt(3)
      gdis   = rdis   - gpl(4)
c
c...Change sign to match direction of vector
c
c     is     = 3
c     if (dabs(gpl(1)) .gt. dabs(gpl(is))) is = 1
c     if (dabs(gpl(2)) .gt. dabs(gpl(is))) is = 2
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  plnint (gpt,gvec,gpl,gout,kerr)
c
c   FUNCTION:  This routine calculates the intersection point of a point
c              and vector with a plane.
c
c   INPUT:  gpt     R*8  D3  -  Starting point of vector.
c
c           gvec    R*8  D3  -  Vector to intersect with plane.
c
c           gpl     R*8  D4  -  Input plane.
c
c   OUTPUT: gout    R*8  D3  -  Intersection point of plane & vector.
c
c           kerr    I*4  D1  -  Returns 1 when the vector and plane do
c                               not intersect.
c
c***********************************************************************
c
      subroutine plnint (gpt,gvec,gpl,gout,kerr)
c
      integer*4 kerr
c
      real*8 gpt(3),gvec(3),gpl(4),gout(3)
c
      real*8 coso,len
c
c...Intersect vector with plane
c
      kerr   = 0
      coso   = (gvec(1)*gpl(1)) + (gvec(2)*gpl(2)) + (gvec(3)*gpl(3))
c
c......Vector does not intersect plane
c
      if (dabs(coso) .le. 1.d-6) go to 9000
c
c......Calculate new point
c
      len    = ((gpl(1)*gpt(1)) + (gpl(2)*gpt(2)) + (gpl(3)*gpt(3)) -
     1          gpl(4)) / coso
      gout(1) = gpt(1) - (len*gvec(1))
      gout(2) = gpt(2) - (len*gvec(2))
      gout(3) = gpt(3) - (len*gvec(3))
c
c...End of routine
c
 8000 return
c
c...Error calculating intersection point
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  plnvpt (gvec,gpt,gpl,kerr)
c
c   FUNCTION:  This routine calculates a plane with a given vector and
c              through a point.
c
c   INPUT:  gvec    R*8  D3  -  Vector of plane.
c
c           gpt     R*8  D3  -  Point that lies on plane.
c
c   OUTPUT: gpl     R*8  D4  -  Output plane.
c
c           kerr    I*4  D1  -  Returns 1 when the vector does not have
c                               a length.
c
c***********************************************************************
c
      subroutine plnvpt (gvec,gpt,gpl,kerr)
c
      integer*4 kerr
c
      real*8 gvec(3),gpt(3),gpl(4)
c
      real*8 rdis
c
c...Calculate plane distance from 0,0,0
c
      kerr   = 0
      rdis   = dsqrt(gvec(1)**2 + gvec(2)**2 + gvec(3)**2)
      if (rdis .eq. 0.) go to 9000
      gpl(1) = gvec(1) / rdis
      gpl(2) = gvec(2) / rdis
      gpl(3) = gvec(3) / rdis
      gpl(4) = gpl(1)*gpt(1) + gpl(2)*gpt(2) + gpl(3)*gpt(3)
c
c...End of routine
c
 8000 return
c
c...Zero length vector
c
 9000 kerr   = 1
      gpl(1) = 0.
      gpl(2) = 0.
      gpl(3) = 0.
      gpl(4) = 0.
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setijk (gangn,gtv,gfi,gtheta)
c
c   FUNCTION:  This routine calculates i,j,k of vector specified by
c              tangent plane and angle from XY trace of this plane.
c
c   INPUT:  gangn   R*8  D1  -  angle from XY trace of plane to vector.
c
c           gfi     R*8  D1  -  angle from XY-plane to vector.
c
c           gtheta  R*8  D1  -  angle from X-axis to XY trace of plane.
c
c   OUTPUT: gtv     R*8  D3  -  defined vector.
c
c***********************************************************************

      subroutine setijk (gangn,gtv,gfi,gtheta)
c
      real*8 gangn,gtv(3),gfi,gtheta,vec(3)
c
c...start from X-axis
c
      vec(1) = 1.
      vec(2) = 0.
      vec(3) = 0.
c
c...rotate X on XY-plane up to desired angle
c
      call vecadj (vec,vec,gangn,3)
c
c...rotate vector to match with plane
c
      call vecadj (vec,vec,gfi,1)
c
c...rotate plane at 'XY trace of plane' angle
c
      call vecadj (vec,gtv,gtheta,3)
c
      return
      end
c
c***************************************************************
c
c   SUBROUTINE:  vecrot (gtxyz,gtv,gaxis,gang)
c
c   FUNCTION:  This routine rotates an input vector around
c              the specified axis at given angle.
c
c   INPUT:  gtxyz   R*8  D3  -  The input vector.
c           gaxis   R*8  D3  -  The axis to rotate about
c           gang    R*8  D1  -  The angle to rotate
c
c   OUTPUT: gtv     R*8  D3  -  The adjusted vector.
c
c***************************************************************
c
      subroutine vecrot (gtxyz,gtv,gaxis,gang)
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      real*8 gtxyz(3),gtv(3),gaxis(4),gang
c
      real*8 u1,u2,u3,u12,u23,u13,ct,st,oc,tmtr(4,3)
c
c...some common vars (Faux & Pratt p. 73)
c
      ct = dcos(gang/RAD)
      st = dsin(gang/RAD)
      oc = 1.d0 - ct
      u1 = gaxis(1)
      u2 = gaxis(2)
      u3 = gaxis(3)
      u12 = u1*u2
      u13 = u1*u3
      u23 = u3*u2
c
c...setup matrix of conversion
c
      tmtr(1,1) = u1*u1 + ct*(1.d0 - u1*u1)
      tmtr(2,1) = u12*oc + u3*st
      tmtr(3,1) = u13*oc - u2*st

      tmtr(1,2) = u12*oc - u3*st
      tmtr(2,2) = u2*u2 + ct*(1.d0 - u2*u2)
      tmtr(3,2) = u23*oc + u1*st

      tmtr(1,3) = u13*oc + u2*st
      tmtr(2,3) = u23*oc - u1*st
      tmtr(3,3) = u3*u3 + ct*(1.d0 - u3*u3)
c
c...rotate the vector
c
      call ptmatr (gtxyz,gtv,tmtr,2)
c
 8000 return
      end
c
c***************************************************************
c
c   SUBROUTINE:  veadja (gtxyz,gtv)
c
c   FUNCTION:  This routine calculates a vector in the machine
c              system (output) from the part system (input).
c
c   INPUT:  gtxyz   R*8  D3  -  The current vector.
c
c   OUTPUT: gtv     R*8  D3  -  The adjusted vector.
c
c***************************************************************
c
      subroutine veadja (gtxyz,gtv)
c
      include 'post.inc'
c
      real*8 gtxyz(3),gtv(3)
c
      equivalence (LASTAB,KPOSMP(1260)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTWRK(20),LASTAB
c
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2)
c
      integer*4 i
c
      real*8 snum
c
c...Adjust vector for all rotary tables
c
      gtv(1) = gtxyz(1)
      gtv(2) = gtxyz(2)
      gtv(3) = gtxyz(3)
      do 100 i=1,LASTAB,1
          snum = 360. - ROTSTO(i,1)
          call vecadj (gtv,gtv,snum,IRTWRK(i))
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***************************************************************
c
c   SUBROUTINE:  veadjr (gtxyz,gtv)
c
c   FUNCTION:  This routine calculates a vector in the part
c              system (input) from the machine system (output).
c
c   INPUT:  gtxyz   R*8  D3  -  The current vector.
c
c   OUTPUT: gtv     R*8  D3  -  The adjusted vector.
c
c***************************************************************
c
      subroutine veadjr (gtxyz,gtv)
c
      include 'post.inc'
c
      real*8 gtxyz(3),gtv(3)
c
      equivalence (LASTAB,KPOSMP(1260)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTWRK(20),LASTAB
c
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2)
c
      integer*4 i
c
c...Adjust vector for all rotary axes
c
      gtv(1) = gtxyz(1)
      gtv(2) = gtxyz(2)
      gtv(3) = gtxyz(3)
      do 100 i=LASTAB,1,-1
          call vecadj (gtv,gtv,ROTSTO(i,1),IRTWRK(i))
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***************************************************************
c
c   SUBROUTINE:  vecadj (gtxyz,gtv,gaxis,kaxis)
c
c   FUNCTION:  This routine rotates an input vector around
c              the specified axis at given angle.
c
c   INPUT:  gtxyz   R*8  D3  -  The current vector.
c           gaxis   R*8  D1  -  The angle to rotate
c           kaxis   I*4  D1  -  Which axis to adjust for.
c
c   OUTPUT: gtv     R*8  D3  -  The adjusted vector.
c
c***************************************************************
c
      subroutine vecadj (gtxyz,gtv,gaxis,kaxis)
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      integer*4 kaxis
c
      real*8 gtxyz(3),gtv(3),gaxis
c
      integer*4 is1,is2,is3,inx(6)
c
      real*8 pt(3),ti,tk,angl
c
      data inx /3,2,3,1,1,2/
c
c...Set up for which axis we are
c...rotating around
c
      is1    = inx(kaxis*2-1)
      is2    = inx(kaxis*2)
      is3    = kaxis
c
c...Set up the angle to rotate around
c
      angl   = gaxis
      if (kaxis .eq. 1) angl = 360.d0 - gaxis
c
      pt(is1) = gtxyz(is1)
      pt(is2) = gtxyz(is2)
c
c...Rotate the ijk position
c...around the rotary axis
c
      ti     = dsin (angl/RAD)
      tk     = dcos (angl/RAD)
      gtv(is1) = pt(is1)*tk - pt(is2)*ti
      gtv(is2) = pt(is2)*tk + pt(is1)*ti
      gtv(kaxis) = gtxyz(kaxis)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  vecang (gtv,kaxis,galph)
c
c   FUNCTION:  This routine defines angle of the vector from the first
c              axis when rotation is around the 3-rd.
c
c   INPUT:  gtv     R*8  D3  -  vector
c
c           kaxis   I*4  D1  -  the rotary axis of vector
c
c   OUTPUT: galph   R*8  D3  -  angle of vector
c
c***********************************************************************
c
      subroutine vecang (gtv,kaxis,galph)
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      integer*4 kaxis
c
      real*8 gtv(3),galph
c
      integer*4 is1,is2,inx(6)
c
      real*8 rvec,rdvec,rtv1
c
      data inx /3,2,3,1,1,2/
c
      is1    = inx(kaxis*2-1)
      is2    = inx(kaxis*2)
c
c...Unitize components of rotary axis
c
      rvec   = dsqrt (gtv(is1)*gtv(is1) + gtv(is2)*gtv(is2))
      rdvec  = dnint(rvec*10.0d0**14) / 10.0d0**14
      if (rdvec .eq. 0.) then
          galph   = 0.
      else
          if (dabs(gtv(is2)) .lt. dabs(gtv(is1))) then
              rtv1  = dsign (1.d0,gtv(is1)) /
     -                dsqrt (1.d0 + (gtv(is2)/gtv(is1))**2)
          else
              rtv1  = gtv(is1) / rvec
          end if
c
c...Do the actual calculation
c
          galph = dacos(rtv1) * RAD
          rdvec = dnint(gtv(is2)*10.0d0**14) / 10.0d0**14
          if (rdvec .lt. 0.) galph = 360.d0 - galph
          if (kaxis .eq. 1) galph = 360.d0 - galph
          if (galph .ge. 360.0) galph = galph - 360.0d0
      endif
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  split_bspl (gptn,gpts)
c
c   FUNCTION:  This routine splints bspline segment at t=.5 into two
c              segments.
c
c   INPUT:  gptn    R*8  D3.4 -  Input spline segment (4 points).
c
c   OUTPUT: gpts    R*8  D3.7 -  Output spline (2 attached segments at
c                                split point).
c
c***********************************************************************
c
      subroutine split_bspl (gptn,gpts)
c
      real*8 gptn(3,*),gpts(3,*)
c
      real*8 q1(3)
c
      call copyn (gptn(1,1),gpts(1,1),3)
      call copyn (gptn(1,4),gpts(1,7),3)
c
      call casteljau (gptn,1,gpts(1,3))
      call casteljau (gptn,2,gpts(1,5))
      call vcplvc (gpts(1,3),gpts(1,5),q1,1.d0)
      call vctmsc (q1,.5d0,gpts(1,4))
      call vcplvc (gptn,gptn(1,2),q1,1.d0)
      call vctmsc (q1,.5d0,gpts(1,2))
      call vcplvc (gptn(1,3),gptn(1,4),q1,1.d0)
      call vctmsc (q1,.5d0,gpts(1,6))
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  casteljau (gptn,ksel,gqpt)
c
c   FUNCTION:  This routine calculates the distance of the point from
c              the line defined by point-vector.
c
c   INPUT:  gptn    R*8  D3.4 -  Spline segment (4 points).
c
c           ksel    I*4  D1   -  Index of selected control point.
c
c   OUTPUT: gqpt    R*8  D3   -  Control point on right/left side from
c                                split point.
c
c***********************************************************************
c
      subroutine casteljau (gptn,ksel,gqpt)
c
      real*8 gptn(3,*),gqpt(3)
      integer*4 ksel
c
      real*8 vec(3)
      integer*4 ns,ie
c
c...set offset index for second control point and segment end point
c...close to selected control point
c
      ns     = 3 - ksel
      ie     = (ksel - 1) * 3
c
c...calculate control point at the segment split point
c
      vec(1) = 2.*gptn(1,1+ksel) + gptn(1,1+ns) + gptn(1,1+ie)
      vec(2) = 2.*gptn(2,1+ksel) + gptn(2,1+ns) + gptn(2,1+ie)
      vec(3) = 2.*gptn(3,1+ksel) + gptn(3,1+ns) + gptn(3,1+ie)
      call vctmsc (vec,.25d0,gqpt)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bezevl (bcrv,u,pcv,vcv)
c
c   FUNCTION:  This routine evaluates a bezier curve segment.
c
c   INPUT:  bcrv    R*8  D3,4 -  Curve segment (4 points).
c           u       R*8  D1   -  Parameter value at which to evaluate.
c
c   OUTPUT: pcv     R*8  D3   -  Point on curve.
c           vcv     R*8  D3   -  Slope of curve.
c
c***********************************************************************
c
      subroutine bezevl (bcrv,u,pcv,vcv)
c
      real*8 bcrv(3,4), u, pcv(3), vcv(3)
c
      real*8 c1,c2,c3,c4,um,v1(3),v2(3),v3(3)
c
      um = 1.d0-u
      c1 = um**3
      c2 = 3.d0*u*um**2
      c3 = 3.d0*u**2*um
      c4 = u**3
      pcv(1) = c1*bcrv(1,1)+c2*bcrv(1,2)+c3*bcrv(1,3)+c4*bcrv(1,4)
      pcv(2) = c1*bcrv(2,1)+c2*bcrv(2,2)+c3*bcrv(2,3)+c4*bcrv(2,4)
      pcv(3) = c1*bcrv(3,1)+c2*bcrv(3,2)+c3*bcrv(3,3)+c4*bcrv(3,4)
      call vcmnvc (bcrv(1,2),bcrv(1,1),v1)
      call vcmnvc (bcrv(1,3),bcrv(1,2),v2)
      call vcmnvc (bcrv(1,4),bcrv(1,3),v3)
      c1 = 3.d0*um**2
      c2 = 6.d0*u*um
      c3 = 3.d0*u**2
      vcv(1) = c1*v1(1)+c2*v2(1)+c3*v3(1)
      vcv(2) = c1*v1(2)+c2*v2(2)+c3*v3(2)
      vcv(3) = c1*v1(3)+c2*v2(3)+c3*v3(3)
c
8000  return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  curvpv (pti, bcrv,pto,vco,kerr)
c
c   FUNCTION:  This routine projects a point on to a bezier curve segment.
c
c   INPUT:  pti     R*8  D3   -  Point to poject.
c           bcrv    R*8  D3,4 -  Curve segment (4 points).
c           u       R*8  D1   -  Initial u value.
c
c   OUTPUT: u       R*8  D1   -  Final u value.
c           pto     R*8  D3   -  Projected point on curve.
c           vco     R*8  D3   -  Slope of curve at pto.
c           kerr    I*4  D1   -  0 if no error, else 1.
c
c***********************************************************************
c
      subroutine curvpv (pti, bcrv, u,  pto, vco, kerr)
c
      real*8 pti(3), bcrv(3,4), u, pto(3), vco(3)
      integer*4 kerr
c
      real*8 ctan,den,uerr,oerr,du
      integer*4 iknt,itim

      kerr = 0
      iknt = 0
      itim = 0
      ctan = 1.d0
   10 call bezevl (bcrv, u, pto, vco)
      iknt = iknt+1
      if (iknt.gt.1000) goto 9000
      den = vco(1)**2+vco(2)**2+vco(3)**2
      if (den.lt.1.d-4) den = 1.d-4
c      uerr = vco(1)*(pti(1)-pto(1))
c      uerr = uerr+vco(2)*(pti(2)-pto(2))
c      uerr = uerr+vco(3)*(pti(3)-pto(3))
c      uerr = uerr/den
      uerr = (vco(1)*(pti(1)-pto(1))+vco(2)*(pti(2)-pto(2))+
     1        vco(3)*(pti(3)-pto(3)))/den
      if (dabs(uerr).lt.1.d-4) goto 200
      if (itim.eq.0) then
        du = .01
        if (uerr.lt.0.d0) du = -.01
      else
        den = oerr-uerr
        if (dabs(den).gt.1.d-3) ctan = du/den
        if (ctan.lt.0.d0) ctan = -ctan
        if (ctan.gt.1.d0) ctan = 1.d0
        du = uerr*ctan
      endif
      if (u+du.gt.1.d0) du = 1.d0-u
      if (u+du.lt.0.d0) du = -u
      itim = itim+1
      u = u+du
      oerr = uerr
      if (itim.lt.100) goto 40
      goto 9000
   40 if (dabs(du).gt.1.d-5) goto 10
c
  200 continue
      call unitvc (vco,vco)
c
8000  return
c
9000  kerr = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptlnds (gptn,gline,gdist)
c
c   FUNCTION:  This routine calculates the distance of the point from
c              the line defined by point-vector.
c
c   INPUT:  gptn    R*8  D3  -  Point coordinates.
c
c           gline   R*8  D6  -  Point-vector (line definition).
c
c   OUTPUT: gdist   R*8  D1  -  Distance point - line.
c
c***********************************************************************
c
      subroutine ptlnds (gptn,gline,gdist)
c
      real*8 gptn(3),gline(6),gdist
c
      real*8 d1,d2,d3,vec(3)
c
      d1     = gptn(1) - gline(1)
      d2     = gptn(2) - gline(2)
      d3     = gptn(3) - gline(3)
c
      call unitvc (gline(4),vec)
      gdist  = dsqrt ((d1*vec(2) - d2*vec(1))**2 +
     -                (d2*vec(3) - d3*vec(2))**2 +
     -                (d3*vec(1) - d1*vec(3))**2)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  plnlnp (gvec1,gvec2,gpt,gpl,kerr)
c
c   FUNCTION:  This routine calculates a plane paralel to two lines &
c              through a point.
c
c   INPUT:  gvec1   R*8  D3  -  Vector of the first line.
c
c           gvec2   R*8  D3  -  Vector of the second line.
c
c           gpt     R*8  D3  -  Point that lies on plane.
c
c   OUTPUT: gpl     R*8  D4  -  Output plane.
c
c           kerr    I*4  D1  -  Returns 1 when the vector does not have
c                               a length.
c
c***********************************************************************
c
      subroutine plnlnp (gvec1,gvec2,gpt,gpl,kerr)
c
      integer*4 kerr
c
      real*8 gvec1(3),gvec2(3),gpt(3),gpl(4)
c
      real*8 rdis
c
c...Calculate plane distance from 0,0,0
c
      kerr   = 0
      gpl(1) = gvec1(2)*gvec2(3) - gvec1(3)*gvec2(2)
      gpl(2) = gvec1(3)*gvec2(1) - gvec1(1)*gvec2(3)
      gpl(3) = gvec1(1)*gvec2(2) - gvec1(2)*gvec2(1)
      gpl(4) = 0.0 - gpl(1)*gpt(1) - gpl(2)*gpt(2) - gpl(3)*gpt(3)
      rdis = dsqrt (gpl(1)**2 + gpl(2)**2 + gpl(3)**2)
      if (rdis .lt. 1.d-7) go to 9000
c
c...End of routine
c
 8000 return
c
c...Zero length vector
c
 9000 kerr   = 1
      gpl(1) = 0.
      gpl(2) = 0.
      gpl(3) = 0.
      gpl(4) = 0.
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  nptln (gpt,gptvc,gpto)
c
c   FUNCTION:  This routine calculates the point on line which is
c              the perpendicular projection of an input point.
c
c   INPUT:  gpt     R*8  D3  -  Input point.
c
c           gptvc   R*8  D6  -  Point-vector defining the line.
c
c   OUTPUT: gpto    R*8  D3  -  Point on the line (projected pepr).
c
c
c***********************************************************************
c
      subroutine nptln (gpt,gptvc,gpto)
c
      real*8 gpt(3),gptvc(6),gpto(3)
c
      real*8 vec(3),r,ndot
c
      vec(1) = gpt(1) - gptvc(1)
      vec(2) = gpt(2) - gptvc(2)
      vec(3) = gpt(3) - gptvc(3)
      r      = ndot (gptvc(4),vec)
      gpto(1) = gptvc(1) + r*gptvc(4)
      gpto(2) = gptvc(2) + r*gptvc(5)
      gpto(3) = gptvc(3) + r*gptvc(6)
      return
      end
c
c*********************************************************************
c*    E_SUBROUTINE     : vctmsc(gvci,gfac,gvco)
c*       Scale vector (<gvco> = gfac*<gvci>).
c*    PARAMETERS
c*       INPUT  :  gvci  R*8  D3   - input vector 1
c*                 gfac  R*8       - factor to applay with vector.
c*       OUTPUT :  gvco  R*8  D3   - output vector
c*
c*    RETURNS      : none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine vctmsc(gvci,gfac,gvco)
c
      real*8 gvci(3),gvco(3),gfac
c
      gvco(1) = gfac * gvci(1)
      gvco(2) = gfac * gvci(2)
      gvco(3) = gfac * gvci(3)
c
 8000 return
      end
c
c*********************************************************************
c*    E_SUBROUTINE     : vcplvc (gvc1,gvc2,gvco,gfac)
c*       Plus/minus vector (<gvco> = <gvc1> + gfac*<gvc2>).
c*    PARAMETERS
c*       INPUT  :  gvc1  R*8  D3   - input vector 1
c*                 gvc2  R*8  D3   - input vector 2
c*                 gfac  R*8       - factor to applay with vector 2.
c*       OUTPUT :  gvco  R*8  D3   - output vector
c*
c*    RETURNS      : none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine vcplvc (gvc1,gvc2,gvco,gfac)
c
      real*8 gvc1(3),gvc2(3),gvco(3),gfac
c
      gvco(1) = gvc1(1) + gfac * gvc2(1)
      gvco(2) = gvc1(2) + gfac * gvc2(2)
      gvco(3) = gvc1(3) + gfac * gvc2(3)
c
 8000 return
      end
c
c*********************************************************************
c*    E_SUBROUTINE     : vcmnvc (gvc1,gvc2,gvco)
c*       Vector minus vector.
c*    PARAMETERS
c*       INPUT  :  gvc1  R*8  D3   - input vector 1
c*                 gvc2  R*8  D3   - input vector 2
c*       OUTPUT :  gvco  R*8  D3   - output vector
c*
c*    RETURNS      : none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine vcmnvc (gvc1,gvc2,gvco)
c
      real*8 gvc1(3),gvc2(3),gvco(3)
c
      gvco(1) = gvc1(1) - gvc2(1)
      gvco(2) = gvc1(2) - gvc2(2)
      gvco(3) = gvc1(3) - gvc2(3)
c
 8000 return
      end
c
c*********************************************************************
c*    E_SUBROUTINE     : crosvc (gvc1,gvc2,gvec)
c*       Calculate the cross product of two vectors (input is safe).
c*    PARAMETERS
c*       INPUT  :  gvc1  R*8  D3   - vector 1
c*
c*                 gvc2  R*8  D3   - vector 2
c*
c*       OUTPUT :  gvec  R*8  D3   - output cross product vector
c*
c*    RETURNS      : none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine crosvc (gvc1,gvc2,gvec)
c
      real*8 gvc1(3),gvc2(3),gvec(3)
c
      real*8 vec(3)
c
      vec(1) = gvc1(2)*gvc2(3) - gvc1(3)*gvc2(2)
      vec(2) = gvc1(3)*gvc2(1) - gvc1(1)*gvc2(3)
      vec(3) = gvc1(1)*gvc2(2) - gvc1(2)*gvc2(1)
      gvec(1) = vec(1)
      gvec(2) = vec(2)
      gvec(3) = vec(3)
c
      return
      end
c
c*********************************************************************
c*    E_SUBROUTINE     : islnln (gpt1,gvc1,gpt2,gvc2,gpt,kerr)
c*       Intersect two lines which are known to lie in the same plane.
c*    PARAMETERS
c*       INPUT  :  gpt1  R*8  D3   - point on line 1
c*
c*                 gvc1  R*8  D3   - unit vector of line 1
c*
c*                 gpt2  R*8  D3   - point on line 2
c*
c*                 gvc2  R*8  D3   - unit vector of line 2
c*
c*       OUTPUT :  gvec  R*8  D3   - intersection point
c*
c*                 kerr  I*4  D1   - 1 = lines parallel, 0 = OK.
c*
c*    RETURNS      : none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine islnln (gpt1,gvc1,gpt2,gvc2,gpt,kerr)
c
      real*8 gpt1(3),gvc1(3),gpt2(3),gvc2(3),gpt(3)
      integer*4 kerr
c
      real*8 ndot,proj,t1(3),t2(3),t
c
      kerr = 0
      proj = ndot(gvc1,gvc2)
      if (dabs(proj) .gt. .99999d0) then
          kerr = 1
      else
          call vcplvc (gpt1,gpt2,t1,-1.d0)
          t2(1) = gvc2(1) * proj
          t2(2) = gvc2(2) * proj
          t2(3) = gvc2(3) * proj
          call vcplvc (t2,gvc1,t2,-1.d0)
          t = ndot (t1,t2)
          t = t / (1.d0 - proj*proj)
          call vcplvc (gpt1,gvc1,gpt,t)
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  isciln (gprm,kprm,glin,gpt,knum)
c
c   FUNCTION:  This routine intersects circle with line.  Circle is
c              defined by gprm & kprm (same format as RCIPRM & ICIPRM),
c              line is defined by point glpt and unitvector glvc.
c              CI and LN must be coplanar.
c
c   INPUT:  gprm    R*8  D25   -  Circle parameters (see RCIPRM)
c
c           kprm    I*4  D1    -  Circle parameters (see ICIPRM)
c
c           glin    R*8  D6    -  Point-vector defining line
c
c   OUTPUT: gpt     R*8  D6    -  Intersection points coordinates.
c
c           knum    I*4  D1    -  Number of i/o points.
c
c***********************************************************************
c
      subroutine isciln (gprm,kprm,glin,gpt,knum)
c
      include 'post.inc'
      integer*4 kprm(8),knum
      real*8 gprm(25),glin(6),gpt(6)
c
      equivalence (PPTOLR,POSMAP(1274))
c
      real*8 PPTOLR(10)
c
      real*8 ndist,pcn(3),r,dis
      integer*4 num
c
      knum   = 0
      if (dabs(glin(kprm(3))-gprm(kprm(3))) .gt. PPTOLR(kprm(3)*2-1))
     -          go to 8000
c
c...intersect circle with line
c
      call nptln (gprm,glin,pcn)
      dis  = ndist (pcn,gprm)
      if (dis .gt. gprm(4)) go to 8000
      if (dis .lt. gprm(4)-PPTOLR(kprm(1)*2-1)) then
         r   = dsqrt (gprm(4)**2 - dis*dis)
         call vcplvc (pcn,glin(4),gpt(1),r)
         call vcplvc (pcn,glin(4),gpt(4),-r)
         num = 2
      else
         call copyn (pcn,gpt,3)
         num = 1
      end if
      knum   = num
c
 8000 return
      end
c
c*********************************************************************
c*    E_SUBROUTINE     : unitvc (gvin,gvec)
c*       Unitize vector
c*    PARAMETERS
c*       INPUT  :  gvin  R*8  D3   - input vector
c*
c*       OUTPUT :  gvec  R*8  D3   - unitized output vector
c*
c*    RETURNS      : none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine unitvc (gvin,gvec)
c
      real*8 gvin(3),gvec(3)
c
      real*8 ndot, dd
c
      dd = dsqrt(ndot(gvin,gvin))
      if (dd .eq. 0.) dd = 1.0
      gvec(1) = gvin(1) / dd
      gvec(2) = gvin(2) / dd
      gvec(3) = gvin(3) / dd
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  intvec1 (gang,gan1,gdlt,kdir,kflg)
c
c   FUNCTION:  This routine checks if angle is inside sector defined
c              by start and delta angle in specified direction.  All
c              angles are on rotary scale.
c
c   INPUT:  gang    R*8  D1    -  Angle to check
c
c           gan1    R*8  D1    -  Start angle of arc
c
c           gdlt    R*8  D1    -  Delta angle of arc
c
c           kdir    I*4  D1    -  Arc direction:1 - CLW, 2 - CCLW.
c
c   OUTPUT: kflg    I*4  D1    -  1 = inside, 0 = outside sector.
c
c***********************************************************************
c
      subroutine intvec1 (gang,gan1,gdlt,kdir,kflg)
c
      real*8 gang,gan1,gdlt
      integer*4 kdir,kflg
c
      real*8 ful,a1,a2
c
      data ful /360.d0/
c
      a1     = gan1
      kflg   = 0
      if (kdir .eq. 2) then
         a2 = gan1 + gdlt
         if (gang .ge. a1) then
            if (gang .le. a2) kflg = 1
         else
            if (a2 .ge. ful) then
               a2 = a2 - ful
               if (gang .le. a2) kflg = 1
            end if
         end if
      else
         a2 = gan1 - gdlt
         if (gang .le. a1) then
            if (gang .gt. a2) kflg = 1
         else
            if (a2 .le. 0.0) then
               a2 = a2 + ful
               if (gang .ge. a2) kflg = 1
            end if
         end if
      end if
c
      return
      end
c
c********************************************************************/
c   FUNCTION:  ndot (g1,g2)
c
c   FUNCTION:  This function returns dot product of 2 vectors.
c
c   INPUT:  g1    R*8  D3    -  Vector 1.
c
c           g2    R*8  D3    -  Vector 2.
c
c   OUTPUT:  none
c
c********************************************************************
      function ndot (g1,g2)
c
      real*8 g1(3),g2(3),ndot
c
      ndot = g1(1)*g2(1)+g1(2)*g2(2)+g1(3)*g2(3)
c
      return
      end
c
c********************************************************************/
c   FUNCTION:  ndist (g1,g2)
c
c   FUNCTION:  This function returns distance between 2 points.
c
c   INPUT:  g1    R*8  D3    -  Cartesian coordinates of point 1.
c
c           g2    R*8  D3    -  Cartesian coordinates of point 2.
c
c   OUTPUT:  none
c
c********************************************************************
      function ndist (g1,g2)
c
      real*8 g1(3),g2(3),ndist
c
      ndist = dsqrt((g1(1)-g2(1))**2+(g1(2)-g2(2))**2+(g1(3)-g2(3))**2)
c
      return
      end
c
c********************************************************************/
c   FUNCTION:  nmag (gvec)
c
c   FUNCTION:  This function returns magnitude of vector.
c
c   INPUT:  gvec    R*8  D3    -  Input vector.
c
c   OUTPUT:  none
c
c********************************************************************
      function nmag (gvec)
c
      real*8 gvec(3),nmag
c
      nmag = dsqrt(gvec(1)**2+gvec(2)**2+gvec(3)**2)
c
      return
      end
c
c********************************************************************/
c   FUNCTION:  angl2p (gvec1,gvec2,gnvec)
c
c   FUNCTION:  This function returns the angle between two vectors
c              relative to a normal vector.  The angle will be between
c              0 and 360 degrees.
c
c   INPUT:  gvec1    R*8  D3    -  First vector.
c
c           gvec2    R*8  D3    -  Second vector.
c
c           gnvec    R*8  D3    -  Vector normal to 'gvec1' and 'gvec2'
c                                  that the angle will be relative to.
c
c   OUTPUT:  none
c
c   RETURNS: Angle between vectors.
c
c********************************************************************
      function angl2p (gvec1,gvec2,gnvec)
c
      include 'post.inc'
c
      real*8 gvec1(3),gvec2(3),gnvec(3),angl2p
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 PI,RAD
c
      real*8 vc1(3),vc2(3),temp,ndot,ndist,angle,tempvec(3),nmag
c
c...Unitize vectors
c
      call unitvc (gvec1,vc1)
      call unitvc (gvec2,vc2)
c
c...Calculate angle between vectors
c
      temp   = ndot(vc1,vc2)
      if (temp .gt. .9999) then
          angle  = ndist(vc1,vc2)
      else
          if (temp .lt. -.9999) then
              call vcplvc (vc1,vc2,tempvec,1.d0)
              angle  = PI     - nmag(tempvec)
          else
              angle = dacos(temp)
          endif
      endif
c
c...Set angle direction
c
      angle  = angle  * RAD
      call crosvc (gvec1,gvec2,tempvec)
      temp = ndot(tempvec,gnvec)
      if (temp .lt. 0.) angle = 360.d0 - angle
c
c...End of routine
c
      angl2p = angle
      return
      end
c
c********************************************************************/
c   FUNCTION:  isparl (gvec1,gvec2)
c
c   FUNCTION:  This function determines if two vectors are parallel.
c
c
c   INPUT:  gvec1    R*8  D3    -  First vector.
c
c           gvec2    R*8  D3    -  Second vector.
c
c           gnvec    R*8  D3    -  Vector normal to 'gvec1' and 'gvec2'
c                                  that the angle will be relative to.
c
c   OUTPUT:  none
c
c   RETURNS: 0 if vectors are not parallel, 1 if vectors are the same,
c            -1 if vectors are in opposite direction.
c
c********************************************************************
      function isparl (gvec1,gvec2)
c
      include 'post.inc'
c
      real*8 gvec1(3),gvec2(3)
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 PI,RAD
c
      integer*4 isparl
c
      real*8 ndot,ndist
c
c...Determine if angles are parallel
c
      if (1.d0 - dabs(ndot(gvec1,gvec2)) .lt. 1.e-6) then
          isparl = 1
          if (ndist(gvec1,gvec2) .gt. .0001) isparl = -1
      else
          isparl = 0
      endif
c
c...End of routine
c
 8000 return
      end
