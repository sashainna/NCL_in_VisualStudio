c
c***********************************************************************
c
c   FILE NAME:  math
c   CONTAINS:
c             betvec  getvan  plndis plnint plnvpt setijk ptlnds gtpola 
c             ptmatr ptmatb vctovc f_mag vcplvc vcmnvc uvcplvc unitvc
c             vcpara conv_4_8 conv8_4 conv4_4 conv8_8 f_cross f_dot vctmsc 
c             f_dist plplcross point_on_line point_on_plane 
c             fill_array mnvc unitizevc avcplbvc triple_cross 
c             triple_dot intrpt intrvec lnlncross xyzvc min2_r8 max2_r8
c             max3_r8 min3_r8 perpvc
c
C*    MODULE NAME AND RELEASE LEVEL
C*        math.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:16
c
c***********************************************************************
c
c***********************************************************************
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
c***********************************************************************
c
      subroutine betvec (gvec1,gvec2,gang)
      implicit real*8 (a-h,o-z)
      real*8 gvec1(3),gvec2(3)
c
      data PI /3.14159 26535 89793 2D0/, one/1.d00/
c
      cosb = f_dot(gvec1,gvec2)
      if (cosb .lt. -one) then
         cosb = -one
      else if (cosb .gt.one) then
         cosb = one
      endif
      gang   = 180.0/PI * dacos (cosb)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getvan (gtv1,gtv2,gan1,gan2,gdlt,gfi,gtheta,gtvmp,ifl)
c
c   FUNCTION:  This routine defines plane between two vectors and
c              vectors' angles on this plane (Tool Vector Motion Plane).
c
c   INPUT:  gtv1    R*8  D3  -  first vector
c
c           gtv2    R*8  D3  -  second vector
c
c   OUTPUT: gan1    R*8  D1  -  angle of the gtv1 on TVMP
c
c           gan2    R*8  D1  -  angle of the gtv2 on TVMP
c
c           gdlt    R*8  D1  -  delta angle between gtv1 and gtv2
c
c           gfi     R*8  D1  -  angle from XY-plane to the TVMP
c
c           gtheta  R*8  D1  -  angle from X-axis to XY trace of plane
c
c           gtvmp   R*8  D3  -  vector defining TVMP
c
c*********************************************************************
c
      subroutine getvan (gtv1,gtv2,gan1,gan2,gdlt,gfi,
     *                               gtheta,gtvmp,ifl)
      implicit real*8 (a-h,o-z)
c
      real*8 gtv1(3),gtv2(3),gtvmp(3),pln(3),tvlin(3),tl(3)
      integer*4 ifl,n1,n2,n3
      data n1/1/,n2/2/,n3/3/,one/1.d00/,zero/0.d00/,small1/1.d-10/,
     *     small2/1.d-12/,twopi/360.d00/
c
      ifl    = 0
c
c...get plane on vectors
c
      call f_cross(gtv1,gtv2,pln)
c
      vec = f_mag(pln)
      p   = dabs(f_dot(gtv1,gtv2))
      if (p .gt.one - small1 .or.vec.lt.small1) ifl = 1
c
c...check if vectors are parallel
c
      if (ifl .eq. 0) go to 400
c
c...parallel - use vertical plane
c
      pln(3) = zero
      pln(1) = gtv1(2)
      pln(2) = - gtv1(1)
      vec    = dsqrt (pln(1)**2 + pln(2)**2)
      if (vec .lt. small2) then
          pln(1) = zero
          pln(2) = one
          vec    = one
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
      tvlin(3)  = zero
      if (p .gt. small2) then
          tvlin(1)  = - pln(2) / p
          tvlin(2)  = pln(1) / p
      else
          tvlin(1)  = one
          tvlin(2)  = zero
      end if
c
c...get line angle (from X-axis) and plane angle (from XY-plane)
c
      call vecang (tvlin,n3,gtheta)
      alp    = twopi - gtheta
      call vecadj (pln,tl,alp,n3)
      call vecang (tl,n1,gfi)
      bet    = twopi - gfi
c
c...get start angle
c
      call vecadj (gtv1,tl,alp,n3)
      call vecadj (tl,tl,bet,n1)
      call vecang (tl,n3,gan1)
c
c...get delta angle - shortest way
c
      call betvec (gtv1,gtv2,gdlt)
c
c...get end angle
c
      gan2   = gan1 + gdlt
c
      return
      end
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
      implicit real*8 (a-h,o-z)
      real*8 gpl(4),gpt(3)
      integer*4 is
c
c...Calculate distance of point to plane
c
      gdis   = f_dot(gpl,gpt) - gpl(4)
c
c...Change sign to match direction of vector
c
c       is     = 3
c       if (dabs(gpl(1)) .gt. dabs(gpl(is))) is = 1
c       if (dabs(gpl(2)) .gt. dabs(gpl(is))) is = 2

      return
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
      implicit real*8 (a-h,o-z)
      real*8 gpt(3),gvec(3),gpl(4),gout(3)
      integer*4 kerr
      data small/1.d-06/
c
c...Intersect vector with plane
c
      coso   = f_dot(gvec,gpl)
      if (dabs(coso).le.small) then
c
c...Vector does not intersect plane
c
         kerr   = 1
      else
c
c...Calculate new point
c
         kerr   = 0
         dlen = (f_dot(gpl,gpt) - gpl(4))/coso
         call uvcplvc(gpt,gvec,gout, -dlen)
      endif

      return
      end
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
      implicit real*8 (a-h,o-z)
      real*8 gvec(3),gpt(3),gpl(4)
      integer*4 kerr
      data zero/0.d00/,one/1.d00/
c
c...Calculate plane distance from 0,0,0
c
      rdis = f_mag(gvec)
      if (rdis .eq. zero) then
         kerr   = 1
         gpl(1) = zero
         gpl(2) = zero
         gpl(3) = zero
         gpl(4) = zero
      else
         kerr   = 0
         call vctmsc(gvec,gpl,one/rdis)
         gpl(4) = f_dot(gpl,gpt)
      endif 

      return
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
      implicit real*8 (a-h,o-z)
      real*8 gtv(3),vec(3)
      integer*4 n1,n2,n3
      data n1/1/, n2/2/, n3/3/, one/1.d00/, zero/0.d00/
c
c...start from X-axis
c
      vec(1) = one
      vec(2) = zero
      vec(3) = zero
c
c...rotate X on XY-plane up to desired angle
c
      call vecadj (vec,vec,gangn,n3)
c
c...rotate vector to match with plane
c
      call vecadj (vec,vec,gfi,n1)
c
c...rotate plane at 'XY trace of plane' angle
c
      call vecadj (vec,gtv,gtheta,n3)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptlnds (gptn,gline,gdis)
c
c   FUNCTION:  This routine calculates the distance of the point from
c              the line defined by point-vector.
c
c   INPUT:  gptn    R*8  D3  -  Point coordinates.
c
c           gline   R*8  D6  -  Point-vector (line definition).
c
c   OUTPUT: gdis    R*8  D1  -  Distance point - line.
c
c***********************************************************************
c
      subroutine ptlnds(gptn,gline,gdis)
      implicit real*8 (a-h,o-z)
      real*8 gptn(3),gline(6), x(3)

      call vcmnvc(gptn,gline,x)

      a = f_dot(x,gline(4))
      a = f_dot(x,x) - a*a
      if ( a.gt.0.0) then
         gdis = dsqrt(a)
      else
         gdis = 0.
      endif

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtpola (gvecz,gang,gmatr)
c
c   FUNCTION:  This routine calculates conversion matrix to convert
c              coordinates from original system to the rotated.
c
c   INPUT:  gvecz   R*8  D3  -  Z-axis vector of new system defined in
c                               old system.
c
c   OUTPUT: gang    R*8  D2  -  Spherical coordinates of Z-axis vector
c                               (1) attitude angle, (2) longtitude angle.
c
c           gmatr   R*8  D12 -  Conversion matrix (rotations only) from
c                               old cartesian system to new (rotated)
c                               cartesian system.
c
c***********************************************************************
c
      subroutine gtpola (gvecz,gang,gmatr)
      implicit real*8 (a-h,o-z)
      real*8 gvecz(3),gang(2),gmatr(4,3),vc(3)
      integer*4 n3
      data n3/3/, zero/0.d00/
c
c...RAD is local constant here (name from postworks)
c
      RAD    = 180.d0/dacos(-1.d0)
c
c...Convert Z vector to spheric coordinates
c
      call vctovc(gvecz,vc)
      call vecang (vc,n3,tab)
      piv = dacos (vc(3)) * RAD
      gang(1) = tab
      gang(2) = piv
c
c...Get conversion matrix
c
      c1      = dcos (piv/RAD)
      s1      = dsin (piv/RAD)
      c2      = dcos (tab/RAD)
      s2      = dsin (tab/RAD)
      gmatr(1,1) = c1 * c2
      gmatr(2,1) = zero - s2
      gmatr(3,1) = s1 * c2
      gmatr(1,2) = s2 * c1
      gmatr(2,2) = c2
      gmatr(3,2) = s1 * s2
      gmatr(1,3) = zero - s1
      gmatr(2,3) = zero 
      gmatr(3,3) = c1
      gmatr(4,1) = zero 
      gmatr(4,2) = zero
      gmatr(4,3) = zero 

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptmatr (gpti,gpto,gmat,ktyp)
c
c   FUNCTION:  This routine converts point/vector coordinates from
c              system I to system II using convertion matrix.  The
c              value of determinant must be +/-1.0 (no scale appl.).
c
c   INPUT:  gpti    R*8  D3    -  Input coordinates.
c
c           gmat    R*8  D4.3  -  Conversion matrix.
c
c           ktyp    I*2  D1    -  Input entity type: 1 - point (adjust
c                                 coordinates for displacement),
c                                 2 - vector (displacement not used).
c
c   OUTPUT: gpto    R*8  D3    -  Output coordinates.
c
c***********************************************************************
c
      subroutine ptmatr (gpti,gpto,gmat,ktyp)
      implicit real*8 (a-h,o-z)
      real*8 gpti(3),gpto(3),gmat(4,3),vec(3)
      integer*2 ktyp,i
      data zero/0.d00/
c
      rnum   = zero
      do 110 i=1,3
          if (ktyp .eq. 1) rnum = gmat(4,i)
          vec(i) = gpti(i) - rnum
  110 continue
c
      do 320 i=1,3
          gpto(i) = vec(1)*gmat(i,1) + vec(2)*gmat(i,2) +
     -              vec(3)*gmat(i,3)
  320 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptmatb (gpti,gpto,gmat,ktyp)
c
c   FUNCTION:  This routine converts point/vector coordinates back from
c              system II to system I using convertion matrix using the
c              same MX created for conversion I -> II.  The value
c              of determinant must be +/-1.0 (no scale appl.).
c
c   INPUT:  gpti    R*8  D3    -  Input coordinates.
c
c           gmat    R*8  D4.3  -  Conversion matrix.
c
c           ktyp    I*2  D1    -  Input entity type: 1 - point (adjust
c                                 coordinates for displacement),
c                                 2 - vector (displacement not used).
c
c   OUTPUT: gpto    R*8  D3    -  Output coordinates.
c
c***********************************************************************
c
      subroutine ptmatb (gpti,gpto,gmat,ktyp)
      real*8 gpti(3),gpto(3),gmat(4,3),vec(3)
      integer*4 i
      integer*2 ktyp
c
      call vctovc(gpti,vec)
      do 1 i=1,3
          gpto(i) = vec(1)*gmat(1,i) + vec(2)*gmat(2,i) +
     -              vec(3)*gmat(3,i)
          if (ktyp .eq. 1) gpto(i) = gpto(i) + gmat(4,i)
  1   continue
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  vctovc (gpti,gpto)
c
c   FUNCTION:  This routine is equivalent to um_vctovc. 
c
c   INPUT:  gpti    R*8  D3    -  Input coordinates.
c
c   OUTPUT: gpto    R*8  D3    -  Output coordinates.
c
c***********************************************************************
      subroutine vctovc (gpti,gpto)
      real*8 gpti(3),gpto(3)
      integer*2 i
c
      do 1 i=1,3
  1      gpto(i) = gpti(i)

      return
      end
c***********************************************************************
c
c   FUNCTION:  f_mag (v)
c
c   FUNCTION:  calc. magnitude of a vector
c
c   INPUT:  v    R*8  D3    -  Input vector.
c
c   OUTPUT: returns ||v|| 
c
c***********************************************************************
      function f_mag (v)
      implicit real*8 (a-h,o-z)
      real*8 v(3), zero/0.d00/
      integer*2 i
c
      r = zero
      do 1 i=1,3
  1      r = r + v(i)**2
      f_mag = dsqrt(r)

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  vcplvc (gpi1,gpi2,gpo)
c
c   FUNCTION:  This routine is equivalent to um_vcplvc. 
c
c   INPUT:  gpi1    R*8  D3    -  First vector.
c
c           gpi2    R*8  D3    -  Second vector.
c
c   OUTPUT: gpo     R*8  D3    -  Resultant vector.
c
c***********************************************************************
      subroutine vcplvc (gpi1,gpi2,gpo)
      real*8 gpi1(3),gpi2(3),gpo(3)
      integer*2 i
c
      do 1 i=1,3
  1      gpo(i) = gpi1(i) + gpi2(i)

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  vcmnvc (gpi1,gpi2,gpo)
c
c   FUNCTION:  This routine is equivalent to um_vcmnvc. 
c
c   INPUT:  gpi1    R*8  D3    -  First vector.
c
c           gpi2    R*8  D3    -  Second vector.
c
c   OUTPUT: gpo     R*8  D3    -  Resultant vector.
c
c***********************************************************************
      subroutine vcmnvc (gpi1,gpi2,gpo)
      real*8 gpi1(3),gpi2(3),gpo(3)
      integer*2 i
c
      do 1 i=1,3
  1      gpo(i) = gpi1(i) - gpi2(i)

      return
      end
c***********************************************************************
c
c
c   SUBROUTINE:  uvcplvc (gpi1,gpi2,gpo,gfc)
c
c   FUNCTION:  This is universal +/- vector*factor routine
c
c   INPUT:  gpi1    R*8  D3    -  First vector.
c
c           gpi2    R*8  D3    -  Second vector.
c
c           gfc     R*8  D1    -  factor to mult gpi2.
c
c   OUTPUT: gpo     R*8  D3    -  Resultant vector.
c
c***********************************************************************
      subroutine uvcplvc (gpi1,gpi2,gpo,gfc)
      implicit real*8 (a-h,o-z)
      real*8 gpi1(3),gpi2(3),gpo(3)
      integer*2 i
c
      do 1 i=1,3
  1      gpo(i) = gpi1(i) + gfc*gpi2(i)

      return
      end
c***********************************************************************
c
c   SUBROUTINE:   unitvc (gpi,gpo)
c
c   FUNCTION:  calculates unit vector
c
c   INPUT:  gpi    R*8  D3    -   vector.
c
c   OUTPUT: gpo    R*8  D3   -  Resultant vector.
c
c   NOTE:   returns the input vector if its length is too small 
c
c***********************************************************************
      subroutine unitvc (gpi,gpo)
      implicit real*8 (a-h,o-z)
      real*8 gpi(3),gpo(3), small /1.d-8/
      integer*2 i
c
      gfc = f_mag(gpi)
      if(gfc.gt.small) then
         do 1 i=1,3
  1         gpo(i) = gpi(i)/gfc
      else
         do 2 i=1,3
  2         gpo(i) = gpi(i)
      endif

      return
      end
c***********************************************************************
c
c   FUNCTION:  vcpara (v1,v2)
c
c   FUNCTION:  Determine if two vectors are parallel
c
c   INPUT:  v1    R*8  D3    -  First vector.
c           v2    R*8  D3    -  Second vector.
c
c   OUTPUT: returns ||v|| 
c
c***********************************************************************
      integer*4 function vcpara (v1,v2)
      implicit real*8 (a-h,o-z)
      real*8 v1(3),v2(3),small /1.d-8/
c
      integer*2 i
c
      real*8 a
c
      vcpara = 0
      a = f_dot(v1,v2)
      if ((1.-dabs(a)) .lt. small) vcpara = 1
      return
      end
c***********************************************************************
c
c   SUBROUTINE:   conv4_8(gpi,gpo,n)
c
c   FUNCTION: converts array real*4 of n elements into array real*8 
c
c   INPUT:  gpi    R*4  Dn    - array
c
c   OUTPUT: gpo    R*8  Dn   -  Resultant array
c
c***********************************************************************
      subroutine conv4_8(gpi,gpo,n)
      real*4  gpi(n)
      real*8 gpo(n)
      integer*2 i
          
      do 1 i=1,n
1     gpo(i) = gpi(i)
      return
      end
c***********************************************************************
c
c   SUBROUTINE:   conv4_8(gpi,gpo,n)
c
c   FUNCTION: converts array real*4 of n elements into array real*8 
c
c   INPUT:  gpi    R*4  Dn    - array
c
c   OUTPUT: gpo    R*8  Dn   -  Resultant array
c
c***********************************************************************
      function fmt48(gpi)
      real*4 gpi
      real*8 fmt48
      integer*4 j

      if (gpi .gt. 0) then
        j = 10000.*gpi + 0.5
        fmt48 = j/10000.
      else if (gpi .lt. 0) then
        j = 10000.*gpi - 0.5
        fmt48 = j/10000.
      else
         fmt48 = 0
      endif

      return
      end
c***********************************************************************
c
c   SUBROUTINE:   conv8_4(gpi,gpo,n)
c
c   FUNCTION: converts array real*8 of n elements into array real*4
c
c   INPUT:  gpi    R*8  Dn    - array
c
c   OUTPUT: gpo    R*4  Dn   -  Resultant array
c
c***********************************************************************
      subroutine conv8_4(gpi,gpo,n)
      real*8 gpi(n)
      real*4 gpo(n)
      integer*2 i

      do 1 i=1,n
 1       gpo(i) = gpi(i)
      return
      end
c***********************************************************************
c
c   SUBROUTINE:   conv4_4(gpi,gpo,n)
c
c   FUNCTION: writes array real*4 of n elements into another array real*4
c
c   INPUT:  gpi    R*4  Dn    - array
c
c   OUTPUT: gpo    R*4  Dn   -  Resultant array
c
c***********************************************************************
      subroutine conv4_4(gpi,gpo,n)
      real*4 gpi(n), gpo(n)
      integer*2 i

      do 1 i=1,n
 1       gpo(i) = gpi(i)
      return
      end
c***********************************************************************
c
c   SUBROUTINE:   conv8_8(gpi,gpo,n)
c
c   FUNCTION: writes array real*4 of n elements into another array real*4
c
c   INPUT:  gpi    R*8  Dn    - array
c
c   OUTPUT: gpo    R*8  Dn   -  Resultant array
c
c***********************************************************************
      subroutine conv8_8(gpi,gpo,n)
      real*8 gpi(n), gpo(n)
      integer*2 i

      do 1 i=1,n
 1       gpo(i) = gpi(i)
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  f_cross(a, b, v)
c
c   FUNCTION: calculates cross product of 2 vectors
c
c   INPUT:  a    R*8  D3    - 1st vector
c           b    R*8  D3    - 2nd vector
c
c   OUTPUT: v    R*8  D3   -  Resultant vector; v = axb
c
c***********************************************************************
      subroutine f_cross(a, b, v)
      real*8 a(3), b(3), v(3), v1,v2,v3

      v1 = a(2)*b(3) - a(3)*b(2)
      v2 = a(3)*b(1) - a(1)*b(3)
      v3 = a(1)*b(2) - a(2)*b(1)

      v(1) = v1
      v(2) = v2
      v(3) = v3

      return
      end
c***********************************************************************
c
c   FUNCTION:  f_dot(a,b)
c
c   FUNCTION: calculates dot product of 2 vectors
c
c   INPUT:  a    R*8  D3    - 1st vector
c           b    R*8  D3    - 2nd vector
c
c   RETURNS: f_dot   R*8  D1   - dot product (a,b) 
c
c***********************************************************************
      function f_dot(a,b)
      implicit real*8 (a-h,o-z)
      real*8 a(3), b(3), zero/0.d00/
      integer*2 i

      d = zero
      do 1 i=1,3
 1       d = d + a(i)*b(i)    
      f_dot = d

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  vctmsc(vi,vo, fac)
c
c   FUNCTION: multiplies vector by scalar
c
c   INPUT:  vi    R*8  D3    - vector to be multiplied
c           fac   R*8  D1    - scalar
c
c   OUTPUT: vo    R*8  D3   -  Resultant vector; vo = fac*vi
c
c***********************************************************************

      subroutine vctmsc(vi,vo, fac)
      implicit real*8 (a-h,o-z)
      real*8 vi(3), vo(3)
      integer*2 i

      do 1 i=1,3
 1       vo(i) = vi(i)*fac   

      return
      end
c***********************************************************************
c
c   FUNCTION:  f_dist(p1,p2)
c
c   FUNCTION: calculates distance between 2 points
c
c   INPUT:  p1       R*8  D3    - 1st point
c           p2       R*8  D3    - 2nd point
c
c   RETURNS: f_dist  R*8  D1   - distance between p1,p2
c
c***********************************************************************
      function f_dist(p1,p2)
      implicit real*8 (a-h,o-z)
      real*8 p1(3), p2(3),zero/0.d00/
      integer*2 i

      d = zero
      do 1 i=1,3
 1       d = d + (p1(i)-p2(i))**2    
      f_dist = dsqrt(d)

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  plplcross(p1,n1,p2,n2,line,fail)
c
c   FUNCTION: finds line of intersection of 2 planes 
c
c   INPUT:  p1    R*8  D3    -  a point on first plane
c           n1    R*8  D3    -  unit vector normal to 1st plane
c
c           p2    R*8  D3    -  a point on 2nd plane
c           n2    R*8  D3    -  unit vector normal to 2nd plane
c
c   OUTPUT: line  R*8  D6    -  line of intersection (point + unit vector)
c           fail  L          -  true if planes don't intersect; false otherwise
c
c***********************************************************************
      subroutine plplcross(p1,n1,p2,n2,line,fail)
      implicit real*8 (a-h,o-z)
      real*8 p1(3),n1(3),p2(3),n2(3),line(6),a1(3),vec(3),
     *       small/1.d-06/
      logical fail

      call f_cross(n1,n2,vec)
      fail = dabs(f_mag(vec)).lt.small
      if(fail) return

      call unitvc(vec,line(4))
      call f_cross(n1,line(4),a1)

      call vcmnvc(p2,p1,vec)
      al = f_dot(vec,n2)/f_dot(a1,n2)

      call uvcplvc(p1,a1,line,al)

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  point_on_line(p, ln, pres)
c
c   FUNCTION: projects point on a line 
c
c   INPUT:  p     R*8  D3    -  given point
c           ln    R*8  D6    -  given line
c
c   OUTPUT: pres  R*8  D3    -  resultant point
c
c***********************************************************************
      subroutine point_on_line(p, ln, pres)
      implicit real*8 (a-h,o-z)
      real*8 p(3),ln(6),pres(3), vec(3)

      call vcmnvc(p, ln, vec)
      t = f_dot(ln(4),vec)
      call uvcplvc(ln(1), ln(4), pres, t) 

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  point_on_plane(point,p,n,pres)
c
c   FUNCTION: projects point on  plane 
c
c   INPUT:  point  R*8  D3  -  given point
c           p,n    R*8  D3  -  a point and unit normal defining plane
c
c   OUTPUT: pres   R*8  D3  -  resultant point
c
c***********************************************************************
      subroutine point_on_plane(point,p,n,pres)
      implicit real*8 (a-h,o-z)
      real*8 point(3),p(3),n(3),pres(3),vec(3),a(3)
     
      call vcmnvc(point,p,vec)
      call vctmsc(n,a,f_dot(vec,n))
      call vcmnvc(point,a,pres)

      return
      end
c***********************************************************************
c
c   SUBROUTINE: fill_array(v,n,value)
c
c   FUNCTION: assigns same value to all elements of an array
c
c   INPUT:  v      R*8  Dn  -  array to be filled 
c           n      I*2  D1  -  dim(v)
c           value  R*8  D1  -  value to be assigned to all elements of v
c
c   OUTPUT: array v: v(i) = value, i=1,...,n
c
c***********************************************************************
      subroutine fill_array(v,n,value)
      implicit real*8 (a-h,o-z)
      real*8 v(n)

      do 1 i = 1,n
 1       v(i) = value
     
      return
      end
c***********************************************************************
c
c   SUBROUTINE: mnvc(v)
c
c   FUNCTION: changes sign of a vector
c
c   INPUT:  v      R*8  D3  -  a vector 
c
c   OUTPUT: vector v = -v
c
c***********************************************************************
      subroutine mnvc(v)
      implicit real*8 (a-h,o-z)
      real*8 v(3)

      do 1 i = 1,3
 1       v(i) = -v(i)
     
      return
      end
c***********************************************************************
c
c   SUBROUTINE: unitizevc(v)
c
c   FUNCTION: makes unit vector of a vector
c
c   INPUT:  v      R*8  D3  -  a vector 
c
c   OUTPUT: vector v = v/||v||
c
c***********************************************************************
      subroutine unitizevc(v)
      implicit real*8 (a-h,o-z)
      real*8 v(3),zero/0.d0/
      integer*2 i
c
      f = f_mag(v)
      if(f.gt.zero) then
         do 1 i=1,3
  1         v(i) = v(i)/f
      endif

      return
      end
c***********************************************************************
c
c   SUBROUTINE: avcplbvc(a,v1,b,v2,vres)
c
c   FUNCTION: calc. linear combination of 2 vectors
c
c   INPUT:  
c          a      R*8  D1  -   number
c          v1     R*8  D3  -   vector 
c          b      R*8  D1  -   number
c          v2     R*8  D3  -   vector 
c
c   OUTPUT: vector vres = a*v1 + b*v2
c
c***********************************************************************
      subroutine avcplbvc(a,v1,b,v2,vres)
      implicit real*8 (a-h,o-z)
      real*8 v1(3),v2(3),vres(3)

      do 1 i=1,3
 1       vres(i) = a*v1(i) + b*v2(i)
     
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  triple_cross(a,b,c,v)     
c
c   FUNCTION: calc. vector product of 3 vectors: [[a,b],c]
c
c   INPUT:  
c          a      R*8  D3  -   1st vector
c          b      R*8  D3  -   2nd vector
c          c      R*8  D3  -   3rd vector
c
c   OUTPUT: 
c           v      R*8  D3  -   resulting vector 
c
c***********************************************************************
      subroutine triple_cross(a,b,c,v)
      real*8 a(3),b(3),c(3),v(3),ac,bc,f_dot
c
c...  [[a,b],c] = (a,c)*b - (b,c)*a ;  [a,[b,c]] = (a,c)*b - (a,b)
c
      ac = f_dot(a,c)
      bc = f_dot(b,c)
      call avcplbvc(ac,b,-bc,a,v)
 
      return
      end
c***********************************************************************
c
c   FUNCTION:  triple_dot(a,b,c)     
c
c   FUNCTION: calc. scalar triple product of 3 vectors: (a,[b,c])
c
c   INPUT:  
c          a      R*8  D3  -   1st vector
c          b      R*8  D3  -   2nd vector
c          c      R*8  D3  -   3rd vector
c
c   OUTPUT: 
c           Returns the result (R*8) 
c
c***********************************************************************
      function triple_dot(a,b,c)
      implicit real*8 (a-h,o-z)
      real*8 a(3),b(3),c(3)

      res = a(1)*(b(2)*c(3) - b(3)*c(2)) + 
     *      a(2)*(b(3)*c(1) - b(1)*c(3)) + 
     *      a(3)*(b(1)*c(2) - b(2)*c(1))  

      triple_dot = res

      return
      end
c***********************************************************************
c
c     SUBROUTINE: intrpt (gpt1,gpt2,gpto,grat)
c
c     FUNCTION:  This routine interpolates point at given ratio
c                between two points.
c
c     INPUT:   gpt1     R*8  D3  - First point.
c
c              gpt2     R*8  R3  - Second point.
c
c              grat     R*8  D1  - Ratio factor.
c
c     OUTPUT:  gpto     R*8  D3  - Output point coordinates.
c
c******************************************************************
c
      subroutine intrpt (gpt1,gpt2,gpto,grat)
c
      real*8 gpt1(3),gpt2(3),gpto(3),grat
c
      gpto(1) = gpt1(1) + grat * (gpt2(1) - gpt1(1))
      gpto(2) = gpt1(2) + grat * (gpt2(2) - gpt1(2))
      gpto(3) = gpt1(3) + grat * (gpt2(3) - gpt1(3))
c
      return
      end

c***********************************************************************
c
c     SUBROUTINE: intrvec (gpt1,gpt2,gpto,grat)
c
c     FUNCTION:  This routine interpolates vector at given ratio
c                between two vectors.
c
c     INPUT:   gpt1     R*8  D3  - First point.
c
c              gpt2     R*8  R3  - Second point.
c
c              grat     R*8  D1  - Ratio factor.
c
c     OUTPUT:  gpto     R*8  D3  - Output point coordinates.
c******************************************************************
c
      subroutine intrvec (gpt1,gpt2,gpto,grat)
c
      real*8 gpt1(3),gpt2(3),gpto(3),grat
c
      real*8 an1,an2,dlta,fi,thet,tvmp(4)
      integer*4 nfl
c
      call getvan (gpt1,gpt2,an1,an2,dlta,fi,thet,tvmp,nfl)
      if (dlta .gt. .1) then
         an2    = an1 + grat * dlta
         call setijk (an2,gpto,fi,thet)
      else
         gpto(1) = gpt1(1)
         gpto(2) = gpt1(2)
         gpto(3) = gpt1(3)
      end if
c
      return
      end

c***********************************************************************
c
c     SUBROUTINE: lnlncross(p1,n1,p2,n2,p)
c
c     FUNCTION:  finds intersection point of 2 lines defined as point-vector;
c                The lines are assumed to be co-planar and not ||.
c
c     INPUT:   
c                p1,n1    R*8  D3  -  point/vector of the 1st line.
c                p2,n2    R*8  D3  -  point/vector of the 2nd line.
c
c
c     OUTPUT:  
c                p        R*8  D3  - intersection point.
c     NOTE:  if lines are ||, p={0,0,0} is returned
c******************************************************************
c
      subroutine lnlncross(p1,n1,p2,n2,p)
      
      real*8 p1(3),n1(3),p2(3),n2(3),p(3),v(3),
     *       f_mag,f_dot,t,d,vn2,det,one/1.d0/,zero/0.d0/

      call vcmnvc(p2,p1,v)
      d = f_mag(v)

      if(d.eq.zero) then
         call vctovc(p1,p)
         return
      endif

      call unitizevc(v)
      vn2 = f_dot(v,n2)
      det = f_dot(v,n1) - f_dot(n1,n2)*vn2

      if(det.eq.zero) then
         call fill_array(p,3,zero)
         return
      endif

      t = d*(one - vn2*vn2)/det
      call uvcplvc(p1,n1,p,t)

      return
      end
c***********************************************************************
c
c   SUBROUTINE: xyzvc(x,y,z,vec)
c
c   FUNCTION: constructs a 3D vector from x,y,z components
c
c   INPUT:
c          x      R*8  D1  -   x component
c          y      R*8  D1  -   y component
c          z      R*8  D1  -   z component
c
c   OUTPUT:
c          vec    R*8  D3  -   vector
c
c***********************************************************************
      subroutine xyzvc(x,y,z,vec)
      real*8 x,y,z,vec(3)

      vec(1) = x
      vec(2) = y
      vec(3) = z

      return
      end

c***********************************************************************
c
c   FUNCTION: max2_r8(x,y)
c
c   FUNCTION: gives max(x,y)
c
c   INPUT:
c          x      R*8  D1  - a number 
c          y      R*8  D1  - a number
c
c   OUTPUT:
c          returns max(x,y)
c
c***********************************************************************
      function max2_r8(x,y)
      real*8 x,y,max2_r8
      
      if (x.ge.y) then
         max2_r8 = x
      else
         max2_r8 = y
      endif
    
      return
      end
c***********************************************************************
c
c   FUNCTION: min2_r8(x,y)
c
c   FUNCTION: gives min(x,y)
c
c   INPUT:
c          x      R*8  D1  - a number 
c          y      R*8  D1  - a number
c
c   OUTPUT:
c          returns min(x,y)
c
c***********************************************************************
      function min2_r8(x,y)
      real*8 x,y,min2_r8
      
      if (x.ge.y) then
         min2_r8 = y
      else
         min2_r8 = x
      endif
    
      return
      end
c***********************************************************************
c
c   FUNCTION: min3_r8(x,y,z)
c
c   FUNCTION: gives min(x,y,z)
c
c   INPUT:
c          x      R*8  D1  - a number 
c          y      R*8  D1  - a number
c          z      R*8  D1  - a number
c
c   OUTPUT:
c          returns min(x,y,z)
c
c***********************************************************************
      function min3_r8(x,y,z)
      real*8 x,y,z,min3_r8,min2_r8
      
      min3_r8 = min2_r8 (x, min2_r8 (y,z))
    
      return
      end
c***********************************************************************
c
c   FUNCTION: max3_r8(x,y,z)
c
c   FUNCTION: gives max(x,y,z)
c
c   INPUT:
c          x      R*8  D1  - a number 
c          y      R*8  D1  - a number
c          z      R*8  D1  - a number
c
c   OUTPUT:
c          returns max(x,y,z)
c
c***********************************************************************
      function max3_r8(x,y,z)
      real*8 x,y,z,max3_r8,max2_r8
      
      max3_r8 = max2_r8 (x, max2_r8 (y,z))
    
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  perpvc(a, b)
c
c   FUNCTION: calculates a vector perpto another vector
c
c   INPUT:  a    R*8  D3    - 1st vector
c
c   OUTPUT: b    R*8  D3   -  Resultant perpendicular vector
c
c***********************************************************************
c
      subroutine perpvc(a, b)
c
      real*8 a(3), b(3)

      if (dabs(a(1)) .lt. .00001) then
          b(1) = 0.
          v1 = a(2)
          b(2) = a(3) * (-1.)
          b(3) = v1
      else
          v1 = a(1)
          b(1) = a(2) * (-1.)
          b(2) = v1
          b(3) = 0.
      endif
c
      return
      end
