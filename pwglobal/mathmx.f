
c***********************************************************************
c
c   FILE NAME:  mathmx
c   CONTAINS:
c               determ  gtpola  matpta  matptr  matpvv  mxident  mxinv
c               mxirot  mxtmmx  mxtomx  mxvrot  mxerot  ptmatb  ptmatr
c               rotimx  rotemx  rotvmx  mataxs  isidmx
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mathmx.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        05/18/15 , 10:16:39
c
c***********************************************************************
c
c*********************************************************************
c*    E_SUBROUTINE     : subroutine determ (gmat,kol,gval)
c*       calculates value of determinant generated from the 3 X 4
c*       matrix.
c*    PARAMETERS
c*       INPUT  :  gmat  R*8  D4,3 - input matrix
c*                 kol   I*2  D1   - column selector: 0 = 1,2,3
c*                                   1 = 4,2,3; 2 = 1,4,3; 3 = 1,2,4.
c*
c*       OUTPUT :  gval  R*8  D1   - determinant value.
c*
c*    RETURNS      : none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine determ (gmat,kol,gval)
c
      real*8 gmat(4,3),gval
      integer*4 kol
c
      integer*4 is(3)
c
      is(1) = 1
      is(2) = 2
      is(3) = 3
      if (kol .ne. 0) is(kol) = 4
c
      gval  = gmat(is(1),1)*gmat(is(2),2)*gmat(is(3),3) +
     -        gmat(is(2),1)*gmat(is(3),2)*gmat(is(1),3) +
     -        gmat(is(3),1)*gmat(is(1),2)*gmat(is(2),3) -
     -        gmat(is(3),1)*gmat(is(2),2)*gmat(is(1),3) -
     -        gmat(is(2),1)*gmat(is(1),2)*gmat(is(3),3) -
     -        gmat(is(1),1)*gmat(is(3),2)*gmat(is(2),3)
      gval  = -gval
c
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
c
      real*8 gvecz(3),gang(2),gmatr(4,3)
c
      include 'post.inc'
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
      real*8 PI,RAD
c
      real*8 vc(3),tab,piv,c1,c2,s1,s2
c
c...Convert Z vector to spheric coordinates
c
      vc(1)  = gvecz(1)
      vc(2)  = gvecz(2)
      vc(3)  = gvecz(3)
      call vecang (vc,3,tab)
      piv = dacos (vc(3)) * RAD
      gang(1) = tab
      gang(2) = piv
c
c...Get convertion matrix
c
      c1      = dcos (piv/RAD)
      s1      = dsin (piv/RAD)
      c2      = dcos (tab/RAD)
      s2      = dsin (tab/RAD)
      gmatr(1,1) = c1 * c2
      gmatr(2,1) = 0.d0 - s2
      gmatr(3,1) = s1 * c2
      gmatr(1,2) = s2 * c1
      gmatr(2,2) = c2
      gmatr(3,2) = s1 * s2
      gmatr(1,3) = 0.d0 - s1
      gmatr(2,3) = 0.d0
      gmatr(3,3) = c1
      gmatr(4,1) = 0.d0
      gmatr(4,2) = 0.d0
      gmatr(4,3) = 0.d0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  matpta (gpti,gpto,gmat,ktyp)
c
c   FUNCTION:  This routine converts point/vector coordinates from
c              system I to system II using convertion matrix with a
c              scale factor.
c
c   INPUT:  gpti    R*8  D3    -  Input coordinates.
c
c           gmat    R*8  D4.3  -  Conversion matrix.
c
c           ktyp    I*4  D1    -  Input entity type: 1 - point (adjust
c                                 coordinates for displacement),
c                                 2 - vector (displacement not used),
c                                 3 - unit vector to unit vector.
c
c   OUTPUT: gpto    R*8  D3    -  Output coordinates.
c
c***********************************************************************
c
      subroutine matpta (gpti,gpto,gmat,ktyp)
c
      real*8 gpti(3),gpto(3),gmat(4,3),vec(3),rnum,scl,dd
c
      integer*4 ktyp,i
c
      rnum   = 0.0
      do 110 i=1,3
          if (ktyp .eq. 1) then
              scl = gmat(1,i)**2 + gmat(2,i)**2 + gmat(3,i)**2
              rnum = gmat(4,i) / dsqrt(scl)
          end if
          vec(i) = gpti(i) - rnum
  110 continue
c
      dd  = 0.d0
      do 320 i=1,3
          gpto(i) = vec(1)*gmat(i,1) + vec(2)*gmat(i,2) +
     -              vec(3)*gmat(i,3)
          dd    = gpto(i)**2 + dd
  320 continue
      if (ktyp .eq. 3) then
          dd    = dsqrt(dd)
          gpto(1) = gpto(1) / dd
          gpto(2) = gpto(2) / dd
          gpto(3) = gpto(3) / dd
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  matptr (gpti,gpto,gmat,ktyp)
c
c   FUNCTION:  This routine converts point/vector coordinates back from
c              system II to system I using convertion matrix with a scale
c              factor.  The conversion matrix is the same created for
c              conversion I -> II.
c
c   INPUT:  gpti    R*8  D3    -  Input coordinates.
c
c           gmat    R*8  D4.3  -  Conversion matrix.
c
c           ktyp    I*4  D1    -  Input entity type: 1 - point (adjust
c                                 coordinates for displacement),
c                                 2 - vector (displacement not used).
c
c   OUTPUT: gpto    R*8  D3    -  Output coordinates.
c
c***********************************************************************
c
      subroutine matptr (gpti,gpto,gmat,ktyp)
c
      integer*4 ktyp
c
      real*8 gpti(3),gpto(3),gmat(4,3),vec(3),scl
c
      integer*4 i
c
      vec(1) = gpti(1)
      vec(2) = gpti(2)
      vec(3) = gpti(3)
      do 320 i=1,3
          scl = gmat(1,i)**2 + gmat(2,i)**2 + gmat(3,i)**2
          gpto(i) = (vec(1)*gmat(1,i) + vec(2)*gmat(2,i) +
     -              vec(3)*gmat(3,i)) / scl
          if (ktyp .eq. 1) gpto(i) = gpto(i) + gmat(4,i)/dsqrt(scl)
  320 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  matpvv (kfl,gpt,gzvec,gxvec,gmat,kerr)
c
c   FUNCTION:  This routine calculates a matrix using an Origin, Z-axis,
c              and an optional X-axis.
c
c   INPUT:  kfl     I*4  D3  -  (1) = 1 = Use origin point (gpt) in
c                               matrix calculation (translation),
c                               (2) = 1 = Use Z-axis vector in matrix
c                               calculation (rotation).  (3) = 1 = Use
c                               X-axis in matrix calculation.
c
c           gpt     R*8  D3  -  Origin of new system defined in old system.
c                               Only used if 'kfl(1)' is set to 1.
c
c           gvecz   R*8  D3  -  Z-axis vector of new system defined in
c                               old system.  Only used if 'kfl(2)' is set
c                               to 1.
c
c           gvecx   R*8  D3  -  X-axis vector of new system defined in
c                               old system.  Only used if 'kfl(3)' is set
c                               to 1.  If not specified, then 'gzvec' will
c                               be rotated in the shortest manner to the
c                               absolute Z-axis and the X-axis will not
c                               be adjusted from this new rotated position.
c
c   OUTPUT: gmatr   R*8  D12 -  Calculated conversion matrix from old
c                               cartesian system to new cartesian system.
c
c           kerr    I*4  D1  -  1 = An error occured attemptint to calculate
c                               the matrix.
c
c***********************************************************************
c
      subroutine matpvv (kfl,gpt,gvecz,gvecx,gmatr,kerr)
c
      integer*4 kfl(3),kerr
c
      real*8 gpt(3),gvecz(3),gvecx(3),gmatr(4,3)
c
      integer*4 i
c
      real*8 vec1(3),vec2(3),zvec(3),rang(2),nmag,angl2p,mx1(4,3),
     1       mx2(4,3)
c
c...Initialize routine
c
      kerr   = 0
      zvec(1) = 0.
      zvec(2) = 0.
      zvec(3) = 1.
c
      call mxident (mx1)
c
c...Z-axis Rotation
c
      if (kfl(2) .eq. 1) then
          if (nmag(gvecz) .eq. 0.) go to 9000
          call unitvc (gvecz,vec1)
          call gtpola (vec1,rang,mx1)
      endif
c
c...X-axis Rotation
c
      if (kfl(3) .eq. 1) then
          if (nmag(gvecx) .eq. 0.) go to 9000
          call ptmatr (gvecx,vec1,mx1,2)
          vec1(3) = 0.
          call unitvc (vec1,vec1)
          vec2(1) = 1.
          vec2(2) = 0.
          vec2(3) = 0.
          rang(1) = angl2p (vec1,vec2,zvec)
          do 100 i=1,3,1
              call vecrot (mx1(1,i),mx1(1,i),zvec,rang(1))
  100     continue
      endif
c
c...Origin
c
      if (kfl(1) .eq. 1) then
          call mxident (mx2)
          mx2(4,1) = gpt(1)
          mx2(4,2) = gpt(2)
          mx2(4,3) = gpt(3)
          call mxtmmx (mx2,mx1,gmatr)
      else
          call mxtomx (mx1,gmatr)
      endif
c
c...End of routine
c
 8000 return
c
c...Could not calculate matrix
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mxident (gmatr)
c
c   FUNCTION:  This routine returns an identity matrix.
c
c   INPUT:  none.
c
c   OUTPUT: gmatr   R*8  D4.3 -  Identity matrix.
c
c***********************************************************************
c
      subroutine mxident (gmatr)
c
      real*8 gmatr(4,3)
c
c...Create identity matrix
c
      gmatr(1,1) = 1.
      gmatr(2,1) = 0.
      gmatr(3,1) = 0.
      gmatr(4,1) = 0.
      gmatr(1,2) = 0.
      gmatr(2,2) = 1.
      gmatr(3,2) = 0.
      gmatr(4,2) = 0.
      gmatr(1,3) = 0.
      gmatr(2,3) = 0.
      gmatr(3,3) = 1.
      gmatr(4,3) = 0.
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mxinv (gmati,gmato)
c
c   FUNCTION:  This routine returns returns the inverse of a matrix.
c
c   INPUT:  gmati   R*8  D4.3 -  Matrix to inverse.
c
c   OUTPUT: gmato   R*8  D4.3 -  Inversed matrix.
c
c           kerr    I*4  D1   -  1 - If an matrix cannot be inversed.
c
c***********************************************************************
c
      subroutine mxinv (gmati,gmato,kerr)
c
      integer*4 kerr
c
      real*8 gmati(4,3),gmato(4,3)
c
      integer*4 i
c
      real*8 sm,sx,sy,sz
c
      data sm /1.d-8/
c
c...Check for valid matrix
c
      kerr  = 0
      sx = gmati(1,1)**2 + gmati(1,2)**2 + gmati(1,3)**2
      sy = gmati(2,1)**2 + gmati(2,2)**2 + gmati(2,3)**2
      sz = gmati(3,1)**2 + gmati(3,2)**2 + gmati(3,3)**2
      if (sx .lt. sm .or. sy .lt. sm .or. sz .lt. sm) go to 9000
c
c...Inverse rotation components
c
      do 100 i=1,3,1
          gmato(i,1) = gmati(1,i) / sx
          gmato(i,2) = gmati(2,i) / sy
          gmato(i,3) = gmati(3,i) / sz
  100 continue
c
c...Inverse translations
c
      do 200 i=1,3,1
          gmato(4,i) = -gmato(1,i)*gmati(4,1) - gmato(2,i)*gmati(4,2) -
     1                 gmato(3,i)*gmati(4,3)
  200 continue
c
c...End of routine
c
 8000 return
c
c...Could not inverse matrix
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mxirot (gmat,grot,gtrans,kerr)
c
c   FUNCTION:  This routine calculates the required XYZ rotations and
c              translations to satisfy the matrix.  The order of the
c              rotations and translation are calculated as follows depending
c              on the value of 'kdir'.
c
c                  1. Translation
c                  2. Z(2) or X(3)-rotation
c                  3. Y-rotation
c                  4. X(2) or Z(3)-rotation
c
c              Unless the rotation is only about the Y-axis, the rotations
c              will always be calculated using the X & Y axes.  The angle
c              rotations will be between -180 and 180 degrees.
c
c   INPUT:  gmat    R*8  D3.4 -  Input matrix.
c
c           kdir    I*4  D1   -  1 = Calculate ZYX rotations, 2 = XYZ rotations.
c
c   OUTPUT: grot    R*8  D3   -  Rotations about the XYZ axes.
c
c           gtrans  R*3  D3   -  The translation in XYZ.
c
c           kerr    I*4  D1   -  1 = The matrix does not create a square
c                                coordinate system.
c
c***********************************************************************
c
      subroutine mxirot (gmat,grot,gtrans,kdir,kerr)
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD,rdvec
c
      integer*4 kerr,kdir
c
      real*8 gmat(4,3),grot(3),gtrans(3)
c
      integer*4 i,ix1,ix3
c
      real*8 vec1(3),vec2(3),xvec(3),yvec(3),zvec(3),rnum,nmag,angl2p
c
      data xvec /1.,0.,0./, yvec /0.,1.,0./, zvec /0.,0.,1./
c
c...Initialize routine
c
      kerr   = 0
      if (kdir .eq. 1) then
          ix1    = 1
          ix3    = 3
      else
          ix1    = 3
          ix3    = 1
      endif
      grot(1) = 0.
      grot(2) = 0.
      grot(3) = 0.
c
c...Determine translation of matrix
c
      gtrans(1) = gmat(4,1)
      gtrans(2) = gmat(4,2)
      gtrans(3) = gmat(4,3)
c
c...Determine rotation of matrix
c
      if (kdir .eq. 1) then
          call matpta (zvec,vec1,gmat,3)
      else
          call matpta (xvec,vec1,gmat,3)
      endif
      rnum   = vec1(ix3)
      call dpoint (rnum,rnum,5)
c
c...Z-only Rotation
c
      if (rnum .eq. 1.) then
          if (kdir .eq. 1) then
              call matpta (xvec,vec1,gmat,3)
              call vecang (vec1,3,grot(3))
          else
              call matpta (zvec,vec1,gmat,3)
              call vecang (vec1,1,grot(1))
          endif
c
c...X/Z & Y Rotation
c......X-axis first (kdir = 1)
c......Z-axis first (kdir = 2)
c
      else
          vec2(1) = vec1(1)
          vec2(2) = vec1(2)
          vec2(3) = vec1(3)
          vec2(ix1) = 0.
          call unitvc (vec2,vec2)
          if (nmag(vec2) .gt. 0.) then
              grot(ix1) = dasin(vec2(2)) * RAD
              rdvec = dnint(vec2(ix3)*10.0d0**14) / 10.0d0**14
              if (rdvec .lt. 0.) grot(ix1) = 360.d0 - grot(ix1)
c
              if (kdir .eq. 1) then
                  grot(ix1) = 360.d0 - grot(ix1)
              endif
c
              call dpoint (grot(ix1),rnum,3)
              rnum = dabs(rnum)
              if (rnum .eq. 0. .or. rnum .eq. 180. .or. rnum .eq. 360.)
     1            grot(ix1) = 0.
              if (grot(ix1) .ne. 0.) then
                  if (kdir .eq. 1) then
                      call vecrot (vec1,vec2,xvec,-grot(ix1))
                  else
                      call vecrot (vec1,vec2,zvec,-grot(ix1))
                  endif
              else
                  vec2(1) = vec1(1)
                  vec2(2) = vec1(2)
                  vec2(3) = vec1(3)
              endif
          else
              vec2(1) = vec1(1)
              vec2(2) = vec1(2)
              vec2(3) = vec1(3)
          endif
c
c......Then Y-axis
c
          if (nmag(vec2) .gt. 0.) then
              if (kdir .eq. 2) then
                rnum = vec2(1)
                vec2(1) = -vec2(3)
                vec2(3) = rnum
              endif
              call vecang (vec2,2,grot(2))
              vec1(1) = gmat(1,ix1)
              vec1(2) = gmat(2,ix1)
              vec1(3) = gmat(3,ix1)
c
c......Then Z-axis
c
              if (kdir .eq. 1) then
                  call vecrot (vec1,vec1,xvec,-grot(1))
              else
                  call vecrot (vec1,vec1,zvec,-grot(3))
              endif
              call vecrot (vec1,vec1,yvec,-grot(2))
              call unitvc (vec1,vec1)
              call vecang (vec1,ix3,grot(ix3))
          endif
      endif
c
c...End of routine
c
 8000 do 500 i=1,3,1
          if (grot(i) .gt. 180.) grot(i) = grot(i) - 360.
          if (grot(i) .lt. -180.) grot(i) = grot(i) + 360
  500 continue
      return
c
c...Invalid matrix
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mxtmmx (gmat1,gmat2,gmatr)
c
c   FUNCTION:  This routine multiplies a matrix by another matrix.
c
c   INPUT:  gmat1   R*8  D4.3 - First matrix to multiply.
c
c           gmat2   R*8  D4.3 - Second matrix to multiply.
c
c   OUTPUT: gmatr   R*8  D4.3 -  Resultant matrix.
c
c***********************************************************************
c
      subroutine mxtmmx (gmat1,gmat2,gmatr)
c
      real*8 gmat1(4,3),gmat2(4,3),gmatr(4,3)
c
      integer*4 i,j
c
c...Multiply the two matrices
c
      do 200 i=1,3,1
          do 100 j=1,4,1
              gmatr(j,i) = gmat1(1,i)*gmat2(j,1) + gmat1(2,i)*gmat2(j,2)
     1                     + gmat1(3,i)*gmat2(j,3)
  100     continue
  200 continue
c
c...Adjust origin
c
      gmatr(4,1) = gmatr(4,1) + gmat1(4,1)
      gmatr(4,2) = gmatr(4,2) + gmat1(4,2)
      gmatr(4,3) = gmatr(4,3) + gmat1(4,3)
c
c...End of routine
c
 8000 return
      end
c
c
c***********************************************************************
c
c   SUBROUTINE:  mxtomx (gmat1,gmatr)
c
c   FUNCTION:  This routine copies a matrix into another matrix.
c
c   INPUT:  gmat1   R*8  D4.3 - Matrix to copy.
c
c   OUTPUT: gmatr   R*8  D4.3 -  Resultant matrix.
c
c***********************************************************************
c
      subroutine mxtomx (gmat1,gmatr)
c
      real*8 gmat1(4,3),gmatr(4,3)
c
      integer*4 i,j
c
c...Multiply the two matrices
c
      do 200 i=1,3,1
          do 100 j=1,4,1
              gmatr(j,i) = gmat1(j,i)
  100     continue
  200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mxvrot (gmat,grot,gtrans,kerr)
c
c   FUNCTION:  This routine calculates a vector of rotation, the rotation
c              about this vector, and translations to satisfy the matrix.
c              The order of the rotation and translation is calculated as
c              follows.
c
c                  1. Translation
c                  2. Rotation
c
c   INPUT:  gmat    R*8  D3.4 -  Input matrix.
c
c   OUTPUT: gvec    R*8  D1   -  Rotation vector.
c
c           grot    R*8  D1   -  Rotation about the vector.
c
c           gtrans  R*3  D3   -  The translation in XYZ.
c
c           kerr    I*4  D1   -  1 = The matrix does not create a square
c                                coordinate system.
c
c***********************************************************************
c
      subroutine mxvrot (gmat,gvec,grot,gtrans,kerr)
c
      include 'post.inc'
c
      equivalence (PI,POSMAP(0001)), (RAD,POSMAP(0002))
c
      real*8 PI,RAD
c
      integer*4 kerr
c
      real*8 gmat(4,3),gvec(3),grot,gtrans(3)
c
      real*8 vec1(3),vec2(3),xvec(3),zvec(3),ang1,ang2,rfi,thet,rnum
c
      real*8 trc,co,g11,g22,g33
c
c...Initialize routine
c
      kerr   = 0
c
c...Determine translation of matrix
c
      gtrans(1) = gmat(4,1)
      gtrans(2) = gmat(4,2)
      gtrans(3) = gmat(4,3)
c
c...Determine plane of rotation and angles
c
      g11 = gmat(1,1)
      g22 = gmat(2,2)
      g33 = gmat(3,3)
      trc = g11 + g22 + g33
      co = (trc - 1.)/2
      if (co .gt. 0.999999) then
        grot = 0
        gvec(1) = 0
        gvec(2) = 0
        gvec(3) = 1
      else if (co .lt. -0.999999) then
        grot = 180
        if (g11.ge.g22 .and. g11.ge.g33) then
          rnum = dsqrt(g11 - g22 - g33 + 1)
          gvec(1) = rnum / 2
          gvec(2) = gmat(1,2)/rnum
          gvec(3) = gmat(1,3)/rnum
        else if (g22.ge.g11 .and. g22.ge.g33) then
          rnum = dsqrt(g22 - g11 - g33 + 1)
          gvec(1) = gmat(1,2)/rnum
          gvec(2) = rnum / 2
          gvec(3) = gmat(3,2)/rnum
        else
          rnum = dsqrt(g33 - g11 - g22 + 1)
          gvec(1) = gmat(1,3)/rnum
          gvec(2) = gmat(2,3)/rnum
          gvec(3) = rnum / 2
        endif
      else
        grot = dacos (co) * RAD
        vec1(1) = gmat(3,2) - gmat(2,3)
        vec1(2) = gmat(1,3) - gmat(3,1)
        vec1(3) = gmat(2,1) - gmat(1,2)
        call unitvc (vec1,gvec)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mxerot (gmat,grot,gtrans,kdir,kerr)
c
c   FUNCTION:  This routine calculates the EULER angles and translations
c              to satisfy the matrix.  The order of the rotation and
c              translation is calculated as follows.
c
c                  1. Translation
c                  2. Rotation
c
c   INPUT:  gmat    R*8  D3.4 -  Input matrix.
c
c           kdir    I*4  D1   - 1 = Calculate rotations from Matrix
c                               Coordsys to Standard Coordsys.
c                               2 = Calculate rotations from Standard
c                               Coordsys to Matrix Coordsys.
c
c   OUTPUT: grot    R*8  D3   -  Alpha(z), Beta(x), Gamma(z) rotations.
c
c           gtrans  R*3  D3   -  The translation in XYZ.
c
c           kerr    I*4  D1   -  1 = The matrix does not create a square
c                                coordinate system.
c
c***********************************************************************
c
      subroutine mxerot (gmat,grot,gtrans,kdir,kerr)
c
      include 'post.inc'
c
      equivalence (PI,POSMAP(0001)), (RAD,POSMAP(0002))
c
      real*8 PI,RAD
c
      integer*4 kdir,kerr
c
      real*8 gmat(4,3),grot(3),gtrans(3)
c
      integer*4 i
      integer*4 isparl
      logical mxeqmx
c
      real*8 xvec(3),yvec(3),zvec(3),xvec1(3),yvec1(3),zvec1(3),
     1       xvecn(3),yvecn(3),zvecn(3),d,xmat(4,3)
      real*8 angl2p,ndist
c
      data xvecn /1.,0.,0./, yvecn /0.,1.,0./, zvecn /0.,0.,1./
c
      integer*4 DEBUGX
      parameter (DEBUGX=0)
c
c...Initialize routine
c
      kerr   = 0
      grot(1) = 0.
      grot(2) = 0.
      grot(3) = 0.
c
c...Determine translation of matrix
c
      gtrans(1) = gmat(4,1)
      gtrans(2) = gmat(4,2)
      gtrans(3) = gmat(4,3)
c
c...From Matrix CS to Standard CS
c
      if (kdir .eq. 1) then
          call copyn (xvecn,xvec1,3)
          call copyn (yvecn,yvec1,3)
          call copyn (zvecn,zvec1,3)
          call matpta (xvecn,xvec,gmat,3)
          call matpta (yvecn,yvec,gmat,3)
          call matpta (zvecn,zvec,gmat,3)
c
c...From Standard CS to Matrix CS
c
      else
          call copyn (xvecn,xvec,3)
          call copyn (yvecn,yvec,3)
          call copyn (zvecn,zvec,3)
          call matpta (xvecn,xvec1,gmat,3)
          call matpta (yvecn,yvec1,gmat,3)
          call matpta (zvecn,zvec1,gmat,3)
      endif
c
      if (DEBUGX .eq. 1) then
          call dbgeul (xvec,yvec,zvec,'RED',1)
          call dbgeul (xvec1,yvec1,zvec1,'GREEN',2)
      endif
c
c...Determine Node vector
c...(vector at Intersection of identity and new XYZ planes
c
      ifl    = isparl(zvec1,zvec)
      if (ifl .eq. 0) then
          call crosvc(zvec1,zvec,nvec)
          call unitvc(nvec,nvec)
      else if (ifl .eq. -1) then
          call vctmsc (xvec,-1.d0,nvec)
      endif
c
c...Get Alpha angle
c
      grot(1) = angl2p (xvec,nvec,zvec)
c
c...Adjust coordinate system for Alpha rotation
c
      call vecrot (xvec,xvec,zvec,grot(1))
      call vecrot (yvec,yvec,zvec,grot(1))
      if (DEBUGX .eq. 1) call dbgeul (xvec,yvec,zvec,'ORANGE',0)
c
c...Get Beta rotation
c
      grot(2) = angl2p (zvec,zvec1,xvec)
c
c...Adjust coordinate system for Beta rotation
c
      call vecrot (yvec,yvec,xvec,grot(2))
      call vecrot (zvec,zvec,xvec,grot(2))
      if (DEBUGX .eq. 1) call dbgeul (xvec,yvec,zvec,'MAGNTA',0)
c
c...Get Gamma rotation
c
      grot(3) = angl2p (xvec,xvec1,zvec)
c
c...Adjust coordinate system for Gamma rotation
c
      call vecrot (xvec,xvec,zvec,grot(3))
      call vecrot (yvec,yvec,zvec,grot(3))
      if (DEBUGX .eq. 1) call dbgeul (xvec,yvec,zvec,'CYAN',0)
c
c...Make sure rotations are correct
c
      d = ndist(xvec,xvec1)
      if (d .gt. .0001) go to 9000
      d = ndist(yvec,yvec1)
      if (d .gt. .0001) go to 9000
      d = ndist(zvec,zvec1)
      if (d .gt. .0001) go to 9000
c
c...Check Rotations to Matrix routines
c
      if (DEBUGX .eq. 2) then
          call rotemx (gtrans,grot,xmat,kdir)
          if (.not.mxeqmx(xmat,gmat)) kerr = 1
      endif
c
c...End of routine
c
 8000 do 500 i=1,3,1
          if (grot(i) .gt. 180.) grot(i) = grot(i) - 360.
          if (grot(i) .lt. -180.) grot(i) = grot(i) + 360
  500 continue
      return
c
c...Invalid matrix
c
 9000 kerr   = 1
      go to 8000
      end
c
      subroutine dbgeul (xvec,yvec,zvec,color,first)
c
      character*(*) color
c
      real*8 xvec(3),yvec(3),zvec(3)
c
      integer*4 strlen1,nc,ierr,first
c
      real*8 pt(3)
c
      character*80 tbuf,msg
c
      if (first .eq. 1) then
          call lstout ('$$',2,msg,ierr)
          tbuf = 'ERASE/ALL'
          nc = strlen1(tbuf)
          call lstout (tbuf,nc,msg,ierr)
          tbuf = 'DRAFT/ANOTE,STYLE=STRING'
          nc = strlen1(tbuf)
          call lstout (tbuf,nc,msg,ierr)
      endif
c
      write (tbuf,7000) color
 7000 format ('DRAFT/MODIFY,COLOR="',a'"')
      nc = strlen1(tbuf)
      call lstout (tbuf,nc,msg,ierr)
c
      write (tbuf,7001) color
 7001 format ('DRAFT/ANOTE,COLOR="',a'"')
      nc = strlen1(tbuf)
      call lstout (tbuf,nc,msg,ierr)
c
      if (first .ne. 0) then
          write (tbuf,6000) zvec(1),zvec(2),zvec(3)
 6000     format ('PLANE/',f10.6,',',f10.6,',',f10.6,',0')
          nc = strlen1(tbuf)
          call lstout (tbuf,nc,msg,ierr)
      endif
c
      write (tbuf,7002) xvec(1),xvec(2),xvec(3)
 7002 format ('VECTOR/',f10.6,',',f10.6,',',f10.6)
      nc = strlen1(tbuf)
      call lstout (tbuf,nc,msg,ierr)
      pt(1) = xvec(1) * 1.1
      pt(2) = xvec(2) * 1.1
      pt(3) = xvec(3) * 1.1
      write (tbuf,7003) 'X',pt(1),pt(2),pt(3)
 7003 format ('ANOTE/',A1,',AT',',(PT/',f10.6,',',f10.6,',',f10.6,')')
      nc = strlen1(tbuf)
      call lstout (tbuf,nc,msg,ierr)
c
      write (tbuf,7002) yvec(1),yvec(2),yvec(3)
      nc = strlen1(tbuf)
      call lstout (tbuf,nc,msg,ierr)
      pt(1) = yvec(1) * 1.1
      pt(2) = yvec(2) * 1.1
      pt(3) = yvec(3) * 1.1
      write (tbuf,7003) 'Y',pt(1),pt(2),pt(3)
      nc = strlen1(tbuf)
      call lstout (tbuf,nc,msg,ierr)
c
      write (tbuf,7002) zvec(1),zvec(2),zvec(3)
      nc = strlen1(tbuf)
      call lstout (tbuf,nc,msg,ierr)
      pt(1) = zvec(1) * 1.1
      pt(2) = zvec(2) * 1.1
      pt(3) = zvec(3) * 1.1
      write (tbuf,7003) 'Z',pt(1),pt(2),pt(3)
      nc = strlen1(tbuf)
      call lstout (tbuf,nc,msg,ierr)
c
      call lstout ('*STOP',5,msg,ierr)
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
c           ktyp    I*4  D1    -  Input entity type: 1 - point (adjust
c                                 coordinates for displacement),
c                                 2 - vector (displacement not used).
c
c   OUTPUT: gpto    R*8  D3    -  Output coordinates.
c
c***********************************************************************
c
      subroutine ptmatb (gpti,gpto,gmat,ktyp)
c
      integer*4 ktyp
c
      real*8 gpti(3),gpto(3),gmat(4,3),vec(3)
c
      integer*4 i
c
      vec(1) = gpti(1)
      vec(2) = gpti(2)
      vec(3) = gpti(3)
      do 320 i=1,3
          gpto(i) = vec(1)*gmat(1,i) + vec(2)*gmat(2,i) +
     -              vec(3)*gmat(3,i)
          if (ktyp .eq. 1) gpto(i) = gpto(i) + gmat(4,i)
  320 continue
c
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
c           ktyp    I*4  D1    -  Input entity type: 1 - point (adjust
c                                 coordinates for displacement),
c                                 2 - vector (displacement not used).
c
c   OUTPUT: gpto    R*8  D3    -  Output coordinates.
c
c***********************************************************************
c
      subroutine ptmatr (gpti,gpto,gmat,ktyp)
c
      real*8 gpti(3),gpto(3),gmat(4,3),vec(3),rnum
c
      integer*4 ktyp,i
c
      rnum   = 0.0
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
c   SUBROUTINE:  rotimx (gtrans,grot,gmat,kdir)
c
c   FUNCTION:  This routine calculates the matrix given translations and
c              rotations about the XYZ axes. The order of the
c              rotations and translation are calculated as follows depending
c              on the value of 'kdir'.
c
c                  1. X(2) or Z(3)-rotation
c                  2. Y-rotation
c                  3. Z(2) or X(3)-rotation
c                  4. Translation
c
c   INPUT:  gtrans  R*3  D3   -  The translation in XYZ.
c
c           grot    R*8  D3   -  Rotations about the XYZ axes.
c
c           kdir    I*4  D1   -  1 = Calculate ZYX rotations, 2 = XYZ rotations.
c
c   OUTPUT: gmat    R*8  D3.4 -  Calculated matrix.
c
c***********************************************************************
c
      subroutine rotimx (gtrans,grot,gmat,kdir)
c
c
      integer*4 kdir
c
      real*8 gmat(4,3),grot(3),gtrans(3)
c
      integer*4 i,j
c
      real*8 rvec(3,3)
c
      data rvec /1.,0.,0., 0.,1.,0., 0.,0.,1./
c
c...Initialize routine
c
      call mxident (gmat)
c
c...Solve for rotations
c
      if (kdir .eq. 1) then
          do 200 i=3,1,-1
              do 100 j=1,3,1
                  call vecrot (gmat(1,j),gmat(1,j),rvec(1,i),grot(i))
  100         continue
  200     continue
      else
          do 400 i=1,3,1
              do 300 j=1,3,1
                  call vecrot (gmat(1,j),gmat(1,j),rvec(1,i),grot(i))
  300         continue
  400     continue
      endif
c
c...Determine translation of matrix
c
      gmat(4,1) = gtrans(1)
      gmat(4,2) = gtrans(2)
      gmat(4,3) = gtrans(3)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotvmx (gtrans,gvec,grot,gmat)
c
c   FUNCTION:  This routine calculates the matrix given translations and
c              a rotation about a single vector.
c
c   INPUT:  gtrans  R*3  D3   -  The translation in XYZ.
c
c           gvec    R*8  D3   -  Vector to rotate about.
c
c           grot    R*8  D1   -  Rotation angle.
c
c   OUTPUT: gmat    R*8  D3.4 -  Calculated matrix.
c
c***********************************************************************
c
      subroutine rotvmx (gtrans,gvec,grot,gmat)
c
      real*8 gmat(4,3),gvec(3),grot,gtrans(3)
c
      integer*4 i
c
c...Initialize routine
c
      call mxident (gmat)
c
c...Solve for rotation
c
      do 200 i=1,3,1
          call vecrot (gmat(1,i),gmat(1,i),gvec,grot)
  200 continue
c
c...Determine translation of matrix
c
      gmat(4,1) = gtrans(1)
      gmat(4,2) = gtrans(2)
      gmat(4,3) = gtrans(3)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotemx (gtrans,grot,gmat,kdir)
c
c   FUNCTION:  This routine calculates the matrix given translations and
c              rotations about the XYZ axes.
c
c   INPUT:  gtrans  R*3  D3   -  The translation in XYZ.
c
c           grot    R*8  D3   -  Euler angle rotations.
c
c           kdir    I*4  D1   - 1 = Calculate rotations from Matrix
c                               Coordsys to Standard Coordsys.
c                               2 = Calculate rotations from Standard
c                               Coordsys to Matrix Coordsys.
c
c   OUTPUT: gmat    R*8  D3.4 -  Calculated matrix.
c
c***********************************************************************
c
      subroutine rotemx (gtrans,grot,gmat,kdir)
c
      integer*4 kdir
c
      real*8 gmat(4,3),grot(3),gtrans(3)
c
      integer*4 i,j
c
      real*8 rvec(3,3)
c
      data rvec /1.,0.,0., 0.,1.,0., 0.,0.,1./
c
c...Initialize routine
c
      call mxident (gmat)
c
c...Solve for rotations
c...From Matrix CS to Standard CS
c
      if (kdir .eq. 1) then
c
c......Determine Node vector
c
          call vecrot (gmat(1,1),gmat(1,1),rvec(1,3),-grot(3))
          call vecrot (gmat(1,2),gmat(1,2),rvec(1,3),-grot(3))
c
c......Determine Z-axis vector
c
          call vecrot (gmat(1,2),gmat(1,2),gmat(1,1),-grot(2))
          call vecrot (gmat(1,3),gmat(1,3),gmat(1,1),-grot(2))
c
c......Determine XY-axis vectors
c
          call vecrot (gmat(1,1),gmat(1,1),gmat(1,3),-grot(1))
          call vecrot (gmat(1,2),gmat(1,2),gmat(1,3),-grot(1))
c
c...From Standard CS to Matrix CS
c
      else
c
c......Determine Node vector
c
          call vecrot (gmat(1,1),gmat(1,1),rvec(1,3),grot(1))
          call vecrot (gmat(1,2),gmat(1,2),rvec(1,3),grot(1))
c
c......Determine Z-axis vector
c
          call vecrot (gmat(1,2),gmat(1,2),gmat(1,1),grot(2))
          call vecrot (gmat(1,3),gmat(1,3),gmat(1,1),grot(2))
c
c......Determine XY-axis vectors
c
          call vecrot (gmat(1,1),gmat(1,1),gmat(1,3),grot(3))
          call vecrot (gmat(1,2),gmat(1,2),gmat(1,3),grot(3))
      endif
c
c...Determine translation of matrix
c
      gmat(4,1) = gtrans(1)
      gmat(4,2) = gtrans(2)
      gmat(4,3) = gtrans(3)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mataxs (kfl,gpt,grot,kaxis,knrot,gmatr)
c
c   FUNCTION:  This routine calculates a matrix using a set of rotary
c              axes positions.
c
c   INPUT:  kfl     I*4  D1  -  1 = Use rotary axis angle as provided.
c                               2 = Use the reverse angle.
c
c           gpt     R*8  D3  -  Origin of new system defined in old system.
c
c           grot    R*8  D20 -  Current rotary axis positions used to
c                               calculate rotary positions.
c
c           kaxis   I*4  D20 -  Axis that each of the rotary axes rotates
c                               about.
c
c           knrot   I*4  D1  -  Number of active axes in 'grot'.
c
c   OUTPUT: gmatr   R*8  D12 -  Calculated conversion matrix from old
c                               cartesian system to new cartesian system.
c
c***********************************************************************
c
      subroutine mataxs (kfl,gpt,grot,kaxis,knrot,gmatr)
c
      include 'post.inc'
c
      equivalence (IXFMFL,KPOSMP(0989)), (LASTAB,KPOSMP(1260))
c
      integer*4 IXFMFL(5),LASTAB
c
      integer*4 kfl,kaxis(20),knrot
c
      real*8 gpt(3),grot(20),gmatr(4,3)
c
      integer*4 i
c
      real*8 mx1(4,3),ang,org(3)
c
c...Initialize routine
c
      org(1) = 0.
      org(2) = 0.
      org(3) = 0.
c
      call mxident (gmatr)
c
c...Calculate matrix for current rotary positions
c
      do 100 i=1,knrot,1
          ang    = grot(i)
          if (kfl .eq. 1) ang = -ang
          if (i .le. LASTAB .and. IXFMFL(1) .eq. 1) ang = -ang
          if (i .gt. LASTAB .and. IXFMFL(2) .eq. 1) ang = -ang
          call axadj (gmatr(1,1),gmatr(1,1),ang,org,kaxis(i))
          call axadj (gmatr(1,2),gmatr(1,2),ang,org,kaxis(i))
          call axadj (gmatr(1,3),gmatr(1,3),ang,org,kaxis(i))
  100 continue
c
c...Origin
c
      gmatr(4,1) = -gpt(1)
      gmatr(4,2) = -gpt(2)
      gmatr(4,3) = -gpt(3)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   FUNCTION:  isidmx (gmx)
c
c   FUNCTION:  This routine determines if a matrix is an identity matrix.
c              axes positions.
c
c   INPUT:  mx      R*8  D12 -  Matrix to test.
c
c   OUTPUT: isidmx  L*4  D1  -  True if matrix is an identity matrix.
c
c***********************************************************************
c
      logical function isidmx (gmx)
c
      real*8 gmx(12)
c
      integer*4 i
c
      real*8 toler
c
c...Initialize routine
c
      toler = 1.d-8
      isidmx = .true.
c
c...Determine if identity matrix
c
      do 100 i=1,12,1
          if (dabs(gmx(i)) .gt. toler) isidmx = .false.
  100 continue
c
      if (.not. isidmx) then
          if (dabs(1.d0-gmx(1)) .le. toler .and.
     1        dabs(gmx(4)) .le. toler .and.
     2        dabs(1.d0-gmx(6)) .le. toler .and.
     3        dabs(gmx(8)) .le. toler .and.
     4        dabs(1.d0-gmx(11)) .le. toler .and.
     5        dabs(gmx(12)) .le. toler) isidmx = .true.
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   FUNCTION:  mxeqmx (gmx1,gmx2)
c
c   FUNCTION:  This routine determines if a matrix is an identity matrix.
c              axes positions.
c
c   INPUT:  mx1     R*8  D12 -  1st Matrix to compare.
c
c           mx2     R*8  D12 -  2nd Matrix to compare.
c
c   OUTPUT: mxeqmx  L*4  D1  -  True if matrixes are identical.
c
c***********************************************************************
c
      logical function mxeqmx (gmx1,gmx2)
c
      real*8 gmx1(12),gmx2(12)
c
      integer*4 i
c
      real*8 toler
c
c...Initialize routine
c
      toler = 1.d-8
      mxeqmx = .true.
c
c...Determine if identity matrix
c
      do 100 i=1,12,1
          if (dabs(gmx1(i)-gmx2(i)) .gt. toler) mxeqmx = .false.
  100 continue
c
c...End of routine
c
 8000 return
      end
