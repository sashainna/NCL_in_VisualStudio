c***********************************************************************
c*
c*   FILE NAME:  flmath.f
c*   CONTAINS:
c*                       dpont2  pgtlan
C*   COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*   MODULE NAME AND RELEASE LEVEL
C*      flmath.f , 25.1
C*   DATE AND TIME OF LAST  MODIFICATION
C*      04/29/15 , 15:10:04
c*
c***********************************************************************
c*********************************************************************
c
c  SUBROUTINE:  dpont2 (dnum,acy)
c
c  FUNCTION:  This routine truncates a REAL*8 number 'acy' digits to
c             the right of the decimal point.
c
c     INPUT:  dnum    R*8  D1  -  Number to truncate.
c
c             acy     I*2  D1  -  Number of digits to right of decimal
c                                 point.
c
c     OUTPUT: dnum    R*8  D1  -  Truncated number.
c
c*********************************************************************
c
      SUBROUTINE dpont2 (DNUM,ACY)
C
      REAL*8 DNUM
C
      INTEGER*2 ACY
C
      REAL*8 FCT
C
C...ROUND OFF NUMBER
C
      FCT = 0.5D0
      IF (DNUM.LT.0.0D0) FCT = -0.5D0
      DNUM   = DINT(DNUM*10.0D0**ACY+FCT) / 10.0D0**ACY
C
      RETURN
      END
c
c***********************************************************************
c
c   SUBROUTINE:  pgtlan (gtv1,gtv2,gan1,gan2,gdlt,gfi,gtheta,gtvmp)
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
      subroutine pgtlan (gtv1,gtv2,gan1,gan2,gdlt,gfi,gtheta,gtvmp)
c
      include 'com8a.com'
      include 'fillet.com'
c
      real*8 gtv1(3),gtv2(3),gan1,gan2,gdlt,gfi,gtheta,gtvmp(3),
     -       p,vec,pln(3),alp,tvlin(3),bet,tl(3)
c
      integer*4 j1,j3
c
      data j1/1/, j3/3/
c
      
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
      iflx = 0
      if (p .gt. .9999999999d0 .or. vec .lt. 1.d-10) iflx = 1
c
c...check if vectors are parallel
c
      if (iflx .eq. 0) go to 400
c
c...parallel - use vertical plane
c
      pln(3) = 0.
      pln(1) = gtv1(2)
      pln(2) = - gtv1(1)
      vec    = dsqrt (pln(1)**2 + pln(2)**2)
      if (vec .lt. 1.d-12) then
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
      if (p .gt. 1.d-12) then
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
      call vecang (tvlin,j3,gtheta)
      alp    = 360. - gtheta
      call vecadj (pln,tl,alp,j3)
      call vecang (tl,j1,gfi)
      bet    = 360. - gfi
c
c...get start angle (1-st vector)
c
      call vecadj (gtv1,tl,alp,j3)
      call vecadj (tl,tl,bet,j1)
      call vecang (tl,j3,gan1)
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
