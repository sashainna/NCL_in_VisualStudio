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
C*    MODULE NAME AND RELEASE LEVEL
C*       vector.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:53
c***********************************************************************
c
      subroutine vecang (gtv,kaxis,galph)
c
      real*8 RAD,PI
c
      integer*4 kaxis
c
      real*8 gtv(3),galph
c
      integer*4 is1,is2,inx(6)
c
      real*8 rvec,rdvec,rtv1,fct
c
      data inx /3,2,3,1,1,2/
c
      data PI /3.1415926535897932D0/
c
      RAD    = 180.0 / PI
      is1    = inx(kaxis*2-1)
      is2    = inx(kaxis*2)
c
c...Unitize components of rotary axis
c
      rvec   = dsqrt (gtv(is1)*gtv(is1) + gtv(is2)*gtv(is2))
      fct    = 0.5d0
      if (rvec.lt.0.0d0) fct = -0.5d0
      rdvec  = dint(rvec*10.0d0**14+fct) / 10.0d0**14
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
          fct   = 0.5d0
          if (gtv(is2).lt.0.0d0) fct = -0.5d0
          rdvec = dint(gtv(is2)*10.0d0**14+fct) / 10.0d0**14
          if (rdvec .lt. 0.) galph = 360.d0 - galph
          if (kaxis .eq. 1) galph = 360.d0 - galph
          if (galph .ge. 360.0) galph = galph - 360.0d0
      endif
c
      return
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
      real*8 RAD,PI
c
      integer*4 kaxis
c
      real*8 gtxyz(3),gtv(3),gaxis
c
      integer*4 is1,is2,inx(6)
c
      real*8 pt(3),ti,tk,angl
c
      data inx /3,2,3,1,1,2/
c
      data PI /3.1415926535897932D0/
c
      RAD    = 180.0 / PI
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
c***************************************************************
c
c   SUBROUTINE:  ptadjr (gtxyz,gtv,gaxis)
c
c   FUNCTION:  This routine reverses adjusted point from XY plan
c              back to the true coordinates.
c
c   INPUT:  gtxyz   R*8  D3  -  The XY plan point coordinates.
c           gaxis   R*8  D2  -  The angles (polar system) at which
c                               current system is adjusted.
c
c   OUTPUT: gtv     R*8  D3  -  The true point coordinates.
c
c***************************************************************
c
      subroutine ptadjr (gtxyz,gtv,gaxis)
c
      real*8 gtxyz(3),gtv(3),gaxis(2)
c
      integer*4 kaxis
      real*8 pt(3),vec,a,b
c
      gtv(1) = gtxyz(1)
      gtv(2) = gtxyz(2)
      gtv(3) = gtxyz(3)
      vec    = dsqrt (gtv(1)**2 + gtv(2)**2 + gtv(3)**2)
      if (vec .lt. 1.d-6) go to 800
c
c...Unitize point coordinates to 
c...adjust it like vector
c
      gtv(1) = gtv(1) / vec
      gtv(2) = gtv(2) / vec
      gtv(3) = gtv(3) / vec
c
      a     = gaxis(1)
      b     = gaxis(2)
c
c...adjust point as vector
c
      kaxis = 2
      call vecadj (gtv,gtv,a,kaxis)
      kaxis = 3
      call vecadj (gtv,gtv,b,kaxis)
c
c...recover output point
c
      gtv(1) = gtv(1) * vec
      gtv(2) = gtv(2) * vec
      gtv(3) = gtv(3) * vec
c
  800 return
      end
c
c***************************************************************
c
c   SUBROUTINE:  getang (piv,tab,tvec)
c
c   FUNCTION:  This routine converts Cartesian system of coord.  
c              to spherical system of coordinates.
c
c   INPUT:  tvec    R*8  D3  -  Cartesian point coordinates.
c
c   OUTPUT: piv     R*8  D1  -  The polar angle.
c
c           tab     R*8  D1  -  The longtitude angle.
c
c***************************************************************
c
      subroutine getang (piv,tab,tvec)
c
      real*8 tvec(3),tab,piv,tv(3),bet
c
      integer*4 kax
c
      tv(1)  = tvec(1)
      tv(2)  = tvec(2)
      tv(3)  = tvec(3)
c
c...Get rotary table
c
      kax    = 3
      call vecang (tv,kax,tab)
      bet    = 360. - tab
      call vecadj (tv,tv,bet,kax)
c
c...Get rotary pivot
c
      kax    = 2
      call vecang (tv,kax,piv)
c
  400 return
      end
c
c***************************************************************
c
c   SUBROUTINE:  verotat (gtxyz,gtv,angle,gaxis)
c
c   FUNCTION:  This routine rotates an input vector around 
c              the specified axis at given angle.
c
c   INPUT:  gtxyz   R*8  D3  -  The current vector.
c           angle   R*8  D1  -  The angle to rotate
c           gaxis   R*8  D3  -  Which axis to adjust for.
c
c   OUTPUT: gtv     R*8  D3  -  The adjusted vector.
c
c***************************************************************
c
      subroutine verotat (gtxyz,gtv,angle,gaxis)
c
      real*8 gtxyz(3),gtv(3),gaxis(3),angle,gu(3),gx(3)
c
      call f_cross(gtxyz,gaxis,gtv)
      call unitvc(gtv,gtv)
      call unitvc(gtxyz,gx)
      gu(1) = dcos(angle)
      gu(2) = dsin(angle)
      gu(3) = 0.d0
      do 10, i=1,3
10      gtv(i) = gx(i)*gu(1)+gtv(i)*gu(2)
      call unitvc(gtv,gtv)
      return
      end 
c
c***********************************************************************
c
c   SUBROUTINE:  gtmatr (krtax,grot,gtran,gmat)
c
c   FUNCTION:  This routine defines the conversion matrix for
c              one rotary axis.
c
c   INPUT:  krtax   I*4  D1    -  Linear axis number rotary axis rotates
c                                 about.
c
c           grot    R*8  D1    -  Rotary axis angle.
c
c           gtran   R*8  D3    -  Rotary axis origin.
c
c   OUTPUT: gmat    R*8  D4.3  -  Conversion matrix with displacement
c                                 vector.
c
c***********************************************************************
c
      subroutine gtmatr (krtax,grot,gtran,gmat)
c
      integer*4 krtax
c
      real*8 grot,gmat(4,3),gtran(3)
c
      integer*4 i,j
c
      real*8 vax(3,3),ang
c
      do 110 i=1,3
          vax(i,1) = 0.0
          vax(i,2) = 0.0
          vax(i,3) = 0.0
  110 continue
      vax(1,1) = 1.0
      vax(2,2) = 1.0
      vax(3,3) = 1.0
      ang    = 360.0 - grot
      call vecadj (vax(1,1),vax(1,1),ang,krtax)
      call vecadj (vax(1,2),vax(1,2),ang,krtax)
      call vecadj (vax(1,3),vax(1,3),ang,krtax)
      do 320 j=1,3
          do 310 i=1,3
              gmat(i,j) = vax(i,j)
  310     continue
          gmat(4,j) = gtran(j)
  320 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtmatb (krtax,grot,gtran,gmat)
c
c   FUNCTION:  This routine defines conversion matrix for the table
c              which is perpendicular to the tool axis.
c
c   INPUT:  krtax   I*4  D1    -  Linear axis number rotary table rotates
c                                 about.
c
c           grot    R*8  D1    -  Rotary table current angle.
c
c           gtran   R*8  D3    -  Rotary table origin.
c
c   OUTPUT: gmat    R*8  D4.3  -  Conversion matrix with displacement
c                                 vector.
c
c***********************************************************************
c
      subroutine gtmatb (krtax,grot,gtran,gmat)
c
      integer*4 krtax
c
      real*8 gmat(4,3),gtran(3),grot
c
      integer*4 i,j,nax
c
      real*8 vax(5,3),vec(3)
c
      data vax /1.,0.,0.,1.,0., 0.,1.,0.,0.,1., 0.,0.,1.,0.,0./
c
      nax = krtax
      if (nax .eq. 3) nax = 0
      do 320 j=1,3
          do 310 i=1,3
              gmat(i,j) = vax(nax+i,j)
  310     continue
  320 continue
c
c...rotate matrix about Z axis (table rotation axis)
c
      call vecadj (gmat(1,1),gmat(1,1),grot,3)
      call vecadj (gmat(1,2),gmat(1,2),grot,3)
      call vecadj (gmat(1,3),gmat(1,3),grot,3)
      call ptmatr (gtran,vec,gmat,2)
c
c...add table origin
c
      gmat(4,1) = gtran(1)
      gmat(4,2) = gtran(2)
      gmat(4,3) = gtran(3)
c
      return
      end
c
c
c***********************************************************************
c
c   SUBROUTINE:  matmul (gmat1,gmat2,gmat)
c
c   FUNCTION:  This routine multiplies two matrixes (rows by columns).
c
c   INPUT:  gmat1   R*8  D4.3  -  Input matrix I.
c
c           gmat1   R*8  D4.3  -  Input matrix II.
c
c   OUTPUT: gmat    R*8  D4.3  -  Output matrix = gmat1 X gmat2.
c
c***********************************************************************
c
      subroutine matmul (gmat1,gmat2,gmat)
c
      real*8 gmat(4,3),gmat1(4,3),gmat2(4,3)
c
      integer*4 i,j,k
      real*8 sum
c
      do 150 k=1,3
          do 130 j=1,3
              sum = 0.0
              do 110 i=1,3
                  sum = sum + gmat2(j,i) * gmat1(i,k)
  110         continue
              gmat(j,k) = sum
  130     continue
  150 continue
c
      return
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
            

