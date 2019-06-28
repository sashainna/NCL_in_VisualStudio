c
c***********************************************************************
c
c   FILE NAME:  math4.f
c   CONTAINS: Vector manipulation routines in real*4:
c
c             vctovc4 f_mag4 vcplvc4 vcmnvc4 uvcplvc4 unitvc4
c             f_cross4 f_dot4 vctmsc4 f_dist4 
c             fill_array4 mnvc4 unitizevc4 avcplbvc4 triple_cross4 
c             triple_dot4 xyzvc4 point_on_plane4 max2_r4 min2_r4
c             max3_r4 min3_r4
c
C*    MODULE NAME AND RELEASE LEVEL
C*        math4.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  vctovc4 (gpti,gpto)
c
c   FUNCTION:  This routine is equivalent to um_vctovc. 
c
c   INPUT:  gpti    R*4  D3    -  Input coordinates.
c
c   OUTPUT: gpto    R*4  D3    -  Output coordinates.
c
c***********************************************************************
      subroutine vctovc4 (gpti,gpto)
      real*4 gpti(3),gpto(3)
      integer*2 i
c
      do 1 i=1,3
  1      gpto(i) = gpti(i)

      return
      end
c***********************************************************************
c
c   FUNCTION:  f_mag4 (v)
c
c   FUNCTION:  calc. magnitude of a vector
c
c   INPUT:  v    R*4  D3    -  Input vector.
c
c   OUTPUT: returns ||v|| 
c
c***********************************************************************
      function f_mag4 (v)
      real*4 v(3), zero/0./,r,f_mag4
      integer*2 i
c
      r = zero
      do 1 i=1,3
  1      r = r + v(i)**2
      f_mag4 = sqrt(r)

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  vcplvc4 (gpi1,gpi2,gpo)
c
c   FUNCTION:  This routine is equivalent to um_vcplvc. 
c
c   INPUT:  gpi1    R*4  D3    -  First vector.
c
c           gpi2    R*4  D3    -  Second vector.
c
c   OUTPUT: gpo     R*4  D3    -  Resultant vector.
c
c***********************************************************************
      subroutine vcplvc4 (gpi1,gpi2,gpo)
      real*4 gpi1(3),gpi2(3),gpo(3)
      integer*2 i
c
      do 1 i=1,3
  1      gpo(i) = gpi1(i) + gpi2(i)

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  vcmnvc4 (gpi1,gpi2,gpo)
c
c   FUNCTION:  This routine is equivalent to um_vcmnvc. 
c
c   INPUT:  gpi1    R*4  D3    -  First vector.
c
c           gpi2    R*4  D3    -  Second vector.
c
c   OUTPUT: gpo     R*4  D3    -  Resultant vector.
c
c***********************************************************************
      subroutine vcmnvc4 (gpi1,gpi2,gpo)
      real*4 gpi1(3),gpi2(3),gpo(3)
      integer*2 i
c
      do 1 i=1,3
  1      gpo(i) = gpi1(i) - gpi2(i)

      return
      end
c***********************************************************************
c
c
c   SUBROUTINE:  uvcplvc4 (gpi1,gpi2,gpo,gfc)
c
c   FUNCTION:  This is universal +/- vector*factor routine
c
c   INPUT:  gpi1    R*4  D3    -  First vector.
c           gpi2    R*4  D3    -  Second vector.
c           gfc     R*4  D1    -  factor to mult gpi2.
c
c   OUTPUT: gpo     R*4  D3    -  Resultant vector.
c
c***********************************************************************
      subroutine uvcplvc4 (gpi1,gpi2,gpo,gfc)
      real*4 gpi1(3),gpi2(3),gpo(3),gfc
      integer*2 i
c
      do 1 i=1,3
  1      gpo(i) = gpi1(i) + gfc*gpi2(i)

      return
      end
c***********************************************************************
c
c   SUBROUTINE:   unitvc4 (gpi,gpo)
c
c   FUNCTION:  calculates unit vector
c
c   INPUT:  gpi    R*4  D3    -   vector.
c
c   OUTPUT: gpo    R*4  D3   -  Resultant vector.
c
c   NOTE:   returns the input vector if its length is too small 
c
c***********************************************************************
      subroutine unitvc4 (gpi,gpo)
      real*4 gpi(3),gpo(3),zero/0./,gfc,f_mag4
      integer*2 i
c
      gfc = f_mag4(gpi)
      if(gfc.eq.zero) gfc = 1.

      do 1 i=1,3
  1      gpo(i) = gpi(i)/gfc

      return
      end

c***********************************************************************
c
c   SUBROUTINE:  f_cross4(a, b, v)
c
c   FUNCTION: calculates cross product of 2 vectors
c
c   INPUT:  a    R*4  D3    - 1st vector
c           b    R*4  D3    - 2nd vector
c
c   OUTPUT: v    R*4  D3   -  Resultant vector; v = axb
c
c***********************************************************************
      subroutine f_cross4(a, b, v)
      real*4 a(3), b(3), v(3), vv(3)

      vv(1) = a(2)*b(3) - a(3)*b(2)
      vv(2) = a(3)*b(1) - a(1)*b(3)
      vv(3) = a(1)*b(2) - a(2)*b(1)

      call vctovc4(vv,v)

      return
      end
c***********************************************************************
c
c   FUNCTION:  f_dot4(a,b)
c
c   FUNCTION: calculates dot product of 2 vectors
c
c   INPUT:  a    R*4  D3    - 1st vector
c           b    R*4  D3    - 2nd vector
c
c   RETURNS: f_dot4   R*4  D1   - dot product (a,b) 
c
c***********************************************************************
      function f_dot4(a,b)
      real*4 a(3), b(3), zero/0./,d,f_dot4
      integer*2 i

      d = zero
      do 1 i=1,3
 1       d = d + a(i)*b(i)    
      f_dot4 = d

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  vctmsc4(vi,vo, fac)
c
c   FUNCTION: multiplies vector by scalar
c
c   INPUT:  vi    R*4  D3    - vector to be multiplied
c           fac   R*4  D1    - scalar
c
c   OUTPUT: vo    R*4  D3   -  Resultant vector; vo = fac*vi
c
c***********************************************************************

      subroutine vctmsc4(vi,vo, fac)
      real*4 vi(3), vo(3),fac
      integer*2 i

      do 1 i=1,3
 1       vo(i) = vi(i)*fac   

      return
      end
c***********************************************************************
c
c   FUNCTION:  f_dist4(p1,p2)
c
c   FUNCTION: calculates distance between 2 points
c
c   INPUT:  p1       R*4  D3    - 1st point
c           p2       R*4  D3    - 2nd point
c
c   RETURNS: f_dist  R*4  D1   - distance between p1,p2
c
c***********************************************************************
      function f_dist4(p1,p2)
      real*4 p1(3), p2(3),zero/0./,d,f_dist4
      integer*2 i

      d = zero
      do 1 i=1,3
 1       d = d + (p1(i)-p2(i))**2    
      f_dist4 = sqrt(d)

      return
      end
c***********************************************************************
c
c   SUBROUTINE: fill_array4(v,n,value)
c
c   FUNCTION: assigns same value to all elements of an array
c
c   INPUT:  v      R*4  Dn  -  array to be filled 
c           n      I*2  D1  -  dim(v)
c           value  R*4  D1  -  value to be assigned to all elements of v
c
c   OUTPUT: array v: v(i) = value, i=1,...,n
c
c***********************************************************************
      subroutine fill_array4(v,n,value)
      real*4 v(1),value
      integer*2 n

      do 1 i = 1,n
 1       v(i) = value
     
      return
      end
c***********************************************************************
c
c   SUBROUTINE: mnvc4(v)
c
c   FUNCTION: changes sign of a vector
c
c   INPUT:  v      R*4  D3  -  a vector 
c
c   OUTPUT: vector v = -v
c
c***********************************************************************
      subroutine mnvc4(v)
      real*4 v(3)
      integer*2 n

      do 1 i = 1,3
 1       v(i) = -v(i)
     
      return
      end
c***********************************************************************
c
c   SUBROUTINE: unitizevc4(v)
c
c   FUNCTION: makes unit vector of a vector
c
c   INPUT:  v      R*4  D3  -  a vector 
c
c   OUTPUT: vector v = v/||v||
c
c***********************************************************************
      subroutine unitizevc4(v)
      real*4 v(3),zero/0./,f,f_mag4
      integer*2 i
c
      f = f_mag4(v)
      if(f.eq.zero) return

      do 1 i=1,3
  1      v(i) = v(i)/f

      return
      end
c***********************************************************************
c
c   SUBROUTINE: avcplbvc4(a,v1,b,v2,vres)
c
c   FUNCTION: calc. linear combination of 2 vectors
c
c   INPUT:  
c          a      R*4  D1  -   number
c          v1     R*4  D3  -   vector 
c          b      R*4  D1  -   number
c          v2     R*4  D3  -   vector 
c
c   OUTPUT: vector vres = a*v1 + b*v2
c
c***********************************************************************
      subroutine avcplbvc4(a,v1,b,v2,vres)
      real*4 v1(3),v2(3),vres(3),a,b
      integer*2 i

      do 1 i=1,3
 1       vres(i) = a*v1(i) + b*v2(i)
     
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  triple_cross4(a,b,c,v)     
c
c   FUNCTION: calc. vector product of 3 vectors: [[a,b],c]
c
c   INPUT:  
c          a      R*4  D3  -   1st vector
c          b      R*4  D3  -   2nd vector
c          c      R*4  D3  -   3rd vector
c
c   OUTPUT: 
c           v      R*4  D3  -   resulting vector 
c
c***********************************************************************
      subroutine triple_cross4(a,b,c,v)
      real*4 a(3),b(3),c(3),v(3),ac,bc,f_dot4
c
c...  [[a,b],c] = (a,c)*b - (b,c)*a ;  [a,[b,c]] = (a,c)*b - (a,b)*c
c
      ac = f_dot4(a,c)
      bc = f_dot4(b,c)
      call avcplbvc4(ac,b,-bc,a,v)
 
      return
      end
c***********************************************************************
c
c   FUNCTION:  triple_dot4(a,b,c)     
c
c   FUNCTION: calc. scalar triple product of 3 vectors: (a,[b,c])
c
c   INPUT:  
c          a      R*4  D3  -   1st vector
c          b      R*4  D3  -   2nd vector
c          c      R*4  D3  -   3rd vector
c
c   OUTPUT: 
c           Returns the result (R*4) 
c
c***********************************************************************
      function triple_dot4(a,b,c)
      real*4 a(3),b(3),c(3),res,triple_dot4

      res = a(1)*(b(2)*c(3) - b(3)*c(2)) + 
     *      a(2)*(b(3)*c(1) - b(1)*c(3)) + 
     *      a(3)*(b(1)*c(2) - b(2)*c(1))  

      triple_dot4 = res

      return
      end

c***********************************************************************
c
c   SUBROUTINE: xyzvc4(x,y,z,vec)
c
c   FUNCTION: constructs a 3D vector from x,y,z components
c
c   INPUT:  
c          x      R*4  D1  -   x component
c          y      R*4  D1  -   y component
c          z      R*4  D1  -   z component
c
c   OUTPUT: 
c          vec    R*4  D3  -   vector
c
c***********************************************************************
      subroutine xyzvc4(x,y,z,vec)     
      real*4 x,y,z,vec(3)
      
      vec(1) = x
      vec(2) = y
      vec(3) = z

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  point_on_plane4(point,p,n,pres)
c
c   FUNCTION: projects point on  plane
c
c   INPUT:  point  R*4  D3  -  given point
c           p,n    R*4  D3  -  a point and unit normal defining plane
c
c   OUTPUT: pres   R*4  D3  -  resultant point
c
c***********************************************************************
      subroutine point_on_plane4(point,p,n,pres)
      real*4 point(3),p(3),n(3),pres(3),vec(3),a(3),
     *       f_dot4
    
      call vcmnvc4(point,p,vec)
      call vctmsc4(n,a,f_dot4(vec,n))
      call vcmnvc4(point,a,pres)

      return
      end
c***********************************************************************
c
c   FUNCTION: max2_r4(x,y)
c
c   FUNCTION: gives max(x,y)
c
c   INPUT:
c          x      R*4  D1  - a number
c          y      R*4  D1  - a number
c
c   OUTPUT:
c          returns max(x,y)
c
c***********************************************************************
      function max2_r4(x,y)
      real*4 x,y,max2_r4
     
      if (x.ge.y) then
         max2_r4 = x
      else
         max2_r4 = y
      endif

      return
      end
c***********************************************************************
c
c   FUNCTION: min2_r4(x,y)
c
c   FUNCTION: gives min(x,y)
c
c   INPUT:
c          x      R*4  D1  - a number
c          y      R*4  D1  - a number
c
c   OUTPUT:
c          returns min(x,y)
c
c***********************************************************************
      function min2_r4(x,y)
      real*4 x,y,min2_r4

      if (x.ge.y) then
         min2_r4 = y
      else
         min2_r4 = x
      endif

      return
      end
c***********************************************************************
c
c   FUNCTION: min3_r4(x,y,z)
c
c   FUNCTION: gives min(x,y,z)
c
c   INPUT:
c          x      R*4  D1  - a number
c          y      R*4  D1  - a number
c          z      R*4  D1  - a number
c
c   OUTPUT:
c          returns min(x,y,z)
c
c***********************************************************************
      function min3_r4(x,y,z)
      real*4 x,y,z,min3_r4,min2_r4

      min3_r4 = min2_r4 (x, min2_r4 (y,z))
   
      return
      end
c***********************************************************************
c
c   FUNCTION: max3_r4(x,y,z)
c
c   FUNCTION: gives max(x,y,z)
c
c   INPUT:
c          x      R*4  D1  - a number
c          y      R*4  D1  - a number
c          z      R*4  D1  - a number
c
c   OUTPUT:
c          returns max(x,y,z)
c
c***********************************************************************
      function max3_r4(x,y,z)
      real*4 x,y,z,max3_r4,max2_r4

      max3_r4 = max2_r4 (x, max2_r4 (y,z))

      return
      end
