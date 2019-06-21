C*********************************************************************
C*    NAME         :  mxmath.f
C*       CONTAINS:
C*          nclf_invmx  invmx   multmx  crotmx  identmx
C*
C*    COPYRIGHT 1992 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       mxmath.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:19
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclf_invmx
C*      C-wrapper for 'invmx' routine.
C*    PARAMETERS   
C*       INPUT  : 
C*          mx        - Matrix to invert.
C*       OUTPUT :  
C*          mxinv     - Inverted matrix.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine nclf_invmx (mx,mxinv)
c
      real*8 mx(12),mxinf(12)
c
      call invmx(mx,mxinv)
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine invmx
C*      Invert a matrix
C*    PARAMETERS   
C*       INPUT  : 
C*          mx        - Matrix to invert.
C*       OUTPUT :  
C*          mxinv     - Inverted matrix.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine invmx (mx, mxinv)

      include 'com.com'
c      include 'com4.com'

      real*8 mx(12), mxinv(12)

      real*8 sfx, sfy, sfz, sm
      integer*2 i, j
      parameter (sm=1.d-8)

      sfx=mx(1)**2+mx(5)**2+mx(9)**2
      sfy=mx(2)**2+mx(6)**2+mx(10)**2
      sfz=mx(3)**2+mx(7)**2+mx(11)**2
      if(sfx.lt.sm.or.sfy.lt.sm.or.sfz.lt.sm) then
c          error. mx does not invert
        ifl(2)=121
        goto 999
      endif
c          do rotation params
      do 10 i=1,3
      j=4*i-3
      mxinv(i)=mx(j)/sfx
      mxinv(i+4)=mx(j+1)/sfy
10    mxinv(i+8)=mx(j+2)/sfz              
c          origin tranlation in inverted matrix
      do 20 j=4,12,4                
      i=j-3                          
20    mxinv(j)=-mxinv(i)*mx(4)-mxinv(i+1)*mx(8)-mxinv(i+2)*mx(12)

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mxmult
C*      Multiply two matrices.
C*    PARAMETERS   
C*       INPUT  : 
C*          mx1       - Matrix to mult.
C*          mx2       - Matrix to mult.
C*       OUTPUT :  
C*          mxout     - Mx1 * Mx2.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine mxmult (mx1, mx2, mxout)

      include 'com.com'
c      include 'com4.com'

      real*8 mx1(12), mx2(12), mxout(12)

      real*8 tmpmx(12)
      integer*2 i, j, k

      i=0
      do 10 j=1,9,4
      do 10 k=1,4
      i=i+1
10    tmpmx(i)=mx1(j)*mx2(k)+mx1(j+1)*mx2(k+4)+mx1(j+2)*mx2(k+8)
c          fix origin
      tmpmx(4)=tmpmx(4)+mx1(4)
      tmpmx(8)=tmpmx(8)+mx1(8)
      tmpmx(12)=tmpmx(12)+mx1(12)

      do 20 i=1,12
20    mxout(i) = tmpmx(i)

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crotmx
C*      Create a rotation matrix
C*    PARAMETERS   
C*       INPUT  : 
C*          zaxis, xaxis  - new coodinate axes
C*       OUTPUT :  
C*          mx, mxinv     - rotation matrix and its inverse
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crotmx (zaxis,xaxis,mx,mxinv)
C*      Creates a matrix based on input Z-axis and X-axis vectors.
C*      Also returns its inverse matrix.
C*    PARAMETERS   
C*       INPUT  :  zaxis   R*8  D3   - New Z-axis vector.
C*
C*                 xaxis   R*8  D3   - New X-axis vector.
C*
C*       OUTPUT :  mx      R*8  D12  - Rotation matrix.
C*
C*                 mxinv   R*8  D12  - Inverse rotation matrix.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine crotmx (zaxis,xaxis,mx,mxinv)
c
      real*8 zaxis(3),xaxis(3),mx(12),mxinv(12)
c
      real*8 yaxis(3)
c
      integer*2 i
c
      call f_cross (zaxis,xaxis,yaxis)
      call unitizevc (yaxis)
c
      do i = 1,3 
        mx(i) = xaxis(i)
        mx(4+i) = yaxis(i)
        mx(8+i) = zaxis(i)
      enddo
      mx(4) = 0.
      mx(8) = 0.
      mx(12) = 0.
c
      do i = 1,12
        mxinv(i) = mx(i)
      enddo
c
      mxinv(5) = mx(2)
      mxinv(2) = mx(5)
c
      mxinv(9) = mx(3)
      mxinv(3) = mx(9)
c
      mxinv(10) = mx(7)
      mxinv(7) = mx(10)
c
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine identmx (gmx)
C*      Creates an identity matrix.
C*    PARAMETERS   
C*       INPUT  :  none
C*
C*       OUTPUT :  gmx  R*8  D12  - Identity matrix.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine identmx (gmx)
c
      real*8 gmx(12)
c
      integer*2 i
c
      do 100 i=1,12,1
          gmx(i) = 0.
  100 continue
      gmx(1) = 1.
      gmx(6) = 1.
      gmx(11) = 1.
c
      return
      end
