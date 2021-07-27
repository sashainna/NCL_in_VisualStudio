C*********************************************************************
C*    NAME         :  tsfsup.f
C*       CONTAINS:
C*    ptinsf (u, v, npts, bndy, insf)
C*    COPYRIGHT 1992 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       tsfsup.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:49
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptinsf1
C*      Determine if a point is inside or outside a trimmed surface boundary.
C*    PARAMETERS   
C*       INPUT  : 
C*          u        - U coordinate of point.
C*          v        - V coordinate of point.
C*          nb       - Number of boundaries.
C*          npts     - Number of u,v points in boundary.
C*          bndy     - U,v boundary.
C*       OUTPUT :  
C*          insf     - = 1, in boundary, = 0 on boundary, =-1 outside boundary.
C*          uout     - u value of nearest boundary point.
C*          vout     - v value of nearest boundary point.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ptinsf1 (u, v, nb, npts, bndy, insf, uout, vout)

      include 'com.com'

      real*8 u, v, bndy(12), uout, vout
      integer*2 nb, npts(10), insf

      real*8 dsq, dsqhld, a1, b1, a2, b2, an1, bn1, u1,v1, u2,v2, hut
      real*8 d1, d2, ux, vx, ut
      integer*2 i, j, k, n, ix, ihld, ilft
      real*8 EPS,EPS2
      parameter (EPS=1.0d-8)
      parameter (EPS2=1.0d-12)

      ix = 0

      do 40 k=1,nb
        n = ix+npts(k)
        dsqhld = 1.0d8
        do 20 i=ix+1,n-1
          j = i*2-1
          u1 = bndy(j)
          v1 = bndy(j+1)
          u2 = bndy(j+2)
          v2 = bndy(j+3)
          a1 = u2-u1
          b1 = v2-v1
          a2 = u1-u
          b2 = v1-v
          an1 = b1
          bn1 = -a1
          d1 = an1*b1-bn1*a1
          d2 = a2*bn1-b2*an1
          ut = 0.0
          if (d1.ne.0.0) ut = d2/d1
          if (ut.gt.1.0) ut = 1.0
          if (ut.lt.0.0) ut = 0.0
          ux = u1+ut*a1
          vx = v1+ut*b1
          dsq = (u-ux)**2+(v-vx)**2
          if (dsq.lt.dsqhld) then
            dsqhld = dsq
            ilft = 0
            if (ut.gt.0.0.and.ut.lt.1.0) then
              if (a2*an1+b2*bn1.ge.0.0) ilft = 1
            endif
            hut = ut
            ihld  = i
            uout = ux
            vout = vx
          endif
20      continue

        if (hut.eq.0.0.or.hut.eq.1.0) then
          ilft = 0
          j = ihld*2-1
          u1 = bndy(j)
          v1 = bndy(j+1)
          u2 = bndy(j+2)
          v2 = bndy(j+3)
          if (hut.lt.0.5) then
            i = ihld-1
            if (i.eq.ix) i = n-1
            j = i*2-1
            a1 = u1-bndy(j)
            b1 = v1-bndy(j+1)
            a2 = u2-u1
            b2 = v2-v1
          else
            i = ihld+2
            if (i.gt.n) i = ix+2
            j = i*2-1
            a1 = u2-u1
            b1 = v2-v1
            a2 = bndy(j)-u2
            b2 = bndy(j+1)-v2
          endif
          d1 = a1*b2-b1*a2
          if (d1.lt.0.0) ilft = 1
        endif

        insf = 2*ilft-1
        if (dsqhld.lt.EPS2) insf = 0
        if (insf.ne.1) goto 999
        ix = ix+npts(k)
40    continue

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptinsf
C*      Determine if a point is inside or outside a trimmed surface boundary.
C*    PARAMETERS   
C*       INPUT  : 
C*          u        - U coordinate of point.
C*          v        - V coordinate of point.
C*          nb       - Number of boundaries.
C*          npts     - Number of u,v points in boundary.
C*          bndy     - U,v boundary.
C*       OUTPUT :  
C*          insf     - = 1, in boundary, = 0 on boundary, =-1 outside boundary.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ptinsf (u, v, nb, npts, bndy, insf)

      include 'com.com'

      real*8 u, v, bndy(12)
      integer*2 nb, npts(10), insf

      real*8 uout, vout

      call ptinsf1 (u, v, nb, npts, bndy, insf, uout, vout)

999   return
      end

