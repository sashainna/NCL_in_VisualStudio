C*********************************************************************
C*    NAME         :  cvintx.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
c*       cvintx.f , 25.1
c*    DATE AND TIME OF LAST  MODIFICATION
c*       04/29/15 , 15:09:47
C*********************************************************************
C
c **********************************************************************
c **********************************************************************
c **  subroutine name: cvintx
c **
c **
c **  purpose of subroutine: this routine handles the curve definition:
c **                         curve/intof,sf/pl,sf/pl,pt
c **
c **      a curve as the intersection of a surf and another surf or plane
c **      sf/pl = a surf or plane id.  one of the two sf/pl entities must
c **      be a surface.  the curve defined is along the intersection of
c **      these two sf/pl entities.  no extention of surfaces are used.
c **      the optional near point specifies which of curve segment should 
c **      be used in cases where more than one curve segment is formed by
c **      the intersection.
c **      
c **      the method used for this curve definition is to generate a 
c **      series of points along the intersection of the specified surf/
c **      plane geometry then pass them to the curve/fit routine for the
c **      final curve canonical data generation.
c ** 
c **  Input:
c **    itsk - 1 = generate xyz points, 2 = generate uv points of sf.
c **      
c **********************************************************************
c **********************************************************************
 
      subroutine cvintx (itsk)

      include 'com4a.com'

      common/pblok/p

      real*8 p(400)
      real*4 ap(800)
      integer*2 ip(1600)
      equivalence (p,ap,ip)

      real*8 asn,px(3000)
      real*8 vx(6)
      integer*4 n,kn
      integer*2 ksn(4),nmx,i,na,nbx
      equivalence (asn,ksn)
      equivalence (isrf,ifl(54)),(ia,ifl(51)),(ntk,ifl(79))
c
c*****************************   cv/intof,sf/pl,sf/pl,pt             -- 5 --
c
      call cvptsn (itsk,n,px,vx)
      if (err) go to 99999
c
c...add to list after last point corresponding asn's so it is
c...compatible with ranfile and will support point & vectors
c...when crvfit is called in gencrv. Here we have points only.
c
      ksn(1) = 0
      ksn(3) = 3
      ksn(4) = 3
      nmx    = 300 
      nbx    = 0
      na     = n
  100 if (nmx .gt. na) nmx = na
      do 105 i=1,nmx
         ksn(2) = nbx + i
         p(i) = asn
  105 continue
c
c...push asn's in chunks of 300 unless n <= 300 and one push is OK
c
      kn     = nmx
      call ncl_put_asn (p,kn)
      na     = na - nmx
      nbx    = nbx + nmx
      if (na .gt. 0) go to 100
c
c...If UV points are generated (itsk=2), points are in the list 
c...to use by C routines interpolating rbsplines
c
      isc10(3) = n
      go to 99999
c
c...End of routine
c
99999 return
      end
