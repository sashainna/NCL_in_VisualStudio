C*********************************************************************
C*    NAME         :  cvsfsd.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cvsfsd.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:48
C*********************************************************************
C
c **********************************************************************
c **********************************************************************
c **  subroutine name: cvsfsd
c **
c **  last revision: to change from allowing a minimum of 2 to 20 for the
c **                 the number of points used in the definition.
c **  purpose of subroutine: this routine handles curve definition:
c **                          a curve as a edge of a surface
c **                          ncl or mesh surface only
c **
c **    cv/sf,edge,num-pts,u/v offset
c **
c **      sf = name of surface to define curve along
c **      edge = a scalar indicating which of four possible edges of the
c **             surface to generate the curve along.
c **                edge = 1 - v=0 curve
c **                edge = 2 - v=1 curve
c **                edge = 3 - u=0 curve
c **                edge = 4 - u=1 curve
c **      num-pts = number of points to pass to curve fit routine
c **      u/v offset = value from 0 to 1 indicating u or v offset value
c **                   to be used when generating points.  this specifies
c **                   a distance in the v direction when defining a u 
c **                   type curve and a distance in the u direction when
c **                   defining a v type curve.
c **
c **      the method used for this curve definition is to temporarily 
c **      define a number of points along the edge or a constant offset 
c **      distance from the edge of a surface then pass them to the 
c **      curve/fit routine for the final curve canonical data generation.
c **
c **********************************************************************
c **********************************************************************
 
      subroutine cvsfsd

      include 'com8a.com'

      common/pblok/p

      real*8 p(610),asp(200)

      real*8 asn,asw,wmax
      real*4 u,v,dx
      integer*4 nclkey,na
      integer*2 ksn(4), edge, nwds, itype, iwf, isf
      equivalence (asn,ksn)
 
c*******************   cv/sf1,<edge>,<num-pts>,<u/v-offset>  -- 4 --

      edge = isc10(3)
      asw  = sc(11)
      asn  = asw
c
c...get the surface header information
c
      call gtdesc (asn,nclkey,nwds,itype)
      call sftype (nclkey, isf)
c
c...ncl, wf or mesh surfaces are ok
c
      if (isf.eq.25.or.isf.eq.27) goto 9321
      call isitwf(nclkey, iwf)
      if (iwf.eq.0) call gtgeo(nclkey,p(1))
c
c..... if the offset is specified as percentage compute the corresponding
c..... surface parameter
c
      if (isc10(13) .eq. 1) call cvsfs1 (nclkey,edge,sc(12))
c
c...set up which proper u or v edge to generate 
c......u edges
c
      if (edge .lt. 3) then
        v = sc(12)
        if (edge .eq. 2) v = 1.-v
        u = 0.
c
c......v edges
c
      else
        u = sc(12)
        if (edge .eq. 4) u = 1.-u
        v = 0.
      endif
c
c...load number of points to generate for curve fitting
c
      np = isc10(4)
      dx = np-1
      dx = 1./dx
c
c...set asn word for points.  Ran file and sc(11:22) support was replaced
c...by uu_list support (here ncl_put_uv and ncl_put_asn)
c
      ksn(1) = 0
      ksn(3) = 3
      ksn(4) = 3
c
c...get "np" points along surface in p array 
c          
      do 120 i=1,np
        call srfevl(asw,u,v,p(i*3-2))
        if (ifl(2).gt.0) goto 9999
c
c...put the asw of point in a temporary array
c
        ksn(2) = i
        asp(i) = asn
        if (edge .lt. 3) then
          u = u + dx
          if (u .gt. 1.) u = 1.
        endif
        if (edge .gt. 2) then
          v = v + dx
          if (v .gt. 1.) v = 1.
        endif
120   continue
c
c...store points in list
c
      call ncl_put_uv (p,np)
c
c...store asn words in list
c
      na = np
      call ncl_put_asn (asp,na)
      isc10(3)=np
      go to 99999

9321  ifl(2)=321
c
c...error exit.
c
9999  if(ifl(2).lt.1)ifl(2)=5
      err=.true.
c
c...exit back to geogn2 and let the curve/fit routines gen the curve
c
99999 return
      end
