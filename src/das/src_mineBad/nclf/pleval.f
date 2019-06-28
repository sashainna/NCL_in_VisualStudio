C*********************************************************************
C*    NAME         :  pleval.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       pleval.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:25
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pleval(nclkey,dtype)
C*      display circle and curve geometry types on the plotter
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

       subroutine pleval (nclkey, max, npts,buf, n)

c  dtype    type of display
c
c   7      disply/ci
c   8      disply/cv

      include 'com4a.com'
c      include '../incf/dspcom.com'

      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/dspcom/ws(20),w(maxwd)

      real*8 ws,w
      real*4 aws(40), aw(300)
      equivalence (ws,aws),(w,aw)

       integer*2 dtype,ninc,ix
       integer*4 nclkey
       real*8 sinc,xv,yv,zv,seq,p(3),dist
       real*4 sss,plunit(2),vt(3)
       integer*4 i4s107(2),svx,svy
       equivalence (i4s107,sc(107)),(sc(117),plunit)
       integer*2 isc115(2),slx,sly
       equivalence (isc115,sc(115))
       integer*2 max, npts, n
       real*8 buf(450) 
       data p/0.0,0.0,0.0/

 
	   if (n .eq. 1) then
       dtype = 6
       call gtgeo (nclkey, w(1))
       ws(1)=0.
       aws(39)=0.
c              bypass adispl value for number of cords
	   sc(12) = 40
c              put code here to display around a point
c...Display plane around display point
c...Bobby  -  2/25/94
c
      call gtdpt(nclkey,p)

	   dist = w(1)*p(1)+w(2)*p(2)+w(3)*p(3)-w(4)
	   w(4) = w(1)
	   w(5) = w(2)
	   w(6) = w(3)
	   w(1) = p(1) - w(1) * dist
	   w(2) = p(2) - w(2) * dist
	   w(3) = p(3) - w(3) * dist
	   w(7) = 1
	   w(8) = 0
	   w(9) = 0
	   w(10) = 0
	   w(11) = 0
       p(1) = 0
       p(2) = 0
       p(3) = 0

c          if it is a circle, generate the rest of the canonical data
c              if a full circle type geometry, create a dummy ok plane
	   if (w(8).eq.0. .and. w(9).eq.0. .and. w(10).eq.0.) then

c                  generate a,b,c values of a vector normal to the
c                  normal to the plane of the circle and a vector
c                  offset from that one by small amounts in all three directions
c
		   xv=w(4)+.1
		   yv=w(5)+.3
		   zv=w(6)+.7
		   w(8)=w(5)*zv-w(6)*yv 
		   w(9)=w(6)*xv-w(4)*zv 
		   w(10)=w(4)*yv-w(5)*xv 
		   seq=dsqrt(w(8)**2+w(9)**2+w(10)**2) 
		   w(8)=w(8)/seq 
		   w(9)=w(9)/seq 
		   w(10)=w(10)/seq

c                  calculate the d value of the ok plane based on
c                  the vector calculated above and adjust it by
c                  the radius (w(7)) value minus .0001 so it cuts
c                  the full circle a little

		   w(11)=w(8)*w(1)+w(9)*w(2)+w(10)*w(3)-w(7)+.00001
       endif

	   w(12)=w(10)*w(5)-w(9)*w(6)
	   w(13)=w(8)*w(6)-w(10)*w(4)
	   w(14)=w(9)*w(4)-w(8)*w(5)
	   h=w(11)-w(8)*w(1)-w(9)*w(2)-w(10)*w(3)
	   cosphi=h/w(7)
	   w(15)=acos(cosphi)

c          go to starting point of geometry
       sss=0.

       if(sc(12) .lt. 10.0) sc(12) = 10.
       sinc=1./(sc(12)-1.)
       ninc=sc(12)-1.
	   ix = 0
	   npts = ninc

       do 100 i=1,ninc
           call dcrvpt (sss, dtype, buf(ix+1), vt)
		   ix = ix + 3
           sss=sss+sinc
100    continue

       else if (n .eq. 2) then
		   npts = 2
		   ix = 0
		   buf (ix+1) = w(1)
		   buf (ix+2) = w(2)
		   buf (ix+3) = w(3)

           svx = i4s107(1)
           svy = i4s107(2)
           w(1) = w(1) + w(4) * .5
           w(2) = w(2) + w(5) * .5
           w(3) = w(3) + w(6) * .5
		   ix = ix +3
		   buf (ix+1) = w(1)
		   buf (ix+2) = w(2)
		   buf (ix+3) = w(3)

           slx = isc115(1)
           sly = isc115(2)
           i4s107(1) = slx
           i4s107(2) = sly
           i4s107(1) = svx
           i4s107(2) = svy
       endif


      return
      end
