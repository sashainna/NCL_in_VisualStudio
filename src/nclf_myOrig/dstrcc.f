C*********************************************************************
C*    NAME         :  dstrcc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dstrcc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:00
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dstrcc(nclkey,dtype)
C*      Display circle and curve geometry types on the plotter
C*      Also used to display PLANEs as dashed circle.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey : Key of entity to display
C*          dtype  : Type of entity to display
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/


       subroutine dstrcc (nclkey, dtype, tfmat)

c  dtype    type of display
c
c   6      disply/pl
c   7      disply/ci
c   8      disply/cv

      include 'com4a.com'
c      include '../incf/dspcom.com'

      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/dspcom/ws(20),w(maxwd)

      real*8 w,ws
      real*4 aw(300),aws(40)
      equivalence (w,aw),(ws,aws)

      real*8 p(3)
      real*8 pout(3)
      real*4 v(3)
       integer*2 dtype,ninc
       integer*4 nclkey
       real*8 tfmat(12)
       real*8 sinc,xv,yv,zv,seq
       real*4 sss,plunit(2)
       integer*4 i4s107(2),svx,svy
       equivalence (i4s107,sc(107)),(sc(117),plunit)
       integer*2 isc115(2),slx,sly
       equivalence (isc115,sc(115))
       logical b

       b = .false. 
       
       ws(1)=0.
       aws(39)=0.
       if (dtype.eq.8) then
           aws(40)=aw(1)
           aw(1)=0.
       else if (dtype.eq.6) then
c              bypass adispl value for number of cords
           sc(12) = 40
c              put code here to display around a point
c              get the point and the normal vector directly from
c              unibase. kathy
c
           w(4) = w(1)
           w(5) = w(2)
           w(6) = w(3)
           call gtdpt(nclkey,w(1))
           w(7) = 1.
           w(8) = 0.
           w(9) = 0.
           w(10) = 0.
           w(11) = 0.
       endif

c          if it is a circle, generate the rest of the canonical data
       if (dtype.eq.6.or.dtype.eq.7) then
c              if a full circle type geometry, create a dummy ok plane
           if (w(8).eq.0. .and. w(9).eq.0. .and. w(10).eq.0.) then

c                  generate a,b,c values of a vector normal to the
c                  normal to the plane of the circle and a vector
c                  offset from that one by small amounts in all three directions
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
       endif

c          go to starting point of geometry
       sss=0.
       call dcrvpt (sss, dtype,p,v)
       call cctmtf(p, tfmat, pout)
       call gmova3(pout(1),pout(2),pout(3))

       if(sc(12) .lt. 4.) sc(12) = 4.
       sinc=1./(sc(12)-1.)
       ninc=sc(12)-1.
       do 100 i=1,ninc
           sss=sss+sinc
           call dcrvpt (sss, dtype,p,v)
c
c....set up translation matrix first
c....Yurong 9/30/97
c
           call cctmtf(p, tfmat, pout)
           if (dtype.eq.6.and..not.b) then
                call gmova3(pout(1),pout(2),pout(3))
                b = .true.
           else
                call glina3(pout(1),pout(2),pout(3))
                b = .false.
           endif
100    continue
       call gdraw

c
c...Circles are now drawn in um_drw3_circle routine,
c...and Curve and Plane labels are drawn in the class
c...dispatcher (uc_draw), after being added as a valid
c...wireframe entity (wf_geom_type), ... so no need for this code
c...Bobby  -  8/8/94
c
c       if((dtype.eq.7).or.(dtype.eq.8) .or.
c     1    (dtype.eq.6)) then
c
c          call dcrvpt(.5, dtype,p,v)
c          call drwlab(p(1),p(2),p(3),nclkey) BOBBY
c
c       endif

c              if it's a plane, display the normal vector
       if (dtype.eq.6) then
c
c....set up translation matrix first
c....Yurong 9/30/97
c
           p(1) = w(1)
           p(2) = w(2)
           p(3) = w(3)
           call cctmtf(p, tfmat, pout)
           call gmova3(pout(1), pout(2), pout(3))
           svx = i4s107(1)
           svy = i4s107(2)
           w(1) = w(1) + w(4) * .5
           w(2) = w(2) + w(5) * .5
           w(3) = w(3) + w(6) * .5
           slx = isc115(1)
           sly = isc115(2)
c
c....set up translation matrix first
c....Yurong 9/30/97
c
           p(1) = w(1)
           p(2) = w(2)
           p(3) = w(3)
           call cctmtf(p, tfmat, pout)
           call glina3(pout(1), pout(2), pout(3))
           i4s107(1) = slx
           i4s107(2) = sly
c           call darrow(minus) here
           i4s107(1) = svx
           i4s107(2) = svy
           call gdraw
       endif


      return
      end
