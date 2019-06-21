
C*********************************************************************
C*    NAME         :  lathex.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       lathex.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:14
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine lathex (g,x,y,ox,oy,igx)
c*          solve x on shape g per input y
C*    PARAMETERS   
C*       INPUT  : 
C*          y
C*       OUTPUT :  
C*          x
C*          ox
C*          oy
C*          igx  - index of the picked component
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine lathex (g,x,y,ox,oy,igx)
c
      include 'com4a.com'
      include 'mocom.com'

      real*8 g(1000),asn,x,y,tol
      real*4 ad(300)
      integer*2 ksn(4)
      equivalence (asn,ksn),(d,ad)
      equivalence (ad(7),xmin),(ad(8),ymin),(ad(9),xmax),(ad(10),ymax)
c
      tol = sc(27)
      asn=g(1)
      ng=ksn(3)
      ox=xmax
      oy=ymin
c                   start at g(11)
      x=xmax
      i=10
10    i=i+1
      if(i.lt.ng) goto 15
c                   no find. use xmin  ????????
      x=xmin
      i=ng-2
      if(itype.eq.7) i=ng-6
      goto 50
15    asn=g(i)
      if(ksn(3).ne.2.or.ksn(4).ne.5) goto 30
c
c..... item is line. if yln=g(i+2) is ok, use it
c
      itype=5
      if (g(i+2)-y .le. tol) then    
c                   (line does not apply)
         ox=g(i+1)
         oy=g(i+2)
         i=i+2
         goto 10
      endif
c                   calc x for this line
      dy=g(i+2)-oy
      ro=1.
      if (dy.gt.0.) ro=(y-oy)/dy
      x=ox+ro*(g(i+1)-ox)
      goto 50
30    if (ksn(3).ne.6.or.ksn(4).ne.7) goto 10
c                   item is circle.
      itype=7
      if (g(i+2).lt.y) then
c                    circle does not apply.
         ox=g(i+1)
         oy=g(i+2)
         i=i+6
         goto 10
      endif
c                     solve x for this circle.
      xe=g(i+1)
      ye=g(i+2)
      r=g(i+5)
      dy=y-g(i+4)
      dx=0.
      dxsq=r**2-dy**2
      if (dxsq.gt.0.) dx=sqrt(dxsq)
      x=g(i+3)+dx
c                     if x,y not between oxy and exy, flip dx
      vx=xe-ox
      vy=ye-oy
      dis=-vy*(xe-g(i+3))+vx*(ye-g(i+4))
      if (dis.lt.0.) then 
         dis=-dis
         vx=-vx
         vy=-vy
      endif 
      dck=-vy*dx+vx*dy
      if (dck-dis+tol.ge.0.) goto 50
      x=g(i+3)-dx
c
c..... exit
c
50    continue
      if (x.gt.xmax) x=xmax
      if (x.lt.xmin) x=xmin
c
c..... store g-index for lthmot
c
      igx=i
      return
      end
