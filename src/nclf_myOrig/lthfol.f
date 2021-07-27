C*********************************************************************
C*    NAME         :  lthfol.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       lthfol.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:15
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine lthfol (oycut)
c*         gen tool motion along sh2 to  1st found of -- oycut/xmin/ymax
c*         sh2 is in f-tbl
c*         if nec, follow xmin to find an endpt.
c*         do not allow +x or -y moves.
c*         o/p via putcl  5000-5 recs for st-ln moves
c*                        3000-2  +  5000-5 (many pts) for circs.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine lthfol (oycut)

      include 'com4a.com'
      include 'const.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 buf(120),asn,e,f,a,b,dbuf(3),abuf(6),rdat(13),x,y,ox,oy
      real*4 ad(300),af(2000)
      integer*2 ksn(4),ka(40),kf(4000)
      equivalence (a(2),x),(a(3),y),(a(5),ox),(a(6),oy)
      equivalence (asn,ksn),(d,ad),(a,ka),(f,af,kf),(buf,dbuf)
      equivalence (ad(7),xmin),(ad(8),ymin),(ad(9),xmax),(ad(10),ymax)

      real*8 bb
c
      abuf(3)=0.
      abuf(4)=sc(4)
      abuf(5)=sc(5)
      abuf(6)=sc(6)
      ifx=ka(4)
      nf=kf(3)
      tol=sc(27)
      xt=x
      yt=y
      hx=x
      hy=y
c               init rdat for cirint o/p
      rdat(1)=5.
      do 8 i=2,10
8     rdat(i)=0.
      rdat(11)=1.
      goto 20
c
c                    find next entity and update all
c
10    j=ifx+1
      do 12 i=j,nf
      k=4*i
      if(kf(k-1).eq.2.and.kf(k).eq.5)goto 15
      if(kf(k-1).eq.6.and.kf(k).eq.7)goto 15
12    continue
c                    another entity not found. goto this x and oycut
      yt=oycut
      goto 80
c                    next entity found. hold ox,y and point to next.
15    ox=f(ifx+1)
      oy=f(ifx+2)
      ifx=i
c
c                    do motion for this entity.
20    asn=f(ifx)
c
      if(ksn(3).ne.2.or.ksn(4).ne.5) goto 100
c
c*******************  line  ******************
      dx=f(ifx+1)-ox
      dy=f(ifx+2)-oy
      if(abs(dy).gt.tol) goto 30
c                   ln is ypl      (temp goto ln endpt)
      xt=f(ifx+1)
      yt=f(ifx+2)
      goto 80
c                 inters ln and oycut  (line must not point dn or rgt)
30    if(dy+tol.lt.0..or.dx-tol.gt.0.) ifl(2)=247
      ro=(oycut-oy)/dy
      xt=ox+ro*dx
      yt=oycut
      if(ro.le.1.) goto 40
c                 ro .gt. 1     use xy ln endpt
      xt=f(ifx+1)
      yt=f(ifx+2)
      goto 80
c                 ro must be .gt. 0
cccccccccc           (is this next error real?)      17-nov-83
40    if(ro.le.0.)ifl(2)=5
c                 output this pt (if no error)
80    if(ifl(2).gt.0) goto 999
c                 loc must be ok per xmin,oycut,current tool y-val
      if(xt.lt.xmin) xt=xmin
      if(yt.gt.oycut)yt=oycut
      if(yt.lt.y)yt=y
c                 load xt,yt in buf(1),  if all ok.
90    dx=hx-xt
      dy=hy-yt
c                 if no chg from last loc, skip the o/p step
      if(abs(dx).lt.tol.and.abs(dy).lt.tol)goto 92
      buf(1)=xt
      buf(2)=yt
      hx=xt
      hy=yt
      abuf(1) = dbuf(1)
      abuf(2) = dbuf(2)
cc      if (ka(2).gt.0)call plotm(abuf,.false.)
      if (ka(1).gt.0) then
        if (ifl(82) .eq. 0) then
          call putcl(5000,5,1,dbuf)
        else
          call putcl(5000,5,1,abuf)
        endif
      endif
c                  if oycut was reached, exit now
92    if(abs(yt-oycut).le.tol)goto 999
c                  oycut not reached.  if x=xmin, goto oycut now
      if(abs(xt-xmin).gt.tol)goto 10
      xt=xmin
      yt=oycut
      goto 90
c
c************************  circle  *************************
c                  get cir info
100   jfx=2*ifx
      xc=f(ifx+3)
      yc=f(ifx+4)
      r=f(ifx+5)
      bst=af(jfx+11)
      bnd=af(jfx+12)
c                  set new angles as reqd.
      if(yt.le.oy+tol) goto 110
c                      new start angl
      dx=xt-xc
      dy=yt-yc
      bst=0.

      if (abs(dy).lt..0001) dy=0.

      if (dx**2+dy**2.lt.1.e-6) goto 102

      if (abs(dx).lt..0001) then
        if (dy .gt. 0.) then
           bst = HALF_PI
        else if (dy .lt. 0.) then
           bst = - HALF_PI
        endif
        goto 102
      endif

      bst=atan2(dy,dx)

102   if(bst.lt.0.) bst=bst+TWO_PI

110   if(oycut.ge.f(ifx+2)-tol) goto 120
c                      new end angl
      dy=oycut-yc
      dxsq=r**2-dy**2
      dx=0.
      if (dxsq.gt.0.) dx=sqrt(dxsq)
c                      dx +/- from arc direction.  ok ?????????
      ksigx=4*ifx-2
      if (kf(ksigx).lt.0) dx=-dx
      bnd=0.

      if (abs(dy).lt..0001) dy=0.

      if (dx**2+dy**2.lt.1.e-6) goto 112

      if (abs(dx).lt..0001) then
        if (dy .gt. 0.) then
           bnd = HALF_PI
        else if (dy .lt. 0.) then
           bnd = - HALF_PI
        endif
        goto 112
      endif

      bnd=atan2(dy,dx)

112   if (bnd.lt.0.) bnd=bnd+TWO_PI

120   delb=bnd-bst
      toler=sc(27)
      h=r-toler
      if(h.lt.r*.8)h=r*.8
      co=h/r
      db=acos(co)*2.
      anpts=abs(delb)/db
      npts=anpts+1.
c                  npts = 2 min, 40 max
      if (npts.lt.2) npts=2
      if (npts.gt.40) npts=40
      anpts=npts
      db=delb/anpts
      n=0
      bb=bst
c                  calc x,y and add to buf.
      do 130 n=1,npts
      i=3*n-2
      bb=bb+db
      buf(i)=xc+r*cos(bb)
      buf(i+1)=yc+r*sin(bb)
      buf(i+2)=0.
c                   if plotting, do it now
cc      if (ka(2).eq.0) goto 130
cc      abuf(1)=buf(i)
cc      abuf(2)=buf(i+1)
cc      call plotm(abuf,.false.)
130   continue
      xt=buf(i)
      yt=buf(i+1)
c                   issue clfile records  (if kcl on)
      if (ka(1).eq.0) goto 140
c                    3000-2 rec.  (nccs format, not westinghouse)
      rdat(6)=xc
      rdat(7)=yc
      rdat(12)=r
      call putcl(3000,2,13,rdat)
c                 now 5000-5 rec
      if (ifl(82) .eq. 0) then
        call putcl(5000,5,npts,buf)
      else
        it = 5
        do 135 n=1,npts
          i=3*n-2
          abuf(1)=buf(i)
          abuf(2)=buf(i+1)
          call putcl(5000,it,1,abuf)
          it = 6
135     continue
      endif
140   continue
      hx=xt
      hy=yt
      if (abs(yt-oycut).gt.tol) goto 10
 
999   continue
      x=xt
      y=yt
      return
      end
