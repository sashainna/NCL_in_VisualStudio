C*********************************************************************
C*    NAME         :  wlproj.f
C*       CONTAINS:
C*                 wlproj
C*    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
C*      MODULE NAME AND RELEASE LEVEL
C*         wlproj.f , 25.1
C*      DATE AND TIME OF LAST MODIFICATION
C*         04/29/15 , 15:10:54
C*********************************************************************
C
C***********************************************************************
C*    E_SUBROUTINE     : subroutine wlavgh
C*       Average results of two projections. Mostly borrowed from cssurf    
C*    PARAMETERS
C*       INPUT  :
C*          h1, h2        - two heights (up the tool axis)
C*          hs1, hs2      - two projection planes
C*          eps   - tolerance
C*       OUTPUT :
C*          h     - average height
C*          ifnd  - found a new different height iff 1
C*     RETURNS      : none
C*     SIDE EFFECTS : none
C*     WARNINGS     : none
C***********************************************************************
 
      subroutine wlavgh (hs1,hs2,h1,h2,h,ifnd,eps)

      include 'com4a.com'
      include 'mocom.com'

      real*4 hs1(7),hs2(7)
      real*4 h1,h2,h,eps
      integer*2 ifnd

      integer*2 jd(600)
      real*4 ad(300),asc(310)
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      equivalence (d,ad,jd)
      equivalence (ifl(51),ia),(ifl(54),isrf)

      real*4 p99,t99,co12,co,si,hrad,x,y,z,a,b,c,den,tan1,tan2

      real*4 yta,ytb,ytc,rx,ry,rz,sec,yt1,yt2,zt1,zt2,yc,zc
      real*4 f_dot4

      ifnd = 0

      p99=.9999
      t99 = 70.705375
      hrad = tool(1)/2.

      co12 = f_dot4 (hs1,hs2)
      co12 = abs (co12)
c
c..... if pl1 parlel pl2
c
      if (co12 .gt. .999999) return

      x=t(1,ia)
      y=t(2,ia)
      z=t(3,ia)
      a=t(4,ia)
      b=t(5,ia)
      c=t(6,ia)

      yta = hs1(1) + hs2(1)
      ytb = hs1(2) + hs2(2)
      ytc = hs1(3) + hs2(3)
c             rgt sense
      rx=ytb*c-ytc*b
      ry=ytc*a-yta*c
      rz=yta*b-ytb*a
c             actual yt axis   (perpto rgt and ta)
      yta=rz*b-ry*c
      ytb=rx*c-rz*a
      ytc=ry*a-rx*b
      sec = yta*yta+ytb*ytb+ytc*ytc
      if (sec .lt. eps) return
      sec = sqrt(sec)
cccccccccccccccccccccccc
32    yta=yta/sec
      ytb=ytb/sec
      ytc=ytc/sec

      dx=hs1(5)-x
      dy=hs1(6)-y
      dz=hs1(7)-z
      zt1=a*dx+b*dy+c*dz
      yt1=yta*dx+ytb*dy+ytc*dz
c             sfpt 2  in t-sys
      dx=hs2(5)-x
      dy=hs2(6)-y
      dz=hs2(7)-z
      zt2=a*dx+b*dy+c*dz
      yt2=yta*dx+ytb*dy+ytc*dz

      if (abs(zt1-zt2) .lt. eps) return
 
      tan1=0.
      tan2=0.
      if (yt2.ne.0.) tan2=(zt2-h2)/yt2
      if (yt1.ne.0.) tan1=(zt1-h1)/yt1
      den=tan1-tan2
      if (abs(den) .lt. .001) return

c          calc effective r-ctr
42    yc=(h2-h1)/den
      zc=h1+tan1*yc
      r=sqrt((yt1-yc)**2+(zt1-zc)**2)
c             convex or concave
43    if (yc.lt.yt1) goto 50
c          convex case.  det rcorn,side,top contact
44    zcn=sbe*(yc-tool(6))+cbe*(zc-tool(2))

      if (zcn.gt.0.) goto 45
c             rcorn contact case
      si=(tool(2)-zc)/(tool(2)+sc(25)+r)
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si*si)
         tn=si/co
      endif
      h=tool(2)+tn*tool(6)
      goto 80
c             side or top case
C             ZCN TOP NON-BARREL
45    ZCN=SBE*(YC-hrad)+CBE*(ZC-TOOL(3))
47    IF (ZCN.GT.0.) GOTO 48

C              SIDE CASE  NON-BARREL
      H=ZC+YC*SBE/CBE
      GOTO 80

c              top case
48    si=(zc-tool(3))/(r+sc(25))
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si**2)
         tn=si/co
      endif
      h=tool(3)-tn*tool(1)*.5
      goto 80
c             concave cs.  decide if corn or top
50    r=r-sc(25)
      hyp=r-tool(2)
      dz=zc-tool(2)
      zcn=sbe*(yc-tool(6))+cbe*(zc-tool(2))
      if (zcn.lt.0.) goto 60
      dysq=hyp*hyp-dz*dz
      if (dysq.le.0.) return
      dy=sqrt(dysq)
      yc=tool(6)-dy
      tgap=r-sqrt((tool(3)-zc)*(tool(3)-zc)+(hrad-yc)*(hrad-yc))
c             if tgap neg, go top route
c                rcorn contact
      if (tgap.lt.0.) goto 60
      tn=dz/dy
      h=tool(2)+tn*tool(6)
      goto 80
c             top contact
60    si=(tool(3)-zc)/r
      if (si .gt. p99) then
         tn = t99
      else if (si .lt. -p99) then
         tn = -t99
      else
         co=sqrt(1.-si*si)
         tn=si/co
      endif

      h=tool(3)-tn*hrad

80    ifnd = 1

999   return
      end

C***********************************************************************
C*    E_SUBROUTINE     : subroutine wlproj(toler)
C*       General purpose tool-to-surface "wall" projection.
C*       Mostly borrowed from cssurf 
C***********************************************************************
 
      subroutine wlproj(toler)
 
      include 'com4a.com'
      include 'mocom.com'

      real*8 toler

      integer*2 jd(600)
      real*4 ad(300),asc(310)
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      equivalence (d,ad,jd)
      equivalence(ifl(51),ia),(ifl(54),isrf)

      real*4 u,v,ubeg,vbeg,u0,v0,u1,v1,u2,v2,h,h0,h1,h2,dis,dis0,dis1
     x       dis2,dbest,a,b,c,x,y,z,dx,dy,dz,eps

      real*4 hs0(7),hs1(7),hs2(7)
      integer*2 i,ihx,itim,ibest,ifnd

      ihx=6*isrf+9

      h = t(ihx,ia)
      ubeg = t(ihx-2,ia)
      vbeg = t(ihx-1,ia)
      
      x=t(1,ia)
      y=t(2,ia)
      z=t(3,ia)
      a=t(4,ia)
      b=t(5,ia)
      c=t(6,ia)
      itim = 0
      hdel = tool(3)/2.
      dis = 1.e9
      eps = toler*toler

10    continue
c
c..... look point is h distance along ta from te
c
      s(8,isrf) = x+h*a
      s(9,isrf) = y+h*b
      s(10,isrf) = z+h*c
      u = ubeg
      v = vbeg
      call surfpn(u,v,1)
      if (ifl(2) .gt. 0) goto 80

      if (itim .eq. 0) then

        if (hdel .lt. 2.*toler) goto 99
        do i=1,7
          hs0(i) = s(i,isrf)
        enddo
        h0 = h
        u0 = u
        v0 = v
        
        dx = s(8,isrf) - s(5,isrf)
        dy = s(9,isrf) - s(6,isrf)
        dz = s(10,isrf) - s(7,isrf)
        dis0 = dx*dx + dy*dy + dz*dz

        h = h0 + hdel
        if (h .gt. tool(3)) h = tool(3)

        itim = 1
        goto 10
      else if (itim .eq. 1) then

        do i=1,7
          hs1(i) = s(i,isrf)
        enddo

        h1 = h
        u1 = u
        v1 = v
        dx = s(8,isrf) - s(5,isrf)
        dy = s(9,isrf) - s(6,isrf)
        dz = s(10,isrf) - s(7,isrf)
        dis1 = dx*dx + dy*dy + dz*dz

        h = h0 - hdel
        if (h .lt. tool(2)) h = tool(2)

        itim = 2
        goto 10
      else if (itim .eq. 2) then

        do i=1,7
          hs2(i) = s(i,isrf)
        enddo

        h2 = h
        u2 = u
        v2 = v
        dx = s(8,isrf) - s(5,isrf)
        dy = s(9,isrf) - s(6,isrf)
        dz = s(10,isrf) - s(7,isrf)
        dis2 = dx*dx + dy*dy + dz*dz

        call wlavgh (hs1,hs2,h1,h2,h,ifnd,eps)

        if (ifnd .eq. 1) then 
          itim = 3
          goto 10
        endif
      else
        dx = s(8,isrf) - s(5,isrf)
        dy = s(9,isrf) - s(6,isrf)
        dz = s(10,isrf) - s(7,isrf)
        dis = dx*dx + dy*dy + dz*dz
      endif
        
80    continue
      if (ifl(2) .gt. 0) then
        if (itim .eq. 0) goto 999
        itim = itim - 1
        ifl(2) = 0
      endif

      ibest = 0
      dbest = dis0

      if (itim .gt. 0 .and. dis1 .lt. dbest-eps) then
        ibest = 1
        dbest = dis1
      endif

      if (itim .gt. 1 .and. dis2 .lt. dbest-eps) then
        ibest = 2
        dbest = dis2
      endif

      if (itim .gt. 2 .and. dis .le. dbest+eps) goto 99

      if (ibest .eq. 1) then
        h = h1
        u = u1
        v = v1
        do i=1,7
          s(i,isrf) = hs1(i)
        enddo
      else if (ibest .eq. 2) then
        h = h2
        u = u2
        v = v2
        do i=1,7
          s(i,isrf) = hs2(i)
        enddo
      else
        h = h0
        u = u0
        v = v0       
        do i=1,7
          s(i,isrf) = hs0(i)
        enddo
      endif

99    continue

      t(ihx,ia) = h
      t(ihx-2,ia) = u
      t(ihx-1,ia) = v


999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine wlcvpv (px,py,pz,vx,vy,vz)
C*       SOLVES A POINT/VECTOR FOR THIS CV PER CURRENT TOOL LOC.
C*       Mostly borrowed from ucrvpv
C*     PARAMETERS
C*       INPUT  :
C*          iclsd     - the "closed" flag
C*       OUTPUT :
C*          px,py,pz  - curve point
C*          vx,vy,vz  - curve vector
C*     RETURNS      : none
C*     SIDE EFFECTS : none
C*     WARNINGS     : none
C*********************************************************************

      subroutine wlcvpv (px,py,pz,vx,vy,vz,iclsd)
 
      include 'com4a.com'
      include 'mocom.com'

      real*4 px,py,pz,vx,vy,vz
      integer*2 iclsd

      real*8 u,a(3),b(3),fct
      real*4 du0
      integer*2 kd(600),ietype,nwds
      real*4 ad(300)
      equivalence(d,ad,kd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      logical linit
      real*4 herr,vtmp(3)
      integer*2 izro
      data izro /0/
 
      fct = 1.0d0
      du0 = 0.0005

      linit = .true.
c          index to d-tbl
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
c          pt to h-val in tcol(ia) and set misc params
      jtim=0
      lgouck=0
      iknt=0
      ihx=6*isrf+9
      iux=ihx-2
      h=t(ihx,ia)

40    xe=t(1,ia)+t(4,ia)*h
      ye=t(2,ia)+t(5,ia)*h
      ze=t(3,ia)+t(6,ia)*h
      u=t(iux,ia)
      if(u.lt.0.)u=0.
      if(u.gt.1.)u=1.
      itim=0
      ctan=1.

      jsh = 0

      fct = 1.0d0
      linit = .true.

42    call uevcvt(u,isrf,izro,a,b,ifl(2))
      if (ifl(2) .eq. 466) go to 99

c          if here 1000 times, get out     ( safety exit )
      iknt=iknt+1
      if(iknt.gt.1000)goto 58

      den = b(1)**2+b(2)**2+b(3)**2
c              guard against zero divide       7-27-82
      if(abs(den).lt.1.e-8) den=.001

      uerr= (b(1)*(xe-a(1))+b(2)*(ye-a(2))+b(3)*(ze-a(3)))/den
      if(abs(uerr).lt.1.e-4) goto 60

      if(.not.linit)goto 45
      linit = .false.
      du = du0
      if (uerr.lt.0.0) du = -du0
      goto 50
45    continue
c          calc du
      den=oerr-uerr
c          if den small, use old ctan
        if(abs(den).gt.1.e-5) ctan=fct*du/den
c          neg ctan is unreal
        if(ctan.le.0.) ctan=-ctan
        if(ctan.gt.1.) ctan = 1.0
48    du=uerr*ctan
50    continue
c          make sure next u does not exceed 0,1
      if(u+du.gt.1.)du=1.-u
      if(u+du.lt.0.)du=-u
c          update for next iter
      itim=itim+1



      u=u+du
      oerr=uerr
      if(itim.lt.120)goto 52

      if (lgouck.eq.1) then
        u = t(19,ia)
        goto 70
      endif
c
c...   We are not converging. Try successively reducing iteration step size.
c
      if((itim-itim/40*40).eq.0) fct = fct*.75d0
      ctan = ctan*.9d0
      if(itim.lt.240)goto 52
c          iterk failure. funny seg?
      ifl(2)=139
      goto 99
c          if u-chg real, go again.
52    if(abs(du).gt.1.e-5)goto 42

        if (abs(uerr).lt..001) goto 60
        if (u.gt..001.and.u.lt..999) goto 60

      if (iclsd.eq.0) goto 60


      u=1.-u
      linit = .true.
      goto 42
c          curvpv mystery exit
58    ifl(2)=134
      goto 99
c          u found this ept.  check h-value.
60    px=a(1)
      py=a(2)
      pz=a(3)
      herr=t(4,ia)*(px-xe)+t(5,ia)*(py-ye)+t(6,ia)*(pz-ze)
      h=h+herr
      
      herr = abs(herr) 

      if (jtim.gt.9 .or. herr.lt.sc(27)) goto 70
c          allow 10 h-fix
      jtim=jtim+1
      goto 40
c************************  normal exit path. fixup all data
70    continue
      sec=sqrt(b(1)**2+b(2)**2+b(3)**2)
      if(sec.gt.0.)goto 75
c          error.  sec=0.
      ifl(2)=135
      goto 99
75    continue
      
      vx=b(1)/sec
      vy=b(2)/sec
      vz=b(3)/sec


      t(iux,ia)=u
      t(ihx,ia)=h
 
99    return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : wldis (pt0,ta0,pt,ddd)
C*       Finds distance from a point to the tool axis line
C*     PARAMETERS
C*       INPUT  :
C*          pt0, ta0  - tool position
C*          pt        - point in space
C*       OUTPUT :
C*          ddd     distance
C*     RETURNS      : none
C*     SIDE EFFECTS : none
C*     WARNINGS     : none
C*********************************************************************

      subroutine wldis (pt0,ta0,pt,ddd)

      real*8 pt(3),ddd
      real*4 pt0(3),ta0(3)

      real*8 cta,cfw,wi
      integer*2 i

      cta = 0
      ddd = 0
      do i = 1,3
        wi = pt(i) - pt0(i)
        cta = cta + wi*ta0(i)
        ddd = ddd + wi*wi
      enddo

      ddd = ddd - cta*cta

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : wlsf0(pt0,ta0,u0,tol)
C*       Finds a surface point close to the initial tool position
C*     PARAMETERS
C*       INPUT  :
C*          pt0, ta0 - Initial tool position
C*          uu, vv   - Initial surface parameters
C*          tol      - tolerance
C*       OUTPUT :
C*          uu, vv   - surface parameters closer to the tool
C*     RETURNS      : none
C*     SIDE EFFECTS : none
C*     WARNINGS     : none
C*********************************************************************

      subroutine wlsf0(pt0,ta0,uu,vv,tol)
 
      include 'com4a.com'
      include 'mocom.com'

      real*8 tol
      real*4 uu,vv,pt0(3),ta0(3)

      integer*2 isrf
      equivalence (ifl(54),isrf)

      real*8 u,v,umin,vmin,ddd,dmin,eps
      real*8 u0,u1,v0,v1
      real*8 sv(9)
      integer*2 ierr

      if (ifl(330+isrf).eq.1) then
        call gtmmuv(isrf,u0,v0,u1,v1)
        if (u0.gt.u1 .or. v0.gt.v1) return
      else
        u0 = 0
        v0 = 0
        u1 = 1
        v1 = 1
      endif

      if (uu.gt.u1 .or. uu.lt.u0) uu = (u0 + u1)/2.
      if (vv.gt.v1 .or. vv.lt.v0) vv = (v0 + v1)/2.

      umin = uu
      vmin = vv

      call uevsft(umin,vmin,isrf,sv,ierr)
      if (ierr .ne. 0) return

      eps = tol*tol

      call wldis (pt0,ta0,sv,dmin)
      if (dmin .lt. 2*eps) return
      if (dmin .lt. 1.d12) dmin = dmin - eps

      u = u0
20    if (u .gt. u1) goto 90

      v = v0
25    if (v .gt. v1) goto 30

      call uevsft(u,v,isrf,sv,ierr)
      if (ierr .ne. 0) return

      call wldis (pt0,ta0,sv,ddd)

      if (ddd .lt. dmin) then
        dmin = ddd
        umin = u
        vmin = v
      endif        

      v = v + 0.05
      goto 25

30    u = u + 0.05
      goto 20

90    uu = umin
      vv = vmin

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : wlcv0(pt0,ta0,u0,tol)
C*       Finds a curve point close to the initial tool position
C*     PARAMETERS
C*       INPUT  :
C*          pt0, ta0 - Initial tool position
C*          u0       - Initial curve parameter
C*          tol      - tolerance
C*       OUTPUT :
C*          u0       - curve parameter closer to the tool
C*     RETURNS      : none
C*     SIDE EFFECTS : none
C*     WARNINGS     : none
C*********************************************************************

      subroutine wlcv0(pt0,ta0,u0,tol)
 
      include 'com4a.com'
      include 'mocom.com'

      real*8 tol
      real*4 u0,pt0(3),ta0(3)

      integer*2 isrf
      equivalence (ifl(54),isrf)

      real*8 u,umin,ddd,dmin,eps,cta,cfw,wi
      real*8 a(3),b(3)
      integer*2 izro,i
      data izro /0/

      umin = u0
      call uevcvt(umin,isrf,izro,a,b,ifl(2))
      if (ifl(2) .ne. 0) return

      eps = tol*tol

      call wldis (pt0,ta0,a,dmin)
      if (dmin .lt. 2*eps) return
      if (dmin .lt. 1.d12) dmin = dmin - eps

      u = 0.
20    if (u .gt. 1) goto 90
      call uevcvt(u,isrf,izro,a,b,ifl(2))
      if (ifl(2) .ne. 0) return

      call wldis (pt0,ta0,a,ddd)
      if (ddd .lt. dmin) then
        dmin = ddd
        umin = u
      endif        

      u = u + 0.01
      goto 20

90    u0 = umin

      return
      end
