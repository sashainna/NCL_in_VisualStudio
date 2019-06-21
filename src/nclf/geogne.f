c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       geogne.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:06
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1986 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: GEOGNE
C **
C **  LAST REVISION:
C **
C **  PURPOSE OF ROUTINE:  HANDLE PT DIST ALONG A CV. IF THE DISTANCE IS
C **                       POSITIVE, MOVE ALONG THE CURVE IN THE DIRECTION
C **                       IN WHICH THE CURVE WAS BUILT. IF THE DISTANCE
C **                       IS NEGATIVE, MOVE ALONG THE CURVE IN THE
C **                       OPPOSITE DIRECTION TO WHICH IT WAS BUILT.
C **                       IF A START POINT IS GIVEN, PROJECT IT ONTO 
C **                       THE CURVE AND START THERE. OTHERWISE, START AT
C **                       THE BEGINNING OF THE CURVE IF THE DISTANCE IS
C **                       POSITIVE OR THE END OF THE CURVE IF THE
C **                       DISTANCE IS NEGATIVE.
c
c ** aak 05-feb-1998: 
c ** splitted into 2 routines: the new one for NCL > 8.5 and the old one
c ** for NCL < 8.5
c ** The new routine: 
c ** crv is evaluated with a tolerance into a polyline;
c ** then, one finds a point on the polyline at the specified distance;
c ** finally, the point is projected on curve through evaluator.
c
C **                                                                  **
C **********************************************************************
C **********************************************************************
 
      subroutine geogne (w)
 
      include 'com.com'
      include 'mocom.com'
      include 'wrksys.com'
      include 'suvcom.com'
c
c... motion common equivalences
c
      integer*2 ktv(4),ksn(4)
      equivalence (tv,ktv)
      real*8 asn
      equivalence (asn,ksn)

      integer*2 ia,isrf
      equivalence(ifl(51),ia),(ifl(54),isrf)
c
c... local variables
c
      real*8 w(12),p(3),
     *       tol,dis,u,usav,
     *       one/1.d00/,zero/0.d00/,fac/25.4d0/
      real*4 p4(3),v4(3)
      integer*4 nclkey, cvkey
      integer*2 nwds,ietype,idx,iux,ihx,iext
      logical lv84,csusav
c
      lv84 = sc(169).lt.8.499d0

      if (lv84) then
         call geogneo (w)
         return
      endif

      ktv(3) = 3
      ktv(4) = idst
      asn    = sc(10)

      call gtdesc (sc(11),cvkey,nwds,ietype)
c
c...vp 30-apr-97 use evaluator for ncl curve
c
      isrf = 3
      call evstup(cvkey,isrf)
      dis  = sc(12)
c
c... if nearpt is not given, take one of the ends as nearpt;
c... otherwise, get nearpt from Unibase
c
      if (ksn(3).eq.1) then
         iext = 0
         u = zero
         if (dis.lt.zero) u = one
         call uevcvt (u,isrf,ifl(72),p,w(4),ifl(2))
      else
         iext = 1
         call gtentt (sc(13),.true.,nclkey,ietype,p)
      endif
c
c... ifl(264) = 1: units/mm
c... lwrk/ifl(72) = modsys/refsys is in effect
c... convert p back to the basic frame and project it on the curve; 
c... u = u-param of projection
c
      if (ifl(264).eq.1) then
         dis = dis/fac
         call vctmsc (p,p,one/fac)
      endif

      if (lwrk) call conent (p,wrkmx,POINT)
      if (ifl(72) .eq. 1) call conent (p,sc(56),POINT)

      call gettol (tol)
      call ncl_crvpt_atdis (cvkey,0.1*tol,p,dis,w,w(4),u,iext)

      if (lwrk) then
         call conent (w,invwrk,POINT)
         call conent (w(4),invwrk,VECTOR)
      endif

      if (ifl(264).eq.1) call vctmsc (w,w,fac)
c
c... if point is within curve, project on curve through evaluator;
c... if on extension, change sign of tanvc if it's to the left of u=0 end.
c
      if (iext.eq.0) then
c
c... prepare globals for projecting through evaluator
c... csu-stuff is needed to transfer u-param to ucrvpv
c
         idx  = 50*(isrf-1)
         ihx  = 6*isrf+9
         iux  = ihx-2
         ia   = 1
         call fill_array4 (t(4,ia),3,0.)
         t(ihx,ia)   = zero
         d(idx+1) = zero
         d(idx+2) = sc(11)

         csusav = csuv
         usav   = csu
         csuv   = .true.
         csu    = u
         call conv8_4(w,t(1,ia),3)

         call ucrvpv (p4(1),p4(2),p4(3),v4(1),v4(2),v4(3))
         call conv4_8 (p4,w,3)
         call conv4_8 (v4,w(4),3)
         csuv = csusav
         csu  = usav
      else if (iext.eq.1 .and. u.eq.zero) then
         call mnvc (w(4))
      endif

      if (ifl(72) .eq. 1) then
         call conent (w,sc(68),POINT)
         call conent (w(4),sc(68),VECTOR)
      endif
      call unitizevc (w(4))

      if (ksn(2) .ne. 8) then
         call ptentt (POINT,w,nclkey,tv)
         rest = tv
      endif

      return
      end

c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       geogne.f , 13.2
c**    DATE AND TIME OF LAST MODIFICATION
c**       10/9/97 , 11:02:42
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1986 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: GEOGNE
C **
C **  LAST REVISION:
C **
C **  PURPOSE OF ROUTINE:  HANDLE PT DIST ALONG A CV. IF THE DISTANCE IS
C **                       POSITIVE, MOVE ALONG THE CURVE IN THE DIRECTION
C **                       IN WHICH THE CURVE WAS BUILT. IF THE DISTANCE
C **                       IS NEGATIVE, MOVE ALONG THE CURVE IN THE
C **                       OPPOSITE DIRECTION TO WHICH IT WAS BUILT.
C **                       IF A START POINT IS GIVEN, PROJECT IT ONTO 
C **                       THE CURVE AND START THERE. OTHERWISE, START AT
C **                       THE BEGINNING OF THE CURVE IF THE DISTANCE IS
C **                       POSITIVE OR THE END OF THE CURVE IF THE
C **                       DISTANCE IS NEGATIVE.
C **                                                                  **
C **********************************************************************
C **********************************************************************
 
      subroutine geogneo (w)
 
c      include 'com8.com'
      include 'com.com'
 
      integer*2 maxpt, maxddd
      parameter (maxpt=50)
      parameter (maxddd=(maxpt*6+(maxpt+1)/2))

c          motion common equivalences
      real*4 t(30,3)
      equivalence (t,tcol)

      common/dddcom/d(maxddd)
      real*8 d
      real*4 ad(maxddd*2)
      equivalence (d,ad)

      integer*2 nwds,ietype,iwf,iknt,nseg,idel,idx,iux,ihx
      integer*2 ind, iseg, i,itim,idir,imid,jdx,isegk,ixx
      integer*4 nclkey
      real*8 e(9),dis,xm,ym,zm,adis,bdis,ro,holdu,sum,dst
      real*8 u,c1,c2,c3,c4,xp,yp,zp,uerr,oerr,ctan,du
      real*8 w(12),a(3),b(3),q(12),p(3),den
      real*8 xe,ye,ze,ox,oy,oz,dx,dy,dz,delx,dely,delz,osum,v,um,sdel
      real*4 p4x,p4y,p4z
      integer*2 ktv(4),ksn(4)
      equivalence (tv,ktv)
      real*8 asn
      equivalence (asn,ksn)
      integer*2 ia,ib,ic,isrf
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      logical nored,trflg

      trflg=.true.
      nored=.false.
      ktv(3)=3
      ktv(4)=idst
      asn=sc(10)
      iknt=0
      sum=0.
      isrf = 3
      dis=sc(12)
      idir=1
      if (dis.lt.0) then
        idir=-1
        dis=-dis
      endif
 
      call gtdesc(sc(11),nclkey,nwds,ietype)
      call isitwf(nclkey,iwf)
c
c...vp 30-apr-97 use evaluator for ncl curve
c
      if (sc(169) .gt. 8.399d0) iwf = 1
      if (iwf.eq.0) then
        call gtentt(sc(11),trflg,nclkey,ietype,d)
        nseg=ad(1)
      else
        call evstup(nclkey,isrf)
        nseg = 1
      endif
      if (ksn(3).eq.1) goto 100
c      if (iwf.eq.1) goto 9315
c               get nearpt in p
      call gtentt(sc(13),trflg,nclkey,ietype,p)
c                     set current point on cv because if we are starting
c                     at an end point & point is on extension, no iterating
c                     will be done and it wont get set. ijd 12-jun-86
      xp=p(1)
      yp=p(2)
      zp=p(3)
      dst=1.e12
      if (iwf.eq.1) then
        du = 1.d0/9.d0
        v = -du
        u = 0.d0
        do 4 i=1,10
        v=v+du
        if (v.lt.0.0d0) v = 0.0d0
        if (v.gt.1.0d0) v = 1.0d0
        call uevcvt (v,isrf,ifl(72),a,b, ifl(2))
        adis = (p(1)-a(1))**2+(p(2)-a(2))**2+(p(3)-a(3))**2
        if (adis.ge.dst) goto 4
        dst = adis
        u = v
4       continue
        idx = 50*(isrf-1)
        ihx = 6*isrf+9
        iux = ihx-2
        ia = 1
        if (ifl(72) .eq. 1)  call transf (p, sc(56), 3, 3)
        t(1,ia)  = p(1)
        t(2,ia)  = p(2)
        t(3,ia)  = p(3)
        t(4,ia)  = 0.0
        t(5,ia)  = 0.0
        t(6,ia)  = 0.0
        t(iux,ia) = u
        t(ihx,ia) = 0.0
        d(idx+1) = 0.0d0
        d(idx+2) = sc(11)
        call ucrvpv (p4x,p4y,p4z,xm,ym,zm)
        if (ifl(2).gt.0) goto 999
        if (ifl(72) .eq. 1)  then
          p(1) = p4x
          p(2) = p4y
          p(3) = p4z
          call transf (p, sc(68), 3, 3)
          ox = p(1)
          oy = p(2)
          oz = p(3)
        else
          ox = p4x
          oy = p4y
          oz = p4z
        endif
        u = t(iux,ia)
        goto 120
      endif
c          find an initial seg per nearpt.
      ind=nseg-1
      idx=(nseg+1)/2-5
      idel=idx
      iseg=1
c          loop thru segs to find start seg
      do 20 i=1,ind
      idx=idx+6
      jdx=2*idx+5
c          check nrpt against cvpts and approx midpts
      xm=(d(idx)+ad(jdx))*.666667 +d(idx+6)*.333333
      ym=(d(idx+1)+ad(jdx+1))*.666667+d(idx+7)*.333333
      zm=(d(idx+2)+ad(jdx+2))*.666667+d(idx+8)*.333333
      adis=(p(1)-xm)**2+(p(2)-ym)**2+(p(3)-zm)**2
      bdis=(p(1)-d(idx))**2+(p(2)-d(idx+1))**2+(p(3)-d(idx+2))**2
      if(bdis.lt.adis)adis=bdis
      if(adis.ge.dst)goto 20
      dst=adis
      iseg=i
20    continue
 
c          activate seg(iseg). use delta format
30    idx=idel+6*iseg-1
      isegk=isegk+1
      ixx=idx+6
      jdx=2*idx+6
      ro=ad(jdx+6)
      do 35 i=1,3
      e(i)=ad(jdx+i)
      e(i+6)=d(ixx+i)-d(idx+i)
35    e(i+3)=e(i+6)-ro*ad(jdx+i+12)
 
c          nrpt in segsys
      xe=p(1)-d(idx+1)
      ye=p(2)-d(idx+2)
      ze=p(3)-d(idx+3)
c          approx 1st u per nrpt
      u=(xe*e(7)+ye*e(8)+ze*e(9))/(e(7)**2+e(8)**2+e(9)**2)
      if(u.lt.0.)u=0.
      if(u.gt.1.)u=1.
      ctan=1.
      itim=0

42    c1=(1.-u)**2
      c2=2.*u*(1.-u)
      c3=u**2
c          if here 100 times, get out     ( safety exit )
      iknt=iknt+1
      if(iknt.gt.100)goto 9315
      do 44 i=1,3
      a(i)=c2*e(i)+c3*e(i+3)
44    b(i)=c1*e(i)+c2*e(i+3)+c3*e(i+6)-a(i)
      den= b(1)**2+b(2)**2+b(3)**2
c              guard against zero divide       7-27-82
      if(abs(den).lt.1.e-8) den=.001
      uerr=((b(1)*(xe-a(1))+b(2)*(ye-a(2))+b(3)*(ze-a(3)))/den-u)/3.
      if(abs(uerr).lt.1.e-6) goto 60
      if(itim.eq.0)goto 50
c          calc du
      den=oerr-uerr
c          if den small, use old ctan
      if(abs(den).gt.1.e-3) ctan=du/den
c          neg ctan is unreal
      if(ctan.le.0.)ctan=1.
50    du=uerr*ctan
c          make sure next u does not exceed 0,1
      if(u+du.gt.1.)du=1.-u
      if(u+du.lt.0.)du=-u
c          update for next iter
      itim=itim+1
      u=u+du
      oerr=uerr
      if(itim.gt.50)goto 9315
c          if u-chg real, go again.
52    if(abs(du).gt.1.e-5)goto 42
c          stall-out.  probably 0,1 excess case this seg
c           if uerr small, accept it and go finup
      if(abs(uerr).le..002)goto 60
c          set next iseg per this u-case
      iseg=iseg+1
      if(u.lt..5)iseg=iseg-2
      if(iseg.gt.0.and.iseg.lt.nseg)goto 30
c          must exit now ( all routes have been tried ).
      if (iseg.lt.1) iseg=1
      if (iseg.ge.nseg) iseg=nseg-1

60    ox=a(1)+u*b(1)+d(idx+1)
      oy=a(2)+u*b(2)+d(idx+2)
      oz=a(3)+u*b(3)+d(idx+3)

      goto 120
c                        we start here if no startpoint was given.
c                        if dis is positive, start at beginning of cv
c                        if dis is negative, start at end of cv
100   continue
      iseg=1
      u=0.
      idx=(nseg+1)/2
      if (idir.gt.0.) goto 115
      iseg=nseg-1
      u=1.
      idx=(nseg+1)/2+iseg*6

c                       save cv end point (idx set above)
115   continue
      if (iwf.eq.1) call uevcvt (u,isrf,ifl(72),d(idx+1),b, ifl(2))
      ox=d(idx+1)
      oy=d(idx+2)
      oz=d(idx+3)

c                       get segment iseg into array q
120   continue
      du=.01*idir
      if (iwf.eq.1) goto 130
      idx=(nseg+1)/2+(iseg-1)*6
      jdx=idx*2+6
      ro=ad(jdx+6)
      q(1)=d(idx+1)
      q(2)=d(idx+2)
      q(3)=d(idx+3)
      q(4)=q(1)+ad(jdx+1)
      q(5)=q(2)+ad(jdx+2)
      q(6)=q(3)+ad(jdx+3)
      q(10)=d(idx+7)
      q(11)=d(idx+8)
      q(12)=d(idx+9)
      q(7)=q(10)-ad(jdx+13)*ro
      q(8)=q(11)-ad(jdx+14)*ro
      q(9)=q(12)-ad(jdx+15)*ro

130   imid=1
c                             if the next iteration will take us past 
c                             the end of the cv, reduce u step size to
c                             get exactly to end.
      holdu=u+du*2
      if (holdu.ge.0.and.holdu.le.1.) goto 140
c                                               near enough to end of 
c                                               this seg. go load next.
      if (u.lt.1.d-4.or.u.gt..9999d0) goto 135
      du=(1.-u)/2.
      if (idir.eq.-1) du=-u/2.
      goto 140

135   iseg=iseg+idir
      if (iseg.lt.1.or.iseg.gt.nseg-1) goto 136
      u=0.
      if (idir.eq.-1)u=1.
      goto 120

c                       we ran off the end of the curve - project
c                       point along extension of curve.
c                       get slope of curve at end point in array a.
136   if (idir.eq.1) goto 137
      if (iwf.eq.1) then
        u = 0.0d0
        call uevcvt (u,isrf,ifl(72),a,w(4), ifl(2))
        w(4) = -w(4)
        w(5) = -w(5)
        w(6) = -w(6)
      else
        w(4)=q(1)-q(4)
        w(5)=q(2)-q(5)
        w(6)=q(3)-q(6)
      endif
      goto 138

137   continue
      if (iwf.eq.1) then
        u = 1.0d0
        call uevcvt (u,isrf,ifl(72),a,w(4), ifl(2))
      else
        w(4)=q(10)-q(7)
        w(5)=q(11)-q(8)
        w(6)=q(12)-q(9)
      endif
c                                         unitize slope vector
138   den=dsqrt(w(4)**2+w(5)**2+w(6)**2)
      if (den.lt.1.d-8) goto 160
      w(4)=w(4)/den
      w(5)=w(5)/den
      w(6)=w(6)/den
c                                        project remaining distance 
c                                        (dis-sum) along slope vector.
      adis=dabs(dis-sum)
      w(1)=xp+adis*w(4)
      w(2)=yp+adis*w(5)
      w(3)=zp+adis*w(6)
      if (ksn(2) .eq. 8) goto 999
      goto 900

c                   get 2 points on cv and calc arc length thru them.
140   u=u+du
      if (iwf.eq.1) then
        call uevcvt (u,isrf,ifl(72),a,b, ifl(2))
        xp = a(1)
        yp = a(2)
        zp = a(3)
      else
        um=1.-u
        c1=um**3
        c2=3.*u*um**2
        c3=3.*u**2*um
        c4=u**3
        xp=c1*q(1)+c2*q(4)+c3*q(7)+c4*q(10)
        yp=c1*q(2)+c2*q(5)+c3*q(8)+c4*q(11)
        zp=c1*q(3)+c2*q(6)+c3*q(9)+c4*q(12)
      endif
      if (imid.eq.0) goto 150
      xm=xp
      ym=yp
      zm=zp
      imid=0
      goto 140

150   dx=xp-ox
      dy=yp-oy
      dz=zp-oz
      den=dx**2+dy**2+dz**2
      v=(dx*(xm-ox)+dy*(ym-oy)+dz*(zm-oz))/den
      delx=ox+v*dx-xm
      dely=oy+v*dy-ym
      delz=oz+v*dz-zm
      sdel=dsqrt(dx**2+dy**2+dz**2+5.3334d0*(delx**2+dely**2+delz**2))
      osum=sum
      sum=sum+sdel
      ox=xp
      oy=yp
      oz=zp
      if (sum.lt.dis) goto 130
c                               the sum has exceeded the req'd distance.
c                               proportion u between old sum and this sum.
      holdu=u-du*2
      if (holdu.lt.0.)holdu=0.
      if (holdu.gt.1.)holdu=1.
      u=(dis-osum)*(u-holdu)/(sum-osum)+holdu
c                               calculate point on cv at this u value.
160   continue
      if (iwf.eq.1) then
        call uevcvt (u,isrf,ifl(72),w,w(4), ifl(2))
      else
        um=1.-u
        c1=um**3
        c2=3.*u*um**2
        c3=3.*u**2*um
        c4=u**3
        w(1)=c1*q(1)+c2*q(4)+c3*q(7)+c4*q(10)
        w(2)=c1*q(2)+c2*q(5)+c3*q(8)+c4*q(11)
        w(3)=c1*q(3)+c2*q(6)+c3*q(9)+c4*q(12)
c
c...get slop vector if in declpv call
c
        if (ksn(2) .eq. 8) then
          c1 = 0. - um*um
          c2 = um * (1. - 3.*u)
          c3 = u * (2. - 3.*u)
          c4 = u * u
          w(4) = c1*q(1)+c2*q(4)+c3*q(7)+c4*q(10)
          w(5) = c1*q(2)+c2*q(5)+c3*q(8)+c4*q(11)
          w(6) = c1*q(3)+c2*q(6)+c3*q(9)+c4*q(12)
        endif
      endif
      if (ksn(2) .eq. 8) then
          dis  = dsqrt (w(4)**2+w(5)**2+w(6)**2)
          w(4) = w(4) / dis
          w(5) = w(5) / dis
          w(6) = w(6) / dis
          go to 999
      end if

c          geosto this item
900   ietype = 3
      call ptentt(ietype,w,nclkey,tv)
      rest=tv
      goto 999

c                      failed to project start point to curve
9315  ifl(2)=315
      goto 990

990   err=.true.
      tv=0.
      rest=0.
 
999   return
      end
