c********************************************************************
c*    name         :  geognd.f
c*       contains:
c*    copyright 1984 (c) mills data system inc.  all rights reserved.
c*    MODULE NAME AND RELEASE LEVEL
c*       geognd.f , 25.1
c*    DATE AND TIME OF LAST  MODIFICATION
c*       04/29/15 , 15:10:06
c********************************************************************
c
c*********************************************************************
c*   e_subroutine     : subroutine geognd
c*   handle those defs which use curve.
c*   Golden section search from, "Optimum Seeking Methods" by Douglass
c*     Wilde, Prentice-Hall, Englewood Cliffs, NJ, 1944, pp. 10 - 35
c*        pt/9     point/intof,pl1,cv1,pt1      [nearpt]
c*        pt/11    point/intof,ci1,cv1,pt1         "
c*        ln/10    ln/pt1,tanto<perpto>,cv1,pt2    "
c*    parameters   
c*       input  : 
c*          none
c*       output :  
c*          none
c*    returns      : none
c*    side effects : none
c*    warnings     : only for Ver 9.05 and greater
c********************************************************************

      subroutine geognd

      include 'com8a.com'
      include 'mocom.com'

      integer*2 ktv(4),ksn(4), ln10, iredef, npts
      real*8 dis,adis,xp,yp,zp,fx,fy,hfx,f_dist
      real*8 asn, cvasw,dx,dy,sec,ubeg,uend,valmin,hldmin,
     *       f_mag,uer,uerbeg,uerend,du,
     *       w(12),a(3),b(3),pgelnw,q(12),p(6),mmorin

      equivalence (tv,ktv)
      equivalence (asn,ksn)

      integer*4 nclkey, keycrv
      integer*2 nwds, ietype, iclsd, jup, jdn, jupwrp, jdnwrp
      integer*2 ia,isrf
      equivalence(ifl(51),ia),(ifl(54),isrf)
      logical trflg, found
c
c...  dec'ls for Golden section search
c
      integer*4 ncheck,nn,itmax,its
      real*8 gsm,agsm,bgsm,aterm,bterm,vala,valb,slen,root,gstol
      real*8 errstar, ustar,uercalc

      integer*2 imin,jmin,itim,jtim,jbar

      if (sc(169).lt.9.0499d0) then
        call geogndo
        goto 999
      endif

      if (ifl(264).eq.1) then
c                 units = millimeters
          mmorin = 25.4
      else
          mmorin = 1.
      endif
 
      trflg = .true.
      found = .false.
      ln10=0
      ktv(4)=idst
      asn=sc(10)
      iredef=0
      isrf = 3
      ifl(2) = 0

c     select point intersection or line perto/tanto
c       and Golden convergence tolerances for each case
      if(idst.eq.3) then
c        pt/9&11:machining tolerance
        gstol = sc(167)
        goto 20
      else
c        ln/10:  0.00001 implies 0.0006 deg on either side of pi/4
        gstol = 0.00001
      endif

c********************************** ln/10
      ktv(3)=6
      ln10=2
      if(sc(12).eq.646.) ln10=1
c              get pt1 in w
      pgelnw=sc(11)
      call gtentt(pgelnw, trflg, nclkey, ietype, w(1))
      cvasw=sc(13)
      goto 41

c************************************ pt9   get pl1,cv1,nearpt
20    pgelnw=sc(11)
c     get pl or ci into q              2-may-83
      if(ksn(2).eq.7)goto 30
      if(ksn(2).ne.10)goto 990
      call gtentt(pgelnw, trflg, nclkey, ietype, q(1))
c     circle may not be tipped
      if (q(4)**2+q(5)**2.lt.1.d-5) goto 40
      ifl(2)=161
      goto 990

30    call gtdesc (pgelnw, nclkey, nwds, ietype)
      if (ietype.ne.5) goto 35
c     entity in sc(11)  is a line. (redef)  4-feb-87
c     get the line and convert to plane in q-tbl
      iredef=1 
      call gtentt(pgelnw, trflg, nclkey, ietype, q(1))
      p(1) = q(1)
      p(2) = q(2)
      p(3) = q(3)
      dx= -q(5)
      dy=  q(4)
      sec=dsqrt(dx**2+dy**2)
      if(sec.gt..0001) goto 32

c     error. line is vertical
      ifl(2)=44
      goto 990

32    dx=dx/sec
      dy=dy/sec
      q(4)=dx*q(1)+dy*q(2)
      q(3)=0.
      q(1)=dx
      q(2)=dy
      goto 40

35    call gtplt(pgelnw, ifl(72), q(1))
40    cvasw=sc(12)
41    continue
      call gtdesc(cvasw,keycrv,nwds,ietype)
c
c...vp 29-apr-97
c...evaluate ncl curve like bspline using evaluator
c
      call evstup(keycrv,isrf)
      npts = 2
c
c.... get nearpt in p
c
      pgelnw=sc(13)
      if(idst.ne.3) pgelnw=sc(14)
      call gtdesc(pgelnw,nclkey,nwds,ietype)
      if (nclkey.gt.0) call gtentt(pgelnw, trflg, nclkey, ietype, p(1))

c
c.... number of sub-intervals to search
c
      ncheck = 21
      nn = ncheck - 1
      imin = nn/2
c
c..... find an initial seg per nearpt
c
      dis=1.e12
      if (nclkey.gt.0 .and. (ietype.eq.3.or.ietype.eq.21)) then
        du = 1.0d0/nn
        u = 0.d0
        do 45 i=0,nn,1
          if (u .gt. 1.) u = 1.d0
          call uevcvt (u,isrf,ifl(72),a,b, ifl(2))
          if (ifl(2) .gt. 0) goto 990
          adis = f_dist(p,a)
          if (adis.lt.dis) then
            dis = adis
            imin = i
          endif
          u=u+du
45      continue
      endif

      call gtclsd (keycrv, 0, iclsd)
      if (iclsd.eq.1) then
        jupwrp = nn
        jdnwrp = 0
      else
        jupwrp = 0
        jdnwrp = nn
      endif
c
c.....Golden section search on pl1 or ci1 
c.....initialize search variables
c

c
c.... set up gridded search
c
      du = 1.0/nn
      jup    = imin   - 1
      jdn    = imin
c
c.... number of Golden section search evaluations
c
      itmax = 32
      iend = 0
      uend = 0.0d0
      uerend = uercalc(ln10,ksn(2),uend,q,w,xp,yp,zp,fx,fy)
      if (ifl(2).eq.163) goto 990 

c     Golden section search parameters, see Wilde above for doc
c     Golden section mean gsm = 1.618
      gsm = (1.0+dsqrt(5.0d0))/2.0
c     agsm = Golden search interval agsm = .382
      agsm = 1.0 - 1.0/gsm
c     bgsm = Golden search interval bgsm = .618
      bgsm = 1.0/gsm
c     initialize objectives (for minimization)
      vala = 1.0E5
      valb = 1.0E5
c
c...  check each sub-interval for zero
c
      do 47 itim = 1,nn*2,1
c       
c..... the order in which segments are considered is defined 
c..... by their distance from the near point
c........Step down one on even passes
c
        jtim   = itim   / 2
        if (2*jtim .eq. itim) then
            ibeg   = jdn    - 1
            if (ibeg .lt. 0) then
              if (iclsd .eq. 0) goto 47
              ibeg = jupwrp
            endif
            jdn    = ibeg
c
c........Step up on odd passes
c
        else
            ibeg   = jup    + 1
            if (ibeg .gt. nn) then
              if (iclsd .eq. 0) goto 47
              ibeg = jdnwrp
            endif
            jup    = ibeg
        endif

        ubeg = ibeg * du
        uerbeg = uerend
        iend = ibeg + 1
        uend = iend * du
        if (uend.gt.1.0) uend = 1.0

        uerend = uercalc(ln10,ksn(2),uend,q,w,xp,yp,zp,fx,fy)

c        sign change implies a root in this interval
c          use Golden section search to find the min or max
c          of a unimodal function, see Wilde for details
c
c..... Eduard 3/15/00 In other words, if uerbeg and uerend are of 
c..... the same sign, we could skip this segment. At the moment 
c..... we check it anyway.
c
          its = 0
          do while (its.lt.itmax)
c
c..... Note that the rate of convergence is constant, i.e., the next
c..... slen is appr. 1.618 times smaller than the previous one.
c..... Going until its=32 means going until slen is appr. 1.e-8
c
            slen = uend - ubeg
c            convergence test - return root at best point
            valmin = dmin1(vala,valb)
            if (valmin.lt.gstol .and.
     x         (.not.found .or. valmin.lt.hldmin)) then
              if (vala.lt.valb) then
                root = aterm
              else
                root = bterm
              endif 
c              we're done!
              hldmin = valmin
              found = .true.
            endif
           
c
c..... If the interval is too small go to next itim even if
c..... itmax is not reached
c

c            new evaluation points
            aterm = ubeg + agsm*slen
            bterm = uend - agsm*slen

c            build the last two experiments (Wilde)
c            absolute value of error => U shaped unimodal function
            vala = dabs(uercalc(ln10,ksn(2),aterm,q,w,xp,yp,zp,fx,fy))
            valb = dabs(uercalc(ln10,ksn(2),bterm,q,w,xp,yp,zp,fx,fy))

c            isolate interval containing minimum
            if (vala .lt. valb) then
              uend = bterm
            else if (vala .eq. valb) then
              ubeg = aterm
              uend = bterm
            else
              ubeg = aterm
            endif

c            bump the iteration counter
            its = its+1

c          end of golden section search for root
          enddo

c        end of search for root inside sub-interval

c     end of root enumeration
        if (found) goto 50
47    continue
      goto 989
50    continue

c     update state of current solution
      ustar = root
      errstar = uercalc(ln10,ksn(2),ustar,q,w,xp,yp,zp,fx,fy)

c     store the geometric entity found
90    if(idst.eq.3) goto 95
      w(4)=xp-w(1)
      w(5)=yp-w(2)
      w(6)=0.
c
c... if line shorter than .010, make it unit length.  13-jan-84
c... aak 15-may-1998: added factor "mmorin" into the next "if"
c
      if (f_mag(w(4)).le.0.01*mmorin) then
         w(4) =  fy*mmorin
         w(5) = -fx*mmorin
      endif

      goto 900

95    w(1)=xp
      w(2)=yp
      w(3)=zp

c     geosto this item
900   ietype = ktv(4) 

c     if redef of a line, put pt in sc(15-17) and exit.  4-feb-87
      if(iredef.eq.0) goto 902
      sc(15)=xp
      sc(16)=yp
      sc(17)=zp
      goto 999
901   continue

902   call ptentt(ietype, w, nclkey, tv)
      rest=tv
      goto 999
c
c...   Curve does not intersect something.....raz
c...    331 = FAILED TO FIND PLANE-CURVE INTERSECTION
c...    417 = FAILED TO FIND CIRCLE-CURVE INTERSECTION
c...    418 = CAN NOT PROJECT POINT ONTO CURVE
c
989   if (idst.eq.3 .and. ksn(2).eq.7) ifl(2)=331 
      if (idst.eq.3 .and. ksn(2).eq.10) ifl(2)=417 
      if (idst.eq.5 .and. ksn(2).eq.10) ifl(2)=418

c     error exit. set err and zero tv & 'rest'
990   err=.true.
      if (ifl(2).eq.0) ifl(2)=5
      tv=0.
      rest=0.

999   return
      end


c*********************************************************************
c*   e_subroutine     : real*4 function uercalc
c*   compute signed error at a given u-value for these cases (uer)
c*        pt/9     point/intof,pl1,cv1,pt1      [nearpt]
c*        pt/11    point/intof,ci1,cv1,pt1         "
c*        ln/10    ln/pt1,tanto<perpto>,cv1,pt2    "
c*    parameters   
c*       input  : 
c*          none
c*       output :  
c*          none
c*    returns      : uer
c*    side effects : none
c*    warnings     : only for Ver 9.05 and greater
c********************************************************************/


      real*8 function uercalc(
     1        ln10,ksn2,
     2        uval,
     3        q,w,xp,yp,zp,fx,fy)

c     implicit none

      include 'com8a.com'

      integer*2 ln10
      real*8 uval, q(1), w(1), fx, fy
      real*8 a(3), b(3), mag1, mag2
      real*8 uer, xp, yp, zp
      integer*2 ia,isrf

      equivalence(ifl(51),ia),(ifl(54),isrf)

c     initialize 
      uer = 0.0
      isrf = 3

      call uevcvt (uval,isrf,ifl(72),a,b,ifl(2))
      xp = a(1)
      yp = a(2)
      zp = a(3)
      fx = b(1)
      fy = b(2)

      if (idst.eq.3) then
c       point def'n
c       calc uer for ci1 or pl1  (if circle, ignore z)
        if (ksn2.eq.10) then
c            circle equation error
          uer=sqrt((q(1)-xp)**2+(q(2)-yp)**2)-q(7)
        else
c          plane equation error
          uer=q(4)-q(1)*xp-q(2)*yp-q(3)*zp
        endif
      else
c        line def'n
c       check for vertical tangent - all ln/10 defs in z-plane
        sec=dsqrt(fx**2+fy**2)
        if(sec.lt.1.0d-6) then
c         error. cv tanvec appears vertical, z-dircos .eq.1
          ifl(2)=163
          goto 990
        endif

c        normalize tangent/normal
        fx = fx/sec
        fy = fy/sec

c       if not vertical tanvec define ln10
        if (ln10.ne.2) then
c         tanto case => flip fx, fy
          hfx =  fx
          fx  = -fy
          fy  =  hfx
        endif

c        compute distance squared from pt1 to test point
c       uer = dsqrt( (w(1)-xp)**2 + (w(2)-yp)**2 )
c
c..... the error function above was wrong for the line definitions
c..... replaced with the old one (from geogndo)
c
        uer=fx*(w(1)-xp)+fy*(w(2)-yp)
      endif

c     normal and error exit - errors handled by caller
990   continue

c     return signed error value
      uercalc =  uer

      end
c*********************************************************************
c*    e_subroutine     : subroutine geogndo
c*   old version of geognd
c*   handle those defs which use curve.
c*        pt/7     point/intof,pl1,cv1,pt1   (nearpt)
c*        pt/10    point/intof,ci1,cv1,pt1      "
c*       ln/10    ln/pt1,tanto<perpto>,cv1,pt2
c*    parameters   
c*       input  : 
c*          none
c*       output :  
c*          none
c*    returns      : none
c*    side effects : none
c*    warnings     : none
c********************************************************************/

      subroutine geogndo

      include 'com8a.com'
      include 'mocom.com'

      integer*2 maxpt, maxddd
      parameter (maxpt=50)
      parameter (maxddd=(maxpt*6+(maxpt+1)/2))

      common/dddcom/dd(maxddd)
      real*8 dd
      real*4 ad(maxddd*2)
      equivalence (dd,ad)

      integer*2 ktv(4),ksn(4), ln10, isegk, iredef, iwf, i, npts,
     *          idx, ind, idel, iseg, jdx, ixx, itim
      real*4 e(9),atol,dis,xm,ym,zm,adis,bdis,ro,
     *       xn,yn,zn,c2,c3,c4,xp,yp,zp,uer,oerr,utan,du
      real*8 asn, cvasw, dx,dy,sec, u, v, b1, b2, b3, fx, fy, hfx,
     *       f_mag,
     *       w(12),a(3),b(3),pgelnw,q(12),p(6),duer,den,delu,mmorin

      equivalence (tv,ktv)
      equivalence (asn,ksn)

      integer*4 nclkey, keycrv
      integer*2 nwds, ietype, iclsd
      integer*2 ia,isrf
      equivalence(ifl(51),ia),(ifl(54),isrf)
      logical trflg

      if (ifl(264).eq.1) then
c                 units = millimeters
          mmorin = 25.4
      else
          mmorin = 1.
      endif
 
      trflg = .true.
      ln10=0
      ktv(4)=idst
      asn=sc(10)
      atol=sc(27)/10.
      utan=1.
      isegk=0
      iredef=0
      isrf = 3

c              pt/7,10  or  ln/10
      if(idst.eq.3)goto 20
c********************************** ln/10
      ktv(3)=6
      ln10=2
      if(sc(12).eq.646.) ln10=1
c              get pt1 in w
      pgelnw=sc(11)
      call gtentt(pgelnw, trflg, nclkey, ietype, w(1))
      cvasw=sc(13)
      goto 41
c************************************ pt7   get pl1,cv1,nearpt
20    pgelnw=sc(11)
c              get pl or ci into q              2-may-83
      if(ksn(2).eq.7)goto 30
      if(ksn(2).ne.10)goto 990
      call gtentt(pgelnw, trflg, nclkey, ietype, q(1))
c              circle may not be tipped
      if(q(4)**2+q(5)**2.lt.1.d-5)goto 40
      ifl(2)=161
      goto 990
30    call gtdesc (pgelnw, nclkey, nwds, ietype)
      if (ietype.ne.5)goto 35
c                entity in sc(11)  is a line. (redef)  4-feb-87
c                get the line and convert to plane in q-tbl
      iredef=1 
      call gtentt(pgelnw, trflg, nclkey, ietype, q(1))
      p(1) = q(1)
      p(2) = q(2)
      p(3) = q(3)
      dx=-q(5)
      dy=q(4)
      sec=dsqrt(dx**2+dy**2)
      if(sec.gt..0001)goto 32
c               error. line is vertical
      ifl(2)=44
      goto 990
32    dx=dx/sec
      dy=dy/sec
      q(4)=dx*q(1)+dy*q(2)
      q(3)=0.
      q(1)=dx
      q(2)=dy
      goto 40
35    call gtentt(pgelnw, trflg, nclkey, ietype, q(1))
40    cvasw=sc(12)
41    continue
      call gtdesc(cvasw,keycrv,nwds,ietype)
      call isitwf(keycrv,iwf)
c
c...vp 29-apr-97
c...evaluate ncl curve like bspline using evaluator
c
      if (sc(169) .gt. 8.399d0) iwf = 1
      if (iwf.eq.0) then
        call gtentt(cvasw, trflg, keycrv, ietype, dd(1))
        npts=ad(1)
      else
        call evstup(keycrv,isrf)
        npts = 2
        iseg = 1
      endif
c               get nearpt in p
      pgelnw=sc(13)
      if(idst.ne.3)pgelnw=sc(14)
      call gtdesc(pgelnw,nclkey,nwds,ietype)
      if (nclkey.gt.0) call gtentt(pgelnw, trflg, nclkey, ietype, p(1))

c          find an initial seg per nearpt
      dis=1.e12
      if (iwf.eq.1) then
        du = 1.d0/9.d0
        v = 0.d0
        u = 0.d0
        do 45 i=1,10
        if (v.gt.1.0d0) v = 1.0d0
        call uevcvt (v,isrf,ifl(72),a,b, ifl(2))
        adis = (p(1)-a(1))**2+(p(2)-a(2))**2+(p(3)-a(3))**2
        if (adis.lt.dis) then
          dis = adis
          u = v
        endif
        v=v+du
45      continue
        goto 65
      endif
      ind=npts-1
      idx=(npts+1)/2-5
      idel=idx
      iseg=1
c          loop thru segs to find start seg
      do 50 i=1,ind
      idx=idx+6
      jdx=2*idx+5
c          check nrpt against cvpts and approx midpts
      xm=(dd(idx)+ad(jdx))*.666667 +dd(idx+6)*.333333
      ym=(dd(idx+1)+ad(jdx+1))*.666667+dd(idx+7)*.333333
      zm=(dd(idx+2)+ad(jdx+2))*.666667+dd(idx+8)*.333333
      adis=(p(1)-xm)**2+(p(2)-ym)**2+(p(3)-zm)**2
      bdis=(p(1)-dd(idx))**2+(p(2)-dd(idx+1))**2+(p(3)-dd(idx+2))**2
      if(bdis.lt.adis)adis=bdis
      if(adis.ge.dis)goto 50
      dis=adis
      iseg=i
50    continue

c          activate seg(iseg)
55    idx=idel+6*iseg-1
      isegk=isegk+1
      ixx=idx+6
      jdx=2*idx+6
      ro=ad(jdx+6)
      do 60 i=1,3
      e(i)=ad(jdx+i)
      e(i+6)=dd(ixx+i)-dd(idx+i)
60    e(i+3)=e(i+6)-ro*ad(jdx+i+12)

c          nrpt in segsys
      xn=p(1)-dd(idx+1)
      yn=p(2)-dd(idx+2)
      zn=p(3)-dd(idx+3)

c          approx 1st u per nrpt
      u=(xn*e(7)+yn*e(8)+zn*e(9))/(e(7)**2+e(8)**2+e(9)**2)

65    if(u.lt.0.)u=0.
      if(u.gt.1.)u=1.

c          converge on pl1   or ci1
      itim=0
70    if (iwf.eq.1) then
        call uevcvt (u,isrf,ifl(72),a,b, ifl(2))
        xp = a(1)
        yp = a(2)
        zp = a(3)
        fx = b(1)
        fy = b(2)
      else
        c2=3.*u*(1.-u)**2
        c3=3.*u**2*(1.-u)
        c4=u**3
        xp=c2*e(1)+c3*e(4)+c4*e(7)+dd(idx+1)
        yp=c2*e(2)+c3*e(5)+c4*e(8)+dd(idx+2)
        zp=c2*e(3)+c3*e(6)+c4*e(9)+dd(idx+3)
      endif
c               if ln10, do cv tanvec (xy only) and uer
      if(idst.eq.3)goto 703
      if (iwf.eq.0) then
        b3=u**2
        b2=2.*u-3.*b3
        b1=1.-4.*u+3.*b3
        fx=b1*e(1)+b2*e(4)+b3*e(7)
        fy=b1*e(2)+b2*e(5)+b3*e(8)
      endif
      sec=dsqrt(fx**2+fy**2)
      if(sec.ge.1.d-6)goto 701
c        error.  cv tanvec appears vertical.
      ifl(2)=163
      goto 990
701   fx=fx/sec
      fy=fy/sec
      if(ln10.eq.2)goto 702
c                tanto. flip fx,y
      hfx=fx
      fx=-fy
      fy=hfx
702   uer=fx*(w(1)-xp)+fy*(w(2)-yp)
      goto 706
c             calc uer for pl1 or ci1  (if circle, ignore z)
703   if(ksn(2).eq.7)goto 704
      uer=sqrt((q(1)-xp)**2+(q(2)-yp)**2)-q(7)
      goto 706
704   uer=q(4)-q(1)*xp-q(2)*yp-q(3)*zp
706   duer=uer
      if(dabs(duer).le.atol)goto 90
      itim=itim+1
      if(itim.lt.12)goto 71
c          error. too many iters
c...  Modified error to reflect type of define/redefine - raz
c      ifl(2)=134
c      goto 990
c
      goto 989

71    if(itim.gt.1)goto 72
      du=.01
      if(u+du.gt.1.)du=-.01
      goto 74
72    den=oerr-uer
      if(dabs(den).gt.1.e-6) utan=uer/den
      du=du*utan
      if(du+u.gt.1.)du=1.-u
      if(du+u.lt.0.)du=-u
74    oerr=uer
      u=u+du
      delu=du
c
c...vp 4/3/98 parameter increment can be very small and
c...it is OK since it is double, this prevents switching between
c...segments when intersection is on the end of segment
c
      if(dabs(delu).gt.1.d-8)  goto 70
c          stall-out.  probably u=0,1 excess.  accept excess if small
c          or activate another seg.
      if(dabs(duer).lt..002)goto 90
c
c...vp 4/3/98 spline works with 1 segment so do not jump
c...to next segment but try again or finish iterations
c
      if (iwf .eq. 1) then
         if (itim .lt. 8) go to 70
         go to 77
      end if

      iseg=iseg+1
      if(u.lt..5)iseg=iseg-2
      if(iseg.gt.0.and.iseg.lt.npts.and.isegk.lt.30)goto 55
c
c... mystery exit.  either ran off crv end or inf loop
c... check for closed curve
c
      if (isegk.gt.60) goto 989
  77  call gtclsd (keycrv, 0, iclsd)
      if (iclsd.eq.0) goto 989

      if (iwf.eq.1) then
        u = 1.0d0-u
        goto 65
      endif

      if (iseg.lt.1) then
        iseg = npts-1
      else
         iseg = 1
      endif

      goto 55
c
c... pt found. finup
c
90    if(idst.eq.3) goto 95
      w(4)=xp-w(1)
      w(5)=yp-w(2)
      w(6)=0.
c
c... if line shorter than .010, make it unit length.  13-jan-84
c... aak 15-may-1998: added factor "mmorin" into the next "if"
c
      if (f_mag(w(4)).le.0.01*mmorin) then
         w(4) =  fy*mmorin
         w(5) = -fx*mmorin
      endif

      goto 900

95    w(1)=xp
      w(2)=yp
      w(3)=zp

c          geosto this item
900   ietype = ktv(4) 

c                if redef of a line, put pt in sc(15-17) and exit.  4-feb-87
      if(iredef.eq.0)goto 902
      sc(15)=xp
      sc(16)=yp
      sc(17)=zp
      goto 999
901   continue

902   call ptentt(ietype, w, nclkey, tv)
      rest=tv
      goto 999
c
c...   Curve does not intersect something.....raz
c...    331 = FAILED TO FIND PLANE-CURVE INTERSECTION
c...    417 = FAILED TO FIND CIRCLE-CURVE INTERSECTION
c...    418 = CAN NOT PROJECT POINT ONTO CURVE
c
989   if (idst.eq.3 .and. ksn(2).eq.7) ifl(2)=331 
      if (idst.eq.3 .and. ksn(2).eq.10) ifl(2)=417 
      if (idst.eq.5 .and. ksn(2).eq.10) ifl(2)=418
c          error exit. set err and zero tv & 'rest'
990   err=.true.
      if(ifl(2).eq.0)ifl(2)=5
      tv=0.
      rest=0.

999   return
      end
