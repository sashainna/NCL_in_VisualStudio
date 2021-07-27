C*********************************************************************
C*    NAME         :  rldpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       rldpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:38
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rldpre
c*       preprocessor for rldsrf.
c*       w is work table for 2 entities. (cols 1 & 3)
c*       build canon form in p-tbl.
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
      subroutine rldpre

      include 'com8a.com'

      integer*2 maxpt, maxwd, maxptx
      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2)) ! thus maxwd = 325

      common/wblok/w(maxwd*4+80)

      integer*2 ietype, kent, m,n, iwx,jwx, ipt1, itx,jtx 
      integer*2 ksn(4),lsc(400),ient(4),aix,ncir
      integer*4 nclkey,key1,key2
      logical trflg
      real*8 w
      real*8 tbuf(3)
      real*4 aw(8*(maxwd+20))

      integer*2 prtyp,icoor
      real*8 prdat(16)
      common/primbl/prdat,prtyp,icoor

      equivalence(asn,ksn),(sc,lsc),(sc(53),ient),(aw,w)
c
c..... 2 pts not allowed as surf def
c
      if(lsc(44).eq.3.and.lsc(48).eq.3) goto 98
      kent=1
      m=1
      ncir = 0
      ipt1=0
10    iwx=((maxwd+20)*2)*(m-1)
      jwx=iwx*2
      itx=iwx+maxwd
      jtx=jwx+(maxwd*2)
      n=1
      if(m.eq.2) n=3
      asn=sc(m+10)
      ldw=iwx
c
c..... if line or point, load 1 loc down
c
      if(ksn(4).eq.5.or.ksn(4).eq.3) ldw=iwx+1
      trflg = .true.
      call gtentt(asn, trflg, nclkey, ietype, w(ldw+1))
      if (m .eq. 1) then
         key1 = nclkey
         if (ifl(346).eq.0 .and. ietype .eq. CIRCLE .and. 
     *       (w(8)**2 + w(9)**2 + w(10)**2 .lt.0.9)) ncir = 1
      else
         key2 = nclkey
         if (ncir.eq.1 .and. ietype .eq. CIRCLE .and. 
     *       (w(ldw+8)**2 + w(ldw+9)**2 + w(ldw+10)**2 .lt.0.9))
     *     ncir = 2
      endif
c
c..... if ncl curve check if trimmed
c
      if (ietype .eq. 8) then
         call ncl_get_tpar (nclkey,tbuf)
         if (tbuf(1) .ne. 0. .or. tbuf(2) .ne. 1.d0)
     -       call rdf_curve (nclkey,tbuf,ldw*2+1)
      endif

      if (debug) then
          aix=ldw*2
          write (cout,1011)(aw(aix+i),i=1,4)
1011      format(' in rldpre:aw=',4f8.5)
          call putmsg(cout,80,17,0)
          write(cout,1012) (w(i),i=ldw+3,ldw+5)
1012      format( '     w=',3f8.5)
          call putmsg(cout,80,17,0)
          aix=(ldw+5)*2+1
          write (cout,1013) (aw(i),i=aix,aix+5)
1013      format ('deltas=',6f8.5)
          call putmsg(cout,80,1,0)
          write(cout,1012) (w(i),i=ldw+9,ldw+11)
          call putmsg(cout,80,17,0)
          aix=(ldw+11)*2+1
          write (cout,1013) (aw(i),i=aix,aix+5)
          call putmsg(cout,80,1,0)
          write(cout,1012) (w(i),i=ldw+15,ldw+17)
          call putmsg(cout,80,17,0)
          aix=(ldw+17)*2+1
          write (cout,1013) (aw(i),i=aix,aix+5)
          call putmsg(cout,80,1,0)
      endif

      ient(n)=8
      if(ksn(4).ne.8) goto 20
c**********  curve.  put nwds in aw(300) and no activ seg in aw(262)
      aw(jtx+40)=aw(jwx+1)
      aw(jwx+1)=0.
      w(itx+1)=0.
      goto 50

20    if(ksn(4).ne.7) goto 30
c**********  circle.  add 4 params for fwd sense and half-angle phi 
      i=iwx+1
	if (ncir.eq.2 .and. ifl(346).eq.0) then
	  co = f_dot(w(4),w(ldw+4))
	  if (co .lt. -0.5) then
	    w(ldw+4) = -w(ldw+4)
	    w(ldw+5) = -w(ldw+5)
	    w(ldw+6) = -w(ldw+6)
	  endif
	endif
      call cirbld(w(i))
      ient(n)=7
      goto 50

30    if(ksn(4).ne.5) goto 40
c**********  line.   create a 2-pt curve
32    i=iwx
      j=jwx
      w(i+8)=w(i+2)+w(i+5)
      w(i+9)=w(i+3)+w(i+6)
      w(i+10)=w(i+4)+w(i+7)
      aw(j+21)=w(i+5)/3.
      aw(j+22)=w(i+6)/3.
      aw(j+23)=w(i+7)/3.
c
c..... also seg1
c
      aw(j+9)=aw(j+21)
      aw(j+10)=aw(j+22)
      aw(j+11)=aw(j+23)
c          finup misc
      aw(j+12)=1.
      aw(j+13)=0.
      aw(j+14)=1.
      w(i+1)=0.
      w(itx+1)=0.
      w(itx+20)=0.
      aw(j+2)=1.
      aw(jtx+40)=2.
      goto 50

40    if(ksn(4).ne.3) goto 98
c**********  point.  make it a very short 2-pt curve.
c                  get fwd from other entity (cv or ci) and go line
c                  route.  must postpone if this is 1st entity
      if(m.ne.1) goto 42
      ipt1=1
      m=2
      goto 50
c
c..... point to other entity
c
42    ioent=1
      if(m.eq.1)ioent=3
      iox=(maxwd+20)*(ioent-1)
      jox=2*iox
c
c..... if that entity is cv, pu 1st set of deltas
c
      if(ient(ioent).eq.7) goto 44
c
c..... get npts and point to end of 1st xyz
c
      npts=aw(jox+(maxwd+20)*2)
      ind=iox+(npts+1)/2+3
      jnd=2*ind
      fx=aw(jnd+1)
      fy=aw(jnd+2)
      fz=aw(jnd+3)
      goto 46
c
c..... other entity is circle. fwd is in w(12-14)
c
44    fx=w(iox+12)
      fy=w(iox+13)
      fz=w(iox+14)
46    sec=dsqrt(fx**2+fy**2+fz**2)*1000.
      if(sec.lt..001) goto 98
      w(iwx+5)=fx/sec
      w(iwx+6)=fy/sec
      w(iwx+7)=fz/sec
      goto 32
50    kent=kent+1
      m=kent
      if(kent.lt.3) goto 10
c
c..... if ipt1 on, go back and convert to short cv
c
      call rld_primdat(key1,key2,prtyp,prdat)

      if(ipt1.eq.0) goto 99
      ipt1=0
      m=1
      iwx=0
      jwx=0
      itx=maxwd
      jtx=maxwd*2
      goto 42
c
c..... error exit
c
98    ifl(2)=12
      err=.true.

99    return
      end

c**********************************************************************
      subroutine rdf_curve (nclkey,gpar,kx)
c
      include 'com8a.com'
      include 'wrksys.com'
c
      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))
c
      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q,invx
      common/wblok/w(maxwd*4+80)

      REAL*8 P(400)
      EQUIVALENCE (X,P)
      real*8 x(maxptx),y(maxptx),z(maxptx)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx),dy(maxptx)
      real*4 dz(maxptx),ch(maxptx),q(maxpt*36)
      integer*2 invx(maxptx)

      INTEGER*2 inv(maxptx),ivf,iret
 
      real*4 aw(maxwd*8+160)
      equivalence (w,aw)
 
      real*8 gpar(3)
      integer*4 nclkey
      integer*2 kx
      integer*2 nt,is,ie,npt,ixp,ixd,j,iwx
      logical midpt
c
      real*8 vc1(3),pt1(3),vc2(3),pt2(3)
      real*8 dis,dq,dt,f_dist
      real*8 fac/25.4d0/
c
c...Check start point of the curve
c...if point is close to the Ist control point replace control point
c...othervise create new point
c
      midpt = .false.
      npt = 0
      nt  = aw(kx) 
      iwx  = (nt + kx) / 2
      dis = dabs(gpar(1)) 
      call ncvevl1 (nclkey,2,0.d0,pt1,vc1)
      call ncvevl1 (nclkey,2,1.d0,pt2,vc2)
      if (lwrk) then
        call conent (pt1, invwrk, POINT)
        call conent (pt2, invwrk, POINT)
        call conent (vc1, invwrk, VECTOR)
        call conent (vc2, invwrk, VECTOR)
      endif
      if (ifl(264).eq.1) then
         call vctmsc (pt1,pt1,fac)
         call vctmsc (pt2,pt2,fac)
      endif
         
      call unitvc (vc1,vc1)
      call unitvc (vc2,vc2)
c
      npt  = 1 
      X(npt) = pt1(1)
      Y(npt) = pt1(2)
      Z(npt) = pt1(3)
      A(npt) = vc1(1)
      B(npt) = vc1(2)
      C(npt) = vc1(3)
      inv(npt) = 1 
      invx(npt) = 1 
c
      aw(kx) = 0.
      do 105 is=1,nt,1
         if (gpar(1) .lt. aw(kx+is-1)) go to 120
  105 continue     
  120 if (is .gt. 1) then
          dt = aw(kx+is-1) - aw(kx+is-2)
          dq = (aw(kx+is-1) - gpar(1)) / dt 
          if (dq .lt. .01) is = is + 1 
      else
          dq = -gpar(1) / aw(kx+1)
          if (dq .lt. .01) then
            dt = f_dist(pt1,w(iwx+1))
            if (dt.lt.sc(27)) then
              is = is + 1 
              dt = aw(kx+1)/2.0
              call ncvevl1 (nclkey,2,dt,pt1,vc1)
              if (lwrk) then
                call conent (pt1, invwrk, POINT)
                call conent (vc1, invwrk, VECTOR)
              endif
              if (ifl(264).eq.1) then
                 call vctmsc (pt1,pt1,fac)
              endif
              npt  = npt + 1
              X(npt) = pt1(1)
              Y(npt) = pt1(2)
              Z(npt) = pt1(3)
              A(npt) = vc1(1)
              B(npt) = vc1(2)
              C(npt) = vc1(3)
              inv(npt) = 1
              invx(npt) = 1
              if (nt .le. 2) midpt = .true.
            endif
          endif
      end if
c
c...Check end point of the curve
c...if point is close to the Ist control point replace control point
c...othervise create new point
c
      do 205 ie=1,nt,1
         if (gpar(2) .lt. aw(kx+ie)) go to 220
  205 continue
  220 if (ie .gt. nt) then
         ie = nt
         dq = (gpar(2) - 1.0) / (1.0 - aw(kx+ie-2)) 
         if (dq .lt. .01) then
           ixp = 6*(ie-1)+iwx
           dt = f_dist(pt2,w(ixp+1))
           if (dt.lt.sc(27)) then
             ie = ie - 1   
             if (.not. midpt) then
               dt = (1.0 - aw(kx+ie-1))/2.0 + aw(kx+ie-1)
               call ncvevl1 (nclkey,2,dt,pt1,vc1)
               midpt = .true.
             else
               midpt = .false.
             endif
           endif
         endif
      else
         dt = aw(kx+ie) - aw(kx+ie-1)
         dq = (gpar(2) - aw(kx+ie-1)) / dt
         if (dq .lt. .01) ie = ie - 1
      end if
c
c...copy intermediate points
c
      iwx  = (nt + kx) / 2
      do 305 i=is,ie,1
         npt  = npt + 1
         ixp  = 6*(i-1) + iwx
         ixd  = 2*ixp + 6
         X(npt) = w(ixp+1)
         Y(npt) = w(ixp+2)
         Z(npt) = w(ixp+3)
         A(npt) = aw(ixd+1)
         B(npt) = aw(ixd+2)
         C(npt) = aw(ixd+3)
         inv(npt) = 1 
         invx(npt) = 1 
  305 continue
      if (midpt) then
        npt  = npt + 1
        X(npt) = pt1(1)
        Y(npt) = pt1(2)
        Z(npt) = pt1(3)
        A(npt) = vc1(1)
        B(npt) = vc1(2)
        C(npt) = vc1(3)
        inv(npt) = 1
        invx(npt) = 1
      endif
      npt  = npt + 1 
      X(npt) = pt2(1)
      Y(npt) = pt2(2)
      Z(npt) = pt2(3)
      A(npt) = vc2(1)
      B(npt) = vc2(2)
      C(npt) = vc2(3)
      inv(npt) = 1 
      invx(npt) = 1 
c
c...set curve slope & interpolate the curve 
c
      iret   = 0
      ivf    = 1
c
c..... Eduard: I changed the next call to slpfar, making use of the common
c..... block array invx.  2/19/99. 
c      call slpset(inv,npt,ivf,iret)
c
      call slpfar(npt,ivf,iret)
c
c..... error in slpfar step
c
      if (iret.ge.1) then
           ifl(2) = iret + 46
           goto 8000
      endif

c     if (iret .ne. 0) go to 8000
      call crvpre(inv,npt,0)
c
c...replace w array with new curve data
c
      do 405 is=1,npt
         aw(kx+is-1) = dx(is)
  405 continue 
      iwx  = (npt + kx)/2
      do 415 is=1,npt
         i = 6*(is-1)+iwx
         w(i+1)=x(is)
         w(i+2)=y(is)
         w(i+3)=z(is)
         j=2*i+6
         aw(j+1)=a(is)
         aw(j+2)=b(is)
         aw(j+3)=c(is)
         aw(j+4)=dy(is)
         aw(j+5)=dz(is)
         aw(j+6)=ch(is)
  415 continue

 8000 return
      end
