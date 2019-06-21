C*********************************************************************
C*    NAME         :  cirpri.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
c*       cirpri.f , 25.1
c*    DATE AND TIME OF LAST  MODIFICATION
c*       04/29/15 , 15:09:42
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cirpri
C*          this routine handles circle cases 13 - 18    
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine cirpri

      include 'com8a.com'
      include 'mocom.com'

      integer*2 maxpt, maxddd
      parameter (maxpt=50)
      parameter (maxddd=(maxpt*6+(maxpt+1)/2))
      common/dddcom/cd(maxddd)
      real*8 cd
      real*4 ad(maxddd*2)
      equivalence (cd,ad)

      common/wblok/w(600)
      real*8 w

      integer*2 kd(600),ia,ib,ic,isrf
      equivalence (d,kd)
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      real*8 u(11),v(11),a(3),b(3),c(3),p(6),ha(2),hb(2),hc(2)
      real*4 e(9)
      integer*2 ksn(4)
      integer*4 nclkey
      integer*2 nwds, ietype
      logical trflg
      real*8 uu, vv, du, hu, asn, ddsq, dd, aok,bok, uerr, cvmx,cvmy
      real*8 acv,bcv,ccv, ax,ay, c1,c2,c3, holdu, xn,yn,zn
      real*8 fx,fy, ro, xlm,ylm, aln,bln,cln, xm,ym,zm, df,dx,dy, 
     *       co, hco,den, xc,yc, amod, rc, rx,ry, rlcrv, 
     *       dis, adis, bdis, sec, ferr, ofer, rerr, xlp, ylp,  
     *       ra, chd, h, aa,bb,cc, f_dist,ermx
      real*4 x4,y4,z4,a4,b4,c4
      integer*2 idx,jdx,kdx,iux,ihx,ix
      integer*2 i, isub, iwf, itim, jtim, ktim, ixx, isegk
      integer*2 ind, iseg, isx, iexit, npts, idel
      integer*2 i2v3/3/, i2v4/4/

      equivalence (asn,ksn),(w(20),u),(w(40),v)
      equivalence (w(65),a),(w(70),b),(w(75),c)

c          zero w(1-11) & w(6)=1     ( these are all xy view circles )
      call fill_array (w(1),11,0.d0)
      w(6)=1.
c          get subtype and branch
      trflg = .true.
      asn=sc(10)
      isub=ksn(2)
      if (isub .eq. 26) goto 260
      go to (130, 140, 150, 160, 170, 180, 990) (isub-12)
c
c***************************   ci/xyls,ln1,xyls,ln2,xyls,ln3   -- 13 --
c
c                get 3 modifiers + lines  and do 3 directed pls
130   do 1310 i=1,3
      isx=2*i+10
      asn=sc(isx)
      call gtentt(sc(isx), trflg, nclkey, ietype, u(1))
      a(i)=-u(5)
      b(i)=u(4)
      sec=dsqrt(u(4)**2+u(5)**2)
      if(sec.gt.0.)goto 1302
c                 error. something wrong.
1301  ifl(2)=163
      goto 990
1302  a(i)=a(i)/sec
      b(i)=b(i)/sec
c                 check this nrm vs modifier
      amod=sc(isx-1)
      if(amod.ne.638.)goto 1303
      if(a(i))1307,1301,1308
1303  if(amod.ne.641.)goto 1304
      if(a(i))1308,1301,1307
1304  if(amod.ne.639.)goto 1305
      if(b(i))1307,1301,1308
1305  if(amod.ne.642.)goto 1301
      if(b(i))1308,1301,1307
c                flip nrm
1307  a(i)=-a(i)
      b(i)=-b(i)
1308  c(i)=a(i)*u(1)+b(i)*u(2)
1310  continue
c                bisect l1,l2
      hco=a(1)*a(2)+b(1)*b(2)
      co=dabs(hco)
      if(co.lt..9999995d0)goto 132
c                 ln1 parlel ln2.  must point at each other.
      if(hco.gt..5)then
         ifl(2)=163
         goto 990
      endif
c 
      ha(1)=a(1)
      hb(1)=b(1)
      hc(1)=(c(1)-c(2))/2.
      goto 134
c                 ln1,ln2 not parlel.  do bisector pl
132   den=b(1)*a(2)-a(1)*b(2)
      xc=(b(1)*c(2)-b(2)*c(1))/den
      yc=(a(2)*c(1)-a(1)*c(2))/den
      ha(1)=a(1)-a(2)
      hb(1)=b(1)-b(2)
      hc(1)=ha(1)*xc+hb(1)*yc
c                 ditto l1,l3
134   hco=a(1)*a(3)+b(1)*b(3)
      co=dabs(hco)
      if(co.lt..9999995d0)goto 136
c                 l1,l3 parlel
      if(hco.gt..5) then
         ifl(2)=163
         goto 990
      endif 
c
      ha(2)=a(1)
      hb(2)=b(1)
      hc(2)=(c(1)-c(3))/2.
      goto 138
c                 intof l1,l3
136   den=b(1)*a(3)-a(1)*b(3)
      xc=(b(1)*c(3)-b(3)*c(1))/den
      yc=(a(3)*c(1)-a(1)*c(3))/den
      ha(2)=a(1)-a(3)
      hb(2)=b(1)-b(3)
      hc(2)=ha(2)*xc+hb(2)*yc
c                 inters 2 bisector pls
138   den=hb(1)*ha(2)-hb(2)*ha(1)
      if(den.eq.0.) then
         ifl(2)=163
         goto 990
      endif 
c
      xc=(hb(1)*hc(2)-hb(2)*hc(1))/den
      yc=(hc(1)*ha(2)-hc(2)*ha(1))/den
      rc=dabs(c(1)-a(1)*xc-b(1)*yc)
      if(rc.lt..0005) then
         ifl(2)=163
         goto 990
      endif 
c
c                 do final chk vs 3 lines for valid ctrpt
      do 139 i=1,3
      if(a(i)*xc+b(i)*yc-c(i).lt.0.) then
         ifl(2)=163
         goto 990
      endif 
c
139   continue
      w(1)=xc
      w(2)=yc
      w(7)=rc
      goto 999

c*********************    ci/xyls,ln1,xyls,cv1,pt1,radius,r    -- 14 --
c                                                       27-dec-84
140   continue
c
c...get ln1 in u
c       cv1 in d
c       pt1 (or pv1) in p
c
      jtim=0
      ktim=0
      iexit=0
      rlcrv=0.
c
c...vp 4/3/98 linear error depends on units, otherwise
c...in MM it's problem with big parts when pt increment falls
c...below significant number in single precision
c
      ermx = .1*sc(27) 
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
      call gtentt(sc(15), trflg, nclkey, ietype, p(1))
      call gtdesc(sc(14), nclkey, nwds, ietype)
      call isitwf(nclkey, iwf)
c
c...vp 29-apr-97
c...evaluate ncl curve like bspline using evaluator
c
      if (sc(169) .gt. 8.399d0) iwf = 1
      if (iwf .eq. 1) then
        isrf=3
        call evstup(nclkey, isrf)
        ia = 1
        ihx = 6*isrf+9
        iux = ihx-2 
        call xyzvc4 (0.,0.,0., t(4,ia))
        idx = 50*(isrf-1)
        kdx = idx*4
        d(idx+2) = sc(14)
        kd(kdx+2)=1
        kd(kdx+3)=1
      else	
        call gtentt(sc(14), trflg, nclkey, ietype, cd(1))
        npts=ad(1)
      endif
c
c...do pl parlel ln1, left/right radius amt.
c
      rx =  u(5)
      ry = -u(4)
      xlm=0.
      ylm=0.
      if(sc(11).eq.638.)xlm=1.
      if(sc(11).eq.641.)xlm=-1.
      if(sc(11).eq.639.)ylm=1.
      if(sc(11).eq.642.)ylm=-1.

      if(rx*xlm+ry*ylm.le.0.) then
         rx = -rx
         ry = -ry
      endif

      sec=dsqrt(rx**2+ry**2)
c
c... error. line seems vertical
c
      if(sec.le.0.001) then
         ifl(2)=44
         goto 999
      endif

      aln=rx/sec
      bln=ry/sec
      cln=aln*u(1)+bln*u(2)+sc(16)
      cvmx=0.
      cvmy=0.
      if(sc(13).eq.638.)cvmx=1.
      if(sc(13).eq.641.)cvmx=-1.
      if(sc(13).eq.639.)cvmy=1.
      if(sc(13).eq.642.)cvmy=-1.
      dis=1.d12
      iseg=1
c
c... find an initial seg per nearpt
c
      if (iwf.eq.1) then
        du = 1.d0/9.d0
        vv = 0.d0
        uu = 0.d0

        do 48 i=1,10
           if (vv.gt.1.0d0) vv = 1.0d0
           call uevcvt (vv,isrf,ifl(72),a,b,ifl(2))
           adis = f_dist(p,a)

           if (adis.lt.dis) then
             dis = adis
             uu = vv
           endif

           vv=vv+du
48      continue

        t(iux,ia) = uu
        idx=0
        cd(1)=0.0
        cd(2)=0.0
        goto 65
      endif
c
c...************** iwf != 1 ******************************************
c
      ind=npts-1
      idx=(npts+1)/2-5
      idel=idx
c          loop thru segs to find start seg
      do 50 i=1,ind
      idx=idx+6
      jdx=2*idx+5
c          check nrpt against cvpts and approx midpts
      xm=(cd(idx)+ad(jdx))*.666667 +cd(idx+6)*.333333
      ym=(cd(idx+1)+ad(jdx+1))*.666667+cd(idx+7)*.333333
      zm=(cd(idx+2)+ad(jdx+2))*.666667+cd(idx+8)*.333333
      adis=(p(1)-xm)**2+(p(2)-ym)**2+(p(3)-zm)**2
      bdis=(p(1)-cd(idx))**2+(p(2)-cd(idx+1))**2+(p(3)-cd(idx+2))**2
      if(bdis.lt.adis)adis=bdis
      if(adis.ge.dis)goto 50
      dis=adis
      iseg=i
50    continue

c          activate seg(iseg) in e-tbl
55    idx=idel+6*iseg-1
      isegk=isegk+1
      ixx=idx+6
      jdx=2*idx+6
      ro=ad(jdx+6)
      do 60 i=1,3
      e(i)=ad(jdx+i)
      e(i+6)=cd(ixx+i)-cd(idx+i)
60    e(i+3)=e(i+6)-ro*ad(jdx+i+12)
c
c... *****************************************************************
c

65    continue

      if (iwf.eq.1) then

        if (ktim.gt.256) goto 75
        call conv8_4 (p,t(1,ia),3)

        if (ifl(72).eq.1) then
          call conv4_8 (t(1,ia),a,3)
          call conent(a,sc(56),i2v3)
          call conv8_4 (a,t(1,ia),3)
        endif

        call ucrvpv (x4,y4,z4,a4,b4,c4)
        if (ifl(2).gt.0) goto 75

        if (ifl(72).eq.1) then
          a(1) = x4
          a(2) = y4
          a(3) = z4
          b(1) = a4
          b(2) = b4
          b(3) = c4
          call conent(a,sc(68),i2v3)
          call conent(b,sc(68),i2v4)
          x4 = a(1)
          y4 = a(2)
          z4 = a(3)
          a4 = b(1)
          b4 = b(2)
          c4 = b(3)
        endif

        uu = t(iux,ia)
        fx = a4
        fy = b4
        ax = x4-uu*fx
        ay = y4-uu*fy
        rlcrv=1.
        if(fy*cvmx-fx*cvmy.lt.0.)rlcrv=-1.
        if (uu.gt.0.0d0 .and. uu.lt.1.0d0) goto 80
        sec = fx**2+fy**2
        if (sec.eq.0.0d0) goto 75
        if (uu.lt..5d0) sec = -sec
        dx = fx/sec
        dy = fy/sec
        ax = p(1)-x4
        ay = p(2)-y4
        co = ax*dx+ay*dy
        ax = x4+co*dx-uu*fx
        ay = y4+co*dy-uu*fy
        goto 80
      endif
c
c ... ***********************************************************
c          nrpt in segsys
      xn=p(1)-cd(idx+1)
      yn=p(2)-cd(idx+2)
      zn=p(3)-cd(idx+3)
      uu=.5
      itim=0
c           find u-value this seg, this nrpt
70    itim=itim+1
      jtim=jtim+1
      c1=(1.-uu)**2
      c2=2.*uu*(1.-uu)
      c3=uu**2
      ax=c2*e(1)+c3*e(4)
      ay=c2*e(2)+c3*e(5)
      fx=c1*e(1)+c2*e(4)+c3*e(7)-ax
      fy=c1*e(2)+c2*e(5)+c3*e(8)-ay
      hu=uu
      den=fx**2+fy**2
      if(den.eq.0.)goto 75
c           if itim=1, set r/l of crv
      if(itim.gt.1)goto 72
      rlcrv=1.
      if(fy*cvmx-fx*cvmy.lt.0.)rlcrv=-1.
72    uerr=((fx*(xn-ax)+fy*(yn-ay))/den-uu)/3.
      if(dabs(uerr).lt.1.d-5)goto 80
      holdu=uu+uerr
      uu=holdu
      if(uu.lt.0.)uu=0.
      if(uu.gt.1.)uu=1.
c             12 iters max
      if(itim.gt.12.or.jtim.gt.50)goto 75
c             if any real chg, go again
74    if(dabs(uu-hu).gt.1.d-5)goto 70
c
c             stall-out.  try next seg if any.
      iseg=iseg+1
      if(holdu.lt.0.)iseg=iseg-2
      if(iseg.gt.0.and.iseg.lt.npts)goto 55
c             no seg there.  set iexit and go calc last xc,yc
      iexit=1
      goto 80
c
c... mystery exit.  cv is vertical, complex, impossible, etc.
c
75    ifl(2)=268
      goto 999
c
c... cpt solve.
c
80    sec = dsqrt(fx**2+fy**2)
      if(sec.eq.0.) goto 75
      acv=fy*rlcrv/sec
      bcv=-fx*rlcrv/sec
      ccv=acv*(ax+cd(idx+1))+bcv*(cd(idx+2)+ay)+sc(16)
      den=aln*bcv-acv*bln
      den=-den
      if(den.eq.0.) goto 75

      xc = ( bln*ccv - bcv*cln)/den
      yc = (-aln*ccv + acv*cln)/den

      ferr=(fx*(xc-p(1))+fy*(yc-p(2)))/sec

      if(dabs(ferr).lt.ermx.or.iexit.eq.1)goto 90
c
c...calc df
c
      if(ktim.le.0) then
         df = 0.1*ferr
      else
         den = ofer-ferr
         if(den.eq.0.) goto 75
         df = df*ferr/den
      endif

      ktim=ktim+1
      ofer=ferr
c
c... real chg in cpt.  set next nrpt and try again
c
      p(1) = cd(idx+1)+ax+uu*fx+fx*df/sec
      p(2) = cd(idx+2)+ay+uu*fy+fy*df/sec
c
c ... aak 14-jan-1998: should extrap. z-coord. for non-flat curves
c
      if (iwf.eq.1) p(3) = z4 + c4*df/sec

      goto 65
c
c...******* end of iteration loop *************************
c
c           finup circle def
c            if tanpt on cv extension, must not be internal.
90    if(iexit.eq.0)goto 95
      if(iseg.eq.0.and.holdu.gt..01)goto 75
      if(iseg.eq.npts.and.holdu.lt..99)goto 75
95    w(1)=xc
      w(2)=yc
      w(7)=sc(16)
cc                chg from 360 circle to finite arc          7-jul-87
c                  first chk cpt vs. crvpt 
      dx=cd(idx+1)+ax+uu*fx-xc
      dy=cd(idx+2)+ay+uu*fy-yc
      rerr=dsqrt(dx**2+dy**2)-sc(16)
      if(dabs(rerr).gt..003)goto 75

cc                 (add ok plane to w(8-11)
      aok=aln+acv
      bok=bln+bcv
      sec=dsqrt(aok**2+bok**2)
cc                  if sec very small, ln and crv are parlel
      if(sec.lt.1.d-5) then
         ifl(2)=163
         goto 990
      endif 
c
      xlp=xc-aln*sc(16)
      ylp=yc-bln*sc(16)
      w(8)=-aok/sec
      w(9)=-bok/sec
      w(11)=w(8)*xlp+w(9)*ylp
cc                  end of chg 7-jul-87
      goto 999

c***************************  ci/pt1,xyls,inout,ci1,radius,r   -- 15 --
150   continue              
      call gtentt(sc(11), trflg, nclkey, ietype, u(1))
      call gtentt(sc(14), trflg, nclkey, ietype, v(1))
      ra=sc(15)
c              check for tipped axis
      if(v(4)**2+v(5)**2.lt.1.d-6)goto 151
      ifl(2)=161
      goto 990
151   dx=u(1)-v(1)
      dy=u(2)-v(2)
      chd=dsqrt(dx**2+dy**2)
      if(chd.lt..002) then
         ifl(2)=163
         goto 990
      endif 
c
      dx=dx/chd
      dy=dy/chd
      if(chd.ge.v(7))goto 152
c                  pt1 is inside ci1.   out is ng
      if(sc(13).eq.653.) then
         ifl(2)=163
         goto 990
      endif 
c
      h=v(7)-sc(15)
ccccccccccccccc         check for real????????
      goto 154
152   if(sc(13).eq.652.)goto 153
      h=ra+v(7)
      goto 154
c                      "in" specified.
153   h=ra-v(7)
      if(dabs(v(7)-chd).lt.1.d-5)h=dabs(h)
c                     h must be real  (allow small neg h)
154   if(h.ge.0.)goto 1541
      if(h.lt.-.0001) then
         ifl(2)=163
         goto 990
      endif 
c
      h=0.
1541  aa=-dy
      bb=dx
c                     check xyls
      if(sc(12).ne.638.)goto 1542
      if (aa .lt. 0.) then
         goto 155
      elseif (aa .eq. 0.) then
         ifl(2) = 163
         goto 990
      elseif (aa .gt. 0.) then
         goto 156
      endif
c
1542  if(sc(12).ne.641.)goto 1544
      if (aa .lt. 0.) then
         goto 156
      elseif (aa .eq. 0.) then
         ifl(2) = 163
         goto 990
      elseif (aa .gt. 0.) then
         goto 155
      endif
c
1544  if(sc(12).ne.639.)goto 1546
      if (bb .lt. 0.) then
         goto 155
      elseif (bb .eq. 0.) then
         ifl(2) = 163
         goto 990
      elseif (bb .gt. 0.) then
         goto 156
      endif
c
1546  if(sc(12).ne.642.) then
         ifl(2) = 163
         goto 990
      endif
c
      if (bb .lt. 0.) then
         goto 156
      elseif (bb .eq. 0.) then
         ifl(2) = 163
         goto 990
      elseif (bb .gt. 0.) then
         goto 155
      endif
c
155   aa=-aa
      bb=-bb
156   cc=(h**2+chd**2-ra**2)/(2.*chd)
      ddsq=h**2-cc**2
c
c...Allow for round off (problem on Alpha/VMS)
c...Bobby  -  06/17/97
c
      if (ddsq .ge. -1.d-6 .and. ddsq .le. 1.d-6) ddsq = 0.
c
      if(ddsq.lt.0.) then
         ifl(2) = 163
         goto 990
      endif
c
      dd=dsqrt(ddsq)
c                        finup
      w(1)=v(1)+dx*cc+aa*dd
      w(2)=v(2)+dy*cc+bb*dd
      w(7)=ra
      goto 999

160   continue                 

c***************************  ci/canon,x,y,z,i,j,k,r,<a,b,c,d> -- 16 --
      isx=isc10(3)
      do 161 i=1,isx
161   w(i)=sc(i+10)

162   if(w(7).gt..0001)goto 163
      ifl(2)=165
      goto 999

163   chd=dsqrt(w(4)**2+w(5)**2+w(6)**2)
      if(chd.gt..0001)goto 164
      ifl(2)=298
      goto 999

164   w(4)=w(4)/chd
      w(5)=w(5)/chd
      w(6)=w(6)/chd

      if (isx.ne.11)goto 999
c                    limit plane specified
      chd=dsqrt(w(8)**2+w(9)**2+w(10)**2)
      if(chd.lt..0001)goto 999
      w(8)=w(8)/chd
      w(9)=w(9)/chd
      w(10)=w(10)/chd
      if (abs(w(4)*w(8)+w(5)*w(9)+w(6)*w(10)).lt..001)goto 165
c---                     axis vector and limit plane vector not perp
      ifl(2)=299
      goto 990

165   dis=w(1)*w(8)+w(2)*w(9)+w(3)*w(10)
      if(abs(w(11)-dis).le.w(7))goto 999
c---                      limit plane does not cut circle
      ifl(2)=300
      goto 990

c***************************  ci/canon,pt1,ve1,r,<pl1>         -- 17 --
170   continue               
      isx=isc10(3)
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      if (ietype .ne. 21) then
          call gtentt(sc(12), trflg, nclkey, ietype, p(1))
          ix    = 0
          if (ietype .eq. 21) ix = 3 
          w(4) = p(ix+1)
          w(5) = p(ix+2)
          w(6) = p(ix+3)
      end if
      w(7)=sc(13)
      if(isx.ne.11) goto 162
c                          limit plane specified
      call gtplt (sc(14), ifl(72),  w(8))
      goto 162

c***************************  ci/ci1,<xval>,<yval>,<zval>      -- 18 --
180   continue   
      if (sc(169) .lt. 9.75d0) then             
        call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      else
        call gtcirc(sc(11), trflg, nclkey, ietype, w(1))
      endif
      if(isc10(3).eq.0)goto 999
      w(1)=w(1)+sc(12)
      if(isc10(3).lt.2)sc(13)=0.
      w(2)=w(2)+sc(13)
      if(isc10(3).lt.3)sc(14)=0.
      w(3)=w(3)+sc(14)
      if(w(8).eq.0.and.w(9).eq.0.and.w(10).eq.0)goto 999
      w(11)=w(11)+sc(12)*w(8)+sc(13)*w(9)+sc(14)*w(10)
      goto 999

c***************************  ci/offset,in/out,ci1,dis         -- 26 --
260   continue                
      if (sc(169) .lt. 9.75d0) then
        call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      else
        call gtcirc(sc(11), trflg, nclkey, ietype, w(1))
      endif
      ro = w(7)
      w(7) = w(7) + sc(12)
      if(w(7) .le. .0001) then
        ifl(2) = 165
        goto 999
      endif
      if (w(8).eq.0 .and. w(9).eq.0 .and. w(10).eq.0) goto 999
      co = w(1)*w(8) + w(2)*w(9) + w(3)*w(10)
      adis = w(11) - co
      bdis = adis*(w(7)/ro)
      w(11) = bdis + co
      goto 999

c***** error exit: impossible geometry
990   if(ifl(2).lt.1) ifl(2)=163

999   return
      end
