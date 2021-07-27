C*********************************************************************
C*    NAME         :  cirprj.f
C*       CONTAINS:
C*    COPYRIGHT 1999 (c) NCCS.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       cirprj.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:42
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cirprj
C*          this routine handles circle cases 19 - 22 
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
      subroutine cirprj

      include 'com8a.com'
      include 'mocom.com'

      integer*2 maxpt, maxwd
      parameter (maxpt=50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/wblok/w(4*(maxwd+20))
      real*8 w

      integer*2 kd(600),ia,ib,ic,isrf
      equivalence(d,kd)
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
c
      real*8 u(11),v(11),a(3),b(3),c(3),p(6),ap(3),pt1(3)
      real*8 asn
      integer*2 ksn(4), isc(100)
      integer*4 nclkey, nclkey2
      integer*2 nwds, ietype
      logical trflg
      real*8 uu, vv, du, cvmx,cvmy, cvmx2,cvmy2, acv,bcv,ccv 
      real*8 fx,fy,fz, ra,rc,ro, xm,ym, xn,yn, df,dx,dy, 
     *       den, xc,yc, rx,ry, rlcrv, dis, adis, ax,ay,
     *       sec, ferr, ofer, rerr, chd, aa, f_dist,ermx
      real*4 x4,y4,z4,a4,b4,c4, xx4,yy4,zz4,aa4,bb4,cc4
      integer*2 idx,kdx,ihx,iux,i,ind,isub,ktim,noext
      integer*2 i2v3/3/, i2v4/4/

      equivalence (asn,ksn),(w(20),u),(w(40),v),(sc,isc)
      equivalence (w(65),a),(w(70),b),(w(75),c)

c
c..... check that the version is not old
c  
      if (sc(169) .gt. 8.399d0) goto 10
      ifl(2)=5
      goto 999 
10    continue
c
c..... Set w(1-5) and w(7-11) equal to zero, and w(6)=1     
c..... (these are all xy view circles)
c
      call fill_array (w(1),11,0.d0)
      w(6)=1.
      call fill_array (tcol(1),45,0.d0)
      trflg = .true.
      ia = 1                      ! ia is equivalent to ifl(51)
      ktim=0
      ind=0
c
c..... the maximal error is 1/10 of the current tolerance
c
      ermx = .1*sc(27)
c
c..... get subtype and branch
c
      asn=sc(10)
      isub=ksn(2)
      go to (190, 290, 390, 490, 990) (isub-18)

c***************    ci/inout,ci1,xyls,cv1,pt1,radius,r    -- 19 --
190   continue                   
 
      noext = 0
      cvmx=0.
      cvmy=0.
      if(sc(13).eq.638.) cvmx=1.
      if(sc(13).eq.641.) cvmx=-1.
      if(sc(13).eq.639.) cvmy=1.
      if(sc(13).eq.642.) cvmy=-1.
c
c..... get ci1 in u, pt1 (or pv1) in p
c
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
      call gtentt(sc(15), trflg, nclkey, ietype, p(1))
      ra=sc(16)
      if (ra .lt. 0.001) then
         ifl(2)=165
         goto 999
      endif
c
c..... ra is radius of circle under construction
c..... rc is radius of ci1, (rx,ry) its center 
c
      rc=u(7)
      if (rc .lt. 0.001) then
         ifl(2)=165
         goto 999
      endif
      rx=u(1)
      ry=u(2)
c
c..... check that circle ci1 is not tipped from horizontal plane 
c
      if (u(4)**2+u(5)**2 .ge. 1.d-6) then      
         ifl(2)=161
         goto 999
      endif
c
c..... ro is distance between center of ci1 and that of circle
c..... under construction; it is defined using inout modifier
c
      if (sc(11) .eq. 652.) then
         ro=rc-ra
      else 
         ro=rc+ra     
      endif
      if (ro .lt. 0.001) then
         ifl(2)=165
         goto 999
      endif
c
c...evaluate ncl curve like bspline using evaluator
c
        call gtdesc(sc(14), nclkey, nwds, ietype)
        isrf=3
        call evstup(nclkey, isrf)
c
c..... curve data is passed to ucrvpv as dtbl 
c
        idx = 50*(isrf-1)
        d(idx+2) = sc(14)
        kdx = idx*4
        kd(kdx+2)=1
        kd(kdx+3)=1
c
c..... find an initial segment close to pt1
c
      dis=1.d12
      du = 1.d0/9.d0
      vv = 0.d0
      uu = 0.d0

      do 195 i=1,10
          if (vv.gt.1.0d0) vv = 1.0d0
          call uevcvt (vv,isrf,ifl(72),a,b,ifl(2))
          adis = f_dist(p,a)
          if (adis.lt.dis) then
            dis = adis
            uu = vv
          endif
          vv=vv+du
195   continue

c
c..... curve parameter uu and point pt1 are passed to ucrvpv as t 
c
      iux = 25    
      t(iux,ia) = uu
      pt1(1) = p(1)
      pt1(2) = p(2)
      pt1(3) = p(3)
c
c  ******** start of iteration loop **********
c
200   if (ktim.gt.256) goto 975
      call conv8_4 (p,t(1,1),3)
c
c..... ifl(72)=1 if refsys is in effect.
c..... then sc(56)-sc(67) contain the current refsys matrix
c..... sc(68)-sc(79) contain its inverse
c
      if (ifl(72).eq.1) then
        call conv4_8 (t(1,1),a,3)
        call conent(a,sc(56),i2v3)
        call conv8_4 (a,t(1,1),3)
      endif
 
         call ucrvpv (x4,y4,z4,a4,b4,c4)
         if (ifl(2).gt.0) goto 999
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
         fx=a4    
         fy=b4      
         fz=c4      

      sec = dsqrt(fx**2+fy**2)
      if (sec.le.0.001) goto 975
      fx = fx/sec
      fy = fy/sec
c
c..... (fx,fy) is normalized tangent vector to (horizontal
c..... projection of) cv1 at (x4,y4)
c
c..... compute the next iteration for the center point (xc,yc)
c
      acv=fy
      bcv=-fx
      if (acv*cvmx+bcv*cvmy.lt.0.) then      
         acv=-acv
         bcv=-bcv
      endif
c
c..... (acv,bcv) is a normal direction to cv1 at (x4,y4)
c
      ax=x4+acv*ra
      ay=y4+bcv*ra
c
c..... (ax,ay) is at the distance ra from (x4,y4) 
c
      dx=rx-ax
      dy=ry-ay
      ccv=dabs(acv*dx + bcv*dy)     
c
c..... ccv is the projection of the interval (ax,ay) - (rx,ry)
c..... onto the normal, i.e.,  
c..... ccv is the distance from the center of ci1 to the line  
c..... ln thru (ax,ay) in the tangent (fx,fy) direction
c
      den=fx*dx + fy*dy     
c
c..... den is the projection of the interval (ax,ay) - (rx,ry)
c..... onto the line ln
c
      rlcrv=1.0
      if (den .lt. 0.) then 
         rlcrv=-1.0
         fx=-fx
         fy=-fy
         den=-den
      endif

      if (ccv .lt. ro) then
          chd=dsqrt(ro**2-ccv**2)
      else 
          chd=0.0
      endif

c
c..... find intersection of line ln and circle with radius ro:
c..... this is the next iteration for the center point
c
      del = den - chd
      xc = ax + fx*del
      yc = ay + fy*del
c
c..... ferr is how far from cv1 the circle touches the tangent line
c
      ferr=fx*(xc-p(1))+fy*(yc-p(2))
      if(dabs(ferr).lt.ermx) goto 250
c
c..... calculate df
c
      if(ktim.le.0) then
         df = 0.1*ferr
      else
         del = ofer-ferr
         if(del.eq.0.) then
            ifl(2)=268
            goto 999
         endif
         df = df*ferr/del
      endif

      if(dabs(df) .gt. 0.1) then
         if (df .gt. 0) then
            df = 0.1
         else
            df = -0.1
         endif
      endif

      ofer=ferr

      uu=t(25,1)
c
c..... if we are moving on the curve extension, try to fit a circle tangent
c..... to this extension, if it doesn't work move inside the curve
c
      if (uu .le. 0.0 .and. df*rlcrv .lt. 0.0) then
        if (noext.eq.0 .or. noext.eq.2) then
          noext = noext + 1
c
c........When moving off of the U=0 extension
c........Then tangent vector needs to be reversed
c........Bobby - 3/7/07
c
          rlcrv = rlcrv * (-1.)
        else
          df = -df
        endif
      else if (uu .ge. 1.0 .and. df*rlcrv .gt. 0.0) then
        if (noext.eq.0 .or. noext.eq.1) then
          noext = noext + 2
        else
          df = -df
        endif
      endif
      if (noext.eq.1 .or. noext.eq.2) then
        if (rlcrv .lt. 0) then 
          fx = -fx
          fy = -fy
          den = -den
        endif
        del = den - chd
        if (del .lt. 0.) del = den + chd
        if (del .ge. 0.) then
          xc = ax + fx*del
          yc = ay + fy*del
          x4 = x4 + fx*del
          y4 = y4 + fy*del
c
c..... FSR 60432: check which of the two extension tangencies
c..... is closer to the near point
cc     x      (x4-pt1(1)+fx*chd)*fx+(y4-pt1(2)+fy*chd)*fy .lt. 0) then
c
          den = (x4-pt1(1)+fx*chd)*fx+(y4-pt1(2)+fy*chd)*fy
          if (sc(169).ge.9.35 .and. den .lt. 0) then
            del = 2*chd
            xc = xc + fx*del
            yc = yc + fy*del
            x4 = x4 + fx*del
            y4 = y4 + fy*del           
          endif
          goto 250
        endif
      endif
c
c..... set next near point and try again
c
      p(1) = x4+fx*df
      p(2) = y4+fy*df
      p(3) = z4+fz*df

      ktim=ktim+1
      goto 200
c
c*********** end of iteration loop ************
c
250   w(1)=xc
      w(2)=yc
      w(7)=ra    
c
c..... check whether circle is tangent to cv1
c
      dx=x4-xc
      dy=y4-yc
      rerr=dsqrt(dx**2+dy**2)-ra     
      if(dabs(rerr).gt.0.001) goto 975
c
c..... check whether circle is tangent to ci1
c
      dx=rx-xc
      dy=ry-yc
      rerr=dsqrt(dx**2+dy**2)-ro     
      if(dabs(rerr).gt.0.001) goto 990

c
c..... change from 360 circle to finite arc -- add ok plane to w(8-11)
c
      if (ro .lt. 0.001) then
           ifl(2)=165
           goto 999
      endif
      ro=rc/ro
      xm=rx + (xc-rx)*ro
      ym=ry + (yc-ry)*ro
      a(1)=ym-y4    
      a(2)=x4-xm    
      a(3)=0.0      
c
c..... make sure the smaller arc is chosen
c
      if ( a(1)*(x4-xc)+a(2)*(y4-yc) .lt. 0.0) then 
        a(1)=-a(1)
        a(2)=-a(2)
      endif

      aa=f_mag(a)
      if (aa .lt. 0.001) then
           ifl(2)=32 
           goto 999
      endif
      w(11)=(x4*a(1)+y4*a(2))/aa
      call unitizevc(a)
      w(8)=a(1)     
      w(9)=a(2)     
      goto 999

c***************    ci/tanto,cv1,xl,pt1,radius,r               -- 20 --
290   continue                   

      cvmx=0.
      cvmy=0.
      if(sc(12).eq.638.) cvmx=1.
      if(sc(12).eq.641.) cvmx=-1.
      if(sc(12).eq.639.) cvmy=1.
      if(sc(12).eq.642.) cvmy=-1.
c
c..... get pt1 (or pv1) in p
c
      call gtentt(sc(13), trflg, nclkey, ietype, p(1))
      ra=sc(14)
      if (ra .lt. 0.001) then
         ifl(2)=165
         goto 999
      endif
c
c..... (rx,ry) are coordinates of pt1
c
      rx=p(1)
      ry=p(2)
c
c...evaluate ncl curve like bspline using evaluator
c
        call gtdesc(sc(11), nclkey, nwds, ietype)
        isrf=3
        call evstup(nclkey, isrf)
c
c..... curve data is passed to ucrvpv as dtbl 
c
        idx = 50*(isrf-1)
        d(idx+2) = sc(11)
        kdx = idx*4
        kd(kdx+2)=1
        kd(kdx+3)=1
c
c..... find an initial segment close to pt1
c
      dis=1.d12
      du = 1.d0/9.d0
      vv = 0.d0
      uu = 0.d0

      do 295 i=1,10
          if (vv.gt.1.0d0) vv = 1.0d0
          call uevcvt (vv,isrf,ifl(72),a,b,ifl(2))
          adis = f_dist(p,a)
          if (adis.lt.dis) then
            dis = adis
            uu = vv
          endif
          vv=vv+du
295   continue

c
c..... curve parameter and point pt1 are passed to ucrvpv as t 
c
      iux = 25    
      t(iux,ia) = uu
c
c  ********** first run before starting iteration **********
c
      call conv8_4 (p,t(1,1),3)
c
c..... ifl(72)=1 if refsys is in effect.
c..... then sc(56)-sc(67) contain the current refsys matrix
c..... sc(68)-sc(79) contain its inverse
c
      if (ifl(72).eq.1) then
        call conv4_8 (t(1,1),a,3)
        call conent(a,sc(56),i2v3)
        call conv8_4 (a,t(1,1),3)
      endif
 
      call ucrvpv (x4,y4,z4,a4,b4,c4)
      if (ifl(2).gt.0) goto 999

      a(1) = x4
      a(2) = y4
      a(3) = z4
      b(1) = a4
      b(2) = b4
      b(3) = c4
      if (ifl(72).eq.1) then
         call conent(a,sc(68),i2v3)
         call conent(b,sc(68),i2v4)
      endif
c
c..... here uu is 0 or 1, so we extend cv1 tangentially 
c
      if ((uu.gt.0. .and. uu.lt.1.) .or. ind.eq.0) then     
         call vcmnvc(a,p,ap)
         dis=dabs(f_dot(ap,b))
         if (uu .le. 0.) dis=-dis
         call uvcplvc(a,b,a,dis)
      endif
      x4 = a(1)
      y4 = a(2)
      z4 = a(3)
      a4 = b(1)
      b4 = b(2)
      c4 = b(3)
      fx=a4    
      fy=b4      
      fz=c4      
c
c..... Error exit if pt1 is too far from cv1.              
c
      adis = f_dist(p,a)
      if (adis.gt.2.*ra) goto 990

      sec = dsqrt(fx**2+fy**2)
      if (sec.le.0.001) goto 975
      fx = fx/sec
      fy = fy/sec
c
c..... (fx,fy) is normalized tangent vector to (horizontal
c..... projection of) cv1 at (x4,y4)
c
      xn = -fy
      yn = fx
c
c..... compute the next iteration for the center point (xc,yc)
c
      acv=rx-x4
      bcv=ry-y4
      if (acv*xn+bcv*yn.lt.0.) then      
         xn = -xn
         yn = -yn
      endif
c
c..... (xn,yn) is the normal direction to cv1 at (x4,y4), pointing 
c..... toward pt1.
c
      ax=x4+xn*ra
      ay=y4+yn*ra
c
c..... (ax,ay) is at the distance ra from (x4,y4) 
c
      ccv=dabs(adis - ra)     
c
c..... ccv is the distance from the pt1 to the line  
c..... ln thru (ax,ay) in the tangent (fx,fy) direction
c
      rlcrv=1.0
      if (fx*cvmx+fy*cvmy .lt. 0.) then 
         fx=-fx
         fy=-fy
         rlcrv=-1.0
      endif

      if (ccv .lt. ra) then
          chd=dsqrt(ra**2-ccv**2)
      else 
          chd=0.0
      endif
c
c..... find intersection of line ln and circle with radius ra:
c..... this is the next iteration for the center point
c
      xc = ax + fx*chd
      yc = ay + fy*chd
c
c..... ferr is how far from cv1 the circle touches the tangent line
c
      ferr=fx*(xc-p(1))+fy*(yc-p(2))
      if(dabs(ferr).lt.ermx) goto 350
c
c..... calculate df
c
      df = 0.1*ferr
      if(dabs(df) .gt. 0.1) then
         if (df .gt. 0) then
            df = 0.1
         else
            df = -0.1
         endif
      endif

      ofer=ferr
c
c..... set next near point and try again
c
      uu=t(25,1)
      p(1) = x4+fx*df
      p(2) = y4+fy*df
      p(3) = z4+fz*df

      if (uu.gt.0.0d0 .and. uu.lt.1.0d0) then     
         ind = 1
      else if ((uu .le. 0.0) .and. (df*rlcrv .lt. 0.0)) then 
            ind = 1
      else if ((uu .ge. 1.0) .and. (df*rlcrv .gt. 0.0)) then 
            ind = 1
      endif

      ktim=1
c
c  ********** start of iteration loop **********
c
300   if (ktim.gt.256) goto 975
      call conv8_4 (p,t(1,1),3)
c
c..... ifl(72)=1 if refsys is in effect.
c..... then sc(56)-sc(67) contain the current refsys matrix
c..... sc(68)-sc(79) contain its inverse
c
      if (ifl(72).eq.1) then
        call conv4_8 (t(1,1),a,3)
        call conent(a,sc(56),i2v3)
        call conv8_4 (a,t(1,1),3)
      endif
 
      if ((uu.gt.0.0d0 .and. uu.lt.1.0d0) .or. ind.eq.0) then     
         call ucrvpv (x4,y4,z4,a4,b4,c4)
         if (ifl(2).gt.0) goto 999

         a(1) = x4
         a(2) = y4
         a(3) = z4
         b(1) = a4
         b(2) = b4
         b(3) = c4
         if (ifl(72).eq.1) then
           call conent(a,sc(68),i2v3)
           call conent(b,sc(68),i2v4)
         endif
         x4 = a(1)
         y4 = a(2)
         z4 = a(3)
         a4 = b(1)
         b4 = b(2)
         c4 = b(3)
         fx=a4    
         fy=b4      
         fz=c4      
      else
c
c..... here uu is 0 or 1, so we extend cv1 tangentially 
c
         x4=p(1)
         y4=p(2)
         z4=p(3)
      endif

      sec = dsqrt(fx**2+fy**2)
      if (sec.le.0.001) goto 975
      fx = fx/sec
      fy = fy/sec
c
c..... (fx,fy) is normalized tangent vector to (horizontal
c..... projection of) cv1 at (x4,y4)
c
      xn = -fy
      yn = fx
c
c..... compute the next iteration for the center point (xc,yc)
c
      acv=rx-x4
      bcv=ry-y4
      if (acv*xn+bcv*yn.lt.0.) then      
         xn = -xn
         yn = -yn
      endif
c
c..... (xn,yn) is the normal direction to cv1 at (x4,y4), pointing 
c..... toward pt1.
c
      ax=x4+xn*ra
      ay=y4+yn*ra
c
c..... (ax,ay) is at the distance ra from (x4,y4) 
c
      dx=rx-ax
      dy=ry-ay
      ccv=dabs(xn*dx + yn*dy)     
c
c..... ccv is the projection of the interval (ax,ay) - (rx,ry)
c..... onto the normal, i.e.,  
c..... ccv is the distance from the center of ci1 to the line  
c..... ln thru (ax,ay) in the tangent (fx,fy) direction
c
      den=fx*dx + fy*dy     
c
c..... den is the projection of the interval (ax,ay) - (rx,ry)
c..... onto the line ln
c
      rlcrv=1.0
      if (den .lt. 0.) then 
         rlcrv=-1.0
         fx=-fx
         fy=-fy
         den=-den
      endif

      if (ccv .lt. ra) then
          chd=dsqrt(ra**2-ccv**2)
      else 
          chd=0.0
      endif

c
c..... find intersection of line ln and circle with radius ra:
c..... this is the next iteration for the center point
c
      xc = ax + fx*(den-chd)
      yc = ay + fy*(den-chd)
c
c..... ferr is how far from cv1 the circle touches the tangent line
c
      ferr=fx*(xc-p(1))+fy*(yc-p(2))
      if(dabs(ferr).lt.ermx) goto 350
c
c..... calculate df
c
      if(ktim.le.0) then
         df = 0.1*ferr
      else
         den = ofer-ferr
         if(den.eq.0.) then
            ifl(2)=268
            goto 999
         endif
         df = df*ferr/den
      endif

      if(dabs(df) .gt. 0.1) then
         if (df .gt. 0) then
            df = 0.1
         else
            df = -0.1
         endif
      endif

      ofer=ferr
c
c..... set next near point and try again
c
      uu=t(25,1)
      p(1) = x4+fx*df
      p(2) = y4+fy*df
      p(3) = z4+fz*df

      if (uu.gt.0.0d0 .and. uu.lt.1.0d0) then     
         ind = 1
      else if ((uu .le. 0.0) .and. (df*rlcrv .lt. 0.0)) then 
            ind = 1
      else if ((uu .ge. 1.0) .and. (df*rlcrv .gt. 0.0)) then 
            ind = 1
      endif

      ktim=ktim+1
      goto 300
c
c*********** end of iteration loop ************
c
350   w(1)=xc
      w(2)=yc
      w(7)=ra    
c
c..... check whether circle is tangent to cv1
c
      dx=x4-xc
      dy=y4-yc
      rerr=dsqrt(dx**2+dy**2)-ra     
      if(dabs(rerr).gt.0.001) goto 975
c
c..... check whether circle is tangent to ci1
c
      dx=rx-xc
      dy=ry-yc
      rerr=dsqrt(dx**2+dy**2)-ra     
      if(dabs(rerr).gt.0.001) goto 990

      goto 999

c***************    ci/center,pt1,tanto,cv1                    -- 21 --
390   continue                   

c
c..... get pt1 (or pv1) in p
c
      call gtentt(sc(11), trflg, nclkey, ietype, p(1))
c
c..... (rx,ry) are coordinates of pt1
c
      rx=p(1)
      ry=p(2)
c
c...evaluate ncl curve like bspline using evaluator
c
        call gtdesc(sc(11), nclkey, nwds, ietype)
        isrf=3
        call evstup(nclkey, isrf)
c
c..... curve data is passed to ucrvpv as dtbl 
c
        idx = 50*(isrf-1)
        d(idx+2) = sc(12)
        kdx = idx*4
        kd(kdx+2)=1
        kd(kdx+3)=1
c
c..... find an initial segment close to pt1
c
      dis=1.d12
      du = 1.d0/9.d0
      vv = 0.d0
      uu = 0.d0

      do 395 i=1,10
          if (vv.gt.1.0d0) vv = 1.0d0
          call uevcvt (vv,isrf,ifl(72),a,b,ifl(2))
          adis = f_dist(p,a)
          if (adis.lt.dis) then
            dis = adis
            uu = vv
          endif
          vv=vv+du
395   continue

c
c..... curve parameter and point pt1 are passed to ucrvpv as t 
c
      iux = 25    
      t(iux,ia) = uu

      call conv8_4 (p,t(1,1),3)
c
c..... ifl(72)=1 if refsys is in effect.
c..... then sc(56)-sc(67) contain the current refsys matrix
c..... sc(68)-sc(79) contain its inverse
c
      if (ifl(72).eq.1) then
        call conv4_8 (t(1,1),a,3)
        call conent(a,sc(56),i2v3)
        call conv8_4 (a,t(1,1),3)
      endif
 
         call ucrvpv (x4,y4,z4,a4,b4,c4)
         if (ifl(2).gt.0) goto 999
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
         fx=a4    
         fy=b4      
         fz=c4      


      ra=dsqrt((x4-rx)**2+(y4-ry)**2)
      if (ra.le.0.001) goto 975

      if (uu.le.0.0d0 .or. uu.ge.1.0d0) then     
c
c..... here uu is 0 or 1, so we extend cv1 tangentially 
c
      chd=(x4-rx)*fx+(y4-ry)*fy
      ra=dsqrt(ra*ra - chd*chd)
      endif

      w(1)=rx
      w(2)=ry
      w(7)=ra    
      goto 999

c***************    ci/xyls,cv1,xyls,cv2,pt3,radius,r    -- 22 --
490   continue                   

      cvmx=0.
      cvmy=0.
      if(sc(11).eq.638.) cvmx=1.
      if(sc(11).eq.641.) cvmx=-1.
      if(sc(11).eq.639.) cvmy=1.
      if(sc(11).eq.642.) cvmy=-1.

      cvmx2=0.
      cvmy2=0.
      if(sc(13).eq.638.) cvmx2=1.
      if(sc(13).eq.641.) cvmx2=-1.
      if(sc(13).eq.639.) cvmy2=1.
      if(sc(13).eq.642.) cvmy2=-1.
c
c..... get pt3 (or pv3) in p
c
      call gtentt(sc(15), trflg, nclkey, ietype, p(1))

c
c..... ra is radius of circle under construction
c
      ra = sc(16)
      if (ra .lt. 0.001) then
         ifl(2) = 165
         goto 999
      endif
c
c...evaluate ncl curve cv1 like bspline using evaluator
c
      call gtdesc(sc(12), nclkey, nwds, ietype)
      isrf = 1
      call evstup(nclkey, isrf)
c
c..... curve data is passed to ucrvpv as dtbl 
c
      idx = 50*(isrf-1)
      d(idx+2) = sc(12)
      kdx = idx*4
      kd(kdx+2) = 1
      kd(kdx+3) = 1
c
c..... find an initial segment of cv1 close to pt3
c
      dis = 1.d12
      du = 1.d0/9.d0
      vv = 0.d0
      uu = 0.d0

      do 494 i=1,10
          if (vv.gt.1.0d0) vv = 1.0d0
          call uevcvt (vv,isrf,ifl(72),a,b,ifl(2))
          adis = f_dist(p,a)
          if (adis.lt.dis) then
            dis = adis
            uu = vv
          endif
          vv=vv+du
494   continue

c
c..... curve parameter and point pt3 are passed to ucrvpv as t 
c
      ihx=6*isrf+9
      iux = ihx - 2
      t(iux,ia) = uu
c
c...evaluate ncl curve cv2 like bspline using evaluator
c
      isrf = 3
      ia = 3
      call gtdesc(sc(14), nclkey2, nwds, ietype)
      call evstup(nclkey2, isrf)
c
c..... curve data is passed to ucrvpv as dtbl 
c
      idx = 50*(isrf-1)
      d(idx+2) = sc(14)
      kdx = idx*4
      kd(kdx+2) = 1
      kd(kdx+3) = 1
c
c..... find an initial segment ov cv2 close to pt3
c
      dis = 1.d12
      du = 1.d0/9.d0
      vv = 0.d0
      uu = 0.d0

      do 495 i=1,10
          if (vv.gt.1.0d0) vv = 1.0d0
          call uevcvt (vv,isrf,ifl(72),a,b,ifl(2))
          adis = f_dist(p,a)
          if (adis.lt.dis) then
            dis = adis
            uu = vv
          endif
          vv=vv+du
495   continue

c
c..... curve parameter and point pt3 are passed to ucrvpv as t 
c
      ihx=6*isrf+9
      iux = ihx - 2
      t(iux,ia) = uu
c
c  ******** start of iteration loop **********
c
c
500   if (ktim.gt.256) goto 975
      ia = 3
      isrf = 3
      call conv8_4 (p,t(1,ia),3)
c
c..... ifl(72)=1 if refsys is in effect.
c..... then sc(56)-sc(67) contain the current refsys matrix
c..... sc(68)-sc(79) contain its inverse
c
      if (ifl(72).eq.1) then
        call conv4_8 (t(1,ia),a,3)
        call conent(a,sc(56),i2v3)
        call conv8_4 (a,t(1,ia),3)
      endif
 
      if ((uu.gt.0.0d0 .and. uu.lt.1.0d0) .or. ind.eq.0) then     
         call ucrvpv (x4,y4,z4,a4,b4,c4)
         if (ifl(2).gt.0) goto 999
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
         fx=a4    
         fy=b4      
         fz=c4      
      else
c
c..... here uu is 0 or 1, so we extend cv2 tangentially 
c
         x4=p(1)
         y4=p(2)
         z4=p(3)
      endif
c
c..... (fx,fy) is the unitized tangent vector to (horizontal
c..... projection of) cv2 at (x4,y4)
c
      sec = dsqrt(fx**2+fy**2)
      if (sec.le.0.001) goto 975
      fx = fx/sec
      fy = fy/sec
c
c..... (acv,bcv) is a normal direction to cv2 at (x4,y4)
c
      acv=fy
      bcv=-fx
      if (acv*cvmx2+bcv*cvmy2.lt.0.) then      
         acv=-acv
         bcv=-bcv
      endif
c
c..... (ax,ay) is at the distance ra from (x4,y4) 
c
      ax=x4+acv*ra
      ay=y4+bcv*ra

c
c..... Project (ax,ay) on cv1
c
      ia = 1
      isrf = 1
      ihx=6*isrf+9
      iux = ihx - 2
      p(1) = ax
      p(2) = ay
      call conv8_4 (p,t(1,ia),3)
 
      if (ifl(72).eq.1) then
        call conv4_8 (t(1,ia),a,3)
        call conent(a,sc(56),i2v3)
        call conv8_4 (a,t(1,ia),3)
      endif
 
      call ucrvpv (xx4,yy4,zz4,aa4,bb4,cc4)
      if (ifl(2).gt.0) goto 999
      if (ifl(72).eq.1) then
           a(1) = xx4
           a(2) = yy4
           a(3) = zz4
           b(1) = aa4
           b(2) = bb4
           b(3) = cc4
           call conent(a,sc(68),i2v3)
           call conent(b,sc(68),i2v4)
           aa4 = b(1)
           bb4 = b(2)
           cc4 = b(3)
           xx4 = a(1)
           yy4 = a(2)
           zz4 = a(3)
      endif
      uu=t(25,ia)
      if (uu.le.0.0 .or. uu.ge.1.0) then
         chd=(ax-xx4)*aa4+(ay-yy4)*bb4
         xx4=xx4+aa4*chd
         yy4=yy4+bb4*chd
      endif
 
      dx = ax - xx4
      dy = ay - yy4
      rlcrv = 1.0
      if ((cvmx*dx + cvmy*dy) .lt. 0.) rlcrv=-1.0 
c
c..... ferr is the difference between the distance of (ax,ay) to cv1
c..... the the radius ra
c
      adis = dsqrt (dx**2 + dy**2)
      ferr = adis - rlcrv*ra
      if (dabs(ferr) .lt. ermx) goto 550
c
c..... calculate df
c
      if(ktim.le.0) then
         df = 0.1*ferr
      else
         den = ofer-ferr
         if(den.eq.0.) then
            ifl(2)=268
            goto 999
         endif
         df = df*ferr/den
      endif

      if (dabs(df) .gt. 0.1) then
         if (df .gt. 0) then
            df = 0.1
         else
            df = -0.1
         endif
      endif

      ofer=ferr
c
c..... set next near point and try again
c
      ia = 3
      isrf = 3
      uu=t(25,ia)
      p(1) = x4+fx*df
      p(2) = y4+fy*df
      p(3) = z4+fz*df

      if (uu.gt.0.0d0 .and. uu.lt.1.0d0) then     
         ind = 1
      else if ((uu .le. 0.0) .and. (df*rlcrv .lt. 0.0)) then 
            ind = 1
      else if ((uu .ge. 1.0) .and. (df*rlcrv .gt. 0.0)) then 
            ind = 1
      endif

      ktim=ktim+1
      goto 500
c
c*********** end of iteration loop ************
c
550   w(1)=ax
      w(2)=ay
      w(7)=ra    
c
c..... check whether circle is tangent to cv1
c
      dx = xx4-ax
      dy = yy4-ay
      rerr = dsqrt(dx**2+dy**2) - ra     
      if (dabs(rerr) .gt. 0.001) goto 975
c
c..... check whether circle is tangent to cv1
c
      dx = x4-ax
      dy = y4-ay
      rerr = dsqrt(dx**2+dy**2) - ra     
      if (dabs(rerr).gt.0.001) goto 990

c
c..... change from 360 circle to finite arc -- add ok plane to w(8-11)
c
      a(1)=yy4-y4    
      a(2)=x4-xx4    
      a(3)=0.0      
c
c..... make sure the smaller arc is chosen
c
      if ( a(1)*(x4-ax)+a(2)*(y4-ay) .lt. 0.0) then 
        a(1)=-a(1)
        a(2)=-a(2)
      endif

      aa=f_mag(a)
      if (aa .lt. 0.001) then
           ifl(2)=32 
           goto 999
      endif

      w(11)=(x4*a(1)+y4*a(2))/aa
      call unitizevc(a)
      w(8)=a(1)     
      w(9)=a(2)     
      goto 999

c
c..... error exit: curve is no good for this circle definition
c
975   ifl(2)=268 
      go to 999

c***** error exit: impossible geometry
990   if(ifl(2).lt.1) ifl(2)=163

999   return
      end
