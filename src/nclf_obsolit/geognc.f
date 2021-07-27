C*********************************************************************
C*    NAME         :  geognc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       geognc.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:06
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine geognc
C*      last revision: add pl/8      pem   8-mar-84                     
C*                                                                   
C*   prepare and store  line,plane canon lists.
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
      subroutine geognc
      include 'com8a.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld,key0
      integer*2 istold, idstsv,ist0
      character*64 labsav 


      integer*2 isc(100),ktv(4),ksn(4)
      real*8 w(15),a(35),u(15),t(15),dis,secsq,co,rgt,cosa,sina
      real*8 dx,dy,dz,pdir,tval
      real*4 mmorin

      integer*4 nclkey,svinx
      integer*2  ietype, plnr, iflg
      logical trflg

      equivalence (a,jb),(sc(10),isc),(tv,ktv)
      equivalence (asn,ksn)
      integer*2 isc13(4)
      equivalence(isc13,sc(13))
      logical nored

      real*8 rtok
      logical leq1
      equivalence (token2,leq1),(rtok,leq1)

      trflg = .true.

      if (ifl(264).eq.0) then
c                     units are inches
          mmorin = 1
      else
c                     units are millimeters
          mmorin = 25.4
      endif

      radian=57.2957795d0
      nored=.false.
      ktv(4)=idst
      isub=isc(2)
c          branch to geo type per idst
290   if(idst-6)300,400,990
c******************************************************  line
300   ktv(3)=6
      if(isub-2)310,320,329
c
c     **********************   ln/pt1,pt2                         -- 1 --
c                                 pt1,pv2
c                                 pv1,pv2
c                                 pv1,pt2
c
310   call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, w(4))
      w(4)=w(4)-w(1)
      w(5)=w(5)-w(2)
      w(6)=w(6)-w(3)
c     if(idst.eq.4)goto 252
      goto 390
c
c     **********************   ln/pt1,pa,ln2                      -- 2 --
c                                 pv1,pa,ln2
c                                 pt1,pa,pv2
c                                 pv1,pa,pv2
c          get ln2 (pv2), insert pt1 (pv1) into first 3.
320   continue
      call gtentt(sc(12), trflg, nclkey, ietype, w(1))
      call gtentt(sc(11), trflg, nclkey, ietype, u(1))
      w(1) = u(1)
      w(2) = u(2)
      w(3) = u(3)
      goto 390
c          cont ln subtype branching
329   if(isub-4)330,340,349
c     **********************   ln/pa,ln1,xyls,trdist  (2d move)   -- 3 --
c          get ln1 and erect unit normal   ( sto temp in w7,8 )
330   continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      secsq=w(4)**2+w(5)**2
      if(secsq.gt.1.d-6)goto 332
c          error. ln1 appears vertical
331   ifl(2)=44
      goto 990
332   sec=dsqrt(secsq)
      w(7)=-w(5)/sec
      w(8)=w(4)/sec
c          assume move in above direction
      pdir=1.d0
      tval=1.d0
      iwx=sc(12)-637.d0
      if(iwx.lt.4)goto 334
      iwx=iwx-3
      tval=-tval
c          move ptr to w(7,8) area
334   iwx=iwx+6
      if(tval*w(iwx))335,3340,336
c          move computes to be zero (?)
3340  ifl(2)=45
      goto 990
335   pdir=-pdir
c          make the move
336   w(1)=w(1)+pdir*sc(13)*w(7)
      w(2)=w(2)+pdir*sc(13)*w(8)
      goto 390
c     **********************   ln/fwd                             -- 4 --
340   do 342 i=1,3
      w(i)=sc(i)
342   w(i+3)=sc(i+6) * mmorin
      goto 390
c          cont line,sub branching
349   if(isub-6)350,360,369
c
c     **********************   ln/pt1,rgtlft,tanto,ci1            -- 5 --
c                                 pv1,rgtlft,tanto,ci1
c          get pt1 in w(1), ci1 in u(1)
c
350   continue
cccccccc           circle only   (temp)     12-may-83
      if(isc(16).ne.7)goto 990
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(13), trflg, nclkey, ietype, u(1))
c          circle must be in z-plane
      if(u(4)**2+u(5)**2.lt.1.d-4)goto 352
c          error. canted circle
      ifl(2)=161
      goto 990
352   dx=u(1)-w(1)
      dy=u(2)-w(2)
      chd=dsqrt(dx**2+dy**2)
      if(chd.lt..001)goto 353
      sina=u(7)/chd
      if(sina**2.lt.1.)goto 354
c          error. pt must be outside circle
353   ifl(2)=162
      goto 990
354   cosa=dsqrt(1.-sina**2)
      rgt=1.
      if(sc(12).eq.8.)rgt=-1.
      rldis=u(7)*cosa*rgt
      bkdis=u(7)*sina
      cdx=dx/chd
      cdy=dy/chd
      w(4)=dx+rldis*cdy-bkdis*cdx
      w(5)=dy-rldis*cdx-bkdis*cdy
      w(6)=0.
      goto 390
c     **********************   ln/rl,tanto,ci1,rl,tanto,ci2       -- 6 --
c          get ci1 in u, ci2 in t
360   continue
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
      call gtentt(sc(14), trflg, nclkey, ietype, t(1))
c          circles may not be canted
      if(u(4)**2+u(5)**2.gt.1.d-4)goto 361
      if(t(4)**2+t(5)**2.lt.1.d-4)goto 362
361   ifl(2)=161
      goto 990
362   dx=t(1)-u(1)
      dy=t(2)-u(2)
      chd=dsqrt(dx**2+dy**2)
      if (chd.eq.0) goto 363
      rgt1=1.
      rgt2=1.
      if(sc(11).eq.8.)rgt1=-1
      if(sc(13).eq.8.)rgt2=-1
      sina=(rgt2*t(7)-rgt1*u(7))/chd
      if(sina**2.lt.1.)goto 364
c          error. impossible geometry
363   ifl(2)=163
      goto 990
364   cosa=dsqrt(1.-sina**2)
      cdx=dx/chd
      cdy=dy/chd
      tlx=cdx*cosa+cdy*sina
      tly=cdy*cosa-cdx*sina
      w(1)=u(1)+tly*u(7)*rgt1
      w(2)=u(2)-tlx*u(7)*rgt1
      w(3)=u(3)
      w(4)=t(1)+tly*t(7)*rgt2-w(1)
      w(5)=t(2)-tlx*t(7)*rgt2-w(2)
      w(6)=0.
      goto 390
369   if(isub-8)370,380,389
c      ***********************   ln/xaxis or yaxis                -- 7 --
c                zero w(1-6)
370   do 372 i=1,6
372   w(i)=0.
      if(sc(11).ne.84)goto 374
      w(4)=mmorin
      if(isc(3).eq.2)w(2)=sc(12)
      goto 390
374   if(sc(11).ne.85.) goto 990
      w(5)=mmorin
      if(isc(3).eq.2) w(1)=sc(12)
      goto 390
c
c      ***********************   ln/pt1,aa,alf,xaxis-yaxis        -- 8 --
c
380   alf=sc(12)
      if(isc13(4).eq.84.or.isc13(4).eq.85) then
          if (isc13(4).eq.84) go to 382
          if (isc13(4).eq.85) then
              alf=alf+90
              go to 382
          endif
          go to 990
      endif
c            
c                                ln/pt1,aa,alf,ln2        
c                                ln/pt1,aa,alf,pv2        
c                                ln/pv1,aa,alf,pv2        
c                                ln/pv1,aa,alf,ln2        
c
      call gtentt(sc(13), trflg, nclkey, ietype, w(1))
      if (w(4).eq.0. .and. w(5).eq.0.) then
          ifl(2)=31
          go to 990
      endif

c          calculate angle from x-axis based on arctan of (y-value/x-value)
      alf=alf+datan2(w(5),w(4))*radian
c             get pt1 in w(1-3)
382   continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      alf=alf/radian
      w(4)=dcos(alf) * mmorin
      w(5)=dsin(alf) * mmorin
      w(6)=0
      goto 390
389   if(isub.gt.9)goto 3891
c     ****************************  ln/pt1,perpto,ln2             -- 9 --
c                                      pt1,perpto,pv2
c                                      pv1,perpto,ln2
c                                      pv1,perpto,pv2
c                                      
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
c                   make ln go from pt to ln (2d only)       17-dec-86
      aa=u(4)
      b=u(5)
      den=aa**2+b**2
      if (den.lt.1.d-7)goto 331
      ro=(aa*(w(1)-u(1))+b*(w(2)-u(2)))/den
      dx=u(1)+ro*aa-w(1)
      dy=u(2)+ro*b-w(2)
      dsq=dx**2+dy**2
c               if dist pt1 to ln1 very small, use same mag as ln1
      if (dsq.lt.1.d-5)goto 3890
      w(4)=dx
      w(5)=dy
      w(6)=0.
      go to 390
3890  continue
      w(4)=-u(5)
      w(5)=u(4)
      w(6)=0.
      goto 390
ccccccccccccccccccc   ln/11,12,13      (ln/10 goes to geognd)
3891  if(isub.gt.13)goto 3896
      if(isub-12)3892,3893,3894
c******************************     ln/a,b,c,d<,e,f>             -- 11 --
3892  if(isc(3).gt.4)goto 38922
c              4 values given.  fix sc to hold 6
      sc(15)=sc(14)
      sc(14)=sc(13)
      sc(16)=0.
      sc(13)=0.
38922 do 38924 i=1,3
      w(i)=sc(i+10)
38924 w(i+3)=sc(i+13)-sc(i+10)
      goto 390
c******************************     ln/intof,pl1,pl2             -- 12 --
3893  continue
      call gtplt(sc(11), ifl(72), u(1))
      call gtplt(sc(12), ifl(72), t(1))
c                cross pls for lnvecs and unitize.
      w(4)=u(2)*t(3)-u(3)*t(2)
      w(5)=u(3)*t(1)-u(1)*t(3)
      w(6)=u(1)*t(2)-u(2)*t(1)
      sec=dsqrt(w(4)**2+w(5)**2+w(6)**2)
      if(sec.lt..00005)goto 391
      w(4)=w(4)/sec * mmorin
      w(5)=w(5)/sec * mmorin
      w(6)=w(6)/sec * mmorin
c                solve a pt per origin pt
      co=u(1)*t(1)+u(2)*t(2)+u(3)*t(3)
      den=1.-co**2
      d2=(t(4)-u(4)*co)/den
      d1=(u(4)-t(4)*co)/den
      w(1)=u(1)*d1+t(1)*d2
      w(2)=u(2)*d1+t(2)*d2
      w(3)=u(3)*d1+t(3)*d2
      goto 390
c*****************************  ln/xyls,ci1,atangl,ang,axis<line>  -- 13 --
3894  continue
c                     get circle, check for tipped.
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
      if(u(4)**2+u(5)**2.gt.1.d-4)goto 361
c                     sc(14) line or axis
      call gtdesc(sc(14),nclkey,nwds,ietype)
      if(nclkey.gt.0)goto 38944
c                      axis. zero a(1-3) and chg 1 or 2 to 1.0
      do 38941 i=1,3
38941 a(i)=0.
      if(isc(20).ne.84.and.isc(20).ne.85)goto 990
      if(isc(20).eq.84) a(1)=1.
      if(a(1).eq.0.) a(2)=1.
      goto 38946
c                      get ln1.  check for ok in xy view.
38944 continue
      call gtentt(sc(14), trflg, nclkey, ietype, t(1))
      sec=dsqrt(t(4)**2+t(5)**2)
      if(sec.lt..002)goto 363
      a(1)=t(4)/sec
      a(2)=t(5)/sec
      a(3)=0.
38946 ang=sc(13)/radian
      si=dsin(ang)
      co=dcos(ang)
c                      build lnvec in w(4,5)
      w(4)=co*a(1)-si*a(2)
      w(5)=co*a(2)+si*a(1)
      w(6)=0.
      aa=-w(5)
      bb=w(4)
c                 very small direction numbers are assumed to be zero.
      if(dabs(aa).lt.1.d-7) aa=0.
      if(dabs(bb).lt.1.d-7) bb=0.
c                      compare to xyls
      if(sc(11).ne.638.)goto 38947
      if(aa)38950,363,38952
38947 if(sc(11).ne.641.)goto 38948
      if(aa)38952,363,38950
38948 if(sc(11).ne.639.)goto 38949
      if(bb)38950,363,38952
38949 if(sc(11).ne.642.)goto 363
      if(bb)38952,363,38950
38950 aa=-aa
      bb=-bb
38952 w(1)=u(1)+u(7)*aa
      w(2)=u(2)+u(7)*bb
      w(3)=u(3)
      w(4)=w(4)*mmorin
      w(5)=w(5)*mmorin
      goto 390

c     **********************   ln/ln1,<xval>,<yval>,<zval>        -- 14 --
c                                 pv1
3896  if(isub-15)38962,3897,990
38962 call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      if(isc(3).eq.0)goto 390
      w(1)=w(1)+sc(12)
      if(isc(3).gt.1) w(2)=w(2)+sc(13)
      if(isc(3).gt.2) w(3)=w(3)+sc(14)
      goto 390
c      ********************   ln/pl1                              -- 15 --
3897  call gtplt(sc(11), ifl(72), u(1))
      sec=dsqrt(u(1)**2+u(2)**2)
      if (sec.lt.1.d-6) then
           ifl(2)=322
           goto 990
      endif
      w(4)=-mmorin*u(2)/sec
      w(5)=mmorin*u(1)/sec
      w(6)=0.
      den=1.-u(3)**2
      d1=u(4)/den
      w(1)=u(1)*d1
      w(2)=u(2)*d1
      w(3)=0.
      goto 900
c          line check area. length must be gt 0.
390   if(w(4)**2+w(5)**2+w(6)**2.gt.1.d-6)goto 900
391   ifl(2)=31
      goto 990
c
c*******************************************************  plane
c
400   ktv(3)=4
      if(isub-2)410,640,429
c     **********************  pl/a,b,c,d          ( ax+by+cz=d )  -- 1 --
410   do 412 i=1,4
412   w(i)=sc(i+10)
c          goto plchk area
      goto 490
c     **********************  pl/pt1,pt2,pt3                      -- 2 --
c          get 3 pts
640   do 422 i=1,3
         j=3*i+1
         call gtentt(sc(i+10), trflg, nclkey, ietype, w(j))
422   continue
c          chg pt2, pt3 to deltas
      do 424 i=4,6
      w(i+3)=w(i+3)-w(i)
424   w(i+6)=w(i+6)-w(i)
c          finish the equation and go plchk area
      w(1)=w(8)*w(12)-w(9)*w(11)
      w(2)=w(9)*w(10)-w(7)*w(12)
      w(3)=w(7)*w(11)-w(8)*w(10)
426   w(4)=w(1)*w(4)+w(2)*w(5)+w(3)*w(6)
      goto 490
429   if(isub-4)430,440,449
c     **********************  pl/pt1,pa,pl1                       -- 3 --
c          get pl1 and then pt1
430   continue
      call gtplt(sc(12), ifl(72), w(1))
      call gtentt(sc(11), trflg, nclkey, ietype, w(4))
      goto 426
c     **********************  pl/pa,pl1,xyzls,trdist              -- 4 --
440   continue
      call gtplt(sc(11), ifl(72), w(1))
c          assume pos move along plnorm
      pdir=1.0d0
      tval=1.0d0
      iwx=sc(12)-637.d0
      if(iwx.lt.4)goto 442
      iwx=iwx-3
      tval=-1.d0
c          if w(iwx) zero, direction modifier is invalid for this plane
442   if(dabs(tval*w(iwx)) .lt. 1.e-8) goto 445
      if(tval*w(iwx))443,445,444
443   pdir=-pdir
444   w(4)=w(4)+pdir*sc(13)
      goto 490
445   ifl(2)=213
      go to 990
449   if(isub-6)450,460,469
c450  **********************  pl/pt1,perpto,ve1                   -- 5 --
c          get ve1 and pt1.  do pl const.
450   continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      if (isub .eq. 5) then
          call gtentt(sc(12), trflg, nclkey, ietype, w(4))
          if (ietype .eq. 21) then
              w(4) = w(7)
              w(5) = w(8)
              w(6) = w(9)
          end if
      end if
451   dis = w(1)*w(4)+w(2)*w(5)+w(3)*w(6)
      w(1) = w(4)
      w(2) = w(5)
      w(3) = w(6)
      w(4) = dis 
      goto 490
c     **********************  pl/pt1,pt2,perpto,pl1               -- 6 --
460   continue
      call gtentt(sc(11), trflg, nclkey, ietype, u(1))
      call gtentt(sc(12), trflg, nclkey, ietype, u(4))
c              delta pt2
      dx=u(4)-u(1)
      dy=u(5)-u(2)
      dz=u(6)-u(3)
c              get pl1 in t-tbl and write object pl
      call gtplt(sc(13), ifl(72), t(1))
      w(1)=dy*t(3)-dz*t(2)
      w(2)=dz*t(1)-dx*t(3)
      w(3)=dx*t(2)-dy*t(1)
      w(4)=w(1)*u(1)+w(2)*u(2)+w(3)*u(3)
      goto 490
469   if(isub-8)470,480,489
c     **********************  pl/pt1,perpto,pl2,pl3               -- 7 --
470   continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(5))
      call gtplt(sc(12), ifl(72), u(1))
      call gtplt(sc(13), ifl(72), t(1))
      w(1)=u(2)*t(3)-u(3)*t(2)
      w(2)=u(3)*t(1)-u(1)*t(3)
      w(3)=u(1)*t(2)-u(2)*t(1)
      w(4)=w(1)*w(5)+w(2)*w(6)+w(3)*w(7)
      goto 490
c     **********************  pl/ln1<,perpto,pl1>                 -- 8 --
c                 get ln1 into u
480   continue
      call gtentt(sc(11), trflg, nclkey, ietype, u(1))
      if(isc(3).ne.1) goto 482
c                 no plane given.  load z=0 in t-tbl
      t(1)=0.
      t(2)=0.
      t(3)=1.
      t(4)=0.
      goto 484
c                 nwds must =2
482   if(isc(3).ne.2)goto 990
c                 get pl1 into t
      call gtplt(sc(12), ifl(72), t(1))
c                 target pl is normal to ln and pl1
484   w(1)=u(5)*t(3)-u(6)*t(2)
      w(2)=u(6)*t(1)-u(4)*t(3)
      w(3)=u(4)*t(2)-u(5)*t(1)
      w(4)=w(1)*u(1)+w(2)*u(2)+w(3)*u(3)
      isc(3)=0
c
c            Add the tag point to plane definition. kathy
c
      if (isc10(4).eq.0) then
         w(5) = u(1) + (u(4)/2.)
         w(6) = u(2) + (u(5)/2.)
         w(7) = u(3) + (u(6)/2.)
      endif
      goto 490
c
c...pl/pv1                                                        -- 9 --
c
489   if (isub .eq. 9) then
        sc(12) = sc(11)
        go to 450
c
c...     pl/pl1                                                  -- 10 --
c
      else if (isub .eq. 10) then
        call gtplt(sc(11), ifl(72), w)
        go to 900
c
c...     pl/pt1,parlel,ve1,ve2                                   -- 11 --
c
      else if (isub .eq. 11) then
        call gtentt(sc(11), trflg, nclkey, ietype, w(4))
        do i=1,2
          j=3*i+4
          call gtentt(sc(11+i), trflg, nclkey, ietype, w(j))
          if (ietype .eq. 21) then
            do k=0,2
              w(j+k) = w(j+k+3)
            enddo 
          endif
        enddo
        w(1)=w(8)*w(12)-w(9)*w(11)
        w(2)=w(9)*w(10)-w(7)*w(12)
        w(3)=w(7)*w(11)-w(8)*w(10)
        w(4)=w(1)*w(4)+w(2)*w(5)+w(3)*w(6)
        goto 490
c
c...     pl/fit,cv												-- 12 --
c
      else if (isub .eq. 12) then
        call gtdesc(sc(11),nclkey,nwds,ietype)
        if  (ietype .eq. CURVE .or. ietype .eq. SURF) then
          if (ietype .eq. CURVE) then
          call plcvsf (nclkey,1,w,plnr,iflg)
        else 
            call plcvsf (nclkey,0,w,plnr,iflg)
          endif
            if (iflg .eq. 1) then
              ifl(2) = 163
              goto 990
            endif
        else if (ietype .eq. CIRCLE) then
          call gtentt(sc(11), trflg, nclkey, ietype, w(1))
          plnr = 1
          else
          call error(223)
        endif
        t(1) = w(1)
        t(2) = w(2)
        t(3) = w(3)
        goto 451
      else
        goto 990
      endif
c
c...plane finup area. normal must have real length.
c
490   secsq=w(1)**2+w(2)**2+w(3)**2
      if(secsq.gt.1.d-9)goto 491
      ifl(2)=32
      goto 990
491   co=1.d0/dsqrt(secsq)
      do 492 i=1,4
492   w(i)=w(i)*co
      goto 900
c
c****************************** (matrix handled in geognb) *****
c500
c          geo store this item
900   ietype = ktv(4)
c
c          if there is a tag point for plane add it to w. kathy
      if (ietype .eq. 6 .and. isc10(4) .ne. 0) then
          itype = ietype
          nkey = nclkey
          call gtentt (sc(15),trflg,nclkey,ietype,w(5))
          nclkey = nkey
          ietype = itype
          ktv(4) = itype
c
c              Initialize the w array.  (for SGI and pl/fit  in particular)
      elseif (ietype .eq. 6 .and. isc10(4) .eq. 0) then
          if (isub .eq. 12) then
            w(5)=t(1)
            w(6)=t(2)
            w(7)=t(3)
          else
            w(5)=0.0
            w(6)=0.0
            w(7)=0.0
          endif
      endif
c
c          update wdknt and put ipg/iel in tv for vstore
c
	
	
      if (isub .eq. 12 .and. sc(199) .ne. 0) then
c
c.... update the planar scalar
c
          idstsv = idst
          labsav = savid2
          svinx = isvsub
          ist0 = istold
          key0 = keyold 
c
c...still keep the logic, just fill token2 9-64 with spaces
c
          do 100 i=1,64,1
              token2(i:i) = ' '
  100     continue
          rtok = sc(199)
          call vstchk
          rest = tv + plnr
          idst = 2
          keyold = keyhld
          istold = ist
          ifl(9) = ifl(11)
          ifl(10) = ifl(12)
          call vstore
          idst = idstsv
          savid2 = labsav
          isvsub = svinx
          istold = ist0
          keyold = key0
      endif
      call ptentt(ietype, w, nclkey, tv)
      rest = tv
      goto 999
c          error exit. set err and zero tv & 'rest'
990   err=.true.
      if(ifl(2).eq.0)ifl(2)=5
      tv=0.
      rest=0.
999   return
      end

