C*********************************************************************
C*    NAME         :  shapre.f
C*       CONTAINS:
C*                 shapre
C*                 shapre2
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       shapre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:42
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine shapre
C*       description
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
      subroutine shapre

      include 'com8a.com'
      include 'const.com'

      common/wblok/w(144),a(12),b(12),xnd,ynd,errb
cc      common/pblok/p(400)
      real*8 p(1000)
      integer*2 ksc(500),kw(576),ksn(4),kp(1600),ka(48),kb(48),ktv(4)
      integer*4 nclkey
      integer*2 ietype,ksn4
      logical trflg
      equivalence (sc,ksc),(w,kw),(asn,ksn),(p,kp),(a,ka),(b,kb)
      equivalence (ksn4,ksn(4))
      real*4 ang(2)
      equivalence (tv,ktv),(pang,ang)

      integer*2 maxpts,npts
      parameter (maxpts=100)
      real*8 pts(maxpts*2)
      integer*4 ncevolve2d

      trflg = .false.
      kclw=0
      kclwa=0
      kclwb=0
C
C...Clear out p
C
      call fill_array (p(1),1000,0.d0)
      if (isc10(2) .eq. 2) then
          call gtdesc(tv,nclkey,nwds,ietype)
          call gtshpt(p, nclkey)
          asn = p(1)
          goto 800
      endif
      nwds=ksc(39)
      if (nwds.lt.141) goto 9
C
C...error.  too much input
C
8     ifl(2)=59
      goto 99
C
C...Get input data from next ranfil rec(s) into w-tbl
C 
9     irec=ifl(4)
      iwl=-34
10    irec=irec+1
      iwl=iwl+35
      call getran(w(iwl),irec)
      if (iwl+34.lt.nwds) goto 10
C
C...Must be out-right at this time
C
      if (kw(1).eq.653.and.kw(2).eq.24) goto 15
C
C...Error.  out-right reqd.
C
      ifl(2)=5
      goto 99
C
C...Find last entity loc in w-list
C
15    i=nwds+1
      do 17 j=1,nwds
      i=i-1
      k=4*i
      if (kw(k).eq.5.and.kw(k-1).eq.6) goto 20
17    continue
C
C...If here, no first/last entity was found
C
18    ifl(2)=6
      goto 99
20    ilast=i
C
C...Get last entity and do limpl2
C
      asn=w(i)
      if (ksn(4).eq.5) goto 22

C
C...Error. limpl must be line
C 
21    ifl(2)=5
      goto 99

22    call gtentt(asn,trflg,nclkey,ietype,a)
      tl=dsqrt(a(4)**2+a(5)**2)
      if (tl.gt..0001) goto 24

C
C...Error.  impossible geometry case
C
23    ifl(2)=163
      goto 99

24    p(5)=a(5)/tl
      p(6)=-a(4)/tl
      p(7)=p(5)*a(1)+p(6)*a(2)
C
C...Get ent1, do limpl1 and seed b-tbl
C
      do 25 i=2,nwds
      k4=kw(4*i)
25    if(k4.eq.5.or.k4.eq.7) goto 26
      goto 18
26    if (k4.eq.7) goto 21
      ifirst=i
      asn=w(i)
      call gtentt(asn,trflg,nclkey,ietype,b)
c
      tl=dsqrt(b(4)**2+b(5)**2)
      if (tl.lt..0001) goto 23
      p(2)=b(5)/tl
      p(3)=-b(4)/tl
      p(4)=b(1)*p(2)+b(2)*p(3)
c****************    begin at w(2) and work thru list. add to p-tbl
c
c                    at this time items in loc4 must be:
c                         5     line
c                         7     circle
c                         8     curve or spline
c                         19    modifier
c                         2000  pp command string
C*******************************************************************
      ipb=0
      iwx=1
      ipx=7
      nent=0
30    iwx=iwx+1
      ipx=ipx+1
      oxnd=xnd
      oynd=ynd
      npts = 0
C
C...If error, exit now.
C
      if (ifl(2).gt.0) goto 80
      if (iwx.gt.nwds) goto 80
      if (ipx.gt.1000) goto 8
      asn = w(iwx)
      if (ksn4.ne.2000) goto 40
C
C...pp command found.  add to p-tbl b/u
C
      npp=ksn(3)+1
      do 32 i=1,npp
      p(ipx)=w(iwx)
      iwx=iwx+1
32    ipx=ipx+1
      iwx=iwx-1
      ipx=ipx-1
      goto 30

40    if (ksn4.ne.19) goto 50
C
C...Modifier found. add to p-list
C
      p(ipx)=w(iwx)
C
C...If clw or cclw, set kclw
C
      if (ksn(1).eq.60) kclw=-1
      if (ksn(1).eq.59) kclw=1
      goto 30
C
C...Must be geom entity
C
50    if (ksn4.ne.5 .and. ksn4.ne.7 .and. ksn4.ne.8) then 
        ifl(2)=12
        goto 80
      endif

      if (ipb.eq.0) ipb=ipx
C
C...Move b-info to a
C
      ipa=ipb
      kclwa=kclwb
      do 52 i=1,12
52    a(i)=b(i)
C
C...Get entity into b
C

C
C...b-entity is a curve - evolve and store as a polyline 
C
      if (ksn4 .ne. 8) goto 556
        call gtdesc (asn, nclkey, nwds1, ietype)

        dtol = sc(27)
c
c...Metric conversion for tolerance
c
        if (ifl(264).eq.1) dtol = dtol/25.4d0
        isf = 3
        call evstup (nclkey, isf)

        npts = ncevolve2d(isf, dtol, maxpts, pts)
c
c...Metric conversion for points on curve
c
        if (ifl(264).eq.1) then
          do 53 i=1,npts
          pts(i*2-1)= pts(i*2-1) *25.4d0
          pts(i*2)= pts(i*2) *25.4d0
53        continue
        endif

        if (npts.le.0) then
          ifl(2) = 163
        else if (npts.eq.1) then
          goto 30
        else if (npts.gt.maxpts) then
c
c..... test allocation error: if greater than allocated space
c
          ifl(2) = 156
        endif
        if (ifl(2) .gt. 0) goto 99

        b(1) = pts(1)
        b(2) = pts(2)
        call fill_array (b(3),9,0.d0)
        b(4) = pts(3) - pts(1)
        b(5) = pts(4) - pts(2)
        goto 56

55      asn = 0.
        ksn(3) = 2
        ksn(4) = 5
        do 555 i=1,npts-2      
          nent = nent + 1
          ipa = ipa + 3
          ipb = ipb + 3
          ipx = ipx + 3
          p(ipa)=asn
          j = 2*i + 1
          p(ipa+1)=pts(j)
          p(ipa+2)=pts(j+1)
555     continue

        b(1) = pts(j)
        b(2) = pts(j+1)
        b(4) = pts(j+2) - b(1)
        b(5) = pts(j+3) - b(2)
        b(12) = asn
        goto 30

cc      endif

  556 call gtentt(asn,trflg,nclkey,ietype,b)
C
C...If circle, set kclwb and unset kclw
C 
      if (ksn4.eq.7) then 
        kclwb=kclw
        kclw=0
      endif

56    nent=nent+1
      b(12)=asn
      ipb=ipx
      ipx=ipb+2
      if (ksn4.eq.7) ipx=ipb+6
C
C...First time here, go next entity
C
      if (nent.lt.2) goto 30
c
c..... when nent=1 the b-entity is the first limiting line
c
      if (ka(48).eq.7) goto 60
C
C...a-entity is line.   (turn off kclw)
C
      kclw=0
      xnd=a(1)+a(4)
      ynd=a(2)+a(5)
      call berr
      if (dabs(errb).lt..001) goto 58
      call bentst
      tl=dsqrt(a(4)**2+a(5)**2)
      co=a(4)/tl
      si=a(5)/tl
      derr=-si*(xnd-a(1))+co*(ynd-a(2))
      if (dabs(derr).le..001) goto 58
      call ilbent
      if (ifl(2).gt.0) goto 99
58    asn=0.
      ksn(4)=5
      ksn(3)=2
      p(ipa)=asn
      p(ipa+1)=xnd
      p(ipa+2)=ynd
      if (npts .gt. 2) goto 55
      goto 30
C
C...a-entity is circle.  chk for xyplan
C
60    sec=dsqrt(a(4)**2+a(5)**2)
      if (sec.lt..001) goto 62
C
C...Error.  tipped circle.
C
      ifl(2)=161
      goto 99
62    tl=dsqrt(a(8)**2+a(9)**2)
      if (dabs(tl-1.d0).gt..001) goto 66
      d1=a(11)-a(8)*a(1)-a(9)*a(2)
      d2sq=a(7)**2-d1**2
      d2=0.
      if (d2sq.gt.0.) d2=dsqrt(d2sq)
C
C...Calc a-circle ndpt
C
      fx=-a(6)*a(9)
      fy=a(6)*a(8)
      xnd=a(1)+a(8)*d1+fx*d2
      ynd=a(2)+a(9)*d1+fy*d2
      call berr
      if (dabs(errb).lt..001) goto 68
      call bentst
C
C...See if on a-circle
C
      dx=xnd-a(1)
      dy=ynd-a(2)
      derr=dsqrt(dx**2+dy**2)-a(7)
      if (dabs(derr).lt..001) goto 68
C
C...ndpt a-entity not on b-ent.  intersect for same.
C
66    call icbent(kclwa)
C
C...Add a-circle to p-list
C
68    asn=0.
      ksn(4)=7
      ksn(3)=6
C
C...Set ksn(2)=1 for cclw, =-1 for clw  (std math notation)
C...If circle has limpl, get clw from a(6)
C
      ksn(2)=a(6)*1.1
      if (a(8)**2+a(9)**2.gt..001) goto 70
C
C...No limpl. look from stpt to ndpt
C... (the following is valid only for arcs .lt. 180 degrees.)
C
      dx=oxnd-a(1)
      dy=oynd-a(2)
      fx=xnd-oxnd
      fy=ynd-oynd
      ksn(2)=1
      co=-dy*fx+dx*fy
      if (co.lt.0.) ksn(2)=-1
C
C...If clw or cclw given, it takes precedence.
C
70    if (kclwa.ne.0) ksn(2)=kclwa
      p(ipa)=asn
      p(ipa+1)=xnd
      p(ipa+2)=ynd
      p(ipa+3)=a(1)
      p(ipa+4)=a(2)
      p(ipa+5)=a(7)
      dx=oxnd-a(1)
      dy=oynd-a(2)
C
C...Avoid very small dy values
C
      if (dabs(dy).lt..0001) dy=0.
C
C...Note.  all angles are positive and measured in radians
C...if dx,dy=0, do not try datan step
C
      ang(1)=0.
      if (dx**2+dy**2.lt.1.d-6) goto 72
      if (dabs(dx).lt..0001) then
        if (dy .gt. 0.) then 
           ang(1) = HALF_PI
        else if (dy .lt. 0.) then 
           ang(1) = - HALF_PI
        endif
        goto 72
      endif
      ang(1)=datan2(dy,dx)
72    if (ang(1).lt.0.) ang(1)=ang(1)+TWO_PI
      dx=xnd-a(1)
      dy=ynd-a(2)
      if (dabs(dy).lt..0001) dy=0.
      ang(2)=0.
      if (dx**2+dy**2.lt.1.d-6) goto 74
      if (dabs(dx).lt..0001) then
        if (dy .gt. 0.) then 
           ang(2) = HALF_PI
        else if (dy .lt. 0.) then 
           ang(2) = - HALF_PI
        endif
        goto 74
      endif
      ang(2)=datan2(dy,dx)
74    if (ang(2).lt.0.) ang(2)=ang(2)+TWO_PI
      delb=ang(2)-ang(1)
      psig=ksn(2)
      if (psig*delb.gt.0.) goto 76
      if (psig.gt.0) ang(2)=ang(2)+TWO_PI
      if (psig.lt.0) ang(1)=ang(1)+TWO_PI
76    p(ipa+6)=pang
      if (npts .gt. 2) then
        ipa = ipa + 4
        ipb = ipb + 4
        goto 55
      endif
      goto 30
C
C...Normal exit area.  finish p(1) and sto via putent if no error.
C
80    if (ifl(2).gt.0) goto 809
      asn=w(1)
      ksn(3)=ipb-1
      p(1)=asn
800   np=ksn(3)
      call ptshpt (p,nclkey)
C
C... Added for reset/disply,sh. kathy
C... check if entity should be displayed and make attribute blanked if
C... it is not to be displayed
C
      if (ldspsh) then 
      else
          call blkgeo (nclkey,1)
      endif
      call ptdesc (nclkey, 18, tv)
      ktv(3)=np
      rest=tv
809   continue
99    return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine shapre2
C*       description   This subroutine handles the NSHAPE command
C*                     which makes a shape without asking for
C*                     the beginning or end lines, or asking for
C*                     clockwise or counterclockwise.
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
      subroutine shapre2

      include 'com8a.com'
      include 'const.com'

      common/wblok/w(144),a(12),b(12),xnd,ynd,errb
cc      common/pblok/p(400)
      real*8 p(1000)
      integer*2 ksc(500),kw(576),ksn(4),kp(1600),ka(48),kb(48),ktv(4)
      integer*4 nclkey
      integer*2 ietype
      logical trflg,firstl,lastl
      real*4 ang(2)
      real*8 whold(144),dx,dy
      real*8 c(12)
      equivalence (sc,ksc),(w,kw),(asn,ksn),(p,kp),(a,ka),(b,kb)
      equivalence (ksn4,ksn(4))
      equivalence (tv,ktv),(pang,ang)

      integer*2 maxpts,npts,i,j,ii,nn
      parameter (maxpts=100)
      real*8 pts(maxpts*2)
      integer*4 ncevolve2d

      trflg = .false.
      firstl = .true.
      lastl  = .true.
      kclw=0
      kclwa=0
      kclwb=0
C
C...Clear out p
C
      call fill_array (p(1),1000,0.d0)

      if (isc10(2) .eq. 2) then
          call gtdesc(tv,nclkey,nwds,ietype)
          call gtshpt(p, nclkey)
          asn = p(1)
          goto 800
      endif
      nwds=ksc(39)
      if (nwds.lt.141) goto 9
C
C...Error.  Too much input.
C
8     ifl(2)=59
      goto 99
C
C...Get input data from next ranfil rec(s) into w-tbl.
C
9     irec=ifl(4)
      iwl=-34
10    irec=irec+1
      iwl=iwl+35
      call getran(w(iwl),irec)
C
C...Want to rebuild w so that the beginning and ending lines are included in
C...the shape and not considered to be the limiting lines.
C...w will be two entries bigger, the additional entries are for the first
C...and last entities, they will be placed twice in the array.
C
      whold(1)=w(1)
      whold(2)=w(2)
      do 12 i=3,nwds+1
         whold(i)=w(i-1)
12    continue
      whold(nwds+2)=w(nwds)
      do 13 i=1,nwds+2
         w(i)=whold(i)
13    continue
      if (iwl+34 .lt. nwds) goto 10

C
C...Must be out-right at this time, if not, an error.
C
      if (kw(1).ne.653 .or. kw(2).ne.24) then
         ifl(2)=5
         goto 99
      endif

C
C...At this point w and kw have been loaded and it is time to start
C...building the shape.
C
115   ipb=0
      iwx=1
      ipx=7
      nent=0
130   iwx=iwx+1
      ipx=ipx+1
      oxnd=xnd
      oynd=ynd
      npts = 0
C
C...If there is an error, exit now.
C
      if (ifl(2).gt.0) goto 80
      if (iwx.gt.nwds+2) goto 80
      if (ipx.gt.1000) goto 8
      asn=w(iwx)
      if (ksn4.ne.2000) goto 140
C
C...pp command found.  add to p-tbl b/u
C
      npp=ksn(3)+1
      do 132 i=1,npp
      p(ipx)=w(iwx)
      iwx=iwx+1
132   ipx=ipx+1
      iwx=iwx-1
      goto 130
C
C...Must be geom entity
C
140   if (ksn4.ne.19) goto 150
C
C...Modifier found.
C
      p(ipx)=w(iwx)
      if (ksn(1).eq.60) kclw=-1
      if (ksn(1).eq.59) kclw=1
      goto 130

150   if (ksn4.ne.5 .and. ksn4.ne.7 .and. ksn4.ne.8) then
        ifl(2)=12
        goto 80
      endif

      if (ipb.eq.0) ipb=ipx
C
C...Move b-info to a
C
      ipa=ipb
      kclwa=kclwb
      do 152 i=1,12
152   a(i)=b(i)
C
C...Get entity into b
C

C
C...b-entity is a curve - evolve and store as a polyline 
C
      if (ksn4 .ne. 8) goto 556
        if (iwx .eq. nwds+2) goto 130
        if (iwx .eq. 3) then
          ipx = ipx + 1
          goto 130
        endif
        call gtdesc (asn, nclkey, nwds1, ietype)

        dtol = sc(27)
c
c...Metric conversion for tolerance
c
        if (ifl(264).eq.1) dtol = dtol/25.4d0
        isf = 3
        call evstup (nclkey, isf)

        npts = ncevolve2d(isf, dtol, maxpts, pts)
c
c...Metric conversion for points
c
        if (ifl(264).eq.1) then
          do 153 i=1,npts
          pts(i*2-1)= pts(i*2-1) *25.4d0
          pts(i*2)= pts(i*2) *25.4d0
153       continue
        endif
        if (npts.le.0) then
          ifl(2) = 163
        else if (npts.eq.1) then
          goto 130
        else if (npts.gt.maxpts) then
c
c..... test allocation error: if greater than allocated space
c
          ifl(2) = 156
        endif
        if (ifl(2) .gt. 0) goto 99

        if (iwx .eq. 2) goto 155
        b(1) = pts(1)
        b(2) = pts(2)
        call fill_array (b(3),9,0.d0)
        b(4) = pts(3) - pts(1)
        b(5) = pts(4) - pts(2)
        goto 156

155     asn = 0.
        ksn(3) = 2
        ksn(4) = 5
        
        nn = npts - 2
        if (iwx .eq. nwds+1 .or. nent .eq. 0) nn = npts - 1
        ii = 1
        if (iwx .eq. 2) then
          ii = 0
          ipa = ipa - 3
        endif

        do 555 i=ii, nn     
          nent = nent + 1
          ipa = ipa + 3
          ipb = ipb + 3
          ipx = ipx + 3
          p(ipa) = asn
          j = 2*i + 1
          p(ipa+1) = pts(j)
          p(ipa+2) = pts(j+1)
555     continue

        j = (npts-2)*2 + 1
        b(1) = pts(j)
        b(2) = pts(j+1)
        b(4) = pts(j+2) - b(1)
        b(5) = pts(j+3) - b(2)
        b(12) = asn
        goto 130

cc      endif

  556 call gtentt(asn,trflg,nclkey,ietype,b)
C
C...If circle, set kclwb and unset kclw
C
      if (ksn4.ne.7) then
        kclwb=kclw
        kclw=0
      endif
156   nent=nent+1
      b(12)=asn
      ipb=ipx
      ipx=ipb+2
      if (ksn4.eq.7) ipx=ipb+6
C
C...First time here, go next entity
C
      if (nent.lt.2) then
         if (ksn4.eq.7) firstl =.false.
         goto 130
      else if (nent.eq.(nwds+1)) then
         if (ksn(4).eq.7) lastl = .false.
      endif
      if (ka(48).ne.5) goto 160
C
C...a-entity is line.   (turn off kclw)
C
      kclw=0
C
C...Need to make sure that we are starting with the right end
C...points of the line, otherwise, the line will not be part
C...of the shape.
C
      if (nent.eq.2) then
         xnd=a(1)
         ynd=a(2)
      else
         xnd=a(1)+a(4)
         ynd=a(2)+a(5)
         if (ietype.eq.5) then
C
C...Need to make sure that the two lines are connected.
C
            if ((iwx.ne.(nwds+2)).and.(((abs(xnd-b(1)).gt..001).and.
     x         (abs(xnd-(b(1)+b(4))).gt..001)).and.
     x         (abs(ynd-b(2)).gt..001).and.
     x         (abs(ynd-(b(2)+b(5))).gt..001)).and. 
     x         (abs(a(1)-(b(1)+b(4))).gt..001).and.
     x         (abs(a(2)-(b(2)+b(5))).gt..001).and.
     x         (abs(a(1)-b(1)).gt..001).and.
     x         (abs(a(2)-b(2)).gt..001)) then
              ifl(2)=471
              goto 99
            endif
         endif
C
C...Need to make sure have the correct ending point.
C
         if ((iwx.eq.(nwds+2)).and.(abs(xnd-oxnd).le..0001).and.
     x       (abs(ynd-oynd).le.0001)) then
            xnd=a(1)
            ynd=a(2)
         endif
      endif
      call berr
      if (dabs(errb).lt..001) goto 158
      call bentst
      tl=dsqrt(a(4)**2+a(5)**2)
      co=a(4)/tl
      si=a(5)/tl
      derr=-si*(xnd-a(1))+co*(ynd-a(2))
      if (dabs(derr).le..001) goto 158
      call ilbent
      if (ifl(2).gt.0) goto 99
158   asn=0.
      ksn(4)=5
      ksn(3)=2
      p(ipa)=asn
      p(ipa+1)=xnd
      p(ipa+2)=ynd
C
C...Only do this check if it isn't the first point because
C...nothing has been placed in p(12) and p(13) yet, so
C...if the line first point is 0,0, this will be true,
C...and only the endpoint will be part of the shape. JLS 9/30/99
C
      if (nent.eq.3) then
         if ((abs((p(9))-(p(12))).le..0001).and.
     x       (abs((p(10))-(p(13))).le..0001)) then
            p(9)=a(1)+a(4)
            p(10)=a(2)+a(5)
         endif
      endif
 
      if (npts .gt. 2) goto 155
      goto 130
C
C...a-entity is circle.  chk for xyplan
C
160   sec=dsqrt(a(4)**2+a(5)**2)
      if (sec.lt..001) goto 162
C
C...The circle is tipped, set error.
C
      ifl(2)=161
      goto 99

162   tl=dsqrt(a(8)**2+a(9)**2)
      if (dabs(tl-1.d0).gt..001) goto 166
      d1=a(11)-a(8)*a(1)-a(9)*a(2)
      d2sq=a(7)**2-d1**2
      d2=0.
      if (d2sq.gt.0.) d2=dsqrt(d2sq)
C
C...Calc a-circle ndpt
C

      fx=a(6)*a(9)
      fy=-a(6)*a(8)
      xnd=a(1)+a(8)*d1+fx*d2
      ynd=a(2)+a(9)*d1+fy*d2
C
C...Need to make sure we have the correct endpoints for the
C...circle and that the direction is correct.
C
      if ((abs(xnd-oxnd).le..001).and.(abs(ynd-oynd).le..001)) then
        xnd=a(1)+a(8)*d1-fx*d2
        ynd=a(2)+a(9)*d1-fy*d2
        a(6)=-(a(6))
      endif
      call berr
      if (dabs(errb).lt..001) goto 168
      call bentst
C
C...See if on a-circle
C
      dx=xnd-a(1)
      dy=ynd-a(2)
      derr=dsqrt(dx**2+dy**2)-a(7)
      if (dabs(derr).lt..001) goto 168
C
C...ndpt a-entity not on b-ent.  intersect for same.
C
166    call icbent(kclwa)
C
C...Add a-circle to p-list
C...If nent is equal to 2 then we are starting with a circle, and
C...we need to find the starting point of the arc.
C
168   if (nent.eq.2) then
         asn=0
         ksn(4)=5
         ksn(3)=2
         p(8)=asn
         xnd=a(1)+a(8)*d1-fx*d2
         ynd=a(2)+a(9)*d1-fy*d2
C
C...It is necessary to do another check to make sure we have the correct
C...end points.
C
         asn=w(4)
         call gtentt(asn,trflg,nclkey,ietype,c)
         if ((abs(xnd-c(1)).le..001).or.
     x       (abs(xnd-c(1)-c(4)).le..001)) then
            xnd=a(1)+a(8)*d1+fx*d2
            ynd=a(2)+a(9)*d1+fy*d2
         endif
         p(9)=xnd
         p(10)=ynd
         goto 130
      endif
      if (iwx.ne.(nwds+2).and.(.not.((abs(xnd-b(1)).le..001).or.
     x    (abs(xnd-(b(1)+b(4))).le..001)))) then
          ifl(2)=471
          goto 99
      endif
      asn=0.
      ksn(4)=7
      ksn(3)=6
C
C...Set ksn(2)=1 for cclw, =-1 for clw  (std math notation)
C...If circle has limpl, get clw from a(6)
C
      ksn(2)=a(6) * (0.-1.1)
      if (a(8)**2+a(9)**2.gt..001) goto 170
C
C...Look from stpt to ndpt
C...(the following is valid only for arcs .lt. 180 degrees.)
C
      dx=oxnd-a(1)
      dy=oynd-a(2)
      fx=xnd-oxnd
      fy=ynd-oynd
      ksn(2)=1
      co=-dy*fx+dx*fy
      if (co.lt.0.) ksn(2)=-1
170   p(ipa)=asn
      p(ipa+1)=xnd
      p(ipa+2)=ynd
      p(ipa+3)=a(1)
      p(ipa+4)=a(2)
      p(ipa+5)=a(7)
      dx=oxnd-a(1)
      dy=oynd-a(2)
C
C...Avoid very small dy values
C
      if (dabs(dy).lt..0001) dy=0.
C
C...Note.  all angles are + per math notation
C...If dx,dy =0, do not try datan step
C
      ang(1)=0.
      if (dx**2+dy**2.lt.1.d-6) goto 172
      ang(1)=datan2(dy,dx)
172   if (ang(1).lt.0.) ang(1)=ang(1)+TWO_PI
      dx=xnd-a(1)
      dy=ynd-a(2)
      if (dabs(dy).lt..0001) dy=0.
      ang(2)=0.
      if (dx**2+dy**2.lt.1.d-6) goto 174
      ang(2)=datan2(dy,dx)
174   if (ang(2).lt.0.) ang(2)=ang(2)+TWO_PI
      delb=ang(2)-ang(1)
      psig=ksn(2)
      if (psig*delb.gt.0.) goto 176
      if (psig.gt.0) ang(2)=ang(2)+TWO_PI
      if (psig.lt.0) ang(1)=ang(1)+TWO_PI
176   p(ipa+6)=pang
      if (npts .gt. 2) then
        ipa = ipa + 4
        ipb = ipb + 4
        goto 155
      endif
      goto 130

80    if (ifl(2).gt.0) goto 809
      asn=w(1)
      ksn(3)=ipb-1
      p(1)=asn
800   np=ksn(3)
C
C...Determine limp1 and limp2  JLS 5/11/99
C...If the last entity in the shape was a line, the
C...limp2 is created by the line perpto 
C...the last line.
C
      if (lastl) then
         tl = dsqrt((a(4)**2) + (a(5)**2))
         p(5) = a(4)/tl
         p(6) = a(5)/tl
         p(7) = (p(np-1)*p(5)) + (p(np)*p(6))
      else
C
C...If the last entity was a circle, the limp2 is created
C...by the line from the center of the circle to the shape 
C...end point.
C
         dy =p(np-4) - p(np-2)
         dx = p(np-5) - p(np-3)
         tl =dsqrt((dy**2) + (dx**2))
         p(5) = dy/tl
         p(6) = -dx/tl
         p(7) = ((p(5)*p(np-5)) + (p(6)*p(np-4)))
      endif

C
C...Choose the appropriate location in the p array to determine
C...dx and dy, the intial point is the same for both, but if 
C...the first entity is a cirle, we need to look further along in
C...the p array to get the next point.
C
      if (firstl) then
         dx = p(10) - p(13)
         dy = -(p(9) - p(12))
      else
         dx = p(10) - p(17)
         dy = -(p(9) - p(16))
      endif
      tl =dsqrt((dy**2) + (dx**2))
      p(2) = dy/tl
      p(3) = -dx/tl
      p(4) = (((p(9)-dx)*p(2)) + ((p(10)-dy)*p(3)))

      call ptshpt (p,nclkey)

C
C...added for reset/disply,sh. kathy
C...check if entity should be displayed and make attribute blanked if
C...it is not to be displayed
C
       if (ldspsh) then
       else
           call blkgeo (nclkey,1)
       endif
      call ptdesc (nclkey, 18, tv)
      ktv(3)=np
      rest=tv

809   continue
99    return
      end

C*********************************************************************
C**
C**    SUBROUTINE:  setflg(status)
C**
C**    Sets the flag for 2 or 3 dimensional display of the shape.
C**
C**********************************************************************

      subroutine setflg(status)

      include 'com8a.com'

      integer*2 status

      ifl(344)=status

      return
      end


