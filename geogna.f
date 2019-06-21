C*********************************************************************
C*    NAME         :  geogna.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       geogna.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:05
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine geogna
C*       last revision: add new pt,vec, defs.   remove ln,pl to geognc   
C*                       and add new def types.    7/8/82                
C*       prepare and store point and vector canon lists.
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
      subroutine geogna

      include 'com8a.com'
      include 'wrksys.com'
      include 'suvcom.com'

      integer*4 ipnt
      integer*2 nw

      EQUIVALENCE(IFL(54),ISRF),(IFL(51),IA)
 
      integer*2 isc(100),ktv(4),ksn(4),i,evflg
      integer*2 in1,in2,prjvfl,nerptf,plnr,iflg,normfl
      real*8 w(12),a(35),u(12),t(12),zs(4),ptvect(6),ww(12)
      real*8 spacpt(6), nearpt(6), projvc(3), surfu, surfv
      equivalence (a,jb),(sc(10),isc),(tv,ktv)
      equivalence (asn,ksn)
      REAL*4 VX,VY,VZ

      integer*4 nclkey,nkey1,nkey2,nkey3,nkey4,srfkey
      integer*2 ietype,ier,nwds,it,lprcnt,sign
      logical trflg
      real*8 pt(3)
      real*8 tol
      real*4 mmorin
        
      integer*2 intat 
      character*64 cmname
      integer*4 cmsub,ilabsub
      character*64 fillnm 
      integer*2 parslb,iret

      logical nored,trans

      sign = -1
      trflg = .true.
      radian=57.2957795d0
      nored=.false.
      trans=.true.
      ktv(4)=idst
      isub=isc(2)
      if (ifl(264).eq.0) then
c                     units are inches
        mmorin = 1
      else
c                     units are millimeters
        mmorin = 25.4
      endif
c          branch to geo type per idst
      if(idst-4)100,200,999
c****************************************************** point
100   ktv(3)=3
      if (isub-2)110,110,129
110   do 122 i=1,3
122       w(i)=sc(i+10)
      if (isub.eq.2) w(3)=0.
c          if w(3)=987, turn on ifl(26).    off if -987
      if(w(3).eq.987.)ifl(26)=1
      if(w(3).eq.-987.)ifl(26)=0
      goto 900
129   if(isub-4)130,140,149
c     **********************   pt/te
130   w(1)=sc(1)
      w(2)=sc(2)
      w(3)=sc(3)
      goto 900
c
c     **********************   pt/intof,ln1,ln2
c                                       pv1,ln2
c                                       pv1,pv2
c                                       ln1,pv2
c              get ln1 in w(1), ln2 in w(7)
c
140   continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, w(7))
      den=w(4)*w(11)-w(5)*w(10)
      if(dabs(den).gt.1.d-7)goto 141
c          error.  parlel lines.
      ifl(2)=167
      goto 990
141   q1=w(2)*w(4)-w(1)*w(5)
      q2=w(7)*w(11)-w(8)*w(10)
c              xyz load directly in w(1,,)
      w(1)=(w(10)*q1+w(4)*q2)/den
      w(2)=(w(11)*q1+w(5)*q2)/den
      w(3)=0.
      goto 900
149   if(isub-6)150,160,169
c     **********************  pt/center,ci1                       -- 5 --
150   continue
      call gtdesc(sc(11),nclkey,nwds,ietype)
      if  (ietype .eq. 7) then
          call gtentt(sc(11), trflg, nclkey, ietype, w(1))
          goto 900
      endif
c     **********************  pt/center,cv                       -- 5 --
      if  (ietype .eq. 8) then
          call plcvsf (nclkey,1,w,plnr,iflg)
          if (iflg .eq. 1) then
              ifl(2) = 163
              goto 990
          endif
      else
c     **********************  pt/center,sf                       -- 5 --
          call plcvsf (nclkey,0,w,plnr,iflg)
          if (iflg .eq. 1) then
              ifl(2) = 163
              goto 990
          endif
      endif
      go to 900

c     **********************  pt/intof,pl1,pl2,pl3                -- 6 --
c              load 3 pls in w(1),w(5),w(9)
160   continue
      call gtplt(sc(11), ifl(72), w(1))
      call gtplt(sc(12), ifl(72), w(5))
      call gtplt(sc(13), ifl(72), w(9))
c              proj origpt on pl1,2 intersection
      si=w(1)*w(5)+w(2)*w(6)+w(3)*w(7)
      den=1.-si**2
      if(dabs(den).gt.1.d-7)goto 164
c          error. degen case
162   ifl(2)=163
      goto 990
164   d1=(w(4)-w(8)*si)/den
      d2=w(8)-d1*si
      x12=d1*w(1)+d2*w(5)
      y12=d1*w(2)+d2*w(6)
      z12=d1*w(3)+d2*w(7)
c              pl1,2 intersection vector
      vx=w(2)*w(7)-w(3)*w(6)
      vy=w(3)*w(5)-w(1)*w(7)
      vz=w(1)*w(6)-w(2)*w(5)
      den=w(9)*vx+w(10)*vy+w(11)*vz
      if(dabs(den).lt.1.d-7)goto 162
c              effective length p12 to int pt
      tl=(w(12)-w(9)*x12-w(10)*y12-w(11)*z12)/den
c              now object pt
      w(1)=x12+vx*tl
      w(2)=y12+vy*tl
      w(3)=z12+vz*tl
      goto 900
169   if(isub-8)170,180,1891

c*************************** point/intof,pl1,cv1,pt1              --(7)--
c          ( this pt def handled in geognd )  10-8-82
c          ( control never reaches here )
170   goto 990
c
c**************************  point/xyls,intof,ln1,ci1             -- 8 --
c
180   continue
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
      call gtentt(sc(13), trflg, nclkey, ietype, t(1))
c               check for tipped circle        12-sep-84
      if(t(4)**2+t(5)**2.gt.1.d-6)goto 1902
181   sec=dsqrt(u(4)**2+u(5)**2)
      if(sec.gt.1.d-6)goto 182
c          error.  line points in z-direction
      ifl(2)=44
      goto 990
182   dis=( u(5)*(t(1)-u(1))+u(4)*(u(2)-t(2)))/sec
c          dis max = radius
      if(dabs(dis).le.t(7)+1.d-4)goto 184
c          error. ln1 does not intersect ci1
183   ifl(2)=163
      goto 990
184   b=0.
      bsq=t(7)**2-dis**2
      if(bsq.gt.0.)b=dsqrt(bsq)
c              account for xl,xs,yl,ys modifier
      if(sc(11).ne.638.)goto 185
      if(u(4))188,162,189
185   if(sc(11).ne.641.)goto 186
      if(u(4))189,162,188
186   if(sc(11).ne.639.)goto 187
      if(u(5))188,162,189
c              modifier must have been 'ys'
187   if(u(5))189,162,188
188   b=-b
189   w(1)=t(1)+(u(4)*b-u(5)*dis)/sec
      w(2)=t(2)+(u(5)*b+u(4)*dis)/sec
      w(3)=0.
      goto 900
c              bra to pt/9,11,12       (pt/10 is geognd)
1891  if(isub.gt.12)goto 193
      if(isub-11)190,191,192
c
c*****************************  point/xyls,intof,ci1,ci2           -- 9 --
c              conv ci1 to 'ln1' and go use pt/8 logic above.
190   continue
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
      call gtentt(sc(13), trflg, nclkey, ietype, t(1))
c              check for tipped circles.
      if(u(4)**2+u(5)**2.gt.1.d-6)goto 1902
      if(t(4)**2+t(5)**2.le.1.d-6)goto 1904
c              error.  tipped circles ng here.
1902  ifl(2)=161
      goto 990
1904  dx=t(1)-u(1)
      dy=t(2)-u(2)
      chdsq=dx**2+dy**2
      rho=(u(7)**2+chdsq-t(7)**2)/2./chdsq
      u(1)=u(1)+rho*dx
      u(2)=u(2)+rho*dy
      u(4)=-dy
      u(5)=dx
      goto 181
c*****************************  pt/ci1,atangl,ang               -- 11 --
191   continue
      call gtentt(sc(11), trflg, nclkey, ietype, u(1))
      if(u(4)**2+u(5)**2.gt.1.d-6)goto 1902
      ang=sc(12)/radian
      si=dsin(ang)
      co=dcos(ang)
      w(1)=u(1)+co*u(7)
      w(2)=u(2)+si*u(7)
c                 zpt=zci  ????
      w(3)=u(3)
      goto 900
c*****************************  pt/pt1,ci1,angl                 -- 12 --
192   continue
      call gtentt(sc(11), trflg, nclkey, ietype, u(1))
      call gtentt(sc(12), trflg, nclkey, ietype, t(1))
      if(t(4)**2+t(5)**2.gt.1.d-6)goto 1902
      ang=sc(13)/radian
      si=dsin(ang)
      co=dcos(ang)
      dx=u(1)-t(1)
      dy=u(2)-t(2)
      sec=dsqrt(dx**2+dy**2)
c                pt1 must be off ci1 center
      if(sec.lt..001)goto 183
      dx=dx/sec
      dy=dy/sec
      du=dx*co-dy*si
      dv=dx*si+dy*co
      w(1)=t(1)+du*t(7)
      w(2)=t(2)+dv*t(7)
      w(3)=t(3)
      goto 900
193   if (isub.gt.13) goto 194
c*********************************  pt/pt1,xval,yval,zval          -- 13 --
         call gtentt(sc(11), trflg, nclkey, ietype, w(1))
         if (isc(3).eq.0) goto 900
         w(1)=w(1)+sc(12)
         if (isc(3).gt.1) w(2)=w(2)+sc(13)
         if (isc(3).gt.2) w(3)=w(3)+sc(14)
         go to 900
194   if(isub.gt.14) goto 195
c**********************************  pt/xyzls,endpt,<ln, pv, ci, cv> -- 14 --

      asn = sc(12)
      call gtdesc (asn, nclkey, nwds, ietype)
c                            get ln or cir into w-tbl
      if (ietype.ne.8) then
          call gtentt(sc(12), trflg, nclkey, ietype, w(1))
      else
c
c......Find endpoints of curve and compare appropriate coordinates
c
          u = 0
          evflg = -1
          call ncvevl1(nclkey,evflg,u,w(1),w(4))
          u = 1
          call ncvevl1(nclkey,evflg,u,ww(1),ww(4))
          if ((sc(11).eq.638.and.w(1).lt.ww(1)).or.
     x        (sc(11).eq.639.and.w(2).lt.ww(2)).or.
     x        (sc(11).eq.640.and.w(3).lt.ww(3)).or.
     x        (sc(11).eq.641.and.w(1).gt.ww(1)).or.
     x        (sc(11).eq.642.and.w(2).gt.ww(2)).or.
     x        (sc(11).eq.643.and.w(3).gt.ww(3))) then
              do 201 i=1,12
                  w(i) = ww(i)
201           continue
          endif
c
c.....Changed storage of point so point is not moved again by
c.....transformation matrix - ASF 12/12/13.
c
          call ptnomd(POINT, w, nclkey, tv)
          goto 999
c          goto 900
      endif
c                           set direc vec for xyzls
      dx=0.
      dy=0.
      dz=0.
      if(sc(11).eq.638.) dx=1.
      if(sc(11).eq.639.) dy=1.
      if(sc(11).eq.640.) dz=1.
      if(sc(11).eq.641.) dx=-1.
      if(sc(11).eq.642.) dy=-1.
      if(sc(11).eq.643.) dz=-1.
      sec=dx**2+dy**2+dz**2
      if(dabs(1.-sec).lt..0001) goto 1942
c                     error with modifier
      ifl(2)=292
      goto 990
c
c...vp 3.18.93 added pv as line
c
1942  if(ksn(4).eq.5 .or. ksn(4) .eq. 21) goto 1946
c                     entity is circle.  conv to line.
c                      check for ok pl in 8,9,10
      if(dabs(w(8))+dabs(w(9))+dabs(w(10)).lt..01)goto 1947
      fx=w(5)*w(10)-w(6)*w(9)
      fy=w(6)*w(8)-w(4)*w(10)
      fz=w(4)*w(9)-w(5)*w(8)
      dis=w(11)-w(8)*w(1)-w(9)*w(2)-w(10)*w(3)
      hsq=w(7)**2-dis**2
      h=0.
      if(hsq.gt.0.)h=dsqrt(hsq)
c                       conv circ to line
      w(1)=w(1)+w(8)*dis-h*fx
      w(2)=w(2)+w(9)*dis-h*fy
      w(3)=w(3)+w(10)*dis-h*fz
      w(4)=2.*h*fx
      w(5)=2.*h*fy
      w(6)=2.*h*fz
c                       drop thru to line case
c
c                     choose bet endpts of line


      if (debug) then
          write (cout, 9010) dx, dy, dz, w(4), w(5), w(6)
9010      format (' geogna: dx,y,z=',3f5.2,' w(4,5,6)=',3f8.3)
          call putmsg (cout, 80, 16, 0)
      endif



1946  sec=dx*w(4)+dy*w(5)+dz*w(6)
      if(dabs(sec).gt.1.d-4) goto 1948
c                      error.  could not choose bet endpts
1947  ifl(2)=293
      goto 990
1948  if(sec.lt.0.)goto 900
1949  w(1)=w(1)+w(4)
      w(2)=w(2)+w(5)
      w(3)=w(3)+w(6)
      goto 900
195   if (isub.gt.15) goto 196
c*********************************  pt/io,ln1,pl1                 -- 15 --
      call gtentt(sc(11), trflg, nclkey, ietype, t(1))
      call gtplt(sc(12), ifl(72), u(1))
      den=u(1)*t(4)+u(2)*t(5)+u(3)*t(6)
      if(abs(den).gt.1.d-4)goto 1952
      ifl(2)=301
      goto 990
1952  d1=(u(4)-t(1)*u(1)-t(2)*u(2)-t(3)*u(3))/den
      w(1)=t(1)+t(4)*d1
      w(2)=t(2)+t(5)*d1
      w(3)=t(3)+t(6)*d1
      goto 900
196   if (isub.gt.17) goto 197
c*********************************  pt/pn1,n                      -- 17 -- 
c    changed the routine name from gtpnpt to gpnptt on: 3/4/88 by: kathy
      call gtpnnp (sc(11),nw,ipnt)
      if (isc10(3) .lt. 1 .or. isc10(3) .gt. nw) then
         ifl(2) = 340
         goto 990
      endif
      call gpnptt(w,sc(11),isc10(3),trans)
      goto 900
c
c...pt/endpt,pv1                                                  -- 18 --
c
197   if (isub .eq. 18) then
          call gtentt(sc(11), trflg, nclkey, ietype, w(1))
          goto 1949
      endif
c
c...pt/intof,cv1,cv2,pt                                           -- 19 --
c
      if (isub .eq. 19) then
          call gtdesc (sc(11),nkey1,nwds,ietype)
          call gtdesc (sc(12),nkey2,nwds,ietype)
c
c..... here isc(3)=1 means we have have no near point, and 
c..... isc(4)=1 means we have intof3 rather than intof
c
          if (isc(3).eq.0) then
            call gtdesc (sc(13),nclkey,nwds,ietype)
            call gtgeo  (nclkey,pt)
          endif
             
          cmname = SAVID2
          cmsub = isvsub
             
          if (isc10(3) .eq. 2) then
            if (ifl(296) .eq. 1) then
               intat = 1
            else
               intat = 0
            endif   
            ilabsub = 0
            ifl(296) = 0
            if (intat .eq. 1) then
              defwf = .true.
              ilabsub   = isvsub
              fillnm = cmname(1:64)
              if (ilabsub .lt. 1) then
                  iret = parslb(fillnm,ilabsub)
                  ifl(296) = iret
              endif
            endif
          else
            if (ifl(296) .eq. 1) then
              defwf = .true.
              fillnm = cmname(1:64)
              ilabsub = isvsub
            else
              ilabsub = 0
            endif
          endif
      
          call ncl_pt_intof_cv (nkey1,nkey2,isc(3),isc(4),pt,
     1              fillnm,ilabsub,ier)
          if (ier .ne. 0) goto 9471
          
          if (intat.eq. 0) then
             SAVID2 = cmname
             isvsub = cmsub
          endif
          defwf  = .true.
          goto 999
      endif
c
c...pt/on,cv1,u                                               -- 20 --
c
      if (isub .eq. 20) then
          it = 0
          lprcnt = isc10(3)
          call gtdesc (sc(11),nkey1,nwds,ietype)
          call ncl_pt_on_cv (it,nkey1,lprcnt,sc(12),w(1),ier)
          if (ier .ne. 0) goto 9466
          call ptnomd(POINT, w, nclkey, tv)
          goto 999
      endif
c
c...pt/on,sf1,u,v                                             -- 21 --
c
      if (isub .eq. 21) then
          it = 0
          lprcnt = isc10(3)
          call gtdesc (sc(11),nkey1,nwds,ietype)
          call ncl_pt_on_sf (it,nkey1,lprcnt,sc(12),sc(13),w(1),ier)
          if (ier .ne. 0) goto 9466
          call ptnomd(POINT, w, nclkey, tv)
          goto 999
      endif

c
c...  PT/PROJCT,PT1(,VE1),SF1(,PT2)                           -- 22 --
c...  Projecting point onto surface.
c...  Ed Ames 13 Jul 00
c
      if (isub .eq. 22) then
           call fill_array(spacpt(1), 3, 0.0)
           call fill_array(projvc(1), 3, 0.0)
           call fill_array(nearpt(1), 3, 0.0)
           call fill_array(ptvect(1), 6, 0.0)
           prjvfl = 0
           nerptf = 0
           call gtdesc (sc(11),nkey1,nwds,ietype)
           call gtgeo  (nkey1,spacpt)
           call gtdesc (sc(12),nkey2,nwds,ietype)
c
c...  Using the optional projection vector, set the projvec flag to 1.
c
           if ((ietype .eq. VECTOR).or.(ietype .eq. PNTVEC)) then
                call gtgeo  (nkey2,ptvect)
                if (ietype .eq. VECTOR) then
                   call vctovc(ptvect, projvc)
                else
                   call vctovc(ptvect(4), projvc)
                endif
                prjvfl = 1
c
                call gtdesc (sc(13),nkey3,nwds,ietype)
                call gtdesc (sc(16),nkey4,nwds,ietype)
                srfkey = nkey3
                surfu = sc(14)
                surfv = sc(15)
c
c...  If there is an optional near point, set the nearpt flag to 1 
c...  sc(14) = u and sc(15) = v , the parametric surface coordinates
c
                if ((ietype .eq. POINT).or.(ietype .eq. PNTVEC)) then
                     call gtgeo  (nkey4,nearpt)
                     nerptf = 1
                endif
           else
c
c...  No projection vector. The nkey2 variable is from the surface.
c
                srfkey = nkey2
                surfu = sc(13)
                surfv = sc(14)
                call gtdesc (sc(15),nkey3,nwds,ietype)
c
c...  If there is an optional near point, set the nearpt flag to 1 
c...  sc(13) = u and sc(14) = v , the parametric surface coordinates
c
                if ((ietype .eq. POINT).or.(ietype .eq. PNTVEC)) then
                     call gtgeo  (nkey3,nearpt)
                     nerptf = 1
                endif
           endif
c
c...  Do the actual projection
c
           normfl = 0
           call nclf_pt_project_sf(spacpt, srfkey, surfu, surfv,
     1              prjvfl, projvc, nerptf, nearpt, normfl, w, ier)
           if (ier .ne. 0) then
             if (ier .gt. 0) ifl(2) = ier
             goto 990
           endif
           call ptnomd(POINT,w,nkey1,tv)
           goto 999
c
c...  End of PT/PROJCT,PT1(,VE1),SF1[U,V](,PT2)
c
      endif

      goto 990


c******************************************************  vector
200   ktv(3)=3
      in1   = isc(3)
      in2   = isc(4)
c          isub=1 thru 7
c        ( sub types 1,2 use point logic )
c      goto(100,100,230,240,250,260,270,280,290,2910,2915,2920),isub
c
      if (isub .eq. 1 .or. isub .eq. 2) goto 100
      if (isub .eq. 3) goto 230
      if (isub .eq. 4) goto 240
      if (isub .eq. 5) goto 250
      if (isub .eq. 6) goto 260
      if (isub .eq. 7) goto 270
      if (isub .eq. 8) goto 280
      if (isub .eq. 9) goto 290
      if (isub .eq. 10) goto 2910
      if (isub .eq. 11) goto 2915
      if (isub .eq. 12) goto 2920
      if (isub .eq. 13) goto 300
      if (isub .eq. 14) goto 110
      if (isub .eq. 15) goto 110
      if (isub .eq. 16) goto 400
      goto 990
c
c     **********************   ve/fwd                             -- 3 --
230   w(1)=sc(7)
      w(2)=sc(8)
      w(3)=sc(9)
      goto 900
c     ************************ ve/ta                              -- 4 --
240   w(1)=sc(4)
      w(2)=sc(5)
      w(3)=sc(6)
      goto 900
c    ************************* ve/pt1,pt2                         -- 5 --
250   continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(12), trflg, nclkey, ietype, w(4))
      w(1)=w(4)-w(1)
      w(2)=w(5)-w(2)
      w(3)=w(6)-w(3)
      goto 900
c     ************************ ve/pe,pl1,posxyz                   -- 6 --
260   continue
      call gtplt(sc(11), ifl(72), w(1))
c          get pl1 and verify posxyz. if neg, flip vec
      k=sc(12)-653.
262   if(k.lt.4)goto 266
c          negxyz modifier. if that term pos, flip it
c           ( zero value should probably cause error )
      k=k-3
      if(w(k))900,9213,268
c          posxyz
266   if(w(k))268,9213,900
268   w(1)=-w(1)
      w(2)=-w(2)
      w(3)=-w(3)
      goto 900
c     ************************ ve/unit, ve1 or a,b,c              -- 7 --
270   secsq=sc(11)**2+sc(12)**2+sc(13)**2
      if(secsq.gt.1.d-6)goto 272
c          error. vec quanta too small
271   ifl(2)=121
      goto 990
272   sec=dsqrt(secsq)
      w(1)=sc(11)/sec
      w(2)=sc(12)/sec
      w(3)=sc(13)/sec
      goto 900
c     ************************ ve/ve1,cross,ve2                   -- 8 --
280   continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(4))
      call gtentt(sc(12), trflg, nclkey, ietype, u(1))
c
c          note: if ve1 parlel ve2, a 0,0,0 vector will result.  ok?
c
      w(1)=w(5+in1)*u(3+in2)-w(6+in1)*u(2+in2)
      w(2)=w(6+in1)*u(1+in2)-w(4+in1)*u(3+in2)
      w(3)=w(4+in1)*u(2+in2)-w(5+in1)*u(1+in2)
      goto 900
c     *********************** ve/parlel,intof,pl1,pl2,pnxyz       -- 9 --
c          fetch planes and calc intersection vec.
c          unitize and go case 6 for pos/neg xyz.     10-1-82
290   continue
      call gtplt(sc(11), ifl(72), u(1))
      call gtplt(sc(12), ifl(72), t(1))
      w(1)=u(2)*t(3)-u(3)*t(2)
      w(2)=u(3)*t(1)-u(1)*t(3)
      w(3)=u(1)*t(2)-u(2)*t(1)
      secsq=w(1)**2+w(2)**2+w(3)**2
c          vector may not be all zeros.
      if(secsq.lt.1.d-6)goto 271
c          unitize vector
      sec=dsqrt(secsq)
      w(1)=w(1)/sec
      w(2)=w(2)/sec
      w(3)=w(3)/sec
c          pickup direc indicator and go case 6 for flip check.
      k=sc(13)-653.
      goto 262

c     ***********************  ve/ve1, +/- ,ve2                  -- 10 --
2910  continue
      call gtentt(sc(11), trflg, nclkey, ietype, w(1))
      call gtentt(sc(13), trflg, nclkey, ietype, u(1))
      sig=1.
      if(sc(12).eq.19.)goto 2912
      sig=-1.
      if(sc(12).ne.10.)goto 990
2912  w(1)=w(1+in1)+u(1+in2)*sig
      w(2)=w(2+in1)+u(2+in2)*sig
      w(3)=w(3+in1)+u(3+in2)*sig
      goto 900

c     ****************************    ve/pt1,sf1               -- 11 --
2915  continue
      call sfptdo (w)
      goto 999

c          ***********************  ve/ve1,times,d             -- 12 --
2920  call gtentt(sc(11),trflg,nclkey,ietype,w(1))
      w(1)=w(1+in1)*sc(12)
      w(2)=w(2+in1)*sc(12)
      w(3)=w(3+in1)*sc(12)
      goto 900
c
C     **********************   VE/TT,PT1,CV1                   -- 13 --
c
300   call vettcv (w(4),w,ier) 
      IF (ier .gt. 0) goto 990
      nwds   = 3
      ietype = 4
c
c...call ptgeom directly to avoid refsys transformation
c
      CALL ptgeom (ietype,w,nclkey,nwds)
      call ptdesc (nclkey,ietype,tv)
      if (.not.dsplve) call blkgeo (nclkey,1)
      KTV(3) = nwds
      REST   = TV
      GOTO 999
c
C     **********************   VE/ATANGL,alf,ln1,POSX           -- 16 --
c
400   alf=sc(11)
      call gettol (tol)
      call gtentt(sc(12), trflg, nclkey, ietype, w(1))
     
      if (w(4).eq.0)then
        if (w(5).eq.0) then
          ifl(2)=31
          go to 990
        endif
        alf=alf+90
        go to 482
      endif      
c
c...calculate angle from x-axis based on arctan of (y-coor/x-coor)
c
      alf=alf+datan2(w(5),w(4))*radian
482   continue
      
      alf=alf/radian
      w(1)=dcos(alf) * mmorin 
      w(2)=dsin(alf) * mmorin
      w(3)=0

      if (((sc(13).eq.638.or.sc(13).eq.654).and.w(1).lt.0).or.
     1    ((sc(13).eq.639.or.sc(13).eq.655).and.w(2).lt.0).or.
     2    ((sc(13).eq.641.or.sc(13).eq.657).and.w(1).gt.0).or.
     3    ((sc(13).eq.642.or.sc(13).eq.658).and.w(2).gt.0)) then
        w(1) = w(1) * sign
        w(2) = w(2) * sign
      endif    
  
900   if (ifl(55).eq.1.and.idst.eq.3.and.(isub.eq.2.or.
     1     isub.eq.4.or.isub.eq.5.or.isub.eq.8.or.isub.eq.9
     2     .or.isub.eq.11.or.isub.eq.12.or.
     3     (isub.eq.13.and.isc(3).ne.3))) then
c
c               if zsurf on, load zs-tbl from sc(37,)
c
          do 910 i=1,4
910       zs(i)=sc(i+36)
c
c..... if refsys, convert zsurf pl
c
          if(ifl(72).eq.1) call transf(zs,sc(68),4,6)
c
c..... zsurf on. check for valid pl
c
          if (abs(zs(3)).lt.1.e-6) then
c
c..... error. zsurf pl ng.
c
              ifl(2)=276
              goto 990
          endif
          w(3)=(zs(4)-zs(1)*w(1)-zs(2)*w(2))/zs(3)
      endif
c
ccc              add check for zero-length vector
c
       if (idst.ne.4) goto 930
       if (w(1)**2+w(2)**2+w(3)**2.gt.1.d-6) goto 930
c
c..... error, vector length is zero
c
       ifl(2)=317
       goto 990
930    continue
c
c..... geosto this item
c
      ietype = ktv(4)
c
cuni     call ptgeom(ietype, w, nclkey)
cuni     call ptdesc(nclkey, ietype, tv)
c
c..... update wdknt and put ipg/iel in tv for vstore
c
      call ptentt(ietype, w, nclkey, tv)
      rest  = tv
      goto 999
c
c...   Error - illogical direction modifier used
c
9213  ifl(2) = 213
      goto 990
c
c...   Error - Surface evaluator failed
c
9466  ifl(2) = 466
      goto 990
c
c...   Error - Entities are not connected
c
9471  ifl(2) = 471
      goto 990
c
c..... error exit. set err and zero tv & 'rest'
c
990   err = .true.
      if (ifl(2) .eq. 0) ifl(2) = 5
      tv = 0.
      rest = 0.

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptnomd (ietype, w, nclkey, ranwd)
C*       Put entity into unibase with no modification thru refsys, modsys
C*       or units.
C*    PARAMETERS
C*       INPUT  :
C*          ietype     - Type of entity.
C*          w          - Entity data.
C*       OUTPUT :
C*          nclkey     - Key of stored entity.
C*          ranwd      - Associated word of stored entity.
C*    RETURNS      : none
C*    SIDE EFFECTS : Sets common variable rest to ranwd.
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine ptnomd (ietype, w, nclkey, ranwd)

      include 'com.com'

      integer*2 ietype
      integer*4 nclkey
      real*8 w(12), ranwd

      integer*2 ifl72, ifl264
      logical lwrksv

      ifl72    = ifl(72)
      ifl264   = ifl(264)
      lwrksv   = lwrk
      ifl(72)  = 0
      ifl(264) = 0
      lwrk     = .false.

      call ptentt(ietype, w, nclkey, ranwd)

      ifl(72)  = ifl72 
      ifl(264) = ifl264 
      lwrk     = lwrksv

      rest = tv
999   return
      end
