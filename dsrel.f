C*********************************************************************
C*    NAME         :  dsrel.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dsrel.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:00
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dsrel
C*          this routine relates the tool to ds and puts the
C*          resultant p/n in s( ,2).  also puts u,v in tcol(ia).
C*
C*          CHG:  IF BARREL CUTTER, DO ALL IN SUBR DSBARL.   26-JAN-88
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
      subroutine dsrel

      include 'com.com'
      include 'mocom.com'
      include 'drvcom.com'
      include 'shrpcv.com'

      common /psldcom/ irejog,ro,idsjog
      integer*4 irejog,idsjog
      real*4 ro

      integer*2 isdx,jsdx,ksdx

      integer*2 jd(600)
      real*4 ad(300),asc(100)
      real*4 sbe,cbe,cutn
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      integer*2 itfl,iptk,ia,ib,ic,isrf
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      real*8 dx,dy,dz,xt,zt,x1,z1,ti,tj,tk,fx,fy,fz,dst,cute,crad,dtol
      real*8 tx,ty,tz,d1,d2,cr,thk,rdst,fdst,dstb,hcrad
      real*4 rx,ry,rz,sec,h,h1,h2,tn,t1,px,py,pz,vx,vy,vz,dis,rbot,rtop
      real*4 hdia,u,v,xc,zc,zcn,tbe,den,lukpt(3),sal,absal,cal,ted,tcon
      real*4 ssb(7),tonp
      integer*2 i,itim,itlrgt,isftyp,isf0,isft0,isfb,isftb
      equivalence(ksd(329),isftyp)
      integer*4 ikey0,ikey1,ikeyb
      logical lmod,lgck,lfixfw,lv95,lv98

      lv95 = sc(169).lt.9.549d0
      lv98 = sc(169).lt.9.849d0
C               IF BARREL TOOL, CALL DSBARL AND EXIT         26-JAN-88
      IF(IFL(282).EQ.0)GOTO 9
      CALL DSBARL
      GOTO 99
9     CONTINUE
C
C...   Set lmod logical if using guide curve or modify now & not tlon
C...   and not zero cutter.
C
      dtol = sc(27)
      cute = tool(6)
      crad = tool(2)
      hcrad = crad*(1.0-sbe)
      lmod = (lmdnow.or.lcvgid).and.ifl(21).ne.0.and.cute.gt.dtol
      itlrgt = ifl(21)
      lgck = (idsgck.ge.2 .and. itlrgt.ne.0 .and. cute.ge.2.*dtol
     *                          .and. (.not.lmod .or. ifl(57).ne.0))
      isdx=82*(isrf-1)
      jsdx=2*isdx
      ksdx=4*isdx
c          init  h, itim  and cutdia/2
      h=t(21,ia)
      itim=0
      hdia = 0.5 * tool(1)
      if(cbe.eq.0.)cbe=1.
      tbe=-sbe/cbe
c          get ta from t-tbl
      ti=t(4,ia)
      tj=t(5,ia)
      tk=t(6,ia)
c          always try rcorn contact before side,top cases.
      if(ifl(57).gt.0) ifl(57)=1
c-----
c     if ta/normal,ps  use same for ijk      14-dec-82
      if(ifl(23).ne.1)goto 10
      ti=s(1,1)
      tj=s(2,1)
      tk=s(3,1)
c-----
c          ept is hds along ta from te.
10    s(8,2)=t(1,ia)+t(4,ia)*h
      s(9,2)=t(2,ia)+t(5,ia)*h
      s(10,2)=t(3,ia)+t(6,ia)*h
      i=jd(201)
c          if ds is sf or rldsrf, go surfpn route.
      if(i.eq.9.or.i.eq.21.or.i.eq.25.or.i.eq.26.or.i.eq.27)goto 50
c          bra on ln or cv/ci
20    if(i.ne.5)goto 30
c
c          line. use line pt-deltas for p-v
      px=d(53)
      py=d(54)
      pz=d(55)
      vx=d(56)
      vy=d(57)
      vz=d(58)
      goto 40
c          cv or ci    ( ept 1st-try is ready in s-tbl)
30    if(i.eq.8)goto 35
      if(i.eq.7)goto 32
c          error. invalid ds type.
      ifl(2)=28
      goto 99
32    call circpv(px,py,pz,vx,vy,vz)
      goto 40
35    call curvpv(px,py,pz,vx,vy,vz)
C RLS - 03/27
      if (ifl(2) .eq. 466) go to 99
C RLS - END
c          dsn is crossf v & ta
40    rx=vy*tk-vz*tj
      ry=vz*ti-vx*tk
      rz=vx*tj-vy*ti
      sec=sqrt(rx*rx+ry*ry+rz*rz)
c          if sec zero, some special case exists.   ta//v?
      if(sec.eq.0.) goto 9129
c          if v-vec opposes old fwd, flip this rgt sense
      if(vx*t(7,ic)+vy*t(8,ic)+vz*t(9,ic).lt.0.)sec=-sec
      rx = rx/sec
      ry = ry/sec
      rz = rz/sec

      lfixfw = .false.
      if (ishrp.eq.1) then
        if (zercor.eq.0) then
          lfixfw = (t(19,ic)-ushrp)*(t(19,ia)-ushrp).lt.0
        else if (zercor.eq.1) then
          lfixfw = (t(19,ic).gt.0.8 .and. t(19,ia) .lt. 0.2)
        else if (zercor.eq.-1) then
          lfixfw = (t(19,ia).gt.0.8 .and. t(19,ic) .lt. 0.2)
        endif
c
c..... if turned around a sharp corner, make sure the new forward
c..... will be along the curve tangency
c
        if (lfixfw) then
          vvx = s(2,1)*rz - s(3,1)*ry
          vvy = s(3,1)*rx - s(1,1)*rz
          vvz = s(1,1)*ry - s(2,1)*rx
          if (fdshrp*(vx*vvx + vy*vvy + vz*vvz) .lt. 0.) then
            rx = -rx
            ry = -ry
            rz = -rz
          endif
        endif
      endif

      if (itlrgt.eq.0) goto 42
      if (sbe.eq.0.0 .and. .not.lcntct) goto 42
      tx = t(1,ia)
      ty = t(2,ia)
      tz = t(3,ia)
      h = t(21,ia)
c
c...   Unitize line vector
c
      if (jd(201).eq.LINE) then
        dst = sqrt(vx*vx+vy*vy+vz*vz)
        if (dst.eq.0.0) goto 9129
        vx = vx/dst
        vy = vy/dst
        vz = vz/dst
      else
c
c...   Adjust height
c
        h1 = (px-tx)*ti+(py-ty)*tj+(pz-tz)*tk
        if (abs(h-h1).gt.dtol .and. itim.lt.16) then
          t(21,ia) = h1
          itim = itim+1
          goto 20
        endif
      endif
c
c...   Project curve point to extension of curve or to correct place on line.
c
      d1 = (tx+ti*h-px)*vx+(ty+tj*h-py)*vy+(tz+tk*h-pz)*vz
      px = px+vx*d1
      py = py+vy*d1
      pz = pz+vz*d1
c
c...   handle angled tool with no contact
c
      if (.not.lcntct) then
        d1 = ((px-tx)*ti+(py-ty)*tj+(pz-tz)*tk-sc(30))*sbe/cbe
        if (sbe.lt.0) d1 = d1+(tool(1)-sc(28))/2.0d0
        goto 41
      endif
c
c...   Adjust te & cr for thick
c
      thk = sc(24)
      tx = tx-ti*thk
      ty = ty-tj*thk
      tz = tz-tk*thk
      cr = crad+thk
      if (cr.lt.0.0d0) cr = 0.0d0
c
c...   Calculate point on tool axis at height where cr ends
c
      h = cr*(1.0-sbe)
      dx = tx+h*ti
      dy = ty+h*tj
      dz = tz+h*tk
c
c...   calc dist of curve contact point from max width of cutter &
c...   move contact point by that amount. This will create plane parallel
c...   to ta at correct distance. (Signs are all reversed because final result
c...   is multiplied by tool right factor)
c
      d1 = sc(28)/2.0
      if (d1.lt.hdia) d1 = hdia
      d1 = cute-thk-d1
c
c...   If side contact, add in dist above cr * tan beta.
c
      dst = (px-dx)*ti+(py-dy)*tj+(pz-dz)*tk
      if (dst.gt.0.0d0) d1 = d1+dst*sbe/cbe
c
c...   If negative thick comes inside cr, reduce flat.
c...   (Uses tan(a/2)=(1-cos(a))/sin(a), cos(a)=sin(90-a) ).
c
      d2 = crad+thk
      if (d2.lt.0.0d0) d1 = d1+d2*(1-sbe)/cbe
c
c...   If corner radius contact or above, add in correction
c
      d2 = (px-tx)*ti+(py-ty)*tj+(pz-tz)*tk
      if (d2.gt.0.0d0) then
        if (d2.gt.h) d2 = h
        d2 = cr**2-(cr-d2)**2
        if (d2.gt.0.0d0) d1 = d1+dsqrt(d2)
      endif
41    d1 = d1*itlrgt
      px = px+d1*rx
      py = py+d1*ry
      pz = pz+d1*rz
42    continue
c          load dspl in s-tbl.
      s(1,2)=rx
      s(2,2)=ry
      s(3,2)=rz
      s(4,2)=px*s(1,2)+py*s(2,2)+pz*s(3,2)
      goto 99
c          ds is surf or rldsrf
50    itim=itim+1
      u=t(19,ia)
      v=t(20,ia)
      call surfpn(u,v,1)
c
c..... some extra gouge checking: try forward lookpoint on the tool surface and
c..... the one of the left (or right), choose the surface projection closest
c..... to the original lookpoint on the tool axis, project again with the 
c..... original lookpoint and the best projection U,V
c
      if (lgck) then
      if (lv95) then
        if (isftyp.eq.27) then
c
c..... if net DS: save the common block data
c
          isdx=82*(isrf-1)
          jsdx=2*isdx
          ksdx=4*isdx
          isf0=ksd(ksdx+3)
          if (isf0 .gt. 0) then
            isft0=ksd(ksdx+4)
            ikey0=jsd(jsdx+isf0+4)
          endif
c
c..... set the flag telling ssrfpn to save the best surface in the common block
c..... even if projected on extension
c
          ksd(ksdx+100) = 1
        endif
        lukpt(1) = s(8,2)
        lukpt(2) = s(9,2)
        lukpt(3) = s(10,2)
        if (h .ge. tool(2)) then
          d1 =  hdia
        else if (h .le. 0.) then
          d1 = tool(6)
        else
          d1 = tool(6) + sqrt(h*(2.*tool(2)-h))
        endif
c
c..... calculate the forward and the side lookpoints
c
        fx = tj*s(3,2)-tk*s(2,2)
        fy = tk*s(1,2)-ti*s(3,2)
        fz = ti*s(2,2)-tj*s(1,2)
        sec = sqrt(fx*fx+fy*fy+fz*fz)
        if (sec .lt. sc(27)) goto 51
        if (fx*t(7,ia) + fy*t(8,ia) + fz*t(9,ia) .lt. 0.) sec = -sec
        fx = fx/sec
        fy = fy/sec
        fz = fz/sec

        rx = tj*fz - tk*fy
        ry = tk*fx - ti*fz
        rz = ti*fy - tj*fx
        sec = sqrt(rx*rx+ry*ry+rz*rz)
        if (sec .lt. sc(27)) goto 51
        dx=s(8,2)-s(5,2)
        dy=s(9,2)-s(6,2)
        dz=s(10,2)-s(7,2)
        if (rx*dx + ry*dy + rz*dz .gt. 0.) sec = -sec
        rx = rx/sec
        ry = ry/sec
        rz = rz/sec

        s(8,2) = lukpt(1) + d1*fx
        s(9,2) = lukpt(2) + d1*fy
        s(10,2) = lukpt(3) + d1*fz
        u=t(19,ia)
        v=t(20,ia)
        call surfpn(u,v,1)
        fdst=abs(s(1,2)*lukpt(1)+s(2,2)*lukpt(2)+s(3,2)*lukpt(3)-s(4,2))
        fu = u
        fv = v

        s(8,2) = lukpt(1) + d1*rx
        s(9,2) = lukpt(2) + d1*ry
        s(10,2) = lukpt(3) + d1*rz
        u=t(19,ia)
        v=t(20,ia)
        if (isftyp.eq.27) then
c
c.... if net DS: save the best surface isf, key, and U,V; reset the originals
c
          isf1=ksd(ksdx+3)
          isft1=ksd(ksdx+4)
          ikey1=jsd(jsdx+isf1+4)
          ksd(ksdx+3)=isf0
          if (isf0 .gt. 0) then
            ksd(ksdx+4)=isft0
            jsd(jsdx+isf0+4)=ikey0
          endif
        endif
        call surfpn(u,v,1)
        rdst=abs(s(1,2)*lukpt(1)+s(2,2)*lukpt(2)+s(3,2)*lukpt(3)-s(4,2))
c
c..... choose the best projection U,V
c
        if (fdst .le. rdst - sc(27)) then
          u = fu
          v = fv
c
c..... for net DS: fix the common block data so that the chosen U,V will be
c..... used
c
          if (isftyp.eq.27) then
            ksd(ksdx+3)=isf1
            ksd(ksdx+4)=isft1
            jsd(jsdx+isf1+4)=ikey1
          endif
        endif

        s(8,2) = lukpt(1)
        s(9,2) = lukpt(2)
        s(10,2) = lukpt(3)
c
c..... for net DS: unset the flag for ssrfpn
c
        if (isftyp.eq.27) ksd(ksdx+100) = 0
        call surfpn(u,v,1)
c
c..... end of old (lv95) logic
c
      else if (h.le.tool(3)) then

        call dsdis (ktypb,dstb,dtol)

        if (isftyp.eq.27) then
c
c..... if net DS: save the common block data
c
          isf0 = ksd(ksdx+3)
          if (isf0 .gt. 0) then
            isft0 = ksd(ksdx+4)
            ikey0 = jsd(jsdx+isf0+4)
          endif

          isfb = isf0
          isftb = isft0
          ikeyb = ikey0
c
c..... set the flag telling ssrfpn to save the best surface in the common block
c..... even if projected on extension
c
          ksd(ksdx+100) = 1
        endif
        lukpt(1) = s(8,2)
        lukpt(2) = s(9,2)
        lukpt(3) = s(10,2)
        ub = u
        vb = v
        call conv4_4 (s(1,2),ssb,7)

        if (h .ge. hcrad) then
          d1 =  hdia + (tool(3)-h)*tbe
        else if (h .le. 0.) then
          d1 = cute
        else
          d1 = cute + sqrt(h*(2.*crad-h))
        endif
c
c..... calculate the forward and the side lookpoints
c
        fx = tj*s(3,2)-tk*s(2,2)
        fy = tk*s(1,2)-ti*s(3,2)
        fz = ti*s(2,2)-tj*s(1,2)
        sec = sqrt(fx*fx+fy*fy+fz*fz)
        if (sec .lt. dtol) goto 51
        if (fx*t(7,ic) + fy*t(8,ic) + fz*t(9,ic) .lt. 0.) sec = -sec
        fx = fx/sec
        fy = fy/sec
        fz = fz/sec

        rx = tj*fz - tk*fy
        ry = tk*fx - ti*fz
        rz = ti*fy - tj*fx
        sec = sqrt(rx*rx+ry*ry+rz*rz)
        if (sec .lt. dtol) goto 51
        dx=s(8,2)-s(5,2)
        dy=s(9,2)-s(6,2)
        dz=s(10,2)-s(7,2)
        if (rx*dx + ry*dy + rz*dz .gt. 0.) sec = -sec
        rx = rx/sec
        ry = ry/sec
        rz = rz/sec
        
        s(8,2) = lukpt(1) + d1*fx
        s(9,2) = lukpt(2) + d1*fy
        s(10,2) = lukpt(3) + d1*fz
        u = t(19,ia)
        v = t(20,ia)
        call surfpn(u,v,1)
        call dsdis (ktypf,fdst,dtol)
        if (ktypf .lt. 1) ktypf = 1

        call dselct (ssb,ktypb,ub,vb,dstb,isfb,isftb,ikeyb,
     x                   ktypf,u,v,fdst,dtol)
 
        s(8,2) = lukpt(1) + d1*rx
        s(9,2) = lukpt(2) + d1*ry
        s(10,2) = lukpt(3) + d1*rz
        u = t(19,ia)
        v = t(20,ia)
        if (isftyp.eq.27) then
c
c.... if net DS, reset the original best surface isf and key
c
          ksd(ksdx+3)=isf0
          if (isf0 .gt. 0) then
            ksd(ksdx+4)=isft0
            jsd(jsdx+isf0+4)=ikey0
          endif
        endif

        call surfpn(u,v,1)
        call dsdis (ktypr,rdst,dtol) 

        call dselct (ssb,ktypb,ub,vb,dstb,isfb,isftb,ikeyb,
     x                   ktypr,u,v,rdst,dtol)
c
c..... choose the best projection U,V
c
        u = ub
        v = vb
c
c..... for net DS: fix the common block data so that the chosen U,V will be
c..... used
c
        if (isftyp.eq.27) then
            ksd(ksdx+3)=isfb
            if (isfb .gt. 0) then
            ksd(ksdx+4)=isftb
            jsd(jsdx+isfb+4)=ikeyb
c..... unset the flag for ssrfpn
            ksd(ksdx+100) = 0          
          endif
        endif

        call conv4_4 (ssb,s(1,2),7)
        
        s(8,2) = lukpt(1)
        s(9,2) = lukpt(2)
        s(10,2) = lukpt(3)

        if (idsgck .lt. 4) then
          if (isftyp.eq.27) ksd(ksdx+100) = 0
          call surfpn(u,v,1)
        endif

      endif
      endif

C RLS - 03/27
51    if (ifl(2) .eq. 466) go to 99
C RLS - END
      t(19,ia)=u
      t(20,ia)=v
c          calc hds if tllft or tlrgt
      if(ifl(57).gt.0)goto 52
      if (.not.lmod.or.itim.gt.1) goto 99
52    continue
c          if o/b case, calc deltas to sfpnt per tanpl eq
C We should always calculate deltas because u & v can be between 0 & 1 
C on extension of trimmed sf being driven as trimmed    - IJD 28-mar-97
      if(ifl(332).eq.0.and.u.gt.0..and.u.lt.1..and.v.gt.0..and.v.lt.1.)
     x  goto 53
      dis=s(1,2)*s(8,2)+s(2,2)*s(9,2)+s(3,2)*s(10,2)-s(4,2)
      dx=s(8,2)-s(1,2)*dis-t(1,ia)
      dy=s(9,2)-s(2,2)*dis-t(2,ia)
      dz=s(10,2)-s(3,2)*dis-t(3,ia)
      goto 54
c          deltas to sfpnt from te
53    dx=s(5,2)-t(1,ia)
      dy=s(6,2)-t(2,ia)
      dz=s(7,2)-t(3,ia)
54    continue
c
c...   If using guide curve or MODIFY,NOW, redo from center of corner radius
c
      if (lmod .and. ifl(57).eq.0) then
        fx = tj*dz-tk*dy
        fy = tk*dx-ti*dz
        fz = ti*dy-tj*dx
        dx = fy*tk-fz*tj
        dy = fz*ti-fx*tk
        dz = fx*tj-fy*ti
        dst = dsqrt(dx*dx+dy*dy+dz*dz)
        if (dst.lt.dtol) goto 99
        dst = cute/dst
        s(8,2) = s(8,2)+dx*dst
        s(9,2) = s(9,2)+dy*dst
        s(10,2) = s(10,2)+dz*dst
        goto 50
      endif
c          sfpnt in toolsys
      zt=ti*dx+tj*dy+tk*dz
c                                check for neg xt   29-may-85
      xt=dx**2+dy**2+dz**2-zt**2
      if(xt.lt.-1.d-5)goto 98
      if(xt.lt.0.)xt=0.
      xt=dsqrt(xt)
      tn=0.
      if(xt.gt.0.)tn=(zt-h)/xt
c          bra on itim
      if(itim-2)55,60,59
c              ****  time 1
55    x1=xt
      z1=zt
      h1=h
      t1=tn
c          choose between rcorn and top indicated contact
      if(t1.gt.tbe) goto 56
c             rcorn case
      h=crad-t1*cute
      goto 58
c             top case
56    h=tool(3)-t1*tool(1)*.5
      ifl(57)=3
c          if h-chg small and ds seems convex, exit
58    if(abs(h-h1).lt..01.and.ifl(27).eq.0)goto 90
c
c...If first time here and on surface extension
c...calculate actual distance to drive surface
c...so that we do not get a mismatch error due to
c...the cs logic using different calculations
c...FSR 61061
c...Bobby  -  05/25/11
c
      if (itfl .eq. -2 .and. (u .ge. 1. .or. v .ge. 1) .and.
     1    .not. lv98) then
          call dsdis (ityp,dstb,dtol)
          if (dabs(dstb) .lt. dtol) idsjog = 1
      endif
c
      if(abs(h-h1).lt..01)h=h1+.01
      goto 10
c              ****  try time 3 if real change in h
59    continue
      if (sc(169).lt.8.20799) goto 90
      if (itim.gt.3) goto 90
      dis = h-h2
C   ------   This change breaks DS regression test MFGMMP05   ------
C   ------   but fixes bug report NCL28           IJD 28-mar-97
C      if (sc(169).ge.8.4999) dis = abs(dis)
      if (dis .le. .01) goto 90
c              ****  time 2
60    h2=h
c          calc apparent r ctr
      den=t1-tn
c          if no real angle chg, assume flat case.
      if(abs(den).lt.1.e-5)goto 90
      xc=(h2-h1)/den
      zc=h1+t1*xc
c          convex or concave
      if(xc.lt.xt) goto 75
c             convex. see if rcorn case or other.
      ifl(27)=0
      zcn=sbe*(xc-cute)+cbe*(zc-crad)
      if(zcn.gt.0.)goto 64
c             rcorn
      den=xc-cute
      if(den.eq.0.)goto 98
      tn=(crad-zc)/den
      h=crad+tn*cute
      ifl(57)=1
      goto 80
c             side or top
64    zcn=sbe*(xc-hdia)+cbe*(zc-tool(3))
      if(zcn.ge.0.)goto 68
c             side case
      h=zc-tbe*xc
      goto 80
c             top case
68    den=xc-hdia
      if(den.eq.0.)goto 98
      tn=(zc-tool(3))/den
      h=tool(3)-tn*hdia
      goto 80
c             concave. decide if rcorn or top
75    rtop=sqrt((hdia-xc)**2+(tool(3)-zc)**2)
      rbot=sqrt((cute-xc)**2+(zc-crad)**2)+crad
      ifl(27)=1
c             if disk tool and tn .ge. tbe,  go top
      if(asc(66).eq.1..and.tn.ge.tbe)goto 76
      if(rbot.ge.rtop)goto 77
c             top case
76    den=hdia-xc
      if(den.eq.0.)goto 98
      tn=(tool(3)-zc)/den
      h=zc-tn*xc
      ifl(57)=3
      goto 80
c             bot case
77    dx=cute-xc
      if(dx.eq.0.)goto 98
      tn=(zc-crad)/dx
      h=zc+tn*xc
      ifl(57)=1
c          if h-chg real, go again
80    if(abs(h-h2).gt..01)goto 10
c
c          record this h-val in t(21)
90    t(21,ia)=h
      goto 99
c                      Error 129 - tool to drive sf apply failure
9129  ifl(2) = 129
      goto 99
c          error exit due to zero divide.        15-jun-84
98    ifl(2)=163
99    return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine dsrel0
C*          this routine relates the tool to ds and puts the
C*          resultant p/n in s( ,2).  also puts u,v in tcol(ia).
C*
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
      subroutine dsrel0

      include 'com.com'
      include 'mocom.com'
      include 'drvcom.com'

      integer*2 jd(600)
      real*4 ad(300),asc(100)
      real*4 sbe,cbe,cutn
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      integer*2 itfl,iptk,ia,ib,ic,isrf
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      real*8 ti,tj,tk
      real*4 rx,ry,rz,sec,h,px,py,pz,vx,vy,vz
      integer*2 i

c
c..... do as if ta/normal,ps
c
      h = t(21,ia)

      ti = s(1,1)
      tj = s(2,1)
      tk = s(3,1)

      t(4,ia) = ti
      t(5,ia) = tj
      t(6,ia) = tk
c
c          ept is hds along ta from te.
c
      s(8,2)  = t(1,ia) + t(4,ia)*h
      s(9,2)  = t(2,ia) + t(5,ia)*h
      s(10,2) = t(3,ia) + t(6,ia)*h

      i = jd(201)
c          bra on ln or cv/ci
20    if (i .ne. LINE) goto 30
c
c          line. use line pt-deltas for p-v
      px=d(53)
      py=d(54)
      pz=d(55)
      vx=d(56)
      vy=d(57)
      vz=d(58)
      goto 40
c          cv or ci    ( ept 1st-try is ready in s-tbl)
30    if(i .eq. CURVE) goto 35
      if (i .eq. CIRCLE) goto 32
c          error. invalid ds type.
      ifl(2) = 28
      goto 99
32    call circpv(px,py,pz,vx,vy,vz)
      goto 40
35    call curvpv(px,py,pz,vx,vy,vz)
      if (ifl(2) .eq. 466) goto 99
c          dsn is crossf v & ta
40    rx=vy*tk-vz*tj
      ry=vz*ti-vx*tk
      rz=vx*tj-vy*ti
      sec=sqrt(rx*rx+ry*ry+rz*rz)
c          if sec zero, some special case exists.   ta//v?
      if(sec.eq.0.) goto 9129
c          if v-vec opposes old fwd, flip this rgt sense
      if(vx*t(7,ic)+vy*t(8,ic)+vz*t(9,ic).lt.0.)sec=-sec
      rx = rx/sec
      ry = ry/sec
      rz = rz/sec

c          load dspl in s-tbl.
      s(1,2)=rx
      s(2,2)=ry
      s(3,2)=rz
      s(4,2)=px*s(1,2)+py*s(2,2)+pz*s(3,2)
      goto 99
c                      Error 129 - tool to drive sf apply failure
9129  ifl(2) = 129
      goto 99
c          error exit due to zero divide.        15-jun-84
99    return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine dsdis (dst)
C*          Calculates the cutter distance from the DS plane.
C*          Taken from mover.
C*
C*    PARAMETERS   
C*       INPUT  : none
C*       OUTPUT : dst
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine dsdis (ktyp,dst,tol)

      include 'com.com'
      include 'mocom.com'
      include 'drvcom.com'

      integer*2 ktyp
      real*8 dst,tol

      integer*2 jd(600)
      real*4 ad(300),asc(100)
      real*4 sbe,cbe,cutn
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      integer*2 itfl,iptk,ia,ib,ic,isrf
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)

      real*4 trgt,dcon,dco,dsi,q,sec
      real*4 x1,y1,cute,crad,hdia,hgt,dot1,eps
      real*4 ti,tj,tk
      real*4 fv(3),rv(3),svec(3),ta(3)
      real*4 f_dot4,f_mag4

      real*4 SI5,CO5
      parameter (SI5 = 0.0871557)
      parameter (CO5 = 0.9961947)

      integer*2 ALONG,EXTEN,BOTTM,IGNOR,ERRSF
      parameter (ALONG = 0)
      parameter (EXTEN = 1)
      parameter (BOTTM = 2)
      parameter (IGNOR = 3)
      parameter (ERRSF = 4)

      if (ifl(2) .gt. 0) then
        ktyp = ERRSF
        return
      endif

      hdia = tool(1)/2.
      crad = tool(2)
      cute = tool(6)
      hgt = tool(3)

      trgt = ifl(21)
      eps = 5.*tol

      ktyp = IGNOR

      if (trgt.ne.1 .and. trgt.ne.-1) goto 10

      if (lcsavd) then
        dot1 = f_dot4(s(1,2),s(1,3))
        if (abs(dot1) .gt. CO5) then
          goto 10
        endif

      endif

        if (ifl(23).eq.1) then
          ti=s(1,1)
          tj=s(2,1)
          tk=s(3,1)
        else
          ti=t(4,ia)
          tj=t(5,ia)
          tk=t(6,ia)
        endif
        ta(1) = ti
        ta(2) = tj
        ta(3) = tk
c
c..... type IGNOR if the fwd or the right sense is undefined.
c
      call f_cross4(s(1,1),s(1,2),fv)
      sec = f_mag4(fv)
      if (sec .le. 1.e-3) goto 10
      call vctmsc4(fv,fv,1./sec)

      call f_cross4(ta,fv,rv)
      sec = f_mag4(rv)
      if (sec .le. 1.e-3) goto 10
      sec = trgt*sec

      call vctmsc4(rv,rv,1./sec)

         call vcmnvc4(s(5,2),t(1,ia),svec)
c
c..... if behind the type is IGNOR
c
         dot1 = f_dot4(rv,svec)
         if (dot1 .le. tol) goto 10

         y1 = f_dot4(ta,svec)
         x1 = sqrt (f_dot4(svec,svec) - y1*y1)
c
c..... x1,y1 are spt coordinates in the plane through the tool axis and spt.
c
         if (y1 .lt. -eps .or. 
     x      (y1 .lt. -tol .and. x1 .lt. cute - tol)) then
           ktyp = BOTTM
         else
           ktyp = EXTEN
           call unitvc4(svec,svec)
c
c..... the projection is labelled 'along' if the surface point is almost
c..... (within 5 degrees) on the plane perpendicular to the forward direction
c
           dot1 = f_dot4(fv,svec)
           if (abs(dot1) .lt. SI5) then
             ktyp = ALONG
           endif
         endif


10    continue
      dsi = (ta(1)*s(1,2)+ta(2)*s(2,2)+ta(3)*s(3,2))*trgt
      dco = 1.-dsi**2
      if (dco .gt. 0) then
        dco = sqrt (dco)
      else
        dsi = 1.
	  dco = 0.
      endif

c          BRANCH ON TOP OR CORNER CONTACT
      IF (dsi.lt.sbe-.001 .OR. IFL(57).GT.2) then
c           top contact
        dcon = s(4,2)+trgt*(sc(24)+dco*hdia-dsi*hgt)
      else
c           CORNER CONTACT
        dcon = s(4,2)+trgt*(cute*dco-crad*dsi+crad+sc(24))
      endif

      q  = f_dot4 (s(1,2),t(1,ia)) - dcon

      dst = trgt*q

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine dselct(ss0,u0,v0,dst0,isf0,isft0,
C*                                             ikey0,u1,v1,dst1,dtol)
C*          Replace the saved best DS projection if the current one
C*          is better.
C*********************************************************************
C
      subroutine dselct(ss0,ktyp0,u0,v0,dst0,isf0,isft0,ikey0,
     x                      ktyp1,u1,v1,dst1,dtol)

      include 'com.com'
      include 'mocom.com'
      include 'drvcom.com'

      real*8 dst0,dst1,dtol
      real*4 ss0(7),u0,v0,u1,v1
      integer*4 ikey0
      integer*2 isf0,isft0,ktyp0,ktyp1

      integer*2 isrf
      equivalence (ifl(54),isrf)
      integer*2 isftyp
      equivalence (ksd(329),isftyp)

      integer*2 isdx,jsdx,ksdx

      if (ktyp1 .lt. ktyp0 .or.
     x    (ktyp1.eq.ktyp0 .and. dst1.lt.dst0-dtol)) then


        ktyp0 = ktyp1
        u0 = u1
        v0 = v1
        dst0 = dst1

        if (isftyp .eq. 27) then

          isdx=82*(isrf-1)
          jsdx=2*isdx
          ksdx=4*isdx

          isf0 = ksd(ksdx+3)
          if (isf0 .gt. 0) then
            isft0 = ksd(ksdx+4)
            ikey0 = jsd(jsdx+isf0+4)
          endif
        endif

        call conv4_4 (s(1,2),ss0,7)

      endif

      return
      end
