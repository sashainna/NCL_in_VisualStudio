C*********************************************************************
C*    NAME         :  psrela.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       psrela.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:29
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine psrela
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
      subroutine psrela

      include 'com4a.com'
      include 'mocom.com'

      common/lfl6/lfl6
      integer*2 lfl6

      integer*2 jd(600)
      real*4 ad(300),asc(100)
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe)
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      real*8 tb(9)
      real*4 ss(10),tol,uu,vv,ffx,ffy,ffz,fx0,fy0,fz0,fxx,fyy,fzz,daa
      logical lv90,lv93
      real*8 damax,smal
      parameter (smal = 0.173648)
      integer*2 ihadit,lgoug,lf128

      lv90 = sc(169).lt.9.049d0
      lv93 = sc(169).lt.9.349d0
      tol = sc(27)
      lgoug = 0
      ihadit = 0
c
c.....always try lev1 first
c
      if(ifl(56).gt.1) ifl(56)=1

      lf128 = 0
      itim=0
      ltim=0
      jtim=0
      ktim=0
      fx=0.
      fy=0.
      fz=0.
      ffx=0.
      ffy=0.
      ffz=0.
      dx=0.
      dy=0.
      dz=0.
      hdia=tool(1)/2.
      rtan=1.
      daa = 0.
      oerr = 0.

      u=t(13,ia)
      v=t(14,ia)
      uu = 0.
      vv = 0.

      sec=sqrt(t(4,ia)**2+t(5,ia)**2+t(6,ia)**2)
      ti=t(4,ia)/sec
      tj=t(5,ia)/sec
      tk=t(6,ia)/sec
c
c..... aak 26-mar-1998: if tlonps, look point = tend
c
      if (ifl(342).ne.1 .or. lv90) then
         xc = t(1,ia)+ti*tool(2)
         yc = t(2,ia)+tj*tool(2)
         zc = t(3,ia)+tk*tool(2)
      else
         xc = t(1,ia)
         yc = t(2,ia)
         zc = t(3,ia)
      endif

      if(ifl(56).eq.0) goto 22
c          if ta/atangl,alf,ps (cutmode=2), tfwd=fwd proj on cpl
      if(ifl(23).ne.2.and.ifl(23).ne.10.and.ifl(23).ne.11.and.
     x   ifl(23).ne.12)goto 15
      xr=t(8,ia)*tk-t(9,ia)*tj
      yr=t(9,ia)*ti-t(7,ia)*tk
      zr=t(7,ia)*tj-t(8,ia)*ti
c
c.....if ta/tanto,ds (cutmod=3), do all in psmod3
c
      fx=zr*tj-yr*tk
      fy=xr*tk-zr*ti
      fz=yr*ti-xr*tj
c
c...   If angle to part surface is negative, contact is at back of tool.
c
      if (tool(8).ge.0.0) goto 18
      if (sc(169).lt.8.2999) goto 18
         fx = -fx
         fy = -fy
         fz = -fz
      goto 18
c
c.....ifl(6) is gougck flag        8-nov-84
c
15    if(ifl(6).ne.0) then
         ltim = 0
         fx = 0.
         fy = 0.
         fz = 0.
      else
c
c.....gougck off.  fwd from memory
c
         fx=t(16,ia)
         fy=t(17,ia)
         fz=t(18,ia)
         secsq=fx**2+fy**2+fz**2
         if (lfl6.ge.2 .and. secsq .gt. 1.e-6) lgoug = lfl6
      endif
c                proj fxyz on pl perpto ijk if nec        8-nov-84
18    hdis=fx*ti+fy*tj+fz*tk
      if(abs(hdis).lt..001)goto 19
      fx=fx-hdis*ti
      fy=fy-hdis*tj
      fz=fz-hdis*tk
c                unitize fxyz
19    secsq=fx**2+fy**2+fz**2
      if (secsq .lt. 1.e-6) goto 20
      sec=sqrt(secsq)
      fx=fx/sec
      fy=fy/sec
      fz=fz/sec
20    dx=fx*tool(6)
      dy=fy*tool(6)
      dz=fz*tool(6)
c             if disk tool + edge contact, do new ept
22    continue
      if(asc(66).eq.0.)goto 23
      cal=ti*s(1,1)+tj*s(2,1)+tk*s(3,1)
      if(cal.ge.sbe)goto 23
c                edge contact.  calc new ept at cutter edge
      s(8,1)=fx*hdia+ti*tool(3)+t(1,ia)
      s(9,1)=fy*hdia+tj*tool(3)+t(2,ia)
      s(10,1)=fz*hdia+tk*tool(3)+t(3,ia)
      goto 232
23    s(8,1)=xc+dx
      s(9,1)=yc+dy
      s(10,1)=zc+dz
232   itim=itim+1
      if(itim.gt.120)goto 98
      call surfpn(u,v,0)
C RLS - 03/27
      if (ifl(2) .eq. 466) go to 999
C RLS - END

c-c-c        skip next if gougck off, 2nd tim here, or not square case
      if(ifl(6).eq.0.or.ltim.gt.0)goto 234
      ltim=1
      if(abs(s(1,1)*ti+s(2,1)*tj+s(3,1)*tk).lt..99999) goto 234
c             square case.   use tool fwd and go again  (temp guess)
      itim=0
      fx=t(7,ia)
      fy=t(8,ia)
      fz=t(9,ia)
      goto 18
234   continue
c-c-c
       if (ifl(2).eq.128 .and. sc(169).ge.9.449d0) then
         if (jtim.lt.10 .and. ifl(56).eq.1 .and. ihadit.gt.0 .and.
     x         lf128.lt.9) then
           fx = fxx
           fy = fyy
           fz = fzz
           lf128 = lf128 + 1
           ifl(2) = 0
           itim = itim - 1
           da = (1.-0.1*lf128)*daa
           goto 42
         else
           goto 999
         endif
       else
         lf128 = 0
       endif

      t(13,ia)=u
      t(14,ia)=v
c
c...   For blade cutter, check for rear contact
c
      if (lblade) then
        if (tool(13).gt.-tol) goto 99
        if (itim.eq.1) then
          do 236 i=1,9
          ss(i) = s(i,1)
236       tb(i) = t(i,ia)
          d1 = ss(1)*tb(1)+ss(2)*tb(2)+ss(3)*tb(3)-ss(4)
          tool(13) = tool(13)*2.0
          tool(14) = tool(14)*2.0
          call modfy (tb ,tb)
          tool(13) = tool(13)/2.0
          tool(14) = tool(14)/2.0
          s(8,1) = tb(1)
          s(9,1) = tb(2)
          s(10,1) = tb(3)
          goto 232
        endif
        d2 = s(1,1)*tb(1)+s(2,1)*tb(2)+s(3,1)*tb(3)-s(4,1)
        if (d2.lt.d1) goto 99
        do 238 i=1,7
238     s(i,1) = ss(i)
        goto 99
      endif
      if(ifl(23).eq.2.or.ifl(23).eq.10.or.ifl(23).eq.11.or.
     x   ifl(23).eq.12)goto 99
24    if(ifl(56)-1)99,25,65
c************************************   lev1  ******************
25    si=fx*s(1,1)+fy*s(2,1)+fz*s(3,1)
c             if itim 1 and no deltas, go surfpn again
      if(itim.gt.1.or.secsq.gt.1.e-6)goto 26
c              if o/b case, set fwd from te to sfpt and rgt
      if(u.le.0..or.u.ge.1.)goto 252
      if(v.gt.0..and.v.lt.1.)goto 254
c              o/b case
252   fa=s(5,1)-t(1,ia)
      fb=s(6,1)-t(2,ia)
      fc=s(7,1)-t(3,ia)
      xr=fb*tk-fc*tj
      yr=fc*ti-fa*tk
      zr=fa*tj-fb*ti
      goto 256
254   xr=s(3,1)*tj-s(2,1)*tk
      yr=s(1,1)*tk-s(3,1)*ti
      zr=s(2,1)*ti-s(1,1)*tj
256   fx=zr*tj-yr*tk
      fy=xr*tk-zr*ti
      fz=yr*ti-xr*tj
      goto 18
26    rx=fy*tk-fz*tj
      ry=fz*ti-fx*tk
      rz=fx*tj-fy*ti
      if (ihadit.eq.0 .and. itim.eq.1 .and. secsq.gt.1.e-6) then
        fx0 = fx
        fy0 = fy
        fz0 = fz
        ihadit = 1
      endif
c             lateral angular error
      aerr=rx*s(1,1)+ry*s(2,1)+rz*s(3,1)
      if(abs(aerr).gt..005)goto 27
      if (si.gt..002) then
c
c..... if there was a tool vector, and a good projection was found
c..... (no lateral error) - save it
c
        if (ifl(56).eq.1 .and. ihadit.eq.1 .and. lgoug.ge.2 .and.
     x                                                  .not.lv93) then
          si=fx*fx0+fy*fy0+fz*fz0
          if (si .ge. 0.70710678) then
            ihadit = 2
            do i=1,10
              ss(i) = s(i,1)
            enddo
            uu = u
            vv = v
            ffx = fx
            ffy = fy
            ffz = fz
          endif
        endif
        goto 60
      endif
      goto 99
27    jtim=jtim+1
      if(jtim.gt.1)goto 40
c          time 1.  chg fwd per aerr
      da=0.
      if(si.ne.0.)da=-aerr/si*.5
c          if da unreasonable, simply set it small.
      if(abs(da).gt..5) da=.05
      goto 42
c          time 2 and greater. extrap next rpt
40    den=oerr-aerr
      if(abs(den).gt.1.e-6)rtan=da/den
      da=aerr*rtan
      damax = 1.0d0
c
c..... smal is sin(10)
c
      if (lgoug .ge. 3) damax = smal
      if (da.gt.damax) da = damax
      if (da.lt.-damax) da = -damax
42    oerr=aerr
      if (lf128 .eq. 0) then
         daa = da
         fxx = fx
         fyy = fy
         fzz = fz
      endif
      co=sqrt(1.-da**2)
      fx=fx*co-rx*da
      fy=fy*co-ry*da
      fz=fz*co-rz*da
      if(jtim.lt.10)goto 18
c*******************************  init lev 2 process  ************
60    ifl(56)=2
      ktim=0
      fx=0.
      fy=0.
      fz=0.
      dmov=.667*tool(6)
      goto 20
c          lev2 iteration
65    ktim=ktim+1
      odx=dx
      ody=dy
      odz=dz
      co=ti*s(1,1)+tj*s(2,1)+tk*s(3,1)
c          if sfnm shows fwd sense, go use it
      if(abs(co).lt..99999) goto 68
c          if ta parlel sfnm and gt time 3, use it
      if(co.ge..999999.and.ktim.gt.3)goto 99
c          try sfpt proj on cpl
      dis=ti*(xc-s(5,1))+tj*(yc-s(6,1))+tk*(zc-s(7,1))
      fx=s(5,1)+ti*dis-xc
      fy=s(6,1)+tj*dis-yc
      fz=s(7,1)+tk*dis-zc
      if(fx**2+fy**2+fz**2.gt.1.e-4)goto 69
c          no fwd from sfnm or sfpt.  go use this sfpn data.
      goto 99
68    fx=ti*co-s(1,1)
      fy=tj*co-s(2,1)
      fz=tk*co-s(3,1)
69    sec=sqrt(fx**2+fy**2+fz**2)
      if(sec.le.0.)goto 690
      fx=fx/sec
      fy=fy/sec
      fz=fz/sec
c          move in fwd direction
690   dx=dx+fx*dmov
      dy=dy+fy*dmov
      dz=dz+fz*dmov
      radsq=dx**2+dy**2+dz**2
      if(radsq.lt.tool(6)**2) goto 70
      rad=sqrt(radsq)
      if(tool(6).ne.0.)rad=rad/tool(6)
      if(rad.le.0.)goto 70
      dx=dx/rad
      dy=dy/rad
      dz=dz/rad
70    dmov=dmov*.5
c          if next move small, all done
      disq=(dx-odx)**2+(dy-ody)**2+(dz-odz)**2
      if(disq.lt.1.e-6)goto 99
      goto 22
c          error. too many iters or etc.
98    ifl(2)=128
      goto 999
c          normal exit area.
99    continue
c
c..... if there was a tool vector, and a good projection was found at 
c..... level 1, and the level 2 projection is closer to the other side 
c..... of the tool, use the level 1 projection
c
      if (ihadit.eq.2) then
        si=fx*fx0+fy*fy0+fz*fz0
        if (si .lt. -0.17365) then
          do i=1,10
            s(i,1) = ss(i)
          enddo
          t(13,ia) = uu
          t(14,ia) = vv
          t(16,ia) = ffx
          t(17,ia) = ffy
          t(18,ia) = ffz
          return
        endif
      endif
      t(16,ia)=fx
      t(17,ia)=fy
      t(18,ia)=fz

999   return
      end
