C*********************************************************************
C*    NAME         :  ipatch.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ipatch.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:12
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ipatch(sfu,sfv,iret)
C*
C*       chg  16 nov 82    fix stall-out logic after o/b recognized.
C*       chg   7-dec-82    add calls to debugi
C*
C*          applies the ept in srf(8-10) to the patch in d(isrf)
C*          and loads srf p/n in srf(1-7).
C*
C*          u,v     input:   1st try loc on patch.
C*                  output:  last found values.
C*
C*          iret =  0   all ok. inbounds and good solution.
C*               = -1   u(0) bdy violation -- this patch
C*               = +1   u(1)  "     "
C*               = -2   v(0) bdy violation -- this panel
C*               = +2   v(1)  "     "
C*               = +3   application failed.
C*       description
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
      subroutine ipatch(sfu,sfv,iret)

      include 'com4a.com'
      include 'mocom.com'

c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)

      common/cviofl/cvion
      logical cvion

      real*8 xo,yo,zo
      real*4 ad(300),co(11),rx(4),ry(4),rz(4),x(4),y(4),z(4)
      real*4 qx(10),qy(10),qz(10)
      integer*2 kd(600)
      equivalence (d,ad,kd),(ifl(54),isrf)
      logical lumin,lumax,lvmin,lvmax

c          init for converge steps.  zero iret.
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
      istyp=kd(kdx+1)
      j=jdx
      imid=0
      itim=0
      iret=0
      utan=1.
      vtan=1.
      if (ifl(385) .lt. 0) ifl(385) = -ifl(385)
      ifl(306)=0
      if(istyp.ne.25)goto 4
      u=sfu
      v=sfv
      den=4.
      xo=ad(jdx+23)
      yo=ad(jdx+24)
      zo=ad(jdx+25)
      goto 6

ccc++                 guard against zero divide    9-3-82
c              convert sfu,sfv to patch u,v
4     u=sfu
      v=sfv
      den=3.
c      if (sfu.eq..5.and.sfv.eq..5) goto 5
      uden= ad(jdx+16)-ad(jdx+15)
      vden= ad(jdx+18)-ad(jdx+17)
      if (uden.eq.0..or.vden.eq.0.)goto 9128
      u=(sfu-ad(jdx+15))/uden
      v=(sfv-ad(jdx+17))/vden
5     xo=d(idx+23)
      yo=d(idx+24)
      zo=d(idx+25)
ccc++                  end of chg     9-3-82
c          do not allow o/b u,v
6     if(u.lt.0.)u=0.
      if(u.gt.1.)u=1.
      if(v.gt.1.)v=1.
      if(v.lt.0.)v=0.
c          delta the ept from patch origin
      tx=srf(8,isrf)-xo
      ty=srf(9,isrf)-yo
      tz=srf(10,isrf)-zo

c          solve this u,v position and uerr, verr
c           rpts calc differently for different patch types.
10    if(istyp.ne.25) goto 14
c
c             prep 4 rpts for rockwell 25pt patch
      vm=1.-v
      c1=vm**3
      c2=3.*v*vm**2
      c3=3.*v**2*vm
      c4=v**3
c             v-interp for 10 q-pts
      do 11 i=1,10
      j=jdx+20+5*i
      if(i.gt.5) j=j-24
      qx(i)=c1*ad(j+1)+c2*ad(j+2)+c3*ad(j+3)+c4*ad(j+4)
      j=j+25
      qy(i)=c1*ad(j+1)+c2*ad(j+2)+c3*ad(j+3)+c4*ad(j+4)
      j=j+25
11    qz(i)=c1*ad(j+1)+c2*ad(j+2)+c3*ad(j+3)+c4*ad(j+4)
c             u-interp over q for 4 rpts
      um=1.-u
      c1=um**3
      c2=3.*u*um**2
      c3=3.*u**2*um
      c4=u**3
      do 12 i=1,4
      j=5*i-5
      if(i.gt.2)j=j-9
      rx(i)=c1*qx(j+1)+c2*qx(j+2)+c3*qx(j+3)+c4*qx(j+4)
      ry(i)=c1*qy(j+1)+c2*qy(j+2)+c3*qy(j+3)+c4*qy(j+4)
12    rz(i)=c1*qz(j+1)+c2*qz(j+2)+c3*qz(j+3)+c4*qz(j+4)
      goto 32

14    if(istyp.ne.21)goto 18
c*****************************   rld pat
      b1=(1.-u)**2
      b2=2.*u*(1.-u)
      b3=u**2
      do 16 i=1,4
      j=49+jdx+3*i
      rx(i)=b1*ad(j)+b2*ad(j+6)+b3*ad(j+12)
      ry(i)=b1*ad(j+1)+b2*ad(j+7)+b3*ad(j+13)
16    rz(i)=b1*ad(j+2)+b2*ad(j+8)+b3*ad(j+14)
      goto 32
c**********************************   full pat
18    a=(1.-u)*(1.-v)
      b=(1.-u)*v
      c=u*(1.-v)
      e=u*v
      co(1)=a**2
      co(2)=2.*a*b
      co(3)=b**2
      co(4)=0.
      co(5)=2.*a*c
      co(6)=2.*(b*c+a*e)
      co(7)=2.*b*e
      co(8)=0.
      co(9)=c**2
      co(10)=2.*c*e
      co(11)=e**2
c          do 4 rpts
      do 30 i=1,4
      rx(i)=0.
      ry(i)=0.
      rz(i)=0.
c          set p-ptr
      ip=j+46+3*i
      if(i.gt.2)ip=ip+6
c          do the 11 sum
      do 20 k=1,11
      ip=ip+3
      rx(i)=rx(i)+co(k)*ad(ip)
      ry(i)=ry(i)+co(k)*ad(ip+1)
20    rz(i)=rz(i)+co(k)*ad(ip+2)
30    continue
c*****************************************
c          4 int pts  ( 2 & 4 are actually deltas )
32    x(1)=rx(1)+v*(rx(2)-rx(1))
      y(1)=ry(1)+v*(ry(2)-ry(1))
      z(1)=rz(1)+v*(rz(2)-rz(1))
      x(2)=rx(3)+v*(rx(4)-rx(3))-x(1)
      y(2)=ry(3)+v*(ry(4)-ry(3))-y(1)
      z(2)=rz(3)+v*(rz(4)-rz(3))-z(1)
      x(3)=rx(1)+u*(rx(3)-rx(1))
      y(3)=ry(1)+u*(ry(3)-ry(1))
      z(3)=rz(1)+u*(rz(3)-rz(1))
      x(4)=rx(2)+u*(rx(4)-rx(2))-x(3)
      y(4)=ry(2)+u*(ry(4)-ry(2))-y(3)
      z(4)=rz(2)+u*(rz(4)-rz(2))-z(3)
c
      xn=y(2)*z(4)-z(2)*y(4)
      yn=z(2)*x(4)-x(2)*z(4)
      zn=x(2)*y(4)-y(2)*x(4)
c----------
c      if(ifl(85).lt.0)calldebugi(1,itim)
c----------
c
      xu=y(4)*zn-z(4)*yn
      yu=z(4)*xn-x(4)*zn
      zu=x(4)*yn-y(4)*xn
c
      xv=z(2)*yn-y(2)*zn
      yv=x(2)*zn-z(2)*xn
      zv=y(2)*xn-x(2)*yn
ccc--          guard against zero divide    9-3-82
      uden= xu*x(2)+yu*y(2)+zu*z(2)
      vden= xv*x(4)+yv*y(4)+zv*z(4)
      if(uden.eq.0..or.vden.eq.0.)goto 71
      uerr=(xu*(tx-x(1))+yu*(ty-y(1))+zu*(tz-z(1))) /uden-u
      verr=(xv*(tx-x(3))+yv*(ty-y(3))+zv*(tz-z(3))) /vden-v
ccc--              end of chg  9-3-82
c
c..... Non-standard convergence: try going the opposite way.
c..... Used for projecting with a near point.
c
         if (ifl(385) .eq. 1 .or. ifl(385) .eq. 3) uerr = -uerr
         if (ifl(385) .eq. 2 .or. ifl(385) .eq. 3) verr = -verr

      uerr=uerr/den
      if(istyp.ne.21)verr=verr/den
c      if(istyp.ne.25) goto 38
c      uerr=uerr*.75
c      verr=verr*.75
c38    continue
      if(itim.gt.0)goto 40
      du=uerr
      dv=verr
      if(abs(uerr).lt..0001.and.abs(verr).lt..0001)goto 50
      if (abs(du).gt..001) goto 38
      du=.001
      if (uerr.lt.0.)du=-.001
38    if (abs(dv).gt..001) goto 50
      dv=.001
      if (verr.lt.0.)dv=-.001
      goto 50
40    uden=ouerr-uerr
      if(abs(uden).lt.1.e-5)goto 42
      utan=du/uden
c          neg tan is unreal
      if(utan.le.0.)utan=1.
42    du=uerr*utan
      vden=overr-verr
      if(abs(vden).lt.1.e-5)goto 44
      vtan=dv/vden
c          same for vtan
      if(vtan.le.0.)vtan=1.
44    dv=verr*vtan
50    hu=u+uerr
      hv=v+verr
      if(u+du.lt.0.)du=-u
      if(u+du.gt.1.)du=1.-u
      if(v+dv.lt.0.)dv=-v
      if(v+dv.gt.1.)dv=1.-v
      u=u+du
      v=v+dv
      if(abs(uerr).lt..0001.and.abs(verr).lt..0001)goto 70
      itim=itim+1
      ouerr=uerr
      overr=verr
      if(itim.gt.120)goto 71
c          if uv chg real, go again.
      if(abs(du).le.1.e-5.and.abs(dv).le.1.e-5)goto 60
      if (itim.lt.30) goto 10
      if (sc(169).lt.8.20799) goto 10
c
c... Having trouble converging. Project look point to surface & compare with
c... surface point. First calculate surface point
c
      um = u-du
      vm = v-dv
      sx =x(1)+x(2)*um
      sy =y(1)+y(2)*um
      sz =z(1)+z(2)*um
c
c...  Project tool look point to surface
c
      sec = sqrt(xn**2+yn**2+zn**2)
      if (sec.lt.1.e-6) goto 9128
      a = xn/sec
      b = yn/sec
      c = zn/sec
      d1 = a*(tx-sx)+b*(ty-sy)+c*(tz-sz)
      x1 = tx-a*d1
      y1 = ty-b*d1
      z1 = tz-c*d1
c
c...  If on U extension of surface, project sf pt to plane of tool pt
c
      if (um.gt.0.0.and.um.lt.1.0) goto 52
      sec = sqrt(x(2)**2+y(2)**2+z(2)**2)
      if (sec.lt.1.e-6) goto 9128
      xu = x(2)/sec
      yu = y(2)/sec
      zu = z(2)/sec
      d1 = xu*(sx-x1)+yu*(sy-y1)+zu*(sy-z1)
      sx = sx-xu*d1
      sy = sy-yu*d1
      sz = sz-zu*d1
c
c...  If on V extension of surface, project sf pt to plane of tool pt
c
52    if (vm.gt.0.0.and.vm.lt.1.0) goto 54
      sec = sqrt(x(4)**2+y(4)**2+z(4)**2)
      if (sec.lt.1.e-6) goto 9128
      xv = x(4)/sec
      yv = y(4)/sec
      zv = z(4)/sec
      d1 = xv*(sx-x1)+yv*(sy-y1)+zv*(sz-z1)
      sx = sx-xv*d1
      sy = sy-yv*d1
      sz = sz-zv*d1
c
c... If projected tool point in tolerance with surface point, exit
c
54    dsq = (x1-sx)**2+(y1-sy)**2+(z1-sz)**2
      if (dsq.gt.sc(27)**2) goto 10
c
c          stall-out.  (no real chg in u,v)
c            scan patches until best found for current panel.
c            chg panels only as indicated after above search.
c
c              (allow .001 u,v excess without setting iret)
c      if(istyp.ne.25) goto 70
60    uoffm=abs(hu-.5)
      voffm=abs(hv-.5)
      stol = 0.001
      if (cvion) stol = 0.0002

      if(uoffm.lt.voffm)goto 65
c              it is apparently a u-violation
      if(hu.lt.-stol) iret=-1
      if(hu.gt.(1.+stol)) iret=1
      if(hv.lt.-stol) ifl(306)=-2
      if(hv.gt.(1.+stol)) ifl(306)=2
      goto 70
c               apparently a v-violation
65    if(hv.lt.-stol) iret=-2
      if(hv.gt.(1.+stol)) iret=2
      if(hu.lt.-stol) ifl(306)=-1
      if(hu.gt.(1.+stol)) ifl(306)=1
      goto 70

c70    npats=kd(kdx+38)
c      ipat=kd(kdx+41)
c      if(hu.ge.-.001)goto 72
c      if(ipat.lt.2)goto 74
c      iret=-1
c      goto 99
c72    if(hu.le.1.001)goto 74
c      if(ipat.gt.npats-1)goto 74
c      iret=1
c      goto 99
c74    if(hv.lt.-.001)iret=-2
c      if(hv.gt.1.001)iret=2
c
c          calc tanpl
70    a=xn
      b=yn
      c=zn
      sec=sqrt(a**2+b**2+c**2)*ad(jdx+2)
      if(abs(sec).gt.1.e-6)goto 92

c          solution failed.  re-try from midpt if feasible.  15-nov-84
71    if(imid.gt.1.or.itim.eq.0) goto 80
      imid=imid+1
c          decrease iteration step for second retry
      if (imid.gt.1) den = den*2.
      itim=0
      u=.5
      v=.5
      goto 10
80    continue
      goto 9128
c          normal exit.  load vec in srf(1-3)
92    srf(1,isrf)=a/sec
      srf(2,isrf)=b/sec
      srf(3,isrf)=c/sec
c          srf pt
      srf(5,isrf)=x(1)+x(2)*u+xo
      srf(6,isrf)=y(1)+y(2)*u+yo
      srf(7,isrf)=z(1)+z(2)*u+zo
c                        if offset surface, chg srf pt      20-jan-87
      if (kd(kdx+5).ne.7)goto 93
      dis=ad(jdx+2)*ad(jdx+4)
      srf(5,isrf)=srf(5,isrf)+srf(1,isrf)*dis
      srf(6,isrf)=srf(6,isrf)+srf(2,isrf)*dis
      srf(7,isrf)=srf(7,isrf)+srf(3,isrf)*dis
93    continue
c          tanpl const
      srf(4,isrf)=srf(1,isrf)*srf(5,isrf)+srf(2,isrf)*srf(6,isrf)+
     1 srf(3,isrf)*srf(7,isrf)
c          convert back to sfu,sfv
      if(istyp.ne.25) goto 94
      sfu=u
      sfv=v
      goto 99
94    sfu=ad(jdx+15)+u*(ad(jdx+16)-ad(jdx+15))
      sfv=ad(jdx+17)+v*(ad(jdx+18)-ad(jdx+17))
c            if ds and ta 'parelm', add v-vec to tcol(22-24)  23-jul-86
99    if(isrf.ne.2.or.ifl(23).ne.6)goto 999
      ia=ifl(51)
      if (rldinu) then
        t(22,ia)=x(2)
        t(23,ia)=y(2)
        t(24,ia)=z(2)
      else
        t(22,ia)=x(4)
        t(23,ia)=y(4)
        t(24,ia)=z(4)
      endif
999   continue

c
c..... Check if doing a non-standard convergence results in a wrong extension.
c
           if (ifl(385) .gt. 0) then

             tx = srf(8,isrf) - srf(5,isrf)
             ty = srf(9,isrf) - srf(6,isrf)
             tz = srf(10,isrf) - srf(7,isrf)

             uden = tx*xu + ty*yu + tz*zu
             vden = tx*xv + ty*yv + tz*zv

             lumin = (u .lt. 1.e-5)
             lvmin = (v .lt. 1.e-5)
             lumax = (u .gt. 1. - 1.e-5)
             lvmax = (v .gt. 1. - 1.e-5)

             if ((lumin .and. uden.gt.0) .or.
     x           (lumax .and. uden.lt.0) .or.
     x           (lvmin .and. vden.gt.0) .or.
     x           (lvmax .and. vden.lt.0)) then
               ifl(385) = -ifl(385)
               if (iret .eq. 0) then
                 if (lumin .and. uden.gt.0.0001) then
                   iret = -1
                 else if (lumax .and. uden.lt.-0.0001) then
                   iret = 1
                 else if (lvmin .and. vden.gt.0.0001) then
                   iret = -2
                 else if (lvmax .and. vden.lt.-0.0001) then
                   iret = 2
                 endif
               endif 
             endif

           endif
    
      return
c             Error - surf solution failed
9128  iret=3
      ifl(2)=128
      goto 999
      end
