C*********************************************************************
C*    NAME         :  tltang.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       tltang.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:48
C********************************************************************/
C
C*********************************************************************
C  SUBROUTINE     : subroutine tltang(si, co, tpt, tfd, su, sv)
C
C  FUNCTION: this routine calculates the tilt angle of the tool to allow
C            trailing edge to clear ps by user specified distance.
c
c  INPUT:
c            si, co    R4    D1  - sin & cos of the tilt angle
c            tpt       R4    D3  - position of the center of the tool
c            tfd       R4    D3  - forward vector
c            su,sv     R4    D1  - u,v parameters of the surface
c  OUTPUT:
c            si, co    R4    D1  - sin & cos of the new tilt angle
c           
C*************************************************************************

      subroutine tltang (si, co, tpt, tfd, su, sv)
 
      include 'com4a.com'
      include 'const.com'
      include 'mocom.com'

      real*4 si, co, tpt(3), tfd(3), su, sv
c 
      real*4  ss(10)

      integer*2 ia, ib, ic, isrf, isrfsv, jd(600)
      equivalence (ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence (d,jd)

      real*8 
     *       pext(3),ps_plane(7),ps_plane0(7),fd(3),pt(3),vt(3),vec(3),
     *       taxis(3),pt1(3),
     *       cute,hdia,d1,f_dot,f_mag,f_dist,dist,step,sc144,
     *       delta/0.02d00/
      real*4 
     *       u, v, svco, svsi, max2_r4,min2_r4
      integer*2 
     *       N_PTS/9/,iuse,nsf,isf,nb
      logical
     *      lv84

      lv84 = sc(169).lt.8.499d00

      sc144 = sc(144)
      call conv4_8 (t(4,ia),taxis,3)

      svsi = si
      svco = co
      si = 0.
      co = 1.
      hdia = 0.5*sc(28)
      cute=hdia-sc(29)
      if (hdia.lt.sc(27)*2.) goto 997

      step = (hdia + cute)/(N_PTS + one) 
c
c... tool(10) = cldist (clear off distance from end of tool to ps) 
c
      if (jd(1).eq.PLANE) then 
         si = tool(10)/(hdia+cute)
         if (si.gt.1.) si = 1.
         co = sqrt(1.-si*si)
         if (si.gt.svsi) then
           svsi = si
           svco = co
         endif
         goto 997
      endif

      isrfsv = isrf
      call conv4_4(s(1,1),ss,10)
      call conv4_8(ss,ps_plane0,7)
      call conv4_8(tfd,fd,3)
      call conv4_8(tpt,pt,3)
c
c... pext = pt - hdia*fd = back end of the tool 
c
      isrf = 1

      call uvcplvc (pt,fd,pext, -hdia)

      nsf = 1
      if (psmult) call ncl_psmult_nsf (nsf)
c 
c... aak 17-sep-98:
c... loop over all components of a net PS; use only those
c... with ->use flag = 1 (set before by psrel)
c... if nsf > 1, do not consider tool back projections on
c... surface extensions
c
      do 100 isf = 1,nsf

      si = 0.
      iuse =1 
      if (nsf.gt.1) call psload (isf,iuse,nb)
      if (iuse.eq.0) goto 100

      if (nsf.gt.1) then
         u = t(13,ic)
         v = t(14,ic)
      else
         u = su
         v = sv
      endif

      call conv8_4 (pext,s(8,1),3)
      call surfpn (u, v,1)
      if (ifl(2).gt.0) goto 995

      call conv4_8 (s(1,1),ps_plane,7)
      call vctovc (fd,vt)

      if (nsf.gt.1) then
         call point_on_plane (pext,ps_plane(5),ps_plane,vec)
         if (f_dist (vec,ps_plane(5)) .gt. 5.*sc(27) ) goto 200
      endif

      if (f_dot (ps_plane,taxis).lt.ZERO) then
         call mnvc(ps_plane)
         ps_plane(4) = -ps_plane(4)
      endif

      call plndis (ps_plane,pext,d1)
      d1 = d1 - tool(10) - sc(23)
c
c..... vt = pext - d1*ps_plane - pt - cute*fd =
c..... new position of the back end
c
      call uvcplvc(pext,ps_plane,pt1,-d1)
      call vcmnvc(pt1,pt,vt)
      call uvcplvc(vt,fd,vt,-cute)

      if (f_mag(vt).lt. sc(27)*2.) goto 995
      call unitizevc (vt)

      si = f_dot (vt,ps_plane0)
      call mnvc (vt)

200   continue
      if(lv84) goto 3
c**********************************************
c
c..... make sure the inner part of cutter is off the surface:
c..... take N_PTS of control pts on the line from front to back end 
c..... of the tool; if a point is below the surface, increase angle
c..... to make it to be on the surface; finally, take the max angle
c
      dist = ZERO
      
      do 2 i = 1,N_PTS
         dist = dist + step
         call uvcplvc (ps_plane0(5),vt,pt1,-dist)
         call conv8_4 (pt1,s(8,1),3)

         call surfpn (u, v,1)
         if (ifl(2).gt.0) goto 995
         call conv4_8 (s(1,1),ps_plane,7)

         if (nsf.gt.1) then
            call point_on_plane (pt1,ps_plane(5),ps_plane,vec)
            if (f_dist (vec,ps_plane(5)) .gt. 5.*sc(27) ) goto 2
         endif

      	if (f_dot (ps_plane,taxis).lt.ZERO) then
         	call mnvc(ps_plane)
         	ps_plane(4) = -ps_plane(4)
      	endif

         call vcmnvc (pt1,ps_plane(5),vec)

         if (f_dot (vec,ps_plane).le.ZERO) then
            call vcmnvc (ps_plane(5),ps_plane0(5),vec)
            call unitizevc (vec)
            si1 = f_dot (vec,ps_plane0) + delta
            if (si1.ge.si) si = si1
         endif

 2    continue
c**********************************************
 3    continue
      
      si = min2_r4 (1., max2_r4 (0.,si))

      if (si.ge.svsi) then
         svsi = si
         svco = sqrt (1.-si*si)
      endif

100   continue

995   call conv4_4(ss,s(1,1),10)
      isrf = isrfsv

997   continue

      sc(144) = sc144

      si = svsi
      co = svco

      return
      end
