C*********************************************************************
C*    NAME         :  uvend2.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       uvend2.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:52
C*********************************************************************
C
c **********************************************************************
c **********************************************************************
c **  subroutine name: uvend2
c **
c **  last revision:
c **  purpose of subroutine: this routine handles endup when driving to
c **      the edge of a surface.
c **  input -
c **    rdp  = size of step just made
c **  output -
c **    kret = 1 - use the last calculated point
c **         = 0 - don't use the last calculated point
c **    rdp  = new step size
c **
c **********************************************************************
c **********************************************************************
 
      subroutine uvend2 (kret,rdp)
 
      include 'com4a.com'
      include 'mocom.com'
 
      real*4 rdp
      integer*2 kret
 
      integer*2 jd(600)
      real*4 ad(300)
      integer*2 itfl,iptk,ntk,ia,ib,ic,ionps,ionds
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic)
      equivalence(ifl(56),ionps),(ifl(57),ionds)

      real*8 tol,psd,dsd,dis,co,u,v,umin,umax,vmin,vmax
      real*4 vec(3), f_dist4, f_mag4, f_dot4
      logical lclose,lv92,lcvds
      integer*2 i

      if (sc(169) .lt. 8.4999) then
        call uvend2o(kret,rdp)
        goto 99
      endif

      lv92 = sc(169).lt.9.2d0
      if (lv92 .or. ifl(331).eq.0) then
        umin = 0.
        umax = 1.
        vmin = 0.
        vmax = 1.
      endif
      kret = 1
      tol  = 0.2*sc(27)
      tol1 = sc(27)

      lcvds = (jd(201).eq.LINE .or. jd(201).eq.CIRCLE .or.
     x         jd(201).eq.CURVE)

      psd = f_dist4 (s(5,1), t(1,ia))
      if (lcvds) then
        dsd = 0
      else
        dsd = f_dist4 (s(5,2), t(1,ia))
      endif
      lclose = (psd.lt.tol .and. dsd.lt.tol)
      if (iptk.eq.0) lclose = .true.

      if (itfl.le.0) then
         u=t(13,ia)
         v=t(14,ia)
c
c... aak 21-may-1998: relaxed tolerance (tol1 instead of tol)
c... in this case; sometimes the point is off the srf's by
c... a bigger delta then tol in the very beginning of move;
c... as a result, uvend2 thinks that the move has been already 
c... completed and does not create a single point.
c
         if (.not.lv92 .and. ifl(331).eq.1) then
           i = 1
           call gtmmuv(i,umin,vmin,umax,vmax)
         endif
         umin = umin + 1.d-4
         vmin = vmin + 1.d-4
         umax = umax - 1.d-4
         vmax = vmax - 1.d-4
         if (u.le.umin.or.u.ge.umax.or.v.le.vmin.or.v.ge.vmax) then
            lclose = (psd.lt.tol1 .and. dsd.lt.tol1)
            goto 10
         endif

         if (jd(201).ne.PLANE .and. .not.lcvds) then
            u=t(19,ia)
            v=t(20,ia)
            if (.not.lv92 .and. ifl(332).eq.1) then
              i = 2
              call gtmmuv(i,umin,vmin,umax,vmax)
            else
              umin = 0.
              umax = 1.
              vmin = 0.
              vmax = 1.
            endif
            umin = umin + 1.d-4
            vmin = vmin + 1.d-4
            umax = umax - 1.d-4
            vmax = vmax - 1.d-4
            if (u.le.umin.or.u.ge.umax.or.v.le.vmin.or.v.ge.vmax) then
               lclose = (psd.lt.tol1 .and. dsd.lt.tol1)
               goto 10
            endif
         endif

         return
      endif

10    continue


      if (itfl.eq.0.and.lclose) return

      if (itfl.ge.20) then

         if (.not.lclose) then
            itfl=100
         else
            rdp =  tol*100.
            kret = 1
            itfl = 0
         endif

         return
      endif

      if (lclose) goto 40

      isf = 2
      if (psd.ge.dsd) isf = 1

      call vcmnvc4 (t(1,ia),s(5,isf),vec)
      dis = f_mag4 (vec)

      if (dis.ne.0.) then
         call unitizevc4 (vec)
         co = f_dot4 (t(7,ia),vec)
         if (co.lt.1.d-6) co=1.
         dis = dis/co
         if (dis.lt.0.5*tol) goto 40
         rdp = t(10,ia)-dis
         if (rdp.lt.0.) rdp=t(10,ia)/2.
         kret = 0
         if (rdp.lt.tol/2.) goto 40
         itfl = itfl+1
         return
      endif

40    itfl = 20
      rdp  = tol*5.

99    return
      end
c **********************************************************************
c **  subroutine name: uvend2o
c **
c **  last revision:
c **  purpose of subroutine: this routine handles endup when driving to
c **      the edge of a surface when the version flag is less than 9.0.
c **  input -
c **    rdp  = size of step just made
c **  output -
c **    kret = 1 - use the last calculated point
c **         = 0 - don't use the last calculated point
c **    rdp  = new step size
c **
c **********************************************************************
c **********************************************************************
 
      subroutine uvend2o (kret,rdp)
 
      include 'com4a.com'
      include 'mocom.com'
 
      real*4 rdp
      integer*2 kret
 
      integer*2 jd(600)
      real*4 ad(300)
      integer*2 itfl,iptk,ntk,ia,ib,ic,ionps,ionds
      equivalence(d,ad,jd),(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic)
      equivalence(ifl(56),ionps),(ifl(57),ionds)

      real*8 tol, dtolsq, psdsq,dsdsq,dis,vx,vy,vz,co
      real*4 u,v

      kret=1
      if (itfl.gt.0) goto 10
      u=t(13,ia)
      v=t(14,ia)
      if (u.eq.0.or.u.eq.1.or.v.eq.0.or.v.eq.1) goto 10
      if (jd(201).eq.6) goto 99
      u=t(19,ia)
      v=t(20,ia)
      if (u.eq.0.or.u.eq.1.or.v.eq.0.or.v.eq.1) goto 10
      goto 99
10    continue
      tol=sc(27)
c      tol=sc(168)
c      if (sc(169).lt.8.22999) tol = sc(27)
      dtolsq=tol**2 
      psdsq=(s(5,1)-t(1,ia))**2+(s(6,1)-t(2,ia))**2+(s(7,1)-t(3,ia))**2
      dsdsq=(s(5,2)-t(1,ia))**2+(s(6,2)-t(2,ia))**2+(s(7,2)-t(3,ia))**2
      if (itfl.ge.5) goto 50
      if (itfl.eq.0.and.psdsq.lt.dtolsq.and.dsdsq.lt.dtolsq) goto 99
      if (psdsq.lt.dtolsq.and.dsdsq.lt.dtolsq) goto 40
      if (psdsq.lt.dsdsq) goto 20
      dis=dsqrt(psdsq)
      vx=t(1,ia)-s(5,1)
      vy=t(2,ia)-s(6,1)
      vz=t(3,ia)-s(7,1)
      goto 30
20    continue
      dis=dsqrt(dsdsq)
      vx=t(1,ia)-s(5,2)
      vy=t(2,ia)-s(6,2)
      vz=t(3,ia)-s(7,2)
30    continue
      if (dis.eq.0.) goto 40
      vx=vx/dis
      vy=vy/dis
      vz=vz/dis
      co=t(7,ia)*vx+t(8,ia)*vy+t(9,ia)*vz
      if (co.lt..001)co=1.
      dis=dis/co
      if (dis.lt.tol/2.) goto 40
      rdp=t(10,ia)-dis
      if (rdp.lt.0.) rdp=t(10,ia)/2.
      kret=0
      if (rdp.lt.tol/2.) goto 40
      itfl=itfl+1
      goto 99

40    itfl=5
      rdp=tol*5.
      goto 99

50    continue
      if (psdsq.lt.dtolsq.and.dsdsq.lt.dtolsq) goto 60
      itfl=10
      goto 99

60    rdp=tol*100.
      kret=1
      itfl=0

99    return
      end
