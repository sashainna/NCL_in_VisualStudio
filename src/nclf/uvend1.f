C*********************************************************************
C*    NAME         :  uvend1.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       uvend1.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:52
C*********************************************************************
C
c **********************************************************************
c **********************************************************************
c **  subroutine name: uvend1
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
 
      subroutine uvend1 (kret,rdp)
 
      include 'com4a.com'
      include 'mocom.com'

      real*4 rdp
      integer*2 kret
 
      integer*2 jd(600)
      real*4 ad(300)
      equivalence(d,ad,jd)
      integer*2 itfl,iptk,ntk,ia,ib,ic,ionps,ionds,isrf
      equivalence(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic),(ifl(54),isrf)
      equivalence(ifl(56),ionps),(ifl(57),ionds)
c
c... Local variables
c
      real*8 dst(2), dtol, tol, co, max2_r8
      real*4 v(3), f_dot4, f_dist4

      if (sc(169).lt.8.4999) then
        call uvend1o (kret, rdp)
        return
      endif

      if(jd(1).ne.PLANE) then
        isrf = 1
        call vctovc4 (t(1,ia),s(8,1))
        call surfpn (t(13,ia),t(14,ia),1)
      endif
      if (jd(201) .ne. PLANE) then
        if (jd(201) .eq. SURF) then
          isrf = 2
          call vctovc4 (t(1,ia),s(8,2))
          call surfpn (t(19,ia),t(20,ia),1)
        else
          call vctovc4 (t(1,ia),s(5,2))
        endif
      end if

      kret = 1
      tol  = sc(27)
      dtol = tol 
c
c... distances from point to PS and DS
c
      dst(1) = f_dist4 (s(5,1), t(1,ia))
      dst(2) = f_dist4 (s(5,2), t(1,ia))

      if ( (ionps.eq.1 .and. dst(1).gt.dtol) .or.
     *     (ionds.eq.1 .and. dst(2).gt.dtol) ) then
         ifl(2) = 163
         return
      endif

      if (dst(1).lt.dtol) ionps = 1
      if (dst(2).lt.dtol) ionds = 1

      if (ionps.eq.1 .and. ionds.eq.1) then
         itfl = 100
         return
      endif

      isf = 1
      if ( ionps.eq.1 .or. 
     *   ( ionds.ne.1 .and. dst(1).gt.dst(2)) ) isf = 2

      call vcmnvc4 (s(5,isf),t(1,ia),v)
      call unitizevc4 (v)
      co  = f_dot4 (t(7,ia),v)
      if (co .ge. 1.e-3) dst(isf) = dst(isf)/co
      rdp = max2_r8 (dst(isf),0.5*tol)
      if (sc(169).lt.9.25 .or. dabs(co).ge.0.5) call vctovc4 (v,t(7,ia))

      return
      end
c **********************************************************************
c **  subroutine name: uvend1o
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
 
      subroutine uvend1o (kret,rdp)
 
      include 'com4a.com'
      include 'mocom.com'

      real*4 rdp
      integer*2 kret
 
      integer*2 itfl,iptk,ntk,ia,ib,ic,ionps,ionds
      equivalence(itfl,ifl(49)),(iptk,ifl(50)),(ntk,ifl(79))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic)
      equivalence(ifl(56),ionps),(ifl(57),ionds)

      real*8 tol, dtolsq, psdsq,dsdsq,dis,vx,vy,vz,co,svco

      kret=1
      tol=sc(27)
c      dtolsq=4.*tol**2 
      dtolsq=tol**2 
      psdsq=(s(5,1)-t(1,ia))**2+(s(6,1)-t(2,ia))**2+(s(7,1)-t(3,ia))**2
      dsdsq=(s(5,2)-t(1,ia))**2+(s(6,2)-t(2,ia))**2+(s(7,2)-t(3,ia))**2
      if (ionps.eq.1.and.psdsq.gt.dtolsq) goto 9163
      if (ionds.eq.1.and.dsdsq.gt.dtolsq) goto 9163
      if (psdsq.lt.dtolsq) ionps=1
      if (dsdsq.lt.dtolsq) ionds=1
      if (ionps.eq.1.and.ionds.eq.1) goto 40
      if (ionps.eq.1) goto 20
      if (ionds.eq.1) goto 10
      if (psdsq.gt.dsdsq) goto 20
10    continue
      dis=dsqrt(psdsq)
      vx=s(5,1)-t(1,ia)
      vy=s(6,1)-t(2,ia) 
      vz=s(7,1)-t(3,ia)
      goto 30
20    continue
      dis=dsqrt(dsdsq)
      vx=s(5,2)-t(1,ia)
      vy=s(6,2)-t(2,ia) 
      vz=s(7,2)-t(3,ia)
30    continue
      if (dis.eq.0.) dis=1.
      vx=vx/dis
      vy=vy/dis
      vz=vz/dis
      co=t(7,ia)*vx+t(8,ia)*vy+t(9,ia)*vz
      svco=co
      if (co.lt..001)co=1.
      rdp=dis/co
      if (rdp.lt.tol/2.) rdp=tol/2.
c      if (iptk.gt.0) goto 99
      t(7,ia)=vx
      t(8,ia)=vy
      t(9,ia)=vz
c      if (svco.lt.0.) ad(102)=-ad(102)
      goto 99

40    itfl=10
      goto 99

9163  ifl(2)=163
      goto 99

99    return
      end
