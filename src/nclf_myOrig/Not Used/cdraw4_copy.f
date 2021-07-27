
C*********************************************************************
C*    NAME         :  cdraw4.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cdraw4.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:39
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cdraw4 (tijk)
C*       purpose of program: draws the cutter.
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
      subroutine cdraw4 (tijk)
c
      include 'com4a.com'
c
      real*4 tijk(3)

      common/cutcom/ icutr
      integer*2 icutr(540),cutter(540)
      equivalence (icutr,cutter)
      common/ctrcom/ rads,trad,xd,yd,dhgt,ccr,crad,tc,tcs,cp,cr,
     x               len,vc,te,cc,vs,cosa,cosb,cirx,ciry,ainc,amin,
     x               is1,is2,is3,iflg,iflg1,iflg2

      real*4 vc(2),cp(4),cv(3),te(4),cc(8)
      real*8 tc(3),vs(2),tcs(2),scale
      equivalence (sc(106),scale)
      integer*2 ncutr
      equivalence (ifl(251),ncutr)
      real*4 tvsav(3)
      equivalence (sc(139),tvsav)
c
      real*4 i1,j1,i2,j2,len

      if (iflg.eq.1) goto 1500
c
c...calculate circular point
c
      angl   = 0.0
      ipstmv = 1
      ncutr  = ncutr  + 1
      cutter(ncutr) = -9999
      cv(is3) = 0.

 1300 cv(is1) = cos (angl/rads)
      cv(is2) = sin (angl/rads)
      vx    = cv(2) * tijk(3) - cv(3) * tijk(2)
      vy    = cv(3) * tijk(1) - cv(1) * tijk(3)
      vz    = cv(1) * tijk(2) - cv(2) * tijk(1)
      d1    = sqrt (vx**2 + vy**2 + vz**2)
      vx    = vx     / d1
      vy    = vy     / d1
      vz    = vz     / d1
      tc(1) = cr * vx
      tc(2) = cr * vy
      tc(3) = cr * vz
c
c...store corner radius flat
c
      call cvp (tc,tcs)
      cutter(ncutr+1) = (tcs(1)*100.)
      cutter(ncutr+2) = (tcs(2)*100.)
      ncutr  = ncutr  + 2
c
c...increment angle
c
      if (ipstmv .eq. 0) go to 1400
      ipstmv = 0
      cutter(ncutr+1) = -10001
      ncutr  = ncutr  + 1
 1400 if (angl .ge. 360.0) go to 1500
      angl   = angl   + ainc
      if (angl .gt. 360.0) angl = 360.0
      go to 1300

1500  if (iflg1.eq.0) goto 99999

      i2     = te(2) * (0-1)
      j2     = te(1)
      a      = te(1) - cp(1)
      b      = te(2) - cp(2)
      len    = sqrt (a**2 + b**2)
      x3     = cp(1) + a/2
      y3     = cp(2) + b/2
      i1     = b      / len
      j1     = a      / len    * (0-1)
      den    = i1*j2 - j1*i2
      q1     = y3*i1 - x3*j1
      q2     = te(1)*j2 - te(2)*i2
      cirx   = (i2*q1 + i1*q2) / den
      ciry   = (j2*q1 + j1*q2) / den
      ci     = cirx   - cp(1)
      cj     = ciry   - cp(2)
      cr     = sqrt (ci**2 + cj**2)
      if (cr .le. .003) go to 99999
c
c...get starting and ending angle
c
      cosa   = ci / cr * (-1.)
      if (cosa .gt. 1.) cosa = 1.
      if (cosa .lt. -1.) cosa = -1.
      cosa   = acos (cosa) * rads
      if (cj .gt. 0.) cosa = 360. - cosa
      d1     = te(2) - ciry
      if (d1 .ge. -.00011 .and. d1 .lt. 0.) d1 = 0.
      cosb   = (te(1)-cirx) / cr
      if (cosb .gt. 1.0) cosb = 1.0
      if (cosb .lt. -1.0) cosb = -1.0
      cosb   = acos (cosb) * rads
      if (d1 .lt. 0.) cosb = 360. - cosb
      if (cosa .lt. cosb) cosa = cosa + 360.
      rr     = abs ((cr-.003) / cr)
      if (rr .gt. 1.0) rr = 1.0
      ainc   = acos (rr) * rads * 2.
      if (ainc .lt. amin) ainc = amin

99999 return
      end
