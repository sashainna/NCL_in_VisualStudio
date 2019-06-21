C*********************************************************************
C*    NAME         :  meskpn.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       meshpn.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:16
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine meshpn (u,v,iret)
c*          this routine is called by surfpn to handle loading of
c*          mesh patch if necessary and call ipatch to calculate pt &
c*          normal to surface.
C*    PARAMETERS   
C*       INPUT  : 
C*          u         u value to start at
C*          v         v value to start at
C*       OUTPUT :  
C*          u         ending u value
C*          v         ending v value
C*          iret      return value (see ipatch for usage)
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine meshpn (u,v,iret)

      include 'com4a.com'
      include 'mocom.com'

      real*4 u,v
      integer*2 iret

      real*8 e(14)
      real*4 ad(300),ae(28)
      integer*2 kd(600)
      integer*4 nclkey
      integer*2 nwds,ietype
      equivalence (d,ad,kd),(e,ae),(ifl(54),isrf)

      call gtdesc(sc(isrf+143),nclkey,nwds,ietype)
c          set ptrs to srf in d-tbl
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
c          safety counter for ipatch calls
      ipatk=0
      npats=kd(kdx+2)
      nmax=kd(kdx+37)
      mmax=kd(kdx+38)
      ipat=kd(kdx+39)
      if (ipat.eq.0) goto 5

      m=kd(kdx+41)
      n=kd(kdx+42)
      goto 10
c                        no patch in d-table - set up to load patch based on u,v
5     continue
      xm=mmax
      xn=nmax
      m=xm*u+.99
      n=xn*v+.99
      if (m.lt.1)m=1
      if (m.gt.mmax)m=mmax
      if (n.lt.1)n=1
      if (n.gt.nmax)n=nmax
      kd(kdx+41)=m
      kd(kdx+42)=n
      ipat=(n-1)*mmax+m
      goto 100

10    ipatk=ipatk+1
      if (ipatk.le.npats) goto 20
      ifl(2)=138
      goto 999
20    call ipatch(u,v,iret)
      if (iret.eq.0.or.iret.eq.3) goto 999
      kret=iret
      if (abs(kret).eq.1) goto 50
22    if (kret.eq.2) goto 30
      if (n.gt.1) goto 25
      if (ifl(306).eq.0) goto 999
      kret=ifl(306)
      ifl(306)=0
      goto 50
25    n=n-1
      kd(kdx+42)=n
      ipat=ipat-mmax
      goto 100
30    if (n.lt.nmax) goto 35
      if (ifl(306).eq.0) goto 999
      kret=ifl(306)
      ifl(306)=0
      goto 50
35    n=n+1
      kd(kdx+42)=n
      ipat=ipat+mmax
      goto 100
50    if (kret.eq.1) goto 60
      if (m.gt.1) goto 55
      if (ifl(306).eq.0) goto 999
      kret=ifl(306)
      ifl(306)=0
      goto 22
55    m=m-1
      kd(kdx+41)=m
      ipat=ipat-1
      goto 100
60    if (m.lt.mmax) goto 65
      if (ifl(306).eq.0) goto 999
      kret=ifl(306)
      ifl(306)=0
      goto 22
65    m=m+1
      kd(kdx+41)=m
      ipat=ipat+1

100   call gtmptt(nclkey,ipat,d(idx+25))
      d(idx+23)=d(idx+25)
      d(idx+24)=d(idx+26)
      d(idx+25)=d(idx+27)
      ad(jdx+52)=0.
      ad(jdx+53)=0.
      ad(jdx+54)=0.
      xmm=mmax
      xm=m
      xnm=nmax
      xn=n
      ad(jdx+15)=(xm-1.)/xmm
      ad(jdx+16)=xm/xmm
      ad(jdx+17)=(xn-1.)/xnm
      ad(jdx+18)=xn/xnm
      kd(kdx+39)=ipat
      goto 10
999   return
      end
