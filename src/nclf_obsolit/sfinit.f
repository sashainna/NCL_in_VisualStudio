C*********************************************************************
C*    NAME         :  sfinit.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       sfinit.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:41
C*********************************************************************
C
C **********************************************************************
C **  SUBROUTINE NAME: SFINIT
C **
C **  LAST REVISION:
C **  PURPOSE OF SUBROUTINE: INITIALIZE SURFACE TABLES
C **  INPUT -
C **    ASW      - ASSOCIATED WORD OF SURFACE
C **    ISF      - INDEX TO SURFACE TABLE
C **  OUTPUT -
C **    NONE
C **
C **********************************************************************
C **********************************************************************
 
      subroutine sfinit (asw, isf, u, v)
 
      include 'com4a.com'
      include 'mocom.com'
      include 'drvcom.com'

      REAL*8 ASW
      INTEGER*2 ISF
      REAL*4 U, V
 
c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)

      REAL*8 ASN
      REAL*4 AD(300)
      INTEGER*2 KD(600),KSN(4),KTV(4)
      EQUIVALENCE (D,AD,KD),(IFL(54),ISRF),(ASN,KSN),(TV,KTV)
      integer*4 nclkey
      integer*2 nwds, ietype

      IDX=(ISF-1)*50
      JDX=IDX*2
      KDX=IDX*4
      ISFX=(ISF-1)*MXSFHD
      IPNX=(ISF-1)*MXPNHD
      ASN=ASW
      call gtdesc(asn,nclkey,nwds,ietype)
      if (ifl(270).eq.isf) then
        iwf = 1
      else
        call isitwf (nclkey, iwf)
      endif
c
c..... aak 20-mar-1998: changes to fix go2 statement with net srf in OML
c
      if (iwf.eq.1 .and. ietype.ne.NETSF) then
           ad(jdx+2) = 1.
           call evstup (nclkey, isf)
           kd(kdx+1) = SURF
           goto 40
      endif

      call gtgeom (asn, srfhed(isfx+1), nclkey, nwds, ietype)
      D(IDX+1)=SRFHED(ISFX+1)
      D(IDX+2)=SRFHED(ISFX+2)
c
c... probably OML surface
c
      if (srfhed(isfx+1) .eq. 0) then
        AD(JDX+2)=1.
        KD(KDX+1)=9
        goto 40
      endif
c
c... super mesh (net) surface     18-DEC-86
c
      if (kd(kdx+1).eq.NETSF) then
        ksn(1) = 27
        ksn(2) = kd(kdx+2)
        ksn(3) = 0
        issx =(isf-1)*82
        sd(issx+1) = asn
        sd(issx+2) = asw
        goto 40
      endif

      IF (KD(KDX+1).NE.26) GOTO 10
C                    MESH SURF
      KD(KDX+37)=KD(KDX+4)
      KD(KDX+38)=KD(KDX+3)
      KD(KDX+39)=0
      AD(JDX+2)=1.
c      D(IDX+2)=ASW
      GOTO 40

10    AD(JDX+2)=1.
      IF(KD(KDX+1).NE.25)GOTO 30
C                     QUILT SRF
      D(IDX+2)=ASW
      DO 20 I=IDX+3,IDX+10
20    D(I)=0.
      GOTO 40
c                     NCL surface
30    KD(KDX+1)=9
C                     GET PANEL
      npans=kd(kdx+2)
      fnpans=npans
      ipan=v*fnpans+.99
      if (ipan.lt.1) ipan=1
      if (ipan.gt.npans) ipan=npans
      call gtspan (nclkey, ipan, panhed(ipnx+1))
      D(IDX+10)=PANHED(IPNX+1)
      KD(KDX+39)=0
      KD(KDX+41)=0
      KD(KDX+42)=ipan
      fipan=ipan
c
c..... avoid zero division - for OML
c
      if (fnpans .eq. 0) then
        AD(JDX+17)=0
        AD(JDX+18)=0
      else
        AD(JDX+17)=(fipan-1.)/fnpans
        AD(JDX+18)=fipan/FNPANS
      endif
      IF(KD(KDX+37).EQ.1)KD(KDX+1)=21

40    CONTINUE
      sc(143+isf)=asw
 
999   RETURN
      END

C*********************************************************************
C*    E_SUBROUTINE     : subroutine sfini1 (asw,isf,ust,vst,te,ta)
C*      Initialize the part surface. Copied form psinit, with the added
C*      search for good initial (u,v). Here real units (inches or MM)
C*      are used.
C*    PARAMETERS
C*       INPUT  :
C*          isf      - surface number
C*          asw      - ASW of part surface
C*          ust,vst  - starting u,v
C*          te       - Tool end point.
C*          ta       - Tool axis.
C*       OUTPUT :
C*          ust,vst  - starting u,v
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine sfini1 (asw,isf,ust,vst,te,ta)

      include 'com.com'
      include 'mocom.com'

      real*8 asw, te(3), ta(3)
      real*4 ust,vst
      integer*2 isf
c
c..... motion common equivalences
c
      real*4 ad(300)
      integer*2 kd(600)
      equivalence (d,ad,kd)

      real*4 asc(100), dsk
      equivalence (sc,asc),(asc(66),dsk)

      integer*2 ia, isrf
      equivalence (ifl(51),ia),(ifl(54),isrf)
c
c..... local variables
c
      real*8 f_dot
      real*8 svc(3),d0,dij,tol,tolsq
      real*4 u,v,u0,v0
      integer*2 idx,jdx

      call getsct (tol)

      ia = 3
      isrf = isf
      IDX=(ISF-1)*50
      JDX=IDX*2

      call conv8_4 (te,t(1,ia),3)
      call conv8_4 (ta,t(4,ia),3)
      call xyzvc4  (0.,0.,0.,t(7,ia))

      s(8,isf)  = te(1)
      s(9,isf)  = te(2)
      s(10,isf) = te(3)

      u = ust
      v = vst

      call sfinit (asw, isf, ust, vst)

      call surfpn (u,v,1)
      if (ifl(2).gt.0) return

      tolsq = tol*tol

      svc(1) = s(8,isf) - s(5,isf)
      svc(2) = s(9,isf) - s(6,isf)
      svc(3) = s(10,isf) - s(7,isf)
      d0 = f_dot(svc,svc)
      u0 = u
      v0 = v
      if (d0 .lt. tolsq) goto 50

      do i = 1,3
        do j = 1,3
          u = 0.5*(i-1)
          v = 0.5*(j-1)
          if (abs(u-ust).gt.0.001 .or. abs(v-ust).gt.0.001) then
            call surfpn (u,v,1)
            if (ifl(2).eq.0) then
               svc(1) = s(8,isf) - s(5,isf)
               svc(2) = s(9,isf) - s(6,isf)
               svc(3) = s(10,isf) - s(7,isf)
               dij = f_dot(svc,svc)
               if (dij .lt. tolsq) goto 50
               if (dij .lt. d0) then
                 u0 = u
                 v0 = v
                 d0 = dij
               endif
            endif
          endif
        enddo
      enddo

      u = u0
      v = v0

50    continue
      ust = u
      vst = v
      sfa = s(1,isf)
      sfb = s(2,isf)
      sfc = s(3,isf)
c
c..... if psnorm not up, reverse direc(1) and ps tanpl.
c
      if (ta(1)*sfa+ta(2)*sfb+ta(3)*sfc .lt. 0.) then

      AD(JDX+2) = -AD(JDX+2)

      call mnvc4 (s(1,isf))
      s(4,isf) = -s(4,isf)
      endif

999   return
      end
