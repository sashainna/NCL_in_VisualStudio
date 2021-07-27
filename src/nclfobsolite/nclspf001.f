c*****************************************************
c**    FILE: nclspf001.f
c**
c**    CONTAINS: Fortran routines used in NCL and IGES.
c**              Here NCL surface evaluation routines.
c**
c**              patexp (ipnkey,ipat,panel,pat)
c**              nsfevl (asw, sfu, sfv, pvs)
c**              mshevl (asw, sfu, sfv, pvs)
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       nclspf001.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:19
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: PATEXP (IPNKEY,IPAT,PANEL,PAT)
C **
C **  PURPOSE OF SUBROUTINE: EXPAND NCL SURFACES FROM RAN FILE FORM TO
C **                         16 POINT PATCHES.
C **        INPUT ARGUMENTS - IPGIN = PAGE NUMBER OF PANEL IN RANFILE
C **                        - IELIN = ELEMENT NUMBER OF PANEL IN RANFILE
C **                        - IPAT  = PATCH NUMBER IN PANEL
C **                        - PANEL = PANEL HEADER WORD
C **        OUTPUT ARGUMENT - PAT   = EXPANDED PATCH (26 REAL*8'S) 
C **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE PATEXP (IPNKEY,IPAT,PANEL,PAT)
 
      INCLUDE 'com4a.com'
 
      REAL*8 PANEL,PAT(26)
      INTEGER*4 IPNKEY
      INTEGER*2 IPAT

      REAL*8 PANHED,PAT1(26),PAT2(14)
      REAL*4 APAT1(52),APAT2(28)
      INTEGER*2 IPNHED(4),IPNTYP,NPATS
      EQUIVALENCE (PANHED,IPNHED,IPNTYP),(IPNHED(2),NPATS)
      EQUIVALENCE (PAT1,APAT1),(PAT2,APAT2)

      PANHED=PANEL
      NSZ=14
      IF (IPNTYP.EQ.1) NSZ=8
C                                         GET PATCH INTO ARRAY PAT1
      CALL GTPPTT(IPNKEY,IPAT,PAT1,NSZ)
C                                         GET NEXT PATCH INTO ARRAY PAT2
      CALL GTPPTT(IPNKEY,IPAT+1,PAT2,NSZ)

      RHO=APAT1(28)
      IF (IPNTYP.EQ.0) GOTO 100
C                                  IF RULED SURFACE, FAKE INTO FULL SF
      RHO=APAT1(16)
      DO 50 I=1,3
      APAT1(I+24)=APAT1(I+12)
      APAT1(I+15)=APAT1(I+9)
      APAT1(I+12)=APAT1(I+6)
      APAT1(I+6) =APAT1(I+12)/3.
      APAT1(I+9) =APAT1(I+6)*2.
      TMP=(APAT1(I+24)-APAT1(I+15))/3.
      APAT1(I+18)=TMP+APAT1(I+15)
      APAT1(I+21)=TMP*2.+APAT1(I+15)
      APAT2(I+24)=APAT2(I+12)
      APAT2(I+15)=APAT2(I+9)
      APAT2(I+12)=APAT2(I+6)
      APAT2(I+6) =APAT2(I+12)/3.
      APAT2(I+9) =APAT2(I+6)*2.
      TMP=(APAT2(I+24)-APAT2(I+15))/3.
      APAT2(I+18)=TMP+APAT2(I+15)
      APAT2(I+21)=TMP*2.+APAT2(I+15)
50    CONTINUE
C...
C      WRITE(19,9020)
C9020  FORMAT(' PAT1')
C      WRITE(19,9010) PAT1(1),PAT1(2),PAT1(3),(APAT1(I),I=7,27)
C      WRITE(19,9030)
C9030  FORMAT(' PAT2')
C      WRITE(19,9010) PAT2(1),PAT2(2),PAT2(3),(APAT2(I),I=7,27)
C...
     
100   CONTINUE
      DX=PAT2(1)-PAT1(1)
      DY=PAT2(2)-PAT1(2)
      DZ=PAT2(3)-PAT1(3)
      APAT2(4)=0.
      APAT2(5)=0.
      APAT2(6)=0.
      DO 110 I=1,10,3
      APAT1(I+39)=DX+APAT2(I+3)
      APAT1(I+40)=DY+APAT2(I+4)
      APAT1(I+41)=DZ+APAT2(I+5)
      APAT1(I+27)=APAT1(I+39)-RHO*(APAT2(I+15)-APAT2(I+3))
      APAT1(I+28)=APAT1(I+40)-RHO*(APAT2(I+16)-APAT2(I+4))
      APAT1(I+29)=APAT1(I+41)-RHO*(APAT2(I+17)-APAT2(I+5))
110   CONTINUE

      DO 120 I=1,26
120   PAT(I)=PAT1(I)

C...
C      WRITE(19,9040)
C9040  FORMAT(' PATOUT')
C      WRITE(19,9010) PAT1(1),PAT1(2),PAT1(3),(APAT1(I),I=7,51)
C9010  FORMAT(3F14.4)
C...
      RETURN
      END
c
c **********************************************************************
c **********************************************************************
c **  subroutine name: nsfevl
c **
c **  last revision:   9/3/92 , 16:21:14
c **  purpose of subroutine: this routine evaluates a ncl surface at u & v
c **      returns point, normal, slope in u direc, slope in v direc
c **
c **********************************************************************
c **********************************************************************
 
      subroutine nsfevl (asw, sfu, sfv, pvs)

      include 'com4a.com'

      parameter (maxpt=50)
      parameter (maxpn=20)
      parameter (mxpnhd=(2+(maxpt+1)/2))
      parameter (mxsfhd=(2+(maxpn+1)/2))

      real*8 asw,pvs(12)
      real*4 sfu,sfv

      real*8 sfhead(mxsfhd)
      real*4 asfhed(mxsfhd*2)
      integer*2 isfhed(mxsfhd*4)
      equivalence (sfhead,asfhed,isfhed)

      real*8 panhed(mxpnhd)
      real*4 apnhed(mxpnhd*2)
      integer*2 ipanhd(mxpnhd*4)
      equivalence (panhed,apnhed,ipanhd)

      real*8 p1(30)
      real*4 ap1(60)
      equivalence (p1,ap1)
      integer*4 nclkey, ipnkey
      integer*2 nwds, ietype
      call gtgeom (asw, sfhead, nclkey, nwds, ietype)
      npans=isfhed(2)
      fnpans=npans
      do 10 ipan=1,npans
        fpan=ipan
        if (sfv.le.fpan/fnpans) goto 20
10    continue
      ipan=npans
20    v=(sfv-(ipan-1)/fnpans)*fnpans
      call gtspa1(nclkey,ipan,panhed,ipnkey)
      npats=ipanhd(2)
      apnhed(npats+5)=1.
      do 30 ipat=1,npats
        if (apnhed(ipat+4).gt.sfu) goto 40
30    continue
40    ipat=ipat-1
      u=(sfu-apnhed(ipat+4))/(apnhed(ipat+5)-apnhed(ipat+4))
      if (u.lt.0.)u=0.
      if (u.gt.1.)u=1.
      if (v.lt.0.)v=0.
      if (v.gt.1.)v=1.

      call patexp(ipnkey,ipat,panhed,p1(3))
      p1(1)=p1(3)
      p1(2)=p1(4)
      p1(3)=p1(5)
      ap1(8)=0.
      ap1(9)=0.
      ap1(10)=0.

      call sfevl2(sfhead(2),p1,ap1,u,v,pvs)
      
99999 return
      end
c
c **********************************************************************
c **********************************************************************
c **  subroutine name: mshevl
c **
c **  last revision:   9/3/92 , 16:20:44
c **  purpose of subroutine: this routine evaluates a ncl surface at u & v
c **      returns point, normal, slope in u direc, slope in v direc
c **
c **********************************************************************
c **********************************************************************
 
      subroutine mshevl (asw, sfu, sfv, pvs)

      include 'com4a.com'

      real*8 asw,pvs(12)
      real*4 sfu,sfv

      real*8 sfhead(10)
      real*4 asfhed(4)
      integer*2 isfhed(8)
      equivalence (sfhead,asfhed,isfhed)

      real*8 p1(30)
      real*4 ap1(60)
      equivalence (p1,ap1)
      integer*4 nclkey
      integer*2 nwds, ietype

      call gtgeom (asw, sfhead, nclkey, nwds, ietype)
      mmax=isfhed(3)
      nmax=isfhed(4)
      fmmax=mmax
      fnmax=nmax
      do 10 m=1,mmax      
        fm=m
        if (sfu.le.fm/fmmax) goto 20
10    continue
      m=mmax
20    u=(sfu-(m-1)/fmmax)*fmmax
      do 30 n=1,nmax      
        fn=n
        if (sfv.le.fn/fnmax) goto 40
30    continue
      n=nmax
40    v=(sfv-(n-1)/fnmax)*fnmax
      if (u.lt.0.)u=0.
      if (u.gt.1.)u=1.
      if (v.lt.0.)v=0.
      if (v.gt.1.)v=1.
      ipat=(n-1)*mmax+m
      call gtmptt(asw,ipat,p1(3))

      p1(1)=p1(3)
      p1(2)=p1(4)
      p1(3)=p1(5)
      ap1(8)=0.
      ap1(9)=0.
      ap1(10)=0.

      call sfevl2(sfhead(2),p1,ap1,u,v,pvs)
      
99999 return
      end
