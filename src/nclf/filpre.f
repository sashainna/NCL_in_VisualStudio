C*********************************************************************
C*    NAME         :  filpre.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       filpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:03
C*********************************************************************
C
C **********************************************************************
C **  SUBROUTINE NAME: FILPRE
C **
C **  LAST REVISION:
C **  PURPOSE OF SUBROUTINE: THIS ROUTINE HANDLES VARIABLE RADIUS FILLET
C **      SURFACES BETWEEN 2 SURFACES OR A PLANE AND A SURFACE.
C **  INPUT -
C **    NONE
C **  OUTPUT -
C **    NONE
C **
C **********************************************************************
C **********************************************************************
 
      subroutine filpre

      INCLUDE 'com4a.com'
      include 'mocom.com'

C          MOTION COMMON EQUIVALENCES
      REAL*4 SRF(10,3)
      EQUIVALENCE (SRF,S)

      integer*2 maxpt, maxwd
      PARAMETER (MAXPT=50)
      PARAMETER (MAXWD=(MAXPT*6+(MAXPT+1)/2))
      COMMON/DSPCOM/WS(20),W(MAXWD)
      REAL*8 W,WS
      REAL*4 AW(300),AWS(40)
      EQUIVALENCE (WS,AWS),(W,AW)

      integer*2 mxflpt
      PARAMETER (MXFLPT=200)
      REAL*8 BUF(MXFLPT*3+3),PS1(MXFLPT*3),PS2(MXFLPT*3),PS3(MXFLPT*3)
      REAL*8 PS4(MXFLPT*3)
      REAL*8 NPT(3),P1(3),P2(3),P3(3),PL1(4),PL2(4),ASN,VT(3),CPL(4)
      REAL*8 VSV1(3),VSV2(3),CVASW(4),TOL,DTOL,ddtol,VS(6)
      REAL*8 ASWSF1,ASWSF2,ASWNPT,ASWRCV,ASWLM1,ASWLM2,u8
      REAL*4 SS1(7),SS2(7),DIR1,DIR2
      INTEGER*2 KSN(4),KSWRCV(4)
      EQUIVALENCE(ASN,KSN),(ASWRCV,KSWRCV)
      REAL*4 U1,V1,U2,V2,R1,R2,U,DU,VX,VY,VZ,SEC,DIS,TDIS,THK,SI,CO,COSQ
      REAL*4 P,Q,A,B,D1,SCLRS
      INTEGER*2 I,J,IX,IFL4x,IFL7,IFL8,NPTS,NP,NP1,NP2,NP3,NP4,ITIM
      integer*4 nclkey, pntyp
      integer*2 nwds, ietype, isf, irs,itsk
      real*4 uv(4)
      equivalence(sc(19),uv)

      logical lv91
      integer*2 prtyp,icoor
      real*8 prdat(16)
      common/primbl/prdat,prtyp,icoor


      lv91 = sc(169).lt.9.149d0
      isf = 3
      irs = 0
      u1=uv(1)
      v1=uv(2)
      u2=uv(3)
      v2=uv(4)
      IFL4x=IFL(4)
      IFL7=IFL(7)
      IFL8=IFL(8)
      TOL=SC(27)
      DTOL=TOL*2.d0
      if (sc(169).lt.8.2999) DTOL=TOL*10.d0
      ddtol=tol*10.
      CTOL=TOL*20.
      DIR1=0.
      DIR2=0.
      ASWSF1=SC(11)
      ASWSF2=SC(12)
      ASWNPT=SC(13)
      ASWRCV=SC(14) 
      R1=SC(15)
      R2=SC(16)
      ASWLM1=SC(17)
      ASWLM2=SC(18)
c
c..... determine apriori if this is a cylinder
c
      prtyp = 0
c
c...For consistency
c...Use the surface analyze routine
c...to determine if this is a valid cylinder
c...Bobby - 3/6/03
c
cc      if (.not.lv91 .and. ASWRCV.eq.0.) then
cc        asn = sc(10)
cc        if (ksn(3).eq.0 .or. abs(r1-r2).le.0.1*sc(27)) then
cc          call gtdesc(ASWSF1,nclkey,nwds,ietype)
cc          if (ietype .eq. 9) call ncl_get_sf_primtyp(nclkey,prtyp)
cc          if (ietype.eq.6 .or. prtyp.eq.3) then
cc            call gtdesc(ASWSF2,nclkey,nwds,ietype)
cc            if (ietype .eq. 9) call ncl_get_sf_primtyp(nclkey,prtyp)
cc            if (ietype.eq.6 .or. prtyp.eq.3) then
cc              prtyp = 5
cc              icoor = 1
cc            endif
cc          endif
cc        endif
cc      endif
cc      if (prtyp .ne. 5) prtyp = 0

C       IF REFSYS, USE X-AXIS SCALE FACTOR FOR RADII (WON'T WORK IF 
C       SCALE IS DIFFERENT ALONG DIFFERENT AXES)
      IF (IFL(72).EQ.1) SCLRS=DSQRT(SC(56)**2+SC(57)**2+SC(58)**2)
C                    GET NEAR POINT
      ASN=ASWNPT
C      CALL GETENT(NPT,KSN(3),KSN(1),KSN(2),0)
      call gtgeom (asn, npt, nclkey, nwds, ietype)

      ASN=ASWLM1
      IF (KSN(4).EQ.8) GOTO 10
      IF (KSN(4).EQ.3) GOTO 20
      IF (KSN(4).EQ.0) GOTO 30
      IF (KSN(4).EQ.20) GOTO 40
      GOTO 990
C                    CURVE LIMIT ENTITY
10    CONTINUE
      npts = sc(12)
      call crvlen (asn, npts, tv, ifl(2))
      NPTS=TV/TOL/50.
      IF (NPTS.GT.MXFLPT)NPTS=MXFLPT
      IF (NPTS.LT.3)NPTS=3
      call gtdesc (asn, nclkey, nwds, ietype)
      call isitwf (nclkey, iwf)
      if (iwf.eq.1) then
        call evstup (nclkey, isf)
      else
        call gtgeom (asn, w, nclkey, nwds, ietype)
      endif
      WS(1)=0.
      AWS(39)=0.
      AWS(40)=AW(1)
      AW(1)=0.
      ISC10(2)=8
      U=0.
      DU=NPTS-1
      DU=1./DU
      DO 12 I=1,NPTS
      if (iwf.eq.1) then
        u8 = u
        call uevcvt (u8, isf, irs, buf(i*3-2), vt, ifl(2))
      else
        call dcrvpt(u,ietype,buf(i*3-2),vt)
      endif
      U=U+DU
12    CONTINUE
      VT(1)=BUF(4)-BUF(1)
      VT(2)=BUF(5)-BUF(2)
      VT(3)=BUF(6)-BUF(3)
      CALL PTSMTH(BUF,NPTS,VT,DTOL)
      GOTO 50
C                    POINT LIMIT ENTITIES
20    continue
c      CALL GETENT(P1,KSN(3),KSN(1),KSN(2),0)
      call gtgeom (asn, p1, nclkey, nwds, ietype)
      ASN=ASWLM2
c      CALL GETENT(P2,KSN(3),KSN(1),KSN(2),0)
      call gtgeom (asn, p2, nclkey, nwds, ietype)
      IF ((P1(1)-P2(1))**2+(P1(2)-P2(2))**2+(P1(3)-P2(3))**2.LT..00004)
     1   GOTO 980
C                  PROJECT NEAR POINT ONTO SURFACES
      CALL SFPT(ASWSF1,NPT,2,DIR1,U1,V1,SS1)
      IF (IFL(2).GT.0) GOTO 980
      CALL SFPT(ASWSF2,NPT,3,DIR2,U2,V2,SS2)
      IF (IFL(2).GT.0) GOTO 980
      DO 22 I=1,3
      VSV1(I)=SS1(I)
22    VSV2(I)=SS2(I)
C                  PROJECT 1ST POINT ONTO SURFACES
      CALL SFPT(ASWSF1,P1,2,DIR1,U1,V1,SS1)
      IF (IFL(2).GT.0) GOTO 980
      CALL SFPT(ASWSF2,P1,3,DIR2,U2,V2,SS2)
      IF (IFL(2).GT.0) GOTO 980
C                  CALC PLANES THRU LIMIT POINTS
      VX=SS1(2)*SS2(3)-SS1(3)*SS2(2)
      VY=SS1(3)*SS2(1)-SS1(1)*SS2(3)
      VZ=SS1(1)*SS2(2)-SS1(2)*SS2(1)
      SEC=SQRT(VX**2+VY**2+VZ**2)
      IF (SEC.LT..001) GOTO 980
      PL1(1)=VX/SEC
      PL1(2)=VY/SEC
      PL1(3)=VZ/SEC
      PL1(4)=P1(1)*PL1(1)+P1(2)*PL1(2)+P1(3)*PL1(3)

      CALL SFPT(ASWSF1,P2,2,DIR1,U1,V1,SS1)
      IF (IFL(2).GT.0) GOTO 980
      CALL SFPT(ASWSF2,P3,3,DIR2,U2,V2,SS2)
      IF (IFL(2).GT.0) GOTO 980
      VX=SS1(2)*SS2(3)-SS1(3)*SS2(2)
      VY=SS1(3)*SS2(1)-SS1(1)*SS2(3)
      VZ=SS1(1)*SS2(2)-SS1(2)*SS2(1)
      SEC=SQRT(VX**2+VY**2+VZ**2)
      IF (SEC.LT..001) GOTO 980
      PL2(1)=VX/SEC
      PL2(2)=VY/SEC
      PL2(3)=VZ/SEC
      PL2(4)=P2(1)*PL2(1)+P2(2)*PL2(2)+P2(3)*PL2(3)
C                DRIVE TO PLANE 1

C                DRIVE TO PLANE 2 SAVING PTS, TOT DIS
      GOTO 60
C                        NO LIMIT ENTITY - USE CV INSECTION
30    CONTINUE
      ISC10(3)=1
      SC(11)=ASWSF1
      SC(12)=ASWSF2
      SC(13)=ASWNPT
        if (lv91) then
          call cvintx(1)
        else
          itsk = -1
          call cvio(itsk)
        endif
      NPTS=ISC10(3)
      IF (NPTS.GT.MXFLPT)NPTS=MXFLPT
      NP=NPTS
c
c...vp 15-sep-97 replaced ranfile by uu_list support since civntx
c...call creates points in the list.  Trans thru refsys is not
c...required here so call direct ncl_get_ent (instead getent_list)
c...Free list after retrieving points to use by crvgen.
c...In case of problems see old version in sccs!
c
      if (lv91) then
        call ncl_get_asn (isc10(3),1,ps1(1),npts)
        do 38 i=1,npts
           asn = ps1(i)
           call ncl_get_ent (ksn(2),buf(i*3-2))
38      continue
      else
        do 39 i=1,npts
           call ncl_get_ent (i,buf(i*3-2))
39      continue
      endif

      call ncl_free_uv
c
      vt(1)=buf(4)-buf(1)
      vt(2)=buf(5)-buf(2)
      vt(3)=buf(6)-buf(3)
      call ptsmth(buf,npts,vt,ddtol)
c
      GOTO 50
C                        PATERN LIMIT ENTITY
40    CONTINUE
      call gtdesc (asn, nclkey, nwds, ietype)
      CALL GTPNNP (nclkey, NPTS, pntyp)
      IF (NPTS.GT.MXFLPT)NPTS=MXFLPT
      DO 41 I=1,NPTS
41      CALL GTPNPT(BUF(I*3-2),pntyp,nclkey,I)
      VT(1)=BUF(4)-BUF(1)
      VT(2)=BUF(5)-BUF(2)
      VT(3)=BUF(6)-BUF(3)
      CALL PTSMTH(BUF,NPTS,VT,DTOL)
      GOTO 50
50    CONTINUE
C                  PROJECT NEAR POINT ONTO SURFACES
      CALL SFPT(ASWSF1,NPT,2,DIR1,U1,V1,SS1)
      IF (IFL(2).GT.0) GOTO 980
      CALL SFPT(ASWSF2,NPT,3,DIR2,U2,V2,SS2)
      IF (IFL(2).GT.0) GOTO 980
      DO 52 I=1,3
      VSV1(I)=SS1(I)
52    VSV2(I)=SS2(I)

60    DIS=0.
      TDIS=0.
      DO 62 IX=1,NPTS*3-3,3
62    TDIS=TDIS+DSQRT((BUF(IX+3)-BUF(IX))**2+(BUF(IX+4)-BUF(IX+1))**2
     X             +(BUF(IX+5)-BUF(IX+2))**2)
      IF (KSWRCV(4).EQ.8) THEN
        call gtdesc (aswrcv, nclkey, nwds, ietype)
        call isitwf (nclkey, iwf)
        if (iwf.eq.1) then
          call evstup (nclkey, isf)
        else
          call gtgeom (aswrcv, w, nclkey, nwds, ietype)
        endif
        WS(1)=0.
        AWS(39)=0.
        AWS(40)=AW(1)
        AW(1)=0.
        ISC10(2)=8
      ENDIF
C                STARTING AT PT1,CALC CHKPL, CALC THICK, 
C                PROJECT PT ONTO SFS & CHKPL,SAVE SF PTS, CALC CONTROL PT
      IX=0
      DO 100 I=1,NPTS
      ITIM=0
      IF (KSWRCV(4).EQ.8) THEN
        U=DIS/TDIS
        if (iwf.eq.1) then
          u8 = u
          call evstup (nclkey, isf)
          call uevcvt (u8, isf, irs, p1, vt, ifl(2))
        else
          call dcrvpt (u,kswrcv(4),p1,vt)
        endif
        THK=DABS(P1(2))
        IF (THK.LT.DTOL)THK=DTOL
      ELSE
        THK=R1+(R2-R1)*DIS/TDIS
      ENDIF
      IF (IFL(72).EQ.1) THK=THK*SCLRS
C                  CALC CHECK PLANE
      IF (NPTS.GT.2) GOTO 64
      VX=BUF(4)-BUF(1)
      VY=BUF(5)-BUF(2)
      VZ=BUF(6)-BUF(3)
      GOTO 67
64    CONTINUE
      IVX=IX
      IF (I.EQ.1) IVX=IX+3
      IF (I.EQ.NPTS) IVX=IX-3
      VX1=BUF(IVX+1)-BUF(IVX-2)
      VY1=BUF(IVX+2)-BUF(IVX-1)
      VZ1=BUF(IVX+3)-BUF(IVX)
      SEC=SQRT(VX1**2+VY1**2+VZ1**2)
      IF (SEC.EQ.0.) GOTO 980
      VX1=VX1/SEC
      VY1=VY1/SEC
      VZ1=VZ1/SEC
      VX2=BUF(IVX+4)-BUF(IVX+1)
      VY2=BUF(IVX+5)-BUF(IVX+2)
      VZ2=BUF(IVX+6)-BUF(IVX+3)
      SEC=SQRT(VX2**2+VY2**2+VZ2**2)
      IF (SEC.EQ.0.) GOTO 980
      VX2=VX2/SEC
      VY2=VY2/SEC
      VZ2=VZ2/SEC
      CO=VX1*VX2+VY1*VY2+VZ1*VZ2
      IF (ABS(CO).GT..9999) GOTO 65
      XF=BUF(IVX+4)-BUF(IVX-2)
      YF=BUF(IVX+5)-BUF(IVX-1)
      ZF=BUF(IVX+6)-BUF(IVX)
      CHD=SQRT(XF**2+YF**2+ZF**2)
      IF (CHD.EQ.0.) GOTO 65
      XF=XF/CHD
      YF=YF/CHD
      ZF=ZF/CHD
      XU=VY1*VZ2-VZ1*VY2
      YU=VZ1*VX2-VX1*VZ2
      ZU=VX1*VY2-VY1*VX2
      SEC=SQRT(XU**2+YU**2+ZU**2)
      IF (SEC.LT.SC(27)/2.) GOTO 65
      XU=XU/SEC
      YU=YU/SEC
      ZU=ZU/SEC
      XR=YF*ZU-ZF*YU
      YR=ZF*XU-XF*ZU
      ZR=XF*YU-YF*XU
      SEC=SQRT(XR**2+YR**2+ZR**2)
      IF (SEC.EQ.0.) GOTO 65
      XR=XR/SEC
      YR=YR/SEC
      ZR=ZR/SEC
      HCHD=CHD/2.
      P1(1)=BUF(IVX-2)+XF*HCHD
      P1(2)=BUF(IVX-1)+YF*HCHD
      P1(3)=BUF(IVX)+ZF*HCHD
      SI=SQRT(1-CO**2)
      RAD=HCHD/SI
      H=SQRT(RAD**2-(HCHD)**2)
      XR=P1(1)-XR*H-BUF(IX+1)
      YR=P1(2)-YR*H-BUF(IX+2)
      ZR=P1(3)-ZR*H-BUF(IX+3)
      VX=YU*ZR-ZU*YR
      VY=ZU*XR-XU*ZR
      VZ=XU*YR-YU*XR
      GOTO 67
65    IF (I.EQ.NPTS) GOTO 66
      VX=VX1
      VY=VY1
      VZ=VZ1
      GOTO 67
66    VX=VX2
      VY=VY2
      VZ=VZ2
67    DO 68 J=1,3
      P1(J)=BUF(IX+J)
      P2(J)=P1(J)
68    P3(J)=P1(J)

      SEC=SQRT(VX**2+VY**2+VZ**2)
      IF (SEC.LT.SC(27)/2.) GOTO 980
      CPL(1)=VX/SEC
      CPL(2)=VY/SEC
      CPL(3)=VZ/SEC
      CPL(4)=P1(1)*CPL(1)+P1(2)*CPL(2)+P1(3)*CPL(3)
C                  PROJECT CURRENT POINT ONTO SURFACES
70    CALL NESTPT(ASWSF1,P1,P2,2,THK,DIR1,U1,V1,CPL,SS1)
      IF (IFL(2).GT.0) GOTO 980
      CALL NESTPT(ASWSF2,P1,P3,3,THK,DIR2,U2,V2,CPL,SS2)
      IF (IFL(2).GT.0) GOTO 980
      IF (VSV1(1)*SS1(1)+VSV1(2)*SS1(2)+VSV1(3)*SS1(3).LT.0.) THEN
        DO 71 J=1,4
71        SS1(J)=-SS1(J)
      ENDIF
      IF (VSV2(1)*SS2(1)+VSV2(2)*SS2(2)+VSV2(3)*SS2(3).LT.0.) THEN
        DO 72 J=1,4
72        SS2(J)=-SS2(J)
      ENDIF
      DO 73 J=1,3
      VSV1(J)=SS1(J)
73    VSV2(J)=SS2(J)

      VX=CPL(2)*SS1(3)-CPL(3)*SS1(2)
      VY=CPL(3)*SS1(1)-CPL(1)*SS1(3)
      VZ=CPL(1)*SS1(2)-CPL(2)*SS1(1)
      SS1(1)=VY*CPL(3)-VZ*CPL(2)
      SS1(2)=VZ*CPL(1)-VX*CPL(3)
      SS1(3)=VX*CPL(2)-VY*CPL(1)
      SEC=SQRT(SS1(1)**2+SS1(2)**2+SS1(3)**2)
      IF(SEC.EQ.0)GOTO 980
      SS1(1)=SS1(1)/SEC
      SS1(2)=SS1(2)/SEC
      SS1(3)=SS1(3)/SEC
      SS1(4)=SS1(1)*SS1(5)+SS1(2)*SS1(6)+SS1(3)*SS1(7)

      VX=CPL(2)*SS2(3)-CPL(3)*SS2(2)
      VY=CPL(3)*SS2(1)-CPL(1)*SS2(3)
      VZ=CPL(1)*SS2(2)-CPL(2)*SS2(1)
      SS2(1)=VY*CPL(3)-VZ*CPL(2)
      SS2(2)=VZ*CPL(1)-VX*CPL(3)
      SS2(3)=VX*CPL(2)-VY*CPL(1)
      SEC=SQRT(SS2(1)**2+SS2(2)**2+SS2(3)**2)
      IF(SEC.EQ.0)GOTO 980
      SS2(1)=SS2(1)/SEC
      SS2(2)=SS2(2)/SEC
      SS2(3)=SS2(3)/SEC
      SS2(4)=SS2(1)*SS2(5)+SS2(2)*SS2(6)+SS2(3)*SS2(7)

      SI=SS1(1)*SS2(1)+SS1(2)*SS2(2)+SS1(3)*SS2(3)
      P=SS1(1)*P1(1)+SS1(2)*P1(2)+SS1(3)*P1(3)-SS1(4)-THK
      Q=SS2(1)*P1(1)+SS2(2)*P1(2)+SS2(3)*P1(3)-SS2(4)-THK
      COSQ=1.-SI**2
      IF (COSQ.EQ.0.)GOTO 980
      A=(P-SI*Q)/COSQ
      B=Q-A*SI
      DO 76 J=1,3
76    P1(J)=P1(J)-A*SS1(J)-B*SS2(J)
C      D1=CPL(1)*P1(1)+CPL(2)*P1(2)+CPL(3)*P1(3)-CPL(4)
C      DO 78 J=1,3
C78    P1(J)=P1(J)-CPL(J)*D1
C      IF (ABS(A).LT.TOL.AND.ABS(B).LT.TOL.AND.ABS(D1).LT.TOL)GOTO 80
      IF (ABS(A).LT.TOL.AND.ABS(B).LT.TOL)GOTO 80
      ITIM=ITIM+1
      IF (ITIM.GT.30)GOTO 980
      GOTO 70

80    CONTINUE
      if (prtyp.eq.5 .and. i.eq.1) then
        prdat(1) = p1(1)
        prdat(2) = p1(2)
        prdat(3) = p1(3)
        prdat(4) = cpl(1)
        prdat(5) = cpl(2)
        prdat(6) = cpl(3)
        prdat(7) = r1
        prdat(8) = tdis
      endif
      VT(1)=SS1(1)+SS2(1)
      VT(2)=SS1(2)+SS2(2)
      VT(3)=SS1(3)+SS2(3)
      SEC=SQRT(VT(1)**2+VT(2)**2+VT(3)**2)
      IF (SEC.EQ.0.) GOTO 980
      VT(1)=VT(1)/SEC
      VT(2)=VT(2)/SEC
      VT(3)=VT(3)/SEC
      CO=VT(1)*SS1(1)+VT(2)*SS1(2)+VT(3)*SS1(3)
      IF (CO.EQ.0.) GOTO 980
      D1=THK/CO
      DO 90 J=1,3
      PS1(IX+J)=P1(J)-SS1(J)*THK
      PS2(IX+J)=P1(J)-SS2(J)*THK
      PS3(IX+J)=PS1(IX+J)+(PS1(IX+J)-P1(J)+VT(J)*D1)*CTOL
90    PS4(IX+J)=PS2(IX+J)+(PS2(IX+J)-P1(J)+VT(J)*D1)*CTOL

C  ---   DEBUG   ---
C      WRITE (19,9010) I, PS1(IX+1),PS1(IX+2),PS1(IX+3)
C9010  FORMAT (' PS1('I4')=PT/'2(F12.6,','),F12.6)
C      WRITE (19,9020) I, PS2(IX+1),PS2(IX+2),PS2(IX+3)
C9020  FORMAT (' PS2('I4')=PT/'2(F12.6,','),F12.6)
C      WRITE (19,9030) I, PS3(IX+1),PS3(IX+2),PS3(IX+3)
C9030  FORMAT (' PS3('I4')=PT/'2(F12.6,','),F12.6)
C  ---   DEBUG   ---
      IF (I.EQ.NPTS) GOTO 100
      IF (I.GT.1) GOTO 98
      DO 92 J=1,3
92    VS(J)=CPL(J)
98    DIS=DIS+DSQRT((BUF(IX+4)-BUF(IX+1))**2+(BUF(IX+5)-BUF(IX+2))**2
     X             +(BUF(IX+6)-BUF(IX+3))**2)
      IF (DIS.GT.TDIS)DIS=TDIS
      IX=IX+3
100   CONTINUE
      DO 104 I=1,3
104   VS(I+3)=CPL(I)
C                         SMOOTH POINTS
      VT(1)=BUF(4)-BUF(1)
      VT(2)=BUF(5)-BUF(2)
      VT(3)=BUF(6)-BUF(3)
      NP1=NPTS
      CALL PTSMTH(PS1,NP1,VT,DTOL)
      NP2=NPTS
      CALL PTSMTH(PS2,NP2,VT,DTOL)
      NP3=NPTS
      CALL PTSMTH(PS3,NP3,VT,DTOL)
      NP4=NPTS
      CALL PTSMTH(PS4,NP4,VT,DTOL)

C  ---   DEBUG   ---
C      DO 810 I=1,NP1
C810     WRITE (19,9040) I, PS1(I*3-2),PS1(I*3-1),PS1(I*3)
C9040  FORMAT (' PS1A('I4')=PT/'2(F12.6,','),F12.6)
C      DO 820 I=1,NP2
C820     WRITE (19,9050) I, PS2(I*3-2),PS2(I*3-1),PS2(I*3)
C9050  FORMAT (' PS2A('I4')=PT/'2(F12.6,','),F12.6)
C      DO 830 I=1,NP3
C830     WRITE (19,9060) I, PS3(I*3-2),PS3(I*3-1),PS3(I*3)
C9060  FORMAT (' PS3A('I4')=PT/'2(F12.6,','),F12.6)
C  ---   DEBUG   ---
C                          FIT CVS THRU PTS
      IFL(4)=IFL(4)+30
      IFL(7)=IFL(4)
      IFL(8)=0
      CALL CRVGEN(NP1,PS1,VS,CVASW(1))
      IF (IFL(2).GT.0) GOTO 990
      CALL CRVGEN(NP2,PS2,VS,CVASW(3))
      IF (IFL(2).GT.0) GOTO 990
      CALL CRVGEN(NP3,PS3,VS,CVASW(2))
      IF (IFL(2).GT.0) GOTO 990
      CALL CRVGEN(NP4,PS4,VS,CVASW(4))
      IF (IFL(2).GT.0) GOTO 990
      DO 120 I=1,4
120   SC(10+I)=CVASW(I)
      ISC10(1)=607
      ISC10(2)=4
      ISC10(3)=0
      ISC10(4)=0
      GOTO 999

980   IFL(2)=163
990   IF (IFL(2).LT.1)IFL(2)=5
      ERR=.TRUE.

999   IFL(4)=IFL4x
      IFL(7)=IFL7
      IFL(8)=IFL8
C  ---   DEBUG   ---
C      CLOSE (19)
C  ---   DEBUG   ---
      RETURN
      END
