c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       srfpn2.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:44
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1981,1982 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  SUBROUTINE NAME: SRFPN2
C **
C **  PURPOSE OF PROGRAM:                                             **
C          THIS ROUTINE IS CALLED WHEN A SRF P/N IS REQD.
C          ACTIVATE PATCH/PANEL AS NEC AND CALL IPATCH TO CALC
C          THE ACTUAL SRF DATA.
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE SRFPN2 (U,V,IRET)
 
      INCLUDE 'com4a.com'
      include 'mocom.com'
      include 'drvcom.com'
 
C          MOTION COMMON EQUIVALENCES
      real*4 srf(10,3)
      equivalence (srf,s)
 
      REAL*8 E(14)
      REAL*4 AD(300),AE(28)
      INTEGER*2 KD(600)
      EQUIVALENCE (D,AD,KD),(E,AE),(IFL(54),ISRF)
      INTEGER*2 NWDS,IETYPE
      INTEGER*4 ISFKEY 
C          SET PTRS TO SRF IN D-TBL
      IDX=50*(ISRF-1)
      JDX=2*IDX
      KDX=4*IDX
      JPX=2*MXPNHD*(ISRF-1) 
      ISTYP=KD(KDX+1)
C+++
7     NPANS=KD(KDX+2)
C             GET SURFACE UNIBASE KEY
      CALL GTDESC(SC(ISRF+143),ISFKEY,NWDS,IETYPE)
C          SAFETY COUNTER FOR IPATCH CALLS
      IPATK=0
8     NPATS=KD(KDX+38)
C
C          IF NO ACTIVE PATCH, GO SEARCH.
      IPAT=KD(KDX+41)
      IF(IPAT.EQ.0) GOTO 900
C          ALLOW SOME SMALL O/B BEFORE ACTIVATING A PATCH
      IF(U+.001.LT.AD(JDX+15).OR.U-.001.GT.AD(JDX+16))GOTO 900
      GOTO 50
C          SCAN S-LIST THIS PANEL FOR INDICATED PATCH
900   DO 910 I=2,NPATS
      IF(APNHED(JPX+I+4).GE.U)GOTO 920
910   CONTINUE
C          MUST BE LAST PAT
      I=NPATS+1
920   IPAT=I-1
C          SET U-MIN/MAX
925   AD(JDX+15)=APNHED(JPX+IPAT+4)
      AD(JDX+16)=APNHED(JPX+IPAT+5)
      IF(IPAT.EQ.NPATS) AD(JDX+16)=1.
C          IF SFU NOT IN THIS RANGE, PUT IT AT UMID
      IF(U.LT.AD(JDX+15).OR.U.GT.AD(JDX+16)) U=
     1   (AD(JDX+15)+AD(JDX+16))/2.
C          POINT TO ACTIV PANEL IN RANFIL
10    IPAN=KD(KDX+42)
C      ISFX=4*MXSFHD*(ISRF-1)+2*IPAN+7
C      IPG=KSFHED(ISFX)
C      IEL=KSFHED(ISFX+1)
      IF(ISTYP.EQ.9)GOTO 19
C*********************************************  RLDSRF
C          READ 8 WDS THIS PATCH INTO D-TBL
130   CONTINUE
C130   CALL GETENT(D(IDX+25),8,IPG,IEL,0)
      CALL GTSPTT(ISFKEY,IPAN,IPAT,D(IDX+25),8)
      RHO=AD(JDX+64)
C          READ 8 WDS NEXT PATCH INTO E
C140   CALL GETENT(E,8,IPG,IEL,0)
      CALL GTSPTT(ISFKEY,IPAN,IPAT+1,E,8)
      DX=E(1)-D(IDX+25)
      DY=E(2)-D(IDX+26)
      DZ=E(3)-D(IDX+27)
C          P7,P8 FROM NEXT PAT
      AD(JDX+70)=DX
      AD(JDX+71)=DY
      AD(JDX+72)=DZ
      AD(JDX+73)=DX+AE(7)
      AD(JDX+74)=DY+AE(8)
      AD(JDX+75)=DZ+AE(9)
C          ZER0 E(2,3) AND EXTRAP BACK FOR PTS 5,6
      E(2)=0.
      E(3)=0.
      DO 145 I=64,69
      J=I+6
      K=I-54
      L=K-6
145   AD(JDX+I)=AD(JDX+J)-RHO*(AE(K)-AE(L))
      GOTO 46
C*******************************************  FULL SRF
C          MOVE POINTER TO IPAT IN RANFIL
19    CONTINUE
C          READ 14 WDS THIS PATCH INTO D-TBL
C30    CALL GETENT(D(IDX+25),14,IPG,IEL,0)
      CALL GTSPTT(ISFKEY,IPAN,IPAT,D(IDX+25),14)
      RHO=AD(JDX+76)
C          READ 14 WDS NEXT PATCH INTO E
      CALL GTSPTT(ISFKEY,IPAN,IPAT+1,E,14)
      DX=E(1)-D(IDX+25)
      DY=E(2)-D(IDX+26)
      DZ=E(3)-D(IDX+27)
C          NOTE.  IMPROVE THE FOLLOWING LATER. <<<<<<<<<<
      AD(JDX+88)=DX
      AD(JDX+89)=DY
      AD(JDX+90)=DZ
      AD(JDX+91)=DX+AE(7)
      AD(JDX+92)=DY+AE(8)
      AD(JDX+93)=DZ+AE(9)
      AD(JDX+94)=DX+AE(10)
      AD(JDX+95)=DY+AE(11)
      AD(JDX+96)=DZ+AE(12)
      AD(JDX+97)=DX+AE(13)
      AD(JDX+98)=DY+AE(14)
      AD(JDX+99)=DZ+AE(15)
C          ZER0 E(2,3) AND EXTRAP BACK FOR PTS 9-12
      E(2)=0.
      E(3)=0.
      DO 45 I=76,87
      J=I+12
      K=I-60
      L=K-12
45    AD(JDX+I)=AD(JDX+J)-RHO*(AE(K)-AE(L))
C          UPSHIFT ORIGPT 2 LOCS AND ZERO PT1  ....!@#$%^&*?@#$/^%!!
46    D(IDX+23)=D(IDX+25)
      D(IDX+24)=D(IDX+26)
      D(IDX+25)=D(IDX+27)
      AD(JDX+52)=0.
      AD(JDX+53)=0.
      AD(JDX+54)=0.
C          RECORD ACTIVE PAT AND BEGIN AT MIDPT.
      KD(KDX+41)=IPAT
C          U,V=.5 INIT
      U=.5
      V=.5
C****************************************************
C
50    CONTINUE
C          U,V ARE INPUT
C          CPT IS TE, FOR NOW.  LATER SET BY CALLING SUBR.
CC    SRF(8,ISRF)=      )
CC    SRF(9,ISRF)=      )  SET IN CALLING SUBR.
CC    SRF(10,ISRF)=     )
C***************************************
      IPATK=IPATK+1
      IF(IPATK.LT.31) GOTO 55
C          ERROR.  AFTER 30 IPATCH CALLS, GET OUT.
      IFL(2)=138
      GOTO 99
55    CALL IPTCH2(U,V,IRET)
C***************************************
      IF(IRET.EQ.0.OR.IRET.EQ.3)GOTO 99
      IF(IABS(IRET).EQ.1)GOTO 80
C          MUST BE V-BDY VIOLATION  ( IRET= +/- 2 )
C          IF ANOTHER PANEL THERE, ACTIVATE IT
      IPAN=KD(KDX+42)
      IPAN=IPAN+IRET/2
      IF(IPAN.LT.1.OR.IPAN.GT.NPANS)GOTO 89
C          GET PANEL INTO PANHED
      KPX=KDX+2*IPAN+7
C      CALL GETENT(D(IDX+10),12,KD(KPX),KD(KPX+1),0)
      CALL GTSPAN(ISFKEY,IPAN,PANHED(MXPNHD*(ISRF-1)+1))
      D(IDX+10)=PANHED(MXPNHD*(ISRF-1)+1)
      KD(KDX+42)=IPAN
      FNPANS=KD(KDX+2)
C          SET V MIN/MAX
      FIPAN=IPAN
      AD(JDX+18)=FIPAN/FNPANS
      AD(JDX+17)=AD(JDX+18)-1./FNPANS
C          IF V O/B, SET IT HALFWAY
      IF(V.LT.AD(JDX+17).OR.V.GT.AD(JDX+18))
     1  V=(AD(JDX+17)+AD(JDX+18))/2.
C          FORCE A NEW PAT SCH
      KD(KDX+41)=0
      GOTO 8
C          U-BDY VIOLATION. TRY UP OR DOWN PATCH (IF ANY)
80    IPAT=IPAT+IRET
      IF(IPAT.LT.1.OR.IPAT.GT.NPATS)GOTO 89
      KD(KDX+41)=IPAT
C          GO UPDAT U MIN/MAX
      GOTO 925
C          UNFIXABLE BDY VIOLATION.  SET WARNING FLAG.
C89    IFL(2)=-127
89    CONTINUE
99    RETURN
      END
