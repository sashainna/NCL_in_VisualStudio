C*********************************************************************
C*    NAME         :  crvgen.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       crvgen.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:45
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crvgen(npts, px, vs, asw)
C*        THIS ROUTINE FITS A CURVE THRU AN ARRAY OF POINTS
C*    PARAMETERS   
C*       INPUT  : 
C*          NPTS    - NUMBER OF POINTS IN ARRAY
C*          PX      - ARRAY OF POINTS
C*          VS      - Slope at start and end of curve
C*        OUTPUT -
C*          ASW     - ASSOCIATED WORD OF CURVE CREATED
C*            common array w contains curve.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      SUBROUTINE CRVGEN (NPTS, PX, VS, ASW)
 
      include 'com4a.com'

      INTEGER*2 NPTS
      REAL*8 PX(600), VS(6), ASW

      PARAMETER (MAXPT = 50)
      PARAMETER (MAXPTx = 1000)

      COMMON/PBLOK/X,Y,Z,A,B,C,DX,DY,DZ,CH,Q,INVX

      REAL*8 P(400)
      EQUIVALENCE (X,P)
      REAL*4 A(MAXPTx),B(MAXPTx),C(MAXPTx),DX(MAXPTx),DY(MAXPTx)
      real*4 DZ(MAXPTx)
      REAL*4 CH(MAXPTx)
      REAL*8 X(MAXPTx),Y(MAXPTx),Z(MAXPTx)
      REAL*4 Q(240)
      INTEGER*2 INVX(20)

      COMMON/WBLOK/W(600)
 
      REAL*8 W
      REAL*4 AW(1200)
      EQUIVALENCE (W,AW)

      REAL*8 ASN
      INTEGER*2 INV(MAXPTx),KSN(4)
      EQUIVALENCE (ASN,KSN)
      INTEGER*2 NP,I,J,IWX,ISEG
      integer*4 ipx
      LOGICAL NORED

C          START LOADING OF TEMPORARY POINTS 10 RECORDS PAST THE CURRENT
C          END OF THE ACTIVE RANFIL RECORD
      IF (NPTS .LT. 2) THEN
        IFL(2) = 378
        GOTO 999
      ENDIF
      NP   = NPTS
c
c...vp 15-sep-97 replaced ranfile by uu_list support.
c...This is required since crvfit works now with the list.
c...List must be free before this insertions, 
c...sc(11-22) is not used anymore.
c
      call ncl_put_uv (vs,2)
      call ncl_put_uv (px,np)
      ipx   = np + 2
      ksn(1) = 0
      ksn(3) = 3
      ksn(4) = 3
c
c...PUT THE ASW POINTERS TO THE POINTS IN A TEMPORARY ARRAY
c
      do 120 i=2,np+1
        ksn(2) = i + 1  
        p(I) = asn
120   continue
c
c...set up asw for vectors associated with the 1-st 
c...and last point
c
      p(1) = p(2)
      ksn(2) = 1
      ksn(4) = 4
      p(2)   = asn
      ksn(2) = 2
      p(ipx) = asn
      isc10(3) = ipx 
      call ncl_put_asn (p,ipx)
c
200   CONTINUE
      ranpts = .true.
      CALL CRVFIT (INV,NP)
      ranpts = .false.
      IF (IFL(2).GT.0) GOTO 999
      CALL CRVPRE(INV,NP,0)
      IF (IFL(2).GT.0) GOTO 999
 
C        BUILD CANON FORM IN W-TBL, CALL PUTENT
C             ADD S-VALUES FIRST
      DO 220 I=1,NP
220   AW(I)=DX(I)
C             ADD SEGS 1 BY 1 STARTING AT IWX = (NPTS+1)/2
      IWX=(NP+1)/2
      DO 230 ISEG=1,NP
          I=6*(ISEG-1)+IWX
          W(I+1)=X(ISEG)
          W(I+2)=Y(ISEG)
          W(I+3)=Z(ISEG)
          J=2*I+6
          AW(J+1)=A(ISEG)
          AW(J+2)=B(ISEG)
          AW(J+3)=C(ISEG)
          AW(J+4)=DY(ISEG)
          AW(J+5)=DZ(ISEG)
230       AW(J+6)=CH(ISEG)
 
C             CALL PUTENT TO STORE THE CANONICAL FORM IN THE RANFIL
      KSN(3)=IWX+6*NP
      KSN(4)=8
      NORED=.FALSE.
      CALL PUTENT (W(1),KSN(3),KSN(1),KSN(2),NORED,KSN(4))
      ASW=ASN

999   call ncl_free_uv 
      RETURN
      END
