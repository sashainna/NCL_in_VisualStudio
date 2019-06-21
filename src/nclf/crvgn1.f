C*********************************************************************
C*    NAME         :  crvgn1.f
C*       CONTAINS:
C*    COPYRIGHT 1999 (c) NCCS.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
**       crvgn1.f , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:45
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crvgn1(npts, px, vs, asw)
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
C*********************************************************************
C
      SUBROUTINE CRVGN1 (NPTS, PX, vs, ASW)
 
      include 'com4a.com'

      integer*2 maxpt,maxpts,maxptx,npts
      PARAMETER (MAXPT = 50)
      parameter (maxpts=600)
      PARAMETER (MAXPTx = 1000)

      REAL*8 PX(maxpts*3), VS(maxpts*3), ASW

      COMMON/PBLOK/X,Y,Z,A,B,C,DX,DY,DZ,CH,Q,INVX

      REAL*8 P(2*maxpts)

      REAL*8 X(MAXPTx),Y(MAXPTx),Z(MAXPTx)
      REAL*4 A(MAXPTx),B(MAXPTx),C(MAXPTx)
      real*4 DX(MAXPTx),DY(MAXPTx),DZ(MAXPTx)
      REAL*4 CH(MAXPTx),q(maxpt*36)
      INTEGER*2 INVX(maxptx)

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
      real*8 dsq,dsq0,dtolsq
      real*8 dv(3) 

C          START LOADING OF TEMPORARY POINTS 10 RECORDS PAST THE CURRENT
C          END OF THE ACTIVE RANFIL RECORD
      IF (NPTS .LT. 2) THEN
        IFL(2) = 163
        GOTO 999
      ENDIF
      NP = NPTS

      dtolsq = sc(27)*sc(27)

      call ncl_put_uv (px,np)
      ipx   = np
      ksn(1) = 0
      ksn(3) = 3
c
c...PUT THE ASW POINTERS TO THE POINTS IN A TEMPORARY ARRAY
c
      j = 1
      do 120 i=1,np
        ksn(4) = 3
        ksn(2) = i
        p(j) = asn
        j = j + 1

        if (i .lt. np) then
          dv(1) = px(3*i+1) - px(3*i-2)
          dv(2) = px(3*i+2) - px(3*i-1)
          dv(3) = px(3*i+3) - px(3*i)
          dsq0 = dv(1)*dv(1)+dv(2)*dv(2) + dv(3)*dv(3)
        endif

        if (dsq0 .ge. dtolsq) then
          dv(1) = vs(3*i-2)
          dv(2) = vs(3*i-1)
          dv(3) = vs(3*i)
          dsq = dv(1)*dv(1)+dv(2)*dv(2) + dv(3)*dv(3)
          if (dsq .ge. dtolsq) then
            call ncl_put_uv (dv,1)
            ipx = ipx + 1
            ksn(4) = 4
            ksn(2) = ipx
            p(j) = asn
            j = j + 1
          endif
        endif

120   continue

      isc10(3) = ipx 
      call ncl_put_asn (p,ipx)
 
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
