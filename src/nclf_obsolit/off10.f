C*************************************************************************
C**    NAME           : off10.f
C**       CONTAINS:
C**  1. SUBROUTINE OFF1ST (KLOSED,ISIDE,ISLE,R,NPPA,XA,YA,XB,YB,NPPB,IER)
C**  2. SUBROUTINE XCORN (UXL,UYL,X,Y,XN,YN ,KARC1,KARC2,KARC3
C**    x          ,SSQL,ISIDE,R,XOUT1,YOUT1,XOUT2,YOUT2 ,NOPTS,UX,UY,SSQ)
C**  3. SUBROUTINE BOXES (NPPB,XB,YB,XL1,YL1,XL2,YL2,LLT,NBOX)
C**  4. SUBROUTINE DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO
C**    x          ,XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC
C**    x          ,NPPC,IERR)
C**  5. SUBROUTINE SEEK (NOSEGIN,NOBOXIN,NOBOXSS,KARC1,XC1,YC1,R1,KLOK1
C**    x          ,X1,Y1,X2,Y2,XB,YB,MM1,MM2,XMM,YMM,LLT,XL1,YL1,XL2,YL2
C**    x          ,KLOSED,NOELX,NOBOXX,XINT,YINT,ILOOP)
C**  6. FUNCTION   IBOX (X1,Y1,X2,Y2,X3,Y3,X4,Y4)
C**  7. SUBROUTINE SHUFFLE (XA,YA,NPPA,NSEQ,ICEN,USEGS,X3,Y3)
C**  8. SUBROUTINE DOUBLE (XA,YA,NPPA)
C**  9. SUBROUTINE STRAIT (XA,YA,NPPA)
C** 10. SUBROUTINE LARGEST (XA,YA,NPPA,LSEG)
C** 11. SUBROUTINE XCLOCK (XA,YA,NPPA,AREA,KLOK)
C** 13. SUBROUTINE LITARC (XA,YA,NPPA)
C** 14. SUBROUTINE MIDARC (XA,YA,NSEQ,XMID,YMID)
C** 15. SUBROUTINE INARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,XI,YI,IN)
C** 16. SUBROUTINE BOXARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,XL,YL,XG,YG)
C**     SUBROUTINE CHKARC
C**     SUBROUTINE CHKRC1
C**     SUBROUTINE defold
C**     SUBROUTINE lcfind
C**     SUBROUTINE LARGEST2
c**
C**    MODULE NAME AND RELEASE LEVEL
C**       off10.f , 25.1
C**    DATE AND TIME OF LAST  MODIFICATION
C**       04/29/15 , 15:10:22
C***********************************************************************
c**
c** copyright (c) 1990 MILLS DATA SYSTEMS CORPORATION
c**
c **********************************************************************
c **********************************************************************
c **  1. SUBROUTINE OFF1ST
C **  purpose of subroutine: COMPUTE FIRST OFFSET STRING
c **    input:
C **      KLOSED.......1/0 STRING CLOSED/OPEN
C **      ISIDE........1 - RIGHT, -1 - LEFT
C **      ISLE.........0 - POCKET LOOP, 1 - ISLAND LOOP
C **      RAD..........TOOL RADIUS
C **      NPPA.........NUMBER OF POINTS IN STRING
C **      XA,YA,NPPA...ARRAYS OF INPUT STRING
C **    output:
c **      XB,YB,NPPB...ARRAYS OF OUTPUT STRING
C **      IER..........0 - OK, ELSE - ERROR
c **********************************************************************
c **********************************************************************

      SUBROUTINE OFF1ST(KLOSED,ISIDE,ISLE,RAD,NPPA,XA,YA,XB,YB,NPPB,IER)

      include 'com4a.com'
      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts),rad
      integer*2 KLOSED,ISIDE,ISLE,NPPA,NPPB,IER

      real*4 rmin,r,rarc,co,ux,uy,uxl,uyl,dx,dy,ssq,s,x1,y1,add
      real*4 XOUT1,YOUT1,XOUT2,YOUT2,xn,yn,xc,yc
      integer*2 karc1,karc2,karc3,klok,nppam1,nopts,k
      logical lv93,lv94

C---  INITIALISATION
      IF (NPPA.GT.maxpts) THEN
        IER = 11
        GOTO 99
      ENDIF

      lv93 = sc(169) .lt. 9.349
      lv94 = sc(169) .lt. 9.449

      rmin = tol*3.0
      if (sc(169).lt.8.199999) rmin = 1.0
      karc2 = 0
      R = ABS(RAD)
      NPPAM1 = NPPA-1

C --- STARTS WITH ARC
      IF (XA(2).EQ.FLARC) THEN
        RARC = XA(3)
        KLOK = YA(3)
        CALL TANARC (2,XA(4),YA(4),RARC,KLOK,XA(5),YA(5),XA(1),YA(1),
     +               UX,UY)
        SSQ = 0.0001
        KARC1 = 1
      ELSE

C --- STARTS WITH SEG
        DX = XA(2) - XA(1)
        DY = YA(2) - YA(1)
        SSQ = DX*DX + DY*DY
        KARC1 = 0
        S = SQRT(SSQ)
        UX = DX/S
        UY = DY/S
      END IF

C --- COMPUTES FIRST OFFSET POINT
      CALL ENDPTS (1,ISIDE,R,XA,YA,NPPA,X1,Y1,IER)
      IF (IER.EQ.1) GOTO 99
      XB(1) = X1
      YB(1) = Y1
      NPPB = 1

C --- COMPUTES INNER OFFSET POINTS
      IF (NPPAM1.LT.2) GOTO 31
      K = 1
      IF (XA(2).EQ.FLARC) K = 0
   17 K = K+1
      IF (K.GT.NPPAM1) GO TO 31

      coa0 = coa
      coa = 1

C --- CORNER ENDS WITH ARC
      IF (XA(K+1).EQ.FLARC) THEN
        K = K+1
        KARC1 = 1
        RARC = XA(K+1)
        KLOK = YA(K+1)
        xc = xa(k+2)
        yc = ya(k+2)
c
c..... the block below was added to fix FSR 60450; it is supposed to generally
c..... improve the case of 2 adjacent arcs: if the offset arcs intersect we
c..... put the new offset arc right after the old one instead of trying to
c..... solve the corner (i.e., without a connecting segment)
c
        ADD = ISIDE*KLOK
        RNEW = RARC + ADD*R
        xb1 = xc + (RNEW/RARC)*(XA(K-1)-xc)
        yb1 = yc + (RNEW/RARC)*(YA(K-1)-yc)
        xb2 = xc + (RNEW/RARC)*(XA(K+3)-xc)
        yb2 = yc + (RNEW/RARC)*(YA(K+3)-yc)
        if (karc2.eq.1 .and. nppb.gt.3 .and. sc(169).ge.9.05 .and.
     +      XB(nppb-3).EQ.FLARC .and. rnew.gt.rmin) then
          nopts = 0
          klb = yb(nppb-2)
          CALL ARCAR1 (xb(nppb-1),yb(nppb-1),xb(nppb-2),klb,
     +                   xb(nppb-4),yb(nppb-4),xb(nppb),yb(nppb),
     +                   xa(k+2),ya(k+2),rnew,KLOK,
     +                   XB1,YB1,XB2,YB2,XINT,YINT,nopts)
          if (nopts .ge. 1) then
            if (nopts .eq. 1) then
              xb(nppb) = xint
              yb(nppb) = yint
            endif
            nppb = nppb + 1
            if (nppb+3 .gt. maxpts) goto 90
            XB(NPPB) = FLARC
            YB(NPPB) = 0.0
            NPPB = NPPB+1
            XB(NPPB) = RNEW
            YB(NPPB) = KLOK
            NPPB = NPPB+1
            XB(NPPB) = xc
            YB(NPPB) = yc
            NPPB = NPPB+1
            XB(NPPB) = xb2
            YB(NPPB) = yb2
            goto 18
          endif
        endif
        CALL TANARC (2,xc,yc,RARC,KLOK,XA(K+3),YA(K+3),
     +               XA(K-1),YA(K-1),DX,DY)
        X = XA(K-1)
        Y = YA(K-1)
        XN = X + 100.0*DX
        YN = Y + 100.0*DY
        UXL = UX
        UYL = UY
        SSQL = SSQ
        CALL XCORN (UXL,UYL,X,Y,XN,YN,KARC1,KARC2,KARC3
     +    ,SSQL,ISIDE,RAD,XOUT1,YOUT1,XOUT2,YOUT2,NOPTS,UX,UY,SSQ)
        if (nopts .eq. -1) then
          ier = 11
          return
        endif
c
c..... avoid creating a "fold" when offseting an (almost tangent) arc
c..... after a segment
c
        if (.not.lv93 .and. lwatfl .and. karc2.eq.0 .and.
     x      karc3.eq.0) then
          co = uxl*dx + uyl*dy
          if (co .gt. 0.99) then
            dx = xb1 - xout1
            dy = yb1 - yout1
            co = uxl*dx + uyl*dy
            if (co .lt. 0) goto 175
          endif
        endif

        NPPB = NPPB+1
        if (nppb.gt.maxpts) goto 90
        XB(NPPB) = XOUT1
        YB(NPPB) = YOUT1
        IF (NOPTS.EQ.2) THEN

C --- OUTSIDE ARC
          IF (KARC3.EQ.1) THEN
            NPPB = NPPB+1
            if (nppb.gt.maxpts) goto 90
            XB(NPPB) = FLARC
            YB(NPPB) = 0.0
            NPPB = NPPB+1
            if (nppb.gt.maxpts) goto 90
            KL = -1
            IF (RAD.LT.0.) KL = 1
            XB(NPPB) = R
            YB(NPPB) = KL
            NPPB = NPPB+1
            if (nppb.gt.maxpts) goto 90
            XB(NPPB) = X
            YB(NPPB) = Y
          ENDIF
          NPPB = NPPB+1
          if (nppb.gt.maxpts) goto 90
          XB(NPPB) = XOUT2
          YB(NPPB) = YOUT2
        ENDIF
  175   NPPB = NPPB+1
        if (nppb.gt.maxpts) goto 90
        XB(NPPB) = xb1
        YB(NPPB) = yb1
        IF (RNEW.GT.rmin) THEN
          NPPB = NPPB+1
          if (nppb.gt.maxpts) goto 90
          XB(NPPB) = FLARC
          YB(NPPB) =0.0
          NPPB = NPPB+1
          if (nppb.gt.maxpts) goto 90
          XB(NPPB) = RNEW
          YB(NPPB) = KLOK
          NPPB = NPPB+1
          if (nppb.gt.maxpts) goto 90
          XB(NPPB) = xc
          YB(NPPB) = yc
        ENDIF
        NPPB = NPPB+1
        if (nppb.gt.maxpts) goto 90
        XB(NPPB) = xb2
        YB(NPPB) = yb2
   18   CALL TANARC (1,xc,yc,RARC,KLOK,XA(K+3),YA(K+3),
     +               XA(K-1),YA(K-1),UX,UY)
        KARC2 = 1
        SSQ = 0.0001
        K = K + 2

C --- CORNER ENDS WITH SEG
      ELSE
        X = XA(K)
        Y = YA(K)
        XN = XA(K+1)
        YN = YA(K+1)
        KARC1 = 0
        UXL = UX
        UYL = UY
        SSQL = SSQ
        CALL XCORN (UXL,UYL,X,Y,XN,YN,KARC1,KARC2,KARC3
     +    ,SSQL,ISIDE,RAD,XOUT1,YOUT1,XOUT2,YOUT2,NOPTS,UX,UY,SSQ)
        if (nopts .eq. -1) then
          ier = 11
          return
        endif
c
c..... avoid creating a "fold" when offseting an (almost tangent) segment
c..... after an arc
c
        if (.not.lv93 .and. lwatfl .and. karc2.eq.1 .and.
     x      karc3.eq.0) then
          co = uxl*ux + uyl*uy
          if (co .gt. 0.99) then
            xb1 = x + iside*rad*uy
            yb1 = y - iside*rad*ux
            dx = xb1 - xb(nppb)
            dy = yb1 - yb(nppb)
            co = uxl*dx + uyl*dy
            if (co .lt. 0) goto 185
          endif
        endif
c
c..... avoid creating a "fold" when offseting a short segment
c..... after a corner. qar 95170
c
        coa = uxl*ux + uyl*uy
        if (.not.lv94 .and. karc2.eq.0 .and. karc3.eq.0) then
          dx = xout1 - xb(nppb)
          dy = yout1 - yb(nppb)
          s = sqrt (dx*dx + dy*dy)
          if (s .gt. tol) then
            dx = dx/s
            dy = dy/s
            co = uxl*dx + uyl*dy
            if (co .lt. -0.99) then
              if (coa .gt. 0.965926 .and. coa0 .lt. 0) then
                goto 185
              else if (coa0 .gt. 0.965926 .and. coa .lt. 0) then
                nppb = nppb-1
              endif
            endif
          endif
        endif

        NPPB = NPPB+1
        if (nppb.gt.maxpts) goto 90
        XB(NPPB) = XOUT1
        YB(NPPB) = YOUT1
        IF (NOPTS.EQ.2) THEN

C --- OUTSIDE ARC
          IF (KARC3.EQ.1) THEN
            NPPB = NPPB+1
            if (nppb.gt.maxpts) goto 90
            XB(NPPB) = FLARC
            YB(NPPB) = 0.0
            NPPB = NPPB+1
            if (nppb.gt.maxpts) goto 90
            KL = -1
            IF (RAD.LT.0.) KL = 1
            XB(NPPB) = R
            YB(NPPB) = KL
            NPPB = NPPB+1
            if (nppb.gt.maxpts) goto 90
            XB(NPPB) = X
            YB(NPPB) = Y
          ENDIF
          NPPB = NPPB+1
          if (nppb.gt.maxpts) goto 90
          XB(NPPB) = XOUT2
          YB(NPPB) = YOUT2
        ENDIF
  185   KARC2 = 0
      ENDIF
      GOTO 17

C---  COMPUTES FINAL OFFSET POINT
   31 NPPB = NPPB + 1
      if (nppb.gt.maxpts) goto 90
      CALL ENDPTS (2,ISIDE,R,XA,YA,NPPA,X1,Y1,IER)
      IF (IER.EQ.1) GOTO 99
      XB(NPPB) = X1
      YB(NPPB) = Y1
   90 IF (NPPB.GT.maxpts) IER = 11

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  2. SUBROUTINE XCORN
C **  purpose of subroutine: COMPUTE OFFSET POINTS FOR CORNER
C **    INPUT:
c **      UXL,UYL.......UNIT VECTOR ALONG LEADING EDGE
C **      X,Y...........CORNER POINT
C **      XN,YN.........THIRD POINT OF CORNER (NOT = X,Y)
C **      KARC1.........1- WITH IN COMING ARC, 0- WITHOUT
C **      KARC2.........1- WITH OUT GOING ARC, 0- WITHOUT
C **      SSQL..........SQUARE OF LENGTH OF LEADING EDGE (NON-ZERO)
C **      ISIDE.........1/-1  RIGHT/LEFT
C **      RAD...........TOOL RADIUS
C **    OUTPUT:
c **      XOUT1,YOUT1...FIRST OUTPUT POINT
C **      XOUT2,YOUT2...SECOND OUTPUT POINT (IF DEFINED)
C **      NOPTS.........NO OF POINTS OUTPUT (EITHER 1 OR 2)
C **      UX,UY.........UNIT VECTOR ALONG TRAILING EDGE
C **      SSQ...........SQUARE OF LENGTH OF LEADING EDGE
C **      KARC3.........1- RADIUS ON OUTSIDE CORNER, 0- NO RADIUS
c **********************************************************************
c **********************************************************************

      SUBROUTINE XCORN (UXL,UYL,X,Y,XN,YN ,KARC1,KARC2,KARC3
     + ,SSQL,ISIDE,RAD,XOUT1,YOUT1,XOUT2,YOUT2 ,NOPTS,UX,UY,SSQ)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

C---  INITIALISATION
      eps = tol/254.
      LSIDE = ISIDE
      IF (RAD.LT.0) LSIDE = -LSIDE
      DX = XN- X
      DY = YN- Y
      SSQ = DX*DX + DY*DY
      if (ssq .lt. eps) then
        nopts = -1
        return
      endif
      SCORNSQ = AMIN1 (SSQL,SSQ)
      S = SQRT(SSQ)
      UX = DX/S
      UY = DY/S
      KARC3 = 0

C --- COMPUTES PARAMETERS FOR DETERMINING TYPE OF CORNER
      SDOOBIE = (-UYL*UX + UXL*UY) * ISIDE
      IF (ABS(SDOOBIE).LT.eps) SDOOBIE = 0.0
      CTHETA = UXL*UX + UYL*UY
      IF (CTHETA.GT.0.9999) CTHETA = 1.0
      RADJ = ABS(RAD)

C --- CHECK FOR OUTSIDE RADIUS ON CORNER
      IF (LARCS.EQ.0) GO TO 3
      IF (SDOOBIE*LSIDE.LT.0.0.AND.CTHETA.LT.0.7) GO TO 7

C --- REGULAR - NO OUTSIDE RADIUS
    3 DISTSQ = 0.0
      IF(CTHETA.GT.-1.0) DISTSQ = (RADJ*RADJ)*((1.-CTHETA)/(1.+CTHETA))
      IF (SDOOBIE.EQ.0.0.AND.CTHETA.EQ.1.0) GO TO 4
      IF (SDOOBIE.EQ.0.0 .AND. CTHETA.EQ.-1.0) GO TO 5
      IF (((SDOOBIE .LT. 0.0) .AND. (DISTSQ .GT. SCORNSQ)) .OR.
     +    ((SDOOBIE .GE. 0.0) .AND. (CTHETA.LT.-0.08)))
     +    GO TO 5
      IF (SDOOBIE.LT.0.0.AND.KARC1+KARC2.NE.0) GO TO 5

C --- REFLEX ANGLE > 90 OUTPUT SINGLE POINT
    4 DIST = SQRT(DISTSQ)
      IF (SDOOBIE.GE.0.) DIST = -DIST
      XOUT1 = X + ISIDE*RADJ*UY + DIST*UX
      YOUT1 = Y - ISIDE*RADJ*UX + DIST*UY
      NOPTS = 1
      GOTO 99

C --- OTHERWISE OUTPUT TWO POINTS
    5 IF (SDOOBIE .LT. 0.) DIST = 0.
      IF (SDOOBIE .GE. 0.) DIST = RADJ
      XOUT1 = X + ISIDE*RADJ*UYL + DIST*UXL
      YOUT1 = Y - ISIDE*RADJ*UXL + DIST*UYL
      XOUT2 = X + ISIDE*RADJ*UY  - DIST*UX
      YOUT2 = Y - ISIDE*RADJ*UX  - DIST*UY
      NOPTS = 2
      IF (SDOOBIE .GE. 0.0) GO TO 99
      IF (ABS(XOUT1-XOUT2).GT.0.25) GO TO 99
      IF (ABS(YOUT1-YOUT2).GT.0.25) GO TO 99
      GO TO 4

C --- OUTSIDE ARC
    7 KARC3 = 1
      NOPTS = 2
      XOUT1 = X + ISIDE*RADJ*UYL
      YOUT1 = Y - ISIDE*RADJ*UXL
      XOUT2 = X + ISIDE*RADJ*UY
      YOUT2 = Y - ISIDE*RADJ*UX

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  3. SUBROUTINE BOXES
C **  purpose of subroutine: DIVIDE XB,YB SEGMENTS INTO BOXES with
c **                         their max/min values
c **    input:
C **      NPPB,xb,yb........TOTAL NO OF POINTS and string arrays
c **    output:
C **      NBOX..............TOTAL NO OF BOXES
c **      llt...............array of box allocation indexes
c **      xl1,yl1,xl2,yl2...box max/min arrays
c **
C **      NIB...............NO OF SEGMENTS PER BOX
c **********************************************************************
c **********************************************************************

      SUBROUTINE BOXES(NPPB,XB,YB,XL1,YL1,XL2,YL2,LLT,NBOX)

      include 'com4a.com'
      include 'pokcom.com'

      real*4 XB(maxpts),YB(maxpts)
      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)

      integer*2 i,k,n,n1,n2,npp,nib
      real*4 snib

C---  DIVIDE INTO OPTIMAL NUMBER OF GROUPS
      n2 = 0
      DO 17 I = 1, NPPB
        IF (XB(I).EQ.FLARC) n2 = n2+1
   17 CONTINUE
c..... n2 is the number of arcs
      NPP = NPPB-3*n2-1
      if (sc(169) .le. 9.349) then
        SNIB = NPP
        SNIB = SQRT(SNIB)
        NIB = SNIB + 0.5
        NBOX = NPP/NIB
        N1 = NPP - NBOX*NIB
C                   IF (N1.NE.0) NBOX = NBOX + 1
        IF (NBOX .LT. 30) GO TO 30

C---  IF NECESSARY INCREASE NO OF POINTS PER BOX
c     SUBJECT TO MAXIMUM NO OF BOXES = 29
        NIB = (NPP-3*N1-1)/29
        N1 = NPP - NIB*119
        IF (N1.NE.0) NIB = NIB + 1
        NBOX = NPP/NIB
        N1 = NPP - NBOX*NIB
      else if (npp .lt. 870) then
        SNIB = NPP
        SNIB = SQRT(SNIB)
        NIB = SNIB + 0.5
        NBOX = NPP/NIB
        N1 = NPP - NBOX*NIB
      else
C---  IF NECESSARY INCREASE NO OF POINTS PER BOX
c     SUBJECT TO MAXIMUM NO OF BOXES = 29
        NIB = NPP/29
        nbox = 29
        N1 = NPP - NIB*29
      endif
C---  DEFINE LLT ARRAY
   30 N2 = -1
      DO 40 I = 1, NBOX
        IF(I.EQ.1) LLT(I) =  1
        IF(I.GT.1) LLT(I) =  LLT(I-1) + NIB
        N2 = N2 + 1
        IF (N2.LE.N1 .AND. I.GT.1) LLT(I) = LLT(I) + 1
   40 CONTINUE
      LLT(NBOX+1) = NPP + 1
      DO 55 I = 1, NPPB-4
        IF (XB(I+1).EQ.FLARC) THEN
          DO 50 N = 1, NBOX+1
            IF (LLT(N).GT.I) LLT(N) = LLT(N)+3
   50     CONTINUE
        END IF
   55 CONTINUE

C---  DEFINE BOXES
      DO 70 N = 1, NBOX
        N1 = LLT(N)
        N2 = LLT(N+1)
        IF(N2.GT.NPPB) N2 = NPPB
        XL1(N) = 1000000000000.0
        YL1(N) = 1000000000000.0
        XL2(N) =-1000000000000.0
        YL2(N) =-1000000000000.0
        K = N1-1
   60   K = K+1
        IF (K.GT.N2) GOTO 65
        IF (XB(K).EQ.FLARC) THEN
          R = XB(K+1)
          KLOK = YB(K+1)
          CALL BOXARC (XB(K+2),YB(K+2),R,KLOK,XB(K-1),YB(K-1),
     +                 XB(K+3),YB(K+3),X1,Y1,X2,Y2)
          K = K+3
        ELSE
          X1 = XB(K)
          Y1 = YB(K)
          X2 = X1
          Y2 = Y1
        ENDIF
        IF (X1 .LT. XL1(N)) XL1(N) = X1
        IF (Y1 .LT. YL1(N)) YL1(N) = Y1
        IF (X2 .GT. XL2(N)) XL2(N) = X2
        IF (Y2 .GT. YL2(N)) YL2(N) = Y2
        GOTO 60
   65   continue
   70 CONTINUE

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  4. SUBROUTINE DELOOPS
C **  purpose of subroutine: CLEANS UP KNOTTED STRINGS FOR FULL POCKET
C **    INPUT:
c **      NOBOXES..........NUMBER OF BOXES
C **      NPPB.............NUMBER OF ELEMENTS IN INPUT STRING
C **      MM1,MM2..........FIRST AND LAST POINTS OF B LOOP
C **      XMM,YMM..........START/END POINT OF B LOOP
C **      INGOS............NUMBER OF CURRENT INNER LOOPS
C **      INGO.............HITTING SEGMENT
C **      INTO.............HIT SEGMENT
C **      XING,YING........HIT LOCATION
C **      ING..............CURRENT INNER LOOP
C **      LLT..............ARRAY OF BOX ALLOCATION NUMBERS
C **      XL1,YL1,XL2,YL2..BOX ARRAYS
C **      KLOSED...........1/0  STRING CLOSED/OPEN
C **      XB,YB............INPUT STRING
C **    OUTPUT:
C **      INGOS............NUMBER OF CURRENT INNER LOOPS
C **      INGO.............HITTING SEGMENT
C **      INTO.............HIT SEGMENT
C **      XING,YING........HIT LOCATION
c **      XC,YC............DEKNOTTED STRING FOR OUTPUT
C **      NPPC.............TOTAL NUMBER OF POINTS IN OUTPUT STRING
c **********************************************************************
c **********************************************************************

      SUBROUTINE DELOOPS (NOBOXES,NPPB,MM1,MM2,XMM,YMM,INGOS,INGO,INTO,
     +  XING,YING,ING,LLT,XL1,YL1,XL2,YL2,KLOSED,XB,YB,XC,YC,NPPC,IERR)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XL1(30),YL1(30),XL2(30),YL2(30)
      integer*4 LLT(30)
      real*4 XB(maxpts),YB(maxpts),XC(maxpts),YC(maxpts)
      integer*4 INGO(mxingos),INTO(mxingos)
      real*4 XING(mxingos),YING(mxingos)

C---  INITIALIZATION
      eps = tol/254.
      IERR = 0
      NPP = MM2
      ILOOP = 0
      NOBOX = 1
      DO 11 I = 1, NOBOXES
        IF (MM1.GE.LLT(I)-1.AND.MM1.LT.LLT(I+1)-1) THEN
          NOBOX = I
          GO TO 12
        END IF
   11 CONTINUE
   12 DO 13 I = 1, NOBOXES
        IF (MM2.GE.LLT(I).AND.MM2.LE.LLT(I+1)-1) THEN
          NOBOXSS = I
          GO TO 14
        END IF
   13 CONTINUE
   14 NX = MM1
      XINT = XMM
      YINT = YMM
      KARC1 = 0
      R1 = 0.0
      KLOK1 = 0
      XC1 = 0.0
      YC1 = 0.0

C---  COMPUTES FIRST DELOOPED POINT
      NPPC = 1
      XC(1) = XINT
      YC(1) = YINT

C---  COMPUTES INNER DELOOPED POINTS
      IF (MM2-MM1.LT. 1) GO TO 51
      N = MM1-1
   15 N = N+1
      IF (N.GT.MM2) GO TO 51
      IF (N.LT.NX .AND. ILOOP.EQ.1) N = NX
      IF (ILOOP.EQ.0 .AND. N.GE.LLT(NOBOX+1)-1) NOBOX = NOBOX+1
      IF (ILOOP.EQ.1) NOBOX = NOBOXX
      IF (XB(N+1).EQ.FLARC) THEN
        KARC1 = 1
        R1 = XB(N+2)
        KLOK1 = YB(N+2)
        XC1 = XB(N+3)
        YC1 = YB(N+3)
        X1 = XINT
        Y1 = YINT
        X2 = XB(N+4)
        Y2 = YB(N+4)
      ELSE
        KARC1 = 0
        X1 = XINT
        Y1 = YINT
        X2 = XB(N+1)
        Y2 = YB(N+1)
      END IF
      CALL SEEK (N,NOBOX,NOBOXSS,KARC1,XC1,YC1,R1,KLOK1,
     +       X1,Y1,X2,Y2,XB,YB,MM1,MM2,XMM,YMM,LLT,XL1,YL1,XL2,YL2,
     +       KLOSED,NX,NOBOXX,XINT,YINT,ILOOP)
      IF (ILOOP.EQ.1) THEN
        IF(INGOS.EQ.0) GO TO 21
        DO 27 J = 1,INGOS
          IF (N.NE.INGO(J))GO TO 27
          IF (NX.NE.INTO(J)) GO TO 27
          IF (ABS(XINT-XING(J)).GT.eps) GO TO 27
          IF (ABS(YINT-YING(J)).GT.eps) GO TO 27
          GO TO 32
   27   CONTINUE
   21   INGOS = INGOS+1

        if (ingos .gt. mxingos) then
          ingos = mxingos
          IERR = 11
          goto 99
        endif

        INGO(INGOS) = N
        INTO(INGOS) = NX
        XING(INGOS) = XINT
        YING(INGOS) = YINT
      END IF
   32 IF (KARC1.EQ.1) THEN
        NPPC = NPPC+1
        IF (NPPC.GT.maxpts) THEN
          IERR = 11
          GO TO 99
        END IF
        XC(NPPC) = FLARC
        YC(NPPC) = 0.0
        N = N+1
        NPPC = NPPC+1
        IF (NPPC.GT.maxpts) THEN
          IERR = 11
          GO TO 99
        END IF
        XC(NPPC) = R1
        YC(NPPC) = KLOK1
        N = N+1
        NPPC = NPPC+1
        IF (NPPC.GT.maxpts) THEN
          IERR = 11
          GO TO 99
        END IF
        XC(NPPC) = XC1
        YC(NPPC) = YC1
        N = N+1
      END IF
      IF (ILOOP.EQ.0) THEN
        XINT = X2
        YINT = Y2
      END IF
      IF (N.GE.MM2) GO TO 51
      NPPC = NPPC+1
      IF (NPPC.GT.maxpts) THEN
        IERR = 11
        GO TO 99
      END IF
      XC(NPPC) = XINT
      YC(NPPC) = YINT
      GO TO 15

C---  COMPUTES FINAL DELOOPED POINT
   51 NPPC = NPPC + 1
      IF (NPPC.GT.maxpts) THEN
        IERR = 11
        GO TO 99
      END IF
      XC(NPPC) = XMM
      YC(NPPC) = YMM

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  5. SUBROUTINE SEEK
C **  purpose of subroutine: LOOKS FOR LOOPS IN B-STRING
C **    INPUT:
c **      NOSEGIN............SEQ NO OF INPUT ELEMENT
C **                         -1 FOR END CHECK, -2 & -3 FOR LUPLUP
C **      NOBOXIN............SEQ NO OF INPUT BOX
C **      NOBOXSS............TOTAL NO OF BOXES IN LOOP
C **      KARC1..............1- ARC, 0- STRAIGHT SEG
C **      XC1,YC1,R1,KLOK1...ARC CENTER, RADIUS AND DIRECTION
C **      X1,Y1,X2,Y2........INPUT SEGMENT OR ARC POINTS
C **      XB,YB..............STRING ARRAYS
C **      MM1................FIRST INDEX IN LOOP
C **      MM2................LAST INDEX IN LOOP
C **      XMM,YMM............FIRST POINT IN LOOP
C **      LLT................ARRAY OF BOX PARAMETERS
C **      XL1,YL1,XL2,YL2....ARRAYS DEFINING BOXES
C **      KLOSED.............1/0 CLOSED/OPEN STRING
C **    OUTPUT:
c **      NOELX..............SEQ NO OF OUTPUT ELEMENT
C **      NOBOXX.............SEQ NO OF OUTPUT BOX
C **      XINT,YINT..........INTERSECTION POINT CAUSING LOOP
C **      ILOOP..............1/0 LOOP FOUND/NOT FOUND
c **********************************************************************
c **********************************************************************

      SUBROUTINE SEEK (NOSEGIN,NOBOXIN,NOBOXSS,KARC1,XC1,YC1,R1,KLOK1,
     +          X1,Y1,X2,Y2,XB,YB,MM1,MM2,XMM,YMM,LLT,XL1,YL1,XL2,YL2,
     +          KLOSED,NOELX,NOBOXX,XINT,YINT,ILOOP)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XL1(30),YL1(30)
      integer*4 LLT(30)
      real*4 XL2(30),YL2(30)

      real*4 XB(maxpts),YB(maxpts)

C---  INITIALIZATION
      EPS = tol/25.4
      ILOOP = 0
      INT = 0
      NOELX = 0
      NOBOXX = 0
      KARC2 = 0
      NOEND = 0
      if (noboxin .gt.noboxss) go to 99
      IF (KARC1.EQ.1) THEN
        CALL BOXARC (XC1,YC1,R1,KLOK1,X1,Y1,X2,Y2,XL,YL,XR,YR)
      ELSE
        XL = AMIN1 (X1,X2)
        YL = AMIN1 (Y1,Y2)
        XR = AMAX1 (X1,X2)
        YR = AMAX1 (Y1,Y2)
      END IF
      X2TEMP = X2
      Y2TEMP = Y2

C---  LOOP HANDLING LISTED BOXES
C---  CHECK CURRENT BOX AGAINST INPUT BOX
      DO 30 NOBOX = NOBOXIN, NOBOXSS
        NOEND = 0
        IVAL=1
        IF (NOBOX .GE. NOBOXIN+1) IVAL = IBOX (XL,YL,XR,YR,XL1(NOBOX)
     +     ,YL1(NOBOX), XL2(NOBOX), YL2(NOBOX))
        IF (IVAL.EQ.0) GO TO 30

C---  LOOP HANDLING SEGMENTS WITHIN BOX
        N1 = LLT(NOBOX)
        N2 = LLT(NOBOX+1) - 1
        IF (NOBOX.EQ.NOBOXSS) N2 = MM2
        IF (NOBOX.EQ.NOBOXIN.AND.NOSEGIN.GT.0) THEN
          KARC2 = 0
          IF (KARC1.EQ.0.AND.XB(NOSEGIN+2).EQ.FLARC) KARC2 = 1
          IF (KARC1.EQ.1.AND.XB(NOSEGIN+5).EQ.FLARC) KARC2 = 1
          IF (KARC1.EQ.0.AND.KARC2.EQ.0) N1 = NOSEGIN+2
          IF (KARC1.EQ.0.AND.KARC2.EQ.1) N1 = NOSEGIN+1
          IF (KARC1.EQ.1) N1 = NOSEGIN + 4
          IF (KARC1+KARC2.GT.0) NOEND = 1
        END IF
        IF (NOBOX.EQ.NOBOXSS.AND.NOSEGIN.EQ.MM1) THEN
          N2 = N2 - 1
          IF (KARC1.EQ.1) N2 = N2+1
          IF (KARC1.EQ.1) NOEND = 1
          IF (KARC1.EQ.0.AND.XB(MM2+1).EQ.FLARC) N2 = N2+1
        END IF
        IF (N1.GT.N2) GO TO 30

C---  REDUCE SIZE OF INPUT SEGMENT AND ITS BOX ACCORDINGLY
        NOSEG = N1-1
   20   NOSEG = NOSEG+1
        IF (NOSEG.GT.N2) GO TO 30
        IF (NOSEGIN.EQ.MM1.AND.NOSEG.EQ.MM2.AND.KARC1.EQ.0.
     +      AND.XB(MM2+1).NE.FLARC) GO TO 30
        KARC2 = 0
        IF (XB(NOSEG+1).EQ.FLARC) THEN
          KARC2 = 1
          R2 = XB(NOSEG+2)
          KLOK2 = YB(NOSEG+2)
          XC2 = XB(NOSEG+3)
          YC2 = YB(NOSEG+3)
          X3 = XB(NOSEG)
          Y3 = YB(NOSEG)
          X4 = XB(NOSEG+4)
          Y4 = YB(NOSEG+4)
        END IF
        IF (NOSEGIN.GT.0) THEN
          IF (NOSEG-NOSEGIN.EQ.1.AND.KARC1+KARC2.EQ.0) GO TO 20
          IF (NOSEG-NOSEGIN.EQ.1.AND.KARC2.EQ.1) NOEND = 1
          IF (NOSEG-NOSEGIN.EQ.4.AND.KARC1.EQ.1) NOEND = 1
        END IF
        IF (NOSEGIN.EQ.-3) NOEND = 1
        IF (INT.EQ.1) THEN
          IF (KARC1.EQ.1) THEN
            CALL BOXARC (XC1,YC1,R1,KLOK1,X1,Y1,XINT,YINT,XL,YL,XR,YR)
          ELSE
            XL= AMIN1(X1,XINT)
            YL= AMIN1(Y1,YINT)
            XR= AMAX1(X1,XINT)
            YR= AMAX1(Y1,YINT)
          END IF
          X2TEMP = XINT
          Y2TEMP = YINT
        END IF

C---  COMPUTE SUB-BOX OF BOX SEGMENT
        IF (KARC2.EQ.1) THEN
          CALL BOXARC (XC2,YC2,R2,KLOK2,X3,Y3,X4,Y4,XLS,YLS,XRS,YRS)
        ELSE
          XLS = AMIN1 (XB(NOSEG),XB(NOSEG+1))
          YLS = AMIN1 (YB(NOSEG),YB(NOSEG+1))
          XRS = AMAX1 (XB(NOSEG),XB(NOSEG+1))
          YRS = AMAX1 (YB(NOSEG),YB(NOSEG+1))
        END IF

C---  CHECK FOR INTERSECTION
        INT = 0
        IF (IBOX (XL,YL,XR,YR,XLS,YLS,XRS,YRS) .EQ. 1) THEN

C --- SEGMENT INTERSECTS SEGMENT
          IF (KARC1+KARC2.EQ.0) THEN
            CALL LINEX(0,X1,Y1,X2TEMP,Y2TEMP,XB(NOSEG),YB(NOSEG),
     +                 XB(NOSEG+1),YB(NOSEG+1),XINT1,YINT1,INT)
            if (int.eq.2) int = 0
            GO TO 26
          END IF

C --- ARC INTERSECTS SEGMENT
          IF (KARC1.EQ.1.AND.KARC2.EQ.0) THEN
            IS = 1
            IF (NOSEGIN.EQ.MM1.AND.NOSEG.EQ.MM2) IS = 2
            CALL ARCSEG (IS,NOEND,XC1,YC1,R1,KLOK1,X1,Y1,
     x                   X2TEMP,Y2TEMP,XB(NOSEG),YB(NOSEG),
     x                   XB(NOSEG+1),YB(NOSEG+1),XINT1,YINT1,INT)
            GO TO 26
          END IF

C --- SEGMENT INTERSECTS ARC
          IF (KARC1.EQ.0.AND.KARC2.EQ.1) THEN
            IS = 2
            IF (NOSEGIN.EQ.MM1.AND.NOSEG.EQ.MM2) IS = 1
            CALL ARCSEG (IS,NOEND,XC2,YC2,R2,KLOK2,X3,Y3,X4,Y4,
     +                   X1,Y1,X2TEMP,Y2TEMP,XINT1,YINT1,INT)
            GO TO 26
          END IF

C --- ARC INTERSECTS ARC
          IF (KARC1.EQ.1.AND.KARC2.EQ.1) THEN
            IS = 1
            IF (NOSEGIN.EQ.MM1.AND.NOSEG.EQ.MM2) IS = 2
            CALL ARCARC (IS,NOEND,XC1,YC1,R1,KLOK1,X1,Y1,X2TEMP,Y2TEMP,
     +             XC2,YC2,R2,KLOK2,X3,Y3,X4,Y4,XINT1,YINT1,INT)
          END IF
        END IF
   26   IF (NOSEGIN.LT.-1.AND.
     +      ABS(XINT1-XMM).LT.EPS.AND.ABS(YINT1-YMM).LT.EPS) INT = 0

C---  UPDATE OF VARIABLES
        IF (INT.EQ.1) THEN
          IF (NOSEGIN.EQ.MM1.AND.NOSEG.EQ.MM2.AND.
     +      ABS(XINT1-XMM).LT.EPS.AND.ABS(YINT1-YMM).LT.EPS) GO TO 27
          XINT = XINT1
          YINT = YINT1
          ILOOP = 1
          NOELX = NOSEG
          NOBOXX = NOBOX
        END IF
   27   IF (KARC2.EQ.1) NOSEG = NOSEG+3
        NOEND = 0
        GO TO 20
   30 CONTINUE

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  6. FUNCTION IBOX
C **  purpose of function: INDICATOR OF INTERSECTION BETWEEN
c **                       THE 1-2 BOX AND THE 3-4 BOX
C **                       IT ASSUMES X1.LE.X2 AND X3.LE.X4,
c **                       AND SIMILARLY WITH THE Y'S
c **********************************************************************
c **********************************************************************

      FUNCTION IBOX (X1,Y1,X2,Y2,X3,Y3,X4,Y4)

      real*4 x1,x2,x3,x4, y1,y2,y3,y4
      integer*2 ibox

      IBOX = 0
      IF ((X2.GE.X3) .AND. (X1.LE.X4) .AND. (Y2.GE.Y3) .AND. (Y1.LE.Y4))
     +      IBOX=1

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  7. SUBROUTINE SHUFFLE
C **  purpose of subroutine: REORDERS STRING TO START FROM DIVISION
c **                         DEFINED BY MID NSEQ OR FROM POINT ON SEG.
C **                         THE METHOD IS TO SET UP THE NEW ORDER IN
c **                         TEMPORARY ARRAYS XT,YT AND THEN TO REDEFINE
c **                         XA,YA ACCORDINGLY
C **                         FOR A GIVEN POINT - ADJUST TO LOCATE ON ARC
C **    INPUT:
c **      XA,YA........ARRAYS OF INPUT STRING
C **      NPPA.........NO OF POINTS IN INPUT STRING
C **      NSEQ.........SEQUENCE NO OF DIVIDED SEGMENT
C **      ICEN.........1- BY RELATIVE LOC, 0- BY GIVEN POINT
C **      USEGS........DIVISION RELATIVE LOCATION
C **      X3,Y3........DIVISION POINT OR OUTPUT POINT
C **    OUTPUT:
c **      XA,YA,NPPA...AS ABOVE
C **      X3,Y3........DIVISION POINT OR OUTPUT POINT
c **********************************************************************
c **********************************************************************

      SUBROUTINE SHUFFLE (XA,YA,NPPA,NSEQ,ICEN,USEGS,X3,Y3)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XT(maxpts),YT(maxpts)

      USEG1 = USEGS
      IF (ICEN.EQ.0) THEN
        IF (XA(NSEQ+1).EQ.FLARC) THEN
          R = XA(NSEQ+2)
          XC = XA(NSEQ+3)
          YC = YA(NSEQ+3)
          DX = X3-XC
          DY = Y3-YC
          DD = SQRT(DX*DX+DY*DY)
          DX = DX/DD
          DY = DY/DD
          XT(1) = XC+R*DX
          YT(1) = YC+R*DY
        ELSE
          XT(1) = X3
          YT(1) = Y3
        END IF
        GO TO 16
      END IF

C --- FOR A GIVEN RELATIVE LOCATION - FIND POINT
      IF (XA(NSEQ+1).EQ.FLARC) THEN
        R = XA(NSEQ+2)
        KLOK = YA(NSEQ+2)
        XC = XA(NSEQ+3)
        YC = YA(NSEQ+3)
        CALL XYARC (R,KLOK,XC,YC,
     +      XA(NSEQ),YA(NSEQ),XA(NSEQ+4),YA(NSEQ+4),USEG1,X3,Y3)
      ELSE
        CALL XYSEG (XA(NSEQ),YA(NSEQ),XA(NSEQ+1),YA(NSEQ+1),
     +              USEG1,X3,Y3)
      END IF
      XT(1) = X3
      YT(1) = Y3

C --- SHUFFLE STRING
   16 K1 = 1
      DO 23 I = NSEQ+1, NPPA
        K1 = K1+1
        XT(K1) = XA(I)
        YT(K1) = YA(I)
   23 CONTINUE
      IF (NSEQ.EQ.1) GO TO 41
      DO 33 I = 2, NSEQ
        K1 = K1+1
        XT(K1) = XA(I)
        YT(K1) = YA(I)
   33 CONTINUE
   41 IF (XA(NSEQ+1).EQ.FLARC .AND. USEG1.GT.0.) THEN
        DO 44 I = NSEQ+1, NSEQ+3
          K1 = K1+1
          XT(K1) = XA(I)
          YT(K1) = YA(I)
   44   CONTINUE
      END IF
      NPPT = K1+1
      XT(NPPT) = XT(1)
      YT(NPPT) = YT(1)
      DO 55 I= 1, NPPT
        XA(I) = XT(I)
        YA(I) = YT(I)
   55 CONTINUE
      NPPA = NPPT
C check for double point at end
      DX = ABS (XA(NPPA)-XA(NPPA-1))
      DY = ABS (YA(NPPA)-YA(NPPA-1))
      tolacc=tol/2.54
      IF (DX.LT.TOLACC.AND.DY.LT.TOLACC) THEN
        NPPA = NPPA-1
      ENDIF

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  8. SUBROUTINE DOUBLE
C **  purpose of subroutine: REMOVE DOUBLE POINTS
c **    input/output:
C **      XA,YA...STRING OF POINTS
C **      NPPA....NUMBER OF POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE DOUBLE (XA,YA,NPPA)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      NPP = NPPA
      EE = tol/.254

C --- REMOVE DOUBLE POINTS
      I = 0
   11 I = I + 1
      IF (I.GE.NPP) GO TO 35
      K1 = 1
      IF (XA(I+1).EQ.FLARC) THEN
        K1 = 4
      ENDIF
      IF (ABS(XA(I)-XA(I+K1)).LT.EE.AND.ABS(YA(I)-YA(I+K1)).LT.EE)THEN
        DO 19 J = I, NPP - K1
          XA(J) = XA(J+K1)
          YA(J) = YA(J+K1)
   19   CONTINUE
        I = I-1
        NPP = NPP - K1
      ELSE
        I = I+K1-1
      END IF
      GO TO 11
   35 continue
      NPPA = NPP

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  9. SUBROUTINE STRAIT
C **  purpose of subroutine: REMOVES STRAIGHT LINE POINTS AND
c **                         overlapping ends
c **    input/output:
C **      XA,YA...STRING OF POINTS
C **      NPPA....NUMBER OF POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE STRAIT (XA,YA,NPPA)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      NPP = NPPA
      ERAD = tol/25.4
      EE = erad*.5

C --- CLEAR middle OF redundant data
      I = 0
   23 I = I+1
      IF (I.GE.NPP-1) GO TO 31
      IF (XA(I+1).EQ.FLARC) THEN
        IF (npp-i.ge.8.and.XA(i+5).EQ.FLARC) THEN
          IF (ABS(XA(i+2)-XA(i+6)).LT.ERAD.AND.
     +        ABS(YA(i+2)-YA(i+6)).LT.ERAD) THEN
            IF (ABS(XA(i+3)-XA(i+7)).LT.ERAD.AND.
     +          ABS(YA(i+3)-YA(i+7)).LT.ERAD) THEN
             IF (ABS(XA(i)-XA(i+8)).GT.ERAD.OR.
     +           ABS(YA(i)-YA(i+8)).GT.ERAD) THEN
              x1s = xa(i)-xa(i+3)
              y1s = ya(i)-ya(i+3)
              x2e = xa(i+8)-xa(i+7)
              y2e = ya(i+8)-ya(i+7)
              if ((x1s*x2e+y1s*y2e).gt.-ee) then
C --- REMOVE in-line arc
                DO 24 J = I+4, NPP-4
                  XA(J) = XA(J+4)
                  YA(J) = YA(J+4)
   24           CONTINUE
                I = I-1
                NPP = NPP-4
                GO TO 23
              end if
             end if
            end if
          END IF
        END IF
        I = I+3
        GO TO 23
      END IF
      IF (XA(I+2).EQ.FLARC) THEN
        I = I+4
        GO TO 23
      END IF
      X1 = XA(I+1)-XA(I)
      Y1 = YA(I+1)-YA(I)
      D1 = SQRT(X1*X1+Y1*Y1)
      X2 = XA(I+2)-XA(I+1)
      Y2 = YA(I+2)-YA(I+1)
      D2 = SQRT(X2*X2+Y2*Y2)
      SI = X1*Y2-Y1*X2
      IF (ABS(SI).LT.d1*d2*EE) THEN
C --- REMOVE in-LINE POINT
        DO 25 J = I+1, NPP-1
          XA(J) = XA(J+1)
          YA(J) = YA(J+1)
   25   CONTINUE
        I = I-1
        NPP = NPP-1
      END IF
      GO TO 23

C --- CLEAR ENDS OF OVERLAP
   31 IF (npp.gt.4.and.XA(2).EQ.FLARC.AND.XA(NPP-3).EQ.FLARC) THEN
        IF (ABS(XA(3)-XA(NPP-2)).LT.ERAD.AND.
     +      ABS(YA(3)-YA(NPP-2)).LT.ERAD) THEN
          IF (ABS(XA(4)-XA(NPP-1)).LT.ERAD.AND.
     +        ABS(YA(4)-YA(NPP-1)).LT.ERAD) THEN
            rr = xa(3)*xa(3)
            x1s = xa(1)-xa(4)
            y1s = ya(1)-ya(4)
            x2e = xa(npp)-xa(npp-1)
            y2e = ya(npp)-ya(npp-1)
            if (ya(3)*(x1s*y2e-x2e*y1s).gt.rr*ee) then
c... we have overlap maybe
              x1e = xa(5)-xa(4)
              y1e = ya(5)-ya(4)
              x2s = xa(npp-4)-xa(npp-1)
              y2s = ya(npp-4)-ya(npp-1)
              if (ya(3)*(x2s*y1e-x1e*y2s).gt.rr*ee) then
c... we have overlap for sure
                if ((x1e*x2s+y1e*y2s).gt.-ee) then
c... .lt. 90 - one arc - modify first arc and remove end
                  XA(1) = XA(NPP-4)
                  YA(1) = YA(NPP-4)
                  NPP = NPP-4
                else
c... .gt. 90 - two arcs - shorten 1st only
                  XA(1) = XA(NPP)
                  YA(1) = YA(NPP)
                endif
                go to 35
              end if
            end if
          END IF
        END IF
      END IF
      IF (NPP.GT.4.AND.XA(NPP-3).EQ.FLARC) GO TO 35
      IF (XA(2).EQ.FLARC) GO TO 35
      X1 = XA(NPP)-XA(NPP-1)
      Y1 = YA(NPP)-YA(NPP-1)
      D1 = SQRT(X1*X1+Y1*Y1)
      X2 = XA(2)-XA(1)
      Y2 = YA(2)-YA(1)
      D2 = SQRT(X2*X2+Y2*Y2)
      SI = X1*Y2-Y1*X2
      IF (ABS(SI).LT.d1*d2*EE) THEN
        x1 = xa(npp)-xa(1)
        y1 = ya(npp)-ya(1)
        if ((x1*x2+y1*y2).gt.0.0) then
c... straight ends overlap remove end
          XA(1) = XA(NPP-1)
          YA(1) = YA(NPP-1)
          NPP = NPP-1
        end if
      END IF
   35 continue
      NPPA = NPP

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE segmax
C **  purpose of subroutine: FIND LARGEST SEGMENT IN STRING (only
c **                         STRAIGHT SEGMENTs are considered)
c **    input:
C **      XA,YA......STRING of points
C **      NPPA.......NO OF POINTS
C **    output:
c **      LSEG....LARGEST SEGMENT
c **      dd  ....squared length of the LARGEST SEGMENT
c **********************************************************************
c **********************************************************************

      SUBROUTINE  segmax (xa,ya,nppa,dd,lseg)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)
      integer*2 nppa,lseg

      real*4 dx,dy,dd,dsq

      integer*2 k

      DD = 0.0
      K = 0
   21 K = K+1
      IF (K.GE.NPPA) GO TO 99
      IF (XA(K+1).EQ.FLARC) THEN
        K = K+3
        goto 21
      ELSE
        DX = XA(K+1)-XA(K)
        DY = YA(K+1)-YA(K)
      END IF
      DSQ = DX*DX+DY*DY
      IF (DSQ.GT.DD) THEN
        LSEG = K
        DD = DSQ
      END IF
      GO TO 21

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  10. SUBROUTINE LARGES1
C **  purpose of subroutine: FIND LARGEST SEGMENT IN STRING, PREFER
c **                         STRAIGHT SEGMENT
c **    input:
C **      XA,YA......STRING of points
C **      NPPA.......NO OF POINTS
C **      lastlup....same contour processed for the second time iff 1
C **    output:
c **      LSEG....LARGEST SEGMENT no if<>0
c **********************************************************************
c **********************************************************************

      SUBROUTINE LARGES1 (XA,YA,NPPA,LSEG,lastlup)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)
      integer*2 nppa,lseg,lastlup

      real*4 dx,dy,dd,dsq,factor,eps
      parameter (eps = 0.0025)

      integer*2 k,k1

      DD = 0.0
      K = 0
   21 K = K+1
      IF (K.GE.NPPA) GO TO 50
      IF (XA(K+1).EQ.FLARC) THEN
        DX = XA(K+4)-XA(K)
        DY = YA(K+4)-YA(K)
        K1 = K
        K = K+3
        FACTOR = 1.5
      ELSE
        DX = XA(K+1)-XA(K)
        DY = YA(K+1)-YA(K)
        K1 = K
        FACTOR = 1.0
      END IF
      DSQ = (DX*DX+DY*DY)*FACTOR
      IF (DSQ.GT.DD) THEN
        LSEG = K1
        DD = DSQ
      END IF
      GO TO 21

50    if (lastlup.eq.1 .and.lseg.eq.nppa-1 .and. nppa.gt.4) then
c
c..... if the first point is exactly in the middle, always choose
c..... the first segment - for consistency
c
        dx = abs(xa(2)+xa(nppa-1)-2*xa(1))
        dy = abs(ya(2)+ya(nppa-1)-2*ya(1))
        if (dx.lt.eps .and. dy.lt.eps) then
          lseg = 1
        endif
      endif

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  10. SUBROUTINE LARGES2
C **  purpose of subroutine: FIND LARGEST SEGMENT IN STRING, PREFER
c **                         STRAIGHT SEGMENT
c **    input:
C **      XA,YA...STRING of points
C **      NPPA....NO OF POINTS
C **    output:
c **      LSEG....LARGEST SEGMENT no if<>0
c **********************************************************************
c **********************************************************************

      SUBROUTINE LARGEST (XA,YA,NPPA,LSEG)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)
      integer*2 nppa,lseg

      integer*2 lastlup

      lastlup = 0

      call larges1 (XA,YA,NPPA,LSEG,lastlup)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  10. SUBROUTINE LARGES2
C **  purpose of subroutine: FIND LARGEST GOOD SEGMENT IN STRING, PREFER
c **                         STRAIGHT SEGMENT
c **    input:  
C **      RAD........Radius
C **      XA,YA......STRING of points
C **      NPPA.......NO OF POINTS
C **      XB,YB......STRING of points
C **      NPPB.......NO OF POINTS
C **      lastlup....same contour processed for the second time iff 1
C **    output:
c **      LSEG....LARGEST SEGMENT no if<>0
c **********************************************************************
c **********************************************************************

      SUBROUTINE LARGES2 (RAD,XA,YA,NPPA,XB,YB,NPPB,LSEG)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      integer*2 nppa,nppb,lseg,lastlup
      logical*2 distfl,lv101
      real*8 ver
      real*4 dx,dy,dd,dsq,factor,eps,rad,dist,dist2,dist3,xx,yy
      parameter (eps = 0.0025)

      integer*2 k,k1,idx

      idx = 169
      call getsc(idx,ver)
      lv101 = ver.gt.10.049
      distfl = .false.
      DD = 0.0
      K = 0
   21 K = K+1
      IF (K.GE.NPPB) GO TO 99
      IF (XB(K+1).EQ.FLARC) THEN
        DX = XB(K+4)-XB(K)
        DY = YB(K+4)-YB(K)
        K1 = K
        K = K+3
        FACTOR = 1.5
      ELSE
        DX = XB(K+1)-XB(K)
        DY = YB(K+1)-YB(K)
        K1 = K
        FACTOR = 1.0
        if (K .EQ. 1) FACTOR = 4.0
      END IF 
      DSQ = (DX*DX+DY*DY)*FACTOR
      if (K .EQ. 1 .AND. (DSQ .EQ. 0.0 .or. XB(K+1).EQ.FLARC)) then
        LSEG = K1
        goto 99
      endif
      IF (DSQ.GT.DD) THEN   
        xx = 0.5 * (XB(K+1) + XB(K))
        yy = 0.5 * (YB(K+1) + YB(K))
        call dsptlp0 (xx,yy,xa,ya,nppa,dist)
c
c.....Added more accuracy in distance checking.  Now the endpoints and
c.....midpoints will be used to get a more accurate distance - ASF 9/25/13
c
        IF (lv101 .and. dist .gt. 0.95*rad) THEN
          call dsptlp0 (XB(K),YB(K),xa,ya,nppa,dist2)
          IF (dist2 .gt. 0.95*rad) THEN
            call dsptlp0 (XB(K+1),YB(K+1),xa,ya,nppa,dist3)
            distfl = (dist3 .gt. 0.95*rad)
          ENDIF
        ENDIF
        if (dist .gt. 0.95*rad .and. (.not.lv101 .or. distfl)) THEN
            if (K1 .EQ. 2 .and. XB(K1+1).EQ.FLARC .and.
     x          XB(K1-1).EQ. XB(K1)) then
                LSEG = K1-1
                goto 99
            endif
            LSEG = K1
            DD = DSQ
            if (K .EQ. 1) goto 99
        ENDIF
      END IF
      GO TO 21

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE xccw
C **  purpose of subroutine: DETERMINE DIRECTION OF CLOSED OFFSET
c **    input:
C **      XA,YA...CLOSED STRING
C **      NPPA....NO OF STRING POINTS
C **    output:
c **      AREA....AREA
C **      KLOK....1- CCW, -1- CW
c **********************************************************************
c **********************************************************************

      SUBROUTINE xccw (XA,YA,NPPA,KLOK)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'com.com'
      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)
      real*4 area

      AREA = 0.0
      I = 0
      X1 = 0.0
      Y1 = 0.0
   22 I = I+1
      IF(I.GT.NPPA-1) GO TO 66
      IF (XA(I+1).EQ.FLARC) THEN
        CALL MIDARC (XA,YA,I,X2,Y2)
        X2 = X2-XA(1)
        Y2 = Y2-YA(1)
        AREA = AREA + 0.5*(X1*Y2 - X2*Y1)
        X1 = X2
        Y1 = Y2
        X2 = XA(I+4)-XA(1)
        Y2 = YA(I+4)-YA(1)
        AREA = AREA + 0.5*(X1*Y2 - X2*Y1)
        I = I+3
      ELSE
        X2 = XA(I+1) - XA(1)
        Y2 = YA(I+1) - YA(1)
        AREA = AREA + 0.5*(X1*Y2 - X2*Y1)
      END IF
      X1 = X2
      Y1 = Y2
      GO TO 22
   66 KLOK = -1
      IF (AREA.GT.0.0) KLOK = 1
      i=klok
      call xccw1 (xa,ya,nppa,i)
      if (i*klok .eq. -1) then
        klok=i
      endif

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE xccw1
C **  purpose of subroutine: DETERMINE DIRECTION OF CLOSED LOOP.
c **    input:
C **      XA,YA...CLOSED STRING
C **      NPPA....NO OF STRING POINTS
C **    output:
C **      KLOK....1- CCW, -1- CW, 0- could not determine
c **********************************************************************
c **********************************************************************

      SUBROUTINE xccw1 (XA,YA,NPPA,KLOK)

      include 'const.com'
      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa, klok

      real*4 a1,dx,dy,odx,ody,dx1,dy1,sec
      integer*2 i, k, k1, khld

      klok = 0
      if (nppa.lt.4) goto 99
      a1 = 0.0
c
c... Get start vector
c
      if (xa(2).eq.FLARC) then
        call tanarc (1,xa(4),ya(4),xa(3),ya(3),xa(1),ya(1),xa(5),ya(5),
     x               odx,ody)
        i = 0
        k = ya(3)
      else
        odx = xa(2)-xa(1)
        ody = ya(2)-ya(1)
        i = 1
        k = 0
      endif
      sec = sqrt(odx**2+ody**2)
      if (sec.gt.0.0) then
        odx = odx/sec
        ody = ody/sec
      endif
      dx1 = odx
      dy1 = ody
      k1 = k
c
c... Loop through string to determine if total angular change is +2PI or -2PI.
c
   10 i = i+1
      if(i.ge.nppa) go to 90
      if (xa(i+1).eq.FLARC) then
        khld = k
        k = ya(i+2)
        if (i.gt.1) then
c
c... get angle betwen start of arc & previous element
c
          call tanarc (1,xa(i+3),ya(i+3),xa(i+2),k,xa(i),ya(i),
     x                 xa(i+4),ya(i+4),dx,dy)
          sec = sqrt(dx**2+dy**2)
          if (sec.gt.0.0) then
            dx = dx/sec
            dy = dy/sec
          endif
          call addang (odx,ody,dx,dy,0,khld,k,a1)
          odx = dx
          ody = dy
        endif
        call tanarc (2,xa(i+3),ya(i+3),xa(i+2),k,xa(i),ya(i),
     x               xa(i+4),ya(i+4),dx,dy)
        I = I+3
      else
        khld = k
        k = 0
        dx = xa(i+1)-xa(i)
        dy = ya(i+1)-ya(i)
      endif
      sec = sqrt(dx**2+dy**2)
      if (sec.gt.0.0) then
        dx = dx/sec
        dy = dy/sec
      endif
c
c... If current element is a straight line, add in angle between it & previous
c... element. If current element is a circular arc, add in angular change
c... of arc.
c
      call addang (odx,ody,dx,dy,k,khld,k,a1)
      odx = dx
      ody = dy
      goto 10
   90 continue
c
c... Add in angle between last element & first element.
c
      call addang (dx,dy,dx1,dy1,0,k,k1,a1)

      if (a1 .ge. HALF_PI) then
        klok = 1
      else if (a1 .le. -HALF_PI) then
        klok = -1
      endif

   99 return
      end

c **********************************************************************
c **********************************************************************
c **  11. SUBROUTINE XCLOCK
C **  purpose of subroutine: DETERMINE DIRECTION OF CLOSED OFFSET
c **    input:
C **      XA,YA...CLOSED STRING
C **      NPPA....NO OF STRING POINTS
C **    output:
c **      AREA....AREA
C **      KLOK....1- CCW, -1- CW
c **********************************************************************
c **********************************************************************

      SUBROUTINE XCLOCK (XA,YA,NPPA,AREA,KLOK)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'com.com'
      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      AREA = 0.0
      I = 0
      X1 = 0.0
      Y1 = 0.0
   22 I = I+1
      IF(I.GT.NPPA-1) GO TO 66
      IF (XA(I+1).EQ.FLARC) THEN
        CALL MIDARC (XA,YA,I,X2,Y2)
        X2 = X2-XA(1)
        Y2 = Y2-YA(1)
        AREA = AREA + 0.5*(X1*Y2 - X2*Y1)
        X1 = X2
        Y1 = Y2
        X2 = XA(I+4)-XA(1)
        Y2 = YA(I+4)-YA(1)
        AREA = AREA + 0.5*(X1*Y2 - X2*Y1)
        I = I+3
      ELSE
        X2 = XA(I+1) - XA(1)
        Y2 = YA(I+1) - YA(1)
        AREA = AREA + 0.5*(X1*Y2 - X2*Y1)
      END IF
      X1 = X2
      Y1 = Y2
      GO TO 22
   66 KLOK = -1
      IF (AREA.GT.0.0) KLOK = 1
      i=klok
      if (sc(169).gt.8.229999) call KLOKIT(xa,ya,nppa,i)
      if (i.ne.klok) then
        klok=i
        area = -area
      endif

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  11A. SUBROUTINE KLOKIT
C **  purpose of subroutine: DETERMINE DIRECTION OF CLOSED LOOP.
c **    input:
C **      XA,YA...CLOSED STRING
C **      NPPA....NO OF STRING POINTS
C **    output:
C **      KLOK....1- CCW, -1- CW
c **********************************************************************
c **********************************************************************

      SUBROUTINE KLOKIT (XA,YA,NPPA,KLOK)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa, klok

      real*4 a1,dx,dy,odx,ody,dx1,dy1,sec
      integer*2 i, k, k1, khld
      logical larc

      larc = .FALSE.
      klok = -1
      if (nppa.lt.4) goto 99
      a1 = 0.0
c
c... Get start vector
c
      if (xa(2).eq.FLARC) then
        call tanarc (1,xa(4),ya(4),xa(3),ya(3),xa(1),ya(1),xa(5),ya(5),
     x               odx,ody)
        i = 0
        k = ya(3)
        larc = .TRUE.
      else
        odx = xa(2)-xa(1)
        ody = ya(2)-ya(1)
        i = 1
        k = 0
      endif
      sec = sqrt(odx**2+ody**2)
      if (sec.gt.0.0) then
        odx = odx/sec
        ody = ody/sec
      endif
      dx1 = odx
      dy1 = ody
      k1 = k
c
c... Loop through string to determine if total angular change is +2PI or -2PI.
c
   10 i = i+1
      if(i.ge.nppa) go to 90
      if (xa(i+1).eq.FLARC) then
        khld = k
        k = ya(i+2)
        larc = .TRUE.
        if (i.gt.1) then
c
c... get angle betwen start of arc & previous element
c
          call tanarc (1,xa(i+3),ya(i+3),xa(i+2),k,xa(i),ya(i),
     x                 xa(i+4),ya(i+4),dx,dy)
          sec = sqrt(dx**2+dy**2)
          if (sec.gt.0.0) then
            dx = dx/sec
            dy = dy/sec
          endif
          call addang (odx,ody,dx,dy,0,khld,k,a1)
          odx = dx
          ody = dy
        endif
        call tanarc (2,xa(i+3),ya(i+3),xa(i+2),k,xa(i),ya(i),
     x               xa(i+4),ya(i+4),dx,dy)
        I = I+3
      else
        khld = k
        k = 0
        dx = xa(i+1)-xa(i)
        dy = ya(i+1)-ya(i)
      endif
      sec = sqrt(dx**2+dy**2)
      if (sec.gt.0.0) then
        dx = dx/sec
        dy = dy/sec
      endif
c
c... If current element is a straight line, add in angle between it & previous
c... element. If current element is a circular arc, add in angular change
c... of arc.
c
      call addang (odx,ody,dx,dy,k,khld,k,a1)
      odx = dx
      ody = dy
      goto 10
   90 continue
c
c... Add in angle between last element & first element.
c
      call addang (dx,dy,dx1,dy1,0,k,k1,a1)
c
c... For self-intersected boundary loop, angle checking logic will not work
c
c     if (a1.gt.0.0) klok = 1
      if ((a1.gt.0.001 .and. .not. larc) .or. 
     x    (a1.gt.-1.e-6 .and. larc)) klok = 1
     
   99 return
      end

c **********************************************************************
c **********************************************************************
c **  11B. SUBROUTINE ADDANG
C **  purpose of subroutine: Determine the angle between 2 unit vectors
C **                         & add to total
c **    input:
C **      dx1,dy1  - First vector
C **      dx2,dy2  - Second vector
C **      klok     - If vectors are start & end of circle, direction of circle
C **                 else 0
C **      klok1    - If last element was circle, its direction, else 0.
C **      klok2    - If next element is circle, its direction, else 0.
C **      A1       - Current total angle
C **    output:
C **      A1       - New total angle
c **********************************************************************
c **********************************************************************

      SUBROUTINE ADDANG (dx1,dy1,dx2,dy2,klok,klok1,klok2,a1)

      include 'const.com'

      real*4 dx1,dy1,dx2,dy2,a1
      integer*2 klok,klok1,klok2

      real*4 si,co,alf

c
c... Get angle in -PI/2 to +PI/2 range
c
      si = dx1*dy2-dy1*dx2
      if (si.gt.1.0) si = 1.0
      if (si.lt.-1.0) si = -1.0
      alf = asin(si)
c
c... If cosine of angle is negative, angle is obtuse
c
      co = dx1*dx2+dy1*dy2
      if (co.lt.0.0) then
        if (abs(si).lt..0001) then
c
c... Angle is +PI or -PI
c... If the last item was a circle, angle is in opposite direction to circle
c
          if (klok1.ne.0) then
            alf = -PI*klok1
c
c... If the this item is a circle, angle is in opposite direction to circle
c
          else if (klok2.ne.0) then
            alf = -PI*klok2
c
c... Both items are lines, discard this line & return angle of 0.
c
          else
            dx2 = dx1
            dy2 = dy1
          endif
c
c... Get angle in range -PI to -PI/2 or PI/2 to PI as appropriate.
c
        else if (si.lt.0.0) then
          alf = -alf-PI
        else if (si.gt.0.0) then
          alf = PI-alf
        endif
      endif
c
c... If angle is opposite to direction of circle, convert
c
      if (alf*klok.lt.0.0) then
        if (alf.gt.0.0) then
          alf = alf-TWO_PI
        else
          alf = TWO_PI+alf
        endif
      endif
      a1 = a1+alf

   99 return
      end

c **********************************************************************
c **********************************************************************
c **  13. SUBROUTINE LITARC
C **  purpose of subroutine: ELIMINATE VERY LITTLE ARCS
c **********************************************************************
c **********************************************************************

      SUBROUTINE LITARC (XA,YA,NPPA)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      TOLARC = TOL*10.
      NPP = NPPA
      LIT = 0
      IF (NPP.LT.5) GO TO 99
      K = 0
   17 K = K+1
      IF (K.GE.NPP-3) GO TO 37
      IF (XA(K+1).EQ.FLARC) THEN
        DX = XA(K+4)-XA(K)
        DY = YA(K+4)-YA(K)
        SQ = DX*DX+DY*DY
        IF (SQ.LT.TOLARC) THEN
          LIT = 1
          DO 27 I = K+1, NPP-3
            XA(I) = XA(I+3)
            YA(I) = YA(I+3)
   27     CONTINUE
          NPP = NPP-3
        END IF
      END IF
      GO TO 17
   37 NPPA = NPP

   99 RETURN
      END


c **********************************************************************
c **********************************************************************
c **  14. SUBROUTINE MIDARC
C **  purpose of subroutine: FIND A POINT IN MID ARC
c **    input:
C **      XA,YA......ARRAY
C **      NSEQ.......ARC LOCATION IN ARRAY
C **    output:
c **     XMID,YMID...MID POINT
c **********************************************************************
c **********************************************************************

      SUBROUTINE MIDARC (XA,YA,NSEQ,XMID,YMID)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      USEG = 0.5
      R = XA(NSEQ+2)
      KLOK = YA(NSEQ+2)
      XC = XA(NSEQ+3)
      YC = YA(NSEQ+3)
      CALL XYARC (R,KLOK,XC,YC,
     +     XA(NSEQ),YA(NSEQ),XA(NSEQ+4),YA(NSEQ+4),USEG,XMID,YMID)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  15. SUBROUTINE INARC
C **  purpose of subroutine: FIND IF POINT IS WITHIN ARC
c **    input:
C **      XC,YC,R.......CENTER AND RADIUS OF ARC
C **      X1,Y1,X2,Y2...FIRST AND LAST POINTS OF ARC
C **      KLOK..........1- CCW, -1- CW
C **      XI,YI.........POINT ON CIRCLE
C **    output:
c **      IN............1- IN ARC, 0- OUTSIDE ARC
c **********************************************************************
c **********************************************************************

      SUBROUTINE INARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,XI,YI,IN)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      IN = 0
      EPS = tol/254.

C --- CIRCLE
      IF (X1.EQ.X2 .AND. Y1.EQ.Y2) THEN
        IN = 1
        GOTO 99
      ENDIF

C --- ON END
      IF (ABS(X1-XI).LT.EPS .AND. ABS(Y1-YI).LT.EPS) THEN
        IN = 1
        GO TO 99
      END IF
      IF (ABS(X2-XI).LT.EPS .AND. ABS(Y2-YI).LT.EPS) THEN
        IN = 1
        GOTO 99
      ENDIF
      CV1 = (X1-XC)*(YI-YC) - (XI-XC)*(Y1-YC)
      CV2 = (XI-XC)*(Y2-YC) - (X2-XC)*(YI-YC)
      KV1 = 0
      IF (CV1.LT.0) KV1 = -1
      IF (CV1.GT.0) KV1 =  1
      KV2 = 0
      IF (CV2.LT.0) KV2 = -1
      IF (CV2.GT.0) KV2 =  1

C --- ONE 180 DEG
      IF (KV1.EQ.0 .OR. KV2.EQ.0) THEN
        IF (KV1.EQ.0 .AND. KV2.EQ.KLOK) IN = 1
        IF (KV2.EQ.0 .AND. KV1.EQ.KLOK) IN = 1
        GO TO 99
      ENDIF

C --- SAME DIRECTION VECTORS, ELSE- OPPOSITE DIRECTIONS
      IF (KV1+KV2. NE.0) THEN
        IF (KV1.EQ.KLOK) IN = 1
      ELSE
        CV  = (X1-XC)*(Y2-YC) - (X2-XC)*(Y1-YC)
        IF (CV.LT.0.0 .AND. KLOK.GT.0) IN = 1
        IF (CV.GT.0.0 .AND. KLOK.LT.0) IN = 1
      END IF

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  16. SUBROUTINE BOXARC
C **  subroutine: FIND THE BOX TO ENCLOSE ARC
c **    input:
C **      XC,YC,R.......CENTER AND RADIUS OF ARC
C **      X1,Y1,X2,Y2...FIRST AND LAST POINTS OF ARC
C **      KLOK..........1- CCW, -1- CW
C **    output:
c **      XL,YL,XG,YG...BOX POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE BOXARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,XL,YL,XG,YG)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      real*4 X(6),Y(6)

      X(1) = X1
      Y(1) = Y1
      X(2) = X2
      Y(2) = Y2
      INS = 2
      X(3) = XC + R
      Y(3) = YC
      X(4) = XC
      Y(4) = YC + R
      X(5) = XC - R
      Y(5) = YC
      X(6) = XC
      Y(6) = YC - R
      DO 15 I = 3, 6
        CALL INARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,X(I),Y(I),IN)
        IF (IN.EQ.1)THEN
          INS = INS + 1
          X(INS) = X(I)
          Y(INS) = Y(I)
        END IF
   15 CONTINUE
      XL =  1000000000000.0
      YL =  1000000000000.0
      XG = -1000000000000.0
      YG = -1000000000000.0
      DO 60 I = 1, INS
        IF (XL .GT. X(I)) XL = X(I)
        IF (YL .GT. Y(I)) YL = Y(I)
        IF (XG .LT. X(I)) XG = X(I)
        IF (YG .LT. Y(I)) YG = Y(I)
   60 CONTINUE

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE CHKARC
C **  purpose of subroutine: FIND WHETHER A CIRCLE INTERSECTS
c **                         ARC XY WHEN MOVING ALONG SEGMENT AB
c **    input:
C **      aa0,bb0,aa1,bb1 ... SEGMENT AB ENDPOINTS
C **      xx0,yy0,xx1,yy1 ... ARC XY ENDPOINTS
C **      crad            ... circle radius
C **      rad             ... arc XY radius
C **      xc,yc           ... arc XY center
C **      klok            ... arc XY orientation
C **    output:
c **      ierr .........  0, iff no intersection
c **                      1, iff intersection
c **********************************************************************
c **********************************************************************
      SUBROUTINE CHKARC (aa0,bb0,aa1,bb1,crad,xx0,yy0,xx1,yy1,
     x                                        rad,xc,yc,klok,ierr)

      include 'pokcom.com'

      real*8 xx0,yy0,xx1,yy1
      real*4 aa0,bb0,aa1,bb1,crad
      real*4 xc,yc,rad
      integer*2 ierr,klok

      real*4 size
      real*4 x0,y0,x1,y1,XINT,YINT

      ierr = 0
      x0 = xx0
      x1 = xx1
      y0 = yy0
      y1 = yy1
c
c..... the 'if' below is a QAR 92311 fix
c
      if (crad .gt. tol) then
        if ((aa0-x0)**2 + (bb0-y0)**2 .lt. crad**2) then
          ierr = 1
          return
        endif
        a0 = aa0 + crad
        a1 = a0
        b0 = bb0
        b1 = b0
        CALL ARCARC (2,1,aa0,bb0,crad,1,a0,b0,a1,b1,
     +             xc,yc,rad,klok,x0,y0,x1,y1,XINT,YINT,ierr)
        if (ierr.eq.1) return
      endif

      va = aa1 - aa0
      vb = bb1 - bb0

      SIZE = sqrt (va*va + vb*vb)

      if (size .lt. tol/2.) return

      if (crad .gt. tol .and. size .gt. tol) then
        if ((aa1-x0)**2 + (bb1-y0)**2 .lt. crad**2) then
          ierr = 1
          return
        endif
        a0 = aa1 + crad
        a1 = a0
        b0 = bb1
        b1 = b0
        CALL ARCARC (2,1,aa1,bb1,crad,1,a0,b0,a1,b1,
     +             xc,yc,rad,klok,x0,y0,x1,y1,XINT,YINT,ierr)
        if (ierr.eq.1) return
      endif

      ua = - vb/size
      ub = va/size

      do 10 i = 1,2
        if (i.eq.2) then
          if (crad .le. tol/2.) return
          ua = -ua
          ub = -ub
        endif
        a0 = aa0 + crad*ua
        a1 = aa1 + crad*ua
        b0 = bb0 + crad*ub
        b1 = bb1 + crad*ub

        CALL ARCSEG (2,0,XC,YC,rad,klok,
     +                         x0,y0,x1,y1,a0,b0,a1,b1,XINT,YINT,ierr)
        if (ierr.eq.1) return
10    continue
c
c.....Added another check along the original segment for case where arc is
c.....smaller than the circle.  In this case, the island could be missed if
c.....it is between the two segments defined in the loop above - ASF 10/29/13.
c.......Note that this could still miss small enough islands so additional
c.......checks may need to be added later for more accuracy.
c
      CALL ARCSEG (2,0,XC,YC,rad,klok,
     +                       x0,y0,x1,y1,aa0,bb0,aa1,bb1,XINT,YINT,ierr)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE CHKRC1
C **  purpose of subroutine: FIND WHETHER A CIRCLE INTERSECTS an arc
c **
c **    input:
C **      a0,b0           ... circle center
C **      crad            ... circle radius
C **      xx0,yy0,xx1,yy1 ... arc XY ENDPOINTS
C **      r               ... arc radius
C **    output:
c **      ierr .........  0, iff no intersection
c **                      1, iff intersection
c **********************************************************************
c **********************************************************************
      SUBROUTINE CHKRC1 (a0,b0,crad,xx0,yy0,xx1,yy1,r,xc,yc,klok,ierr)

      include 'pokcom.com'

      real*8 xx0,yy0,xx1,yy1
      real*4 a0,b0,crad,r
      integer*2 ierr

      real*4 d0,d1
      real*4 x1,y1,x40,y40,x41,y41
      integer*2 in,nop

      ierr = 0
      if (crad .le. tol/2.) return

      va = a0 - xx0
      vb = b0 - yy0

      d0 = sqrt (va*va + vb*vb)
      if (d0 + tol/2. .lt. crad) then
        ierr = 1
        return
      endif

      ua = xx1 - a0
      ub = yy1 - b0

      d1 = sqrt (ua*ua + ub*ub)
      if (d1 + tol/2. .lt. crad) then
        ierr = 1
        return
      endif

      CALL CIRCIR (a0,b0,crad,XC,YC,R,NOP,X1,Y1,X2,Y2)
      if (nop .lt. 2) return

      x40 = xx0
      y40 = yy0
      x41 = xx1
      y41 = yy1

      CALL INARC (XC,YC,R,KLOK,x40,y40,x41,y41,X1,Y1,IN)
      if (in.eq.1) then
        ierr = 1
        return
      else
        CALL INARC (XC,YC,R,KLOK,x40,y40,x41,y41,X2,Y2,IN)
        if (in.eq.1) ierr = 1
      endif

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE lcfind (XA,YA,NPPA,LSEG,UEG,xl,yl,IERR)
C **  purpose of subroutine: FIND a POINT ON a CONTOUR
c **    input:
C **      XA,YA,NPPA...contour
C **      XL,YL........COORDINATES OF POINT
C **    output:
c **      LSEG.........SEGMENT OF ENDPOINT
C **      UEG..........RELATIVE LOCATION OF ENDPOINT
C **      IERR.........0 - OK, 9 - ERROR - ENDPOINT TOO FAR
c **********************************************************************
c **********************************************************************
      SUBROUTINE lcfind (XA,YA,NPPA,LSEG,UEG,xl,yl,IERR)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),UEG,xl,yl
      integer*2 lseg,nppa,ierr

      real*4 xn,yn,xn1,yn1,dist,dist1,r,u
      integer*2 index,k,klok

      IERR = 0
      LSEG = 0
      UEG = 0.0
      XN = 0.0
      YN = 0.0
      INDEX = 1
      DIST = 10000000.0
      K = 0
   21 K = K+1
      IF (K.GE.NPPA) GOTO 90
      IF (XA(K+1).EQ.FLARC) THEN
        R = XA(K+2)
        KLOK = YA(K+2)
        CALL DSPTARC (xl,yl,R,KLOK,XA(K+3),YA(K+3),
     +        XA(K),YA(K),XA(K+4),YA(K+4),DIST1,XN1,YN1,U)

        IF (U.ge.0.0.AND.U.le.1.0.AND.DIST1.LT.DIST) THEN
          DIST = DIST1
          LSEG = K
          UEG = U
          XN = XN1
          YN = YN1
        ENDIF
        K = K+3
      ELSE
        CALL DSPTLN (xl,yl,XA(K),YA(K),XA(K+1),YA(K+1),
     +        DIST1,XN1,YN1)
        CALL USEG (XA(K),YA(K),XA(K+1),YA(K+1),XN1,YN1,U)

        IF (U.ge.0.0.AND.U.le.1.0.AND.DIST1.LT.DIST) THEN
          DIST = DIST1
          LSEG = K
          UEG = U
          XN = XN1
          YN = YN1
        ENDIF
      ENDIF
      GOTO 21
   90 IF (DIST.GT.1000000.0) IERR = 9

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE defold
C **  purpose of subroutine: REMOVES offset folds
c **    input/output:
C **      XA,YA...STRING OF POINTS
C **      NPPA....NUMBER OF POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE defold (XA,YA,NPPA)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts),xb(maxpts),yb(maxpts)

      ichg = 0

      nppb = nppa
      do k = 1,nppa
        xb(k) = xa(k)
        yb(k) = ya(k)
      enddo

      CALL LARGEST (xb,yb,nppa,lseg)
      ICEN = 1
      USEG = 0.5
      CALL SHUFFLE (xb,yb,nppb,LSEG,ICEN,USEG,xmm,ymm)
      CALL DOUBLE (xb,yb,nppb)

5     k = 0

10    k = k+1
      if (k .gt. nppb-2) goto 20

      if (xb(k+1) .eq. flarc) then
        k = k+3
        goto 10
      endif

      if (xb(k+2) .eq. flarc) goto 10

      dx1 = xb(k+1) - xb(k)
      dy1 = yb(k+1) - yb(k)
      d1 = sqrt (dx1*dx1 + dy1*dy1)
      if (d1 .lt. tol) goto 10
      ux1 = dx1/d1
      uy1 = dy1/d1

      dx2 = xb(k+2) - xb(k+1)
      dy2 = yb(k+2) - yb(k+1)
      d2 = sqrt (dx2*dx2 + dy2*dy2)
      if (d2 .lt. tol) goto 10
      ux2 = dx2/d2
      uy2 = dy2/d2

      co = ux1*ux2 + uy1*uy2
      if (co .lt. -0.99) then
        d = d1
        if (d2 .lt. d1) d = d2
        si = 1 - co*co
        if (si .gt. 0) si = sqrt(si)
        h = d*si
        if (h .lt. 5*tol) then

        nppb = nppb-1
        do j = k+1,nppb
          xb(j) = xb(j+1)
          yb(j) = yb(j+1)
        enddo
        ichg = 1
        goto 5
        endif
      endif

      if (k .lt. nppb-2) goto 10

20    continue

      if (ichg .eq. 1) then
        xl = xa(1)
        yl = ya(1)
        call lcfind (xb,yb,nppb,lseg,useg,xl,yl,ierr)

        if (ierr.eq.0) then
          ICEN = 1
          CALL SHUFFLE (xb,yb,nppb,lseg,ICEN,useg,xl,yl)
        endif

        CALL DOUBLE (xb,yb,nppb)
        nppa = nppb
        do k = 1,nppb
          xa(k) = xb(k)
          ya(k) = yb(k)
        enddo

      endif

      RETURN
      END
