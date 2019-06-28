C*************************************************************************
C**    NAME           : off09.f
C**       CONTAINS:
C**  SUBROUTINE TURNED (DX1,DY1,DX2,DY2,STEP,STEPIT)
C**  SUBROUTINE INLOOP (XP, YP, XA, YA, NPPA, IN, IERR)
C**  SUBROUTINE lpinside
C**  SUBROUTINE inlup1
C**  SUBROUTINE BRKARC (r,klok,xc,yc,x1,y1,x2,y2,xt,yt,npt)
C**  SUBROUTINE DSPTLN (XP,YP,X1,Y1,X2,Y2,DIST,XN,YN)
C**  SUBROUTINE ARCSEG (MOVE,NOEND,XC,YC,R,KLOK,XA1,YA1,XA2,YA2
c**  x                 ,XL1,YL1,XL2,YL2,XINT,YINT,INTER)
C**  SUBROUTINE CIRCIR (XC1,YC1,R1,XC2,YC2,R2,NOP,X1,Y1,X2,Y2)
C**  SUBROUTINE ARCARC (MOVE,NOEND,XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2
C**  x                 ,XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,XINT,YINT,INTER)
C**  SUBROUTINE USEG (X1,Y1,X2,Y2,XU,YU,U)
C**  SUBROUTINE LINEX (IT,X1,Y1,X2,Y2,X3,Y3,X4,Y4,XINT,YINT,IER)
C**  SUBROUTINE FLIPO (XA,YA,NPPA)
C**  SUBROUTINE ENDPTS (KEND,ISIDE,RR,XA,YA,NPPA,X1,Y1,IER)
C**  SUBROUTINE TANARC (KEND,XC,YC,R,KLOK,X1,Y1,X2,Y2,UX,UY)
C**  SUBROUTINE DSPTARC (XP,YP,R,KLOK,XC,YC,X1,Y1,X2,Y2,DIST,XN,YN,U)
C**  SUBROUTINE UARC (R,KLOK,XC,YC,X1,Y1,X2,Y2,X3,Y3,U)
C**  SUBROUTINE XYARC (R,KLOK,XC,YC,X1,Y1,X2,Y2,U,X3,Y3)
C**  SUBROUTINE XYSEG (X1,Y1,X2,Y2,U,XU,YU)
C**  SUBROUTINE CHKSEG (aa0,bb0,aa1,bb1,crad,xx0,yy0,xx1,yy1,ierr)
C**  SUBROUTINE CHKSG1 (a0,b0,crad,xx0,yy0,xx1,yy1,ierr)
C**  SUBROUTINE flook
C**  SUBROUTINE flook1
C**  SUBROUTINE islandinside
C**  SUBROUTINE entrypntcut
C**  SUBROUTINE entrypntoff
C**
C**    MODULE NAME AND RELEASE LEVEL
C**       off09.f , 25.1
C**    DATE AND TIME OF LAST  MODIFICATION
C**       04/29/15 , 15:10:21
C***********************************************************************
c**
c** copyright (c) 2003 NCCS
c**
c **********************************************************************
c **********************************************************************
c **  SUBROUTINE TURNED
c **  purpose of subroutine: computes maximum step over per corner
c **    input:
c **      dx1,dy1...vector direction of side one
c **      dx2,dy2...vector direction of side two
c **      step......effective tool diameter value
c **    output:
c **      stepit....smallest step over for that corner angle
c **********************************************************************
c **********************************************************************

      SUBROUTINE TURNED (DX1,DY1,DX2,DY2,STEP,STEPIT)
      real*4 cosa, s, dx1,dx2, dy1,dy2, step,stepit

      cosa = -dx1*dx2 - dy1*dy2
      if (cosa .gt. 1.) cosa = 1.
      if (cosa .lt. -1.) cosa = -1.
c
c..... Let s be sin(a/2)**2
c
      s = (1. - cosa)*.5
      s = .5*step*(1. + sqrt (s))
      if (s .lt. stepit) stepit = s

      return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE INLOOP
c **  purpose of subroutine: find if point is inside loop
c **    input:
c **      xp,yp........point in question
c **      xa,ya,nppa...lane/loop array and size
c **    output:
c **      in...........0- if outside loop, 1- if inside
c **      ierr.........0- ok 1- over maximum points
c **********************************************************************
c **********************************************************************

      SUBROUTINE INLOOP (XP, YP, XA, YA, NPPA, IN, IERR)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 xa(maxpts), ya(maxpts), xb(maxpts), yb(maxpts)

      ierr = 0

      call evolvlp (xa,ya,nppa,xb,yb,nppb,ierr)
      if (ierr .ne. 0) goto 99

      call inlup1 (xp,yp,xb,yb,nppb,in)

99    return
      end

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE lpinside
c **  purpose of subroutine: find if loop B is inside loop A
c **    input:
c **      inda ........ loop A index
c **      indb ........ loop B index
c **    output:
c **      xb,yb,nppb...loop B
c **      in...........0- if outside loop, 1- if inside
c **********************************************************************
c **********************************************************************

      SUBROUTINE lpinside (indb,xb,yb,nppb,inda,in)

      include 'pokcom.com'

      real*4 xb(maxpts),yb(maxpts),xa(maxpts),ya(maxpts)
      real*4 xpt,ypt
      integer*2 nppb,nppa,inda,indb,in,ins

      ierr = 0
      in = 0

      call getlan (indb,xb,yb,nppb)
      xpt = xb(1)
      ypt = yb(1)

      call getlan (inda,xa,ya,nppa)
      call inloop (xpt, ypt, xa, ya, nppa, ins, ierr)
      if (ierr .ne. 0) goto 99

      in = ins

99    return
      end

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE inlup1
c **  purpose of subroutine: find if point is inside a polyline loop
c **    input:
c **      xp,yp........point in question
c **      xb,yb,nppb...lane/loop array and size
c **    output:
c **      in...........0- if outside loop, 1- if inside
c **********************************************************************
c **********************************************************************

      SUBROUTINE inlup1 (xp, yp, xb, yb, nppb, in)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'const.com'
      include 'pokcom.com'

      real*4 xp,yp,xb(maxpts),yb(maxpts)
      integer*2 nppb,in

      real*4 xn,yn,eps,dist,dx1,dy1,ang1,dx2,dy2,ang2,dang,adang,angtot
      integer*2 k

      eps = tol/2.54

      angtot = 0.
      dx1 = xb(1)-xp
      dy1 = yb(1)-yp
      ang1 = atan2(dy1,dx1)
      if (ang1 .lt. 0.) ang1 = TWO_PI + ang1
      do 45 k=2,nppb
        dx2 = xb(k)-xp
        dy2 = yb(k)-yp
        ang2 = atan2(dy2,dx2)
        if (ang2 .lt. 0.) ang2 = TWO_PI + ang2
        dang = ang2-ang1

        adang = abs(dang)
        if (abs(adang - PI) .lt. eps) then
          call dsptln (xp,yp,xb(k-1),yb(k-1),xb(k),yb(k),dist,xn,yn)
          if (dist .lt. tol) then
            in = 0
            return
          endif
        endif

        if (adang .gt. PI) then
          if (dang .lt. 0.) then
            dang = TWO_PI + dang
          else if (dang .gt. 0.) then
            dang = dang - TWO_PI
          endif
        endif
        angtot = angtot+dang
        ang1 = ang2
45    continue
      in = 1
      if (abs(angtot) .lt. eps) in = 0

99    return
      end

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE BRKARC
c **  purpose of subroutine: breaks an arc into straight line segments
c **    input:
c **      r,klok........arc radius and direction (1 if ccw, -1 if cw)
c **      xc,yc.........arc center point
c **      x1,y1,x2,y2...arc end points
c **    output:
c **      xt,yt,npt.....line segment arrays and number of points
c **********************************************************************
c **********************************************************************

      SUBROUTINE BRKARC (r,klok,xc,yc,x1,y1,x2,y2,xt,yt,npt)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'const.com'
      include 'pokcom.com'

      real*4 xt(30), yt(30)

      if (r.lt.tol*PI) then
        xt(1) = x1
        yt(1) = y1
        xt(2) = x2
        yt(2) = y2
        npt = 2
        goto 99
      endif
      npt = 0
      cosa = 2.*((r-tol)/r)**2 -1.0
      alph = acos (cosa)
      dx = x1-xc
      dy = y1-yc
      ang1 = atan2 (dy,dx)
      dx = x2-xc
      dy = y2-yc
      ang2 = atan2 (dy,dx)
      if ((ang2-ang1)*klok.lt.0.0) ang2 = TWO_PI*klok + ang2
      ang = ang2 - ang1
      num = abs(ang/alph) + 1
      if (num.gt.29) num = 29
      if (num.lt.4) num = 4
      dang = ang/num
      xt(1) = x1
      yt(1) = y1
      ang = ang1
      do 36 k = 2,num
        ang = ang+dang
        dx = cos(ang)
        dy = sin(ang)
        xt(k) = xc+r*dx
        yt(k) = yc+r*dy
36    continue
      npt = num+1
      xt(npt) = x2
      yt(npt) = y2

99    return
      end

c **********************************************************************
c **********************************************************************
c **  17. SUBROUTINE DSPTLN
C **  purpose of subroutine: DISTANCE POINT TO LINE
c **    input:
C **      XP,YP..........COORDINATES OF POINT
C **      X1,Y1  X2,Y2...ENDS OF LINE
c **    output:
C **      DIST...........DISTANCE POINT TO LINE
C **      XN,YN..........CLOSEST POINT ON LINE TO XP,YP
C **      U..............RELATIVE LOCATION
c **********************************************************************
c **********************************************************************

      SUBROUTINE DSPTLN (XP,YP,X1,Y1,X2,Y2,DIST,XN,YN)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      DX = X2-X1
      DY = Y2-Y1
      S2 = DX*DX + DY*DY
      IF (S2.LT.1E-7) GO TO 15
      GO TO 17
   15 DIST = SQRT((XP-X1)*(XP-X1) + (YP-Y1)*(YP-Y1))
      GO TO 21
   17 DIST = ABS((XP-X1)*DY + (YP-Y1)*(-DX)) / SQRT(S2)
      U = ((XP-X1)*DX + (YP-Y1)*DY) / S2
   21 XN = X1 + U*(X2-X1)
      YN = Y1 + U*(Y2-Y1)

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  18. SUBROUTINE ARCSEG
C **  purpose of subroutine: INTERSECTION BETWEEN ARC AND SEGMENT
c **    input:
C **      MOVE..............TOOL MOVES ON 1- ARC, 2- SEGMENT
C **      NOEND.............0- ALLOW ENDS INTERSECTION, 1- DONT ALLOW
C **      XC,YC,R,KLOK......CENTER, RADIUS AND DIRECTION OF ARC
C **      XA1,YA1,XA2,YA2...FIRST AND LAST POINTS OF ARC
C **      XL1,YL1,XL2,YL2...POINTS OF SEGMENT
C **    output:
c **      XINT,YINT.........INTERSECTION POINT
C **      INTER.............0- NO INTERSECION, 1- INTERSECTION
c **********************************************************************
c **********************************************************************

      SUBROUTINE ARCSEG (MOVE,NOEND,XC,YC,R,KLOK,XA1,YA1,XA2,YA2,
     x                   XL1,YL1,XL2,YL2,XINT,YINT,INTER)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      EPS = tol/2.54
      INTER = 0

C --- FIND DISTANCE CENTER TO SEG
      CALL DSPTLN (XC,YC,XL1,YL1,XL2,YL2,DIST,XN,YN)

C --- NO INTERSECTION
      IF (DIST.GT.R) GO TO 99

C --- LINE TANGENT
      IF (ABS(DIST-R).LT.EPS) THEN
        XINT = XN
        YINT = YN
        IF (NOEND.EQ.1) THEN
          IF (ABS(XL1-XN).LT.EPS.AND.ABS(YL1-YN).LT.EPS) GO TO 99
          IF (ABS(XL2-XN).LT.EPS.AND.ABS(YL2-YN).LT.EPS) GO TO 99
          IF (ABS(XA1-XN).LT.EPS.AND.ABS(YA1-YN).LT.EPS) GO TO 99
          IF (ABS(XA2-XN).LT.EPS.AND.ABS(YA2-YN).LT.EPS) GO TO 99
        END IF
        CALL USEG (XL1,YL1,XL2,YL2,XINT,YINT,U)
        IF (U.LT.0.0 .OR. U.GT.1.0) GO TO 99
        CALL INARC (XC,YC,R,KLOK,XA1,YA1,XA2,YA2,XINT,YINT,IN)
        IF (IN.EQ.0) GO TO 99
        INTER = 1
        GO TO 90
      END IF

C --- LINE CUTS CENTER
      IF (DIST.LT.EPS) THEN
        DX1 = XL2-XL1
        DY1 = YL2-YL1
        AS = SQRT (DX1*DX1 + DY1*DY1)
        DX1 = DX1/AS
        DY1 = DY1/AS
        XDD = DX1*R
        YDD = DY1*R
        X2 = XC + XDD
        Y2 = YC + YDD
        X1 = XC - XDD
        Y1 = YC - YDD
        GO TO 21
      END IF

C --- LINE CUTS ARC TWICE
      DX1 = (XN-XC)/DIST
      DY1 = (YN-YC)/DIST
      AS = SQRT(R*R-DIST*DIST)
      X1 = DY1*AS
      Y1 = DX1*AS
      X2 = XN + X1
      Y2 = YN - Y1
      X1 = XN - X1
      Y1 = YN + Y1
   21 IN1 = 0
      IF (NOEND.EQ.1) THEN
        IF (ABS(XL1-X1).LT.EPS.AND.ABS(YL1-Y1).LT.EPS) GO TO 25
        IF (ABS(XL2-X1).LT.EPS.AND.ABS(YL2-Y1).LT.EPS) GO TO 25
        IF (ABS(XA1-X1).LT.EPS.AND.ABS(YA1-Y1).LT.EPS) GO TO 25
        IF (ABS(XA2-X1).LT.EPS.AND.ABS(YA2-Y1).LT.EPS) GO TO 25
      END IF
      CALL USEG (XL1,YL1,XL2,YL2,X1,Y1,U1)
      IF (U1.LT.0.0 .OR. U1.GT.1.0) GO TO 25
      CALL INARC (XC,YC,R,KLOK,XA1,YA1,XA2,YA2,X1,Y1,IN1)
   25 IN2 = 0
      IF (NOEND.EQ.1) THEN
        IF (ABS(XL1-X2).LT.EPS.AND.ABS(YL1-Y2).LT.EPS) GO TO 27
        IF (ABS(XL2-X2).LT.EPS.AND.ABS(YL2-Y2).LT.EPS) GO TO 27
        IF (ABS(XA1-X2).LT.EPS.AND.ABS(YA1-Y2).LT.EPS) GO TO 27
        IF (ABS(XA2-X2).LT.EPS.AND.ABS(YA2-Y2).LT.EPS) GO TO 27
      END IF
      CALL USEG (XL1,YL1,XL2,YL2,X2,Y2,U2)
      IF (U2.LT.0.0 .OR. U2.GT.1.0) GO TO 27
      CALL INARC (XC,YC,R,KLOK,XA1,YA1,XA2,YA2,X2,Y2,IN2)
   27 IF (IN1.EQ.0 .AND. IN2.EQ.0) GO TO 99
      INTER = 1

C --- ONE POINT WITHIN ARC/SEG
      IF (IN1.EQ.1 .AND. IN2.EQ.0) THEN
        XINT = X1
        YINT = Y1
        GO TO 90
      END IF
      IF (IN2.EQ.1 .AND. IN1.EQ.0) THEN
        XINT = X2
        YINT = Y2
        GO TO 90
      END IF

C --- TWO POINTS WITHIN ARC/SEG, MOVING ON SEG
      IF (MOVE.EQ.2) THEN
        IF (U1.LT.U2) THEN
          XINT = X1
          YINT = Y1
        ELSE
          XINT = X2
          YINT = Y2
        END IF
        GO TO 90
      END IF

C --- TWO POINTS WITHIN ARC/SEG, MOVING ON ARC
      IF (ABS(X1-XA2).LT.EPS.AND.ABS(Y1-YA2).LT.EPS) THEN
        XINT = X2
        YINT = Y2
        GO TO 90
      END IF
      IF (ABS(X2-XA2).LT.EPS.AND.ABS(Y2-YA2).LT.EPS) THEN
        XINT = X1
        YINT = Y1
        GO TO 90
      END IF
      IF (ABS(X1-XA1).LT.EPS.AND.ABS(Y1-YA1).LT.EPS) THEN
        XINT = XA1
        YINT = YA1
        GO TO 90
      END IF
      IF (ABS(X2-XA1).LT.EPS.AND.ABS(Y2-YA1).LT.EPS) THEN
        XINT = XA1
        YINT = YA1
        GO TO 90
      END IF
      CALL LINEX (0,XA1,YA1,X1,Y1,XA2,YA2,X2,Y2,XINT,YINT,IER)
      IF (IER.EQ.0) THEN
        XINT = X1
        YINT = Y1
      ELSE
        XINT = X2
        YINT = Y2
      END IF
   90 continue

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  19. SUBROUTINE CIRCIR
C **  purpose of subroutine: INTERSECTION BETWEEN TWO CIRCLES
c **    input:
C **      XC1,YC1,R1....CENTER AND RADIUS OF 1ST CIRCLE
C **      XC2,YC2,R2....CENTER AND RADIUS OF 2ND CIRCLE
C **    output:
c **      NOP...........NUMBER OF POINTS -1 OVERLAP
C **      X1,Y1,X2,Y2...INTERSECTION POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE CIRCIR (XC1,YC1,R1,XC2,YC2,R2,NOP,X1,Y1,X2,Y2)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      eps = tol/2540.
      A = XC2-XC1
      B = YC2-YC1
      D = B*B+A*A
      NOP = 0
      IF (D.GT.((R1+R2)*(R1+R2)).OR.D.LT.((R1-R2)*(R1-R2))) GO TO 99
      IF (D.GT.eps) GO TO 5
      IF (R1.EQ.R2) NOP = -1
      GO TO 99
    5 H = 0.5*(1.0+(R1*R1-R2*R2)/D)
      S = R1*R1/D-H*H
      IF (S.GE.0.0) GO TO 6
      NOP = 0
      GO TO 99

C --- ONE SOLUTION
    6 S = SQRT(S)
      IF (ABS(S).GT.eps) GO TO 7
      NOP = 1
      GO TO 10

C --- TWO SOLUTIONS
    7 NOP = 2
      X2 = H*A-S*B+XC1
      Y2 = H*B+S*A+YC1
   10 X1 = H*A+S*B+XC1
      Y1 = H*B-S*A+YC1

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  20. SUBROUTINE ARCARC
C **  purpose of subroutine: INTERSECTION BETWEEN ARC AND ARC
c **    input:
C **      MOVE.............TOOL MOVES ON 1- 1ST ARC, 2- 2ND ARC
C **      NOEND............0- ALLOW ENDS INTERSECTION, 1- DONT ALLOW
C **      XCA,YCA,RA,KLOKA.CENTER, RADIUS AND DIRECTION OF 1ST ARC
C **      XCB,YCB,RB,KLOKB.CENTER, RADIUS AND DIRECTION OF 2ND ARC
C **      XA1,YA1,XA2,YA2..FIRST AND LAST POINTS OF 1ST ARC
C **      XB1,YB1,XB2,YB2..FIRST AND LAST POINTS OF 2ND ARC
C **    output:
c **      XINT,YINT........INTERSECTION POINT
C **      INTER............0- NO INTERSECION, 1- INTERSECTION 2- OVERLAP
c **********************************************************************
c **********************************************************************

      SUBROUTINE ARCARC (MOVE,NOEND,XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,
     +            XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,XINT,YINT,INTER)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      INTER = 0
      EPS = tol/2.54

C --- FIND INTERSECTION BETWEEN CIRCLES
      CALL CIRCIR (XCA,YCA,RA,XCB,YCB,RB,NOP,X1,Y1,X2,Y2)

C --- NO CONTACT
      IF (NOP.EQ.0) GO TO 99

C --- OVERLAP
      IF (NOP.EQ.-1) THEN
        INTER = 2
        GO TO 99
      END IF

C --- TANGENT, POSSIBLE ONE POINT
      IF (NOP.EQ.1) THEN
        IF (NOEND.EQ.1) THEN
          IF (ABS(XA1-X1).LT.EPS.AND.ABS(YA1-Y1).LT.EPS) GO TO 99
          IF (ABS(XA2-X1).LT.EPS.AND.ABS(YA2-Y1).LT.EPS) GO TO 99
          IF (ABS(XB1-X1).LT.EPS.AND.ABS(YB1-Y1).LT.EPS) GO TO 99
          IF (ABS(XB2-X1).LT.EPS.AND.ABS(YB2-Y1).LT.EPS) GO TO 99
        END IF
        CALL INARC (XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,X1,Y1,IN1)
        CALL INARC (XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,X1,Y1,IN2)
        IF (IN1.NE.0 .AND. IN2.NE.0) THEN
          XINT = X1
          YINT = Y1
          INTER = 1
        END IF
        GO TO 99
      END IF

C --- POSSIBLE TWO POINTS, FIND DISTANCE 1ST CENTER TO DIVIDE LINE
      IN1 = 0
      IF (NOEND.EQ.1) THEN
        IF (ABS(XA1-X1).LT.EPS.AND.ABS(YA1-Y1).LT.EPS) GO TO 21
        IF (ABS(XA2-X1).LT.EPS.AND.ABS(YA2-Y1).LT.EPS) GO TO 21
        IF (ABS(XB1-X1).LT.EPS.AND.ABS(YB1-Y1).LT.EPS) GO TO 21
        IF (ABS(XB2-X1).LT.EPS.AND.ABS(YB2-Y1).LT.EPS) GO TO 21
      END IF
      CALL INARC (XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,X1,Y1,I1)
      IF (I1.EQ.0) GO TO 21
      CALL INARC (XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,X1,Y1,I2)
      IF (I2.EQ.1) IN1 = 1
   21 IN2 = 0
      IF (NOEND.EQ.1) THEN
        IF (ABS(XA1-X2).LT.EPS.AND.ABS(YA1-Y2).LT.EPS) GO TO 23
        IF (ABS(XA2-X2).LT.EPS.AND.ABS(YA2-Y2).LT.EPS) GO TO 23
        IF (ABS(XB1-X2).LT.EPS.AND.ABS(YB1-Y2).LT.EPS) GO TO 23
        IF (ABS(XB2-X2).LT.EPS.AND.ABS(YB2-Y2).LT.EPS) GO TO 23
      END IF
      CALL INARC (XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,X2,Y2,I1)
      IF (I1.EQ.0) GO TO 23
      CALL INARC (XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,X2,Y2,I2)
      IF (I2.EQ.1) IN2 = 1
   23 IF (IN1.EQ.0 .AND. IN2.EQ.0) GO TO 99
      INTER = 1

C --- ONE POINT WITHIN ARC/ARC
      IF (IN1.EQ.1 .AND. IN2.EQ.0) THEN
        XINT = X1
        YINT = Y1
        GO TO 99
      END IF
      IF (IN2.EQ.1 .AND. IN1.EQ.0) THEN
        XINT = X2
        YINT = Y2
        GO TO 99
      END IF

C --- TWO POINTS WITHIN ARC/ARC, MOVING ON 1ST ARC
      IF (MOVE.EQ.1) THEN
        IF (ABS(X1-XA2).LT.EPS.AND.ABS(Y1-YA2).LT.EPS) THEN
          XINT = X2
          YINT = Y2
          GO TO 99
        END IF
        IF (ABS(X2-XA2).LT.EPS.AND.ABS(Y2-YA2).LT.EPS) THEN
          XINT = X1
          YINT = Y1
          GO TO 99
        END IF
        IF (ABS(X1-XA1).LT.EPS.AND.ABS(Y1-YA1).LT.EPS) THEN
          XINT = XA1
          YINT = YA1
          GO TO 99
        END IF
        IF (ABS(X2-XA1).LT.EPS.AND.ABS(Y2-YA1).LT.EPS) THEN
          XINT = XA1
          YINT = YA1
          GO TO 99
        END IF
        CALL LINEX (0,XA1,YA1,X1,Y1,XA2,YA2,X2,Y2,XINT,YINT,IER)
        IF (IER.EQ.0) THEN
          XINT = X1
          YINT = Y1
        ELSE
          XINT = X2
          YINT = Y2
        END IF
      ELSE

C --- TWO POINTS WITHIN ARC/ARC, MOVING ON 2ND ARC
        IF (ABS(X1-XB2).LT.EPS.AND.ABS(Y1-YB2).LT.EPS) THEN
          XINT = X2
          YINT = Y2
          GO TO 99
        END IF
        IF (ABS(X2-XB2).LT.EPS.AND.ABS(Y2-YB2).LT.EPS) THEN
          XINT = X1
          YINT = Y1
          GO TO 99
        END IF
        IF (ABS(X1-XB1).LT.EPS.AND.ABS(Y1-YB1).LT.EPS) THEN
          XINT = XB1
          YINT = YB1
          GO TO 99
        END IF
        IF (ABS(X2-XB1).LT.EPS.AND.ABS(Y2-YB1).LT.EPS) THEN
          XINT = XB1
          YINT = YB1
          GO TO 99
        END IF
          CALL LINEX (0,XB1,YB1,X1,Y1,XB2,YB2,X2,Y2,XINT,YINT,IER)
        IF (IER.EQ.0) THEN
          XINT = X1
          YINT = Y1
        ELSE
          XINT = X2
          YINT = Y2
        END IF
      END IF

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE ARCAR1
C **  purpose of subroutine: INTERSECTION BETWEEN ARC AND ARC - adjusted
C                            from arcarc for use in off1st
c **    input:
C **      XCA,YCA,RA,KLOKA.CENTER, RADIUS AND DIRECTION OF 1ST ARC
C **      XCB,YCB,RB,KLOKB.CENTER, RADIUS AND DIRECTION OF 2ND ARC
C **      XA1,YA1,XA2,YA2..FIRST AND LAST POINTS OF 1ST ARC
C **      XB1,YB1,XB2,YB2..FIRST AND LAST POINTS OF 2ND ARC
C **    output:
c **      XINT,YINT........INTERSECTION POINT
C **      INTER............0- NO INTERSECION, 1- INTERSECTION 2- OVERLAP
c **********************************************************************
c **********************************************************************

      SUBROUTINE ARCAR1 (XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,
     +            XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,XINT,YINT,INTER)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      INTER = 0
      EPS = tol/2.54

C --- FIND INTERSECTION BETWEEN CIRCLES
      CALL CIRCIR (XCA,YCA,RA,XCB,YCB,RB,NOP,X1,Y1,X2,Y2)

C --- NO CONTACT
      IF (NOP.EQ.0) GO TO 99

C --- OVERLAP
      IF (NOP.EQ.-1) THEN
        INTER = 2
        GO TO 99
      END IF

C --- TANGENT, POSSIBLE ONE POINT
      IF (NOP.EQ.1) THEN
        CALL INARC (XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,X1,Y1,IN1)
        CALL INARC (XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,X1,Y1,IN2)
        IF (IN1.NE.0 .AND. IN2.NE.0) THEN
          XINT = X1
          YINT = Y1
          INTER = 1
        END IF
        GO TO 99
      END IF

C --- POSSIBLE TWO POINTS
      IN1 = 0
      CALL INARC (XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,X1,Y1,I1)
      IF (I1.EQ.0) GO TO 21
      CALL INARC (XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,X1,Y1,I2)
      IF (I2.EQ.1) IN1 = 1
   21 IN2 = 0
      CALL INARC (XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,X2,Y2,I1)
      IF (I1.EQ.0) GO TO 23
      CALL INARC (XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,X2,Y2,I2)
      IF (I2.EQ.1) IN2 = 1
   23 IF (IN1.EQ.0 .AND. IN2.EQ.0) then
c
c..... main difference from arcarc: if no intersection check if an end
c..... of one arc is inside the other, if so accept this endpoint
c
        d = sqrt ((xb1-XCA)**2 + (yb1-YCA)**2)
        i1 = 0
        if (abs(d - ra) .lt. 0.1*eps)
     +     CALL INARC (XCA,YCA,RA,KLOKA,XA1,YA1,XA2,YA2,xb1,yb1,I1)
        if (I1.EQ.1) then
          INTER = 1
          xint = xb1
          yint = yb1
        else
          i2 = 0
          d = sqrt ((xa2-XCB)**2 + (ya2-YCB)**2)
          if (abs(d - RB) .lt. 0.1*eps)
     +      CALL INARC (XCB,YCB,RB,KLOKB,XB1,YB1,XB2,YB2,xa2,ya2,I2)
          if (I2.EQ.1) then
            INTER = 1
            xint = xa2
            yint = ya2
          endif
        endif
        GO TO 99
      endif
      INTER = 1

C --- ONE POINT WITHIN ARC/ARC
      IF (IN1.EQ.1 .AND. IN2.EQ.0) THEN
        XINT = X1
        YINT = Y1
        GO TO 99
      END IF
      IF (IN2.EQ.1 .AND. IN1.EQ.0) THEN
        XINT = X2
        YINT = Y2
        GO TO 99
      END IF

C --- TWO POINTS WITHIN ARC/ARC, MOVING ON 1ST ARC
        IF (ABS(X1-XA2).LT.EPS.AND.ABS(Y1-YA2).LT.EPS) THEN
          XINT = X2
          YINT = Y2
          GO TO 99
        END IF
        IF (ABS(X2-XA2).LT.EPS.AND.ABS(Y2-YA2).LT.EPS) THEN
          XINT = X1
          YINT = Y1
          GO TO 99
        END IF
        IF (ABS(X1-XA1).LT.EPS.AND.ABS(Y1-YA1).LT.EPS) THEN
          XINT = XA1
          YINT = YA1
          GO TO 99
        END IF
        IF (ABS(X2-XA1).LT.EPS.AND.ABS(Y2-YA1).LT.EPS) THEN
          XINT = XA1
          YINT = YA1
          GO TO 99
        END IF
        CALL LINEX (0,XA1,YA1,X1,Y1,XA2,YA2,X2,Y2,XINT,YINT,IER)
        IF (IER.EQ.0) THEN
          XINT = X1
          YINT = Y1
        ELSE
          XINT = X2
          YINT = Y2
        END IF

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  21. SUBROUTINE USEG
C **  purpose of subroutine: FIND RELATIVE POSITION OF POINT ON SEGMENT
c **    input:
C **      X1,Y1,X2,Y2...SEG POINTS 1 TO 2
C **      XU,YU.........POINT ON LINE
C **    output:
c **      U.............RELATIVE POSITION
c **********************************************************************
c **********************************************************************

      SUBROUTINE USEG (X1,Y1,X2,Y2,XU,YU,U)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      eps = tol/2.54
c
c.....project point to line
c
      dx = x2-x1
      px = xu-x1
      dy = y2-y1
      py = yu-y1
      u1p = (px * dx + py * dy )/(dx * dx + dy * dy)
      xu1 = x1 + u1p * (x2-x1)
      yu1 = y1 + u1p * (y2-y1)
      px1 = xu1-x1
      py1 = yu1-y1
      
      if (abs(dy) .gt. abs(dx)) then
        dx = dy
        px1 = py1
      endif
      if (abs(dx) .lt. eps*.1) then
        u = 1.5
      else
        u = px1/dx
      endif

      END

c **********************************************************************
c **********************************************************************
c **  22. SUBROUTINE LINEX
C **  purpose of subroutine: TESTS FOR INTERSECTION OF TWO LINES OR
c **                         SEGMENTS
C **    INPUT:
c **      IT..........0- SEGMENTS
C **                  1- INFINITE LINES
C **      X1,Y1,ETC...COORDINATES OF POINTS DEFINING THE LINES
C **    OUTPUT:
c **      XINT,YINT...POINT OF INTERSECTION IF IT EXISTS
C **      IER.........0- NO INTERSECTION
C **                  1- LINES INTERSECT
C **                  2- LINES ARE POINTS OR PARALLEL AND OVERLAP
c **********************************************************************
c **********************************************************************

      SUBROUTINE LINEX (IT,X1,Y1,X2,Y2,X3,Y3,X4,Y4,XINT,YINT,IER)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      EPS = tol/25400.
      SIZE12 = (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1)
      SIZE34 = (X4-X3)*(X4-X3) + (Y4-Y3)*(Y4-Y3)
      IF (SIZE12.LT.EPS.OR.SIZE34.LT.EPS) THEN
        IER = 2
        GO TO 99
      END IF

C---  FIRST HANDLE CASES WHERE ONE OF THE LINES IS A POINT
      IF ((SIZE12 .GE. EPS) .OR. (SIZE34 .GE. EPS)) GO TO 10

C---  BOTH OF THE LINES ARE POINTS
      SIZE13 = (X3-X1)*(X3-X1) + (Y3-Y1)*(Y3-Y1)
      IF (SIZE13 .GE. EPS) GO TO 5

C---  POINTS ARE COINCIDENT
      IER=1
      XINT=X1
      YINT=Y1
      GOTO 99

C---  POINTS ARE DIFFERENT
    5 IER=0
      GO TO 99
   10 IF ((SIZE12 .GE. EPS) .AND. (SIZE34 .GE. EPS)) GO TO 20

C---  ONLY ONE OF THE LINES IS A POINT
      IF (SIZE12 .GE. EPS) GO TO 13
      XP = X1
      YP = Y1
      XL1 = X3
      YL1 = Y3
      XL2 = X4
      YL2 = Y4
      GO TO 16
   13 XP = X3
      YP = Y3
      XL1 = X1
      YL1 = Y1
      XL2 = X2
      YL2 = Y2

C---  CHECK IF POINT LIES ON INFINITE LINE
   16 CHECK = (YP-YL1)*(XP-XL2) - (YP-YL2)*(XP-XL1)
      IF (ABS(CHECK) .LT. EPS) GO TO 17

C---  EXIT IF POINT OFF INFINITE LINE
      IER=0
      GOTO 99

C---  POINT ON INFINITE LINE
   17 IF (IT .EQ. 0) GO TO 18

C---  LINE INFINITE SO EXIT
      IER = 1
      XINT = XP
      YINT = YP
      GO TO 99

C---  FIND PARAMETRIC POSITION OF POINT ON LINE
   18 PARANUM = XP - XL1
      PARADEN = XL2 - XL1
      IF (ABS(PARADEN) .LT. EPS) PARANUM = YP - YL1
      IF (ABS(PARADEN) .LT. EPS) PARADEN = YL2  -YL1
      PARAPOS = PARANUM/PARADEN
      IF ((PARAPOS.GE.0.) .AND. (PARAPOS.LE.1.)) GO TO 19

C---  POINT OFF SEGMENT SO EXIT
      IER=0
      GO TO 99

C---  POINT IS ON SEGMENT
   19 IER = 1
      XINT = XP
      YINT = YP
      GO TO 99

C---  NEITHER OF THE LINES IS A POINT, COMPUTE DETERMINANT
   20 DET =(X2-X1)*(Y4-Y3) - (X4-X3)*(Y2-Y1)
      IF (ABS(DET) .GE. EPS) GO TO 40

C---  LINES PARALLEL. CHECK FOR COINCIDENCE (COMPARE LINE 16)
      CHECK = (Y3-Y1)*(X3-X2) - (Y3-Y2)*(X3-X1)
      IF (ABS(CHECK) .LT. EPS) GO TO 23

C---  LINES NOT COINCIDENT SO EXIT
      IER = 0
      GO TO 99

C---  LINES COINCIDENT
   23 IF (IT .EQ. 0) GO TO 26

C---  INFINITE COINCIDENT LINES, SO EXIT WITH IER=2
      IER=2
      GO TO 99

C---  SEGMENTS ALONG INFINITE LINE. COMPUTE PARAMETRIC POSITIONS OF PTS
C     P3 AND P4, TAKING P1 AT ZERO AND P2 AT ONE.
   26 IF (ABS(X2-X1) .LT. EPS) GO TO 29
      P3 = (X3-X1)/(X2-X1)
      P4 = (X4-X1)/(X2-X1)
      GO TO 32
   29 P3 = (Y3-Y1)/(Y2-Y1)
      P4 = (Y4-Y1)/(Y2-Y1)

C---  PICK OUT BIGGER AND SMALLER
   32 BIGGER  = AMAX1(P3,P4)
      SMALLER = AMIN1(P3,P4)
      IF ((BIGGER.LT.0.) .OR. (SMALLER.GT.1.)) GO TO 35
      IF (BIGGER.LT.EPS) GO TO 36
      IF (SMALLER .GT. (1.-EPS)) GO TO 37

C---  IN REMAINING CASES SEGMENTS OVERLAP; EXIT WITH IER=2
      IER=2
      GO TO 99

C---  NO INTERSECTION SO EXIT
   35 IER=0
      GO TO 99

C---  INTERSECTION WITH POINT AT END OF SEGMENT
   36 IER = 1
      XINT = X1
      YINT = Y1
      GO TO 99
   37 IER = 1
      XINT = X2
      YINT = Y2
      GO TO 99

C---  MAIN CASE - NON-ZERO, NON-PARALLEL LINES
   40 DETU = (X3-X1)*(Y4-Y3) - (Y3-Y1)*(X4-X3)
      DETV = (X3-X1)*(Y2-Y1) - (Y3-Y1)*(X2-X1)
      U= DETU/DET
      V= DETV/DET
      IF (IT .EQ. 0) GO TO 45

C---  INFINITE LINES - COMPUTE POINT OF INTERSECTION
      IER = 1
      XINT = X1 + U*(X2-X1)
      YINT = Y1 + U*(Y2-Y1)
      GO TO 99

C---  LINES ARE SEGMENTS. CHECK THAT (U,V) IS IN [0,1] .
   45 IF ((U.LT.(-EPS)).OR.(U.GT.(1.+EPS)).OR.(V.LT.(-EPS)).OR.
     +           (V.GT.(1.+EPS))) GO TO 50

C---  U,V ARE IN RANGE SO COMPUTE INTERSECTION.
      IER=1
      XINT=X1+U*(X2-X1)
      YINT=Y1+U*(Y2-Y1)
      GOTO 99
C
C---  U,V NOT IN RANGE HENCE SEGMENTS DO NOT INTERSECT
   50 IER = 0

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  23. SUBROUTINE FLIPO
C **  purpose of routine: TO FLIP A STRING
c **    input/output:
C **      XA,YA,NPPA...STRINGS AND NO OF POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE FLIPO (XA,YA,NPPA)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

C --- FLIP ARCS
      I = 0
   23 I = I+1
      IF (I .GT. NPPA-3) GO TO 33
        IF (XA(I).EQ.FLARC) THEN
          TX = XA(I)
          TY = YA(I)
          XA(I) = XA(I+2)
          YA(I) = YA(I+2)
          XA(I+2) = TX
          YA(I+2) = TY
          YA(I+1) = -YA(I+1)
          I = I+3
        END IF
      GO TO 23

C --- FLIP STRING
   33 N = NPPA/2
      DO 44 I = 1, N
        J = NPPA-I+1
        TX = XA(I)
        TY = YA(I)
        XA(I) = XA(J)
        YA(I) = YA(J)
        XA(J) = TX
        YA(J) = TY
   44 CONTINUE

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  24. SUBROUTINE ENDPTS
C **  purpose of subroutine: FIND OFFSET FIRST OR LAST POINT
C **    INPUT:
c **      KEND....1- FIRST POINT, 2-LAST POINT
C **      ISIDE...1- OFFSET ON RIGHT, -1-LEFT
C **      RR......RADIUS
C **      NPPA....NO OF POINTS IN A-STRING
C **      XA,YA...ARRAYS OF A-STRING
C **    OUTPUT:
c **      X1,Y1...OFFSET END POINTS
C **      IER.....0/1- END PT COMPATIBLE/INCOMPATIBLE
c **********************************************************************
c **********************************************************************

      SUBROUTINE ENDPTS (KEND,ISIDE,RR,XA,YA,NPPA,X1,Y1,IER)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'pokcom.com'

      real*4 XA(maxpts),YA(maxpts)

      IER = 0
      IF (KEND.EQ.2.AND.NPPA.GT.4) GO TO 53
      IF (KEND.EQ.2.AND.NPPA.LT.5) GO TO 63

C --- FIRST POINT
      IF (XA(2).EQ.FLARC) THEN
        RARC = XA(3)
        KLOK = YA(3)
        ADD = ISIDE*KLOK
        RNEW = RARC + ADD*RR
        IF (RNEW.LT.0.0)GO TO 97
        X1 = XA(4) + (RNEW/RARC)*(XA(1)-XA(4))
        Y1 = YA(4) + (RNEW/RARC)*(YA(1)-YA(4))
      ELSE
        DX = XA(2) - XA(1)
        DY = YA(2) - YA(1)
        S = SQRT(DX*DX + DY*DY)
        DX = DX/S
        DY = DY/S
        X1 = XA(1) + DY*RR*ISIDE
        Y1 = YA(1) - DX*RR*ISIDE
      END IF
      GO TO 99

C --- LAST POINT IN ARC
   53 IF (XA(NPPA-3).EQ.FLARC) THEN
        RARC = XA(NPPA-2)
        KLOK = YA(NPPA-2)
        ADD = ISIDE*KLOK
        RNEW = RARC + ADD*RR
        IF (RNEW.LT.0.0)GO TO 97
        X1 = XA(NPPA-1) + (RNEW/RARC)*(XA(NPPA)-XA(NPPA-1))
        Y1 = YA(NPPA-1) + (RNEW/RARC)*(YA(NPPA)-YA(NPPA-1))
        GO TO 99
      END IF

C --- LAST POINT ON SEGMENT
   63 DX = XA(NPPA) - XA(NPPA-1)
      DY = YA(NPPA) - YA(NPPA-1)
      S = SQRT(DX*DX + DY*DY)
      DX = DX/S
      DY = DY/S
      X1 = XA(NPPA) + DY*RR*ISIDE
      Y1 = YA(NPPA) - DX*RR*ISIDE
      GO TO 99

   97 IER =1
   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  25. SUBROUTINE TANARC
C **  purpose of subroutine: GIVES UNIT VECTOR AT END OF ARC
c **    input:
C **      KEND...........1- 1ST POINT OF ARC, 2- 2ND POINT
C **      XC,YC,R,KLOK...CENTER POINTS, RADIUS AND DIRECTION OF ARC
C **      X1,Y1,X2,Y2....1ST AND 2ND POINTS OF ARC
C **    output:
c **      UX,UY..........VECTOR AT END
c **********************************************************************
c **********************************************************************

      SUBROUTINE TANARC (KEND,XC,YC,R,KLOK,X1,Y1,X2,Y2,UX,UY)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      IF (KEND.EQ.1) THEN
        X3 = X1
        Y3 = Y1
      ELSE
        X3 = X2
        Y3 = Y2
      END IF
        DX = (X3-XC)/R
        DY = (Y3-YC)/R
        UX = -DY*KLOK
        UY =  DX*KLOK

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  26. SUBROUTINE DSPTARC
C **  purpose of subroutine: DISTANCE POINT TO ARC
c **    input:
C **      XP,YP.........COORDINATES OF POINT
C **      R.............RADIUS OF ARC
C **      KLOK..........1- CCW, -1- CW
C **      XC,YC.........CENTER OF ARC
C **      X1,Y1,X2,Y2...START AND END POINTS OF ARC
C **    output:
c **      DIST..........DISTANCE
C **      XN,YN.........POINT ON ARC
C **      U.............RELATIVE LOCATION
c **********************************************************************
c **********************************************************************

      SUBROUTINE DSPTARC (XP,YP,R,KLOK,XC,YC,X1,Y1,X2,Y2,DIST,XN,YN,U)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      DX = XP-XC
      DY = YP-YC
      DD = SQRT(DX*DX+DY*DY)
      IF (DD.LT.0.1) THEN
        DIST = R
        U = 99.0
        GO TO 99
      END IF
      DIST = ABS(DD-R)
      DX = DX/DD
      DY = DY/DD
      XN = XC+R*DX
      YN = YC+R*DY
      CALL UARC (R,KLOK,XC,YC,X1,Y1,X2,Y2,XN,YN,U)

   99 RETURN
      END

c **********************************************************************
c **********************************************************************
c **  27. SUBROUTINE UARC
C **  purpose of subroutine: FIND RELATIVE LOCATION OF POINT ON ARC
c **    input:
C **      R.............RADIUS OF ARC
C **      KLOK..........1- CCW, -1- CW
C **      XC,YC.........CENTER OF ARC
C **      X1,Y1,X2,Y2...START AND END POINTS OF ARC
C **      X3,Y3.........POINT ON ARC
C **    output:
c **      U.............RELATIVE LOCATION OF X3,Y3 ON ARC
c **********************************************************************
c **********************************************************************

      SUBROUTINE UARC (R,KLOK,XC,YC,X1,Y1,X2,Y2,X3,Y3,U)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'const.com'

C --- FIND THE 3 ANGLES
      DX = X1-XC
      DY = Y1-YC
      IF (KLOK.EQ.1) THEN
        A1 = ATAN2(DY,DX)
      ELSE
        A2 = ATAN2(DY,DX)
      END IF
      DX = X2-XC
      DY = Y2-YC
      IF (KLOK.EQ.1) THEN
        A2 = ATAN2(DY,DX)
      ELSE
        A1 = ATAN2(DY,DX)
      END IF
      DX = X3-XC
      DY = Y3-YC
      A3 = ATAN2(DY,DX)

C --- FIND RELATIVE LOCATION
      A21 = A2-A1
      IF (A21.LT.0.0) A21 = TWO_PI+A21
      IF (A21.GE.TWO_PI) A21 = A21-TWO_PI
      A31 = A3-A1
      IF (A31.LT.0.0) A31 = TWO_PI+A31
      IF (A31.GE.TWO_PI) A31 = A31-TWO_PI
      U = A31/A21
      IF (KLOK.EQ.-1) U = 1.0-U

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  28. SUBROUTINE XYARC
C **  purpose od subroutine: FIND COORDINATES FROM RELATIVE LOCATION
c **                         OF POINT ON ARC
c **    input:
C **      R.............RADIUS OF ARC
C **      KLOK..........1- CCW, -1- CW
C **      XC,YC.........CENTER OF ARC
C **      X1,Y1,X2,Y2...START AND END POINTS OF ARC
C **      U.............RELATIVE LOCATION OF X3,Y3 ON ARC
C **    output:
c **      X3,Y3.........COORDINATES OF POINT ON ARC
c **********************************************************************
c **********************************************************************

      SUBROUTINE XYARC (R,KLOK,XC,YC,X1,Y1,X2,Y2,U,X3,Y3)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      include 'const.com'

C --- FIND THE 2 ANGLES
      DX = X1-XC
      DY = Y1-YC
      IF (KLOK.EQ.1) THEN
        A1 = ATAN2(DY,DX)
      ELSE
        A2 = ATAN2(DY,DX)
      END IF
      DX = X2-XC
      DY = Y2-YC
      IF (KLOK.EQ.1) THEN
        A2 = ATAN2(DY,DX)
      ELSE
        A1 = ATAN2(DY,DX)
      END IF

C --- FIND COORDINATES OF RELATIVE LOCATION
      A21 = A2-A1
      IF (A21.LT.0.0) A21 = TWO_PI+A21
      IF (A21.GE.TWO_PI) A21 = A21-TWO_PI
      IF (KLOK.EQ.1) THEN
        A31 = U*A21
      ELSE
        A31 = (1.0-U)*A21
      END IF
      IF (A31.LT.0.0) A31 = TWO_PI+A31
      IF (A31.GE.TWO_PI) A31 = A31-TWO_PI
      A3 = A31+A1
      DX = COS(A3)
      DY = SIN(A3)
      X3 = XC+R*DX
      Y3 = YC+R*DY

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  29. SUBROUTINE XYSEG
C **  purpose of subroutine: FIND COORDINATES FROM RELATIVE POSITION
c **                         OF POINT ON SEGMENT
c **    input:
C **      X1,Y1,X2,Y2...SEG POINTS 1 TO 2
C **      U.............RELATIVE POSITION
C **    output:
c **      XU,YU.........POINT ON LINE
c **********************************************************************
c **********************************************************************

      SUBROUTINE XYSEG (X1,Y1,X2,Y2,U,XU,YU)

      implicit real*4 (a-h,o-z), integer*2 (i-n)

      XU = X1+U*(X2-X1)
      YU = Y1+U*(Y2-Y1)

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  30. SUBROUTINE CHKSEG
C **  purpose of subroutine: FIND WHETHER A CIRCLE INTERSECTS
c **                         SEGMENT XY WHEN MOVING ALONG SEGMENT AB
c **    input:
C **      aa0,bb0,aa1,bb1 ... SEGMENT AB ENDPOINTS
C **      xx0,yy0,xx1,yy1 ... SEGMENT XY ENDPOINTS
C **      crad            ... circle radius
C **    output:
c **      ierr .........  0, iff no intersection
c **                      1, iff intersection
c **********************************************************************
c **********************************************************************

      SUBROUTINE CHKSEG (aa0,bb0,aa1,bb1,crad,xx0,yy0,xx1,yy1,ierr)

      include 'pokcom.com'

      real*8 xx0,yy0,xx1,yy1
      real*4 aa0,bb0,aa1,bb1,crad
      integer*2 ierr

      real*4 size,va,vb,ua,ub
      real*4 a0,b0,a1,b1,x0,y0,x1,y1,XINT,YINT
      integer*2 klok,i

      ierr = 0
      klok = 1

      va = aa1 - aa0
      vb = bb1 - bb0
      size = sqrt (va*va + vb*vb)

      if (size .lt. 0.5*tol) then
        va = 1.
        vb = 0.
      else
        va = va/size
        vb = vb/size
      endif
            
      x0 = xx0
      x1 = xx1
      y0 = yy0
      y1 = yy1
c
c..... the 2 arcseg calls below were added to fix QAR 92043: we check if the
c..... circle centered at (a0,b0) intersect the "wall" XY, and then check the
c..... circle centered at (a1,b1).
c
      if (crad .gt. 0.) then
        a0 = aa0 + va*crad
        a1 = a0
        b0 = bb0 + vb*crad
        b1 = b0
c
c..... the 'if' below is a QAR 92311 fix
c
        if ((aa0-x0)**2 + (bb0-y0)**2 .lt. crad**2) then
          ierr = 1
          return
        endif
        CALL ARCSEG (2,0,aa0,bb0,crad,klok,
     +             a0,b0,a1,b1,x0,y0,x1,y1,XINT,YINT,ierr)
        if (ierr.eq.1) return
        if (size .lt. 0.5*tol) return
        a0 = aa1 - va*crad
        a1 = a0
        b0 = bb1 - vb*crad
        b1 = b0
        if ((aa1-x0)**2 + (bb1-y0)**2 .lt. crad**2) then
          ierr = 1
          return
        endif
        CALL ARCSEG (2,0,aa1,bb1,crad,klok,
     +             a0,b0,a1,b1,x0,y0,x1,y1,XINT,YINT,ierr)
        if (ierr.eq.1) return
        ua = - vb
        ub = va
      endif

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

        CALL LINEX (0,a0,b0,a1,b1,x0,y0,x1,y1,XINT,YINT,ierr)
             
        if (ierr.eq.1 .and. ((a0-XINT)**2 + (b0-YINT)**2 .lt. tol**2 
     x       .or. (a1-XINT)**2 + (b1-YINT)**2 .lt. tol**2)) then
          ierr = 0
          return
        endif
        if (ierr.eq.2) ierr = 0
        if (ierr.eq.1) return
10    continue

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  32. SUBROUTINE CHKSG1
C **  purpose of subroutine: FIND WHETHER A CIRCLE INTERSECTS a
c **                         SEGMENT XY
c **    input:
C **      a0,b0           ... circle center
C **      xx0,yy0,xx1,yy1 ... SEGMENT XY ENDPOINTS
C **      crad            ... circle radius
C **    output:
c **      ierr .........  0, iff no intersection
c **                      1, iff intersection
c **********************************************************************
c **********************************************************************

      SUBROUTINE CHKSG1 (a0,b0,crad,xx0,yy0,xx1,yy1,ierr)

      include 'pokcom.com'

      real*8 xx0,yy0,xx1,yy1
      real*4 a0,b0,crad
      integer*2 ierr

      real*4 ua,ub,va0,vb0,va1,vb1,size,d0,d1,dd,co

      ierr = 0
      if (crad .le. tol/2.) return

      va0 = a0 - xx0
      vb0 = b0 - yy0

      d0 = sqrt (va0*va0 + vb0*vb0)
      if (d0 + tol/2. .lt. crad) then
        ierr = 1
        return
      endif

      ua = xx1 - xx0
      ub = yy1 - yy0

      size = sqrt (ua*ua + ub*ub)

      if (size .lt. tol/2.) return

      ua = ua/size
      ub = ub/size

      co = ua*va0 + ub*vb0
      if ( co.le.0.) return

      if (co.ge.size) then
        va1 = a0 - xx1
        vb1 = b0 - yy1
        d1 = sqrt (va1*va1 + vb1*vb1)
        if (d1 + tol/2. .lt. crad) ierr = 1
        return
      endif

      dd = sqrt (d0*d0 - co*co)
      if (dd + tol/2. .lt. crad) ierr = 1

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **   subroutine name:  flook
c **
c **  purpose of subroutine: to find an entry point that would not
c **                         gouge the pocket geometry
c **
c **********************************************************************
c **********************************************************************

      subroutine flook(sid,ix,rad,crad,tolc,tolm,ier)

      include 'com.com'
      include 'const.com'
      include 'rrdm.com'
      include 'pokcom.com'

      integer*2 ix
      integer*4 ier
      real*4 sid,rad,tolc,tolm
      real*8 crad

      integer*2 isvasw(480)
      equivalence (rsvasw, isvasw)

      integer*2 i,k,nppa,klok
      real*4 vx,vy,dis,del,radc,dx,dy,a0,b0,fmm,sdrad,du
      real*4 x0,y0,x1,y1,ro,xl,yl,vxr,vyr,xc,yc,r,alp0,alp1,dal,bet

      real*4 XA(maxpts),YA(maxpts)

      radc = rad
      ro = isvasw(3)*crad/radc
      fmm = 1.
      if (ifl(264) .eq. 0) then
        fmm = 25.4
        radc = radc/fmm
      endif
      sdrad = sid*radc

      CALL GETLAN (ix,xa,ya,nppa)

      k = 1
      x0 = xa(k)
      y0 = ya(k)

50      k = k+1
        if (xa(k) .lt. flarc) then
          x1 = xa(k)
          y1 = ya(k)
          vx = x1 - x0
          vy = y1 - y0
          dis = sqrt(vx**2+vy**2)
          np = dis/radc + 0.5
          if (np .lt. 1) np = 1
          del = dis/np

          vx = vx/dis
          vy = vy/dis
          dx = vx*del
          dy = vy*del

          vxr = sdrad*vy
          vyr = -sdrad*vx

          do i = 1,np
            a0 = (x0 - vxr)*fmm
            b0 = (y0 - vyr)*fmm
            xl = x0 + ro*vxr
            yl = y0 + ro*vyr
            if (i .eq. 1) then
              xl = xl + tolc*vx
              yl = yl + tolc*vy
            endif
            call chkhel(rad,crad,a0,b0,xl,yl,tolm,ier)
            if (ier.ne.0 .or. look.eq.2) return
            x0 = x0 + dx
            y0 = y0 + dy
          enddo
        else
          r = xa(k+1)
          klok = ya(k+1)
          xc = xa(k+2)
          yc = ya(k+2)
          x1 = xa(k+3)
          y1 = ya(k+3)

          DX = X0-XC
          DY = Y0-YC
          vx = (-dy*klok)/r
          vy = (dx*klok)/r
          IF (KLOK.EQ.1) THEN
            alp0 = ATAN2(DY,DX)
          ELSE
            alp1 = ATAN2(DY,DX)
          ENDIF
          DX = X1-XC
          DY = Y1-YC
          IF (KLOK.EQ.1) THEN
            alp1 = ATAN2(DY,DX)
          ELSE
            alp0 = ATAN2(DY,DX)
          ENDIF

          dal = alp1 - alp0
          IF (dal.LT.0.0) dal = TWO_PI + dal
          IF (dal.GE.TWO_PI) dal = dal - TWO_PI

          dis = abs(dal)*r
          np = dis/radc + 0.5
          if (np .lt. 1) np = 1
          du = 1./np

          do i = 1,np
            if (i .gt. 1) then
              IF (KLOK.EQ.1) THEN
                bet = (i-1)*du*dal
              ELSE
                bet = (1.0-(i-1)*du)*dal
              ENDIF
              IF (bet.LT.0.0) bet = TWO_PI+bet
              IF (bet.GE.TWO_PI) bet = bet-TWO_PI
              alp = bet+alp0
              DX = COS(alp)
              DY = SIN(alp)
              X0 = XC+R*DX
              Y0 = YC+R*DY
              vx = -dy*klok
              vy = dx*klok
            endif
            vxr = sdrad*vy
            vyr = -sdrad*vx
            a0 = (x0 - vxr)*fmm
            b0 = (y0 - vyr)*fmm
            xl = x0 + ro*vxr
            yl = y0 + ro*vyr
            if (i .eq. 1) then
              xl = xl + tolc*vx
              yl = yl + tolc*vy
            endif
            call chkhel(rad,crad,a0,b0,xl,yl,tolm,ier)
            if (ier.ne.0 .or. look.eq.2) return
          enddo
          k = k+3
        endif

        x0 = x1
        y0 = y1

100   if (k .lt. nppa) goto 50

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  chkhel
c **
c **  purpose of subroutine: gouge-checking for a helix; if no gouge, the
c **                         new starting point xlook,ylook is set
c **
c **********************************************************************
c **********************************************************************

      subroutine chkhel(rad,crad,a0,b0,x0,y0,tolm,ier)

      include 'com.com'
      include 'rrdm.com'
      include 'pokcom.com'

      integer*4 ier
      real*4 a0,b0,x0,y0,rad,tolm
      real*8 crad

      integer*2 isvasw(480)
      equivalence (rsvasw, isvasw)

      real*4 ro
      integer*2 i,j,ientry,jcut


        ientry = 1
        i = 1
        jcut = 0
c
c..... isvasw(4*i-1) is 1,0,-1 if the perimeter condition is in,on,out (with
c..... i = 1); for islands it is 1,0,-1 if the condition is out,on,in
c..... For now, we do not check the perimeter gouging with the OUT condition,
c..... or an island gouging with the IN condition.
c
2215    if (isvasw(4*i-1) .eq. -1) then
          i = i+1
          if (i .ge. islnds + 2) return
          goto 2215
        endif

        ro = rad + crad*isvasw(4*i-1) - tolm
        if (ro .lt. 0.) ro = 0.

        call chkgou (ientry,i,ro,a0,b0,0.,0.,jcut,ier,0)
        if (ier.ne.0 .or. jcut.eq.1) return

        i = i + 1
        if (i .lt. islnds + 2) goto 2215

        if (jcut .eq. 0) then
          look = 2
          xlook = x0
          ylook = y0
          do j = islnds+2, lanes
            loca(j+1) = 0
            lane(j) = 0
            loop(j) = 0
            mama(j) = 0
          enddo
          lanes = islnds+1
          if (ifl(264) .eq. 0) then
            xlook = xlook*25.4
            ylook = ylook*25.4
          endif
          return
        endif

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  flook1
c **
c **  purpose of subroutine: try to shuffle a pocket loop so that
c **                         a ramp entry would not gouge the pocket
c **                         geometry
c **    input:
C **      ix              ... contour number
C **      xt,yt           ... currrent entry segment
C **      dis             ... entry distance
C **    output:
c **      jshuff........  2, iff the loop is reshuffled
c **      ier  .........  error flag
c **********************************************************************
c **********************************************************************

      subroutine flook1 (ix,xt,yt,dis,jshuff,ier)

      include 'com.com'
      include 'const.com'
      include 'rrdm.com'
      include 'pokcom.com'

      integer*2 ix,jshuff
      real*4 xt(2),yt(2),dis
      integer*4 ier

      integer*2 k,nppa,nppb,ifwd
      real*4 XA(maxpts),YA(maxpts),XB(maxpts),YB(maxpts)
      real*4 vx,vy,dv,delta,d1,d2,xmm,ymm,useg
      integer*2 lseg,icen
      integer*4 kk,kk0
      real*8 xxb,yyb

      CALL GETLAN (ix,xa,ya,nppa)
      if (nppa .lt. 5) return

      delta = 5*tol
c
c..... get the first segment direction and length
c
      if (xa(2) .eq. flarc) return
      vx = xa(2) - xa(1)
      vy = ya(2) - ya(1)

      dv = sqrt(vx**2+vy**2)
      if (dv .lt. tol) return

      vx = vx/dv
      vy = vy/dv

      dv = xa(1)*vx + ya(1)*vy

      d1 = xt(1)*vx + yt(1)*vy
      d2 = xt(2)*vx + yt(2)*vy

      d1 = d1 - dv
      d2 = d2 - dv

      ifwd = 0
c
c..... current entry goes either along the first segment (ifwd = 1),
c..... or in the opposite direction (ifwd = -1)
c
      if (d1 .lt. tol .or. d2 .lt. tol) then
        dv = d1 + d2

        if (dv .gt. delta) then
           ifwd = 1
        else if (dv .lt. -delta) then
           ifwd = -1
        endif
      endif

      if (ifwd .eq. 0) return

      dv = dis + delta
      dsq = dv*dv
c
c..... find a segment longer than the entry distance
c
      call cpylap (xa,ya,nppa,xb,yb,nppb)
      CALL segmax (xb,yb,nppb,dv,lseg)

      if (dv .lt. dsq) return

      ICEN = 1
      dv = sqrt(dv)
c
c..... shuffle the loop so that the new first segment will be long
c..... enough for the entry
c
      if (ifwd .eq. -1) then
        USEG = (dv + dis)/(2*dv)
      else
        USEG = (dv - dis)/(2*dv)
      endif
      CALL SHUFFLE (xb,yb,nppb,LSEG,ICEN,USEG,XMM,YMM)

C... remove in-line points, arcs and overlap
      CALL STRAIT (xb,yb,nppb)

c
c..... remove duplicate points
c
      if (nppb .gt. nppa) then
        CALL DOUBLE (xb,yb,nppb)
      endif

      if (nppb .eq. nppa) then
        jshuff = 2

        kk0 = loca(ix)

        do k = 1,nppa

          kk = kk0 + k - 1
          xxb = xb(k)
          yyb = yb(k)

          call rrplce (kk,xxb,yyb,ier)
          if (ier .ne. 0) return

        enddo

      endif

      return
      end
      
c **********************************************************************
c **********************************************************************
c **   subroutine name:  poknt0
c **
c **  purpose of subroutine: check if an 'off part' entry gouges,
c **                         if not calculate the starting z-level
c **********************************************************************
c **********************************************************************

      subroutine poknt0 (xx,yy,zbot,crad,buf,trans,mx,jcut,ier)

      include 'com.com'
      include 'const.com'
      include 'vocab.com'
      include 'rrdm.com'
      include 'pokcom.com'
      include 'pokpl.com'

      common/tacom/ tabot
      real*8 tabot

      integer*4 ier
      logical trans
      real*8 xx,yy,zbot,buf(6),mx(12)

      integer*4 itp, ind
      real*8 xx0,yy0,znow
      real*4 tolm,fact,a1,b1,crad

      integer*2 i,k,nppa,klok,kcut

      real*4 vx,vy,dis,dx,dy,du,dmin,dst,x2,y2
      real*4 x0,y0,x1,y1,xc,yc,r,alp0,alp1,dal,bet,xb,yb

      real*4 XA(maxpts),YA(maxpts)

      znow = buf(3)
      xx0 = sc(1)
      yy0 = sc(2)

      fact = 1.
      if (ifl(264).eq.0) fact = 25.4

      tolm = tol*fact

      x0 = xx0*fact
      y0 = yy0*fact

      a1 = xx*fact
      b1 = yy*fact

c      crad = (sc(28)/2.+sc(24))*fact
      crad = crad*fact

      if (.not.lwatfl) then
c
c..... check if geometry is gouged
c
        if (nwrn .le. 0) then
          jcut = 0
          call chkrmp(crad,x0,y0,a1,b1,tolm,jcut,ier)
          if (ier.ne.0 .or. jcut.ne.0) goto 999
        endif
      else
        i = 1
        CALL GETLAN (i,xa,ya,nppa)

        dmin = flarc
        k = 1
        jcut = 1
        x0 = xa(k)
        y0 = ya(k)
        xb = x0
        yb = y0

50        k = k+1
          kcut = 0
          if (xa(k) .lt. flarc) then
            x1 = xa(k)
            y1 = ya(k)
            vx = x1 - x0
            vy = y1 - y0
            dis = sqrt(vx**2+vy**2)
            np = dis/crad + 0.5
            if (np .lt. 1) np = 1
            du = 1./np
            dx = vx*du
            dy = vy*du
            do i = 1,np
              dst = (x0-a1)**2 + (y0-b1)**2
              if (dst .lt. dmin) then
                if (nwrn .le. 0) then
                  call chkrmp(crad,x0,y0,a1,b1,tolm,kcut,ier)
                  if (ier.ne.0) goto 999
                endif
                if (kcut.eq.0) then
                  jcut = 0
                  dmin = dst
                  xb = x0
                  yb = y0
                endif
              endif
              x0 = x0 + dx
              y0 = y0 + dy
            enddo
          else
            r = xa(k+1)
            klok = ya(k+1)
            xc = xa(k+2)
            yc = ya(k+2)
            x1 = xa(k+3)
            y1 = ya(k+3)

            DX = X0-XC
            DY = Y0-YC
            IF (KLOK.EQ.1) THEN
              alp0 = ATAN2(DY,DX)
            ELSE
              alp1 = ATAN2(DY,DX)
            ENDIF
            DX = X1-XC
            DY = Y1-YC
            IF (KLOK.EQ.1) THEN
              alp1 = ATAN2(DY,DX)
            ELSE
              alp0 = ATAN2(DY,DX)
            ENDIF

            dal = alp1 - alp0
            IF (dal.LT.0.0) dal = TWO_PI + dal
            IF (dal.GE.TWO_PI) dal = dal - TWO_PI

            dis = abs(dal)*r
            np = dis/crad + 0.5
            if (np .lt. 1) np = 1
            du = 1./np

            do i = 1,np
              if (i .gt. 1) then
                IF (KLOK.EQ.1) THEN
                  bet = (i-1)*du*dal
                ELSE
                  bet = (1.0-(i-1)*du)*dal
                ENDIF
                IF (bet.LT.0.0) bet = TWO_PI+bet
                IF (bet.GE.TWO_PI) bet = bet-TWO_PI
                alp = bet+alp0
                DX = COS(alp)
                DY = SIN(alp)
                X0 = XC+R*DX
                Y0 = YC+R*DY
              endif
              dst = (x0-a1)**2 + (y0-b1)**2
              if (dst .lt. dmin) then
                if (nwrn .le. 0) then
                  call chkrmp(crad,x0,y0,a1,b1,tolm,kcut,ier)
                  if (ier.ne.0) goto 999
                endif
                if (kcut.eq.0) then
                  jcut = 0
                  dmin = dst
                  xb = x0
                  yb = y0
                endif
              endif
            enddo
            k = k+3
          endif

          x0 = x1
          y0 = y1

          if (k .lt. nppa) goto 50

          if (jcut .ne. 0) goto 999

          if (look.eq.-1 .and. ispirl.eq.in) then
            look = 2
            xlook = xb
            ylook = yb
            do i = islnds+2, lanes
              loca(i+1) = 0
              lane(i) = 0
              loop(i) = 0
              mama(i) = 0
            enddo
            lanes = islnds+1
            return
          endif
c
c.....Extend the point with expand distance
c
          if (lwatfl .and. offdis.gt. 0.0) then               
              call entrypntoff(xb,yb,a1,b1,offdis,x2,y2)
              call chkrmp(crad,x2,y2,a1,b1,tolm,kcut,ier)
              if (ier.eq.0) then
                 xb = x2
                 yb = y2
              endif
          endif
          
          xx0 = xb/fact
          yy0 = yb/fact

        endif

      zbot = botpl(4)

      if (sfcut .and. (zbot .lt. botpl(1))) then
c
c..... qar 96082 - waterline based of revsf
c
        if (lwatfl .and. tamode .ge. 1) then
          buf(1) = xx0
          buf(2) = yy0
          buf(3) = tabot
          itp = -1
          ind = 0
          call surftl (itp, ind, buf, trans, mx, ier)
          if (ier .ne. 0) goto 999
          zbot = buf(3)
          return
        endif
c
c... bottom below high surface tool pt
c
        buf(1) = xx
        buf(2) = yy
        buf(3) = zbot
        itp = -1
        ind = 0
        call surftl (itp, ind, buf, trans, mx, ier)
        if (ier .ne. 0) goto 999
        zbot = buf(3)
      endif

      buf(1) = xx0
      buf(2) = yy0
      buf(3) = znow

999   return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  chkrmp
c **
c **  purpose of subroutine: gouge-checking for a helix; if no gouge, the
c **                         new starting point xlook,ylook is set
c **
c **********************************************************************
c **********************************************************************

      subroutine chkrmp(crad,a0,b0,a1,b1,tolm,jcut,ier)

      include 'com.com'
      include 'rrdm.com'
      include 'pokcom.com'

      real*4 a0,b0,a1,b1,crad,tolm
      integer*2 jcut
      integer*4 ier

      integer*2 isvasw(480)
      equivalence (rsvasw, isvasw)

      real*4 ro,vx,vy
      integer*2 i,ientry

        if (islnds.eq.0) return

        vx = a1 - a0
        vy = b1 - b0
        ro = vx**2+vy**2
        if (ro .lt. tolm*tolm) return

        ientry = 0
        i = 2
c
c..... isvasw(4*i-1) is 1,0,-1 if the perimeter condition is in,on,out (with
c..... i = 1); for islands it is 1,0,-1 if the condition is out,on,in
c..... For now, we do not check the perimeter gouging with the OUT condition,
c..... or an island gouging with the IN condition.
c
2215    if (isvasw(4*i-1) .eq. -1) then
          i = i+1
          if (i .ge. islnds + 2) return
          goto 2215
        endif

        ro = crad*isvasw(4*i-1) - tolm
        if (ro .lt. 0.) ro = 0.

        call chkgou (ientry,i,ro,a0,b0,a1,b1,jcut,ier,0)
        if (ier.ne.0 .or. jcut.eq.1) return

        i = i + 1
        if (i .lt. islnds + 2) goto 2215

      return
      end

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE islandinside 
c **  purpose of subroutine: check if island ix inside pocket or not
c **    input:
c **      ix ........ index
c **    output:
c **      in...........0- if outside loop, 1- if inside
c **********************************************************************
c **********************************************************************

      SUBROUTINE islandinside (ix,in,ierr)

      include 'com.com'
      include 'pokcom.com'

      real*4 xb(maxpts),yb(maxpts),xa(maxpts),ya(maxpts)
      real*4 xpt,ypt
      integer*2 nppb,nppa,ip,ix,in,jj

      jj = 0
      ierr = 0
      ip = 1
c
c...get the pocket boudanry
c
      call getlan (ip,xa,ya,nppa)
c
c...get the island boudanry
c      
      call getlan (ix,xb,yb,nppb)
      
250   jj = jj + 1  
      if (xb(jj) .ge. 0.9*flarc) then
          jj = jj + 3
      endif
      xpt = xb(jj)
      ypt = yb(jj)
     
      if (ifl(264) .eq. 0) then
          xpt = xpt*25.4
          ypt = ypt*25.4
      endif
               
      call inloop (xpt, ypt, xa, ya, nppa, in, ierr)
      if (in .eq. 0) goto 99
      if (ierr .ne. 0) goto 99
      if (jj .lt. nppb) goto 250

99    return
      end

c **********************************************************************
c **********************************************************************
c **   SUBROUTINE entrypntcut
c **  purpose of subroutine: check if entry point(xx,yy) already been cut or not
c **    input:
c **      ix ........ index
c **    output:
c **      in...........1- already cut, 0- no cut yet
c **********************************************************************
c **********************************************************************

      SUBROUTINE entrypntcut (idir,ix,xx,yy,in)

      include 'com.com'
      include 'pokcom.com'

      real*4 xb(maxpts),yb(maxpts),xa(maxpts),ya(maxpts)
      real*4 xx,yy,dist,dmin
      integer*2 nppb,nppa,ip,ix,in,jx,i,idir
      
      dmin = tol

      ierr = 0
      ip = 1
      i = locapo
      jx = i
      in  = 0
c
c...get the previous toolpath
c
200   call getlan (jx,xa,ya,nppa)

c                
c...Check if the entry point on the previous toolpath
c
      call dsptlp0 (xx,yy,xa,ya,nppa,dist)
      if (dist .lt. dmin) then
        in = 1
        goto 99
      endif
      
      i = i+1
      jx = i + idir
                    
      if (jx .lt. ix) goto 200

99    return
      end

c **********************************************************************
c **  SUBROUTINE entrypntoff
c **  purpose of subroutine: expand the entry point by offdis
c **    input:
c **      x0,y0...entry point
c **      x1,y1...vector direction of side two
c **      step......effective tool diameter value
c **    output:
c **      x2,y2...shifted entry point with STEP
c **********************************************************************
c **********************************************************************

      SUBROUTINE entrypntoff (X0,Y0,X1,Y1,STEP,X2,Y2)
      
      include 'com.com'

      real*4 fact,dis,vx,vy,x0,x1,x2,y0,y1,y2,step

      fact = 1.
      if (ifl(264).eq.0) fact = 25.4
      
      vx = x0 - x1
      vy = y0 - y1
      dis = sqrt(vx**2+vy**2)
      x2 = x0 + step * fact * vx/dis            
      y2 = y0 + step * fact * vy/dis
            
      return
      end
