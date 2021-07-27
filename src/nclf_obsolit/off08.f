C*************************************************************************
C**    NAME           : off08.f
C**       CONTAINS:
C**     subroutine getlpt
C**     subroutine lclift
C**     subroutine lcmmx0
C**     subroutine lcmmx1
C**     subroutine lcmmx
C**     subroutine lctran
C**     subroutine lcshif
C**     subroutine lcinsr
C**     subroutine lcins1
C**     subroutine lisins
C**     subroutine lcstra
C**     subroutine jcupdt1
C**     subroutine jcupdt
C**     subroutine lcarnd
C**     subroutine lcdis
C**     subroutine llpts
C**     subroutine llcut1
C**     subroutine llcutn
C**     subroutine llcut2
C**     subroutine llcut
C**     subroutine lscut1
C**     subroutine lscutn
C**     subroutine lscut2
C**     subroutine lsctfin
C**     subroutine lscut
C**     subroutine lacer
C**     subroutine lacer1
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       off08.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:21
C***********************************************************************
c**
c** copyright (c) 2003 NCCS
c**
c **********************************************************************
c **********************************************************************
c **  SUBROUTINE getlpt
C **  purpose of subroutine: GET VALUES OF A LANE/LOOP INTO ARRAY
c **    input:
C **      LAN..........LANE NUMBER OF LOOP
C **      LUP..........LOOP NUMBER OF LOOP
C **    output:
c **      INDEX........INDEX OF LANE/LOOP IN RRLN
C **      XA,YA,NPPA...STRING OF LANE/LOOP
C **      IERR.....0-OK, 2 or more - error
c **********************************************************************
c **********************************************************************
      SUBROUTINE getlpt (LAN,LUP,INDEX,xpt,ypt,ierr)

      include 'pokcom.com'

      integer*2 ierr
      real*4 xpt,ypt

      integer*2 nppa
      integer*4 kk,ier
      real*8 xx,yy

      ierr = 0

C --- FIND LANE/LOOP LOCATION
      INDEX = 0
      DO K = LANES, 1, -1
        IF (LANE(K).EQ.LAN) THEN
          IF (LOOP(K).EQ.LUP) then
C --- GET VALUES INTO xpt,ypt
            kk = LOCA(K)
            INDEX = K
            NPPA = LOCA(K+1)-kk
            if (nppa .gt. maxpts) then
              ierr = 11
              return
            endif
            call rrget(kk,xx,yy,ier)
            xpt = xx
            ypt = yy
            if (ier .gt. 1) ierr = ier
          endif
          return
        ENDIF
      enddo

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE lclift
C **  purpose of subroutine: add a tool-lift
c **********************************************************************
c **********************************************************************
      SUBROUTINE lclift

      include 'pokcom.com'

        LANES = LANES+1
        LANE(LANES) = 999
        LOOP(LANES) = 0
        LOCA(LANES+1) = LOCA(LANES)
        MAMA(LANES) = 0

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  SUBROUTINE lctrans
C **  purpose of subroutine: apply the lace (inverse) transform to a point
c **********************************************************************
c **********************************************************************
      SUBROUTINE lctrans (xp,yp,iway)

      include 'pokcom.com'

      real*4 xp,yp
      integer*2 iway

      real*4 vx,vy

      vx = xp
      vy = yp

      if (iway .eq. -1) then
        xp = xlace*vx - ylace*vy
        yp = ylace*vx + xlace*vy
      else
        xp =  xlace*vx + ylace*vy
        yp = -ylace*vx + xlace*vy
      endif

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **   subroutine name:  lctran
c **
c **  purpose of subroutine: transform input along the lacing direction.
c **
c **********************************************************************
c **********************************************************************
      subroutine lctran (xrr,yrr,nrr,ix)

      include 'pokcom.com'

      real*4 xrr(maxpts),yrr(maxpts)
      integer*2 nrr,ix

      integer*2 i,k,k0

        k0 = 1
        if (ix .gt. 0) k0 = loca(ix)
        i = 0
  100   i = i+1
        k = k0+i-1
        if (xrr(k) .ne. flarc) then
          call lctrans (xrr(k),yrr(k),1)
        else
          i = i+1
        endif
        if (i .lt. nrr) goto 100

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  lcmmx1(v,vmin1,vmax1)
c **
c **  Purpose of subroutine: update vmin,vmax for an input point.
c **
c **    input:
c **      v.................current point y-value
c **      vmin1,vmax1.......current mininmum and maximum y-values
c **    output:
c **      vmin1,vmax1.......updated mininmum and maximum y-values
c **********************************************************************
c **********************************************************************
      subroutine lcmmx1 (v,vmin1,vmax1)

      real*4 v,vmin1,vmax1

          if (vmin1 .gt. v) then
            vmin1 = v
          else if (vmax1 .lt. v) then
            vmax1 = v
          endif

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  lcmmx
c **
c **  Purpose of subroutine: update vmin,vmax for an input point, if close
c **                         to either one, update corresponding u-range.
c **
c **    input:
c **      u,v..........current point
c **      eps..........tolerance
c **    output:
c **      u00,u10......updated u-range for vmin
c **      u01,u11......updated u-range for vmax
c **********************************************************************
c **********************************************************************
      subroutine lcmmx (u,v,u00,u10,u01,u11,eps)

      include 'pokcom.com'

      real*4 eps,u,v,u00,u10,u01,u11

          if (v .lt. vmin-eps) then
              u00 = u
              u10 = u
          endif
          if (v .gt. vmax+eps) then
              u01 = u
              u11 = u
          endif

          call lcmmx1 (v,vmin,vmax)
          if (v .le. vmin+eps) call lcmmx1 (u,u00,u10)
          if (v .ge. vmax-eps) call lcmmx1 (u,u01,u11)

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  lcmmx0(xa,ya,nppa,v0,v1)
c **
c **  Purpose of subroutine: compute vmin,vmax for a contour.
c **
c **    input:
c **      xa,ya,nppa...contour points and the number of points
c **    output:
c **      v0, v1.......mininmum and maximum y-values
c **********************************************************************
c **********************************************************************
      subroutine lcmmx0(xa,ya,nppa,v0,v1)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa
      real*4 v0,v1

      real*4 v,XC,YC,R,X1,Y1,X2,Y2
      integer*2 klok,in,k

        v0 = ya(1)
        v1 = v0

        k = 1
 100    k = k+1
        if (xa(k) .eq. flarc) then
          r = xa(k+1)
          klok = ya(k+1)
          xc = xa(k+2)
          yc = ya(k+2)
          x1 = xa(k-1)
          y1 = ya(k-1)
          x2 = xa(k+3)
          y2 = ya(k+3)
          v = yc + r
          CALL INARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,xc,v,IN)
          if (in .eq. 1) call lcmmx1 (v,v0,v1)
          v = yc - r
          CALL INARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,xc,v,IN)
          if (in .eq. 1) call lcmmx1 (v,v0,v1)
          k = k+2
        else
          v = ya(k)
          call lcmmx1 (v,v0,v1)
        endif
        if (k .lt. nppa) goto 100

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  function kolo
c **
c **  Purpose of subroutine: A MOD-type function.
c **
c **********************************************************************
c **********************************************************************
      integer*2 function kolo(i,npa)

      integer*2 i,npa
        if (i .gt. npa) then
          kolo = i-npa
        else if (i .lt. 1) then
          kolo = i+npa
        else
          kolo = i
        endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lcshif (iarc,xa,ya,k,xi,yi,nppa,ierr)
c **  purpose of subroutine: shift the contour points forward and insert
c **  a new in-segment or in-arc point.
c **    input:
c **      iarc.........0 iff a segment point is inserted, 1 iff arc point
c **      XA,YA,NPPA...STRINGS AND NO OF POINTS
c **      k   .........current index
c **      xi,yi........point to insert
c **    output:
c **      XA,YA,NPPA...updated string and number of points
c **      IERR.........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lcshif (iarc,xa,ya,k,xi,yi,nppa,ierr)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts),xi,yi
      integer*2 iarc,k,nppa,ierr

      integer*2 j,k1

        if (iarc .eq. 1) then
c..... new arc point inserted into an existing arc
          do j = nppa,k,-1
            xa(j+4) = xa(j)
            ya(j+4) = ya(j)
          enddo
          xa(k+4) = xi
          ya(k+4) = yi
          k = k+4
          nppa = nppa+4
        else
          k1 = k+1
          if (k .lt. nppa) then
c..... new segment point when the segment is not last
            do j = nppa,k1,-1
              xa(j+1) = xa(j)
              ya(j+1) = ya(j)
            enddo
            xa(k1) = xi
            ya(k1) = yi
          else
c..... when the segment is last - the point is just appended
            xa(k1) = xi
            ya(k1) = yi
          endif
          k = k1
          nppa = nppa+1
        endif

        if (nppa .gt. maxpts) ierr = 13

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lcins1 (xa,ya,nppa,v,tolsq,ibot,ierr)
c **  purpose of subroutine: insert in-segment (in-arc) v-level
c **                         intersections
c **    input:
c **      XA,YA,NPPA...STRINGS AND NO OF POINTS
c **      v............the level
c **      ibot.........1 if at the bottom, -1 if at the top
c **      tolsq
c **    output:
c **      XA,YA,NPPA...updated string and number of points
c **      IERR...........0 -OK, OTHER- ERROR
c **********************************************************************
c **********************************************************************
      subroutine lcins1 (xa,ya,nppa,v,tolsq,ibot,ierr)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa,ibot,ierr
      real*4 v,tolsq

      real*4 x1,y1,x2,y2,cx,cy,d1,d2,dy
      integer*2 k,k1,k2,klok,kolo,in,iarc

        k = 0
 100    k = k+1
        iarc = 0
        k1 = kolo(k+1,nppa)
        if (xa(k1) .eq. flarc) then
          iarc = 1
          r = xa(k1+1)
          klok = ya(k1+1)
          cx = xa(k1+2)
          cy = ya(k1+2)
          x1 = xa(k)
          y1 = ya(k)
          k2 = kolo(k+4,nppa)
          x2 = xa(k2)
          y2 = ya(k2)
          dy = (cy - v)*ibot
          if (abs(dy - r) .lt. tol) then
c
c..... insert tangency if not coincident with an endpoint
c
            CALL INARC (cx,cy,R,KLOK,X1,Y1,X2,Y2,cx,v,IN)
            if (in .eq. 1) then
              d1 = (x1-cx)**2 + (y1-v)**2
              d2 = (x2-cx)**2 + (y2-v)**2
              if (d1.gt.tolsq .and. d2.gt.tolsq) then
                call lcshif (iarc,xa,ya,k,cx,v,nppa,ierr)
                if (ierr .gt. 0) return
              endif
            endif
          endif
          k = k+3
        endif
        if (k .lt. nppa) goto 100

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lcins2 (xa,ya,nppa,v,tolsq,ierr)
c **  purpose of subroutine: insert in-segment (in-arc) v-level
c **                         intersections
c **    input:
c **      XA,YA,NPPA...STRINGS AND NO OF POINTS
c **      v   .........the level
c **      tolsq
c **    output:
c **      XA,YA,NPPA...updated string and number of points
c **      IERR...........0 -OK, OTHER- ERROR
c **********************************************************************
c **********************************************************************
      subroutine lcins2 (xa,ya,nppa,v,tolsq,ierr)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa,ierr
      real*4 v,tolsq

      real*4 x1,y1,x2,y2,cx,cy,dx,d1,d2,dy,tol1
      integer*2 k,k1,k2,klok,kolo,in,iarc

        tol1 = tol/2.

        k = 0
 100    k = k+1
        iarc = 0
        k1 = kolo(k+1,nppa)
        if (xa(k1) .eq. flarc) then
          iarc = 1
          r = xa(k1+1)
          klok = ya(k1+1)
          cx = xa(k1+2)
          cy = ya(k1+2)
          x1 = xa(k)
          y1 = ya(k)
          k2 = kolo(k+4,nppa)
          x2 = xa(k2)
          y2 = ya(k2)
          dy = abs (cy - v)
          if (dy .le. r-tol) then
c
c..... insert non-tangent circle intersection(s) if not coincident
c..... with an endpoint
c
            dx = sqrt(r**2 - (cy-v)**2)
            if ((y1-v)*klok .lt. -tol) dx = -dx
            xi = cx - dx
            d1 = (x1-xi)**2 + (y1-v)**2
            d2 = (x2-xi)**2 + (y2-v)**2
            if (d1.gt.tolsq .and. d2.gt.tolsq) then
              CALL INARC (cx,cy,R,KLOK,X1,Y1,X2,Y2,xi,v,IN)
              if (in .eq. 1) then
                call lcshif (iarc,xa,ya,k,xi,v,nppa,ierr)
                if (ierr .gt. 0) return
              endif
            endif
            xi = cx + dx
            d1 = (x1-xi)**2 + (y1-v)**2
            d2 = (x2-xi)**2 + (y2-v)**2
            if (d1.gt.tolsq .and. d2.gt.tolsq) then
              CALL INARC (cx,cy,R,KLOK,X1,Y1,X2,Y2,xi,v,IN)
              if (in .eq. 1) then
                call lcshif (iarc,xa,ya,k,xi,v,nppa,ierr)
                if (ierr .gt. 0) return
              endif
            endif
          endif
          k = k+3
        else
          y1 = v - ya(k)
          y2 = ya(k1) - v
          if (abs(y1).gt.tol1 .and. abs(y2).gt.tol1 .and. y1*y2.gt.0)
     x                                                           then
            xi = xa(k) + y1*(xa(k1)-xa(k))/(ya(k1)-ya(k))
            call lcshif (iarc,xa,ya,k,xi,v,nppa,ierr)
            if (ierr .gt. 0) return
          endif
        endif
        if (k .lt. nppa) goto 100

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lcinsr (xa,ya,nppa,nstp,ierr)
c **  purpose of subroutine: insert in-segment (in-arc) all v-level
c **                         intersections into a contour
c **    input:
c **      xa,ya,nppa...contour points and the number of points
c **      nstp.........number of v-level steps
c **      dva..........if not zero, an extra level intersections at top and 
c **                   bottom
c **      tolsq
c **    output:
c **      xa,ya,nppa...updated string and number of points
c **      IERR.........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lcinsr (dva,xa,ya,nppa,nstp,tolsq,ierr)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa,nstp,ierr

      real*4 v,dva,tolsq
      integer*2 n,ibot

        ibot = 1
        call lcins1 (xa,ya,nppa,vmin,tolsq,ibot,ierr)
        if (ierr .gt. 0) return

        if (dva .gt. 0) then
          v = vmin + dva
          call lcins2 (xa,ya,nppa,v,tolsq,ierr)
          if (ierr .gt. 0) return
        endif

        v = vmin
        do n = 1,nstp-1
          v = vmin + n*stpcur
          call lcins2 (xa,ya,nppa,v,tolsq,ierr)
          if (ierr .gt. 0) return
        enddo

        if (dva .gt. 0) then
          v = vmax - dva
          call lcins2 (xa,ya,nppa,v,tolsq,ierr)
          if (ierr .gt. 0) return
        endif

        ibot = -1
        call lcins1 (xa,ya,nppa,vmax,tolsq,ibot,ierr)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lisins (i,nstp,ierr)
c **  purpose of subroutine: insert in-segment (in-arc) all v-level
c **                         intersections into a contour
c **    input:
c **      xa,ya,nppa...contour points and the number of points
c **      nstp.........number of v-levels
c **    output:
c **      xa,ya,nppa...updated string and number of points
c **      IERR.........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lisins (is,nstp,ierr)

      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid
      common/yisl/yisl0(100),yisl1(100),iisl(100),isldir(100)
      real*4 yisl0,yisl1
      integer*2 iisl,isldir

      integer*2 is,nstp,ierr

      real*4 v,tolsq
      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa,n

        tolsq = tol**2
        CALL getlan (iisl(is),xa,ya,nppa)
        nppa = nppa-1

        isldir(is) = 0

        v = vmin
        do n = 1,nstp
          if (yisl0(is).lt.(v-tol) .and. yisl1(is).gt.(v+tol))
     x      call lcins2 (xa,ya,nppa,v,tolsq,ierr)
          if (ierr .gt. 0) return
          v = v + stpcur
        enddo

        if (ierr.eq.0) call lisadd (is,xa,ya,nppa)

      return
      end

c **********************************************************************
c **********************************************************************
c **  9. SUBROUTINE lcstra (xa,ya,nppa)
C **  purpose of subroutine: REMOVES STRAIGHT LINE POINTS AND
c **                         overlapping ends
c **    input/output:
C **      XA,YA...STRING OF POINTS
C **      NPPA....NUMBER OF POINTS
c **********************************************************************
c **********************************************************************

      SUBROUTINE lcstra (xa,ya,nppa)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)

      integer*2 npp,i,j
      real*4 erad,ee,e2,x1s,y1s,x2e,y2e,x1,x2,y1,y2,d1,d2,si

      NPP = NPPA+1
      ERAD = tol/25.4
      EE = erad*.5
      e2 = erad*erad

C --- CLEAR middle OF redundant data
      I = 0
   23 I = I+1
      IF (I.GE.NPP-1) GOTO 35
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
                DO J = I+4, NPP-4
                  XA(J) = XA(J+4)
                  YA(J) = YA(J+4)
                enddo
                I = I-1
                NPP = NPP-4
                GO TO 23
              end if
             end if
            end if
          END IF
        END IF
        I = I+3
        GOTO 23
      END IF
      IF (I+2.GE.NPP) GOTO 35
      IF (XA(I+2).EQ.FLARC) GOTO 23
      X1 = XA(I+1)-XA(I)
      Y1 = YA(I+1)-YA(I)
      D1 = X1*X1+Y1*Y1
      if (d1 .gt. e2) then
        X2 = XA(I+2)-XA(I+1)
        Y2 = YA(I+2)-YA(I+1)
        D2 = X2*X2+Y2*Y2
        if (d2 .gt. e2) then
          SI = X1*Y2-Y1*X2
          si = si**2
        endif
      endif
      IF ((d1.le.e2) .or. (d2.le.e2). or. (SI.lt.d1*d2*E2)) THEN
C --- REMOVE in-LINE POINT
        DO J = I+1, NPP-1
          XA(J) = XA(J+1)
          YA(J) = YA(J+1)
        enddo
        I = I-1
        NPP = NPP-1
      END IF
      GOTO 23

   35 NPPA = NPP-1

      RETURN
      END

c **********************************************************************
c **********************************************************************
c **  subroutine lcdis (xa,ya,nppa,i1,i2,dis,d0,ierr)
c **  Purpose of subroutine: compute the squared distance between points
c **  along the current contour.
c **    input:
c **      xa,ya,nppa.....contour points and the number of points
c **      i1,i2..........first and last points
c **      d0.............current longest distance, the routine exits if d0
c **                     is exceeded.
c **    output:
c **      dis............distance
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lcdis (xa,ya,nppa,i1,i2,dis,d0,ierr)

      include 'const.com'
      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts),dis,d0
      integer*2 i1,i2,nppa,ierr

      real*4 co,a,x1,y1,x2,y2,cx,cy,r
      integer*2 k,k1,k2,kolo

        k = 0
        dis = 0
        x1 = xa(i1)
        y1 = ya(i1)

 200    k = k+1
        k1 = kolo(i1+k,nppa)
        if (xa(k1) .eq. flarc) then
          r = xa(k1+1)
          cx = xa(k1+2)
          cy = ya(k1+2)
          k2 = kolo(k1+3,nppa)
          x2 = xa(k2)
          y2 = ya(k2)
          co = ((x1-cx)*(x2-cx) + (y1-cy)*(y2-cy))/(r*r)
          a = acos(co)
          dis = dis + r*PI*a
          k = k+3
          k1 = k2
        else
          x2 = xa(k1)
          y2 = ya(k1)
          dis = dis + sqrt((x2-x1)**2 + (y2-y1)**2)
        endif
        x1 = x2
        y1 = y2
        if (k .ge. nppa) then
          ierr = 13
          return
        endif
        if (dis.lt.d0 .and. k1.ne.i2) goto 200

        return
        end

c **********************************************************************
c **********************************************************************
c **  subroutine lssto (lan,mom,xc,yc,nppc,ierr)
c **  purpose of subroutine: store the next cut after weeding
c **********************************************************************
c **********************************************************************
      subroutine lssto (lan,mom,xc,yc,nppc,ierr)

      include 'pokcom.com'

      real*4 xc(maxpts),yc(maxpts)
      integer*2 lan,mom,nppc,ierr,lup

         lup = 2

         call lcstra (xc,yc,nppc)
         if (lctflg) call lctran (xc,yc,nppc,0)
         call stolap (lup,lan,mom,xc,yc,nppc,ierr)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lcaddarc (nppc,xc,yc,r,klok,cx,cy)
c **  purpose of subroutine: add arc parameters to a contour
c **********************************************************************
c **********************************************************************
      subroutine lcaddarc (nppc,xc,yc,r,cklok,cx,cy)

      include 'pokcom.com'

      real*4 xc(maxpts),yc(maxpts)
      real*4 r,cklok,cx,cy
      integer*2 nppc

      nppc = nppc+1
      xc(nppc) = flarc
      yc(nppc) = 0
      nppc = nppc+1
      xc(nppc) = r
      yc(nppc) = cklok
      nppc = nppc+1
      xc(nppc) = cx
      yc(nppc) = cy

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lcarnd (xa,ya,nppa,i1,i2,xc,yc,nppc,ierr)
c **  purpose of subroutine: add the shortest chain between two contour
c **                         points to the current level cut.
c **    input:
c **      xa,ya,nppa.....current contour: points and the number of points
c **      xc,yc,nppc.....current level cut: points and the number of points
c **      i1,i2..........first and last contour points
c **      jcut..........."cut" flags for current contour
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lcarnd (lan,mom,xa,ya,jcut,jdir,nppa,i1,i2,xc,yc,nppc,
     x                                                            ierr)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts),xc(maxpts),yc(maxpts)
      integer*2 lan,mom,i1,i2,nppa,nppc,ierr,jdir
      integer*2 jcut(maxpts)

      real*4 d1,d2,r,cklok,cx,cy
      integer*2 i,k,j,kolo
      integer*2 jfwd,ilst
      logical lifted,latarc

      integer*2 bshrt,bsame,bccw,bcw
      parameter (bshrt=0,bsame=1,bccw=2,bcw=3)

        d1 = 0
        d2 = flarc
        lifted = .false.
        jfwd = 0

        if (bddir .eq. bsame) then
          jfwd = jdir
        else if (bddir .eq. bccw) then
          jfwd = 1
        else if (bddir .eq. bcw) then
          jfwd = -1
        endif

        if (jfwd .eq. 0) then
          jfwd = 1
          call lcdis (xa,ya,nppa,i1,i2,d1,d2,ierr)
          if (ierr .gt. 0) return
          call lcdis (xa,ya,nppa,i2,i1,d2,d1,ierr)
          if (ierr .gt. 0) return

          if (d2 .lt. d1) jfwd = -1
          if (bddir .eq. bsame) jdir = jfwd
        endif

        xc(nppc) = xa(i1)
        yc(nppc) = ya(i1)

        if (lboth) then
          if (jcut(i1).eq.1 .and. nppc.gt.1) then

            call lssto (lan,mom,xc,yc,nppc,ierr)

            if (ierr.gt.0) return

            call lclift
            lifted = .true.
            nppc = 0
            ilst = i1
          else
            jcut(i1) = 1
          endif
        endif

        if (jfwd .eq. 1) then
          k = 0
 300      k = k+1
          i = kolo(i1+k,nppa)
          if (lboth .and. i .ne. i2) then
              if (.not.lifted) then
                if (jcut(i).eq.1) then
                  nppc = nppc+1
                  xc(nppc) = xa(i)
                  yc(nppc) = ya(i)
                  call lssto (lan,mom,xc,yc,nppc,ierr)
                  if (ierr.gt.0) return
                  call lclift
                  lifted = .true.
                  nppc = 0
                  ilst = i
                  goto 310
                endif
              else
                if (jcut(i).eq.1) then
                  ilst = i
                  goto 310
                else
                  lifted = .false.
                  nppc = nppc+1
                  xc(nppc) = xa(ilst)
                  yc(nppc) = ya(ilst)
                endif
              endif
          endif
          if (i.eq.i2 .and. jcut(i).eq.0 .and. lifted) then
            nppc = nppc+1
            xc(nppc) = xa(ilst)
            yc(nppc) = ya(ilst)
          endif
          nppc = nppc+1
          xc(nppc) = xa(i)
          yc(nppc) = ya(i)
 310      jcut(i) = 1
          if (i .ne. i2) goto 300
        else
          i = i1
 400      i = kolo(i-1,nppa)
          j = kolo(i-2,nppa)
          latarc = (xa(j) .eq. flarc)
          if (latarc) then
c
c..... reverse arcs on the way
c
            i = j-1
            r = xa(j+1)
            cklok = -ya(j+1)
            cx = xa(j+2)
            cy = ya(j+2)
            if (.not.lifted .and. nppc.gt.0)
     x        call lcaddarc (nppc,xc,yc,r,cklok,cx,cy)
          endif
          if (lboth .and. i .ne. i2) then
              if (.not.lifted) then
                if (jcut(i).eq.1) then
                  nppc = nppc+1
                  xc(nppc) = xa(i)
                  yc(nppc) = ya(i)
                  call lssto (lan,mom,xc,yc,nppc,ierr)
                  if (ierr.gt.0) return
                  call lclift
                  lifted = .true.
                  nppc = 0
                  ilst = i
                  goto 410
                endif
              else
                if (jcut(i).eq.1) then
                  ilst = i
                  goto 410
                else
                  lifted = .false.
                  nppc = nppc+1
                  xc(nppc) = xa(ilst)
                  yc(nppc) = ya(ilst)
                  if (latarc)
     x              call lcaddarc (nppc,xc,yc,r,cklok,cx,cy)
                endif
              endif
          endif
          if (i.eq.i2 .and. jcut(i).eq.0 .and. lifted) then
            nppc = nppc+1
            xc(nppc) = xa(ilst)
            yc(nppc) = ya(ilst)
            if (latarc)
     x        call lcaddarc (nppc,xc,yc,r,cklok,cx,cy)
          endif
          nppc = nppc+1
          xc(nppc) = xa(i)
          yc(nppc) = ya(i)
 410      jcut(i) = 1
          if (i .ne. i2) goto 400
        endif

        nppc = nppc+1
        if (nppc .gt. maxpts) ierr = 13

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine jcupdt1 (ldown,v,xa,ya,jca,jcb,nppa)
c **  purpose of subroutine: mark all boundary below the last level as "cut";
c **                         copy current "cut" flags between CLW and CCLW
c **********************************************************************
c **********************************************************************
      subroutine jcupdt1 (ldown,v,xa,ya,jca,jcb,nppa)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      real*4 v
      logical ldown
      integer*2 jca(maxpts),jcb(maxpts)
      integer*2 nppa

      integer*2 i
      real*4 v1,pm

        i = 0
        if (ldown) then
          v1 = -v - tol
          pm = -1.
        else
          v1 = v - tol
          pm = 1.
        endif

 200    i = i+1
        if (jca(i) .eq. 0 .and. pm*ya(i) .lt. v1) then
           jca(i) = 1
        endif
        if (i+1 .lt. nppa) then
          if (xa(i+1) .eq. flarc) i = i+3
          goto 200
        endif

        call jcupdt (jca,jcb,nppa)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine jcupdt (jca,jcb,nppa)
c **  purpose of subroutine: copy current "cut" flags between CLW and CCLW
c **********************************************************************
c **********************************************************************
      subroutine jcupdt (jca,jcb,nppa)

      include 'pokcom.com'

      integer*2 jca(maxpts),jcb(maxpts)
      integer*2 nppa

      integer*2 i,j

        if (jca(1) .eq. 1) jcb(1) = 1
        do i = 2,nppa
          j = nppa + 2 - i
          if (jca(i) .eq. 1) jcb(j) = 1
        enddo

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine llcut1 (lan,mom,xa,ya,nppa,v,klok,ierr)
c **  purpose of subroutine: compute the first level lace pocketing cut
c **  for the current contour in the current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **      klok...........contour orientation: 1 if CCW, -1 if CW
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine llcut1 (lan,mom,xa,ya,nppa,v,klok,ierr)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 lan,mom,nppa,ierr,klok
      real*4 v

      real*4 x0,xb(2),yb(2)
      integer*2 i,i1,i2,i0,k,nppb,lup,l,kolo

        if (klok .eq. -1) call flipo (xa,ya,nppa+1)
c
c..... loop number should not be 1, or this cut will be interpreted as
c..... final (CUTCOM problem - qar 96246)
c
c.....change back from 2 to 1, as it causes a lot of other lace problems,
c.....change back to 2 before  call stolap(), to fix CUTCOM problem
c
        lup = 1
        nppb = 2
c
c..... find min x
c
        x0 = flarc
        i0 = -1
        k = 0
 100    k = k+1
        if (xa(k) .lt. x0) then
          x0 = xa(k)
          i0 = k
        endif
        if (xa(k+1) .eq. flarc) k = k+3
        if (k .lt. nppa) goto 100
c
c..... starting with the min x segment, go along until find a segment on
c..... the v-level
c
        k = -1
 200    k = k+1
        i = kolo(i0 + k,nppa)
        i1 = kolo(i + 1,nppa)
        if (xa(i1) .eq. flarc) then
          k = k+3
        else if (abs(ya(i)-v).lt.tol .and. abs(ya(i1)-v).lt.tol .and.
     *           abs(xa(i1)-xa(i)).gt.tol) then
          xb(1) = xa(i)
c
c..... get the largest segment, possibly consisting of several consecutive
c..... ones, if all within tol to the v-level
c
          l = 0
 300      l = l+1
          if (l .eq. nppa) then
            ierr = 13
            return
          endif
          i2 = kolo(i1+1,nppa)
          if (xa(i2).lt.flarc .and. abs(ya(i2)-v).lt.tol) then
            i1 = i2
            k = k+1
            goto 300
          endif
          xb(2) = xa(i1)
          yb(1) = v
          yb(2) = v
          if (lctflg) call lctran (xb,yb,nppb,lup)
          lup=2
          call stolap(LUP,LAN,MOM,XB,YB,NPPB,IERR)
          if (ierr.gt.0) return
c..... lift and retract
          call lclift
          k = k+1
        endif
        if (k .lt. nppa-1) then
          lup = 1
          goto 200
        endif

        if (klok .eq. -1) call flipo (xa,ya,nppa+1)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine llpts (lan,xa,ya,nppa,v)
c **  purpose of subroutine: compute the next level lace pocketing cut
c **  for the current contour in the current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine llpts (lan,xa,ya,nppa,v,ilace)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 lan,nppa,ilace
      real*4 v

      real*4 x,y,x1,y1,dx,dy,dy0,dy1,cx,cy,r,tolsq,tol1
      integer*2 ki,k,k0,k1,kolo,klok

        ki = 0
        tolsq = tol**2
        tol1 = 0.5*tol

        k1 = 1
 100    k = k1
        k1 = kolo(k+1,nppa)
        if (xa(k1) .eq. flarc) k1 = kolo(k1+3,nppa)

        dy = ya(k) - v
        dy1 = ya(k1) - v
c
c...... dy > 0 iff current vertex is above, < 0 iff below, = 0 iff on
c...... dy1 > 0 iff next vertex is above, < 0 iff below, = 0 iff on
c
        if (abs(dy) .lt. tol1) then
c
c..... add vertices on the v-level if next to a wall
c
          k0 = kolo(k-1,nppa)
          if (k0.gt.3 .and. xa(k0-2).eq.flarc) k0 = k0-3
          dy0 = ya(k0) - v
c
c...... dy0 > 0 iff previous vertex was above, < 0 iff below
c
          if (abs(dy1) .lt. tol1) then
            dx = xa(k1) - xa(k)
            if ((lan.lt.100 .and. (dx*dy0.lt.0)) .or.
     *          (lan.gt.100 .and. (dx*dy0.gt.0))) then
              call llpush (k,lan,xa(k))
            endif
          else if (abs(dy0).lt.tol1) then
            dx = xa(k) - xa(k0)
            if ((lan.lt.100 .and. (dx*dy1.lt.0)) .or.
     *          (lan.gt.100 .and. (dx*dy1.gt.0))) then
              call llpush (k,lan,xa(k))
            endif
          else
            if (dy0*dy1 .lt. 0) call llpush (k,lan,xa(k))
          endif
        endif
c..... add 'clean' intersections
        if (ilace.eq.1 .and. xa(k+1) .eq. flarc) then
          r = xa(k+2)
          klok = ya(k+2)
          cx = xa(k+3)
          cy = ya(k+3)
          x = xa(k)
          y = ya(k)
          x1 = xa(k1)
          y1 = ya(k1)
          if (abs(cy - v) .le. r-tol1) then
            dx = sqrt(r**2 - (cy-v)**2)
            xi = cx - dx
            if ((dy**2 + (xi-x)**2 .ge. tolsq) .and.
     *          (dy1**2 + (xi-x1)**2 .ge. tolsq)) then
              call inarc (cx,cy,r,klok,x,y,x1,y1,xi,v,in)
              if (in .eq. 1) call llpush (ki,lan,xi)
            endif
            xi = cx + dx
            if ((dy**2 + (xi-x)**2 .ge. tolsq) .and.
     *          (dy1**2 + (xi-x1)**2 .ge. tolsq)) then
              call inarc (cx,cy,r,klok,x,y,x1,y1,xi,v,in)
              if (in .eq. 1) call llpush (ki,lan,xi)
            endif
          endif
        else if (ilace.eq.1 .and. abs(dy).ge.tol1 .and.
     *             abs(dy1).ge.tol1 .and. dy*dy1 .lt. 0) then
          xi = xa(k) - dy*(xa(k1)-xa(k))/(ya(k1)-ya(k))
          call llpush (ki,lan,xi)
        endif
        if (k.lt.nppa .and. k.lt.k1) goto 100

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine llcutn (lan,mom,xa,ya,nppa,v,ierr)
c **  purpose of subroutine: compute the next level lace pocketing cut
c **  for the current contour in the current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine llcutn (lan,mom,xa,ya,nppa,v,xl,yl,ierr)

      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid
      common/yisl/yisl0(100),yisl1(100),iisl(100),isldir(100)
      real*4 yisl0,yisl1
      integer*2 iisl,isldir

      real*4 xa(maxpts),ya(maxpts)
      integer*2 lan,mom,nppa,ierr
      real*4 v,xl,yl

      real*4 xb(2),yb(2)
      integer*2 ile,nppb,nppc,lup,klok,i1,l1,npt
      real*4 xc(maxpts),yc(maxpts)

        nppb = 1
        call llnul
        lup = 1
c
c..... add perimeter intersections
c
        call llpts (lan,xa,ya,nppa,v,lup)
c
c..... add island intersections
c
        DO 32 ILE = 101, islnds+100
          i = ile - 100
          if (insid(i).eq.lan .and.
     *        yisl0(i).lt.(v-tol) .and. yisl1(i).gt.(v+tol)) then
            CALL getlan (iisl(i),xc,yc,nppc)
            call llpts (ile,xc,yc,nppc-1,v,lup)
          endif
  32    continue

        klok = 1
        call llsort (nppb,klok)
        if (nppb .lt. 2) return
c
c..... loop number should not be 1, or this cut will be interpreted as
c..... final (CUTCOM problem - qar 96246)
c
c.....change back from 2 to 1, as it causes a lot of other lace problems,
c.....change back to 2 before  call stolap(), to fix CUTCOM problem
c
        lup =1
        npt = 2
 200    call llpop (i1,l1,xb(1))
        call llpop (i1,l1,xb(2))
        yb(1) = v
        yb(2) = v
        xl = xb(nppb)
        yl = yb(nppb)
        if (lctflg) call lctran (xb,yb,npt,lup)
        lup = 2
        call stolap(lup,lan,mom,xb,yb,npt,ierr)
        if (ierr .gt. 0) return
c
c..... retract
c
        call lclift
        nppb = nppb-2
        if (nppb .gt. 1) then
            lup = 1
            goto 200
        endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine llcut2 (lan,mom,xa,ya,nppa,v,ierr)
c **  purpose of subroutine: compute the final level lace pocketing cut
c **  for the current contour in the current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine llcut2 (lan,mom,xa,ya,nppa,v,xl,yl,ierr)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 lan,mom,nppa,ierr
      real*4 v,xl,yl

      real*4 xb(2),yb(2)
      integer*2 i,k,k0,k1,nppb,lup,klok,i1,l1,kon,kon0,kon1

c
c..... loop number should not be 1, or this cut will be interpreted as
c..... final (CUTCOM problem - qar 96246)
c
c.....change back from 2 to 1, as it causes a lot of other lace problems,
c.....change back to 2 before  call stolap(), to fix CUTCOM problem
c
        lup = 1
        nppb = 1
        call llnul

        k0 = 1
        kon0 = 0
        if (abs(ya(k0) - v) .lt. tol) kon0 = 1

        k1 = 2
        if (xa(k1) .eq. flarc) k1 = k1+3
        kon1 = 0
        if (abs(ya(k1) - v) .lt. tol) kon1 = 1

 100    k = k1
        kon = kon1
        k1 = k+1
        if (k1 .gt. nppa) k1 = 1
        if (xa(k1) .eq. flarc) k1 = k1+3
c
c..... add vertices if next to a wall
c
        kon1 = 0
        if (abs(ya(k1) - v) .lt. tol) kon1 = 1
        if (kon.eq.1 .and. (kon0+kon1).eq.1) call llpush (k,lan,xa(k))
        k0 = k
        kon0 = kon
        if (k .lt. nppa) goto 100

        klok = 1
        call llsort (nppb,klok)
        if (nppb.lt.2) return
        i = 2
 200    call llpop (i1,l1,xb(1))
        call llpop (i1,l1,xb(2))
        yb(1) = v
        yb(2) = v
        if (lctflg) call lctran (xb,yb,i,lup)
        lup = 2
        call stolap(LUP,LAN,MOM,XB,YB,i,IERR)
        if (ierr.gt.0) return
        xl = xb(nppb)
        yl = yb(nppb)

        call lclift
        nppb = nppb-2
        if (nppb .gt. 1) goto 200

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine llcut (lan,mom,xa,ya,nppa,nstp,ldown,ierr)
c **  purpose of subroutine: compute lace pocketing for one contour in the
c **  current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      step...........distance between cuts
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine llcut (lan,mom,xa,ya,nppa,nstp,ldown,lflip,ierr)

      include 'rrdm.com'
      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 lan,mom,nppa,nstp,ierr
      logical ldown,lflip

      real*4 v,v0,v1,dv,xl,yl,useg
      integer*2 klok,n,lanes0,lup,icen,lseg

        if (ldown) then
          klok = -1
          v0 = vmax
          v1 = vmin
          dv = -stpcur
        else
          klok = 1
          v0 = vmin
          v1 = vmax
          dv = stpcur
        endif
c
c..... first level
c
        lanes0 = lanes
        call llcut1 (lan,mom,xa,ya,nppa,v0,klok,ierr)

        if (ierr.gt.0 .or. nstp.lt.1) return

        if (lanes.eq.lanes0) then
          v = v0+dv/8.
          call llcutn (lan,mom,xa,ya,nppa,v,xl,yl,ierr)
          if (ierr .gt. 0) return
        endif

        v = v0
c..... remaining levels
        do n = 1,nstp-1
          v = v + dv
          if (v .gt.vmax+tol .and. v.lt.vmin-tol) return
          call llcutn (lan,mom,xa,ya,nppa,v,xl,yl,ierr)
          if (ierr .gt. 0) return
        enddo
c
c..... last level
c
        lanes0 = lanes
        call llcut2 (lan,mom,xa,ya,nppa,v1,xl,yl,ierr)

        if (lanes.eq.lanes0) then
          v = v1-dv/8.
          call llcutn (lan,mom,xa,ya,nppa,v,xl,yl,ierr)
          if (ierr .gt. 0) return
        endif
c
c..... add final pass around the contour
c
      if (lcfin .ne. fpnon) then

      lflip = (lcfin.eq.fpclw .or. (lcfin.eq.fpsam .and. .not.ldown)
     x                        .or. (lcfin.eq.fprev .and. ldown))

      if (lflip) call flipo (xa,ya,nppa+1)

      CALL lcfind (XA,YA,NPPA,LSEG,USEG,xl,yl,IERR)
      IF (IERR.eq.0) then
        ICEN = 1
        nppa = nppa + 1
        xa(nppa) = xa(1)
        ya(nppa) = ya(1)
        CALL SHUFFLE (XA,YA,NPPA,LSEG,ICEN,USEG,xl,yl)
        call lcstra (xa,ya,nppa)
        lup = 1
        if (lctflg) call lctran (xa,ya,nppa,lup)
        lane(lanes) = -1
c
c..... qar 96246: added missing corner rounding
c
        if (rcorn .gt. 0) call filcor(xa,ya,nppa,rcorn,0)

        call stolap(LUP,LAN,MOM,xa,ya,nppa,IERR)
        call lclift
      else
        ierr = 0
      endif

      endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine xminaty(xa,ya,nppa,v,i0,x0)
c **  purpose of subroutine: find leftmost vertex at a given y-level.
c **    input:
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............y-level
c **    output:
c **      i0 ...........index of leftmost vertex at v, or -1 if not found
c **      x0 ...........least x at level v
c **********************************************************************
c **********************************************************************
      subroutine xminaty(xa,ya,nppa,v,i0,x0)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 nppa,i0,k
      real*4 v,x0

        x0 = flarc
        i0 = -1
        k = 0
 100    k = k+1
        if (xa(k).lt.x0 .and. (abs(ya(k) - v).lt.tol)) then
          x0 = xa(k)
          i0 = k
        endif
        if (xa(k+1) .eq. flarc) k = k+3
        if (k .lt. nppa) goto 100
        
      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lscut1 (lan,mom,xa,ya,nppa,v,vn,ifin,ierr)
c **  purpose of subroutine: compute the first level lace pocketing cut
c **  for the current contour in the current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **      vn.............next y-level
c **    output:
c **      ifin...........index of last xa,ya contour point added to the cut
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lscut1 (lan,mom,xa,ya,jcut,nppa,v,dva,vn,ifin,ierr)

      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 jcut(maxpts)
      integer*2 lan,mom,nppa,ifin,ierr
      real*4 v,dva,vn

      real*4 xb(maxpts),yb(maxpts)
      real*4 x0,x1,tol1,sta,stb,a,b,a0,b0,a1,b1,c,d,d0,d1,t,pm
      integer*2 i,i0,i1,k,k1,nppb,lup,kolo

        lup = 1
        nppb = 1
        tol1 = 5.*tol
        sta = (stpcur - tol)**2
        stb = (stpcur + tol1)**2
        pm = 1
        if (vn .lt. v) pm = -1
c
c..... find leftmost vn-level vertex i0
c
 10     continue
        call xminaty(xa,ya,nppa,vn,i0,x0)

        if (i0 .lt. 0) then
          ierr = 51
          return
        endif
c
c..... find leftmost v-level vertex i1
c
        call xminaty(xa,ya,nppa,v,i1,x1)

        if (i1 .lt. 0) then
          ierr = 51
          return
        endif

        d = (v-vn)**2 + (x1-x0)**2
        if (x1.gt.x0 .and. d.gt.stb) then
c
c..... if i1 and i0 are too far apart we start at a point at the distance
c..... stpcur from i0
c
          a0 = 0
          b0 = 0
          d0 = 0
c
c..... find new i0 such that the next vertex i1 is the first one at no less
c..... than stpcur from (x0,vn)
c
 120      i1 = kolo(i0 + 1,nppa)
          if (xa(i1) .eq. flarc) i1 = i1+3
          a1 = x0 - xa(i1)
          b1 = vn - ya(i1)
          d1 = a1**2 + b1**2
          if (d1 .lt. sta) then
            i0 = i1
            d0 = d1
            a0 = a1
            b0 = b1
            goto 120
          endif
          if (d1 .lt. stb) then
            i0 = i1
            xb(1) = xa(i1)
            yb(1) = ya(i1)
          else if (xa(i0+1) .eq. flarc) then
            xb(1) = xa(i0)
            yb(1) = ya(i0)
          else
            a = xa(i1)-xa(i0)
            b = ya(i1)-ya(i0)
            d = sqrt(a**2 + b**2)
            a = a/d
            b = b/d
            c = a*a0 + b*b0
            t = c + sqrt(c**2 + stpcur**2 - d0)
            xb(1) = xa(i0) + t*a
            yb(1) = ya(i0) + t*b
          endif
        else
          i0 = i1
          xb(1) = xa(i0)
          yb(1) = ya(i0)
          if (lboth) jcut(i0) = 1
        endif
c
c..... starting with the min x vertex, go along until find the max x vertex
c..... at the v-level
c
        x1 = xb(1)
        k1 = 0
        k = -1
 200    k = k+1
        i = kolo(i0 + k,nppa)
        i1 = kolo(i+1,nppa)
        if (abs(ya(i) - v).lt.tol .and. xa(i).gt.x1) then
          x1 = xa(i)
          k1 = k
        endif
        if (xa(i1) .eq. flarc) k = k+3
        if (k .lt. nppa) goto 200
c
c..... if just a single point intersection at a 'lace+both' mode, move
c..... a little inside (as for a pure 'lace')        
c
        if (k1.eq.0 .and. dva.ne.0) then
          v = v + dva
          goto 10
        endif

        if (k1 .gt. 0) then
          do k = 1,k1
            i = kolo(i0 + k,nppa)

            if (lacefl .gt. 1 .or. ya(i)*pm .ge. v*pm) then

            nppb = nppb + 1
            xb(nppb) = xa(i)
            yb(nppb) = ya(i)

            if (lboth .and. k.lt.k1) then
              jcut(i) = 1
            endif

            endif

          enddo
        endif

        ifin = i
        call lssto (lan,mom,xb,yb,nppb,ierr)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lscutn (lan,mom,xa,ya,nppa,v,vn,klok,ifin,ista,ierr
c **  purpose of subroutine: compute the next level scrub pocketing cut
c **  for the current contour in the current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **      vn.............next y-level
c **      ccw............1 if CCW, -1 if CW
c **      ifin...........last point of previous motion
c **    output:
c **      ifin...........last point of this motion
c **      ista...........first point at current y-level
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lscutn (lan,mom,xa,ya,jcut,nppa,v,vn,klok,jdir,
     x                                               ifin,ista,ierr)

      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid
      common/yisl/yisl0(100),yisl1(100),iisl(100),isldir(100)
      real*4 yisl0,yisl1
      integer*2 iisl,isldir

      real*4 xa(maxpts),ya(maxpts)
      integer*2 jcut(maxpts)
      integer*2 lan,mom,nppa,klok,ifin,ista,ierr,jdir
      real*4 v,vn

      real*4 xb(maxpts),yb(maxpts),x1,x2,ccw,vinit,veps
      integer*2 i,nppb,npt,nppc,lup,i1,l1,i2,l2,jdum,itry,ipm
      real*4 xc(maxpts),yc(maxpts),xi,yi
      integer*2 jc(maxpts)

      integer*2 bshrt,bsame,bccw,bcw
      parameter (bshrt=0,bsame=1,bccw=2,bcw=3)

        lup = 0
        ix = 1
        ccw = 1.
        if (vn .lt. v) ccw = -1.
c        if (bddir .eq. bsame) jdir = -jdir
        jdum = 0

        veps = 0.1*tol
        vinit = v
        itry = 0
        ipm = 1

10      call llnul
c
c..... add perimeter intersections
c
        call llpts (lan,xa,ya,nppa,v,lup)
c
c..... add island intersections
c
        DO 32 ILE = 101, islnds+100
          i = ile - 100
          if (insid(i).eq.lan .and.
     *        yisl0(i).lt.(v-tol) .and. yisl1(i).gt.(v+tol)) then
            call lisget (i,xc,yc,jc,nppc)
            call llpts (ile,xc,yc,nppc,v,lup)
            if (bddir .eq. bsame) isldir(i) = -isldir(i)
          endif
  32    continue

        call llsort (nppb,klok)
        if (nppb.lt.2) return

        if (2*(nppb/2) .ne. nppb) then
          if (itry .lt. 20) then
            itry  = itry + 1
            ipm = -ipm
            i = (itry+1)/2
            v = vinit + ipm*i*veps
            goto 10
          else
            ierr = 51
            return
          endif
        else if (itry .gt .0) then
          v  = vinit
        endif

        call llpop (i1,l1,x1)
        npt = 1
        ista = i1

        if (lnosteps) then
          xb(npt) = x1
          yb(npt) = v
          npt = npt+1
          goto 100
        endif

        if (lacefl .eq. 1) then
          call lclift
          xb(npt) = x1
          yb(npt) = v
          npt = npt+1
        else
        call lcarnd (lan,mom,xa,ya,jcut,jdum,nppa,ifin,i1,xb,yb,npt,
     x                                                         ierr)

        if (rcorn .gt. 0) then
          i = lanes
          CALL GETLAN (i,xc,yc,nppc)
          if (xb(2) .lt. flarc) then
            xi = (xb(1) + xb(2))/2
            yi = (yb(1) + yb(2))/2
            nppc = nppc+1
            if (xlace .ne. 1.) then
              xc(nppc) = xi*xlace + yi*ylace
              yc(nppc) = xlace*yi - ylace*xi
            else
              xc(nppc) = xi
              yc(nppc) = yi
            endif
          endif
          call filcor(xc,yc,nppc,rcorn,0)
          if (xb(2) .lt. flarc) then
            if (xc(nppc-3) .lt. flarc) nppc = nppc-1
            xb(1) = xi
            yb(1) = yi
          endif
          lanes = lanes-1
c          call lcstra (xc,yc,nppc)
          CALL STOLAP (LUP,LAN,MOM,xc,yc,nppc,IERR)
        endif
        endif

c        xb(npt) = x1
c        yb(npt) = v
100     continue
        call llpop (i2,l2,x2)
        nppb = nppb-2
 210    if (nppb .le. 1) goto 220
          i1 = i2
          l1 = l2
          x1 = x2
          nppb = nppb-1
          call llpop (i2,l2,x2)
          if (l1.ne.l2) then
            ierr = 51
            return
          endif
          if (l1 .ge. 100) then
            call lisget (l1-100,xc,yc,jc,nppc)

            call lcarnd (lan,mom,xc,yc,jc,isldir(l1-100),nppc,i1,i2,
     x                                               xb,yb,npt,ierr)

            if (lboth) call lisjct (l1-100,jc)

          else
            call lcarnd (lan,mom,xa,ya,jcut,jdir,nppa,i1,i2,xb,yb,npt,
     x                                                           ierr)
          endif
          if (ierr .gt. 0) return

          nppb = nppb-1
          call llpop (i2,l2,x2)
        goto 210
 220    xb(npt) = x2
        yb(npt) = v

        ifin = i2

        if (lnosteps) goto 250

        if (rcorn .le. 0) then
        if (lane(lanes).ne.999 .and. npt.gt.1 .and. xb(2).lt.flarc) then
            do i = 2,npt
              xb(i-1) = xb(i)
              yb(i-1) = yb(i)
            enddo
            npt = npt-1
          endif
        endif

250     continue
        call lssto (lan,mom,xb,yb,npt,ierr)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine llace2a (lan,mom,xa,ya,nppa,v,vn,klok,ifin,ista,ierr
c **  purpose of subroutine: compute the next level scrub pocketing cut
c **  for the current contour in the current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **      vn.............next y-level
c **      ccw............1 if CCW, -1 if CW
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine llace2a (lan,mom,xa,ya,jcut,nppa,v,klok,jdir,
     x                                   ifin,ista,xl,yl,ierr)
      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid
      common/yisl/yisl0(100),yisl1(100),iisl(100),isldir(100)
      real*4 yisl0,yisl1
      integer*2 iisl,isldir

      real*4 xa(maxpts),ya(maxpts)
      integer*2 jcut(maxpts)
      integer*2 lan,mom,nppa,klok,ifin,ista,ierr,jdir
      real*4 v,xl,yl

      real*4 xb(maxpts),yb(maxpts),x1,x2
      integer*2 i,nppb,npt,nppc,lup,i1,l1,i2,l2,jdum
      real*4 xc(maxpts),yc(maxpts)
      integer*2 jc(maxpts)

        lup = 1
        jdum = 0
        call llnul

        nppb = 0
        k = 0
 100    k = k+1
        k1 = kolo(k+1,nppa)
c..... add vertices if next to a wall
        if (abs(ya(k) - v).lt.tol) call llpush (k,lan,xa(k))
        if (xa(k1) .eq. flarc) k = k+3
        if (k .lt. nppa) goto 100

        call llsort (nppb,klok)
        if (nppb .lt. 1) return

        call llpop (i1,l1,x1)
        npt = 1
        ista = i1

          call lclift
          xb(npt) = x1
          yb(npt) = v
          npt = npt+1


c        xb(npt) = x1
c        yb(npt) = v

        call llpop (i2,l2,x2)
        nppb = nppb-2
 210    if (nppb .le. 1) goto 220
          i1 = i2
          l1 = l2
          x1 = x2
          nppb = nppb-1
          call llpop (i2,l2,x2)
          if (l1.ne.l2) then
            ierr = 51
            return
          endif
          if (l1 .ge. 100) then
            call lisget (l1-100,xc,yc,jc,nppc)

            call lcarnd (lan,mom,xc,yc,jc,isldir(l1-100),nppc,i1,i2,
     x                                               xb,yb,npt,ierr)

            if (lboth) call lisjct (l1-100,jc)

          else
            call lcarnd (lan,mom,xa,ya,jcut,jdir,nppa,i1,i2,xb,yb,npt,
     x                                                           ierr)
          endif
          if (ierr .gt. 0) return

          nppb = nppb-1
          call llpop (i2,l2,x2)
        goto 210
 220    xb(npt) = x2
        yb(npt) = v

        ifin = i2
        xl = x2
        yl = v

        if (rcorn .le. 0) then
        if (lane(lanes).ne.999 .and. npt.gt.1 .and. xb(2).lt.flarc) then
            do i = 2,npt
              xb(i-1) = xb(i)
              yb(i-1) = yb(i)
            enddo
            npt = npt-1
          endif
        endif

        call lssto (lan,mom,xb,yb,npt,ierr)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine llace2 (lan,mom,xa,ya,nppa,v,vn,klok,ifin,ista,ierr
c **  purpose of subroutine: compute the next level scrub pocketing cut
c **  for the current contour in the current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **      vn.............next y-level
c **      ccw............1 if CCW, -1 if CW
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine llace2 (lan,mom,xa,ya,jcut,nppa,v,klok,jdir,
     x                                               ifin,ista,ierr)

      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid
      common/yisl/yisl0(100),yisl1(100),iisl(100),isldir(100)
      real*4 yisl0,yisl1
      integer*2 iisl,isldir

      real*4 xa(maxpts),ya(maxpts)
      integer*2 jcut(maxpts)
      integer*2 lan,mom,nppa,klok,ifin,ista,ierr,jdir
      real*4 v

      real*4 xb(maxpts),yb(maxpts),x1,x2
      integer*2 i,nppb,npt,nppc,lup,i1,l1,i2,l2,jdum
      real*4 xc(maxpts),yc(maxpts)
      integer*2 jc(maxpts)

        lup = 1
        jdum = 0
        call llnul

        nppb = 0
        k = 0
 100    k = k+1
        k1 = kolo(k+1,nppa)
c..... add vertices if next to a wall
        if (abs(ya(k) - v).lt.tol) call llpush (k,lan,xa(k))
        if (xa(k1) .eq. flarc) k = k+3
        if (k .lt. nppa) goto 100

        call llsort (nppb,klok)
        if (nppb .lt. 1) return

        call llpop (i1,l1,x1)
        npt = 1
        ista = i1

          call lclift
          xb(npt) = x1
          yb(npt) = v
          npt = npt+1


c        xb(npt) = x1
c        yb(npt) = v

        call llpop (i2,l2,x2)
        nppb = nppb-2
 210    if (nppb .le. 1) goto 220
          i1 = i2
          l1 = l2
          x1 = x2
          nppb = nppb-1
          call llpop (i2,l2,x2)
          if (l1.ne.l2) then
            ierr = 51
            return
          endif
          if (l1 .ge. 100) then
            call lisget (l1-100,xc,yc,jc,nppc)

            call lcarnd (lan,mom,xc,yc,jc,isldir(l1-100),nppc,i1,i2,
     x                                               xb,yb,npt,ierr)

            if (lboth) call lisjct (l1-100,jc)

          else
            call lcarnd (lan,mom,xa,ya,jcut,jdir,nppa,i1,i2,xb,yb,npt,
     x                                                           ierr)
          endif
          if (ierr .gt. 0) return

          nppb = nppb-1
          call llpop (i2,l2,x2)
        goto 210
 220    xb(npt) = x2
        yb(npt) = v

        ifin = i2

        if (rcorn .le. 0) then
        if (lane(lanes).ne.999 .and. npt.gt.1 .and. xb(2).lt.flarc) then
            do i = 2,npt
              xb(i-1) = xb(i)
              yb(i-1) = yb(i)
            enddo
            npt = npt-1
          endif
        endif

        call lssto (lan,mom,xb,yb,npt,ierr)
        call lclift

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lscut2 (lan,mom,xa,ya,nppa,v,klok,ifin,ista,ierr)
C **  purpose of subroutine: calculate last run for SCRUB POCKET contour
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      v..............current y-level
c **      ccw............1 if CCW, -1 if CW
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lscut2 (lan,mom,xa,ya,jcut,nppa,v,dva,klok,ifin,ista,
     x                                           xl,yl,jlast,ierr)

      include 'rrdm.com'
      include 'pokcom.com'

      real*4 xa(maxpts),ya(maxpts)
      integer*2 jcut(maxpts)
      integer*2 lan,mom,nppa,klok,ifin,ista,ierr,jlast
      real*4 v,dva,xl,yl

      integer*2 i,k,k1,nppb,npt,lup,kolo,i1,l1,jdum
      real*4 xb(maxpts),yb(maxpts)
      real*4 a,b,d,x1,tolsq

        lup = 1
        jdum = 0
        tolsq = tol*tol

   10   call llnul
        nppb = 0
        k = 0
 100    k = k+1
        k1 = kolo(k+1,nppa)
c..... add vertices if next to a wall
        if (abs(ya(k) - v).lt.tol) call llpush (k,lan,xa(k))
        if (xa(k1) .eq. flarc) k = k+3
        if (k .lt. nppa) goto 100

        call llsort (nppb,klok)
        if (nppb .lt. 1) return

        if (nppb.eq.1 .and. dva.ne.0) return

        npt = 0
        if (lacefl.gt.1 .and. rcorn .gt. 0) then
          i = lanes
          CALL GETLAN (i,xb,yb,npt)
          if (lctflg) then
            ylace = -ylace
            call lctran (xb,yb,npt,lup)
            ylace = -ylace
          endif
          lanes = lanes-1
          npt = npt-1
        endif

        if (jlast .eq. 0) then
          do k = 1,nppb
           call llpop (i1,l1,x1)
          enddo
        else
          call llpop (i1,l1,x1)
        endif

        if (lacefl .eq. 1) then
          call lclift
          ifin = i1

        else
        npt = npt+1
        call lcarnd (lan,mom,xa,ya,jcut,jdum,nppa,ifin,i1,xb,yb,npt,
     x                                                         ierr)

        npt = npt-1
        ifin = i1

c 400    if (rcorn .gt. 0) call filcor(xb,yb,npt,rcorn,0)
 400    if (rcorn .gt. 0) then
          if (npt .gt. 1) then
            if (npt .gt. 4 .and. xb(npt-3).eq.flarc) goto 410
            d = (xb(npt)-xa(ifin))**2 + (yb(npt)-ya(ifin))**2
            if (d .lt. tolsq) then
              ista = -1
              a = (xb(npt-1) + xb(npt))/2
              b = (yb(npt-1) + yb(npt))/2
              xb(npt) = a
              yb(npt) = b
              xl = a
              yl = b
            endif
          endif
410       call filcor(xb,yb,npt,rcorn,0)
          if (ista .eq. -1) npt = npt-1
        else if (lane(lanes).ne.999 .and. npt.gt.1
     x                              .and. xb(2).lt.flarc) then
          do i = 2,npt
            xb(i-1) = xb(i)
            yb(i-1) = yb(i)
          enddo
          npt = npt-1
        endif

        call lssto (lan,mom,xb,yb,npt,ierr)

        endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lsctfin (lan,mom,ifin,i1,xb,yb,nppb,x2,y2,ierr)
C **  purpose of subroutine: calculate and store the final pass around
C **  the perimeter for SCRUB POCKET contour
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      ifin...........ending point for the last lane
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lsctfin (lan,mom,ifin,i1,xb,yb,nppb,x2,y2,ierr)

      include 'pokcom.com'

      real*4 xb(maxpts),yb(maxpts)
      integer*2 lan,mom,nppb,ifin,i1,ierr
      real*4 x2,y2

      integer*2 icen,k,npt,kccw
      real*4 useg

      ICEN = 1
      USEG = 0

      kccw = 0
      if (lacefl .gt. 1) then
        kccw = -1
        if (lnosteps) kccw = 1
      endif

      nppb = nppb + 1
      xb(nppb) = xb(1)
      yb(nppb) = yb(1)

      if (rcorn .eq. 0) then
        if (lacefl .gt. 1) then
          k = ifin + 1
          if (xb(k) .ne. flarc) ifin = k 
        endif
        if (ifin .lt. nppb) then
          CALL SHUFFLE (xb,yb,nppb,ifin,ICEN,USEG,X2,Y2)
        endif
        npt = nppb
        if (lacefl .gt. 1 .and. .not.lnosteps) then
          k = nppb - 3
          if (k.lt.1 .or. xb(k).ne.flarc) npt = nppb-1
        endif
      else
        if (ifin .gt. nppb) ifin = 1
        if (i1 .eq. -1) then
          ifin = ifin - 1
          if (ifin .lt. 1) ifin = nppb - 1
          icen = 0
          CALL SHUFFLE (xb,yb,nppb,ifin,ICEN,USEG,X2,Y2)
          npt = nppb
        else
          CALL SHUFFLE (xb,yb,nppb,ifin,ICEN,USEG,X2,Y2)
          npt = nppb
          if (lacefl .gt. 1 .and. .not.lnosteps) then
            k = nppb - 3
            if (k.lt.1 .or. xb(k).ne.flarc) npt = nppb-1
          endif
        endif
        call filcor(xb,yb,npt,rcorn,kccw)
      endif

      call lssto (lan,mom,xb,yb,npt,ierr)

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lscut (lan,mom,xa,ya,nppa,nstp,ldown,ierr)
c **  purpose of subroutine: compute lace pocketing for one contour in the
c **  current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      step...........distance between cuts
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lscut (lan,mom,xa,ya,nppa,nstp,ldown,lflip,ierr)

      include 'com4a.com'
      include 'rrdm.com'
      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid

      real*4 xa(maxpts),ya(maxpts)
      integer*2 lan,mom,nppa,nstp,ierr
      logical ldown,lflip

      real*4 v,v0,v1,vn,dv,dva,tolsq
      real*4 xb(maxpts),yb(maxpts)
      integer*2 klok,n,i,j,lpm,ifin,i1,jdir,laneb,jlast,lanes0
      integer*2 jcuta(maxpts),jcutb(maxpts)
      logical lv95,lboth0

c
c..... insert all missing level intersections into the perimeter contour,
c..... so that an intersection is always a vertex
c
        lv95  = sc(169).lt.9.549d0
        tolsq = tol**2
        dva = 0
c
c..... for a lace mode with a final cut at the end add intersections at a small
c..... distance from top and bottom
c
        if (.not.lv95 .and. lacefl.eq.1 .and. stpcur.gt.0 ) then
          dva = stpcur/8.
          lnosteps = .false.
        endif

        if (lnosteps) then
          v = vmin
          call lcins2 (xa,ya,nppa,v,tolsq,ierr)
        else
          call lcinsr (dva,xa,ya,nppa,nstp,tolsq,ierr)
        endif
c
c..... create a copy of the perimeter contour with the opposite orientation
c
        xb(1) = xa(1)
        yb(1) = ya(1)
        I = 1
   10   I = I+1
        j = nppa + 2 - i
        IF (XA(I).EQ.FLARC) THEN
          xb(j) = xa(i+2)
          yb(j) = ya(i+2)
          xb(j-1) = xa(i+1)
          yb(j-1) = -ya(i+1)
          xb(j-2) = xa(i)
          yb(j-2) = ya(i)
          I = I+2
        else
          xb(j) = xa(i)
          yb(j) = ya(i)
        endif
        if (i .lt. nppa) goto 10

        if (lboth) then
          do j = 1,nppa
            jcuta(j) = 0
            jcutb(j) = 0
          enddo
        endif

        do i = 1, islnds
          if (ierr.eq.0 .and. insid(i).eq.lan)
     x        call lisins (i,nstp,ierr)
        enddo

        if (ldown) then
          klok = -1
          v0 = vmax
          v1 = vmin
          dva = -dva
          dv = -stpcur
        else
          klok = 1
          v0 = vmin
          v1 = vmax
          dv = stpcur
        endif
c
c..... klok is used to determine how to orient the perimeter: 1 - ccw, -1 - cw
c..... lpm is used for the cut direction: 1 - left to right, -1 - right to left
c
        v = v0
        lpm = 1
        jdir = 0
c
c..... cut the first level
c
        v = v0 + dv
        if (ierr. gt. 0) return

        if (lnosteps) then
c
c..... if the contour is narrow (nstp = 0): either do one cut,
c..... or just the final pass
c
          ifin = 1
          i1 = 1
          x2 = xa(1)
          y2 = ya(1)
          if (lcfin.eq.fpnon) then
            call lscutn (lan,mom,xa,ya,jcuta,nppa,v,v,lpm,jdir,
     x                   ifin,i1,ierr)
            jlast = 0
          else if (lcfin.eq.fprev .or. (klok.eq.1 .and. lcfin.eq.fpccw)
     x             .or. (klok.eq.-1 .and. lcfin.eq.fpclw)) then
            jlast = 1
          else
            jlast = -1
          endif
          goto 100
        endif

        if (klok .eq. 1) then
          call lscut1 (lan,mom,xa,ya,jcuta,nppa,v0,dva,v,ifin,ierr)
          if (lboth) call jcupdt (jcuta,jcutb,nppa)
        else
          call lscut1 (lan,mom,xb,yb,jcutb,nppa,v0,dva,v,ifin,ierr)
          if (lboth) call jcupdt (jcutb,jcuta,nppa)
        endif
        if (ierr.gt.0 .or. nstp.lt.1) return
        i1 = ifin

        do n = 1,nstp-1
          if (v .gt.vmax+tol .and. v.lt.vmin-tol) return
c
c..... do intermediate levels
c
          vn = v + dv
          if (lacefl .eq. 2) then
          klok = -klok
          lpm = -lpm
          if (ifin.gt.1) ifin = nppa+2-ifin
          endif

          if (klok .eq. 1) then
            call lscutn (lan,mom,xa,ya,jcuta,nppa,v,vn,lpm,jdir,
     x                   ifin,i1,ierr)

            if (lboth) call jcupdt1 (ldown,v,xa,ya,jcuta,jcutb,nppa)

          else
            call lscutn (lan,mom,xb,yb,jcutb,nppa,v,vn,lpm,jdir,
     x                   ifin,i1,ierr)

            if (lboth) call jcupdt1 (ldown,v,xb,yb,jcutb,jcuta,nppa)

          endif
          v = vn
          if (ierr .gt. 0) return
        enddo
c
c..... do last level
c
        if (lacefl .eq. 1) then
c
c..... last level at lace mode
c
          jlast = 1
          if (lcfin .eq. fpnon) then
            jlast = 0
          else if (lcfin.eq.fprev .or. (klok.eq.1 .and. lcfin.eq.fpccw)
     x           .or. (klok.eq.-1 .and. lcfin.eq.fpclw)) then
            jlast = -1
          endif

          if (jlast .eq. 0) then
            X2 = 0.0
            Y2 = 0.0
            jlast = -1
            v = v1 - dva
            if (klok .eq. 1) then
              call llace2 (lan,mom,xa,ya,jcuta,nppa,v,lpm,jdir,
     x                   ifin,i1,ierr)
            else
              call llace2 (lan,mom,xb,yb,jcutb,nppa,v,lpm,jdir,
     x                   ifin,i1,ierr)
            endif
            jlast = 0
          else
            lboth0 = lboth
            lanes0 = lanes

            lboth = .false.
            X2 = 0.0
            Y2 = 0.0

            if (klok .eq. 1) then
              call lscut2 (lan,mom,xa,ya,jcuta,nppa,v,dva,lpm,ifin,i1,
     x                                                 x2,y2,jlast,ierr)

              if (lanes .eq. lanes0) then
                v = v1 - dva
                lboth = lboth0
                call llace2a (lan,mom,xa,ya,jcuta,nppa,v,lpm,jdir,
     x                   ifin,i1,x2,y2,ierr)
                lboth = .false.
              endif

            else
              call lscut2 (lan,mom,xb,yb,jcutb,nppa,v,dva,lpm,ifin,i1,
     x                                                 x2,y2,jlast,ierr)

              if (lanes .eq. lanes0) then
                v = v1 - dva
                lboth = lboth0
                call llace2a (lan,mom,xb,yb,jcutb,nppa,v,lpm,jdir,
     x                   ifin,i1,x2,y2,ierr)
                lboth = .false.
              endif

            endif
          endif

        else
c
c..... last level at scrub mode
c
          klok = -klok
          lpm = -lpm
          if (ifin.gt.1) ifin = nppa+2-ifin
          if (i1.gt.1) i1 = nppa+2-i1
          X2 = 0.0
          Y2 = 0.0
          lboth = .false.

          jlast = 1
          if (lcfin .eq. fpnon) then
            jlast = 0
          else if (lcfin.eq.fprev .or. (klok.eq.1 .and. lcfin.eq.fpccw)
     x             .or. (klok.eq.-1 .and. lcfin.eq.fpclw)) then
            jlast = -1
          endif

          if (klok .eq. 1) then
            call lscut2 (lan,mom,xa,ya,jcuta,nppa,v,dva,lpm,ifin,i1,
     x                                             x2,y2,jlast,ierr)
          else
            call lscut2 (lan,mom,xb,yb,jcutb,nppa,v,dva,lpm,ifin,i1,
     x                                             x2,y2,jlast,ierr)
          endif
        endif

100     continue
c
c..... final pass
c
        if (jlast .ne. 0) then
          laneb = lanes + 1

          if (jlast .eq. 1) then
            if (ifin.gt.1) ifin = nppa+2-ifin
          endif

          if (klok*jlast .eq. -1) then
            call lsctfin (lan,mom,ifin,i1,xa,ya,nppa,x2,y2,ierr)
          else
            lflip = .true.
            call lsctfin (lan,mom,ifin,i1,xb,yb,nppa,x2,y2,ierr)
          endif
c
c..... activate last pass feedrate for final passes
c
          do i = laneb,lanes
            loop(i) = 1
          enddo

          if (lacefl .eq. 1) then
            call lclift
          endif

        endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lacer1 (lan,mom,xa,ya,nppa,step,ierr)
c **  purpose of subroutine: compute lace pocketing for one contour in the
c **  current horizontal direction.
c **    input:
c **      lan............current contour lane number
c **      mom............current contour index
c **      xa,ya,nppa.....contour points and the number of points
c **      step...........distance between cuts
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lacer1 (lan,mom,xa,ya,nppa,step,lflip,ierr)

      include 'com4a.com'
      include 'pokcom.com'

      integer*2 lan,mom,nppa,ierr
      logical lflip
      real*4 xa(maxpts),ya(maxpts)
      real*4 step

      integer*2 nstp
      logical ldown,lv96

        lv96  = sc(169).lt.9.649d0
c
c..... calculate min and max level for this contour
c
        call lcmmx0(xa,ya,nppa,vmin,vmax)
        ldown = (step .lt. 0)
c
c..... calculate number of steps, so that they are evenly distributed;
c..... stpcur is the factual step.
c
        lnosteps = .false.
        stpcur = abs(step)
        nstp = (vmax - vmin)/stpcur
        if (nstp*stpcur .lt. vmax-vmin+tol) nstp = nstp + 1
        stpcur = (vmax - vmin)/nstp
        if (stpcur .lt. stpmin) then
          if (nstp .lt. 2) then
            if (.not.lv96) lnosteps = .true.
            nstp = 0
            vmin = (vmin+vmax)/2
            vmax = vmin
            stpcur = 0
            ldown = .false.
          else if (stpcur .lt. stpmin - tol) then
            ierr = 16
            return
          endif
        endif

        nppa = nppa-1

        if (lacefl .eq. 1 .and. .not.lboth) then
          call llcut (lan,mom,xa,ya,nppa,nstp,ldown,lflip,ierr)
        else
          call lscut (lan,mom,xa,ya,nppa,nstp,ldown,lflip,ierr)
        endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine lacer (step,lanex,step)
c **  purpose of subroutine: compute lace pocketing motions in the
c **  current horizontal direction.
c **    input:
c **      step...........distance between cuts
c **      LANEX..........MAX LANE NUMBER
c **    output:
c **      IERR...........0 - OK, OTHER - ERROR
c **********************************************************************
c **********************************************************************
      subroutine lacer (step,lanex,ierr)

      include 'rrdm.com'
      include 'pokcom.com'

      common/insi/insid(100)
      integer*2 insid
      common/yisl/yisl0(100),yisl1(100),iisl(100),isldir(100)
      real*4 yisl0,yisl1
      integer*2 iisl,isldir

      integer*2 ierr,lanex
      real*4 step

      integer*2 lan,lupok,index,ile,i,il,nppa,nppb,in,lup,ile1
      logical lflip
      real*4 xa(maxpts),ya(maxpts),xb(maxpts),yb(maxpts),xpt,ypt

      if (vmax .lt. vmin) step = -step
      lupok = 1
      ylace = -ylace

      LOCAPO = LANES+1
      call llini
      if (islnds .gt. 0) call lisini(islnds)

      do 100 lan = 1, lanex
        call getlup (lan,lupok,index,xa,ya,nppa,ierr)
        if (ierr .gt. 0) goto 999
        IF (index. eq. 0) goto 100
c
c..... for every contour find which island(s) is inside, calculate
c..... the min and max for each such island, calculate lace cuts
c
        DO 10 ILE = 101, islnds+100
          i = ile - 100
          if (insid(i) .gt. 0) goto 10
          CALL getlpt (ILE,lupok,IL,xpt,ypt,ierr)
          if (ierr.gt.1) goto 999
          IF (IL.EQ.0) GOTO 10
          call inloop (xpt, ypt, xa, ya, nppa, in, ierr)
          if (ierr.eq.1) goto 999
          if (in.eq.1) then
            insid(i) = lan
            iisl(i) = IL
            CALL GETLAN (IL,xb,yb,nppb)
            call lcmmx0(xb,yb,nppb,yisl0(i),yisl1(i))
          endif
  10    continue

        lflip = .false.
        call lacer1 (lan,index,xa,ya,nppa,step,lflip,ierr)
        if (ierr .gt. 0) goto 999

        if (lcfin .eq. fpnon) goto 30

        lup = 1
        DO 20 ILE = 101, islnds+100
          if (ierr .gt. 0) return
          ile1 = ile
          i = ile - 100
          if (insid(i).eq.lan) then
            il = iisl(i)
            CALL getlan (il,xb,yb,nppb)
            if (lflip) then
              call flipo (xb,yb,nppb)
c..... signal to pokntr that this island is CCW
              ile1 = ile + 100
            endif
            if (lctflg) call lctran (xb,yb,nppb,lup)
            if (lacefl .eq. 1) then
              call stolap(lup,ile1,il,xb,yb,nppb,ierr)
              call lclift
            else
              call lclift
              if (rcorn .gt. 0) call filcor(xb,yb,nppb,rcorn,1)
              call stolap(lup,ile1,il,xb,yb,nppb,ierr)
            endif
          endif
  20    continue

c
c..... if the option is SCRUB, i.e., 'down', we retract only at the end
c
  30    if (lacefl .eq. 2) call lclift
        if (islnds .gt. 0) call lisrst
  100 continue

  999 call lldel
      if (islnds .gt. 0) call lisdel
      return
      end
