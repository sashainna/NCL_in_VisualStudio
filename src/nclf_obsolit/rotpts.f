C*********************************************************************
C*    NAME         :  rotpts.f
C*       CONTAINS:
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        rotpts.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:38
C*********************************************************************

C*********************************************************************
C*
C*       Subroutine rotpts
C*
C*       This routine is a modification of the PREPST routine ROTPTS
C*
C*********************************************************************

      subroutine rotpts (wrkpts,ierr)
      include 'com8a.com'
      include 'fillet.com'
 
      real*8 wrkpts(6,4),pbuf(12),secsq,co,sectol
      real*8 ti,tj,clp(3),vec,tlvc(3),RADS /57.29577 95d0/,ptv(3)
      integer*2 ierr
C
C...IROTPT = 1 - Rotate points to the XYPLANE
C...IROTPT = 0 - Rotate points back to original plane
C...ORGA - Original ALPHA angle
C...ORGB - Original BETA angle
C...NOPTS - Number of points to transform
C
      sectol = 1.d-12
C
C...Set error flag
C
      ierr  = 0
      if (irotpt .eq. 0) goto 3000
C
C...Get tool vector for corner point
C
      if (MULTAX .eq. 1) then
          tlvc(1) = wrkpts(4,2)
          tlvc(2) = wrkpts(5,2)
          tlvc(3) = wrkpts(6,2)
      else
          tlvc(1) = 0.
          tlvc(2) = 0.
          tlvc(3) = 1.
      end if
C
C...Load points into pbuf
C
      pbuf(4) = wrkpts(1,1)
      pbuf(5) = wrkpts(2,1)
      pbuf(6) = wrkpts(3,1)
      pbuf(7) = wrkpts(1,2)
      pbuf(8) = wrkpts(2,2)
      pbuf(9) = wrkpts(3,2)
C
C...The elements in wrkpts(x,3) are the same as the elements
C...in wrkpts(x,2), it is the shared point, so skip it and 
C...put the end elements (wrkpts(x,4)) into pbuf.
C
      pbuf(10) = wrkpts(1,4)
      pbuf(11) = wrkpts(2,4)
      pbuf(12) = wrkpts(3,4)
C
C...Rotate points to the XYPLAN
C
      DO 200 I=4,6
          pbuf(I+3)=pbuf(I+3)-pbuf(I)
          pbuf(I+6)=pbuf(I+6)-pbuf(I)
  200 CONTINUE
C
C...Unitize vectors to improve reliabilty with small distances
C...and shallow angles.
C
      call unitvc(pbuf(7),pbuf(7))
      call unitvc(pbuf(10),pbuf(10))
C
C...Finish the equation
C
      pbuf(1)=pbuf(8)*pbuf(12)-pbuf(9)*pbuf(11)
      pbuf(2)=pbuf(9)*pbuf(10)-pbuf(7)*pbuf(12)
      pbuf(3)=pbuf(7)*pbuf(11)-pbuf(8)*pbuf(10)
      pbuf(4)=pbuf(1)*pbuf(4)+pbuf(2)*pbuf(5)+pbuf(3)*pbuf(6)
 
      secsq=pbuf(1)**2+pbuf(2)**2+pbuf(3)**2
      if (secsq.gt.sectol) goto 300
C
C...If the plane vector length is small, the 2 line segments are colinear.
C...Create a plane containing the first line segment and normal to the plane
C...containing the first line segemnt and the tool axis. 
C
      call f_cross(wrkpts(4,1),pbuf(7),pbuf(10))
      call f_cross(pbuf(7),pbuf(10),pbuf)
      pbuf(4)=pbuf(1)*pbuf(4)+pbuf(2)*pbuf(5)+pbuf(3)*pbuf(6)
      secsq=pbuf(1)**2+pbuf(2)**2+pbuf(3)**2
      if (secsq.gt.sectol) goto 300
      ierr= 1
      goto 9000
  300 co=1.D0/DSQRT(secsq)
      do 400 i=1,4
          pbuf(i)=pbuf(i)*co
  400 continue
C
C...Get angle between tlvc and plane
C
      co    = tlvc(1)*pbuf(1) + tlvc(2)*pbuf(2) + tlvc(3)*pbuf(3)
      if (co .gt. 1.) co = 1.
      if (co .lt. -1.) co = -1.
      co    = dacos (co) * RADS
      call dpont2 (co,4)
C
C...Flip the plane if direction are opposite
C
      if (co .lt. 90.) goto 500
      pbuf(1) = -pbuf(1)
      pbuf(2) = -pbuf(2)
      pbuf(3) = -pbuf(3)
C
C...Output rotation axis vector to see if valid circular record
C...can be output
C
  500 cirbuf(4)  =  pbuf(1)
      cirbuf(5)  =  pbuf(2)
      cirbuf(6)  =  pbuf(3)
C
C...Rotate XY using XY comps only
C
      vec   = DSQRT (pbuf(1)**2 + pbuf(2)**2)
      IF (vec .ne. 0.0) goto 550
      tvx(1) = 1.0
      tvx(2) = .0
      GO TO 225
  550 tvx(1) = (pbuf(1) / vec)
      tvx(2) = (pbuf(2) / vec)
C
C...Rotate XY in shortest way
C
  225 If (tvx(1) .LT. 0.) then
          tvx(1) = -tvx(1)
          tvx(2) = -tvx(2)
      end if
C
C...Adjust tool axis for yz rot
C
      ti     = tvx(2)
      tj     = tvx(1)
 
      vec    = pbuf(1)
      tvz(1) = (pbuf(2)*ti) + (vec*tj)
      tvz(2) = (pbuf(2)*tj) - (vec*ti)
      tvz(3) = pbuf(3)
      vec    = DSQRT (tvz(1)**2 + tvz(2)**2 + tvz(3)**2)
      tvz(1) = (tvz(1) / vec)
      tvz(2) = (tvz(2) / vec)
      tvz(3) = (tvz(3) / vec)
C
C...Rotate points around to 0,0,1 ptv
C
      DO 1000 I=1,4,1
          call rotpt (wrkpts(1,i),wrkpts(1,i))
 1000 CONTINUE
C
C...DONT ALLOW PRPND FEED RATE ADJUST
C
      IF (CO .EQ. 90. .AND. IFEDCO .NE. 0) FEDOUT = -1.
      RETURN
C
C... ROTATE POINTS BACK TO ORIGINAL LOCATIONS
C
 3000 CLP(1) = WRKPTS(1,1)*TVZ(3) + WRKPTS(3,1)*TVZ(1)
      CLP(2) = WRKPTS(2,1)
      CLP(3) = WRKPTS(3,1)*TVZ(3) - WRKPTS(1,1)*TVZ(1)
      WRKPTS(1,1) = CLP(1)*TVX(1) - CLP(2)*TVX(2)
      WRKPTS(2,1) = CLP(2)*TVX(1) + CLP(1)*TVX(2)
      WRKPTS(3,1) = CLP(3)
C
      if (IFILTA .eq. 1) then
          wrkpts(4,1) = FILTAX(1)
          wrkpts(5,1) = FILTAX(2)
          wrkpts(6,1) = FILTAX(3)
      else IF (MULTAX .EQ. 1) THEN
          PTV(1) = WRKPTS(4,1)*TVZ(3) + WRKPTS(6,1)*TVZ(1)
          PTV(2) = WRKPTS(5,1)
          PTV(3) = WRKPTS(6,1)*TVZ(3) - WRKPTS(4,1)*TVZ(1)
          WRKPTS(4,1) = PTV(1)*TVX(1) - PTV(2)*TVX(2)
          WRKPTS(5,1) = PTV(2)*TVX(1) + PTV(1)*TVX(2)
          WRKPTS(6,1) = PTV(3)
      END IF
C
 9000 RETURN
      END

C*********************************************************************
C*
C*       Subroutine rotpt
C*
C*       This routine is a modification of the PREPST routine ROTPTS
C*
C*********************************************************************

      subroutine rotpt (wrkpt,wrkpto)
c
      include 'com8a.com'
      include 'fillet.com'
c
      real*8 wrkpt(6),wrkpto(6)
c
      real*8 clp(3),ptv(3),opt(3)
c
      CLP(1) = WRKPT(1)
      CLP(2) = WRKPT(2)
      CLP(3) = WRKPT(3)
      PTV(1) = WRKPT(4)
      PTV(2) = WRKPT(5)
      PTV(3) = WRKPT(6)
C
      OPT(1) = CLP(1)*TVX(1) + CLP(2)*TVX(2)
      OPT(2) = CLP(2)*TVX(1) - CLP(1)*TVX(2)
      OPT(3) = CLP(3)
      WRKPTO(1) = OPT(1)*TVZ(3) - OPT(3)*TVZ(1)
      WRKPTO(2) = OPT(2)
      WRKPTO(3) = OPT(3)*TVZ(3) + OPT(1)*TVZ(1)
C
      IF (MULTAX .EQ. 1) THEN
          PTV(1)  = WRKPT(4)*TVX(1) + WRKPT(5)*TVX(2)
          PTV(2)  = WRKPT(5)*TVX(1) - WRKPT(4)*TVX(2)
          WRKPTO(4) = PTV(1)*TVZ(3) - PTV(3)*TVZ(1)
          WRKPTO(5) = PTV(2)
          WRKPTO(6) = PTV(3)*TVZ(3) + PTV(1)*TVZ(1)
      END IF
c
      return
      end 
c
C*********************************************************************
C*
C*       Subroutine rtprst
C*
C*       This routine resets the rotation matrix used by Filleting.
C*
C*********************************************************************
      subroutine rtprst
c
      include 'fillet.com'
c
      tvx(1) = 1.
      tvx(2) = 0.
      tvx(3) = 0.
c
      tvz(1) = 0.
      tvz(2) = 0.
      tvz(3) = 1.
c
      return
      end

