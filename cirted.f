c
c***********************************************************************
c
c   FILE NAME:  cirted
c   CONTAINS:
c               cirted  cir3ax  circen  cirdra  cirdr3  cirpla  cirota
c               cirgna  circnv
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c       cirted.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c       09/11/13 , 12:59:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cirted ()
c
c   FUNCTION:  This routine calculates circular records.
c
c   INPUT:    none
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine cirted
c
      include 'post.inc'
      include 'pted.inc'
c
      equivalence (ICIPRM,KPOSMP(1230))
c
      integer*4 ICIPRM(8)
c
      equivalence (RCIPRM,POSMAP(2049))
c
      real*8 RCIPRM(25)
c
      integer*4 isect
c
      real*8 plgeo(15),rmat(12)
c
c...Calculate circle record
c
      call cirdra (ICIPRM,RCIPRM)
c
c...Calculate circular command geometry
c
      if (PCNV_TYPE .ne. PCNV_CONVERT) then
          call cirpla (ICIPRM,RCIPRM,plgeo,isect)
c
c...Output circular command
c
          call cirota (ICIPRM,RCIPRM,plgeo,isect,rmat,0)
c
c...Tape conversion in effect
c...Convert circular record
c
      else
          call circnv (ICIPRM,RCIPRM)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cir3ax ()
c
c   FUNCTION:  This routine calculates 3-axis circular records.
c
c   INPUT:    none
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine cir3ax
c
      include 'post.inc'
c
      equivalence (ICIPRM,KPOSMP(1230)), (ICIPR2,KPOSMP(1266))
c
      integer*4 ICIPRM(8), ICIPR2(8)
c
      equivalence (RCIPRM,POSMAP(2049))
c
      real*8 RCIPRM(25)
c
      integer*4 isect
c
      real*8 plgeo(15),rmat(12)
c
c...Calculate circular parameters
c
      call cirdr3 (ICIPRM,RCIPRM,rmat)
      if (ICIPR2(1) .eq. 1) go to 8000
c
c...Calculate circular command geometry
c
      call cirpla (ICIPRM,RCIPRM,plgeo,isect)
c
c...Output circular command
c
      call cirota (ICIPRM,RCIPRM,plgeo,isect,rmat,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  circen
c
c   FUNCTION:  This routine stores the circular center point fore use
c              with the actual circular record when conversational
c              circular interpolation is supported.
c
c   INPUT:    none
c
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine circen
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'
c
      equivalence (REGSW ,KPOSMP(0405))
      equivalence (ICIPRM,KPOSMP(1230)), (MCHPLN,KPOSMP(1261))
      equivalence (CIRCOD,KPOSMP(1378)), (CIRCON,KPOSMP(4242))
c
      integer*4 MCHPLN,CIRCOD(6),REGSW(MAXFMT),CIRCON(8),ICIPRM(8)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (REGSTO,POSMAP(1032)), (MCHNUM,POSMAP(1287))
      equivalence (TLVEC ,POSMAP(1369)), (STONUM,POSMAP(1387))
      equivalence (RCIPRM,POSMAP(2049)), (CIRCOV,POSMAP(2215))
c
      real*8 REGSTO(MAXFMT),STONUM(3,4),MCHNUM(3,4),RCIPRM(25),
     1       CIRCOV(8),RAD,TLVEC(3)
c
      integer*4 inx(4),ix1,ix2,ix3
c
      real*8 mnum(3),stnum(3),v1(3),d1
c
      data inx /2,3,1,2/
c
c...Determine machining plane
c
      if (REGSW(CIRCOD(4)) .eq. 1 .and. REGSW(CIRCOD(5)) .eq. 1) then
          MCHPLN = 1
      else if (REGSW(CIRCOD(3)) .eq. 1 .and. REGSW(CIRCOD(5)) .eq. 1)
     1             then
          MCHPLN = 2
      else
          MCHPLN = 3
      endif
      ix1    = inx(MCHPLN)
      ix2    = inx(MCHPLN+1)
      ix3    = MCHPLN
      ICIPRM(1) = ix1
      ICIPRM(2) = ix2
      ICIPRM(3) = ix3
c
c...Work with transformed positions
c
      call ptxfm (STONUM(1,3),stnum,1)
      call ptxfm (MCHNUM(1,3),mnum,1)
c
c...Store circle center
c
      RCIPRM(ix1) = REGSTO(CIRCOD(ix1+2))
      RCIPRM(ix2) = REGSTO(CIRCOD(ix2+2))
      RCIPRM(ix3) = stnum(ix3)
c
c...Store point coming from
c
      RCIPRM(11) = stnum(ix1)
      RCIPRM(12) = stnum(ix2)
      RCIPRM(13) = stnum(ix3)
c
c...Calculate Radius
c
      v1(1) = stnum(ix1) - RCIPRM(ix1)
      v1(2) = stnum(ix2) - RCIPRM(ix2)
      RCIPRM(4) = dsqrt(v1(1)**2 + v1(2)**2)
c
c...Beginning angle
c
      d1     = v1(1) / RCIPRM(4)
      if (d1 .gt. 1.0d0) d1 = 1.0d0
      if (d1 .lt. -1.0d0) d1 = -1.0d0
      RCIPRM(5) = dacos(d1) * RAD
      if (v1(2) .lt. 0.) RCIPRM(5) = 360. - RCIPRM(5)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirdra (kprm,gprm)
c
c   FUNCTION:  This routine calculates the circle parameters, direction,
c              starting angle, ending angle, and delta angle of a
c              circular record.
c
c   INPUT:    kprm(5) I*4  D1   -  Circular direction, 1 = CLW, 2 = CCLW.
c
c
c   OUTPUT:   kprm    I*4  D8   -  Integer circular record buffer.
c                                  (1:3) = XYZ pointer depending on
c                                          plane selection.
c                                  (4) = Starting quadrant (not used).
c                                  (5) = Direction 1 - CLW, 2 = CCLW.
c
c             gprm    R*8  D12  -  Real circular record buffer.
c                                  (1:3) = Center point.
c                                  (4) = Radius.
c                                  (5) = Beginning angle.
c                                  (6) = Ending angle.
c                                  (7) = Unsigned delta angle,
c                                  (8:10) = Ending point.
c                                  (11,12) = Start point (X,Y).
c
c***********************************************************************
c
      subroutine cirdra (kprm,gprm)
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'
c
      integer*4 kprm(8)
c
      real*8 gprm(25)
c
      equivalence (REGSW ,KPOSMP(0405))
      equivalence (INCR  ,KPOSMP(1226)), (MCHPLN,KPOSMP(1261))
      equivalence (ICRFMT,KPOSMP(1368)), (ICRSGN,KPOSMP(1369))
      equivalence (CIRCOD,KPOSMP(1378))
      equivalence (ICRIJP,KPOSMP(1384)), (IHELIX,KPOSMP(1395))
      equivalence (PLNCOD,KPOSMP(4222))
      equivalence (CIRCON,KPOSMP(4242)), (ICRHEL,KPOSMP(4250))
c
      integer*4 INCR,ICRFMT,MCHPLN,CIRCOD(6),ICRSGN,ICRHEL(10),IHELIX,
     1          REGSW(MAXFMT),ICRIJP,CIRCON(8),PLNCOD(4)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (REGSTO,POSMAP(1032)), (MCHNUM,POSMAP(1287))
      equivalence (LINAXS,POSMAP(1299)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (STONUM,POSMAP(1387))
      equivalence (HELANG,POSMAP(2213)), (CIRCOV,POSMAP(2215))
      equivalence (ROTANG,POSMAP(5173))
c
      REAL*8 REGSTO(MAXFMT),STONUM(3,4),MCHNUM(3,4),TLVEC(3),RAD,
     1       HELANG(2),CIRCOV(8),LINAXS(6),AXSOUT(10),ROTANG(20,2)
c
      integer*4 inx(4)
c
      real*8 d1,d2,ra,v1(2),v2(2),x1,y1,ijk(3),rd,mnum(3),stnum(3)
c
      data inx /2,3,1,2/
c
c...Machining plane codes undefined
c...Calculate Machining plane
c
      if (PLNCOD(1) .eq. 0 .and. PLNCOD(2) .eq. 0 .and.
     1    PLNCOD(3) .eq. 0) then
          if (REGSW(CIRCOD(3)) .eq. 0) then
              MCHPLN = 1
          else if (REGSW(CIRCOD(4)) .eq. 0) then
              MCHPLN = 2
          else
              MCHPLN = 3
          endif
      endif
c
c...Machining plane indices
c
      kprm(1) = inx(MCHPLN)
      kprm(2) = inx(MCHPLN+1)
      kprm(3) = MCHPLN
c
c...Work with transformed positions
c
      call ptxfm (MCHNUM(1,3),mnum,1)
      if (ICRFMT .ne. 10) then
          call ptxfm (STONUM(1,3),stnum,1)
c
c...Set up known conversational control circle parameters
c
      else
          kprm(5) = 2
          if (REGSTO(CIRCON(3)) .lt. 0) kprm(5) = 1
          gprm(7) = dabs(REGSTO(CIRCON(3)))
          gprm(6) = gprm(5) + REGSTO(CIRCON(3))
          if (gprm(6) .lt. 0.) gprm(6) = gprm(6) + 360.
          if (gprm(6) .gt. 360.) gprm(6) = gprm(6) - 360.
          ijk(1) = dcos(gprm(6)/RAD)
          ijk(2) = dsin(gprm(6)/RAD)
          gprm(kprm(1)+7) = gprm(kprm(1)) + gprm(4)*ijk(1)
          gprm(kprm(2)+7) = gprm(kprm(2)) + gprm(4)*ijk(2)
          gprm(kprm(3)+7) = mnum(kprm(3))
          mnum(1) = gprm(8)
          mnum(2) = gprm(9)
          mnum(3) = gprm(10)
          stnum(kprm(1)) = gprm(11)
          stnum(kprm(2)) = gprm(12)
          stnum(kprm(3)) = gprm(13)
c
c...Save current position
c
          call ptxfr (mnum,MCHNUM(1,3),1)
          call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,2,1)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,3,5)
      endif
c
c...Initialize routine
c
      if (ICRIJP .eq. 1) then
          ijk(1) = REGSTO(CIRCOD(3))
          ijk(2) = REGSTO(CIRCOD(4))
          ijk(3) = REGSTO(CIRCOD(5))
      else
          ijk(1) = REGSTO(CIRCOD(kprm(1)+2))
          ijk(2) = REGSTO(CIRCOD(kprm(2)+2))
          ijk(3) = REGSTO(CIRCOD(kprm(3)+2))
      endif
      rd     = REGSTO(CIRCOD(6))
c
c...Determine if helical interpolation
c
      IHELIX = 0
      if (ICRHEL(1) .eq. 1) then
          if (ICRHEL(2) .ne. 0 .and.
     1            dabs(mnum(kprm(3))-stnum(kprm(3))) .gt. .0001) then
              IHELIX = 1
          else if (ICRHEL(3) .ne. 0) then
              if (REGSW(CIRCOD(kprm(3)+2)) .eq. 1 .and. ijk(3) .ne. 0)
     1                IHELIX = 1
          endif
      endif
c
c...Circle center
c......Absolute center
c
      if (ICRFMT .eq. 1 .or.
     1    (INCR .eq. 1 .and. (ICRFMT .eq. 4 .or. ICRFMT .eq. 5)) .or.
     2    ICRFMT .eq. 9) then
          gprm(kprm(1)) = ijk(1)
          gprm(kprm(2)) = ijk(2)
          gprm(kprm(3)) = stnum(kprm(3))
c
c......Incremental center
c
      else if (ICRFMT .eq. 2 .or.
     1         (ICRFMT .eq. 4 .and. INCR .eq. 2)) then
          d1 = 1.0d0
          if (ICRSGN .eq. 2) d1 = -1.0d0
          gprm(kprm(1)) = stnum(kprm(1)) + ijk(1)*d1
          gprm(kprm(2)) = stnum(kprm(2)) + ijk(2)*d1
          gprm(kprm(3)) = stnum(kprm(3))
c
c......Unsigned incremental center
c......or Radius
c
      else if (ICRFMT .eq. 3 .or. ICRFMT .eq. 6 .or.
     1         (INCR .eq. 2 .and. ICRFMT .eq. 5)) then
          if (ICRFMT .eq. 6) then
              ra = rd
          else
              ra = dsqrt(ijk(1)**2+ijk(2)**2)
          endif
c
c.........v1 is vector from start point of circular record to mid point
c.........on chord between start point and end point. xyzc is
c.........center point of chord (which is center point of circle
c.........for semi-circle).
c
          v1(1) = (mnum(kprm(1)) - stnum(kprm(1))) / 2.d0
          v1(2) = (mnum(kprm(2)) - stnum(kprm(2))) / 2.d0
          x1    = stnum(kprm(1)) + v1(1)
          y1    = stnum(kprm(2)) + v1(2)
          d1    = dsqrt(v1(1)**2+v1(2)**2)
          v2(1) = v1(2) / d1
          v2(2) = v1(1) / d1
          if (kprm(5) .eq. 1) v2(2) = -v2(2)
          if (kprm(5) .eq. 2) v2(1) = -v2(1)
          d2 = ra**2 - d1**2
          if (d2 .le. 0.) then
              d2 = 0.
          else
              d2 = dsqrt(d2)
          endif
          if (ra .lt. 0.) then
              v2(1) = -v2(1)
              v2(2) = -v2(2)
          endif
          gprm(kprm(1)) = x1 + d2*v2(1)
          gprm(kprm(2)) = y1 + d2*v2(2)
          gprm(kprm(3)) = STONUM(kprm(3),3)
      endif
c
c...Calculate Radius
c
      if (ICRFMT .ne. 10) then
          v1(1) = stnum(kprm(1)) - gprm(kprm(1))
          v1(2) = stnum(kprm(2)) - gprm(kprm(2))
          gprm(4) = dsqrt(v1(1)**2 + v1(2)**2)
c
c...Ending point
c
          gprm(kprm(1)+7) = mnum(kprm(1))
          gprm(kprm(2)+7) = mnum(kprm(2))
          gprm(kprm(3)+7) = mnum(kprm(3))
c
c...Starting point
c
          gprm(11) = stnum(kprm(1))
          gprm(12) = stnum(kprm(2))
c
c...Beginning angle
c
          d1     = v1(1) / gprm(4)
          if (d1 .gt. 1.0d0) d1 = 1.0d0
          if (d1 .lt. -1.0d0) d1 = -1.0d0
          gprm(5) = dacos(d1) * RAD
          if (v1(2) .lt. 0.) gprm(5) = 360. - gprm(5)
c
c...Ending angle
c
          v2(1) = mnum(kprm(1)) - gprm(kprm(1))
          v2(2) = mnum(kprm(2)) - gprm(kprm(2))
          d2    = v2(1) / gprm(4)
          if (d2 .gt. 1.0d0) d2 = 1.0d0
          if (d2 .lt. -1.0d0) d2 = -1.0d0
          gprm(6) = dacos(d2) * RAD
          if (v2(2) .lt. 0.) gprm(6) = 360. - gprm(6)
c
c...Delta angle
c
          if (kprm(5) .eq. 1) then
              gprm(7) = gprm(5) - gprm(6)
          else
              gprm(7) = gprm(6) - gprm(5)
          endif
          if (gprm(7) .le. 0.) gprm(7) = gprm(7) + 360.
      endif
c
c...Helical deltas
c
      if (IHELIX .eq. 1) then
          if (ICRHEL(2) .eq. 0) then
              if (ICRHEL(3) .eq. 1) then
                  d1    = (gprm(7) / RAD) * ijk(3)
              else
                  d1    = ijk(3) * (gprm(7)/360.)
              endif
              mnum(kprm(3)) = stnum(kprm(3)) - d1
          else
              d1     = stnum(kprm(3)) - mnum(kprm(3))
          endif
          HELANG(1) = d1
          HELANG(2) = gprm(7)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirdr3 (kprm,gprm,gmat)
c
c   FUNCTION:  This routine calculates the circle parameters, direction,
c              starting angle, ending angle, and delta angle of a
c              circular record.  It should be called when a 3-axis
c              circular interpolation record is active.
c
c   INPUT:    none
c
c
c   OUTPUT:   kprm    I*4  D8   -  Integer circular record buffer.
c                                  (1:3) = XYZ pointer depending on
c                                          plane selection.
c                                  (4) = Starting quadrant (not used).
c                                  (5) = Direction 1 - CLW, 2 = CCLW.
c
c             gprm    R*8  D25  -  Real circular record buffer.
c                                  (1:3) = Center point.
c                                  (4) = Radius.
c                                  (5) = Beginning angle.
c                                  (6) = Ending angle.
c                                  (7) = Unsigned delta angle,
c                                  (8:10) = Ending point.
c                                  (11,12) = Start point (X,Y).
c
c             gmat    R*8  D12  -  Matrix to transform circular record to
c                                  XY-plane.
c
c***********************************************************************
c
      subroutine cirdr3 (kprm,gprm,gmat)
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'
c
      integer*4 kprm(8)
c
      real*8 gprm(25),gmat(12)
c
      equivalence (INCR  ,KPOSMP(1226)), (ICIPR2,KPOSMP(1266))
      equivalence (ICRFMT,KPOSMP(1368)), (IC3AXF,KPOSMP(1747))
c
      integer*4 INCR,IC3AXF(10),ICIPR2(8),ICRFMT
c
      equivalence (REGSTO,POSMAP(1032)), (MCHNUM,POSMAP(1287))
      equivalence (STONUM,POSMAP(1387)), (AXSSTO,POSMAP(1425))
      equivalence (RCIPR2,POSMAP(2074))
c
      REAL*8 REGSTO(MAXFMT),STONUM(3,4),MCHNUM(3,4),AXSSTO(10),
     1       RCIPR2(25)
c
      integer*4 ierr,inc
c
      real*8 spt(3),mpt(3),ept(3),a(3),b(3),c(3),sang,delt,v1(3),an(2),
     1       axspt(10),laxis(6),midpt(3,4),rot(20,2)
c
c...Calculate mid point
c
      if (ICIPR2(1) .eq. 0) then
          call copyn (AXSSTO,axspt,10)
          inc    = INCR
          if (IC3AXF(2) .eq. 2 .and. IC3AXF(9) .eq. 1) then
              if (ICRFMT .eq. 1 .or. ICRFMT .eq. 9) inc = 1
              if (ICRFMT .eq. 2 .or. ICRFMT .eq. 3) inc = 2
          endif

          if (inc .eq. 1) then
              axspt(1) = REGSTO(IC3AXF(5))
              axspt(3) = REGSTO(IC3AXF(6))
              axspt(5) = REGSTO(IC3AXF(7))
          else
              axspt(1) = AXSSTO(1) + REGSTO(IC3AXF(5))
              axspt(3) = AXSSTO(3) + REGSTO(IC3AXF(6))
              axspt(5) = AXSSTO(5) + REGSTO(IC3AXF(7))
          endif
          call alladr (axspt,laxis,midpt,rot,v1,5,1)
c
c......Circular is output in dual block mode
c......Go get end point of circle
c
          if (IC3AXF(2) .eq. 1) then
              ICIPR2(1) = 1
              call copyn (midpt(1,1),RCIPR2(1))
              go to 8000
          endif
      endif
      ICIPR2(1) = 0
c
c...Get vector normal to the plane of the circle.
c... a = vector from starting point to midpoint of arc.
c... b = vector from starting point to endpoint of arc.
c... c = vector normal to plane of circle
c
      call vcmnvc (midpt,STONUM(1,1),a)
      call vcmnvc (MCHNUM(1,1),STONUM(1,1),b)
      call crosvc (a,b,c)
      call unitvc (c,c)
c
c...Get transformation matrix
c...to place circle on XY plane
c
      call gtpola (c,an,gmat)
c
c...Get transformed points
c
      call ptmatr (STONUM(1,1),spt,gmat,2)
      call ptmatr (midpt(1,1),mpt,gmat,2)
      call ptmatr (MCHNUM(1,1),ept,gmat,2)
c
c...Calculate circle
c
      call cirpar (spt,mpt,mpt,ept,gprm,sang,delt,ierr)
c
c...Machining plane indices & direction
c
      kprm(1) = 1
      kprm(2) = 2
      kprm(3) = 3
      kprm(5) = 1
      if (delt .gt. 0.) kprm(5) = 2
c
c...Calculate Radius
c
      v1(1) = spt(kprm(1)) - gprm(kprm(1))
      v1(2) = spt(kprm(2)) - gprm(kprm(2))
      gprm(4) = dsqrt(v1(1)**2 + v1(2)**2)
c
c...Ending point
c
      gprm(kprm(1)+7) = ept(kprm(1))
      gprm(kprm(2)+7) = ept(kprm(2))
      gprm(kprm(3)+7) = ept(kprm(3))
c
c...Starting point
c
      gprm(11) = spt(kprm(1))
      gprm(12) = spt(kprm(2))
      gprm(13) = spt(kprm(3))
c
c...Beginning & ending angle
c
      gprm(5) = sang
      gprm(6) = sang   + delt
      if (gprm(6) .lt. 0.) gprm(6) = gprm(6) + 360.
      if (gprm(6) .gt. 360.) gprm(6) = gprm(6) - 360.
c
c...Delta angle
c
      gprm(7) = delt
      if (gprm(7) .lt. 0.) gprm(7) = gprm(7) + 360.
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirpla (kprm,gprm,geo,ksect)
c
c   FUNCTION:  This routine calculates the geometry used for an APT
c              source circular output record.
c
c   INPUT:    kprm    I*4  D8   -  Integer circular record buffer.
c
c             gprm    R*8  D12  -  Real circular record buffer.
c
c
c   OUTPUT:   geo     R*8  D15  -  Circular commands geometry.
c                                  (1:6)   = PSIS plane.
c                                  (7:9)   = Initial direction (INDIRV).
c                                  (10:15) = Check surface plane.
c
c             ksect   I*4  D1   -  Which intersection of check surface
c                                  to stop on.
c
c***********************************************************************
c
      subroutine cirpla (kprm,gprm,geo,ksect)
c
      integer*4 kprm(8),ksect
c
      real*8 gprm(12),geo(15)
c
      real*8 v1(3),d1
c
c...Part surface plane
c
      geo(kprm(1)) = 0.
      geo(kprm(2)) = 0.
      geo(kprm(3)) = 1.
      geo(4) = gprm(1)
      geo(5) = gprm(2)
      geo(6) = gprm(3)
c
c...Initial direction
c
      v1(kprm(1)) = gprm(11) - gprm(kprm(1))
      v1(kprm(2)) = gprm(12) - gprm(kprm(2))
      v1(kprm(3)) = 0.
      d1    = dsqrt(v1(kprm(1))**2 + v1(kprm(2))**2)
      v1(kprm(1)) = v1(kprm(1)) / d1
      v1(kprm(2)) = v1(kprm(2)) / d1
      call crosvc (v1,geo,geo(7))
      if (kprm(5) .eq. 2) call vctmsc (geo(7),-1.d0,geo(7))
c
c...Check surface plane
c
      v1(kprm(1)) = gprm(kprm(1)+7) - gprm(kprm(1))
      v1(kprm(2)) = gprm(kprm(2)+7) - gprm(kprm(2))
      v1(kprm(3)) = 0.
      d1    = dsqrt(v1(kprm(1))**2 + v1(kprm(2))**2)
      v1(kprm(1)) = v1(kprm(1)) / d1
      v1(kprm(2)) = v1(kprm(2)) / d1
      call crosvc (v1,geo,geo(10))
      geo(13) = gprm(8)
      geo(14) = gprm(9)
      geo(15) = gprm(10)
      ksect   = 1
      if (gprm(7) .gt. 180.5) ksect = 2
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirota (kprm,gprm,geo,ksect)
c
c   FUNCTION:  This routine outputs an APT source circular record.
c
c   INPUT:    kprm    I*4  D8   -  Integer circular record buffer.
c
c             gprm    R*8  D12  -  Real circular record buffer.
c
c             geo     R*8  D15  -  Circular commands geometry.
c
c             ksect   I*4  D1   -  Which intersection of check surface
c                                  to stop on.
c
c             gmat    r*8  D12  -  Matrix to adjust points through when
c                                  'krot' = 1.
c
c             krot    I*4  D1   -  1 = This is a 3-axis circle.  The
c                                  points should be adjusted through the
c                                  matrix rather than by the rotary angles.
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine cirota (kprm,gprm,geo,ksect,gmat,krot)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kprm(8),ksect,krot
c
      real*8 gprm(12),geo(15),gmat(3)
c
      equivalence (XFMFL ,KPOSMP(0921)), (MACHTP,KPOSMP(1201))
      equivalence (IHELIX,KPOSMP(1395))
c
      integer*4 IHELIX,XFMFL(5),MACHTP
c
      equivalence (PI    ,POSMAP(0001)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369))
      equivalence (CIRTOL,POSMAP(2201)), (HELANG,POSMAP(2213))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 HELANG(2),TLVEC(3),ROTSTO(20,2),PI,CIRTOL(5),AXSOUT(10)
c
      integer*2 fdes(10),fdes1(10)
      integer*4 nc,nc1,nc2,nc3,vtyp(4),vwds(4),i
      integer*4 vpsis,vplane,vindir,vtlon,vgofwd,vcirc,vcanon,von,
     1          vintof,vcoupl,vatang,isav
c
      real*8 rsav,ndot,vval(4),tvo(3),rgeo(15),rprm(8),rsto(20,2),
     1       rlin(6),rnum(3)
c
      character*80 aptdat
      byte bdat(80)
c
      equivalence (aptdat,bdat)
c
      data vatang /1/, vcanon /5031/, vcirc /4026/, vcoupl /1049/
      data vgofwd /4030/, vindir /4033/
      data vintof /727/, von /71/, vplane /4032/, vpsis /4029/
      data vtlon /4028/
c
      data fdes  /3,0,1,8,8,4,4,1,0,0/
      data fdes1 /3,0,1,8,8,6,6,1,0,0/
c
c...Adjust rotary axes to transformations
c
      if (XFMFL(4) .ne. 0) then
          isav   = XFMFL(4)
          XFMFL(4) = 0
          call cpyrot (ROTSTO,rsto)
          call axsadr (AXSOUT,rlin,ROTSTO,tvo)
          XFMFL(4) = isav
c
c...Lathe tool axis vector
c
      else if (MACHTP .eq. 2) then
          tvo(1) = 0.
          tvo(2) = 1.
          tvo(3) = 0.
c
c...Calculate tool axis vector at machine
c
      else
          call veadja (TLVEC,tvo)
      endif
c
c...Adjust tool axis for 3-axis circular
c
      if (krot .eq. 1) then
          call ptmatr (tvo,tvo,gmat,2)
      endif
c
c...If the current tool axis and
c...circular axis do not match
c...then create points around circle
c
      if (PCNV_TYPE .eq. PCNV_SIMUL .or.
     1   (tvo(kprm(3)) .gt. -.9995 .and. tvo(kprm(3)) .lt. .9995)) then
          call cirgna (kprm,gprm,3,geo,gmat,krot)
          go to 8000
      endif
c
c...If delta distance is too small then
c...create points around circle
c
      rsav   = 2.d0 * PI * gprm(4) * (gprm(7)/360.d0)
      if (rsav .lt. CIRTOL(2) .or. gprm(4) .lt. CIRTOL(3) .or.
     1    gprm(4) .gt. CIRTOL(4)) then
          call cirgna (kprm,gprm,1,geo,gmat,krot)
          go to 8000
      endif
c
c...PSIS statement
c
      call copyn (geo,rgeo,15)
c
c
      if (krot .eq. 0) then
          call veadjr (rgeo(1),rgeo(1))
          call ptrtad (rgeo(4),rgeo(4),ROTSTO,1)
          call preadr (rgeo(4),rgeo(4),ROTSTO,rgeo(1))
      else
          call ptmatb (geo(1),rgeo(1),gmat,2)
          call ptmatb (geo(4),rgeo(4),gmat,2)
      endif
      call ptxfr (rgeo,rgeo,2)
      call ptxfr (rgeo(4),rgeo(4),1)
      rsav   = rgeo(4)
      rgeo(4) = ndot(rgeo,rgeo(4))
      call getvwd (vpsis,aptdat,nc,1,PSTWRD,PSTWVL,NPSTWD)
      aptdat(7:8) = '/('
      nc     = 8
      call getvwd (vplane,aptdat(nc+1:),nc1,1,PSTWRD,PSTWVL,NPSTWD)
      nc     = nc     + nc1    + 1
      aptdat(nc:nc) = '/'
      do 100 i=1,4,1
          call rtoc (rgeo(i),aptdat(nc+1:),nc1)
          nc     = nc     + nc1    + 1
          if (i .eq. 4) then
              aptdat(nc:nc) = ')'
          else
              aptdat(nc:nc) = ','
          endif
  100 continue
      call ptd_addapt(bdat,nc)
      rgeo(4) = rsav
c
c...INDIRV statement
c
      if (krot .eq. 0) then
          rnum(1) = 0.
          rnum(2) = 0.
          rnum(3) = 0.
          call preadr (rnum,rnum,ROTSTO,rgeo(7))
      else
          call ptmatb (geo(7),rgeo(7),gmat,2)
      endif
      call veadjr (rgeo(7),rgeo(7))
      call ptxfr (rgeo(7),rgeo(7),1)
      call getvwd (vindir,aptdat,nc,1,PSTWRD,PSTWVL,NPSTWD)
      aptdat(7:7) = '/'
      nc     = 7
      do 200 i=1,3,1
          call ftoc (rgeo(i+6),aptdat(nc+1:),nc1,fdes1)
          nc     = nc     + nc1
          if (i .ne. 3) then
              nc     = nc     + 1
              aptdat(nc:nc) = ','
          endif
  200 continue
      call ptd_addapt(bdat,nc)
c
c...COUPLE statement
c...(If helical interpolation)
c
      if (IHELIX .eq. 1) then
          vwds(1) = vcoupl
          vtyp(2) = 2
          vval(2) = HELANG(1)
          vtyp(3) = 1
          vwds(3) = vatang
          vtyp(4) = 2
          vval(4) = HELANG(2)
          nc     = 4
          call ptdf_aptstmt (nc,vtyp,vwds,vval,aptdat)
      endif
c
c...TLON,GOFWD statement
c
      call getvwd (vtlon,aptdat,nc,1,PSTWRD,PSTWVL,NPSTWD)
      nc     = nc     + 1
      aptdat(nc:nc) = ','
      call getvwd (vgofwd,aptdat(nc+1:),nc1,1,PSTWRD,PSTWVL,NPSTWD)
      nc     = nc     + nc1    + 1
      aptdat(nc:nc+1) = '/('
      nc2    = nc
      nc     = nc     + 1
      call getvwd (vcirc,aptdat(nc+1:),nc1,1,PSTWRD,PSTWVL,NPSTWD)
      nc     = nc     + nc1    + 1
      aptdat(nc:nc+1) = '/'
      call getvwd (vcanon,aptdat(nc+1:),nc1,2,PSTWRD,PSTWVL,NPSTWD)
      nc     = nc     + nc1    + 1
      aptdat(nc:nc) = ','
      nc3    = nc
c
      if (krot .eq. 0) then
          call ptrtad (gprm,rprm,ROTSTO,1)
          call preadr (rprm,rprm,ROTSTO,rnum)
      else
          call ptmatb (gprm(1),rprm(1),gmat,2)
      endif
      call ptxfr (rprm,rprm,1)
      do 300 i=1,3,1
          call ftoc (rprm(i),aptdat(nc+1:),nc1,fdes)
          nc     = nc     + nc1    + 1
          aptdat(nc:nc) = ','
  300 continue
      aptdat(nc+1:) = ' $'
      nc     = nc     + 2
      call ptd_addapt(bdat,nc)
c
      aptdat = ' '
      nc     = nc3
      do 310 i=1,3,1
          call ftoc (rgeo(i),aptdat(nc+1:),nc1,fdes1)
          nc     = nc     + nc1    + 1
          aptdat(nc:nc) = ','
  310 continue
      call ftoc (gprm(4),aptdat(nc+1:),nc1,fdes)
      nc     = nc     + nc1    + 1
      aptdat(nc:nc+3) = '), $'
      nc     = nc     + 3
      call ptd_addapt(bdat,nc)
c
      aptdat = ' '
      call getvwd (von,aptdat(nc2+1:),nc1,2,PSTWRD,PSTWVL,NPSTWD)
      nc     = nc2    + nc1    + 1
      aptdat(nc:nc) = ','
      if (ksect .ne. 1) then
          call itoc (ksect,aptdat(nc+1:),nc1,0)
          nc     = nc     + nc1    + 1
          aptdat(nc:nc) = ','
          call getvwd (vintof,aptdat(nc+1:),nc1,2,PSTWRD,PSTWVL,NPSTWD)
          nc     = nc     + nc1    + 1
          aptdat(nc:nc) = ','
      endif
      nc     = nc     + 1
      aptdat(nc:nc) = '('
      call getvwd (vplane,aptdat(nc+1:),nc1,1,PSTWRD,PSTWVL,NPSTWD)
      nc     = nc     + nc1    + 1
      aptdat(nc:nc) = '/'
c
      if (krot .eq. 0) then
          call veadjr (rgeo(10),rgeo(10))
          call ptrtad (rgeo(13),rgeo(13),ROTSTO,1)
          call preadr (rgeo(13),rgeo(13),ROTSTO,rgeo(10))
      else
          call ptmatb (geo(10),rgeo(10),gmat,2)
          call ptmatb (geo(13),rgeo(13),gmat,2)
      endif
      call ptxfr (rgeo(10),rgeo(10),2)
      call ptxfr (rgeo(13),rgeo(13),1)
      rsav = ndot(rgeo(10),rgeo(13))
      do 320 i=1,3,1
          call ftoc (rgeo(i+9),aptdat(nc+1:),nc1,fdes1)
          nc     = nc     + nc1    + 1
          if (i .eq. 4) then
              aptdat(nc:nc) = ')'
          else
              aptdat(nc:nc) = ','
          endif
  320 continue
      call ftoc (rsav,aptdat(nc+1:),nc1,fdes)
      nc     = nc     + nc1    + 1
      aptdat(nc:nc) = ')'
      call ptd_addapt(bdat,nc)
c
c...Final point for helical interpolation
c
      if (IHELIX .eq. 1) call motota
c
c...Reset rotary axes for transformation
c
      if (XFMFL(4) .ne. 0) call cpyrot (rsto,ROTSTO)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirgna (kprm,gprm,kminpt,geo,gmat,krot)
c
c   FUNCTION:  This routine generates and outputs linear points around
c              a circular record.
c
c   INPUT:    kprm    I*4  D8   -  Integer circular record buffer.
c
c             gprm    R*8  D12  -  Real circular record buffer.
c
c             kminpt  I*4  D1   -  Minimum number of points to generate.
c
c             geo     R*8  D15  -  (0:3) = Circular plane vector.
c
c
c             gmat    r*8  D12  -  Matrix to adjust points through when
c                                  'krot' = 1.
c
c             krot    I*4  D1   -  1 = This is a 3-axis circle.  The
c                                  points should be adjusted through the
c                                  matrix rather than by the rotary angles.
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine cirgna (kprm,gprm,kminpt,geo,gmat,krot)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kprm(8),kminpt,krot
c
      real*8 gprm(12),gmat(12),geo(15)
c
      equivalence (IHELIX,KPOSMP(1395))
c
      integer*4 IHELIX
c
      equivalence (RAD   ,POSMAP(0002)), (CIRBUF,POSMAP(0731))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (MOVDIS,POSMAP(1570))
      equivalence (CIRTOL,POSMAP(2201)), (HELANG,POSMAP(2213))
      equivalence (MOVTIM,POSMAP(3546)), (OFEED ,POSMAP(3560))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 HELANG(2),MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),
     1       ROTANG(20,2),CIRTOL(2),RAD,MOVDIS(4),MOVTIM,OFEED(4),
     2       CIRBUF(7)
c
      integer*4 npt,i,inc,ierr
c
      real*8 dang,hdis,bang,mnum(3),d,rnum
c
      character*80 msg
c
c...Output circular record to simulation file
c
      if (PCNV_TYPE .eq. PCNV_SIMUL) then
          call ptxfr (CIRBUF(1),CIRBUF(1),2)
          call ptxfr (CIRBUF(4),CIRBUF(4),1)
          if (krot .eq. 0) then
              call ptrtad (gprm(1),CIRBUF(1),ROTANG,1)
              call veadjr (geo,CIRBUF(4))
          else
              call ptmatb (gprm(1),CIRBUF(1),gmat,2)
              call ptmatb (geo,CIRBUF(4),gmat,2)
          endif
          CIRBUF(7) = gprm(4)
          call simcir (msg,ierr)
      endif
c
c...Save final point
c
      if (krot .eq. 1) then
          inc    = 1
          call ptmatr (MCHNUM(1,inc),mnum,gmat,2)
      else
          inc    = 3
          call ptxfr (MCHNUM(1,inc),mnum,1)
      endif
c
c...Calculate maximum angle to move
c
      rnum   = (gprm(4)-CIRTOL(1)) / gprm(4)
      d      = dacos(rnum) * 2.
      npt    = (gprm(7)/RAD) / d + 1
      if (npt .lt. kminpt) npt = kminpt
      dang    = (gprm(7)/RAD) / npt
      if (IHELIX .eq. 1) then
          hdis = HELANG(1) / (HELANG(2)/RAD) * dang
          MCHNUM(kprm(3),inc) = MCHNUM(kprm(3),inc) + HELANG(1)
      endif
      if (kprm(5) .eq. 1) dang = -dang
c
c...Create points around circle
c
      bang   = gprm(5) / RAD
      do 400 i=1,npt,1
          if (i .lt. npt) then
              bang   = bang   + dang
              MCHNUM(kprm(1),inc) = gprm(kprm(1)) + gprm(4) *
     1            dcos(bang)
              MCHNUM(kprm(2),inc) = gprm(kprm(2)) + gprm(4) *
     1            dsin(bang)
              if (krot .eq. 1)
     1            MCHNUM(kprm(3),inc) = gprm(kprm(3))
              if (IHELIX .eq. 1)
     1            MCHNUM(kprm(3),inc) = MCHNUM(kprm(3),inc) - hdis
          else
              MCHNUM(1,inc) = mnum(1)
              MCHNUM(2,inc) = mnum(2)
              MCHNUM(3,inc) = mnum(3)
          endif
          if (krot .eq. 0) then
              call ptxfr (MCHNUM(1,inc),MCHNUM(1,inc),1)
              call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,2,1)
          else
              call ptmatb (MCHNUM(1,1),MCHNUM(1,1),gmat,2)
              call ptrtad (MCHNUM(1,1),MCHNUM(1,3),ROTANG(1,1),0)
          endif
          call preadj (MCHNUM(1,1),MCHNUM(1,2),TLVEC,TLVEC)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,2,5)
          call mdistm
          MOVTIM = MOVDIS(3) / OFEED(2)
          call motota
  400 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  circnv (kprm,gprm)
c
c   FUNCTION:  This routine converts a circular record from the input
c              format to the output format.
c
c   INPUT:    kprm    I*4  D8   -  Integer circular record buffer.
c                                  (1:3) = XYZ pointer depending on
c                                          plane selection.
c                                  (4) = Starting quadrant (not used).
c                                  (5) = Direction 1 - CLW, 2 = CCLW.
c
c             gprm    R*8  D12  -  Real circular record buffer.
c                                  (1:3) = Center point.
c                                  (4) = Radius.
c                                  (5) = Beginning angle.
c                                  (6) = Ending angle.
c                                  (7) = Unsigned delta angle,
c                                  (8:10) = Ending point.
c                                  (11,12) = Start point (X,Y).
c
c   OUTPUT:   none
c
c***********************************************************************
c
      subroutine circnv (kprm,gprm)
c
      include 'menu.inc'
      include 'post.inc'
      include 'ptedpost.inc'
c
      integer*4 kprm(8)
c
      real*8 gprm(12)
c
      equivalence (INCR   ,KPOSMP(1226))
      equivalence (ICRFMTI,KPOSMP(1368)), (ICRSGNI,KPOSMP(1369))
      equivalence (CIRCODI,KPOSMP(1378))
c
      integer*4 ICRFMTI,ICRSGNI,CIRCODI(6),INCR
c
      equivalence (STONUM,POSMAP(1387))
c
      REAL*8 STONUM(3,4)
c
      equivalence (REGSW ,OKPOSMP(0405))
      equivalence (ICRFMT,OKPOSMP(1368)), (ICRSGN,OKPOSMP(1369))
      equivalence (CIRCOD,OKPOSMP(1378)), (ICRIJP,OKPOSMP(1384))
c
      integer*4 ICRFMT,CIRCOD(6),ICRSGN,ICRIJP,REGSW(MAXFMT)
c
      equivalence (REGSTO,OPOSMAP(1032))
c
      REAL*8 REGSTO(MAXFMT)
c
      integer*4 ifmt,ix(3),i,ccod(6)
c
      real*8 rnum,prm(12)
c
c...Determine if circular format is the same
c
      if (ICRFMTI .eq. ICRFMT .and. ICRSGNI .eq. ICRSGN) go to 8000
      if (ICRFMT .eq. 7 .or. ICRFMT .eq. 8) go to 8000
c
c...Remove old center point
c
      ix(1) = kprm(1) + 2
      ix(2) = kprm(2) + 2
      ix(3) = 6
      do 100 i=1,3,1
         if (CIRCODI(ix(i)) .ne. 0) REGSW(CIRCODI(ix(i))) = 0
  100 continue
c
c...Format new center point
c
      call copynk (CIRCOD,ccod,6)
      call copyn (gprm,prm,12)
      ifmt   = ICRFMT
c
c......Determine if absolute or incremental
c......center point output based on abs/incr mode
c
      if (ifmt .eq. 4) then
          if (INCR .eq. 1) then
              ifmt   = 1
          else
              ifmt   = 2
          endif
      else if (ifmt .eq. 5) then
          if (INCR .eq. 1) then
              ifmt   = 1
          else
              ifmt   = 3
          endif
      endif
c
c...Set center point registers
c
      if (ICRIJP .eq. 1 .or. kprm(3) .eq. 0) then
          ix(1) = 3
          ix(2) = 4
          ix(3) = 5
      else
          ix(1) = kprm(1) + 2
          ix(2) = kprm(2) + 2
          ix(3) = kprm(3) + 2
      endif
c
c...Absolute IJK's
c
      if (ifmt .eq. 1) then
          REGSTO(ccod(ix(1))) = prm(kprm(1))
          REGSW(ccod(ix(1))) = 1
          REGSTO(ccod(ix(2))) = prm(kprm(2))
          REGSW(ccod(ix(2))) = 1
c
c...Incremental IJK's
c
      else if (ifmt .eq. 2 .or. ifmt .eq. 3) then
          do 300 i=1,2,1
              rnum   = prm(kprm(i)) - STONUM(kprm(i),3)
              if (ifmt .eq. 2) then
                  if (ICRSGN .eq. 2) rnum   = rnum   * (-1.)
              else
                  rnum   = dabs(rnum)
              endif
              REGSTO(ccod(ix(i))) = rnum
              REGSW(ccod(ix(i))) = 1
  300     continue
c
c...Radius
c
      else if (ifmt .eq. 6) then
          rnum   = prm(4)
          if (prm(7) .gt. 180.d0) rnum = rnum * (-1.)
          REGSTO(ccod(6)) = prm(kprm(2))
          REGSW(ccod(6)) = 1
c
c...Absolute & Radius
c
      else if (ifmt .eq. 9) then
          REGSTO(ccod(ix(1))) = prm(kprm(1))
          REGSW(ccod(ix(1))) = 1
          REGSTO(ccod(ix(2))) = prm(kprm(2))
          REGSW(ccod(ix(2))) = 1
          rnum   = prm(4)
          if (prm(7) .gt. 180.d0) rnum = rnum * (-1.)
          REGSTO(ccod(6)) = prm(4)
          REGSW(ccod(6)) = 1
      endif
c
c...End of routine
c
 8000 return
      end
