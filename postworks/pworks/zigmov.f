c
c***********************************************************************
c
c   FILE NAME:  zigmov
c   CONTAINS:
c               zigmov
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        zigmov.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:13
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  zigmov
c
c   FUNCTION:  This is the controlling routine for Motion (Type 5000)
c              records.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine zigmov (kfl,gdis)
c
      integer*4 kfl
      real*8 gdis
c
      include 'post.inc'
c
      equivalence (IRAP  ,KPOSMP(3199)), (IZIGON,KPOSMP(0370))
      equivalence (HLDFLG,KPOSMP(1622)), (ISCIRC,KPOSMP(1238))
      equivalence (IZIGAD,KPOSMP(0371))
c
      integer*4 HLDFLG,IRAP,IZIGON,ISCIRC,IZIGAD
c
      equivalence (MCHNUM,POSMAP(1287)), (STONUM,POSMAP(1387))
      equivalence (LINAXS,POSMAP(1299)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (ZIGDIS,POSMAP(0188))
      equivalence (ZIGHIG,POSMAP(0187))
      equivalence (ZIGSLP,POSMAP(0189)), (ZIGDEP,POSMAP(0190))
      equivalence (HLDMCH,POSMAP(2141)), (ZIGSTO,POSMAP(0191))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),HLDMCH(3,4),
     -       ROTANG(20,2),ZIGHIG,ZIGDEP,ZIGSLP,ZIGDIS,STONUM(3,4),
     -       ZIGSTO
c
      real*8 d,dlt,sls,dcut,dmax,h,s,r,hl,hm,ndist,pt(3),pto(3),
     -       pln(4)
c
      integer*4 ier
c
c...Reset zig if Rapid Motion
c
      if (IRAP .ne. 0) then
          ZIGDEP = 0.0
          go to 8000
      end if
      ZIGSTO = ZIGDEP
c
c...Initialize routine
c
      h      = ZIGHIG
      if (ZIGDIS .eq. 0.0) go to 800
      if (IZIGON .eq. 2) then
          IZIGON = 0
          h  = 0.
          go to 800
      end if
      sls    = 2. * ZIGHIG/ZIGDIS
      if (ZIGSLP .gt. 0.0) then
          hl = ZIGDEP
          s  = sls
          hm = ZIGHIG
      else
          hl = ZIGHIG-ZIGDEP
          s  = 0. - sls
          hm = 0.
      end if
      r      = hl / ZIGHIG
c
c...Set reverse distance and maximal linear limit
c
      dcut   = .5 * (1.0 - r) * ZIGDIS
      if (r .lt. .5) then
          dmax = 100000.0
      else
          dmax = 3.0 * dcut
      end if
c
      if (kfl .eq. 1) then
c
c...Linear motion
c......Get movement dist in tool vector plane
c
          pln(1) = TLVEC(1)
          pln(2) = TLVEC(2)
          pln(3) = TLVEC(3)
          pln(4) = 0.0
          if (HLDFLG .eq. 1) then
              call plnint (HLDMCH(1,2),pln,pln,pto,ier)
          else
              call plnint (STONUM(1,2),pln,pln,pto,ier)
          end if
          call plnint (MCHNUM(1,2),pln,pln,pt,ier)
          dlt    = ndist (pt,pto)
      else
c
c...Circular motion
c...Use input parameter
c
          dlt    = gdis
      end if
c
c...Get current depth
c
      if (dlt .lt. dcut) then
          h   = ZIGDEP + s * dlt
      else if (dlt .le. dmax) then
          h   = hm
          s   = 0. - s
      else
          d   = dlt - dcut
          if (d .gt. .5*ZIGDIS) d = .5*ZIGDIS
          h   = hm - s * d
          s   = 0. - s
      end if
c
c...Store current depth for next move
c
      ZIGSLP = s
  800 ZIGDEP = h
c
c...Shift point at this height
c
      if (kfl .eq. 1) then
          IZIGAD = 1
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,2,5)
          IZIGAD = 2
      end if
c
 8000 return
      end
c
      subroutine zigrst
c
      include 'post.inc'
c
      equivalence (MCHNUM,POSMAP(1287)), (STONUM,POSMAP(1387))
      equivalence (LINAXS,POSMAP(1299)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (ZIGDIS,POSMAP(0188))
      equivalence (ZIGHIG,POSMAP(0187))
      equivalence (ZIGSLP,POSMAP(0189)), (ZIGDEP,POSMAP(0190))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),
     1       ROTANG(20,2),ZIGHIG,ZIGDEP,ZIGSLP,ZIGDIS,STONUM(3,4)
c
      if (ZIGDEP .ne. 0.0) then
          call clrmot (1)
          MCHNUM(1,2) = MCHNUM(1,2) + ZIGDEP * TLVEC(1)
          MCHNUM(2,2) = MCHNUM(2,2) + ZIGDEP * TLVEC(2)
          MCHNUM(3,2) = MCHNUM(3,2) + ZIGDEP * TLVEC(3)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,2,5)
          call rapset (1,2)
          call movpos
          call raprst
          ZIGDEP = 0.0
      end if
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  zigcir (gprm,kprm)
c
c   FUNCTION:  This routine sets parameters for helical motion to
c              simulate zigzag at arc.
c
c   INPUT:   gprm    R*8  D25 -  Output circle parameters (RCIPRM form).
c
c            kprm    I*4  D8  -  Output circle parameters (ICIPRM form).
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine zigcir (gprm,kprm)
c
      integer*4 kprm(8)
      real*8 gprm(25)
c
      include 'post.inc'
c
      equivalence (IHELIX,KPOSMP(1392)), (ICRHEL,KPOSMP(4250))
c
      integer*4 IHELIX,ICRHEL(10)
c
      equivalence (TLVEC ,POSMAP(1369)), (ZIGDEP,POSMAP(0190))
      equivalence (HELANG,POSMAP(2213)), (RAD   ,POSMAP(0002))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 TLVEC(3),ZIGDEP,ROTANG(20,2),HELANG(2),RAD
c
      real*8 tv(3),dlt,zold
c
      integer*4 is3
c
c...Check circle plan & TV relation
c...and helical support
c
      call pivvec (ROTANG,tv)
      is3    = kprm(3)
      if (is3 .eq. 0 .or. dabs(tv(is3)) .lt. .99 .or. ICRHEL(1) .ne. 1)
     1    go to 8000
c
c...Get arc length and
c...calculate zig depth for this arc
c
      dlt    = gprm(4) * gprm(7)/RAD
      zold   = ZIGDEP
      call zigmov (2,dlt)
      HELANG(1) = (ZIGDEP - zold) / gprm(7)
      if (tv(is3) .lt. 0.0) HELANG(1) = 0.0 - HELANG(1)
      IHELIX = ICRHEL(1)
c
 8000 return
      end
