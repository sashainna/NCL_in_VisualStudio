c
c********************************************************************
c
c     FILE NAME: getang
c     CONTAINS:
c               getang  gettvp  gettpl  getspl  defang  rotini
c               rscini  getijk  ptrtad  ptswad  tabadj  pivadj
c               pivvec
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        getang.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:07
c
c********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  getang (gtv,gabc1,gabc2,gvcdif,kerr)
c
c   FUNCTION:  This routine defines rotary coordinates for the input
c              tool vector
c
c   INPUT:  gtv     R*8  D3  -  tool vector
c
c   OUTPUT: gabc1   R*8  D2  -  the first set of angles
c
c           gabc2   R*8  D2  -  the second set of angles
c
c           gvcdif  R*8  D3  -  the vector of difference between
c                               obtained solution and input vector
c
c           kerr    I*4  D1  -  error status
c                               0 - OK
c                               1 - active 2 axis have same rotary axis
c                               2 - available rotary geometry can not
c                                   satisfy the input tool vector.
c                               3 - spindle vector is horizontal (for
c                                   swivel head only)
c
c***********************************************************************
c
      subroutine getang (gtv,gabc1,gabc2,gvcdif,kerr)
c
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (IRTACT,KPOSMP(1256))
      equivalence (IRTPNT,KPOSMP(1258)), (IZSWIV,KPOSMP(1224))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRTYPE,KPOSMP(1486)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IRTYPE(20),IRTWRK(20),IRTACT(2),IRTPNT(2),
     -          IZSWIV,IRTINC(4)
c
      equivalence (VECTOL,POSMAP(1247))
      equivalence (SPLVEC,POSMAP(1350)), (SPLDIS,POSMAP(1353))
      equivalence (TPLVEC,POSMAP(1354))
      equivalence (GAMMA ,POSMAP(1358)), (TPLDIS,POSMAP(1368))
      equivalence (VECSAV,POSMAP(1372)), (ROTSTO,POSMAP(5213))
      equivalence (SADAN ,POSMAP(5334)), (TADAN ,POSMAP(5354))
c
      real*8 SPLVEC(3),SPLDIS,TPLVEC(3),GAMMA(2),SADAN(20),TADAN(20),
     1       TPLDIS,VECSAV(3),ROTSTO(20,2),VECTOL
c
      integer*4 kerr
c
      real*8 gtv(3),gabc1(2),gabc2(2),gvcdif(3)
c
      real*8 rvecz,avpt(3),avpt2(3),avps(3),avps2(3),abc(20),
     -       tvo(3),rnum,nmag
c
      integer*4 itvin,isvin,itve,isvs,i,j
c
      kerr   = 0
      itvin  = IRTINC(IRTACT(1))
      isvin  = IRTINC(IRTACT(2))
c
c...see if 3 axis machine
c
      if (IRTNUM .eq. 0 .or. itvin .eq. 0) then
          call vcmnvc(VECSAV,gtv,gvcdif)
          rnum   = nmag(gvcdif)
          if (rnum .gt. VECTOL) kerr = 2
          return
      end if
c
c...multiaxis machine
c...26-aug-96 vp. changed test to d-5 from d-7.
c
      call betvec (VECSAV,gtv,rnum)
      if (rnum .lt. 1.d-9) go to 900
      itve   = IRTPNT(1)
      isvs   = IRTPNT(2)
c
c...get the first rotary axis plane and starting angle
c
      call gettvp (gtv,TPLDIS,GAMMA(1))
c
c...see if tool vector is on rotary axis or almost
c
      rvecz  = dabs (TPLDIS)
      rvecz  = dnint (rvecz*10.0d0**14) / 10.0d0**14
c
c...get solution vectors
c
      call getpnt (TPLVEC,SPLVEC,TPLDIS,SPLDIS,avpt,avpt2,VECTOL,kerr)
      if (kerr .eq. 1) go to 900
c
      do 205 i=1,3
          avps(i) = avpt(i)
          avps2(i) = avpt2(i)
  205 continue
c
c...adjust back crossing vectors for deviation angles
c...order reversed then in "gettpl, getspl" routines
c...heads first
c
      do 305 j=isvs,isvin-1,1
          if (TADAN(j) .ne. 0.) then
              call vecadj (avpt,avpt,TADAN(j),IRTWRK(j))
              call vecadj (avpt2,avpt2,TADAN(j),IRTWRK(j))
          end if
          if (SADAN(j) .ne. 0.) then
              call vecadj (avps,avps,SADAN(j),IRTWRK(j))
              call vecadj (avps2,avps2,SADAN(j),IRTWRK(j))
          end if
  305 continue
c
c...then tables
c
      do 405 j=itve,itvin+1,-1
          if (TADAN(j) .ne. 0.) then
              call vecadj (avpt,avpt,TADAN(j),IRTWRK(j))
              call vecadj (avpt2,avpt2,TADAN(j),IRTWRK(j))
          end if
          if (SADAN(j) .ne. 0.) then
              call vecadj (avps,avps,SADAN(j),IRTWRK(j))
              call vecadj (avps2,avps2,SADAN(j),IRTWRK(j))
          end if
  405 continue
c
c...get rotary angles
c...first set
c
      call defang (avpt,avps,rvecz,GAMMA,gabc1)
c
c...second set
c
      call defang (avpt2,avps2,rvecz,GAMMA,gabc2)
c
c...get the actual tool vector (using the first set)
c
      do 505 i=1,IRTNUM,1
          abc(i) = ROTSTO(i,1)
  505 continue
      abc(itvin) = gabc1(1)
      if (isvin .le. IRTNUM) abc(isvin) = gabc1(2)
      call getijk (abc,tvo)
c
c...setup the error vector
c
      gvcdif(1) = tvo(1) - gtv(1)
      gvcdif(2) = tvo(2) - gtv(2)
      gvcdif(3) = tvo(3) - gtv(3)
c
c...check for horizontal spindle if swivel head
c
      if (IZSWIV .eq. 1) then
          tvo(1) = 0.d0
          tvo(2) = 0.d0
          tvo(3) = 1.d0
          do 605 i=IRTNUM,1,-1
              if (IRTYPE(i) .eq. 2)
     -            call vecadj (tvo,tvo,abc(i),IRTWRK(i))
  605     continue
          if (dabs(tvo(3)) .lt. 1.d-6) kerr = 5
      end if
c
c...errors
c
  900 return
      end
c
c***************************************************************
c
c   SUBROUTINE:  gettpl (gpl,gstan,gadan)
c
c   FUNCTION:  This routine defines plane created rotating tool
c              vector around the first rotary axis and starting
c              angle of the tool vector for this axis.
c
c   INPUT:  none
c
c   OUTPUT: gpl     R*8  D3  -  the plane vector
c
c           gstan   R*8  D1  -  zero position of the first rotary axis
c
c           gadan   R*8  D20 -  angles used to rotate the plane vector
c
c***************************************************************
c
      subroutine gettpl (gpl,gstan,gadan)
c
      include 'post.inc'
c
      real*8 gpl(3),gstan,gadan(20)
c
      equivalence (IRTACT,KPOSMP(1256))
      equivalence (IRTPNT,KPOSMP(1258)), (NCONTB,KPOSMP(1347))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTYPE(20),IRTWRK(20),IRTACT(2),IRTPNT(2),
     -          NCONTB,IRDEAD(20),IRTINC(4)
c
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2)
c
      integer*4 jrt,irot,idev,itvin,itve,iflg,jtax
c
      real*8 devt,angp
c
      itvin  = IRTINC(IRTACT(1))
      itve   = IRTPNT(1)
c
c...get type of the first rotary axis
c
      jrt    = IRTYPE(itvin)
      irot   = IRTWRK(itvin)
c
c...get plane vector
c
      gpl(1) = 0.d0
      gpl(2) = 0.d0
      gpl(3) = 0.d0
      gpl(irot) = 1.d0
      angp   = 0.d0
c
c...adjust plane vector for locked carriers
c...if rotary axis is table, in reversed direction
c
      if (jrt .eq. 2) go to 200
      iflg   = 1
c
c...order from rider to carrier
c
      do 180 jtax=itvin+1,itve,1
          if (NCONTB .eq. 2) then
              if (IRTYPE(jtax) .eq. 1 .and.
     -            IRDEAD(jtax) .ne. 0) go to 180
          end if
          idev   = IRTWRK(jtax)
          if (irot .ne. idev) iflg = 0
          gadan(jtax) = ROTSTO(jtax,1)
          devt   = 360.d0 - ROTSTO(jtax,1)
          if (iflg .eq. 0) then
              call vecadj (gpl,gpl,devt,IRTWRK(jtax))
          else
c
c...The following code causes a dual rotary table
c...with the rider around the Z-axis and kicked
c...around the Z-axis on the machine base to be off
c...by the angle of the kick (PWORKS_9403.MDF)
c...Commenting out this code does not affect any
c...of the production test output
c...Bobby - 05/09/13
c
cc              angp = angp + devt
cc              if (angp .gt. 360.d0) angp = angp - 360.d0
          end if
  180 continue
c
  200 gstan  = angp
      return
      end
c
c***************************************************************
c
c   SUBROUTINE:  gettvp (gtv,gdist,gstrt)
c
c   FUNCTION:  This routine defines plane distance created by tool
c              vector rotated around the first rotary axis and
c              starting angle of the tool vector for this axis.
c
c   INPUT:  gtv     R*8  D3  -  the current vector
c
c   OUTPUT: gdist   R*8  D1  -  the plane distance from 0,0,0
c
c           gstrt   R*8  D1  -  the starting angle of the vector
c
c***************************************************************
c
      subroutine gettvp (gtv,gdist,gstrt)
c
      include 'post.inc'
c
      equivalence (IRTACT,KPOSMP(1256)), (NCONTB,KPOSMP(1347))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTYPE(20),IRTWRK(20),IRTACT(2),
     -          IRDEAD(20),NCONTB,IRTINC(4)
c
      equivalence (TPLANG,POSMAP(1357)), (ROTSTO,POSMAP(5213))
c
      real*8 TPLANG,ROTSTO(20,2)
c
      real*8 gtv(3),gdist,gstrt
c
      integer*4 irot,itvin,jtax
c
      real*8 rtv(3),devt,angv
c
c...get the first rotary axis
c
      itvin  = IRTINC(IRTACT(1))
      irot   = IRTWRK(itvin)
c
      rtv(1) = gtv(1)
      rtv(2) = gtv(2)
      rtv(3) = gtv(3)
c
c...adjust tool vector for locked axis if any
c...order from carrier to rider
c
      do 250 jtax=1,itvin-1,1
          if (NCONTB .eq. 2) then
              if (IRTYPE(jtax) .eq. 1 .and.
     -            IRDEAD(jtax) .ne. 0) go to 250
          end if
          devt   = 360.d0 - ROTSTO(jtax,1)
          call vecadj (rtv,rtv,devt,IRTWRK(jtax))
  250 continue
c
c...get plane distance
c
  300 gdist  = rtv(irot)
c
c...end starting angle
c
      call vecang (rtv,irot,angv)
      gstrt  = TPLANG + angv
      if (gstrt .gt. 360.d0) gstrt = gstrt - 360.d0
c
      return
      end
c
c***************************************************************
c
c   SUBROUTINE:  getspl (gpl,gdist,gstrt,gadan,gsvec)
c
c   FUNCTION:  This routine defines plane created rotating spindle
c              vector around the second rotary axis and starting
c              angle of the spindle vector for this axis.
c
c   INPUT:  none
c
c   OUTPUT: gpl     R*8  D3  -  The plane vector
c
c           gdist   R*8  D1  -  The plane distance from 0,0,0
c
c           gstrt   R*8  D1  -  The starting angle of the vector
c
c           gadan   R*8  D20 -  Angles used to rotate the plane vector
c
c           gsvec   R*8  D3  -  Spindel vector adjusted for locked
c                               heads
c
c***************************************************************
c
      subroutine getspl (gpl,gdist,gstrt,gadan,gsvec)
c
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (IRTACT,KPOSMP(1256))
      equivalence (IRTPNT,KPOSMP(1258)), (NCONTB,KPOSMP(1347))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IRTYPE(20),IRTWRK(20),IRTACT(2),IRTPNT(2),
     -          IRDEAD(20),NCONTB,IRTINC(4)
c
      equivalence (SPIVEC,POSMAP(3583)), (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2),SPIVEC(3)
c
      real*8 gpl(3),gdist,gstrt,gadan(20),gsvec(3)
c
      integer*4 jrt,irot,idev,isvin,isvs,ifl1,i,jtax,iflg
c
      real*8 rsv(3),devt,angp,angv
c
      isvin  = IRTINC(IRTACT(2))
      isvs   = IRTPNT(2)
      ifl1   = 0
c
c...set spindle vector
c
      do 110 i=1,3
          rsv(i) = SPIVEC(i)
          gsvec(i) = SPIVEC(i)
          gpl(i) = 0.0
  110 continue
c
c...get type of the second rotary axis if exist,
c...if not then substitute by spindle vector
c
      if (isvin .gt. IRTNUM) then
          gpl(1) = SPIVEC(1)
          gpl(2) = SPIVEC(2)
          gpl(3) = SPIVEC(3)
          gdist  = 1.d0
          gstrt  = 0.d0
          ifl1   = 1
          jrt    = 2
      else
          jrt    = IRTYPE(isvin)
          irot   = IRTWRK(isvin)
          gpl(irot) = 1.d0
      end if
c
      angp   = 0.d0
c
c...adjust plane vector for locked carriers
c...if rotary axis is head only
c...order from rider to carrier
c
      if (jrt .eq. 1) go to 200
      iflg   = 1
      do 180 jtax=isvin-1,isvs,-1
          idev   = IRTWRK(jtax)
          if (irot .ne. idev) iflg = 0
          if (IRTYPE(jtax) .eq. 1) go to 180
          devt   = ROTSTO(jtax,1)
          gadan(jtax) = 360.d0 - devt
          if (iflg .eq. 0) then
              call vecadj (gpl,gpl,devt,IRTWRK(jtax))
              call vecadj (gsvec,gsvec,devt,IRTWRK(jtax))
          else
              if (IRTYPE(jtax) .eq. 2) go to 180
              angp = angp + devt
              if (angp .gt. 360.d0) angp = angp - 360.d0
          end if
  180 continue
      if (ifl1 .eq. 1) go to 8000
c
c...adjust spindle vector for locked axis if any
c
  200 do 280 jtax=IRTNUM,isvin+1,-1
          if (NCONTB .eq. 2) then
              if (IRTYPE(jtax) .eq. 1 .and.
     -            IRDEAD(jtax) .ne. 0) go to 280
          end if
          call vecadj (rsv,rsv,ROTSTO(jtax,1),IRTWRK(jtax))
  280 continue
c
c...get plane distance
c
  300 gdist  = rsv(irot)
c
c...end starting angle
c
      call vecang (rsv,irot,angv)
      gstrt  = angp + angv
      if (gstrt .gt. 360.d0) gstrt = gstrt - 360.d0
c
 8000 return
      end
c
c***************************************************************
c
c   SUBROUTINE:  defang (gtvec,gsvec,gdist,gamma,gangle)
c
c   FUNCTION:  This routine defines rotary position of rotary axis
c
c   INPUT:  gtvec   R*8  D3  -  tool vector coordinates
c
c           gsvec   R*8  D3  -  spindle vector coordinates
c
c           gdist   R*8  D1  -  tool plane distance
c
c           gamma   R*8  D2  -  zero position of the tool and spindle
c                               rotary axis (deg).
c
c   OUTPUT: gangle  R*8  D2  -  rotary axis position (absolute in deg).
c
c***************************************************************
c
      subroutine  defang (gtvec,gsvec,gdist,gamma,gangle)
c
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IRTWRK(20),IRTACT(2),IRTINC(4)
c
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2)
c
      real*8 gtvec(3),gsvec(3),gdist,gamma(2),gangle(2)
c
      integer*4 itvin,isvin
c
      real*8 rott,tang
c
      itvin  = IRTINC(IRTACT(1))
      isvin  = IRTINC(IRTACT(2))
c
c...first rotary axis (tool vector position)
c...see if tool vector is on tool rotary axis
c
      if (gdist .ne. 1.d0) then
          call vecang (gtvec,IRTWRK(itvin),tang)
          rott   = gamma(1) - tang
          if (rott .lt. 0.d0) rott = rott + 360.d0
          if (rott .ge. 360.d0 - 1.d-6) rott = rott - 360.d0
          gangle(1) = rott
      else
          gangle(1) = ROTSTO(itvin,1)
      end if
c
c...second rotary axis (spindle vector position)
c...if exist
c
      if (isvin .le. IRTNUM) then
          call vecang (gsvec,IRTWRK(isvin),tang)
          rott   = tang - gamma(2)
          if (rott .lt. 0.d0) rott = rott + 360.d0
          if (rott .ge. 360.d0 - 1.d-6) rott = rott - 360.d0
          gangle(2) = rott
      end if
c
      return
      end
c
c*******************************************************************
c
c   SUBROUTINE:  rotini (kerr)
c
c   FUNCTION:  This routine sets first and second rotary axis
c              indexes, indexes of "end of first axis description",
c              "start of second axis description" and spindle plane
c              definition.
c
c   NOTE:      This routine initializes variables for rotary axes
c              using: IRTNUM - number of specifieded rotary axis,
c                     IRTYPE - array with type of rotary axis
c                              Table - 1, Head - 2
c                     IRDEAD - array with flags marking locked axis
c                              0 - active, 1 - locked, -1 - internal
c              Parameters have to be stored in the following order:
c              Tables from Riders to Carriers first, then Heads from
c              Carriers to Riders.  Up to 4 axes can be specified at
c              once with up to 2 active.
c
c   INPUT:  none
c
c   OUTPUT: kerr    I*4  D1  -  error status:
c                               0 - OK
c                               1 - two rotary axis are same,
c                                   1-st active, 2-nd locked
c                               2 - single rotary axis is spindle axis
c                                   1-st locked
c                               3 - equivalent 1 and 2
c
c********************************************************************
c
      subroutine rotini (kerr)
c
      include 'post.inc'
c
      equivalence (ISIDBL,KPOSMP(0812)), (IRTOUT,KPOSMP(0813))
      equivalence (MACHTP,KPOSMP(1201)), (IRTNUM,KPOSMP(1243))
      equivalence (IRTACT,KPOSMP(1256))
      equivalence (IRTPNT,KPOSMP(1258)), (LASTAB,KPOSMP(1260))
      equivalence (IFHEAD,KPOSMP(1281)), (IRTINC,KPOSMP(1461))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTDEF,KPOSMP(1485)), (IRTWRK,KPOSMP(1506))
      equivalence (MOTBLD,KPOSMP(3984))
c
      integer*4 IRTNUM,IRTYPE(20),IRDEAD(20),IRTACT(2),IRTPNT(2),
     -          MACHTP,LASTAB,IFHEAD,IRTWRK(20),ISIDBL,IRTDEF,
     -          IRTOUT(4),MOTBLD,IRTINC(4)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (VECTOL,POSMAP(1247)), (PPTOLR,POSMAP(1274))
      equivalence (SPLVEC,POSMAP(1350)), (SPLDIS,POSMAP(1353))
      equivalence (TPLVEC,POSMAP(1354)), (TPLANG,POSMAP(1357))
      equivalence (GAMMA ,POSMAP(1358)), (SPIVEC,POSMAP(3583))
      equivalence (PPINCR,POSMAP(1443))
      equivalence (SPIVEC,POSMAP(3583)), (BLDVEC,POSMAP(3589))
      equivalence (BLDSTO,POSMAP(3592)), (ROTSTO,POSMAP(5213))
      equivalence (SADAN ,POSMAP(5334)), (TADAN ,POSMAP(5354))
c
      real*8 SPLVEC(3),SPLDIS,TPLVEC(3),TPLANG,GAMMA(2),SADAN(20),
     1       BLDSTO,BLDVEC(3),TADAN(20),SPIVEC(3),ROTSTO(20,2),VECTOL,
     2       PPTOLR(10),RAD,PPINCR(10)
c
      integer*4 kerr
c
      integer*4 i,itvin,isvin,itve,isvs,next,iru(20)
c
      real*8 rnum,snum,spinad(3),devt,pl(4),ndot
c
c...initiliaze rotary flags
c
      kerr   = 0
      next   = 1
      LASTAB = 0
      IFHEAD = 2
      do 95 i=1,20,1
          iru(i) = IRDEAD(i)
   95 continue
  100 itvin  = 0
      isvin  = 0
      VECTOL = .000001
      if (IRTNUM .eq. 0) return
      rnum   = 0
      do 105 i=1,IRTNUM,1
          if (IRTYPE(i) .eq. 2) then
              LASTAB = i - 1
              IFHEAD = 1
              go to 200
          end if
  105 continue
      LASTAB = IRTNUM
c
c...reset angles used to adjust plane's vector
c...get active axis indexes
c
  200 do 205 i=1,IRTNUM,1
          TADAN(i) = 0.d0
          SADAN(i) = 0.d0
          if (iru(i) .eq. 0) then
              if (itvin .eq. 0) then
                  itvin = i
              else
                  isvin = i
              end if
          end if
  205 continue
c
      do 210 i=1,IRTDEF,1
          IRTOUT(i) = 1
          if (PPTOLR(i+6) .gt. rnum) rnum = PPTOLR(i+6)
          if (PPINCR(i+6) .gt. rnum) rnum = PPINCR(i+6)
  210 continue
      VECTOL = dsin(rnum/RAD)
c
c...if second axis does not exist set its index behind all axis
c
      if (isvin .eq. 0) then
          isvin = IRTNUM + 1
          next  = 0
      end if
c
c...set start/end indexes for the first and second axis
c
      if (isvin .eq. itvin+1) go to 370
      if (IRTYPE(itvin) .eq. 2 .or. itvin .eq. IRTNUM) go to 370
      do 305 i=itvin+1,isvin-1
          if (IRTYPE(i) .eq. 2) go to 350
  305 continue
  350 isvs   = i
      itve   = isvs - 1
      go to 400
c
  370 itve   = itvin
      isvs   = itve + 1
c
c...store indexes
c
  400 do 420 i=1,IRTDEF,1
          if (itvin .eq. IRTINC(i)) IRTACT(1) = i
          if (isvin .eq. IRTINC(i)) IRTACT(2) = i
  420 continue
      if (IRTACT(1) .eq. 0) IRTACT(1) = IRTDEF + 1
      if (IRTACT(2) .eq. 0) IRTACT(2) = IRTACT(1) + 1
      IRTPNT(1) = itve
      IRTPNT(2) = isvs
c
c...get spindle plane definition, starting angle
c...and angles used to rotate plane
c
      call getspl (SPLVEC,SPLDIS,GAMMA(2),SADAN,spinad)
c
c...get the first rotary axis plane, zero position angle
c...and angles used to rotate plane
c
      call gettpl (TPLVEC,TPLANG,TADAN)
c
c...check if rotary planes are parallel
c
      rnum   = dabs (TPLVEC(1) * SPLVEC(1) + TPLVEC(2) * SPLVEC(2) +
     -               TPLVEC(3) * SPLVEC(3))
      if (rnum .gt. 1.d0) rnum = 1.d0
      rnum   = dnint (rnum * 10.0d0**6) / 10.0d0**6
c
c...or spindle vector is || to the spindle rotary axis
c
      snum   = dabs (spinad(1) * SPLVEC(1) + spinad(2) * SPLVEC(2) +
     -               spinad(3) * SPLVEC(3))
      if (snum .gt. 1.d0) snum = 1.d0
      snum   = next * dnint (snum * 10.0d0**6) / 10.0d0**6
      if (rnum .ne. 1.d0 .and. snum .ne. 1.d0) go to 8000
      if (isvin .gt. IRTNUM) go to 900
c
c...lock the second axis and
c...reset all staff
c
      kerr   = 1
      iru(isvin) = 1
      go to 100
c
c...error - single rotary axis is on spindle axis
c
  900 kerr   = kerr + 2
c
c...lock the first axis and
c...forget about rotaries
c
c      IRDEAD(itvin) = 1
      itvin  = 0
c      IRTACT(1) = itvin
      do 910 i=itve,itvin+1,-1
          devt   = ROTSTO(i,1)
          call vecadj (spinad,spinad,devt,IRTWRK(i))
  910 continue
c
      SPIVEC(1) = spinad(1)
      SPIVEC(2) = spinad(2)
      SPIVEC(3) = spinad(3)
 8000 if (MACHTP .eq. 3) then
         pl(1) = SPIVEC(1)
         pl(2) = SPIVEC(2)
         pl(3) = SPIVEC(3)
         pl(4) = 0.0
         call plnint (BLDVEC,SPIVEC,pl,spinad,i)
         call vecang (spinad,3,BLDSTO)
         rnum   = dsqrt (ndot(spinad,spinad))
         if (rnum .gt. 0.d0) then
             BLDVEC(1) = spinad(1) / rnum
             BLDVEC(2) = spinad(2) / rnum
             BLDVEC(3) = spinad(3) / rnum
         end if
         ISIDBL = 1
         IRTOUT(4) = 1
         MOTBLD = 1
      end if
      return
      end
c
c*******************************************************************
c
c   SUBROUTINE:  rscini
c
c   FUNCTION:  This routine redefines the rotary axes arrays when
c              any of the rotary axes do not rotate about a major
c              axis.  It does this by increasing the number of
c              defined rotary axes inserting the offset rotaries
c              into the arrays.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c********************************************************************
c
      subroutine rscini
c
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (LASTAB,KPOSMP(1260)), (ISCWRK,KPOSMP(1453))
      equivalence (IRTINC,KPOSMP(1461)), (IRDEAD,KPOSMP(1465))
      equivalence (IRTDEF,KPOSMP(1485))
      equivalence (IRTYPE,KPOSMP(1486)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IRTYPE(20),IRDEAD(20),LASTAB,IRTWRK(20),
     1          ISCWRK(2,4),IRTINC(4),IRTDEF
c
      equivalence (AXSSTO,POSMAP(1425)), (SCROT ,POSMAP(2119))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (TABORG,POSMAP(5374))
c
      real*8 ROTANG(20,2),ROTSTO(20,2),SCROT(2,4),TABORG(3,20),
     1       AXSSTO(10)
c
      integer*4 i,inc,irt(20),iry(20),ird(20),ifl
c
      real*8 rsto(20),torg(3,20),tvec(3),rlin(6),sgn
c
c...Define default rotary axes positions
c
      IRTDEF = IRTNUM
      IRTINC(1) = 1
      IRTINC(2) = 2
      IRTINC(3) = 3
      IRTINC(4) = 4
c
c...Determine if a rotary axis moves about
c...any other axis besides a major axis
c
      ifl    = 0
      do 100 i=1,IRTNUM,1
          if (ISCWRK(1,i) .ne. 0) ifl = 1
  100 continue
      if (ifl .eq. 0) go to 8000
c
c...A rotary axis does not rotate about a major axis
c...Adjust all rotary axis arrays to accomodate this
c
      inc    = 0
      call axsadr (AXSSTO,rlin,ROTSTO,tvec)
      do 200 i=1,IRTNUM,1
          sgn = -1.
          if (IRTYPE(i) .eq. 2) sgn = 1.
c
c......Define complementary rotations
c......prior to defined axis
c
          if (ISCWRK(1,i) .ne. 0) then
              if (ISCWRK(2,i) .ne. 0) then
cc     1            (inc .ne. 88 .and. ISCWRK(2,i) .ne. irt(inc))) then
                  inc    = inc    + 1
                  irt(inc) = ISCWRK(2,i)
                  iry(inc) = IRTYPE(i)
                  ird(inc) = -1
                  rsto(inc) = SCROT(2,i) * sgn
                  if (IRTYPE(i) .eq. 2) then
                      call copynv (0.,torg(1,inc),3)
                  else
                      call copyn (TABORG(1,i),torg(1,inc),3)
                  endif
              endif
              inc    = inc    + 1
              irt(inc) = ISCWRK(1,i)
              iry(inc) = IRTYPE(i)
              ird(inc) = -1
              rsto(inc) = SCROT(1,i) * sgn
              if (IRTYPE(i) .eq. 2) then
                  call copynv (0.,torg(1,inc),3)
              else
                  call copyn (TABORG(1,i),torg(1,inc),3)
              endif
          endif
c
c......Store defined user axis
c
          inc    = inc    + 1
          irt(inc) = IRTWRK(i)
          iry(inc) = IRTYPE(i)
          ird(inc) = IRDEAD(i)
          rsto(inc) = ROTSTO(i,2)
          IRTINC(i) = inc
          if (IRTYPE(i) .eq. 2 .and. ISCWRK(1,i) .ne. 0) then
              call copynv (0.,torg(1,inc),3)
          else
              call copyn (TABORG(1,i),torg(1,inc),3)
          endif
c
c......Define adjustment rotations
c......after defined axis
c
          if (ISCWRK(1,i) .ne. 0) then
              inc    = inc    + 1
              irt(inc) = ISCWRK(1,i)
              iry(inc) = IRTYPE(i)
              ird(inc) = -1
              rsto(inc) = -SCROT(1,i) * sgn
              if (IRTYPE(i) .eq. 2 .and. ISCWRK(2,i) .ne. 0) then
                  call copynv (0.,torg(1,inc),3)
              else
                  call copyn (TABORG(1,i),torg(1,inc),3)
              endif
              if (ISCWRK(2,i) .ne. 0) then
                  inc    = inc    + 1
                  irt(inc) = ISCWRK(2,i)
                  iry(inc) = IRTYPE(i)
                  ird(inc) = -1
                  rsto(inc) = -SCROT(2,i) * sgn
                  call copyn (TABORG(1,i),torg(1,inc),3)
              endif
          endif
          if (IRTYPE(i) .eq. 1) LASTAB = inc
  200 continue
c
c...Redefine global arrays
c
      IRTNUM = inc
      do 300 i=1,IRTNUM,1
          IRTWRK(i) = irt(i)
          IRTYPE(i) = iry(i)
          IRDEAD(i) = ird(i)
          ROTSTO(i,1) = rsto(i)
          ROTSTO(i,2) = rsto(i)
          call copyn (torg(1,i),TABORG(1,i),3)
  300 continue
      call axsadj (rlin,ROTSTO(1,2),AXSSTO)
c
      inc    = 1
      do 400 i=IRTDEF+1,4,1
          IRTINC(i) = IRTNUM + inc
          inc    = inc    + 1
  400 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getijk (gang,gvec)
c
c   FUNCTION:  This routine defines tool vector coordinates using the
c              rotary axis position.
c
c   INPUT:  gang    R*8  D20 -  rotary axis position
c
c   OUTPUT: gvec    R*8  D3  -  tool vector coordinates
c
c***********************************************************************
c
      subroutine getijk (gang,gvec)
c
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (IRTNUM,KPOSMP(1243))
      equivalence (NCONTB,KPOSMP(1347)), (IRTACT,KPOSMP(1256))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IRTYPE(20),IRTWRK(20),IRTACT(2),NCONTB,
     -          IRDEAD(20),MACHTP
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
      equivalence (SPIVEC,POSMAP(3583))
c
      real*8 PI,RAD,SPIVEC(3)
c
      integer*4 i
c
      real*8 gang(20),gvec(3)
c
c...setup tool vector along spindle axis
c
      if (MACHTP .eq. 2) then
          gvec(1) = 0.
          gvec(2) = 1.
          gvec(3) = 0.
      else
          gvec(1) = SPIVEC(1)
          gvec(2) = SPIVEC(2)
          gvec(3) = SPIVEC(3)
      endif
      if (IRTACT(1) .eq. 0) go to 8000
c
c...rotate in reversed order than tool vector
c
      do 305 i=IRTNUM,1,-1
          if (NCONTB .eq. 2) then
              if (IRTYPE(i) .eq. 1 .and.
     -            IRDEAD(i) .ne. 0) go to 305
          end if
          if (gang(i) .ne. 0.d0)
     -        call vecadj (gvec,gvec,gang(i),IRTWRK(i))
  305 continue
 8000 return
      end
c
c********************************************************************
c
c   SUBROUTINE:  ptrtad (gcli,gopt,gang,krev)
c
c   FUNCTION:  This routine converts a cl/machine point coordinates
c              to the machine/cl coordinates for rotary axis position
c              and tool length.
c
c   INPUT:  gcli    R*8  D3  -  The current point.
c           gang    R*8  D20 -  The angular position.
c           krev    I*4  D1  -  direction of convertion:
c                               0 - cl to machine
c                               1 - machine to cl
c
c   OUTPUT: gopt    R*8  D3  -  The adjusted point.
c
c***************************************************************
c
      subroutine ptrtad (gcli,gopt,gang,krev)
c
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (IFHEAD,KPOSMP(1281))
      equivalence (NOPIVS,KPOSMP(1282)), (NOTABS,KPOSMP(1283))
      equivalence (ITP   ,KPOSMP(1801)), (NOTOOL,KPOSMP(1802))
      equivalence (IRTACT,KPOSMP(1256)), (IZSWIV,KPOSMP(1224))
      equivalence (IZIGAD,KPOSMP(0371)), (IJKROT,KPOSMP(1739))
c
      integer*4 IRTNUM,IFHEAD,NOPIVS,NOTABS,ITP,NOTOOL,IZSWIV,
     -          IZIGAD,IRTACT(2),IJKROT
c
      equivalence (TL    ,POSMAP(3601)), (SPIVEC,POSMAP(3583))
      equivalence (ZIGDEP,POSMAP(0190)), (IJKVC ,POSMAP(0329))
c
      real*8 TL(120),SPIVEC(3),ZIGDEP,IJKVC(3)
c
      integer*4 krev
c
      real*8 gcli(3),gopt(3),gang(20)
c
      integer*4 iflt
c
      real*8 rnum(3),rd,t
c
c...see if point has to be adjusted
c
      if (IRTNUM .eq. 0 .or. IRTACT(1) .eq. 0 .or.
     -   (NOTABS .eq. 2 .and. IFHEAD .eq. 2)) then
c
c...do not adjust for rotary axis
c...adjust for tool length if NOTOOL = 1
c
          iflt   = 2 - NOTOOL
          if (krev .eq. 1) iflt = 0 - iflt
          rd     = 0.d0
          if (IZIGAD .eq. 1) rd = (2*krev - 1) * ZIGDEP
          t      = iflt * TL(ITP) + rd
c
c...vp 5/5/98 adjust for tool axis vector directly
c...if no rotary axes supported but IJK tool vector
c...is used
c
          if (IJKROT .eq. 1) then
             call vcplvc (gcli,IJKVC,gopt,t)
          else
             call vcplvc (gcli,SPIVEC,gopt,t)
          end if
ccc       gopt(1) = gcli(1) + t * SPIVEC(1)
ccc       gopt(2) = gcli(2) + t * SPIVEC(2)
ccc       gopt(3) = gcli(3) + t * SPIVEC(3)
          go to 8000
      end if
c
c...set point
c
      rnum(1) = gcli(1)
      rnum(2) = gcli(2)
      rnum(3) = gcli(3)
c
c...adjust point first for tables then for heads
c...when defining machine coordinates or vice versa
c...when defining cl coordinates
c
      if (krev .eq. 0) then
          call tabadj (rnum,rnum,gang,krev)
          if (IFHEAD .eq. 1) call pivadj (rnum,rnum,gang,krev)
ccc          if (IZSWIV .eq. 1) call ptswad (rnum,rnum,gang,krev)
      else
ccc          if (IZSWIV .eq. 1) call ptswad (rnum,rnum,gang,krev)
          if (IFHEAD .eq. 1) call pivadj (rnum,rnum,gang,krev)
          call tabadj (rnum,rnum,gang,krev)
      end if
c
c...set point
c
      gopt(1) = rnum(1)
      gopt(2) = rnum(2)
      gopt(3) = rnum(3)
 8000 return
      end
c
c********************************************************************
c
c   SUBROUTINE:  ptswad (gcli,gopt,gang,krev)
c
c   FUNCTION:  This routine converts machine point coordinates to the
c              swivel machine coordinates for rotary axis position
c              and tool length.
c
c   INPUT:  gcli    R*8  D3  -  The current point.
c           gang    R*8  D20 -  The angular position.
c           krev    I*4  D1  -  direction of convertion:
c                               0 - cl to machine
c                               1 - machine to cl
c
c   OUTPUT: gopt    R*8  D3  -  The adjusted point.  If spindle vector
c                               is horizontal then gopt(3) = gcli(3)
c
c***************************************************************
c
      subroutine ptswad (gcli,gopt,gang,krev)
c
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (IFHEAD,KPOSMP(1281))
c
      integer*4 IRTNUM,IFHEAD
c
      integer*4 krev
c
      real*8 gcli(3),gopt(3),gang(20)
c
      integer*4 isig
c
      real*8 dist,tv(3)
c
c...see if point has to be adjusted
c
      if (IRTNUM .eq. 0 .or. IFHEAD .ne. 1) return
c
c...get spindle vector
c
      call pivvec (gang,tv)
c
      isig   = 1 - krev - krev
c
c...get z-axis
c
      if (isig .eq. 1) then
          gopt(3) = gcli(3)
          if (dabs(tv(3)) .gt. 1.d-8) gopt(3) = gcli(3) / tv(3)
          dist   = gopt(3)
      else
          dist   = gcli(3)
          if (dabs(tv(3)) .gt. 1.d-8) gopt(3) = gcli(3) * tv(3)
      end if
c
c...adjust x,y coordinates
c
      gopt(1) = gcli(1) - isig * dist * tv(1)
      gopt(2) = gcli(2) - isig * dist * tv(2)
      return
      end
c
c********************************************************************
c
c   SUBROUTINE:  tabadj (gcli,gopt,gang,krev)
c
c   FUNCTION:  This routine converts a cl/machine point coordinates to
c              the machine/cl coordinates for rotary tables position
c              and tool length.
c
c   INPUT:  gcli    R*8  D3  -  The current point.
c           gang    R*8  D20 -  The angular position.
c           krev    I*4  D1  -  direction of convertion:
c                               0 - cl to machine
c                               1 - machine to cl
c
c   OUTPUT: gopt    R*8  D3  -  The adjusted point.
c
c***************************************************************
c
      subroutine tabadj (gcli,gopt,gang,krev)
c
      include 'post.inc'
c
      equivalence (IZIGAD,KPOSMP(0371)), (LASTAB,KPOSMP(1260))
      equivalence (IFHEAD,KPOSMP(1281)), (NOTABS,KPOSMP(1283))
      equivalence (NCONTB,KPOSMP(1347)), (IRDEAD,KPOSMP(1465))
      equivalence (IRTYPE,KPOSMP(1486)), (IRTWRK,KPOSMP(1506))
      equivalence (ITP   ,KPOSMP(1801)), (NOTOOL,KPOSMP(1802))
c
      integer*4 IRTYPE(20),IRTWRK(20),LASTAB,IFHEAD,NOTABS,ITP,
     1          IZIGAD,NOTOOL,NCONTB,IRDEAD(20)
c
      equivalence (ZIGDEP,POSMAP(0190)), (TL    ,POSMAP(3601))
      equivalence (SPIVEC,POSMAP(3583)), (TABORG,POSMAP(5374))
c
      real*8 TABORG(3,20),TL(120),SPIVEC(3),ZIGDEP
c
      integer*4 krev
c
      real*8 gcli(3),gopt(3),gang(20),rd,anl
c
      integer*4 i,j,iflt,ifzg
c
      gopt(1) = gcli(1)
      gopt(2) = gcli(2)
      gopt(3) = gcli(3)
c
c...do not adjust for tables rotations
c
      if (NOTABS .eq. 2) return
c
c...initialize offset
c
      ifzg = 0
      if (IFHEAD .eq. 1) then
          iflt = 0
      else
          iflt = 2 - NOTOOL
          if (IZIGAD .eq. 1) ifzg = 1
      end if
c
c...adjust for tool length
c
      rd     = iflt * TL(ITP) - ifzg * ZIGDEP
      if (krev .eq. 1) then
          gopt(1) = gcli(1) - rd * SPIVEC(1)
          gopt(2) = gcli(2) - rd * SPIVEC(2)
          gopt(3) = gcli(3) - rd * SPIVEC(3)
      end if
c
c...adjust point for all tables from rider to carrier
c...when defining machine point or from carrier to rider
c...when defining cl point
c
      do 325 j=1,LASTAB
         if (krev .eq. 1) then
             i = LASTAB + 1 - j
             anl = gang(i)
         else
             i = j
             anl = 360.d0 - gang(i)
         end if
c
c...adjust point for table rotation
c
         if (NCONTB .eq. 2 .and. IRDEAD(i) .eq. 1) go to 325
         call axadj (gopt,gopt,anl,TABORG(1,i),IRTWRK(i))
  325 continue
c
c...add tool length if defining machine point
c
      if (krev .eq. 0) then
          gopt(1) = gopt(1) + rd * SPIVEC(1)
          gopt(2) = gopt(2) + rd * SPIVEC(2)
          gopt(3) = gopt(3) + rd * SPIVEC(3)
      end if
c
      return
      end
c
c********************************************************************
c
c   SUBROUTINE:  pivadj (gcli,gopt,gang,krev)
c
c   FUNCTION:  This routine converts a cl/machine point coordinates to
c              the machine/cl coordinates for rotary heads position
c              and tool length.
c
c   INPUT:  gcli    R*8  D3  -  The current point.
c           gang    R*8  D20 -  The angular position.
c           krev    I*4  D1  -  direction of convertion:
c                               0 - cl to machine
c                               1 - machine to cl
c
c   OUTPUT: gopt    R*8  D3  -  The adjusted point.
c
c***************************************************************
c
      subroutine pivadj (gcli,gopt,gang,krev)
c
      real*8 gcli(3),gopt(3),gang(20)
c
      integer*4 krev
c
      include 'post.inc'
c
      equivalence (IZIGAD,KPOSMP(0371)), (IZSWIV,KPOSMP(1224))
      equivalence (IRTNUM,KPOSMP(1243)), (NOPIVS,KPOSMP(1282))
      equivalence (IRTYPE,KPOSMP(1486))
      equivalence (IRTDEF,KPOSMP(1485)), (IRTWRK,KPOSMP(1506))
      equivalence (ITP   ,KPOSMP(1801)), (NOTOOL,KPOSMP(1802))
c
      integer*4 IRTYPE(20),IRTWRK(20),NOPIVS,ITP,NOTOOL,IZIGAD,
     1          IZSWIV,IRTDEF,IRTNUM
c
      equivalence (ZIGDEP,POSMAP(0190)), (TL    ,POSMAP(3601))
      equivalence (SPIVEC,POSMAP(3583)), (TABORG,POSMAP(5374))
c
      real*8 ZIGDEP,TABORG(3,20),TL(120),SPIVEC(3)
c
      real*8 pivoff(3),t,rd
c
      integer*4 isig,iflt,iflp,i
c
c...do not adjust for pivot distance nor tool length
c...if flags are set
c
      if (NOPIVS .eq. 1 .or. NOTOOL .eq. 1) go to 100
c
      gopt(1) = gcli(1)
      gopt(2) = gcli(2)
      gopt(3) = gcli(3)
      go to 8000
c
c...initialize offset
c
  100 isig   = 1 - krev - krev
      iflt   = 2 - NOTOOL
      iflp   = 2 - NOPIVS
      pivoff(1) = 0.d0
      pivoff(2) = 0.d0
      pivoff(3) = 0.d0
      rd     = 0.d0
      if (IZIGAD .eq. 1) rd = ZIGDEP
      t      = iflt * TL(ITP) - rd
c
c...Adjust point for swivel Z-axis
c
      if (krev .eq. 1 .and. IZSWIV .eq. 1)
     1        call ptswad (gcli,gopt,gang,krev)
c
c...adjust offset for all heads from rider to carrier
c
      do 325 i=IRTNUM,1,-1
         if (IRTYPE(i) .eq. 2) then
            pivoff(1) = pivoff(1) + isig * (iflp * TABORG(1,i) +
     -                  t * SPIVEC(1))
            pivoff(2) = pivoff(2) + isig * (iflp * TABORG(2,i) +
     -                  t * SPIVEC(2))
            pivoff(3) = pivoff(3) + isig * (iflp * TABORG(3,i) +
     -                  t * SPIVEC(3))
c
c...adjust offset vector for head rotation
c
            call vecadj (pivoff,pivoff,gang(i),irtwrk(i))
c
c...turn off tool length offset flag after it was used
c...with the lowest rider
c
            t      = 0.d0
         end if
  325 continue
c
c...get point
c
      gopt(1) = gcli(1) + pivoff(1)
      gopt(2) = gcli(2) + pivoff(2)
      gopt(3) = gcli(3) + pivoff(3)
c
c...Adjust point for swivel Z-axis
c
      if (krev .eq. 0 .and. IZSWIV .eq. 1)
     1        call ptswad (gopt,gopt,gang,krev)
 8000 return
      end
c
c********************************************************************
c
c   SUBROUTINE:  pivvec (gang,gvec)
c
c   FUNCTION:  This routine returns the head (pivot) vector for
c              the current rotary axis position.
c
c   INPUT:  gang    R*8  D20 -  The angular position.
c
c   OUTPUT: gvec    R*8  D3  -  The adjusted point.  If spindle vector
c                               is horizontal then gopt(3) = gcli(3)
c
c***************************************************************
c
      subroutine pivvec (gang,gvec)
c
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (IFHEAD,KPOSMP(1281))
      equivalence (IRTYPE,KPOSMP(1486)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IFHEAD,IRTYPE(20),IRTWRK(20)
c
      equivalence (SPIVEC,POSMAP(3583))
c
      real*8 SPIVEC(3)
c
      real*8 gvec(3),gang(20)
c
      integer*4 i
c
c...get spindle vector
c
      gvec(1)  = SPIVEC(1)
      gvec(2)  = SPIVEC(2)
      gvec(3)  = SPIVEC(3)
      do 105 i=IRTNUM,1,-1
            if (IRTYPE(i) .eq. 2)
     -          call vecadj (gvec,gvec,gang(i),IRTWRK(i))
  105 continue
      call dpoint (gvec(1),gvec(1),8)
      call dpoint (gvec(2),gvec(2),8)
      call dpoint (gvec(3),gvec(3),8)
      return
      end
