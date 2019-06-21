c***********************************************************************
c
c   FILE NAME:  tlperp
c   CONTAINS:
c               perpto  ptalim  limang  matrot  gtmatr  matmul  limlin
c               scnlim
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        tlperp.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:13
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  perpto (kflg)
c
c   FUNCTION:  This routine checks if tool vector is perpendicular to
c              rotary table if any exists.
c
c   INPUT:  none.
c
c   OUTPUT: kflg    I*4  D1    -  Rotary axis number (table) or 0 if
c                                 if TV is not perpto to any table.
c
c***********************************************************************
c
      subroutine perpto (kflg)
c
      include 'post.inc'
c
      integer*4 kflg
c
      equivalence (MACHTP,KPOSMP(1201)), (ITP   ,KPOSMP(1801))
      equivalence (LASTAB,KPOSMP(1260)), (NRLTAB,KPOSMP(0111))
      equivalence (KERRSV,KPOSMP(0109)), (MODROT,KPOSMP(4031))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTWRK,KPOSMP(1506))
      equivalence (MODRAT,KPOSMP(4033)), (LTMODE,KPOSMP(4125))
c
      integer*4 IRTWRK(20),LASTAB,IRDEAD(20),
     -          MODROT,NRLTAB,KERRSV,LTMODE,MACHTP,ITP,MODRAT
c
      equivalence (SPIVEC,POSMAP(3583)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (MATRXT,POSMAP(2482))
      equivalence (LNRTOL,POSMAP(2251)), (TL    ,POSMAP(3601))
      equivalence (ROTSTO,POSMAP(5213)), (TABORG,POSMAP(5374))
c
      real*8 TLVEC(3),SPIVEC(3),ROTSTO(20,2),VECSAV(3),
     -       MATRXT(4,3),TABORG(3,20),LNRTOL(3),TL(120)
c
      integer*4 i,j,irot
c
      real*8 rnum,spv(3),vec(3,20),ang
c
      kflg   = 0
      if (LASTAB .eq. 0 .or. MACHTP .eq. 2 .or. MACHTP .eq. 4 .and.
     -    LTMODE .ne. 3) go to 8000
      if (LNRTOL(1) .ne. 0.0 .and. MODROT .eq. 0) go to 8000
      if (KERRSV .eq. 7) then
          kflg = NRLTAB
          go to 8000
      end if
      rnum = VECSAV(1)*TLVEC(1) + VECSAV(2)*TLVEC(2) +
     1       VECSAV(3)*TLVEC(3)
      if (dabs(1.0-rnum) .gt. 1.0d-7) go to 8000

cc      rnum   = dsqrt ((VECSAV(1)-TLVEC(1))**2 +
cc     -                (VECSAV(2)-TLVEC(2))**2 +
cc     -                (VECSAV(3)-TLVEC(3))**2)
cc      if (rnum .gt. 1.0d-7) go to 8000
c
c...Get curent spindle (tool) vector
c
      call pivvec (ROTSTO,spv)
c
c...Loop thru all tables
c
      do 200 j=1,LASTAB,1
c
c...Setup rider table vector
c
          vec(1,j) = 0.d0
          vec(2,j) = 0.d0
          vec(3,j) = 0.d0
          irot   = IRTWRK(j)
          vec(irot,j) = 1.d0
c
c...adjust it to the curent carier position if any
c
          do 100 i=j+1,LASTAB
              ang = 360. - ROTSTO(i,1)
              call vecadj (vec(1,j),vec(1,j),ang,IRTWRK(i))
  100     continue
c
c...See if tool vector is perpto to any unlocked table
c
          rnum   = vec(1,j)*spv(1) + vec(2,j)*spv(2) + vec(3,j)*spv(3)
          if (dabs(rnum) .gt. 1.d0) rnum = 1.d0
          call dpoint (rnum,rnum,6)
          if (dabs(rnum) .eq. 1.d0 .and. IRDEAD(j) .eq. 0) go to 600
  200 continue
      go to 8000
c
c...Set flag to point table perpto spindle axis
c
  600 kflg   = j
      if (MODRAT .ne. 0) MODROT = kflg
c
c...get conversion matrix for tables
c...adjust translation vector for pivot offset if any
c
      call matrot (j,LASTAB,IRTWRK,ROTSTO,TABORG,MATRXT)
      spv(1) = MATRXT(4,1)
      spv(2) = MATRXT(4,2)
      spv(3) = MATRXT(4,3)
      call pivadj (spv,spv,ROTSTO,0)
c
c...update matrix by total offset vector
c
      MATRXT(4,1) = spv(1)
      MATRXT(4,2) = spv(2)
      MATRXT(4,3) = spv(3)
c
 8000 return
      end
c***********************************************************************
c
c   SUBROUTINE:  perptp (gabc,gsto,kflg)
c
c   FUNCTION:  This routine checks if tool vector is perpendicular to
c              rotary table if any exists.
c
c   INPUT:  none.
c
c   OUTPUT: kflg    I*4  D1    -  Rotary axis number (table) or 0 if
c                                 if TV is not perpto to any table.
c
c***********************************************************************
c
      subroutine perptp (gabc,gsto,kflg)
c
      include 'post.inc'
c
      integer*4 kflg
      real*8 gabc(20),gsto(20)
c
      equivalence (MACHTP,KPOSMP(1201)), (ITP   ,KPOSMP(1801))
      equivalence (LASTAB,KPOSMP(1260))
      equivalence (NRLTAB,KPOSMP(0111))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTWRK,KPOSMP(1506))
      equivalence (KERRSV,KPOSMP(0109)), (MODROT,KPOSMP(4031))
      equivalence (MODRAT,KPOSMP(4033)), (LTMODE,KPOSMP(4125))
c
      integer*4 IRTWRK(20),LASTAB,IRDEAD(20),
     -          MODROT,NRLTAB,KERRSV,LTMODE,MACHTP,ITP,MODRAT
c
      equivalence (SPIVEC,POSMAP(3583)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (MATRXT,POSMAP(2482))
      equivalence (LNRTOL,POSMAP(2251)), (TL    ,POSMAP(3601))
      equivalence (ROTSTO,POSMAP(5213)), (TABORG,POSMAP(5374))
c
      real*8 TLVEC(3),SPIVEC(3),ROTSTO(20,2),VECSAV(3),
     -       MATRXT(4,3),TABORG(3,20),LNRTOL(3),TL(120)
c
      integer*4 i,j,irot
c
      real*8 rnum,spv(3),vec(3,20),ang
c
      kflg   = 0
      if (LASTAB .eq. 0 .or. MACHTP .eq. 2 .or. MACHTP .eq. 4 .and.
     -    LTMODE .ne. 3) go to 8000
      if (LNRTOL(1) .ne. 0.0 .and. MODROT .eq. 0) go to 8000
      if (KERRSV .eq. 7) then
          kflg = NRLTAB
          go to 8000
      end if
c
c...Get curent spindle (tool) vector
c
      call pivvec (gabc,spv)
c
c...Loop thru all tables
c
      do 200 j=1,LASTAB,1
          if (IRDEAD(j) .ne. 0) go to 200
c
c...Setup rider table vector
c
          vec(1,j) = 0.d0
          vec(2,j) = 0.d0
          vec(3,j) = 0.d0
          irot   = IRTWRK(j)
          vec(irot,j) = 1.d0
c
c...adjust it to the curent carier position if any
c
          do 100 i=j+1,LASTAB
              ang = 360. - gabc(i)
              call vecadj (vec(1,j),vec(1,j),ang,IRTWRK(i))
  100     continue
c
c...See if tool vector is perpto to any unlocked table
c
          call betvec (spv,vec,rnum)
          call dpoint (rnum,rnum,3)
          if (rnum .lt. .0005 .or. rnum .gt. 179.9995) go to 600
  200 continue
      go to 8000
c
c...Set flag to point table perpto spindle axis
c
  600 rnum   = gsto(j) - gabc(j)
      call dpoint (rnum,rnum,3)
      if (rnum .gt. 0.0) go to 8000
      kflg   = j
      if (MODRAT .ne. 0) MODROT = kflg
c
c...get conversion matrix for tables
c...adjust translation vector for pivot offset if any
c
      call matrot (j,LASTAB,IRTWRK,gabc,TABORG,MATRXT)
      spv(1) = MATRXT(4,1)
      spv(2) = MATRXT(4,2)
      spv(3) = MATRXT(4,3)
      call pivadj (spv,spv,gabc,0)
c
c...update matrix by total offset vector
c
      MATRXT(4,1) = spv(1)
      MATRXT(4,2) = spv(2)
      MATRXT(4,3) = spv(3)
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tvprtb (kflg)
c
c   FUNCTION:  This routine checks if tool vector is perpendicular to
c              any rotary table if exists.
c
c   INPUT:  none.
c
c   OUTPUT: kflg    I*4  D1    -  Rotary axis number (table) or 0 if
c                                 if TV is not perpto to any table.
c
c***********************************************************************
c
      subroutine tvprtb (gang,kflg)
c
      include 'post.inc'
c
      real*8 gang(4)
      integer*4 kflg
c
      equivalence (LASTAB,KPOSMP(1260))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IRTWRK(20),LASTAB,IRDEAD(20),IRTACT(2),IRTINC(4)
c
      equivalence (SPIVEC,POSMAP(3583)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (MATRXT,POSMAP(2482))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 TLVEC(3),SPIVEC(3),ROTSTO(20,2),VECSAV(3),MATRXT(4,3)
c
      integer*4 j,ir1,ir2,ip1
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
      real*8 rnum,vec(3),ang
c
      kflg   = 0
      ip1    = IRTINC(ir1)
c
c...Get curent first rotary axis vector
c
      vec(1) = 0.d0
      vec(2) = 0.d0
      vec(3) = 0.d0
      vec(IRTWRK(ip1)) = 1.d0
c
c...Loop thru all rotary axes adjusting vector
c...excluding first rotary axis (TB rider/HD carrier)
c
      do 200 j=ip1+1,IRTNUM,1
          call vecadj (vec,vec,gang(j),IRTWRK(j))
  200 continue
c
c...See if tool vector is perpto to second axis
c
      call betvec (vec,SPIVEC,ang)
      rnum = 180.0 - ang
      if (rnum .lt. .0005) ang = rnum
      if (ang .lt. .0005) kflg = ir1
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptalim (krot,kaxs,gaxs,gang,gpti,kerr)
c
c   FUNCTION:  This routine checks if rotation of the table can bring
c              destination point in machine limits.  If so then the
c              intermediate point on axis limit and the best table
c              position for this point are calculated.
c
c   INPUT:  krot    I*4  D1    -  Table axis number.
c
c           kaxs    I*4  D10   -  Flag for each axis that is out of
c                                 limits.
c
c           gaxs    R*8  D10   -  Primary and secondary machine axes.
c
c   OUTPUT: gang    R*8  D2    -  Rotary table angles for point
c                                 laying on linear axis limit.
c
c           gpti    R*8  D3    -  Point on linear axis limit.
c
c           kerr    I84  D1    -  Returns 1 if destination point can not
c                                 fit in limits at any table position.
c
c***********************************************************************
c
      subroutine ptalim (krot,kaxs,gaxs,gang,gpti,kerr)
c
      integer*4 kaxs(10),krot,kerr
      real*8 gaxs(10),gang(2),gpti(3)
c
      include 'post.inc'
c
      equivalence (LASTAB,KPOSMP(1260)), (NRLTAB,KPOSMP(0111))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTWRK(20),LASTAB,IRDEAD(20),NRLTAB
c
      equivalence (AXSSTO,POSMAP(1425)), (LIMITS,POSMAP(1254))
      equivalence (MATRXT,POSMAP(2482)), (ROTSTO,POSMAP(5213))
      equivalence (TABORG,POSMAP(5374))
c
      real*8 AXSSTO(10),TABORG(3,20),LIMITS(2,10),
     1       ROTSTO(20,2),MATRXT(4,3)
c
      integer*4 nx,i,ir,ier,lnx(5),lmx(3),lx,nud,jindex,nxsl
c
      real*8 ndot,org(3),dlt(3),dd,rlmn(3),ddl,ang,pt2(3),
     1       ang1,fula,ptj(3),do,rrl(6),plm(4),pt1(3),wl(3),rout(10),
     2       rsto(10)
c
      data lnx /1,0,2,0,3/, lmx /1,3,5/
      data fula /360.0/
c
      kerr   = 0
c
c...Work with non-transformed locations
c
      call axsxfr (gaxs,rout)
      call axsxfr (AXSSTO,rsto)
c
c...Get table origin (rotation center)
c
      call copyn (TABORG(1,krot),org,3)
      ir     = IRTWRK(krot)
      pt2(1) = rout(lmx(1))
      pt2(2) = rout(lmx(2))
      pt2(3) = rout(lmx(3))
      pt1(1) = rsto(lmx(1))
      pt1(2) = rsto(lmx(2))
      pt1(3) = rsto(lmx(3))
c
c...Get delta from old position
c
      call vcplvc (pt2,pt1,dlt,-1.d0)
      ddl  = dsqrt (dlt(1)**2 + dlt(2)**2 + dlt(3)**2)
      if (ddl .lt. 1.0d-8) go to 9000
c
c...Get line (vector) from old to new position
c
      rlmn(1) = dlt(1) / ddl
      rlmn(2) = dlt(2) / ddl
      rlmn(3) = dlt(3) / ddl
      do      = 0.0
c
c...Get the closest limit violation
c
      i      = 0
      nxsl   = 0
  100 i      = i + 1
      if (i .gt. 6) go to 200
      if (kaxs(i) .eq. 0) go to 100
      nx     = i
      nud    = kaxs(i)
      if (jindex(lmx,nx,3) .eq. 0) go to 8000
      lx     = lnx(nx)
      if (rlmn(lx) .eq. 0.0) go to 100
      dd   = (rout(nx) - LIMITS(nud,nx)) / dlt(lx)
      if (dd .gt. do) then
         do = dd
         nxsl = nx
      end if
      go to 100
c
c...Get point on limit in machine coordinates
c
  200 if (nxsl .eq. 0) go to 8000
      plm(1) = 0.
      plm(2) = 0.
      plm(3) = 0.
      wl(1) = 0.
      wl(2) = 0.
      wl(3) = 0.
      if (kaxs(nxsl) .eq. 2) then
          plm(lnx(nxsl)) = 1.0
      else
          plm(lnx(nxsl)) = -1.0
      end if
      wl(lnx(nxsl)) = LIMITS(kaxs(nxsl),nxsl)
      call ptmatr (plm,plm,MATRXT,2)
      call ptmatr (wl,wl,MATRXT,1)
      plm(4) = -ndot (plm,wl)

      gpti(1) = pt2(1) - do * ddl * rlmn(1)
      gpti(2) = pt2(2) - do * ddl * rlmn(2)
      gpti(3) = pt2(3) - do * ddl * rlmn(3)
c
c...Convert machine coordinates to table
c
      call ptmatr (gpti,ptj,MATRXT,1)
      call ptmatr (pt1,pt1,MATRXT,1)
      call ptmatr (pt2,pt2,MATRXT,1)
      call ptmatr (org,org,MATRXT,1)
      org(3) = ptj(3)
      pt1(3) = ptj(3)
      pt2(3) = ptj(3)
c
c...get distance of the cutline from table center
c...in the table plane
c
      call ptmatr (rlmn,rrl(4),MATRXT,0)
      rrl(6) = 0.
      call unitvc (rrl(4),rrl(4))
      call copyn (ptj,rrl(1),3)
      rrl(3) = ptj(3)
c
c...Get table rotation from current pos to get
c...the best point on limit
c
      dd = LIMITS(2,nx) - LIMITS(1,nx)
      call scnlim1 (org,pt1,ptj,pt2,plm,rrl,dd,ang,ier)
      if (ier .eq. 2) call scnlim (org,ptj,ang,ier)
      if (ier .ne. 0) go to 8000
c
c...Get absolute table position for limit point
c
      ang1   = ROTSTO(krot,1) - ang
      if (ang1 .lt. 0.0) ang1 = ang1 + fula
      if (ang1 .ge. fula) ang1 = ang1 - fula
      gang(1) = ang1
      gang(2) = ang1
      NRLTAB = krot
      go to 9000
c
c...Error, destination point is
c...out of limits at any table position
c
 8000 kerr   = 1
 9000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  limang (gmch,krt,kerr)
c
c   FUNCTION:  This routine checks if destination point violates any
c              linear axis limit and it is fixable by table rotation.
c
c
c   INPUT:  gmch    R*8  D3.4  -  Destination point position.
c
c           krt     I*4  D1    -  Rotary table number.
c
c   OUTPUT: kerr    I*4  D1    -  Return 0 if point is in limits or
c                                 7 if point is out of limits.
c
c***********************************************************************
c
      subroutine limang (gmch,krt,kerr)
c
      real*8 gmch(3,4)
      integer*4 kerr,krt
c
      include 'post.inc'
c
      equivalence (LIMERR,KPOSMP(0110)), (IRTDEF,KPOSMP(1485))
c
      integer*4 LIMERR,IRTDEF
c
      equivalence (TBLIML,POSMAP(2480))
      equivalence (PTPOST,POSMAP(2472)), (PTDEST,POSMAP(2477))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2),TBLIML(2),PTPOST(3),PTDEST(3)
c
      real*8 raxs(10),rlin(6),tabl(2),vec(3),mch(3,4),pti(3)
c
      integer*4 i,nout,naxs(10),ierr
c
      kerr   = 0
      LIMERR = 0
c
c...Adjust cl point and check limits
c
      call alladj (gmch,rlin,raxs,ROTSTO,2,5)
      call lmtchk (raxs,nout,naxs,0)
      ierr   = 0
c
c...vp 12-mar-97 do not process limit errors on any rotary axis,
c...not just that table which perp-to tool vector
c      if (nout .eq. 1 .and. naxs(6+krt) .ne. 0) nout = 0
c
      do 45 i=1,IRTDEF
         ierr = ierr + naxs(i+6)
   45 continue
      if (nout .eq. 1 .and. ierr .ne. 0) nout = 0
      if (nout .ne. 0) then
c
c...Limit error, check if table rotation
c...can help
c
          call ptalim (krt,naxs,raxs,tabl,pti,ierr)
          if (ierr .eq. 0) then
              PTDEST(1) = gmch(1,2)
              PTDEST(2) = gmch(2,2)
              PTDEST(3) = gmch(3,2)
              TBLIML(1) = tabl(1)
              TBLIML(2) = tabl(2)
              raxs(1) = pti(1)
              raxs(3) = pti(2)
              raxs(5) = pti(3)
              call alladr (raxs,rlin,mch,ROTSTO,vec,5,2)
              PTPOST(1) = mch(1,2)
              PTPOST(2) = mch(2,2)
              PTPOST(3) = mch(3,2)
              kerr   = 7
              LIMERR = kerr
          else
              call psterr (1,'NOROTB1','NOROTB2',-1)
          end if
      end if
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  matrot (kfir,klast,krtax,grot,gtran,gmat)
c
c   FUNCTION:  This routine defines the conversion matrix from
c              machine coordinates to table plane using rotary tables
c              of the machine.  Conversion for heads shold not be applied
c              in this case and only pivot offset is added to the matrix
c              (see perpto routine).
c
c   INPUT:  kfir    I*4  D1    -  The first rotary table number to start
c                                 conversion.
c
c           klast   I*4  D1    -  The last rotary table number to finish
c                                 conversion.
c
c           krtax   I*4  D4    -  Rotary axes' vectors (IRTWRK).
c
c           grot    R*8  D20.2 -  Rotary axes position.
c
c           gtran   R*8  D3.20 -  Rotary axes origin positions.
c
c   OUTPUT: gmat    R*8  D4.3  -  Conversion matrix (displacement vector
c                                 is in 4,n indexes.
c
c***********************************************************************
c
      subroutine matrot (kfir,klast,krtax,grot,gtran,gmat)
c
      include 'post.inc'
c
      integer*4 kfir,klast,krtax(20)
c
      equivalence (IRTYPE,KPOSMP(1486))
c
      integer*4 IRTYPE(20)
c
      real*8 grot(20),gmat(4,3),gtran(3,20)
c
      integer*4 i,j
c
      real*8 ang,matr2(4,3),matr1(4,3),mat(4,3),tmp(3),
     -       ml1(12),ml2(12),ml(12)
c
      equivalence (matr1,ml1), (matr2,ml2), (mat,ml)
c
c...Initialize dummy unit matrix
c
      do 110 i=1,12
          ml1(i) = 0.0
  110 continue
      do 115 i=1,3
          matr1(i,i) = 1.0
  115 continue
c
c...get matrix of table at its normal position
c
      call gtmatb (krtax(kfir),grot(kfir),gtran(1,kfir),matr2)
      call copyn (matr2,mat,12)
      tmp(1) = matr2(4,1)
      tmp(2) = matr2(4,2)
      tmp(3) = matr2(4,3)
c
c...Get rotation matrix for each rotation
c...and multiply by previous matrix
c
      do 300 i=kfir+1,klast,1
          ang  = grot(i)
          if (IRTYPE(i) .eq. 1) ang = 360. - ang
          call gtmatr (krtax(i),ang,gtran(1,i),matr1)
          call matmul (matr1,matr2,mat)
c
c...Add displacement vector
c...reverse current trans using old matrix
c
          mat(4,1) = tmp(1)
          mat(4,2) = tmp(2)
          mat(4,3) = tmp(3)
          call ptmatb (tmp,tmp,matr1,1)
          do 310 j=1,12
              ml2(j) = ml(j)
  310     continue
  300 continue
c
c...Put in output matrix
c
      call copyn (mat,gmat,12)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtmatr (krtax,grot,gtran,gmat)
c
c   FUNCTION:  This routine defines the conversion matrix for
c              one rotary axis.
c
c   INPUT:  krtax   I*4  D1    -  Linear axis number rotary axis rotates
c                                 about.
c
c           grot    R*8  D1    -  Rotary axis angle.
c
c           gtran   R*8  D3    -  Rotary axis origin.
c
c   OUTPUT: gmat    R*8  D4.3  -  Conversion matrix with displacement
c                                 vector.
c
c***********************************************************************
c
      subroutine gtmatr (krtax,grot,gtran,gmat)
c
      integer*4 krtax
c
      real*8 grot,gmat(4,3),gtran(3)
c
      integer*4 i,j
c
      real*8 vax(3,3),ang
c
      do 110 i=1,3
          vax(i,1) = 0.0
          vax(i,2) = 0.0
          vax(i,3) = 0.0
  110 continue
      vax(1,1) = 1.0
      vax(2,2) = 1.0
      vax(3,3) = 1.0
      ang    = 360.0 - grot
      call vecadj (vax(1,1),vax(1,1),ang,krtax)
      call vecadj (vax(1,2),vax(1,2),ang,krtax)
      call vecadj (vax(1,3),vax(1,3),ang,krtax)
      do 320 j=1,3
          do 310 i=1,3
              gmat(i,j) = vax(i,j)
  310     continue
          gmat(4,j) = gtran(j)
  320 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtmatb (krtax,grot,gtran,gmat)
c
c   FUNCTION:  This routine defines conversion matrix for the table
c              which is perpendicular to the tool axis.
c
c   INPUT:  krtax   I*4  D1    -  Linear axis number rotary table rotates
c                                 about.
c
c           grot    R*8  D1    -  Rotary table current angle.
c
c           gtran   R*8  D3    -  Rotary table origin.
c
c   OUTPUT: gmat    R*8  D4.3  -  Conversion matrix with displacement
c                                 vector.
c
c***********************************************************************
c
      subroutine gtmatb (krtax,grot,gtran,gmat)
c
      integer*4 krtax
c
      real*8 gmat(4,3),gtran(3),grot
c
      integer*4 i,j,nax
c
      real*8 vax(5,3),vec(3)
c
      data vax /1.,0.,0.,1.,0., 0.,1.,0.,0.,1., 0.,0.,1.,0.,0./
c
      nax = krtax
      if (nax .eq. 3) nax = 0
      do 320 j=1,3
          do 310 i=1,3
              gmat(i,j) = vax(nax+i,j)
  310     continue
  320 continue
c
c...rotate matrix about Z axis (table rotation axis)
c
      call vecadj (gmat(1,1),gmat(1,1),grot,3)
      call vecadj (gmat(1,2),gmat(1,2),grot,3)
      call vecadj (gmat(1,3),gmat(1,3),grot,3)
      call ptmatr (gtran,vec,gmat,2)
c
c...add table origin
c
      gmat(4,1) = gtran(1)
      gmat(4,2) = gtran(2)
      gmat(4,3) = gtran(3)
c
      return
      end
c
c
c***********************************************************************
c
c   SUBROUTINE:  matmul (gmat1,gmat2,gmat)
c
c   FUNCTION:  This routine multiplies two matrixes (rows by columns).
c
c   INPUT:  gmat1   R*8  D4.3  -  Input matrix I.
c
c           gmat1   R*8  D4.3  -  Input matrix II.
c
c   OUTPUT: gmat    R*8  D4.3  -  Output matrix = gmat1 X gmat2.
c
c***********************************************************************
c
      subroutine matmul (gmat1,gmat2,gmat)
c
      real*8 gmat(4,3),gmat1(4,3),gmat2(4,3)
c
      integer*4 i,j,k
      real*8 sum
c
      do 150 k=1,3
          do 130 j=1,3
              sum = 0.0
              do 110 i=1,3
                  sum = sum + gmat2(j,i) * gmat1(i,k)
  110         continue
              gmat(j,k) = sum
  130     continue
  150 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  limlin (gplm,gorg,gvlin,gdlin,kerr)
c
c   FUNCTION:  This routine defines the intersection line of two planes
c              where one is limit plane (general definition) and other
c              is table plane (XY plane at z=gorg(3))
c
c   INPUT:  gplm    R*8  D4    -  Limit plane definitin (i,j,k,D).
c
c           gorg    R*8  D3    -  Table center point.
c
c   OUTPUT: gvlin   R*8  D6    -  Intof line definition (4,5,6 - vector
c                                 pp to line, 1,2,3 point on the line
c                                 at the shortest distance from gorg.
c
c           gdlin   R*8  D1    -  Distance from table center to the line.
c
c           kerr    I*4  D1    -  Return 1 if planes are parallel.
c
c***********************************************************************
c
      subroutine limlin (gplm,gorg,gvlin,gdlin,kerr)
c
      integer*4 kerr
      real*8 gplm(4),gorg(3),gvlin(6),gdlin
c
      real*8 vp(3),p,ddl,rnum
c
c...See if limit plane is || to table
c
      kerr   = 1
      ddl    = dsqrt (gplm(1)**2 + gplm(2)**2)
      if (ddl .lt. 1.d-9) go to 8000
      kerr   = 0
c
c...get intersection line direction
c
      gvlin(4) = 0. - gplm(2)/ddl
      gvlin(5) = gplm(1)/ddl
      gvlin(6) = 0.0
c
c...Get the closest point on the line
c...from the table center
c
      vp(1)  = gplm(1) / ddl
      vp(2)  = gplm(2) / ddl
      p      = gorg(3)
      ddl    = vp(1)*gplm(1) + vp(2)*gplm(2)
c
      rnum   = (gplm(1)*gorg(1) + gplm(2)*gorg(2) +
     -          gplm(3)*gorg(3) + gplm(4)) / ddl
      gvlin(1) = gorg(1) - vp(1)*rnum
      gvlin(2) = gorg(2) - vp(2)*rnum
      gvlin(3) = p
c
c...Get distance of the line from table center
c
      gdlin  = dsqrt ((gvlin(1) - gorg(1))**2 +
     -                (gvlin(2) - gorg(2))**2)
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  scnlim (gorg,gpti,gang,kerr)
c
c   FUNCTION:  This routine is used to check all intersections off
c              the circle with limit planes of the machine.  The circle
c              is created by point on the first limit plane rotated
c              about the table center.
c
c   INPUT:  gorg    R*8  D3    -  Table center point.
c
c           gpti    R*8  D3    -  Point on limit in table coordinates.
c
c   OUTPUT: gang    R*8  D1    -  Angle from gpti point to the most
c                                 distant point of the circle intersec-
c                                 tion with any other limit plane.
c
c           kerr    I*4  D1    -  Returns 1 if all intersections are
c                                 conflicting.
c
c***********************************************************************
c
      subroutine scnlim (gorg,gpti,gang,kerr)
c
      include 'post.inc'
c
      integer*4 kerr
      real*8 gorg(3),gpti(3),gang
c
      equivalence (LIMITS,POSMAP(1254))
      equivalence (MATRXT,POSMAP(2482))
c
      real*8 LIMITS(2,10),MATRXT(4,3)
c
      integer*4 lnx(6),lmx(3),nxr(10),i,j,ner,ier,nn,lx,nud
c
      real*8 axes(10),plm(4),w(3),vlin(6),dlin,b,pp(3),pc(3),
     -       pnn(3,12),drad,d,ang,ang1,rout(10)
c
      data lnx /1,0,2,0,3,0/, lmx /1,3,5/
c
      kerr   = 1
      drad   = dsqrt ((gpti(1) - gorg(1))**2 +
     -                (gpti(2) - gorg(2))**2)
      axes(2) = 0.0
      axes(4) = 0.0
      axes(6) = 0.0
      axes(7) = 0.0
      axes(8) = 0.0
      axes(9) = 0.0
      axes(10) = 0.0
      nn     = 0
      nud    = 0
c
c...First check lower limits then upper
c
  100 nud    = nud + 1
      if (nud .gt. 2) go to 500
      do 400 i=1,6
          lx     = lnx(i)
          if (lx .eq. 0) go to 400
c
c...Get limit plane in table coordinates
c
          plm(1) = 0.
          plm(2) = 0.
          plm(3) = 0.
          plm(lx) = 1.0
          if (nud .eq. 1) plm(lx) = 0 - 1.0
          call ptmatr (plm,plm,MATRXT,2)
          w(1)   = 0.
          w(2)   = 0.
          w(3)   = 0.
          w(lx)  = LIMITS(nud,i)
          call ptmatr (w,w,MATRXT,1)
          plm(4) = 0. - (plm(1)*w(1) + plm(2)*w(2) + plm(3)*w(3))
c
c...Get intersection line of limit plane with table plane
c
          call limlin (plm,gorg,vlin,dlin,ier)
          if (ier .eq. 1) go to 400
          if (dlin .ge. drad) go to 400
c
c...Get point on this limit plane
c
          b      = dsqrt (drad*drad - dlin*dlin)
          j      = 0
  200     pp(1)  = vlin(1) + b * vlin(4)
          pp(2)  = vlin(2) + b * vlin(5)
          pp(3)  = vlin(3)
c
c...Check if point is in machine limits
c
          call ptmatb (pp,pc,MATRXT,1)
          axes(lmx(1)) = pc(1)
          axes(lmx(2)) = pc(2)
          axes(lmx(3)) = pc(3)
          call axsxfm (axes,rout)
          call lmtchk (rout,ner,nxr,0)
          if (ner .gt. 0) go to 300
c
c...Add point to stack
c
          nn     = nn + 1
          pnn(1,nn) = pp(1)
          pnn(2,nn) = pp(2)
          pnn(3,nn) = pp(3)
  300     b      = 0. - b
          j      = j + 1
          if (j .eq. 1) go to 200
  400 continue
      go to 100
c
c...Select point most distant from
c...that which caused the error
c
  500 if (nn .eq. 0) go to 8000
      kerr  = 0
      b     = 0.0
      do 800 i=1,nn
          d   = dsqrt ((gpti(1) - pnn(1,i))**2 +
     -                 (gpti(2) - pnn(2,i))**2)
          if (d .gt. b) then
              b = d
              j = i
          end if
  800 continue
c
c...Get angle of the selected point
c
      w(1) = (gpti(1) - gorg(1)) / drad
      w(2) = (gpti(2) - gorg(2)) / drad
      w(3) = 0.0
      call vecang (w,3,ang)
      w(1) = (pnn(1,j) - gorg(1)) / drad
      w(2) = (pnn(2,j) - gorg(2)) / drad
      call vecang (w,3,ang1)
      d     = ang1 - ang
      if (d .lt. -180.0) then
          d   = 360.0 + d
      else if (d .gt. 180.0) then
          d   = d - 360.0
      end if
      gang   = d
c
c...End of routine
c
 8000 return
      end

c***********************************************************************
c
c   SUBROUTINE:  scnlim1 (gorg,gpti,gang,kerr)
c
c   FUNCTION:  This routine is used to check all intersections off
c              the circle with limit planes of the machine.  The circle
c              is created by point on the first limit plane rotated
c              about the table center.
c
c   INPUT:  gorg    R*8  D3    -  Table center point.
c
c           gpti    R*8  D3    -  Point on limit in table coordinates.
c
c   OUTPUT: gang    R*8  D1    -  Angle from gpti point to the most
c                                 distant point of the circle intersec-
c                                 tion with any other limit plane.
c
c           kerr    I*4  D1    -  Returns 1 if all intersections are
c                                 conflicting.
c
c***********************************************************************
c
      subroutine scnlim1 (gorg,gpt1,gpti,gpt2,gplm,grrl,gdll,
     -                    gang,kerr)
c
      include 'post.inc'
c
      integer*4 kerr
      real*8 gorg(3),gpt1(3),gpti(3),gpt2(3),grrl(6),gang,gplm(4),gdll
c
      equivalence (LIMITS,POSMAP(1254))
      equivalence (MATRXT,POSMAP(2482)), (FUZZ8 ,POSMAP(4913))
c
      real*8 FUZZ8,LIMITS(2,10),MATRXT(4,3)
c
      integer*4 lnx(6),lmx(3),nxr(10),ner,ier,ifl,neg
c
      real*8 axes(10),w(3),v(3),vlin(6),dlin,dpt,ddl,
     -       ndot,ndist,drad,d,ang,ppp(3),qqq(3),mzr(3),d2,dpc,t,
     -       pcl(3),delm,zaxs(3),yaxs(3),d3,dpcl,rout(10)
c
      data lnx /1,0,2,0,3,0/, lmx /1,3,5/, zaxs /0.,0.,1.0/
c
c...get distances and set neutral values in axis register
c
      ifl    = 0
      kerr   = 1
      drad   = dsqrt ((gpti(1) - gorg(1))**2 +
     -                (gpti(2) - gorg(2))**2)
      dpt    = dsqrt ((gpt2(1) - gorg(1))**2 +
     -                (gpt2(2) - gorg(2))**2)
      axes(2) = .5*(LIMITS(1,2) + LIMITS(2,2))
      axes(4) = .5*(LIMITS(1,4) + LIMITS(2,4))
      axes(6) = .5*(LIMITS(1,6) + LIMITS(2,6))
      axes(7) = .5*(LIMITS(1,7) + LIMITS(2,7))
      axes(8) = .5*(LIMITS(1,8) + LIMITS(2,8))
      axes(9) = .5*(LIMITS(1,9) + LIMITS(2,9))
      axes(10) = .5*(LIMITS(1,10) + LIMITS(2,10))

      mzr(1) = 0.
      mzr(2) = 0.
      mzr(3) = 0.
      call ptmatr (mzr,mzr,MATRXT,1)
      mzr(3) = gorg(3)
c
c...Get intersection line of limit plane with table plane
c
      call limlin (gplm,gorg,vlin,dlin,ier)
      if (ier .eq. 1) go to 8000
c
c...check if table center is behind the limit plane
c
      call nptln (mzr,vlin,ppp)
      call vcplvc (ppp,mzr,ppp,-1.d0)
      call vcplvc (vlin,gorg,qqq,-1.d0)
      if (ndot (ppp,qqq) .lt. -.00001) ifl = 1
c
      call crosvc (vlin(4),gplm,v)
      call unitvc (v,v)
      delm = gdll / dabs(v(3))
c
c...Get point on the opposite limit plane
c
      call crosvc (vlin(4),zaxs,yaxs)
      call unitvc (yaxs,yaxs)
      call vcplvc (vlin,yaxs,ppp,-delm)

      d3   =  ndist(ppp,gorg)
      neg  = 1
      if (ndot(yaxs,ppp) .lt. 0.) neg = 0
c
c...check if rotation can fix limit error when table center
c...is located outside limit
c
      d    = ndist (gpt2,gpt1)
      call ptlnds (gorg,grrl,ddl)
      call nptln (gorg,grrl,pcl)
      dpc  = ndist (pcl,gpt1)
      d2   = ndist (gpt2,gorg)
      if (ifl .eq. 1) then
         if (ddl .gt. dlin) go to 200
         t  = d / dpc
         if (t .lt. 1. .and. dlin .le. drad) go to 200
         go to 8000
      end if
c
c...Get angle of the selected point
c
  200 call vcplvc (pcl,gorg,w,-1.d0)
      dpcl = ndist(pcl,gorg)
      if (neg .eq. 0. .and. d3 .gt. dpcl) then
         if (dpcl .gt. FUZZ8) then
            call unitvc (w,w)
            v(1) = -yaxs(1)
            v(2) = -yaxs(2)
            v(3) = -yaxs(3)
            call betvec (w,v,ang)
            call crosvc (w,v,pcl)
         else
            call betvec (grrl(4),vlin(4),ang)
            call crosvc (grrl(4),vlin(4),pcl)
         end if
c
c...postion point on the opposite limit plane
c...using old logic in 'scnlim'
c
      else
         kerr = 2
         go to 8000
      end if
      if (pcl(3) .lt. 0.0) then
         d   = -ang
      else
         d   = ang
      end if
c
c...make sure that rotation will not violate other limits
c...otherwise use old logic
c
      kerr   = 2
      call vecadj (gpti,ppp,d,3)
      axes(lmx(1)) = ppp(1)
      axes(lmx(2)) = ppp(2)
      axes(lmx(3)) = ppp(3)
      call axsxfm (axes,rout)
      call lmtchk (rout,ner,nxr,0)
      if (ner .gt. 0) go to 8000
      call vecadj (gpt2,ppp,d,3)
      axes(lmx(1)) = ppp(1)
      axes(lmx(2)) = ppp(2)
      axes(lmx(3)) = ppp(3)
      call axsxfm (axes,rout)
      call lmtchk (rout,ner,nxr,0)
      if (ner .gt. 0) go to 8000
c
      gang   = d
      kerr   = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  reprot2 (gang,gout,gsavi,krot)
c
c   FUNCTION:  This routine is used to replace second rotary axis
c              value in ROTSTO with new value when called in tlaxis and
c              we know that the first rotary axis remains unchanged.
c
c   INPUT:  gang    R*8  D20   -  Current rotary position.
c
c           krot    I*4  D1    -  Rotary axis index which remains unchd.
c
c   OUTPUT: gout    R*8  D20   -  Previous rotary position.
c
c           gsav    R*8  D1    -  Saved value of replaced gout(x)
c
c***********************************************************************
c
      subroutine reprot2 (gang,gout,gsav,krot)
c
      include 'post.inc'
c
      integer*4 krot
c
      real*8 gang(20),gout(20,2),gsav
c
      equivalence (IRTACT,KPOSMP(1256)), (IRTINC,KPOSMP(1461))
c
      integer*4 IRTACT(2),IRTINC(4)
c
      integer*4 ir1,ir2
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
      if (IRTINC(ir1) .eq. krot) then
          gsav = gout(IRTINC(ir2),1)
          gout(IRTINC(ir2),1) = gang(IRTINC(ir2))
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fixrot2 (gsav,gout,krot)
c
c   FUNCTION:  This routine is used to fix replaced second rotary axis
c              value in ROTSTO with original value when called in tlaxis.
c
c   INPUT:  krot    I*4  D1    -  Rotary axis index which remains unchd.
c
c           gsav    R*8  D1    -  Saved value of replaced gout(x)
c
c   OUTPUT: gout    R*8  D20   -  Previous rotary position.
c
c***********************************************************************
c
      subroutine fixrot2 (gsav,gout,krot)
c
      include 'post.inc'
      real*8 gout(20,2),gsav
      integer*4 krot
c
      equivalence (IRTACT,KPOSMP(1256)), (IRTINC,KPOSMP(1461))
c
      integer*4 IRTACT(2),IRTINC(4)
c
      integer*4 ir1,ir2
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
      if (IRTINC(ir1) .eq. krot) then
          gout(IRTINC(ir2),1) = gsav
      end if
c
      return
      end

