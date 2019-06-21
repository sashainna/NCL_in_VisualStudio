c
c***********************************************************************
c
c   FILE NAME:  ptadj
c   CONTAINS:
c               alladj  alladr  axsadj  axsadr  ptxfm   ptxfr   axsxfm
c               axsxfr  linclc  linclr  preadj  preadr  pstadj
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       ptadj.f , 24.3
c     DATE AND TIME OF LAST  MODIFICATION
c       02/06/14 , 11:01:45
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  alladj (gmch,glin,gaxs,grot,kst,ken)
c
c   FUNCTION:  This routine adjusts points for the following conditions.
c
c                 1)  Creates input cl points from the TRANS points.
c                 2)  Adjusts points for table/head rotations.
c                 3)  Adjusts points for TRANS/LAST.
c                 4)  Creates Primary and Secondary linear axes.
c                 5)  Creates actual output linear axes.
c
c   INPUT:  gmch    R*8  D3.4 -  (n,1) = Input cl point.  (n,2) = TRANS
c                                adjusted points.  (n,3) = Rotary ad-
c                                justed points.  (n,4) = TRANS/LAST ad-
c                                justed points.
c
c           glin    R*8  D6   -  Primary and secondary linear axis.
c
c           grot    R*8  D20.2-  Rotary angles on a rotary & linear
c                                scale.
c
c           kst     I*4  D1   -  Beginning modification phase from above
c                                list.
c
c           ken     I*4  D1   -  Ending modification phase from above
c                                list.
c
c   OUTPUT: gmch    R*8  D3.4 -  See INPUT.
c
c           glin    R*8  D6   -  See INPUT.
c
c           gaxs    R*8  D10  -  Primary and secondary linear axis ad-
c                                justed for TRANS/-AXIS.
c
c***********************************************************************
c
      subroutine alladj (gmch,glin,gaxs,grot,kst,ken)
c
      integer*4 kst,ken
c
      real*8 gmch(3,4),glin(6),gaxs(10),grot(20,2)
c
      real*8 vec(3)
c
c...Go to starting adjustment
c
      go to (100,200,300,400,500), kst
c
c...Create input cl points
c...from TRANS points
c
  100 call getijk (grot,vec)
      call preadr (gmch(1,2),gmch(1,1),grot,vec)
      if (ken .eq. 1) go to 8000
c
c...Adjust points to table/head rotations
c
  200 call ptrtad (gmch(1,2),gmch(1,3),grot(1,1),0)
      if (ken .eq. 2) go to 8000
c
c...Adjust points to TRANS/LAST
c
  300 call pstadj (gmch(1,3),gmch(1,4))
      if (ken .eq. 3) go to 8000
c
c...Create Primary and Secondary
c...linear axes
c
  400 call linclc (gmch(1,4),glin)
      if (ken .eq. 4) go to 8000
c
c...Create output linear axes
c
  500 call axsadj (glin,grot(1,2),gaxs)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  alladr (gaxs,glin,gmch,grot,gvec,kst,ken)
c
c   FUNCTION:  This routine reverses the following point adjustments.
c
c                 5)  Reverse TRANS/-AXIS adjustments for linear and
c                     rotary axes.  Creates tool axis vector also.
c                 4)  Create XYZ from Primary & Secondary linear axes.
c                 3)  Reverse TRANS/LAST adjustments.
c                 2)  Reverse table/head rotation adjustments.
c                 1)  Reverse TRANS adjustments.
c
c   INPUT:  gaxs    R*8  D10  -  Primary and secondary linear axis ad-
c                                justed for TRANS/-AXIS.
c
c           glin    R*8  D6   -  Primary and secondary linear axis.
c
c           gmch    R*8  D3.4 -  (n,1) = Input cl point.  (n,2) = TRANS
c                                adjusted points.  (n,3) = Rotary ad-
c                                justed points.  (n,4) = TRANS/LAST ad-
c                                justed points.
c
c           grot    R*8  D20.2-  (n,1) = Rotary angles on a rotary scale.
c                                (n,2) = Rotary angles on a linear scale.
c
c           kst     I*4  D1   -  Beginning modification phase from above
c                                list.
c
c           ken     I*4  D1   -  Ending modification phase from above
c                                list.  'ken' should be less than 'kst'.
c
c   OUTPUT: gmch    R*8  D3.4 -  See INPUT.
c
c           glin    R*8  D6   -  See INPUT.
c
c           grot    R*8  D4   -  See INPUT.
c
c           gvec    R*8  D3   -  Tool axis vector.
c
c***********************************************************************
c
      subroutine alladr (gaxs,glin,gmch,grot,gvec,kst,ken)
c
      integer*4 kst,ken
c
      real*8 gmch(3,4),glin(6),gaxs(10),grot(20,2),gvec(3)
c
c...Go to starting adjustment
c
      go to (500,400,300,200,100), kst
c
c...Adjust output linear axes
c
  100 call axsadr (gaxs,glin,grot,gvec)
      if (ken .ge. 5) go to 8000
c
c...Adjust for Primary and Secondary
c...linear axes
c
  200 call linclr (glin,gmch(1,4))
      if (ken .ge. 4) go to 8000
c
c...Adjust points to TRANS/LAST
c
  300 call pstadr (gmch(1,4),gmch(1,3))
      if (ken .ge. 3) go to 8000
c
c...Adjust points to table/head rotations
c
  400 call ptrtad (gmch(1,3),gmch(1,2),grot(1,1),1)
      if (ken .eq. 2) go to 8000
c
c...Create input cl points
c
  500 call preadr (gmch(1,2),gmch(1,1),grot,gvec)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  axsadj (glin,grot,gout)
c
c   FUNCTION:  This routine modifies the output machine axes after all
c              other calculations have been made.
c
c                 TRANS/-AXIS  TRANS/SCALE,-AXIS
c
c   INPUT:  glin    R*8  D6  -  Current linear primary and secondary
c                               axes.
c
c           grot    R*8  D20 -  Current rotary axes on linear scale.
c
c   OUTPUT: gout    R*8  D10 -  Modified output axes.
c
c***********************************************************************
c
      subroutine axsadj (glin,grot,gout)
c
      include 'post.inc'
c
      equivalence (XFMFL ,KPOSMP(0969)), (IRTINC,KPOSMP(1461))
c
      integer*4 XFMFL(10),IRTINC(4)
c
      equivalence (TRANAX,POSMAP(1320)), (PPINCR,POSMAP(1443))
      equivalence (XFMMAT,POSMAP(4025)), (XFMSTO,POSMAP(4037))
c
      real*8 TRANAX(20),PPINCR(10),XFMMAT(12),XFMSTO(10)
c
      real*8 glin(6),grot(20),gout(10)
c
      integer*4 i,inum
c
      real*8 rdif,rnum
c
c...Apply TRANS/-AXIS to axes
c
      do 100 i=1,6,1
          gout(i) = (glin(i) + TRANAX(i)) * TRANAX(i+10)
  100 continue
c
      do 200 i=1,4,1
          gout(i+6) = (grot(IRTINC(i)) + TRANAX(i+6)) * TRANAX(i+16)
  200 continue
c
c...Make sure move is on
c...minimum increment
c
      do 300 i=1,10,1
          if (PPINCR(i) .ne. 0) then
              rdif   = gout(i) / PPINCR(i)
              inum   = rdif
              rnum   = rdif   - inum
              gout(i) = inum
              if (rnum .ge. .5) gout(i) = gout(i) + 1.
              if (rnum .le. -.5) gout(i) = gout(i) - 1.
              gout(i) = gout(i) * PPINCR(i)
          endif
  300 continue
c
c...Apply transformation matrix
c
      call axsxfm (gout,gout)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  axsadr (gout,glin,grot,gvec)
c
c   FUNCTION:  This routine reverses the following modifications to the
c              output axes.
c
c                 TRANS/-AXIS  TRANS/SCALE,-AXIS
c
c   INPUT:  gout    R*8  D10  -  Current linear/rotary output axes po-
c                               sitions.
c
c   OUTPUT: glin    R*8  D6   -  Linear primary and secondary axes.
c
c           grot    R*8  D20.2-  (n,1) = Rotary axes on linear scale.
c                                (n,2) = Rotary axes on rotary scale.
c
c           gvec    R*8  D3   -  Tool axis vector.
c
c***********************************************************************
c
      subroutine axsadr (gout,glin,grot,gvec)
c
      include 'post.inc'
c
      equivalence (IRTINC,KPOSMP(1461)), (IRTDEF,KPOSMP(1485))
c
      integer*4 IRTINC(4),IRTDEF
c
      equivalence (TRANAX,POSMAP(1320))
c
      real*8 TRANAX(20)
c
c
      real*8 glin(6),grot(20,2),gout(10),gvec(3),rbase(4)
c
      integer*4 i
c
      real*8 rout(10)
c
c...Remove matrix transformation
c
      call axsxfr (gout,rout)
c
c...Apply TRANS/-AXIS to axes
c
      do 100 i=1,6,1
          if (TRANAX(i+10) .eq. 0.) then
              glin(i) = 0.
          else
              glin(i) = rout(i) / TRANAX(i+10) - TRANAX(i)
          endif
  100 continue
c
      do 200 i=1,4,1
          if (TRANAX(i+16) .eq. 0.) then
              grot(IRTINC(i),2) = 0.
          else
              grot(IRTINC(i),2) = rout(i+6) / TRANAX(i+16) - TRANAX(i+6)
          endif
  200 continue
c
c...Get rotary scale angles &
c...tool axis vector
c
      call linrot (grot,rbase,0)
      call getijk (grot,gvec)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptxfm (gin,gout,ktype)
c
c   FUNCTION:  This routine applies the active transformation matrix to
c              XYZ coordinates and Vectors.
c
c   INPUT:  ginp    R*8  D10  -  Current coordinates.
c
c           ktype   I*4  D1   -  1 = Point, 2 = Vector.
c
c   OUTPUT: gout    R*8  D10  -  Output coordinates.
c
c***********************************************************************
c
      subroutine ptxfm (gin,gout,ktype)
c
      include 'post.inc'
c
      integer*4 ktype
c
      real*8 gin(10),gout(10)
c
      equivalence (XFMFL ,KPOSMP(0969))
c
      integer*4 XFMFL(20)
c
      equivalence (XFMMAT,POSMAP(4025)), (XFMSTO,POSMAP(4037))
c
      real*8 XFMMAT(12),XFMSTO(10)
c
c...Apply transformation matrix
c
      if (XFMFL(4) .ne. 0) then
          call matpta (gin,gout,XFMMAT,ktype)
      else
          call copyn (gin,gout,3)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptxfr (gin,gout,ktype)
c
c   FUNCTION:  This routine removes the active transformation matrix from
c              XYZ coordinates and Vectors.
c
c   INPUT:  ginp    R*8  D10  -  Current coordinates.
c
c           ktype   I*4  D1   -  1 = Point, 2 = Vector.
c
c   OUTPUT: gout    R*8  D10  -  Output coordinates.
c
c***********************************************************************
c
      subroutine ptxfr (gin,gout,ktype)
c
      include 'post.inc'
c
      integer*4 ktype
c
      real*8 gin(10),gout(10)
c
      equivalence (XFMFL ,KPOSMP(0969))
c
      integer*4 XFMFL(20)
c
      equivalence (XFMMAT,POSMAP(4025)), (XFMSTO,POSMAP(4037))
c
      real*8 XFMMAT(12),XFMSTO(10)
c
c...Apply transformation matrix
c
      if (XFMFL(4) .ne. 0) then
          call matptr (gin,gout,XFMMAT,ktype)
      else
          call copyn (gin,gout,3)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  axsxfm (gin,gout)
c
c   FUNCTION:  This routine applies the active transformation matrix to
c              the output axes.
c
c   INPUT:  ginp    R*8  D10  -  Current linear/rotary output axes
c                                positions.
c
c   OUTPUT: gout    R*8  D10  -  Output axes positions with active
c                                transformation matrix applied.
c
c***********************************************************************
c
      subroutine axsxfm (gin,gout)
c
      include 'post.inc'
c
      real*8 gin(10),gout(10)
c
      equivalence (XFMFL ,KPOSMP(0969)), (IJKROT,KPOSMP(1739))
c
      integer*4 XFMFL(20),IJKROT
c
      equivalence (XFMMAT,POSMAP(4025)), (XFMSTO,POSMAP(4037))
c
      real*8 XFMMAT(12),XFMSTO(10)
c
      real*8 rpt(3)
c
c...Apply transformation matrix
c
      if (XFMFL(4) .ne. 0 .and. XFMFL(14) .ne. 0) then
          rpt(1) = gin(1)
          rpt(2) = gin(3)
          rpt(3) = gin(5)
          call matpta (rpt,rpt,XFMMAT,1)
          gout(1) = rpt(1)
          gout(2) = gin(2)
          gout(3) = rpt(2)
          gout(4) = gin(4)
          gout(5) = rpt(3)
          gout(6) = gin(6)
          if (IJKROT .ne. 1) then
              gout(7) = gin(7) - XFMSTO(7)
              gout(8) = gin(8) - XFMSTO(8)
              gout(9) = gin(9) - XFMSTO(9)
              gout(10) = gin(10) - XFMSTO(10)
          endif
      else
          call copyn (gin,gout,10)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  axsxfr (gin,gout)
c
c   FUNCTION:  This routine applies the inverse of the active transformation
c              matrix to the output axes (returns the output axes without
c              the transformation matrix applied).
c
c   INPUT:  ginp    R*8  D10  -  Current linear/rotary output axes
c                                positions.
c
c   OUTPUT: gout    R*8  D10  -  Output axes positions without active
c                                transformation matrix applied.
c
c***********************************************************************
c
      subroutine axsxfr (gin,gout)
c
      include 'post.inc'
c
      real*8 gin(10),gout(10)
c
      equivalence (XFMFL ,KPOSMP(0969)), (IJKROT,KPOSMP(1739))
c
      integer*4 XFMFL(20),IJKROT
c
      equivalence (XFMMAT,POSMAP(4025)), (XFMSTO,POSMAP(4037))
c
      real*8 XFMMAT(12),XFMSTO(10)
c
      real*8 rpt(3)
c
c...Apply transformation matrix
c
      if (XFMFL(4) .ne. 0 .and. XFMFL(14) .ne. 0) then
          rpt(1) = gin(1)
          rpt(2) = gin(3)
          rpt(3) = gin(5)
          call matptr (rpt,rpt,XFMMAT,1)
          gout(1) = rpt(1)
          gout(2) = gin(2)
          gout(3) = rpt(2)
          gout(4) = gin(4)
          gout(5) = rpt(3)
          gout(6) = gin(6)
          if (IJKROT .ne. 1) then
              gout(7) = XFMSTO(7) + gin(7)
              gout(8) = XFMSTO(8) + gin(8)
              gout(9) = XFMSTO(9) + gin(9)
              gout(10) = XFMSTO(10) + gin(10)
          endif
      else
          call copyn (gin,gout,10)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  linclc (glin,gmch)
c
c   FUNCTION:  This routine calculates the actual machine slide posi-
c              tions for the linear axis from the machine XYZ positions.
c
c   INPUT:  glin    R*8  D3  Machine XYZ position.
c
c   OUTPUT: gmch    R*8  D6  Acutual machine slide positions for primary
c                            and secondary linear XYZ axes.
c
c***********************************************************************
c
      subroutine linclc (glin,gmch)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (ACTLIN,KPOSMP(1205))
      equivalence (LNCALC,KPOSMP(1208)), (ACTBLN,KPOSMP(1336))
c
      integer*4 NUMLIN(3),ACTLIN(3),LNCALC(3),ACTBLN(3)
c
      equivalence (LNDIST,POSMAP(1251)), (LINSTO,POSMAP(1399))
c
      real*8 LNDIST(3),LINSTO(6)
c
      real*8 glin(3),gmch(6)
c
      integer*4 i,inc,j,iaxs(10)
c
c...Calculate Primary and Secondary Axes
c
      inc    = 1
      do 500 i=1,3,1
          if (NUMLIN(i) .eq. 2) then
c
c......1st calculation type
c
              if (LNCALC(i) .eq. 1) then
                  if (ACTLIN(i) .eq. 1) then
                      gmch(inc) = glin(i) - LNDIST(i) - LINSTO(inc+1)
                      gmch(inc+1) = LINSTO(inc+1)
                  else if (ACTLIN(i) .eq. 2) then
                      gmch(inc) = LINSTO(inc)
                      gmch(inc+1) = glin(i) - LNDIST(i) - LINSTO(inc)
                  endif

c
c......2nd calculation type
c
              else if (LNCALC(i) .eq. 2) then
                  if (ACTLIN(i) .eq. 1) then
                      gmch(inc) = glin(i)
                      gmch(inc+1) = LINSTO(inc+1)
                  else if (ACTLIN(i) .eq. 2) then
                      gmch(inc) = LINSTO(inc)
                      gmch(inc+1) = glin(i) - LINSTO(inc)
                  endif
c
c......3rd calculation type
c
              else
                  if (ACTLIN(i) .eq. 1) then
                      gmch(inc) = glin(i)
                      gmch(inc+1) = LINSTO(inc+1)
                  else if (ACTLIN(i) .eq. 2) then
                      gmch(inc) = LINSTO(inc)
                      gmch(inc+1) = glin(i)
                  endif
              endif
c
c...Check if output of both linear axes is required
c
              if (ACTBLN(i) .eq. 1) then
                  if (LNCALC(i) .eq. 3) then
                      gmch(inc+2-ACTLIN(i)) = gmch(inc-1+ACTLIN(i))
                  else
                      do 405 j = 1,10
                          iaxs(j) = 0
  405                 continue
                      iaxs(inc+2-ACTLIN(i)) = 1
                      call frcmot (2,3,iaxs)
                  end if
              end if
c
c......Primary axis only
c
          else
              gmch(inc) = glin(i)
              gmch(inc+1) = LINSTO(inc+1)
          endif
          inc    = inc    + 2
  500 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  linclr (gmch,glin)
c
c   FUNCTION:  This routine calculates the XYZ position from the machine
c              Primary and Secondary positions.
c
c   INPUT:  gmch    R*8  D6  Actual machine slide positions for primary
c                            and secondary linear XYZ axes.
c
c   OUTPUT: glin    R*8  D3  Machine XYZ position.
c
c***********************************************************************
c
      subroutine linclr (gmch,glin)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (ACTLIN,KPOSMP(1205))
      equivalence (LNCALC,KPOSMP(1208)), (ACTBLN,KPOSMP(1336))
c
      integer*4 NUMLIN(3),ACTLIN(3),LNCALC(3),ACTBLN(3)
c
      equivalence (LNDIST,POSMAP(1251)), (LINSTO,POSMAP(1399))
c
      real*8 LNDIST(3),LINSTO(6)
c
      real*8 glin(3),gmch(6)
c
      integer*4 inc,i
c
c...Calculate Primary and Secondary Axes
c
      inc    = 1
      do 500 i=1,3,1
          if (NUMLIN(i) .eq. 2) then
c
c......1st calculation type
c
              if (LNCALC(i) .eq. 1) then
                  glin(i) = gmch(inc) + LNDIST(i) + gmch(inc+1)
c
c......2nd calculation type
c
              else if (LNCALC(i) .eq. 2) then
                  if (ACTLIN(i) .ne. 2) then
                      glin(i) = gmch(inc)
                  else
                      glin(i) = gmch(inc+1) + gmch(inc)
                  endif
c
c......3rd calculation type
c
              else
                  if (ACTLIN(i) .ne. 2) then
                      glin(i) = gmch(inc)
                  else
                      glin(i) = gmch(inc+1)
                  endif
              endif
c
c......Primary axis only
c
          else
              glin(i) = gmch(inc)
          endif
          inc    = inc    + 2
  500 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  preadj (glin,gout,gtvi,gtvo)
c
c   FUNCTION:  This routine modifies the input CLPT's prior to all other
c              manipulation of points, applying the following values.
c
c                 ORIGIN  TRANS   TRANS/SCALE   TRANS/MX
c                 Lathe XY to ZX transformation
c
c   INPUT:  glin    R*8  D3  -  Input cl point.
c
c           gtvi    R*8  D3  -  Input tool axis vector.
c
c   OUTPUT: gout    R*8  D3  -  Modified cl point.
c
c           gtvo    R*8  D3  -  Modified tool axis vector.
c
c***********************************************************************
c
      subroutine preadj (glin,gout,gtvi,gtvo)
c
      include 'post.inc'
      real*8 glin(3),gout(3),gtvi(3),gtvo(3)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHXY ,KPOSMP(1225))
      equivalence (LTHXD ,KPOSMP(1339))
      equivalence (TURFL ,KPOSMP(1824)), (MXFLAG,KPOSMP(4002))
      equivalence (MXTRAN,KPOSMP(4003)), (LTMODE,KPOSMP(4125))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 MTPDYN,MACHTP,LTHXY,TURFL(5),MXFLAG,MXTRAN,LTHXD,
     -          LTMODE
c
      equivalence (ORGIN ,POSMAP(1305)), (TRANZ ,POSMAP(1308))
      equivalence (TAXTOL,POSMAP(2262)), (TRAXSV,POSMAP(2475))
      equivalence (TURDIS,POSMAP(3961)), (TRZX  ,POSMAP(3993))
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
      equivalence (PRTRAD,POSMAP(4604))
c
      real*8 ORGIN(3),TRANZ(12),TRZX(2),TURDIS,REFMAT(12),PRTRAD,
     -       TRAMAT(12),TAXTOL,TRAXSV
c
      real*8 rnum
c
c...Apply ORIGIN & TRANS to points
c
      if (MXTRAN .eq. 0) then
          gout(1) = (glin(1) - ORGIN(1) + TRANZ(1)) * TRANZ(7)
          gout(2) = (glin(2) - ORGIN(2) + TRANZ(2)) * TRANZ(8)
          gout(3) = (glin(3) - ORGIN(3) + TRANZ(3)) * TRANZ(9)
      else
          call matpta (glin,gout,TRAMAT,1)
      end if
c
c...Apply tool axis tolerance
c
      call copyn (gtvi,gtvo,3)
      if (dabs(gtvo(1)) .lt. TAXTOL) gtvo(1) = 0.
      if (dabs(gtvo(2)) .lt. TAXTOL) gtvo(2) = 0.
      if (dabs(gtvo(3)) .lt. TAXTOL) gtvo(3) = 0.
c
c...Translate up tool axis
c
      gout(1) = gout(1) + TRAXSV*gtvo(1)
      gout(2) = gout(2) + TRAXSV*gtvo(2)
      gout(3) = gout(3) + TRAXSV*gtvo(3)
c
c...Apply machine adjust data if specified
c
      if (MXFLAG .eq. 1) then
          call matpta (gout,gout,REFMAT,1)
      end if
c
c...Lathe XY to ZX transformation
c
      if (MTPDYN .eq. 2) then
          if (LTMODE .ne. 2 .and. LTHXY .eq. 1) then
              rnum    = gout(1)
              gout(1) = gout(2)
              gout(2) = gout(3)
              gout(3) = rnum
              if (LTMODE .eq. 0 .or. LTMODE .eq. 3) then
                 rnum    = gtvi(1)
                 gtvo(1) = gtvi(2)
                 gtvo(2) = gtvi(3)
                 gtvo(3) = rnum
              endif
          endif
c
c...LMDP mode, wrap points around the part
c
          if (MACHTP .eq. 4) then
             if (LTMODE .eq. 2) then
                call predia (gout,gout,gtvo)
             else if (LTMODE .eq. 1) then
                call prefac (gout,gout)
             end if
          end if
          if (LTHXD .eq. 1) gout(1) = 0. - gout(1)
c
c...TURRET/ADJUST translations
c
          gout(3) = gout(3) + TRZX(1)
          gout(1) = gout(1) + TRZX(2)
c
c...Rear Turret adjustment
c
         if (TURFL(2) .eq. 2) then
             if (LTHXD .eq. 1) then
                 gout(1) = gout(1) - TURDIS
             else
                 gout(1) = gout(1) + TURDIS
             endif
         endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  preadr (glin,gout,grot,gvco)
c
c   FUNCTION:  This routine creates input CLPT's from points modified
c              using the following values.
c
c                 ORIGIN  TRANS   TRANS/SCALE
c                 Lathe XY to ZX transformation
c
c   INPUT:  glin    R*8  D3    -  Modified cl point.
c
c           gvco    R*8  D3    -  Tool axis vector.
c
c   OUTPUT: gout    R*8  D3    -  Input cl point.
c
c           grot    R*8  D20.2 - Rotary axes position on Mill/Turn.
c
c***********************************************************************
c
      subroutine preadr (glin,gout,grot,gvco)
c
      include 'post.inc'
      real*8 glin(3),gout(3),grot(20,2),gvco(3)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHXY ,KPOSMP(1225))
      equivalence (TURFL ,KPOSMP(1824)), (MXFLAG,KPOSMP(4002))
      equivalence (MXTRAN,KPOSMP(4003)), (LTMODE,KPOSMP(4125))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 MTPDYN,MACHTP,LTHXY,TURFL(5),MXFLAG,MXTRAN,LTMODE
c
      equivalence (ORGIN ,POSMAP(1305)), (TRANZ ,POSMAP(1308))
      equivalence (TRAXSV,POSMAP(2475))
      equivalence (TURDIS,POSMAP(3961)), (TRZX  ,POSMAP(3993))
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
c
      real*8 ORGIN(3),TRANZ(12),TRZX(2),TURDIS,REFMAT(12),TRAMAT(12),
     1       TRAXSV
c
      integer*4 i
c
      real*8 rnum
c
      gout(1) = glin(1)
      gout(2) = glin(2)
      gout(3) = glin(3)
c
c...Lathe transformations
c
      if (MTPDYN .eq. 2) then
c
c......Rear Turret adjustment
c
         if (TURFL(2) .eq. 2) gout(1) = gout(1) - TURDIS
c
c......TURRET/ADJUST translations
c
          gout(3) = gout(3) - TRZX(1)
          gout(1) = gout(1) - TRZX(2)
c
c......LMDP mode, unwrap points from cylinder
c
          if (MACHTP .eq. 4) then
             if (LTMODE .eq. 2) then
                call prediar (gout,gout,grot,gvco)
             else if (LTMODE .eq. 1) then
                call prefacr (gout,gout)
             end if
          end if
c
c......Lathe XY to ZX transformation
c
          if (LTMODE .ne. 2 .and. LTHXY .eq. 1) then
              rnum    = gout(1)
              gout(1) = gout(3)
              gout(3) = gout(2)
              gout(2) = rnum
cc              if (LTMODE .eq. 0 .or. LTMODE .eq. 3) then
cc                  rnum    = gvco(1)
cc                  gvco(1) = gvco(3)
cc                  gvco(3) = gvco(2)
cc                  gvco(2) = rnum
cc              end if
          endif
      endif
c
c...Apply machine adjust data if specified
c
      if (MXFLAG .eq. 1) then
          call matptr (gout,gout,REFMAT,1)
      end if
c
c...Translate down tool axis
c
      gout(1) = gout(1) - TRAXSV*gvco(1)
      gout(2) = gout(2) - TRAXSV*gvco(2)
      gout(3) = gout(3) - TRAXSV*gvco(3)
c
c...Apply ORIGIN & TRANS to points
c
      if (MXTRAN .eq. 0) then
          do 500 i=1,3,1
              if (TRANZ(i+6) .eq. 0.) then
                  gout(i) = 0.
              else
                  gout(i) = gout(i) / TRANZ(i+6) - TRANZ(i) + ORGIN(i)
              endif
  500     continue
      else
          call matptr (gout,gout,TRAMAT,1)
c
c...Causes TLVEC to be set to pre-xform values
c...When all routines expect post-xform values
c
cc          call matptr (gvco,gvco,TRAMAT,2)
      end if
c
c...Apply TRANS/MX if specified
c
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pstadj (glin,gout)
c
c   FUNCTION:  This routine modifies the linear XYZ axes just prior to
c              being converted to machine axes, applying the following
c              values.
c
c                 TRANS/,LAST  TRANS/SCALE,LAST
c                 Lathe diameter programming.
c
c   INPUT:  glin    R*8  D3  -  Input cl point.
c
c   OUTPUT: gout    R*8  D3  -  Modified cl point.
c
c***********************************************************************
c
      subroutine pstadj (glin,gout)
c
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHDIA,KPOSMP(1228))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 MTPDYN,MACHTP,LTHDIA(2)
c
      equivalence (TRANZ ,POSMAP(1308))
c
      real*8 TRANZ(12)
c
      real*8 glin(3),gout(3)
c
c...Apply ORIGIN & TRANS to points
c
      gout(1) = (glin(1) + TRANZ(4)) * TRANZ(10)
      gout(2) = (glin(2) + TRANZ(5)) * TRANZ(11)
      gout(3) = (glin(3) + TRANZ(6)) * TRANZ(12)
c
c...Lathe diameter programming
c
      if (MTPDYN .eq. 2 .and. LTHDIA(1) .ne. LTHDIA(2)) then
          if (LTHDIA(2) .eq. 1) then
              gout(1) = gout(1) / 2.
          else
              gout(1) = gout(1) * 2.
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pstadr (glin,gout)
c
c   FUNCTION:  This routine reverses the following modifications to the
c              linear XYZ axes.
c
c                 TRANS/,LAST  TRANS/SCALE,LAST
c                 Lathe diameter programming.
c
c   INPUT:  glin    R*8  D3  -  Input cl point.
c
c   OUTPUT: gout    R*8  D3  -  Modified cl point.
c
c***********************************************************************
c
      subroutine pstadr (glin,gout)
c
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHDIA,KPOSMP(1228))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 MACHTP,LTHDIA(2),MTPDYN
c
      equivalence (TRANZ ,POSMAP(1308))
c
      real*8 TRANZ(12)
c
      real*8 glin(3),gout(3)
c
      integer*4 i
c
      real*8 rnum(3)
c
      rnum(1) = glin(1)
      rnum(2) = glin(2)
      rnum(3) = glin(3)
c
c...Lathe diameter programming
c
      if (MTPDYN .eq. 2 .and. LTHDIA(1) .ne. LTHDIA(2)) then
          if (LTHDIA(1) .eq. 1) then
              rnum(1) = rnum(1) / 2.
          else
              rnum(1) = rnum(1) * 2.
          endif
      endif
c
c...Apply ORIGIN & TRANS to points
c
      do 500 i=1,3,1
          if (TRANZ(i+9) .eq. 0.) then
              gout(i) = 0.
          else
              gout(i) = rnum(i) / TRANZ(i+9) - TRANZ(I+3)
          endif
  500 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  predia (ginp,gout,gtv)
c
c   FUNCTION:  This routine converts XY flat plan into cylindrical
c              surface wraped around cylinder with predefined radius.
c              Used with MODE/MILL,DIAMTR on Lathe Mill machine.
c
c
c   INPUT:  ginp    R*8  D3  -  Input cl point (2D surface XY plan).
c
c   OUTPUT: gout    R*8  D3  -  Modified cl point (cylinder surface).
c
c           gtv     R*8  D3  -  Tool vector (PERPTO cylinder at point).
c
c***********************************************************************
c
      subroutine predia (ginp,gout,gtv)
c
      include 'post.inc'
      real*8 ginp(3),gout(3),gtv(3)
c
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (LTHDIA,KPOSMP(1228)), (LTMODE,KPOSMP(4125))
      equivalence (MODCYL,KPOSMP(4204)), (NCPLAN,KPOSMP(4226))
c
      integer*4 MACHTP,LTMODE,LTHDIA(2),MODCYL,NCPLAN(3)
c
      equivalence (PI    ,POSMAP(0001)), (PRTRAD,POSMAP(4604))
c
      real*8 PI,PRTRAD
c
      real*8 alph
      integer*4 iax(5),nax,nay
      data iax /1,2,3,1,2/
c
c...get primary & secondary axis of table plane
c...and calculate tool angle from circumference
c
      nax     = iax(NCPLAN(2)+1)
      nay     = iax(NCPLAN(2)+2)
      alph    = ginp(2) / PRTRAD
c     if (NCPLAN(3) .ne. nax) alph = alph + .5d0*PI
c
c...get tool axis vector
c
      gtv(nax) = dcos (alph)
      gtv(nay) = dsin (alph)
      gtv(NCPLAN(2))  = 0.d0
c
c...wrap the input point up the cylinder
c
      gout(NCPLAN(2)) = ginp(1)
      gout(nax) = PRTRAD * gtv(nax)
      gout(nay) = PRTRAD * gtv(nay)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prediar (ginp,gout,grot,gtvo)
c
c   FUNCTION:  This routine reverses convertion of XY flat plan into
c              cylindrical surface wraped around cylinder with predefined
c              radius.  Used with MODE/MILL,DIAMTR on Lathe Mill machine
c              to unwrap cylinder.
c
c
c   INPUT:  ginp    R*8  D3    -  Input wraped point coordinates.
c
c   OUTPUT: gout    R*8  D3    -  Modified unwraped cl point.
c
c           grot    R*8  D20.2 -  Rotary axes position at point.
c
c           gtvo    R*8  D3    -  Tool vector at output point.
c
c***********************************************************************
c
      subroutine prediar (ginp,gout,grot,gtvo)
c
      include 'post.inc'
      real*8 ginp(3),gout(3),grot(20,2),gtvo(3)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHXY ,KPOSMP(1225))
      equivalence (LTHDIA,KPOSMP(1228)), (IRTINC,KPOSMP(1461))
      equivalence (LTMODE,KPOSMP(4125))
      equivalence (MODCYL,KPOSMP(4204)), (NCPLAN,KPOSMP(4226))
c
      integer*4 MACHTP,LTMODE,LTHDIA(2),LTHXY,NCPLAN(3),MODCYL,
     1          IRTINC(4)
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
      equivalence (PRTRAD,POSMAP(4604))
c
      real*8 PI,RAD,PRTRAD
c
      real*8 alph
      integer*4 iax(5),irt
      data iax /1,2,3,1,2/
c
c...get cylinder rotary axis and its angle
c
      irt     = 1
      if (MODCYL .ne. 0) irt = MODCYL
      alph    = grot(IRTINC(irt),2) / RAD
c
c...adjust angle for spindle vector/primary axis misplacement
c
      if (NCPLAN(3) .ne. iax(NCPLAN(2)+1)) alph = alph + .5d0*PI
      if (alph .gt. 2.d0*PI) alph = alph - 2.d0*PI
c
c...unwrap the point from cylinder to XY plane
c
      gout(1) = ginp(NCPLAN(2))
      gout(2) = PRTRAD * alph
      gout(3) = 0.d0
c
c...set tool axis to original XY plan
c
      gtvo(1)  = 0.d0
      gtvo(2)  = 0.d0
      gtvo(3)  = 1.d0
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prefac (ginp,gout)
c
c   FUNCTION:  This routine converts XY plan into X-Y plan
c              used with MODE/MILL,FACE on Lathe Mill machine.
c
c
c   INPUT:  ginp    R*8  D3  -  Input cl point (XY pp plan).
c
c   OUTPUT: gout    R*8  D3  -  Modified cl point (X-Y machine).
c
c***********************************************************************
c
      subroutine prefac (ginp,gout)
c
      include 'post.inc'
      real*8 ginp(3),gout(3)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHXY ,KPOSMP(1225))
c
      integer*4 MACHTP,LTHXY
c
      equivalence (ZLEVEL,POSMAP(4852))
c
      real*8 ZLEVEL
c
      real*8 rnum
c
      if (LTHXY .eq. 2) then
         rnum = ginp(1)
         gout(1) = ginp(2)
         gout(2) = rnum
         gout(3) = ZLEVEL
      else
         rnum = ginp(3)
         gout(3) = ZLEVEL
         gout(2) = rnum
         gout(1) = ginp(1)
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prefacr (ginp,gout)
c
c   FUNCTION:  This routine reverses convertion of XY flat plan into
c              cylindrical surface wraped around cylinder with predefined
c              radius.  Used with MODE/MILL,DIAMTR on Lathe Mill machine
c              to unwrap cylinder.
c
c
c   INPUT:  ginp    R*8  D3  -  Input wraped point coordinates.
c
c   OUTPUT: gout    R*8  D3  -  Modified unwraped cl point.
c
c
c***********************************************************************
c
      subroutine prefacr (ginp,gout)
c
      include 'post.inc'
      real*8 ginp(3),gout(3)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHXY ,KPOSMP(1225))
c
      integer*4 MACHTP,LTHXY
c
      real*8 rnum
c
      if (LTHXY .eq. 2) then
         rnum = ginp(1)
         gout(1) = ginp(2)
         gout(2) = rnum
         gout(3) = ginp(3)
      else
         rnum = ginp(2)
         gout(1) = ginp(1)
         gout(2) = ginp(3)
         gout(3) = rnum
      end if
c
      return
      end
