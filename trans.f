c
c***********************************************************************
c
c   FILE NAME:  trans
c   CONTAINS:
c               trans   trapvv  traoff  traout  tramac
c
c     COPYRIGHT 2005 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        trans.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/12/15 , 17:51:12
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  trans
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 TRANS/[SCALE,] x,y,z [,LAST]
c                 TRANS/mx
c                 TRANS/UP,n
c                 TRANS/[TOOL [,NOMORE]] [,] [VECTOR [,i,j,k]] [,AUTO]
c                 TRANS/[SCALE,] -AXIS[,n] ,v, ...
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine trans
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (XFMLK ,KPOSMP(0941)), (XFMFL ,KPOSMP(0969))
      equivalence (MXFLAG,KPOSMP(4002)), (MXTRAN,KPOSMP(4003))
c
      integer*4 MXCL,IPSTWD(50),MXFLAG,MXTRAN,XFMFL(20),XFMLK(5)
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (TRANZ ,POSMAP(1308)), (TRANAX,POSMAP(1320))
      equivalence (AXSSTO,POSMAP(1425)), (TRAXSV,POSMAP(2475))
      equivalence (TRAMAT,POSMAP(4013)), (XFMXVC,POSMAP(4047))
c
      real*8 METCNV,PSTWD(50),TRANZ(12),TRANAX(20),TRAMAT(12),
     1       AXSSTO(10),XFMXVC(3),TRAXSV
c
      integer*4 ist,inc,i,nprm,isub,ilast,iscal,nwds,imatr,ifl(4),ierr
c
      real*8 rcnv,tvec(3)
c
c...Initialize routine
c
      if (MXCL .eq. 0) go to 9000
      imatr  = 0
      rcnv   = METCNV
c
c...TRANS/UP,val
c
      if (IPSTWD(1) .eq. 112) then
          if (MXCL .lt. 2 .or. IPSTWD(2) .ne. 0) then
              inc    = i
              go to 9100
          endif
          TRAXSV = PSTWD(2) * rcnv
          go to 8000
      endif
c
c...See if SCALE was specified
c
      iscal  = 0
      ist    = 1
      nwds   = MXCL
      if (IPSTWD(1) .eq. 25 .or. IPSTWD(1) .eq. 610) then
          ist    = 2
          rcnv   = 1.0
          nwds   = MXCL   - 1
          if (IPSTWD(1) .eq. 610) imatr = 1
          iscal  = 1 - imatr
      endif
      if (nwds .eq. 0) go to 9000
c
c...TRANS/x,y,z
c
      ilast  = 0
      if (IPSTWD(ist) .eq. 0) then
c
c......TRANS/...,LAST
c
          if (IPSTWD(MXCL) .eq. 52) then
              if (imatr .eq. 1) go to 9100
              nwds   = nwds   - 1
              ilast  = 1
              if (nwds .eq. 0) go to 9000
          endif
c
c......Set pointers
c
          inc    = 0
          if (ilast .eq. 1) inc = 3
          if (iscal .eq. 1) inc = inc + 6
c
c......TRANS/n
c
          if (nwds .eq. 1) then
              TRANZ(inc+1) = PSTWD(ist) * rcnv
              TRANZ(inc+2) = PSTWD(ist) * rcnv
              TRANZ(inc+3) = PSTWD(ist) * rcnv
              MXTRAN = 0
          else if (nwds .eq. 3) then
              ist    = ist    - 1
              do 300 i=1,3,1
                  if (IPSTWD(ist+i) .ne. 0) then
                      inc    = ist    + i
                      go to 9100
                  endif
                  TRANZ(inc+i) = PSTWD(ist+i) * rcnv
  300         continue
              MXTRAN = 0
c
c...TRANS/MX,...
c
          else if (nwds .eq. 12) then
              ist    = ist    - 1
              do 315 i=1,12,1
                  if (IPSTWD(ist+i) .ne. 0) go to 9100
                  TRAMAT(i) = PSTWD(ist+i)
  315         continue
              TRAMAT(4) = TRAMAT(4) * METCNV
              TRAMAT(8) = TRAMAT(8) * METCNV
              TRAMAT(12) = TRAMAT(12) * METCNV
              MXTRAN = 1
          else
              go to 9000
          endif
c
c...TRANS/TOOL,VECTOR,i,j,k
c
      else if (IPSTWD(1) .eq. 617 .or. IPSTWD(1) .eq. 604) then
          inc    = 1
          ifl(1) = 0
          ifl(2) = 0
          ifl(3) = 0
          ifl(4) = 0
c
c......TRANS/TOOL
c
  400     if (ist .gt. 2) go to 9000
          if (IPSTWD(inc) .eq. 617) then
c
c.........TRANS/TOOL,NOMORE
c
              if (inc .lt. MXCL .and. IPSTWD(inc+1) .eq. 53) then
                  XFMFL(5) = 0
                  call traoff
                  inc    = inc    + 2
                  ist    = 3
c
c.........TRANS/TOOL,ON-OFF
c
              else if (inc .lt. MXCL .and. (IPSTWD(inc+1) .eq. 71 .or.
     1                 IPSTWD(inc+1) .eq. 72)) then
                  XFMFL(14) = 1
                  if (IPSTWD(inc+1) .eq. 72) XFMFL(14) = 0
                  ist    = 3
                  if (ist .le. MXCL) go to 9700
                  go to 8000
c
c.........TRANS/TOOL
c
              else
                  if (ifl(1) .eq. 1) go to 9000
                  ifl(1) = 1
                  inc    = inc    + 1
                  ist    = ist    + 1
              endif
c
c......TRANS/VECTOR
c
          else if (IPSTWD(inc) .eq. 604) then
              if (ifl(2) .eq. 1) go to 9000
              ifl(2) = 1
              inc    = inc   + 1
c
c..........TRANS/VECTOR,i,j,k
c
              if (inc .lt. MXCL .and. IPSTWD(inc) .eq. 0) then
                  if (MXCL .lt. inc+2 .or. IPSTWD(inc) .ne. 0 .or.
     1                IPSTWD(inc+1) .ne. 0 .or. IPSTWD(inc+2) .ne. 0)
     2                    go to 9000
                  ifl(3) = 1
                  tvec(1) = PSTWD(inc)
                  tvec(2) = PSTWD(inc+1)
                  tvec(3) = PSTWD(inc+2)
                  inc     = inc    + 3
               endif
c
c......TRANS/TOOL,AUTO
c
          else if (IPSTWD(inc) .eq. 88) then
              if (ifl(4) .ne. 0) go to 9000
              ifl(4) = 1
              inc    = inc    + 1
              ist    = ist    + 1
c
c......TRANS/TOOL,NOW
c
          else if (IPSTWD(inc) .eq. 161) then
              if (ifl(4) .ne. 0) go to 9000
              ifl(4) = 2
              inc    = inc    + 1
              ist    = ist    + 1
c
c......Unrecognized word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9400
              go to 9200
          endif
          if (inc .le. MXCL) go to 400
c
c......Calculate and output new matrix
c
          if (ifl(1)+ifl(2) .gt. 0 .and. ifl(4) .ne. 1) then
              XFMFL(5) = 0
              call trapvv (ifl,AXSSTO,tvec,1,ierr)
              if (ierr .ne. 0) go to 9600
          else if (ifl(4) .eq. 1) then
              XFMFL(5) = 1
              do 450 i=1,3,1
                  XFMLK(i) = ifl(i)
                  XFMXVC(i) = tvec(i)
  450         continue
          endif
          XFMFL(14) = 1
c
c...TRANS/-AXIS(,n),v
c
      else
          if (nwds .lt. 2) go to 9000
c
c......Get next parameter section
c
  500     inc    = ist
          if (IPSTWD(ist) .eq. 0) go to 9200
          inc    = inc    + 1
          do 600 i=inc,MXCL,1
              if (IPSTWD(i) .ne. 0) go to 650
  600     continue
          i      = MXCL   + 1
  650     nprm   = i      - inc
c
c.........Invalid number of parameters
c
          if (nprm .lt. 1) then
              inc    = i
              go to 9100
          else if (nprm .gt. 2) then
              inc    = inc    + 2
              go to 9200
          endif
c
c.........Check parameter values
c
          isub   = 1
          if (nprm .eq. 2) then
              isub   = PSTWD(inc)
              inc    = inc    + 1
          endif
c
c......Set trans value
c
          if (IPSTWD(ist) .eq. 84) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9300
              endif
              ist    = 0
          else if (IPSTWD(ist) .eq. 85) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9300
              endif
              ist    = 2
          else if (IPSTWD(ist) .eq. 86) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9300
              endif
              ist    = 4
          else if (IPSTWD(ist) .eq. 132) then
              if (isub .lt. 1 .or. isub .gt. 4) then
                  inc    = inc    - 1
                  go to 9300
              endif
              ist    = 6
          else
              go to 9400
          endif
          if (iscal .eq. 1) ist = ist + 10
          ist    = ist    + isub
          TRANAX(ist) = PSTWD(inc) * rcnv
          ist    = inc    + 1
          if (ist .le. MXCL) go to 500
          if (ilast .eq. 1) go to 9500
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Number expected
c
 9100 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9200 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9300 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Invalid minor word
c
 9400 call psterr (2,'INVMINOR',' ',ist)
      go to 8000
c
c...Invalid minor word
c...Warning
c
 9500 call psterr (1,'IVMINORW',' ',MXCL)
      go to 8000
c
c...Invalid matrix definition
c
 9600 call psterr (2,'INVMX',' ',MXCL)
      go to 8000
c
c...Too many parameters
c...Warning
c
 9700 call psterr (1,'INVNUMPR',' ',ist)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE: trapvv (kfl,gaxs,gvecx,kwhen,kerr)
c
c   FUNCTION:  This routine calculates and outputs a transformation
c              block based on the provided axes positions.
c
c   WARNING:   The TRANS/AXIS values are not taken into account for
c              transformation blocks, nor are secondary linear axes.

c
c   INPUT:  kfl     I*4  D3    -  (1) = 1 - Use origin point (gpos) in matrix
c                                 calculation, (2) = 1 - Use Z-axis vector,
c                                 (3) = 1 - Use X-axis vector.
c
c           gaxs    R*8  D10   -  Current machine axis position.
c
c           gvecx   R*8  D3    -  X-axis vector of new system when
c                                 'kfl(3)' = 1.
c
c           kwhen   I*4  D1    -  1 = Only time this Macro is called,
c                                 2 = Called prior to motion block,
c                                 3 = Called after motion block.
c
c   OUTPUT: kerr    I*4  D1    -  1 = An error occurred attempting to
c                                 calculate the matrix.
c
c***********************************************************************
c
      subroutine trapvv (kfl,gaxs,gvecx,kwhen,kerr)
c
      include 'post.inc'
c
      equivalence (XFMFL ,KPOSMP(0969))
      equivalence (IRTNUM,KPOSMP(1243)), (LASTAB,KPOSMP(1260))
      equivalence (NOTABS,KPOSMP(1283)), (IRTWRK,KPOSMP(1506))
      equivalence (IJKROT,KPOSMP(1739))
c
      integer*4 IJKROT,NOTABS,XFMFL(20),IRTWRK(20),IRTNUM,LASTAB
c
      equivalence (TLVEC ,POSMAP(1369)), (XFMMAT,POSMAP(4025))
      equivalence (XFMSTO,POSMAP(4037))
c
      real*8 XFMSTO(10),TLVEC(3),XFMMAT(12)
c
      integer*4 kfl(3),kerr,kwhen
c
      real*8 gaxs(10),gvecx(3)
c
      integer*4 inc,ifl
c
      real*8 rlin(6),rmch(3,4),rrot(20,2),rvec(3),rmat(12),org(3)
c
c...Initialize routine
c
      kerr   = 0
      ifl    = 1
c
c...Second time here
c...Use previously calculated matrix
c
      if (kwhen .eq. 3) then
          call mxtomx (XFMMAT,rmat)
c
c...Calculate XYZIJK positions from axes
c
      else
          call alladr (gaxs,rlin,rmch,rrot,rvec,5,4)
          if (IJKROT .eq. 1) then
              call copyn (TLVEC,rvec,3)
          else
              if (NOTABS .eq. 1) call pivvec (rrot,rvec)
          endif
c
c...Calculate new matrix based on rotary axes
c
          if (XFMFL(3) .eq. 4 .and. XFMFL(12) .eq. 1) then
              if (LASTAB .ge. IRTNUM .and. XFMFL(11) .eq. 2) then
                  call mxident (rmat)
              else
                  inc    = LASTAB + 1
                  inum = IRTNUM - LASTAB
                  if (XFMFL(11) .eq. 1) then
                      inc    = 1
                      inum   = IRTNUM
                  endif
                  if (kfl(1) .eq. 1) then
                      org(1) = rmch(1,4)
                      org(2) = rmch(2,4)
                      org(3) = rmch(3,4)
                  else
                      org(1) = 0.
                      org(2) = 0.
                      org(3) = 0.
                  endif
                  call mataxs (3-XFMFL(17),org,rrot(inc,1),IRTWRK(inc),
     1                         inum,rmat)
c     1                     IRTNUM,rmat)
              endif
c
c...Calculate new matrix based on new ZX plane (Pt-Ve-Ve)
c
          else
              call matpvv (kfl,rmch(1,4),rvec,gvecx,rmat,kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c...If matrix does not contain rotation then
c...cancel transformations
c
          call dpoint (rmat(1),org(1),6)
          call dpoint (rmat(6),org(2),6)
          call dpoint (rmat(11),org(3),6)
          if (org(1) .eq. 1. .and. org(2) .eq. 1. .and. org(3) .eq. 1)
     1        ifl = 2

c
c...Store current axis position
c
          if (ifl .eq. 1) call axsxfr (gaxs,XFMSTO,10)
      endif
c
c...Output matrix
c
      call traout (rmat,ifl,kwhen)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: traoff
c
c   FUNCTION:  This routine cancels a transformation block.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine traoff
c
      include 'post.inc'
c
      equivalence (XFMMAT,POSMAP(4025))
c
      real*8 XFMMAT(12)
c
      call traout (XFMMAT,2,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: traout (gmat,kfl,kwhen)
c
c   FUNCTION:  This routine outputs a transformation block.
c
c   INPUT:  gmat    R*8  D3.4  -  Matrix to output.
c
c           kfl     I*4  D1    -  1 = Output matrix block.  2 = Output
c                                 cancel matrix block.
c
c           kwhen   I*4  D1    -  1 = Only time this Macro is called,
c                                 2 = Called prior to motion block,
c                                 3 = Called after motion block.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine traout (gmat,kfl,kwhen)
c
      include 'post.inc'
c
      integer*4 kfl,kwhen
c
      real*8 gmat(4,3)
c
      equivalence (SIMACT,KPOSMP(0174)), (ICYCSW,KPOSMP(0271))
      equivalence (XFMREG,KPOSMP(0931)), (XFMCD ,KPOSMP(0946))
      equivalence (XFMFL ,KPOSMP(0969)), (FRMFLG,KPOSMP(1211))
      equivalence (IRTACT,KPOSMP(1256)), (NROT  ,KPOSMP(1366))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRTWRK,KPOSMP(1506)), (ICUTDO,KPOSMP(3301))
c
      integer*4 XFMFL(20),XFMCD(5),XFMREG(10),FRMFLG,SIMACT,ICUTDO(15),
     1          ICYCSW(5),IRTWRK(20),IRTACT(2),NROT,IRTINC(4)
c
      equivalence (XFMVL ,POSMAP(1189)), (AXSOUT,POSMAP(1340))
      equivalence (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425)), (XFMMAT,POSMAP(4025))
      equivalence (XFMSTO,POSMAP(4037)), (ROTSTO,POSMAP(5213))
c
      real*8 XFMVL(5),XFMMAT(12),LINSTO(6),ROTSTO(20,2),AXSSTO(10),
     1       AXSOUT(10),XFMSTO(10)
c
      integer*4 i,j,ierr,isav,ifl,isw,idir,iend,ip1
c
      real*8 rot(4),rtrans(3),vec1(3),rnum,rnum1,mat(4,3),tmat(4,3),sgn
c
c...Don't output transformation block(s)
c
      if (XFMFL(1) .ne. 1) go to 8000
      isw   = 0
c
c...Cancel Transformation
c
      if (kfl .eq. 2) then
          if (SIMACT .eq. 1) then
              call mxident (XFMMAT)
              go to 8000
          endif
          if (XFMFL(4) .eq. 0) go to 8000
c
c......Call XFORM Macro
c
          call tramac (0,rtrans,vec1,rot,kwhen,ifl)
c
c......Output XFORM/OFF block
c
          if (ifl .eq. 0) then
              call clrbuf
              if (XFMFL(4) .eq. 1 .or. XFMFL(4) .eq. 3 .or.
     1            (XFMCD(3) .eq. XFMCD(4) .and.
     2             XFMVL(3) .eq. XFMVL(4))) then
                  call codout (XFMCD(3),XFMVL(3))
                  if (XFMFL(6) .eq. 1) then
                      call codout (XFMREG(1),0.d0)
                      call codout (XFMREG(2),0.d0)
                      call codout (XFMREG(3),0.d0)
                  endif
                  call clrbuf
              endif
              if ((XFMCD(3) .ne. XFMCD(4) .or.
     1             XFMVL(3) .ne. XFMVL(4)) .and.
     2            (XFMFL(4) .eq. 2 .or. XFMFL(4) .eq. 3)) then
                  call codout (XFMCD(4),XFMVL(4))
                  call clrbuf
              endif
          endif
          XFMFL(4) = 0
          call mxident (XFMMAT)
c
c...Enable transformation
c
      else
          if (SIMACT .eq. 1) then
              call mxtomx (gmat,XFMMAT)
              go to 8000
           endif
c
c...Cancel cutcom if active and option is set
c
          isw    = ICUTDO(2)
          if (XFMFL(9) .ne. 1) isw = 0
          if (isw .eq. 1) call cutoff
c
c...Combine matrix with
c...Currently active matrix
c
          if (XFMFL(2) .eq. 1 .and. XFMFL(4) .ne. 0) then
              call mxinv (XFMMAT,tmat,ierr)
              call mxtmmx (tmat,gmat,mat)
          else
              call mxtomx (gmat,mat)
          endif
c
c...Calculate rotations and translations
c...from matrix
c
          idir   = 1
          if (XFMFL(3) .eq. 2 .or. XFMFL(3) .eq. 3) idir = XFMFL(16)
          if (XFMFL(3) .eq. 5) then
	           call mxerot (mat,rot,rtrans,XFMFL(17),ierr)
              if (ierr .eq. 1) call psterr (1,'BADMX',' ',0)
          else
              call mxirot (mat,rot,rtrans,idir,ierr)
              if (XFMFL(17) .eq. 1) then
                  rot(1) = rot(1) * -1.
                  rot(2) = rot(2) * -1.
                  rot(3) = rot(3) * -1.
              endif
          endif
          call mxvrot (mat,vec1,rot(4),rtrans,ierr)
          if (XFMFL(17) .eq. 1) rot(4) = rot(4) * -1.
c
c...Calculate rotations from rotary axes output
c
          if (XFMFL(3) .eq. 4) then
              rot(1) = 0.
              rot(2) = 0.
              rot(3) = 0.
              sgn = 1.
              if (XFMFL(17) .eq. 2) sgn = -1.
              do 50 i=1,NROT,1
                  ip1   = IRTINC(IRTACT(i))
                  if (XFMFL(2) .eq. 1) then
                      rot(IRTWRK(ip1)) = AXSOUT(ip1+6) * sgn
                  else
                      rot(IRTWRK(ip1)) = XFMSTO(ip1+6) * sgn
                  endif
   50         continue
          endif
c
c...Determine if translation and/or rotation blocks
c...will be output
c
          rnum   = dabs(rtrans(1))+dabs(rtrans(2))+dabs(rtrans(3))
          rnum1  = dabs(rot(1)) + dabs(rot(2)) + dabs(rot(3))
          call dpoint (rnum,rnum,4)
          call dpoint (rnum1,rnum1,4)
          if (rnum .ne. 0.) then
              if (XFMFL(4) .eq. 0) XFMFL(4) = 1
              if (XFMFL(4) .eq. 2) XFMFL(4) = 3
          endif
          if (rnum1 .ne. 0.) then
              if (XFMFL(4) .eq. 0) XFMFL(4) = 2
              if (XFMFL(4) .eq. 1) XFMFL(4) = 3
          endif
c
c...Call XFORM Macro
c
          call tramac (1,rtrans,vec1,rot,kwhen,ifl)
          if (ifl .eq. 1) go to 1200
c
c...Prepare for output
c
          if (XFMFL(3) .gt. 1) then
              vec1(1) = 0.
              vec1(2) = 0.
              vec1(3) = 0.
          else
              rot(1) = rot(4)
              rot(2) = 0.
              rot(3) = 0.
          endif
c
c...Output translation block
c
          call clrbuf
          iend   = 1
          if (XFMFL(3) .eq. 3) iend = 3
          do 1000 j=1,iend,1
              if (XFMFL(3) .eq. 3) then
                  rnum   = dabs(rtrans(1)) + dabs(rtrans(2)) +
     1                     dabs(rtrans(3))
                  rnum1  = dabs(rot(j))
                  call dpoint (rnum,rnum,4)
                  call dpoint (rnum1,rnum1,4)
              endif
              if (XFMFL(3) .eq. 1 .or. rnum .ne. 0 .or.
     1            (XFMCD(1) .eq. XFMCD(2) .and. XFMVL(1) .eq. XFMVL(2)
     2             .and. rnum1 .ne. 0.)) then
                  call codout (XFMCD(1),XFMVL(1))
                  do 100 i=1,3,1
                      call codout (XFMREG(i),rtrans(i))
  100             continue
                  if (XFMCD(1) .ne. XFMCD(2) .or.
     1                XFMVL(1) .ne. XFMVL(2)) call clrbuf
              endif
c
c...Output rotation block
c
              if (XFMFL(3) .eq. 1 .or. rnum1 .ne. 0.) then
                  if (XFMCD(1) .ne. XFMCD(2) .or.
     1                XFMVL(1) .ne. XFMVL(2))
     2                    call codout (XFMCD(2),XFMVL(2))
c
c......Rotation about major axes
c
                  if (XFMFL(3) .ge. 2) then
                      do 200 i=1,3,1
                          if (XFMFL(3) .ne. 5) rot(i) = -rot(i)
                          call codout (XFMREG(i+3),rot(i))
  200                 continue
                      call codout (XFMCD(5),XFMVL(5))
c
c......Rotation about a vector
c
                  else
                      if (XFMFL(3) .eq. 3) vec1(j) = 1.
                      do 300 i=1,3,1
                          call codout (XFMREG(i+6),vec1(i))
  300                 continue
                      rot(j) = -rot(j)
                      call codout (XFMREG(10),rot(j))
                      vec1(j) = 0.
                  endif
                  call codout (XFMCD(5),XFMVL(5))
                  call clrbuf
              endif
              rtrans(1) = 0.
              rtrans(2) = 0.
              rtrans(3) = 0.
 1000     continue
c
c...Set the active transformation matrix
c
 1200     call mxtomx (gmat,XFMMAT)
      endif
c
c...Adjust current machine position
c...to new matrix
c
      call axsadj (LINSTO,ROTSTO(1,2),AXSSTO)
      isav   = FRMFLG
      FRMFLG = 2
      call from (AXSSTO,ifl)
      if (ICYCSW(1) .ne. 1) call frcg01
      if (isw .eq. 1) call cuton
      FRMFLG = isav
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: tramac (ktype,gtrans,gvec,grot,kwhen,kfl)
c
c   FUNCTION:  This routine calls and processes the XFORM Macro if it
c              exists.  The XFORM Macro will replace the transformation
c              block output.
c
c   INPUT:  ktype   I*4  D1    -  0 = XFORM/OFF output, 1 = XFORM/ON output.
c
c           gtrans  R*8  D3    -  XYZ translations.
c
c           gvec    R*8  D3    -  Rotation vector.
c
c           grot    R*8  D4    -  XYZ axes rotations (1:3) and single
c                                 vector rotation (4).
c
c           kwhen   I*4  D1    -  1 = Only time this Macro is called,
c                                 2 = Called prior to motion block,
c                                 3 = Called after motion block.
c
c   OUTPUT: kfl     I*4  D1    -  1 = XFORM Macro exists and was called.
c
c***********************************************************************
c
      subroutine tramac (ktype,gtrans,gvec,grot,kwhen,kfl)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (IPSTWD,KPOSMP(0006)), (LSTPC ,KPOSMP(0083))
c
      integer*4 ITYPE,ISUBT,IPSTWD(50),LSTPC,MXCL
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      equivalence (LPSTWD,CPOSMP(0217))
c
      character*512 LPSTWD
c
      integer*4 ktype,kfl,kwhen
c
      real*8 gtrans(3),gvec(3),grot(4)
c
      integer*4 ierr,isav(2),ifl,mxsv,i,ipsav(50),impt,ilst
c
      real*8 psav(50)
c
      character*80 msg
      character*512 lsav
c
c...Call XFORM Macro
c
      isav(1) = ITYPE
      isav(2) = ISUBT
      mxsv   = MXCL
      do 50 i=1,mxsv,1
          ipsav(i) = IPSTWD(i)
          psav(i)  = PSTWD(i)
   50 continue
      ITYPE  = 2000
      ISUBT  = 4065
      PSTWD(1) = ktype
      if (ktype .eq. 1) PSTWD(1) = kwhen
      IPSTWD(1) = 0
      if (ktype .eq. 0) then
          MXCL   = 1
      else
          MXCL   = 11
          do 75 i=1,3,1
              PSTWD(i+1) = gtrans(i)
              IPSTWD(i+1) = 0
              PSTWD(i+4) = gvec(i)
              IPSTWD(i+4) = 0
              PSTWD(i+7) = -grot(i)
              IPSTWD(i+7) = 0
   75     continue
          PSTWD(11) = -grot(4)
          IPSTWD(11) = 0
      endif
      call ppcall (ifl,msg,ierr)
      if (ierr .lt. 0) go to 9100
c
c......Process XFORM Macro
c
      kfl   = 0
      if (ifl .ne. 0) go to 8000
      kfl    = 1
      impt   = IMACPT
  100     call precmp (msg,ierr)
          if (ierr .lt. 0) go to 9100
          if (ierr .ne. 0) then
              call lsterr (msg,msg,ierr)
              if (ierr .ne. 0) go to 9100
              if (IMACPT .eq. impt) go to 100
          else
c
c.........PREGEN or Text Output post command
c
              if (IMACPT .eq. impt .and. ITYPE .eq. 2000) then
                  call psword (ifl,ilst,msg,ierr)
                  if (ierr .ne. 0) then
                      call lsterr (msg,msg,ierr)
                      if (ierr .ne. 0) go to 9100
                      go to 100
                  else if (ifl .eq. 0) then
                      go to 100
c
c........Process special commands that are
c........supported for the XFORM Macro
c............ROTABL/ADJUST
c
                  else if (ISUBT .eq. 1026 .and. IPSTWD(1) .eq. 159)
     1                    then
                      call rotabl
                      go to 100
c
c...........TRANS
c
                  else if (ISUBT .eq. 1037) then
                      if (IPSTWD(1) .ne. 617 .or. (MXCL .eq. 2 .and.
     1                   (IPSTWD(2) .eq. 71 .or. IPSTWD(2) .eq. 72)))
     2                        then
                          call trans
                          go to 100
c
c..............Invalid nesting of post command
c
                      else
                          call errtxt ('NESTCALL',msg)
                          call lsterr (msg,msg,ierr)
                          if (ierr .ne. 0) go to 9100
                          go to 100
                      endif
c
c............POSCAL
c
                  else if (ISUBT .eq. 1109) then
                      call poscal
                      go to 100
c
c...........Invalid nesting of post command
c
                  else
                      call errtxt ('NESTCALL',msg)
                      call lsterr (msg,msg,ierr)
                      if (ierr .ne. 0) go to 9100
                      go to 100
                  endif
c
c........Invalid nesting of post commands
c
              else if (IMACPT .eq. impt) then
                  call errtxt ('NESTCALL',msg)
                  call lsterr (msg,msg,ierr)
                  if (ierr .ne. 0) go to 9100
                  go to 100
c
c.........End of XFORM Macro
c
              else
                  IPC    = LSTPC
              endif
          endif
c
c...Restore post words
c
  200 ITYPE  = isav(1)
      ISUBT  = isav(2)
      LPSTWD = lsav
      MXCL   = mxsv
      do 300 i=1,MXCL,1
          IPSTWD(i) = ipsav(i)
          PSTWD(i)  = psav(i)
  300 continue
c
c...End of routine
c
 8000 return
c
c...An I/O error occurred
c
 9100 call errkil (msg,ierr)
      end
