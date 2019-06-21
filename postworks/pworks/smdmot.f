c
c***********************************************************************
c
c   FILE NAME:  smdmot
c   CONTAINS:
c               smdctl  smdaxs  smdhd2  smdhd3  smdmot  smddis  smdlmt
c               smdgcd  smdout
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        smdmot.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:49:01
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  smdctl
c
c   FUNCTION:  This is the Stringer Drill controlling routine for Motion
c              (Type 5000) records.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine smdctl
c
      include 'post.inc'
c
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (NPT   ,KPOSMP(0059)), (MXPT  ,KPOSMP(0060))
      equivalence (LOOKPT,KPOSMP(0061)), (CYCCOD,KPOSMP(0211))
      equivalence (ICYCSW,KPOSMP(0271)), (IRTINC,KPOSMP(1461))
      equivalence (IRTDEF,KPOSMP(1485))
      equivalence (IRFNLS,KPOSMP(1640)), (IRTNLS,KPOSMP(1641))
      equivalence (IRTFRC,KPOSMP(1777)), (IRAP  ,KPOSMP(3199))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 ISUBT,MXCL,IRAP,MXPT,LOOKPT,ICYCSW(5),CYCCOD(20),
     1          NPT,MTPDYN,IRTFRC,IRFNLS,IRTNLS(4),IRTDEF,IRTINC(4)
c
      equivalence (CLPT  ,POSMAP(0491))
      equivalence (VECNXT,POSMAP(1248)), (MCHNUM,POSMAP(1287))
      equivalence (LINAXS,POSMAP(1299)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (VECSAV,POSMAP(1372))
      equivalence (ROTBAS,POSMAP(1435)), (ROTBSV,POSMAP(2275))
      equivalence (FUZZ4 ,POSMAP(4912))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (ROTNXT,POSMAP(5293))
c
      real*8 CLPT(240),MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),
     1       VECSAV(3),ROTANG(20,2),ROTBAS(4),VECNXT(3),
     2       ROTNXT(20,2),ROTSTO(20,2),FUZZ4,ROTBSV(4)
c
      integer*4 inc,i,ierr
c
c...Loop through CLPT's
c
      inc    = 1
      MXPT   = MXCL
      do 5000 i=1,MXPT,1
          LOOKPT = i
c
c......Setup Machine axes values
c
          call copyn (CLPT(inc),MCHNUM(1,2),3)
          if (NPT .eq. 3) then
             call copyn (VECSAV,TLVEC,3)
          else
             call unitvc (CLPT(inc+3),TLVEC)
          endif
c
c......Calculate rotary angles &
c......Adjusted linear axes
c
          call smdaxs (CLPT,TLVEC,i,ierr)
          inc = inc + NPT
 5000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdaxs (gmch,gvec,kpt,kerr)
c
c   FUNCTION:  This routine accepts as input, an XYZ position and tool
c              axis vector and calculates and outputs a motion block
c              based on the active head(s).
c
c   INPUT:  gmch    R*8  D3   -  Input XYZ position.
c
c           gvec    R*8  D3   -  Tool axis vector.
c
c           kpt     I*4  D1   -  Pointer into CLPT array of point being
c                                passed in.
c
c   OUTPUT: kerr    I*4  D1   -  1 = An error occurred calculating the
c                                the axes positions.  -1 = A warning
c                                occurred, the positions were still
c                                calculated.
c
c***********************************************************************
c
      subroutine smdaxs (gmch,gvec,kpt,kerr)
c
      include 'post.inc'
c
      equivalence (SGMACT,KPOSMP(0996))
c
      integer*4 SGMACT(2)
c
      integer*4 kpt,kerr
c
      real*8 gmch(3),gvec(3)
c
c...Initialize routine
c
      kerr   = 0
c
c...Head #2 is active
c
      if (SGMACT(1) .eq. 2) then
          call smdhd2 (gmch,gvec,kpt,kerr)
          if (kerr .eq. 1) go to 8000
c
c...Head #3 and/or #4 is active
c
      else
          call smdhd3 (gmch,gvec,kpt,kerr)
      endif
c
c...Output motion block
c
      call smdmot
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdhd2 (gmch,gvec,kpt,kerr)
c
c   FUNCTION:  This routine accepts as input, an XYZ position and tool
c              axis vector and calculates the linear and rotary
c              positions of Head 2.
c
c              1st GOTO = Tool end point and forward vector
c              2nd GOTO = Same end point and tool axis vector
c
c   INPUT:  gmch    R*8  D3   -  Input XYZ position.
c
c           gvec    R*8  D3   -  Tool axis vector.
c
c           kpt     I*4  D1   -  Pointer into CLPT array of point being
c                                passed in.
c
c   OUTPUT: kerr    I*4  D1   -  1 = An error occurred calculating the
c                                the axes positions.  -1 = A warning
c                                occurred, the positions were still
c                                calculated.
c
c***********************************************************************
c
      subroutine smdhd2 (gmch,gvec,kpt,kerr)
c
      include 'post.inc'
c
      equivalence (SGMHFL,KPOSMP(0998))
c
      integer*4 SGMHFL(10)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (SGMOFS,POSMAP(5099)), (SGMOUT,POSMAP(5454))
      equivalence (SGMFWD,POSMAP(5544)), (SGMTAX,POSMAP(5547))
c
      real*8 SGMOFS(2),SGMOUT(6,3),RAD,SGMFWD(3),SGMTAX(3)
c
      integer*4 kpt,kerr
c
      integer*4 ierr
c
      real*8 gmch(3),gvec(3)
c
      real*8 p1(3),p2(3),p3(3),p4(3),v1(3),v2(3),vn1(3),vn2(3),vn3(3),
     1       vn4(3),vn5(3),axs(3),dist,ndist,ang,rnum
c
c...Initialize routine
c
      SGMHFL(5) = 0
      call copyn (gmch,p1,3)
      call unitvc (gvec,v1)
c
c...Get 2nd GOTO point &
c...Points are the same &
c...Make sure fwd vector is perpto tlaxis vec
c
      call smdnxt (p2,v2,kpt,ierr)
      if (ierr .ne. 0) go to 9000
c
      if (ndist(p1,p2) .gt. .01) then
          call psterr (1,'SMDNSAME',' ',-1)
          kerr   = -1
      endif
c
      call unitvc (v2,v2)
      call betvec (v1,v2,ang)
      if (ang .lt. 89.99 .or. ang .gt. 90.01) then
          call psterr (1,'SMDNPERP',' ',-1)
          kerr   = -1
      endif
c
c...Calculate A-axis rotation angle
c......Calculate normal to first point vector
c......and Z-axis vector (cross product)
c
      vn1(1) = v1(2)
      vn1(2) = -v1(1)
      vn1(3) = 0.
      call unitvc (vn1,vn1)
c
c......Calculate normal to 'vn1' vector
c......and first point vector
c
      call crosvc (vn1,v1,vn2)
      call unitvc (vn2,vn2)
c
c......Calculate A-axis
c
      rnum = (v2(1)*vn2(1)) + (v2(2)*vn2(2)) + (v2(3)*vn2(3))
      if (rnum .gt. 1.) rnum = 1.
      axs(1) = dacos(rnum) * RAD
      if ((v2(2) .gt. 0 .and. v1(1) .gt. 0) .or.
     1    (v2(2) .lt. 0 .and. v1(1) .lt. 0)) axs(1) = -axs(1)
c
c...Calculate B-axis rotation angle
c......Calculate normal to first point vector
c......and Z-axis vector (cross product)
c
      vn3(1) = -vn1(2)
      vn3(2) = vn1(1)
      vn3(3) = 0.
      call unitvc (vn3,vn3)
c
c......Calculate B-axis
c
      rnum = (v1(1)*vn3(1)) + (v1(2)*vn3(2)) + (v1(3)*vn3(3))
      if (rnum .gt. 1.) rnum = 1.
      axs(2) = dacos(rnum) * RAD
      if (v1(3) .gt. 0.) axs(2) = -axs(2)
c
c...Calculate C-axis
c
      axs(3) = dacos(vn3(1)) * RAD
      if (v1(2) .lt. 0.) axs(3) = -axs(3)
      if (axs(3) .lt. -165.) axs(3) = axs(3) + 360.
c
c...Calculate MCD point
c......Project point onto A-axis plane
c
      p3(1) = p2(1) + (SGMOFS(1)*v2(1))
      p3(2) = p2(2) + (SGMOFS(1)*v2(2))
      p3(3) = p2(3) + (SGMOFS(1)*v2(3))
c
c......Calculate normal vector to first point
c
      call crosvc (v1,v2,vn4)
      call unitvc (vn4,vn4)
c
c......Calculate distance from point to A-axis plane
c
      dist   = dtan(axs(1)/RAD) * SGMOFS(1)
c
c......Calculate point at intersection of
c......A-axis plane and normal vector
c
      p4(1) = p1(1) + (dist*vn4(1))
      p4(2) = p1(2) + (dist*vn4(2))
      p4(3) = p1(3) + (dist*vn4(3))
c
c......Calculate vector between A-axis and
c......intersection point
c
      vn5(1) = p3(1) - p4(1)
      vn5(2) = p3(2) - p4(2)
      vn5(3) = p3(3) - p4(3)
      call unitvc (vn5,vn5)
c
c......Calculate MCD point on A-axis plane
c
      SGMOUT(1,1) = p3(1) - (vn5(1)*SGMOFS(1))
      SGMOUT(2,1) = p3(2) - (vn5(2)*SGMOFS(1))
      SGMOUT(3,1) = p3(3) - (vn5(3)*SGMOFS(1))
c
c...Store rotary axes
c
      SGMOUT(4,1) = axs(1)
      SGMOUT(5,1) = axs(2)
      SGMOUT(6,1) = axs(3)
c
c...Store FWD and Tool axis vectors
c
      call copyn (v1,SGMFWD,3)
      call copyn (v2,SGMTAX,3)
c
c...End of routine
c
 8000 return
c
c...Could not get second point
c
 9000 call psterr (2,'SMDNOPT',' ',-1)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdhd3 (gmch,gvec,kpt,kerr)
c
c   FUNCTION:  This routine accepts as input, an XYZ position and tool
c              axis vector and calculates the linear and rotary
c              positions of Heads 3 and 4.
c
c              Head 3
c              ------
c              1st GOTO = Tool end point and tool axis vector
c
c              Head 4
c              ------
c              1st GOTO = Tool end point and tool axis vector
c
c              Heads 3 & 4
c              -----------
c              1st GOTO = Head 3 Tool end point and tool axis vector
c              2nd GOTO = Head 4 Tool end point and tool axis vector
c
c   INPUT:  gmch    R*8  D3   -  Input XYZ position.
c
c           gvec    R*8  D3   -  Tool axis vector.
c
c           kpt     I*4  D1   -  Pointer into CLPT array of point being
c                                passed in.
c
c   OUTPUT: kerr    I*4  D1   -  1 = An error occurred calculating the
c                                the axes positions.  -1 = A warning
c                                occurred, the positions were still
c                                calculated.
c
c***********************************************************************
c
      subroutine smdhd3 (gmch,gvec,kpt,kerr)
c
      include 'post.inc'
c
      equivalence (SGMACT,KPOSMP(0996))
c
      integer*4 SGMACT(2)
c
      equivalence (RAD   ,POSMAP(0002)), (STONUM,POSMAP(1387))
      equivalence (SGMOFS,POSMAP(5099)), (SGMOUT,POSMAP(5454))
      equivalence (SGMFWD,POSMAP(5544)), (SGMTAX,POSMAP(5547))
      equivalence (SGMTL ,POSMAP(5550))
c
      real*8 SGMOFS(2),SGMOUT(6,3),RAD,SGMFWD(3),SGMTAX(3),SGMTL(2),
     1       STONUM(3,4)
c
      integer*4 kpt,kerr
c
      real*8 gmch(3),gvec(3)
c
      integer*4 ierr
c
      real*8 p3(3),p4(3),v3(3),v4(3),vn3(3),vn4(3),chk
c
c...Initialize routine
c
      call copyn (gmch,p3,3)
      call copyn (gmch,p4,3)
      call unitvc (gvec,v3)
      call unitvc (gvec,v4)
c
c...Get 2nd GOTO point if both heads are active
c
      if (SGMACT(2) .eq. 4) then
          call smdnxt (p4,v4,kpt,ierr)
          if (ierr .ne. 0) go to 9000
          call unitvc (v4,v4)
      endif
c
c...Calculate K-axis and W-axis vectors
c...These are opposite to each other and
c...perpendicular to the Head 2 vectors
c
      call crosvc (SGMFWD,SGMTAX,vn3)
      call unitvc (vn3,vn3)
      call crosvc (SGMTAX,SGMFWD,vn4)
      call unitvc (vn4,vn4)
c
c...Head 3
c......Make sure axes are perpendicular
c
      if (SGMACT(1) .eq. 3) then
          chk    = (vn3(1)*v3(1)) + (vn3(2)*v3(2)) + (vn3(3)*v3(3))
          if (chk .gt. 1.) chk = 1.
          if (chk .lt. -1.) chk = -1.
          chk    = acos(chk) * RAD
          if (chk .lt. 179.9 .or. chk .gt. 180.1) then
              call psterr (1,'SMDNPRP3',' ',-1)
              kerr   = -1
          endif
c
c......Calculate UVW axes
c
          SGMOUT(1,2) = ((STONUM(1,2)*SGMFWD(1)) +
     1                   (STONUM(2,2)*SGMFWD(2)) +
     2                   (STONUM(3,2)*SGMFWD(3))) -
     3                  ((p3(1)*SGMFWD(1)) + (p3(2)*SGMFWD(2)) +
     4                   (p3(3)*SGMFWD(3)))
c
          SGMOUT(2,2) = ((p3(1)*SGMTAX(1)) + (p3(2)*SGMTAX(2)) +
     1                   (p3(3)*SGMTAX(3))) -
     2                  ((STONUM(1,2)*SGMTAX(1)) +
     3                   (STONUM(2,2)*SGMTAX(2)) +
     4                   (STONUM(3,2)*SGMTAX(3)))
c
          SGMOUT(3,2) = SGMOFS(2) - (((STONUM(1,2)*vn3(1)) +
     1                   (STONUM(2,2)*vn3(2)) + (STONUM(3,2)*vn3(3))) -
     2                  ((p3(1)*vn3(1)) + (p3(2)*vn3(2)) +
     3                   (p3(3)*vn3(3)))) - SGMTL(1)
      endif
c
c......Head 4
c
      if (SGMACT(1) .eq. 4 .or. SGMACT(2) .eq. 4) then
          chk    = (vn4(1)*v4(1)) + (vn4(2)*v4(2)) + (vn4(3)*v4(3))
          if (chk .gt. 1.) chk = 1.
          if (chk .lt. -1.) chk = -1.
          chk    = acos(chk) * RAD
          if (chk .lt. 179.9 .or. chk .gt. 180.1) then
              call psterr (1,'SMDNPRP4',' ',-1)
              kerr   = -1
          endif
c
c......Calculate IJK axes
c
          SGMOUT(1,3) = ((STONUM(1,2)*SGMFWD(1)) +
     1                   (STONUM(2,2)*SGMFWD(2)) +
     2                   (STONUM(3,2)*SGMFWD(3))) -
     3                  ((p4(1)*SGMFWD(1)) + (p4(2)*SGMFWD(2)) +
     4                   (p4(3)*SGMFWD(3)))
c
          SGMOUT(2,3) = ((p4(1)*SGMTAX(1)) + (p4(2)*SGMTAX(2)) +
     1                   (p4(3)*SGMTAX(3))) -
     2                  ((STONUM(1,2)*SGMTAX(1)) +
     3                   (STONUM(2,2)*SGMTAX(2)) +
     4                   (STONUM(3,2)*SGMTAX(3)))
c
          SGMOUT(3,3) = SGMOFS(2) - (((STONUM(1,2)*vn4(1)) +
     1                   (STONUM(2,2)*vn4(2)) + (STONUM(3,2)*vn4(3))) -
     2                  ((p4(1)*vn4(1)) + (p4(2)*vn4(2)) +
     3                   (p4(3)*vn4(3)))) - SGMTL(2)
      endif
c
c...End of routine
c
 8000 return
c
c...Could not get second point
c
 9000 call psterr (2,'SMDNOPT',' ',-1)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdmot
c
c   FUNCTION:  This routine controls the output of a motion block for
c              HEAD 2,3,4.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine smdmot
c
      include 'post.inc'
c
      equivalence (SGMACT,KPOSMP(0996)), (IRAP  ,KPOSMP(3199))
c
      integer*4 SGMACT(2),IRAP
c
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (SGMSTO,POSMAP(5101)), (SGMOUT,POSMAP(5454))
c
      real*8 MCHNUM(3,4),STONUM(3,4),TLVEC(3),VECSAV(3),SGMOUT(6,3),
     1       SGMSTO(6,3)
c
      integer*4 i,j
c
c...Calculate machining time and deltas
c
      call smddis
c
c...Check machine limits
c
      call smdlmt
c
c...Output miscellaneous codes
c
      call smdgcd
c
c...Output axes registers
c
      call smdout
c
c...Save current position &
c...Reset rapid flag
c
      IRAP   = 0
      if (SGMACT(1) .eq. 2) then
          do 100 i=1,3,1
              STONUM(i,2) = MCHNUM(i,2)
              VECSAV(i) = TLVEC(i)
  100     continue
      endif
c
      do 300 j=1,3,1
          do 200 i=1,6,1
              SGMSTO(i,j) = SGMOUT(i,j)
  200     continue
  300 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smddis
c
c   FUNCTION:  This routine calculates the travel deltas and machining
c              times for Heads 2,3,4.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine smddis
c
      include 'post.inc'
c
      equivalence (SGMREG,KPOSMP(0951)), (SGMACT,KPOSMP(0996))
      equivalence (IRAP  ,KPOSMP(3199))
c
      integer*4 SGMREG(6,3),SGMACT(2),IRAP
c
      equivalence (MCHDLS,POSMAP(1221)), (MCHNUM,POSMAP(1287))
      equivalence (STONUM,POSMAP(1387)), (MCHDLT,POSMAP(1474))
      equivalence (MCHMIN,POSMAP(1506)), (MCHMAX,POSMAP(1518))
      equivalence (PFEED ,POSMAP(3540)), (MOVTIM,POSMAP(3546))
      equivalence (RAPLMT,POSMAP(3567))
      equivalence (SGMSTO,POSMAP(5101)), (SGMOUT,POSMAP(5454))
      equivalence (SGMDLS,POSMAP(5472)), (SGMDLT,POSMAP(5490))
      equivalence (SGMMIN,POSMAP(5508)), (SGMMAX,POSMAP(5526))
c
      real*8 MCHDLS(3,4),MCHNUM(3,4),STONUM(3,4),MCHDLT(3,4),
     1       MCHMIN(3,4),MCHMAX(3,4),SGMSTO(6,3),SGMOUT(6,3),
     2       SGMDLS(6,3),SGMDLT(6,3),PFEED(4),MOVTIM,SGMMIN(6,3),
     3       SGMMAX(6,3),RAPLMT(10)
c
      integer*4 inc,i,ipt,k
c
      real*8 ndist,mdis,r1,r2
c
c...Calculate machining distances
c
      inc    = SGMACT(1) - 1
      do 100 i=1,3,1
c
c......Tool tip
c
          call codint (SGMREG(i,inc),MCHNUM(i,2),r1,k)
          call codint (SGMREG(i,inc),STONUM(i,2),r2,k)
          mdis   = dabs(r1-r2)
          MCHDLS(i,2) = MCHDLS(i,2) + mdis
          MCHDLT(i,2) = MCHDLT(i,2) + mdis
          if (MCHNUM(i,2) .lt. MCHMIN(i,2)) MCHMIN(i,2) = MCHNUM(i,2)
          if (MCHNUM(i,2) .gt. MCHMAX(i,2)) MCHMAX(i,2) = MCHNUM(i,2)
c
c......Linear axes
c
          call codint (SGMREG(i,inc),SGMOUT(i,inc),r1,k)
          call codint (SGMREG(i,inc),SGMSTO(i,inc),r2,k)
          mdis   = dabs(r1-r2)
          SGMDLS(i,inc) = SGMDLS(i,inc) + mdis
          SGMDLT(i,inc) = SGMDLT(i,inc) + mdis
          if (SGMOUT(i,inc) .lt. SGMMIN(i,inc))
     1          SGMMIN(i,inc) = SGMOUT(i,inc)
          if (SGMOUT(i,inc) .gt. SGMMAX(i,inc))
     1          SGMMAX(i,inc) = SGMOUT(i,inc)
c
c......Rotary axes
c
          if (inc .eq. 1) then
              ipt    = i + 3
              call codint (SGMREG(ipt,inc),SGMOUT(ipt,inc),r1,k)
              call codint (SGMREG(ipt,inc),SGMSTO(ipt,inc),r2,k)
              mdis   = dabs(r1-r2)
              SGMDLS(ipt,inc) = SGMDLS(ipt,inc) + mdis
              SGMDLT(ipt,inc) = SGMDLT(ipt,inc) + mdis
              if (SGMOUT(ipt,inc) .lt. SGMMIN(ipt,inc))
     1              SGMMIN(ipt,inc) = SGMOUT(ipt,inc)
              if (SGMOUT(ipt,inc) .gt. SGMMAX(ipt,inc))
     1              SGMMAX(ipt,inc) = SGMOUT(ipt,inc)
          endif
  100 continue
c
c...Calculate the machining time
c
      mdis   = ndist(MCHNUM(1,2),STONUM(1,2))
      r1     = PFEED(1)
      if (IRAP .eq. 1) r1 = RAPLMT(1)
      if (mdis .eq. 0. .or. r1 .eq. 0.) then
          MOVTIM = .00001
      else
          MOVTIM = mdis / r1
      endif
      call addtim (MOVTIM)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdlmt
c
c   FUNCTION:  This routine checks the Head 2,3,4 axes against the
c              machine limits.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine smdlmt
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (SGMREG,KPOSMP(0951)), (SGMACT,KPOSMP(0996))
      equivalence (REGBNC,KPOSMP(2010))
c
      integer*4 SGMREG(6,3),SGMACT(2),REGBNC(MAXFMT)
c
      equivalence (SGMLIM,POSMAP(5137)), (SGMOUT,POSMAP(5454))
      equivalence (SGMMIN,POSMAP(5508)), (SGMMAX,POSMAP(5526))
c
      real*8 SGMOUT(6,3),SGMLIM(2,6,3),SGMMIN(6,3),SGMMAX(6,3)
c
      equivalence (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
c
      integer*4 inc,i,j,iend,iout,k
c
      real*8 r1
c
      character*1 lax(6)
      character*20 lbuf
      character*80 msg1,msg2
c
      data lax /'X','Y','Z','A','B','C'/
c
c...Check the axes against the limits
c
      iend   = 1
      if (SGMACT(2) .ne. 0) iend = 2
      do 200 j=1,iend,1
          inc    = SGMACT(j) - 1
          do 100 i=1,6,1
              if (i .lt. 4 .or. inc .eq. 1) then
c
c......Check HEAD 2 linear axes within
c......the limit range rather than the
c......physical limits themselves
c
                  iout   = 0
                  if (inc .eq. 1 .and. i .le. 3) then
                      if (SGMMAX(i,2)-SGMMIN(i,2) .gt.
     1                    SGMLIM(2,i,2)-SGMLIM(1,i,2)) iout = 1
c
c......Check the rest of the axes
c......against the actual limits
c
                  else
                      call codint (SGMREG(i,inc),SGMOUT(i,inc),r1,k)
                      if (r1 .lt. SGMLIM(1,i,inc)) iout = -1
                      if (r1 .gt. SGMLIM(2,i,inc)) iout = 1
                  endif
c
c......Lower limit exceeded
c
                  if (iout .eq. -1) then
                      lbuf   = REGST(SGMREG(i,inc))
                      if (SGMREG(i,inc) .eq. 0 .or.
     1                    REGBNC(SGMREG(i,inc)) .eq. 0) then
                          lbuf = lax(i)
                      else
                          lbuf = REGST(SGMREG(1,inc))
                      endif
                      call perrst ('LMTLWR',1,msg1,0,r1,lbuf,3)
                      call perrst ('LMTMSG',1,msg2,0,r1,lbuf,2)
                      call perrst (msg2,2,msg2,0,SGMLIM(1,i,inc),lbuf,2)
                      call psterr (1,msg1,msg2,-1)
c
c......Upper limit
c
                  else if (iout .eq. 1) then
                      lbuf   = REGST(SGMREG(i,inc))
                      if (SGMREG(i,inc) .eq. 0 .or.
     1                    REGBNC(SGMREG(i,inc)) .eq. 0) then
                          lbuf = lax(i)
                      else
                          lbuf = REGST(SGMREG(i,inc))
                      endif
                      call perrst ('LMTUPR',1,msg1,0,r1,lbuf,3)
                      call perrst ('LMTMSG',1,msg2,0,r1,lbuf,2)
                      call perrst (msg2,2,msg2,0,SGMLIM(1,i,inc),lbuf,2)
                      call psterr (1,msg1,msg2,-1)
                  endif
              endif
  100     continue
  200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdgcd
c
c   FUNCTION:  This routine outputs the miscellaneous codes for the
c              Stringer machine when the Head is set to 2,3,4.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine smdgcd
c
      include 'post.inc'
c
      equivalence (MOTCOD,KPOSMP(1240))
      equivalence (NRAPCD,KPOSMP(3190)), (RAPCD ,KPOSMP(3191))
      equivalence (IRAP  ,KPOSMP(3199))
c
      integer*4 IRAP,NRAPCD,RAPCD(5),MOTCOD
c
      equivalence (MOTCDV,POSMAP(1284))
      equivalence (MOVDIS,POSMAP(1570)), (PFEED ,POSMAP(3540))
      equivalence (OFEED ,POSMAP(3560)), (RAPVL ,POSMAP(3577))
c
      real*8 RAPVL(5),MOVDIS(4),OFEED(6),PFEED(4),MOTCDV
c
      integer*4 i,ifl(10)
c
      data ifl /1,0,0,0,0,0,0,0,0,0/
c
c...Rapid mode
c
      if (IRAP .eq. 1) then
          do 100 i=1,NRAPCD,1
              call codout (RAPCD(i),RAPVL(i))
  100     continue
c
c...Feed rate
c
      else
          call codout (MOTCOD,MOTCDV)
          OFEED(1) = PFEED(1)
          OFEED(2) = PFEED(1)
          OFEED(3) = PFEED(1)
          MOVDIS(4) = 0
          call frnout (ifl)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdout
c
c   FUNCTION:  This routine outputs a motion block for Heads 2,3,4.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine smdout
c
      include 'post.inc'
c
      equivalence (REGFRC,KPOSMP(0603))
      equivalence (SGMREG,KPOSMP(0951)), (SGMACT,KPOSMP(0996))
      equivalence (SGMHFL,KPOSMP(0998))
      equivalence (SGMCOD,KPOSMP(1018)), (MOTFLG,KPOSMP(1073))
c
      integer*4 SGMREG(6,3),SGMACT(2),MOTFLG,REGFRC(96),SGMCOD(10),
     1          SGMHFL(10)
c
      equivalence (SGMOUT,POSMAP(5454)), (SGMCDV,POSMAP(5552))
      equivalence (SGMQVL,POSMAP(5562))
c
      real*8 SGMOUT(6,3),SGMCDV(10),SGMQVL
c
      integer*4 inc,i,j,ih3,ih4,ifl,ierr
c
      character*80 msg
c
c...Output the Head axes
c
      MOTFLG = 1
      ih3 = 0
      ih4 = 0
      do 200 j=1,2,1
          if (SGMACT(j) .ne. 0) then
              if (SGMACT(j) .eq. 3) ih3 = 1
              if (SGMACT(j) .eq. 4) ih4 = 1
              inc    = SGMACT(j) - 1
              do 100 i=1,6,1
                  if (i .lt. 4 .or. inc .eq. 1) then
                      call codout (SGMREG(i,inc),SGMOUT(i,inc))
                  endif
  100         continue
          endif
  200 continue
c
c...Output the Q-axis
c
      if (SGMCOD(1) .ne. 0) then
          if (SGMACT(1) .eq. 2 .and. SGMHFL(6) .eq. 1) then
              call cmpcod (SGMCOD(1),SGMCDV(1),0.d0,1,ifl)
              if (SGMHFL(8) .eq. 1 .or. ifl .eq. 1) then
                  REGFRC(SGMCOD(1)) = 3
                  call codout (SGMCOD(1),SGMCDV(1))
              endif
              SGMHFL(6) = 0
              SGMHFL(8) = 0
c
          else if (SGMACT(2) .eq. 4) then
              if (SGMQVL .ne. 999.) then
                  REGFRC(SGMCOD(1)) = 3
                  call codout (SGMCOD(1),SGMQVL)
              endif
          else if ((SGMACT(1) .eq. 3 .and. SGMHFL(3) .eq. 1) .or.
     1        (SGMACT(1) .eq. 4 .and. SGMHFL(4) .eq. 1)) then
              REGFRC(SGMCOD(1)) = 3
              call codout (SGMCOD(1),SGMCDV(1))
              SGMHFL(3) = 0
              SGMHFL(4) = 0
          endif
      endif
      SGMQVL = 999.
c
c...Clear the output buffer
c
      call clrbuf
c
c...Output simulation motion
c
      call simsmd (2,msg,ierr)
c
c...End of routine
c
 8000 return
      end
