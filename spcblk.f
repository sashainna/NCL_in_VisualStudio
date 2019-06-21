c
c***********************************************************************
c
c     FILE NAME: spcblk.f
c
c     CONTAINS:  spcblk  fndreg  delreg  axsreg  deregs
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        spcblk.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/12/15 , 17:49:27
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  spcblk (kblk,kregs,gvals,knreg,kfmod)
c
c   FUNCTION: This routine handles the following special type of blocks.
c             Any codes output due to belonging to a special block will
c             be removed from the register list.
c
c                CUTCOM/ADJUST          PBLK_FOFFSET
c                DELAY                  PBLK_DWELL
c                GOHOME                 PBLK_GOHOME
c                POSTN                  PBLK_POSTN
c                TOOLNO/ADJUST          PBLK_TOFFSET
c
c   INPUT:  kblk     I*4  D1  - Block type.
c
c           kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c***********************************************************************
c
      subroutine spcblk (kblk,kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      integer*4 kblk,kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (REGSW ,KPOSMP(0405)), (XFMFL ,KPOSMP(0969))
      equivalence (XFMREG,KPOSMP(0931)), (XFMCD ,KPOSMP(0946))
      equivalence (NUMLIN,KPOSMP(1202)), (FRMFLG,KPOSMP(1211))
      equivalence (TLOCD ,KPOSMP(1860)), (FMTDES,KPOSMP(2133))
      equivalence (IFOTYP,KPOSMP(3151))
      equivalence (CUTCCD,KPOSMP(3271)), (DELYCD,KPOSMP(3351))
      equivalence (DELYFL,KPOSMP(3356)), (HOMCOD,KPOSMP(3361))
      equivalence (HOMREG,KPOSMP(3366)), (PSTNCD,KPOSMP(3376))
      equivalence (PSTNRG,KPOSMP(3381))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 TLOCD(8),CUTCCD(30),DELYCD(5),DELYFL(5),HOMCOD(5),
     1          HOMREG(10),NUMLIN(3),PSTNCD(5),PSTNRG(15),
     2          REGSW(MAXFMT),IFOTYP,XFMFL(20),XFMCD(5),XFMREG(10),
     3          FRMFLG
c
      equivalence (DUMMY ,POSMAP(0003)), (DELYVL,POSMAP(0165))
      equivalence (DELYTM,POSMAP(0166)), (HOMCDV,POSMAP(0167))
      equivalence (PSTNCV,POSMAP(0172)), (XFMVL ,POSMAP(1189))
      equivalence (AXSOUT,POSMAP(1340)), (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425))
      equivalence (CUTCVR,POSMAP(2402)), (CUTCVL,POSMAP(2410))
      equivalence (TLOVL ,POSMAP(3983)), (TLOBAS,POSMAP(3988))
      equivalence (XFMMAT,POSMAP(4025)), (XFMSTO,POSMAP(4037))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 TLOVL(8),CUTCVL(30),TLOBAS,CUTCVR(8),DUMMY,DELYVL,DELYTM,
     1       HOMCDV(5),PSTNCV(5),XFMVL(5),XFMMAT(12),LINSTO(6),
     2       ROTSTO(20,2),AXSSTO(10),XFMSTO(10),AXSOUT(10)
c
      integer*4 nc,inc,icd(6),iwrd,i,vtyp(20),vwds(20),inum,itrans,irot,
     1          isimo,isimf,idid,ifl,isav,ierr,ipt,iary(4)
      integer*4 vtlno,vadj,vcutc,vax(4),vplus,vminus,vdelay,vghome,
     1          vghmod(4),vrev,vpostn,vincr,vnorm,vtrans
c
      real*8 rvl(3),rnum,bnum,vval(20),trv(3),rot(3),vec(3),xmat(4,3),
     1       mat(4,3)
c
      character*80 vtxt,msg
c
      data vadj /159/, vax /84,85,86,132/, vcutc /1007/, vdelay /1010/
      data vghmod /1023,88,4019,162/, vghome /17/, vincr /66/
      data vminus /10/, vnorm /111/, vplus /19/, vpostn /1024/
      data vrev /97/, vtlno /1025/, vtrans /1037/
c
c...CUTCOM/ADJUST
c...TOOLNO
c
      if (kblk .eq. PBLK_FOFFSET .or. kblk .eq. PBLK_TOFFSET) then
c
c......CUTCOM/ADJUST
c
          if (kblk .eq. PBLK_FOFFSET) then
              iwrd   = vcutc
              bnum   = CUTCVR(4)
              icd(1) = CUTCCD(19)
              rvl(1) = CUTCVL(19)
              icd(2) = CUTCCD(20)
              rvl(2) = CUTCVL(20)
              icd(3) = CUTCCD(21)
              icd(4) = CUTCCD(22)
              icd(5) = CUTCCD(23)
              icd(6) = CUTCCD(16)
              ipt    = 1
              iary(3) = -1
              iary(4) = -1
c
c......TOOLNO/ADJUST
c
          else
              iwrd   = vtlno
              bnum   = TLOBAS
              icd(1) = TLOCD(4)
              rvl(1) = TLOVL(4)
              icd(2) = TLOCD(5)
              rvl(2) = TLOVL(5)
              icd(3) = TLOCD(6)
              icd(4) = TLOCD(7)
              icd(5) = TLOCD(8)
              icd(6) = TLOCD(1)
              ipt    = 3
              iary(1) = -1
              iary(2) = -1
          endif
c
c......Find PLUS/MINUS
c
          inum   = vplus
          iary(ipt) = 1
          call fndreg (kregs,gvals,knreg,icd(1),rvl(1),inc)
          if (inc .eq. 0) then
              call fndreg (kregs,gvals,knreg,icd(2),rvl(2),inc)
              if (inc .eq. 0) go to 8000
              inum   = vminus
              iary(ipt) = 2
          endif
          call delreg (kregs,gvals,knreg,inc)
c
c......Setup main command syntax
c
          vtyp(1) = 1
          vwds(1) = iwrd
          vtyp(2) = 1
          vwds(2) = vadj
          vtyp(3) = 1
          vwds(3) = inum
          nc     = 3
c
c......Look for offset register
c
          iary(ipt+1) = 0
          call fndreg (kregs,gvals,knreg,icd(6),DUMMY,inc)
          if (inc .ne. 0) then
              nc     = nc    + 1
              vtyp(nc) = 2
              vval(nc) = gvals(inc) - bnum
              call delreg (kregs,gvals,knreg,inc)
              iary(ipt+1) = vval(nc)
          endif
c
c......Look for XAXIS,YAXIS,ZAXIS
c
          i     = 1
  100     if (i .gt. knreg) go to 200
          if (kregs(i) .eq. icd(3)) then
              inc    = 1
          else if (kregs(i) .eq. icd(4)) then
              inc    = 2
          else if (kregs(i) .eq. icd(5)) then
              inc    = 3
          else
              i      = i      + 1
              go to 100
          endif
          nc     = nc    + 1
          vtyp(nc) = 1
          vwds(nc) = vax(inc)
          nc     = nc    + 1
          vtyp(nc) = 2
          vval(nc) = vax(inc)
          call delreg (kregs,gvals,knreg,i)
          go to 100
c
c......Output command
c
  200     if (PCNV_TYPE .eq. PCNV_APTSRC) then
              call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
          else if (PCNV_TYPE .eq. PCNV_SIMUL) then
              call simofs (iary(1),iary(2),iary(3),iary(4),msg,ierr)
          endif
c
c...DELAY
c
      else if (kblk .eq. PBLK_DWELL) then
c
c......Get Delay code and register
c
          call fndreg (kregs,gvals,knreg,DELYCD(1),DELYVL,inc)
          if (inc .eq. 0) go to 8000
          call delreg (kregs,gvals,knreg,inc)
          inum   = 0
          call fndreg (kregs,gvals,knreg,DELYCD(2),DUMMY,inc)
          if (inc .ne. 0) then
              inum   = 1
          else
              call fndreg (kregs,gvals,knreg,DELYCD(3),DUMMY,inc)
              if (inc .ne. 0) inum = 2
          endif
          if (inc .ne. 0) then
              rnum   = gvals(inc)
              call delreg (kregs,gvals,knreg,inc)
          endif
          if (inum .eq. 0 .and. DELYFL(1) .ne. 5) go to 8000
          if (DELYFL(3) .eq. 1) then
              call fndreg (kregs,gvals,knreg,-4,DUMMY,i)
              call delreg (kregs,gvals,knreg,i)
          endif
c
c......Calculate delay value
c
          if (DELYFL(1) .eq. 1) inum = 1
          if (DELYFL(1) .eq. 2) inum = 2
          if (DELYFL(1) .eq. 3) then
              inum   = 1
              if (IFOTYP .eq. 2 .or. IFOTYP .eq. -2) inum = 2
          else if (DELYFL(1) .eq. 5) then
              rnum   = DELYTM
              inc    = 61
          else if (DELYFL(1) .eq. 6) then
              inum   = 1
              if (rnum .ne. 0.) rnum   = 1. / rnum
          endif
          if (inum .eq. 1) then
              if (DELYFL(2) .eq. 1) rnum = rnum / 1000.
              if (DELYFL(2) .eq. 3) rnum = rnum * 60.
          endif
c
c......Output DELAY command
c
          vtyp(1) = 1
          vwds(1) = vdelay
          vtyp(2) = 2
          vval(2) = rnum
          nc     = 2
          if (inum .eq. 2) then
              nc     = nc     + 1
              vtyp(nc) = 1
              vwds(nc) = vrev
          endif
          call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
c
c...GOHOME
c
      else if (kblk .eq. PBLK_GOHOME) then
c
c......Get GOHOME mode
c
          do 600 i=1,4,1
              call fndreg (kregs,gvals,knreg,HOMCOD(i),HOMCDV(i),inc)
              if (inc .ne. 0) then
                  inum   = i
                  call delreg (kregs,gvals,knreg,inc)
                  go to 650
              endif
  600     continue
          go to 8000
c
c......Setup main command syntax
c
  650     vtyp(1) = 1
          vwds(1) = vghome
          vtyp(2) = 1
          vwds(2) = vghmod(inum)
          nc     = 2
c
c......Look for axes
c
          call axsreg (kregs,gvals,knreg,HOMREG,vtyp,vwds,vval,nc)
c
c......Output command
c
          call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
c
c...POSTN
c
      else if (kblk .eq. PBLK_POSTN) then
c
c......Get POSTN mode
c

          do 700 i=1,2,1
              call fndreg (kregs,gvals,knreg,PSTNCD(i),PSTNCV(i),inc)
              if (inc .ne. 0) then
                  inum   = i
                  call delreg (kregs,gvals,knreg,inc)
                  go to 750
              endif
  700     continue
          go to 8000
c
c......Setup main command syntax
c
  750     vtyp(1) = 1
          vwds(1) = vpostn
          vtyp(2) = 1
          if (inum .eq. 1) then
              vwds(2) = vnorm
          else
              vwds(2) = vincr
          endif
          nc     = 2
c
c......Look for axes
c
          call axsreg (kregs,gvals,knreg,PSTNRG,vtyp,vwds,vval,nc)
c
c......Output command
c
          call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
c
c...TRANS
c
      else if (kblk .eq. PBLK_XFORM) then
          itrans = 0
          irot   = 0
c
c......Determine if TRANS & Rotate codes are the same
c
          isimo  = 0
          isimf  = 0
          if (XFMCD(1) .eq. XFMCD(2) .and. XFMVL(1) .eq. XFMVL(2))
     1        isimo = 1
          if (XFMCD(3) .eq. XFMCD(4) .and. XFMVL(3) .eq. XFMVL(4))
     1        isimf = 1
c
c......Determine block sub-type
c
          call fndreg (kregs,gvals,knreg,XFMCD(1),XFMVL(1),inc)
          if (inc .ne. 0) then
              itrans = 1
              if (isimo .eq. 1) irot = 1
          else
              call fndreg (kregs,gvals,knreg,XFMCD(2),XFMVL(2),inc)
              if (inc .ne. 0) then
                  irot   = 1
              else
                  call fndreg (kregs,gvals,knreg,XFMCD(3),XFMVL(3),inc)
                  if (inc .ne. 0) then
                      itrans = 2
                      if (isimf .eq. 1) irot = 2
                  else
                      call fndreg (kregs,gvals,knreg,XFMCD(4),XFMVL(4),
     1                             inc)
                      if (inc .ne. 0) then
                          irot   = 2
                      else
                          go to 8000
                      endif
                  endif
              endif
          endif
          call delreg (kregs,gvals,knreg,inc)
c
c......Initialize command
c
          vtyp(1) = 1
          vwds(1) = vtrans
          nc     = 1
c
c......Get the matrix parameters
c.........Translations
c
          idid   = 0
          trv(1) = 0.
          trv(2) = 0.
          trv(3) = 0.
          if (itrans .eq. 1) then
              do 800 i=1,3,1
                  call fndreg (kregs,gvals,knreg,XFMREG(i),DUMMY,inc)
                  if (inc .ne. 0) then
                      idid   = 1
                      trv(i) = gvals(inc)
                      call delreg (kregs,gvals,knreg,inc)
                  endif
  800         continue
              if (idid .eq. 0) itrans = 2
          endif
c
c.........Rotations
c
          idid   = 0
          vec(1) = 0.
          vec(2) = 0.
          vec(3) = 0.
          rot(1) = 0.
          rot(2) = 0.
          rot(3) = 0.
          if (irot .eq. 1) then
              do 820 i=1,3,1
                  if (XFMFL(3) .eq. 1) then
                      call fndreg (kregs,gvals,knreg,XFMREG(i+6),DUMMY,
     1                             inc)
                      if (inc .ne. 0) then
                          vec(i) = gvals(inc)
                          call delreg (kregs,gvals,knreg,inc)
                          idid   = 1
                      endif
                  else
                      call fndreg (kregs,gvals,knreg,XFMREG(i+3),DUMMY,
     1                             inc)
                      if (inc .ne. 0) then
                          rot(i) = gvals(inc) * -1.
                          call delreg (kregs,gvals,knreg,inc)
                          idid   = 1
                      endif
                  endif
  820         continue
              if (idid .eq. 0) irot = 2
              if (irot .eq. 1 .and. XFMFL(3) .eq. 1) then
                  call fndreg (kregs,gvals,knreg,XFMREG(10),DUMMY,inc)
                  if (inc .ne. 0) then
                      rot(1) = gvals(inc) * -1.
                      if (rot(1) .eq. 0.) irot = 0
                      call delreg (kregs,gvals,knreg,inc)
                   endif
              endif
              call fndreg (kregs,gvals,knreg,XFMCD(5),XFMVL(5),inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
          endif
c
c......Translation only matrix
c......Get the active rotation matrix
c
          if (XFMFL(2) .eq. 2 .and. itrans .ne. 0 .and.
     1        irot .eq. 0 .and. (XFMFL(4) .eq. 2 .or. XFMFL(4) .eq. 3))
     2            then
              if (XFMFL(3) .eq. 1) then
                  call mxvrot (XFMMAT,rot,rvl,ierr)
                  rot(1) = rot(1) * -1.
              else if (XFMFL(3) .eq. 5) then
                  call mxerot (XFMMAT,rot,tvl,XFMFL(17),ierr)
              else
                  call mxirot (XFMMAT,rot,rvl,XFMFL(16),ierr)
                  if (XFMFL(17) .eq. 1) then
                      rot(1) = rot(1) * -1.
                      rot(2) = rot(2) * -1.
                      rot(3) = rot(3) * -1.
                  endif
              endif
              irot = 1
c
c......Rotation only matrix
c......Get the active translation matrix
c
          else if (XFMFL(2) .eq. 2 .and. itrans .eq. 0 .and.
     1        irot .ne. 0 .and. (XFMFL(4) .eq. 1 .or. XFMFL(4) .eq. 3))
     2            then
              if (XFMFL(3) .eq. 1) then
                  call mxvrot (XFMMAT,rvl,trv,ierr)
              else if (XFMFL(3) .eq. 5) then
                  call mxerot (XFMMAT,rvl,trv,XFMFL(17),ierr)
              else
                  call mxirot (XFMMAT,rvl,trv,XFMFL(16),ierr)
              endif
              itrans = 1
          endif
c
c......Calculate the transformation matrix
c
          if (itrans .eq. 1 .or. irot .eq. 1) then
              if (XFMFL(3) .eq. 1) then
                  if (XFMFL(17) .eq. 1) rot(1) = rot(1) * -1.
                  call rotvmx (trv,vec,rot,xmat)
              else if (XFMFL(3) .eq. 5) then
                  call rotemx (trv,rot,xmat,XFMFL(17))
              else
                  if (XFMFL(17) .eq. 1) then
                      rot(1) = rot(1) * -1.
                      rot(2) = rot(2) * -1.
                      rot(3) = rot(3) * -1.
                  endif
                  call rotimx (trv,rot,xmat,XFMFL(16))
              endif
c
c......Matrix is in effect
c......Merge the matrices
c

              if (XFMFL(2) .eq. 1 .and. XFMFL(4) .ne. 0) then
                  call mxtomx (XFMMAT,mat)
                  call mxtmmx (mat,xmat,XFMMAT)
              else
                  call mxtomx (xmat,XFMMAT)
              endif
c
c......Store current axis position
c
              if (irot .eq. 1) call axsxfr (AXSSTO,XFMSTO,10)
c
c......Set the XFORM flags
c
              if (itrans .and. irot .eq. 1) then
                  XFMFL(4) = 3
              else if (itrans .eq. 1) then
                  XFMFL(4) = 1
              else if (irot .eq. 1) then
                  XFMFL(4) = 2
              endif
c
c.........Build the TRANS/mx command
c
              do 840 i=1,12,1
                  nc     = nc     + 1
                  vtyp(nc) = 2
                  vval(nc) = XFMMAT(i)
  840         continue
c
c......TRANS/NOMORE
c
          else
              nc     = nc     + 1
              vtyp(nc) = 2
              vval(nc) = 0.
              XFMFL(4) = 0
          endif
c
c......Adjust current machine position
c......to new matrix
c
          if (itrans .ne. 0 .or. irot .ne. 0) then
              call axsadj (LINSTO,ROTSTO(1,2),AXSSTO)
              call copyn (AXSSTO,AXSOUT,10)
              isav   = FRMFLG
              FRMFLG = 2
              call from (AXSSTO,ifl)
              FRMFLG = isav
          endif
c
c...Output the TRANS/mx command
c
cc          call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fndreg (kregs,gvals,knreg,kfnd,gfndv,kinc)
c
c   FUNCTION: This routine searches the list of registers for the
c             specified register (kfnd) and its optional value (gfndv).
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c           kfnd     I*4  D1  - Register to search for.  This can be a
c                               logical or physical register.
c
c           gfndv    R*8  D1  - The value to search for in the requested
c                               register (kfnd).  If set to DUMMY, then
c                               the value is not used in the search.
c
c   OUTPUT: kinc     I*4  D1  - Index into 'kregs' of register that
c                               matches the search criteria.  0 if it was
c                               not found.
c
c***********************************************************************
c
      subroutine fndreg (kregs,gvals,knreg,kfnd,gfndv,kinc)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      integer*4 kregs(MAXFMT),knreg,kfnd,kinc
c
      real*8 gvals(MAXFMT),gfndv
c
      equivalence (REGSW ,KPOSMP(0405))
c
      integer*4 REGSW(MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 i,ireg,lreg
c
c...Search for requested register
c
      kinc   = 0
      do 100 i=1,knreg,1
          ireg   = kregs(i)
          lreg   = ireg
          if (kfnd .lt. 0) call reglog (ireg,lreg)
          if (lreg .eq. kfnd .and. REGSW(ireg) .eq. 1 .and.
     2        (gvals(i) .eq. gfndv .or. gfndv .eq. DUMMY)) then
              kinc   = i
              go to 8000
          endif
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  delreg (kregs,gvals,knreg,kinc)
c
c   FUNCTION: This routine deletes a register from a list of registers.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c           kinc     I*4  D1  - Index into register list to delete.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c***********************************************************************
c
      subroutine delreg (kregs,gvals,knreg,kinc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGSW ,KPOSMP(0405))
c
      integer*4 REGSW(MAXFMT)
c
      integer*4 kregs(MAXFMT),knreg,kinc
c
      real*8 gvals(MAXFMT)
c
      integer*4 i
c
c...Delete requested register
c
      if (kinc .gt. knreg .or. kinc .le. 0) go to 8000
      REGSW(kregs(kinc)) = 0
      do 100 i=kinc,knreg-1,1
          kregs(i) = kregs(i+1)
          gvals(i) = gvals(i+1)
  100 continue
      knreg  = knreg  - 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  axsreg (kregs,gvals,knreg,kaxs,ktyp,kwds,gnum,knc)
c
c   FUNCTION: This routine deletes a register from a list of registers.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c           kaxs     I*4  D1  - Axis registers.
c
c           ktyp     I*4  Dn  - Type of parameters in command.
c
c           kwds     I*4  Dn  - List of vocabulary words in command.
c
c           gnum     R*8  Dn  - List of real values in command.
c
c           knc      I*4  D1  - Number of parameters stored in command.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           ktyp     I*4  Dn  - Updated command parameter types.
c
c           kwds     I*4  Dn  - Updated list of vocabulary words.
c
c           gnum     R*8  Dn  - Updated list of real values.
c
c           knc      I*4  D1  - Updated number of parameters.
c
c***********************************************************************
c
      subroutine axsreg (kregs,gvals,knreg,kaxs,ktyp,kwds,gnum,knc)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kregs(MAXFMT),knreg,kaxs(10),ktyp(10),kwds(10),knc
c
      real*8 gvals(MAXFMT),gnum(10)
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTDEF,KPOSMP(1485))
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 IRTDEF,NUMLIN(3)
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 inc,i,iasub(10),iainc(10)
      integer*4 vax(4)
c
      character*1 lval(4)
c
      data iasub /1,2,1,2,1,2,1,2,3,4/, iainc /1,1,2,2,3,3,4,4,4,4/
c
      data vax /84,85,86,132/
c
      data lval /'1','2','3','4'/
c
c......Look for axes
c
      do 100 i=1,10,1
          if ((i .le. 6 .and. iasub(i) .le. NUMLIN(iainc(i))) .or.
     1        (i .gt. 6 .and. iasub(i) .le. IRTDEF)) then
              call fndreg (kregs,gvals,knreg,kaxs(i),DUMMY,inc)
              if (inc .ne. 0) then
                  knc     = knc     + 1
                  ktyp(knc) = 1
                  kwds(knc) = vax(iainc(i))
                  if (iasub(i) .ne. 1) then
                      knc    = knc    + 1
                      ktyp(knc) = 2
                      gnum(knc) = iasub(i)
                  endif
                  knc    = knc    + 1
                  ktyp(knc) = 2
                  gnum(knc) = gvals(inc)
                  call delreg (kregs,gvals,knreg,inc)
              endif
          endif
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  deregs (kregs,gvals,knreg)
c
c   FUNCTION: This routine deletes the registers on the Delete Stack from
c             a list of registers.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers in list.
c
c***********************************************************************
c
      subroutine deregs (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      integer*4 i
c
c...Delete registers on stack
c
      do 100 i=NDELST,1,-1
          call delreg (kregs,gvals,knreg,DELSTK(i))
  100 continue
c
c...End of routine
c
 8000 return
      end
