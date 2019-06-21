c
c***********************************************************************
c
c     FILE NAME: regfmt.f
c
c     CONTAINS:  reglog  setblk  blktyp  codin   prmin    msgin   regdtm
c                seqblk
c
c             Block types
c             -----------
c             PBLK_AXSIZE        Rotary axis size block
c             PBLK_DWELL         DELAY tape block
c             PBLK_FOFFSET       Fixture offset tape block
c             PBLK_GOHOME        GOHOME tape block
c             PBLK_MOTION        Motion tape block (see subtypes)
c             PBLK_POSTN         POSTN tape block
c             PBLK_TOFFSET       Tool offset tape block
c             PBLK_XFORM         Transformation block
c             PBLK_UNKNOWN       Miscellaneous tape block
c
c             Motion blocks
c             -------------
c             PMOT_CIRCUL        Circular interpolation
c             PMOT_CYCLE         CYCLE block
c             PMOT_LINEAR        Linear motion
c             PMOT_ROTARY        Rotary (and linear) motion
c             PMOT_CYCOFF        CYCLE/OFF block
c             PMOT_CIR3AX        3-axis circular interpolation
c             PMOT_CIRCEN        Circular center point (dual block)
c
c             Modes not supported yet
c             -----------------------
c             Spline interpolation
c             LMDP, LMDP modes
c             Multiple rapid codes
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        regfmt.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 13:37:00
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  reglog (ireg,jreg)
c
c   FUNCTION: Convert a physical register to a logical register.
c
c   INPUT:  kpreg    I*4  D1  - Physical register.
c
c   OUTPUT: klreg    I*4  D1  - Logical register.
c
c***********************************************************************
c
      subroutine reglog (kpreg,klreg)
c
      include 'post.inc'
c
      integer*4 kpreg, klreg
c
      equivalence (MOTREG,KPOSMP(0381)), (NUMLIN,KPOSMP(1202))
      equivalence (IRTDEF,KPOSMP(1485)), (FEDCD ,KPOSMP(3162))
c
      integer*4 MOTREG(24),NUMLIN(3),FEDCD(8),IRTDEF
c
      integer*4 ist(12),inc(12),istt(8),inct(8),lreg(20),i,ipt
c
      data ist /1,1,2,2,1,1,2,2,1,1,2,2/, inc /1,1,1,1,2,2,2,2,3,3,3,3/
      data istt /1,1,2,2,3,3,4,4/, inct /14,15,17,18,20,21,23,24/
c
      data lreg /-4,-4,-5,-5,-6,-6,-7,-7,-8,-8,-9,-9,
     1           -10,-10,-11,-11,-12,-12,-13,-13/
c
c...Initialize routine
c
      klreg  = kpreg
      ipt    = 0
      if (klreg .le. 0) go to 8000
c
c...G-code
c
      if (kpreg .ge. 18 .and. kpreg .le. 28) then
          klreg = -1
          go to 8000
      endif
c
c...M-code
c
      if (kpreg .ge. 37 .and. kpreg .le. 47) then
          klreg = -2
          go to 8000
      endif
c
c...Linear axis register
c
      do 100 i=1,12,1
          ipt    = ipt    + 1
          if (ist(i) .le. NUMLIN(inc(i))) then
              if (kpreg .eq. MOTREG(i)) then
                  klreg = lreg(ipt)
                  go to 8000
              endif
          endif
  100 continue
c
c...Rotary axis register
c
      do 200 i=1,8,1
          ipt    = ipt    + 1
          if (istt(i) .le. IRTDEF) then
              if (kpreg .eq. MOTREG(inct(i))) then
                  klreg = lreg(ipt)
                  go to 8000
              endif
          endif
  200 continue
c
c...Feedrate register
c
      do 300 i=1,8,1
          if (kpreg .eq. FEDCD(i)) klreg = -14
  300 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setblk (cdat,knc,ktyp,kmot,kreg,gval,knreg,kcnv)
c
c   FUNCTION: Sets the block type, motion type and various other modes.
c
c
c   INPUT:  cdat    C*1  Dn  - NC Block.
c
c           knc     I*4  D1  - Length of NC block.
c
c           kcnv    I*4  D1  - Conversion type in process.
c                              1 = APT Source or Simulation File
c                              2 = Tape to Tape
c
c   OUTPUT: ktyp    I*4  D1  - Block type.
c
c           kmot    I*4  D1  - Motion block type.
c
c           kreg    I*4  Dn  - Array of physical registers in this block.
c
c           gval    R*8  Dn  - Values for 'kreg'.
c
c           knreg   I*4  D1  - Number of registers in block.  Returns
c                              -1 if block is bad.
c
c***********************************************************************
c
      subroutine setblk (cdat,knc,ktyp,kmot,kreg,gval,knreg,kcnv)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 knc,ktyp,kmot,kreg(MAXFMT),knreg,kcnv
c
      real*8 gval(MAXFMT)
c
      character*80 cdat
c
      equivalence (ICYCSW,KPOSMP(0271))
      equivalence (REGSW ,KPOSMP(0405)), (REGFRC,KPOSMP(0603))
      equivalence (SEQCOD,KPOSMP(0847))
      equivalence (MACPSW,KPOSMP(0859)), (ICYMTP,KPOSMP(0920))
      equivalence (PRTBNC,KPOSMP(1153)), (INCR  ,KPOSMP(1226))
      equivalence (ABSCOD,KPOSMP(1241)), (INCCOD,KPOSMP(1242))
      equivalence (MCHPLN,KPOSMP(1261))
      equivalence (IFOTYP,KPOSMP(3151)), (FEDMCD,KPOSMP(3170))
      equivalence (PLNCOD,KPOSMP(4222))
c
      integer*4 INCR,ABSCOD,INCCOD,REGSW(MAXFMT),PLNCOD(4),MCHPLN,
     1          FEDMCD(8),IFOTYP,ICYCSW(5),ICYMTP,MACPSW(30),SEQCOD,
     2          PRTBNC,REGFRC(MAXFMT)

      equivalence (DUMMY ,POSMAP(0003))
      equivalence (REGSTO,POSMAP(1032)), (SEQCUR,POSMAP(1183))
      equivalence (ABSCDV,POSMAP(1285)), (INCCDV,POSMAP(1286))
      equivalence (PLNCDV,POSMAP(2208)), (FEDMVL,POSMAP(3320))
      equivalence (FEED  ,POSMAP(3547)), (MACPRM,POSMAP(4300))
c
      real*8 ABSCDV,INCCDV,REGSTO(MAXFMT),PLNCDV(4),FEDMVL(8),SEQCUR,
     1       MACPRM(30),FEED(4),DUMMY
c
      equivalence (PRTBLK,CPOSMP(1653))
c
      character*512 PRTBLK
c
      integer*4 i,i1,ix,ireg,lreg,iend,ierr,nc
c
      real*8 val
c
      character*132 msg
c
c...Initialize routine
c
      ix     = 1
      ktyp   = PBLK_UNKNOWN
      PRTBNC = 0
      NDELST = 0
      if (kmot .eq. PMOT_CYCOFF) kmot = PMOT_LINEAR
      knreg  = 0
c
c...Cycle interrupt was last block
c...Restore CYCLE block type
c
      ICYCSW(2) = 0
      ICYMTP = 0
      if (ICYCSW(1) .lt. 0) then
          ICYCSW(1) = ICYCSW(1) * -1
          kmot   = PMOT_CYCLE
      endif
c
c...Loop through registers
c
      do while (ix .le. knc)
          i1 = knc - ix + 1
c
c...Parse Macro call parameters
c
          if (ICYMTP .ne. 0 .and. cdat(ix:ix) .eq. '(') then
              call prmin (cdat(ix:),i1,MACPSW,MACPRM,iend,ierr)
              if (ierr .eq. 1) go to 9000
          else
c
c...Parse register
c
              call codin (cdat(ix:),i1,1,0,ireg,val,iend,ktyp,kmot)
c
c......Error parsing register
c......Check for message block
c
              if (ireg .eq. -1000) then
                  call msgin (cdat(ix:),i1,msg,nc,iend)
                  if (nc .lt. 0) go to 9000
                  PRTBLK(PRTBNC+1:) = msg(1:nc)
                  if (PRTBNC .eq. 0) then
                      knreg = knreg + 1
                      kreg(knreg) = -1000
                  endif
                  PRTBNC = PRTBNC + nc
                  ix = ix + iend
                  cycle
              endif
c
c...Store register value
c
              knreg  = knreg  + 1
              if (knreg .gt. MAXFMT) go to 9000
              kreg(knreg) = ireg
              gval(knreg) = val
              REGSTO(ireg) = val
c
c...Sequence number
c
              if (ireg .eq. SEQCOD .and. kcnv .eq. 1) then
                  SEQCUR = val
              else
                  REGSW(ireg) = 1
                  REGFRC(ireg) = 1
              endif
c
c...Get block type
c
              call blktyp (ireg,val,lreg,ktyp,kmot)
c
c...Get logical register
c
              call reglog (ireg,lreg)
c
c...Absolute/incremental
c
              if ((ireg .eq. ABSCOD .or. lreg .eq. ABSCOD) .and.
     1            (val .eq. ABSCDV .or. ABSCDV .eq. DUMMY)) then
                  INCR = 1
                  goto 200
              endif
              if ((ireg .eq. INCCOD .or. lreg .eq. INCCOD) .and.
     1            (val .eq. INCCDV .or. INCCDV .eq. DUMMY)) then
                  INCR = 2
                  goto 200
              endif
c
c...Machine plane
c
              do 100 i=1,3,1
                  if ((ireg .eq. PLNCOD(i) .or. lreg .eq. PLNCOD(i))
     1                 .and.  (val .eq. PLNCDV(i) .or.
     2                 PLNCDV(i) .eq. DUMMY)) then
                      MCHPLN = 4 - i
                      if (kcnv .eq. 1) then
                          NDELST = NDELST + 1
                          DELSTK(NDELST) = knreg
                      endif
                      goto 200
                  endif
  100         continue
c
c...Feed rate mode
c
              do 150 i=1,4,1
                  if ((ireg .eq. FEDMCD(i) .or. lreg .eq. FEDMCD(i))
     1                .and. (val .eq. FEDMVL(i) .or.
     2                FEDMVL(i) .eq. DUMMY)) then
                      IFOTYP = i
                      if (kcnv .eq. 1) then
                          NDELST = NDELST + 1
                          DELSTK(NDELST) = knreg
                      endif
                      goto 200
                  endif
  150         continue
c
c...Feed rate register
c
              if (lreg .eq. -14) FEED(1) = gval(knreg)
c
c...Motion block
c
  200         continue
              if (ktyp .eq. PBLK_MOTION .and.
     1            kmot .ne. PMOT_CIRCEN .and. lreg .ge. -13 .and.
     2            lreg .le. -4) then
                  call stomot (ireg,val)
                  if (kcnv .eq. 1) then
                      NDELST = NDELST + 1
                      DELSTK(NDELST) = knreg
                  endif
              endif
          endif
c
c...Point to next register
c
          ix = ix + iend
          do while (ix .le. knc .and. cdat(ix:ix) .eq. ' ')
              ix = ix+1
          enddo
      enddo
c
c...End of routine
c
 8000 continue
      return
c
c...Bad block found
c
 9000 knreg = -1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  blktyp (kpreg,gval,klreg,ktyp,kmot)
c
c   FUNCTION: Determines if this register sets a specific block type.
c
c   INPUT:  kpreg   I*4  D1  - Physical register.
c
c           gval    R*8  D1  - Value stored in register.
c
c           ktyp    I*4  D1  - Current block type.
c
c           kmot    I*4  D1  - Current Motion block type.
c
c   OUTPUT: klreg   I*4  D1  - Logical register.  If there is no appropriate
c                              logical register, then the physical register
c                              number will be returned.
c
c           ktyp    I*4  D1  - Updated block type.
c
c           kmot    I*4  D1  - Updated motion block type.
c
c***********************************************************************
c
      subroutine blktyp (kpreg,gval,klreg,ktyp,kmot)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kpreg,klreg,ktyp,kmot
c
      real*8 gval
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (CYCCOD,KPOSMP(0211)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCSV,KPOSMP(0291)), (MACCOD,KPOSMP(0801))
      equivalence (MACBLK,KPOSMP(0889))
      equivalence (ICYMTP,KPOSMP(0920)), (XFMFL ,KPOSMP(0921))
      equivalence (XFMCD ,KPOSMP(0946)), (ICIPRM,KPOSMP(1230))
      equivalence (MOTCOD,KPOSMP(1240)), (RSIZCD,KPOSMP(1328))
      equivalence (ICRFMT,KPOSMP(1368))
      equivalence (CIRCOD,KPOSMP(1378)), (IC3AXF,KPOSMP(1747))
      equivalence (TLOCD ,KPOSMP(1860)), (ICYBLK,KPOSMP(3060))
      equivalence (RAPCD ,KPOSMP(3191))
      equivalence (CUTCCD,KPOSMP(3271)), (DELYCD,KPOSMP(3351))
      equivalence (HOMCOD,KPOSMP(3361)), (HOMREG,KPOSMP(3366))
      equivalence (PSTNCD,KPOSMP(3376)), (PSTNRG,KPOSMP(3381))
      equivalence (CIRCON,KPOSMP(4242))
c
      integer*4 CUTCCD(30),DELYCD(5),HOMCOD(5),HOMREG(10),TLOCD(8),
     1          MOTCOD,RAPCD(5),CIRCOD(6),CYCCOD(20),PSTNCD(5),
     2          PSTNRG(15),ICIPRM(8),ICYCSW(5),ICYCSV(3),RSIZCD(5),
     3          IC3AXF(10),ICYCFL(30),ICYMTP,MACCOD(5),MACBLK,
     4          ICYBLK(20),XFMFL(5),XFMCD(5),ICRFMT,CIRCON(8)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (DELYVL,POSMAP(0165)), (HOMCDV,POSMAP(0167))
      equivalence (PSTNCV,POSMAP(0172)), (MACCDV,POSMAP(1184))
      equivalence (XFMVL ,POSMAP(1189)), (MOTCDV,POSMAP(1284))
      equivalence (RSIZCV,POSMAP(1600)), (CIRCDV,POSMAP(2206))
      equivalence (CIRCOV,POSMAP(2215)), (C3XCDV,POSMAP(2244))
      equivalence (CUTCVL,POSMAP(2410)), (CYCCDV,POSMAP(2901))
      equivalence (RAPVL ,POSMAP(3577)), (TLOVL ,POSMAP(3983))
c
      real*8 DELYVL,HOMCDV(5),CUTCVL(30),TLOVL(5),MOTCDV,RAPVL(5),
     1       CIRCDV(2),CYCCDV(25),PSTNCV(5),RSIZCV,C3XCDV(2),MACCDV(5),
     2       XFMVL(5),DUMMY,CIRCOV(8)
c
      integer*4 i
c
c...Get logical register
c
      call reglog (kpreg,klreg)
c
c...Determine block type
c...Delay
c
      if ((kpreg .eq. DELYCD(1) .or. klreg .eq. DELYCD(1)) .and.
     1    (gval .eq. DELYVL .or. DELYVL .eq. DUMMY)) then
          ktyp = PBLK_DWELL
c
c...Fixture offset
c
      else if (((kpreg .eq. CUTCCD(19) .or. klreg .eq. CUTCCD(19)) .and.
     1         (gval .eq. CUTCVL(19) .or. CUTCVL(19) .eq. DUMMY)) .or.
     2         ((kpreg .eq. CUTCCD(20) .or. klreg .eq. CUTCCD(20)) .and.
     3         (gval .eq. CUTCVL(20) .or. CUTCVL(20) .eq. DUMMY))) then
          if (CUTCCD(21) .ne. 0 .or. CUTCCD(22) .ne. 0 .or.
     1        CUTCCD(22) .ne. 0) ktyp = PBLK_FOFFSET
c
c...GOHOME
c
      else if (((kpreg .eq. HOMCOD(1) .or. klreg .eq. HOMCOD(1)) .and.
     1         (gval .eq. HOMCDV(1) .or. HOMCDV(1) .eq. DUMMY)) .or.
     2         ((kpreg .eq. HOMCOD(2) .or. klreg .eq. HOMCOD(2)) .and.
     3         (gval .eq. HOMCDV(2) .or. HOMCDV(2) .eq. DUMMY)) .or.
     4         ((kpreg .eq. HOMCOD(3) .or. klreg .eq. HOMCOD(3)) .and.
     5         (gval .eq. HOMCDV(3) .or. HOMCDV(3) .eq. DUMMY)) .or.
     6         ((kpreg .eq. HOMCOD(4) .or. klreg .eq. HOMCOD(4)) .and.
     7         (gval .eq. HOMCDV(4) .or. HOMCDV(4) .eq. DUMMY))) then
          if (HOMREG(1) .ne. 0 .or. HOMREG(3) .ne. 0 .or.
     1        HOMREG(5) .ne. 0) ktyp = PBLK_GOHOME
c
c...POSTN
c
      else if (((kpreg .eq. PSTNCD(1) .or. klreg .eq. PSTNCD(1)) .and.
     1         (gval .eq. PSTNCV(1) .or. PSTNCV(1) .eq. DUMMY)) .or.
     2         ((kpreg .eq. PSTNCD(2) .or. klreg .eq. PSTNCD(2)) .and.
     3         (gval .eq. PSTNCV(2) .or. PSTNCV(2) .eq. DUMMY))) then
          if (PSTNRG(1) .ne. 0 .or. PSTNRG(3) .ne. 0 .or.
     1        PSTNRG(5) .ne. 0) ktyp = PBLK_POSTN
c
c...Tool length offset
c
      else if (((kpreg .eq. TLOCD(4) .or. klreg .eq. TLOCD(4)) .and.
     1         (gval .eq. TLOVL(4) .or. TLOVL(4) .eq. DUMMY)) .or.
     2         ((kpreg .eq. TLOCD(5) .or. klreg .eq. TLOCD(5)) .and.
     3         (gval .eq. TLOVL(5) .or. TLOVL(5) .eq. DUMMY))) then
          if (TLOCD(6) .ne. 0 .or. TLOCD(7) .ne. 0 .or.
     1        TLOCD(8) .ne. 0) ktyp = PBLK_TOFFSET
c
c...Rotary axis size block
c
      else if ((kpreg .eq. RSIZCD(1) .or. klreg .eq. RSIZCD(1)) .and.
     1         (gval .eq. RSIZCV .or. RSIZCV .eq. DUMMY)) then
          ktyp = PBLK_AXSIZE
c
c...Transformation block
c
      else if (XFMFL(1) .eq. 1 .and.
     1         (((kpreg .eq. XFMCD(1) .or. klreg .eq. XFMCD(1)) .and.
     2         (gval .eq. XFMVL(1) .or. XFMVL(1) .eq. DUMMY)) .or.
     3         ((kpreg .eq. XFMCD(2) .or. klreg .eq. XFMCD(2)) .and.
     4         (gval .eq. XFMVL(2) .or. XFMVL(2) .eq. DUMMY)) .or.
     5         ((kpreg .eq. XFMCD(3) .or. klreg .eq. XFMCD(3)) .and.
     6         (gval .eq. XFMVL(3) .or. XFMVL(3) .eq. DUMMY)) .or.
     7         ((kpreg .eq. XFMCD(4) .or. klreg .eq. XFMCD(4)) .and.
     8         (gval .eq. XFMVL(4) .or. XFMVL(4) .eq. DUMMY)))) then
          ktyp = PBLK_XFORM
c
c...Linear motion code
c
      else if (((kpreg .eq. MOTCOD .or. klreg .eq. MOTCOD) .and.
     1         (gval .eq. MOTCDV .or. MOTCDV .eq. DUMMY)) .or.
     2         ((kpreg .eq. RAPCD(1) .or. klreg .eq. RAPCD(1)) .and.
     3         (gval .eq. RAPVL(1) .or. RAPVL(1) .eq. DUMMY)) .and.
     4         (ICYCFL(30) .ne. 1 .or. ICYCSW(1) .eq. 0)) then
          kmot   = PMOT_LINEAR
c
c...Circular motion block
c
      else if ((kpreg .eq. CIRCOD(1) .or. klreg .eq. CIRCOD(1)) .and.
     1         (gval .eq. CIRCDV(1) .or. CIRCDV(1) .eq. DUMMY)) then
          ktyp   = PBLK_MOTION
          kmot   = PMOT_CIRCUL
          ICIPRM(5) = 1
      else if ((kpreg .eq. CIRCOD(2) .or. klreg .eq. CIRCOD(2)) .and.
     3         (gval .eq. CIRCDV(2) .or. CIRCDV(2) .eq. DUMMY)) then
          ktyp   = PBLK_MOTION
          kmot   = PMOT_CIRCUL
          ICIPRM(5) = 2
      else if (ICRFMT .eq. 10 .and.
     1         ((kpreg .eq. CIRCON(2) .or. klreg .eq. CIRCON(2)) .and.
     3         (gval .eq. CIRCOV(2) .or. CIRCOV(2) .eq. DUMMY))) then
          ktyp   = PBLK_MOTION
          kmot   = PMOT_CIRCUL
c
c...3-axis Circular motion block
c
      else if ((kpreg .eq. IC3AXF(3) .or. klreg .eq. IC3AXF(3)) .and.
     1         gval .eq. C3XCDV(1) .and. IC3AXF(1) .eq. 1) then
          ktyp   = PBLK_MOTION
          kmot   = PMOT_CIR3AX
          ICIPRM(5) = 1
      else if ((kpreg .eq. IC3AXF(4) .or. klreg .eq. IC3AXF(4)) .and.
     3         gval .eq. C3XCDV(2) .and. IC3AXF(1) .eq. 1) then
          ktyp   = PBLK_MOTION
          kmot   = PMOT_CIR3AX
          ICIPRM(5) = 2
c
c...Circular center block
c
      else if ((kpreg .eq. CIRCON(1) .or. klreg .eq. CIRCON(1)) .and.
     1         (gval .eq. CIRCOV(1) .or. CIRCOV(1) .eq. DUMMY)) then
          ktyp   = PBLK_MOTION
          kmot   = PMOT_CIRCEN
c
c...Standard motion block
c
      else if (klreg .ge. -13 .and. klreg .le. -4 .and.
     1         (ktyp .eq. PBLK_UNKNOWN .or. ktyp .eq. PBLK_MOTION)) then
          ktyp   = PBLK_MOTION
cc          if (klreg .ge. -13 .and. klreg .le. -10 .and.
cc     1        kmot .eq. PMOT_LINEAR) kmot = PMOT_ROTARY
c
c...Macro Call
c...Assume CYCLE/OFF for now
c
      else if (ICYCFL(30) .eq. 1 .and. kpreg .eq. MACCOD(1) .and.
     1         (gval .eq. MACCDV(1) .or. MACCDV(1) .eq. DUMMY)) then
          ICYMTP = 1
          if (kmot .eq. PMOT_CYCLE .or.
     1        kmot .eq. PMOT_LINEAR) kmot = PMOT_CYCOFF
          ICYCSV(3) = ICYCSW(1)
          ICYCSW(1) = 0
          ICYCSW(2) = 2
c
c...Cycles
c
      else
          do 500 i=1,15,1
              if ((kpreg .eq. CYCCOD(i) .or. klreg .eq. CYCCOD(i)) .and.
     1            (gval .eq. CYCCDV(i) .or. CYCCDV(i) .eq. DUMMY)) then
                  if (i .eq. 1) then
                      if (kmot .eq. PMOT_CYCLE .or.
     1                    kmot .eq. PMOT_LINEAR) kmot = PMOT_CYCOFF
                      ICYCSV(3) = ICYCSW(1)
                      ICYCSW(1) = 0
                      ICYCSW(2) = 2
                  else if (i .eq. 15) then
                      if (kmot .eq. PMOT_CYCLE) kmot = PMOT_LINEAR
                      ICYCSW(1) = ICYCSW(1) * -1
                  else
                      ktyp   = PBLK_MOTION
                      kmot   = PMOT_CYCLE
                      ICYCSW(1) = i      - 1
                      ICYCSW(2) = 1
                      MACBLK = ICYBLK(ICYCSW(1)+1)
                      if (ICYCFL(30) .eq. 1 .and. ICYMTP .eq. 0)
     1                    ICYMTP = 2
                  endif
                  go to 550
              endif
  500     continue
  550     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  codin (cdat,knc,kcase,kcnv,kreg,gval,kend,ktyp,ksub)
c
c   FUNCTION:  This routine finds a register at the start of a string.
c
c   INPUT:    cdat    C*n  D1   -  String which may contain a register.
c
c             knc     I*4  D1   -  Number of characters in 'cdat'.
c
c             kcase   I*4  D1   -  =1 respect case, =0 ignore case.
c
c             kcnv    I*4  D1   -  =1 convert register, =0 don't.
c
c             ktyp    I*4  D1   -  Type of block being parsed.
c
c             ksub    I*4  D1   -  Subtype of block being parsed.
c
c   OUTPUT:   kreg    I*4  D1   -  Register Number or -1000 if no valid
c                                  register found.
c
c             gval    R*8  D1   -  Register value.
c
c             kend    I*4  D1   -  Ending character position of register.
c
c***********************************************************************
c
      subroutine codin (cdat,knc,kcase,kcnv,kreg,gval,kend,ktyp,ksub)
c
      include 'post.inc'
      include 'pted.inc'
      include 'menu.inc'
c
      common/REGCOM/RGBEG, RGNUM, RGTAB, RGINC, REGCNV
c
      integer*4 RGBEG(100),RGNUM(100),RGTAB(MAXFMT),RGINC(MAXFMT),
     1          REGCNV(MAXFMT)
c
      integer*4 kreg, knc, kend, kcase, kcnv,ktyp,ksub
c
      real*8 gval
c
      character*(*) cdat
c
      equivalence (MOTREG,KPOSMP(0381)), (INCR  ,KPOSMP(1226))
      equivalence (ABSCOD,KPOSMP(1241)), (INCCOD,KPOSMP(1242))
      equivalence (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*4 MOTREG(24),INCR,ABSCOD,INCCOD,REGBNC(MAXFMT),
     1          REGENC(MAXFMT)
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (GRANGE,POSMAP(0801)), (MRANGE,POSMAP(0955))
      equivalence (MOTCDV,POSMAP(1284)), (ABSCDV,POSMAP(1285))
      equivalence (INCCDV,POSMAP(1286))
c
      real*8 DUMMY,MOTCDV,ABSCDV,INCCDV,GRANGE(14,11),MRANGE(7,11)
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*4 i,i1,i2,k,jj,kk,m,n,ierr,ireg,lreg,nreg,next,matched,
     1          flstrcmp,flstrcmpi,isnumchar,irbuf(MAXFMT),ineg,
     2          ire(MAXFMT),irbuf1(MAXFMT),nreg1,maxc
c
      logical done
c
      real*8 rval(MAXFMT),rnum
c
      character*1 bslash
      character*2 lsgn(2)
      character*60 lval
      character*80 msg
c
      data lsgn /'P(','N('/
C WNT-START
      data bslash /'\'/
C WNT-END
C SGI-SUN-HPX-IBM-START
C     data bslash /'\\'/
C SGI-SUN-HPX-IBM-END
c
c...Initialize routine
c
      kreg = -1000
      k = ichar(cdat(1:1))-32
      if (k.lt.1.or.k.gt.100) goto 8000
      n = RGNUM(k)
      if (n.eq.0) goto 8000
      m = RGBEG(k)
      n = m+n-1
      nreg = 0
      maxc = 0
c
c...Loop through all registers in the register table that start
c...with the first letter found in the string to find correct register.
c
      do 100 i=m,n,1
          ireg = RGTAB(i)
          lreg = REGBNC(ireg)
          if (REGST(ireg)(lreg:lreg) .eq. bslash) lreg = lreg - 1
          if (lreg.gt.knc) goto 100
          if (kcase.eq.1) then
              matched = flstrcmp(REGST(ireg), cdat, lreg)
          else
              matched = flstrcmpi(REGST(ireg), cdat, lreg)
          endif
          if (matched.ne.1) goto 100
c
c...Check for Special sign P( & N(
c
          ineg   = 0
          if (FMTDES(2,ireg) .eq. 3) then
              lreg   = lreg   + 1
              if (cdat(lreg:lreg+1) .eq. lsgn(1)) then
                  lreg   = lreg   + 1
              else if (cdat(lreg:lreg+1) .eq. lsgn(2)) then
                  lreg   = lreg   + 1
                  ineg   = 1
              else
                  go to 100
              endif
          endif
c
c...Find register value
c
          next = lreg+1
          if (REGST(ireg)(REGBNC(ireg):REGBNC(ireg)) .eq. bslash) then
              do while (next .le. knc .and. cdat(next:next) .eq. ' ')
                  next = next + 1
              enddo
          endif
          i1 = next
          done = next.gt.knc
          do while (.not.done)
              done = isnumchar(cdat(next:next)).ne.1
              if (.not.done) then
                  next = next+1
                  done = next.gt.knc
              endif
          enddo
          i2 = next-1
          if (i2 .lt. i1 .and. (FMTDES(8,ireg) .ne. 0 .or.
     1        FMTDES(9,ireg) .ne. 0)) goto 100
c
c...Check register end string if any.
c
          if (REGENC(ireg).gt.0) then
              lreg = REGENC(ireg)
              if (REGEN(ireg)(lreg:lreg) .eq. bslash) lreg = lreg - 1
              if (next+lreg-1.gt.knc) goto 100
              if (kcase.eq.1) then
                  matched = flstrcmp(REGEN(ireg), cdat(next:), lreg)
              else
                  matched = flstrcmpi(REGEN(ireg), cdat(next:), lreg)
              endif
              if (matched.ne.1) goto 100
              next = next+lreg
          endif
c
c...Calculate register value
c
          if (i2 .lt. i1) then
              rnum   = 0
          else
              lval = cdat(i1:i2)
              call ctof (lval,rnum,FMTDES(1,ireg),ierr,msg)
cc              call ctor(lval, gval, ierr)
              if (ierr.ne.0) goto 100
              if (ineg .eq. 1) rnum = -rnum
          endif
cc          if (kcnv.eq.0) goto 200
c
c...Check G-code groups
c
cc          if (ireg.gt.17.and.ireg.lt.29) then
          if (ireg .eq. 18) then
              do 30 jj = 1,11
                  do 20 kk = 1,14
                      if (GRANGE(kk,jj).eq.DUMMY) goto 30
                      if (GRANGE(kk,jj).eq.rnum) then
                          ireg = jj+17
                          goto 80
                      endif
   20             continue
   30         continue
c
c...Check M-code groups
c
cc          else if (ireg.gt.36.and.ireg.lt.48) then
          else if (ireg .eq. 37) then
              do 50 jj = 1,11
                  do 40 kk = 1,7
                      if (MRANGE(kk,jj).eq.DUMMY) goto 50
                      if (MRANGE(kk,jj).eq.rnum) then
                          ireg = jj+36
                          goto 80
                      endif
   40             continue
   50         continue
          endif
   80     continue
          rval(ireg) = rnum
          ire(ireg) = next
          call reglog (ireg,lreg)
cc          if (lreg.eq.ABSCOD.and.gval.eq.ABSCDV) INCR = 1
cc          if (lreg.eq.INCCOD.and.gval.eq.INCCDV) INCR = 2
          nreg   = nreg   + 1
          irbuf(nreg) = ireg
          if (REGBNC(ireg) .gt. maxc) maxc = REGBNC(ireg)
          if ((ireg .gt. 17 .and. ireg .lt. 29) .or.
     1        (ireg .gt. 36 .and. ireg .lt. 48)) go to 200
cc          goto 200
  100 continue
      if (nreg .eq. 0) goto 8000
  200 continue
cc      if (i2.lt.i1) goto 8000
c
c...Determine correct register to use
c...depending on current mode
c
      nreg1  = 0
      do 250 i=1,nreg,1
          if (REGBNC(irbuf(i)) .eq. maxc) then
              nreg1   = nreg1   + 1
              irbuf1(nreg1) = irbuf(i)
          endif
  250 continue
      call regdtm (irbuf1,nreg1,ktyp,ksub,ireg)
c
c...Get register's value
c
      ierr = 0
cc      lval = cdat(i1:i2)
cc      call ctof (lval,gval,FMTDES(1,ireg),ierr,msg)
cc      if (ierr .ne. 0) goto 8000
      gval   = rval(ireg)
      kreg = ireg
      kend = ire(ireg) - 1
c
c...Convert input register to output register
c
      if (kcnv .eq. 1) then
          if ((ireg .gt. 17 .and. ireg .lt. 29) .or.
     1        (ireg .gt. 36 .and. ireg .lt. 48)) then
              if (ireg .lt. 29) then
                  i1     = ireg   - 17
              else
                  i1     = ireg   - 25
              endif
              call ptd_schreglist (i1,gval,gval,kreg)
          else if (REGCNV(ireg) .gt. 0) then
              kreg   = REGCNV(ireg)
          endif
      endif
c
c...End of routine
c
 8000 continue
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmin (cdat,knc,ksw,gprm,kend,kerr)
c
c   FUNCTION:  This routine parses the parameters of a Macro Call block.
c
c   INPUT:    cdat    C*n  D1   -  String which contains the Macro
c                                  parameters.
c
c             knc     I*4  D1   -  Number of characters in 'cdat'.
c
c   OUTPUT:   ksw     I*4  D30  -  1 = This parameter is defined in this
c                                  block.
c
c             gval    R*8  D1   -  Macro parameter values.
c
c             kend    I*4  D1   -  Ending character position of Macro
c                                  parameters.
c
c             kerr    I*4  D1   -  Returns 1 when an error occured parsing
c                                  the Macro parameters.
c
c***********************************************************************
c
      subroutine prmin (cdat,knc,ksw,gprm,kend,kerr)
c
      include 'post.inc'
c
      equivalence (MACBLK,KPOSMP(0889)), (USRBRG,KPOSMP(2600))
      equivalence (NCUSRB,KPOSMP(3025))
c
      integer*4 MACBLK,USRBRG(20,20),NCUSRB(20)
c
      integer*4 knc,ksw(30),kend,kerr
c
      real*8 gprm(30)
c
      character*(*) cdat
c
      integer*4 i,nc,nprm,ireg(30),iblk(30),inc,isnumchar
c
      logical done
c
      character*30 lval
c
c...Initialize routine
c
      if (cdat(1:1) .ne. '(') go to 9000
      do 100 i=1,30,1
          ksw(i) = 0
          gprm(i) = 0.
          ireg(i) = i
  100 continue
      nprm   = 0
      lval   = ' '
      nc     = 0
      kerr   = 0
c
c...Define register order
c
      if (MACBLK .ne. 0 .and. NCUSRB(MACBLK) .ne. 0) then
          inc    = 0
          do 230 i=1,NCUSRB(MACBLK),1
              if (USRBRG(i,MACBLK) .gt. 1000) then
                  inc    = inc    + 1
                  iblk(inc) = USRBRG(i,MACBLK) - 1000
                  ireg(iblk(inc)) = 0
              endif
  230     continue
          do 270 i=1,30,1
              if (ireg(i) .ne. 0) then
                  inc    = inc    + 1
                  iblk(inc) = ireg(i)
              endif
  270     continue
c
      else
          do 280 i=1,30,1
              iblk(i) = ireg(i)
  280     continue
      endif
c
c...Loop through all registers in the register table that start
c...with the first letter found in the string to find correct register.
c
      do 500 i=2,knc,1
c
c...End of parameter
c
          if (cdat(i:i) .eq. ',' .or. cdat(i:i) .eq. ')') then
              nprm   = nprm   + 1
              if (nc .ne. 0) then
                  call ctor(lval(1:nc),gprm(iblk(nprm)),kerr)
                  if (kerr .ne. 0) goto 8000
                  ksw(iblk(nprm)) = 1
                  if (cdat(i:i) .eq. ')') go to 8000
                  nc     = 0
              endif
c
c...Check for valid digit
c
          else if (cdat(i:i) .ne. ' ') then
              done = isnumchar(cdat(i:i)) .ne. 1
              if (done) go to 9000
              nc     = nc     + 1
              lval(nc:nc) = cdat(i:i)
          endif
  500 continue
c
c...End of routine
c
 8000 kend   = i
      return
c
c...Parsing error
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  msgin (cdat,knc,cmsg,knco,kend)
c
c   FUNCTION:  This routine finds a message block at the start of a string.
c
c   INPUT:    cdat    C*n  D1   -  String which may contain a message block.
c
c             knc     I*4  D1   -  Number of characters in 'cdat'.
c
c   OUTPUT:   cmsg    I*4  D1   -  Message block text.
c
c             knco    I*4  D1   -  Number of characters in 'cmsg' or
c                                  -1 if a message block was not found.
c
c             kend    I*4  D1   -  Ending character position of message block.
c
c***********************************************************************
c
      subroutine msgin (cdat,knc,cmsg,knco,kend)
c
      include 'post.inc'
c
      equivalence (NCBMSG,KPOSMP(0843)), (NCEMSG,KPOSMP(0844))
c
      integer*4 NCBMSG,NCEMSG
c
      equivalence (MSGST ,CPOSMP(0976)), (MSGEN ,CPOSMP(1500))
c
      character*10 MSGST,MSGEN
c
      integer*4 knc,knco,kend
c
      character*(*) cdat,cmsg
c
      integer*4 matched,flstrcmpi,inc,ibnc,lreg
c
c...Check for start of message
c
      knco = -1
      matched = flstrcmpi(MSGST(1:NCBMSG),cdat(1:knc),NCBMSG)
      if (matched .ne. 1) goto 8000
c
c...Allow for 1st character of "start of message" string
c...to be a space
c
      ibnc   = NCBMSG
      if (MSGST(1:1) .eq. ' ') ibnc = ibnc - 1
c
c...Check for end of message
c
      if (NCEMSG .gt. 0) then
          inc = index(cdat(ibnc+1:knc),MSGEN(1:NCEMSG))
          if (inc .ne. 0) then
              cmsg = cdat(ibnc+1:ibnc+inc)
              knco = inc - 1
          endif
      else
          cmsg = cdat(ibnc+1:knc)
          knco = knc - ibnc
      endif
c
c...Determine end of message string
c
      if (knco .ne. -1) kend = ibnc + NCEMSG + knco
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  regdtm (kary,knreg,ktyp,ksub,kreg)
c
c   FUNCTION:  This routine does a "best guess" attempt on determining
c              the register to use when multiple registers use the same
c              character designation.  This determination is based on
c              the current block type.
c
c   INPUT:    kary    I*4  Dn   -  Array of registers to select from.
c
c             knreg   I*4  D1   -  Number of registers in 'kary'.
c
c             ktyp    I*4  D1   -  Type of block being parsed.
c
c             ksub    I*4  D1   -  Subtype of block being parsed.
c
c   OUTPUT:   kreg    I*4  D1   -  Register Number.
c
c***********************************************************************
c
      subroutine regdtm (kary,knreg,ktyp,ksub,kreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      common/REGCOM/RGBEG, RGNUM, RGTAB, RGINC, REGCNV
      integer*4 RGBEG(100), RGNUM(100), RGTAB(MAXFMT), RGINC(MAXFMT)
      integer*4 REGCNV(MAXFMT)
c
      integer*4 kary(MAXFMT),knreg,ktyp,ksub,kreg
c
      equivalence (CYCREG,KPOSMP(0231)), (MOTREG,KPOSMP(0381))
      equivalence (MBLREG,KPOSMP(0807)), (IJKREG,KPOSMP(0827))
      equivalence (SEQCOD,KPOSMP(0847)), (ALNCOD,KPOSMP(0848))
      equivalence (XFMREG,KPOSMP(0931)), (XFMCD ,KPOSMP(0946))
      equivalence (MACHTP,KPOSMP(1201)), (NUMLIN,KPOSMP(1202))
      equivalence (INCR  ,KPOSMP(1226)), (ICIPR2,KPOSMP(1266))
      equivalence (AXSINC,KPOSMP(1296)), (RSIZCD,KPOSMP(1328))
      equivalence (CIRCOD,KPOSMP(1378)), (IRTDEF,KPOSMP(1485))
      equivalence (IJKROT,KPOSMP(1739)), (IC3AXF,KPOSMP(1747))
      equivalence (TLCCD ,KPOSMP(1839)), (TLOCD ,KPOSMP(1860))
      equivalence (SPNSCD,KPOSMP(3144)), (IFOTYP,KPOSMP(3151))
      equivalence (FEDCD ,KPOSMP(3162))
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
      equivalence (ICUTDO,KPOSMP(3301)), (DELYCD,KPOSMP(3351))
      equivalence (HOMREG,KPOSMP(3366)), (PSTNRG,KPOSMP(3381))
c
      integer*4 CUTCCD(30),DELYCD(5),HOMREG(10),TLOCD(8),INCR,
     1          IJKREG(3),CIRCOD(6),CYCREG(20),MOTREG(24),ICIPR2(8),
     2          IJKROT,MACHTP,MBLREG(3),SEQCOD,ALNCOD,TLCCD(10),
     3          SPNSCD(2),FEDCD(8),PSTNRG(15),CUTCFL(20),ICUTDO(15),
     4          AXSINC(10),NUMLIN(3),IRTDEF,RSIZCD(5),IC3AXF(10)
      integer*4 XFMREG(10),IFOTYP,XFMCD(5)
c
      integer*4 i,j,iaxs(24),istt(10),iptt(6)
c
      data iaxs /1,1,2,2,3,3,4,4,5,5,6,6, 7,7,7,8,8,8,9,9,9,10,10,10/
      data istt /1,1,2,2,3,3,1,2,3,4/
      data iptt /1,2,1,2,1,2/
c
c...Only 1 register to choose from
c
      if (knreg .eq. 1) then
          kreg   = kary(1)
          go to 8000
c
c...DELAY block
c
      else if (ktyp .eq. PBLK_DWELL) then
          do 100 i=1,knreg,1
              if (kary(i) .eq. DELYCD(2) .or. kary(i) .eq. DELYCD(3))
     1                then
                  kreg   = kary(i)
                  go to 8000
              endif
  100     continue
c
c...Axis size block
c
      else if (ktyp .eq. PBLK_AXSIZE) then
          do 150 i=1,knreg,1
              do 140 j=1,IRTDEF,1
              if (kary(i) .eq. RSIZCD(j+1)) then
                  kreg   = kary(i)
                  go to 8000
              endif
  140         continue
  150     continue
c
c...Fixture offset block
c
      else if (ktyp .eq. PBLK_FOFFSET) then
          do 250 i=1,knreg,1
              do 200 j=16,23,1
                  if (kary(i) .eq. CUTCCD(j)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
  200         continue
  250     continue
c
c...GOHOME block
c
      else if (ktyp .eq. PBLK_GOHOME) then
          do 350 i=1,knreg,1
              do 300 j=1,10,1
                  if (((j .le. 6 .and. iptt(j) .le. NUMLIN(istt(j)))
     1                .or. (j .gt. 6 .and. istt(j) .le. IRTDEF)) .and.
     2                  kary(i) .eq. HOMREG(j)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
  300         continue
  350     continue
c
c...POSTN block
c
      else if (ktyp .eq. PBLK_POSTN) then
          do 450 i=1,knreg,1
              do 400 j=1,10,1
                  if (((j .le. 6 .and. iptt(j) .le. NUMLIN(istt(j)))
     1                .or. (j .gt. 6 .and. istt(j) .le. IRTDEF)) .and.
     2                  kary(i) .eq. PSTNRG(j)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
  400         continue
  450     continue
c
c...Tool length offset block
c
      else if (ktyp .eq. PBLK_TOFFSET) then
          do 480 i=1,knreg,1
              do 470 j=1,8,1
                  if (kary(i) .eq. TLOCD(j)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
  470         continue
  480     continue
c
c...Transformation block
c
      else if (ktyp .eq. PBLK_XFORM) then
          do 485 i=1,knreg,1
              do 483 j=1,10,1
                  if (kary(i) .eq. XFMREG(j)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
  483         continue
              if (kary(i) .eq. XFMCD(5)) then
                  kreg   = kary(i)
                  go to 8000
              endif
  485     continue
c
c...Circular block
c
      else if (ktyp .eq. PBLK_MOTION .and. ksub .eq. PMOT_CIRCUL) then
          do 550 i=1,knreg,1
              do 500 j=1,6,1
                  if (kary(i) .eq. CIRCOD(j)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
  500         continue
  550     continue
c
c...3-axis circular block
c
      else if ((ktyp .eq. PBLK_MOTION .or.
     1         (ktyp .eq. PBLK_UNKNOWN .and. IC3AXF(2) .eq. 1)) .and.
     2         ksub .eq. PMOT_CIR3AX .and. ICIPR2(1) .eq. 0) then
          do 570 i=1,knreg,1
              do 560 j=5,7,1
                  if (kary(i) .eq. IC3AXF(j)) then
                      if (IC3AXF(2) .eq. 1) ktyp = PBLK_MOTION
                      kreg   = kary(i)
                      go to 8000
                  endif
  560         continue
  570     continue
c
c...Cycle block
c
      else if (ktyp .eq. PBLK_MOTION .and. ksub .eq. PMOT_CYCLE) then
          do 650 i=1,knreg,1
              do 600 j=1,14,1
                  if (kary(i) .eq. CYCREG(j)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
  600         continue
  650     continue
c
c...Cycle off block
c
      else if (ktyp .eq. PBLK_MOTION .and. ksub .eq. PMOT_CYCOFF) then
          do 680 i=1,knreg,1
              if (kary(i) .eq. CYCREG(2)) then
                  kreg   = kary(i)
                  go to 8000
              endif
  680     continue
      endif
c
c...Standard block
c......Search for a motion register first
c
      do 750 i=1,knreg,1
          do 700 j=1,24,1
              if (j .ne. 13 .and. j .ne. 16 .and. j .ne. 19 .and.
     1            j .ne. 22 .and. kary(i) .eq. MOTREG(j)) then
c
c.........Always absolute/incremental
c
                  if (j .gt. 12 .and. AXSINC(iaxs(j)) .ne. 1) then
                      if (RGINC(kary(i)) .eq. AXSINC(iaxs(j))-1) then
                          kreg   = kary(i)
                          go to 8000
                      endif
c
c.........Only use absolute registers in absolute mode and
c.........incremental registers in incremental mode.
c
                  else if (RGINC(kary(i)) .eq. 0 .or.
     1                RGINC(kary(i)) .eq. INCR) then
                      kreg   = kary(i)
                      go to 8000
                  endif
              endif
  700     continue
  750 continue
c
      if (IJKROT .eq. 1) then
          do 800 i=1,knreg,1
              if (kary(i) .eq. IJKREG(1) .or.
     1            kary(i) .eq. IJKREG(2) .or.
     2            kary(i) .eq. IJKREG(3)) then
                  kreg   = kary(i)
                  go to 8000
              endif
  800     continue
      endif
c
      if (MACHTP .eq. 3) then
          do 900 i=1,knreg,1
              if (kary(i) .eq. MBLREG(2) .or.
     1            kary(i) .eq. MBLREG(3)) then
                  kreg   = kary(i)
                  go to 8000
              endif
  900     continue
      endif
c
c......Cutcom codes
c
      if (ICUTDO(2) .eq. 1) then
c
c..........Vector registers
c
          do 1000 i=1,knreg,1
              if ((CUTCFL(1) .ge. 1 .and. CUTCFL(1) .le. 3) .or.
     1            ICUTDO(3) .eq. 3) then
                  if (kary(i) .eq. CUTCCD(6) .or. kary(i) .eq. CUTCCD(7)
     1                    .or. kary(i) .eq. CUTCCD(8)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
c
c..........Angular register
c
              else if (CUTCFL(1) .eq. 4) then
                  if (kary(i) .eq. CUTCCD(9)) then
                      kreg   = kary(i)
                      go to 8000
                  endif
              endif
 1000     continue
      endif
c
c......Feed rate registers
c
      do 1100 i=1,knreg,1
          if (kary(i) .eq. FEDCD(IFOTYP)) then
              kreg   = kary(i)
              go to 8000
          endif
 1100 continue
      do 1150 i=1,knreg,1
          do 1120 j=1,8,1
              if (kary(i) .eq. FEDCD(j)) then
                  kreg   = kary(i)
                  go to 8000
              endif
 1120     continue
 1150 continue

c
c......Search for other valid codes
c......That are not part of previous block types
c
      do 1250 i=1,knreg,1
          if (kary(i) .eq. SEQCOD .or. kary(i) .eq. ALNCOD .or.
     1        kary(i) .eq. TLCCD(3) .or. kary(i) .eq. TLCCD(4) .or.
     2        kary(i) .eq. SPNSCD(1) .or. kary(i) .eq. SPNSCD(2)) then
              kreg   = kary(i)
              go to 8000
          endif
 1250 continue
c
c...Could not find the code above
c...so just select the first one
c
      kreg   = kary(1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  seqblk (cdat,knc,kseq,kret)
c
c   FUNCTION: Resequences an NC block or simply checks for bad blocks.
c
c
c   INPUT:  cdat    C*1  Dn  - NC Block.
c
c           knc     I*4  D1  - Length of NC block.
c
c           kseq    I*4  D1  - Current sequence number.
c                              When set to -1, this routine will only check
c                              for valid NC blocks, return 0
c                              When set to -1000, this routine will remove the
c                              sequence register from block, return 0.
c
c   OUTPUT: cdat    C*1  Dn  - Updated NC Block.
c
c           knc     I*4  D1  - Length of NC block.
c
c           kret    I*4  D1  - Returns 1 if a sequence number was replaced
c                              in this block.  Returns -1 if this is a
c                              bad block.
c
c***********************************************************************
c
      subroutine seqblk (cdat,knc,kseq,kret)
c
      include 'post.inc'
c
      integer*4 knc,kseq,kret
c
      character*256 cdat
c
      equivalence (SEQCOD,KPOSMP(0847)), (MACPSW,KPOSMP(0859))
c
      integer*4 SEQCOD,MACPSW(30)

      equivalence (MACPRM,POSMAP(4300))
c
      real*8 MACPRM(30)
c
      integer*4 i1,ix,ireg,iend,ierr,oreg,rnc,ityp,imot
c
      real*8 val,rval
c
      character*256 sreg
c
c...Initialize routine
c
      ix     = 1
      kret   = 0
c
c...Loop through registers
c
      do while (ix .le. knc)
          i1 = knc - ix + 1
c
c...Parse Macro call parameters
c
          if (cdat(ix:ix) .eq. '(') then
              call prmin (cdat(ix:),i1,MACPSW,MACPRM,iend,ierr)
          else
              ierr = 1
          endif
c
c...Parse register
c
          if (ierr .ne. 0) then
              call codin (cdat(ix:),i1,1,0,ireg,val,iend,ityp,imot)
              if (ireg .eq. -1000) then
                  call msgin (cdat(ix:),i1,sreg,rnc,iend)
                  if (rnc .lt. 0) go to 9000
              endif
c
c...Sequence number
c...Replace it with the new one
c
              if (ireg .eq. SEQCOD .and. kseq .ge. 0) then
                  rval = kseq
                  call fmtincod (ireg,rval,oreg,sreg,rnc)
                  cdat(ix+rnc:) = cdat(ix+iend:)
                  cdat(ix:ix+rnc-1) = sreg(1:rnc)
                  kret = 1
                  go to 8000
              else if (ireg .eq. SEQCOD .and. kseq .eq. -1000) then
c
c...if sequence number pass in is -1000, remove the sequence register
c
                  cdat(ix:) = cdat(ix+iend:)
                  kret = 0
                  go to 8000
              endif
          endif
c
c...Point to next register
c
          ix = ix + iend
          do while (ix .le. knc .and. cdat(ix:ix) .eq. ' ')
              ix = ix+1
          enddo
      enddo
c
c...End of routine
c
 8000 continue
      return
c
c...Bad block found
c
 9000 kret   = -1
      go to 8000
      end
