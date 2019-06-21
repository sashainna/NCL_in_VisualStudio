c
c***********************************************************************
c
c   FILE NAME:  cycted
c   CONTAINS:
c               cycted  cycpma  cyccma  cycota
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c       cycted.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c       06/09/14 , 16:26:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cycted (kregs,gvals,knreg)
c
c   FUNCTION:  This routine processes Cycle Blocks.
c
c   INPUT:  kregs    I*4  DMAXFMT - List of registers in current block.
c
c           gvals    R*8  DMAXFMT - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  DMAXFMT - Updated list of registers.
c
c           gvals    R*8  DMAXFMT - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine cycted (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
      equivalence (ICYCSV,KPOSMP(0291)), (ICYMTP,KPOSMP(0920))
c
      integer*4 ICYMTP,ICYCSW(5),ICYCDO(15),ICYCSV(3)
c
      integer*4 nreg,ireg(2)
c
      real*8 toppl,rappl,fedpl,val(2)
c
c...Build cycle parameter settings
c
      call cycpma (kregs,gvals,knreg)
c
c...Output cycle command
c
      call cyccma (toppl,rappl,fedpl)
c
c...Output motion record
c
      if (ICYMTP .ne. 1) call cycota (toppl,rappl,fedpl)
c
c...Cancel single shot cycle
c
      if (ICYMTP .eq. 2) then
          ICYCSV(3) = ICYCSW(1)
          ICYCSW(1) = 0
          ICYMTP = 1
          MINTER = PMOT_CYCOFF
          nreg   = 0
          call cyclea (ireg,val,nreg)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cycpma (kregs,gvals,knreg)
c
c   FUNCTION:  This routine calculates the cycle parameters from the tape
c              codes.
c
c   INPUT:  kregs    I*4  DMAXFMT - List of registers in current block.
c
c           gvals    R*8  DMAXFMT - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  DMAXFMT - Updated list of registers.
c
c           gvals    R*8  DMAXFMT - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine cycpma (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (CYCREG,KPOSMP(0231)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (MACPSW,KPOSMP(0859))
      equivalence (ICYMTP,KPOSMP(0920))
c
      integer*4 ICYCDO(15),CYCREG(20),ICYCSW(5),ICYMTP,MACPSW(30),
     1          ICYCFL(30),CYCCOD(20)
c
      equivalence (DUMMY ,POSMAP(0003)), (RCYCDO,POSMAP(2931))
      equivalence (MACPRM,POSMAP(4300))
c
      real*8 DUMMY,RCYCDO(20),MACPRM(30)
c
      integer*4 i,inc,inx(15),istx(15),rnx(15),ifnd
c
      data inx  /2,4,3,5,5,6,6,7,7, 7, 7, 8, 8, 9,10/
      data istx /1,1,1,1,2,1,2,1,2, 2, 2, 1, 2, 1, 1/
      data rnx  /1,4,2,5,6,7,8,9,9,10,10,11,12,13,14/
c
c...Initialize routine
c
      if (PCNV_TYPE .eq. PCNV_APTSRC) then
          do 100 i=1,10,1
              ICYCDO(i) = 0
  100     continue
      else
          ICYCDO(1) = ICYCSW(1)
      endif
c
c...Search for cycle registers
c
      do 200 i=1,15,1
          if (ICYCFL(30) .ne. 1) then
              if ((ICYCSW(1) .le. 4 .or. ICYCSW(1) .eq. 11) .and.
     1            (i .eq. 6 .or. i .eq. 7)) go to 200
              if ((ICYCSW(1) .eq. 5 .or. ICYCSW(1) .eq. 13) .and.
     2            (i .ge. 8 .and. i .le. 11)) go to 200
          endif
c
c...Standard register format
c
          ifnd   = 0
          if (ICYCFL(30) .ne. 1) then
              call fndreg (kregs,gvals,knreg,CYCREG(i),DUMMY,inc)
              if (inc .ne. 0) then
                  ICYCDO(inx(i)) = istx(i)
                  RCYCDO(rnx(i)) = gvals(inc)
                  call delreg (kregs,gvals,knreg,inc)
                  ifnd   = 1
              endif
c
c...Macro Call format
c
          else
              if (CYCREG(i) .ne. 0 .and. MACPSW(CYCREG(i)) .ne. 0) then
                  ICYCDO(inx(i)) = istx(i)
                  RCYCDO(rnx(i)) = MACPRM(CYCREG(i))
                  MACPSW(CYCREG(i)) = 0
                  ifnd   = 1
              else if (i .eq. 1 .and. MACPSW(CYCREG(i)+1) .ne. 0) then
                  ICYCDO(inx(i)) = istx(i)
                  RCYCDO(rnx(i)) = MACPRM(CYCREG(i)+1) * -1.
                  MACPSW(CYCREG(i)+1) = 0
                  ifnd   = 1
              endif
          endif
c
c...-TPI will be CYCLE/REVERS
c
          if (i .eq. 3 .and. ifnd .eq. 1 .and. ICYCSW(1) .eq. 10 .and.
     1        CYCCOD(11) .eq. CYCCOD(13)) then
              if (RCYCDO(rnx(i)) .ge. 0.) then
                  ICYCSW(1) = 12
                  if (PCNV_TYPE .ne. PCNV_APTSRC) ICYCDO(1) = ICYCSW(1)
              else
                  RCYCDO(rnx(i)) = -RCYCDO(rnx(i))
              endif
          endif
  200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cyccma
c
c   FUNCTION:  This routine calculates the various cycle levels and
c              outputs the CYCLE command.
c
c   INPUT:  none
c
c   OUTPUT: gtop     R*8  D1  - Top of part.
c
c           grapto   R*8  D1  - Rapto position.
c
c           gfedto   R*8  D1  - Final depth.
c
c***********************************************************************
c
      subroutine cyccma (gtop,grapto,gfedto)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      real*8 gtop,grapto,gfedto
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCREG,KPOSMP(0231))
      equivalence (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (MCHOPT,KPOSMP(0308))
      equivalence (MOTREG,KPOSMP(0381)), (MACPSW,KPOSMP(0859))
      equivalence (INCR  ,KPOSMP(1226))
      equivalence (IFOTYP,KPOSMP(3151)), (DELYFL,KPOSMP(3356))
c
      integer*4 ICYCDO(15),ICYCFL(30),ICYCSW(5),MCHOPT(20),INCR,
     1          MOTREG(24),DELYFL(5),IFOTYP,CYCREG(20),MACPSW(30)
c
      equivalence (REGSTO,POSMAP(1032)), (MCHNUM,POSMAP(1287))
      equivalence (STONUM,POSMAP(1387)), (CYCVR ,POSMAP(2926))
      equivalence (RCYCDO,POSMAP(2931))
      equivalence (CYCPSV,POSMAP(2951)), (FEED  ,POSMAP(3547))
      equivalence (MACPRM,POSMAP(4300))
c
      real*8 RCYCDO(20),STONUM(3,4),REGSTO(MAXFMT),CYCPSV(5),FEED(4),
     1       MCHNUM(3,4),CYCVR(5),MACPRM(30)
c
      integer*4 i,nco,vwds(40),vtyp(40),idid,ist
      integer*4 vcycle,vfedto,vrapto,vdwell,vstep,vrpeat,vrtrct,vofset,
     1          vmode(13),vipm,vipr,vmmpm,vmmpr,vparam
c
      real*8 vval(40),dep,fval,rval
c
      character*80 vtxt
c
      data vmode / 82, 211, 212, 213, 153, 163,  81, 151, 262, 1008,
     1            249, 168, 152/
      data vcycle /1054/, vdwell /279/, vfedto /281/, vofset /705/
      data vrapto /280/, vrpeat /1083/, vrtrct /295/, vstep /92/
      data vipm /73/, vipr /74/, vmmpm /315/, vmmpr /316/, vparam /934/
c
c...Initialize command
c
      vwds(1) = vcycle
      vtyp(2) = 1
      vwds(2) = vmode(ICYCSW(1))
      nco    = 2
c
c...Calculate Top of part
c...Based on register value
c
      if (CYCREG(15) .ne. 0) then
          if ((ICYCFL(7) .eq. 1 .and. INCR .eq. 1) .or.
     1            ICYCFL(7) .eq. 2) then
              gtop   = RCYCDO(14)
          else
              gtop   = STONUM(3,3) + RCYCDO(14)
          endif
      endif
c
c...Rapto plane
c
      if (ICYCFL(8) .eq. 4) then
          grapto = gtop   + RCYCDO(4) + CYCVR(2)
      else if ((ICYCFL(8) .eq. 1 .and. INCR .eq. 1) .or.
     1             ICYCFL(8) .eq. 2) then
          grapto = RCYCDO(4) + CYCVR(2)
      else
          grapto = STONUM(3,3) + RCYCDO(4) + CYCVR(2)
      endif
c
c...Top of part
c...Based on Rapto plane
c
      if (CYCREG(15) .eq. 0) then
          rval   = .1
          if (MCHOPT(2) .eq. 2) rval = 2.5
          gtop   = grapto - rval
      else
          rval   = grapto - gtop
      endif
c
c...Final depth
c
      if (CYCREG(1) .ne. 0) then
          dep    = RCYCDO(1)
      else
          ist    = MOTREG(8+INCR)
          dep    = REGSTO(ist)
      endif
      if ((ICYCFL(4) .eq. 1 .and. INCR .eq. 1) .or.
     1        ICYCFL(4) .eq. 2) then
          if (CYCREG(1) .ne. 0) then
              gfedto = dep
          else
              gfedto = MCHNUM(3,3)
          endif
      else
          if (ICYCFL(6) .eq. 1) then
              gfedto = grapto + dep
          else if (ICYCFL(6) .eq. 2) then
              gfedto = STONUM(3,3) + dep
          else
              gfedto = gtop   + dep
          endif
      endif
      fval   = gtop  - gfedto
c
c...Determine if CYCLE command should be output
c
      idid   = 0
      do 50 i=1,10,1
          if (i .ne. 4 .and. ICYCDO(i) .gt. 0) idid = 1
   50 continue
cc      if (fval .ne. CYCPSV(1) .or. rval .ne. CYCPSV(2)) idid = 1
      if (dabs(fval-CYCPSV(1)) .gt. .0001 .or.
     1    dabs(rval-CYCPSV(2)) .gt. .0001) idid = 1
      if (MILAST .ne. PMOT_CYCLE) idid = 1
      if (idid .eq. 0) go to 8000
      CYCPSV(1) = fval
      CYCPSV(2) = rval
c
c...FEDTO
c
      if (PCNV_TYPE .eq. PCNV_APTSRC) then
          nco    = nco    + 1
          vtyp(nco) = 1
          vwds(nco) = vfedto
          nco    = nco    + 1
          vtyp(nco) = 2
          vval(nco) = fval
c
c...Feedrate
c
          nco    = nco    + 1
          vtyp(nco) = 1
          if (IFOTYP .eq. 2) then
              if (MCHOPT(2) .eq. 1) then
                  vwds(nco) = vipr
              else
                  vwds(nco) = vmmpr
              endif
          else
              if (MCHOPT(2) .eq. 1) then
                  vwds(nco) = vipm
              else
                  vwds(nco) = vmmpm
              endif
          endif
          nco    = nco    + 1
          vtyp(nco) = 2
          vval(nco) = FEED(1)
c
c...RAPTO
c
          nco    = nco    + 1
          vtyp(nco) = 1
          vwds(nco) = vrapto
          nco    = nco    + 1
          vtyp(nco) = 2
          rval   = grapto - gtop
          vval(nco) = rval
c
c...DWELL
c
          if (ICYCDO(5) .ne. 0) then
              nco    = nco    + 1
              vtyp(nco) = 1
              vwds(nco) = vdwell
              do 100 i=1,ICYCDO(5),1
                  nco    = nco    + 1
                  vtyp(nco) = 2
                  if (DELYFL(2) .eq. 1) then
                      vval(nco) = RCYCDO(4+i) / 1000.
                  else if (DELYFL(2) .eq. 2) then
                      vval(nco) = RCYCDO(4+i)
                  else
                      vval(nco) = RCYCDO(4+i) * 60.
                  endif
  100         continue
          endif
c
c...STEP
c
          if (ICYCDO(6) .ne. 0) then
              nco    = nco    + 1
              vtyp(nco) = 1
              vwds(nco) = vstep
              do 200 i=1,ICYCDO(6),1
                  nco    = nco    + 1
                  vtyp(nco) = 2
                  if (ICYCFL(12) .eq. 1 .or. ICYCFL(12) .eq. 2 .or.
     1                    i .eq. 2) then
                      vval(nco) = RCYCDO(6+i)
                  else if (ICYCFL(12) .eq. 3) then
                      vval(nco) = dabs(RCYCDO(6+i))
                  else
                      vval(nco) = dabs(grapto-gfedto) / RCYCDO(6+i)
                  endif
  200         continue
          endif
c
c...OFFSET
c
          if (ICYCDO(7) .ne. 0) then
              nco    = nco    + 1
              vtyp(nco) = 1
              vwds(nco) = vofset
              do 300 i=1,ICYCDO(7),1
                  nco    = nco    + 1
                  vtyp(nco) = 2
                  vval(nco) = RCYCDO(8+i)
  300         continue
          endif
c
c...RTRCTO
c
          if (ICYCDO(8) .ne. 0) then
              nco    = nco    + 1
              vtyp(nco) = 1
              vwds(nco) = vrtrct
              do 400 i=1,ICYCDO(8),1
                  if (ICYCDO(8) .eq. 2 .or. ICYCFL(9) .eq. 2 .or.
     1                (RCYCDO(11) .ne. CYCPSV(6) .and. ICYCSW(3) .eq. 1)
     2                .or. (RCYCDO(11) .ne. grapto .and.
     3                ICYCSW(3) .eq. 2)) then
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      vval(nco) = RCYCDO(10+i)
                  else
                      nco    = nco    - 1
                  endif
  400         continue
          endif
c
c...REPEAT
c
          if (ICYCDO(9) .ne. 0) then
              nco    = nco    + 1
              vtyp(nco) = 1
              vwds(nco) = vrpeat
              nco    = nco    + 1
              vtyp(nco) = 2
              vval(nco) = RCYCDO(13)
          endif
c
c...PARAMS
c
          if (ICYCFL(30) .eq. 1) then
              idid   = 0
              do 500 i=1,30,1
                  if (MACPSW(i) .ne. 0) then
                      if (idid .eq. 0) then
                          nco    = nco    + 1
                          vtyp(nco) = 1
                          vwds(nco) = vparam
                          idid   = 1
                      endif
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      vval(nco) = i
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      vval(nco) = MACPRM(i)
                      MACPSW(i) = 0
                  endif
  500         continue
          endif

c
c...Output CYCLE command
c
          call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cycota (gtop,grapto,gfedto)
c
c   FUNCTION:  This routine outputs the positioning move (GOTO) during
c              a cycle sequence.
c
c   INPUT:  gtop     R*8  D1  - Top of part.
c
c           grapto   R*8  D1  - Rapto position.
c
c           gfedto   R*8  D1  - Final depth.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine cycota (gtop,grapto,gfedto)
c
      include 'post.inc'
      include 'pted.inc'
c
      real*8 gtop,grapto,gfedto
c
      equivalence (ICYCSW,KPOSMP(0271)), (ISEQSW,KPOSMP(0845))
c
      integer*4 ICYCSW(5),ISEQSW
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387))
      equivalence (RCYCDO,POSMAP(2931)), (CYCPSV,POSMAP(2951))
      equivalence (PFEED ,POSMAP(3540)), (FEED  ,POSMAP(3547))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 STONUM(3,4),MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),
     1       ROTANG(20,2),CYCPSV(10),RCYCDO(20),PFEED(4),FEED(4)
c
      integer*4 ifl
c
      real*8 rsav(20),fsav
c
c...Change current position to
c...reflect final depth
c
      AXSOUT(5) = gtop
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c...Output motion block
c
      if (PCNV_TYPE .eq. PCNV_APTSRC) then
          call motota
c
c...Simulate cycle
c
      else
          ifl    = 0
          rsav(1) = RCYCDO(1)
          rsav(4) = RCYCDO(4)
          fsav    = FEED(1)
          RCYCDO(1) = gtop - gfedto
          RCYCDO(4) = grapto - gtop
          ISEQSW = 0
          call cymman (ifl)
          RCYCDO(1) = rsav(1)
          RCYCDO(4) = rsav(4)
          FEED(1) = fsav
      endif
c
c...Restore retract position
c
      if (ICYCSW(3) .eq. 1) then
          CYCPSV(7) = CYCPSV(6)
      else
          CYCPSV(7) = grapto
      endif
c
c...Reset current postion
c...to either Rapto or Clearance plane
c
      AXSOUT(5) = gfedto
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c...Save current position
c
      call svtmot
c
c...End of routine
c
 8000 return
      end
