c
c***********************************************************************
c
c   FILE NAME:  motout
c   CONTAINS:
c               motgcd  motmcd  motout  polout  rotout clmput  frcg01
c               getacy
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        motout.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/29/13 , 16:01:03
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  motgcd (kfl,kcnt)
c
c   FUNCTION:  This routine outputs the following codes, which are
c              necesarry in a motion block.
c
c              1.  Unclamp axis code(s).
c              2.  Cutter compensation codes.
c              3.  Buffered SELCTL & TOOLNO.
c              4.  Absolute/Incremental code (G90/G91).
c              5.  Linear motion code (G01).
c              6.  Feed rate.
c
c   INPUT:  kfl     I*4  D10 - 1 = This axis is waiting to be output.
c
c           kcnt    I*4  D1  - Number of axes waiting to be output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine motgcd (kfl,kcnt)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (ICYCSW,KPOSMP(0271)), (REGFRC,KPOSMP(0603))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (INCR  ,KPOSMP(1226)), (ICIPRM,KPOSMP(1230))
      equivalence (ISCIRC,KPOSMP(1238)), (MOTCOD,KPOSMP(1240))
      equivalence (ABSCOD,KPOSMP(1241)), (INCCOD,KPOSMP(1242))
      equivalence (MCHPLN,KPOSMP(1261))
      equivalence (POLCOD,KPOSMP(1262)), (IPOLIN,KPOSMP(1265))
      equivalence (ICRFMT,KPOSMP(1368))
      equivalence (CIRCOD,KPOSMP(1378)), (ICRPLF,KPOSMP(1397))
      equivalence (PLNCOD,KPOSMP(4222)), (ICRDIR,KPOSMP(1390))
      equivalence (SLWFL ,KPOSMP(1701)), (LNCRAP,KPOSMP(3212))
      equivalence (SLWCD ,KPOSMP(1706)), (ISLWDN,KPOSMP(1721))
      equivalence (ISLWNM,KPOSMP(1722)), (IC3AXF,KPOSMP(1747))
      equivalence (ICYLFL,KPOSMP(1901))
      equivalence (IFOTYP,KPOSMP(3151)), (IFDFRC,KPOSMP(3153))
      equivalence (IDPMOV,KPOSMP(3188)), (NRAPCD,KPOSMP(3190))
      equivalence (RAPCD ,KPOSMP(3191)), (IRAP  ,KPOSMP(3199))
      equivalence (IRAPSW,KPOSMP(3205)), (ICUTDO,KPOSMP(3301))
      equivalence (ISBSPL,KPOSMP(4085)), (BSPCOD,KPOSMP(4089))
      equivalence (LFLMCD,KPOSMP(4104)), (LDLMCD,KPOSMP(4105))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
      equivalence (FRAPCD,KPOSMP(4138)), (DRAPCD,KPOSMP(4139))
c
      integer*4 INCR,MOTCOD,ABSCOD,INCCOD,CYCCOD(20),ICYCFL(30),
     1          IFOTYP,IDPMOV,NRAPCD,RAPCD(5),IRAP,ICYCSW(5),IC3AXF(10),
     2          IFDFRC(4),IRAPSW,ICIPRM(8),ISCIRC,CIRCOD(6),PLNCOD(4),
     3          MCHPLN,POLCOD(3),IPOLIN,ICUTDO(15),SLWFL(5),SLWCD(15),
     4          ISLWDN,ISLWNM,REGFRC(MAXFMT),ICRFMT
      integer*4 ICYLFL(20),MACHTP,ICRDIR,LNCRAP,ICRPLF,LTMODE,
     1          MTPDYN,LFLMCD,LDLMCD,FRAPCD,DRAPCD,ISBSPL,BSPCOD
c
      equivalence (MOTCDV,POSMAP(1284)), (ABSCDV,POSMAP(1285))
      equivalence (INCCDV,POSMAP(1286)), (AXSOUT,POSMAP(1340))
      equivalence (BSPCDV,POSMAP(4901))
      equivalence (CIRCDV,POSMAP(2206)), (PLNCDV,POSMAP(2208))
      equivalence (POLCDV,POSMAP(2227)), (SLWVL ,POSMAP(2456))
      equivalence (CYCCDV,POSMAP(2901)), (RAPVL ,POSMAP(3577))
      equivalence (FLMCDV,POSMAP(4600)), (DLMCDV,POSMAP(4601))
      equivalence (FRAPVL,POSMAP(4882)), (DRAPVL,POSMAP(4883))
c
      real*8 MOTCDV,ABSCDV,INCCDV,RAPVL(5),CIRCDV(2),DRAPVL,BSPCDV,
     1       PLNCDV(3),POLCDV(2),SLWVL(15),CYCCDV(20),FLMCDV,DLMCDV,
     2       FRAPVL,AXSOUT(10)
c
      integer*4 kfl(10),kcnt
c
      integer*4 i,icslw(4),ifl,is,ireg,ipdid,isav
c
      real*8 vreg
c
      data icslw /4,6,8,10/
c
c...Clamp/unclamp axes if necessary
c
      call clmput (AXSOUT,kfl)
c
c...Cutter compensation codes
c
      if (ICUTDO(2) .eq. 1) call cutout (kfl)
c
c...Output buffered SELCTL and/or TOOLNO
c
      call tlwhen (kfl)
c
c...Output Absolute/Incremental code
c
      if (INCR .eq. 1) then
          call codout (ABSCOD,ABSCDV)
      else
          call codout (INCCOD,INCCDV)
      endif
c
c...Canned cycle or Threadcutting
c...End of output codes
c
      if (ICYCSW(1) .eq. 1 .or. ICYCSW(1) .eq. 4) then
          if (MOTCOD .ne. 0) then
              ireg   = MOTCOD
              call regtyp (ireg,MOTCDV)
              if (REGFRC(ireg) .gt. 0) REGFRC(ireg) = -1
          endif
          go to 8000
       endif
c
c...Output circular plane selection code
c...in block by itself
c
      ipdid  = 0
      if (ISCIRC .eq. 1 .and. ICRPLF .eq. 2 .and. ICIPRM(3) .ne. 0) then
          ireg   = PLNCOD(4-MCHPLN)
          call regtyp (ireg,PLNCDV(4-MCHPLN))
          call cmpcod (ireg,PLNCDV(4-MCHPLN),0.d0,0,ipdid)
          if (ipdid .eq. 1) then
              call codout (PLNCOD(4-MCHPLN),PLNCDV(4-MCHPLN))
              call clrbuf
          endif
      endif
c
c...Cycle was just canceled
c...force output of linear interp code
c
      if (ICYCSW(2) .eq. 3) then
          ICYCSW(2) = 0
          if ((MTPDYN .eq. 1 .and. ICYCFL(22) .eq. 1) .or.
     1         (MTPDYN .eq. 2 .and. ICYLFL(8) .eq. 1)) call frcg01
      endif
c
c...3-axis circular was just performed
c...force output of linear interp code
c
      if (IC3AXF(8) .eq. 1 .and. ((ISCIRC .eq. 0 .or.
     1    (ISCIRC .eq. 1 .and. ICIPRM(3) .ne. 0)) .or.
     2    (ISCIRC .eq. 0 .and. ICRFMT .eq. 10))) then
          IC3AXF(8) = 0
          if (ISCIRC .eq. 1) then
              ireg   = CIRCOD(1)
              call regtyp (ireg,CIRCDV(1))
              REGFRC(ireg) = 3
          else
              call frcg01
          endif
c
c......Make sure 3-D circle code is output
c......next time around
c
          isav   = ICIPRM(3)
          ICIPRM(3) = 0
          call mcigcd (ICIPRM(5),ireg,vreg)
          ICIPRM(3) = isav
          vreg   = vreg * (0-1) - 1
          call setcod (ireg,vreg)
      endif
c
c...Rapid move
c
      if (IRAP .eq. 1) then
          ifl    = 0
          if (ICYCSW(4) .eq. 1 .and. CYCCOD(15) .ne. 0) then
              if (ICYCFL(22) .eq. 1 .and. MOTCOD .ne. 0) then
                  ireg   = MOTCOD
                  call regtyp (ireg,MOTCDV)
                  REGFRC(ireg) = -1
              endif
              call codout (CYCCOD(15),CYCCDV(15))
          else if (IPOLIN .eq. 1) then
              call codout (POLCOD(2),POLCDV(2))
          else
              if (LTMODE .eq. 0 .or. LTMODE .eq. 3) then
                 if (LNCRAP .eq. 1) ifl    = 1
                 do 600 i=1,NRAPCD,1
                     if (RAPCD(i) .eq. -1 .or. (RAPCD(i) .ge. 18 .and.
     1                   RAPCD(i) .le. 28)) ifl = 0
                     call codout (RAPCD(i),RAPVL(i))
  600            continue
              else if (LTMODE .eq. 1) then
                 call codout (FRAPCD,FRAPVL)
              else if (LTMODE .eq. 2) then
                 call codout (DRAPCD,DRAPVL)
              end if
          endif
          if (IFDFRC(3) .eq. 1 .or. IFDFRC(4) .eq. 1) IRAPSW = 1
c
c...Setup feedrate for output
c
      else
          call frnout (kfl)
c
c...Slowdown is active
c...Output appropriate slowdown code
c
          ifl    = 1
          if (SLWFL(1) .eq. 1 .and. (ISLWDN .eq. 1 .or. ISLWDN .eq. 3))
     1            then
c
c......Determine slowdow code to output
c
              if (ISCIRC .eq. 1) then
                  is     = icslw(ISLWNM) + ICIPRM(5)
              else
                  is     = ISLWNM
              endif
c
c......Force out slowdown code
c
              if (SLWFL(3) .eq. 1) then
                  ireg   = SLWCD(is)
                  call regtyp (ireg,SLWVL(is))
                  if (ireg .gt. 0 .and. REGFRC(ireg) .eq. 0)
     1                    REGFRC(ireg) = 1
              endif
c
c......Output slowdown code
c
              call codout (SLWCD(is),SLWVL(is))
              if (SLWFL(2) .eq. 1) ifl = 0
              if (ISLWDN .eq. 3) ISLWDN = 5
c
c...Cancel slowdown
c
          else if (SLWFL(1) .eq. 1 .and. (ISLWDN .eq. 4 .or.
     1             ISLWDN .eq. 5)) then
              call codout (SLWCD(13),SLWVL(13))
              if (ISLWDN .eq. 5) ISLWDN = 2
          endif
      endif
c
c...Output cutting mode
c......Circular interpolation
c
      if (ISCIRC .eq. 1) then
          if (ipdid .eq. 0 .and. ICIPRM(3) .ne. 0)
     1        call codout (PLNCOD(4-MCHPLN),PLNCDV(4-MCHPLN))
          if (ifl .eq. 1 .and. ICRFMT .ne. 10) then
              call mcigcd (ICIPRM(5),ireg,vreg)
              if (ICRDIR .eq. 1) then
                  call regtyp (ireg,vreg)
                  if (ireg .gt. 0 .and. REGFRC(ireg) .eq. 0)
     1                REGFRC(ireg) = 1
              endif
              call codout (ireg,vreg)
          endif
      else if (ISBSPL .eq. 1) then
c
c...B-spline interpolation
c
c
c......Linear interpolation
c
      else if (IFOTYP .ne. 3 .or. IDPMOV .ne. 1) then
          if (ifl .eq. 1) then
              if (IPOLIN .eq. 1) then
                  call codout (POLCOD(1),POLCDV(1))
              else if (MTPDYN .eq. 2 .and. LTMODE .eq. 1) then
                  call codout (LFLMCD,FLMCDV)
              else if (MTPDYN .eq. 2 .and. LTMODE .eq. 2) then
                  call codout (LDLMCD,DLMCDV)
              else
                  call codout (MOTCOD,MOTCDV)
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
c   SUBROUTINE:  motmcd (kfl,kcnt)
c
c   FUNCTION:  This routine outputs the folloing codes, which are nece-
c              sarry following a motion block.
c
c              1.  Clamp axis code(s).
c
c   INPUT:  kfl     I*4  D10 - 1 = This axis is waiting to be output.
c
c           kcnt    I*4  D1  - Number of axes waiting to be output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine motmcd (kfl,kcnt)
c
      include 'post.inc'
c
      integer*4 kfl(10),kcnt
c
      equivalence (IRTCLM,KPOSMP(1306))
      equivalence (CLMPCD,KPOSMP(1307)), (ICLMPF,KPOSMP(1351))
      equivalence (IRTDEF,KPOSMP(1485)), (CLMPUB,KPOSMP(1757))
c
      integer*4 IRTDEF,IRTCLM,CLMPCD(2,10),ICLMPF(10),CLMPUB(2,10)
c
      equivalence (CLMPVL,POSMAP(1454))
c
      real*8 CLMPVL(2,10)
c
      integer*4 i,inc,ifrc
c
c...Clamp axes if necessary
c
      if (IRTCLM .eq. 2) then
          inc    = 0
          do 100 i=7,IRTDEF+6,1
              if (kfl(i) .eq. 1 .and. CLMPCD(1,i) .ne. 0 .and.
     1            ICLMPF(i) .eq. 0) then
                  inc    = 1
                  call codout (CLMPCD(1,i),CLMPVL(1,i))
                  ICLMPF(i) = 1
                  if (CLMPUB(1,i) .ne. 0)
     1                call frcblk (CLMPUB(1,i),ifrc,1)
              endif
  100     continue
          if (inc .eq. 1) call clrbuf
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  motout (gmch,kfl,kcnt)
c
c   FUNCTION:  This routine outputs a single motion block.
c
c   INPUT:  gmch    R*8  D10 Machine slide positions for primary and
c                            secondary linear XYZ axes.
c
c           kfl     I*4  D6  1 = This axis is waiting to be output.
c
c           kcnt    I*4  D1  Number of linear axes waiting to be output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine motout (gmch,kfl,kcnt)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271))
      equivalence (MOTREG,KPOSMP(0381)), (REGFRC,KPOSMP(0603))
      equivalence (IRTOUT,KPOSMP(0813)), (MOTFLG,KPOSMP(1073))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (INCR  ,KPOSMP(1226)), (ICIPRM,KPOSMP(1230))
      equivalence (ISCIRC,KPOSMP(1238))
      equivalence (IPOLIN,KPOSMP(1265)), (ICIPR2,KPOSMP(1266))
      equivalence (ISACIR,KPOSMP(1346)), (IJKROT,KPOSMP(1739))
      equivalence (ICRFMT,KPOSMP(1368)), (IHELIX,KPOSMP(1392))
      equivalence (FMTDES,KPOSMP(2133)), (FEDCD ,KPOSMP(3162))
      equivalence (ICRHEL,KPOSMP(4250))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 MOTFLG,INCR,MOTREG(24),ICIPRM(8),ISCIRC,ICRFMT,
     1          ICIPR2(8),IPOLIN,IRTOUT(4),REGFRC(MAXFMT),FEDCD(8),
     2          ISACIR,ICYCSW(5),MACHTP,IJKROT,IHELIX,ICRHEL(10)
c
      equivalence (RCIPRM,POSMAP(2049)), (RCIPR2,POSMAP(2074))
c
      real*8 RCIPRM(25),RCIPR2(25)
c
      integer*4 kfl(6),kcnt,isav(8)
c
      real*8 gmch(10)
c
      integer*4 inc,i,irsav(4),ityp,ierr,nst,iax(6),imode
c
      real*8 rnum
c
      character*80 lmsg
c
      data iax /1,1,2,2,3,3/
c
c...Save feed rate force values
c...just in case they were disabled using the FORCE command,
c...because 'frnout' may force their output with a mode
c...change or FEDRAT command.
c
      do 50 i=1,8,1
          if (FEDCD(i) .ne. 0) isav(i) = REGFRC(FEDCD(i))
   50 continue
c
c...Setup motion G-codes for output
c
      call motgcd (kfl,kcnt)
c
c...Output circular codes
c
      if (ISCIRC .eq. 1) then
          if (ICRFMT .eq. 7 .or. ICRFMT .eq. 10) then
              call polout (RCIPRM,ICIPRM,gmch,kfl,kcnt)
              if (kcnt .eq. 0) then
                  call clrbuf
                  go to 2000
              endif
          else
              call cirijk (RCIPRM,ICIPRM)
          endif
c
c...Polar coordinate linear move
c
      else if (IPOLIN .eq. 1) then
          call polout (RCIPR2,ICIPR2,gmch,kfl,kcnt)
          if (kcnt .eq. 0) then
              call clrbuf
              go to 2000
          endif
      endif
c
c...Output linear axis
c
      MOTFLG = 1
      inc    = 1
      do 500 i=1,6,1
          if (kfl(i) .eq. 1) then
c
c......Helical interpolation
c......3rd axis may be output in special mode
c
              imode  = INCR
              if (IHELIX .eq. 1 .and. ICIPRM(3) .eq. iax(i) .and.
     1            ICRHEL(6) .ne. 0) imode = ICRHEL(6)
c
c......Absolute mode
c
              if (imode .eq. 1) then
                  call codout (MOTREG(inc),gmch(i))
c
c......Incremental mode
c
              else
                  call increg (MOTREG(inc),gmch(i),rnum)
                  call setcod (MOTREG(inc),gmch(i))
                  call codout (MOTREG(inc+1),rnum)
              endif
          endif
c
          inc    = inc    + 2
  500 continue
c
      inc    = 11
      nst    = 1
      if (IJKROT .eq. 1) then
         call ijkout (gmch(7),1)
         nst = 4
         inc = 20
         if (MACHTP .ne. 3) nst = 5
      end if
      do 600 i=nst,4,1
          if (IRTOUT(i) .eq. 0) go to 600
          inc    = inc    + 3
          irsav(i) = FMTDES(3,MOTREG(inc))
          if (kfl(i+6) .eq. 1) call rotout (gmch(i+6),i,1)
  600 continue
c
c...Clear output buffer
c
  800 call clrbuf
c
c...Output simulation record
c
      ityp   = 2
      if (ISACIR .ne. 0) ityp = 3
      if (ICYCSW(1) .ne. 0) ityp = 4
      call simmot (ityp,lmsg,ierr)
c
c...Reset FORCE values for feed rates
c...which were saved above
c
      do 850 i=1,8,1
          if (FEDCD(i) .ne. 0) then
              if (isav(i) .eq. -2 .or. isav(i) .eq. 2)
     1                REGFRC(FEDCD(i)) = isav(i)
          endif
  850 continue
c
c...Reset rotary sign format flags
c
      inc    = 11
      do 1000 i=nst,4,1
          if (IRTOUT(i) .eq. 0) go to 1000
          inc    = inc    + 3
          FMTDES(3,MOTREG(inc)) = irsav(i)
 1000 continue
c
c...Output trailing motion codes
c
 2000 call motmcd (kfl,kcnt)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  polout (gprm,kprm,gmch,kfl,kcnt)
c
c   FUNCTION:  This routine formats outputs polar coordinate circular
c              and linear records and inhibits the linear axis from
c              being output.
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           gmch    R*8  D10 -  Current output axes position.
c
c           kfl     I*4  D10 -  1 = This axis is waiting to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine polout (gprm,kprm,gmch,kfl,kcnt)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (REGFRC,KPOSMP(0603))
      equivalence (MOTFLG,KPOSMP(1073)), (IPOLIN,KPOSMP(1265))
c
      integer*4 MOTREG(24),MOTFLG,REGFRC(MAXFMT),IPOLIN
c
      integer*4 kprm(8),kfl(10),kcnt
c
      real*8 gprm(25),gmch(10)
c
      integer*4 inc,i,isub(6)
c
      data isub /1,1, 2,2, 3,3/
c
c...Initialize routine
c
      MOTFLG = 1
c
c...Output circular parameters
c
      call cirijk (gprm,kprm)
c
c...Set linear output registers
c
      inc    = 1
      do 100 i=1,6,1
          if (kfl(i) .eq. 1 .and. (isub(i) .eq. kprm(1) .or.
     1                          isub(i) .eq. kprm(2))) then
              call setcod (MOTREG(inc),gmch(i))
              kfl(i) = 0
              kcnt   = kcnt   - 1
              REGFRC(MOTREG(inc)) = 0
          endif
          inc    = inc    + 2
  100 continue
c
c...End of routine
c
 8000 IPOLIN = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotout (grot,kax,kfl)
c
c   FUNCTION:  This routine formats and outputs the rotary axis posi-
c              tions.
c
c   INPUT:  grot    R*8  D1  Current position for rotary axis.
c
c           kax     I*4  D1  Which rotary axis to output in the range of
c                            1-4.
c
c           kfl     I*4  D1  1 = Output this rotary axis.  2 = Format
c                            and set the register value only, do not
c                            output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rotout (grot,kax,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (INCR  ,KPOSMP(1226))
      equivalence (IRTSCL,KPOSMP(1288))
      equivalence (IRTCLW,KPOSMP(1292)), (AXSINC,KPOSMP(1296))
      equivalence (ROTDCD,KPOSMP(1405)), (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 MOTREG(24),INCR,IRTSCL(4),IRTCLW(4),AXSINC(10),
     1          ROTDCD(2,4)
c
      equivalence (AXSSTO,POSMAP(1425)), (ROTCRM,POSMAP(1439))
      equivalence (ROTDVL,POSMAP(2041)), (ROTZER,POSMAP(4555))
c
      real*8 AXSSTO(10),ROTCRM(4),ROTZER(2),ROTDVL(2,4)
c
      integer*4 kax,kfl
c
      real*8 grot
c
      integer*4 isub(4),inc,idir,jdir,inum
c
      data isub /13,16,19,22/
c
      real*8 rnum,rdlt,dir,ang,tnum
c
c...Set up output value for rotary axis
c
      inc    = isub(kax)
      rnum   = grot
      if (IRTCLW(kax) .eq. 2) rnum = grot * (-1.)
c
c...Calculate rotary axis direction
c
      dir    = grot - AXSSTO(kax+6)
      if (dir .eq. 0.) then
          idir   = 1
          jdir   = 0
      else
          idir   = 1 * (dir/dabs(dir))
          if (IRTCLW(kax) .eq. 2) idir = 0 - idir
          jdir = 1
          if (idir .eq. -1) jdir = 2
      endif
c
c...Absolute mode
c......Linear scale
c
      if ((INCR .eq. 1 .or. AXSINC(kax+6) .eq. 2) .and.
     1        AXSINC(kax+6) .ne. 3)  then
          if (IRTSCL(kax) .eq. 1) then
c             rnum   = rnum * ROTCRM(kax)
c
c......Rotary scale
c
          else
              rnum   = dmod(rnum,360.d0)
              if (rnum .lt. 0.) rnum = rnum + 360.
              call codint (MOTREG(inc+1),rnum,tnum,inum)
              if (tnum .eq. 360.) tnum = 0.
              if (tnum .eq. 0. .and. idir .eq. -1 .and.
     1                MOTREG(inc+1) .ne. 0) then
                  if (FMTDES(3,MOTREG(inc+1)) .ne. 0) then
                      if (FMTDES(3,MOTREG(inc+1)) .eq. 3) then
                         FMTDES(3,MOTREG(inc+1)) = 13
                      else
                         FMTDES(3,MOTREG(inc+1)) = 2
                      endif
                  endif
                  rnum   = ROTZER(1)
              else if (tnum .eq. 0. .and. idir .eq. 1) then
                  rnum   = ROTZER(2)
              endif
              if (idir .eq. -1) rnum = rnum * (-1.)
          endif
c
c......Output rotary axis
c
          rnum   = rnum * ROTCRM(kax)
          if (kfl .eq. 1) then
              call codout (MOTREG(inc+1),rnum)
              if (jdir .ne. 0)
     1            call codout (ROTDCD(jdir,kax),ROTDVL(jdir,kax))
          else
              call setcod (MOTREG(inc+1),rnum)
          endif
c
c...Incremental mode
c

      else
          if (IRTSCL(kax) .eq. 1) then
              rnum   = rnum   * ROTCRM(kax)
              call increg (MOTREG(inc+1),rnum,rdlt)
          else
              rnum   = dmod(rnum,360.d0)
c             if (rnum .lt. 0.) rnum = rnum + 360.
              rnum   = rnum   * ROTCRM(kax)
              ang    = grot * ROTCRM(kax)
              call increg (MOTREG(inc),ang,rdlt)
              if (IRTCLW(kax) .eq. 2) rdlt = 0.d0 - rdlt
          endif
          call setcod (MOTREG(inc+1),rnum)
          if (kfl .eq. 1) then
              call codout (MOTREG(inc+2),rdlt)
              if (jdir .ne. 0)
     1            call codout (ROTDCD(jdir,kax),ROTDVL(jdir,kax))
          else
              call setcod (MOTREG(inc+2),rdlt)
          endif
      endif
c
c...Store internal rotary axis
c
      call setcod (MOTREG(inc),grot)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ijkout (gvec,kfl)
c
c   FUNCTION:  This routine formats and outputs the rotary axis posi-
c              tions.
c
c   INPUT:  gvec    R*8  D3  Ignored.
c
c           kfl     I*4  D1  0 = Store tool axis vector only,
c                            1 = Output tool axis vector.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine ijkout (gvec,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (INCR  ,KPOSMP(1226))
      equivalence (IJKREG,KPOSMP(0827))
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 MOTREG(24),INCR,IJKREG(3)
c
      equivalence (AXSSTO,POSMAP(1425)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372))
c
      real*8 AXSSTO(10),TLVEC(3),VECSAV(3)
c
      real*8 gvec(3)
c
      integer*4 i,kfl,inum
c
      real*8 rvec(3),rnum
c
c...Adjust for transformations
c
      call ptxfm (TLVEC,rvec,2)
c
c...When one component has the value of 1.
c...then the other two must have a vaule of 0.
c
      call codint (IJKREG(3),rvec(3),rnum,inum)
      if (dabs(rnum) .eq. 1.) then
          rvec(1) = 0.
          rvec(2) = 0.
      else
         call codint (IJKREG(2),rvec(2),rnum,inum)
         if (dabs(rnum) .eq. 1.) then
             rvec(1) = 0.
             rvec(3) = 0.
         else
             call codint (IJKREG(1),rvec(1),rnum,inum)
             if (dabs(rnum) .eq. 1.) then
                 rvec(2) = 0.
                 rvec(3) = 0.
             endif
         endif
      endif
c
c...Absolute mode only
c
      do 115 i=1,3,1
          if (kfl .eq. 1) then
              call codout (IJKREG(i),rvec(i))
          else
              call setcod (IJKREG(i),rvec(i))
          endif
  115 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mcigcd (kdir,kreg,greg)
c
c   FUNCTION:  This routine formats circular interpolation code register.
c
c   INPUT:  kdir    I*4  D1  Circular direction (ICIPRM(5))
c
c   OUTPUT: kreg    I*4  D1  Register number used to output code.
c
c           greg    R*8  D1  Code value to set up register
c
c***********************************************************************
c
      subroutine mcigcd (kdir,kreg,greg)
c
      include 'post.inc'
      integer*4 kdir,kreg
      real*8 greg
c
      equivalence (MACHTP,KPOSMP(1201)), (ICIPRM,KPOSMP(1230))
      equivalence (IHELIX,KPOSMP(1392))
      equivalence (CIRCOD,KPOSMP(1378)), (IC3AXF,KPOSMP(1747))
      equivalence (LFIRCD,KPOSMP(4112))
      equivalence (LDIRCD,KPOSMP(4118)), (LTMODE,KPOSMP(4125))
      equivalence (MTPDYN,KPOSMP(4126)), (ICRHEL,KPOSMP(4250))
c
      integer*4 MACHTP,MTPDYN,CIRCOD(6),LFIRCD(6),LDIRCD(6),
     -          LTMODE,IHELIX,ICRHEL(10),ICIPRM(8),IC3AXF(10)
c
      equivalence (CIRCDV,POSMAP(2206)), (HELCDV,POSMAP(2242))
      equivalence (C3XCDV,POSMAP(2244))
      equivalence (CFRCDV,POSMAP(4595)), (CDRCDV,POSMAP(4597))
c
      real*8 CIRCDV(2),CFRCDV(2),CDRCDV(2),HELCDV(2),C3XCDV(2)
c

      if (MTPDYN .eq. 2 .and. LTMODE .eq. 1) then
         kreg = LFIRCD(3-kdir)
         greg = CFRCDV(3-kdir)
      else if (MTPDYN .eq. 2 .and. LTMODE .eq. 2) then
         kreg = LDIRCD(kdir)
         greg = CDRCDV(kdir)
      else if (ICIPRM(3) .eq. 0) then
            kreg = IC3AXF(kdir+2)
            greg = C3XCDV(kdir)
      else
         if (IHELIX .eq. 0) then
            kreg = CIRCOD(kdir)
            greg = CIRCDV(kdir)
         else
            kreg = ICRHEL(3+kdir)
            greg = HELCDV(kdir)
         end if
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lcdout (gaxs,kfl,kcnt)
c
c   FUNCTION:  This routine formats circular interpolation code register.
c
c   INPUT:  kdir    I*4  D1  Circular direction (ICIPRM(5))
c
c   OUTPUT: kreg    I*4  D1  Register number used to output code.
c
c           greg    R*8  D1  Code value to set up register
c
c***********************************************************************
c
      subroutine lcdout (gaxs,kfl,kcnt)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kcnt(10)
c
      real*8 gaxs(10)
c
      equivalence (MCHOPT,KPOSMP(0308)), (ICIPRM,KPOSMP(1230))
      equivalence (ISCIRC,KPOSMP(1238)), (MACHTP,KPOSMP(1201))
      equivalence (FMTDES,KPOSMP(2133)), (LFDLCD,KPOSMP(4107))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
c
      integer*4 MACHTP,MTPDYN,LTMODE,ISCIRC,LFDLCD,MCHOPT(4),
     -          ICIPRM(8)
c
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (AXSSTO,POSMAP(1425)), (RCIPRM,POSMAP(2049))
      equivalence (RDRLMT,POSMAP(3538)), (FEED  ,POSMAP(3547))
      equivalence (FMVTIM,POSMAP(3553)), (MILTOL,POSMAP(4879))
c
      real*8 MILTOL,AXSSTO(10),RDRLMT,FEED(4),RCIPRM(25),RAD,FMVTIM
c
      real*8 dis,ndist,pt(6),vec(3),smn,cr1,cr2,d(2),dfi,tol,pnt(3),
     -       sgn,rat,crt,vsm(3),dlt,d2,vc(3),vzer(3),sped,cr,a
c
      integer*4 k
c
      data vzer /0.,0.,0./
c
c...In LMDP mode only circles requre L code
c
      if (LTMODE .eq. 2) then
          if (ISCIRC .eq. 1) go to 2000
          go to 8000
      end if
c
c...make sure that linear axis move
c
      if (kcnt(1)+kcnt(2)+kcnt(3)+kcnt(4) .eq. 0) go to 8000
c
c...Polar interpolation (LMFP mode)
c
      pt(1) = AXSSTO(1)
      pt(2) = AXSSTO(3)
      pt(3) = 0.
      pt(4) = gaxs(1)
      pt(5) = gaxs(3)
      pt(6) = 0.
      call vcplvc (pt(4),pt(1),vec,-1.d0)
      dis   = ndist (pt,pt(4))
      if (ISCIRC .ne. 1) then
c
c...get minimum L span and select closest to the center
c...point, since it is the worse case
c
         smn   = RDRLMT * FEED(1)
         if (smn .gt. dis) go to 7000
         d(1)  = ndist (pt(1),vzer)
         d(2)  = ndist (pt(4),vzer)
         k     = 1
         sgn   = 1.d0
         if (d(1) .gt. d(2)) then
             k = 2
             sgn = -1.d0
         end if
         if (d(k) .lt. .001) go to 7000
c
c...check if move cross center of rotary axis
c
         call betvec (pt(1),pt(4),a)
         if (a .lt. .01) go to 7000
         call copyn (pt(k*3-2),pnt,3)
         rat   = smn / dis
         crt   = 1.0
c
c...get curvature radius and chordal error
c
  100    dlt   = crt * smn
         call vcplvc (vzer,vec,vsm,crt*rat)
         call vcplvc (pnt,vsm,vc,sgn)
         d2    = ndist (vc,vzer)
         call betvec (pnt,vc,dfi)
         sped  = sgn * (d2 - d(k)) / dfi
         call crad (sped,dfi,d(k),cr1,cr2)
         cr    = .5 * (cr1 + cr2)
         tol   = cr - dsqrt(cr**2-.25*dlt*dlt)
c
c...fix error if necessary by decreasing step L
c...thru feed rate reduction
c
         if (tol .gt. MILTOL) then
            crt  = crt * .7
            go to 100
         end if
         FMVTIM = FMVTIM / crt
         smn   = dlt
      else
c
c...circle record in polar coordinates: for now we assume
c...that N/C control takes care of min span calculation
c...and only chordal error of arc is considered, but it may
c...be not enough and spiral curvature should be considered
c
         go to 2000
      end if
      go to 7000
c
c...Cylindrical interpolation:
c...get min L-code only for circles
c
 2000 a      = .5 * RDRLMT * FEED(1) / RCIPRM(4)
      if (a .gt. 1.d0) then
         crt = .9999 / a
      else
         crt = 1.d0
      end if
      a      = a * crt
      smn    = 2. * dasin (a)
c
c...check for chordal tolerance
c
      if (MILTOL .lt. RCIPRM(4)) then
         dlt = 2. * dacos (1.d0 - MILTOL/RCIPRM(4))
      else
         dlt = smn
      end if
      if (smn .gt. dlt) crt = crt * dlt / smn
      smn    = crt * smn * RAD
      FMVTIM = FMVTIM / crt
c
c...make sure L code is not zero
c...and output it
c
 7000 a      = 10.d0**(-FMTDES(5+MCHOPT(2),LFDLCD))
      if (smn .lt. a) smn = a
      call codout (LFDLCD,smn)

 8000 return
      end



c
c***********************************************************************
c
c   SUBROUTINE:  clmput (gaxs,kfl)
c
c   FUNCTION:  This routine outputs the clamp/unclamp rotary axes codes
c
c   INPUT:  gaxs    R*8  D10 - Output axes values.
c
c           kfl     I*4  D10 - 1 = This axis is waiting to be output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clmput (gaxs,kfl)
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381))
      equivalence (MACHTP,KPOSMP(1201)), (IRTCLM,KPOSMP(1306))
      equivalence (CLMPCD,KPOSMP(1307)), (CLMPBK,KPOSMP(1327))
      equivalence (ICLMPF,KPOSMP(1351)), (IRTDEF,KPOSMP(1485))
      equivalence (CLMPUB,KPOSMP(1757)), (IRAP  ,KPOSMP(3199))
c
      integer*4 IRTCLM,IRTDEF,CLMPCD(2,10),CLMPBK,ICLMPF(10),
     1          CLMPUB(2,10),MOTREG(24),IRAP,MACHTP
c
      equivalence (PPTOLR,POSMAP(1274)), (CLMPVL,POSMAP(1454))
c
      real*8 CLMPVL(2,10),PPTOLR(10)
c
      integer*4 kfl(10)
c
      real*8 gaxs(10)
c
      integer*4 i,inc,ifrc,imov,ifl(10),isub(4)
c
      data isub /13,16,19,22/
c
      if (MACHTP .eq. 5 .and. IRAP .eq. 1) go to 8000
      if (IRTCLM .ne. 1) then
c
c...Determine if any rotary axis moves
c
          imov   = 0
          do 50 i=7,IRTDEF+6,1
              inc    = isub(i-6)
              if (IRTCLM .eq. 5) then
                  call cmpcod (MOTREG(inc),gaxs(i),PPTOLR(i),1,ifl(i))
              else
                  ifl(i) = kfl(i)
              endif
              imov   = imov   + ifl(i)
   50     continue
c
          inc    = 0
          do 100 i=7,IRTDEF+6,1
c
c...Unclamp axis
c
              if ((ifl(i) .eq. 1 .or. (IRTCLM .eq. 4 .and. imov .ne. 0))
     1            .and. ICLMPF(i) .eq. 1) then
                  ICLMPF(i) = 0
                  if (CLMPCD(2,i) .ne. 0) then
                      inc    = 1
                      call codout (CLMPCD(2,i),CLMPVL(2,i))
                      if (CLMPUB(2,i) .ne. 0)
     1                    call frcblk (CLMPUB(2,i),ifrc,1)
                  endif
c
c...Clamp axis
c
              else if ((ifl(i) .eq. 0 .and.
     1              (IRTCLM .ne. 4 .or. imov .eq. 0)) .and.
     2              ICLMPF(i) .eq. 0) then
                  ICLMPF(i) = 1
                  if (CLMPCD(1,i) .ne. 0) then
                      inc    = 1
                      call codout (CLMPCD(1,i),CLMPVL(1,i))
                      if (CLMPUB(1,i) .ne. 0)
     1                    call frcblk (CLMPUB(1,i),ifrc,1)
                  endif
              endif
  100     continue
          if (inc .eq. 1 .and. CLMPBK .eq. 1) call clrbuf
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  frcg01
c
c   FUNCTION:  This routine outputs forces the output of the Motion
c              interpolation code (G00, G01, etc.).
c
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine frcg01
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGFRC,KPOSMP(0603)), (MOTCOD,KPOSMP(1240))
c
      integer*4 MOTCOD,REGFRC(MAXFMT)
c
      equivalence (MOTCDV,POSMAP(1284))
c
      real*8 MOTCDV
c
      integer*4 ireg
c
c...Force output of linear interp code
c
      if (MOTCOD .ne. 0) then
          ireg   = MOTCOD
          call regtyp (ireg,MOTCDV)
          REGFRC(ireg) = 3
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getacy (kinc,gacy)
c
c   FUNCTION:  This routine returns the accuracy/tolerance used for the
c              output linear or rotary axes.
c
c   INPUT:  kinc    I*4  D1  - 1 = Return linear axes accuracy,
c                              2 = Rotary axes accuracy.
c
c   OUTPUT: gacy    R*8  D1  - Accuracy used for axes output, i.e. .0001.
c
c***********************************************************************
c
      subroutine getacy (kinc,gacy)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (MOTREG,KPOSMP(0381)), (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 MOTREG(24),MCHOPT(20)
c
      integer*4 kinc
c
      real*8 gacy
c
c...Linear axis accuracy
c
      if (kinc .eq. 1) then
          gacy   = 10.d0**(-FMTDES(5+MCHOPT(2),MOTREG(1)))
c
c...Rotary axis accuracy
c
      else
          gacy   = 10.d0**(-FMTDES(5+MCHOPT(2),MOTREG(13)))
      endif
c
c...End of routine
c
 8000 return
      end

