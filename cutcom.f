c
c***********************************************************************
c
c   FILE NAME:  cutcom
c   CONTAINS:
c               cutcom  cuton   cutoff
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cutcom.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:05
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cutcom
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 CUTCOM/ON
c                        OFF(,MODIFY,n,v)
c
c                 CUTCOM/LEFT (,XYPLAN)(,d)(,NORMAL)(,MODIFY,n,v) $
c                        RIGHT  ZXPLAN       PERPTO
c                               YZPLAN
c
c                        (,XAXIS,x)(,YAXIS,y)(,ZAXIS,z)(,MANUAL)
c
c                 CUTCOM/ENDPT(,d)(,TOOL,d,r)(,MAXIPM,f)(,ROTREF,n)(,PS) $
c                                                                    DS
c                        (,XAXIS,x)(,YAXIS,y)(,ZAXIS,z)(,MANUAL)
c
c                 CUTCOM/ADJUST,ON
c                               OFF
c
c                 CUTCOM/ADJUST/n(,PLUS )(,XAXIS(,x))(,YAXIS,(y)) $
c                                  MINUS
c
c                        (,ZAXIS(,z))
c
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cutcom
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICYCSW,KPOSMP(0271)), (MCHPLN,KPOSMP(1261))
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
      equivalence (ICUTDO,KPOSMP(3301))
c
      integer*4 MXCL,IPSTWD(50),CUTCFL(20),CUTCCD(30),ICUTDO(15),MCHPLN,
     1          ICYCSW(5)
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (CUTTER,POSMAP(0744)), (RCUTDO,POSMAP(2229))
      equivalence (CTOFRG,POSMAP(2401)), (CUTCVR,POSMAP(2402))
      equivalence (CUTCVL,POSMAP(2410))
      equivalence (LSTTN ,POSMAP(3990)), (TLOFRG,POSMAP(3991))
c
      real*8 METCNV,PSTWD(50),CTOFRG,TLOFRG,CUTCVR(8),CUTCVL(30),
     1       RCUTDO(10),LSTTN,CUTTER(7)
c
      integer*4 inc,ifl(10),i,ierr,ireg
c
      real*8 fxd,rdis,rax(3),tool(2),fipm,frot
c
      character*80 lmsg
c
c...CUTCOM/ON
c
      inc    = 1
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 71) then
          if (ICYCSW(1) .ne. 0) go to 9900
          ICUTDO(1) = 1
          ICUTDO(2) = 1
          CTOFRG = CUTCVR(5)
          call cuton
c
c...CUTCOM/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          ICUTDO(1) = 0
          ICUTDO(2) = 0
c
c......CUTCOM/OFF,MODIFY,n,v
c
          inc    = 2
          if (inc .le. MXCL) then
c
c.........Check syntax
c
              if (IPSTWD(inc) .ne. 55) then
                  if (IPSTWD(inc) .eq. 0) go to 9500
                  go to 9100
              endif
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .lt. 1. .or. PSTWD(inc) .gt. 3) go to 9400
              ifl(1) = PSTWD(inc)
              inc    = inc    + 1
c
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rdis = PSTWD(inc)
c
c.........Output ADJUST codes
c
              if (ICUTDO(6) .ne. 0 .and. CUTCFL(3) .eq. 1) then
                  inc    = ifl(1) + 11
                  call codout (CUTCCD(inc),CUTCVL(inc))
                  call codout (CUTCCD(15),rdis)
              endif
          endif
c
c......Output CUTCOM/OFF code
c
          call cutoff
c
c...CUTCOM/LEFT
c...       RIGHT
c
      else if (IPSTWD(1) .eq. 8 .or. IPSTWD(1) .eq. 24) then
          if (ICYCSW(1) .ne. 0) go to 9900
          do 30 i=1,8,1
              ifl(i) = 0
   30     continue
          fxd    = CTOFRG
   40     inc    = inc    + 1
          if (inc .gt. MXCL) go to 80
c
c......CUTCOM/,XYPLAN
c
          if (IPSTWD(inc) .eq. 33) then
              if (ifl(1) .ne. 0) go to 9600
              ifl(1) = 1
c
c......CUTCOM/,ZXPLAN
c
          else if (IPSTWD(inc) .eq. 41) then
              if (CUTCFL(2) .ne. 1) go to 9800
              if (ifl(1) .ne. 0) go to 9600
              ifl(1) = 2
c
c......CUTCOM/,YZPLAN
c
          else if (IPSTWD(inc) .eq. 37) then
              if (CUTCFL(2) .ne. 1) go to 9800
              if (ifl(1) .ne. 0) go to 9600
              ifl(1) = 3
c
c......CUTCOM/,d
c
          else if (IPSTWD(inc) .eq. 0) then
              if (ifl(2) .ne. 0) go to 9600
              ifl(2) = 1
              fxd    = PSTWD(inc)
c
c......CUTCOM/,NORMAL
c
          else if (IPSTWD(inc) .eq. 111) then
              if (ifl(3) .ne. 0) go to 9600
              ifl(3) = 1
c
c......CUTCOM/,PERPTO
c
          else if (IPSTWD(inc) .eq. 18) then
              if (CUTCFL(1) .ne. 1) go to 9800
              if (ifl(3) .ne. 0) go to 9600
              ifl(3) = 2
c
c......CUTCOM/,MODIFY,n,v
c
          else if (IPSTWD(inc) .eq. 55) then
              if (ifl(4) .ne. 0) go to 9600
              inc    = inc    + 1
c
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .lt. 1. .or. PSTWD(inc) .gt. 3) go to 9400
              ifl(4) = PSTWD(inc)
              inc    = inc    + 1
c
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rdis = PSTWD(inc)
c
c......CUTCOM/,XAXIS,x
c
          else if (IPSTWD(inc) .eq. 84) then
              if (ifl(5) .ne. 0) go to 9600
              ifl(5) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rax(1) = PSTWD(inc) * METCNV
c
c......CUTCOM/,YAXIS,z
c
          else if (IPSTWD(inc) .eq. 85) then
              if (ifl(6) .ne. 0) go to 9600
              ifl(6) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rax(2) = PSTWD(inc) * METCNV
c
c......CUTCOM/,ZAXIS,z
c
          else if (IPSTWD(inc) .eq. 86) then
              if (ifl(7) .ne. 0) go to 9600
              ifl(7) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rax(3) = PSTWD(inc) * METCNV
c
c......CUTCOM/,MANUAL
c
          else if (IPSTWD(inc) .eq. 158) then
              if (ifl(8) .ne. 0) go to 9600
              ifl(8) = 1
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          go to 40
c
c......Set up cutcom parameters
c
   80     if (ifl(1) .eq. 0) ifl(1) = ICUTDO(4)
          if (ifl(3) .eq. 0) ifl(3) = ICUTDO(5)
          ICUTDO(1) = 1
          ICUTDO(2) = 1
          ICUTDO(3) = 1
          if (IPSTWD(1) .eq. 24) ICUTDO(3) = 2
          ICUTDO(4) = ifl(1)
          MCHPLN = 4 - ICUTDO(4)
          ICUTDO(5) = ifl(3)
          ICUTDO(6) = ifl(4)
          ICUTDO(7) = ifl(5)
          ICUTDO(8) = ifl(6)
          ICUTDO(9) = ifl(7)
          ICUTDO(10) = ifl(8)
c
          CTOFRG = fxd    + CUTCVR(3)
          RCUTDO(1) = rdis
          RCUTDO(2) = rax(1)
          RCUTDO(3) = rax(2)
          RCUTDO(4) = rax(3)
          call cuton
c
c......Output Simulation record
c
          call simsta (0,lmsg,ierr)
c
c...CUTCOM/ENDPT
c
      else if (IPSTWD(1) .eq. 664) then
          if (CUTCFL(17) .ne. 1) go to 9800
          if (ICYCSW(1) .ne. 0) go to 9900
          do 130 i=1,10,1
              ifl(i) = 0
  130     continue
          fxd    = CTOFRG
          tool(1) = CUTTER(1)
          tool(2) = CUTTER(2)
  140     inc    = inc    + 1
          if (inc .gt. MXCL) go to 180
c
c......CUTCOM/,TOOL
c
          if (IPSTWD(inc) .eq. 617) then
              if (ifl(1) .ne. 0) go to 9600
              ifl(1) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              tool(1) = PSTWD(inc)
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              tool(2) = PSTWD(inc)
c
c......CUTCOM/,MAXIPM
c
          else if (IPSTWD(inc) .eq. 96) then
              if (ifl(2) .ne. 0) go to 9600
              ifl(2) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              fipm   = PSTWD(inc)
c
c......CUTCOM/,ROTREF
c
          else if (IPSTWD(inc) .eq. 68) then
              if (ifl(3) .ne. 0) go to 9600
              ifl(3) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              frot   = PSTWD(inc)
c
c......CUTCOM/,d
c
          else if (IPSTWD(inc) .eq. 0) then
              if (ifl(4) .ne. 0) go to 9600
              ifl(4) = 1
              fxd    = PSTWD(inc)
c
c......CUTCOM/PS,DS
c
          else if (IPSTWD(inc) .eq. 728 .or. IPSTWD(inc) .eq. 729) then
              if (ifl(9) .ne. 0) go to 9600
              ifl(9) = IPSTWD(inc) - 727
c
c......CUTCOM/,XAXIS,x
c
          else if (IPSTWD(inc) .eq. 84) then
              if (ifl(5) .ne. 0) go to 9600
              ifl(5) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rax(1) = PSTWD(inc) * METCNV
c
c......CUTCOM/,YAXIS,z
c
          else if (IPSTWD(inc) .eq. 85) then
              if (ifl(6) .ne. 0) go to 9600
              ifl(6) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rax(2) = PSTWD(inc) * METCNV
c
c......CUTCOM/,ZAXIS,z
c
          else if (IPSTWD(inc) .eq. 86) then
              if (ifl(7) .ne. 0) go to 9600
              ifl(7) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rax(3) = PSTWD(inc) * METCNV
c
c......CUTCOM/,MANUAL
c
          else if (IPSTWD(inc) .eq. 158) then
              if (ifl(8) .ne. 0) go to 9600
              ifl(8) = 1
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          go to 140
c
c......Set up cutcom parameters
c
  180     if (ifl(9) .eq. 0) ifl(9) = 1
          ICUTDO(1) = 1
          ICUTDO(2) = 1
          ICUTDO(3) = 3
          ICUTDO(4) = ifl(4)
          ICUTDO(5) = ifl(2)
          ICUTDO(6) = ifl(3)
          ICUTDO(7) = ifl(5)
          ICUTDO(8) = ifl(6)
          ICUTDO(9) = ifl(7)
          ICUTDO(10) = ifl(8)
          ICUTDO(11) = ifl(9)
c
          CTOFRG = fxd    + CUTCVR(3)
          RCUTDO(2) = rax(1)
          RCUTDO(3) = rax(2)
          RCUTDO(4) = rax(3)
          RCUTDO(5) = tool(1)
          RCUTDO(6) = tool(2)
          RCUTDO(7) = fipm
          RCUTDO(8) = frot
          call cuton
c
c......Output Simulation record
c
          call simsta (0,lmsg,ierr)
c
c...CUTCOM/ADJUST
c
      else if (IPSTWD(1) .eq. 159) then
          if (MXCL .lt. 2) go to 9000
          inc    = inc    + 1
c
c......CUTCOM/ADJUST,ON
c
          if (IPSTWD(inc) .eq. 71) then
              call codout (CUTCCD(17),CUTCVL(17))
              if (CUTCFL(12) .eq. 1) call clrbuf
              call simofs (1,0,-1,-1,lmsg,ierr)
c
c......CUTCOM/ADJUST,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              call codout (CUTCCD(18),CUTCVL(18))
              if (CUTCFL(13) .eq. 1) call clrbuf
              call simofs (0,0,-1,-1,lmsg,ierr)
c
c......CUTCOM/ADJUST,n
c
          else
              if (IPSTWD(inc) .ne. 0) go to 9300
              ireg   = PSTWD(inc)
              fxd    = PSTWD(inc) + CUTCVR(4)
              inc    = inc    + 1
              ifl(1) = 0
              ifl(2) = 0
              ifl(3) = 0
              ifl(4) = 0
              if (inc .gt. MXCL) go to 7000
c
c.........Get remaining parameters
c
  200         if (inc .gt. MXCL) go to 7000
              if (IPSTWD(inc) .eq. 0) go to 9500
c
c............CUTCOM/ADJUST,PLUS
c
              if (IPSTWD(inc) .eq. 19) then
                  if (ifl(1) .ne. 0) go to 9600
                  ifl(1) = 1
                  inc    = inc    + 1
c
c............CUTCOM/ADJUST,MINUS
c
              else if (IPSTWD(inc) .eq. 10) then
                  if (ifl(1) .ne. 0) go to 9600
                  ifl(1) = 2
                  inc    = inc    + 1
c
c............CUTCOM/ADJUST,XAXIS
c
              else if (IPSTWD(inc) .eq. 84) then
                  if (ifl(2) .ne. 0) go to 9600
                  ifl(2) = 1
                  inc    = inc    + 1
                  rax(1) = 0
                  if (IPSTWD(inc) .eq. 0) then
                      rax(1) = PSTWD(inc) * METCNV
                      inc    = inc    + 1
                  endif
c
c............CUTCOM/ADJUST,YAXIS
c
              else if (IPSTWD(inc) .eq. 85) then
                  if (ifl(3) .ne. 0) go to 9600
                  ifl(3) = 1
                  inc    = inc    + 1
                  rax(2) = 0
                  if (IPSTWD(inc) .eq. 0) then
                      rax(2) = PSTWD(inc) * METCNV
                      inc    = inc    + 1
                  endif
c
c............CUTCOM/ADJUST,ZAXIS
c
              else if (IPSTWD(inc) .eq. 86) then
                  if (ifl(4) .ne. 0) go to 9600
                  ifl(4) = 1
                  inc    = inc    + 1
                  rax(3) = 0
                  if (IPSTWD(inc) .eq. 0) then
                      rax(3) = PSTWD(inc) * METCNV
                      inc    = inc    + 1
                  endif
c
c.........Invalid minor word
c
              else
                  if (IPSTWD(inc) .eq. 0) go to 9500
                  go to 9100
              endif
              go to 200
c
c.........Output fixture offset block
c
 7000         call codout (CUTCCD(17),CUTCVL(17))
              if (ifl(1) .eq. 1) call codout (CUTCCD(19),CUTCVL(19))
              if (ifl(1) .eq. 2 .and. CUTCFL(11) .ne. 1)
     1                call codout (CUTCCD(20),CUTCVL(20))
              if (ifl(2) .eq. 1) call codout (CUTCCD(21),rax(1))
              if (ifl(3) .eq. 1) call codout (CUTCCD(22),rax(2))
              if (ifl(4) .eq. 1) call codout (CUTCCD(23),rax(3))
              if (ifl(1) .eq. 2 .and. CUTCFL(11) .eq. 1) fxd = 0 - fxd
              call codout (CUTCCD(16),fxd)
              if (ifl(2) .eq. 1 .or. ifl(3) .eq. 1 .or. ifl(4) .eq. 1
     1            .or. CUTCFL(12) .eq. 1) call clrbuf
              call simofs (ifl(1),ireg,-1,-1,lmsg,ierr)
          endif
      endif
      if (MXCL .gt. inc) go to 9700
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
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Conflicting parameters
c
 9600 call psterr (2,'CONFPRM',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
c
c...Not valid for this machine
c
 9800 call psterr (2,'NOTVALID',' ',inc)
      go to 8000
c
c...CUTCOM programmed with CYCLE/ON
c
 9900 call psterr (2,'CYCCUT',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cuton
c
c   FUNCTION:  This routine outputs a cutter compensation on block.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cuton
c
      include 'post.inc'
c
      equivalence (INMDEF,KPOSMP(3323)), (CUTCFL,KPOSMP(3251))
      equivalence (CUTCCD,KPOSMP(3271)), (ICUTDO,KPOSMP(3301))
      equivalence (PLNCOD,KPOSMP(4222))
c
      integer*4 CUTCCD(30),ICUTDO(15),PLNCOD(4),CUTCFL(20),INMDEF
c
      equivalence (PLNCDV,POSMAP(2208)), (RCUTDO,POSMAP(2229))
      equivalence (CTOFRG,POSMAP(2401)), (CUTCVL,POSMAP(2410))
      equivalence (LSTTN ,POSMAP(3990)), (TLOFRG,POSMAP(3991))
c
      real*8 CTOFRG,TLOFRG,CUTCVL(30),RCUTDO(10),LSTTN,PLNCDV(3)
c
      integer*4 inc
c
c...CUTCOM/ENDPT
c
      if (ICUTDO(3) .eq. 3) then
          INMDEF = 0
c
c......Output cutcom on code
c
          call codout (CUTCCD(24),CUTCVL(24))
c
c......Output Tool codes
c
          call codout (CUTCCD(25),RCUTDO(5)/2.d0)
          call codout (CUTCCD(26),RCUTDO(6))
c
c......Output MAXIPM code
c
          if (ICUTDO(5) .eq. 1) call codout (CUTCCD(28),RCUTDO(7))
c
c......Output ROTREF code
c
          if (ICUTDO(6) .eq. 1) call codout (CUTCCD(27),RCUTDO(8))
c
c......Output cutcom register
c
          if (ICUTDO(4) .eq. 1)
     1        call tlncod (LSTTN,LSTTN,TLOFRG,CTOFRG,4)
c
c...Normal Cutcom
c
      else
c
c......Output plane selection code
c
          call codout (PLNCOD(ICUTDO(4)),PLNCDV(ICUTDO(4)))
c
c......Output cutcom on codes
c
          call codout (CUTCCD(ICUTDO(3)),CUTCVL(ICUTDO(3)))
c
          if (ICUTDO(6) .ne. 0 .and. CUTCFL(3) .eq. 1) then
              inc    = ICUTDO(6) + 11
              call codout (CUTCCD(inc),CUTCVL(inc))
              call codout (CUTCCD(15),RCUTDO(1))
          endif
c
c......Output cutcom register
c
          call tlncod (LSTTN,LSTTN,TLOFRG,CTOFRG,4)
      endif
c
c...Force out cutcom codes
c
      if (CUTCFL(4) .eq. 1) call clrbuf
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutoff
c
c   FUNCTION:  This routine outputs a cutter compensation off block.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cutoff
c
      include 'post.inc'
c
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
c
      integer*4 CUTCCD(30),CUTCFL(20)
c
      equivalence (CTOFRG,POSMAP(2401)), (CUTCVR,POSMAP(2402))
      equivalence (CUTCVL,POSMAP(2410))
      equivalence (LSTTN ,POSMAP(3990)), (TLOFRG,POSMAP(3991))
c
      real*8 CTOFRG,TLOFRG,CUTCVL(30),LSTTN,CUTCVR(8)
c
      integer*4 ierr
c
      character*80 lmsg
c
c...Output CUTCOM/OFF codes
c
      CUTCVR(5) = CTOFRG
      call codout (CUTCCD(3),CUTCVL(3))
      if (CUTCFL(9) .eq. 1) then
          CTOFRG = CUTCVR(3)
          call tlncod (LSTTN,LSTTN,TLOFRG,CTOFRG,4)
      endif
      if (CUTCFL(5) .eq. 1) call clrbuf
c
c......Output Simulation record
c
      call simsta (0,lmsg,ierr)
c
c...End of routine
c
 8000 return
      end
