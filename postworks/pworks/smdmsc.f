c
c***********************************************************************
c
c   FILE NAME:  smdmsc
c   CONTAINS:
c               smdset  smdnxt  smdfrc  smdufc
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        smdmsc.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:12
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  smdset (khed,khom)
c
c   FUNCTION:  This routine initializes the positioning variables for
c              the selected head(s) on a Stringer Mill/Drill style
c              machine.
c
c   INPUT:  khed    I*4  D2  -  Active head(s) for Stringer machine.
c
c           khom    I*4  D1  -  0 = Activate specified heads.  1 = Move
c                               specified HEADS to home position.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine smdset (khed,khom)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (SIMACT,KPOSMP(0174))
      equivalence (SGMACT,KPOSMP(0996)), (SGMHFL,KPOSMP(0998))
      equivalence (SGMBLK,KPOSMP(1008)), (SGMCOD,KPOSMP(1018))
      equivalence (RAPCD ,KPOSMP(3190)), (IRAP  ,KPOSMP(3199))
c
      integer*4 SGMACT(2),SGMHFL(10),IPSTWD(50),SGMBLK(10),SGMCOD(10),
     1          RAPCD(5),IRAP,ITYPE,ISUBT,MXCL,SIMACT
c
      equivalence (PSTWD ,POSMAP(0441)), (SGMSTO,POSMAP(5101))
      equivalence (SGMHOM,POSMAP(5119)), (SGMOUT,POSMAP(5454))
      equivalence (SGMCDV,POSMAP(5552)), (SGMQVL,POSMAP(5562))
c
      real*8 SGMOUT(6,3),PSTWD(50),SGMCDV(10),SGMQVL,SGMHOM(6,3),
     1       SGMSTO(6,3)
c
      integer*4 khed(2),khom
c
      integer*4 isav(2),inc,ifrc,msav,i
c
c...Clear any held back motion
c
      call clrmot (0)
c
c...HEAD/1,HOME
c
      if (khom .eq. 1) then
          if (khed(1) .eq. 1) then
              SGMHFL(1) = 0
              SGMHFL(2) = 0
              if (SIMACT .eq. 1) then
                  msav   = MXCL
                  MXCL   = 0
                  call gohome
                  MXCL   = msav
              else
                  call frcblk (SGMBLK(5),ifrc,1)
              endif
              SGMHFL(8) = 1
c
c...HEAD/2,HOME
c
          else if (khed(1) .eq. 2) then
              SGMHFL(1) = 0
              SGMHFL(2) = 0
c
c......Output simulation block
c
              if (SIMACT .eq. 1) then
                  do 100 i=1,3,1
                     SGMOUT(i,1) = SGMHOM(i,1)
                     SGMOUT(i+3,1) = SGMHOM(i+3,1)
                     SGMOUT(i,2) = SGMSTO(i,2)
                     SGMOUT(i,3) = SGMSTO(i,3)
  100             continue
                  isav(1) = SGMACT(1)
                  isav(2) = SGMACT(2)
                  SGMACT(1) = khed(1)
                  SGMACT(2) = khed(2)
                  IRAP   = 1
                  call smdmot
                  SGMACT(1) = isav(1)
                  SGMACT(2) = isav(2)
                  IRAP   = 0
c
c......Output standard block
c
              else
                  if (SGMHFL(7) .eq. 1) then
                      call frcblk (SGMBLK(2),ifrc,1)
                      SGMHFL(7) = 0
                  endif
                  call codout (SGMCOD(6),SGMCDV(6))
                  call clrbuf
              endif
              SGMHFL(8) = 1
c M38
c
c...HEAD/3,4,HOME
c
          else if (khed(1) .eq. 3 .and. khed(2) .eq. 4) then
              do 200 i=1,3,1
                 SGMOUT(i,1) = SGMSTO(i,1)
                 SGMOUT(i+3,1) = SGMSTO(i+3,1)
                 SGMOUT(i,2) = SGMHOM(i,2)
                 SGMOUT(i,3) = SGMHOM(i,3)
  200         continue
              isav(1) = SGMACT(1)
              isav(2) = SGMACT(2)
              SGMACT(1) = khed(1)
              SGMACT(2) = khed(2)
              if (SGMHFL(5) .eq. 1) SGMQVL = SGMCDV(1)
              IRAP   = 1
              call smdfrc (2,0)
              call smdfrc (3,1)
              call smdfrc (4,1)
              call smdmot
              SGMACT(1) = isav(1)
              SGMACT(2) = isav(2)
              IRAP   = 0
c
c......Determine if next commmand is HEAD/2,HOME
c
              call clsamp (0)
              if (ITYPE .eq. 2000 .and. ISUBT .eq. 1002 .and.
     1            MXCL .eq. 2 .and. PSTWD(1) .eq. 2. .and.
     2            IPSTWD(2) .eq. 1308) SGMHFL(7) = 1
              call clsamp (-1)
          endif
c
c...Set up for current head
c
      else
          inc    = 1
c
c......HEAD/1
c
          if (khed(1) .eq. 1) then
              if (SGMHFL(2) .ne. 1 .and. SGMHFL(1) .eq. 0) then
                  call frcblk (SGMBLK(1),ifrc,1)
                  SGMHFL(1) = 1
                  SGMHFL(2) = 1
               endif
               SGMHFL(6) = 0
               SGMHFL(8) = 1
c
c......HEAD/2
c
          else if (khed(1) .eq. 2) then
              if (SGMHFL(2) .lt. 2) then
                  call frcblk (SGMBLK(2),ifrc,1)
                  call smdfrc (2,1)
              endif
              SGMHFL(2) = 2
              SGMHFL(1) = 0
              SGMHFL(3) = 0
              SGMHFL(4) = 0
              SGMHFL(5) = 1
              SGMHFL(6) = 1
              call frcg01
c
c......HEAD/3
c
          else if (khed(1) .eq. 3) then
              inc    = 2
              if (SGMHFL(6) .eq. 0) then
                  if (khed(2) .eq. 0) SGMHFL(6) = 1
                  if (SGMHFL(3) .eq. 0) then
                      call frcblk (SGMBLK(3),ifrc,1)
                      call smdfrc (2,0)
                      call smdfrc (3,1)
                      call smdfrc (4,1)
                  endif
              endif
              SGMHFL(3) = 1
              SGMHFL(5) = 0
          endif
c
c......HEAD/4
c
          if (khed(inc) .eq. 4) then
              if (SGMHFL(6) .eq. 0) then
                  SGMHFL(6) = 1
                  if (SGMHFL(4) .eq. 0) then
                      call frcblk (SGMBLK(4),ifrc,1)
                      call smdfrc (2,0)
                      call smdfrc (3,1)
                      call smdfrc (4,1)
                  endif
              endif
              SGMHFL(4) = 1
              SGMHFL(5) = 0
          endif
c
c...Set Active head
c
          SGMACT(1) = khed(1)
          SGMACT(2) = khed(2)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdnxt (gpt,gvec,kinc,kerr)
c
c   FUNCTION:  This routine returns the next motion point, either from
c              the CLPT array, Clfile, or Motion Stack.
c
c   INPUT:  kinc    I*4  D1  -  Pointer within CLPT array of current point.
c
c   OUTPUT: kinc    I*4  D1  -  Pointer within CLPT array of next point
c                               if it was retrieved from CLPT array.
c
c           gpt     R*8  D3  -  Next cl point.
c
c           gvec    R*8  D3  -  Next tool axis vector.
c
c           kerr    I*4  D1  -  Could not find next point.
c
c***********************************************************************
c
      subroutine smdnxt (gpt,gvec,kinc,kerr)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (NPT   ,KPOSMP(0059))
      equivalence (MXPT  ,KPOSMP(0060)), (AXSSPT,KPOSMP(1401))
c
      integer*4 AXSSPT,MXPT,NPT,ITYPE
c
      equivalence (CLPT  ,POSMAP(0491)), (TLVEC ,POSMAP(1369))
c
      real*8 CLPT(240),TLVEC(3)
c
      integer*4 kinc,kerr
c
      real*8 gpt(3),gvec(3)
c
      integer*4 inc,isav,iary(10),icnt,isw
c
      real*8 rmch(3,4),rlin(6),rrot(20,2),raxs(10)
c
c...Next point is on the Motion Stack
c
      if (AXSSPT .ne. 0) then
          isav   = AXSSPT
          call popaxs (rmch,rlin,rrot,raxs,gvec,iary,icnt)
          call copyn (rmch(1,2),gpt,3)
          AXSSPT = isav
c
c...Next point is from CLPT array
c
      else
          if (kinc .lt. MXPT) then
              inc    = kinc * NPT + 1
c
c...Next point is from clfile
c
          else
              isw    = 0
  500         call clsamp (isw)
              isw    = 1
              if (ITYPE .eq. 14000) go to 9000
              if (ITYPE .ne. 5000) go to 500
              inc    = 1
              call clsamp (-2)
          endif
          call copyn (CLPT(inc),gpt,3)
          if (NPT .gt. 3) then
              call copyn (CLPT(inc+3),gvec,3)
          else
              call copyn (TLVEC,gvec,3)
          endif
      endif
      kerr   = 0
c
c...End of routine
c
 8000 return
c
c...Could not find next point
c
 9000 kerr   = 1
      call clsamp (-1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smdfrc (khed,kon)
c
c   FUNCTION:  This routine forces the output of the axes associated
c              with the specified head in the next motion block.
c
c   INPUT:  khed    I*4  D1  -  Active head to force axes for.
c
c           kon     I*4  D1  -  1 = Force axes on next motion block,
c                               0 = Don't force axes.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine smdfrc (khed,kon)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (REGFRC,KPOSMP(0603))
      equivalence (SGMREG,KPOSMP(0951))
c
      integer*4 REGFRC(MAXFMT),MOTREG(24),SGMREG(6,3)
c
      integer*4 khed,kon
c
      integer*4 i,ipt(5),inc,ion
c
      data ipt /1,5,9,14,17/
c
c...Force linear code
c
      ion    = kon
      if (kon .eq. 1) then
          ion    = 3
          call frcg01
      endif
c
c...Head 1 is active
c
      if (khed .eq. 1) then
          do 50 i=1,5,1
              REGFRC(MOTREG(ipt(i))) = ion
   50     continue
c
c....Head 2,3, or 4 is active
c
      else
          inc    = khed   - 1
          do 100 i=1,6,1
              if (i .lt. 4 .or. inc .eq. 1) then
                  REGFRC(SGMREG(i,inc)) = ion
              endif
  100     continue
      endif
c
c...End of routine
c
 8000 return
      end
