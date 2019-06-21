c
c***********************************************************************
c
c   FILE NAME:  cylman
c   CONTAINS:
c               cylman  cylmdo  cylpek  popcyl  pshcyl
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cylman.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:06
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE: cylman
c
c   FUNCTION:  This is the controlling routine for post generated Lathe
c              cycles.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cylman
c
      include 'post.inc'
c
      equivalence (ICYCDO,KPOSMP(0276)), (ICYLFL,KPOSMP(1901))
c
      integer*4 ICYCDO(15),ICYLFL(20)
c
      equivalence (MCHNUM,POSMAP(1287)), (STONUM,POSMAP(1387))
      equivalence (RCYCDO,POSMAP(2931))
c
      real*8 MCHNUM(3,4),STONUM(3,4),RCYCDO(20)
c
      real*8 rfin(3),rclr(3)
c
c...Assign cycle positions
c...Current & final position
c
      rclr(1) = STONUM(1,2)
      rclr(2) = STONUM(2,2)
      rclr(3) = STONUM(3,2)
c
      rfin(1) = MCHNUM(1,2)
      rfin(2) = MCHNUM(2,2)
      rfin(3) = MCHNUM(3,2)
c
c...Perform cycle
c
      call cylmdo (rfin,rclr)
c
c...Cancel cycle
c
      if (ICYLFL(11) .eq. 1) call cyloff
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cylmdo (gfin,gpos,gclr)
c
c   FUNCTION:  This routine performs all post-generated Lathe cycles.
c
c   INPUT:  gfin    R*8  D3  -  The linear axes position at the end of
c                               the cycle.
c
c           gpos    R*8  D3  -  The rapto linear axes position.
c
c           gclr    R*8  D3  -  The current linear axes position.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cylmdo (gfin,gclr)
c
      include 'post.inc'
c
      equivalence (ICYCDO,KPOSMP(0276))
c
      integer*4 ICYCDO(15)
c
      equivalence (RAD   ,POSMAP(0002)), (RCYCDO,POSMAP(2931))
c
      real*8 RCYCDO(20),RAD
c
      real*8 gfin(3),gclr(3)
c
      integer*4 ipt,istk(5),ifl
c
      real*8 rmch(3),rpck,rdep,rdep1,fdep,rprm(6),rcnt,rpos(3),
     1       rdis,tvec(3),rvec(2),rnum,angl,bangl,rstrt(3)
c
c...Initialize routine
c
      ipt    = 0
c
c...CYCLE/DEEP
c...      DRILL
c...      THRU
c
      if (ICYCDO(1) .eq. 1 .or. ICYCDO(1) .eq. 2 .or.
     1    ICYCDO(1) .eq. 3 .or. ICYCDO(1) .eq. 4 .or.
     2    ICYCDO(1) .eq. 11 .or. ICYCDO(1) .eq. 12) then
c
c......Calculate rapto position
c
          tvec(1) = gfin(1) - gclr(1)
          tvec(2) = gfin(3) - gclr(3)
          rdis   = dsqrt(tvec(1)**2 + tvec(2)**2)
          tvec(1) = tvec(1) / rdis
          tvec(2) = tvec(2) / rdis
          rpos(1) = gclr(1) + RCYCDO(8) * tvec(1)
          rpos(2) = gclr(2)
          rpos(3) = gclr(3) + RCYCDO(8) * tvec(2)
c
c......Calculate final depth
c
          rdep   = dsqrt((gfin(1)-rpos(1))**2 + (gfin(3)-rpos(3))**2)
          rpck   = 0.
c
c......Position to Rapto plane
c
         if (ICYCDO(7) .eq. 1) call pshcyl (istk,ipt,1,rpos)
c
c......Calculate current depth
c
  100    call cylpek (rdep,rpck,rprm,ifl)
c
c......Plunge tool
c
          rmch(1) = rpos(1) + rprm(5) * tvec(1)
          rmch(2) = rpos(2)
          rmch(3) = rpos(3) + rprm(5) * tvec(2)
          call pshcyl (istk,ipt,3,rmch)
c
c......Retract tool (relief)
c
          if (ICYCDO(10) .ne. 0 .and. RCYCDO(13) .ne. 0. .and.
     1        ifl .eq. 0) then
              rmch(1) = rmch(1) - RCYCDO(13) * tvec(1)
              rmch(2) = rmch(2)
              rmch(3) = rmch(3) - RCYCDO(13) * tvec(2)
              call pshcyl (istk,ipt,2,rmch)
          endif
          if (ifl .eq. 0) go to 100
c
c......Position to original point
c
          call pshcyl (istk,ipt,2,gclr)
c
c...CYCLE/FACE
c
      else if (ICYCDO(1) .eq. 5 .or. ICYCDO(1) .eq. 6) then
c
c......Calculate rapto position
c
          rdis   = gfin(3) - gclr(3)
          tvec(1) = rdis   / dabs(rdis)
          rpos(1) = gclr(1)
          rpos(2) = gclr(2)
          rpos(3) = gclr(3) + RCYCDO(8) * tvec(1)
c
c......Calculate final depths
c
          rdep   = gfin(3) - rpos(3)
          fdep   = dabs(rdep)
          if (ICYCDO(4) .eq. 0) then
              rdep1  = rdep
          else
              rdep1  = rdep   + RCYCDO(3)
              if (dabs(rdep1) .gt. fdep) fdep = dabs(rdep1)
          endif
          rpck   = 0.
c
c......Calculate current depth
c
  200    call cylpek (fdep,rpck,rprm,ifl)
         rcnt   = 0
c
c......Position to Rapto plane
c
  220     if (ICYCDO(7) .eq. 1) call pshcyl (istk,ipt,1,rpos)
c
c......Plunge tool
c
          rmch(1) = rpos(1)
          rmch(2) = rpos(2)
          if (fdep .ne. 0.) then
              rmch(3) = rpos(3) + rprm(5) * (rdep1/fdep)
          else
              rmch(3) = rpos(3)
          endif
          call pshcyl (istk,ipt,3,rmch)
c
c......Feed across part
c
          rmch(1) = gfin(1)
          rmch(2) = rpos(2)
          if (fdep .ne. 0.) then
              rmch(3) = rpos(3) + rprm(5) * (rdep/fdep)
          else
              rmch(3) = rpos(3)
          endif
          call pshcyl (istk,ipt,3,rmch)
c
c......Retract tool
c
          rmch(1) = gfin(1)
          rmch(2) = gclr(2)
          rmch(3) = gclr(3)
          call pshcyl (istk,ipt,2,rmch)
c
c......Position to original point
c
          call pshcyl (istk,ipt,1,gclr)
          if (ifl .eq. 0) go to 200
          rcnt   = rcnt   + 1
          if (ICYCDO(12) .ne. 0 .and. rcnt .lt. RCYCDO(17)) go to 220
c
c...CYCLE/ROUGH
c...      TURN
c
      else if (ICYCDO(1) .eq. 7 .or. ICYCDO(1) .eq. 8 .or.
     1         ICYCDO(1) .eq. 13 .or. ICYCDO(1) .eq. 14) then
c
c......Calculate rapto position
c
          rdis   = gfin(1) - gclr(1)
          tvec(1) = rdis   / dabs(rdis)
          rpos(1) = gclr(1) + RCYCDO(8) * tvec(1)
          rpos(2) = gclr(2)
          rpos(3) = gclr(3)
c
c......Calculate final depths
c
          rdep   = gfin(1) - rpos(1)
          fdep   = dabs(rdep)
          if (ICYCDO(4) .eq. 0) then
              rdep1  = rdep
          else
              rdep1  = rdep   + RCYCDO(3)
              if (dabs(rdep1) .gt. fdep) fdep = dabs(rdep1)
          endif
          rpck   = 0.
c
c......Calculate current depth
c
  300    call cylpek (fdep,rpck,rprm,ifl)
         rcnt   = 0
c
c......Position to Rapto plane
c
  320     if (ICYCDO(7) .eq. 1) call pshcyl (istk,ipt,1,rpos)
c
c......Plunge tool
c
          if (fdep .ne. 0.) then
              rmch(1) = rpos(1) + rprm(5) * (rdep1/fdep)
          else
              rmch(1) = rpos(1)
          endif
          rmch(2) = rpos(2)
          rmch(3) = rpos(3)
          call pshcyl (istk,ipt,3,rmch)
c
c......Feed across part
c
          if (fdep .ne. 0.) then
              rmch(1) = rpos(1) + rprm(5) * (rdep/fdep)
          else
              rmch(1) = rpos(1)
          endif
          rmch(2) = rpos(2)
          rmch(3) = gfin(3)
          call pshcyl (istk,ipt,3,rmch)
c
c......Retract tool
c
          rmch(1) = gclr(1)
          rmch(2) = gclr(2)
          rmch(3) = gfin(3)
          call pshcyl (istk,ipt,2,rmch)
c
c......Position to original point
c
          call pshcyl (istk,ipt,1,gclr)
          if (ifl .eq. 0) go to 300
          rcnt   = rcnt   + 1
          if (ICYCDO(12) .ne. 0 .and. rcnt .lt. RCYCDO(17)) go to 320
c
c...CYCLE/THREAD
c
      else if (ICYCDO(1) .eq. 9 .or. ICYCDO(1) .eq. 10) then
c
c......Calculate final depth
c
          rdep   = gfin(1) - gclr(1)
          fdep   = dabs(rdep)
          tvec(1) = rdep   / fdep
          if (ICYCDO(9) .ne. 0) then
              rdep   = dabs(RCYCDO(11)) * tvec(1)
              fdep   = dabs(rdep)
          endif
          rdep1  = gfin(3) - gclr(3)
          tvec(2) = rdep1  / dabs(rdep1)
c
c......Calculate top of part
c
          rpos(2) = gfin(1) - rdep
          if (ICYCDO(4) .eq. 0) then
              rpos(1) = rpos(2)
          else
              rpos(1) = rpos(2) + RCYCDO(3)
          endif
c
c......Calculate current depth
c
          rpck   = 0.
  400     call cylpek (fdep,rpck,rprm,ifl)
          rcnt   = 0
c
c......Position along tool angle
c
  420     rmch(2) = gclr(2)
          rmch(3) = gclr(3)
          if (ICYCDO(11) .ne. 0 .and. RCYCDO(15) .ne. 0. .and.
     1        fdep .ne. 0.) then
              rmch(1) = gclr(1) + rprm(5) * tvec(1)
              rmch(2) = gclr(2)
              rmch(3) = gclr(3) + (dtan(RCYCDO(15)/RAD/2.) *
     1                            (rprm(5) * tvec(2)))
              call pshcyl (istk,ipt,2,rmch)
          endif
c
c...Position to final depth
c
          rmch(1) = rpos(1) + rprm(5) * tvec(1)
          rdep1   = rmch(1)
          rmch(2) = rmch(2)
          rmch(3) = rmch(3)
          call pshcyl (istk,ipt,2,rmch)
          rstrt(1) = rmch(1)
          rstrt(2) = rmch(2)
          rstrt(3) = rmch(3)
c
c......Feed across part
c
          rmch(1) = rpos(2) + rprm(5) * tvec(1)
c
c.........No chamfer
c
          if (ICYCDO(13) .ne. 1 .or. fdep .eq. 0.) then
              rmch(2) = gfin(2)
              rmch(3) = gfin(3)
              call pshcyl (istk,ipt,4,rmch)
c
c.........Chamfer
c
          else
              rvec(1) = rmch(1) - rstrt(1)
              rvec(2) = gfin(3) - rstrt(3)
              rdis   = dsqrt(rvec(1)**2 + rvec(2)**2)
              rvec(1) = rvec(1) / rdis
              rvec(2) = rvec(2) / rdis
c
c............Not enough room for chamfer
c............Feed across part
c
              if (dabs(rvec(1)) .gt. dabs(rvec(2))) then
                  rmch(2) = gfin(2)
                  rmch(3) = gfin(3)
                  call pshcyl (istk,ipt,4,rmch)
c
c............Cut to beginning of chamfer
c
              else
                  bangl  = dacos(rvec(1))
                  angl   = 45./RAD - (90./RAD - bangl)
                  rnum   = rprm(5) * dsin(bangl)
                  rnum   = rnum / dsin(angl)
                  rmch(1) = rpos(2) + rnum * dcos(45.d0/RAD) * tvec(1)
                  rmch(2) = gfin(2)
                  rmch(3) = gfin(3) - rnum * dcos(45.d0/RAD) * tvec(2)
                  call pshcyl (istk,ipt,4,rmch)
c
c............Cut chamfer
c
                  rmch(1) = rpos(2)
                  rmch(2) = gfin(2)
                  rmch(3) = gfin(3)
                  call pshcyl (istk,ipt,5,rmch)
              endif
          endif
c
c......Retract tool
c
          rmch(1) = gclr(1)
          rmch(2) = gclr(2)
          rmch(3) = gfin(3)
          call pshcyl (istk,ipt,2,rmch)
c
c......Position to original point
c
          call pshcyl (istk,ipt,1,gclr)
          if (ifl .eq. 0) go to 400
          rcnt   = rcnt   + 1
          if (ICYCDO(12) .ne. 0 .and. rcnt .lt. RCYCDO(17)) go to 420
      endif
c
c...End of routine
c
 8000 call popcyl (istk,ipt)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cylpek (gdep,gpck,gprm,kfl)
c
c   FUNCTION:  This routine calculates the pecking depths for Lathe
c              cycles.
c
c   INPUT:  gdep    R*8  D1  -  Final depth of cycle.
c
c           gpck    R*8  D1  -  Should be set to 0. on first call and
c                               will be modified by this routine.
c
c   OUTPUT: gpck    R*8  D1  -  Returns the depth of the current cut.
c
c           gprm    R*8  D6  -  Loca array that is used to calculate
c                               each depth of cut.
c
c                                    1 = Initial peck.
c                                    2 = Final peck.
c                                    3 = Difference between (1) & (2).
c                                    4 = Maximum final peck.
c                                    5 = Current depth.
c                                    6 = Final depth.
c
c           kfl     I*4  D1  -  Returns 1 when the final depth has been
c                               reached.
c
c***********************************************************************
c
      subroutine cylpek (gdep,gpck,gprm,kfl)
c
      include 'post.inc'
c
      equivalence (ICYCDO,KPOSMP(0276))
c
      integer*4 ICYCDO(15)
c
      equivalence (RCYCDO,POSMAP(2931))
c
      real*8 RCYCDO(20)
c
      integer*4 kfl
c
      real*8 gdep,gpck,gprm(6)
c
c...First time here
c
      if (gpck .eq. 0.) then
          kfl    = 0
          gprm(5) = 0.
          gprm(6) = dabs(gdep)
c
c......No pecking required
c
          if (ICYCDO(8) .eq. 0 .or. RCYCDO(9) .eq. 0.) go to 7000
c
c......Define pecking depths
c
          gprm(1) = dabs(RCYCDO(9))
          gprm(2) = dabs(RCYCDO(10))
          if (ICYCDO(8) .eq. 1 .or. gprm(2) .eq. 0.) then
              gprm(2) = gprm(1)
              gprm(3) = 0.
              gprm(4) = 0.
          else
              gprm(3) = gprm(1) - gprm(2)
              gprm(4) = gprm(2) * .2
          endif
      endif
c
c...Calculate next pecking distance
c
      gpck   = gprm(1) - (gprm(5) / gprm(6)) * gprm(3)
      if (gprm(5)+gpck+1.d-5 .ge. gprm(6) .or.
     1    gprm(5)+gprm(2)+gprm(4)+1.d-5 .ge. gprm(6)) go to 7000
      gprm(5) = gprm(5) + gpck
      go to 8000
c
c...Final depth reached
c
 7000 gpck   = gprm(6) - gprm(5)
      gprm(5) = gprm(6)
      kfl    = 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: popcyl (kstk,kpt)
c
c   FUNCTION:  This routine clears out the Lathe manual cycle stack.
c
c   INPUT:  kstk    I*4  D5  -  Local cycle stack.
c
c           kpt     I*4  D1  -  Pointer to 'kstk'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine popcyl (kstk,kpt)
c
      integer*4 kstk(5),kpt
c
      real*8 rnum(3)
c
c...Clean up manual cycle stack
c
      call pshcyl (kstk,kpt,-1,rnum)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: pshcyl (kstk,kpt,kfl,gpos)
c
c   FUNCTION:  This routine controls the actual output of all blocks
c              generated by a Lathe post genterated cycle.  It puts
c              blocks on the stack to satisfy lookahead routines and
c              takes them off the stack to output them.
c
c   INPUT:  kstk    I*4  D5  -  Local cycle stack that stores the type
c                               of block to output.  Currenty only 1
c                               block is stored on the stack at a time.
c
c           kpt     I*4  D1  -  Pointer to 'kstk'.  Should be set to 0
c                               for initial call.
c
c           kfl     I*4  D1  -  Type of block to output and can have the
c                               following values:
c
c                                  -1 = An input block is not given.
c                                       Output the next stack block
c                                       only.
c
c                                   1 = Position in rapid mode.
c                                   2 = Position using rapid feedrate.
c                                   3 = Position using cycle feedrate.
c                                   4 = Threadcutting positioning move.
c
c           gpos    R*8  D3  -  Tool end point position to output when
c                               'kfl' is less than 6.
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pshcyl (kstk,kpt,kfl,gpos)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (ICYCSW,KPOSMP(0271))
      equivalence (IFITYP,KPOSMP(3150)), (POSFED,KPOSMP(3209))
c
      integer*4 IFITYP,ITYPE,ISUBT,MXCL,POSFED,ICYCSW(5)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (RCYCDO,POSMAP(2931)), (PFEED ,POSMAP(3540))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),ROTANG(20,2),TLVEC(3),RCYCDO(20),
     1       PFEED(4),AXSOUT(10)
c
      integer*4 kstk(5),kpt,kfl
c
      real*8 gpos(3)
c
      integer*4 ifl(10),icnt,its,ims,iss,ifsv,isv
c
      real*8 rmch(3,4),rlin(6),raxs(10),rfsv
c
      if (kpt .eq. 0 .and. kfl .eq. -1) go to 8000
c
c...Remove previous position
c...from stack
c
      if (kpt .ne. 0) then
          if (kstk(kpt) .le. 5) then
              call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,ifl,icnt)
          else
              its    = ITYPE
              iss    = ISUBT
              ims    = MXCL
              call popcmd
              ITYPE  = its
              ISUBT  = iss
              MXCL   = ims
          endif
      endif
c
c...Push current position
c...onto lookahead stack
c
      if (kfl .eq. -1) go to 500
      if (kfl .le. 5) then
          rmch(1,2) = gpos(1)
          rmch(2,2) = gpos(2)
          rmch(3,2) = gpos(3)
          call alladj (rmch,rlin,raxs,ROTANG,1,5)
          call pshaxs (raxs,TLVEC)
c
c...Push the post command
c...onto lookahead stack
c...(Actually place the STOP command
c... onto the lookahead stack)
c
      else
          its    = ITYPE
          iss    = ISUBT
          ims    = MXCL
          ITYPE  = 2000
          ISUBT  = 2
          MXCL   = 0
          call pshcmd
          ITYPE  = its
          ISUBT  = iss
          MXCL   = ims
      endif
c
c...First time here
c...Don't output previous cycle block
c
      if (kpt .eq. 0) then
          kpt    = 1
          kstk(kpt) = kfl
          go to 8000
      endif
c
c...Output previous cycle block
c......Previous position
c
  500 if (kstk(kpt) .le. 5) then
          if (icnt .ne. 0) then
              ifsv   = IFITYP
              rfsv   = PFEED(1)
c
c........Move in rapid mode
c
              if (kstk(kpt) .eq. 1) then
                  call rapset (5,1)
c
c........Move at cycle rapid rate
c
              else if (kstk(kpt) .eq. 2) then
                  IFITYP = 1
                  PFEED(1) = RCYCDO(5)
              endif
c
c........Perform cycle move
c...........Threadcutting move
c
              if (kstk(kpt) .eq. 4) then
                  isv   = ICYCSW(1)
                  call thrmot (RCYCDO(6),RCYCDO(7),0,0.d0,1)
                  ICYCSW(1) = isv
                  ICYCSW(2) = 3
                  POSFED = 1
c
c...........Feed controlled move
c
              else
                  call motion (ifl,icnt)
                  if (kstk(kpt) .eq. 1) call raprst
              endif
              IFITYP = ifsv
              PFEED(1) = rfsv
          endif
c
c......Post commands go here
c
      endif
c
c...Push current cycle block
c...onto local stack
c
      if (kfl .eq. -1) then
          kpt   = kpt   - 1
      else
          kstk(kpt) = kfl
      endif
c
c...End of routine
c
 8000 return
      end
