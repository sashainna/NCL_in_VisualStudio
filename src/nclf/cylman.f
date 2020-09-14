c
c***********************************************************************
c
c   FILE NAME:  cylman
c   CONTAINS:
c               cylman  cylmdo  cylpek
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cylman.f , 25.1
c     DATE AND TIME OF LAST  MODIFICATION
c        04/29/15 , 15:09:49
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE: cylman (spt,gpt,kcyc,gcyc,kret,gout,knout)
c
c   FUNCTION:  This is the controlling routine for post generated Lathe
c              cycles.
c
c   INPUT:  spt     R*8  D6  -  Point coming from.
c
c           gpt     R*8  D6  -  Point to perform cycle at.
c
c           kcyc    I*2  D10 -  Integer cycle parameters.
c
c           gcyc    R*8  D10 -  Real cycle parameters.
c
c           kret    I*2  D1  -  1 = Retract to rapto plane.  2 = Retract
c                               to clearance plane.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                for cycle motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c***********************************************************************
c
      subroutine cylman (spt,gpt,kcyc,gcyc,kret,gout,knout)
c
      include 'com.com'
c
      integer*2 kcyc(10),kret,knout
c
      real*8 spt(6),gpt(6),gcyc(10),gout(4,500)
c
      real*8 rfin(3),rclr(3)
c
c...Assign cycle positions
c...Current & final position
c
      rclr(1) = spt(1)
      rclr(2) = spt(2)
      rclr(3) = spt(3)
c
      rfin(1) = gpt(1)
      rfin(2) = gpt(2)
      rfin(3) = gpt(3)
c
c...Perform cycle
c
      call cylmdo (rfin,rclr,kcyc,gcyc,gout,knout)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cylmdo (gfin,gclr,kcyc,gcyc,gout,knout)
c
c   FUNCTION:  This routine performs all post-generated Lathe cycles.
c
c   INPUT:  gfin    R*8  D3  -  The linear axes position at the end of
c                               the cycle.
c
c           gclr    R*8  D3  -  The current linear axes position.
c
c           kcyc    I*2  D10 -  Integer cycle parameters.
c
c           gcyc    R*8  D10 -  Real cycle parameters.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                 for cycle motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cylmdo (gfin,gclr,kcyc,gcyc,gout,knout)
c
      include 'com.com'
      include 'const.com'
c
      integer*2 knout,kcyc(10),rcnt
c
      real*8 gfin(3),gclr(3),gout(4,500),gcyc(10)
c
      integer*4 ipt,istk(5),iflg,is1,is2,is3
c
      real*8 rmch(3),rpck,rdep,rdep1,fdep,rprm(6),rpos(3),rstrt(3),
     1       rdis,tvec(3),rvec(2),rnum,angl,bangl
c
c...Initialize routine
c
      ipt    = 0
      knout  = 0
      is1    = 2
      is2    = 3
      is3    = 1
c
c...CYCLE/DEEP
c...      DRILL
c...      THRU
c
      if (kcyc(2) .eq. 1 .or. kcyc(2) .eq. 2 .or.
     1    kcyc(2) .eq. 3) then
c
c......Calculate rapto position
c
          tvec(1) = gfin(is1) - gclr(is1)
          tvec(2) = gfin(is3) - gclr(is3)
          rdis   = dsqrt(tvec(1)**2 + tvec(2)**2)
          tvec(1) = tvec(1) / rdis
          tvec(2) = tvec(2) / rdis
          rpos(is1) = gclr(is1) + gcyc(2) * tvec(1)
          rpos(is2) = gclr(is2)
          rpos(is3) = gclr(is3) + gcyc(2) * tvec(2)
c
c......Calculate final depth
c
          rdep   = dsqrt((gfin(is1)-rpos(is1))**2 +
     1                 (gfin(is3)-rpos(is3))**2)
          rpck   = 0.
c
c......Position to Rapto plane
c
          if (gcyc(2) .ne. 0.) call pshcyc (gout,knout,gcyc(7),rpos)
c
c......Calculate current depth
c
  100     call cylpek (rdep,rpck,rprm,iflg,kcyc,gcyc)
c
c......Plunge tool
c
          rmch(is1) = rpos(is1) + rprm(5) * tvec(1)
          rmch(is2) = rpos(is2)
          rmch(is3) = rpos(is3) + rprm(5) * tvec(2)
          call pshcyc (gout,knout,gcyc(6),rmch)
c
c......Retract tool (relief)
c
          if (gcyc(5) .ne. 0 .and.  iflg .eq. 0) then
              rmch(is1) = rmch(is1) - gcyc(5) * tvec(1)
              rmch(is2) = rmch(is2)
              rmch(is3) = rmch(is3) - gcyc(5) * tvec(2)
              call pshcyc (gout,knout,gcyc(6),rmch)
          endif
          if (iflg .eq. 0) go to 100
c
c......Position to original point
c
          call pshcyc (gout,knout,gcyc(7),gclr)
c
c...CYCLE/FACE
c
      else if (kcyc(2) .eq. 4) then
c
c......Calculate rapto position
c
          rdis   = gfin(is3) - gclr(is3)
          tvec(1) = rdis   / dabs(rdis)
          rpos(is1) = gclr(is1)
          rpos(is2) = gclr(is2)
          rpos(is3) = gclr(is3) + gcyc(2) * tvec(1)
c
c......Calculate final depths
c
          rdep   = gfin(is3) - rpos(is3)
          fdep   = dabs(rdep)
          if (gcyc(1) .eq. 0.) then
              rdep1  = rdep
          else
              rdep1  = rdep   + gcyc(1)
              if (dabs(rdep1) .gt. fdep) fdep = dabs(rdep1)
          endif
          rpck   = 0.
c
c......Calculate current depth
c
  200    call cylpek (fdep,rpck,rprm,iflg,kcyc,gcyc)
         rcnt   = 0
c
c......Position to Rapto plane
c
  220     if (gcyc(2) .ne. 0.) call pshcyc (gout,knout,gcyc(7),rpos)
c
c......Plunge tool
c
          rmch(is1) = rpos(is1)
          rmch(is2) = rpos(is2)
          if (fdep .ne. 0.) then
              rmch(is3) = rpos(is3) + rprm(5) * (rdep1/fdep)
          else
              rmch(is3) = rpos(is3)
          endif
          call pshcyc (gout,knout,gcyc(6),rmch)
c
c......Feed across part
c
          rmch(is1) = gfin(is1)
          rmch(is2) = rpos(is2)
          if (fdep .ne. 0.) then
              rmch(is3) = rpos(is3) + rprm(5) * (rdep/fdep)
          else
              rmch(is3) = rpos(is3)
          endif
          call pshcyc (gout,knout,gcyc(6),rmch)
c
c......Retract tool
c
          rmch(is1) = gfin(is1)
          rmch(is2) = gclr(is2)
          rmch(is3) = gclr(is3)
          call pshcyc (gout,knout,gcyc(7),rmch)
c
c......Position to original point
c
          call pshcyc (gout,knout,gcyc(7),gclr)
          if (iflg .eq. 0) go to 200
          rcnt   = rcnt   + 1
          if (rcnt .lt. kcyc(5)) go to 220
c
c...CYCLE/ROUGH
c...      TURN
c
      else if (kcyc(2) .eq. 5) then
c
c......Calculate rapto position
c
          rdis   = gfin(is1) - gclr(is1)
          tvec(1) = rdis   / dabs(rdis)
          rpos(is1) = gclr(is1) + gcyc(2) * tvec(1)
          rpos(is2) = gclr(is2)
          rpos(is3) = gclr(is3)
c
c......Calculate final depths
c
          rdep   = gfin(is1) - rpos(is1)
          fdep   = dabs(rdep)
          if (gcyc(1) .ne. 0.) then
              rdep1  = rdep
          else
              rdep1  = rdep   + gcyc(1)
              if (dabs(rdep1) .gt. fdep) fdep = dabs(rdep1)
          endif
          rpck   = 0.
c
c......Calculate current depth
c
  300    call cylpek (fdep,rpck,rprm,iflg,kcyc,gcyc)
         rcnt   = 0
c
c......Position to Rapto plane
c
  320     if (gcyc(2) .ne. 0.) call pshcyc (gout,knout,gcyc(7),rpos)
c
c......Plunge tool
c
          if (fdep .ne. 0.) then
              rmch(is1) = rpos(is1) + rprm(5) * (rdep1/fdep)
          else
              rmch(is1) = rpos(is1)
          endif
          rmch(is2) = rpos(is2)
          rmch(is3) = rpos(is3)
          call pshcyc (gout,knout,gcyc(6),rmch)
c
c......Feed across part
c
          if (fdep .ne. 0.) then
              rmch(is1) = rpos(is1) + rprm(5) * (rdep/fdep)
          else
              rmch(is1) = rpos(is1)
          endif
          rmch(is2) = rpos(is2)
          rmch(is3) = gfin(is3)
          call pshcyc (gout,knout,gcyc(6),rmch)
c
c......Retract tool
c
          rmch(is1) = gclr(is1)
          rmch(is2) = gclr(is2)
          rmch(is3) = gfin(is3)
          call pshcyc (gout,knout,gcyc(7),rmch)
c
c......Position to original point
c
          call pshcyc (gout,knout,gcyc(7),gclr)
          if (iflg .eq. 0) go to 300
          rcnt   = rcnt   + 1
          if (rcnt .lt. kcyc(5)) go to 320
c
c...CYCLE/THREAD
c
      else if (kcyc(2) .eq. 6) then
c
c......Calculate final depth
c
          rdep   = gfin(is1) - gclr(is1)
          fdep   = dabs(rdep)
          tvec(1) = rdep   / fdep
          if (gcyc(2) .ne. 0.) then
              rdep   = dabs(gcyc(2)) * tvec(1)
              fdep   = dabs(rdep)
          endif
          rdep1  = gfin(is3) - gclr(is3)
          tvec(2) = rdep1  / dabs(rdep1)
c
c......Calculate top of part
c
          rpos(is2) = gfin(is1) - rdep
          if (gcyc(1) .eq. 0.) then
              rpos(is1) = rpos(is2)
          else
              rpos(is1) = rpos(is2) + gcyc(1)
          endif
c
c......Calculate current depth
c
          rpck   = 0.
  400     call cylpek (fdep,rpck,rprm,iflg,kcyc,gcyc)
          rcnt   = 0
c
c......Position along tool angle
c
  640     rmch(is2) = gclr(is2)
          rmch(is3) = gclr(is3)
          if (gcyc(5) .ne. 0. .and. fdep .ne. 0.) then
              rmch(is1) = gclr(is1) + rprm(5) * tvec(1)
              rmch(is2) = gclr(is2)
              rmch(is3) = gclr(is3) + (dtan(gcyc(5)/RADIAN/2.) *
     1                            (rprm(5) * tvec(2)))
              call pshcyc (gout,knout,gcyc(7),rmch)
          endif
c
c...Position to final depth
c
          rmch(is1) = rpos(is1) + rprm(5) * tvec(1)
          rdep1   = rmch(is1)
          rmch(is2) = rmch(is2)
          rmch(is3) = rmch(is3)
          call pshcyc (gout,knout,gcyc(7),rmch)
          rstrt(1) = rmch(1)
          rstrt(2) = rmch(2)
          rstrt(3) = rmch(3)
c
c......Feed across part
c
          rmch(is1) = rpos(is2) + rprm(5) * tvec(1)
c
c.........No chamfer
c
          if (kcyc(6) .ne. 1 .or. fdep .eq. 0.) then
              rmch(is2) = gfin(is2)
              rmch(is3) = gfin(is3)
              call pshcyc (gout,knout,gcyc(6),rmch)
c
c.........Chamfer
c
          else
              rvec(1) = rmch(is1) - rstrt(is1)
              rvec(2) = gfin(is3) - rstrt(is3)
cc              rvec(1) = rmch(is1) - rdep1
cc              rvec(2) = gfin(is3) - gfin(is3)
              rdis   = dsqrt(rvec(1)**2 + rvec(2)**2)
              rvec(1) = rvec(1) / rdis
              rvec(2) = rvec(2) / rdis
c
c............Not enough room for chamfer
c............Feed across part
c
              if (dabs(rvec(1)) .gt. dabs(rvec(2))) then
                  rmch(is2) = gfin(is2)
                  rmch(is3) = gfin(is3)
                  call pshcyc (gout,knout,gcyc(6),rmch)
c
c............Cut to beginning of chamfer
c
              else
                  bangl  = dacos(rvec(1))
                  angl   = 45./RADIAN - (90./RADIAN - bangl)
                  rnum   = rprm(5) * dsin(bangl)
                  rnum   = rnum / dsin(angl)
                  rmch(is1) = rpos(is2) + rnum * dcos(45.d0/RADIAN) *
     1                tvec(1)
                  rmch(is2) = gfin(is2)
                  rmch(is3) = gfin(is3) - rnum * dcos(45.d0/RADIAN) *
     1                tvec(2)
                  call pshcyc (gout,knout,gcyc(6),rmch)
c
c............Cut chamfer
c
                  rmch(is1) = rpos(is2)
                  rmch(is2) = gfin(is2)
                  rmch(is3) = gfin(is3)
                  call pshcyc (gout,knout,gcyc(6)/10.,rmch)
              endif
          endif
c
c......Retract tool
c
          rmch(is1) = gclr(is1)
          rmch(is2) = gclr(is2)
          rmch(is3) = gfin(is3)
          call pshcyc (gout,knout,gcyc(7),rmch)
c
c......Position to original point
c
          call pshcyc (gout,knout,gcyc(7),gclr)
          if (iflg .eq. 0) go to 400
          rcnt   = rcnt   + 1
          if (rcnt .lt. kcyc(5)) go to 640
c
c...THREAD/lead
c
      else if (kcyc(2) .eq. 7) then
          call pshcyc (gout,knout,gcyc(6),gfin)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cylpek (gdep,gpck,gprm,kfl,kcyc,gcyc)
c
c   FUNCTION:  This routine calculates the pecking depths for Lathe
c              cycles.
c
c   INPUT:  gdep    R*8  D1  -  Final depth of cycle.
c
c           gpck    R*8  D1  -  Should be set to 0. on first call and
c                               will be modified by this routine.
c
c           kcyc    I*2  D10 -  Integer cycle parameters.
c
c           gcyc    R*8  D10 -  Real cycle parameters.
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
      subroutine cylpek (gdep,gpck,gprm,kfl,kcyc,gcyc)
c
      include 'com.com'
c
      integer*4 kfl,kcyc(10)
c
      real*8 gdep,gpck,gprm(6),gcyc(10)
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
          if (gcyc(3) .eq. 0.) go to 7000
c
c......Define pecking depths
c
          gprm(1) = dabs(gcyc(3))
          gprm(2) = dabs(gcyc(4))
          if (gprm(2) .eq. 0.) then
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
