C*********************************************************************
C*    NAME:  ssdist.f
C*       CONTAINS:  ssdist 
C*                  makfed
C*                  fedmut   
C*                  setfed
C*                  resfed
C*    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
C*
C*    MODULE NAME AND RELEASE LEVEL
C*       ssdist.f , 25.2
C*    DATE AND TIME OF LAST MODIFICATION
C*       11/22/17 , 11:15:21
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ssdist (itask) 
c*    FUNCTION :  Finds a point at specified distance from/to check 
c*                surface and outputs feed rate slowdown if applicable
c*                and goto points. 
C*    PARAMETERS   
C*       INPUT  : itask - I*2 flag 2 = FROM, 3 = GOTO etc.
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ssdist (itask)

      include 'com8a.com'
      include 'comgt.com'

      integer*2 itask
c
      logical lf1f2
      integer*2 modac,modsl,svac
      integer*4 nu,nrp,i,nrf

      real*8 gtp(6),gfed(6),gfac(6),FEEDR
      real*8 mdist,dis,s1,s2
c
      equivalence (FEEDR,MOTMAP(24))
c
c...Initialize
c
      modsl  = 0
      modac  = 0
      svac   = ifl(315)
      if (rpfron .or. ifl(323) .eq. 1) ifl(315) = 0
      if (ifl(314) .ne. 0) modsl = 1
      if (ifl(315) .ne. 0) modac = 1
      if (ifl(95) .eq. 1 .or. ifl(42) .ne. 0) modsl = 2
      nu     = 3 * (ifl(82) + 1)
      do 55 i=1,nu
         gtp(i) = sc(i)
   55 continue
      if (itask .eq. 2) go to 1000
c     
c...Check point distance 
c
      dis    = mdist (FROMSV,gtp,FHIGT,nu) 
      if (dis .lt. .001 .or. modsl+modac .eq. 0) go to 1000
      lf1f2  = FEEDC(2) .lt. FEEDC(1) 
      nrf    = 0
      nrp    = 0
      nor    = 3
      s2     = 0.0
      s1     = 1.0
      is     = 0
c
c...For slowdown OUT find point forward
c
      if (modac .ne. 0) then
         is   = 2
         if (FEDIS(2) .lt. dis-.001) then
            nrf = 1 
            s2  = FEDIS(2) / dis               
            if (FEDIS(2) .lt. .001) then
               nrf = 0
            else
               call makfed (FROMSV,gtp,s2,nu,gfac)
            end if
         else
            nrf = 2
         end if
      end if
c
c...For slwdown AT find point from CS
c
      if (modsl .ne. 0) then
         if (FEDIS(1) .le. dis) then
            nrp = 1 
            s1  = 1.0 - FEDIS(1) / dis               
            if (dis-FEDIS(1) .lt. .001) then
               nrp = -1
               is  = 1
            else if (FEDIS(1) .lt. .001) then
               nrp = 0
            else
               call makfed (FROMSV,gtp,s1,nu,gfed)
            end if       
         else
            nrp = -1
            is  = 1
         end if       
      end if
c
c...Set all flags according to conditions
c
      if (modsl+modac .gt. 0) then
         if (nrp .lt. nrf .and. modac .eq. 1) then
            if (is .eq. 1 .and. lf1f2) is = 2
            if (nrp .gt. 0 .and. lf1f2) nrp = 0
            if (nrp .ne. 0) nor = 1
         else if (nrp .eq. nrf) then
            if (s1 .lt. s2+.002) then
               nor = 1
               if (lf1f2) then
                  nrp = 0
               else if (FEEDC(2) .gt. FEEDC(1)) then
                  nrf = 0
               else
                  nrp = 0
                  nrf = 0
               end if
            end if
         end if
      end if
c
c...Output accelerate feed or slow down
c...if slow has to start immediately  
c
      if (is .ne. 0) call fedmut (FEEDC(is))
c
c...FR/OUT...
c...Output intermediate point & feed rate 
c
      if (nrf .eq. 1 .and. FEEDR .ne. FEEDC(nor)) then
         call putcl (5000,5,1,gfac)
         call fedmut (FEEDC(nor))
      end if           
c
c...FR/AT...
c
      if (nrp .eq. 1 .and. FEEDR .ne. FEEDC(1)) then
         call putcl (5000,5,1,gfed)
         call fedmut (FEEDC(1))
      end if
c
c...Output last point & restore feed
c
 1000 if (itask .eq. 2) then
          call putcl (5000,3,1,gtp)     
          go to 7000
      else 
          call putcl (5000,5,1,gtp)   
c         Modification to block output of FEDRAT/ 0 as delayed value
c         after executing the POCKET/ command
c         Sasha, Aug. 04, 2017
          if (FEEDC(3) .ne. 0.0) call fedmut (FEEDC(3))
      end if
c
c...If ONCE turn off appropriate flag 
c...unless it is FROM statement
c
      if (ifl(314) .eq. 2) call resfed(1) 
      if (ifl(315) .eq. 2) call resfed(2)
 7000 do 7005 i=1,nu
         FROMSV(i) = sc(i)
 7005 continue
      if (rpfron .or. ifl(323) .eq. 1) ifl(315) = svac 
      if (itask .ne. 2) ifl(323) = 0
c
c...End of routine
c
 8000 return 
      end
c
C*********************************************************************
C*    E_SUBROUTINE: makfed (gpt1,gpt2,grat,knu,gfed) 
c*    FUNCTION:  Generates a point/ptvec at specified distance from the 
c*                first point. 
C*    PARAMETERS   
C*       INPUT:  gtp1(21) - First point|pv|superpoint (from). 
C*               gtp2(21) - Second point|pv|superpoint. 
C*               grat     - Ratio defining position of the intermediate
C*                           point between points 1 and 2.
C*               knu      - flag - 3 = points only,
C*                          6 = points with tool axis vector (MU/ON).
C*                          21 = superpoints (expcl record).
C*       OUTPUT: gfed(21) - Intermediate point|pv|superpoint at 
C*                          specified distance 
C*          
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine makfed (gpt1,gpt2,grat,knu,gfed)
c
      include 'com.com'
c
      real*8 gpt1(21),gpt2(21),grat,gfed(21)
      integer*4 knu
c
c...interpolate point
c
      call intrpt (gpt1,gpt2,gfed,grat)
c
c...interpolate TA vector
c      
      if (knu .gt. 3) then
          call intrvec (gpt1(4),gpt2(4),gfed(4),grat)
c
c...interpolate FWD vector & other stuff
c      
          if (knu .gt. 6) then
             call intrvec (gpt1(7),gpt2(7),gfed(7),grat)
             call intrpt (gpt1(10),gpt2(10),gfed(10),grat)
             call intrvec (gpt1(13),gpt2(13),gfed(13),grat)
             call intrpt (gpt1(16),gpt2(16),gfed(16),grat)
             call intrvec (gpt1(19),gpt2(19),gfed(19),grat)
          end if
      end if
c
      return
      end
c
c*********************************************************************
C*    E_SUBROUTINE     : subroutine fedmut (gfrd) 
c*    FUNCTION :  Puts out feed rate record to clfile saving the feed
C*                rate value in common variable.  Void if last feed
C*                rate value is same.   
C*    PARAMETERS   
C*       INPUT  : gfrd - feed rate value to output.
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine fedmut (gfrd)
c
      include 'com8a.com'
      include 'comgt.com'
c
      real*8 gfrd
c
      real*8 FEEDR
      equivalence (FEEDR,MOTMAP(24))
c
      if (gfrd .ne. FEEDR) call putcl (2000,1009,2,gfrd)
      FEEDR  = gfrd
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setfed (gfrd) 
c*    FUNCTION :  Sets feed rates values (FEEDC(1-3)) for FR/AT,OUT  
c*                command according to last specified primary feed 
c*                rate.  Should be used whenever feed rate is changed. 
C*    PARAMETERS   
C*       INPUT  : gfrd - primary feed rate.
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine setfed (gfrd)
c
      include 'com8a.com'
      include 'comgt.com'
c
      real*8 gfrd
c
      FEEDC(3) = gfrd
      if (sclat) FEEDC(1) = FEEDC(4) * gfrd
      if (sclout) FEEDC(2) = FEEDC(5) * gfrd
c
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine resfed (numf) 
c*    FUNCTION :  Restores feed rates values (FEEDC(1-5), FEDIS(1-2) etc)
C*                after FR/...ONCE so the previously used feed rate will 
c*                be in effect.
C*    PARAMETERS   
C*       INPUT  : numf - 1 = reset FR/AT,  2 = reset FR/OUT.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C********************************************************************/
      subroutine resfed (numf)
c
      include 'com8a.com'
      include 'comgt.com'
c
      integer*2 numf
c
c...Reset vars used in old version FR/AT
c
      if (numf .eq. 1) then
          ifl(213) = ifl(214)
          sc(125) = sc(127)
          sc(126) = sc(128)
          sclat  = sclats
      else
          sclout = sclous
      end if
c
      FEEDC(numf) = FEEDS(numf)
      FEEDC(numf+3) = FEEDS(numf+3)
      FEDIS(numf) = FEDIS(numf+2)
      ifl(313+numf) = ifl(319+numf)
c
      return
      end
