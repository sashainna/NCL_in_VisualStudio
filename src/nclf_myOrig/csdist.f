C*********************************************************************
C*    NAME         :  csdist.f
C*       CONTAINS:  csdist
C*                  fed2pt
C*                  mdist 
C*    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
C*
C*    MODULE NAME AND RELEASE LEVEL 
C*       csdist.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:45
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine csdist (kerr)
c*    FUNCTION :  Finds a point at specified distance from/to check 
c*                surface and outputs feed rate slowdown if applicable
c*                and goto points. 
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine csdist (iclass, isubcl)

      include 'com.com'
      include 'comgt.com'
c
      integer*2 isubcl,iclass

      integer*4 nn,nrp,nop,ntot,mu,i,j,kd,nof,nrf,nor,isf
      integer*4 maxpt

      integer*2 numitm,modsl,modac,svac,lfeed,nu,inc,is,k,isub,isb
      !real*8 gtp(4200),gfed(21),gfac(21),tout(640),FEEDR
      real*8 gtp(420),gfed(21),gfac(21),tout(420),FEEDR
      real*8 s1,s2
      equivalence (lfeed,ifl(315))
      equivalence (FEEDR,MOTMAP(24))
c
c...Initialize
c
      if (iclass .eq. 5200) then
        !maxpt = 4200
        maxpt = 420
        mu = 20
        nu = 21
      else
        maxpt = 120
        mu    = 20 * (2 - ifl(82))
        nu    = 3 * (ifl(82) + 1)
      endif
      modsl  = 0
      modac  = 0
      
       !m1p   = 0
       
      svac   = ifl(315)
      if (rpfron .or. ifl(323) .eq. 1) ifl(315) = 0
      if (ifl(314) .ne. 0) modsl = 1
      if (ifl(315) .ne. 0) modac = 1
      if (ifl(95) .eq. 1 .or. ifl(42) .ne. 0) modsl = 2
      s1     = 10000.d0
      s2     = 0.d0
c     
c...Check # of points stored
c
      ntot = 0
      call ncl_tstptr(nrcn,i)
      if (i .ne. 0) call ptpnum (ntot)
      call ncl_zroptr(nrcn)
      nrf    = 0
      nrp    = 0 
      nor    = 3
      isf    = 3
      if (ntot .gt. 0) then
c
c...For slowdown OUT find point forward
c
         if (modac .eq. 1) then
            kd     = 0 - 1
            call fed2pt (iclass,ntot,kd,gfac,s2,nrf,nof)
            isf    = 2
            call ncl_zroptr(nrcn)
         end if
c
c...For slwdown find AT point from CS
c...reading records backward
c
         if (modsl .eq. 1) then
            kd     = 1
            call fed2pt (iclass,ntot,kd,gfed,s1,nrp,nop)
            if (nrp .lt. 1) isf = 1 
         end if
c
c...Do not restore primary feed if distance too short
c
         if (modsl+modac .gt. 0) then
            if (nrp .lt. nrf .and. modac .eq. 1) then
               if (isf .eq. 1 .and. FEEDC(2) .lt. FEEDC(1)) isf = 2
               if (nrp .ne. 0) nor = 1
               if (nrp .gt. 0 .and. FEEDC(2) .lt. FEEDC(1)) nrp = 0
            else if (nrp .eq. nrf) then
               if (s1 .lt. s2+.002) then
                  nor = 1
                  if (FEEDC(2) .lt. FEEDC(1)) then
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
      end if
c
c...Output points to cl file
c                    
  200 i     = 0
      inc   = 0
      isub  = isubcl
      call ncl_zroptr(nrcn)
c
c......Check if feed rate necessary before the first pt
c
      if (isf .ne. 0) call fedmut (FEEDC(isf))
      if (iclass .eq. 5200) then
         if (isf .eq. 2) isub = 2 
         if (isf .eq. 1) isub = 4 
      end if
c
c......Get points from storage & output motion
c
!210   call ptgetn (nrcn,nn,gtp,maxpt,nu, m1p)
210      call ptgetn (nrcn,nn,gtp,maxpt)
      if (nn .eq. 0) go to 1000
      is    = 0
c
c......Check for AUTOST the first point
c
      if (ifl(90) .eq. 1 .and. i .eq. 1) then
          do 215 k=1,nu,1
             tout(k) = gtp(k)
  215     continue
          numitm = 1
          isb = 5
          if (iclass .eq. 5200) isb = 1
          call putcl(iclass,isb,numitm,tout(1))
          is  = 1 
          i   = 1
      end if
c
c......Loop for all points in motion record
c
  300 if (i .ge. ntot) go to 1000
      if (is .eq. nn) go to 210 
c
c......Restore fedrat for selected point
c......if OUT is in effect 
c
      if (i*nu+1 .eq. nrf .and. FEEDR .ne. FEEDC(nor)) then
          if (nof .eq. 1) then
              do 325 k=1,nu,1
                  inc = inc + 1
                  tout(inc) = gfac(k)
  325         continue
          end if 
          if (iclass .eq. 5200) isub = 2
          if (inc .gt. 0) call putcl(iclass,isub,inc/nu,tout(1))
          if (nor .ne. 0) call fedmut (FEEDC(nor))
          isub = isubcl
          inc  = 0
      end if 
c
c......Output slow fedrat for selected point,
c......add exact AT point if any
c
      if (i*nu+1 .eq. nrp .and. FEEDR .ne. FEEDC(1)) then
          if (nop .eq. 1) then
              do 335 k=1,nu,1
                  inc = inc + 1
                  tout(inc) = gfed(k)
  335         continue
          end if 
          if (inc .gt. 0) call putcl(iclass,isub,inc/nu,tout(1))
          call fedmut (FEEDC(1))
          isub = isubcl
          if (iclass .eq. 5200) isub = 4
          inc  = 0
      end if 
      j    = 0 
c
c......Single point loop (nu * real8) 
c
  375 j    = j + 1 
      inc  = inc + 1
      tout(inc) = gtp(is*nu+j)
c
c......Flush GOTO buffer when full
c
      if (inc .eq. maxpt) then
          call putcl(iclass,isub,inc/nu,tout(1))
          isub = 6
          inc  = 0
      end if
c
c......If point ready get next
c
      if (j .eq. nu) then
          i  = i + 1
          is = is + 1
          go to 300
      end if
      go to 375
c
c...End of points list, flush buffer & 
c...restore fedrat if slowdn was set
c
 1000 if (inc .ne. 0) call putcl(iclass,isub,inc/nu,tout(1))
      if (nrp .ne. 0 .and. modac .eq. 0) call fedmut (FEEDC(3)) 
c 
c...End of routine
c...turn off slowdown if ONCE
c
 8000 if (ifl(314) .eq. 2) call resfed (1) 
      if (svac .ne. 0) ifl(315) = svac
      if (ifl(315) .eq. 2) call resfed (2)
      if (nn .ne. 0) then
         do 8005 i=1,nu
            FROMSV(i) = gtp((nn-1)*nu+i)
 8005    continue
         ifl(323) = 0
      end if
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE: subroutine fed2pt (iclass,knum,kdir,gfed,kfl,ksu)
c*    FUNCTION :  Finds a point at specified distance from the first 
c*                point or to the last point (see kdir) by analyzing
C*                points in GOTO record..
C*    PARAMETERS   
C*      INPUT:  iclass   - class of cl record
C*              knum     - Total # of points in GOTO record.  
C*              kdir     - Direction flag: 1 - distance to CS,
C*                         -1 - distance from previous motion. 
C*      OUTPUT: gfed(21) - Intermediate point at specified distance, 
C*                         FEDIS(1) for kdir = 1, FEDIS(2) for -1.
C*              kfl      - Pointer to the closest point in the GOTO
C*                         record which is before 'gfed' if used.
C*              ksu      - Status flag for gfed point: 0 - gfed is
C*                         not required (use 'kfl' pointer), 1 - gfed is
C*                         defined. 
C*          
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine fed2pt (iclass,knum,kdir,gfed,gndis,kfl,ksu)
c
      include 'com.com'
      include 'comgt.com'
c
      integer*2 iclass
      integer*4 kfl,ksu,knum,kdir 
      real*8 gtp(420),gfed(21),gndis
c
      real*8 dis,mdist,rnum,s
      real*8 tdis,pbuf(462)
c
      integer*4 i,j,is,ie,ii,nu,mu,nn,m1p,iof
      integer*4 maxpt
c
      equivalence (gtp(1),pbuf(22))
c
c...Initialize
c
      if (iclass .eq. 5200) then
        maxpt =420
        mu = 20
        nu = 21
      else
        maxpt = 120
        nu    = (ifl(82) + 1) * 3
        mu    = 120 / nu
      endif
      !snu = nu
      m1p   = 0
      kfl   = 0
      ksu   = 0
      dis   = 0.d0
      ii    = 0 - kdir
      nn    = 0
      do 55 i=1,nu
          pbuf(21-nu+i) = FROMSV(i)
   55 continue
c
c...Set indexis for AT from CS
c
  100 if (kdir .lt. 0) then
          !snu = nu
          !call ptgetn (nrcn,nn,gtp,maxpt,nu,m1p)
          call ptgetn (nrcn,nn,gtp,maxpt)
          !nu=snu
          is   = 1
          ie   = nn 
          iof  = 22 - nu 
          tdis = FEDIS(2)
          rnum = 0.d0
c
c...Set indexis for AT 
c
      else
          !call ptgetp (nrcn,nn,gtp,maxpt,nu, m1p)
          call ptgetp (nrcn,nn,gtp,maxpt)
          is   = nn + m1p 
          ie   = 2 
          iof  = 22
          if (is+kfl .eq. knum) ie = 1 
          tdis = FEDIS(1)
          rnum = 10000.d0
      end if
c     nu=snu
      if (nn .eq. 0) go to 800
c
c...Calculate distance
c
      !if (kdir .lt. 0) then
      !    ii    = 0 - kdir     
      !else
      !    ii=-kdir
      !end if
      do 125 i=is,ie,ii
          j     = (i - 1) * nu + iof
          rnum  = mdist (pbuf(j),pbuf(j+nu*ii),FHIGT,nu)
          dis   = dis + rnum
          if (dis .gt. tdis) go to 600
          kfl   = kfl + 1
  125 continue 
      m1p   = 1
c
c...Save last used point in buffer
c
      if (kfl .ge. knum) go to 800
      if (kdir .lt. 0) then
         do 205 i=1,nu,1
            pbuf(21-nu+i) = gtp(maxpt-nu+i)
  205    continue
      else 
         do 215 i=1,nu,1
            pbuf(maxpt+iof-1+i) = gtp(i)
  215    continue
      end if
      go to 100
c
c...Check if linearization is required
c
  600 if ((dis-tdis) .gt. .002) then
          s    = 1.d0 - (dis - tdis) / rnum
          call makfed (pbuf(j),pbuf(j+nu*ii),s,nu,gfed)
          ksu  = 1
      else 
c
c...Use closest point
c
          s    = (1 + ii)/2
          is   = j - 1 + nu * ii
          kfl  = kfl + 1
          do 305 i=1,nu,1
             gfed(i) = pbuf(is+i)
  305     continue 
      end if
c
c...Set pointer and actual distance of the point
c
  800 if (kdir .eq. 1) then
          kfl  = (knum - kfl - 1) * nu + 1 
          gndis = (1.0 - s) * rnum
      else
          kfl  = kfl * nu + 1
          gndis = s * rnum
      end if
c
c...End of routine
c
 8000 return
      end 
c
C********************************************************************/
C*    E_SUBROUTINE     : function mdist (gtp1,gtp2,ghig,nrf)
c*    FUNCTION :  Calculates distance between two pointvectors (or
C*                points) at specified higth up along the vector part
C*                of pointvector.  
C*    PARAMETERS   
C*       INPUT  : 
C*          gtp1,gtp2(6) - r*8 coordinates of pointvectors (6) or 
C*                         points (3).
C*          ghig   - R*8 higth at which distance is evaluted.
C*          nrf    - I*4 flag: 3 = gtp are points (higth offset is
C*                   not used), 6 = gtp are pointvectors (apply ghig). 
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      function mdist (gtp1,gtp2,ghig,nrf)
c
      real*8 mdist,gtp1(6),gtp2(6),ghig
      integer*4 nrf
c
      real*8 p1(3),p2(3),ndist 
      integer*4 i
c
      if (nrf .gt. 3) then
         do 55 i=1,3,1
             p1(i) = gtp1(i) + ghig * gtp1(i+3)
             p2(i) = gtp2(i) + ghig * gtp2(i+3)
   55    continue
         mdist = ndist (p1,p2)
      else
         mdist = ndist (gtp1,gtp2)
      end if
      return
      end
