C*********************************************************************
c*    NAME         :  yfmotion.f
c*       CONTAINS:
c*
c*				yfscrub
c*				yfrmill
c*				yfapok
c*				yflrgh
c*				yflfin
c*				yfapwd
c*
c*    COPYRIGHT 1996 (c) Numerical Control Computer Sciences Inc.
c*              All Rights Reserved.
c*    MODULE NAME AND RELEASE LEVEL
c*       yfmotrtn.f , 25.1
c*    DATE AND TIME OF LAST MODIFICATION
c*       04/29/15 , 15:10:58
c*********************************************************************
c
c*********************************************************************
c*    E_FUNCTION     : yfscrub (ps,sdat,pts,ierr)
c*       This function processes the SCRUB command.
c*    PARAMETERS
c*    INPUT  :
c*       ps           Part surface.
c*       sdat         Scrub parameters.
c*       pts          Bounding points.
c*    OUTPUT :
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfscrub (ps,sdat,pts,ierr)
c
      include 'com8a.com'
c
      integer*4 ps(4),pts(4),sdat(4),ierr
c
      common/hptcom/hldctp
      real*8 hldctp(27)
c
      integer*4 nn
      integer*2 iflg

      integer*4 jsn(2)
      integer*2 ksn(4)
      real*8 asn
c
      equivalence (asn,ksn,jsn)

      integer*2 iclass, isubcl
c
c...Initialize routine
c
      ifl(86) = 0
c
c...Set SCRUB parameters
c
      ksn(1) = 739
      ksn(2) = sdat(3) + 1
      sc(10) = asn
      jsn(1) = ps(1)
      ksn(4) = ps(2)
      sc(11) = asn
      sc(12) = sdat(1)
      sc(13) = sdat(2)
      ksn(3) = 3
      ksn(4) = 3
      jsn(1) = pts(1)
      sc(14) = asn
      jsn(1) = pts(2)
      sc(15) = asn
      jsn(1) = pts(3)
      sc(16) = asn
      jsn(1) = pts(4)
      sc(17) = asn
c
c...Issue SCRUB command
c
      ifl(270) = -1
      ifl(130) = 0
      call scrub
c
c...Store cldata
c
      if (ifl(2) .lt. 1) then
          call ncl_tstptr(nrcn, iflg)
          if (iflg .ne. 0) then
              call ptpnum (nn)
              if (nn .gt. 0) then
                iclass = 5200
                isubcl = 5
                call csdist (iclass, isubcl)
                call conv8_8(hldctp(22),hldctp(7),6)
                iclass = 5220
                isubcl = 3
                numitm = 13
                call putcl (iclass,isubcl,numitm,hldctp)
              endif
              call ptclos
          endif
      endif
c
c...End of routine
c
 8000 ierr   = ifl(2)
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yfrmill (ps,ds,cs,irdat,rdat,ierr)
c*       This function processes the RMILL command.
c*    PARAMETERS
c*    INPUT  :
c*       ps           Part surface.
c*       ds           Drive surfaces and attributes.
c*       cs           Check surfaces and attributes.
c*       irdat        Integer Rmill parameters.
c*       rdat         Rmill parameters.
c*    OUTPUT :
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfrmill (ps,ds,cs,irdat,rdat,ierr)
c
      include 'com8a.com'
c
      integer*4 ps(4),ds(6),cs(6),irdat(8),ierr
c
      real*8 rdat(16)
c
      common/hptcom/hldctp
      real*8 hldctp(27)

      common/regthk/ rthk
      real*8 rthk(8)
c
      integer*4 nn
      integer*4 jsn(2)
      integer*2 ksn(4),irsc19(8),isc22(4),irapid(2)
      real*4 rsc16,rsc21,rsc19(4)
      real*8 asn
c
      equivalence (asn,ksn,jsn)
      equivalence (sc(16),rsc16), (sc(21),rsc21)
      equivalence (sc(19),rsc19,irsc19), (sc(22),isc22)

      integer*2 iclass, isubcl
c
      data irapid /0,5/
c
c...Initialize routine
c
      ifl(86) = 0
c
c...Setup RMILL statement
c
      ksn(1) = 750
      ksn(2) = irdat(1)
      ksn(3) = irdat(2)
      sc(10) = asn
c
c...Setup Part surface
c
      jsn(1) = ps(1)
      ksn(4) = ps(2)
      sc(11) = asn
c
c...Setup Drive surfaces
c
      jsn(1) = ds(1)
      ksn(4) = ds(2)
      sc(14) = asn
      isc22(1) = ds(3)
c
      jsn(1) = ds(4)
      ksn(4) = ds(5)
      sc(15) = asn
      isc22(2) = ds(6)
c
c...Setup Check surfaces
c
      jsn(1) = cs(1)
      ksn(4) = cs(2)
      sc(12) = asn
      isc22(3) = cs(3)
c
      jsn(1) = cs(4)
      ksn(4) = cs(5)
      sc(13) = asn
      isc22(4) = cs(6)
c
c...Setup Rmill parameters
c
      if (irdat(7) .ne. 0) then
          jsn(1) = irdat(3)
          ksn(3) = 0
          ksn(4) = irdat(4)
          sc(16) = asn
      else
          rsc16 = rdat(1)
      endif
c
      sc(17) = rdat(2)
      sc(18) = rdat(3)
      do 100 i=1,3,1
          if (rdat(i+3) .eq. 0.) then
              irsc19(i*2-1) = irapid(1)
              irsc19(i*2) = irapid(2)
          else
              rsc19(i) = rdat(i+3)
          endif
  100 continue
c
      if (irdat(8) .ne. 0) then
          jsn(1) = irdat(5)
          ksn(3) = 0
          ksn(4) = irdat(6)
          sc(21) = asn
      else
          rsc21 = rdat(7)
      endif
c
c...Setup THICKs
c
      do 200 i=1,8,1
         rthk(i) = rdat(i+7)
  200 continue
c
c...Issue RMILL command
c
      ifl(270) = -1
      ifl(130) = 0
      lrmill = .true.
      call regmil
      lrmill = .false.
c
c...Store cldata
c
      if (ifl(2) .lt. 1) then
          call ncl_tstptr(nrcn, iflg)
          if (iflg .ne. 0) then
              call ptpnum (nn)
              if (nn .gt. 0) then
                iclass = 5200
                isubcl = 5
                call csdist (iclass, isubcl)
                call conv8_8(hldctp(22),hldctp(7),6)
                iclass = 5220
                isubcl = 3
                numitm = 13
                call putcl (iclass,isubcl,numitm,hldctp)
              endif
              call ptclos
          endif
      endif
c
c...End of routine
c
 8000 ierr   = ifl(2)
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yfapok (ps,ds,islatt,isle,irdat,rdat,ierr)
c*       This function processes the Advanced POCKET command.
c*    PARAMETERS
c*    INPUT  :
c*       ps           Part surface.
c*       ds           Perimeter geometry and perimeter/island attributes.
c*       isle         Island geometry.
c*       irdat        Integer Pocket parameters.
c*       rdat         Pocket parameters.
c*    OUTPUT :
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfapok (ps,ds,islatt,island,irdat,rdat,ierr)
c
      include 'com4a.com'
      include 'rrdm.com'
c
      integer*4 ps(4),ds(6),island(200),irdat(20),ierr,islatt(100)
c
      real*8 rdat(16)
c
      common/hptcom/hldctp
      real*8 hldctp(27)
c
      integer*4 nn, iflg
c
      integer*4 jsn(2)
      integer*2 ksn(4)
      real*8 asn
c
      equivalence (asn,ksn,jsn)
      equivalence (sc(16),rsc16), (sc(21),rsc21)
c
      integer*2 iclass, isubcl
c
      integer*2 ramp,plunge,helix,omit,idir(2),ispir(2),iret(2),icorn(2)
c
      data ramp /854/,plunge /1001/,helix /855/,omit /172/
      data idir /60,59/,iret /113,112/,icorn /182,183/,ispir /653,652/

      integer*2 izro,iwlev
      data izro /0/
      data iwlev /0/
c
c...Initialize routine
c
      ifl(86) = 0
c
c...Setup POKMOD statement
c......Entry method
c
      nramps = irdat(1)
      if (irdat(2) .eq. 1) then
          entry = ramp
      else if (irdat(2) .eq. 2) then
          entry = plunge
      else if (irdat(2) .eq. 3) then
          entry = helix
      else
          entry = omit
      endif
      rmpdis = rdat(1)
      rtract = irdat(3)
c
c......Clearance level
c
      cltyp = irdat(14)
      if (irdat(14) .eq. 1) then
          jsn(1) = irdat(15)
          ksn(3) = 0
          ksn(4) = irdat(16)
          clrlvl = asn
      else
          clrlvl = rdat(5)
      endif
c
c......Distances & Directions
c
      numlvl = rdat(2)
      retdis = rdat(3)
      pdir = idir(irdat(4))
      ispirl = ispir(irdat(5))
c
c......Retract/Corner Options
c
      slift = iret(irdat(6)+1)
      corner = icorn(irdat(7))
c
c..... warn/nowarn/avoid flag
c
      nwrn = 0
      if (iabs(irdat(8)) .eq. 1) nwrn = irdat(8)
c
c......Step-overs
c
      maxstp = rdat(13)
      minstp = rdat(14)
c
c......Feedrates
c
      if (irdat(17) .eq. 0) then
          slwang = 2.0
      else
          slwang = cos(rdat(6)/57.29577951)
      endif

      genfr  = rdat(7)
      retrfr = rdat(8)
      posfr  = rdat(9)
      entrfr = rdat(10)
      tranfr = rdat(11)
      finfr  = rdat(12)
      frsfr = -1.
c
c...Setup POCKET statement
c
      isc10(1) = 738
      isc10(2) = 3
c
c......Part Surface
c
      jsn(1) = ps(1)
      ksn(4) = ps(2)
      sc(11) = asn
c
c......Top plane
c
      if (irdat(11) .eq. 1) then
          isc10(2) = 2
          jsn(1) = irdat(12)
          ksn(3) = 0
          ksn(4) = irdat(13)
          sc(12) = asn
      else
          isc10(2) = 3
          sc(12) = rdat(4)
      endif
c
c......Pocket perimeter
c
      jsn(1) = ds(1)
      ksn(3) = 1
      if (ds(3) .eq. 1) ksn(3) = -1
      if (ds(3) .eq. 2) ksn(3) = 0
      ksn(4) = ds(2)
      rsvasw(1) = asn
c
c......Pocket islands
c
      do 500 i=1,ds(4),1
          jsn(1) = island(i*2-1)
          ksn(3) = -1
          if (islatt(i) .eq. 1) ksn(3) = 1
          if (islatt(i) .eq. 2) ksn(3) = 0
          ksn(4) = island(i*2)
          rsvasw(i+1) = asn
  500 continue
      isc10(3) = ds(4) + 1
c
c......Optional endpoint
c
      if (irdat(18) .eq. 1) then
          isc10(4) = 2
          jsn(1) = irdat(19)
          ksn(4) = irdat(20)
          sc(13) = asn
      else
          isc10(4) = 0
      endif
c
c...Issue POCKET command
c
      ifl(270) = -1
      ifl(130) = 0
      call razpok(izro,iwlev)
c
c...Store cldata
c
      if (ifl(2) .lt. 1) then
          call ncl_tstptr(nrcn, iflg)
          if (iflg .ne. 0) then
              call ptpnum (nn)
              if (nn .gt. 0) then
                iclass = 5200
                isubcl = 5
                call csdist (iclass, isubcl)
                call conv8_8(hldctp(22),hldctp(7),6)
                iclass = 5220
                isubcl = 3
                numitm = 13
                call putcl (iclass,isubcl,numitm,hldctp)
              endif
              call ptclos
          endif
      endif
c
c...End of routine
c
 8000 ierr   = ifl(2)
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yflrgh (ps,ds,irdat,rdat,pwd,ierr)
c*       This function processes the Lathe ROUGH command.
c*    PARAMETERS
c*    INPUT  :
c*       ps           Rough stock shape.
c*       ds           Finished part shape.
c*       irdat        Integer Rough parameters.
c*       rdat         Rough parameters.
c*       pwd          Post-processor command parameters.
c*    OUTPUT :
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yflrgh (ps,ds,irdat,rdat,pwd,ierr)
c
      include 'com4a.com'
      include 'rrdm.com'
c
      integer*4 ps(2),ds(2),irdat(20),ierr
c
      real*8 rdat(16),pwd(300)
c
      common/hptcom/hldctp
      real*8 hldctp(27)
c
      integer*4 nn
c
      integer*2 ix, iflg
c
      integer*4 jsn(2)
      integer*2 ksn(4)
      real*4 rsn(2)
      real*8 asn
c
      real*8 jbrx(36)
      integer*2 jbx(144)
      equivalence (jbrx,jbx)
c
      equivalence (asn,rsn,ksn,jsn)
      equivalence (sc(16),rsc16), (sc(21),rsc21)
c
      integer*2 iclass, isubcl
c
      integer*2 depth,cutang,retrct,out,retrn,off,xaxis,yaxis,
     1          lathe
c
      data depth /510/, cutang /160/, retrct /7/, out /653/
      data retrn /322/, off /72/, xaxis /84/, yaxis /85/
      data lathe /700/
c
c...Initialize routine
c
      ifl(86) = 0
c
c...Rough stock shape
c
      jsn(1) = ps(1)
      ksn(4) = ps(2)
      jbrx(1) = asn
c
c...CLDIST
c
      jbrx(2) = rdat(1)
c
c...Finished part shape
c
      jsn(1) = ds(1)
      ksn(4) = ds(2)
      jbrx(3) = asn
c
c...STOCK
c
      jbrx(4) = rdat(2)
      jbrx(5) = rdat(3)
c
c...DEPTH
c
      ix = 6
      call yfapwd (irdat(1),pwd(1),ix,jbrx)
      ksn(1) = depth
      ksn(2) = 0
      rsn(2) = rdat(4)
      jbrx(ix) = asn
      ix = ix + 1
c
c...CUTANG
c
      call yfapwd (irdat(6),pwd(51),ix,jbrx)
      ksn(1) = cutang
      ksn(2) = 0
      rsn(2) = rdat(5)
      jbrx(ix) = asn
      ix = ix + 1
c
c...RETRCT
c
      call yfapwd (irdat(8),pwd(101),ix,jbrx)
      ksn(1) = retrct
      ksn(2) = out
      jbrx(ix) = asn
      ix = ix + 1
c
      rsn(1) = rdat(6)
      rsn(2) = rdat(7)
      jbrx(ix) = asn
      ix = ix + 1
c
c...RETURN
c
      call yfapwd (irdat(10),pwd(151),ix,jbrx)
      if (irdat(3) .ne. 3) then
          ksn(1) = retrn
          jbrx(ix) = asn
          ix = ix + 1
          asn = 0
          if (irdat(3) .eq. 1) then
              ksn(4) = xaxis
          else if (irdat(3) .eq. 2) then
              ksn(4) = yaxis
          else if (irdat(3) .eq. 3) then
              ksn(4) = off
          else
              jsn(1) = irdat(4)
              ksn(3) = 3
              ksn(4) = irdat(5)
          endif
          jbrx(ix) = asn
          ix = ix + 1
      endif
c
c...Final post-processor command
c
      call yfapwd (irdat(12),pwd(201),ix,jbrx)
c
c...Setup Lathe statement
c
      isc10(1) = lathe
      isc10(2) = 1
      isc10(3) = ix - 1
      call putran (jbx,ix)
c
c...Issue Lathe command
c
      ifl(270) = -1
      ifl(130) = 0
      call lthctl
c
c...Store cldata
c
      if (ifl(2) .lt. 1) then
          call ncl_tstptr(nrcn, iflg)
          if (iflg .ne. 0) then
              call ptpnum (nn)
              if (nn .gt. 0) then
                iclass = 5200
                isubcl = 5
                call csdist (iclass, isubcl)
                call conv8_8(hldctp(22),hldctp(7),6)
                iclass = 5220
                isubcl = 3
                numitm = 13
                call putcl (iclass,isubcl,numitm,hldctp)
              endif
              call ptclos
          endif
      endif
c
c...End of routine
c
 8000 ierr   = ifl(2)
      rpfron = .false.
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yflfin (ds,irdat,rdat,pwd,ierr)
c*       This function processes the Lathe FINISH command.
c*    PARAMETERS
c*    INPUT  :
c*       ds           Finished part shape.
c*       irdat        Integer Rough parameters.
c*       rdat         Rough parameters.
c*       pwd          Post-processor command parameters.
c*    OUTPUT :
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yflfin (ds,irdat,rdat,pwd,ierr)
c
      include 'com4a.com'
      include 'rrdm.com'
c
      integer*4 ds(2),irdat(20),ierr
c
      real*8 rdat(16),pwd(300)
c
      common/hptcom/hldctp
      real*8 hldctp(27)
c
      integer*4 nn
c
      integer*2 ix
c
      integer*4 jsn(2)
      integer*2 ksn(4)
      real*4 rsn(2)
      real*8 asn
c
      real*8 jbrx(36)
      integer*2 jbx(144)
      equivalence (jbrx,jbx)
c
      equivalence (asn,rsn,ksn,jsn)
      equivalence (sc(16),rsc16), (sc(21),rsc21)
c
      integer*2 iclass, isubcl
c
      integer*2 engage,retrct,out,retrn,off,xaxis,lathe,invers
c
      data engage /324/, retrct /7/, out /653/
      data retrn /322/, off /72/, xaxis /84/
      data lathe /700/, invers /822/
c
c...Initialize routine
c
      ifl(86) = 0
c
c...Finished part shape
c
      jsn(1) = ds(1)
      ksn(4) = ds(2)
      jbrx(1) = asn
c
c...STOCK
c
      jbrx(4) = rdat(1)
      jbrx(5) = rdat(2)
c
c...INVERS
c
      ix = 6
      if (irdat(1) .ne. 0) then
         ksn(1) = invers
         jbrx(ix) = asn
         ix = ix + 1
      endif
c
c...ENGAGE
c
      ix = 6
      call yfapwd (irdat(2),pwd(1),ix,jbrx)
      ksn(1) = engage
      ksn(2) = out
      jbrx(ix) = asn
      ix = ix + 1
      rsn(1) = rdat(3)
      rsn(2) = rdat(4)
      jbrx(ix) = asn
      ix = ix + 1
c
c...RETRCT
c
      call yfapwd (irdat(4),pwd(51),ix,jbrx)
      ksn(1) = retrct
      ksn(2) = out
      jbrx(ix) = asn
      ix = ix + 1
c
      rsn(1) = rdat(5)
      rsn(2) = rdat(6)
      jbrx(ix) = asn
      ix = ix + 1
c
c...RETURN
c
      call yfapwd (irdat(6),pwd(101),ix,jbrx)
      if (irdat(8) .ne. 3) then
          ksn(1) = retrn
          jbrx(ix) = asn
          ix = ix + 1
          asn = 0
          if (irdat(8) .eq. 1) then
              ksn(4) = xaxis
          else if (irdat(8) .eq. 3) then
              ksn(4) = off
          else
              jsn(1) = irdat(9)
              ksn(3) = 3
              ksn(4) = irdat(10)
          endif
          jbrx(ix) = asn
          ix = ix + 1
      endif
c
c...Final post-processor command
c
      call yfapwd (irdat(11),pwd(151),ix,jbrx)
c
c...Setup Lathe statement
c
      isc10(1) = lathe
      isc10(2) = 0
      isc10(3) = ix - 1
      call putran (jbx,ix)
c
c...Issue Lathe command
c
      ifl(270) = -1
      ifl(130) = 0
      call lthctl
c
c...Store cldata
c
      if (ifl(2) .lt. 1) then
          call ncl_tstptr(nrcn, iflg)
          if (iflg .ne. 0) then
              call ptpnum (nn)
              if (nn .gt. 0) then
                iclass = 5200
                isubcl = 5
                call csdist (iclass, isubcl)
                call conv8_8(hldctp(22),hldctp(7),6)
                iclass = 5220
                isubcl = 3
                numitm = 13
                call putcl (iclass,isubcl,numitm,hldctp)
              endif
              call ptclos
          endif
      endif
c
c...End of routine
c
 8000 ierr   = ifl(2)
      rpfron = .false.
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yfapwd (irdat,pwd,ix,jbrx)
c*       This function stores a post-processor command within a LATHE/
c*       ROUGH/FINISH structure.
c*    PARAMETERS
c*    INPUT  :
c*       irdat        (1) = Major word (0 if no command).
c*                    (2) = Number of minor words/values.
c*       pwd          Actual minor words/values.
c*       ix           Beginning increment for 'jbrx' structure.
c*    OUTPUT :
c*       ix           Next available index into 'jbrx'.
c*       jbrx         Formatted post-processor command.
c*    RETURNS      :
c*       none
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfapwd (irdat,pwd,ix,jbrx)
c
      integer*2 ix
      integer*4 irdat(2)
c
      real*8 pwd(50),jbrx(50)
c
      integer*2 i
c
      integer*2 ksn(4)
      real*8 asn
      equivalence (asn,ksn)
c
c...Store the post command structure
c
      if (irdat(1) .ne. 0) then
         ksn(1) = 2000
         ksn(2) = irdat(1)
         ksn(3) = irdat(2) + 1
         jbrx(ix) = asn
         ix = ix + 1
         do 100 i=1,irdat(2),1
             jbrx(ix) = pwd(i)
             ix = ix + 1
  100    continue
      endif
c
      return
      end
