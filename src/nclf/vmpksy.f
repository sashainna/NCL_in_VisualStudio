C*********************************************************************
C*    NAME         :  vmpksy.f
C*       CONTAINS:
C*           vmpini  vmodsy  vmpksy  vmpprm  vmppar  vmpsav vmprst
C*
C*    COPYRIGHT 2011 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       vmpksy.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:53
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vmpini
c*      This routine initializes the paramters used by the VOLUMILL
c*      commands.
c*
c*         IVMPFL(1)  = 0 = CLW pocket direction.
c*                      1 = CCLW pocket direction.
c*         IVMPFL(2)  = 1 = Clearance plane is incremental distance, 2 = plane,
c*                      3 = plane label.
c*         IVMPFL(3)  = 1 = Top plane is incremental distance, 2 = plane,
c*                      3 = plane label.
c*         IVMPFL(4)  = 0 = Don't smooth boundaries.
c*                      1 = Smooth boundaries.
c*         IVMPFL(5)  = Number of flutes on cutter.
c*         IVMPFL(6)  = 1 = Ramp entry, 2 = Helix entry.
c*         IVMPFL(7)  = 0 = Perform same level passes first.
c*                      1 = Perform depth passes first.
c*         IVMPFL(8)  = 2 = Bottom plane is plane, 3 = plane label.
c*         IVMPFL(9)  = 0 = Cut slots to transfer to new pocket area.
c*         IVMPFL(9)  = 1 = Use side-mill only strategy.
c*         IVMPFL(10) = 0 = Depth parameter is the recommended distance (DIST).
c*                    = 1 = Depth parameter is the maximum distance (DEPTH).
c*
c*         VMPPAS(1) = Maximum depth of cut.
c*         VMPPAS(2) = Transitional move maximum depth of cut.
c*         VMPPAS(3) = Stepover between passes.
c*         VMPPAS(4) = Stepover between arc passes for opening new area.
c*         VMPPAS(5) = Minimum boundary radius that can be machined.
c*         VMPPAS(6) = Minimum radius for final pass corners.
c*         VMPPAS(7) = Minimum angle for corner when smoothing boundaries.
c*
c*         VMPBPL    = Pocket bottom plane.
c*         VMPTPL    = Pocket top plane.
c*
c*         VMPTHK(1) = PS final thick.
c*         VMPTHK(2) = DS final thick.
c*         VMPTHK(3) = Offset open curve thick.
c*
c*         VMPENT(1) = Ramp/Helix entry angle.
c*         VMPENT(2) = Helix radius.
c*
c*         IVMPFD(i) = 0 = Use active feedrate, 1 = feedrate is specified,
c*                     2 = Use percentage of general feedrate.
c*
c*         VMPFED(1) = General feedrate.
c*         VMPFED(2) = Positioning feedrate.
c*         VMPFED(3) = Entry feedrate.
c*         VMPFED(4) = Transitional feedrate between pockets.
c*
c*         IVMPSP(i) = 0 = Use active spindle speed, 1 = speed is specified,
c*                     2 = Use percentage of spindle speed.
c*
c*         VMPSPD(1) = General spindle speed.
c*         VMPSPD(2) = Spindle speed during entry.
c*         VMPSPD(3) = Spindle speed during transitional moves.
c*         VMPSPD(4) = Amount to dwell after changing spindle speeds.
c*
c*         VMPCLF    = Clearance Plane.
c*         VMPCLF(1) = Clearance distance.
c*         VMPRPT    = Rapto plane.
c*         VMPRPT(1) = Rapto distance.
c*         VMPPOS(1) = Distance to raise above floor.
c*         VMPPOS(2) = Max distance when positioning between pockets.
c*
c*         IVMADJ    = 1 = Adjust entry feed rate.
c*         VMPFLN    = Length of cut portion of cutter.
c*         VMPTLN    = Overall length of cutter.
c*         IVMCON    = 1 = Contour ramping permitted
c*         IVMMIN    = 1 = Enforce minimum feed rate
c*         VMPMIN    = Minimum feed rate
c*         VMPRAP(1) = XY rapid feed rate
c*         VMPRAP(2) = Z rapid feed rate
c*         VMPDRL(1) = Pre-drilled holes diameter.
c*         VMPDRL(2) = Pre-drilled holes angle.
c*
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
      subroutine vmpini
c
      include 'com8a.com'
      include 'vmpcom.com'
c
c...Initialize parameters
c
      IVMPFL(1) = 1
      IVMPFL(2) = 1
      IVMPFL(3) = 1
      IVMPFL(5) = 0
      IVMPFL(6) = 1
      IVMPFL(7) = 0
      IVMPFL(8) = 2
      IVMPFL(9) = 1
      IVMPFL(10) = 0
c
      VMPPAS(1) = -1.
      VMPPAS(2) = .5
      VMPPAS(3) = 0.
      VMPPAS(4) = 0.
      VMPPAS(5) = 0.
      VMPPAS(6) = 0.
      VMPPAS(7) = 10.
c
      VMPENT(1) = 10.
      VMPENT(2) = .5
c
      VMPPOS(1) = .1
      VMPPOS(2) = .1
c
      VMPFLN = 0.
      VMPTLN = 0.
      IVMCON = 1
      VMPRAP(1) = 0.
      VMPRAP(2) = 0.
      IVMMIN = 0
      VMPMIN = 0.
c
      do 100 i=1,4,1
          IVMPFD(i) = 0
          VMPFED(i) = 0.
          IVMPSP(i) = 0
          VMPSPD(i) = 0.
          VMPBPL(i) = 0.
          VMPCLF(i) = 0.
          VMPRPT(i) = 0.
          VMPDRL(i) = 0.
  100 continue
      IVMPSP(4) = 1
      VMPBPL(3) = 1.
      VMPTPL(1) = 0.
      VMPCLF(1) = .5
      VMPRPT(1) = .1
      IVMADJ = 1
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vmodsy
c*      This routine handles the parsing and syntax checking for
c*      the VMPMOD command.
c*
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
      subroutine vmodsy
c
      include 'com8a.com'
      include 'vmpcom.com'
c
      integer*2 ierr,i,j
      integer*4 jerr
c
      integer*2 RAMP,HELIX,CLW,CCLW,CLRSRF,RAPTO,RTRCTO,DEPTH,STEP,INCR,
     1          RADIUS,ROUGH,SMOOTH,FEDRAT,SPINDL,DWELL,FLUTES,DIST,
     2          RAPID,LEVEL,DEEP,SIDE,SLOT,IN,OUT,SMALL,ON,OFF,MIN,DRILL
c
      character*80 tcutcm,tempc
      character*96 tok2
      integer*4 strlen1,isub
      integer*2 li,lj,is1,is4,nc
      integer*2 itemp(4),tctcm1,tctcm2,tjctcm
      real*8 temp
      equivalence (itemp,temp)
      
      parameter (CCLW   =   59)
      parameter (CLRSRF = 1057)
      parameter (CLW    =   60)
      parameter (DEEP   =  153)
      parameter (DEPTH  =  510)
      parameter (DIST   =  575)
      parameter (DRILL  =  163)
      parameter (DWELL  =  197)
      parameter (FEDRAT = 1009)
      parameter (FLUTES =  206)
      parameter (HELIX  =  855)
      parameter (INCR   =   66)
      parameter (LEVEL  =  759)
      parameter (RADIUS =   23)
      parameter (RAPID  =    5)
      parameter (RAPTO  =  280)
      parameter (RAMP   =  854)
      parameter (ROUGH  =  320)
      parameter (RTRCTO =  295)
      parameter (SIDE   =   94)
      parameter (SLOT   =  207)
      parameter (SMOOTH = 1085)
      parameter (SPINDL = 1031)
      parameter (STEP   =   92)
      parameter (IN     =  652)
      parameter (OUT    =  653)
      parameter (SMALL  =  663)
      parameter (ON     =   71)
      parameter (OFF    =   72)
      parameter (MIN    =  964)
c
c...Check for Slash (/)
c
      if (ifl(389) .ne. 1) go to 9100
      if (nextyp.ne.5) go to 9000
c
c...Get next token
c
      ifl(44) = 9
      iprs   = 0
  200 if (iprs .eq. 0) then
          if (nextyp .eq. 11) go to 7000
          call parsit
          if (ityp .eq. 7) go to 7000
      endif
      if (ityp .ne. 1) go to 9030
      istsv  = ist
      iprs   = 0
c
c...VMPMOD/RAMP,ang
c
      if (ist .eq. RAMP) then
          IVMPFL(6) = 1
          call parsit
          if (scalar) then
              VMPENT(1) = tv
          else
               go to 9050
          endif
c
c...VMPMOD/HELIX,ang,rad
c
      else if (ist .eq. HELIX) then
          IVMPFL(6) = 2
          call parsit
          if (scalar) then
              VMPENT(1) = tv
              call parsit
              if (scalar) then
                  VMPENT(2) = tv
              else
                   go to 9050
              endif
          endif
c
c...VMPMOD/CLW
c
      else if (ist .eq. CLW) then
          IVMPFL(1) = 0
c
c...VMPMOD/CCLW
c
      else if (ist .eq. CCLW) then
          IVMPFL(1) = 1
c
c...VMPMOD/CLRSRF,pln
c...              d
c
      else if (ist .eq. CLRSRF) then
          call parsit
          sca = 0
          tok2 = token2
          isub = ivxsub
          if (scalar) sca = 1
          call prfloc (VMPCLF,3,ierr)
          if (ierr .eq. 0) then
              IVMPFL(2) = 2
              if (sca .eq. 0 .and. tok2(1:1) .ne. '@') then
                  IVMPFL(2) = 3
                  call expnm2 (tok2,isub,nc,VMPLBC)
              endif
          else if (sca .eq. 1) then
              IVMPFL(2) = 1
              iprs   = 1
          else
              go to 9070
          endif
c
c...VMPMOD/RAPTO,d
c
      else if (ist .eq. RAPTO) then
          call parsit
          if (.not. scalar) go to 9050
          VMPRPT(1) = tv
c
c...VMPMOD/RTRCTO,d1,d2
c
      else if (ist .eq. RTRCTO) then
          call parsit
          if (scalar) then
              VMPPOS(1) = tv
              call parsit
              if (scalar) then
                  VMPPOS(2) = tv
              else
                  VMPPOS(2) =  VMPPOS(1)
                  iprs   = 1
              endif
          else
              go to 9050
          endif
c
c...VMPMOD/DEPTH,d1
c
      else if (ist .eq. DEPTH) then
          call parsit
          if (scalar) then
              IVMPFL(10) = 1
              VMPPAS(1) = tv
          else
              go to 9050
          endif
c
c...VMPMOD/DIST,d1
c
      else if (ist .eq. DIST) then
          call parsit
          if (scalar) then
              IVMPFL(10) = 0
              VMPPAS(1) = tv
          else
              go to 9050
          endif
c
c...VMPMOD/STEP,d1
c
      else if (ist .eq. STEP) then
          call parsit
          if (scalar) then
              VMPPAS(3) = tv
          else
              go to 9050
          endif
c
c...VMPMOD/RADIUS,rad
c
      else if (ist .eq. RADIUS) then
          call parsit
          if (scalar) then
              VMPPAS(5) = tv
          else
              go to 9050
          endif
c
c...VMPMOD/SIDE
c
      else if (ist .eq. SIDE) then
          IVMPFL(9) = 1
c
c...VMPMOD/SLOT
c
      else if (ist .eq. SLOT) then
          IVMPFL(9) = 0
          call parsit
          if (scalar) then
              VMPPAS(2) = tv
              call parsit
              if (scalar) then
                  VMPPAS(4) = tv
              else
                  go to 9050
              endif
          else
              go to 9050
          endif
c
c...VMPMOD/ROUGH
c
      else if (ist .eq. ROUGH) then
          IVMPFL(4) = 0
c
c...VMPMOD/SMOOTH
c
      else if (ist .eq. SMOOTH) then
          IVMPFL(4) = 1
          call parsit
          if (scalar) then
              VMPPAS(7) = tv
              call parsit
              if (scalar) then
                  VMPPAS(6) = tv
              else
                  iprs   = 1
              endif
          else
              go to 9050
          endif
c
c...FEDRAT,feeds
c
      else if (ist .eq. FEDRAT) then
c
c.....Added option to adjust feed rate for entry - ASF 12/3/13.
c
          IVMADJ = 1
          j = 1
          do 500 i=1,5,1
              call parsit
cc              if (vocab .and. ist .eq. RAPID) then
cc                  IVMPFD(i) = 2
              if (scalar .and. j .lt. 5) then
                  if (tv .eq. 0.) then
                      IVMPFD(j) = 0
                  else if (tv .lt. 0.) then
                      IVMPFD(j) = 2
                      VMPFED(j) = -tv
                  else
                      IVMPFD(j) = 1
                      VMPFED(j) = tv
                  endif
                  j = j + 1
              else if (vocab) then
                  if (ist .eq. IN) then
                      IVMADJ = 0
                  else if (ist .eq. OUT) then
                      IVMADJ = 1
                  else if (j .lt. 5) then
                      go to 9182
                  else
                      iprs = 1
                  endif
              else
                  iprs = 1
              endif
  500     continue
c
c...SPINDL,speeds
c
      else if (ist .eq. SPINDL) then
          do 600 i=1,3,1
              call parsit
              if (scalar) then
                  if (tv .eq. 0.) then
                      IVMPSP(i) = 0
                  else if (tv .lt. 0.) then
                      IVMPSP(i) = 2
                      VMPSPD(i) = -tv
                  else
                      IVMPSP(i) = 1
                      VMPSPD(i) = tv
                  endif
              else
                  go to 9050
              endif
  600     continue
c
c...VMPMOD/DWELL,d
c
      else if (ist .eq. DWELL) then
          call parsit
          if (scalar) then
              VMPSPD(4) = tv
          else
              go to 9050
          endif
c
c...VMPMOD/FLUTES,n,len,cutlen
c
      else if (ist .eq. FLUTES) then
          call parsit
          if (scalar) then
              IVMPFL(5) = tv
              if (nextyp .ne. 11) then
                  call parsit
                  if (scalar) then
                      VMPFLN = tv
                      VMPTLN = tv
                  else
                      iprs = 1
                  endif
                  if (nextyp .ne. 11) then
                      call parsit
                      if (scalar) then
                          VMPTLN = tv
                      else
                          iprs = 1
                      endif
                  endif
              endif
          else
              go to 9050
          endif
c
c...VMPMOD/LEVEL
c
      else if (ist .eq. LEVEL) then
          IVMPFL(7) = 0
c
c...VMPMOD/DEEP
c
      else if (ist .eq. DEEP) then
          IVMPFL(7) = 1
c
c...VMPMOD/SMALL
c
      else if (ist .eq. SMALL) then
          call parsit
          if (ityp .ne. 1) go to 9030
          if (ist.eq.OFF) then
              IVMCON = 0
          else if (ist.eq.ON) then
              IVMCON = 1
          else
              go to 9030
          endif
c
c...VMPMOD/MIN
c
      else if (ist .eq. MIN) then
          call parsit
          if (ityp.eq.1.and.ist.eq.OFF) then
              IVMMIN = 0
          else if (scalar) then
              IVMMIN = 1
              VMPMIN = tv
          else
              go to 9030
          endif
c
c...VMPMOD/RAPID
c
      else if (ist .eq. RAPID) then
          call parsit
          if (scalar) then
              VMPRAP(1) = tv
              VMPRAP(2) = tv
              if (nextyp .ne. 11) then
                  call parsit
                  if (scalar) then
                      VMPRAP(2) = tv
                  else
                      iprs = 1
                  endif
              endif
          else
              go to 9030
          endif
c
c...VMPMOD/DRILL
c
      else if (ist .eq. DRILL) then
          call parsit
          if (scalar) then
              VMPDRL(1) = tv
              call parsit
              if (scalar) then
                  VMPDRL(2) = tv
               else
                   go to 9030
               endif
          else
              go to 9030
          endif
c
c...Unrecognized minor word
c
      else
          go to 9040
      endif
      if (nextyp .ne. 11) go to 200
c
c...Normal exit
c
 7000 continue
c
c...End of routine
c
 8000 return
c
c...Slash expected (/)
c
 9000 call error (22)
      go to 8000
c
c...Error from geo parsing routine
c
 9020 call error (ierr)
      go to 8000
c
c...Vocabulary word expected
c
 9030 call error (232)
      go to 8000
c
c...Unrecognized minor word
c
 9040 call error (436)
      go to 8000
c
c...Number expected
c
 9050 call error (7)
      go to 8000
c
c...Input value out of range
c
 9060 call error (445)
      go to 8000
c
c...Plane expected
c
 9070 call error (19)
      go to 8000
c
c...Plane expected
c
 9182 call error (182)
      go to 8000
c
c...Not authorized to run VoluMill
c
 9100 call error (5)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vmpksy
c*      This routine handles the parsing and syntax checking for
c*      the VMPOCK command.
c*
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
      subroutine vmpksy
c
      include 'com8a.com'
      include 'rrdm.com'
      include 'vocab.com'
      include 'vmpcom.com'
      include 'const.com'
c
      integer*2 ierr,i,j,ietype,nwds,ktv(4),tgeotp,tityp,numscl
      integer*2 npts,tinx,itrflg
      integer*4 jerr,nclkey,jtv(2),vcpara,ind1,ind2
c
      real*8 pt(3),hgt
c
      character*96 tok2
c
      equivalence (tv,ktv,jtv)
c
      integer*2 VPOSITN
      parameter (VPOSITN = 1072)
c
c...Initialize routine
c
      if (ifl(389) .ne. 1) go to 9100
      NVMPPT = 0
      VMPTHK(1) = sc(23)
      VMPTHK(2) = sc(24)
      VMPTHK(3) = 0.
      isc10(1) = ist
      isc10(2) = 1
      iprs   = 0
      dirp   = 0
      diri   = 1
      tv     = 0.
c
c...Check for Slash (/)
c
      ifl(44) = 9
      if (nextyp.ne.5) go to 9000
c
c...Get bottom plane
c
      call parsit
      sca = 0
      tok2 = token2
      isub = ivxsub
      if (scalar) sca = 1
      call prfloc (VMPBPL,3,ierr)
      if (ierr .eq. 0) then
          IVMPFL(8) = 2
          if (sca .eq. 0 .and. tok2(1:1) .ne. '@') then
              IVMPFL(8) = 3
              call expnm2 (tok2,isub,nc,VMPLBB)
          endif
          if (vcpara(VMPBPL,sc(4)) .ne. 1) go to 9070
          call betvec (VMPBPL,sc(4),ang)
          if (ang .gt. 1.) then
              call vctovc (sc(4),VMPBPL)
              VMPBPL(4) = VMPBPL(4) * (-1.)
          endif
      else
          go to 9080
      endif
c
c...Get top plane
c
      call parsit
      sca = 0
      tok2 = token2
      isub = ivxsub
      if (scalar) sca = 1
      call prfloc (VMPTPL,3,ierr)
      if (ierr .eq. 0) then
          IVMPFL(3) = 2
          if (sca .eq. 0 .and. tok2(1:1) .ne. '@') then
              IVMPFL(3) = 3
              call expnm2 (tok2,isub,nc,VMPLBT)
          endif
          if (vcpara(VMPTPL,VMPBPL) .ne. 1) go to 9070
          call betvec (VMPTPL,VMPBPL,ang)
          if (ang .gt. 1.) then
              call vctovc (VMPBPL,VMPTPL)
              VMPBPL(4) = VMPBPL(4) * (-1.)
          endif
      else if (sca .eq. 1) then
          IVMPFL(3) = 1
          iprs   = 1
      else
          go to 9080
      endif
c
c...Get IN,OFFSET direction modifier
c
      if (iprs .eq. 0) call parsit
      iprs = 1
      if (vocab) then
          if (ist .eq. IN) then
              dirp = 0
              diri = 1
          else if (ist .eq. OFFSET) then
              dirp = 3
              diri = 3
          else
              call error (436)
              go to 7000
          endif
          iprs = 0
      endif
c
c...Get perimeter geometry
c
  100 if (iprs .ne. 1) call parsit
      iprs   = 0
      if (.not. (ityp .eq. 2 .and. (geotyp .eq. 14 .or.
     1    geotyp .eq. CURVE .or. geotyp .eq. CIRCLE .or.
     2    geotyp .eq. SURF .or.  geotyp .eq. PATERN))) go to 9010
c
c......Surface
c
      if (geom .and. geotyp .eq. surf) then
          call gtdesc (tv,nclkey,nwds,ietype)
          call stpky1 (nclkey,jtv(1),ktv(4),sc(27),dirp,ifl(2))
          if (ifl(2) .gt. 0) go to 8000
          if (jtv(1) .eq. 0) ktv(4) = 21
      endif
c
c......Store the geometry in the data area
c
      ktv(3) = 1
      RSVASW(1) = tv
      RSVTOK(1) = token2
      RSVSUB(1) = ivxsub
c
c.....Check for open sides if points or curve given
c.......Currently only support composites and points
c
      tityp = ityp
      tgeotp = geotyp
      npts = ktv(1)
c
c...Get the island geometry
c
      iprs   = 0
      do 200 i=2,120,1
          idtype = 0
          call parsit
          if (ityp .eq. 7) go to 210
          if (vocab) then
              iprs   = 1
              go to 210
          endif
c
c......Surface
c
          if (geom .and. geotyp .eq. surf) then
              call gtdesc (tv,nclkey,nwds,ietype)
              call stpkys (nclkey,nwds,sc(27),ifl(2))
              if (ifl(2) .gt. 0) go to 8000
              isc10(3) = nwds
              do 170 j=2,nwds,1
                  if (diri .ne. 3) then
                      call gtpkys (j,jtv(1),ktv(4),ifl(2))
                      if (ifl(2) .gt. 0) go to 8000
                      if (jtv(1) .eq. 0) ktv(4) = 21
                  endif
                  ktv(3) = -1
                  RSVASW(j) = tv
                  RSVTOK(j) = token2
                  RSVSUB(j) = ivxsub
  170         continue
              go to 500
          endif
c
c......All other geometry
c......Store the geometry in the data area
c
          ktv(3) = 1
          RSVASW(i) = tv
          RSVTOK(i) = token2
          RSVSUB(i) = ivxsub
          if (nextyp .eq. 11) then
              isc10(3) = i
              go to 7000
          endif
  200 continue
  210 isc10(3) = i - 1
c
c...Get next token
c
  500 if (iprs .eq. 0) then
          if (nextyp .eq. 11) go to 7000
          call parsit
          if (ityp .eq. 7) go to 7000
      endif
      if (ityp .ne. 1) go to 9030
      istsv  = ist
      iprs   = 0
c
c...VMPOCK/PS,THICK,pthk
c
      if (ist .eq. VPS) then
          call parsit
          if (.not. vocab .or. ist .ne. THICK) go to 9040
          call parsit
          if (.not. scalar) go to 9050
          VMPTHK(1) = tv
c
c...VMPOCK/DS,THICK,pthk
c
      else if (ist .eq. VDS) then
          call parsit
          if (.not. vocab .or. ist .ne. THICK) go to 9040
          call parsit
          if (.not. scalar) go to 9050
          VMPTHK(2) = tv
          call parsit
          if (.not. scalar) then
              iprs = 1
          else
              VMPTHK(3) = tv
          endif
c
c...Define open sides
c
      else if (vocab.and.voc.eq.VOPEN) then
          if (tityp.eq.2.and.(tgeotp.eq.14.or.tgeotp.eq.curve)) then
              tv = rsvasw(1)
              if (tgeotp.eq.curve) then
                 call gtdesc (rsvasw(1), nclkey, nwds, ietype)
                 call cvtype1(nclkey,ietype)
                 if (ietype.ne.5) then
                     call error(12)
                     goto 7000
                 endif
              else
                  nclkey = 0
              endif
              call ncl_genpocket_storegeo(rsvtok(1),nclkey,tgeotp,npts)
c
c.....Set flag for pocket boundary retrieval later
c
              ktv(4) = 22
              rsvasw(1) = tv
c              openfl = 1
c
c.....Get and store open section indices
c
              call parsit
  550         if (ityp.eq.1.and.ist.eq.ALL) then
c...                                                               ALL
                  if (numscl.eq.0) then
                      call ncl_genpocket_setall
                      if (nxteos) goto 7000
                  else
                      call error(182)
                      goto 7000
                  endif
              else if (ityp.eq.3.or.ityp.eq.4) then
                  numscl = numscl + 1
                  ind1 = tv
                  ind2 = tv
                  tinx = inx
                  call parser
                  if (ityp.eq.1.and.ist.eq.THRU) then
c...                                                              THRU
                      call parser
                      if (ityp.eq.3.or.ityp.eq.4) then
                          ind2 = tv
                          call ncl_genpocket_addindex(ind1,ind2)
                          if (nextyp.eq.11) goto 7000
                          tinx = inx
                          call parser
                          if (ityp.eq.1) inx = tinx
                          goto 550
                      else
                          call error(7)
                          goto 7000
                      endif
                  else if (ityp.eq.3.or.ityp.eq.4.or.ityp.eq.7.or.
     x                    ityp.eq.1) then
c...                                         SINGLE COMPOSITE COMPONENT
                      if (tgeotp.eq.8) then
                          call ncl_genpocket_addindex(ind1,ind2)
                          if (ityp.eq.7) goto 7000
                          inx = tinx
                          call parser
                          if (ityp.eq.1) inx = tinx
                      endif
                      goto 550
                  else
                      call error(7)
                      goto 7000
                  endif
              endif
          else
              call error(12)
              goto 7000
          endif
c
c...VMPOCK/POSITN,pt1,...,ptn
c
      else if (ist .eq. VPOSITN) then
          itrflg = 2
  600     call gtpt (itrflg,pt,ierr)
          if (ierr .ne. 0) then
              iprs   = 1
              ierr   = 0
          else
              if (VMPDRL(2) .ne. 0.) then
                  if (VMPDRL(1) .eq. 0.) VMPDRL(1) = sc(28)
                  hgt = (VMPDRL(1)/2.) / tan(VMPDRL(2)/RADIAN)
                  call uvcplvc (pt,sc(4),pt,hgt)
              endif
              call nclf_vm_push_entry(pt)
              go to 600
          endif
c
c...Unrecognized minor word
c
      else
          go to 9040
      endif
      if (nextyp .ne. 11) go to 500
c
c...Normal exit
c
 7000 continue
c
c...End of routine
c
 8000 return
c
c...Slash expected (/)
c
 9000 call error (22)
      go to 8000
c
c...Invalid pocket geometry
c
 9010 call error (177)
      go to 8000
c
c...Error from geo parsing routine
c
 9020 call error (ierr)
      go to 8000
c
c...Vocabulary word expected
c
 9030 call error (232)
      go to 8000
c
c...Unrecognized minor word
c
 9040 call error (436)
      go to 8000
c
c...Number expected
c
 9050 call error (7)
      go to 8000
c
c...Input value out of range
c
 9060 call error (445)
      go to 8000
c
c...Plane is not parallel
c
 9070 call error (330)
      go to 8000
c
c...Plane expected
c
 9080 call error (19)
      go to 8000
c
c...Not authorized to run VoluMill
c
 9100 call error (5)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : vmpprm (kparm,gparm,gpos)
c*      This routine returns the paramters required by the VOLUMILL
c*      routine.
c*
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
c*          kparm(1) = Pocket direction.
c*          kparm(2) = Smooth boundaries flag.
c*          kparm(3) = Number of cutter flutes.
c*          kparm(4) = Entry mode, 1 = ramp, 2 = helix.
c*          kparm(5) = 1 = Perform level passes 1st, 2 = Depth passes.
c*          kparm(6) = Tool number.
c*          kparm(7) = Coolant mode, 0 = OFF, 1 = Flood, 2 = Mist, 3 = Air.
c*          kparm(8) = 1 = Geometry is outside of XY-plane.
c*          kparm(9) = 0 = Side-mill only, 1 = Allow slot cutting.
c*          kparm(10) = 1 = Adjust Feed Rate for Entry.
c*          kparm(11) = 1 = Use contour ramping.
c*          kparm(12) = 1 = Enforce minimum feed rate.
c*          kparm(13) = 1 = Enter off part for Waterline Roughing.
c*          kparm(14) = 0 = Contour only (OMIT stock).
c*          kparm(15) = 1 = Finish island tops.
c*
c*          gparm(1) = Maximum depth of cut.
c*          gparm(2) = Transitional move depth of cut.
c*          gparm(3) = Stepover between passes.
c*          gparm(4) = Stepover between new area passes.
c*          gparm(5) = Minimum boundary radius.
c*          gparm(6) = Minimum radius for final pass corners.
c*          gparm(7) = Minimum angle for corner smoothing.
c*          gparm(8) = PS thick.
c*          gparm(9) = DS thick.
c*          gparm(10) = Ramp/Helix entry angle.
c*          gparm(11) = Helix radius.
c*          gparm(12) = General feedrate.
c*          gparm(13) = Positioning feedrate.
c*          gparm(14) = Entry feedrate.
c*          gparm(15) = Transitional feedrate between pockets.
c*          gparm(16) = General spindle speed.
c*          gparm(17) = Spindle speed during entry.
c*          gparm(18) = Spindle speed during transitional moves.
c*          gparm(19) = Amount to dwell after changing spindle speeds.
c*          gparm(20) = Cutter diameter.
c*          gparm(21) = Cutter corner radius.
c*          gparm(22) = Tolerance.
c*          gparm(23) = Total length of cutter
c*          gparm(24) = Length of cut section of cutter
c*          gparm(25) = Minimum feed rate
c*          gparm(26) = XY rapid feed rate
c*          gparm(27) = Z rapid feed rate
c*          gparm(28) = Offset open side thick
c*          gparm(29) = Predrilled hole diameter.
c*          gparm(30) = Predrilled hole angle.
c*
c*          gpos(1)  = Pocket bottom depth.
c*          gpos(2)  = Pocket top plane Z-level.
c*          gpos(3)  = Clearance Z-level.
c*          gpos(4)  = Rapto Z-level.
c*          gpos(5)  = Distance to raise above floor.
c*          gpos(6)  = Distance to raise between pockets.
c*          gpos(7)  = Pocket bottom Z-level.
c*          gpos(8)  = Step up distance.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine vmpprm (kparm,gparm,gpos)
c
      include 'com8a.com'
      include 'comgt.com'
      include 'cutter.com'
      include 'mocom.com'
      include 'rrdm.com'
      include 'vmpcom.com'
c
      real*8 FEEDR
c
      equivalence (FEEDR ,MOTMAP(24))
c
      integer*4 kparm(*)
c
      real*8 gparm(*),gpos(*)
c
      integer*2 nwds,nstep
c
      real*8 fed,spd,cnv,bpl(4),tpl(4),cpl(4),rnum
c
c...Initialize transformation to XY-plane
c
      nwds   = 4
      call xyplmx (sc(4),VMPMX,IVMPMX)
      if (IVMPMX) then
          kparm(8) = 1
          call invmx (VMPMX,VMPMXI)
c
          bpl(1) = VMPBPL(1)
          bpl(2) = VMPBPL(2)
          bpl(3) = VMPBPL(3)
          bpl(4) = VMPBPL(4)
          call transf (bpl,VMPMX,nwds,PLANE)
c
          if (IVMPFL(3) .ne. 1) then
              tpl(1) = VMPTPL(1)
              tpl(2) = VMPTPL(2)
              tpl(3) = VMPTPL(3)
              tpl(4) = VMPTPL(4)
              call transf (tpl,VMPMX,nwds,PLANE)
          endif
c
          if (IVMPFL(2) .ne. 1) then
              cpl(1) = VMPCLF(1)
              cpl(2) = VMPCLF(2)
              cpl(3) = VMPCLF(3)
              cpl(4) = VMPCLF(4)
              call transf (cpl,VMPMX,nwds,PLANE)
          endif
c
      else
          kparm(8) = 0
          do 50 i=1,4,1
              bpl(i) = VMPBPL(i)
              tpl(i) = VMPTPL(i)
              cpl(i) = VMPCLF(i)
   50     continue
      endif
c
c...Initialize units conversion
c
      cnv = 1.
      if (ifl(264) .eq. 1) cnv = 1. / 25.4
c
c...Initialize parameters
c
      if (ifl(389) .ne. 1) go to 9100
      kparm(1) = IVMPFL(1)
      kparm(2) = IVMPFL(4)
      kparm(3) = IVMPFL(5)
      kparm(4) = IVMPFL(6)
      kparm(5) = IVMPFL(7)
      kparm(6) = CTLNO
      kparm(7) = CCOOL
      kparm(9) = IVMPFL(9)
      kparm(10) = IVMADJ
      kparm(11) = IVMCON
      kparm(12) = IVMMIN
      kparm(13) = OFFPRT
      kparm(14) = WSTOCK
      kparm(15) = WFINIS
c
      gparm(2) = VMPPAS(2) * cnv
      gparm(3) = VMPPAS(3) * cnv
      if (gparm(3) .eq. 0.) gparm(3) = TOOL(6)*2. * cnv
      gparm(4) = VMPPAS(4) * cnv
      if (gparm(4) .eq. 0.) gparm(4) = gparm(3) * cnv
      gparm(5) = VMPPAS(5) * cnv
      gparm(6) = VMPPAS(6) * cnv
      gparm(7) = VMPPAS(7)
      gparm(8) = VMPTHK(1) * cnv
      gparm(9) = VMPTHK(2) * cnv
      gparm(10) = VMPENT(1)
      gparm(11) = VMPENT(2) * cnv
      gparm(20) = sc(28) * cnv
      gparm(21) = sc(29) * cnv
      gparm(22) = sc(27) * cnv
      gparm(25) = VMPMIN * cnv
      gparm(26) = VMPRAP(1) * cnv
      gparm(27) = VMPRAP(2) * cnv
      gparm(28) = VMPTHK(3) * cnv
      call nclf_genpocket_offthk(gparm(28))
      gparm(29) = VMPDRL(1) * cnv
      gparm(30) = VMPDRL(2)
      if (VMPTLN .gt. 0.0) then
          gparm(23) = VMPTLN * cnv
      else
          gparm(23) = tool(3) * cnv
      endif
      if (VMPFLN .gt.0.0) then
          gparm(24) = VMPFLN *cnv
      else
          gparm(24) = gparm(23)
      endif
c
      if (IVMPFL(3) .eq. 1) then
          gpos(1) = VMPTPL(1) * cnv
      else
          gpos(1) = (tpl(4) - bpl(4)) * cnv
      endif
c
      if (IVMPFL(3) .eq. 1) then
          gpos(2) = (bpl(4) + VMPTPL(1)) * cnv
      else
          gpos(2) = tpl(4) * cnv
      endif
c
      if (IVMPFL(2) .eq. 1) then
          gpos(3) = (gpos(2) + VMPCLF(1)) * cnv
      else
          gpos(3) = cpl(4) * cnv
      endif
c
cc      gpos(4) = (gpos(2) + VMPRPT(1)) * cnv
      gpos(4) = VMPRPT(1) * cnv
      gpos(5) = VMPPOS(1) * cnv
      gpos(6) = VMPPOS(2) * cnv
      gpos(7) = bpl(4) * cnv
      gpos(8) = WSTEP * cnv
c
      if (VMPPAS(1) .lt. 0.) then
          gparm(1) = gpos(1) / VMPPAS(1)
      else
          rnum = VMPPAS(1) * cnv
          if (VMPPAS(1) .eq. 0.) rnum = sc(28)/2. * cnv
          nstep  = gpos(1) / rnum
          if (nstep .eq. 0) nstep = 1
          if (IVMPFL(10) .eq. 1 .and. rnum*nstep .lt. gpos(1))
     1        nstep = nstep + 1
          gparm(1) = gpos(1) / nstep
      endif
c
      fed    = FEEDR * cnv
      if (IVMPFD(1) .eq. 1) fed = VMPFED(1) * cnv
      spd    = CRPM
      if (IVMPSP(1) .eq. 1) spd = VMPSPD(1)
      do 100 i=1,4,1
          gparm(i+11) = fed
          if (IVMPFD(i) .eq. 1) then
              gparm(i+11) = VMPFED(i) * cnv
cc          else if (IVMPFD(i) .eq. 2) then
cc              gparm(i+11) = 0.
          else if (IVMPFD(i) .eq. 2) then
              gparm(i+11) = fed * VMPFED(i)
          endif
c
          gparm(i+15) = spd
          if (IVMPSP(i) .eq. 1) then
              gparm(i+15) = VMPSPD(i)
          else if (IVMPSP(i) .eq. 2) then
              gparm(i+15) = spd * VMPSPD(i)
          endif
  100 continue
c
c...End of routine
c
 8000 return
c
c...Not authorized to run VoluMill
c
 9100 call error (5)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : vmppar (kparm,gparm,gpos,cbotpl,ctoppl,cclrpl)
c*      This routine returns the paramters required by the VoluMill
c*      Pocket Modals form.
c*
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
c*          kparm(1) = Pocket direction.
c*          kparm(2) = Smooth boundaries flag.
c*          kparm(3) = Number of cutter flutes.
c*          kparm(4) = Entry mode, 1 = ramp, 2 = helix.
c*          kparm(5) = 1 = Perform level passes 1st, 2 = Depth passes.
c*          kparm(6) = 1 = Top level is distance, 2 = Plane, 3 = Plane label.
c*          kparm(7) = 1 = Clearance level is distance, 2 = Plane,
c*                     3 = Plane label.
c*          kparm(8) = General feedrate flag.
c*          kparm(9) = Positioning feedrate flag.
c*          kparm(10) = Entry feedrate flag.
c*          kparm(11) = Transitional feedrate flag.
c*          kparm(12) = General spindle speed flag.
c*          kparm(13) = Entry spindle speed flag.
c*          kparm(14) = Transition spindle speed flag.
c*          kparm(15) = 2 = Bottom plane is a plane, 3 = plane label.
c*          kparm(16) = 0 = Side-mill only, 1 = Allow slot cutting
c*          kparm(17) = 0 = Depth of cut is DIST, 1 = DEPTH.
c*          kparm(18) = 1 = Adjust Feed Rate for Entry
c*          kparm(19) = 1 = Contour ramping permitted
c*          kparm(20) = 1 = Minimum feed rate enforced
c*
c*          gparm(1)  = Maximum depth of cut.
c*          gparm(2)  = Transitional move depth of cut.
c*          gparm(3)  = Stepover between passes.
c*          gparm(4)  = Stepover between new area passes.
c*          gparm(5)  = Minimum boundary radius.
c*          gparm(6)  = Minimum radius for final pass corners.
c*          gparm(7)  = Minimum angle for corner smoothing.
c*          gparm(8)  = Ramp/Helix entry angle.
c*          gparm(9)  = Helix radius.
c*          gparm(10) = General feedrate.
c*          gparm(11) = Positioning feedrate.
c*          gparm(12) = Entry feedrate.
c*          gparm(13) = Transitional feedrate between pockets.
c*          gparm(14) = General spindle speed.
c*          gparm(15) = Spindle speed during entry.
c*          gparm(16) = Spindle speed during transitional moves.
c*          gparm(17) = Amount to dwell after changing spindle speeds.
c*          gparm(18) = Distance to raise above floor.
c*          gparm(19) = Distance to raise between pockets.
c*          gparm(20) = Length of flutes.
c*          gparm(21) = Length of cutter.
c*          gparm(22) = Minimum feed rate
c*          gparm(23) = XY rapid feed rate
c*          gparm(24) = Z rapid feed rate
c*          gparm(25) = Predrilled hole diameter.
c*          gparm(26) = Predrilled hole angle.
c*
c*          gpos(1:4)   = Pocket bottom plane.
c*          gpos(5:8)   = Pocket top plane or distance(5)
c*          gpos(9:12)  = Clearance plane or distance(9).
c*          gpos(13)    = Rapto Z-level.
c*
c*          cbotpl      = Label of bottom plane.
c*          ctoppl      = Label of top plane.
c*          cclrpl      = Label of clearance plane.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine vmppar (kparm,gparm,gpos,cbotpl,ctoppl,cclrpl)
c
      include 'com8a.com'
      include 'cutter.com'
      include 'comgt.com'
      include 'vmpcom.com'
c
      real*8 FEEDR
c
      equivalence (FEEDR ,MOTMAP(24))
c
      integer*4 kparm(*)
c
      real*8 gparm(*),gpos(*)
c
      byte cbotpl(8),ctoppl(*),cclrpl(*)
c
      real*8 fed,spd
c
c...Initialize parameters
c
      kparm(1)  = IVMPFL(1)
      kparm(2)  = IVMPFL(4)
      kparm(3)  = IVMPFL(5)
      kparm(4)  = IVMPFL(6)
      kparm(5)  = IVMPFL(7)
      kparm(6)  = IVMPFL(3)
      kparm(7)  = IVMPFL(2)
      kparm(15) = IVMPFL(8)
      kparm(16) = IVMPFL(9)
      kparm(17) = IVMPFL(10)
      kparm(18) = IVMADJ
      kparm(19) = IVMCON
      kparm(20) = IVMMIN
c
      gparm(1) = VMPPAS(1)
      gparm(2) = VMPPAS(2)
      gparm(3) = VMPPAS(3)
      gparm(4) = VMPPAS(4)
      gparm(5) = VMPPAS(5)
      gparm(6) = VMPPAS(6)
      gparm(7) = VMPPAS(7)
      gparm(8) = VMPENT(1)
      gparm(9) = VMPENT(2)
      gparm(18) = VMPPOS(1)
      gparm(19) = VMPPOS(2)
      gparm(20) = VMPFLN
      gparm(21) = VMPTLN
      gparm(22) = VMPMIN
      gparm(23) = VMPRAP(1)
      gparm(24) = VMPRAP(2)
      gparm(25) = VMPDRL(1)
      gparm(26) = VMPDRL(2)
c

      fed    = FEEDR
      if (IVMPFD(1) .eq. 1) fed = VMPFED(1)
      spd    = CRPM
      if (IVMPSP(1) .eq. 1) spd = VMPSPD(1)
      do 100 i=1,4,1
          kparm(i+7) = IVMPFD(i)
          gparm(i+9) = fed
          if (IVMPFD(i) .ne. 0) gparm(i+9) = VMPFED(i)
c
          if (i .ne. 4) kparm(i+11) = IVMPSP(i)
          gparm(i+13) = spd
          if (IVMPSP(i) .ne. 0) gparm(i+13) = VMPSPD(i)
c
          gpos(i) = VMPBPL(i)
          gpos(i+4) = VMPTPL(i)
          gpos(i+8) = VMPCLF(i)
  100 continue
      gpos(13) = VMPRPT(1)
c
      call ctob (VMPLBB,cbotpl(1))
      call ctob (VMPLBT,ctoppl(1))
      call ctob (VMPLBC,cclrpl(1))
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vmpsav
c*      This routine saves the current VMPMOD settings.
c*
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
      subroutine vmpsav
c
      include 'com8a.com'
      include 'vmpcom.com'
c
c...Save VMPMOD parameters
c
      do 100 i=1,8,1
          IVMSFL(i) = IVMPFL(i)
  100 continue
c
      do 200 i=1,7,1
          VMSPAS(i) = VMPPAS(i)
  200 continue
c
      VMSENT(1) = VMPENT(1)
      VMSENT(2) = VMPENT(2)
c
      VMSPOS(1) = VMPPOS(1)
      VMSPOS(2) = VMPPOS(2)
c
      IVMSDJ = VMPADJ
      VMSFLN = VMPFLN
      VMSTLN = VMPTLN
      IVMSCN = IVMCON
      IVMMIN = VMPMIN
      VMSRAP(1) = VMPRAP(1)
      VMSRAP(2) = VMPRAP(2)
c
      do 300 i=1,4,1
          IVMSFD(i) = IVMPFD(i)
          VMSFED(i) = VMPFED(i)
          IVMSSP(i) = IVMPSP(i)
          VMSSPD(i) = VMPSPD(i)
          VMSBPL(i) = VMPBPL(i)
          VMSCLF(i) = VMPCLF(i)
          VMSRPT(i) = VMPRPT(i)
          VMSDRL(i) = VMPDRL(i)
  300 continue
c
c...End of routine
c
 8000 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vmprst
c*      This routine restores the current VMPMOD settings.
c*
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
      subroutine vmprst
c
      include 'com8a.com'
      include 'vmpcom.com'
c
c...Save VMPMOD parameters
c
      do 100 i=1,8,1
          IVMPFL(i) = IVMSFL(i)
  100 continue
c
      do 200 i=1,7,1
          VMPPAS(i) = VMSPAS(i)
  200 continue
c
      VMPENT(1) = VMSENT(1)
      VMPENT(2) = VMSENT(2)
c
      VMPPOS(1) = VMSPOS(1)
      VMPPOS(2) = VMSPOS(2)
c
      VMPADJ = IVMSDJ
      VMPFLN = VMSFLN
      VMPTLN = VMPSLN
      IVMCON = IVMSCN
      VMPRAP(1) = VMSRAP(1)
      VMPRAP(2) = VMPRAP(2)
c
      do 300 i=1,4,1
          IVMPFD(i) = IVMSFD(i)
          VMPFED(i) = VMSFED(i)
          IVMPSP(i) = IVMSSP(i)
          VMPSPD(i) = VMSSPD(i)
          VMPBPL(i) = VMSBPL(i)
          VMPCLF(i) = VMSCLF(i)
          VMPRPT(i) = VMSRPT(i)
          VMPDRL(i) = VMSDRL(i)
  300 continue
c
c...End of routine
c
 8000 return
      end
