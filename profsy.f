

C*********************************************************************
C*    NAME         :  profsy.f
C*       CONTAINS:
C*           profsy  prfloc
C*
C*    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       profsy.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       07/28/15 , 11:11:17
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine profsy
c*      This routine handles the parsing and syntax checking for
c*      the PROFIL command.  It builds the syscom in preparation for
c*      the motion generation section in the following format:
c*
c*         isc10(1) = ist of PROFIL vocabulary word (761).
c*
c*         IPRFFL(1) = 1 = DS passes given as number of passes.
c*                     2 = Given as part thickness.
c*         IPRFFL(2) = 1 = DS step given as number of passes.
c*                     2 = Given as (maximum) step.
c*         IPRFFL(3) = DS condition, 1 = ON, 2 = TO.
c*         IPRFFL(4) = 0 = PS is not defined.  1 = PS is a plane. 2 = Surf.
c*         IPRFFL(5) = 1 = Profile direction is CLW, 2 = CCLW, 3 = Vector.
c*         IPRFFL(6) = 1 = Top plane given as number of passes.
c*                     2 = Given as a plane, 3 = distance.
c*         IPRFFL(7) = 1 = Clearance plane is a plane, 2 = absolute distance,
c*                     3 = incremental distance.
c*         IPRFFL(8) = 0 = Fixed tool axis, 1 = Normal to PS,
c*                     2 = Normal to secondary PS.
c*         IPRFFL(9) = 1 = STEP is number of passes.
c*                     2 = STEP is (maximum) depth.
c*         IPRFFL(10)= 1 = Perform same level passes first.
c*                     2 = Perform depth passes first.
c*         IPRFFL(11)= Entry method.  0 = Omit, 1 = Arc, 2 = Ramp,
c*                     3 = Comb(Ramp+Arc)
c*         IPRFFL(12)= 0 = No retraction, 1 = Retract to clearance plane/dis,
c*                     2 = Given as a plane, 3 = absolute distance,
c*                     4 = incremental distance.
c*         IPRFFL(13)= Exit method.  0 = Omit, 1 = Arc, 2 = Ramp,
c*                     3 = Comb(Arc+Ramp)
c*         IPRFFL(14)= Ending point type.  1 = End point of curve, 2 = User
c*                     specified.
c*         IPRFFL(15)= START, 0 = No END, 1 = END
c*         IPRFFL(16)= MAXDP option.  2 = OFF, 1 = ON
c*         IPRFFL(17)= 0 = Starting point determines curve offset direction,
c*                     -1 = Right, 1 = Left.
c*         IPRFFL(18)= 0 = Leave tool down on loop transition moves.(DOWN)
c*                     1 = Perform depth style exit and entry during transition 
c*                     moves.(UP) 2 = Disable the automatic retract (OFF)
c*         IPRFFL(19)= 0 = No rapto distance, 1 = Rapto plane is a plane,
c*                     2 = absolute distance, 3 = incremental distance.
c*         IPRFFL(20)= CUTCOM option. 0 = ALL, 1 = START
c*         IPRFFL(21)= Apply Tilt Angle to beginning of profile.
c*                     1 = Distance, 2 = Percentage, 3 = ATANGL for entire move
c*         IPRFFL(22)= Apply Tilt Angle to end of profile.
c*                     1 = Distance, 2 = Percentage
c*
c*         PRFTV(1)  = Drive surface.
c*         PRFTV(2)  = Part surface when surface or canted plane is specified.
c*         PRFTV(3)  = Surface for controlling normal tool axis.
c*
c*         PRFPAS(1) = Thickness of DS passes.
c*         PRFPAS(2) = Number of passes.
c*         PRFPAS(3) = (Maximum) stepover distance.
c*
c*         PRFDEP(1) = Thickness of PS depth.
c*         PRFDEP(2) = Number of levels.
c*         PRFDEP(3) = (Maximum) depth distance.
c*
c*         PRFSPT    = Starting point on curve.
c*         PRFIVC    = Initial direction.
c*         PRFNVC    = Offset plane vector.
c*         PRFEPT    = Ending point on curve.
c*
c*         PRFTHK(1) = DS final thick.
c*         PRFTHK(2) = PS final thick.
c*
c*         PRFENT(1) = Arc radius
c*         PRFENT(2) = Arc Entry rise value.
c*         PRFENT(3) = Ramp distance.
c*         PRFENT(4) = Ramp angle.
c*         PRFENT(5) = Ramp Entry rise value.
c*         PRFENT(6) = CUTCOM Positional Distance.
c*
c*         PRFEXI(1) = Arc radius.
c*         PRFEXI(2) = Arc Exit rise value.
c*         PRFEXI(3) = Ramp distance.
c*         PRFEXI(4) = Ramp angle.
c*         PRFEXI(5) = Ramp Exit rise value.
c*         PRFEXI(6) = Retract distance between depths.
c*
c*         PRFLAP(1) = Start of curve overlap.
c*         PRFLAP(2) = End of curve overlap.
c*
c*         PRFANG    = Fillet angle setting.
c*
c*         PRFPLN    = Part surface (or lowest level) plane.
c*         PRFTPL    = Top Plane.
c*         PRFTPL(1) = Top distance.
c*         PRFCLF    = Clearance Plane.
c*         PRFCLF(1) = Clearance distance.
c*         PRFTHK(4) = Rapto parameter.
c*         PRFRPL    = Retract plane.
c*         PRFRPL(1) = Retract distance.
c*         PRFPPL    = Rapto plane.
c*         PRFPPL(1) = Rapto distance.
c*
c*         PRFMXD    = Maximum step size.
c*
c*         PRFTLT(1) = Tilt distance at beginning of move.
c*         PRFTLT(2) = Tilt distance at end of move.
c*         PRFTLT(3) = Tilt angle at beginning of move.
c*         PRFTLT(4) = Tilt angle at end of move.
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
      subroutine profsy
c
      include 'com8a.com'
      include 'prfcom.com'
      include 'rrdm.com'
c
      !integer*2 ktv(4)
      !integer*4 jtv(2)
      !equivalence (tv,jtv,ktv)
c
      integer*2 ierr,i,ie
      integer*4 iacy6,jerr,icvtyp,inum
c
      logical lpas,lsca
c
      data iacy6 /6/
c
      integer*2 PASS,ENDPT,NEARPT,START,THICK,NUM,OFFSET,PS,CLW,CCLW,
     1          RAPTO,INCR,DIST,NORMAL,SAME,CLRSRF,STEP,DEPTH,LEVEL,
     2          OMIT,ARC,ATANGL,RETRCT,RTRCTO,DEEP,CS,PAST,FILLET,
     3          FEDRAT,RAPID,LEFT,RIGHT,AUTO,UP,DOWN,OFF,BEGIN,VOCEND,
     4          AT,OUT,PERCNT
c
      character*80 tcutcm,tempc
      integer*4 strlen1
      integer*2 li,lj,is1,is4
      integer*2 itemp(4),tctcm1,tctcm2,tjctcm
      real*8 temp
      equivalence (itemp,temp)
      
      parameter (ARC    =  182)
      parameter (AT     =  189)
      parameter (ATANGL =    1)
      parameter (AUTO   =   88)
      parameter (CCLW   =   59)
      parameter (CLRSRF = 1057)
      parameter (CLW    =   60)
      parameter (CS     =  753)
      parameter (DEEP   =  153)
      parameter (DEPTH  =  510)
      parameter (DIST   =  575)
      parameter (DOWN   =  113)
      parameter (ENDPT  =  664)
      parameter (FEDRAT = 1009)
      parameter (FILLET =  402)
      parameter (INCR   =   66)
      parameter (LEFT   =    8)
      parameter (LEVEL  =  759)
      parameter (MAXDP  =  736)
      parameter (NEARPT =  866)
      parameter (NORMAL =  820)
      parameter (NUM    =  563)
      parameter (OFF    =   72)
      parameter (OFFSET =  666)
      parameter (OMIT   =  172)
      parameter (ON     =   71)
      parameter (OUT    =  653)
      parameter (PASS   =  755)
      parameter (PAST   =  715)
      parameter (PERCNT =  763)
      parameter (PS     =  728)
      parameter (RAPID  =    5)
      parameter (RAPTO  =  280)
      parameter (RETRCT =    7)
      parameter (RIGHT  =   24)
      parameter (RTRCTO =  295)
      parameter (SAME   =  730)
      parameter (START  =   57)
      parameter (STEP   =   92)
      parameter (THICK  =  717)
      parameter (UP     =  112)
      parameter (CUTCOM = 1007)
      parameter (XYPLAN = 33, YZPLAN = 37, ZXPLAN = 41)
      parameter (ALL = 816)
      parameter (NOMORE = 53)
      parameter (VOCEND =  499)

c
c...Check for Slash (/)
c
      if (nextyp.ne.5) go to 9000
c
c...Initialize routine
c
      isc10(1) = ist
c
      IPRFFL(1) = 1
      IPRFFL(2) = 1
      IPRFFL(3) = 1
      IPRFFL(4) = 0
      IPRFFL(5) = 3
      IPRFFL(6) = 1
      IPRFFL(7) = 1
      IPRFFL(8) = 0
      IPRFFL(9) = 1
      IPRFFL(10) = 1
      IPRFFL(11) = 0
      IPRFFL(12) = 0
      IPRFFL(13) = 0
      IPRFFL(14) = 1
      IPRFFL(15) = 0
      IPRFFL(16) = 2
      IPRFFL(17) = 0
      IPRFFL(18) = 0
      IPRFFL(19) = 0
      IPRFFL(19) = 0
      IPRFFL(20) = 0
      IPRFFL(21) = 0
      IPRFFL(22) = 0
c
      call vctovc (sc(1),PRFSPT)
      call vctovc (sc(4),PRFNVC)
      call vctovc (sc(7),PRFIVC)
c
      call plnvpt (sc(4),sc(1),PRFCLF,jerr)
c
      PRFANG = 0.
c
      PRFPAS(1) = 0.
      PRFPAS(2) = 1.
      PRFPAS(3) = sc(28) - sc(29)*2.
c
      PRFDEP(1) = 0.
      PRFDEP(2) = 1.
      PRFDEP(3) = 0.
c
      PRFENT(1) = 0.
      PRFENT(2) = 0.
      PRFENT(3) = 0.
      PRFENT(4) = 0.
      PRFENT(5) = 0.
      PRFENT(6) = 0.
c
      PRFEXI(1) = 0.
      PRFEXI(2) = 0.
      PRFEXI(3) = 0.
      PRFEXI(4) = 0.
      PRFEXI(5) = 0.
      PRFEXI(6) = 0.
c
      PRFTHK(1) = 0.
      PRFTHK(2) = 0.
c
      PRFLAP(1) = 0.
      PRFLAP(2) = 0.
c
      PRFTPL(1) = 0.
      PRFRPL(1) = 0.
      PRFPPL(1) = 0.
c
      PRFTV(2) = 0.
c
      PRFMXD = 0.
c
      do 100 i=1,10,1
          IPRFFD(i) = 0
          PRFFED(i) = 0.
  100 continue
      IPRFFD(2) = 2
      IPRFFD(3) = 2
c
      PRFTLT(1) = 0.
      PRFTLT(2) = 0.
      PRFTLT(3) = 0.
      PRFTLT(4) = 0.
c
      tjctcm = 0
      li = 0
      lj = 0
           
c
c...PROFIL/[ON ,] ds [,thick] [,AUTO ] [,PAST]
c...        OFF                 LEFT
c...                            RIGHT
c
      ifl(44) = 9
      call parsit
c
c......PROFIL/ON-OFF
c
      if (ityp .eq. 1) then
          if (ist .eq. ON) then
              IPRFFL(3) = 1
          else if (ist .eq. OFF) then
              IPRFFL(3) = 2
          else
              go to 9070
          endif
          call parsit
      endif
c
c......PROFIL/ds
c
      if (.not. geom .or. (geotyp .ne. CURVE .and. 
     1          geotyp.ne.LINE .and.geotyp.ne.CIRCLE .and.
     2          geotyp .ne. VANOTE)) go to 9010
      PRFTV(1) = tv
      icvtyp = 1
      if (geotyp .eq. VANOTE) then
          if (IPRFFL(3) .eq. 2) go to 9110
          call uaf_get_txt_defstyle (inum)
          if (inum .eq. 0) go to 9110
          icvtyp = 0
          IPRFFL(8) = 1
      endif
c
      if (nextyp .eq. 11) go to 7000
      call parsit
c
c......PROFIL/ds,thick
c
      if (scalar) then
          if (icvtyp .eq. 0) go to 9110
          PRFTHK(1) = tv
          call parsit
      endif
c
c......PROFIL/ds,AUTO
c
      if (vocab .and. ist .eq. AUTO) then
          if (icvtyp .eq. 0) go to 9110
          IPRFFL(17) = 0
          call parsit
c
c......PROFIL/ds,LEFT
c
      else if (vocab .and. ist .eq. LEFT) then
          if (icvtyp .eq. 0) go to 9110
          IPRFFL(17) = 1
          call parsit
c
c......PROFIL/ds,RIGHT
c
      else if (vocab .and. ist .eq. RIGHT) then
          if (icvtyp .eq. 0) go to 9110
          IPRFFL(17) = -1
          call parsit
      endif
c
c......PROFIL/ds,PAST
c
      if (vocab .and. ist .eq. PAST) then
          if (icvtyp .eq. 0) go to 9110
          call parsit
          if (.not. scalar) go to 9050
          PRFLAP(1) = tv
          call parsit
          if (.not. scalar) then
              PRFLAP(2) = PRFLAP(1)
          else
              PRFLAP(2) = tv
              call parsit
          endif
      endif
      go to 550
c
c...Parse other parameters
c
  500 if (nextyp .eq. 11) go to 7000
      call parsit
  550 if (ityp .eq. 7) go to 7000
      if (ityp .ne. 1) go to 9030
c
c...START
c
      if (ist .eq. START) then
          if (icvtyp .eq. 0) go to 9110
  600     if (nextyp .eq. 11) go to 7000
          call parsit
  650     if (ityp .eq. 7) go to 7000
c
c......START,vec
c
          if (ityp .ne. 1) then
              call prfloc (PRFIVC,2,ierr)
              if (ierr .ne. 0) go to 9020
              IPRFFL(5) = 3
c
c......START,ENDPT
c
          else if (ist .eq. ENDPT) then
              call vctovc (SC(1),PRFSPT)
c
c......START,NEARPT
c
          else if (ist .eq. NEARPT) then
              call parsit
              call prfloc (PRFSPT,1,ierr)
              if (ierr .ne. 0) go to 9020
c
c......START,END
c
          else if (ist .eq. VOCEND) then
              IPRFFL(15) = 1
c
c......START,CLW
c
          else if (ist .eq. CLW) then
              IPRFFL(5) = 1
c
c......START,CCLW
c
          else if (ist .eq. CCLW) then
              IPRFFL(5) = 2
c
c......START,OMIT
c
          else if (ist .eq. OMIT) then
              IPRFFL(11) = 0
c
c......START,ARC
c
          else if (ist .eq. ARC) then
              if (IPRFFL(11) .eq. 2) then
                 IPRFFL(11) = 3
              else
                 IPRFFL(11) = 1
              endif
              call parsit
              if (.not. scalar) go to 9050
              if (tv .le. 0.) go to 9060
              PRFENT(1) = tv
              call parsit
              if (scalar) then
                  PRFENT(2) = tv
              else
                  go to 650
              endif
c
c......START,ATANGL
c
          else if (ist .eq. ATANGL) then
              IPRFFL(11) = 2
              call parsit
              if (.not. scalar) go to 9050
              PRFENT(3) = tv
              call parsit
              if (.not. scalar) go to 9050
              PRFENT(4) = tv
              call parsit
              if (scalar) then
                  PRFENT(5) = tv
              else
                  go to 650
              endif                           
          else
              go to 550
          endif
          go to 600
c
c...OFFSET
c
      else if (ist .eq. OFFSET) then
          if (icvtyp .eq. 0) go to 9110
          lpas   = .false.
  700     if (nextyp .eq. 11) go to 7000
          call parsit
  750     if (ityp .eq. 7) go to 7000
c
c......OFFSET,thick
c
          if (scalar .and. .not. lpas) then
              if (tv .lt. 0.) go to 9060
              lpas   = .true.
              IPRFFL(1) = 2
              PRFPAS(1) = tv
c
c......OFFSET,PASS
c
          else if (ityp .eq. 1 .and. ist .eq. PASS) then
              call parsit
              if (.not. scalar) go to 9050
              lpas   = .true.
              PRFPAS(2) = tv
              IPRFFL(2) = 1
c
c......OFFSET,DOWN
c
          else if (ityp .eq. 1 .and. IST .eq. DOWN) then
              IPRFFL(18) = 0
c
c......OFFSET,UP
c
          else if (ityp .eq. 1 .and. IST .eq. UP) then
              IPRFFL(18) = 1
c
c......OFFSET,OFF
c
          else if (ityp .eq. 1 .and. IST .eq. OFF) then
              IPRFFL(18) = 2
c
c......OFFSET,max-step
c
          else if (scalar) then
              PRFPAS(3) = tv
              IPRFFL(2) = 2
          else
              go to 550
          endif
          go to 700
c
c...PS
c
      else if (ist .eq. PS) then
          lpas   = .false.
          lsca   = .false.
  800     if (nextyp .eq. 11) go to 7000
          call parsit
  850     if (ityp .eq. 7) go to 7000
c
c......PS,plane-sf / top-pl
c
          if (geom .and. (geotyp .eq. PLANE .or. geotyp .eq. SURF)) then
              if (icvtyp .eq. 0) go to 9110
              IPRFFL(4) = 1
              call prfloc (PRFPLN,3,ierr)
              if (ierr .ne. 0) then
                 if (geotyp .eq. PLANE) then
                      go to 9020
c
c.........PS,sf
c
                  else if (geotyp .eq. SURF) then
                      if (lpas) go to 9090
                      PRFTV(2) = tv
                      IPRFFL(4) = 2
                  endif
              endif
c
c.........Determine if plane is parallel
c.........to tool axis
c
              if (IPRFFL(4) .eq. 1) then
                  lpas   = .true.
                  call betvec (PRFPLN,sc(4),ang)
                  call dpoint (ang,ang,iacy6)
                  PRFTV(2) = tv
                  if (ang .eq. 0.) then
                      IPRFFL(4) = 1
                  else if (ang .eq. 180.) then
                      call vctmsc (PRFPLN,PRFPLN,-1.d0)
                      PRFPLN(4) = PRFPLN(4) * (-1.)
                      IPRFFL(4) = 1
                  else
                      IPRFFL(4) = 2
                  endif
              endif
c
c......PS,thick
c
          else if (scalar) then
              PRFTHK(2) = tv
              if (icvtyp .eq. 0) PRFTHK(2) = -tv
              lsca   = .true.
c
c......PS,NORMAL [,psn]
c
          else if (ityp .eq. 1 .and. ist .eq. NORMAL) then
              if (icvtyp .eq. 0) go to 9110
              IPRFFL(8) = 1
              call parsit
              if (geom) then
                  if (geotyp .eq. SURF .or. geotyp .eq. PLANE) then
                      if (IPRFFL(4) .eq. 1) IPRFFL(4) = 2
                      IPRFFL(8) = 2
                      PRFTV(3) = tv
                  else
                      go to 9100
                  endif
                  call parsit
              else
                  PRFTV(3) = PRFTV(1)
              endif
c
c.........PS,NORMAL,OUT
c
              if (ityp .eq. 1 .and. ist .eq. OUT) then
                  IPRFFL(21) = 1
                  call parsit
                  if (ityp .eq. 1 .and.
     1                  (ist .eq. DIST .or. ist .eq. PERCNT)) then
                      if (ist .eq. PERCNT) IPRFFL(21) = 2
                      call parsit
                  endif
c
                  if (scalar) then
                      PRFTLT(1) = tv
                      call parsit
                      if (scalar) then
                          PRFTLT(3) = tv
                          call parsit
                      else
                          go to 9050
                      endif
                  else
                      go to 9050
                  endif
              endif
c
c.........PS,NORMAL,AT
c
              if (ityp .eq. 1 .and. ist .eq. AT) then
                  IPRFFL(22) = 1
                  call parsit
                  if (ityp .eq. 1 .and.
     1                  (ist .eq. DIST .or. ist .eq. PERCNT)) then
                      if (ist .eq. PERCNT) IPRFFL(22) = 2
                      call parsit
                  endif
c
                  if (scalar) then
                      PRFTLT(2) = tv
                      call parsit
                      if (scalar) then
                          PRFTLT(4) = tv
                          call parsit
                      else
                          go to 9050
                      endif
                  else
                      go to 9050
                  endif
              endif
              go to 850
c
c......PS,ATANGL [,ang]
c
          else if (ityp .eq. 1 .and. ist .eq. ATANGL) then
              if (icvtyp .eq. 0) go to 9110
              IPRFFL(8) = 1
              IPRFFL(21) = 3
              call parsit
              if (scalar) then
                  PRFTLT(3) = tv
                  call parsit
                  if (geom) then
                      if (geotyp .eq. SURF .or. geotyp .eq. PLANE) then
                          if (IPRFFL(4) .eq. 1) IPRFFL(4) = 2
                          IPRFFL(8) = 2
                          PRFTV(3) = tv
                      else
                          go to 9100
                      endif
                  else
                      PRFTV(3) = PRFTV(1)
                      go to 850
                  endif
              else
                  go to 9050
              endif
c
c......PS,SAME
c
          else if (ityp .eq. 1 .and. ist .eq. SAME) then
              if (icvtyp .eq. 0) go to 9110
              IPRFFL(8) = 0
          else
              go to 550
          endif
          go to 800
c
c...RAPTO
c
      else if (ist .eq. RAPTO) then
          call parsit
c
c......RAPTO,plane
c
          if (geom) then
              call prfloc (PRFPPL,3,ierr)
              if (ierr .ne. 0) go to 9020
              IPRFFL(19) = 1
          else
c
c......RAPTO,DIST
c
              if (ityp .eq. 1 .and. ist .eq. DIST) then
                  IPRFFL(19) = 2
                  call parsit
c
c......RAPTO,INCR
c
              else if (ityp .eq. 1 .and. ist .eq. INCR) then
                  IPRFFL(19) = 3
                  call parsit
              endif
c
c......RAPTO,distance
c
              if (scalar) then
                  if (IPRFFL(19) .eq. 0) IPRFFL(19) = 2
                  PRFPPL(1) = tv
                  PRFPPL(5) = tv
              endif
          endif
          go to 500
c
c...CLRSRF
c
      else if (ist .eq. CLRSRF) then
          call parsit
c
c......CLRSRF,plane
c
          if (geom) then
              call prfloc (PRFCLF,3,ierr)
              if (ierr .ne. 0) go to 9020
              IPRFFL(7) = 1
          else
c
c......CLRSRF,DIST
c
              if (ityp .eq. 1 .and. ist .eq. DIST) then
                  IPRFFL(7) = 2
                  call parsit
c
c......CLRSRF,INCR
c
              else if (ityp .eq. 1 .and. ist .eq. INCR) then
                  IPRFFL(7) = 3
                  call parsit
              endif
c
c......CLRSRF,distance
c
              if (scalar) then
                  if (IPRFFL(7) .eq. 1) IPRFFL(7) = 2
                  PRFCLF(1) = tv
                  PRFCLF(5) = tv
              endif
          endif
          go to 500
c
c...STEP
c
      else if (ist .eq. STEP) then
          lpas   = .false.
 1100     if (nextyp .eq. 11) go to 7000
          call parsit
 1150     if (ityp .eq. 7) go to 7000
c
c......STEP,thick
c
          if (scalar .and. .not. lpas) then
              if (tv .lt. 0.) go to 9060
              lpas   = .true.
              IPRFFL(6) = 3
              PRFDEP(1) = tv
c
c......STEP,plane
c
          else if (geom) then
              call prfloc (PRFTPL,3,ierr)
              if (ierr .ne. 0) go to 9020
              IPRFFL(6) = 2
              lpas   = .true.
c
c......STEP,PASS
c
          else if (ityp .eq. 1 .and. ist .eq. PASS) then
              call parsit
              if (.not. scalar) go to 9050
              lpas   = .true.
              PRFDEP(2) = tv
              IPRFFL(9) = 1
c
c......STEP,max-step
c
          else if (scalar) then
              PRFDEP(3) = tv
              IPRFFL(9) = 2
c
c......STEP,LEVEL
c
          else if (ityp .eq. 1 .and. ist .eq. LEVEL) then
              IPRFFL(10) = 1
c
c......STEP,DEEP
c
          else if (ityp .eq. 1 .and. ist .eq. DEEP) then
              IPRFFL(10) = 2
          else
              go to 550
          endif
          go to 1100
c
c...RETRCT
c
      else if (ist .eq. RETRCT) then
 1200     if (nextyp .eq. 11) go to 7000
          call parsit
 1250     if (ityp .eq. 7) go to 7000
c
c......RETRCT,OFF
c
          if (vocab .and. ist .eq. OFF) then
              IPRFFL(12) = 0
c
c......RETRCT,ON
c
          else if (vocab .and. ist .eq. ON) then
              IPRFFL(12) = 1
c
c......RETRCT,DIST,dis
c
          else if (vocab .and. ist .eq. DIST) then
              IPRFFL(12) = 3
              call parsit
              if (.not. scalar) go to 9050
              PRFRPL(1) = tv
              
c
c......RETRCT,INCR,dis
c
          else if (vocab .and. ist .eq. INCR) then
              IPRFFL(12) = 4
              call parsit
              if (.not. scalar) go to 9050
              PRFRPL(1) = tv
c
c......RETRCT,dis
c
          else if (scalar) then
              if (tv .lt. 0.) go to 9060
              IPRFFL(12) = 3
              PRFRPL(1) = tv
              PRFRPL(5) = tv
c
c......RETRCT,plane
c
          else if (geom) then
              call prfloc (PRFRPL,3,ierr)
              if (ierr .ne. 0) go to 9020
              IPRFFL(12) = 2
c
c......RETRCT,RTRCTO
c
          else if (vocab .and. ist .eq. RTRCTO) then
              call parsit
              if (tv .lt. 0.) go to 9060
              PRFEXI(6) = tv
c
c......RETRCT,OMIT
c
          else if (vocab .and. ist .eq. OMIT) then
              IPRFFL(13) = 0
c
c......RETRCT,ARC
c
          else if (vocab .and. ist .eq. ARC) then
              if (IPRFFL(13) .eq. 2) then
                  IPRFFL(13) = 3
              else
                  IPRFFL(13) = 1            
              endif
              call parsit
              if (.not. scalar) go to 9050
              if (tv .le. 0.) go to 9060
              PRFEXI(1) = tv
              call parsit
              if (scalar) then
                  PRFEXI(2) = tv
              else
                  go to 1250
              endif
c
c......RETRCT,ATANGL
c
          else if (vocab .and. ist .eq. ATANGL) then
              IPRFFL(13) = 2
              call parsit
              if (.not. scalar) go to 9050
              PRFEXI(3) = tv
              call parsit
              if (.not. scalar) go to 9050
              PRFEXI(4) = tv
              call parsit
              if (scalar) then
                  PRFEXI(5) = tv
              else
                  go to 1250
              endif
          else
              go to 550
          endif
          go to 1200
c
c...CS
c
      else if (ist .eq. CS) then
          if (icvtyp .eq. 0) go to 9110
 1300     if (nextyp .eq. 11) go to 7000
          call parsit
 1350     if (ityp .eq. 7) go to 7000
c
c......CS,ENDPT
c
          if (ist .eq. ENDPT) then
              IPRFFL(14) = 1
c
c......CS,NEARPT
c
          else if (ist .eq. NEARPT) then
              call parsit
              call prfloc (PRFEPT,1,ierr)
              if (ierr .ne. 0) go to 9020
              IPRFFL(14) = 2
          else
              go to 550
          endif
          go to 1300
c
c...FILLET,ang
c
      else if (ist .eq. FILLET) then
          if (icvtyp .eq. 0) go to 9110
          call parsit
          if (.not. scalar) go to 9050
          if (tv .lt. 0.) go to 9060
          PRFANG = tv
          go to 500
c
c...FEDRAT,feeds
c
      else if (ist .eq. FEDRAT) then
          ie = 6
          if (icvtyp .eq. 0) ie = 4
          do 1500 i=1,ie,1
              call parsit
              if (vocab .and. ist .eq. RAPID) then
                  IPRFFD(i) = 2
              else if (scalar) then
                  if (tv .eq. 0.) then
                      IPRFFD(i) = 0
                  else if (tv .lt. 0.) then
                      IPRFFD(i) = 3
                      PRFFED(i) = -tv
                  else
                      IPRFFD(i) = 1
                      PRFFED(i) = tv
                  endif
              else
                  go to 9050
              endif
 1500     continue
          go to 500
c
c...CUTCOM
c
      else if (vocab .and. voc. eq. CUTCOM) then
          tjctcm = 1
          tctcm1 = 0
          tctcm2 = 0
          tcutcm = ' '
          call stclix(is1,is4)
          itemp(is4) = OFF
          prrct(15) = temp
          do i = 1,15
 1600       if (nextyp .eq. 11) goto 7000
            call parsit
            if (vocab .and. voc.eq.NOMORE) goto 500
            if (i.eq.1 .and. vocab .and. 
     *          (ist.eq.left .or. ist.eq.right.or. ist.eq.on)) then
              tctcm1 = ist
            else if ((i.eq.1 .or. i.eq.2) .and. vocab .and. 
     *               (ist.eq.xyplan.or. ist.eq.yzplan .or.
     *                ist.eq.zxplan)) then
              tctcm2 = ist
            else if (vocab .and. ist.eq.OFFSET) then    
               call parsit
               if (scalar) then
                 PRFENT(6) = tv
               else 
                 call error (4)
                 goto 9040
               endif
               goto 1600
            else if (vocab .and. (ist.eq.ALL .or. ist.eq.START)) then
                if (ist.eq.ALL) then
                  IPRFFL(20) = 0
                else
                  IPRFFL(20) = 1
                endif
                goto 1600
            else
              lj = li + strlen1(token2)
              if (li .eq. 1) then
                tempc = token2
                lj = lj - 1
              else
                tempc = ',' // token2
              endif           
              tcutcm(li:lj) = tempc
              li = lj + 1
            endif
            if (vocab) then
              itemp(is4) = ist
              prrct(tjctcm) = temp
            else if (scalar) then
              prrct(tjctcm) = tv
            else
              call error (4)
              goto 9040
            endif
            tjctcm = tjctcm + 1
          enddo
c
c...MAXDP,maxstp
c
      else if (ist .eq. MAXDP) then
        call parsit
        if (vocab) then
            if (ist .eq. OFF) then
                IPRFFL(16) = 2
            else
                goto 9070
            endif
        else if (scalar) then
            if (tv .eq. 0) then
                IPRFFL(16) = 2
            else if (tv .gt. 0) then
                IPRFFL(16) = 1
            PRFMXD = tv;
            else
                goto 9060
            endif 
        else
            goto 9070
        endif
c
c...Unrecognized minor word
c
      else
          go to 9040
      endif
c
c...Normal exit
c
 7000 continue
 
c*******************************************************
c    All syntax is correct, load CUTCOM parameter values
c******************************************************* 
      iptcom = tjctcm
      if (iptcom .gt. 1) then
        jctcm1 = tctcm1
        jctcm2 = tctcm2
        cutcm  = tcutcm
      endif
c
c...End of routine
c

 8000 call prfsavepar(PRFPPL(5), PRFRPL(5), PRFCLF(5))
      return
c
c...Slash expected (/)
c
 9000 call error (22)
      go to 8000
c
c...Curve expected
c
 9010 call error (21)
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
 9040 call error (182)
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
c...ON or OFF expected
c
 9070 call error (56)
      go to 8000
c
c...Invalid Part Surface
c
 9080 call error (36)
      go to 8000
c
c...Plane expected
c
 9090 call error (19)
      go to 8000
c
c...Plane or Surf expected
c
 9100 call error (326)
      go to 8000
c
c...Not valid when engraving annotation
c
 9110 call error (528)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine prfloc (gpt,kfl,kerr)
C*      This routine parses a point or vector in a command.  The first
C*      parameter of the point/vector should already be parsed via
C*      'parsit'.
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          kfl    I*2   D1   1 = Parse Point, 2 = Vector, 3 = Plane.
C*       OUTPUT :  
C*          gpt    R*8   D3   Parsed point, vector, or plane data.
C*          kerr   I*2   D1   Returns non-zero on error.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine prfloc (gpt,kfl,kerr)
c
      include 'com8a.com'
c
      integer*2 kfl,kerr
c
      real*8 gpt(4)
c
      integer*2 ietype,nwds
      integer*4 nclkey
c
      logical trflg
c
      real*8 ptx(6),ang
c
c...Initialize routine
c
      kerr   = 0
      trflg  = .true.
c
c...Point
c
      if (geom .and. geotyp .eq. POINT) then
          if (kfl .ne. 1) then
              go to 9000
          endif
          call gtentt (tv,trflg,nclkey,ietype,gpt)
c
c...Vector
c
      else if (geom .and. geotyp .eq. VECTOR) then
          if (kfl .ne. 2) go to 9010
          call gtentt (tv,trflg,nclkey,ietype,gpt)
c
c...Point-Vector
c
      else if (geom .and. geotyp .eq. PNTVEC) then
          call gtentt (tv,trflg,nclkey,ietype,ptx)
          if (kfl .eq. 1) then
              call vctovc (ptx(1),gpt)
          else if (kfl .eq. 2) then
              call vctovc (ptx(4),gpt)
          else
              go to 9030
          endif
c
c...Plane
c
      else if (geom .and. (geotyp .eq. PLANE .or. geotyp .eq. SURF))
     1      then
          if (kfl .eq. 1) then
              go to 9000
              
          else if (kfl .eq. 2) then
              go to 9010
          endif
c
c......Planar surface
c
          if (geotyp .eq. SURF) then
              call gtdesc (tv,nclkey,nwds,ietype)
              call ncl_get_sf_primtyp(nclkey,ietype)
              if (ietype .eq. 3) then
                  call gtplt (tv,ifl(72),gpt)
              else
                  go to 9030
              endif
c
c......Plane
c
          else
              call gtentt (tv,trflg,nclkey,ietype,gpt)
          endif
c
c......Make sure plane points in
c......general direction of tool axis
c
          call betvec (sc(4),gpt,ang)
          if (ang .gt. 90.) then
              call vctmsc (gpt,gpt,-1.d0)
              gpt(4) = gpt(4) * (-1.)
          endif
c
c...Scalar (canonical form)
c
      else if (scalar) then
          gpt(1) = tv
          call parsit
          if (.not. scalar) go to 9020
          gpt(2) = tv
          call parsit
          if (.not. scalar) go to 9020
          gpt(3) = tv
          if (kfl .eq. 3) then
              call parsit
              if (.not. scalar) go to 9020
              gpt(4) = tv
          endif
c
c...Unrecognized geometry type
c
      else if (kfl .eq. 1) then
          go to 9000
      else if (kfl .eq. 2) then
          go to 9010
      else
          go to 9030

      endif
c
c...End of routine
c
 8000   return
c
c...Point expected
c
 9000 kerr   = 20
      go to 8000
c
c...Vector expected
c
 9010 kerr   = 11
      go to 8000
c
c...Scalar expected
c
 9020 kerr   = 7
      go to 8000
c
c...Plane expected
c
 9030 kerr   = 19
      go to 8000
      end

C*    E_SUBROUTINE     : prfpar (fthick1,fthick2, toolaxangle, tiltanglebegin,tiltangleend, tiltdistbegin, tiltdistend,
C*                              endis, enangle, enrise, enrad, enrise2,exdis, exangle, exrise, exrad, exrise2,
C*                               clrdis, rptdis, rtrdis, lrtrdis,fnpas, cutcpdst,fdrt1,
C*                               fdrt2, fdrt3, fdrt4, fdrt5, fdrt6, fnorm)
C*       Retrieves/saves current interface defined PROFIL parameters from/to prfcom.com
C*       Fortran common area.  It is used to load/update the  PROFILL
C*       statement building form.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          fthick1           thickness in "curve" interface form screen  
C*          fthick2           thickness in "part surface" interface form screen  
C*          toolaxangle       tool axis angle in "part surface" interface form screen  
C*          tiltanglebegin    tilt begin angle in "part surface" interface form screen  
C*          tiltangleend      tilt end angle in "part surface" interface form screen  
C*          tiltdistbegin     tilt begin dist in "part surface" interface form screen  
C*          tiltdistend       tilt end dist in "part surface" interface form screen  
C*          endis             entry dist in "part surface" interface form screen  
C*          enangle           entry angle in "part surface" interface form screen  
C*          enrise            entry rise in "part surface" interface form screen  
C*          enrad             entry radius in "part surface" interface form screen  
C*          enrise2           entry radius rise in "part surface" interface form screen  
C*          exdis             exit dist in "part surface" interface form screen  
C*          exangle           exit angle in "part surface" interface form screen  
C*          exrise            exit rise in "part surface" interface form screen  
C*          exrad             exit radius in "part surface" interface form screen  
C*          exrise2           exit radius rise in "part surface" interface form screen  
C*          clrdis            cleardist in "positioning" interface form screen  
C*          rptdis            raptdist in "positioning" interface form screen   
C*          rtrdis            retractdist in "positioning" interface form screen   
C*          lrtrdis           loop retract dist in "positioning" interface form screen   
C*          fnpas             number of passes in "passes" interface form screen  
C*          cutcpdst          cutcom distance in "options" interface form screen  
C*          fdrt1-fdrt6       dedrats in "feed rates" interface form screen  
C*          fnorm             NORMAL is in command


C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

               
       subroutine prfpar (fthick1,fthick2, toolaxangle, tiltanglebegin, 
     x  tiltangleend, tiltdistbegin, tiltdistend,
     x  endis, enangle, enrise, enrad, enrise2,
     x  exdis, exangle, exrise, exrad, exrise2,
     x  clrdis, rptdis, rtrdis, lrtrdis,fnpas, cutcpdst,fdrt1, 
     x  fdrt2, fdrt3, fdrt4, fdrt5, fdrt6, fnorm)

      include 'com8a.com'
      include 'prfcom.com'
        
      
      real*8 fthick1, fthick2, toolaxangle, tiltanglebegin, 
     x tiltangleend, tiltdistbegin, tiltdistend,
     x endis, enangle, enrise, enrad, enrise2,
     x exdis, exangle, exrise, exrad, exrise2, 
     x clrdis, rptdis, rtrdis, lrtrdis, fnpas, cutcpdst,
     x fdrt1, fdrt2, fdrt3, fdrt4, fdrt5, fdrt6 
      
      integer*4 fnorm
 
      
      fthick1 = PRFTHK(1)
      fthick2 = PRFTHK(2)
      
      toolaxangle = PRFTV(3)
      tiltanglebegin = PRFTLT(3)
      tiltangleend = PRFTLT(4)
      tiltdistbegin = PRFTLT(1)
      tiltdistend = PRFTLT(2)
      
      endis  = PRFENT(3)
      enangle  = PRFENT(4)
      enrise = PRFENT(5)
      enrad = PRFENT(1)
      enrise2 = PRFENT(2)
      
      exdis  = PRFEXI(3)
      exangle  = PRFEXI(4)
      exrise = PRFEXI(5)
      exrad = PRFEXI(1)
      exrise2 = PRFEXI(2)
      
      clrdis = PRFCLF(5)
      
      rptdis = PRFPPL(5)
      rtrdis = PRFRPL(5)
      
      lrtrdis = PRFPPL(5)
      
      fnpas = PRFDEP(2)
      
      cutcpdst = PRFENT(6)
      
      fdrt1 = PRFFED(1)
      fdrt2 = PRFFED(2)
      fdrt3 = PRFFED(3)
      fdrt4 = PRFFED(4)
      fdrt5 = PRFFED(5)
      fdrt6 = PRFFED(6)
      
      if (IPRFFL(8) .eq. 1) then
          fnorm  = IPRFFL(8)
      endif
      
      return
      end


C*********************************************************************
C*    E_SUBROUTINE     : subroutine prfsavepar ()
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine prfsavepar (rptdis, rtrdis,clrdis)

      include 'com8a.com'
      include 'prfcom.com'
      
      real*8 rptdis, rtrdis,clrdis
      
      !frptdis = rptdis
      !frtrdis = rtrdis
      
      PRFPPL(5) = rptdis
      PRFRPL(5) = rtrdis
      PRFCLF(5) = clrdis
      
      return
      end
