c
c***********************************************************************
c
c     FILE NAME: bldang
c
c     CONTAINS: bldchk  bldchk1 bldang  bldrot  bldret  bretgn  bldcir
c               bldmot  alncck  alnck1  alcntl  alngen  alngen1 alnset
c               gtlspt  setbld  alnpos  alnmck  smoout  smopts  smock1
c               makeci  tntoci  gtfwvc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        bldang.f , 24.3
c     DATE AND TIME OF LAST  MODIFICATION
c        08/04/14 , 15:29:53
c
c***********************************************************************
c
c***********************************************************************
c
c     SUBROUTINE: bldchk (gmaxs,gmch,grot,gtv,gbang,gdis,gdlt,ksid,kflg)
c
c     FUNCTION:  This routine calculates blade rotary axis position
c                for Ultrasonic cutter (MACHTP = 3).  The 4-th rotary
c                axis is used for blade angular position so 3 rotary
c                axes are left for motion positioning.  The blade
c                angle must be tangent to the motion direction at
c                any point.
c
c     INPUT:   gmaxs  R*8  D10 -  machine axes for curent point
c
c              gmch   R*8  D3  -  previous point coordinates
c
c              grot   R*8  D3  -  previous rotary axes position
c
c              gtv    R*8  D3  -  previous tool axis vector
c
c     OUTPUT:  gbang  R*8  D3  -  Blade angles:
c                                 (1) - Initial (zero) Blade axis angle.
c                                 (2) - Blade absolute angle (FWD).
c                                 (3) - Base angle for Blade Rotary axis.
c
c              gdis   R*8  D1  -  move span
c
c              gdlt   R*8  D1  -  delta blade rotation
c
c              ksid   I*4  D1  -  blade side
c
c              kflg   I*4  D1  -  1 - blade angle is set, 0 - motion is
c                                 along tool axis (blade angle not set)
c
c***********************************************************************
c
      subroutine bldchk (gmaxs,gmch,grot,gtv,gbang,gdis,gdlt,ksid,kflg)
c
      include 'post.inc'
c
      integer*4 ksid(2),kflg
      real*8 gmch(3),grot(20,2),gbang(3,4),gdlt(2),gtv(3),gmaxs(10),gdis
c
      equivalence (ISIDBL,KPOSMP(0812)), (IFWDFL,KPOSMP(0832))
      equivalence (IFWSFL,KPOSMP(0833)), (IRTNUM,KPOSMP(1243))
      equivalence (IRAP  ,KPOSMP(3199)), (ALNMOD,KPOSMP(4049))
c
      integer*4 IRAP,IRTNUM,ISIDBL,IFWDFL,ALNMOD,IFWSFL
c
      equivalence (BLDVEC,POSMAP(3589))
      equivalence (BLDSTO,POSMAP(3592)), (FWDVEC,POSMAP(4567))
      equivalence (FWDDIR,POSMAP(4580)), (FWDSAV,POSMAP(4591))
c
      real*8 BLDVEC(3),BLDSTO,FWDDIR(3),FWDSAV(3),
     -       FWDVEC(3)
c
      integer*4 j,ifw,iaxsw(10),iaxcnt
c
      real*8 vdir(3),ppt(3,4),vpt(3),rpt(20,2),lpt(6),vdirn(3),ndist
c
      call alladr (gmaxs,lpt,ppt,rpt,vpt,5,1)
      call whchax (gmaxs,iaxsw,iaxcnt)
      if (iaxcnt*iaxsw(10) .eq. 1) iaxcnt = 0
      kflg   = 0
      gdlt(1) = 0.d0
      gdlt(2) = 0.d0
c
c...Get last point axes,
c...see if output held back
c
c      call gtlspt (gmch,gtv,grot)
      if (iaxcnt .eq. 0) go to 8100
      if (IRAP .ne. 0) go to 8100
c
c...Get FWD vector in part system of coordinates
c...If no motion then FWD vector is change of the
c...tool vector (if any)
c......ALNMOD = 1 use linear move as a FWD vector to comply
c......with old version of Pworks (ver <= 10.30.96)
c
      if (ALNMOD .eq. 1) then
          if (IFWDFL .eq. 0) then
             call gtfwvc (gmch,gtv,ppt(1,2),vpt,vdir,ifw)
          else
             call copyn (FWDVEC,vdir,3)
          end if
          call copyn (vdir,vdirn,3)
c
c......ALNMOD - 2 use FWD vector from 5200/5210 records in
c......forward sense
c
      else if (ALNMOD .eq. 2) then
          call copyn (FWDSAV,vdir,3)
          call copyn (FWDDIR,vdirn,3)
          if (IFWSFL .eq. 0) call copyn (vdir,vdirn,3)
c
c......ALNMOD - 3 use FWD vector from 5200/5210 records in
c......backward sense
c
      else
          call copyn (FWDSAV,vdir,3)
          call copyn (vdir,vdirn,3)
      end if
c
c...Get blade direction vector at start of move
c...and define blade rotary position
c
      ksid(1) = ISIDBL
      call bldchk1 (gmch,gtv,grot,vdir,grot,gbang(1,1),gdlt(1),
     -              ksid(1),j)
c
c...define blade rotary position at end point
c...force same side of blade on motion as set at start point
c
      ksid(2) = -ksid(1)
      call bldchk1 (ppt,vpt,rpt,vdirn,grot,gbang(1,2),gdlt(2),
     -              ksid(2),kflg)
c
c...get move span (used in bldang to test align condition)
c
      gdis   = ndist(gmch,ppt(1,2))
c
 8100 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: bldchk1 (gppt,gvpt,grpt,gdir,grot,gbang,gdlt,
c                          ksid,kflg)
c
c     FUNCTION:  This routine does the actual calculations of blade
c                rotary axis position using direction vector of
c                blade motion in part's coordinates.
c
c     INPUT:   gppt   R*8  D3  -  CL point coordinates
c
c              gvpt   R*8  D3  -  tool axis vector at point
c
c              grpt   R*8  D3  -  rotary axes position at point
c
c              gdir   R*8  D3  -  direction of blade at point
c
c              grot   R*8  D20.2  -  previous rotary axes position
c
c     OUTPUT:  gbang  R*8  D3  -  Blade angles:
c                                 (1) - Initial (zero) Blade axis angle.
c                                 (2) - Blade absolute angle (FWD).
c                                 (3) - Base angle for Blade Rotary axis.
c
c              gdlt   R*8  D1  -  delta blade rotation
c
c              ksid   I*4  D1  -  blade side
c
c              kflg   I*4  D1  -  1 - blade angle is set, 0 - motion is
c                                 along tool axis (blade angle not set)
c
c***********************************************************************
c
      subroutine bldchk1 (gppt,gvpt,grpt,gdir,grot,gbang,gdlt,
     -                    ksid,kflg)
c
      include 'post.inc'
c
      integer*4 ksid,kflg
      real*8 gppt(3),grot(20,2),gbang(3),gvpt(3),grpt(20,2),gdir(3),gdlt
c
      equivalence (ISIDBL,KPOSMP(0812))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTWRK,KPOSMP(1506))
      equivalence (IJKROT,KPOSMP(1739))
c
      integer*4 IRTNUM,ISIDBL,IRTWRK(20),IJKROT
c
      equivalence (SPIVEC,POSMAP(3583)), (BLDSTO,POSMAP(3592))
      equivalence (BLDVEC,POSMAP(3589)), (ROTBAS,POSMAP(1435))
      equivalence (FWDDIR,POSMAP(4580)), (FUZZ4 ,POSMAP(4912))
c
      real*8 BLDVEC(3),ROTBAS(4),BLDSTO,FWDDIR(3),SPIVEC(3),FUZZ4
c
      integer*4 ierr,i
c
      real*8 ptf(3),ptd(3),vec(3),ang,plpar(4),anv,anb,d,bas,ndot
      real*8 a1,a2,ad,fi,th,tvmp(4)
c
      kflg   = 0
c
c...Build a plane thru the point perpto TLVEC
c...and project FWD vector on this plane
c
      call plnvpt (gvpt,gppt,plpar,ierr)
      call vcplvc (gppt,gdir,ptf,1.d0)
      call plnint (ptf,gvpt,plpar,ptd,ierr)
      call vcplvc (ptd,gppt,vec,-1.d0)
      d      = dsqrt (ndot(vec,vec))
      if (d .lt. 1.d-5) go to 8100
      call unitvc (vec,vec)
c
c...Adjust direction vector to the spindle
c...zero position.  If rotary axes are not supported
c...use tool axis vector rotation as conversion tool
c...vp 5/5/98 added for GFM robot
c
      if (IJKROT .eq. 1) then
         call getlan (SPIVEC,gvpt,a1,a2,ad,fi,th,tvmp)
         call vecrot (vec,vec,tvmp,ad)
      else
         do 310 i=1,IRTNUM,1
            ang = 360.0 - grpt(i,1)
            call vecadj (vec,vec,ang,IRTWRK(i))
  310    continue
      end if
c
c...Get absolute angle of blade counted from initial
c
      call vecang(vec,3,anv)
      call vecang(BLDVEC,3,anb)
c     ksid   = ISIDBL
      bas    = ROTBAS(4)
      call bldrot (anv,anb,bas,BLDSTO,grot(4,1),ang,ksid)
c
c...Check if Blade rotation in delta limit,
c...set output for blade rotary axis
c
      gdlt   = dabs (ang + bas - grot(4,2))
      gbang(1) = ang
      gbang(2) = anv
      gbang(3) = bas
      kflg   = 1
c
 8100 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: bldang (kflg)
c
c     FUNCTION:  This routine calculates blade rotary axis position
c                for Ultrasonic cutter (MACHTP = 3).  The 4-th rotary
c                axis is used for blade angular position so 3 rotary
c                axes are left for motion positioning.  The blade
c                angle must be tangent to the motion direction at
c                any point.
c
c     INPUT:   kflg     I*4  D1  -  status of blade align: 0 - ALIGN/OFF
c                                   or not required, 1 - ALIGN point is
c                                   preset.
c
c     OUTPUT:  kflg     I*4  D1  -  Output status for ALIGN command:
c                                   0 - ignore align point (if set) since
c                                   retract tool & align blade will be
c                                   executed, 1 - use align point if any.
c
c***********************************************************************
c
      subroutine bldang (kflg)
c
      include 'post.inc'
c
      integer*4 kflg
c
      equivalence (ISIDBL,KPOSMP(0812)), (IBPOST,KPOSMP(0830))
      equivalence (IFWSFL,KPOSMP(0833))
      equivalence (IFWDFL,KPOSMP(0832)), (ALNMOD,KPOSMP(4049))
      equivalence (SMOKDO,KPOSMP(4083))
c
      integer*4 ISIDBL,SMOKDO,IBPOST(2),ALNMOD,IFWDFL,IFWSFL
c
      equivalence (AXSOUT,POSMAP(1340)), (BLDSTO,POSMAP(3592))
      equivalence (ROTBAS,POSMAP(1435))
      equivalence (BLDELT,POSMAP(3593)), (ALNDIS,POSMAP(4444))
      equivalence (ALNAXS,POSMAP(4570)), (FWDDIR,POSMAP(4580))
      equivalence (TBPOST,POSMAP(4585)), (ROTANG,POSMAP(5173))
c
      real*8 AXSOUT(10),ALNDIS(3),ROTANG(20,2),ROTBAS(4),
     -       ALNAXS(10),BLDSTO,BLDELT,FWDDIR(3),TBPOST(6)
c
      integer*4 ifl,i,sd(4),ifl1,ixbl
c
      real*8 d(4),rmch(3),rot(20,2),tax(3),bang(3,4),dis
c
c...Get last point axes,
c...see if output held back
c
      ifl    = 0
      ifl1   = 0
      i      = 3
      ixbl   = 4
      call gtlspt (rmch,tax,rot)
c
c...Check blade rotation for end point and align point
c...if set, cancel align if align blade angle is small
c
      if (kflg .eq. 1) then
          call bldchk (ALNAXS,rmch,rot,tax,bang(1,1),dis,
     -                 d(1),sd(1),ifl1)
          if (d(1) .lt. ALNDIS(3) .and. SMOKDO .ne. 1) then
             ifl1 = 0
          else
             i    = 1
             ixbl = 2
          end if
      end if
c
c...If align is not preset check blade for end point
c
      if (ifl1 .eq. 0) then
          call bldchk (AXSOUT,rmch,rot,tax,bang(1,3),dis,d(3),sd(3),ifl)
          if (kflg .eq. 1 .and. d(3) .gt. ALNDIS(3)) then
             ifl1 = 1
             i    = 1
             ixbl = 2
          end if
      end if

      if (ifl+ifl1 .eq. 0) then
          if (d(ixbl) .gt. .0005) then
              rot(4,1) = bang(1,ixbl)
              rot(4,2) = bang(1,ixbl) + bang(3,ixbl)
              ROTBAS(4) = bang(3,ixbl)
              BLDSTO = bang(2,ixbl)
              ISIDBL = sd(ixbl)
          end if
          go to 8100
      end if
c
c...Check if Blade rotation in delta limit -
c...over max: retract and rotate blade, see if align &
c...end point have close blade angle cancel align point
c     if (ALNMOD .ne. 2) kflg = ifl1
c
      if (ALNMOD .ne. 2 .or. IFWDFL .eq. 0) kflg = ifl1
c
c     if (d(i) .gt. BLDELT .and. SMOKDO .ne. 2) then
c...
c...above 'if' is replaced by the following conditions where we need
c...retract blade to set its angle:
c...  old 'if' when align option is not 2,
c...          for ALNMOD = 2:
c...  when smooth/aa is pending and direction vector
c...     was set for the very first point of the curve,
c...  when one curve is complete and we start processing of the next
c...     one with smooth/aa still active (SMOKDO = 2)
c...  when smooth/aa is disabled and every point can cause significant
c...     blade rotation (tool retract)
c
      if (d(i) .gt. BLDELT .and.
     -   ((ALNMOD .ne. 2 .and. SMOKDO .ne. 2) .or.
     -   (ALNMOD .eq. 2 .and. SMOKDO .eq. 1 .and. IFWSFL .ne. 1) .or.
     -   (ALNMOD .eq. 2 .and. SMOKDO .eq. 2 .and. IFWSFL .eq. 2) .or.
     -   (ALNMOD .eq. 2 .and. SMOKDO .eq. 0))) then
          if (dabs(d(1)-d(2)).lt.ALNDIS(3) .and. SMOKDO.ne.1) kflg = 0
          call pshaxs (AXSOUT,tax)
          if (kflg .ne. 0) call pshaxs (ALNAXS,tax)
          call bretgn (bang(1,i),sd(i),rmch,tax,rot)
          TBPOST(4) = bang(1,ixbl)
          TBPOST(5) = bang(3,ixbl)
          TBPOST(6) = bang(2,i)
          IBPOST(2) = sd(i)
      else
c
c......Small angle: make align point if required.
c......ALNMOD = 2:  when SMOOTH/AA active align point can be set at
c......the end of move if move is long enough hance blade angle
c......at align end point will be same as at the start point (ixbl=3)
c......otherwise cancel align end point.
c
          if (ALNMOD .eq. 2) then
             if (kflg .eq. 2) then
                if (dabs(d(3)-d(4)) .lt. ALNDIS(3) .and. SMOKDO .ne. 2
     -              .or. dis .lt. ALNDIS(2)) then
                   kflg = 0
                else
                   ixbl = 3
                end if
             end if
          end if
c
          if (kflg .ne. 0) call alnset (kflg)
          ROTANG(4,1) = bang(1,ixbl)
          ROTANG(4,2) = bang(1,ixbl) + bang(3,ixbl)
          AXSOUT(10) = bang(1,ixbl) + bang(3,ixbl)
          ROTBAS(4) = bang(3,ixbl)
          BLDSTO = bang(2,ixbl)
          ISIDBL = sd(ixbl)
      end if
c
c...End of routine
c
 8000 return
c
c...Blade angle not changed,
c...set old angle
c
 8100 ROTANG(4,1) = rot(4,1)
      ROTANG(4,2) = rot(4,2)
      AXSOUT(10) = rot(4,2)
      kflg   = 0
      go to 8000
      end
c
c***********************************************************************
c
c     SUBROUTINE: bldrot (gang,gent,gbase,gbsav,gsto,gout,ksid)
c
c     FUNCTION:  This routine returns blade rotary axis position
c                on rotary scale, adjusts base angle and finds what
c                blade side is used.
c
c     INPUT:   gang     R*8  D1  -  Blade absolute angle (FWD angle).
c
c              gent     R*8  D1  -  Initial (zero) Blade axis position.
c
c              gbase    R*8  D1  -  Base angle for Blade Rotary axis.
c
c              gbsav    R*8  D1  -  Previous Blade angle (old FWD).
c
c              gsto     R*8  D1  -  Previous Blade axis position.
c
c              ksid     I*4  D1  -  Previous active blade side.
c                                   1 = default side, 2 = oposite sdie.
c
c     OUTPUT:  gout     R*8  D1  -  Blade rotary axis position on rotary
c                                   scale.
c
c              gbase    R*8  D1  -  Current base angle.
c
c              ksid     I*4  D1  -  Current blade side.
c
c***********************************************************************
c
      subroutine bldrot (gang,gent,gbase,gbsav,gsto,gout,ksid)
c
      integer*4 ksid
      real*8 gang,gent,gout,gbase,gbsav,gsto
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (IBLDIR,KPOSMP(0834))
c
      integer*4 MOTREG(24),IBLDIR
c
      real*8 ang,asav,sta,com,out,del,base,asto,rd
c
      integer*4 inum,irt,nbl
c
      base  = gbase
c
c...Format angles
c
      call codint (MOTREG(22),gang,ang,inum)
      if (ang .eq. 360.0) ang = 0.d0
      gang = ang
      call codint (MOTREG(22),gent,sta,inum)
      if (sta .eq. 360.0) sta = 0.d0
      call codint (MOTREG(22),gbsav,asav,inum)
      if (asav .eq. 360.0) asav = 0.d0
      call codint (MOTREG(22),gsto,asto,inum)
      if (asto .eq. 360.0) asto = 0.d0
c
      del    = ang - asav
      rd     = del
      if (del .lt. 0.d0) del = del + 360.d0
c
c...Check if same blade side
c...or same side is forced
c
      nbl    = ksid
      if (nbl .gt. 0) then
         if (del .gt. 90.d0 .and. del .lt. 270.d0) nbl = 3 - ksid
      else
         nbl = -ksid
      end if
c
c...User forced blade side
c
      if (IBLDIR .eq. 1 .or. IBLDIR .eq. 2) nbl = IBLDIR
c
c...Translate blade angle for initial position
c...adjust for blade side
c
      com    = ang - sta
      if (nbl .eq. 1) then
          out = com
      else
          out = 180.0 + com
      end if
c
c...Set-up output angle (for ROTANG(4,1))
c
      if (out .lt. 0.0) out = out + 360.0
      if (out .gt. 360.0) out = out - 360.0
      if (out .eq. 360.0) out = 0.0
      del    = out - asto
      irt    = 1
      if (del .gt. 180.0 .or.
     -    (del .gt. -180.0 .and. del .lt. 0.)) then
           irt = 2
      end if
c
c...Adjust base angle
c
      if (irt .eq. 1 .and. out .lt. asto) base = base + 360.0
      if (irt .eq. 2 .and. out .gt. asto) base = base - 360.0

c
c...End of routine
c
      gout   = out
      gbase  = base
      ksid   = nbl
c
      return
      end

c***********************************************************************
c
c     SUBROUTINE: bldret (kflg,kret)
c
c     FUNCTION:  This routine generates for subsequent calls post motion
c                to retract blade tool when blade rotation exceeds
c                specified limit.
c
c     INPUT:   kflg     I*4  D1  -  Input switch: 1 - .
c
c              ksid     I*4  D1  -  Flag from mchlin to signal rectract
c                                   procedure from linearization.
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine bldret (kflg,kret)
c
      integer*4 kflg,kret
c
      include 'post.inc'
c
      equivalence (IFWSFL,KPOSMP(0833)), (IZIGON,KPOSMP(0370))
      equivalence (IRETBL,KPOSMP(0817)), (HLDFLG,KPOSMP(1622))
      equivalence (IZIGAD,KPOSMP(0371)), (ISCIRC,KPOSMP(1238))
      equivalence (IFLNRS,KPOSMP(1731)), (ITP   ,KPOSMP(1801))
      equivalence (SMOKAN,KPOSMP(4072)), (SMOKDO,KPOSMP(4083))
c
      integer*4 IRETBL,ITP,HLDFLG,IFLNRS,IZIGON,
     -          ISCIRC,IZIGAD,IFWSFL,SMOKDO,SMOKAN
c
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (AXSOUT,POSMAP(1340)), (PTPOST,POSMAP(2472))
      equivalence (TBPOST,POSMAP(4585)), (PTDEST,POSMAP(2477))
      equivalence (LINAXS,POSMAP(1299)), (ROTBAS,POSMAP(1435))
      equivalence (RETFED,POSMAP(2254)), (TL    ,POSMAP(3601))
      equivalence (PTLPLU,POSMAP(2301)), (BLDSTO,POSMAP(3592))
      equivalence (ROTSTB,POSMAP(4401)), (ROTANG,POSMAP(5173))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),VECSAV(3),STONUM(3,4),ROTSTO(20,2),
     -       TLVEC(3),AXSOUT(10),PTPOST(3),LINAXS(6),BLDSTO,
     -       TBPOST(6),PTDEST(3),ROTANG(20,2),ROTBAS(4),
     -       TL(120),PTLPLU(3),ROTSTB(4,2),RETFED(4)
c
      integer*4 i,iaxs(10),nax,ix
c
c...Retract at current point
c
      if (kflg .eq. 0) then
          IFLNRS = 1
          if (HLDFLG .eq. 1) call clrmot (1)
          call linfed (1,RETFED)
          kflg = 1
c
c...Rotate blade at retract point
c
      else if (kflg .eq. 1) then
          call setbld (4,AXSOUT,ROTANG,1)
          ROTBAS(4) = TBPOST(2)
          if (RETFED(1) .eq. 0.) call raprst
          call linfed (6,RETFED)
          kflg = 2
c
c...Plunge to old point
c
      else if (kflg .eq. 2) then
          if (HLDFLG .eq. 1) call clrmot (1)
          if (IZIGON .eq. 1 .and. ISCIRC .eq. 1) IZIGAD = 1
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iaxs,nax)
          call setbld (4,AXSOUT,ROTANG,1)
          if (RETFED(3) .eq. 0.) call raprst
          call linfed (2,RETFED)
          kflg = 3
c
c...Move to new point
c
      else if (kflg .eq. 3) then
          if (HLDFLG .eq. 1) call clrmot (1)
          if (IZIGON .eq. 1) IZIGAD = 1
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iaxs,nax)
          do 105 i=1,3
              ROTSTO(i,1) = ROTSTB(i,1)
              ROTSTO(i,2) = ROTSTB(i,2)
  105     continue
          IZIGAD = 2
c
c...if SMOOTH is in effect & FWD vector from cl file use start
c...point direction for blade (ix=1) otherwise end point direction
c
          ix     = 2
          if (SMOKDO .eq. 1 .and. IFWSFL .ne. 0) ix = 1
          call setbld (4,AXSOUT,ROTANG,ix)
          ROTBAS(4) = TBPOST(ix*3-1)
          call linfed (5,RETFED)
          kflg   = 0
          IRETBL = 0
      end if
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: bretgn (gang,ksid,gmch,gax,grot)
c
c     FUNCTION:  This routine generates retract point and saves
c                plunge back and next point for blade cutter.
c
c     INPUT:   gang     R*8  D3  -  Blade rotary axis position.
c
c              ksid     I*4  D1  -  Blade side.
c
c              gmch     R*8  D3  -  Previous position (cl point).
c
c              gax      R*8  D3  -  Previous tool vector.
c
c              grot     R*8  D20,2 -  Previous rotary axes.
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine bretgn (gang,ksid,gmch,gax,grot)
c
      real*8 gang(3),gmch(3),gax(3),grot(20,2)
      integer*4 ksid
c
      include 'post.inc'
c
      equivalence (IRETBL,KPOSMP(0817)), (IBPOST,KPOSMP(0830))
      equivalence (IFLNRS,KPOSMP(1731)), (ITP   ,KPOSMP(1801))
c
      integer*4 IBPOST(2),IRETBL,ITP,IFLNRS
c
      equivalence (MCHNUM,POSMAP(1287)), (AXSOUT,POSMAP(1340))
      equivalence (LINAXS,POSMAP(1299)), (TL    ,POSMAP(3601))
      equivalence (ROTSTB,POSMAP(4401)), (TBPOST,POSMAP(4585))
c
      real*8 MCHNUM(3,4),AXSOUT(10),LINAXS(6),ROTSTB(4,2),
     -       TBPOST(6),TL(120)
c
      real*8 rdis,rmch(3,4),lnax(6),mxout(10)
c
      integer*4 i
c
c...Save current position for plunge move
c
      do 105 i=1,3
         ROTSTB(i,1) = grot(i,1)
         ROTSTB(i,2) = grot(i,2)
  105 continue
      call copyn (gmch,rmch(1,2),3)
      call alladj (rmch,lnax,mxout,grot,2,5)
      call pshaxs (mxout,gax)
c
c...Save blade position for next move
c
      TBPOST(1) = gang(1)
      TBPOST(2) = gang(3)
      TBPOST(3) = gang(2)
      IBPOST(1) = ksid
c
c...Get retract position using current point
c
      call mwrdis (rmch(1,3),grot,rdis)
      call vcplvc (gmch,gax,MCHNUM(1,2),rdis)
      call alladj (MCHNUM,LINAXS,AXSOUT,grot,2,5)
      IFLNRS = 0
      IRETBL = 1
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: bldcir (gprm,kprm)
c
c     FUNCTION:  This routine calculates blade position for circular
c                motion when circular interpolation is used.
c
c     INPUT:   gprm     R*8  D25 -  Output circle parameters (RCIPRM form).
c
c              kprm     I*4  D8  -  Output circle parameters (ICIPRM form).
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine bldcir (gprm,kprm)
c
      real*8 gprm(25)
      integer*4 kprm(8)
c
      include 'post.inc'
c
      equivalence (IZIGON,KPOSMP(0370))
      equivalence (ISIDBL,KPOSMP(0812)), (ISCIRC,KPOSMP(1238))
      equivalence (IRTNUM,KPOSMP(1243))
c
      integer*4 ISIDBL,ISCIRC,IRTNUM,IZIGON
c
      equivalence (RAD   ,POSMAP(0002)), (TLVEC ,POSMAP(1369))
      equivalence (ZIGDEP,POSMAP(0190)), (ZIGSTO,POSMAP(0191))
      equivalence (AXSOUT,POSMAP(1340)), (BLDSTO,POSMAP(3592))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (BLDVEC,POSMAP(3589)), (SPIVEC,POSMAP(3583))
      equivalence (BLDELT,POSMAP(3593)), (ROTBAS,POSMAP(1435))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2),RAD,AXSOUT(10),BLDVEC(3),SPIVEC(3),
     -       ROTANG(20,2),ROTBAS(4),BLDSTO,BLDELT,TLVEC(3),
     -       VECSAV(3),STONUM(3,4),ZIGDEP,ZIGSTO
c
      real*8 vec(3),a,anv,ang,anb,base,d,a36,ane,spv(3),bang(3)
c
      integer*4 isid,is3
c
      data a36 /360.d0/
c
c...Check if spindle vector is perp to CI plan
c
      call pivvec (ROTANG,spv)
      is3    = kprm(3)
      if (dabs(spv(is3)) .lt. .99) go to 8000
c
c...Get direction angle at start point
c
      call tntoci (ROTANG,gprm,kprm,vec)
      call vecang(vec,3,bang(2))
c
c...Get absolute angle of blade counted from initial
c
      call vecang(BLDVEC,3,anb)
      isid   = ISIDBL
      bang(3) = ROTBAS(4)
      call bldrot (bang(2),anb,bang(3),BLDSTO,ROTSTO(4,1),
     -             bang(1),isid)
      base    = bang(3)
      ang     = bang(1)
      anv     = bang(2)
c
c...Check if Blade rotation in delta limit,
c...when start cutting the circle
c
      d      = dabs (bang(1) + bang(3) - ROTSTO(4,2))
      if (d .gt. BLDELT) then
          call bretgn (bang(1),bang(3),STONUM(1,2),VECSAV,ROTSTO)
          if (IZIGON .eq. 1) ZIGDEP = ZIGSTO
          call bldmot
      end if
c
c...Get blade positon at the end point of arc
c
      if (kprm(5) .eq. 2) then
          d  = gprm(7)
      else
          d  = 0.d0 - gprm(7)
      end if
c
c...Reverse delta angle if tool is oriented in '-'.
c
      if (spv(is3) .lt. 0.d0) d = 0.d0 - d
      a      = ang + d
      anv    = anv + d
      if (anv .ge. a36) then
          anv  = anv - a36
      else if (anv .lt. 0.d0) then
          anv  = anv + a36
      end if
c
c...Adjust base angle if necessary
c
      if (a .ge. a36) then
          a  = a - a36
          base = base + a36
      else if (a .lt. 0.) then
          a  = a + a36
          base = base - a36
      end if
c
c...Set output angles
c
      ane    = a + base
      ROTANG(4,1) = a
      ROTANG(4,2) = ane
      AXSOUT(10) = ane
      ROTBAS(4) = base
      BLDSTO = anv
      ISIDBL = isid
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: bldmot
c
c     FUNCTION:  This routine retracts the tool to rotate blade tangent
c                to circular arc at the start point.  All parameters
c                for repositioning are calculated in bldcir and bretgn.
c
c     INPUT:   none
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine bldmot
c
      include 'post.inc'
c
      equivalence (ISCIRC,KPOSMP(1238)), (IRETBL,KPOSMP(0817))
c
      integer*4 IRETBL,ISCIRC
c
      equivalence (AXSOUT,POSMAP(1340)), (ZIGDEP,POSMAP(0190))
      equivalence (RETFED,POSMAP(2254))
c
      real*8 AXSOUT(10),ZIGDEP,RETFED(4)
c
      integer*4 iaxfl4,iaxfl2,iaxcnt,iaxsw(10),icv
c
      iaxfl4 = 0
      iaxfl2 = 0
      iaxcnt = 0
      icv    = ISCIRC
c
c...Output motion block for each phase of retract proc
c
  100 if (IRETBL .ne. 0) call bldret (iaxfl4,iaxfl2)
      ISCIRC = 0
      call clrmot (1)
      call whchax (AXSOUT,iaxsw,iaxcnt)
      call motion (iaxsw,iaxcnt)
      ISCIRC = icv
      if (iaxfl4 .ne. 3) go to 100
      call linfed (5,RETFED)
      IRETBL = 0
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: alncck (kflg)
c
c     FUNCTION:  This routine checks if ALIGN/CUT command and
c                SMOOTH/AA command should be applied.
c
c     INPUT:   none
c
c     OUTPUT:  kflg     I*4  D1  -  0 = use next point (move short),
c                                   1 = align point created.
c
c***********************************************************************
c
      subroutine alncck (kflg)
c
      include 'post.inc'
c
      integer*4 kflg
c
      equivalence (ISCIRC,KPOSMP(1238)), (HLDFLG,KPOSMP(1622))
      equivalence (MODBLD,KPOSMP(3984)), (IRAP  ,KPOSMP(3199))
      equivalence (ALNCUT,KPOSMP(4040))
      equivalence (ALNMOD,KPOSMP(4049)), (SMOKAN,KPOSMP(4072))
      equivalence (SMOKDO,KPOSMP(4083))
c
      integer*4 IRAP,HLDFLG,ISCIRC,ALNCUT,SMOKAN,SMOKDO,MODBLD,
     -          ALNMOD
c
      equivalence (ALNBXS,POSMAP(0243)), (MCHNUM,POSMAP(1287))
      equivalence (AXSOUT,POSMAP(1340)), (AXSSTO,POSMAP(1425))
      equivalence (STONUM,POSMAP(1387))
      equivalence (HLDMCH,POSMAP(2141)), (ALNAXS,POSMAP(4570))
c
      real*8 MCHNUM(3,4),HLDMCH(3,4),STONUM(3,4),ALNBXS(10),
     -       ALNAXS(10),AXSOUT(10),AXSSTO(10)
c
      real*8 rmch(3)
c
      if (IRAP .ne. 0 .or. ISCIRC .eq. 1) go to 8000
c
c...ignore align setup if SMOOTH/AA processed (for ALNMOD 2).
c
      if (SMOKDO .eq. 2 .and. ALNMOD .eq. 2) go to 8000
c
c...Get previous position,
c...see if output held back
c
      if (HLDFLG .eq. 1) then
          call copyn (HLDMCH(1,2),rmch,3)
      else
          call copyn (STONUM(1,2),rmch,3)
      end if
      if (SMOKAN .eq. 1) then
         call smock1 (rmch,MCHNUM(1,2),ALNAXS,ALNBXS,kflg)
c
c...added 9/16/96 vp, when not blade machine 'bldang' is not
c...called and the next position must be pushed on stack here
c
         if (MODBLD .eq. 0) call alnset (kflg)
         SMOKDO = kflg
         SMOKAN = 3
      else if (ALNCUT .ne. 0) then
         call alnck1 (rmch,MCHNUM(1,2),AXSSTO,AXSOUT,ALNAXS,
     -                ALNBXS,kflg)
      end if
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: alnck1 (gsto,gmch,gxsto,gxout,gxnew,gxbac,kflg)
c
c     FUNCTION:  This routine checks if move is long enough to applay
c                ALIGN/CUT command and calculates align point coordinates
c                if required based on machine axes linear interpolation.
c
c     INPUT:   gsto    R*8  D3  -  previous CL point coordinates
c
c              gmch    R*8  D3  -  CL point coordinates
c
c              gxsto   R*8  D10 -  previous machine axes
c
c              gxout   R*8  D10 -  current machine axes
c
c     OUTPUT:  kflg     I*4  D1  -  0 = use current point (short move),
c                                   1 = align point created.
c
c              gxnew   R*8  D10 -  align point machine axes at start of move
c
c              gxbac   R*8  D10 -  align point machine axes at end of move
c
c***********************************************************************
c
      subroutine alnck1 (gsto,gmch,gxsto,gxout,gxnew,gxbac,kflg)
c
      include 'post.inc'
c
      real*8 gsto(3),gmch(3),gxsto(10),gxout(10),gxnew(10),gxbac(10)
      integer*4 kflg
c
      equivalence (ALNDIS,POSMAP(4444))
c
      real*8 ALNDIS(3)
c
      real*8 rat, d, ndist
c
      integer*4 i
c
c...Check span of move
c
      d     = ndist (gsto,gmch)
      if (d .lt. ALNDIS(2)) go to 8000
c
c...Set align point ratio
c
      rat   = ALNDIS(1) / d
c
c...Interpolate point & rotary axes where blade
c...will be aligned using machine axes. For end point
c...align use same distance as for start point (future use)
c
      do 110 i=1,10,1
         d = gxout(i) - gxsto(i)
         gxnew(i) = gxsto(i) + rat * d
         gxbac(i) = gxout(i) - rat * d
  110 continue
      kflg   = 1
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: alcntl (kflg)
c
c     FUNCTION:  This routine controls ALIGN/CUT flags
c
c     INPUT:   kflg    I*4  -  1 = reset ALIGN/CUT after omit.
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine alcntl (kflg)
c
      include 'post.inc'
c
      integer*4 kflg
c
      equivalence (ALNCUT,KPOSMP(4040)), (ALNCSK,KPOSMP(4041))
      equivalence (ALNMOV,KPOSMP(4045)), (ALNMSK,KPOSMP(4046))
      equivalence (ALNREN,KPOSMP(4048))
c
      integer*4 ALNCUT,ALNCSK,ALNMOV,ALNMSK,ALNREN
c
c...Reset ALIGN/CUT after OMIT move done
c...or ALIGN/MOVE after OMIT move done
c
      if (kflg .eq. 1) then
          if (ALNCSK .ne. 0) then
              ALNCUT = ALNCSK
              ALNCSK = 0
          end if
          if (ALNCUT .eq. 2) ALNCUT = 0
      else if (kflg .eq. 2) then
          if (ALNMSK .ne. 0) then
              ALNMOV = ALNMSK
              ALNMSK = 0
          end if
          if (ALNREN .eq. 3) then
              ALNREN = 0
              ALNMOV = 0
          end if
          if (ALNMOV .eq. 2) ALNMOV = 0
      end if
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: alngen1 (kflg)
c
c     FUNCTION:  This routine gets the end point of move when
c                ALIGN command is in effect.
c
c     INPUT:   none
c
c     OUTPUT:  kflg     I*4  D1  -  0 = align complete.
c
c***********************************************************************
c
      subroutine alngen1 (kflg)
c
      include 'post.inc'
c
      integer*4 kflg
c
      equivalence (IFWSFL,KPOSMP(0833))
      equivalence (ALNCUT,KPOSMP(4040)), (ALNMOD,KPOSMP(4049))
      equivalence (SMOKDO,KPOSMP(4083))
c
      integer*4 ALNMOD,ALNCUT,IFWSFL,SMOKDO
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (FWDDIR,POSMAP(4580)), (FWDSAV,POSMAP(4591))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 TLVEC(3),MCHNUM(3,4),AXSOUT(10),LINAXS(6),ROTANG(20,2),
     -       FWDDIR(3),FWDSAV(3)
c
      integer*4 iary(10),icnt
c
c...get next point
c
      call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
c
c...if expanded cl file allow end point alignment also
c
      if (ALNMOD .eq. 2 .and. ALNCUT .ne. 0) then
         kflg   = kflg + 1
         if (kflg .gt. 2) kflg = 0
      else
         kflg   = 0
      end if
c
c...if SMOOTH/AA is on and align point was created using 3 points
c...circle, FWDSAV is set tangent to circle, so to output last point
c...replace FWDSAV by FWDVEC (same as FWDDIR since IFWSFL = 0)
c
      if (SMOKDO .eq. 2 .and. IFWSFL .eq. 0)
     -        call copyn (FWDDIR,FWDSAV,3)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: alngen (kflg)
c
c     FUNCTION:  This routine retrieves points from stack created by
c                ALIGN/MOVE command.
c
c     INPUT:   kflg     I*4  D1  -  if > 0 = point number to retrieve,
c                                   -1 = reset align mode & feed rates
c
c     OUTPUT:  kflg     I*4  D1  -  if > 0 = point number to get wnen
c                                            next call of alngen,
c                                   0 = positioning complete.
c                                   -1 = next call reset align mode.
c
c***********************************************************************
c
      subroutine alngen (kflg)
c
      include 'post.inc'
c
      integer*4 kflg
c
      equivalence (INCR  ,KPOSMP(1226)), (IFLNRS,KPOSMP(1731))
      equivalence (IRPSAV,KPOSMP(3204)), (IRAPDO,KPOSMP(3220))
      equivalence (IRPSUP,KPOSMP(3189)), (IRPALT,KPOSMP(3196))
      equivalence (IRPTAX,KPOSMP(3197)), (IRAP  ,KPOSMP(3199))
      equivalence (ALNMOV,KPOSMP(4045)), (ALNREN,KPOSMP(4048))
c
      integer*4 IFLNRS,IRAPDO(8),IRAP,IRPSAV,INCR,IRPSUP,IRPALT,
     -          ALNMOV,ALNREN,IRPTAX
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435))
      equivalence (TBPOST,POSMAP(4585)), (RAPDIS,POSMAP(3582))
      equivalence (ALNMPS,POSMAP(4461)), (ROTANG,POSMAP(5173))
c
      real*8 ROTBAS(4),TBPOST(6),TLVEC(3),MCHNUM(3,4),AXSOUT(10),
     -       LINAXS(6),ROTANG(20,2),ALNMPS(3),RAPDIS
c
      integer*4 iary(10),icnt
      real*8 rap(2)
c
      data rap /0.,0./
c
      if (kflg .lt. 0) then
          if (IFLNRS .eq. 1) call raprst
          if (IFLNRS .eq. 2) call linfed (5,ALNMPS(2))
          kflg   = 0
          ALNREN = 3
          go to 8000
      end if
c
c...3 - point is rapid above from point
c
      if (kflg .eq. 3) then
         if (IRAP .ne. 0) then
            call linfed (1,rap)
         else
            call linfed (1,ALNMPS(2))
         end if
         kflg = 2
c
c...2 - point above next point
c
      else if (kflg .eq. 2) then
         if (IRAP .eq. 0) then
            call linfed (1,ALNMPS(2))
         else
            call linfed (1,rap)
            if (IRPALT*IRPTAX .eq. 5 .and. ALNMPS(1) .gt. 0.) then
               call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
            end if
         end if
         call setbld (4,AXSOUT,ROTANG,1)
         ROTBAS(4) = TBPOST(2)
         kflg = 1
c
c...1 - next point (plunge)
c
      else if (kflg .eq. 1) then
         call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
         if (IRAP .ne. 0) call raprst
         call linfed (2,ALNMPS(2))
         call setbld (4,AXSOUT,ROTANG,1)
         ROTBAS(4) = TBPOST(2)
         kflg = -1
      end if
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: alnset (kflg)
c
c     FUNCTION:  This routine replaces end point of move by calculated
c                align point when ALIGN/CUT command is in effect.
c
c     INPUT:   none
c
c     OUTPUT:  kflg     I*4  D1  -  0 = use next point (move short),
c                                   1 = start mid point created.
c                                   2 = end mid point created.
c
c***********************************************************************
c
      subroutine alnset (kflg)
c
      include 'post.inc'
c
      integer*4 kflg
c
      equivalence (ALNBXS,POSMAP(0243))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ALNAXS,POSMAP(4570)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),AXSOUT(10),ROTANG(20,2),TLVEC(3),
     -       ALNAXS(10),LINAXS(6),ALNBXS(10)
c
c...push end point on stack
c
      call pshaxs (AXSOUT,TLVEC)
c
c...Set align point for output
c
      if (kflg .eq. 1) then
         call copyn (ALNAXS,AXSOUT,10)
      else
         call copyn (ALNBXS,AXSOUT,10)
      end if
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
 8100 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: gtlspt (gmch,gtax,grot)
c
c     FUNCTION:  Gets last part program position & rotary axes
c                considering held back position.
c
c     INPUT:   none
c
c     OUTPUT:  gmch     R*8  D3    -  CL position (MCHNUM(1:3,2))
c
c              gtax     R*8  D3    -  Tool axis vector
c
c              grot     R*8  D20.2 -  Rotary axes
c
c***********************************************************************
c
      subroutine gtlspt (gmch,gtax,grot)
c
      include 'post.inc'
c
      real*8 gmch(3),gtax(3),grot(20,2)
c
      equivalence (HLDFLG,KPOSMP(1622))
c
      integer*4 HLDFLG
c
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (HLDMCH,POSMAP(2141)), (HLDVEC,POSMAP(2177))
      equivalence (ROTSTO,POSMAP(5213)), (HLDROT,POSMAP(5253))
c
      real*8 VECSAV(3),STONUM(3,4),ROTSTO(20,2),HLDMCH(3,4),
     -       HLDROT(20,2),HLDVEC(3)
c
c...See if output held back
c
      if (HLDFLG .eq. 1) then
          call copyn (HLDMCH(1,2),gmch,3)
          call copyn (HLDVEC,gtax,3)
          call cpyrot (HLDROT,grot)
      else
          call copyn (STONUM(1,2),gmch,3)
          call copyn (VECSAV,gtax,3)
          call cpyrot (ROTSTO,grot)
      end if
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: setbld (kax,gaxs,grot,kfl)
c
c     FUNCTION:  Sets blade rotary axis angle.
c
c     INPUT:   kax      I*4  D1    -  Rotary axis index
c
c     OUTPUT:  gaxs     R*8  D10   -  Machine axes
c
c              grot     R*8  D20.2 -  Rotary axes
c
c***********************************************************************
c
      subroutine setbld (kax,gaxs,grot,kfl)
c
      include 'post.inc'
c
      integer*4 kax,kfl
      real*8 gaxs(10),grot(20,2)
c
      equivalence (ISIDBL,KPOSMP(0812)), (IBPOST,KPOSMP(0830))
c
      integer*4 ISIDBL,IBPOST(2)
c
      equivalence (TBPOST,POSMAP(4585)), (BLDSTO,POSMAP(3592))
c
      real*8 TBPOST(6),BLDSTO
c
      integer*4 ix
c
c...Set up rotary axis values
c
      ix     = 1
      if (kfl .eq. 2) ix = 4
      grot(kax,1) = TBPOST(ix)
      gaxs(kax+6) = TBPOST(ix) + TBPOST(ix+1)
      grot(kax,2) = gaxs(kax+6)
      BLDSTO = TBPOST(ix+2)
      ISIDBL = IBPOST(kfl)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: alnpos (gclp,gvec,kflg)
c
c     FUNCTION:  Get 3-rd point for ALIGN/MOVE command and set blade
c                direction for motion following curent move (applaying
c                ALIGN/CUT if active), so at 2-nd point blade is aligned
c                for move 2-3.
c
c     INPUT:   none
c
c     OUTPUT:  gclp     R*8  D3   -  3-rd CL point coordinates
c
c              gvec     R*8  D3   -  3-rd CL point tool axis
c
c     INPUT:   kflg     I*4  D1   -  1 = 3-rd point is set, 0 = command
c                                    has been canceled.
c
c***********************************************************************
c
      subroutine alnpos (gclp,gvec,kflg)
c
      include 'post.inc'
c
      integer*4 kflg
      real*8 gclp(3),gvec(3)
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (MCHPLN,KPOSMP(1261)), (ALNMOV,KPOSMP(4045))
c
      integer*4 ITYPE,ISUBT,MXCL,NPT,MCHPLN,ALNMOV
c
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (PPTOLR,POSMAP(1274))
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387)), (ROTBAS,POSMAP(1435))
      equivalence (RCIPRM,POSMAP(2049)), (CIRBPP,POSMAP(4845))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 CLPT(240),CIRBUF(7),MCHNUM(3,4),TLVEC(3),STONUM(3,4),
     1       ROTBAS(4),RCIPRM(25),ROTSTO(20,2),ROTANG(20,2),
     2       CIRBPP(7),PPTOLR(10)
c
      real*8 ndist,savp(3),rcpr(25),savr(20,2),vc(3)
c
      integer*4 icpr(8),isw,ierr,inc
c
      kflg   = 0
      isw    = 0
  100 call clsamp (isw)
  150 isw    = 1
c
c...Post word
c
      if (ITYPE .eq. 14000) go to 8000
      if (ITYPE .eq. 2000 .or. ITYPE .eq. 14000) then
         if (ISUBT .eq. 5) go to 8000
         if (ISUBT .eq. 1076) then
             call align
             if (ALNMOV .eq. 0) go to 8000
         end if
         go to 100
      else if (ITYPE .eq. 3000) then
         call clsamp (1)
         if (ITYPE .ne. 5000) go to 150
c
c............Check for valid circle
c
         call copyn (STONUM(1,3),savp,3)
         call copyn (MCHNUM(1,3),STONUM(1,3),3)
         call cpyrot (ROTSTO,savr)
         call cpyrot (ROTANG,ROTSTO)
         call cirdir (CIRBUF,CIRBPP,rcpr,icpr,0,ierr)
         call tntoci (ROTSTO,rcpr,icpr,vc)
         call vcplvc (MCHNUM(1,2),vc,gclp,1.d0)
         call copyn (savp,STONUM(1,3),3)
         call cpyrot (savr,ROTSTO)
         if (ierr .ne. 0 .or. icpr(3) .ne. MCHPLN) go to 150
c
c............Valid circle
c............Calculate tangency point
c
         call copyn (TLVEC,gvec,3)
         go to 1000
c
c...Motion record
c
      else if (ITYPE .eq. 5000) then
         inc  = 1
  500    call copyn (CLPT(inc),gclp,3)
         if (NPT .eq. 3) then
            call copyn (TLVEC,gvec,3)
         else
            call copyn (CLPT(inc+3),gvec,3)
         end if
         if (ndist(gclp,MCHNUM(1,2)) .lt. PPTOLR(1) .and.
     -       ndist(gvec,TLVEC) .lt. 1.74d-5) then
            inc = inc + NPT
            if (inc .gt. MXCL) go to 100
            go to 500
         end if
      else
         go to 100
      end if
 1000 kflg   = 1
c
 8000 call clsamp (-1)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: tntoci (grot,gcpr,kcpr,gvec)
c
c     FUNCTION:  Get direction vector tangent to the circle at the first
c                point in the part coordinates.
c
c     INPUT:   grot     R*8  D20.2 -  Rotary axes at start point
c
c              gcpr     R*8  D25   -  circle parameters (like RCIPRM)
c
c              kcpr     I*4  D9    -  circle parameters (like ICIPRM)
c
c     OUTPUT:  gvec     R*8  D3    -  direction vector at start point
c
c***********************************************************************
c
      subroutine tntoci (grot,gcpr,kcpr,gvec)
c
      include 'post.inc'
c
      integer*4 kcpr(8)
      real*8 grot(20,2),gcpr(25),gvec(3)
c
      equivalence (IRTNUM,KPOSMP(1243)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IRTWRK(20)
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      real*8 anv, a36, a
      integer*4 i
c
      data a36 /360.d0/
c
c...get vector tangent to circle at start point
c
      if (kcpr(5) .eq. 2) then
         anv = gcpr(5) + 90.0
      else
         anv = gcpr(5) - 90.0
      end if
      if (anv .ge. a36) anv = anv - a36
      if (anv .lt. 0.0) anv = anv + a36
      gvec(kcpr(1)) = dcos (anv/RAD)
      gvec(kcpr(2)) = dsin (anv/RAD)
      gvec(kcpr(3)) = 0.d0
c
c...translate vector to CL coordinate system
c
      do 310 i=1,IRTNUM,1
         a = a36 - grot(i,1)
         call vecadj (gvec,gvec,a,IRTWRK(i))
  310 continue
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: alnmck (kflg)
c
c     FUNCTION:  This routine checks if move meets all requirements to
c                apply align positioning (ALIGN/MOVE) command and
c                calculates necessary align points coordinates.  Use
c                alngen to retrieve positioning points.
c
c     INPUT:   kflg     I*4  D1  -  inital value = 0.
c
c     OUTPUT:  kflg     I*4  D1  -  0 = positioning mode canceled
c                                   n = created 'n' align positioning
c                                       points.
c
c***********************************************************************
      subroutine alnmck (kflg)
c
      include 'post.inc'
c
      integer*4 kflg
c
      equivalence (ISIDBL,KPOSMP(0812)), (IBPOST,KPOSMP(0830))
      equivalence (IFLNRS,KPOSMP(1731)), (IRPALT,KPOSMP(3196))
      equivalence (IRPTAX,KPOSMP(3197)), (IRAP  ,KPOSMP(3199))
      equivalence (IRAPDO,KPOSMP(3220))
      equivalence (ALNCUT,KPOSMP(4040)), (ALNMOV,KPOSMP(4045))
c
      integer*4 IRAP,ALNMOV,ALNCUT,ISIDBL,IRAPDO(8),IRPALT,IFLNRS,
     -          IBPOST(2),IRPTAX
c
      equivalence (ALNBXS,POSMAP(0243))
      equivalence (MCHNUM,POSMAP(1287)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (ROTBAS,POSMAP(1435))
      equivalence (TBPOST,POSMAP(4585)), (BLDSTO,POSMAP(3592))
      equivalence (ALNMPS,POSMAP(4461)), (ALNAXS,POSMAP(4570))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),ROTBAS(4),ALNAXS,AXSOUT(10),TBPOST(4),
     -       TLVEC(3),BLDSTO,ALNMPS(3),ROTANG(20,2),ALNBXS(10)
c
      integer*4 ier,icut,ifl,isd
      real*8 pt(3),vec(3),rmch(3,4),laxs(6),raxs(10),rang(20,2),
     -       dlt,bang(3),vpt(3),stpt(3),strt(20,2),stvc(3)
c
      if (IRAP .eq. 0) ALNMOV = 2
c
c...get old position
c
      call gtlspt (stpt,stvc,strt)
c
c...Look ahead for 3-rd point
c
      call alnpos (pt,vec,kflg)
      if (kflg .eq. 0) go to 8000
c
c...define machine axes for 3-rd point
c
      call copyn (pt,rmch(1,2),3)
      call tlaxis (rmch,vec,laxs,raxs,rang,ROTBAS,0,0,ier)
c
c...Check if ALIGN/CUT active
c
      icut   = 0
      if (ALNCUT .ne. 0) then
         call alnck1 (MCHNUM(1,2),pt,AXSOUT,raxs,ALNAXS,ALNBXS,icut)
         if (icut .ne. 0) then
            call alladr (ALNAXS,laxs,rmch,rang,vec,5,1)
            call copyn (rmch(1,2),pt,3)
         end if
      end if
      call vcplvc (pt,MCHNUM(1,2),vpt,-1.d0)
      call unitvc (vpt,vpt)
c
c...# 2 or 3
c...push curent (plunge) point on stack
c
      call pshaxs (AXSOUT,TLVEC)
c
c...# 1 or 2
c...make point above current point
c...for this point blade angle is calculated using
c...'rmch(1,2)' and 'rang' as last defined
c
      isd    = ISIDBL
      call bldchk1 (rmch(1,2),vec,rang,vpt,strt,bang,dlt,
     -              isd,ifl)
      if (ifl .eq. 1) then
         TBPOST(1) = bang(1)
         TBPOST(2) = bang(3)
         TBPOST(3) = bang(2)
         IBPOST(1) = isd
c        ISIDBL = isd
c        BLDSTO = bang(2)
      end if
      call vcplvc (MCHNUM(1,2),TLVEC,MCHNUM(1,2),ALNMPS(1))
      call alladj (MCHNUM,laxs,AXSOUT,ROTANG,2,5)
c
c...none or 1
c...check if RP mode is modified (Z & XY) than make point
c...over last point at the same hight (ALNMPS(1))
c
      if (IRAP .ne. 0 .and. IRPALT*IRPTAX .eq. 5 .and.
     -                      ALNMPS(1) .gt. 0.) then
            call pshaxs (AXSOUT,TLVEC)
            call vcplvc (stpt,stvc,rmch(1,2),ALNMPS(1))
            call alladj (rmch,laxs,AXSOUT,strt,2,5)
            kflg  = 3
      else
         kflg  = 2
      end if
      IFLNRS = 1
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: smoout
c
c     FUNCTION:  This routine checks if move meets all requirements to
c                apply align positioning (ALIGN/MOVE) command and
c                calculates necessary align points coordinates.  Use
c                alngen to retrieve positioning points.
c
c     INPUT:   kflg     I*4  D1  -  inital value = 0.
c
c     OUTPUT:  kflg     I*4  D1  -  0 = positioning mode canceled
c                                   n = created 'n' align positioning
c                                       points.
c
c***********************************************************************
      subroutine smoout
c
      include 'post.inc'
c
      equivalence (SMOKAN,KPOSMP(4072)), (SMONRN,KPOSMP(4075))
      equivalence (SMONCD,KPOSMP(4076)), (SMOORN,KPOSMP(4079))
      equivalence (SMOOCD,KPOSMP(4080)), (SMOKDO,KPOSMP(4083))
      equivalence (SMOSEL,KPOSMP(4084))
c
      integer*4 SMOKAN,SMONCD(3),SMONRN,SMOORN,SMOOCD(3),SMOKDO,
     -          SMOSEL
c
      equivalence (SMORAN,POSMAP(4464)), (SMONVL,POSMAP(4450))
      equivalence (SMOOVL,POSMAP(4453))
c
      real*8 SMORAN,SMONVL(3),SMOOVL(3)
c
      integer*4 i
c
      SMONVL(1) = 0.
      SMONVL(2) = 0.
      SMONVL(3) = SMORAN
c
c...output smooth_on
c
      if (SMOKDO .eq. 1) then
         call clrbuf
         do 110 i=1,SMONRN
            call codout (SMONCD(i),SMONVL(i))
  110    continue
         call codout (SMONCD(3),SMONVL(3))
         call clrbuf
         SMOKDO = 2
c
c...output smooth_off
c
      else if (SMOKAN .eq. 2 .and. SMOKDO .eq. 2) then
         call clrbuf
         do 120 i=1,SMOORN
            call codout (SMOOCD(i),SMOOVL(i))
  120    continue
         call clrbuf
         SMOKDO = 0
         SMOKAN = 0
      end if
c
      return
      end

c***********************************************************************
c
c     SUBROUTINE: smopts (gclp,gvec,kflg)
c
c     FUNCTION:  Get 3-rd point for SMOOTH command (if FWD vector
c                undefined) and get direction tangent to motion at
c                curent point.
c
c     INPUT:   none
c
c     OUTPUT:  gclp     R*8  D3   -  CL point coordinates
c
c              gvec     R*8  D3   -  tangency vector
c
c     INPUT:   kflg     I*4  D1   -  1 = 3-rd point is set, 0 = command
c                                    has been canceled.
c
c***********************************************************************
c
      subroutine smopts (gclp,gvec,kcir,kflg)
c
      include 'post.inc'
c
      integer*4 kflg,kcir
      real*8 gclp(3),gvec(3)
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (MCHPLN,KPOSMP(1261)), (IRTWRK,KPOSMP(1506))
      equivalence (SMOKAN,KPOSMP(4072))
c
      integer*4 ITYPE,ISUBT,MXCL,NPT,MCHPLN,SMOKAN,IRTNUM,IRTWRK(20)
c
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (PPTOLR,POSMAP(1274))
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387))
      equivalence (RCIPRM,POSMAP(2049)), (CIRBPP,POSMAP(4845))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 CLPT(240),CIRBUF(7),MCHNUM(3,4),TLVEC(3),STONUM(3,4),
     1       RCIPRM(25),ROTSTO(20,2),ROTANG(20,2),
     2       CIRBPP(7),PPTOLR(10)
c
      real*8 ndist,savp(3),rcpr(25),savr(20,2),vc(3),ang
c
      integer*4 icpr(8),isw,ierr,inc,i
c
      kflg   = 0
      kcir   = 0
      isw    = 0
  100 call clsamp (isw)
  150 isw    = 1
c
c...Post word
c
      if (ITYPE .eq. 14000) go to 8000
      if (ITYPE .eq. 2000 .or. ITYPE .eq. 14000) then
         if (ISUBT .eq. 5) go to 8000
         if (ISUBT .eq. 1085) then
             call smooth
             if (SMOKAN .eq. 2) go to 8000
         end if
         go to 100
      else if (ITYPE .eq. 3000) then
         call clsamp (1)
         if (ITYPE .ne. 5000) go to 150
c
c............Check for valid circle
c............Calculate tangency vector
c
         call copyn (STONUM(1,3),savp,3)
         call copyn (MCHNUM(1,3),STONUM(1,3),3)
         call cpyrot (ROTSTO,savr)
         call cpyrot (ROTANG,ROTSTO)
         call cirdir (CIRBUF,CIRBPP,rcpr,icpr,0,ierr)
         call tntoci (ROTSTO,rcpr,icpr,vc)
         call copyn (savp,STONUM(1,3),3)
         call cpyrot (savr,ROTSTO)
         if (ierr .ne. 0 .or. icpr(3) .ne. MCHPLN) go to 150
c
c...Valid circle
c...adjust back to part coordinates
c
         do 250 i=1,IRTNUM,1
            ang = 360.d0 - ROTSTO(i,1)
            call vecadj (vc,vc,ang,IRTWRK(i))
  250    continue
         call copyn (vc,gvec,3)
         kcir = 1
         go to 1000
c
c...Motion record
c
      else if (ITYPE .eq. 5000) then
         inc  = 1
  500    call copyn (CLPT(inc),gclp,3)
         if (ndist(gclp,MCHNUM(1,2)) .lt. PPTOLR(1)) then
            inc = inc + NPT
            if (inc .gt. MXCL) go to 100
            go to 500
         end if
      else
         go to 100
      end if
 1000 kflg   = 1
c
 8000 call clsamp (-1)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: smock1 (gsto,gmch,gxnew,gxbac,kflg)
c
c     FUNCTION:  Get 3-rd point for SMOOTH command and get
c                direction tangent to motion at curent point
c
c     INPUT:   none
c
c     OUTPUT:  kflg     I*4  D1   -  CL point coordinates
c
c***********************************************************************
c
      subroutine smock1 (gsto,gmch,gxnew,gxbac,kflg)
c
      include 'post.inc'
c
      real*8 gsto(3),gmch(3),gxnew(10),gxbac(10)
      integer*4 kflg
c
      equivalence (IFWSFL,KPOSMP(0833))
      equivalence (ALNMOD,KPOSMP(4049))
c
      integer*4 IFWSFL,ALNMOD
c
      equivalence (ALNDIS,POSMAP(4444)), (SMODIS,POSMAP(4465))
      equivalence (FWDDIR,POSMAP(4580)), (FWDSAV,POSMAP(4591))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 SMODIS,ROTSTO(20,2),FWDSAV(3),FWDDIR(3),ALNDIS(3)
c
      real*8 pt(3),vec(3),rmch(3,4),lnax(6)
      integer*4 icir
c
c...get 3-rd point if FWD vector undefined
c
      if (IFWSFL .eq. 0) then
         call smopts (pt,vec,icir,kflg)
         if (kflg .ne. 0) then
            if (icir .eq. 0) then
c
c...Calculate tangency vector using 3 points
c...making circle
c
               call makeci (gsto,gmch,pt,vec)
               if (ALNMOD .eq. 2) call copyn (vec,FWDSAV,3)
            end if
            call vcplvc (gsto,vec,rmch(1,2),SMODIS)
            call alladj (rmch,lnax,gxnew,ROTSTO,2,5)
         end if
c
c...Expanded GT record, use FWD vectors
c
      else
         call vcplvc (gsto,FWDSAV,rmch(1,2),SMODIS)
         call alladj (rmch,lnax,gxnew,ROTSTO,2,5)
         call vcplvc (gmch,FWDDIR,rmch(1,2),(-ALNDIS(1)))
         call alladj (rmch,lnax,gxbac,ROTSTO,2,5)
         kflg = 1
      end if
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: makeci (gpt1,gpt2,gpt3,gvec)
c
c     FUNCTION:  Make circle thru 3 points and calculate vector
c                tangent to the circle at point 1.  Points can linear
c                but can not be multiple.
c
c     INPUT:   gpt1     R*8  D3   -  point 1 coordinates
c
c              gpt2     R*8  D3   -  point 2 coordinates
c
c              gpt3     R*8  D3   -  point 3 coordinates
c
c     OUTPUT:  gvec     R*8  D3   -  tangency vector at point 1.
c
c***********************************************************************
c
      subroutine makeci (gpt1,gpt2,gpt3,gvec)
c
      real*8 gpt1(3),gpt2(3),gpt3(3),gvec(3)
c
      real*8 pln(4),vc1(3),vc2(3),v1(3),v2(3),c1(3),c2(3),cen(3),
     -       ln1(3),ln2(3),ndot
c
      integer*4 ier
c
c...Get circle plane
c
      call vcplvc (gpt2,gpt1,vc1,-1.d0)
      call vcplvc (gpt3,gpt2,vc2,-1.d0)
      call unitvc (vc1,v1)
      call unitvc (vc2,v2)
      call plnlnp (v1,v2,gpt1,pln,ier)
c
c...segments are ||
c
      if (ier .ne. 0) then
         go to 8000
      else
         call unitvc (pln,pln)
c
c...bisect segments
c
         call vcplvc (gpt1,vc1,c1,.5d0)
         call vcplvc (gpt2,vc2,c2,.5d0)
         call crosvc (v1,pln,ln1)
         call crosvc (v2,pln,ln2)
c
c...intersect bisectors
c
         call unitvc (ln1,ln1)
         call unitvc (ln2,ln2)
         call islnln (c1,ln1,c2,ln2,cen,ier)
         if (ier .ne. 0) go to 8000
c
c...get starting vector of circle
c...and tangent to circle at this point
c...For fututre needs: 'cen' is center of circle,
c...dsqrt(ndot(c1,c1)) is radius of circle (after
c...next line!)
c
         call vcplvc (gpt1,cen,c1,-1.d0)
         call crosvc (c1,pln,v1)
         call unitvc (v1,v1)
c
c...make sure direction is toward the first point
c
         if (ndot(v1,vc1) .lt. 0.) call vcplvc (v1,v1,v1,-2.d0)
      end if
c
 8000 call copyn (v1,gvec,3)
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: gtfwvc (gpt1,gvc1,gpt2,gvc2,gvec)
c
c     FUNCTION:  Calculate FWD vector using 2 points or tool vector
c                displacement if points are same.
c
c     INPUT:   gpt1     R*8  D3   -  First CL point coordinates
c
c              gvc1     R*8  D3   -  First point tool vector
c
c              gpt2     R*8  D3   -  Second CL point coordinates
c
c              gvc1     R*8  D3   -  Second point tool vector
c
c     OUTPUT:  gvec     R*8  D3   -  Forward direction vector
c
c***********************************************************************
c
      subroutine gtfwvc (gpt1,gvc1,gpt2,gvc2,gdir,kflg)
c
      real*8 gpt1(3),gpt2(3),gvc1(3),gvc2(3),gdir(3)
      integer*4 kflg
c
      real*8 ndist,d
c
c...Get FWD vector in part system of coordinates
c...If no motion then FWD vector is change of the
c...tool vector (if any) but NOTE, if tool vector creates
c...vector oriented backward this may cause problem with
c...blade side orientation!
c
      kflg = 0
      d      = ndist (gpt2,gpt1)
      if (d .lt. 1.d-5) then
          d = ndist (gvc2,gvc1)
          if (d .lt. 1.d-5) go to 8000
          gdir(1) = (gvc2(1) - gvc1(1)) / d
          gdir(2) = (gvc2(2) - gvc1(2)) / d
          gdir(3) = (gvc2(3) - gvc1(3)) / d
      else
          gdir(1) = (gpt2(1) - gpt1(1)) / d
          gdir(2) = (gpt2(2) - gpt1(2)) / d
          gdir(3) = (gpt2(3) - gpt1(3)) / d
      end if
      kflg = 1
c
 8000 return
      end
