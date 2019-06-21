c
c***********************************************************************
c
c   FILE NAME:  cirmsc
c   CONTAINS:
c               ciptad1  clptpp1  cirfin1  ciaxis1  cicent1  cirqad1
c               cirver   getadt   get_plan cirhl1   pcircl
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cirmsc.f , 24.3
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 18:02:04
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ciptad1 (gpti,gpto,grot,gvec)
c
c   FUNCTION:  This routine recovers original part program CL point from
c              preadjusted point coordinates if in LMDP mode, otherwise
c              converts preadjusted CL points to machine coordinates.
c
c   INPUT:  gpti    R*8  D3  -  Circle point in CL coordinate system.
c
c           grot    R*8  D20,2 -  Rotary axes position at this point.
c
c   OUTPUT: gpto    R*8  D3  -  Circle point in machine system or in
c                               part system if Lathe in LMDP mode.
c
c           gvec    R*8  D3  -  Tool axis vector at circle point in part
c                               system if ....
c
c***********************************************************************
c
      subroutine ciptad1 (gpti,gpto,grot,gvec)
c
      include 'post.inc'
      real*8 gpti(3),gpto(3),grot(20,2),gvec(3)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTMODE,KPOSMP(4125))
c
      integer*4 MACHTP,LTMODE
c
      if (MACHTP .eq. 4 .and. LTMODE .eq. 2) then
         call preadr (gpti,gpto,grot,gvec)
      else
         call ptrtad (gpti,gpto,grot,0)
         call ptxfm (gpto,gpto,1)
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clptpp1 (gpti,gpto,gvec)
c
c   FUNCTION:  This routine recovers original part program CL point from
c              saved CLPT coordinates if in LMDP mode, otherwise
c              converts preadjusted CL points to machine coordinates.
c
c   INPUT:  knum    I*4  D1  -  Start index of point in CLPT array.
c
c           grot    R*8  D8  -  Rotary axes position at this point.
c
c   OUTPUT: gpto    R*8  D3  -  Original CL point in part system, or
c                               adusted machine point.
c
c           gvec    R*8  D3  -  Tool axis vector at point in part
c                               system if multax is on.
c
c***********************************************************************
c
      subroutine clptpp1 (knum,gpto,grot,gvec)
c
      include 'post.inc'
      integer*4 knum
      real*8 gpto(3),grot(20,2),gvec(3)
c
      equivalence (NPT   ,KPOSMP(0059))
      equivalence (MACHTP,KPOSMP(1201)), (LTMODE,KPOSMP(4125))
c
      integer*4 MACHTP,LTMODE,NPT
c
      equivalence (CLPT  ,POSMAP(0491)), (CLPTPP,POSMAP(4605))
c
      real*8 CLPT(240),CLPTPP(240)
c
      if (MACHTP .eq. 4 .and. LTMODE .eq. 2) then
         call copyn (CLPTPP(knum),gpto,3)
         if (NPT .eq. 6) call copyn (CLPTPP(knum+3),gvec,3)
      else
         call ptrtad (CLPT(knum),gpto,grot,0)
         call ptxfm (gpto,gpto,1)
         if (NPT .eq. 6) call ptxfm (CLPT(knum+3),gvec,2)
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirfin1 (kprm,gprm)
c
c   FUNCTION:  This routine recovers original part program CL point from
c              saved CLPT coordinates if in LMDP mode, otherwise
c              converts preadjusted CL points to machine coordinates.
c
c   INPUT:  kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine cirfin1 (kprm,gprm)
c
      include 'post.inc'
      integer*4 kprm(8)
      real*8 gprm(25)
c
      equivalence (IZIGON,KPOSMP(0370)), (IZIGAD,KPOSMP(0371))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (ICIPR2,KPOSMP(1266)), (IRTOUT,KPOSMP(0813))
      equivalence (IHELIX,KPOSMP(1392)), (ICIDLT,KPOSMP(1741))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
      equivalence (ICRHEL,KPOSMP(4250))
c
      integer*4 ICRHEL(10),IHELIX,ICIPR2(8),MACHTP,ICIDLT,
     1          IRTOUT(4),IZIGON,IZIGAD,MTPDYN,LTMODE
c
      equivalence (ZIGDEP,POSMAP(0190)), (ZIGSTO,POSMAP(0191))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (ROTBAS,POSMAP(1435)), (HELANG,POSMAP(2213))
      equivalence (RCIPR2,POSMAP(2074))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),VECSAV(3),
     1       ROTANG(20,2),ROTSTO(20,2),HELANG(2),STONUM(3,4),ZIGDEP,
     2       RCIPR2(25),ZIGSTO,ROTBAS(4)
c
      real*8 h,mcp(3),vec(3),tmch(3,4),tlin(6),trot(20,2),taxs(10),
     1       vsav(3),rval
c
      integer*4 ierr,i
c
c...Set up final position for Lathe LMDP mode
c
      if (MACHTP .eq. 4 .and. LTMODE .eq. 2) then
         mcp(kprm(1)) = gprm(kprm(1)+7)
         mcp(kprm(2)) = gprm(kprm(2)+7)
         mcp(kprm(3)) = gprm(kprm(3)+7)
         call copyn (mcp,MCHNUM(1,1),3)
         call preadj (mcp,mcp,vec,TLVEC)
         call copyn (mcp,MCHNUM(1,2),3)
         call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,1,0,
     -                ierr)
      else
c
c...Set up final position for milling machines
c
         call cpyrot (ROTSTO,ROTANG)
         call copyn (VECSAV,TLVEC,3)
c
c...Set zigzag parameters if ZIGZAG active &
c...add blade angle if UCM is active
c
         if (IZIGON .ne. 0) then
             if (ICRHEL(1) .eq. 1) then
                call zigcir (gprm,kprm)
             else
                IZIGAD = 2
                ZIGSTO = ZIGDEP
             end if
         end if
         h      = ZIGDEP
         if (MACHTP .eq. 3 .and. IRTOUT(4) .eq. 1)
     -            call bldcir (gprm,kprm)
         ZIGDEP = h
         if (IZIGON .ne. 0) IZIGAD = 1
c
         if (kprm(3) .eq. 0) then
             if (IHELIX .ne. 0) then
                 rval = -(HELANG(1)*gprm(7))
                 call vcplvc (gprm(8),gprm(17),mcp,rval)
                 call copyn (mcp,gprm(8),3)
                 call vcplvc (RCIPR2(8),gprm(17),RCIPR2(8),rval)
                 call vcplvc (gprm(1),gprm(17),gprm(1),rval)
                 call copyn (gprm(1),RCIPR2(1),3)
             else
                 call copyn (gprm(8),mcp,3)
             endif
         else
             mcp(kprm(1)) = gprm(kprm(1)+7)
             mcp(kprm(2)) = gprm(kprm(2)+7)
c
             if (IHELIX .ne. 0 .and. ICRHEL(2) .eq. 1) then
                mcp(kprm(3)) = gprm(kprm(3)+7) - HELANG(1) * gprm(7)
                gprm(kprm(3)+7) = mcp(kprm(3))
                RCIPR2(kprm(3)+7) = mcp(kprm(3))
             else
                mcp(kprm(3)) = gprm(kprm(3)+7)
             endif
         endif
         call ptxfr (mcp,MCHNUM(1,3),1)
         if (MACHTP .eq. 2 .or. MACHTP .eq. 4) then
             vsav(1) = TLVEC(1)
             vsav(2) = TLVEC(2)
             vsav(3) = TLVEC(3)
         endif
         call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,2,1)
         call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,3,5)
c
c...Restore tool axis if in LATHE/MILL mode
c...Otherwise setpos adjusts it for XY-ZX xfrom
c...Which is not correct
c
         if (MACHTP .eq. 2 .or. MACHTP .eq. 4) then
             do 100 i=1,3,1
                 TLVEC(i) = vsav(i)
  100        continue
         endif
c
         if (ICIDLT .eq. 1 .and. IHELIX .ne. 0) then
             tmch(1,3) = MCHNUM(1,3)
             tmch(2,3) = MCHNUM(2,3)
             tmch(3,3) = MCHNUM(3,3)
             if (kprm(3) .ne. 0) tmch(kprm(3),3) = STONUM(kprm(3),3)
             call cpyrot (ROTSTO,trot)
             do 180 i=1,4,1
                 taxs(i+6) = AXSOUT(i+6)
  180        continue
             call alladr (taxs,tlin,tmch,trot,TLVEC,2,1)
             call alladj (tmch,tlin,taxs,trot,3,5)
             call tmpdlt (MCHNUM,LINAXS,ROTANG,AXSOUT,tmch,tlin,trot,
     1                    taxs,0)
         endif
      endif
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ciaxis1 (gbuf,grot)
c
c   FUNCTION:  This routine recovers ROTSTO angles for circle center
c              point in general, but when LMDP mode is active, center
c              point is in pp system and tool axis vector differs from
c              the circle first point so tlaxis must be called.
c
c   NOTE IF IN LMDP MODE: This routine can not be used after any circle
c                         record is output since base angles must
c                         correspond to the start point of circle, and
c                         base angles are not affected by this routine.
c
c   INPUT:  gbuf    R*8  D7  -  CL preadjusted circular record.
c
c           gbpp    R*8  D3  -  CL circular record
c
c   OUTPUT: grot    R*8  D20,2-  Rotary axes position for circle center.
c
c
c***********************************************************************
c
      subroutine ciaxis1 (gbuf,gbpp,grot)
c
      include 'post.inc'
      real*8 grot(20,2),gbuf(7),gbpp(7)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTMODE,KPOSMP(4125))
c
      integer*4 MACHTP,LTMODE
c
      equivalence (ROTBAS,POSMAP(1435)), (ROTSTO,POSMAP(5213))
c
      real*8 ROTBAS(4),ROTSTO(20,2)
c
      real*8 mcp(3,4),linx(6),maxs(10)
c
      integer*4 ierr
c
      if (MACHTP .eq. 4 .and. LTMODE .eq. 2) then
         call copyn (gbpp(1),mcp(1,1),3)
         call copyn (gbuf(1),mcp(1,2),3)
         call tlaxis (mcp,gbuf(4),linx,maxs,grot,ROTBAS,0,0,ierr)
      else
         call cpyrot (ROTSTO,grot)
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cicent1 (gprm,gcen)
c
c   FUNCTION:  This routine converts circle center point according to
c              machine type, specificly for LMDP mode it is in cylindri-
c              cal coordinate system.
c
c   INPUT:  gprm    R*8  D3  -  Circle center point (RCIPRM(1:3))
c
c   OUTPUT: gcen    R*8  D3  -  Converted circle center point.
c
c
c***********************************************************************
c
      subroutine cicent1 (gprm,gcen)
c
      include 'post.inc'
      real*8 gprm(3),gcen(3)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTMODE,KPOSMP(4125))
c
      integer*4 MACHTP,LTMODE
c
      equivalence (RAD   ,POSMAP(0002)), (PRTRAD,POSMAP(4604))
c
      real*8 RAD,PRTRAD
c
      if (MACHTP .eq. 4) then
         if (LTMODE .eq. 2) then
            gcen(1) = gprm(1)
            gcen(3) = gprm(3)
            gcen(2) = RAD * gprm(2) / PRTRAD
         else if (LTMODE .eq. 1) then
            call copyn (gprm,gcen,3)
         else
            call copyn (gprm,gcen,3)
         end if
      else
         call copyn (gprm,gcen,3)
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirqad1 (gprm,gpr1,kprm,kinx,knum,kflg)
c
c   FUNCTION:  This routine breaks circle arc (after it is broken
c              according to machine requirements) on intersections with
c              machine coordinate axes.  If brake points are created,
c              then consequtive calls are necessary to output all points.
c
c   INPUT:  gprm    R*8  D25 -  Original circle parameters (see RCIPRM)
c
c           gpr1    R*8  D25 -  Circle real parameters formated for
c                               output (curent circle arc).
c
c           kprm    I*4  D8  -  Integer circle parameters.
c
c           kflg    I*4  D1  -  Control flag: 0 - initial call, n - index
c                               of point to output.
c
c   OUTPUT: kinx    I*4  D4  -  Indeses of intersection points in storage
c                               according to output sequence.
c
c           knum    I*4  D1  -  Number of intersection points to output.
c
c           kflg    I*4  D1  -  Control flag for next calls (index of
c                               point to retrieve)
c
c***********************************************************************
      subroutine cirqad1 (gprm,gpr1,kprm,kinx,knum,kflg)
c
      include 'post.inc'
      real*8 gprm(25),gpr1(25)
      integer*4 kprm(8),knum,kflg,kinx(4)
c
      equivalence (MACHTP,KPOSMP(1201)), (LTMODE,KPOSMP(4125))
c
      integer*4 MACHTP,LTMODE
c
      equivalence (RCPRM6,POSMAP(4853))
      equivalence (CINAXS,POSMAP(4859)), (CINANG,POSMAP(4871))
      equivalence (ROTANG,POSMAP(5173)), (RCPRM6,POSMAP(4853))
c
      real*8 ROTANG(20,2),RCPRM6(6),CINAXS(12),CINANG(8)
c
      integer*4 i,n,k,is(4),npt
      real*8 ptm(3,4),ptin(12),axln(18),linx(6),mxout(10),ang(4),
     -       dlt(4)
c
      data is /1,7,13,1/
      data axln /.0,.0,.0, 1.,.0,.0,
     -           .0,.0,.0, .0,1.,.0,
     -           .0,.0,.0, .0,.0,1./
c
c...First time here: get all intersections of arc with axes
c
      if (kflg .eq. 0) then
         call copyn (gpr1(6),RCPRM6,5)
c
c...translate circle center to machine axis system
c
         call copyn (gprm,ptm(1,3),3)
         call alladj (ptm,linx,mxout,ROTANG,3,5)
         gpr1(1) = mxout(1)
         gpr1(2) = mxout(3)
         gpr1(3) = mxout(5)
c
c...intersect circle arc with machine XYZ axes
c
         n     = 0
         do 115 i=1,3,1
            kinx(i) = i
            if (kprm(3) .ne. i) then
               call copyn (axln(is(kprm(i))),linx,6)
               linx(kprm(3)) = gpr1(kprm(3))
               call isciln (gpr1,kprm,linx,ptin(n*3+1),npt)
               call cirver (gpr1,kprm,ptin(n*3+1),ang(n+1),dlt(n+1),npt)
               n = n + npt
            end if
  115    continue
         kinx(4) = 4
c
c...sort points by arc distance from start point
c...! note ! dlt is sorted but ang array is not sorted
c
         call sort (dlt,kinx,n)
         if (n .gt. 0) then
            call copyn (ptin,CINAXS,12)
            call copyn (ang,CINANG(1),4)
            call copyn (dlt,CINANG(5),4)
         end if
         knum   = n
         if (knum .gt. 0) kflg = knum + 1
      end if
c
c...put circle point on axis into gprm buffer
c
      if (kflg .ne. 0) then
         if (kflg .eq. 1) then
            call copyn (RCPRM6,gpr1(6),5)
            gpr1(7) = RCPRM6(2) - CINANG(4+knum)
            knum  = 0
         else
            k   = knum - kflg + 2
            gpr1(6) = CINANG(kinx(k))
            gpr1(7) = CINANG(4+k)
            if (kflg .le. knum) gpr1(7) = gpr1(7) - CINANG(3+k)
            call copyn (CINAXS(3*kinx(k)-2),gpr1(8),3)
         end if
         kflg = kflg - 1
      end if
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirver (gprm,kprm,gpt,gang,gdlt,knum)
c
c   FUNCTION:  This routine verifies if intersection point of circle and
c              line is within the circle arc span.
c
c   INPUT:  gprm    R*8  D25 -  Circle real parameters formated for
c                               output (curent circle arc).
c
c           kprm    I*4  D8  -  Integer circle parameters.
c
c           gpt     R*8  D6  -  Coordinates of points to verify.
c
c           knum    I*4  D1  -  Number of points to verify.
c
c   OUTPUT: knum    I*4  D1  -  Number of valid points on arc.
c
c           gang    R*8  D2  -  Angular position of valid pointis on arc.
c
c           gdlt    R*8  D2  -  Delta angle of points counted from the
c                               initial point of arc.
c
c*********************************************************************
      subroutine cirver (gprm,kprm,gpt,gang,gdlt,knum)
c
      include 'post.inc'
      real*8 gprm(25),gpt(6),gang(2),gdlt(2)
      integer*4 kprm(8),knum
c
      real*8 vec(3),ptin(3),an1,ang,an2,fan
c
      integer*4 i,n,k
c
      data fan /360.d0/
c
c...Get arc end point angle and recover
c...start point angle
c
      call vcplvc (gprm(8),gprm(1),vec,-1.d0)
      call vecang (vec,kprm(3),an2)
      if (kprm(5) .eq. 1) then
         an1 = an2 + gprm(7)
         if (an1 .ge. fan) an1 = an1 - fan
      else
         an1 = an2 - gprm(7)
         if (an1 .lt. 0.) an1 = an1 + fan
      end if
c
c...check all points
c
      n    = 0
      do 115 i=1,knum
         call copyn (gpt(i*3-2),ptin,3)
         call vcplvc (ptin,gprm,vec,-1.d0)
         call vecang (vec,kprm(3),ang)
c
c......verify point using its angle
c
         call intvec1 (ang,an1,gprm(7),kprm(5),k)
         if (k .eq. 1 .and. dabs(ang-an1) .gt. .001 .and.
     -                      dabs(ang-an2) .gt. .001) then
            n     = n + 1
            call copyn (ptin,gpt(n*3-2),3)
            call getadt (an1,ang,gdlt(n),kprm(5))
            gang(n) = ang
         end if
  115 continue
      knum   = n
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getadt (gan1,gang,gdlt,kdir)
c
c   FUNCTION:  This routine calculates absolute delta angle based on
c              direction of arc (start angle and end angle are on rotary
c              scale).
c
c   INPUT:  gan1    R*8  D1  -  Arc start angle.
c
c           gang    R*8  D1  -  Arc end angle.
c
c           kdir    I*4  D1  -  Direction of rotation (1 - CW, 2 - CCW)
c
c   OUTPUT: gdlt    R*8  D1  -  Delta angle from start angle.
c
c*********************************************************************
      subroutine getadt (gan1,gang,gdlt,kdir)
c
      real*8 gan1,gang,gdlt
      integer*4 kdir
c
      real*8 fan,ang
c
      data fan /360.d0/
c
      ang    = gang
      if (kdir .eq. 2) then
         if (ang .lt. gan1) ang = ang + fan
         gdlt = ang - gan1
      else
         if (ang .gt. gan1) ang = ang - fan
         gdlt = gan1 - ang
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:   get_plan (kprm,kfl,ksw)
c
c   FUNCTION:  This routine selects machine axes to be forced to output
c              when performing circular interpolation in any machine plan.
c
c   INPUT:  kprm    I*4  D3  -  Arc start angle.
c
c   OUTPUT: kfl     I*4  D1  -  flag for force routine: 3 = force
c                               selected axes output, 4 = force selected
c                               linear axes output.
c
c           ksw     I*4  D10 -  selected machine axes.
c
c*********************************************************************
c
      subroutine get_plan (kprm,kfl,ksw)
c
      include 'post.inc'
      integer*4 kprm(3),kfl,ksw(10)
c
      equivalence (LTMODE,KPOSMP(4125))
      equivalence (MODCYL,KPOSMP(4204)), (NCPLAN,KPOSMP(4226))
c
      integer*4 MODCYL,LTMODE,NCPLAN(3)
c
c...3-axis circular
c
      if (kprm(3) .eq. 0) then
          ksw(1) = 1
          ksw(2) = 1
          ksw(3) = 1
          kfl    = 4
      else
c
c...Mill/Turn
c
          if (LTMODE .eq. 2) then
             if (MODCYL .eq. 0) then
                ksw(1) = 1
                ksw(5) = 1
                ksw(7) = 1
             else
                ksw(6+MODCYL) = 1
                ksw(2*NCPLAN(2)-1) = 1
                ksw(2*NCPLAN(3)-1) = 0
             end if
             kfl  = 3
c
c...Standard mill/lathe
c
          else
             ksw(kprm(1)) = 1
             ksw(kprm(2)) = 1
             ksw(kprm(3)) = 0
             kfl  = 4
          end if
      endif
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirhl1 (gprm,kprm)
c
c   FUNCTION:  This routine updates the temporary delta arrays by
c              calling tmpdlt. Note that horizontal circular points
c              are calculated and used here (NOT helical) - delta Z
c              is updated by calling cirfin1 from cirout
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine cirhl1 (gprm,kprm)
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (CIRTOL,POSMAP(2201)), (ROTSTO,POSMAP(5213))
c
      real*8 RAD,CIRTOL(5),TLVEC(3),ROTSTO(20,2),AXSOUT(10)
c
      integer*4 kprm(8)

      real*8 gprm(25)
c
      integer*4 i,itim
c
      real*8 rnum,rinc,rtim
      real*8 trst(20,2),tast(10),rprm(25),rsgn(2),
     1       tmch(3,4),tlin(6),trot(20,2),taxs(10),tmst(3,4),tlst(6)
c
      data rsgn /-1.,1./

c
c...Initialize routine
c
      do 100 i=1,25,1
          rprm(i) = gprm(i)
  100 continue

      call cpyrot (ROTSTO,trot)
      call cpyrot (ROTSTO,trst)
      do 180 i=1,4,1
        taxs(i+6) = AXSOUT(i+6)
        tast(i+6) = AXSOUT(i+6)
  180 continue

      tmst(1,3) = gprm(8)
      tmst(2,3) = gprm(9)
      tmst(3,3) = gprm(10)
      call alladj (tmst,tlst,tast,trst,3,5)
      call alladr (tast,tlst,tmst,trst,TLVEC,2,1)
c
c...Calculate degree increment
c
      rnum    = (gprm(4)-0.5*CIRTOL(5)) / gprm(4)
      if (rnum .gt. 1.0d0) rnum = 1.0
      if (rnum .lt. -1.0d0) rnum = -1.0
      rinc   = dacos(rnum) * RAD * 2.0
      if (rinc .lt. 1.) rinc = 1.
      rtim   = dint(rprm(7)/rinc) + 1
      rinc   = rprm(7) / rtim
      itim   = rtim
c
c...Create circular points
c
      tmch(1,3) = tmst(1,3)
      tmch(2,3) = tmst(2,3)
      tmch(3,3) = tmst(3,3)

      do 500 i=1,itim,1
c
c......Calculate next point
c
          if (i .ne. itim) then
              rprm(5) = rprm(5) + rinc * rsgn(kprm(5))
              call cirpt (rprm,kprm,rprm(5),tmch(1,3))
          else
              tmch(1,3) = gprm(8)
              tmch(2,3) = gprm(9)
              tmch(3,3) = gprm(10)
          endif
c
c......Adjust points for output
c
          call alladj (tmch,tlin,taxs,trot,3,5)
          call alladr (taxs,tlin,tmch,trot,TLVEC,2,1)
c
c..... update circular delta arrays
c
          call tmpdlt (tmch,tlin,trot,taxs,tmst,tlst,trst,tast,1)

  500 continue

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pcircl (cmsg,kerr)
c
c   FUNCTION:  This routine processes the CIRCLE command and outputs a
c              circular interpolation record by calling cirout with the
c              input parameters.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pcircl (cmsg,kerr)
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICIPRM,KPOSMP(1230)), (ISCIRC,KPOSMP(1238))
      equivalence (IHELIX,KPOSMP(1392)), (ICIDLT,KPOSMP(1741))
c
      integer*4 ICIPRM(8),ISCIRC,IHELIX,ICIDLT,MXCL,IPSTWD(50)
c
      equivalence (PSTWD ,POSMAP(0441)), (CIRBUF,POSMAP(0731))
      equivalence (RCIPRM,POSMAP(2049)), (HELANG,POSMAP(2213))
c
      real*8 PSTWD(50),RCIPRM(25),HELANG(2),CIRBUF(7)
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i
c
c...Determine command syntax
c
      i      = MXCL   + 1
      if (MXCL .lt. 27) go to 9000
      do 100 i=1,5,1
          if (IPSTWD(i) .ne. 0) go to 9000
          ICIPRM(i) = PSTWD(i)
  100 continue
      if (IPSTWD(6) .ne. 0) go to 9000
      IHELIX = PSTWD(6)
      do 110 i=1,20,1
          if (IPSTWD(i+6) .ne. 0) go to 9000
          RCIPRM(i) = PSTWD(i+6)
  110 continue
      if (IPSTWD(27) .ne. 0) go to 9000
      HELANG(1) = PSTWD(27)
c
c...Store circle definition
c
      CIRBUF(1) = RCIPRM(1)
      CIRBUF(2) = RCIPRM(2)
      CIRBUF(3) = RCIPRM(3)
      if (ICIPRM(3) .eq. 0) then
          CIRBUF(4) = RCIPRM(17)
          CIRBUF(5) = RCIPRM(18)
          CIRBUF(6) = RCIPRM(19)
      else
          CIRBUF(ICIPRM(1)) = 0.
          CIRBUF(ICIPRM(2)) = 0.
          CIRBUF(ICIPRM(3)) = 1.
      endif
      CIRBUF(7) = RCIPRM(4)
c
c...Clear any held back motion
c
      call clrmot (1)
c
c...Calculate circular delta movements
c
      if (ICIDLT .eq. 1) then
          call tmpdrs
          call cirhl1 (RCIPRM,ICIPRM)
      endif
c
c...Output simulation circle
c
      call simcir (cmsg,kerr)
c
c....Output circular record
c
      ISCIRC = 1
      call cirout (RCIPRM,ICIPRM)
      ISCIRC = 0
      if (MXCL .gt. 27) go to 9100
c
c...End of routine
c
 8000 return
c
c...Number expected
c
 9000 call psterr (2,'NUMBEXP',' ',i)
      go to 8000
c
c...Too many parameters
c
 9100 call psterr (1,'INVPSYNW',' ',MXCL+1)
      go to 8000
      end
