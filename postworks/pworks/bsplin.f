c
c***********************************************************************
c
c   FILE NAME:  bsplin
c   CONTAINS:
c               bsplin bspout bspout_ctp lmtchk_bsp bspck bspck_span
c               bsp_shift bsp_gen bsp_fin bsp_tolin bsp_recap
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        bsplin.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:28:22
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  bsplin
c
c   FUNCTION:  This is the controlling routine for circular interpola-
c              tion and helical records.
c
c   INPUT:  klook   I*4  D1  -  Look ahead flag.  0 = End of look ahead,
c                               1 = Look for next continuation or circle
c                               record, 2 = Circle record encountered
c                               during look ahead, check for following
c                               motion record.
c
c   OUTPUT: none.
c
c***********************************************************************
      subroutine bsplin (klook)
c
      include 'post.inc'
      integer*4 klook
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (ICYCSW,KPOSMP(0271)), (IRAP  ,KPOSMP(3199))
      equivalence (ISBSPL,KPOSMP(4085)), (NPBSPL,KPOSMP(4086))
      equivalence (NPBSCV,KPOSMP(4087))
c
      integer*4 ITYPE,ISUBT,MXCL,NPT,ICYCSW(2),ISBSPL,NPBSPL,
     -          IRAP,NPBSCV
c
      equivalence (CLPT  ,POSMAP(0491))
c
      real*8 CLPT(240)
c
      integer*4 ierr,npsv,ifl,inc,mxsv
c
      character*80 msg
c
      npsv = NPBSPL
      ifl  = 0
c
c...motion record
c
      if (ITYPE .eq. 5000 .and. ISUBT .eq. 3) go to 8000
      if (klook .ne. 0) then
         if (ITYPE .eq. 5000) then
            ierr = 0
ccc           if (ISUBT .eq. 6) then
               call genlst (msg,ierr)
               if (ierr .ne. 0) go to 9200
ccc           end if
            go to 300
         endif
c
c...not motion record
c
c
c.....changed for added level
c.....Yurong 5/26/98
c
c         call clumrk
         call clumrk(1)
         call bsp_fin (klook)
         klook = 0
         if (IRAP .ne. 0) call raprst
         go to 8000
      end if
c
c...Check for active cycle
c
      if (ICYCSW(1) .ne. 0) then
ccc         call psterr (2,'CYCCIR',' ',-1)
         go to 9100
      endif
c
c...collect points, check bspline
c
  300 call bspck (ifl,inc,ierr)
      if (ierr .ne. 0) go to 9000
c
c.....changed for added level
c.....Yurong 5/26/98
c
c      call clmark
      call clmark(1)
      klook  = 2
      if (NPBSPL .lt. 3) klook = 1
 8000 return
c
c...error in bspline
c
 9000 if (inc .ge. 4) then
         if (NPBSPL .ne. 0) call bspout (NPBSCV)
c        NPBSPL = npsv - NPBSPL + 2
cc         NPBSPL = MXCL + 1
         NPBSPL = NPBSPL - inc    + 2
         call bsp_shift (NPBSPL,inc-1)
         ifl  = 1
         if (NPBSPL .gt. 2) go to 300
      else
         mxsv   = MXCL
         call bsp_tolin
         MXCL   = mxsv
         if (NPBSPL .gt. 1) then
            NPBSPL = NPBSPL - 1
            call bsp_shift (NPBSPL,2)
            ifl  = 1
            if (NPBSPL .gt. 2) go to 300
         end if
      end if
c
c.....changed for added level
c.....Yurong 5/26/98
c
c      call clmark
      call clmark(1)
      klook  = 1
      go to 8000
c
 9100 ISBSPL = 0
      klook  = 0
      call mocntl
      go to 8000
c
 9200 call errkil (msg,ierr)
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bspout (knum)
c
c   FUNCTION:  This routine outputs b-spline interpolation block.
c
c   INPUT:  knum   I*4  D1  -  number of control points in the (common)
c                              array containing b-spline data.
c
c   OUTPUT: none.
c
c***********************************************************************
      subroutine bspout (knum)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 knum
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLOUT,KPOSMP(0084))
      equivalence (REGFRC,KPOSMP(0603))
      equivalence (MOTFLG,KPOSMP(1073)), (MOTCOD,KPOSMP(1240))
      equivalence (ISBSPL,KPOSMP(4085)), (NPBSPL,KPOSMP(4086))
      equivalence (NPBSCV,KPOSMP(4087)), (BSPCOD,KPOSMP(4089))
      equivalence (BSPPCD,KPOSMP(4090)), (BSPFRC,KPOSMP(4091))
c
      integer*4 NPBSPL,ISBSPL,NPBSCV,BSPPCD,REGFRC(MAXFMT),BSPCOD,
     -          BSPFRC(4),MOTCOD,ISN,ICLOUT,MOTFLG
c
      equivalence (MOTCDV,POSMAP(1284))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (BSPCDV,POSMAP(4901))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),
     -       ROTSTO(20,2),BSPCDV,MOTCDV
c
      equivalence (ISNBSP,KBSPMP(01))
      equivalence (ICLBSP,KBSPMP(06)), (NIXBSP,KBSPMP(11))
c
      integer*4 ISNBSP(5),ICLBSP(5),NIXBSP
c
      equivalence (BSPAN ,BSPMAP(07))
c
      real*8 BSPAN
c
      integer*4 i,ir,n, iary(10),icnt,imot,isns,icls,jknt,
     -          icod, istrt, kaxs(3)
      real*8 tval, tfct
c
      isns   = ISN
      icls   = ICLOUT
      ISN    = ISNBSP(1)
      ICLOUT = ICLBSP(1)
c
c...make sure G-code  will not be output
c
      imot   = MOTCOD
      call regtyp (imot,MOTCDV)
      if (imot .gt. 0 .and. REGFRC(imot) .eq. 1) REGFRC(imot) = 0
c
c...output bspline interpolation code
c
  100 n    = knum * 3 - 2
      ISBSPL = 1
      ir     = BSPCOD
      call regtyp (ir,BSPCDV)
      if (ir .gt. 0 .and. REGFRC(ir) .eq. 0) REGFRC(ir) = 1
      call codout (ir,BSPCDV)
      if (BSPFRC(2) .eq. 1) call clrbuf
c
c...check machine limits using input points
c
      call lmtchk_bsp
c
c...output b-spline points
c
      jknt = 0
      if (BSPFRC(3).eq.2) then
        istrt = 2
      else
        istrt = 1
        tfct = BSPTPR(n+4)
        if (tfct.gt.0.0d0) then
          tfct = 1.0d0/tfct
        else
          tfct = 1.0d0
        endif
      endif
      icod = BSPPCD
      do 505 i=istrt,n
          MOTFLG = 1
          call ptrtad (BSPCRV(1,i),MCHNUM(1,2),ROTSTO(1,1),1)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTSTO,2,5)
          call whchax (AXSOUT,iary,icnt)
c
c...Siemens format
c
          if (BSPFRC(3).eq.2) then
            jknt = jknt + 1
            if (jknt .eq. 1) then
               tval = 1.0
            else
               tval = 0.
            end if
            if (i .gt. n-2) icod = 0
            call codout (icod,tval)
            if (i .ne. n) then
                call bspout_ctp (AXSOUT,iary,icnt)
            else
                BSPAN = BSPTPR(i+3)
                call cpyrot (ROTSTO,ROTANG)
                call motion (iary,icnt)
            end if
            if (jknt .eq. 3) jknt = 0
c
c...Fanuc format
c
          else
            tval = BSPTPR(i)*tfct
            call codout (icod,tval)
c
c...Force out first point if required.
c
            if (BSPFRC(4).eq.1 .and.i.eq.istrt) then
              kaxs(1) = 1
              kaxs(2) = 1
              kaxs(3) = 1
              call frcmot(2,4,kaxs)
            endif
            call bspout_ctp (AXSOUT,iary,icnt)
          endif
  505 continue
      if (BSPFRC(3).eq.1) then
        do 510,i=n+1,n+4
          tval = BSPTPR(i)*tfct
          call codout (icod,tval)
          call clrbuf
  510   continue
      endif
      call clrmot(0)
c
c...force G-code in the next motion block
c
      if (BSPFRC(1) .eq. 1 .and. imot .gt. 0) then
         if (REGFRC(imot) .eq. 0) REGFRC(imot) = 1
      end if
c
      ISBSPL = 0
      ISN    = isns
      ICLOUT = icls
      MOTFLG = 0
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bspout_ctp (gmch)
c
c   FUNCTION:  This routine outputs motion registers containing single
c              b-spline control point.
c
c   INPUT:  gmch    R*8  D10 - machine axes
c
c   OUTPUT: none.
c
c***********************************************************************
      subroutine bspout_ctp (gmch,kfl,kcnt)
c
      include 'post.inc'
      integer*4 kfl(10),kcnt
      real*8 gmch(10)
c
      equivalence (MOTREG,KPOSMP(0381)), (INCR  ,KPOSMP(1226))
c
      integer*4 MOTREG(24),INCR
c
      integer*4 inc,i
c
      real*8 rnum
c
      inc   = 1
      do 500 i=1,6,1
         if (kfl(i) .eq. 1) then
c
c......Absolute mode
c
            if (INCR .eq. 1) then
               call codout (MOTREG(inc),gmch(i))
c
c......Incremental mode
c
            else
               call increg (MOTREG(inc),gmch(i),rnum)
               call setcod (MOTREG(inc),gmch(i))
               call codout (MOTREG(inc+1),rnum)
            endif
         endif
         inc    = inc + 2
  500 continue
c
      call clrbuf
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lmtchk_bsp
c
c   FUNCTION:  Checks input points used to generate bspline for machine
c              axis limits.  Multiple errors are suppresed since bspline
c              is output as a single record.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
      subroutine lmtchk_bsp
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (REGBNC,KPOSMP(2001))
      equivalence (NPBSPL,KPOSMP(4086))
c
      integer*4 MOTREG(24),REGBNC(MAXFMT),NPBSPL
c
      equivalence (LIMITS,POSMAP(1254)), (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425)), (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2),LIMITS(2,10),STONUM(3,4),LINSTO(6),
     -       AXSSTO(10)
c
      equivalence (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
c
      integer*4 i,j,icnt,iout(10),iosv(10),iers,iax(10)
      real*8 rmch(3,4),mlax(6),gaxs(10),rlow(10),rhig(10)
      character*1 lax(10)
      character*20 lbuf
      character*80 msg1,msg2
c
      data iax /1,3,5,7,9,11,13,16,19,22/
      data lax /'X','U','Y','V','Z','W','A','B','C','D'/
c
      iers = 0
c
c...check all points for limit violation
c
      do 405 i=2,NPBSPL,1
          call ptrtad (PTVEBP(1,i),rmch(1,2),ROTSTO(1,1),1)
          call alladj (rmch,mlax,gaxs,ROTSTO,2,5)
          call bsp_recap (rmch,gaxs,mlax,ROTSTO,STONUM,
     -                    AXSSTO,LINSTO,ROTSTO)
          if (i .le. NPBSPL) then
              call copyn (rmch,STONUM,12)
              call copyn (gaxs,AXSSTO,10)
              call copyn (mlax,LINSTO,6)
          end if
          call lmtchk (gaxs,icnt,iout,0)
          if (icnt .ne. 0) then
             if (iers .eq. 0) then
                call copynk (iout,iosv,10)
                call copyn (gaxs,rlow,10)
                call copyn (gaxs,rhig,10)
             else
                do 305 j=1,10
                   if (iosv(j) .eq. 0) then
                      iosv(j) = iout(j)
                      if (iout(j) .eq. 1) rlow(j) = gaxs(j)
                      if (iout(j) .eq. 2) rhig(j) = gaxs(j)
                   else if (iout(j) .ne. 0) then
                      if (iout(j) .ne. iosv(j)) iosv(j) = 3
                      if (iout(j) .eq. 1) rlow(j) = gaxs(j)
                      if (iout(j) .eq. 2) rhig(j) = gaxs(j)
                   end if
  305           continue
             end if
             iers = icnt
          end if
  405 continue
c
c...output all errors which are set
c
      if (iers .ne. 0) then
         do 605 j=1,10,1
            if (iosv(j) .eq. 0) go to 605
            lbuf = REGST(MOTREG(iax(j)))
            if (REGBNC(MOTREG(iax(j))) .eq. 0) lbuf = lax(j)
            if (iosv(j) .eq. 2) go to 550
c
c...lower limit
c
            call perrst ('LMTLWR',1,msg1,0,rlow(j),lbuf,3)
            call perrst ('LMTMSG',1,msg2,0,rlow(j),lbuf,2)
            call perrst (msg2,2,msg2,0,LIMITS(1,j),lbuf,2)
            call psterr (1,msg1,msg2,-1)
  550       if (iosv(j) .eq. 1) go to 605
c
c...upper limit
c
            call perrst ('LMTUPR',1,msg1,0,rhig(j),lbuf,3)
            call perrst ('LMTMSG',1,msg2,0,rhig(j),lbuf,2)
            call perrst (msg2,2,msg2,0,LIMITS(2,j),lbuf,2)
            call psterr (1,msg1,msg2,-1)
  605    continue
      end if
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bspck (knum,kerr)
c
c   FUNCTION:  Checks data points for b-spline legality. In particular:
c              1) if previous segment is tangent to the new segment,
c              2) if chord error of any segment based on linear interp.
c                 is higher than tolerance, 3) if tool vector has been
c                 changed.
c
c   INPUT:  knum    I*4  D1  -  Index of the erroneous point when CLPT
c                               array contains more than single point.
c
c           kerr    I*4  D1  -  Error status is set to 1 when illegal
c                               point encountered.
c
c   OUTPUT: none.
c
c***********************************************************************
      subroutine bspck (kflg,knum,kerr)
c
      include 'post.inc'
      integer*4 kflg,knum,kerr
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLOUT,KPOSMP(0084))
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (ISBSPL,KPOSMP(4085)), (NPBSPL,KPOSMP(4086))
      equivalence (NPBSCV,KPOSMP(4087))
c
      integer*4 NPBSPL,ISBSPL,NPBSCV,NPT,MXCL,ISN,ICLOUT
c
      equivalence (CLPT  ,POSMAP(0491)), (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (BSPTOL,POSMAP(4902))
c
      real*8 ROTSTO(20,2),ROTANG(20,2),STONUM(3,4),CLPT(240),
     -       VECSAV(3),BSPTOL(2)
c
      equivalence (ISNBSP,KBSPMP(01))
      equivalence (ICLBSP,KBSPMP(06)), (NIXBSP,KBSPMP(11))
c
      integer*4 ISNBSP(5),ICLBSP(5),NIXBSP
c
      integer*4 i, j, nx, ifxv, inv(10), ier1, ier2, nt, n, lst, npbsv
      real*8 ang,rmch(3,4),ptve(10,10),crv(3,30),t(30),tol,vse(6)
      real*8 ndist
c
c...check if last point is in buffer
c
      tol  = BSPTOL(2)
      kerr = 0
c
      if (NPBSPL .eq. 0) then
          call copyn (STONUM(1,3),PTVEBP(1,1),3)
          NPBSPL = 1
      end if
      ifxv    = 0
      lst     = NPBSPL
      npbsv   = NPBSPL
      nx      = 0
      if (kflg .eq. 1) go to 1000
c
c...set data for b-spline interpolation
c
      do 505 i=1,MXCL
          j   = (i-1) * NPT + 1
          call copyn (CLPT(j),rmch(1,2),3)
c
c...exit if tool vector changed
c
          if (NPT .eq. 6) then
             call betvec (VECSAV,CLPT(j+3),ang)
             if (ang .gt. .002) then
                knum = i
                go to 900
             end if
          end if
          call ptrtad (rmch(1,2),rmch(1,3),ROTSTO,0)
c
c...drop point if too close
c
          if (ndist(rmch(1,3),PTVEBP(1,lst)) .lt. .005) go to 505
          lst = lst + 1
          nx  = nx + 1
          call copyn (rmch(1,3),PTVEBP(1,lst),3)
          INVBP(lst) = ifxv
          if (ifxv .eq. 1) then
             call copyn (CLPT(240+j),PTVEBP(4,lst),3)
          end if
  505 continue
      NPBSPL = lst
c
c...Check if buffer contains minimum number of points to make
c...b-spline, try to join new point(s) to existing b-spline
c
  900 if (npbsv .gt. 2 .and. nx .gt. 0) then
          call copyn (PTVEBP(1,npbsv-2),ptve(1,1),40)
          call copynk (INVBP(npbsv-2),inv(1),4)
          n = 4
          call bspdef (n,1,inv,ptve,crv,t,vse,nt,ier1)
          if (ier1 .eq. 0) then
             call bspck_span (crv,nt,ptve,n,tol,ier2)
             if (ier2 .gt. 0) then
                knum = npbsv + 1
                go to 9000
             end if
          else
             knum = ier1
             go to 9000
          end if
      end if
cc      NPBSPL = lst
c
c...make b-spline
c
 1000 call bsp_gen (lst,knum)
      if (knum .ne. 0) go to 9000
      if (nx .gt. 0) then
         ISNBSP(1) = ISN
         ICLBSP(1) = ICLOUT
      end if
c
 8000 return
c
 9000 kerr = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bspck_span (crvpt,knum,ptve,kpt,gtol,kerr)
c
c   FUNCTION:  Check spans of created b-spline for chardal tolerance.
c
c   INPUT:  crvpt  R*8 D3.*   - b-spline control points.
c           knum   I*4  D1    - number of spans in b-spline.
c           ptve   R*8 D10.*  - b-spline interpolation points
c           kpt    I*4  D1    - number of b-spline interp. points
c           gtol   R*8  D1    - Chordal tolerance
c
c   OUTPUT: kerr   I*4  D1    - 0 = b-spline is OK, n = span index where
c                               spline is out of tolerance.
c
c***********************************************************************
      subroutine bspck_span (crvpt,knum,ptve,kpt,gtol,kerr)
c
      include 'post.inc'
      integer*4 knum,kpt,kerr
      real*8 crvpt(3,*),ptve(10,*),gtol
c
      integer*4 i,j,k1,k2,n,ic
c
      real*8 ndist,d1,d2,tlin(6),tlir(6),rd,q1(3),soff,
     -       bcrv(3,7),p1(3),v1(3),p2(3),v2(3),u
c
      kerr   = 0
      soff   = .15 * gtol
      n      = knum * 3 - 2
      i      = 1
c
c...When only 3 points are interpolated, they mostly are singular
c...points (separate GT records) and bspline has single span so
c...we need check chords for both segments
c
      if (n .eq. 4) then
         call copyn (ptve(1,1),tlin(1),3)
         call vcplvc (ptve(1,2),tlin(1),tlin(4),-1.d0)
         d1    = ndist (ptve(1,1),ptve(1,2))
         call unitvc (tlin(4),tlin(4))
         call copyn (ptve(1,2),tlir(1),3)
         call vcplvc (ptve(1,3),tlir(1),tlir(4),-1.d0)
         d2    = ndist (ptve(1,2),ptve(1,3))
         call unitvc (tlir(4),tlir(4))
c
         call split_bspl (crvpt,bcrv)
         do 55 j=2,6
            call nptln (bcrv(1,j),tlin,q1)
            if (ndist(tlin(1),q1) .le. d1) then
               call ptlnds (bcrv(1,j),tlin,rd)
               if (rd .gt. gtol) then
                  i = 1
                  go to 9000
               end if
            else if (ndist(tlir(1),q1) .lt. d2) then
               call ptlnds (bcrv(1,j),tlir,rd)
               if (rd .gt. gtol) then
                  i = 2
                  go to 9000
               end if
            end if
   55    continue
         go to 8000
      end if
c
c...When FIT reduced number of interpolation points in curve (crvpt 1, 4,
c...7 etc) so 'ptve' points are asynchroneous with bspline spans
c...we know that bspline is in tolerance used in bspdef. All we need is
c...the case when bspdef did not fail and 'crvpt' span matches 2 'ptve'
c...points (synchroneous) but control points (hance bspline) are off
c...the line segment based on these two 'ptve' points.
c
      ic     = 1
c
c...find the first ptve point which matches crvpt point
c
  100 d1 = ndist (ptve(1,i),crvpt(1,ic))
      if (d1 .gt. soff) then
         i = i + 1
         if (i .gt. kpt) go to 9000
         goto 100
      endif
c
c...find cl point at end of this curve segment
c
  110 j = i+1
  120 d2 = ndist (ptve(1,j),crvpt(1,ic+3))
      if (d2 .gt. soff) then
        j = j+1
        if (j.gt.kpt) goto 9000
        goto 120
      endif
      u = 0.d0
c
c...check 4 points on each cl point segment against curve segment.
c
      do k1=i,j-1
        call vcmnvc(ptve(1,k1+1),ptve(1,k1),v1)
        do k2=1,4
          d1 = k2
          d1 = d1/4.d0
          call vcplvc(ptve(1,k1),v1,p1,d1)
          call curvpv (p1,crvpt(1,ic),u,p2,v2,kerr)
          if (kerr.ne.0) goto 9000
          if (ndist(p1,p2).gt.gtol) goto 9000
        enddo
      enddo
      i  = j
      ic = ic+3
      if (ic.lt.n) goto 110
      goto 8000
c
 8000 return
c
c...return erroneous point index
c
 9000 kerr  = i
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bspck_span (crvpt,knum,ptve,kpt,gtol,kerr)
c
c   FUNCTION:  Check spans of created b-spline for chardal tolerance.
c
c   INPUT:  crvpt  R*8 D3.*   - b-spline control points.
c           knum   I*4  D1    - number of spans in b-spline.
c           ptve   R*8 D10.*  - b-spline interpolation points
c           kpt    I*4  D1    - number of b-spline interp. points
c           gtol   R*8  D1    - Chordal tolerance
c
c   OUTPUT: kerr   I*4  D1    - 0 = b-spline is OK, n = span index where
c                               spline is out of tolerance.
c
c***********************************************************************
      subroutine bspck_span_old (crvpt,knum,ptve,kpt,gtol,kerr)
c
      include 'post.inc'
      integer*4 knum,kpt,kerr
      real*8 crvpt(3,*),ptve(10,*),gtol
c
      integer*4 i,j,n,ic,ss
c
      real*8 ndist,d1,d2,tlin(6),tlir(6),rd,q1(3),q2(3),soff,
     -       bcrv(3,7)
c
      kerr   = 0
      soff   = .15 * gtol
      n      = knum * 3 - 2
      i      = 1
c
c...When only 3 points are interpolated, they mostly are singular
c...points (separate GT records) and bspline has single span so
c...we need check chords for both segments
c
      if (n .eq. 4) then
         call copyn (ptve(1,1),tlin(1),3)
         call vcplvc (ptve(1,2),tlin(1),tlin(4),-1.d0)
         d1    = ndist (ptve(1,1),ptve(1,2))
         call unitvc (tlin(4),tlin(4))
         call copyn (ptve(1,2),tlir(1),3)
         call vcplvc (ptve(1,3),tlir(1),tlir(4),-1.d0)
         d2    = ndist (ptve(1,2),ptve(1,3))
         call unitvc (tlir(4),tlir(4))
c
         call split_bspl (crvpt,bcrv)
         do 55 j=2,6
            call nptln (bcrv(1,j),tlin,q1)
            if (ndist(tlin(1),q1) .le. d1) then
               call ptlnds (bcrv(1,j),tlin,rd)
               if (rd .gt. gtol) then
                  i = 1
                  go to 9000
               end if
            else if (ndist(tlir(1),q1) .lt. d2) then
               call ptlnds (bcrv(1,j),tlir,rd)
               if (rd .gt. gtol) then
                  i = 2
                  go to 9000
               end if
            end if
   55    continue
         go to 8000
      end if
c
c...When FIT reduced number of interpolation points in curve (crvpt 1, 4,
c...7 etc) so 'ptve' points are asynchroneous with bspline spans
c...we know that bspline is in tolerance used in bspdef. All we need is
c...the case when bspdef did not fail and 'crvpt' span matches 2 'ptve'
c...points (synchroneous) but control points (hance bspline) are off
c...the line segment based on these two 'ptve' points.
c
      ic     = 1
c
c...find the first ptve point which matches crvpt point
c
  100 d1   = ndist (ptve(1,i),crvpt(1,ic))
      if (d1 .gt. soff) then
         i    = i + 1
         if (i .lt. kpt) go to 100
      else
c
c...found!, if next ptve point matches next crvpt point check this
c...span for tolerance.  Select more distant control point (crvpt)
c...from line segment
c
         d2 = ndist (ptve(1,i+1),crvpt(1,ic+3))
         if (d2 .lt. soff) then
            call copyn (ptve(1,i),tlin(1),3)
            call vcplvc (ptve(1,i+1),tlin(1),tlin(4),-1.d0)
            call ptlnds (crvpt(1,ic+1),tlin,d1)
            call ptlnds (crvpt(1,ic+2),tlin,d2)
            ss  = 2
            if (d1 .gt. d2) ss = 1
c
c...use '1/2 of de Casteljau' algorythm on that side
c...of segment to estimate chordal error
c
            call casteljau (crvpt(1,ic),ss,q2)
            call ptlnds (q2,tlin,rd)
            if (rd .gt. gtol) go to 9000
         end if
         ic    = ic + 3
         if (ic .lt. n) go to 100
      end if
c
 8000 return
c
c...return erroneous point index
c
 9000 kerr  = i
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bsp_shift (knum,kinx)
c
c   FUNCTION:  Shift b-spline data in common arrays to the first slot.
c
c   INPUT:  knum   I*4  D1    - number of points in PTVEBP,INVBP arrays
c                               to reposition.
c           kinx   I*4  D1    - index of the first point data to shift.
c
c   OUTPUT: none
c
c***********************************************************************
      subroutine bsp_shift (knum,kinx)
c
      include 'post.inc'
      integer*4 knum,kinx
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLOUT,KPOSMP(0084))
      equivalence (NPBSPL,KPOSMP(4086))
c
      integer*4 NPBSPL,ISN,ICLOUT
c
      equivalence (ISNBSP,KBSPMP(01))
      equivalence (ICLBSP,KBSPMP(06)), (NIXBSP,KBSPMP(11))
c
      integer*4 ISNBSP(5),ICLBSP(5),NIXBSP
c
      call copynk (INVBP(kinx),INVBP(1),knum)
      call copyn (PTVEBP(1,kinx),PTVEBP(1,1),knum*10)
      NPBSPL = knum
      ISNBSP(1) = ISN
      ICLBSP(1) = ICLOUT
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  bsp_gen (knum,kerr)
c
c   FUNCTION:  Main routine to create b-spline curve using buffered
c              data.  Created b-spline may use only part of buffer.
c
c   INPUT:  knum   I*4  D1    - number of points in PTVEBP,INVBP arrays
c                               to use for interpolation.
c           kerr   I*4  D1    - 0 = spline is OK,all points has been used
c                               n = index of the first point rejected
c                               due to tolerance or other error.
c
c   OUTPUT: none
c
c***********************************************************************
      subroutine bsp_gen (knum,kerr)
c
      include 'post.inc'
      integer*4 knum,kerr
c
      equivalence (NPBSPL,KPOSMP(4086)), (NPBSCV,KPOSMP(4087))
c
      integer*4 NPBSCV,NPBSPL
c
      equivalence (BSVCST,BSPMAP(01)), (BSVCND,BSPMAP(04))
      equivalence (BSPTOL,POSMAP(4902))
c
      real*8 BSVCST(3),BSVCND(3),BSPTOL(2)
c
      integer*4 ns,ier,ierr,n
      real*8 vec(6),tol
c
      tol  = BSPTOL(2)
      n    = knum
      ierr = 0
  100 call bspdef (n,1,INVBP,PTVEBP,BSPCRV,BSPTPR,vec,ns,ier)
      if (ier .eq. 0) call bspck_span(BSPCRV,ns,PTVEBP,n,tol,ier)
      n    = ier
      if (ier .ne. 0) ierr = ier
      if (ier .eq. 1) go to 8000
      if (n .gt. 2) go to 100
c
      NPBSCV = ns
      call copyn (vec,BSVCST,3)
      call copyn (vec(4),BSVCND,3)
c
 8000 kerr   = ierr
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bsp_fin (kflg)
c
c   FUNCTION:  Finish bspline interpolation when buffer contains some
c              number of points but spline was not created or output.
c
c   INPUT:  kflg   I*4  D1    - flag equivalent to 'klook' (1 - bspline
c                               is not created, 2 - bspline is created).
c   OUTPUT: none
c
c***********************************************************************
      subroutine bsp_fin (kflg)
c
      include 'post.inc'
      integer*4 kflg
c
      equivalence (ISBSPL,KPOSMP(4085))
      equivalence (NPBSPL,KPOSMP(4086)), (NPBSCV,KPOSMP(4087))
c
      integer*4 NPBSCV,NPBSPL,ISBSPL
c
      integer*4 npsv,ns,ierr
c
c...make sure that b-spline was created
c...using all points
c
      npsv   = NPBSPL
      if (kflg .eq. 2) then
         call bspout (NPBSCV)
      else
  100    if (npsv .gt. 2) then
            call bsp_gen (npsv,ierr)
            ns   = npsv - ierr
            if (ierr .eq. 1) then
               call bsp_tolin
            else
               call bspout (NPBSCV)
            end if
            npsv = ns
            go to 100
         else
            call bsp_tolin
         end if
      end if
      NPBSPL = 0
      ISBSPL = 0
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bsp_tolin
c
c   FUNCTION:  Output next point in bspline buffer as a linear motion.
c              Use when an error occured in bspline interpolation and
c              original CLPT point can not be available and bspline
c              buffer must be used to recover point coordinates.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
      subroutine bsp_tolin
c
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLOUT,KPOSMP(0084))
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (ISBSPL,KPOSMP(4085))
c
      integer*4 MXCL,NPT,ISBSPL,ISN,ICLOUT
c
      equivalence (CLPT  ,POSMAP(0491)), (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 CLPT(240),STONUM(3,4),VECSAV(3),ROTSTO(20,2)
c
      equivalence (ISNBSP,KBSPMP(01))
      equivalence (ICLBSP,KBSPMP(06)), (NIXBSP,KBSPMP(11))
c
      integer*4 ISNBSP(5),ICLBSP(5),NIXBSP
c
      real*8 rmch(3,4)
      integer*4 isns, icls
c
      ISBSPL = 0
      isns   = ISN
      icls   = ICLOUT
      ISN    = ISNBSP(1)
      ICLOUT = ICLBSP(1)
c
c...convert bspline buffer point to cl point
c
      call copyn (PTVEBP(1,2),rmch(1,3),3)
      call ptrtad (rmch(1,3),rmch(1,2),ROTSTO,1)
      call copyn (rmch(1,2),CLPT(1),3)
      if (NPT .eq. 6) call copyn (VECSAV,CLPT(4),3)
      MXCL   = 1
      call mocntl
c
      ISN    = isns
      ICLOUT = icls
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  bsp_recap (gmch,gmx,glx,grt,gmst,gms,gls,grs)
c
c   FUNCTION:  Set axes delta travel, minimum, maximum etc. variables
c              used in print file summary.
c
c   INPUT:  gmch  R*8  D3.4 - see MCHNUM
c
c           gmx   R*8  D10  - see AXSOUT
c
c           glx   R*8  D6   - see LINAXS
c
c           grt   R*8  D20.2 - see ROTANG
c
c           gmst  R*8  D3.4 - see STONUM
c
c           gms   R*8  D10  - see AXSSTO
c
c           gls   R*8  D6   - see LINSTO
c
c           grs   R*8  D4.2 - see ROTSTO
c
c   OUTPUT: none
c
c***********************************************************************
      subroutine bsp_recap (gmch,gmx,glx,grt,gmst,gms,gls,grs)
c
      include 'post.inc'
      real*8 gmch(3,4),gmx(10),glx(6),grt(20,2),gmst(3,4),gms(10),
     -       gls(6),grs(20,2)
c
      equivalence (LTHDIA,KPOSMP(1228)), (IRTINC,KPOSMP(1461))
      equivalence (IRTDEF,KPOSMP(1485))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
c
      integer*4 MTPDYN,LTMODE,LTHDIA(2),IRTINC(4),IRTDEF
c
      equivalence (MCHDLS,POSMAP(1221))
      equivalence (ROTDLS,POSMAP(1233)), (AXSDLS,POSMAP(1237))
      equivalence (MCHDLT,POSMAP(1474)), (ROTDLT,POSMAP(1486))
      equivalence (LINDLT,POSMAP(1490)), (AXSDLT,POSMAP(1496))
      equivalence (MCHMIN,POSMAP(1506)), (MCHMAX,POSMAP(1518))
      equivalence (ROTMIN,POSMAP(1530)), (ROTMAX,POSMAP(1534))
      equivalence (LINMIN,POSMAP(1538)), (LINMAX,POSMAP(1544))
      equivalence (AXSMIN,POSMAP(1550)), (AXSMAX,POSMAP(1560))
      equivalence (AXSDIS,POSMAP(1574)), (LINDLS,POSMAP(1594))
c
      real*8 MCHDLS(3,4),ROTDLS(4),AXSDLS(10),MCHDLT(3,4),ROTDLT(4),
     -       LINDLT(6),AXSDLT(10),MCHMIN(3,4),MCHMAX(3,4),ROTMIN(4),
     -       ROTMAX(4),LINMIN(6),LINMAX(6),AXSMIN(10),AXSMAX(10),
     -       AXSDIS(10),LINDLS(6)
c
      integer*4 i,j,ireg(10)
c
      real*8 mdis(3,4),rdis(4),ldis(6),adis(10),maxlin,maxrot,rout(10),
     1       rsto(10)
c
      data ireg /1,3,5,7,9,11,14,17,20,23/
c
c...Work with non-transformed axes
c
      call axsxfr (gmx,rout)
      call axsxfr (gms,rsto)
c
c...Accumulate individual axis distances &
c...Set minimum & maximum travel limits
c
      do 200 i=1,4,1
          do 100 j=1,3,1
              mdis(j,i) = dabs(gmch(j,i)-gmst(j,i))
              MCHDLS(j,i) = MCHDLS(j,i) + mdis(j,i)
              MCHDLT(j,i) = MCHDLT(j,i) + mdis(j,i)
              if (gmch(j,i) .lt. MCHMIN(j,i))
     1                MCHMIN(j,i) = gmch(j,i)
              if (gmch(j,i) .gt. MCHMAX(j,i))
     1                MCHMAX(j,i) = gmch(j,i)
  100     continue
          if (i .le. IRTDEF) then
              rdis(i) = dabs(grt(IRTINC(i),2)-grs(IRTINC(i),2))
              ROTDLS(i) = ROTDLS(i) + rdis(i)
              ROTDLT(i) = ROTDLT(i) + rdis(i)
              if (grt(IRTINC(i),2) .lt. ROTMIN(i))
     1            ROTMIN(i) = grt(IRTINC(i),2)
              if (grt(IRTINC(i),2) .gt. ROTMAX(i))
     1            ROTMAX(i) = grt(IRTINC(i),2)
           endif
  200 continue
c
      do 300 i=1,6,1
          ldis(i) = dabs(glx(i)-gls(i))
          LINDLS(i) = LINDLS(i) + ldis(i)
          LINDLT(i) = LINDLT(i) + ldis(i)
          if (glx(i) .lt. LINMIN(i)) LINMIN(i) = glx(i)
          if (glx(i) .gt. LINMAX(i)) LINMAX(i) = glx(i)
  300 continue
c
      maxlin = 0.d0
      maxrot = 0.d0
      do 400 i=1,10,1
          adis(i) = dabs(rout(i)-rsto(i))
          if (i .le. 6) then
              if (i .le. 2 .and. MTPDYN .eq. 2) then
                 if (LTMODE .eq. 0 .and. LTHDIA(2) .eq. 2)
     1                adis(i) = adis(i) / 2.
              end if
c             call increg (MOTREG(ireg(i)),rout(i),rnum)
c             AXSDIS(i) = dabs(rnum)
              AXSDIS(i) = adis(i)
              if (adis(i) .gt. maxlin) maxlin = adis(i)
          else
              AXSDIS(i) = adis(i)
              if (adis(i) .gt. maxrot) maxrot = adis(i)
          endif
          AXSDLS(i) = AXSDLS(i) + AXSDIS(i)
          AXSDLT(i) = AXSDLT(i) + AXSDIS(i)
          if (rout(i) .lt. AXSMIN(i)) AXSMIN(i) = rout(i)
          if (rout(i) .gt. AXSMAX(i)) AXSMAX(i) = rout(i)
  400 continue
c
      return
      end
c
c     subroutine bspsph_cnvs (knum,gvec,gseg)
c
c     include 'post.inc'
c     integer*4 knum,gvec(3,*),gseg(10,*)
c
c     equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
c
c     real*8 PI,RAD
c
c     real*8 a,b,c,ang,vc1(3),vc2(3)
c
c     integer*4 i,n
c
c...first point can be used as a base to create conversion
c...matrix from real vector to relative vector (0,0 point)
c
c     gseg(1,1) = 0.
c     gseg(2,1) = 0.
c     gseg(3,1) = 0.
c     do 205 i=2,knum
c        call betvec (gvec(1,i-1),gvec(1,i),ang)
c        r = ang / RAD
c...xxxxxxxxxxxxxxxxxxxxxxxx
c
c
c 205 continue
c     return
c     end
