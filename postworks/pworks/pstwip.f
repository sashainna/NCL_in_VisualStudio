c
c***********************************************************************
c
c   FILE NAME:  pstwip
c   CONTAINS:
c               leader  lintol  maxdpx  mchtol  mode    opstop  origin
c               pod     pprint  pptol
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pstwip.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        01/29/14 , 10:08:24
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  leader
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 LEADER/n
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine leader
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      integer*4 inc
c
c...LEADER/n
c
      inc    = 1
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 0) then
          if (PSTWD(inc) .le. 0.) go to 9400
          call ledout (PSTWD(inc))
c
c...Invalid minor word
c
      else
          go to 9300
      endif
      if (MXCL .gt. inc) go to 9700
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lintol
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 LINTOL/[RAPID,] n
c                                 ON
c                                 OFF
c
c                 LINTOL/ADJUST,ON ,tol [,ATANGL,ang] [,LENGTH,tl]
c                               OFF
c
c                 LINTOL/AXIS,tol
c
c                 LINTOL/LINEAR,dis
c
c                 LINTOL/TOLER,tol
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine lintol
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (LINTSW,KPOSMP(0114)), (MCHOPT,KPOSMP(0308))
      equivalence (LNRADJ,KPOSMP(1277))
c
      integer*4 MXCL,IPSTWD(50),MCHOPT(20),LINTSW,LNRADJ
c
      equivalence (METCNV,POSMAP(0004)), (DTOLER,POSMAP(0303))
      equivalence (PSTWD ,POSMAP(0441)), (LNRTOL,POSMAP(2251))
      equivalence (LNRATL,POSMAP(2286)), (VTOLER,POSMAP(4911))
c
      real*8 PSTWD(50),LNRTOL(2),METCNV,VTOLER,DTOLER,LNRATL(5)
c
      integer*4 inc,ist
c
c...Initialize routine
c
      inc    = 1
      ist    = 1
      if (MXCL .eq. 0) go to 9000
c
c...LINTOL/RAPID,n
c
      if (IPSTWD(inc) .eq. 5) then
          inc    = inc    + 1
          if (inc .gt. MXCL) go to 9500
          ist    = 2
      endif
c
c...LINTOL/n
c
      if (IPSTWD(inc) .eq. 0) then
          if (PSTWD(inc) .lt. 0.) go to 9400
          LNRTOL(ist) = PSTWD(inc) * METCNV
c
c...LINTOL/ON
c
      else if (IPSTWD(inc) .eq. 71) then
          LNRTOL(ist) = .001
          if (MCHOPT(2) .eq. 2) LNRTOL(ist) = .025
c
c...LINTOL/OFF
c
      else if (IPSTWD(inc) .eq. 72) then
          LNRTOL(ist) = 0.
c
c...LINTOL/ADJUST,ON/OFF
c
      else if (IPSTWD(inc) .eq. 159) then
          inc    = inc    + 1
          if (IPSTWD(inc) .eq. 71) then
              LNRADJ = 1
          else if (IPSTWD(inc) .eq. 72) then
              LNRADJ = 2
          else
              go to 9100
          endif
c
c......LINTOL/ADJUST,tol
c
  100     inc    = inc    + 1
          if (inc .le. MXCL) then
              if (IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  LNRATL(1) = PSTWD(inc)
c
c......LINTOL/ADJUST,ATANGL,ang
c
              else if (IPSTWD(inc) .eq. 1) then
                  if (inc .eq. MXCL) go to 9000
                  inc    = inc    + 1
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  LNRATL(2) = PSTWD(inc)
c
c......LINTOL/ADJUST,DELTA,ang
c
              else if (IPSTWD(inc) .eq. 188) then
                  if (inc .eq. MXCL) go to 9000
                  inc    = inc    + 1
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  LNRATL(3) = PSTWD(inc)
c
c......LINTOL/ADJUST,LENGTH,tl
c
              else if (IPSTWD(inc) .eq. 9) then
                  if (inc .eq. MXCL) go to 9000
                  inc    = inc    + 1
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  LNRATL(4) = PSTWD(inc)
              else
                  go to 9100
              endif
              go to 100
          endif

c
c...LINTOL/AXIS,toler
c
      else if (IPSTWD(inc) .eq. 132) then
          inc = inc + 1
          if (IPSTWD(inc) .ne. 0) go to 9000
          if (PSTWD(inc) .lt. 0.0) go to 9400
          VTOLER = PSTWD(inc)
c
c...LINTOL/LINEAR,dis
c
      else if (IPSTWD(inc) .eq. 76) then
          inc = inc + 1
          if (IPSTWD(inc) .eq. 0) then
             if (PSTWD(inc) .lt. 0.0) go to 9400
             DTOLER = PSTWD(inc) * METCNV
             LINTSW = 1
          else if (IPSTWD(inc) .eq. 72) then
             LINTSW = 0
          else
             go to 9100
          end if
c
c...LINTOL/TOLER,tol
c
      else if (IPSTWD(inc) .eq. 731) then
          inc = inc + 1
          if (IPSTWD(inc) .eq. 0) then
             if (PSTWD(inc) .lt. 0.0) go to 9400
             DTOLER = PSTWD(inc) * METCNV
             LINTSW = 2
          else if (IPSTWD(inc) .eq. 72) then
             LINTSW = 0
          else
             go to 9100
          end if
c
c...Invalid minor word
c
      else
          go to 9100
      endif
      if (MXCL .gt. inc) go to 9700
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Number expected
c
 9500 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  maxdpx
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 MAXDPM/n
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine maxdpx
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (PSTWD ,POSMAP(0441)), (MAXDPM,POSMAP(3545))
c
      real*8 PSTWD(50),MAXDPM
c
      integer*4 inc
c
c...MAXDPM/n
c
      inc    = 1
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 0) then
          if (PSTWD(inc) .lt. 0.) go to 9400
          MAXDPM = PSTWD(inc)
          if (MAXDPM .eq. 0.d0) MAXDPM = 99999999.
c
c...Invalid minor word
c
      else
          go to 9300
      endif
      if (MXCL .gt. inc) go to 9700
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mchtol
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 MCHTOL/cirtol(,cirdlt)
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mchtol
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (CIRTOL,POSMAP(2201))
c
      real*8 PSTWD(20),CIRTOL(5),METCNV
c
      integer*4 inc
c
c...MCHTOL/cirtol
c
      inc    = 1
      if (MXCL .lt. 1) go to 9000
      if (IPSTWD(1) .ne. 0) go to 9300
      if (PSTWD(1) .lt. 0.) go to 9400
c
c......MCHTOL/,cirdlt
c
      inc    = 2
      if (inc .le. MXCL) then
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .lt. 0.) go to 9400
          CIRTOL(2) = PSTWD(inc) * METCNV
      endif
c
      CIRTOL(1) = PSTWD(1) * METCNV
      if (MXCL .gt. inc) go to 9700
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mode
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 MODE/INCR,ON
c                           OFF
c
c                 MODE/-AXIS,n
c
c                 MODE/AXIS(,n1(,n2))
c
c                 MODE/LATHE
c                      MILL
c
c                 MODE/TOOL,XYPLAN
c                           ZXPLAN
c                           YZPLAN
c                           i,j,k
c
c                 MODE/BLADE,OFF
c                            ON , FRONT
c                                 REAR
c                                 BOTH
c
c                 MODE/ROTATE,ON
c                             OFF
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mode
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MULTAX,KPOSMP(0056)), (MOTREG,KPOSMP(0381))
      equivalence (IRTOUT,KPOSMP(0813)), (IBLDIR,KPOSMP(0834))
      equivalence (TAXCD ,KPOSMP(1081)), (NTAXCD,KPOSMP(1090))
      equivalence (MACHTP,KPOSMP(1201)), (LTHXY ,KPOSMP(1225))
      equivalence (NUMLIN,KPOSMP(1202)), (ACTLIN,KPOSMP(1205))
      equivalence (LNCALC,KPOSMP(1208))
      equivalence (INCR  ,KPOSMP(1226)), (INCRFC,KPOSMP(1227))
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (LASTAB,KPOSMP(1260)), (LTHXD ,KPOSMP(1339))
      equivalence (SPITAX,KPOSMP(1279)), (ACTBLN,KPOSMP(1336))
      equivalence (NCONTB,KPOSMP(1347)), (NROT  ,KPOSMP(1366))
      equivalence (ICMREG,KPOSMP(1393)), (IRTINC,KPOSMP(1461))
      equivalence (LTHDIA,KPOSMP(1228)), (IRDEAD,KPOSMP(1465))
      equivalence (IRTDEF,KPOSMP(1485)), (TURFL ,KPOSMP(1824))
      equivalence (LTHPCL,KPOSMP(1899)), (REGBNC,KPOSMP(2001))
      equivalence (MODBLD,KPOSMP(3984)), (MODROT,KPOSMP(4031))
      equivalence (MODRAT,KPOSMP(4033))
      equivalence (MILREG,KPOSMP(4106)), (LPCREG,KPOSMP(4108))
      equivalence (LTHREG,KPOSMP(4110))
      equivalence (LTMODE,KPOSMP(4125)), (LTHSVD,KPOSMP(4127))
      equivalence (LTHXYS,KPOSMP(4129))
      equivalence (MOTRGS,KPOSMP(4141)), (NUMLIS,KPOSMP(4133))
      equivalence (SRTNUM,KPOSMP(4140)), (SDFNUM,KPOSMP(4165))
      equivalence (LTCREG,KPOSMP(4195)), (MOTROT,KPOSMP(4153))
      equivalence (LTPREG,KPOSMP(4201)), (MODCYL,KPOSMP(4204))
      equivalence (LUNREG,KPOSMP(4205)), (ISRCYL,KPOSMP(4206))
      equivalence (ISRVAL,KPOSMP(4208))
      equivalence (PLNCOD,KPOSMP(4222)), (NCPLAN,KPOSMP(4226))
c
      integer*4 MXCL,IPSTWD(50),NUMLIN(3),ACTLIN(3),INCR,INCRFC,IRTNUM,
     1          IRDEAD(20),NROT,SPITAX,TAXCD(3,3),NTAXCD(3),MULTAX,
     -          MACHTP,IRTOUT(4),ACTBLN(3),LASTAB,MODROT,NCONTB,MODBLD,
     -          MOTREG(24),LTMODE,MILREG,TURFL(5),LTHSVD(2),LTHPCL(2),
     -          LTHDIA(2),LTHXY,LPCREG(2),LTCREG(3),LTHREG,MOTROT(12),
     -          LTPREG(2),ICMREG(3),MOTRGS(12),NUMLIS(3),SRTNUM,LTHXD,
     -          MODRAT,MODCYL,NCPLAN(3),ISRCYL,LUNREG,PLNCOD(4),
     -          REGBNC(66),ISRVAL,LTHXYS,IBLDIR,LNCALC(3),IRTINC(4)
      integer*4 IRTDEF,SDFNUM
c
      equivalence (RAD   ,POSMAP(0002)), (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (TAXVL ,POSMAP(1173))
      equivalence (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399)), (AXSSTO,POSMAP(1425))
      equivalence (ROTBAS,POSMAP(1435)), (PLNCDV,POSMAP(2208))
      equivalence (RTOLER,POSMAP(2400))
      equivalence (SPIVEC,POSMAP(3583)), (GLMCDV,POSMAP(4602))
      equivalence (TURDIS,POSMAP(3961)), (TRZX  ,POSMAP(3993))
      equivalence (PRTRAD,POSMAP(4604))
      equivalence (ZLEVEL,POSMAP(4852)), (MILTOL,POSMAP(4879))
      equivalence (TLMCDV,POSMAP(4880)), (TLLCDV,POSMAP(4881))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 PSTWD(50),SPIVEC(3),TAXVL(3,3),VECSAV(3),ROTSTO(20,2),
     -       GLMCDV(2),LINSTO(6),AXSSTO(10),STONUM(3,4),PRTRAD,
     -       TURDIS,TRZX(2),ZLEVEL,MILTOL,METCNV,TLMCDV,TLLCDV,
     -       RAD,ROTANG(20,2),TLVEC(3),RTOLER,ROTBAS(4),PLNCDV(4)
c
      integer*4 inc,isub,iaxs(6),is,ipt,i,num,ltsv,ierr,isav
      integer*4 iax(5),nay
c
      real*8 rary(3),rdis,zero(4),rnum
c
      character*80 lmsg,sbuf
c
      data iax /1,2,3,1,2/
      data zero /0.,0.,0.,0./
c
c...MODE/BLADE
c
      if (MXCL .eq. 0) go to 9000
      inc    = 1
      if (IPSTWD(1) .eq. 191) then
          if (MXCL .ne. 2 .and. MXCL .ne. 3) go to 9000
          if (MACHTP .ne. 3) go to 9600
          inc    = 2
c
c......MODE/BLADE,ON
c
          if (IPSTWD(inc) .eq. 71) then
c
c.........MODE/BLADE,ON,(BOTH,FRONT,REAR)
c
              if (MXCL .eq. 3) then
                  inc    = 3
                  if (IPSTWD(inc) .eq. 83) then
                      IBLDIR = 3
                  else if (IPSTWD(inc) .eq. 148) then
                      IBLDIR = 1
                  else if (IPSTWD(inc) .eq. 149) then
                      IBLDIR = 2
                  else
                      go to 9100
                  endif
              endif
              IRTOUT(4) = 1
              MODBLD = 1
c
c...vp 12/11/96 make sure blade axis contains last used angle
c...(rotary scale angle is OK but tlaxis resets linear value?)
c
              ROTSTO(4,2) = ROTSTO(4,1) + ROTBAS(4)
c
c......MODE/BLADE,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              if (MXCL .ne. 2) go to 9000
              IRTOUT(4) = 0
              MODBLD = 0
          else
              go to 9100
          endif
c
c...MODE/ROTATE,ON
c...            OFF
c
      else if (IPSTWD(1) .eq. 1066) then
          if (MXCL .gt. 3) go to 9000
          inc    = 2
          if (IPSTWD(inc) .eq. 0) go to 9300
          if (MXCL .eq. 1 .or. IPSTWD(inc) .eq. 71) then
             if (LASTAB .eq. 0) go to 9700
             MODROT = 1
             MODRAT = 0
             call perpto (num)
             if (num .eq. 0) go to 9800
             MODROT = num
             if (inc .eq. MXCL) go to 8000
             if (IPSTWD(MXCL) .ne. 0) go to 9300
             rdis = PSTWD(MXCL) * METCNV
             if (rdis .lt. 1.d-4) go to 9400
             RTOLER = rdis
          else if (IPSTWD(inc) .eq. 72) then
             MODROT = 0
             MODRAT = 0
c
c......ROTATE,AUTO,(n)
c
          else if (IPSTWD(inc) .eq. 88) then
             if (LASTAB .eq. 0) go to 9700
             MODROT = LASTAB
             MODRAT = 1
             if (inc .eq. MXCL) go to 8000
             if (IPSTWD(MXCL) .ne. 0) go to 9300
             rdis = PSTWD(MXCL) * METCNV
             if (rdis .lt. 1.d-4) go to 9400
             RTOLER = rdis
          else
             go to 9100
          end if
c
c...MODE/INCR
c
      else if (IPSTWD(1) .eq. 66) then
          if (MXCL .lt. 2) go to 9000
          inc    = 2
c
c......MODE/INCR,ON
c
          if (IPSTWD(inc) .eq. 71) then
              INCR   = 2
c
c......MODE/INCR,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              INCR   = 1
              if (INCRFC .eq. 2) then
                  call frcmot (1,2,iaxs)
              else if (INCRFC .eq. 3) then
                  call frcmot (2,2,iaxs)
              endif
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
          if (MXCL .gt. 2) go to 9200
c
c...MODE/-AXIS
c
      else if (IPSTWD(1) .ge. 84 .and. IPSTWD(1) .le. 86) then
          if (MXCL .lt. 2) go to 9000
          inc    = 2
          isub   = IPSTWD(1) - 83
          if (IPSTWD(inc) .ne. 0) go to 9300
          num    = PSTWD(inc)
          if (num .lt. 1) go to 9400
          if (num .gt. 1+(NUMLIN(isub)-1)*2) go to 9400
          if (num .eq. 3) then
              ACTBLN(isub) = 1
          else
              isav   = ACTLIN(isub)
              ACTLIN(isub) = num
              ACTBLN(isub) = 0
              if (num .eq. 1 .and. isav .eq. 2 .and.
     1            LNCALC(isub) .eq. 2)
     2                call alladj (STONUM,LINSTO,AXSSTO,ROTSTO,2,5)
          end if
          if (MXCL .gt. 2) go to 9200
c
c...MODE/AXIS
c
      else if (IPSTWD(1) .eq. 132) then
          iaxs(1) = 1
          iaxs(2) = 1
          iaxs(3) = 1
          iaxs(4) = 1
c
          isub   = MXCL
          if (isub .gt. 3) isub = 3
          do 500 inc=inc+1,isub,1
              if (IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .le. 0 .or. PSTWD(inc) .gt. IRTDEF)
     1                go to 9400
              is     = PSTWD(inc)
              iaxs(is) = 0
  500     continue
          inc    = 3
          if (MXCL .ge. 3 .and. PSTWD(2) .eq. PSTWD(3)) go to 9300
c
          IRDEAD(IRTINC(1)) = iaxs(1)
          IRDEAD(IRTINC(2)) = iaxs(2)
          IRDEAD(IRTINC(3)) = iaxs(3)
          IRDEAD(IRTINC(4)) = iaxs(4)
          NROT   = MXCL   - 1
          if (NROT .gt. 2) NROT = 2
          call rotbeg
          if (NCONTB .eq. 2) then
c             call getijk (ROTSTO,VECSAV)
              call alladr (AXSSTO,LINSTO,STONUM,ROTSTO,VECSAV,5,2)
          end if
          if (MXCL .gt. 3) go to 9200
c
c...MODE/TOOL
c
      else if (IPSTWD(1) .eq. 617) then
          inc    = 2
          if (inc .gt. MXCL) go to 9000
c
c......MODE/TOOL,XYPLAN
c
          if (IPSTWD(inc) .eq. 33) then
              SPITAX = 1
              SPIVEC(1) = 0.
              SPIVEC(2) = 0.
              SPIVEC(3) = 1.
c
c......MODE/TOOL,ZXPLAN
c
          else if (IPSTWD(inc) .eq. 41) then
              SPITAX = 2
              SPIVEC(1) = 0.
              SPIVEC(2) = 1.
              SPIVEC(3) = 0.
c
c......MODE/TOOL,YZPLAN
c
          else if (IPSTWD(inc) .eq. 37) then
              SPITAX = 3
              SPIVEC(1) = 1.
              SPIVEC(2) = 0.
              SPIVEC(3) = 0.
c
c......MODE/TOOL,i,j,k
c
          else
              if (IPSTWD(inc) .ne. 0) go to 9100
              if (inc+2 .gt. MXCL) go to 9000
              ipt    = 0
              do 800 inc=inc,inc+2,1
                  if (IPSTWD(inc) .ne. 0) go to 9300
                  ipt    = ipt    + 1
                  rary(ipt) = PSTWD(inc)
  800         continue
              inc    = inc    - 1
              rdis   = dsqrt(rary(1)**2 + rary(2)**2 + rary(3)**2)
              if (rdis .eq. 0.) go to 9400
              do 850 i=1,3,1
                  SPIVEC(i) = rary(i) / rdis
  850         continue
              SPITAX = 1
              if (dabs(SPIVEC(1)) .gt. dabs(SPIVEC(3))) SPITAX = 3
              if (dabs(SPIVEC(2)) .gt. dabs(SPIVEC(4-SPITAX)))
     1                SPITAX = 2
          endif
c
          call getijk (ROTSTO,VECSAV)
          call copyn (VECSAV,TLVEC,3)
c
c         if (MULTAX .ne. 1) then
c             VECSAV(1) = SPIVEC(1)
c             VECSAV(2) = SPIVEC(2)
c             VECSAV(3) = SPIVEC(3)
c         end if
c
c......Reinitialize rotary axes using new spindle vector
c
          call rotbeg
c
c......Output MODE/TOOL codes
c
          do 870 i=1,NTAXCD(SPITAX),1
              call codout (TAXCD(i,SPITAX),TAXVL(i,SPITAX))
  870     continue
          if (MXCL .gt. inc) go to 9200
c
c......MODE/MILL
c
      else if (IPSTWD(1) .eq. 151) then
          if (MACHTP .ne. 4) go to 9700
          ltsv = LTMODE
          inc  = 2
c
c......FACE - LMFP mode, switch axes/registers & adjust last
c......point to new system, so feed and time will be correct
c
          if (IPSTWD(inc) .eq. 81) then
             if (LTHPCL(1) .ne. 1) go to 9700
             if (LTMODE .ne. 1) then
                NUMLIN(2) = 1
                call copynk (LTPREG,MOTREG(5),2)
                ICMREG(2) = MOTREG(5)
                LTMODE = 1
                call copyn (STONUM(1,2),STONUM(1,1),3)
                ZLEVEL = STONUM(3,2)
                call preadj (STONUM,STONUM(1,2),VECSAV,VECSAV)
                call alladj (STONUM,LINSTO,AXSSTO,ROTSTO,2,5)
                IRTNUM = 0
                if (ltsv .eq. 0 .or. ltsv .eq. 3) LTHXYS = LTHXY
                LTHXY = 1
             end if
c
c......DIAMTR - LMDP mode, get part radius
c
          else if (IPSTWD(inc) .eq. 205) then
             if (LTHPCL(2) .ne. 1) go to 9700
             is     = 5
             if (MODCYL .ne. 0 .and. inc .lt. MXCL) then
                inc   = inc + 1
                if (IPSTWD(inc) .ne. 0) then
                   call parse_cyl1 (inc,NCPLAN,num,ierr)
                   if (ierr .ne. 0) go to 8000
                else
                   num = 1
                   inc = inc - 1
                end if
                SPIVEC(1) = 0.
                SPIVEC(2) = 0.
                SPIVEC(3) = 0.
                SPIVEC(NCPLAN(3)) = 1.
                MODCYL = num
                nay    = iax(NCPLAN(2)+2)
                if (nay .eq. NCPLAN(3)) nay = iax(NCPLAN(2)+1)
                is     = 4*nay - 3
             else
                SPIVEC(1) = 1.
                SPIVEC(2) = 0.
                SPIVEC(3) = 0.
             end if
             call copynk (MOTRGS,MOTREG(1),12)
             call nulreg (MOTREG(is),4,0)
             NUMLIN(2) = 0
             LTMODE = 2
             IRTNUM = SRTNUM
             IRTDEF = SDFNUM
             call getprad (PRTRAD)
c
c......save output R/D mode and set R mode
c
             call copynk (LTHDIA,LTHSVD,2)
             LTHDIA(1) = 1
             LTHDIA(2) = 1
             if (ltsv .eq. 0 .or. ltsv .eq. 3) LTHXYS = LTHXY
             LTHXY = 1
             call limadj (LTHDIA(2),LTHSVD(2))
             call copynk (LTCREG,MOTREG(13),3)
c
c......AUTO - set Y register and rotary axis
c
          else if (IPSTWD(inc) .eq. 88 .or. MXCL .lt. inc) then
             LTHDIA(1) = 1
             LTHDIA(2) = 1
             call rstlmod (LTMODE)
             LTMODE = 3
          end if
          call rscini
          call rotbeg
          call getijk (ROTSTO,VECSAV)
          call copyn (VECSAV,TLVEC,3)
c
c.......Output  M-code & G-code controling C axis
c
          if (LTMODE .gt. 0) then
             if (LTMODE .eq. 2 .and. MODCYL .ne. 0) then
                call clrbuf
                call usr_plane (NCPLAN,sbuf,num)
                call add_append (sbuf,num)
                call clrbuf
             end if
             if (MILREG .ne. 0) then
                call codout (MILREG,TLMCDV)
                call clrbuf
             end if
             if (LTMODE .ne. 3)
     -            call codout (LPCREG(LTMODE),GLMCDV(LTMODE))
             inc    = inc + 1
             if (inc .le. MXCL .and. MODCYL .eq. 0 .and. LTMODE .ne. 3)
     -       then
                if (IPSTWD(inc) .ne. 0) go to 9300
                MILTOL = PSTWD(inc) * METCNV
             end if
c
c...output user plane definition if specified
c...and enable cylindrical interpolation command
c
             if (LTMODE .eq. 2 .and. MODCYL .ne. 0) then
                rnum = PRTRAD / RAD
                call out_cyl_stat (rnum)
             end if
          end if
          if (ltsv .ne. LTMODE) call simsta (0,lmsg,ierr)
c
c...check for part XY/ZX plan conversion
c
          if (LTMODE .eq. 3) then
              LTHXY  = LTHXYS
              inc = inc - 1
              call parse_pplan (inc)
          end if
          if (MXCL .gt. inc) go to 9200
c
c...MODE/LATHE
c
      else if (IPSTWD(1) .eq. 700) then
          if (MACHTP .ne. 4) go to 9700
          ltsv = LTMODE
c
c...See if coming from LMFP mode and adjust last point & registers
c
          call copynk (MOTRGS,MOTREG(1),12)
c         call nulreg (MOTREG(5),2,0)
          NUMLIN(2) = 0
          if (LTMODE .eq. 1) then
             LTMODE = 0
             if (LTHXY .eq. 1) then
                STONUM(2,1) = STONUM(3,1)
                STONUM(3,1) = 0.0
             else
                STONUM(1,1) = STONUM(2,1)
                STONUM(2,1) = 0.0
             end if
             call preadj (STONUM,STONUM(1,2),VECSAV,VECSAV)
             call alladj (STONUM,LINSTO,AXSSTO,ROTSTO,2,5)
          end if
c
c...See if coming from LMDP mode and restore rotary axis
c
          if (LTMODE .eq. 2) then
             if (MODCYL .ne. 0) then
                call codout (LPCREG(LTMODE),GLMCDV(LTMODE))
                call out_cyl_stat (1.d0)
                MODCYL = 0
             end if
             call limadj (LTHSVD(2),LTHDIA(2))
             STONUM(1,2) = PRTRAD
             STONUM(2,2) = 0.
             call copyn (zero,ROTSTO(1,1),IRTNUM)
             call copyn (zero,ROTSTO(1,2),IRTNUM)
             call alladj (STONUM,LINSTO,AXSSTO,ROTSTO,2,5)
             call nulreg (MOTREG(13),3,0)
c
             SPIVEC(1) = 0.
             SPIVEC(2) = 0.
             SPIVEC(3) = 1.
             call rotbeg
             call getijk (ROTSTO,VECSAV)
             IRTNUM = 0
          else if (LTMODE .eq. 3) then
             call copyn (zero,AXSSTO(7),IRTNUM)
             call setpos
          end if
          LTMODE = 0
          LTHXY  = LTHXYS
          LTHDIA(1) = LTHSVD(1)
          LTHDIA(2) = LTHSVD(2)
          call parse_pplan (inc)
          call clrbuf
          call codout (LTHREG,TLLCDV)
          call clrbuf
          if (ltsv .ne. LTMODE) call simsta (0,lmsg,ierr)
          if (MXCL .gt. inc) go to 9200
c
c...Unrecognized command
c
      else
          if (IPSTWD(1) .eq. 0) go to 9500
          go to 9100
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9200 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
c
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9600 call psterr (2,'NOAXIS',' ',inc)
      go to 8000
c
c...Not valid command
c
 9700 call psterr (2,'NOTVALID',' ',inc)
      go to 8000
c
c...Tool vector not perpto table
c
 9800 call psterr (2,'NOTPERP',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  opskip
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 OPSKIP/ON
c                        OFF
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine opskip
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IOPSKP,KPOSMP(0092)), (OPKBLK,KPOSMP(3046))
      equivalence (IOPSON,KPOSMP(4051)), (NOPSON,KPOSMP(4050))
c
      integer*4 MXCL,IPSTWD(50),IOPSKP,OPKBLK,IOPSON(10),
     -          NOPSON
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      integer*4 inc,num,i,n,jindex
c
      if (MXCL .eq. 0) go to 9000
      inc    = 1
c
c...OPSKIP/ON
c
      if (IPSTWD(1) .eq. 71) then
          IOPSKP = 1
          do 115 i=inc+1,MXCL,1
             if (IPSTWD(i) .ne. 0) go to 9600
             num   = PSTWD(i)
             if (num .lt. 1 .or. num .gt. 9) go to 9700
             if (jindex(IOPSON,num,NOPSON) .eq. 0) then
                NOPSON = NOPSON + 1
                IOPSON(NOPSON) = num
             end if
  115     continue
c
c...OPSKIP/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          num    = 0
          do 215 i=inc+1,MXCL,1
             if (IPSTWD(i) .ne. 0) go to 9600
             num   = PSTWD(i)
             if (num .eq. 0) then
                 NOPSON = 0
                 if (i .ne. MXCL) go to 9200
             else
                 if (num .lt. 1 .or. num .gt. 9) go to 9700
                 n     = jindex(IOPSON,num,NOPSON)
                 if (n .ne. 0) then
                    IOPSON(n) = IOPSON(NOPSON)
                    NOPSON = NOPSON - 1
                 end if
             end if
  215     continue
          if (NOPSON .eq. 0 .or. num .eq. 0) then
              IOPSKP = 2
              NOPSON = 0
          end if
c
c......Force stop user defined block
c
          call pshblk (OPKBLK)
c
c...Unrecognized minor word
c
      else
          if (IPSTWD(inc) .eq. 0) go to 9500
          go to 9100
      endif
c     if (MXCL .gt. 1) go to 9200
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9200 call psterr (1,'INVPSYNW',' ',MXCL+1)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Number expected
c
 9600 call psterr (2,'NUMBREXP',' ',i)
      go to 8000
c
c...Invalid range
c
 9700 call psterr (2,'INPRANG',' ',i)
      go to 8000
c
c...Not active OPSKIP code
c
c9800 call perrst ('INVOPSOF',1,msg,num,0.d0,lbuf,1)
c     call psterr (2,msg,' ',i)
c     go to 8000
c
c...Minor word expected
c
 9900 call psterr (2,'NUMBREXP',' ',i)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  opstop
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 OPSTOP
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine opstop
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (OPSSEQ,KPOSMP(0306))
      equivalence (OPSTCD,KPOSMP(1075)), (IPRDES,KPOSMP(1154))
      equivalence (OPSBLK,KPOSMP(3047))
c
      integer*4 MXCL,OPSSEQ,OPSTCD,IPRDES(2,10),OPSBLK
c
      equivalence (OPSTVL,POSMAP(1168))
c
      real*8 OPSTVL
c
      integer*4 ierr
c
      character*80 lmsg
c
c...Output stop code
c
      if (MXCL .ne. 0) go to 9000
      call codout (OPSTCD,OPSTVL)
      call clrbuf
c
c...Force stop user defined block
c
      call pshblk (OPSBLK)
c
c...Output print file record
c
      call prtrec (IPRDES(1,7))
c
c...Set end of sequence
c
      call endseq (OPSSEQ)
c
c...Output Simulation record
c
      call simsta (2,lmsg,ierr)
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  origin
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 ORIGIN/x,y[,z]
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine origin
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MXTRAN,KPOSMP(4003))
c
      integer*4 MXCL,IPSTWD(50),MXTRAN
c
      equivalence (PSTWD ,POSMAP(0441)), (ORGIN ,POSMAP(1305))
      equivalence (METCNV,POSMAP(0004))
c
      real*8 PSTWD(50),ORGIN(3),METCNV
c
      integer*4 inc
c
c...Check command syntax
c
      if (MXCL .lt. 2) go to 9000
c
c...Loop through parameters
c
      do 100 inc=1,MXCL,1
          if (inc .gt. 3) go to 9100
          if (IPSTWD(inc) .ne. 0) go to 9200
          ORGIN(inc) = PSTWD(inc) * METCNV
  100 continue
      MXTRAN = 0
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Too many parameters
c
 9100 call psterr (1,'INVPSYNW',' ',inc)
      go to 8000
c
c...Number expected
c
 9200 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pod
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 POD/AIR,LOW
c                         HIGH
c                         ON
c                         OFF
c                         NOMORE
c                     LOCK,ON
c
c                 POD/DOWN
c
c                 POD/row,col,UP
c                             UP,CLAMP
c
c                 POD/row,col,CLAMP,ON
c                                   OFF
c
c                 POD/row,col,AIR,OFF
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pod
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (PODACD,KPOSMP(0372)), (PODSCD,KPOSMP(0818))
      equivalence (PODCCD,KPOSMP(0379))
c
      integer*4 MXCL,IPSTWD(50),PODACD(7),PODSCD(6),PODCCD(2)
c
      equivalence (PSTWD ,POSMAP(0441)), (PODAVL,POSMAP(2494))
      equivalence (PODSVL,POSMAP(3594))
c
      real*8 PSTWD(50),PODAVL(7),PODSVL(6)
c
      integer*4 inc,isw(3),k,nall,n1op(3),n2op(6),indx,jindex
c
      real*8 podrc(2)
c
      data n1op /1011, 114, 113/,  n2op / 63, 62, 53,  0, 71, 72/
c
c
      isw(1) = 0
      isw(2) = 0
      isw(3) = 0
c
c...Check command syntax
c
      if (MXCL .gt. 4) go to 9000
      nall   = 1
      indx   = 0
      inc    = 0
      go to 1000
c
c...Parameter 1 and 2
c
  100 if (inc .gt. 2) go to 300
c
c......Get Pod row & column number
c
      if (IPSTWD(inc) .eq. 0) then
          if (inc .eq. 2 .and. nall .ne. 0) go to 9500
          nall = 0
          podrc(inc) = PSTWD(inc)
c
c......All Pod commands
c......1-st word
c
      else if (inc .eq. 1) then
          k     = jindex (n1op,IPSTWD(inc),3)
          if (k .eq. 0) go to 9300
          isw(1) = k
          if (k .eq. 3) indx = 4
c
c......2-nd word
c
      else if (inc .eq. 2) then
          if (nall .eq. 0) go to 9200
          k      = jindex (n2op,IPSTWD(inc),6)
          if (k .eq. 0) go to 9300
          if (isw(1) .eq. 2 .and. k .ne. 5) go to 9300
          indx   = k
          if (isw(1) .eq. 1 .and. k .eq. 3) indx = 7
          if (isw(1) .eq. 2 .and. k .eq. 5) indx = 3
      end if
      go to 1000
c
c...Parameter 3-th
c
  300 if (inc .eq. 3) then
          if (IPSTWD(inc) .eq. 0) go to 9500
          if (nall .eq. 1) go to 9000
c
c......Selected Pod commands
c
          if (IPSTWD(inc) .eq. 112) then
              isw(2) = 2
              if (inc .eq. MXCL) indx = 2
          else if (IPSTWD(inc) .eq. 1060) then
              isw(2) = 4
          else if (IPSTWD(inc) .eq. 1011) then
              isw(2) = 6
          else
              go to 9300
          end if
c
c...Parameter 4-th
c
      else if (inc .eq. 4) then
          if (IPSTWD(inc) .eq. 0) go to 9500
          if (nall .eq. 1) go to 9100
c
c....../row,col,UP,CLAMP
c
          if (isw(2) .eq. 2) then
              if (IPSTWD(inc) .eq. 1060) then
                  indx = 3
              else
                  go to 9300
              end if
c
c....../row,col,CLAMP,ON/OFF
c
          else
              k = jindex(n2op(5),IPSTWD(inc),2)
              if (k .eq. 0) go to 9300
              if (isw(2) .eq. 4) then
                  indx = 4 + (k - 1)
c
c....../row,col,AIR,OFF
c
              else
                  if (k .ne. 2) go to 9300
                  indx = 6
              end if
          end if
      end if
c
c...Get next parameter
c
 1000 if (indx .eq. 0) then
          inc = inc + 1
          if (inc .gt. MXCL) go to 9500
          go to 100
      else
          if (MXCL .gt. inc) go to 9000
      end if
c
c...Output command
c
      if (nall .eq. 1) then
          call codout (PODACD(indx),PODAVL(indx))
      else
          call clrbuf
          call codout (PODSCD(1),PODSVL(1))
          call codout (PODCCD(1),podrc(1))
          call codout (PODCCD(2),podrc(2))
          call clrbuf
          call codout (PODSCD(indx),PODSVL(indx))
          call clrbuf
      end if
c
c...End of routine
c
 8000 return
c
c...Invalid command syntax
c
 9000 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Too many parameters
c
 9100 call psterr (1,'INVPSYNW',' ',inc)
      go to 8000
c
c...Number expected
c
 9200 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Invalid minor word
c
 9300 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pprint
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 PPRINT text
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pprint
c
      include 'post.inc'
c
      equivalence (ICUCHG,KPOSMP(0068)), (IDSPLY,KPOSMP(0091))
      equivalence (SIMACT,KPOSMP(0174)), (PPRIPV,KPOSMP(0175))
      equivalence (IPVCON,KPOSMP(0176)), (IPVCTR,KPOSMP(0177))
      equivalence (IPRDES,KPOSMP(1154)), (ITP   ,KPOSMP(1801))
c
      integer*4 IDSPLY,IPRDES(2,10),ITP,SIMACT,PPRIPV,IPVCON,IPVCTR,
     1          ICUCHG
c
      equivalence (LPSTWD,CPOSMP(0217)), (LPPRIN,CPOSMP(6499))
c
      character*512 LPSTWD,LPPRIN
c
      integer*4 nc,strlen1,inc,nindex,ierr
c
      character*80 lmsg
c
c...Store PPRINT text
c
      LPPRIN = LPSTWD
c
c...Check for Tool Description
c
      inc    = nindex(LPSTWD,' ')
      if (inc .ne. 0) then
          if (ITP .ne. 0 .and. (LPSTWD(inc:inc+3) .eq. 'TLN,' .or.
     1        LPSTWD(inc:inc+3) .eq. 'tln,'))
     2            TLNAME(ITP) = LPSTWD(inc+4:)
      endif
c
c......Check for PPRINT IPV SHANK/HOLDER
c
      if (SIMACT .eq. 1 .and. IPVCTR .eq. 0) then
          inc    = nindex(LPSTWD,' ')
          if (inc .ne. 0) then
              if ((LPSTWD(inc:inc+3) .eq. 'IPV ' .or.
     1            LPSTWD(inc:inc+3) .eq. 'ipv ') .or. IPVCON .eq. 2)
     2                then
                  inc    = inc + nindex(LPSTWD(inc+3:),' ')
                  if (inc .ne. 0) then
                      inc = inc + 2
                      if (LPSTWD(inc:inc+6) .eq. 'CUTTER ' .or.
     1                    LPSTWD(inc:inc+6) .eq. 'CUTTER ' .or.
     2                    LPSTWD(inc:inc+5) .eq. 'SHANK ' .or.
     3                    LPSTWD(inc:inc+5) .eq. 'shank ' .or.
     4                    LPSTWD(inc:inc+6) .eq. 'HOLDER ' .or.
     5                    LPSTWD(inc:inc+6) .eq. 'holder ' .or.
     6                    LPSTWD(inc:inc+4) .eq. 'TOOL ' .or.
     7                    LPSTWD(inc:inc+4) .eq. 'tool ') then
c
                          if (ICUCHG .eq. 1) call simcut (lmsg,ierr)
                          if (LPSTWD(inc:inc+4) .ne. 'TOOL ' .and.
     1                        LPSTWD(inc:inc+4) .ne. 'tool ') IPVCTR = 1
                          ICUCHG = 0
c
                      endif
                  endif
              endif
          endif
c
c...Check for PPRINT IPV
c
      else if ((PPRIPV .eq. 2 .or. IPVCON .eq. 2) .and. SIMACT .eq. 0)
     1        then
          inc    = nindex(LPSTWD,' ')
          if (inc .ne. 0) then
              if ((LPSTWD(inc:inc+3) .eq. 'IPV ' .or.
     1            LPSTWD(inc:inc+3) .eq. 'ipv ') .or. IPVCON .eq. 2)
     2                then
                  IPVCON = 0
                  nc     = strlen1(LPSTWD)
                  if (LPSTWD(nc:nc) .eq. '~') IPVCON = 1
                  go to 8000
              endif
          endif
      endif
c
c...Output message block
c
      IPVCON = 0
      nc     = strlen1(LPPRIN)
      if (IDSPLY .eq. 1) call msgout (LPPRIN,nc)
c
c...Output Simulation block
c
      call simdis (LPPRIN,nc,lmsg,ierr)
c
c...Output PPRINT record
c
      call prtrec (IPRDES(1,10))
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pptol
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 PPTOL/tol
c                 PPTOL/-AXIS,(n),tol, ...
c                 PPTOL/VECTOR,tol
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pptol
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (PPTOLR,POSMAP(1274))
      equivalence (TAXTOL,POSMAP(2262))
c
      real*8 METCNV,PSTWD(50),PPTOLR(10),TAXTOL
c
      integer*4 ist,inc,i,nprm,isub
c
c...PPTOL/tol
c
      if (MXCL .eq. 1) then
          inc    = 1
          if (IPSTWD(1) .ne. 0) go to 9100
          if (PSTWD(1) .lt. 0.) go to 9200
          do 100 i=1,10,1
              PPTOLR(i) = PSTWD(1) * METCNV
  100     continue
c
c...PPTOL/VECTOR,tol
c
      else if (IPSTWD(1) .eq. 604) then
          inc    = 2
          if (MXCL .lt. 2) go to 9100
          if (IPSTWD(2) .ne. 0) go to 9100
          if (PSTWD(2) .lt. 0.) go to 9200
          TAXTOL = PSTWD(2)
c
c...PPTOL/-AXIS(,n)
c
      else
          if (MXCL .lt. 2) go to 9400
          ist    = 1
c
c......Get next parameter section
c
  500     inc    = ist
          if (IPSTWD(ist) .eq. 0) go to 9000
          inc    = inc    + 1
          do 600 i=inc,MXCL,1
              if (IPSTWD(i) .ne. 0) go to 650
  600     continue
          i      = MXCL   + 1
  650     nprm   = i      - inc
c
c.........Invalid number of parameters
c
          if (nprm .lt. 1) then
              inc    = i
              go to 9100
          else if (nprm .gt. 2) then
              inc    = inc    + 2
              go to 9000
          endif
c
c.........Check parameter values
c
          isub   = 1
          if (nprm .eq. 2) then
              isub   = PSTWD(inc)
              inc    = inc    + 1
          endif
c
          if (PSTWD(inc) .lt. 0.) go to 9200
c
c......Set tolerance
c
          if (IPSTWD(ist) .eq. 84) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9200
              endif
              ist    = 0
          else if (IPSTWD(ist) .eq. 85) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9200
              endif
              ist    = 2
          else if (IPSTWD(ist) .eq. 86) then
              if (isub .ne. 1 .and. isub .ne. 2) then
                  inc    = inc    - 1
                  go to 9200
              endif
              ist    = 4
          else if (IPSTWD(ist) .eq. 132) then
              if (isub .lt. 1 .or. isub .gt. 4) then
                  inc    = inc    - 1
                  go to 9200
              endif
              ist    = 6
          else
              go to 9300
          endif
          ist    = ist    + isub
          PPTOLR(ist) = PSTWD(inc) * METCNV
          ist    = inc    + 1
          if (ist .le. MXCL) go to 500
      endif
c
c...End of routine
c
 8000 return
c
c...Minor word expected
c
 9000 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Number expected
c
 9100 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Input value out of range
c
 9200 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Invalid minor word
c
 9300 call psterr (2,'INVMINOR',' ',ist)
      go to 8000
c
c...Invalid command syntax
c
 9400 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getprad (kreg,knum,kflg)
c
c   FUNCTION:  This routine defines part radius in lathe mode.
c
c
c   INPUT:  none.
c
c   OUTPUT: grad   R*8   D1  - Part radius.
c
c***********************************************************************
c
      subroutine getprad (grad)
c
      include 'post.inc'
      real*8 grad
c
      equivalence (LTHXY ,KPOSMP(1225)), (LTHDIA,KPOSMP(1228))
      equivalence (LTHXD ,KPOSMP(1339)), (TURFL ,KPOSMP(1824))
c
      integer*4 LTHXY,LTHDIA(2),LTHXD,TURFL(2)
c
      equivalence (STONUM,POSMAP(1387))
      equivalence (TURDIS,POSMAP(3961)), (TRZX  ,POSMAP(3993))
c
      real*8 STONUM(3,4),TURDIS,TRZX(2)
c
      integer*4 inx
c
      inx    = 1
      if (LTHXY .eq. 1) inx = 2
      grad   = STONUM(inx,1) - TRZX(2)
      if (TURFL(2) .eq. 2) then
         if (LTHXD .eq. 1) then
             grad = grad + TURDIS
         else
             grad = grad - TURDIS
         end if
      end if
      if (LTHDIA(1) .eq. 2) grad = .5 * grad
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  nulreg (kreg,knum,kflg)
c
c   FUNCTION:  This routine nulifies specified registers.
c
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine nulreg (kreg,knum,kflg)
c
      include 'post.inc'
      integer*4 kreg(4),knum,kflg
c
      integer*4 i
c
      do 115 i=1,knum
         kreg(i) = kflg
  115 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rstlmod (kmod)
c
c   FUNCTION:  This routine resets LMFP/LMDP mode to MILL,AUTO mode.
c
c
c   INPUT:  kmod   I*4   D1  -  Current mode.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rstlmod (kmod)
c
      include 'post.inc'
      integer*4 kmod
c
      equivalence (MOTREG,KPOSMP(0381)), (NUMLIN,KPOSMP(1202))
      equivalence (LTHXY ,KPOSMP(1225))
      equivalence (LTHDIA,KPOSMP(1228)), (IRTNUM,KPOSMP(1243))
      equivalence (IRTDEF,KPOSMP(1485))
      equivalence (LPCREG,KPOSMP(4108)), (LTMODE,KPOSMP(4125))
      equivalence (LTHSVD,KPOSMP(4127)), (LTHXYS,KPOSMP(4129))
      equivalence (MOTRGS,KPOSMP(4141)), (NUMLIS,KPOSMP(4133))
      equivalence (SRTNUM,KPOSMP(4140)), (MOTROT,KPOSMP(4153))
      equivalence (SDFNUM,KPOSMP(4165))
      equivalence (MODCYL,KPOSMP(4204))
c
      integer*4 MOTREG(24),NUMLIN(3),LTHDIA(2),LTHSVD(2),LTMODE,
     -          IRTNUM,MOTRGS(12),NUMLIS(3),MOTROT(12),SRTNUM,
     -          LPCREG(2),MODCYL,LTHXY,LTHXYS,IRTDEF,SDFNUM
c
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399)), (AXSSTO,POSMAP(1425))
      equivalence (SPIVEC,POSMAP(3583)), (GLMCDV,POSMAP(4602))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 SPIVEC(3),VECSAV(3),STONUM(3,4),LINSTO(6),ROTSTO(20,2),
     -       AXSSTO(10),GLMCDV(2)
c
      real*8 zero(4)
      integer*4 mod
c
      data zero /0.,0.,0.,0./
c
c...when coming from LMDP mode
c...reset machine limits and cylidrical options
c
      mod  = kmod
      if (mod .eq. 2) then
         call limadj (LTHSVD(2),LTHDIA(2))
         if (MODCYL .ne. 0) then
            call codout (LPCREG(LTMODE),GLMCDV(LTMODE))
            call out_cyl_stat (1.d0)
            MODCYL = 0
         end if
      end if
      call copynk (MOTRGS,MOTREG(1),12)
      call copynk (MOTROT,MOTREG(13),12)
      call copynk (NUMLIS,NUMLIN,3)
      LTMODE = 3
      IRTNUM = SRTNUM
      IRTDEF = SDFNUM
      SPIVEC(1) = 1.
      SPIVEC(2) = 0.
      SPIVEC(3) = 0.
      call rotbeg
      if (mod .eq. 0) then
         VECSAV(1) = 1.
         VECSAV(2) = 0.
         VECSAV(3) = 0.
         call copyn (zero,ROTSTO(1,1),4)
         call copyn (zero,ROTSTO(1,2),4)
         call alladj (STONUM,LINSTO,AXSSTO,ROTSTO,2,5)
         LTHXYS = LTHXY
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  parse_cyl1 (kax,kerr)
c
c   FUNCTION:  This routine parses enhaced cylindrical interpolation
c              command.
c
c   INPUT:  none
c
c   OUTPUT: kax    I*4   D1  -  cylinder axis index (1=X,2=Y,3=Z)
c
c           kerr   I*4   D1  -  error status in command syntax.
c
c***********************************************************************
c
      subroutine parse_cyl1 (kinc,kplan,krot,kerr)
c
      include 'post.inc'
      integer*4 kinc,kplan(3),krot,kerr
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRTYPE,KPOSMP(1486)), (IRTWRK,KPOSMP(1506))
      equivalence (SRTNUM,KPOSMP(4140)), (SDFNUM,KPOSMP(1485))
c
      integer*4 MXCL,IPSTWD(20),IRTYPE(20),IRTWRK(20),SRTNUM,
     1          IRTINC(4),SDFNUM
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      integer*4 num,kax
c
      kerr   = 0
c
      if (IPSTWD(kinc) .eq. 0) go to 9000
      if (IPSTWD(kinc) .eq. 132) then
         kinc = kinc + 1
         if (IPSTWD(kinc) .ne. 0) go to 9300
         num = PSTWD(kinc)
         if (num .lt. 0 .or. num .gt. SDFNUM) go to 9200
         if (IRTYPE(IRTINC(num)) .ne. 1) go to 9400
         kax   = IRTWRK(IRTINC(num))
         krot  = num
         kinc  = kinc + 1
         if (IPSTWD(kinc) .eq. 0) go to 9000
         num   = IPSTWD(kinc) - 83
         if (num .lt. 0 .or. num .gt. 3) go to 9100
         if (num .eq. kax) go to 9600
         kplan(1) = kax + 3
         kplan(2) = kax
         kplan(3) = num
         if (kinc .gt. MXCL) go to 9500
      else
         go to 9100
      end if
c
 8000 return
c
c...Errors
c
 9000 call psterr (2,'MINOREXP',' ',kinc)
      go to 9900
c
 9100 call psterr (2,'INVMINOR',' ',kinc)
      go to 9900
c
 9200 call psterr (2,'INPRNG',' ',kinc)
      go to 9900
c
 9300 call psterr (2,'NUMBEXP',' ',kinc)
      go to 9900
c
 9400 call psterr (2,'TABCYL',' ',kinc)
      go to 9900
c
c...Too many parameters
c
 9500 call psterr (1,'INVPSYNW',' ',kinc+1)
      go to 9900
c
 9600 call psterr (2,'TABCYL1',' ',kinc)
c
 9900 kerr  = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  add_append (lbuf,knum)
c
c   FUNCTION:  This routine appends string to the appned buffer.
c
c
c   INPUT:  lbuf   C*n   D1  -  character string to append to APPEND
c
c           knum   I*4   D1  -  Number of characters in lbuf.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine add_append (lbuf,knum)
c
      include 'post.inc'
      integer*4 knum
      character*80 lbuf
c
      equivalence (NCAPPN,KPOSMP(0856))
c
      integer*4 NCAPPN
c
      equivalence (APPSTR,CPOSMP(5987))
c
      character*512 APPSTR
      character*512 tmpb
c
      integer*4 nc
c
      tmpb   = APPSTR
      nc     = NCAPPN
c
c...APPEND is limited to 512 characters, cut off
c...excessive characters
c
      if (knum + NCAPPN .gt. 512) nc = 512 - knum
      APPSTR = lbuf(1:knum) // tmpb(1:nc)
      NCAPPN = knum + nc
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fmtcod_app (kreg,gnum,kfl,cdat,knc)
c
c   FUNCTION:  This routine formats a code for appned buffer.
c
c   INPUT:  kreg    I*4  D1   -  Register to format for output.
c
c           gval    R*8  D1   -  Register value.
c
c           kfl     I*4  D1   -  Flag: 1 = output value with address,
c                                2 = format address only.
c
c   OUTPUT: cdat    C*n  D1   -  Text string representation of register
c                                and value.
c
c           knc     I*4  D1   -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine fmtcod_app (kreg,gnum,kfl,cdat,knc)
c
      include 'post.inc'
      integer*4 kreg,kfl,knc
      real*8 gnum
      character*80 cdat
c
      equivalence (PCHSPC,KPOSMP(1103))
c
      integer*4 PCHSPC
c
      equivalence (LEDCHR,CPOSMP(0986))
c
      character*1 LEDCHR
c
      integer*4 nreg,nc
      character*80 sbuf
c
      nreg = kreg
      if (kfl .eq. 2) nreg = -kreg
      call fmtcod (nreg,gnum,sbuf,nc)
c
c...separate register with a space
c
      if (nc .eq. 0) go to 8000
      if (PCHSPC .eq. 1) then
         cdat = sbuf(1:nc) // LEDCHR
         nc   = nc + 1
      else
         cdat = sbuf(1:nc)
      end if
 8000 knc   = nc
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  usr_plane (kpln,cdat,knc)
c
c   FUNCTION:  This routine formats user specified machine plane
c              definition codes  (Sinumerik 880).
c
c   INPUT:  kpln   I*4 D3 - Machine plane definition axes (see NCPLAN
c                           array specification).
c
c   OUTPUT: cdat   C*n D1 - string containing definition codes.
c
c           knc    I*4 D1 - number of characters in cdat.
c
c***********************************************************************
      subroutine usr_plane (kpln,cdat,knc)
c
      include 'post.inc'
      integer*4 kpln(3),knc
      character*80 cdat
c
      equivalence (MOTREG,KPOSMP(0381))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTWRK,KPOSMP(1506))
      equivalence (ISXVAL,KPOSMP(4209)), (PLNCOD,KPOSMP(4222))
c
      integer*4 MOTREG(24),IRTWRK(20),PLNCOD(4),ISXVAL,IRTNUM
c
      equivalence (PLNCDV,POSMAP(2208))
c
      real*8 PLNCDV(4)
c
      integer*4 i,num,nc,is,ipt,inx,jindex
      real*8 rval(3)
      character*80 sbuf
c
      data rval /0.,0.,0./
c
      if (PLNCOD(4) .eq. 0) go to 8000
      call codout (PLNCOD(4),PLNCDV(4))
c
c...get register' number for each axis defining the plane
c
      is    = 1
      do 155 i=1,3
         num   = kpln(i)
         if (num .gt. 3) then
            num = kpln(1) - 3
            inx = jindex (IRTWRK,num,IRTNUM)
            ipt = 13 + 3*(inx - 1)
            inx = MOTREG(ipt)
         else
            ipt = 4*(num - 1) + 1
            inx = MOTREG(ipt)
         end if
         call fmtcod_app (inx,rval(i),ISXVAL,sbuf,nc)
         cdat(is:) = sbuf(1:nc)
         is     = is + nc
  155 continue
      knc    = is - 1
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  out_cyl_stat (gnum)
c
c   FUNCTION:  This routine formats and outputs cylindrical interpolation
c              select/diselect block  (Sinumerik 880).
c
c   INPUT:  gnum   R*8 D1  - specifies number of linear units (inch/mm)
c                            per (angular) degree on the cylinder
c                            diameter. Value 1. is used to deselect
c                            cylindrical interpolation mode.
c
c   OUTPUT: none
c
c***********************************************************************
      subroutine out_cyl_stat (gnum)
c
      include 'post.inc'
      real*8 gnum
c
      equivalence (MOTREG,KPOSMP(0381)), (MODCYL,KPOSMP(4204))
      equivalence (LUNREG,KPOSMP(4205)), (ISRCYL,KPOSMP(4206))
      equivalence (ISRVAL,KPOSMP(4208))
c
      integer*4 MOTREG(24),LUNREG,MODCYL,ISRVAL,ISRCYL
c
      character*80 sbuf
      integer*4 nc,ipt
c
      if (LUNREG .ne. 0) call codout (LUNREG,gnum)
      if (ISRCYL .eq. 1) then
         ipt = 13 + 3*(MODCYL - 1)
         ipt = MOTREG(ipt)
         call fmtcod_app (ipt,gnum,ISRVAL,sbuf,nc)
         call add_append (sbuf,nc)
         call clrbuf
      end if

      return
      end

c***********************************************************************
c
c   SUBROUTINE:  parse_pplan (knum)
c
c   FUNCTION:  This routine formats and outputs cylindrical interpolation
c              select/diselect block  (Sinumerik 880).
c
c   INPUT:  knum   I*4 D1  - specifies index of word to parse in IPSTWD
c
c   OUTPUT: none
c
c***********************************************************************
      subroutine parse_pplan (knum)
c
      include 'post.inc'
      integer*4 knum
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (LTHXY ,KPOSMP(1225))
c
      integer*4 MXCL,IPSTWD(50),LTHXY
c
      knum = knum + 1
      if (MXCL .lt. knum) go to 8000
      if (IPSTWD(knum) .eq. 0) go to 9500
      if (IPSTWD(knum) .eq. 33) then
         LTHXY = 1
      else if (IPSTWD(knum) .eq. 41) then
         LTHXY = 2
      else
         go to 9100
      end if
c
 8000 return
c
c...Invalid minor word
c
 9100 call psterr (2,'INVMINOR',' ',knum)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',knum)
      go to 8000
      end
