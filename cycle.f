c
c***********************************************************************
c
c   FILE NAME:  cycle
c   CONTAINS:
c               cycle  cyclth
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cycle.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        08/11/16 , 10:31:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cycle
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 CYCLE/ON
c                       OFF
c
c                 CYCLE/AUTO,ON
c                            OFF
c
c                 CYCLE/AVOID
c
c                 CYCLE/mode,FEDTO,depth, (funit ,feed(,rap)) $
c                            (,RAPTO,r) (,DWELL,dwl1(,dwl2)) $
c                            (,STEP,peck1(,peck2)) $
c                            (,OFFSET,xoff(,yoff))) $
c                            (,RTRCTO,ret(,plng)) (REPEAT,l) $
c                            (,PARAMS,m1,n1,...,mn,nn)
c
c                    mode = BORE, BORE7, BORE8, BORE9, DEEP, DRILL,
c                           FACE, MILL, REAM, REVERS, SHIFT, TAP, THRU
c
c                    funit = IPM, IPR, MMPM, MMPR, TPI
c
c                 CYCLE/CIRCUL,DEPTH,z, RADIUS,d, ON, (TOOL,tdia,) $
c                                       DIAMTR    IN
c                              STEP,z1 (,IPM,f(,rap)) (,RAPTO,r)   $
c                                        IPR
c                              (,ATANGL,a) (,DOWN) (,CCLW) (RTRCTO,ret)
c                                            UP      CLW
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cycle
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICYCFL,KPOSMP(0181)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (ICYCSV,KPOSMP(0291))
      equivalence (MACPSW,KPOSMP(0859)), (CYCPSW,KPOSMP(0890))
      equivalence (ICYMTP,KPOSMP(0920)), (MACHTP,KPOSMP(1201))
      equivalence (IHELIX,KPOSMP(1392)), (IFITYP,KPOSMP(3150))
      equivalence (ICUTDO,KPOSMP(3301)), (LTMODE,KPOSMP(4125))
      equivalence (ICRHEL,KPOSMP(4250))
c
      integer*4 MXCL,IPSTWD(50),ICYCFL(30),ICYCSW(5),ICYCDO(15),IFITYP,
     1          LTMODE,ICYCSV(5),MACHTP,ICUTDO(15),IHELIX,ICRHEL(10),
     2          MACPSW(30),CYCPSW(30),ICYMTP
c
      equivalence (RAD   ,POSMAP(0002)), (DUMMY ,POSMAP(0003))
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (CUTTER,POSMAP(0744))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (CYCVR ,POSMAP(2926)), (RCYCDO,POSMAP(2931))
      equivalence (CYCPSV,POSMAP(2951)), (CYCPLN,POSMAP(2964))
      equivalence (RPM,POSMAP(3307))
      equivalence (PFEED ,POSMAP(3540)), (CYCPRM,POSMAP(4330))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 METCNV,PSTWD(50),RAD,CYCVR(5),RCYCDO(20),PFEED(4),
     1       CYCPSV(10),CYCPLN(4),MCHNUM(3,4),LINAXS(6),AXSOUT(10),
     2       TLVEC(3),VECSAV(3),ROTANG(20,2),STONUM(3,4),DUMMY,
     3       CYCPRM(30),CUTTER(7),RPM
c
      integer*4 inc,i,ifl(10),imode(14),nmode,isub(16),ival(16),icnt,
     1          iaxsw(10),ierr,inum,ipsw(30)
c
      real*8 rv(20),rprm(30)
c
      data nmode /14/
      data imode /82,211,212,213,153,163,81,151,262,1008,249,168,152,
     1            75/
c
      data isub /2,3,3,4,5,5,6,6,7,7,8,8,9,10,10,10/
      data ival /1,1,2,1,1,2,1,2,1,2,1,2,1, 1, 2, 3/
c
c...This machine is a Lathe
c...Call Lathe CYCLE parsing routine
c
      if (MACHTP .eq. 2 .or. MACHTP .eq. 4 .and. LTMODE .eq. 0) then
          call cyclth
          go to 8000
      endif
c
c...Initialize routine
c
      inc    = 0
      if (MXCL .eq. 0) go to 9000
      inc    = 1
c
c...CYCLE/ON
c
      if (IPSTWD(1) .eq. 71) then
          if (ICUTDO(2) .eq. 1) go to 9930
c         if (ICYCSW(1) .ne. 0) then
          if (ICYCSW(1) .eq. 0) then
              if (ICYCDO(1) .le. 0) go to 9920
              ICYCSV(1) = IFITYP
              CYCPSV(3) = PFEED(IFITYP)
c         else
c             if (ICYCDO(1) .le. 0) go to 9920
          endif
          if (ICYCDO(3) .ne. 0) then
              IFITYP = ICYCDO(3)
              if (IFITYP .ne. 5) PFEED(IFITYP) = RCYCDO(2)
          endif
          ICYCSW(1) = ICYCFL(21)
          ICYCSW(2) = 1
          ICYMTP = 0
c
c...CYCLE/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          call cycoff (1)
c
c...CYCLE/AUTO
c
      else if (IPSTWD(1) .eq. 88) then
          inc    = inc    + 1
          if (MXCL .lt. inc) go to 9000
c
c......CYCLE/AUTO,ON
c
          if (IPSTWD(inc) .eq. 71) then
              if (ICYCFL(1) .ne. 1) go to 9200
              ICYCFL(21) = 1
c
c......CYCLE/AUTO,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              ICYCFL(21) = 2
c
c......Invalid minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
c
c...CYCLE/AVOID
c
      else if (IPSTWD(inc) .eq. 187) then
          if (ICYCSW(1) .eq. 0) go to 9800
          if (CYCPLN(4) .eq. DUMMY) go to 9910
c
c.......Get clearance location
c
          call plnint (STONUM(1,2),VECSAV,CYCPLN,MCHNUM(1,2),ierr)
          if (ierr .eq. 1) go to 9900
c
c......Cancel cycle &
c......Output retract block
c
          call cycoff (1)
          ICYCSW(4) = 1
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,1,5)
          TLVEC(1) = VECSAV(1)
          TLVEC(2) = VECSAV(2)
          TLVEC(3) = VECSAV(3)
          call rapset (1,2)
          call whchax (AXSOUT,iaxsw,icnt)
          if (icnt .gt. 0) call motion (iaxsw,icnt)
          call cyc2nd (0)
          ICYCSW(4) = 0
c
c...CYCLE/CIRCUL
c
      else if (IPSTWD(1) .eq. 75) then
          if (ICYCSW(1) .eq. 0) then
              ICYCSV(1) = IFITYP
              CYCPSV(3) = PFEED(IFITYP)
          endif
          do i=1,10
            ifl(i) = 0
            rv(i) = 0
          enddo
          ifl(1) = 14
          ifl(3) = IFITYP
          rv(8) = CUTTER(1)
          rv(2) = PFEED(IFITYP)
          rv(3) = CYCVR(3)
          rv(11) = 0
          do i=2,mxcl
              inc = i + 1
c
c......CYCLE/CIRCUL,DEPTH
c
              if (IPSTWD(i) .eq. 510) then
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  rv(1) = PSTWD(i+1)
                  inc = i + 2
                  if (i+2 .le. MXCL .and. IPSTWD(i+2) .eq. 0) go to 9500
c
c......CYCLE/CIRCUL,DIAMTR
c
              else if (IPSTWD(i) .eq. 205) then
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  rv(8) = PSTWD(i+1)/2.
                  inc = i + 2
                  if (i+2 .le. MXCL .and. IPSTWD(i+2) .eq. 0) go to 9500
c
c......CYCLE/CIRCUL,RADIUS
c
              else if (IPSTWD(i) .eq. 23) then
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  rv(8) = PSTWD(i+1)
                  inc = i + 2
                  if (i+2 .le. MXCL .and. IPSTWD(i+2) .eq. 0) go to 9500
c
c......CYCLE/CIRCUL,STEP
c
              else if (IPSTWD(i) .eq. 92) then
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  rv(7) = PSTWD(i+1)
                  inc = i + 2
                  if (i+2 .le. MXCL .and. IPSTWD(i+2) .eq. 0) go to 9500
c
c......CYCLE/CIRCUL,TOOL
c
              else if (IPSTWD(i) .eq. 617) then
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  rv(5) = PSTWD(i+1)
                  inc = i + 2
                  if (i+2 .le. MXCL .and. IPSTWD(i+2) .eq. 0) go to 9500
c
c......CYCLE/CIRCUL,ATANGL
c
              else if (IPSTWD(i) .eq. 1) then
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  rv(6) = PSTWD(i+1)
                  inc = i + 2
                  if (i+2 .le. MXCL .and. IPSTWD(i+2) .eq. 0) go to 9500
c
c......CYCLE/CIRCUL,IPM
c
              else if (IPSTWD(i) .eq. 73 .or. IPSTWD(i) .eq. 74 .or.
     1                 IPSTWD(i) .eq. 315 .or. IPSTWD(i) .eq. 316 .or.
     2                 IPSTWD(i).eq.143) then
                  if (IPSTWD(i) .eq. 73 .or. IPSTWD(i) .eq. 315) then
                      ifl(3) = 1
                  else if (IPSTWD(i) .eq. 74 .or. IPSTWD(i) .eq. 316)
     1                    then
                      ifl(3) = 2
c
c.... no IPR allowed
c
c...else if (IPSTWD(6) .eq. 143) then
c......ifl(3) = 3
c
                  else
                      goto 9100
                  endif
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  rv(2) = PSTWD(i+1) * METCNV  ! feedrate
                  inc = i + 2
                  if (ifl(3) .eq. 2) then
                      ifl(3) = 1
                      rv(2) = rv(2) * RPM * METCNV
                  endif
c
c......if (ifl(3) .eq. 3) then
c......ifl(3) = 2
c......if (rv(2) .ne. 0) rv(2) = 1. / rv(2)
c......endif
c
c
c.........CYCLE/,IPM,rap
c
                  if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                      if (PSTWD(inc) .lt. 0.) go to 9400
                      rv(3) = PSTWD(inc) * METCNV
                      inc    = inc    + 1
                  endif
                  if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) go to 9500
c
c......CYCLE/CIRCUL,UP / DOWN
c
              else if (IPSTWD(i).eq.112 .or. IPSTWD(i).eq.113) then
                  if (IPSTWD(i).eq.113) ifl(5) = 1
c
c......CYCLE/CIRCUL,CCLW / CLW
c
              else if (IPSTWD(i).eq.59 .or. IPSTWD(i).eq.60) then
                  if (IPSTWD(i).eq.60) ifl(6) = 1
c
c......CYCLE/CIRCUL,IN / ON
c
              else if (IPSTWD(i).eq.71 .or. IPSTWD(i).eq.48) then
                  if (IPSTWD(i).eq.71) ifl(7) = 1
c
c......CYCLE/CIRCUL,RAPTO
c
              else if (IPSTWD(i) .eq. 280) then ! RAPTO
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  rv(4) = PSTWD(i+1)
                  inc = i + 2
                  if (i+2 .le. MXCL .and. IPSTWD(i+2) .eq. 0) go to 9500
c
c......CYCLE/,RTRCTO,ret
c
              else if (IPSTWD(i) .eq. 295) then
                  if (i .ge. MXCL .or. IPSTWD(i+1) .ne. 0) go to 9300
                  if (ifl(8) .ne. 0) go to 9600
                  ifl(8) = 1
                  rv(11) = PSTWD(i+1)
                  inc = i + 2
                  if (i+2 .le. MXCL .and. IPSTWD(i+2) .eq. 0) go to 9500
c
c......Unknown parameter
c
              else if (IPSTWD(i) .ne. 0) then
                  goto 9100
              endif
          enddo
          if (rv(7) .eq. 0) goto 9100
          ifl(4) = rv(1)/rv(7) + 0.5

          IHELIX = ICRHEL(1)
          ICYCSW(1) = ICYCFL(21)

c
c......If UP is specified and RTRCTO,ret is not specified or ret is specified
c......as 0, default ret value equal to the sum of depth + rapto
c
          if (ifl(5) .eq. 0) then
              if (ifl(8) .eq. 0) rv(11) = rv(1) + rv(4)
              if (rv(11) .eq. 0.0d0) rv(11) = rv(1) + rv(4)
          endif


          do i=1,10
              ICYCDO(i) = ifl(i)
              RCYCDO(i) = rv(i)
          enddo
          ICYCSW(2) = 1
          CYCPSV(4) = 0.
          RCYCDO(11) = rv(11)
          inc = mxcl
c
c...CYCLE/mode,...
c
      else
          if (MXCL .lt. 3) go to 9000
          if (ICYCSW(1) .eq. 0) then
              ICYCSV(1) = IFITYP
              CYCPSV(3) = PFEED(IFITYP)
          endif
          inc    = 1
          do 100 i=1,10,1
              ifl(i) = 0
  100     continue
          if (ICYCFL(30) .eq. 1) then
              do 120 i=1,30,1
                  ipsw(i) = 0
  120         continue
          endif
          ICYCDO(3) = IFITYP
          rv(2) = PFEED(IFITYP)
          rv(3) = CYCVR(3)
          rv(4) = 0.
          rv(14) = 1.
          rv(15) = 1.
          rv(16) = 1.
  200     if (IPSTWD(inc) .eq. 0) go to 9500
c
c......CYCLE/mode
c
          do 220 i=1,nmode,1
              if (IPSTWD(inc) .eq. imode(i)) go to 230
  220     continue
          i      = 0
  230     if (i .ne. 0) then
              if (ICUTDO(2) .eq. 1) go to 9930
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = i
              inc    = inc    + 1
c
c......CYCLE/,FEDTO,depth
c
          else if (IPSTWD(inc) .eq. 281) then
              if (ifl(2) .ne. 0) go to 9600
              ifl(2) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(1) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,FEDTO,dia,ang
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .le. 0. .or. PSTWD(inc) .ge. 180.)
     1                    go to 9400
                  rv(1) = (.5*rv(1)) / (dtan(PSTWD(inc)/RAD/2.))
                  inc    = inc    + 1
              endif
c
c......CYCLE/,IPM,feed
c
          else if (IPSTWD(inc) .eq. 73 .or. IPSTWD(inc) .eq. 74 .or.
     1             IPSTWD(inc) .eq. 315 .or. IPSTWD(inc) .eq. 316 .or.
     2             IPSTWD(inc) .eq. 143) then
              if (ifl(3) .ne. 0) go to 9600
              ifl(3) = 1
              if (IPSTWD(inc) .eq. 74 .or. IPSTWD(inc) .eq. 316)
     1                ifl(3) = 2
              if (IPSTWD(inc) .eq. 143) ifl(3) = 5
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .le. 0.) go to 9400
              rv(2) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,IPM,rap
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  rv(3) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,RAPTO,r
c
          else if (IPSTWD(inc) .eq. 280) then
              if (ifl(4) .ne. 0) go to 9600
              ifl(4) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(4) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c......CYCLE/,DWELL,dwl1
c
          else if (IPSTWD(inc) .eq. 279) then
              if (ifl(5) .ne. 0) go to 9600
              ifl(5) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(5) = PSTWD(inc)
              inc    = inc    + 1
c
c............CYCLE/,DWELL,dwl2
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  ifl(5) = 2
                  rv(6) = PSTWD(inc)
                  inc    = inc    + 1
              endif
c
c......CYCLE/,STEP,peck1
c
          else if (IPSTWD(inc) .eq. 92) then
              if (ifl(6) .ne. 0) go to 9600
              ifl(6) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(7) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,STEP,peck2
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  ifl(6) = 2
                  rv(8) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,OFFSET,xoff
c
          else if (IPSTWD(inc) .eq. 705) then
              if (ifl(7) .ne. 0) go to 9600
              ifl(7) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(9) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,OFFSET,yoff
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  ifl(7) = 2
                  rv(10) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,RTRCTO,ret
c
          else if (IPSTWD(inc) .eq. 295) then
              if (ifl(8) .ne. 0) go to 9600
              ifl(8) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(11) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,RTRCTO,plng
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  ifl(8) = 2
                  rv(12) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,REPEAT,l
c
          else if (IPSTWD(inc) .eq. 1083) then
              if (ifl(9) .ne. 0) go to 9600
              ifl(9) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .lt. 0.) go to 9400
              rv(13) = PSTWD(inc)
              inc    = inc    + 1
c
c......CYCLE/,PARAMS,m1,n1,...
c

          else if (IPSTWD(inc) .eq. 934) then
              do 300 inc=inc+1,MXCL-1,2
                  if (IPSTWD(inc) .ne. 0) go to 310
                  if (IPSTWD(inc+1) .ne. 0) go to 9300
                  inum   = PSTWD(inc)
                  if (PSTWD(inc) .lt. 1 .or. PSTWD(inc) .gt. 30)
     1                go to 9400
                  rprm(inum) = PSTWD(inc+1)
                  ipsw(inum) = 1
  300         continue
  310         continue
c
c......CYCLE/,START
c
          else if (IPSTWD(inc) .eq. 57) then
              if (ifl(10) .ne. 0) go to 9600
              ifl(10) = 1
              inc    = inc    + 1
c
c.........CYCLE/,START,n
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .le. 0) go to 9400
                  ifl(10) = 2
                  rv(14) = PSTWD(inc)
                  inc    = inc    + 1
c
c............CYCLE/,START,n,i
c
                  if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                      if (PSTWD(inc) .lt. 0.) go to 9400
                      ifl(10) = 3
                      rv(15) = PSTWD(inc)
                      inc    = inc    + 1
c
c...............CYCLE/,START,n,i,m
c
                      if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                          if (PSTWD(inc) .lt. 0.) go to 9400
                          ifl(10) = 4
                          rv(16) = PSTWD(inc)
                          inc    = inc    + 1
                      endif
                  endif
              endif
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          if (inc .le. MXCL) go to 200
c
c......Make sure a mode & FEDTO were specified
c
          if (ifl(1) .eq. 0 .or. ifl(2) .eq. 2) go to 9500
c
c......Set up CYCLE parameters
c
          ICYCSW(1) = ICYCFL(21)
          ICYCSW(2) = 1
          ICYMTP = 0
c
          do 500 i=1,10,1
              ICYCDO(i) = ifl(i)
  500     continue
c
          do 600 i=1,16,1
              if (ICYCDO(isub(i)) .ge. ival(i)) RCYCDO(i) = rv(i)
  600     continue
          RCYCDO(3) = rv(3)
          RCYCDO(14) = rv(14)
          RCYCDO(15) = rv(15)
          RCYCDO(16) = rv(16)
          ICYCDO(11) = RCYCDO(16)
          if (RCYCDO(3) .eq. 0.) RCYCDO(3) = 100000.
c
          if (ICYCDO(3) .ne. 0) then
              IFITYP = ICYCDO(3)
              if (IFITYP .ne. 5) PFEED(IFITYP) = RCYCDO(2)
          endif
c
          CYCPSV(4) = 0.
      endif
c
      if (ICYCFL(30) .eq. 1) then
          do 700 i=1,30,1
              CYCPSW(i) = ipsw(i)
              CYCPRM(i) = rprm(i)
  700     continue
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
c...Not valid for this machine
c
 9200 call psterr (2,'NOTVALID',' ',inc)
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
c...Conflicting parameters
c
 9600 call psterr (2,'CONFPRM',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
c
c...Cycle not currently active
c
 9800 call psterr (2,'CYCNACT',' ',inc)
      go to 8000
c
c...Tool axis does not intersect plane
c
 9900 call psterr (2,'VECPLN',' ',inc)
      go to 8000
c
c...AVOID plane not yet defined
c
 9910 call psterr (2,'NOAVOID',' ',inc)
      go to 8000
c
c...CYCLE/ON with no previous cycle
c
 9920 call psterr (2,'NOCYCDEF',' ',inc)
      go to 8000
c
c...CYCLE programmed with CUTCOM/ON
c
 9930 call psterr (2,'CYCCUT',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cyclth
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 CYCLE/ON
c                       OFF
c
c                 CYCLE/AUTO,ON
c                            OFF
c
c                 CYCLE/mode (,ZAXIS ,z) (,XAXIS ,x) (,FEDTO,depth) $
c                              ZCOORD     XCOORD
c
c                            (,funit,feed(,rap)) (,TPI,leadk(,leadi)) $
c                            (,RAPTO,r) (,STEP,peck1(,peck2)) $
c                            (,OFFSET,zoff(,xoff)) $
c                            (,RTRCTO,ret1(,ret2)) $
c                            (,TOOL,t1(,t2)) (,REPEAT,rep) (,ON )
c                                                            OFF
c
c                    mode = DEEP, DRILL, FACE, ROUGH, THREAD, THRU, TURN
c
c                    funit = IPM, IPR, MMPM, MMPR
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cyclth
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (ICYCSV,KPOSMP(0291))
      equivalence (ICYLFL,KPOSMP(1901)), (IFITYP,KPOSMP(3150))
      equivalence (ICUTDO,KPOSMP(3301))
c
      integer*4 MXCL,IPSTWD(50),ICYLFL(20),ICYCSW(5),ICYCDO(15),IFITYP,
     1          ICYCSV(5),ITYPE,ISUBT,ICUTDO(15)
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (CLPT  ,POSMAP(0491)), (CLSAV ,POSMAP(0201))
      equivalence (RCYCDO,POSMAP(2931)), (CYCPSV,POSMAP(2951))
      equivalence (CYLVR ,POSMAP(2993)), (PFEED ,POSMAP(3540))
c
      real*8 METCNV,PSTWD(50),CYLVR(5),RCYCDO(20),PFEED(4),CYCPSV(10),
     1       CLPT(240),CLSAV(21)
c
      integer*4 inc,i,ifl(13),imode(7),nmode,isub(17),ival(17),isav(3)
c
      real*8 rv(20)
c
      data nmode /7/
      data imode /153,163,81,320,1036,152,80/
c
      data isub /2,3,4,5,5,6,6,7,8,8,9,9,10,10,11,11,12/
      data ival /1,1,1,1,2,1,2,1,1,2,1,2, 1, 2, 1, 2, 1/
c
      inc    = 0
      if (MXCL .eq. 0) go to 9000
      inc    = 1
c
c...CYCLE/ON
c
      if (IPSTWD(1) .eq. 71) then
          if (ICUTDO(2) .eq. 1) go to 9930
          if (ICYCSW(1) .ne. 0) then
              ICYCSV(1) = IFITYP
              CYCPSV(3) = PFEED(IFITYP)
          else
              if (ICYCDO(1) .le. 0) go to 9920
          endif
          if (ICYCDO(3) .ne. 0) then
              IFITYP = ICYCDO(3)
              if (IFITYP .ne. 5) PFEED(IFITYP) = RCYCDO(2)
          endif
          ICYCSW(1) = ICYLFL(20)
          ICYCSW(2) = 1
c
c...CYCLE/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          call cyloff
c
c...CYCLE/AUTO
c
      else if (IPSTWD(1) .eq. 88) then
          inc    = inc    + 1
          if (MXCL .lt. inc) go to 9000
c
c......CYCLE/AUTO,ON
c
          if (IPSTWD(inc) .eq. 71) then
              if (ICYLFL(1) .eq. 1) ICYLFL(20) = 1
c
c......CYCLE/AUTO,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              ICYLFL(20) = 2
c
c......Invalid minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
c
c...CYCLE/mode,...
c
      else
          if (MXCL .lt. 3) go to 9000
          if (ICYCSW(1) .eq. 0) then
              ICYCSV(1) = IFITYP
              CYCPSV(3) = PFEED(IFITYP)
          endif
          inc    = 1
          do 100 i=1,13,1
              ifl(i) = 0
  100     continue
          ICYCDO(3) = IFITYP
          RCYCDO(1) = 0.
          RCYCDO(2) = 0.
          rv(4) = PFEED(IFITYP)
          rv(5) = CYLVR(1)
          rv(8) = 0.
  200     if (IPSTWD(inc) .eq. 0) go to 9500
c
c......CYCLE/mode
c
          do 220 i=1,nmode,1
              if (IPSTWD(inc) .eq. imode(i)) go to 230
  220     continue
          i      = 0
  230     if (i .ne. 0) then
              if (ICUTDO(2) .eq. 1) go to 9930
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = i * 2 - 1
              inc    = inc    + 1
c
c......CYCLE/,ZAXIS,z
c
          else if (IPSTWD(inc) .eq. 86 .or. IPSTWD(inc) .eq. 118) then
              if (ifl(2) .ne. 0) go to 9600
              ifl(2) = 1
              if (IPSTWD(inc) .eq. 118) ifl(2) = 2
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(1) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c......CYCLE/,XAXIS,x
c
          else if (IPSTWD(inc) .eq. 84 .or. IPSTWD(inc) .eq. 116) then
              if (ifl(3) .ne. 0) go to 9600
              ifl(3) = 1
              if (IPSTWD(inc) .eq. 116) ifl(3) = 2
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(2) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c......CYCLE/,FEDTO,depth
c
          else if (IPSTWD(inc) .eq. 281) then
              if (ifl(4) .ne. 0) go to 9600
              ifl(4) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(3) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c......CYCLE/,IPM,feed
c
          else if (IPSTWD(inc) .eq. 73 .or. IPSTWD(inc) .eq. 74 .or.
     1             IPSTWD(inc) .eq. 315 .or. IPSTWD(inc) .eq. 316) then
              if (ifl(5) .ne. 0) go to 9600
              ifl(5) = 1
              if (IPSTWD(inc) .eq. 74 .or. IPSTWD(inc) .eq. 316)
     1                ifl(5) = 2
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .le. 0.) go to 9400
              rv(4) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,IPM,rap
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  rv(5) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,TPI,leadk
c
          else if (IPSTWD(inc) .eq. 143) then
              if (ifl(6) .ne. 0) go to 9600
              ifl(6) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .le. 0.) go to 9400
              rv(6) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,TPI,leadi
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  ifl(6) = 2
                  rv(7) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,RAPTO,r
c
          else if (IPSTWD(inc) .eq. 280) then
              if (ifl(7) .ne. 0) go to 9600
              ifl(7) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(8) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c......CYCLE/,STEP,peck1
c
          else if (IPSTWD(inc) .eq. 92) then
              if (ifl(8) .ne. 0) go to 9600
              ifl(8) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(9) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,STEP,peck2
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  ifl(8) = 2
                  rv(10) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,OFFSET,zoff
c
          else if (IPSTWD(inc) .eq. 705) then
              if (ifl(9) .ne. 0) go to 9600
              ifl(9) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(11) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,OFFSET,xoff
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  ifl(9) = 2
                  rv(12) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,RTRCTO,ret
c
          else if (IPSTWD(inc) .eq. 295) then
              if (ifl(10) .ne. 0) go to 9600
              ifl(10) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(13) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........CYCLE/,RTRCTO,plng
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  ifl(10) = 2
                  rv(14) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......CYCLE/,TOOL,t1
c
          else if (IPSTWD(inc) .eq. 617) then
              if (ifl(11) .ne. 0) go to 9600
              ifl(11) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(15) = PSTWD(inc)
              inc    = inc    + 1
c
c............CYCLE/,TOOL,t2
c
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9400
                  ifl(11) = 2
                  rv(16) = PSTWD(inc)
                  inc    = inc    + 1
              endif
c
c......CYCLE/,REPEAT,l
c
          else if (IPSTWD(inc) .eq. 1083) then
              if (ifl(12) .ne. 0) go to 9600
              ifl(12) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .lt. 0.) go to 9400
              rv(17) = PSTWD(inc)
              inc    = inc    + 1
c
c......CYCLE/,ON
c
          else if (IPSTWD(inc) .eq. 71) then
              if (ifl(13) .ne. 0) go to 9600
              ifl(13) = 1
              inc    = inc    + 1
c
c......CYCLE/,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              if (ifl(13) .ne. 0) go to 9600
              ifl(13) = 2
              inc    = inc    + 1
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          if (inc .le. MXCL) go to 200
c
c......Make sure a mode was specified
c
          if (ifl(1) .eq. 0) go to 9500
c
c......Set up CYCLE parameters
c
          ICYCSW(1) = ICYLFL(20)
          ICYCSW(2) = 1
c
          do 500 i=1,13,1
              ICYCDO(i) = ifl(i)
  500     continue
          if (ICYCDO(8) .ne. 0) ICYCDO(1) = ICYCDO(1) + 1
c
          do 600 i=1,17,1
              if (ICYCDO(isub(i)) .ge. ival(i)) RCYCDO(i) = rv(i)
  600     continue
          RCYCDO(5) = rv(5)
          if (RCYCDO(5) .eq. 0.) RCYCDO(5) = 100000.
          RCYCDO(8) = rv(8)
c
          if (ICYCDO(5) .ne. 0) then
              IFITYP = ICYCDO(5)
              if (IFITYP .ne. 5) PFEED(IFITYP) = RCYCDO(4)
          endif
c
c......Axes were specified
c......Perform thread sequence now
c
          if (ICYCDO(2) .ne. 0 .or. ICYCDO(3) .ne. 0) then
c
c.........Adjust points to machine system
c
              if (ifl(3) .ne. 2) then
                  CLPT(1) = CLSAV(1) + RCYCDO(2)
              else
                  CLPT(1) = RCYCDO(2)
              endif
              CLPT(2) = CLSAV(2)
              if (ifl(2) .ne. 2) then
                  CLPT(3) = CLSAV(3) + RCYCDO(1)
              else
                  CLPT(3) = RCYCDO(1)
              endif
c
c.........Output cycle motion
c
              isav(1) = ITYPE
              isav(2) = ISUBT
              isav(3) = MXCL
              ITYPE = 5000
              ISUBT = 5
              MXCL  = 1
              call mocntl
              If (ICYCSW(1) .ne. 3) call cyloff
              ITYPE  = isav(1)
              ISUBT  = isav(2)
              MXCL   = isav(3)
          endif
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
c...Not valid for this machine
c
 9200 call psterr (2,'NOTVALID',' ',inc)
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
c...Conflicting parameters
c
 9600 call psterr (2,'CONFPRM',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9700 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
c
c...Cycle not currently active
c
 9800 call psterr (2,'CYCNACT',' ',inc)
      go to 8000
c
c...Tool axis does not intersect plane
c
 9900 call psterr (2,'VECPLN',' ',inc)
      go to 8000
c
c...AVOID plane not yet defined
c
 9910 call psterr (2,'NOAVOID',' ',inc)
      go to 8000
c
c...CYCLE/ON with no previous cycle
c
 9920 call psterr (2,'NOCYCDEF',' ',inc)
      go to 8000
c
c...CYCLE programmed with CUTCOM/ON
c
 9930 call psterr (2,'CYCCUT',' ',inc)
      go to 8000
      end
