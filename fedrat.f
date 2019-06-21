c
c***********************************************************************
c
c   FILE NAME:  fedrat
c   CONTAINS:
c               fedrat  rapid   raprst  rapset  slowdn  thread
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        fedrat.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:06
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  fedrat
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 FEDRAT/IPM   ,n
c                        IPR
c                        MMPM
c                        MMPR
c                        INVERS
c
c                 FEDRAT/LENGTH,n
c                        SCALE
c
c                 FEDRAT/LOCK,ON
c                             OFF
c
c                 FEDRAT/MAXIPM,n
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine fedrat
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MACHTP,KPOSMP(1201)), (IFEDSW,KPOSMP(3104))
      equivalence (IFDSUP,KPOSMP(3111)), (IFITYP,KPOSMP(3150))
      equivalence (FEDOCD,KPOSMP(3217)), (IFDLTP,KPOSMP(3187))
c
      integer*4 MXCL,IPSTWD(50),MACHTP,IFITYP,FEDOCD(2),IFDLTP,IFEDSW,
     1          IFDSUP(4)
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (FEDOVL,POSMAP(3001)), (FEDLEN,POSMAP(3539))
      equivalence (PFEED ,POSMAP(3540)), (MAXIPM,POSMAP(3544))
c
      real*8 METCNV,PSTWD(50),FEDOVL(2),FEDLEN,PFEED(4),MAXIPM
c
      integer*4 inc,imod,ifl(2)
c
      real*8 fed
c
c...FEDRAT/LENGTH
c
      inc    = 1
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 9) then
          inc    = 2
          if (MXCL .lt. 2) go to 9300
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (MACHTP .ne. 2) then
              FEDLEN = PSTWD(inc) * METCNV
              IFDLTP = 1
          endif
c
c...FEDRAT/SCALE
c
      else if (IPSTWD(1) .eq. 25) then
          inc    = 2
          if (MXCL .lt. 2) go to 9300
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (MACHTP .ne. 2) then
              FEDLEN = PSTWD(inc)
              IFDLTP = 2
          endif
c
c...FEDRAT/LOCK
c
      else if (IPSTWD(1) .eq. 114) then
          inc    = 2
          if (MXCL .lt. 2) go to 9500
          if (IPSTWD(inc) .eq. 71) then
              call codout (FEDOCD(1),FEDOVL(1))
          else if (IPSTWD(inc) .eq. 72) then
              call codout (FEDOCD(2),FEDOVL(2))
          else
              go to 9100
          endif
c
c...FEDRAT/MAXIPM
c
      else if (IPSTWD(1) .eq. 96) then
          inc    = 2
          if (MXCL .lt. 2) go to 9300
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .lt. 0.) go to 9400
          MAXIPM = PSTWD(inc) * METCNV
          if (MAXIPM .eq. 0.d0) MAXIPM = 99999999.
c
c...FEDRAT/n
c
      else
          imod   = IFITYP
          ifl(1) = 0
          ifl(2) = 0
  200     if (IPSTWD(inc) .eq. 0) then
              if (ifl(2) .eq. 1) go to 9600
              ifl(2) = 1
              fed    = PSTWD(inc) * METCNV
              if (fed .le. 0.) go to 9400
c
c......FEDRAT/IPM,MMPM
c
          else if (IPSTWD(inc) .eq. 73 .or. IPSTWD(inc) .eq. 315) then
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = 1
              imod   = 1
c
c......FEDRAT/IPR,MMPR
c
          else if (IPSTWD(inc) .eq. 74 .or. IPSTWD(inc) .eq. 316) then
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = 1
              imod   = 2
c
c......FEDRAT/INVERS
c
          else if (IPSTWD(inc) .eq. 6) then
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = 1
              imod   = 4
              if (IFDSUP(4) .ne. 1) imod = 1
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          inc    = inc    + 1
          if (inc .le. MXCL) go to 200
c
c......Set feed rate
c
          IFITYP = imod
          if (ifl(2) .eq. 1) then
              PFEED(IFITYP) = fed
              if (IFITYP .eq. 4) PFEED(1) = fed
              IFEDSW = 1
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
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rapid
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 RAPID(/)(MODIFY,)(XAXIS)(,RTRCTO)(,ON )
c                                   YAXIS            OFF
c                                   ZAXIS
c                                   TOOL
c
c                 RAPID/FEDTO(,SCALE ,n)
c                              LENGTH
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rapid
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (INCR  ,KPOSMP(1226))
      equivalence (IRPSUP,KPOSMP(3189)), (IRPALT,KPOSMP(3196))
      equivalence (IRPTAX,KPOSMP(3197)), (IRAP  ,KPOSMP(3199))
      equivalence (IRAPDO,KPOSMP(3220)), (IRPSAV,KPOSMP(3204))
      equivalence (IRPRET,KPOSMP(3219)), (LTMODE,KPOSMP(4125))
c
      integer*4 MXCL,IPSTWD(50),IRPSUP,IRPALT,IRPTAX,IRAP,IRAPDO(5),
     1          INCR,IRPSAV,IRPRET,LTMODE
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (RAPDIS,POSMAP(3582))
c
      real*8 METCNV,PSTWD(50),RAPDIS
c
      integer*4 inc,i,ifl(4),isup,ialt,itax,imod,iret,irsp
c
c...RAPID (no parameters)
c
      inc    = 0
      IRPSAV = INCR
      if (MXCL .eq. 0) then
          IRAP   = IRPSUP
          if (LTMODE .ne. 0) call raplmod (IRAP)
          IRAPDO(1) = IRPALT
          IRAPDO(2) = IRPTAX
          IRAPDO(3) = 2
          IRAPDO(4) = 1
          IRAPDO(5) = IRPRET
          RAPDIS = 0.
c
c...RAPID/MODIFY,-AXIS,ON-OFF
c
      else if (IPSTWD(1) .ne. 281) then
          inc    = 1
          isup   = IRPSUP
          ialt   = IRPALT
          itax   = IRPTAX
          imod   = 2
          iret   = IRPRET
          do 100 i=1,3,1
              ifl(i) = 0
  100     continue
  200     if (IPSTWD(inc) .eq. 0) go to 9500
c
c......RAPID/MODIFY
c
          if (IPSTWD(inc) .eq. 55) then
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = 1
              isup   = 3
c
c......RAPID/XAXIS
c
          else if (IPSTWD(inc) .eq. 84) then
              if (ifl(2) .eq. 1) go to 9600
              ifl(2) = 1
              ialt   = 2
c
c......RAPID/YAXIS
c
          else if (IPSTWD(inc) .eq. 85) then
              if (ifl(2) .eq. 1) go to 9600
              ifl(2) = 1
              ialt   = 3
c
c......RAPID/ZAXIS
c
          else if (IPSTWD(inc) .eq. 86) then
              if (ifl(2) .eq. 1) go to 9600
              ifl(2) = 1
              ialt   = 4
c
c......RAPID/TOOL
c
          else if (IPSTWD(inc) .eq. 617) then
              if (ifl(2) .eq. 1) go to 9600
              ifl(2) = 1
              ialt   = 5
              itax   = 1
c
c......RAPID/ON
c
          else if (IPSTWD(inc) .eq. 71) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              imod   = 1
c
c......RAPID/OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              imod   = 2
c
c......RAPID/RTRCTO
c
          else if (IPSTWD(inc) .eq. 295) then
              if (ifl(4) .eq. 1) go to 9600
              ifl(4) = 1
              iret   = 1
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          inc    = inc    + 1
          if (inc .le. MXCL) go to 200
c
c......Set up for rapid move
c
          IRAP   = isup
          if (LTMODE .ne. 0) call raplmod (IRAP)
          IRAPDO(1) = ialt
          IRAPDO(2) = itax
          IRAPDO(3) = imod
          IRAPDO(4) = 1
          IRAPDO(5) = iret
          RAPDIS = 0.
c
c...RAPID/FEDTO
c
      else
          inc    = 2
          if (inc .gt. MXCL) go to 9500
c
c......RAPID/FEDTO,LENGTH
c
          if (IPSTWD(inc) .eq. 9) then
              IRAPDO(4) = 1
c
c......RAPID/FEDTO,SCALE
c
          else if (IPSTWD(inc) .eq. 25) then
              IRAPDO(4) = 2
c
c......Unrecognized minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
c
c......Get distance for feed
c
          inc    = inc    + 1
          if (inc .gt. MXCL) then
              RAPDIS = 0.
          else
              if (IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .lt. 0. .or. (IRAPDO(4) .eq. 2 .and.
     1            PSTWD(inc) .gt. 1.)) go to 9400
              RAPDIS = PSTWD(inc)
              if (IRAPDO(4) .eq. 1) RAPDIS = RAPDIS * METCNV
          endif
c
c......Set up for rapid move
c
          IRAP   = IRPSUP
          if (LTMODE .ne. 0) call raplmod (IRAP)
          IRAPDO(1) = 1
          IRAPDO(2) = 1
          IRAPDO(3) = 2
          IRAPDO(5) = 0
          if (MXCL .gt. inc) go to 9700
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
      end
c
c***********************************************************************
c
c   SUBROUTINE:  raprst
c
c   FUNCTION:  This routine cancels rapid mode and resets all rapid
c              parameters.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine raprst
c
      include 'post.inc'
c
      equivalence (INCR  ,KPOSMP(1226))
      equivalence (IRPALT,KPOSMP(3196)), (IRPTAX,KPOSMP(3197))
      equivalence (IRAP  ,KPOSMP(3199)), (IRAPDO,KPOSMP(3220))
      equivalence (IRPSAV,KPOSMP(3204)), (IRPRET,KPOSMP(3219))
c
      integer*4 IRPALT,IRPTAX,IRAP,IRAPDO(5),INCR,IRPSAV,IRPRET
c
      equivalence (RAPDIS,POSMAP(3582))
c
      real*8 RAPDIS
c
c...Reset rapid mode
c
      IRAP   = 0
      IRAPDO(1) = IRPALT
      IRAPDO(2) = IRPTAX
      IRAPDO(3) = 2
      IRAPDO(4) = 1
      IRAPDO(5) = IRPRET
      RAPDIS = 0.
      INCR   = IRPSAV
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rapset (kalt,ktax)
c
c   FUNCTION:  This routine initializes rapid motion utilizing the
c              following modes:
c
c                  1. Specified rapid support.
c                  2. Input rapid alteration.
c                  3. Input rapid plunge/retract.
c                  4. Non-modal rapid.
c                  5. Full move at rapid rate.
c
c   INPUT:  kalt    I*4  D1  - Same as IRPALT.
c
c           ktax    I*4  D1  - Same as IRPTAX.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rapset (kalt,ktax)
c
      include 'post.inc'
c
      equivalence (INCR  ,KPOSMP(1226)), (IRPSUP,KPOSMP(3189))
      equivalence (IRAP  ,KPOSMP(3199)), (IRAPDO,KPOSMP(3220))
      equivalence (IRPSAV,KPOSMP(3204)), (IRPRET,KPOSMP(3219))
c
      integer*4 IRAP,IRAPDO(5),INCR,IRPSAV,IRPSUP,IRPRET
c
      equivalence (RAPDIS,POSMAP(3582))
c
      real*8 RAPDIS
c
      integer*4 kalt,ktax
c
c...Reset rapid mode
c
      IRPSAV = INCR
      IRAP   = IRPSUP
      IRAPDO(1) = kalt
      IRAPDO(2) = ktax
      IRAPDO(3) = 2
      IRAPDO(4) = 1
      IRAPDO(5) = IRPRET
      RAPDIS = 0.
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  slowdn
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 SLOWDN/OFF
c                        ON(,n)(,NEXT)
c                                AUTO
c
c                 SLOWDN/RAMP,OFF
c                             ON [,LIMIT,maxvel,axsvel] [,STEP,min,max]
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine slowdn
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (SLWFL ,KPOSMP(1701)), (SLWCD ,KPOSMP(1706))
      equivalence (ISLWDN,KPOSMP(1721)), (ISLWNM,KPOSMP(1722))
      equivalence (IACCFL,KPOSMP(1745))
c
      integer*4 MXCL,IPSTWD(50),SLWFL(5),SLWCD(15),ISLWDN,ISLWNM,
     1          MCHOPT(20),IACCFL(2)
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (SLWVR ,POSMAP(2451))
      equivalence (SLWVL ,POSMAP(2456)), (MAXVEL,POSMAP(4293))
      equivalence (AXSVEL,POSMAP(4294)), (ACCSTP,POSMAP(4295))
c
      real*8 PSTWD(20),SLWVL(15),SLWVR(5),METCNV,MAXVEL,AXSVEL,
     1       ACCSTP(2)
c
      integer*4 inc,inum,isw,i
c
      real*8 ssover(2,13),rnum
c
      data ssover /.0001,  1.000,   .0004,  3.000,   .0006,  5.000,
     1             .0009,  7.000,   .0017, 10.000,   .0028, 15.000,
     2             .0038, 20.000,   .0053, 25.000,   .0073, 30.000,
     3             .0093, 40.000,   .0126, 50.000,   .0186, 75.000,
     4             .0272,100.000  /
c
c...SLOWDN/OFF
c
      inc    = 1
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 72) then
          ISLWDN = 2
          if (SLWFL(1) .eq. 1) call codout (SLWCD(13),SLWVL(13))
c
c...SLOWDN/ON
c
      else if (IPSTWD(1) .eq. 71) then
          inum   = 1
          isw    = 1
          rnum   = SLWVR(2)
c
c......SLOWDN/ON,n
c
          inc    = inc    + 1
          if (inc .gt. MXCL) go to 500
          if (IPSTWD(inc) .eq. 0) then
              if (SLWFL(1) .eq. 1) then
                  inum   = PSTWD(inc)
                  if (inum .lt. 1 .or. inum .gt. SLWFL(4)) go to 9400
              else
                  rnum   = PSTWD(inc) * METCNV
                  if (rnum .lt. 0.) go to 9400
              endif
              inc    = inc    + 1
          endif
c
c......SLOWDN/ON,AUTO
c
          if (inc .gt. MXCL) go to 500
          if (IPSTWD(inc) .eq. 88) then
              isw    = 1
c
c......SLOWDN/ON,NEXT
c
          else if (IPSTWD(inc) .eq. 162) then
              isw    = 3
c
c......Invalid minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9200
              go to 9500
          endif
c
c......Set slowdown flags
c
  500     ISLWDN = isw
          ISLWNM = inum
          SLWVR(2) = rnum
c
c......Calculate maximum feed for slowdowns
c......when angular change = 90 degrees
c
          do 800 i=2,13,1
              if (SLWVR(2) .ge. ssover(1,i-1) .and.
     1            SLWVR(2) .lt. ssover(1,i)) go to 850
  800     continue
  850     SLWVR(3) = ssover(2,i-1)
          if (MCHOPT(2) .eq.  2) SLWVR(3) = SLWVR(3) * 25.4
c
c...SLOWDN/RAMP
c
      else if (IPSTWD(1) .eq. 854) then
c
c......SLOWDN/RAMP,OFF
c
          if (MXCL .lt. 2) go to 9000
          if (IPSTWD(2) .eq. 72) then
              IACCFL(2) = 2
              inc    = 2
c
c......SLOWDN/RAMP,ON
c
          else if (IPSTWD(2) .eq. 71) then
              IACCFL(2) = IACCFL(1)
              inc    = 3
 1000         if (inc .gt. MXCL) go to 1200
              if (inc+2 .gt. MXCL) go to 9000
c
c.........SLOWDN/RAMP,ON,LIMIT
c
              if (IPSTWD(inc) .eq. 1078) then
                  inc    = inc    + 1
                  if (IPSTWD(inc) .ne. 0) go to 9300
                  if (PSTWD(inc) .lt. .001) go to 9400
                  MAXVEL = PSTWD(inc) * METCNV
                  inc    = inc    + 1
                  if (IPSTWD(inc) .ne. 0) go to 9300
                  if (PSTWD(inc) .lt. .001) go to 9400
                  AXSVEL = PSTWD(inc) * METCNV
                  inc    = inc    + 1
c
c.........SLOWDN/RAMP,ON,STEP
c
              else if (IPSTWD(inc) .eq. 92) then
                  inc    = inc    + 1
                  if (IPSTWD(inc) .ne. 0) go to 9300
                  if (PSTWD(inc) .lt. .001) go to 9400
                  ACCSTP(1) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
                  if (IPSTWD(inc) .ne. 0) go to 9300
                  if (PSTWD(inc) .lt. .001) go to 9400
                  ACCSTP(2) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              else if (IPSTWD(inc) .eq. 0) then
                  go to 9200
              else
                  go to 9500
              endif
              go to 1000
 1200         continue
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
c...Minor word expected
c
 9200 call psterr (2,'MINOREXP',' ',inc)
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
c...Invalid minor word
c
 9500 call psterr (2,'INVMINOR',' ',inc)
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
c   SUBROUTINE:  thread
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 THREAD/OFF
c
c                 THREAD/leadk(,leadi)(,XAXIS,x)(,ZAXIS,z)(,INCR,k) $
c                                                           DECR
c                        (,AUTO)
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine thread
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (ICYCSV,KPOSMP(0291))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (LTHXY ,KPOSMP(1225)), (IFITYP,KPOSMP(3150))
      equivalence (MTPDYN,KPOSMP(4126)), (LTMODE,KPOSMP(4125))
c
      integer*4 MXCL,IPSTWD(50),ICYCSW(5),ICYCDO(15),IFITYP,
     1          ICYCSV(5),LTHXY,MACHTP,LTMODE,MTPDYN
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387)), (RCYCDO,POSMAP(2931))
      equivalence (CYCPSV,POSMAP(2951)), (RPM   ,POSMAP(3307))
      equivalence (PFEED ,POSMAP(3540)), (ROTANG,POSMAP(5173))
c
      real*8 RPM,PSTWD(50),RCYCDO(20),PFEED(4),CYCPSV(10),LINAXS(6),
     1       METCNV,MCHNUM(3,4),AXSOUT(10),ROTANG(20,2),STONUM(3,4),
     2       TLVEC(3)
c
      integer*4 inc,i,ifl(5),isub(5),ival(5)
c
      real*8 rv(5)
c
      data isub /1,1,2,3,4/
      data ival /1,2,1,1,1/
c
      if (MTPDYN .ne. 2 .or. LTMODE .ne. 0) go to 9700
      inc    = 0
      if (MXCL .eq. 0) go to 9000
      inc    = 1
c
c...THREAD/OFF
c
      if (IPSTWD(1) .eq. 72) then
          call cyloff
c
c...THREAD/leads
c
      else
          if (MXCL .lt. 1) go to 9000
c
c......Initialize parsing
c
          if (ICYCSW(1) .eq. 0) then
              ICYCSV(1) = IFITYP
              CYCPSV(3) = PFEED(IFITYP)
          endif
          inc    = 1
          do 100 i=1,5,1
              ifl(i) = 0
  100     continue
          RCYCDO(2) = 0.
          RCYCDO(3) = 0.
c
c......THREAD/leadk
c
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (ifl(1) .eq. 1) go to 9600
          if (PSTWD(inc) .lt. 0) go to 9400
          ifl(1) = 1
          rv(1) = PSTWD(inc)
          inc    = inc    + 1
          if (inc .gt. MXCL) go to 400
c
c.........THREAD/leadk,leadi
c
          if (IPSTWD(inc) .eq. 0) then
              if (PSTWD(inc) .lt. 0) go to 9400
              ifl(1) = 2
              rv(2) = PSTWD(inc)
              inc    = inc    + 1
          endif
          if (rv(1) .eq. 0. .and. rv(2) .eq. 0.) then
              inc    = inc    - 1
              go to 9400
          endif
          if (inc .gt. MXCL) go to 400
c
c......Get the rest of the parameters
c
  200    if (IPSTWD(inc) .eq. 0) go to 9500
c
c.........THREAD/,XAXIS,x
c
          if (IPSTWD(inc) .eq. 84 .or. IPSTWD(inc) .eq. 116) then
              if (ifl(2) .ne. 0) go to 9600
              ifl(2) = 1
              if (IPSTWD(inc) .eq. 116) ifl(2) = 2
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(3) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........THREAD/,ZAXIS,z
c
          else if (IPSTWD(inc) .eq. 86 .or. IPSTWD(inc) .eq. 118) then
              if (ifl(3) .ne. 0) go to 9600
              ifl(3) = 1
              if (IPSTWD(inc) .eq. 118) ifl(3) = 2
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(4) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........THREAD/,INCR,k
c
          else if (IPSTWD(inc) .eq. 66) then
              if (ifl(4) .ne. 0) go to 9600
              ifl(4) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(5) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........THREAD/,DECR,k
c
          else if (IPSTWD(inc) .eq. 65) then
              if (ifl(4) .ne. 0) go to 9600
              ifl(4) = 2
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rv(5) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........THREAD/,AUTO
c
          else if (IPSTWD(inc) .eq. 88) then
              if (ifl(5) .ne. 0) go to 9600
              ifl(5) = 1
              inc    = inc    + 1
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          if (inc .le. MXCL) go to 200
c
c......Set up THREAD parameters
c
  400     ICYCSW(1) = 4
          ICYCSW(2) = 1
c
          do 500 i=1,5,1
              ICYCDO(i) = ifl(i)
  500     continue
c
          do 600 i=1,5,1
              if (ICYCDO(isub(i)) .ge. ival(i)) RCYCDO(i) = rv(i)
  600     continue
c
c......Axes were specified
c......Perform thread sequence now
c
          if (ICYCDO(2) .ne. 0 .or. ICYCDO(3) .ne. 0) then
c
c.........Adjust points to machine system
c
              if (LTHXY .eq. 1) then
                  if (ICYCDO(3) .ne. 2) then
                      MCHNUM(1,1) = STONUM(1,1) + RCYCDO(4)
                  else
                      MCHNUM(1,1) = RCYCDO(4)
                  endif
                  if (ICYCDO(2) .ne. 2) then
                      MCHNUM(2,1) = STONUM(2,1) + RCYCDO(3)
                  else
                      MCHNUM(2,1) = RCYCDO(3)
                  endif
                  MCHNUM(3,1) = STONUM(3,1)
              else
                  if (ICYCDO(2) .ne. 2) then
                      MCHNUM(1,1) = STONUM(1,1) + RCYCDO(3)
                  else
                      MCHNUM(1,1) = RCYCDO(3)
                  endif
                  MCHNUM(2,1) = STONUM(2,1)
                  if (ICYCDO(3) .ne. 2) then
                      MCHNUM(3,1) = STONUM(3,1) + RCYCDO(4)
                  else
                      MCHNUM(3,1) = RCYCDO(4)
                  endif
              endif
              call preadj (MCHNUM(1,1),MCHNUM(1,2),TLVEC,TLVEC)
              call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,2,5)
c
c.........Output threading motion
c
              call thrmot (RCYCDO(1),RCYCDO(2),ICYCDO(4),RCYCDO(5),
     1                     ICYCDO(5))
          endif
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
c...THREAD not valid for mills
c
 9700 call psterr (2,'NOTVALID',' ',0)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  raplmod (ksup)
c
c   FUNCTION:  This routine sets rapid for Lathe in Polar or Cylindrical
c              interpolation mode if it is supported, otherwise LMFP or
c              LMDP mode is canceled and rapid is in MODE/MILL,AUTO.
c
c   INPUT:  none.
c
c   OUTPUT: ksup  I*4  D1  - Rapid mode flag.
c
c***********************************************************************
      subroutine raplmod (ksup)
c
      include 'post.inc'
      integer*4 ksup
c
      equivalence (IRPSUP,KPOSMP(3189)), (LTMODE,KPOSMP(4125))
      equivalence (IRFSUP,KPOSMP(4136)), (IRDSUP,KPOSMP(4137))
c
      integer*4 IRPSUP,IRFSUP,IRDSUP,LTMODE
c
      if (LTMODE .eq. 1) then
         if (IRFSUP .eq. 1) then
            ksup = 1
         else
c
c...Reset LMFP mode to Auto
c
            call rstlmod (LTMODE)
         end if
      else if (LTMODE .eq. 2) then
         if (IRDSUP .eq. 1) then
            ksup = 1
         else
c
c...Reset LMDP mode to Auto
c
            call rstlmod (LTMODE)
         end if
      else if (LTMODE .eq. 3) then
         ksup = IRPSUP
      end if
c
      return
      end
