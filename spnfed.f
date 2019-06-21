c
c***********************************************************************
c
c   FILE NAME:  spnfed
c   CONTAINS:
c               fedctl  frnout  sfmctl  spnclr  spncod  spncol  spndon
c               tabfnd  svfedc  rsfedc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        spnfed.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 09:23:04
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  fedctl
c
c   FUNCTION:  This routine calculates all FPM and FPR feed rates de-
c              pending on the input mode.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine fedctl (kfl)
c
      include 'post.inc'
c
      equivalence (ISLWFD,KPOSMP(1723)), (IACCFD,KPOSMP(1744))
      equivalence (IFITYP,KPOSMP(3150)), (IRAP  ,KPOSMP(3199))
c
      integer*4 IFITYP,IRAP,ISLWFD,IACCFD
c
      equivalence (SLWFD ,POSMAP(2471))
      equivalence (RPM   ,POSMAP(3307)), (PFEED ,POSMAP(3540))
      equivalence (FEED  ,POSMAP(3547)), (RAPLMT,POSMAP(3567))
      equivalence (ACCFD ,POSMAP(4292))
c
      real*8 RPM,PFEED(4),FEED(4),RAPLMT(10),SLWFD,ACCFD
c
      integer*4 kfl(10)
c
      integer*4 i
c
c...Slowdown feedrate
c
      if (ISLWFD .eq. 1 .or. IACCFD .eq. 1) then
          if (IACCFD .eq. 1) then
              FEED(1) = ACCFD
              IACCFD = 0
          else
              FEED(1) = SLWFD
              ISLWFD = 0
          endif
          if (RPM .eq. 0.) then
              FEED(2) = 0.
          else
              FEED(2) = FEED(1) / RPM
          endif
          FEED(3) = 0.
          FEED(4) = FEED(1)
c
c...Rapid feed rate
c
      else if (IRAP .eq. 1 .or. IRAP .eq. 2) then
          FEED(1) = 0.
          do 100 i=1,6,1
              if (kfl(i) .eq. 1 .and. RAPLMT(i) .gt. FEED(1))
     1                FEED(1) = RAPLMT(i)
  100     continue
          if (FEED(1) .eq. 0.) FEED(1) = RAPLMT(1)
          if (RPM .eq. 0.) then
              FEED(2) = 0.
          else
              FEED(2) = PFEED(1) / RPM
          endif
          FEED(3) = 0.
          FEED(4) = PFEED(1)
c
c...FPR input feedrate
c
      else if (IFITYP .eq. 2 .or. IFITYP .eq. 5) then
          if (RPM .eq. 0.) then
              call psterr (2,'FPROFF',' ',-1)
              IFITYP = 1
              FEED(1) = PFEED(1)
              FEED(2) = 0.
              FEED(3) = 0.
              FEED(4) = PFEED(1)
          else
              FEED(1) = PFEED(2) * RPM
              FEED(2) = PFEED(2)
              FEED(3) = 0.
              FEED(4) = FEED(1)
          endif
c
c...FPM input feedrate
c
      else if (IFITYP .ne. 2) then
          FEED(1) = PFEED(1)
          if (RPM .eq. 0.) then
              FEED(2) = 0.
          else
              FEED(2) = PFEED(1) / RPM
          endif
          FEED(3) = 0.
          FEED(4) = PFEED(1)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  frnout (kfl)
c
c   FUNCTION:  This routine outputs the feed rate mode selection code
c              and the current feed rate.
c
c   INPUT:  kfl     I*4  D10 - 1 = This axis is waiting to be output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine frnout (kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (MCHOPT,KPOSMP(0308))
      equivalence (REGFRC,KPOSMP(0603)), (ICIPRM,KPOSMP(1230))
      equivalence (ISCIRC,KPOSMP(1238)), (IRTDEF,KPOSMP(1485))
      equivalence (IJKROT,KPOSMP(1739)), (IFDRCD,KPOSMP(3213))
      equivalence (FMTDES,KPOSMP(2133)), (IFEDSW,KPOSMP(3104))
      equivalence (IROTFT,KPOSMP(3110)), (IFDSUP,KPOSMP(3111))
      equivalence (IFDTYP,KPOSMP(3148))
      equivalence (IFITYP,KPOSMP(3150)), (IFOTYP,KPOSMP(3151))
      equivalence (NFDSUP,KPOSMP(3152)), (IFDFRC,KPOSMP(3153))
      equivalence (IFDCTP,KPOSMP(3157))
      equivalence (IFDOUT,KPOSMP(3158)), (FEDCD ,KPOSMP(3162))
      equivalence (FEDMCD,KPOSMP(3170)), (IFDEXD,KPOSMP(3178))
      equivalence (IFDUNT,KPOSMP(3182)), (NFEDTB,KPOSMP(3183))
      equivalence (IFDSPC,KPOSMP(3185)), (IRAPSW,KPOSMP(3205))
      equivalence (IFDCIR,KPOSMP(3206)), (POSFED,KPOSMP(3209))
      equivalence (IFDRAD,KPOSMP(3210)), (IFDRCN,KPOSMP(3211))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 IROTFT,IFDTYP(2),IFITYP,IFOTYP,IFDOUT(4),FEDCD(8),
     1          FEDMCD(8),IFDUNT,NFEDTB(2),REGFRC(MAXFMT),NFDSUP,
     2          IFDFRC(4),IFEDSW,IFDEXD(4),MCHOPT(20),IRAPSW,ISCIRC,
     3          ICIPRM(8),IFDCIR,POSFED,ICYCSW(5),IFDRAD,IFDRCD(4),
     4          IFDRCN,IRTDEF,IFDCTP,IFDSPC(2),IJKROT,IFDSUP(4)
c
      equivalence (PI    ,POSMAP(0001)), (PPTOLR,POSMAP(1274))
      equivalence (AXSOUT,POSMAP(1340)), (STONUM,POSMAP(1387))
      equivalence (AXSSTO,POSMAP(1425)), (IJKDIS,POSMAP(1453))
      equivalence (MOVDIS,POSMAP(1570))
      equivalence (RCIPRM,POSMAP(2049)), (FEDMVL,POSMAP(3320))
      equivalence (FEDCNS,POSMAP(3003)), (FRTVAL,POSMAP(3014))
      equivalence (FDIVCO,POSMAP(3328))
      equivalence (FEDTCD,POSMAP(3330)), (FEDTVL,POSMAP(3420))
      equivalence (FEDSPC,POSMAP(3510)), (FEDRNG,POSMAP(3512))
      equivalence (PFEED ,POSMAP(3540)), (DPMBPW,POSMAP(3551))
      equivalence (FMVTIM,POSMAP(3553)), (OFEED ,POSMAP(3560))
c
      real*8 MOVDIS(4),FEDMVL(8),FEDCNS(4),FDIVCO(2),FEDTCD(90),
     1       FEDTVL(90),FEDSPC(2),FEDRNG(2,8),FMVTIM,DPMBPW(2),
     2       OFEED(6),AXSOUT(10),AXSSTO(10),STONUM(3,4),RCIPRM(25),
     3       PPTOLR(3),PFEED(4),PI,FRTVAL(2),IJKDIS
c
      integer*4 kfl(10)
c
      integer*4 inc,iyx,imod,i,inum,ircd(4),nc,index,ifty
c
      real*8 rnum,snum,r2n,rmax,dis,rdis,tnum,rfed,rary(4),rrad(4),rval
c
      character*20 sbuf
c
c...Determine output feed rate mode
c
      if (POSFED .eq. 2) go to 8000
      imod   = IFOTYP
      ifty = 0
c
c......Rotary axes moves
c
      if (MOVDIS(4) .ne. 0. .and. ICYCSW(1) .ne. 1) then
          IFOTYP = IROTFT
c
c......Tool axis moves with no linear movement
c
      else if (IJKROT .eq. 1 .and. IJKDIS .ne. 0. .and.
     1         MOVDIS(1) .eq. 0.) then
          if (IFDSUP(4) .eq. 1) then
              IFOTYP = 4
          else if (IFDSUP(3) .eq. 1) then
              IFOTYP = 3
          else
              IFOTYP = IFDOUT(IFITYP)
          endif
c
c......FPM input mode
c
      else if (IFITYP .eq. 1) then
          IFOTYP = IFDOUT(1)
c
c......FPR input mode
c
      else if (IFITYP .eq. 2) then
          IFOTYP = IFDOUT(2)
c
c......Inverse time mode
c
      else if (IFITYP .eq. 4) then
          IFOTYP = 4
      endif
c
c......Force FPR when Z-axis moves
c
      if (IFDSPC(1) .eq. 1) then
          if (kfl(5) .eq. 1) then
              IFOTYP = IFDOUT(2)
          else
              IFOTYP = IFDOUT(1)
          endif
      endif
c
c......Force FPR when feed is less than ...
c
      call dpoint (OFEED(1),rnum,5)
      if (IFOTYP .eq. IFDOUT(1) .and. rnum .lt. FEDSPC(1))
     1        IFOTYP = IFDOUT(2)
c
c...Calculate output feed rate numbers
c......FPM when linear movement is greater
c......DPM when rotary movement is greater
c
      if (IFOTYP .eq. 6) then
          rnum   = MOVDIS(3) / DPMBPW(1)
          snum   = MOVDIS(4) / DPMBPW(2)
          IFOTYP = 1
          if (snum .gt. rnum) IFOTYP = 3
      endif
c
c......FPM mode
c
      if (IFOTYP .eq. 1 .or. IFOTYP .eq. 7) then
          ifty   = IFOTYP
c
c.........Feed control point active
c
          if (IFOTYP .eq. 7 .or. IFDCTP .eq. 1) then
              IFOTYP = 1
              rfed   = OFEED(2)
c
c............Protect against feed rate control point
c............remaining stationary during FCP IPM feed rate mode
c
              if (rfed*FMVTIM .le. PPTOLR(1) .and.
     1            MOVDIS(2) .le. PPTOLR(1)) rfed = PFEED(1)
c
c.........Axes slides active
c
          else
              rfed   = OFEED(3)
          endif
c
c.........Determine if Extended Prec feed required
c
          if (IFDEXD(1) .eq. 1 .and. FEDCD(1) .ne. 0 .and.
     1        FEDCD(5) .ne. 0) then
              rnum   = rfed   * FEDCNS(1)
              call codint (FEDCD(5),rnum,snum,inum)
              tnum   = snum   / FEDCNS(1)
              call codint (FEDCD(1),rfed,rnum,inum)
              if (rnum .ne. tnum) then
                  call rtoc (snum,sbuf,nc)
                  nc     = index(sbuf,'.')
                  if (nc-1 .le. FMTDES(MCHOPT(2)+3,FEDCD(5)))
     1                    IFOTYP = 5
              endif
          endif
c
c.........Standard Precision FPM
c
         if (IFOTYP .eq. 1) then
              if (rfed .lt. FEDRNG(1,1)) rfed = FEDRNG(1,1)
              if (rfed .gt. FEDRNG(2,1)) rfed = FEDRNG(2,1)
              if (IFDTYP(1) .eq. 1) then
                  rnum   = rfed
              else
                  call tabfnd (FEDTVL(1),NFEDTB(1),rfed,inc)
                  rnum   = FEDTCD(inc)
              endif
c
c.........Extended precision FPM
c
          else
              rnum   = rfed   * FEDCNS(1)
              if (rnum .gt. FEDRNG(2,5)) then
                  rnum = FEDRNG(2,5)
                  if (ifty .eq. 1) then
                      OFEED(3) = rnum   / FEDCNS(1)
                  else
                      OFEED(2) = rnum   / FEDCNS(1)
                  endif
              endif
          endif
c
c......FPR mode
c
      else if (IFOTYP .eq. 2) then
          if (OFEED(6) .lt. FEDRNG(1,2)) OFEED(6) = FEDRNG(1,2)
          if (OFEED(6) .gt. FEDRNG(2,2)) OFEED(6) = FEDRNG(2,2)
              if (IFDTYP(2) .eq. 1) then
                  rnum   = OFEED(6)
              else
                  call tabfnd (FEDTVL(46),NFEDTB(2),OFEED(6),inc)
                  rnum   = FEDTCD(45+inc)
              endif
c
c......DPM mode
c
      else if (IFOTYP .eq. 3) then
          if (OFEED(4) .lt. FEDRNG(1,3)) OFEED(4) = FEDRNG(1,3)
          if (OFEED(4) .gt. FEDRNG(2,3)) OFEED(4) = FEDRNG(2,3)
          rnum   = OFEED(4)
c
c......Inverse time mode
c
      else if (IFOTYP .eq. 4) then
          if (FMVTIM .eq. 0.) then
              rnum    = FEDRNG(2,4)
              if (IFDEXD(3) .eq. 1) rnum = FEDRNG(2,8)
          else
              if (ISCIRC .eq. 1) then
                  if (IFDCIR .eq. 2) then
                      rnum   = OFEED(3) / RCIPRM(4)
                  else if (IFDCIR .eq. 3) then
                      rnum   = FMVTIM
                  else
                      rnum   = 1. / FMVTIM
                  endif
              else
                  if (IFDOUT(4) .eq. 3) then
                      rnum   = FMVTIM
                  else
                      rnum   = 1. / FMVTIM
                  endif
              endif
          endif
c
c.........Standard inverse time
c
          if (IFDOUT(4) .eq. 1 .or. IFDOUT(4) .eq. 3) then
              if (IFDUNT .eq. 2) then
                  if (ISCIRC .eq. 1) then
                      if (IFDCIR .eq. 2 .or. IFDCIR .eq. 3) then
                          rnum   = rnum   * 60.
                      else
                          rnum   = rnum   / 60.
                      endif
                  else
                      if (IFDOUT(4) .eq. 3) then
                          rnum   = rnum   * 60.
                      else
                          rnum   = rnum   / 60.
                      endif
                  endif
              endif
              if (IFDEXD(4) .eq. 1 .and. rnum .lt. FEDRNG(1,4))
     1                IFOTYP = 8
              if (IFOTYP .eq. 4) then
                  if (rnum .lt. FEDRNG(1,4)) rnum = FEDRNG(1,4)
                  if (rnum .gt. FEDRNG(2,4)) rnum = FEDRNG(2,4)
c
c............Extended precision inverse time
c
              else
                  rnum   = rnum   * FEDCNS(4)
                  if (rnum .lt. FEDRNG(1,8)) rnum = FEDRNG(1,8)
                  if (rnum .gt. FEDRNG(2,8)) rnum = FEDRNG(2,8)
              endif
c
c.........Toshiba vector speed
c
          else if (IFDOUT(4) .eq. 4) then
              call rtfrad (rrad)
              rdis = dsqrt(rrad(1)**2 + rrad(2)**2 + rrad(3)**2 +
     1                     rrad(4)**2)
              rval = MOVDIS(4) / FRTVAL(MCHOPT(2))
              rnum = OFEED(2) * dsqrt((MOVDIS(3)**2 + rval**2) /
     1               (MOVDIS(3)**2 + (PI/180.*rdis*rval)**2))
              if (rnum .lt. FEDRNG(1,4)) rnum = FEDRNG(1,4)
              if (rnum .gt. FEDRNG(2,4)) rnum = FEDRNG(2,4)
c
c.........Okuma Delta / Time
c
          else if (IFDOUT(4) .eq. 5) then
              if (IFDUNT .eq. 2) rnum = rnum / 60.
              rval = MOVDIS(4) / FRTVAL(MCHOPT(2))
              rnum = dsqrt(MOVDIS(3)**2 + rval**2) * rnum
              if (rnum .lt. FEDRNG(1,4)) rnum = FEDRNG(1,4)
              if (rnum .gt. FEDRNG(2,4)) rnum = FEDRNG(2,4)
c
c.........Secondary Okuma
c
          else if (IFDOUT(4) .eq. 6) then
              if (IFDUNT .eq. 2) rnum = rnum / 60.
              rval = MOVDIS(4) / FRTVAL(MCHOPT(2))
              rnum = OFEED(2) / dsqrt(MOVDIS(3)**2 + rval**2)
              if (rnum .lt. FEDRNG(1,4)) rnum = FEDRNG(1,4)
              if (rnum .gt. FEDRNG(2,4)) rnum = FEDRNG(2,4)
c
c.........Bendix inverse time
c
          else
c
c............Get maximum axis movement
c
              if (ISCIRC .eq. 1) then
                  rdis   = dabs(STONUM(ICIPRM(1),3)-RCIPRM(ICIPRM(1)))
                  dis    = dabs(STONUM(ICIPRM(2),3)-RCIPRM(ICIPRM(2)))
                  if (dis .gt. rdis) rdis = dis
              else
                  rdis   = -1.
                  do 400 i=1,6,1
                      dis    = dabs(AXSOUT(i)-AXSSTO(i))
                      if (dis .gt. rdis) rdis = dis
  400             continue
              endif
c
c............Get 2N
c
              iyx    = 0
              rmax   = rdis   / FDIVCO(2)
  500         r2n    = 2.d0**iyx
              if(r2n .gt. rmax) go to 600
              iyx    = iyx    + 1
              go to 500
c
c............Calculate FRN
c
  600         if (ISCIRC .eq. 1) then
                  r2n    = 2.d0**(iyx+1)
                  rnum   = (FDIVCO(1)*r2n*OFEED(3)) / (60.d0*RCIPRM(4))
              else
                  rnum   = (FDIVCO(1)*r2n*OFEED(3)) / (60.d0*MOVDIS(3))
              endif
              if (rnum .lt. FEDRNG(1,4)) rnum = FEDRNG(1,4)
              if (rnum .gt. FEDRNG(2,4)) rnum = FEDRNG(2,4)
          endif
c
c......Combination FPM/DPM
c
      else
          IFOTYP = 3
          if (OFEED(5) .lt. FEDRNG(1,3)) OFEED(5) = FEDRNG(1,3)
          if (OFEED(5) .gt. FEDRNG(2,3)) OFEED(5) = FEDRNG(2,3)
          rnum   = OFEED(5)
      endif
c
c...Make sure wrong feed rate mode
c...is not forced out
c
      if (NFDSUP .gt. 1) then
          do 1000 i=1,8,1
              if (FEDCD(i) .ne. 0) then
                  if (REGFRC(FEDCD(i)) .ne. 0 .and. i .ne. IFOTYP) then
                      if (REGFRC(FEDCD(IFOTYP)) .ne. 0)
     -                    REGFRC(FEDCD(IFOTYP)) = REGFRC(FEDCD(i))
                      REGFRC(FEDCD(i)) = 0
                  endif
              endif
 1000    continue
      endif
c
c...Force out feed rate on
c...FEDRAT command or
c...Mode change
c
      if (IFDFRC(1) .eq. 1 .and. IFEDSW .eq. 1 .and.
     1    FEDCD(IFOTYP) .ne. 0 .and. REGFRC(FEDCD(IFOTYP)) .ge. 0)
     2         REGFRC(FEDCD(IFOTYP)) = 1
      if (IFDFRC(2) .eq. 1 .and. IFOTYP .ne. imod .and.
     1    FEDCD(IFOTYP) .ne. 0 .and. REGFRC(FEDCD(IFOTYP)) .ge. 0)
     2         REGFRC(FEDCD(IFOTYP)) = 1
      IFEDSW = 2
c
c...Output feed rate codes
c
      call codout (FEDMCD(IFOTYP),FEDMVL(IFOTYP))
      call codout (FEDCD(IFOTYP),rnum)
c
c......Rotray axes radius
c
      if (ifty .eq. 7 .and. IFDRAD .eq. 1) then
          call rtfrad (rary)
          call wchrad (rary,ircd)
          do 1010 i=1,IRTDEF,1
              if (ircd(i) .eq. 1) call codout (IFDRCD(i),rary(i))
 1010     continue
      endif
c
c...Move after rapid move
c...Force feed rate on next block
c...931112 and feed rate mode code if required
c
      if (IRAPSW .eq. 1) then
          IRAPSW = 0
          if (IFDFRC(3) .eq. 1 .and. FEDCD(IFOTYP) .ne. 0 .and.
     1        REGFRC(FEDCD(IFOTYP)) .ge. 0) REGFRC(FEDCD(IFOTYP)) = 3
          if (IFDFRC(4) .eq. 1 .and. FEDMCD(IFOTYP) .ne. 0) then
              i    = FEDMCD(IFOTYP)
              if (i .lt. 0) call regtyp (i,FEDMVL(IFOTYP))
              if (REGFRC(i) .ge. 0) REGFRC(i) = 3
          end if
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  sfmctl (grad,gdia,kdia)
c
c   FUNCTION:  This routine calculates the RPM or SFM spindle speed
c              depending on which mode is in effect.
c              for output.
c
c   INPUT:  grad    R*8  D1  -  Current radius for lathe programming.
c
c           gdia    R*8  D1  -  Current cutter diameter for mill prog.
c
c           kdia    R*8  D1  -  1 = 'grad' is radius.  2 = diameter.
c                               'kdia' is only used for Lathes.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine sfmctl (grad,gdia,kdia)
c
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (SPNSFM,KPOSMP(3103)), (SPNMOD,KPOSMP(3142))
      equivalence (SPNSCD,KPOSMP(3144))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
c
      integer*4 MACHTP,SPNSFM,SPNMOD,SPNSCD(2),LTMODE,MTPDYN
c
      equivalence (PI    ,POSMAP(0001)), (FTUNT ,POSMAP(0005))
      equivalence (RPMOUT,POSMAP(3012))
      equivalence (MAXSFM,POSMAP(3304)), (SFMSAV,POSMAP(3306))
      equivalence (RPM   ,POSMAP(3307)), (SFM   ,POSMAP(3308))
      equivalence (SFMTOL,POSMAP(3317)), (SPNRAD,POSMAP(3318))
c
      real*8 PI,FTUNT,MAXSFM,RPM,SFM,SFMTOL,SPNRAD,SFMSAV,RPMOUT
c
      integer*4 kdia
c
      real*8 grad,gdia
c
      real*8 circum,rnum,rsav,arad
c
c...Calculate circumference
c...of moving object
c
      arad  = dabs(grad)
      if (MTPDYN .eq. 1 .or. LTMODE .gt. 0) then
          rnum   = gdia
          if (SPNRAD .ne. 0.) rnum = SPNRAD
          circum = (rnum/FTUNT) * PI
      else
          if (kdia .eq. 1) then
              circum = (dabs(arad+SPNRAD)/FTUNT) * 2.d0 * PI
          else
              circum = (dabs(arad+SPNRAD)/FTUNT) * PI
          endif
      endif
c
c...RPM mode
c...Calculate SFM speed
c
      if (SPNMOD .eq. 1) then
          SFM    = RPM    * circum
c
c...SFM mode
c...Calculate RPM speed
c
      else
          rsav   = RPM
          if (circum .eq. 0.) then
              RPM    = MAXSFM
              SFM    = 0.
          else
              SFM    = SFMSAV
              RPM    = SFM    / circum
              if (RPM .gt. MAXSFM) then
                  RPM    = MAXSFM
                  SFM    = RPM    * circum
              endif
         endif
c
c......SFM not supported
c......Output RPM speed
c
          if (SPNSFM .eq. 2) then
              if (dabs(RPM-RPMOUT) .gt. SFMTOL) then
                  call spndon
              else
                  RPM    = RPMOUT
              endif
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  spnclr
c
c   FUNCTION:  This routine forces out a user defined spindle block.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine spnclr
c
      include 'post.inc'
c
      equivalence (SPNBLK,KPOSMP(3123))
c
      integer*4 SPNBLK
c
      integer*4 ifrc
c
c...Output user defined spindle block
c
      if (SPNBLK .ne. 0) call frcblk (SPNBLK,ifrc,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  spncod (kreg,gval)
c
c   FUNCTION:  This routine is used to set up a spindle block register
c              for output.
c
c   INPUT:  kreg    I*4  D1  -  Register to output.
c
c           gval    R*8  D1  -  Value to store in register.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine spncod (kreg,gval)
c
      include 'post.inc'
c
      equivalence (SPNBLK,KPOSMP(3123))
c
      integer*4 SPNBLK
c
      integer*4 kreg
c
      real*8 gval
c
c...User defined spindle block
c...Set register values only
c
      if (SPNBLK .ne. 0) then
          call setcod (kreg,gval)
c
c...Normal spindle block
c...Output spindle code
c
      else
          call codout (kreg,gval)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  spncol (kreg,gval,kfl)
c
c   FUNCTION:  This routine sets up a combination SPINDL/COOLNT on code
c              when it applies.
c
c   INPUT:  kfl     I*4  D1  -  1 = Routine is being called from SPINDL.
c                               2 = COOLNT.
c
c   OUTPUT: kreg    I*4  D1  -  Combination SPINDL/COOLNT on register.
c                               'kreg' will not be changed when a com-
c                               bination code does not apply.
c
c           gval    R*8  D1  -  Value for 'kreg'.
c
c***********************************************************************
c
      subroutine spncol (kreg,gval,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGSW ,KPOSMP(0405))
      equivalence (SPNDCD,KPOSMP(3105)), (COOLCD,KPOSMP(3128))
      equivalence (SPCOCD,KPOSMP(3132)), (LSTCOL,KPOSMP(3140))
      equivalence (SPNDIR,KPOSMP(3141)), (ICOLSW,KPOSMP(3146))
c
      integer*4 REGSW(MAXFMT),SPNDCD(4),COOLCD(4),SPCOCD(6),LSTCOL,
     1          SPNDIR,ICOLSW
c
      equivalence (SPNDVL,POSMAP(3007)), (COOLVL,POSMAP(3294))
      equivalence (SPCOVL,POSMAP(3298)), (RPM   ,POSMAP(3307))
c
      real*8 SPNDVL(4),COOLVL(4),SPCOVL(6),RPM
c
      integer*4 kreg,kfl
c
      real*8 gval
c
      integer*4 inc,ireg,ion
c
      real*8 rval
c
c...Does this machine have a
c...Combination SPINDL/COOLNT register
c
      inc    = (LSTCOL-1) * 2 + SPNDIR
      ireg   = SPCOCD(inc)
      if (SPNDIR .gt. 2 .or. RPM .eq. 0. .or. ICOLSW .ne. 1 .or.
     1    ireg .eq. 0) go to 8000
c
c...Get COOLNT register
c
      if (kfl .eq. 1) then
          ion    = COOLCD(LSTCOL)
          rval   = COOLVL(LSTCOL)
c
c...Get SPINDL register
c
      else
          ion    = SPNDCD(SPNDIR)
          rval   = SPNDVL(SPNDIR)
      endif
c
c...Determine if SPINDL/COOLNT
c...is waiting to be output
c
      call regtyp (ion,rval)
      if (ion .eq. 0 .or. REGSW(ion) .eq. 0) go to 8000
c
c...SPINDL & COOLNT are turned on
c...Return proper code
c
      REGSW(ion) = 0
      kreg   = ireg
      gval   = SPCOVL(inc)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  spndon
c
c   FUNCTION:  This routine outputs a spindle/on block.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine spndon
c
      include 'post.inc'
c
      equivalence (REGSW ,KPOSMP(0405)), (MACHTP,KPOSMP(1201))
      equivalence (NSPRG ,KPOSMP(3101)), (SPNTYP,KPOSMP(3102))
      equivalence (SPNSFM,KPOSMP(3103)), (SPNDCD,KPOSMP(3105))
      equivalence (SPNBCD,KPOSMP(3109))
      equivalence (SPNRCD,KPOSMP(3115)), (SPNFCD,KPOSMP(3118))
      equivalence (NSPNTB,KPOSMP(3125))
      equivalence (SPCOCD,KPOSMP(3132)), (LSTCOL,KPOSMP(3140))
      equivalence (SPNDIR,KPOSMP(3141)), (SPNMOD,KPOSMP(3142))
      equivalence (SPNRNG,KPOSMP(3143)), (SPNSCD,KPOSMP(3144))
      equivalence (ICOLSW,KPOSMP(3146)), (SPNXCD,KPOSMP(3147))
      equivalence (LTMODE,KPOSMP(4125))
c
      integer*4 REGSW(4),NSPRG,SPNTYP,SPNSFM,SPNDCD(4),SPNBCD,
     1          SPNRCD(3),SPNFCD(3),NSPNTB(3),
     2          SPCOCD(6),LSTCOL,SPNDIR,SPNMOD,SPNRNG,SPNSCD(2),
     3          ICOLSW,SPNXCD,LTMODE,MACHTP
c
      equivalence (SPNDVL,POSMAP(3007)), (SPNBVL,POSMAP(3011))
      equivalence (RPMOUT,POSMAP(3012))
      equivalence (SPNRVL,POSMAP(3016)), (SPNFVL,POSMAP(3019))
      equivalence (SPNTCD,POSMAP(3024))
      equivalence (SPNTVL,POSMAP(3159)), (SPCOVL,POSMAP(3298))
      equivalence (RPMSAV,POSMAP(3305)), (SFMSAV,POSMAP(3306))
      equivalence (RPM   ,POSMAP(3307)), (SFM   ,POSMAP(3308))
      equivalence (SPNLMT,POSMAP(3309)), (SPNXVL,POSMAP(3319))
      equivalence (SPMLMT,POSMAP(4930))
c
      real*8 SPNDVL(4),SPNBVL,SPNRVL(3),SPNFVL(3),SPNTVL(135),RPMOUT,
     1       SPCOVL(6),RPMSAV,SFMSAV,RPM,SFM,SPNLMT(2,4),SPNXVL,
     2       SPNTCD(135),SPMLMT(2,4)
c
      integer*4 ist,inc,icod,irg,i
c
      real*8 rnum,splm(2,4)
c
      character*1 lbuf
      character*80 msg
c
c...Set correct spindle speed limits for
c...Mill/Turn machine
c
      if (MACHTP .eq. 4 .and. LTMODE .ne. 0) then
          do 100 i=1,4,1
              splm(1,i) = SPMLMT(1,i)
              splm(2,i) = SPMLMT(2,i)
  100     continue
      else
          do 200 i=1,4,1
              splm(1,i) = SPNLMT(1,i)
              splm(2,i) = SPNLMT(2,i)
  200     continue
      endif

c
c...Auto spindle range select
c
      irg    = SPNRNG
      if (SPNRNG .eq. 4) then
          if (NSPRG .eq. 1) then
              irg    = 2
          else if (NSPRG .eq. 2) then
              irg    = 3
              if (RPM .le. splm(2,1)) irg = 1
          else
              irg    = 3
              if (RPM .le. splm(2,2)) irg = 2
              if (RPM .le. splm(2,1)) irg = 1
          endif
      endif
c
c...RPM mode
c
      if (SPNMOD .eq. 1 .or. SPNSFM .eq. 2) then
          if (SPNSFM .eq. 1) call spncod (SPNFCD(1),SPNFVL(1))
          RPMOUT = RPM
c
c......Output spindle speed
c......Check limits
c
          if (RPM .lt. splm(1,irg)) then
              call perrst ('SPNLMT',1,msg,0,RPM,lbuf,2)
              call perrst (msg,2,msg,0,splm(1,irg),lbuf,2)
              call psterr (1,'SPNMIN',msg,-1)
              RPM    = splm(1,irg)
              if (RPMSAV  .lt. splm(1,irg)) RPMSAV = RPM
c
          else if (RPM .gt. splm(2,irg)) then
              call perrst ('SPNLMT',1,msg,0,RPM,lbuf,2)
              call perrst (msg,2,msg,0,splm(2,irg),lbuf,2)
              call psterr (1,'SPNMAX',msg,-1)
              RPM    = splm(2,irg)
              if (RPMSAV  .gt. splm(2,irg)) RPMSAV = RPM
          endif
c
c.........Direct spindle speed output
c
          if (SPNTYP .eq. 2) then
              rnum   = RPM
c
c.........Spindle table output
c
          else if (SPNTYP .eq. 3) then
              ist    = (irg-1) * 45 + 1
              call tabfnd (SPNTVL(ist),NSPNTB(irg),RPM,inc)
              rnum   = SPNTCD(ist+inc-1)
              RPM    = SPNTVL(ist+inc-1)
c
c.........Percentage of max output
c
          else if (SPNTYP .eq. 4) then
              rnum   = RPM   / splm(2,irg) * 100.D0
              if (rnum .lt. 1.) rnum = 1.
              if (rnum .gt. 99.) rnum = 99.
          endif
          if (SPNDIR .eq. 2) rnum = 0. - rnum
          call spncod (SPNSCD(1),rnum)
c
c...SFM mode
c
      else
          if (SPNSFM .eq. 1) call spncod (SPNFCD(2),SPNFVL(2))
c
c......Output spindle speed
c......Check limits
c
          if (SFM .lt. SPNLMT(1,4)) then
              call perrst ('SPNLMT',1,msg,0,SFM,lbuf,2)
              call perrst (msg,2,msg,0,SPNLMT(1,4),lbuf,2)
              call psterr (1,'SFMMIN',msg,-1)
              SFM    = SPNLMT(1,4)
              if (SFMSAV  .lt. SPNLMT(1,4)) SFMSAV = SFM
c
          else if (SFM .gt. SPNLMT(2,4)) then
              call perrst ('SPNLMT',1,msg,0,SFM,lbuf,2)
              call perrst (msg,2,msg,0,SPNLMT(2,4),lbuf,2)
              call psterr (1,'SFMMAX',msg,-1)
              RPM    = SPNLMT(2,4)
              if (SFMSAV  .gt. SPNLMT(2,4)) SFMSAV = SFM
          endif
c
c.........Direct SFM output
c
          rnum   = SFM
          if (SPNDIR .eq. 2) rnum = 0. - rnum
          call spncod (SPNSCD(2),rnum)
c
c......Output spindle radius
c
          if (SPNXVL .ne. 0.) call spncod (SPNXCD,SPNXVL)
      endif
c
c...Output spindle range
c
      call spncod (SPNRCD(irg),SPNRVL(irg))
c
c...Output spindle direction
c......SPINDL/,BOTH
c
      if (SPNDIR .eq. 3) then
          call spncod (SPNBCD,SPNBVL)
c
c......SPINDL/,(CLW,CCLW)
c
      else
          icod   = SPNDCD(SPNDIR)
          rnum   = SPNDVL(SPNDIR)
          call spncol (icod,rnum,1)
          call spncod (icod,rnum)
      endif
c
c...Clear spindle block
c
      call spnclr
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tabfnd (gtab,kntab,gval,kpt)
c
c   FUNCTION:  This routine finds the closest match for an input value
c              and an input table of values.  The position within the
c              table of the closest match is returned.
c
c   INPUT:  gtab    R*8  Dn  -  Table of values.
c
c           kntab   I*4  D1  -  Number of values in table.
c
c           gval    R*8  D1  -  Value to match against table.
c
c   OUTPUT: kpt     I*4  D1  -  Pointer within table of closest match.
c
c***********************************************************************
c
      subroutine tabfnd (gtab,kntab,gval,kpt)
c
      integer*4 kntab,kpt
c
      real*8 gtab(2),gval
c
      integer*4 i
c
      real*8 rnum
c
c...Find closes match of value
c...in furnished table
c
      call dpoint (gval,rnum,6)
      do 100 i=1,kntab-1,1
          if (rnum .lt. gtab(i)) go to 200
  100 continue
      i      = kntab  + 1
c
c...Found match
c
  200 kpt    = i      - 1
      if (kpt .lt. 1) kpt = 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  svfedc (ksw)
c
c   FUNCTION:  This routine saves the feed rate output code switches and
c              sets them all to zero.
c
c   INPUT:  none
c
c   OUTPUT: ksw     I*4  D8  -  Saved feed rate output code switches.
c
c***********************************************************************
c
      subroutine svfedc (ksw)
c
      include 'post.inc'
c
      equivalence (REGFRC,KPOSMP(0603)), (FEDCD ,KPOSMP(3162))
c
      integer*4 FEDCD(8),REGFRC(92)
c
      integer*4 ksw(8)
c
      integer*4 i
c
c...Save Feed Rate switches and
c...Set to 0
c
      do 100 i=1,8,1
          if (FEDCD(i) .ne. 0) then
              ksw(i) = REGFRC(FEDCD(i))
              REGFRC(FEDCD(i)) = -1
          else
              ksw(i) = 0
          endif
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rsfedc (ksw)
c
c   FUNCTION:  This routine saves the feed rate output code switches and
c              sets them all to zero.
c
c   INPUT:  ksw     I*4  D8  -  Saved feed rate output code switches to
c                               restore.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine rsfedc (ksw)
c
      include 'post.inc'
c
      equivalence (REGFRC,KPOSMP(0603)), (FEDCD ,KPOSMP(3162))
c
      integer*4 FEDCD(8),REGFRC(92)
c
      integer*4 ksw(8)
c
      integer*4 i
c
c...Save Feed Rate switches and
c...Set to 0
c
      do 100 i=1,8,1
          if (FEDCD(i) .ne. 0) REGFRC(FEDCD(i)) = ksw(i)
  100 continue
c
c...End of routine
c
 8000 return
      end
