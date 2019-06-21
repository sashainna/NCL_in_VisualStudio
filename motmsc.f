c
c***********************************************************************
c
c   FILE NAME:  motmsc
c   CONTAINS:
c               axssub  gmotrg  lmtchk  mchdis  tmpdrs  tmpdlt  mchtim
c               popaxs  popcmd  pshaxs  pshcmd  rtfrad  wchrad  gttvec
c               rrglmt  axsacl  cpyrot
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        motmsc.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 09:22:40
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  axssub (ksc,ksp,kfl)
c
c   FUNCTION:  This routine returns machine linear axes register sub-
c              scripts (for active axes) when given are cartesian
c              axes subscripts.
c
c   INPUT:  ksc     I*4  D3  -  Cartesian axes subscripts (X = 1, Y = 2,
c                               Z = 3).
c
c           kfl     I*4  D1  -  1 = Do not sort subscripts (i.e. assume
c                               always X = 1 etc.), 2 = sort subscripts
c                               (assume that ksc contains axes sub-
c                               scripts in proper order).
c
c   OUTPUT: ksp     I*4  D3  -  Machine linear axes subsripts.
c
c***********************************************************************
c
      subroutine axssub (ksc,ksp,kfl)
c
      integer*4 ksc(3),ksp(3),kfl
c
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (ACTLIN,KPOSMP(1205))
c
      integer*4 NUMLIN(3),ACTLIN(3)
c
      integer*4 i,j,isw(2,3)
c
      data isw /1,2, 3,4, 5,6/
c
      do 300 i=1,3,1
c
c...Not sorted
c
          if (kfl .eq. 1) then
              j = i
c
c...Sorted
c
          else
              j = ksc(i)
          end if
          ksp(j) = 0
          if (NUMLIN(i) .ne. 0) then
              ksp(j) = isw(ACTLIN(j),j)
          endif
  300 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gmotrg (kax,kreg)
c
c   FUNCTION:  This routine returns the register assigned to the input
c              linear axis.
c
c   INPUT:  kax     I*4  D3  -  Linear axis requesting motion register
c                               for (X=1, Y=2, Z=3).
c
c   OUTPUT: kreg    I*4  D3  -  Register assigned to this axis.
c
c***********************************************************************
c
      subroutine gmotrg (kax,kreg)
c
      integer*4 kax,kreg
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (ACTLIN,KPOSMP(1205))
c
      integer*4 ACTLIN(3),MOTREG(24)
c
      integer*4 isub(2,3)
c
      data isub /1,3, 5,7, 9,11/
c
c...Get proper register for linear axis
c
      kreg   = MOTREG(isub(ACTLIN(kax),kax))
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lmtchk (glin,kfl,kaxs,kwrn)
c
c   FUNCTION:  This routine checks the axes values against the machine
c              limits and optionally outputs an error message if the
c              limits are exceeded.
c
c   INPUT:  glin    R*8  D10 -  Linear axes values to check against
c                               limits.
c
c           kwrn    I*4  D1  -  1 = Output warning when axis exceeds
c                               limits.  2 = Find axes exactly on limit
c                               as well as past limits.
c
c   OUTPUT: kfl     I*4  D1  -  Returns the number of axes that are out
c                               of limits.
c
c           kaxs    I*4  D10 -  Flag for each axis that is out of
c                               limits.  0 = Within limits.  1 = Outside
c                               lower limit.  2 = Outside upper limit.
c
c                               When 'kwrn' equals 2, then the following
c                               may also be returned.  3 = On lower limit,
c                               4 = On upper limit.
c
c***********************************************************************
c
      subroutine lmtchk (glin,kfl,kaxs,kwrn)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (NUMLIN,KPOSMP(1202))
      equivalence (IRTNUM,KPOSMP(1243)), (LASTAB,KPOSMP(1260))
      equivalence (NOPIVS,KPOSMP(1282)), (NOTABS,KPOSMP(1283))
      equivalence (NUMINF,KPOSMP(1396)), (NOTOOL,KPOSMP(1802))
      equivalence (REGBNC,KPOSMP(2001))
c
      integer*4 REGBNC(MAXFMT),IRTNUM,MOTREG(24),NUMINF,NUMLIN(3),
     1          NOPIVS,NOTOOL,LASTAB,NOTABS
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (INFZON,POSMAP(0071)),(LIMITS,POSMAP(1254))
c
      real*8 LIMITS(2,10),INFZON(2,10,4),DUMMY
c
      equivalence (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
c
      integer*4 kfl,kaxs(10),kwrn
c
      real*8 glin(10)
c
      integer*4 i,j,idid,inum,iax(10),jax(10),ipsav,itsav,ibsav
c
      real*8 rnum(10),rlin(6),rmch(3,4),rrot(20,2),rvec(3),raxs(10),
     1       lim(2)
c
      character*1 lax(10)
      character*20 lbuf
      character*80 msg1,msg2
c
      data iax /1,3,5,7,9,11,13,16,19,22/
      data jax /1,3,5,7,9,11,13,16,19,22/
      data lax /'X','U','Y','V','Z','W','A','B','C','D'/
c
c...Adjustments are not made for
c...Pivot or Tool Length
c...Make those adjustments here
c
      if ((NOPIVS .eq. 2 .and. LASTAB .lt. IRTNUM) .or.
     1    (NOTABS .eq. 2 .and. LASTAB .gt. 0) .or.
     2    (IRTNUM .ne. 0 .and. NOTOOL .eq. 2)) then
c
c......First create input coordinates
c
          call alladr (glin,rlin,rmch,rrot,rvec,5,2)
c
c......Then setup correct flags
c
          ipsav  = NOPIVS
          itsav  = NOTOOL
          ibsav  = NOTABS
          NOPIVS = 1
          NOTOOL = 1
          NOTABS = 1
c
c......And adjust for Pivot and Tool Length
c
          call alladj (rmch,rlin,raxs,rrot,2,5)
c
c......Now reset flags
c
          NOPIVS = ipsav
          NOTOOL = itsav
          NOTABS = ibsav
c
c...Use input axes as is
c
      else
          do 60 i=1,10,1
              raxs(i) = glin(i)
   60     continue
      endif
c
c...Work with non-transformed axes
c
      call axsxfr (raxs,raxs)
c
c...Make sure limits are checked
c...when a linear axis does not exist
c...(neither primary nor secondary)
c
      do 100 i=1,3,1
          if (NUMLIN(i) .eq. 0) then
              if (i .eq. 1) then
                  iax(i*2-1) = 3
              else
                  iax(i*2-1) = 1
              endif
          endif
  100 continue
c
c...Check linear axes against limits
c
      kfl    = 0
      do 1000 i=1,10,1
          kaxs(i) = 0
          call codint (MOTREG(iax(i)),raxs(i),rnum(i),inum)
c
c......Lower limit
c
          if (rnum(i) .lt. LIMITS(1,i)) then
              kfl    = kfl    + 1
              kaxs(i) = 1
              if (kwrn .eq. 1) then
                  if (MOTREG(jax(i)) .eq. 0 .or.
     1                REGBNC(MOTREG(jax(i))) .eq. 0) then
                      lbuf = lax(i)
                  else
                      lbuf = REGST(MOTREG(jax(i)))
                  endif
                  call perrst ('LMTLWR',1,msg1,0,rnum(i),lbuf,3)
                  call perrst ('LMTMSG',1,msg2,0,rnum(i),lbuf,2)
                  call perrst (msg2,2,msg2,0,LIMITS(1,i),lbuf,2)
                  call psterr (1,msg1,msg2,-1)
              endif
c
c......Upper limit
c
          else if (rnum(i) .gt. LIMITS(2,i)) then
              kfl    = kfl    + 1
              kaxs(i) = 2
              if (kwrn .eq. 1) then
                  if (MOTREG(jax(i)) .eq. 0 .or.
     1                REGBNC(MOTREG(jax(i))) .eq. 0) then
                      lbuf = lax(i)
                  else
                      lbuf = REGST(MOTREG(jax(i)))
                  endif
                  call perrst ('LMTUPR',1,msg1,0,rnum(i),lbuf,3)
                  call perrst ('LMTMSG',1,msg2,0,rnum(i),lbuf,2)
                  call perrst (msg2,2,msg2,0,LIMITS(2,i),lbuf,2)
                  call psterr (1,msg1,msg2,-1)
              endif
          endif
c
c......Check for axis exactly on limit
c
          if (kwrn .eq. 2) then
              call codint (MOTREG(iax(i)),LIMITS(1,i),lim(1),inum)
              call codint (MOTREG(iax(i)),LIMITS(2,i),lim(2),inum)
              if (rnum(i) .eq. lim(1)) then
                  kfl    = kfl    + 1
                  kaxs(i) = 3
              else if (rnum(i) .eq. lim(2)) then
                  kfl    = kfl    + 1
                  kaxs(i) = 4
              endif
          endif
 1000 continue
c
c......Interference zones
c......Don't check if limits are exceeded
c
      if (kfl .ne. 0) go to 8000
      do 2000 i=1,NUMINF,1
          idid   = 0
          do 1500 j=1,10,1
              if (INFZON(1,j,i) .eq. DUMMY) go to 1500
c
              if (INFZON(2,j,i) .eq. DUMMY) then
                  if (rnum(j) .ne. INFZON(1,j,i)) go to 2000
c
              else
                  if (rnum(j) .lt. INFZON(1,j,i) .or.
     1                rnum(j) .gt. INFZON(2,j,i)) go to 2000
              endif
              idid   = 1
 1500     continue
c
c......Tool currently in interference zone
c......Output error message
c
          if (idid .eq. 1) then
              if (kwrn .eq. 1) then
                  call perrst ('INFZON',1,msg1,i,rnum(i),lbuf,1)
                  call psterr (1,msg1,' ',-1)
              endif
          endif
 2000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mchdis
c
c   FUNCTION:  This routine performs the following calculations:
c
c              1) Calculates the delta movement for each adjustment of
c                 the XYZ axes, linear axes, rotary axes and output
c                 axes.
c
c              2) Accumulates delta movement for all of the above
c                 axes.
c
c              3) Determines lower and upper movement limits for all of
c                 the above axes.
c
c              4) Calculates the delta movement for the tool tip, feed
c                 control point, output linear & rotary axes.
c
c   INPUT:  kfl     I*4  D1  -  1 = Add machining distances to running
c                               totals.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mchdis (kfl)
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (IJKREG,KPOSMP(0827))
      equivalence (XFMFL ,KPOSMP(0969))
      equivalence (MACHTP,KPOSMP(1201)), (LTHDIA,KPOSMP(1228))
      equivalence (ISCIRC,KPOSMP(1238)), (IRTNUM,KPOSMP(1243))
      equivalence (LASTAB,KPOSMP(1260))
      equivalence (NOPIVS,KPOSMP(1282)), (NOTABS,KPOSMP(1283))
      equivalence (IJKROT,KPOSMP(1739))
      equivalence (ITMDLT,KPOSMP(1740)), (ITP   ,KPOSMP(1801))
      equivalence (NOTOOL,KPOSMP(1802)), (IFDLTP,KPOSMP(3187))
      equivalence (NUMLIN,KPOSMP(1202)), (ACTLIN,KPOSMP(1205))
      equivalence (IRTINC,KPOSMP(1461)), (IRTDEF,KPOSMP(1485))
      equivalence (ISBSPL,KPOSMP(4085))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
c
      integer*4 ITP,IFDLTP,MOTREG(24),IRTNUM,IRTINC(4),IJKROT,
     1          LTHDIA(2),MACHTP,NUMLIN(3),ACTLIN(3),MTPDYN,
     2          LTMODE,ISBSPL,ITMDLT,NOPIVS,NOTABS,NOTOOL,XFMFL(20),
     3          LASTAB,ISCIRC,IRTDEF,IJKREG(3)
c
      equivalence (PI    ,POSMAP(0001)), (RAD   ,POSMAP(0002))
      equivalence (TMPDIS,POSMAP(0332)), (TMRDIS,POSMAP(0344))
      equivalence (TMLDIS,POSMAP(0348)), (TMADIS,POSMAP(0354))
      equivalence (MCHDLS,POSMAP(1221))
      equivalence (ROTDLS,POSMAP(1233)), (AXSDLS,POSMAP(1237))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399)), (AXSSTO,POSMAP(1425))
      equivalence (IJKDIS,POSMAP(1453))
      equivalence (MCHDLT,POSMAP(1474)), (ROTDLT,POSMAP(1486))
      equivalence (LINDLT,POSMAP(1490)), (AXSDLT,POSMAP(1496))
      equivalence (MCHMIN,POSMAP(1506)), (MCHMAX,POSMAP(1518))
      equivalence (ROTMIN,POSMAP(1530)), (ROTMAX,POSMAP(1534))
      equivalence (LINMIN,POSMAP(1538)), (LINMAX,POSMAP(1544))
      equivalence (AXSMIN,POSMAP(1550)), (AXSMAX,POSMAP(1560))
      equivalence (MOVDIS,POSMAP(1570)), (AXSDIS,POSMAP(1574))
      equivalence (LINDLS,POSMAP(1594))
      equivalence (RCIPRM,POSMAP(2049)), (FEDLEN,POSMAP(3539))
      equivalence (DPMBPW,POSMAP(3551)), (TL    ,POSMAP(3601))
      equivalence (ADJDIS,POSMAP(4505))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),VECSAV(3),PI,
     1       STONUM(3,4),LINSTO(6),ROTANG(20,2),ROTSTO(20,2),AXSSTO(10),
     2       MCHDLT(3,4),ROTDLT(4),LINDLT(6),AXSDLT(10),MCHMIN(3,4),
     3       MCHMAX(3,4),ROTMIN(4),ROTMAX(4),LINMIN(6),LINMAX(6),IJKDIS,
     4       AXSMIN(10),AXSMAX(10),MOVDIS(4),FEDLEN,TL(120)
      real*8 AXSDIS(10),DPMBPW(2),MCHDLS(3,4),ROTDLS(4),AXSDLS(10),
     1       LINDLS(6),RCIPRM(25),RAD,TMPDIS(3,4),TMRDIS(4),TMLDIS(6),
     2       TMADIS(10),ADJDIS(10)
c
      equivalence (BSPAN ,BSPMAP(07))
c
      real*8 BSPAN
c
      integer*4 kfl
c
      integer*4 i,j,k,ireg(10),ipln(2,3),kreg,iadj,ipsav,itsav,ip1,inum
c
      real*8 mdis(3,4),rdis(4),ldis(6),vd1,vd2,mnum(3),snum(3),leng,
     1       rnum,adis(10),maxlin,maxrot,rtrad(4),rperc,ladis,r1,r2,
     2       rout(10),rsto(10),rlin(6),rmch(3,4),rrot(20,2),rvec(3),
     3       rlst(6),rmst(3,4),rrst(20,2),rvst(3),raxs(10),axst(10),
     4       rnum1,rnum2
c
      data ireg /1,3,5,7,9,11,14,17,20,23/
      data ipln /2,3, 1,3, 1,2/
c
c...Work with non-transformed axes
c
      call axsxfr (AXSOUT,rout)
      call axsxfr (AXSSTO,rsto)
c
c...If not points are not adjusted for rotary axes
c...then create set of adjusted points to use
c...with feedrate limit calculations
c
      iadj   = 0
      IJKDIS = 0.
      if (NOPIVS .eq. 2 .and. NOTOOL .eq. 2 .and. LASTAB .lt. IRTNUM)
     1        then
          iadj   = 1
c
c......First create input coordinates
c
          call alladr (rout,rlin,rmch,rrot,rvec,5,2)
          call alladr (rsto,rlst,rmst,rrst,rvst,5,2)
c
c......Then setup correct flags
c
          ipsav  = NOPIVS
          itsav  = NOTOOL
          NOPIVS = 1
          NOTOOL = 1
c
c......And adjust for Pivot and Tool Length
c
          call alladj (rmch,rlin,raxs,rrot,2,5)
          call alladj (rmst,rlst,axst,rrst,2,5)
c
c......Now reset flags
c
          NOPIVS = ipsav
          NOTOOL = itsav
      endif
c
c...Accumulate individual axis distances &
c...Set minimum & maximum travel limits
c
      if (ISBSPL .eq. 1) go to 500
      do 200 i=1,4,1
          do 100 j=1,3,1
c
c...Round values to primary X, Y and Z axes to fix small variations in
c...feedrate for small moves with large feedrate values.
c
              if (ITMDLT .eq. 1) then
                  mdis(j,i) = TMPDIS(j,i)
              else
                  kreg = MOTREG((j-1)*4+1)
                  if (kreg .eq. 0) then
                      if (j .eq. 1) then
                          kreg = MOTREG((2-1)*4+1)
                      else
                          kreg = MOTREG((1-1)*4+1)
                      endif
                  endif
                  call codint (kreg, MCHNUM(j,i), r1, k)
                  call codint (kreg, STONUM(j,i), r2, k)
                  mdis(j,i) = dabs(r1-r2)
              endif
              if (kfl .eq. 1) then
                  MCHDLS(j,i) = MCHDLS(j,i) + mdis(j,i)
                  MCHDLT(j,i) = MCHDLT(j,i) + mdis(j,i)
                  if (MCHNUM(j,i) .lt. MCHMIN(j,i))
     1                MCHMIN(j,i) = MCHNUM(j,i)
                  if (MCHNUM(j,i) .gt. MCHMAX(j,i))
     1                MCHMAX(j,i) = MCHNUM(j,i)
              endif
  100     continue
          ip1    = IRTINC(i)
          if (ITMDLT .eq. 1) then
              rdis(i) = TMRDIS(i)
          else
              if (ip1 .ne. 0) then
                  rdis(i) = dabs(ROTANG(ip1,2)-ROTSTO(ip1,2))
              else
                  rdis(i) = 0.
              endif
          endif
          if (kfl .eq. 1) then
              ROTDLS(i) = ROTDLS(i) + rdis(i)
              ROTDLT(i) = ROTDLT(i) + rdis(i)
              if (ROTANG(ip1,2) .lt. ROTMIN(i)) ROTMIN(i)= ROTANG(ip1,2)
              if (ROTANG(ip1,2) .gt. ROTMAX(i)) ROTMAX(i)= ROTANG(ip1,2)
          endif
  200 continue
c
      do 300 i=1,6,1
          if (ITMDLT .eq. 1) then
              ldis(i) = TMLDIS(i)
          else
              ldis(i) = dabs(LINAXS(i)-LINSTO(i))
          endif
          if (kfl .eq. 1) then
              LINDLS(i) = LINDLS(i) + ldis(i)
              LINDLT(i) = LINDLT(i) + ldis(i)
              if (LINAXS(i) .lt. LINMIN(i)) LINMIN(i) = LINAXS(i)
              if (LINAXS(i) .gt. LINMAX(i)) LINMAX(i) = LINAXS(i)
          endif
  300 continue
c
      maxlin = 0.d0
      maxrot = 0.d0
      do 400 i=1,10,1
          if (ITMDLT .eq. 1) then
              adis(i) = TMADIS(i)
          else
c
c...Round values to primary X, Y and Z axes to fix small variations in
c...feedrate for small moves with large feedrate values.
c
              kreg = MOTREG(ireg(i))
              call codint (kreg, rout(i), r1, k)
              call codint (kreg, rsto(i), r2, k)
              adis(i) = dabs(r1-r2)
              if (iadj .eq. 1) then
                  call codint (kreg, raxs(i), r1, k)
                  call codint (kreg, axst(i), r2, k)
                  ADJDIS(i) = dabs(r1-r2)
              endif
          endif
          if (i .le. 6) then
              if (i .le. 2 .and. MTPDYN .eq. 2) then
                 if (LTMODE .eq. 0 .and. LTHDIA(2) .eq. 2)
     1                adis(i) = adis(i) / 2.
              end if
              if (ITMDLT .eq. 1 .or. XFMFL(4) .ne. 0) then
                  AXSDIS(i) = adis(i)
              else
                  call increg (MOTREG(ireg(i)),rout(i),rnum)
                  AXSDIS(i) = dabs(rnum)
              endif
              if (iadj .eq. 1) then
                  if (ADJDIS(i) .gt. maxlin) maxlin = ADJDIS(i)
              else
                  if (adis(i) .gt. maxlin) maxlin = adis(i)
              endif
          else
              AXSDIS(i) = adis(i)
              if (adis(i) .gt. maxrot) maxrot = adis(i)
          endif
          if (kfl .eq. 1) then
              AXSDLS(i) = AXSDLS(i) + AXSDIS(i)
              AXSDLT(i) = AXSDLT(i) + AXSDIS(i)
              if (rout(i) .lt. AXSMIN(i)) AXSMIN(i) = rout(i)
              if (rout(i) .gt. AXSMAX(i)) AXSMAX(i) = rout(i)
          endif
  400 continue
c
c...Circular interpolation
c...machining distances
c
  500 if (ISCIRC .eq. 1) then
          MOVDIS(1) = (2.d0 * PI * RCIPRM(4)) * (RCIPRM(7)/360.d0)
          MOVDIS(2) = MOVDIS(1)
          MOVDIS(3) = MOVDIS(1)
          MOVDIS(4) = 0.
          go to 8000
      endif
c
c...Bspline interpolation
c
      if (ISBSPL .eq. 1) then
          MOVDIS(1) = BSPAN
          MOVDIS(2) = MOVDIS(1)
          MOVDIS(3) = MOVDIS(1)
          MOVDIS(4) = 0.
          go to 8000
      endif
c
c...Check if secondary axes move together with primary
c...and discard this motion if so.
c
      do 505 i=1,3,1
         if (NUMLIN(i) .eq. 2) adis(2*i+1-ACTLIN(i)) = 0.d0
         if (NUMLIN(i) .eq. 0) then
             adis(2*i-1) = mdis(i,4)
             adis(2*i) = 0.
         endif
  505 continue

c
c...Get delta distance to use
c...for machining time calculations
c
      MOVDIS(1) = dsqrt (mdis(1,2)**2 + mdis(2,2)**2 + mdis(3,2)**2)
      if (iadj .eq. 1) then
          MOVDIS(3) = dsqrt ((ADJDIS(1) + ADJDIS(2))**2 + (ADJDIS(3) +
     1                       ADJDIS(4))**2 + (ADJDIS(5) + ADJDIS(6))**2)
      else
          MOVDIS(3) = dsqrt ((adis(1) + adis(2))**2 + (adis(3) +
     1                       adis(4))**2 + (adis(5) + adis(6))**2)
      endif
      vd1    = 0.0
      do 510 i=1,IRTDEF,1
          vd1 = vd1 + adis(6+i)**2
  510 continue
      MOVDIS(4) = dsqrt (vd1)
c     MOVDIS(4) = dsqrt (adis(7)**2 + adis(8)**2 + adis(9)**2 +
c    1                   adis(10)**2)
c
c......No rotary angle movement
c
cc      if (MOVDIS(4) .eq. 0. .or. ((NOPIVS .eq. 2 .or. NOTABS .eq. 2)
      if (MOVDIS(4) .eq. 0. .or. NOTABS .eq. 2 .and.
     1       NOTOOL .eq. 2) then
          MOVDIS(2) = MOVDIS(1)
          if (iadj .eq. 0 .and. dabs(MOVDIS(3)-MOVDIS(1)) .lt. .005)
     1        MOVDIS(3) = MOVDIS(1)
c
c......Get feed rate control point distance
c
      else
          vd1    = dsqrt (TLVEC(1)**2 + TLVEC(2)**2 + TLVEC(3)**2)
          vd2    = dsqrt (VECSAV(1)**2 + VECSAV(2)**2 + VECSAV(3)**2)
c
          leng   = FEDLEN
          if (IFDLTP .eq. 2) leng = TL(ITP) * FEDLEN
          do 700 i=1,3,1
              mnum(i) = MCHNUM(i,2) + ((TLVEC(i)/vd1) * leng)
              snum(i) = STONUM(i,2) + ((VECSAV(i)/vd2) * leng)
  700     continue
c
c.........Linear axes move the most
c
          ladis = dsqrt ((mnum(1)-snum(1))**2 +
     1                   (mnum(2)-snum(2))**2 +
     2                   (mnum(3)-snum(3))**2)
          if (maxlin .gt. (maxrot*(DPMBPW(1)/DPMBPW(2)))) then
              MOVDIS(2) = ladis
c
c.........Rotary axes move the most
c
          else
              MOVDIS(2) = 0.
              call rtfrad (rtrad)
              do 1000 i=1,IRTDEF,1
                  MOVDIS(2) = MOVDIS(2) +
     1                        (rtrad(i)*2.d0*PI * AXSDIS(i+6)/360.d0)**2
 1000         continue
              MOVDIS(2) = dsqrt(MOVDIS(2))

c
c.............Always take the longest distance
c.............because sometimes the pulse weight is set wrong
c.............Better to be safe with slower feed rates
c
              if (MOVDIS(2) .lt. ladis) then
                  MOVDIS(2) = ladis
c
c............Take away portion of linear move
c
              else
                  rperc = MOVDIS(3) / (MOVDIS(4)*(DPMBPW(1)/DPMBPW(2)))
                  rperc = rperc * 1.25
                  if (rperc .ge. 1.) then
                      MOVDIS(2) = ladis
                  else
                      MOVDIS(2) = MOVDIS(2) - (MOVDIS(2)-ladis)*rperc
                  endif
              endif
          endif
      endif
c
c...Calculate tool axis distance
c
      if (IJKROT .eq. 1) then
          do 2000 i=1,3,1
             call codint (IJKREG(i),VECSAV(i),rnum1,inum)
             call codint (IJKREG(i),TLVEC(i),rnum2,inum)
             IJKDIS = IJKDIS + dabs(rnum2-rnum1)
 2000     continue
       endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tmpdrs
c
c   FUNCTION:  This routine resets the delta movement values for canned
c              routines (circular, cycles, etc.).
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine tmpdrs
c
      include 'post.inc'
c
      equivalence (TMPDIS,POSMAP(0332)), (TMRDIS,POSMAP(0344))
      equivalence (TMLDIS,POSMAP(0348)), (TMADIS,POSMAP(0354))
c
      real*8 TMPDIS(3,4),TMRDIS(4),TMLDIS(6),TMADIS(10)
c
      integer*4 i,j
c
c...Reset temporary machining deltas
c
      do 200 i=1,4,1
          do 100 j=1,3,1
              TMPDIS(j,i) = 0.
  100     continue
          TMRDIS(i) = 0.
  200 continue
c
      do 300 i=1,6,1
          TMLDIS(i) = 0.
  300 continue
c
      do 400 i=1,10,1
          TMADIS(i) = 0.
  400 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tmpdlt (gmch,glin,grot,gaxs,gmsto,glsto,grsto,gasto)
c
c   FUNCTION:  This routine calculates the delta movement for all axes
c              during a canned routine (circular, cycles, etc.).  The
c              calculated values are stored in global arrays and will
c              be added to the actual deltas in 'mchdis'.
c
c   INPUT:  gmch    R*8  D3.4 -  Current XYZ positions.
c
c           glin    R*8  D6   -  Current linear axes position.
c
c           grot    R*8  D20.2  -  Current rotary axes position.
c
c           gaxs    R*8  D10  -  Current output axes position.
c
c           gmsto   R*8  D3.4 -  Previous XYZ positions.
c
c           glsto   R*8  D6   -  Previous linear axes position.
c
c           grsto   R*8  D20.2  -  Previous rotary axes position.
c
c           gasto   R*8  D10  -  Previous output axes position.
c
c           kfl     I*4  D1   -  1 = Save 'Current' positions in 'Previous'
c                                positions.
c
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine tmpdlt (gmch,glin,grot,gaxs,gmsto,glsto,grsto,gasto,
     1                   kfl)
c
      include 'post.inc'
c
      equivalence (IRTINC,KPOSMP(1461))
c
      integer*4 IRTINC(4)
c
      equivalence (TMPDIS,POSMAP(0332)), (TMRDIS,POSMAP(0344))
      equivalence (TMLDIS,POSMAP(0348)), (TMADIS,POSMAP(0354))
      equivalence (MCHMIN,POSMAP(1506)), (MCHMAX,POSMAP(1518))
      equivalence (ROTMIN,POSMAP(1530)), (ROTMAX,POSMAP(1534))
      equivalence (LINMIN,POSMAP(1538)), (LINMAX,POSMAP(1544))
      equivalence (AXSMIN,POSMAP(1550)), (AXSMAX,POSMAP(1560))
c
      real*8 MCHMIN(3,4),MCHMAX(3,4),ROTMIN(4),ROTMAX(4),LINMIN(6),
     1       LINMAX(6),AXSMIN(10),AXSMAX(10),TMPDIS(3,4),TMRDIS(4),
     2       TMLDIS(6),TMADIS(10)
c
      integer*4 kfl
c
      real*8 gmch(3,4),glin(6),grot(20,2),gaxs(10),gmsto(3,4),glsto(6),
     1       grsto(20,2),gasto(10)
c
      integer*4 i,j,ip1
c
      real*8 rout(10),rsto(10)
c
c...Work with non-transformed axes
c
      call axsxfr (gaxs,rout)
      call axsxfr (gasto,rsto)
c
c...Accumulate individual axis distances &
c...Set minimum & maximum travel limits
c......MCHNUM & ROTANG
c
      do 200 i=1,4,1
          do 100 j=1,3,1
              TMPDIS(j,i) = TMPDIS(j,i) + dabs(gmch(j,i)-gmsto(j,i))
              if (kfl .eq. 1) gmsto(j,i) = gmch(j,i)
              if (gmch(j,i) .lt. MCHMIN(j,i)) MCHMIN(j,i) = gmch(j,i)
              if (gmch(j,i) .gt. MCHMAX(j,i)) MCHMAX(j,i) = gmch(j,i)
  100     continue
          ip1    = IRTINC(i)
          if (ip1 .eq. 0) then
              TMRDIS(i) = 0.
          else
              TMRDIS(i) = TMRDIS(i) + dabs(grot(ip1,2)-grsto(ip1,2))
              if (kfl .eq. 1) grsto(ip1,2) = grot(ip1,2)
              if (grot(ip1,2) .lt. ROTMIN(i)) ROTMIN(i) = grot(ip1,2)
              if (grot(ip1,2) .gt. ROTMAX(i)) ROTMAX(i) = grot(ip1,2)
          endif
  200 continue
c
c......LINAXS
c
      do 300 i=1,6,1
          TMLDIS(i) = TMLDIS(i) + dabs(glin(i)-glsto(i))
          if (kfl .eq. 1) glsto(i) = glin(i)
          if (glin(i) .lt. LINMIN(i)) LINMIN(i) = glin(i)
          if (glin(i) .gt. LINMAX(i)) LINMAX(i) = glin(i)
  300 continue
c
c......AXSOUT
c
      do 400 i=1,10,1
          TMADIS(i) = TMADIS(i) + dabs(rout(i)-rsto(i))
          if (kfl .eq. 1) rsto(i) = rout(i)
          if (rout(i) .lt. AXSMIN(i)) AXSMIN(i) = rout(i)
          if (rout(i) .gt. AXSMAX(i)) AXSMAX(i) = rout(i)
  400 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mchtim (gaxs,kfl,kcnt,kfl)
c
c   FUNCTION:  This routine performs the following calculations:
c
c              1) Calculates the machining time for the current move.
c
c              2) Calculates the actual feed rates for for the tool tip,
c                 feed control point, output linear & rotary axes.
c
c   INPUT:  gaxs    R*8  D10 -  Machine axis positions to use to calculate
c                               machining time.
c
c           kaxsw   I*4  D10 -  Machine axes output switches.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  1 = Add machining times to running
c                               totals.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mchtim (gaxs,kaxsw,kcnt,kfl)
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381))
      equivalence (MACHTP,KPOSMP(1201)), (LTHDIA,KPOSMP(1228))
      equivalence (IRTNUM,KPOSMP(1243)), (LASTAB,KPOSMP(1260))
      equivalence (NOPIVS,KPOSMP(1282)), (IRTDEF,KPOSMP(1485))
      equivalence (IACLFL,KPOSMP(1732)), (ITP   ,KPOSMP(1801))
      equivalence (NOTOOL,KPOSMP(1802)), (IFDCTP,KPOSMP(3157))
      equivalence (IRPSUP,KPOSMP(3189)), (IRAP  ,KPOSMP(3199))
      equivalence (POSFED,KPOSMP(3209))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
      equivalence (IFDADJ,KPOSMP(3243))
c
      integer*4 ITP,MACHTP,LTHDIA(2),MTPDYN,POSFED,IRAP,IRTNUM,
     1          LTMODE,IRPSUP,MOTREG(24),IFDCTP,NOPIVS,NOTOOL,LASTAB,
     2          IFDADJ,IACLFL,IRTDEF
c
      equivalence (BRKPRM,POSMAP(1215)), (PPTOLR,POSMAP(1274))
      equivalence (AXSOUT,POSMAP(1340)), (AXSSTO,POSMAP(1425))
      equivalence (MOVDIS,POSMAP(1570)), (AXSDIS,POSMAP(1574))
      equivalence (RPM   ,POSMAP(3307))
      equivalence (FEDLMT,POSMAP(3528)), (RDRLMT,POSMAP(3538))
      equivalence (FEDLEN,POSMAP(3539)), (MAXIPM,POSMAP(3544))
      equivalence (MAXDPM,POSMAP(3545)), (MOVTIM,POSMAP(3546))
      equivalence (FEED  ,POSMAP(3547)), (DPMBPW,POSMAP(3551))
      equivalence (FMVTIM,POSMAP(3553))
      equivalence (OFEED ,POSMAP(3560)), (TIMSCL,POSMAP(3566))
      equivalence (RAPLMT,POSMAP(3567)), (ACCFED,POSMAP(4291))
      equivalence (ADJDIS,POSMAP(4505))
c
      real*8 MOVDIS(4),RDRLMT,FEDLEN,MAXIPM,MAXDPM,MOVTIM,FMVTIM,
     1       FEED(4),DPMBPW(2),OFEED(6),FEDLMT(10),RPM,AXSDIS(10),
     2       TIMSCL,BRKPRM(5),PPTOLR(10),RAPLMT(10),ACCFED,ADJDIS(10),
     3       AXSOUT(10),AXSSTO(10)
c
      integer*4 kaxsw(10),kcnt,kfl
c
      real*8 gaxs(10)
c
      integer*4 i,irpmod
c
      real*8 rfed,rnum,mdis(3),mtim
c
c...Work with rounded numbers
c
      call codint (MOTREG(1),MOVDIS(1),mdis(1),i)
      call codint (MOTREG(1),MOVDIS(2),mdis(2),i)
      call codint (MOTREG(1),MOVDIS(3),mdis(3),i)
      if (mdis(1) .lt. PPTOLR(1)) mdis(1) = PPTOLR(1)
      if (dabs(mdis(2)-mdis(1)) .le. PPTOLR(1)*1.5) mdis(2) = mdis(1)
      if (dabs(mdis(3)-mdis(1)) .le. PPTOLR(1)*1.5) mdis(3) = mdis(1)
c
c...Determine rapid mode
c
      irpmod = 0
      if (IRAP .eq. 1 .and. IRPSUP .eq. 1) irpmod = 1
c
c...Calculate machining time
c
      rfed   = FEED(1)
      if (POSFED .eq. 2) rfed = MAXDPM
      if (mdis(2) .eq. 0. .or. rfed .eq. 0.) then
          MOVTIM = .00001
      else
          MOVTIM = mdis(2) / rfed
      endif
      mtim   = MOVTIM
c
c...Make sure feedrate is not too fast
c......Axes feed limits
c
      do 1000 i=1,6+IRTDEF,1
          rnum   = AXSDIS(i)
          if (NOPIVS .eq. 2 .and. NOTOOL .eq. 2 .and.
     1        LASTAB .lt. IRTNUM) rnum = ADJDIS(i)
          rfed   = rnum   / MOVTIM
          if (rfed .gt. FEDLMT(i) .and. irpmod .eq. 0) then
              MOVTIM = rnum / FEDLMT(i)
          else if (rfed .gt. RAPLMT(i) .and. irpmod .eq. 1) then
              MOVTIM = rnum / RAPLMT(i)
          endif
 1000 continue
c
c......Maximum FPM
c
      rfed   = mdis(1) / MOVTIM
      if (rfed .gt. MAXIPM .and. irpmod .eq. 0)
     1    MOVTIM = mdis(1) / MAXIPM
c
c...Don't do during 2-axis lathe mode
c
      if (MTPDYN .eq. 1 .or. LTMODE .ne. 0) then
         rfed   = mdis(3) / MOVTIM
         if (rfed .gt. MAXIPM .and. irpmod .eq. 0)
     1       MOVTIM = mdis(3) / MAXIPM
      end if
c
c......Maximum DPM
c
      rfed   = MOVDIS(4) / MOVTIM
      if (rfed .gt. MAXDPM .and. irpmod .eq. 0)
     1    MOVTIM = MOVDIS(4) / MAXDPM
c
c......Minimum tape reader time
c
      if (MOVTIM .lt. RDRLMT .and. irpmod .eq. 0) MOVTIM = RDRLMT
c
c...Accumulate machining times
c
      if (kfl .eq. 1) then
          FMVTIM = MOVTIM
          if (IACLFL .eq. 1) call acltim (AXSSTO,AXSOUT,MOVTIM,1)
          rnum = MOVTIM * TIMSCL
          call addtim (rnum)
          if (IRAP .eq. 0) then
              BRKPRM(2) = BRKPRM(2) + rnum
              BRKPRM(3) = BRKPRM(3) + mdis(2)
          endif
      endif
c
c...Set current feed rates
c
      if (IFDADJ .ne. 2) mtim = FMVTIM
      FEED(1) = mdis(2) / mtim
      if (LTMODE .eq. 1 .or. LTMODE .eq. 2) then
         if (IRAP .eq. 0) then
            call lcdout (gaxs,kaxsw,kcnt)
            FEED(1) = mdis(2) / mtim
         end if
      end if

      if (RPM .eq. 0.) then
          FEED(2) = 0.
      else
          FEED(2) = FEED(1) / RPM
      endif
      FEED(3) = FEED(1)
      FEED(4) = MOVDIS(4) / mtim
c
      OFEED(1) = mdis(1) / mtim
      OFEED(2) = mdis(2) / mtim
c
c...Set to OFEED(1) on 2-axis lathe
c
      if (MTPDYN .eq. 1 .or. LTMODE .ne. 0) then
         OFEED(3) = mdis(3) / mtim
      else
         OFEED(3) = OFEED(1)
      end if
c
c...Added 10/21/92 to eliminate roundoff
c...error of F-code with high number of digits
c
c      write (6,1) ofeed(2),ofeed(3)
c1     format (1h ,f10.4,4x,f10.4)
      rnum = dabs (OFEED(2)-OFEED(3))
      if (rnum .lt. .1) OFEED(3) = OFEED(2)
      OFEED(4) = MOVDIS(4) / mtim
      rfed   = dsqrt (mdis(3)**2 + (MOVDIS(4) *
     1               (DPMBPW(1)/DPMBPW(2)))**2)
      OFEED(5) = rfed   / mtim
      if (RPM .eq. 0.) then
          OFEED(6) = 0.
      else
          if (IFDCTP .eq. 1) then
              OFEED(6) = OFEED(2) / RPM
          else
              OFEED(6) = OFEED(3) / RPM
          endif
      endif
c
c...Save feed rate for acceleration calculations
c
      if (kfl .eq. 1) then
          ACCFED = OFEED(2)
cc          if (mdis(2) .eq. 0.) ACCFED = 0.
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  popaxs (gmch,glin,grot,gaxs,gvec,kax,kcnt)
c
c   FUNCTION:  This routine pops the output axes arrays off of a stack.
c
c   INPUT:  none.
c
c   OUTPUT: gmch    R*8  D3.4  -  Current XYZ positions.
c
c           glin    R*8  D6    -  Current linear axes position.
c
c           grot    R*8  D20.2 -  Current rotary axes position.
c
c           gaxs    R*8  D10   -  Current output axes position.
c
c           gvec    R*8  D3    -  Current tool axis vector.
c
c           kax     I*4  D10   -  Array of switches that show which axes
c                                are waiting to be output.  A value of 1
c                                signifies this axis needs to be output.
c
c           kcnt    I*4  D1    -  Number of axes waiting to be output.
c
c***********************************************************************
c
      subroutine popaxs (gmch,glin,grot,gaxs,gvec,kax,kcnt)
c
      include 'post.inc'
c
      equivalence (AXSSPT,KPOSMP(1401)), (IJKROT,KPOSMP(1739))
c
      integer*4 AXSSPT,IJKROT
c
      equivalence (IJKVC ,POSMAP(0329)), (AXSSTK,POSMAP(1601))
c
      real*8 AXSSTK(10,20),IJKVC(3)
c
      integer*4 kax(10),kcnt
c
      real*8 gmch(3,4),glin(6),grot(20,2),gaxs(10),gvec(3)
c
      integer*4 i
c
c...Pop the output axes variables
c...off of the stack
c
      if (AXSSPT .eq. 0) go to 9000
      do 100 i=1,10,1
          gaxs(i) = AXSSTK(i,AXSSPT)
  100 continue
c
c...vp 5/6/98 retrieve tool axis vector from stack
c...but zero out the rotary axis before adjusting point
c...so one correct convertion will be done
c
      if (IJKROT .eq. 1) then
         call copyn (gaxs(7),IJKVC,3)
         gaxs(7) = 0.d0
         gaxs(8) = 0.d0
         gaxs(9) = 0.d0
      end if
      call alladr (gaxs,glin,gmch,grot,gvec,5,1)
c
c...vp 5/6/98 for IJK support tool axis vector must be in TLVEC
c...to process correctly in mhchax
c
      if (IJKROT .eq. 1) call copyn (IJKVC,gvec,3)
      call whchax (gaxs,kax,kcnt)
      AXSSPT = AXSSPT - 1
c
c...End of routine
c
 8000 return
c
c...Stack underflow
c
 9000 call psterr (3,'AXSTKUND',' ',-1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  popcmd
c
c   FUNCTION:  This routine pops a post-processor command off of a
c              stack.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine popcmd
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005))
      equivalence (AXSSPT,KPOSMP(1401)), (CMDSPT,KPOSMP(1403))
c
      integer*4 AXSSPT,CMDSPT,ITYPE,ISUBT,MXCL
c
      equivalence (AXSSTK,POSMAP(1601))
c
      real*8 AXSSTK(10,20)
c
c...Push the clfile record type & sub-type
c...onto the stack
c
      if (CMDSPT .eq. 0) go to 9000
      ITYPE  = AXSSTK(1,CMDSPT)
      ISUBT  = AXSSTK(2,CMDSPT)
      MXCL   = AXSSTK(3,CMDSPT)
      AXSSPT = AXSSPT - 1
      CMDSPT = 0
c
c...End of routine
c
 8000 return
c
c...Stack underflow
c
 9000 call psterr (3,'AXSTKUND',' ',-1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pshaxs (gaxs,gvec)
c
c   FUNCTION:  This routine pushes the output axes arrays onto a stack.
c
c   INPUT:  gaxs    R*8  D10  -  Current output axes position.
c
c           gvec    R*8  D3   -  Current tool axis vector.  Stored when
c                                tool axis vector is output instead of
c                                rotary axes.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pshaxs (gaxs,gvec)
c
      include 'post.inc'
c
      equivalence (AXSSPT,KPOSMP(1401)), (IJKROT,KPOSMP(1739))
c
      integer*4 AXSSPT,IJKROT
c
      equivalence (AXSSTK,POSMAP(1601))
c
      real*8 AXSSTK(10,20)
c
      real*8 gaxs(10),gvec(3)
c
      integer*4 i
c
c...Push the output axes variables
c...onto the stack
c
      if (AXSSPT .eq. 20) go to 9000
      AXSSPT = AXSSPT + 1
      do 400 i=1,10,1
          AXSSTK(i,AXSSPT) = gaxs(i)
  400 continue
c
c...If tool axis is output instead of rotary axes
c...then save the tool axis in the rotary axes spot
c
      if (IJKROT .eq. 1) call copyn (gvec,AXSSTK(7,AXSSPT),3)
c
c...End of routine
c
 8000 return
c
c...Stack overflow
c
 9000 call psterr (3,'AXSTKOVR',' ',-1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pshcmd
c
c   FUNCTION:  This routine pushes the current post-processor command
c              onto a stack.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pshcmd
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005))
      equivalence (AXSSPT,KPOSMP(1401)), (CMDSPT,KPOSMP(1403))
c
      integer*4 AXSSPT,CMDSPT,ITYPE,ISUBT,MXCL
c
      equivalence (AXSSTK,POSMAP(1601))
c
      real*8 AXSSTK(10,20)
c
c...Push the clfile record type & sub-type
c...onto the stack
c
      if (AXSSPT .eq. 20) go to 9000
      AXSSPT = AXSSPT + 1
      CMDSPT = AXSSPT
      AXSSTK(1,CMDSPT) = ITYPE
      AXSSTK(2,CMDSPT) = ISUBT
      AXSSTK(3,CMDSPT) = MXCL
c
c...End of routine
c
 8000 return
c
c...Stack overflow
c
 9000 call psterr (3,'AXSTKOVR',' ',-1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rtfrad (grad)
c
c   FUNCTION:  This routine returns the average radius (for last point &
c              current point) for rotation around the specified rotary axis.
c              6/24/91 - radius is calculated regardless if point is
c              adjusted for pivot/table or not.
c
c   INPUT:  none
c
c   OUTPUT: grad    R*8  D4  -  Average radius at which tool control point
c                               rotates around moving rotary axis.
c
c***********************************************************************
c
      subroutine rtfrad (grad)
c
      include 'post.inc'
c
      equivalence (ITP   ,KPOSMP(1801)), (NOTOOL,KPOSMP(1802))
      equivalence (NOPIVS,KPOSMP(1282)), (IRTNUM,KPOSMP(1243))
      equivalence (IFHEAD,KPOSMP(1281)), (NOTABS,KPOSMP(1283))
      equivalence (LASTAB,KPOSMP(1260)), (IRTINC,KPOSMP(1461))
      equivalence (IRTDEF,KPOSMP(1485)), (IRDEAD,KPOSMP(1465))
      equivalence (IRTWRK,KPOSMP(1506))
      equivalence (IFDLTP,KPOSMP(3187))
c
      integer*4 IRTWRK(20),IRTNUM,ITP,NOTOOL,NOPIVS,IFHEAD,
     -          NOTABS,IRDEAD(20),LASTAB,IFDLTP,IRTDEF,IRTINC(4)
c
      equivalence (RAD   ,POSMAP(0002)), (PI    ,POSMAP(0001))
      equivalence (TL    ,POSMAP(3601)), (FEDLEN,POSMAP(3539))
      equivalence (VECSAV,POSMAP(1372)), (MCHNUM,POSMAP(1287))
      equivalence (SPIVEC,POSMAP(3583))
      equivalence (STONUM,POSMAP(1387)), (TLVEC ,POSMAP(1369))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (TABORG,POSMAP(5374))
c
      real*8 TL(120),MCHNUM(3,4),VECSAV(3),ROTANG(20,2),STONUM(3,4),
     -       PI,RAD,ROTSTO(20,2),TABORG(3,20),SPIVEC(3),FEDLEN,TLVEC(3)
c
      real*8 grad(4)
c
      integer*4 inx(6),kax,i,is1,is2,j
c
      real*8 cosa,rad1,rad2,pvr1(3),pvr2(3),andd,plen,pv(3),trad(20),
     -       tlen,prtpt(3),prtps(3),tcvct(6,20),tcvcs(6,20)
c
      data inx /3,2,3,1,1,2/
c
      do 50 i=1,IRTNUM,1
          trad(i) = 0.
   50 continue
      grad(1) = 0.d0
      grad(2) = 0.d0
      grad(3) = 0.d0
      grad(4) = 0.d0
c
c...Get feed control point on tool along the spindle vector
c...for heads or tool vector for tables.
c
      plen   = FEDLEN - TL(ITP)
      tlen   = FEDLEN
      if (IFDLTP .eq. 2) then
          plen = (FEDLEN - 1.) * TL(ITP)
          tlen = FEDLEN * TL(ITP)
      end if
      do 110 i=1,3
          if (NOTOOL .eq. 1) then
              pvr1(i) = plen * SPIVEC(i)
c             tbr1(i) = tlen * VECSAV(i)
c             tbr2(i) = tlen * TLVEC(i)
          else
              pvr1(i) = 0.d0
c             tbr1(i) = 0.d0
c             tbr2(i) = 0.d0
          end if
          pvr2(i) = pvr1(i)
  110 continue
c
      if (IFHEAD .ne. 1) go to 500
c
c...Heads
c......Walk thru all heads from rider to carrier
c
      do 290 i= IRTNUM,LASTAB+1,-1
          kax = IRTWRK(i)
          is1 = inx(kax+kax-1)
          is2 = inx(kax+kax)
          andd = dabs (ROTANG(i,2) - ROTSTO(i,2))
c
c......Add center point components
c
          do 220 j=1,3
              pvr1(j) = pvr1(j) - TABORG(j,i)
              pvr2(j) = pvr2(j) - TABORG(j,i)
  220     continue
c
c......Get median head rotation radius
c......using last point and current point rads
c
          if (andd .lt. .001) go to 280
          rad1 = dsqrt (pvr1(is1)**2 + pvr1(is2)**2)
          rad2 = dsqrt (pvr2(is1)**2 + pvr2(is2)**2)
          trad(i) = (rad1 + rad2) / 2
c
c...adjust backward pivot radius for this axis
c...to use with carier if any
c
  280     if (i .ne. LASTAB+1) then
              cosa = ROTSTO(i,1)
              call vecadj (pvr1,pvr1,cosa,kax)
              cosa = ROTANG(i,1)
              call vecadj (pvr2,pvr2,cosa,kax)
          end if
  290 continue
c
c...Get feed rate point on tool
c
  500 if (LASTAB .eq. 0) go to 8000
      call pivvec (ROTANG,pv)
      prtpt(1) = MCHNUM(1,3) + plen * pv(1)
      prtpt(2) = MCHNUM(2,3) + plen * pv(2)
      prtpt(3) = MCHNUM(3,3) + plen * pv(3)
      call pivvec (ROTSTO,pv)
      prtps(1) = STONUM(1,3) + plen * pv(1)
      prtps(2) = STONUM(2,3) + plen * pv(2)
      prtps(3) = STONUM(3,3) + plen * pv(3)
c
c...Set initial table's centers and rotation vectors
c
      do 550 i=1,LASTAB,1
          kax = IRTWRK(i)
          do 540 j=1,3
              tcvct(j,i) = TABORG(j,i)
              tcvct(3+j,i) = .0
              tcvcs(j,i) = TABORG(j,i)
              tcvcs(3+j,i) = .0
  540     continue
          tcvct(3+kax,i) = 1.0
          tcvcs(3+kax,i) = 1.0
  550 continue
c
c...Tables from rider to carrier
c
      do 890 i=LASTAB,1,-1
          kax = IRTWRK(i)
          andd = dabs (ROTANG(i,2) - ROTSTO(i,2))
          if (andd .lt. .001) go to 680
c
c......Get median table rotation radius
c......using last point and current point rads
c
          call ptlnds (prtpt,tcvct(1,i),rad1)
          call ptlnds (prtps,tcvcs(1,i),rad2)
          trad(i) = .5 * (rad1 + rad2)
c
c......Adjust all carried table's center position & axis vector for this
c......axis for current & old angle to use with next 'i'
c
  680     do 750 j=1,i-1
              cosa = 360.d0 - ROTANG(i,1)
              call axadj (tcvct(1,j),tcvct(1,j),cosa,TABORG(1,i),kax)
              call vecadj (tcvct(4,j),tcvct(4,j),cosa,kax)
              cosa = 360.d0 - ROTSTO(i,1)
              call axadj (tcvcs(1,j),tcvcs(1,j),cosa,TABORG(1,i),kax)
              call vecadj (tcvcs(4,j),tcvcs(4,j),cosa,kax)
  750     continue
  890 continue
c
c...End of routine
c...Set defined axes radii
c
 8000 do 8100 i=1,IRTDEF,1
          grad(i) = trad(IRTINC(i))
 8100 continue
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wchrad (grad,krsw)
c
c   FUNCTION:  This routine defines which the radius code to output for
c              FPM feed rate type 7.
c
c   INPUT:  grad    R*8  D4  -  Average radius at which tool control point
c                               rotates around moving rotary axis.
c
c   OUTPUT: grad    R*8  D4  -  Average radius code to output with rotary
c                               axis.
c
c***********************************************************************
c
      subroutine wchrad (grad,krsw)
c
      real*8 grad(4)
      integer*4 krsw(4)
c
      include 'post.inc'
c
      equivalence (IRTDEF,KPOSMP(1485)), (IFDRCD,KPOSMP(3213))
      equivalence (IFDRCN,KPOSMP(3211))
c
      integer*4 IRTDEF,IFDRCD(4),IFDRCN
c
      equivalence (AXSOUT,POSMAP(1340))
c
      real*8 AXSOUT(10)
c
      integer*4 iaxsw(10),i,n,ist,jindex
c
      real*8 rnum
c
c...Initialize switches
c
      krsw(1) = 0
      krsw(2) = 0
      krsw(3) = 0
      krsw(4) = 0
c
c...Check which axis rotates
c
      n      = 0
      rnum   = 0.0
      call whchax (AXSOUT,iaxsw,i)
      do 110 i=1,IRTDEF,1
          if (IFDRCD(i) .ne. 0) krsw(i) = 1
          if (iaxsw(i+6) .eq. 1) then
              n    = n + 1
              rnum = rnum + grad(i)
          else if (IFDRCN .gt. 1) then
              krsw(i) = 0
          end if
  110 continue
c
c...If single radius code is used use median radii
c
      if (IFDRCN .eq. 1) then
           ist   = jindex (krsw,1,4)
           if (ist .ne. 0) grad(ist) = rnum / n
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gttvec (gvec,gang)
c
c   FUNCTION:  This routine returns true spindle vector depending on
c              machine rotary axis configuration.
c
c   INPUT:  gang    R*8  D20 -  Rotary axis position.
c
c   OUTPUT: gvec    R*8  D3  -  Spindle vector.
c
c***********************************************************************
c
      subroutine gttvec (gvec,gang)
c
      real*8 gvec(3),gang(20)
c
      include 'post.inc'
c
      equivalence (XFMFL ,KPOSMP(0969))
      equivalence (IRTNUM,KPOSMP(1243)), (NOTABS,KPOSMP(1283))
      equivalence (LASTAB,KPOSMP(1260))
c
      integer*4 IRTNUM,NOTABS,LASTAB,XFMFL(20)
c
      equivalence (TLVEC ,POSMAP(1369)), (SPIVEC,POSMAP(3583))
c
      real*8 TLVEC(3),SPIVEC(3)
c
c...Spindle vector if no rotary axis
c
      if (IRTNUM .eq. 0) then
          gvec(1) = SPIVEC(1)
          gvec(2) = SPIVEC(2)
          gvec(3) = SPIVEC(3)
      else
c
c...Get head vector (if any rotary head exists)
c
          call pivvec (gang,gvec)
          if ((LASTAB .ne. 0 .and. NOTABS .eq. 1) .or. XFMFL(4) .gt. 1)
     1            then
c
c...Get tool vector (if tables are active)
c
              gvec(1) = TLVEC(1)
              gvec(2) = TLVEC(2)
              gvec(3) = TLVEC(3)
          endif
      endif
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rrglmt (glin,grot,gaxs)
c
c   FUNCTION:  This routine determines if a rotary axis value will exceed
c              the maximum allowed and will adjust the base angles if
c              necessary, to place the axis back towards 0 degrees.
c
c   INPUT:  glin    R*8  D6   -  Current linear axes positions.
c
c           grot    R*8  D20.2-  Current rotary axes positions.
c
c           gaxs    R*8  D10  -  Current output axes positions.
c
c   OUTPUT: grot    R*8  D20.2 -  Updated rotary axes positions on a
c                                linear scale.
c
c***********************************************************************
c
      subroutine rrglmt (glin,grot,gaxs)
c
      include 'post.inc'
c
      equivalence (RTRMCD,KPOSMP(0093)), (IRTSCL,KPOSMP(1288))
      equivalence (IRTINC,KPOSMP(1461)), (IRTDEF,KPOSMP(1485))
c
      integer*4 IRTDEF,IRTINC(4),IRTSCL(4),RTRMCD(4)
c
      equivalence (DUMMY ,POSMAP(0003)), (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425)), (ROTBAS,POSMAP(1435))
      equivalence (ROTBSV,POSMAP(2275)), (RTRMMX,POSMAP(4360))
      equivalence (RTRMVL,POSMAP(4364)), (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2),ROTBAS(4),ROTBSV(4),RTRMMX(4),RTRMVL(4),
     1       LINSTO(6),AXSSTO(10),DUMMY
c
      real*8 gaxs(10),glin(6),grot(20,2)
c
      integer*4 i,j,idir(4),idir1(4),idid,iary(4),ifl(10),ip1
c
      real*8 rdlt(4),rnum
c
c...Loop through rotary axes
c
      idid   = 0
      do 100 i=1,IRTDEF,1
          iary(i) = 0
          idir(i) = 3
          idir1(i) = 3
          ip1    = IRTINC(i)
          if (IRTSCL(i) .eq. 1) then
              if (dabs(gaxs(i+6)) .gt. RTRMMX(i)) then
                  ROTBAS(i) = 0.
                  ROTSTO(ip1,2) = ROTSTO(ip1,1)
                  if (ROTSTO(ip1,2) .gt. 180.d0) then
                      ROTSTO(ip1,2) = ROTSTO(ip1,2) - 360.
                      ROTBAS(i) = -360.
                  endif
                  iary(i) = 1
                  idid   = 1
              endif
          endif
  100 continue
c
c...Rotary axis base angle was adjusted
c...Recalculate output axes
c
      if (idid .eq. 1) then
          call rotlin (ROTSTO,ROTBAS,1,idir,rdlt)
          call axsadj (LINSTO,ROTSTO(1,2),AXSSTO)
          call copyn (ROTBAS,ROTBSV,4)
          call rotlin (grot,ROTBAS,1,idir1,rdlt)
          call axsadj (glin,grot(1,2),gaxs)
c
c......Output rotary position codes
c
          do 300 i=1,IRTDEF,1
              if (iary(i) .eq. 1) then
                  if (RTRMCD(i) .ne. 0) then
                      rnum   = RTRMVL(i)
                      if (rnum .eq. DUMMY) rnum = AXSSTO(i+6)
                      call codout (RTRMCD(i),rnum)
                      call clrbuf
                      call rotout (AXSSTO(i+6),i,2)
                  else
                      do 200 j=1,10,1
                          ifl(j) = 0
  200                 continue
                      ifl(i+6) = 1
                      call pnout (1,0.d0,AXSSTO,ifl)
                  endif
              endif
  300     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  axsacl (gdelt,gv0,gvf,gacc,gtim)
c
c   FUNCTION:  This routine calculates the time it will take to move
c              an axis a specified distance based on the change in
c              feed rate and the acceleration constant for this axis.
c
c              WARNING: This routine is not called yet, though it should
c                       work with basic acceleration calculations, look
c                       ahead logic to find the feed rate and angular
c                       change for the next move needs to be implemented,
c                       probably based on slowdown blocks.  Cycle and
c                       circular times should also call this routine.
c
c   INPUT:  gdelt   R*8  D1   -  Delta movement for this axis.
c
c           gv0     R*8  D1   -  Initial velocity (FPM).
c
c           gvf     R*8  D1   -  Programmed velocity (FPM).
c
c           gacc    R*8  D1   -  Acceleration constant for this axis (FPM).
c
c   OUTPUT: gtim    R*8  D1   -  Amount of time this move will actually
c                                take.
c
c***********************************************************************
c
      subroutine axsacl (gdelt,gv0,gvf,gacc,gtim)
c
      include 'post.inc'
c
      real*8 gdelt,gv0,gvf,gacc,gtim
c
      real*8 ta,d
c
c...Make sure there is a move
c
      gtim = 0.
      if (gacc .eq. 0. .or. gdelt .eq. 0.) go to 8000
c
c...Calculate time to get up to speed
c...using the following calculation
c...ta = (vf-v0) / a
c
      ta = (gvf-gv0) / gacc
c
c...Now calculate the distance
c...it takes to make this move
c...using the following calculation
c...d = v0*ta + .5*a*ta**2
c
      d = gv0*ta + .5*gacc*ta**2
c
c...Delta distance is greater than
c...acceleration distance
c
      if (gdelt .gt. d) then
          gtim = ta + (gdelt-d)/gvf
c
c...Acceleration distance is greater than
c...delta distance
c
      else
          gtim = ta / (d/gdelt)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cpyrot (ginp,gout)
c
c   FUNCTION:  This routine copies the rotary axis values from one
c              array to another based on the number of defined rotary
c              axes.
c
c   INPUT:  ginp    R*8  D20,2  -  Array to copy from.
c
c   OUTPUT: gout    R*8  D20,2  -  Array to copy to.
c
c***********************************************************************
c
      subroutine cpyrot (ginp,gout)
c
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (IRTNUM,KPOSMP(1243))
c
      integer*4 MACHTP,IRTNUM
c
      real*8 ginp(20,2),gout(20,2)
c
      integer*4 i,inum
c
c...Blade machines always have 4 rotary axes
c
      inum   = IRTNUM
      if (MACHTP .eq. 3) inum = 4
c
c...Copy rotary axis array
c
      do 100 i=1,inum,1
          gout(i,1) = ginp(i,1)
          gout(i,2) = ginp(i,2)
  100 continue
c
c...End of routine
c
 8000 return
      end
