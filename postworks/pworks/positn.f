c
c***********************************************************************
c
c   FILE NAME:  positn
c   CONTAINS:
c               gohome  plunge  poscal  positn  postn  retrct  retrst
c               rotabl
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        positn.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:09
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  gohome
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 GOHOME
c
c                 GOHOME/CHECK(,-AXIS(,n(,pos)),...)
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine gohome
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (REGFRC,KPOSMP(0603))
      equivalence (IRPALT,KPOSMP(3196)), (IRPTAX,KPOSMP(3197))
      equivalence (HOMCOD,KPOSMP(3361)), (HOMREG,KPOSMP(3366))
c
      integer*4 MXCL,IPSTWD(50),IRPALT,IRPTAX,HOMCOD(5),HOMREG(10),
     1          REGFRC(MAXFMT)
c
      equivalence (METCNV,POSMAP(0004)), (HOME  ,POSMAP(0151))
      equivalence (HOMCDV,POSMAP(0167)), (PSTWD ,POSMAP(0441))
      equivalence (AXSOUT,POSMAP(1340))
c
      real*8 METCNV,PSTWD(50),AXSOUT(10),HOME(10),HOMCDV(5)
c
      integer*4 ist,inc,i,nprm,isub,ifl(10),imod,idid
c
      real*8 rax(10)
c
c...GOHOME
c
      if (MXCL .eq. 0) then
          call axsxfm (HOME,AXSOUT)
c
c......Move to home position
c
          call rapset (IRPALT,IRPTAX)
          call movpos
          call raprst
          call rotbeg
          go to 8000
      endif
c
c...GOHOME/mode
c
      if (MXCL .lt. 1) go to 9400
      ist    = 1
      inc    = 1
c
c......GOHOME/CHECK
c
      if (IPSTWD(1) .eq. 1023) then
          imod   = 1
c
c......GOHOME/AUTO
c
      else if (IPSTWD(1) .eq. 88) then
          imod   = 2
c
c......GOHOME/FROM
c
      else if (IPSTWD(1) .eq. 4019) then
          imod   = 3
c
c......GOHOME/NEXT
c
      else if (IPSTWD(1) .eq. 162) then
          imod   = 4
c
c......Invalid minor word
c
      else
          if (IPSTWD(1) .eq. 0) go to 9000
          go to 9300
      endif
c
c......Clear storage
c
      do 50 i=1,10,1
          ifl(i) = 0
          rax(i) = 0.
   50 continue
      idid   = 0
      if (MXCL .eq. 1) go to 500
      ist    = 2
c
c......Get next parameter section
c
  100 inc    = ist
      if (IPSTWD(ist) .eq. 0) go to 9000
      inc    = inc    + 1
c
c......Find end of parameter section
c
      do 200 i=inc,MXCL,1
          if (IPSTWD(i) .ne. 0) go to 250
  200 continue
      i      = MXCL   + 1
  250 nprm   = i      - inc
c
c......Invalid number of parameters
c
      if (nprm .gt. 2) then
          inc    = inc    + 2
          go to 9000
      endif
c
c......Check parameter values
c
      isub   = 1
      if (nprm .eq. 2) then
          isub   = PSTWD(inc)
          inc    = inc    + 1
      endif
c
c...Set Position
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
      ifl(ist) = 1
      if (nprm .eq. 0) then
          inc    = inc    - 1
      else
          rax(ist) = PSTWD(inc) * METCNV
      endif
      ist    = inc    + 1
      if (ist .le. MXCL) go to 100
      idid   = 1
c
c.....Output GOHOME block
c
  500 call codout (HOMCOD(imod),HOMCDV(imod))
      do 600 i=1,10,1
          if (ifl(i) .ne. 0) then
              if (HOMREG(i) .ne. 0) REGFRC(HOMREG(i)) = 1
              call codout (HOMREG(i),rax(i))
          endif
  600 continue
      if (idid .eq. 1) call clrbuf
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
c   SUBROUTINE:  plunge
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 PLUNGE[/feed]
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine plunge
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICYCSW,KPOSMP(0271)), (IFITYP,KPOSMP(3150))
c
      integer*4 MXCL,IPSTWD(50),ICYCSW(5),IFITYP
c
      equivalence (DUMMY ,POSMAP(0003)), (RETPOS,POSMAP(0177))
      equivalence (PSTWD ,POSMAP(0441)), (AXSOUT,POSMAP(1340))
      equivalence (PFEED ,POSMAP(3540))
c
      real*8 AXSOUT(10),PFEED(4),PSTWD(50),RETPOS(10),DUMMY
c
      integer*4 i,inc,isav
c
      real*8 rsav,rfed
c
c...PLUNGE
c
      inc    = 0
      if (MXCL .eq. 0 .or. (MXCL .ge. 1 .and. IPSTWD(1) .eq. 0)) then
          if (RETPOS(1) .eq. DUMMY) go to 9000
          if (ICYCSW(1) .ne. 0) go to 9700
c
c......PLUNGE/feed
c
          rfed   = 0.
          if (MXCL .ne. 0) then
              inc    = 1
              if (PSTWD(inc) .lt. 0.) go to 9300
              rfed   = PSTWD(inc)
          endif
c
c......Restore previous point
c
          do 100 i=1,10,1
              AXSOUT(i) = RETPOS(i)
  100     continue
c
c......Set feed rate
c
          if (rfed .eq. 0.) then
              call rapset (1,2)
          else
              isav   = IFITYP
              rsav   = PFEED(IFITYP)
              IFITYP = 1
              PFEED(1) = rfed
          endif
c
c......Plunge tool
c
          call movpos
c
c......Reset feed rate
c
          if (rfed .eq. 0.) then
              call raprst
          else
              IFITYP = isav
              PFEED(1) = rsav
          endif
          if (MXCL .gt. 1) go to 9200
          go to 8000
c
c...Invalid command syntax
c
      else
          inc    = 1
          go to 9100
      endif
c
c...End of routine
c
 8000 return
c
c...A RETRCT/GOHOME has not yet been used
c
 9000 call psterr (2,'NORETRCT',' ',0)
      go to 8000
c
c...Number expected
c
 9100 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9200 call psterr (1,'INVPSYNW',' ',MXCL+1)
      go to 8000
c
c...Input value out of range
c
 9300 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...CYCLE currently active
c
 9700 call psterr (2,'CYCACT',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  poscal
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 POSCAL/x,y,z [,i,j,k]
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine poscal
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (VECSAV,POSMAP(1372))
c
      real*8 METCNV,PSTWD(50),VECSAV(3)
c
      integer*4 i
c
      real*8 mnum(6)
c
c...Initialize routine
c
      if (MXCL .ne. 3 .and. MXCL .ne. 6) go to 9100
c
c...Get position values
c
      do 50 i=1,MXCL,1
          if (IPSTWD(i) .ne. 0) go to 9000
          mnum(i) = PSTWD(i)
          if (i .le. 3) mnum(i) = mnum(i) * METCNV
   50 continue
c
c...Use previous tool axis
c
      if (MXCL .ne. 6) then
          mnum(4) = VECSAV(1)
          mnum(5) = VECSAV(2)
          mnum(6) = VECSAV(3)
      endif
c
c...Set current position
c
      call setend (mnum)
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
c...Invalid command syntax
c
 9100 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  positn
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 POSITN/-AXIS,(n),pos...
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine positn
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (AXSOUT,POSMAP(1340)), (AXSSTO,POSMAP(1425))
c
      real*8 METCNV,PSTWD(50),AXSOUT(10),AXSSTO(10)
c
      integer*4 ist,inc,i,nprm,isub
c
c...Initialize routine
c
      if (MXCL .lt. 2) go to 9400
      ist    = 1
      do 50 i=1,10,1
          AXSOUT(i) = AXSSTO(i)
   50 continue
c
c...Get next parameter section
c
  100 inc    = ist
      if (IPSTWD(ist) .eq. 0) go to 9000
      inc    = inc    + 1
c
c......Find end of parameter section
c
      do 200 i=inc,MXCL,1
          if (IPSTWD(i) .ne. 0) go to 250
  200 continue
      i      = MXCL   + 1
  250 nprm   = i      - inc
c
c......Invalid number of parameters
c
      if (nprm .lt. 1) then
          inc    = i
          go to 9100
      else if (nprm .gt. 2) then
          inc    = inc    + 2
          go to 9000
      endif
c
c......Check parameter values
c
      isub   = 1
      if (nprm .eq. 2) then
          isub   = PSTWD(inc)
          inc    = inc    + 1
      endif
c
c...Set Position
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
      if (ist .lt. 7) then
         AXSOUT(ist) = PSTWD(inc) * METCNV
      else
         AXSOUT(ist) = PSTWD(inc)
      end if
      ist    = inc    + 1
      if (ist .le. MXCL) go to 100
c
c...Output new position now
c
      call rapset (1,2)
      call movpos
      call raprst
      call rotbeg
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
c   SUBROUTINE:  postn
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 POSTN/n
c
c                 POSTN/OFF
c
c                 POSTN/XYPLAN
c
c                 POSTN/(NORMAL)(,n)(,-AXIS(,n(,pos)),...)
c                        INCR
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine postn
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ISEQSW,KPOSMP(0845)), (PSTNCD,KPOSMP(3376))
      equivalence (PSTNRG,KPOSMP(3381)), (PSTNOF,KPOSMP(3396))
      equivalence (IPSTNF,KPOSMP(3397))
c
      integer*4 MXCL,IPSTWD(50),ISEQSW,PSTNCD(5),IPSTNF,PSTNRG(15),
     1          PSTNOF
c
      equivalence (METCNV,POSMAP(0004)), (PSTNCV,POSMAP(0172))
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 METCNV,PSTWD(50),PSTNCV(5)
c
      integer*4 ist,inc,i,nprm,isub,ifl(10),imod,idid,isav
c
      real*8 rax(10),rreg
c
c...POSTN/n
c
      if (MXCL .eq. 0) go to 9400
      inc    = 1
      if (MXCL .eq. 1 .and. IPSTWD(1) .eq. 0) then
          call clrbuf
          isav   = ISEQSW
          ISEQSW = 2
          call codout (PSTNRG(11),PSTWD(1))
          call clrbuf
          ISEQSW = isav
c
c...POSTN/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          call codout (PSTNCD(3),PSTNCV(3))
          if (PSTNOF .eq. 1) call clrbuf
          if (MXCL .gt. 1) go to 9500
c
c...POSTN/XYPLAN
c
      else if (IPSTWD(1) .eq. 33) then
          IPSTNF = 1
          if (MXCL .gt. 1) go to 9500
c
c...POSTN/-AXIS,...
c
      else
          if (MXCL .lt. 1) go to 9400
          ist    = 1
          inc    = 1
          imod   = 1
          rreg   = 0.
          idid   = 0
c
c......POSTN/NORMAL
c
          if (IPSTWD(inc) .eq. 111) then
              imod   = 1
              inc    = inc    + 1
c
c......POSTN/INCR
c
          else if (IPSTWD(1) .eq. 66) then
              imod   = 2
              inc    = inc    + 1
          endif
c
c......POSTN/,n
c
          if (inc .gt. MXCL) go to 500
          if (IPSTWD(inc) .eq. 0) then
              rreg   = PSTWD(inc)
              inc    = inc    + 1
          endif
c
c......Clear storage
c
          do 50 i=1,10,1
              ifl(i) = 0
              rax(i) = 0.
   50     continue
          if (MXCL .eq. 1) go to 500
          ist    = inc
c
c......Get next parameter section
c
  100     inc    = ist
          if (IPSTWD(ist) .eq. 0) go to 9000
          inc    = inc    + 1
c
c......Find end of parameter section
c
          do 200 i=inc,MXCL,1
              if (IPSTWD(i) .ne. 0) go to 250
  200     continue
          i      = MXCL   + 1
  250     nprm   = i      - inc
c
c......Invalid number of parameters
c
          if (nprm .gt. 2) then
              inc    = inc    + 2
              go to 9000
          endif
c
c......Check parameter values
c
          isub   = 1
          if (nprm .eq. 2) then
              isub   = PSTWD(inc)
              inc    = inc    + 1
          endif
c
c...Set Position
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
          ifl(ist) = 1
          if (nprm .eq. 0) then
              inc    = inc    - 1
          else
              rax(ist) = PSTWD(inc) * METCNV
          endif
          ist    = inc    + 1
          if (ist .le. MXCL) go to 100
          idid   = 1
c
c.....Output POSTN block
c
  500     call pnout (imod,rreg,rax,ifl)
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
c
c...Too many parameters
c
 9500 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  retrct (kfl)
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 RETRCT[/][TO  ], ...
c                           PAST
c
c                 RETRCT/ON
c                        OFF
c
c                 RETRCT/TOOL, ...
c
c   INPUT:  kfl     I*4  D1  -  1 = Output this retract move now.  2 =
c                               Store the retract move in the AXSOUT
c                               array, the calling routine will output
c                               the move.  When set to 2, a standard
c                               RETRCT command with no parameters is
c                               assumed.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine retrct (kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (ICYCSW,KPOSMP(0271)), (MACHTP,KPOSMP(1201))
      equivalence (LRTRCT,KPOSMP(1278)), (LRTTAD,KPOSMP(1335))
      equivalence (IRETMD,KPOSMP(1340)), (IRETYP,KPOSMP(1342))
      equivalence (IRETFL,KPOSMP(1345)), (IRTSHF,KPOSMP(1374))
      equivalence (IFITYP,KPOSMP(3150)), (MTPDYN,KPOSMP(4126))
c
      integer*4 MXCL,IPSTWD(50),ICYCSW(5),ICYCFL(25),CYCCOD(20),
     1          IFITYP,LRTRCT,LRTTAD,MACHTP,IRETMD(2),IRETYP,IRETFL,
     2          MTPDYN,IRTSHF
c
      equivalence (RETPOS,POSMAP(0177)), (PSTWD ,POSMAP(0441))
      equivalence (AXSOUT,POSMAP(1340)), (AXSSTO,POSMAP(1425))
      equivalence (RETFED,POSMAP(2254)), (RETPL ,POSMAP(2258))
      equivalence (RETDIS,POSMAP(2279))
      equivalence (CYCCDV,POSMAP(2901)), (PFEED ,POSMAP(3540))
      equivalence (BLDELT,POSMAP(3593)), (RETFSV,POSMAP(3600))
      equivalence (RTSHFD,POSMAP(4947))
c
      real*8 CYCCDV(25),AXSOUT(10),AXSSTO(10),PFEED(4),PSTWD(50),
     1       RETPOS(10),RETDIS,RETFED(4),BLDELT,RETFSV,RTSHFD,RETPL(4)
c
      integer*4 kfl
c
      integer*4 inc,ierr,i,ifl(5),irt,iln,imod(2),mxc,is
c
      real*8 rfed,rval(4),nmag
c
c...Issue standard retract command
c...with no parameters
c
      mxc    = MXCL
      if (kfl .eq. 2) mxc    = 0
c
c...RETRCT
c
      inc    = 0
      if (mxc .eq. 0 .or. (mxc .ge. 1 .and. (IPSTWD(1) .eq. 0 .or.
     1    IPSTWD(1) .eq. 70 .or. IPSTWD(1) .eq. 69 .or.
     2    IPSTWD(1) .eq. 84 .or. IPSTWD(1) .eq. 85 .or.
     3    IPSTWD(1) .eq. 86))) then
          if (ICYCSW(1) .ne. 0) go to 9700
c
c......RETRCT/ON
c......       PAST
c
          inc    = 1
          imod(1) = IRETMD(1)
          if (mxc .ge. inc .and. (IPSTWD(inc) .eq. 70 .or.
     1        IPSTWD(inc) .eq. 69)) then
              imod(1) = 1
              if (IPSTWD(inc) .eq. 70) imod(1) = 2
              inc    = inc    + 1
          endif
c
c......RETRCT/-XAXIS
c
          imod(2) = IRETMD(2)
          if (mxc .ge. inc .and. (IPSTWD(inc) .eq. 84 .or.
     1        IPSTWD(inc) .eq. 85 .or. IPSTWD(inc) .eq. 86 .or.
     2        IPSTWD(inc) .eq. 617)) then
              if (IPSTWD(inc) .eq. 617) then
                  imod(2) = 4
              else
                  imod(2) = IPSTWD(inc) - 83
              endif
              inc    = inc    + 1
          endif
c
c......RETRCT/feed
c
          rfed   = 0.
          if (mxc .ge. inc) then
              if (IPSTWD(inc) .ne. 0) go to 9000
              if (PSTWD(inc) .lt. 0.) go to 9300
              rfed   = PSTWD(inc)
              inc    = inc    + 1
          endif
c
c......Save current position
c
          do 100 i=1,10,1
              RETPOS(i) = AXSSTO(i)
  100     continue
c
c......Calculate retract point
c
          call retpln (imod,AXSOUT,ierr)
          if (ierr .eq. 1) go to 9600
c
c......Set feed rate
c
          if (rfed .eq. 0.) then
              RETFSV = 0.
              call rapset (1,2)
          else
              IRETYP = IFITYP
              RETFSV = PFEED(IFITYP)
              IFITYP = 1
              PFEED(1) = rfed
          endif
c
c......Retract tool
c
          IRETFL = 1
          if (kfl .ne. 2) then
              call movpos
c
c......Reset feed rate
c
              call retrst
              if (mxc .ge. inc) go to 9200
          endif
          go to 8000
      endif
c
c...RETRCT/ON
c
      inc    = 1
      ifl(1) = 0
      ifl(2) = 0
      ifl(3) = 0
      ifl(4) = 0
      ifl(5) = 0
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 71) then
          ICYCSW(3) = 1
          if (ICYCFL(1) .eq. 1) call codout (CYCCOD(19),CYCCDV(19))
c
c...RETRCT/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          ICYCSW(3) = 2
          if (ICYCFL(1) .eq. 1) call codout (CYCCOD(20),CYCCDV(20))
c
c...RETRCT/TOOL
c
      else if (IPSTWD(1) .eq. 617) then
          go to 200
c
c...Unrecognized minor word
c
      else
          if (IPSTWD(inc) .eq. 0) go to 9500
          go to 9100
      endif
      if (MXCL .gt. 1) go to 9200
      go to 8000
c
c...RETRCT/TOOL,ON|OFF
c
  200 if (IPSTWD(inc) .eq. 0) go to 9500
      if (IPSTWD(inc) .eq. 617) then
         if (ifl(1) .eq. 1) go to 9600
         ifl(1) = 1
         if (inc .lt. MXCL) then
             inc    = inc + 1
             if (IPSTWD(inc) .eq. 0) go to 9500
             irt    = IPSTWD(inc)
             if (irt .eq. 72 .or. irt .eq. 71) then
                 LRTRCT = 72 - irt
                 inc    = inc    + 1
             end if
         else
             go to 9500
         end if
c
c...RETRCT/TOOL,...LENGTH,...
c
      else if (IPSTWD(inc) .eq. 9) then
         if (ifl(2) .eq. 1) go to 9600
         if (LRTRCT .eq. 0 .and. MTPDYN .ne. 3) go to 9800
         ifl(2) = 1
         if (inc .lt. MXCL) then
             inc    = inc + 1
             if (IPSTWD(inc) .ne. 0) then
                 iln    = IPSTWD(inc)
                 if (iln .eq. 72 .or. iln .eq. 71) then
                     LRTTAD = iln - 70
                     inc    = inc    + 1
                 else
                     go to 9100
                 end if
             end if
             if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                 RETDIS = PSTWD(inc)
                 inc    = inc + 1
             end if
         end if
         LRTRCT = 1
c
c...RETRCT/TOOL,...CLRSRF,...
c
      else if (IPSTWD(inc) .eq. 1057) then
         if (ifl(2) .eq. 1) go to 9600
         if (LRTRCT .eq. 0 .and. MTPDYN .ne. 3) go to 9800
         ifl(2) = 1
         is     = 0
         if (inc+4 .gt. MXCL) go to 9900
         do 500 inc=inc+1,inc+4,1
             if (inc .gt. MXCL) go to 9900
             if (IPSTWD(inc) .ne. 0) go to 9900
             is     = is     + 1
             rval(is) = PSTWD(inc)
  500    continue
         if (nmag(rval) .lt. .001) go to 9300
         call unitvc (rval,RETPL)
         RETPL(4) = rval(4)
         LRTRCT = 2
c
c...RETRCT/TOOL,...FEEDZ,ret[,plg[,ofs]]
c
      else if (IPSTWD(inc) .eq. 219) then
         if (ifl(3) .eq. 1) go to 9600
         if (LRTRCT .eq. 0 .and. MTPDYN .ne. 3) go to 9800
         ifl(3) = 1
         inc    = inc + 1
         if (IPSTWD(inc) .ne. 0 .or. inc .gt. MXCL) go to 9000
         if (PSTWD(inc) .lt. 0.) go to 9300
         RETFED(1) = PSTWD(inc)
         inc    = inc + 1
         if (IPSTWD(inc) .eq. 0 .and. inc .le. MXCL) then
             if (PSTWD(inc) .lt. 0.) go to 9300
             RETFED(2) = PSTWD(inc)
             inc    = inc + 1
             if (IPSTWD(inc) .eq. 0 .and. inc .le. MXCL) then
                 if (PSTWD(inc) .lt. 0.) go to 9300
                 RETFED(4) = PSTWD(inc)
                 inc    = inc + 1
             endif
         endif
c
c...RETRCT/TOOL,...ATANGL,ang
c
      else if (IPSTWD(inc) .eq. 1) then
         if (MTPDYN .ne. 3) go to 9800
         if (ifl(4) .eq. 1) go to 9600
         ifl(4) = 1
         inc    = inc + 1
         if (IPSTWD(inc) .ne. 0 .or. inc .gt. MXCL) go to 9000
         if (PSTWD(inc) .lt. 0. .or. PSTWD(inc) .gt. 360.) go to 9300
         BLDELT = PSTWD(inc)
c
c...RETRCT/TOOL,...OFFSET,(dir,dis)
c
      else if (IPSTWD(inc) .eq. 705) then
         if (ifl(5) .eq. 1) go to 9600
         if (LRTRCT .eq. 0 .and. MTPDYN .ne. 3) go to 9800
         ifl(5) = 1
         inc    = inc + 1
         if (IPSTWD(inc) .eq. 0 .or. inc .gt. MXCL) go to 9500
         if (IPSTWD(inc) .eq. 112) then
             IRTSHF = 1
         else if (IPSTWD(inc) .eq. 8) then
             IRTSHF = 2
         else if (IPSTWD(inc) .eq. 24) then
             IRTSHF = 3
         else
             go to 9100
         endif
         inc    = inc    + 1
         if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
             RTSHFD = PSTWD(inc)
             inc    = inc + 1
         endif
      else
          go to 9100
      endif
      if (inc .lt. MXCL) go to 200
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
c...Input value out of range
c
 9300 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
c...Minor word expected
c
 9500 call psterr (2,'MINOREXP',' ',inc)
      go to 8000
c
c...Tool axis does not intersect plane
c
 9600 call psterr (2,'VECPLN',' ',inc)
      go to 8000
c
c...CYCLE currently active
c
 9700 call psterr (2,'CYCACT',' ',inc)
      go to 8000
c
c...Not valid for this machine type
c
 9800 call psterr (2,'NOTAPPLY',' ',inc)
      go to 8000
c
c...Not valid for this machine type
c
 9900 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  retrst
c
c   FUNCTION:  This routine resets all parameters internally set by the
c              RETRCT command.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine retrst
c
      include 'post.inc'
c
      equivalence (IRETYP,KPOSMP(1342)), (IRETFL,KPOSMP(1345))
      equivalence (IFITYP,KPOSMP(3150))
c
      integer*4 IRETYP,IFITYP,IRETFL
c
      equivalence (PFEED ,POSMAP(3540)), (RETFSV,POSMAP(3600))
c
      real*8 RETFSV,PFEED(4)
c
c......Reset feed rate
c
      if (RETFSV .eq. 0.) then
          call raprst
      else
          IFITYP = IRETYP
          PFEED(1) = RETFSV
      endif
      IRETFL = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotabl
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 ROTABL/(AXIS,m),ATANGL,n(,CLW   )(,SAME  )(,NOW )
c                                           CCLW     ROTREF   NEXT
c                                           ORIENT
c                                           INCR
c
c                 ROTABL/NEXT(,AXIS,n),CLW
c                                      CCLW
c
c                 ROTABL/ON (,AXIS,n),m1, [,THRU] ,mn
c                        OFF
c
c                 ROTABL/ORIGIN(,AXIS,n),x,y,z
c
c                 ROTABL/SIZE(,AXIS,n),c
c
c                 ROTABL/ROTATE(,AXIS,n),XAXIS
c                                        YAXIS
c                                        ZAXIS
c
c                 ROTABL/SHORT,COMBIN
c                              LARGE
c                              PLUS
c                              AXIS,n
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rotabl
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (NOPIVS,KPOSMP(1282)), (NOTABS,KPOSMP(1283))
      equivalence (IRTMOD,KPOSMP(1284)), (RSIZCD,KPOSMP(1328))
      equivalence (RSIZFL,KPOSMP(1333)), (IRTOUT,KPOSMP(0813))
      equivalence (IRTRTE,KPOSMP(1343)), (IACNXT,KPOSMP(1647))
      equivalence (IRTNXT,KPOSMP(1361)), (IRTNXF,KPOSMP(1365))
      equivalence (IRSRTE,KPOSMP(1375))
      equivalence (IRTDUP,KPOSMP(1413)), (IRTDAC,KPOSMP(1417))
      equivalence (ISCWRK,KPOSMP(1453))
      equivalence (IRTINC,KPOSMP(1461)), (IRDEAD,KPOSMP(1465))
      equivalence (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506)), (IRTFRC,KPOSMP(1777))
      equivalence (IFITYP,KPOSMP(3150)), (IRAP  ,KPOSMP(3199))
c
      integer*4 MXCL,IPSTWD(50),IRTNUM,IRTMOD(4),IRTNXT(4),IRTNXF,
     1          IRDEAD(20),IFITYP,RSIZCD(5),RSIZFL,NOTABS,IRTOUT(4),
     2          IRTRTE(2),NOPIVS,IACNXT,IRTDUP(4),IRTDAC(9,4),IRAP,
     3          IRTFRC,IRSRTE(3),IRTINC(4),IRTWRK(4),IRTYPE(20),
     4          ISCWRK(2,4)
c
      equivalence (PI    ,POSMAP(0001))
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (VECNXT,POSMAP(1248))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (AXSSTO,POSMAP(1425)), (ROTBAS,POSMAP(1435))
      equivalence (ROTCRM,POSMAP(1439)), (RSIZCV,POSMAP(1600))
      equivalence (SCROT ,POSMAP(2119)), (LNRTOL,POSMAP(2251))
      equivalence (RPM   ,POSMAP(3307)), (PFEED ,POSMAP(3540))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (ROTNXT,POSMAP(5293)), (TABORG,POSMAP(5374))
c
      real*8 METCNV,PSTWD(50),MCHNUM(3,4),LINAXS(6),AXSOUT(10),
     1       TLVEC(3),TABORG(3,20),STONUM(3,4),ROTANG(20,2),
     2       ROTSTO(20,2),AXSSTO(10),ROTBAS(4),RPM,PFEED(4),VECSAV(3),
     3       PI,RSIZCV,ROTCRM(4),LNRTOL(3),VECNXT(3),ROTNXT(20,2),
     4       SCROT(2,4)
c
      integer*4 inc,iax,idir,iref,inxt,i,ifl(7),iary(10),icnt,ifsv,ion,
     1          inx,ipt,ival,iend,j,ithru,ient,ilin,imrsv,iaxfl,ierr,
     2          ip1,irt(4),iry(4),iwrk(2,4),ist,ien
c
      real*8 ang,rary(10),ipm,fsv,rrot(2,4),rlin(6),tvec(3)
c
      inc    = 0
      if (IRTNUM .eq. 0 .and. IRTOUT(4) .eq. 0) go to 9200
c
c...ROTABL/..,ATANGL,..
c
      if (MXCL .eq. 0) go to 9000
      inc    = 1
      if (IPSTWD(1) .eq. 132 .or. IPSTWD(1) .eq. 1) then
          if (MXCL .lt. 2) go to 9000
          inc    = 1
          iax    = 1
          ip1    = IRTINC(iax)
          idir   = 3
          ipm    = PFEED(1)
          if (IFITYP .eq. 2) then
              ipm    = PFEED(2) * RPM
          endif
          iref   = 0
          inxt   = 1
          ilin   = 0
          do 100 i=1,7,1
              ifl(i) = 0
  100     continue
  200     if (IPSTWD(inc) .eq. 0) go to 9500
c
c......ROTABL/,AXIS,m
c
          if (IPSTWD(inc) .eq. 132) then
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              iax   = PSTWD(inc)
              if (iax .le. 0 .or. iax .gt. 4) go to 9200
              if (IRTOUT(iax) .ne. 1) go to 9200
              inc    = inc    + 1
              ip1    = IRTINC(iax)
c
c......ROTABL/,ATANGL,n
c
          else if (IPSTWD(inc) .eq. 1) then
              if (ifl(2) .eq. 1) go to 9600
              ifl(2) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              ang    = PSTWD(inc)
              inc    = inc    + 1
c
c......ROTABL/,NEUTRL
c
          else if (IPSTWD(inc) .eq. 166) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              idir   = 3
              inc    = inc    + 1
c
c......ROTABL/,CLW
c
          else if (IPSTWD(inc) .eq. 60) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              idir   = 1
              inc    = inc    + 1
c
c......ROTABL/,CCLW
c
          else if (IPSTWD(inc) .eq. 59) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              idir   = 2
              inc    = inc    + 1
c
c......ROTABL/,ORIENT
c
          else if (IPSTWD(inc) .eq. 246) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              idir   = 4
              inc    = inc    + 1
c
c......ROTABL/,IPM
c......        MMPM
c
          else if (IPSTWD(inc) .eq. 73 .or. IPSTWD(inc) .eq. 315) then
              if (ifl(4) .eq. 1) go to 9600
              ifl(4) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .lt. 0.) go to 9400
              ipm    = PSTWD(inc)
              inc    = inc    + 1
c
c......ROTABL/,RPM
c
c          else if (IPSTWD(inc) .eq. 78) then
c              if (ifl(4) .eq. 1) go to 9600
c              ifl(4) = 1
c              inc    = inc    + 1
c              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
c              if (PSTWD(inc) .lt. 0.) go to 9400
c              iftp   = 2
c              ipm    = PSTWD(inc)
c              inc    = inc    + 1
c
c......ROTABL/,INCR
c
          else if (IPSTWD(inc) .eq. 66) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              idir   = 5
              inc    = inc    + 1
c
c......ROTABL/,SAME
c
          else if (IPSTWD(inc) .eq. 54) then
              if (ifl(5) .eq. 1) go to 9600
              ifl(5) = 1
              iref   = 0
              inc    = inc    + 1
c
c......ROTABL/,ROTREF
c
          else if (IPSTWD(inc) .eq. 68) then
              if (ifl(5) .eq. 1) go to 9600
              ifl(5) = 1
              iref   = 1
              inc    = inc    + 1
c
c......ROTABL/,NOW
c
          else if (IPSTWD(inc) .eq. 161) then
              if (ifl(6) .eq. 1) go to 9600
              ifl(6) = 1
              inxt   = 1
              inc    = inc    + 1
c
c......ROTABL/,NEXT
c
          else if (IPSTWD(inc) .eq. 162) then
              if (ifl(6) .eq. 1) go to 9600
              ifl(6) = 1
              if (IRTMOD(iax) .eq. 1) inxt   = 0
              IACNXT = 1
              inc    = inc    + 1
c
c......ROTABL/,POSITN
c
          else if (IPSTWD(inc) .eq. 1072) then
              if (ifl(7) .eq. 1) go to 9600
              ifl(7) = 1
              ilin   = 0
              inc    = inc    + 1
c
c......ROTABL/,CUT
c
          else if (IPSTWD(inc) .eq. 803) then
              if (ifl(7) .eq. 1) go to 9600
              ifl(7) = 1
              ilin   = 1
              if (LNRTOL(1) .eq. 0.) ilin = 0
              inc    = inc    + 1
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          if (inc .le. MXCL) go to 200
c
c......Make sure ATANGL was specified
c
          if (ifl(2) .ne. 1) go to 9500
c
c......Set up linear scale axis
c
          call cpyrot (ROTSTO,ROTANG)
c
c.........NEUTRL,CLW,CCLW
c
          if (idir .le. 3) then
              ROTANG(ip1,1) = dmod(ang,360.0d0)
              iary(1) = 3
              iary(2) = 3
              iary(3) = 3
              iary(4) = 3
              iary(iax) = idir
              call rotlin (ROTANG,ROTBAS,0,iary,rary)
c
c.........ORIENT
c
          else if (idir .eq. 4) then
              ROTANG(ip1,2) = ang
c
c..........INCR
c
          else
              ROTANG(ip1,2) = ROTANG(ip1,2) + ang
          endif
c
c......Calculate rotary scale axis
c
          inx = inxt
          if (idir .eq. 4) inx = 1
          call linrot (ROTANG,ROTBAS,inx)
c
c......Calculate tool axis vector
c
          call getijk (ROTANG(1,1),TLVEC)
          if (inxt .eq. 0) then
              if (ifl(7) .eq. 1) then
                  call copyn (VECSAV,VECNXT,3)
                  call cpyrot (ROTSTO,ROTNXT)
                  IRTFRC = 1
              endif
              call copyn (TLVEC,VECSAV,3)
              call cpyrot (ROTANG,ROTSTO)
              go to 8000
          endif
c
c......Calculate machine location
c.........SAME
c
          if (iref .eq. 0) then
              do 600 i=1,10,1
                  AXSOUT(i) = AXSSTO(i)
  600         continue
              call axsadj (LINAXS,ROTANG(1,2),rary)
              AXSOUT(iax+6) = rary(iax+6)
              call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c.........ROTREF
c
          else
              do 700 i=1,4,1
                  MCHNUM(1,i) = STONUM(1,i)
                  MCHNUM(2,i) = STONUM(2,i)
                  MCHNUM(3,i) = STONUM(3,i)
  700         continue
              call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,1,5)
          endif
c
c......Which axis will be output ?
c
ctest          call whchax (AXSOUT,iary,icnt)
ctest          if (icnt .eq. 0) go to 8000
c
c......Set up feed rate
c
          if (ipm .eq. 0.) then
              call rapset (1,2)
          else
              ifsv   = IFITYP
              fsv    = PFEED(1)
c
              IFITYP = 1
              PFEED(1) = ipm
          endif
c
c......Output motion block
c
          imrsv = IRAP
          iaxfl = 0
  800     IRAP = imrsv
          if (ilin .eq. 1) then
              ierr = 0
              call mchlin (iaxfl,ierr,ilin,ient)
          endif
          call whchax (AXSOUT,iary,icnt)
          call motion (iary,icnt)
          call clrmot (0)
          if (iaxfl .ne. 0) go to 800
c
c......Reset feed rate
c
          if (ipm .eq. 0.) then
              call raprst
          else
              IFITYP = ifsv
              PFEED(1) = fsv
          endif
c
c......Reinitialize rotaries if dead axis moves
c
          if (IRDEAD(ip1) .eq. 1) call rotbeg
c
c...ROTABL/ADJUST
c
      else if (IPSTWD(1) .eq. 159) then
          if (MXCL .lt. 3) go to 9000
          inc    = 3
          if (IPSTWD(inc) .eq. 71) then
              ion    = 1
          else if (IPSTWD(inc) .eq. 72) then
              ion    = 2
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
          inc    = 2
          if (IPSTWD(inc) .eq. 1002) then
              NOPIVS = ion
          else if (IPSTWD(inc) .eq. 177) then
              NOTABS = ion
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
          inc    = 3
          if (inc .ne. MXCL) go to 9700
c
c...ROTABL/NEXT
c
      else if (IPSTWD(1) .eq. 162) then
          if (MXCL .lt. 2) go to 9000
          inc    = 2
          iax    = 1
c
c......ROTABL/NEXT,AXIS,n
c
          if (IPSTWD(inc) .eq. 132) then
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              iax   = PSTWD(inc)
              if (iax .le. 0 .or. iax .gt. 4) go to 9200
              if (IRTOUT(iax) .ne. 1) go to 9200
              inc    = inc    + 1
          endif
c
c......ROTABL/NEXT,CLW
c
          if (inc .gt. MXCL) go to 9500
          if (IPSTWD(inc) .eq. 60) then
              IRTNXT(iax) = 1
          else if (IPSTWD(inc) .eq. 59) then
              IRTNXT(iax) = 2
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
          IRTNXF = 1
          if (inc .ne. MXCL) go to 9700
c
c...ROTABL/ON
c...       OFF
c
      else if (IPSTWD(1) .eq. 71 .or. IPSTWD(1) .eq. 72) then
          if (MXCL .lt. 2) go to 9000
          ival   = 1
          if (IPSTWD(1) .eq. 72) ival = 0
          inc    = 2
          iax    = 1
c
c......ROTABL/ON,AXIS,n
c
          if (IPSTWD(inc) .eq. 132) then
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              iax   = PSTWD(inc)
              if (iax .le. 0 .or. iax .gt. 4) go to 9200
              inc    = inc    + 1
          endif
          if (IRTDUP(iax) .le. 1) go to 9200
c
c......ROTABL/ON,m1,[...] [THRU] [,mn]
c
          if (inc .gt. MXCL) go to 9500
          ithru  = 0
          do 1000 i=inc,MXCL,1
              ipt    = PSTWD(i)
              if (IPSTWD(i) .eq. 152 .and. ithru .eq. 0) then
                  if (i .eq. inc .or. i .eq. MXCL .or. ithru .eq. 1)
     1                go to 9300
                  if (PSTWD(i+1) .lt. PSTWD(i-1)) go to 9400
                  ithru  = 1
              else
                  if (IPSTWD(i) .ne. 0) go to 9300
                  if (ipt .lt. 1 .or. ipt .gt. IRTDUP(iax)) go to 9400
                  ithru  = 0
              endif
 1000     continue
          do 1020 i=inc,MXCL,1
              if (IPSTWD(i) .eq. 152) then
                  iend   = PSTWD(i+1)
                  do 1010 j=ipt,iend,1
                      IRTDAC(j,iax) = ival
 1010             continue
              else
                  ipt    = PSTWD(i)
                  IRTDAC(ipt,iax) = ival
              endif
 1020     continue
c
c...ROTABL/ORIGIN
c
      else if (IPSTWD(1) .eq. 1027) then
          if (MXCL .lt. 4) go to 9000
          inc    = 2
          iax    = 1
c
c......ROTABL/ORIGIN,AXIS,n
c
          if (IPSTWD(inc) .eq. 132) then
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              iax   = PSTWD(inc)
              if (iax .le. 0 .or. iax .gt. 4) go to 9200
              if (IRTOUT(iax) .ne. 1) go to 9200
              inc    = inc    + 1
          endif
c
c......ROTABL/ORIGIN,x,y,z
c
          if (MXCL-inc .lt. 2) go to 9000
          do 2100 i=1,3,1
              if (IPSTWD(inc) .ne. 0) go to 9300
              rary(i) = PSTWD(inc)
              inc    = inc    + 1
 2100     continue
c
c.........Set indexes for nutating axes
c
          ist     = IRTINC(iax)
          ien     = IRTINC(iax)
          if (ISCWRK(1,iax) .ne. 0) then
              ist    = ist    - 1
              ien    = ien    + 1
              if (ISCWRK(2,iax) .ne. 0) then
                  ist    = ist    - 1
                  ien    = ien    + 1
              endif
          endif
c
c.........Set Head origin
c
          do 2120 i=ist,ien-1,1
              if (IRTYPE(IRTINC(iax)) == 2) then
                  TABORG(1,i) = 0.
                  TABORG(2,i) = 0.
                  TABORG(3,i) = 0.
              else
                  TABORG(1,i) = rary(1) * METCNV
                  TABORG(2,i) = rary(2) * METCNV
                  TABORG(3,i) = rary(3) * METCNV
              endif
 2120     continue
          TABORG(1,ien) = rary(1) * METCNV
          TABORG(2,ien) = rary(2) * METCNV
          TABORG(3,ien) = rary(3) * METCNV
c
c......Check for end of command
c
          if (inc .le. MXCL) then
              inc    = inc    - 2
              go to 9700
          endif
c
c...ROTABL/TABLE,HEAD
c
      else if (IPSTWD(1) .eq. 177 .or. IPSTWD(1) .eq. 1002) then
          if (MXCL .lt. 2) go to 9000
          inc    = 1
          iax    = 0
          ihed   = 0
          if (IPSTWD(1) .eq. 1002) ihed = 1
          do 2990 i=1,4,1
              iwrk(1,i) = 0
              iwrk(2,i) = 0
              irt(i) = 0
 2990     continue
c
c......ROTABL/TABLE,HEAD
c
 3000     if (iax .eq. 4 .or. (iax .eq. 2 .and. LICOPT(6) .eq. 0))
     1            go to 9800
          iax    = iax    + 1
          if (IPSTWD(inc) .eq. 177) then
              if (ihed .eq. 1) go to 9900
              iry(iax) = 1
              inc    = inc    + 1
          else if (IPSTWD(inc) .eq. 1002) then
              iry(iax) = 2
              inc    = inc    + 1
          else
              iry(iax) = iry(iax-1)
          endif
c
c......ROTABL/TABLE,AAXIS,BAXIS,CAXIS
c
          if (inc .gt. MXCL) go to 9500
          if (IPSTWD(inc) .eq. 140) then
              irt(iax) = 1
          else if (IPSTWD(inc) .eq. 141) then
              irt(iax) = 2
          else if (IPSTWD(inc) .eq. 142) then
              irt(iax) = 3
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
          inc    = inc    + 1
c
c.........ROTABL/TABLE,-AXIS,a,b
c
          ipt    = 1
 3050     if (inc .le. MXCL) then
              if (IPSTWD(inc) .eq. 84) then
                  iwrk(ipt,iax) = 1
              else if (IPSTWD(inc) .eq. 85) then
                  iwrk(ipt,iax) = 2
              else if (IPSTWD(inc) .eq. 86) then
                  iwrk(ipt,iax) = 3
              else
                  go to 3000
              endif
              if (LICOPT(6) .eq. 0) go to 9800
c
              inc    = inc    + 1
              if (inc .gt. MXCL) go to 9300
              if (IPSTWD(inc) .ne. 0) go to 9300
              rrot(ipt,iax) = PSTWD(inc)
c
              ipt    = ipt    + 1
              inc    = inc    + 1
              if (inc .le. MXCL) go to 3050
          endif
c
c......Prepare to initialize rotaries
c
          IRTNUM = iax
          do 3080 i=1,IRTNUM,1
              IRTYPE(i) = iry(i)
              IRTWRK(i) = irt(i)
              ISCWRK(1,i) = iwrk(1,i)
              ISCWRK(2,i) = iwrk(2,i)
              SCROT(1,i) = rrot(1,i)
              SCROT(2,i) = rrot(2,i)
              TABORG(1,i) = TABORG(1,IRTINC(i))
              TABORG(2,i) = TABORG(2,IRTINC(i))
              TABORG(3,i) = TABORG(3,IRTINC(i))
              AXSSTO(i+6) = 0.
              ROTBAS(i) = 0.
 3080     continue
          call axsadr (AXSSTO,rlin,ROTSTO,tvec)
          call setpos
          IRDEAD(1) = 0
          IRDEAD(2) = 0
          IRDEAD(3) = 1
          IRDEAD(4) = 1
c
c......Initialize rotary axes
c
          call rscini
          call rotbeg
c
c...ROTABL/SIZE
c
      else if (IPSTWD(1) .eq. 196) then
          if (MXCL .lt. 2) go to 9000
          inc    = 2
          iax    = 1
c
c......ROTABL/SIZE,AXIS,n
c
          if (IPSTWD(inc) .eq. 132) then
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              iax   = PSTWD(inc)
              if (iax .le. 0 .or. iax .gt. 4) go to 9200
              if (IRTOUT(iax) .ne. 1) go to 9200
              inc    = inc    + 1
          endif
c
c......ROTABL/SIZE,n
c
          if (inc .gt. MXCL) go to 9000
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .le. 0.) go to 9400
          ROTCRM(iax) = PSTWD(inc) / 360.d0
          rary(1) = PSTWD(inc)
          if (RSIZFL .eq. 1) rary(1) = rary(1) / PI / 2.d0
          if (RSIZFL .eq. 2) rary(1) = rary(1) / PI
          if (RSIZCD(1) .ne. 0 .and. RSIZCD(iax+1) .ne. 0) then
              call codout (RSIZCD(1),RSIZCV)
              call codout (RSIZCD(iax+1),rary(1))
              call clrbuf
          endif
          if (inc .lt. MXCL) go to 9700
c
c...ROTABL/SHORT
c
      else if (IPSTWD(1) .eq. 851) then
          if (MXCL .lt. 2) go to 9000
          if (IPSTWD(2) .eq. 0) go to 9500
          inc    = 2
          iax    = 1
          IRSRTE(1) = IRTRTE(1)
          IRSRTE(2) = IRTRTE(2)
          IRSRTE(3) = 0
c
c......ROTABL/SHORT,COMBIN
c
          if (IPSTWD(inc) .eq. 1071) then
              IRTRTE(1) = 1
c
c......ROTABL/SHORT,LARGE
c
          else if (IPSTWD(inc) .eq. 7) then
              IRTRTE(1) = 2
c
c......ROTABL/SHORT,PLUS
c
          else if (IPSTWD(inc) .eq. 19) then
              IRTRTE(1) = 3
c
c......ROTABL/SHORT,AXIS,n
c
          else if (IPSTWD(inc) .eq. 132) then
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              iax   = PSTWD(inc)
              if (iax .le. 0 .or. iax .gt. 4) go to 9200
              if (IRTOUT(iax) .ne. 1) go to 9200
              IRTRTE(1) = 4
              IRTRTE(2) = iax
c
c......Unrecognized minor word
c
          else
              if (IPSTWD(inc) .eq. 0) go to 9500
              go to 9100
          endif
c
c......ROTABL/SHORT,NEXT
c
          if (inc .lt. MXCL) then
              inc    = inc    + 1
              if (IPSTWD(inc) .eq. 162) then
                  IRSRTE(3) = 1
              else if (IPSTWD(inc) .ne. 71) then
                  go to 9100
              endif
          endif
          if (inc .lt. MXCL) go to 9700
c
c...Unrecognized minor word
c
      else
          if (IPSTWD(inc) .eq. 0) go to 9500
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
c...No such axis
c
 9200 call psterr (2,'NOAXIS',' ',inc)
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
c...Not valid for this machine
c
 9800 call psterr (1,'NOTVALID',' ',inc)
      go to 8000
c
c...Define tables first
c
 9900 call psterr (1,'TAB1ST',' ',inc)
      go to 8000
      end
