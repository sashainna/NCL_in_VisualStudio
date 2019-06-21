c
c***********************************************************************
c
c   FILE NAME:  pstwqz
c   CONTAINS:
c               qaxis   rewind  seqno   smooth  spindl  stop    tmark
c               tmset
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pstwqz.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:11
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  QAXIS
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 QAXIS/qval [,feed]
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine qaxis
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (REGFRC,KPOSMP(0603))
      equivalence (SGMCOD,KPOSMP(1018)), (MOTCOD,KPOSMP(1240))
      equivalence (FEDCD ,KPOSMP(3162))
c
      integer*4 MXCL,IPSTWD(50),MOTCOD,SGMCOD(10),REGFRC(96),FEDCD(8)
c
      equivalence (PSTWD ,POSMAP(0441)), (MOTCDV,POSMAP(1284))
      equivalence (SGMQVL,POSMAP(5562))
c
      real*8 PSTWD(50),MOTCDV,SGMQVL
c
      integer*4 inc
c
c...Initialize routine
c
      inc    = 1
c
c...QAXIS/qpos,feed
c
      if (MXCL .eq. 2) then
          if (IPSTWD(inc) .ne. 0) go to 9000
          inc    = 2
          if (IPSTWD(inc) .ne. 0) go to 9100
          call codout (MOTCOD,MOTCDV)
          if (SGMCOD(1) .ne. 0) then
              REGFRC(SGMCOD(1)) = 1
              call codout (SGMCOD(1),PSTWD(1))
              call codout (FEDCD(1),PSTWD(2))
              call clrbuf
          endif
c
c...QAXIS/qpos
c
      else if (MXCL .eq. 1) then
          if (IPSTWD(inc) .ne. 0) go to 9100
          SGMQVL = PSTWD(1)
c
c...Invalid number of parameters
c
      else
          go to 9000
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
c...Number expected
c
 9100 call psterr (2,'NUMBEXP',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rewind
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 REWIND
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rewind
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (REWCOD,KPOSMP(1080))
c
      integer*4 MXCL,REWCOD
c
      equivalence (REWCDV,POSMAP(1172))
c
      real*8 REWCDV
c
c...Output stop code
c
      if (MXCL .ne. 0) go to 9000
      call codout (REWCOD,REWCDV)
      call clrbuf
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
c   SUBROUTINE:  seqno
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 SEQNO/ON
c                       OFF
c                       n(,INCR,i(,m))
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine seqno
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ISEQSW,KPOSMP(0845)), (SEQFRQ,KPOSMP(0846))
      equivalence (ISEQBK,KPOSMP(0852))
c
      integer*4 MXCL,IPSTWD(50),ISEQSW,SEQFRQ,ISEQBK
c
      equivalence (PSTWD ,POSMAP(0441)), (ISEQ  ,POSMAP(1164))
      equivalence (SEQINC,POSMAP(1165))
c
      real*8 PSTWD(50),ISEQ,SEQINC
c
      integer*4 ifrq,inc
c
      real*8 rnum,rinc
c
c...Initialize routine
c
      inc    = 0
      if (MXCL .eq. 0) go to 9000
      inc    = 1
c
c...SEQNO/ON
c
      if (IPSTWD(1) .eq. 71) then
          ISEQSW = 1
          ISEQBK = 99999999
          if (MXCL .gt. 1) go to 9200
c
c...SEQNO/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          ISEQSW = 0
          if (MXCL .gt. 1) go to 9200
c
c...SEQNO/n
c
      else
          if (IPSTWD(1) .ne. 0) go to 9100
          if (PSTWD(1) .lt. 0.) go to 9400
          rnum   = PSTWD(1)
          rinc   = SEQINC
          ifrq   = 99999999
c
c......SEQNO/n,INCR,i
c
          inc    = 2
          if (MXCL .ge. 2) then
              if (IPSTWD(inc) .eq. 0) go to 9500
              if (IPSTWD(inc) .ne. 66) go to 9100
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .lt. 0.) go to 9400
              rinc   = PSTWD(inc)
              ifrq   = 1
c
c.........SEQNO/n,INCR,i,m
c
              inc    = inc    + 1
              if (inc .le. MXCL) then
                  if (IPSTWD(inc) .ne. 0) go to 9300
                  if (PSTWD(inc) .le. 0) go to 9400
                  ifrq   = PSTWD(inc)
              endif
          endif
c
          ISEQSW = 1
          ISEQBK = 99999999
          ISEQ   = rnum
          SEQINC = rinc
          SEQFRQ = ifrq
          if (inc .lt. MXCL) go to 9200
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
      end
c
c***********************************************************************
c
c   SUBROUTINE:  set
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 SET/LINEAR
c                     LINCIR(,toler)(,ALL   )
c                                     COMBIN
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine set
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (ICMBSW,KPOSMP(1385)), (ICSPRE,KPOSMP(1386))
c
      integer*4 MXCL,IPSTWD(20),ICMBSW,ICSPRE
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (CIRTOS,POSMAP(2241))
      equivalence (FUZZ4 ,POSMAP(4912))
c
      real*8 PSTWD(20),CIRTOS,FUZZ4,METCNV
c
      integer*4 inc,ncmb,ncsw
c
      real*8 rnum
c
      rnum   = CIRTOS
      inc    = 1
      if (MXCL .lt. 1) go to 9100
c
c...SET/LINEAR
c
      if (IPSTWD(inc) .eq. 76) then
         ncmb = 0
         ncsw = 0
         if (MXCL .gt. inc) go to 9300
c
c...SET/LINCIR
c
      else if (IPSTWD(inc) .eq. 95) then
         ncsw = 1
         ncmb = 0
  100    inc = inc + 1
         if (inc .gt. MXCL) go to 1000
         if (IPSTWD(inc) .eq. 0) then
            if (PSTWD(inc) .lt. FUZZ4) go to 9400
            rnum = PSTWD(inc) * METCNV
         else
            if (IPSTWD(inc) .eq. 1071) then
               ncmb = 1
            else if (IPSTWD(inc) .eq. 51) then
               ncmb = 0
            else
               go to 9200
            end if
         end if
         go to 100
      else
         go to 9200
      end if
c
c...set command vars
c
 1000 ICMBSW = ncmb
      ICSPRE = ncsw
      CIRTOS = rnum
c
 8000 return
c
c...Invalid command
c
 9100 call psterr (2,'INVPSYN',' ',1)
      go to 8000
c
c...Invalid minor word
c
 9200 call psterr (2,'INVMINOR',' ',inc)
      go to 8000
c
c...Invalid command syntax
c
 9300 call psterr (2,'INVPSYNW',' ',MXCL+1)
      go to 1000
c
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
c
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smooth
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 SMOOTH/OFF
c                        ON [,STEP,max [,npts]]
c
c                 SMOOTH/ATANGL,OFF
c                               ang [,dis] [STEP,max [,npts]]
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine smooth
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MACHTP,KPOSMP(1201)), (SMOKON,KPOSMP(4071))
      equivalence (SMOKAN,KPOSMP(4072)), (SMOCCD,KPOSMP(4073))
      equivalence (SMOKDO,KPOSMP(4083)), (SMONPT,KPOSMP(4095))
c
      integer*4 MXCL,IPSTWD(50),MACHTP,SMOKON,SMOKAN,SMOCCD(2),
     -          SMOKDO,SMONPT
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (SMOCVL,POSMAP(4448))
      equivalence (SMORAN,POSMAP(4464)), (SMODIS,POSMAP(4465))
      equivalence (SMOSTP,POSMAP(4466))
c
      real*8 PSTWD(50),SMORAN,SMODIS,SMOCVL(2),SMOSTP,METCNV
c
      integer*4 inc,imod,inum
c
      real*8 rnum,rnum1,rnum2
c
      imod   = 0
      rnum1  = 0.
      inum   = 1
c
c...SMOOTH/ATANGL,ang[,dis]
c
      if (MACHTP .ne. 3) then
          call psterr (2,'NOTVALID',' ',1)
          go to 8000
      end if
      inc    = 1
      if (MXCL .lt. 1) go to 9500
      if (IPSTWD(inc) .eq. 1) then
          inc    = 2
          if (MXCL .lt. 2) go to 9100
          if (IPSTWD(inc) .eq. 0) then
              rnum = PSTWD(inc)
              if (rnum .lt. 0. .or. rnum .gt. 360.) go to 9200
  100         inc    = inc    + 1
              if (inc .le. MXCL) then
c
c......SMOOTH/ATANGL,...,STEP
c
                 if (IPSTWD(inc) .eq. 92) then
                     inc    = inc    + 1
                     if (inc .gt. MXCL) go to 9500
                     if (IPSTWD(inc) .ne. 0) go to 9100
                     if (PSTWD(inc) .lt. 0.) go to 9200
                     rnum1  = PSTWD(inc) * METCNV
                     inc    = inc    + 1
                     if (inc .le. MXCL) then
                         if (IPSTWD(inc) .ne. 0) go to 9100
                         inum   = PSTWD(inc)
                         if (inum .lt. 1) go to 9200
                     endif
c
c......SMOOTH/ATANGL,...,dis
c
                 else if (IPSTWD(inc) .eq. 0) then
                     if (PSTWD(inc) .lt. 0.) go to 9200
                     rnum2  = PSTWD(inc) * METCNV
                 else
                     go to 9600
                 endif
                 go to 100
              endif
              SMOKAN = 1
              SMORAN = rnum
              SMOSTP = rnum1
              SMONPT = inum
              SMODIS = rnum2
c
c...SMOOTH/ATANGL,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              SMOKAN = 2
              SMOKDO = 2
              call smoout
          else
              go to 9100
          end if
c
c...SMOOTH/OFF
c...       ON
c
      else if (IPSTWD(inc) .eq. 72) then
          SMOKON = 2
          imod   = 1
      else if (IPSTWD(inc) .eq. 71) then
          inc    = inc    + 1
          if (inc .le. MXCL) then
c
c......SMOOTH/ON,...,STEP
c
              if (IPSTWD(inc) .eq. 92) then
                  inc    = inc    + 1
                  if (inc .gt. MXCL) go to 9500
                  if (IPSTWD(inc) .ne. 0) go to 9100
                  if (PSTWD(inc) .lt. 0.) go to 9200
                  rnum1  = PSTWD(inc)
                  inc    = inc    + 1
                  if (inc .le. MXCL) then
                      if (IPSTWD(inc) .ne. 0) go to 9100
                      inum   = PSTWD(inc)
                      if (inum .lt. 1) go to 9200
                  endif
              else
                  go to 9600
              endif
          endif
          SMOKON = 1
          imod   = 1
          SMOSTP = rnum1
          SMONPT = inum
      else
          go to 9200
      end if
c
c...Output codes
c
      if (imod .eq. 1) then
          call clrbuf
          call codout (SMOCCD(SMOKON),SMOCVL(SMOKON))
          call clrbuf
      end if
      if (MXCL .gt. inc) go to 9500
c
 8000 return
c
c...Errors
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
c...Invalid command
c
 9300 call psterr (2,'NOTVALID',' ',inc)
      go to 8000
c
c...Invalid command syntax
c
 9400 call psterr (2,'INVPSYN',' ',MXCL+1)
      go to 8000
c
c...Invalid command syntax
c
 9500 call psterr (2,'INVPSYNW',' ',MXCL+1)
      go to 8000
c
c...Invalid minor word
c
 9600 call psterr (2,'INVMINOR',' ',MXCL+1)
      go to 8000
c
      end
c
c***********************************************************************
c
c   SUBROUTINE:  spindl
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 SPINDL/ON
c                        OFF
c                        (RPM,)n(,CLW )(,LOW   )
c                         SFM     CCLW   MEDIUM
c                                 BOTH   HIGH
c                                        AUTO
c
c                 SPINDL/LOCK,ON
c                             OFF
c
c                 SPINDL/MAXRPM,n
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine spindl
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MCHOPT,KPOSMP(0308)), (SGMACT,KPOSMP(0996))
      equivalence (SGMCOD,KPOSMP(1018)), (MACHTP,KPOSMP(1201))
      equivalence (LTHDIA,KPOSMP(1228))
      equivalence (NSPRG ,KPOSMP(3101)), (SPNSFM,KPOSMP(3103))
      equivalence (SPNDCD,KPOSMP(3105))
      equivalence (SPNFCD,KPOSMP(3118)), (SPNOCD,KPOSMP(3121))
      equivalence (SPNOFF,KPOSMP(3124)), (SPNDIR,KPOSMP(3141))
      equivalence (SPNMOD,KPOSMP(3142)), (SPNRNG,KPOSMP(3143))
      equivalence (SPNSCD,KPOSMP(3144)), (MTPDYN,KPOSMP(4126))
c
      integer*4 MXCL,IPSTWD(50),NSPRG,SPNSFM,SPNSCD(2),SPNDCD(4),
     1          SPNFCD(3),SPNOCD(2),SPNOFF,SPNDIR,SPNMOD,SPNRNG,
     2          MCHOPT(20),MACHTP,LTHDIA(2),MTPDYN,SGMACT(2),
     3          SGMCOD(10)
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (PSTWD ,POSMAP(0441)), (AXSSTO,POSMAP(1425))
      equivalence (SPNDVL,POSMAP(3007))
      equivalence (SPNFVL,POSMAP(3019)), (SPNOVL,POSMAP(3022))
      equivalence (MAXSFM,POSMAP(3304)), (RPMSAV,POSMAP(3305))
      equivalence (SFMSAV,POSMAP(3306)), (RPM   ,POSMAP(3307))
      equivalence (SFM   ,POSMAP(3308)), (SPNRAD,POSMAP(3318))
      equivalence (SPNXVL,POSMAP(3319)), (SGMCDV,POSMAP(5552))
c
      real*8 PSTWD(50),SPNDVL(4),SPNFVL(3),SPNOVL(2),MAXSFM,RPMSAV,
     1       SFMSAV,RPM,SFM,METCNV,SPNRAD,SPNXVL,AXSSTO(10),SGMCDV(10)
c
      integer*4 inc,idir,imod,irg,ifl(5),ierr
c
      real*8 spd,rrad
c
      character*80 lmsg
c
c...SPINDL/LOCK
c
      inc    = 1
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 114) then
          inc    = 2
          if (MXCL .lt. 2) go to 9500
          if (IPSTWD(inc) .eq. 71) then
              call codout (SPNOCD(1),SPNOVL(1))
          else if (IPSTWD(inc) .eq. 72) then
              call codout (SPNOCD(2),SPNOVL(2))
          else
              go to 9100
          endif
c
c...SPINDL/MAXRPM
c
      else if (IPSTWD(1) .eq. 79) then
          inc    = 2
          if (MXCL .lt. 2) go to 9500
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .le. 0.) go to 9400
          MAXSFM = PSTWD(inc)
c
c......Machine controls SFM speeds
c
          if (SPNSFM .eq. 1) then
              call clrbuf
              call codout (SPNFCD(3),SPNFVL(3))
              call codout (SPNSCD(1),MAXSFM)
              call clrbuf
          endif
c
c...SPINDL/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          RPM    = 0.
          SFM    = 0.
          if (MACHTP .eq. 5 .and. SGMACT(1) .ne. 1) then
              call codout (SGMCOD(3),SGMCDV(3))
              call clrbuf
          else
              call codout (SPNDCD(3),SPNDVL(3))
              if (SPNOFF .eq. 1) call clrbuf
          endif
c
c...SPINDL/ORIENT
c
      else if (IPSTWD(1) .eq. 246) then
          RPM    = 0.
          SFM    = 0.
          call codout (SPNDCD(4),SPNDVL(4))
          if (SPNOFF .eq. 1) call clrbuf
c
c...SPINDL/ON
c
      else if (IPSTWD(1) .eq. 71) then
          RPM    = RPMSAV
          SFM    = SFMSAV
          if (MACHTP .eq. 5 .and. SGMACT(1) .ne. 1) then
              call codout (SGMCOD(2),SGMCDV(2))
              call clrbuf
              inc    = MXCL   + 1
          else
              if (SPNMOD .ne. 2 .or. SPNSFM .eq. 1) call spndon
          endif
c
c......SPINDL/n
c
      else
          imod   = SPNMOD
          idir   = SPNDIR
          irg    = 4
          if (MACHTP .eq. 1 .or. MACHTP .eq. 3 .or. MACHTP .eq. 5) then
              rrad   = 0.
          else
              rrad   = AXSSTO(1)
          endif
          ifl(1) = 0
          ifl(2) = 0
          ifl(3) = 0
          ifl(4) = 0
          ifl(5) = 0
  200     if (IPSTWD(inc) .eq. 0) then
              if (ifl(2) .eq. 1) go to 9600
              ifl(2) = 1
              spd    = PSTWD(inc)
              if (spd .le. 0.) go to 9400
c
c......SPINDL/RPM
c
          else if (IPSTWD(inc) .eq. 78) then
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = 1
              imod   = 1
c
c......SPINDL/SFM
c
          else if (IPSTWD(inc) .eq. 115) then
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = 1
              imod   = 2
c
c......SPINDL/CLW
c
          else if (IPSTWD(inc) .eq. 60) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              idir   = 1
c
c......SPINDL/CCLW
c
          else if (IPSTWD(inc) .eq. 59) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              idir   = 2
c
c......SPINDL/BOTH
c
          else if (IPSTWD(inc) .eq. 83) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              idir   = 3
c
c......SPINDL/LOW
c
          else if (IPSTWD(inc) .eq. 63) then
              if (ifl(4) .eq. 1) go to 9600
              ifl(4) = 1
              irg    = 1
              if (NSPRG .eq. 1) irg = 4
c
c......SPINDL/MEDIUM
c
          else if (IPSTWD(inc) .eq. 61) then
              if (ifl(4) .eq. 1) go to 9600
              ifl(4) = 1
              irg    = 2
              if (NSPRG .eq. 2) irg = 4
c
c......SPINDL/HIGH
c
          else if (IPSTWD(inc) .eq. 62) then
              if (ifl(4) .eq. 1) go to 9600
              ifl(4) = 1
              irg    = 3
              if (NSPRG .eq. 1) irg = 4
c
c......SPINDL/AUTO
c
          else if (IPSTWD(inc) .eq. 88) then
              if (ifl(4) .eq. 1) go to 9600
              ifl(4) = 1
              irg    = 4
c
c......SPINDL/(RADIUS,DIAMTR)
c
          else if (IPSTWD(inc) .eq. 23 .or. IPSTWD(inc) .eq. 205) then
              if (ifl(5) .eq. 1) go to 9600
              ifl(5) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0.) go to 9300
              if (PSTWD(inc) .lt. 0.) go to 9400
              rrad   = PSTWD(inc) * METCNV
              if (MTPDYN .eq. 2) then
                  if (IPSTWD(inc-1) .eq. 23 .and. LTHDIA(2) .eq. 2)
     1                    rrad = rrad * 2.
                  if (IPSTWD(inc-1) .eq. 205 .and. LTHDIA(2) .eq. 1)
     1                    rrad = rrad / 2.
              else
                  if (IPSTWD(inc-1) .eq. 23) rrad = rrad * 2.0d0
              endif
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          inc    = inc    + 1
          if (inc .le. MXCL) go to 200
c
c......Output spindle on
c
          SPNMOD = imod
          SPNDIR = idir
          SPNRNG = irg
          if (MTPDYN .eq. 2) then
              SPNRAD = rrad   - AXSSTO(1)
          else
              SPNRAD = rrad
          endif
          if (ifl(5) .eq. 0) then
              SPNXVL = 0
          else
              SPNXVL = rrad
          endif
          if (ifl(2) .eq. 1) then
              if (SPNMOD .eq. 1) then
                  if (ifl(2) .eq. 1) RPM    = spd
                  RPMSAV = RPM
              else
                  if (MCHOPT(1) .ne. MCHOPT(2)) then
                      if (MCHOPT(2) .eq. 1) then
                          SFM    = spd    * METCNV * (1000./12.)
                      else
                          SFM    = spd    * METCNV * (12./1000.)
                      endif
                  else
                      SFM    = spd
                  endif
                  SFMSAV = SFM
               endif
               if (ifl(2) .eq. 1 .and. SPNMOD .ne. 2 .or. SPNSFM .eq. 1)
     1                call spndon
          endif
      endif
c
c...Output Simulation record
c
      call simsta (0,lmsg,ierr)
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
c   SUBROUTINE:  stop
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 STOP
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine stop
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (STPSEQ,KPOSMP(0307))
      equivalence (STOPCD,KPOSMP(1076)), (IPRDES,KPOSMP(1154))
      equivalence (STPBLK,KPOSMP(3048))
c
      integer*4 MXCL,STPSEQ,STOPCD,IPRDES(2,10),STPBLK
c
      equivalence (STOPVL,POSMAP(1169))
c
      real*8 STOPVL
c
      integer*4 ierr
c
      character*80 lmsg
c
c...Output stop code
c
      if (MXCL .ne. 0) go to 9000
      call codout (STOPCD,STOPVL)
      call clrbuf
c
c...Force stop user defined block
c
      call pshblk (STPBLK)
c
c...Output print file record
c
      call prtrec (IPRDES(1,6))
c
c...Set end of seqence
c
      call endseq (STPSEQ)
c
c...Output Simulation record
c
      call simsta (1,lmsg,ierr)
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
c   SUBROUTINE:  tmark
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 TMARK(/n)(,NOW)
c                 TMARK/AUTO
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine tmark
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (NOUTSW,KPOSMP(1074))
c
      integer*4 MXCL,IPSTWD(50),NOUTSW
c
      equivalence (PSTWD ,POSMAP(0441)), (ISEQ  ,POSMAP(1164))
      equivalence (SEQINC,POSMAP(1165))
c
      real*8 PSTWD(50),ISEQ,SEQINC
c
      integer*4 inow,inc
c
      real*8 rnum
c
c...Initialize routine
c
      inc    = 1
c
c...TMARK/AUTO
c
      if (MXCL .ge. 1 .and. IPSTWD(1) .eq. 88) then
          call rwstop
          if (MXCL .gt. 1) go to 9200
c
c...TMARK
c
      else
          if (MXCL .eq. 0) then
              call tmset (ISEQ,1)
c
c......TMARK/NOW
c
          else if (IPSTWD(1) .eq.  161) then
              call tmset (ISEQ,1)
              NOUTSW = 1
              call clrbuf
              if (MXCL .gt. 1) go to 9200
c
c......TMARK/n
c
          else
              if (IPSTWD(1) .ne. 0) go to 9100
              if (PSTWD(1) .lt. 0.) go to 9400
              rnum   = PSTWD(1)
              inow   = 0
c
c.........TMARK/n,NOW
c
              if (MXCL .gt. 1) then
                  inc    = 2
                  if (IPSTWD(inc) .ne. 161) go to 9100
                  inow   = 1
              endif
c
              call tmset (rnum,2)
              if (inow .eq. 1) then
                  NOUTSW = 1
                  call clrbuf
              endif
              if (MXCL .gt. 2) go to 9200
          endif
      endif
c
c...End of routine
c
 8000 return
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
c...Input value out of range
c
 9400 call psterr (2,'INPRNG',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE: tmset (gval,kfl)
c
c   FUNCTION:  This routine sets up the next block as an alignment
c              block.
c
c   INPUT:  gval    R*8  D1  -  Value for the alignment block sequence
c                               number.
c
c           kfl     I*4  D1  -  1 = Output using current sequence number,
c                               2 = Use value in 'gval', 3 = Use CYCLE
c                               sequence number.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine tmset (gval,kfl)
c
      include 'post.inc'
c
      equivalence (IHDID ,KPOSMP(0851)), (ALNFLG,KPOSMP(0855))
c
      integer*4 IHDID,ALNFLG
c
      equivalence (ALNVAL,POSMAP(1166))
c
      real*8 ALNVAL
c
      integer*4 kfl
c
      real*8 gval
c
      IHDID  = 1
      ALNVAL = gval
      ALNFLG = kfl
c
c...End of routine
c
 8000 return
      end
