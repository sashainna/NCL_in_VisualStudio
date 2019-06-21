c
c***********************************************************************
c
c   FILE NAME:  auxfun
c   CONTAINS:
c               auxfun  force  insert  prefun  regord
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        auxfun.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/19/15 , 09:02:04
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  auxfun
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 AUXFUN/n(,NOW )
c                           NEXT
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine auxfun
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(20)
c
      integer*4 inc,inow
c
c...AUXFUN/n
c
      inc    = 1
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 0) then
          inc    = 2
          inow   = 2
c
c......AUXFUN/n,NOW
c
          if (inc .le. MXCL) then
              if (IPSTWD(inc) .eq. 161) then
                  inow   = 1
c
c......AUXFUN/n,NEXT
c
              else if (IPSTWD(inc) .eq. 162) then
                  inow   = 2
c
c......Unrecognized minor word
c
              else
                  if (IPSTWD(inc) .eq. 0) go to 9200
                  go to 9500
              endif
          endif
c
c......Output M-code
c
          call codout (-2,PSTWD(1))
          if (inow .eq. 1) call clrbuf
c
c...Number expected
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
c   SUBROUTINE:  force
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 FORCE/BLOCK,n(,NOW )
c                                NEXT
c
c                 FORCE/NOW
c
c                 FORCE/reg(,val)(,NOW   ),...
c                                  NEXT
c                                  MOTION
c                                  ON
c                                  OMIT
c                                  OFF
c                                  NOMORE
c
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine force
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (REGFRC,KPOSMP(0603)), (MUSRBK,KPOSMP(3022))
c
      integer*4 MXCL,IPSTWD(50),REGFRC(MAXFMT),MUSRBK
c
      equivalence (PSTWD ,POSMAP(0441))
      equivalence (REGSTO,POSMAP(1032)), (REGVAL,POSMAP(5000))
c
      real*8 PSTWD(50),REGSTO(MAXFMT),REGVAL(MAXFMT)
c
      integer*4 ist,inc,i,nprm,inum,ifrc,inxt
c
      real*8 rnum
c
c...Initialize routine
c
      if (MXCL .lt. 1) go to 9400
      inc    = 1
c
c...FORCE/BLOCK,n
c
      if (IPSTWD(1) .eq. 5027) then
          if (MXCL .lt. 2) go to 9400
          inc    = 2
          if (IPSTWD(inc) .ne. 0) go to 9100
          if (PSTWD(inc) .lt. 1 .or. PSTWD(inc) .gt. MUSRBK) go to 9200
          inxt   = 1
          if (MXCL .ge. 3) then
              inc    = 3
c
c......FORCE/BLOCK,n,NOW
c
              if (IPSTWD(inc) .eq. 161) then
                  inxt   = 0
c
c......FORCE/BLOCK,n,NEXT
c
              else if (IPSTWD(inc) .eq. 162) then
                  inxt   = 1
c
c......Unrecognized minor word
c
              else
                  if (IPSTWD(inc) .eq. 0) go to 9000
                  go to 9300
              endif
          endif
c
c......Force user defined block
c
          inum   = PSTWD(2)
          if (inxt .eq. 1) then
              call pshblk (inum)
          else
              call frcblk (inum,ifrc,1)
          endif
          if (MXCL .gt. inc) go to 9500
c
c...FORCE/NOW
c
      else if (IPSTWD(inc) .eq. 161) then
          call clrbuf
          if (MXCL .gt. inc) go to 9500
c
c...FORCE/reg
c
      else
          ist    = inc
c
c......Get next parameter section
c
  100     inc    = ist
          if (IPSTWD(ist) .ne. 0) go to 9100
c
c......Find end of parameter section
c
          do 200 i=inc,MXCL,1
              if (IPSTWD(i) .ne. 0) go to 250
  200     continue
          inc    = MXCL   + 1
          go to 9000
  250     nprm   = i      - inc
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
          inum   = PSTWD(inc)
          if (nprm .eq. 2) then
              inc    = inc    + 1
              if (IPSTWD(inc) .ne. 0) go to 9100
              rnum = PSTWD(inc)
              call regtyp (inum,rnum)
          else
              if (inum .le. 0 .and. inum .ge. -3) go to 9200
              if (inum .gt. 0) rnum = REGSTO(inum)
              call regtyp (inum,rnum)
              if (inum .le. 0) go to 9200
              rnum   = REGSTO(inum)
          endif
          if (inum .lt. 1 .or. inum .gt. MAXFMT) go to 9200
          REGVAL(inum) = rnum
c
c......FORCE/...,NOW
c
          inc    = inc    + 1
          if (IPSTWD(inc) .eq. 161) then
              REGFRC(inum) = 1
              call codout (inum,REGVAL(inum))
              call clrbuf
c
c......FORCE/...,NEXT
c
          else if (IPSTWD(inc) .eq. 162) then
              REGFRC(inum) = 1
c
c......FORCE/...,ON
c
          else if (IPSTWD(inc) .eq. 71) then
              REGFRC(inum) = 2
c
c......FORCE/...,MOTION
c
          else if (IPSTWD(inc) .eq. 5028) then
              REGFRC(inum) = 3
c
c......FORCE/...,OMIT
c
          else if (IPSTWD(inc) .eq. 176) then
              call setcod (inum,REGVAL(inum))
              REGFRC(inum) = -1
c
c......FORCE/...,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              call setcod (inum,REGVAL(inum))
              REGFRC(inum) = -2
c
c......FORCE/...,NOMORE
c
          else if (IPSTWD(inc) .eq. 53) then
              call setcod (inum,REGVAL(inum))
              REGFRC(inum) = 0
c
c......Invalid minor word
c
          else
              go to 9300
          endif
          ist    = inc    + 1
          if (ist .le. MXCL) go to 100
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
 9300 call psterr (2,'INVMINOR',' ',inc)
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
c   SUBROUTINE:  insert
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 INSERT text
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine insert
c
      include 'post.inc'
c
      equivalence (INSSEQ,KPOSMP(0304)), (INSEOB,KPOSMP(0305))
      equivalence (NEOB  ,KPOSMP(0841)), (PRTBNC,KPOSMP(1153))
      equivalence (IPRDES,KPOSMP(1154))
c
      integer*4 NEOB,PRTBNC,IPRDES(2,10),INSSEQ,INSEOB
c
      equivalence (LPSTWD,CPOSMP(0217)), (LEOB  ,CPOSMP(0971))
      equivalence (LSTBLK,CPOSMP(0988)), (PRTBLK,CPOSMP(1653))
c
      character*5 LEOB
      character*512 LPSTWD
      character*512 LSTBLK,PRTBLK
c
      integer*4 nc,strlen1,nci,ifrc
c
      character*512 sbuf,abuf
c
c...Build INSERT block
c
      call clrbuf
      nci    = strlen1(LPSTWD)
c
c......Generate sequence number
c
      nc     = 0
      if (INSSEQ .eq. 1) call sequnc (2,sbuf,nc,ifrc)
      if (nc .gt. 0 .and. nci .gt. 0) then
          abuf   = sbuf(1:nc) // LPSTWD(1:nci)
      else if (nci .gt. 0) then
          abuf   = LPSTWD(1:nci)
      else if (nc .gt. 0) then
          abuf   = sbuf(1:nc)
      end if
      nc     = nc     + nci
c
c...Add End-of-Block
c
      if (INSEOB .eq. 1 .and. NEOB .ne. 0) then
          if (NEOB .gt. nci .or.
     1        LPSTWD(nci-NEOB+1:nci) .ne. LEOB(1:NEOB)) then
              if (nc .gt. 0) then
                  sbuf   = abuf(1:nc) // LEOB(1:NEOB)
              else
                  sbuf   = LEOB(1:NEOB)
              end if
              nc     = nc     + NEOB
          else
              sbuf   = abuf
          endif
      else
          sbuf   = abuf
          if (INSEOB .ne. 1) nc = -nc
      endif
c
c...Output punch file record
c
c     call clrbuf
      call pakout (sbuf,nc,1)
c
c...Output print file record
c
      LSTBLK = sbuf
      PRTBLK = sbuf
      if (nc .lt. 0) nc = -nc
      PRTBNC = nc
      call prtrec (IPRDES(1,5))
      PRTBNC = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  append
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 APPEND text
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine append
c
      include 'post.inc'
c
      equivalence (NCAPPN,KPOSMP(0856))
c
      integer*4 NCAPPN
c
      equivalence (LPSTWD,CPOSMP(0217)), (APPSTR,CPOSMP(5987))
c
      character*512 LPSTWD,APPSTR
c
      integer*4 strlen1
c
c...Setup APPEND string
c
      NCAPPN = strlen1(LPSTWD)
      APPSTR = LPSTWD(1:NCAPPN)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prefun
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 PREFUN/n(,NOW )
c                           NEXT
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine prefun
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 MXCL,IPSTWD(50)
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(20)
c
      integer*4 inc,inow
c
c...PREFUN/n
c
      inc    = 1
      if (MXCL .eq. 0) go to 9000
      if (IPSTWD(1) .eq. 0) then
          inc    = 2
          inow   = 2
c
c......PREFUN/n,NOW
c
          if (inc .le. MXCL) then
              if (IPSTWD(inc) .eq. 161) then
                  inow   = 1
c
c......PREFUN/n,NEXT
c
              else if (IPSTWD(inc) .eq. 162) then
                  inow   = 2
c
c......Unrecognized minor word
c
              else
                  if (IPSTWD(inc) .eq. 0) go to 9200
                  go to 9500
              endif
          endif
c
c......Output G-code
c
          call codout (-1,PSTWD(1))
          if (inow .eq. 1) call clrbuf
c
c...Number expected
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
c   SUBROUTINE:  regodr
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 REGORD/(START,)r1,r2,...,rn
c                         NEXT
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine regodr
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (NRGORD,KPOSMP(0837)), (REGORD,KPOSMP(3707))
c
      integer*4 MXCL,IPSTWD(50),NRGORD,REGORD(92)
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      integer*4 inc,i,inum,ireg(92),nreg
c
c...Initialize routine
c
      inc    = 1
      if (MXCL .lt. 1) go to 9300
      nreg   = NRGORD
c
c...REGORD/START
c
      if (IPSTWD(1) .eq. 57) then
          inc    = inc    + 1
          nreg   = 0
c
c...REGORD/NEXT
c
      else if (IPSTWD(1) .eq. 162) then
          inc    = inc    + 1
      endif
c
c...Initialize register order
c
      do 100 i=1,MAXFMT,1
          ireg(i) = 0
  100 continue
      if (nreg .ne. 0) then
          do 150 i=1,nreg,1
              ireg(i) = REGORD(i)
  150     continue
      endif
c
c...REGORD/r1,...,rn
c
      if (inc .gt. MXCL) go to 9100
      do 200 i=inc,MXCL,1
          if (IPSTWD(i) .ne. 0) go to 9100
          inum   = PSTWD(i)
          if (inum .lt. 1 .or. inum .gt. MAXFMT) go to 9200
          if (nreg .eq. MAXFMT) go to 9300
          nreg   = nreg   + 1
          ireg(nreg) = inum
  200 continue
c
c...Store new register order
c
      NRGORD = nreg
      do 300 i=1,nreg,1
          REGORD(i) = ireg(i)
  300 continue
c
c...End of routine
c
 8000 return
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
c...Too many parameters
c
 9300 call psterr (1,'INVPSYN',' ',inc)
      go to 8000
      end
