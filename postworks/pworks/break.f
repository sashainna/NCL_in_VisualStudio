c
c***********************************************************************
c
c   FILE NAME:  break
c   CONTAINS:
c               break  brknam  brkfil  brkini  brkrst  brktap  brkyet
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        break.f , 25.4
c     DATE AND TIME OF LAST  MODIFICATION
c        03/11/16 , 11:04:22
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  break (cmsg,kerr)
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 BREAK
c
c                 BREAK/OFF
c
c                 BREAK/(ON   )(,n)(,FEDTO,f)(,RAPTO,r)
c                        AUTO
c                        TIMES
c                        DELTA
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine break (cmsg,kerr)
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (BRKOP ,KPOSMP(1137))
c
      integer*4 MXCL,IPSTWD(50),BRKOP(10)
c
      equivalence (METCNV,POSMAP(0004)), (FTMCNV,POSMAP(0056))
      equivalence (PSTWD ,POSMAP(0441)), (BRKVR ,POSMAP(1205))
c
      real*8 METCNV,PSTWD(50),BRKVR(10),FTMCNV
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 inc,i,imod
c
      real*8 rv(6)
c
c...BREAK
c
      inc    = 0
      if (MXCL .eq. 0) then
          call brkyet (cmsg,kerr)
c
c...BREAK/OFF
c
      else if (IPSTWD(1) .eq. 72) then
          inc    = 1
          BRKOP(1) = 0
          call brkrst
          if (MXCL .gt. 1) go to 9500
c
c...BREAK/PCHFILE
c
      else if (IPSTWD(1) .eq. 5024) then
          call brkfil (cmsg,kerr)
          call brkrst
c
c...BREAK/mode
c
      else
          inc    = 1
          imod   = BRKOP(1)
          do 50 i=1,6,1
              rv(i) = BRKVR(i)
   50     continue
c
c......BREAK/ON
c
          if (IPSTWD(inc) .eq. 71) then
              imod   = 1
              inc    = inc    + 1
              if (inc .gt. MXCL) go to 500
              if (IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9200
                  rv(1) = PSTWD(inc) * FTMCNV
                  inc    = inc    + 1
              endif
c
c......BREAK/AUTO
c
          else if (IPSTWD(inc) .eq. 88) then
              imod   = 2
              inc    = inc    + 1
              if (inc .gt. MXCL) go to 500
              if (IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9200
                  rv(1) = PSTWD(inc) * FTMCNV
                  inc    = inc    + 1
              endif
c
c......BREAK/TIMES
c
          else if (IPSTWD(inc) .eq. 28) then
              imod   = 3
              inc    = inc    + 1
              if (inc .gt. MXCL) go to 500
              if (IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9200
                  rv(2) = PSTWD(inc)
                  inc    = inc    + 1
              endif
c
c......BREAK/DELTA
c
          else if (IPSTWD(inc) .eq. 188) then
              imod   = 4
              inc    = inc    + 1
              if (inc .gt. MXCL) go to 500
              if (IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9200
                  rv(3) = PSTWD(inc) * METCNV
                  inc    = inc    + 1
              endif
c
c......BREAK/SEQNO
c
          else if (IPSTWD(inc) .eq. 1019) then
              imod   = 5
              inc    = inc    + 1
              if (inc .gt. MXCL) go to 500
              if (IPSTWD(inc) .eq. 0) then
                  if (PSTWD(inc) .lt. 0.) go to 9200
                  rv(6) = PSTWD(inc)
                  inc    = inc    + 1
              endif
          endif
c
c......Get next parameter section
c
          if (inc .gt. MXCL) go to 500
  100     if (IPSTWD(inc) .eq. 0) go to 9000
c
c.........RAPTO
c
          if (IPSTWD(inc) .eq. 280) then
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9100
              if (PSTWD(inc) .lt. 0.) go to 9200
              rv(4) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........FEDTO
c
          else if (IPSTWD(inc) .eq. 281) then
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9100
              if (PSTWD(inc) .lt. 0.) go to 9200
              rv(5) = PSTWD(inc) * METCNV
              inc    = inc    + 1
c
c.........Invalid minor word
c
          else
              go to 9300
          endif
          if (inc .le. MXCL) go to 100
c
c......Set break variables
c
  500     BRKOP(1) = imod
          do 600 i=1,6,1
              BRKVR(i) = rv(i)
  600     continue
          call brkrst
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
c   SUBROUTINE:  brknam (kunit,cmsg,kerr)
c
c   FUNCTION:  This routine outputs creates the punch file name for
c              a new file after break command.  It also changes the
c              reel number on the PARTNO card.
c
c   INPUT:  kunit   I*4  D1  Logical unit number.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine brknam (kunit,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IDUMMY,KPOSMP(0088)), (MCHOPT,KPOSMP(0308))
c
      integer*4 IDUMMY,MCHOPT(20)
c
      equivalence (LPARTN,CPOSMP(0067))
c
      character*66 LPARTN
c
      integer*4 kunit,kerr
c
      character*(*) cmsg
c
      integer*4 nc,ipt,inc,i,nc1,ist,index,strlen1,irecl,ierr,nindex,
     -           inum
c
      character*10 ldig
      character*20 att(4)
      character*80 ldat
      character*(MAX_PATH) ldev
      character*(MAX_FILE) lfil,lext,sbuf
c
      data ldig /'0123456789'/
      data att /'sequential','list','formatted','new'/
      data irecl /512/
c
c...Close current punch file
c
      call clsfil (kunit)
c
c...Change reel number on PARTNO card
c
      call getvwd (5030,ldat,nc,2,PSTWRD,PSTWVL,NPSTWD)
      inc    = index(LPARTN,ldat(1:nc))
c
c......REEL found
c......Search for Reel number
c
      if (inc .ne. 0) then
          ist    = (inc+nc) + nindex(LPARTN(inc+nc:66),' ') - 1
          do 100 i=ist,66,1
              inc    = index(ldig,LPARTN(i:i))
              if (inc .eq. 0) go to 120
  100     continue
          i      = 67
c
c.........Reel number found
c.........Increment it
c
  120     if (i .ne. ist) then
              call ctoi (LPARTN(ist:i-1),inum,ierr)
              if (ierr .eq. 0) then
                  inum   = inum   + 1
                  call itoc (inum,ldat,nc,0)
                  sbuf   = LPARTN(1:ist-1) // ldat(1:nc) // LPARTN(i:66)
                  LPARTN = sbuf
                  SFRMBUF(1) = LPARTN
              endif
          endif
      endif
c
c...Increment program number
c
      if (MCHOPT(4) .ne. IDUMMY) MCHOPT(4) = MCHOPT(4) + 1
c
c...Open new punch file
c
      call fbreak (PCHFIL,ldev,lfil,lext)
c
c......Search for number on file extension
c
      nc     = strlen1(lext)
      do 500 i=1,nc,1
          ipt    = index(ldig,lext(i:i))
          if (ipt .ne. 0) go to 520
  500 continue
c
c......Number not found
c......Append '1' to end of extension
c
      lext   = lext(1:nc) // '1'
      go to 550
c
c......Number found
c......Increment number
c
  520 ipt    = i
      do 530 i=ipt,nc,1
          inc    = index(ldig,lext(i:i))
          if (inc .eq. 0) go to 540
  530 continue
      i      = nc     + 1
  540 call ctoi (lext(ipt:i-1),inum,ierr)
      if (ierr .eq. 0) then
          inum   = inum   + 1
          call itoc (inum,ldat,nc1,0)
          if (ipt .eq. 1) then
              sbuf   = ldat(1:nc1) // lext (i:nc)
          else
              sbuf   = lext(1:ipt-1) // ldat(1:nc1) // lext(i:nc)
          endif
          lext   = sbuf
      else
          lext   = lext(1:nc) // '1'
      endif
c
c......Open punch file
c
  550 call fparse (lfil,PCHFIL,ldev,lext)
      call opnfil (kunit,PCHFIL,att,irecl,cmsg,kerr)
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  brkpr1 (cmsg,kerr)
c
c   FUNCTION:  This routine outputs creates the print file name for
c              a new file after break command.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine brkpr1 (cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IDUMMY,KPOSMP(0088)), (MCHOPT,KPOSMP(0308))
c
      integer*4 IDUMMY,MCHOPT(20)
c
      equivalence (LPARTN,CPOSMP(0067))
c
      character*66 LPARTN
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,ipt,inc,i,nc1,ist,index,strlen1,irecl,ierr,nindex,
     -           inum
c
      character*10 ldig
      character*20 att(4)
      character*80 ldat
      character*(MAX_PATH) ldev
      character*(MAX_FILE) lfil,lext,sbuf
c
      data ldig /'0123456789'/
      data att /'sequential','list','formatted','new'/
      data irecl /132/
c
c...Close current print file
c
      call clsfil (LUNSC3)
c
c...Open new print file
c
      call fbreak (PRTFIL,ldev,lfil,lext)
c
c......Search for number on file extension
c
      nc     = strlen1(lext)
      do 500 i=1,nc,1
          ipt    = index(ldig,lext(i:i))
          if (ipt .ne. 0) go to 520
  500 continue
c
c......Number not found
c......Append '1' to end of extension
c
      lext   = lext(1:nc) // '1'
      go to 550
c
c......Number found
c......Increment number
c
  520 ipt    = i
      do 530 i=ipt,nc,1
          inc    = index(ldig,lext(i:i))
          if (inc .eq. 0) go to 540
  530 continue
      i      = nc     + 1
  540 call ctoi (lext(ipt:i-1),inum,ierr)
      if (ierr .eq. 0) then
          inum   = inum   + 1
          call itoc (inum,ldat,nc1,0)
          if (ipt .eq. 1) then
              sbuf   = ldat(1:nc1) // lext (i:nc)
          else
              sbuf   = lext(1:ipt-1) // ldat(1:nc1) // lext(i:nc)
          endif
          lext   = sbuf
      else
          lext   = lext(1:nc) // '1'
      endif
c
c......Open print file
c
  550 call fparse (lfil,PRTFIL,ldev,lext)
      call opnfil (LUNSC3,PRTFIL,att,irecl,cmsg,kerr)
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  brkfil (cmsg,kerr)
c
c   FUNCTION:  This routine outputs a FINI sequence to the current punch
c              file and opens up a new punch file.  It also changes the
c              reel number on the PARTNO card.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine brkfil (cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (BRKOP ,KPOSMP(1137))
c
      integer*4 BRKOP(10)
c
      equivalence (PHFILE,CPOSMP(2535))
c
      character*40 PHFILE
c
      integer*4 kerr
c
      character*(*) cmsg
c
      character*80 ldat
c
      data ldat /'XKl6speN9h'/
c
c...Output FINI sequence
c
      call fini
c
      if (PHFILE .ne. ' ' .and. IOPFL(7) .eq. 1) then
         call pchhdr (0,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
      else
c
c...Close current punch file,
c...open new
c
         call brknam (LUNSC4,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
      end if
c
c...Create new print file
c
      if (BRKOP(5) .eq. 1 .and. IOPFL(4) .eq. 1) then
         call brkpr1 (cmsg,kerr)
         if (kerr .ne. 0) go to 8000
      endif
c
c...Initialize certain post variables
c
      call brkini
c
c...Initialize new punch file
c
      call tapini
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  brkini
c
c   FUNCTION:  This routine resets certain post-processor variables
c              between Tape breaks that create a new punch file.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine brkini
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IPCHSQ,KPOSMP(1108))
      equivalence (IFIRST,KPOSMP(1131)), (MOT1ST,KPOSMP(1349))
      equivalence (IPAGE ,KPOSMP(1151)), (IPLINE,KPOSMP(1152))
c
      integer*4 IPCHSQ,IFIRST,MOT1ST,IPAGE,IPLINE
c
c...Reset Tape break parameters
c
      call brkrst
c
c...Initialize certain variables
c
      IFIRST = 1
      IPAGE  = 0
      IPCHSQ = 0
      IPLINE = IOPFL(3) + 1
      MOT1ST = 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  brkrst
c
c   FUNCTION:  This routine resets the Tape Break conditional para-
c              meters and should be called upon initialization, after a
C              tape break and when a new BREAK mode is specified.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine brkrst
c
      include 'post.inc'
c
      equivalence (BRKPRM,POSMAP(1215))
c
      real*8 BRKPRM(5)
c
c...Reset Tape Break parameters
c
      BRKPRM(1) = 0.
      BRKPRM(2) = 0.
      BRKPRM(3) = 0.
      BRKPRM(4) = 0.
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  brktap (cmsg,kerr)
c
c   FUNCTION:  This routine performs the default tape break sequence
c              (Retract tool, Open new punch file, etc.).
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine brktap (cmsg,kerr)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (BRKOP ,KPOSMP(1137))
      equivalence (IRETMD,KPOSMP(1340)), (IFITYP,KPOSMP(3150))
c
      integer*4 ITYPE,ISUBT,MXCL,BRKOP(10),IFITYP,IRETMD(2)
c
      equivalence (ISEQ  ,POSMAP(1164)), (SEQINC,POSMAP(1165))
      equivalence (BRKVR ,POSMAP(1205))
      equivalence (AXSOUT,POSMAP(1340)), (AXSSTO,POSMAP(1425))
      equivalence (PFEED ,POSMAP(3540))
c
      real*8 BRKVR(10),AXSOUT(10),AXSSTO(10),PFEED(4),ISEQ,SEQINC
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 icl(3),i,isav,ifl,ierr,inc
c
      real*8 rax(10),rsav
c
c...Check for TAPBRK Macro
c
      icl(1) = ITYPE
      icl(2) = ISUBT
      icl(3) = MXCL
      ITYPE  = 2000
      ISUBT  = 4022
      MXCL   = 0
      call ppcall (ifl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (ifl .eq. 0) then
          call brkrst
          go to 8000
      endif
      ITYPE = icl(1)
      ISUBT = icl(2)
      MXCL  = icl(3)
c
c...Retract tool if necessary
c
      if (BRKOP(1) .ne. 1 .and. BRKOP(3) .eq. 1) then
c
c......Save current position
c
          do 100 i=1,10,1
              rax(i) = AXSSTO(i)
  100     continue
c
c......Set feed rate for retract
c
          if (BRKVR(4) .eq. 0.) then
              call rapset (1,2)
          else
              isav   = IFITYP
              rsav   = PFEED(IFITYP)
              IFITYP = 1
              PFEED(1) = BRKVR(4)
          endif
c
c......Retract tool
c
          call retpln (IRETMD,AXSOUT,ierr)
          if (ierr .eq. 1) then
              call psterr (2,'VECPLN',' ',inc)
          else
              call movpos
          endif
c
c......Reset feed rate
c
          if (BRKVR(4) .eq. 0.) then
              call raprst
          else
              IFITYP = isav
              PFEED(1) = rsav
          endif
      endif
c
c...Create new punch file
c
      if (BRKOP(2) .eq. 1) then
          call brkfil (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      else
          call brkrst
      endif
c
c...Issue TMARK command
c
      if (BRKOP(4) .eq. 1) call tmset (ISEQ,1)
c
c...Plunge tool if necessary
c
      if (BRKOP(1) .ne. 1 .and. BRKOP(3) .eq. 1) then
c
c......Set feed rate for plunge
c
          if (BRKVR(5) .eq. 0.) then
              call rapset (1,2)
          else
              isav   = IFITYP
              rsav   = PFEED(IFITYP)
              IFITYP = 1
              PFEED(1) = BRKVR(5)
          endif
c
c......Plunge tool
c
          do 500 i=1,10,1
              AXSOUT(i) = rax(i)
  500     continue
          call movpos
c
c......Reset feed rate
c
          if (BRKVR(5) .eq. 0.) then
              call raprst
          else
              IFITYP = isav
              PFEED(1) = rsav
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
c   SUBROUTINE:  brkyet (cmsg,kerr)
c
c   FUNCTION:  This routine determines is a Tape Break is required at
c              this time.  If it is, then the Tape Break routines will
c              be called.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine brkyet (cmsg,kerr)
c
      include 'post.inc'
c
      equivalence (BRKOP ,KPOSMP(1137))
c
      integer*4 BRKOP(4)
c
      equivalence (BRKVR ,POSMAP(1205)), (BRKPRM,POSMAP(1215))
c
      real*8 BRKVR(10),BRKPRM(5)
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ibrk
c
c...Initialize routine
c
      ibrk   = 0
c
c...BREAK/ON or AUTO active
c
      if (BRKOP(1) .eq. 1 .or. BRKOP(1) .eq. 2) then
          if (BRKPRM(1) .ge. BRKVR(1)) ibrk = 1
c
c...BREAK/TIMES active
c
      else if (BRKOP(1) .eq. 3) then
          if (BRKPRM(2) .ge. BRKVR(2)) ibrk = 1
c
c...BREAK/DELTA active
c
      else if (BRKOP(1) .eq. 4) then
          if (BRKPRM(3) .ge. BRKVR(3)) ibrk = 1
c
c...BREAK/SEQNO active
c
      else if (BRKOP(1) .eq. 5) then
          if (BRKPRM(4) .ge. BRKVR(6)) ibrk = 1
      endif
c
c...Tape break is required
c
      if (ibrk .eq. 1) call brktap (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
