c
c***********************************************************************
c
c   FILE NAME:  loadtl
c   CONTAINS:
c               loadtl  selctl  toolno  turret  unload  skewtl  ltlcmd
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        loadtl.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/29/13 , 15:59:41
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  loadtl
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 LOADTL/tn((,LENGTH),tl)(,OFFSET,h(,d))(,LARGE) $
c                                                         SMALL
c                        (,HIGH)(,AUTO  )
c                          LOW    MODIFY
c                                 MANUAL
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine loadtl
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IPRDES,KPOSMP(1154)), (MACHTP,KPOSMP(1201))
      equivalence (ITP   ,KPOSMP(1801)), (MXTOOL,KPOSMP(1803))
      equivalence (TOOLFL,KPOSMP(1804)), (TLCCD ,KPOSMP(1839))
      equivalence (TLCBLK,KPOSMP(1859)), (ITSELF,KPOSMP(1870))
      equivalence (LSTGRP,KPOSMP(1871)), (LSTTSP,KPOSMP(1872))
      equivalence (SELGRP,KPOSMP(1873)), (SELTSP,KPOSMP(1874))
      equivalence (THDOPT,KPOSMP(1878)), (NUMTHD,KPOSMP(1883))
c
      integer*4 MXCL,IPSTWD(50),ITP,MXTOOL,TOOLFL(20),TLCCD(20),TLCBLK,
     1          ITSELF,LSTGRP,LSTTSP,SELGRP,SELTSP,MACHTP,IPRDES(2,10),
     2          ISUBT,THDOPT(5),NUMTHD
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425)), (CTOFRG,POSMAP(2401))
      equivalence (CUTCVR,POSMAP(2402))
      equivalence (TL    ,POSMAP(3601)), (TLTIM ,POSMAP(3721))
      equivalence (TLNO  ,POSMAP(3841))
      equivalence (TLCTIM,POSMAP(3962)), (TLCVL ,POSMAP(3963))
      equivalence (TLOBAS,POSMAP(3988))
      equivalence (TSELN ,POSMAP(3989)), (LSTTN ,POSMAP(3990))
      equivalence (TLOFRG,POSMAP(3991)), (TSELTM,POSMAP(3992))
      equivalence (MAXTN ,POSMAP(3995)), (THDVAL,POSMAP(3996))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 METCNV,PSTWD(50),VECSAV(3),STONUM(3,4),LINSTO(6),LSTTN,
     1       ROTSTO(20,2),AXSSTO(10),CTOFRG,TLCTIM,TLCVL(20),TSELN,
     2       TLOFRG,TL(120),TLTIM(120),TLNO(120),TSELTM,TLOBAS,MAXTN,
     3       CUTCVR(8),THDVAL(5)
c
      integer*4 inc,ifl(5),i,igrp,ispn,itfl,ipt,ix(3),lgrp(2),lspn(2),
     1          nc,ierr
c
      real*8 tnum,rfl(2),rtl,itn,nhed
c
      character*24 lbuf
      character*80 msg
c
      data lgrp /26,7/, lspn /62,63/
c
c...Initialize routine
c
      if (MACHTP .eq. 2) go to 9700
      if (MXCL .eq. 0) go to 9000
c
c...LOADTL/SET
c
      if (IPSTWD(1) .eq. 1087) then
          nhed   = 0
          do 50 i=2,MXCL,2
              if (IPSTWD(i) .eq. 0) go to 9500
              if (IPSTWD(i+1) .ne. 0) go to 9300
              if (nhed .eq. 5) go to 9200
              nhed   = nhed   + 1
              THDOPT(nhed) = IPSTWD(i)
              THDVAL(nhed) = PSTWD(i+1)
   50     continue
          NUMTHD = nhed
          go to 8000
      endif
c
c...LOADTL/tn
c
      inc    = 0
      do 100 i=1,5,1
          ifl(i) = 0
  100 continue
      rfl(1) = 0
      rfl(2) = 0
      inc    = 1
      if (ISUBT .eq. 1075) inc = 2
      if (IPSTWD(inc) .ne. 0) then
          tnum   = -1
          do 150 i=1,NUMTHD,1
              if (IPSTWD(inc) .eq. THDOPT(i)) then
                  tnum = THDVAL(i)
                  go to 160
              endif
  150     continue
  160     if (tnum .eq. -1) go to 9300
          inc = MXCL
      else
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .lt. 0) go to 9400
          tnum   = PSTWD(inc)
      endif
      inc    = inc    + 1
c
c......LOADTL/tn,LENGTH,tl
c
      itfl   = 0
      rtl    = 0
      if (inc .gt. MXCL) go to 7000
      if (IPSTWD(inc) .eq. 9 .or. IPSTWD(inc) .eq. 0) then
          if (IPSTWD(inc) .ne. 0) then
              inc    = inc    + 1
              if (inc .gt. MXCL) go to 9300
          endif
          if (IPSTWD(inc) .ne. 0) go to 9300
          rtl    = PSTWD(inc) * METCNV
          itfl   = 1
          ix(1)  = inc
          inc    = inc    + 1
      endif
c
c...Get remaining parameters
c
  200 if (inc .gt. MXCL) go to 7000
      if (IPSTWD(inc) .eq. 0) go to 9500
c
c......LOADTL/,OFFSET,h,d
c
      if (IPSTWD(inc) .eq. 705) then
          if (ifl(1) .eq. 1) go to 9600
          ifl(1) = 1
          inc    = inc    + 1
          if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
          rfl(1) = PSTWD(inc)
          inc    = inc    + 1
          if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
              ifl(2) = 1
              rfl(2) = PSTWD(inc)
              inc    = inc    + 1
          endif
c
c......LOADTL/,SMALL
c
      else if (IPSTWD(inc) .eq. 26) then
          if (ifl(3) .ne. 0) go to 9600
          ifl(3) = 1
          ix(2)  = inc
          inc    = inc    + 1
c
c......LOADTL/,LARGE
c
      else if (IPSTWD(inc) .eq. 7) then
          if (ifl(3) .ne. 0) go to 9600
          ifl(3) = 2
          ix(2)  = inc
          inc    = inc    + 1
c
c......LOADTL/,HIGH
c
      else if (IPSTWD(inc) .eq. 62) then
          if (ifl(4) .ne. 0) go to 9600
          ifl(4) = 1
          ix(3)  = inc
          inc    = inc    + 1
c
c......LOADTL/,LOW
c
      else if (IPSTWD(inc) .eq. 63) then
          if (ifl(4) .ne. 0) go to 9600
          ifl(4) = 2
          ix(3)  = inc
          inc    = inc    + 1
c
c......LOADTL/,AUTO
c
      else if (IPSTWD(inc) .eq. 88) then
          if (ifl(5) .ne. 0) go to 9600
          ifl(5) = 1
          inc    = inc    + 1
c
c......LOADTL/,MODIFY
c
      else if (IPSTWD(inc) .eq. 55) then
          if (ifl(5) .ne. 0) go to 9600
          ifl(5) = 2
          inc    = inc    + 1
c
c......LOADTL/,MANUAL
c
      else if (IPSTWD(inc) .eq. 158) then
          if (ifl(5) .ne. 0) go to 9600
          ifl(5) = 3
          inc    = inc    + 1
c
c......Invalid minor word
c
      else
          go to 9100
      endif
      go to 200
c
c...Check for SELCTL match
c
 7000 if (TOOLFL(1) .eq. 1) then
          if (ITSELF .eq. 1) then
              if (tnum .ne. TSELN) then
                  call perrst ('TOOLDIF2',1,msg,0,TSELN,lbuf,2)
                  call psterr (1,'TOOLDIF1',msg,1)
                  tnum   = TSELN
              endif
c
              if (ifl(3) .eq. 0) ifl(3) = SELGRP
              if (ifl(4) .eq. 0) ifl(4) = SELTSP
c
              if (TOOLFL(2) .eq. 1 .and. ifl(3) .ne. SELGRP) then
                  call getvwd (lgrp(SELGRP),lbuf,nc,2,PSTWRD,PSTWVL,
     1                         NPSTWD)
                  call perrst ('TOOLDIF2',1,msg,0,0.d0,lbuf,3)
                  call psterr (1,'TOOLGRP1',msg,ix(2))
                  ifl(3) = SELGRP
              endif
c
              if (TOOLFL(3) .eq. 1 .and. ifl(4) .ne. SELTSP) then
                  call getvwd (lspn(SELTSP),lbuf,nc,2,PSTWRD,PSTWVL,
     1                         NPSTWD)
                  call perrst ('TOOLDIF2',1,msg,0,0.d0,lbuf,3)
                  call psterr (1,'TOOLSPN1',msg,ix(3))
                  ifl(4) = SELTSP
              endif
              if (TSELTM .ne. 0.) then
                  call stlcod (tnum)
                  call clrbuf
              endif
c
c......Tool does not need to be selected
c......On MODIFY and MANUAL tool changes
c
          else if (ifl(5) .eq. 2 .or. ifl(5) .eq. 3) then
              SELGRP = LSTGRP
              SELTSP = LSTTSP
c
c......Tool was not selected
c
          else
              SELGRP = LSTGRP
              SELTSP = LSTTSP
              call psterr (1,'TOOLNSEL',' ',1)
              call stlcod (tnum)
              call clrbuf
          endif
      endif
c
c...Check for previously loaded tool
c
      if (TOOLFL(20) .eq. 2 .or. MXTOOL .ge. 120) then
          do 7100 i=1,MXTOOL,1
              if (tnum .eq. TLNO(i)) go to 7200
 7100     continue
      endif
c
c......Use next available slot
c
      if (MXTOOL .ge. 120) then
          call psterr (1,'TOOLOVR1','TOOLOVR2',1)
          i      = MXTOOL
      else
          MXTOOL = MXTOOL + 1
          i      = MXTOOL
      endif
      TL(i) = rtl
      TLTIM(i) = 0.
c
c...Store tool data
c
 7200 ITP    = i
      TLNO(ITP) = tnum
      if (itfl .eq. 0) rtl = TL(ITP)
      if (TLNO(i) .gt. MAXTN) then
          call perrst ('TOOLMAX',1,msg,0,MAXTN,lbuf,2)
          call psterr (1,msg,' ',1)
      endif
      if (rtl .ne. TL(ITP) .and. TL(ITP) .ne. 0.) then
          call perrst ('TOOLEN2',1,msg,0,TL(ITP),lbuf,2)
          call psterr (1,'TOOLEN1',msg,ix(1))
      endif
      TL(ITP) = rtl
      call skewtl (ITP)
      itn    = LSTTN
      igrp   = LSTGRP
      ispn   = LSTTSP
c
      LSTTN  = TLNO(ITP)
      if (TOOLFL(2) .ne. 1) ifl(3) = 1
      if (TOOLFL(3) .ne. 1) ifl(4) = 1
      if (ifl(1) .eq. 1) TLOFRG = rfl(1) + TLOBAS
      if (ifl(2) .eq. 1) CTOFRG = rfl(2) + CUTCVR(3)
      if (ifl(3) .ne. 0) LSTGRP = ifl(3)
      if (ifl(4) .ne. 0) LSTTSP = ifl(4)
      ITSELF = 0
c
c...Output tool change sequence
c
      if (ifl(5) .eq. 2) go to 7900
c
c......Turn off tool length compensation
c
      if (TOOLFL(8) .eq. 1) then
          rfl(1) = TLOFRG
          call tlnoff
          TLOFRG = rfl(1)
      endif
c
c......Manual tool change
c
      if (ifl(5) .eq. 3) then
          if (TOOLFL(6) .eq. 1) then
              MXCL   = 0
              call tmark
          endif
          call codout (TLCCD(6),TLCVL(6))
          call clrbuf
c
c......Normal tool change
c
      else
c
c.........Output unload code
c
          if (TOOLFL(5) .eq. 1) then
              ipt    = 7
              if (igrp .eq. 2) ipt = 9
              if (ispn .eq. 2) ipt = ipt + 1
              call codout (TLCCD(ipt),TLCVL(ipt))
          endif
c
c.........Output tool code
c
          tnum   = TLNO(ITP)
          if (TOOLFL(4) .eq. 1) tnum = itn
          if (TOOLFL(5) .ne. 1 .or. TOOLFL(15) .eq. 1) then
              call tlncod (tnum,tnum,TLOFRG,CTOFRG,1)
              if (TOOLFL(11) .eq. 1) call clrbuf
          endif
c
c.........Output tool change code
c
          if (TOOLFL(6) .eq. 1) then
              MXCL   = 0
              call tmark
          endif
          call codout (TLCCD(5),TLCVL(5))
          call pshblk (TLCBLK)
          call clrbuf
      endif
c
c......Output print file record
c
      call prtrec (IPRDES(1,8))
c
c......Alignment block after tool change
c
      if (TOOLFL(7) .eq. 1) then
          MXCL   = 0
          call tmark
      endif
c
c......Turn on tool length compensation
c
      if (TOOLFL(8) .eq. 1) call tlnon
c
c......End of sequence
c
      call endseq (TOOLFL(10))
c
c......Add tool time to machining time
c
      call addtim (TLCTIM)
c
c......Set new tool position
c
      call alladr (AXSSTO,LINSTO,STONUM,ROTSTO,VECSAV,5,1)
c
c...Output Simulation record
c
 7900 call simsta (3,msg,ierr)
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
c...Not valid for this machine
c
 9700 call psterr (2,'NOTVALID',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  selctl
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 SELCTL/tn  ((,LENGTH),tl)(,LARGE)(,HIGH)(,MODIFY ) $
c                        AUTO,lastn          SMALL   LOW    NOW
c                                                           NEXT,tm
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine selctl
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MACHTP,KPOSMP(1201)), (MXTOOL,KPOSMP(1803))
      equivalence (TOOLFL,KPOSMP(1804)), (ITSELF,KPOSMP(1870))
      equivalence (LSTGRP,KPOSMP(1871)), (LSTTSP,KPOSMP(1872))
      equivalence (SELGRP,KPOSMP(1873)), (SELTSP,KPOSMP(1874))
      equivalence (THDOPT,KPOSMP(1878)), (NUMTHD,KPOSMP(1883))
c
      integer*4 MXCL,IPSTWD(50),MXTOOL,TOOLFL(20),ITSELF,SELGRP,SELTSP,
     1          MACHTP,ITYPE,ISUBT,LSTGRP,LSTTSP,THDOPT(5),NUMTHD
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (CTOFRG,POSMAP(2401)), (TL    ,POSMAP(3601))
      equivalence (TLNO  ,POSMAP(3841)), (TSELN ,POSMAP(3989))
      equivalence (TSELTM,POSMAP(3992)), (MAXTN ,POSMAP(3995))
      equivalence (THDVAL,POSMAP(3996))
c
      real*8 METCNV,PSTWD(50),CTOFRG,TSELN,TSELTM,TL(120),TLNO(120),
     1       MAXTN,THDVAL(5)
c
      integer*4 inc,ifl(3),i,itfl,ipt,isw,ist

      real*8 tnum,rtm,rtl
c
      character*80 msg
c
c...Initialize routine
c
      inc    = 0
      if (MACHTP .eq. 2 .or. TOOLFL(1) .ne. 1) go to 9700
      do 5 i=1,3,1
          ifl(i) = 0
    5 continue
      if (MXCL .eq. 0) go to 9000
      inc    = 1
      if (ISUBT .eq. 1074) inc = 2
c
c...SELCTL/AUTO [,lastn]
c
      if (IPSTWD(inc) .eq. 88) then
          isw    = 0
c
c......Find next LOADTL
c
   20     call clsamp (isw)
          isw    = 1
          ist    = 0
c
c......Found FINI. Give error if tool number not specied, otherwise
c......save tool number in PSTWDS array.
c
          if (ITYPE .eq. 14000) then
              call clsamp (-1)
              if (MXCL .lt. 2 .or. IPSTWD(2) .ne. 0) go to 9800
              IPSTWD(1) = 0
              PSTWD(1)  = PSTWD(2)
              ist       = 1
c
c......Found LOADTL
c
          else
              if (ITYPE .ne. 2000) go to 20
              if (ISUBT .eq. 1055) then
                  if (MXCL .lt. 1) go to 20
                  ipt    = 1
              else if (ISUBT .eq. 1075) then
                  if (MXCL .lt. 2 .or. IPSTWD(1) .ne. 617) go to 20
                  ipt    = 2
              else
                  go to 20
              endif
              if (IPSTWD(ipt) .ne. 0) then
                  tnum   = -1
                  do 150 i=1,NUMTHD,1
                      if (IPSTWD(ipt) .eq. THDOPT(i)) then
                          tnum = THDVAL(i)
                          go to 160
                      endif
  150             continue
  160             if (tnum .eq. -1) go to 20
              else
                  if (IPSTWD(ipt) .ne. 0) go to 20
                  if (PSTWD(ipt) .lt. 0) go to 20
                  tnum   = PSTWD(ipt)
              endif
              call clsamp (-1)
              if (ISUBT .eq. 1056) then
                  IPSTWD(1) = 0
                  PSTWD(1)  = tnum
                  if (MXCL .gt. 1 .and. IPSTWD(2) .eq. 0) ist = 1
              else
                  IPSTWD(2) = 0
                  PSTWD(2)  = tnum
                  if (MXCL .gt. 2 .and. IPSTWD(3) .eq. 0) ist = 2
              endif
c
c......Found LOADTL. Save tool number and restore in PSTWDS array.
c......after rewinding cl file.
c
          endif
c
c......If the statement had a tool number, move any remaining data down.
c
          if (ist .ne. 0) then
              MXCL = MXCL - 1
              do 180 i=ist+1,MXCL,1
                  IPSTWD(i) = IPSTWD(i+1)
                  PSTWD(i)  = PSTWD(i+1)
  180         continue
          endif
      endif
c
c...SELCTL/tn
c
      if (IPSTWD(inc) .ne. 0) go to 9300
      if (PSTWD(inc) .lt. 0) go to 9400
      tnum   = PSTWD(inc)
      inc    = inc    + 1
c
c......SELCTL/tn,LENGTH,tl
c
      itfl   = 0
      rtl    = 0
      if (inc .gt. MXCL) go to 7000
      if (IPSTWD(inc) .eq. 9 .or. IPSTWD(inc) .eq. 0) then
          if (IPSTWD(inc) .ne. 0) then
              inc    = inc    + 1
              if (inc .gt. MXCL) go to 9300
          endif
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .lt. 0.) go to 9400
          rtl    = PSTWD(inc) * METCNV
          itfl   = inc
          inc    = inc    + 1
      endif
c
c...Get remaining parameters
c
  200 if (inc .gt. MXCL) go to 7000
      if (IPSTWD(inc) .eq. 0) go to 9500
c
c......SELCTL/,SMALL
c
      if (IPSTWD(inc) .eq. 26) then
          if (ifl(1) .ne. 0) go to 9600
          ifl(1) = 1
          inc    = inc    + 1
c
c......SELCTL/,LARGE
c
      else if (IPSTWD(inc) .eq. 7) then
          if (ifl(1) .ne. 0) go to 9600
          ifl(1) = 2
          inc    = inc    + 1
c
c......SELCTL/,HIGH
c
      else if (IPSTWD(inc) .eq. 62) then
          if (ifl(2) .ne. 0) go to 9600
          ifl(2) = 1
          inc    = inc    + 1
c
c......SELCTL/,LOW
c
      else if (IPSTWD(inc) .eq. 63) then
          if (ifl(2) .ne. 0) go to 9600
          ifl(2) = 2
          inc    = inc    + 1
c
c......SELCTL/,MODIFY
c
      else if (IPSTWD(inc) .eq. 55) then
          if (ifl(3) .ne. 0) go to 9600
          ifl(3) = 1
          inc    = inc    + 1
c
c......SELCTL/,NOW
c
      else if (IPSTWD(inc) .eq. 161) then
          if (ifl(3) .ne. 0) go to 9600
          ifl(3) = 2
          inc    = inc    + 1
c
c......SELCTL/,NEXT,tm
c
      else if (IPSTWD(inc) .eq. 162) then
          if (ifl(3) .ne. 0) go to 9600
          inc    = inc    + 1
          if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .lt. 0.) go to 9400
          rtm    = PSTWD(inc)
          ifl(3) = 3
          inc    = inc    + 1
c
c......Invalid minor word
c
      else
          go to 9100
      endif
      go to 200
c
c...Check for previously loaded tool
c
 7000 if (TOOLFL(20) .eq. 2 .or. MXTOOL .ge. 120) then
          do 7100 i=1,MXTOOL,1
              if (tnum .eq. TLNO(i)) go to 7200
 7100     continue
      endif
c
c......Use next available slot
c
      if (MXTOOL .ge. 120) then
          call psterr (1,'TOOLOVR1','TOOLOVR2',1)
          i      = MXTOOL
      else
          MXTOOL = MXTOOL + 1
          i      = MXTOOL
      endif
      TL(i) = rtl
c
c...Store tool data
c
 7200 ipt    = i
      TLNO(ipt) = tnum
      if (itfl .eq. 0) rtl = TL(ipt)
      if (TLNO(ipt) .gt. MAXTN) then
          call perrst ('TOOLMAX',1,msg,0,MAXTN,msg,2)
          call psterr (1,msg,' ',1)
      endif
      if (rtl .ne. TL(ipt)) then
          call perrst ('TOOLEN2',1,msg,0,TL(ipt),msg,2)
          call psterr (1,'TOOLEN1',msg,itfl)
      endif
      TL(ipt) = rtl
      call skewtl (ipt)
c
      TSELN  = TLNO(ipt)
      if (TOOLFL(2) .ne. 1) ifl(1) = 1
      if (TOOLFL(3) .ne. 1) ifl(2) = 1
      SELGRP = LSTGRP
      if (ifl(1) .ne. 0) SELGRP = ifl(1)
      SELTSP = LSTTSP
      if (ifl(2) .ne. 0) SELTSP = ifl(2)
      ITSELF = 1
c
c...Output select tool sequence
c
      if (ifl(3) .eq. 1) go to 8000
      if (ifl(3) .ne. 3) then
          call stlcod (TSELN)
          if (ifl(3) .eq. 2) call clrbuf
          TSELTM = 0.
      else
          TSELTM = rtm    / 60.
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
c...Not valid for this machine
c
 9700 call psterr (2,'NOTVALID',' ',inc)
      go to 8000
c
c...Next LOADTL command not found
c
 9800 call psterr (2,'NOLDTOOL',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  toolno
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 TOOLNO/ADJUST,ON
c                               OFF
c
c                 TOOLNO/ADJUST/n(,d)(,PLUS )(,XAXIS(,x))(,YAXIS,(y)) $
c                                      MINUS
c
c                        (,ZAXIS(,z))(,XYPLAN)(,NOW )
c                                      YZPLAN   NEXT
c                                      ZXPLAN
c
c                 TOOLNO/TIMES,ONCE
c                              ALL
c
c                 TOOLNO/OFSETL,ON
c                               OFF
c
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine toolno
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (SPITAX,KPOSMP(1279)), (NOTOOL,KPOSMP(1802))
      equivalence (TOOLFL,KPOSMP(1804))
      equivalence (TLOCD ,KPOSMP(1860)), (TLOFPL,KPOSMP(1875))
      equivalence (TLOFDR,KPOSMP(1876)), (TOFNXT,KPOSMP(1877))
c
      integer*4 MXCL,IPSTWD(50),SPITAX,TLOCD(8),TLOFPL,TLOFDR,TOFNXT,
     1          TOOLFL(20),NOTOOL
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (CTOFRG,POSMAP(2401)), (CUTCVR,POSMAP(2402))
      equivalence (TLOBAS,POSMAP(3988)), (TLOFRG,POSMAP(3991))
c
      real*8 METCNV,PSTWD(50),CTOFRG,TLOFRG,TLOBAS,CUTCVR(8)
c
      integer*4 inc,ifl(10),i
c
      real*8 tlr,tld,rax(3)
c
c...TOOLNO/ADJUST
c
      inc    = 1
      if (MXCL .lt. 2) go to 9000
      if (IPSTWD(1) .eq. 0) go to 9500
      if (IPSTWD(1) .eq. 159) then
c
c...TOOLNO/ADJUST,ON
c
          inc    = inc    + 1
          if (IPSTWD(inc) .eq. 71) then
              call tlnon
              if (MXCL .gt. inc) go to 9700
c
c...TOOLNO/ADJUST,OFF
c
          else if (IPSTWD(inc) .eq. 72) then
              TLOFRG = TLOBAS
              call tlnoff
              if (MXCL .gt. inc) go to 9700
c
c...TOOLNO/ADJUST,n
c
          else
              if (IPSTWD(inc) .ne. 0) go to 9300
              tlr    = PSTWD(inc)
              inc    = inc    + 1
              do 100 i=1,10,1
                  ifl(i) = 0
  100         continue
              if (inc .gt. MXCL) go to 7000
c
c......TOOLNO/ADJUST,d
c
              if (IPSTWD(inc) .eq. 0) then
                  tld    = PSTWD(inc)
                  inc    = inc    + 1
                  ifl(1) = 1
                  if (inc .gt. MXCL) go to 7000
              endif
c
c......Get remaining parameters
c
  200         if (inc .gt. MXCL) go to 7000
              if (IPSTWD(inc) .eq. 0) go to 9500
c
c.........TOOLNO/ADJUST,PLUS
c
              if (IPSTWD(inc) .eq. 19) then
                  if (ifl(2) .ne. 0) go to 9600
                  ifl(2) = 1
                  inc    = inc    + 1
c
c.........TOOLNO/ADJUST,MINUS
c
              else if (IPSTWD(inc) .eq. 10) then
                  if (ifl(2) .ne. 0) go to 9600
                  ifl(2) = 2
                  inc    = inc    + 1
c
c.........TOOLNO/ADJUST,XAXIS
c
              else if (IPSTWD(inc) .eq. 84) then
                  if (ifl(3) .ne. 0) go to 9600
                  ifl(3) = 1
                  inc    = inc    + 1
                  rax(1) = 0
                  if (IPSTWD(inc) .eq. 0) then
                      rax(1) = PSTWD(inc) * METCNV
                      inc    = inc    + 1
                  endif
c
c.........TOOLNO/ADJUST,YAXIS
c
              else if (IPSTWD(inc) .eq. 85) then
                  if (ifl(4) .ne. 0) go to 9600
                  ifl(4) = 1
                  inc    = inc    + 1
                  rax(2) = 0
                  if (IPSTWD(inc) .eq. 0) then
                      rax(2) = PSTWD(inc) * METCNV
                      inc    = inc    + 1
                  endif
c
c.........TOOLNO/ADJUST,ZAXIS
c
              else if (IPSTWD(inc) .eq. 86) then
                  if (ifl(5) .ne. 0) go to 9600
                  ifl(5) = 1
                  inc    = inc    + 1
                  rax(3) = 0
                  if (IPSTWD(inc) .eq. 0) then
                      rax(3) = PSTWD(inc) * METCNV
                      inc    = inc    + 1
                  endif
c
c.........TOOLNO/ADJUST,XYPLAN
c
              else if (IPSTWD(inc) .eq. 33) then
                  if (ifl(6) .ne. 0) go to 9600
                  ifl(6) = 1
                  inc    = inc    + 1
c
c.........TOOLNO/ADJUST,ZXPLAN
c
              else if (IPSTWD(inc) .eq. 41) then
                  if (ifl(6) .ne. 0) go to 9600
                  ifl(6) = 2
                  inc    = inc    + 1
c
c.........TOOLNO/ADJUST,YZPLAN
c
              else if (IPSTWD(inc) .eq. 37) then
                  if (ifl(6) .ne. 0) go to 9600
                  ifl(6) = 3
                  inc    = inc    + 1
c
c.........TOOLNO/ADJUST,NOW
c
              else if (IPSTWD(inc) .eq. 161) then
                  if (ifl(7) .ne. 0) go to 9600
                  ifl(7) = 1
                  inc    = inc    + 1
c
c.........TOOLNO/ADJUST,NEXT
c
              else if (IPSTWD(inc) .eq. 162) then
                  if (ifl(7) .ne. 0) go to 9600
                  ifl(7) = 2
                  inc    = inc    + 1
c
c.........Invalid minor word
c
              else
                  go to 9100
              endif
              go to 200
c
c......Setup tool length adjustment variables
c
 7000         TLOFRG = tlr    + TLOBAS
              if (ifl(1) .eq. 1) CTOFRG = tld    + CUTCVR(3)
              TLOFDR = ifl(2)
c
              if (ifl(6) .eq. 0) then
                  TLOFPL = SPITAX
              else
                  TLOFPL = ifl(6)
              endif
c
c......Wait for next Z-move to output
c
              if (ifl(7) .eq. 2) then
                   TOFNXT = 1
                   go to 8000
              endif
c
c......Output tool length compensation on block
c
              TOFNXT = 0
              if (ifl(3) .eq. 1) call codout (TLOCD(6),rax(1))
              if (ifl(4) .eq. 1) call codout (TLOCD(7),rax(2))
              if (ifl(5) .eq. 1) call codout (TLOCD(8),rax(3))
              call tlnon
              if (ifl(3) .eq. 1 .or. ifl(4) .eq. 1 .or.
     1            ifl(5) .eq. 1 .or.  ifl(7) .eq. 1) call clrbuf
          endif
c
c....TOOLNO/TIMES
c
      else if (IPSTWD(1) .eq. 28) then
          inc    = 2
          if (MXCL .ne. 2 .or. IPSTWD(2) .eq. 0) go to 9500
          if (IPSTWD(2) .eq. 51) then
              TOOLFL(20) = 1
          else if (IPSTWD(2) .eq. 325) then
              TOOLFL(20) = 2
          else
              go to 9100
          endif
c
c....TOOLNO/OFSETL
c
      else if (IPSTWD(1) .eq. 275) then
          inc    = 2
          if (MXCL .ne. 2 .or. IPSTWD(2) .eq. 0) go to 9500
          if (IPSTWD(2) .eq. 71) then
              NOTOOL = 1
          else if (IPSTWD(2) .eq. 72) then
              NOTOOL = 2
          else
              go to 9100
          endif
c
c...Unrecognized minor word
c
      else
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
c...Number expected
c
 9300 call psterr (2,'NUMBEXP',' ',inc)
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
c   SUBROUTINE:  turret
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 TURRET/FRONT
c                        REAR
c
c                 TURRET/CLW
c                        CCLW
c
c                 TURRET/tn((,RADIUS),ra)(,OFFSET,h(,d))(,ADJUST,z,x) $
c
c                        (,FRONT)(,CLW )(,AUTO  )
c                          REAR    CCLW   MODIFY
c                                         MANUAL
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine turret
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IPRDES,KPOSMP(1154)), (MACHTP,KPOSMP(1201))
      equivalence (ITP   ,KPOSMP(1801)), (MXTOOL,KPOSMP(1803))
      equivalence (TOOLFL,KPOSMP(1804)), (TURFL ,KPOSMP(1824))
      equivalence (TLCCD ,KPOSMP(1839)), (TLCBLK,KPOSMP(1859))
c
      integer*4 MXCL,IPSTWD(50),ITP,MXTOOL,TOOLFL(20),TLCCD(20),TLCBLK,
     1          TURFL(5),IPRDES(2,10),MACHTP
c
      equivalence (METCNV,POSMAP(0004)), (PSTWD ,POSMAP(0441))
      equivalence (CTOFRG,POSMAP(2401)), (CUTCVR,POSMAP(2402))
      equivalence (TL    ,POSMAP(3601))
      equivalence (TLTIM ,POSMAP(3721)), (TLNO  ,POSMAP(3841))
      equivalence (TLCTIM,POSMAP(3962)), (TLCVL ,POSMAP(3963))
      equivalence (TLOBAS,POSMAP(3988))
      equivalence (LSTTN ,POSMAP(3990)), (TLOFRG,POSMAP(3991))
      equivalence (TRZX  ,POSMAP(3993)), (MAXTN ,POSMAP(3995))
c
      real*8 METCNV,PSTWD(50),CTOFRG,TLCTIM,TLCVL(20),LSTTN,CUTCVR(8),
     1       TLOFRG,TL(120),TLTIM(120),TLNO(120),TRZX(2),TLOBAS,MAXTN
c
      integer*4 inc,ifl(6),i,itfl,ix,ierr,isav
c
      real*8 tnum,rfl(4),rtl
c
      character*20 lbuf
      character*80 msg
c
      if (MACHTP .ne. 2 .and. MACHTP .ne. 4) go to 9700
      inc    = 1
      isav   = MXCL
      if (MXCL .eq. 0) go to 9000
c
c...TURRET/FRONT
c
      if (IPSTWD(1) .eq. 148) then
          TURFL(2) = 1
c
c...TURRET/REAR
c
      else if (IPSTWD(1) .eq. 149) then
          if (TURFL(1) .ne. 2) go to 9700
          TURFL(2) = 2
c
c...TURRET/CLW
c
      else if (IPSTWD(1) .eq. 60) then
          call codout (TLCCD(11),TLCVL(11))
c
c...TURRET/CCLW
c
      else if (IPSTWD(1) .eq. 59) then
          call codout (TLCCD(12),TLCVL(12))
c
c...TURRET/tn
c
      else
          do 100 i=1,6,1
              ifl(i) = 0
  100     continue
          rfl(1) = 0
          rfl(2) = 0
          rfl(3) = 0
          rfl(4) = 0
          rtl    = 0
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .lt. 0) go to 9400
          tnum   = PSTWD(1)
          inc    = 2
c
c......TURRET/tn,RADIUS,ra
c
          itfl   = 0
          if (inc .gt. MXCL) go to 7000
          if (IPSTWD(inc) .eq. 23 .or. IPSTWD(inc) .eq. 0) then
              if (IPSTWD(inc) .ne. 0) then
                  inc    = inc    + 1
                  if (inc .gt. MXCL) go to 9300
              endif
              if (IPSTWD(inc) .ne. 0) go to 9300
              if (PSTWD(inc) .lt. 0.) go to 9400
              rtl    = PSTWD(inc) * METCNV
              itfl   = 1
              ix     = inc
              inc    = inc    + 1
          endif
c
c...Get remaining parameters
c
  200     if (inc .gt. MXCL) go to 7000
          if (IPSTWD(inc) .eq. 0) go to 9500
c
c......TURRET/,OFFSET,h,d
c
          if (IPSTWD(inc) .eq. 705) then
              if (ifl(1) .eq. 1) go to 9600
              ifl(1) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rfl(1) = PSTWD(inc)
              inc    = inc    + 1
              if (inc .le. MXCL .and. IPSTWD(inc) .eq. 0) then
                  ifl(2) = 1
                  rfl(2) = PSTWD(inc)
                  inc    = inc    + 1
              endif
c
c......TURRET/,ADJUST,z,x
c
          else if (IPSTWD(inc) .eq. 159) then
              if (ifl(3) .eq. 1) go to 9600
              ifl(3) = 1
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rfl(3) = PSTWD(inc)
              inc    = inc    + 1
              if (inc .gt. MXCL .or. IPSTWD(inc) .ne. 0) go to 9300
              rfl(4) = PSTWD(inc)
              inc    = inc    + 1
c
c......TURRET/,FRONT
c
          else if (IPSTWD(inc) .eq. 148) then
              if (ifl(4) .ne. 0) go to 9600
              ifl(4) = 1
              inc    = inc    + 1
c
c......TURRET/,REAR
c
          else if (IPSTWD(inc) .eq. 149) then
              if (TURFL(1) .ne. 2) then
                  call psterr (1,'INVPSYNW',' ',inc)
                  inc    = inc    + 1
              else
                  if (ifl(4) .ne. 0) go to 9600
                  ifl(4) = 2
                  inc    = inc    + 1
              endif
c
c......TURRET/,CLW
c
          else if (IPSTWD(inc) .eq. 60) then
              if (ifl(5) .ne. 0) go to 9600
              ifl(5) = 1
              inc    = inc    + 1
c
c......TURRET/,CCLW
c
          else if (IPSTWD(inc) .eq. 59) then
              if (ifl(5) .ne. 0) go to 9600
              ifl(5) = 2
              inc    = inc    + 1
c
c......TURRET/,AUTO
c
          else if (IPSTWD(inc) .eq. 88) then
              if (ifl(6) .ne. 0) go to 9600
              ifl(6) = 1
              inc    = inc    + 1
c
c......TURRET/,MODIFY
c
          else if (IPSTWD(inc) .eq. 55) then
              if (ifl(6) .ne. 0) go to 9600
              ifl(6) = 2
              inc    = inc    + 1
c
c......TURRET/,MANUAL
c
          else if (IPSTWD(inc) .eq. 158) then
              if (ifl(6) .ne. 0) go to 9600
              ifl(6) = 3
              inc    = inc    + 1
c
c......Invalid minor word
c
          else
              go to 9100
          endif
          go to 200
c
c...Check for previously loaded tool
c
 7000     if (TOOLFL(20) .eq. 2 .or. MXTOOL .ge. 120) then
              do 7100 i=1,MXTOOL,1
                  if (tnum .eq. TLNO(i)) go to 7200
 7100         continue
          endif
c
c......Use next available slot
c
          if (MXTOOL .ge. 120) then
              call psterr (1,'TOOLOVR1','TOOLOVR2',1)
              i      = MXTOOL
          else
              MXTOOL = MXTOOL + 1
              i      = MXTOOL
          endif
          TL(i) = rtl
          TLTIM(i) = 0.
c
c...Store tool data
c
 7200     ITP    = i
          TLNO(ITP) = tnum
          if (itfl .eq. 0) rtl = TL(ITP)
          if (TLNO(i) .gt. MAXTN) then
              call perrst ('TOOLMAX',1,msg,0,MAXTN,lbuf,3)
              call psterr (1,'msg',' ',1)
          endif
          if (rtl .ne. TL(ITP)) then
              call perrst ('TOOLRD2',1,msg,0,TL(ITP),lbuf,2)
              call psterr (1,'TOOLRD1',msg,ix)
          endif
          TL(ITP) = rtl
          call skewtl (ITP)
c
          LSTTN  = TLNO(ITP)
          if (ifl(1) .eq. 1) TLOFRG = rfl(1) + TLOBAS
          if (ifl(2) .eq. 1) CTOFRG = rfl(2) + CUTCVR(3)
          if (ifl(3) .eq. 1) then
              TRZX(1) = rfl(3)
              TRZX(2) = rfl(4)
          else
              TRZX(1) = 0.
              TRZX(2) = 0.
          endif
          if (ifl(4) .ne. 0) TURFL(2) =ifl(4)
c
c...Output tool change sequence
c
          if (ifl(6) .eq. 2) go to 8000
c
c......Turn off tool length compensation
c
          if (TOOLFL(8) .eq. 1) then
              rfl(1) = TLOFRG
              call tlnoff
              TLOFRG = rfl(1)
          endif
c
c......Manual tool change
c
          if (ifl(6) .eq. 3) then
              if (TOOLFL(6) .eq. 1) then
                  MXCL   = 0
                  call tmark
              endif
              call codout (TLCCD(6),TLCVL(6))
              call clrbuf
c
c......Normal tool change
c
          else
              if (ifl(5) .ne. 0)
     1                call codout (TLCCD(ifl(5)+10),TLCVL(ifl(5)+10))
c
c.........Output tool code
c
              tnum   = TLNO(ITP)
              call tlncod (tnum,tnum,TLOFRG,CTOFRG,1)
              if (TOOLFL(11) .eq. 1) call clrbuf
c
c.........Output tool change code
c
              if (TOOLFL(6) .eq. 1) then
                  MXCL   = 0
                  call tmark
              endif
              call codout (TLCCD(5),TLCVL(5))
              call pshblk (TLCBLK)
              call clrbuf
          endif
c
c......Output print file record
c
          call prtrec (IPRDES(1,8))
c
c......Alignment block after tool change
c
          if (TOOLFL(7) .eq. 1) then
              MXCL   = 0
              call tmark
          endif
c
c......Turn on tool length compensation
c
          if (TOOLFL(8) .eq. 1) call tlnon
c
c......End of sequence
c
          call endseq (TOOLFL(10))
c
c......Add tool time to machining time
c
          call addtim (TLCTIM)
c
c......Output Simulation record
c
          call simsta (3,msg,ierr)
      endif
      if (MXCL .gt. inc) go to 9800
c
c...End of routine
c
 8000 MXCL   = isav
      return
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
c...Not valid for this machine
c
 9700 call psterr (2,'NOTVALID',' ',inc)
      go to 8000
c
c...Too many parameters
c
 9800 call psterr (1,'INVPSYNW',' ',inc+1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  unload
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 UNLOAD/TOOL(,tn)
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine unload
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MACHTP,KPOSMP(1201)), (ITP   ,KPOSMP(1801))
      equivalence (TOOLFL,KPOSMP(1804)), (TLCCD ,KPOSMP(1839))
      equivalence (LSTGRP,KPOSMP(1871)), (LSTTSP,KPOSMP(1872))
c
      integer*4 MXCL,IPSTWD(50),ITP,TOOLFL(20),TLCCD(20),LSTGRP,LSTTSP,
     1          MACHTP
c
      equivalence (PSTWD ,POSMAP(0441))
      equivalence (CTOFRG,POSMAP(2401))
      equivalence (TLCTIM,POSMAP(3962)), (TLCVL ,POSMAP(3963))
      equivalence (LSTTN ,POSMAP(3990)), (TLOFRG,POSMAP(3991))
      equivalence (MAXTN ,POSMAP(3995))
c
      real*8 PSTWD(50),CTOFRG,TLCTIM,TLCVL(20),LSTTN,TLOFRG,MAXTN
c
      integer*4 inc,ipt
c
      real*8 tnum
c
      if (MACHTP .eq. 2 .or. TOOLFL(1) .ne. 1) go to 9700
      inc    = 0
c
c...UNLOAD/TOOL
c
      if (MXCL .eq. 0) go to 9000
      inc    = 1
      if (IPSTWD(inc) .eq. 0) go to 9500
      if (IPSTWD(inc) .ne. 617) go to 9100
c
c......UNLOAD/TOOL,tn
c
      inc    = inc    + 1
      tnum   = LSTTN
      if (inc .le. MXCL) then
          if (IPSTWD(inc) .ne. 0) go to 9300
          if (PSTWD(inc) .lt. 0 .or. PSTWD(inc) .gt. MAXTN)
     1            go to 9400
          tnum   = PSTWD(inc)
      endif
c
c......Unload the tool
c
      LSTTN  = 0
c
c.........Turn off tool length compensation
c
      if (TOOLFL(8) .eq. 1) call tlnoff
c
c.........Output unload code
c
      ipt    = 7
      if (LSTGRP .eq. 2) ipt = 9
      if (LSTTSP .eq. 2) ipt = ipt + 1
      call codout (TLCCD(ipt),TLCVL(ipt))
c
c.........Output tool code
c
      if (TOOLFL(15) .eq. 1) call tlncod (tnum,tnum,TLOFRG,CTOFRG,1)
      call clrbuf
c
c.........End of sequence
c
      call endseq (TOOLFL(10))
c
c......Add tool time to machining time
c
      call addtim (TLCTIM)
      if (MXCL .gt. inc) go to 9200
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
c...Not valid for this machine
c
 9700 call psterr (2,'NOTVALID',' ',inc)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  skewtl (knum)
c
c   FUNCTION:  This routine replaces the tool length specified in LOADTL
c              command by length specified in .maf file when tool number
c              matches.
c
c
c   INPUT:  knum     I*4  D1  -  Tool array pointer to check and replace
c                                length if exists in skewing tool array.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine skewtl (knum)
c
      include 'post.inc'
c
      integer*4 knum
c
      equivalence (MXGTOL,KPOSMP(4001))
c
      integer*4 MXGTOL
c
      equivalence (GTLNO ,POSMAP(4051)), (GTLEN ,POSMAP(4171))
      equivalence (TL    ,POSMAP(3601)), (TLNO  ,POSMAP(3841))
c
      real*8 GTLNO(120),GTLEN(120),TL(120),TLNO(120)
c
      integer*4 i
c
c
c...See if skewing tools are used
c
      if (MXGTOL .eq. 0) go to 8000
c
c...Check for previously loaded tool
c
      do 105 i=1,MXGTOL,1
          if (TLNO(knum) .eq. GTLNO(i)) go to 7200
  105 continue
      go to 8000
c
c...Replace tool length
c
 7200 TL(knum) = GTLEN(i)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   FUNCTION:  ltlcmd
c
c   FUNCTION:  This funcion determines if the active post-processsor
c              command is a LOADTL statement.
c
c   INPUT:  none
c
c   OUTPUT: .TRUE. if it is a LOADTL command, otherwise .FALSE.
c
c***********************************************************************
c
      logical function ltlcmd ()
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (TOOLFL,KPOSMP(1804))
c
      integer*4 MXCL,IPSTWD(50),ITYPE,ISUBT,TOOLFL(20)
c
c...Determine if LOADTL command
c
      ltlcmd = ITYPE .eq. 2000 .and.
     1         ((ISUBT .eq. 1055 .and. MXCL .ge. 1) .or.
     2          (TOOLFL(18) .eq. 1 .and. ISUBT .eq. 1075 .and.
     3           MXCL .ge. 2 .and. IPSTWD(1) .eq. 617))
      return
      end
