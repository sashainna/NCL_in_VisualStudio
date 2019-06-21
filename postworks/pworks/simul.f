c
c***********************************************************************
c
c   FILE NAME: simul.for
c   CONTAINS:
c               simopn  simini  simhdr  simisn  simsta  simatr  simofs
c               simcir  simcut  simdis  simmot  simmow  simpin  simstk
c               simprf  simerr  simwrt  simtyp  getpiv  simln   rotmx
c               vctmmx  simnum
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        simul.f , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:56:40
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  simopn (cmsg,kerr)
c
c   FUNCTION:  This routine opens the Machine Simulation file and writes
c              the header record.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simopn (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 irecl,nc,ncf,strlen1
c
      character*20 att(4)
      character*(MAX_PATH) ldev,fnam,sdev
      character*(MAX_FILE) lfil,lext,sfil,sext
c
c...Get Machine number extension
c
      nc     = strlen1(PSTNAM(PREPT))
c
c...Break apart input filename
c
      call fbreak (LCMPFI,ldev,lfil,lext)
c
c...Close Print File
c
      call clsfil (LUNSC3)
c
c...Open simulation file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 132
      call fbreak (LOPFL(10),sdev,sfil,sext)
      if (sfil .eq. ' ') sfil = lfil
      if (sdev .eq. ' ') sdev = ldev
      if (sext .eq. ' ') sext = '.sim'
      if (IOPFL(6) .eq. 1) then
          ncf    = strlen1(sfil)
          fnam   = sfil(1:ncf) // '_' // PSTNAM(PREPT)(1:nc)
          sfil   = fnam
      endif
      call fparse (sfil,fnam,sdev,sext)
      call opnfil (LUNSC3,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Write Simulation Header
c
c     call simhdr (cmsg,kerr)
c     if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simini
c
c   FUNCTION:  This routine writes the Machine Simulation Header Record.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine simini
c
      include 'post.inc'
c
      equivalence (SIMOFR,KPOSMP(0122)), (SIMACT,KPOSMP(0174))
      equivalence (ICYCFL,KPOSMP(0181)), (HLDBCK,KPOSMP(1223))
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (NOPIVS,KPOSMP(1282)), (NOTABS,KPOSMP(1283))
      equivalence (ICIRSW,KPOSMP(1391))
      equivalence (NOTOOL,KPOSMP(1802)), (ICYLFL,KPOSMP(1901))
      equivalence (ICRHEL,KPOSMP(4250))
c
      integer*4 ICYCFL(30),HLDBCK,NOPIVS,NOTABS,ICRHEL(10),NOTOOL,
     1          SIMACT,ICIRSW,ICYLFL(20),IRTNUM,SIMOFR(8)
c
      integer*4 i
c
c...Initialize variables for
c...Machine Simulation file output
c
      ICIRSW = 2
      ICRHEL(1) = 2
      ICYCFL(1) = 2
      ICYCFL(21) = 2
      ICYLFL(1) = 2
      ICYLFL(20) = 2
cc      HLDBCK = 2
      NOTABS = 1
      NOPIVS = 1
      NOTOOL = 1
      SIMACT = 1
c
      do 100 i=1,8,1
          SIMOFR(i) = 0
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simhdr (cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation Header Record.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simhdr (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308)), (MOTREG,KPOSMP(0381))
      equivalence (MACHTP,KPOSMP(1201)), (NUMLIN,KPOSMP(1202))
      equivalence (LNCALC,KPOSMP(1208)), (IRTNUM,KPOSMP(1243))
      equivalence (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506)), (REGBNC,KPOSMP(2001))
c
      integer*4 MCHOPT(20),NUMLIN(3),IRTYPE(20),MACHTP,LNCALC(3),
     1          IRTWRK(20),REGBNC(MAXFMT),MOTREG(24),IRTNUM
c
      equivalence (LNDIST,POSMAP(1251)), (TABORG,POSMAP(5374))
c
      real*8 LNDIST(3),TABORG(3,20)
c
      equivalence (LPARTN,CPOSMP(0067)), (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
      character*66 LPARTN
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,strlen1,i,inc,j,irt
c
      real*8 rnum,rtab(3)
      character*80 lnum
c
c...Write Post-Processor info
c
      MFKNC  = 0
      call simwrt ('HEADER',6,cmsg,kerr)
c
c......PostProcessor name (PostWorks)
c
      nc     = strlen1(PGMNAM)
      call simwrt (PGMNAM,nc,cmsg,kerr)
c
c......Revision Date
c
      nc     = strlen1(REVDAT)
      call simwrt (REVDAT,nc,cmsg,kerr)
c
c......Machine Number
c
      nc     = strlen1(PSTNAM(PREPT))
      call simwrt (PSTNAM(PREPT),nc,cmsg,kerr)
c
c......Run Date & Time
c
      call simwrt (LDATE,11,cmsg,kerr)
      call simwrt (LTIME,8,cmsg,kerr)
c
c......Input clfile
c
      nc     = strlen1(CLNAME)
      if (nc .ne. 0) then
          call shfile (CLNAME,lnum,80)
          nc     = strlen1(lnum)
          call simwrt (lnum,nc,cmsg,kerr)
      else
          call shfile (LCMPFI,lnum,80)
          nc     = strlen1(lnum)
          call simwrt (lnum,nc,cmsg,kerr)
      endif
c
c......Clfile Creation Date
c
      nc     = strlen1(CLDATE)
      call simwrt (CLDATE,nc,cmsg,kerr)
      nc     = strlen1(CLTIME)
      call simwrt (CLTIME,nc,cmsg,kerr)
c
c......Units
c
      call itoc (MCHOPT(2),lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c......End of Post Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...Write Machine Info
c
      call simwrt ('MACHINE',7,cmsg,kerr)
c
c...Machine Type
c
      call itoc (MACHTP,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c......Linear axes setup (# of, Register, Calc type, Dist between)
c
      inc    = 1
      do 500 i=1,3,1
          call itoc (NUMLIN(i),lnum,nc,0)
          call simwrt (REGST(MOTREG(inc)),REGBNC(MOTREG(inc)),cmsg,kerr)
          inc    = inc    + 2
          call simwrt (REGST(MOTREG(inc)),REGBNC(MOTREG(inc)),cmsg,kerr)
          inc    = inc    + 2
          call simwrt (lnum,nc,cmsg,kerr)
          call itoc (LNCALC(i),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
          call dpoint (LNDIST(i),rnum,6)
          call rtoc (rnum,lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
  500 continue
c
c......Rotary axes setup (# of, Register, Type, Rot axes, Origin)
c......This routine is called before 'rscini'
c
      irt = IRTNUM
      if (MACHTP .eq. 3) irt = irt + 1
      call itoc (irt,lnum,nc,0)
      if (MACHTP .eq. 3) irt = 4
      call simwrt (lnum,nc,cmsg,kerr)
      inc    = 14
      do 600 i=1,irt,1
          if (i .le. IRTNUM .or. (i .eq. 4 .and. MACHTP .eq. 3)) then
              call simwrt (REGST(MOTREG(inc)),REGBNC(MOTREG(inc)),cmsg,
     1                     kerr)
              call itoc (IRTYPE(i),lnum,nc,0)
              call simwrt (lnum,nc,cmsg,kerr)
              if (IRTWRK(i) .eq. 1) then
                  call simwrt ('1.,0.,0.',8,cmsg,kerr)
              else if (IRTWRK(i) .eq. 2) then
                  call simwrt ('0.,1.,0.',8,cmsg,kerr)
              else
                  call simwrt ('0.,0.,1.',8,cmsg,kerr)
              endif
              do 605 j=1,3
                 call dpoint (TABORG(j,i),rtab(j),5)
  605         continue
              call getr8s (rtab,3,lnum,nc)
              call simwrt (lnum,nc,cmsg,kerr)
          endif
          inc    = inc    + 3
  600 continue
c
c......End of Machine Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...Write PARTNO
c
      call simwrt ('PARTNO',6,cmsg,kerr)
      call simwrt ('0',1,cmsg,kerr)
      call simwrt ('0',1,cmsg,kerr)
      nc     = strlen1(LPARTN)
      call simwrt (LPARTN,nc,cmsg,kerr)
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simisn (gclw,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation ISN Record.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simisn (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001))
c
      integer*4 ISN
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 JCLDAT(200)
c
      equivalence (RCLDAT,JCLDAT)
c
      integer*4 nc,i
c
      character*256 lnum
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('ISN',3,cmsg,kerr)
c
c...Write out isn value
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Write out number of values in record
c
      call itoc (JCLDAT(1),lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Write out ISN values
c
      do 100 i=2,JCLDAT(1)+1,1
          call itoc (JCLDAT(i),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
  100 continue
c
c...End of ISN Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simsta (kstop,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation Status Record.
c
c   INPUT:  kstop   I*4  D1  -  Type of STOP record to write.  0 = No
c                               stop, 1 = STOP, 2 = OPSTOP, 3 = LOADTL.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simsta (kstop,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (NCUT  ,KPOSMP(0062))
      equivalence (NCUTDS,KPOSMP(0063)), (ICUTYP,KPOSMP(0064))
      equivalence (NSHANK,KPOSMP(0067))
      equivalence (ITP   ,KPOSMP(1801)), (SPNFCD,KPOSMP(3118))
      equivalence (ICOLSW,KPOSMP(3146)), (ICUTDO,KPOSMP(3301))
c
      integer*4 ISN,ICOLSW,ICUTDO(15),ITP,NCUT,NCUTDS,SPNFCD(3),
     1          NSHANK,ICUTYP(3)
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (CUTTER,POSMAP(0744)), (CUDISP,POSMAP(0759))
      equivalence (CUTOFS,POSMAP(0766)), (SEQCUR,POSMAP(1183))
      equivalence (RPM   ,POSMAP(3307)), (TL    ,POSMAP(3601))
      equivalence (TLNO  ,POSMAP(3841))
c
      real*8 CUTTER(7),TL(120),TLNO(120),SEQCUR,CUDISP(7),RPM,
     -       METCNV,CUTOFS(4,3)
c
      equivalence (CUTSYM,CPOSMP(3009))
c
      character*80 CUTSYM(3)
c
      integer*4 kstop,kerr
c
      character*(*) cmsg
c
      integer*4 nc,i,strlen1,inum
c
      real*8 rtb(10),rnum
c
      character*256 lnum
c
c...For later versions of NCL
c...Create new simulation file
c
      if (IOPFL(10) .ne. 3) go to 8000
      if (NCLVER .gt. 9.549) then
          call simatr (kstop,cmsg,kerr)
          go to 8000
      endif
c
c...Initialize routine
c
      MFKNC = 0
      call simwrt ('STATUS',6,cmsg,kerr)
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Stop style
c
      call itoc (kstop,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...CUTTER
c
      call itoc (NCUT,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
      if (NCUT .gt. 0) then
          do 115 i=1,NCUT
             call dpoint (CUTTER(i),rtb(i),4)
  115     continue
          call getr8s (rtb,NCUT,lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
      endif
c
c...Display CUTTER
c
      call itoc (NCUTDS,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
      if (NCUTDS .gt. 0) then
          do 215 i=1,NCUTDS
             call dpoint (METCNV*CUDISP(i),rtb(i),4)
  215     continue
          call getr8s (rtb,NCUTDS,lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
      endif
c
c...RPM
c
      call dpoint (RPM,rnum,3)
      call rtoc (rnum,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...COOLNT
c
      call itoc (ICOLSW,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...CUTCOM
c
      if (ICUTDO(1) .eq. 0) then
          call itoc (ICUTDO(1),lnum,nc,0)
      else
          call itoc (ICUTDO(3),lnum,nc,0)
      endif
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Tool Number
c
      call rtoc (TLNO(ITP),lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Tool Length
c
      call dpoint (TL(ITP),rnum,4)
      call rtoc (rnum,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Cutter display flag
c
      call itoc (ICUTYP(1),lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Cutter symbol
c
      nc     = strlen1(CUTSYM(1))
      call simwrt (CUTSYM(1),nc,cmsg,kerr)
c
c...Symbol offset
c
      do 400 i=1,4,1
         call dpoint (METCNV*CUTOFS(i,1),rtb(i),4)
  400 continue
      call getr8s (rtb,4,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Cutter shank
c
      call itoc (NSHANK,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
      if (NSHANK .gt. 0) then
          do 500 i=1,NSHANK,1
              call dpoint (METCNV*CUTOFS(i,2),rtb(i),4)
  500     continue
          call getr8s (rtb,NSHANK,lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
      endif
c
c...Machine mode
c
      call simtyp (inum)
      call itoc (inum,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...End of Status Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simatr (kstop,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation ATTRIB Record.
c
c   INPUT:  kstop   I*4  D1  -  Type of STOP record to write.  0 = No
c                               stop, 1 = STOP, 2 = OPSTOP, 3 = LOADTL.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simatr (kstop,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (SIMOFR,KPOSMP(0122))
      equivalence (ITP   ,KPOSMP(1801)), (SPNFCD,KPOSMP(3118))
      equivalence (ICOLSW,KPOSMP(3146)), (ICUTDO,KPOSMP(3301))
c
      integer*4 ISN,ICOLSW,ICUTDO(15),ITP,SPNFCD(3),SIMOFR(4)
c
      equivalence (SEQCUR,POSMAP(1183)), (RPM   ,POSMAP(3307))
      equivalence (TL    ,POSMAP(3601)), (TLNO  ,POSMAP(3841))
c
      real*8 TL(120),TLNO(120),SEQCUR,RPM
c
      integer*4 kstop,kerr
c
      character*(*) cmsg
c
      integer*4 nc,inum,i
c
      real*8 rnum
c
      character*256 lnum
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('ATTRIB',6,cmsg,kerr)
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Stop style
c
      call itoc (kstop,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...RPM
c
      call dpoint (RPM,rnum,3)
      call rtoc (rnum,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...COOLNT
c
      call itoc (ICOLSW,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...CUTCOM
c
      if (ICUTDO(1) .eq. 0) then
          call itoc (ICUTDO(1),lnum,nc,0)
      else
          call itoc (ICUTDO(3),lnum,nc,0)
      endif
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Tool Number
c
      call rtoc (TLNO(ITP),lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Tool Length
c
      call dpoint (TL(ITP),rnum,4)
		call rtoc (rnum,lnum,nc)
		call simwrt (lnum,nc,cmsg,kerr)
c
c...Machine mode
c
      call simtyp (inum)
      call itoc (inum,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Fixture offsets
c
      do 100 i=1,4,1
          call itoc (SIMOFR(i),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
  100 continue
c
c...End of Status Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simofs (kcmode,kcreg,ktmode,ktreg,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation OFFSET Record.
c
c   INPUT:  kcmode  I*4  D1  -  0 = Fixture offset off, 1 = Plus,
c                               2 = Minus. -1 = Don't change.
c
c           kcreg   I*4  D1  -  Register for Fixture offsets.
c                               -1 = Don't change.
c
c           ktmode  I*4  D1  -  0 = Tool offset off, 1 = Plus,
c                               2 = Minus. -1 = Don't change.
c
c           ktreg   I*4  D1  -  Register for Tool offsets.
c                               -1 = Don't change.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simofs (kcmode,kcreg,ktmode,ktreg,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (SIMOFR,KPOSMP(0122))
c
      integer*4 ISN,SIMOFR(8)
c
      equivalence (SEQCUR,POSMAP(1183))
c
      real*8 SEQCUR
c
      integer*4 kerr,kcmode,kcreg,ktmode,ktreg
c
      character*(*) cmsg
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('OFFSET',6,cmsg,kerr)
c
c...Store Offset modes and registers
c
      if (kcmode .ne. -1) SIMOFR(1) = kcmode
      if (kcreg .ne. -1) SIMOFR(2) = kcreg
      if (ktmode .ne. -1) SIMOFR(3) = ktmode
      if (ktreg .ne. -1) SIMOFR(4) = ktreg
c
c...Output offsets
c
      call simatr (0,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simcir (cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation CIRCLE Record.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simcir (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001))
c
      integer*4 ISN
c
      equivalence (CIRBUF,POSMAP(0731)), (SEQCUR,POSMAP(1183))
c
      real*8 CIRBUF(7),SEQCUR
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,i
c
      real*8 rtb(7)
c
      character*256 lnum
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('CIRCLE',6,cmsg,kerr)
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Circle
c
      do 115 i=1,7,1
          call dpoint (CIRBUF(i),rtb(i),6)
  115 continue
      call getr8s (rtb,7,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...End of Circle Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simcut (cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation CUTTER Record.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simcut (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (NCUT  ,KPOSMP(0062))
      equivalence (NCUTDS,KPOSMP(0063)), (ICUTYP,KPOSMP(0064))
      equivalence (NSHANK,KPOSMP(0067)), (ICUCHG,KPOSMP(0068))
c
      integer*4 ISN,NCUT,NCUTDS,NSHANK,ICUTYP(3),ICUCHG
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (CUTTER,POSMAP(0744)), (CUDISP,POSMAP(0759))
      equivalence (CUTOFS,POSMAP(0766)), (SEQCUR,POSMAP(1183))
c
      real*8 CUTTER(7),SEQCUR,CUDISP(7),METCNV,CUTOFS(4,3)
c
      equivalence (CUTSYM,CPOSMP(3009))
c
      character*80 CUTSYM(3)
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,i,j,strlen1,index,inc
c
      real*8 rtb(12)
c
      character*256 lnum
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('CUTTER',6,cmsg,kerr)
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...CUTTER
c
      call itoc (NCUT,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
      if (NCUT .gt. 0) then
          do 115 i=1,NCUT
             call dpoint (CUTTER(i),rtb(i),4)
  115     continue
          call getr8s (rtb,NCUT,lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
      endif
c
c...Display CUTTER
c
      call itoc (NCUTDS,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
      if (NCUTDS .gt. 0) then
          do 215 i=1,NCUTDS
             call dpoint (METCNV*CUDISP(i),rtb(i),4)
  215     continue
          call getr8s (rtb,NCUTDS,lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
      endif
c
c...Cutter display flags
c
      do 300 i=1,3,1
          call itoc (ICUTYP(i),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
  300 continue
c
c...Cutter symbol
c
      do 310 i=1,3,1
          inc    = index(CUTSYM(i),',')
          if (inc .ne. 0) CUTSYM(i)(inc:inc) = '@'
          nc     = strlen1(CUTSYM(i))
          call simwrt (CUTSYM(i),nc,cmsg,kerr)
  310 continue
c
c...Symbol offset
c
      inc    = 0
      do 410 i=1,3,1
          do 400 j=1,4,1
              inc    = inc    + 1
              call dpoint (METCNV*CUTOFS(j,i),rtb(inc),4)
  400     continue
  410 continue
      call getr8s (rtb,inc,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Cutter shank
c
      call itoc (NSHANK,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...End of Cutter Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 ICUCHG = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simdis (cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation PPRINT Record.
c
c   INPUT:  cdat    C*n  D1  -  Text of PPRINT statement.
c
c           knc     I*4  D1  -  Number of chars in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simdis (cdat,knc,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001))
c
      integer*4 ISN
c
      equivalence (SEQCUR,POSMAP(1183))
c
      real*8 SEQCUR
c
      integer*4 kerr,knc
c
      character*(*) cmsg,cdat
c
      integer*4 nc
c
      character*256 lnum
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('PPRINT',6,cmsg,kerr)
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...PPRINT data
c
      call simwrt (cdat,knc,cmsg,kerr)
c
c...End of PPRINT Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simmot (ktype,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation Motion Record.
c
c   INPUT:  ktype   I*4  D1  -  Type of Motion record to write.
c                               1 = FROM, 2 = GOTO, 3 = CIRCLE,
c                               4 = CYCLE, 5 = PRESET.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simmot (ktype,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICUCHG,KPOSMP(0068))
      equivalence (IOPSKP,KPOSMP(0092))
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (MACHTP,KPOSMP(1201)), (IRTNUM,KPOSMP(1243))
      equivalence (MXTRAN,KPOSMP(4003)), (MODRAT,KPOSMP(4033))
c
      integer*4 ISN,IOPSKP,MCHOPT(20),IRTNUM,MODRAT,MACHTP,
     1          MXTRAN,ICUCHG
c
      equivalence (MCHNUM,POSMAP(1287)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (STONUM,POSMAP(1387))
      equivalence (AXSSTO,POSMAP(1425))
      equivalence (MOVDIS,POSMAP(1570)), (AXSDIS,POSMAP(1574))
      equivalence (MOVTIM,POSMAP(3546)), (TRAMAT,POSMAP(4013))
c
      real*8 MCHNUM(3,4),TLVEC(3),AXSOUT(10),MOVTIM,STONUM(3,4),
     1       AXSDIS(10),AXSSTO(10),MOVDIS(4),TRAMAT(12)
c
      integer*4 ktype,kerr
c
      character*(*) cmsg
c
      integer*4 i,j,ntim,is
c
      real*8 timm,ratio(10),axs(10),rlin(6),rmch(3,4),rvec(3),rnum,
     1       rrot(20,2),smtol,tmch(3,4)
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      timm   = MOVTIM
      is = 2
      if (MACHTP .eq. 2 .or. MACHTP .eq. 4 .or. (MXTRAN .eq. 1 .and.
     1    MCHOPT(8) .eq. 0)) is = 1
c
c...Output CUTTER record if it has changed
c
      if (ICUCHG .eq. 1 .and. NCLVER .gt. 9.549) call simcut (cmsg,kerr)
c
c...Determine if we need to output multiple moves
c...based on the rotary axes movement
c
      if (IRTNUM .ge. 1 .and. ktype .eq. 2 .and. MOVDIS(4) .ne. 0. .and.
     1    MCHOPT(6) .ne. 2) then
          smtol = .005
          if (MCHOPT(2) .eq. 2) smtol = .125
          call simnum(smtol,ntim)
c
c......We need to break up the move
c.........Determine ratio factors
c
          if (ntim .gt. 1) then
              do 300 i=1,10,1
                  ratio(i) = (AXSOUT(i)-AXSSTO(i)) / ntim
  300         continue
c
c.........Output intermediate points
c
              timm   = MOVTIM / ntim
              if (MCHOPT(6) .eq. 3)  then
                  call alladr (AXSSTO,rlin,tmch,rrot,rvec,5,2)
                  call alladr (AXSOUT,rlin,rmch,rrot,rvec,5,2)
                  do 350 i=1,3,1
                      ratio(i) = (rmch(i,2)-tmch(i,2)) / ntim
  350             continue
              endif
              do 500 i=1,ntim-1,1
c
c.........Normal axes simulation
c
                  do 400 j=1,10,1
                      axs(j) = AXSSTO(j) + ratio(j) * i
  400             continue
                  call alladr (axs,rlin,rmch,rrot,rvec,5,1)
c
c.........Machine accepts tool tip programming
c.........and user wants the tool tip to move
c.........in a straight line
c
                  if (MCHOPT(6) .eq. 3) then
                      do 420 j=1,3,1
                          rmch(j,2) = tmch(j,2) + ratio(j) * i
  420                 continue
                      call alladj (rmch,rlin,axs,rrot,rvec,2,5)
                      call alladr (axs,rlin,rmch,rrot,rvec,2,1)
                  endif
c
                  if (MACHTP .eq. 2 .or. MACHTP .eq. 4) then
                      rnum = rvec(1)
                      rvec(1) = rvec(3)
                      rvec(3) = rvec(2)
                      rvec(2) = rnum
                  endif
                  call simmow (ktype,rmch(1,is),rvec,axs,timm,cmsg,kerr)
  500         continue
          endif
      endif
c
c...Write out final position
c
      if (MACHTP .eq. 2 .or. MACHTP .eq. 4) then
          rvec(1) = TLVEC(3)
          rvec(2) = TLVEC(1)
          rvec(3) = TLVEC(2)
      else
          call copyn (TLVEC,rvec,3)
      endif
      if (MXTRAN .eq. 1 .and. MCHOPT(8) .eq. 0)
     1    call matptr (rvec,rvec,TRAMAT,2)
      call simmow (ktype,MCHNUM(1,is),rvec,AXSOUT,timm,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simmow (ktype,gmch,gvec,gaxs,gtim,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation Motion Record.
c
c   INPUT:  ktype   I*4  D1  -  Type of Motion record to write.
c                               1 = FROM, 2 = GOTO, 3 = CIRCLE,
c                               4 = CYCLE, 5 = PRESET.
c
c           gmch    R*8  D3     Tool end point.
c
c           gvec    R*8  D3  -  Tool axis vector.
c
c           gaxs    R*8  D10 -  Output linear and rotary axes.
c
c           gtim    R*8  D1  -  Machining time for this move.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simmow (ktype,gmch,gvec,gaxs,gtim,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (IOPSKP,KPOSMP(0092))
      equivalence (MOTREG,KPOSMP(0381)), (XFMFL ,KPOSMP(0969))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (NOPIVS,KPOSMP(1282)), (NOTABS,KPOSMP(1283))
      equivalence (IRTDEF,KPOSMP(1485))
      equivalence (IRTWRK,KPOSMP(1506))
      equivalence (NOTOOL,KPOSMP(1802)), (FEDCD ,KPOSMP(3162))
      equivalence (IRAP  ,KPOSMP(3199)), (LTMODE,KPOSMP(4125))
c
      integer*4 ISN,IOPSKP,IRAP,FEDCD(6),MOTREG(24),LTMODE,IRTDEF,
     1          IRTWRK(20),MACHTP,XFMFL(20),NOPIVS,NOTABS,NOTOOL
c
      equivalence (SEQCUR,POSMAP(1183)), (PPTOLR,POSMAP(1274))
      equivalence (MOVDIS,POSMAP(1570))
      equivalence (PFEED ,POSMAP(3540)), (OFEED ,POSMAP(3560))
      equivalence (BLDVEC,POSMAP(3589))
c
      real*8 OFEED(6),SEQCUR,PPTOLR(3),PFEED(4),MOVDIS(4),BLDVEC(3)
c
      integer*4 ktype,kerr
c
      real*8 gmch(3),gvec(3),gaxs(10),gtim
c
      character*(*) cmsg
c
      integer*4 nc,i,nn,iax(10),iay(10),isav(3),ixfl
c
      real*8 rnum,rtb(10),tvec(3),rmch(3,4),rlin(6),rrot(20,2),raxs(10)
c
      character*256 lnum
c
      data iax /1, 3, 5, 7, 9, 11, 13, 16, 19, 22/
      data iay /1, 3, 1, 3, 9, 11, 13, 16, 19, 22/
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('MOTION',6,cmsg,kerr)
c
c...Axes must be adjusted for rotaries
c...When Pted is running
c
      if ((NOTABS .eq. 2 .or. NOPIVS .eq. 2 .or. NOTOOL .eq. 2) .and.
     1    IPROGM .eq. 2) then
          call alladr (gaxs,rlin,rmch,rrot,tvec,5,1)
          isav(1) = NOTABS
          isav(2) = NOPIVS
          isav(3) = NOTOOL
          ixfl   = XFMFL(4)
          NOTABS = 1
          NOPIVS = 1
          NOTOOL = 1
          XFMFL(4) = 0
          call alladj (rmch,rlin,raxs,rrot,2,5)
          NOTABS = isav(1)
          NOPIVS = isav(2)
          NOTOOL = isav(3)
          XFMFL(4) = ixfl
      else
          call copyn (gaxs,raxs,10)
      endif
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Optional Skip
c
      call itoc (IOPSKP,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Motion type
c
      call itoc (ktype,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Input coordinates
c
      do 115 i=1,3
         call dpoint (gmch(i),rtb(i),6)
         call dpoint (gvec(i),rtb(3+i),6)
  115 continue
      call getr8s (rtb,3,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
      call getr8s (rtb(4),3,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Machine coordinates
c
      do 215 i=1,10
         call codint (MOTREG(iax(i)),raxs(i),rtb(i),nn)
  215 continue
      if (MACHTP .eq. 3 .and. IRTDEF .lt. 3) then
          rtb(IRTDEF+7) = rtb(10)
          rtb(10) = 0.
      endif
      call getr8s (rtb,10,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Feed Rate
c
      rnum   = OFEED(2)
      if (IRAP .eq. 1) then
          rnum = 0.
      else
         if (rnum*gtim .le. PPTOLR(1) .and.
     1       MOVDIS(2) .le. PPTOLR(1)) rnum = PFEED(1)
      end if
      call dpoint (rnum,rnum,3)
      call rtoc (rnum,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Machining Time
c
      call dpoint (gtim,rnum,4)
      call rtoc (rnum,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Calculate blade direction
c...for Ultrasonic machines
c
      if (MACHTP .eq. 3) then
          tvec(1) = BLDVEC(1)
          tvec(2) = BLDVEC(2)
          tvec(3) = BLDVEC(3)
          call vecadj (tvec,tvec,raxs(10),3)
          do 300 i=IRTDEF+6,7,-1
              call vecadj (tvec,tvec,raxs(i),IRTWRK(i-6))
  300     continue
          do 310 i=1,3
             call dpoint (tvec(i),rtb(i),6)
  310     continue
          call getr8s (rtb,3,lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
      endif
c
c...End of Motion Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simsmd (ktype,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation Record for
c              the Stringer drilling head.
c
c   INPUT:  ktype   I*4  D1  -  Type of Motion record to write.
c                               1 = FROM, 2 = GOTO, 3 = CIRCLE,
c                               4 = CYCLE, 5 = PRESET.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simsmd (ktype,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICUCHG,KPOSMP(0068))
      equivalence (IOPSKP,KPOSMP(0092)), (SGMREG,KPOSMP(0951))
      equivalence (SGMACT,KPOSMP(0996)), (SGMCOD,KPOSMP(1018))
      equivalence (IRAP  ,KPOSMP(3199))
c
      integer*4 ISN,IOPSKP,SGMREG(6,3),ICUCHG,SGMACT(2),SGMCOD(10),
     1          IRAP
c
      equivalence (REGSTO,POSMAP(1032)), (SEQCUR,POSMAP(1183))
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (PFEED ,POSMAP(3540)), (MOVTIM,POSMAP(3546))
      equivalence (SGMOUT,POSMAP(5454))
c
      real*8 MCHNUM(3,4),TLVEC(3),MOVTIM,SGMOUT(6,3),PFEED(4),SEQCUR,
     1       REGSTO(92)
c
      integer*4 ktype,kerr
c
      character*(*) cmsg
c
      integer*4 nc,i,j,n,nn,inc
c
      real*8 rnum,rtb(12)
c
      character*256 lnum
c
c...Output CUTTER record if it has changed
c
      if (IOPFL(10) .ne. 3) go to 8000
      if (ICUCHG .eq. 1 .and. NCLVER .gt. 9.549) call simcut (cmsg,kerr)
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('HEDMOT',6,cmsg,kerr)
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Optional Skip
c
      call itoc (IOPSKP,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Motion type
c
      call itoc (ktype,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Active heads
c
      call itoc (SGMACT(1),lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
      call itoc (SGMACT(2),lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Input coordinates
c
      do 115 i=1,3
         call dpoint (MCHNUM(i,2),rtb(i),6)
         call dpoint (TLVEC(i),rtb(3+i),6)
  115 continue
      call getr8s (rtb,3,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
      call getr8s (rtb(4),3,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Machine coordinates
c
      n      = 6
      inc    = 0
      do 220 i=1,3,1
         do 200 j=1,n,1
            inc    = inc    + 1
            call codint (SGMREG(j,i),SGMOUT(j,i),rtb(inc),nn)
  200    continue
         n = 3
  220 continue
      call getr8s (rtb,12,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Q-axis
c
      rnum   = REGSTO(SGMCOD(1))
      call getr8s (rnum,1,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Feed Rate
c
      if (IRAP .eq. 1) then
          rnum = 0.
      else
          rnum = PFEED(1)
      endif
      call dpoint (rnum,rnum,3)
      call rtoc (rnum,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Machining Time
c
      call dpoint (MOVTIM,rnum,4)
      call rtoc (rnum,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...End of Motion Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simpin (cmsg,kerr)
c
c   FUNCTION:  This routine writes a TOOLPN record to the simulation file.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simpin (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (IPSTWD,KPOSMP(0006))
c
      integer*4 ISN,MXCL,ISUBT,IPSTWD(50)
c
      equivalence (PSTWD,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,i
c
      character*256 lnum
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
c
c...Check command syntax
c
      if (MXCL .ne. 9) go to 9000
      do 100 i=1,9,1
          if (IPSTWD(i) .ne. 0) go to 9000
  100 continue
      i      = 4
      if (PSTWD(4) .eq. 0. .and. PSTWD(5) .eq. 0. .and.
     1    PSTWD(6) .eq. 0.) go to 9100
      i      = 7
      if (PSTWD(7) .eq. 0. .and. PSTWD(8) .eq. 0. .and.
     1    PSTWD(9) .eq. 0.) go to 9100
c
c...Write TOOLPN record
c
      call getvwd (ISUBT,lnum,nc,1,PSTWRD,PSTWVL,NPSTWD)
      call simwrt (lnum,nc,cmsg,kerr)
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
      call getr8s (PSTWD,9,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
      call simwrt ('#EOL#',5,cmsg,kerr)
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
c...Zero vector encountered
c
 9100 call psterr (2,'INPRNG',' ',i)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simstk (kstop,cmsg,kerr)
c
c   FUNCTION:  This routine writes a STOCK or FIXTUR record to
c              the simulation file.
c
c   INPUT:  kstop   I*4  D1  -  Type of STOP record to write.  0 = No
c                               stop, 1 = STOP, 2 = OPSTOP, 3 = LOADTL.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simstk (kstop,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
c
      integer*4 ISN,ITYPE,ISUBT,MXCL
c
      integer*4 kstop,kerr
c
      character*(*) cmsg
c
      integer*2 ICLDAT(200)
      integer*4 JCLDAT(200)
      character*512 LCLDAT
c
      equivalence (RCLDAT,ICLDAT,JCLDAT,LCLDAT)
c
      integer*4 nc,i,n
c
      real*8 rtb(12)
c
      character*256 lnum
c
      integer*2 STOCKV,FIXTV,ATV,BOXV,CYLV,LOADV,STLV,INCHV,MMV,CLONEV,
     1          MOVEV,REMOVV,MODV,CHIPV,CONEV,SPHERV,TORUSV
c
      parameter (ATV=189)
      parameter (BOXV=340)
      parameter (CHIPV=331)
      parameter (CLONEV=576)
      parameter (CONEV=632)
      parameter (CYLV=620)
      parameter (FIXTV=898)
      parameter (INCHV=303)
      parameter (LOADV=1075)
      parameter (MMV=296)
      parameter (MODV=732)
      parameter (MOVEV=577)
      parameter (REMOVV=843)
      parameter (SPHERV=631)
      parameter (STLV=330)
      parameter (STOCKV=321)
      parameter (TORUSV=627)
c
c...Initialize routine
c
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      if (ITYPE.eq.2600) then
        call simwrt ('STOCK',5,cmsg,kerr)
      else
        call simwrt ('FIXTUR',6,cmsg,kerr)
      endif
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
      call itoc (ISUBT,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Box, cone, cylinder, sphere, torus
c
      if (ISUBT .eq. BOXV .or. ISUBT .eq. CONEV .or. ISUBT .eq. CYLV
     1    .or. ISUBT .eq. SPHERV .or. ISUBT .eq. TORUSV) then
        call itoc (JCLDAT(1),lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
        call itoc (JCLDAT(2),lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
        n = 6
        if (ISUBT.eq.CONEV) n = 9
        if (ISUBT.eq.CYLV) n = 8
        if (ISUBT.eq.SPHERV) n = 4
        if (ISUBT.eq.TORUSV) n = 8
        do i=1,n
          call dpoint (RCLDAT(i+1),rtb(i),6)
        enddo
        call getr8s (rtb,n,lnum,nc)
        call simwrt (lnum,nc,cmsg,kerr)
c
c...load
c
      else if (ISUBT.eq.LOADV) then
        call itoc (JCLDAT(1),lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
        nc   = JCLDAT(2)
        lnum = LCLDAT(9:nc+8)
        call simwrt (lnum,nc,cmsg,kerr)
c
c...STL
c
      else if (ISUBT.eq.STLV) then
        call itoc (JCLDAT(1),lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
        call itoc (JCLDAT(3),lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
        nc   = JCLDAT(2)
        lnum = LCLDAT(17:nc+16)
        call simwrt (lnum,nc,cmsg,kerr)
c
c...clone
c
      else if (ISUBT.eq.CLONEV) then
        call itoc (JCLDAT(1),lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
        call itoc (JCLDAT(2),lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
        call itoc (JCLDAT(3),lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
c
c...move
c
      else if (ISUBT.eq.MOVEV) then
        do i=1,12
          call dpoint (RCLDAT(i+1),rtb(i),6)
        enddo
        call getr8s (rtb,12,lnum,nc)
        call simwrt (lnum,nc,cmsg,kerr)
        lnum = LCLDAT(105:110)
        nc = 6
        call simwrt (lnum,nc,cmsg,kerr)
        n = ICLDAT(56)
        call itoc (n,lnum,nc,0)
        call simwrt (lnum,nc,cmsg,kerr)
        do i=1,JCLDAT(1)
          call itoc (JCLDAT(i+28),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
        enddo
c
c...remove
c
      else if (ISUBT.eq.REMOVV) then
        do i=1,JCLDAT(1)
          call itoc (JCLDAT(i+2),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
        enddo
c
c...modify
c
      else if (ISUBT.eq.MODV) then
        do i=2,5
          call itoc (JCLDAT(i),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
        enddo
        call dpoint (RCLDAT(4),rtb(1),6)
        call rtoc (rtb(1),lnum,nc)
        call simwrt (lnum,nc,cmsg,kerr)
        do i=1,JCLDAT(1)
          call itoc (JCLDAT(i+8),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
        enddo
c
c...chips
c
      else if (ISUBT.eq.CHIPV) then
        do i=1,MXCL,1
          call rtoc (RCLDAT(i),lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
        enddo
      endif
c
c...End of Stock Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simprf (cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation PROFIL Record.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simprf (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
c
      integer*4 ISN,ISUBT,MXCL,ITYPE
c
      equivalence (SEQCUR,POSMAP(1183))
c
      real*8 SEQCUR
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,i,strlen1,inum,mxc,ist
c
      character*256 lnum
c
      integer*2 ICLDAT(200)
      integer*4 JCLDAT(100)
      character*512 LCLDAT
c
      equivalence (RCLDAT,ICLDAT,JCLDAT,LCLDAT)
c
c...Initialize routine
c
      MFKNC = 0
      if (IOPFL(10) .ne. 3) go to 8000
      call simwrt ('PROFIL',6,cmsg,kerr)
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Profile symbol
c
      if (ITYPE .eq. 7110) then
          nc     = strlen1(LCLDAT(1:20))
          call simwrt (LCLDAT(1:20),nc,cmsg,kerr)
      else
          nc     = ICLDAT(1)
          call simwrt (LCLDAT(9:8+nc),nc,cmsg,kerr)
      endif
c
c...Profile flags
c
      call itoc (ISUBT,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
      if (ITYPE .eq. 7110) then
          call itoc (JCLDAT(6),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
          inum   = ICLDAT(13)
      else
          call itoc (JCLDAT(2),lnum,nc,0)
          call simwrt (lnum,nc,cmsg,kerr)
          inum   = ICLDAT(2)
      endif
      call itoc (inum,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...Profile
c
      ist    = 2 + (ICLDAT(1)+7) / 8
      if (ITYPE .eq. 7110) ist = 5
      mxc    = MXCL - ist + 1
      do 300 i=ist,MXCL,8
          inum   = mxc
          if (inum .gt. 8) inum = 8
          call getr8s (RCLDAT(i),inum,lnum,nc)
          call simwrt (lnum,nc,cmsg,kerr)
          mxc    = mxc    - inum
  300 continue
c
c...End of Profile Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simerr (cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine writes the Machine Simulation ERROR Record.
c
c   INPUT:  cdat    C*n  D1  -  Text of ERROR statement.
c
c           knc     I*4  D1  -  Number of chars in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simerr (cdat,knc,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001))
c
      integer*4 ISN
c
      equivalence (SEQCUR,POSMAP(1183))
c
      real*8 SEQCUR
c
      integer*4 kerr,knc
c
      character*(*) cmsg,cdat
c
      integer*4 nc
c
      character*256 lnum
c
c...Initialize routine
c
      kerr   = 0
      if (IOPFL(10) .ne. 3) go to 8000
      MFKNC = 0
      call simwrt ('ERROR',5,cmsg,kerr)
c
c...ISN
c
      call itoc (ISN,lnum,nc,0)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...SEQNO
c
      call rtoc (SEQCUR,lnum,nc)
      call simwrt (lnum,nc,cmsg,kerr)
c
c...ERROR text
c
      call simwrt (cdat,knc,cmsg,kerr)
c
c...End of ERROR Info
c
      call simwrt ('#EOL#',5,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simwrt (cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine buffers and writes Machine Simulation File
c              records.
c
c   INPUT:  cdat    C*n  D1  -  Character string to write.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine simwrt (cdat,knc,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 knc,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 rindex,ist,inc
c
c...Buffer input data
c
      if (MFKNC .eq. 0) then
          LPRTXT = cdat(1:knc)
      else
          LPRTXT(MFKNC+1:MFKNC+1) = ','
          LPRTXT(MFKNC+2:) = cdat(1:knc)
          MFKNC  = MFKNC  + 1
      endif
      MFKNC  = MFKNC  + knc
c
c...End of record
c...Output it
c
      if (knc .eq. 5 .and. cdat(1:knc) .eq. '#EOL#') then
          ist    = 1
  100     if (MFKNC .gt. 132) then
              inc    = rindex(LPRTXT(ist:ist+131),',')
              if (inc .eq. 0) then
                  call wrtxt (LUNSC3,LPRTXT(ist:ist+131),132,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  ist    = ist    + 132
                  MFKNC  = MFKNC  - 132
              else
                  call wrtxt (LUNSC3,LPRTXT(ist:ist+inc-1),inc,cmsg,
     1                        kerr)
                  if (kerr .ne. 0) go to 8000
                  ist    = ist    + inc
                  MFKNC  = MFKNC  - inc
              endif
              go to 100
          endif
          if (MFKNC .ne. 0) call wrtxt (LUNSC3,LPRTXT(ist:ist+MFKNC-1),
     1                                  MFKNC,cmsg,kerr)
          MFKNC  = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simtyp (knum)
c
c   FUNCTION:  This routine defines simulation type for machine type
c              and current mode.
c
c   INPUT:  none
c
c   OUTPUT: knum    I*4  D1  -  Simulation mode: 1 = mill 2-6 axis,
c                               2 = lathe.
c
c***********************************************************************
c
      subroutine simtyp (knum)
c
      include 'post.inc'
      integer*4 knum
c
      equivalence (MACHTP,KPOSMP(1201)), (LTMODE,KPOSMP(4125))
c
      integer*4 MACHTP,LTMODE
c
      if (MACHTP .ne. 4) then
          if (MACHTP .eq. 3) then
              knum = 1
          else
              knum = MACHTP
          end if
      else
          if (LTMODE .ne. 0) then
              knum = 1
          else
              knum = 2
          end if
      end if
c
      return
      end
c
c********************************************************************
c
c   SUBROUTINE:  getpiv (lth,ja,jb,pivoff)
c
c   FUNCTION:  Get a rotary axis pivot vector. Copy-and-paste from pivadj.
c              Called from simnum.
c
c   INPUT:  ja,jb      I*4        -  Active rotary axes.
c           lth        I*4           Calculation type:
c                                    0 for HH, 1 for TH, 2 for TT
c
c   OUTPUT: pivoff     R*8  D3    -  Pivot vector.
c
c***************************************************************
c
      subroutine getpiv (lth,ja,jb,pivoff)

      integer*4 lth,ja,jb
      real*8 pivoff(3)

      include 'post.inc'

      equivalence (IZSWIV,KPOSMP(1224)), (IRTNUM,KPOSMP(1243))
      equivalence (NOPIVS,KPOSMP(1282))
      equivalence (IRTINC,KPOSMP(1461)), (IRTDEF,KPOSMP(1485))
      equivalence (IRTYPE,KPOSMP(1486))
      equivalence (ITP   ,KPOSMP(1801)), (NOTOOL,KPOSMP(1802))
      equivalence (IZIGAD,KPOSMP(0371))

      integer*4 IRTDEF,IRTYPE(20),NOPIVS,ITP,NOTOOL,IZIGAD,
     1          IZSWIV,IRTINC(4),IRTNUM

      equivalence (ZIGDEP,POSMAP(0190)), (TL    ,POSMAP(3601))
      equivalence (SPIVEC,POSMAP(3583)), (TABORG,POSMAP(5374))

      real*8 ZIGDEP,TABORG(3,20),TL(120),SPIVEC(3)

      real*8 t,rd

      integer*4 iflt,iflp,i

      if (lth .eq. 2) then
        pivoff(1) = TABORG(1,2)
        pivoff(2) = TABORG(2,2)
        pivoff(3) = TABORG(3,2)
        return
      endif
c
c...Always adjust for pivots and heads
c...during simulation
c
cc      if (NOPIVS .ne. 1 .and. NOTOOL .ne. 1) return
c
      pivoff(1) = 0.d0
      pivoff(2) = 0.d0
      pivoff(3) = 0.d0
c
c...initialize offset
c
cc      iflt   = 2 - NOTOOL
cc      iflp   = 2 - NOPIVS
      iflt   = 1
      iflp   = 1
      rd     = 0.d0
      if (IZIGAD .eq. 1) rd = ZIGDEP
      t      = iflt * TL(ITP) - rd
c
c...adjust offset for all heads from rider to carrier
c
      do 325 i=IRTNUM,1,-1
         if (IRTYPE(IRTINC(i)) .eq. 2) then
            pivoff(1) = pivoff(1) - (iflp * TABORG(1,i) +
     -                  t * SPIVEC(1))
            pivoff(2) = pivoff(2) - (iflp * TABORG(2,i) +
     -                  t * SPIVEC(2))
            pivoff(3) = pivoff(3) - (iflp * TABORG(3,i) +
     -                  t * SPIVEC(3))
c
c...turn off tool length offset flag after it was used
c...with the lowest rider
c
            t      = 0.d0
         endif
  325 continue

      return
      end
c
c********************************************************************
c
c   SUBROUTINE:  simln (a0,a)
c
c   FUNCTION:  Calculate the initial linear axis positions, and the linear
c              axis increments. Copy-and-paste from linclr.
c              Called from simnum.
c
c   OUTPUT: a0    R*8  D3    -  Initial position.
c           a     R*8  D3    -  Increment vector.
c
c***************************************************************
c
      subroutine simln (a0,a)

      real*8 a0(3),a(3)

      include 'post.inc'

      equivalence (NUMLIN,KPOSMP(1202)), (ACTLIN,KPOSMP(1205))
      equivalence (LNCALC,KPOSMP(1208))
c
      integer*4 NUMLIN(3),ACTLIN(3),LNCALC(3)
c
      equivalence (LNDIST,POSMAP(1251))
      equivalence (AXSOUT,POSMAP(1340))
      equivalence (AXSSTO,POSMAP(1425))

      real*8 LNDIST(3),AXSOUT(10),AXSSTO(10)

      integer*4 inc,i
c
c...Calculate Primary and Secondary Axes
c
      inc    = 1
      do 500 i=1,3,1
          if (NUMLIN(i) .eq. 2) then
c
c......1st calculation type
c
              if (LNCALC(i) .eq. 1) then
                a0(i) = axssto(inc) + LNDIST(i) + axssto(inc+1)
c
c......2nd calculation type
c
              else if (LNCALC(i) .eq. 2) then
                  if (ACTLIN(i) .ne. 2) then
                      a0(i) = axssto(inc)
                  else
                      a0(i) = axssto(inc+1) + axssto(inc)
                  endif
c
c......3rd calculation type
c
              else
                  if (ACTLIN(i) .ne. 2) then
                      a0(i) = axssto(inc)
                  else
                      a0(i) = axssto(inc+1)
                  endif
              endif

             if (ACTLIN(i) .eq. 2) then
                a(i) = axsout(inc+1) - axssto(inc+1)
             else
                a(i) = axsout(inc) - axssto(inc)
             endif
c
c......Primary axis only
c
          else
              a0(i) = axssto(inc)
              a(i) = axsout(inc) - axssto(inc)
          endif
          inc = inc + 2
  500 continue
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotmx(k,c,s,mx,ord)
c
c   FUNCTION:  Calculate a 3-by-3 clockwise rotation matrix, or its
c              derivatives. Called from simnum.
c
c   INPUT:  k                -  rotation axis: 1-X, 2-Y, 3-Z
c           c                -  cosine
c           s                -  sine
c           ord              -  0 - the rotation matrix,
c                               1 - first derivative, 2 - second derivative
c
c                                                (c  -s   0)
c   OUTPUT: mx               -  rotation matrix: (s   c   0) for k = 3
c                                                (0   0   1)
c
c***********************************************************************
c
      subroutine rotmx(k,c,s,mx,ord)

      integer*4 k,ord
      real*8 c,s,mx(3,3)

      integer*4 i,j,ii,jj

      if (k .eq. 1) then
        i = 2
        j = 3
      else if (k .eq. 2) then
        i = 3
        j = 1
      else
        i = 1
        j = 2
      endif

      do ii = 1,3
        do jj = 1,3
          mx(ii,jj) = 0.
        enddo
      enddo

      if (ord .eq. 0) then
        mx(k,k) = 1.
        mx(i,i) = c
        mx(i,j) = -s
        mx(j,i) = s
        mx(j,j) = c
      else if (ord .eq. 1) then
        mx(i,i) = -s
        mx(i,j) = -c
        mx(j,i) = c
        mx(j,j) = -s
      else if (ord .eq. 2) then
        mx(i,i) = -c
        mx(i,j) = s
        mx(j,i) = -s
        mx(j,j) = -c
      endif

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  vctmmx (vi,vo,mx)
c
c   FUNCTION:  Multiply a 3-by-3 matrix by a column vector.
c              Called from simnum.
c
c   INPUT:  vi    R*8  D3    -  Input coordinates.
c
c           mx    R*8  D4.3  -  Conversion matrix.
c
c   OUTPUT: vo    R*8  D3    -  Output coordinates.
c
c***********************************************************************
c
      subroutine vctmmx (vi,vo,mx)

      real*8 vi(3),vo(3),mx(3,3)

      integer*4 i

      do i=1,3
        vo(i) = vi(1)*mx(i,1) + vi(2)*mx(i,2) + vi(3)*mx(i,3)
      enddo

      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simnum(stol,ntim)
c
c   FUNCTION:  Find number of steps into which to divide to move.
c              This is done by analyzing the curve build by
c              the alladr call form simmot (that is the curve which
c              would be built if ntim were large, e.g., 200).
c              We estimate the number of points needed to approximate
c              the curve by segments within the given tolerance, based
c              on the worst, i.e., the fastest changing part of the curve.
c
c   INPUT:  stol  R*8  D1  -  Tolerance
c
c   OUTPUT: ntim    I*4  D1  -  Number of steps to subdivide current
c                               motion
c
c***********************************************************************
c
      subroutine simnum(stol,ntim)

      include 'post.inc'

      real*8 stol
      integer*4 ntim

      equivalence (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506))
      equivalence (ITP   ,KPOSMP(1801)), (NOTOOL,KPOSMP(1802))

      integer*4 IRTWRK(20),IRTYPE(20),IRTACT(2),ITP,NOTOOL

      equivalence (RAD   ,POSMAP(0002)), (AXSOUT,POSMAP(1340))
      equivalence (AXSSTO,POSMAP(1425))
      equivalence (TL    ,POSMAP(3601)), (SPIVEC,POSMAP(3583))
      equivalence (TABORG,POSMAP(5374))

      real*8 RAD
      real*8 AXSOUT(10),AXSSTO(10),TABORG(3,20),TL(120),SPIVEC(3)

      integer*4 IRTINC(4)

      real*8 dal,dbe,alp,bet,coa,sia,cob,sib,al0,be0,dt,t,s,kap
      real*8 angle,cosang,del,del0,dal2,dbe2
      real*8 b0(3),b(3),v0(3),ra(3),rb(3),tmp(3),ra1(3),rb1(3),
     x       ra2(3),rb2(3),b1(3)
      real*8 nmag
      real*8 mxa(3,3),mxb(3,3),mxa1(3,3),mxb1(3,3),mxa2(3,3),mxb2(3,3)

      integer*4 i,j,n,ia,ib,lth,ja,jb

      ntim = 1
c
c..... active rotary axis
c
      ja = irtact(1)
      jb = irtact(2)
      dal = AXSOUT(6+ja)-AXSSTO(6+ja)
      dbe = AXSOUT(6+jb)-AXSSTO(6+jb)
      if (dabs(dal).lt.0.5 .and. dabs(dbe).lt.0.5) return
c
c..... lth is the calculation type: 0 for HH, 1 for TH, 2 for TT
c
      lth = 2
      if (IRTYPE(IRTINC(ja)) .eq. 2) lth = lth-1
      if (IRTYPE(IRTINC(jb)) .eq. 2) lth = lth-1
c
c..... which axis the rotation is around
c
      ia = IRTWRK(IRTINC(ja))
      ib = IRTWRK(IRTINC(jb))
      call getpiv (lth,ja,jb,v0)
      call simln (b0,b)

      ntim = 200
      del0 = 1.

      al0 = AXSSTO(6+ja)/RAD
      dal = dal/RAD
      dal2 = dal*dal
      be0 = AXSSTO(6+jb)/RAD
      dbe = dbe/RAD
      dbe2 = dbe*dbe

      if (lth .eq. 1) then ! TH
        b0(1) = b0(1) - TABORG(1,IRTINC(ja))
        b0(2) = b0(2) - TABORG(2,IRTINC(ja))
        b0(3) = b0(3) - TABORG(3,IRTINC(ja))
      else if (lth .eq. 2) then ! TT
cc        s = (2. - NOTOOL) * TL(ITP)
        s = TL(ITP)
        b0(1) = b0(1) - s * SPIVEC(1) - v0(1)
        b0(2) = b0(2) - s * SPIVEC(2) - v0(2)
        b0(3) = b0(3) - s * SPIVEC(3) - v0(3)
      endif
c
c..... calculate curvature at n points; then estimate at each point the safe
c..... parameter increment based on the curvature and the curve speed.
c
      n = 6
      dt = 1./n
      do 100 i=1,n+1
        t = (i-1)*dt
        alp = al0 + dal*t
        coa = dcos(alp)
        sia = dsin(alp)
        bet = be0 + dbe*t
        cob = dcos(bet)
        sib = dsin(bet)
c
c..... get rotation matrices and their derivatives
c
        call rotmx(ib,cob,sib,mxb,0)
        call rotmx(ia,coa,sia,mxa,0)
        call rotmx(ib,cob*dbe,sib*dbe,mxb1,1)
        call rotmx(ia,coa*dal,sia*dal,mxa1,1)
        call rotmx(ib,cob*dbe2,sib*dbe2,mxb2,2)
        call rotmx(ia,coa*dal2,sia*dal2,mxa2,2)
c
c..... b1 is the linear axis adjustment
c
        do j=1,3
          b1(j) = b0(j) + b(j)*t
        enddo
c
c..... rb is the result after the "rider" matrix is applied
c..... ra is the "final" curve point
c..... ra1 and ra2 are the first and second curve derivatives
c
      if (lth .eq. 1) then ! TH

        call vctmmx(v0,tmp,mxb)
        do j=1,3
          rb(j) = b1(j) + tmp(j)
        enddo

        call vctmmx(rb,ra,mxa)

        do j=1,3
          ra(j) = ra(j) + TABORG(j,IRTINC(ja))
        enddo

        call vctmmx(v0,tmp,mxb1)
        do j=1,3
          rb1(j) = b(j) + tmp(j)
        enddo

        call vctmmx(rb1,ra1,mxa)
        call vctmmx(rb,tmp,mxa1)
        do j=1,3
          ra1(j) = ra1(j) + tmp(j)
        enddo

        call vctmmx(v0,rb2,mxb2)

      else if (lth .eq. 2) then ! TT

        call vctmmx(b1,rb,mxb)
        do j=1,3
          rb(j) = v0(j) + rb(j)
        enddo

        call vctmmx(rb,ra,mxa)

        call vctmmx(b1,rb1,mxb1)
        call vctmmx(b,tmp,mxb)
        do j=1,3
          rb1(j) = rb1(j) + tmp(j)
        enddo

        call vctmmx(rb1,ra1,mxa)
        call vctmmx(rb,tmp,mxa1)
        do j=1,3
          ra1(j) = ra1(j) + tmp(j)
        enddo

        call vctmmx(b1,rb2,mxb2)
        call vctmmx(b,tmp,mxb1)
        do j=1,3
          rb2(j) = rb2(j) + 2.*tmp(j)
        enddo

      else ! HH
        call vctmmx(v0,rb,mxb)
        call vctmmx(rb,tmp,mxa)

        do j=1,3
          ra(j) = b1(j) + tmp(j)
        enddo

        call vctmmx(v0,rb1,mxb1)

        call vctmmx(rb1,ra1,mxa)
        call vctmmx(rb,tmp,mxa1)
        do j=1,3
          ra1(j) = ra1(j) + tmp(j) + b(j)
        enddo

        call vctmmx(v0,rb2,mxb2)

      endif

        call vctmmx(rb2,rb2,mxa)
        call vctmmx(rb1,ra2,mxa1)
        call vctmmx(rb,tmp,mxa2)

        do j=1,3
          ra2(j) = rb2(j) + 2*ra2(j) + tmp(j)
        enddo
c
c..... s is the local speed, or the length of the tangent vector
c..... kap is the local curvature
c
        s = nmag(ra1)
        call crosvc(ra1,ra2,tmp)
        kap = nmag(tmp)/(s*s*s)
c
c..... angle is the relative (calculated for the unit speed curve)
c..... angular increment for which the chord is within tolerance to the
c..... arc of radius 1/kap - and so within tolerance to our curve, which
c..... is locally approximated by the arc
c
        cosang = 1. - stol*kap
        angle = dacos(cosang)
c
c..... del is the actual curve parameter increment
c
        del = (2.*angle)/(kap*s)
        if (del .lt. del0) del0 = del

100   continue

      if (del0 .gt. 0.001) ntim = 1./del0 + 0.5

      return
      end
