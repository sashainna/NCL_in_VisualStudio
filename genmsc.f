c
c***********************************************************************
c
c   FILE NAME: genmsc.for
c   CONTAINS:
c               addtim  clsamp  fini    finrst  genini  genlst  lodmch
c               tapini  lodmaf  makmat
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        genmsc.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        10/19/15 , 09:02:33
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  addtim (gtim)
c
c   FUNCTION:  This routine adds the input time to all modes of time
c              time accumulations.
c
c   INPUT:  gtim    R*8  D1  -  Amount of time in minutes to add to
c                               accumulative machining times.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine addtim (gtim)
c
      include 'post.inc'
c
      equivalence (ITP   ,KPOSMP(1801))
c
      integer*4 ITP
c
      equivalence (SEQTIM,POSMAP(3558)), (CUTTIM,POSMAP(3559))
      equivalence (TLTIM ,POSMAP(3721))
c
      real*8 SEQTIM,CUTTIM,TLTIM(120)
c
      real*8 gtim
c
      TLTIM(ITP) = TLTIM(ITP) + gtim
      SEQTIM = SEQTIM + gtim
      CUTTIM = CUTTIM + gtim
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clsamp (kfl)
c
c   FUNCTION:  This routine loads either the next point (if one is
c              waiting on the stack) or the next clfile record.  It is
c              used by routines that must look ahead to complete their
c              calculations.
c
c              *BUG* When looking ahead in the clfile any equations
c                    encountered WILL CHANGE the value of the variable
c                    in the equation prematurely.
c
c   INPUT:  kfl     I*4  D1  -  Should be set to 0 on the first call so
c                               that the clfile and stack pointers can
c                               saved.  1 = Normal call.  -1 = Reset
c                               pointers (the next point/record will not
c                               be loaded). -2 = End sampling of clfile,
c                               but do not reset pointers. Use -2 when
c                               the calling routine will process the
c                               clfile record being sampled.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clsamp (kfl)
c
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MULTAX,KPOSMP(0056)), (IFIREC,KPOSMP(0057))
      equivalence (IFIPT ,KPOSMP(0058)), (NPT   ,KPOSMP(0059))
      equivalence (MXPT  ,KPOSMP(0060)), (LOOKPT,KPOSMP(0061))
      equivalence (NCUT  ,KPOSMP(0062)), (NCUTDS,KPOSMP(0063))
      equivalence (ICUTYP,KPOSMP(0064)), (NSHANK,KPOSMP(0067))
      equivalence (ICUSAV,KPOSMP(0069)), (ICLSMP,KPOSMP(0077))
      equivalence (CLISAV,KPOSMP(0131))
      equivalence (ICLMSV,KPOSMP(0151)), (NEXCIR,KPOSMP(1275))
      equivalence (AXSSPT,KPOSMP(1401)), (AXSSPV,KPOSMP(1402))
      equivalence (CMDSPT,KPOSMP(1403)), (CMDSPV,KPOSMP(1404))
      equivalence (IRAP  ,KPOSMP(3199))
c
      integer*4 MXCL,NPT,AXSSPT,AXSSPV,ITYPE,ISUBT,IFIREC,IFIPT,NEXCIR,
     1          ICLMSV(20),MXPT,LOOKPT,CMDSPT,CMDSPV,IPSTWD(50),MULTAX,
     2          IRAP,CLISAV(20),NCUT,NCUTDS,ICUTYP(3),NSHANK,ICUSAV(6),
     3          ICLSMP
c
      equivalence (CUSAV ,POSMAP(0364)), (PSTWD ,POSMAP(0441))
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (CUTTER,POSMAP(0744))
      equivalence (CIRBSV,POSMAP(0751)), (CUDISP,POSMAP(0759))
      equivalence (CUTOFS,POSMAP(0766)), (CLPTSV,POSMAP(1801))
c
      real*8 CLPT(240),CLPTSV(240),CIRBSV(7),CIRBUF(7),CUTTER(7),
     1       CUDISP(7),CUTOFS(4,3),PSTWD(20),CUSAV(26)
c
      equivalence (LPSTWD,CPOSMP(0217)), (LPWSAV,CPOSMP(0729))
      equivalence (CUTSYM,CPOSMP(3009)), (SYMSAV,CPOSMP(4147))
c
      character*80 CUTSYM(3),SYMSAV(3)
      character*132 LPWSAV
      character*512 LPSTWD
c
      integer*4 kfl
c
      integer*4 i,ierr,iax(10),icnt,inc
      integer*4 ipwdsv(50)
      save ipwdsv
c
      real*8 rmch(3,4),rlin(6),raxs(10),rrot(20,2),rvec(3)
      real*8 pwdsv(20)
c
      character*80 msg
c
      save pwdsv
c
      if (kfl .eq. -2) then
          call rststo (msg,ierr)
          go to 8000
      endif
      ierr = 0
      ICLSMP = 1
c
c...Initial call
c...Mark clfile position &
c...Save current CLPT array
c...vp 12/30/97 save whole CLPT since MXCL can be from later
c...pp command without any association to GOTO record
c
      if (kfl .eq. 0) then
c
c.....changed for added level
c.....Yurong 5/26/98
c
c          call clmark
          call clmark(1)
          CLISAV(1) = IRAP
          CLISAV(2) = MULTAX
          AXSSPV = AXSSPT
          CMDSPV = CMDSPT
          call copyn (CLPT,CLPTSV,240)
          call copyn (CIRBUF,CIRBSV,7)
          call copyn (PSTWD,pwdsv,20)
          call copynk (IPSTWD,ipwdsv,50)
          ICUSAV(1) = NCUT
          ICUSAV(2) = NCUTDS
          ICUSAV(3) = NSHANK
          call copynk (ICUTYP,ICUSAV(4),3)
          call copyn (CUTTER,CUSAV,7)
          call copyn (CUDISP,CUSAV(8),7)
          call copyn (CUTOFS,CUSAV(15),12)
          SYMSAV(1) = CUTSYM(1)
          SYMSAV(2) = CUTSYM(2)
          SYMSAV(3) = CUTSYM(3)
          LPWSAV = LPSTWD(1:132)
c
c...Final call
c...Reset clfile position
c
      else if (kfl .eq. -1) then
          if (ICLMSV(1) .eq. -1) go to 8000
c
c.....changed for added level
c.....Yurong 5/26/98
c
c          call clumrk
          call clumrk(1)
          IRAP   = CLISAV(1)
          MULTAX = CLISAV(2)
          AXSSPT = AXSSPV
          CMDSPT = CMDSPV
          call copyn (CLPTSV,CLPT,240)
          call copyn (CIRBSV,CIRBUF,7)
          call copyn (pwdsv,PSTWD,20)
          call copynk (ipwdsv,ipstwd,50)
          NCUT = ICUSAV(1)
          NCUTDS = ICUSAV(2)
          NSHANK = ICUSAV(3)
          call copynk (ICUSAV(4),ICUTYP,3)
          call copyn (CUSAV,CUTTER,7)
          call copyn (CUSAV(8),CUDISP,7)
          call copyn (CUSAV(15),CUTOFS,12)
          CUTSYM(1) = SYMSAV(1)
          CUTSYM(2) = SYMSAV(2)
          CUTSYM(3) = SYMSAV(3)
          LPSTWD(1:132) = LPWSAV
          go to 8000
      endif
c
c...Next point is circular motion
c...after held back motion
c
      if (NEXCIR .eq. 1) then
          if (kfl .eq. 0) then
              ITYPE  = 3000
              MXCL   = 7
          else
              ITYPE  = 5000
              ISUBT  = 5
              MXCL   = ICLMSV(7)
          endif
c
c...Next point comes
c...off the stack
c
      else if (AXSSPT .ne. 0) then
          if (CMDSPT .eq. AXSSPT) then
              call popcmd
          else
              call popaxs (rmch,rlin,rrot,raxs,rvec,iax,icnt)
              do 500 i=1,3,1
                  CLPT(i) = rmch(i,2)
                  CLPT(i+3) = rvec(i)
  500         continue
              ITYPE  = 5000
              ISUBT  = 6
              MXCL   = 1
          endif
c
c...Next point comes
c...from multiple point GOTO
c
      else if (LOOKPT .lt. MXPT) then
          LOOKPT = LOOKPT + 1
          inc    = (LOOKPT-1) * NPT
          do 600 i=1,NPT,1
              CLPT(i) = CLPT(inc+i)
  600     continue
          ITYPE  = 5000
          ISUBT  = 6
          MXCL   = 1
c
c...Next point comes
c...from the cl/object file
c
      else
          if (IMACPT .ne. 0) then
              call precmp (msg,ierr)
              if (ierr .lt. 0) go to 9000
          endif
          if (IMACPT .eq. 0) then
              call clread (IFIREC,IFIPT,1,msg,ierr)
              if (ierr .lt. 0) go to 9000
          endif
      endif
c
c...End of routine
c
 8000 ICLSMP = 0
      return
c
c...Error reading clfile
c
 9000 call errkil (msg,ierr)
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fini
c
c   FUNCTION:  This routine processes the FINI card.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine fini
c
      include 'post.inc'
c
      equivalence (FINCOD,KPOSMP(1105)), (NEOT  ,KPOSMP(1106))
      equivalence (IPCHPN,KPOSMP(1107)), (IPRDES,KPOSMP(1154))
c
      integer*4 FINCOD,NEOT,IPCHPN,IPRDES(2,10)
c
      equivalence (LEDEND,POSMAP(1202)), (FINCDV,POSMAP(1203))
c
      real*8 LEDEND,FINCDV
c
      equivalence (LEOT  ,CPOSMP(2211)), (PCHBUF,CPOSMP(2231))
      equivalence (PHFILE,CPOSMP(2535))
c
      character*20 LEOT
      character*40 PHFILE
      character*72 PCHBUF
c
c...Output the FINI code
c
      call clrbuf
      call codout (FINCOD,FINCDV)
      call clrbuf
c
c...Output End-of-tape mark
c
      if (NEOT .ne. 0) call pakout (LEOT,NEOT,0)
c
c...Output final leader
c
      if (LEDEND .ne. 0) call ledout (LEDEND)
c
c...Clear punch buffer
c
      if (IPCHPN .ne. 0) call pchout (PCHBUF,IPCHPN)
c
c...Output print file record
c
      call prtrec (IPRDES(1,9))
c
c...Reset end of tape variables
c...If we don't need them for the Punch File Header
c...Otherwise this reset will be done after the
c...header is created
c
      if (PHFILE .eq. ' ') call finrst
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  finrst
c
c   FUNCTION:  This routine resets post variables after processing the
c              FINI card.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine finrst
c
      include 'post.inc'
c
      equivalence (TAPLEN,POSMAP(1204))
      equivalence (MCHDLS,POSMAP(1221)), (ROTDLS,POSMAP(1233))
      equivalence (AXSDLS,POSMAP(1237)), (MCHDLT,POSMAP(1474))
      equivalence (ROTDLT,POSMAP(1486)), (LINDLT,POSMAP(1490))
      equivalence (AXSDLT,POSMAP(1496)), (MCHMIN,POSMAP(1506))
      equivalence (MCHMAX,POSMAP(1518)), (ROTMIN,POSMAP(1530))
      equivalence (ROTMAX,POSMAP(1534)), (LINMIN,POSMAP(1538))
      equivalence (LINMAX,POSMAP(1544)), (AXSMIN,POSMAP(1550))
      equivalence (AXSMAX,POSMAP(1560)), (LINDLS,POSMAP(1594))
      equivalence (SEQTIM,POSMAP(3558)), (CUTTIM,POSMAP(3559))
      equivalence (TLTIM ,POSMAP(3721))
c
      real*8 TAPLEN,MCHDLS(3,4),ROTDLS(4),LINDLS(6),
     1       AXSDLS(10),MCHDLT(3,4),ROTDLT(4),LINDLT(6),AXSDLT(10),
     2       MCHMIN(3,4),MCHMAX(3,4),ROTMIN(4),ROTMAX(4),LINMIN(6),
     3       LINMAX(6),AXSMIN(10),AXSMAX(10),SEQTIM,CUTTIM,TLTIM(120)
c
      integer*4 i,j
c
c...Reset end of tape variables
c
      do 100 i=1,4,1
          do 80 j=1,3,1
              MCHDLS(j,i) = 0.
              MCHDLT(j,i) = 0.
              MCHMIN(j,i) =  1000000.
              MCHMAX(j,i) = -1000000.
   80     continue
          ROTDLS(i) = 0.
          ROTDLT(i) = 0.
          ROTMIN(i) =  1000000.
          ROTMAX(i) = -1000000.
  100 continue
c
      do 200 i=1,6,1
          LINDLS(i) = 0.
          LINDLT(i) = 0.
          LINMIN(i) =  1000000.
          LINMAX(i) = -1000000.
  200 continue
c
      do 300 i=1,10,1
          AXSDLS(i) = 0.
          AXSDLT(i) = 0.
          AXSMIN(i) =  1000000.
          AXSMAX(i) = -1000000.
  300 continue
c
      SEQTIM = 0.
      CUTTIM = 0.
      do 400 i=1,120,1
          TLTIM(i) = 0.
  400 continue
      TAPLEN = 0.
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  genini
c
c   FUNCTION:  This routine initializes the post-processor and should be
c              called for every machine configuration file processed.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine genini
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICLMSV,KPOSMP(0151))
      equivalence (PICLMSV,KPOSMP(328)), (MOTREG,KPOSMP(0381))
      equivalence (IRTOUT,KPOSMP(0813)), (IFWSFL,KPOSMP(0833))
      equivalence (SGMACT,KPOSMP(0996)), (SGMHFL,KPOSMP(0998))
      equivalence (NCHDR ,KPOSMP(1174)), (MACHTP,KPOSMP(1201))
      equivalence (NUMLIN,KPOSMP(1202)), (LTHXY ,KPOSMP(1225))
      equivalence (LTHDIA,KPOSMP(1228))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTACT,KPOSMP(1256))
      equivalence (IRTMOD,KPOSMP(1284))
      equivalence (IROTPS,KPOSMP(1350)), (ICMREG,KPOSMP(1393))
      equivalence (IRTINC,KPOSMP(1461)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTDEF,KPOSMP(1485)), (IRTWRK,KPOSMP(1506))
      equivalence (ACHEAD,KPOSMP(1645)), (IACHFL,KPOSMP(1646))
      equivalence (IACFLG,KPOSMP(1648))
      equivalence (LTMODE,KPOSMP(4125)), (LTHSVD,KPOSMP(4127))
      equivalence (LTHXYS,KPOSMP(4129))
      equivalence (MOTRGS,KPOSMP(4141)), (NUMLIS,KPOSMP(4133))
      equivalence (SRTNUM,KPOSMP(4140)), (MOTROT,KPOSMP(4153))
      equivalence (SDFNUM,KPOSMP(4165))
c
      integer*4 IRTNUM,IRTMOD(4),IROTPS,NCHDR,MOTREG(24),
     1          NUMLIN(3),MACHTP,ICMREG(3),MOTRGS(4),NUMLIS(3),
     2          MOTROT(12),SRTNUM,LTMODE,IRTOUT(4),IFWSFL,LTHXY,
     3          LTHXYS,ACHEAD,IRTYPE(20),IRTWRK(20),IACHFL,LTHDIA(2),
     4          LTHSVD(2),IACFLG(5),IRTACT(2),ICLMSV(20),PICLMSV(20),
     5          IRTINC(4),IRTDEF,SDFNUM,SGMACT(2),SGMHFL(10)
c
      equivalence (TLATOL,POSMAP(0057))
      equivalence (ISEQBG,POSMAP(1163)), (ISEQ  ,POSMAP(1164))
      equivalence (AXSSTO,POSMAP(1425)), (ROTBAS,POSMAP(1435))
      equivalence (FUZZ4 ,POSMAP(4912)), (FUZZ8 ,POSMAP(4913))
      equivalence (FUZZM ,POSMAP(4914)), (ROTSTO,POSMAP(5213))
      equivalence (TABORG,POSMAP(5374)), (SGMDLS,POSMAP(5472))
      equivalence (SGMDLT,POSMAP(5490)), (SGMMIN,POSMAP(5508))
      equivalence (SGMMAX,POSMAP(5526)), (SGMQVL,POSMAP(5562))
c
      real*8 ROTSTO(20,2),ROTBAS(4),AXSSTO(10),FUZZ4,FUZZ8,FUZZM,
     1       TLATOL,ISEQBG,ISEQ,TABORG(3,20),SGMQVL,SGMDLS(6,3),
     2       SGMDLT(6,3),SGMMIN(6,3),SGMMAX(6,3)
c
      equivalence (LMNAME,CPOSMP(0141)), (LHDR  ,CPOSMP(2733))
c
      character*40 LMNAME
      character*132 LHDR
c
      integer*4 nc,nc1,nc2,nc3,nc4,strlen1,i,j,ir1,ir2,ip1
c
      real*8 rlin(6),rvec(3),nmag
c
      character*20 sb1,sb2
      character*80 msg
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
c...Miscellaneous initializations
c
      FUZZ4  = 1.0d-4
      FUZZ8  = 1.0d-8
      FUZZM  = 1.0d-15
      ICMREG(1) = MOTREG(1)
      ICMREG(2) = MOTREG(5)
      if (MACHTP .eq. 4 .and. ICMREG(2) .eq. 0)
     -    ICMREG(2) = ICMREG(1)
      ICMREG(3) = MOTREG(9)
      ICLMSV(1) = -1
      PICLMSV(1) = -1
c
c...Beginning sequence number
c
      ISEQBG = ISEQ
c
c...Save Y-axis registers for Lathe/Mill manipulation
c
      if (MACHTP .eq. 4) then
         call copynk (MOTREG(1),MOTRGS,12)
         call copynk (MOTREG(13),MOTROT(1),12)
         SRTNUM = IRTNUM
         SDFNUM = IRTDEF
         IRTNUM = 0
         IRTDEF = 0
         LTMODE = 0
         LTHSVD(1) = LTHDIA(1)
         LTHSVD(2) = LTHDIA(2)
         LTHXYS = LTHXY
         call copynk (NUMLIN,NUMLIS,3)
      end if
c
c...Set up Print file header
c
      nc1    = strlen1(LMNAME)
      nc2    = strlen1(PGMNAM)
      call errtxt ('LOGG1',msg)
      nc     = strlen1(msg)
      if (nc .eq. 5 .and. msg(1:nc) .eq. 'LOGG1') then
         msg = 'Numerical Control Computer Sciences'
      else
         call uncrmsg (nc,msg,msg)
      endif
      call itoc (PWVER1,sb1,nc3,1)
      call itoc (PWVER2,sb2,nc4,1)
      LHDR   = PGMNAM(1:nc2) // '/' // LMNAME(1:nc1) // '  Version: '
     1         // sb1(1:nc3) // '.' // sb2(1:nc4) // '  By: ' // msg
      NCHDR  = strlen1(LHDR)
c
c...Tool descriptions
c
      do 300 i=1,120,1
          TLNAME(i) = ' '
  300 continue
c
c...Adjust rotaries when one does not
c...rotate about a major axis
c
      call rscini
c
c...Set rotary positioning axis flag
c
      IROTPS = 0
      do 500 i=1,IRTDEF,1
          if (IRTMOD(i) .eq. 2) IROTPS = 1
          IRTOUT(i) = 1
  500 continue
      IFWSFL = 0
c
c...Set up default rotary positions
c
      call axsadr (AXSSTO,rlin,ROTSTO,rvec)
      call linrot (ROTSTO,ROTBAS,1)
c
c...Initialize rotary axes
c
      call rotbeg
c
c...Set default position
c
      call setpos
c
c...Determine if C-axis head
c
      if (ACHEAD .eq. 3) then
          ACHEAD = 2
          IACHFL = 0
          TLATOL = 0.
          if (IRTNUM .eq. 2 .and. IRTWRK(1) .eq. 3 .and.
     1            IRTYPE(1) .eq. 2 .and. IRTYPE(2) .eq. 2) then
              ACHEAD = 1
              IACFLG(1) = 2
              IACFLG(2) = 1
              IACFLG(3) = 1
              IACFLG(4) = 1
              TLATOL = .0002
          endif
      else
          ip1    = IRTINC(ir1)
          if (IACFLG(1) .eq. 2 .and. (IRTYPE(ip1) .eq. 1 .or.
     1        nmag(TABORG(1,ip1)) .ne. 0.)) IACFLG(1) = 3
      endif
c
c...Stringer machine initializations
c
      SGMQVL = 999.
c
c...Initialize Stringer Drill Machine
c
      SGMACT(1) = 1
      SGMACT(2) = 0
      do 4110 i=1,3,1
          do 4100 j=1,6,1
               SGMDLS(j,i) = 0.
               SGMDLT(j,i) = 0.
               SGMMIN(j,i) = 100000.
               SGMMAX(j,i) = -100000.
 4100     continue
 4110 continue
c
      do 4140 i=1,10,1
          SGMHFL(i) = 0
 4140 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  genlst (cmsg,kerr)
c
c   FUNCTION:  This routine outputs a record to the listing file, if
c              this option has been specified.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine genlst (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 isav
c
c...Output listing record
c
      kerr   = 0
      if (IOPFL(2) .eq. 1) then
          isav   = IOPFL(4)
          IOPFL(4) = 0
          call clwrit (INEREC,INEPT,cmsg,kerr)
          IOPFL(4) = isav
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  opnmch (klun,cmach,cmsg,kerr)
c
c   FUNCTION:  This routine opens an existing Machine descriptor file.
c
c   INPUT:  cmach   C*n  D1  -  Post-processor name.
c
c   OUTPUT: klun    I*4  D1  -  Logical unit number of Machine descrip-
c                               tor file.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine opnmch (klun,cmach,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      integer*4 klun,kerr
c
      character*(*) cmach,cmsg
c
      integer*4 nc,strlen1,irecl
c
      character*20 att(4)
      character*(MAX_PATH) fnam
c
c...Format file name
c
      nc     = strlen1(cmach)
      fnam   = 'PWORKS_' // cmach(1:nc) // '.MDF'
c
c...Open Machine Definition File
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 8000
      call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
      if (kerr .eq. -2) then
          call fparse (fnam,fnam,DVDATA,'.MDF')
          call opnfil (LUNSC4,fnam,att,irecl,cmsg,kerr)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodmch (klun,cmsg,kerr)
c
c   FUNCTION:  This routine loads the input Machine descriptor file.
c
c   INPUT:  klun    I*4  D1  -  Logical unit number of Machine descrip-
c                               tor file.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c                               Returns -1 when a warning occurred.
c
c***********************************************************************
c
      subroutine lodmch (klun,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 klun,kerr
c
      character*(*) cmsg
c
      equivalence (NPARTN,KPOSMP(0075))
      equivalence (NKPOSM,KPOSMP(0171)), (NPOSMA,KPOSMP(0172))
      equivalence (NCPOSM,KPOSMP(0173)), (MACHTP,KPOSMP(1201))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTMOD,KPOSMP(1284))
      equivalence (ISCWRK,KPOSMP(1453))
      equivalence (MACUST,KPOSMP(3980)), (MTPDYN,KPOSMP(4126))
c
      equivalence (MPOSMP,CPOSMP(0001)), (LPARTN,CPOSMP(0067))
      equivalence (LUPDAT,CPOSMP(3001))
c
      equivalence (PI    ,POSMAP(1)), (RAD   ,POSMAP(2))
c
      real*8 PI,RAD
c
      integer*4 MPOSMP(2000),MACHTP,IRTNUM,MACUST,NKPOSM,NPARTN,
     -          NPOSMA,NCPOSM,IRTMOD(4),MTPDYN,ISCWRK(2,4)
c
      character*8 LUPDAT
      character*66 LPARTN
c
      integer*4 i,is,j,jindex,mkp,mpo,mcp,npart
c
      real*8 pi1, rad1
c
      character*66 partn
c
c...save PI & RAD so nobody screw them
c
      pi1    = PI
      rad1   = RAD
      npart  = NPARTN
      partn  = LPARTN
      mkp    = 2
      mpo    = 4
      mcp    = 1
c
c...Load the Common arrays
c
      call rdcom (klun,1,KPOSMP(1),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Check version of Common arrays
c
        if (NKPOSM .gt. mkp) mkp = NKPOSM
        if (NPOSMA .gt. mpo) mpo = NPOSMA
        if (NCPOSM .gt. mcp) mcp = NCPOSM
c
      do 205 i=2,mkp
          call rdcom (klun,i,KPOSMP((i-1)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  205 continue
      is     = mkp + 1
      do 215 i=is,mkp+mcp
          call rdcom (klun,i,MPOSMP((i-is)*2000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  215 continue
      is     = is + mcp
      do 300 i=is,mkp+mcp+mpo,1
          call rdcom (klun,i,POSMAP((i-is)*1000+1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  300 continue
c
c...Update .MDF data if old file version
c...verify license if custom MDF file
c
      call update
      if (NLICMC .ne. 0) then
          if (MACUST .eq. 0) go to 9000
          j   = jindex (LICMCH,MACUST,NLICMC)
          if (j .eq. 0) then
              go to 9000
          else
              go to 8000
          end if
      end if
      call gtmach (MTPDYN)
c
c...Verify authorized options license
c......Mill
c
      if (MACHTP .eq. 1 .or. MACHTP .eq. 3) then
          if (IRTNUM .eq. 0 .or. (IRTNUM .eq. 1 .and. IRTMOD(1) .eq. 2))
     1            then
              if (LICOPT(3) .eq. 0 .and. LICOPT(4) .eq. 0 .and.
     1            LICOPT(5) .eq. 0 .and. LICOPT(6) .eq. 0) go to 9000
          else if (IRTNUM .eq. 1) then
              if (LICOPT(4) .eq. 0 .and. LICOPT(5) .eq. 0 .and.
     1            LICOPT(6) .eq. 0) go to 9000
          else if (IRTNUM .eq. 2) then
              if (LICOPT(5) .eq. 0 .and. LICOPT(6) .eq. 0) go to 9000
              do 400 i=1,IRTNUM,1
                  if (ISCWRK(1,i) .ne. 0 .and. LICOPT(6) .eq. 0)
     1                go to 9000
  400         continue
          else
              if (LICOPT(6) .eq. 0) go to 9000
          endif
c
c......2-axis Lathe
c
      else if (MACHTP .eq. 2) then
          if (LICOPT(1) .eq. 0 .and. LICOPT(2) .eq. 0) go to 9000
c
c......Mill/Lathe Combination
c
      else if (MACHTP .eq. 4) then
          if (LICOPT(2) .eq. 0) go to 9000
          if (IRTNUM .eq. 0) then
              if (LICOPT(3) .eq. 0 .and. LICOPT(4) .eq. 0 .and.
     1            LICOPT(5) .eq. 0 .and. LICOPT(6) .eq. 0) go to 9000
          else if (IRTNUM .eq. 1) then
              if (LICOPT(4) .eq. 0 .and. LICOPT(5) .eq. 0 .and.
     1            LICOPT(6) .eq. 0) go to 9000
          else if (IRTNUM .eq. 2) then
              if (LICOPT(5) .eq. 0 .and. LICOPT(6) .eq. 0) go to 9000
          else
              if (LICOPT(6) .eq. 0) go to 9000
          endif
c
c......Stringer Machine
c
      else if (MACHTP .eq. 5) then
          if (LICOPT(6) .eq. 0) go to 9000
c
c......Unrecognized machine type
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 call clsfil (klun)
      PI   = pi1
      RAD  = rad1
      NPARTN = npart
      LPARTN = partn
c
c...Make sure MDF file is not from
c...a future version of Mpost
c
      if (kerr .eq. 0) then
          call cnvday (LUPDAT,nupd)
          call cnvday (REVDAT,nrev)
          if (nupd .gt. nrev) then
              kerr   = -1
              call perrst ('MDFVER',1,cmsg,0,0.,LUPDAT,3)
              call perrst (cmsg,2,cmsg,0,0.,REVDAT,3)
          endif
      endif
      return
c
c...The current license does not
c...support this machine configuration file
c
 9000 call errtxt ('INVLIC',cmsg)
      kerr   = 3
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tapini (cmsg,kerr)
c
c   FUNCTION:  This routine outputs the initial information to the punch
c              file.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine tapini
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NPARTN,KPOSMP(0075)), (IDUMMY,KPOSMP(0088))
      equivalence (MCHOPT,KPOSMP(0308)), (ISEQSW,KPOSMP(0845))
      equivalence (UNTCOD,KPOSMP(1078)), (IPARTO,KPOSMP(1102))
      equivalence (NBOT  ,KPOSMP(1136)), (PSTNRG,KPOSMP(3381))
c
      integer*4 IPARTO,MCHOPT(20),UNTCOD(2),ISEQSW,NBOT,PSTNRG(15),
     1          IDUMMY,NPARTN
c
      equivalence (UNTCDV,POSMAP(1170)), (LEDBEG,POSMAP(1201))
c
      real*8 LEDBEG,UNTCDV(2)
c
      equivalence (LPARTN,CPOSMP(0067)), (LBOT  ,CPOSMP(2515))
c
      character*20 LBOT
      character*66 LPARTN
c
      integer*4 strlen1,nc,nc1,isav,nlop,i
c
      real*8 rnum
c
      character*80 lbuf,abuf
c
c...Output Start-of-tape mark
c
      if (NBOT .ne. 0) call pakout (LBOT,NBOT,0)
c
c...Output program number
c
      if (MCHOPT(4) .ne. IDUMMY .and. PSTNRG(11) .ne. 0) then
          isav   = ISEQSW
          ISEQSW = 2
          rnum   = MCHOPT(4)
          call codout (PSTNRG(11),rnum)
          call clrbuf
          ISEQSW = isav
      endif
c
c...Output PARTNO card to Punch file
c
      if (IPARTO .eq. 1 .or. IPARTO .eq. 3) then
          nlop   = NPARTN
          if (IPARTO .eq. 1) nlop = 1
          call getvwd (1045,abuf,nc1,1,PSTWRD,PSTWVL,NPSTWD)
          do 100 i=1,nlop,1
              lbuf   = abuf(1:nc1) // SFRMBUF(i)
              nc     = strlen1(lbuf)
              call pchout (lbuf,nc)
  100     continue
c
c......As message block
c
      else if (IPARTO .eq. 2 .or. IPARTO .eq. 4) then
          nlop   = NPARTN
          if (IPARTO .eq. 2) nlop = 1
          do 200 i=1,nlop,1
              nc     = strlen1(SFRMBUF(i))
              call msgout (SFRMBUF(i),nc)
  200     continue
      endif
c
c...Output initial leader
c
      call ledout (LEDBEG)
c
c...Output Rewind stop code
c
      call rwstop
c
c...Output Units selection
c
      call codout (UNTCOD(MCHOPT(2)),UNTCDV(MCHOPT(2)))
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodmaf (klun,cmsg,kerr)
c
c   FUNCTION:  This routine reads an machine adjust file, crates data
c              used to adjust cl data to machine.
c
c   INPUT:  klun    I*4  D1  Input file logical unit number.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodmaf (klun,cmsg,kerr)
c
      include 'menu.inc'
      include 'compile.inc'
      include 'post.inc'
C WNT-START
       include 'postworks_nt.inc'
C WNT-END
c
      integer*4 klun,kerr
c
      character*(*) cmsg
c
      equivalence (MXGTOL,KPOSMP(4001)), (MXFLAG,KPOSMP(4002))
c
      integer*4 MXGTOL,MXFLAG
c
      equivalence (GTLNO ,POSMAP(4051)), (GTLEN ,POSMAP(4171))
      equivalence (REFMAT,POSMAP(4001))
c
      real*8 GTLNO(120),GTLEN(120),REFMAT(12)
c
      integer*4 i,ipt,nwds,inx,inum,nc,ifl,iseq,ierr,
     1          ist,ien,ityp,ibuf(50),irec,np
C WNT-START
      integer*4 flag
C WNT-END
c
      real*8 rbuf(50),gts(18),vec(3),pt1(6),pt2(6),vc(3),ve(3)
c
      equivalence (pt1,gts(1)), (pt2,gts(7))
c
      character*80 ldat,msg,msg1
c
      irec   = 0
      np     = 0
      ifl    = 0
      MXGTOL = 0
      MXFLAG = 0
c
c...Initialize statement parsing routines
c
      MAXPRN = 0
      MAXTOK = 100
c
c...Read source file record
c
  100 kerr   = 0
      call srcprs (irec,ldat,nc,ifl,cmsg,kerr)
      if (kerr .eq. 2) go to 8000
      if (kerr .ne. 0 .and. NTOK .eq. 0) go to 9000
c
c...Initialize per record flags
c
      ipt    = 1
      if (iseq .ne. 2) iseq   = 0
c
c...Nxx sequence number
c
      if (ICTYP(ipt) .eq. 4) then
         ist    = RCSUB(ipt)
         ien    = ICNC(ipt)
         if (LCTXT(ist:ist) .eq. 'N' .and. ist .lt. ien) then
             call ctoi (LCTXT(ist+1:ien),inum,ierr)
             if (ierr .eq. 0) then
                ipt    = ipt    + 1
                if (ipt .ge. NTOK) go to 9000
                if (ICTYP(ipt) .ne. 2 .or. RCSUB(ipt) .ne. 2)
     1                    go to 9000
                ipt    = ipt    + 1
             endif
         endif
      endif
c
c...Next token should be a Post word
c
  200 if (kerr .ne. 0 .or. ICTYP(ipt) .ne. 1) go to 9000
      ityp   = RCSUB(ipt)
      ipt    = ipt    + 1
      nwds   = 0
      if (ipt .le. NTOK) then
         if (ICTYP(ipt) .ne. 2 .or. RCSUB(ipt) .ne. 7) go to 9000
         if (ipt .lt. NTOK) then
             ipt    = ipt    + 1
             call srcpwd (ityp,irec,ipt,ibuf,rbuf,nwds,cmsg,kerr)
             if (kerr .ne. 0) go to 9000
         endif
      endif
c
c...Standard TRANS matrix
c
      if (ityp .eq. 1037) then
          if (np .ne. 0) go to 9500
          if (nwds .ne. 12) go to 9100
          do 325 i=1,nwds,1
              if (ibuf(i) .ne. 0) go to 9200
              REFMAT(i) = rbuf(i)
  325     continue
          np     = 2
          MXFLAG = 1
c
c...TOOLNO/tn,len
c
      else if (ityp .eq. 1025) then
          if (nwds .gt. 3 .or. nwds .lt. 2) go to 9100
          if (ibuf(1) .ne. 0) go to 9200
          i      = MXGTOL + 1
          if (i .gt. 120) go to 9400
          if (nwds .eq. 2) then
             if (ibuf(2) .ne. 0) go to 9200
             GTLEN(i) = rbuf(2)
          else
             if (ibuf(2) .ne. 9) go to 9300
             if (ibuf(3) .ne. 0) go to 9200
             GTLEN(i) = rbuf(3)
          end if
          MXGTOL = i
          GTLNO(i) = rbuf(1)
c
c...POINT/x,y,z,i,j,k
c
      else if (ityp .eq. 4035) then
          if (np .ge. 3) go to 9500
          if (nwds .ne. 6) go to 9100
          inx    = np * 6
          do 425 i=1,nwds,1
              if (ibuf(i) .ne. 0) go to 9200
              gts(i+inx) = rbuf(i)
  425     continue
          np    = np + 1
c
c...Make matrix from 2 points
c
          if (np .eq. 3) then
              call makmat (gts,gts(7),gts(13),REFMAT,ierr)
              if (ierr .ne. 0) go to 9100
              MXFLAG = 1
c
ccccc....temporary check matrix conv
c
              ien = 1
              vec(1) = gts(1) + .5*(gts(7)-gts(1))
              vec(2) = gts(2) + .5*(gts(8)-gts(2))
              vec(3) = gts(3) + .5*(gts(9)-gts(3))
              call matpta (vec,vc,REFMAT,ien)
              call matptr (vc,ve,REFMAT,ien)
cccccc
          end if
      else
          go to 9000
      endif
c
      go to 100
c
c...End of routine
c
 8000 call clsfil (klun)
      kerr   = 0
      return
c
c...Invalid adjust file record
c
 9000 kerr   = 3
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C9010 call trmmsg (' ')
C     call errtxt ('INVADJF',msg)
C     call trmmsg (msg)
C     call trmmsg (ldat)
C     if (kerr .ne. 3) call trmmsg (msg1)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 9010 flag = 1
      call errtxt ('INVADJF',msg)
      nc = strlen1(msg)
      call add1dispmsg (msg, nc, flag)
      nc = strlen1(ldat)
      call add1dispmsg (ldat, nc, flag)
      if (kerr .ne. 3) then
          nc = strlen1(msg1)
          call add1dispmsg(msg1, nc, flag)
      endif
C WNT-END
      kerr   = 0
      go to 100
c
c...Invalid number of parameters
c
 9100 call errtxt ('INVNUMPR',msg1)
      go to 9010
c
c...Number expected
c
 9200 call errtxt ('NUMBEXP',msg1)
      go to 9010
c
c...Invalid minor word
c
 9300 call errtxt ('INVMINOR',msg1)
      go to 9010
c
c...Too many tools
c
 9400 call errtxt ('TOOLOVR1',msg1)
      go to 9010
c
c...Multiple definition of point or matrix
c
 9500 call errtxt ('MULTPARM',msg1)
      go to 9010
      end
c
c***********************************************************************
c
c   SUBROUTINE:  makmat (gpt1,gpt2,gpt3,gmat,kerr)
c
c   FUNCTION:  This routine creates conversion matrix using 3 pairs of
c              points where the first triplets defines an input coord-
c              system and the second one defines the output coord-sys.
c
c   INPUT:  gpt1    R*8  D6  Reference point 1; 1-3 original coordintaes
c                            4-6 new coordinates
c
c           gpt2    R*8  D6  Reference point 2;         --"--
c
c           gpt3    R*8  D6  Reference point 3;         --"--
c
c   OUTPUT: gmat    R*8  D12 Conversion Matrix array.
c
c           kerr    I*4  D1  Returns 1 when points are too close or
c                            on a single line.
c
c***********************************************************************
c
      subroutine makmat (gpt1,gpt2,gpt3,gmat,kerr)
c
      include 'post.inc'
c
      integer*4 kerr
      real*8 gpt1(6),gpt2(6),gpt3(6),gmat(12)
c
      equivalence (MXGTOL,KPOSMP(4001)), (MXFLAG,KPOSMP(4002))
c
      integer*4 MXGTOL,MXFLAG
c
      equivalence (REFMAT,POSMAP(4001))
c
      real*8 REFMAT(12)
c
      integer*4 i
c
      real*8 ve1(3),ve2(3),dl1,dl2,org(3),vc1(3),vc2(3),
     -       ve3(3),vz1(3),tran(3),scal,tmp(3),mat3(4,3),
     -       mat1(4,3),mat2(4,3),a,a1,a2,b,b1,b2,ml(12),
     -       ve4(3),vz2(3),vs1(3),vs2(3),dl3,dl4,c,c1,c2,ndot,ndist
c
      equivalence (ml,mat1)
c
c...Set initial values
c
      kerr   = 0
c
c...Get direction of old & new system
c
      do 115 i=1,3,1
          ve1(i) = gpt2(i) - gpt1(i)
          ve2(i) = gpt2(i+3) - gpt1(i+3)
          ve3(i) = gpt3(i) - gpt1(i)
          ve4(i) = gpt3(i+3) - gpt1(i+3)
          tran(i) = 0.d0
  115 continue
c
      dl1 = ndist (gpt2,gpt1)
      dl2 = ndist (gpt2(4),gpt1(4))
      dl3 = ndist (gpt3,gpt1)
      dl4 = ndist (gpt3(4),gpt1(4))
c
c...Check if points are different,
c...get scale factor
c
      if (dl1 .lt. .001 .or. dl2 .lt. .001) go to 9000
      if (dl3 .lt. .001 .or. dl4 .lt. .001) go to 9000
      scal   = dl2 / dl1
c
      do 215 i=1,3,1
          ve1(i) = ve1(i) / dl1
          ve2(i) = ve2(i) / dl2
          ve3(i) = ve3(i) / dl3
          ve4(i) = ve4(i) / dl4
  215 continue
c
c...Get (Z) vector of old & new planes
c
      vz1(1) = ve1(2)*ve3(3) - ve1(3)*ve3(2)
      vz1(2) = ve1(3)*ve3(1) - ve1(1)*ve3(3)
      vz1(3) = ve1(1)*ve3(2) - ve1(2)*ve3(1)
      a1     = dsqrt(ndot(vz1,vz1))
      if (a1 .lt. .001) go to 9000
      vz2(1) = ve2(2)*ve4(3) - ve2(3)*ve4(2)
      vz2(2) = ve2(3)*ve4(1) - ve2(1)*ve4(3)
      vz2(3) = ve2(1)*ve4(2) - ve2(2)*ve4(1)
      a2     = dsqrt(ndot(vz2,vz2))
      if (a2 .lt. .001) go to 9000
      do 225 i=1,3
         vz1(i) = vz1(i) / a1
         vz2(i) = vz2(i) / a2
  225 continue
c
c...Get rotation of Z axis
c
      call vecang (ve1,3,a1)
      call vecang (ve2,3,a2)
      a      =  a1
      a1     = 0. - a1
      a2     = 0. - a2
c
c...Rotate vectors to ZX plane
c
      call vecadj (ve1,vc1,a1,3)
      call vecadj (ve2,vc2,a2,3)
      call vecadj (vz1,vs1,a1,3)
      call vecadj (vz2,vs2,a2,3)
c
c...Get rotation of Y axis
c
      call vecang (vc1,2,b1)
      call vecang (vc2,2,b2)
      b      = b1
      b1     = 0. - b1
      b2     = 0. - b2
c
      call vecadj (vs1,vz1,b1,2)
      call vecadj (vs2,vz2,b2,2)
c
c...Get the total rotation of plane vector
c
      call vecang (vz1,3,c1)
      call vecang (vz2,3,c2)
      c      = c1 - c2
c
c...Get translation matrix for rotation
c...about Z and Y axes from original position
c
      call gtmatr (3,a,tran,mat1)
      call gtmatr (2,b,tran,mat2)
      call matmul (mat1,mat2,mat3)
c
c...Get translation matrix for rotation
c...about Z from ZX plan to final position
c
      call gtmatr (3,c,tran,mat1)
      call matmul (mat3,mat1,mat2)
      call gtmatr (2,b2,tran,mat1)
      call matmul (mat2,mat1,mat3)
      call gtmatr (3,a2,tran,mat2)
      call matmul (mat3,mat2,mat1)
c
      do 325 i=1,12
          gmat(i)=ml(i) * scal
  325 continue
c
c...Translate origin and add to output matrix
c
      call matpta (gpt1,tmp,gmat,2)
      tmp(1) = gpt1(4) - tmp(1)
      tmp(2) = gpt1(5) - tmp(2)
      tmp(3) = gpt1(6) - tmp(3)
      call ptmatb (tmp,org,mat1,2)
c
      gmat(4) = 0. - org(1)
      gmat(8) = 0. - org(2)
      gmat(12) = 0. - org(3)
c
c...End of routine
c
 8000 return
c
c...Error in definition
c
 9000 kerr  = 1
      go to 8000
      end
