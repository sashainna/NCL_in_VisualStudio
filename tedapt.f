c
c***********************************************************************
c
c     FILE NAME: tedapt.f
c
c     CONTAINS:  ptdf_aptsrc  ptdf_aptstmt
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        tedapt.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:19
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_aptsrc(cdat, knc)
c
c   FUNCTION: Convert an NC block into apt source statements and add
c             them to the apt source list.
c
c   INPUT:  cdat    C*n  D1  - NC block
c
c           knc     I*4  D1  - Length of block
c
c           klin    I*4  D1  - Line number from file.
c
c           ktyp    I*4  D1  - Type of file to create, PCNV_APTSRC or
c                              PCNV_SIMUL.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptdf_aptsrc (cdat,knc,klin,ktyp)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      integer*4 knc,klin,ktyp
c
      character*(*) cdat
c
      equivalence (ISN   ,KPOSMP(0001))
      equivalence (IOPSKP,KPOSMP(0092)), (REGSW ,KPOSMP(0405))
      equivalence (NEOB  ,KPOSMP(0841)), (NCBMSG,KPOSMP(0843))
      equivalence (NCEMSG,KPOSMP(0844)), (NOPSKP,KPOSMP(1077))
      equivalence (NRWSTP,KPOSMP(1104)), (NEOT  ,KPOSMP(1106))
      equivalence (ICSMRG,KPOSMP(1132)), (NBOT  ,KPOSMP(1136))
      equivalence (IRTDEF,KPOSMP(1485)), (FMTDES,KPOSMP(2133))
      equivalence (IROTFT,KPOSMP(3110))
      equivalence (IFDRAD,KPOSMP(3210)), (IFDRCD,KPOSMP(3213))
c
      integer*4 REGSW(MAXFMT),NEOB,NCBMSG,NCEMSG,NOPSKP,NEOT,NBOT,
     1          ICSMRG,IFDRAD,IRTDEF,IFDRCD(4),IROTFT,IOPSKP,ISN,
     2          NRWSTP
      integer*2 FMTDES(10,MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003)), (REGSTO,POSMAP(1032))
c
      real*8 DUMMY,REGSTO(MAXFMT)
c
      equivalence (LEOB  ,CPOSMP(0971)), (MSGST ,CPOSMP(0976))
      equivalence (MSGEN ,CPOSMP(1500)), (LOPSKP,CPOSMP(1648))
      equivalence (LRWSTP,CPOSMP(2201))
      equivalence (LEOT  ,CPOSMP(2211)), (LBOT  ,CPOSMP(2515))
c
      character*5 LEOB, LOPSKP
      character*10 LRWSTP, MSGST, MSGEN
      character*20 LEOT, LBOT

      integer*4 i,i1,i2,i3,ix,nco,lnc,jnc,nregs,lpartno,reglst(MAXFMT),
     1          ierr1,ierr,inc,nc,nc1
c
      real*8 regvals(MAXFMT)
c
      character*8 cpartno
      character*80 aptdat,msg
      byte bdat(80)
c
      equivalence (aptdat, bdat)
c
      integer*4 displyv /1021/
      integer*4 vinsert /1046/
      integer*4 offv    /72/
      integer*4 onv     /71/
      integer*4 opskipv /1012/
      integer*4 partnov /1045/
      integer*4 pprintv /1044/
      integer*4 preapt  /4048/
      integer*4 pstapt  /4049/
c
      integer*4 vwds(4),vtyp(4)
c
      real*8 vval(4)
c
c...Initialize routine
c
      PCNV_TYPE = ktyp
      SMSTOP = 0
      SMSTAT = 0
      ISN    = klin
      TBLAST = TBLOCK
      MILAST = MINTER
      TBLOCK = PBLK_UNKNOWN
      if (MINTER .eq. PMOT_ROTARY) MINTER = PMOT_LINEAR
      jnc    = knc
      do 20 i=1,MAXFMT
        REGSW(i) = 0
        if (FMTDES(10,i) .eq. 4) REGSTO(i) = 0.
   20 continue
cc      call getvwd (gotov,cgoto,lgoto,1,PSTWRD,PSTWVL,NPSTWD)
c
c...Get past leading blanks.
c
      ix  = 1
      do while (cdat(ix:ix).eq.' ')
          ix = ix + 1
          if (ix .gt. knc) goto 8000
      enddo
      lnc = knc - ix + 1
c
c...Beginning of tape character
c
      if (NBOT .gt. 0 .and. NBOT .le. lnc .and.
     1      cdat(ix:NBOT+ix-1) .eq. LBOT(1:NBOT)) then
          if (ktyp .eq. PCNV_APTSRC) then
              nco = lnc + 3
              if (nco .gt. 72) nco = 72
              aptdat(1:nco) = '$$ ' // cdat(ix:knc)
              call ptd_addapt (bdat, nco)
          endif
          goto 8000
      endif
c
c...End of tape character
c
      if (NEOT .gt. 0 .and. NEOT .le. lnc .and.
     1      cdat(ix:NEOT+ix-1) .eq. LEOT(1:NEOT)) then
          if (ktyp .eq. PCNV_APTSRC) then
              nco = lnc + 3
              if (nco .gt. 72) nco = 72
              aptdat(1:nco) = '$$ ' // cdat(ix:knc)
              call ptd_addapt (bdat, nco)
          endif
          goto 8000
      endif
c
c...Message, output as PPRINT
c
      i1 = index(cdat(ix:knc), MSGST(1:NCBMSG))
      if ((NCBMSG .lt. lnc) .and. (i1 .gt. 0)) then
c
c......Turn display on, if necessary.
c
          if (PTEDSP .eq. 0 .and. ktyp .eq. PCNV_APTSRC) then
              vwds(1) = displyv
              vtyp(2) = 1
              vwds(2) = onv
              call ptdf_aptstmt (2,vtyp,vwds,vval,msg)
              PTEDSP = 1
          endif
          i1 = i1 + NCBMSG
          i2 = knc
          if (NCEMSG .gt. 0) then
              i3 = index(cdat(i1:knc),MSGEN(1:NCEMSG))
              if (i3 .gt. 0) then
                  i2 = i3 + i1 - 2
              endif
          endif
          if (ktyp .eq. PCNV_APTSRC) then
              call getvwd (pprintv,aptdat,i,1,PSTWRD,PSTWVL,NPSTWD)
              nco = i2 - i1 + i + 1
              if (nco .gt. 72) nco = 72
              aptdat(7:nco) = cdat(i1:i2)
              call ptd_addapt (bdat,nco)
          else
              nco = i2 - i1 + 1
              call simdis (cdat(i1:i2),nco,msg,ierr)
          endif
c
c......Get rid of message text from tape block
c
          cdat(i1-NCBMSG:i2+NCEMSG) = ' '
          do while (cdat(ix:ix).eq.' ')
              ix = ix + 1
              if (ix .gt. lnc) goto 8000
          enddo
          lnc = lnc - ix + 1
cc          goto 8000
      endif
c
c...PARTNO
c
      call getvwd (partnov,cpartno,lpartno,1,PSTWRD,PSTWVL,NPSTWD)
      if (cdat(ix:ix+lpartno-1) .eq. cpartno(1:lpartno)) then
          if (ktyp .eq. PCNV_APTSRC) then
              nco = lnc
              if (nco .gt. 72) nco = 72
              aptdat(1:nco) = cdat(ix:knc)
              call ptd_addapt (bdat,nco)
          endif
          goto 8000
      endif
c
c...OPSKIP character(s)
c
      if (IOPSKP .eq. 0) then
          if (NOPSKP .gt. 0 .and. NOPSKP .le. lnc) then
              if (cdat(ix:ix+NOPSKP-1) .eq. LOPSKP(1:NOPSKP)) then
                  if (ktyp .eq. PCNV_APTSRC) then
                      vwds(1) = opskipv
                      vtyp(2) = 1
                      vwds(2) = onv
                      call ptdf_aptstmt (2,vtyp,vwds,vval,msg)
                  endif
                  IOPSKP = 1
                  ix  = ix + NOPSKP
                  if (ix .gt. knc) goto 8000
              endif
          endif
      else
          if (NOPSKP .gt. 0 .and. NOPSKP .le. lnc) then
              if (cdat(ix:ix+NOPSKP-1) .eq. LOPSKP(1:NOPSKP)) then
                  ix  = ix + NOPSKP
                  if (ix .gt. knc) goto 8000
              else
                  if (ktyp .eq. PCNV_APTSRC) then
                      vwds(1) = opskipv
                      vtyp(2) = 1
                      vwds(2) = offv
                      call ptdf_aptstmt (2,vtyp,vwds,vval,msg)
                  endif
                  IOPSKP = 0
              endif
          endif
      endif
      do while (cdat(ix:ix).eq.' ')
          ix = ix + 1
          if (ix .gt. knc) goto 8000
      enddo
c
c...Remove EOB characters
c
      lnc = knc - ix + 1
      if (lnc .ge. NEOB) then
          if (NEOB.gt.0) then
              if (cdat(knc-NEOB+1:knc).eq.LEOB(1:NEOB)) then
                  lnc = lnc-NEOB
                  jnc = jnc-NEOB
              endif
          endif
      endif
c
c...Rewind stop code
c
      if (lnc .ge. NRWSTP .and. NRWSTP .ne. 0) then
          if (cdat(ix:ix+NRWSTP-1) .eq. LRWSTP(1:NRWSTP)) then
              ix     = ix     + NRWSTP
              lnc    = lnc    - NRWSTP
          endif
      endif
c
c...Parse input block
c
      if (lnc .eq. 0) go to 8000
      call setblk (cdat(ix:),lnc,TBLOCK,MINTER,reglst,regvals,nregs,1)
      if (nregs .lt. 0) go to 9000
c
c...Call pre-processing macro
c
      call ptdf_mactoapt (preapt)
c
c...Delete any unnecessary codes
c
      call deregs (reglst,regvals,nregs)
c
c...Handle special block types
c
      call spcblk (TBLOCK,reglst,regvals,nregs)
c
c...Handle special post-processor commands
c
      call precmd (reglst,regvals,nregs)
c
c...Output motion statement
c
      if (TBLOCK .eq. PBLK_MOTION) call motted (reglst,regvals,nregs)
c
c...Handle afterwards post-processor commands
c
      call pstcmd (reglst,regvals,nregs)
c
c...Call post-processing macro
c
      if (ktyp .eq. PCNV_APTSRC) call ptdf_mactoapt (pstapt)
c
c...Get rid of any extraneous codes
c......CHECK/LENGTH
c......Rotary axis radius
c
      call fndreg (reglst,regvals,nregs,ICSMRG,DUMMY,inc)
      if (inc .ne. 0) call delreg (reglst,regvals,nregs,inc)
      if (TBLOCK .eq. PBLK_MOTION .and. MINTER .eq. PMOT_ROTARY .and.
     1    IROTFT .eq. 7 .and. IFDRAD .eq. 1) then
          do 2500 i=1,IRTDEF,1
              call fndreg (reglst,regvals,nregs,IFDRCD(i),DUMMY,inc)
              if (inc .ne. 0) call delreg (reglst,regvals,nregs,inc)
 2500     continue
      endif
c
c...Output any codes leftover
c...as INSERT statement
c
      call getvwd (vinsert,aptdat,nco,1,PSTWRD,PSTWVL,NPSTWD)
      nc     = nco
      do 3000 i=1,nregs,1
          if (REGSW(reglst(i)) .eq. 1) then
              call fmtcod (reglst(i),regvals(i),aptdat(nco+1:),nc1)
              nco    = nco    + nc1
          endif
 3000 continue
       if (nco .gt. nc) then
           if (ktyp .eq. PCNV_APTSRC) then
               if (nco .gt. 72) nco = 72
               call ptd_addapt (bdat, nco)
           else
               call simdis (aptdat(nc+1:),nco-nc,msg,ierr1)
           endif
       endif
c
c...End of routine
c
 8000 return
c
c...Invalid tape block
c
 9000 continue
      nco = knc + 3
      if (nco .gt. 72) nco = 72
      aptdat(1:nco) = '$$ ' // cdat(1:knc)
      call ptd_addapt (bdat, nco)
      goto 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ptdf_aptstmt (knc,ktyp,kwds,gvals,gtxt)
c
c   FUNCTION: Create an apt statement out of a list of vocabulary words,
c             real values, and/or text strings and add it to the apt source
c             list.
c
c   INPUT:  knc     I*4  D1  - Number of parameters.  The first parameter
c                              is always the major word.
c
c           ktyp    I*4  Dn  - 1 = Vocabulary word, 2 = Real value, 3 = Text.
c
c           kwds    I*4  Dn  - Vocabulary words or starting position within
c                              'gtxt' of text.
c
c           gvals   R*8  Dn  - Real values or ending position within 'gtxt'
c                              of text.
c
c           gtxt    C*n  D1  - Character string.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ptdf_aptstmt (knc,ktyp,kwds,gvals,gtxt)
c
      include 'menu.inc'
      include 'pted.inc'
c
      integer*4 knc,ktyp(10),kwds(10)
c
      real*8 gvals(10)
c
      character*(*) gtxt
c
      integer*2 fdes(10)
      integer*4 i,nc,l1,ix,ist,ien,strlen1
c
      real*8 rnum
c
      character*1 sep
      character*8 lnum
      character*20 lbuf
      character*80 aptdat
      byte bdat(80)
c
      equivalence (rnum,lnum)
c
      equivalence (aptdat,bdat)
c
      data fdes /3,0,1,8,8,4,4,1,0,0/
c
      if (PCNV_TYPE .eq. PCNV_SIMUL) go to 8000
c
c...Format Major word
c
      call getvwd (kwds(1),aptdat,l1,1,PSTWRD,PSTWVL,NPSTWD)
      sep = '/'
      ix = 7
c
c...Loop through minor words/values
c
      do 100 i=2,knc,1
          aptdat(ix:ix) = sep
          sep    = ','
          ix     = ix     + 1
          if (ktyp(i) .eq. 1) then
              if (kwds(i) .eq. -1) then
                  rnum   = gvals(i)
                  lbuf   = lnum
                  nc     = strlen1(lbuf)
              else
                  call getvwd (kwds(i),lbuf,nc,2,PSTWRD,PSTWVL,NPSTWD)
              endif
          else if (ktyp(i) .eq. 2) then
              call ftoc (gvals(i),lbuf,nc,fdes)
          else
              ist    = kwds(i)
              ien    = gvals(i)
              lbuf   = gtxt(ist:ien)
              nc     = ien    - ist    + 1
          endif
          if (ix+nc+2 .gt. 72) then
              aptdat(ix:) = '$'
              call ptd_addapt (bdat,ix)
              aptdat = ' '
              ix     = 8
          endif
          aptdat(ix:) = lbuf
          ix     = ix     + nc
  100 continue
c
c...Output final command
c
      if (knc .gt. 1) l1 = ix - 1
      call ptd_addapt (bdat,l1)
c
c...End of routine
c
 8000 return
      end
