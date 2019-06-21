c***********************************************************************
c
c     FILE NAME: pstcmd.f
c
c     CONTAINS:  precmd  pstcmd  tmarka  clampa  coolna  cutcma  cyclea
c                fedrta  loadta  modea   postna  rotaba  selcta  spinda
c                toolna  unitsa  opstpa  rapida  stopa   rewnda  finia
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pstcmd.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        06/09/14 , 16:26:02
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  precmd (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the following commands by searching
c             for the appropriate codes.  These commands are output
c             prior to the motion block.
c
c                CLAMP      COOLNT    CUTCOM   CYCLE   FEDRAT    LOADTL
c                MODE/INCR  MODE/TOOL POSTN/n  RAPID   SELCTL    SLOWDN
c                SPINDL     TOOLNO    UNITS
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine precmd (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      integer*4 ierr
c
      character*80 msg
c
c...Don't process if no registers
c
      if (knreg .eq. 0) go to 8100
c
      SMSTAT = 0
      SMSTOP = 0
c
c...UNITS
c
      call unitsa (kregs,gvals,knreg)
c
c...TMARK
c
      call tmarka (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...CLAMP
c
      call clampa (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...COOLNT
c
      call coolna (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...CUTCOM
c
      call cutcma (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...CYCLE
c
      call cyclea (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...RAPID
c
      call rapida (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...FEDRAT
c
      call fedrta (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...LOADTL
c
      call loadta (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...MODE
c
      call modea (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...POSTN
c
      call postna (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...ROTABL
c
      call rotaba (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...SELCTL
c
      call selcta (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...SPINDL
c
      call spinda (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...TOOLNO
c
      call toolna (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...Output Simulation record
c
 8000 continue
      if (PCNV_TYPE .eq. PCNV_SIMUL .and. SMSTAT .eq. 1)
     1    call simsta (SMSTOP,msg,ierr)
c
c...End of routine
c
 8100 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pstcmd (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the following commands by searching
c             for the appropriate codes.  These commands are output
c             after the motion block.
c
c                FINI    OPSTOP  REWIND  STOP
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine pstcmd (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
      integer*4 ierr
c
      character*80 msg
c
c...Don't process if no registers
c
      if (knreg .eq. 0) go to 8000
      SMSTAT = 0
      SMSTOP = 0
c
c...OPSTOP
c
      call opstpa (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...STOP
c
      call stopa (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...REWIND
c
      call rewnda (kregs,gvals,knreg)
      if (knreg .eq. 0) go to 8000
c
c...FINI
c
      call finia (kregs,gvals,knreg)
c
c...Output Simulation record
c
      if (PCNV_TYPE .eq. PCNV_SIMUL .and. SMSTAT .eq. 1)
     1    call simsta (SMSTOP,msg,ierr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tmarka (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the TMARK command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine tmarka (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (ALNCOD,KPOSMP(0848))
c
      integer*4 ALNCOD
c
      equivalence (DUMMY ,POSMAP(0003)), (SEQCUR,POSMAP(1183))
c
      real*8 DUMMY,SEQCUR
c
      integer*4 inc,nco
c
      integer*4 vtmark,vwds(3),vtyp(3)
c
      real*8 vval(3)
c
      character*80 vtxt
c
      data vtmark /1005/
c
c...Check for Alignment register
c
      call fndreg (kregs,gvals,knreg,ALNCOD,DUMMY,inc)
      if (inc .ne. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vwds(1) = vtmark
              nco    = 1
              if (gvals(inc) .ne. 0.) then
                  nco    = nco    + 1
                  vtyp(nco) = 2
                  vval(nco) = gvals(inc)
              endif
              call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
          endif
          if (gvals(inc) .ne. 0) SEQCUR = gvals(inc)
          call delreg (kregs,gvals,knreg,inc)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clampa (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the CLAMP command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine clampa (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (IRTCLM,KPOSMP(1306)), (CLMPCD,KPOSMP(1307))
      equivalence (IJKROT,KPOSMP(1739))
c
      integer*4 CLMPCD(2,10),IRTCLM,IJKROT,IRTNUM
c
      equivalence (CLMPVL,POSMAP(1454))
c
      real*8 CLMPVL(2,10)
c
      integer*4 i,j,inc,nco
      integer*4 vclamp,vaxis,von,voff,vwds(5),vtyp(5)
c
      real*8 vval(5)
c
      character*80 vtxt
c
      data vaxis /132/, vclamp /1060/, voff /72/, von /71/
c
c...Determine if clamping is necessary
c
      if (IRTNUM .eq. 0 .or. IRTCLM .eq. 1 .or. IJKROT .eq. 1)
     1      go to 8000
c
c...Setup basic command
c
      vtyp(1) = 1
      vwds(1) = vclamp
      vtyp(2) = 1
      vwds(2) = vaxis
      nco    = 2
c
c...Check for clamping codes
c
      do 200 i=7,10,1
          do 100 j=1,2,1
              call fndreg (kregs,gvals,knreg,CLMPCD(j,i),CLMPVL(j,i),
     1                     inc)
              if (inc .ne. 0) then
                  if (PCNV_TYPE .eq. PCNV_APTSRC) then
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      vval(nco) = i      - 6
                      nco    = nco    + 1
                      vtyp(nco) = 1
                      vwds(nco) = voff
                      if (j .eq. 1) vwds(nco) = von
                      call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
                  endif
                  call delreg (kregs,gvals,knreg,inc)
                  nco    = 2
              endif
  100     continue
  200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  coolna (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the COOLNT command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine coolna (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (COOLCD,KPOSMP(3128)), (ICOLSW,KPOSMP(3146))
c
      integer*4 COOLCD(4),ICOLSW
c
      equivalence (COOLVL,POSMAP(3294))
c
      real*8 COOLVL(4)
c
      integer*4 i,inc
      integer*4 vcool,vwds(2),vtyp(2),vprm(4)
c
      real*8 vval(2)
c
      character*80 vtxt
c
      data vcool /1030/, vprm /90,89,1011,72/
c
c...Setup basic command
c
      vtyp(1) = 1
      vwds(1) = vcool
c
c...Check for COOLNT codes
c
      do 100 i=1,4,1
          call fndreg (kregs,gvals,knreg,COOLCD(i),COOLVL(i),inc)
          if (inc .ne. 0) then
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vtyp(2) = 1
                  vwds(2) = vprm(i)
                  call ptdf_aptstmt (2,vtyp,vwds,vval,vtxt)
              else
                  ICOLSW = 1
                  if (i .eq. 4) ICOLSW = 0
                  SMSTAT = 1
              endif
              call delreg (kregs,gvals,knreg,inc)
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
c   SUBROUTINE:  cutcma (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the CUTCOM command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine cutcma (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (MCHPLN,KPOSMP(1261)), (CUTCFL,KPOSMP(3251))
      equivalence (CUTCCD,KPOSMP(3271)), (ICUTDO,KPOSMP(3301))
c
      integer*4 CUTCCD(30),ICUTDO(15),CUTCFL(20),MCHPLN
c
      equivalence (DUMMY ,POSMAP(0003)), (CUTTER,POSMAP(0744))
      equivalence (CUTCVL,POSMAP(2410))
c
      real*8 DUMMY,CUTCVL(30),CUTTER(7)
c
      integer*4 i,inc,inc1,inc2,nco,nc,icd(4),ie,ireg,imod,ierr
      integer*4 vcut,vwds(10),vprm(6),vtyp(10),vadj,vpln(3),vminus
c
      real*8 vval(10)
c
      character*80 vtxt,msg
c
      data vadj /159/, vcut /1007/, vpln /37,41,33/
      data vprm /8,24,72,664,71,72/, vminus /10/
      data icd /1,2,3,24/
c
c...Setup basic command
c
      vwds(1) = vcut
      nco    = 1
c
c...Check for CUTCOM codes
c
      ie     = 3
      if (CUTCFL(17) .eq. 1) ie = 4
      do 100 i=1,ie,1
          call fndreg (kregs,gvals,knreg,CUTCCD(icd(i)),CUTCVL(icd(i)),
     1                 inc)
          if (inc .ne. 0) then
              nco    = nco    + 1
              vtyp(nco) = 1
              vwds(nco) = vprm(i)
              if (i .le. 2) then
                  nco    = nco    + 1
                  vtyp(nco) = 1
                  vwds(nco) = vpln(MCHPLN)
                  call fndreg (kregs,gvals,knreg,CUTCCD(4),DUMMY,inc1)
                  if (inc1 .ne. 0) then
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      vval(nco) = gvals(inc1)
                      call delreg (kregs,gvals,knreg,inc1)
                  endif
                  ICUTDO(1) = 1
                  ICUTDO(2) = 1
                  ICUTDO(3) = i
              else if (i .eq. 3) then
                  ICUTDO(1) = 0
                  ICUTDO(2) = 0
c
c......CUTCOM/ENDPT
c
              else if (i .eq. 4) then
c
c......CUTCOM/ENDPT,d
c
                  call fndreg (kregs,gvals,knreg,CUTCCD(4),DUMMY,inc1)
                  if (inc1 .ne. 0) then
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      vval(nco) = gvals(inc1)
                      call delreg (kregs,gvals,knreg,inc1)
                  endif
c
c......CUTCOM/ENDPT,TOOL
c
                  call fndreg (kregs,gvals,knreg,CUTCCD(25),DUMMY,inc1)
                  call fndreg (kregs,gvals,knreg,CUTCCD(26),DUMMY,inc2)
                  if (inc1 .ne. 0 .or. inc2 .ne. 0) then
                      nco    = nco    + 1
                      vtyp(nco) = 1
                      vwds(nco) = 617
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      if (inc1 .eq. 0) then
                          vval(nco) = CUTTER(1) / 2.
                      else
                          vval(nco) = gvals(inc1)
                      endif
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      if (inc2 .eq. 0) then
                          vval(nco) = CUTTER(2)
                      else
                          vval(nco) = gvals(inc2)
                      endif
                      call delreg (kregs,gvals,knreg,inc1)
                      call fndreg (kregs,gvals,knreg,CUTCCD(26),DUMMY,
     1                             inc2)
                      call delreg (kregs,gvals,knreg,inc2)
                  endif
c
c......CUTCOM/ENDPT,ROTREF
c
                  call fndreg (kregs,gvals,knreg,CUTCCD(27),DUMMY,inc1)
                  if (inc1 .ne. 0) then
                      nco    = nco    + 1
                      vtyp(nco) = 1
                      vwds(nco) = 68
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      vval(nco) = gvals(inc1)
                      call delreg (kregs,gvals,knreg,inc1)
                  endif
c
c......CUTCOM/ENDPT,MAXIPM
c
                  call fndreg (kregs,gvals,knreg,CUTCCD(28),DUMMY,inc1)
                  if (inc1 .ne. 0) then
                      nco    = nco    + 1
                      vtyp(nco) = 1
                      vwds(nco) = 96
                      nco    = nco    + 1
                      vtyp(nco) = 2
                      vval(nco) = gvals(inc1)
                      call delreg (kregs,gvals,knreg,inc1)
                  endif
                  ICUTDO(1) = 1
                  ICUTDO(2) = 1
                  ICUTDO(3) = 3
              endif
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
              else
                  SMSTAT = 1
              endif
              call delreg (kregs,gvals,knreg,inc)
          endif
  100 continue
c
c...Delete any extraneous CUTCOM registers
c
      if (ICUTDO(2) .eq. 1) then
c
c......Vector registers
c
          if ((CUTCFL(1) .ge. 1 .and. CUTCFL(1) .le. 3) .or.
     1        ICUTDO(3) .eq. 3) then
              call fndreg (kregs,gvals,knreg,CUTCCD(5),CUTCVL(5),inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
              do 200 i=6,8,1
                  call fndreg (kregs,gvals,knreg,CUTCCD(i),DUMMY,inc)
                  if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
  200         continue
c
c......Angular register
c
          else if (CUTCFL(1) .eq. 4) then
              call fndreg (kregs,gvals,knreg,CUTCCD(9),DUMMY,inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
          endif
c
c......Approach / Departure codes
c
          if (CUTCFL(3) .eq. 1) then
              do 300 i=12,14,1
                  call fndreg (kregs,gvals,knreg,CUTCCD(i),CUTCVL(i),
     1                         inc)
                  if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
  300         continue
              call fndreg (kregs,gvals,knreg,CUTCCD(15),DUMMY,inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
          endif
      endif
c
c...CUTCOM/ADJUST
c
      vtyp(2) = 1
      vwds(2) = vadj
c
c...CUTCOM/ADJUST,ON/OFF
c
      do 400 i=17,18,1
          call fndreg (kregs,gvals,knreg,CUTCCD(i),CUTCVL(i),inc)
          if (inc .ne. 0) then
              imod   = 18 - i
              ireg   = 0
              vtyp(3) = 1
              vwds(3) = vprm(i-12)
              nc     = 3
              call delreg (kregs,gvals,knreg,inc)
              call fndreg (kregs,gvals,knreg,CUTCCD(16),DUMMY,inc)
              if (inc .ne. 0) then
                  if (i .eq. 17) then
                      imod   = 1
                      vtyp(3) = 2
                      vval(3) = gvals(inc)
                      if (vval(3) .lt. 0. .and. CUTCFL(11) .eq. 1) then
                          imod   = 2
                          vval(3) = -vval(3)
                          nc     = nc     + 1
                          vtyp(nc) = 1
                          vwds(nc) = vminus
                      endif
                      ireg = vval(3)
                  endif
                  call delreg (kregs,gvals,knreg,inc)
              endif
c
c......Output APT source record
c
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
c
c......Output Simulation file record
c
              else if (PCNV_TYPE .eq. PCNV_SIMUL) then
                  call simofs (imod,ireg,-1,-1,msg,ierr)
              endif
          endif
  400 continue
c
c...CUTCOM/ADJUST,n
c
      call fndreg (kregs,gvals,knreg,CUTCCD(16),DUMMY,inc)
      if (inc .ne. 0) then
c
c......Output APT source record
c
          nc     = 3
          vtyp(nc) = 2
          vval(nc) = gvals(inc)
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              if (vval(nc) .lt. 0. .and. CUTCFL(11) .eq. 1) then
                  vval(nc) = -vval(nc)
                  nc     = nc     + 1
                  vtyp(nc) = 1
                  vwds(nc) = vminus
              endif
              call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
c
c......Output Simulation file record
c
          else if (PCNV_TYPE .eq. PCNV_SIMUL) then
              imod   = 1
              ireg   = vval(nc)
              if (vval(nc) .lt. 0. .and. CUTCFL(11) .eq. 1) then
                  imod   = 2
                  ireg   = -ireg
              endif
              call simofs (imod,ireg,-1,-1,msg,ierr)
          endif
          call delreg (kregs,gvals,knreg,inc)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cyclea (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the CYCLE/OFF and RETRCT commands.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.  When 'knreg'
c                               is set to 0, then an internal CYCLE/OFF
c                               command is being generated.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine cyclea (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (CYCCOD,KPOSMP(0211)), (CYCREG,KPOSMP(0231))
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
      equivalence (ICYCSV,KPOSMP(0291)), (MACCOD,KPOSMP(0801))
      equivalence (ICYMTP,KPOSMP(0920))
c
      integer*4 CYCCOD(20),ICYCSW(5),ICYCDO(15),CYCREG(20),ICYCSV(3),
     1          ICYMTP,MACCOD(5),ICYCFL(30)
c
      equivalence (DUMMY ,POSMAP(0003)), (MACCDV,POSMAP(1184))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387))
      equivalence (CYCCDV,POSMAP(2901)), (RCYCDO,POSMAP(2931))
      equivalence (CYCPSV,POSMAP(2951)), (PSTCMD,POSMAP(5173))
c
      real*8 CYCCDV(25),CYCPSV(10),RCYCDO(20),STONUM(3,4),MCHNUM(3,4),
     1       LINAXS(6),AXSOUT(10),TLVEC(3),ROTANG(20,2),DUMMY,MACCDV(5)
c
      integer*4 i,j,inc,inx(3),iret(3),ie
      integer*4 vmaj(3),vmin(3),vwds(2),vtyp(2),vrap
c
      real*8 vval(2)
c
      character*80 vtxt
c
      data vmaj /1054,7,7/, vmin /72,71,72/, vrap /5/
      data inx /1,19,20/ iret /0,1,2/
c
c...Check for CYCLE codes
c
      ie     = 3
      if (knreg .eq. 0) ie = 1
      do 100 i=1,ie,1
          if (knreg .eq. 0) then
              inc    = 1
          else if (ICYMTP .eq. 1 .and. ICYCSW(1) .eq. 0 .and. i .eq. 1)
     1            then
              call fndreg (kregs,gvals,knreg,MACCOD(1),MACCDV(1),inc)
          else
              call fndreg (kregs,gvals,knreg,CYCCOD(inx(i)),
     1                     CYCCDV(inx(i)),inc)
          endif
          if (inc .ne. 0) then
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vwds(1) = vmaj(i)
                  vtyp(2) = 1
                  vwds(2) = vmin(i)
                  call ptdf_aptstmt (2,vtyp,vwds,vval,vtxt)
              endif
              if (knreg .ne. 0) call delreg (kregs,gvals,knreg,inc)
              if (iret(i) .ne. 0) ICYCSW(3) = iret(i)
c
c......CYCLE/OFF
c
              if (i .eq. 1) then
                  do 80 j=1,10,1
                      ICYCDO(j) = 0
   80             continue
c
c.........Search for CYCLE/OFF code
c.........When cycles are called via a Macro
c
                  if (ICYMTP .eq. 1 .and. knreg .ne. 0) then
                      call fndreg (kregs,gvals,knreg,CYCCOD(inx(i)),
     1                     CYCCDV(inx(i)),inc)
                      call delreg (kregs,gvals,knreg,inc)
                      ICYMTP = 0
                  endif
c
c.........Search for Rapto code
c
                  if (knreg .ne. 0) then
                      call fndreg (kregs,gvals,knreg,CYCREG(2),DUMMY,
     1                    inc)
                      if (inc .ne. 0) then
                          RCYCDO(4) = gvals(inc)
                          CYCPSV(7) = gvals(inc)
                          call delreg (kregs,gvals,knreg,inc)
                      endif
                  endif
c
c.........Set retract position
c
                  if (ICYCSV(3) .gt. 0) then
                      AXSOUT(5) = CYCPSV(7)
                      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,
     1                             5,1)
                  endif
                  ICYCSV(3) = 0
              endif
          endif
  100 continue
c
c...Check for cycle interrupt
c
      if (ICYCSW(1) .lt. 0) then
          call fndreg (kregs,gvals,knreg,CYCCOD(15),CYCCDV(15),inc)
          if (inc .ne. 0) then
              call delreg (kregs,gvals,knreg,inc)
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vwds(1) = vrap
                  call ptdf_aptstmt (1,vtyp,vwds,vval,vtxt)
              endif
          endif
c
c...Remove CYCLE definition code
c
      else if (ICYCSW(1) .gt. 0) then
          if (ICYCFL(30) .eq. 1) then
              call fndreg (kregs,gvals,knreg,MACCOD(1),MACCDV(1),inc)
              if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
          endif
          call fndreg (kregs,gvals,knreg,CYCCOD(ICYCSW(1)+1),
     1                 CYCCDV(ICYCSW(1)+1),inc)
          if (inc .ne. 0) then
              call delreg (kregs,gvals,knreg,inc)
              CYCPSV(6) = STONUM(3,3)
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
c   SUBROUTINE:  fedrta (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the FEDRAT command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine fedrta (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (IFOTYP,KPOSMP(3151)), (RAPCD ,KPOSMP(3191))
      equivalence (IRAP  ,KPOSMP(3199)), (FEDOCD,KPOSMP(3217))
c
      integer*4 IFOTYP,FEDOCD(2),IRAP,RAPCD(5)
c
      equivalence (DUMMY ,POSMAP(0003)), (FEDOVL,POSMAP(3001))
      equivalence (FEED  ,POSMAP(3547))
c
      real*8 DUMMY,FEED(4),FEDOVL(2)
c
      integer*4 inc,i
c
      integer*4 vfedrt,vwds(3),vtyp(3),vprm(2),vlock
c
      real*8 vval(3)
c
      character*80 vtxt
c
      data vfedrt /1009/, vlock /114/, vprm /71,72/
c
c...Check for Feedrate code
c
      call fndreg (kregs,gvals,knreg,-14,DUMMY,inc)
      if (inc .ne. 0) then
          if (gvals(inc) .ne. 0.) FEED(1) = gvals(inc)
          call delreg (kregs,gvals,knreg,inc)
c
c......Cancel RAPID if rapid code is F0
c
          if ((RAPCD(1) .ge. 15 .and. RAPCD(1) .le. 17) .or.
     1        RAPCD(1) .eq. -14) IRAP = 0
      endif
c
c...Check for FEDRAT/LOCK codes
c
      do 100 i=1,2,1
          call fndreg (kregs,gvals,knreg,FEDOCD(i),FEDOVL(i),inc)
          if (inc .ne. 0) then
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vwds(1) = vfedrt
                  vtyp(2) = 1
                  vwds(2) = vlock
                  vtyp(3) = 1
                  vwds(3) = vprm(i)
                  call ptdf_aptstmt (3,vtyp,vwds,vval,vtxt)
              endif
              call delreg (kregs,gvals,knreg,inc)
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
c   SUBROUTINE:  loadta (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the LOADTL command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine loadta (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
      include 'ptedtool.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (NCUT  ,KPOSMP(0062)), (ICUTYP,KPOSMP(0064))
      equivalence (NSHANK,KPOSMP(0067)), (ICUCHG,KPOSMP(0068))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (ITP   ,KPOSMP(1801)), (MXTOOL,KPOSMP(1803))
      equivalence (TLCCD ,KPOSMP(1839)), (LTMODE,KPOSMP(4125))
c
      integer*4 ITP,TLCCD(20),NCUT,MXTOOL,NSHANK,ICUCHG,ICUTYP(3),
     1          MACHTP,LTMODE
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (CUTTER,POSMAP(0744)), (CUTOFS,POSMAP(0766))
      equivalence (REGSTO,POSMAP(1032))
      equivalence (TL    ,POSMAP(3601)), (TLNO  ,POSMAP(3841))
      equivalence (TLCVL ,POSMAP(3963))
c
      real*8 TLNO(120),TL(120),CUTTER(7),REGSTO(MAXFMT),TLCVL(20),DUMMY,
     1       CUTOFS(4,3)
c
      equivalence (CUTSYM,CPOSMP(3009))
c
      character*80 CUTSYM(3)
c
      integer*4 i,j,inc,inc1,nco,ien,strlen1,itreg
      integer*4 vcuttr,vlodtl,vlngth,vdispl,vshank,vholdr,vofset,
     1          vwds(20),vtyp(20),vradus,vturet
c
      real*8 vval(20),tln
c
      character*80 aptdat,vtxt
      byte bdat(80)
c
      equivalence (aptdat,bdat)
c
      data vcuttr /4025/, vlngth /9/, vlodtl /1055/
      data vdispl /1021/, vshank /192/, vholdr /157/, vofset /705/
      data vturet /1033/, vradus /23/
c
c...Check for LOADTL code
c
      if (TLCCD(5) .ne. 0) then
          call fndreg (kregs,gvals,knreg,TLCCD(5),TLCVL(5),inc)
          if (inc .eq. 0) go to 8000
          call delreg (kregs,gvals,knreg,inc)
      else
          call fndreg (kregs,gvals,knreg,TLCCD(4),DUMMY,inc)
          if (inc .eq. 0) go to 8000
      endif
      itreg = TLCCD(4)
      if (itreg .eq. 0) itreg = TLCCD(3)
      if (itreg .ne. 0 .and. REGSTO(itreg) .ne. 0) then
          call fndreg (kregs,gvals,knreg,itreg,DUMMY,inc1)
          if (inc1 .ne. 0) call delreg (kregs,gvals,knreg,inc1)
          tln    = REGSTO(TLCCD(3))
      else
          tln    = ITP    + 1
          if (MXTOOL .eq. 0) tln = 1
      endif
c
c...Check for corresponding Cutter
c
      i      = 1
      ITPX   = 0
      do while (i .le. NTOOL .and. ITPX .eq. 0)
          if (tln .eq. TLNUM(i)) ITPX = i
          i      = i      + 1
      enddo
      if (ITPX .eq. 0) ITPX = NTOOL
c
c...See if tool was loaded before
c
      i      = 1
      ITP    = 0
      do while (i .le. MXTOOL .and. ITP .eq. 0)
          if (tln .eq. TLNO(i)) ITP = i
          i      = i      + 1
      enddo
c
c...Tool was not previously used
c
      if (ITP .eq. 0) then
          MXTOOL = MXTOOL + 1
          ITP    = MXTOOL
      endif
c
c...LOADTL variables
c
      TLNO(ITP) = tln
      if (ITPX .eq. 0) then
          TL(ITP) = 0.
      else
          TL(ITP) = TLLEN(ITPX)
c
c...CUTTER variables
c
          NCUT   = NCUTR(ITPX)
          do 100 i=1,NCUT,1
              CUTTER(i) = CUTR(ITPX,i)
  100     continue
c
c...Cutter display variables
c
          NSHANK = ISHKFL(ITPX)
          do 300 i=1,3,1
              ICUTYP(i) = ICTYPE(ITPX,i)
              CUTSYM(i) = CUSYM(ITPX,i)
              do 200 j=1,4,1
                  CUTOFS(j,i) = CUPARM(ITPX,j,i)
  200         continue
  300     continue
      endif
c
c...APT source output
c
      if (PCNV_TYPE .eq. PCNV_APTSRC) then
c
c......Output LOADTL statement
c
          vtyp(1) = 1
          if (MACHTP .eq. 2 .or. (MACHTP .eq. 4 .and. LTMODE .eq. 0))
     1            then
              vwds(1) = vturet
          else
              vwds(1) = vlodtl
          endif
          vtyp(2) = 2
          vval(2) = tln
          nco    = 2
          if (TL(ITP) .ne. 0.d0) then
              nco    = nco    + 1
              vtyp(nco) = 1
              if (MACHTP .eq. 2 .or.
     1            (MACHTP .eq. 4 .and. LTMODE .eq. 0)) then
                  vwds(nco)= vradus
              else
                  vwds(nco)= vlngth
              endif
              nco    = nco    + 1
              vtyp(nco) = 2
              vval(nco) = TL(ITP)
          endif
          call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
c
c...Output CUTTER command
c
          if (ITPX .ne. 0) then
              if (NCUT .ne. 0) then
                  vtyp(1) = 1
                  vwds(1) = vcuttr
                  nco    = NCUT   + 1
                  do 500 i=1,NCUT,1
                      if (CUTTER(i) .lt. -9000) then
                          vtyp(i+1) = 1
                          vwds(i+1) = CUTTER(i) + 10000
                      else
                          vtyp(i+1) = 2
                          vval(i+1) = CUTTER(i)
                      endif
  500             continue
                  call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
              endif
c
c...CUTTER/DISPLY
c
              vtyp(1) = 1
              vwds(1) = vcuttr
              vtyp(2) = 1
              vwds(2) = vdispl
              do 900 i=1,3,1
                  nco    = 3
                  vtyp(nco) = 1
                  if (i .eq. 1) nco = 2
                  if (i .eq. 2) vwds(3) = vshank
                  if (i .eq. 3) vwds(3) = vholdr
c
c......CUTTER/DISPLY,sym
c
                  if ((ICUTYP(i) .eq. 1 .and. i .ne. 1) .or.
     1                     ICUTYP(i) .eq. 2) then
                      if (ICUTYP(i) .eq. 2) then
                          nco    = nco    + 1
                          vtyp(nco) = 3
                          vtxt = CUTSYM(i)
                          vwds(nco) = 1
                          vval(nco) = strlen1(CUTSYM(i))
                      endif
c
c.........Determine number of parameters to output
c
                      if (i .eq. 1) then
                          ien    = 0
                          if (CUTOFS(1,i) .ne. 0. .or.
     1                        CUTOFS(2,i) .ne. 0.) then
                              ien    = 2
                              nco    = nco    + 1
                              vtyp(nco) = 1
                              vwds(nco) = vofset
                          endif
                      else
                          ien    = 0
                          do 620 j=1,4,1
                              if (CUTOFS(j,i) .ne. 0.) ien = j
  620                     continue
                      endif
c
c.........CUTTER/DISPLY,sym,parms
c
                      do 650 j=1,ien,1
                          if (j .eq. 3 .and. ICUTYP(i) .ne. 1) then
                              vtyp(j+nco) = 1
                              vwds(j+nco) = vofset
                              nco    = nco    + 1
                          endif
                          vtyp(j+nco) = 2
                          vval(j+nco) = CUTOFS(j,i)
  650                 continue
                      nco    = nco    + ien
c
c.........CUTTER/DISPLY,SHANK,CUTTER-HOLDER
c
                      if (i .eq. 2) then
                          nco    = nco    + 1
                          vtyp(nco) = 1
                          vwds(nco) = vcuttr
                          if (NSHANK .eq. 1) vwds(nco) = vholdr
                      endif
                      call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
                  endif
  900        continue
          endif
c
      else
          ICUCHG = 1
          SMSTAT = 1
          SMSTOP = 3
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  modea (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the MODE command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine modea (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (TAXCD ,KPOSMP(1081)), (NTAXCD,KPOSMP(1290))
      equivalence (INCR  ,KPOSMP(1226))
      equivalence (ABSCOD,KPOSMP(1241)), (INCCOD,KPOSMP(1242))
      equivalence (SPITAX,KPOSMP(1279))
c
      integer*4 TAXCD(3,3),INCR,ABSCOD,INCCOD,NTAXCD(3),SPITAX
c
      equivalence (TAXVL ,POSMAP(1173)), (ABSCDV,POSMAP(1285))
      equivalence (INCCDV,POSMAP(1286)), (SPIVEC,POSMAP(3583))
c
      real*8 TAXVL(3,3),ABSCDV,INCCDV,SPIVEC(3)
c
      integer*4 i,inc
      integer*4 vmode,vwds(3),vtyp(3),vprm(3),von,voff,vincr,vtool
c
      real*8 vval(3)
c
      character*80 vtxt
c
      data vincr /66/, vmode /1003/, von /71/, voff /72/
      data vprm /33,41,37/, vtool /617/
c
c...Setup basic command
c
      vtyp(1) = 1
      vwds(1) = vmode
c
c...MODE/INCR
c
      call fndreg (kregs,gvals,knreg,ABSCOD,ABSCDV,inc)
      if (inc .ne. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vtyp(2) = 1
              vwds(2) = vincr
              vtyp(3) = 1
              vwds(3) = voff
              call ptdf_aptstmt (3,vtyp,vwds,vval,vtxt)
          endif
          call delreg (kregs,gvals,knreg,inc)
          INCR   = 1
      else
          call fndreg (kregs,gvals,knreg,INCCOD,INCCDV,inc)
          if (inc .ne. 0) then
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vtyp(2) = 1
                  vwds(2) = vincr
                  vtyp(3) = 1
                  vwds(3) = von
                  call ptdf_aptstmt (3,vtyp,vwds,vval,vtxt)
              endif
              call delreg (kregs,gvals,knreg,inc)
              INCR   = 2
          endif
      endif
c
c...MODE/TOOL
c
      vtyp(2) = 1
      vwds(2) = vtool
      do 200 i=1,3,1
          call fndreg (kregs,gvals,knreg,TAXCD(1,i),TAXVL(1,i),inc)
          if (inc .ne. 0) then
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vtyp(3) = 1
                  vwds(3) = vprm(i)
                  call ptdf_aptstmt (3,vtyp,vwds,vval,vtxt)
              endif
              call delreg (kregs,gvals,knreg,inc)
              if (NTAXCD(i) .ge. 2) then
                  call fndreg (kregs,gvals,knreg,TAXCD(2,i),TAXVL(2,i),
     1                         inc)
                  if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
              endif
              if (NTAXCD(i) .ge. 3) then
                  call fndreg (kregs,gvals,knreg,TAXCD(3,i),TAXVL(3,i),
     1                         inc)
                  if (inc .ne. 0) call delreg (kregs,gvals,knreg,inc)
              endif
              SPITAX = i
              SPIVEC(1) = 0.
              SPIVEC(2) = 0.
              SPIVEC(3) = 0.
              SPIVEC(4-i) = 1.
          endif
  200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  postna (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the POSTN/n command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine postna (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (PSTNRG,KPOSMP(3381))
c
      integer*4 PSTNRG(15)
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 inc
      integer*4 vpostn,vwds(2),vtyp(2)
c
      real*8 vval(2)
c
      character*80 vtxt
c
      data vpostn /1024/
c
c...Check for POSTN code
c
      call fndreg (kregs,gvals,knreg,PSTNRG(11),DUMMY,inc)
      if (inc .ne. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vtyp(1) = 1
              vwds(1) = vpostn
              vtyp(2) = 2
              vval(2) = gvals(inc)
              call ptdf_aptstmt (2,vtyp,vwds,vval)
          endif
          call delreg (kregs,gvals,knreg,inc,vtxt)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotaba (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the ROTABL/SIZE command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine rotaba (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (RSIZCD,KPOSMP(1328)), (RSIZFL,KPOSMP(1333))
      equivalence (IRTDEF,KPOSMP(1485))
c
      integer*4 RSIZCD(5),RSIZFL,IRTNUM,IRTDEF
c
      equivalence (PI    ,POSMAP(0001)), (DUMMY ,POSMAP(0003))
      equivalence (ROTCRM,POSMAP(1439)), (RSIZCV,POSMAP(1600))
c
      real*8 ROTCRM(4),RSIZCV,PI,DUMMY
c
      integer*4 inc,iax(4),i
      integer*4 vrotab,vsize,vaxis,vwds(5),vtyp(5)
c
      real*8 vval(5)
c
      character*80 vtxt
c
      data iax /-10,-11,-12,-13/
      data vrotab /1026/, vaxis /132/, vsize /196/
c
c...Check for ROTABL/SIZE code
c
      call fndreg (kregs,gvals,knreg,RSIZCD(1),RSIZCV,inc)
c
c...Check for Axes codes
c
      if (inc .ne. 0) then
          call delreg (kregs,gvals,knreg,inc)
          vwds(1) = vrotab
          vtyp(2) = 1
          vwds(2) = vsize
          vtyp(3) = 1
          vwds(3) = vaxis
          do 100 i=1,IRTDEF,1
              call fndreg (kregs,gvals,knreg,RSIZCD(i+1),DUMMY,inc)
              if (inc .ne. 0) then
                  if (PCNV_TYPE .eq. PCNV_APTSRC) then
                      vtyp(4) = 2
                      vval(4) = i
                      vtyp(5) = 2
                      vval(5) = gvals(inc)
                      call ptdf_aptstmt (5,vtyp,vwds,vval,vtxt)
                  endif
                  if (RSIZFL .eq. 1) vval(5) = gvals(inc) * PI * 2.d0
                  if (RSIZFL .eq. 2) vval(5) = gvals(inc) * PI
                  ROTCRM(i) = vval(5) / 360.
                  call delreg (kregs,gvals,knreg,inc)
              endif
  100     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rapida (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the RAPID command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine rapida (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'pted.inc'
      include 'post.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (MOTCOD,KPOSMP(1240))
      equivalence (NRAPCD,KPOSMP(3190)), (RAPCD ,KPOSMP(3191))
      equivalence (IRAP  ,KPOSMP(3199))
c
      integer*4 NRAPCD,RAPCD(5),IRAP,MOTCOD
c
      equivalence (MOTCDV,POSMAP(1284)), (RAPVL ,POSMAP(3577))
c
      real*8 MOTCDV,RAPVL(5)
c
      integer*4 inc,i
c
c...Check for Linear motion code
c
      call fndreg (kregs,gvals,knreg,MOTCOD,MOTCDV,inc)
      if (inc .ne. 0) then
          IRAP   = 0
          if (PCNV_TYPE .ne. PCNV_CONVERT)
     1        call delreg (kregs,gvals,knreg,inc)
      else if (RAPCD(1) .ne. MOTCOD .or. RAPVL(1) .eq. MOTCDV) then
          IRAP   = 0
      endif
c
c...Check for RAPID codes
c
      if (RAPCD(1) .ge. 15 .and. RAPCD(1) .le. 17) RAPCD(1) = -14
      call fndreg (kregs,gvals,knreg,RAPCD(1),RAPVL(1),inc)
      if (inc .ne. 0) then
          IRAP   = 1
          if (PCNV_TYPE .ne. PCNV_CONVERT)
     1        call delreg (kregs,gvals,knreg,inc)
          do 100 i=2,NRAPCD,1
              if (RAPCD(i) .ge. 15 .and. RAPCD(i) .le. 17)
     1            RAPCD(i) = -14
              call fndreg (kregs,gvals,knreg,RAPCD(i),RAPVL(i),inc)
              if (inc .ne. 0 .and. PCNV_TYPE .ne. PCNV_CONVERT)
     1            call delreg (kregs,gvals,knreg,inc)
  100     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  selcta (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the SELCTL command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine selcta (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
      include 'ptedtool.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (TOOLFL,KPOSMP(1804)), (TLCCD ,KPOSMP(1839))
c
      integer*4 TOOLFL(20),TLCCD(20)
c
      equivalence (DUMMY ,POSMAP(0003)), (TL    ,POSMAP(3601))
      equivalence (TLNO  ,POSMAP(3841))
c
      real*8 DUMMY,TLNO(120),TL(120)
c
      integer*4 inc,i,j,k,nco
      integer*4 vwds(10),vtyp(10),vlngth,vselct
c
      real*8 vval(10)
c
      character*80 vtxt
c
      data vlngth /9/, vselct /1056/
c
c...Check for SELCTL register
c
      if (TOOLFL(1) .eq. 1) then
          call fndreg (kregs,gvals,knreg,TLCCD(3),DUMMY,inc)
          if (inc .ne. 0) then
c
c...Format SELCTL command
c
              vtyp(1) = 1
              vwds(1) = vselct
              vtyp(2) = 2
              vval(2) = gvals(inc)
              nco    = 2
c
c...See if tool was loaded before
c
              i      = 1
              j      = 0
              do while (i .le. 120 .and. j .eq. 0 .and. TLNO(i) .ne. 0)
                  if (TLNO(i) .eq. vval(2)) j = i
                  i = i + 1
              enddo
c
c...Tool not previously used
c
              if (i .le. 120 .and. j .eq. 0) then
                  j = i
                  k = 0
                  TL(i) = 0.d0
                  do while (k .le. NTOOL .and. TL(i) .eq. 0.)
                      if (vval(2) .eq. TLNUM(k)) TL(i) = TLLEN(k)
                      k = k + 1
                  enddo
              endif
c
c...Format tool length
c
              if (TL(j) .ne. 0.d0) then
                  nco    = nco    + 1
                  vtyp(nco) = 1
                  vwds(nco)= vlngth
                  nco    = nco    + 1
                  vtyp(nco) = 2
                  vval(nco) = TL(j)
              endif
c
c...Output SELCTL statement
c
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
              endif
              call delreg (kregs,gvals,knreg,inc)
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
c   SUBROUTINE:  spinda (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the SPINDL command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine spinda (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (NSPRG ,KPOSMP(3101)), (SPNDCD,KPOSMP(3105))
      equivalence (SPNRCD,KPOSMP(3115)), (SPNFCD,KPOSMP(3118))
      equivalence (SPNOCD,KPOSMP(3121)), (COOLCD,KPOSMP(3128))
      equivalence (SPCOCD,KPOSMP(3132)), (SPNDIR,KPOSMP(3141))
      equivalence (SPNMOD,KPOSMP(3142)), (SPNRNG,KPOSMP(3143))
      equivalence (SPNSCD,KPOSMP(3144)), (ICOLSW,KPOSMP(3146))
c
      integer*4 COOLCD(4),NSPRG,SPNDCD(4),SPNRCD(3),SPNSCD(3),SPNOCD(2),
     1          SPCOCD(6),SPNDIR,SPNMOD,SPNRNG,SPNFCD(3),ICOLSW
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (SPNDVL,POSMAP(3007)), (SPNRVL,POSMAP(3016))
      equivalence (SPNFVL,POSMAP(3019)), (SPNOVL,POSMAP(3022))
      equivalence (COOLVL,POSMAP(3294)), (SPCOVL,POSMAP(3298))
      equivalence (RPM   ,POSMAP(3307)), (SFM   ,POSMAP(3308))
c
      real*8 COOLVL(4),SPNDVL(4),SPNRVL(3),SPNFVL(3),SPNOVL(2),
     1       SPCOVL(6),RPM,SFM,DUMMY
c
      integer*4 i,inc,sdir,icool,irg,idir(6),nco
      integer*4 vcool,vwds(6),vtyp(6),vprm(4),vonoff(2),vlock,vmod(2),
     1          vcol(6),vrng(3),vspind
c
      real*8 vval(6),sped
c
      character*80 vtxt
c
      data idir /1,2,1,2,1,2/
c
      data vcol /90,90,89,89,1011,1011/, vcool /1030/, vlock /114/
      data vmod /78,115/, vonoff /71,72/, vprm /60,59,72,246/
      data vrng /63,61,62/, vspind /1031/
c
c...Setup basic command
c
      vtyp(1) = 1
      vwds(1) = vspind
c
c...SPINDL/OFF codes
c
      do 100 i=3,4,1
          call fndreg (kregs,gvals,knreg,SPNDCD(i),SPNDVL(i),inc)
          if (inc .ne. 0) then
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vtyp(2) = 1
                  vwds(2) = vprm(i)
                  call ptdf_aptstmt (2,vtyp,vwds,vval,vtxt)
              else
                  RPM    = 0
                  SMSTAT = 1
              endif
              call delreg (kregs,gvals,knreg,inc)
              go to 8000
          endif
  100 continue
c
c...SPINDL/ON codes
c
      sdir   = 0
      icool  = 0
      sped   = 0.
      irg    = 0
      do 200 i=1,2,1
          call fndreg (kregs,gvals,knreg,SPNDCD(i),SPNDVL(i),inc)
          if (inc .ne. 0) then
              sdir = i
              call delreg (kregs,gvals,knreg,inc)
          endif
  200 continue
c
c......SPINDL/COOLNT codes
c
      if (sdir .eq. 0) then
          do 300 i=1,6,1
              call fndreg (kregs,gvals,knreg,SPCOCD(i),SPCOVL(i),inc)
              if (inc .ne. 0) then
                  sdir   = idir(i)
                  icool  = vcol(i)
                  call delreg (kregs,gvals,knreg,inc)
              endif
  300     continue
      endif
c
c......Spindle mode code
c
      do 400 i=1,2,1
          call fndreg (kregs,gvals,knreg,SPNFCD(i),SPNFVL(i),inc)
          if (inc .ne. 0) then
              SPNMOD = i
              call delreg (kregs,gvals,knreg,inc)
          endif
  400 continue
c
c......Spindle speed register
c
      call fndreg (kregs,gvals,knreg,SPNSCD(SPNMOD),DUMMY,inc)
      if (inc .ne. 0) then
          sped   = gvals(inc)
          call delreg (kregs,gvals,knreg,inc)
      endif
c
c......Spindle range
c.........Low Range
c
      if (NSPRG .eq. 2 .or. NSPRG .eq. 3) then
          call fndreg (kregs,gvals,knreg,SPNRCD(1),SPNRVL(1),inc)
          if (inc .ne. 0) then
              irg    = 1
              call delreg (kregs,gvals,knreg,inc)
          endif
      endif
c
c.........Medium range
c
      if (NSPRG .eq. 1 .or. NSPRG .eq. 3) then
          call fndreg (kregs,gvals,knreg,SPNRCD(2),SPNRVL(2),inc)
          if (inc .ne. 0) then
              irg    = 2
              call delreg (kregs,gvals,knreg,inc)
          endif
      endif
c
c.........High range
c
      if (NSPRG .eq. 2 .or. NSPRG .eq. 3) then
          call fndreg (kregs,gvals,knreg,SPNRCD(3),SPNRVL(3),inc)
          if (inc .ne. 0) then
              irg    = 3
              call delreg (kregs,gvals,knreg,inc)
          endif
      endif
c
c......Build command
c
      nco    = 1
      if (sped .ne. 0) then
          vtyp(2) = 1
          vwds(2) = vmod(SPNMOD)
          vtyp(3) = 2
          vval(3) = sped
          nco    = 3
          if (SPNMOD .eq. 1) then
              RPM    = sped
          else
              SFM    = sped
          endif
      endif
c
      if (sdir .ne. 0) then
          nco    = nco    + 1
          vtyp(nco) = 1
          vwds(nco) = vprm(sdir)
          SPNDIR = sdir
      endif
c
      if (irg .ne. 0) then
          nco    = nco    + 1
          vtyp(nco) = 1
          vwds(nco) = vrng(irg)
          SPNRNG = irg
      endif
c
c......Output command
c
      if (nco .gt. 1) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              call ptdf_aptstmt (nco,vtyp,vwds,vval,vtxt)
          else
              SMSTAT = 1
          endif
      endif
c
c......Output COOLNT command
c
      if (icool .ne. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vwds(1) = vcool
              vtyp(1) = 1
              vwds(2) = icool
              call ptdf_aptstmt (2,vtyp,vwds,vval,vtxt)
              vwds(1) = vspind
          endif
          ICOLSW = 1
      endif
c
c...SPINDL/LOCK codes
c
      do 600 i=1,2,1
          call fndreg (kregs,gvals,knreg,SPNOCD(i),SPNOVL(i),inc)
          if (inc .ne. 0) then
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vtyp(2) = 1
                  vwds(2) = vlock
                  vtyp(3) = 1
                  vwds(3) = vonoff(i)
                  call ptdf_aptstmt (3,vtyp,vwds,vval,vtxt)
              endif
              call delreg (kregs,gvals,knreg,inc)
          endif
  600 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  toolna (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the TOOLNO command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine toolna (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (TLOCD ,KPOSMP(1860)), (TLOSGN,KPOSMP(1869))
c
      integer*4 TLOCD(8),TLOSGN
c
      equivalence (DUMMY ,POSMAP(0003)), (TLOVL ,POSMAP(3983))
c
      real*8 TLOVL(5),DUMMY
c
      integer*4 i,inc,nc,ierr,imod,ireg
      integer*4 vtlno,vwds(5),vadj,vprm(2),vtyp(5),vminus
c
      real*8 vval(5)
c
      character*80 vtxt,msg
c
      data vadj /159/, vtlno /1025/, vprm /71,72/, vminus /10/
c
c...Setup basic command
c
      vtyp(1) = 1
      vwds(1) = vtlno
      vtyp(2) = 1
      vwds(2) = vadj
c
c...TOOLNO/ADJUST,ON/OFF
c
      do 100 i=2,3,1
          call fndreg (kregs,gvals,knreg,TLOCD(i),TLOVL(i),inc)
          if (inc .ne. 0) then
              imod   = 3 - i
              ireg   = 0
              vtyp(3) = 1
              vwds(3) = vprm(i-1)
              nc     = 3
              call delreg (kregs,gvals,knreg,inc)
              call fndreg (kregs,gvals,knreg,TLOCD(1),TLOVL(1),inc)
              if (inc .ne. 0) then
                  if (i .eq. 2) then
                      vtyp(3) = 2
                      vval(3) = gvals(inc)
                      if (vval(3) .lt. 0. .and. TLOSGN .eq. 1) then
                          imod   = 2
                          vval(3) = -vval(3)
                          nc     = nc     + 1
                          vtyp(nc) = 1
                          vwds(nc) = vminus
                      endif
                      ireg = vval(3)
                  endif
                  call delreg (kregs,gvals,knreg,inc)
              endif
c
c......Output APT source record
c
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
c
c......Output Simulation file record
c
              else if (PCNV_TYPE .eq. PCNV_SIMUL) then
                  call simofs (-1,-1,imod,ireg,msg,ierr)
              endif
          endif
  100 continue
c
c...TOOLNO/ADJUST,n
c
      call fndreg (kregs,gvals,knreg,TLOCD(1),DUMMY,inc)
      if (inc .ne. 0) then
c
c......Output APT source record
c
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              nc     = 3
              vtyp(nc) = 2
              vval(nc) = gvals(inc)
              if (vval(nc) .lt. 0. .and. TLOSGN .eq. 1) then
                  vval(nc) = -vval(nc)
                  nc     = nc     + 1
                  vtyp(nc) = 1
                  vwds(nc) = vminus
              endif
              call ptdf_aptstmt (nc,vtyp,vwds,vval,vtxt)
c
c......Output Simulation file record
c
          else if (PCNV_TYPE .eq. PCNV_SIMUL) then
              imod   = 1
              ireg   = vval(nc)
              if (vval(nc) .lt. 0. .and. TLOSGN .eq. 1) then
                  imod   = 2
                  ireg   = -ireg
              endif
              call simofs (imod,ireg,-1,-1,msg,ierr)
          endif
          call delreg (kregs,gvals,knreg,inc)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  unitsa (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the UNITS command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine unitsa (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (IUNIT ,KPOSMP(0087))
      equivalence (MCHOPT,KPOSMP(0308)), (UNTCOD,KPOSMP(1078))
c
      integer*4 UNTCOD(2),MCHOPT(20),IUNIT
c
      equivalence (UNTCDV,POSMAP(1170))
c
      real*8 UNTCDV(2)
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 i,inc,ierr,isav,ipc,strlen1,nc,inum
      integer*4 vunit,vwds(10),vprm(2),vtyp(10),vmach,vopt
c
      real*8 vval(10),rval
c
      character*8 lval
      character*80 vtxt,msg
c
      equivalence (rval,lval)
c
      data vmach /1015/, vopt /144/, vprm /303,301/
      data vunit /841/
c
c...Check for UNITS codes
c
      isav   = MCHOPT(2)
      do 100 i=1,2,1
          call fndreg (kregs,gvals,knreg,UNTCOD(i),UNTCDV(i),inc)
c
c...Output MACHIN statement
c
          if (inc .ne. 0) then
              MCHOPT(1) = i
              MCHOPT(2) = i
c
c......Change default units
c
              if (IUNIT .eq. 1) then
                  if (MCHOPT(2) .eq. 2) call metini (2)
              else
                  if (MCHOPT(2) .eq. 1) call metini (1)
              endif
              IUNIT = MCHOPT(2)
c
c......Output APT Source statement
c
              if (PCNV_TYPE .eq. PCNV_APTSRC) then
                  vtyp(1) = 1
                  vwds(1) = vmach
                  nc     = strlen1(LMNAME)
                  call ctoi (LMNAME(1:nc),inum,ierr)
                  if (ierr .eq. 0) then
                      lval   = 'PWORKS'
                      vtyp(3) = 2
                      vval(3) = inum
                      ipc    = 3
                  else
                      lval   = LMNAME(1:nc)
                      ipc    = 2
                  endif
                  vtyp(2) = 1
                  vwds(2) = -1
                  vval(2) = rval
                  vtyp(ipc+1) = 1
                  vwds(ipc+1) = vopt
                  vtyp(ipc+2) = 2
                  vval(ipc+2) = 1.
                  vtyp(ipc+3) = 2
                  vval(ipc+3) = i
                  vtyp(ipc+4) = 2
                  vval(ipc+4) = 2.
                  vtyp(ipc+5) = 2
                  vval(ipc+5) = i
                  call ptdf_aptstmt (ipc+5,vtyp,vwds,vval,vtxt)
                  vtyp(1) = 1
                  vwds(1) = vunit
                  vtyp(2) = 1
                  vwds(2) = vprm(i)
                  call ptdf_aptstmt (2,vtyp,vwds,vval,vtxt)
c
c......Output Simulate header record
c......With new Units setting
c
              else if (PCNV_TYPE .eq. PCNV_SIMUL) then
                  if (MCHOPT(2) .ne. isav) call simhdr (msg,ierr)
              endif
c
c...Remove register from block
c
              call delreg (kregs,gvals,knreg,inc)
              go to 8000
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
c   SUBROUTINE:  opstpa (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the OPSTOP command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine opstpa (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (OPSTCD,KPOSMP(1075))
c
      integer*4 OPSTCD
c
      equivalence (OPSTVL,POSMAP(1168))
c
      real*8 OPSTVL
c
      integer*4 inc
      integer*4 vopstp,vwds(2),vtyp(2)
c
      real*8 vval(2)
c
      character*80 vtxt
c
      data vopstp /3/
c
c...Check for OPSTOP code
c
      call fndreg (kregs,gvals,knreg,OPSTCD,OPSTVL,inc)
      if (inc .ne. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vtyp(1) = 1
              vwds(1) = vopstp
              call ptdf_aptstmt (1,vtyp,vwds,vval,vtxt)
          else
              SMSTOP = 2
              SMSTAT = 1
          endif
          call delreg (kregs,gvals,knreg,inc)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stopa (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the STOP command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine stopa (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (STOPCD,KPOSMP(1076))
c
      integer*4 STOPCD
c
      equivalence (STOPVL,POSMAP(1169))
c
      real*8 STOPVL
c
      integer*4 inc
      integer*4 vstop,vwds(2),vtyp(2)
c
      real*8 vval(2)
c
      character*80 vtxt
c
      data vstop /2/
c
c...Check for STOP code
c
      call fndreg (kregs,gvals,knreg,STOPCD,STOPVL,inc)
      if (inc .ne. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vtyp(1) = 1
              vwds(1) = vstop
              call ptdf_aptstmt (1,vtyp,vwds,vval,vtxt)
          else
              SMSTOP = 1
              SMSTAT = 1
          endif
          call delreg (kregs,gvals,knreg,inc)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rewnda (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the REWIND command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine rewnda (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (REWCOD,KPOSMP(1080))
c
      integer*4 REWCOD
c
      equivalence (REWCDV,POSMAP(1172))
c
      real*8 REWCDV
c
      integer*4 inc
      integer*4 vrewnd,vwds(2),vtyp(2)
c
      real*8 vval(2)
c
      character*80 vtxt
c
      data vrewnd /1006/
c
c...Check for REWIND code
c
      call fndreg (kregs,gvals,knreg,REWCOD,REWCDV,inc)
      if (inc .ne. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vtyp(1) = 1
              vwds(1) = vrewnd
              call ptdf_aptstmt (1,vtyp,vwds,vval,vtxt)
          endif
          call delreg (kregs,gvals,knreg,inc)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  finia (kregs,gvals,knreg)
c
c   FUNCTION: This routine handles the FINI command.
c
c   INPUT:  kregs    I*4  Dn  - List of registers in current block.
c
c           gvals    R*8  Dn  - List of register values.
c
c           knreg    I*4  D1  - Number of registers in list.
c
c   OUTPUT: kregs    I*4  Dn  - Updated list of registers.
c
c           gvals    R*8  Dn  - Updated list of register values.
c
c           knreg    I*4  D1  - Updated number of registers.
c
c***********************************************************************
c
      subroutine finia (kregs,gvals,knreg)
c
      include 'menu.inc'
      include 'post.inc'
      include 'pted.inc'
c
      integer*4 kregs(MAXFMT),knreg
c
      real*8 gvals(MAXFMT)
c
      equivalence (FINCOD,KPOSMP(1105))
c
      integer*4 FINCOD
c
      equivalence (FINCDV,POSMAP(1203))
c
      real*8 FINCDV
c
      integer*4 inc
      integer*4 vfini,vwds(2),vtyp(2)
c
      real*8 vval(2)
c
      character*80 vtxt
c
      data vfini /4012/
c
c...Check for REWIND code
c
      call fndreg (kregs,gvals,knreg,FINCOD,FINCDV,inc)
      if (inc .ne. 0) then
          if (PCNV_TYPE .eq. PCNV_APTSRC) then
              vtyp(1) = 1
              vwds(1) = vfini
              call ptdf_aptstmt (1,vtyp,vwds,vval,vtxt)
          endif
          call delreg (kregs,gvals,knreg,inc)
      endif
c
c...End of routine
c
 8000 return
      end
