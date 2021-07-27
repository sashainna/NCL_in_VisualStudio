C*********************************************************************
C*    NAME         :  plotm.f
C*       CONTAINS:
C*               plotm (buf,penup)
C*               gtrafl (trafl)
C*               strafl (trafl)
C*               gettra (tmx,imx)
C*               dismot (kst,ken,gpt)
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*        plotm.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*        08/17/15 , 17:49:06
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine plotm (buf)
c*       plots motion commands as they are generated.
C*    PARAMETERS   
C*       INPUT  : 
C*                 buf contains the x,y,z,i,j,k of the point.
C*                 penup is true if the move is to be made without
C*                    drawing.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine plotm (buf,penup)
c
      include 'com8a.com'
      include 'cutter.com'
      include 'wrksys.com'
c
      logical penup
c
      real*8 buf(9)
c
      integer*4 i,idc(3),icfl(10),symkey(3)
c
      real*4 asc(320)
      real*8 lbuf(9),lastmx,lastmy,lastmz,cbuf(20)
c
      character*80 sym,ssym,hsym
c
      equivalence (sc,asc)
      equivalence (lastmx,sc(171)), (lastmy,sc(172))
      equivalence (lastmz,sc(173))
      integer*2 i2v3, i2v4
      data i2v3 /3/, i2v4 /4/ 
c
c...Plot motion in NCL-VX mode.
c
      if (ifl(322).eq.1) then
        call ncl_vx_plotm (sc(171), buf)
        lastmx = buf(1)
        lastmy = buf(2)
        lastmz = buf(3)
        goto 99999
      endif
c
c...Do not plot motion in Batch or NCL-VT modes
c
      if (ifl(35).eq.1 .or. (ifl(35) .eq. 2 .and. ifl(350) .eq. 0))
     1        goto 99999

      do 100,i=1,9,1
          lbuf(i) = buf(i)
  100 continue
c
c...Reset tracut matrix if requested
c
      if (itrafl .eq. 0 .and. ifl(73) .eq. 1) then
          call conent (lbuf,sc(93),i2v3)
          call conent (lbuf(4),sc(93),i2v4)
          call conent (lbuf(7),sc(93),i2v4)
      endif
c
c...Perform metric conversion
c
      if (ifl(264).eq.1) then
          do 150,i=1,3,1
             lbuf(i) = lbuf(i) / 25.4
  150     continue
      endif
c
c...Apply working coordinate system matrix
c
      if (lwrk) then
          call conent(lbuf, wrkmx, i2v3)
          call conent(lbuf(4), wrkmx, i2v4)
          call conent(lbuf(7), wrkmx, i2v4)
          if (wrkscl.ne.1.0d0) then
              call unitizevc(lbuf(4))
              call unitizevc(lbuf(7))
          endif
      endif
c
c...Start new motion segment
c
cc      if (ifl(270) .lt. 0) then
cc          call motbgn
cc          ifl(270) = 0
cc      endif
c
c...Pen is down so plot motion
c...First move the last motion position
c...then draw a line to the new motion position
c
      if (.not. penup) then
c
c......Determine if cutter needs to be displayed
c
          idc(1) = 0
          if (ifl(128).eq.1 .and. ifl(129).gt.0) then
              if (ifl(130) .eq. 0 .or. ifl(130) .ge. ifl(129)) then
                  ifl(130) = 0
                  idc(1) = icutfl(1)
                  if (idc(1) .eq. 0) idc(1) = 1
              endif
              ifl(130) = ifl(130) + 1
          endif

          if (icutfl(3) .eq. 1 .or. icutfl(3) .eq. -1) then
              idc(1) = icutfl(1)
              if (idc(1) .eq. 0) idc(1) = 1
          endif
c
c......Store actual cutter parameters
c
          call gdscut (cbuf,sym,ssym,hsym,symkey,icfl)
c
c......Plot this motion
c
          idc(2) = icutfl(5)
          idc(3) = icutfl(6)
          call nclf_cutter_set(cbuf,idc,icutfl(2),cutsym(1),cutsym(2),
     1                         cutsym(3),cutkey)
          call pltmot (sc(171),lbuf,lbuf(7))
      endif
c
c...Save this new position as the starting coordinates
c...of the next move
c
      lastmx = lbuf(1)
      lastmy = lbuf(2)
      lastmz = lbuf(3)
cc      call uw_newline_flush()
99999 continue
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtrafl (trafl)
c
c   FUNCTION:  Interface to the C routines which returns the "Apply
c              Tracut" flag for motion display.
c
c   INPUT:  none.
c
c   OUTPUT: trafl   I*4  D1  -  1 = TRACUT should be applied to dis-
c                               played motion.
c
c***********************************************************************
c
      subroutine gtrafl (trafl)
c
      include 'com.com'
      include 'cutter.com'
c
      integer*4 trafl
c
c...Return the Apply Tracut flag
c
      trafl = itrafl
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  strafl (trafl)
c
c   FUNCTION:  Interface to the C routines which sets the "Apply
c              Tracut" flag for motion display.
c
c   INPUT:  trafl   I*4  D1  -  1 = TRACUT should be applied to dis-
c                               played motion.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine strafl (trafl)
c
      include 'com.com'
      include 'cutter.com'
c
      integer*4 trafl
c
c...Set the Apply Tracut flag
c
      itrafl = trafl
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gettra (tmx,imx)
c
c   FUNCTION:  Interface to the C routines which returns the TRACUT
c              and inverse TRACUT matrices.
c
c   INPUT:  none.
c
c   OUTPUT: tmx     R*8  D12 -  TRACUT matrix.
c
c           imx     R*8  D12 -  Inverse of TRACUT matrix.
c
c***********************************************************************
c
      subroutine gettra (tmx,imx)
c
      include 'com8a.com'
c
      real*8 tmx(12),imx(12)
c
c...Return TRACUT matrices
c
      do 100 i=1,12,1
          tmx(i) = sc(40+i)
          imx(i) = sc(92+i)
  100 continue
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dismot (kst,ken,gpt)
c*       Plots motion after it has been generated by reading the clfile.
C*    PARAMETERS   
C*       INPUT  : 
C*          kst     = Starting clfile record of this motion.
C*          ken     = Ending clfile record of this motion.
C*          gpt     = Last programmed tool position and axis vector.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine dismot (kst,ken,gpt)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*2 i2v3,i2v4
      integer*4 kst(2),ken(2)
c
      real*8 gpt(6)
c
      integer*2 iclf,np,npt,ifl129,icfl3,jerr,iflg,iflg1,ifl73,
     1          trasv
      integer*4 irec(2),iclw(6),jclbuf(840),it,im,id
c
      logical done,lrsav,ldid,first,rpsav,isav,lsttr
c
      real*8 tpt(6),dclbuf(640),spt(9),opt(9),tmx(12),vmx(12),fed
c
      equivalence (dclbuf,jclbuf)
c
      data i2v3 /3/, i2v4 /4/
c
c...Initialize routine
c
      do 100 i=1,3,1
          tpt(i) = gpt(i)
          tpt(i+3) = sc(i+3)
  100 continue
      if (ifl(73) .eq. 1) then
          call conent (tpt,sc(41),i2v3)
          call conent (tpt(4),sc(41),i2v4)
      endif
      npt  = 3
      if (ifl(82) .eq. 1) npt = 6
      spt(4) = tpt(4)
      spt(5) = tpt(5)
      spt(6) = tpt(6)
      ldid  = .false.
      first = .true.
      lsttr = .false.
c
      lrsav = rpfron
      ifl129 = ifl(129)
      icfl3  = icutfl(3)
      ifl73  = -1
cc	lcumov = .true.
      if (.not. lcumov) then
          ifl(129) = 0
          if (icutfl(3) .eq. 1) icutfl(3) = -1
      endif
c
c...Main loop to read clfile
c
      done = .false.
c      irec = kst
      call ncl_setptr(kst,irec) 
      do while (.not. done)
c
c...Read clfile
c
          call clread (iclf,irec,iclw,dclbuf,jerr)
          if (jerr .eq. 1) then
              done = .true.
c
c...Motion record
c
          else if (iclw(3) .eq. 5000 .or. iclw(3) .eq. 5200) then
              np     = npt
              if (iclw(3) .eq. 5200) np = 21
              do 400 i=1,iclw(5),np
                  do 300 j=1,npt,1
                      spt(j) = dclbuf(i+j-1)
  300             continue
c
c......Get tool axis vector
c
                  if (np .eq. 3) then
                      spt(4) = tpt(4)
                      spt(5) = tpt(5)
                      spt(6) = tpt(6)
                  endif
c
c......Get forward vector
c
                  if (np .eq. 21) then
                      spt(7) = dclbuf(i+6)
                      spt(8) = dclbuf(i+7)
                      spt(9) = dclbuf(i+8)
                  else
                      call vcmnvc (dclbuf(i),tpt,spt(7))
                      call unitvc (spt(7),spt(7))
                  endif
c
c......Mark start of motion
c
cc                  ifl(270) = 0
                  if (first .and. ifl(216) .eq. 0) then
                      call motbgn (kst,gpt)
                      first = .false.
                  endif
c
c......Ouput buffered motion
c......and save current motion
c......if we aren't displaying the cutter
c......until the last move
c
                  if (.not. lcumov) then
                      isav   = rpfron
                      if (ldid) then
                          rpfron = rpsav
                          call plotm (opt,.false.)
                      endif
                      do 500 j=1,9,1
                          opt(j) = spt(j)
  500                 continue
                      ldid = .true.
                      rpsav = isav
                      rpfron = isav
c
c......Plot the motion
c......with cutter display
c
                  else
                      call plotm (spt,.false.)
                  endif
  400         continue
              rpfron = .false.
              lsttr  = .false.
c
c...ISN record
c
          else if (iclw(3) .eq. 1000) then
c
c......Flush motion if held back
c
              if (ldid) then
                  isav   = rpfron
                  rpfron = rpsav
                  if (lsttr) then
                      trasv  = ifl(73)
                      ifl(73) = 1
                  endif
                  call plotm (opt,.false.)
                  if (lsttr) ifl(73) = trasv
                  rpsav = isav
                  rpfron = isav
                  ldid = .false.
              endif
              call motisn (jclbuf)
c
c...Post-processor variable
c
          else if (iclw(3) .eq. 2000) then
              RPFRON = .false.
c
c......COOLNT
c
              if (iclw(4) .eq. 1030) then
                  call clcool (dclbuf,iclw(5),CCOOL)
                  call motcln (CCOOL)
c
c......CUTCOM
c
              else if (iclw(4) .eq. 1007) then
                  call clcutc (dclbuf,iclw(5),it,im,id)
                  if (it .eq. 1) then
                      ICCMOD = im
                      ICCDIR = id
                      call motccm (ICCMOD,ICCDIR)
                  endif
c
c......FEDRAT
c
              else if (iclw(4) .eq. 1009) then
                  fed = CFEED(FITYP)
                  call clfed (dclbuf,iclw(5),FITYP,fed)
                  CFEED(FITYP) = fed
c
c......LOADTL
c
              else if (iclw(4) .eq. 1055) then
                  call clldtl (dclbuf,iclw(5),CTLNO,CTLEN)
                  call mottln (CTLNO,CTLEN)
c
c......RAPID
c
              else if (iclw(4) .eq. 5) then
                  RPFRON = .true.
c
c......SPINDL
c
              else if (iclw(4) .eq. 1031) then
                  call clspn (dclbuf,iclw(5),CRPM,CSPDIR)
                  call motspn (CSPDIR,CRPM)
              endif
c
c...Some motion commands (COPY)
c...will output TRACUT records
c...We need to recognize these
c
          else if (iclw(3) .eq. 7000) then
              if (ifl73 .eq. -1) then
                  call gettra (tmx,vmx)
                  ifl73  = ifl(73)
              endif
              if (iclw(4) .eq. 1) then
                  call conv8_8 (dclbuf(1),sc(41),12)
                  call invmx (sc(41),sc(93))
                  ifl(73) = 1
                  lsttr  = .true.
              else
                  ifl(73) = 0
              endif
          endif
c
c...Check for end of clfile range
c
c          if (irec .eq. ken .or. irec .eq. 0) done = .true.
          call ncl_eqlptr(irec,ken,iflg)
          call ncl_tstptr(irec,iflg1)
          if (iflg .eq. 1 .or. iflg1 .eq. 0) done = .true.
      enddo
c
c...Display last motion with cutter
c...when holding back cutter display
c
      ifl(129)  = ifl129
      icutfl(3) = icfl3
      if (ldid) then
          rpfron = rpsav
          if (lsttr) then
              trasv  = ifl(73)
              ifl(73) = 1
          endif
          call plotm (opt,.false.)
          if (lsttr) ifl(73) = trasv
      endif
c
c...Mark end of motion
c
cc      if (.not. first .and. (ifl(347) .lt. 3 .or. lrsav))
      if (.not. first) call motend (ken)
c
c...End of routine
c
 8000 rpfron = lrsav
      if (ifl73 .ne. -1) then
          ifl(73) = ifl73
          call conv8_8 (tmx,sc(41),12)
          call conv8_8 (vmx,sc(93),12)
      endif
      return
      end
