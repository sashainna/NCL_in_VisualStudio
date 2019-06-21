c
c***********************************************************************
c
c   FILE NAME:  mocntl
c   CONTAINS:
c               mocntl      mocntl1     fwdset      nmlset      pre_clrchk
c               pre_cross   pre_retrct  pre_linchk  pre_movlin
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mocntl.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:35:00
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  mocntl
c
c   FUNCTION:  Determines which motion controlling routine to call based
c              on the current machine type.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mocntl
c
      include 'post.inc'
c
      equivalence (MXPT  ,KPOSMP(0060)), (SGMACT,KPOSMP(0996))
      equivalence (MACHTP,KPOSMP(1201)), (HLDFLG,KPOSMP(1622))
      equivalence (IRAP  ,KPOSMP(3199)), (IRAPDO,KPOSMP(3220))
c
      integer*4 HLDFLG,IRAP,IRAPDO(8),MXPT,MACHTP,SGMACT(2)
c
      equivalence (RCYCDO,POSMAP(2931)), (CYCPSV,POSMAP(2951))
      equivalence (PFEED ,POSMAP(3540))
c
      real*8 RCYCDO(20),CYCPSV(10),PFEED(4)
c
c...Call Stringer Drill controlling routine
c
      if (MACHTP .eq. 5 .and. SGMACT(1) .ne. 1) then
          call smdctl
c
c...Call standard motion control routine
c
      else
          call mocntl1
      endif
c
c...End of routine
c
 8000 MXPT   = 0
c
c......Reset rapid mode
c
      if (IRAP .ne. 0 .and. IRAPDO(3) .ne. 1 .and. HLDFLG .ne. 1)
     1        call raprst
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mocntl1
c
c   FUNCTION:  This is the controlling routine for Motion (Type 5000)
c              records.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mocntl1
c
      include 'post.inc'
c
      equivalence (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MULTAX,KPOSMP(0056))
      equivalence (NPT   ,KPOSMP(0059)), (MXPT  ,KPOSMP(0060))
      equivalence (LOOKPT,KPOSMP(0061))
      equivalence (KERRSV,KPOSMP(0109)), (LIMERR,KPOSMP(0110))
      equivalence (ICLIPS,KPOSMP(0112)), (LINTSW,KPOSMP(0114))
      equivalence (CYCCOD,KPOSMP(0211))
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
      equivalence (ICYCSV,KPOSMP(0291)), (IZIGON,KPOSMP(0370))
      equivalence (IRETBL,KPOSMP(0817)), (IFWDFL,KPOSMP(0832))
      equivalence (IFWSFL,KPOSMP(0833)), (IHDID ,KPOSMP(0851))
      equivalence (XFMLK ,KPOSMP(0941)), (XFMFL ,KPOSMP(0969))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (HLDBCK,KPOSMP(1223)), (ISCIRC,KPOSMP(1238))
      equivalence (LRTRCT,KPOSMP(1278))
      equivalence (MROTTV,KPOSMP(1334)), (IRTRTE,KPOSMP(1343))
      equivalence (LRTRFL,KPOSMP(1348)), (NROT  ,KPOSMP(1366))
      equivalence (IRSRTE,KPOSMP(1375)), (IRTINC,KPOSMP(1461))
      equivalence (IRTDEF,KPOSMP(1485)), (HLDFLG,KPOSMP(1622))
      equivalence (IRFNLS,KPOSMP(1640)), (IRTNLS,KPOSMP(1641))
      equivalence (ACHEAD,KPOSMP(1645))
      equivalence (IMAXFL,KPOSMP(1729)), (IJKROT,KPOSMP(1739))
      equivalence (IRTFRC,KPOSMP(1777)), (IFITYP,KPOSMP(3150))
      equivalence (IRPALT,KPOSMP(3196)), (IRPTAX,KPOSMP(3197))
      equivalence (IRAP  ,KPOSMP(3199)), (ILINPT,KPOSMP(3200))
      equivalence (IRAPDO,KPOSMP(3220))
      equivalence (ICUTDO,KPOSMP(3301)), (INMLFL,KPOSMP(3324))
      equivalence (MODBLD,KPOSMP(3984)), (MDTLIN,KPOSMP(4032))
      equivalence (ALNCUT,KPOSMP(4040))
      equivalence (ALNMOV,KPOSMP(4045)), (ALNMRP,KPOSMP(4047))
      equivalence (SMOKON,KPOSMP(4071))
      equivalence (SMOKAN,KPOSMP(4072)), (LTMODE,KPOSMP(4125))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 ISUBT,MXCL,MULTAX,HLDBCK,HLDFLG,IRAP,IRAPDO(8),ISCIRC,
     1          MXPT,LOOKPT,ICYCSW(5),ICYCDO(15),CYCCOD(20),ICYCSV(5),
     2          IRETBL,IFITYP,MACHTP,IMAXFL,LIMERR,MROTTV,IZIGON,NPT,
     3          MDTLIN,IHDID,ALNCUT,ALNMRP,MODBLD,ALNMOV,LINTSW,
     4          IFWDFL,SMOKAN,MTPDYN,LTMODE,KERRSV,IFWSFL,ICLIPS,
     5          ACHEAD,SMOKON,INMLFL,ICUTDO(15),IRPALT,IRPTAX
      integer*4 IPSTWD(50),XFMFL(20),XFMLK(5),IRTFRC,IRFNLS,IRTNLS(4),
     1          IRTDEF,ILINPT,IRSRTE(3),IRTRTE(2),NROT,LRTRCT,IJKROT,
     2          LRTRFL,IRTINC(4)
c
      equivalence (CLSAV ,POSMAP(0201)), (CPLNFD,POSMAP(0390))
      equivalence (CLPT  ,POSMAP(0491))
      equivalence (VECNXT,POSMAP(1248)), (MCHNUM,POSMAP(1287))
      equivalence (LINAXS,POSMAP(1299)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (VECSAV,POSMAP(1372))
      equivalence (ROTBAS,POSMAP(1435)), (CLSAVS,POSMAP(2180))
      equivalence (LNRTOL,POSMAP(2251)), (ROTBSV,POSMAP(2275))
      equivalence (RCYCDO,POSMAP(2931))
      equivalence (CYCPSV,POSMAP(2951)), (PFEED ,POSMAP(3540))
      equivalence (XFMXVC,POSMAP(4047)), (SMOSTP,POSMAP(4466))
      equivalence (NMLVEC,POSMAP(4492))
      equivalence (NMLSAV,POSMAP(4495)), (FWDVEC,POSMAP(4567))
      equivalence (FWDDIR,POSMAP(4580)), (FWDSAV,POSMAP(4591))
      equivalence (CLPTPP,POSMAP(4605)), (FUZZ4 ,POSMAP(4912))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (ROTNXT,POSMAP(5293))
c
      real*8 CLPT(240),MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),
     1       VECSAV(3),ROTANG(20,2),ROTBAS(4),LNRTOL(3),RCYCDO(20),
     2       CYCPSV(10),PFEED(4),CLPTPP(240),FWDDIR(3),FWDSAV(3),
     3       FWDVEC(3),CLSAV(21),CLSAVS(21),SMOSTP,NMLVEC(3),NMLSAV(3),
     4       XFMXVC(3),VECNXT(3),ROTNXT(20,2),ROTSTO(20,2),FUZZ4,
     5       ROTBSV(4),CPLNFD
c
      integer*4 inc,i,iaxsw(10),iaxcnt,ifl,ierr,iaxfl(20),icysv,ient,
     1          isav,npc,iflg,imrsv,j,ifsv,iofl,ilin,ixfm
c
      real*8 ptout(6,30),ndist,dis,fsv,rfed(30)
c
c...Initialize routine
c
      iaxcnt = 0
      icysv  = ICYCSW(1)
c
c...Loop through CLPT's
c
      inc    = 1
      MXPT   = MXCL
      if (IRAP .ne. 0 .and. ALNMRP .eq. 1) ALNMOV = 1
      do 5000 i=1,MXPT,1
          LOOKPT = i
          IMAXFL = 0
c
c......Automatic transformation blocks
c
          ixfm   = 0
          XFMFL(10) = 0
c
          if (XFMFL(5) .eq. 1 .and. NPT .gt. 3 .and.
     1        ndist(CLPT(inc+3),VECSAV) .gt. 1.e-5) then
              ixfm   = 1
              if (XFMFL(7) .eq. 3) then
                  ixfm   = 4
              else if (XFMFL(7) .eq. 1 .or. XFMFL(8) .eq. 2) then
cc     1            ICYCSW(1) .eq. 1) then
                  ixfm   = 2
              endif
              if (XFMFL(8) .eq. 2 .or. XFMFL(13) .eq. 1) call traoff
              XFMFL(10) = 1
          endif
c
          if (ixfm .eq. 1 .or. ixfm .eq. 4) then
              call copyn (CLPT(inc),MCHNUM(1,2),3)
              call unitvc (CLPT(inc+3),TLVEC)
              call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                     0,2,ierr)
              ifl    = 1
              if (ixfm .eq. 4) ifl = 2
              call trapvv (XFMLK,AXSOUT,XFMXVC,ifl,ierr)
cc              if (isw .ne. 0) call cyc2nd (0)
          endif
c
c......check for clip planes
c
          iaxfl(8) = 0
          npc    = 0
          iofl   = 1
          ifsv   = -1
          if (ICLIPS .eq. 1) then
             call pre_clrchk (inc,ptout,rfed,iaxfl(8),iofl)
             if (iaxfl(8) .eq. 0) then
                go to 4900
             else
                npc = 1
             end if
          end if
  150     if (npc .ne. 0) call copyn (ptout(1,npc),CLPT(inc),NPT)
          if (iaxfl(8) .ne. 0  .and. iofl .gt. 1 .and.
     1            rfed(npc) .ge. 0.) then
              if (rfed(npc) .eq. 0.) then
                  ifsv   = 0
                  call rapset (5,2)
              else
                  ifsv   = IFITYP
                  fsv    = PFEED(1)
                  IFITYP = 1
                  PFEED(1) = rfed(npc)
              endif
          endif
c
c......check if prepst lintol is active
c
          iaxfl(9) = 0
          if (LINTSW .ne. 0 .and. IRAP .eq. 0)
     -           call pre_linchk (inc,iaxfl(9))
  100     if (iaxfl(9) .ne. 0) call pre_movlin (inc,iaxfl(9))
c
c......Setup Machine axes values
c
          if (LTMODE .eq. 2) call copyn (CLPTPP(inc),MCHNUM(1,1),3)
          call copyn (CLPT(inc),MCHNUM(1,2),3)
          if (NPT .eq. 3) then
             call copyn (VECSAV,TLVEC,3)
          else
c
c...vp 12/4/97 make sure TA is unit vector
c
             call unitvc (CLPT(inc+3),TLVEC)
c             call betvec (CLPT(inc+3),VECSAV,a)
c             if (a .gt. .001) call copyn (CLPT(inc+3),TLVEC,3)
          endif
c
c......Setup FWD vectors (now only for US50)
c
          if (MACHTP .eq. 3) then
             call fwdset (FWDDIR,FWDVEC,FWDSAV,i,IFWDFL)
          end if
c
c......Setup PS-normal vectors
c......used for 3-D cutcom
c
           if (ICUTDO(3) .eq. 3) then
               call nmlset (NMLVEC,NMLSAV,i,INMLFL,ICUTDO(11))
           endif
c          inc    = inc    + NPT
c
c......Look ahead for AC head
c
          iaxfl(11) = 0
  180     if (ACHEAD .eq. 1) call aclook (iaxfl(11))
c
c......Calculate rotary angles &
c......Adjusted linear axes
c
          iaxfl(3) = 0
  200     iflg = 1
          if (LNRTOL(1) .gt. 0. .and. ALNMOV .eq. 0 .and.
     1        (IRAP .eq. 0 .or. LNRTOL(2) .ne. 0.)) iflg = 2
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,1,iflg,
     1                 ierr)
c
c......Make sure rotary register limit has not been reached
c
          call rrglmt (LINAXS,ROTANG,AXSOUT)
c
c......Reset tool axis and rotary positions
c......when ROTABL/ATANGL,...,NEXT,CUT is active
c......Make sure rotary axes move in correct direction
c
          if (IRTFRC .eq. 1) then
              call copyn (VECNXT,VECSAV,3)
              call cpyrot (ROTNXT,ROTSTO)
              call setbas (ROTSTO(1,2),ROTBSV)
              if (IRFNLS .eq. 0) then
                  do 220 j=1,IRTDEF,1
                      dis   = ROTANG(IRTINC(j),2) - ROTSTO(IRTINC(j),2)
                      IRTNLS(j) = 0
                      if (dis .gt. FUZZ4) IRTNLS(j) = 1
                      if (dis .lt. -FUZZ4) IRTNLS(j) = 2
  220             continue
                  IRFNLS = 1
              endif
              IRTFRC = 2
          endif
c
c......vp 10-sep-96 fix loop
c......make sure that final point is in limits when
c......LIMERR was active
c
          if (iaxfl(3) .lt. 0 .and. ierr .eq. 7) then
             LIMERR = 0
             KERRSV = 0
          end if
c
c......FROM statement
c
          if (ISUBT .eq. 3) then
              call from (AXSOUT,ifl)
              if (ifl .eq. 1) then
                  call savmot (MCHNUM,LINAXS,TLVEC,ROTANG,AXSOUT)
                  go to 5000
              endif
          endif
c
c......Single pass threading
c
          if (ICYCSW(1) .eq. 4) then
              call thrmot (RCYCDO(1),RCYCDO(2),ICYCDO(4),RCYCDO(5),
     1                     ICYCDO(5))
              icysv = ICYCSW(1)
              go to 5000
          endif
c
c......Canned cycle
c
          iaxfl(1) = 0
c
c.........RAPID programmed in cycle
c.........One shot CYCLE/OFF
c
          if (ICYCSW(1) .ne. 0 .and. IRAP .ne. 0) then
              if (MTPDYN .eq. 1) then
                  if (CYCCOD(15) .eq. 0) then
                      call cycoff (1)
                  else
                      call cycoff (0)
                  endif
                  ICYCSW(4) = 1
c
c.........RAPID cancels lathe cycle
c
              else if (MTPDYN .eq. 2) then
                  call cyloff
              endif
          endif
c
c.........Lathe canned cycle
c
  300     if (MACHTP .eq. 2 .or. (MACHTP .eq. 4 .and. LTMODE .ne. 3))
     1        then
              if (ICYCSW(1) .ne. 0 .or. iaxfl(1) .ne. 0) then
                  call cylctl (iaxfl(1))
                  if (iaxfl(1) .eq. 0) then
                      icysv = ICYCSW(1)
                      go to 5000
                  endif
              endif
c
c.........Milling cycle block
c
          else
              if (ICYCSW(1) .ne. 0 .or. iaxfl(1) .ne. 0) then
                  call cycctl (iaxfl(1))
c
c............Don't output Xform block with retract move
c............Since the tool axis has not changed yet
c
                  if (iaxfl(1) .eq. 1 .and.
     1                (ixfm .eq. 2 .or. ixfm .eq. 4)) ixfm = ixfm + 1
              endif
c
c............Cancel CYCLE/MILL
c
              if (ICYCSW(1) .ne. 0) then
                  if (ICYCDO(1) .eq. 8 .or. ICYCDO(1) .eq. 14) then
                      call cycoff (2)
                      icysv = 0
                  endif
                  go to 5000
              endif
          endif
c
c......Linearize move
c
          iaxfl(2) = 0
          imrsv  = IRAP
  500     IRAP   = imrsv
          ilin   = 0
          if (((LNRTOL(1) .ne. 0. .or. LNRTOL(2) .ne. 0.) .and.
     1        ALNMOV .eq. 0 .and. iaxfl(11) .eq. 0) .or.
     2        MDTLIN .ne. 0) then
              ilin   = 1
          else if (LRTRFL .eq. 1 .and. NROT .gt. 0 .and.
     1             LRTRCT .ne. 0 .and.  IJKROT .ne. 1 .and.
     2            (ierr .ne. 0 .or. iaxfl(2) .ne. 0)) then
              ilin   = 2
          endif
          if (ilin .ne. 0) then
              call mchlin (iaxfl(2),ierr,ilin,ient)
          endif
          iaxfl(3) = 0
  600     if (LIMERR .ne. 0) call tablin (iaxfl(3),ierr)
          iaxfl(5) = 0
  620     if (MDTLIN .ne. 0) call rtmlin (iaxfl(5),ierr,ient)
c
          iaxfl(10) = 0
 1600 continue
          if ((SMOKON .eq. 1 .or. SMOKAN .eq. 8 .or. SMOKAN .eq. 3)
     1        .and. SMOSTP .ne. 0.) call smodep (iaxfl(10))
c
c......Zigzag move
c
          if (IZIGON .ne. 0) then
              if (ISCIRC .eq. 0 .and. iaxfl(2) .gt. -3)
     -             call zigmov (1,0.d0)
          end if
c
c......Blade cutter control routines
c......blade rotary axis set-up
c
          iaxfl(7) = 0
          if (ALNMOV .ne. 0) call alnmck (iaxfl(7))
  640     if (iaxfl(7) .ne. 0) call alngen (iaxfl(7))
c
          iaxfl(6) = 0
  660     if ((ALNCUT .ne. 0 .or. SMOKAN .eq. 1) .and.
     -         iaxfl(7) .eq. 0) call alncck (iaxfl(6))
c
          if (MODBLD .eq. 1 .and. iaxfl(7) .eq. 0)
     -           call bldang (iaxfl(6))
          iaxfl(4) = 0
  700     if (IRETBL .ne. 0) call bldret (iaxfl(4),iaxfl(2))
c
c......Determine the number of axis
c......that will actually be output
c
          call whchax (AXSOUT,iaxsw,iaxcnt)
          if (iaxcnt .eq. 0) go to 4000
c
c...Only 1 axis moves
c...Hold back motion
c
          if (iaxcnt .eq. 1 .and. HLDBCK .eq. 1 .and. IRAP .eq. 0 .and.
     1        iaxfl(8) .eq. 0) then
              call hldmot (iaxsw)
c
c...Output motion block
c
          else
              call clrmot (1)
c
c......16/6/93 check again which axes since
c......clrmot could reset force switches
c
              call whchax (AXSOUT,iaxsw,iaxcnt)
c
c......Do not output TMARK
c......until final move for this CL point
c
              isav   = 0
              if ((iaxfl(2) .ne. 0 .or. iaxfl(3) .ne. 0 .or.
     1             iaxfl(4) .ne. 0 .or. iaxfl(5) .ne. 0) .and.
     2             IHDID .ne. 0) then
                  isav   = IHDID
                  IHDID  = 0
              endif
              call motion (iaxsw,iaxcnt)
              if (isav .ne. 0) IHDID = isav
          endif
c
c
c......Check if any motion generating
c......routine has control
c
 4000     if (iaxfl(4) .ne. 0) go to 700
          if (SMOKAN .ne. 0 .and. iaxfl(4) .eq. 0) call smoout
          if (iaxfl(7) .ne. 0) then
              if (iaxfl(7) .gt. 0) go to 640
              call alngen (iaxfl(7))
          end if
          if (iaxfl(6) .ne. 0) then
              call alngen1 (iaxfl(6))
              go to 660
          end if
          if (iaxfl(10) .ne. 0) go to 1600
          if (iaxfl(5) .ne. 0) go to 620
          if (iaxfl(3) .gt. 0) then
              if (LIMERR .ne. 0) go to 600
          else
              if (iaxfl(3) .lt. 0) go to 200
          end if
          if (iaxfl(2) .ne. 0 .and. iaxfl(4) .eq. 0) go to 500
c
c.........Automatic transformation blocks
c.........When output after motion block
c
          if (ixfm .eq. 2 .or. ixfm .eq. 4) then
              ifl    = 1
              if (ixfm .eq. 4) ifl = 3
              call trapvv (XFMLK,AXSOUT,XFMXVC,ifl,ierr)
              ixfm   = 0
          else if (ixfm .eq. 3 .or. ixfm .eq. 5) then
              ixfm   = ixfm   - 1
          endif
c
          if (iaxfl(1) .ne. 0) go to 300
          if (MTPDYN .eq. 3) then
             call alcntl (1)
             call alcntl (2)
          end if
          IFWSFL = IFWDFL
          if (iaxfl(11) .ne. 0) go to 180
          if (iaxfl(9) .ne. 0) go to 100
          if (ifsv .ne. -1) then
              if (rfed(npc) .eq. 0.) then
                  call raprst
              else
                  IFITYP = ifsv
                  PFEED(1) = fsv
              endif
              ifsv   = -1
          endif
          if (npc .lt. iaxfl(8)) then
              npc = npc + 1
              go to 150
          endif
          if (ICLIPS .ne. 1) call copyn (CLSAV,CLSAVS,NPT)
 4900     inc = inc + NPT
 5000 continue
c
c...End of routine
c
 8000 IRTFRC = 0
c......Reset cycle parameters
c......After interrupt by RAPID move
c
      ILINPT = 0
      if ((ICYCSW(1) .ne. 3 .and. icysv .ne. 3) .or.
     1    ICYCSW(4) .eq. 1) ICYCSW(1) = icysv
      if (ICYCSW(4) .eq. 1) then
          ICYCSW(2) = 1
          IFITYP = ICYCSV(2)
          if (IFITYP .ne. 5) then
              PFEED(IFITYP) = CYCPSV(5)
          else
              RCYCDO(2) = CYCPSV(5)
          endif
      endif
      ICYCSW(4) = 0
c
c......Reset ROTABL/SHORT,NEXT
c
      if (IRSRTE(3) .eq. 1) then
          IRTRTE(1) = IRSRTE(1)
          IRTRTE(2) = IRSRTE(2)
          IRSRTE(3) = 0
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fwdset (gfwd,gvec,gsav,knx,kflg)
c
c   FUNCTION:  This routine sets forward vector for move in the part
c              system.
c
c   INPUT:  gfwd  R*8  D3  -  previous FWD vector (if IFWSFL set).
c
c           knx   I*4  D1  -  CLPT array index corresponding to point
c
c   OUTPUT: gfwd  R*8  D3  -  forward direction vector for next move.
c
c           gvec  R*8  D3  -  linear move vector
c
c           gsav  R*8  D3  -  previous FWD direction vector.
c
c           kflg  I*4  D1  -  1 = FWD vector from cl file is set,
c                             0 = using linear move (gvec) as FWD vector.
c
c***********************************************************************
c
      subroutine fwdset (gfwd,gvec,gsav,knx,kflg)
c
      include 'post.inc'
c
      real*8 gvec(3),gfwd(3),gsav(3)
      integer*4 knx,kflg
c
      equivalence (IFWSFL,KPOSMP(0833))
      equivalence (HLDFLG,KPOSMP(1622)), (MOTEXP,KPOSMP(4211))
c
      integer*4 HLDFLG,MOTEXP,IFWSFL
c
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (HLDMCH,POSMAP(2141)), (HLDVEC,POSMAP(2177))
c
      real*8 MCHNUM(3,4),STONUM(3,4),TLVEC(3),VECSAV(3),
     1       HLDMCH(3,4),HLDVEC(3)
c
      integer*4 ifl
c
c...save FWD vector for previous point
c
      if (IFWSFL .eq. 1) call copyn (gfwd,gsav,3)
c
c...set linear move vector
c
      if (HLDFLG .eq. 1) then
          call gtfwvc (HLDMCH(1,2),HLDVEC,MCHNUM(1,2),TLVEC,gvec,
     -                 ifl)
      else
          call gtfwvc (STONUM(1,2),VECSAV,MCHNUM(1,2),TLVEC,gvec,
     -                 ifl)
      end if
      if (IFWSFL .eq. 0) call copyn (gvec,gsav,3)
c
c...set forward vector if expanded GT record in file
c
      if (MOTEXP .eq. 1) then
         call copyn (CLFWD(knx*3-2),gfwd,3)
         kflg = 1
      else
         call copyn (gvec,gfwd,3)
         kflg = 0
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  nmlset (gnorm,gsav,knx,kflg,kwhich)
c
c   FUNCTION:  This routine sets the tool normal vector based on the
c              part surface or drive surface normal.
c
c   INPUT:  gnorm  R*8  D3  -  Previous Normal vector (if IFWSFL set).
c
c           knx    I*4  D1  -  CLPT array index corresponding to point
c
c           kwhich I*4  D1  -  1 = Use part surface normal, 2 = drive
c                              surface.
c
c   OUTPUT: gnorm  R*8  D3  -  Normal direction vector for this move.
c
c           gsav   R*8  D3  -  Previous normal direction vector.
c
c           kflg  I*4  D1   -  0 = Previously define normal vector is
c                              used for 'gnorm' (an extended motion
c                              record is not active), 1 = Normal vector
c                              from part surface is set, 2 = Drive surface.
c
c***********************************************************************
c
      subroutine nmlset (gnorm,gsav,knx,kflg,kwhich)
c
      include 'post.inc'
c
      integer*4 knx,kflg,kwhich
c
      real*8 gnorm(3),gsav(3)
c
      equivalence (INMSFL,KPOSMP(3322)), (INMDEF,KPOSMP(3323))
      equivalence (MOTEXP,KPOSMP(4211))
c
      integer*4 INMSFL,INMDEF,MOTEXP
c
      equivalence (TLVEC ,POSMAP(1369))
c
      real*8 TLVEC(3)
c
c...Save normal vector for previous point
c
      if (INMSFL .ne. 0) call copyn (gnorm,gsav,3)
c
c...Set normal vector if expanded GOTO record in file
c
      if (MOTEXP .eq. 1) then
         if (kwhich .eq. 1) then
             call copyn (CLPTE2(knx*3-2),gnorm,3)
             kflg = 1
         else
             call copyn (CLPTE4(knx*3-2),gnorm,3)
             kflg = 2
         endif
         INMDEF = 1
c
c...Use tool axis vector if
c...Normal vector was never defined
c...Otherwise just use previous vector
c
      else
         if (INMDEF .eq. 0) call copyn (TLVEC,gnorm,3)
         kflg = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pre_clrchk (knx,gpt,gfed,kout,kfl)
c
c   FUNCTION:  This routine checks motion scope in CLRSRF planes.
c
c   INPUT:  knx    I*4  D1 - Pointer in CLPT where is current point.
c
c   OUTPUT: kout   I*4  D1 - Number of points to output: 0 - motion out
c                            of scope, > 0 - output all.
c
c           gpt    R*8  D6.10 - CL points to move to (actually should
c                               be 0 - 2 points).
c
c           gfed   R*8  Dn   - Feed rate at each location.  -1 = Use
c                              programmed feed rate.  0 = Rapid.
c
c           kfl    I*4  D1 - >1 means that the move crosses the plane
c                            boundary coming from outside the plane.
c                            The calling routine should adjust the feed
c                            rate if user requested.
c
c***********************************************************************
c
      subroutine pre_clrchk (knx,gpt,gfed,kout,kfl)
c
      include 'post.inc'
c
      integer*4 knx,kout,kfl
c
      real*8 gpt(6,30),gfed(30)
c
      equivalence (NPT   ,KPOSMP(0059)), (NPLANE,KPOSMP(0113))
      equivalence (HLDFLG,KPOSMP(1622))
c
      integer*4 NPLANE,NPT,HLDFLG
c
      equivalence (CLSAV ,POSMAP(0201)), (CLPT  ,POSMAP(0491))
      equivalence (VECSAV,POSMAP(1372)), (CLSAVS,POSMAP(2180))
c
      real*8 CLPT(240),CLSAV(21),VECSAV(3),CLSAVS(21)
c
      real*8 ptin(6),pto(6)
c
      call copyn (CLSAVS,pto,NPT)
      call copyn (CLPT(knx),ptin,NPT)
      if (NPT .eq. 3) then
         call copyn (VECSAV,ptin(4),3)
         call copyn (VECSAV,pto(4),3)
      end if
c
c...cross current move with planes
c
      call pre_cross (ptin,pto,gpt,gfed,kout,kfl)
c
c...save current cl point as a from point for the next move,
c...so next move will be accurate
c
      call copyn (CLPT(knx),CLSAVS,NPT)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pre_cross (gpt,gsav,gpto,gfed,kout,kfl)
c
c   FUNCTION:  This routine crosses single move with CLRSRF planes to
c              determine cl points to output (based on prepst cross.f).
c
c   INPUT:  gpt    R*8  D6 - CLPT current point.
c
c           gsav   R*8  D6 - CLPT from point.
c
c   OUTPUT: kout   I*4  D1 - Number of points to output: 0 - motion out
c                            of scope, > 0 - output all.
c
c           gpto   R*8  D6.10 - CLPT points to move to (actually should
c                               be 0 - 2 points).
c
c           gfed   R*8  Dn   - Feed rate at each location.  -1 = Use
c                              programmed feed rate.  0 = Rapid.
c
c           kfl    I*4  D1 - >1 means that the move crosses the plane
c                            boundary coming from outside the plane.
c
c***********************************************************************

      subroutine pre_cross (gpt,gsav,gpto,gfed,kout,kfl)
c
      include 'post.inc'
c
      integer*4 kout,kfl
c
      real*8 gpt(6),gpto(6,30),gsav(6),gfed(30)
c
      equivalence (NPT   ,KPOSMP(0059)), (NPLANE,KPOSMP(0113))
      equivalence (CPLNTR,KPOSMP(0115)), (CLRTCP,KPOSMP(0121))
      equivalence (MXFLAG,KPOSMP(4002)), (MXTRAN,KPOSMP(4003))

      integer*4 NPT,NPLANE,CPLNTR,MXTRAN,CLRTCP,MXFLAG
c
      equivalence (CLSAV ,POSMAP(0201)), (CPLANE,POSMAP(0253))
      equivalence (REFMAT,POSMAP(4001))
      equivalence (TRAMAT,POSMAP(4013)), (FUZZ4 ,POSMAP(4912))
c
      real*8 CPLANE(5,10),CLSAV(21),FUZZ4,TRAMAT(12),REFMAT(12)
C
      real*8 pvec(3),vec(3),rlen,pto(6),fuzz,sg,ds,ast,afi,adl,fi,tht,
     1       tvm(4),ds1,ds2,dist1(10),dist2(10),ptio(3,10),tvec(3),
     2       ptout(6,11),ndist,nmag,ndot,tplane(5,10),plpt(3)
C
      integer*4 i,j,is1,k,ptinfl,iold,indx(10),ifinfl(10),nout,
     -          iboth,ierr,is,inew
C
      fuzz  = .1d0 * FUZZ4
c
      kout  = 0
      kfl   = 0
      call copyn (gsav,pto,6)
      call vcplvc (gpt,pto,pvec,-1.d0)
      rlen  = nmag(pvec)
      if (rlen .lt. FUZZ4) go to 8000
      call unitvc (pvec,vec)
c
c... clear ptinfl & other flags
c
      ptinfl  = 1
      iboth   = 1
      nout    = 0
      CLRTCP = 0
      do 5000 i=1,NPLANE,1
         if (iboth .eq. 2) go to 5000
         ifinfl(i) = 0
         inew = 0
         iold = 0
c
c...Calculate working plane from
c...translation matrix
c
         if (CPLNTR .eq. 1) then
             plpt(1) = CPLANE(1,i) * CPLANE(4,i)
             plpt(2) = CPLANE(2,i) * CPLANE(4,i)
             plpt(3) = CPLANE(3,i) * CPLANE(4,i)
             tvec(1) = 0.
             tvec(2) = 0.
             tvec(3) = 0.
             call preadj (plpt,plpt,tvec,tvec)
             call copyn (CPLANE(1,i),tvec,3)
             if (MXTRAN .eq. 1) call matpta (tvec,tvec,TRAMAT,2)
             if (MXFLAG .eq. 1) call matpta (tvec,tvec,REFMAT,2)
             call unitvc (tvec,tplane(1,i))
             tplane(4,i) = ndot(tvec,plpt)
             tplane(5,i) = CPLANE(5,i)
         else
             call copyn (CPLANE(1,i),tplane(1,i),5)
         endif
C
C...Get what side of plane
C...points are on and intersect if not single side
C
         ds1 = ndot(pto(1),tplane(1,i)) - tplane(4,i)
         ds2 = ndot(gpt,tplane(1,i)) - tplane(4,i)
         sg = tplane(5,i)
         if (ds1*sg .lt. -fuzz .or. ds2*sg .lt. -fuzz) then
             if (ds1*sg .ge. -fuzz) iold = 1
             if (ds2*sg .ge. -fuzz) inew = 1
             iboth = 0
c
c...get intersection point if points are on opposite sides
c
             if (iold+inew .eq. 1) then
                if (inew .eq. 0) ptinfl = inew
                nout = nout + 1
                call plnint (pto,vec,tplane(1,i),ptio(1,nout),ierr)
             else
                iboth = 2
             end if
         else
c
c...both points are on specified
c...side of plane
c
             ifinfl(i) = 1
         end if
 5000 continue
c
c... see if both points are on correct side of all planes
c
      if (iboth .eq. 1) then
         call copyn (gpt,gpto,NPT)
         kout   = 1
         go to 8000
c
c... see if old point and new point are on the same sides of all
c... planes , not the correct side
c
      else if (iboth .eq. 2) then
         CLRTCP = 1
         go to 7000
      end if
      is   = 0
c
c... incr thru  intersects
c
      is1 = 0
      do 1050 i=1,nout,1
c
c... get what side of plane
c... intersect points points are on
c
         do 1075 j=1,NPLANE,1
            if (ifinfl(j) .eq. 1) go to 1075
            ds = ndot (ptio(1,i),tplane(1,j)) - tplane(4,j)
            sg = tplane(5,j)
            if (sg*ds .lt. -fuzz) go to 1050
 1075    continue
c
c...output intersection point
c
         is1 = is1 + 1
         indx(is1) = is1
         call copyn (ptio(1,i),ptout(1,is1),NPT)
         dist1(is1) = ndist (ptout(1,is1),pto)
         dist2(is1) = dist1(is1)
 1050 continue
c
c...check if newpoint has to be output
c
      CLRTCP = 1 - ptinfl
      if (ptinfl .ne. 0) then
         is1  = is1 + 1
         indx(is1) = is1
         call copyn (gpt,ptout(1,is1),NPT)
         dist1(is1) = rlen
         dist2(is1) = rlen
      end if
      nout = is1
      if (nout .lt. 1) go to 7000
 1090 if (nout .eq. 1) go to 1190
C
C...sort points by distance and purge same points
c...order indexes of points order are in indx
C
      call sort (dist1,indx,nout)
      do 1150 k=1,nout-1,1
         if (dabs(dist1(k)-dist1(k+1)) .lt. fuzz) indx(k+1) = 0
 1150 continue
c
c... area for calculating vectors
c
 1190 if (NPT .gt. 3) then
         call getlan (pto(4),gpt(4),ast,afi,adl,fi,tht,tvm)
         do 2050 k = 1,nout,1
c
c... calculate tool axis vectors
c... calc distances between points
c
            if (indx(k) .ne. 0)  then
               ds1    = dist2(indx(k)) / rlen
               ds2    = ast + ds1 * adl
               call setijk (ds2,ptout(4,indx(k)),fi,tht)
            end if
 2050    continue
      end if
c
c...copy points to output buffer in sorted order
c
      kout   = 0
      do 2100 i=1,nout,1
         if (indx(i) .ne. 0) then
            kout = kout + 1
            call copyn (ptout(1,indx(i)),gpto(1,kout),NPT)
            if (NPT .eq. 3) call copyn (CLSAV(4),gpto(4,kout),3)
         end if
 2100 continue
      kfl    = nout
c
c...Tool retraction is on
c...Retract and reposition tool
c...from intersection point to final point
c
      if (kfl .gt. 1) call pre_retrct (gpto,CLSAV,gfed,kout,0)
      go to 8000
c
 7000 kout   = 0
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pre_retrct (gpt,gsav,gfed,kout,kfl)
c
c   FUNCTION:  This routine retracts and repositions the tool when a
c              clipping region is being re-entered.
c
c   INPUT:  gpt    R*8  D6,n - Array of points awaiting output.  The
c                              first point is the entry intersection
c                              point.
c
c           gsav   R*8  D6   - CLPT from point.
c
c           kout   I*4  D1   - Number of points to output.
c
c           kfl    I*4  D1   - 1 = Force retract if final point on
c                              clipping plane.  0 = Perform normally.
c
c   OUTPUT: gpt    R*8  D6,n - Updated output points.
c
c           gfed   R*8  Dn   - Feed rate at each location.  -1 = Use
c                              programmed feed rate.  0 = Rapid.
c
c           kout   I*4  D1   - Updated number of points to output.
c
c***********************************************************************

      subroutine pre_retrct (gpt,gsav,gfed,kout,kfl)
c
      include 'post.inc'
c
      integer*4 kout,kfl
c
      real*8 gpt(6,30),gsav(6),gfed(30)
c
      equivalence (CLRTFL,KPOSMP(0116)), (CLRTCP,KPOSMP(0121))
      equivalence (IFITYP,KPOSMP(3150))
c
      integer*4 CLRTFL(3),IFITYP,CLRTCP
c
      equivalence (CPLNFD,POSMAP(0390))
      equivalence (CLRTPL,POSMAP(0391)), (CLRTFD,POSMAP(0403))
      equivalence (CLRTDS,POSMAP(0406)), (MCHNUM,POSMAP(1287))
      equivalence (LINAXS,POSMAP(1299)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (PFEED ,POSMAP(3540))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 CLRTPL(4,3),CLRTFD(3),CPLNFD,CLRTDS,MCHNUM(3,4),LINAXS(6),
     1       AXSOUT(10),TLVEC(3),ROTANG(20,2),PFEED(4)
C
      integer*4 i,j,nout,ierr,ifl,ifsv
C
      real*8 pts(6,30),pln(4),ptsav(6),fsv,d,ndist
c
c...See if the retract option is enabled
c
      if (kfl .eq. 1) then
          if (CLRTCP .eq. 0) go to 8000
      else
          d = ndist(gpt(1,1),gsav)
          if (CLRTFL(1) .eq. 0 .or. d .le. CLRTDS) then
              gfed(1) = CPLNFD
              do 50 i=2,kout,1
                  gfed(i) = -1
   50         continue
              go to 8000
          endif
      endif
c
c...Create copy of input points
c
      nout   = kout
      do 120 i=1,nout,1
          do 100 j=1,6,1
              pts(j,i) = gpt(j,i)
  100     continue
  120 continue
      do 150 i=1,6,1
          ptsav(i) = gsav(i)
  150 continue
c
c...Perform retract/plunge operation
c
      kout  = 0
      do 200 i=1,3,1
          if (CLRTFL(i) .ne. 0) then
              ifl    = CLRTFL(i)
              call copyn (CLRTPL(1,i),pln,4)
          endif
c
          if (CLRTFL(i) .ne. 0 .or. i .ne. 3) then
              kout   = kout   + 1
              if (ifl .eq. 1) then
                  call plnint (ptsav,ptsav(4),pln,gpt(1,kout),ierr)
                  if (ierr .ne. 0) go to 9000
              else
                  call vcplvc (ptsav,ptsav(4),gpt(1,kout),pln(1))
              endif
              call copyn (ptsav(4),gpt(4,kout),3)
              call copyn (pts,ptsav,6)
c
              if (i .eq. 1) then
                  gfed(kout) = CLRTFD(1)
              else if (i .eq. 2) then
                  gfed(kout) = CPLNFD
              else if (CLRTFL(2) .ne. 0) then
                  gfed(kout) = CLRTFD(2)
              else if (CLRTFL(3) .ne. 0) then
                  gfed(kout) = CLRTFD(1)
              else
                  gfed(kout) = -1
              endif
          endif
c
          if (kfl .eq. 1) go to 6000
c
  200 continue
c
c...Copy remaining points to output array
c
  400 j      = kout
      do 500 i=1,nout,1
          kout   = kout   + 1
          call copyn (pts(1,i),gpt(1,kout),6)
          gfed(kout) = -1
  500 continue
      if (CLRTFL(3) .ne. 0 .and. j .ne. 0) then
          gfed(j+1) = CLRTFD(3)
      else if (CLRTFL(2) .ne. 0 .and. j .ne. 0) then
          gfed(j+1) = CLRTFD(2)
      endif
      go to 8000
c
c...Retract forced at clearance plane
c...By CLRSRF/STOP-NOMORE
c...Output point
c
 6000 call copyn (gpt,MCHNUM(1,2),3)
      call copyn (gpt(4,1),TLVEC,3)
      call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,1,5)
      ifsv   = IFITYP
      fsv    = PFEED(1)
      if (gfed(1) .eq. 0) then
          call rapset (5,2)
      else
          IFITYP = 1
          PFEED(1) = gfed(1)
      endif
      call movpos
      IFITYP = ifsv
      PFEED(1) = fsv
c
c...End of routine
c
 8000 return
c
c...Error intersecting with retract plane
c
 9000 kout   = 0
      go to 400
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pre_linchk (kinc,kout)
c
c   FUNCTION:  This routine checks CL motion for prepst LINTOL condition.
c              If move has to be broken apart some common variables are
c              set to interpolate desired number of points.
c
c   INPUT:  kinc   I*4  D1 - CLPT current point index.
c
c   OUTPUT: kout   I*4  D1 - Calculated number of points to output.
c
c***********************************************************************
      subroutine pre_linchk (kinc,kout)
c
      include 'post.inc'
      integer*4 kinc,kout
c
      equivalence (NPT   ,KPOSMP(0059)), (LINTSW,KPOSMP(0114))
      equivalence (LINITM,KPOSMP(0115))
c
      integer*4 NPT,LINTSW,LINITM
c
      equivalence (CLSAV ,POSMAP(0201)), (DTOLER,POSMAP(0303))
      equivalence (PREAST,POSMAP(0304)), (PREAFI,POSMAP(0305))
      equivalence (PREADL,POSMAP(0306)), (PREFI ,POSMAP(0307))
      equivalence (PRETHT,POSMAP(0308)), (PREPTS,POSMAP(0309))
      equivalence (PREVEC,POSMAP(0315)), (CLPT  ,POSMAP(0491))
      equivalence (FUZZ4 ,POSMAP(4912))
c
      real*8 CLSAV(21),DTOLER,PREAST,PREAFI,PREADL,PREFI,PRETHT,
     -       PREPTS(6),PREVEC(3),CLPT(240),FUZZ4
c
      real*8 pts(6),delt,dist(3),tvm(4),ndist
c
      integer*4 inx(5),m,k,itim
c
      data inx /1,2,3,1,2/
c
      itim   = 0
      call copyn (CLPT(kinc),pts,NPT)
c
c...linaer delta move limit
c
      if (LINTSW .eq. 1) then
         delt  = ndist (pts,CLSAV)
      else
c
c...tolerance active
c
         dist(1) = dabs (pts(1) - CLSAV(1))
         dist(2) = dabs (pts(2) - CLSAV(2))
         dist(3) = dabs (pts(3) - CLSAV(3))
         k     = 1
         if (dist(2) .gt. dist(k)) k = 2
         if (dist(3) .gt. dist(k)) k = 3
         m     = inx(k+1)
         k     = inx(k+2)
         if (dist(m) .lt. dist(inx(k))) m = k
         delt  = dist(m)
      end if
      if (delt .lt. DTOLER+FUZZ4) go to 8000
c
c...interpolation required,
c...calculate some common constants
c
      itim   = (delt-FUZZ4) / DTOLER + 1
      call copyn (CLSAV,PREPTS,NPT)
      call vcplvc (pts,CLSAV,PREVEC,-1.d0)
      if (NPT .ne. 3) then
         call getlan (CLSAV(4),pts(4),PREAST,PREAFI,PREADL,
     -                PREFI,PRETHT,tvm)
      end if
      kout   = itim
      LINITM = itim
c
c...within tolerance
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pre_movlin (kinc,kout)
c
c   FUNCTION:  This routine interpolates CL point for prepst LINTOL
c              command.  Point & TV are placed in current CLPT point inc.
c              Must be called until kout=0 to output destination point.
c
c   INPUT:  kinc   I*4  D1 - CLPT current point index.
c
c           kout   I*4  D1 - Interpolation index (reversed order i.e. it
c                            is LINITM for 1-st point and 1 for last).
c
c   OUTPUT: kout   I*4  D1 - Remaining number of points to output.
c
c***********************************************************************
      subroutine pre_movlin (kinc,kout)
c
      include 'post.inc'
      integer*4 kinc,kout
c
      equivalence (NPT   ,KPOSMP(0059)), (LINITM,KPOSMP(0115))
c
      integer*4 NPT,LINITM
c
      equivalence (PREAST,POSMAP(0304))
      equivalence (PREADL,POSMAP(0306)), (PREFI ,POSMAP(0307))
      equivalence (PRETHT,POSMAP(0308)), (PREPTS,POSMAP(0309))
      equivalence (PREVEC,POSMAP(0315)), (CLPT  ,POSMAP(0491))
c
      real*8 PREAST,PREADL,PREFI,PRETHT,CLPT(240),PREVEC(3),PREPTS(6)
c
      real*8 den,ratio,ang
c
c...set ratio for interpolation
c
      den    = LINITM
      kout   = kout - 1
      ratio  = (LINITM - kout) / den
c
c...interpolate point & tool vector if necessary
c
      call vcplvc (PREPTS,PREVEC,CLPT(kinc),ratio)
      if (NPT .ne. 3) then
         ang = PREAST + ratio*PREADL
         call setijk (ang,CLPT(kinc+3),PREFI,PRETHT)
      end if
c
      return
      end
