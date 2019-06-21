c
c***********************************************************************
c
c   FILE NAME:  postp
c   CONTAINS:
c               postp   ppword  pgword  machin
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        postp.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        08/11/16 , 10:36:20
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  postp (cmsg,kerr)
c
c   FUNCTION:  This is the controlling routine for processing the cl-
c              file.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine postp (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (IPSTWD,KPOSMP(0006))
      equivalence (MULTAX,KPOSMP(0056)), (IFIREC,KPOSMP(0057))
      equivalence (IFIPT ,KPOSMP(0058)), (ICUCHG,KPOSMP(0068))
      equivalence (NUMERR,KPOSMP(0082)), (LSTPC ,KPOSMP(0083))
      equivalence (ICLOUT,KPOSMP(0084)), (IERRPC,KPOSMP(0085))
      equivalence (NUMWRN,KPOSMP(0089)), (NUMFAT,KPOSMP(0090))
      equivalence (IPVCON,KPOSMP(0176)), (IPVCTR,KPOSMP(0177))
      equivalence (ICYCDO,KPOSMP(0276))
      equivalence (IFWSFL,KPOSMP(0833)), (BRKOP ,KPOSMP(1137))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (ISCIRC,KPOSMP(1238)), (ISACIR,KPOSMP(1346))
      equivalence (ICSPRE,KPOSMP(1386)), (ICIRSW,KPOSMP(1391))
      equivalence (IHELIX,KPOSMP(1392)), (LOOKFL,KPOSMP(1628))
      equivalence (TOOLFL,KPOSMP(1804)), (LOOKFL,KPOSMP(1628))
      equivalence (BSPLSW,KPOSMP(4084)), (ICRHEL,KPOSMP(4250))
c
      integer*4 ISN,ITYPE,ISUBT,MXCL,MULTAX,IFIREC,IFIPT,LSTPC,IERRPC,
     1          LOOKFL(12),ISCIRC,ICIRSW,IHELIX,ICLOUT,IPSTWD(50),
     2          BRKOP(10),ISACIR,BSPLSW,IFWSFL,ICSPRE,NUMFAT,NUMWRN,
     3          NUMERR,ICRHEL(10),TOOLFL(20),IPVCON,IPVCTR,ICUCHG,
     4          MACHTP,ICYCDO(15)
c
      equivalence (CLPOS ,POSMAP(0222)), (FWDSAV,POSMAP(4591))
c
      real*8 CLPOS(21),FWDSAV(3)
c
      equivalence (ERRMAC,CPOSMP(0193)), (LPSTWD,CPOSMP(0217))
      equivalence (LMACRO,CPOSMP(0861))
c
      character*24 LMACRO,ERRMAC
      character*512 LPSTWD
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ifl,ifini,ilst,jdat(5),iopmac,isav,nhelx,helx(16)
      integer*4 cwrn,icrmac,icrrec,icrpt
c
      logical ltlcmd
c
      character*8 aerr(7)
c
      data nhelx /16/
      data helx /1022, 1030, 1049, 1007, 1010, 1021, 1009, 1046,
     1           1013, 1012, 1048, 0005, 1019, 1031, 1005, 1037/
      data aerr /' ','INVPSIS','INVCKPL','INVGOFWD','VECZERO','NOCRAD',
     1           'CIRVEC'/
c
c...Get the current date & time
c...at start of run
c
      call ncdate (LDATE)
      call ftim (LTIME)
c
c...Initialize routine
c
      call genini
      cwrn = 1
      icrrec = 0
      icrmac = 0
c
c...Initialize punch file
c
      call tapini
c
c...Check for OPTION Macro
c
      call lodmac (144,jdat)
      iopmac = jdat(1)
c
c...Call G$MAIN Macro
c
      ISN    = 1
      ITYPE  = 2000
      ISUBT  = 3999
      MXCL   = 0
      call ppcall (ifl,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Check for a tape break
c
      ifini  = 0
  100 if (BRKOP(1) .ne. 0 .and. BRKOP(1) .ne. 1) then
          call brkyet (cmsg,kerr)
          if (kerr .ne. 0) go to 9000
      endif
c
c...Get the next clfile record
c...from a called Macro
c
      if (IMACPT .ne. 0) then
          call precmp (cmsg,kerr)
          if (kerr .ne. 0) go to 9000
      endif
c
c...Read the clfile record
c
      if (IMACPT .eq. 0) then
          if (ifini .eq. 1) then
              ICLOUT = ICLOUT + 1
              go to 7000
          endif
          call clread (IFIREC,IFIPT,1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Increment cl record number
c
      ICLOUT = ICLOUT + 1
      if (IPVCON .ne. 0) IPVCON = IPVCON + 1
c
c...Return to Look ahead caller
c
      if (LOOKFL(1) .ne. 0) then
          call bsplin (LOOKFL(1))
          go to 100
      endif
      if (LOOKFL(2) .ne. 0) then
          call circul (LOOKFL(2), LOOKFL(3),icrmac)
c
c......Call CIRCLE Macro
c
          if (icrmac .eq. 1 .and. LOOKFL(2) .eq. 0) then
              ITYPE = 2000
              ISUBT = 4026
              MXCL  = 0
              call ppcall (ifl,cmsg,kerr)
              if (kerr .ne. 0) go to 9000
              if (ifl .eq. 0) ICLOUT = ICLOUT - 1
              icrmac = 0
              ISCIRC = 0
c
c.......Error processing CIRCLE Macro
c
          else if (icrmac .eq. -1) then
              IFIREC = icrrec
              IFIPT  = icrpt
              icrmac = 0
              ISCIRC = 0
          endif
          go to 100
      endif
c
c...ISN record
c
      if (ITYPE .eq. 1000) then
          call simisn (cmsg,kerr)
c
c...Post word
c
      else if (ITYPE .eq. 2000) then
c
c......PPRINT issued
c......Check for following LOADTL statement
c
          if (ISUBT .eq. 1044 .and. TOOLFL(19) .eq. 1) then
              call clsamp (0)
              if (ltlcmd()) TOOLFL(19) = -1
              call clsamp (-1)
          else if (TOOLFL(19) .eq. -1 .and. ltlcmd()) then
              TOOLFL(19) = 1
          endif
c
c......Check for Macro call
c
          call ppcall (ifl,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          if (ifl .eq. 0) then
              ICLOUT = ICLOUT - 1
              go to 100
c
c.........No Post Macro for this command
c.........Check for OPTION Macro
c
          else if (iopmac .ne. 0 .and. MXCL .ge. 1 .and.
     1             IPSTWD(1) .eq. 144) then
              isav   = ISUBT
              ISUBT  = IPSTWD(1)
              IPSTWD(1) = isav
              call ppcall (ifl,cmsg,kerr)
              if (kerr .ne. 0) go to 9000
              if (ifl .eq. 0) then
                  ICLOUT = ICLOUT - 1
                  go to 100
              endif
              IPSTWD(1) = ISUBT
              ISUBT   = isav
          endif
c
c......No Macro call
c......Check for PREGEN command
c
          call pgword (ifl,ilst,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          if (ifl .eq. 0) then
              if (ilst .eq. 1) go to 1000
              go to 100
          endif
c
c......Post-processor command
c.........Cancel helical interpolation
c
cc          inc    = jindex(helx,ISUBT,nhelx)
cc          if (inc .eq. 0) IHELIX = 0
cc          IHELIX = 0
          call ppword (cmsg,kerr)
          if (kerr .ne. 0) go to 9000
c
c...Circle record
c
      else if (ITYPE .eq. 3000) then
          if (IOPFL(10) .eq. 3) call clrmot (0)
          ISCIRC = ICIRSW
          ISACIR = 1
          if (IHELIX .eq. 2) ISCIRC = 1
          cwrn = 1
          if (ISUBT.eq.99) cwrn = 0
c
c......Determine if CIRCLE Macro exists
c
          call lodmac (4026,jdat)
          if (jdat(1) .ne. 0)  then
              icrmac = 1
              icrrec = IFIREC
              icrpt  = IFIPT
              ISCIRC = 1
              go to 100
          else
              icrmac = 0
              if (NCLVER .gt. 9.549) call simcir (cmsg,kerr)
          endif
c
c...Motion record
c
      else if (ITYPE .eq. 5000) then
c
c......Circular interpolation
c
          if (ISCIRC .eq. 1) then
             call clrmot (0)
             call circul (LOOKFL(2),cwrn,icrmac)
             cwrn = 1
c
c.........Error processing CIRCLE Macro
c
             if (icrmac .eq. -1) then
                IFIREC = icrrec
                IFIPT  = icrpt
                icrmac = 0
                ISCIRC = 0
                go to 100
             else if (icrmac .eq. 1) then
                go to 100
             endif
c
c......Linear interpolation
c......Check for Macro call
c
          else
             call ppcall (ifl,cmsg,kerr)
             if (kerr .ne. 0) go to 9000
             if (ifl .eq. 0) then
                ICLOUT = ICLOUT - 1
                go to 100
             endif
c
             if (BSPLSW .eq. 1) call bsplin(LOOKFL(1))
             if (LOOKFL(1) .eq. 0) then
c
c......Process motion record
c
                IHELIX = 0
                if (ISACIR .ne. 0) then
                   if (ISACIR .eq. 2 .and. ISUBT .ne. 6) ISACIR = 0
                   if (ISACIR .eq. 1) ISACIR = 2
                endif
                call mocntl
             end if
          endif
c
c......Positioning record (do not process, set FWD vector)
c......IFWSFL remains 2 until motion is processed
c
      else if (ITYPE .eq. 5210) then
          call copyn (CLPOS(7),FWDSAV,3)
          IFWSFL = 2
c
c......CUTTER record
c
      else if (ITYPE .eq. 6000 .or.  (ITYPE .eq. 7100 .and.
     1         (ISUBT .eq. 1 .or. ISUBT .eq. 2 .or. ISUBT .eq. 4)))
     2             then
          if (NCLVER .lt. 9.549) then
              call simsta (0,cmsg,kerr)
          else if (IPVCTR .eq. 1 .and. MACHTP .ne. 5) then
              ICUCHG = 0
              call simcut (cmsg,kerr)
          endif
c
c......STOCK/FIXTURE record
c
      else if (ITYPE .eq. 2600 .or. ITYPE.eq.2601) then
          call simstk (0,cmsg,kerr)
c
c......Cutter Profile record
c
      else if (ITYPE .eq. 7110 .or. ITYPE .eq. 7120) then
          call simprf (cmsg,kerr)
c
c......APT Source file error
c
      else if (ITYPE .eq. 8000) then
          call psterr (4,'INVAPTSC',aerr(ISUBT+1),0)
c
c......FINI record
c
      else if (ITYPE .eq. 14000) then
          IHELIX = 0
          call clrmot (2)
          ISUBT  = 4012
          MXCL   = 0
          ifini  = 1
          call ppcall (ifl,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          if (ifl .eq. 0) then
              ICLOUT = ICLOUT - 1
              go to 100
          endif
          go to 7000
      endif
c
c...Write listing record
c
 1000 call genlst (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if ((NUMFAT .gt. MAXFAT) .or.
     1    (NUMERR .gt. MAXPER).or.
     2    (NUMWRN .gt. MAXWRN)) then
         kerr = -10
         go to 9000
      endif
      go to 100
c
c...FINI
c...End of program
c
 7000 ITYPE  = 14000
      MXCL   = 0
      call genlst (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call fini
c
c...End of routine
c
 8000 return
c
c...An error occurred processing clfile
c
 9000 IERRPC = LSTPC
      ERRMAC = LMACRO
      if (kerr .lt. 0) go to 8000
      kerr   = 0
c
c......Check for ERROR macro
c
      ISUBT  = 1103
      LPSTWD = ERRLAB
      call ppcall (ifl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (ifl .eq. 0) go to 100
c
c......Print out error message
c
      call lsterr (cmsg,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 100
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ppword (cmsg,kerr)
c
c   FUNCTION:  This routine processes post processor commands.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ppword (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (IPSTWD,KPOSMP(0006)), (TOOLFL,KPOSMP(1804))
      equivalence (IRAP  ,KPOSMP(3199))
c
      integer*4 ISUBT,IRAP,IPSTWD(50),MXCL,TOOLFL(20)
c
      equivalence (LPARTN,CPOSMP(0067)), (LPSTWD,CPOSMP(0217))
c
      character*66 LPARTN
      character*512 LPSTWD
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 inc,ignor(12),nignor,jindex
c
      data nignor /12/
      data ignor /128,129,130,1014,1038,1039,1040,1041,1043,1059,1064,
     1            1080/
c
c...Ignored post commands
c
      inc    = jindex(ignor,ISUBT,nignor)
      if (inc .ne. 0) go to 8000
c
c...Initialize routine
c
      call clrmot (2)
      if (IRAP .ne. 0 .and. ISUBT .ne. 1106) call raprst
c
c...ARCSLP
c
      if (ISUBT .eq. 1029) then
          call arcslp
c
c...ALIGN
c
      else if (ISUBT .eq. 1076) then
          call align
c
c...APPEND
c
      else if (ISUBT .eq. 1199) then
          call append
c
c...AUXFUN
c
      else if (ISUBT .eq. 1022) then
          call auxfun
c
c...BREAK
c
      else if (ISUBT .eq. 16) then
          call break (cmsg,kerr)
c
c...CHECK
c
      else if (ISUBT .eq. 1023) then
          call check
c
c...CIRCLE
c
      else if (ISUBT .eq. 4026) then
          call pcircl (cmsg,kerr)
c
c...CLAMP
c
      else if (ISUBT .eq. 1060) then
          call clamp
c
c...CLRSRF
c
      else if (ISUBT .eq. 1057) then
          call clrsrf
c
c...COOLNT
c
      else if (ISUBT .eq. 1030) then
          call coolnt
c
c...COUPLE
c
      else if (ISUBT .eq. 1049) then
          call couple
c
c...CUTCOM
c
      else if (ISUBT .eq. 1007) then
          call cutcom
c
c...CYCLE
c
      else if (ISUBT .eq. 1054) then
          call cycle
c
c...DELAY
c
      else if (ISUBT .eq. 1010) then
          call delay
c
c...DISPLY
c
      else if (ISUBT .eq. 1021) then
          call disply
c
c...END
c
      else if (ISUBT .eq. 1) then
          call fini
c
c...FEDRAT
c
      else if (ISUBT .eq. 1009) then
          call fedrat
c
c...FORCE
c
      else if (ISUBT .eq. 1106) then
          call force
c
c...GOHOME
c
      else if (ISUBT .eq. 17) then
          call gohome
c
c...HEAD
c
      else if (ISUBT .eq. 1002) then
          call head
c
c...INSERT
c
      else if (ISUBT .eq. 1046) then
          call insert
c
c...LEADER
c
      else if (ISUBT .eq. 1013) then
          call leader
c
c...LINTOL
c
      else if (ISUBT .eq. 1067) then
          call lintol
c
c...LOAD/TOOL
c
      else if (ISUBT .eq. 1075 .and. MXCL .gt. 1 .and.
     1        TOOLFL(18) .eq. 1) then
          call loadtl
c
c...LOADTL
c
      else if (ISUBT .eq. 1055) then
          call loadtl
c
c...MACHIN
c
      else if (ISUBT .eq. 1015) then
          go to 8000
c
c...MAXDPM
c
      else if (ISUBT .EQ. 1062) then
          call maxdpx
c
c...MCHTOL
c
      else if (ISUBT .eq. 1016) then
          call mchtol
c
c...MODE
c
      else if (ISUBT .eq. 1003) then
          call mode
c
c...OPSKIP
c
      else if (ISUBT .eq. 1012) then
          call opskip
c
c...OPSTOP
c
      else if (ISUBT .eq. 3) then
          call opstop
c
c...ORIGIN
c
      else if (ISUBT .eq. 1027) then
          call origin
c
c...PARTNO
c
      else if (ISUBT .eq. 1045) then
          LPARTN = LPSTWD
c
c...PLUNGE
c
      else if (ISUBT .eq. 1001) then
          call plunge
c
c...POD
c
      else if (ISUBT .eq. 1088) then
          call pod
c
c...POSCAL
c
      else if (ISUBT .eq. 1109) then
          call poscal
c
c...POSITN
c
      else if (ISUBT .eq. 1072) then
          call positn
c
c...POSTN
c
      else if (ISUBT .eq. 1024) then
          call postn
c
c...PPTOL
c
      else if (ISUBT .eq. 1068) then
          call pptol
c
c...PREFUN
c
      else if (ISUBT .eq. 1048) then
          call prefun
c
c...QAXIS
c
      else if (ISUBT .eq. 1089) then
          call qaxis
c
c...RAPID
c
      else if (ISUBT .eq. 5) then
          call rapid
c
c...REGORD
c
      else if (ISUBT .eq. 1110) then
          call regodr
c
c...REWIND
c
      else if (ISUBT .eq. 1006) then
          call rewind
c
c...RETRCT
c
      else if (ISUBT .eq. 7) then
          call retrct (1)
c
c...ROTABL
c...ROTHED
c
      else if (ISUBT .eq. 1026 .or. ISUBT .eq. 1035) then
          call rotabl
c
c...SEQNO
c
      else if (ISUBT .eq. 1019) then
          call seqno
c
c...SELCTL
c
      else if (ISUBT .eq. 1056) then
          call selctl
c
c...SELECT/TOOL
c
      else if (ISUBT .eq. 1074 .and. MXCL .gt. 1 .and.
     1        TOOLFL(18) .eq. 1) then
          call selctl
c
c...SET
c
      else if (ISUBT .eq. 1087) then
          call set
c
c...SLOWDN
c
      else if (ISUBT .eq. 1063) then
          call slowdn
c
c...SMOOTH
c
      else if (ISUBT .eq. 1085) then
          call smooth
c
c...SPINDL
c
      else if (ISUBT .eq. 1031) then
          call spindl
c
c...STOP
c
      else if (ISUBT .eq. 2) then
          call stop
c
c...THREAD
c
      else if (ISUBT .eq. 1036) then
          call thread
c
c...TMARK
c
      else if (ISUBT .eq. 1005) then
          call tmark
c
c...TOOLNO
c
      else if (ISUBT .eq. 1025) then
          call toolno
c
c...TOOLPN
c
      else if (ISUBT .eq. 1053) then
          call simpin (cmsg,kerr)
c
c...TRANS
c
      else if (ISUBT .eq. 1037) then
          call trans
c
c...TURRET
c
      else if (ISUBT .eq. 1033) then
          call turret
c
c...UNLOAD
c
      else if (ISUBT .eq. 10) then
          call unload
c
c...Unrecognized Major word
c
      else
          call psterr (2,'INVMAJOR',' ',0)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  psword (kfl,klst,cmsg,kerr)
c
c   FUNCTION:  This routine processes Register style post processor
c              commands.
c
c   INPUT:  none.
c
c   OUTPUT: kfl     I*4  D1  0 = This record was processed in this rou-
c                            tine.  1 = This record was not recognized
c                            by this routine, process it somewhere else.
c
c           klst    I*4  D1  0 = Do not output this record to listing
c                            file.  1 = Output it.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine psword (kfl,klst,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (LSTPC ,KPOSMP(0083)), (IERRPC,KPOSMP(0085))
      equivalence (IPRTOP,KPOSMP(1176)), (IPERRF,KPOSMP(1177))
c
      integer*4 ISUBT,MXCL,IPSTWD(50),LSTPC,IERRPC,IPRTOP,IPERRF
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      equivalence (ERRMAC,CPOSMP(0193)), (LPSTWD,CPOSMP(0217))
      equivalence (LMACRO,CPOSMP(0861))
c
      character*24 LMACRO,ERRMAC
      character*512 LPSTWD
c
      integer*4 kfl,kerr,klst
c
      character*(*) cmsg
c
c...Try PREGEN command first
c
      call pgword (kfl,klst,cmsg,kerr)
      if (kfl .eq. 0) go to 8000
      kfl    = 0
      klst   = 0
c
c...APPEND
c
      if (ISUBT .eq. 1199) then
          call append
c
c...AUXFUN
c
      else if (ISUBT .eq. 1022) then
          call auxfun
c
c...FORCE
c
      else if (ISUBT .eq. 1106) then
          call force
c
c...INSERT
c
      else if (ISUBT .eq. 1046) then
          call insert
c
c...OPSKIP
c
      else if (ISUBT .eq. 1012) then
          call opskip
c
c...PREFUN
c
      else if (ISUBT .eq. 1048) then
          call prefun
c
c...REGORD
c
      else if (ISUBT .eq. 1110) then
          call regodr
c
c...Unrecognized Major word
c
      else
          kfl    = 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pgword (kfl,klst,cmsg,kerr)
c
c   FUNCTION:  This routine processes PREGEN post processor commands.
c
c   INPUT:  none.
c
c   OUTPUT: kfl     I*4  D1  0 = This record was processed in this rou-
c                            tine.  1 = This record was not recognized
c                            by this routine, process it somewhere else.
c
c           klst    I*4  D1  0 = Do not output this record to listing
c                            file.  1 = Output it.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pgword (kfl,klst,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (LSTPC ,KPOSMP(0083)), (IERRPC,KPOSMP(0085))
      equivalence (IPRTOP,KPOSMP(1176)), (IPERRF,KPOSMP(1177))
c
      integer*4 ISUBT,MXCL,IPSTWD(50),LSTPC,IERRPC,IPRTOP,IPERRF
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      equivalence (ERRMAC,CPOSMP(0193)), (LPSTWD,CPOSMP(0217))
      equivalence (LMACRO,CPOSMP(0861))
c
      character*24 LMACRO,ERRMAC
      character*512 LPSTWD
c
      integer*4 kfl,kerr,klst
c
      character*(*) cmsg
c
      integer*4 nc,strlen1,iary(2)
c
      character*80 ldat
c
c...Initialize routine
c
      kfl    = 1
      klst   = 0
c
c...ERROR
c
      if (ISUBT .eq. 1103) then
          kfl    = 0
          if (IMACPT .ge. 1 .and. IMACHD(1,IMACPT) .ne. 1103) then
              IERRPC = LSTPC
              ERRMAC = LMACRO
          endif
          nc     = strlen1(LPSTWD)
          call touppr (LPSTWD,ldat)
c
c...Output user message as warning instead of error if the first
c   five characters are "WARN:"
c
          if (ldat(1:5) .eq. 'WARN:') then
              call errtxt (LPSTWD(6:nc),ldat)
              call psterr(1,ldat,'',-1)
          else
              call errtxt(LPSTWD(1:nc),ldat)
              call psterr (2,ldat,'',-1)
          endif
c
c...IFTOL
c
      else if (ISUBT .eq. 1108) then
          kfl    = 0
          if (MXCL .ne. 1 .or. IPSTWD(1) .ne. 0) go to 9000
          IFTOL  = PSTWD(1)
c
c...LISTING
c
      else if (ISUBT .eq. 1101) then
          kfl    = 0
          if (MXCL .ne. 1) go to 9000
c
c......Listing/on
c
          if (IPSTWD(1) .eq. 71) then
              ILSTOP = 1
c
c......Listing/off
c
          else if (IPSTWD(1) .eq. 72) then
              ILSTOP = 0
c
c......Listing/page
c
          else if (IPSTWD(1) .eq. 5021) then
              call lsthed (cmsg,kerr)
c
          else
              go to 9000
          endif
c
c...PPRINT
c
      else if (ISUBT .eq. 1044) then
          call clrmot (2)
          kfl    = 0
          klst   = 1
          call pprint
c
c...PRINT
c
      else if (ISUBT .eq. 1102) then
          kfl    = 0
          nc     = strlen1(LPSTWD)
          call lstout (LPSTWD,nc,cmsg,kerr)
c
c...PRINTF
c
      else if (ISUBT .eq. 1104) then
          kfl    = 0
          if (MXCL .lt. 1) go to 9000
c
c......Printf/on
c
          if (IPSTWD(1) .eq. 71) then
              IPRTOP = 1
c
c......Printf/off
c
          else if (IPSTWD(1) .eq. 72) then
              call errtxt ('PRTOFF',ldat)
              nc     = strlen1(ldat)
              call prtout (' ',1)
              call prtout (ldat,nc)
              call prtout (' ',1)
              IPRTOP = 0
c
c......Printf/page
c
          else if (IPSTWD(1) .eq. 5021) then
              if (IPRTOP .eq. 1) call prthed (1)
c
c......Printf/record,n
c
          else if (IPSTWD(1) .eq. 5026) then
              if (MXCL .ne. 2 .or. PSTWD(2) .lt. 1 .or.
     1            PSTWD(2) .gt. 10) go to 9000
              iary(1) = PSTWD(2)
              iary(2) = 0
              call prtrec (iary)
c
          else
              go to 9000
          endif
c
c......PSTERR
c
      else if (ISUBT .eq. 1107) then
          kfl    = 0
          IPERRF = 1
          if (MXCL .ne. 0) go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Syntax error
c
 9000 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  machin (kopt,cmsg,kerr)
c
c   FUNCTION:  This routine searches the clfile for the MACHIN/pstnam
c              card and stores its parameters.  It also searches for the
c              first PARTNO card.
c
c   INPUT:  none.
c
c   OUTPUT: kopt    I*4  Dn  Options specified on MACHIN card.  These
c                            will override any options stored in the
c                            Machine descriptor file.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine machin (kopt,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IFIREC,KPOSMP(0057)), (IFIPT ,KPOSMP(0058))
      equivalence (NPT   ,KPOSMP(0059)), (NPARTN,KPOSMP(0075))
c
      integer*4 ITYPE,ISUBT,MXCL,IPSTWD(50),IFIREC,IFIPT,NPT,NPARTN
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      integer*4 IPSTNM(2)
      character*40 LPSTNM
      equivalence (PSTWD(1),LPSTNM,IPSTNM)
c
      equivalence (LPARTN,CPOSMP(0067)), (LPSTWD,CPOSMP(0217))
c
      character*66 LPARTN
      character*512 LPSTWD
c
      integer*4 kopt(20),kerr
c
      character*(*) cmsg
c
      integer*4 i,imach,nop,inc,ierr,inum,ifl,nc,strlen1,iprept
c
      real*8 opmn(8),opmx(8)
c
      character*6 lpgm
      character*80 msg
c
      data nop /8/, opmn /1,1,0,-9999999999.d0,10,1,0,0/
      data opmx /2,2,9999999999.d0,9999999999.d0,512,3,3,1/
      data lpgm /'PWORKS'/
c
c...Initialize routine
c
      imach  = PREPT
      iprept = PREPT
      NPARTN = 0
      LPARTN = ' '
      SFRMBUF(1) = ' '
      NPT    = 3
      do 50 i=1,20,1
          kopt(i) = -999999
   50 continue
c
c...Read clfile record
c
  100 call clread (IFIREC,IFIPT,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Is this a MACHIN/PWORKS card
c
      if (ITYPE .eq. 2000 .and. ISUBT .eq. 1015 .and.
     1        PREPT .lt. 10) then
          ifl    = 0
c
c......Break out long post-processor name
c
          if (IPSTNM(1) .eq. -1 .and. IPSTNM(2) .gt. 8 .and.
     1            IPSTNM(2) .le. 40) then
              nc     = IPSTNM(2)
              LPSTNM(1:nc) = LPSTNM(9:nc+8)
              inc    = (nc+7) / 8 + 2
          else
              nc     = strlen1(LPSTNM(1:8))
              if (LPSTNM(1:6) .eq. lpgm .and.
     1            ichar(LPSTNM(7:7)) .le. 32) then
                  LPSTNM(1:8) = lpgm
                  nc = strlen1(lpgm)
              endif
              inc    = 2
          endif
          if (LPSTNM(1:nc) .ne. lpgm) then
              if (iprept .eq. 0) then
                  call opnmch (LUNSC4,LPSTNM(1:nc),msg,ierr)
                  if (ierr .eq. 0) then
                      call clsfil (LUNSC4)
                      ifl    = 1
                      PREPT  = PREPT  + 1
                      PSTNAM(PREPT) = LPSTNM(1:nc)
                  endif
              endif
          else
              if (PREPT .eq. 0) ifl    = 2
          endif
c
          if (ifl .ne. 0) then
              if (MXCL .lt. ifl .or. MXCL .gt. inc+9) go to 9100
              do 500 i=inc,MXCL,1
                  if (IPSTWD(i) .eq. 144) go to 600
                  if (IPSTWD(i) .ne. 0) go to 9100
                  PREPT  = PREPT  + 1
                  inum   = PSTWD(i)
                  call itoc (inum,PSTNAM(PREPT),nc,0)
  500         continue
              go to 650
c
  600         if (i+1 .ge. MXCL) go to 9100
              do 620 i=i+1,MXCL,2
                  if (IPSTWD(i) .ne. 0) go to 9100
                  if (PSTWD(i) .lt. 1 .or. PSTWD(i) .gt. nop) go to 9100
                  inc    = PSTWD(i)
                  if (PSTWD(i+1) .lt. opmn(inc) .or.
     1                PSTWD(i+1) .gt. opmx(inc)) go to 9100
                  kopt(inc) = PSTWD(i+1)
  620         continue
  650         imach  = 1
          endif
c
c...PARTNO card
c
      else if (ITYPE .eq. 2000 .and. ISUBT .eq. 1045) then
          if (NPARTN .eq. 100) then
              if (imach .eq. 1) go to 8000
          else
              NPARTN = NPARTN + 1
              if (NPARTN .eq. 1) LPARTN = LPSTWD
              SFRMBUF(NPARTN) = LPSTWD
          endif
c
c...FINI card encountered
c
      else if (ITYPE .eq. 14000) then
          if (imach .eq. 0) go to 9000
          go to 8000
      endif
      go to 100
c
c...End of routine
c
 8000 return
c
c...MACHIN card not found
c
 9000 call errtxt ('NOMACHIN',cmsg)
      kerr   = 1
      go to 8000
c
c...MACHIN card syntax error
c
 9100 call errtxt ('MACHSYN',cmsg)
      kerr   = 1
      go to 8000
      end
