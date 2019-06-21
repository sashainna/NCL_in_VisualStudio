c
c***********************************************************************
c
c   FILE NAME:  postp
c   CONTAINS:
c               postp   ppword  machin
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        postp.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:52
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
      equivalence (MULTAX,KPOSMP(0056)), (IFIREC,KPOSMP(0057))
      equivalence (IFIPT ,KPOSMP(0058)), (LSTPC ,KPOSMP(0083))
      equivalence (IERRPC,KPOSMP(0085))
c
      integer*4 ISN,ITYPE,ISUBT,MXCL,MULTAX,IFIREC,IFIPT,LSTPC,IERRPC
c
      equivalence (CLPT  ,POSMAP(0491)), (CLSAV ,POSMAP(0738))
c
      real*8 CLPT(240),CLSAV(6)
c
      equivalence (ERRMAC,CPOSMP(0209)), (LPSTWD,CPOSMP(0217))
      equivalence (LMACRO,CPOSMP(0861))
c
      character*24 LMACRO,ERRMAC
      character*512 LPSTWD
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ifl,ifini,inc
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
c...Get the next clfile record
c...from a called Macro
c
      ifini  = 0
  100 if (IMACPT .ne. 0) then
          call precmp (cmsg,kerr)
          if (kerr .ne. 0) go to 9000
      endif
c
c...Read the clfile record
c
      if (IMACPT .eq. 0) then
          if (ifini .eq. 1) then
              ITYPE  = 14000
              go to 1000
          else
              call clread (IFIREC,IFIPT,1,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
      endif
c
c...Post word
c
      if (ITYPE .eq. 2000) then
c
c......Check for Macro call
c
          call ppcall (ifl,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          if (ifl .eq. 0) go to 100
c
c......No Macro call
c
          call ppword (ifl,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          if (ifl .eq. 0) go to 100
c
c...Motion record
c
      else if (ITYPE .eq. 5000 .or. ITYPE .eq. 5210) then
c
c......Check for Macro call
c
          call ppcall (ifl,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          if (ifl .eq. 0) go to 100
c
c......Save last Cl point
c
          if (MULTAX .eq. 0) then
              inc    = MXCL   * 3
              CLSAV(1) = CLPT(inc-2)
              CLSAV(2) = CLPT(inc-1)
              CLSAV(3) = CLPT(inc)
          else
              inc    = MXCL   * 6
              CLSAV(1) = CLPT(inc-5)
              CLSAV(2) = CLPT(inc-4)
              CLSAV(3) = CLPT(inc-3)
              CLSAV(4) = CLPT(inc-2)
              CLSAV(5) = CLPT(inc-1)
              CLSAV(6) = CLPT(inc)
          endif
c
c......Multax record
c
      else if (ITYPE .eq. 9000) then
          if (IMULTO .ne. -1) go to 100
c
c......Fini record
c
      else if (ITYPE .eq. 14000) then
          ISUBT  = 4012
          MXCL   = 0
          ifini  = 1
          call ppcall (ifl,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
          if (ifl .eq. 0) go to 100
      endif
c
c...Write clfile record
c
 1000 call clwrit (INEREC,INEPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...FINI record
c...Write last record
c
      if (ITYPE .ne. 14000) go to 100
      if (IOPFL(4) .eq. 1) call wrncl (LUNSC3,INEREC,JCOBUF,cmsg,kerr)
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
c   SUBROUTINE:  ppword (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine processes post processor commands.
c
c   INPUT:  none.
c
c   OUTPUT: kfl     I*4  D1  0 = do not output this record to the cl-
c                            file.  1 = output this record to the cl-
c                            file.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ppword (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (LSTPC ,KPOSMP(0083)), (IERRPC,KPOSMP(0085))
c
      integer*4 ITYPE,ISUBT,MXCL,IPSTWD(50),LSTPC,IERRPC
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
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,strlen1
c
      character*80 ldat
c
c...Initialize routine
c
      kfl    = 1
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
          call errtxt (LPSTWD(1:nc),ldat)
          call lsterr (ldat,cmsg,kerr)
c
c...FORCE, APPEND, REGORD
c
      else if (ISUBT .eq. 1106 .or. ISUBT .eq. 1199 .or.
     1         ISUBT .eq. 1110) then
          kfl    = 0
c
c...IFTOL
c
      else if (ISUBT .eq. 1108) then
         kfl = 0
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
c...MACHIN
c
      else if (ISUBT .eq. 1015) then
          call machin (kfl,cmsg,kerr)
c
c...MULTAX
c
      else if (ISUBT .EQ. 1105) then
          kfl    = 0
          if (MXCL .ne. 1 .or. (IPSTWD(1) .ne. 71 .and.
     1        IPSTWD(1) .ne. 72)) go to 9000
          ITYPE  = 9000
          if (IPSTWD(1) .eq. 71) then
              ISUBT  = 1
              IMULTO = 1
          else
              ISUBT  = 0
              IMULTO = 0
          endif
          kfl    = 1
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
c
c...PSTERR
c
      else if (ISUBT .eq. 1107) then
          kfl    = 0
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
c   SUBROUTINE:  machin (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine processes the MACHIN card(s).  The MACHIN
c              card for this post will be ignored and all other MACHIN
c              cards will be output to the clfile and put on a stack of
c              posts to run, if this option was specified.
c
c   INPUT:  none.
c
c   OUTPUT: kfl     I*4  D1  0 = do not output this record to the cl-
c                            file.  1 = output this MACHIN card to the
c                            clfile.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine machin (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      character*6 LPSTNM
      equivalence (PSTWD(1),LPSTNM)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      character*6 lpgm
c
      data lpgm /'PMACRO'/
c
c...MACHIN/PMACRO
c
      if (LPSTNM .eq. lpgm) then
          kfl    = 0
c
c...MACHIN/post
c
      else
          kfl    = 1
          if (IOPFL(7) .eq. 1) then
              if (MCHPT .eq. 10) go to 9000
              MCHPT  = MCHPT  + 1
              MCHNAM(MCHPT) = LPSTNM
          endif
      endif
c
c...End of routine
c
 8000 return
c
c...Too many MACHIN cards
c
 9000 call errtxt ('MACHMAX',cmsg)
      kerr   = 1
      go to 8000
      end
