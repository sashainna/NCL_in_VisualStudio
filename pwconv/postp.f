c
c***********************************************************************
c
c   FILE NAME:  postp
c   CONTAINS:
c               postp   trans  preadj  prtout  pstsum
c
c     COPYRIGHT 2011 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        postp.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:14
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
      equivalence (IFIPT ,KPOSMP(0058))
c
      integer*4 ISN,ITYPE,ISUBT,MXCL,MULTAX,IFIREC,IFIPT
c
      equivalence (CLPT  ,POSMAP(0491)), (CLSAV ,POSMAP(0738))
c
      real*8 CLPT(240),CLSAV(6)
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 inc,ierr
c
c...Read the clfile record
c
  100 call clread (IFIREC,IFIPT,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...MasterCam TRANS record
c
      if (ITYPE .eq. 2000 .and. ISUBT .eq. 1037) then
          call trans (ierr)
          if (ierr .eq. 0) go to 100
c
c...Motion record
c
      else if (ITYPE .eq. 5000 .or. ITYPE .eq. 5210) then
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
c...Multax record
c
      else if (ITYPE .eq. 9000) then
          if (IMULTO .ne. -1) go to 100
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
 9000 call lsterr (cmsg,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 100
      end
c
c***********************************************************************
c
c   SUBROUTINE:  trans
c
c   FUNCTION:  This routine processes the following command(s).
c
c                 TRANS/mx
c
c   INPUT:  none.
c
c   OUTPUT:
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine trans (kerr)
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MXTRAN,KPOSMP(4003))
c
      integer*4 MXCL,IPSTWD(50),MXTRAN
c
      equivalence (PSTWD ,POSMAP(0441)), (TRAMAT,POSMAP(4013))
c
      real*8 PSTWD(50),TRAMAT(12)
c
      integer*4 kerr
c
      integer*4 i
c
c...TRANS/mx
c
      kerr   = 1
      if (MXCL .eq. 12) then
          do 100 i=1,12,1
              if (IPSTWD(i) .ne. 0) go to 8000
              TRAMAT(i) = PSTWD(i)
  100     continue
          MXTRAN = 1
          kerr   = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  preadj (glin,gout,gtvi,gtvo)
c
c   FUNCTION:  This routine modifies the input CLPT's prior to all other
c              manipulation of points, applying the following values.
c
c                 TRANS/MX
c
c   INPUT:  glin    R*8  D3  -  Input cl point.
c
c           gtvi    R*8  D3  -  Input tool axis vector.
c
c   OUTPUT: gout    R*8  D3  -  Modified cl point.
c
c           gtvo    R*8  D3  -  Unchanged tool axis vector.
c
c***********************************************************************
c
      subroutine preadj (glin,gout,gtvi,gtvo)
c
      include 'post.inc'
c
      real*8 glin(3),gout(3),gtvi(3),gtvo(3)
c
      equivalence (MXTRAN,KPOSMP(4003))
c
      integer*4 MXTRAN
c
      equivalence (TRAMAT,POSMAP(4013))
c
      real*8 TRAMAT(12)
c
c...Apply TRANS/mx
c
      if (MXTRAN .eq. 1) then
          call matpta (glin,gout,TRAMAT,1)
      endif
c
c...End of routine
c
 8000 return
      end
c
c...PRTOUT
c
      subroutine prtout (cdat,knc)
c
      integer*4 knc
c
      character*(*) cdat
c
      return
      end
c
c...PSTSUM
c
      subroutine pstsum
c
      return
      end
