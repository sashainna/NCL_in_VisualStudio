c
c***********************************************************************
c
c   FILE NAME:  clfile
c   CONTAINS:
c               clread  clwrit  cldwrt  clrew   clmark  clumrk  nclrd
c               nclwr   catlod  rdcatc  gethdr  cl4sto  clpt52
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        clfile.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        08/11/16 , 10:03:57
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  clread (krec,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine reads a neutral format clfile record and
c              sets up global arrays depending on the type of record.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c           kpt     I*4  D1  Pointer to next logical clfile record
c                            within physical record.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine clread (krec,kpt,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLREC,KPOSMP(0002))
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MULTAX,KPOSMP(0056)), (NPT   ,KPOSMP(0059))
      equivalence (NCUT  ,KPOSMP(0062)), (NCUTDS,KPOSMP(0063))
      equivalence (MXFLAG,KPOSMP(4002))
      equivalence (MXTRAN,KPOSMP(4003)), (MOTEXP,KPOSMP(4211))
c
      integer*4 ISN,ICLREC,ITYPE,ISUBT,MXCL,IPSTWD(50),MULTAX,
     -          NPT,MXFLAG,MXTRAN,NCUT,NCUTDS,MOTEXP
c
      equivalence (METCNV,POSMAP(0004)), (CLPOS ,POSMAP(0222))
      equivalence (PSTWD ,POSMAP(0441)), (TLATOL,POSMAP(0057))
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (CLSAV ,POSMAP(0201)), (CUTTER,POSMAP(0744))
      equivalence (CUDISP,POSMAP(0759)), (CUTOFS,POSMAP(0758))
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
      equivalence (CLPTPP,POSMAP(4605)), (CIRBPP,POSMAP(4845))
c
      real*8 METCNV,PSTWD(50),CLPT(240),CIRBUF(7),CLSAV(21),
     1       CUTTER(7),REFMAT(12),TRAMAT(12),CUDISP(7),
     2       CUTOFS,CLPTPP(240),CIRBPP(7),CLPOS(21),TLATOL
c
      integer*2 ICLDAT(200)
      integer*4 JCLDAT(200)
      character*8 lclt,lclnm,lclrv
      character*11 lcld
      character*66 LCLDAT
      character*80 lclf
c
      equivalence (RCLDAT,ICLDAT,JCLDAT,LCLDAT)
      equivalence (RCLDAT(1),lclf), (RCLDAT(11),lcld)
      equivalence (RCLDAT(13),lclt), (RCLDAT(14),lclnm)
      equivalence (RCLDAT(15),lclrv)
c
      equivalence (LPSTWD,CPOSMP(0001))
c
      character*66 LPSTWD
c
      integer*4 krec,kpt,kerr,is1,is4
c
      character*(*) cmsg
c
      integer*4 i,j,inc,inbuf(4),nwds,ix1,ix4,mxm
c
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 /0/, is4 /3/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 /3/, is4 /0/
C WNT-DOS-DEC-END
c
c...Read logical clfile record
c
      if (ICLF .eq. 2) then
          call srcrd (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
      else if (ICLF .eq. 1) then
          call nclrd (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
      else if (ICLF .eq. 3) then
          call srcrd (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
c
c...Store integer values
c
      ISN    = inbuf(1)
      ICLREC = inbuf(2)
      ITYPE  = inbuf(3)
      ISUBT  = inbuf(4)
      MXCL   = nwds
c
c... Fix overflow of integer*2 ISN in the clfiles.
c
      if (ICLF .eq. 2) then
          if (ISN .lt. 0) ISN = 32768 + (32768 + ISN)
      else
          if (ISN .lt. 0) ISN = 65536 + (32768 + ISN)
      endif
c
c...Post word
c
      if (ITYPE .eq. 2000) then
c
c......LETTER, PARTNO, INSERT and PPRINT
c
          if (ISUBT .ge. 1043 .and. ISUBT .le. 1046) then
              LPSTWD = LCLDAT(1:66)
c
c......Major/Minor word
c
          else
c
c......Support MACHIN names over 6 characters
c
              mxm    = 0
              if (ISUBT .eq. 1015) then
                  if (JCLDAT(1) .eq. -1 .and. JCLDAT(2) .gt. 8 .and.
     1                    JCLDAT(2) .le. 40) then
                      mxm    = (JCLDAT(2)+7) / 8 + 1
                  else
                      mxm    = 1
                  endif
              endif
              do 300 i=1,nwds,1
                  if (ICLDAT(i*4-is4) .eq. 0 .and.
     1                ICLDAT(i*4-is1) .ne. 0 .and. i .gt. mxm) then
                      IPSTWD(i) = ICLDAT(i*4-is1)
                      PSTWD(i) = 0.
                  else
                      IPSTWD(i) = 0
                      PSTWD(i) = RCLDAT(i)
                  endif
  300         continue
          endif
c
c...Circle record
c
      else if (ITYPE .eq. 3000) then
          do 500 i=1,7,1
              if (i .ge. 4 .and. i .le. 6) then
                  CIRBUF(i) = RCLDAT(i)
              else
                  CIRBUF(i) = RCLDAT(i) * METCNV
              endif
  500     continue
          call copyn (CIRBUF,CIRBPP,7)
          call preadj (CIRBUF(1),CIRBUF(1),CIRBUF(4),CIRBUF(4))
          if (MXTRAN .eq. 1) call matpta (CIRBUF(4),CIRBUF(4),TRAMAT,3)
          if (MXFLAG .eq. 1) call matpta (CIRBUF(4),CIRBUF(4),REFMAT,3)
c
c...Motion record
c
      else if (ITYPE .eq. 5000) then
          MOTEXP = 0
          inc    = 0
          do 800 i=0,nwds-1,NPT
              do 700 j=1,NPT,1
                  if (j .gt. 3) then
                      CLPT(inc+j) = RCLDAT(i+j)
                  else
                      CLPT(inc+j) = RCLDAT(i+j) * METCNV
                  endif
  700         continue
              call copyn (CLPT(inc+1),CLPTPP(inc+1),NPT)
              ix1  = inc + 1
              ix4  = inc + 4
              call preadj (CLPT(inc+1),CLPT(inc+1),CLPT(ix4),CLPT(ix4))
              if (NPT .eq. 6) then
                  if (MXTRAN .eq. 1)
     -                call matpta (CLPT(ix4),CLPT(ix4),TRAMAT,3)
                  if (MXFLAG .eq. 1)
     -                call matpta (CLPT(ix4),CLPT(ix4),REFMAT,3)
              end if
              inc    = inc    + NPT
  800     continue
          MXCL   = inc    / NPT
c
c...Expanded motion record (5200)
c
      else if (ITYPE .eq. 5200) then
          call clpt52 (nwds,CLPT,CLFWD,CLPTE1,CLPTE2,CLPTE3,CLPTE4)
cc          ITYPE  = 5000
cc          NPT    = 6
c
c...Direction record (5210)
c
      else if (ITYPE .eq. 5210) then
          call clpt52 (nwds,CLPOS,CLPOS(7),CLPOS(10),CLPOS(13),
     -                      CLPOS(16),CLPOS(19))
c
c...Cutter
c
      else if (ITYPE .eq. 6000) then
          NCUT   = MXCL
          if (NCUT .gt. 7) NCUT = 7
          NCUTDS = NCUT
c
c...Updated Aug-04-2016, KC
c...Cutter angles not modified by unit conversion
c
          do 900 i=1,NCUT,1
              if (i .le. 4 .or. i .eq. 7) then
                  CUTTER(i) = RCLDAT(i) * METCNV
              else
                  CUTTER(i) = RCLDAT(i)
              endif
              CUDISP(i) = RCLDAT(i)
  900     continue
c
c...Displayed Cutter
c
      else if (ITYPE .eq. 7100) then
          if (ISUBT .eq. 1) then
              NCUTDS = MXCL
              if (NCUTDS .gt. 7) NCUTDS = 7
              do 950 i=1,NCUTDS,1
                  CUDISP(i) = RCLDAT(i)
  950         continue
          endif
c
c...Clfile Header
c
      else if (ITYPE .eq. 7400) then
          CLNAME = lclf
          CLDATE = lcld
          CLTIME = lclt
          CAMNAM = lclnm
          CAMREV = lclrv
c
c...Multax
c
      else if (ITYPE .eq. 9000) then
          MULTAX = 1 - ISUBT
          NPT    = 3
          if (MULTAX .eq. 1) NPT = 6
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clwrit (krec,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine takes the global arrays and creates a neu-
c              tral clfile logical record.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to write.
c
c           kpt     I*4  D1  Pointer to next logical clfile record
c                            within physical record.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine clwrit (krec,kpt,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLREC,KPOSMP(0002))
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MULTAX,KPOSMP(0056)), (NPT   ,KPOSMP(0059))
      equivalence (ICLOUT,KPOSMP(0084))
c
      integer*4 ISN,ICLREC,ITYPE,ISUBT,MXCL,IPSTWD(50),MULTAX,ICLOUT,NPT
c
      equivalence (PSTWD ,POSMAP(0441))
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (CLSAV ,POSMAP(0201))
c
      real*8 PSTWD(50),CLPT(240),CIRBUF(7),CLSAV(21)
c
      integer*2 ICLDAT(200)
      integer*4 JCLDAT(200)
      character*66 LCLDAT
      equivalence (RCLDAT,ICLDAT,JCLDAT,LCLDAT)
c
      equivalence (LPSTWD,CPOSMP(0001))
c
      character*66 LPSTWD
c
      integer*4 krec,kpt,kerr
c
      character*(*) cmsg
c
      integer*4 inbuf(4),nwds,i,inc,nopt,imult,j,is1
c
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 /0/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 /3/
C WNT-DOS-DEC-END
c
c...Store integer values
c
      if (PGMNAM .eq. 'PostMacro') ICLOUT  = ICLOUT + 1
      inbuf(1) = ISN
      inbuf(2) = ICLOUT
      inbuf(3) = ITYPE
      inbuf(4) = ISUBT
      nwds     = MXCL
c
c...Post word
c
      if (ITYPE .eq. 2000) then
c
c......LETTER, PARTNO, INSERT and PPRINT
c
          if (ISUBT .ge. 1043 .and. ISUBT .le. 1046 .or.
     -        ISUBT .eq. 1199) then
              LCLDAT(1:66) = LPSTWD
c
c......Major/Minor word
c
          else
              do 300 i=1,nwds,1
                  if (IPSTWD(i) .ne. 0) then
                      ICLDAT(i*4-3) = 0
                      ICLDAT(i*4-2) = 0
                      ICLDAT(i*4-1) = 0
                      ICLDAT(i*4) = 0
                      ICLDAT(i*4-is1) = IPSTWD(i)
                  else
                      RCLDAT(i) = PSTWD(i)
                  endif
  300         continue
          endif
c
c...Circle record
c
      else if (ITYPE .eq. 3000) then
          do 500 i=1,7,1
              RCLDAT(i) = CIRBUF(i)
  500     continue
c
c...Motion record
c
      else if (ITYPE .eq. 5000) then
          imult  = MULTAX
          if (IMULTO .ne. -1) imult = IMULTO
          inc    = 0
          nopt   = 3
          if (imult .eq. 1) nopt = 6
          do 800 i=0,nwds*NPT-1,NPT
              do 700 j=1,nopt,1
                  RCLDAT(inc+j) = CLPT(i+j)
  700         continue
              inc    = inc    + nopt
  800     continue
          nwds   = inc
      endif
c
c...Write clfile record &
c...Listing record
c
      call lstdat (inbuf,RCLDAT,nwds,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      if (IOPFL(4) .eq. 1)
     1        call nclwr (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cldwrt (krec,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine writes out a neutral clfile record from the
c              %CLDATA array.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to write.
c
c           kpt     I*4  D1  Pointer to next logical clfile record
c                            within physical record.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cldwrt (krec,kpt,cmsg,kerr)
c
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clrew
c
c   FUNCTION:  This routine rewinds the input clfile.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clrew
c
      include 'post.inc'
      include 'clnrd.inc'
c
      equivalence (IFIREC,KPOSMP(0057)), (IFIPT ,KPOSMP(0058))
c
      integer*4 IFIREC,IFIPT
c
c...Rewind input clfile
c
      IFIREC = 0
      IFIPT  = 10000
      LSTPTC = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clmark
c
c   FUNCTION:  This routine saves the current position in the clfile/
c              Macro when looking ahead.  It also saves certain varia-
c              bles, but not the entire clfile record (PSTWD,CLPT,etc).
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
c...added level params
c...Yurong 5/28/98
c
      subroutine clmark(kst)
c
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLREC,KPOSMP(0002))
      equivalence (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (IFIREC,KPOSMP(0057)), (IFIPT ,KPOSMP(0058))
      equivalence (NPT   ,KPOSMP(0059))
      equivalence (LSTPC ,KPOSMP(0083)), (ICLOUT,KPOSMP(0084))
      equivalence (ICLMSV,KPOSMP(0151)), (PICLMSV,KPOSMP(0328))
      integer*4 kst
c
      integer*4 ISN,ITYPE,ISUBT,MXCL,IFIREC,IFIPT,LSTPC,ICLOUT,
     1          ICLMSV(20),PICLMSV(20),NPT,ICLREC
c
      equivalence (LCLMSV,CPOSMP(0001)), (LMACRO,CPOSMP(0861))
c
      character*24 LCLMSV(2),LMACRO
c
      integer*4 i
c
c...Save clfile and Macro pointers
c
      if (kst.eq.1) then
         ICLMSV(1) = IFIREC
         ICLMSV(2) = IFIPT
         ICLMSV(3) = ISN
         ICLMSV(4) = ICLOUT
         ICLMSV(5) = ITYPE
         ICLMSV(6) = ISUBT
         ICLMSV(7) = MXCL
         ICLMSV(8) = NPT
         ICLMSV(17) = ICLREC
         if (IMACPT .ne. 0) then
            ICLMSV(9) = LSTPC
            ICLMSV(10) = IPC
            ICLMSV(11) = IMACPT
            do 100 i=1,5,1
               ICLMSV(i+11) = IMACHD(i,IMACPT)
  100       continue
            LCLMSV(1) = LMACRO
         endif
       else
         PICLMSV(1) = IFIREC
         PICLMSV(2) = IFIPT
         PICLMSV(3) = ISN
         PICLMSV(4) = ICLOUT
         PICLMSV(5) = ITYPE
         PICLMSV(6) = ISUBT
         PICLMSV(7) = MXCL
         PICLMSV(8) = NPT
         PICLMSV(17) = ICLREC
         if (IMACPT .ne. 0) then
            PICLMSV(9) = LSTPC
            PICLMSV(10) = IPC
            PICLMSV(11) = IMACPT
            do 400 i=1,5,1
               PICLMSV(i+11) = IMACHD(i,IMACPT)
  400       continue
            LCLMSV(1) = LMACRO
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
c   SUBROUTINE:  clumrk
c
c   FUNCTION:  This routine restores the current position in the clfile/
c              Macro after looking ahead.  It also restores certain
c              variables, but not the entire clfile record (PSTWD,CLPT
c              ,etc).
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
c...added level params
c...Yurong 5/28/98
c
      subroutine clumrk(kst)
c
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLREC,KPOSMP(0002))
      equivalence (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (IFIREC,KPOSMP(0057)), (IFIPT ,KPOSMP(0058))
      equivalence (NPT   ,KPOSMP(0059))
      equivalence (LSTPC ,KPOSMP(0083)), (ICLOUT,KPOSMP(0084))
      equivalence (ICLMSV,KPOSMP(0093))
      equivalence (PICLMSV,KPOSMP(0115))
      integer*4 kst
c
      integer*4 ISN,ITYPE,ISUBT,MXCL,IFIREC,IFIPT,LSTPC,ICLOUT,
     1          ICLMSV(16), PICLMSV(16), NPT,ICLREC
c
      equivalence (LCLMSV,CPOSMP(0001)), (LMACRO,CPOSMP(0861))
c
      character*24 LCLMSV(2),LMACRO
c
      integer*4 i
c
c...Restore clfile and Macro pointers
c
      if (kst.eq.1) then
         if (ICLMSV(1) .eq. -1) go to 8000
         IFIREC = ICLMSV(1)
         IFIPT  = ICLMSV(2)
         ISN    = ICLMSV(3)
         ICLOUT = ICLMSV(4)
         ITYPE  = ICLMSV(5)
         ISUBT  = ICLMSV(6)
         MXCL   = ICLMSV(7)
         NPT    = ICLMSV(8)
         ICLREC = ICLMSV(17)
         ICLMSV(1) = -1
         if (ICLMSV(10) .ne. 0) then
             LSTPC  = ICLMSV(9)
             IPC    = ICLMSV(10)
             IMACPT = ICLMSV(11)
             do 100 i=1,5,1
                IMACHD(i,IMACPT) = ICLMSV(i+11)
  100        continue
         endif
         ICLMSV(10) = 0
       else
         if (PICLMSV(1) .eq. -1) go to 8000
         IFIREC = PICLMSV(1)
         IFIPT  = PICLMSV(2)
         ISN    = PICLMSV(3)
         ICLOUT = PICLMSV(4)
         ITYPE  = PICLMSV(5)
         ISUBT  = PICLMSV(6)
         MXCL   = PICLMSV(7)
         NPT    = PICLMSV(8)
         ICLREC = PICLMSV(17)
         PICLMSV(1) = -1
         if (PICLMSV(10) .ne. 0) then
             LSTPC  = PICLMSV(9)
             IPC    = PICLMSV(10)
             IMACPT = PICLMSV(11)
             do 400 i=1,5,1
                IMACHD(i,IMACPT) = PICLMSV(i+11)
  400        continue
         endif
         PICLMSV(10) = 0
      endif
         LMACRO = LCLMSV(1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  nclrd (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c
c   FUNCTION:  This routine reads an NCL clfile record and converts it
c              to a neutral format.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c           kpt     I*4  D1  Pointer of to next logical clfile record
c                            within physical record.
c
c   OUTPUT: kbuf    I*4  D4  Array to receive integer clfile data.  1 =
c                            ISN, 2 = CL record #, 3 = Record type, 4 =
c                            Record sub-type.
c
c           gbuf    R*8  Dn  Array to receive real clfile data.
c
c           kmxcl   I*4  D1  Number of reals in 'gbuf'.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine nclrd (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      equivalence (ICLREC,KPOSMP(0002))
c
      integer*4 ICLREC
c
      integer*4 krec,kpt,kbuf(4),kmxcl,kerr
c
      real*8 gbuf(10)
c
      character*(*) cmsg
c
      integer*2 ibuf(4)
      integer*4 i,ipt,nwds,inc,jindex,inum,is1,is4,jisn
c
      real*8 rbuf
c
      equivalence (rbuf,ibuf)
c
c...Initialze routine
c
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 / 1/, is4 / 4/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 / 4/, is4 / 1/
C WNT-DOS-DEC-END
      kerr   = 0
      ICLREC = ICLREC + 1
      jisn   = 0
c
c...Read cl record if necessary
c
   50 kpt    = kpt    + 1
      if (kpt .ge. 36 .or. krec .ne. LSTIRC) then
          if (kpt .ge. 36) then
              krec   = krec   + 1
              kpt    = 1
          endif
          call rdncl (LUNSC1,krec,JCLBUF,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Store ISN
c
      ipt    = (kpt*4) - 3
      if (ICLBUF(ipt+1) .eq. 1001) then
          jisn   = JCLBUF(kpt*2)
          go to 50
      endif
c
c...Store integer values
c
      kbuf(1) = ICLBUF(ipt+3)
      if (jisn .ne. 0) kbuf(1) = jisn
      kbuf(2) = ICLREC
      kbuf(3) = ICLBUF(ipt+1)
      kbuf(4) = ICLBUF(ipt+2)
      nwds    = ICLBUF(ipt) - 1
      if (kbuf(3) .ne. 1000) jisn    = 0
c
c...Post word
c
      if (kbuf(3) .eq. 2000) then
          inc    = jindex (VNMAJ,kbuf(4),NNMAJ)
          if (inc .ne. 0) kbuf(4) = VAMAJ(inc)
          do 100 i=1,nwds,1
c
              kpt    = kpt    + 1
              if (kpt .ge. 36) then
                  krec   = krec   + 1
                  kpt    = 1
                  call rdncl (LUNSC1,krec,JCLBUF,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
c
              gbuf(i) = RCLBUF(kpt)
  100     continue
          if (kbuf(4) .lt. 1043 .or. kbuf(4) .gt. 1046) then
              ipt    = 1
              if (kbuf(4) .eq. 1015) ipt = 2
              do 200 i=ipt,nwds,1
                  rbuf   = gbuf(i)
                  if (ibuf(is1) .eq. 0) then
                      inum   = ibuf(is4)
                      inc    = jindex (VNMIN,inum,NNMIN)
                      if (inc .ne. 0) then
                          ibuf(is4) = VAMIN(inc)
                          gbuf(i) = rbuf
                      endif
                  endif
  200         continue
          endif
c
          kmxcl  = nwds
c
c...Circular record
c
      else if (kbuf(3) .eq. 3000) then
          kpt    = kpt    + 5
          do 500 i=1,7,1
c
              kpt    = kpt    + 1
              if (kpt .ge. 36) then
                  krec   = krec   + 1
                  kpt    = kpt    - 35
                  call rdncl (LUNSC1,krec,JCLBUF,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
c
              gbuf(i) = RCLBUF(kpt)
  500     continue
          kmxcl  = 7
c
c...Motion record
c
      else if (kbuf(3) .eq. 5000 .or. kbuf(3) .eq. 5200) then
c          kpt    = kpt    + 2
          do 800 i=1,nwds,1
c
              kpt    = kpt    + 1
              if (kpt .ge. 36) then
                  krec   = krec   + 1
                  kpt    = kpt    - 35
                  call rdncl (LUNSC1,krec,JCLBUF,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
c
              gbuf(i) = RCLBUF(kpt)
  800     continue
c          kmxcl  = nwds   - 2
          kmxcl  = nwds
c
c...Multax record
c
      else if (kbuf(3) .eq. 9000) then
c          kbuf(4)   = 1 - kbuf(4)
          kmxcl  = 0
c
c...Unsupported record type
c...Store cldata as is
c
      else
          do 1000 i=1,nwds,1
c
              kpt    = kpt    + 1
              if (kpt .ge. 36) then
                  krec   = krec   + 1
                  kpt    = 1
                  call rdncl (LUNSC1,krec,JCLBUF,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
c
              gbuf(i) = RCLBUF(kpt)
 1000     continue
          kmxcl  = nwds
      endif
c
c...End of routine
c
 8000 LSTIRC = krec
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  nclwr (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c
c   FUNCTION:  This routine converts a neutral format clfile record to
c              an NCL clfile record and writes it.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c           kpt     I*4  D1  Pointer of to next logical clfile record
c                            within physical record.
c
c   OUTPUT: kbuf    I*4  D4  Array to receive integer clfile data.  1 =
c                            ISN, 2 = CL record #, 3 = Record type, 4 =
c                            Record sub-type.
c
c           gbuf    R*8  Dn  Array to receive real clfile data.
c
c           kmxcl   I*4  D1  Number of reals in 'gbuf'.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine nclwr (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'pregen.inc'
c
      integer*4 krec,kpt,kbuf(4),kmxcl,kerr
c
      real*8 gbuf(10)
c
      character*(*) cmsg
c
      integer*2 ibuf(4),idata(4)
      integer*4 i,ipt,inc,jindex,inum,jdata(2)
c
      real*8 rcirc(5),rbuf,rdata
c
      equivalence (rbuf,ibuf)
      equivalence (idata,jdata,rdata)
c
      data rcirc /5.,0.,0.,0.,0./
c
c
c...Initialze routine
c
      kerr   = 0
c
c...Write cl record if necessary
c
      kpt    = kpt    + 1
      if (kpt .ge. 36) then
          call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          krec   = krec   + 1
          kpt    = 1
      endif
c
c...Store ISN record
c
      if (kbuf(1) .gt. 32767) then
          idata(1) = 1
          idata(2) = 1001
          jdata(2) = kbuf(1)
          RCOBUF(kpt) = rdata
          kpt    = kpt    + 1
          if (kpt .ge. 36) then
              call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              krec   = krec   + 1
              kpt    = 1
          endif
      endif
c
c...Store integer values
c
      ipt    = (kpt*4) - 3
      ICOBUF(ipt)   = kmxcl   + 1
      ICOBUF(ipt+1) = kbuf(3)
      ICOBUF(ipt+2) = kbuf(4)
      ICOBUF(ipt+3) = kbuf(1)
c
c...Post word
c
      if (kbuf(3) .eq. 2000) then
          inc    = jindex (VAMAJ,kbuf(4),NNMAJ)
          if (inc .ne. 0) ICOBUF(ipt+2) = VNMAJ(inc)
c
          if (kbuf(4) .lt. 1043 .or. kbuf(4) .gt. 1046) then
              ipt    = 1
              if (kbuf(4) .eq. 1015) ipt = 2
              do 100 i=ipt,kmxcl,1
                  rbuf   = gbuf(i)
                  if (ibuf(1) .eq. 0) then
                      inum   = ibuf(4)
                      inc    = jindex (VAMIN,inum,NNMIN)
                      if (inc .ne. 0) then
                          ibuf(1) = 0
                          ibuf(2) = 0
                          ibuf(3) = 0
                          ibuf(4) = VNMIN(inc)
                          gbuf(i) = rbuf
                      endif
                  endif
  100         continue
          endif
c
          do 200 i=1,kmxcl,1
              kpt    = kpt    + 1
              if (kpt .ge. 36) then
                  call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  krec   = krec   + 1
                  kpt    = 1
              endif
              RCOBUF(kpt) = gbuf(i)
  200     continue
c
c...Circular record
c
      else if (kbuf(3) .eq. 3000) then
          ICOBUF(ipt) = ICOBUF(ipt) + 5
          do 400 i=1,5,1
c
              kpt    = kpt    + 1
              if (kpt .ge. 36) then
                  call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  krec   = krec   + 1
                  kpt    = 1
              endif
              RCOBUF(kpt) = rcirc(i)
  400     continue
c
          do 500 i=1,7,1
c
              kpt    = kpt    + 1
              if (kpt .ge. 36) then
                  call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  krec   = krec   + 1
                  kpt    = 1
              endif
c
              RCOBUF(kpt) = gbuf(i)
  500     continue
c
c...Motion record
c
c      else if (kbuf(3) .eq. 5000) then
c          ICOBUF(ipt) = ICOBUF(ipt) + 2
c          do 700 i=1,2,1
c
c              kpt    = kpt    + 1
c              if (kpt .ge. 36) then
c                  call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
c                  if (kerr .ne. 0) go to 8000
c                  krec   = krec   + 1
c                  kpt    = 1
c              endif
c              RCOBUF(kpt) = 0.
c  700     continue
c
c          do 800 i=1,kmxcl,1
c
c              kpt    = kpt    + 1
c              if (kpt .ge. 36) then
c                  call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
c                  if (kerr .ne. 0) go to 8000
c                  krec   = krec   + 1
c                  kpt    = 0
c              endif
c
c              RCOBUF(kpt) = gbuf(i)
c  800     continue
c
c...Multax record
c
      else if (kbuf(3) .eq. 9000) then
          ICOBUF(ipt+2) = 1 - ICOBUF(ipt+2)
c
c...Unsupported record type
c...Store cldata as is
c
      else
          do 1000 i=1,kmxcl,1
c
              kpt    = kpt    + 1
              if (kpt .ge. 36) then
                  call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  krec   = krec   + 1
                  kpt    = 1
              endif
c
              RCOBUF(kpt) = gbuf(i)
 1000     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  catlod (cfnam,cmsg,kerr)
c
c   FUNCTION:  This routine reads an CATIA clfile record and converts it
c              to a neutral format stored in the binary scratch file.
c
c   INPUT:  cfnsm   C*n  D1  Input Catia clfile name.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine catlod (cfnam,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      integer*4 kerr
c
      character*(*) cfnam,cmsg
c
      equivalence (ISN   ,KPOSMP(0001))
      integer*4 ISN

      character*8 cdum
      character*20 att(4)
c
      integer*2 inum(4),ibuf(1024),inbuf(1028),mxc
      integer*4 i,j,n,m,ipt,jindex,klen,lcon,last2,inc,nwds,
     -          nvl,irecl,jbuf(512),jnum(2),is1,is2,is4,irec,
     -          ipt2,icr,icpt,imlt,jbfo(512),jtmp(512),ifini
c
      real*8 rbuf(256),rbfo(256),rnum,dumm,gts(6)
c
      equivalence (rbuf,jbuf,ibuf), (inbuf(1),mxc,jbfo)
      equivalence (inbuf(5),rbfo)
      equivalence (dumm,cdum), (rnum,inum,jnum)
c
      data cdum /' '/
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 / 1/, is2 / 2/, is4 / 4/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 / 4/, is2 / 2/, is4 / 1/
C WNT-DOS-DEC-END
c
c...Initialze routine
c
      kerr   = 0
      irec   = 0
      ipt    = 0
      ifini  = 0
      LSTPTC = 0
      icpt   = 0
      icr    = 0
      gts(4) = 0.
      gts(5) = 0.
      gts(6) = 1.
      imlt   = 0
c
c...Open Catia clfile
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 3228
      call opnfil (LUNSC2,cfnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call getfnm (LUNSC2,LCMPFI,NCCMPF,MAX_PATH)
c
c...Open scratch clfile
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'scratch'
      irecl  = 512
      call opnfil (LUNSC1,'clfile.tmp',att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Read cl record if necessary
c
  100 ipt    = ipt    + 1
      if (ipt*2 .ge. LSTPTC) then
          if (ipt*2 .ge. LSTPTC) then
              irec   = irec   + 1
              ipt    = 2
          endif
          call rdcatc (irec,n,m,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if (irec .eq. 1 .and. ipt .eq. 2) ipt = n / 2 + 2
      endif
c
c...Store integer values
c
      call gethdr (irec,ipt,inbuf,klen,lcon,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      ipt2   = ipt*2 - 1
      last2  = (ipt + klen - 1) * 2
      i      = 0
c
c...Post word
c
      if (inbuf(3) .eq. 2000) then
          nvl    = inbuf(4)
          inc    = jindex (CNMAJ,nvl,CNNMAJ)
          if (inc .ne. 0) inbuf(4) = CAMAJ(inc)
c
          if (ipt2 .gt. last2 .and. lcon .eq. 0) go to 300
          call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if (inbuf(4) .lt. 1043 .or. inbuf(4) .gt. 1046) then
c
c...Store real/integer values
c...replacing minor words when necessary
c
              i     = j / 2
              m     = 1
              if (inbuf(4) .eq. 1015) then
                  rbfo(1) = rbuf(1)
                  m = 2
              end if
              do 210 n=m,i,1
                  rnum   = rbuf(n)
                  if (jnum(1) .eq. 0) then
                      inc    = jindex (CNMIN,jnum(2),CNNMIN)
                      if (inc .ne. 0) then
                          jnum(2) = 0
                          inum(4) = CAMIN(inc)
                      endif
                  endif
                  rbfo(n) = rnum
  210         continue
c
c...Store text (PPRINT, PARTNO, INSERT & LETTER)
c
          else
              n     = 1
              m     = 0
  250         if (n .gt. j*2) go to 260
c
c......Get first 3 times I*2
c
              if (mod(n,4) .ne. 0) then
                  m    = m + 1
                  inum(m) = ibuf(n)
                  n    = n + 1
                  if (m .eq. 4) then
                      i    = i + 1
                      rbfo(i) = rnum
                      rnum = dumm
                      m    = 0
                  end if
              else
c
c......Skip the forth I*2 in word
c
                  n    = n + 1
              end if
              go to 250
  260         if (m .ne. 0) then
                  i = i + 1
                  rbfo(i) = rnum
              end if
              do 290 n=i+1,9
                  rbfo(n) = dumm
  290         continue
              i   = 9
          end if
  300     nwds   = i
          go to 2000
c
c...Circular record
c
      else if (inbuf(3) .eq. 3000) then
          call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          i     = j / 2
          do 550 n=6,i
              rbfo(n-5) = rbuf(n)
  550     continue
          nwds  = 7
c
c...Motion record
c
      else if (inbuf(3) .eq. 5000) then
          call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          i     = j / 2
          do 850 n=3,i
              rbfo(n-2) = rbuf(n)
  850     continue
          nwds  = i - 2
          if (inbuf(4) .eq. 4) then
              call godlta (nwds,rbfo,imlt,gts)
              inbuf(4) = 5
          end if
          j     = imlt * 3 + 3
          do 855 i=1,j,1
              gts(i) = rbfo(nwds-j+i)
  855     continue
c
c...Multax record
c
      else if (inbuf(3) .eq. 9000 .and. inbuf(4) .eq. 2) then
          call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          i     = j / 2
          do 950 n=1,i
              rbfo(n) = rbuf(n)
  950     continue
          nwds  = 0
          inbuf(4) = ibuf(4)
          imlt  = ibuf(4)
c
c...FINI record
c
      else if (inbuf(3) .eq. 14000) then
          ifini  = 1
          nwds   = 0
c
c...Unsupported record type
c...Store cldata as is
c
      else
          call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          nwds   = j / 2
          do 1050 n=1,nwds
              rbfo(n) = rbuf(n)
 1050     continue
      endif
 2000 ISN    = inbuf(1)
      inbuf(2) = ISN
      mxc    = nwds
      do 2100 i=1,(nwds+1)*2,1
          if (icpt .eq. 128) then
              icr = icr + 1
              call wrprm (LUNSC1,icr,jtmp,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              icpt  = 0
          end if
          icpt  = icpt + 1
          jtmp(icpt) = jbfo(i)
 2100 continue
c
      ipt    = ipt - 1
      if (ifini .eq. 0) go to 100
      if (icpt .ne. 0) then
          icr = icr + 1
          call wrprm (LUNSC1,icr,jtmp,cmsg,kerr)
      end if
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rdcatc (krec,klen,kcon,cmsg,kerr)
c
c   FUNCTION:  This routine reads a CATIA clfile phisical record with
c              its attributes.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c   OUTPUT: klen    I*4  D1  Length of the first logical record in I*2.
c
c           kcon    I*1  D1  Continuation mark: 0 - logical record is
c                            complete, 2 - this logical record is
c                            continued from the previous phisical record.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rdcatc (krec,klen,kcon,cmsg,kerr)
c
      integer*4 klen,krec,kcon,kerr
      character*(*) cmsg
c
      include 'menu.inc'
      include 'clnrd.inc'
c
c...Read in phisical cl record
c
      call rdcat (LUNSC2,krec,JCLBUF,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      LSTPTC = ICLBUF(1) / 2
      klen   = ICLBUF(3) / 2
      kcon   = ICLBUF(4) / 256
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gethdr (krec,kpnt,ibuf,klen,kcon,cmsg,kerr)
c
c   FUNCTION:  This routine parses a CATIA clfile 1000 type record and
c              the header of the following logical record.
c
c   INPUT:  krec    I*4  D1  Current clfile physical record number.
c
c           kpnt    I*4  D1  Starting pointer of the 1000 logical
c                            record in I*4 (from the start of phisical rec)
c
c   OUTPUT: ibuf    I*2  D4  Array to receive integer clfile data:  1 =
c                            ISN, 2 = CL record #, 3 = Record type, 4 =
c                            Record sub-type.
c
c           kpnt    I*4  D1  Starting pointer of the data in the logical
c                            record in I*4 (from the start of phisical rec)
c
c
c           klen    I*4  D1  Number of I*4 data words in logical record to
c                            process after the record header.
c
c           kcon    I*1  D1  Continuation mark: 0 - logical record is
c                            complete, 1 - this logical record is not
c                            complete and will be continued in the next
c                            phisical record, 2 - this logical record is
c                            continued from the previous phisical record.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine gethdr (krec,kpnt,ibuf,klen,kcon,cmsg,kerr)
c
      integer*4 krec,klen,kpnt,kcon,kerr
      integer*2 ibuf(4)
      character*(*) cmsg
c
      include 'clnrd.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001))
      integer*4 ISN
c
      integer*2 i1000(40)
      integer*4 i,ipt,ipt1,nwds2,nc4,ilen,lcon,nc40,j1000(20)
c
      equivalence (i1000,j1000)
c
c...Initialize routine
c
      nc4    = kpnt
      lcon   = 0
      i      = 0
c
c...Get I*2 pointer and logical record length
c
  100 ipt1   = nc4*2 - 1
      nc40   = nc4
      ipt    = ipt1
      if (ipt .lt. LSTPTC) then
          nwds2  = ICLBUF(ipt) / 2
          lcon   = ICLBUF(ipt+1) / 256
      end if
      ipt    = ipt + 2
      nc4    = nc4 + 1
c
c...See if next record is required
c
  200 if (ipt .gt. LSTPTC) then
          krec  = krec + 1
          call rdcatc (krec,ilen,kcon,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if (lcon .eq. 1 .and. kcon .ne. 2 .or.
     -        lcon .ne. 1 .and. kcon .eq. 2) go to 7800
          nwds2  = ilen
c
c...Skip "N/CDATA..." record
c
          if (krec .eq. 1) then
              nc4   = ilen / 2 + 2
          else
              nc4   = 2
          end if
          go to 100
      else
c
c...Store integers for 1000 record
c...and next logical record
c
          i    = i + 1
          j1000(i) = JCLBUF(nc4)
          ipt    = ipt + 2
          nc4    = nc4 + 1
          if (i .eq. 3) then
              if (j1000(2) .eq. 5000) go to 500
          else if (i .eq. 7) then
              go to 100
          else if (i .eq. 10) then
              go to 600
          end if
      end if
      go to 200
c
c...type 5000, subt = 6 does not have
c...1000 type description record.
c
  500 ibuf(1) = ISN
      ibuf(2) = j1000(1)
      ibuf(3) = j1000(2)
      ibuf(4) = j1000(3)
      go to 900
c
c...Get logical record description
c
  600 ibuf(1) = j1000(3)
      ibuf(2) = j1000(8)
      ibuf(3) = j1000(9)
      ibuf(4) = j1000(10)
c
c...Save pointer and record length
c
  900 if (nc4 .gt. kpnt) kcon = lcon
      kpnt   = nc4
      klen   = nc40 + nwds2 / 2 - nc4
      return
c
c...Error
c
 7800 cmsg   = '*FATAL* Clfile record has unknown structure.'
 8000 kerr    = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cl4sto (kpt,kls2,krec,kcon,klen,jbuf,knum,cmsg,kerr)
c
c   FUNCTION:  This routine extracts a CATIA clfile logical record data
c              from the phisical record.
c
c   INPUT:  kpt     I*4  D1  Current pointer of data (I*4 words) in the
c                            phisical record.
c
c           kls2    I*4  D1  the last I*2 word pointer in logical record.
c
c           krec    I*4  D1  Current clfile physical record number.
c
c           kcon    I*4  D1  Continuation mark: 0 - logical record is
c                            complete, 1 - logical record is not complet
c                            and will be continued.
c
c           klen    I*4  D1  Number of I*4 data words in logical record to
c                            process after the record header.
c
c   OUTPUT: kls2    I*4  D1  The last I*2 word pointer in logical record
c                            when it is a continuation of record.
c
c           krec    I*4  D1  Current clfile physical record number.
c
c           kcon    I*4  D1  Continuation mark: 0 - logical record is
c                            complete, 2 - this logical record is
c                            continued from the previous phisical record.
c
c           klen    I*4  D1  Number of I*4 data words in logical record to
c                            process when it is a continuation of record.
c
c           jbuf    I*4  Dn  Data array (I*4 words).
c
c           knum    I*4  D1  Number of words in jbuf.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cl4sto (kpt,kls2,krec,kcon,klen,jbuf,knum,cmsg,kerr)
c
      integer*4 kpt,knum,krec,kcon,klen,kls2,kerr,jbuf(*)
      character*(*) cmsg
c
      include 'clnrd.inc'
      include 'post.inc'
c
      integer*4 j,lcon,ipt,ilen
c
      j      = 0
c
c...Get I*2 pointer of data
c
      ipt    = kpt*2 - 1
c
c...Store I*4 words
c
  800 if (ipt .lt. kls2) then
          j     = j + 1
          jbuf(j) = JCLBUF(kpt)
          ipt   = ipt + 2
          kpt   = kpt + 1
      else
c
c...Read next record if end reached
c...and continuation mark is set
c
          if (kpt*2 .ge. LSTPTC .and. kcon .eq. 1) then
              krec   = krec   + 1
              call rdcatc (krec,ilen,lcon,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              kpt    = 3
              ipt    = 5
              kls2   = ilen + 2
              klen   = ilen * 2
              kcon   = lcon
          else
              go to 8000
          endif
      end if
      go to 800
c
 8000 knum   = j
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  godlta (kwds,gbuf)
c
c   FUNCTION:  This routine replaces GODLTA record by GOTO record.
c
c   INPUT:  kwds    I*4  D1  Number of words in GODLTA record.
c
c           gbuf    R*8  Dn  GODLTA record contens.
c
c   OUTPUT: kwds    I*4  D1  Number of words in GOTO record.
c
c           gbuf    R*8  Dn  GOTO record.
c
c***********************************************************************
c
      subroutine godlta (kwds,gbuf,kmul,gpts)
c
      integer*4 kwds,kmul
      real*8 gbuf(*),gpts(6)
c
      include 'clnrd.inc'
c
      integer*4 nwds,i

      real*8 delt(6),ddl
c
c...get GODLTA data
c
      nwds   = kwds
      do 110 i=1,nwds
          delt(i) = gbuf(i)
  110 continue
      if (nwds .lt. 4) then
          delt(4) = gpts(4)
          delt(5) = gpts(5)
          delt(6) = gpts(6)
      end if
c
c...Create GOTO using last point data
c......GODLTA/z along tool vector
c
      if (nwds .eq. 1) then
          ddl = delt(1) / dsqrt(gpts(4)**2+gpts(5)**2+gpts(6)**2)
          gbuf(1) = gpts(1) + gpts(4) * ddl
          gbuf(2) = gpts(2) + gpts(5) * ddl
          gbuf(3) = gpts(3) + gpts(6) * ddl
      else
c
c......GODLTA/dx,dy,dz(,i,j,k)
c
          do 210 i=1,3
              gbuf(i) = gpts(i) + delt(i)
  210     continue
      endif
      kwds   = 3
      if (kmul .eq. 1) then
          kwds   = 6
          do 220 i=4,kwds
              gbuf(i) = delt(i)
  220     continue
      endif
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  clpt52 (kwds,gtpt,gtpf,gtp1,gtp2,gtp3,gtp4)
c
c   FUNCTION:  This routine sets common arrays for expanded goto record
c              (type 5200 and 5210).
c
c   INPUT:  kwds    I*4  D1  Number of words in RCLDAT for this record
c
c   OUTPUT: gtpt    R*8  D6  GOTO point and tool vector.
c
c           gtpf    R*8  D3  Forward vector at the point.
c
c           gtp1-4  R*8  D3  Additional data (for future use) at the
c                            point.
c
c***********************************************************************
c
      subroutine clpt52 (kwds,gtpt,gtpf,gtp1,gtp2,gtp3,gtp4)
c
      include 'post.inc'
      integer*4 kwds
      real*8 gtpt(240),gtpf(60),gtp1(60),gtp2(60),gtp3(60),
     -       gtp4(60)
c
      equivalence (MXCL  ,KPOSMP(0005))
      equivalence (MULTAX,KPOSMP(0056))
      equivalence (MXFLAG,KPOSMP(4002))
      equivalence (MXTRAN,KPOSMP(4003)), (MOTEXP,KPOSMP(4211))
c
      integer*4 MXCL,MULTAX,MXFLAG,MXTRAN,MOTEXP
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
      equivalence (CLPTPP,POSMAP(4605))
c
      real*8 METCNV,REFMAT(12),TRAMAT(12),CLPTPP(240)
c
      integer*4 inc,jnc,i,ix1,ix4
c
      MOTEXP = 1
      inc    = 0
      jnc    = 1
      do 850 i=0,kwds-1,21
          ix1  = inc + 1
          ix4  = inc + 4
c
c...store GT points and tool vector data
c
          call vctmsc (RCLDAT(i+1),METCNV,gtpt(ix1))
          call copyn (RCLDAT(i+4),gtpt(ix4),3)
c
c...store FWD vector and extra stuff in work arrays
c
          call copyn (RCLDAT(i+7),gtpf(jnc),3)
          call copyn (RCLDAT(i+10),gtp1(jnc),3)
          call copyn (RCLDAT(i+13),gtp2(jnc),3)
          call copyn (RCLDAT(i+16),gtp3(jnc),3)
          call copyn (RCLDAT(i+19),gtp4(jnc),3)
c
c...translate all data if any translation is active
c
          if (MXTRAN .eq. 1) then
              call matpta (gtpf(jnc),gtpf(jnc),TRAMAT,3)
              call matpta (gtp1(jnc),gtp1(jnc),TRAMAT,3)
              call matpta (gtp2(jnc),gtp2(jnc),TRAMAT,3)
              call matpta (gtp3(jnc),gtp3(jnc),TRAMAT,3)
              call matpta (gtp4(jnc),gtp4(jnc),TRAMAT,3)
          end if
          if (MXFLAG .eq. 1) then
              call matpta (gtpf(jnc),gtpf(jnc),REFMAT,3)
              call matpta (gtp1(jnc),gtp1(jnc),REFMAT,3)
              call matpta (gtp2(jnc),gtp2(jnc),REFMAT,3)
              call matpta (gtp3(jnc),gtp3(jnc),REFMAT,3)
              call matpta (gtp4(jnc),gtp4(jnc),REFMAT,3)
          end if
c
          call copyn (gtpt(ix1),CLPTPP(ix1),6)
          call preadj (gtpt(ix1),gtpt(ix1),gtpt(ix4),gtpt(ix4))
          if (MULTAX .eq. 1) then
              if (MXTRAN .eq. 1)
     -            call matpta (gtpt(ix4),gtpt(ix4),TRAMAT,3)
              if (MXFLAG .eq. 1)
     -            call matpta (gtpt(ix4),gtpt(ix4),REFMAT,3)
          end if
          inc    = inc    + 6
          jnc    = jnc    + 3
  850 continue
      MXCL   = inc    / 6
c
      return
      end
