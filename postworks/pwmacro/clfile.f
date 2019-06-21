c
c***********************************************************************
c
c   FILE NAME:  clfile
c   CONTAINS:
c               clread  clwrit  cldwrt  clrew   clmark  clumrk  nclrd
c               nclwr   clpt52  srcyc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        clfile.f , 25.4
c     DATE AND TIME OF LAST  MODIFICATION
c        08/11/16 , 10:28:28
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  clread (krec,kpt,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine reads a neutral format clfile record and
c              sets up global arrays depending on the type of record.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c           kpt     I*4  D1  Pointer to next logical clfile record
c                            within physical record.
c
c           kfl     I*4  D1  1 = Standard clfile read, 2 = CLREAD command
c                                style read.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine clread (krec,kpt,kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLREC,KPOSMP(0002))
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (MULTAX,KPOSMP(0056)), (NPT   ,KPOSMP(0059))
      equivalence (NCUT  ,KPOSMP(0062)), (NCUTDS,KPOSMP(0063))
      equivalence (ICUTYP,KPOSMP(0064)), (NSHANK,KPOSMP(0067))
      equivalence (ICUCHG,KPOSMP(0068)), (NCLD  ,KPOSMP(0076))
      equivalence (SIMACT,KPOSMP(0174))
      equivalence (MACHTP,KPOSMP(1201)), (MROTTV,KPOSMP(1334))
      equivalence (IRAP  ,KPOSMP(3199)), (MXFLAG,KPOSMP(4002))
      equivalence (MXTRAN,KPOSMP(4003)), (MOTEXP,KPOSMP(4211))
c
      integer*4 ISN,ICLREC,ITYPE,ISUBT,MXCL,IPSTWD(50),MULTAX,
     -          MROTTV,NPT,MXFLAG,MXTRAN,NCUT,NCUTDS,ICUTYP(3),
     -          MOTEXP,NSHANK,IRAP,ICUCHG,NCLD,SIMACT,
     3          MACHTP
c
      equivalence (METCNV,POSMAP(0004)), (CLPOS ,POSMAP(0222))
      equivalence (PSTWD ,POSMAP(0441)), (TLATOL,POSMAP(0057))
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (CLSAV ,POSMAP(0201)), (CUTTER,POSMAP(0744))
      equivalence (CUDISP,POSMAP(0759)), (CUTOFS,POSMAP(0766))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
      equivalence (CLPTPP,POSMAP(4605)), (CIRBPP,POSMAP(4845))
c
      real*8 METCNV,PSTWD(50),CLPT(240),CIRBUF(7),CLSAV(21),
     1       CUTTER(7),VECSAV(3),REFMAT(12),TRAMAT(12),CUDISP(7),
     2       CUTOFS(4,3),CLPTPP(240),CIRBPP(7),CLPOS(21),TLATOL
c
      integer*2 ICLDAT(200)
      integer*4 JCLDAT(200)
      character*8 lclt,lclnm,lclrv
      character*11 lcld
      character*512 LCLDAT
      character*80 lclf
c
      equivalence (RCLDAT,ICLDAT,JCLDAT,LCLDAT)
      equivalence (RCLDAT(1),lclf), (RCLDAT(11),lcld)
      equivalence (RCLDAT(13),lclt), (RCLDAT(14),lclnm)
      equivalence (RCLDAT(15),lclrv)
c
      equivalence (LPSTWD,CPOSMP(0217)), (CUTSYM,CPOSMP(3009))
c
      character*80 CUTSYM(3)
      character*512 LPSTWD
c
      integer*4 krec,kpt,kfl,kerr,is1,is4,mxm
c
      character*(*) cmsg
c
      integer*4 i,j,inc,inbuf(4),nwds,ix1,ix4,ierr,ist,mxc,nc
c
      real*8 vc1(3),vc2(3),rver
c
      data vc1 /0.,0.,1./
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
      if (ICLF .eq. 1) then
          call nclrd (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
      else
          call srcrd (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
c
c...CLREAD style read
c
      if (kfl .eq. 2) then
          CLDATA(1) = inbuf(1)
          CLDATA(2) = inbuf(2)
          CLDATA(3) = inbuf(3)
          CLDATA(4) = inbuf(4)
          CLTEXT = ' '
          j      = CLDATA(4)
          call getvwd (j,CLTEXT(25:48),nc,2,PSTWRD,PSTWVL,NPSTWD)
          do 100 i=1,nwds,1
              CLDATA(i+4) = RCLDAT(i)
  100     continue
          NCLD   = nwds   + 4
c
c.....Post word
c.....Store minor word values
c
          if (inbuf(3) .eq. 2000) then
c
c......LETTER, PARTNO, INSERT and PPRINT
c
              if ((inbuf(4) .ge. 1043 .and. inbuf(4) .le. 1046) .or.
     1            inbuf(4) .eq. 1199) then
                  CLTEXT(49:114) = LCLDAT(1:nwds*8)
c
c......Major/Minor word
c
              else
                  mxm    = 0
                  if (inbuf(4) .eq. 1015) then
                      if (JCLDAT(1) .eq. -1 .and. JCLDAT(2) .gt. 8 .and.
     1                        JCLDAT(2) .le. 40) then
                          mxm    = (JCLDAT(2)+7) / 8 + 1
                      else
                          if (ICLDAT(4) .eq. 0) LCLDAT(7:8) = ' '
                          mxm    = 1
                      endif
                  endif
                  do 130 i=1,nwds,1
                      inc    = (i+2) * 24 - 23
                      if (ICLDAT(i*4-is4) .eq. 0 .and.
     1                    ICLDAT(i*4-is1) .ne. 0 .and. i .gt. mxm) then
                          j      = ICLDAT(i*4-is1)
                          call getvwd (j,CLTEXT(inc:inc+8),nc,2,PSTWRD,
     1                                 PSTWVL,NPSTWD)
                          CLDATA(i+4) = 0.
                      else if (i .le. mxm) then
                          CLTEXT(inc:inc+23) = LCLDAT(inc:inc+23)
                          CLDATA(i+4) = 0.
                      else
                          CLTEXT(inc:inc+23) = ' '
                          CLDATA(i+4) = RCLDAT(i)
                      endif
  130             continue
              endif
c
c.....MULTAX
c.....Store ON/OFF state
c
          else if (inbuf(3) .eq. 9000) then
              if (inbuf(4) .eq. 1) then
                  CLDATA(4) = 71
              else
                  CLDATA(4) = 72
              endif
          endif
          go to 8000
      endif
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
      if ((ICLF .eq. 2).or.(ICLF.eq.4)) then
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
          if ((ISUBT .ge. 1043 .and. ISUBT .le. 1046) .or.
     1        ISUBT .eq. 1199) then
              LPSTWD = LCLDAT(1:nwds*8)
              if (ICLF .ne. 2 .and. ICLF .ne. 4) LPSTWD(67:) = ' '
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
                      if (ICLDAT(4) .eq. 0) LCLDAT(7:8) = ' '
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
                      if (MROTTV .eq. 1) then
                          CLPT(inc+j) = RCLDAT(i+j)
                          if (j .eq. 4 .and.
     1                        dabs(RCLDAT(i+j)) .lt. TLATOL .and.
     1                        dabs(RCLDAT(i+j+1)) .lt. TLATOL) then
                              CLPT(inc+j) = 0.
                              RCLDAT(i+j+1) = 0.
                          endif
                      else
                          CLPT(inc+j) = VECSAV(j-3)
                      end if
                  else
                      CLPT(inc+j) = RCLDAT(i+j) * METCNV
                  endif
  700         continue
              call copyn (CLPT(inc+1),CLPTPP(inc+1),NPT)
              ix1  = inc + 1
              ix4  = inc + 4
              if (NPT .eq. 6) then
                  call preadj (CLPT(inc+1),CLPT(inc+1),CLPT(ix4),
     1                         CLPT(ix4))
              else
                  call preadj (CLPT(inc+1),CLPT(inc+1),vc1,vc2)
              endif
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
          ITYPE  = 5000
          NPT    = 6
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
          ICUTYP(1) = 0
          ICUTYP(2) = 0
          ICUTYP(3) = 0
          NSHANK = 0
          if (MACHTP .ne. 5 .or. SIMACT .ne. 1) ICUCHG = 1
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
              ICUTYP(1) = 1
              ICUTYP(2) = 0
              ICUTYP(3) = 0
              NSHANK = 0
              if (MACHTP .ne. 5 .or. SIMACT .ne. 1) ICUCHG = 1
c
c......Cutter symbol (Pre 9.6)
c
          else if (ISUBT .eq. 2) then
              CUTSYM(1) = LCLDAT(1:20)
              ICUTYP(1) = ICLDAT(16)
              do 1200 i=1,4,1
                  CUTOFS(i,1) = RCLDAT(i+4)
 1200         continue
              if (ICUTYP(1) .le. 2) then
                  NSHANK = 0
                  ICUTYP(2) = 0
                  ICUTYP(3) = 0
                  CUTSYM(2) = ' '
                  CUTSYM(3) = ' '
              endif
              if (MACHTP .ne. 5 .or. SIMACT .ne. 1) ICUCHG = 1
c
c......Cutter shank (Pre 9.6)
c
          else if (ISUBT .eq. 4) then
              NSHANK = MXCL
              do 1250 i=1,NSHANK,1
                  CUTOFS(i,2) = RCLDAT(i)
 1250         continue
              if (MACHTP .ne. 5 .or. SIMACT .ne. 1) ICUCHG = 1
c
c......CUTTER/symbol (9.6+)
c
          else if (ISUBT .eq. 5 .or. ISUBT .eq. 7) then
              if (ISUBT .eq. 5) then
                  mxc = 20
                  ist = 1
              else
                  mxc = ICLDAT(1)
                  ist = 25
              endif
              CUTSYM(1) = LCLDAT(ist:ist+mxc-1)
              ICUTYP(1) = ICLDAT(2)
              if (ISUBT .eq. 5) ICUTYP(1) = ICLDAT(11)
              ist = 1
              if (ISUBT .eq. 5) ist = 3
              do 1300 i=1,2,1
                  CUTOFS(i,1) = RCLDAT(ist+i)
 1300         continue
              CUTOFS(3,1) = 0.
              CUTOFS(4,1) = 0.
              ICUTYP(2) = 0
              ICUTYP(3) = 0
              CUTSYM(2) = ' '
              CUTSYM(3) = ' '
              if (MACHTP .ne. 5 .or. SIMACT .ne. 1) ICUCHG = 1
c
c......CUTTER/SHANK-HOLDER (9.6+)
c
          else if (ISUBT .eq. 6 .or. ISUBT .eq. 8) then
              ist = 2
              if (ISUBT .eq. 6) ist = 11
              inc    = 2
              if (ICLDAT(ist+1) .eq. 2) inc = 3
              ICUTYP(inc) = ICLDAT(ist)
              if (inc .eq. 2) NSHANK = ICLDAT(ist+1)
              if (ISUBT .eq. 6) then
                  mxc = 20
                  ist = 1
              else
                  mxc = ICLDAT(1)
                  ist = 41
              endif
              CUTSYM(inc) = LCLDAT(ist:ist+mxc-1)
              ist = 1
              if (ISUBT .eq. 6) ist = 3
              do 1400 i=1,4,1
                  CUTOFS(i,inc) = RCLDAT(i+ist)
 1400         continue
              if (MACHTP .ne. 5 .or. SIMACT .ne. 1) ICUCHG = 1
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
          call ctor (lclrv,rver,ierr)
          if (ierr .eq. 0) NCLVER = rver

c
c...APT Source error
c
      else if (ITYPE .eq. 8000) then
          LPSTWD = LCLDAT(1:nwds*8)
c
c...Multax
c
      else if (ITYPE .eq. 9000) then
          MULTAX = ISUBT
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
      character*512 LCLDAT
      equivalence (RCLDAT,ICLDAT,JCLDAT,LCLDAT)
c
      equivalence (LPSTWD,CPOSMP(0217))
c
      character*512 LPSTWD
c
      integer*4 krec,kpt,kerr
c
      character*(*) cmsg
c
      integer*4 inbuf(4),nwds,i,inc,nopt,imult,j,is1,nc,strlen1
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
              nc    = strlen1(LPSTWD)
              if (nc .lt. 512) LPSTWD(nc+1:) = ' '
cc              if (nc .lt. 66) nc = 66
              nc = 66
              nwds  = (nc+7) / 8 * 8
              if (nwds .gt. nc) LPSTWD(nc+1:nwds) = ' '
              LCLDAT(1:nwds) = LPSTWD(1:nwds)
              nwds = nwds / 8
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
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (NCLD  ,KPOSMP(0076)), (ICLOUT,KPOSMP(0084))
c
      integer*4 NCLD,ICLOUT
c
      integer*4 krec,kpt,kerr
c
      character*(*) cmsg
c
      integer*4 inbuf(4),nwds,i,ist,ien,is1,iwrd
c
      integer*2 icld(976)
      character*1952 lcld
      equivalence (CLDATA,icld,lcld)
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
      if (PGMNAM .ne. 'PostMacro') go to 8000
      ICLOUT  = ICLOUT + 1
      inbuf(1) = CLDATA(1)
      inbuf(2) = ICLOUT
      inbuf(3) = CLDATA(3)
      inbuf(4) = CLDATA(4)
      if (inbuf(3) .eq. 2000 .and. CLTEXT(25:48) .ne. ' ') then
          call getvnm (CLTEXT(25:48),iwrd,PSTWRD,PSTWVL,NPSTWD)
          if (iwrd .ne. 0) inbuf(4) = iwrd
      endif
      nwds     = NCLD
c
c....Store CLTEXT into CLDATA array
c
      if (inbuf(3) .eq. 2000) then
          do 500 i=5,nwds,1
              ist     = (i-2)*24 - 23
              ien     = ist + 23
              if (CLTEXT(ist:ien) .ne. ' ') then
                  call getvnm (CLTEXT(ist:ien),iwrd,PSTWRD,PSTWVL,
     1                NPSTWD)
                  if (iwrd .ne. 0 .and. (inbuf(4) .lt. 1043 .or.
     1                inbuf(4) .gt. 1046) .and. inbuf(4) .ne. 1199)
     2                    then
                      icld(i*4-3) = 0
                      icld(i*4-2) = 0
                      icld(i*4-1) = 0
                      icld(i*4) = 0
                      icld(i*4-is1) = iwrd
                  else
                      lcld(ist:ien) = CLTEXT(ist:ien)
                  endif
              endif
  500     continue
c
c...Fill text commands
c
          if ((inbuf(4) .ge. 1043 .and. inbuf(4) .le. 1046) .or.
     1        inbuf(4) .eq. 1199) then
              ien    = nwds*8
              if (ien .lt. 104) lcld(ien+1:104) = ' '
              nwds   = 13
          endif
      endif
c
c...Write clfile record &
c...Listing record
c
      nwds   = nwds   - 4
      call lstdat (inbuf,CLDATA(5),nwds,cmsg,kerr)
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
         call inisto
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
      equivalence (ICLMSV,KPOSMP(0151)), (PICLMSV,KPOSMP(0328))
      integer*4 kst
c
      integer*4 ISN,ITYPE,ISUBT,MXCL,IFIREC,IFIPT,LSTPC,ICLOUT,
     1          ICLMSV(20), PICLMSV(20), NPT,ICLREC
c
      equivalence (LCLMSV,CPOSMP(0001)), (LMACRO,CPOSMP(0861))
c
      character*24 LCLMSV(2),LMACRO
c
      integer*4 i,ierr
c
      character*80 msg
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
         if (ICLMSV(10) .ne. -1) then
             LSTPC  = ICLMSV(9)
             IPC    = ICLMSV(10)
             IMACPT = ICLMSV(11)
             do 100 i=1,5,1
                IMACHD(i,IMACPT) = ICLMSV(i+11)
  100        continue
             LMACRO = LCLMSV(1)
         endif
         ICLMSV(10) = -1
         call rststo (msg,ierr)
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
         if (PICLMSV(10) .ne. -1) then
             LSTPC  = PICLMSV(9)
             IPC    = PICLMSV(10)
             IMACPT = PICLMSV(11)
             do 400 i=1,5,1
                IMACHD(i,IMACPT) = PICLMSV(i+11)
  400        continue
             LMACRO = LCLMSV(1)
         endif
         PICLMSV(10) = -1
      endif
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
c              *** THIS ROUTINE IS NO LONGER USED. ***
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
      integer*4 i,ipt,nwds,inc,jindex,inum,is1,is4
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
c
c...Read cl record if necessary
c
      kpt    = kpt    + 1
      if (kpt .ge. 36 .or. krec .ne. LSTIRC) then
          if (kpt .ge. 36) then
              krec   = krec   + 1
              kpt    = 1
          endif
          call rdncl (LUNSC1,krec,JCLBUF,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Store integer values
c
      ipt    = (kpt*4) - 3
      kbuf(1) = ICLBUF(ipt+3)
      kbuf(2) = ICLREC
      kbuf(3) = ICLBUF(ipt+1)
      kbuf(4) = ICLBUF(ipt+2)
      nwds    = ICLBUF(ipt) - 1
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
          if ((kbuf(4) .lt. 1043 .or. kbuf(4) .gt. 1046) .and.
     1        kbuf(4) .ne. 1199) then
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
          kbuf(4)   = 1 - kbuf(4)
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
c...Initialze routine
c
      kerr   = 0
c
c...Store ISN record
c
      if (kbuf(1) .gt. 32767) then
          kpt    = kpt    + 1
          if (kpt .ge. 36) then
              call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              krec   = krec   + 1
              kpt    = 1
          endif
          idata(1) = 1
          idata(2) = 1001
          jdata(2) = kbuf(1)
          kpt    = kpt    + 1
          gbuf(kpt) = rdata
      endif
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
          if ((kbuf(4) .lt. 1043 .or. kbuf(4) .gt. 1046) .and.
     1        kbuf(4) .ne. 1199) then
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
          ICOBUF(ipt)   = 1
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
      equivalence (MROTTV,KPOSMP(1334)), (MXFLAG,KPOSMP(4002))
      equivalence (MXTRAN,KPOSMP(4003)), (MOTEXP,KPOSMP(4211))
c
      integer*4 MXCL,MULTAX,MROTTV,MXFLAG,MXTRAN,MOTEXP
c
      equivalence (METCNV,POSMAP(0004)), (VECSAV,POSMAP(1372))
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
      equivalence (CLPTPP,POSMAP(4605))
c
      real*8 METCNV,VECSAV(3),REFMAT(12),TRAMAT(12),CLPTPP(240)
c
      integer*4 inc,jnc,i,ix1,ix4
c
      real*8 vc1(3),vc2(3)
c
      data vc1 /0.,0.,1./
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
          if (MULTAX .eq. 1) then
              if (MROTTV .eq. 1) then
                  call copyn (RCLDAT(i+4),gtpt(ix4),3)
              else
                  call copyn (VECSAV,gtpt(ix4),3)
              end if
          else
              call copyn (VECSAV,gtpt(ix4),3)
          end if
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
          if (MULTAX .eq. 1) then
              call preadj (gtpt(ix1),gtpt(ix1),gtpt(ix4),gtpt(ix4))
              if (MXTRAN .eq. 1)
     1            call matpta (gtpt(ix4),gtpt(ix4),TRAMAT,3)
              if (MXFLAG .eq. 1)
     1            call matpta (gtpt(ix4),gtpt(ix4),REFMAT,3)
          else
              call preadj (gtpt(ix1),gtpt(ix1),vc1,vc2)
          endif
          inc    = inc    + 6
          jnc    = jnc    + 3
  850 continue
      MXCL   = inc    / 6
c
      return
      end
