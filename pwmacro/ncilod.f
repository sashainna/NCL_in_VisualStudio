c
c***********************************************************************
c
c   FILE NAME:  ncilod
c   CONTAINS:
c               ncilod  nciprs  ncihed  ncippr  ncitls  ncitln  ncitlc
c               ncidly  ncicol  ncispn  ncicut  ncixfm  ncimsc
c
c     COPYRIGHT 2009 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        ncilod.f , 25.6
c     DATE AND TIME OF LAST  MODIFICATION
c        05/20/16 , 16:16:35
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ncilod (cfnam,ktest,cmsg,kerr)
c
c   FUNCTION:  This routine reads a MasterCam NCI file, converts it to a
c              neutral format and stores it in a scratch file.
c
c   INPUT:  cfnam   C*n  D1  Input Apt Source file name.
c
c           ktest   I*4  D1  1 = Test to see if this is a valid clfile.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncilod (cfnam,ktest,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
      include 'compile.inc'
      include 'ncicom.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END
c
      integer*4 kerr,ktest
c
      character*(*) cfnam,cmsg
c
      equivalence (NCUT  ,KPOSMP(0064)), (ICYCSW,KPOSMP(0271))
      equivalence (MCHPLN,KPOSMP(1261)), (ICUTDO,KPOSMP(3301))
c
      integer*4 MCHPLN,ICUTDO(15),ICYCSW(5),NCUT
c
      equivalence (VECSAV,POSMAP(1372))
      equivalence (CTOFRG,POSMAP(2401)), (TL    ,POSMAP(3601))
c
      real*8 CTOFRG,TL(120),VECSAV(3)
c
      integer*4 i,nc,ifl,irecl,ityp,ifini,irec,mpos,mpt,mcont,nprs,
     1          itprm(5)
C WNT-START
      integer*4 flag,strlen1
C WNT-END
c
      real*8 spt(6),trmat(12),rtprm
c
      character*20 att(4)
      character*132 msg,msg1
      character*256 ldat
c
c...Initialize routine
c
C SUN-SGI-IBM-HPX-START
C      IS1    = 4
C      IS4    = 1
C SUN-SGI-IBM-HPX-END
C WNT-START
       IS1    = 1
       IS4    = 4
C WNT-END
c
c...Open Apt Source file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 256
      call opnfil (LUNSC2,cfnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call getfnm (LUNSC2,LCMPFI,NCCMPF,MAX_PATH)
c
c...Initialize routine
c
      INBUF(2) = 0
      irec   = 0
      ifl    = 0
      ifini  = 0
      mpos   = 0
      mpt    = 0
      mcont  = 5
      nprs   = 0
      spt(1) = 0.
      spt(2) = 0.
      spt(3) = 0.
      spt(4) = 0.
      spt(5) = 0.
      spt(6) = 1.
      do 50 i=1,12,1
          trmat(i) = 0.
   50 continue
      trmat(1) = 1.
      trmat(6) = 1.
      trmat(11) = 1.
c
      MAXTOK = 100
      CTOFRG = 0
      ICUTDO(1) = 0
      ICPT   = 0
      ICR    = 0
      ICYCSW(1) = 0
      MCHPLN = 3
      MULT   = 0
      NCUT   = 2
      TL(1)  = 0.
      TLNAME(1) = ' '
      VECSAV(1) = 0.
      VECSAV(2) = 0.
      VECSAV(3) = 1.
c
c...Store default MULTAX mode
c...if this is an INCLUDed clfile
c
      if (INCLNO .gt. 0 .and. ktest .eq. 0) then
          call wrmult (MULT,0,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Read source file record
c
  100 kerr   = 0
      nprs   = nprs   + 1
      if (ktest .eq. 1 .and. nprs .gt. 2) go to 8000
      call nciprs (irec,ityp,ldat,nc,ifl,cmsg,kerr)
      if (kerr .eq. 2) go to 8000
      if (kerr .ne. 0) go to 9000
      INBUF(2) = irec
      if (ktest .eq. 1) go to 100
c
c...Output buffered motion record
c...if current record is not motion
c
      if (ityp .ne. 0 .and. ityp .ne. 1 .and. ityp .ne. 11) then
          call ncimot (ityp,-1,mpt,mcont,spt,cmsg,kerr)
          if (kerr .lt. 0) go to 8000
          if (kerr .ne. 0) go to 9000
      endif
c
c...Header record
c
      if (ityp .eq. 1050) then
          call ncihed (cmsg,kerr)
c
c...PPRINT record
c
      else if (ityp .ge. 1005 .and. ityp .le. 1008) then
          call ncippr (ityp,cmsg,kerr)
c
c...Motion record
c
      else if (ityp .eq. 0 .or. ityp .eq. 1 .or. ityp .eq. 11) then
          if (ICYCSW(1) .ne. 0) then
              call ncicyf (cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          call ncimot (ityp,mpos,mpt,mcont,spt,cmsg,kerr)
c
c...Circular record
c
      else if (ityp .eq. 2 .or. ityp .eq. 3) then
          if (ICYCSW(1) .ne. 0) then
              call ncicyf (cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          call ncicir (ityp,spt,cmsg,kerr)
          if (kerr .ne. 0) go to 9000
c
c...Delay/Spindle record
c
      else if (ityp .eq. 4) then
          call ncidly (cmsg,kerr)
c
c...Cycle off record
c
      else if (ityp .eq. 80) then
          call ncicyf (cmsg,kerr)
c
c...Cycle record
c
      else if (ityp .eq. 81) then
          call ncicyc (spt,cmsg,kerr)
c
c...Cycle repeat record
c
      else if (ityp .eq. 100) then
          call ncicym (spt,cmsg,kerr)
c
c...Lathe threading parameters One
c
      else if (ityp .eq. 200) then
          call ncithp (itprm,rtprm)
c
c...Lathe threading parameters Two
c
      else if (ityp .eq. 201) then
          call ncithr (spt,itprm,rtprm,cmsg,kerr)
c
c...Tool change record
c
      else if (ityp .ge. 1000 .and. ityp .le. 1002) then
          call ncitlc (ityp,cmsg,kerr)
c
c...End of file
c
      else if (ityp .eq. 1003) then
          go to 8000
c
c...Cancel cutcom
c
      else if (ityp .eq. 1004) then
          call ncicut (40,cmsg,kerr)
c
c...Cutter parameters
c
      else if (ityp .eq. 1013) then
          call ncictr (cmsg,kerr)
c
c...Working coordinate system
c
      else if (ityp .eq. 1014 .or. ityp .eq. 1027) then
          call ncixfm (ityp,trmat,cmsg,kerr)
c
c...Miscellaneous parameters
c
      else if (ityp .eq. 1016) then
          call ncimsc (cmsg,kerr)
c
c...Tool description
c
      else if (ityp .eq. 20001) then
          call ncitls
c
c...Tool parameters
c
      else if (ityp .eq. 20007) then
          call ncitln
c
c...Unsupported record
c
      else
          if (ityp .le. 0) go to 8000
          go to 100
      endif
c
c...Get next record
c
      if (kerr .lt. 0) go to 8000
      if (kerr .ne. 0) go to 9000
      go to 100
c
c...End-of-file
c...Make sure FINI card is output
c
 8000 if (ifini .eq. 0 .and. kerr .eq. 0 .and. ktest .eq. 0) then
         ifini  = 1
         INBUF(3) = 14000
         INBUF(4) = 1
         MXC    = 0
         call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
         if (ICPT .ne. 0) then
             ICR    = ICR    + 1
             call wrprm (LUNSC1,ICR,JCLBUF,cmsg,kerr)
         endif
      endif
c
c......Close input file
c
      call clsfil (LUNSC2)
      return
c
c...Invalid Apt Source record
c
 9000 if (kerr .lt. 0) go to 8000
      i      = 0
      kerr   = 2
      call errtxt ('INVAPTSC',msg)
 9010 if (ktest .eq. 1) go to 8000
C SUN-SGI-IBM-HPX-START
C     call trmmsg (' ')
C      if (i .eq. 1) call trmmsg (msg1)
C      if (kerr .gt. 1 .or. kerr .eq. 0) call trmmsg (msg)
C      call trmmsg (ldat)
C SUN-SGI-IBM-HPX-END
C WNT-START
      flag = 1
      if (i .eq. 1) then
          nc = strlen1(msg1)
          call add1dispmsg (msg1, nc, flag)
      endif
      if (kerr .gt. 1 .or. kerr .eq. 0) then
          nc = strlen1(msg)
          call add1dispmsg (msg, nc, flag)
      endif
      nc = strlen1(ldat)
      call add1dispmsg (ldat, nc, flag)
C WNT-END
      kerr   = 0
      go to 100
      end
c
c***********************************************************************
c
c   SUBROUTINE:  nciprs (krec,ktyp,cdat,knc,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from an MasterCam NCI file, and
c              performs the initial parsing.
c
c   INPUT:  kfl     I*4  D1  1 = Do not read record, only parse it.
c                            'cdat' and 'knc' will be furnished by the
c                            calling routine.
c
c   OUTPUT: krec    I*4  D1  Record number which was read.
c
c           ktyp    I*4  D1  MasterCam record type processed.
c
c           cdat    C*n  D1  Data from record.
c
c           knc     I*4  D1  Number of chars in 'cdat'.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine nciprs (krec,ktyp,cdat,knc,kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'compile.inc'
      include 'ncicom.inc'
c
      integer*4 krec,kerr,kfl,knc,ktyp
c
      character*(*) cdat,cmsg
c
      integer*4 strlen1,ist,inc,i,ierr
c
      character*80 lwrd(50)
c
c...Initialize routine
c
      inc    = 0
c
c...Read from source file
c
  100 ITXTPT = 1
      if (kfl .eq. 0) then
          call rdtxt (LUNSC2,cdat,cmsg,kerr)
          if (kerr .eq. 1) then
              kerr   = 2
              go to 8000
          endif
          if (kerr .ne. 0) go to 8000
          knc    = strlen1(cdat)
          krec   = krec   + 1
      endif
c
c...Get record type
c
      if (inc .eq. 0) then
          call ctoi (cdat(1:knc),ktyp,kerr)
          if (kerr .ne. 0) go to 8000
          inc    = 1
          go to 100
      endif
c
c...Parse text record
c
      if ((ktyp .ge. 1005 .and. ktyp .le. 1008) .or.
     1    ktyp .eq. 20001 .or.knc .eq. 0) then
          ICTYP(1) = 4
          RCSUB(1) = 1
          ICNC(1) = knc
          if (knc .eq. 0) then
              ICNC(1) = 1
              LCTXT(1:1) = ' '
          else
              LCTXT(1:knc) = cdat(1:knc)
          endif
          NTOK   = 1
c
c...Parse all other records
c
      else
          ist    = 0
          if (ktyp .eq. 1050) ist = 9
          call srcbwd (cdat,knc,lwrd,NTOK,50,ist,0)
c
c......Loop through parameters
c......and get types
c
          do 500 i=1,NTOK,1
              if (i .ne. ist) call ctor (lwrd(i),RCSUB(i),ierr)
              if (ierr .eq. 0 .and. i .ne. ist) then
                  ICTYP(i) = 3
              else
                  ICTYP(i) = 4
                  RCSUB(i) = ITXTPT
                  ICNC(i) = strlen1(lwrd(i))
                  LCTXT(ITXTPT:ITXTPT+ICNC(i)-1) = lwrd(i)
                  ITXTPT = ITXTPT + ICNC(i)
              endif
  500     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncihed (cmsg,kerr)
c
c   FUNCTION:  This routine stores the header record in the clfile.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncihed (cmsg,kerr)
c
      include 'ncicom.inc'
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ist,ien,it1,it2,it3
c
      character*8 lct,lclrv,lclnm
      character*11 lcd
      character*80 lclf
c
      equivalence (ROBUF(1),lclf), (ROBUF(11),lcd), (ROBUF(13),lct)
      equivalence (ROBUF(14),lclnm), (ROBUF(15),lclrv)
c
c...Setup record
c
      INBUF(3) = 7400
      INBUF(4) = 0
      MXC    = 16
      ist    = RCSUB(9)
      ien    = ist    + ICNC(9) - 1
      lclf = LCTXT(ist:ien)
c
c...Set up date and time
c
      it1    = RCSUB(3)
      it2    = RCSUB(4)
      it3    = RCSUB(5)
      call fmtdat (it2,it1,it3,lcd)
      it1    = RCSUB(6)
      it2    = RCSUB(7)
      it3    = RCSUB(8)
      call fmttim (it1,it2,it3,lct)
c
c...Setup program and version number
c
      lclnm = "M-CAM"
      it1    = RCSUB(1)
      it2    = RCSUB(2)
      write (lclrv,10) it1,it2
   10 format (i2.2,'.',i3.3)
c
c...Write clfile record
c
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncippr (ktype,cmsg,kerr)
c
c   FUNCTION:  This routine stores PPRINT records in the clfile.
c
c   INPUT:  ktype   I*4  D1  Type of NCI record being parsed.  1006
c                            will output an INSERT record, all other
c                            types will output a PPRINT record.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine ncippr (ktype,cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 ktype,kerr
c
      character*(*) cmsg
c
      integer*4 ist,ien,nci,j,ival(2)
c
      real*8 rval,rtxt(5)
c
      character*40 ltxt
c
      equivalence (rval,ival), (rtxt,ltxt)
c
c...Setup record
c
      INBUF(3) = 2000
      INBUF(4) = 1044
      if (ktype .eq. 1006) INBUF(4) = 1046
      MXC    = 9
      ist    = RCSUB(1)
      ien    = ICNC(1)
      TOBUF  = LCTXT(ist:ien)
c
c...Added to prevent modifying the RCSUB array with input
c   record type 1001
c
      if (ktype .eq. 99999) goto 400
c
c...Check for PPRINT MACHIN
c
      nci    = ien    - ist    + 1
      call srcprs (irec,TOBUF,nci,1,cmsg,kerr)
      if (ICTYP(1) .eq. 1 .and. RCSUB(1) .eq. 1015) then
          if (ICTYP(3) .eq. 4 .and. (ICNC(3)-RCSUB(3)) .lt. 40) then
              INBUF(3) = 2000
              INBUF(4) = 1015
              ist    = RCSUB(3)
              ien    = ICNC(3)
              MXC    = 1
              call touppr (LCTXT(ist:ien),ltxt)
              if (ien-ist+1 .le. 8) then
                  ROBUF(MXC) = rtxt(1)
              else
                  ival(1) = -1
                  ival(2) = ien - ist + 1
                  ROBUF(MXC) = rval
                  do 200 i=1,(ival(2)+7)/8,1
                      MXC    = MXC    + 1
                      ROBUF(MXC) = rtxt(i)
  200             continue
              endif
c
              do 300 i=5,NTOK,2
                  MXC    = MXC    + 1
                  if (ICTYP(i) .eq. 1) then
                      IOBUF((MXC-1)*4+is4) = 0
                      IOBUF((MXC-1)*4+is1) = RCSUB(i)
                  else
                      ROBUF(MXC) = RCSUB(i)
                  endif
  300         continue
          endif
      endif
c
c...Write clfile record
c
  400 call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncitls
c
c   FUNCTION:  This routine stores the tool description text.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ncitls
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 ist,ien
c
c...Save tool description
c
      ist    = RCSUB(1)
      ien    = ICNC(1)
      TLNAME(1) = LCTXT(ist:ien)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncitln
c
c   FUNCTION:  This routine stores the programmed tool length as the
c              tool length + the holder length.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine ncitln
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      equivalence (NCUT  ,KPOSMP(0064))
c
      integer*4 NCUT
c
      equivalence (CUTTER,POSMAP(0744)), (TL    ,POSMAP(3601))
c
      real*8 TL(120),CUTTER(7)
c
c...Store tool length
c
      TL(1) = RCSUB(3)
      CUTTER(3) = RCSUB(2)
      NCUT = 3
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncitlc (ktyp,cmsg,kerr)
c
c   FUNCTION:  This routine stores tool change records in the clfile.
c
c   INPUT:  ktyp    I*4  D1  Record type (1000,1001,1002)
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncitlc (ktyp,cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kerr,ktyp
c
      character*(*) cmsg
c
      equivalence (MACHTP,KPOSMP(1201)), (MCHPLN,KPOSMP(1261))
c
      integer*4 MCHPLN,MACHTP
c
      equivalence (CUTTER,POSMAP(0744))
      equivalence (CTOFRG,POSMAP(2401)), (TL    ,POSMAP(3601))
c
      real*8 CTOFRG,TL(120),CUTTER(7)
c
      integer*2 inum(4)
      integer*4 i,strlen1
c
      real*8 rnum
c
      equivalence (inum,rnum)
c
c...Initialize routine
c
      inum(is4) = 0
c
c...Output POSTN command
c
      if (ktyp .eq. 1001) then
          INBUF(3) = 2000
          INBUF(4) = 1024
          ROBUF(1) = RCSUB(1)
          MXC    = 1
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
       endif
c
c...Output SELCTL/LOADTL command
c
      if (ktyp .ne. 1000) then
          INBUF(3) = 2000
c
c......Lathe Tool change
c
          if (MACHTP .eq. 2) then
              INBUF(4) = 1033
              ROBUF(1) = RCSUB(4)
              MXC    = 1
              if (CUTTER(1) .ne. 0.) then
                  inum(is1) = 23
                  ROBUF(2) = rnum
                  ROBUF(3) = CUTTER(1)
                  MXC    = 3
              endif
              MXC    = MXC    + 1
              inum(is1) = 705
              ROBUF(MXC) = rnum
              MXC    = MXC    + 1
              ROBUF(MXC) = RCSUB(5)
c
c......Mill Tool change
c
          else
              INBUF(4) = 1055
              if (ktyp .eq. 1000) INBUF(4) = 1056
              ROBUF(1) = RCSUB(4)
              MXC    = 1
              if (TL(1) .ne. 0.) then
                  inum(is1) = 9
                  ROBUF(2) = rnum
                  ROBUF(3) = TL(1)
                  MXC    = 3
                  TL(1) = 0.
              endif
          endif
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Output tool description
c
      if (ktyp .ne. 1000 .and. TLNAME(1) .ne. ' ') then
          LCTXT(1:66) = 'TLN,' // TLNAME(1)
          RCSUB(1) = 1.
          ICNC(1) = strlen1(LCTXT(1:66))
c
c...Change record type 1008 to 99999 to prevent checking
c   PPRINT MACHIN logic which modifies the RCSUB array
c
          call ncippr (99999,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          TLNAME(1) = ' '
      endif
c
c...Output CUTTER record
c
      if (ktyp .ne. 1000) then
          INBUF(3) = 6000
          INBUF(4) = 1
c
c......Lathe cutter
c
          if (MACHTP .eq. 2) then
              ROBUF(1) = CUTTER(1) * 2.
              MXC    = 1
c
c......Mill cutter
c
          else
              ROBUF(1) = CUTTER(1)
              ROBUF(2) = CUTTER(2)
              ROBUF(3) = CUTTER(3)
              MXC    = 3
          endif
c
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      endif
c
c...Output SPINDL command
c
      if (RCSUB(8) .ne. 0) then
          call ncispn (RCSUB(8),RCSUB(17),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Output tool change support commands
c
      if ((ktyp .ne. 1000 .or. MACHTP .eq. 2) .and. RCSUB(6) .ne. 0)
     1        then
          INBUF(3) = 2000
c
c......SPINDL/MAXRPM
c
          if (MACHTP .eq. 2) then
              INBUF(4) = 1031
              inum(is1) = 79
              ROBUF(1) = rnum
              ROBUF(2) = RCSUB(6)
              MXC    = 2
c
c......TOOLNO/ADJUST
c
          else
              INBUF(4) = 1025
              inum(is1) = 159
              ROBUF(1) = rnum
              ROBUF(2) = RCSUB(6)
              if (RCSUB(7) .eq. 1) then
                  inum(is1) = 37
                  MCHPLN = 1
              else if (RCSUB(7) .eq. 2) then
                  inum(is1) = 41
                  MCHPLN = 2
              else
                  inum(is1) = 33
                  MCHPLN = 3
              endif
              ROBUF(3) = rnum
              inum(is1) = 162
              ROBUF(4) = rnum
              MXC    = 4
           endif
           call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
           if (kerr .ne. 0) go to 8000
      endif
c
c...Output COOLNT command
c
      i      = RCSUB(10)
      if (i .ne. 0) call ncicol (i,cmsg,kerr)
c
c...Store cutter compensation register
c
      CTOFRG = RCSUB(5)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncictr
c
c   FUNCTION:  This routine stores cutter records in the clfile.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncictr (cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      equivalence (ICUTDO,KPOSMP(3301))
c
      integer*4 ICUTDO(16)
c
      equivalence (CUTTER,POSMAP(0744))
c
      real*8 CUTTER(7)
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Setup CUTTER record
c
      CUTTER(1) = RCSUB(2)
      CUTTER(2) = RCSUB(3)
c
c...Setup CUTCOM direction
c
      ICUTDO(3) = RCSUB(1)
      if (ICUTDO(3) .eq. 0) ICUTDO(3) = 40
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncidly (cmsg,kerr)
c
c   FUNCTION:  This routine stores DELAY and SPINDL records in the clfile.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncidly (cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Output DELAY command
c
      INBUF(3) = 2000
      INBUF(4) = 1010
      ROBUF(1) = RCSUB(1)
      MXC    = 1
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Output SPINDL command
c
      call ncispn (RCSUB(2),RCSUB(2),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicol (kcol,cmsg,kerr)
c
c   FUNCTION:  This routine stores COOLNT records in the clfile.
c
c   INPUT:  kcol    I*4  D1  Coolant type, 0 = OFF, 1= FLOOD, 2 = MIST,
c                            3 = TOOL.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncicol (kcol,cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kerr,kcol
c
      character*(*) cmsg
c
      integer*2 inum(4),iwrd(4)
c
      real*8 rnum
c
      equivalence (inum,rnum)
c
      data iwrd /72,89,90,1011/
c
c...Initialize routine
c
      inum(is4) = 0
c
c...Setup COOLNT command
c
      INBUF(3) = 2000
      INBUF(4) = 1030
      inum(is1) = iwrd(kcol+1)
      ROBUF(1) = rnum
      MXC    = 1
c
c...Write clfile record
c
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncispn (grpm,gdir,cmsg,kerr)
c
c   FUNCTION:  This routine stores SPINDL records in the clfile.
c
c   INPUT:  grpm    R*8  D1  + = Spindle clockwise speed, 0 = Spindle off,
c                            - = Spindle counter-clockwise speed. (Mill)
c
c                            + = Spindle RPM, 0 = Spindle off,
c                            - = Spindle SFM (Lathe)
c
c           gdir    R*8  D1  + = Spindle CLW, 0 = Spindle off,
c                            - = Spindle CCLW. (Lathe)
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncispn (grpm,gdir,cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kerr
c
      real*8 grpm,gdir
c
      character*(*) cmsg
c
      equivalence (MACHTP,KPOSMP(1201))
c
      integer*4 MACHTP
c
      integer*2 inum(4)
c
      real*8 rnum
c
      equivalence (inum,rnum)
c
c...Initialize routine
c
      inum(is4) = 0
      INBUF(3) = 2000
      INBUF(4) = 1031
c
c...Setup SPINDL command
c......SPINDL/OFF
c
      if (grpm .eq. 0) then
          inum(is1) = 72
          ROBUF(1)  = rnum
          MXC    = 1
c
c......Lathe Spindle
c
      else if (MACHTP .eq. 2) then
c
c..........SPINDL/SFM
c
          if (grpm .lt. 0.) then
              inum(is1) = 115
              ROBUF(1) = rnum
              ROBUF(2) = 0.-grpm
c
c..........SPINDL/RPM
c
          else
              inum(is1) = 78
              ROBUF(1) = rnum
              ROBUF(2) = grpm
          endif
c
c..........SPINDL/,dir
c
          if (gdir .lt. 0.) then
              inum(is1) = 59
              ROBUF(3) = rnum
          else
              inum(is1) = 60
              ROBUF(3) = rnum
          endif
          MXC    = 3
c
c......Mill Spindle
c
      else
          if (grpm .lt. 0.) then
              ROBUF(1) = 0.-grpm
              inum(is1) = 59
              ROBUF(2) = rnum
          else
              ROBUF(1) = grpm
              inum(is1) = 60
              ROBUF(2) = rnum
          endif
          MXC    = 2
      endif
c
c...Write clfile record
c
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicut (kcut,cmsg,kerr)
c
c   FUNCTION:  This routine stores CUTCOM records in the clfile.
c
c   INPUT:  kcol    I*4  D1  Cutcom type, 0 = No change, 40,140 = OFF,
c                            41 = LEFT, 42 = RIGHT.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncicut (kcut,cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kerr,kcut
c
      character*(*) cmsg
c
      equivalence (MCHPLN,KPOSMP(1261)), (ICUTDO,KPOSMP(3301))
c
      integer*4 MCHPLN,ICUTDO(16)
c
      equivalence (CTOFRG,POSMAP(2401))
c
      real*8 CTOFRG
c
      integer*2 inum(4),icwrd(2),ipwrd(3)
c
      real*8 rnum
c
      equivalence (inum,rnum)
c
      data icwrd /8,24/, ipwrd /37,41,33/
c
c...Initialize routine
c
      inum(is4) = 0
c
c...Setup CUTCOM record
c
      INBUF(3) = 2000
      INBUF(4) = 1007
      if (kcut .eq. 40. .or. kcut .eq. 140.) then
          inum(is1) = 72
          ROBUF(1) = rnum
          MXC    = 1
      else
          inum(is1) = icwrd(kcut-40)
          ROBUF(1) = rnum
          inum(is1) = ipwrd(MCHPLN)
          ROBUF(2) = rnum
          MXC    = 2
          if (CTOFRG .ne. 0) then
              ROBUF(3) = CTOFRG
              MXC    = 3
          endif
      endif
c
c...Write clfile record
c
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store CUTCOM state
c
      ICUTDO(3) = kcut
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncixfm (ktyp,gmx,cmsg,kerr)
c
c   FUNCTION:  This routine stores transformation matrix (Working
c              Coordinate System) records in the clfile.
c
c   INPUT:  ktyp    I*4  D1  1014 = Tool Plane View Matrix,
c                            1027 = Working Coordinate System
c           gmx     R*4  D12 Active transformation matrix.
c
c   OUTPUT: gmx     R*4  D12 Active transformation matrix.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncixfm (ktyp,gmx,cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 ktyp,kerr
c
      real*8 gmx(12)
c
      character*(*) cmsg
c
      integer*2 inum(4)
      integer*4 i,ifl,inc
c
      real*8 rnum
c
      equivalence (rnum,inum)
c
c...Output 1027 WCS as
c...TRANS/TRFORM,mx
c
      inc    = 0
      if (ktyp .eq. 1027) then
          inc    = 1
          inum(is1) = 110
          ROBUF(1) = rnum
      endif
c
c...Get copy of matrix
c
      do 100 i=1,3,1
          ROBUF(i+inc) = RCSUB(i)
          ROBUF(i+4+inc) = RCSUB(i+3)
          ROBUF(i+8+inc) = RCSUB(i+6)
  100 continue
      if (ktyp .eq. 1027) then
          ROBUF(4+inc) = RCSUB(10)
          ROBUF(8+inc) = RCSUB(11)
          ROBUF(12+inc) = RCSUB(12)
      else
          ROBUF(4+inc) = 0.
          ROBUF(8+inc) = 0.
          ROBUF(12+inc) = 0.
      endif
c
c...Compare with active matrix
c
      ifl    = 0
      do 200 i=1,12,1
          if (ROBUF(i+inc) .ne. gmx(i)) ifl = 1
  200 continue
c
c...Setup TRANS command
c
      if (ifl .eq. 1) then
          INBUF(3) = 2000
          INBUF(4) = 1037
          MXC    = inc    + 12
c
c...Write clfile record
c
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...Save active matrix
c
          do 500 i=1,12,1
              gmx(i) = ROBUF(i+inc)
  500     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncimsc (cmsg,kerr)
c
c   FUNCTION:  This routine stores CUTCOM/ADJUST records in the clfile.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncimsc (cmsg,kerr)
c
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 inum(4)
c
      real*8 rnum
c
      equivalence (inum,rnum)
c
c...Initialize routine
c
      inum(is4) = 0
      INBUF(3) = 2000
      INBUF(4) = 1007
c
c...Setup CUTCOM/ADJUST command
c
      inum(is1) = 159
      ROBUF(1)  = rnum
      ROBUF(2)  = RCSUB(9)
      MXC    = 2
c
c...Write clfile record
c
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
