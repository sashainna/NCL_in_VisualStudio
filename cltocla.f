c
c***********************************************************************
c
c   FILE NAME:  cltocla
c         Functions to convert CL file to Cla file
c   CONTAINS:
c             ptd_clclose
c             clopen(clfile, nc)
c             init_load
c             ptd_cltocla (krec,kpt, ctape, nc, kerr)
c             cltocla (krec,kpt, ctape, nc, flag, kerr)
c             rtoc88 (gnum,cbuf,knc)
c             add_space(ctape, spos, nc)
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cltocla.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/29/13 , 16:07:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_clclose
c
c   FUNCTION:  This routine close a binary clfile
c
c
c   INPUT: None
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine ptd_clclose
      include 'menu.inc'

      call clsfil(LUNSC1)
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  ptd_clopen(clfile, nc)
c
c   FUNCTION:  This routine open a binary clfile
c
c
c   INPUT: clfile:   clfile to open.
c
c          nc:       number of characters in clfile
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine ptd_clopen(clfile, nc)
c
      include 'menu.inc'
c
      integer*4 nc
      character*(MAX_PATH) clfile
c
      character*(MAX_PATH) fnam
C
      fnam = clfile(1:nc)
      call clopen(fnam, nc)
      return
      end


c***********************************************************************
c
c   SUBROUTINE:  clopen(clfile, nc)
c
c   FUNCTION:  This routine open a binary clfile
c
c
c   INPUT: clfile:   clfile to open.
c
c          nc:       number of characters in clfile
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine clopen(clfile, nc)

      include 'menu.inc'

      character*(*) clfile
      integer*4 nc, kerr

      integer*4 irecl
      character*20 att(4)
      character*80 cmsg

      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 288

      call opnfil (LUNSC1,clfile, att, irecl,cmsg,kerr)
      return

      end
c***********************************************************************
c
c   SUBROUTINE:  init_load
c
c   FUNCTION:  This routine initialize value before load cl file
c
c
c   INPUT: None
c
c   OUTPUT: None
c
c***********************************************************************
c
      subroutine init_load

      include 'post.inc'
      include 'clnrd.inc'
c
      equivalence (MULTAX,KPOSMP(0056)), (NPT   ,KPOSMP(0059))
      equivalence (NCUT  ,KPOSMP(0062))
      equivalence (ICLREC,KPOSMP(0002))

      integer*4 MULTAX, NPT, NCUT, ICLREC
c
      equivalence (VECSAV,POSMAP(1372)), (SPIVEC,POSMAP(3583))
c
      real*8 VECSAV(3),SPIVEC(3)

      MULTAX   = 0
      NPT = 3
      NCUT   = 0
      ICLREC = 0
      LSTIRC = 0
      VECSAV(1) = SPIVEC(1)
      VECSAV(2) = SPIVEC(2)
      VECSAV(3) = SPIVEC(3)
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  ptd_cltocla (krec,kpt, ctape, nc, kerr)
c
c   FUNCTION:  This routine read a binary clfile record
c              and convert to a text string
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c           kpt     I*4  D1  Pointer to next logical clfile record
c                            within physical record.
c
c   OUTPUT: ctape:  converted text string
c
c           nc:     number of characters in "ctape"
c
c***********************************************************************
c
      subroutine ptd_cltocla (krec,kpt, ctape, nc, flag, kerr)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     integer*4 krec,kpt,kerr, nc, flag
C     character*10000 ctape
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      integer*4 i, krec, nc, kerr
      integer*4 kpt, flag
      byte ctape(100000)
      byte ctape1(100000)
      character*100000 cdat
      equivalence (cdat, ctape1)
      integer knc, kpt1, krec1, err
C
      knc = nc
      kpt1 = kpt
      krec1 = krec

      call cltocla (krec1,kpt1, cdat, knc, flag, err)
      kpt = kpt1
      krec = krec1
      nc = knc
      kerr = err
      do 200 i = 1, knc, 1
         ctape(i) = ctape1(i)
  200 continue

      return
C WNT-END
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call cltocla (krec,kpt, ctape, nc, flag, kerr)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  cltocla (krec,kpt, ctape, nc, flag, kerr)
c
c   FUNCTION:  This routine read a binary clfile record
c              and convert to a text string
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c           kpt     I*4  D1  Pointer to next logical clfile record
c                            within physical record.
c
c   OUTPUT: ctape:  converted text string
c
c           nc:     number of characters in "ctape"
c           flag: 0, FINI reached
c                 1: end with text string, no new line char added
c                 2: end with numbers or keyword,  no new line char added
c                 3: end with numbers, new line char added
c                 4: Text post-processor command.
c
c***********************************************************************
c
      subroutine cltocla (krec,kpt, ctape, nc, flag, kerr)
c
      include 'menu.inc'
      include 'post.inc'
      include 'clnrd.inc'

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
      equivalence (PSTWD ,POSMAP(0441)), (TLATOL,POSMAP(0027))
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (CLSAV ,POSMAP(0201)), (CUTTER,POSMAP(0744))
      equivalence (CUDISP,POSMAP(0759)), (VECSAV,POSMAP(1372))
      equivalence (REFMAT,POSMAP(4001)), (TRAMAT,POSMAP(4013))
      equivalence (CLPTPP,POSMAP(4605)), (CIRBPP,POSMAP(4845))

      equivalence (KRDTEXT,  KPOSMP(4229))
      character*40 cpstnum
      integer*4 KRDTEXT
c
      real*8 METCNV,PSTWD(50),CLPT(240),CIRBUF(7),CLSAV(21),
     1       CUTTER(7),VECSAV(3),REFMAT(12),TRAMAT(12),CUDISP(7),
     2       CLPTPP(240),CIRBPP(7),CLPOS(21),TLATOL
c
      integer*2 ICLDAT(200)
      integer*4 JCLDAT(200), jtemp
      character*8 lclt,lclnm,lclrv
      character*11 lcld
      character*82 chtemp
      character*800 LCLDAT
      character*1  LCDAT2(800)
      character*80 lclf
c
      equivalence (RCLDAT,ICLDAT,JCLDAT,LCLDAT,LCDAT2)
      equivalence (RCLDAT(1),lclf), (RCLDAT(11),lcld)
      equivalence (RCLDAT(13),lclt), (RCLDAT(14),lclnm)
      equivalence (RCLDAT(15),lclrv)
c
      equivalence (LPSTWD,CPOSMP(0001))
c
      character*66 LPSTWD
c
      integer*4 krec,kpt,kerr,flag,nc, is1,is4, knc, ic, nns,ival,inc,
     1          mxc,np,imx(3),ierr,inum(2)
      character*(*) ctape
c
      character*80 cmsg
      integer*4 rindex, strlen1, wrdsave, numsave
c
      integer*4 i,j,isx(3),ILAST
      character*24 csnum
      character*200 ctext
c
      real*8 rver,rnum
c
      REAL*8 RTEXT(9)
      EQUIVALENCE (RTEXT,ctext)
      integer*4 igtyp, k, nxt,ist
c
C VAX-SUN-SGI-IBM-HPX-START
C      byte nl
C      character*1 newline
C      EQUIVALENCE (nl,newline)
C      data nl /10/
C      integer*4 leol /1/
C      data is1 /0/, is4 /3/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      byte nl(2) /13,10/
      character*2 newline
      EQUIVALENCE (nl,newline)
      integer*4 leol /2/
      data is1 /3/, is4 /0/
C WNT-DOS-DEC-END
c
      equivalence (rnum,inum)
c
      flag = 2
      KRDTEXT = 1
      ICLF = 1
      call clread (krec,kpt,cmsg,kerr)
      if (kerr.ne.0) return
      if (MXCL.lt.0) then
         kerr = 1
         return
      endif
      nns = 0
C
C...SET SEQNO
C
      ILAST   = 8
      write (ctape(1:6),5) ISN
    5 FORMAT (1HN,I5)
C
C...shift seqno left
C
      nns     = rindex(ctape(1:6),' ')
      if (nns .ne. 0) ctape(2:6) = ctape(nns+1:6)
      ctape(7:) = ', '
C
C...GO TO PROPER RECORD TYPE
C
c
c...ISN record
c
      if (ITYPE .eq. 1000) then
          call getvwd (4023,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+2) = csnum(1:ic) // '/ '
          ilast  = ilast + ic + 2
c
          if (MXCL .gt. 0) then
              inc = 1
              call itoc (JCLDAT(1),csnum,knc,-1)
              ctape(ilast+1:ilast+knc) = csnum(1:knc)
              ilast = ilast + knc
c
  100         if (inc .gt. JCLDAT(1)) go to 110
              inc = inc + 1
              call itoc (JCLDAT(inc),csnum,knc,-1)
              ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
              ilast = ilast + knc + 2
              if (inc .eq. -1) then
                  inum(1) = JCLDAT(inc+1)
                  inum(2) = JCLDAT(inc+2)
                  call rtoc (rnum,csnum,knc)
                  ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
                  ilast = ilast + knc + 2
                  inc = inc + 2
              endif
              go to 100
  110         continue
          endif
c
c...Post word
c
      else if (ITYPE .eq. 2000) then
c
c...major word
c
          call getvwd (ISUBT,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+1) = csnum(1:ic) // '/'
          ilast  = ilast + ic + 1
          IF (MXCL .EQ. 0) ilast = ilast - 1
c
c......LETTER, PARTNO, INSERT and PPRINT
c
          if (ISUBT .ge. 1043 .and. ISUBT .le. 1046) then
              ctape(ilast+1:ilast+66)= LCLDAT(1:66)
              NC = strlen1(ctape)
              flag = 4
              return
          else
c
c......Major/Minor word
c
c
c...major word
c
              nxt = 1
              if (ISUBT.eq.1015) then
C
C...MACHIN/NAME
C
                  if (JCLDAT(1) .eq. -1 .and. JCLDAT(2) .gt. 8 .and.
     1                    JCLDAT(2) .le. 40) then
                      nc     = JCLDAT(2)
                      ctext(1:nc) = LCLDAT(9:8+nc)
                      nxt    = (nc+7)/8 + 2
                  else
                      nc     = 8
                      if (ichar(LCLDAT(7:7)) .le. 32) then
                          nc = 6
                      else if (ichar(LCLDAT(8:8)) .le. 32) then
                          nc = 7
                      endif

                      RTEXT(1) = RCLDAT(1)
                      nxt = 2
                  endif
                  ctext(nc+1:nc+2) = ', '
                  ctape(ilast+1:) = ctext(1:nc+2)
                  ilast  = ilast  + nc     + 2
              endif
              do 300 i=nxt,MXCL,1
                  if (IPSTWD(i).ne.0) then
c
c...vocabulary word (MINOR). Should have been converted in nclrd()
c
                      call getvwd (IPSTWD(i),csnum,ic,2,PSTWRD,
     -                   PSTWVL,NPSTWD)
                      ctape(ilast+1:ilast+ic+2) = csnum(1:ic)//', '
                      ilast  = ilast + ic + 2
                      IF (MXCL .EQ. i) ilast = ilast - 2
                  else
c
c...number
c
                      call rtoc(PSTWD(i), cpstnum, knc)
                      ctape(ilast+1:ilast+knc+2) = cpstnum(1:knc)//', '
                      ilast  = ilast + knc + 2
                      IF (MXCL .EQ. i) ilast = ilast - 2
                  endif
                  flag = 2
  300         continue
         endif
c
c...STOCK/FIXTUR
c
      else if (ITYPE .eq. 2600 .or. ITYPE .eq. 2601) then
          ival = 321
          if (ITYPE .eq. 2601) ival = 898
          call getvwd (ival,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+1) = csnum(1:ic) // '/'
          ilast  = ilast + ic + 1
c
c......Get stock type
c
          if (ISUBT .eq. 732) ISUBT = 55
          if (ISUBT .eq. 620) ISUBT = 4042
          ival = ISUBT
          if (ISUBT .eq. 331) ival = 843
          call getvwd (ival,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+1) = csnum(1:ic)
          ilast  = ilast + ic
c
c......STOCK/BOX-CYLNDR
c
          if (ISUBT .eq. 340 .or. ISUBT .eq. 4042 .or. ISUBT .eq. 632
     1        .or. ISUBT .eq. 631 .or. ISUBT .eq. 627) then
              call itoc(JCLDAT(1), csnum, knc, -1)
              ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
              ilast  = ilast + knc + 2
              call itoc(JCLDAT(2), csnum, knc, -1)
              ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
              ilast  = ilast + knc + 2
              do 400 i=2,MXCL,1
                  call rtoc(RCLDAT(i), cpstnum, knc)
                  ctape(ilast+1:ilast+knc+2) = ', ' // cpstnum(1:knc)
                  ilast  = ilast + knc + 2
  400         continue
c
c......STOCK/LOAD
c
          else if (ISUBT .eq. 1075) then
              call itoc(JCLDAT(1), csnum, knc, -1)
              ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
              ilast  = ilast + knc + 2
              knc = JCLDAT(2)
              ctape(ilast+1:ilast+knc+3) = ',"' // LCLDAT(9:knc+8) //
     1            '"'
              ilast  = ilast + knc + 3
c
c......STOCK/STL
c
          else if (ISUBT .eq. 330) then
              call itoc(JCLDAT(1), csnum, knc, -1)
              ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
              ilast  = ilast + knc + 2
              ival = 303
              if (JCLDAT(3) .eq. 1) ival = 301
              call getvwd (ival,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
              ctape(ilast+1:ilast+ic+2) = ', ' // csnum(1:ic)
              ilast  = ilast + ic + 2
              knc = JCLDAT(2)
              ctape(ilast+1:ilast+knc+3) = ',"' // LCLDAT(17:knc+16) //
     1            '"'
              ilast  = ilast + knc + 3
c
c......STOCK/CLONE
c
          else if (ISUBT .eq. 576) then
              do 420 i=1,3,1
                  call itoc(JCLDAT(i), csnum, knc, -1)
                  ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
                  ilast  = ilast + knc + 2
  420         continue
c
c......STOCK/MOVE
c
          else if (ISUBT .eq. 577) then
              do 430 i=2,13,1
                  call rtoc(RCLDAT(i), cpstnum, knc)
                  ctape(ilast+1:ilast+knc+2) = ', ' // cpstnum(1:knc)
                  ilast  = ilast + knc + 2
  430         continue
              knc = strlen1(LCLDAT(105:110))
              ctape(ilast+1:ilast+knc+2) = ', ' // LCLDAT(105:104+knc)
              ilast  = ilast + knc + 2
              inc = 28
              do 440 i=1,JCLDAT(1),1
                  inc = inc + 1
                  call itoc(JCLDAT(inc), csnum, knc, -1)
                  ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
                  ilast  = ilast + knc + 2
  440         continue
c
c......STOCK/REMOVE
c
          else if (ISUBT .eq. 843) then
              inc = 2
              do 450 i=1,JCLDAT(1),1
                  inc = inc + 1
                  call itoc(JCLDAT(inc), csnum, knc, -1)
                  ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
                  ilast  = ilast + knc + 2
  450         continue
c
c......STOCK/REMOVE,CHIPS
c
          else if (ISUBT .eq. 331) then
              call getvwd (ISUBT,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
              ctape(ilast+1:ilast+ic+2) = ', ' // csnum(1:ic)
              ilast  = ilast + ic + 2
              do 455 i=1,MXCL,1
                  call rtoc(RCLDAT(i), cpstnum, knc)
                  ctape(ilast+1:ilast+knc+2) = ', ' // cpstnum(1:knc)
                  ilast  = ilast + knc + 2
  455         continue
c
c......STOCK/MODIFY
c
          else if (ISUBT .eq. 55) then
              do 460 i=2,5,1
                  call itoc(JCLDAT(i), csnum, knc, -1)
                  ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
                  ilast  = ilast + knc + 2
  460         continue
              call rtoc(RCLDAT(4), cpstnum, knc)
              ctape(ilast+1:ilast+knc+2) = ', ' // cpstnum(1:knc)
              ilast  = ilast + knc + 2
              inc = 8
              do 470 i=1,JCLDAT(1),1
                  inc = inc + 1
                  call itoc(JCLDAT(inc), csnum, knc, -1)
                  ctape(ilast+1:ilast+knc+2) = ', ' // csnum(1:knc)
                  ilast  = ilast + knc + 2
  470         continue
          endif
c
c...Circular record
c
      else if (ITYPE .eq. 3000) then
         call getvwd (4026,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
         ctape(ilast+1:ILAST+ic+2) = csnum(1:ic) // '/ '
         ilast  = ilast + ic + 3
c
c...circle value
c
         do 500 i=1,7,1
             call rtoc(CIRBUF(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//', '
             ilast  = ilast + knc + 2
             IF (7 .EQ. i) ilast = ilast - 2
  500    continue
         flag = 2
c
c...Motion record
c
      else if (ITYPE .eq. 5000) then
         if (ISUBT.eq.3) then
c...FROM
             igtyp = 4019
         else if (ISUBT.eq.6) then
c...GOTO continue
             igtyp = 4013
             ctape(1:1) = 'C'
c...GOTO
         else
             igtyp = 4013
         endif
c
c...store as NPT a group, total MXCL group
c
         wrdsave = ilast
         k = -1
  510    k = k+1
         call getvwd (igtyp,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
         ctape(ilast+1:ilast + ic + 2) = csnum(1:ic) // '/ '
         ilast  = ilast + ic + 2
         if (k.eq.0) numsave = ilast
         do 550 i=k*NPT+1,k*NPT+3,1
             call rtoc88(CLPT(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
c             if ((i.eq.(k*NPT+3)).and.(NPT.eq.6)) then
c                 ctape(ilast+1:ilast+2) = '$'//newline
c                 ilast = ilast + 2
c             else if ((i.eq.(k*NPT+3)).and.(NPT.eq.3)) then
c                 ctape(ilast:ilast) = newline
c             endif
  550    continue
         if (NPT.eq.6) then
           ctape(ilast+1:ilast+leol+1) = '$' // newline
           ilast = ilast + leol + 1
         else
	     ctape(ilast:ilast+leol-1) = newline
           ilast = ilast+leol-1
         endif
         if (NPT.eq.6) then
c
c...added space for line up
c
             call add_space(ctape, ilast+1, numsave)
             ilast = ilast + numsave

             do 600 i=k*NPT+4,k*NPT+6,1
                call rtoc88(CLPT(i), cpstnum, knc)
                ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
                ilast  = ilast + knc + 1
c                if (i .EQ. (k*NPT+6)) then
c                    ctape(ilast:ilast) = newline
c                endif
  600        continue
           ctape(ilast:ilast+leol-1) = newline
           ilast = ilast+leol-1
         endif
         if ((k+1).lt.MXCL) then
c
c...added space for line up
c
             call add_space(ctape, ilast+1, wrdsave)
             ilast = ilast + wrdsave
             goto 510
         endif
         flag = 3
c
c...End position record
c
      else if (ITYPE .eq. 5220) then
          call getvwd (4019,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+2) = csnum(1:ic) // '/ '
          ilast  = ilast + ic + 2
c
c...save the value for line up
c
          wrdsave = ilast
c
c...5 spaces for "END" line up
c
          call getvwd (1,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)// ','
          ilast  = ilast + 6
c
c...save the value for line up
c
          numsave = ilast
c
c...6 value followed, 3 value a line
c
          do 910 i=1,3,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
  910     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, numsave)
          ilast = ilast + numsave
c
          do 920 i=4,6,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
  920     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, wrdsave)
          ilast = ilast + wrdsave
c
c...5 spaces for "CS" line up
c
          call getvwd (753,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...6 value followed, 3 value a line
c
          do 930 i=7,9,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
  930     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, numsave)
          ilast = ilast + numsave
c
          do 940 i=10,12,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
  940     continue
          ctape(ilast:ilast+leol-1) = newline
          ilast = ilast + leol - 1
         flag = 3

      else if (ITYPE .eq. 5210) then
          call getvwd (4019,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+2) = csnum(1:ic) // '/ '
          ilast  = ilast + ic + 2
c
c...save the value for line up
c
          wrdsave = ilast
c
c...5 spaces for "START" line up
c
          call getvwd (57,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...save the value for line up
c
          numsave = ilast
c
c...21 value a group
c
          k = -1
  950     k = k+1
c
c...6 value followed, 3 value a line
c
          do 1000 i=k*21+1, k*21+3,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1000     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, numsave)
          ilast = ilast + numsave
c
          do 1010 i=k*21+4,k*21+6,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1010     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, wrdsave)
          ilast = ilast + wrdsave
c
c...5 spaces for "FWD" line up
c
          call getvwd (651,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...3 value followed, 3 value a line
c
          do 1020 i=k*21+7,k*21+9,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1020     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, wrdsave)
          ilast = ilast + wrdsave
c
c...5 spaces for "PS" line up
c
          call getvwd (728,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...6 value followed, 3 value a line
c
          do 1030 i=k*21+10,k*21+12,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
c             endif
 1030     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, numsave)
          ilast = ilast + numsave
c
          do 1040 i=k*21+13,k*21+15,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1040     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, wrdsave)
          ilast = ilast + wrdsave
c
c...5 spaces for "DS" line up
c
          call getvwd (729,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...6 value followed, 3 value a line
c
          do 1050 i=k*21+16,k*21+18,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1050     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, numsave)
          ilast = ilast + numsave
c
          do 1060 i=k*21+19,k*21+21,1
             call rtoc88(CLPOS(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1060     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
          if ((k+1)*21.lt.MXCL) then
c
c...added space for line up
c
              call add_space(ctape, ilast+1, numsave)
              ilast = ilast + numsave
              goto 950
          else
              ilast = ilast - leol - 1
              ctape(ilast:ilast+leol-1) = newline
              ilast = ilast+leol-1
          endif
          flag = 3

      else if (ITYPE .eq. 5200) then
          if (ISUBT.eq.6) ctape(1:1) = 'C'
          call getvwd (4013,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+2) = csnum(1:ic) // '/ '
          ilast  = ilast + ic + 2
c
c...save the value for line up
c
          wrdsave = ilast
c
c...5 spaces for "LARGE" line up
c
          call getvwd (7,csnum,ic,2,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...save the value for line up
c
          numsave = ilast
c
c...21 value a group
c
          k = -1
 1080     k = k+1
c
c...6 value followed, 3 value a line
c
          do 1100 i=k*6+1,k*6+3,1
             call rtoc88(CLPT(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1100     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast + 1, numsave)
          ilast = ilast + numsave
c
          do 1110 i=k*6+4,k*6+6,1
             call rtoc88(CLPT(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1110     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, wrdsave)
          ilast = ilast + wrdsave
c
c...5 spaces for "FWD" line up
c
          call getvwd (651,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...3 value followed, 3 value a line
c
          do 1120 i=k*3+1,k*3+3,1
             call rtoc88(CLFWD(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1120     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast + 1, wrdsave)
          ilast = ilast + wrdsave
c
c...5 spaces for "PS" line up
c
          call getvwd (728,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...6 value followed, 3 value a line
c
          do 1130 i=k*3+1,k*3+3,1
             call rtoc88(CLPTE1(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
c             endif
 1130     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, numsave)
          ilast = ilast + numsave

          do 1140 i=k*3+1,k*3+3,1
             call rtoc88(CLPTE2(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1140     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1

c
c...added space for line up
c
          call add_space(ctape, ilast+1, wrdsave)
          ilast = ilast + wrdsave
c
c...5 spaces for "DS" line up
c
          call getvwd (729,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+6) = csnum(1:ic)//','
          ilast  = ilast + 6
c
c...6 value followed, 3 value a line
c
          do 1150 i=k*3+1,k*3+3,1
             call rtoc88(CLPTE3(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
c             endif
 1150     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
c
c...added space for line up
c
          call add_space(ctape, ilast+1, numsave)
          ilast = ilast + numsave
c
          do 1160 i=k*3+1,k*3+3,1
             call rtoc88(CLPTE4(i), cpstnum, knc)
             ctape(ilast+1:ilast+knc+1) = cpstnum(1:knc)//','
             ilast  = ilast + knc + 1
 1160     continue
          ctape(ilast+1:ilast+leol+1) = '$' // newline
          ilast = ilast + leol + 1
          if ((k+1) .lt. MXCL) then
c
c...added space for line up
c
              call add_space(ctape, ilast+1, numsave)
              ilast = ilast + numsave
              goto 1080
          else
              ilast = ilast - leol - 1
              ctape(ilast:ilast+leol-1) = newline
              ilast = ilast + leol - 1
          endif
          flag = 3
c
c...Cutter definition
c
      else if (ITYPE .eq. 6000) then
          call getvwd (4025,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+2) = csnum(1:ic) // '/ '
          ilast  = ilast + ic + 2
          do 900 i=1,NCUT,1
             if (CUDISP(i) .eq. 700-10000) then
                 cpstnum = 'LATHE'
                 knc = 5
             else
                 call rtoc(CUDISP(i), cpstnum, knc)
             endif
             ctape(ilast+1:ilast+knc+2) = cpstnum(1:knc)//', '
             ilast  = ilast + knc + 2
             IF (NCUT .EQ. i) ilast = ilast - 2
  900    continue
         flag = 2
c
c...TRACUT
c
      else if (ITYPE .eq. 7000) then
          call getvwd (1038,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+2) = csnum(1:ic) // '/ '
          ilast  = ilast + ic + 2
c
c......TRACUT/NOMORE
c
          if (ISUBT .eq. 2) then
              call getvwd (53,csnum,ic,2,PSTWRD,PSTWVL,NPSTWD)
              ctape(ilast+1:ilast+ic+2) = csnum(1:ic)
              ilast  = ilast + ic
c
c......TRACUT/mx
c
          else
             do 1600 i=1,MXCL,1
                call rtoc(RCLDAT(i), cpstnum, knc)
                ctape(ilast+1:ilast+knc+2) = cpstnum(1:knc)//', '
                ilast  = ilast + knc + 2
                if (i .eq. MXCL) ilast = ilast - 2
 1600        continue
             flag = 2
          endif
c
c...CUTTER/DISPLY
c
      else if (ITYPE .eq. 7100) then
          call getvwd (4025,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+2) = csnum(1:ic) // '/ '
          ilast  = ilast + ic + 2
          ctape(ilast+1:ilast+10) = 'DISPLY, '
          ilast = ilast + 8
c
c......CUTTER/DISPLY,params
c
          if (ISUBT .eq. 1) then
             do 1700 i=1,NCUTDS,1
                call rtoc(CUDISP(i), cpstnum, knc)
                ctape(ilast+1:ilast+knc+2) = cpstnum(1:knc)//', '
                ilast  = ilast + knc + 2
                if (NCUTDS .eq. i) ilast = ilast - 2
 1700        continue
c
c......CUTTER/DISPLY,sym (Pre NCL V9.6)
c
          else if (ISUBT .eq. 2) then
             chtemp = ' '
             do 1705 i = 1, 20, 1
                ctape(ilast+i:ilast+i+1) = LCDAT2(i)
                chtemp(i:i+1) = LCDAT2(i)
 1705        continue
             jtemp = strlen1(chtemp)
             ilast = ilast + jtemp
c
             jtemp = ICLDAT(16)
             call itoc (jtemp,csnum,ic,-1)
             ctape(ilast+1:ilast+ic+2) = ', ' // csnum(1:ic)
             ilast = ilast + ic + 2
c
             if (MXCL .gt. 4) then
                 do 1706 i=5,MXCL,1
                     ctape((ilast+1):(ilast+2)) = ', '
                     ilast = ilast + 2
                     call rtoc(RCLDAT(i),cpstnum,knc)
                     ctape(ilast+1:ilast+knc) = cpstnum(1:knc)
                     ilast  = ilast + knc
 1706            continue
             endif
c
c......CUTTER/DISPLY,PART-ALL
c
          else if (ISUBT .eq. 3) then
              if (ICLDAT(4) .eq. 0) then
                  csnum  = 'PART'
                  knc    = 4
              else
                  csnum  = 'ALL'
                  knc    = 3
              endif
              ctape(ilast+1:ilast+knc) = csnum(1:knc)
              ilast  = ilast + knc
c
c......CUTTER/DISPLY,SHANK (Pre NCL V9.6)
c
          else if (ISUBT .eq. 4) then
              ctape(ilast:(ilast+8)) = 'SHANK, 1'
              ilast = ilast + 8
              do 1720 i=1,MXCL,1
                  if (RCLDAT(i) .eq. 716-10000) then
                      csnum  = 'CUTTER'
                      knc    = 6
                  else if (RCLDAT(i) .eq. 157-10000) then
                      csnum  = 'HOLDER'
                      knc    = 6
                  else
                      call rtoc(RCLDAT(i),csnum,knc)
                  endif
                  ctape(ilast+1:(ilast+2)) = ', '
                  ilast = ilast + 2
                  ctape(ilast+1:ilast+knc) = csnum(1:knc)
                  ilast = ilast + knc
 1720         continue
c
c......CUTTER/DISPLY,sym
c
          else if (ISUBT .eq. 5 .or. ISUBT .eq. 7) then
             if (ISUBT .eq. 5) then
                 mxc = 20
                 ist = 0
             else
                 mxc = ICLDAT(1)
                 ist = 24
             endif
             chtemp = ' '
             do 1730 i = 1, mxc, 1
                ctape(ilast+i:ilast+i+1) = LCDAT2(i+ist)
                chtemp(i:i+1) = LCDAT2(i+ist)
 1730        continue
             jtemp = strlen1(chtemp)
             ilast = ilast + jtemp
c
             ist = 2
             if (ISUBT .eq. 5) ist = 11
             jtemp = ICLDAT(ist)
             call itoc (jtemp,csnum,ic,-1)
             ctape(ilast+1:ilast+ic+2) = ', ' // csnum(1:ic)
             ilast = ilast + ic + 2
c
             if (ISUBT .eq. 5) then
                 ist = 4
                 mxc = MXCL
             else
                 ist = 2
                 mxc = 3
             endif
c
             if (mxc .gt. ist) then
                 do 1735 i=ist,mxc,1
                     ctape((ilast+1):(ilast+2)) = ', '
                     ilast = ilast + 2
                     call rtoc(RCLDAT(i),cpstnum,knc)
                     ctape(ilast+1:ilast+knc) = cpstnum(1:knc)
                     ilast  = ilast + knc
 1735            continue
             endif
c
c.....CUTTER/DISPLY,SHANK-HOLDER
c
          else if (ISUBT .eq. 6 .or. ISUBT .eq. 8) then
             ist = 2
             if (ISUBT .eq. 6) ist = 11
             if (ICLDAT(ist+1) .eq. 2) then
                 ctape(ilast+1:ilast+6) = 'HOLDER'
                 ilast = ilast + 6
             else
                 ctape(ilast+1:ilast+6) = 'SHANK'
                 ilast = ilast + 5
             endif
c
             jtemp = ICLDAT(ist)
             call itoc (jtemp,csnum,ic,-1)
             ctape(ilast+1:ilast+ic+2) = ', ' // csnum(1:ic)
             ilast = ilast + ic + 2
c
             if (ICLDAT(ist) .gt. 1) then
                 ctape(ilast+1:ilast+2) = ', '
                 ilast = ilast + 2
                 if (ISUBT .eq. 6) then
                     mxc = 20
                     ist = 0
                 else
                     mxc = ICLDAT(1)
                     ist = 40
                 endif
                 chtemp = ' '
                 do 1740 i = 1, mxc, 1
                    ctape(ilast+i:ilast+i+1) = LCDAT2(i+ist)
                    chtemp(i:i+1) = LCDAT2(i+ist)
 1740            continue
                 jtemp = strlen1(chtemp)
                 ilast = ilast + jtemp
             endif
c
             if (ISUBT .eq. 6) then
                 ist = 4
                 mxc = MXCL
             else
                 ist = 2
                 mxc = 5
             endif
c
             if (mxc .gt. ist) then
                 do 1748 i=ist,mxc,1
                     ctape((ilast+1):(ilast+2)) = ', '
                     ilast = ilast + 2
                     call rtoc(RCLDAT(i),cpstnum,knc)
                     ctape(ilast+1:ilast+knc) = cpstnum(1:knc)
                     ilast  = ilast + knc
 1748            continue
             endif
c
             ist = 3
             if (ISUBT .eq. 6) ist = 12
             if (ICLDAT(ist) .eq. 0) then
                 ctape(ilast+1:ilast+8) = ', CUTTER'
                 ilast = ilast + 8
             else if (ICLDAT(ist) .eq. 1) then
                 ctape(ilast+1:ilast+8) = ', HOLDER'
                 ilast = ilast + 8
             endif
          endif
c
c...Cutter Profile
c
      else if (ITYPE .eq. 7110 .or. ITYPE .eq. 7120) then
          call getvwd (761,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+1) = csnum(1:ic) // '/'
          ilast  = ilast + ic + 1
c
c......Store profile symbol
c
          if (ITYPE .eq. 7110) then
              mxc = 20
              ist = 0
          else
              mxc = ICLDAT(1)
              ist = 8
          endif
          chtemp = ' '
          do 1800 i = 1, mxc, 1
              ctape(ilast+i+1:ilast+i+1) = LCDAT2(i+ist)
              chtemp(i:i+1) = LCDAT2(i+ist)
 1800     continue
          jtemp = strlen1(chtemp)
          ilast = ilast + jtemp + 1
c
c......Store profile attributes
c
          jtemp = ISUBT
          call itoc(jtemp, csnum, ic, -1)
          ctape(ilast+1:(ilast+ic+2)) = ', ' // csnum(1:ic)
          ilast = ilast + ic + 2
c
          ist = 2
          if (ITYPE .eq. 7110) ist = 6
          np = JCLDAT(ist)
          call itoc(np, csnum, ic, -1)
          ctape(ilast+1:(ilast+ic+2)) = ', ' // csnum(1:ic)
          ilast = ilast + ic + 2
c
          ist = 2
          if (ITYPE .eq. 7110) ist = 13
          jtemp = ICLDAT(ist)
          call itoc(jtemp, csnum, ic, -1)
          ctape(ilast+1:(ilast+ic+2)) = ', ' // csnum(1:ic)
          ilast = ilast + ic + 2
c
c......Store profile
c
          ist = 1 + (mxc+7) / 8
          if (ITYPE .eq. 7110) ist = 4
          do 1840 i=1,np*4,1
              call rtoc(RCLDAT(i+ist), csnum, ic)
              ctape((ilast+1):(ilast+ic+3)) = ", " // csnum(1:ic)
              ilast = ilast + ic + 2
 1840     continue
c
c...SEQUNC
c
      else if (ITYPE .eq. 7200) then
          call getvwd (818,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+1) = csnum(1:ic) // '/'
          ilast  = ilast + ic + 1
c
c......SEQUNC/END
c
          if (ISUBT .eq. 2) then
              call getvwd (1,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
              ctape(ilast+1:ilast+ic+2) = csnum(1:ic)
              ilast  = ilast + ic
          else
c
c......P1 - Sequence number / label
c
              chtemp = ' '
              ilast = ilast + 1
              do 2105 i = 1, 20, 1
                 ctape(ilast+i:ilast+i+1) = LCDAT2(i)
                 chtemp(i:i+1) = LCDAT2(i)
                 if (LCDAT2(i) .eq. ' ') go to 2106
 2105         continue
 2106         jtemp = strlen1(chtemp)
              ilast = ilast + jtemp
              ctape(ilast+1:(ilast+2)) = ", "
              ilast = ilast + 2
c
c......P2-7   FROM(6)
c......P8-13  CUTTER(6)
c......P14-23 CUTTER/DISPLY(10)
c
              do 2110 i = 1, 22, 1
                 call rtoc(RCLDAT(3+i), ctape(ilast+1:), ic)
                 ctape((ilast+ic+1):(ilast+ic+3)) = ", "
                 ilast = ilast + ic + 2
 2110         continue
c
c......P24-26 CUTTER/SYMBOLs w/terminating @ character
c
              if (rver .lt. 9.550) then
                  isx(1) = 200
                  isx(2) = 0
                  isx(3) = 0
                  imx(1) = 10
                  imx(2) = 0
                  imx(3) = 0
              else if (rver .lt. 9.650) then
                  isx(1) = 552
                  isx(2) = 572
                  isx(3) = 592
                  imx(1) = 20
                  imx(2) = 20
                  imx(3) = 20
              else
                  isx(1) = 552
                  isx(2) = isx(1) + ICLDAT(101)
                  isx(3) = isx(2) + ICLDAT(102)
                  imx(1) = ICLDAT(101)
                  imx(2) = ICLDAT(102)
                  imx(3) = ICLDAT(103)
              endif
              do 2118 j=1,3,1
                  chtemp = ' '
                  if (imx(j) .ne. 0) then
                      do 2115 i = 1, imx(j), 1
                         ctape(ilast+i:ilast+i+1) = LCDAT2(isx(j)+i)
                         chtemp(i:i+1) = LCDAT2(isx(j)+i)
 2115                 continue
                  endif
                  if (chtemp(1:1) .eq. ' ') then
                      chtemp(1:1) = '@'
                      ctape(ilast+1:ilast+1) = '@'
                  endif
                  jtemp = strlen1(chtemp)
                  ilast = ilast + jtemp
                  ctape(ilast+1:(ilast+2)) = ", "
                  ilast = ilast + 2
 2118         continue
c
c......P27-36 Various flags
c
              do 2120 i = 1, 10, 1
                 jtemp = ICLDAT(108+i)
                 call itoc(jtemp, csnum, ic, -1)
                 ctape(ilast+1:(ilast+ic)) = csnum(1:ic)
                 ctape((ilast+ic+1):(ilast+ic+3)) = ", "
                 ilast = ilast + ic + 2
 2120         continue
c
c......P37 RAPID
c......P38 MULTAX
c......P39 FEDRAT/MODE
c......P40 SPINDL Direction
c
              do 2130 i = 1, 4, 1
                 jtemp = ICLDAT(120+i)
                 call itoc(jtemp, csnum, ic, -1)
                 ctape(ilast+1:(ilast+ic)) = csnum(1:ic)
                 ctape((ilast+ic+1):(ilast+ic+3)) = ", "
                 ilast = ilast + ic + 2
 2130         continue
c
c......P41     IPM Feedrate
c......P42-P53 TRACUT
c......P54     IPR Feedrate
c......P55     SPINDL/rpm
c
              do 2140 i = 1, 15, 1
                 call rtoc(RCLDAT(31+i), ctape(ilast+1:), ic)
                 ctape((ilast+ic+1):(ilast+ic+3)) = ", "
                 ilast = ilast + ic + 2
 2140         continue
c
c......P56-P65 Cycle flags
c
              do 2150 i = 1, 10, 1
                 jtemp = ICLDAT(184+i)
                 call itoc(jtemp, csnum, ic, -1)
                 ctape(ilast+1:(ilast+ic)) = csnum(1:ic)
                 ctape((ilast+ic+1):(ilast+ic+3)) = ", "
                 ilast = ilast + ic + 2
 2150         continue
c
c......P66-P75 Cycle parameters
c
              do 2160 i = 1, 9, 1
                 call rtoc(RCLDAT(49+i), ctape(ilast+1:), ic)
                 ctape((ilast+ic+1):(ilast+ic+3)) = ", "
                 ilast = ilast + ic + 2
 2160         continue
              call rtoc(RCLDAT(59), ctape(ilast+1:), ic)
              ilast = ilast + ic
c
c......P76-85 CUTTER/DISPLY Part 2 (10)
c
              if (MXCL .gt. 59) then
                  do 2170 i = 1, 10, 1
                     ctape((ilast+1):(ilast+2)) = ", "
                     call rtoc(RCLDAT(59+i), ctape(ilast+3:), ic)
                     ilast = ilast + ic + 2
 2170             continue
              else
                  ctape(ilast+1:ilast+30) =
     1                ',0.,0.,0.,0.,0.,0.,0.,0.,0.,0.'
                  ilast = ilast + 30
              endif
          endif
c
c...UNITS
c
      else if (ITYPE .eq. 7300) then
          call getvwd (841,ctape(ilast+1:),ic,1,PSTWRD,PSTWVL,NPSTWD)
          ilast  = ilast+ic
          ctape(ilast+1:ilast+3) = '/ '
          ilast  = ilast+3
          if (ISUBT.eq.2) then
            call getvwd (301,ctape(ilast:),ic,2,PSTWRD,PSTWVL,NPSTWD)
          else
            call getvwd (303,ctape(ilast:),ic,2,PSTWRD,PSTWVL,NPSTWD)
          endif
          ilast  = ilast + ic
          flag = 1
c
c...Clfile Header
c
      else if (ITYPE .eq. 7400) then
          ist    = 0
          call getvwd (4009,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:ilast+ic+1) = csnum(1:ic) // '/'
          ilast  = ilast + ic + 1
c
c......Clfile name
c
          ilast  = ilast  + 1
          ic     = strlen1(CLNAME)
          ctape(ilast:) = CLNAME(1:ic)//'   '
          ilast  = ilast  + ic + 4
c
c......Date
c
          ic     = strlen1(CLDATE)
          if (ilast-ist+ic+5 .ge. 72) then
              ctape(ilast:) = '~' // newline // '        '
              ist   = ilast + leol + 1
              ilast = ist   + 8
          endif
          ctape(ilast:) = CLDATE(1:ic)//'   '
          ilast  = ilast  + ic + 4
c
c......Time
c
          ic     = strlen1(CLTIME)
          if (ilast-ist+ic+5 .ge. 72) then
              ctape(ilast:) = '~' // newline // '        '
              ist   = ilast + leol + 1
              ilast = ist   + 8
          endif
          ctape(ilast:) = CLTIME(1:ic)//'   '
          ilast  = ilast  + ic + 4
c
c......Program Name
c
          ic     = strlen1(CAMNAM)
          if (ilast-ist+ic+5 .ge. 72) then
              ctape(ilast:) = '~' // newline // '        '
              ist   = ilast + leol + 1
              ilast = ist   + 8
          endif
          ctape(ilast:) = CAMNAM(1:ic)//'   '
          ilast  = ilast  + ic + 4
c
c......Program Version
c
          ic     = strlen1(CAMREV)
          if (ilast-ist+ic+5 .ge. 72) then
              ctape(ilast:) = '~' // newline // '        '
              ist   = ilast + leol + 1
              ilast = ist   + 8
          endif
          call ctor (CAMREV(1:ic),rver,ierr)
          ctape(ilast:) = CAMREV(1:ic)//'   '
          ilast  = ilast  + ic
          flag = 1
c
c...MULTAX
c
      else if (ITYPE .eq. 9000) then
          MULTAX = 1 - ISUBT
          call getvwd (1105,csnum,ic,1,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:) = csnum(1:ic) // '/ '
          ilast  = ilast + ic + 2
          call getvwd (71+ISUBT,csnum,ic,2,PSTWRD,PSTWVL,NPSTWD)
          ctape(ilast+1:) = csnum(1:ic)
          ilast  = ilast + ic
          flag = 1
c
c...FINI
c
      else if (ITYPE .eq. 14000) then
          ctape(ilast+1:ilast+4) = 'FINI'
          ilast  = ilast + 4
          flag = 0
      endif

      if (ilast .le. 8) ilast = 0
      NC = ilast

      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  rtoc88 (gnum,cbuf,knc)
c
c   FUNCTION:  This routine converts a real number to a character
c              string.
c
c   INPUT:  gnum    R*8  D1  -  Real value to be converted to charac-
c                               ter string.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string from 'gnum'.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c***********************************************************************
c
      subroutine rtoc88 (gnum,cbuf,knc)
c
      integer*4 knc
c
      real*8 gnum
c
      character*(*) cbuf
c
      integer*4 nrndex,ien
c
      character*17 snum
      integer*4 i
c
c...Convert integer number to character string
c
      write (snum,10) gnum
   10 format (f17.8)
c
c...Return significant digits only
c
      ien    = nrndex(snum,'0')
      cbuf   = snum(1:ien)
c
c...following spaces
c
      do 100 i = ien+1, 17
          cbuf(i:i) = ' '
  100 continue
      knc = 17

      return
      end

c***********************************************************************
c
c   SUBROUTINE:  add_space(ctape, spos, nc)
c
c   FUNCTION:  This routine added spaces into string
c
c   INPUT:  ctape:  string to add space
c           spos: start position to added space
c           nc:   number of spaces to add
c
c   OUTPUT: ctape
c
c***********************************************************************
c
      subroutine add_space(ctape, spos, nc)
c
      character*(*) ctape
c
      integer*4 spos, nc
c
      integer*4 i

      do 100 i=spos, nc+spos, 1
         ctape(i:i)=' '
  100 continue
      return
      end
