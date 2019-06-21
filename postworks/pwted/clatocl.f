c
c***********************************************************************
c
c   FILE NAME:  clatocl.f
c   CONTAINS:
c           ptd_clatocl (clafile, nc1, clfile, nc2, kerr)
c           clatocl (clafile,nc1, clfile, nc2, err)
c           lodcla (cmsg,kerr)
c           srcbwd (cbuf,knc,cwrd,knwd,kmxwd,krem)
c           srcprs (krec,cdat,knc,kfl,cmsg,kerr)
c           srcpwd (ktyp,krec,kpt,kbuf,gbuf,knw,cmsg,kerr)
c           srcrd (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c           srcppr (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c           srcpar (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c           wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
c           ignwrd (ktyp,kflg)
c           srcppl (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c           chknum (kbuf,knum)
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        clatocl.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ptd_clatocl (clafile, nc1, clfile, nc2, kerr)
c
c   FUNCTION:  This routine read a text clfile
c              and convert to a binary file
c
c   INPUT:  clafile:
c           clfile:
c
c   OUTPUT: kerr
c
c***********************************************************************
c
      subroutine ptd_clatocl (clafile, nc1, clfile, nc2, kerr)
c
      include 'menu.inc'
c
      character*(MAX_PATH) clafile, clfile
c
      integer*4 nc1, nc2, kerr
c
      character*(MAX_PATH) cdat1, cdat2
C
      cdat1 = clafile(1:nc1)
      cdat2 = clfile(1:nc2)
C
      call clatocl (cdat1,nc1, cdat2, nc2, kerr)
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  clatocl (clafile,nc1, clfile, nc2, err)
c
c   FUNCTION:  This routine read a text clfile record
c              and convert to a binary cl file
c
c   INPUT:
c
c   OUTPUT:
c
c***********************************************************************
c
      subroutine clatocl (clafile,nc1, clfile, nc2, err)

      include 'menu.inc'

      character*(*) clafile, clfile
      integer*4 nc1, nc2, err, kerr

      character*80 msg
      character*20 att(4)
      integer*4 irecl
c
c...Open scratch binary clfile
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'UNKNOWN'
      irecl  = 288
      call opnfil (LUNSC3,clfile,att,irecl, msg,kerr)
      if (kerr .ne. 0) go to 8000
      LCMPFI = clfile
      NCCMPF = nc1
c
c...Open Apt Source file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      call opnfil (LUNSC2,clafile,att,irecl,msg,kerr)
      if (kerr .ne. 0) go to 8000

      call lodcla (msg,kerr)
c
c...close files
c
      call clsfil(LUNSC2)
      call clsfil(LUNSC3)

 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodcla (cmsg,kerr)
c
c   FUNCTION:  This routine reads an APT Source file, converts it to a
c              neutral format
c
c   INPUT:  cfnam   C*n  D1  Input Apt Source file name.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodcla (cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
      include 'compile.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END

c
      integer*4 kerr
c
      character*(*) cmsg
c
      equivalence (VECSAV,POSMAP(1372))
      real*8 VECSAV(3)
      equivalence (RCLDAT(1),lclf), (RCLDAT(11),lcld)
      equivalence (RCLDAT(13),lclt), (RCLDAT(14),lclnm)
      equivalence (RCLDAT(15),lclrv)
      character*8 lclt,lclnm,lclrv
      character*11 lcld
      character*800 LCLDAT
      character*1 LCDAT2(800),rmrktx(120)
      character*80 lclf
      integer*4 krec,kpt, mxc

c
      integer*4 inbuf(4)
      integer*2 iobuf(960)
      integer*4 i,ipt,nwds,inc,index,inum,nc,ifl,igo,icont,iseq,ierr,
     1          ist,ien,nci,ityp,ibuf(500),icpt,icr,ifini,irec,
     2          is1,is4,j,mfl(4),nprs, stsize,isx(3),imx(3),strlen1,
     3          nwrd,ncip,chknum,imfl,nugci, expand, startd
c
      real*8 robuf(240),rbuf(500),gts(6),rlstvr,rver,rlabel(3)
c
      character*20 tempst
      character*24 lbuf
      character*80 ldat,msg,msg1,udat,lwrd(5)
      character*200 tobuf
c
      equivalence (robuf,iobuf,tobuf)

      equivalence (rlabel, tempst)
      equivalence (LPSTWD,CPOSMP(0001))
      character*66 LPSTWD
      equivalence (RCLDAT,ICLDAT,JCLDAT,LCLDAT,LCDAT2)
      integer*2 ICLDAT(200)
      integer*4 JCLDAT(200), temp, hasseq
c
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 /0/, is4 /3/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 /3/, is4 /0/
C WNT-DOS-DEC-END
c
c...Initialize statement parsing routines
c
      MAXPRN = 0
      MAXTOK = 100
c
c...LETTER  PPRINT  PARTNO  INSERT  REMARK
c
      call getvwd (1043,LCOMTX(1),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1044,LCOMTX(2),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1045,LCOMTX(3),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (1046,LCOMTX(4),nc,1,PSTWRD,PSTWVL,NPSTWD)
      call getvwd (4009,LCOMTX(5),nc,1,PSTWRD,PSTWVL,NPSTWD)
c
c...Initialze routine
c
      krec = 1
      kpt = 0
      inbuf(2) = 0
      irec   = 0
      ifl    = 0
      iseq   = 0
      ifini  = 0
      icpt   = 0
      icr    = 0
      gts(4) = 0.
      gts(5) = 0.
      gts(6) = 1.0
      MULT   = 0
      mfl(1) = 0
      mfl(2) = 0
      mfl(3) = 0
      mfl(4) = 0
      nprs   = 0
      nwrd   = 0
      ICOMCI = 0
      ncip   = 0
      nugci  = 0
      JMAC   = 0
      imfl   = 0
      expand = 0
      rlstvr = 9.6
      rver   = rlstvr
      do 10 i=1,5
   10 PTL(i) = 0.0d0
      PTL(6) = 1.0d0
      VECSAV(1) = 0.0d0
      VECSAV(2) = 0.0d0
      VECSAV(3) = 1.0d0
c
c...Read source file record
c
  100 kerr   = 0
      nprs   = nprs   + 1
      call srcprs (irec,ldat,nc,ifl,cmsg,kerr)
      if (kerr.eq.2) goto 3000

      if (kerr .ne. 0 .and. NTOK .eq. 0) go to 9000
  150 inbuf(2) = irec
c
c...Initialize per record flags
c
      ifl    = 0
      ipt    = 1
      igo    = 0
      icont  = 5
c
c...Nxx sequence number
c
c...see if this record has sequence number
c
      hasseq = 0
      if (ICTYP(ipt) .eq. 4) then
         ist    = RCSUB(ipt)
         ien    = ICNC(ipt)
         if ((LCTXT(ist:ist) .eq. 'N' .or. LCTXT(ist:ist) .eq. 'C')
     1        .and. ist .lt. ien) then
             if (LICOPT(7) .eq. 1) go to 9600
             call ctoi (LCTXT(ist+1:ien),inum,ierr)
             if (ierr .eq. 0) then
                inbuf(1) = inum
                if (LCTXT(ist:ist) .eq. 'C') icont = 6
                ipt    = ipt    + 1
                if (ipt .ge. NTOK) go to 9000
                if (ICTYP(ipt) .ne. 2 .or. RCSUB(ipt) .ne. 2)
     1                    go to 9000
                ipt    = ipt    + 1
                iseq   = 1
             endif
         endif
      endif
c
c...PARTNO, PPRINT, INSERT, LETTER
c
      if (NTOK .ge. ipt .and. ICTYP(ipt) .eq. 1 .and.
     1    ((RCSUB(ipt) .ge. 1043. .and. RCSUB(ipt) .le. 1046.) .or.
     2    RCSUB(ipt) .eq. 4009)) then
c
c......Break out text
c
  160    kerr   = 0
         inum   = RCSUB(ipt)
         call touppr (ldat(1:nc),udat)
         call getvwd (inum,lbuf,nci,1,PSTWRD,PSTWVL,NPSTWD)
         inc    = index(udat,lbuf(1:nci))
         if (inc .eq. 0) go to 9000
         tobuf  = ldat(inc+nci+1:nc)
         if (inum.eq.4009) then
            rmrktx(1) = ldat(inc+nci+1:nc)
         endif
         nci    = nc     - (inc+nci) + 1
c
c......Text is continued onto next line
c
  180    if (ldat(nc:nc) .eq. '~') then
             call srcprs (irec,ldat,nc,ifl,cmsg,kerr)
             if (kerr .lt. 0) go to 9000
             if (kerr .ne. 2) then
                tobuf(nci-1:nci+nc-1) = ldat(1:nc)
                nci = nci+nc
             endif
             if (ldat(nc:nc) .eq. '~') goto 180
         endif
c
c......REMARK Clfile Header Record
c
         if ((RCSUB(ipt) .eq. 4009) .or. (inum .eq. 4009)) then
             if (nprs .eq. 1 .or. (nprs .eq. 2 .and.
     1           nwrd .ne. 0 .and. nwrd .lt. 5)) then
                 call srcbwd (tobuf,nci,lwrd(nwrd+1),inc,5,1)
                 nwrd   = nwrd   + inc
                 if (nwrd .ge. 1) lclf = lwrd(1)
                 if (nwrd .ge. 2) lcld = lwrd(2)
                 if (nwrd .ge. 3) lclt = lwrd(3)
                 if (nwrd .ge. 4) lclnm = lwrd(4)
                 if (nwrd .ge. 5) lclrv = lwrd(5)
                 call ctor (lclrv,rver,ierr)
                 if (ierr .ne. 0) rver = rlstvr
             endif
             inbuf(3) = 7400
             inbuf(4) = inum
             nwds = 16
             call nclwr (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
             go to 100
c
c......Store text record
c
         else
             inbuf(3) = 2000
             inbuf(4) = inum
             LCLDAT(1:66) = tobuf(1:66)
             nwds    = 9
             kerr   = 0
             call nclwr (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
             go to 100
         endif
      endif
c
c...Next token should be a Post word
c
  200 if (kerr .ne. 0 .or. ICTYP(ipt) .ne. 1) go to 9000
      ityp   = RCSUB(ipt)
      ipt    = ipt    + 1
      nwds   = 0
      if (ipt .le. NTOK) then
         if (ICTYP(ipt) .ne. 2 .or. RCSUB(ipt) .ne. 7) go to 9000
         if (ipt .lt. NTOK) then
            ipt    = ipt    + 1
            call srcpwd (ityp,irec,ipt,ibuf,rbuf,nwds,cmsg,kerr)
            if (kerr .ne. 0) go to 9000
          endif
      endif
c
c...ISN
c
      if (ityp .eq. 4023) then
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
         do 210 i=1,nwds,1
             JCLDAT(i) = rbuf(i)
  210    continue
         inbuf(3) = 1000
         inbuf(4) = 0
         nwds = (nwds+1) / 2
c
c...CUTTER
c
      else if (ityp .eq. 4025) then
c
c......CUTTER/DISPLY
c
         if ((ICTYP(ipt).eq.1) .and. (RCSUB(ipt).eq.1021)) then
             inbuf(3) = 7100
c
c.........CUTTER/DISPLY,PART
c
             if (ICTYP(ipt+2) .eq. 1 .and. RCSUB(ipt+2) .eq. 260) then
                 nwds = 1
                 inbuf(4) = 3
                 ICLDAT(4) = 0
c
c.........CUTTER/DISPLY,ALL
c
             else if (ICTYP(ipt+2) .eq. 1 .and. RCSUB(ipt+2) .eq. 51)
     1               then
                 nwds = 1
                 inbuf(4) = 3
                 ICLDAT(4) = 1
c
c.........CUTTER/DISPLY,SHANK-HOLDER
c
             else if (ICTYP(ipt+2) .eq. 1 .and.
     1           (RCSUB(ipt+2) .eq. 192 .or. RCSUB(ipt+2) .eq. 157))
     2               then
                 ipt    = ipt    + 2
                 if (rver .lt. 9.550) then
                     do 230 i=4,nwds,1
                         if (ibuf(i) .eq. 4025) then
                             RCLDAT(i-3) = 716 - 10000
                         else if (ibuf(i) .eq. 157) then
                             RCLDAT(i-3) = 157 - 10000
                         else if (ibuf(i) .eq. 0) then
                             RCLDAT(i-3) = rbuf(i)
                         else
                             go to 9000
                         endif
  230                continue
                     nwds = nwds - 3
                     inbuf(4) = 4
                 else if (rver .lt. 9.650) then
                     ICLDAT(11) = RCSUB(ipt+2)
                     ICLDAT(12) = 1
                     if (RCSUB(ipt) .eq. 157) ICLDAT(12) = 2
                     if (ICLDAT(11) .le. 1) then
                         LCLDAT(1:20) = ' '
                         ist = 4
                     else
                         ist = rbuf(4)
                         ien = ibuf(4)
                         LCLDAT(1:20) = LCTXT(ist:ien)
                         ist = 5
                     endif
                     inc = 3
                     do 235 i=ist,nwds,1
                         if (ibuf(i) .eq. 4025 .and. ICLDAT(12) .ne. 2)
     1                           then
                             ICLDAT(12) = 0
                         else if (ibuf(i) .eq. 157 .and.
     1                            ICLDAT(12) .ne. 2) then
                             ICLDAT(12) = 1
                         else if (ibuf(i) .eq. 0) then
                             inc = inc + 1
                             RCLDAT(inc) = rbuf(i)
                         else
                             go to 9000
                         endif
  235                continue
                     nwds = inc
                     inbuf(4) = 6
                 else
                     ICLDAT(2) = RCSUB(ipt+2)
                     ICLDAT(3) = 1
                     if (RCSUB(ipt) .eq. 157) ICLDAT(3) = 2
                     if (ICLDAT(2) .le. 1) then
                         ICLDAT(1) = 0
                         ist = 4
                     else
                         ist = rbuf(4)
                         ien = ibuf(4)
                         imx(1) = ien - ist + 1
                         ICLDAT(1) = imx(1)
                         LCLDAT(41:40+imx(1)) = LCTXT(ist:ien)
                         ist = 5
                     endif
                     inc = 1
                     do 236 i=ist,nwds,1
                         if (ibuf(i) .eq. 4025 .and. ICLDAT(3) .ne. 2)
     1                           then
                             ICLDAT(3) = 0
                         else if (ibuf(i) .eq. 157 .and.
     1                            ICLDAT(3) .ne. 2) then
                             ICLDAT(3) = 1
                         else if (ibuf(i) .eq. 0) then
                             inc = inc + 1
                             RCLDAT(inc) = rbuf(i)
                         else
                             go to 9000
                         endif
  236                continue
                     nwds = 5 + (imx(1)+7) / 8
                     inbuf(4) = 8
                 endif
c
c.........CUTTER/DISPLY,symbol
c
             else if (ICTYP(ipt+2) .eq. 4) then
                 ist = rbuf(2)
                 ien = ibuf(2)
                 if (chknum(ibuf(3),nwds-2) .ne. 0) go to 9000
                 ipt = ipt + 2
                 if (rver .lt. 9.550) then
                     LCLDAT(1:20) = LCTXT(ist:ien)
                     ICLDAT(16) = rbuf(3)
                     do 260 i = 1, 4, 1
                         if (i .lt. nwds-2) then
                             RCLDAT(i+4) = rbuf(i+3)
                         else
                             RCLDAT(i+4) = 0.
                         endif
  260                continue
                     inbuf(4) = 2
                     nwds   = 8
                 else if (rver .lt. 9.650) then
                     LCLDAT(1:20) = LCTXT(ist:ien)
                     ICLDAT(11) = rbuf(3)
                     do 265 i = 1, 2, 1
                         if (i .lt. nwds-2) then
                             RCLDAT(i+3) = rbuf(i+3)
                         else
                             RCLDAT(i+3) = 0.
                         endif
  265                continue
                     inbuf(4) = 5
                     nwds   = 5
                 else
                     imx(1) = ien - ist + 1
                     LCLDAT(25:24+imx(1)) = LCTXT(ist:ien)
                     ICLDAT(1) = imx(1)
                     ICLDAT(2) = rbuf(3)
                     do 266 i = 1, 2, 1
                         if (i .lt. nwds-2) then
                             RCLDAT(i+1) = rbuf(i+3)
                         else
                             RCLDAT(i+1) = 0.
                         endif
  266                continue
                     inbuf(4) = 7
                     nwds = 3 + (imx(1)+7) / 8
                 endif
c
c.........CUTTER/DISPLY,parameters
c
            else
                nwds = nwds - 1
                if (chknum(ibuf(2),nwds) .ne. 0) go to 9000
                call copyn (rbuf(2),RCLDAT,nwds)
                inbuf(4) = 1
            endif
c
c......CUTTER/parameters
c
         else
            if (ibuf(1) .eq. 700) then
                rbuf(1) = ibuf(1)-10000
                ibuf(1) = 0
            endif
            if (chknum(ibuf,nwds) .ne. 0) go to 9000
            call copyn (rbuf,RCLDAT,nwds)
            inbuf(3) = 6000
            inbuf(4) = 6
         endif
c
c...PROFIL
c
      else if (ityp .eq. 761) then
          ist = rbuf(1)
          ien = ibuf(1)
          if (chknum(ibuf(2),nwds-1) .ne. 0) go to 9000
          inbuf(4) = rbuf(2)
          if (rver .lt. 9.650) then
              LCLDAT(1:20) = LCTXT(ist:ien)
              inbuf(3) = 7110
              JCLDAT(6) = rbuf(3)
              ICLDAT(13) = rbuf(4)
              ist = 0
          else
              imx(1) = ien - ist + 1
              ICLDAT(1) = imx(1)
              LCLDAT(9:8+imx(1)) = LCTXT(ist:ien)
              inbuf(3) = 7120
              JCLDAT(2) = rbuf(3)
              ICLDAT(2) = rbuf(4)
              ist = 2 + (imx(1)+7) / 8 - 5
          endif
          do 275 i=5,nwds,1
              RCLDAT(i+ist) = rbuf(i)
  275     continue
c
c...CIRCLE
c
      else if (ityp .eq. 4026) then
         if (LICOPT(7) .eq. 1 .and.
     -       (ityp .eq. 4026)) go to 9600
         mxc    = 7
         if (nwds .ne. mxc) go to 9000
         if (chknum(ibuf,nwds) .ne. 0) go to 9000
         call copyn (rbuf,RCLDAT,nwds)
         inbuf(3) = 3000
         inbuf(4) = 5
c
c...GOTO
c
      else if (ityp .eq. 4013 .or. ityp .eq. 4019) then
         if (ibuf(1).eq.0) then
c
c...should be 5000 or 5200 type
c
             if (ityp .eq. 4019) icont = 3
             mxc    = 0
             do 300 i=1,nwds
                 RCLDAT(i) = rbuf(i)
  300        continue
c
c......Check for another point
c......in this GOTO record
c
  350        mxc = mxc + nwds
             call srcprs (irec,ldat,nc,ifl,cmsg,kerr)
             if (kerr .ne. 0 .and. NTOK .eq. 0) go to 9000
             if ((ICTYP(1) .eq. 1) .and. (RCSUB(1) .eq. 4013)) then
                 nwds   = 0
                 call srcpwd (ityp,irec,3,ibuf,rbuf,nwds,cmsg,kerr)
                 if (kerr .ne. 0) go to 9000
                 do 400 i=1,nwds
                     RCLDAT(mxc+i) = rbuf(i)
  400            continue
                 goto 350
             else
c
c...write this record and parse again
c
                 inbuf(3) = 5000
cc                 if (expand .eq. 1) inbuf(3) = 5200
                 inbuf(4) = icont
                 call nclwr (krec,kpt,inbuf,RCLDAT,mxc,cmsg,kerr)
                 mxc = 0
                 goto 150
             endif
             inbuf(3) = 5000
             inbuf(4) = icont
         else
c
c..."START","LARGE", "END"
c
             i = 0
             j = 0
             temp = nwds
  572        j = j + 1
             i = i + 1
             if (j.gt.temp) goto 577
             if (ibuf(j).ne.0) then
c
c...ignore the minor word
c...remember the type
c
                 j = j + 1
                 RCLDAT(i) = rbuf(j)
                 nwds = nwds - 1
             else
                 RCLDAT(i) = rbuf(j)
             endif
             goto 572
  577        if (ibuf(1).eq.57) then
c
c...START
c
                 inbuf(3) = 5210
                 expand = 1
             else if (ibuf(1).eq.1) then
c
c...END
c
                 inbuf(3) = 5220
                 expand = 0
             else if (ibuf(1).eq.7) then
c
c...LARGE
c
                 inbuf(3) = 5200
                 inbuf(4) = icont
             endif
         endif
c
c...UNITS
c
      else if (ityp .eq. 841) then
         if (nwds .ne. 1) go to 9000
         inbuf(3) = 7300
        if (ibuf(1) .eq. 301) then
            inbuf(4) = 2
        else if (ibuf(1) .eq. 303) then
            inbuf(4) = 1
        else
            go to 9000
        endif
        nwds = nwds - 1
c
c...MULTAX
c
      else if (ityp .eq. 1105) then
         if (nwds .gt. 1) go to 9000
         inbuf(3) = 9000
         if (nwds .eq. 1) then
            if (ibuf(1) .eq. 71) then
                inbuf(4) = 1
            else if (ibuf(1) .eq. 72) then
                inbuf(4) = 0
            else
                go to 9000
            endif
            nwds = nwds - 1
         else
            inbuf(4) = 1
         endif
c
c...FINI
c
      else if (ityp .eq. 4012) then
         ifini  = 1
         inbuf(3) = 14000
         nwds   = 0
c
c...TRACUT
c
      else if (ityp .eq. 1038) then
         inbuf(3) = 7000
         inbuf(4) = 1
c
c......TRACUT/NOMORE
c
         if (ibuf(1) .eq. 53) then
             nwds   = 0
             inbuf(4) = 2
c
c......TRACUT/mx
c
         else
             call copyn (rbuf(1),RCLDAT(1),12)
         endif
c
c...SEQUNC
c
      else if (ityp .eq. 818) then
         inbuf(3) = 7200
         inbuf(4) = 1
c
c......SEQUNC/END
c
         if (ibuf(1) .eq. 1) then
             nwds   = 0
             inbuf(4) = 2
         else
c
c......Sequence number / label
c
             startd = 0
             nwds = 77
             if (rver .lt. 9.549) nwds = 69
             ien = 0
             do 700 i = 1, 80, 1
                if (startd .eq. 0) then
                   ist = i
                   if (ldat(ist:ist) .eq. '/') startd = 1
                else
                   if (ldat(i:i) .eq. ' ' .and. ien .eq. 0) then
                       ist = i
                   else if (ldat(i:i) .ne. ' ') then
                       ien = i
                       if (ldat(ien:ien) .eq. ',') go to 710
                   endif
                endif
  700        continue
  710        if (i .lt. 80) then
                tempst(1:20) = '                    '
                ist = ist + 1
                ien = ien - 1
                stsize = ien - ist + 1
                LCLDAT(1:20) = ldat(ist:ien)
             endif
c
c......FROM(6)
c......CUTTER(6)
c......CUTTER/DISPLY(6)
c
             call copyn (rbuf(2),RCLDAT(4),22)
c
c......CUTTER/DISPLY,symbol
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
                 ICLDAT(101) = ibuf(24) - rbuf(24) + 1
                 ICLDAT(102) = ibuf(25) - rbuf(25) + 1
                 ICLDAT(103) = ibuf(26) - rbuf(26) + 1
                 isx(1) = 552
                 isx(2) = isx(1) + ICLDAT(101)
                 isx(3) = isx(2) + ICLDAT(102)
                 imx(1) = ICLDAT(101)
                 imx(2) = ICLDAT(102)
                 imx(3) = ICLDAT(103)
             endif
             do 718 j=1,3,1
                 if (isx(j) .ne. 0) then
                     ist = rbuf(23+j) - 1
                     do 715 i=1,imx(j),1
                        if (ist+i .le. ibuf(23+j) .and.
     1                      LCTXT(ist+i:ist+i) .ne. '@') then
                           LCDAT2(isx(j)+i) = LCTXT(ist+i:ist+i)
                        else
                           LCDAT2(isx(j)+i) = ' '
                        endif
  715                continue
                 endif
  718        continue
c
c......Various flags
c
             do 720 i = 1, 10, 1
                ICLDAT(108+i) = rbuf(26+i)
  720        continue
c
c......RAPID
c......MULTAX
c......FEDRAT/MODE
c......SPINDL/DIR
c
             do 730 i = 1, 4, 1
                ICLDAT(120+i) = rbuf(36+i)
  730        continue
c
c......FEDRAT/IPM
c......TRACUT(12)
c......FEDRAT/IPR
c......SPINDL/RPM
c
             call copyn (rbuf(41),RCLDAT(32),15)
c
c......CYCLE flags(10)
c
             do 740 i = 1, 10, 1
                ICLDAT(184+i) = rbuf(55+i)
  740        continue
c
c......CYCLE values(10)
c
             call copyn (rbuf(66),RCLDAT(50),10)
c
c......CUTTER display values Part 2 (10)
c
             call copyn (rbuf(76),RCLDAT(60),10)
          endif
c
c...STOCK/FIXTUR
c
      else if (ityp .eq. 321 .or. ityp .eq. 898) then
         inbuf(3) = 2600
         inbuf(4) = ibuf(1)
         if (ityp .eq. 898) inbuf(3) = 2601
c
c......STOCK/BOX-CYLNDR
c
         if (ibuf(1) .eq. 340 .or. ibuf(1) .eq. 4042 .or.
     1       ibuf(1) .eq. 632 .or. ibuf(1) .eq. 631 .or.
     2       ibuf(1) .eq. 627) then
             if (ibuf(1) .eq. 340) then
                 if (nwds .ne. 9) go to 9000
                 nwds = 7
             else if (ibuf(1) .eq. 4042) then
                 if (nwds .ne. 11) go to 9000
                 nwds = 9
             else if (ibuf(1) .eq. 632) then
                 if (nwds .ne. 12) go to 9000
                 nwds = 10
             else if (ibuf(1) .eq. 631) then
                 if (nwds .ne. 7) go to 9000
                 nwds = 5
             else if (ibuf(1) .eq. 627) then
                 if (nwds .ne. 11) go to 9000
                 nwds = 9
             endif
             JCLDAT(1) = rbuf(2)
             JCLDAT(2) = rbuf(3)
             do 800 i=1,nwds-1,1
                 RCLDAT(i+1) = rbuf(i+3)
  800        continue
c
c......STOCK/LOAD
c
         else if (ibuf(1) .eq. 1075) then
             JCLDAT(1) = rbuf(2)
             ist    = RCSUB(NTOK)
             ien    = ICNC(NTOK)
             nc     = ien - ist + 1
             if (nc .le. 0) go to 9000
             LCLDAT(9:8+nc) = LCTXT(ist:ien)
             JCLDAT(2) = nc
             nwds = (nc+7) / 8 + 1
c
c......STOCK/STL
c
         else if (ibuf(1) .eq. 330) then
             JCLDAT(1) = rbuf(2)
             if (ibuf(3) .eq. 303) then
                 JCLDAT(3) = 0
             else if (ibuf(3) .eq. 301) then
                 JCLDAT(3) = 1
             else
                 go to 9000
             endif
             ist    = RCSUB(NTOK)
             ien    = ICNC(NTOK)
             nc     = ien - ist + 1
             if (nc .le. 0) go to 9000
             LCLDAT(17:16+nc) = LCTXT(ist:ien)
             JCLDAT(2) = nc
             nwds = (nc+7) / 8 + 2
c
c......STOCK/CLONE
c
         else if (ibuf(1) .eq. 576) then
             JCLDAT(1) = rbuf(2)
             JCLDAT(2) = rbuf(3)
             JCLDAT(3) = rbuf(4)
             nwds = 2
c
c......STOCK/MOVE
c
         else if (ibuf(1) .eq. 577) then
             do 810 i=1,12,1
                 RCLDAT(i+1) = rbuf(i+1)
  810        continue
             do 815 i=1,NTOK,1
                 if (ICTYP(i) .eq. 4) then
                     ist = RCSUB(i)
                     ien = ICNC(i)
                 endif
  815        continue
             LCLDAT(105:112) = LCTXT(ist:ien)
             nc = nwds - 14
             if (nc .le. 0) go to 9000
             JCLDAT(1) = nc
             do 820 i=1,nc,1
                 JCLDAT(i+28) = rbuf(i+14)
  820        continue
             nwds = (nc+1) / 2 + 14
c
c......STOCK/REMOVE
c
         else if (ibuf(1) .eq. 843) then
             if (nwds .le. 1) go to 9000
c
c.........STOCK/REMOVE,CHIPS
c
             if (ibuf(2) .eq. 331) then
                 inbuf(4) = ibuf(2)
                 do 825 i=1,nwds+2,1
                     RCLDAT(i) = rbuf(i+2)
  825            continue
                 nwds   = nwds   - 2
c
c.........STOCK/REMOVE,ids
c
             else
                 JCLDAT(1) = nwds - 1
                 do 830 i=1,nwds-1,1
                     JCLDAT(i+2) = rbuf(i+1)
  830            continue
                 nwds = nwds / 2 + 1
             endif
c
c......STOCK/MODIFY
c
         else if (ibuf(1) .eq. 55) then
             inbuf(4) = 732
             JCLDAT(2) = rbuf(2)
             JCLDAT(3) = rbuf(3)
             JCLDAT(4) = rbuf(4)
             JCLDAT(5) = rbuf(5)
             RCLDAT(4) = rbuf(6)
             nc = nwds - 6
             if (nc .le. 0) go to 9000
             JCLDAT(1) = nc
             do 840 i=1,nc,1
                 JCLDAT(i+8) = rbuf(i+6)
  840        continue
             nwds = (nc+1) / 2 + 4
         else
             go to 9000
         endif
c
c...DEFAULT -> Post Word
c
      else
         inbuf(3) = 2000
         inbuf(4) = ityp
         if (nwds .ne. 0) then
             do 1015 i=1,nwds,1
                if (ibuf(i) .eq. 0) then
                    RCLDAT(i) = rbuf(i)
                else
                    ICLDAT(i*4-is4) = 0
                    ICLDAT(i*4-is1) = ibuf(i)
                endif
 1015        continue
         endif
c
c......
      endif
 2000 call nclwr (krec,kpt,inbuf,RCLDAT,nwds,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
      if (ifini .eq. 0) go to 100
 3000 call wrnclend(krec)
c
c...End of routine
c
 8000 return
c
c...Invalid Apt Source record
c
 9000 if (kerr .lt. 0) go to 8000
      i      = 0
      kerr   = 2
      call errtxt ('INVAPTSC',msg)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C9010 call trmmsg (' ')
C      if (i .eq. 1) call trmmsg (msg1)
C      if (kerr .gt. 1 .or. kerr .eq. 0) call trmmsg (msg)
C      call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 9010 flag  =1
      if (i .eq. 1) then
          nc = strlen1(msg1)
          call add1dispmsg(msg1, nc, flag)
      endif
      if (kerr .gt. 1 .or. kerr .eq. 0) then
          nc = strlen1(msg)
          call add1dispmsg(msg, nc, flag)
      endif
      nc = strlen1 (ldat)
      call add1dispmsg(ldat, nc, flag)
C WNT-END
      kerr   = 0
      go to 100
c
c...Errors in APT circular sequence
c
 9600 kerr   = 10
      go to 9000
c
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcbwd (cbuf,knc,cwrd,knwd,kmxwd,krem)
c
c   FUNCTION:  This routine breaks out 'words' from a text string.  A
c              word is separated by spaces in the string.
c
c   INPUT:  cbuf    C*n  D1  Text string which contains words.
c
c           knc     I*4  D1  Number of chars in 'cbuf'.
c
c           kmxwd   I*4  D1  Maximum number of words to break out of
c                            'cbuf'.
c
c           krem    I*4  D1  1 = Parsing a REMARK statement (at least
c                            2 spaces between words due to spaces in
c                            filename). 0 = Parsing any other string.
c
c   OUTPUT: cwrd    C*n  Dn  Array of text strings to receive words.
c
c           knwd    I*4  D1  Number of words found.
c
c***********************************************************************
c
      subroutine srcbwd (cbuf,knc,cwrd,knwd,kmxwd,krem)
c
      integer*4 knc,knwd,kmxwd,krem
c
      character*(*) cbuf,cwrd(5)
c
      integer*4 ist,inc,inc1,ien,nindex
c
c...Find next word in text string
c
      knwd   = 0
      ist    = 1
  100 inc    = nindex(cbuf(ist:knc),' ')
      if (inc .eq. 0) go to 8000
      inc    = ist    + inc     - 1
      if (krem .eq. 1) then
          inc1   = index(cbuf(inc:knc),'  ')
      else
          inc1   = index(cbuf(inc:knc),' ')
      endif
      ien    = inc    + inc1   - 2
      if (ien .lt. inc) ien = knc
      if (cbuf(inc:ien).ne.'/') then
          knwd   = knwd   + 1
          cwrd(knwd) = cbuf(inc:ien)
      endif
      ist    = ien    + 1
      if (ist .le. knc .and. knwd .lt. kmxwd) go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcprs (krec,cdat,knc,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from an APT Source file, and
c              performs the initial parsing.
c
c   INPUT:  kfl     I*4  D1  1 = Do not read record, only parse it.
c                            'cdat' and 'knc' will be furnished by the
c                            calling routine.
c
c   OUTPUT: krec    I*4  D1  Record number which was read.
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
      subroutine srcprs (krec,cdat,knc,kfl,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 krec,knc,kerr,kfl
c
      character*(*) cdat,cmsg
      character*80 dbug
c
      integer*4 strlen1,ityp,is1,ie1,l1,it,skn,ier1,lcom,jindex
      character*6 lrem
c
c...Record does not need to be read
c...parser data only
c
      ITXTPT = 1
  100 if (kfl .eq. 1)  go to 200
c
c......read a line from buffer
c
      if (PTD_RDBUF.eq.1) then
          if (krec.gt.BUFEND) goto 8000
          call ptd_getstr(kfl, krec, cdat,knc)
          krec   = krec   + 1
          if (knc .eq. 0) go to 100
          goto 200
      endif
      call rdtxt (LUNSC2,cdat,cmsg,kerr)
      if (kerr .eq. 1) then
          kerr   = 2
          go to 8000
      endif
      if (kerr .ne. 0) go to 8000
      knc    = strlen1(cdat)
      krec   = krec   + 1
      if (knc .eq. 0) go to 100
c
c...we don't need set max for string
c
cctemp      if (knc .gt. MCNSRC) knc = MCNSRC
c
c...Parse record
c
  200 skn    = knc
      call stpars (cdat,knc,2,cmsg,kerr,lcom)
c
c...See if $$ CIRCLE but make sure it is not comment
c...just with circle word
c
      if (lcom .eq. 1 .and. NTOK .eq. 0) then
         if (cdat(knc+1:knc+1) .eq. '%') then
           cdat(knc+1:knc+1) = ' '
         else
           cdat(knc+1:knc+2) = ' '
         endif
         call stpars (cdat,skn,2,dbug,ier1,lcom)
         if (ier1 .ne. 0) go to 100
         if (NTOK .eq. 0) go to 100
         ier1 = RCSUB(1)
         if (ICOMCI .eq. 2 .or. ICTYP(1) .eq. 1 .and.
     -      (ier1 .eq. 4042 .or. ier1 .eq. 4026)) then
            if (jindex(ICTYP,4,NTOK-1) .ne. 0) go to 100
            if (ICOMCI .eq. 1 .and.
     -         (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 7)) go to 100
            ICOMCI = 1
            if (ICTYP(NTOK) .eq. 4) then
               is1    = RCSUB(NTOK)
               if (LCTXT(is1:ICNC(NTOK)) .eq. '$') ICOMCI = 2
            end if
            knc    = skn
         else
            go to 100
         end if
      end if
c
      if (kerr .ne. 0) go to 8000
      if (NTOK .eq. 0) go to 100
      if (ICTYP(1) .eq. 4) then
         if ((LCTXT(1:1) .eq. 'N' .or. LCTXT(1:1) .eq. 'C')
     1                            .and. ICNC(1) .gt. 0) then
            it     = 2
            if (ICTYP(it) .eq. 2 .and. NTOK .gt. 2) it = 3
         else
            it     = 1
         end if
c
c... if LETTER  PPRINT  PARTNO  INSERT
c
         if (ICTYP(it) .eq. 4) then
            is1    = RCSUB(it)
            l1     = is1 + 6
            ie1    = ITXTPT
            if (ie1-is1 .ge. 5) then
              call touppr (LCTXT(is1:is1+5),lrem)
              if (LCTXT(l1:l1) .ne. '/' .and.
     1            (lrem .eq. LCOMTX(1) .or. lrem .eq. LCOMTX(2) .or.
     2             lrem .eq. LCOMTX(3) .or. lrem .eq. LCOMTX(4) .or.
     3             lrem .eq. LCOMTX(5))) then
                  call getvnm (lrem,ityp,PSTWRD,PSTWVL,NPSTWD)
                  RCSUB(it) = ityp
                  ICTYP(it) = 1
              endif
            endif
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
c   SUBROUTINE:  srcpwd (ktyp,krec,kpt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  This routine validates and parses an APT Source file
c              record.
c
c   INPUT:  ktyp    I*4  D1  Major word value of post-processor command
c                            being parsed.
c
c           kpt     I*4  D1  Token within global arrays at which to
c                            start parsing.
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           kbuf    I*4  D50 Array to receive minor word values of
c                            parameters in the commmand.
c
c           gbuf    R*8  D50 Array to receive real values of parameters
c                            in the command.
c
c           knw     I*4  D1  Number of parameters in command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred in the
c                            input command syntax.
c
c***********************************************************************
c
      subroutine srcpwd (ktyp,krec,kpt,kbuf,gbuf,knw,cmsg,kerr)
c
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 ktyp,kpt,kbuf(500),knw,kerr,krec
      character*(*) cmsg
c
      real*8 gbuf(500)
c
      integer*4 i,j,ist,ien,nc,ipt,ival(2)
      integer*4 pinc
c
      real*8 rtxt(5),rval
c
      character*40 ltxt
      character*512 tempst
      character*80 ldat
c
      equivalence (rtxt,ltxt), (rval,ival)
c
c...Check for correct syntax
c
      knw    = 0
      ipt    = kpt
      pinc   = 0
  100 do 500 i=ipt,NTOK,2
         knw    = knw    + 1
c
c......Minor word
c
         if (ICTYP(i) .eq. 1) then
             kbuf(knw) = RCSUB(i)
c
c......Real value
c
         else if (ICTYP(i) .eq. 3) then
             kbuf(knw) = 0
             gbuf(knw) = RCSUB(i)
c
c......MACHIN/name
c
         else if (i .eq. ipt .and. ICTYP(i) .eq. 4 .and.
     1            (ICNC(i)-RCSUB(i)) .le. 40 .and. ktyp .eq. 1015) then
             ist    = RCSUB(i)
             ien    = ICNC(i)
             call touppr (LCTXT(ist:ien),ltxt)
             kbuf(knw) = 0
             if (ien-ist+1 .le. 8) then
                 gbuf(knw) = rtxt(1)
             else
                 ival(1) = -1
                 ival(2) = ien - ist + 1
                 gbuf(knw) = rval
                 do 200 j=1,(ival(2)+7)/8,1
                     knw    = knw    + 1
                     kbuf(knw) = 0
                     gbuf(knw) = rtxt(j)
  200            continue
             endif
c
c......text string
c
         else if (ICTYP(i) .eq. 4 .or. ICTYP(i) .eq. 7) then
c
c......$ - Continuation character
c
            ist    = RCSUB(i)
            ien    = ICNC(i)
            if (ICTYP(i) .eq. 7) then
                tempst(ist:ien) = LCTXT(ist:ien)
            else
                if (LCTXT(ist:ien) .eq. '$') go to 600
                call touppr (LCTXT(ist:ien),tempst(ist:ien))
            endif
            gbuf(knw) = ist
            kbuf(knw) = ien
            pinc   = kbuf(knw)
c
c......Unrecognized parameter
c
         else
             go to 9000
         endif
c
c...Next delimeter should be comma
c
         if (i .lt. NTOK .and. (ICTYP(i+1) .ne. 2 .or.
     1        RCSUB(i+1) .ne. 2)) go to 9000
  500 continue
      go to 8000
c
c...Get continuation record
c
  600 call srcprs (krec,ldat,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      ipt    = 1
      knw    = knw    - 1
      go to 100
c
c...End of routine
c
 8000 continue
      if (pinc .ne. 0) LCTXT(1:pinc) = tempst(1:pinc)
      return
c
c...Invalid syntax
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcrd (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c
c   FUNCTION:  This routine reads an APT Source record and converts it
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
      subroutine srcrd (krec,kpt,kbuf,gbuf,kmxcl,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      equivalence (ICLREC,KPOSMP(0002))
c
      integer*4 ICLREC
c
      integer*4 krec, kpt, kbuf(4),kmxcl,kerr
C WNT-START
      integer*4 krec1,kpt1
C WNT-END
c
      real*8 gbuf(10)
c
      character*(*) cmsg
c
      integer*2 ibuf(4)
      integer*4 i,ipt,nwds
c
      real*8 rbuf
c
      equivalence (rbuf,ibuf)
c
c...Initialze routine
c
      kerr   = 0
      ICLREC = ICLREC + 1
c
c...In winNT, parameter is not pointer, they are not allowwed changed
c...it will have errors without error messsge (the result will wrong)
c...changed by Yurong
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C... SYSTEM OTHER THAN
c
c...Read cl record if necessary
c

C      kpt   = kpt    + 1
C      if ((kpt .gt. 64) .or. (krec .ne. LSTIRC)) then
C         if (kpt .gt. 64) then
C             krec   = krec   + 1
C             kpt    = 1
C         endif
C        call rdprm (LUNSC1,krec,JCLBUF,cmsg,kerr)
C         if (kerr .ne. 0) go to 8000
C      endif
C      ipt    = (kpt*4) - 3
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END

C WNT-START
      kpt1 = kpt + 1
      krec1 = krec
      if ((kpt1 .gt. 64) .or. (krec .ne. LSTIRC)) then
         if (kpt1 .gt. 64) then
             krec1   = krec1   + 1
             kpt1    = 1
         endif
         call rdprm (LUNSC1,krec1,JCLBUF,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
      endif
      ipt    = (kpt1*4) - 3
C WNT-END
c
c...Store integer values
c

      kbuf(1) = ICLBUF(ipt+1)
      kbuf(2) = ICLREC
      kbuf(3) = ICLBUF(ipt+2)
      kbuf(4) = ICLBUF(ipt+3)
      nwds    = ICLBUF(ipt)
c
c...Store real values
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C... SYSTEM OTHER THAN WINNT

C      do 100 i=1,nwds,1
c
C         kpt    = kpt    + 1
C         if (kpt .gt. 64) then
C             krec   = krec   + 1
C             kpt    = 1
C             call rdprm (LUNSC1,krec,JCLBUF,cmsg,kerr)
C             if (kerr .ne. 0) go to 8000
C         endif
c
C         gbuf(i) = RCLBUF(kpt)
C 100 continue
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START

      do 100 i=1,nwds,1
         kpt1    = kpt1    + 1
         if (kpt1 .gt. 64) then
             krec1   = krec1   + 1
             kpt1    = 1
             call rdprm (LUNSC1,krec1,JCLBUF,cmsg,kerr)
             if (kerr .ne. 0) go to 8000
         endif

         gbuf(i) = RCLBUF(kpt1)
  100 continue
C WNT-END
c
      kmxcl  = nwds
c
c...End of routine
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C8000 LSTIRC = krec
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
 8000 LSTIRC = krec1
      krec = krec1
      kpt = kpt1
C WNT-END
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcppr (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  This routine validates an APT Source file statement
c              enclosed in printesis.  One level of printesis only is
c              valid.
c
c   INPUT:  ktyp    I*4  D1  Major word value of post-processor command
c                            being parsed.
c
c           kpt     I*4  D1  Token within global arrays at which to
c                            start parsing.
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           kbuf    I*4  D50 Array to receive minor word values of
c                            parameters in the commmand.
c
c           gbuf    R*8  D50 Array to receive real values of parameters
c                            in the command.
c
c           klt     I*4  D1  Pointer of the closing parentesis in command.
c
c           knw     I*4  D1  Number of parameters in command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred in the
c                            input command syntax.
c
c***********************************************************************
c
      subroutine srcppr (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
      character*(*) cmsg
      integer*4 ktyp,kpt,klt,kbuf(*),kerr,krec,knw
      real*8 gbuf(*)
c
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 i,ist,ien,nc,ipt,icom,ntk
c
      character*80 ldat
c
c...Check for correct syntax
c
      kerr   = 0
      knw    = 0
      ntk    = 0
      ipt    = kpt
c
  100 do 500 i=ipt,NTOK,1
         ntk    = ntk + 1
         if (ntk .eq. 1) then
            if (ICTYP(i) .ne. 1) go to 9000
            ktyp   = RCSUB(i)
         else if (ntk .eq. 2) then
            if (ICTYP(i) .ne. 2 .and. RCSUB(i) .ne. 7) go to 9000
         else if (ntk .eq. 3 .and. ICTYP(i) .eq. 2 .and.
     -            RCSUB(i) .eq. 17) then
            klt = i
            go to 9100
         else
c
c......Minor word
c
           if (ICTYP(i) .eq. 1) then
              knw    = knw    + 1
              icom   = 0
              kbuf(knw) = RCSUB(i)
c
c......Delimiting coma
c
           else if (ICTYP(i) .eq. 2) then
              if (RCSUB(i) .eq. 2 .and. icom .eq. 0) then
                 icom   = 1
                 go to 500
              else if (RCSUB(i) .eq. 18) then
                 if (icom .eq. 1) go to 9000
                 klt    = i
                 go to 8000
              else
                 go to 9000
              end if
c
c......Real value
c
           else if (ICTYP(i) .eq. 3) then
              knw    = knw    + 1
              icom   = 0
              kbuf(knw) = 0
              gbuf(knw) = RCSUB(i)
c
c......$ - Continuation character
c
           else if (ICTYP(i) .eq. 4) then
              ist    = RCSUB(i)
              ien    = ICNC(i)
              if (LCTXT(ist:ien) .eq. '$') go to 600
              go to 9000
c
c......Unrecognized parameter
c
           else
              go to 9000
           endif
         endif
  500 continue
      go to 8000
c
c...Get continuation record
c
  600 call srcprs (krec,ldat,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      ipt    = 1
      go to 100
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 kerr   = 1
      go to 8000
c
 9100 kerr   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  srcpar (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  This routine parses an APT Source file statement
c              enclosed in printesis.  One level of printesis only is
c              valid.
c
c   INPUT:  ktyp    I*4  D1  Major word value of post-processor command
c                            being parsed.
c
c           kpt     I*4  D1  Token within global arrays at which to
c                            start parsing.
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           kbuf    I*4  D50 Array to receive minor word values of
c                            parameters in the commmand.
c
c           gbuf    R*8  D50 Array to receive real values of parameters
c                            in the command.
c
c           klt     I*4  D1  Pointer of the closing parentesis in command.
c
c           knw     I*4  D1  Number of parameters in command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred in the
c                            input command syntax.
c
c***********************************************************************
c
      subroutine srcpar (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
      character*(*) cmsg
      integer*4 ktyp,kpt,klt,kbuf(*),kerr,krec,knw
      real*8 gbuf(*)
c
      include 'menu.inc'
      include 'compile.inc'
c
      character*80 ldat
      integer*4 j,ist,nc,np,ipt,ilt
c
      kerr   = 0
      ipt    = kpt
      np     = 0
c
  100 do 250 j=ipt,NTOK
c
c...Check if not continuation mark
c
         if (ICTYP(j) .eq. 4) then
             ist    = RCSUB(j)
             if (LCTXT(ist:ICNC(j)) .eq. '$') go to 300
             go to 9000
         end if
         np = np + 1
c
c...Baypass "/("
c
         if (np .eq. 1) then
             if (ICTYP(j) .ne. 2 .or. RCSUB(j) .ne. 7) go to 9000
         else if (np .eq. 2) then
             if (ICTYP(j) .ne. 2 .or. RCSUB(j) .ne. 17) go to 9000
c
c...Parse statement in parentesis
c
         else if (np .eq. 3) then
             call srcppr (ktyp,krec,j,ilt,kbuf,gbuf,knw,cmsg,kerr)
             if (kerr .ne. 0) go to 9000
             klt    = ilt
             go to 8000
         end if
  250 continue
c
c...Parse continuation line
c
  300 call srcprs (krec,ldat,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
      ipt    = 1
      go to 100
c
c...End of routine
c
 8000 return
c
c...Error
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
c
c   FUNCTION:  This routine write cl work file logical record.
c
c   INPUT:  kbuf    I*4  Dn  Input data buffer.
c
c           kmxc    I*2  D1  Number of R*8 words in logical cl record.
c
c           krec    I*4  D1  Record number to write.
c
c           kcpt    I*4  D1  Pointer of last used JCLBUF array location.
c
c   OUTPUT: krec    I*4  D1  Last used Record number.
c
c           kcpt    I*4  D1  Pointer of last used JCLBUF array location.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred during
c                            write procedure.
c
c*****************************e*****************************************
c
      subroutine wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
c
      integer*4 kbuf(*),krec,kcpt,kerr
      integer*2 kmxc
      character*(*) cmsg
c
      include 'clnrd.inc'
      include 'menu.inc'
c
      integer*4 i,icr,icpt,ierr
c
      kerr   = 0
      icpt   = kcpt
      icr    = krec
      do 215 i=1,(kmxc+1)*2,1
         if (icpt .eq. 128) then
             icr    = icr    + 1
             call wrprm (LUNSC1,icr,JCLBUF,cmsg,ierr)
             if (ierr .ne. 0) go to 9000
             icpt   = 0
         endif
         icpt   = icpt   + 1
         JCLBUF(icpt) = kbuf(i)
  215 continue
c
      kcpt   = icpt
      krec   = icr
 8000 return
c
c...Error in writing
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ignwrd (ktyp,kflg)
c
c   FUNCTION:  This routine checks if it is valid postprocessor
c              command, so it can be ignored.
c
c   INPUT:  ktyp    I*4  D1 - Major word value.
c
c           kflg    I*4  D1 - Index of word in the 'ignor' array
c                             or 0 if not existing.
c***********************************************************************
c
      subroutine ignwrd (ktyp,kflg)
c
      integer*4 ktyp,kflg
c
      integer*4 ignor(20),nignor,jindex
c
      data nignor /4/
      data ignor /4038,4039,4040,4043,16*0/
c
      kflg  = jindex (ignor,ktyp,nignor)
c
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  srcppl (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
c   FUNCTION:  This routine parses an APT Source PLANE statement using
c              nested POINT definitions and returns PL in cononical form.
c
c   INPUT:  ktyp    I*4  D1  Major word value of post-processor command
c                            being parsed.
c
c           kpt     I*4  D1  Token within global arrays at which to
c                            start parsing.
c
c   OUTPUT: krec    I*4  D1  Record number of input record.
c
c           kbuf    I*4  D50 Array to receive minor word values of
c                            parameters in the commmand: here zeroed.
c
c           gbuf    R*8  D50 Array to receive real values of parameters
c                            in the command.
c
c           klt     I*4  D1  Pointer of the closing parentesis in command.
c
c           knw     I*4  D1  Number of parameters in output buffer = 4.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred in the
c                            input command syntax.
c
c***********************************************************************
c
      subroutine srcppl (ktyp,krec,kpt,klt,kbuf,gbuf,knw,cmsg,kerr)
c
      character*(*) cmsg
      integer*4 ktyp,kpt,klt,kbuf(*),kerr,krec,knw
      real*8 gbuf(*)
c
      include 'menu.inc'
      include 'compile.inc'
c
      character*80 ldat
c
      integer*4 ityp,irec,ipt,ilt,nwds,i,j,ier,ibuf(10),
     -          np,icom,nc,ist
c
      real*8 d1,d2,rbuf(10),pt(9),ndist,vc1(3),vc2(3)
c
      kerr   = 0
      knw    = 0
      ipt    = kpt
      np     = 0
      icom   = 0
c
c...Expecting nested POINT definiton
c...get pt coordinates
c
  100 i    = ipt + 1
         if (ICTYP(i) .eq. 1) then
            ityp   = RCSUB(i)
            if (ityp .ne. 4035) go to 9000
            call srcppr (ityp,irec,i,ilt,ibuf,rbuf,nwds,
     -                   cmsg,ier)
            if (ier .ne. 0 .or. nwds .ne. 3) go to 9100
            do 155 j=1,nwds
               if (ibuf(j) .ne. 0) go to 9000
               pt(np+j) = rbuf(j)
  155       continue
            np     = np + 3
            icom   = 0
         else
            go to 9000
         end if
c
c...Process delimiters after POINT definition ",( and )"
c
  200    do 250 i=ilt+1,NTOK,1
            if (ICTYP(i) .eq. 2) then
               if (RCSUB(i) .eq. 2 .and. icom .eq. 0) then
                   icom = 1
                   go to 250
               else if (RCSUB(i) .eq. 17) then
                   if (icom .eq. 0) go to 9000
                   ipt = i
                   go to 100
               else if (RCSUB(i) .eq. 18) then
                   if (i .eq. NTOK .and. np .eq. 9) go to 800
                   go to 9000
               else
                   go to 9000
               end if
c
c...Check for continuation line
c
            else if (ICTYP(i) .eq. 4) then
               ist    = RCSUB(i)
               if (LCTXT(ist:ICNC(i)) .eq. '$') go to 600
c
c...Unrecognized parameter
c
               go to 9000
            else
               go to 9000
            end if
  250    continue
      continue
c
c...Get next record from file
c
  600 call srcprs (krec,ldat,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
      ilt    = 0
      go to 200
c
c...Process PLANE/ statement,
c...create cononical definition
c
  800 d1     = ndist (pt(1),pt(4))
      d2     = ndist (pt(1),pt(7))
      do 805 i = 1,3,1
         vc1(i) = (pt(i+3) - pt(i)) / d1
         vc2(i) = (pt(i+6) - pt(i)) / d2
         kbuf(i) = 0
  805 continue
      call plnlnp (vc1,vc2,pt,gbuf,kerr)
      gbuf(4) = 0.d0 - gbuf(4)
c
c...Set output buffer like for simple input
c
      kbuf(4) = 0
      knw     = 4
      klt     = NTOK
      go to 8000
c
c...Errors
c
 9000 kerr   = 1
      go to 8000
 9100 kerr   = 2
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  chknum (kbuf,knum)
c
c   FUNCTION:  This routine checks if APT source buffer contains
c              numerical data only.
c
c   INPUT:  kbuf    I*4  D1 - Integer buffer (minor word value).
c
c           knum    I*4  D1 - Number of words to check.
c
c***********************************************************************
c
      function chknum (kbuf,knum)
c
      integer*4 chknum, kbuf(*), knum
c
      integer i
c
      chknum = 1
      do 100 i=1,knum,1
         if (kbuf(i) .ne. 0) go to 200
  100 continue
      chknum = 0
  200 return
      end


c
      subroutine wrnclend(krec)

      include 'menu.inc'
      include 'clnrd.inc'
      include 'pregen.inc'
      integer*4 krec, kerr
      character*80 cmsg

      call wrncl (LUNSC3,krec,JCOBUF,cmsg,kerr)
      return
      end

