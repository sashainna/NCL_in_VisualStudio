c
c***********************************************************************
c
c   FILE NAME: clload.for
c   CONTAINS:
c               opninp  wrclrc2  do_circul  outmot2  outmot  ncllod
c               cltype  cpywrk
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        clload.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/12/15 , 17:38:06
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  opninp (cmsg,kerr)
c
c   FUNCTION:  This routine opens the input clfile.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine opninp (cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
      include 'clnrd.inc'
      include 'pregen.inc'
c
      equivalence (IFIREC,KPOSMP(0057)), (IFIPT ,KPOSMP(0058))
      equivalence (MACHTP,KPOSMP(1201))
c
      integer*4 IFIREC,IFIPT,MACHTP
c
      integer*4 kerr
c
      character*(*) cmsg
c
      character*20 att(4)
      character*(MAX_PATH) ldev,fnam,cmpfil
      character*(MAX_FILE) lfil,lext
c
c...Initialize routine
c
      INCLFL = 0
      INCLNO = 0
c
c...Determine clfile type
c
  100 if (ICLF .eq. 0) then
          call cltype (LCMPFI,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Get input clfile name
c
      call fbreak (LCMPFI,ldev,lfil,lext)
      if (lext .eq. ' ') then
           lext = '.cl'
           if ((ICLF .eq. 2).or.(ICLF.eq.4)) lext = '.cla'
           if (ICLF .eq. 6) lext = '.nci'
      endif
      call fparse (lfil,fnam,ldev,lext)
c
c...Open temporary clfile
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'scratch'
      irecl  = 512
      call opnfil (LUNSC1,'clfile.tmp',att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Open and load input Apt source file
c
      if ((ICLF .eq. 2).or.(ICLF.eq.4)) then
          call srclod (fnam,0,cmsg,kerr)
c
c...Open input NCL clfile
c
      else if (ICLF .eq. 1) then
          call ncllod (fnam,0,cmsg,kerr)
c
c...Open input CATIA clfile
c
      else if (ICLF .eq. 3) then
          call catlod (fnam,0,cmsg,kerr)
c
c...Open input CATIA V5 clfile
c
      else if (ICLF .eq. 5) then
          call cv5lod (fnam,0,cmsg,kerr)
c
c...Open MasterCam clfile
c......Determine if Mill or Lathe
c
      else if (ICLF .eq. 6) then
          if (PGMNAM .eq. 'PostWorks' .and. INCLFL .eq. 0) then
              if (PREPT .ne. 0) then
                  call opnmch (LUNSC4,PSTNAM(PREPT),cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  call lodmch (LUNSC4,cmsg,kerr)
                  if (kerr .gt. 0) go to 8000
              else
                  MACHTP = 1
              endif
          endif
          call ncilod (fnam,0,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
c
c...INCLUD file was just processed
c...Copy rest of original clfile
c
      if (INCLFL .ne. 0) then
          call cpywrk (2,cmsg,kerr)
          LCMPFI = cmpfil
      endif
c
c...Check for next INCLUD file
c
      if (INCLNO .gt. 0) then
          call cpywrk (1,cmsg,kerr)
          cmpfil = LCMPFI
          LCMPFI = INCLNM(INCLNO)
          ICLF   = 0
          go to 100
      endif
c
c...Set up clfile pointers
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
c   SUBROUTINE:  wrclrc2 (kbuf,kmxc,krec,kcpt,cmsg,kerr)
c
c   FUNCTION:  This routine writes neutral cl file logical record and
c              controls creation of circular records when SET/LINCIR,...
c              is in effect.
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
c***********************************************************************
c
      subroutine wrclrc2 (kbuf,kmxc,krec,kcpt,cmsg,kerr)
c
      integer*4 kbuf(*),krec,kcpt,kerr
      integer*2 kmxc
      character*(*) cmsg
c
      include 'clnrd.inc'
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (CIRTOS,POSMAP(2241))
      real*8 CIRTOS
c
      integer*4 j, ityp, isub, nwds,inc
      real*8    r8dum
      integer*4 i4dum(2)
      integer*2 i2dum(4)
      equivalence (r8dum,i4dum,i2dum)
      integer*4 mxbuf
      parameter (mxbuf=3000)
      integer*4 is1,is2,is4
C VAX-SUN-SGI-IBM-HPX-START
C     data is1 /0/, is2 /4/, is4 /1/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 /1/, is2 /1/, is4 /4/
C WNT-DOS-DEC-END
c
c
      kerr = 0
      nwds = (MULT+1)*3
      i4dum(1) = kbuf(2)
      ityp = i2dum(1)
      isub = i2dum(2)
      i4dum(1) = kbuf(4-is1)
c
c...Motion record. For CIRTYP 1 and 2, buffer up first record and all
c...continuation records. For CIRTYP 3, buffer up records until max is
c...reached.
c
      if (ityp .eq. 5000) then
        if (CIRTYP .gt. 0 .and. ICYACT .eq. 0) then
          if (CIRTYP .eq. 1 .or. CIRTYP .eq. 2) then
            if (isub.eq.5 .and. ICNT.gt.0) then
              call do_circul (JCIRBF,NREC,krec,kcpt,cmsg,kerr)
              if (kerr.gt.0) goto 9000
            endif
          endif
          j = (kmxc+1)*2
          if (ICNT+j.gt.mxbuf) then
            call do_circul (JCIRBF,NREC,krec,kcpt,cmsg,kerr)
            if (kerr.gt.0) goto 9000
          endif
          call copynk(kbuf, JCIRBF(ICNT+1), j)
          ICNT = ICNT+j
          NREC = NREC+1
        else
          call wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
          if (kerr.gt.0) goto 9000
          call copyn (kbuf((kmxc-MULT*3-2)*4+1), PTL, nwds)
        endif
c
c...SET/,...
c
      else if (ityp.eq.2000.and.isub.eq.1087) then
        if (ICNT.gt.0) then
          call do_circul(JCIRBF,NREC,krec,kcpt,cmsg,kerr)
          if (kerr.gt.0) goto 9000
        endif
c
c...SET/LINCIR,...
c
        j = i2dum(2-is1)
        if (j .eq. 95) then
            inc = 5
  100       if (inc/2 .le. kmxc) then
                i4dum(1) = kbuf(inc)
                i4dum(2) = kbuf(inc+1)
                if (i2dum(is4) .ne. 0) then
                    CIRTOS = r8dum
                else
                    j = i2dum(is2)
                    if (j.eq.1071) then
                        CIRTYP = 3
                    else if (j.eq.51) then
                        CIRTYP = 2
                    else
                        CIRTYP = 1
                    endif
                endif
                inc = inc + 2
                go to 100
            endif
c
c...SET/LINEAR,... and other SET commands
c
        else
          if (j.eq.76) CIRTYP = 0
          call wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
          if (kerr.gt.0) goto 9000
        endif
c
c...CYCLE
c
      else if (ityp .eq. 2000 .and. isub .eq. 1054) then
          if (ICNT .gt. 0) then
              call do_circul(JCIRBF,NREC,krec,kcpt,cmsg,kerr)
              if (kerr.gt.0) goto 9000
          endif
          j = i2dum(2-is1)
          if (j .eq. 72) then
              ICYACT = 0
          else if (j .ne. 88 .and. j .ne. 187 .and. j .ne. 144) then
              ICYACT = 1
          endif
          call wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
          if (kerr.gt.0) goto 9000
c
c...Everything else
c
      else
        if (ICNT .gt. 0) then
          call do_circul(JCIRBF,NREC,krec,kcpt,cmsg,kerr)
          if (kerr.gt.0) goto 9000
        endif
c        if (ityp.eq.9000) MULT = isub
        call wrclrc (kbuf,kmxc,krec,kcpt,cmsg,kerr)
        if (kerr.gt.0) goto 9000
      endif
c
 8000 return
c
c...Error
c
 9000 kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  do_circul (kdat, mrec, krec, kcpt, cmsg, kerr)
c
c   FUNCTION:  Create circular records out of goto points.
c
c   INPUT:  kdat    I*2  Dn  Input data buffer.
c
c           mrec    I*4  D1  Numder of cl records in kdat
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
      subroutine do_circul (kdat, mrec, krec, kcpt, cmsg, kerr)
c
      integer*2 kdat(*)
      integer*4 mrec, krec, kcpt, kerr
      character*(*) cmsg
c
      include 'clnrd.inc'
      include 'post.inc'
c
      equivalence (NPT,KPOSMP(0059))
      integer*4 NPT
c
      real*8 svpt(1500), cirr(7), hldcirr(7)
      integer*4 i, j, k, n, ix, nwds, npts, ifl, iflsv
      integer*4 istrt, iend, stpt
      integer*4 kseq, ixk
      integer*4 ksq(500), ksn(500)
      integer*2 ix2
c
      NPT  = (MULT+1)*3
      kerr = 0
      nwds = 0
      ixk  = 0
      ix   = 1
      do 20 i=1,mrec
        n    = kdat(ix)
        kseq = kdat(ix+1)
        call copyn (kdat(ix+4), svpt(nwds+1), n)
        nwds = nwds+n
        ix   = ix + n*4 +4
        k    = n/NPT
        do 10 j=1,k
          ixk      = ixk+1
          ksq(ixk) = kseq
          ksn(ixk) = n
          n        = n-NPT
  10    continue
  20  continue

      npts = nwds / NPT
c
c...If less than 3 points, just output 5000 record
c
      if (npts.lt.3) then
        if (npts.gt.0) then
          ix = 1
          do 40 i=1,mrec
            ix2 = kdat(ix)
            call wrclrc (kdat(ix),ix2,krec,kcpt,cmsg,kerr)
            if (kerr.gt.0) goto 9000
            ix = ix + (ix2+1)*4
   40     continue
        call copyn (svpt((npts-1)*NPT+1),PTL,NPT)
        endif
        goto 8000
      endif
c
      kseq = kdat(2)
c
c...Find circular moves
c
      if (CIRTYP.eq.1) then
        j = 1
        call pre_circk (j,npts,PTL,svpt,cirr,ifl)
        if (ifl.eq.1) then
          call outcir2 (cirr, kseq, krec, kcpt, cmsg, kerr)
          if (kerr.gt.0) goto 9000
        endif
        ix2 = kdat(1)
        call wrclrc (kdat,ix2,krec,kcpt,cmsg,kerr)
        if (kerr.gt.0) goto 9000
        call copyn (svpt((npts-1)*NPT+1),PTL,NPT)
        goto 8000
      endif
c
      stpt  = 1
      istrt = 1
      iend  = 3
      iflsv = -1
c
      do 200 i=1,npts
        call pre_circk (istrt,iend,PTL,svpt,cirr,ifl)
        if (ifl.eq.0) then
c
c...Circle not found - output saved circle, if any
c
          if (iflsv.eq.1) then
            call outcir2 (hldcirr, kseq, krec, kcpt, cmsg, kerr)
            if (kerr.gt.0) goto 9000
            j = (istrt-1)*NPT+1
            n = (iend-istrt)*NPT
            call outmot (svpt(j),n,kseq,krec,kcpt,cmsg,kerr)
            if (kerr.gt.0) goto 9000
            istrt = iend
            stpt  = iend
            iend  = iend+2
          else
            istrt = istrt+1
            iend  = iend+1
          endif
          call copyn (svpt((istrt-2)*NPT+1),PTL,NPT)
        else
c
c...Circle found - output saved points, if any
c
          if (iflsv.eq.0) then
            n = istrt-stpt
            if (n.gt.0) then
              j = (stpt-1)*NPT+1
              call outmot2 (svpt(j),ksq(stpt),ksn(stpt),n,
     x                    krec,kcpt,cmsg,kerr)
              if (kerr.gt.0) goto 9000
            endif
            kseq = ksq(istrt)
          endif
          iend = iend+1
          call copyn (cirr, hldcirr, 7)
        endif
        iflsv = ifl
        if (iend.gt.npts) goto 300
  200 continue

  300 continue
c
c --- Output last points or circle.
c
      if (ifl.eq.0) then
          n = npts-stpt+1
          if (n.gt.0) then
            j = (stpt-1)*NPT+1
            call outmot2 (svpt(j),ksq(stpt),ksn(stpt),n,
     x                    krec,kcpt,cmsg,kerr)
          endif
      else
          call outcir2 (hldcirr, kseq, krec, kcpt, cmsg, kerr)
          j = (istrt-1)*NPT+1
          n = (npts-istrt+1)*NPT
          call outmot (svpt(j),n,kseq,krec,kcpt,cmsg,kerr)
      endif
      call copyn (svpt((npts-1)*NPT+1),PTL,NPT)
c
 8000 continue
      mrec = 0
      ICNT = 0
      return
c
c...Error
c
 9000 kerr   = 1
      goto 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  outmot2 (pts, ksq, ksn, npts, krec, kcpt, cmsg, kerr)
c
c   FUNCTION:  Create goto record(s) out of goto points.
c
c   INPUT:  pts     R*8  Dn  Input data buffer.
c
c           ksq     I*4  D1  Array of sequence numbers of each point in each
c                            record.
c
c           ksn     I*4  D1  Array of number of points remaining in record for
c                            each point in each record.
c
c           npts    I*4  D1  Number of points in pts array.
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
      subroutine outmot2 (pts, ksq, ksn, npts, krec, kcpt, cmsg, kerr)
c
      real*8 pts(*)
      integer*4 ksq(*), ksn(*)
      integer*4 npts,krec,kcpt,kerr
      character*(*) cmsg
c
      include 'post.inc'
c
      equivalence (NPT,KPOSMP(0059))
      integer*4 NPT
c
      integer*4 ix1,ix2,n1,n2,kseq,irec,icpt
c
      kerr = 0
      icpt = kcpt
      irec = krec
      ix1  = 1
      ix2  = 1
      n2   = npts*NPT
c
   20 continue
      kseq = ksq(ix2)
      n1   = ksn(ix2)
      if (n1.gt.n2) n1 = n2
      call outmot (pts(ix1), n1, kseq, irec, icpt, cmsg, kerr)
      if (kerr.ne.0) goto 9000
      n2   = n2-n1
      ix1  = ix1+n1
      ix2  = ix2+n1/NPT
      if (n2.gt.0) goto 20
c
      kcpt = icpt
      krec = irec
c
 8000 return
c
c...Error
c
 9000 kerr   = 1
      goto 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  outmot (pts, npts, kseq, krec, kcpt, cmsg, kerr)
c
c   FUNCTION:  Create goto record out of goto points.
c
c   INPUT:  pts     R*8  Dn  Input data buffer.
c
c           npts    I*4  D1  Number of cl records in kdat.
c
c           kseq    I*4  D1  Sequence number of record.
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
      subroutine outmot (pts, npts, kseq, krec, kcpt, cmsg, kerr)
c
      real*8 pts(*)
      integer*4 npts,kseq,krec,kcpt,kerr
      character*(*) cmsg
c
      integer*4 ix,n1,irec,nwds,mxrec,icpt,ierr
      integer*2 khed(4), i2, isub
c
      kerr  = 0
      icpt  = kcpt
      irec  = krec
      isub  = 5
      ix    = 1
      mxrec = 240
      nwds  = npts
c
   10 continue
        n1 = nwds
        if (n1 .gt. mxrec) n1 = mxrec
        khed(1) = n1
        khed(2) = kseq
        khed(3) = 5000
        khed(4) = isub
        i2 = 0
        call wrclrc (khed,i2,irec,icpt,cmsg,ierr)
        if (ierr.ne.0) goto 9000
        i2 = khed(1)-1
        call wrclrc (pts(ix),i2,irec,icpt,cmsg,ierr)
        if (ierr.ne.0) goto 9000
        nwds = nwds - mxrec
        ix   = ix + mxrec
        isub = 6
      if (nwds.gt.0) goto 10
c
      kcpt   = icpt
      krec   = irec
c
 8000 return
c
c...Error
c
 9000 kerr   = 1
      goto 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncllod (fnam,cmsg,kerr)
c
c   FUNCTION:  Read NCL clfile and write neutral scratch file, creating
c              circular records as necessary.
c
c   INPUT:  fnam    C*n  D1  CL filename.
c
c           ktest   I*4  D1  1 = Test to see if this is a valid clfile.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred during
c                            write procedure.
c
c***********************************************************************
c
      subroutine ncllod (fnam,ktest,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      integer*4 kerr,ktest
      character*(*) fnam,cmsg
c
      equivalence (VECSAV,POSMAP(1372))
      real*8 VECSAV(3)
c
      character*20 att(4)
      character*72 lbuf
      integer*4 i, j, jrec, jpt, krec, kpt, irecl, kcnt
      integer*4 ifin, ipt, itype, isub, icirc, jisn
      integer*4 is1, is4, inc, inum, jindex
      integer*2 nwds, ibuf(4)
      integer*4 jbuf(840), jclbf(844)
      integer*2 kbuf(1680), kclbf(1684)
      real*8 rbuf, gbuf(420)
      logical lpstwd
c
      equivalence (gbuf,jbuf,kbuf), (gbuf(2),lbuf), (jclbf,kclbf)
      equivalence (rbuf,ibuf)
c
C VAX-SUN-SGI-IBM-HPX-START
C     data is1 / 1/, is4 / 4/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 / 4/, is4 / 1/
C WNT-DOS-DEC-END

      kerr   = 0
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 288
      call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (ktest .eq. 0) call getfnm (LUNSC2,LCMPFI,NCCMPF,MAX_PATH)
c
c...Initialize variables
c
      jrec  = 0
      jpt   = 0
      kcnt  = 0
      krec  = 0
      kpt   = 9999
      jisn  = 0
      ifin  = 0
      icirc = 0
      CIRTYP = 0
      MULT  = 0
      ICYACT = 0
      do 10 i=1,5
   10 PTL(i) = 0.0d0
      PTL(6) = 1.0d0
      VECSAV(1) = 0.0d0
      VECSAV(2) = 0.0d0
      VECSAV(3) = 1.0d0
c
c...Store default MULTAX mode
c...if this is an INCLUDed clfile
c
      if (INCLNO .gt. 0 .and. ktest .eq. 0) then
          call wrmult (MULT,0,jrec,jpt,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Process clfile records until FINI found.
c
  100 continue
      if (ktest .eq. 1) then
          kcnt   = kcnt   + 1
          if (kcnt .gt. 2) go to 8000
      endif
c
c...Read cl record if necessary
c
      kpt    = kpt    + 1
      if (kpt .ge. 36) then
          krec   = krec   + 1
          kpt    = 1
          call rdncl (LUNSC2,krec,jclbf,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Store ISN
c
      ipt    = (kpt*4) - 3
      if (kclbf(ipt+1) .eq. 1001) then
          jisn   = jclbf(kpt*2)
          go to 100
      endif
c
c...Extract header
c
      nwds    = kclbf(ipt) - 1
      itype   = kclbf(ipt+1)
      isub    = kclbf(ipt+2)
c
c...Make sure valid clfile
c
      if (nwds .lt. 0 .or. nwds .gt. 420) go to 9000
      if (itype .gt. 14000) go to 9000
c
c...MULTAX - NCL CL file uses 0 for on, 1 for off,
c...Postworks uses 1 for on, 0 for off
c
      if (itype.eq.9000) then
        isub  = 1-isub
        MULT  = isub
      endif
      kbuf(1) = nwds
      kbuf(2) = kclbf(ipt+3)
      if (jisn .ne. 0) kbuf(2) = jisn
      kbuf(3) = itype
      kbuf(4) = isub
      jisn    = 0
      lpstwd = .false.
      if (itype .eq. 2000) then
        inc    = jindex (VNMAJ,isub,NNMAJ)
        if (inc .ne. 0) kbuf(4) = VAMAJ(inc)
        lpstwd = isub .lt. 1043 .or. isub .gt. 1046
      endif
      j       = 3
      do 200 i=1,nwds
        kpt     = kpt + 1
        if (kpt .ge. 36) then
          krec  = krec + 1
          kpt   = 1
          call rdncl (LUNSC2,krec,jclbf,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
        endif
        jbuf(j) = jclbf(kpt*2-1)
        j       = j+1
        jbuf(j) = jclbf(kpt*2)
        j       = j+1
  200 continue
      if (lpstwd) then
        ipt = 1
        if (kbuf(4) .eq. 1015) ipt = 2
        do 205 i=ipt+1,nwds+1,1
          rbuf = gbuf(i)
          if (ibuf(is1) .eq. 0) then
            inum   = ibuf(is4)
            inc    = jindex (VNMIN,inum,NNMIN)
            if (inc .ne. 0) then
              ibuf(is4) = VAMIN(inc)
              gbuf(i) = rbuf
            endif
          endif
  205   continue
      else if (itype .eq. 2000) then
        lbuf(67:) = ' '
      endif
c
c...Don't look for circular moves in GOTO records following a circular rec
c
      if (icirc.gt.0) then
        if (itype.eq.5000) then
          if (icirc.eq.1) then
            icirc = 2
          else if (isub.ne.6) then
            icirc = 0
          endif
        else
          icirc = 0
        endif
      endif
c
c...Remove first 5 real*8's from circular record
c
      if (itype.eq.3000) then
        j    = 7
        nwds = 7
        kbuf(1) = nwds
        call copyn (jbuf(13),jbuf(3),j)
      endif
c
c...Type 14000 record needs a subtype of 1
c...Due to addition of PPRINT INCLUD commands
c
      if (itype .eq. 14000) kbuf(4) = 1
c
c...Write out record
c
      if (ktest .eq. 0) then
          if (icirc.gt.0) then
            call wrclrc (jbuf,nwds,jrec,jpt,cmsg,kerr)
          else
            call wrclrc2 (jbuf,nwds,jrec,jpt,cmsg,kerr)
          endif
      endif
      if (kerr .ne. 0) go to 8000
      if (itype.eq.3000) icirc = 1
c
c...Keep going until FINI seen
c
      if (itype.ne.14000) goto 100
c
c...Write out the last record, if any
c
      if (jpt .ne. 0 .and. ktest .eq. 0) then
         jrec   = jrec   + 1
         call wrprm (LUNSC1,jrec,JCLBUF,cmsg,kerr)
      endif

      ICLF = 2
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
c
c...Invalid clfile format
c
 9000 kerr   = 1
      cmsg = 'Invalid clfile format.'
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  outcir2 (gcir,kseq,krec,kcpt,cmsg,kerr)
c
c   FUNCTION:  This routine outputs a circular record to the cl file
c              with a subtype of 99 to show that it was generated by
c              SET/LINCIR,...
c
c   INPUT:  gcir    R*8  D7  Circle canonical data.
c
c           kseq    I*4  D1  Sequence number of motion statement.
c
c           krec    I*4  D1  Output Clfile record number.
c
c           kcpt    I*4  D1  Output Clfile record pointer.
c
c   OUTPUT: kerr    I*4  D1  Returns internal error number if any.
c
c           cmsg    C*n  D1  Text of error message.
c
c***********************************************************************
c
      subroutine outcir2 (gcir,kseq,krec,kcpt,cmsg,kerr)
c
      include 'post.inc'
      include 'menu.inc'
      include 'clnrd.inc'
c
      real*8 gcir(7)
      integer*4 kseq,kerr,krec,kcpt
      character*80 cmsg
c
      integer*4 icr,icpt
      integer*2 khed(4), ix2
c
      kerr   = 0
      icr    = krec
      icpt   = kcpt
c
c...create circular CL record
c
      khed(1) = 7
      khed(2) = kseq
      khed(3) = 3000
      khed(4) = 99
      ix2     = 0
      call wrclrc (khed,ix2,icr,icpt,cmsg,kerr)
      if (kerr .ne. 0) goto 8000
      ix2     = 6
      call wrclrc (gcir,ix2,icr,icpt,cmsg,kerr)
      if (kerr .ne. 0) goto 8000
      krec = icr
      kcpt = icpt
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cltype (fnam,cmsg,kerr)
c
c   FUNCTION:  Reads an input clfile and determines which type it is.
c              ICLF is updated with the appropriate file type.
c
c   INPUT:  fnam    C*n  D1  CL filename.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred during
c                            write procedure.
c
c***********************************************************************
c
      subroutine cltype (fnam,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      integer*4 kerr
      character*(*) fnam,cmsg
c
      character*(MAX_PATH) ldev
      character*(MAX_FILE) lfil,lext
c
c...Get base filename
c
      call fbreak (fnam,ldev,lfil,lext)
c
c...NCL clfile
c
      if (lext .eq. ' ') call fparse (lfil,fnam,ldev,'.cl')
      call ncllod (fnam,1,cmsg,kerr)
      if (kerr .eq. 0) then
          ICLF   = 1
          go to 8000
      endif
c
c...APT source file
c
      if (lext .eq. ' ') call fparse (lfil,fnam,ldev,'.cla')
      call srclod (fnam,1,cmsg,kerr)
      if (kerr .eq. 0) then
          ICLF   = 2
          go to 8000
      endif
c
c...Catia V5 file
c
      if (lext .eq. ' ') call fparse (lfil,fnam,ldev,'.clfile')
      call cv5lod (fnam,1,cmsg,kerr)
      if (kerr .eq. 0) then
          ICLF   = 5
          go to 8000
      endif
c
c...Catia file
c
      if (lext .eq. ' ') call fparse (lfil,fnam,ldev,'.clfile')
      call catlod (fnam,1,cmsg,kerr)
      if (kerr .eq. 0) then
          ICLF   = 3
          go to 8000
      endif
c
c...MasterCam file
c
      if (lext .eq. ' ') call fparse (lfil,fnam,ldev,'.nci')
      call ncilod (fnam,1,cmsg,kerr)
      if (kerr .eq. 0) then
          ICLF   = 6
          go to 8000
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cpywrk (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine copies the working clfile into another
c              clfile in support of INCLUDing external clfiles.
c
c   INPUT:  kfl     I*4  D1  -  1 = Copy first part of clfile up to
c                               the INCLUD line.  2 = Copy last part
c                               of clfile after INCLUD line.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cpywrk (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*2 inum(4),ibuf(256)
      integer*4 i,j,irec,jrec,icpt,irecl,jnum(2),jbuf(128),ilun,ist,ifl
c
      character*20 att(4)
c
      equivalence (jbuf,ibuf), (jnum,inum)
c
c...Copy first part of original working clfile
c...to new working clfile
c
      if (kfl .eq. 1) then
c
c......First open 2nd temporary file
c
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'scratch'
          irecl  = 512
          call opnfil (LUNSC4,'clfile1.tmp',att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Transfer the clfile
c
          irec   = 0
  100     irec   = irec   + 1
          call rdprm (LUNSC1,irec,JCLBUF,cmsg,kerr)
          if (kerr .eq. 0) then
              call wrprm (LUNSC4,irec,JCLBUF,cmsg,kerr)
              go to 100
          endif
          ilun   = LUNSC1
          LUNSC1 = LUNSC3
          LUNSC3 = ilun
          INCLPT(1) = INCLIX(1,INCLNO)
          INCLPT(2) = INCLIX(2,INCLNO)
          INCLFL = INCLNO
c
c...Copy second part of original working clfile
c...to new working clfile
c
      else
          call rdprm (LUNSC4,INCLPT(1),jbuf,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          irec   = INCLPT(1) - 1
          icpt   = INCLPT(2)
          ifl    = 0
          idid   = 0
c
c......Copy the included file first
c......and then the remainder of the original work file
c
          jrec   = 0
          ist    = 1
  200     jrec   = jrec   + 1
          call rdprm (LUNSC1,jrec,JCLBUF,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          do 210 i=ist,128,1
              jnum(1) = JCLBUF(i)
              jnum(2) = JCLBUF(i+1)
              if (i/2*2 .ne. i) then
                  if (inum(1) .eq. 0 .and. inum(3) .eq. 14000 .and.
     1                inum(4) .eq. 1) then
                      ifl   = ifl   + 1
                      if (ifl .eq. 1) go to 250
                  endif
              endif
              if (icpt .eq. 128) then
                  irec   = irec   + 1
                  call wrprm (LUNSC4,irec,jbuf,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  icpt   = 0
              endif
              icpt   = icpt   + 1
              jbuf(icpt) = JCLBUF(i)
c
c.........Adjust pointers for nested include files
c
              if (INCLNO .gt. INCLFL .and. ifl .eq. 0) then
                  do 205 j=INCLFL+1,INCLNO,1
                      if (idid .eq. 0 .and. jrec .eq. INCLIX(1,j) .and.
     1                    i .eq. INCLIX(2,j)) then
                          INCLIX(1,j) = irec   + 1
                          INCLIX(2,j) = icpt
                          idid   = 1
                      endif
  205             continue
              endif
  210     continue
c
c......Check for FINI card
c
          if (ifl .ge. 2) then
              if (icpt .ne. 0) then
                  irec   = irec   + 1
                  call wrprm (LUNSC4,irec,jbuf,cmsg,kerr)
              endif
              go to 300
          endif
          ist    = 1
          go to 200
c
c......Now setup to copy the rest of the
c......original work clfile
c......First restore MULTAX setting
c
  250     inum(1) = 0
          inum(3) = 9000
          inum(4) = INCLMU(INCLFL)
          do 260 i=1,2,1
              if (icpt .eq. 128) then
                  irec   = irec   + 1
                  call wrprm (LUNSC4,irec,jbuf,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  icpt   = 0
              endif
              icpt   = icpt   + 1
              jbuf(icpt) = jnum(i)
  260     continue
c
          jrec   = INCLPT(1) - 1
          ist    = INCLPT(2) + 1
          ilun   = LUNSC1
          LUNSC1 = LUNSC3
          LUNSC3 = ilun
          go to 200
c
c......Set up correct LUN numbers and
c......Get ready for next include file
c
  300     ilun   = LUNSC1
          LUNSC1 = LUNSC4
          LUNSC4 = ilun
          call clsfil (LUNSC3)
          call clsfil (LUNSC4)
c
c......Found nested include
c......Adjust pointers so current include
c......is last on list
c
          if (INCLNO .gt. INCLFL) then
              do 350 i=INCLFL,INCLNO-1,1
                  INCLIX(1,i) = INCLIX(1,i+1)
                  INCLIX(2,i) = INCLIX(2,i+1)
                  INCLNM(i) = INCLNM(i+1)
                  INCLMU(i) = INCLMU(i+1)
  350         continue
          endif
          INCLFL = 0
          INCLNO = INCLNO - 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  wrmult (kmult,krec,kcr,kcpt,cmsg,kerr)
c
c   FUNCTION:  This routine writes a MULTAX record to the clfile.
c
c   INPUT:  kmult   I*4  D1  -  0 = MULTAX/OFF.  1 = MULTAX/ON.
c
c           krec    I*4  D1  -  Input record number of multax record.
c
c           kcr     I*4  D1  -  Pointer to output clfile record.
c
c           kpt     I*4  D1  -  Pointer to within clfile record.
c
c   OUTPUT: kcr     I*4  D1  -  Pointer to output clfile record.
c
c           kpt     I*4  D1  -  Pointer to within clfile record.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine wrmult (kmult,krec,kcr,kpt,cmsg,kerr)
c
      integer*4 kmult,krec,kcr,kpt,kerr
c
      character*(*) cmsg
c
      integer*2 inbuf(4),mxc
      integer*4 jbuf(2)
c
      equivalence (jbuf,inbuf)
c
c...Store MULTAX record
c
      inbuf(1) = 0
      inbuf(2) = KREC
      inbuf(3) = 9000
      inbuf(4) = KMULT
      mxc    = 0
      call wrclrc (jbuf,mxc,kcr,kpt,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
