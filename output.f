c
c***********************************************************************
c
c   FILE NAME: output.for
c   CONTAINS:
c               output  pakout  pchout  prtdat  prthed  prtout  prtrec
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       output.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c       10/19/15 , 09:03:10
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  output
c
c   FUNCTION:  This routine formats the print file record and outputs
c              both the print and punch file records.  This routine
c              should be called after a complete logical Control tape
c              block is ready for output.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine output
c
      include 'post.inc'
c
      equivalence (MOTFLG,KPOSMP(1073))
      equivalence (PCHBNC,KPOSMP(1109)), (PRTBNC,KPOSMP(1153))
      equivalence (IPRDES,KPOSMP(1154))
c
      integer*4 MOTFLG,PCHBNC,PRTBNC,IPRDES(2,10)
c
      equivalence (PCHBLK,CPOSMP(3341))
c
      character*512 PCHBLK
c
      integer*4 inc
c
c...Output punch file record
c
      call pakout (PCHBLK,PCHBNC,1)
      PCHBNC = 0
c
c...Output print file record
c
      if (MOTFLG .eq. 1) then
          inc    = 3
      else
          inc    = 4
      endif
      call prtrec (IPRDES(1,inc))
      PRTBNC = 0
      MOTFLG = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pakout (cbuf,knc,kfl)
c
c   FUNCTION:  This routine formats a text string for output to the
c              punch file, packing it if necessary.
c
c   INPUT:  cbuf    C*n  D1  -  Single tape block to output to the punch
c                               file.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c           kfl     I*4  D1  -  1 = Count this block towards number of
c                                   blocks to output prior to tape break.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pakout (cbuf,knc,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (IPACKD,KPOSMP(1101)), (IPCHPN,KPOSMP(1107))
      equivalence (BRKOP ,KPOSMP(1137))
c
      integer*4 IPACKD,IPCHPN,MCHOPT(20),BRKOP(10)
c
      equivalence (TAPLEN,POSMAP(1204)), (BRKPRM,POSMAP(1215))
c
      real*8 TAPLEN,BRKPRM(5)
c
      equivalence (PCHBUF,CPOSMP(2231))
c
      character*72 PCHBUF
c
      integer*4 knc
c
      character*(*) cbuf
c
      integer*4 inc,nc,nci
c
      real*8 rnum,runit
c
      character*72 sbuf
      character*512 tbuf
c
c...Accumulate tape length
c
      rnum   = knc
      runit  = 120.
      if (MCHOPT(2) .eq. 2) runit = 393.7007874
      TAPLEN = TAPLEN + rnum / runit
      BRKPRM(1) = BRKPRM(1) + rnum / runit
c
c...Accumulate number of tape blocks
c
      if (kfl .eq. 1 .and. BRKOP(1) .eq. 5) BRKPRM(4) = BRKPRM(4) + 1
c
c...File is unpacked
c...Output buffer as is
c
      if (IPACKD .eq. 2 .and. knc .ge. 0) then
          if (IPCHPN .ne. 0) then
              tbuf   = PCHBUF(1:IPCHPN) // cbuf(1:knc)
              nci    = IPCHPN + knc
              IPCHPN = 0
          else
              tbuf   = cbuf(1:knc)
              nci    = knc
          endif
          if (nci .le. MCHOPT(5)) then
              call pchout (tbuf,nci)
          else
              nc     = nci
              inc    = 1
  100         if (nc .lt. MCHOPT(5)) then
                  call pchout (tbuf(inc:inc+nc-1),nc)
              else
                  call pchout (tbuf(inc:inc+MCHOPT(5)-1),MCHOPT(5))
              endif
              inc    = inc    + MCHOPT(5)
              nc     = nc     - MCHOPT(5)
              if (nc .gt. 0) go to 100
          endif
c
c...Pack buffer into 72 character
c...output buffer
c
      else
          nci    = knc
          if (nci .lt. 0) nci = -nci
          if (IPCHPN+nci .lt. 72) then
              if (IPCHPN .eq. 0) then
                  PCHBUF = cbuf
                  IPCHPN = nci
              else
                  sbuf   = PCHBUF(1:IPCHPN) // cbuf(1:nci)
                  PCHBUF = sbuf
                  IPCHPN = IPCHPN + nci
              endif
          else
              if (IPCHPN .eq. 0) then
                  PCHBUF = cbuf(1:72)
              else
                  sbuf   = PCHBUF(1:IPCHPN) // cbuf(1:72-IPCHPN)
                  PCHBUF = sbuf
              endif
              call pchout (PCHBUF,72)
              PCHBUF = cbuf(72-IPCHPN+1:nci)
              IPCHPN = nci    - (72-IPCHPN)
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
c   SUBROUTINE:  pchout (cbuf,knc)
c
c   FUNCTION:  This routine outputs a punch file record.  It also adds
c              the card id string in cols 73-80 for packed records.
c
c   INPUT:  cbuf    C*n  D1  -  Text buffer to output to punch file.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pchout (cbuf,knc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IPACKD,KPOSMP(1101)), (IPCHPN,KPOSMP(1107))
      equivalence (IPCHSQ,KPOSMP(1108))
c
      integer*4 IPACKD,IPCHPN,IPCHSQ
c
      equivalence (LPARTN,CPOSMP(0067)), (PCHBUF,CPOSMP(2231))
c
      character*66 LPARTN
      character*72 PCHBUF
c
      integer*4 knc
c
      character*(*) cbuf
c
      integer*4 nc,nspac,ierr
c
      character*20 lnum
      character*72 spac
      character*80 msg
      character*512 sbuf
c
      data spac /' '/
c
      if (IOPFL(7) .eq. 0) go to 8000
c
c...File is unpacked
c...Output buffer as is
c
      if (IPACKD .eq. 2) then
          sbuf   = cbuf
          nc     = knc
c
c...File is packed
c...Add card id to buffer
c
      else
          call itoc (IPCHSQ,lnum,nc,4)
          nspac  = 72 - knc
          if (nspac .eq. 0) then
              sbuf   = cbuf(1:knc) // LPARTN(1:4) // lnum(1:nc)
          else
              sbuf   = cbuf(1:knc) // spac(1:nspac) // LPARTN(1:4) //
     1                 lnum(1:nc)
          endif
          nc     = 80
          IPCHSQ = IPCHSQ + 1
      endif
c
c...Output buffer
c
      call wrtxt (LUNSC4,sbuf,nc,msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...End of routine
c
 8000 return
c
c...Error writing punch file
c
 9000 call errkil (msg,ierr)
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prtdat (krec,klin,cout,knco,cbuf,kary,kfl)
c
c   FUNCTION:  This routine formats a Print Descriptor line for output
c              to the print file.
c
c   INPUT:  krec    I*4  D1  -  Record number of Print Descriptor.
c
c           klin    I*4  D1  -  Line number within record to format.
c
c           kary    I*4  D3  -  (3) = Increment when multiple copies of
c                               the same line is output.  See also
c                               OUTPUT.
c
c   OUTPUT: cout    I*4  D1  -  Formatted Print Descriptor line.
c
c           knco    I*4  D1  -  Number of characters in 'cout'.
c
c           cbuf    C*n  D2  -  (1) = Remaining field text when a con-
c                               tinuation line is required.
c
c           kary    I*4  D3  -  (1) = Number of chars in 'cbuf'.  (2) =
c                               Current overflow variable field.
c
c           kfl     I*4  D1  -  Returns 1 when a continuation record is
c                               required.  'kfl' should be set to 0
c                               prior to the first call and not modified
c                               after that.  'cbuf' and 'kary' should
c                               not be modified by the calling routine.
c
c***********************************************************************
c
      subroutine prtdat (krec,klin,cout,knco,cbuf,kary,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,66)
c
      integer*4 krec,klin,knco,kary(3),kfl
c
      character*(*) cout,cbuf
c
      integer*4 i,ipt,nc,inc,isub,ist,ien,ifl,ibeg,iary(3)
c
      character*512 sbuf,ldat,lstar
c
      data lstar /'*****************************************************
     1******************************************************************
     2*************'/
c
c...Store text data
c
      ifl    = kfl
      kfl    = 0
      ipt    = (krec-1) * 6 + klin
      if (ifl .eq. 0) then
          if (FRMFNC(1,ipt) .eq. 0) then
              knco   = 0
              cout = ' '
          else
              knco   = FRMFNC(1,ipt)
              cout   = FRMBUF(ipt)(1:FRMFNC(1,ipt))
          endif
          ibeg   = 1
c
c...Store overflow text
c
      else
          inc    = (krec-1) * 150 + (kary(2)-1) * 10 + 1
          isub   = IPRTXT(inc+4)
          if (isub .gt. FRMFNC(1,ipt)) then
              knco   = 0
              cout   = ' '
          else
              knco   = FRMFNC(1,ipt) - isub
              cout   = ' '
              cout(isub+1:FRMFNC(1,ipt)) =
     1            FRMBUF(ipt)(isub+1:FRMFNC(1,ipt))
          endif
          ibeg   = kary(2)
      endif
c
c...Store variable data
c
      ipt    = (krec-1) * 150 + (ibeg-1) * 10 + 1
      do 2000 i=ibeg,FRMFNC(3,krec),1
          if (IPRTXT(ipt+9)-klin) 1900,100,8000
c
c......Overflow data
c
  100     if (ifl .eq. 1) then
              ldat   = cbuf
              nc     = kary(1)
              ifl    = 0
              go to 600
          endif
c
c......Variable data
c
          iary(1) = IPRTXT(ipt)
          iary(2) = IPRTXT(ipt+1)
          iary(3) = IPRTXT(ipt+2)
          if (iary(3) .eq. 0 .and. kary(3) .ne. 0) then
              if (iary(1) .eq. 4) then
                  iary(1) = 3
                  iary(2) = iary(2) + kary(3) - 1
              else if (iary(1) .eq. 1) then
                 iary(3) = kary(3)
              endif
          endif
          call pdfwvl (iary,IPRTXT(ipt+5),ldat,nc)
c
c......Place data in slot
c
  600     if (nc .eq. 0) go to 1900
          ist    = IPRTXT(ipt+3)
          ien    = IPRTXT(ipt+4)
          isub   = ien    - ist    + 1
c
c.........Room to spare
c
          if (nc .lt. isub) then
              inc    = IPRTXT(ipt+6)
              if (inc .eq. 1) then
                  ist    = ien    - nc     + 1
              else if (inc .eq. 2) then
                  ien    = ist    + nc     - 1
              else
                  ist    = ist    + (isub-nc) / 2
                  ien    = ist    + nc     - 1
              endif
c
c.........Overflow
c
          else if (nc .gt. isub) then
              inc    = IPRTXT(ipt+7)
              if (inc .eq. 1) then
                  ldat(isub:isub) = '>'
              else if (inc .eq. 2) then
                  sbuf   = '<' // ldat(nc-isub+2:nc)
                  ldat(1:isub) = sbuf
              else if (inc .eq. 3) then
                  ldat(1:isub) = lstar
              else
                  cout(ist:ien) = ldat(1:isub)
                  cbuf   = ldat(isub+1:nc)
                  kary(1) = nc     - isub
                  kary(2) = i
                  knco   = ien
                  kfl    = 1
                  go to 8000
              endif
              nc     = isub
          endif
c
c.........Store data
c
          cout(ist:ien) = ldat(1:nc)
          if (ien .gt. knco) knco = ien
 1900     ipt    = ipt    + 10
 2000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prthed (kfl)
c
c   FUNCTION:  This routine outputs the print file page header and
c              trailer.
c
c   INPUT:  kfl     I*4  D1  -  1 = Print page header.  2 = Page trail-
c                               er.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine prthed (kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IPAGE ,KPOSMP(1151)), (IPLINE,KPOSMP(1152))
      equivalence (IPRDES,KPOSMP(1154)), (NCHDR ,KPOSMP(1174))
      equivalence (IPRTOP,KPOSMP(1176))
c
      integer*4 IPAGE,IPLINE,IPRDES(2,10),NCHDR,IPRTOP
c
      equivalence (LHDR  ,CPOSMP(2733))
c
      character*132 LHDR
c
      integer*4 kfl
c
      integer*4 i,j,k,ifl,nc,iary(3),isub,inc,ipt,icnt,iprt,
     1          ierr,irec
c
      character*80 msg
      character*512 ldat,lbuf
c
c....Write out each line in record
c
      if (IOPFL(4) .eq. 0 .or. IPRTOP .eq. 0) go to 8000
      if (kfl .eq. 1) then
          IPAGE  = IPAGE  + 1
          IPLINE = 0
      endif
      ifl    = 0
      iprt   = 0
      do 500 i=1,2,1
          if (IPRDES(i,kfl) .eq. 0) go to 500
          irec   = IPRDES(i,kfl)
c
c......Output New Page and/or
c......Blank line if needed
c
          if (FRMFNC(4,irec) .eq. 1) then
              call wrtxt (LUNSC3,LFORMF,1,msg,ierr)
              if (ierr .ne. 0) go to 9000
          endif
          if (FRMFNC(5,irec) .eq. 1) then
              IPLINE = IPLINE + 1
              call wrtxt (LUNSC3,' ',1,msg,ierr)
              if (ierr .ne. 0) go to 9000
          endif
          ipt    = (irec-1) * 36 + 1
          do 300 j=1,FRMFNC(2,irec),1
c
c......Define number of times to output
c......the current line definition
c
c.........Post variable selected
c
              if (PRTLDS(ipt+3) .eq. 3) then
                  inc    = PRTLDS(ipt+4)
                  if (inc .lt. 0) then
                      icnt   = KPOSMP(-inc)
                  else
                      icnt   = POSMAP(inc)
                  endif
c
c.........Subscripted Post variable
c
              else if (PRTLDS(ipt+3) .eq. 4) then
                  inc    = PRTLDS(ipt+4)
                  isub   = PRTLDS(ipt+5)
                  if (isub .lt. 0) then
                      isub   = KPOSMP(-isub)
                  else
                      isub   = POSMAP(isub)
                  endif
c
                  if (inc .lt. 0) then
                      icnt   = KPOSMP(-inc+isub-1)
                  else
                      icnt   = POSMAP(inc+isub-1)
                  endif
c
c.........Single time
c
              else
                  icnt   = 1
              endif
c
c......Output internal header line
c
              if (j .eq. 2 .and. kfl .eq. 1) then
                  iprt   = 1
                  IPLINE = IPLINE + 1
                  call wrtxt (LUNSC3,LHDR,NCHDR,msg,ierr)
                  if (ierr .ne. 0) go to 9000
              endif
c
c......Output New Page and blank line
c......prior to formatted line
c
              if (PRTLDS(ipt) .eq. 1) then
                  call wrtxt (LUNSC3,LFORMF,1,msg,ierr)
                  if (ierr .ne. 0) go to 9000
              endif
              if (PRTLDS(ipt+1) .eq. 1) then
                  IPLINE = IPLINE + 1
                  call wrtxt (LUNSC3,' ',1,msg,ierr)
                  if (ierr .ne. 0) go to 9000
              endif
c
c......Format print line
c
              do 200 k=1,icnt,1
                  iary(3) = k
  100             call prtdat (irec,j,ldat,nc,lbuf,iary,ifl)
c
c......Output print line
c
                  IPLINE = IPLINE + 1
                  call wrtxt (LUNSC3,ldat,nc,msg,ierr)
                  if (ierr .ne. 0) go to 9000
c
c......Check for continuation line
c
                  if (ifl .eq. 1) go to 100
  200         continue
c
c......Output blank line after formatted line
c
              if (PRTLDS(ipt+2) .eq. 1) then
                  IPLINE = IPLINE + 1
                  call wrtxt (LUNSC3,' ',1,msg,ierr)
                  if (ierr .ne. 0) go to 9000
              endif
              ipt    = ipt    + 6
  300     continue
c
c......Output blank line after record
c
          if (FRMFNC(6,irec) .eq. 1) then
              IPLINE = IPLINE + 1
              call wrtxt (LUNSC3,' ',1,msg,ierr)
              if (ierr .ne. 0) go to 9000
          endif
  500 continue
c
c...Output internal header line
c...If not already output
c
      if (iprt .eq. 0 .and. kfl .eq. 1) then
          IPLINE = IPLINE + 1
          call wrtxt (LUNSC3,LHDR,NCHDR,msg,ierr)
          if (ierr .ne. 0) go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Error writing to print file
c
 9000 call errkil (msg,ierr)
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prtout (cdat,knc)
c
c   FUNCTION:  This routine writes a line to the print file.
c
c   INPUT:  cdat    C*n  D1  -  Text of line to write.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine prtout (cdat,knc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IPAGE ,KPOSMP(1151))
      equivalence (IPLINE,KPOSMP(1152)), (NTRAIL,KPOSMP(1175))
      equivalence (IPRTOP,KPOSMP(1176))
c
      integer*4 IPLINE,NTRAIL,IPRTOP,IPAGE
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc,ierr
c
      character*80 msg
c
c...Write print file trailer
c
      if (IOPFL(4) .eq. 1 .and. IPRTOP .eq. 1) then
          if (IPLINE+NTRAIL .ge. IOPFL(3) .and. IPAGE .ne. 0)
     1            call prthed (2)
c
c...Write print file header
c
          if (IPLINE .ge. IOPFL(3)) call prthed (1)
c
c...Write print file record
c
          nc     = knc
          if (nc .gt. 132) nc = 132
          call wrtxt (LUNSC3,cdat,nc,msg,ierr)
          if (ierr .ne. 0) go to 9000
          IPLINE = IPLINE + 1
      endif
c
c...End of routine
c
 8000 return
c
c...Error writing to print file
c
 9000 call errkil (msg,ierr)
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prtrec (krec)
c
c   FUNCTION:  This routine outputs an entire Print Descriptor record
c              to the print file.
c
c   INPUT:  krec    I*4  D2  -  Record numbers to output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine prtrec (krec)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IPRTOP,KPOSMP(1176))
c
      integer*4 IPRTOP
c
      integer*4 krec(2)
c
      integer*4 i,j,k,ifl,nc,iary(3),ipt,icnt
c
      real*8 rnum
c
      character*512 ldat,lbuf
c
c....Write out each line in record
c
      if (IOPFL(4) .eq. 0 .or. IPRTOP .eq. 0) go to 8000
      ifl    = 0
      do 500 i=1,2,1
          if (krec(i) .eq. 0) go to 500
c
c......Output New Page and/or
c......Blank line if needed
c
          if (FRMFNC(4,krec(i)) .eq. 1) call prthed (1)
          if (FRMFNC(5,krec(i)) .eq. 1) call prtout (' ',1)
          ipt    = (krec(i)-1) * 36 + 1
          do 300 j=1,FRMFNC(2,krec(i)),1
c
c......Define number of times to output
c......the current line definition
c
              call pdfgvl (PRTLDS(ipt+3),rnum)
              icnt   = rnum
c
c......Output New Page and blank line
c......prior to formatted line
c
              if (PRTLDS(ipt) .eq. 1) call prthed (1)
              if (PRTLDS(ipt+1) .eq. 1) call prtout (' ',1)
c
c......Format print line
c
              do 200 k=1,icnt,1
                  iary(3) = k
  100             call prtdat (krec(i),j,ldat,nc,lbuf,iary,ifl)
c
c......Output print line
c
                  call prtout (ldat,nc)
c
c......Check for continuation line
c
                  if (ifl .eq. 1) go to 100
  200         continue
c
c......Output blank line after formatted line
c
              if (PRTLDS(ipt+2) .eq. 1) call prtout (' ',1)
              ipt    = ipt    + 6
  300     continue
c
c......Output blank line after record
c
          if (FRMFNC(6,krec(i)) .eq. 1) call prtout (' ',1)
  500 continue
c
c...End of routine
c
 8000 return
      end
