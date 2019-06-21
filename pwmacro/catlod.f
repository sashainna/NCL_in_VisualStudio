c
c***********************************************************************
c
c   FILE NAME:  catlod.f
c   CONTAINS:
c               catlod  cv5lod  rdcatc  catrd5  gethdr  cl4sto  godlta
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        catlod.f , 24.3
c     DATE AND TIME OF LAST  MODIFICATION
c        07/28/14 , 09:19:41
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  catlod (cfnam,ktest,cmsg,kerr)
c
c   FUNCTION:  This routine reads an CATIA clfile record and converts it
c              to a neutral format stored in the binary scratch file.
c
c   INPUT:  cfnsm   C*n  D1  Input Catia clfile name.
c
c           ktest   I*4  D1  1 = Test to see if this is a valid clfile.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine catlod (cfnam,ktest,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      integer*4 kerr,ktest
c
      character*(*) cfnam,cmsg
c
      equivalence (ISN   ,KPOSMP(0001))
      integer*4 ISN

      character*8 cdum
      character*20 att(4)
c
      integer*2 inum(4),ibuf(1024),inbuf(1028),mxc
      integer*4 i,j,n,m,ipt,jindex,klen,lcon,last2,inc,nwds,nprs,
     -          nvl,irecl,jbuf(512),jnum(2),is1,is2,is4,irec,
     -          ipt2,icr,icpt,jbfo(512),jtmp(512),ifini,idid
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
      MULT   = 0
      nprs   = 0
      idid   = 0
c
c...Store default MULTAX mode
c...if this is an INCLUDed clfile
c
      if (INCLNO .gt. 0 .and. ktest .eq. 0) then
          call wrmult (MULT,0,icr,icpt,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
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
c...Read cl record if necessary
c
  100 nprs   = nprs   + 1
      if (ktest .eq. 1 .and. nprs .gt. 5) then
          if (idid .eq. 0) go to 9000
          go to 8000
      endif
      ipt    = ipt    + 1
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
      if (inbuf(3) .le. 0 .or. inbuf(3) .gt. 14000) go to 9000
c
c...Post word
c
      if (inbuf(3) .eq. 2000) then
          idid   = 1
          nvl    = inbuf(4)
          inc    = jindex (CNMAJ,nvl,CNNMAJ)
          if (inc .ne. 0) inbuf(4) = CAMAJ(inc)
c
          if (ipt2 .gt. last2 .and. lcon .eq. 0) go to 300
          if (ktest .eq. 0) then
              call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
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
          idid   = 1
          if (ktest .eq. 0) then
              call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          i     = j / 2
          do 550 n=6,i
              rbfo(n-5) = rbuf(n)
  550     continue
          nwds  = 7
c
c...Motion record
c
      else if (inbuf(3) .eq. 5000) then
          idid   = 1
          if (ktest .eq. 0) then
              call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          i     = j / 2
          do 850 n=3,i
              rbfo(n-2) = rbuf(n)
  850     continue
          nwds  = i - 2
          if (inbuf(4) .eq. 4) then
              call godlta (nwds,rbfo,MULT,gts)
              inbuf(4) = 5
          end if
          j     = MULT * 3 + 3
          do 855 i=1,j,1
              gts(i) = rbfo(nwds-j+i)
  855     continue
c
c...Multax record
c
      else if (inbuf(3) .eq. 9000 .and. inbuf(4) .eq. 2) then
          idid   = 1
          if (ktest .eq. 0) then
              call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          i     = j / 2
          do 950 n=1,i
              rbfo(n) = rbuf(n)
  950     continue
          nwds  = 0
          inbuf(4) = ibuf(4)
          MULT  = ibuf(4)
c
c...FINI record
c
      else if (inbuf(3) .eq. 14000) then
          idid   = 1
          ifini  = 1
          inbuf(4) = 1
          nwds   = 0
c
c...Unsupported record type
c...Store cldata as is
c
      else
          if (ktest .eq. 0) then
              call cl4sto (ipt,last2,irec,lcon,klen,jbuf,j,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
          nwds   = j / 2
          if (nwds .lt. 0 .or. nwds .gt. 256) go to 9000
          do 1050 n=1,nwds
              rbfo(n) = rbuf(n)
 1050     continue
      endif
 2000 ISN    = inbuf(1)
      inbuf(2) = ISN
      mxc    = nwds
      if (ktest .eq. 0) then
          do 2100 i=1,(nwds+1)*2,1
              if (icpt .eq. 128) then
                  icr = icr + 1
                  call wrprm (LUNSC1,icr,jtmp,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  icpt  = 0
              end if
              icpt  = icpt + 1
              jtmp(icpt) = jbfo(i)
 2100     continue
c
          ipt    = ipt - 1
          if (ifini .eq. 0) go to 100
          if (icpt .ne. 0) then
              icr = icr + 1
              call wrprm (LUNSC1,icr,jtmp,cmsg,kerr)
          end if
      else if (ifini .eq. 0) then
          go to 100
      endif
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
c
c...Error parsing file
c
 9000 if (ktest .eq. 1) kerr = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cv5lod (cfnam,ktest,cmsg,kerr)
c
c   FUNCTION:  This routine reads a CATIA V5 clfile record and converts it
c              to a neutral format stored in the binary scratch file.
c
c   INPUT:  cfnsm   C*n  D1  Input Catia clfile name.
c
c           ktest   I*4  D1  1 = Test to see if this is a valid clfile.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cv5lod (cfnam,ktest,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      integer*4 kerr,ktest
c
      character*(*) cfnam,cmsg
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLREC,KPOSMP(0002))
c
      integer*4 ISN,ICLREC
c
      character*1 lnum(8)
      character*20 att(4)
      character*66 ltxt
c
      integer*2 inum(4),ibuf(1024),inbuf(4),inbf(1024)
      integer*4 i,j,ipt,jindex,inc,nwds,ival,nvl,irecl,jbuf(512),
     1          jnum(2),is1,is4,js1,js2,irec,maxi,icr,icpt,
     2          jtmp(512),ifini,jinbf(512),jnbuf(2),idsrec,nprs
c
      real*8 rbuf(256),rnum,gts(6),rinbf(256),rtxt(9),dsval(6)
c
      equivalence (rbuf,jbuf,ibuf)
      equivalence (rtxt,ltxt), (rnum,inum,jnum,lnum)
      equivalence (rinbf,jinbf,inbf), (inbuf,jnbuf)
c
C VAX-SUN-SGI-IBM-HPX-START
C      data is1 / 1/, is4 / 4/
C      data js1 / 1/, js2 / 2/
C VAX-SUN-SGI-IBM-HPX-END
C WNT-DOS-DEC-START
      data is1 / 4/, is4 / 1/
      data js1 / 2/, js2 / 1/
C WNT-DOS-DEC-END
c
c
c...Initialze routine
c
      kerr   = 0
      irec   = 2
      ipt    = 0
      ifini  = 0
      LSTPTC = 0
      icpt   = 0
      icr    = 0
      gts(4) = 0.
      gts(5) = 0.
      gts(6) = 1.
      MULT   = 0
      nprs   = 0
c
c...Store default MULTAX mode
c...if this is an INCLUDed clfile
c
      if (INCLNO .gt. 0 .and. ktest .eq. 0) then
          call wrmult (MULT,0,icr,icpt,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Open Catia clfile
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      irecl  = 264
      maxi   = 66
      call opnfil (LUNSC2,cfnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call getfnm (LUNSC2,LCMPFI,NCCMPF,MAX_PATH)
c
c...Open scratch clfile
c
      if (ktest .eq. 0) then
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'scratch'
          irecl  = 512
          call opnfil (LUNSC1,'clfile.tmp',att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Get next logical record
c
      ipt    = maxi  + 1
  100 nprs   = nprs   + 1
      if (ktest .eq. 1 .and. nprs .gt. 2) go to 8000
      if (irec .eq. 1865) then
         nwds = 1
      endif
      call catrd5 (irec,inbuf,jinbf,ipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (inbuf(1) .lt. 0 .or. inbuf(1) .gt. 256) go to 9000
      if ((inbuf(3) .le. 0 .or. inbuf(3) .gt. 14000) .and.
     1    inbuf(3) .ne. 28000) go to 9000
      nwds   = inbuf(1)
c
c...ISN record
c
      if (inbuf(3) .eq. 1000) then
          ISN    = inbuf(4)
          ICLREC = inbuf(2)
          go to 100
c
c...Post word
c
      else if (inbuf(3) .eq. 2000) then
          nvl    = inbuf(4)
          inc    = jindex (CNMAJ,nvl,CNNMAJ)
          if (inc .ne. 0) inbuf(4) = CAMAJ(inc)
c
c......Store real/integer values
c......replacing minor words when necessary
c
          if (inbuf(4) .lt. 1043 .or. inbuf(4) .gt. 1046) then
              do 210 i=1,nwds,1
                  if (inbuf(4) .eq. 1015 .and. i .eq. 1) then
                      rnum   = rinbf(i)
                      if (jnum(js2) .eq. 109) then
                          lnum(1) = 'P'
                          lnum(2) = 'W'
                          lnum(3) = 'O'
                          lnum(4) = 'R'
                          lnum(5) = 'K'
                          lnum(6) = 'S'
                          lnum(7) = ' '
                          lnum(8) = ' '
                      endif
                  else
                      rnum   = rinbf(i)
                      if (jnum(js1) .eq. 0) then
                          ival   = jnum(js2)
                          inum(is1) = 0
                          inum(is4) = ival
                          inc    = jindex (CNMIN,ival,CNNMIN)
                          if (inc .ne. 0) inum(is4) = CAMIN(inc)
                      endif
                  endif
                  rbuf(i) = rnum
  210         continue
c
c......Store text (PPRINT, PARTNO, INSERT & LETTER)
c
          else
              inc    = 0
              do 260 i=1,nwds,1
                  rnum  = rinbf(i)
                  do 250 j=1,6,1
                      inc    = inc    + 1
                      ltxt(inc:inc) = lnum(9-j)
  250             continue
  260         continue
              if (nwds .lt. 11) ltxt(nwds*6:66) = ' '
              nwds   = 9
              do 270 i=1,nwds,1
                  rbuf(i) = rtxt(i)
  270         continue
          endif
c
c...Circular record
c
      else if (inbuf(3) .eq. 3000) then
          do 550 i=6,nwds,1
              rbuf(i-5) = rinbf(i)
  550     continue
          nwds  = 7
c
c...Motion record
c
      else if (inbuf(3) .eq. 5000) then
          do 850 i=3,nwds,1
              rbuf(i-2) = rinbf(i)
  850     continue
          nwds  = nwds   - 2
          if (inbuf(4) .eq. 4) then
              call godlta (nwds,rbuf,MULT,gts)
              inbuf(4) = 5
          endif
          j     = MULT * 3 + 3
          do 855 i=1,j,1
              gts(i) = rbuf(nwds-j+i)
  855     continue
c
c......Store drive surface
c......Contact point & Normal
c
          if (idsrec .eq. 1) then
              do 856 i=7,14,1
                  rbuf(i) = 0
  856         continue
              do 857 i=1,6,1
                  rbuf(i+15) = dsval(i)
  857         continue
              inbuf(3) = 5200
              nwds   = 21
          endif
          idsrec = 0
c
c...Multax record
c
      else if (inbuf(3) .eq. 9000 .and. inbuf(4) .eq. 2) then
          inbuf(4) = jinbf(js2)
          MULT  = inbf(is4)
c
c...Drive surface record
c
      else if (inbuf(3) .eq. 10000) then
          do 870 i=3,nwds,1
              dsval(i-2) = rinbf(i)
  870     continue
          idsrec = 1
c
c...FINI record
c
      else if (inbuf(3) .eq. 14000) then
          ifini  = 1
          nwds   = 0
          inbuf(4) = 1
c
c...Unmodified clfile record
c
      else
          do 900 i=1,nwds,1
              rbuf(i) = rinbf(i)
  900     continue
      endif
c
c...Store logical record
c
      if (ktest .eq. 0) then
          inbuf(1) = nwds
          inbuf(2) = ISN
          do 2100 i=1,(nwds+1)*2,1
              if (icpt .eq. 128) then
                  icr = icr + 1
                  call wrprm (LUNSC1,icr,jtmp,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  icpt  = 0
              end if
              icpt  = icpt + 1
              if (i .le. 2) then
                  jtmp(icpt) = jnbuf(i)
              else
                  jtmp(icpt) = jbuf(i-2)
              endif
 2100     continue
c
c...Write out last record
c
          if (ifini .eq. 0) go to 100
          if (icpt .ne. 0) then
              icr = icr + 1
              call wrprm (LUNSC1,icr,jtmp,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
      endif
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
c
c...Error parsing file
c
 9000 if (ktest .eq. 1) kerr = 1
      go to 8000
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
c   SUBROUTINE:  catrd5 (krec,kdes,kbuf,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine reads CATIA V5 clfile physical records and
c              breaks out the logical records.
c
c   INPUT:  krec    I*4  D1  Next clfile physical record to read.
c
c           kpt     I*4  D1  Pointer within physical record of logical
c                            record.  Should be set to record length + 1
c                            on the first call and will be modified by
c                            this routine thereafter.
c
c   OUTPUT: kdes    I*2  D4  Clfile record descriptor.  (1) = Number of
c                            of R*8s in record, (2) = Clfile record number,
c                            (3) = Record type, (4) = Record sub-type.
c
c           kbuf    I*1  Dn  Logical clfile record data.
c
c           kpt     I*4  D1  Pointer within physical record of logical
c                            record.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine catrd5 (krec,kdes,kbuf,kpt,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
c
      integer*2 kdes(4)
      integer*4 krec,kbuf(*),kerr,kpt
c
      character*(*) cmsg
c
      integer*4 iout,inum
c
c...Increment pointer
c
      if (kpt .ge. 66) then
          call rdcat5 (LUNSC2,krec,JCLBUF,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          krec   = krec   + 1
          kpt    = 0
      endif
c
c...Build the descriptor buffer
c
      kpt    = kpt    + 1
      kdes(2) = JCLBUF(kpt)
      kpt    = kpt    + 1
      kdes(1) = JCLBUF(kpt)
      if (kdes(1) .gt. 2048) go to 9000
      if (kpt .ge. 66) then
          call rdcat5 (LUNSC2,krec,JCLBUF,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          krec   = krec   + 1
          kpt    = 0
      endif
      kpt    = kpt    + 1
      kdes(4) = JCLBUF(kpt)
      kpt    = kpt    + 1
      kdes(3) = JCLBUF(kpt)
C SUN-SGI-IBM-HPX-START
C     inum   = kdes(1)
C     kdes(1) = kdes(2)
C     kdes(2) = inum
C     inum   = kdes(3)
C     kdes(3) = kdes(4)
C     kdes(4) = inum
C SUN-SGI-IBM-HPX-END
      kdes(1) = (kdes(1)-12) / 8
c
c...Loop through clfile record data
c
      iout   = 0
  100 if (iout .ge. kdes(1)*2) go to 8000
      if (kpt .ge. 66) then
          call rdcat5 (LUNSC2,krec,JCLBUF,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          krec   = krec   + 1
          kpt    = 0
      endif
c
c...Store clfile record data
c
      kpt    = kpt    + 1
      iout   = iout   + 1
      kbuf(iout) = JCLBUF(kpt)
      go to 100
c
c...End of routine
c
 8000 return
c
c...Error reading file
c
 9000 kerr   = 1
      go to 8000
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
      if (kpnt .gt. 808) go to 7800
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
