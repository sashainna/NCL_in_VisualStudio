c
c***********************************************************************
c
c   FILE NAME:  pchhdr
c   CONTAINS:
c               pchhdr  phfvdf
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pchhdr.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:09
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  pchhdr (cmsg,kerr)
c
c   FUNCTION:  This routine loads and compiles the Punch Header File,
c              and merges it with the punch file (which must already be
c              created).
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1     Text of error message.
c
c           kerr    I*4  D1     Returns nonzero when an error occurred.
c
c***********************************************************************
c
      subroutine pchhdr (kflg,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IPHFLO,KPOSMP(0369))
c
      integer*4 IPHFLO
c
      equivalence (PHFILE,CPOSMP(2535))
c
      character*40 PHFILE
c
      integer*4 kflg,kerr
c
      character*(*) cmsg
c
      integer*4 irecl,nc,strlen1,nca,ityp,inum,iloop,nloop,nrec,i,j,
     1          iary(10),ierr,idid,itrail,ifl,phdfnc(100),ferr
c
      real*8 rnum
c
      character*20 att(4)
      character*80 msg
      character*256 phdbuf(100)
      character*512 lbuf,ldat
      character*(MAX_PATH) fnam
c
      data att /'sequential','list','formatted','old'/
c
c...Initialize routine
c
      itrail = 0
      iloop  = 0
      ferr   = 0
      msg    = ' '
c
c...Open Punch File
c
      att(4) = 'new'
      irecl = 512
      call opnfil (LUNPRM,PCHFIL,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Rewind temporary Punch File
c
      endfile (LUNSC4)
      rewind (LUNSC4)
c
c...Open Punch Header File
c
      if (IPHFLO .eq. 1) then
          irecl  = 132
          nc     = strlen1(PHFILE)
          att(1) = 'sequential'
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'old'
          irecl  = 80
          fnam   = 'pworks_' // PHFILE(1:nc) // '.phf'
          call opnfil (LUNPHD,fnam,att,irecl,msg,ferr)
          if (ferr .eq. -2) then
              call fparse (fnam,fnam,DVDATA,'.phf')
              call opnfil (LUNPHD,fnam,att,irecl,msg,ferr)
          endif
          if (ferr .ne. 0) go to 1100
          IPHFLO = 2
      endif
c
c...Find Start of Header Information
c
  100 call rdtxt (LUNPHD,lbuf,cmsg,kerr)
      if (kerr .eq. 1) go to 1000
      if (kerr .ne. 0) go to 8000
      nc     = strlen1(lbuf)
c
c......Find Header Record
c
      call pdftyp (lbuf,nc,ldat,nca,ityp,inum,2)
      if (ityp .eq. 102) go to 1000
      if (ityp .ne. 101) go to 100
c
c...Insert header data
c
  200 call rdtxt (LUNPHD,lbuf,cmsg,kerr)
      if (kerr .eq. 1) go to 1000
      if (kerr .ne. 0) go to 8000
      nc     = strlen1(lbuf)
      idid   = 0
      call pdftyp (lbuf,nc,ldat,nca,ityp,inum,2)
      if (ityp .eq. 102) go to 1000
c
c......LOOPST
c
      if (ityp .eq. 103 .and. iloop .eq. 0) then
          call pdfrdf (ldat,nca,iary,ierr)
          if (iary(4) .ne. 0 .and. ierr .eq. 0) then
              iloop  = 1
              nrec   = 1
              idid   = 1
              call pdfgvl (iary(4),rnum)
              nloop  = rnum
              phdbuf(nrec) = lbuf
              phdfnc(nrec) = nc
          endif
c
c......LOOPND
c
      else if (ityp .eq. 104 .and. iloop .eq. 1) then
          do 500 i=1,nloop,1
              do 400 j=2,nrec,1
                  call phfvdf (phdbuf(j),phdfnc(j),i,ldat,nca)
                  call wrtxt (LUNPRM,ldat,nca,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
  400         continue
  500     continue
          iloop  = 0
          idid   = 1
      endif
c
c......Text Record
c
      if (idid .eq. 0) then
          if (iloop .ne. 0) then
              nrec   = nrec   + 1
              phdbuf(nrec) = lbuf
              phdfnc(nrec) = nc
          else
              call phfvdf (lbuf,nc,0,ldat,nca)
              call wrtxt (LUNPRM,ldat,nca,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
      endif
      go to 200
c
c...End of HEADER / TRAILER record
c...Make sure there is not an active loop
c
 1000 kerr   = 0
      if (iloop .eq. 1) then
          do 1050 i=1,nrec,1
              call phfvdf (phdbuf(i),phdfnc(i),0,ldat,nca)
              call wrtxt (LUNPRM,ldat,nca,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
 1050     continue
          iloop  = 0
      endif
c
c...End of TRAILER record
c
      if (itrail .eq. 1) go to 8000
c
c...End of HEADER record
c...Write out Punch File data
c
      ifl    = kerr
      kerr   = 0
 1100 call rdtxt (LUNSC4,lbuf,cmsg,kerr)
      if (kerr .eq. 1) go to 1200
      if (kerr .ne. 0) go to 8000
      nc     = strlen1(lbuf)
      call wrtxt (LUNPRM,lbuf,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 1100
c
c...End of Punch File
c...See if we need to write trailer data
c
 1200 kerr   = 0
      if (itrail .eq. 1 .or. ifl .ne. 0) go to 8000
      itrail = 1
      go to 200
c
c...End of routine
c
 8000 if (kflg .eq. 0) then
         call brknam (LUNPRM,cmsg,kerr)
         if (IPHFLO .eq. 2) rewind (LUNPHD)
         rewind (LUNSC4)
      else
         call clsfil (LUNPRM)
         if (IPHFLO .eq. 2) call clsfil (LUNPHD)
c        call clsfil (LUNSC4)
      endif
c
      if (ferr .ne. 0) then
          kerr   = ferr
          cmsg   = msg
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: phfvdf (cbuf,knc,kinc,cout,knco)
c
c   FUNCTION:  This routine parses a data line from the Punch Header
c              File and fills in all variable locations.
c
c   INPUT:  cbuf    C*n  D1  -  Input line.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c           kinc    I*4  D1  -  Contains the current subscript number
c                               if this line is in a looping region.
c                               Otherwise it contains 0.
c
c           klin    I*4  D1  -  Current line number.
c
c           knvar   I*4  D1  -  Number variables defined for this re-
c                               cord.
c
c   OUTPUT: cout    C*n  D1  -  Fully parsed line.
c
c           knco    I*4  D1  -  Number of characters in 'cout'.
c
c***********************************************************************
c
      subroutine phfvdf (cbuf,knc,kinc,cout,knco)
c
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 knc,kinc,knco
c
      character*(*) cbuf,cout
c
      integer*4 ist,ix,inc,inc1,nc,ncv,index,ityp,iary(10),inum,ierr
c
      real*8 rnum
c
      character*512 lbuf,ldat
c
c...Blank line
c
      if (knc .eq. 0) then
          cout   = ' '
          knco   = 0
          go to 8000
      endif
c
c...Initialize routine
c
      knco   = 0
      ist    = 1
c
c...Search for Post Variable '[var]'
c
  200 inc    = index(cbuf(ist:),'[')
      if (inc .eq. 0) go to 7000
      inc    = ist    + inc    - 1
      inc1   = index(cbuf(inc:),']')
      if (inc1 .eq. 0) go to 7000
      inc1   = inc    + inc1   - 1
      ldat   = cbuf(inc:inc1)
      nc     = inc1    - inc    + 1
c
c...Copy any text prior to start of variable
c
      if (inc .ne. ist) then
          cout(knco+1:) = cbuf(ist:inc-1)
          knco   = knco    + (inc-ist)
      endif
      ist    = inc1    + 1
c
c...Break out variable
c
      ix     = 1
  500 call pdfbrk (ldat,ix,nc,lbuf,ncv)
      if (nc .eq. 0) then
          ierr   = 1
      else
          ityp   = 1
          call pdfwrd (lbuf,ncv,iary,ierr)
      endif
c
c......Invalid variable descriptor
c
      if (ierr .eq. 1) then
          cout(knco+1:) = ldat(1:nc)
          knco   = knco   + nc
          go to 200
      endif
c
c......Asterisk (*) subscript within loop
c
      if (iary(3) .eq. 0 .and. kinc .ne. 0) then
          if (iary(1) .eq. 4) then
              iary(1) = 3
              iary(2) = iary(2) + kinc   - 1
          else if (iary(1) .eq. 1) then
              iary(3) = kinc
          endif
      endif
c
c......Get format or word placement
c
      call pdfbrk (ldat,ix,nc,lbuf,ncv)
      inum   = 0
c
c.........Format
c
      if (ncv .ne. 0) then
          call ctocd (lbuf,ncv,inum,rnum,ierr)
c
c.........Word placement
c
          if (ierr .eq. 1 .or. inum .le. 0 .or. rnum .ne. DUMMY) then
              call ctoi (lbuf(1:ncv),inum,ierr)
              if (ierr .eq. 1 .or. inum .le. 0) then
                  cout(knco+1:) = ldat(1:nc)
                  knco   = knco   + nc
                  go to 200
              endif
              inum   = -inum
          endif
      endif
c
c......Get Variable's value
c
      call pdfwvl (iary,inum,lbuf,ncv)
      cout(knco+1:) = lbuf(1:ncv)
      knco   = knco   + ncv
      go to 200
c
c...Last parameter found
c...Copy remainder of text
c
 7000 if (ist .le. knc) then
          cout(knco+1:) = cbuf(ist:knc)
          knco   = knco   + (knc-ist+1)
      endif
c
c...End of routine
c
 8000 return
      end
