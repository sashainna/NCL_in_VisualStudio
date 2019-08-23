C***********************************************************************
C*    NAME         :  clfile.f
C*       CONTAINS:
C*					clinit  getcln  setclf  setas  clsave  clfile  clwrit
C*					clwr    clload  clrev
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        clfile.for , 26.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*        09/26/18 , 12:49:56
C***********************************************************************
C
c***********************************************************************
c
c   SUBROUTINE:  clinit
c
c   FUNCTION:  FORTRAN routine for opening the internal clfile.  The
c              record pointer for this clfile is stored in I4STAT(2).
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clinit
c
      include 'com8a.com'
      include 'mocom.com'
c
      integer*2 iclf
c
c...Initialize the clfile storage
c
      iclf = 0
      call clopen(iclf,imotp)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getcln (fname,nc)
c
c   FUNCTION:  Interface to C routines for retrieving the clfile name.
c              flag (IFL(69)) and for saving the cl disk file name in
c              CLFNAM.
c
c   INPUT:  none
c
c   OUTPUT: fname   C*1024 D1  -  CL disk file name.
c
c           nc      I*4  D1  -  Number of chars in 'fname'.
c
c***********************************************************************
c
      subroutine getcln (fname,nc)
c
      include 'com8a.com'
c
      integer*4 nc,strlen1
      character*(MAX_PATH) fname
c
c...Return clfile name
c
      fname = clfnam
      nc = strlen1(clfnam)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setclf (fname,ncf,kfl)
c
c   FUNCTION:  Interface to C routines for setting the "Save clfile"
c              flag (IFL(69)) and for saving the cl disk file name in
c              CLFNAM.
c
c   INPUT:  fname   C*1024 D1  -  CL disk file name.
c
c           ncf     I*4  D1  -  Number of chars in 'fname'.
c
c           kfl     I*2  D1  -  Save clfile flag.  1 = Yes.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine setclf (fname,ncf,kfl)
c
      include 'com8a.com'
c
      integer*2 kfl
      integer*4 ncf
      character*(MAX_PATH) fname
c
c...Save clfile name
c
      if (ncf .le. 0) then
          clfnam = " "
      else
          clfnam = fname(1:ncf)
      endif
      ifl(69) = kfl
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getapn (fname,nc)
c
c   FUNCTION:  Interface to C routines for retrieving the APT source name.
c
c   INPUT:  none.
c
c   OUTPUT: fname   C*1024 D1  -  APT source disk file name.
c
c           nc      I*4  D1  -  Number of characters in 'fname'.
c
c***********************************************************************
c
      subroutine getapn (fname,nc)
c
      include 'com8a.com'
c
      integer*4 nc,strlen1
      character*(MAX_PATH) fname
c
c...Get Apt Source file name
c
      fname = asfnam
      nc = strlen1(asfnam)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setas (fname,ncf,kfl)
c
c   FUNCTION:  Interface to C routines for setting the "Save APT source
c              file" flag (IFL(88)) and for saving the APT source disk
c              file name in ASFNAM.
c
c   INPUT:  fname   C*1024 D1  -  APT source disk file name.
c
c           ncf     I*4  D1  -  Number of chars in 'fname'.
c
c           kfl     I*2  D1  -  Save APT source file flag.  1 = Yes.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine setas (fname,ncf,kfl)
c
      include 'com8a.com'
c
      integer*2 kfl
      integer*4 ncf
      character*(MAX_PATH) fname
c
c...Save Apt Source file name
c
      if (ncf .le. 0) then
          asfnam = " "
      else
          asfnam = fname(1:ncf)
      endif
      ifl(88) = kfl
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clsave (cfil,kst,ken,kerr)
c
c   FUNCTION:  This routine opens a cl disk file and saves the internal
c              clfile to it.
c
c   INPUT:  cfil    C*1024 D1  -  Name of disk file to create.
c
c           kst     I*4  D1  -  Beginning clfile record for clfile
c                               creation.
c
c           ken     I*4  D1  -  Ending clfile record.
c
c   OUTPUT: kerr    I*2  D1  -  Returns 1 if an error occurred.
c
c***********************************************************************
c
      subroutine clsave (cfil,kst,ken,kerr)
c
      include 'com8a.com'
c
      character*(MAX_PATH) cfil
      integer*4 kst(2),ken(2)
      integer*2 kerr
c
      integer*2 iclf
      character*(MAX_PATH) tfil
c
      call flname (6,cfil,tfil)
      call flopnw (cllun,tfil,'DIRECT','UNFORMATTED',288,'NULL',
     1             kerr)
      if (kerr .ne. 0) go to 8000
c
      iclf = 0
      call clwrit (iclf,tfil,kst,ken)
      close (unit=cllun)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clfile
c
c   FUNCTION:  This routine saves both the clfile and APT source files
c              to disk.  It should be called upon exiting NCLCAM.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clfile
c
      include 'com8a.com'
c
      integer*2 iclf
      integer*4 irst(2),iren(2)
c
      character*(MAX_PATH) clnam
c
      iclf   = 0
      call ncl_zroptr(irst)
      call ncl_zroptr(iren)
c
c...Reverse clfile if necessary
c
      if (ifl(367) .eq. 1) call clrev (iclf,irst,iren)
      if (ifl(69) .eq. 0) go to 1000
c
c...Open clfile
c
      call aptcl (clnam)
      call getclf (clnam)
c
c...Save clfile
c
      call ncl_zroptr(irst)
      call ncl_zroptr(iren)
      call clwrit (iclf,clnam,irst,iren)
c
c...Output Apt Source File
c
 1000 call ncl_zroptr(irst)
      call ncl_zroptr(iren)
      call aptsrc (1,asfnam,iclf,irst,iren)
c
c...End of routine
c
 8000 close (unit=cllun)
      call clclos (iclf)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clwrit (iclf,clnam,kst,ken)
c
c   FUNCTION:  This routine writes out an internal clfile to a disk
c              file.  The disk file must already be open using the
c              CLLUN logical unit number.
c
c   INPUT:  iclf    I*2  D1  -  Specifies which internal clfile to write
c                               out.  1 = Primary clfile (usually
c                               created by the current part program).
c                               2 = Secondary clfile (usually loaded
c                               from an external clfile for back plot-
c                               ting).
c
c           clnam   C*1024 D1  -  Output clfile name.
c
c           kst     I*4  D1  -  Beginning clfile record for clfile
c                               creation.
c
c           ken     I*4  D1  -  Ending clfile record.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clwrit (iclf,clnam,kst,ken)
c
      include 'com8a.com'
      include 'cutter.com'
      include 'mocom.com'
c
      integer*2 iclf
      integer*4 kst(2),ken(2)
c
      character*(MAX_PATH) clnam
c
      integer*2 ibuf(4),iclbuf(144),idata(480),npts
      integer*2 ipt,icnt,iflg,iflg1
      integer*4 irec,inc,icpt(2),i,j,k,n,iclw(6),jclbuf(72),nc,strlen1
c
      real*8 rdata(420),dclbuf(36)
c
      character*8 lclt,lclnm,lclrv
      character*11 lcld
      character*48 cclbuf
      character*80 lclf
c
      equivalence (dclbuf,iclbuf,jclbuf)
      equivalence (dclbuf(2),cclbuf)
      equivalence (rdata,ibuf), (rdata,idata)
      equivalence (rdata(2),lclf), (rdata(12),lcld), (rdata(14),lclt)
      equivalence (rdata(15),lclnm), (rdata(16),lclrv)
c
c...Initialize routine
c
c      icpt   = kst
      call ncl_setptr(kst,icpt)
      inc    = 0
      irec   = 0
      npts   = 3
c
c...Store Clfile name and Date record
c
      lclf   = clnam
      call ncdate (lcld)
      call nctime (lclt)
      lclnm  = 'NCLCAM'
      write (lclrv,10) sc(119)
   10 format (f8.2)
      ibuf(1) = 17
      ibuf(2) = 7400
      ibuf(3) = 0
      ibuf(4) = 0
      n      = 0
      call clwr (rdata,ibuf(1),clbuff,irec,inc,n)
c
c...Not starting from the beginning
c...Store previous MULTAX,CUTTER,FEDRAT & GOTO
c
c      if (kst .ne. 0) then
      call ncl_tstptr (kst,iflg)
      if (iflg .ne. 0) then
          call clread (iclf,icpt,iclw,rdata(2),jerr)
          if (jerr .eq. 1) go to 100
          if (iclw(3) .ne. 7200) then
              call gclinf (rdata(5),rdata(11),rdata(33),npts)
c              icpt   = kst
              call ncl_setptr(kst,icpt)
              idata(113) = 0
              idata(126) = 0
              if (npts .eq. 6) idata(126) = 1
          else
              ibuf(1) = iclw(5) + 1
              ibuf(2) = iclw(3)
              ibuf(3) = iclw(4)
              ibuf(4) = iclw(1)
              call clwr (rdata,ibuf(1),clbuff,irec,inc,iclw(1))
          endif
c
          iclbuf(2) = 2000
          iclbuf(3) = 1015
          iclbuf(4) = 0
          n      = 0
          nc     = strlen1(CPSTNM)
          if (nc .gt. 8) then
              iclbuf(1) = (nc+7) / 8 + 2
              jclbuf(3) = -1
              jclbuf(4) = nc
              cclbuf(9:nc+8) = CPSTNM(1:nc)
          else
              iclbuf(1) = 2
              cclbuf(1:nc) = CPSTNM(1:nc)
              if (CPSTNM .eq. 'PWORKS') then
                  dclbuf(3) = PSTNUM
                  iclbuf(1) = 3
              endif
          endif
          call clwr (dclbuf,iclbuf(1),clbuff,irec,inc,n)
c
          iclbuf(1) = 1
          iclbuf(2) = 9000
          iclbuf(3) = 1
          npts   = 3
          if (idata(126) .eq. 1) then
              iclbuf(3) = 0
              npts   = 6
          endif
          iclbuf(4) = 0
          call clwr (dclbuf,iclbuf(1),clbuff,irec,inc,n)
c
          if (iclbuf(113) .eq. 0 .or. iclbuf(113) .eq. 2 .or.
     1        iclbuf(113) .eq. 3) then
              ipt    = 11
          else
              ipt    = 17
          endif
          do 20 i=ipt+5,ipt,-1
              if (rdata(i) .ne. 0) go to 25
   20     continue
          i      = ipt
   25     icnt   = i - ipt + 1
          iclbuf(1) = icnt   + 1
          iclbuf(2) = 6000
          iclbuf(3) = 0
          iclbuf(4) = 0
          do 26 i=1,icnt,1
              dclbuf(i+1) = rdata(ipt+i-1)
   26     continue
          call clwr (dclbuf,iclbuf(1),clbuff,irec,inc,n)
c
          if (rdata(33) .gt. 0.) then
              iclbuf(1) = 2
              iclbuf(2) = 2000
              iclbuf(3) = 1009
              iclbuf(4) = 0
              dclbuf(2) = rdata(33)
              call clwr (dclbuf,iclbuf(1),clbuff,irec,inc)
          endif
c
          iclbuf(1) = npts   + 1
          iclbuf(2) = 5000
          iclbuf(3) = 3
          iclbuf(4) = 0
          dclbuf(2) = rdata(5)
          dclbuf(3) = rdata(6)
          dclbuf(4) = rdata(7)
          dclbuf(5) = rdata(8)
          dclbuf(6) = rdata(9)
          dclbuf(7) = rdata(10)
          call clwr (dclbuf,iclbuf(1),clbuff,irec,inc,n)
      endif
c
c...Save clfile
c
  100 call clread (iclf,icpt,iclw,rdata(2),jerr)
      if (jerr .eq. 1) go to 7000
      if (iclw(3) .eq. 5100 .or. iclw(3) .eq. 5300) go to 200
      if (iclw(3) .eq. 2000 .and. iclw(4) .eq. 1091) go to 200
      if (.not. lexpcl .and. iclw(3) .eq. 1000) go to 200
c
c...Output expanded motion record (5200) as 5000 record if flag is not set
c
      if (.not. lexpcl .and. iclw(3) .eq. 5200) then
        iclw(3) = 5000
        if (iclw(4) .ne. 3 .and. iclw(4) .ne. 5 .and. iclw(4) .ne. 6)
     1          iclw(4) = 5
        j = 1
        k = 1
        n = iclw(5) / 21
        iclw(5) = n * npts
        do 120 i=2,n
          j=j+npts
          k=k+21
          rdata(j+1) = rdata(k+1)
          rdata(j+2) = rdata(k+2)
          rdata(j+3) = rdata(k+3)
          if (npts.gt.3) then
            rdata(j+4) = rdata(k+4)
            rdata(j+5) = rdata(k+5)
            rdata(j+6) = rdata(k+6)
          endif
  120   continue
      endif
c
c...Multax
c
      if (iclw(3) .eq. 9000) then
        if (iclw(4).eq.0) then
          npts = 6
        else
          npts = 3
        endif
      endif
c
c...Ignore 5210/5220
c...If not expanded clfile
c
      if (lexpcl .or. iclw(3) .lt. 5200 .or. iclw(3) .ge. 5300) then
c
c...Write out clfile record
c
          ibuf(1) = iclw(5) + 1
          ibuf(2) = iclw(3)
          ibuf(3) = iclw(4)
          ibuf(4) = iclw(1)
          call clwr (rdata,ibuf(1),clbuff,irec,inc,iclw(1))
      endif
c
c...End of loop
c
c  200 if (icpt .ne. 0 .and. icpt .ne. ken) go to 100
  200 call ncl_tstptr (icpt,iflg)
      call ncl_eqlptr (icpt,ken,iflg1)
      if (iflg .ne. 0 .and. iflg1 .ne. 1) go to 100
c
c...Add FINI card
c
 7000 ibuf(1) = 1
      ibuf(2) = 14000
      ibuf(3) = 0
      call isn4i2 (nline,ibuf(4))
      inc    = inc    + 1
      clbuff(inc) = rdata(1)
      call clput (irec,clbuff)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clwr (gdata,knc,gbuff,krec,kinc,kisn)
c
c   FUNCTION:  This routine buffers logical clfile records and outputs
c              the physical record when full.
c
c   INPUT:  gdata   R*8  Dn  -  Logical clfile record to buffer.
c
c           knc     I*2  D1  -  Size of 'gdata'.
c
c           gbuff   R*8  Dn  -  Physical buffer used to store logical
c                               records.
c
c           krec    I*4  D1  -  Current clfile record.
c
c           kinc    I*4  D1  -  Pointer inside 'gbuff'.
c
c           kisn    I*4  D1  -  I*4 version of ISN for type 1000 record.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clwr (gdata,knc,gbuff,krec,kinc,kisn)
c
      integer*2 knc
      integer*4 krec,kinc,kisn
c
      real*8 gdata(36),gbuff(36)
c
      integer*2 i,idata(4)
      integer*4 jdata(2)
c
      real*8 rdata
c
      equivalence (idata,jdata,rdata)
c
c...Store ISN record
c
      rdata = gdata(1)
      if (kisn .gt. 32767) then
          idata(1) = 1
          idata(2) = 1001
          jdata(2) = kisn
          kinc   = kinc   + 1
          gbuff(kinc) = rdata
          if (kinc .eq. 35) then
              call clput (krec,gbuff)
              krec   = krec   + 1
              kinc   = 0
          endif
      endif
c
c...Store clfile record
c
      do 200 i=1,knc,1
          kinc   = kinc   + 1
          gbuff(kinc) = gdata(i)
          if (kinc .eq. 35) then
              call clput (krec,gbuff)
              krec   = krec   + 1
              kinc   = 0
          endif
  200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clload (iclf,clnam,jfl,ierr)
c
c   FUNCTION:  This routine reads an NCL clfile record and converts it
c              to a neutral format.
c
c   INPUT:  iclf    I*2  D1  Specifies which internal clfile to load.
c                            1 = Primary, 2 = Secondary.
c
c           clnam   C*1024 D1  Name of disk clfile to load.
c
c           nci     I*4  D1  Number of chars in 'clnam'.
c
c           jfl     I*2  D1  0 = Load a clfile for back plotting.  1 =
c                            Load a clfile after interactively editing
c                            it (removes the FINI card and deletes the
c                            disk clfile).
c
c   OUTPUT: ierr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine clload (iclf,clnam,nci,jfl,ierr)
c
      include 'com8a.com'
c
      integer*2 iclf,jfl,ierr
      integer*4 nci
      character*(MAX_PATH) clnam
      character*(MAX_PATH) cln
c
      integer*2 ibuf(144),nc
      integer*4 krec,kpt,ipt,i,iclpt(2),iclw(6),jisn,jbuf(72)
c
      real*8 rbuf(36),rclw(420)
c
      character*20 lclw
c
      equivalence (rbuf,ibuf,jbuf)
      equivalence (rclw,lclw)
c
c...Initialze routine
c
      ierr   = 0
      krec   = 0
      kpt    = 100
c
c...Open external clfile
c
      call flname (6,clnam(1:nci),cln)
      call flopen (cllun,cln,'OLD','DIRECT','UNFORMATTED',288,'NULL',
     1             ierr)
      if (ierr .ne. 0) go to 8000
c
c...Allocate internal clfile storage
c
      call clopen (iclf,iclpt)
c
c...Read cl record if necessary
c
  100 kpt    = kpt    + 1
      if (kpt .ge. 36) then
          krec   = krec   + 1
          kpt    = 1
          read (cllun,rec=krec,err=9000) rbuf
      endif
c
c...Store ISN
c
      ipt    = (kpt*4) - 3
      if (ibuf(ipt+1) .eq. 1001) then
          jisn   = jbuf(kpt*2)
          go to 100
      endif
c
c...Store integer values
c
      iclw(1) = ibuf(ipt+3)
      if (jisn .ne. 0) iclw(1) = jisn
      iclw(2) = iclw(1)
      iclw(3) = ibuf(ipt+1)
      iclw(4) = ibuf(ipt+2)
      iclw(5) = ibuf(ipt) - 1
      if (iclw(5) .gt. 420 .or. iclw(5) .lt. 0) go to 9000
      if (iclw(3) .ne. 1000) jisn    = 0
c
c...Store real values
c
      do 200 i=1,iclw(5),1
          kpt    = kpt    + 1
          if (kpt .ge. 36) then
              krec   = krec   + 1
              kpt    = 1
              read (cllun,rec=krec,err=9000) rbuf
          endif
          rclw(i) = rbuf(kpt)
  200 continue
c
c...SEQUNC record
c...Save pointer to it
c
      if (iclw(3) .eq. 7200 .and. iclw(4) .eq. 1) then
          do 300 i=1,20,1
              if (lclw(i:i) .eq. char(0) .or. lclw(i:i) .eq. ' ')
     1            go to 320
  300     continue
          i = 21
  320     nc = i - 1
          call seqsto (iclf,lclw,nc)
      endif
c
c...Ignore FINI records on reload after edit
c...and when restoring a session
c
      if (iclw(3) .eq. 14000 .and. jfl .eq. 1) go to 8000
      if (iclw(3) .eq. 14001) go to 8000
c
c...Save cl record
c
      call clstor (iclf,iclpt,iclw,rclw)
      if (iclw(3) .ne. 14000) go to 100
c
c...End of routine
c
 8000 if (jfl .eq. 1) then
          close (unit=cllun,status='delete')
      else
          close (unit=cllun)
      endif
      return
c
c...Error loading clfile
c
 9000 ierr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clrev (kclf,kst,ken)
c
c   FUNCTION:  This routine reverses the order of user specified sections
c              of the clfile using the REVERS/ON and REVERS/OFF commands.
c
c   INPUT:  kclf    I*2  D1  Specifies which internal clfile to load.
c                            Should be set to 0 = Primary.
c
c           kst     I*4  D1  Beginning record of clfile to process.
c
c           ken     I*4  D1  Ending record of clfile to process.
c
c   OUTPUT: kclf    I*2  D1  Returns 1 = Secondary clfile, when there
c                            were reversed clfile records.
c
c***********************************************************************
c
      subroutine clrev (kclf,kst,ken)
c
      include 'com8a.com'
c
      integer*2 kclf
      integer*4 kst(2),ken(2)
c
      integer*2 NSTK
c
      parameter (NSTK=4)
c
      integer*4 inc,i,j,icir,nent(10),i1,i2,ifed,ismod(10),icut,
     1          irec(2),modrec(2,10),lstrec(2),irvst(2),iclw(6),
     2          irven(2),icpt(2),endrec(2,10),strec(2,10),iorec(2),
     3          tmpclw(6),irap
      integer*2 idata(480),npts,icont,tdata(480),
     1          ioclf,irev,idid,is1,is4,iend,nc,iflg,iflg1,
     2          iwrd(10)
c
 
c
      real*8 rdata(420),tmpdat(420)
c
      character*20 lclw
c
      equivalence (rdata,idata), (tmpdat,tdata)
      equivalence (rdata(2),lclw)
c
      data iwrd /0, 1009, 1007, 5, 0,0,0,0,0,0/
c
      data is1 /4/, is4 /1/
c
c...Initialize routine
c
      i1     = 1
      i2     = 2
      call ncl_setptr(kst,icpt)
      idid   = 0
      irev   = 0
      call ncl_zroptr(irec)
      npts   = 3
c
c...Initialize stacks
c
      icir   = 1
      ifed   = 2
      icut   = 3
      irap   = 4
      do 20 i=1,NSTK,1
          nent(i) = 0
          ismod(i) = 0
          call ncl_zroptr(strec(1,i))
          call ncl_zroptr(endrec(1,i))
          call ncl_zroptr(modrec(1,i))
   20 continue
      call inirvl
c
c...Initialize secondary clfile
c
      ioclf  = 1
      call clopen (ioclf,iorec)
c
c...Reverse clfile
c
  100 if (irev .ne. 2) then
          call ncl_setptr(icpt,lstrec)
          call clread (kclf,icpt,iclw,rdata(2),jerr)
          if (jerr .eq. 1) go to 8000
      else
          call ncl_setptr (irec,lstrec)
          call clprev (kclf,irec,iclw,rdata(2),jerr)
          if (jerr .eq. 1) go to 8000
      endif
c
c...SEQUNC record
c...Save pointer to it
c
      if (iclw(3) .eq. 7200 .and. iclw(4) .eq. 1) then
          do 200 i=1,20,1
              if (lclw(i:i) .eq. char(0) .or. lclw(i:i) .eq. ' ')
     1            go to 220
  200     continue
          i = 21
  220     nc = i - 1
          call seqsto (ioclf,lclw,nc)
      endif
c
c...Multax
c
      if (iclw(3) .eq. 9000) then
        if (iclw(4).eq.0) then
          npts = 6
        else
          npts = 3
        endif
      endif
c
c...REVERS/ON
c
      if (ifl(351) .eq. 1 .and. iclw(3) .eq. 2000 .and.
     1    iclw(4) .eq. 1008) then
          if (iclw(5) .eq. 1 .and. idata(is4+4) .eq. 71) then
              if (irev .ne. 0) go to 100
              irev   = 1
              call ncl_setptr (icpt,irven)
              do 240 i=1,NSTK,1
                  ismod(i) = 0
  240         continue
              call ncl_tstptr(modrec(1,ifed),iflg)
              if (iflg .ne. 0)
     1            call pshrvl (modrec(1,ifed),ifed,nent(ifed),i1)
              call ncl_tstptr(modrec(1,icut),iflg)
              if (iflg .ne. 0)
     1            call pshrvl (modrec(1,icut),icut,nent(icut),i1)
              go to 100
          endif
c
c...REVERS/OFF
c
          if (iclw(5) .eq. 1 .and. idata(is4+4) .eq. 72) then
              if (irev .ne. 1) go to 100
              irev   = 2
              call ncl_setptr (icpt,irvst)
              call ncl_setptr (irvst,irec)
              idid   = 1
              icont  = 0
              call clprev (kclf,irec,iclw,rdata(2),jerr)
              if (jerr .eq. 1) go to 8000
c
c......Get the special record stack pointers
c
              do 250 i=1,NSTK,1
                  ismod(i) = 0
                  call poprvl (i,nent(i),strec(1,i),endrec(1,i))
                  if (i .eq. ifed .or. i .eq. icut) then
                      call ncl_tstptr(strec(1,i),iflg)
                      if (iflg .ne. 0) then
                          call clread (kclf,strec(1,i),tmpclw,tmpdat(2),
     1                        jerr)
                          call clstor (ioclf,iorec,tmpclw,tmpdat(2))
                          call poprvl (i,nent(i),strec(1,i),endrec(1,i))
                      endif
                  endif
  250         continue
              go to 100
          endif
      endif
c
c...Reverse pass in effect
c
      if (irev .eq. 2) then
c
c......Check for FEDRAT, CUTCOM, and RAPID output
c
          do 260 i=2,NSTK,1
              call ncl_eqlptr(irec,endrec(1,i),iflg)
              if (iflg .eq. 1) then
                  call ncl_setptr(irec,modrec(1,i))
                  ismod(i) = 1
                  if (i .ne. irap) go to 2000
              else if (iclw(3) .eq. 2000 .and. iclw(4) .eq. iwrd(i))
     1                then
c
c......No previous CUTCOM record
c......Assume CUTCOM/OFF
c
                  if (i .eq. icut) then
                      if (idata(is4+4) .eq. 8 .or. idata(is4+4) .eq. 24)
     1                        then
                          idata(is4+4) = 72
                          iclw(5) = 1
                          call clstor (ioclf,iorec,iclw,rdata(2))
                      endif
                  endif
                  go to 2000
              endif
  260     continue
c
c......Circular record
c
          if (iclw(3) .eq. 3000) then
              call ncl_eqlptr(lstrec,strec(1,icir),iflg)
              if (iflg .eq. 1) then
                  ismod(icir) = 1
                  call poprvl (icir,nent(icir),strec(1,icir),
     x                                        endrec(1,icir))
              endif
              go to 2000
          endif
c
c......Motion record
c
          if (iclw(3) .eq. 5000 .or. iclw(3) .eq. 5200) then
              np    = npts
              if (iclw(3) .ge. 5200) np = 21
c
c.........Check for special records on stack
c............Circular
c............Feedrate (motion prior to)
c
              call ncl_eqlptr(irec,endrec(1,icir),iflg)
              if (iflg .eq. 1 .or. ismod(ifed) .eq. 1 .or.
     1            ismod(icut) .eq. 1 .or. ismod(irap) .eq. 1) then
c
c...............Use last point on circle as starting point
c
                  if (iclw(5) .gt. np) then
c
c...............Output appropriate mode record
c
                      do 330 i=2,NSTK,1
                          if (ismod(i) .eq. 1) then
                              call clread (kclf,strec(1,i),tmpclw,
     1                                     tmpdat(2),jerr)
c
c.................Reverse CUTCOM direction
c
cc                              if (i .eq. icut) then
cc                                  if (tdata(is4+4) .eq. 8) then
cc                                      tdata(is4+4) = 24
cc                                  else if(tdata(is4+4) .eq. 24) then
cc                                      tdata(is4+4) = 8
cc                                  endif
cc                              endif
c
                              call clstor (ioclf,iorec,tmpclw,tmpdat(2))
                              call poprvl (i,nent(i),strec(1,i),
     1                                     endrec(1,i))
                              ismod(i) = 0
                          endif
  330                 continue
c
c..............Output motion record
c
                      do 340 i=1,6,1
                          tmpclw(i) = iclw(i)
  340                 continue
                      tmpclw(4) = 5
                      tmpclw(5) = np
                      do 310 i=1,np,1
                          tmpdat(i+1) = rdata(iclw(5)-np+i+1)
  310                 continue
                      call clstor (ioclf,iorec,tmpclw,tmpdat(2))
                      iclw(5) = iclw(5) - np
c
c...............Output circle record
c
                      call ncl_eqlptr(irec,endrec(1,icir),iflg)
                      if (iflg .eq. 1) then
                          call clread (kclf,strec(1,icir),tmpclw,
     1                                 tmpdat(2),jerr)
                          call clstor (ioclf,iorec,tmpclw,tmpdat(2))
                          ismod(icir) = 0
                      endif
                  endif
              endif
c
c.........Reverse motion
c
              if (iclw(5) .gt. np) then
                  iend   = iclw(5) - np
                  inc    = iend   + 1
                  do 500 i=1,iend/2,np
                      do 400 j=1,np,1
                          rnum   = rdata(i+j)
                          rdata(i+j) = rdata(inc+j)
                          rdata(inc+j) = rnum
  400                 continue
                      inc    = inc    - np
  500             continue
              endif
c
c.........Handle continuation records
c
              if (icont .eq. 0 .and. iclw(4) .eq. 6) then
                  iclw(4) = 5
                  icont = 1
              else if (icont .eq. 1 .and. iclw(4) .ne. 6) then
                  iclw(4) = 6
                  icont = 0
              endif
c
c.........Post-Circular
c
              if (ismod(icir) .eq. 1) then
c
c............Use last point as starting point of circle
c
                  if (iclw(5) .gt. np) then
                      do 600 i=1,6,1
                          tmpclw(i) = iclw(i)
  600                 continue
                      tmpclw(4) = 6
                      tmpclw(5) = np
                      do 610 i=1,np,1
                          tmpdat(i+1) = rdata(iclw(5)-np+i+1)
  610                 continue
                      call clstor (ioclf,iorec,tmpclw,tmpdat(2))
                      iclw(5) = iclw(5) - np
c
c............Single point record
c
                  else
                      iclw(4) = 6
                  endif
                  ismod(icir) = 0
              endif
c
c.........Output buffered mode records
c
              do 630 i=2,NSTK,1
                  if (ismod(i) .eq. 1) then
                      call clread (kclf,strec(1,i),tmpclw,tmpdat(2),
     1                             jerr)
c
c.................Reverse CUTCOM direction
c
cc                      if (i .eq. icut) then
cc                          if (tdata(is4+4) .eq. 8) then
cc                              tdata(is4+4) = 24
cc                          else if(tdata(is4+4) .eq. 24) then
cc                              tdata(is4+4) = 8
cc                          endif
cc                      endif
c
                      call clstor (ioclf,iorec,tmpclw,tmpdat(2))
                      call poprvl (i,nent(i),strec(1,i),endrec(1,i))
                      ismod(i) = 0
                  endif
  630         continue
c
c.........Output motion record
c
              call clstor (ioclf,iorec,iclw,rdata(2))
c
c......Other than motion
c
          else
              ismod(icir) = 0
              call clstor (ioclf,iorec,iclw,rdata(2))
          endif
c
c...Forward pass through reversed records
c...Mark the following special records
c
      else if (irev .eq. 1) then
c
c......Post command
c
          if (iclw(3) .eq. 2000) then
              if (ismod(icir) .eq. 1)
     1            call poprvl (icir,nent(icir),strec(1,icir),
     2                                        endrec(1,icir))
              ismod(icir) = 0
c
c.........Recognized mode setting command
c
              do 700 i=2,NSTK,1
                  if (iclw(4) .eq. iwrd(i)) then
                      if (i .ne. irap) then
                          if (ismod(i) .eq. 1)
     1                        call poprvl (i,nent(i),strec(1,i),
     2                            endrec(1,i))
                          if (nent(i) .ne. 0)
     1                        call pshrvl (lstrec,i,nent(i),i2)
                      endif
                      call pshrvl (lstrec,i,nent(i),i1)
                      ismod(i) = 1
                  endif
  700         continue
c
c......Circular
c
          else if (iclw(3) .eq. 3000) then
              if (ismod(icir) .eq. 1)
     1            call poprvl (icir,nent(icir),strec(1,icir),
     2                                        endrec(1,icir))
              call pshrvl (lstrec,icir,nent(icir),i1)
              ismod(icir)  = 1
c
c......Motion record
c
          else if (iclw(3) .eq. 5000 .or. iclw(3) .eq. 5200) then
              if (ismod(icir) .eq. 1) then
                  call pshrvl (lstrec,icir,nent(icir),i2)
                  ismod(icir)  = 2
              else if (ismod(icir) .eq. 2) then
                  if (iclw(4) .eq. 6) then
                      call pshrvl (lstrec,icir,nent(icir),i2)
                  else
                      ismod(icir) = 0
                  endif
              endif
c
c...Set rapid mode
c
              if (ismod(irap) .eq. 1) then
                  call pshrvl (lstrec,irap,nent(irap),i2)
              endif
c
              do 720 i=2,NSTK,1
                  ismod(i) = 0
  720         continue
c
c......Other record type
c
          else
              if (ismod(icir) .eq. 1)
     1            call poprvl (icir,nent(icir),strec(1,icir),
     2                                        endrec(1,icir))
              ismod(icir) = 0
          endif
c
c...REVERS not in effect
c...Write out clfile record
c
      else
          call clstor (ioclf,iorec,iclw,rdata(2))
          do 800 i=2,NSTK,1
              if (iclw(3) .eq. 2000 .and. iclw(4) .eq. iwrd(i) .and.
     1            i .ne. irap) call ncl_setptr(lstrec,modrec(1,i))
  800     continue
      endif
c
c...Check for end of range
c
 2000 if (irev .eq. 2) then
          call ncl_tstptr(irec,iflg)
          call ncl_eqlptr(irec,irven,iflg1)
          if (iflg .ne. 0 .and. iflg1 .ne. 1) go to 100
          call inirvl
          irev   = 0
      endif
      call ncl_tstptr(icpt,iflg)
      call ncl_eqlptr(icpt,ken,iflg1)
      if (iflg .ne. 0 .and. iflg1 .ne. 1) go to 100
c
c...End of routine
c
 8000 if (idid .eq. 1 .and. irev .eq. 0) kclf = ioclf
      return
      end
