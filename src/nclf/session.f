C*********************************************************************
C*    NAME         :  session.f
C*       CONTAINS:
C*                   nclf_save_session_file()
C*                   nclf_save_session_common()
C*                   svcm--()
C*                   nclf_load_session_file()
C*                   nclf_load_session_common()
C*                   ldcm--()
C*
C*    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       session.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:40
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : nclf_save_session_file(cdir,knc,kerr)
c*        Saves the 'partpgm.ncl', 'ranfil.rf' and 'cl' files to an NCL
c*        session directory.
C*    PARAMETERS   
C*       INPUT  : 
c*        cdir  = The NCL session directory to save the files to.
c*
c*        knc   = Number of chars in 'cdir'.
c*
C*       OUTPUT :  
c*        kerr  = 1 = An error occurred writing the files.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine nclf_save_session_file (cdir,knc,kerr)
c
      include 'com8a.com'
      include 'mocom.com'
c
      integer*4 knc,kerr
c
      character*(MAX_PATH) cdir
c
      integer*2 ibuf(144),ioerr,irec,iclf,jerr,iflg
      integer*4 jrec,inc,jcpt(2),iclw(6),nc,strlen1,n
c
      real*8 rbuf(421)
c
      character*(MAX_LEN) buf
      character*(MAX_PATH) sfil
c
      equivalence (rbuf,ibuf)
c
c...Initialize routine
c
      kerr   = 0
c
c...Save the Part Program file
c
      sfil   = cdir(1:knc) // 'NCL_session.pgm'
		nc = strlen1(sfil)
      call nclf_save_src(sfil,nc,ioerr)
      if (ioerr .ne. 0) go to 9000
c
c...Open the Ranfil file
c
      sfil   = cdir(1:knc) // 'NCL_session.rf'
      call flopnw (SCRLUN, sfil, 'DIRECT', 'UNFORMATTED', 288, 'NULL',
     1             ioerr)
      if (ioerr .ne. 0) go to 9000
c
c...Save the Ranfil file
c
      irec   = 0
  500 if (irec .gt. ifl(4)) go to 900
      call getran (ibuf,irec)
      write (SCRLUN,rec=irec+1,err=9100) ibuf
      irec   = irec   + 1
      go to 500
c
c...Close the Ranfil file
c
  900 close (unit=SCRLUN)
c
c...Open the Primary CL file
c
      sfil   = cdir(1:knc) // 'NCL_session.cl'
      call flopnw (cllun, sfil, 'DIRECT', 'UNFORMATTED', 288, 'NULL',
     1             ioerr)
      if (ioerr .ne. 0) go to 9000
c
c...Save the Primary CL file
c
      jrec   = 0
      call ncl_zroptr(jcpt)
      inc    = 0
      iclf   = 0
 1000 call clread (iclf,jcpt,iclw,rbuf(2),jerr)
      if (jerr .eq. 1) go to 1400
      ibuf(1) = iclw(5) + 1
      ibuf(2) = iclw(3)
      ibuf(3) = iclw(4)
      call isn4i2 (iclw(1),ibuf(4))
      call clwr (rbuf,ibuf(1),clbuff,jrec,inc,iclw(1))
      call ncl_tstptr(jcpt,iflg)
      if (iflg .ne. 0) go to 1000
c
c...Add pseudo FINI card
c
 1400 ibuf(1) = 1
      ibuf(2) = 14001
      ibuf(3) = 0
      ibuf(4) = 0
      n      = 0
      call clwr (rbuf,ibuf(1),clbuff,jrec,inc,n)
c
c...Close the Primary CL file
c
      if (inc .ne. 0) call clput (jrec,clbuff)
      close (unit=cllun)
c
c...Open the Secondary CL file
c...If required
c
      iclf   = 1
      call ncl_zroptr(jcpt)
      call clread (iclf,jcpt,iclw,rbuf(2),jerr)
      if (jerr .eq. 1) go to 1900
      sfil   = cdir(1:knc) // 'NCL_session.cl2'
      call flopnw (cllun, sfil, 'DIRECT', 'UNFORMATTED', 288, 'NULL',
     1             ioerr)
      if (ioerr .ne. 0) go to 9000
c
c...Save the Secondary CL file
c
      call ncl_zroptr(jcpt)
      jrec   = 0
      inc    = 0
      iclf   = 1
 1500 call clread (iclf,jcpt,iclw,rbuf(2),jerr)
      if (jerr .eq. 1) go to 1800
      ibuf(1) = iclw(5) + 1
      ibuf(2) = iclw(3)
      ibuf(3) = iclw(4)
      call isn4i2 (iclw(1),ibuf(4))
      call clwr (rbuf,ibuf(1),clbuff,jrec,inc,iclw(1))
      call ncl_tstptr(jcpt,iflg)
      if (iflg .ne. 0) go to 1500
c
c...Add pseudo FINI card
c
 1800 ibuf(1) = 1
      ibuf(2) = 14001
      ibuf(3) = 0
      ibuf(4) = 0
      n      = 0
      call clwr (rbuf,ibuf(1),clbuff,jrec,inc,n)
c
c...Close the Secondary CL file
c
      if (inc .ne. 0) call clput (jrec,clbuff)
      close (unit=cllun)
 1900 continue
c
c...End of routine
c
 8000 return
c
c...Error opening file
c
 9000 kerr   = 1
      go to 8000
c
c...Error writing file
c
 9100 kerr   = 1
      close (unit=SCRLUN)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : nclf_save_session_common(cdir,knc,kerr)
c*        Saves the Fortran common arrays to an NCL session Directory.
C*    PARAMETERS   
C*       INPUT  : 
c*        cdir  = The NCL session directory to save the files to.
c*
c*        knc   = Number of chars in 'cdir'.
c*
C*       OUTPUT :  
c*        kerr  = 1 = An error occurred writing the files.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine nclf_save_session_common (cdir,knc,kerr)
c
      include 'com.com'
      include 'comgt.com'
      include 'cmm.com'
      include 'cutter.com'
      include 'drvcom.com'
      include 'fillet.com'
      include 'gidcom.com'
      include 'ifcom.com'
      include 'mocom.com'
      include 'rrdm.com'
      include 'status.com'
      include 'suvcom.com'
      include 'vaxvoc.com'
      include 'wrksys.com'
c
      integer*4 knc,kerr
c
      character*(MAX_PATH) cdir
c
      integer*2 ibuf(256)
      integer*4 jrec,inc,jbuf(128)
c
      real*4 rbuf(128)
      real*8 qbuf(64)
c
      character*512 sbuf
      character*(MAX_PATH) sfil
c
      equivalence (ibuf,jbuf,rbuf,qbuf,sbuf)
c
c...Initialize routine
c
      kerr   = 0
      inc    = 0
      jrec   = 0
c
c...Open the Common file
c
      sfil   = cdir(1:knc) // 'NCL_session.ndf'
      call flopnw (SCRLUN, sfil, 'DIRECT', 'UNFORMATTED', 512, 'NULL',
     1             ioerr)
      if (ioerr .ne. 0) go to 9000
c
c...Save Integer*2 arrays
c
      call svcmi2 (INCLMP,N_INCLMP,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (ICMCOM,N_ICMCOM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (ICTCOM,N_ICTCOM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (IFILET,N_IFILET,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (IRRDM,N_IRRDM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (VVCNUM,N_VOC,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (ISTMAP,N_ISTMAP,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (ISUVCM,N_ISUVCM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (IIFCOM,N_IIFCOM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (IGIDCM,N_IGIDCM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi2 (ibuf,-1,ibuf,inc,jrec,kerr)
c
c...Save Integer*4 arrays
c
      call svcmi4 (KNCLMP,N_KNCLMP,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi4 (KCTCOM,N_KCTCOM,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi4 (KFILET,N_KFILET,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi4 (KRRDM,N_KRRDM,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi4 (KSUVCM,N_KSUVCM,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi4 (KIFCOM,N_KIFCOM,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmi4 (jbuf,-1,jbuf,inc,jrec,kerr)
c
c...Save Real*4 arrays
c
      call svcmr4 (RNCLMP,N_RNCLMP,rbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr4 (RRRDM,N_RRRDM,rbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr4 (RSUVCM,N_RSUVCM,rbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr4 (RGIDCM,N_RGIDCM,rbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr4 (rbuf,-1,rbuf,inc,jrec,kerr)
c
c...Save Real*8 arrays
c
      call svcmr8 (GNCLMP,N_GNCLMP,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (GCMCOM,N_GCMCOM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (MOTMAP,N_MOTMAP,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (GCTCOM,N_GCTCOM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (GSFCOM,N_GSFCOM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (GFILET,N_GFILET,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (MOCCOM,N_MOCCOM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (GRRDM,N_GRRDM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (WRKSYS,N_WRKSYS,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (GGIDCM,N_GGIDCM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmr8 (qbuf,-1,qbuf,inc,jrec,kerr)
c
c...Save Character*1 arrays
c
      call svcmc1 (CNCLMP,N_CNCLMP,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmc1 (CCMCOM,N_CCMCOM,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmc1 (CCTCOM,N_CCTCOM,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmc1 (CRRDM,N_CRRDM,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmc1 (VVCNAM,N_VOC*6,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmc1 (CWKSYS,N_CWKSYS,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call svcmc1 (sbuf,-1,sbuf,inc,jrec,kerr)
c
c...Close common file
c
      close (unit=SCRLUN)
c
c...End of routine
c
 8000 return
c
c...Error opening file
c
 9000 kerr   = 1
      go to 8000
c
c...Error writing file
c
 9100 kerr   = 1
      close (unit=SCRLUN)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : svcm-- (ibuf,knc,obuf,kinc,krec,kerr)
C*        Writes out the Fortran common arrays to a disk file.
C*            svcmi2 = Writes out INTEGER*2 common arrays.
C*            svcmi4 = Writes out INTEGER*4 common arrays.
C*            svcmr4 = Writes out REAL*4 common arrays.
C*            svcmr8 = Writes out REAL*8 common arrays.
C*            svcmc1 = Writes out CHARACTER common arrays.
C*    PARAMETERS   
C*       INPUT  : 
c*        ibuf  = Input array to save.
c*
c*        knc   = Number of entities in 'ibuf'.
c*
c*        kinc  = Should be set to zero on first call.
c*
c*        krec  = Current record in saved common file.
c*
C*       OUTPUT :  
c*        obuf  = Buffered output array.
C*
c*        kinc  = Index into 'obuf'.
c*
c*        krec  = Current record in saved common file.
c*
c*        kerr  = 1 = An error occurred writing the files.
c*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine svcmi2 (kibuf,knc,kobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 kibuf(256),knc,kobuf(256)
      integer*4 kinc,krec,kerr
c
      integer*2 i
c
c...Flush output buffer
c
      if (knc .eq. -1) then
          krec   = krec   + 1
          write (SCRLUN,rec=krec,err=9000) kobuf
          kinc   = 0
          go to 8000
      endif
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 256) then
              krec   = krec   + 1
              write (SCRLUN,rec=krec,err=9000) kobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          kobuf(kinc) = kibuf(i)
  100 continue
c
c...End of routine
c
 8000 return
c
c...Error writing file
c
 9000 kerr   = 1
      go to 8000
      end
C
      subroutine svcmi4 (kibuf,knc,kobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 knc
      integer*4 kibuf(128),kobuf(128),kinc,krec,kerr
c
      integer*2 i
c
c...Flush output buffer
c
      if (knc .eq. -1) then
          krec   = krec   + 1
          write (SCRLUN,rec=krec,err=9000) kobuf
          kinc   = 0
          go to 8000
      endif
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 128) then
              krec   = krec   + 1
              write (SCRLUN,rec=krec,err=9000) kobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          kobuf(kinc) = kibuf(i)
  100 continue
c
c...End of routine
c
 8000 return
c
c...Error writing file
c
 9000 kerr   = 1
      go to 8000
      end
C
      subroutine svcmr4 (gibuf,knc,gobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 knc
      integer*4 kinc,krec,kerr
c
      real*4 gibuf(128),gobuf(128)
c
      integer*2 i
c
c...Flush output buffer
c
      if (knc .eq. -1) then
          krec   = krec   + 1
          write (SCRLUN,rec=krec,err=9000) gobuf
          kinc   = 0
          go to 8000
      endif
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 128) then
              krec   = krec   + 1
              write (SCRLUN,rec=krec,err=9000) gobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          gobuf(kinc) = gibuf(i)
  100 continue
c
c...End of routine
c
 8000 return
c
c...Error writing file
c
 9000 kerr   = 1
      go to 8000
      end
C
      subroutine svcmr8 (gibuf,knc,gobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 knc
      integer*4 kinc,krec,kerr
c
      real*8 gibuf(64),gobuf(64)
c
      integer*2 i
c
c...Flush output buffer
c
      if (knc .eq. -1) then
          krec   = krec   + 1
          write (SCRLUN,rec=krec,err=9000) gobuf
          kinc   = 0
          go to 8000
      endif
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 64) then
              krec   = krec   + 1
              write (SCRLUN,rec=krec,err=9000) gobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          gobuf(kinc) = gibuf(i)
  100 continue
c
c...End of routine
c
 8000 return
c
c...Error writing file
c
 9000 kerr   = 1
      go to 8000
      end
C
      subroutine svcmc1 (cibuf,knc,cobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 knc
      integer*4 kinc,krec,kerr
c
      character*1 cibuf(512),cobuf(512)
c
      integer*2 i
c
c...Flush output buffer
c
      if (knc .eq. -1) then
          krec   = krec   + 1
          write (SCRLUN,rec=krec,err=9000) cobuf
          kinc   = 0
          go to 8000
      endif
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 512) then
              krec   = krec   + 1
              write (SCRLUN,rec=krec,err=9000) cobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          cobuf(kinc) = cibuf(i)
  100 continue
c
c...End of routine
c
 8000 return
c
c...Error writing file
c
 9000 kerr   = 1
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : nclf_load_session_file(cdir,ksec,knc,kerr)
c*        Loads the 'partpgm.ncl', 'ranfil.rf' and 'cl' files from an NCL
c*        session directory.
C*    PARAMETERS   
C*       INPUT  : 
c*        cdir  = The NCL session directory to load the files from.
c*
c*        knc   = Number of chars in 'cdir'.
c*
c*        ksec  = 1 = Load secondary clfile.
C*        slen  = block length of the PP file (max line length of the session PP file)
c*
C*       OUTPUT :  
c*        kerr  = 1 = An error occurred loading the files.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine nclf_load_session_file (cdir,knc,ksec, slen, kerr)
c
      include 'com8a.com'
      include 'mocom.com'
c
      integer*4 knc,ksec,kerr,slen
c
      character*(MAX_PATH) cdir
c
      integer*2 ibuf(144),ioerr,irec,iclf, ilen
      integer*4 jrec,jcpt,strlen1,nc
      character*(MAX_PATH) temp
c
      real*8 rbuf(421)
c
      character*(MAX_LEN) buf
      character*(MAX_PATH) sfil
c
      equivalence (rbuf,ibuf)
c
c...Initialize routine
c
      kerr   = 0
c
c...Load the Part Program file
c
      sfil   = cdir(1:knc) // 'NCL_session.pgm'
		nc = strlen1(sfil)
      call nclf_load_src(sfil,nc,1,jrec)
      if (jrec .eq. -1) go to 9000
c
c...Open the Ranfil file
c
      sfil   = cdir(1:knc) // 'NCL_session.rf'
      call flopen (SCRLUN, sfil, 'OLD', 'DIRECT', 'UNFORMATTED', 288,
     1             'NULL', ioerr)
      if (ioerr .ne. 0) go to 9000
c
c...Load the Ranfil file
c
      irec   = 0
  500 read (SCRLUN,rec=irec+1,err=900) ibuf
      call putran (ibuf,irec)
      irec   = irec   + 1
      go to 500
c
c...Close the Ranfil file
c
  900 close (unit=SCRLUN)
c
c...Load the Primary CL file
c
      sfil   = cdir(1:knc) // 'NCL_session.cl'
      nc     = strlen1(sfil)
      iclf   = 0
      jcpt   = 0
      call clload (iclf,sfil,nc,jcpt,ioerr)
      if (ioerr .ne. 0) go to 9000
c
c...Open the Secondary CL file
c...If required
c
      if (ksec .eq. 1) then
          sfil   = cdir(1:knc) // 'NCL_session.cl2'
          nc     = strlen1(sfil)
          iclf   = 1
          jcpt   = 0
          call clload (iclf,sfil,nc,jcpt,ioerr)
          if (ioerr .ne. 0) go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Error opening file
c
 9000 kerr   = 1
      go to 8000
c
c...Error writing file
c
 9100 kerr   = 1
      close (unit=SCRLUN)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : nclf_load_session_common(cdir,knc,kerr)
c*        Saves the Fortran common arrays to an NCL session Directory.
C*    PARAMETERS   
C*       INPUT  : 
c*        cdir  = The NCL session directory to save the files to.
c*
c*        knc   = Number of chars in 'cdir'.
c*
C*       OUTPUT :  
c*        kerr  = 1 = An error occurred writing the files.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine nclf_load_session_common (cdir,knc,kerr)
c
      include 'com.com'
      include 'comgt.com'
      include 'cmm.com'
      include 'cutter.com'
      include 'drvcom.com'
      include 'fillet.com'
      include 'gidcom.com'
      include 'ifcom.com'
      include 'mocom.com'
      include 'rrdm.com'
      include 'status.com'
      include 'suvcom.com'
      include 'vaxvoc.com'
      include 'wrksys.com'
c
      integer*4 knc,kerr
c
      character*(MAX_PATH) cdir
c
      integer*2 ibuf(256),iclf,iflg,jerr
      integer*4 jrec,inc,jbuf(128),iclw(6)
c
      real*4 rbuf(128)
      real*8 qbuf(64),dcltmp(640)
c
      character*512 sbuf
      character*(MAX_PATH) sfil
c
      equivalence (ibuf,jbuf,rbuf,qbuf,sbuf)
c
c...Initialize routine
c
      kerr   = 0
      inc    = 0
      jrec   = 0
c
c...Open the Common file
c
      sfil   = cdir(1:knc) // 'NCL_session.ndf'
      call flopen (SCRLUN, sfil, 'OLD', 'DIRECT', 'UNFORMATTED', 512,
     1             'NULL', ioerr)
      if (ioerr .ne. 0) go to 9000
c
c...Save Integer*2 arrays
c
      call ldcmi2 (INCLMP,N_INCLMP,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (ICMCOM,N_ICMCOM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (ICTCOM,N_ICTCOM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (IFILET,N_IFILET,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (IRRDM,N_IRRDM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (VVCNUM,N_VOC,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (ISTMAP,N_ISTMAP,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (ISUVCM,N_ISUVCM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (IIFCOM,N_IIFCOM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi2 (IGIDCM,N_IGIDCM,ibuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      inc    = 0
c
c...Load Integer*4 arrays
c
      call ldcmi4 (KNCLMP,N_KNCLMP,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi4 (KCTCOM,N_KCTCOM,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi4 (KFILET,N_KFILET,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi4 (KRRDM,N_KRRDM,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi4 (KSUVCM,N_KSUVCM,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmi4 (KIFCOM,N_KIFCOM,jbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      inc    = 0
c
c...Load Real*4 arrays
c
      call ldcmr4 (RNCLMP,N_RNCLMP,rbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr4 (RRRDM,N_RRRDM,rbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr4 (RSUVCM,N_RSUVCM,rbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr4 (RGIDCM,N_RGIDCM,rbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      inc    = 0
c
c...Save Real*8 arrays
c
      call ldcmr8 (GNCLMP,N_GNCLMP,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (GCMCOM,N_GCMCOM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (MOTMAP,N_MOTMAP,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (GCTCOM,N_GCTCOM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (GSFCOM,N_GSFCOM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (GFILET,N_GFILET,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (MOCCOM,N_MOCCOM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (GRRDM,N_GRRDM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (WRKSYS,N_WRKSYS,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmr8 (GGIDCM,N_GGIDCM,qbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      inc    = 0
c
c...Save Character*1 arrays
c
      call ldcmc1 (CNCLMP,N_CNCLMP,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmc1 (CCMCOM,N_CCMCOM,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmc1 (CCTCOM,N_CCTCOM,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmc1 (CRRDM,N_CRRDM,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmc1 (VVCNAM,N_VOC*6,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      call ldcmc1 (CWKSYS,N_CWKSYS,sbuf,inc,jrec,kerr)
      if (kerr .ne. 0) go to 9000
c
      inc    = 0
c
c...Reset the clfile pointer
c
      iclf   = 0
      call clgetp (iclf,IMOTP)
c
c...and the ARCSLP/FILLET pointer
c
      if (IFL(347) .ne. 0) then
c          ISREC2  = I4STAT(2)
          call ncl_setptr(IMOTP,ISREC2)
          iclw(3) = 0
          call ncl_tstptr(ISREC2,iflg)
          do while (iclw(3) .ne. 5000 .and. iclw(3) .ne. 5200 .and.
     1              iflg .ne. 0)
              call clprev (iclf,ISREC2,iclw,dcltmp,jerr)
              call ncl_tstptr(ISREC2,iflg)
          enddo
c          ISREC  = ISREC2
          call ncl_setptr(ISREC2,ISREC)
          iclw(3) = 0
          call ncl_tstptr(ISREC,iflg)
          if (IFL(347) .ge. 2) then
              do while (iclw(3) .ne. 5000 .and. iclw(3) .ne. 5200 .and.
     1                  iflg .ne. 0)
                  call clprev (iclf,ISREC,iclw,dcltmp,jerr)
                  call ncl_tstptr(ISREC,iflg)
              enddo
          endif
      endif
c
c...Close common file
c
      close (unit=SCRLUN)
c
c...End of routine
c
 8000 return
c
c...Error opening file
c
 9000 kerr   = 1
      go to 8000
c
c...Error reading file
c
 9100 kerr   = 1
      close (unit=SCRLUN)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : ldcm-- (ibuf,knc,obuf,kinc,krec,kerr)
C*        Loads in the Fortran common arrays from a disk file.
C*            ldcmi2 = Writes out INTEGER*2 common arrays.
C*            ldcmi4 = Writes out INTEGER*4 common arrays.
C*            ldcmr4 = Writes out REAL*4 common arrays.
C*            ldcmr8 = Writes out REAL*8 common arrays.
C*            ldcmc1 = Writes out CHARACTER common arrays.
C*    PARAMETERS   
C*       INPUT  : 
c*        ibuf  = Input array to load.
c*
c*        knc   = Number of entities in 'ibuf'.
c*
c*        kinc  = Should be set to zero on first call.
c*
c*        krec  = Current record in saved common file.
c*
C*       OUTPUT :  
c*        obuf  = Buffered output array.
C*
c*        kinc  = Index into 'obuf'.
c*
c*        krec  = Current record in saved common file.
c*
c*        kerr  = 1 = An error occurred reading the files.
c*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ldcmi2 (kibuf,knc,kobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 kibuf(256),knc,kobuf(256)
      integer*4 kinc,krec,kerr
c
      integer*2 i
c
c...Initialize routine
c
      if (kinc .eq. 0) kinc = 257
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 256) then
              krec   = krec   + 1
              read (SCRLUN,rec=krec,err=9000) kobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          kibuf(i) = kobuf(kinc)
  100 continue
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
C
      subroutine ldcmi4 (kibuf,knc,kobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 knc
      integer*4 kibuf(128),kobuf(128),kinc,krec,kerr
c
      integer*2 i
c
c...Initialize routine
c
      if (kinc .eq. 0) kinc = 128
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 128) then
              krec   = krec   + 1
              read (SCRLUN,rec=krec,err=9000) kobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          kibuf(i) = kobuf(kinc)
  100 continue
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
C
      subroutine ldcmr4 (gibuf,knc,gobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 knc
      integer*4 kinc,krec,kerr
c
      real*4 gibuf(128),gobuf(128)
c
      integer*2 i
c
c...Initialize routine
c
      if (kinc .eq. 0) kinc = 128
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 128) then
              krec   = krec   + 1
              read (SCRLUN,rec=krec,err=9000) gobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          gibuf(i) = gobuf(kinc)
  100 continue
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
C
      subroutine ldcmr8 (gibuf,knc,gobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 knc
      integer*4 kinc,krec,kerr
c
      real*8 gibuf(64),gobuf(64)
c
      integer*2 i
c
c...Initialize routine
c
      if (kinc .eq. 0) kinc = 64
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 64) then
              krec   = krec   + 1
              read (SCRLUN,rec=krec,err=9000) gobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          gibuf(i) = gobuf(kinc)
  100 continue
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
C
      subroutine ldcmc1 (cibuf,knc,cobuf,kinc,krec,kerr)
c
      include 'com.com'
c
      integer*2 knc
      integer*4 kinc,krec,kerr
c
      character*1 cibuf(512),cobuf(512)
c
      integer*2 i
c
c...Initialize routine
c
      if (kinc .eq. 0) kinc = 512
c
c...Buffer common
c
      do 100 i=1,knc,1
          if (kinc .ge. 512) then
              krec   = krec   + 1
              read (SCRLUN,rec=krec,err=9000) cobuf
              kinc   = 0
          endif
          kinc   = kinc   + 1
          cibuf(i) = cobuf(kinc)
  100 continue
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
