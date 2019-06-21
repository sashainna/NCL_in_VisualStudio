c***********************************************************************
c
c   FILE NAME:  aptmac
c   CONTAINS:
c               ammac   amgmac  amgrec  amstor  amterm  amgmac  amini
c               amopen  aptmac  aptcal  aptinc  amcall  amread  opninc
c               compini
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        aptmac.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:49
c
c***********************************************************************
c***********************************************************************
c
c   SUBROUTINE: aptmac (kpt,cmsg,kerr)
c
c   FUNCTION:  This routine parses MACRO declaration in source file and
c              stores paramters and macro name in scratch file
c
c   INPUT:  kpt     I*4  D1    -  Pointer to start parsing tokens in the
c                                 source line
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine aptmac (kpt,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'compile.inc'
c
      integer*4 kpt,kerr
c
      character*(*) cmsg
c
      integer*4 ist,ien,np,idef(50),nc,lcom,strlen1
c
      real*8 rdef(50)
c
      character*8 macnm,parm(50)
      character*80 ldat
c
c...Initialize routine
c
      kerr   = 0
      ist    = RCSUB(kpt)
      ien    = ICNC(kpt)
      macnm  = LCTXT(ist:ien)
      kpt    = kpt    + 3
      if (ICTYP(kpt) .eq. 2 .and. RCSUB(kpt) .eq. 7.) RCSUB(kpt) = 2
      np     = 0
c
c...Store parameters
c
  100 if (kpt .gt. NTOK) go to 500
c
c......Comma delimiter
c
      if (ICTYP(kpt) .ne. 2 .or. RCSUB(kpt) .ne. 2.) go to 9000
      kpt    = kpt    + 1
      if (kpt .gt. NTOK) go to 9000
c
c......Parameter name
c
      if (ICTYP(kpt) .ne. 4) go to 9000
      np     = np     + 1
      ist    = RCSUB(kpt)
      ien    = ICNC(kpt)
      parm(np) = LCTXT(ist:ien)
      kpt    = kpt    + 1
c
c......Default value
c
      idef(np) = 0
      rdef(np) = 0.
      if (kpt .lt. NTOK .and. ICTYP(kpt) .eq. 2 .and. RCSUB(kpt) .eq. 1)
     1        then
          kpt    = kpt    + 1
          idef(np) = ICTYP(kpt)
          if (ICTYP(kpt) .eq. 1 .or. ICTYP(kpt) .eq. 3) then
              rdef(np) = RCSUB(kpt)
          else
              go to 9000
          endif
          kpt    = kpt    + 1
      endif
c
c...Go get next parameter
c
      go to 100
c
c...End of parsing MACRO statement
c
  500 call ammac (macnm,np,parm,idef,rdef,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Load entire MACRO
c
 1000 call rdtxt (LUNSC2,ldat,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Store MACRO record
c
      nc     = strlen1(ldat)
      call amstor (ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Look for TERMAC
c
      call stpars (ldat,nc,1,cmsg,kerr,lcom)
      if (NTOK .ne. 1 .or. ICTYP(1) .ne. 1 .or. RCSUB(1) .ne. 4010.)
     1    go to 1000
      call amterm (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
c
c...Error during Macro definition
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE: aptcal (kpt,cdat,knc,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine parses CALL statement in source file and
c              calls specified macro.  If macro has not been declared
c              it trys to open file with macro name used as file name.
c
c   INPUT:  kpt     I*4  D1    -  Pointer to start parsing tokens in the
c                                 source line
c
c           cdat    c*n  D1    -  Source line to parse
c
c           knc     I*4  D1    -  Number of characters in cdat
c
c           kfl     I*4  D1    -  Flag: 0 - macro can be declared, 1 -
c                                 macro is defined in include file.
c
c   OUTPUT: kfl     I*4  D1    -  Flag: 0 - no files required, 1 - macro
c                                 file with macro definition is open.
c
c           cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
      subroutine aptcal (kpt,cdat,knc,kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'compile.inc'
c
      integer*4 kfl,knc,kpt,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 ist,ien,nc,strlen1,itry,inc,inc1
c
      character*8 macnm
      character*(MAX_PATH) lfil,ldev
      character*(MAX_FILE) lfil1,lext
c
c...Initialize routine
c
      if (JMAC .eq. 1) go to 9000
      kerr   = 0
      itry   = kfl
      kfl    = 0
      JMNP   = 0
c
c...Get MACRO name
c
      kpt    = kpt    + 1
      if (kpt .ge. NTOK) go to 9000
      if (ICTYP(kpt) .ne. 2 .or. RCSUB(kpt) .ne. 7.) go to 9000
      kpt    = kpt    + 1
      if (ICTYP(kpt) .ne. 4) go to 9000
      ist    = RCSUB(kpt)
      ien    = ICNC(kpt)
      macnm  = LCTXT(ist:ien)
      kpt    = kpt    + 1
c
c...Get parameters
c
  100 if (kpt .gt. NTOK) go to 500
c
c......Comma delimiter
c
      if (ICTYP(kpt) .ne. 2 .or. RCSUB(kpt) .ne. 2.) go to 9000
      kpt    = kpt    + 1
      if (kpt .gt. NTOK) go to 9000
c
c......Parameter name
c
      if (ICTYP(kpt) .ne. 4) go to 9000
      JMNP     = JMNP     + 1
      ist    = RCSUB(kpt)
      ien    = ICNC(kpt)
      MPARM(JMNP) = LCTXT(ist:ien)
      kpt    = kpt    + 1
c
c......Default value
c
      if (kpt .ge. NTOK) go to 9000
      if (ICTYP(kpt) .ne. 2 .or. RCSUB(kpt) .ne. 1) go to 9000
      kpt    = kpt    + 1
      MDEF(JMNP) = ICTYP(kpt)
      if (ICTYP(kpt) .eq. 1 .or. ICTYP(kpt) .eq. 3) then
          MRDEF(JMNP) = RCSUB(kpt)
      else
          go to 9000
      endif
      kpt    = kpt    + 1
c
c...Go get next parameter
c
      go to 100
c
c...End of parsing CALL statement
c...Fill in CALL statement with default parameters
c
  500 call amcall (macnm,cmsg,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Show that a MACRO is being called
c
      JMAC   = 1
c
c...End of routine
c
 8000 return
c
c...Error during Macro definition
c......Try and open include file
c
 9000 if (itry .ne. 0) then
         kerr = 1
         go to 8000
      end if
c
      inc    = index(cdat(1:knc),'/') + 1
      inc1   = index(cdat(1:knc),',') - 1
      if (inc1 .le. 0) inc1 = knc
      lfil   = cdat(inc:inc1)
      nc     = inc1 - inc
      call opninc (lfil,nc,'.mac',cmsg,kerr)
      if (kerr .lt. 0) then
          kerr = 27
          go to 8000
      end if
      if (kerr .ne. 0) go to 8000
c
c......Include file opened
c
      call fbreak (lfil,ldev,lfil1,lext)
      nc     = strlen1(lfil1)
      if (inc1 .lt. knc) then
              MCMD   = cdat(1:inc-1) // lfil1(1:nc) // cdat(inc1+1:)
      else
              MCMD   = cdat(1:inc-1) // lfil1(1:nc)
      endif
      MCMDNC = strlen1(MCMD)
      kfl    = 1
      go to 8000
c
      end
c
c***********************************************************************
c
c   SUBROUTINE: amcall (cmac,cmsg,kerr)
c
c   FUNCTION:  This routine process CALL MACRO statement replacing
c              any variables declared in macro definition by actual
c              values used in call statement.
c
c   INPUT:  cmac    C*n  D1    -  Macro name to process
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
      subroutine amcall (cmac,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmac,cmsg
c
      integer*4 i,j,np,idef(50),ist,ien
c
      real*8 rdef(50)
c
      character*8 parm(50)
c
c...Initialize routine
c
      kerr   = 0
c
c...Get the MACRO definition
c
      call amgmac (cmac,np,parm,idef,rdef,ist,ien,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store the parameters
c
      do 200 i=1,JMNP,1
          do 100 j=1,np,1
              if (parm(j) .eq. MPARM(i)) then
                  idef(j) = MDEF(i)
                  rdef(j) = MRDEF(i)
                  go to 200
              endif
  100     continue
c
c......Parameter not found
c
          go to 9000
  200 continue
c
c...Make sure all parameters are assigned &
c...Transfer parameters to global arrays
c
      JMNP   = np
      MRECS = ist
      MRECE = ien
      do 300 i=1,JMNP,1
          if (idef(i) .eq. 0) go to 9000
          MPARM(i) = parm(i)
          MDEF(i) = idef(i)
          MRDEF(i) = rdef(i)
  300 continue
c
c...End of routine
c
 8000 return
c
c...CALL does not match MACRO definition
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE: amread (krec,cdat,knc,kcom,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine assigns value of parameters
c              to the named token when processing macro body.
c
c   INPUT:  krec    I*4  D1   -  Record number in scratch file with macro
c
c           cdat    C*n  D1   -  Macro source line
c
c           knc     I*4  D1   -  Number of characters in source line
c
c           kcom    I*4  D1   -  Flag marking line commented out
c
c           kfl     I*4  D1   -  Flag: 0 - read next record, 1 - don't read,
c                                parse only buffer.
c
c   OUTPUT: ------- same as input if read source (kfl = 0) ---------
c
c           cmsg    C*n  D1   -  Error text when an error occurred.
c
c           kerr    I*4  D1   -  Returns 1 when an error occurred.
c
c***********************************************************************
      subroutine amread (krec,cdat,knc,kcom,kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'clnrd.inc'
      include 'compile.inc'
c
      integer*4 krec,knc,kerr,kcom,kfl
c
      character*(*) cdat,cmsg
c
      integer*4 ierr,i,j,ist,ien
c
      character*8 ldum
      character*80 msg
c
c...Read next MACRO record
c
  100 if (kfl .ne. 1) then
          call amgrec (krec,cdat,knc,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Parse record
c
      call stpars (cdat,knc,1,msg,ierr,kcom)
c
c...Substitute MACRO parameters
c
      do 300 i=1,NTOK,1
          if (ICTYP(i) .eq. 4) then
              ist    = RCSUB(i)
              ien    = ICNC(i)
              ldum   = LCTXT(ist:ien)
              do 200 j=1,JMNP,1
                  if (ldum .eq. MPARM(j)) then
                      ICTYP(i) = MDEF(j)
                      RCSUB(i) = MRDEF(j)
                      go to 300
                  endif
  200         continue
          endif
  300 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: aptinc (kpt,cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine parses READ/2,... statement to include
c              file with macro definition(s) and opens specified file
c
c   INPUT:  kpt     I*4  D1    -  Pointer to start parsing tokens in the
c                                 source line
c
c           cdat    C*n  D1   -  Input source line
c
c           knc     I*4  D1   -  Number of characters in source line
c
c   OUTPUT: cmsg    C*n  D1   -  Error text when an error occurred.
c
c           kerr    I*4  D1   -  Returns 1 when an error occurred.
c
c***********************************************************************
      subroutine aptinc (kpt,cdat,knc,cmsg,kerr)
c
      include 'compile.inc'
      include 'post.inc'
      include 'menu.inc'
c
      integer*4 kpt,kerr,knc
      character*(*) cmsg,cdat
c
      integer*4 ist,ien,nc,strlen1
c
      character*80 cname
c
c...Initialize routine
c
      kerr   = 0
c
c...Check syntax (READ/2,)
c
      kpt    = kpt + 1
      if (ICTYP(kpt) .ne. 2 .or. RCSUB(kpt) .ne. 7.) go to 9000
      kpt    = kpt + 1
      if (ICTYP(kpt) .ne. 3 .and. ICTYP(kpt) .ne. 5 .or.
     -    RCSUB(kpt) .ne. 2.0) go to 9000
      kpt    = kpt + 1
      if (ICTYP(kpt) .ne. 2 .or. RCSUB(kpt) .ne. 2.) go to 9000
c
c...get the rest of line as a text
c
      ist    = index (cdat,',') + 1
      ien    = knc
      cname  = cdat(ist:ien)
      nc = strlen1(cname)
C VAX-SUN-SGI-IBM-HPX-DOS-DEC-START
C     call remspc (cname,cname,nc)
C VAX-SUN-SGI-IBM-HPX-DOS-DEC-END
      call opninc (cname,nc,'.mac',cmsg,kerr)
      if (kerr .lt. 0) kerr = 27
c
 8000 return
c
c...Errors
c
 9000 call errtxt ('INVPSYN',cmsg)
      kerr   = 1
      go to 8000
c
 9100 call errtxt ('INCOVL',cmsg)
 9200 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ammac (cnam,npar,cpar,kdef,rdef,cmsg,kerr)
c
c   FUNCTION:  This routine stores APT macro declaration in the
c              scratch file header and updates common block with
c              macro specification
c
c   INPUT:  cnam    C*8  D1    -  Macro name.
c
c           npar    I*4  D1    -  Number of parameters in macro
c
c           cpar    C*8  D50   -  Parameter names
c
c           kdef    I*4  D50   -  Parameter definition flags
c
c           rdef    R*8  D50   -  Parameter value (if kdef(x) = 3)
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine ammac (cnam,npar,cpar,kdef,rdef,cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      character*8 cnam,cpar(50)
      integer*4 npar,kdef(50)
      real*8 rdef(50)
      character*(*) cmsg
c
      integer*4 kerr
c
      real*8 rbuf(50)
c
      integer*4 irec,i,jbuf(100)
c
      character*8 cbuf(50)
c
      equivalence (rbuf,cbuf,jbuf)
c
      kerr   = 0
c
c...Store macro name
c
      if (SCRECN .lt. 0) call amopen (LUNMAC,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Check if new macro
c
      if (NMACS .eq. MAXMCN) go to 9100
      do 55 i=1,NMACS,1
         if (cnam .eq. MCLST(i)) go to 9200
   55 continue
c
      NMACS  = NMACS + 1
      MCLST(NMACS) = cnam
      MCNPAR(NMACS) = npar
c
c...Store macro header in scratch file
c
      do 110 i=1,npar
         jbuf(i) = kdef(i)
  110 continue
      cbuf(50) = cnam
      irec   = SCRECN + 1
      MCRBSF(NMACS) = irec
      call wrmac (LUNMAC,irec,jbuf,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      do 120 i=1,npar
         cbuf(i) = cpar(i)
  120 continue
      irec   = irec + 1
      call wrmac (LUNMAC,irec,jbuf,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      do 130 i=1,npar
         rbuf(i) = rdef(i)
  130 continue
      SCRECN = irec + 1
      call wrmac (LUNMAC,SCRECN,jbuf,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      MACSTA = 1
c
c...End of routine
c
 8000 return
c
c...Macro number exceded
c
 9100 call errtxt ('MAXMCEX',cmsg)
      go to 9900
c
c...Macro number exceded
c
 9200 call errtxt ('MULTMAC',cmsg)
 9900 kerr = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  amgmac (cnam,npar,cpar,kdef,rdef,krec,klst,cmsg,kerr)
c
c   FUNCTION:  This routine stores APT macro declaration in the
c              scratch file header and updates common block with
c              macro specification
c
c   INPUT:  cnam    C*8  D1    -  Macro name.
c
c   OUTPUT: npar    I*4  D1    -  Number of parameters in macro
c
c           cpar    C*8  D50   -  Parameter names
c
c           kdef    I*4  D50   -  Parameter definition flags
c
c           rdef    R*8  D50   -  Parameter value (if kdef(x) = 3)
c
c           krec    I*4  D1    -  First record of macro definition
c
c           klst    I*4  D1    -  Last record of macro definition
c
c           cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine amgmac (cnam,npar,cpar,kdef,rdef,krec,klst,
     -                   cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      character*8 cnam,cpar(50)
      integer*4 npar,kdef(50),kerr,krec,klst
      real*8 rdef(50)
      character*(*) cmsg
c
      real*8 rbuf(50)
c
      integer*4 irec,i,imac,jbuf(100)
c
      character*8 cbuf(50)
c
      equivalence (rbuf,cbuf,jbuf)
c
      kerr   = 0
c
c...Find macro name
c
      do 55 imac=1,NMACS,1
         if (cnam .eq. MCLST(imac)) go to 200
   55 continue
      go to 9100
c
  200 npar   = MCNPAR(imac)
c
c...Get macro header from scratch file
c
      irec   = MCRBSF(imac)
      call rdmac (LUNMAC,irec,jbuf,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      do 110 i=1,npar
         kdef(i) = jbuf(i)
  110 continue
c
c...make sure it is correct macro
c
      if (cbuf(50) .ne. cnam) go to 9100
c
c...get paramter names and default values
c
      irec   = irec + 1
      call rdmac (LUNMAC,irec,jbuf,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      do 120 i=1,npar
         cpar(i) = cbuf(i)
  120 continue
      irec   = irec + 1
      call rdmac (LUNMAC,irec,jbuf,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      do 130 i=1,npar
         rdef(i) = rbuf(i)
  130 continue
      krec   = 5*irec + 1
      klst   = MCRLST(imac)
c
c...End of routine
c
 8000 return
c
c...Macro number exceded
c
 9100 call errtxt ('MULTMAC',cmsg)
 9900 kerr = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  amgrec (cbuf,knc,cmsg,kerr)
c
c   FUNCTION:  This routine gets APT macro record from the scratch file.
c
c   INPUT:  krec    I*4  D1    -  Macro record number to read
c
c   OUTPUT: cbuf    C*80 D1    -  Text buffer containing macro source.
c
c           knc     I*4  D1    -  Number of characters in cbuf
c
c           cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine amgrec (krec,cbuf,knc,cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      character*(*) cmsg
      character*80 cbuf
      integer*4 knc,kerr,krec
c
      integer*4 strlen1,irec,nsub
c
      kerr   = 0
c
c...Get scratch file record number and
c...buffer index
c
      irec   = krec / 5 + 1
      nsub   = krec - 5*(irec - 1)
      if (nsub .eq. 0) then
          nsub = 5
          irec = irec - 1
      end if
c
c...Check if new macro file record has to be read in
c
      if (irec .ne. SCRECM) then
         SCRECM = irec
         call rdmac (LUNMAC,SCRECM,JCMBUF,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
      end if
c
c...Extract from record next line
c
      cbuf = LCMREC(nsub)
      knc  = strlen1 (cbuf)
      krec   = krec   + 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  amstor (cbuf,knc,cmsg,kerr)
c
c   FUNCTION:  This routine stores APT macro text in the scratch file.
c
c   INPUT:  cbuf    C*80 D1    -  Text buffer containing macro source.
c
c           knc     I*4  D1    -  Number of characters in cbuf
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine amstor (cbuf,knc,cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      character*80 cbuf,cmsg
      integer*4 knc,kerr
c
      character*80 lbuf
c
      kerr   = 0
c
c...Check if record is complete
c
      if (MACSTA .gt. 5) then
         SCRECN = SCRECN + 1
         call wrmac (LUNMAC,SCRECN,JCMBUF,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
         MACSTA = 1
      end if
c
c...make sure buffer is text only
c
      if (knc .gt. 80) go to 9100
      lbuf = cbuf(1:knc)
      if (knc .lt. 80) lbuf(knc+1:80) = ' '
c
c...Add curent line to record
c
      LCMREC(MACSTA) = lbuf
      MACSTA = MACSTA + 1
c
c...End of routine
c
 8000 return
c
c...Error, buffer to long
c
 9100 kerr = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  amterm (cmsg,kerr)
c
c   FUNCTION:  This routine terminates APT macro declaration.
c
c   INPUT:  none
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine amterm (cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      character*80 cmsg
      integer*4 kerr
c
      integer*4 ns
c
c...dump buffer if not empty
c
      kerr   = 0
      if (MACSTA .gt. 1) then
         SCRECN = SCRECN + 1
         call wrmac (LUNMAC,SCRECN,JCMBUF,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
      end if
c
c...store TERMAC record number
c
      ns     = 5*(SCRECN-1) + MACSTA - 1
      MCRLST(NMACS) = ns
      MACSTA = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  amini
c
c   FUNCTION:  This routine initializes common variables.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine amini
c
      include 'menu.inc'
      include 'pregen.inc'
c
c...Initialize common variables
c
      LUNMAC = 20
      SCRECN = -1
      SCRECM = -1
      MACSTA = 0
      MAXMCN = 50
      NMACS  = 0
      call compini (1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c   SUBROUTINE:  compini (kflg)
c
c   FUNCTION:  This routine initializes common variables used in
c              'compile.inc' (conflicting with pregen.inc file).
c
c   INPUT:  kflg   - I*4  D1 - not used.
c
c   OUTPUT: none.
c
c***********************************************************************
      subroutine compini (kflg)
c
      include 'compile.inc'
      integer*4 kflg
c
      INCFLN = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  amopen (klun,cmsg,kerr)
c
c   FUNCTION:  This routine opens scratch file for macro definitions
c
c   INPUT:  klun    - I*4   D1  - Logical unit number.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine amopen (klun,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      integer*4 klun,kerr
      character*(*) cmsg
c
      character*20 att(4)
      integer*4 irecl
c
c...Open scratch binary clfile
c
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'scratch'
      irecl  = 400
      call opnfil (klun,'macros.tmp',att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      SCRECN = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  opninc (cdat,knc,cext,cmsg,kerr)
c
c   FUNCTION:  This routine opens include file with macro definitions
c
c   INPUT:  cdat    C*n  D1    -  File name spec.
c
c           knc     I*4  D1    -  Number of characters in cdat
c
c           cext    C*n  D1    -  Default file extention to use.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
      subroutine opninc (cdat,knc,cext,cmsg,kerr)
c
      include 'menu.inc'
      include 'compile.inc'
      include 'clnrd.inc'
c
      integer*4 knc,kerr
      character*(*) cdat,cmsg,cext
c
      character*20 att(4)
      character*(MAX_PATH) ldev,fnam
      character*(MAX_FILE) lfil,lext
      integer*4 irecl,lu,itry
c
      if (INCFLN .gt. 0) go to 9100
      lu    = 21 + INCFLN
c
c...Set attributes to open include file
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      itry   = 0
      call fbreak (cdat,ldev,lfil,lext)
      if (lext .eq. ' ') lext = cext
c
c...Open include file first in current directory
c
  100 call fparse (lfil,fnam,ldev,lext)
      call opnfil (lu,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) then
         if (itry .eq. 0) then
            ldev = DVDATA
            itry = 1
            go to 100
         else
            go to 8000
         end if
      end if
c
c...Store file unit number
c
      INCFLN = INCFLN + 1
      INCLUN(INCFLN) = LUNSC2
      LUNSC2 = lu
c
c...End of routine
c
 8000 return
c
c...Errors
c
 9100 call errtxt ('INCOVL',cmsg)
      kerr   = 1
      go to 8000
      end
