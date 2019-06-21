c
c***********************************************************************
c
c   FILE NAME:  psterr
c   CONTAINS:
c               perrst  psterr  blderr  pstsum
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        psterr.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        11/06/17 , 10:46:35
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  perrst (cmsg,kfl,cout,knum,gnum,cbuf,ktyp)
c
c   FUNCTION:  This routine optionally gets an error message correspond-
c              ing to the input label and replaces the substring '%s' in
c              the message with a string generated from an integer, real
c              or character string.
c
c   INPUT:  cmsg    I*4  D1  -  Contains the label of the error message
c                               when 'kfl' = 1.  Contains the error
c                               message text when 'kfl' = 2.
c
c           kfl     I*4  D1  -  1 = Get error message from file.  2 =
c                               'cmsg' contains the error message.
c
c           knum    I*4  D1  -  Integer value to place in message when
c                               ktyp = 1.
c
c           gnum    R*8  D1  -  Real value to place in message when ktyp
c                               = 2.
c
c           cbuf    C*n  D1  -  Character string to place in message
c                               when ktyp = 3.
c
c           ktyp    I*4  D1  -  1 = Use integer variable for '%s' re-
c                               placement.  2 = Use real variable.  3 =
c                               Use character string.
c
c   OUTPUT: cout    C*n  D1  -  Error message string with inserted vari-
c                               able.
c
c***********************************************************************
c
      subroutine perrst (cmsg,kfl,cout,knum,gnum,cbuf,ktyp)
c
      integer*4 kfl,knum,ktyp
c
      real*8 gnum
c
      character*(*) cmsg,cout,cbuf
c
      integer*4 nc
c
      character*20 lnum
c
c...Actual error message is provided
c
      if (kfl .eq. 2) then
          cout   = cmsg
c
c...Label to message is provided
c
      else
          call errtxt (cmsg,cout)
      endif
c
c......Place integer variable in message
c
      if (ktyp .eq. 1) then
          call itoc (knum,lnum,nc,0)
          call errstr (cout,lnum,0)
c
c......Real variable
c
      else if (ktyp .eq. 2) then
          call rtoc (gnum,lnum,nc)
          call errstr (cout,lnum,0)
c
c......Character string
c
      else
          call errstr (cout,cbuf,0)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  psterr (ktyp,cmsg1,cmsg2,kprm)
c
c   FUNCTION:  This routine formats and outputs an error message to the
c              print file.  The post-processor command that caused the
c              error will also be formatted and output, with the para-
c              meter that caused the error enclosed in brackets ([]).
c
c   INPUT:  ktyp    I*4  D1  -  Type of error encountered.  1 = Warning,
c                               2 = Error, 3 = Fatal, 4 = APT Source
c                               error.
c
c           cmsg1   C*n  D1  -  Text of 1st error message to output.
c
c           cmsg2   C*n  D1  -  Text of 2nd error message to output.
c
c           kprm    I*4  D1  -  Location of parameter within post com-
c                               mand that caused error.  A value of -1
c                               will disable the output of the post com-
c                               mand line.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine psterr (ktyp,cmsg1,cmsg2,kprm)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (IPSTWD,KPOSMP(0006))
      equivalence (NUMERR,KPOSMP(0082)), (LSTPC ,KPOSMP(0083))
      equivalence (ICLOUT,KPOSMP(0084))
      equivalence (NUMWRN,KPOSMP(0089)), (NUMFAT,KPOSMP(0090))
      equivalence (NUMAER,KPOSMP(0097)), (MCHOPT,KPOSMP(0308))
      equivalence (IPRTOP,KPOSMP(1176)), (IPERRF,KPOSMP(1177))
c
      integer*4 ISN,ITYPE,NUMERR,ICLOUT,NUMWRN,NUMFAT,IPERRF,ISUBT,
     1          IPSTWD(50),LSTPC,IPRTOP,MXCL,NUMAER,MCHOPT(20)
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      equivalence (LPSTWD,CPOSMP(0217)), (LMACRO,CPOSMP(0861))
c
      character*24 LMACRO
      character*512 LPSTWD
c
      integer*4 ktyp,kprm,ktyp1
c
      character*(*) cmsg1,cmsg2
c
      integer*4 nc,strlen1,ierr,isav(2),ifl,mxsv,i,ipsav(50),impt,ipop,
     1          ilst,ipfl,irecl
c
      real*8 psav(50)
c
      character*20 lnum,att(4)
      character*80 ldat,msg,msg1,emsg
      character*512 lsav
c
      data att /'sequential','list','formatted','write'/
c
c...Call PSTERR Macro
c
      isav(1) = ITYPE
      isav(2) = ISUBT
      lsav   = LPSTWD
      mxsv   = MXCL
      ITYPE  = 2000
      ISUBT  = 1107
      LPSTWD = cmsg1
      msg1 = cmsg1
      MXCL   = 1
      call ppcall (ifl,msg,ierr)
      if (ierr .lt. 0) go to 9100
c
c......Process PSTERR Macro
c
      if (ifl .eq. 0) then
          impt   = IMACPT
          do 50 i=1,mxsv,1
              ipsav(i) = IPSTWD(i)
              psav(i)  = PSTWD(i)
   50     continue
          IPERRF = 0
  100     call precmp (msg,ierr)
          if (ierr .lt. 0) go to 9100
          if (ierr .ne. 0) then
              call lsterr (msg,msg,ierr)
              if (ierr .ne. 0) go to 9100
              if (IMACPT .eq. impt) go to 100
          else
c
c.........PREGEN post command
c
              if (IMACPT .eq. impt .and. ITYPE .eq. 2000) then
                  call pgword (ifl,ilst,msg,ierr)
                  if (ierr .ne. 0) then
                      call lsterr (msg,msg,ierr)
                      if (ierr .ne. 0) go to 9100
                      go to 100
                  else if (ifl .eq. 0) then
                      go to 100
                  else
                      call errtxt ('NESTCALL',msg)
                      call lsterr (msg,msg,ierr)
                      if (ierr .ne. 0) go to 9100
                      go to 100
                  endif

c
c.........Invalid nesting of post commands
c
              else if (IMACPT .eq. impt) then
                  call errtxt ('NESTCALL',msg)
                  call lsterr (msg,msg,ierr)
                  if (ierr .ne. 0) go to 9100
                  go to 100
c
c.........End of PSTERR Macro
c
              else
                  IPC    = LSTPC
              endif
          endif
c
c.........Restore post words
c
  200     ITYPE  = isav(1)
          ISUBT  = isav(2)
          LPSTWD = lsav
          MXCL   = mxsv
          do 300 i=1,MXCL,1
              IPSTWD(i) = ipsav(i)
              PSTWD(i)  = psav(i)
  300     continue
          if (IPERRF .eq. 0) go to 8000
c
      else
          ITYPE  = isav(1)
          ISUBT  = isav(2)
          LPSTWD = lsav
          MXCL   = mxsv
      endif
c
c...WARNING
c
      ipop   = IPRTOP
      IPRTOP = 1
      if (ktyp .eq. 1) then
          if (MAXWRN .eq. -1) go to 8000
          NUMWRN = NUMWRN + 1
          call errtxt ('PSTWRN',ldat)
          call itoc (NUMWRN,lnum,nc,0)
c
c...ERROR
c
      else if (ktyp .eq. 2) then
          if (MAXPER .eq. -1) go to 8000
          NUMERR = NUMERR + 1
          call errtxt ('PSTERR',ldat)
          call itoc (NUMERR,lnum,nc,0)
c
c...FATAL
c
      else if (ktyp .eq. 3) then
          if (MAXFAT .eq. -1) go to 8000
          NUMFAT = NUMFAT + 1
          call errtxt ('PSTFAT',ldat)
          call itoc (NUMFAT,lnum,nc,0)
c
c...APT Source file error
c
      else
          NUMAER = NUMAER + 1
          call errtxt ('PSTAERR',ldat)
          call itoc (NUMAER,lnum,nc,0)
      endif
c
c...Output error count
c
      ipfl   = 0
 1200 call errstr (ldat,lnum,0)
      call itoc (ISN,lnum,nc,0)
      call errstr (ldat,lnum,0)
      call itoc (ICLOUT,lnum,nc,0)
      call errstr (ldat,lnum,0)
c
      call prtout (' ',1)
      nc     = strlen1(ldat)
      call prtout (ldat,nc)
c
c...Output First error message line
c
      if (msg1 .ne. ' ') then
          call errtxt (msg1,ldat)
          nc     = strlen1(ldat)
          if (nc .gt. 0) then
              call prtout (ldat,nc)
              call simerr (ldat,nc,msg,ierr)
              if (ierr .ne. 0) go to 9100
          endif
      endif
c
c...Output Second error message line
c
      if (ipfl .eq. 0 .and. cmsg2 .ne. ' ') then
          call errtxt (cmsg2,ldat)
          nc     = strlen1(ldat)
          if (nc .gt. 0) then
              call prtout (ldat,nc)
              call simerr (ldat,nc,msg,ierr)
              if (ierr .ne. 0) go to 9100
          endif
      endif
c
c...Output post command that caused error
c
      if (ipfl .eq. 0) then
          if (ITYPE .eq. 2000 .and. kprm .ge. 0) then
              call blderr (kprm,ldat,nc)
              if (nc .gt. 0) call prtout (ldat,nc)
          else if (ITYPE .eq. 8000 .and. kprm .ge. 0) then
              ldat   = LPSTWD
              nc     = strlen1(ldat)
              if (nc .gt. 0) call prtout (ldat,nc)
          endif
          call prtout (' ',1)
          if (ktyp .eq. 1 .and. NUMWRN .gt. MAXWRN) go to 9000
          if (ktyp .eq. 2 .and. NUMERR .gt. MAXPER) go to 9000
          if (ktyp .eq. 3 .and. NUMFAT .gt. MAXFAT) go to 9000
      else
          call prtout (' ',1)
      endif
      IPRTOP = ipop
c
c...Suppress output of Punch file on error
c
c
c...Modify to treat APT source error as regular error and
C...not to create punch file
c
       if (NUMAER .ne. 0) then
	      ktyp1 = 2
	   else
	      ktyp1 = ktyp
	   endif
       if (ipfl .eq. 0 .and. ktyp1 .lt. 4 .and. MCHOPT(7) .ne. 0 .and.
     3     ktyp1 .ge. MCHOPT(7) .and. IOPFL(7) .ne. 0) then
c......Close work file and
c......Pre-existing punch file
c
            call clsdel (LUNSC4)
            IOPFL(7) = 0
            irecl = 512
            call opnfil (LUNSC4,PCHFIL,att,irecl,emsg,ierr)
            if (ierr .eq. 0) call clsdel (LUNSC4)
c
c......Output error text and setup for
c......Punch file deleted error
c
            ipfl   = 1
            NUMERR = NUMERR + 1
            call errtxt ('PSTERR',ldat)
            call itoc (NUMERR,lnum,nc,0)
            msg1   = "NOPUNCH"
            go to 1200
       endif
c

c...End of routine
c
 8000 ITYPE  = isav(1)
      ISUBT  = isav(2)
      LPSTWD = lsav
      return
c
c...Maximum number of errors reached
c...Terminate program
c
 9000 call pstsum
      call errtxt ('MANYERRS',msg)
c
c...An I/O error occurred
c
 9100 call errkil (msg,ierr)
      end
c
c***********************************************************************
c
c   SUBROUTINE:  blderr (kprm,cdat,knc)
c
c   FUNCTION:  This routine formats a post-processor command for output
c              and the parameter that is in question will be enclosed in
c              brackets.
c
c   INPUT:  kprm    I*4  D1  -  Location of parameter within post com-
c                               mand that caused error.
c
c   OUTPUT: cdat    C*n  D1  -  Text of post-processor command.
c
c           knc     I*4  D1  -  Number of chars in 'cdat'.
c
c***********************************************************************
c
      subroutine blderr (kprm,cdat,knc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
c
      integer*4 ITYPE,ISUBT,MXCL,IPSTWD(50)
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      character*6 LPSTNM
      equivalence (PSTWD(1),LPSTNM)
c
      integer*4 kprm,knc
c
      character*(*) cdat
c
      integer*4 ist,nc,i
c
      character*2 ldlm
      character*20 ldat
      character*80 tbuf
c
c...Initialize routine
c
      knc    = 0
      ldlm   = '/ '
      if (ITYPE .ne. 2000) go to 8000
c
c...Store major word
c
      ist    = 1
      if (kprm .eq. 0) then
          ist    = 2
          cdat(1:1) = '['
          knc    = 1
      endif
      call getvwd (ISUBT,cdat(ist:len(cdat)),nc,1,PSTWRD,PSTWVL,NPSTWD)
      knc    = knc    + nc
      if (kprm .eq. 0) then
          knc    = knc    + 1
          cdat(knc:knc) = ']'
          go to 8000
      endif
      if (MXCL .eq. 0) go to 7000
c
c...Machine name
c
      ist    = 1
      if (ISUBT .eq. 1015) then
          if (kprm .eq. 1) then
              knc    = knc    + 1
              cdat(knc:knc) = '['
          endif
          tbuf   = cdat(1:knc) // ldlm // LPSTNM
          cdat   = tbuf
          knc    = knc    + 8
          if (kprm .eq. 1) then
              knc    = knc    + 1
              cdat(knc:knc) = ']'
              go to 8000
          endif
          if (MXCL .eq. 1) go to 7000
          ist    = 2
          ldlm   = ', '
      endif
c
c...Store minor word(s)/value(s)
c
      do 1000 i=ist,MXCL,1
c
c......Minor word
c
          if (IPSTWD(i) .ne. 0) then
              call getvwd (IPSTWD(i),ldat,nc,2,PSTWRD,PSTWVL,NPSTWD)
c
c......Value
c
          else
              call rtoc (PSTWD(i),ldat,nc)
          endif
c
c......Store minor word/value in string
c
          if (knc+nc+4 .gt. 80) then
              ist    = index(cdat(40:knc),',')
              tbuf   = cdat(40+ist:knc)
              cdat   = tbuf
              knc    = knc    - (40+ist) + 1
          endif
          if (kprm .eq. i) then
              knc    = knc    + 1
              cdat(knc:knc) = '['
          endif
          tbuf   = cdat(1:knc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          knc    = knc    + nc     + 2
          if (kprm .eq. i) then
              knc    = knc    + 1
              cdat(knc:knc) = ']'
              go to 8000
          endif
          ldlm   = ', '
 1000 continue
c
c...Append brackets to end of command
c
 7000 if (kprm .gt. MXCL) then
          cdat(knc+1:knc+1) = '['
          cdat(knc+2:knc+2) = ']'
          knc    = knc    + 2
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pstsum
c
c   FUNCTION:  This routine outputs the number of errors encountered
c              while processing the current clfile to the screen and
c              print file.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pstsum
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END
c
      equivalence (NUMERR,KPOSMP(0082)), (NUMWRN,KPOSMP(0089))
      equivalence (NUMFAT,KPOSMP(0090)), (NUMAER,KPOSMP(0097))
c
      integer*4 NUMERR,NUMWRN,NUMFAT,NUMAER
c
      integer*4 nc,strlen1, flag
      character*8 lt
      character*11 ld
      character*20 lnum
      character*80 sbuf
      character*256 ldat
      flag = 1
c
c...Set up Post completion message
c
      call errtxt ('PSTSUM',ldat)
      call errstr (ldat,PGMNAM,0)
      call shfile (PCHFIL,sbuf,40)
      call errstr (ldat,sbuf,0)
      call ncdate (ld)
      call errstr (ldat,ld,0)
      call ftim (lt)
      call errstr (ldat,lt,0)
c
c...Output Post completion
c
      nc     = strlen1(ldat)
      if (IOPFL(10) .ne. 3) then
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C          call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
          nc = strlen1 (ldat)
          call add1dispmsg (ldat, nc, flag)
C WNT-END
      endif
c
      call prtout (' ',1)
      call prtout (ldat,nc)
c
c...Set up error summation text
c
      if (NUMWRN .ne. 0 .or. NUMERR .ne. 0 .or. NUMFAT .ne. 0 .or.
     1    NUMAER .ne. 0) then
          if (NUMAER .eq. 0) then
              call errtxt ('PSTERRSM',ldat)
          else
              call errtxt ('PSTERRSN',ldat)
          endif
          call itoc (NUMWRN,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call itoc (NUMERR,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call itoc (NUMFAT,lnum,nc,0)
          call errstr (ldat,lnum,0)
          if (NUMAER .ne. 0) then
              call itoc (NUMAER,lnum,nc,0)
              call errstr (ldat,lnum,0)
          endif
c
c...Output error summation
c
          nc     = strlen1(ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C          if (IOPFL(10) .ne. 3) call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
          nc = strlen1 (ldat)
          if (IOPFL(10) .ne. 3) call add1dispmsg (ldat, nc, flag)
C WNT-END

c
          call prtout (ldat,nc)
      endif
c
c...End of routine
c
 8000 return
      end
