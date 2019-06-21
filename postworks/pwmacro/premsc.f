c
c***********************************************************************
c
c   FILE NAME:  premsc
c   CONTAINS:
c               bldcmd  erronc  fmtcod  fndmch  postit  ppcall  preini
c               getusr  gethst
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        premsc.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        09/26/17 , 11:56:59
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  bldcmd (cdat,cmsg,kerr)
c
c   FUNCTION:  This routine builds a character string representation of
c              the post command that called the current Macro.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1    -  Character string to receive the ascii
c                                 representation of the command.
c
c           cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine bldcmd (cdat,cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (LMACRO,CPOSMP(0861))
c
      character*24 LMACRO
c
      integer*4 kerr
c
      character*(*) cdat,cmsg
c
      integer*2 idesc(2),ityp
      integer*4 nc,strlen1,i,ncc
c
      real*8 rnum,rval
c
      character*2 ldlm
      character*512 ldat,tbuf
c
      integer*4 jnum(2),jdesc(2)
      equivalence (rnum,jnum), (jdesc,idesc)
c
c...Store major word
c
      cdat   = LMACRO
      ncc    = strlen1(cdat)
      ldlm   = '/ '
c
c...Store minor words
c
      ityp   = 12
      do 1000 i=1,IMACHD(5,IMACPT),1
c
c...Changed Macro argument subscripts to I*4
c...Bobby  -  3/24/92
c
c          idesc(3) = i
          jdesc(2) = i
          call lodtxt (ityp,idesc,idesc,ldat,nc,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if (nc .eq. 0 .or. ldat(1:nc) .eq. ' ') then
              jnum(2) = i
              call lodrel (ityp,rnum,rval,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              call rtoc (rval,ldat,nc)
          endif
          tbuf   = cdat(1:ncc) // ldlm // ldat(1:nc)
          cdat   = tbuf
          ncc    = ncc    + 2 + nc
          ldlm   = ', '
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  erronc
c
c   FUNCTION:  This routine outputs the number of errors encountered
c              while processing the current clfile to the screen and
c              listing file.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine erronc
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END
c
      equivalence (NUMERR,KPOSMP(0082))
c
      integer*4 NUMERR
c
      integer*4 nc,strlen1,ierr, flag
c
      character*20 lnum
      character*80 ldat,msg
      flag = 1;
c
c...Set up error summation text
c
      call errtxt ('ERRSUM',ldat)
      call errstr (ldat,PGMNAM,0)
      if (IOPFL(4) .eq. 1) then
          call shfile (LOBJFI,msg,60)
          call errstr (ldat,msg,0)
      else
          call errstr (ldat,' ',0)
      endif
      call itoc (NUMERR,lnum,nc,0)
      call errstr (ldat,lnum,0)
c
c...Output error summation
c
      nc     = strlen1(ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      call add1dispmsg (ldat, nc, flag)
C WNT-END
c
      call lstout (' ',1,msg,ierr)
      call lstout (ldat,nc,msg,ierr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  subroutine fmtcod (kreg,gval,cdat,knc)
c
c   FUNCTION:  This routine formats a code, including beginning and
c              trailing characters, for output.
c
c   INPUT:  kreg    I*4  D1   -  Register to format for output.  For
c                                kreg < 0 register value is not output
c
c           gval    R*8  D1   -  Register value.
c
c   OUTPUT: cdat    C*n  D1   -  Text string representation of register
c                                and value.
c
c           knc     I*4  D1   -  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine fmtcod (kreg,gval,cdat,knc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*4 kreg,knc
c
      real*8 gval
c
      character*(*) cdat
c
      character*24 lbuf
      character*60 obuf
      character*1 bslash
      integer*4 ireg,i,nc
C WNT-START
      data bslash /'\'/
C WNT-END
C SGI-SUN-HPX-IBM-START
C     data bslash /'\\'/
C SGI-SUN-HPX-IBM-END
c
c...Format register for output
c
      ireg = kreg
      if (kreg .lt. 0) then
          ireg = -kreg
          knc  = 0
      else
          call ftoc (gval,cdat,knc,FMTDES(1,ireg))
      end if
c
      do 100 i=1,2,1
          if (i .eq. 1) then
              nc     = REGBNC(ireg)
              if (nc .ne. 0) lbuf   = REGST(ireg)(1:nc)
          else
              nc     = REGENC(ireg)
              if (nc .ne. 0) lbuf   = REGEN(ireg)(1:nc)
          endif
c
          if (nc .ne. 0) then
              if (lbuf(nc:nc) .eq. bslash) then
                  if (nc .gt. 1 .and. lbuf(nc-1:nc-1) .eq. bslash) then
                      nc     = nc     - 1
                  else
                      lbuf(nc:nc) = ' '
                  endif
              endif
c
              if (knc .ne. 0) then
                  if (i .eq. 1) then
                      obuf   = lbuf(1:nc) // cdat(1:knc)
                  else
                      obuf   = cdat(1:knc) // lbuf(1:nc)
                  endif
              else
                  obuf   = lbuf(1:nc)
              endif
              cdat   = obuf
              knc    = knc    + nc
          endif
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fndmch (cmsg,kerr)
c
c   FUNCTION:  This routine searches the clfile for the MACHIN/pstnam
c              card and stores its parameters.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine fndmch (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (IFIREC,KPOSMP(0057)), (IFIPT ,KPOSMP(0058))
c
      integer*4 ITYPE,ISUBT,MXCL,IPSTWD(50),IFIREC,IFIPT
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      character*6 LPSTNM
      equivalence (PSTWD(1),LPSTNM)
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i,nc
c
      character*6 lpgm
c
      data lpgm /'PMACRO'/
c
c...Read clfile record
c
  100 call clread (IFIREC,IFIPT,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Is this a MACHIN/PMACRO card
c
      if (ITYPE .eq. 2000 .and. ISUBT .eq. 1015 .and.
     1    LPSTNM .eq. lpgm) then
          if (MXCL .eq. 1 .or. MXCL .gt. 11) go to 9100
          do 500 i=2,MXCL,1
              if (IPSTWD(i) .ne. 0) go to 9100
              PREPT  = PREPT  + 1
              PRENUM(PREPT) = PSTWD(i)
              call itoc (PRENUM(PREPT),PSTNAM(PREPT),nc,0)
  500     continue
c
c...FINI card encountered
c
      else if (ITYPE .eq. 14000) then
          go to 9000
c
c...All other post commands
c
      else
          go to 100
      endif
c
c...End of routine
c
 8000 return
c
c...MACHIN card not found
c
 9000 call errtxt ('NOMACHIN',cmsg)
      kerr   = 1
      go to 8000
c
c...MACHIN card syntax error
c
 9100 call errtxt ('MACHSYN',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  postit (cmsg,kerr)
c
c   FUNCTION:  This routine runs the encountered MACHIN post-processors
c              against the newly created clfile.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine postit (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 istat,i,nc
c
      character*(MAX_PATH+40) lcmd
C VAX-START
C     integer*4 lib$spawn
C VAX-END
C WNT-SUN-SGI-IBM-HPX-DOS-DEC-START
      integer*4 system
      character*6 lmch
C WNT-SUN-SGI-IBM-HPX-DOS-DEC-END
c
c...Run MACHIN card post(s)
c
      do 1000 i=1,MCHPT,1

c
c......Build operating system command
c
C VAX-START
C         lcmd   = MCHNAM(i) // ' ' // LOBJFI(1:NOBJFI)
C VAX-END
C WNT-SUN-SGI-IBM-HPX-DOS-DEC-START
          call tolowr (MCHNAM(i),lmch)
          lcmd   = lmch // ' ' // LOBJFI(1:NOBJFI)
C WNT-SUN-SGI-IBM-HPX-DOS-DEC-END
          nc     = NOBJFI + 7
c
c......Spawn post-processor
c
C VAX-START
C         istat  = lib$spawn (lcmd(1:nc))
C         if (.not. istat) go to 9000
C VAX-END
C WNT-SUN-SGI-IBM-HPX-DOS-DEC-START
          istat  = system (lcmd(1:nc))
          if (istat .ne. 0) go to 9000
C WNT-SUN-SGI-IBM-HPX-DOS-DEC-END
 1000 continue
c
c...End of routine
c
 8000 return
c
c...Error trying to spawn sub-process
c
 9000 kerr   = 1
      call errtxt ('SPAWN',cmsg)
      call errstr (cmsg,MCHNAM(i),1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ppcall (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine determines whether a Macro has been defined
c              for the current cl record.  If one has, then the Macro
c              header will be placed on the calling stack and the PC
c              will be modified to point to the beginning of the macro.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ppcall (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (LSTPC ,KPOSMP(0083)), (IERRPC,KPOSMP(0085))
      equivalence (TOOLFL,KPOSMP(1804))
c
      integer*4 ITYPE,ISUBT,MXCL,IPSTWD(50),LSTPC,IERRPC,TOOLFL(20)
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      character*6 LPSTNM
      equivalence (PSTWD(1),LPSTNM)
c
      equivalence (ERRMAC ,CPOSMP(0193)), (LPSTWD,CPOSMP(0217))
      equivalence (LMACRO ,CPOSMP(0861))
c
      character*24 LMACRO,ERRMAC
      character*512 LPSTWD
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*2 idesc(4),ityp
      integer*4 i,jdat(5),nc,ierr,mxc,inc,ipt,isub,ivar
c
      real*8 rnum
c
      character*24 lbuf
      character*512 ldat
c
      integer*2 idat(10)
      integer*4 jdesc(2)
      equivalence (jdat,idat), (jdesc,idesc)
c
c...Initialize routine
c
      kerr   = 0
      kfl    = 1
      isub   = ISUBT
      if (ITYPE .eq. 5000) isub = 4013
c
c...If we are at the maximum
c...level of calls or this Macro
c...was already called, then don't
c...check for Macro definition
c
      if (IMACPT .eq. MAXMAC) go to 8000
c
      if (IMACPT .ne. 0) then
          do 100 i=1,IMACPT,1
              if (isub .eq. IMACHD(1,i)) go to 8000
  100     continue
      endif
c
c...Check for Macro definition
c
      call lodmac (isub,jdat)
      if (jdat(1) .eq. 0 .or. idat(9) .eq. 0) go to 8000
c
c...Found Macro
c...Load its header and call it
c
      if (IMACPT .ne. 0) IMACHD(6,IMACPT) = IPC
      IMACPT = IMACPT + 1
      IMACHD(1,IMACPT) = jdat(1)
      IMACHD(2,IMACPT) = idat(10)
      IMACHD(3,IMACPT) = jdat(3)
      IMACHD(4,IMACPT) = jdat(4)
c      IMACHD(2,IMACPT) = idat(5)
c      IMACHD(3,IMACPT) = idat(6)
c      IMACHD(4,IMACPT) = idat(7)
      IMACHD(5,IMACPT) = 0
c
      if (IMACPT .ne. 1 .and. jdat(1) .eq. 1103) then
          IERRPC = LSTPC
          ERRMAC = LMACRO
      endif
c
      IPC    = jdat(2)
c      IPC    = idat(4)
      call getvwd (jdat(1),LMACRO,nc,1,PSTWRD,PSTWVL,NPSTWD)
      kfl    = 0
c
c...Load the argument arrays
c
      ierr   = 0
      mxc    = MXCL
c
c......Motion
c
      if (ITYPE .eq. 5000) then
          if (IMACHD(2,IMACPT) .eq. 0) then
              mxc    = 0
              ierr   = 1
          else
              mxc    = 1
              ityp   = 1
              ivar   = IMACHD(3,IMACPT)
              rnum   = MXCL
              call storel (ityp,ivar,ivar,rnum,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (IMACHD(2,IMACPT) .ge. 2) then
                  mxc    = 2
                  ityp   = 1
                  ivar   = IMACHD(3,IMACPT) + 1
                  rnum   = ISUBT
                  call storel (ityp,ivar,ivar,rnum,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif
          endif
c
c......Text string command
c......LETTER  PPRINT  PARTNO  INSERT  PRINT
c
      else if ((ISUBT .ge. 1043 .and. ISUBT .le. 1046) .or.
     1         ISUBT .eq. 1102 .or. ISUBT .eq. 1103 .or.
     2         ISUBT .eq. 1107 .or. ISUBT .eq. 1199) then
c
c.........Store all characters of PPRINT statement
c.........otherwise shorter PPRINT text will
c.........have the previous longer PPRINT text
c.........appended to it
c.........Bobby  -  08/03/12
c
cc          nc     = strlen1(LPSTWD)
cc          if (nc .gt. IMACHD(2,IMACPT)*8) then
cc              ierr   = 1
cc              nc     = IMACHD(2,IMACPT) * 8
cc          endif
          if (nc .gt. IMACHD(2,IMACPT)*8) ierr = 1
          nc = IMACHD(2,IMACPT) * 8
          ityp   = 4
          jdesc(1) = IMACHD(4,IMACPT)
          idesc(3) = 1
          idesc(4) = IMACHD(2,IMACPT) * 8
          call stotxt (ityp,idesc,LPSTWD,nc,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          mxc    = 1
          if (ISUBT .eq. 1044) then
              ityp   = 1
              ivar   = IMACHD(3,IMACPT) + 1
              rnum = 0
              if (TOOLFL(19) .eq. -1) then
                  rnum   = 1
                  TOOLFL(19) = 1
              endif
              call storel (ityp,ivar,ivar,rnum,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              mxc    = 2
          endif
c
c......Standard post word
c
      else
          if (MXCL .eq. 0) go to 8000
          if (mxc .gt. IMACHD(2,IMACPT)) then
              ierr   = 1
              mxc    = IMACHD(2,IMACPT)
          endif
c
c.........MACHIN
c
          ipt    = 1
          inc    = 1
          if (ISUBT .eq. 1015 .and. mxc .ne. 0) then
              ldat(1:8) = LPSTNM
              ipt    = 2
              inc    = 9
          endif
c
c.........Store minor word/value arguments
c
          do 1000 i=ipt,mxc,1
              if (IPSTWD(i) .ne. 0) then
                  call getvwd (IPSTWD(i),lbuf,nc,2,PSTWRD,PSTWVL,NPSTWD)
                  ldat(inc:inc+23) = lbuf
              else
                  ityp   = 1
                  ivar   = IMACHD(3,IMACPT) + i - 1
                  call storel (ityp,ivar,ivar,PSTWD(i),cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  ldat(inc:inc+23) = '        '
              endif
              inc    = inc    + 24
 1000     continue
c
c.........Store text arguments
c
          ityp   = 4
          jdesc(1) = IMACHD(4,IMACPT)
          idesc(3) = 1
          idesc(4) = inc    - 1
          nc     = inc    - 1
          call stotxt (ityp,idesc,ldat,nc,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
      IMACHD(5,IMACPT) = mxc
      if (ierr .ne. 0) go to 9000
c
c...End of routine
c
 8000 return
c
c...Too many arguments
c
 9000 call errtxt ('MAXARG',cmsg)
      LSTPC  = IPC
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  preini
c
c   FUNCTION:  This routine defines program variables.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine preini
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'clnrd.inc'
      include 'post.inc'
c
      equivalence (MULTAX,KPOSMP(0056)), (NPT   ,KPOSMP(0059))
      equivalence (IERROR,KPOSMP(0081))
      equivalence (NUMERR,KPOSMP(0082)), (LSTPC ,KPOSMP(0083))
      equivalence (ICLOUT,KPOSMP(0084)), (IERRPC,KPOSMP(0085))
c
      integer*4 MULTAX,IERROR,NUMERR,LSTPC,ICLOUT,IERRPC,NPT
c
      equivalence (CLSAV ,POSMAP(0738)), (CUTTER,POSMAP(0744))
c
      real*8 CLSAV(6),CUTTER(7)
c
      equivalence (ERRMAC,CPOSMP(0193)), (LMACRO,CPOSMP(0861))
c
      character*24 ERRMAC,LMACRO
c
      integer*4 i
c
c...Initialize common variables
c
      CLSAV(1) = 0.
      CLSAV(2) = 0.
      CLSAV(3) = 0.
      CLSAV(4) = 0.
      CLSAV(5) = 0.
      CLSAV(6) = 1.
      do 200 i=1,7,1
          CUTTER(i) = 0.
  200 continue
c
      ERRMAC = ' '
c
      FIOFN  = 0
      do 250 i=1,5,1
          FIOPND(i) = 0
  250 continue
c
      ICLOUT = 0
      IERROR = 0
      IERRPC = 0
      IFTOL  = 0.
      ILSTOP = 1
      IMACPT = 0
      IMULTO = -1
      INEREC = 1
      INEPT  = 0
      IPC    = 0
      IPNTX  = 1
c
      LISLIN = IOPFL(3) + 1
      LISPAG = 0
      LMACRO = ' '
      LSTPC  = 0
c
      MAXMAC = 20
      MAXPRN = 50
      MCHPT  = 0
      MULTAX = 0
c
      NPT    = 3
      NUMERR = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getusr (cout,knc)
c
c   FUNCTION:  This routine returns the name of the active user.
c
c   INPUT:  none.
c
c   OUTPUT: cout    C*n  D1    -  Username.
c
c           knc     I*n  D1    -  Number of chars in 'cout'.
c
c***********************************************************************
c
      subroutine getusr (cout,knc)
c
      integer*4 knc
c
      character*(*) cout
c
C WNT-START
      integer*4 inc,getenvc
C WNT-END
C SUN-SGI-IBM-START
C     integer*4 strlen1
C SUN-SGI-IBM-END
c
c......Get User name
c
C WNT-START
      inc = 8
      knc = getenvc ('USERNAME',cout,inc)
      cout(knc+1:) = ' '
C WNT-END
C SUN-SGI-IBM-START
C     call getenv ('USER',cout)
C     knc = strlen1(cout)
C SUN-SGI-IBM-END
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gethst (cout,knc)
c
c   FUNCTION:  This routine returns the name of the active computer we
c              are running on.
c
c   INPUT:  none.
c
c   OUTPUT: cout    C*n  D1    -  Username.
c
c           knc     I*n  D1    -  Number of chars in 'cout'.
c
c***********************************************************************
c
      subroutine gethst (cout,knc)
c
      integer*4 knc
c
      character*(*) cout
c
C WNT-START
      integer*4 inc,getenvc
C WNT-END
C SUN-SGI-IBM-START
C     integer*4 strlen1
C SUN-SGI-IBM-END
c
c......Get Computer name
c
C WNT-START
      inc = 12
      knc = getenvc ('COMPUTERNAME',cout,inc)
      cout(knc+1:) = ' '
C WNT-END
C SUN-SGI-IBM-START
C     call getenv ('HOSTNAME',cout)
C     knc = strlen1(cout)
C SUN-SGI-IBM-END
c
c...End of routine
c
 8000 return
      end	
c
c***********************************************************************
c
c   SUBROUTINE:  getlic (cout,knc)
c
c   FUNCTION:  This routine returns the license code of computer.
c             it compile with company name and id
c
c   INPUT:  none.
c
c   OUTPUT: cout    C*n  D1    -  Username.
c
c           knc     I*n  D1    -  Number of chars in 'cout'.
c
c***********************************************************************
c
      subroutine getlic (cout,knc)
c
      include 'passwd.inc'
c
      integer*4 knc
c
      character*(*) cout
c
      knc = LICNC(8)
      cout(1:knc) = LICDAT(8)
      cout(knc+1:knc+LICNC(1)+1) = LICDAT(1)
      knc = knc + LICNC(1)
      cout(knc+1:) = ' '
c
c...End of routine
c
 8000 return
      end

