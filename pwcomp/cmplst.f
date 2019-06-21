c
c***********************************************************************
c
c   FILE NAME: cmplst.for
c   CONTAINS:
c               lstdat  lsterr  lsthed  lstout  lstps2  docout
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmplst.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:12
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  lstdat (cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine formats and outputs a MACRO input line to
c              the listing file.
c
c   INPUT:  cdat    C*n  D1  -  Line from input file.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lstdat (cdat,knc,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 knc,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 nc
c
      character*20 lnum
      character*132 ldat,tbuf
c
c...Build listing record
c
      call itoc (ICURLN,ldat,nc,5)
      call itoc (IPC,lnum,nc,6)
      tbuf   = ldat(1:5) // '  ' // lnum(1:nc) // '   '
      ldat   = tbuf
      nc     = nc     + 10
      tbuf   = ldat(1:nc) // cdat(1:knc)
      ldat   = tbuf
      nc     = nc     + knc
c
c...Write listing record
c
      call lstout (ldat,nc,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lsterr (cdat,ctxt,cmsg,kerr)
c
c   FUNCTION:  This routine formats and outputs a compilation error
c              message to the listing file and screen.
c
c   INPUT:  cdat    C*n  D1  -  Line from input file that caused the
c                               error.
c
c           ctxt    C*n  D1  -  Text of error message to output.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lsterr (cdat,ctxt,cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END
c
      integer*4 kerr
c
      character*(*) cdat,cmsg,ctxt
c
      integer*4 nc,strlen1, flag
c
      character*20 lnum
      character*132 ldat
      flag = 1
c
c...Increment error count
c
      NERR   = NERR   + 1
c
c...Format error count & line number
c...for 1st pass
c
      if (IPASS .eq. 1) then
          call errtxt ('ERRLIN',ldat)
          call itoc (NERR,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call itoc (ICURLN,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call errstr (ldat,LMACRO,0)
          nc     = strlen1(ldat)
c
c...Format error count & line number
c...for 2nd pass
c
      else
          call errtxt ('ERRLIN2',ldat)
          call itoc (NERR,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call itoc (IPC,lnum,nc,0)
          call errstr (ldat,lnum,0)
          call errstr (ldat,LMACRO,0)
          nc     = strlen1(ldat)
      endif
c
c...Output error count & line number
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      call add1dispmsg(' ', 1, flag)
      nc = strlen1 (ldat)
      call add1dispmsg(ldat, nc, flag)
C WNT-END
      call lstout (ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Output error message & input line
c
      nc     = strlen1(ctxt)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call trmmsg (ctxt)
C      call lstout (ctxt,nc,cmsg,kerr)
C      if (kerr .ne. 0) go to 8000
c
C      nc     = strlen1(cdat)
C      call trmmsg (cdat)
C      call lstout (cdat,nc,cmsg,kerr)
C      if (kerr .ne. 0) go to 8000
c
C      call trmmsg (' ')
C      call lstout (' ',1,cmsg,kerr)
C      if (kerr .ne. 0) go to 8000
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      nc     = strlen1(ctxt)
      call add1dispmsg(ctxt, nc, flag)
      call lstout (ctxt,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      nc     = strlen1(cdat)
      call add1dispmsg (cdat, nc, flag)
      call lstout (cdat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      call lstout (' ',1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000

C WNT-END
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lsthed (cmsg,kerr)
c
c   FUNCTION:  This routine outputs the listing file header.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lsthed (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 nc,nc1,nc2,ipt,strlen1
c
      character*20 lnum,sb1,sb2
      character*132 ldat,tbuf
c
c...If 1st time here then create header
c
      if (LISPAG .eq. 0) then
          call getsap ('CMPHD1',ipt,IPRMDS,SALABL)
          call itoc (PWVER1,sb1,nc1,1)
          call itoc (PWVER2,sb2,nc2,1)
          LCMPH1 = SAPRM(ipt)
          call errstr (LCMPH1,PGMNAM,1)
          call errstr (LCMPH1,sb1(1:nc1),1)
          call errstr (LCMPH1,sb2(1:nc2),1)
          call errstr (LCMPH1,LDATE,0)
          call errstr (LCMPH1,LTIME,0)
          NCMPH1 = strlen1(LCMPH1)
c
          call getsap ('CMPHD2',ipt,IPRMDS,SALABL)
          LCMPH2 = SAPRM(ipt)
          NCMPH2 = SAPNC(ipt)
          call shfile (LCMPFI(1:NCCMPF),ldat,80)
          nc     = strlen1(ldat)
          tbuf   = LCMPH2(1:NCMPH2) // '  ' // ldat
          LCMPH2 = tbuf
          NCMPH2 = NCMPH2 + nc     + 2
c
          call getsap ('CMPHD3',ipt,IPRMDS,SALABL)
          LCMPH3 = SAPRM(ipt)
          NCMPH3 = SAPNC(ipt)
c
          call getsap ('CMPHD4',ipt,IPRMDS,SALABL)
          LCMPH4 = SAPRM(ipt)
          NCMPH4 = SAPNC(ipt)
      endif
c
c...Write header record
c
      call wrtxt (LUNSC2,LFORMF,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
      LISPAG = LISPAG + 1
      call itoc (LISPAG,lnum,nc,0)
      if (IPASS .eq. 1) then
          ldat   = LMACRO(1:8) // LCMPH1(1:NCMPH1) // ' ' // lnum(1:nc)
      else
          ldat   = '        ' // LCMPH1(1:NCMPH1) // ' ' // lnum(1:nc)
      endif
      nc     = 8 + NCMPH1 + 1 + nc
c
      call wrtxt (LUNSC2,ldat,nc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call wrtxt (LUNSC2,LCMPH2,NCMPH2,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      call wrtxt (LUNSC2,' ',1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IPASS .eq. 1) then
          call wrtxt (LUNSC2,LCMPH3,NCMPH3,cmsg,kerr)
      else
          call wrtxt (LUNSC2,LCMPH4,NCMPH4,cmsg,kerr)
      endif
      if (kerr .ne. 0) go to 8000
      call wrtxt (LUNSC2,' ',1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      LISLIN = 5
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstout (cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine writes a line to the listing file.
c
c   INPUT:  cdat    C*n  D1  -  Text of line to write.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lstout (cdat,knc,cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 nc
c
c...Write listing header
c
      if (IOPFL(2) .eq. 1) then
          if (LISLIN .ge. IOPFL(3)) call lsthed (cmsg,kerr)
c
c...Write listing record
c
          nc     = knc
          if (nc .gt. 132) nc = 132
          call wrtxt (LUNSC2,cdat,nc,cmsg,kerr)
          LISLIN = LISLIN + 1
      endif
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lstps2 (cmac,kpcs,kpce,karg,cmsg,kerr)
c
c   FUNCTION:  This routine formats and outputs a second pass MACRO sum-
c              mation line.
c
c   INPUT:  cmac    C*n  D1  -  Macro name.
c
c           kpcs    I*4  D1  -  Starting PC of Macro.
c
c           kpce    I*4  D1  -  Ending PC of Macro.
c
c           karg    I*4  D1  -  Number of arguments for Macro.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine lstps2 (cmac,kpcs,kpce,karg,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kpcs,kpce,karg,kerr
c
      character*(*) cmac,cmsg
c
      integer*4 inum,nc
c
      character*20 lnum
      character*132 ldat,tbuf
c
c...Build listing record
c
      call itoc (kpcs,lnum,nc,6)
      ldat   = cmac(1:8) // '    ' // lnum(1:nc)
c
      call itoc (kpce,lnum,nc,6)
      tbuf   = ldat(1:18) // '    ' // lnum(1:nc)
      ldat   = tbuf
c
      inum   = kpce   - kpcs   + 4
      call itoc (inum,lnum,nc,6)
      tbuf   = ldat(1:28) // '    ' // lnum(1:nc)
      ldat   = tbuf
c
      call itoc (karg,lnum,nc,2)
      if (lnum(1:1) .eq. '0') lnum(1:1) = ' '
      tbuf   = ldat(1:38) // '      ' // lnum(1:nc)
      ldat   = tbuf
      nc     = 46
c
c...Write listing record
c
      call lstout (ldat,nc,cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  docout (cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine writes a line to the documentation file.
c
c   INPUT:  cdat    C*n  D1  -  Text of line to write.
c
c           knc     I*4  D1  -  Number of characters in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine docout (cdat,knc,cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 knc,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 nc,i
c
c...Write listing header
c
      if (IOPFL(5) .eq. 1) then
          if (DOCLIN .ge. IOPFL(3)) then
              call wrtxt (LUNSC4,LFORMF,1,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              do 100 i=1,4,1
                  call wrtxt (LUNSC4,' ',1,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
  100         continue
              DOCLIN = 4
          endif
c
c...Write listing record
c
          nc     = knc
          if (nc .gt. 132) nc = 132
          call wrtxt (LUNSC4,cdat,knc,cmsg,kerr)
          DOCLIN = DOCLIN + 1
      endif
c
c...End of routine
c
 8000 return
      end
