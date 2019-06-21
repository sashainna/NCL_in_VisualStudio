c
c***********************************************************************
c
c   FILE NAME: errmsg.for
c   CONTAINS:
c               errhnd  errkil  errmsg  errstr  errsum  errtxt
c
c     COPYRIGHT 1997 (c) NFumerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        errmsg.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:41:14
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  errhnd (cvax,kvaxnc,crms,krmsnc)
c
c   FUNCTION:  This routine returns the text of the last Operating sys-
c              tem error and RMS error (VAX/VMS) and is usually called
c              after a File I/O error occurred.
c
c   INPUT:  none.
c
c   OUTPUT: cvax    C*n  D1  -  Returns the text of the Operating sys-
c                               tem error message.
c
c           kvaxnc  I*4  D1  -  Number of characters in 'cvax'.
c
c           crms    C*n  D1  -  Returns the text of the RMS (VAX/VMS)
c                               error message.
c
c           krmsnc  I*4  D1  -  Number of characters in 'crms'.
c
c***********************************************************************
c
      subroutine errhnd (cvax,kvaxnc,crms,krmsnc)
c
c...not necessary to use these lib here, and it
c...cause conflicks with MFC
c
C WNT-START
c...      use DFPORT
c...      use DFLIB
C WNT-END

      integer*4 kvaxnc,krmsnc
c
      character*(*) cvax,crms
c
c      include 'menu.inc'
c
      integer*4 strlen1, num, i
C WNT-VAX-DOS-START
      integer*4 ierr,irms,istv,iunit,ivax,index,nc
C WNT-VAX-DOS-END
c
      character*80 ldat
C VAX-START
C
C...Get last System error numbers
C
C      call errsns (ierr,irms,istv,iunit,ivax)
C
C...Get the Operating system error message text
C
C      if (ivax .eq. 0) then
C          cvax   = ' '
C          kvaxnc = 0
C      else
C          ierr   = lib$sys_getmsg (ivax,nc,ldat)
C          ierr   = index(ldat,'!')
C          if (ierr .gt. 1) ldat = ldat(1:ierr-1)
C          cvax   = 'VMS - ' // ldat
C          kvaxnc = strlen1(cvax)
C      endif
C
C...Get the RMS error message text
C
C      if (irms .eq. 0) then
C          crms   = ' '
C          krmsnc = 0
C      else
C          ierr   = lib$sys_getmsg (irms,nc,ldat)
C          crms   = 'RMS - ' // ldat
C          krmsnc = strlen1(crms)
C      endif
C VAX-END
C SUN-SGI-CIM-IBM-DEC-HPX-START
c
c...Get the Operating system error message text
c
C      call gerror (ldat)
C      cvax   = 'UNIX - ' // ldat
C      kvaxnc = strlen1(cvax)
C      cvax   = cvax(1:kvaxnc) // '\0'
C      crms   = ' '
C      krmsnc = 0
C SUN-SGI-CIM-IBM-DEC-HPX-END
C WNT-START
C
C...Get last System error numbers
C
      call errsns (ierr,irms,istv,iunit,ivax)
      if (ierr .eq. 0) then
          cvax   = ' '
          kvaxnc = 0
      else
c
c...Get the Operating system error message text
c
          if (ierr .eq. 28) then
              ldat = "CLOSE error"
          else if (ierr .eq. 29) then
              ldat = "File not found"
          else if (ierr .eq. 30) then
              ldat = "Open failure"
          else if (ierr .eq. 36) then
              ldat = "Attempt to access non_existent record"
          else if (ierr .eq. 38) then
              ldat = "Error during write"
          else if (ierr .eq. 39) then
              ldat = "Error during read"
          else if (ierr .eq. 43) then
              ldat = "File name specification error"
          else if (ierr .eq. 46) then
              ldat = "Inconsistent OPEN/CLOSE parameters"
          else if (ierr .eq. 47) then
              ldat = "Write to READONLY file"
          else if (ierr .eq. 51) then
              ldat = "Inconsistent file organization"
          else if (ierr .eq. 55) then
              ldat = "Delete error"
          else if (ierr .eq. 88) then
              ldat = "Floating overflow in math library"
          else if (ierr .eq. 89) then
              ldat = "Floating underflow in math library"
          else
              ldat = "Unknown error code: "
              write (ldat(20:),10) ierr
   10         format (i5)
          endif
      endif
      cvax   = 'WINNT - ' // ldat
      kvaxnc = strlen1(cvax)
      cvax   = cvax(1:kvaxnc)
      crms   = ' '
      krmsnc = 0
C WNT-END
C DOS-START
C      call iostat_msg (IBMERR,crms)
C      cvax   = 'DOS - F77L3 error - '
C      ierr   = iand (IBMERR,4095)
C      write (cvax(23:),10) ierr
C  10  format (i5)
C      kvaxnc = strlen1 (cvax)
C      krmsnc = strlen1 (crms)
C DOS-END
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  errkil (cmsg,kerr)
c
c   FUNCTION:  This routine should only be called when a terminal error
c              occurs.  It outputs a termination error message and exits
c              the program. This function could not be called by C++ routine
c
c   INPUT:  cmsg    C*n  D1  -  Text of error message from routine that
c                               got the error.
c
c           kerr    I*4  D1  -  Contains the error condition code from
c                               the routine that got the error.
c
c***********************************************************************
c
      subroutine errkil (cmsg,kerr)
c
C WNT-START
      include 'postworks_nt.inc'
C WNT-END

      integer*4 kerr, flag,strlen1,nc
c
      character*(*) cmsg
c
      integer*4 ivnc,irnc
c
      character*80 vmsg,rmsg
      character cchar
c
c...An error occurred during execution of program
c...Output error message & terminate program
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call trmmsg (' ')
C      call trmmsg (cmsg)
C      if (kerr .lt. 0) then
C         call errhnd (vmsg,ivnc,rmsg,irnc)
C         if (ivnc .ne. 0) then
C             call trmmsg (vmsg)
C         endif
C         if (irnc .ne. 0) then
C             call trmmsg (rmsg)
C         endif
C      endif
c
C      call trmmsg (' ')
c
c...Terminate process
c
C      call trmrst
C      call clsfil (0)
C      call exit
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      flag = 1
      nc = strlen1(cmsg)
      call add1dispmsg(cmsg,nc,flag)
c
c...don't display those message to customer
c
c...      if (kerr .lt. 0) then
c...          call errhnd (vmsg,ivnc,rmsg,irnc)
c...          if (ivnc .ne. 0) then
c...              call add1dispmsg(vmsg,ivnc,flag)
c...          endif
c...          if (irnc .ne. 0) then
c...              call add1dispmsg(rmsg,irnc,flag)
c...          endif
c...      endif
      call clsfil (0)
      return
C WNT-END
      end
c
c***********************************************************************
c
c   SUBROUTINE:  errmsg (cmsg,ktyp,klin)
c
c   FUNCTION:  This routine displays an error message on the terminal
c              screen.
c
c   INPUT:  cmsg    I*4  D1  -  Contains the label of the error message
c                               when 'ktyp' = 1.  Contains the error
c                               message text when 'ktyp' = 2.
c
c           ktyp    I*4  D1  -  1 = Get error message from file.  2 =
c                               'cmsg' contains the error message.
c
c           klin    I*4  D1  -  1 = Display error message on the 1st
c                               error line.  2 = Display error message
c                               on the 2nd error line.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-START
      subroutine errmsg (cmsg,ktyp,klin)
c
      include 'menu.inc'
c
      integer*4 ktyp,klin
c
      character*(*) cmsg
c
      integer*4 nc,ilin,strlen1
c
      character*80 sbuf
c
c...Actual message is provided
c
      if (ktyp .eq. 2) then
      sbuf   = cmsg
c
c...Label to message is provided
c
      else
      call errtxt (cmsg,sbuf)
      endif
c
c...Display error message
c
      if (MOTIF .eq. 1) then
          if (cmsg .ne. 'NOTAPPLY') then
              MFERR  = 1
              MFERTX = sbuf
          endif
      else
          nc     = strlen1(sbuf)
          ilin   = IERRLN + klin
          call plott (ilin,1)
          call dmpbuf (sbuf,nc)
          call clreol
      endif
c
      return
      end
c
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-WNT-END
c***********************************************************************
c
c   SUBROUTINE:  errstr (cmsg,cdat,kfl)
c
c   FUNCTION:  This routine parse a text string containing a '%s' sub-
c              string and replaces it with a specified string.
c
c   INPUT:  cmsg    C*n  D1  -  Text string that contains the sub-string
c                               '%s'.  This string will also be output
c                               with the substitution made.
c
c           cdat    C*n  D1  -  Text string that will replace the '%s'
c                               sub-string in 'cmsg'.
c
c           kfl     I*4  D1  -  If the sub-string '%s' is not found,
c                               then; 0 = do not modify 'cmsg'.  1 =
c                               append 'cdat' to the end of 'cmsg'.
c
c   OUTPUT: cmsg
c
c***********************************************************************
c
      subroutine errstr (cmsg,cdat,kfl)
c
      integer*4 kfl
c
      character*(*) cmsg,cdat
c
      integer*4 i,nc1,nc2,strlen1
c
      character*132 ldat
c
c...Find %s string in 'cmsg'
c
      nc1    = strlen1(cmsg)
      nc2    = strlen1(cdat)
      i      = index(cmsg,'%s')
      ldat   = cmsg
c
c...Replace %s with 'cdat'
c
      if (i .ne. 0) then
      if (i .eq. 1) then
          cmsg   = cdat(1:nc2) // ldat(i+2:nc1)
      else
          cmsg   = ldat(1:i-1) // cdat(1:nc2) // ldat(i+2:nc1)
      endif
c
c...%s was not found
c
      else
         if (kfl .eq. 1) cmsg   = ldat(1:nc1) // ' ' // cdat
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  errsum
c
c   FUNCTION:  This routine outputs the number of errors encountered
c              to the screen and listing file.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine errsum
c
      include 'menu.inc'
C WNT-START
      include 'postworks_nt.inc'
C WNT-END
c
      integer*4 nc,strlen1,ierr, flag
c
      character*20 lnum
      character*80 ldat,msg
c
c...Set up error summation text
c
      call errtxt ('ERRSUM',ldat)
      call errstr (ldat,PGMNAM,0)
      call shfile (LCMPFI,msg,60)
      call errstr (ldat,msg,0)
      call itoc (NERR,lnum,nc,0)
      call errstr (ldat,lnum,0)
c
c...Output error summation
c
      nc     = strlen1(ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C      call trmmsg (ldat)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      flag = 1
      call add1dispmsg(ldat,nc,flag)
      call lstout (' ',1,msg,ierr)
      call lstout (ldat,nc,msg,ierr)
C WNT-END
c
c...End of routine
c
 8000 return
      end
c
C***********************************************************************
c
C   SUBROUTINE:  errtxt (clab,cmsg)
c
C   FUNCTION:  This routine returns the error text for the input label.
c
C   INPUT:  clab    C*8  D1  -  Contains the label of the error mes-
C                               sage.
c
C   OUTPUT: cmsg    C*n  D1  -  Returns the text of the error message.
c
C***********************************************************************
c
      subroutine errtxt (clab,cmsg)
c
      include 'menu.inc'
c
      character*(*) clab,cmsg
c
      integer*4 ipt
c
c...Get error message text
c
      call getsap (clab,ipt,IERRDS,ERLABL)
      cmsg   = SAERR(ipt)
      if (cmsg(1:ERRNC(ipt)) .eq. 'DEFAULT') cmsg = clab
      ERRLAB = clab
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cerrkil (cmsg,kerr)
c
c   FUNCTION:  This routine should only be called when a terminal error
c              occurs.  It outputs a termination error message and exits
c              the program. This function could be called by C++ routine
c
c   INPUT:  cmsg    C*n  D1  -  Text of error message from routine that
c                               got the error.
c
c           kerr    I*4  D1  -  Contains the error condition code from
c                               the routine that got the error.
c
c***********************************************************************
c
      subroutine cerrkil (cmsg,kerr)
c
      include 'postworks_nt.inc'
      character*80 cmsg

      integer*4 kerr
c
      call errkil (cmsg,kerr)
c
      return
      end

c
c***********************************************************************
c
c   E-FUNCTION:  isquiet()
c
c   FUNCTION:  This routine returns the number of characters in 'cstr'.
c              The last non-space character is considered the last char-
c              acter in the string.
c
c   INPUT:  cstr    C*n  D1  -  Character string to determine the length
c                               of.
c
c   OUTPUT: strlen1  I*4  D1  -  Length of 'cstr'.
c
c***********************************************************************
c
      integer*4 function isquiet()
c
      include 'menu.inc'
c
      isquiet = RUNQUIET
      return
      end
