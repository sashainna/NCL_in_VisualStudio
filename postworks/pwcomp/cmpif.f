c
c***********************************************************************
c
c   FILE NAME:  cmpif
c   CONTAINS:
c               cmpif   ifcmd   ifthen  cmpels  elsjmp  cmpenf  cmpdo
c               doend   crelab  creprn
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmpif.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:12
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmpif (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine is the controlling routine for IF state-
c              ments.  It will determine the type of IF statment (IF
c              command or IF THEN ELSE) and call the appropriate rou-
c              tine.
c
c   INPUT:  none.
c
c   OUTPUT: kfl     I*4  D1  Returns 3 when the syntax is 'IF command'
c                            and the global arrays contain the command
c                            to process.  'kfl' should be set to 0
c                            prior to the 1st call and should be set to
c                            0 after processing the returned command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpif (kfl,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr,kfl
c
      character*(*) cmsg
c
      integer*4 inum
c
c...Initialize routine
c
      kerr   = 0
c
c...Check syntax of command
c
      if (IFFLAG .ne. 0) go to 9000
      if (NTOK .lt. 3 .or. (ICTYP(2) .ne. 5 .and. ICTYP(2) .ne. 6) .or.
     1    RCSUB(2) .gt. MAXPRN) go to 9000
      inum   = RCSUB(2)
      if (IPNSMP(inum) .eq. 2) go to 9000
c
c...IF THEN ELSE syntax
c
      if (NTOK .eq. 3 .and. ICTYP(3) .eq. 1 .and. RCSUB(3) .eq. 5005.)
     1        then
          call ifthen (1,cmsg,kerr)
c
c...IF command syntax
c
       else
          call ifcmd (kfl,cmsg,kerr)
       endif
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ifcmd (kfl,cmsg,kerr)
c
c   FUNCTION:  This routine processes the 'IF command' statement.  It
c              will define two dummy labels, 1 for "FALSE" and 1 for
c              "TRUE".  It will also create a JUMPTO statement for the
c              "FALSE" label.  The "FALSE" label will be processed by
c              the main compiling routine.
c
c   INPUT:  none.
c
c   OUTPUT: kfl     I*4  D1  Always returns 3, storing the 'command' in
c                            the common token arrays.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ifcmd (kfl,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr,kfl
c
      character*(*) cmsg
c
      integer*4 nc,inum,i
c
      character*24 ldat
c
c...Initialize routine
c
      if (IFPT .eq. MAXIF) go to 9000
      IFPT   = IFPT   + 1
c
c...Allocate new IF label
c...for "TRUE" branch
c
      call crelab (ldat,nc)
c
c...Simple parenthesis conditional
c...Output parenthesis equation
c
      inum   = RCSUB(2)
      if (IPNSMP(inum) .eq. 1 .or. IPNSMP(inum) .eq. 4)
     1        call creprn (inum)
c
c...Compile IF command
c
      ICMPL(1) = 16
      ICMPL(2) = 5
      ICMPL(3) = 1
      if (IPNSMP(inum) .eq. 3 .or. IPNSMP(inum) .eq. 4) ICMPL(3) = 2
      ICMPL(4) = inum
      LCMPL(9:32) = ldat
      call cmpwrt (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Allocate new IF label
c...for "FALSE" branch
c
      call crelab (LF2LAB(IFPT),NC2LAB(IFPT))
c
c...Compile internal JUMPTO command
c...For "FALSE" branch
c
      ICMPL(1) = 14
      ICMPL(2) = 4
      LCMPL(5:28) = LF2LAB(IFPT)
      call cmpwrt (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Compile "TRUE" Label
c
      call cmplab (ldat,nc,IPC,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store "TRUE" command in common arrays &
c...send it back to compilation routine
c
      do 100 i=3,NTOK,1
          ICTYP(i-2) = ICTYP(i)
          RCSUB(i-2) = RCSUB(i)
          ICNC(i-2) = ICNC(i)
  100 continue
      NTOK   = NTOK   - 2
      kfl    = 3
      IFFLAG = 2
c
c...End of routine
c
 8000 return
c
c...IF/DO nested too deep
c
 9000 call errtxt ('IFDODEEP',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ifthen (kent,cmsg,kerr)
c
c   FUNCTION:  This routine processes the 'IF THEN ELSE' statement.  It
c              will define two dummy labels, 1 for "FALSE" and 1 for
c              "TRUE".  It will also create a JUMPTO statement for the
c              "FALSE" label.  The "FALSE" label will be processed by
c              the 'ELSE' or 'ENDIF' routine.
c
c   INPUT:  kent    I*4  D1  1 = The 'IF' command is the start of a new
c                            'IF THEN ELSE' structure.  2 = The 'IF'
c                            command is part of an 'ELSE IF' statement.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ifthen (kent,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr,kent
c
      character*(*) cmsg
c
      integer*4 nc,inum
c
      character*24 ldat
c
c...Initialize routine
c
      if (kent .eq. 1) then
          if (IFPT .eq. MAXIF) go to 9000
          IFPT   = IFPT   + 1
          IFELSF(IFPT) = 0
      endif
c
c...Allocate new IF label
c...for "TRUE" branch
c
      call crelab (ldat,nc)
c
c...Simple parenthesis conditional
c...Output parenthesis equation
c
      inum   = RCSUB(2)
      if (IPNSMP(inum) .eq. 1 .or. IPNSMP(inum) .eq. 4)
     1        call creprn (inum)
c
c...Compile IF command
c
      ICMPL(1) = 16
      ICMPL(2) = 5
      ICMPL(3) = 1
      if (IPNSMP(inum) .eq. 3 .or. IPNSMP(inum) .eq. 4) ICMPL(3) = 2
      ICMPL(4) = inum
      LCMPL(9:32) = ldat
      call cmpwrt (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Allocate new IF label
c...for ENDIF
c
      if (kent .eq. 1) call crelab (LF2LAB(IFPT),NC2LAB(IFPT))
c
c...Allocate new IF label
c...for "FALSE" branch
c
      call crelab (LF1LAB(IFPT),NC1LAB(IFPT))
c
c...Compile internal JUMPTO command
c...For "FALSE" branch
c
      ICMPL(1) = 14
      ICMPL(2) = 4
      LCMPL(5:28) = LF1LAB(IFPT)
      call cmpwrt (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Compile "TRUE" Label
c
      call cmplab (ldat,nc,IPC,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
c
c...IF/DO nested too deep
c
 9000 call errtxt ('IFDODEEP',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpels (cmsg,kerr)
c
c   FUNCTION:  This routine processes the 'ELSE' statement.  It will
c              create a JUMPTO statement for the previous "TRUE" con-
c              dition in the IF structure, and also process the "FALSE"
c              label for the previous condition.  If the syntax is
c              'ELSE IF', then the routine for compiling the 'IF THEN
c              ELSE' statement will be called to compile the 'IF' por-
c              tion.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpels (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 inum,i
c
c...Make sure IF block is currently being defined
c
      if (IFPT .eq. 0 .or. IFFLAG .eq. 2) go to 9100
      if (IFELSF(IFPT) .ne. 0) go to 9100
c
c...ELSE syntax
c
      if (NTOK .eq. 1) then
          call cmplab (LF1LAB(IFPT),NC1LAB(IFPT),LABPC,cmsg,kerr)
          IFELSF(IFPT) = 1
c
c...ELSE IF THEN syntax
c
      else
c
c......Check syntax
c
          if (NTOK .ne. 4 .or. ICTYP(2) .ne. 1 .or. RCSUB(2) .ne. 4001.
     1        .or. (ICTYP(3) .ne. 5 .and. ICTYP(3) .ne. 6) .or.
     1        RCSUB(3) .gt. MAXPRN) go to 9000
          inum   = RCSUB(3)
          if (IPNSMP(inum) .eq. 2) go to 9000
          if (ICTYP(4) .ne. 1 .or. RCSUB(4) .ne. 5005.) go to 9100
c
c......Compile previous conditional's "FALSE" label
c
          call cmplab (LF1LAB(IFPT),NC1LAB(IFPT),LABPC,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Compile IF portion of statement
c
          do 100 i=2,NTOK,1
              ICTYP(i-1) = ICTYP(i)
              RCSUB(i-1) = RCSUB(i)
              ICNC(i-1) = ICNC(i)
  100     continue
          NTOK   = NTOK   - 1
          call ifthen (2,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8000
c
c...Illegal nesting of IF's
c
 9100 call errtxt ('IFDONEST',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  elsjmp (cmsg,kerr)
c
c   FUNCTION:  This routine compiles the JUMPTO for the previous "TRUE"
c              condition in the IF structure, when an 'ELSE' command is
c              encountered.  This routine must be called directly after
c              the 'getstm' routine, because the 'ELSE' command may be
c              broken into multiple commands by the 'getprn' routine.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine elsjmp (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Compile internal JUMPTO command
c...For previous conditional's "TRUE" branch
c
      ICMPL(1) = 14
      ICMPL(2) = 4
      LCMPL(5:28) = LF2LAB(IFPT)
      call cmpwrt (cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpenf (cmsg,kerr)
c
c   FUNCTION:  This routine processes the 'ENDIF' statement.  It will
c              process the label for the end of the IF structure.  The
c              label for the previous 'ELSE IF' statement will also be
c              processed if an 'ELSE' statement without the optional
c              IF clause was not encountered in the current IF struc-
c              ture.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpenf (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Make sure IF block is currently being defined
c
      if (NTOK .ne. 1) go to 9000
      if (IFPT .eq. 0 .or. IFFLAG .eq. 2) go to 9100
c
c...Output last "FALSE" label when
c...ELSE IF THEN syntax was encountered last
c
      if (IFELSF(IFPT) .eq. 0) then
          call cmplab (LF1LAB(IFPT),NC1LAB(IFPT),LABPC,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Output ENDIF label
c
      call cmplab (LF2LAB(IFPT),NC2LAB(IFPT),LABPC,cmsg,kerr)
      IFPT   = IFPT   - 1
      if (kerr .ne. 0) go to 8000
c
c...Compile NOOP (CONTINUE) command
c
      ICMPL(1) = 2
      ICMPL(2) = 0
      call cmpwrt (cmsg,kerr)
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8000
c
c...Illegal nesting of IF's
c
 9100 call errtxt ('IFDONEST',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpdo (kctyp,gcsub,kcnc,ktok,cmsg,kerr)
c
c   FUNCTION:  This routine is the controlling routine for DO state-
c              ments.  The common arrays for token types & values
c              (ICTYP,RCSUB,ICNC,NTOK) must be passed in as local ar-
c              rays, because this routine generates multiple equations
c              using the global arrays for storage.
c
c   INPUT:  kctyp   I*4  Dn  Local storage of the ICTYP common array.
c
c           gcsub   R*8  Dn  Local storage of the RCSUB common array.
c
c           kcnc    I*4  Dn  Local storage of the ICNC common array.
c
c           ktok    I*4  D1  Local NTOK variable.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpdo (kctyp,gcsub,kcnc,ktok,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr,kctyp(50),kcnc(50),ktok
c
      real*8 gcsub(50)
c
      character*(*) cmsg
c
      integer*4 inum(6),i,j,ipt,icnt,icoma(2),ist,iprn,nc,ityp
c
      character*24 lnum
c
      equivalence (lnum,inum)
c
c...Make sure there is the correct
c...number of parameters in DO statement
c
      if (IFPT .eq. MAXIF) go to 9100
      if (NTOK .lt. 9) go to 9000
      do 100 i=1,5,1
          if (ICTYP(i) .eq. 2 .and. RCSUB(i) .eq. 2.) go to 9000
  100 continue
c
      icnt   = 0
      do 200 i=6,NTOK,1
          if (ICTYP(i) .eq. 2 .and. RCSUB(i) .eq. 2.) then
              if (icnt .eq. 2) go to 9000
              icnt = icnt + 1
              icoma(icnt) = i
          endif
  200 continue
      if (icnt .ne. 2) go to 9000
c
c...Get the ending Label
c
      IFPT   = IFPT   + 1
      IDPT   = IDPT   + 1
      IDARY(IDPT) = IFPT
      if (ICTYP(2) .ne. 4) go to 9500
      ist    = RCSUB(2)
      if ((ICNC(2)-ist+1) .gt. 24) go to 9500
      call touppr (LCTXT(ist:ICNC(2)),IDOLAB(IFPT))
      call getlab (IDOLAB(IFPT),ipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (ipt .ne. 0) go to 9600
c
c...Get the name of the increment variable
c
      if (ICTYP(3) .ne. 4) go to 9300
      ist    = RCSUB(3)
      if ((ICNC(3)-ist+1) .gt. 24) go to 9300
      lnum   = LCTXT(ist:ICNC(3))
      call getscl (lnum,ICTYP(3),ist,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Determine type of variable
c......Valid types are: "(subscripted) Real variable"
c
      if (ICTYP(3) .eq. 5) then
          RCSUB(3) = MFDAT(ist+7,3)
          ICNC(3) = MFDAT(ist+6,3)
          ist    = 4
c
c.........Subscripted variable
c
          if (ICTYP(ist) .eq. 5 .and. RCSUB(ist) .le. MAXPRN) then
              ipt    = RCSUB(ist)
              if (IPNSMP(ipt) .eq. 1) then
                  if (PRNVAR(ipt) .le. 0 .or.
     1                PRNVAR(ipt) .gt. ICNC(3)) go to 9400
                  IDOTYP(IFPT) = 1
                  IDOSUB(IFPT) = RCSUB(3) + PRNVAR(ipt) - 1
c
c............Complex subscript (VARIABLE)
c............Allocate space for dummy subscript variable
c
              else
                  IDOTYP(IFPT) = 2
                  IDOSUB(IFPT) = RCSUB(3)
                  IDOCNC(IFPT) = ICNC(3)
                  iprn   = ipt
                  call crelab (lnum,nc)
                  call getscl (lnum,ityp,ipt,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  if (ityp .ne. 4) go to 9200
c
                  call lodscl (1,NSCAL(1)+1,ipt,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  IDOPRN(IFPT) = ISCAST(1)
                  NSCAL(1) = NSCAL(1) + 1
                  do 250 j=1,6,1
                      MFDAT(j+ipt-1,3) = inum(j)
  250             continue
                  MFDAT(ipt+6,3) = 1
                  MFDAT(ipt+7,3) = ISCAST(1)
                  ISCAST(1) = ISCAST(1) + 1
              endif
              ist    = ist    + 1
c
c.........Unsubscripted variable
c
          else
              IDOTYP(IFPT) = 1
              IDOSUB(IFPT) = RCSUB(3)
          endif
c
c......Unrecognized variable type
c
      else
          go to 9300
      endif
c
c...Check for '='
c
      if (ICTYP(ist) .ne. 2 .or. RCSUB(ist) .ne. 1.) go to 9000
c
c...Save current parameter types
c
      do 300 i=1,NTOK,1
          kctyp(i) = ICTYP(i)
          gcsub(i) = RCSUB(i)
          kcnc(i) = ICNC(i)
  300 continue
      ktok   = NTOK
c
c...Assign dummy label for DO loop
c...LF1LAB is at beginning of DO loop
c...LF2LAB is after DO loop (for ENDDO command)
c
      call crelab (LF1LAB(IFPT),NC1LAB(IFPT))
      call crelab (LF2LAB(IFPT),NC2LAB(IFPT))
c
c...Assign 2 Dummy variables &
c...allocate space for them
c
      call crelab (lnum,nc)
      call getscl (lnum,ityp,ipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (ityp .ne. 4) go to 9200
c
      call lodscl (1,NSCAL(1)+1,ipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      IDOPT1(IFPT) = ISCAST(1)
      NSCAL(1) = NSCAL(1) + 1
      do 400 i=1,6,1
          MFDAT(ipt+i-1,3) = inum(i)
  400 continue
      MFDAT(ipt+6,3) = 1
      MFDAT(ipt+7,3) = ISCAST(1)
      ISCAST(1) = ISCAST(1) + 1
c
      call crelab (lnum,nc)
      call getscl (lnum,ityp,ipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (ityp .ne. 4) go to 9200
c
      call lodscl (1,NSCAL(1)+1,ipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      IDOPT2(IFPT) = ISCAST(1)
      NSCAL(1) = NSCAL(1) + 1
      do 450 i=1,6,1
          MFDAT(ipt+i-1,3) = inum(i)
  450 continue
      MFDAT(ipt+6,3) = 1
      MFDAT(ipt+7,3) = ISCAST(1)
      ISCAST(1) = ISCAST(1) + 1
c
c...Compile Variable assignment
c
      NTOK   = 0
      do 500 i=3,icoma(1)-1,1
          NTOK   = NTOK   + 1
          ICTYP(NTOK) = kctyp(i)
          RCSUB(NTOK) = gcsub(i)
          ICNC(NTOK) = kcnc(i)
  500 continue
      call cmpequ (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Compile Ending value assignment
c
      ICTYP(1) = 5
      RCSUB(1) = IDOPT1(IFPT)
      ICNC(1) = 1
c
      ICTYP(2) = 2
      RCSUB(2) = 1.
c
      NTOK   = 2
      do 1000 i=icoma(1)+1,icoma(2)-1,1
          NTOK   = NTOK   + 1
          ICTYP(NTOK) = kctyp(i)
          RCSUB(NTOK) = gcsub(i)
          ICNC(NTOK) = kcnc(i)
 1000 continue
      call cmpequ (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Compile Increment value assignment
c
      ICTYP(1) = 5
      RCSUB(1) = IDOPT2(IFPT)
      ICNC(1) = 1
c
      ICTYP(2) = 2
      RCSUB(2) = 1.
c
      NTOK   = 2
      do 1100 i=icoma(2)+1,ktok,1
          NTOK   = NTOK   + 1
          ICTYP(NTOK) = kctyp(i)
          RCSUB(NTOK) = gcsub(i)
          ICNC(NTOK) = kcnc(i)
 1100 continue
      call cmpequ (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Compile dummy subscript assignment
c
      if (IDOTYP(IFPT) .eq. 2) then
          ICTYP(1) = 5
          RCSUB(1) = IDOPRN(IFPT)
          ICNC(1) = 1
c
          ICTYP(2) = 2
          RCSUB(2) = 1.
c
          ICTYP(3) = 5
          RCSUB(3) = iprn
          IPNSMP(iprn) = 0
c
          NTOK   = 3
          call cmpequ (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Compile dummy label for DO loop
c
      call cmplab (LF1LAB(IFPT),NC1LAB(IFPT),IPC,cmsg,kerr)
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8000
c
c...IF/DO nested too deeply
c
 9100 call errtxt ('IFDODEEP',cmsg)
      kerr   = 1
      go to 8000
c
c...Error trying to allocate
c...dummy variable storage
c
 9200 call errtxt ('NOSCALAR',cmsg)
      kerr   = 1
      go to 8000
c
c...Undefined variable
c
 9300 call errtxt ('UNDEFVAR',cmsg)
      kerr   = 1
      go to 8000
c
c...Subscript out of bounds
c
 9400 call errtxt ('SUBOUND',cmsg)
      kerr   = 1
      go to 8000
c
c...Label expected
c
 9500 call errtxt ('LABEXP',cmsg)
      kerr   = 1
      go to 8000
c
c...Illegal nesting of IF/DO structures
c
 9600 call errtxt ('IFDONEST',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  doend (cmsg,kerr)
c
c   FUNCTION:  This routine is the compiles the equations and IF state-
c              ment that are required at the end of a DO loop.  The
c              counter is incremented, the limit is checked and and IF
c              statement is generated to close the loop.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine doend (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Make sure this is the last defined
c...DO loop label
c
      if (IDOLPT .ne. IFPT) go to 9000
c
c...Increment DO loop Counter
c...Subscripted Counter
c
      if (IDOTYP(IFPT) .eq. 2) then
c
c.........Compile subscript value assignment
c.........for Counter
c
          ICTYP(1) = 5
          RCSUB(1) = 1
          ICNC(1) = 1
          NPNVAR = 1
          IPNSMP(1) = 0
c
          ICTYP(2) = 2
          RCSUB(2) = 1.
c
          ICTYP(3) = 5
          RCSUB(3) = IDOPRN(IFPT)
          ICNC(3) = 1
c
          NTOK   = 3
          call cmpequ (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c.........Compile counter equation
c
          ICTYP(1) = 5
          RCSUB(1) = IDOSUB(IFPT)
          ICNC(1)  = IDOCNC(IFPT)
c
          ICTYP(2) = 5
          RCSUB(2) = 1.
          ICNC(2)  = 1
          IPNSMP(1) = 0
c
          ICTYP(3) = 2
          RCSUB(3) = 1.
c
          ICTYP(4) = 5
          RCSUB(4) = IDOSUB(IFPT)
          ICNC(4)  = IDOCNC(IFPT)
c
          ICTYP(5) = 5
          RCSUB(5) = 1.
          ICNC(5) = 1
          IPNSMP(1) = 0
c
          ICTYP(6) = 2
          RCSUB(6) = 4.
c
          ICTYP(7) = 5
          RCSUB(7) = IDOPT2(IFPT)
          ICNC(7)  = 1
c
          NTOK   = 7
          call cmpequ (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Non-subscripted Counter
c......Compile Counter equation
c
      else
          ICTYP(1) = 5
          RCSUB(1) = IDOSUB(IFPT)
          ICNC(1)  = 1
c
          ICTYP(2) = 2
          RCSUB(2) = 1.
c
          ICTYP(3) = 5
          RCSUB(3) = IDOSUB(IFPT)
          ICNC(3)  = 1
c
          ICTYP(4) = 2
          RCSUB(4) = 4.
c
          ICTYP(5) = 5
          RCSUB(5) = IDOPT2(IFPT)
          ICNC(5)  = 1
c
          NTOK   = 5
          call cmpequ (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Compile conditional equation
c
      ICTYP(1) = 5
      RCSUB(1) = 2.
      ICNC(1) = 1
      NPNVAR = 2
      IPNSMP(2) = 0
c
      ICTYP(2) = 2
      RCSUB(2) = 1.
c
      ICTYP(3) = 5
      RCSUB(3) = IDOSUB(IFPT)
      ICNC(3) = 1
      NTOK    = 4
      if (IDOTYP(IFPT) .eq. 2) then
          ICNC(3) = IDOCNC(IFPT)
          ICTYP(4) = 5.
          RCSUB(4) = 1.
          ICNC(4) = 1
          NTOK    = 5
      endif
c
      ICTYP(NTOK) = 2
      RCSUB(NTOK) = 13.
c
      NTOK    = NTOK    + 1
      ICTYP(NTOK) = 5
      RCSUB(NTOK) = IDOPT1(IFPT)
      ICNC(NTOK) = 1
c
      call cmpequ (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Compile IF statement
c
      ICMPL(1) = 16
      ICMPL(2) = 5
      ICMPL(3) = 1
      ICMPL(4) = 2
      LCMPL(9:32) = LF1LAB(IFPT)
      call cmpwrt (cmsg,kerr)
c
c...Compile dummy label for after end of DO loop
c
      call cmplab (LF2LAB(IFPT),NC2LAB(IFPT),IPC,cmsg,kerr)
c
c...End of routine
c
 8000 IDOLAB(IFPT) = ' '
      IFPT   = IFPT   - 1
      IDPT   = IDPT   - 1
      IDOLPT = 0
      return
c
c...Invalid nesting of IF/DO structures
c
 9000 call errtxt ('IFDONEST',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  crelab (cdat,knc)
c
c   FUNCTION:  This routine creates a dummy label to be used for in-
c              ternal JUMPTO's, created using IF's and DO's.  The label
c              is in the format 'QZ$n', where 'n' is a unique number.
c
c   INPUT:  none.
c
c   OUTPUT: cdat    C*n  D1  Label text string.
c
c           knc     I*4  D1  Number of characters in 'cdat'.
c
c***********************************************************************
c
      subroutine crelab (cdat,knc)
c
      include 'compile.inc'
c
      integer*4 knc
c
      character*(*) cdat
c
      integer*4 nc
c
      character*5 ldat
c
c...Create next IF/DO label
c
      IFLAB  = IFLAB  + 1
      call itoc (IFLAB,ldat,nc,0)
c
c...If in G$MAIN macro, give unique names and labels
c...because these names are never removed (global variables).
c...This can cause naming conflicts if other macro want to
c...use some created labels.
c...(i.e. do loop in G$MAIN generates variable names that are
c...kept around the entire time.  When G$MAIN is finished,
c...IFLAB = 0.  If any other macro attempts to use a do loop,
c...the generated variable names will conflict.
c
      if (NMACRO .eq. 1) then
           cdat   = 'QZM$' // ldat(1:nc)
           knc    = nc     + 4
      else
           cdat   = 'QZ$' // ldat(1:nc)
           knc    = nc     + 3
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  creprn (kprn)
c
c   FUNCTION:  This routine compiles a parenthesis variable that has
c              been 'optimized' away.  The compiled variable is required
c              by IF statements, which do not support pure numbers.
c
c   INPUT:  kprn    I*4  D1  The parenthesis variable to compile.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine creprn (kprn)
c
      include 'compile.inc'
c
      integer*4 kprn
c
      integer*4 is,ie,iss,ierr
c
      character*80 msg
c
c...Compile REAL parenthesis equation
c
      if (IPNSMP(kprn) .eq. 1) then
          ICMPL(1) = 16
          ICMPL(2) = 1
          ICMPL(3) = 1
          ICMPL(4) = 1
          ICMPL(5) = 3
          JCMPL(4) = kprn
          RCMPL(4) = PRNVAR(kprn)
c
c...Compile TEXT parenthesis equation
c
      else
          ICMPL(1) = 18
          ICMPL(2) = 2
          ICMPL(3) = 1
          ICMPL(4) = 4
          ICMPL(5) = 6
          JCMPL(4) = kprn
          ICMPL(9) = 0
          ICMPL(10) = 0
          ICMPL(1) = ICMPL(1) + 1
          ICMPL(ICMPL(1)) = PRNNC(kprn) - PRNVAR(kprn) + 1
          if (ICMPL(ICMPL(1)) .le. 0) then
              ICMPL(ICMPL(1)) = 0
          else
              iss    = PRNVAR(kprn)
              is     = ICMPL(1) * 2 + 1
              ie     = is     + ICMPL(ICMPL(1)) - 1
              LCMPL(is:ie) = LCTXT(iss:PRNNC(kprn))
              ICMPL(1) = ICMPL(1) + (ICMPL(ICMPL(1))-1) / 2 + 1
          endif
      endif
c
c...Write out compiled record
c
      call cmpwrt (msg,ierr)
c
c...End of routine
c
 8000 return
      end
