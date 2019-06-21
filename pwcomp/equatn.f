c
c***********************************************************************
c
c   FILE NAME:  equatn
c   CONTAINS:
c               cmpequ  equsyn  solveq  vartyp
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        equatn.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:13
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmpequ (cmsg,kerr)
c
c   FUNCTION:  This routine compiles a numeric equation after it has
c              been fully parsed.  If mulitiple operators are in an
c              equation, then the statement will be broken up into mul-
c              tiple operations, using parenthesis variables.  Also,
c              simple parenthesis variables that can equate to a number
c              (#1 = 5 + 3) will be replaced with that number.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine cmpequ (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ifl,nvar,nfnc,nop,ieq,i,ist,ipt,inc,igo,lastop,icnt
c
c...Initialize routine
c
      kerr   = 0
      ifl    = 0
c
c...Check syntax
c
      call equsyn (nvar,nfnc,nop,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Break equation into single operation
c
      ieq    = 1
      call vartyp (ieq,ICTYP,RCSUB,ICNC,NTOK,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      ieq    = ieq    + 1
      ICMPL(2) = 1
c
c...Break out all Functions
c
  200 if (nfnc .eq. 0) go to 1000
c
c......Find next function
c
      do 300 i=ieq+1,NTOK,1
          if (ICTYP(i) .eq. 1) go to 400
  300 continue
  400 ist    = i      + 1
      ipt    = ist
c
c......Get variable type
c
      inc    = 0
      if (RCSUB(ist-1) .eq. 28.) inc = 1
      call vartyp (ipt,ICTYP,RCSUB,ICNC,NTOK,inc,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Simple Function COS(30)
c
      if (ICTYP(ist) .eq. 3) then
          ICTYP(i) = 3
          igo    = RCSUB(i)
          call solveq (igo,RCSUB(i),RCSUB(ist),RCSUB(ist),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......Simple STRLEN Function
c
      else if (ICTYP(ist) .eq. 6) then
          ICTYP(i) = 3
          RCSUB(i) = ICNC(ist) - RCSUB(ist) + 1
c
c......Equatic Function COS(VAR)
c
      else
c
c.........Last operation on statement
c
          if (nop .eq. 0) then
              ICMPL(4) = ICTYP(1)
              JCMPL(4) = RCSUB(1)
              JCMPL(5) = RCSUB(2)
c
c.........Create parenthesis variable equation
c
          else
              ICMPL(4) = 1
              JCMPL(4) = RCSUB(ist)
              if (RCSUB(ist-1) .eq. 28.) IPNSMP(JCMPL(4)) = 0
          endif
c
          ICMPL(3) = RCSUB(i)
          ICMPL(5) = 1
          if (RCSUB(ist-1) .eq. 28.) ICMPL(5) = ICTYP(ist)
          JCMPL(7) = RCSUB(ist)
          ICMPL(1) = 14
          call cmpwrt (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
          ICTYP(i) = 5
          RCSUB(i) = RCSUB(ist)
          if (nfnc .eq. 1 .and. nop .eq. 0) go to 8000
      endif
c
c......Rebuild original equation
c......by removing Function
c
  600 NTOK   = NTOK   - 1
      do 700 i=ist,NTOK,1
          ICTYP(i) = ICTYP(i+1)
          RCSUB(i) = RCSUB(i+1)
          ICNC(i) = ICNC(i+1)
  700 continue
      nfnc   = nfnc   - 1
      go to 200
c
c...Break out highest order equations
c
 1000 if (nop .eq. 1 .or. ifl .eq. 1) then
          ipt    = ieq    + 1
          ist    = ipt    + 2
          if (ICTYP(ist) .ge. 100) ist = ist + 1
          inc    = ist    + 1
          if (ICTYP(inc) .lt. 100) inc = inc + 1
          go to 1200
      endif
      if (nop .eq. 0) go to 1600
c
c......Get next highest order equation
c
      inc    = ieq    + 1
      lastop = 0
      icnt   = 0
 1100 ist    = inc
      inc    = inc    + 1
      if (ICTYP(inc) .lt. 100) inc = inc + 1
      icnt   = icnt   + 1
c      if (inc .ge. NTOK) go to 1500
c
      if (lastop .ne. 0 .and. ICTYP(inc) .ge. lastop) go to 1200
      ipt    = ist
c
c.........Last equation on line
c
      if (icnt .eq. nop) then
          if (ICTYP(inc) .lt. lastop) then
              ist    = inc    + 1
              go to 1200
          else
              go to 1500
          endif
      endif
c
c.........Go get the next equation
c
      lastop = ICTYP(inc)
      inc    = inc    + 1
      go to 1100
c
c.........Found equation to break out
c.........Get variable types
c
 1200 i      = ipt
      call vartyp (i,ICTYP,RCSUB,ICNC,NTOK,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      i      = ist
      call vartyp (i,ICTYP,RCSUB,ICNC,NTOK,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c............Simple equation A = 1 + 3
c
      if (ICTYP(ipt) .eq. 3 .and. ICTYP(ist) .eq. 3 .and.
     1        ICTYP(ist-1) .ne. 107) then
          igo    = RCSUB(ist-1)
          call solveq (igo,RCSUB(ipt),RCSUB(ipt),RCSUB(ist),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          ICTYP(ipt) = 3
          if (nop .eq. 1) go to 1600
c
c............Equatic equation A = B + C
c............Create parenthesis variable equation
c
      else
          if (nop .gt. 1) then
              if (ICTYP(ist-1) .eq. 107) go to 9000
              if (NPNVAR .ge. MAXPRN) go to 9100
              NPNVAR = NPNVAR + 1
              IPNSMP(NPNVAR) = 0
              ICMPL(4) = 1
              JCMPL(4) = NPNVAR
          else
              if (ICTYP(ist-1) .eq. 107) then
                  i     = RCSUB(1)
                  if (ICTYP(1) .ne. 1 .or. i .gt. MAXPRN) go to 9000
                  IPNSMP(i) = 2
              endif
              JCMPL(4) = RCSUB(1)
              ICMPL(4) = ICTYP(1)
              if (ICTYP(1) .eq. 2) JCMPL(5) = RCSUB(2)
          endif
c
          ICMPL(1) = 20
          ICMPL(3) = RCSUB(ist-1)
          ICMPL(5) = ICTYP(ipt)
          ICMPL(6) = ICTYP(ist)
c
          if (ICTYP(ipt) .eq. 1 .or. ICTYP(ipt) .eq. 13) then
              JCMPL(7) = RCSUB(ipt)
          else if (ICTYP(ipt) .eq. 2 .or. ICTYP(ipt) .eq. 12 .or.
     1             ICTYP(ipt) .eq. 14 .or. ICTYP(ipt) .eq. 15) then
              JCMPL(7) = RCSUB(ipt)
              JCMPL(8) = RCSUB(ipt+1)
          else
              RCMPL(4) = RCSUB(ipt)
          endif
c
          if (ICTYP(ist) .eq. 1 .or. ICTYP(ist) .eq. 13) then
              JCMPL(9) = RCSUB(ist)
          else if (ICTYP(ist) .eq. 2 .or. ICTYP(ist) .eq. 12 .or.
     1             ICTYP(ist) .eq. 14 .or. ICTYP(ist) .eq. 15) then
              JCMPL(9) = RCSUB(ist)
              JCMPL(10) = RCSUB(ist+1)
          else
              RCMPL(5) = RCSUB(ist)
          endif
c
          call cmpwrt (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...............Store number in equation
c
          ICTYP(ipt) = 5
          RCSUB(ipt) = NPNVAR
      endif
c
c.........Rebuild original equation by
c.........removing current equation
c
 1400 if (nop .eq. 1) go to 8000
      do 1450 i=inc,NTOK,1
          ipt    = ipt    + 1
          ICTYP(ipt) = ICTYP(i)
          RCSUB(ipt) = RCSUB(i)
          ICNC(ipt) = ICNC(i)
 1450 continue
      NTOK   = ipt
      nop    = nop    - 1
      go to 1000
c
c.........Equations are in correct order
c.........Loop through them again
c
 1500 ifl    = 1
      go to 1000
c
c......Simple equivalence  -  A = B
c
 1600 ist    = ieq    + 1
      ipt    = ist
      call vartyp (ist,ICTYP,RCSUB,ICNC,NTOK,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      i      = RCSUB(1)
      if (ICTYP(ipt) .eq. 3 .and. i .le. MAXPRN) then
          IPNSMP(i) = 1
          PRNVAR(i) = RCSUB(ipt)
      else
          ICMPL(1) = 16
          ICMPL(2) = 1
          ICMPL(3) = 1
          ICMPL(4) = ICTYP(1)
          ICMPL(5) = ICTYP(ipt)
          JCMPL(4) = RCSUB(1)
          if (ICTYP(1) .eq. 2) JCMPL(5) = RCSUB(2)
          if (ICTYP(ipt) .eq. 1 .or. ICTYP(ipt) .eq. 13) then
              JCMPL(7) = RCSUB(ipt)
          else if (ICTYP(ipt) .eq. 2 .or. ICTYP(ipt) .eq. 12 .or.
     1             ICTYP(ipt) .eq. 14 .or. ICTYP(ipt) .eq. 15) then
              JCMPL(7) = RCSUB(ipt)
              JCMPL(8) = RCSUB(ipt+1)
          else
              RCMPL(4) = RCSUB(ipt)
          endif
          call cmpwrt (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
      go to 8000
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      return
c
c...Equation is too complex
c
 9100 call errtxt ('COMPLXEQ',cmsg)
      kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  equsyn (kvar,kfnc,kop,cmsg,kerr)
c
c   FUNCTION:  This routine checks the syntax of a fully parsed numeric
c              equation.
c
c   INPUT:  none.
c
c   OUTPUT: kvar    I*4  D1  Returns the number of variables in the
c                            equation, excluding the variable to the
c                            left of the ='s.
c
c           kfnc    I*4  D1  Returns the number of functions (SIN,COS,
c                            etc) in the equation.
c
c           kop     I*4  D1  Returns the number of operators (+-/,etc)
c                            in the equation, excluding the '='.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine equsyn (kvar,kfnc,kop,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr,kvar,kfnc,kop
c
      character*(*) cmsg
c
      integer*4 inc,ist
c
      character*24 lbuf
c
c...Initialize routine
c
      kerr   = 0
      inc    = 2
      kvar   = 0
      kfnc   = 0
      kop    = 0
c
c...Check syntax
c
      if (ICTYP(2) .eq. 5) then
          if (RCSUB(2) .gt. NPNVAR) go to 9000
          inc    = inc    + 1
      endif
c
      if (NTOK .le. inc) go to 9000
      if (ICTYP(inc) .ne. 2 .or. RCSUB(inc) .ne. 1) go to 9000
c
c......This token should be a number,
c......Real variable or a
c......Function
c
  100     if (inc .eq. NTOK) go to 9100
          inc    = inc    + 1
c
c.........Text string
c.........See if it's actually a Real variable
c
          if (ICTYP(inc) .eq. 4) then
              ist    = RCSUB(inc)
              if ((ICNC(inc)-ist+1) .gt. 24) go to 9100
              lbuf   = LCTXT(ist:ICNC(inc))
              call getscl (lbuf,ICTYP(inc),ist,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (ICTYP(inc) .ne. 5) go to 9100
              RCSUB(inc) = MFDAT(ist+7,3)
              ICNC(inc) = MFDAT(ist+6,3)
c
c............Check for subscript
c
              if (NTOK .ne. inc .and. ICTYP(inc+1) .eq. 5) then
                  inc    = inc    + 1
                  if (RCSUB(inc) .gt. NPNVAR) go to 9000
              endif
c
              kvar   = kvar   + 1
c
c.........Vocabulary word
c
          else if (ICTYP(inc) .eq. 1) then
c
c............Post Variable
c
              if (RCSUB(inc) .ge. 6000.) then
                  call pstvar (inc,ICTYP,RCSUB,NTOK,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  if (ICTYP(inc) .eq. 18) go to 9100
                  kvar   = kvar   + 1
c
c............Function
c............Make sure it's a valid Numeric Function
c............COS  SIN  TAN  INT  SQRT  ACOS  ASIN  ATAN  ABS  STRLEN
c
              else
                  if (inc .eq. NTOK .or. ICTYP(inc+1) .ne. 5) go to 9000
                  if (RCSUB(inc) .eq. 5004.) then
                      RCSUB(inc) = 19.
                  else if (RCSUB(inc) .eq. 5001.) then
                      RCSUB(inc) = 20.
                  else if (RCSUB(inc) .eq. 5002.) then
                      RCSUB(inc) = 21.
                  else if (RCSUB(inc) .eq. 5003) then
                      RCSUB(inc) = 22.
                  else if (RCSUB(inc) .eq. 5007.) then
                      RCSUB(inc) = 23.
                  else if (RCSUB(inc) .eq. 5008.) then
                      RCSUB(inc) = 24.
                  else if (RCSUB(inc) .eq. 5009.) then
                      RCSUB(inc) = 25.
                  else if (RCSUB(inc) .eq. 5010.) then
                      RCSUB(inc) = 26.
                  else if (RCSUB(inc) .eq. 5011.) then
                      RCSUB(inc) = 27.
                  else if (RCSUB(inc) .eq. 5050.) then
                      RCSUB(inc) = 28.
                  else
                      go to 9100
                  endif
c
                  inc    = inc    + 1
                  kfnc   = kfnc   + 1
              endif
c
c.........Real Variable
c
         else if (ICTYP(inc) .eq. 5) then
c
c............Check for subscript
c
              if (NTOK .ne. inc .and. ICTYP(inc+1) .eq. 5) then
                  if (RCSUB(inc) .le. NPNVAR) go to 9000
                  inc    = inc    + 1
                  if (RCSUB(inc) .gt. NPNVAR) go to 9000
              endif
c
c.........Number
c
          else
              if (ICTYP(inc) .eq. 8) ICTYP(inc) = 3
              if (ICTYP(inc) .ne. 3) go to 9100
              kvar   = kvar   + 1
          endif
          inc    = inc    + 1
c
c......This token should be an operator
c
          if (inc .gt. NTOK) go to 8000
          if (ICTYP(inc) .ne. 2) go to 9200
c
c.........Check for valid operator
c
          if (RCSUB(inc) .eq. 8.) then
              ICTYP(inc) = 102
          else if (RCSUB(inc) .eq. 6. .or. RCSUB(inc) .eq. 7.) then
              ICTYP(inc) = 103
          else if (RCSUB(inc) .eq. 4. .or. RCSUB(inc) .eq. 5.) then
              ICTYP(inc) = 104
          else if (RCSUB(inc) .eq. 9. .or. RCSUB(inc) .eq. 10. .or.
     1             RCSUB(inc) .eq. 11. .or. RCSUB(inc) .eq. 12. .or.
     2             RCSUB(inc) .eq. 13. .or. RCSUB(inc) .eq. 14.) then
              ICTYP(inc) = 105
          else if (RCSUB(inc) .eq. 15. .or. RCSUB(inc) .eq. 16.) then
              ICTYP(inc) = 106
          else if (RCSUB(inc) .eq. 3.) then
              ICTYP(inc) = 107
          else
              go to 9200
          endif
          kop    = kop    + 1
c
      go to 100
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
c...Number or Variable expected
c
 9100 call errtxt ('NUMVAREX',cmsg)
      kerr   = 1
      go to 8000
c
c...Operator expected
c
 9200 call errtxt ('OPEREXP',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  solveq (ktyp,gans,garg1,garg2,cmsg,kerr)
c
c   FUNCTION:  This routine solves a simple compiled equation (A = 1+3).
c
c   INPUT:  ktyp    I*4  D1  Type of operator (1-16).
c
c           garg1   R*8  D1  Operand #1.
c
c           garg2   R*8  D1  Operand #2.
c
c   OUTPUT: gans    R*8  D1  Returns the answer.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine solveq (ktyp,gans,garg1,garg2,cmsg,kerr)
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      integer*4 ktyp,kerr
c
      real*8 gans,garg1,garg2
c
      character*(*) cmsg
c
      integer*4 inum
c
      real*8 rnum
c
c...Initialize routine
c
      kerr   = 0
c
c...Go to appropriate section
c
      go to (9000,9000,9000,100,200,300,400,500,600,700,800,900,1000,
     1       1100,1200,1300,9000,9000,1400,1500,1600,1700,1800,1900,
     2       2000,2100,2200), ktyp
c
  100     gans   = garg1 + garg2
          go to 8000
c
  200     gans   = garg1 - garg2
          go to 8000
c
  300     gans   = garg1 * garg2
          go to 8000
c
  400     if (garg2 .eq. 0.) go to 9100
          gans   = garg1 / garg2
          go to 8000
c
  500     if (garg2 .eq. 0.) then
              gans   = 1.
          else if (garg1 .lt. 0.) then
              inum   = dint(garg2)
              gans   = garg1  ** inum
          else
              gans   = garg1 ** garg2
          endif
          go to 8000
c
  600     rnum   = 1.
          if (garg1 .eq. garg2) rnum = 0.
          gans   = rnum
          go to 8000
c
  700     rnum   = 1.
          if (garg1 .lt. garg2) rnum = 0.
          gans   = rnum
          go to 8000
c
  800     rnum   = 1.
          if (garg1 .gt. garg2) rnum = 0.
          gans   = rnum
          go to 8000
c
  900     rnum   = 1.
          if (garg1 .ne. garg2) rnum = 0.
          gans   = rnum
          go to 8000
c
 1000     rnum   = 1.
          if (garg1 .le. garg2) rnum = 0.
          gans   = rnum
          go to 8000
c
 1100     rnum   = 1.
          if (garg1 .ge. garg2) rnum = 0.
          gans   = rnum
          go to 8000
c
 1200     rnum   = 1.
          if (garg1 .eq. 0 .and. garg2 .eq. 0) rnum = 0.
          gans   = rnum
          go to 8000
c
 1300     rnum   = 1.
          if (garg1 .eq. 0 .or. garg2 .eq. 0) rnum = 0.
          gans   = rnum
          go to 8000
c
 1400     gans   = dint(garg1)
          go to 8000
c
 1500     gans   = dcos(garg1/RAD)
          go to 8000
c
 1600     gans   = dsin(garg1/RAD)
          go to 8000
c
 1700     if (dmod(garg1,90.d0) .eq. 0.) go to 9200
          gans   = dtan(garg1/RAD)
          go to 8000
c
 1800     if (garg1 .lt. 0.) go to 9200
          gans   = dsqrt(garg1)
          go to 8000
c
 1900     if (garg1 .lt. -1. .or. garg1 .gt. 1.) go to 9200
          gans   = dacos(garg1) * RAD
          go to 8000
c
 2000     if (garg1 .lt. -1. .or. garg1 .gt. 1.) go to 9200
          gans   = dasin(garg1) * RAD
          go to 8000
c
 2100     gans   = datan(garg1) * RAD
          go to 8000
c
 2200     gans   = dabs(garg1)
          go to 8000
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
c...Divide by zero
c
 9100 call errtxt ('DIVZERO',cmsg)
      kerr   = 1
      go to 8000
c
c...Invalid argument to function
c
 9200 call errtxt ('INVARG',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  vartyp (kinc,kctyp,gcsub,ktok,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine takes a Real number or variable and returns
c              the actual type of variable.  Optimization has been added
c              to convert simple equation parenthesis variables (#1 = 3)
c              into pure real numbers.  The passed array arguments will
c              be changed to reflect the variable type.  1 = Variable.
c              2 = Subscribted variable.  3 = Real number.  Post Var-
c              iables will be ignored and will return the current type.
c
c
c   INPUT:  kinc    I*4  D1  The pointer within the parsed statement
c                            arrays (ICTYP,RCSUB) of the variable to
c                            determine.  'kinc' will also be incremented
c                            by one if the variable is subscripted.
c
c           kctyp   I*4  Dn  Local or global storage of the ICTYP array.
c
c           gcsub   R*8  Dn  Local or global storage of the RCSUB array.
c
c           kcnc    I*4  Dn  Local or global storage of the ICNC array.
c
c           ktok    I*4  D1  Number of values in arrays (NTOK).
c
c           kfl     I*4  D1  0 = Don't allow text variables
c                            1 = Allow text variables (STRLEN, etc.).
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine vartyp (kinc,kctyp,gcsub,kcnc,ktok,kfl,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kinc,kerr,kctyp(50),ktok,kcnc(50),kfl
c
      real*8 gcsub(50)
c
      character*(*) cmsg
c
      integer*4 ipt
c
c...Determine type of variable
c
c...1 = VAR
c...2 = VAR(SUB)
c...3 = 1.0
c...4 = TXTVAR
c...5 = TXTVAR(SUB)
c...6 = string
c
      kerr   = 0
c
c...Parenthesis Variable
c
      if (kctyp(kinc) .eq. 5 .and. gcsub(kinc) .le. MAXPRN) then
          ipt    = gcsub(kinc)
          if (IPNSMP(ipt) .eq. 0) then
              kctyp(kinc) = 1
          else if (IPNSMP(ipt) .eq. 1) then
              kctyp(kinc) = 3
              gcsub(kinc) = PRNVAR(ipt)
          else if (kfl .eq. 1 .and. IPNSMP(ipt) .eq. 3) then
              kctyp(kinc) = 4
          else if (kfl .eq. 1 .and. IPNSMP(ipt) .eq. 4) then
              kctyp(kinc) = 6
              gcsub(kinc) = PRNVAR(ipt)
              kcnc(kinc) = PRNNC(ipt)
          else
              go to 9000
          endif
c
c...Real Variable
c
      else if (kctyp(kinc) .eq. 5) then
c
c......Subscripted variable
c
          if (kinc .lt. ktok .and. kctyp(kinc+1) .eq. 5) then
              ipt    = gcsub(kinc+1)
              if (IPNSMP(ipt) .ne. 0 .and. IPNSMP(ipt) .ne. 1)
     1                go to 9000
c
c.........Simple parenthesis subscript
c
              if (IPNSMP(ipt) .eq. 1) then
                  if (PRNVAR(ipt) .le. 0 .or.
     1                PRNVAR(ipt) .gt. kcnc(kinc)) go to 9100
                  kctyp(kinc) = 1
                  gcsub(kinc) = gcsub(kinc) + PRNVAR(ipt) - 1
c
c.........Equatic parenthesis subscript
c
               else
                  kctyp(kinc) = 2
               endif
c
               kinc   = kinc   + 1
c
c......Non-subscripted variable
c
          else
              if (kcnc(kinc) .ne. 1) go to 9200
              kctyp(kinc) = 1
          endif
c
c...Non-subscripted Post Variable
c
      else if (kctyp(kinc) .eq. 13) then
          kctyp(kinc) = 13
c
c...Argument (%ARG) or
c...Subscripted Post Variable or
c...Clfile Data (%CLDATA)
c
      else if (kctyp(kinc) .eq. 12 .or. kctyp(kinc) .eq. 14 .or.
     1         kctyp(kinc) .eq. 15 .or. kctyp(kinc) .eq. 18) then
          kinc   = kinc   + 1
c
c...Real Number
c
      else
          kctyp(kinc) = 3
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
c...Subscript out of bounds
c
 9100 call errtxt ('SUBOUND',cmsg)
      kerr   = 1
      go to 8000
c
c...Subscript expected
c
 9200 call errtxt ('SUBEXP',cmsg)
      kerr   = 1
      go to 8000
      end
