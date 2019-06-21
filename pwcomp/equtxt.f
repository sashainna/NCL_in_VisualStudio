c
c***********************************************************************
c
c   FILE NAME:  equtxt
c   CONTAINS:
c               cmptxt  txtsyn  solvtx  textyp
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        equtxt.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        09/26/17 , 11:39:30
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmptxt (cmsg,kerr)
c
c   FUNCTION:  This routine compiles a text equation after it has
c              been fully parsed.  If mulitiple operators are in an
c              equation, then the statement will be broken up into mul-
c              tiple operations, using parenthesis variables.  Also,
c              text scalars 'TEXT' will be stored in a parenthesis
c              variable.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine cmptxt (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ifl,nvar,nfnc,nop,ieq,i,ist,ipt,inc,igo,lastop,iarg,
     1          ist0,ist1,ist2,iss,is,ie,icnt
c
c...Initialize routine
c
      kerr   = 0
      ifl    = 0
c
c...Check syntax
c
      call txtsyn (nvar,nfnc,nop,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Break equation into single operation
c
      ieq    = 1
      call textyp (ieq,ICTYP,RCSUB,ist0,ICNC,NTOK,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      ieq    = ieq    + 1
      ICMPL(2) = 2
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
c.........Last operation on statement
c
      if (nop .eq. 0) then
          ICMPL(4) = ICTYP(1)
          JCMPL(4) = RCSUB(1)
          if (ICTYP(1) .eq. 4) then
              ICMPL(9) = ist0
              ICMPL(10) = ICNC(1)
          else
              ICMPL(9) = RCSUB(2)
          endif
c
c.........Create parenthesis variable equation
c
      else
          if (NPNVAR .eq. MAXPRN) go to 9100
          NPNVAR = NPNVAR + 1
          IPNSMP(NPNVAR) = 3
          ICMPL(4) = 4
          JCMPL(4) = NPNVAR
          ICMPL(9) = 1
          ICMPL(10) = 1
      endif
c
c......Write Function equation
c......Function with argument
c
      ICMPL(3) = RCSUB(i)
      if (RCSUB(i) .eq. 35.) then
          iarg   = 1
          call textyp (ipt,ICTYP,RCSUB,ist1,ICNC,NTOK,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
          ICMPL(1) = 18
          ICMPL(5) = ICTYP(ist)
          if (ICTYP(ist) .eq. 4) then
              JCMPL(6) = RCSUB(ist)
              ICMPL(13) = ist1
              ICMPL(14) = ICNC(ist)
          else if (ICTYP(ist) .eq. 6) then
              ICMPL(1) = ICMPL(1) + 1
              ICMPL(ICMPL(1)) = ICNC(ist) - RCSUB(ist) + 1
              if (ICMPL(ICMPL(1)) .le. 0) then
                  ICMPL(ICMPL(1)) = 0
              else
                  iss    = RCSUB(ist)
                  is     = ICMPL(1) * 2 + 1
                  ie     = is     + ICMPL(ICMPL(1)) - 1
                  LCMPL(is:ie) = LCTXT(iss:ICNC(ist))
                  ICMPL(1) = ICMPL(1) + (ICMPL(ICMPL(1))-1) / 2 + 1
              endif
          endif
c
c.........Function without argument
c
      else
          iarg   = 0
          ICMPL(1) = 9
      endif
c
c......Write function
c
      call cmpwrt (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Place parenthesis variable
c......in original equation
c
      if (nfnc .eq. 1 .and. nop .eq. 0) go to 8000
      ICTYP(i) = 6
      RCSUB(i) = NPNVAR
      nfnc   = nfnc   - 1
c
c.........Remove argument if any
c
      if (iarg .eq. 1) then
          NTOK   = NTOK   - 1
          do 700 i=ist,NTOK,1
              ICTYP(i) = ICTYP(i+1)
              RCSUB(i) = RCSUB(i+1)
              ICNC(i) = ICNC(i+1)
  700     continue
      endif
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
      call textyp (i,ICTYP,RCSUB,ist1,ICNC,NTOK,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      i      = ist
      call textyp (i,ICTYP,RCSUB,ist2,ICNC,NTOK,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c............Simple equation A = 'text' + 'stuff'
c
      if (ICTYP(ipt) .eq. 6 .and. ICTYP(ist) .eq. 6) then
          igo    = RCSUB(ist-1)
          call solvtx (igo,ipt,ipt,ist,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          ICTYP(ipt) = 7
          if (igo .eq. 9) ICTYP(ipt) = 6
          if (nop .eq. 1) go to 1600
c
c............Equatic equation A = B + C
c............Create parenthesis variable equation
c
      else
          if (nop .gt. 1) then
              if (NPNVAR .ge. MAXPRN) go to 9100
              NPNVAR = NPNVAR + 1
c
c...Text parenthises variable types are 3
c...Bobby  -  3/24/92
c
c              IPNSMP(NPNVAR) = 0
              IPNSMP(NPNVAR) = 3
              ICMPL(4) = 4
              JCMPL(4) = NPNVAR
              ICMPL(9) = 1
              ICMPL(10) = 1
          else
              JCMPL(4) = RCSUB(1)
              ICMPL(4) = ICTYP(1)
              if (ICTYP(1) .eq. 4) then
                  ICMPL(9) = ist0
                  ICMPL(10) = ICNC(1)
              else
                  ICMPL(9) = RCSUB(2)
              endif
          endif
c
          ICMPL(1) = 18
          ICMPL(3) = RCSUB(ist-1)
          ICMPL(5) = ICTYP(ipt)
          ICMPL(6) = ICTYP(ist)
c
          if (ICTYP(ipt) .eq. 4) then
              JCMPL(6) = RCSUB(ipt)
              ICMPL(13) = ist1
              ICMPL(14) = ICNC(ipt)
c
c...Post arguments use the second I*4
c...for their subscript
c...Bobby  -  3/24/92
c
c          else if (ICTYP(ipt) .eq. 5 .or. ICTYP(ipt) .eq. 12) then
          else if (ICTYP(ipt) .eq. 5) then
              JCMPL(6) = RCSUB(ipt)
              ICMPL(13) = RCSUB(ipt+1)
          else if (ICTYP(ipt) .eq. 12 .or. ICTYP(ipt) .eq. 15 .or.
     1             ICTYP(ipt) .eq. 18) then
              JCMPL(6) = RCSUB(ipt)
              JCMPL(7) = RCSUB(ipt+1)
          else
              ICMPL(1) = ICMPL(1) + 1
              ICMPL(ICMPL(1)) = ICNC(ipt) - RCSUB(ipt) + 1
              if (ICMPL(ICMPL(1)) .le. 0) then
                  ICMPL(ICMPL(1)) = 0
              else
                  iss    = RCSUB(ipt)
                  is     = ICMPL(1) * 2 + 1
                  ie     = is     + ICMPL(ICMPL(1)) - 1
                  LCMPL(is:ie) = LCTXT(iss:ICNC(ipt))
                  ICMPL(1) = ICMPL(1) + (ICMPL(ICMPL(1))-1) / 2 + 1
              endif
          endif
c
          if (ICTYP(ist) .eq. 4) then
              JCMPL(8) = RCSUB(ist)
              ICMPL(17) = ist2
              ICMPL(18) = ICNC(ist)
c
c...Post arguments use the second I*4
c...for their subscript
c...Bobby  -  3/24/92
c
c          else if (ICTYP(ist) .eq. 5 .or. ICTYP(ist) .eq. 12) then
          else if (ICTYP(ist) .eq. 5) then
              JCMPL(8) = RCSUB(ist)
              ICMPL(17) = RCSUB(ist+1)
          else if (ICTYP(ist) .eq. 12 .or. ICTYP(ist) .eq. 15 .or.
     1             ICTYP(ist) .eq. 18) then
              JCMPL(8) = RCSUB(ist)
              JCMPL(9) = RCSUB(ist+1)
          else
              ICMPL(1) = ICMPL(1) + 1
              ICMPL(ICMPL(1)) = ICNC(ist) - RCSUB(ist) + 1
              if (ICMPL(ICMPL(1)) .le. 0) then
                  ICMPL(ICMPL(1)) = 0
              else
                  iss    = RCSUB(ist)
                  is     = ICMPL(1) * 2 + 1
                  ie     = is     + ICMPL(ICMPL(1)) - 1
                  LCMPL(is:ie) = LCTXT(iss:ICNC(ist))
                  ICMPL(1) = ICMPL(1) + (ICMPL(ICMPL(1))-1) / 2 + 1
              endif
          endif
c
          call cmpwrt (cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...............Store parenthesis variable in equation
c
          ICTYP(ipt) = 6
          RCSUB(ipt) = NPNVAR
c
c...Now set up above
c...When it was set here it would
c...overwrite the IMACPT variable when
c...NPNVAR was set to 0 (only a single operation in the equation).
c...Bobby  -  3/24/92
c
c          IPNSMP(NPNVAR) = 3
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
      call textyp (ist,ICTYP,RCSUB,ist1,ICNC,NTOK,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      i      = RCSUB(1)
      if (ICTYP(ipt) .eq. 6 .and. i .le. MAXPRN) then
          IPNSMP(i) = 4
          PRNVAR(i) = RCSUB(ipt)
          PRNNC(i) = ICNC(ipt)
      else
          ICMPL(1) = 18
          ICMPL(2) = 2
          ICMPL(3) = 1
          ICMPL(4) = ICTYP(1)
          ICMPL(5) = ICTYP(ipt)
          JCMPL(4) = RCSUB(1)
c
          if (ICTYP(1) .eq. 4) then
              ICMPL(9) = ist0
              ICMPL(10) = ICNC(1)
          else
              ICMPL(9) = RCSUB(2)
          endif
c
          if (ICTYP(ipt) .eq. 4) then
              JCMPL(6) = RCSUB(ipt)
              ICMPL(13) = ist1
              ICMPL(14) = ICNC(ipt)
c
c...Post arguments use the second I*4
c...for their subscript
c...Bobby  -  3/24/92
c
c          else if (ICTYP(ipt) .eq. 5 .or. ICTYP(ipt) .eq. 12) then
          else if (ICTYP(ipt) .eq. 5) then
              JCMPL(6) = RCSUB(ipt)
              ICMPL(13) = RCSUB(ipt+1)
          else if (ICTYP(ipt) .eq. 12 .or. ICTYP(ipt) .eq. 15 .or.
     1             ICTYP(ipt) .eq. 18) then
              JCMPL(6) = RCSUB(ipt)
              JCMPL(7) = RCSUB(ipt+1)
          else
              ICMPL(1) = ICMPL(1) + 1
              ICMPL(ICMPL(1)) = ICNC(ipt) - RCSUB(ipt) + 1
              if (ICMPL(ICMPL(1)) .le. 0) then
                  ICMPL(ICMPL(1)) = 0
              else
                  iss    = RCSUB(ipt)
                  is     = ICMPL(1) * 2 + 1
                  ie     = is     + ICMPL(ICMPL(1)) - 1
                  LCMPL(is:ie) = LCTXT(iss:ICNC(ipt))
                  ICMPL(1) = ICMPL(1) + (ICMPL(ICMPL(1))-1) / 2 + 1
              endif
          endif
c
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
c   SUBROUTINE:  txtsyn (kvar,kfnc,kop,cmsg,kerr)
c
c   FUNCTION:  This routine checks the syntax of a fully parsed text
c              equation.
c
c   INPUT:  none.
c
c   OUTPUT: kvar    I*4  D1  Returns the number of variables in the
c                            equation, excluding the variable to the
c                            left of the ='s.
c
c           kfnc    I*4  D1  Returns the number of functions.
c
c           kop     I*4  D1  Returns the number of operators (+ ==)
c                            in the equation, excluding the '='.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine txtsyn (kvar,kfnc,kop,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr,kvar,kfnc,kop
c
      character*(*) cmsg
c
      integer*4 inc,ist,isav
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
      if (ICTYP(inc) .ne. 2 .or. RCSUB(inc) .ne. 1.) go to 9000
c
c......This token should be a Text string,
c......Text variable or
c......Function
c
  100     if (inc .eq. NTOK) go to 9100
          inc    = inc    + 1
          if (ICTYP(inc) .eq. 5) then
              ist    = RCSUB(inc)
              if (ist .le. MAXPRN .and. (IPNSMP(ist) .eq. 3 .or.
     1            IPNSMP(ist) .eq. 4)) ICTYP(inc) = 6
          endif
c
c.........Unquoted Text string
c.........See if it's actually a Text variable
c
          if (ICTYP(inc) .eq. 4) then
              ist    = RCSUB(inc)
              if ((ICNC(inc)-ist+1) .gt. 24) go to 9100
              lbuf   = LCTXT(ist:ICNC(inc))
              call getscl (lbuf,ICTYP(inc),ist,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (ICTYP(inc) .ne. 6) go to 9100
              RCSUB(inc) = MFDAT(ist+7,4)
              ICNC(inc)  = MFDAT(ist+6,4)
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
c............Post Argument
c
              if (RCSUB(inc) .eq. 6000. .or. RCSUB(inc) .eq. 6064 .or.
     1            RCSUB(inc) .eq. 6071. .or. RCSUB(inc) .eq. 6072. .or.
     2            RCSUB(inc) .eq. 6080) then
                  isav   = inc
                  call pstvar (inc,ICTYP,RCSUB,NTOK,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  if (ICTYP(isav) .ne. 12 .and. ICTYP(isav) .ne. 15
     1                .and.  ICTYP(isav) .ne. 18) go to 9100
                  kvar   = kvar   + 1
c
c............Function
c............Make sure it's a valid Text Function
c............DATE  TIME  COMMAND  ERRTXT  COMPUTER  USERNAME
c............PCHFILE  PCHBRIEF  PCHNAME  CLNAME
c
              else
                  if (RCSUB(inc) .eq. 5016.) then
                      RCSUB(inc) = 32.
                  else if (RCSUB(inc) .eq. 5017.) then
                      RCSUB(inc) = 33.
                  else if (RCSUB(inc) .eq. 5018.) then
                      RCSUB(inc) = 34.
                  else if (RCSUB(inc) .eq. 5019.) then
                      RCSUB(inc) = 35.
                      if (inc .eq. NTOK .or. ICTYP(inc+1) .ne. 5)
     1                    go to 9000
                      inc    = inc    + 1
                      ist    = RCSUB(inc)
                      if (IPNSMP(ist) .ne. 3 .and. IPNSMP(ist) .ne. 4)
     1                    go to 9000
                      ICTYP(inc) = 6
                  else if (RCSUB(inc) .eq. 5045.) then
                      RCSUB(inc) = 37.
                  else if (RCSUB(inc) .eq. 5046.) then
                      RCSUB(inc) = 38.
                  else if (RCSUB(inc) .eq. 5024.) then
                      RCSUB(inc) = 39.
                  else if (RCSUB(inc) .eq. 5040.) then
                      RCSUB(inc) = 40.
                  else if (RCSUB(inc) .eq. 5041.) then
                      RCSUB(inc) = 41.
                  else if (RCSUB(inc) .eq. 5032.) then
                      RCSUB(inc) = 42.
                  else if (RCSUB(inc) .eq. 5048.) then
                      RCSUB(inc) = 43.
                  else if (RCSUB(inc) .eq. 5049.) then
                      RCSUB(inc) = 44.
c
c.....add LICENSE
c
                  else if (RCSUB(inc) .eq. 5051.) then
                      RCSUB(inc) = 45.
                  else
                      go to 9100
                  endif
                  kfnc   = kfnc   + 1
              endif
c
c.........Text string or Text Variable
c
          else
              if (ICTYP(inc) .ne. 6 .and. ICTYP(inc) .ne. 7) go to 9100
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
          if (RCSUB(inc) .eq. 4.) then
              ICTYP(inc) = 104
          else if (RCSUB(inc) .eq. 9. .or. RCSUB(inc) .eq. 12.) then
              ICTYP(inc) = 105
          else if (RCSUB(inc) .eq. 15. .or. RCSUB(inc) .eq. 16.) then
              ICTYP(inc) = 106
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
c...Text string or Variable expected
c
 9100 call errtxt ('TXTVAREX',cmsg)
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
c   SUBROUTINE:  solvtx (ktyp,kinc,garg1,garg2,cmsg,kerr)
c
c   FUNCTION:  This routine solves a simple text equation (A = 'text').
c
c   INPUT:  ktyp    I*4  D1  Type of operator (4,9,15,16).
c
c           kinc    I*4  D1  Pointer inside arrays (RCSUB,ICNC,etc) to
c                            receive answer.
c
c           karg1   I*4  D1  Operand #1.
c
c           karg2   I*4  D1  Operand #2.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine solvtx (ktyp,kinc,karg1,karg2,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 ktyp,kerr,kinc,karg1,karg2
c
      character*(*) cmsg
c
      integer*4 is1,is2,nc,nce
c
      character*512 tbuf
c
c...Initialize routine
c
      kerr   = 0
      is1    = RCSUB(karg1)
      is2    = RCSUB(karg2)
c
c...Var = 'text' + 'string'
c
      if (ktyp .eq. 4) then
          nc     = (ICNC(karg1)-is1+1) + (ICNC(karg2)-is2+1)
          nce    = ITXTPT + nc     - 1
          tbuf = LCTXT(is1:ICNC(karg1)) // LCTXT(is2:ICNC(karg2))
          LCTXT(ITXTPT:nce) = tbuf
          RCSUB(kinc) = ITXTPT
          ICNC(kinc) = nce
          ITXTPT = ITXTPT + nc
      else if (ktyp .eq. 9) then
          if (ICNC(karg1) .eq. 1 .and. ICNC(karg2) .eq. 1 .and.
     1        LCTXT(is1:ICNC(karg1)) .eq. LCTXT(is2:ICNC(karg2))) then
              LCTXT(ITXTPT:ITXTPT) = ' '
          else
              LCTXT(ITXTPT:ITXTPT) = '1'
          endif
          RCSUB(kinc) = ITXTPT
          ICNC(kinc) = 1
          ITXTPT = ITXTPT + 1
      else if (ktyp .eq. 15) then
          if (ICNC(karg1) .eq. 1 .and. ICNC(karg2) .eq. 1 .and.
     1        LCTXT(is1:ICNC(karg1)) .eq. ' ' .and.
     2        LCTXT(is2:ICNC(karg2)) .eq. ' ') then
              LCTXT(ITXTPT:ITXTPT) = ' '
          else
              LCTXT(ITXTPT:ITXTPT) = '1'
          endif
          RCSUB(kinc) = ITXTPT
          ICNC(kinc) = 1
          ITXTPT = ITXTPT + 1
      else if (ktyp .eq. 16) then
          if (ICNC(karg1) .eq. 1 .and. ICNC(karg2) .eq. 1 .and.
     1        (LCTXT(is1:ICNC(karg1)) .eq. ' ' .or.
     2        LCTXT(is2:ICNC(karg2)) .eq. ' ')) then
              LCTXT(ITXTPT:ITXTPT) = ' '
          else
              LCTXT(ITXTPT:ITXTPT) = '1'
          endif
          RCSUB(kinc) = ITXTPT
          ICNC(kinc) = 1
          ITXTPT = ITXTPT + 1
      else
          go to 9000
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
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  textyp (kinc,kctyp,gcsub,kst,kcnc,ktok,cmsg,kerr)
c
c   FUNCTION:  This routine takes a Text string or variable and returns
c              the actual type of variable.  Optimization has been added
c              to convert simple equation parenthesis variables (#1 =
c              'string') into pure text.  The passed array arguments
c              will be changed to reflect the variable type.  4 =
c              Variable.  5 = Subscribted variable.  6 = Text string.
c              Post Arguments will be ignored and will return the cur-
c              rent type.
c
c
c   INPUT:  kinc    I*4  D1  The pointer within the parsed statement
c                            array arguments of the variable to deter-
c                            mine.  'kinc' will also be incremented by
c                            one if the variable is subscripted.
c
c           kctyp   I*4  Dn  Local or global storage of the ICTYP array.
c
c           gcsub   R*8  Dn  Local or global storage of the RCSUB array.
c
c           kcnc    I*4  Dn  Local or global storage of the ICNC array.
c
c           ktok    I*4  D1  Number of values in arrays (NTOK).
c
c   OUTPUT: kst     I*4  D1  Stores the subscript number when a single
c                            subscript value is given.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine textyp (kinc,kctyp,gcsub,kst,kcnc,ktok,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kinc,kerr,kctyp(50),ktok,kcnc(50),kst
c
      real*8 gcsub(50)
c
      character*(*) cmsg
c
      integer*4 ipt
c
c...Determine type of variable
c
c...4 = VAR
c...5 = VAR(SUB)
c...6 = 'text'
c
      kerr   = 0
c
c...Parenthesis Variable
c
      if (kctyp(kinc) .eq. 6 .and. gcsub(kinc) .le. MAXPRN) then
          ipt    = gcsub(kinc)
          if (IPNSMP(ipt) .eq. 3) then
              kctyp(kinc) = 4
          else if (IPNSMP(ipt) .eq. 4) then
              kctyp(kinc) = 6
              gcsub(kinc) = PRNVAR(ipt)
              kcnc(kinc) = PRNNC(ipt)
          endif
c
c...Text Variable
c
      else if (kctyp(kinc) .eq. 6) then
c
c......Subscripted variable
c
          if (kinc .lt. ktok .and. kctyp(kinc+1) .eq. 5) then
              ipt    = gcsub(kinc+1)
c
c.........Simple parenthesis subscript
c
              if (IPNSMP(ipt) .eq. 1) then
                  if (PRNVAR(ipt) .le. 0 .or.
     1                PRNVAR(ipt) .gt. kcnc(kinc)) go to 9100
                  kctyp(kinc) = 4
                  kst    = PRNVAR(ipt)
                  kcnc(kinc) = PRNVAR(ipt)
c                  gcsub(kinc) = gcsub(kinc) + PRNVAR(ipt) - 1
c                  kcnc(kinc) = 1
c
c.........Equatic parenthesis subscript
c
               else if (IPNSMP(ipt) .eq. 0 .or. IPNSMP(ipt) .eq. 2) then
                  kctyp(kinc) = 5
               else
                  go to 9000
               endif
c
               kinc   = kinc   + 1
c
c......Non-subscripted variable
c
          else
              kctyp(kinc) = 4
              kst    = 1
          endif
c
c...Post Argument
c...Clfile Data
c
      else if (kctyp(kinc) .eq. 12 .or. kctyp(kinc) .eq. 15 .or.
     1         kctyp(kinc) .eq. 18) then
          kinc   = kinc   + 1
c
c...Text string
c
      else
          kctyp(kinc) = 6
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
      return
c
c...Subscript out of bounds
c
 9100 call errtxt ('SUBOUND',cmsg)
      kerr   = 1
      go to 8000
      end
