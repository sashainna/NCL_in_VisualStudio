c
c***********************************************************************
c
c   FILE NAME:  chrmsc.for
c   CONTAINS:
c               cdtoc   ctocd  mininc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        chrmsc.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cdtoc (kcod,gval,cbuf,knc)
c
c   FUNCTION:  This routine converts a numeric register and a value into
c              its text representation, for example, 'A1(3.5)'.
c
c   INPUT:  kcod    I*4  D1  -  Numeric representation of register
c                               (1-MAXFMT) or -1 = G-code, -2 = M-code,
c                               -3 = (EOB), -4-12 = XUYVZWABC@, -13 =
c                               F.
c
c           gval    R*8  D1  -  Value stored in register.
c
c   OUTPUT: cbuf    C*n  D1  -  Text representation of register and
c                               value.
c
c           knc     I*4  D1  -  Number of chars in 'cbuf'.
c
c***********************************************************************
c
      subroutine cdtoc (kcod,gval,cbuf,knc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 kcod,knc
c
      real*8 gval
c
      character*(*) cbuf
c
      integer*4 strlen1,nc,inc
c
      character*14 lcod
      character*20 sbuf,abuf
c
      data lcod /'GM$XUYVZWABC@F'/
c
c...No register in this code
c...Return blank character string
c
      if (kcod .eq. 0 .or. kcod .lt. -14 .or. kcod .gt. MAXFMT) then
          cbuf   = ' '
          knc    = 0
          go to 8000
c
c...Special code
c
      else if (kcod .lt. 0) then
          inc    = kcod   * (-1)
          cbuf   = lcod(inc:inc)
          knc    = 1
c
c...Standard register name
c
      else
          cbuf   = REGID(kcod)
          knc    = strlen1(cbuf)
      endif
c
c...Get register value
c...If register is filtered out (-register_name -> register(99999999)),
c...format the output.
c...If register has an ordinary value, convert to string, concat
c...to string containing register name, and output.
c
      if (gval .eq. DUMMY) go to 8000
      if (gval .eq. 99999999) then
           sbuf = '-' // cbuf(1:knc)
           knc = knc + 1
      else
           call rtoc (gval,abuf,nc)
           sbuf   = cbuf(1:knc) // '(' // abuf(1:nc) // ')'
           knc    = knc    + nc     + 2
      endif
      cbuf   = sbuf
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ctocd (cbuf,knc,kcod,gval,kerr)
c
c   FUNCTION:  This routine converts a text representation of a register
c              and its value, 'A1(3.5)' and converts it into a register
c              number and value.
c
c   INPUT:  cbuf    C*n  D1  -  Text representation of register and
c                               value.
c
c           knc     I*4  D1  -  Number of chars in 'cbuf'.
c
c   OUTPUT: kcod    I*4  D1  -  Numeric representation of register
c                               (1-MAXFMT) or -1 = G-code, -2 = M-code,
c                               -3 = (EOB), -4-12 = XUYVZWABC@, -13 =
c                               F.
c
c           gval    R*8  D1  -  Value stored in register.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ctocd (cbuf,knc,kcod,gval,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 knc,kcod,kerr
c
      real*8 gval
c
      character*(*) cbuf
c
      integer*4 icod,inc,inc2,i,index,nc,strlen1
c
      real*8 rval
c
      character*14 lcod
      character*20 abuf,sbuf,bbuf
c
      data lcod /'GM$XUYVZWABC@F'/
c
c...Get register number
c
      kerr   = 0
      call touppr (cbuf(1:knc),sbuf)
      inc    = index(sbuf(1:knc),'(')
      if (inc .eq. 0) then
          abuf   = sbuf
          nc = knc
      else if (inc .eq. 1) then
          go to 9000
      else
          abuf   = sbuf(1:inc-1)
          nc = inc-1
      endif
c
c...Check if register output is to be filtered out.
c...Format is -register_name.  (ex.  -A means don't output A)
c
      inc2 = 0
      inc2 = index (abuf(1:nc),'-')
      if (inc2 .gt. 0) then
           bbuf = abuf(inc2+1:nc)
      else
           bbuf = abuf(1:nc)
      endif
      nc     = strlen1(bbuf)
      if (nc .gt. 2) go to 9000
c
      if (nc .eq. 1 .and. index(lcod,bbuf(1:1)) .ne. 0) then
          icod   = index(lcod,bbuf(1:1)) * (-1)
          if (icod .eq. -3 .and. inc .ne. 0) go to 9000
          if ((icod .eq. -1 .or. icod .eq. -2) .and. inc .eq. 0)
     1            go to 9000
      else if (nc .eq. 0) then
          icod   = 0
      else
          do 100 i=1,MAXFMT,1
              if (bbuf(1:nc) .eq. REGID(i)) go to 200
  100     continue
          go to 9000
  200     icod   = i
      endif
c
c...Get the register's value
c
      if (inc .eq. 0) then
          rval   = DUMMY
      else
          if (sbuf(knc:knc) .ne. ')') go to 9000
          if (inc+1 .eq. knc) go to 9000
          call ctor (sbuf(inc+1:knc-1),rval,kerr)
          if (kerr .ne. 0) go to 9000
      endif
      if (inc2 .gt. 0)  rval = 99999999
c
c...Store the register and value
c
      kcod   = icod
      gval   = rval
c
c...End of routine
c
 8000 return
c
c...An error occurred parsing the input
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mininc (kreg,gval)
c
c   FUNCTION:  This routine calculates the minimum value which can be
c              output using a specific register.
c
c   INPUT:  kreg    I*4  D1  -  Register to determine minimum value for.
c
c   OUTPUT: gval    R*8  D1  -  Minimum value which can be output using
c                               this register.
c
c***********************************************************************
c
      subroutine mininc (kreg,gval)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087)), (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 IUNIT
c
      integer*4 kreg
c
      real*8 gval
c
c...Determine minimum increment for this register
c
      if (kreg .ne. 0 .and. FMTDES(10,kreg) .ne. 0) then
          gval   = 1.d0 / (10.d0**FMTDES(5+IUNIT,kreg))
c
c...No such register
c
      else
          gval   = 99999999.
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  crptmsg (knc,opmsg,crmsg)
c
c   FUNCTION:  This routine encrypts a text string.
c
c   INPUT:  knc     I*4  D1  -  Input string length.
c
c           opmsg   C*n  D1  -  Input text string.
c
c   OUTPUT: crmsg   C*n  D1  -  Encrypted output string.
c
c***********************************************************************
c
      subroutine crptmsg (knc,opmsg,crmsg)
c
      integer*4 knc
      character*(*) opmsg,crmsg
c
      character*80 bufo
c
      integer*4 n,i,p,ic,k,inc
c
      p    = (knc + 1) / 2
      inc  = 1
      if (2*p .eq. knc) inc = 3
      k    = 1
      n    = 1
      do 215 i=knc,1,-1
         ic = ichar(opmsg(i:i))
         ic = 158 - ic
         bufo(k:k) = char(ic)
         if (n .eq. p) k = k + inc
         if (n .lt. p) then
            k = k + 2
         else
            k = k - 2
         end if
         n    = n + 1
  215 continue
      crmsg = bufo(1:knc)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  uncrmsg (knc,crmsg,opmsg)
c
c   FUNCTION:  This routine decrypts a text string.
c
c   INPUT:  knc     I*4  D1  -  Input string length.
c
c           opmsg   C*n  D1  -  Input encrypted string.
c
c   OUTPUT: crmsg   C*n  D1  -  Output text string.
c
c***********************************************************************
c
      subroutine uncrmsg (knc,crmsg,opmsg)
c
      integer*4 knc
      character*(*) opmsg,crmsg
c
      character*80 bufo
c
      integer*4 n,i,p,ic,k,inc
c
      p    = (knc + 1) / 2
      inc  = 1
      if (2*p .eq. knc) inc = 3
      k    = 1
      n    = 1
      do 215 i=knc,1,-1
         ic = ichar(crmsg(k:k))
         ic = 158 - ic
         bufo(i:i) = char(ic)
         if (n .eq. p) k = k + inc
         if (n .lt. p) then
            k = k + 2
         else
            k = k - 2
         end if
         n    = n + 1
  215 continue
      crmsg = bufo(1:knc)
c
      return
      end
