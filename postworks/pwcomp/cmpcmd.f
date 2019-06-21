c
c***********************************************************************
c
c   FILE NAME:  compil
c   CONTAINS:
c               cmpcmd  cmpscl  termac  endmac  cmpcon  cmpjmp  enadis
c               cmpsyn  nxtsyn
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmpcmd.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:12
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmpcmd (kctyp,gcsub,kcnc,ktok,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine is the 1st pass compiling routine for com-
c              piler commands (IF, DO, TERMAC, etc.).
c
c   INPUT:  kctyp   I*4  Dn  Local storage of the ICTYP common array.
c
c           gcsub   R*8  Dn  Local storage of the RCSUB common array.
c
c           kcnc    I*4  Dn  Local storage of the ICNC common array.
c
c           ktok    I*4  D1  Local NTOK variable.
c
c   OUTPUT: kfl     I*4  D1  Returns 1 when the common token arrays
c                            contain a command to be parsed.  This
c                            may occur during an IF command.  'kfl'
c                            should be set to 0 after processing the
c                            new command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling.
c
c***********************************************************************
c
      subroutine cmpcmd (kctyp,gcsub,kcnc,ktok,kfl,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr,kfl,kctyp(50),kcnc(50),ktok
c
      real*8 gcsub(50)
c
      character*(*) cmsg
c
      integer*4 ityp,isav
c
c...Termac
c
      if (RCSUB(1) .eq. 4010.) then
          call termac (cmsg,kerr)
c
c...Real
c...Char
c
      else if (RCSUB(1) .eq. 4008. .or. RCSUB(1) .eq. 4007.) then
          ityp   = 1
          if (RCSUB(1) .eq. 4007.) ityp = 2
          call cmpscl (ityp,cmsg,kerr)
c
c...SYNTAX
c
      else if (RCSUB(1) .eq. 4011.) then
          call cmpsyn (cmsg,kerr)
c
c...JUMPTO
c
      else if (RCSUB(1) .eq. 4014.) then
          call cmpjmp (cmsg,kerr)
c
c...CONTINUE
c
      else if (RCSUB(1) .eq. 4005.) then
          call cmpcon (cmsg,kerr)
c
c...ENABLE/DISABLE
c
      else if (RCSUB(1) .eq. 4015. .or. RCSUB(1) .eq. 4016.) then
          call enadis (cmsg,kerr)
c
c...DO
c
      else if (RCSUB(1) .eq. 4002.) then
          call cmpdo (kctyp,gcsub,kcnc,ktok,cmsg,kerr)
c
c...IF
c
      else if (RCSUB(1) .eq. 4001.) then
          call cmpif (kfl,cmsg,kerr)
c
c...ELSE
c
      else if (RCSUB(1) .eq. 4004.) then
          call cmpels (cmsg,kerr)
c
c...ENDIF
c
      else if (RCSUB(1) .eq. 4018.) then
          call cmpenf (cmsg,kerr)
c
c...ENDDO
c
      else if (RCSUB(1) .eq. 4062.) then
          if (IDPT .eq. 0) go to 9300
          isav   = IFPT
          IFPT   = IDARY(IDPT)
          call elsjmp (cmsg,kerr)
          IFPT   = isav
c
c...ENDMAC
c
      else if (RCSUB(1) .eq. 4063.) then
          call endmac (cmsg,kerr)
c
c...BREAKF
c
      else if (RCSUB(1) .eq. 4064.) then
          if (IFPT .eq. 0 .or. IFFLAG .eq. 2 .or.
     1        (IDPT .ne. 0 .and. IDARY(IDPT) .eq. IFPT)) go to 9200
          call elsjmp (cmsg,kerr)
c
c...Unrecognized command
c
      else
          go to 9100
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
c...Unrecognized command
c
 9100 call errtxt ('UNRECCMD',cmsg)
      kerr   = 1
      go to 8000
c
c...An IF structure is not being defined
c
 9200 call errtxt ('IFNOTDEF',cmsg)
      kerr   = 1
      go to 8000
c
c...A DO loop is not being defined
c
 9300 call errtxt ('DONOTDEF',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpscl (ktyp,cmsg,kerr)
c
c   FUNCTION:  This routine is the 1st pass compiling routine for REAL
c              & CHAR scalar specifications.
c
c   INPUT:  ktyp    I*4  D1  1 = REAL declaration.  2 = CHAR declaration
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine cmpscl (ktyp,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr,ktyp
c
      character*(*) cmsg
c
      integer*4 i,idim(3),nc,ist,isca(6),ipt,inum,ndim
c
      character*24 lsca
c
      equivalence (isca,lsca)
c
c...Check syntax
c
      if (NTOK .lt. 2) go to 9000
c
c...Parse out scalars & dimensions
c
      i      = 1
  100 i      = i      + 1
          if (i .gt. NTOK) go to 8000
c
c......This parameter should be a text string
c
          if (ICTYP(i) .ne. 4) go to 9100
          ist    = RCSUB(i)
          nc     = ICNC(i) - ist    + 1
          if (nc .lt. 1 .or. nc .gt. 24) go to 9200
c
c......Check for redefinition of scalar
c
          call touppr (LCTXT(ist:ICNC(i)),lsca)
          call getscl (lsca,ICTYP(i),inum,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if (ICTYP(i) .ne. 4) go to 9100
c
c......Check for next scalar
c
          ipt    = i
          idim(1) = 1
          idim(2) = 1
          idim(3) = 1
          ndim   = 0
          if (ipt .ne. NTOK) then
              i      = i      + 1
c
c......Dimensioned scalar
c
              if (ICTYP(i) .eq. 5) then
                  call vartyp (i,ICTYP,RCSUB,ICNC,NTOK,cmsg,0,kerr)
                  if (kerr .ne. 0) go to 8000
                  if (ICTYP(i) .ne. 3) go to 9000
                  if (RCSUB(i) .lt. 1. .or. RCSUB(i) .gt. 32767.)
     1                    go to 9500
                  ndim   = 1
                  idim(1) = RCSUB(i)
                  i      = i      + 1
c
c.....Multi-dimensional scalar
c
              else if (ICTYP(i) .eq. 2 .and. RCSUB(i) .eq. 17) then
  200             i      = i      + 1
                      call vartyp (i,ICTYP,RCSUB,ICNC,NTOK,0,cmsg,kerr)
                      if (kerr .ne. 0) go to 8000
                      if (ICTYP(i) .ne. 3) go to 9300
                      if (RCSUB(i) .lt. 1. .or. RCSUB(i) .gt. 32767.)
     1                    go to 9500
                      if (ndim .eq. 3) go to 9400
                      ndim   = ndim   + 1
                      idim(ndim) = RCSUB(i)
                      if (i .eq. NTOK) go to 9400
                      i      = i      + 1
                      if (ICTYP(i) .ne. 2) go to 9400
                      if (RCSUB(i) .eq. 2) go to 200
                      if (RCSUB(i) .ne. 18) go to 9400
                  i      = i      + 1
              endif
c
c......Check for comma
c
              if (i .eq. NTOK) go to 9000
              if (i .lt. NTOK .and. (ICTYP(i) .ne. 2 .or.
     1            RCSUB(i) .ne. 2)) go to 9000
          endif
c
c......Store new scalar
c
          if (idim(1)*idim(2)*idim(3) .gt. 32767.) go to 9500
          call stoscl (lsca,ktyp,idim,ndim,cmsg,kerr)
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
c...Variable already defined
c
 9100 if (ICTYP(i) .ne. 5 .and. ICTYP(i) .ne. 6) go to 9000
      call errtxt ('MULTVAR',cmsg)
      kerr   = 1
      go to 8000
c
c...Maximum 24 chars per name
c
 9200 call errtxt ('LABMAX',cmsg)
      kerr   = 1
      go to 8000
c
c...Number expected
c
 9300 call errtxt ('NUMBEXP',cmsg)
      kerr   = 1
      go to 8000
c
c...Right parenthesis expected
c
 9400 call errtxt ('PARENRT',cmsg)
      kerr   = 1
      go to 8000
c
c...Number out of range
c
 9500 call errtxt ('INPRANG',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  termac (cmsg,kerr)
c
c   FUNCTION:  This routine processes the TERMAC command.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine termac (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i
c
c...Check syntax
c
      if (NTOK .ne. 1) go to 9000
      if (IMACDF .eq. 0) go to 9100
      IMACDF = 0
c
c...Store ending Macro pointers
c
      MFDAT(IMACPT+2,1) = NLABEL
      MFDAT(IMACPT+4,1) = NSCAL(1)
      MFDAT(IMACPT+6,1) = NSCAL(2)
c
c...The first Macro is the main routine
c
      if (NMACRO .eq. 1) then
          do 100 i=1,8,1
              MFMAIN(i) = MFDAT(IMACPT+i-1,1)
  100     continue
      endif
c
c...Write TERMAC record to compiled statements
c
      ICMPL(1) = 2
      ICMPL(2) = 6
      call cmpwrt (cmsg,kerr)
      if (IFPT .ne. 0) go to 9200
c
c...End of routine
c
 8000 LISLIN = IOPFL(3) + 1
      DOCLIN = IOPFL(3) + 1
      LMACRO = ' '
      return
c
c...Invalid syntax
c
 9000 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8000
c
c...Termac allowed in Macro only
c
 9100 call errtxt ('MACONLY',cmsg)
      kerr   = 1
      go to 8000
c
c...Open IF/DO structure
c
 9200 call errtxt ('UNCLSDIF',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  endmac (cmsg,kerr)
c
c   FUNCTION:  This routine processes the ENDMAC command.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine endmac (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Check syntax
c
      if (NTOK .ne. 1) go to 9000
      if (IMACDF .eq. 0) go to 9100
c
c...Write TERMAC record to compiled statements
c
      ICMPL(1) = 2
      ICMPL(2) = 16
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
c...Termac allowed in Macro only
c
 9100 call errtxt ('MACONLY',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpcon (cmsg,kerr)
c
c   FUNCTION:  This routine processes the CONTINUE command.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpcon (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Initialize routine
c
      kerr   = 0
c
c...Check syntax of command &
c...store in COMPIL arrays
c
      if (NTOK .ne. 1) go to 9000
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
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpjmp (cmsg,kerr)
c
c   FUNCTION:  This routine processes the JUMPTO command.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpjmp (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ist
c
c...Initialize routine
c
      kerr   = 0
c
c...Check syntax of command &
c...store in COMPIL arrays
c
      if (NTOK .ne. 3) go to 9000
      if (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 7) go to 9000
      if (ICTYP(3) .ne. 4) go to 9100
      ist    = RCSUB(3)
      if ((ICNC(3)-ist+1) .gt. 24) go to 9100
c
      ICMPL(1) = 14
      ICMPL(2) = 4
      LCMPL(5:28) = LCTXT(ist:ICNC(3))
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
c...Label expected
c
 9100 call errtxt ('LABEXP',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  enadis (cmsg,kerr)
c
c   FUNCTION:  This routine processes the ENABLE/DISABLE commands.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine enadis (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
c...Initialize routine
c
      kerr   = 0
c
c...Check syntax of command &
c...store in COMPIL arrays
c
      if (NTOK .ne. 3) go to 9000
      if (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 7) go to 9000
      if (ICTYP(3) .ne. 1) go to 9100
      if (RCSUB(3) .ge. 2000. .and. RCSUB(3) .ne. 4013. .and.
     1    RCSUB(3) .ne. 4012. .and. RCSUB(3) .ne. 4022.) go to 9100
c
      ICMPL(1) = 4
      ICMPL(2) = 10
      ICMPL(3) = 0
      if (RCSUB(1) .eq. 4015.) ICMPL(3) = 1
      ICMPL(4) = RCSUB(3)
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
c...Vocabulary word expected
c
 9100 call errtxt ('VOCEXP',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpsyn (cmsg,kerr)
c
c   FUNCTION:  This routine processes the SYNTAX command.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpsyn (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 is,ipt,jpt,irt,inc,ist,iend,nprm,i
c
c...Initialize routine
c
      kerr   = 0
      ICMPL(1) = 4
      ICMPL(2) = 9
      ICMPL(3) = 0
      if (NTOK .lt. 3) go to 9000
      if (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 7) go to 9000
      inc    = 3
      is     = 5
c
c...Check syntax of command &
c...store in COMPIL arrays
c
  100 if (inc .gt. NTOK) go to 1000
c
c......Get next parameter bundle
c
         call nxtsyn (inc,ist,iend,nprm,cmsg,kerr)
         if (kerr .ne. 0) go to 8000
c
c......Store parameter(s)
c
         ICMPL(3) = ICMPL(3) + 1
         ICMPL(is) = nprm
         ipt    = ((is+nprm-1)/4 + 1) * 4 + 1
         jpt    = ipt     / 2 + 1
         irt    = jpt    / 2 + 1
         do 500 i=1,nprm,1
             is     = is     + 1
             ICMPL(is) = ICTYP(ist)
             if (ipt .gt. 200) go to 9200
c
c.........Post word
c
             if (ICTYP(ist) .eq. 0) then
                 JCMPL(jpt) = RCSUB(ist)
                 ist    = ist    + 2
                 ipt    = ipt    + 4
                 jpt    = jpt    + 2
                 irt    = irt    + 1
c
c.........Number
c
              else if (ICTYP(ist) .eq. 3) then
                 RCMPL(irt) = RCSUB(ist)
                 ist    = ist    + 2
                 ipt    = ipt    + 4
                 jpt    = jpt    + 2
                 irt    = irt    + 1
c
c.........Range
c
              else if (ICTYP(ist) .eq. 11) then
                  if (ipt+4 .gt. 200) go to 9200
                  RCMPL(irt) = RCSUB(ist)
                  RCMPL(irt+1) = RCSUB(ist+2)
                  ist    = ist    + 4
                  ipt    = ipt    + 8
                  jpt    = jpt    + 4
                  irt    = irt    + 2
c
c.........Unsupported data type
c
              else
                  go to 9100
              endif
  500     continue
          is     = ipt
      go to 100
c
c...Write out compiled record
c
 1000 ICMPL(1) = ipt    - 1
      call cmpwrt (cmsg,kerr)
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
c...Unsupported data type
c
 9100 call errtxt ('WRDNUMRG',cmsg)
      kerr   = 1
      go to 8000
c
c...Too many parameters
c
 9200 call errtxt ('INVNUMPR',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  nxtsyn (kinc,kst,kend,kprm,cmsg,kerr)
c
c   FUNCTION:  This routine returns the next parameter bundle in a
c              SYNTAX command.  A parameter bundle can consist of either
c              a single parameter or multiple parameters enclosed in
c              parenthesis.  Valid parameters are Numbers, Post words &
c              Numeric ranges (0:1).  Uses the global arrays ICTYP,
c              RCSUB and NTOK.
c
c   INPUT:  kinc    I*4  Dn  Pointer within global arrays at the index
c                            at which to start parsing.  This  variable
c                            will also be modified to point to the index
c                            of the next parameter bundle.
c
c
c
c   OUTPUT: kst     I*4  D1  Returns the beginning index within the
c                            global arrays of the current parameter
c                            bundle.
c
c           kend    I*4  D1  Returns the ending index of the current
c                            parameter bundle.
c
c           kprm    I*4  D1  Returns the number of entries in the cur-
c                            rent parameter bundle.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine nxtsyn (kinc,kst,kend,kprm,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kinc,kerr,kst,kend,kprm
c
      character*(*) cmsg
c
      integer*4 imult
c
c...Initialize routine
c
      kerr   = 0
      kprm   = 0
c
c...Determine if this is a single or multiple entry
c
      if (ICTYP(kinc) .ne. 2 .or. RCSUB(kinc) .ne. 17.) then
          imult  = 0
      else
          kinc   = kinc   + 1
          imult  = 1
      endif
      kst    = kinc
c
c...Get parameter type
c
  200 if (ICTYP(kinc) .eq. 1) then
c
c......Post word
c
          if (RCSUB(kinc) .ge. 4000.) go to 9000
          ICTYP(kinc) = 0
c
c......Number
c......Check for numeric range
c
      else if (ICTYP(kinc) .eq. 3) then
          if (kinc+2 .le. NTOK .and. ICTYP(kinc+1) .eq. 2 .and.
     1        RCSUB(kinc+1) .eq. 5.) then
              if (ICTYP(kinc+2) .ne. 3) go to 9000
              ICTYP(kinc) = 11
              kinc   = kinc   + 2
c
          else
              ICTYP(kinc) = 3
          endif
c
c......Unsupported data type
c
      else
          go to 9100
      endif
c
c......Increment counters
c
      kinc   = kinc   + 1
      kprm   = kprm   + 1
c
c...Single entry
c...Check for delimiting comma
c
      if (imult .eq. 0) then
          if (kinc .le. NTOK .and. ICTYP(kinc) .ne. 2 .and.
     1        RCSUB(kinc) .ne. 2) go to 9200
          kend   = kinc   - 1
          kinc   = kinc   + 1
          go to 8000
c
c...Multiple entry
c...Check for comma or ending parenthesis
c
      else
          if (kinc .gt. NTOK .or. ICTYP(kinc) .ne. 2) go to 9200
          if (RCSUB(kinc) .eq. 2.) then
              kinc   = kinc   + 1
              go to 200
          else if (RCSUB(kinc) .eq. 18.) then
              kend   = kinc   - 1
              kinc   = kinc   + 1
              if (kinc .le. NTOK .and. ICTYP(kinc) .ne. 2 .and.
     1            RCSUB(kinc) .ne. 2) go to 9200
              kinc   = kinc   + 1
              go to 8000
          else
              go to 9200
          endif
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
c...Unsupported data type
c
 9100 call errtxt ('WRDNUMRG',cmsg)
      kerr   = 1
      go to 8000
c
c...Comma expected
c
 9200 call errtxt ('COMAEXP',cmsg)
      kerr   = 1
      go to 8000
      end
