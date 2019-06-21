c
c***********************************************************************
c
c   FILE NAME:  compil
c   CONTAINS:
c               compil  cmpwrd  cmpmac  cmplab  cmpfmt
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        compil.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/08/15 , 11:45:04
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  compil (cmsg,kerr)
c
c   FUNCTION:  This routine is the controlling routine for the 1st pass
c              of compiling a statement after it has been fully parsed.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine compil (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ist,i,mityp(100),micnc(100),ifl,mipt,mtok,inc,
     1          pityp(100),picnc(100),pipt,ptok,ipfl,pary(12)
c
      real*8 rmsub(100),rpsub(100)
c
      character*24 lbuf
c
c...Initialize routine
c
      kerr   = 0
      ifl    = 0
      ipfl   = 0
      pary(10) = 0
      if (NTOK .eq. 0) go to 8000
c
c...Break out all multi-dimensional array references
c
   10 call cmpary (pityp,rpsub,picnc,pary,ptok,ipfl,pipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Text string
c...Check for Real or Text variable
c
   20 if (ICTYP(1) .eq. 4) then
          ist    = RCSUB(1)
          if ((ICNC(1)-ist+1) .gt. 24) go to 9100
          lbuf   = LCTXT(ist:ICNC(1))
          call getscl (lbuf,ICTYP(1),ist,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if (ICTYP(1) .eq. 5 .or. ICTYP(1) .eq. 6) then
              inc    = ICTYP(1) - 2
              RCSUB(1) = MFDAT(ist+7,inc)
              ICNC(1)  = MFDAT(ist+6,inc)
          endif
c
c...Parenthesis Variable
c...Determine if it is a Real or
c...Char equation
c
      else if (ICTYP(1) .eq. 5 .and. RCSUB(1) .le. MAXPRN) then
          do 50 i=2,NTOK,1
              if (ICTYP(i) .eq. 4) go to 55
              if (ICTYP(i) .eq. 6 .or. ICTYP(i) .eq. 7 .or.
     1            ICTYP(i) .eq. 18) go to 60
              if (ICTYP(i) .eq. 1 .and. ((RCSUB(i) .ge. 5016. .and.
     1            RCSUB(i) .le. 5019.) .or. RCSUB(i) .eq. 6071 .or.
     2            RCSUB(i) .eq. 6072 .or. RCSUB(i) .eq. 6080)) go to 60
   50     continue
          go to 80
c
   55     ist    = RCSUB(i)
          if ((ICNC(i)-ist+1) .gt. 24) go to 9100
          lbuf   = LCTXT(ist:ICNC(i))
          call getscl (lbuf,i,ist,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          ICTYP(1) = i
          if (i .ne. 6) go to 70
c
   60     ICTYP(1) = 6
          ist    = RCSUB(1)
          IPNSMP(ist) = 3
      endif
c
c...Remove & compile all multi-argument functions
c
   70 if (ifl .eq. 3) then
          ifl    = 0
      else if (ifl .ne. 1) then
          call cmpfnc (mityp,rmsub,micnc,mtok,ifl,mipt,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Determine type of statement
c
   80 go to (100,9000,9000,9100,200,300,9000,400), ICTYP(1)
c
c...Recognized word
c
  100 call cmpwrd (mityp,rmsub,micnc,mtok,ifl,mipt,cmsg,kerr)
      if (ifl .eq. 1) go to 20
      go to 7000
c
c...Real variable equation
c
  200 call cmpequ (cmsg,kerr)
      go to 7000
c
c...Text variable equation
c
  300 call cmptxt (cmsg,kerr)
      go to 7000
c
c...Format specification
c
  400 call cmpfmt (cmsg,kerr)
      go to 7000
c
c...Check to see if another routine has
c...control of the compiler at this time
c
 7000 if (kerr .ne. 0) go to 8000
      if (ifl .eq. 1) go to 100
      if (ifl .eq. 2) go to 70
      if (ifl .eq. 3) go to 20
      if (ipfl .ne. 0) go to 10
c
c...Generate "FALSE" label for IF
c
      if (IFFLAG .eq. 2) then
          call cmplab (LF2LAB(IFPT),NC2LAB(IFPT),IPC,cmsg,kerr)
          IFFLAG = 0
          IFPT   = IFPT   - 1
      endif
c
c...End of routine
c
 8000 return
c
c...Statement may not begin with this
c
 9000 call errtxt ('NOTBEGIN',cmsg)
      kerr   = 1
      go to 8000
c
c...Undefined Variable
c
 9100 call errtxt ('UNDEFVAR',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpwrd (kctyp,gcsub,kcnc,ktok,kfl,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine is the 1st pass compiling routine for post-
c              processor words.
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
c                            contain an equation to be parsed.  'kfl'
c                            should be set to 0 prior to the 1st call
c                            and should not be altered after that call.
c
c           kpt     I*4  D1  Local pointer within local arrays that
c                            needs to be saved between calls.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine cmpwrd (kctyp,gcsub,kcnc,ktok,kfl,kpt,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kctyp(100),kcnc(100),ktok,kfl,kpt,kerr
c
      real*8 gcsub(100)
c
      character*(*) cmsg
c
c...Reentry for post command
c...with nested equation
c
      if (kfl .eq. 1) go to 300
c
c...Initialize routine
c
      kerr   = 0
c
c...Determine type of command
c
      if (RCSUB(1) .ge. 4000 .and. RCSUB(1) .ne. 4013. .and.
     1    RCSUB(1) .ne. 4017. .and. RCSUB(1) .ne. 4019. .and.
     2    RCSUB(1) .ne. 4020. .and. RCSUB(1) .ne. 4022. .and.
     3    RCSUB(1) .ne. 4026 .and.
     4    RCSUB(1) .ne. 4048. .and. RCSUB(1) .ne. 4049. .and.
     5    (RCSUB(1) .lt. 4053 .or. RCSUB(1) .gt. 4060)) go to 600
c
c...Standard post word
c
      if (NTOK .eq. 1) go to 300
      if (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 7.) go to 9000
      if (NTOK .eq. 2) go to 300
c
c...Macro definition
c
      if (ICTYP(3) .eq. 1 .and. RCSUB(3) .eq. 5006. .and.
     1    RCSUB(3) .ne. 4017.) then
          call cmpmac (cmsg,kerr)
          go to 8000
      endif
c
c...Standard post word
c
  300 call cmppwd (kctyp,gcsub,kcnc,ktok,kfl,kpt,cmsg,kerr)
      go to 8000
c
c...Compiler word
c...Check for Macro definition
c
  600 if (ICTYP(3) .eq. 1 .and. RCSUB(3) .eq. 5006. .and.
     1    (RCSUB(1) .eq. 4013. .or. RCSUB(1) .eq. 4012. .or.
     2     RCSUB(1) .eq. 4022. .or. RCSUB(1) .eq. 4065 .or.
     3     (RCSUB(1) .ge. 4068 .and. RCSUB(1) .le. 4072))) then
          call cmpmac (cmsg,kerr)
      else
          call cmpcmd (kctyp,gcsub,kcnc,ktok,kfl,cmsg,kerr)
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
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpmac (cmsg,kerr)
c
c   FUNCTION:  This routine is the 1st pass compiling routine for MACRO
c              definitions.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine cmpmac (cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ipt,inc,inum,i
c
c...Initialize routine
c
      if (NTOK .ne. 5) go to 9000
      inum   = RCSUB(1)
c
c...Get maximum # of args
c
      if (ICTYP(4) .ne. 2 .or. RCSUB(4) .ne. 2. .or. ICTYP(5) .ne. 3)
     1        go to 9000
      if (RCSUB(5) .lt. 0. .or. RCSUB(5) .gt. 50.) go to 9300
      MAXARG = RCSUB(5)
c
c...Check for Macro definition in effect
c
      if (IMACDF .eq. 1) then
          if (NMACRO .eq. 1) then
              ipt    = NTOK
              NTOK   = 1
              call termac (cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              NTOK   = ipt
          else
              go to 9200
          endif
      endif
c
c......Check for multiple definition
c
      if (MFDAT(1,1) .ne. MFPT(1,1)) then
          call stowrk (MFDAT(1,1),MFDAT(1,1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call lodwrk (MFPT(1,1),MFDAT(1,1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
      inc    = 0
      do 300 i=1,NMACRO,1
          if (inc .eq. MFREC(1)) then
              call lodwrk (MFDAT(2,1),MFDAT(1,1),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              inc    = 0
          endif
c
          inc    = inc    + 1
          ipt    = (inc-1) * MNREC(1) + MNFIX(1) + 1
          if (inum .eq. MFDAT(ipt,1)) go to 9100
  300 continue
c
c......Store Macro name
c
      inc    = inc    + 1
      if (inc .gt. MFREC(1)) then
          MFDAT(2,1) = MFNXT
          call stowrk (MFDAT(1,1),MFDAT(1,1),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          MFDAT(1,1) = MFNXT
          MFDAT(2,1) = 0
          MFNXT  = MFNXT  + 1
          inc    = 1
c          ifl    = 1
      endif
c
      IMACPT = (inc-1) * MNREC(1) + MNFIX(1) + 1
      MFDAT(IMACPT,1) = inum
      MFDAT(IMACPT+1,1) = NLABEL + 1
      MFDAT(IMACPT+2,1) = 0
      MFDAT(IMACPT+3,1) = NSCAL(1) + 1
      MFDAT(IMACPT+4,1) = 0
      MFDAT(IMACPT+5,1) = NSCAL(2) + 1
      MFDAT(IMACPT+6,1) = 0
      MFDAT(IMACPT+7,1) = MFPT(2,5)
      MFDAT(IMACPT+8,1) = MFPT(3,5) + 1
      IMACDF = 1
      NMACRO = NMACRO + 1
      call getvwd (inum,LMACRO,i,1,PSTWRD,PSTWVL,NPSTWD)
c
c...Allocate %ARG & %NARG storage
c
      if (MAXARG .eq. 0) then
c          MFDAT(IMACPT+9,1) = 0
          MFDAT(IMACPT+10,1) = 0
          MFDAT(IMACPT+11,1) = 0
          MFDAT(IMACPT+12,1) = 0
          MFDAT(IMACPT+13,1) = 0
      else
c          MFDAT(IMACPT+9,1) = ISCAST(1)
c          MFDAT(IMACPT+10,1) = ISCAST(1) + 1
          MFDAT(IMACPT+10,1) = ISCAST(1)
          MFDAT(IMACPT+11,1) = MAXARG
c          ISCAST(1) = ISCAST(1) + MAXARG + 1
          MFDAT(IMACPT+12,1) = ISCAST(2)
          MFDAT(IMACPT+13,1) = MAXARG * 24
c          ISCAST(2) = ISCAST(2) + MAXARG * 8
c
c          call stoscl (' ',1,1,cmsg,kerr)
          call stoscl (' ',1,MAXARG,1,cmsg,kerr)
          call stoscl (' ',2,MAXARG*24,1,cmsg,kerr)
      endif
c
c...Initialize various flags/pointers
c
      IFPT   = 0
      IFLAB  = 0
      IDOLPT = 0
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
c...Macro already defined
c
 9100 call errtxt ('MULTMAC',cmsg)
      kerr   = 1
      go to 8000
c
c...Macro currently being defined
c
 9200 call errtxt ('MACDEF',cmsg)
      kerr   = 1
      go to 8000
c
c...Input value out of range
c
 9300 call errtxt ('INPRANG',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmplab (cdat,knc,kpc,cmsg,kerr)
c
c   FUNCTION:  This routine is the 1st pass compiling routine for LABELS
c
c   INPUT:  cdat    C*n  D1  Text of Label.
c
c           knc     I*4  D1  Number of characters in 'cdat'.
c
c           krec    I*4  D1  PC of compiled record for label pointer.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmplab (cdat,knc,kpc,cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kerr,knc,kpc
c
      character*(*) cmsg,cdat
c
      integer*4 i,isca(6),ipt,inum,ierr
c
      real*8 rnum
c
      character*24 lsca
c
      equivalence (isca,lsca)
c
c...Initialize routine
c
      kerr   = 0
c
c...Make sure label is not a number or
c...vocabulary word
c
      call touppr (cdat(1:knc),lsca)
      call ctor (lsca,rnum,ierr)
      if (ierr .eq. 0) go to 9100
      call getvnm (lsca,inum,PSTWRD,PSTWVL,NPSTWD)
      if (inum .ne. 0) go to 9100
c
c...Check for redefinition of label
c
      call getlab (lsca,inum,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (inum .ne. 0) go to 9000
c
c...Store new label
c
      call lodlab (NLABEL+1,ipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      NLABEL = NLABEL + 1
      do 400 i=1,6,1
          MFDAT(i+ipt-1,2) = isca(i)
  400 continue
      MFDAT(ipt+6,2) = kpc
c      MFDAT(ipt+7,2) = kinc
c
c...Check for end of DO loop
c
      do 500 i=1,IFPT,1
          if (lsca .eq. IDOLAB(i)) go to 600
  500 continue
      go to 8000
c
c......End of DO loop
c......Set flag
c
  600 IDOLPT = i
c
c...End of routine
c
 8000 return
c
c...Label already defined
c
 9000 call errtxt ('MULTLAB',cmsg)
      kerr   = 1
      go to 8000
c
c...Label expected
c
 9100 call errtxt ('INVLAB',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpfmt (cmsg,kerr)
c
c   FUNCTION:  This routine processes FORMAT statements.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpfmt (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 inc,ilft,irgt,iparm(3),ist,ie
c
      character*1 lc
c
c...Determine type of FORMAT definition
c
      if (NTOK .lt. 3) go to 9000
      if (ICTYP(2) .ne. 2 .or. RCSUB(2) .ne. 1.) go to 9000
      if (NTOK .eq. 3 .and. ICTYP(3) .eq. 8) go to 1000
c
c...#f = Lm.n,Mm.n,S+-0
c...     T
c...     F
c...     D
c
      inc    = 2
      iparm(1) = 0
      iparm(2) = 0
      iparm(3) = 0
c
      ICMPL(1) = 10
      ICMPL(2) = 11
      ICMPL(3) = RCSUB(1)
      ICMPL(4) = 3
      ICMPL(5) = 3
      ICMPL(6) = 4
      ICMPL(7) = 1
      ICMPL(8) = 0
      ICMPL(9) = 0
      ICMPL(10) = 1
c
c......Get next parameter
c
  100 inc    = inc    + 1
          if (ICTYP(inc) .ne. 4 .and. ICTYP(inc) .ne. 7) go to 9100
          ist    = RCSUB(inc)
          ie     = ICNC(inc)
          call touppr (LCTXT(ist:ist),lc)
c
c.........Floating point format &
c.........Minimum # of digits to output
c
          if (lc .eq. 'L' .or. lc .eq. 'T' .or. lc .eq. 'F' .or.
     1        lc .eq. 'D' .or. lc .eq. 'M') then
              call fmtflt (LCTXT(ist+1:ie),ilft,irgt,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (ilft .lt. 0 .or. irgt .lt. 0) go to 9100
              if (ilft+irgt .gt. 19) go to 9100
              if (lc .eq. 'M') then
                  if (iparm(2) .ne. 0) go to 9200
                  iparm(2) = 1
                  ICMPL(7) = ilft
                  ICMPL(8) = irgt
              else
                  if (iparm(1) .ne. 0) go to 9200
                  iparm(1) = 1
                  ICMPL(4) = 1
                  if (lc .eq. 'T') ICMPL(4) = 2
                  if (lc .eq. 'F') ICMPL(4) = 3
                  if (lc .eq. 'D') ICMPL(4) = 4
                  ICMPL(5) = ilft
                  ICMPL(6) = irgt
              endif
c
c.........Signs (+-*) to include in output
c
          else if (lc .eq. 'S' .and. ist .eq. ie) then
              if (iparm(3) .ne. 0) go to 9200
              iparm(3) = 1
              if (inc .eq. NTOK) go to 9000
              inc    = inc    + 1
              ICMPL(9) = 0
              ICMPL(10) = 0
              if (ICTYP(inc) .eq. 2 .and. RCSUB(inc) .eq. 4.) then
                  ICMPL(9) = 1
                  if (inc .ne. NTOK) then
                      if (ICTYP(inc+1) .eq. 2 .and.
     1                    RCSUB(inc+1) .eq. 5.) then
                          inc    = inc    + 1
                          ICMPL(10) = 1
                      else if (ICTYP(inc+1) .eq. 2 .and.
     1                         RCSUB(inc+1) .eq. 6.) then
                          inc    = inc    + 1
                          ICMPL(10) = 2
                      endif
                  endif
              else if (ICTYP(inc) .eq. 2 .and. (RCSUB(inc) .eq. 5. .or.
     1                 RCSUB(inc) .eq. 6.)) then
                  ICMPL(10) = 1
                  if (RCSUB(inc) .eq. 6.) ICMPL(10) = 2
                  if (inc .ne. NTOK .and. ICTYP(inc+1) .eq. 2 .and.
     1                RCSUB(inc+1) .eq. 4.) then
                      inc    = inc    + 1
                      ICMPL(9) = 1
                  endif
              else
                  go to 9000
              endif
c
c.........Unrecognized parameter
c
          else
              go to 9100
          endif
c
c......This token should be a comma
c
          if (inc .lt. NTOK) then
              inc    = inc    + 1
              if (inc .eq. NTOK) go to 9000
              if (ICTYP(inc) .ne. 2 .or. RCSUB(inc) .ne. 2) go to 9100
              go to 100
          endif
c
c......Write compiled FORMAT record
c
      call cmpwrt (cmsg,kerr)
      go to 8000
c
c...#f = #f
c
 1000 ICMPL(1) = 4
      ICMPL(2) = 11
      ICMPL(3) = RCSUB(1)
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
c...Invalid parameter
c
 9100 call errtxt ('INVPARAM',cmsg)
      kerr   = 1
      go to 8000
c
c...Multiple defined parameter
c
 9200 call errtxt ('MULTPARM',cmsg)
      kerr   = 1
      go to 8000
      end
