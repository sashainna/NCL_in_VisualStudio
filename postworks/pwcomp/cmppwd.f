c
c***********************************************************************
c
c   FILE NAME:  cmppwd
c   CONTAINS:
c               cmppwd  pwdsyn  pstvar  cmptpw
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmppwd.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        08/11/16 , 10:23:01
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmppwd (kctyp,gcsub,kcnc,ktok,kfl,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine compiles a post word command and the DEFINE
c              command after it has been fully parsed.  The common ar-
c              rays for token types & values (ICTYP,RCSUB,ICNC,NTOK)
c              must be passed in as local arrays, because this routine
c              breaks out all equations and returns to the calling rou-
c              tine to compile these.
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
c                            and should not be altered after the call.
c
c           kpt     I*4  D1  Local pointer within local arrays that
c                            needs to be saved between calls.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmppwd (kctyp,gcsub,kcnc,ktok,kfl,kpt,cmsg,kerr)
c
      include 'menu.inc'
c
      include 'compile.inc'
c
      integer*4 kctyp(100),kcnc(100),kfl,kpt,kerr,ktok
c
      real*8 gcsub(100)
c
      character*(*) cmsg
c
      integer*4 i,j,k,ist,ip,nvar,ipt,jpt,irt,inum,ist1,ityp,inc
      integer*4 STOCKV/321/,FIXTURV/898/,LOADV/1075/
      integer*4 STLV/330/,MOVEV/577/
c
c...Initialize routine
c
      kerr   = 0
      if (kfl .eq. 1) then
          kfl    = 0
          go to 200
      endif
c
c...Standalone major word
c
      if ((NTOK .eq. 1 .or. NTOK .eq. 2) .and. RCSUB(1) .ne. 4055) then
          ICMPL(1) = 4
          ICMPL(2) = 3
          ICMPL(3) = RCSUB(1)
          ICMPL(4) = 0
          call cmpwrt (cmsg,kerr)
          go to 8000
      endif
c
c...Save current parameter types
c
      do 100 i=1,NTOK,1
          kctyp(i) = ICTYP(i)
          gcsub(i) = RCSUB(i)
          kcnc(i) = ICNC(i)
  100 continue
      ktok   = NTOK
c
c...Separate equations from command
c
      kpt    = 2
      ist    = 3
  200 kpt    = kpt    + 1
      if (kpt .gt. ktok) go to 400
c
c......Operator
c
      if (kctyp(kpt) .eq. 2) then
c
c.........Comma
c
          if (gcsub(kpt) .eq. 2.) then
              if (kfl .eq. 1) go to 300
              ist    = kpt    + 1
c
c.........Math Operator
c
          else
              kfl    = 1
          endif
c
c......Recognized word
c......Check for Function
c
      else if (kctyp(kpt) .eq. 1) then
          if ((gcsub(kpt) .ge. 5001. .and. gcsub(kpt) .le. 5004.) .or.
     1        (gcsub(kpt) .ge. 5007. .and. gcsub(kpt) .le. 5020.) .or.
     2         gcsub(kpt) .eq. 5029.)
     2            kfl = 1
      endif
c
      go to 200
c
c...This variable is an equation
c...Set up the type arrays and
c...send it back to the 'compil' routine
c
  300 if (NPNVAR .eq. MAXPRN) go to 9000
      ICTYP(1) = 5
      NPNVAR = NPNVAR + 1
      RCSUB(1) = NPNVAR
      IPNSMP(NPNVAR) = 0
      ICTYP(2) = 2
      RCSUB(2) = 1.
      NTOK   = 2
      do 330 i=ist,kpt-1,1
          NTOK   = NTOK   + 1
          ICTYP(NTOK) = kctyp(i)
          RCSUB(NTOK) = gcsub(i)
          ICNC(NTOK) = kcnc(i)
  330 continue
c
c......Remove this equation from
c......the original command
c
      kctyp(ist) = 5
      gcsub(ist) = NPNVAR
      if (kpt .le. ktok) then
          kctyp(ist+1) = 2
          gcsub(ist+1) = 2.
          ip     = ist    + 1
          do 350 i=kpt+1,ktok,1
              ip     = ip     + 1
              kctyp(ip) = kctyp(i)
              gcsub(ip) = gcsub(i)
              kcnc(ip) = kcnc(i)
  350     continue
          ktok   = ip
          kpt    = ist
      else
          ktok   = ist
          kpt    = ist
      endif
      go to 8000
c
c...End of command
c...Check for equation &
c...then do normal compiling of command
c
  400 if (kfl .eq. 1) go to 300
c
c......Move local storage back to global arrays
c
      do 500 i=1,ktok,1
          ICTYP(i) = kctyp(i)
          RCSUB(i) = gcsub(i)
          ICNC(i) = kcnc(i)
  500 continue
      NTOK   = ktok
c
c......Check syntax
c
      ityp   = 1
      if (RCSUB(1) .eq. 4017.) ityp = 2
      if (RCSUB(1) .ge. 4053 .and. RCSUB(1) .le. 4055) ityp = 3
      if (RCSUB(1) .ge. 4056 .and. RCSUB(1) .le. 4060) ityp = 4
      call pwdsyn (ityp,nvar,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (RCSUB(1) .eq. 4053 .and. nvar .ne. 2 .and. nvar .ne. 3)
     1        go to 9200
      if (RCSUB(1) .eq. 4054 .and. (nvar .lt. 3 .or. nvar .gt. 23))
     1        go to 9200
      if (RCSUB(1) .eq. 4055 .and. nvar .ne. 0 .and. nvar .ne. 1)
     1        go to 9200
      if (RCSUB(1) .eq. 4056 .and. nvar .ne. 3 .and. nvar .ne. 4)
     1        go to 9200
      if (RCSUB(1) .eq. 4057 .and. nvar .ne. 1) go to 9200
      if (RCSUB(1) .eq. 4058 .and. nvar .ne. 1) go to 9200
      if (RCSUB(1) .eq. 4059 .and. nvar .ne. 3 .and. nvar .ne. 4)
     1        go to 9200
      if (RCSUB(1) .eq. 4060 .and. nvar .ne. 2 .and. nvar .ne. 3)
     1        go to 9200
c
c......Initialize compilation buffer
c
      ipt    = 4 + ((nvar-1)/4 + 1) * 4 + 1
      jpt    = (ipt-1) / 2 + 1
      irt    = (ipt-1) / 4 + 1
      ist    = 5
      ICMPL(1) = (ipt-1) + nvar*4
      if (ityp .eq. 1) then
          ICMPL(2) = 3
          ICMPL(3) = RCSUB(1)
          ICMPL(4) = nvar
      else if (ityp .eq. 2) then
          ICMPL(2) = 12
          ICMPL(3) = nvar
      else if (ityp .eq. 3) then
          ICMPL(2) = 13
          ICMPL(3) = RCSUB(1)
          ICMPL(4) = nvar
      else if (ityp .eq. 4) then
          ICMPL(2) = 14
          ICMPL(3) = RCSUB(1)
          ICMPL(4) = nvar
      endif
c
c......Store minor words/values
c
      i     = 1
  600 i     = i     + 2
          if (jpt .gt. 100) go to 9300
          ip     = i
c
c.........Determine output variable type
c.........from input variable type
c
          if (ICTYP(i) .eq. 1) then
              if (ityp .eq. 4) go to 9200
              ICTYP(i) = 0
          else if (ICTYP(i) .eq. 5 .or. ICTYP(i) .eq. 12 .or.
     1             ICTYP(i) .eq. 13 .or. ICTYP(i) .eq. 14 .or.
     2             ICTYP(i) .eq. 15 .or. ICTYP(i) .eq. 18) then
              call vartyp (ip,ICTYP,RCSUB,ICNC,NTOK,0,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          else if (ICTYP(i) .eq. 6) then
              call textyp (ip,ICTYP,RCSUB,ist1,ICNC,NTOK,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          else if (ICTYP(i) .eq. 7) then
              ICTYP(i) = 6
          else
              if (ICTYP(i) .ne. 3) go to 9100
          endif
c
          ICMPL(ist) = ICTYP(i)
c
c.........Vocabulary word
c
          if (ICTYP(i) .eq. 0) then
              JCMPL(jpt) = RCSUB(i)
c
c.........Real or Post variable
c
          else if (ICTYP(i) .eq. 1 .or. ICTYP(i) .eq. 13) then
              JCMPL(jpt) = RCSUB(i)
              i      = ip
c
c.........Subscripted Real or Post variable
c
          else if (ICTYP(i) .eq. 2 .or. ICTYP(i) .eq. 12 .or.
     1             ICTYP(i) .eq. 14 .or. ICTYP(i) .eq. 15 .or.
     2             ICTYP(i) .eq. 18) then
              JCMPL(jpt) = RCSUB(i)
              i      = i      + 1
              JCMPL(jpt+1) = RCSUB(i)
c
c.........Real number
c
          else if (ICTYP(i) .eq. 3) then
              RCMPL(irt) = RCSUB(i)
c
c.........Text variable
c
          else if (ICTYP(i) .eq. 4) then
              JCMPL(jpt) = RCSUB(i)
              ICMPL(ipt+2) = ist1
              ICMPL(ipt+3) = ICNC(i)
              i      = ip
c
c.........Single subscript Text variable
c
          else if (ICTYP(i) .eq. 5) then
              JCMPL(jpt) = RCSUB(i)
              i      = i      + 1
              ICMPL(ipt+2) = RCSUB(i)
c
c.........Text string
c
          else if (ICTYP(i) .eq. 6) then
              ip     = RCSUB(i)
c
c............This is a Text only post command
c............Store the text data
c
              if ((nvar .eq. 1 .and. ((RCSUB(1) .ge. 1043. .and.
     1            RCSUB(1) .le. 1046.)  .or. RCSUB(1) .eq. 1102. .or.
     2            RCSUB(1) .eq. 1103.)) .or.
     3            ((RCSUB(1).eq.STOCKV.or.RCSUB(1).eq.FIXTURV) .and.
     4            (i.eq.5  .and. RCSUB(3).eq.LOADV .or.
     5             i.eq.7  .and. RCSUB(3).eq.STLV .or.
     6             i.eq.29 .and. RCSUB(3).eq.MOVEV)) .or.
     7            (ityp .eq. 4) .or.
     8            (RCSUB(1) .eq. 4017. .and. i .gt. 5 .and.
     9             ICTYP(i-3) .eq. 18)) then
                  j = ICNC(i) - ip
                  ICMPL(ipt) = j+1
                  k = ipt*2+1
                  LCMPL(k:k+j) = LCTXT(ip:ICNC(i))
                  if (j+1 .gt. 6) then
                      inc    = ((j+1-6+7)/8) * 4
                      ICMPL(1) = ICMPL(1) + inc
                      ipt    = ipt    + inc
                      jpt    = jpt    + inc/2
                      irt    = irt    + inc/4
                  endif

c
c............Standard post command
c............Make sure Text string is a vocabulary word
c
              else
                  call getvnm (LCTXT(ip:ICNC(i)),inum,PSTWRD,PSTWVL,
     1                         NPSTWD)
                  if (inum .eq. 0) go to 9100
                  ICMPL(ist) = 0
                  JCMPL(jpt) = inum
              endif
c
c.........Unrecognized variable type
c
          else
              go to 9100
          endif
c
c.........Increment pointers
c
          ist    = ist    + 1
          ipt    = ipt    + 4
          jpt    = jpt    + 2
          irt    = irt    + 1
      if (i .lt. NTOK) go to 600
c
c......Write compiled record
c
      call cmpwrt (cmsg,kerr)
c
c...End of routine
c
 8000 return
c
c...Equation is too complex
c
 9000 call errtxt ('COMPLXEQ',cmsg)
      kerr   = 1
      go to 8000
c
c...Variable, voc word or number expected
c
 9100 call errtxt ('VARVOCNM',cmsg)
      kerr   = 1
      go to 8000
c
c....Invalid syntax
c
 9200 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8000
c
c....Too many post words
c
 9300 call errtxt ('MANYPRMS',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pwdsyn (ktyp,kvar,cmsg,kerr)
c
c   FUNCTION:  This routine checks the syntax of a fully parsed post-
c              processor command.
c
c   INPUT:  ktyp    I*4  D1  Defines the type of command we are checking
c                            for syntax.  1 = Standard post command.
c                            2 = DEFINE command, 3 = CL--- command,
c                            4 = File access command (FOPEN, FREAD, etc.).
c
c   OUTPUT: kvar    I*4  D1  Returns the number of minor word/values in
c                            the command.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine pwdsyn (ktyp,kvar,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 ktyp,kvar,kerr
c
      character*(*) cmsg
c
      integer*4 inc,ist,isw
c
      real*8 rbuf
c
      character*24 lbuf
      equivalence (rbuf,lbuf)
c
c...Initialize routine
c
      kerr   = 0
      kvar   = 0
      inc    = 2
      isw    = 1
      if (inc .ge. NTOK) then
          if (ktyp .eq. 2 .or. ktyp .eq. 4) go to 9100
          go to 8000
      endif
c
c...This token should be a number,
c...text, variable, or a vocabulary word
c
  200 if (inc .eq. NTOK) go to 9100
          inc    = inc    + 1
c
c......Parenthesis variable
c......Change it to text if necessary
c
          ist    = RCSUB(inc)
          if (ICTYP(inc) .eq. 5) then
              if (ktyp .eq. 2 .and. isw .eq. 1) go to 9000
              if (ktyp .eq. 3) go to 9300
              if (IPNSMP(ist) .eq. 3 .or. IPNSMP(ist) .eq. 4)
     1                ICTYP(inc) = 6
          endif
c
c......Text string
c......See if it's actually a Variable
c
          if (ICTYP(inc) .eq. 4) then
              if (ktyp .eq. 2 .and. isw .eq. 1) go to 9000
              ist    = RCSUB(inc)
              if ((ICNC(inc)-ist+1) .gt. 24) go to 9100
              lbuf   = LCTXT(ist:ICNC(inc))
              call getscl (lbuf,ICTYP(inc),ist,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (ICTYP(inc) .eq. 5) then
                  RCSUB(inc) = MFDAT(ist+7,3)
                  ICNC(inc) = MFDAT(ist+6,3)
              else if (ICTYP(inc) .eq. 6) then
                  if (ktyp .eq. 3 .and.
     1                (RCSUB(1) .ne. 4054 .or. kvar .lt. 2)) go to 9300
                  RCSUB(inc) = MFDAT(ist+7,4)
                  ICNC(inc) = MFDAT(ist+6,4)
              else if (ICTYP(inc) .eq. 4 .and. RCSUB(1) .eq. 1015.) then
                  if (ICNC(inc)-ist+1 .gt. 6) go to 9100
                  ICTYP(inc) = 3
                  RCSUB(inc) = rbuf
              else if (ICTYP(inc) .eq. 8 .and. RCSUB(1) .eq. 1106.) then
                  ICTYP(inc) = 3
              else
                  go to 9100
              endif
c
c.........Check for subscript
c
              if (NTOK .ne. inc .and. ICTYP(inc+1) .eq. 5) then
                  inc    = inc    + 1
                  if (RCSUB(inc) .gt. NPNVAR) go to 9000
              endif
              kvar   = kvar   + 1
c
c......Vocabulary word
c......Check for Post Variable
c
          else if (ICTYP(inc) .eq. 1) then
              if (RCSUB(inc) .ge. 6000.) then
                  call pstvar (inc,ICTYP,RCSUB,NTOK,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              else if ((ktyp .eq. 2 .and. isw .eq. 1) .or.
     1                  ktyp .eq. 4) then
                  go to 9000
              else if (ktyp .eq. 3) then
                  if (RCSUB(1) .ne. 4054 .or. kvar .lt. 2) go to 9300
              endif
              kvar   = kvar   + 1
c
c......Real number or variable or
c......Text variable
c
          else if (ICTYP(inc) .eq. 3 .or. ICTYP(inc) .eq. 5 .or.
     1             ICTYP(inc) .eq. 6 .or. ICTYP(inc) .eq. 7) then
              if (ktyp .eq. 2 .and. isw .eq. 1) go to 9000
              if (ktyp .eq. 3 .and. ICTYP(inc) .ne. 5 .and.
     1            (RCSUB(1) .ne. 4054 .or. kvar .lt. 2)) go to 9300
              kvar   = kvar   + 1
c
c......Register descriptor
c......w/ FORCE and REGORD commands
c
          else if (ICTYP(inc) .eq. 8 .and.
     1             (RCSUB(1) .eq. 1106. .or. RCSUB(1) .eq. 1110.)) then
              ICTYP(inc) = 3
              kvar   = kvar   + 1
c
c......Invalid parameter
c
          else
              go to 9100
          endif
c
c......This token should be a comma
c
          if (inc .eq. NTOK) then
              if (ktyp .eq. 2 .and. isw .ne. 2) go to 9100
              go to 8000
          endif
          inc    = inc    + 1
          if (ICTYP(inc) .ne. 2 .or. RCSUB(inc) .ne. 2.) go to 9200
          isw    = 3 - isw
      go to 200
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
c...Variable, voc word or number expected
c
 9100 call errtxt ('VARVOCNM',cmsg)
      kerr   = 1
      go to 8000
c
c...Comma expected
c
 9200 call errtxt ('COMAEXP',cmsg)
      kerr   = 1
      go to 8000
c
c...Real variable expected
c
 9300 call errtxt ('VAREXP',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pstvar (kinc,kctyp,gcsub,ktok,cmsg,kerr)
c
c   FUNCTION:  This routine takes a Post-processor variable (%ARG,%CLPT,
c              etc.) and returns the actual type of variable.  The pass-
c              ed array arguments will be changed to reflect the variab-
c              le type.  12 = Macro argument (%ARG).  13 = Post variable.
c              14 = Subscripted Post variable. 15 = Clfile data (%CLDATA)
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
c           ktok    I*4  D1  Number of values in arrays (NTOK).
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine pstvar (kinc,kctyp,gcsub,ktok,cmsg,kerr)
c
      include 'menu.inc'
      include 'compile.inc'
c
      integer*4 kinc,kctyp(100),ktok,kerr
c
      real*8 gcsub(100)
c
      character*(*) cmsg
c
      integer*4 ipt,isub,maxsub(80)
c
      data maxsub /1,1,1,1,1,1,240,1,6,1,6000,6000,7,12,12,12,12,6,6,6,
     1             6,4,4,4,4,10,10,10,10,4,1,1,1,1,6,3,120,120,120,50,
     2             2,1,1,1,1,12,6,4,10,1,1,1,1,1,1,MAXFMT,3,1,10,MAXFMT,
     3             1,1,1,MAXCLD,1,1,1,8,25,1,92,92,2,18,18,18,18,18,18,
     4             1/
c
c...Initialize routine
c
      kerr    = 0
c
c...%ARG or %CLDATA Variable
c...Make sure it has a subscript
c
      if (gcsub(kinc) .eq. 6000. .or. gcsub(kinc) .eq. 6064.) then
          if (kinc .lt. ktok .and. kctyp(kinc+1) .eq. 5) then
              ipt    = gcsub(kinc+1)
              if (IPNSMP(ipt) .ne. 0 .and. IPNSMP(ipt) .ne. 1 .and.
     1            IPNSMP(ipt) .ne. 2) go to 9000
              kctyp(kinc) = 12
              if (gcsub(kinc) .eq. 6064.) kctyp(kinc) = 15
              gcsub(kinc) = gcsub(kinc) - 6000.
              kinc   = kinc   + 1
              if (IPNSMP(ipt) .eq. 0 .or. IPNSMP(ipt) .eq. 2) then
                  gcsub(kinc) = gcsub(kinc) * (-1.)
              else
                  if (PRNVAR(ipt) .le. 0 .or.
     1              (kctyp(kinc) .eq. 12 .and. PRNVAR(ipt) .gt. MAXARG)
     2               .or.
     3              (kctyp(kinc) .eq. 15 .and. PRNVAR(ipt) .gt. MAXCLD))
     4                    go to 9100
                  gcsub(kinc) = PRNVAR(ipt)
              endif
          else
              go to 9000
          endif
c
c...Subscripted Post Variable
c
      else if (kinc .lt. ktok .and. kctyp(kinc+1) .eq. 5) then
c
          isub  = gcsub(kinc) - 6000
          if (isub .eq. 0 .or. maxsub(isub) .eq. 1) go to 9000
c
          ipt    = gcsub(kinc+1)
          if (IPNSMP(ipt) .ne. 0 .and. IPNSMP(ipt) .ne. 1 .and.
     1        IPNSMP(ipt) .ne. 2) go to 9000
          kctyp(kinc) = 14
          if (gcsub(kinc) .eq. 6071 .or. gcsub(kinc) .eq. 6072)
     1        kctyp(kinc) = 18
          gcsub(kinc) = isub
          kinc   = kinc   + 1
          if (IPNSMP(ipt) .eq. 0 .or. IPNSMP(ipt) .eq. 2) then
              gcsub(kinc) = gcsub(kinc) * (-1.)
          else
              if (PRNVAR(ipt) .le. 0 .or.
     1            PRNVAR(ipt) .gt. maxsub(isub)) go to 9100
              gcsub(kinc) = PRNVAR(ipt)
          endif
c
c...Post variable
c
      else
          isub  = gcsub(kinc) - 6000
          if (isub .eq. 0 .or. maxsub(isub) .ne. 1) go to 9000
          kctyp(kinc) = 13
          if (gcsub(kinc) .eq. 6080) kctyp(kinc) = 18
          gcsub(kinc) = isub
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
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmptpw (cbuf,knc,cmsg,kerr)
c
c   FUNCTION:  This routine compiles a Text Only command (LETTER PPRINT
c              PARTNO INSERT) string.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to parse.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmptpw (cbuf,knc,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 knc,kerr
c
      character*(*) cbuf,cmsg
c
c...Write out compiled record
c
      if (ITXCMD .ne. 4009) then
          ICMPL(2) = 3
          ICMPL(3) = ITXCMD
          ICMPL(4) = 1
          ICMPL(5) = 6
          if (knc .lt. 7) then
             ICMPL(9) = 0
          else
             ICMPL(9) = knc    - 6
             LCMPL(19:19+ICMPL(9)-1) = cbuf(7:knc)
          end if
          ICMPL(1) = 9 + (ICMPL(9)-1) / 2 + 1
          call cmpwrt (cmsg,kerr)
c
c...REMARK statement
c...Write out doc record
c
      else
          call docout (cbuf(7:knc),knc-6,cmsg,kerr)
      endif
c
c...End of routine
c
 8000 return
      end
