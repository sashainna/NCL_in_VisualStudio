c
c***********************************************************************
c
c   FILE NAME:  cmpfnc
c   CONTAINS:
c               cmpfnc  fncsyn  cmpary
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmpfnc.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/29/13 , 15:43:50
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmpfnc (kctyp,gcsub,kcnc,ktok,kfl,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine removes and compiles all multi-argument
c              functions from any type of command after it has been ful-
c              ly parsed.  The common arrays for token types & values
c              (ICTYP,RCSUB,ICNC,NTOK) must be passed in as local ar-
c              rays, because this routine breaks out all equations and
c              returns to the calling routine to compile these.
c
c   INPUT:  kctyp   I*4  Dn  Local storage of the ICTYP common array.
c
c           gcsub   R*8  Dn  Local storage of the RCSUB common array.
c
c           kcnc    I*4  Dn  Local storage of the ICNC common array.
c
c           ktok    I*4  D1  Local NTOK variable.
c
c   OUTPUT: kfl     I*4  D1  Returns 2 when the common token arrays
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
      subroutine cmpfnc (kctyp,gcsub,kcnc,ktok,kfl,kpt,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kctyp(100),kcnc(100),kfl,kpt,kerr,ktok
c
      real*8 gcsub(100)
c
      character*(*) cmsg
c
      integer*4 i,ist,ip,nvar,ipt,jpt,irt,iend,ips,ist1,ncomma,is,ie
c
c...Initialize routine
c
      kerr   = 0
      if (kfl .eq. 2) then
          kfl    = 0
          go to 220
      endif
      if (NTOK .eq. 0) go to 8000
c
c...Save current parameter types
c
      do 100 i=1,NTOK,1
          kctyp(i) = ICTYP(i)
          gcsub(i) = RCSUB(i)
          kcnc(i) = ICNC(i)
  100 continue
      ktok   = NTOK
      kpt    = ktok
c
c...Search for multiple parameter functions
c...INDEX  RINDEX  CTOR  RTOC  LOCATE
c
  200 kpt    = kpt    - 1
      if (kpt .eq. 0) go to 1000
c
c......Found function
c
  220 if (kctyp(kpt) .eq. 1 .and. ((gcsub(kpt) .ge. 5012. .and.
     1    gcsub(kpt) .le. 5015.) .or. gcsub(kpt) .eq. 5020. .or.
     2    gcsub(kpt) .eq. 5029.)) then
          ncomma = 0
          ipt    = kpt    + 1
          if (kctyp(ipt) .ne. 2 .or. gcsub(ipt) .ne. 17.) go to 9000
          ist    = ipt    + 1
  250     ipt    = ipt    + 1
c
c.........Operator
c
          if (kctyp(ipt) .eq. 2) then
c
c............End parenthesis
c
              if (gcsub(ipt) .eq. 18.) then
                  if (ncomma .ne. 1) go to 9000
                  iend    = ipt
                  go to 400
c
c............Comma
c
              else if (gcsub(ipt) .eq. 2.) then
                  if (kfl .ne. 0) go to 300
                  ist    = ipt    + 1
                  ncomma = ncomma + 1
c
c............Math Operator
c
              else
                  kfl    = 2
              endif
c
c.........Recognized word
c.........Check for Function
c
          else if (kctyp(ipt) .eq. 1) then
              if ((gcsub(ipt) .ge. 5001. .and. gcsub(ipt) .le. 5004.)
     1            .or. (gcsub(ipt) .ge. 5007. .and.
     2            gcsub(ipt) .le. 5011.) .or. (gcsub(ipt) .ge. 5016.
     3            .and. gcsub(ipt) .le. 5020.) .or.
     4            gcsub(ipt) .eq. 5050) kfl = 2
          endif
c
          go to 250
      endif
      go to 200
c
c...This variable is an equation
c...Set up the type arrays and
c...send it back to the 'compil' routine
c
  300 if (NPNVAR .eq. MAXPRN) go to 9000
      NPNVAR = NPNVAR + 1
      if (gcsub(kpt) .eq. 5015. .or. gcsub(kpt) .eq. 5029.) then
          ICTYP(1) = 5
          IPNSMP(NPNVAR) = 0
      else
          ICTYP(1) = 6
          IPNSMP(NPNVAR) = 3
      endif
      RCSUB(1) = NPNVAR
      ICTYP(2) = 2
      RCSUB(2) = 1.
      NTOK   = 2
      do 330 i=ist,ipt-1,1
          NTOK   = NTOK   + 1
          ICTYP(NTOK) = kctyp(i)
          RCSUB(NTOK) = gcsub(i)
          ICNC(NTOK) = kcnc(i)
  330 continue
c
c......Remove this equation from
c......the original command
c
      kctyp(ist) = ICTYP(1)
      gcsub(ist) = NPNVAR
      kctyp(ist+1) = 2
      gcsub(ist+1) = 2.
      ip     = ist    + 1
      do 350 i=ipt+1,ktok,1
          ip     = ip     + 1
          kctyp(ip) = kctyp(i)
          gcsub(ip) = gcsub(i)
          kcnc(ip) = kcnc(i)
  350 continue
      ktok   = ip
      kpt    = ist
      go to 8000
c
c...End of command
c...Check for equation &
c...then do normal compiling of command
c
  400 if (kfl .ne. 0) go to 300
c
c......Check syntax
c
      call fncsyn (kctyp,gcsub,kcnc,kpt,nvar,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c......Initialize compilation buffer
c
      if (NPNVAR .ge. MAXPRN) go to 9000
      NPNVAR = NPNVAR + 1
      IPNSMP(NPNVAR) = 0
      ipt    = 8 + ((nvar-1)/4 + 1) * 4 + 1
      jpt    = (ipt-1) / 2 + 1
      irt    = (ipt-1) / 4 + 1
      ist    = 9
      ICMPL(1) = (ipt-1)
      ICMPL(3) = gcsub(kpt)
      ICMPL(4) = nvar
c
      if (gcsub(kpt) .eq. 31. .or. gcsub(kpt) .eq. 45.) then
          ICMPL(2) = 8
          ICMPL(5) = 4
      else
          ICMPL(2) = 7
          ICMPL(5) = 1
      endif
      ICMPL(6) = NPNVAR
c
c......Store Function's arguments
c
      i      = kpt    + 2
  600 ips    = i
          if (kctyp(i) .eq. 5 .or. kctyp(i) .eq. 12 .or.
     1        kctyp(i) .eq. 13 .or. kctyp(i) .eq. 14 .or.
     2        kctyp(i) .eq. 15 .or. kctyp(i) .eq. 18) then
              call vartyp (ips,kctyp,gcsub,kcnc,ktok,0,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          else if (kctyp(i) .eq. 6) then
              call textyp (ips,kctyp,gcsub,ist1,kcnc,ktok,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          else if (kctyp(i) .eq. 7) then
              kctyp(i) = 6
          else if (kctyp(i) .eq. 8) then
              kctyp(i) = 7
          else
              if (kctyp(i) .ne. 3) go to 9100
          endif
c
          ICMPL(ist) = kctyp(i)
          if (kctyp(i) .eq. 1 .or. kctyp(i) .eq. 13) then
              JCMPL(jpt) = gcsub(i)
              i      = ips
          else if (kctyp(i) .eq. 2 .or. kctyp(i) .eq. 12 .or.
     1             kctyp(i) .eq. 14 .or. kctyp(i) .eq. 15 .or.
     2             kctyp(i) .eq. 18) then
              JCMPL(jpt) = gcsub(i)
              i      = i      + 1
              JCMPL(jpt+1) = gcsub(i)
          else if (kctyp(i) .eq. 3) then
              RCMPL(irt) = gcsub(i)
          else if (kctyp(i) .eq. 4) then
              JCMPL(jpt) = gcsub(i)
              ICMPL(ipt+2) = ist1
              ICMPL(ipt+3) = kcnc(i)
              i      = ips
          else if (kctyp(i) .eq. 5) then
              JCMPL(jpt) = gcsub(i)
              i      = i      + 1
              ICMPL(ipt+2) = gcsub(i)
          else if (kctyp(i) .eq. 6) then
              ICMPL(ipt) = kcnc(i) - gcsub(i) + 1
              if (ICMPL(ipt) .le. 0) then
                  ICMPL(ipt) = 0
                  ipt    = ipt    + 4
                  ICMPL(1) = ICMPL(1) + 1
              else
                  ip     = gcsub(i)
                  is     = ipt    * 2 + 1
                  ie     = is     + ICMPL(ipt)
                  LCMPL(is:ie) = LCTXT(ip:kcnc(i))
c
                  ip     = (ICMPL(ipt)-1) / 2 + 2
                  ip     = (ip/4 + 1) * 4
                  ICMPL(1) = ICMPL(1) + ip
                  ipt    = ipt    + ip
                  jpt    = jpt    + ip/2
                  irt    = irt    + ip/4
              endif
          else if (kctyp(i) .eq. 7) then
              JCMPL(jpt) = gcsub(i)
          else
              go to 9100
          endif
c
c.........Increment pointers
c
          ist    = ist    + 1
          if (kctyp(ips) .ne. 6) then
              ICMPL(1) = ICMPL(1) + 4
              ipt    = ipt    + 4
              jpt    = jpt    + 2
              irt    = irt    + 1
          endif
          i     = i     + 2
      if (i .lt. iend) go to 600
c
c......Remove this function from
c......the original command
c
      if (gcsub(kpt) .eq. 31. .or. gcsub(kpt) .eq. 45.) then
          kctyp(kpt) = 6
          IPNSMP(NPNVAR) = 3
      else
          kctyp(kpt) = 5
          IPNSMP(NPNVAR) = 0
      endif
      gcsub(kpt) = NPNVAR
      ip     = kpt
      do 900 i=iend+1,ktok,1
          ip     = ip     + 1
          kctyp(ip) = kctyp(i)
          gcsub(ip) = gcsub(i)
          kcnc(ip) = kcnc(i)
  900 continue
      ktok   = ip
c
c......Write compiled record
c
      call cmpwrt (cmsg,kerr)
      go to 200
c
c...No more Multi-parameter Functions
c...Move local storage back to global arrays &
c...Return control to main compiler routine
c
 1000 do 1100 i=1,ktok,1
          ICTYP(i) = kctyp(i)
          RCSUB(i) = gcsub(i)
          ICNC(i) = kcnc(i)
 1100 continue
      NTOK   = ktok
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
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fncsyn (kctyp,gcsub,kcnc,kpt,kvar,cmsg,kerr)
c
c   FUNCTION:  This routine checks the syntax of a fully parsed multi-
c              parameter Function.
c
c   INPUT:  kctyp   I*4  Dn  Local storage of the ICTYP common array.
c
c           gcsub   R*8  Dn  Local storage of the RCSUB common array.
c
c           kcnc    I*4  Dn  Local storage of the ICNC common array.
c
c           kpt     I*4  D1  Pointer to beginning of Function statment
c                            within local arrays.
c
c   OUTPUT: kvar    I*4  D1  Returns number of arguments in Function.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine fncsyn (kctyp,gcsub,kcnc,kpt,kvar,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kctyp(100),kcnc(100),kpt,kerr,kvar
c
      real*8 gcsub(100)
c
      character*(*) cmsg
c
      integer*4 inc,ist,ipt,itok
c
      character*24 lbuf
c
c...Initialize routine
c
      kerr   = 0
      inc    = kpt    + 1
      kvar   = 0
c
c...Get token type
c
  100 inc    = inc    + 1
          ipt    = inc
c
c...Parenthesis variable
c...Change to text if needed
c
          ist    = gcsub(inc)
          if (kctyp(inc) .eq. 5) then
              if (IPNSMP(ist) .eq. 3 .or. IPNSMP(ist) .eq. 4)
     1                kctyp(inc) = 6
          endif
c
c......Text string
c......See if it's actually a Variable
c
          if (kctyp(inc) .eq. 4) then
              ist    = gcsub(inc)
              if ((kcnc(inc)-ist+1) .gt. 24) go to 9100
              lbuf   = LCTXT(ist:kcnc(inc))
              call getscl (lbuf,kctyp(inc),ist,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (kctyp(inc) .eq. 5) then
                  if (gcsub(kpt) .ne. 5014. .and.
     1                gcsub(kpt) .ne. 5015. .and.
     2                gcsub(kpt) .ne. 5020. .and.
     3                gcsub(kpt) .ne. 5029.) go to 9300
                  gcsub(inc) = MFDAT(ist+7,3)
                  kcnc(inc) = MFDAT(ist+6,3)
              else if (kctyp(inc) .eq. 6) then
                  if (gcsub(kpt) .eq. 5015. .or. gcsub(kpt) .eq. 5020.
     1                .or. gcsub(kpt) .eq. 5029.) go to 9200
                  gcsub(inc) = MFDAT(ist+7,4)
                  kcnc(inc) = MFDAT(ist+6,4)
              else
                  go to 9100
              endif
c
c.........Check for subscript
c
              if (kctyp(inc+1) .eq. 5) then
                  inc    = inc    + 1
                  if (gcsub(inc) .gt. NPNVAR) go to 9000
              endif
              kvar   = kvar   + 1
c
c......Format number
c
          else if (kctyp(inc) .eq. 8) then
              kvar   = kvar   + 1
c
c......Post Variable
c
          else if (kctyp(inc) .eq. 1 .and. gcsub(inc) .ge. 6000. .and.
     1             gcsub(inc) .lt. 6500.) then
              itok   = inc    + 1
              call pstvar (inc,kctyp,gcsub,itok,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              kvar   = kvar   + 1
c
c......Real number or variable or
c......Text variable
c
          else if (kctyp(inc) .eq. 3 .or. kctyp(inc) .eq. 5 .or.
     1             kctyp(inc) .eq. 6 .or. kctyp(inc) .eq. 7) then
              kvar   = kvar   + 1
c
c......Invalid parameter
c
          else
              go to 9100
          endif
c
c......Make sure this is a valid parameter
c......for this Function
c......INDEX, RINDEX
c
          if (gcsub(kpt) .eq. 5012. .or. gcsub(kpt) .eq. 5013.) then
              if (kvar .gt. 2) go to 9000
              if (kctyp(ipt) .ne. 6 .and. kctyp(ipt) .ne. 7 .and.
     1            kctyp(ipt) .ne. 12 .and. kctyp(ipt) .ne. 15 .or.
     2            kctyp(ipt) .eq. 18) go to 9300
c
c......CTOR
c
          else if (gcsub(kpt) .eq. 5014.) then
              if (kvar .eq. 1) then
                  if (kctyp(ipt) .ne. 6 .and. kctyp(ipt) .ne. 7 .and.
     1                kctyp(ipt) .ne. 12 .and. kctyp(ipt) .ne. 15 .and.
     2                kctyp(ipt) .ne. 18) go to 9300
              else if (kvar .eq. 2) then
                  if (kctyp(ipt) .ne. 8 .and. kctyp(ipt) .ne. 3 .and.
     1                kctyp(ipt) .ne. 5 .and. kctyp(ipt) .ne. 12 .and.
     2                kctyp(ipt) .ne. 13 .and. kctyp(ipt) .ne. 14 .and.
     3                kctyp(ipt) .ne. 15) go to 9400
              else
                  go to 9000
              endif
c
c......RTOC
c
          else if (gcsub(kpt) .eq. 5015.) then
              if (kvar .eq. 1) then
                  if (kctyp(ipt) .ne. 3 .and. kctyp(ipt) .ne. 5 .and.
     1                kctyp(ipt) .ne. 12 .and. kctyp(ipt) .ne. 13 .and.
     2                kctyp(ipt) .ne. 14 .and. kctyp(ipt) .ne. 15 .and.
     3                kctyp(ipt) .ne. 18) go to 9200
              else if (kvar .eq. 2) then
                  if (kctyp(ipt) .ne. 8 .and. kctyp(ipt) .ne. 3 .and.
     1                kctyp(ipt) .ne. 5 .and. kctyp(ipt) .ne. 12 .and.
     2                kctyp(ipt) .ne. 13 .and. kctyp(ipt) .ne. 14 .and.
     3                kctyp(ipt) .ne. 15) go to 9400
              else
                  go to 9000
              endif
c
c......LOCATE
c
          else if (gcsub(kpt) .eq. 5020.) then
              if (kctyp(ipt) .ne. 3 .and. kctyp(ipt) .ne. 5 .and.
     1            kctyp(ipt) .ne. 12 .and. kctyp(ipt) .ne. 13 .and.
     2            kctyp(ipt) .ne. 14 .and. kctyp(ipt) .ne. 15)
     3                go to 9200
c
c......FMTCOD
c
          else if (gcsub(kpt) .eq. 5029.) then
              if (kvar .eq. 1) then
                  if (kctyp(ipt) .ne. 3 .and. kctyp(ipt) .ne. 5 .and.
     1                kctyp(ipt) .ne. 12 .and. kctyp(ipt) .ne. 13 .and.
     2                kctyp(ipt) .ne. 14 .and. kctyp(ipt) .ne. 15 .and.
     3                kctyp(ipt) .ne. 18) go to 9200
              else if (kvar .eq. 2) then
                  if (kctyp(ipt) .ne. 8 .and. kctyp(ipt) .ne. 3 .and.
     1                kctyp(ipt) .ne. 5 .and. kctyp(ipt) .ne. 12 .and.
     2                kctyp(ipt) .ne. 13 .and. kctyp(ipt) .ne. 14 .and.
     3                kctyp(ipt) .ne. 15) go to 9400
              else
                  go to 9000
              endif
c
c......Unrecognized function
c
          else
              go to 9000
          endif
c
c......This parameter should be a comma or end paren
c
          inc    = inc    + 1
          if (kctyp(inc) .eq. 2) then
              if (gcsub(inc) .eq. 2.) go to 100
              if (gcsub(inc) .ne. 18.) go to 9500
c
c.........End parenthesis / end of function
c
              if (kvar .ne. 2) go to 9000
              if (gcsub(kpt) .eq. 5012.) then
                  gcsub(kpt) = 28.
              else if (gcsub(kpt) .eq. 5013.) then
                  gcsub(kpt) = 29.
              else if (gcsub(kpt) .eq. 5014.) then
                  gcsub(kpt) = 30.
              else if (gcsub(kpt) .eq. 5015.) then
                  gcsub(kpt) = 31.
              else if (gcsub(kpt) .eq. 5020.) then
                  gcsub(kpt) = 36.
              else if (gcsub(kpt) .eq. 5029.) then
                  gcsub(kpt) = 45.
              endif
              go to 8000
          else
              go to 9000
          endif
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
c...Variable, voc word or number expected
c
 9100 call errtxt ('VARVOCNM',cmsg)
      kerr   = 1
      go to 8000
c
c...Number/variable expected
c
 9200 call errtxt ('NUMVAREX',cmsg)
      kerr   = 1
      go to 8000
c
c...Text/variable expected
c
 9300 call errtxt ('TXTVAREX',cmsg)
      kerr   = 1
      go to 8000
c
c...Format number expected
c
 9400 call errtxt ('FMTEXP',cmsg)
      kerr   = 1
      go to 8000
c
c...Comma expected
c
 9500 call errtxt ('COMAEXP',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpary (kctyp,gcsub,kcnc,ktok,kfl,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine removes and compiles all multi-dimensional
c              array references from any type of command after it has
c              been fully parsed.  The common arrays for token types & values
c              (ICTYP,RCSUB,ICNC,NTOK) must be passed in as local arrays
c              because this routine breaks out all equations and returns to
c              the calling routine to compile these.
c
c   INPUT:  kctyp   I*4  Dn  Local storage of the ICTYP common array.
c
c           gcsub   R*8  Dn  Local storage of the RCSUB common array.
c
c           kcnc    I*4  Dn  Local storage of the ICNC common array.
c
c           kary    I*4  Dn  Local storage.  'kary(10)' should be set
c                            to 0 on initial call.
c
c           ktok    I*4  D1  Local NTOK variable.
c
c   OUTPUT: kfl     I*4  D1  Returns 2 when the common token arrays
c                            contain an equation to be parsed.  'kfl'
c                            should be set to 0 prior to the 1st call
c                            and should not be altered after the call.
c
c           kpt     I*4  D1  Local pointer within local arrays that
c                            needs to be saved between calls.
c
c           kary    I*4  D12 Local storage.
c
c                                1,2 = Start and end of MDA subscript.
c                                3,4,5 = Size of each dimension.
c                                6 = Number of dimensions in array.
c                                7,8,9 = Paren var that holds each subscript.
c                                10 = 1 = Currently processing MDA subscript.
c                                11 = 5 = Real variable, 6 = Text variable.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpary (kctyp,gcsub,kcnc,kary,ktok,kfl,kpt,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kctyp(100),kcnc(100),kfl,kpt,kerr,ktok,kary(12)
c
      real*8 gcsub(100)
c
      character*(*) cmsg
c
      integer*4 i,ist,ip,inc,isca
c
      character*24 lbuf
c
c...Initialize routine
c
      kerr   = 0
      if (kfl .eq. 0) then
          if (NTOK .eq. 0) go to 8000
          kpt    = NTOK   + 1
          kary(1) = 0
c
c......Save current parameter types
c
          do 100 i=1,NTOK,1
              kctyp(i) = ICTYP(i)
              gcsub(i) = RCSUB(i)
              kcnc(i) = ICNC(i)
  100     continue
          ktok   = NTOK
c
c...Found all subscripts
c...Create Multi-dimensional array record
c
      else if (kfl .eq. kary(6)) then
          go to 400
      else
          kpt    = kpt    + 1
      endif
c
c...Search for multiple-dimensioned arrays
c
  200 kpt    = kpt    - 1
      if (kpt .eq. 0) go to 1000
c
c......See if Real or Char variable
c
      if (kfl .eq. 0 .and. kpt+1 .le. ktok .and. (kctyp(kpt) .eq. 4 .or.
     1    kctyp(kpt) .eq. 1 .and. (gcsub(kpt) .eq. 6007 .or.
     2    (gcsub(kpt) .ge. 6074 .and. gcsub(kpt) .le. 6079))) .and.
     2    kctyp(kpt+1) .eq. 2 .and. gcsub(kpt+1) .eq. 17) then
          ist    = gcsub(kpt)
          if (kctyp(kpt) .eq. 1) then
              kary(1) = kpt
              if (gcsub(kpt) .eq. 6007) then
                  kary(3) = 6
                  kary(4) = 40
              else
                  kary(3) = 6
                  kary(4) = 3
              endif
              kary(5) = 1
              kary(6) = 2
              kary(7) = 1
              kary(8) = 1
              kary(9) = 1
              kary(10) = 1
              kary(11) = gcsub(kpt)
              kpt    = kpt    + 1
          else if (kctyp(kpt) .eq. 1 .or. kcnc(kpt)-ist+1 .le. 24)
     1      then
              lbuf   = LCTXT(ist:kcnc(kpt))
              call getscl (lbuf,isca,ist,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (isca .eq. 5 .or. isca .eq. 6) then
                  inc    = isca   - 2
                  kary(1) = kpt
                  kary(3) = MFDAT(ist+9,inc)
                  kary(4) = MFDAT(ist+10,inc)
                  kary(5) = MFDAT(ist+11,inc)
                  kary(6) = MFDAT(ist+8,inc)
                  kary(7) = 1
                  kary(8) = 1
                  kary(9) = 1
                  kary(10) = 1
                  kary(11) = isca
                  kpt    = kpt    + 1
              endif
          endif
      endif
c
c......Found array reference
c
       if (kary(1) .ne. 0) then
          ist    = kpt    + 1
  250     kpt    = kpt    + 1
c
c.........Delimeter
c
          if (kctyp(kpt) .eq. 2) then
c
c............End parenthesis
c
              if (gcsub(kpt) .eq. 18.) then
                  kfl   = kfl   + 1
                  if (kfl .ne. kary(6) .and.
     1                (kfl+1 .ne. kary(6) .or. kary(11) .ne. 6))
     2                go to 9100
                  kary(2) = kpt
c
c...............Text variable with 1 less subscript specified
c...............Shift subscripts over and add range subscript
c...............as first subscript
c
                  if (kfl .ne. kary(6)) then
                      kfl    = kfl    + 1
                      kary(9) = kary(8)
                      kary(8) = kary(7)
                      kary(7) = 0
                  endif
                  go to 300
c
c............Comma
c
              else if (gcsub(kpt) .eq. 2.) then
                  if (kfl .eq. kary(6)) go to 9200
                  kfl   = kfl   + 1
                  go to 300
              endif
          endif
c
          go to 250
      endif
      go to 200
c
c...Found a subscript
c...Set up the type arrays and
c...send it back to the 'compil' routine
c
  300 if (NPNVAR .eq. MAXPRN) go to 9000
      NPNVAR = NPNVAR + 1
      kary(kfl+6) = NPNVAR
      ICTYP(1) = 5
      IPNSMP(NPNVAR) = 0
      RCSUB(1) = NPNVAR
      ICTYP(2) = 2
      RCSUB(2) = 1.
      NTOK   = 2
      do 330 i=ist,kpt-1,1
          NTOK   = NTOK   + 1
          ICTYP(NTOK) = kctyp(i)
          RCSUB(NTOK) = gcsub(i)
          ICNC(NTOK) = kcnc(i)
  330 continue
      go to 8000
c
c...End of MDA subscript
c......Create MDA command
c
  400 if (NPNVAR .eq. MAXPRN) go to 9000
      NPNVAR = NPNVAR + 1
      ICMPL(1) = 24
      ICMPL(2) = 15
      ICMPL(3) = NPNVAR
      ICMPL(4) = kary(6)
      ICMPL(11) = kary(11)
      IPNSMP(NPNVAR) = 0
      inc    = 7
      do 420 i=1,3,1
          if (i .le. kary(6) .and. kary(i+6) .ne. 0) then
              if (IPNSMP(kary(i+6)) .eq. 2 .and. (kary(11) .ne. 6 .or.
     1            i .ne. 1)) go to 9400
              if (IPNSMP(kary(i+6)) .gt. 2) go to 9500
          endif
          ICMPL(i+4) = kary(i+2)
          if (i .gt. kary(6)) then
              ICMPL(i+7) = 3
              RCMPL(i+3) = 1
          else if (IPNSMP(kary(i+6)) .eq. 1) then
              if (PRNVAR(kary(i+6)) .le. 0 .or.
     1            PRNVAR(kary(i+6)) .gt. kary(i+2)) go to 9300
              ICMPL(i+7) = 3
              RCMPL(i+3) = PRNVAR(kary(i+6))
          else
              ICMPL(i+7) = 1
              JCMPL(inc) = kary(i+6)
          endif
          inc    = inc    + 2
  420 continue
c
c......Write compiled record
c
      call cmpwrt (cmsg,kerr)
c
c......Remove this MDA subscript from
c......the original command
c
      ip     = kary(1) + 1
      kctyp(ip) = 5
      gcsub(ip) = NPNVAR
      do 450 i=kary(2)+1,ktok,1
          ip     = ip     + 1
          kctyp(ip) = kctyp(i)
          gcsub(ip) = gcsub(i)
          kcnc(ip) = kcnc(i)
  450 continue
      ktok   = ip
      kpt    = kary(1) - 1
      kary(1) = 0
      kfl    = 0
      if (kpt .gt. 0) go to 200
c
c...No more Multi-dimensional array references
c...Move local storage back to global arrays &
c...Return control to main compiler routine
c
 1000 if (kary(10) .eq. 1) then
          do 1100 i=1,ktok,1
              ICTYP(i) = kctyp(i)
              RCSUB(i) = gcsub(i)
              ICNC(i) = kcnc(i)
 1100     continue
          NTOK   = ktok
          kary(10) = 0
      endif
      kfl    = 0
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
c...Not enough subscripts
c
 9100 call errtxt ('SUBNEED',cmsg)
      kerr   = 1
      go to 8000
c
c...Too many subscripts
c
 9200 call errtxt ('SUBMANY',cmsg)
      kerr   = 1
      go to 8000
c
c...Subscript out of range
c
 9300 call errtxt ('SUBOUND',cmsg)
      kerr   = 1
      go to 8000
c
c...Subscript range not allowed
c
 9400 call errtxt ('SUBRANG',cmsg)
      kerr   = 1
      go to 8000
c
c...Invalid syntax
c
 9500 call errtxt ('INVSYN',cmsg)
      kerr   = 1
      go to 8000
      end
