c
c***********************************************************************
c
c   FILE NAME:  prompt.for
c   CONTAINS:
c               prmaxs  prmcod  prmint  prmrel  prmstr  prmvoc  prompt
c               prscrl
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        prompt.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:58
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  prmaxs (kaxs,knc,kmax,ksgn,kfl,klod,kwrn,cmsg,kerr)
c
c   FUNCTION:  This routine prompts for an array of axis descriptors and
c              checks the users' input for valid descriptors and values.
c              The axis descriptors have the following format:
c
c                        X1 X2 Y1 Y2 Z1 Z2 A1 A2 A3 A4
c
c   INPUT:  kaxs    I*4  Dn  -  Array of integers that define which
c                               axis.
c
c           knc     I*4  D1  -  Number of descriptors in array.
c
c           kmax    I*4  D1  -  Maximum number of descriptors that can be
c                               defined.
c
c           ksgn    I*4  D1  -  1 = An End-of-block '$' is valid input.
c                               2 = No.
c
c           kfl     I*4  D1  -  The user requested a specific prompt
c                               when 'kfl' = 'NLEVL'.  In this case the
c                               prompt will be displayed on the Menu
c                               Selection line instead of in the Menu/
c                               Prompt text area.
c
c           klod    I*4  D1  -  0 = This section's prompt text is cur-
c                               rently loaded in memory.  1 = Load the
c                               current section's prompt text into mem-
c                               ory and display the current section and
c                               level.
c
c           kwrn    I*4  D1  -  1 = The user's input to the previous
c                               prompt was not valid.  Reissue that
c                               prompt again.
c
c   OUTPUT: kaxs    I*4  Dn  -  See input.
c
c           knc     I*4  D1  -  See input.
c
c           kwrn    I*4  D1  -  1 = The user interrupted input with a
c                               ^C.  2 = Up arrow was entered.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine prmaxs (kaxs,knc,kmax,ksgn,kfl,klod,kwrn,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kaxs(20),knc,ksgn,kfl,klod,kwrn,kerr,kmax
c
      character*(*) cmsg
c
      character*2 laxs(10)
      character*80 sbuf
c
      integer*4 nc,i,j,inc,index,iary(20),iaxs(20),nca
c
      data laxs /'X1','X2','Y1','Y2','Z1','Z2','A1','A2','A3','A4'/
c
c...Set up default string
c
      if (IMSCAN .eq. 2) go to 200
      nc     = 0
      do 100 i=1,knc,1
          if (kaxs(i) .eq. 0) go to 100
          if (i .ne. 1) then
              nc     = nc     + 1
              sbuf(nc:nc) = ','
          endif
          if (kaxs(i) .eq. -1) then
              sbuf(nc+1:nc+2) = '$'
              nc     = nc     + 1
          else
              sbuf(nc+1:80) = laxs(kaxs(i))
              nc     = nc     + 2
          endif
  100 continue
c
c...Prompt the user
c
  200 call prompt (sbuf,nc,kfl,klod,kwrn,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1) go to 8000
      if (nc .eq. -1) go to 9100
      if (nc .eq. -2) go to 9200
c
c...Parse the users response
c
      i      = 1
      inc    = 0
      call remspc (sbuf,sbuf,nc)
      if (nc .eq. 0) then
          kaxs(1) = 0
          knc    = 0
          kwrn   = 0
          go to 8000
      endif
      call touppr (sbuf,sbuf)
      do 300 j=1,20,1
          iary(j) = 0
  300 continue
c
c......Find the next register
c
  400 if (i .gt. nc) go to 800
      nca    = index(sbuf(i:nc),',')
      if (nca .eq. 0) then
          nca    = nc     + 1
      else
          nca    = i      + nca    - 1
      endif
c
c.........Next register is End-of-block
c
      if (nca .eq. i+1 .and. ksgn .eq. 1 .and. sbuf(i:i) .eq. '$') then
          inc    = inc    + 1
          iaxs(inc) = -1
          i      = nca    + 1
          go to 400
      endif
c
c.........Search for register in array
c
      if (nca .ne. i+2) go to 9000
      do 500 j=1,10,1
          if (sbuf(i:nca-1) .eq. laxs(j)) go to 600
  500 continue
c
c......Invalid input
c
      go to 9000
c
c......Register match
c
  600 if (iary(j) .eq. 1) go to 9000
      if (inc .eq. kmax) go to 9000
      inc    = inc    + 1
      iaxs(inc) = j
      iary(j) = 1
      i      = nca    + 1
      go to 400
c
c......End of parse
c
  800 knc    = inc
      do 900 i=1,knc,1
          kaxs(i) = iaxs(i)
  900 continue
      kwrn   = 0
c
c...End of routine
c
 8000 return
c
c...Invalid users response
c
 9000 kwrn   = 1
      call errmsg ('INVRSP',1,2)
      if (MOTIF .eq. 1) go to 8000
      go to 200
c
c...End of input from user
c
 9100 kwrn   = 1
      go to 8000
c
c...Up arrow
c
 9200 kwrn   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmcod (kreg,gval,kcnt,kwds,ksw,kfl,klod,kwrn,cmsg,
c                        kerr)
c
c   FUNCTION:  This routine prompts for an array of registers and checks
c              the users' input for valid registers and values.
c
c   INPUT:  kreg    I*4  Dn  -  Array of integers that define which
c                               register.
c
c           gval    R*8  Dn  -  Array of values for each register.
c
c           kcnt    I*4  D1  -  Current number of registers defined.
c
c           kwds    I*4  D1  -  Maximum number of registers that can be
c                               defined.
c
c           ksw     I*4  D1  -  1 = Registers can have a value.  2 = No,
c                               also a logical register (less than 0) can
c                               not be specified.
c
c           kfl     I*4  D1  -  The user requested a specific prompt
c                               when 'kfl' = 'NLEVL'.  In this case the
c                               prompt will be displayed on the Menu
c                               Selection line instead of in the Menu/
c                               Prompt text area.
c
c           klod    I*4  D1  -  0 = This section's prompt text is cur-
c                               rently loaded in memory.  1 = Load the
c                               current section's prompt text into mem-
c                               ory and display the current section and
c                               level.
c
c           kwrn    I*4  D1  -  1 = The user's input to the previous
c                               prompt was not valid.  Reissue that
c                               prompt again.
c
c   OUTPUT: kreg    I*4  Dn  -  See input.
c
c           gval    R*8  Dn  -  See input.
c
c           kcnt    I*4  D1  -  Number of registers the user input.
c
c           kwrn    I*4  D1  -  1 = The user interrupted input with a
c                               ^C.  2 = Up arrow was entered.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine prmcod (kreg,gval,kcnt,kwds,ksw,kfl,klod,kwrn,cmsg,
     1                   kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 kreg(2),kcnt,kwds,ksw,kfl,klod,kwrn,kerr
c
      real*8 gval(2)
c
      character*(*) cmsg
c
      integer*4 i,nc,nca,inc,inum,iary(20)
c
      real*8 rnum,rary(20)
c
      character*80 sbuf
c
c...Set up default answer
c
      if (IMSCAN .eq. 2) go to 200
      nc     = 0
      do 100 i=1,kcnt,1
          inum   = kreg(i)
          rnum   = gval(i)
          call cdtoc (inum,rnum,sbuf(nc+1:80),nca)
          nc     = nc     + nca
          if (i .ne. kcnt) then
              nc     = nc     + 1
              sbuf(nc:nc) = ','
          endif
  100 continue
c
c...Get user's input
c
  200 call prompt (sbuf,nc,kfl,klod,kwrn,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1) go to 8000
      if (nc .eq. -1) go to 9000
      if (nc .eq. -2) go to 9200
c
c...Check validity of answer
c
      if (nc .eq. 0) then
          inc    = 0
          kreg(1) = 0
          gval(1) = DUMMY
      else
          i      = 1
          inc    = 0
  500     nca    = index(sbuf(i:nc),',')
          if (nca .eq. 0) then
              nca    = nc     + 1
          else
              nca    = i      + nca    - 1
          endif
          kwrn   = 0
          call ctocd (sbuf(i:nca-1),nca-i,inum,rnum,kwrn)
c
          if (kwrn .eq. 1) go to 9100
          if (inc .eq. kwds) go to 9100
          if (rnum .ne. DUMMY .and. ksw .eq. 2) go to 9100
          if (inum .lt. 0 .and. ksw .eq. 2) go to 9100
c
          inc    = inc    + 1
          iary(inc) = inum
          rary(inc) = rnum
          i      = nca    + 1
          if (i .le. nc) go to 500
      endif
c
c...Store answer
c
      kcnt   = inc
      do 600 i=1,kcnt,1
          kreg(i) = iary(i)
          gval(i) = rary(i)
  600 continue
      kwrn   = 0
c
c...End of routine
c
 8000 return
c
c...End of input from user
c
 9000 kwrn   = 1
      go to 8000
c
c...Invalid response
c
 9100 call errmsg ('INVRSP',1,2)
      kwrn   = 1
      if (IMSCAN .eq. 2) go to 8000
      go to 200
c
c...Up arrow
c
 9200 kwrn   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmint (knum,kcnt,kwds,kmin,kmax,kfl,klod,kwrn,cmsg,
c                        kerr)
c
c   FUNCTION:  This routine prompts for an array of integers and checks
c              the users' input against minimum and maximum values.
c
c   INPUT:  knum    I*4  Dn  -  Array of integers to set up as default
c                               answer.
c
c           kcnt    I*4  D1  -  Current number of values in 'knum'.  A
c                               negative number shows that this is a
c                               dynamic prompt.
c
c           kwds    I*4  D1  -  Maximum size of 'knum'.
c
c           kmin    I*4  Dn  -  Array of minimum values for 'knum'.
c
c           kmax    I*4  Dn  -  Array of maximum values for 'knum'.
c
c           kfl     I*4  D1  -  The user requested a specific prompt
c                               when 'kfl' = 'NLEVL'.  In this case the
c                               prompt will be displayed on the Menu
c                               Selection line instead of in the Menu/
c                               Prompt text area.
c
c           klod    I*4  D1  -  0 = This section's prompt text is cur-
c                               rently loaded in memory.  1 = Load the
c                               current section's prompt text into mem-
c                               ory and display the current section and
c                               level.
c
c           kwrn    I*4  D1  -  1 = The user's input to the previous
c                               prompt was not valid.  Reissue that
c                               prompt again.
c
c   OUTPUT: knum    I*4  Dn  -  Array of integers from user input.
c
c           kcnt    I*4  D1  -  Number of values in 'knum'.
c
c           kwrn    I*4  D1  -  1 = The user interrupted input with a
c                               ^C.  2 = Up arrow.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine prmint (knum,kcnt,kwds,kmin,kmax,kfl,klod,kwrn,cmsg,
     1                   kerr)
c
      include 'menu.inc'
c
      integer*4 knum(2),kcnt,kwds,kmin(2),kmax(2),kfl,klod,kwrn,kerr
c
      character*(*) cmsg
c
      integer*4 i,nc,iary(20),nwds,inc,ist,nchc,ipt,ibtyp
c
      character*80 sbuf
c
c...Set up default answer
c
      ibtyp  = 2
      if (kcnt .lt. 0) then
          ibtyp  = 5
          kcnt  = abs(kcnt)
      endif
      if (IMSCAN .ne. 2) call geti4s (knum,kcnt,sbuf,nc)
c
c...Motif interface
c...Set default answer
c
      if (IMSCAN .eq. 1 .and. kwds .eq. 1 .and. kmax(1)-kmin(1) .lt. 10)
     1        then
          inc    = MLEVL(NLEVL)
          ist    = kmin(1) - 1
          nchc   = kmax(1) - kmin(1) + 1
          ipt    = 0
          do 100 i=1,nchc,1
cc              if (ist+i .eq. knum(1)) then
cc                  call itoc (ist+i,SBCDEF(1,inc),nc,0)
cc              else
                  ipt    = ipt    + 1
                  call itoc (ist+i,SBCDEF(ipt,inc),nc,0)
cc              endif
  100     continue
          JBDCHC(inc) = knum(1) - kmin(1)
          JBNCHC(inc) = nchc
          JBTYPE(inc) = ibtyp
      endif
c
c...Get user's input
c
  200 call prompt (sbuf,nc,kfl,klod,kwrn,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1) go to 8000
      if (nc .eq. -1) go to 9000
      if (nc .eq. -2) go to 9200
c
c...Check validity of answer
c
      call geti4n (sbuf,nc,iary,nwds,kwrn)
      if (kwrn .eq. 1) go to 9100
      if (nwds .eq. 0) then
          kcnt   = 0
          knum(1) = 0
      else
          if (nwds .gt. kwds) go to 9100
          do 400 i=1,nwds,1
              if (iary(i) .lt. kmin(i) .or. iary(i) .gt. kmax(i))
     1                go to 9100
              knum(i) = iary(i)
  400     continue
          kcnt   = nwds
      endif
      kwrn   = 0
c
c...End of routine
c
 8000 return
c
c...End of input from user
c
 9000 kwrn   = 1
      go to 8000
c
c...Invalid response
c
 9100 call errmsg ('INVRSP',1,2)
      kwrn   = 1
      if (IMSCAN .eq. 2) go to 8000
      go to 200
c
c...Up arrow
c
 9200 kwrn   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmrel (gnum,kcnt,kwds,gmin,gmax,kfl,klod,kwrn,cmsg,
c                        kerr)
c
c   FUNCTION:  This routine prompts for an array of integers and checks
c              the users' input against minimum and maximum values.
c
c   INPUT:  gnum    R*8  Dn  -  Array of reals to set up as default
c                               answer.
c
c           kcnt    I*4  D1  -  Current number of values in 'gnum'.
c
c           kwds    I*4  D1  -  Maximum size of 'gnum'.
c
c           gmin    R*8  Dn  -  Array of minimum values for 'gnum'.
c
c           gmax    R*8  Dn  -  Array of maximum values for 'gnum'.
c
c           kfl     I*4  D1  -  The user requested a specific prompt
c                               when 'kfl' = 'NLEVL'.  In this case the
c                               prompt will be displayed on the Menu
c                               Selection line instead of in the Menu/
c                               Prompt text area.
c
c           klod    I*4  D1  -  0 = This section's prompt text is cur-
c                               rently loaded in memory.  1 = Load the
c                               current section's prompt text into mem-
c                               ory and display the current section and
c                               level.
c
c           kwrn    I*4  D1  -  1 = The user's input to the previous
c                               prompt was not valid.  Reissue that
c                               prompt again.
c
c   OUTPUT: gnum    R*8  Dn  -  Array of reals from user input.
c
c           kcnt    I*4  D1  -  Number of values in 'gnum'.
c
c           kwrn    I*4  D1  -  1 = The user interrupted input with a
c                               ^C.  2 = Up arrow.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine prmrel (gnum,kcnt,kwds,gmin,gmax,kfl,klod,kwrn,cmsg,
     1                   kerr)
c
      include 'menu.inc'
c
      integer*4 kcnt,kwds,kfl,klod,kwrn,kerr
c
      real*8 gnum(2),gmin(2),gmax(2)
c
      character*(*) cmsg
c
      integer*4 i,nc,nwds
c
      real*8 rary(20)
c
      character*80 sbuf
c
c...Set up default answer
c
      if (IMSCAN .ne. 2) call getr8s (gnum,kcnt,sbuf,nc)
c
c...Get user's input
c
  200 call prompt (sbuf,nc,kfl,klod,kwrn,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1) go to 8000
      if (nc .eq. -1) go to 9000
      if (nc .eq. -2) go to 9200
c
c...Check validity of answer
c
      call getr8n (sbuf,nc,rary,nwds,kwrn)
      if (kwrn .eq. 1) go to 9100
      if (nwds .eq. 0) then
          kcnt   = 0
          gnum(1) = 0
      else
          if (nwds .gt. kwds) go to 9100
          do 400 i=1,nwds,1
              if (rary(i) .lt. gmin(i) .or. rary(i) .gt. gmax(i))
     1                go to 9100
              gnum(i) = rary(i)
  400     continue
          kcnt   = nwds
      endif
      kwrn   = 0
c
c...End of routine
c
 8000 return
c
c...End of input from user
c
 9000 kwrn   = 1
      go to 8000
c
c...Invalid response
c
 9100 call errmsg ('INVRSP',1,2)
      kwrn   = 1
      if (IMSCAN .eq. 2) go to 8000
      go to 200
c
c...Up arrow
c
 9200 kwrn   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmstr (knum,cdat,kwds,kfl,klod,kwrn,cmsg,kerr)
c
c   FUNCTION:  This routine prompts for a character string and checks
c              the users' input against an array of acceptable strings.
c
c   INPUT:  knum    I*4  D1  -  Position within array of default answer.
c
c           cdat    C*n  Dn  -  Array of acceptable strings.
c
c           kwds    I*4  D1  -  Number of acceptable character strings.
c                               A negative number specifies a dynamic
c                               prompt.  A value of 0 means not to check
c                               against array and return actual text string
c                               entered.
c
c           kfl     I*4  D1  -  The user requested a specific prompt
c                               when 'kfl' = 'NLEVL'.  In this case the
c                               prompt will be displayed on the Menu
c                               Selection line instead of in the Menu/
c                               Prompt text area.
c
c           klod    I*4  D1  -  0 = This section's prompt text is cur-
c                               rently loaded in memory.  1 = Load the
c                               current section's prompt text into mem-
c                               ory and display the current section and
c                               level.
c
c           kwrn    I*4  D1  -  1 = The user's input to the previous
c                               prompt was not valid.  Reissue that
c                               prompt again.
c
c   OUTPUT: cdat    C*n  D1  -  Updated text string when kwds = 0.
c
c           knum    I*4  Dn  -  Position within array of users' answer or
c                               Number of chars in 'cdat' when kwds = 0.
c
c           kwrn    I*4  D1  -  1 = The user interrupted input with a
c                               ^C.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine prmstr (knum,cdat,kwds,kfl,klod,kwrn,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 knum,kwds,kfl,klod,kwrn,kerr
c
      character*(*) cdat(2),cmsg
c
      integer*4 nc,strlen1,i,inc,ipt,ibut,inum
c
      character*80 sbuf
c
c...Dynamic prompt
c
      ibut   = 2
      if (kwds .lt. 0) then
          kwds   = abs(kwds)
          ibut   = 5
      endif
c
c...Motif interface
c...Set default answer
c
      inum   = 1
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1 .and. kwds .gt. 0) then
          inc    = MLEVL(NLEVL)
          ipt    = 0
          inum   = knum
          do 100 i=1,kwds,1
cc              if (i .eq. knum) then
cc                  SBCDEF(1,inc) = cdat(i)
cc              else
                  ipt    = ipt    + 1
                  SBCDEF(ipt,inc) = cdat(i)
cc              endif
  100     continue
          JBNCHC(inc) = kwds
          JBDCHC(inc) = knum - 1
          JBTYPE(inc) = ibut
      endif
c
c...Set up default answer
c
      sbuf   = cdat(inum)
      nc     = strlen1(sbuf)
c
c...Get user's input
c
  200 call prompt (sbuf,nc,kfl,klod,kwrn,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1) go to 8000
      if (nc .eq. -1) go to 9000
      if (nc .eq. -2) go to 9200
c
c...Calling routine wants string returned only
c
      if (kwds .eq. 0) then
          cdat(1) = sbuf
          knum   = nc
          kwrn   = 0
c
c...Check validity of answer
c
      else
          if (nc .eq. 0) go to 9100
          call touppr (sbuf,sbuf)
          do 400 i=1,kwds,1
              if (sbuf .eq. cdat(i)) go to 450
  400     continue
          go to 9100
  450     knum   = i
          kwrn   = 0
      endif
c
c...End of routine
c
 8000 return
c
c...End of input from user
c
 9000 kwrn   = 1
      go to 8000
c
c...Invalid response
c
 9100 call errmsg ('INVRSP',1,2)
      kwrn   = 1
      if (IMSCAN .eq. 2) go to 8000
      go to 200
c
c...Up arrow
c
 9200 kwrn   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prmvoc (knum,kvoc,kwds,kfl,klod,kwrn,cmsg,kerr)
c
c   FUNCTION:  This routine prompts for a vocabulary word and checks the
c              the users' input against an array of accepted words.
c
c   INPUT:  knum    I*4  D1  -  Position within array of default answer.
c
c           kvoc    I*4  Dn  -  Array of acceptable vocabulary word
c                               values.
c
c           kwds    I*4  D1  -  Number of acceptable vocabulary words.
c                               A negative number specifies a dynamic
c                               prompt.
c
c           kfl     I*4  D1  -  The user requested a specific prompt
c                               when 'kfl' = 'NLEVL'.  In this case the
c                               prompt will be displayed on the Menu
c                               Selection line instead of in the Menu/
c                               Prompt text area.
c
c           klod    I*4  D1  -  0 = This section's prompt text is cur-
c                               rently loaded in memory.  1 = Load the
c                               current section's prompt text into mem-
c                               ory and display the current section and
c                               level.
c
c           kwrn    I*4  D1  -  1 = The user's input to the previous
c                               prompt was not valid.  Reissue that
c                               prompt again.
c
c   OUTPUT: knum    I*4  Dn  -  Position within array of users' answer.
c
c           kwrn    I*4  D1  -  1 = The user interrupted input with a
c                               ^C.  2 = Up arrow.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine prmvoc (knum,kvoc,kwds,kfl,klod,kwrn,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 knum,kvoc(2),kwds,kfl,klod,kwrn,kerr
c
      character*(*) cmsg
c
      integer*4 nc,inum,i,inc,ipt,ibut
c
      character*80 sbuf
c
c...Dynamic prompt
c
      ibut   = 2
      if (kwds .lt. 0) then
          kwds   = abs(kwds)
          ibut   = 5
      endif
c
c...Motif interface
c...Set default answer
c
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1) then
          inc    = MLEVL(NLEVL)
          ipt    = 0
          do 100 i=1,kwds,1
cc              if (i .eq. knum) then
cc                  call getvwd (kvoc(i),SBCDEF(1,inc),nc,0,
cc     1                         MENWRD,MENWVL,NMENWD)
cc              else
                  ipt    = ipt    + 1
                  call getvwd (kvoc(i),SBCDEF(ipt,inc),nc,0,
     1                         MENWRD,MENWVL,NMENWD)
cc              endif
  100     continue
          JBNCHC(inc) = kwds
          JBDCHC(inc) = knum - 1
          JBTYPE(inc) = ibut
      endif
c
c...Set up default answer
c
      if (IMSCAN .ne. 2)
     1    call getvwd (kvoc(knum),sbuf,nc,0,MENWRD,MENWVL,NMENWD)
c
c...Get user's input
c
  200 call prompt (sbuf,nc,kfl,klod,kwrn,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1) go to 8000
      if (nc .eq. -1) go to 9000
      if (nc .eq. -2) go to 9200
c
c...Check validity of answer
c
      call getvnr (sbuf,kvoc,kwds,inum,MENWRD,MENWVL,NMENWD)
      if (inum .eq. 0) go to 9100
      knum   = inum
      kwrn   = 0
c
c...End of routine
c
 8000 return
c
c...End of input from user
c
 9000 kwrn   = 1
      go to 8000
c
c...Invalid response
c
 9100 call errmsg ('INVRSP',1,2)
      kwrn   = 1
      if (IMSCAN .eq. 2) go to 8000
      go to 200
c
c...Up arrow
c
 9200 kwrn   = 2
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prompt (cbuf,knc,kfl,klod,kwrn,cmsg,kerr)
c
c   FUNCTION:  This is the prompt handling routine.  It displays a sin-
c              gle prompt and gets the user's input from the keyboard.
c              It also displays the current section and level when it
c              changes.  The HELP feature is controlled by this routine.
c              'prompt' must be called a separate time for each prompt,
c              regardless if it is in the same level or not.
c
c   INPUT:  kfl     I*4  D1  -  The user requested a specific prompt
c                               when 'kfl' = 'NLEVL'.  In this case the
c                               prompt will be displayed on the Menu
c                               Selection line instead of in the Menu/
c                               Prompt text area.
c
c           klod    I*4  D1  -  0 = This section's prompt text is cur-
c                               rently loaded in memory.  1 = Load the
c                               current section's prompt text into mem-
c                               ory and display the current section and
c                               level.
c
c           kwrn    I*4  D1  -  1 = The user's input to the previous
c                               prompt was not valid.  Reissue that
c                               prompt again.
c
c   OUTPUT: cbuf    C*n  D1  -  Character string that contains the
c                               user's input.  If 'knc' is greater than
c                               0 when 'prompt' is called, then 'cbuf'
c                               also contains the default input.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.  -1 =
c                               input was interrupted with a ^C.  -2 =
c                               Up arrow.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine prompt (cbuf,knc,kfl,klod,kwrn,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 knc,kfl,kwrn,klod,kerr
c
      character*(*) cbuf,cmsg
c
      integer*4 ipt,itxt,nc,ntxt,i,ilev,ilod,strlen1,ierr,nca,inc
c
      character*80 sbuf,abuf
c
c...Initialize routine
c
      if (knc .eq. 0) cbuf = ' '
      abuf   = cbuf
      ilev   = MLEVL(NLEVL) + 1
c
c...Motif interface
c
      if (MOTIF .eq. 1) then
c
c......Set default answer
c
          if (IMSCAN .eq. 1) then
              if (klod .eq. 1) then
                  call lodmen (MLEVL,NLEVL,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  PRMSP  = 0
              endif
              inc    = MLEVL(NLEVL)
              IMTYP = 2
              JBTACT(inc) = 1
              if (JBTYPE(inc) .eq. 0) JBTYPE(inc) = 4
              SBDEF(inc) = cbuf
c
c......Return answer from user
c
          else
              abuf   = MFANS
              knc    = MFKNC
              kerr   = 0
              kwrn   = 0
          endif
          go to 8000
      endif
c
c...Error on last prompt input
c...Get input again
c
      if (kwrn .eq. 1) then
          ipt    = IPRDAT(2,ilev)
          itxt   = (ipt-1) * 4 + 5
          nc     = IPRTXT(ipt)
  100     nca    = knc
          call getlin (abuf,knc,len(cbuf),IPRLIN,nc+2)
c
c......Up arrow
c......Go to previous prompt
c
          if (knc .eq. -2) then
              if (PRMSP .ne. 0) go to 8000
              knc    = nca
              go to 100
c
c......^W
c......Redisplay page
c
          else if (knc .eq. -3) then
              call disban
              call dissec
              ipt    = IPRDAT(2,ilev)
              itxt   = (ipt-1) * 4 + 5
              nc     = IPRTXT(ipt)
              call plott (IPRLIN,1)
              call dmpbuf (LPRTXT(itxt:itxt+nc-1),nc)
              knc    = nca
              go to 100
          endif
c
c......Erase error messages
c
          call errmsg (' ',2,1)
          call errmsg (' ',2,2)
c
c......? Entered
c......Display help text
c
          if (knc .eq. 1 .and. abuf(1:1) .eq. '?') then
              abuf   = cbuf
              knc    = strlen1(abuf)
              call levasc (MLEVL,NLEVL,sbuf)
              call helpi (sbuf,MLEVL(1),ierr)

c
c......No help text
c......Go get input again
c
              if (ierr .eq. 1) go to 100
c
c......Help text was displayed
c......Display prompt at Selection line
c
              IPRLIN = ISELIN
              SNLEVL = 0
              call plott (IPRLIN,1)
              call dmpbuf (LPRTXT(itxt:itxt+nc-1),nc)
              go to 100
          endif
c
          PRMSP  = PRMSP  + 1
          PRMSTK(PRMSP) = MLEVL(NLEVL)
          go to 8000
      endif
c
c...No error on last prompt
c...Load the levels of menu items
c
      if (klod .eq. 1) then
          call lodmen (MLEVL,NLEVL,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          PRMSP  = 0
      endif
      ilod   = klod
      klod   = 0
c
c......User requested direct access to prompt
c......Display this prompt only
c
  500 if (kfl .eq. NLEVL) then
          IPRLIN = ISELIN
          call plott (IPRLIN,1)
c
c......First prompt in this section
c......Erase screen & initialize screen pointers
c
      else
          SMLEVL(NLEVL-1) = MLEVL(NLEVL-1)
          SNLEVL = 0
          if (ilod .eq. 1 .or. IPRLIN .lt. IMENBL .or.
     1        IPRLIN .gt. IMENEL) then
              IPRLIN = IMENBL
              call plott (IPRLIN,1)
              call clreos
c
c.........Display active sections
c
              if (ilod .eq. 1) call dissec
c
c......nth prompt in this section
c......Position cursor at prompt line
c
          else
              call prscrl (1)
          endif
      endif
c
c......Display extra text messages
c
      ntxt   = IPRDAT(1,ilev)
      if (ntxt .gt. 1 .and. IPRLIN .ne. ISELIN) then
          call prscrl (1)
          ipt    = IPRDAT(2,ilev)
          nc     = IPRTXT(ipt)
          do 1000 i=2,ntxt,1
              ipt    = ipt    + (nc-1) / 4 + 2
              itxt   = (ipt-1) * 4 + 5
              nc     = IPRTXT(ipt)
              sbuf   = '       ' // LPRTXT(itxt:itxt+nc-1)
              call dmpbuf (sbuf,nc+7)
              call prscrl (1)
 1000     continue
          call prscrl (1)
      endif
c
c......Display prompt & get user's input
c
      ipt    = IPRDAT(2,ilev)
      itxt   = (ipt-1) * 4 + 5
      nc     = IPRTXT(ipt)
 1100 call plott (IPRLIN,1)
      call dmpbuf (LPRTXT(itxt:itxt+nc-1),nc)
 1150 nca    = knc
      call getlin (abuf,knc,len(cbuf),IPRLIN,nc+2)
c
c......Up arrow
c......Go to previous prompt
c
      if (knc .eq. -2) then
          if (PRMSP .ne. 0) go to 8000
          knc    = nca
          go to 1150
c
c......^W
c......Redisplay page
c
      else if (knc .eq. -3) then
          knc    = nca
          call disban
          if (kfl .eq. NLEVL) then
              call dissec
              SNLEVL = 0
          else
              ilod   = 1
          endif
          go to 500
      endif
c
c......Erase error messages
c
      call errmsg (' ',2,1)
      call errmsg (' ',2,2)
c
c......? Entered
c......Display help text
c
      if (knc .eq. 1 .and. abuf(1:1) .eq. '?') then
          abuf   = cbuf
          knc    = strlen1(abuf)
          call levasc (MLEVL,NLEVL,sbuf)
          call helpi (sbuf,MLEVL(1),ierr)
c
c.........No help text
c.........Go get input again
c
          if (ierr .eq. 1) go to 1150
c
c.........Help text was displayed
c.........Display prompt at Selection line
c
          IPRLIN = ISELIN
          SNLEVL = 0
          go to 1100
      endif
      PRMSP  = PRMSP  + 1
      PRMSTK(PRMSP) = MLEVL(NLEVL)
c
c...End of routine
c
 8000 cbuf   = abuf
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prscrl (klin)
c
c   FUNCTION:  This routine advances the cursor 'klin's and scrolls the
c              prompt area if necessary.  'IPRLIN' is used as the cur-
c              rent cursor line.
c
c   INPUT:  klin    I*4  D1  -  Number of lines to advance.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine prscrl (klin)
c
      include 'menu.inc'
c
      integer*4 klin
c
      integer*4 i
c
c...Move the current prompt location line
c
      do 100 i=1,klin,1
c
c......At the bottom of the page
c......Scroll the display
c
          if (IPRLIN .eq. IMENEL) then
              call scrol (IMENBL,IMENEL,1,1)
              call plott (IPRLIN,1)
              call clreol
c
c......Within the page
c......Move prompt location down 1 line
c
          else
              IPRLIN = IPRLIN + 1
          endif
  100 continue
      call plott (IPRLIN,1)
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  prmvcs (kvoc,knc,kmax,ksgn,kfl,klod,kwrn,cmsg,kerr)
c
c   FUNCTION:  This routine prompts for an array of vocabulary words and
c              checks the users' input for valid words.
c
c   INPUT:  kvoc    I*4  Dn  -  Array of integers that define default
c                               vocabulary words.
c
c           knc     I*4  D1  -  Number of words in array 'kvoc'.
c
c           nvoc    I*4  Dn  -  Array of integers that define vocabulary
c                               dictionary.
c
c           kmax    I*4  D1  -  Maximum number of words that can be
c                               defined ('nvoc' size).
c
c           ksgn    I*4  D1  -  1 = An End-of-block '$' is valid input.
c                               2 = No.
c
c           kfl     I*4  D1  -  The user requested a specific prompt
c                               when 'kfl' = 'NLEVL'.  In this case the
c                               prompt will be displayed on the Menu
c                               Selection line instead of in the Menu/
c                               Prompt text area.
c
c           klod    I*4  D1  -  0 = This section's prompt text is cur-
c                               rently loaded in memory.  1 = Load the
c                               current section's prompt text into mem-
c                               ory and display the current section and
c                               level.
c
c           kwrn    I*4  D1  -  1 = The user's input to the previous
c                               prompt was not valid.  Reissue that
c                               prompt again.
c
c   OUTPUT: kaxs    I*4  Dn  -  See input.
c
c           knc     I*4  D1  -  See input.
c
c           kwrn    I*4  D1  -  1 = The user interrupted input with a
c                               ^C.  2 = Up arrow was entered.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine prmvcs (kvoc,knc,nvoc,kmax,ksgn,kfl,klod,kwrn,
     -                   cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kvoc(20),knc,nvoc(20),ksgn,kfl,klod,kwrn,kerr,kmax
c
      character*(*) cmsg
c
      character*80 sbuf,lbuf
      character*24 lvoc(20)
c
      integer*4 nc,i,j,inc,index,iary(20),ivoc(20),nca,jindex,strlen1
c
c...Set up default string
c
c     if (IMSCAN .eq. 2) go to 200
      do 105 i=1,kmax,1
          call getvwd (nvoc(i),lbuf,nca,0,MENWRD,MENWVL,NMENWD)
          lvoc(i) = lbuf(1:nca)
  105 continue
      if (IMSCAN .eq. 2) go to 200
      nc     = 0
      inc    = 0
      do 115 i=1,knc,1
          if (kvoc(i) .eq. 0) go to 115
          j   = jindex (nvoc,kvoc(i),kmax)
          if (j .eq. 0) go to 115
          inc = inc + 1
          if (inc .gt. 1) then
              nc     = nc     + 1
              sbuf(nc:nc) = ','
          endif
          if (kvoc(i) .eq. -1) then
              nc     = nc     + 1
              sbuf(nc:nc+1) = '$'
          else
              nca = strlen1 (lvoc(j))
              sbuf(nc+1:80) = lvoc(j)(1:nca)
              nc     = nc     + nca
          endif
  115 continue
c
c...Prompt the user
c
  200 call prompt (sbuf,nc,kfl,klod,kwrn,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (MOTIF .eq. 1 .and. IMSCAN .eq. 1) go to 8000
      if (nc .eq. -1) go to 9100
      if (nc .eq. -2) go to 9200
c
c...Parse the users response
c
      i      = 1
      inc    = 0
      call remspc (sbuf,sbuf,nc)
      if (nc .eq. 0) then
          kvoc(1) = 0
          knc    = 0
          kwrn   = 0
          go to 8000
      endif
      call touppr (sbuf,sbuf)
      do 300 j=1,20,1
          iary(j) = 0
  300 continue
c
c......Find the next register
c
  400 if (i .gt. nc) go to 800
      nca    = index(sbuf(i:nc),',')
      if (nca .eq. 0) then
          nca    = nc     + 1
      else
          nca    = i      + nca    - 1
      endif
c
c.........Next register is End-of-block
c
      if (nca .eq. i+1 .and. ksgn .eq. 1 .and. sbuf(i:i) .eq. '$') then
          inc    = inc    + 1
          ivoc(inc) = -1
          i      = nca    + 1
          go to 400
      endif
c
c.........Search for word in array
c
      do 500 j=1,kmax,1
          if (sbuf(i:nca-1) .eq. lvoc(j)) go to 600
  500 continue
c
c......Invalid input
c
      go to 9000
c
c......Register match
c
  600 if (iary(j) .eq. 1) go to 9000
      if (inc .eq. kmax) go to 9000
      inc    = inc    + 1
      ivoc(inc) = nvoc(j)
      iary(j) = 1
      i      = nca    + 1
      go to 400
c
c......End of parse
c
  800 knc    = inc
      do 900 i=1,knc,1
          kvoc(i) = ivoc(i)
  900 continue
      kwrn   = 0
c
c...End of routine
c
 8000 return
c
c...Invalid users response
c
 9000 kwrn   = 1
      call errmsg ('INVRSP',1,2)
      if (MOTIF .eq. 1) go to 8000
      go to 200
c
c...End of input from user
c
 9100 kwrn   = 1
      go to 8000
c
c...Up arrow
c
 9200 kwrn   = 2
      go to 8000
      end
