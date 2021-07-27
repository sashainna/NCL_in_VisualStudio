C*********************************************************************
C*    NAME         :  cutool2.f
C*       CONTAINS: This files will open/close/ toolib read/get data from the
C*					the old version toolib files. 
C*          toolparms intool  fntool  gtnxtl
C*          findtl  gettdata        gettparms       gettcomds
C*          getline rd2nd
C*
C*    COPYRIGHT 2007 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*      cutool2.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*      04/29/15 , 15:09:47
C********************************************************************/
C
c
c***********************************************************************
c
c   SUBROUTINE:  toolparms (num, cparms)
c
c   FUNCTION:  Retun the command pass in CUTTER/TOOL,tool, parms parameters
c
c   INPUT:  none   
c
c   OUTPUT: num    I*4 D1  -  number of user paramters
c
c           parms		C*n  D20 -  text string for each user defined
c                               parameter.
c           knc     I*4  D20 -  Number of characters per parameter 'parms'.
c
c***********************************************************************
c
      subroutine toolparms (num, cparms, knc)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 num,knc(20),strlen1
c
      character*20 cparms(20)
c
c...Initialize routine
c
      do 50 i=1,20,1
          cparms(i) = ' '
          knc(i) = 0
   50 continue
      num = nctool
      do 80 i=1,nctool,1
          cparms(i) = lctool(i)
          knc(i) = strlen1(lctool(i))
   80 continue
c
c
c...End of routine
c
 8000 return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  intool (clib,knc,kntool,mkerr)
c
c   FUNCTION:  Opens the NCL Tool Library and initializes the tool
c              search routine 'gtnxtl'.
c
c   INPUT:  clib    C*80 D1  -  Name of NCL Tool Library.
c
c           knc     I*4  D1  -  Number of chars in 'clib'.
c
c   OUTPUT: kntool  I*4  D1  -  Number of tools in library.
c
c           kerr    I*4  D1  -  Returns nonzero if an error occurred.
c
c***********************************************************************
c
      subroutine intool (clib,knc,kntool,kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 kntool,kerr,knc
c
      character*(MAX_PATH) clib
c
      character*(MAX_PATH) mlib,fnam
c
      integer*2 i2err,irecl
c
      integer*4 irec
c
c...Open tool library
c
      kerr   = 0
      kntool = 0
      irecl  = 512
      call fparse (clib(1:knc),mlib,' ','.TLB')
      call flopen (scrlun,mlib,'OLD','DIRECT','UNFORMATTED',
     1             irecl,'NULL',i2err)
      if (i2err .ne. 0) then
          i2err  = 0
          call fparse (mlib,fnam,ncdata,'.TLB')
          call flopen (scrlun,fnam,'OLD','DIRECT','UNFORMATTED',
     1                 irecl,'NULL',i2err)
      endif
      if (i2err .ne. 0) go to 9000
c
c...Load the Header record
c
      TVERSION = 2
      I2NDMX = 4
      irec   = 1
      read (scrlun,rec=irec,err=9000) frec
      if (FGREC(57) .lt. 9.149) TVERSION = 0
      if (FGREC(57) .lt. 9.349) TVERSION = 1
      cutlib = sylib
      kntool = frec(26)
c
c...Load the first index record
c
      irec   = frec(1)
      if (irec .eq. 0) go to 9000
      read (scrlun,rec=irec,err=9000) toolix
      toolpt = 5
c
c...End of routine
c
 8000 return
c
c...Error accessing data base
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fntool
c
c   FUNCTION:  Closes the NCL Tool Library. 
c
c   INPUT:  None.
c
c   OUTPUT: None.
c
c***********************************************************************
c
      subroutine fntool
c
      include 'com8a.com'
c
      close (unit=scrlun)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gtnxtl (ktool,cdes,ctype,kerr)
c
c   FUNCTION:  Loads the next tool number and description from the NCL
c              Tool Library.
c
c   INPUT:  none.
c
c   OUTPUT: ktool   I*4  D1  -  Returns the next tool number.
c
c           cdes    C*n  D1  -  Tool description associated with 'ktool'.
c
c				ctype   I*4  D1  -  Returns the cutter type
c
c           kerr    I*4  D1  -  Returns nonzero if an error occurred.
c
c***********************************************************************
c
      subroutine gtnxtl (ktool,cdes,ctype,kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ktool,kerr, ctype
c
      character*40 cdes
c
      integer*4 irec
c
      integer*4 tfbuf(128)
      equivalence (fbuf,tfbuf)
c
c......Load the next index record
c
      kerr   = 0
  100 if (toolpt .gt. 128) then
          irec   = toolix(3)
          if (irec .eq. 0) go to 1000
          read (scrlun,rec=irec,err=9000) toolix
          toolpt = 5
      endif
c
c......Search for requested tool
c
      do 200 toolpt=toolpt,128,2
          if (toolix(toolpt) .eq. -1) go to 1000
          if (toolix(toolpt+1) .ne. 0) then
              read (scrlun,rec=toolix(toolpt+1),err=9000) tfbuf
              if (fbuf(2).eq.0) go to 300
          endif
  200 continue
          go to 100
c
c......Found tool, now load it
c
  300 call rd2nd (kerr)
      if (kerr .ne. 0) go to 9000
      ktool  = fbuf(4)
      cdes   = fcbuf(17:56)
      if (TVERSION.eq.0) call ajtool(fbuf)
      ctype =  fibuf(30)
      toolpt = toolpt + 2
      go to 8000
c
c...Tool not found
c
 1000 ktool  = 0
      go to 8000
c
c...End of routine
c
 8000 return
c
c...Error reading NCL Tool Library
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  findtl (ktool,cdrawn,kary,cary,knc,knary,kerr)
c
c   FUNCTION:  Loads the requested tool number and the user defined
c              parameters from the NCL Tool Library.
c
c   INPUT:  ktool   I*4  D1  -  Tool number to load.
c
c   OUTPUT: cdrawn  C*30 D1  -  Name of Drawing associated with this
c                               tool.
c
c           kary    I*4  D20 -  Array element contains 1 when this user
c                               defined parameter is defined.  Contains
c                               0 if undefined.
c
c           cary    C*n  D20 -  Default text string for each user defined
c                               parameter.
c
c           knc     I*4  D20 -  Number of characters per parameter 'cary'.
c
c           knary   I*4  D1  -  Highest user defined parameter defined.
c
c           kerr    I*4  D1  -  Returns nonzero if an error occurred.
c
c***********************************************************************
c
      subroutine findtl (ktool,cdrawn,kary,cary,knc,knary,kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ktool,kary(20),knc(20),knary,kerr
c
      character*20 cary(20)
      character*30 cdrawn
c
      integer*4 irec,inum,inum1,lpc(20),iunt
c
      data lpc /305,313,321,329,337,345,353,361,369,377,385,393,401,
     1          409,417,425,433,441,449,457/
c
      integer*4 tfbuf(128)
      equivalence (fbuf,tfbuf)
c
c...Load the Header record
c
      kerr   = 0
      knary  = 0
      irec   = 1
      read (scrlun,rec=irec,err=9000) frec
c
c...Load the next index record
c
      irec   = frec(1)
  100 if (irec .eq. 0) go to 9100
      read (scrlun,rec=irec,err=9000) toolix
c
c...Search for requested tool
c
      do 200 i=5,128,2
          if (toolix(i) .eq. -1) go to 9100
          if ((ktool .eq. toolix(i)).and.(toolix(i+1) .ne. 0)) then
              read (scrlun,rec=toolix(i+1),err=9000) tfbuf
              if (fbuf(2).eq.0) go to 300
          endif
  200 continue
      irec   = toolix(3)
      go to 100
c
c...Found tool
c...Load user defined parameters
c
  300 call rd2nd (kerr)
      if (kerr .ne. 0) go to 9000
      iunt   = ifl(264) + 1
      call metool (fgbuf,frec(25),iunt)
      do 400 i=1,20,1
          kary(i) = 0
          knc(i) = 0
  400 continue
c
c......Cutter parameters
c
      do 500 i=1,6,1
          inum   = fibuf(i+56)
          if (inum .ne. 0) then
              call rtoc (fgbuf(i+8),cary(inum),knc(inum))
              kary(inum) = 1
              if (inum .gt. knary) knary = inum
          endif
c
c......Display Cutter parameters
c
          inum   = fibuf(i+88)
          if (inum .ne. 0) then
              call rtoc (fgbuf(i+16),cary(inum),knc(inum))
              kary(inum) = 1
              if (inum .gt. knary) knary = inum
          endif
  500 continue
c
c......Drawing name
c
      cdrawn  = fcbuf(225:250)
c
c......Load tool parameters
c
      do 600 i=1,20,1
          inum   = fibuf(i+232)
          if (inum .gt. 0) then
              inum1   = fibuf(i+128)
              if (inum1 .gt. 0) then
                  cary(inum) = fcbuf(lpc(i):lpc(i)+inum1-1)
                  knc(inum)  = inum1
              else
                  call rtoc (fgbuf(i+38),cary(inum),knc(inum))
              endif
              kary(inum) = 1
              if (inum .gt. knary) knary = inum
          endif
  600 continue
      go to 8000
c
c...End of routine
c
 8000 return
c
c...Error reading NCL Tool Library
c
 9000 kerr   = 1
      go to 8000
c
c...Tool not found
c
 9100 ktool  = 0
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  gettdata (ktool,cbuf,dbuf,cparm, dparm, icfl,ctype,cdrawn,sym, cdes)
c
c   FUNCTION:  Loads the requested tool number and get the tool data
c
c   INPUT:  ktool   I*4  D1  -  Tool number to load.
c
c   OUTPUT: cbuf:    R*8  D6  -  Actual cutter definition.
c
c           dbuf    R*8  D10 -  Display cutter definition.
c           cparm   I*4  D6  -  Actual cutter definition parmeters
c           dparm   I*4  D6  -  Display cutter definition parmeters
c
c           icfl    I*2  D10  -  Cutter display flags.
c				icfl(1)		-  Pseudo cutter flag
c				icfl(2)		-  Display segments flag
c				icfl(3)		-  Moving cutter flag
c				icfl(4)		-  Shaded flag    icfl(7) shank icfl(8): holder 
c				icfl(5)		-  Shank flag (0: No, 1: Yes)
c				icfl(6)		-  holder flag (0: cutter, 1: holder)
c				icfl(7)		-  
c				icfl(8)		-  
c				icfl(9)		-  Number of cutter values defined
c				icfl(10)		-  Number of pseudo values defined
c
c           sym     C*80 D1  -  Cutter display symbol.
c
c				ctype:  I*4  D1  -  Tool cutter type
c
c				cdrawn  C*30 D1  -  Name of Drawing associated with this
c                               tool.
c				cdes: description of the tool
c
c           kerr    I*4  D1  -  Returns nonzero if an error occurred.
c
c***********************************************************************
c
      subroutine gettdata (ktool,cbuf,dbuf,cparm, dparm,
     1        icfl,ctype,cdrawn,sym, cdes)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ktool,kerr,ctype
c
      integer*4 icfl(10), cparm(6), dparm(6)
      real*8 cbuf(6),dbuf(20)
      character*80 sym
      character*30 cdrawn
      character*40 cdes
c
      integer*4 irec,lpc(20),iunt,strlen1
c
      data lpc /305,313,321,329,337,345,353,361,369,377,385,393,401,
     1          409,417,425,433,441,449,457/
c
      integer*4 tfbuf(128)
      equivalence (fbuf,tfbuf)
c
c...Load the Header record
c
      kerr   = 0
      knary  = 0
      irec   = 1
      
      icfl(1) = 0
      icfl(2) = 0
      icfl(3) = 1
      icfl(4) = 0
      icfl(5) = 0
      icfl(6) = 0
      icfl(9) = 0
      icfl(10) = 0
      read (scrlun,rec=irec,err=9000) frec
c
c...Load the next index record
c
      irec   = frec(1)
  100 if (irec .eq. 0) go to 9100
      read (scrlun,rec=irec,err=9000) toolix
c
c...Search for requested tool
c
      do 200 i=5,128,2
          if (toolix(i) .eq. -1) go to 9100
          if ((ktool .eq. toolix(i)).and.(toolix(i+1) .ne. 0)) then
              read (scrlun,rec=toolix(i+1),err=9000) tfbuf
              if (fbuf(2).eq.0) go to 300
          endif
  200 continue
      irec   = toolix(3)
      go to 100
c
c...Found tool
c...Load user defined parameters
c
  300 call rd2nd (kerr)
      if (kerr .ne. 0) go to 9000
      iunt   = ifl(264) + 1
      call metool (fgbuf,frec(25),iunt)
      if (TVERSION.eq.0) then
          call ajtool(fbuf)
      endif
      ctype = fibuf(30)
      icfl(1) = fibuf(31)
      icfl(2) = fibuf(109)
      icfl(3) = fibuf(110)
      icfl(4) = fibuf(32)      
      icfl(9) = fibuf(29)
      icfl(10) = fibuf(63)
      cdes   = fcbuf(17:56)
c
c......Cutter parameters
c
      do 500 i=1,6,1
          cbuf(i) = fgbuf(i+8)
          cparm(i)   = fibuf(i+56)
c
c......Display Cutter parameters
c
          dbuf(i) = fgbuf(i+16)
          dparm(i) = fibuf(i+88)
  500 continue
      dbuf(10) = fgbuf(27)
      dbuf(11) = fgbuf(65)
      dbuf(12) = fgbuf(66)
      dbuf(13) = fgbuf(67)
      dbuf(14) = fgbuf(69)
      dbuf(15) = fgbuf(70)
      dbuf(16) = fgbuf(71)
      dbuf(17) = fgbuf(72)
c
c......Drawing name
c
      cdrawn  = fcbuf(225:250)
c
c...Cutter symbol
c
      sym = fcbuf(193:202)
      if (strlen1(sym) .ne. 0) then
          if (fibuf(96) .eq. 1) then
              icfl(6) = 0     
              icfl(1) = 2
          else if (icfl(1) .eq. 0) then
              icfl(6) = 1     
              icfl(1) = 3
          else if (icfl(1) .eq. 1) then
              icfl(1) = 4
              icfl(6) = 1     
          endif
      endif
c...fibuf(269): Shank params
c...      if (fibuf(269) .ne.0) then
c...cutter
c...         icfl(5) = 1     
c...      else
c...holder
c...         icfl(5) = 0     
c...      endif  
      icfl(5) = fibuf(269)    
c
c...End of routine
c
 8000 return
c
c...Error reading NCL Tool Library
c
 9000 kerr   = 1
      go to 8000
c
c...Tool not found
c
 9100 ktool  = 0
      go to 8000
      end

c***********************************************************************
c
c   SUBROUTINE:  gettparms (ktool, cparms, knc, kerr)
c
c   FUNCTION:  Loads the requested tool number and get the parameter 
c						label record data
c
c   INPUT:  ktool   I*4  D1  -  Tool number to load.
c
c   OUTPUT: 
c           cparms    C*n  D20 -  text string for each user defined
c                               parameter.
c
c           knc     I*4  D20 -  Number of characters per parameter 'cparms'.
c
c           kerr    I*4  D1  -  Returns nonzero if an error occurred.
c
c***********************************************************************
c
      subroutine gettparms (ktool, cparms, knc, kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ktool,knc(20),kerr
c
      character*80 cparms(20)
      integer*4 tlen, linenum, lend, len, nc
      integer*4 irec
      character*80 linestr
      character*492 charstrs
      character*1800 parmstr
      integer*4 pbuf(128)
      equivalence (pbuf(6), charstrs)
c
c...Initialize routine
c
      do 50 i=1,20,1
          cparms(i) = ' '
          knc(i) = 0
   50 continue
c
c...Load the Header record
c
      kerr   = 0
      irec   = 1
      tlen   = 0
      read (scrlun,rec=irec,err=9000) frec
c
c...Load the next index record
c
      irec   = frec(1)
  100 if (irec .eq. 0) go to 9100
      read (scrlun,rec=irec,err=9000) toolix
c
c...Search for requested tool
c
      do 200 i=5,128,2
          if (toolix(i) .eq. -1) go to 9100
          if ((ktool .eq. toolix(i)).and.(toolix(i+1) .ne. 0)) then
              read (scrlun,rec=toolix(i+1),err=9000) pbuf
              if (pbuf(2).eq.1) then
                  len = pbuf(5) - 20
                  if (tlen.gt.0) then
                      parmstr = parmstr(1:tlen)//charstrs(1:len)
                      tlen = len + tlen
                  else
                      parmstr = charstrs(1:len)
                      tlen = len
                  endif
                  if (pbuf(3).eq.0) goto 300
              endif
          endif
  200 continue
      irec   = toolix(3)
      go to 100
c
  300 linenum = 1
  400 call getline(parmstr, tlen, linenum, linestr, nc, lend)
      if (lend .ne. 0) goto 8000 
      cparms(linenum) = linestr
      knc(linenum) = nc
      linenum = linenum + 1
      if (linenum .lt. 20) goto 400
c
c...End of routine
c
 8000 return
c
c...Error reading NCL Tool Library
c
 9000 kerr   = 1
      go to 8000
c
c...Tool not found
c
 9100 ktool  = 0
      go to 8000
      end

c***********************************************************************
c
c   SUBROUTINE:  gettcomds (ktool, comds, knc, comnum, kerr)
c
c   FUNCTION:  Loads the requested tool number and get the  
c						commands record data
c
c   INPUT:  ktool   I*4  D1  -  Tool number to load.
c
c   OUTPUT: 
c           comds    C*n  D200 -  text string for each ncl command
c
c           knc     I*4  D20 -  Number of characters per command 'comds'.
c
c				comnum  I*4  D1  -  Number of commands
c
c           kerr    I*4  D1  -  Returns nonzero if an error occurred.
c
c***********************************************************************
c
      subroutine gettcomds (ktool, comds, knc, comnum, kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ktool,knc(200),kerr, comnum
c
      character*80 comds(200)
      integer*4 tlen, linenum, lend, len, nc
      integer*4 irec
      character*80 linestr
      character*492 charstrs
      character*16000 comdstr
      integer*4 testrec(128)
      equivalence (testrec(6), charstrs)
c
c...Load the Header record
c
      kerr   = 0
      irec   = 1
      read (scrlun,rec=irec,err=9000) frec
c
c...Load the next index record
c
      irec   = frec(1)
  100 if (irec .eq. 0) go to 9100
      read (scrlun,rec=irec,err=9000) toolix
c
c...Search for requested tool
c
      tlen = 0
      do 200 i=5,128,2
          if (toolix(i) .eq. -1) go to 9100
          if ((ktool .eq. toolix(i)).and.(toolix(i+1) .ne. 0)) then
              read (scrlun,rec=toolix(i+1),err=9000) testrec
              if (testrec(2).eq.2) then
                  len = testrec(5) - 20
                  if (tlen.gt.0) then
                      comdstr = comdstr(1:tlen)//charstrs(1:len)
                      tlen = len + tlen
                  else
                      comdstr = charstrs(1:len)
                      tlen = len
                  endif
                  if (testrec(3).eq.0) goto 300
              endif
          endif
  200 continue
      irec   = toolix(3)
      go to 100
c
  300 linenum = 1
  400 call getline(comdstr, tlen, linenum, linestr, nc, lend)
      if (lend .ne. 0) goto 8000 
      comds(linenum) = linestr
      knc(linenum) = nc
      linenum = linenum + 1
      goto 400
c
c...End of routine
c
 8000 comnum = linenum - 1
      return
c
c...Error reading NCL Tool Library
c
 9000 kerr   = 1
      go to 8000
c
c...Tool not found
c
 9100 ktool  = 0
      go to 8000
      end

c
c***********************************************************************
c
c   SUBROUTINE:  getline(longstr, tlen, linenum, linestr, nc, lend)
c
c   FUNCTION:  This routine parses a character string and get a line
c        line seperated by '\n' character
c
c   INPUT:  longstr    C*n  D1  -  Input line.
c
c           tnc     I*4  D1  -  Number of characters in 'longstr'.
c       linenum:         - line number
c
c
c   OUTPUT: linestr    C*n  D1  -  Returned line string.
c
c           nc    C*n  D1  -  Number of characters in 'linestr'
c
c           lend    I*4  D1  -  Returns 1 if no such line
c
c***********************************************************************
c
      subroutine getline(longstr, tlen, linenum, linestr, nc, lend)
c
      character*(*)  longstr, linestr
      integer*4 i, j, tlen, nc, lend, linenum

      i = 1
      j = 1
      linestr = ' '
      nc = 0
      do while (i.lt.linenum)
          do while (longstr(j:j).ne.char(10))
              j = j + 1
              if ((j.gt.tlen).or.(longstr(j:j).eq.char(0))) then
                  lend = 1
                  return
              endif
          end do
          j = j + 1
          i = i + 1
      end do
      if (j.gt.tlen) then
          lend = 1
          return
      endif
      i = 1
      do while (longstr(j:j).ne.char(10))
          if ((j.gt.tlen).or.(longstr(j:j).eq.char(0))) then
              lend = 0
              return
          endif
          linestr(i:i) = longstr(j:j)
          j = j + 1
          nc = nc + 1
          i = i + 1
      end do
c
c...remove '\r' if have any
c
      if (linestr(nc:nc).eq.char(13)) then
          linestr(nc:nc) = ' '
          nc = nc - 1
      endif
      lend = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rd2nd (kerr)
c
c   FUNCTION:  Reads the continuation record from the tool library.
c
c   INPUT:  none.
c
c   OUTPUT: kerr    I*4  D1  -  Returns non-zero if an error occurs
c                               reading from the file.
c
c***********************************************************************
c
      subroutine rd2nd (kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 kerr
c
      integer*4 i,inc,tmpbuf(128)
c
c...Initialize routine
c
      kerr = 0
c
c...Read continuation record
c
      if (TVERSION .eq. 2) then
          read (scrlun,rec=FBUF(127),err=9000) tmpbuf
          inc    = 128 / I2NDMX * (FBUF(128)-1)
          do 250 i=1,128/I2NDMX,1
              FBUF(128+i) = tmpbuf(inc+i)
  250     continue
c
c...Older version of library
c...Initialize new variables
c
      else
          FBUF(127) = 0
          FBUF(128) = 0
          FGBUF(65) = 0.
          FGBUF(66) = 0.
          FGBUF(67) = 0.
          FIBUF(269) = 0
          FGBUF(69) = 0.
          FGBUF(70) = 0.
          FGBUF(71) = 0.
          FGBUF(72) = 0.
      endif
c
c...End of routine
c
 8000 return
c
c...Error reading library
c
 9000 kerr = 1
      go to 8000
      end


c***********************************************************************
c
c   SUBROUTINE:  nclf_readhead (create_date, create_time, mod_data, mod_time, 
c			units, &vers, cmsg, kerr);
c
c   FUNCTION:  This routine get the header info of the Tool Library.
c
c   INPUT:  none
c
c   OUTPUT: create_date,   create_time:  toolib create time
c           mod_data, mod_time:		toolib midify date and time
c           units:    toolib unit using
c           vers: version number
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine nclf_readhead (create_date, create_time, moddate, 
     1            modtime, units, vers, cmsg, kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 kerr, units
      real*8 vers
c
      character*(12) create_date, create_time, moddate, modtime
      character*(80) cmsg
c


      integer*4 irec
c
c......Load the Header record
c
      irec   = 1
      read (scrlun,rec=irec,err=9000) frec
      vers = FGREC(57)
      create_date(1:12) = FCREC(25:36)
      create_time(1:12) = FCREC(37:48)
      moddate(1:12) = FCREC(49:60)
      modtime(1:12) = FCREC(61:72)
      units = 1
c
c...End of routine
c
 8000 return
c
c...Error reading library
c
 9000 kerr = 1
      go to 8000
      end
            
c
c***********************************************************************
c
c   SUBROUTINE:  nclf_readtl_info (toolno, low_tool, high_tl)
c
c   FUNCTION:  This routine get the tool info of the Tool Library.
c
c   INPUT:  none
c
c   OUTPUT: toolno: total tool number
c           low_tool: low tool number
c           high_tl:  high  tool number
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine nclf_readtl_info (toolno, low_tool, high_tl)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 low_tool, high_tl, toolno
      integer*4 kerr
c
      integer*4 irec
      integer*4 idx, tool
c
      toolno = 0
      tool = 0
c
c...Try to find the lower/high tool numbers
c
c...lowest toolnum
      low_tool = 1000000
c...highest toolnum
      high_tl = 0

c
c...Load the Header record
c
      kerr   = 0
      knary  = 0
      irec   = 1
      read (scrlun,rec=irec,err=9000) frec
c
c......Load the next index record
c
      irec   = frec(1)
  100 if (irec .eq. 0) go to 8000
      read (scrlun,rec=irec,err=9000) toolix
      if (kerr .ne. 0) go to 8000
c
c...Read first index record
c
      idx = 3
c
c...Check for 1 complete loop through file
c...when starting search from the current record
c
  200 idx   = idx   + 2
      if (idx .gt. 127) then
          if (TOOLIX(3) .eq. 0) go to 8000
          read (scrlun,rec=TOOLIX(3),err=9000) toolix
          idx   = 5
      endif
c
c......At end of this index record
c......Read next index record
c
      if (toolix(idx) .eq. -1) go to 8000
      if (low_tool.gt.toolix(idx)) low_tool = toolix(idx)
      if (high_tl.lt.toolix(idx)) high_tl = toolix(idx)
      if (tool .eq. toolix(idx)) goto 200
      toolno = toolno + 1
      tool = toolix(idx)
      go to 200
c
c...End of routine
c
 8000 return
c
c...Error reading from library
c
 9000 ifl(2) = 485
      kerr   = -1
      go to 8000
      end
c
c
 
c***********************************************************************
c
c   SUBROUTINE: nclf_rdlib_disp(tlib, disp, slib, cmsg, kerr)
c
c   FUNCTION: get tool library decription
c
c   INPUT:  none
c
c   OUTPUT: tlib: toolib name
c                 disp: description
c                 slib: symlib
c
c***********************************************************************
c
      subroutine nclf_rdlib_disp(tlib, nc1, disp,nc2, slib, nc3, 
     1                        cmsg, kerr)
c
      include 'com8a.com'
      include 'cutter.com'
      integer*4 nc1, nc2, nc3
      integer*4 strlen1
      character*(256) tlib
      character*22 slib
      character*42 disp
      character*(80) cmsg
      character*22 slib2
      character*40 tdisp
      character*256 lnam
      equivalence (slib2,FIREC(38))
      equivalence (tdisp,FREC(31)), (lnam,FREC(41))
c
c...Load the Header record
c
      kerr   = 0
      knary  = 0
      irec   = 1
      read (scrlun,rec=irec,err=9000) frec

      tlib = lnam(1:256)
      nc1 = strlen1(lnam)

      slib = slib2
      nc3 = FIREC(37)

      disp = tdisp
      nc2 = strlen1(tdisp)
c
c...End of routine
c
 8000 return
c
c...Error reading from library
c
 9000 ifl(2) = 485
      kerr   = -1
      go to 8000
      end

c***********************************************************************
c
c   SUBROUTINE:  getloadtl (ktool, loadtl, knc, kerr)
c
c   FUNCTION:  Loads the requested tool number and get the load command 
c						record data
c
c   INPUT:  ktool   I*4  D1  -  Tool number to load.
c
c   OUTPUT: 
c           loadtl    C*n  D20 -  text string for each load command
c
c           knc     I*4  D20 -  Number of characters per parameter 'loadtl'.
c
c           kerr    I*4  D1  -  Returns nonzero if an error occurred.
c
c***********************************************************************
c
      subroutine getloadtl (ktool, major, mnc, loadtl, knc, parms, kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 ktool,knc(20),kerr, mnc, parms(20)
c
      character*22 loadtl(20)
      integer*4 tlen, nc
      integer*4 irec, lpc(20),strlen1
      character*8 major
      data lpc /305,313,321,329,337,345,353,361,369,377,385,393,401,
     1          409,417,425,433,441,449,457/
c
c...Initialize routine
c
      do 50 i=1,20,1
          loadtl(i) = ' '
          knc(i) = 0
          parms(i) = 0
   50 continue
c
c...Load the Header record
c
      kerr   = 0
      irec   = 1
      tlen   = 0
      read (scrlun,rec=irec,err=9000) frec
c
c...Load the next index record
c
      irec   = frec(1)
  100 if (irec .eq. 0) go to 9100
      read (scrlun,rec=irec,err=9000) toolix
c
c...Search for requested tool
c
      do 200 i=5,128,2
          if (toolix(i) .eq. -1) go to 9100
          if ((ktool .eq. toolix(i)).and.(toolix(i+1) .ne. 0)) then
              read (scrlun,rec=toolix(i+1),err=9000) tfbuf
              if (fbuf(2).eq.0) go to 300
          endif
  200 continue
      irec   = toolix(3)
      go to 100
c
c...Found tool
c...Load command 
c
  300 call rd2nd (kerr)
      if (kerr .ne. 0) go to 9000
c
c......Load Tool Parameters
c
      major = fcbuf(297:302)
      mnc = strlen1(major)
      do 500 i=1,20,1
          nc     = fibuf(i+128)
          if (nc .eq. 0) then
              loadtl(i) = ' '
          else if (nc .eq. -1) then
              call rtoc (fgbuf(i+38), loadtl(i), nc)
          else if (nc .eq. -2) then
              call rtoc (fgbuf (i+38), loadtl(i), nc)
              loadtl(i) = '&' // loadtl(i)(1:nc)
              nc = nc     + 1
          else
              loadtl(i) = fcbuf(lpc(i):lpc(i)+nc-1)
          endif
          knc(i) = nc
          parms(i) = fibuf(i+232)
  500 continue
c
c...End of routine
c
 8000 return
c
c...Error reading NCL Tool Library
c
 9000 kerr   = 1
      go to 8000
c
c...Tool not found
c
 9100 ktool  = 0
      go to 8000
      end

 
c
c***********************************************************************
c
c   SUBROUTINE:  intool2 (kntool,mkerr)
c
c   FUNCTION:  initializes the tool for
c              search routine 'gtnxtl'.
c
c   INPUT:  none
c
c   OUTPUT: kntool  I*4  D1  -  Number of tools in library.
c
c           kerr    I*4  D1  -  Returns nonzero if an error occurred.
c
c***********************************************************************
c
      subroutine intool2 (kntool,kerr)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 kntool,kerr
c
      integer*4 irec
c
c...Open tool library
c
      kerr   = 0
      kntool = 0
c
c...Load the Header record
c
      TVERSION = 2
      I2NDMX = 4
      irec   = 1
      read (scrlun,rec=irec,err=9000) frec
      if (FGREC(57) .lt. 9.149) TVERSION = 0
      if (FGREC(57) .lt. 9.349) TVERSION = 1
      kntool = frec(26)
c
c...Load the first index record
c
      irec   = frec(1)
      if (irec .eq. 0) go to 9000
      read (scrlun,rec=irec,err=9000) toolix
      toolpt = 5
c
c...End of routine
c
 8000 return
c
c...Error accessing data base
c
 9000 kerr   = 1
      go to 8000
      end
c
c
c***********************************************************************
c
c   SUBROUTINE:  stllib (clib,knc)
c
c   FUNCTION:  Interface to C routines which sets the name of the
c              NCL Tool Library.
c
c   INPUT:  clib    C*80 D1  -  Name of NCL Tool Library.
c
c           knc     I*4  D1  -  Number of chars in 'clib'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine stllib (clib,knc)
c
      include 'com.com'
      include 'cutter.com'
c
      integer*4 knc
c
      character*(MAX_PATH) clib
c
      toolib = clib(1:knc)
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  setncsfl5 (flag)
c
c   FUNCTION:  Set ncsfl(5)
c
c   INPUT:  flag: new value of ncsfl(5)
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine setncsfl5 (flag)
c
      include 'com.com'
c
      integer*2 flag
c
      ncsfl(5) = flag
      return
      end
