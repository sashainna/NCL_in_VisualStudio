c
c***********************************************************************
c
c   FILE NAME:  mchsim.for
c   CONTAINS:
c               mchsim  simmdl  simmil  simlth  simbld  simtrn  simmch
c               simaxs  simcop  simdat  simviw
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mchsim.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:24:15
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  mchsim (kask,cdir,cmsg,kerr)
c
c   FUNCTION:  Creates a Machine Simulator machine description data
c              file.
c
c   INPUT:  kask    I*4  D1  -  0 = First time here.  Return error if
c                               file already exists, otherwise over-
c                               write it.
c
c           cdir    C*n  D1  -  Name of machine simulator data direc-
c                               tory.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine mchsim (kask,cdir,cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 kerr,kask
c
      character*(*) cdir,cmsg
c
      integer*4 irecl,inc,nindex
c
      character*20 att(4)
      character*80 tbuf
      character*(MAX_PATH) fnam,ldev,fdir
      character*(MAX_FILE) lext
c
c...Load Header text from .PDF file
c
      call pdfhed
      inc    = nindex(DHED(5),' ')
      if (inc .gt. 1) then
          tbuf   = DHED(5)(inc:)
          DHED(5) = tbuf
          NCHED(5) = NCHED(5) - inc + 1
      endif
  100 inc    = index(DHED(5),' ')
      if (inc .ne. 0) then
          DHED(5)(inc:inc) = '_'
          go to 100
      endif
c
c...Construct the machine data directory
c
cc      ldev   = cdir
      lext   = ' '
      ldev   = ' '
      call fparse (ldev,ldev,cdir,lext)
      if (ldev .eq. ' ') then
          ldev   = './machines/'
      endif
      call fparse (LMNAME,fnam,ldev,lext)
      call fparse ('pworks',fdir,ldev,lext)
c
c...Check to see if file exists
c
      irecl  = 80
      if (kask .eq. 0) then
          att(1) = 'direct'
          att(2) = 'none'
          att(3) = 'unformatted'
          att(4) = 'old'
          call opnfil (LUNSC1,fnam,att,irecl,cmsg,kerr)
          call clsfil (LUNSC1)
c
c...File does not exist
c...Let's create it
c
          if (kerr .eq. -2) then
              cmsg   = fnam
              kerr   = 2
              go to 8000
c
c...File already exists
c...Ask user to for permission
c...to overwrite it
c
          else
              cmsg   = fnam
              kerr   = 1
              go to 8000
          endif
      endif
c
c...Create the Machine Model File
c
      call simmdl (fdir,fnam,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Create the Machine Data File
c
      call simmch (fdir,fnam,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Create the Axis Data Files
c
      call simaxs (fdir,fnam,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 call clsfil (LUNSC1)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simmdl (cdir1,cdir2,cmsg,kerr)
c
c   FUNCTION:  Creates the Machine Model File.
c
c   INPUT:  cdir1   C*n  D1  -  Name of machine simulator demo direc-
c                               tory in which are stored the various
c                               sample machine models.
c
c           cdir2   C*n  D1  -  Name of machine simulator directory in
c                               which the machine model for this MDF
c                               file will be stored.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simmdl (cdir1,cdir2,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201))
c
      integer*4 MACHTP
c
      integer*4 kerr
c
      character*(*) cmsg,cdir1,cdir2
c
      integer*4 irecl
c
      character*20 att(4)
      character*(MAX_PATH) fnam
c
c...Open the Machine Model file
c
      call fparse ('postworks',fnam,cdir2,'.mdl')
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 80
      call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Milling machine
c
      if (MACHTP .eq. 1) then
          call simmil (cdir1,cmsg,kerr)
c
c...Lathe
c
      else if (MACHTP .eq. 2) then
          call simlth (cdir1,cdir2,cmsg,kerr)
c
c...Blade
c
      else if (MACHTP .eq. 3) then
          call simbld (cdir1,cdir2,cmsg,kerr)
c
c...Mill/Turn
c
      else if (MACHTP .eq. 4) then
          call simtrn (cdir1,cdir2,cmsg,kerr)
      endif
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simmil (cdir1,cmsg,kerr)
c
c   FUNCTION:  Creates a Mill Machine Model File.
c
c   INPUT:  cdir1   C*n  D1  -  Name of machine simulator demo direc-
c                               tory in which are stored the various
c                               sample machine models.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simmil (cdir1,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (IRTYPE,KPOSMP(1486)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTNUM,IRTYPE(20),IRTWRK(20)
c
      integer*4 kerr
c
      character*(*) cmsg,cdir1
c
      integer*4 i,nc,inum,ntab,npiv
c
      character*1 laxs(3),ttyp(4),ptyp(4)
      character*20 lnum
      character*80 lfil
c
      data laxs /'x','y','z'/
c
c...Copy the Base Model machine
c
      call simcop (cdir1,'millbase.mdl',0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Get type of rotary axes on machine
c
      ntab   = 0
      npiv   = 0
      do 100 i=1,IRTNUM,1
          if (IRTYPE(i) .eq. 1 .and. ntab .lt. 2) then
              ntab   = ntab   + 1
              ttyp(ntab) = laxs(IRTWRK(i))
          else if (IRTYPE(i) .eq. 2 .and. npiv .lt. 2) then
              npiv   = npiv   + 1
              ptyp(npiv) = laxs(IRTWRK(i))
          endif
  100 continue
c
c...Copy the X-axis and any rotary tables
c...Build the filename first
c
      if (ntab .eq. 0) then
          lfil   = 'm3t.dat'
      else
          inum   = ntab   + 3
          call itoc (inum,lnum,nc,0)
          lfil   = 'm' // lnum(1:nc) // 't' // ttyp(ntab)
          nc     = nc     + 2
          do 200 i=ntab,1,-1
              lfil(nc+1:) = ttyp(i)
              nc     = nc     + 1
  200     continue
          lfil(nc+1:) = '.dat'
      endif
      call simcop (cdir1,lfil,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Copy the Z-axis and any rotary heads
c...Build the filename first
c
      if (npiv .eq. 0) then
          lfil   = 'm3h.dat'
      else
          inum   = npiv   + 3
          call itoc (inum,lnum,nc,0)
          lfil   = 'm' // lnum(1:nc) // 'h' // ttyp(ntab)
          nc     = nc     + 2
          do 300 i=1,npiv,1
              lfil(nc+1:) = ptyp(i)
              nc     = nc     + 1
  300     continue
          lfil(nc+1:) = '.dat'
      endif
      call simcop (cdir1,lfil,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simlth (cdir1,cdir2,cmsg,kerr)
c
c   FUNCTION:  Creates a Lathe Machine Model File.
c
c   INPUT:  cdir1   C*n  D1  -  Name of machine simulator demo direc-
c                               tory in which are stored the various
c                               sample machine models.
c
c           cdir2   C*n  D1  -  Name of machine simulator directory in
c                               which the machine data for this MDF
c                               file will be stored.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simlth (cdir1,cdir2,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (TURFL ,KPOSMP(1824))
c
      integer*4 TURFL(2)
c
      integer*4 kerr
c
      character*(*) cmsg,cdir1,cdir2
c
      integer*4 i,nfil,irecl
c
      character*20 att(4),lfil(5)
      character*(MAX_PATH) fnam
c
      data nfil /5/
      data lfil /'lbase.stk', 'lspindle.stk', 'ltail.stk',
     1           'lxaxis.stk', 'lzaxis.stk'/
c
c...Copy the Base Model machine
c
      call simcop (cdir1,'lathbase.mdl',0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Copy the X-axis based on the number of turrets
c
      if (TURFL(1) .eq. 1) then
          call simcop (cdir1,'l1t.dat',0,cmsg,kerr)
      else
          call simcop (cdir1,'l2t.dat',0,cmsg,kerr)
      endif
c
c...Copy the stock files
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 80
      do 100 i=1,nfil,1
          call clsfil (LUNSC2)
          call fparse (lfil(i),fnam,cdir2,'.txt')
          call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call simcop (cdir1,lfil(i),0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simbld (cdir1,cdir2,cmsg,kerr)
c
c   FUNCTION:  Creates a Ultrasonic Blade Machine Model File.
c
c   INPUT:  cdir1   C*n  D1  -  Name of machine simulator demo direc-
c                               tory in which are stored the various
c                               sample machine models.
c
c           cdir2   C*n  D1  -  Name of machine simulator directory in
c                               which the machine data for this MDF
c                               file will be stored.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simbld (cdir1,cdir2,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kerr
c
      character*(*) cmsg,cdir1,cdir2
c
      integer*4 i,nfil,irecl
c
      character*20 att(4),lfil(5)
      character*(MAX_PATH) fnam
c
      data nfil /5/
      data lfil /'bcaxis1.txt', 'blade.txt', 'bslide1.txt',
     1           'bslide2.txt', 'bzaxis1.txt'/
c
c...Copy the Base Model machine
c
      call simcop (cdir1,'bladbase.mdl',0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Copy the Sweep text files
c

      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 80
      do 100 i=1,nfil,1
          call clsfil (LUNSC2)
          call fparse (lfil(i),fnam,cdir2,'.txt')
          call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call simcop (cdir1,lfil(i),0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simtrn (cdir1,cdir2,cmsg,kerr)
c
c   FUNCTION:  Creates a Mill/Turn Machine Model File.
c
c   INPUT:  cdir1   C*n  D1  -  Name of machine simulator demo direc-
c                               tory in which are stored the various
c                               sample machine models.
c
c           cdir2   C*n  D1  -  Name of machine simulator directory in
c                               which the machine data for this MDF
c                               file will be stored.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simtrn (cdir1,cdir2,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
c
      integer*4 NUMLIN(3),IRTNUM
c
      integer*4 kerr
c
      character*(*) cmsg,cdir1,cdir2
c
      integer*4 i,nfil,irecl
c
      character*20 att(4),lfil(7)
      character*(MAX_PATH) fnam
c
      data nfil /7/
      data lfil /'tbase.stk', 'tbaxis.stk', 'tspindle.stk', 'ttail.stk',
     1           'txaxis.stk', 'tyaxis.stk', 'tzaxis.stk'/
c
c...Copy the Base Model machine
c
      call simcop (cdir1,'turnbase.mdl',0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Copy the Milling Axes based on the supported axes
c
      if (NUMLIN(2) .ne. 0 .and. IRTNUM .ge. 2) then
          call simcop (cdir1,'t5thy.dat',0,cmsg,kerr)
      else if (NUMLIN(2) .ne. 0) then
          call simcop (cdir1,'t4ty.dat',0,cmsg,kerr)
      else if (IRTNUM .ge. 2) then
          call simcop (cdir1,'t4th.dat',0,cmsg,kerr)
      else
          call simcop (cdir1,'t3t.dat',0,cmsg,kerr)
      endif
c
c...Copy the stock files
c
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 80
      do 100 i=1,nfil,1
          if (IRTNUM .lt. 2 .and. i .eq. 2) go to 100
          if (NUMLIN(2) .eq. 0 .and. i .eq. 6) go to 100
          call clsfil (LUNSC2)
          call fparse (lfil(i),fnam,cdir2,'.txt')
          call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call simcop (cdir1,lfil(i),0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simmch (cdir1,cdir2,cmsg,kerr)
c
c   FUNCTION:  Creates the Machine Data File.
c
c   INPUT:  cdir1   C*n  D1  -  Name of machine simulator demo direc-
c                               tory in which are stored the various
c                               sample machine models.
c
c           cdir2   C*n  D1  -  Name of machine simulator directory in
c                               which the machine data for this MDF
c                               file will be stored.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simmch (cdir1,cdir2,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201))
c
      integer*4 MACHTP
c
      integer*4 kerr
c
      character*(*) cmsg,cdir1,cdir2
c
      integer*4 irecl
c
      character*20 att(4)
c
      character*(MAX_PATH) fnam
      character*(MAX_FILE) lfil
c
c...Initialize routine
c
      lfil   = 'machine.dat'
c
c...Open the Machine Data file
c
      call fparse (lfil,fnam,cdir2,' ')
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'new'
      irecl  = 80
      call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Copy the Machine Data File
c
      call simcop (cdir1,lfil,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Copy the Viewing file
c
      if (MACHTP .eq. 2) then
          call simcop (cdir1,'lathview.dat',0,cmsg,kerr)
      else if (MACHTP .eq. 3) then
          call simcop (cdir1,'bladview.dat',0,cmsg,kerr)
      else
          call simcop (cdir1,'millview.dat',0,cmsg,kerr)
      endif
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simaxs (cdir1,cdir2,cmsg,kerr)
c
c   FUNCTION:  Creates the Axis Data Files.
c
c   INPUT:  cdir1   C*n  D1  -  Name of machine simulator demo direc-
c                               tory in which are stored the various
c                               sample machine models.
c
c           cdir2   C*n  D1  -  Name of machine simulator directory in
c                               which the machine data for this MDF
c                               file will be stored.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simaxs (cdir1,cdir2,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (MACHTP,KPOSMP(1201))
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
c
      integer*4 NUMLIN(6),IRTNUM,MOTREG(24),MACHTP
c
      equivalence (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
c
      integer*4 kerr
c
      character*(*) cmsg,cdir1,cdir2
c
      integer*4 irecl,iact,i,inc(10),isub(6),ireg(10)
c
      character*20 att(4)
      character*(MAX_PATH) fnam
      character*(MAX_FILE) lfil
c
      data inc /1,1, 2,2, 3,3, 1,2,3,4/
      data isub /1,2, 1,2, 1,2/
      data ireg /1,1, 5,5, 9,9, 14, 17, 20, 23/
c
c...Loop through each defined axis
c
      do 500 i=1,10,1
          iact   = 0
          if (i .le. 6) then
              if (NUMLIN(inc(i)) .ge. isub(i)) iact = 1
          else
              if (inc(i) .le. IRTNUM .or.
     1            (i .eq. 10 .and. MACHTP .eq. 3)) iact = 1
          endif
          if (iact .eq. 1) then
c
c...Open the Machine Data file
c
              call touppr (REGST(MOTREG(ireg(i)))(1:1),lfil)
              call fparse (lfil,fnam,cdir2,'.dat')
              att(1) = 'sequential'
              att(2) = 'list'
              att(3) = 'formatted'
              att(4) = 'new'
              irecl  = 80
              call opnfil (LUNSC2,fnam,att,irecl,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
c...Copy the Axis Data File
c
              call simcop (cdir1,'axis.dat',i,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
  500 continue
c
c...End of routine
c
 8000 call clsfil (LUNSC2)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simcop (cdir,cfil,kaxs,cmsg,kerr)
c
c   FUNCTION:  Creates a Machine Dependant data file from a template
c              file.  The file being created should already be opened
c              using LUNSC2.
c
c   INPUT:  cdir    C*n  D1  -  Directory in which the template file
c                               exists.
c
c           cfil    C*n  D1  -  Name of machine template data file.
c
c           kaxs    I*4  D1  -  Axis data file currently creating (1-10).
c                               A value of 0 states that this is not
c                               an axis data file.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simcop (cdir,cfil,kaxs,cmsg,kerr)
c
      include 'menu.inc'
c
      integer*4 kerr,kaxs
c
      character*(*) cmsg,cdir,cfil
c
      integer*4 irecl,nc1,nc2,strlen1
c
      character*20 att(4)
      character*80 ldat1,ldat2
      character*(MAX_PATH) fnam
c
c...Open the Machine template file
c
      call fparse (cfil,fnam,cdir,' ')
      att(1) = 'sequential'
      att(2) = 'list'
      att(3) = 'formatted'
      att(4) = 'old'
      irecl  = 80
      call opnfil (LUNSC3,fnam,att,irecl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Read the next data line
c
  100 call rdtxt (LUNSC3,ldat1,cmsg,kerr)
      if (kerr .eq. 1) then
          kerr   = 0
          go to 8000
      endif
      if (kerr .ne. 0) go to 8000
c
c...Parse the data line
c
      nc1    = strlen1(ldat1)
      call simdat (ldat1,nc1,kaxs,ldat2,nc2)
c
c...Write the formatted data record
c
      call wrtxt (LUNSC2,ldat2,nc2,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 100
c
c...End of routine
c
 8000 call clsfil (LUNSC3)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simdat (cinp,knci,kaxs,cout,knco)
c
c   FUNCTION:  Parses the input line looking for post-processor variables
c              and replaces them with their values.
c
c   INPUT:  cinp    C*n  D1  -  Input line to parse.
c
c           knci    I*4  D1  -  Number of chars in 'cinp'.
c
c           kaxs    I*4  D1  -  Axis data file currently creating (1-10).
c                               A value of 0 states that this is not
c                               an axis data file.
c
c   OUTPUT: cout    C*n  D1  -  Parsed output line.
c
c           knco    I*4  D1  -  Number of chars in 'cout'.
c
c***********************************************************************
c
      subroutine simdat (cinp,knci,kaxs,cout,knco)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (MOTREG,KPOSMP(0381)), (MACHTP,KPOSMP(1201))
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IRTYPE,KPOSMP(1486)), (IRTWRK,KPOSMP(1506))
      equivalence (TLCCD ,KPOSMP(1839))
c
      integer*4 MCHOPT(20),TLCCD(20),IRTYPE(20),IRTNUM,IRTWRK(20),
     1          NUMLIN(6),MOTREG(24),MACHTP
c
      equivalence (HOME  ,POSMAP(0151)), (LIMITS,POSMAP(1254))
      equivalence (PPINCR,POSMAP(1443)), (FEDLMT,POSMAP(3528))
      equivalence (SPIVEC,POSMAP(3583))
      equivalence (TLCTIM,POSMAP(3962)), (MAXTN ,POSMAP(3995))
c
      real*8 PPINCR(10),SPIVEC(3),TLCTIM,MAXTN,LIMITS(2,10),HOME(10),
     1       FEDLMT(10)
c
      equivalence (LMNAME,CPOSMP(0141)), (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
      character*40 LMNAME
c
      integer*4 kaxs,knci,knco
c
      character*(*) cinp,cout
c
      integer*4 isub(6),strlen1,nci,nc,ist,ien,inum,inc,iprev,i,
     1          jinc(10),nc1,asub(6),ireg(10),ipt
c
      real*8 rnum
c
      character*1 laxs(9)
      character*10 llin(6)
      character*20 lnum
      character*80 lbuf,tbuf,lvar
c
      data jinc /1,1, 2,2, 3,3, 1,2,3,4/
      data isub /1,2, 1,2, 1,2/
      data asub /1,1, 3,3, 5,5/
c
      data ireg /1,1, 5,5, 9,9, 14, 17, 20, 23/
      data laxs /'X','U','Y','V','Z','W','A','B','C'/
      data llin /'Primary','Secondary','Primary','Secondary','Primary',
     1           'Secondary'/
c
c...Initialize routine
c
      ien    = 0
      lbuf   = cinp
      nci    = knci
c
c...Find start of variable
c
  100 ist    = ien    + 1
      inc    = ist    + index(lbuf(ist:),'[') - 1
      if (inc .le. ist) go to 7000
      ist    = inc
c
c...Find end of variable
c
      ien    = ist    + index(lbuf(ist:),']') - 1
      if (ien .le. ist) go to 7000
c
c...Variable found, find its value
c
      lvar   = lbuf(ist+1:ien-1)
      call getvnm (lvar,inum,MENWRD,MENWVL,NMENWD)
c
c......ACCURACY
c
      if (inum .eq. 501 .and. kaxs .ne. 0) then
          call rtoc (PPINCR(kaxs),lvar,nc)
c
c......AXES
c
      else if (inum .eq. 502) then
          lvar   = ' '
          nc     = 0
          do 400 i=1,10,1
              if (i .le. 6) then
                  if (NUMLIN(jinc(i)) .ge. isub(i)) then
c                      lvar(nc+1:) = laxs(i) // ' '
                      lvar(nc+1:) = REGST(MOTREG(ireg(i)))(1:1) // ' '
                      nc     = nc     + 2
                  endif
              else
                  if (jinc(i) .le. IRTNUM .or.
     1                (i .eq. 10 .and. MACHTP .eq. 3)) then
c                      lvar(nc+1:) = laxs(IRTWRK(i-6)+6)
                      lvar(nc+1:) = REGST(MOTREG(ireg(i)))(1:1) // ' '
                      nc     = nc     + 2
                  endif
              endif
  400     continue
c
c......AXIS
c
      else if (inum .eq. 503 .and. kaxs .ne. 0) then
          if (kaxs .le. 6) then
              if (NUMLIN(jinc(kaxs)) .ge. isub(kaxs)) then
c                  lvar   = laxs(kaxs)
                  lvar   = REGST(MOTREG(ireg(kaxs)))(1:1)
                  nc     = 1
              endif
          else
              if (jinc(kaxs) .le. IRTNUM .or.
     1            (kaxs .eq. 10 .and. MACHTP .eq. 3)) then
c                  lvar   = laxs(IRTWRK(kaxs-6)+6)
                  lvar   = REGST(MOTREG(ireg(kaxs)))(1:1)
                  nc     = 1
              endif
          endif
c
c......AXISDESC
c
      else if (inum .eq. 504 .and. kaxs .ne. 0) then
          if (kaxs .le. 6) then
              nc     = strlen1(llin(kaxs))
              lvar   = llin(kaxs)(1:nc) // '_' // laxs(asub(kaxs)) //
     1                 '-axis'
              nc     = nc     + 7
          else
              iprev  = 0
              inc    = kaxs   - 6
              ipt    = IRTWRK(inc) * 2
              if (MACHTP .eq. 3  .and. kaxs .eq. 10) ipt = 5
              if (kaxs .ne. 7 .and. IRTYPE(inc-1) .eq. IRTYPE(inc))
     1                iprev = 1
              if (IRTYPE(kaxs-6) .eq. 1) then
                  if (iprev .eq. 1) then
                      lvar   = 'Carrier_' // laxs(asub(ipt))
     1                         // '-axis_Table'
                  else
                      lvar   = 'Rider_' // laxs(asub(ipt))
     1                         // '-axis_Table'
                  endif
              else
                  if (iprev .eq. 1) then
                      lvar   = 'Rider_' // laxs(asub(ipt))
     1                         // '-axis_Head'
                  else
                      lvar   = 'Carrier_' // laxs(asub(ipt))
     1                         // '-axis_Head'
                  endif
              endif
              nc     = strlen1(lvar)
          endif
c
c......AXISTYPE
c
      else if (inum .eq. 505 .and. kaxs .ne. 0) then
          if (kaxs .le. 6) then
              lvar   = 'Linear'
          else
              lvar   = 'Rotary'
          endif
          nc     = 6
c
c......DATE
c
      else if (inum .eq. 506) then
          lvar   = LDATE
          nc     = strlen1(lvar)
c
c......FEEDMAX
c
      else if (inum .eq. 507 .and. kaxs .ne. 0) then
          call rtoc (FEDLMT(kaxs),lvar,nc)

c
c......FEEDMIN
c
      else if (inum .eq. 508 .and. kaxs .ne. 0) then
          rnum   = 0.
          call rtoc (rnum,lvar,nc)
c
c......HEADER
c
      else if (inum .eq. 1022) then
          lvar   = DHED(5)
          nc     = NCHED(5)
c
c......HOME
c
      else if (inum .eq. 510 .and. kaxs .ne. 0) then
          call rtoc (HOME(kaxs),lvar,nc)
c
c......LIMITMAX
c
      else if (inum .eq. 511 .and. kaxs .ne. 0) then
          call rtoc (LIMITS(2,kaxs),lvar,nc)
c
c......LIMITMIN
c
      else if (inum .eq. 512 .and. kaxs .ne. 0) then
          call rtoc (LIMITS(1,kaxs),lvar,nc)
c
c......MACH
c
      else if (inum .eq. 513) then
          lvar   = LMNAME
          nc     = strlen1(lvar)
c
c......MACHTYPE
c
      else if (inum .eq. 514) then
          if (MACHTP .eq. 1) then
              lvar   = 'Mill'
              nc     = 4
          else if (MACHTP .eq. 2) then
              lvar   = 'Lathe'
              nc     = 5
          else if (MACHTP .eq. 3) then
              lvar   = 'Ultrasonic Blade'
              nc     = strlen1(lvar)
          else
              lvar   = 'Unknown'
              nc     = 7
          endif
c
c......MAXTLNO
c
      else if (inum .eq. 515) then
          call rtoc (MAXTN,lvar,nc)
c
c......REVDATE
c
      else if (inum .eq. 516) then
          lvar   = REVDAT
          nc     = strlen1(lvar)
c
c......SPINVEC
c
      else if (inum .eq. 517) then
          nc     = 0
          do 1500 i=1,3,1
              call rtoc (SPIVEC(i),lnum,nc1)
              lvar(nc+1:) = lnum(1:nc1) // ' '
              nc     = nc     + nc1    + 1
 1500     continue
c
c......TIME
c
      else if (inum .eq. 518) then
          lvar   = LTIME
          nc     = strlen1(lvar)
c
c......TLCTIME
c
      else if (inum .eq. 519) then
          call rtoc (TLCTIM,lvar,nc)
c
c......TLCTYPE
c
      else if (inum .eq. 520) then
          if (TLCCD(4) .ne. 0) then
              lvar   = 'Automatic'
              nc     = 9
          else
              lvar   = 'Manual'
              nc     = 6
          endif
c
c......UNITS
c
      else if (inum .eq. 1009) then
          if (MCHOPT(1) .eq. 1) then
              lvar   = 'Inch'
              nc     = 4
          else
              lvar   = 'Millimeter'
              nc     = 10
          endif
c
c......Unrecognized word
c
      else
          go to 100
      endif
c
c...Replace variable with value
c
      if (ist .eq. 1) then
          if (ien .eq. nci) then
              tbuf   = lvar(1:nc)
          else
              tbuf   = lvar(1:nc) // lbuf(ien+1:)
          endif
      else if (ien .eq. nci) then
          tbuf   = lbuf(1:ist-1) // lvar(1:nc)
      else
          tbuf   = lbuf(1:ist-1) // lvar(1:nc) // lbuf(ien+1:)
      endif
      lbuf   = tbuf
      nci    = strlen1(lbuf)
      ien    = ist    + nc     - 1
      go to 100
c
c...Store output line
c
 7000 cout   = lbuf
      knco   = nci
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  simviw (cdir,cfil,cmsg,kerr)
c
c   FUNCTION:  Generates the machine simulation file name for this MDF
c              file.
c
c
c   INPUT:  cdir    C*n  D1  -  Name of machine simulator data direc-
c                               tory.
c
c   OUTPUT: cfil    C*n  D1  -  Name of machine simulation file.
c
c           cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine simviw (cdir,cfil,cmsg,kerr)
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 kerr
c
      character*(*) cfil,cdir,cmsg
c
      integer*4 irecl
c
      character*20 att(4)
      character*(MAX_PATH) ldev
      character*(MAX_FILE) lext
c
c...Construct the machine data directory
c
      ldev = ' '
      call fparse (ldev,ldev,cdir,' ')
cc      ldev   = cdir
      lext   = '/postworks.mdl'
      if (ldev .eq. ' ') then
          ldev   = './machines/'
      endif
      call fparse (LMNAME,cfil,ldev,lext)
c
c...Check to see if file exists
c
      irecl  = 80
      att(1) = 'direct'
      att(2) = 'none'
      att(3) = 'unformatted'
      att(4) = 'old'
      call opnfil (LUNSC1,cfil,att,irecl,cmsg,kerr)
      call clsfil (LUNSC1)
c
c...End of routine
c
 8000 return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  getmach (cmach)
c
c   FUNCTION:  get current machine simulation number
c
c
c   INPUT:  None.
c
c   OUTPUT: cmach    B*1  Dn  -  Post-processor name.
c
c***********************************************************************
c
      subroutine getmach (cmach)
c
      include 'post.inc'
c
      byte cmach(*)
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      call pwdctb (LMNAME,cmach)
c
c...End of routine
c
 8000 return
      end
