c
c***********************************************************************
c
c   FILE NAME: precmp.for
c   CONTAINS:
c               precmp  cmpdef  cmpena  cmpfmt  cmpfnc  cmpgot  cmpif
c               cmppwd  cmpreq  cmpsyn  cmpteq  cmptol  cmpclf  cmpfio
c               cmpary
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        precmp.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        09/26/17 , 11:55:18
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  precmp (cmsg,kerr)
c
c   FUNCTION:  This is the controlling routine for processing compiled
c              Macro code.  This routine will return only when there is
c              a clfile type record to process (LOADTL, GOTO, etc).  The
c              global variable 'IMACPT' will be set to zero when this
c              routine is finished processing Macro code.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine precmp (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (LSTPC ,KPOSMP(0083))
c
      integer*4 LSTPC
c
      equivalence (LMACRO,CPOSMP(0861))
c
      character*24 LMACRO
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 igo,nc
c
c...Load the next instruction
c
   40 LSTPC  = IPC
      call lodcod (ICMPL,IPC)
      IPC    = IPC    + ICMPL(1)
c
c...Go to appropriate section
c
      igo    = ICMPL(2) + 1
      go to (40,100,200,300,400,500,600,700,700,900,1000,1100,1200,
     1       1300,1400,1500,600), igo
c
c......Real equation
c
  100 call cmpreq (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......Text equation
c
  200 call cmpteq (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......Post word
c
  300 IPNTX  = 1
      if (ICMPL(3) .eq. 4013 .or. ICMPL(3) .eq. 4019 .or.
     1    ICMPL(3) .eq. 4020) then
          call cmpgot (cmsg,kerr)
      else
          call cmppwd (cmsg,kerr)
      endif
      go to 8000
c
c......Jumpto
c
  400 IPNTX  = 1
      IPC    = JCMPL(2)
      go to 40
c
c......If
c
c......Causes the following command to fail
c......If (%Arg(1) <> 0) buf = buf + ' ' + FMTCOD(%Arg(1),#X2)
c
cc  500 IPNTX  = 1
  500 call cmpif (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......Termac
c
  600 IPNTX  = 1
      IMACPT = IMACPT - 1
      if (IMACPT .eq. 0) then
          LSTPC  = 0
          IPC    = 0
          go to 8000
      else
          IPC    = IMACHD(6,IMACPT)
          call getvwd (IMACHD(1,1),LMACRO,nc,1,PSTWRD,PSTWVL,NPSTWD)
          go to 40
      endif
c
c......Multi-parameter Function
c
  700 call cmpfnc (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......Syntax
c
  900 IPNTX  = 1
      call cmpsyn (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......Enable/Disable
c
 1000 IPNTX  = 1
      call cmpena (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......Format specification
c
 1100 IPNTX  = 1
      call cmpfmt
      go to 40
c
c......Define
c
 1200 IPNTX  = 1
      call cmpdef (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......CLREAD, CLFIND, CLWRIT
c
 1300 IPNTX  = 1
      call cmpclf (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......File I/O Commands
c
 1400 IPNTX  = 1
      call cmpfio (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c......MDA Subscript
c
 1500 call cmpary (cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 40
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpdef (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a DEFINE instruction.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpdef (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 idesc(4)
      integer*4 iend,ipt,jpt,rpt,nca,inc,ist,is,ityp,i,jdesc(2)
c
      real*8 ans
c
      character*512 lans
c
      equivalence (jdesc,idesc)
c
c...Initialize routine
c
      iend   = ICMPL(3)
      ist    = 3
      ipt    = ((ist+iend-1)/4 + 1) * 4 - 7
      jpt    = ipt    / 2 + 1
      rpt    = jpt    / 2 + 1
c
c...Loop through the parameters
c
      do 1000 i=1,iend,2
c
c......Increment pointers
c
          ist    = ist    + 2
          ipt    = ipt    + 8
          jpt    = jpt    + 4
          rpt    = rpt    + 2
          is     = ist    + 1
c
c......Vocabulary word
c
          if (ICMPL(is) .eq. 0) then
              call getvwd (JCMPL(jpt+2),lans,nca,2,PSTWRD,PSTWVL,NPSTWD)
              ityp   = 2
              ans    = 0.
c
c......Real variable
c
          else if (ICMPL(is) .le. 3) then
              call lodrel (ICMPL(is),RCMPL(rpt+1),ans,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              ityp   = 1
              lans   = ' '
              nca    = 1
c
c......Text variable
c
          else if (ICMPL(is) .le. 6 .or. ICMPL(is) .eq. 18) then
              inc    = ipt    + 4
              call lodtxt (ICMPL(is),ICMPL(inc),inc,lans,nca,0,cmsg,
     1                     kerr)
              if (kerr .ne. 0) go to 8000
              ityp   = 2
              ans    = 0.
c
c......Macro argument
c......Clfile record data
c
          else if (ICMPL(is) .eq. 12 .or. ICMPL(is) .eq. 15) then
              jdesc(1) = JCMPL(jpt+2)
              jdesc(2) = JCMPL(jpt+3)
              call lodtxt (ICMPL(is),idesc,inc,lans,nca,0,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              ityp   = 2
              ans    = 0.
              if (nca .eq. 0 .or. lans(1:nca) .eq. ' ') then
                  call lodrel (ICMPL(is),RCMPL(rpt+1),ans,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  ityp   = 1
                  lans   = ' '
                  nca    = 1
              endif
c
c......Post variable
c
          else if (ICMPL(is) .eq. 13 .or. ICMPL(is) .eq. 14) then
              call lodrel (ICMPL(is),RCMPL(rpt+1),ans,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              ityp   = 1
              lans   = ' '
              nca    = 1
          endif
c
c......Store variable
c
          call stoarg (ICMPL(ist),JCMPL(jpt),ityp,ans,lans,nca,cmsg,
     1                 kerr)
          if (kerr .ne. 0) go to 8000
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpena (cmsg,kerr)
c
c   FUNCTION:  This routine interprets an ENABLE/DISABLE instruction.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine cmpena (cmsg,kerr)
c
      include 'pregen.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 jdat(5),ityp
c
      integer*2 idat(10)
      equivalence (jdat,idat)
c
c...See if the requested Macro exists
c
      ityp   = ICMPL(4)
      call lodmac (ityp,jdat)
      if (jdat(1) .eq. 0) go to 9000
c
c...Enable or disable Macro
c
      idat(9) = ICMPL(3)
      call stomac (jdat)
c
c...End of routine
c
 8000 return
c
c...No such macro
c
 9000 call errtxt ('NOMACRO',cmsg)
      kerr   = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpfmt
c
c   FUNCTION:  This routine interprets a Format instruction.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cmpfmt
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
c
      integer*4 isub,isub1,i
c
c...Store Format equivalence
c...#F = #F
c
      isub    = ICMPL(3)
      if (ICMPL(1) .eq. 4) then
          isub1  = ICMPL(4)
          do 100 i=1,10,1
              FMTDES(i,isub) = FMTDES(i,isub1)
  100     continue
c
c...Store Format definition
c
      else
          FMTDES(1,isub) = ICMPL(4)
          FMTDES(2,isub) = ICMPL(9)
          FMTDES(3,isub) = ICMPL(10)
          FMTDES(4,isub) = ICMPL(5)
          FMTDES(5,isub) = ICMPL(5)
          FMTDES(6,isub) = ICMPL(6)
          FMTDES(7,isub) = ICMPL(6)
          FMTDES(8,isub) = ICMPL(7)
          FMTDES(9,isub) = ICMPL(8)
          FMTDES(10,isub) = 2
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpfnc (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a muti-parameter Function.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpfnc (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 ityp,idesc(4)
      integer*4 nc1,nc2,ist,ians,index,rindex,rst,idim,ivar,i,ifmt
c
      real*8 arg1,arg2,ans,rnum
c
      character*512 larg1,larg2
c
      integer*4 jnum(2),jdesc(2)
      equivalence (rnum,jnum), (jdesc,idesc)
c
c...Real function
c
      if (ICMPL(2) .eq. 7) then
          ist    = 8 + ((ICMPL(4)-1)/4 + 1) * 4 + 1
c
c......Load the first argument
c
          if (ICMPL(3) .ne. 36) then
              call lodtxt (ICMPL(9),ICMPL(ist),ist,larg1,nc1,0,cmsg,
     1                     kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c...Load the second argument
c
          if (ICMPL(3) .eq. 28 .or. ICMPL(3) .eq. 29) then
              if (ICMPL(9) .ne. 6) ist = ist + 4
              call lodtxt (ICMPL(10),ICMPL(ist),ist,larg2,nc2,0,cmsg,
     1                     kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c.........INDEX
c
          if (ICMPL(3) .eq. 28) then
              call touppr (larg1(1:nc1),larg1)
              call touppr (larg2(1:nc2),larg2)
              ians   = index(larg1(1:nc1),larg2(1:nc2))
              ans    = ians
c
c......RINDEX
c
          else if (ICMPL(3) .eq. 29) then
              call touppr (larg1(1:nc1),larg1)
              call touppr (larg2(1:nc2),larg2)
              ians   = rindex(larg1(1:nc1),larg2(1:nc2))
              ans    = ians
c
c......CTOR
c
          else if (ICMPL(3) .eq. 30) then
              if (ICMPL(9) .ne. 6) ist = ist + 4
              rst    = (ist-1) / 2 + 1
              ifmt   = JCMPL(rst)
              if (ICMPL(10) .ne. 7) then
                  rst    = (ist-1) / 4 + 1
                  call lodrel (ICMPL(10),RCMPL(rst),arg2,cmsg,kerr)
                  ifmt   = arg2
                  if (ifmt .lt. 1 .or. ifmt .gt. MAXFMT) go to 9100
              endif
              call ctof (larg1(1:nc1),ans,FMTDES(1,ifmt),kerr,cmsg)
              if (kerr .ne. 0) go to 8000
c
c......LOCATE
c
          else if (ICMPL(3) .eq. 36) then
              rst    = (ist-1) / 4 + 1
              call loddim (ICMPL(9),RCMPL(rst),ivar,idim,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
              call lodrel (ICMPL(10),RCMPL(rst+1),arg2,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
              ityp   = 1
              ans    = 0
              do 400 i=0,idim-1,1
                  jnum(1) = ivar   + i
                  call lodrel (ityp,rnum,arg1,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  if (arg1 .eq. arg2) then
                      ans    = i      + 1
                      go to 430
                  endif
  400         continue
  430         continue
c
c......Unrecognized function
c
          else
              go to 9000
          endif
c
c......Store result
c
          ivar   = ICMPL(6)
          ians   = ICMPL(7)
          call storel (ICMPL(5),ivar,ians,ans,cmsg,kerr)
c
c...Text function
c
      else
c
c......Load the real argument
c
          call lodrel (ICMPL(9),RCMPL(4),arg1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c......ROTC
c
          if (ICMPL(3) .eq. 31) then
              ifmt = JCMPL(9)
              if (ICMPL(10) .ne. 7) then
                  call lodrel (ICMPL(10),RCMPL(5),arg2,cmsg,kerr)
                  ifmt   = arg2
                  if (ifmt .lt. 1 .or. ifmt .gt. MAXFMT) go to 9100
              endif
              call ftoc (arg1,larg1,nc1,FMTDES(1,ifmt))
c
c......FMTCOD
c
          else if (ICMPL(3) .eq. 45) then
              ifmt = JCMPL(9)
              if (ICMPL(10) .ne. 7) then
                  call lodrel (ICMPL(10),RCMPL(5),arg2,cmsg,kerr)
                  ifmt   = arg2
                  if (ifmt .lt. 1 .or. ifmt .gt. MAXFMT) go to 9100
              endif
              call fmtcod (ifmt,arg1,larg1,nc1)
c
c......Unrecognized function
c
          else
              go to 9000
          endif
c
c......Store result
c

          jdesc(1) = ICMPL(6)
          idesc(3) = ICMPL(7)
          idesc(4) = ICMPL(8)
          call stotxt (ICMPL(5),idesc,larg1,nc1,cmsg,kerr)
      endif

c...End of routine
c
 8000 return
c
c...Invalid function
c
 9000 call errtxt ('INVINSTR',cmsg)
      kerr   = 1
      go to 8000
c
c...Format specifier out of range
c
 9100 call errtxt ('FMTRNG',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpgot (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a GOTO instruction.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine cmpgot (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ICLREC,KPOSMP(0002)), (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (MULTAX,KPOSMP(0056)), (NPT   ,KPOSMP(0059))
      equivalence (MROTTV,KPOSMP(1334))
c
      integer*4 ICLREC,ITYPE,ISUBT,MXCL,MULTAX,NPT,MROTTV
c
      equivalence (METCNV,POSMAP(0004))
      equivalence (CLPT  ,POSMAP(0491)), (VECSAV,POSMAP(1372))
c
      real*8 CLPT(240),VECSAV(3),METCNV
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ipt,jpt,rpt,i,j,is,ie,ist,inc,isub
c
      real*8 rdesc
c
      integer*4 jdesc(2)
      equivalence (rdesc,jdesc)
c
c...Set up Cl record integer buffer
c
      ICLREC = ICLREC + 1
      ITYPE  = 5000
      ISUBT  = 5
      if (ICMPL(3) .eq. 4019.) ISUBT = 3
      if (ICMPL(3) .eq. 4020.) ISUBT = 6
c
c...Process GOTO instruction
c
      inc    = 0
      ist    = 4
      ipt    = 4 + ((ICMPL(4)-1)/4 + 1) * 4 - 3
      jpt    = ipt    / 2 + 1
      rpt    = ipt    / 4 + 1
c
c......Load the cl point array
c
      do 500 i=1,ICMPL(4),1
c
c.........Increment pointers
c
          inc    = inc    + 1
          ist    = ist    + 1
          ipt    = ipt    + 4
          jpt    = jpt    + 2
          rpt    = rpt    + 1
c
c.........Real variable
c
          if (ICMPL(ist) .le. 3) then
              call lodrel (ICMPL(ist),RCMPL(rpt),CLPT(inc),cmsg,
     1                     kerr)
              if (kerr .ne. 0) go to 8000
c
c.........Post argument
c.........Clfile record data
c
          else if (ICMPL(ist) .eq. 12 .or. ICMPL(ist) .eq. 15) then
              isub   = JCMPL(jpt+1) * (-1)
c
c............Multi-subscripted argument
c
              if (isub .gt. 0 .and. PRNVAR(2,isub) .ne. 0) then
                  if (PRNVAR(1,isub) .le. 0 .or.
     1                PRNVAR(2,isub) .lt. PRNVAR(1,isub) .or.
     2                PRNVAR(2,isub) .gt. IMACHD(2,IMACPT))
     3                    go to 9200
                  inc    = inc    - 1
                  is     = PRNVAR(1,isub)
                  ie     = PRNVAR(2,isub)
                  jdesc(1) = JCMPL(jpt)
                  do 200 j=is,ie,1
                      inc    = inc    + 1
                      jdesc(2) = j
                      call lodrel (ICMPL(ist),rdesc,CLPT(inc),cmsg,kerr)
                      if (kerr .ne. 0) go to 8000
  200             continue
c
c............Single subscripted argument
c
              else
                  call lodrel (ICMPL(ist),RCMPL(rpt),CLPT(inc),cmsg,
     1                         kerr)
                  if (kerr .ne. 0) go to 8000
              endif
c
c.........Post variable
c
          else if (ICMPL(ist) .eq. 13) then
              call lodrel (ICMPL(ist),RCMPL(rpt),CLPT(inc),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
c.........Subscripted Post variable
c
          else if (ICMPL(ist) .eq. 14) then
              isub   = JCMPL(jpt+1) * (-1)
              if (isub .gt. 0) then
                  is     = PRNVAR(1,isub)
                  if (PRNVAR(2,isub) .ne. 0) then
                      ie     = PRNVAR(2,isub)
                  else
                      ie     = PRNVAR(1,isub)
                  endif
              else
                  is     = JCMPL(jpt+1)
                  ie     = is
              endif
              if (is .le. 0 .or. ie .lt. is) go to 9200
c
              inc    = inc    - 1
              jdesc(1) = JCMPL(jpt)
              do 300 j=is,ie,1
                  inc    = inc    + 1
                  jdesc(2) = j
                  call lodrel (ICMPL(ist),rdesc,CLPT(inc),cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
  300         continue
c
c.........Unrecognized data type
c
          else
              go to 9000
          endif
  500 continue
c
c...Check for valid number of parameters
c
      ie     = mod(inc,NPT)
      if (ie .ne. 0) go to 9100
      MXCL   = inc    / NPT
c
c...Replace tool vector by VECSAV
c...if option 5.2.1 is not in effect
c
      if (MROTTV .eq. 2 .and. NPT .eq. 6) then
          do 550 i=4,inc,NPT
              CLPT(i) = VECSAV(1)
              CLPT(i+1) = VECSAV(2)
              CLPT(i+2) = VECSAV(3)
  550     continue
      end if
c
c...End of routine
c
 8000 return
c
c...Real variable expected
c
 9000 call errtxt ('NUMVAREX',cmsg)
      kerr   = 1
      go to 8000
c
c...Invalid number of parameters
c
 9100 call errtxt ('INVGOTO',cmsg)
      kerr   = 1
      go to 8000
c
c...Subscript out of range
c
 9200 call errtxt ('SUBOUND',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpif (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a logical IF instruction.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine cmpif (cmsg,kerr)
c
      include 'pregen.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 ityp,idesc(4)
      integer*4 isub,nc
c
      character*80 ldat
c
      integer*4 jdesc(2)
      equivalence (jdesc,idesc)
c
      isub   = ICMPL(4)
c
c...Real IF statment
c
      if (ICMPL(3) .eq. 1) then
          if (PRNVAR(1,isub) .eq. 0) IPC = JCMPL(3)
c
c...Text IF statement
c
      else
          ityp   = 4
          jdesc(1) = ICMPL(4)
          call lodtxt (ityp,jdesc,isub,ldat,nc,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if (nc .eq. 1 .and. ldat(1:1) .eq. ' ') IPC = JCMPL(3)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmppwd (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a post word instruction.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine cmppwd (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ICLREC,KPOSMP(0002)), (ITYPE ,KPOSMP(0003))
      equivalence (ISUBT ,KPOSMP(0004)), (MXCL  ,KPOSMP(0005))
      equivalence (IPSTWD,KPOSMP(0006)), (NCLD  ,KPOSMP(0076))
c
      integer*4 ICLREC,ITYPE,ISUBT,MXCL,IPSTWD(50),NCLD
c
      equivalence (PSTWD ,POSMAP(0441))
c
      real*8 PSTWD(50)
c
      equivalence (LPSTWD,CPOSMP(0217))
c
      character*512 LPSTWD
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 idesc(4)
      integer*4 nc,ipt,jpt,rpt,i,j,is,ie,ist,inc,isub
c
      real*8 rnum,rdesc
c
      integer*4 jdesc(2),mdesc(4)
      equivalence (rdesc,jdesc), (mdesc,idesc)
c
      integer*4 MACHINV /1015/, STOCKV /321/, FIXTURV /898/
      integer*4 LOADV   /1075/, STLV   /330/, MOVEV   /577/
c
c...vp 12/30/97 make sure to exit with 0 if processed OK.
c
      kerr   = 0
c
c...Set up Cl record integer buffer
c
      ICLREC = ICLREC + 1
      ITYPE  = 2000
      ISUBT  = ICMPL(3)
c
c...Text only command
c...LETTER  PPRINT  PARTNO  INSERT  PRINT
c
      if ((ICMPL(3) .ge. 1043 .and. ICMPL(3) .le. 1046) .or.
     1    ICMPL(3) .eq. 1102 .or. ICMPL(3) .eq. 1103 .or.
     2    ICMPL(3) .eq. 1199) then
          MXCL   = 9
c
c......Blank command
c
          if (ICMPL(4) .eq. 0) then
              LPSTWD = ' '
c
c......Vocabulary word
c
          else if (ICMPL(5) .eq. 0) then
              ist    = rnum
              call getvwd (JCMPL(5),LPSTWD,nc,2,PSTWRD,PSTWVL,NPSTWD)
c
c......Real variable
c
          else if (ICMPL(5) .le. 3) then
              call lodrel (ICMPL(5),RCMPL(3),rnum,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              call rtoc (rnum,LPSTWD,nc)
c
c......Text variable
c
          else if (ICMPL(5) .le. 6) then
              ist    = 9
              call lodtxt (ICMPL(5),ICMPL(9),ist,LPSTWD,nc,0,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (nc .eq. 0) then
                  LPSTWD = ' '
              else
                  LPSTWD = LPSTWD(1:nc)
              endif
c
c......Post argument
c......Clfile record data
c
          else if (ICMPL(5) .eq. 12 .or. ICMPL(5) .eq. 15) then
              mdesc(2) = JCMPL(6)
              call lodtxt (ICMPL(5),idesc,ist,LPSTWD,nc,0,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (nc .eq. 0) then
                  LPSTWD = ' '
              else
                  LPSTWD = LPSTWD(1:nc)
              endif
              if (LPSTWD .eq. ' ') then
                  call lodrel (ICMPL(5),RCMPL(3),rnum,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  call rtoc (rnum,LPSTWD,nc)
              endif
c
c......Post variable
c
          else if (ICMPL(5) .eq. 13 .or. ICMPL(5) .eq. 14) then
              call lodrel (ICMPL(5),RCMPL(3),rnum,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              call rtoc (rnum,LPSTWD,nc)
c
c......Text Post variable
c
          else if (ICMPL(5) .eq. 18) then
              mdesc(1) = JCMPL(5)
              mdesc(2) = JCMPL(6)
              call lodtxt (ICMPL(5),idesc,ist,LPSTWD,nc,0,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (nc .eq. 0) then
                  LPSTWD = ' '
              else
                  LPSTWD = LPSTWD(1:nc)
              endif
c
c......Unrecognized data type
c
          else
              go to 9000
          endif
c
c...Standard post word
c
      else
          inc    = 0
          ist    = 4
          ipt    = 4 + ((ICMPL(4)-1)/4 + 1) * 4 - 3
          jpt    = ipt    / 2 + 1
          rpt    = ipt    / 4 + 1
c
c......Load the post word/value arrays
c
          do 500 i=1,ICMPL(4),1
c
c.........Increment pointers
c
              inc    = inc    + 1
              ist    = ist    + 1
              ipt    = ipt    + 4
              jpt    = jpt    + 2
              rpt    = rpt    + 1
c
c.........Vocabulary word
c
              if (ICMPL(ist) .eq. 0) then
                  IPSTWD(inc) = JCMPL(jpt)
c
c.........Real variable
c
              else if (ICMPL(ist) .le. 3) then
                  call lodrel (ICMPL(ist),RCMPL(rpt),PSTWD(inc),cmsg,
     1                         kerr)
                  if (kerr .ne. 0) go to 8000
                  IPSTWD(inc) = 0
c
c.........Text variable
c
              else if (ICMPL(ist) .le. 6) then
                  call lodtxt (ICMPL(ist),ICMPL(ipt),ipt,LPSTWD,nc,0,
     1                         cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
                  if (nc .eq. 0) then
                      LPSTWD = ' '
                  else
                      LPSTWD = LPSTWD(1:nc)
                  endif
                  if ((ISUBT .eq. MACHINV .and. i .eq. 1) .or.
     1                ((ISUBT .eq. STOCKV .or. ISUBT .eq.FIXTURV) .and.
     2                ((IPSTWD(1).eq.LOADV .and. i.eq.2) .or.
     3                 (IPSTWD(1).eq.STLV  .and. i.eq.3) .or.
     4                 (IPSTWD(1).eq.MOVEV .and. i.eq.14))))
     5                then
                  else
                      call getvnm (LPSTWD(1:nc),IPSTWD(inc),PSTWRD,
     1                             PSTWVL,NPSTWD)
                      if (IPSTWD(inc) .eq. 0) go to 9100
                  endif
c
c.........Post argument
c.........Clfile record data
c
              else if (ICMPL(ist) .eq. 12 .or. ICMPL(ist) .eq. 15) then
                  isub   = JCMPL(jpt+1) * (-1)
c
c............Multi-subscripted argument
c
                  if (isub .gt. 0 .and. PRNVAR(2,isub) .ne. 0) then
                      if (PRNVAR(1,isub) .le. 0 .or.
     1                    PRNVAR(2,isub) .lt. PRNVAR(1,isub))
     2                        go to 9200
                      if ((ICMPL(ist) .eq. 12 .and.
     1                    PRNVAR(2,isub) .gt. IMACHD(2,IMACPT)) .or.
     2                    (ICMPL(ist) .eq. 15 .and.
     3                    PRNVAR(2,isub) .gt. NCLD))
     4                        go to 9200
                      inc    = inc    - 1
                      is     = PRNVAR(1,isub)
                      ie     = PRNVAR(2,isub)
                      mdesc(1) = JCMPL(jpt)
                      jdesc(1) = mdesc(1)
                      do 200 j=is,ie,1
                          inc    = inc    + 1
c
c...............Text argument
c
                          mdesc(2) = j
                          call lodtxt (ICMPL(ist),idesc,isub,LPSTWD,nc,
     1                                 0,cmsg,kerr)
                          if (kerr .ne. 0) go to 8000
                          if (nc .eq. 0) then
                              LPSTWD = ' '
                          else
                              LPSTWD = LPSTWD(1:nc)
                          endif
                          if (LPSTWD .ne. ' ') then
                              if (ISUBT .ne. 1015 .or. i .ne. 1) then
                                  call getvnm (LPSTWD(1:nc),IPSTWD(inc),
     1                                         PSTWRD,PSTWVL,NPSTWD)
                                  if (IPSTWD(inc) .eq. 0) go to 9100
                              endif
c
c...............Real argument
c
                          else
                              jdesc(2) = j
                              call lodrel (ICMPL(ist),rdesc,PSTWD(inc),
     1                                     cmsg,kerr)
                              if (kerr .ne. 0) go to 8000
                              IPSTWD(inc) = 0
                          endif
  200                 continue

                  else
c
c............Single subscripted argument
c
                      mdesc(1) = JCMPL(jpt)
c
c...Changed Macro arguments subscript to I*4
c...Bobby  -  3/24/92
c
c                      idesc(3) = JCMPL(jpt+1)
                      mdesc(2) = JCMPL(jpt+1)
                      call lodtxt (ICMPL(ist),idesc,isub,LPSTWD,nc,0,
     1                             cmsg,kerr)
                      if (kerr .ne. 0) go to 8000
                      if (nc .eq. 0) then
                          LPSTWD = ' '
                      else
                          LPSTWD = LPSTWD(1:nc)
                      endif
                      if (LPSTWD .ne. ' ') then
                          if (ISUBT .ne. 1015 .or. i .ne. 1) then
                              call getvnm (LPSTWD(1:nc),IPSTWD(inc),
     1                                     PSTWRD,PSTWVL,NPSTWD)
                              if (IPSTWD(inc) .eq. 0) go to 9100
                          endif
                      else
                          call lodrel (ICMPL(ist),RCMPL(rpt),PSTWD(inc),
     1                                 cmsg,kerr)
                          if (kerr .ne. 0) go to 8000
                          IPSTWD(inc) = 0
                      endif
                  endif
c
c.........Post variable
c
              else if (ICMPL(ist) .eq. 13) then
                  call lodrel (ICMPL(ist),RCMPL(rpt),PSTWD(inc),cmsg,
     1                         kerr)
                  if (kerr .ne. 0) go to 8000
                  IPSTWD(inc) = 0
c
c.........Subscripted Post variable
c
              else if (ICMPL(ist) .eq. 14) then
                  isub   = JCMPL(jpt+1) * (-1)
                  if (isub .gt. 0) then
                      is     = PRNVAR(1,isub)
                      if (PRNVAR(2,isub) .ne. 0) then
                          ie     = PRNVAR(2,isub)
                      else
                          ie     = PRNVAR(1,isub)
                      endif
                  else
                      is     = JCMPL(jpt+1)
                      ie     = is
                  endif
                  if (is .le. 0 .or. ie .lt. is) go to 9200
c
                  inc    = inc    - 1
                  jdesc(1) = JCMPL(jpt)
                  do 300 j=is,ie,1
                      inc    = inc    + 1
                      jdesc(2) = j
                      call lodrel (ICMPL(ist),rdesc,PSTWD(inc),
     1                             cmsg,kerr)
                      if (kerr .ne. 0) go to 8000
                      IPSTWD(inc) = 0
  300             continue
c
c.........Unrecognized data type
c
              else
                  go to 9000
              endif
  500     continue
      MXCL   = inc
      endif
c
c...End of routine
c
 8000 return
c
c...Text variable expected
c
 9000 call errtxt ('TXTVAREX',cmsg)
      kerr   = 1
      go to 8000
c
c...Variable, Voc word or Number expected
c
 9100 call errtxt ('VARVOCNM',cmsg)
      kerr   = 1
      go to 8000
c
c...Subscript out of range
c
 9200 call errtxt ('SUBOUND',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpreq (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a real equation instruction.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine cmpreq (cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ityp,ist,inum,nc1
c
      real*8 ans,arg1,arg2,dval,eval
c
      character*512 larg1
c
c...Initialize routine
c
      ityp    = ICMPL(3)
c
c...First function parameter is a text variable
c
      if (ityp .eq. 28) then
          ist = 0
          call lodtxt (ICMPL(5),ICMPL(13),ist,larg1,nc1,0,cmsg,kerr)
c
c...Get the 1st operand value
c
      else
          call lodrel (ICMPL(5),RCMPL(4),arg1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...Get the 2nd operand value
c
          if (ityp .ge. 3 .and. ityp .le. 16) then
              call lodrel (ICMPL(6),RCMPL(5),arg2,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
      endif
c
c...Solve equation
c
      go to (50,9000,70,100,200,300,400,500,600,700,800,900,1000,
     1       1100,1200,1300,9000,9000,1400,1500,1600,1700,1800,1900,
     2       2000,2100,2200,2300), ityp
c
   50     ans    = arg1
          go to 3000
c
   70     if (ICMPL(4) .ne. 1 .or. JCMPL(4) .gt. MAXPRN) go to 9000
          ist    = JCMPL(4)
          PRNVAR(1,ist) = arg1
          PRNVAR(2,ist) = arg2
          go to 8000
c
  100     ans    = arg1   + arg2
          go to 3000
c
  200     ans    = arg1   - arg2
          go to 3000
c
  300     ans    = arg1   * arg2
          go to 3000
c
  400     if (arg2 .eq. 0.) go to 9100
          ans    = arg1   / arg2
          go to 3000
c
  500     if (arg2 .eq. 0.) then
              ans    = 1.
          else if (arg1 .lt. 0.) then
              inum   = dint(arg2)
              ans    = arg1   ** inum
          else
              ans    = arg1   ** arg2
          endif
          go to 3000
c
  600     ans    = 1.
          call cmptol (arg1,arg2)
          if (arg1 .eq. arg2) ans = 0.
          go to 3000
c
  700     ans    = 1.
          call cmptol (arg1,arg2)
          if (arg1 .lt. arg2) ans = 0.
          go to 3000
c
  800     ans    = 1.
          call cmptol (arg1,arg2)
          if (arg1 .gt. arg2) ans = 0.
          go to 3000
c
  900     ans    = 1.
          call cmptol (arg1,arg2)
          if (arg1 .ne. arg2) ans = 0.
          go to 3000
c
 1000     ans    = 1.
          call cmptol (arg1,arg2)
          if (arg1 .le. arg2) ans = 0.
          go to 3000
c
 1100     ans    = 1.
          call cmptol (arg1,arg2)
          if (arg1 .ge. arg2) ans = 0.
          go to 3000
c
 1200     ans    = 1.
          if (arg1 .eq. 0 .and. arg2 .eq. 0) ans = 0.
          go to 3000
c
 1300     ans    = 1.
          if (arg1 .eq. 0 .or. arg2 .eq. 0) ans = 0.
          go to 3000
c
 1400     ans    = dint(arg1)
          go to 3000
c
 1500     ans    = dcos(arg1/RAD)
          go to 3000
c
 1600     ans    = dsin(arg1/RAD)
          go to 3000
c
 1700     dval = dmod(arg1,90.d0)
          eval = arg1 / 90.
          if (dval .eq. 0. .and. dmod(eval,2.d0) .ne. 0.) go to 9200
          ans    = dtan(arg1/RAD)
          go to 3000
c
 1800     if (arg1 .lt. 0.) go to 9200
          ans    = dsqrt(arg1)
          go to 3000
c
 1900     if (arg1 .lt. -1. .or. arg1 .gt. 1.) go to 9200
          ans    = dacos(arg1) * RAD
          go to 3000
c
 2000     if (arg1 .lt. -1. .or. arg1 .gt. 1.) go to 9200
          ans    = dasin(arg1) * RAD
          go to 3000
c
 2100     ans    = datan(arg1) * RAD
          go to 3000
c
 2200     ans    = dabs(arg1)
          go to 3000
c
 2300     ans    = nc1
          go to 3000
c
c...Store results of equation
c
 3000 call storel (ICMPL(4),JCMPL(4),JCMPL(5),ans,cmsg,kerr)
c
c...End of routine
c
 8000 return
c
c...Invalid operator
c
 9000 call errtxt ('INVINSTR',cmsg)
      kerr   = 1
      go to 8000
c
c...Divide by zero
c
 9100 call errtxt ('DIVZERO',cmsg)
      kerr   = 1
      ans    = 0.
      go to 3000
c
c...Invalid argument
c
 9200 call errtxt ('INVARG',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpsyn (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a SYNTAX instruction.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpsyn (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (IERROR,KPOSMP(0081))
c
      integer*4 IERROR
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 ityp,idesc(4)
      integer*4 iend,ipt,jpt,rpt,nc,inc,ist,inum,i,j
c
      real*8 rdesc,rnum
c
      character*24 lnum
c
      integer*4 jdesc(2),mdesc(2)
      equivalence (rdesc,jdesc), (mdesc,idesc)
c
c...Check number of arguments
c
      if (IMACHD(5,IMACPT) .gt. ICMPL(3)) go to 9000
c
c...Initialize routine
c
      IERROR = 0
      iend   = IMACHD(5,IMACPT)
      ityp   = 12
      ist    = 5
c
c...Loop through the Macro arguments
c
      do 1000 i=1,iend,1
c
c......Adjust pointers
c
          ipt    = ((ist+ICMPL(ist)-1)/4 + 1) * 4 + 1
          jpt    = ipt    / 2 + 1
          rpt    = jpt    / 2 + 1
c
c......Check type of argument
c
c
c...Changed Macro argument subscripts to I*4
c...Bobby  -  3/24/92
c
c          idesc(3) = i
          mdesc(2) = i
          call lodtxt (ityp,idesc,inc,lnum,nc,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if (nc .eq. 0) then
              lnum   = ' '
          else
              lnum   = lnum(1:nc)
          endif
c
c.........Text argument
c
          if (lnum .ne. ' ') then
              call getvnm (lnum(1:nc),inum,PSTWRD,PSTWVL,NPSTWD)
              if (inum .eq. 0) go to 9000
              do 100 j=1,ICMPL(ist),1
                  if (ICMPL(ist+j) .eq. 0) then
                      if (inum .eq. JCMPL(jpt)) go to 500
                      jpt    = jpt    + 2
                  else if (ICMPL(ist+j) .eq. 11) then
                      jpt    = jpt    + 4
                  else
                      jpt    = jpt    + 2
                  endif
  100         continue
              go to 9000
c
c.........Real argument
c
          else
              jdesc(2) = i
              call lodrel (ityp,rdesc,rnum,cmsg,kerr)
              if (kerr .ne. 0) go to 1000
              do 200 j=1,ICMPL(ist),1
                  if (ICMPL(ist+j) .eq. 3) then
                      if (rnum .eq. RCMPL(rpt)) go to 500
                      rpt    = rpt    + 1
                  else if (ICMPL(ist+j) .eq. 11) then
                      if (rnum .ge. RCMPL(rpt) .and.
     1                    rnum .le. RCMPL(rpt+1)) go to 500
                      rpt    = rpt    + 2
                  else
                      rpt    = rpt    + 1
                  endif
  200        continue
          endif
          go to 9000
c
c......Point to next parameter array
c
  500     do 600 j=1,ICMPL(ist),1
              if (ICMPL(ist+j) .eq. 11) then
                  ipt    = ipt    + 8
              else
                  ipt    = ipt    + 4
              endif
  600     continue
          ist    = ipt
 1000 continue
c
c...End of routine
c
 8000 return
c
c...Command syntax error
c
 9000 IERROR  = 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpteq (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a text equation instruction.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine cmpteq (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 ityp,ist,nc1,nc2,nca,strlen1
c
      character*512 larg1,larg2,lans
      character*(MAX_PATH) ldev
      character*(MAX_FILE) lfil,lext
c
c...Function with no arguments
c
      if ((ICMPL(3) .ge. 32 .and. ICMPL(3) .le. 34) .or.
     1    (ICMPL(3) .ge. 37 .and. ICMPL(3) .le. 45)) then
c
          if (ICMPL(3) .eq. 32) then
              lans   = LDATE
c
          else if (ICMPL(3) .eq. 33) then
              lans   = LTIME
c
          else if (ICMPL(3) .eq. 34) then
              call bldcmd (lans,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
          else if (ICMPL(3) .eq. 37) then
              call gethst (lans,nc1)
c
          else if (ICMPL(3) .eq. 38) then
              call getusr (lans,nc1)
c
          else if (ICMPL(3) .eq. 39) then
              lans   = PCHFIL
c
          else if (ICMPL(3) .eq. 40) then
              call fbreak (PCHFIL,ldev,lfil,lext)
              nc1    = strlen1(lfil)
              nc2    = strlen1(lext)
              lans   = lfil(1:nc1) // lext(1:nc2)
c
          else if (ICMPL(3) .eq. 41) then
              call fbreak (PCHFIL,ldev,lans,lext)
c
          else if (ICMPL(3) .eq. 42) then
              lans   = LCMPFI
c
          else if (ICMPL(3) .eq. 43) then
              call fbreak (LCMPFI,ldev,lfil,lans)
c
          else if (ICMPL(3) .eq. 44) then
              call fbreak (LCMPFI,ldev,lans,lext)
          else if (ICMPL(3) .eq. 45) then
              call getlic (lans,nc1)
          endif
c
c......Store text
c
          nca    = strlen1(lans)
          call stotxt (ICMPL(4),ICMPL(7),lans,nca,cmsg,kerr)
          go to 8000
      endif
c
c...Equation or Function w/arguments
c...Get the 1st operand value
c
      ist    = 19
      call lodtxt (ICMPL(5),ICMPL(11),ist,larg1,nc1,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Get the 2nd operand value
c
      if (ICMPL(3) .ge. 3 .and. ICMPL(3) .le. 16) then
          call lodtxt (ICMPL(6),ICMPL(15),ist,larg2,nc2,0,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Solve equation
c
      ityp    = ICMPL(3)
c
      if (ityp .eq. 1) then
          if (nc1 .ne. 0) lans = larg1(1:nc1)
          nca    = nc1
c
      else if (ityp .eq. 4) then
          if (nc1 .eq. 0) then
              if (nc2 .ne. 0) lans = larg2(1:nc2)
          else if (nc2 .eq. 0) then
              lans   = larg1(1:nc1)
          else
              lans   = larg1(1:nc1) // larg2(1:nc2)
          endif
          nca    = nc1    + nc2
c
      else if (ityp .eq. 9) then
          lans   = '1'
          nca    = 1
          if (nc1 .eq. 0 .or. nc2 .eq. 0) go to 3000
          call touppr (larg1(1:nc1),larg1)
          call touppr (larg2(1:nc2),larg2)
          if (nc1 .eq. nc2 .and. larg1(1:nc1) .eq. larg2(1:nc2))
     1        lans    = ' '
c
      else if (ityp .eq. 12) then
          lans   = '1'
          nca    = 1
          if (nc1 .eq. 0 .or. nc2 .eq. 0) go to 3000
          call touppr (larg1(1:nc1),larg1)
          call touppr (larg2(1:nc2),larg2)
          if (nc1 .ne. nc2 .or. larg1(1:nc1) .ne. larg2(1:nc2))
     1        lans    = ' '
c
      else if (ityp .eq. 15) then
          lans   = '1'
          nca    = 1
          if (nc1 .eq. 0 .or. nc2 .eq. 0) go to 3000
          if (nc1 .eq. nc2 .and. larg1(1:nc1) .eq. ' ' .and.
     1        larg2(1:nc2) .eq. ' ') lans = ' '
c
      else if (ityp .eq. 16) then
          lans   = '1'
          nca    = 1
          if (nc1 .eq. 0 .or. nc2 .eq. 0) go to 3000
          if (nc1 .eq. nc2 .and. (larg1(1:nc1) .eq. ' ' .or.
     1        larg2(1:nc2) .eq. ' ')) lans = ' '
c
      else if (ityp .eq. 35) then
          call errtxt (larg1(1:nc1),lans)
          nca    = strlen1(lans)
c
      else
          go to 9000
      endif
c
c...Store results of equation
c
 3000 call stotxt (ICMPL(4),ICMPL(7),lans,nca,cmsg,kerr)
c
c...End of routine
c
 8000 return
c
c...Invalid operator
c
 9000 call errtxt ('INVINSTR',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmptol (garg1,garg2)
c
c   FUNCTION:  This routine compares two numbers and declares them
c              equal if the fall within IFTOL of each other.
c
c   INPUT:  garg1   R*8  D1    -  First number to comare.
c
c           garg2   R*8  D1    -  Second number to comare.
c
c   OUTPUT: garg2   R*8  D1    -  Returns 'garg1' if the two numbers
c                                 are within tolerance.
c
c***********************************************************************
c
      subroutine cmptol (garg1,garg2)
c
      include 'pregen.inc'
c
      real*8 garg1,garg2
c
c...Are the two numbers within tolerance
c
      if (dabs(garg1-garg2) .le. IFTOL) garg2 = garg1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpclf (cmsg,kerr)
c
c   FUNCTION:  This routine interprets the CLREAD, CLFIND, and CLWRIT
c              instructions.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpclf (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (ICLSMP,KPOSMP(0077))
c
      integer*4 ICLSMP
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 iend,ipt,jpt,rpt,nca,i,irec,irecpt,irecsb,indx,nc,
     1          indxpt,indxsb,ierrpt,ierrsb,iwrd(20),ist,perr,nwrd
c
      logical idid
c
      real*8 ans
c
      character*24 lans
c
c...Initialize routine
c
      if (ICLSMP .eq. 1) then
          kerr   = 2
          go to 8000
      endif
      iend   = ICMPL(4)
      ist    = 4
      ipt    = 4 + ((ICMPL(4)-1)/4 + 1) * 4 - 3
      jpt    = ipt    / 2 + 1
      rpt    = jpt    / 2 + 1
c
      irecpt = 0
      indxpt = 0
      ierrpt = 0
c
      lans   = ' '
      nca    = 0
      nwrd   = 0
c
c...Loop through the parameters
c
      do 1000 i=1,iend,1
c
c......Increment pointers
c
          ist    = ist    + 1
          ipt    = ipt    + 4
          jpt    = jpt    + 2
          rpt    = rpt    + 1
c
c......Clfile record number
c
          if (i .eq. 1 .and. ICMPL(3) .ne. 4055) then
              call lodrel (ICMPL(ist),RCMPL(rpt),ans,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              irec   = ans
              irecpt = ist
              irecsb = jpt
c
c......Clfile record index
c
          else if (i .eq. 2 .and. ICMPL(3) .ne. 4055) then
              call lodrel (ICMPL(ist),RCMPL(rpt),ans,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              indx   = ans
              indxpt = ist
              indxsb = jpt
          else if (i .eq. iend) then
              call lodrel (ICMPL(ist),RCMPL(rpt),ans,cmsg,kerr)
              if (kerr .ne. 0 .and. ICMPL(3) .ne. 4054) go to 8000
              ierrpt = ist
              ierrsb = jpt
          endif
c
c......Vocabulary words to find
c
          if (i .ge. 3 .and. ICMPL(3) .eq. 4054 .and. ierrpt .eq. 0)
     1            then
              nwrd   = nwrd   + 1
              if (ICMPL(ist) .eq. 0) then
                  iwrd(nwrd) = JCMPL(jpt)
              else
                  if (ICMPL(ist) .le. 6) then
                      call lodtxt (ICMPL(ist),ICMPL(ipt),ipt,lans,nc,
     1                             0,cmsg,kerr)
                  else if (ICMPL(ist) .eq. 12 .or. ICMPL(ist) .eq. 15)
     1                    then
                      call lodtxt (ICMPL(ist),ICMPL(ipt),ipt,lans,nc,
     1                             0,cmsg,kerr)
                  endif
                  call getvnm (lans(1:nc),iwrd(nwrd),PSTWRD,PSTWVL,
     1                         NPSTWD)
              endif
          endif
 1000 continue
c
c...Read next clfile record
c
      if (ICMPL(3) .eq. 4053) then
          call clread (irec,indx,2,cmsg,perr)
c
c...Find clfile record
c
      else if (ICMPL(3) .eq. 4054) then
          idid   = .false.
          CLDATA(3) = 0
 1200     if (CLDATA(3) .eq. 14000) then
              perr   = 2
          else
              call clread (irec,indx,2,cmsg,perr)
              do 1250 i=1,nwrd,1
                  if (iwrd(i) .eq. 4023 .and. CLDATA(3) .eq. 1000)
     1                idid = .true.
                  if (iwrd(i) .eq. 4026 .and. CLDATA(3) .eq. 3000)
     1                idid = .true.
                  if (iwrd(i) .eq. 4013 .and. (CLDATA(3) .eq. 5000 .or.
     1                CLDATA(3) .eq. 5200)) idid = .true.
                  if (iwrd(i) .eq. 4019 .and. (CLDATA(3) .eq. 5210 .or.
     1                CLDATA(3) .eq. 5220)) idid = .true.
                  if (iwrd(i) .eq. 4025 .and. (CLDATA(3) .eq. 6000 .or.
     1                CLDATA(3) .eq. 7100)) idid = .true.
                  if (iwrd(i) .eq. 1105 .and. CLDATA(3) .eq. 9000)
     1                idid = .true.
                  if (iwrd(i) .eq. 4012 .and. CLDATA(3) .eq. 14000)
     1                idid = .true.
                  if (CLDATA(3) .eq. 2000 .and. iwrd(i) .eq. CLDATA(4))
     1                idid = .true.
 1250         continue
              if (.not. idid .and. perr .eq. 0) go to 1200
          endif
c
c...Write clfile record
c
      else if (ICMPL(3) .eq. 4055) then
          call cldwrt (INEREC,INEPT,cmsg,kerr)
      endif
c
c...Store variables
c......Clfile record number
c
      if (irecpt .ne. 0 .and. perr .eq. 0) then
          ans   = irec
          call stovar (ICMPL(irecpt),JCMPL(irecsb),1,ans,lans,nca,
     1                 cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c......Clfile index number
c
      if (indxpt .ne. 0 .and. perr .eq. 0) then
          ans   = indx
          call stovar (ICMPL(indxpt),JCMPL(indxsb),1,ans,lans,nca,
     1                 cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c......Error code
c
      if (ierrpt .ne. 0) then
          ans   = perr
          call stovar (ICMPL(ierrpt),JCMPL(ierrsb),1,ans,lans,nca,
     1                 cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      else
          if (kerr .eq. 0) kerr = perr
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpfio (cmsg,kerr)
c
c   FUNCTION:  This routine interprets the FOPEN, FCLOSE, FREW, FREAD,
c              and FWRITE instructions.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpfio (cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      equivalence (ICLSMP,KPOSMP(0077))
c
      integer*4 ICLSMP
c
      integer*4 iend,ipt,jpt,rpt,nca,i,nc,imode,irdpt,irdsb,iunit,
     1          ierrpt,ierrsb,ist,ncf,incpt,incsb,strlen1,ifnpt,ifnsb,
     2          ifn,irecl,inc,idim
c
      logical lsto
c
      real*8 ans
c
      character*20 att(4)
      character*256 lfile,lans,ltmp,lext,ldev,text,tdev
c
c...Initialize routine
c
      iend   = ICMPL(4)
      ist    = 4
      ipt    = 4 + ((ICMPL(4)-1)/4 + 1) * 4 - 3
      jpt    = ipt    / 2 + 1
      rpt    = jpt    / 2 + 1
c
      ierrpt = 0
      ifnpt  = 0
      kerr   = 0
c
      lans   = ' '
      nca    = 0
c
      if (ICLSMP .eq. 1) go to 8000
c
c...Loop through the parameters
c
      do 1000 i=1,iend,1
c
c......Increment pointers
c
          ist    = ist    + 1
          ipt    = ipt    + 4
          jpt    = ipt    / 2 + 1
          rpt    = jpt    / 2 + 1
c
c......File number
c
          if (i .eq. 1) then
              call lodrel (ICMPL(ist),RCMPL(rpt),ans,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              if (ICMPL(ist) .ne. 3) then
                  ifnpt  = ist
                  ifnsb  = jpt
              endif
              ifn    = ans
c
c......File mode
c
          else if (i .eq. 2 .and. ICMPL(3) .eq. 4056) then
              call lodtxt (ICMPL(ist),ICMPL(ipt),ipt,lfile,nc,1,cmsg,
     1                     kerr)
              call touppr (lfile(1:nc),lans)
              if (lans .eq. "READ") then
                  imode  = 1
              else if (lans .eq. "WRITE") then
                  imode  = 2
              else if (lans .eq. "APPEND") then
                  imode  = 3
              else if (lans .eq. "NEW") then
                  imode  = 4
              else if (lans .eq. "TEMP") then
                  imode  = 5
              else
                  imode  = 0
              endif
c
c......Filename
c
          else if (i .eq. 3 .and. ICMPL(3) .eq. 4056) then
              call lodtxt (ICMPL(ist),ICMPL(ipt),ipt,lfile,ncf,1,cmsg,
     1                     kerr)
              lfile(ncf+1:) = ' '
              call fbreak (lfile,ldev,ltmp,lext)
              if (ltmp .eq. '*') then
                  if (PGMNAM .eq. 'PostMacro') then
                      call fbreak (LCMPFI,tdev,ltmp,text)
                  else
                      call fbreak (PCHFIL,tdev,ltmp,text)
                  endif
                  call fparse (ltmp,lfile,tdev,lext)
                  ncf    = strlen1(lfile)
              endif
c
c......Read text variable
c
          else if (i .eq. 2 .and. ICMPL(3) .eq. 4059) then
              call lodtxt (ICMPL(ist),ICMPL(ipt),ipt,lans,nc,1,cmsg,
     1                     kerr)
              if (kerr .ne. 0) go to 8000
              irdpt = ist
              irdsb = jpt
c
c......Error variable
c
          else if (i .eq. 3 .and. ICMPL(3) .eq. 4059) then
              call lodrel (ICMPL(ist),RCMPL(rpt),ans,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              incpt = ist
              incsb = jpt
c
c......Write data
c
          else if (i .eq. 2 .and. ICMPL(3) .eq. 4060) then
              call lodtxt (ICMPL(ist),ICMPL(ipt),ipt,lfile,ncf,1,cmsg,
     1            kerr)
              if (kerr .ne. 0) go to 8000
              lfile(ncf+1:) = ' '
c
c......Error variable
c
          else
              call lodrel (ICMPL(ist),RCMPL(rpt),ans,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              ierrpt = ist
              ierrsb = jpt
          endif
 1000 continue
c
c...Get file number
c
      lsto   = .false.
      if (ICMPL(3) .eq. 4056) then
          if (ifn .eq. 0) then
              FIOFN  = 0
              do 1100 i=1,5,1
                  if (FIOPND(i) .eq. 0) then
                      FIOFN = i
                      go to 1110
                  endif
 1100         continue
 1110         ifn    = FIOFN
              lsto   = .true.
          endif
          if (ifn .lt. 1 .or. ifn .gt. 5) go to 9080
      else
          if (ifn .eq. 0) then
              ifn = FIOFN
              lsto   = .true.
          endif
          if (ifn .eq. 0) go to 9080
          if (FIOPND(ifn) .eq. 0) go to 9050
      endif
      iunit  = ifn    + 20
c
c......Store file number
c
      if (lsto .and. ifnpt .ne. 0) then
          ans    = ifn
          call stovar (ICMPL(ifnpt),JCMPL(ifnsb),1,ans,lans,nca,
     1                 cmsg,kerr)
      endif
c
c...Open file
c
      if (ICMPL(3) .eq. 4056) then
c
c......Get the correct file unit number
c
          if (FIOPND(ifn) .eq. 1) go to 9020
          if (imode .eq. 0) go to 9070
c
c......Set the file attributes
c
          att(1) = 'sequential'
          att(2) = 'list'
          att(3) = 'formatted'
          att(4) = 'old'
          if (imode .eq. 2) att(4) = 'new'
          if (imode .eq. 3) then
              att(1) = 'append'
              att(4) = 'write'
          endif
          if (imode .eq. 5) att(4) = 'write'
          irecl  = 256
c
c......Open the file
c
          call opnfil (iunit,lfile,att,irecl,cmsg,kerr)
          if (imode .eq. 4 .and. kerr .ne. -2) then
              if (kerr .ne. 0) go to 9030
              call clsfil (iunit)
              go to 9090
          endif
          if ((imode .eq. 4 .or. imode .eq. 5) .and. kerr .eq. -2) then
              att(4) = 'new'
              call opnfil (iunit,lfile,att,irecl,cmsg,kerr)
          endif
          if (kerr .eq. -2) go to 9010
          if (kerr .ne. 0) go to 9030
          if (imode .eq. 5) then
              FNTEMP = FNTEMP + 1
              FLFNAM(FNTEMP) = lfile
          endif
          FIOPND(ifn) = 1
          if (lsto) FIOFN = ifn
c
c...Close file
c
      else if (ICMPL(3) .eq. 4057) then
          call clsfil (iunit)
          FIOPND(ifn) = 0
          if (ifn .eq. FIOFN) FIOFN = 0
c
c...Rewind file
c
      else if (ICMPL(3) .eq. 4058) then
          rewind (iunit)
c
c...Read file
c
      else if (ICMPL(3) .eq. 4059) then
          call rdtxt (iunit,lfile,cmsg,kerr)
          if (kerr .eq. 1) go to 9060
          if (kerr .ne. 0) go to 9030
   10     format (a)
          nc     = strlen1(lfile)
          call stovar (ICMPL(irdpt),JCMPL(irdsb),2,nc,lfile,nc,
     1                 cmsg,kerr)
          ans   = nc
          call stovar (ICMPL(incpt),JCMPL(incsb),1,ans,lfile,nc,
     1                 cmsg,kerr)
          call loddim (ICMPL(irdpt),JCMPL(irdsb),inc,idim,cmsg,kerr)
          if (idim .lt. nc) go to 9040
c
c...Write file
c
      else if (ICMPL(3) .eq. 4060) then
          call wrtxt (iunit,lfile,ncf,cmsg,kerr)
          if (kerr .ne. 0) go to 9030
      endif
c
c...End of routine
c......Store Error code
c
 8000 if (ierrpt .ne. 0) then
          ans    = kerr
          call stovar (ICMPL(ierrpt),JCMPL(ierrsb),1,ans,lans,nca,
     1                 cmsg,kerr)
          kerr   = 0
      endif
      return
c
c...No such file
c
 9010 kerr   = 1
      call shfile (lfile,lans,40)
      cmsg   = "No such file."
      go to 8000
c
c...File already open
c
 9020 kerr   = 2
      cmsg   = "File already open."
      go to 8000
c
c...File I/O error
c
 9030 kerr   = 3
      cmsg   = "File I/O error."
      go to 8000
c
c...Buffer too small
c
 9040 kerr   = 4
      cmsg   = "Read buffer too small for data."
      go to 8000
c
c...File not open
c
 9050 kerr   = 5
      cmsg   = "File is not open."
      go to 8000
c
c...End of file
c
 9060 kerr   = 6
      cmsg   = "End of file encountered."
      go to 8000
c
c...Invalid access mode
c
 9070 kerr   = 7
      cmsg   = "Invalid file access mode."
      go to 8000
c
c...Invalid file number
c
 9080 kerr   = 8
      cmsg   = "Invalid file number."
      go to 8000
c
c...File already exists
c
 9090 kerr   = 9
      cmsg   = "File already exists."
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cmpary (cmsg,kerr)
c
c   FUNCTION:  This routine interprets a multi-dimensional array (MDA)
c              subscript.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine cmpary (cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      equivalence (MULTAX,KPOSMP(0056))
c
      integer*4 MULTAX
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i
c
      real*8 arg(3)
c
c...%CLPT array dimensions
c...depend on MULTAX setting
c
      if (ICMPL(11) .eq. 6007) then
          if (MULTAX .eq. 1) then
              ICMPL(5) = 6
              ICMPL(6) = 40
          else
              ICMPL(5) = 3
              ICMPL(6) = 80
          endif
          PRNVAR(2,ICMPL(3)) = 0
          ICMPL(7) = 1
      endif
c
c...Get the subscripts
c
      do 100 i=1,3,1
          call lodrel (ICMPL(i+7),RCMPL(i+3),arg(i),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          if ((arg(i) .eq. 0 .and. i .ne. 1) .or. arg(i) .lt. 0 .or.
     1         arg(i) .gt. ICMPL(i+4)) go to 9000
  100 continue
c
c...Calculate the pointer within the array
c
      PRNVAR(1,ICMPL(3)) = (arg(3)-1) * ICMPL(5)*ICMPL(6) +
     1                     (arg(2)-1) * ICMPL(5) +
     2                     arg(1)
c
c......Subscript range specified
c
      if (arg(1) .eq. 0) then
          PRNVAR(1,ICMPL(3)) = PRNVAR(1,ICMPL(3)) + 1
          PRNVAR(2,ICMPL(3)) = -(PRNVAR(1,ICMPL(3)) + ICMPL(5) - 1)
      else if (ICMPL(8) .eq. 1 .and. (JCMPL(7) .le. MAXPRN .and.
     1         PRNVAR(2,JCMPL(7)) .ne. 0)) then
          PRNVAR(2,ICMPL(3)) =
     1        PRNVAR(1,ICMPL(3)) + PRNVAR(2,JCMPL(7)) - 1
      else
          PRNVAR(2,ICMPL(3)) = 0
      endif
c
c...End of routine
c
 8000 return
c
c...Subscript out of range
c
 9000 call errtxt ('SUBOUND',cmsg)
      kerr   = 1
      go to 8000
      end
