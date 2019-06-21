c
c***********************************************************************
c
c   FILE NAME: lodcmp.for
c   CONTAINS:
c               lodcmp  lodcod  lodmac  loddim  lodrel  lodtxt  memall
c               memdea  stomac  stovar  storel  stotxt  stoarg  inisto
c               savsto  rststo
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        lodcmp.f , 24.4
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/14 , 12:59:53
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  lodcmp (klun,cmsg,kerr)
c
c   FUNCTION:  This routine loads the compiled Macro code file.  It also
c              allocates the memory needed to store the code.
c
c   INPUT:  klun    I*4  D1    -  Unit number of Macro code file.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred try-
c                                 ing to allocate memory.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodcmp (klun,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
c
      integer*4 klun,kerr
c
      character*(*) cmsg
c
      integer*4 jdat(128),i,ipt,nupd,nrev,nobj
c
      character*8 lrev
      equivalence (lrev,jdat(9))
c
c...Read header record
c
      call rdprm (klun,1,jdat,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      do 100 i=1,8,1
          JHED(i) = jdat(i)
  100 continue
c
c...Make sure that this Object file
c...is valid with this version of the program
c
      call cnvday (lrev,nupd)
      if (nupd .eq. 10000000) lrev = ''
      call cnvday (REVDAT,nrev)
      call cnvday (OBJUPD,nobj)
      if (nrev .lt. nupd .or. nupd .lt. nobj) then
          call errtxt ('OBJVER',cmsg)
          call errstr (cmsg,lrev,0)
          call errstr (cmsg,REVDAT,0)
          kerr   = 1
          go to 8000
      endif
c
c...Allocate compiled code memory
c
      call memall (jdat(6),cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store 1st record in memory
c
      call cstmem (jdat,1,MEMPT(1))
c
c...Read rest of compiled code
c
      ipt    = 129
      do 500 i=2,JHED(6),1
          call rdprm (klun,i,jdat,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call cstmem (jdat,ipt,MEMPT(1))
          ipt    = ipt    + 128
  500 continue
c
c...End of routine
c...Close Macro file
c
 8000 call clsfil (klun)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodcod (kcmp,kpc)
c
c   FUNCTION:  This routine loads a compiled Macro instruction from al-
c              located memory.
c
c   INPUT:  kpc     I*4  D1    -  PC from which to load instruction
c                                 from.
c
c   OUTPUT: kcmp    I*2  Dn    -  Array to receive instruction.
c
c***********************************************************************
c
      subroutine lodcod (kcmp,kpc)
c
      include 'pregen.inc'
c
      integer*2 kcmp(*)
      integer*4 kpc
c
      integer*4 ipt,iend
c
c...Load the Macro code instruction
c
      ipt    = (JHED(5)-1) * 256 + kpc + 1
      call clodi2 (MEMPT(1),kcmp,ipt,ipt)
      iend   = ipt    + kcmp(1) - 1
      call clodi2 (MEMPT(1),kcmp,ipt,iend)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodmac (ktyp,kdat)
c
c   FUNCTION:  This routine searches the Macro header portion of memory
c              for the requested Macro and returns its header if found.
c
c   INPUT:  ktyp    I*4  D1    -  Post word value of Macro to search
c                                 for.
c
c   OUTPUT: kdat    I*4  D4    -  Buffer to receive macro header.
c                                 kdat(1) = 0 when the requested macro
c                                 was not found.
c
c***********************************************************************
c
      subroutine lodmac (ktyp,kdat)
c
      include 'pregen.inc'
      integer*4 ktyp,kdat(*)
c
      integer*4 iend,i,jnum,ipt
c
      integer*2 inum(2)
      equivalence (jnum,inum)
c
c...Initialize routine
c
      ipt    = (JHED(1)-1) * 128 + ((IHED(14)+1)/2)
      iend   = IHED(13)
c
c...Check for Macro definition
c
      do 100 i=1,iend,1
          call clodi4 (MEMPT(1),kdat,ipt,ipt+4)
          if (ktyp .eq. kdat(1)) go to 8000
          ipt    = ipt    + 5
  100 continue
c
c...Macro not found
c
      kdat(1) = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  loddim (ktyp,gnum,kvar,kdim,cmsg,kerr)
c
c   FUNCTION:  This routine loads a variable's address and array
c              size from allocated memory.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           gnum    R*8  D2    -  Real variable descriptor.  Contains
c                                 2 I*4's containing the variable
c                                 descriptor.  (1) = pointer to variable,
c                                 (2) = subscript pointer.  For a text
c                                 variable contains the pointe to the
c                                 variable (1st I*4) and either the start
c                                 and end of the string or the subscript
c                                 number.
c
c   OUTPUT: kvar    I*4  D1    -  Address (pointer to) real variable.
c
c           kdim    I*4  D1    -  Array size of variable.
c
c           cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine loddim (ktyp,gnum,kvar,kdim,cmsg,kerr)
c
      include 'pregen.inc'
      include 'post.inc'
c
      integer*2 ktyp
      integer*4 kerr,kvar,kdim
c
      real*8 gnum(2)
c
      character*(*) cmsg
c
      integer*2 ibuf,idesc(2)
      integer*4 ipt,isub,jdesc,ist,ien
c
      real*8 rnum(2)
c
      integer*4 jnum(4)
      equivalence (rnum,jnum), (idesc,jdesc)
c
c...Non-subscripted variable
c
      if (ktyp .eq. 1) then
          rnum(1) = gnum(1)
          if (jnum(1) .le. MAXPRN) go to 9000
          kvar   = jnum(1)
          ipt    = (JHED(2)-1) * 256 + jnum(1) - MAXPRN
          call clodi2 (MEMPT(1),ibuf,ipt,ipt)
          kdim   = ibuf
c
c...Subscripted variable
c
      else if (ktyp .eq. 2) then
          rnum(1) = gnum(1)
          ipt    = (JHED(2)-1) * 256 + jnum(1) - MAXPRN
          call clodi2 (MEMPT(1),ibuf,ipt,ipt)
          if (PRNVAR(1,jnum(2)) .gt. ibuf) go to 9100
          kvar   = jnum(1) + jnum(2) - 1
          kdim   = ibuf   - jnum(2) + 1
c
c...Non-subscripted text variable
c
      else if (ktyp .eq. 4) then
          rnum(1) = gnum(1)
          rnum(2) = gnum(2)
          if (jnum(1) .le. MAXPRN) go to 9000
          kvar   = jnum(1)
          ipt    = (JHED(4)-1) * 256 + (jnum(1)-MAXPRN)/2 + 1
          call clodi2 (MEMPT(1),ibuf,ipt,ipt)
          kdim   = ibuf
c
c...Subscripted text variable
c
      else if (ktyp .eq. 5) then
          rnum(1) = gnum(1)
          rnum(2) = gnum(2)
          ipt    = (JHED(4)-1) * 256 + (jnum(1)-MAXPRN)/2 + 1
          call clodi2 (MEMPT(1),ibuf,ipt,ipt)
          jdesc  = jnum(2)
          ist    = PRNVAR(1,idesc(1))
          ien    = PRNVAR(2,idesc(1))
          kvar   = jnum(1) + ist    - 1
          kdim   = ibuf
          if (ien-ist+1 .lt. kdim) kdim = ien - ist + 1
c
c...Macro argument
c
      else if (ktyp .eq. 12) then
          rnum(1) = gnum(1)
c
c......Get the subscript
c
          isub   = jnum(2)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
          if (isub .le. 0 .or. isub .gt. IMACHD(2,IMACPT)) go to 9100
c
c......Define the address & dimension
c
          kvar   = IMACHD(3,IMACPT) + isub   - 1
          kdim   = IMACHD(2,IMACPT) - isub   + 1
c
c...Clfile record data
c
      else if (ktyp .eq. 15) then
          rnum(1) = gnum(1)
c
c......Get the subscript
c
          isub   = jnum(2)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
          if (isub .le. 0 .or. isub .gt. MAXCLD) go to 9100
c
c......Define the address & dimension
c
          kvar   = isub
          kdim   = MAXCLD - isub   + 1
c
c...Unrecognized variable type
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Unrecognized variable type
c
 9000 call errtxt ('NUMVAREX',cmsg)
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
c   SUBROUTINE:  lodrel (ktyp,gnum,gans,cmsg,kerr)
c
c   FUNCTION:  This routine loads a real variable's value from allocated
c              memory.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           gnum    R*8  D1    -  Real variable descriptor.  Contains
c                                 either a number or 2 I*4's containing
c                                 the variable descriptor.  (1) = point-
c                                 er to variable, (2) = subscript point-
c                                 er.
c
c   OUTPUT: gans    R*8  D1    -  Returns variable's value.
c
c           cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodrel (ktyp,gnum,gans,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
      include 'pted.inc'
      include 'ptedpost.inc'
c
      integer*2 ktyp
      integer*4 kerr
c
      real*8 gnum,gans
c
      character*(*) cmsg
c
      equivalence (NCLD  ,KPOSMP(0076))
c
      integer*4 NCLD
c
      integer*2 ibuf
      integer*4 jnum(2),ipt,inc,isub,jindex
c
      real*8 rnum
c
      equivalence (rnum,jnum)
c
c...Non-subscripted variable
c
      if (ktyp .eq. 1) then
          rnum   = gnum
          if (jnum(1) .le. MAXPRN) then
              gans   = PRNVAR(1,jnum(1))
          else
              ipt    = (JHED(3)-1) * 64 + jnum(1) - MAXPRN
          call clodr8 (MEMPT(1),gans,ipt,ipt)
          endif
c
c...Subscripted variable
c
      else if (ktyp .eq. 2) then
          rnum   = gnum
          ipt    = (JHED(2)-1) * 256 + jnum(1) - MAXPRN
          call clodi2 (MEMPT(1),ibuf,ipt,ipt)
          if (PRNVAR(1,jnum(2)) .gt. ibuf) go to 9100
          ipt    = (JHED(3)-1) * 64 + jnum(1) + PRNVAR(1,jnum(2)) - 1
     1             - MAXPRN
          call clodr8 (MEMPT(1),gans,ipt,ipt)
c
c...Number
c
      else if (ktyp .eq. 3) then
          gans   = gnum
c
c...Macro argument
c
      else if (ktyp .eq. 12) then
          rnum   = gnum
c
c......Get the subscript
c
          isub   = jnum(2)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
          if (isub .le. 0 .or. isub .gt. IMACHD(2,IMACPT)) go to 9100
c
c......Load the argument's value
c
          ipt    = (JHED(3)-1) * 64 + IMACHD(3,IMACPT) + isub - 1 -
     1             MAXPRN
          call clodr8 (MEMPT(1),gans,ipt,ipt)
c
c...Post Variable
c
      else if (ktyp .eq. 13) then
          rnum   = gnum
          if (jnum(1) .eq. 2) then
              gans   = IMACHD(5,IMACPT)
          else
              inc    = jindex (KPSTVR,jnum(1),MAXKVR)
              if (inc .ne. 0) then
                  gans   = KPOSMP(KPSTVL(inc))
              else
                  inc    = jindex (RPSTVR,jnum(1),MAXRVR)
                  if (inc .eq. 0) go to 9000
                  gans   = POSMAP(RPSTVL(inc))
              endif
          endif
c
c...Subscripted Post Variable
c
      else if (ktyp .eq. 14) then
          rnum   = gnum
c
c......Get the subscript
c
          isub   = jnum(2)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
c
c......Integer variable
c
          inc    = jindex (KPSTVR,jnum(1),MAXKVR)
          if (inc .ne. 0) then
              if (isub .le. 0 .or. isub .gt. KPSTSB(inc)) go to 9100
              if (PGMNAM .eq. 'Pted' .and.
     1            PCNV_TYPE .eq. PCNV_CONVERT .and.
     2            (jnum(1) .eq. 56 .or. jnum(1) .eq. 60)) then
                  gans   = OKPOSMP(KPSTVL(inc)+isub-1)
              else
                  gans   = KPOSMP(KPSTVL(inc)+isub-1)
              endif
c
c......Real variable
c
          else
              inc    = jindex (RPSTVR,jnum(1),MAXRVR)
              if (inc .eq. 0) go to 9000
              if (isub .le. 0 .or. isub .gt. RPSTSB(inc)) go to 9100
              if (PGMNAM .eq. 'Pted' .and.
     1            PCNV_TYPE .eq. PCNV_CONVERT .and.
     2            (jnum(1) .eq. 56 .or. jnum(1) .eq. 60)) then
                  gans   = OPOSMAP(RPSTVL(inc)+isub-1)
              else
                  gans   = POSMAP(RPSTVL(inc)+isub-1)
              endif
          endif
c
c...Clfile Record Data
c
      else if (ktyp .eq. 15) then
          rnum   = gnum
c
c......Get the subscript
c
          isub   = jnum(2)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
c
c......Get the variable
c
          if (isub .le. 0 .or. isub .gt. NCLD) go to 9100
          gans   = CLDATA(isub)
c
c...Unrecognized variable type
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Unrecognized variable type
c
 9000 call errtxt ('NUMVAREX',cmsg)
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
c   SUBROUTINE:  lodtxt (ktyp,kdesc,kst,cdat,knc,kfl,cmsg,kerr)
c
c   FUNCTION:  This routine loads a text variable's string from allocat-
c              ed memory.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           kdesc   I*2  D4    -  Text variable descriptor.  Contains
c                                 the pointer to the variable (1st I*4)
c                                 and either the start and end of the
c                                 string or the subscript number.
c
c           kst     I*4  D1    -  Pointer to the beginning of the text
c                                 string, within the ICMPL array, when
c                                 the string is stored in the instruc-
c                                 tion whether than a variable.  'kst'
c                                 will be modified to point to the next
c                                 text string in the instruction in this
c                                 case.
c
c           kfl     I*4  D1    -  0 = Text strings are aligned on 2-byte
c                                 boundaries, 1 = Text strings must be
c                                 aligned on 8-byte boundaries.
c
c   OUTPUT: cdat    C*n  D1    -  Returns variable's text string.
c
c           knc     I*4  D1    -  Number of chars in 'cdat'.
c
c           cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodtxt (ktyp,kdesc,kst,cdat,knc,kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
c
      integer*2 ktyp,kdesc(4)
      integer*4 kst,knc,kerr,kfl
c
      character*(*) cdat,cmsg
c
      equivalence (NCLD  ,KPOSMP(0076))
      equivalence (REGBNC,KPOSMP(2001)), (REGENC,KPOSMP(3522))
c
      integer*4 NCLD,REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*2 ibuf(16384),inum(4)
      integer*4 ipt,ist,ien,inc,isub,strlen1,jnum(2),ienfl
c
      character*32768 lbuf
c
      equivalence (ibuf,lbuf), (inum,jnum)
c
c...Non-subscripted variable
c
      inum(1) = kdesc(1)
      inum(2) = kdesc(2)
      inum(3) = kdesc(3)
      inum(4) = kdesc(4)
      if (ktyp .eq. 4) then
c
c......Parenthesis variable
c
          if (jnum(1) .le. MAXPRN) then
              ist    = PRNVAR(1,jnum(1))
              ien    = PRNVAR(2,jnum(1))
              if (ien .eq. 0) then
                  knc    = 0
              else
                  knc    = ien    - ist    + 1
                  cdat(1:knc) = LPRNTX(ist:ien)
              endif
c
c......Text variable
c
          else
              ipt    = (JHED(4)-1) * 256 + (jnum(1)-MAXPRN)/2 + 1
              call clodi2 (MEMPT(1),ibuf,ipt,ipt+1)
              if (ibuf(2) .eq. 0) then
                  knc    = 0
              else
                  ist    = kdesc(3)
                  ien    = kdesc(4)
                  if (ist .le. 0 .or. ien .gt. ibuf(1)) go to 9100
                  knc    = ien    - ist    + 1
                  if (ibuf(2) .lt. knc) then
                      knc = ibuf(2)
                      ien    = ist    + knc    - 1
                  endif
                  ipt    = ipt    + 2
                  inc    = ipt    + (ibuf(1)-1) / 2
                  call clodi2 (MEMPT(1),ibuf,ipt,inc)
                  cdat(1:knc) = lbuf(ist:ien)
              endif
          endif
c
c...Subscripted variable
c
      else if (ktyp .eq. 5) then
          ipt    = (JHED(4)-1) * 256 + (jnum(1)-MAXPRN)/2 + 1
          call clodi2 (MEMPT(1),ibuf,ipt,ipt+1)
          if (ibuf(2) .eq. 0) then
              knc    = 0
          else
              ist    = PRNVAR(1,kdesc(3))
              ien    = PRNVAR(2,kdesc(3))
              if (ien .eq. 0) ien = ist
              ienfl  = 0
              if (ien .lt. 0) then
                  ien    = ien    * -1
                  ienfl = 1
              endif
              if (ist .le. 0 .or. ist .gt. ibuf(1) .or.
     1            ist .gt. ien .or. ien .gt. ibuf(1)) go to 9100
c
              if (ist .gt. ibuf(2)) then
                  knc    = 0
c
              else
                  if (ien .gt. ibuf(2)) ien = ibuf(2)
                  knc    = ien    - ist    + 1
c                  if (ibuf(2) .lt. knc) then
c                      knc = ibuf(2)
c                      ien    = ist    + knc    - 1
c                  endif
                  ipt    = ipt    + 2
                  inc    = ipt    + (ibuf(1)-1) / 2
                  call clodi2 (MEMPT(1),ibuf,ipt,inc)
c
c......Multiple subscript text variables
c......need to calculate their string length
c
                  if (ienfl .eq. 1) then
                      knc    = strlen1(lbuf(ist:ien))
                      ien    = ist    + knc    - 1
                  endif
                  cdat(1:knc) = lbuf(ist:ien)
              endif
          endif
c
c...Text string
c
      else if (ktyp .eq. 6) then
          if (ICMPL(kst) .eq. 0) then
              knc    = 0
          else
              knc    = ICMPL(kst)
              ist    = kst    * 2      + 1
              ien    = ist    + knc    - 1
              cdat(1:knc) = LCMPL(ist:ien)
              if (kfl .eq. 0) then
                  kst    = kst    + (knc-1) / 2 + 1
              else
                  kst    = kst    + ((knc-6+7)/8) * 4
cc                  kst    = kst    + (knc-1) / 8 * 4
              endif
          endif
c
c...Macro argument
c
      else if (ktyp .eq. 12) then
c
c......Get the subscript
c
          inum(1) = kdesc(3)
          inum(2) = kdesc(4)
          isub   = jnum(1)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
          if (isub .le. 0 .or. isub .gt. IMACHD(2,IMACPT)) go to 9100
c
c......Load the argument's value
c
          ipt    = (JHED(4)-1) * 256 + (IMACHD(4,IMACPT)-MAXPRN)/2 + 1
          call clodi2 (MEMPT(1),ibuf,ipt,ipt+1)
          if (ibuf(2) .eq. 0) then
              knc    = 0
          else
              if ((IMACHD(1,IMACPT) .ge. 1043 .and.
     1            IMACHD(1,IMACPT) .le. 1046) .or.
     2            IMACHD(1,IMACPT) .eq. 1102 .or.
     3            IMACHD(1,IMACPT) .eq. 1103) then
                  ist    = 1
                  ien    = ibuf(2)
              else
                  ist    = (isub-1) * 24 + 1
                  ien    = ist    + 23
              endif
              knc    = ien    - ist    + 1
              if (ibuf(2) .lt. knc) then
                  knc = ibuf(2)
                  ien    = ist    + knc    - 1
              endif
              ipt    = ipt    + 2
              inc    = ipt    + (ibuf(1)-1) / 2
              call clodi2 (MEMPT(1),ibuf,ipt,inc)
              cdat(1:knc) = lbuf(ist:ien)
              knc    = strlen1(cdat(1:knc))
              if (knc .eq. 0) knc = 1
          endif
c
c...Text Post Variable
c
      else if (ktyp .eq. 18) then
c
c......Get the subscript
c
          isub   = jnum(2)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
c
c......Get the text string
c
          if (isub .le. 0 .or. isub .gt. MAXFMT) go to 9100
          if (inum(1) .eq. 71) then
              knc    = REGBNC(isub)
              cdat(1:knc) = REGST(isub)
          else if (inum(1) .eq. 72) then
              knc    = REGENC(isub)
              cdat(1:knc) = REGEN(isub)
          else
              knc    = strlen1(PSTNAM(PREPT))
              cdat(1:knc) = PSTNAM(PREPT)
          endif
c
c...Clfile record data
c
      else if (ktyp .eq. 15) then
c
c......Get the subscript
c
          inum(1) = kdesc(3)
          inum(2) = kdesc(4)
          isub   = jnum(1)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
          if (isub .le. 0 .or. isub .gt. NCLD) go to 9100
c
c.....Get the command word
c
          if (isub .eq. 3) then
              if (CLDATA(3) .eq. 1000) then
                  cdat   = "ISN"
                  knc    = 3
              else if (CLDATA(3) .eq. 2000) then
                  ist    = CLDATA(4)
                  call getvwd (ist,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
              else if (CLDATA(3) .eq. 3000) then
                  ist    = 4026
                  call getvwd (ist,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
              else if (CLDATA(3) .eq. 5000 .or. CLDATA(3) .eq. 5200)
     1                then
                  ist    = 4013
                  call getvwd (ist,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
              else if (CLDATA(3) .eq. 5210 .or. CLDATA(3) .eq. 5220)
     1                then
                  ist    = 4019
                  call getvwd (ist,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
              else if (CLDATA(3) .eq. 6000 .or. CLDATA(3) .eq. 7100)
     1                then
                  ist    = 4025
                  call getvwd (ist,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
              else if (CLDATA(3) .eq. 9000) then
                  ist    = 1105
                  call getvwd (ist,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
              else if (CLDATA(3) .eq. 14000) then
                  ist    = 4012
                  call getvwd (ist,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
              else
                  cdat   = " "
                  knc    = 1
              endif

c
c......Get the post command word
c
          else if (isub .eq. 4) then
              if (CLDATA(3) .eq. 2000 .or. CLDATA(3) .eq. 9000) then
                  ist    = CLDATA(4)
                  call getvwd (ist,cdat,knc,1,PSTWRD,PSTWVL,NPSTWD)
              else
                  cdat   = ' '
                  knc    = 0
              endif
c
c......Text command
c
          else if (CLDATA(3) .eq. 2000 .and.
     1        ((CLDATA(4) .ge. 1043 .and.
     2        CLDATA(4) .le. 1046) .or.
     3        CLDATA(4) .eq. 1102 .or.
     4        CLDATA(4) .eq. 1103)) then
              ist    = isub * 8 - 7 + 16
              ien    = ist  + 65
              if (isub .ne. 5) ien = ist + 7
              cdat   = CLTEXT(ist:ien)
              knc    = strlen1(cdat)
c
c......Post-processor command
c
          else if (CLDATA(3) .eq. 2000) then
              ist    = (isub-2) * 24 - 23
              ien    = ist  + 23
              cdat   = CLTEXT(ist:ien)
              knc    = strlen1(cdat)
c
c......Other commands
c
          else
              ien    = ist + 8 - 1
              cdat   = ' '
              knc    = 0
          endif
          if (knc .eq. 0) knc = 1
c
c...Unrecognized variable type
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Unrecognized variable type
c
 9000 call errtxt ('TXTVAREX',cmsg)
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
c   SUBROUTINE:  memall (ksiz,cmsg,kerr)
c
c   FUNCTION:  This routine allocates a chunk of memory.
c
c   INPUT:  ksiz    I*4  D1    -  Amount of memory to allocate in 512
c                                 byte blocks.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred try-
c                                 ing to allocate memory.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c
c***********************************************************************
c
      subroutine memall (ksiz,cmsg,kerr)
c
      include 'pregen.inc'
c
      integer*4 ksiz,kerr
c
      character*(*) cmsg
C WNT-DOS-START
      integer*4 idat(1)
C WNT-DOS-END
c
c
c...Allocate memory from the current
c...chunk to the requested chunk
c
      MEMSIZ = ksiz
C SUN-SGI-IBM-HPX-DEC-START
C      call cmall (MEMSIZ*512,MEMPT)
C      if (MEMPT(1) .eq. 0) go to 9000
C SUN-SGI-IBM-HPX-DEC-END
C WNT-START
          MEMPT(1) = MALLOC(MEMSIZ*512)
          if (MEMPT(1) .eq. 0) go to 9000
  100 continue

C WNT-END  		
c
c...End of routine
c
 8000 return
c
c...Error trying to allocate memory
c
 9000 kerr   = -1
      call errtxt ('MEMALL',cmsg)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  memdea
c
c   FUNCTION:  This routine deallocates a chunk of memory.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine memdea
c
      include 'pregen.inc'
c
c
c...Allocate memory from the current
c...chunk to the requested chunk
c
      call cmdea (MEMPT)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stomac (kdat)
c
c   FUNCTION:  This routine stores the Macro header in allocated memory.
c
c   INPUT:  kdat    I*4  D4    -  Macro header buffer.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine stomac (kdat)
c
      include 'pregen.inc'
c
      integer*4 kdat(5)
c
      integer*4 iend,i,jdat(5),ipt
c
c...Initialize routine
c
      ipt    = (JHED(1)-1) * 128 + ((IHED(14)+1)/2)
      iend   = IHED(13)
c
c...Check for Macro definition
c
      do 100 i=1,iend,1
          call clodi4 (MEMPT(1),jdat,ipt,ipt+4)
          if (kdat(1) .eq. jdat(1)) go to 1000
          ipt    = ipt + 5
  100 continue
      go to 8000
c
c...Store Macro header
c
 1000 call cstoi4 (MEMPT(1),kdat,ipt,ipt+4)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stovar (ktyp,kdesc,kvar,gnum,cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine calls the appropriate routine to store the
c              specified variable type.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           kdesc   I*4  D2    -  Variable descriptor.  1 = Post vari-
c                                 able number.  2 = subscript.
c
c           kvar    I*4  D1    -  Type of data to store in variable.
c                                 1 = Real number.  2 = Text string.
c
c           gnum    I*4  D1    -  Real value to store in variable.
c
c           cdat    C*n  D1    -  Text string to store in variable.
c
c           knc     I*4  D1    -  Number of chars in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine stovar (ktyp,kdesc,kvar,gnum,cdat,knc,cmsg,kerr)
c
      integer*2 ktyp
      integer*4 kdesc(2),kvar,knc,kerr
c
      real*8 gnum
c
      character*(*) cdat,cmsg
c
c...Real variable
c
      if (ktyp .eq. 1 .or. ktyp .eq. 2) then
          call storel (ktyp,kdesc(1),kdesc(2),gnum,cmsg,kerr)
c
c...Text variable
c
      else if (ktyp .eq. 4 .or. ktyp .eq. 5) then
          call stotxt (ktyp,kdesc,cdat,knc,cmsg,kerr)
c
c...Post-processor variable or
c...Macro argument
c
      else
          call stoarg (ktyp,kdesc,kvar,gnum,cdat,knc,cmsg,kerr)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  storel (ktyp,kvar,ksub,gnum,cmsg,kerr)
c
c   FUNCTION:  This routine stores a real value into a real variable
c              within allocated memory.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           kvar    I*4  D1    -  Pointer to variable.
c
c           ksub    I*4  D1    -  Variable's subscript pointer.
c
c           gnum    R*8  D1    -  Value to store in variable.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine storel (ktyp,kvar,ksub,gnum,cmsg,kerr)
c
      include 'pregen.inc'
c
      integer*2 ktyp
      integer*4 kerr,kvar,ksub
c
      real*8 gnum
c
      character*(*) cmsg
c
      integer*2 ibuf
      integer*4 jnum(2),ipt
c
      real*8 rnum
c
      equivalence (rnum,jnum)
c
c...Clfile look ahead is active
c...Save this variable's value prior to changing it
c
      if (PLOOK .eq. 1) call savsto (ktyp,kvar,ksub,cmsg,kerr)
c
c...Non-subscripted variable
c
      if (ktyp .eq. 1) then
          if (kvar .le. MAXPRN) then
              PRNVAR(1,kvar) = gnum
              PRNVAR(2,kvar) = 0.
          else
              ipt    = (JHED(3)-1) * 64 + kvar - MAXPRN
              call cstor8 (MEMPT(1),gnum,ipt,ipt)
          endif
c
c...Subscripted variable
c
      else if (ktyp .eq. 2) then
          ipt    = (JHED(2)-1) * 256 + kvar - MAXPRN
          call clodi2 (MEMPT(1),ibuf,ipt,ipt)
          if (PRNVAR(1,ksub) .gt. ibuf) go to 9100
          ipt    = (JHED(3)-1) * 64 + kvar + PRNVAR(1,ksub) - 1
     1             - MAXPRN
          call cstor8 (MEMPT(1),gnum,ipt,ipt)
c
c...Unrecognized variable type
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Unrecognized variable type
c
 9000 call errtxt ('NUMVAREX',cmsg)
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
c   SUBROUTINE:  stotxt (ktyp,kdesc,cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine stores a text string into a text variable
c              within allocated memory.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           kdesc   I*2  D4    -  Text variable descriptor.  Contains
c                                 the pointer to the variable (1st I*4)
c                                 and either the start and end of the
c                                 string or the subscript number.
c
c           cdat    C*n  D1    -  Text string to store in variable.
c
c           knc     I*4  D1    -  Number of chars in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine stotxt (ktyp,kdesc,cdat,knc,cmsg,kerr)
c
      include 'pregen.inc'
c
      integer*2 ktyp,kdesc(4)
      integer*4 knc,kerr
c
      character*(*) cdat,cmsg
c
      integer*2 ibuf(16384),inum(2)
      integer*4 ipt,ist,ien,inc,is,nc,jnum
c
      character*32768 lbuf
c
      equivalence (ibuf,lbuf), (jnum,inum)
c
c...Clfile look ahead is active
c...Save this variable's value prior to changing it
c
      if (PLOOK .eq. 1) call savcst (ktyp,kdesc,cmsg,kerr)
c
c...Non-subscripted variable
c
      inum(1) = kdesc(1)
      inum(2) = kdesc(2)
      if (ktyp .eq. 4) then
c
c......Parenthesis variable
c
          if (jnum .le. MAXPRN) then
              if (IPNTX .gt. 512 .or. knc .eq. 0) then
                  PRNVAR(1,jnum) = 0
                  PRNVAR(2,jnum) = 0
              else
                  nc     = knc
                  ien    = IPNTX  + knc    - 1
                  if (ien .gt. 512) then
                      ien    = 512
                      nc     = ien    - IPNTX  + 1
                  endif
                  LPRNTX(IPNTX:ien) = cdat(1:nc)
                  PRNVAR(1,jnum) = IPNTX
                  PRNVAR(2,jnum) = ien
                  IPNTX  = IPNTX  + nc
              endif
c
c......Text variable
c
          else
              IPNTX  = 1
              ipt    = (JHED(4)-1) * 256 + (jnum-MAXPRN)/2 + 1
              call clodi2 (MEMPT(1),ibuf,ipt,ipt+1)
              if (kdesc(3) .le. 0 .or. kdesc(4) .gt. ibuf(1)) go to 9100
c
              if (knc .eq. 0) then
                  if (kdesc(3) .lt. ibuf(2)) ibuf(2) = kdesc(3) - 1
                  call cstoi2 (MEMPT(1),ibuf,ipt,ipt+1)
              else
                  ist    = kdesc(3)
                  ien    = kdesc(4)
                  inc    = ipt    + (ibuf(1)-1) / 2 + 2
                  call clodi2 (MEMPT(1),ibuf,ipt,inc)
                  if (ist .gt. ibuf(2)+1) then
                      is     = ibuf(2) + 1
                      lbuf(is+4:ist+3) = ' '
                  endif
                  if (ist .eq. 1 .and. ien .eq. ibuf(1)) then
                      ien    = knc
                      ibuf(2) = knc
                  endif
                  nc     = ien    - ist    + 1
                  if (knc .lt. nc) ien = ist + knc - 1
                  lbuf(ist+4:ien+4) = cdat(1:knc)
                  if (ien .gt. ibuf(2)) ibuf(2) = ien
                  call cstoi2 (MEMPT(1),ibuf,ipt,inc)
              endif
          endif
c
c...Subscripted variable
c
      else if (ktyp .eq. 5) then
          IPNTX  = 1
          ipt    = (JHED(4)-1) * 256 + (jnum-MAXPRN)/2 + 1
          call clodi2 (MEMPT(1),ibuf,ipt,ipt+1)
          ist    = PRNVAR(1,kdesc(3))
          ien    = PRNVAR(2,kdesc(3))
          if (ien .lt. 0) ien = ien * -1
          if (ien .eq. 0) ien = ist
          if (ist .le. 0 .or. ist .gt. ibuf(1) .or.
     1        ist .gt. ien .or. ien .gt. ibuf(1)) go to 9100
c
          if (knc .eq. 0) then
              if (ist .lt. ibuf(2)) ibuf(2) = ist - 1
              call cstoi2 (MEMPT(1),ibuf,ipt,ipt+1)
          else
              inc    = ipt    + (ibuf(1)-1) / 2 + 2
              call clodi2 (MEMPT(1),ibuf,ipt,inc)
              if (ist .gt. ibuf(2)+1) then
                  is     = ibuf(2) + 1
                  lbuf(is+4:ist+3) = ' '
              endif
              nc     = ien    - ist    + 1
              if (nc .lt. knc) ien = ist + nc - 1
              lbuf(ist+4:ien+4) = cdat(1:knc)
              if (ien .gt. ibuf(2)) ibuf(2) = ien
              call cstoi2 (MEMPT(1),ibuf,ipt,inc)
          endif
c
c...Unrecognized variable type
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Unrecognized variable type
c
 9000 call errtxt ('TXTVAREX',cmsg)
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
c   SUBROUTINE:  stoarg (ktyp,kdesc,kvar,gnum,cdat,knc,cmsg,kerr)
c
c   FUNCTION:  This routine stores a value in a Macro argument or Post
c              variable.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           kdesc   I*4  D2    -  Variable descriptor.  1 = Post vari-
c                                 able number.  2 = subscript.
c
c           kvar    I*4  D1    -  Type of data to store in variable.
c                                 1 = Real number.  2 = Text string.
c
c           gnum    I*4  D1    -  Real value to store in variable.
c
c           cdat    C*n  D1    -  Text string to store in variable.
c
c           knc     I*4  D1    -  Number of chars in 'cdat'.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine stoarg (ktyp,kdesc,kvar,gnum,cdat,knc,cmsg,kerr)
c
      include 'menu.inc'
      include 'pregen.inc'
      include 'post.inc'
      include 'pted.inc'
      include 'ptedpost.inc'
c
      integer*2 ktyp
      integer*4 kdesc(2),kvar,knc,kerr
c
      real*8 gnum
c
      character*(*) cdat,cmsg
c
      equivalence (REGFRC,KPOSMP(0603))
      equivalence (REGBNC,KPOSMP(2001)), (REGENC,KPOSMP(3522))
c
      integer*4 REGFRC(MAXFMT),REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*2 ibuf(256),ityp
      integer*4 ipt,inc,isub,ist,ien,jindex,inum
c
      character*512 lbuf
      equivalence (ibuf,lbuf)
c
c...Macro argument
c
      if (ktyp .eq. 12) then
c
c......Get the subscript
c
          isub   = kdesc(2)
          if (isub .lt. 0) then
              isub   = isub * (-1)
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
          if (isub .le. 0 .or. isub .gt. IMACHD(2,IMACPT)) go to 9100
c
c......Store the argument's text value
c
          ipt    = (JHED(4)-1) * 256 + (IMACHD(4,IMACPT)-MAXPRN)/2 + 1
          call clodi2 (MEMPT(1),ibuf,ipt,ipt+1)
          if ((IMACHD(1,IMACPT) .ge. 1043 .and.
     1        IMACHD(1,IMACPT) .le. 1046) .or.
     2        IMACHD(1,IMACPT) .eq. 1102 .or.
     3        IMACHD(1,IMACPT) .eq. 1103) then
              if (knc .gt. IMACHD(5,IMACPT)*8)
     1            knc = IMACHD(5,IMACPT) * 8
              ist    = 5
              ien    = knc    + 4
              ibuf(2) = knc
          else
              ist    = (isub-1) * 24 + 5
              ien    = ist    + 23
              if (knc .gt. 24) knc = 24
          endif
          inc    = ipt    + (ibuf(1)-1) / 2 + 2
          call clodi2 (MEMPT(1),ibuf,ipt,inc)
          lbuf(ist:ien) = cdat(1:knc)
          call cstoi2 (MEMPT(1),ibuf,ipt,inc)
c
c......Store the argument's numeric value
c
          ityp   = 1
          inum   = IMACHD(3,IMACPT) + isub - 1
          call storel (ityp,inum,inum,gnum,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...Post Variable
c
      else if (ktyp .eq. 13) then
          if (kdesc(1) .eq. 2) then
              IMACHD(5,IMACPT) = gnum
          else
              inc    = jindex (KPSTVR,kdesc(1),MAXKVR)
              if (inc .ne. 0) then
                  KPOSMP(KPSTVL(inc)) = gnum
              else
                  inc    = jindex (RPSTVR,kdesc(1),MAXRVR)
                  if (inc .eq. 0) go to 9000
                  POSMAP(RPSTVL(inc)) = gnum
              endif
          endif
c
c...Subscripted Post Variable
c
      else if (ktyp .eq. 14) then
c
c......Get the subscript
c
          isub   = kdesc(2)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
c
c......Integer variable
c
          inc    = jindex (KPSTVR,kdesc(1),MAXKVR)
          if (inc .ne. 0) then
              if (isub .le. 0 .or. isub .gt. KPSTSB(inc)) go to 9100
              if (PGMNAM .eq. 'Pted' .and.
     1            PCNV_TYPE .eq. PCNV_CONVERT .and.
     2            (kdesc(1) .eq. 56 .or. kdesc(1) .eq. 60)) then
                  OKPOSMP(KPSTVL(inc)+isub-1) = gnum
                  if (kdesc(1) .eq. 60 .and. gnum .eq. 0.)
     1                REGFRC(isub) = 0
              else
                  KPOSMP(KPSTVL(inc)+isub-1) = gnum
              endif
c
c......Real variable
c
          else
              inc    = jindex (RPSTVR,kdesc(1),MAXRVR)
              if (inc .eq. 0) go to 9000
              if (isub .le. 0 .or. isub .gt. RPSTSB(inc)) go to 9100
              if (PGMNAM .eq. 'Pted' .and.
     1            PCNV_TYPE .eq. PCNV_CONVERT .and.
     2            (kdesc(1) .eq. 56 .or. kdesc(1) .eq. 60)) then
                  OPOSMAP(RPSTVL(inc)+isub-1) = gnum
              else
                  POSMAP(RPSTVL(inc)+isub-1) = gnum
              endif
          endif
c
c...Clfile record data
c
      else if (ktyp .eq. 15) then
c
c......Get the subscript
c
          isub   = kdesc(2)
          if (isub .lt. 0) then
              isub   = isub * (-1)
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
          if (isub .le. 0 .or. isub .gt. MAXCLD) go to 9100
c
c......Store the text value
c
          if (knc .ne. 0) then
              ipt    = isub * 24 - 23
              ien    = ipt + ((knc-1)/24 * 24) + 23
              if (CLDATA(3) .eq. 2000 .and. ((CLDATA(4) .ge. 1043 .and.
     1            CLDATA(4) .le. 1046) .or.  CLDATA(4) .eq. 1102 .or.
     2            CLDATA(4) .eq. 1103) .and. ipt .eq. 33) ien = ipt + 65
              CLTEXT(ipt:ien) = cdat(1:knc)
          endif
c
c......Store numeric value
c
          CLDATA(isub) = gnum
c
c...Subscripted Text Post Variable
c
      else if (ktyp .eq. 18) then
c
c......Get the subscript
c
          isub   = kdesc(2)
          if (isub .lt. 0) then
              isub   = 0 - isub
              if (PRNVAR(2,isub) .ne. 0) go to 9100
              isub   = PRNVAR(1,isub)
          endif
c
c......Store text
c
          if (isub .le. 0 .or. isub .gt. MAXFMT) go to 9100
          if (kdesc(1) .eq. 71) then
              REGST(isub) = cdat(1:knc)
              REGBNC(isub) = knc
          else if (kdesc(1) .eq. 72) then
              REGEN(isub) = cdat(1:knc)
              REGENC(isub) = knc
          endif
c
c...Unrecognized variable type
c
      else
          go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Unrecognized variable type
c
 9000 call errtxt ('NUMVAREX',cmsg)
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
c   SUBROUTINE:  inisto
c
c   FUNCTION:  This routine initializes the Real array storage to prepare
c              for looking ahead in the clfile.  During look ahead, up to
c              500 Real Macro Variable values will be saved.  These
c              values will be restored at the end of the look ahead
c              sequence.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine inisto
c
      include 'pregen.inc'
c
c...Initialize look ahead variables
c
      PLOOK  = 1
      NSAVRL = 0
      MSAVRL = 500
      ISAVPX = 1
      MSAVPX = 4000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  savsto (ktyp,kvar,ksub,cmsg,kerr)
c
c   FUNCTION:  This routine saves a Real Macro Variable during a look
c              ahead sequence.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           kvar    I*4  D1    -  Pointer to variable.
c
c           ksub    I*4  D1    -  Variable's subscript pointer.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine savsto (ktyp,kvar,ksub,cmsg,kerr)
c
      include 'pregen.inc'
c
      integer*2 ktyp
      integer*4 kerr,kvar,ksub
c
      character*(*) cmsg
c
      integer*4 jnum(2),i
c
      real*8 rnum
c
      equivalence (rnum,jnum)
c
c...See if this variable is already saved
c
      if (NSAVRL .eq. MSAVRL) go to 8000
      if (ktyp .eq. 1 .and. kvar .le. MAXPRN) go to 8000
      do 100 i=1,NSAVRL,1
          if (ktyp .eq. 2) then
              if (ktyp .eq. SAVIRL(1,i) .and. kvar .eq. SAVIRL(2,i)
     1            .and. ksub .eq. SAVIRL(3,i)) go to 8000
          else
              if (ktyp .eq. SAVIRL(1,i) .and. kvar .eq. SAVIRL(2,i))
     1                go to 8000
          endif
  100 continue
c
c...This variable needs to be saved
c
      NSAVRL = NSAVRL + 1
      SAVIRL(1,NSAVRL) = ktyp
      SAVIRL(2,NSAVRL) = kvar
      SAVIRL(3,NSAVRL) = ksub
      if (ktyp .eq. 2) SAVIRL(4,NSAVRL) = PRNVAR(1,ksub)
      jnum(1) = kvar
      jnum(2) = ksub
      call lodrel (ktyp,rnum,SAVGRL(NSAVRL),cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  savcst (ktyp,kdesc,cmsg,kerr)
c
c   FUNCTION:  This routine saves a Text Macro Variable during a look
c              ahead sequence.
c
c   INPUT:  ktyp    I*2  D1    -  Variable type.
c
c           kdesc   I*2  D4    -  Text variable descriptor.  Contains
c                                 the pointer to the variable (1st I*4)
c                                 and either the start and end of the
c                                 string or the subscript number.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine savcst (ktyp,kdesc,cmsg,kerr)
c
      include 'pregen.inc'
c
      integer*2 ktyp,kdesc(4)
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*2 inum(4)
      integer*4 jnum(2),i,nc
c
      character*512 ldat
c
      equivalence (jnum,inum)
c
c...Initialize routine
c
      inum(1) = kdesc(1)
      inum(2) = kdesc(2)
      inum(3) = kdesc(3)
      inum(4) = kdesc(4)
c
c...See if this variable is already saved
c
      if (NSAVRL .eq. MSAVRL) go to 8000
      if (ktyp .eq. 4 .and. jnum(1) .le. MAXPRN) go to 8000
      if (ktyp .eq. 6) go to 8000
      do 100 i=1,NSAVRL,1
          if (ktyp .eq. SAVIRL(1,i) .and. jnum(1) .eq. SAVIRL(2,i)
     1        .and. jnum(2) .eq. SAVIRL(3,i)) go to 8000
  100 continue
c
c...This variable needs to be saved
c
      call lodtxt (ktyp,kdesc,0,ldat,nc,0,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (ISAVPX+nc .gt. MSAVPX) go to 8000
c
      NSAVRL = NSAVRL + 1
      SAVIRL(1,NSAVRL) = ktyp
      SAVIRL(2,NSAVRL) = jnum(1)
      SAVIRL(3,NSAVRL) = jnum(2)
      SAVIRL(4,NSAVRL) = ISAVPX
      SAVIRL(5,NSAVRL) = nc
      if (nc .ne. 0) then
          SAVGTX(ISAVPX:ISAVPX+nc-1) = ldat(1:nc)
          ISAVPX = ISAVPX + nc
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rststo (cmsg,kerr)
c
c   FUNCTION:  This routine restores Real Macro Variables to their
c              values prior to the look ahead sequence.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1    -  Error text when an error occurred.
c
c           kerr    I*4  D1    -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine rststo (cmsg,kerr)
c
      include 'pregen.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i
c
c...Restore the values changed
c...during a look ahead sequence
c...to their original values
c
      PLOOK  = 0
      do 100 i=1,NSAVRL,1
c
c......Real variables
c
          if (SAVIRL(1,i) .lt. 4) then
              if (SAVIRL(1,i) .eq. 2)
     1            PRNVAR(1,SAVIRL(3,i)) = SAVIRL(4,i)
              call storel (SAVIRL(1,i),SAVIRL(2,i),SAVIRL(3,i),
     1                     SAVGRL(i),cmsg,kerr)
c
c......Text variable
c
          else
              call stotxt (SAVIRL(1,i),SAVIRL(2,i),SAVGTX(SAVIRL(4,i):),
     1                     SAVIRL(5,i),cmsg,kerr)
          endif
  100 continue
      NSAVRL = 0
      ISAVPX = 0
c
c...End of routine
c
 8000 return
      end
