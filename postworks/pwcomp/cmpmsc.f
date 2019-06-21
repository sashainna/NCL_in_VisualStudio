c
c***********************************************************************
c
c   FILE NAME:  cmpmsc
c   CONTAINS:
c               cmpwrt  getlab  getscl  lodcmp  lodlab  lodscl  stoscl
c               lodwrk  stowrk  fmtflt
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cmpmsc.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:12
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmpwrt (cmsg,kerr)
c
c   FUNCTION:  This routine stores a 1st pass compiled record.
c
c   INPUT:  none.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cmpwrt (cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      integer*4 i
c
c...Initialize routine
c
      kerr   = 0
c
c...Make compiled command end on
c...an even 8 byte boundary
c
      ICMPL(1) = ((ICMPL(1)-1)/4 + 1) * 4
c
c...Store compiled line in buffer
c
      do 200 i=1,ICMPL(1),1
          if (MFPT(3,5) .eq. 256) then
              MFDAT(2,5) = MFNXT
              call stowrk (MFDAT(1,5),MFDAT(1,5),cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c              MFPT(2,5) = MFPT(2,5) + 1
              MFPT(2,5) = MFNXT
              MFDAT(1,5) = MFNXT
              MFDAT(2,5) = 0
              MFPT(3,5) = 4
              MFNXT  = MFNXT  + 1
          endif
c
          MFPT(3,5) = MFPT(3,5) + 1
          MFIDAT(MFPT(3,5),5) = ICMPL(i)
  200 continue
      IPC    = IPC    + ICMPL(1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getlab (cdat,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine accepts a string as input and determines if
c              it is a predefined label.
c
c   INPUT:  cdat    C*24 D1  Input text string.
c
c   OUTPUT: kpt     I*4  D1  Returns the pointer within the label
c                            array when 'cdat' is a predefined label.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine getlab (cdat,kpt,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kpt,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 i,ipt,j,idat(2),ist,ilst
c
      character*24 ldat,udat
c
      equivalence (idat,ldat)
c
c...Initialize routine
c
      call touppr (cdat,udat)
      kerr   = 0
c
c...Check only the current macro label list
c
      ist    = MFDAT(IMACPT+1,1)
      ilst   = NLABEL
c
c...Check for Predefined Label
c
      do 500 i=ist,ilst,1
          call lodlab (i,ipt,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          do 400 j=1,6,1
              idat(j) = MFDAT(j+ipt-1,2)
  400     continue
          if (udat .eq. ldat) go to 1000
  500 continue
c
c...Did not find Variable
c
      kpt    = 0
      go to 8000
c
c...Found Variable
c
 1000 kpt    = ipt
      go to 8000
c
c...End of Routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  getscl (cdat,ktyp,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine accepts a string as input and determines if
c              it is a REAL or CHAR variable, or just a text string.
c
c   INPUT:  cdat    C*24 D1  Input text string.
c
c   OUTPUT: ktyp    I*4  D1  'cdat' type.  4 = Text string, 5 = Real
c                            variable, 6 = Text variable.
c
c           kpt     I*4  D1  Returns the pointer within the variable
c                            array when 'cdat' is a variable.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine getscl (cdat,ktyp,kpt,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 ktyp,kpt,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 i,j,ipt,idat(6),ist(4),ilst(4),m,is1(4),ityp(4)
c
      character*24 ldat,udat
c
      equivalence (idat,ldat)
c
c...Initialize routine
c
      call touppr (cdat,udat)
      kerr   = 0
c
c...Check the following variable lists
c...
c...1 = G$MAIN Real variables
c...2 = G$MAIN Char variables
c...3 = Current Real variables
c...4 = Current Char variables
c
      ist(1)  = MFMAIN(4)
      ilst(1) = MFMAIN(5)
      ityp(1) = 1
      is1(1)  = 3
c
      ist(2)  = MFMAIN(6)
      ilst(2) = MFMAIN(7)
      ityp(2) = 2
      is1(2)  = 4
c
      ist(3)  = MFDAT(IMACPT+3,1)
      ilst(3) = NSCAL(1)
      ityp(3) = 1
      is1(3)  = 3
c
      ist(4)  = MFDAT(IMACPT+5,1)
      ilst(4) = NSCAL(2)
      ityp(4) = 2
      is1(4)  = 4
c
c...Check variable lists for scalar
c
      m      = 1
      if (NMACRO .eq. 1) m = 3
      do 800 m=m,4,1
c
c...Check for Defined Variable
c
          do 500 i=ist(m),ilst(m),1
              call lodscl (ityp(m),i,ipt,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              do 400 j=1,6,1
                  idat(j) = MFDAT(j+ipt-1,is1(m))
  400         continue
              if (udat .eq. ldat) go to 1000
              ipt    = ipt    + MNREC(is1(m))
  500     continue
  800 continue
c
c...Did not find Variable
c
      ktyp   = 4
      go to 8000
c
c...Found Variable
c
 1000 ktyp   = 5
      if (ityp(m) .eq. 2) ktyp = 6
      kpt    = ipt
      go to 8000
c
c...End of Routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodcmp (kst,cmsg,kerr)
c
c   FUNCTION:  This routine loads the next compiled record into the
c              ICMPL array.
c
c   INPUT:  kst     I*4  D1  The pointer within the physical compiled
c                            record of the next logical record.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodcmp (kst,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kst,kerr
c
      character*(*) cmsg
c
      integer*4 icnt
c
c...Load up next compiled record
c
      if (kst .gt. 256) then
          call lodwrk (MFDAT(2,5),MFDAT(1,5),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          kst    = 5
      endif
c
c......Get number of I*'s in this record
c
      ICMPL(1) = MFIDAT(kst,5)
      icnt    = 1
c
c......Load the current record
c
  100 if (kst .eq. 256) then
          call lodwrk (MFDAT(2,5),MFDAT(1,5),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          kst    = 4
      endif
c
      kst    = kst    + 1
      icnt   = icnt   + 1
      ICMPL(icnt) = MFIDAT(kst,5)
      if (icnt .lt. ICMPL(1)) go to 100
      kst    = kst    + 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodlab (krel,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine loads the LABEL record that contains the
c              requested label.
c
c   INPUT:  krel    I*4  D1  The pointer within the Label array of the
c                            label needed to be loaded.
c
c   OUTPUT: kpt     I*4  D1  Returns the pointer within the currently
c                            loaded record for the label pointed to by
c                            'krel'.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodlab (krel,kpt,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 krel,kpt,kerr
c
      character*(*) cmsg
c
      integer*4 irec
c
c...Initialize routine
c
      kerr   = 0
c
c...Make sure this label exists
c
      if (krel .le. 0 .or. krel .gt. NLABEL+1) go to 9000
c
c...Calculate record number of
c...requested label
c
      irec   = (krel-1) / MFREC(2)
      kpt    = krel   - (irec*MFREC(2))
      irec   = irec   + 1
      kpt    = (kpt-1) * MNREC(2) + MNFIX(2) + 1
c
c...Determine if requested record
c...is already in memory
c...If not load that record
c
      if (irec .ne. ICREC(2)) then
c
c......Load the 1st Label record
c
          call stowrk (MFDAT(1,2),MFDAT(1,2),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call lodwrk (MFPT(1,2),MFDAT(1,2),cmsg,kerr)
          ICREC(2) = 1
c
c......Load the requested record
c
  200     if (irec .eq. ICREC(2)) go to 8000
          if (MFDAT(2,2) .eq. 0) go to 500
          call lodwrk (MFDAT(2,2),MFDAT(1,2),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          ICREC(2) = ICREC(2) + 1
          go to 200
c
c......There is no next record
c......Make sure the requested Label
c......is the 1st label on the next record
c
  500     if (irec .ne. ICREC(2)+1 .or. kpt .ne. MNFIX(2)+1)
     1            go to 9000
c
c......Allocate a new label record
c
          MFDAT(2,2) = MFNXT
          call stowrk (MFDAT(1,2),MFDAT(1,2),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          ICREC(2) = ICREC(2) + 1
          MFDAT(1,2) = MFNXT
          MFDAT(2,2) = 0
          MFNXT  = MFNXT  + 1
      endif
c
c...End of routine
c
 8000 return
c
c...Label does not exist
c...Compiler internal error
c
 9000 call errtxt ('NOSCALAR',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodscl (ktyp,krel,kpt,cmsg,kerr)
c
c   FUNCTION:  This routine loads the REAL or CHAR variable record that
c              contains the requested variable.
c
c   INPUT:  ktyp    I*4  D1  1 = load a REAL variable.  2 = load a CHAR
c                            variable.
c
c           krel    I*4  D1  The pointer within the real variable array
c                            of the variable needed to be loaded.
c
c   OUTPUT: kpt     I*4  D1  Returns the pointer within the currently
c                            loaded record for the variable pointed to
c                            by 'krel'.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine lodscl (ktyp,krel,kpt,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 krel,kpt,kerr,ktyp
c
      character*(*) cmsg
c
      integer*4 irec,is1,is2
c
c...Initialize routine
c
      kerr   = 0
      if (ktyp .eq. 1) then
          is1    = 1
          is2    = 3
      else
          is1    = 2
          is2    = 4
      endif
c
c...Make sure this variable exists
c
      if (krel .le. 0 .or. krel .gt. NSCAL(is1)+1) go to 9000
c
c...Calculate record number of
c...requested scalar
c
      irec   = (krel-1) / MFREC(is2)
      kpt    = krel   - (irec*MFREC(is2))
      irec   = irec   + 1
      kpt    = (kpt-1) * MNREC(is2) + MNFIX(is2) + 1
c
c...Determine if requested record
c...is already in memory
c...If not load that record
c
      if (irec .ne. ICREC(is2)) then
c
c......Load the 1st Real Scalar record
c
          call stowrk (MFDAT(1,is2),MFDAT(1,is2),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          call lodwrk (MFPT(1,is2),MFDAT(1,is2),cmsg,kerr)
          ICREC(is2) = 1
c
c......Load the requested record
c
  200     if (irec .eq. ICREC(is2)) go to 8000
          if (MFDAT(2,is2) .eq. 0) go to 500
          call lodwrk (MFDAT(2,is2),MFDAT(1,is2),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          ICREC(is2) = ICREC(is2) + 1
          go to 200
c
c......There is no next record
c......Make sure the requested Scalar
c......is the 1st scalar on the next record
c
  500     if (irec .ne. ICREC(is2)+1 .or. kpt .ne. MNFIX(is2)+1)
     1            go to 9000
c
c......Allocate a new scalar record
c
          MFDAT(2,is2) = MFNXT
          call stowrk (MFDAT(1,is2),MFDAT(1,is2),cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          ICREC(is2) = ICREC(is2) + 1
          MFDAT(1,is2) = MFNXT
          MFDAT(2,is2) = 0
          MFNXT  = MFNXT  + 1
      endif
c
c...End of routine
c
 8000 return
c
c...Scalar does not exist
c...Compiler internal error
c
 9000 call errtxt ('NOSCALAR',cmsg)
      kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stoscl (cdat,ktyp,kdim,kndim,cmsg,kerr)
c
c   FUNCTION:  This routine loads the REAL or CHAR variable record that
c              contains the requested variable.
c
c   INPUT:
c
c           cdat    C*n  D1  Scalar name.
c
c           ktyp    I*4  D1  1 = store a REAL variable.  2 = store a
c                            CHAR variable.
c
c           kdim    I*3  D3  Array sizes of scalar.
c
c           kndim   I*4  D1  Number of dimensions in scalar array.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred compiling
c
c***********************************************************************
c
      subroutine stoscl (cdat,ktyp,kdim,kndim,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 ktyp,kdim(3),kerr,kndim
c
      character*(*) cdat,cmsg
c
      integer*4 idim,is1,is2,isca(6),ipt,tdim(3),i
c
      character*24 lsca
c
      equivalence (isca,lsca)
c
c...Set up pointers based on scalar type
c......Real scalar
c
      lsca   = cdat
      do 10 i=1,kndim,1
          tdim(i) = kdim(i)
   10 continue
      do 20 i=kndim+1,3,1
          tdim(i) = 1
   20 continue
      if (ktyp .eq. 1) then
          is1    = 1
          is2    = 3
          idim   = tdim(1) * tdim(2) * tdim(3)
c
c......Text scalar
c
      else
          is1    = 2
          is2    = 4
          idim   = tdim(1) * tdim(2) * tdim(3)
          idim   = ((idim-1) / 2 + 1) * 2
      endif
c
c...Store scalar
c
      call lodscl (ktyp,NSCAL(is1)+1,ipt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      NSCAL(is1) = NSCAL(is1) + 1
      do 100 i=1,6,1
          MFDAT(ipt+i-1,is2) = isca(i)
  100 continue
      MFDAT(ipt+6,is2) = idim
      MFDAT(ipt+7,is2) = ISCAST(is1)
      ISCAST(is1) = ISCAST(is1) + idim
      if (ktyp .eq. 2) ISCAST(is1) = ISCAST(is1) + DESCHR
      MFDAT(ipt+8,is2) = kndim
      MFDAT(ipt+9,is2) = tdim(1)
      MFDAT(ipt+10,is2) = tdim(2)
      MFDAT(ipt+11,is2) = tdim(3)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  lodwrk (kpt,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine reads a record from the compiler work file
c              area.
c
c   INPUT:  kpt     I*4  D1  Record number from which to read data.
c
c   OUTPUT: kdat    I*4  Dn  Buffer to receive data.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine lodwrk (kpt,kdat,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kpt,kdat,kerr
c
      character*(*) cmsg
c
c...Store compiler work file record
c
      call rdmem (kpt,kdat,cmsg,kerr)
c      call rdprm (LUNMAC,MFDAT(2,5),MFDAT(1,5),cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stowrk (kpt,kdat,cmsg,kerr)
c
c   FUNCTION:  This routine stores a record in the compiler work file
c              area.
c
c   INPUT:  kpt     I*4  D1  Record number at which to store data.
c
c           kdat    I*4  Dn  Data to store.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine stowrk (kpt,kdat,cmsg,kerr)
c
      include 'compile.inc'
c
      integer*4 kpt,kdat,kerr
c
      character*(*) cmsg
c
c...Store compiler work file record
c
      call wrmem (kpt,kdat,cmsg,kerr)
c      call wrprm (LUNMAC,MFDAT(1,5),MFDAT(1,5),cmsg,kerr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  fmtflt (cdat,klft,krgt,cmsg,kerr)
c
c   FUNCTION:  This routine processes a text string in the format (m.n)
c              and returns the number to the left 'm' and right 'n' of
c              the decimal point.
c
c   INPUT:  cdat    C*n  D1  Text string to process.
c
c   OUTPUT: klft    I*4  D1  Number to the left of the decimal point.
c
c           krgt    I*4  D1  Number to the right of the decimal point.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine fmtflt (cdat,klft,krgt,cmsg,kerr)
c
      integer*4 klft,krgt,kerr
c
      character*(*) cdat,cmsg
c
      integer*4 index,strlen1,inc,ie
c
c...Get number of digits to the left
c...of the decimal point
c
      inc    = index (cdat,'.')
      if (inc .eq. 0) go to 9000
      call ctoi (cdat(1:inc-1),klft,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Get number of digits to the right
c...of the decimal point
c
      ie     = strlen1(cdat)
      if (ie .eq. inc) go to 9000
      call ctoi (cdat(inc+1:ie),krgt,kerr)
      if (kerr .ne. 0) go to 9000
c
c...End of routine
c
 8000 return
c
c...Invalid syntax
c
 9000 call errtxt ('INVPARAM',cmsg)
      kerr   = 1
      go to 8000
      end
