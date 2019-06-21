c
c***********************************************************************
c
c   FILE NAME: misc.for
c   CONTAINS:
c               datcnv  dpoint  jindex  njndex  copyn  copynk
c               copynv  copykv  gtmach  limadj  sort    swapg
c               swapk   datcnvc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        misc.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:17
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  datcnv (cinp,cout)
c
c   FUNCTION:  This routine converts a revision date format 'mm.dd.yy'
c              to an actual date format 'dd-mmm-yyyy'.
c
c   INPUT:  cinp    C*n  D1  -  Revision date in the format 'mm.dd.yy'.
c
c   OUTPUT: cout    C*n  D1  -  Actual date string in the format
c                               'dd-mmm-yyyy'.
c
c***********************************************************************
c
      subroutine datcnv (cinp,cout)
c
      character*(*) cinp,cout
c
      integer*4 iary(3),inc,index,ierr,nc1,nc2,strlen1,ipt
c
      character*3 ldat(12)
      character*4 lc1,lc2
c
      data ldat /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1           'OCT','NOV','DEC'/
c
c...Get month
c
      nc1    = strlen1(cinp)
      inc    = index(cinp,'.')
      if (inc .eq. 0) go to 9000
      call ctoi (cinp(1:inc-1),iary(2),ierr)
      if (ierr .ne. 0) go to 9000
c
c...Get day
c
      ipt    = inc    + index(cinp(inc+1:nc1),'.')
      if (ipt .eq. 0) go to 9000
      call ctoi (cinp(inc+1:ipt-1),iary(1),ierr)
      if (ierr .ne. 0) go to 9000
c
c...Get year
c
      call ctoi (cinp(ipt+1:nc1),iary(3),ierr)
      if (ierr .ne. 0) go to 9000
      if (iary(3) .lt. 90) iary(3) = iary(3) + 100
      iary(3) = iary(3) + 1900
c
c...Format output date string
c
      call itoc (iary(1),lc1,nc1,2)
      call itoc (iary(3),lc2,nc2,4)
      cout   = lc1(1:nc1) // '-' // ldat(iary(2)) // '-' // lc2(1:nc2)
c
c...End of routine
c
 8000 return
c
c...Error in input date format
c
 9000 cout   = ' '
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  dpoint (ginp,gout,kacy)
c
c   FUNCTION:  This routine rounds off a real number to the specified
c              accuracy.
c
c   INPUT:  ginp    R*8  D1  -  Number to round off.
c
c   OUTPUT: gout    R*8  D1  -  Rounded off number.
c
c           kacy    I*4  D1  -  Accuracy to round off number at.
c
c***********************************************************************
c
      subroutine dpoint (ginp,gout,kacy)
c
      real*8 ginp,gout
c
      integer*4 kacy
c
c...Round off number
c
      gout   = dnint(ginp*10.0D0**kacy) / 10.0D0**kacy
      return
      end
c
c***********************************************************************
c
c   E-FUNCTION:  jindex (kary,knum,ksiz)
c
c   FUNCTION:  This routine returns the first location of 'kary' that
c              contains the number 'knum'.
c
c   INPUT:  kary    I*4  Dn  -  Array of values to search.
c
c           knum    I*4  D1  -  Number to search for.
c
c           ksiz    I*4  D1  -  Size of array 'kary'.
c
c   OUTPUT: jindex  I*4  D1  -  Pointer within 'kary' to position that
c                               contains the value 'knum'.  Returns 0 if
c                               the array does not contain the value
c                               'knum'.
c
c***********************************************************************
c
      integer*4 function jindex (kary,knum,ksiz)
c
      integer*4 kary(*),knum,ksiz
c
      integer*4 i
c
      jindex = 0
c
c...Search for 1st occurrence of 'knum' in 'kary'
c
      do 100 i=1,ksiz,1
          if (knum .eq. kary(i)) go to 200
  100 continue
      go to 8000
c
c...Found value
c
  200 jindex = i
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   E-FUNCTION:  njndex (kary,knum,ksiz)
c
c   FUNCTION:  This routine returns the first location of 'kary' that
c              does not contain the number 'knum'.
c
c   INPUT:  kary    I*4  Dn  -  Array of values to search.
c
c           knum    I*4  D1  -  Number to search for.
c
c           ksiz    I*4  D1  -  Size of array 'kary'.
c
c   OUTPUT: jindex  I*4  D1  -  Pointer to first non-occurance of 'knum'
c                               within 'kary'.  Returns 0 if 'kary'
c                               consists solely of 'knum' values.
c
c***********************************************************************
c
      integer*4 function njndex (kary,knum,ksiz)
c
      integer*4 kary(*),knum,ksiz
c
      integer*4 i
c
      njndex = 0
c
c...Search for 1st occurrence of 'knum' in 'kary'
c
      do 100 i=1,ksiz,1
          if (knum .ne. kary(i)) go to 200
  100 continue
      go to 8000
c
c...Found value
c
  200 njndex = i
c
c...End of routine
c
 8000 return
      end
c
c******************************************************************
c
c     SUBROUTINE: copyn (gpt1,gpt2,knum)
c
c     FUNCTION:  This routine copies 'knum' of R*8 numbers from
c                'gpt1' array to the 'gpt2' array.
c
c******************************************************************
      subroutine copyn (gpt1,gpt2,knum)
c
      real*8 gpt1(*),gpt2(*)
      integer*4 knum,i
c
      do 110 i=1,knum
          gpt2(i) = gpt1(i)
  110 continue
      return
      end
c
c******************************************************************
c
c     SUBROUTINE: copynk (kpt1,kpt2,knum)
c
c     FUNCTION:  This routine copies 'knum' of I*4 numbers from
c                'kpt1' array to the 'kpt2' array.
c
c******************************************************************
      subroutine copynk (kpt1,kpt2,knum)
c
      integer*4 kpt1(*),kpt2(*)
      integer*4 knum,i
c
      do 110 i=1,knum
          kpt2(i) = kpt1(i)
  110 continue
      return
      end
c
c******************************************************************
c
c     SUBROUTINE: copynv (gval,gpt2,knum)
c
c     FUNCTION:  This routine stores 'knum' of R*8 numbers of the
c                value 'gval' to the 'gpt2' array.
c
c******************************************************************
      subroutine copynv (gval,gpt2,knum)
c
      real*8 gval,gpt2(*)
      integer*4 knum,i
c
      do 110 i=1,knum
          gpt2(i) = gval
  110 continue
      return
      end
c
c******************************************************************
c
c     SUBROUTINE: copykv (kval,kpt2,knum)
c
c     FUNCTION:  This routine stores 'knum' of I*4 numbers of the
c                value 'kval' to the 'kpt2' array.
c
c******************************************************************
      subroutine copykv (kval,kpt2,knum)
c
      integer*4 kval,kpt2(*),knum
      integer*4 i
c
      do 110 i=1,knum
          kpt2(i) = kval
  110 continue
      return
      end
c
c******************************************************************
c
c   SUBROUTINE: gtmach (kset)
c
c   FUNCTION:  This routine returns type of active machine.
c
c   INPUT:   none
c
c   OUTPUT: kset    I*4  D1  -  Machine type set in MDF file as:
c                               1 = mill, 2 = lathe, 3 = blade
c
c******************************************************************
      subroutine gtmach (kset)
c
      include 'post.inc'
      integer*4 kset
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHPCL,KPOSMP(1899))
c
      integer*4 MACHTP,LTHPCL(2)
c
      kset   = 1
      if (MACHTP .eq. 1) then
          kset = 1
      else if (MACHTP .eq. 2) then
          kset = 2
      else if (MACHTP .eq. 3) then
          kset = 3
      else if (MACHTP .eq. 4) then
          kset = 2
      end if
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  limadj (kdin,kdsv)
c
c   FUNCTION:  This routine changes X axis limits when switching from
c              radius to diameter output programming and vcve.
c
c
c   INPUT:  kdin   I*4   D1  -  New output programming mode (LTHDIA(2)).
c
c           kdsv   I*4   D1  -  current output programming mode.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine limadj (kdin,kdsv)
c
      include 'post.inc'
      integer*4 kdin,kdsv
c
      equivalence (LIMITS,POSMAP(1254))
c
      real*8 LIMITS(2,10)
c
      if (kdin .ne. kdsv) then
         if (kdin .eq. 1) then
            LIMITS(1,1) = .5 * LIMITS(1,1)
            LIMITS(2,1) = .5 * LIMITS(2,1)
         else
            LIMITS(1,1) = 2. * LIMITS(1,1)
            LIMITS(2,1) = 2. * LIMITS(2,1)
         end if
      end if
c
      return
      end
c
c******************************************************************
c
c   SUBROUTINE: sort (gnum,kinx,knum)
c
c   FUNCTION:  Shell sort routine for real number with associated
c              integer index array.
c
c   INPUT:   gnum   R*8  D(knum) - input array to sort.
c
c            kinx   I*4  D(knum) - index array.
c
c            knum   I*4  D1      - number of entities in gnum.
c
c   OUTPUT:  gnum   R*8  D(knum) - sorted array.
c
c            kinx   I*4  D(knum) - index array after sort.
c
c******************************************************************
c
      subroutine sort (gnum,kinx,knum)
c
      real*8 gnum(8)
      integer*4 kinx(8),knum
c
      integer*4 i,j, jump, j2,j3
c
      jump   = knum
   10 jump   = jump / 2
         if (jump .eq. 0) go to 8000
            j2 = knum - jump
            do 300 j=1,j2
   15          i  = j
   20          j3 = i + jump
               if (gnum(i) .le. gnum(j3)) go to 300
               call swapg (gnum(i),gnum(j3))
               call swapk (kinx(i),kinx(j3))
               i  = i - jump
               if (i .gt. 0) go to 20
  300       continue
            go to 10
 8000 return
      end
c
c******************************************************************
c
c   SUBROUTINE: swapg (gnm1,gnm2)
c
c   FUNCTION:  Swaping routine for real numbers.
c
c******************************************************************
      subroutine swapg (gnm1,gnm2)
c
      real*8 gnm1,gnm2
c
      real*8 rnum
c
      rnum   = gnm1
      gnm1   = gnm2
      gnm2   = rnum
c
      return
      end
c
c******************************************************************
c
c   SUBROUTINE: swapk (knm1,knm2)
c
c   FUNCTION:  Swaping routine for integer numbers.
c
c******************************************************************
      subroutine swapk (knm1,knm2)
c
      integer*4 knm1,knm2
c
      integer*4 knum
c
      knum   = knm1
      knm1   = knm2
      knm2   = knum
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  datcnvc (cinp,cout)
c
c   FUNCTION:  This routine converts a revision date format 'mm.dd.yy'
c              to an actual date format 'dd-mmm-yyyy'.
c			This function can be called from C++ function
c
c   INPUT:  cinp    B*1  D8  -  Revision date in the format 'mm.dd.yy'.
c
c   OUTPUT: cout    B*1  D12 -  Actual date string in the format
c                               'dd-mmm-yyyy'.
c
c***********************************************************************
c
      subroutine datcnvc (binp,bout)
c
      byte binp(8)
      byte bout(12)
      byte tmpinp(8)
      byte tmpout(12)
      character*8 cinp
      character*12 cout
      integer*4 nc
      equivalence (cinp,tmpinp)
      equivalence (cout,tmpout)
c
      call pwdbtc (binp,cinp,nc)
      call datcnv (cinp,cout)
      call pwdctb (cout,bout)
      return
      end
