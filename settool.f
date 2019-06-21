c
c***********************************************************************
c
c   FILE NAME:  settool
c   CONTAINS:
c               set_tllen(tlno, tllen, tnum, ton)
c               set_units
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        settool.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:18
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  set_tllen (tln,tllen,tnum,ton)
c
c   FUNCTION:  This routine loads the tool length tables.
c
c   INPUT:  tln     R*8  D120 - List of tool numbers.
c
c           tllen   R*8  D120 - List of tool lengths.
c
c           tnum    I*4  D1   - Number of tool numbers/lengths.
c
c           ton     I*4  D1   -  =1, initialize tool table,
c                                =0, update tool table.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine set_tllen(tln, tllen, tnum, ton)

      include 'post.inc'
      integer*4 tln(120)
      real*8 tllen(120)
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-START
C     integer*4 tnum, ton
C VAX-SUN-SGI-CIM-IBM-OSF-HPX-DEC-DOS-END
C WNT-START
      integer*4 tnum[REFERENCE], ton[REFERENCE]
C WNT-END
      equivalence (TL, POSMAP(3601)), (TLNO, POSMAP(3841))
      real*8 TL(120), TLNO(120)

      integer*4 i, j

      if (ton.eq.1) then
        do 100 i=1, tnum, 1
           TLNO(i) = tln(i)
           TL(i)   = tllen(i)
  100   continue
        j = tnum+1
        do while (j.le.120 .and. TLNO(j).ne.0.d0)
           TLNO(j) = 0.d0
           j = j+1
        enddo
      else
        do i=1,tnum
          j = 1
          do while (j.le.120 .and. TLNO(j).ne.tln(i) .and.
     1              TLNO(j).ne.0.d0)
            j = j+1
          enddo
          if (j.le.120) then
            TLNO(j) = tln(i)
            TL(j)   = tllen(i)
          endif
        enddo
      endif
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  set_units (kunit)
c
c   FUNCTION:  This routine sets the default units.
c
c   INPUT:  tln     I*4  D1   - 1 = Inches, 2 = Millimeters.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine set_units(kunit)

      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087))
c
c...If units are different than active units
c...then convert post variables
c
      if (IUNIT .eq. 1) then
          if (kunit .eq. 2) call metini (2)
      else
          if (kunit .eq. 1) call metini (1)
      endif
c
c...Set units
c
      IUNIT = kunit
c
c...End of routine
c
 8000 return
      end
