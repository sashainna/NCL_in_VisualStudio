c***********************************************************************
c
c   FILE NAME: banner.for
c   CONTAINS:
c               disban  dissec
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        banner.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:54
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  disban
c
c   FUNCTION:  This routine clears the screen & displays the Status
c              Banner on the 1st line.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine disban
c
      include 'menu.inc'
c
c...Display banner
c
      if (MOTIF .eq. 1) go to 500
      call clrscr
      call plott (1,1)
      call dmpbuf (SAPRM(IBANPT),SAPNC(IBANPT))
c
c...Display Section Levels
c
  500 call dissec
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  dissec
c
c   FUNCTION:  This routine displays the active section and level number
c              on the Status Banner line.  It uses the common variables
c              MLEVL & NLEVL to determine the current section and level.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine dissec
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (LMNAME,CPOSMP(0141))
c
      character*40 LMNAME
c
      integer*4 nc,ispt,strlen1
c
      character*20 lbuf
      character*80 sbuf
c
c...Get section level
c
      if (NLEVL .eq. 1) then
          sbuf = '0'
      else
          call levasc (MLEVL,NLEVL-1,sbuf)
      endif
c
c...Display section prompt
c
      if (ISECP1 .ne. 0) then
          call getsap (sbuf,ispt,IPRMDS,SALABL)
          if (MOTIF .eq. 1) then
              SAPRM(IBANPT)(ISECP1:ISECE1) = SAPRM(ispt)
          else
              nc     = ISECE1 - ISECP1 + 1
              call plott (1,ISECP1)
              call dmpbuf (SAPRM(ispt),nc)
          endif
      endif
c
c...Display machine name
c
      if (ISECP2 .ne. 0) then
          if (MOTIF .eq. 1) then
              SAPRM(IBANPT)(ISECP2:ISECE2) = LMNAME
          else
              nc     = ISECE2 - ISECP2 + 1
              call plott (1,ISECP2)
              call dmpbuf (lbuf,nc)
          endif
      endif
c
c...Display level prompt
c
      if (ISECP3 .ne. 0) then
          if (MOTIF .eq. 1) then
              SAPRM(IBANPT)(ISECP3:ISECE3) = sbuf
          else
              sbuf   = sbuf(1:strlen1(sbuf)) // '.'
              nc     = ISECE3 - ISECP3 + 1
              call plott (1,ISECP3)
              call dmpbuf (sbuf,nc)
          endif
      endif
c
 8000 return
      end
