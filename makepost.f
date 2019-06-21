c
c***********************************************************************
c
c   FILE NAME: makepost.for
c   CONTAINS:
c               makepost
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        makepost.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:56
c
c***********************************************************************
c
c***********************************************************************
c
c   PROGRAM:     makepost
c
c   FUNCTION:  This is the main routine for the makepost program.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      program makepost
c
      include 'menu.inc'
      include 'docum.inc'
      include 'post.inc'
c
      integer*4 ierr,ivnc,irnc
c
      character*80 msg,vmsg,rmsg
c
c...Initialize routine
c
      MOTIF  = 0
      call mninit (msg,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Pass control to the master routine
c
      call master
c
c...End of routine
c
 8000 call trmnl (1)
      call trmrst
      call clsfil (0)
      call exit
c
c...An error occurred during
c...execution of program
c
 9000 call trmmsg (' ')
      call trmmsg (msg)
      if (ierr .lt. 0) then
          call errhnd (vmsg,ivnc,rmsg,irnc)
          if (ivnc .ne. 0) call trmmsg (vmsg)
          if (irnc .ne. 0) call trmmsg (rmsg)
      endif
c
      call trmmsg (' ')
      go to 8000
      end
