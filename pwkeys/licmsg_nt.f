c
c***********************************************************************
c
c   FILE NAME: licmsg_nt.f
c   CONTAINS:
c               messag  prompt
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        licmsg_nt.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:09
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  messag (kmsg)
c
c   FUNCTION:  This routine displays a message on a message box.
c
c   INPUT:  kmsg    I*4  D1    -  Number of message to display.  The
c                                 messages are stored in the 'SAERR'
c                                 array.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C WNT-START
      subroutine messag (kmsg)
c
      include 'menu.inc'
c
      integer*4 kmsg, ret
c
      integer*4 strlen1,nc,flag
      character*80 lans

      if (BATCHFILE.eq.1) then
c
c...don't display these message if batch file
c
         return
      endif
      flag = 0
      lans = SAERR(kmsg)
      nc = strlen1(lans)
c
c...Display message
c
      call add1dispmsg(lans, nc, flag)
c
c...End of routine
c
 8000 return
      end
C WNT-END
c
c***********************************************************************
c
c   SUBROUTINE:  prompt (kpr,kans)
c
c   FUNCTION:  This routine prompts the user for a YES/NO response.  The
c              prompt will be displayed on the form prompt line.
c
c   INPUT:  kmsg    I*4  D1    -  Number of prompt to display.  The
c                                 prompts are stored in the 'SAERR' ar-
c                                 ray.
c
c   OUTPUT: kans    I*4  D1    -  1 = Yes, 2 = No.
c
c***********************************************************************
c
C WNT-START
      subroutine prompt (kpr,kans)
c
      include 'menu.inc'
c
      integer*4 kpr,kans, ans
c
      integer*4 strlen1,nc,nc1,ilin
c
      character*80 lans, title

c
c...Display prompt & get answer
c
      if ((BATCHFILE.eq.1).and.(kpr.eq.2)) then
c... 'Record already exists.  Do you want to replace it?'
c... we don't ask but replace it when batch
c
         kans = 1
         return
      endif
      lans = SAPRM(kpr)
      nc = strlen1(lans)
      title = "NCCS_LICENSE Question"
      nc1 = strlen1(title)
      call getyesno(lans, nc, title, nc1, ans)
      if (ans .eq. 1) then
          kans = 1
      else
          kans = 2
      endif
c
c...End of routine
c
 8000 return
      end
C WNT-END
