c
c***********************************************************************
c
c   FILE NAME: messag.f
c   CONTAINS:
c               messag  prompt
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        messag.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:09
c
c***********************************************************************
C WNT-START
      subroutine msgunix
      return
      end
C WNT-END
c
c***********************************************************************
c
c   SUBROUTINE:  messag (kmsg)
c
c   FUNCTION:  This routine displays a message on the form prompt line.
c
c   INPUT:  kmsg    I*4  D1    -  Number of message to display.  The
c                                 messages are stored in the 'SAERR'
c                                 array.
c
c   OUTPUT: none.
c
c***********************************************************************
c
C HPX-IBM-SGI-SUN-START
C     subroutine messag (kmsg)
c
C     include 'menu.inc'
c
C     integer*4 kmsg
C     character*80 lans, title
C     byte blans(80), btitle(80)
C     equivalence (lans,blans),(title,btitle)
c
C     integer*4 strlen1,nc, plen, tlen

C     if (MOTIF.eq.1) then
C         if ((kmsg.ge.21).and.(kmsg.le.24).and.(BATCHFILE.eq.1)) then
c
c...don't display these message if batch file
c
C              return
C         endif
C         lans = SAERR(kmsg)
C         plen = strlen1(lans)
C         title = 'NCCS_LICENSE Message' // char(0)
C         tlen = 21
c
c...Display message
c
C         call lic_mfmessag(blans, plen, btitle, tlen, BATCHFILE)
C         return
C     endif
c
c...Position cursor at prompt line
c
C     call plott (IPRLIN,1)
C     call clreol
c
c...Display message
c
C     nc     = strlen1(SAERR(kmsg))
C     call dmpbuf (SAERR(kmsg),nc)
c
c...End of routine
c
C8000 return
C     end
C HPX-IBM-SGI-SUN-END
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
C HPX-IBM-SGI-SUN-START
C     subroutine prompt (kpr,kans)
c
C     include 'menu.inc'
c
C     integer*4 kpr,kans, lic_mfprompt
c
C     integer*4 strlen1,nc,nc1,ilin, mlen, tlen
c
C     character*80 lans, title
C     byte blans(80), btitle(80)
C     equivalence (lans,blans),(title,btitle)

C     if (MOTIF.eq.1) then
C         if ((BATCHFILE.eq.1).and.(kpr.eq.2)) then
c... 'Record already exists.  Do you want to replace it?'
c... we don't ask but replace it when batch
c
C             kans = 1
C             return
C         endif
C         lans = SAPRM(kpr)
C         mlen = strlen1(lans)
C         tlen = 21
C         title = 'NCCS_LICENSE Question'
C         kans = lic_mfprompt(blans, mlen, btitle, tlen, BATCHFILE)
C         return
C     endif
c
c...Set up prompt line data
c
C     if (kpr .eq. 1) then
C         ilin   = -1
C     else
C         ilin   = IPRLIN
C         call plott (IPRLIN,1)
C         call clreol
C     endif
c
c...Display prompt & get answer
c
C     nc     = strlen1(SAPRM(kpr)) + 1
C     call dmpbuf (SAPRM(kpr),nc)
C     nc1   = 0
C     call getlin (lans,nc1,3,ilin,nc+2)
C     if (ilin .lt. 0) call trmnl (1)
C     kans   = 2
C     if (nc1 .ge. 1 .and. (lans(1:1) .eq. 'Y' .or.
C    1    lans(1:1) .eq. 'y')) kans = 1
c
c...End of routine
c
C8000 return
C     end
C HPX-IBM-SGI-SUN-END
