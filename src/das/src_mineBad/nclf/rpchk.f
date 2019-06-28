C*********************************************************************
C*    NAME         :  rpchk.f
C*       CONTAINS:
C*					rpchk   rpset   fdchk  fdset
C*    COPYRIGHT 1984 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       rpchk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:38
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rpchk (rpst)
C*                checks to see if the RAPID flag is set.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine rpchk (rpfl)
c

      include 'com8a.com'

      integer*2 rpfl

c
c
c    set the RAPID flag.
c
      if (rpfron) then
		rpfl = 1
      else
	    rpfl = 0
      endif
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rpset (rpfl)
c
c   FUNCTION:  Sets the rapid flag.
c
c   INPUT:  rpfl    I*2  D1  -  1 = Puts RAPID into effect.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rpset (rpfl)
c
      include 'com8a.com'
c
      integer*2 rpfl
c
c...Turn RAPID on
c
      if (rpfl .eq. 1) then
          rpfron = .true.
c
c...Turn RAPID off
c
      else
          rpfron = .false.
      endif
c
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fdchk (kftyp,gfeed)
C*          Returns the current feed rate and mode.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          kftyp   I*4   D1   Feed rate type.  0 = Rapid, 1 = FPM,
C*                             2 = FPR.
C*
C*          gfeed   R*8   D1   Current feed rate.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine fdchk (kftyp,gfeed)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 kftyp
c
      real*8 gfeed
c
c...Check the RAPID flag
c
      if (RPFRON) then
		    kftyp = 0
          gfeed = 0.
c
c...Return the FEDRAT mode
c
      else
	       kftyp = FITYP
          gfeed = CFEED(FITYP)
      endif
c
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fdset (kftyp,gfeed)
C*          Sets the current feed rate and mode.
C*    PARAMETERS   
C*       INPUT  : 
C*          kftyp   I*4   D1   Feed rate type.  0 = Rapid, 1 = FPM,
C*                             2 = FPR.
C*
C*          gfeed   R*8   D1   Current feed rate.
C*       OUTPUT :  
C*          none
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine fdset (kftyp,gfeed)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*4 kftyp
c
      real*8 gfeed
c
c...Set the RAPID flag
c
      if (kftyp .eq. 0) then
          RPFRON = .true.
c
c...Set the FEDRAT mode
c
      else
	       FITYP = kftyp
          CFEED(FITYP) = gfeed
      endif
c
      return
      end
