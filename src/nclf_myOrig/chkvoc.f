C*********************************************************************
C*    NAME         :  chkvoc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       chkvoc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:40
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine chkvoc
C*       purpose of program: to check the prefix of geometry name against
C*       Vocabulary words by calling vocsch.
C*       suffix.
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
C
      subroutine chkvoc(name,ncltyp,gtyp,lsub)

      include 'com4a.com'


      integer*2 gtyp, ncltyp, inm(4)
      character*64 name, temp
      integer*4 lsub
      equivalence (temp, inm)

      token2 = name
      temp = name
      ivxsub =0
      call vstchk
      if (ityp.eq.2 .and. ist.eq.1) call vocsch
c
c                If the prefix is already been used.
      if (ityp .eq. 1 .and. (ist .ge. 602 .and.
     x                       ist .le. 610 .or.
     x                       ist .eq. 636)) then
          gtyp = ist -600
          if (ist .eq. 602) gtyp = 18
          if (ist .eq. 636) gtyp = 20
c
c                If the prefix is already been used.
      else if (ityp .eq. 2 .and. (ist .gt. 2 .and.
     x                            ist .le. 10 .or.
     x                            ist .eq.20 .or. 
     x                            ist .eq. 18)) then
          gtyp = ist
      else
          gtyp = ityp
      endif
      if (lsub.eq.1 .and. gtyp .eq. ncltyp) gtyp = 1

      return
      end
