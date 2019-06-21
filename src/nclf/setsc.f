C*********************************************************************
C*    NAME         :  setsc.f
C*       CONTAINS:
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       setsc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:40
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setsc
c*          C callable routine to set toler (sc(27)), maxdp (sc(54)) &
c*          auto maxdp values (sc(162)) depending on units.
C*       INPUT  : 
C*          ival -   0 = set inch values
C*                   1 = set mm values
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setsc(ival)

      include 'com4a.com'

      integer*2 ival
      real*4 asc162(2)
      equivalence (sc(162),asc162)

      if (ival .eq. 0) then
          sc(27) = 0.001
          sc(54) = 4
          asc162(2)=.01
          sc(167) = .001
          sc(168) = .0001
      else
          sc(27) = 0.025
          sc(54) = 100
          asc162(2)=.25
          sc(167) = .025
          sc(168) = .0001
      endif

      return
      end
