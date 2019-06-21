C*********************************************************************
C*    NAME         :  nclver.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       nclver.f , 25.4
C*    DATE AND TIME OF LAST  MODIFICATION
C*       01/20/17 , 13:01:21
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclver
c*       display the current version number of ncl  
c*...COPYRIGHT notice also set in interface/iunihep.msg - RAZ.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine nclver

      include 'com4a.com'
      
      integer*4 bit

c     Open the scrolling window if not open already
c
      if (ifl(35).eq.0)  call opnwin()
c
      call getmchbit(bit)
      write (cout,1010) sc(119), bit
c...  format (13x, '***** NCL PROCESSOR VERSION ',f6.3,' WINDOWS ', 
1010  format (12x, '***** NCL PROCESSOR VERSION ',f8.3,' WINDOWS ', 
     x             i2 '-bit *****')
c 
c...Added check for NCL-VT mode 
c...Paul  -  10/22/91
c...Old version was:
c    call putmsg (cout, 69, 12, 1) 
c
      if( ifl(35) .eq. 2) then 
        call ersw3(15,1)
        call putmsg (cout, 79, 16, 1)
      else 
        call putmsg (cout, 79, 12, 1)
      endif

      cout(12:)='COPYRIGHT (C) NUMERICAL CONTROL COMPUTER SCIENCES'
      cout(61:)=' 1991-2019'

c
c...Added check for NCL-VT mode
c...Paul  -  10/22/91
c...Old version was:
c    call putmsg (cout, 71, 13, 0)
c
      if( ifl(35) .eq. 2) then
        call putmsg (cout, 71, 17, 5)
      else
        call putmsg (cout, 71, 13, 0)
      endif

      return
      end
