c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ersall.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:01
c**
c*****************************************************
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: ersall                                         **
c **                                                                  **
c **  purpose of program: to clear terminal screen                    **
c **                                                                  **
c **********************************************************************
c **                                                                  **
c **  subroutine name: erslin                                         **
c **                                                                  **
c **  purpose of program: to clear to end of line on terminal screen  **
c **                                                                  **
c **********************************************************************
c **                                                                  **
c **  subroutine name: crsplt                                         **
c **                                                                  **
c **  purpose of program: to position the curser to a row specified   **
c **    by 'lx' and to a column specified by 'ly'                     **
c **                                                                  **
c **********************************************************************
c **                                                                  **
c **  subroutine name: ersw3                                          **
c **                                                                  **
c **  purpose of program: to erase the rest of window three from the  **
c **    position given by 'lx' (row) and 'ly' (column)                **
c **                                                                  **
c **********************************************************************
c **********************************************************************
 
      subroutine ersall
 
      include 'com8a.com'
 
c
c...Added VX checks ifl(322)
c
c
c...Check for 501 vt mode
c...Sharon - 26aug91
c
      if (ifl(35).eq. 2 .and. ifl(322) .ne. 1) then
          call clrscr
          call wflush
      endif
      return
      end
 
c *****************************************************************************
      subroutine erslin
c *****************************************************************************
 
      include 'com8a.com'
c
c...Check for 501 vt mode
c...Sharon - 26aug91
c
      if (ifl(35).eq. 2 .and. ifl(322) .ne. 1) then
          call clreol
          call wflush
      endif
      return
      end
 
c *****************************************************************************
      subroutine crsplt (lxc,lyc)
c *****************************************************************************
 
      include 'com8a.com'
      integer*4 lxc1, lyc1
c
c...Check for 501 vt mode
c...Sharon - 26aug91
c
c
c...Check for 501 vt mode
c...Paul -10/28/91  
c...Old version was:
c   if (ifl(35).eq. 2) then
c
      if (ifl(35).eq. 2 .and. ifl(322) .ne. 1) then
         if (lyc .lt.5 .or.  ifl(131) .eq. 0) then
          lyc1 = lyc
          lxc1 = lxc
          call plot (lyc1,lxc1)
          call wflush
         endif
      endif
      return
      end
 
c *****************************************************************************
      subroutine ersw3 (lyc,lxc)
c *****************************************************************************

      include 'com8a.com'
      integer*2 lxc,   lyc
      integer*4 passx, passy
c
c...Check for 501 vt mode
c...Sharon - 26aug91
c
      if (ifl(35).eq. 2) then
        if (ifl(322) .eq. 1) then
            call vx_ersw3()
        else if (lyc .lt.5 .or.  ifl(131) .eq. 0) then
          passx = lxc
          passy = lyc
          call plot (passy,passx)
          call clreos
          call wflush
        endif
      endif
      return

      end
