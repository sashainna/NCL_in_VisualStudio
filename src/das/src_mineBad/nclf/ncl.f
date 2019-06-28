C*********************************************************************
C*    NAME         :  ncl.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ncl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:19
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclsys
c*          to call driver subroutine
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          ierr     -   0 if no error, 2 if err from *run icon, else 1
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine nclsys(ierr)

      include 'com8a.com'

      common/wblok/w(600)
      common/pblok/p(400)
      common/blok/b(75)
      common/dddcom/d(130)
      common/vblok/v(120)
      common/fitcom/f(400)
      common/pokom/q(150)

      integer*2 ierr, dummy
c      character*1 bparm

c      external rsttrm
c      call nclsig

c         set up files & error control
c              get batch flag
c      bparm = '0'

      ifl (275)=0

c
c...Added check for NCL-VT mode (for *START command)
c...Paul  -  10/25/91
c...Old version was:
c   call driver
c

10    call driver(dummy)

      ierr = ifl(275)

      return
      end
