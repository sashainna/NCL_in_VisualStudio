
C*********************************************************************
C*    NAME         :  ptpsrc.for
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ptpsrc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:31
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptpsrc (cline, cnchar, cstr)
c*       This routine gets the current line from the source file for
C*        the "C" edit routines.
C*    PARAMETERS   
C*       INPUT  : 
C*          cline       line number to put (0 means use nline)
C*          cnchar      number of characters in the string (not used)
C*          cstr        current string
C*       OUTPUT :  
C*          cline       line number that was actually written
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ptpsrc (cline, cnchar, cstr)

      include 'com8a.com'

      integer*2 cnchar
      integer*4 cline
      character*(*) cstr
c
      integer*2 ktyp
      integer*4 i,nc

      if (cline.eq.0) cline = nline
      i = cline - 1
      ktyp = 0
cc      if (lincld) ktyp = 1
cc      if (lincfi) ktyp = 3
 
      nc = cnchar
      
      call wrtpp(cstr,nc,i,irctyp,0)

99999 return
      end
