
C*********************************************************************
C*    NAME         :  gtpsrc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       gtpsrc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:10
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtpsrc (cline, cnchar, cstr)
c*       This routine gets a line from the source file for
C*        the "C" edit routines.
C*    PARAMETERS   
C*       INPUT  : 
C*          cline       line number to get (0 means use nline)
C*       OUTPUT :  
C*          cline       line number read
C*          cnchar      number of characters in the string
C*          cstr        current string
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine gtpsrc (cline, cnchar, cstr)

      include 'com8a.com'

      integer*2 cnchar
      integer*4 cline
      character*(MAX_LEN) cstr
      integer*4 i,knc

      cnchar = 0
      if (cline.eq.0) cline = nline
c                                   if insert mode, return null string
      if (ifl(37).eq.2) goto 99999

      i = cline - 1
      call getsrc (cstr, knc,i,0)
      if (srceof) go to 99999
      do 800 i=knc,1,-1
800      if (cstr(i:i).ne.' ') go to 850
850   cnchar=i
      if (ifl(387) .ne. 0 .and. cnchar.ge.ifl(387)) cnchar=ifl(387) - 1
99999 return
      end
