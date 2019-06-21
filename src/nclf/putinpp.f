C*********************************************************************
C*    NAME         :  putinpp.f
C*       CONTAINS:  putinw (cbuf,nc,num)
C*                  putinf (num)
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*        putinpp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:32
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : putinw (cbuf,nc,num)
C*      C callable routine to copy buffer into the w2 F77 accessable 
C*      buffer.
C*    PARAMETERS
C*       INPUT  :
C*          cbuf  - input character array
C*          nc    - number of characters in cbuf
C*          num   - line number - 1 in w2 where cbuf is addressed
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/ 
      subroutine putinw (cbuf,nc,num)
c
      include 'com.com'
c
      character*80 cbuf
      integer*4 num,nc
c
      call nclf_putw2 (num+1,cbuf,nc,irctyp)
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : putinf (num)
C*      C callable routine to output w2 buffer to the part program file.
C*    PARAMETERS
C*       INPUT  :
C*          num   - number of lines in w2 array
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/ 
      subroutine putinf (num)
c
      include 'com.com'
c
      integer*4 num
c
      logical lsvfrm

      lsvfrm = fromt
      fromt = .true.
      ifl(123) = num
      svll = nline
      call putsrc
      fromt = lsvfrm
      return
      end
