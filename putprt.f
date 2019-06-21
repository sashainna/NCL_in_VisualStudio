C*********************************************************************
C*    NAME         :  putprt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putprt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:32
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putprt (cmsg,numchr,ifrc)
c*       writes  line to the print file.             
C*    PARAMETERS   
C*       INPUT  : 
c*          cmsg    =  the array containing the data to be output          
c*          numchr  =  the number of characters to be printed              
c*          ifrc    =  1 = Write to print file even if this is an
c*                     line from an include file.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine putprt (cmsg,numchr,ifrc)

      include 'com8a.com'

      integer*2 ifrc

      character*(*) cmsg
      character*1 ff
      character*(MAX_LEN) temp
 
      if (SIMCOM) return
c
c     for differnet carriage control. kathy
c
c	  data ff /char(12)/

      write (temp,10) cmsg(1:numchr)
   10 format (a)
      if ((ifl(149) .eq. 1 .or. ifl(149) .eq. -2) .and.
     1    ifl(218) .eq. 0 .and.
     2    (incflg .or. irctyp .ne. 1 .or. ifrc .eq. 1)) then
          if ((prline+1).lt.ifl(151)) then
              prline=prline+1
          else
              ff = char(12)
              write (prtlun,1020) ff
1020    	  format (a)
              prline=0
          endif
          write (prtlun,10) cmsg(1:numchr)
      endif

      return
      end
