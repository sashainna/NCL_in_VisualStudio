C*********************************************************************
C*    NAME         :  putmsg.for
C*       CONTAINS:
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putmsg.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:32
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putmsg (cmsg,numchr,lineno,ieras)
c*       writes  line to the screen or print file.
C*    PARAMETERS   
C*       INPUT  : 
c*          cmsg    =  the array containing the data to be output          
c*          numchr  =  the number of characters to be printed              
c*          lineno  =  not used
c*          ieras   =  not used
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine putmsg (cmsg,numchr,lineno,ieras)

      include 'com8a.com'

      character*1 cmsg(*)
      character*(80) cmsg1
      
      integer*2 numchr,lineno,ieras
      integer*4 numchr4,lineno4,colmno4, strlen1
     
      if (SIMCOM) return

      do 222 i=1,80
222   cmsg1(i:i) = cmsg(i)

      numchr4 = strlen1(cmsg1)
      lineno4 = lineno
      colmno4 = ieras

      if (ifl(35).eq.1) then
        call putprt(cmsg,numchr,1)
c
c...VX
c
      else if (ifl(35) .eq. 2 .and. ifl(322) .eq. 1) then
           call vx_putmsg (cmsg,numchr,lineno,ieras)
c
c...Added check for NCL-VT mode and for NCL501+ mode
c...Paul  -  10/3/91                    02/19/92
c
      else if (ifl(35).eq.2 .and. ifl(350) .le. 1) then 
c
c...Added check for NCL-VT mode
c...Paul  -  10/28/91
c
          if (ifl(131) .eq. 1 .and. lineno4 .gt. 4) goto 10000

          if (lineno4 .eq. 0) lineno4 = 1
          if (colmno4 .eq. 0) colmno4 = 1
          call plot(lineno4,colmno4)
          call dmpbuf(cmsg1,numchr4)
          call wflush
      else
c        call ptmsgu(cmsg,numchr,lineno,ieras)
        call ptmsgu(cmsg,numchr)
      endif
 
10000 return
      end
