C*********************************************************************
C*    NAME         :  putmsg_w2.f
C*       CONTAINS:
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putmsg_w2.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:32
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putmsg_w2 (cmsg,numchr,lineno,ieras)
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
      subroutine putmsg_w2 (cmsg,numchr,lineno,ieras)

      include 'com8a.com'

      character*1 cmsg(80)
      character*80 cmsg1
      
      integer*2 numchr,lineno,ieras
      integer*4 numchr4,lineno4,colmno4, strlen1
     
      if (SIMCOM) return

      do 222 i=1,80
222   cmsg1(i:i) = cmsg(i)

      numchr4 = strlen1(cmsg1)
      lineno4 = lineno
      colmno4 = ieras

          if (ifl(131) .eq. 1 .and. lineno4 .gt. 4) goto 10000

          if (lineno4 .eq. 0) lineno4 = 1
          if (colmno4 .eq. 0) colmno4 = 1
          call plot_w2(lineno4,colmno4)
          call dmpbuf_w2(cmsg1,numchr4)
c         call wflush

10000 return
      end
