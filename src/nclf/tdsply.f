C*********************************************************************
C*    NAME         :  tdsply.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       tdsply.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:47
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine tdsply(lnk,td,jptk)
C*       Print motion point
C*    PARAMETERS   
C*       INPUT  : 
C*          lnk      line count 
C*          td       data to print
C*          jptk     point count
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine tdsply(lnk,td,jptk)

      include 'com4a.com'
      include 'mocom.com'

      integer*2 lnk, jptk
      real*8 td(9)

      real*8 buf(6)

      if (.not.motdfl) go to 999
      if (lnk.ne.1) go to 32
      if (ifl(82).eq.1) then
        write(cout,26)
      else
        write(cout,261)
      endif
261   format(12x'x'11x'y'11x'z')
26    format(12x'x'11x'y'11x'z'11x'i'11x'j'11x'k')
      call putmsg(cout,80,15,0)
      iptcnt=0
      if (.not.motdfl.or.ifl(35).eq.1) go to 31
      call ersw3 (16,1)
31    lnk=15
32    lnk=lnk+1
      if(lnk.lt.24.or.ifl(35).eq.1)goto 870
c          blank-out work area
      call ersw3 (16,1)
      lnk=16
870   continue
      l=3
      if(ifl(82).eq.1) l=6
      jptk=jptk+1
      do 30 j=1,l
30    buf(j)=td(j)
      if (ifl(73).eq.0.or.ifl(267).eq.0) goto 40
      call conent(buf,sc(41),3)
      if (ifl(82).eq.1) call conent(buf(4),sc(41),4)
40    if (ifl(35).eq.1) go to 8701
      write(cout,19)jptk,(buf(j),j=1,l)
19    format(i4,3f12.4,3f12.6)
      go to 8702
8701  write(cout,191)(buf(j),j=1,l)
191   format(4x3f12.4,3f12.6)
8702  call putmsg(cout,80,lnk,0)
999   return
      end
