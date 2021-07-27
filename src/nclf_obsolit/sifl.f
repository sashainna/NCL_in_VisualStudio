C*********************************************************************
C*    NAME         :  sifl.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sifl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:43
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sifl
c*       displays contents of ifl common        
c*                                                                   
c*       last revision: to add capability to show 100 ifl entries        
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine sifl
      include 'com8a.com'
      call parsit
      if (ityp.eq.3) then
          j=itv
      else
          j=1
      endif
      k=14
      do 10 i=j,j+9
          if (i.gt.numifl) then
              cout=' '
          else
              write(cout,5)i,ifl(i)
5             format('ifl(',i3,')= ',i6)
          endif
          k=k+1
10        call putmsg(cout,80,k,0)
      return
      end
