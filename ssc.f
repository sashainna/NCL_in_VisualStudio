C*********************************************************************
C*    NAME         :  ssc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ssc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:44
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ssc
c*       display contents of sc common         
c*                                                                   
c*       last revision: to add capability to show up to 125 sc entries   
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
      subroutine ssc
c
      include 'com8a.com'

       real*8 asn
       integer*2 ksn(4)
       equivalence(asn,ksn)   
       integer*2 i2sc(400)
       equivalence (sc,i2sc)

      call parsit
      if (ityp.eq.3) then
          j=itv
      else
          j=1
      endif
      k=14
      do 100 i=j,j+9
          if (i.gt.numsc) then
              cout=' '
          else
              asn=sc(i) 
              write(cout,1010)i,sc(i),(ksn(l),l=1,4)
1010          format(' sc(',i3,')= ',f14.7,'  ',4i6)
          endif
          k=k+1
100       call putmsg(cout,80,k,0)
c
      return
      end
