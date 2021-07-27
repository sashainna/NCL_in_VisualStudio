c**
C*********************************************************************
C*    NAME         :  chr2int.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       chr2int.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:40
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine chr2int (cins,lngth,iout)
C*     convert a character string to an integer value.
C*                                                               
C*   calling parameters: cinss - input character string         
C*                       lngth - length of input character string
C*                       iout - i*4 variable where integer value is  
C*                               returned.                          
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
       subroutine chr2int (cins,lngth,iout)

      include 'com4a.com'

       character cins(10)
      integer*2 lngth
       integer*4 iout,itemp,i
       logical neg,pos

       pos=.false.
       neg=.false.
       if (cins(1).eq.'+'.or.cins(1).eq.'-') then
           if (cins(1).eq.'-') then
               neg=.true.
           else
               pos=.true.
           endif
           cins(1)='0'
       endif
       iout=0
       do 10 i=0,lngth-1
       itemp=ichar(cins(lngth-i))-48
10     iout=iout+itemp*(10**i)
       if (neg) then
           iout=-iout
           cins(1)='-'
       else if (pos) then
           cins(1)='+'
       endif
       return
       end
