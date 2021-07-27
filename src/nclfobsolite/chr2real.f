C*********************************************************************
C*    NAME         :  chr2real.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       chr2real.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:40
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine chr2real (cins,lngth,out)
C*       convert a character string to a real value.
C*    PARAMETERS   
C*       INPUT  : 
C*          cins -  input character string
C*          lngth - length of input character string
C*          out -   r*8 variable where real value is
C*                  returned.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
       subroutine chr2real (cins,lngth,out)

      include 'com4a.com'

       character*1 cins(20)
       integer*2 scale
       real*8 out,temp

       jst=1
       if (cins(1).eq.'+'.or.cins(1).eq.'-') jst=2
       out=0.
       scale=0
       do 10 i=jst,lngth
           if(cins(i).eq.'.') then
               scale=lngth-i
           else
               jwd=0
               jwd=ichar(cins(i))
               jwd=jwd-48
               temp=jwd
               out=out*10.d0 +temp
           endif
10     continue
       out=out/10.d0**scale
       if(cins(1).eq.'-')out=-out
       return
       end
