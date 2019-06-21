C*********************************************************************
C*    NAME         :  dlen.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dlen.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:56
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : function dlen (str, slen) 
C*     general purpose routine which returns the dynamic length of a
C*     character string by stripping trailing blanks               
C*     where:                                                     
C*        str = character string to be tested                    
C*        slen = length of string to be tested                   
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
      function dlen (str, slen)  

      include 'com4a.com'

      character*(*) str
      integer*2 dlen, slen, i
   
      do 100 i = slen, 1, -1
          if (str(i:i) .ne. ' ') goto 200
100   continue

200   dlen = i

      end
