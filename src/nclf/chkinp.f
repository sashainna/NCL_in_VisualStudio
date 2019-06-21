      
C*********************************************************************
C*    NAME         :  chkinp.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       chkinp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:39
C********************************************************************/
C
      subroutine chkinp (name,l)
      character*(*) name
      integer*2 i, l
      integer*4 strlen1,len
     
c
c...use this part when correct name "PP" file (add ".pp" if necessary)
c
      len = strlen1(name)

      if (l .eq. 1) then  
         if (name(len:len) .eq. '.') then
            name((len+1):) = 'pp'
            goto 100
         endif

         do 200 i = len,1,-1
            if(name(i:i) .eq. '.') goto 100
            if(name(i:i) .eq. '/' .or. name(i:i) .eq. ']') then
               name((len+1):) = '.pp' 
               goto 100
            endif
200      continue
         name((len+1):) = '.pp'  	
         goto 100
      endif
c
c...use this part when create name "CL" file from name "PP" file
c
      if (l .eq. 2) then
       do 300 i=len,1,-1
        if (name(i:i).eq.'.' )  then
          name((i+1):) = 'cl'
          goto 100
        endif
300   continue
      endif

100   return
      end

