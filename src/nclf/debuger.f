c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       debuger.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:51
c**
c*****************************************************
       subroutine debuger(string, w)

       character*20   string
       real*8         w(600)

       write(16,10) string
10     format(1x,'debuger: ',a20)
       do 40 i=1,100,6
       write(16,20) i, (w(i+j),j=0,5)
20     format(1x,i4,1x,6f12.5)
40     continue
       return
       end
