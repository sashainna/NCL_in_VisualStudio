c**********************************************************************
c**
c**    NAME         :  dspcom.com 
c**
c**		CONTAINS:
c**          dspcom common - for use in sdplcc and dcrvpt
c**
c**    MODULE NAME AND RELEASE LEVEL 
c**       dspcom.com , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:07:30
c**
c*********************************************************************
       common/dspcom/w(150)

       real*8 w
       real*4 aw(300)
       equivalence (w,aw)
