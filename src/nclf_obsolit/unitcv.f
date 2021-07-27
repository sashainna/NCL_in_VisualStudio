c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       unitcv.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:50
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine unitcv (dbuf)
C*       convert dbuf from internal units (inches) to external units.
C*    PARAMETERS   
C*       INPUT  : 
C*          dbuf   -  input buffer
C*       OUTPUT :  
C*          none 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine unitcv (dbuf)


      include 'com8a.com'

      real*8 dbuf(3)
      if (ifl(264).eq.1.) then
c                  units is millimeters
          do 10,i=1,3
10        dbuf(i) = dbuf(i) * 25.4d0
      endif

      return
      end
