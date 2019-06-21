c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       gtmptt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:10
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtmptt(srfkey,ipat,dbuf)
C*       get mesh patch from unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          srfkey - surface key
C*          dbuf   - real*8 array
C*          ipat   - patch number
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtmptt(srfkey,ipat,dbuf)

      include 'com8a.com'
      include 'wrksys.com'

      integer*4 srfkey
      integer*2 ipat
      real*8 dbuf(26)

      real*8 umx(12)
      data umx /25.4d0,  0.0d0,  0.0d0, 0.0d0,
     y           0.0d0, 25.4d0,  0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 25.4d0, 0.0d0/

      call gtmpat (srfkey,ipat,dbuf)

      if (lwrk) call trnmsh(dbuf,invwrk)
      if (ifl(264).eq.1) call trnmsh(dbuf,umx)

      return
      end
