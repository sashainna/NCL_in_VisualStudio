c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ptmptt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:30
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptmptt(srfkey,ipat,dbuf)
C*       put mesh patch into unibase
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

      subroutine ptmptt(srfkey,ipat,dbuf)

      include 'com8a.com'
      include 'wrksys.com'

      integer*4 srfkey
      integer*2 ipat
      real*8 dbuf(26)

      real*8 umx(12)
      data umx /0.0d0, 0.0d0, 0.0d0, 0.0d0,
     y          0.0d0, 0.0d0, 0.0d0, 0.0d0,
     z          0.0d0, 0.0d0, 0.0d0, 0.0d0/

      if (ifl(264).eq.1) then 
        umx(1)  = 1.0d0/25.4d0
        umx(6)  = 1.0d0/25.4d0
        umx(11) = 1.0d0/25.4d0
        call trnmsh(dbuf,umx)
      endif
      if (lwrk) call trnmsh(dbuf,wrkmx)

      call ptmpat (srfkey,ipat,dbuf)

      return
      end
