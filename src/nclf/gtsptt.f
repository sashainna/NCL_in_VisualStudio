c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       gtsptt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:11
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtsptt(srfkey,ipan,ipat,dbuf,nwds)
C*       get geometry from unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          srfkey - surface key in unibase
C*          ipan   - panel number
C*          ipat   - patch number
C*          dbuf   - real*8 array
C*          nwds   - number of words  (8 rldsrf, 14 full surf)
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtsptt(srfkey,ipan,ipat,dbuf,nwds)


      include 'com8a.com'
      include 'wrksys.com'

      integer*4 srfkey
      integer*2 ipan,ipat,nwds
      real*8 dbuf(14)

      real*8 umx(12)
      data umx /25.4d0,  0.0d0,  0.0d0, 0.0d0,
     y           0.0d0, 25.4d0,  0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 25.4d0, 0.0d0/

      call gtspat (srfkey,ipan,ipat,dbuf)

      if (lwrk) call transf(dbuf,invwrk,nwds,91)
      if (ifl(264).eq.1) call transf(dbuf,umx,nwds,91)

      return
      end
