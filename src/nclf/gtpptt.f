c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       gtpptt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:10
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtpptt(pankey,ipatnm,dbuff,nwds)
C*       get geometry from unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          pankey - panel key
C*          ipatnm - patch number
C*          dbuf   - real*8 array
C*          nwds   - number of words  (8 rldsrf, 14 full surf)
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtpptt(pankey,ipatnm,dbuf,nwds)


      include 'com8a.com'
      include 'wrksys.com'

      integer*4 pankey
      integer*2 nwds
      real*8 dbuf

      real*8 umx(12)
      data umx /25.4d0,  0.0d0,  0.0d0, 0.0d0,
     y           0.0d0, 25.4d0,  0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 25.4d0, 0.0d0/
      integer*2 ISFPAN
      data ISFPAN /91/

      call gtppat (pankey,ipatnm,dbuf)

      if (lwrk) call transf(dbuf,invwrk,nwds,ISFPAN)
      if (ifl(264).eq.1) call transf(dbuf,umx,nwds,ISFPAN)

      return
      end
