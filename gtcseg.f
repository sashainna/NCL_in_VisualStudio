c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       gtcseg.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:09
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtcseg(nclkey,isgnum,dbuf)
C*       get a curve segment from unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey - unibase key
C*          isgnum - segment number
c*                  data buffer containing canonical data of entity
C*       OUTPUT :  
C*          dbuf   -  curve segment
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtcseg(nclkey,isgnum,dbuf)


      include 'com8a.com'
      include 'wrksys.com'

      integer*4 nclkey
      integer*2 isgnum
      real*8 dbuf(6)

      real*8 umx(12)
      data umx /25.4d0,  0.0d0,  0.0d0, 0.0d0,
     y           0.0d0, 25.4d0,  0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 25.4d0, 0.0d0/

      integer*2 ICVSEG, NWDS
      data ICVSEG /81/
      data NWDS   /6/

      call gtcvsg(nclkey,isgnum,dbuf)

c                  working system
      if (lwrk) call transf (dbuf,invwrk,NWDS,ICVSEG)
      if (ifl(264).eq.1) call transf (dbuf,umx,NWDS,ICVSEG)

      return
      end
