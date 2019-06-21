c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ptspnt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:31
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptspnt(srfkey,ipan,dbuf)
C*       put geometry into unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          srfkey - surface key
C*          dbuf  - real*8 array
C*          ipan  - panel number
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ptspnt(srfkey,ipan,dbuf)

      include 'com8a.com'
      include 'wrksys.com'

      integer*4 srfkey
      integer*2 ipan
      real*8 dbuf(2)

      real*8 dbuf1
      integer*2 idbuf1(4),nwds
      equivalence (dbuf1,idbuf1)

      real*8 umx(12)
      data umx /0.0d0, 0.0d0, 0.0d0, 0.0d0,
     y          0.0d0, 0.0d0, 0.0d0, 0.0d0,
     z          0.0d0, 0.0d0, 0.0d0, 0.0d0/

      dbuf1=dbuf(1)
      nwds=8
      if (idbuf1(1).eq.0) nwds=14
      nwds=2+(idbuf1(2)+1)/2+(idbuf1(2)+1)*nwds

      if (ifl(72).eq.1) call transf(dbuf,sc(56),nwds,9)

      if (ifl(264).eq.1) then 
        umx(1)  = 1.0d0/25.4d0
        umx(6)  = 1.0d0/25.4d0
        umx(11) = 1.0d0/25.4d0
        call transf(dbuf,umx,nwds,9)
      endif
c                     work system, transform entity with work mx
      if (lwrk) call transf(dbuf,wrkmx,nwds,9)

c         check if entity should be displayed and make attribute blanked if
c         it is not to be displayed
c     added for reset/disply,sf. kathy
c
      if (ldspsf) then
      else
          call blkgeo (srfkey, 1)
      endif

      call ptspan (srfkey,ipan,dbuf)

      return
      end
