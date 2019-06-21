c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ptmhdt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:30
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptmhdt( dbuf, nclkey)
C*       put mesh surf header into unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          dbuf  - real*8 array
C*          nclkey  - unibase key
c*                
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ptmhdt (dbuf, nclkey)

      include 'com8a.com'
      include 'wrksys.com'

      real*8 dbuf(3)
      integer*4 nclkey

      real*8 tdbuf
      real*4 rtdbuf(2)
      integer*2 itdbuf(4)
      equivalence (tdbuf,rtdbuf,itdbuf)

      if (lwrk .or. ifl(264).eq.1) then
c                     work system
          tdbuf=dbuf(2)
          if (itdbuf(1).eq.7) then
             if (ifl(264).eq.1) rtdbuf(2)=rtdbuf(2)/25.4d0
             if (lwrk) rtdbuf(2)=rtdbuf(2)/wrkscl
             dbuf(2)=tdbuf
          endif
      endif

      call ptmhed (dbuf, nclkey)

      return
      end
