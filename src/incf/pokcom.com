c*********************************************************************
c**
c**    NAME         :  pokcom.com
c**
c**    CONTAINS:  
c**      Common blocks for pocketing.
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       pokcom.com , 25.2
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 17:23:15
c*********************************************************************

      common/rrrl4/flarc,tol,stpmin,stpmax,stpcur,stplst,cutin,bthk,
     x       rcorn,xlook,ylook,ulook,xlace,ylace,vmin,vmax,stpcov
      real*4 flarc,tol,stpmin,stpmax,stpcur,stplst,cutin,bthk,
     x       rcorn,xlook,ylook,ulook,xlace,ylace,vmin,vmax,stpcov

      common/rrint4/islnds,locapo,larcs,lifts,look,ltrarc,lanes,
     x              lacefl,bddir
      integer*4 islnds,locapo,larcs,lifts,look,ltrarc,lanes,
     x          lacefl,bddir

      common/rrlog/lwatfl,lwr,lboth,lctflg,lnosteps
      logical lwatfl,lwr,lboth,lctflg,lnosteps

      integer*2 mxln
      parameter (mxln=900) 
      common/rrln/lane(mxln),loop(mxln),loca(mxln),mama(mxln),
     x            narcpt(mxln)
      integer*4 lane, loop, loca, mama, narcpt

      common/rrint2/ishuff(mxln)
      integer*2 ishuff

      integer*2 maxpts
      parameter (maxpts=3000)

      integer*2 maxing
      parameter (maxing=50)

      integer*2 mxingos
      parameter (mxingos=500)
