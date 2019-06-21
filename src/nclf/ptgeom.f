c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ptgeom.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:30
c**
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptgeom( ietype, dbuf, nclkey, nwds)
C*       put geometry into unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          dbuf  - real*8 array
C*          nwds  - number of words in entity
c*                  data buffer containing canonical data of entity
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine ptgeom (ietype, dbuf, nclkey, nwds)

      include 'com8a.com'
      include 'wrksys.com'

      real*8 dbuf(16)
      integer*4 nclkey
      integer*2 ietype,nwds

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      real*8 buf(3),umx(12)
      integer*2 nwd, i2v3, i2v4
      data i2v3 /3/
      data i2v4 /4/
      data umx /0.0d0, 0.0d0, 0.0d0, 0.0d0,
     y          0.0d0, 0.0d0, 0.0d0, 0.0d0,
     z          0.0d0, 0.0d0, 0.0d0, 0.0d0/

      nwd = nwds
c                  If plane get 7 words of data but set the number of 
c                  words to 4.
      if (ietype .eq. 6) nwd = 4
      if (lwrk .or. ifl(264).eq.1) then
c          units mm or work system, transform entity.
        umx(1)  = 1.0d0/25.4d0
        umx(6)  = 1.0d0/25.4d0
        umx(11) = 1.0d0/25.4d0
        if (ietype.eq.10) then
          if (ifl(264).eq.1) then
           buf(1) = dbuf(4)
           buf(2) = dbuf(8)
           buf(3) = dbuf(12)
           call conent (buf, umx, i2v4)
c           if (ifl(264).eq.1) call conent (buf, umx, i2v4)
c           if (lwrk) call conent (buf, wrkmx, i2v4)
           dbuf(4) = buf(1)
           dbuf(8) = buf(2)
           dbuf(12) = buf(3)
           dbuf(13) = dbuf(13)/25.4d0
           dbuf(14) = dbuf(14)/25.4d0
           dbuf(15) = dbuf(15)/25.4d0
           dbuf(16) = dbuf(16)/25.4d0
          endif
        else if (ietype.ne.9) then
          if (ifl(264).eq.1) call transf(dbuf,umx,nwd,ietype)
          if (lwrk) call transf(dbuf,wrkmx,nwd,ietype)
        endif
      endif
      nclkey = keyold
      call ptgeo (ietype, dbuf, nclkey)
c
c           If it's a plane and there is a tag point project the
c           point to plane and switch the result with the original
c           point of the plane.
      if (nwds .eq. 7 .and. ietype .eq. 6) then
             if (ifl(264).eq.1) call transf(dbuf(5),umx,i2v3,i2v3)
             if (lwrk) call transf(dbuf(5),wrkmx,i2v3,i2v3)
             buf(1)=dbuf(5)
             buf(2)=dbuf(6)
             buf(3)=dbuf(7)
             call projpt(nclkey,buf)
             dbuf(5)=buf(1)
             dbuf(6)=buf(2)
             dbuf(7)=buf(3)
      endif
          

      return
      end
