c*****************************************************
c**    NAME         :  gtgeom.f
c**       CONTAINS:
c**                 gtgeom
c**                 gtgeve
c**                 gtplt
c**                 cicvln
c**                 gtcirc
c**    MODULE NAME AND RELEASE LEVEL
c**       gtgeom.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:10
c*****************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtgeom(asn,dbuf,nclkey,nwds,ietype)
C*       get geometry from unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          asn   - associated word of item to get
C*          dbuf  - real*8 array
c*                  data buffer containing canonical data of entity
C*       OUTPUT :  
C*          nclkey -  unibase key for the item
C*          nwds   -  number of words in the buffer
C*          ietype -  ncl type for the item
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtgeom (asn,dbuf,nclkey,nwds,ietype)


      include 'com8a.com'
      include 'wrksys.com'

      real*8 asn,dbuf(16)
      integer*4 nclkey
      integer*2 nwds,ietype

      real*8 ve(3), umx(12), tdbuf
      integer*2 i2v4, itdbuf(4)
      real*4 rtdbuf(2)
      equivalence (tdbuf,itdbuf,rtdbuf)
      data i2v4 /4/
      data umx /25.4d0,  0.0d0,  0.0d0, 0.0d0,
     y           0.0d0, 25.4d0,  0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 25.4d0, 0.0d0/

      call gtdesc (asn,nclkey,nwds,ietype)

      call gtgeo (nclkey, dbuf)

      if (lwrk .or. ifl(264).eq.1) then
c                  working system
         if (ietype.eq.9) then
c                  its a surface header - scale offset distance
c                  if its an offset surface
             tdbuf=dbuf(2)
             if (itdbuf(1).eq.7) then
                 if (lwrk) rtdbuf(2)=rtdbuf(2)/wrkscl
                 if (ifl(264).eq.1) rtdbuf(2)=rtdbuf(2)*25.4d0
                 dbuf(2)=tdbuf
             endif
         else if (ietype.eq.10) then
c                                      *** matrix
          if (ifl(264).eq.1) then
             ve(1) = dbuf(4)
             ve(2) = dbuf(8)
             ve(3) = dbuf(12)
c            if (lwrk) call conent (ve, invwrk, i2v4)
c            if (ifl(264).eq.1) call conent (ve, umx, i2v4)
             call conent (ve, umx, i2v4)
             dbuf(4) = ve(1)
             dbuf(8) = ve(2)
             dbuf(12) = ve(3)
           endif
         else
             if (lwrk) call transf(dbuf,invwrk,nwds,ietype)
             if (ifl(264).eq.1) call transf(dbuf,umx,nwds,ietype)
         endif
      endif

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtgeve(asn,dbuf,nclkey,nwds,ietype)
C*       get geometry from unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          asn    - associated word of item to get
C*       OUTPUT :  
C*          dbuf   - real*8 array
c*                   data buffer containing vector 
C*          nclkey - unibase key for the item
C*          nwds   - number of words in the buffer
C*          ietype - ncl type (vector 4 or pointvector 21) 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtgeve (asn,dbuf,nclkey,nwds,ietype)
c
      include 'com8a.com'
c
      real*8 asn,dbuf(3)
      integer*4 nclkey
      integer*2 nwds,ietype
c
      integer*2 ix
      real*8 rnum(6)
c
      equivalence (rsn,isn) 
c
      call gtgeom (asn,rnum,nclkey,nwds,ietype)
      ix     = 3
      if (ietype .eq. 4) ix = 0
      dbuf(1) = rnum(ix+1)
      dbuf(2) = rnum(ix+2)
      dbuf(3) = rnum(ix+3)
c
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtplt(asn,dbuf,nclkey,nwds,ietype)
C*       Get plane or surface with plane primitive from unibase
C*    PARAMETERS   
C*       INPUT  : 
C*          asn    - associated word of item to get
C*          trflg  - =1 transform through refsys matrix
C*       OUTPUT :  
C*          dbuf   - data buffer containing plane 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtplt (asn,trflg,dbuf)
c
      include 'com8a.com'
      include 'wrksys.com'
c
      real*8 asn,dbuf(4)
      integer*2 trflg
c
      real*8 tbuf(16)
      integer*4 nclkey
      integer*2 nwds,ietype
c
      call gtdesc (asn,nclkey,nwds,ietype)
      if (ietype.eq.PLANE) then
        call gtgeom (asn,dbuf,nclkey,nwds,ietype)
        if (trflg.eq.1) call transf(dbuf,sc(68),nwds,ietype)
      else
        call gtprimt(nclkey,trflg,primtyp,tbuf)
        dbuf(1) = tbuf(1)
        dbuf(2) = tbuf(2)
        dbuf(3) = tbuf(3)
        dbuf(4) = tbuf(4)
        nwds = 4
        ietype = PLANE
      endif
c
      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : integer*2 function cicvln(i)
C*
C*    PURPOSE OF PROGRAM: Determine if an entity is of a curve type
C*                        (that is, a line, a circle or a curve)
C*
C*    PARAMETERS
C*       INPUT  :
C*          i   -  the entity ist-number (as returned by parsit)
C*       OUTPUT :
C*          none
C*    RETURNS      : 1 iff a curve type
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
      integer*2 function cicvln(i)
      include 'com.com'

      integer*2 i

      cicvln = 0
      if (i.eq.CIRCLE .or. i.eq.CURVE .or. i.eq.LINE) cicvln = 1

      return
      end

C********************************************************************
C*    E_SUBROUTINE     : subroutine gtcir (asn, trflg,
C*                                          nclkey, ietype, buf)
C*       Given a descriptor (asn), determine the UNIBASE key
C*       (NCLKEY) and entity type (IETYPE), and retrieve the data
C*       into a buffer (BUF). If the data is to be transformed
C*       (TRFLG is true and a REFSYS is defined), the retrieved
C*       data will be transformed prior to being returned.
C*       This function get the full circle ijk value, differnet 
C*       with gtent, which set ijk zero for full circle.
C*    PARAMETERS   
C*       INPUT  : 
C*          ranfwd            ranfile ID
C*          trflg             logical flag
C*                               TRUE apply REFSYS if defined
C*                               FALSE do no apply matrix
C*       OUTPUT :  
C*          nclkey            UNIBASE key
C*          ietype            entity type
C*          buf               buffer for returned data
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************

      subroutine gtcirc (asn, trflg, nclkey, ietype, buf)

      include 'com4a.com'
      include 'wrksys.com'

      real*8 asn,buf(35),umx(12)
      logical trflg
      integer*4 nclkey
      integer*2 ietype, nwds
      data umx /25.4d0,  0.0d0,  0.0d0, 0.0d0,
     y           0.0d0, 25.4d0,  0.0d0, 0.0d0,
     z           0.0d0,  0.0d0, 25.4d0, 0.0d0/


      call gtdesc (asn,nclkey,nwds,ietype)

      if (ietype.eq.CIRCLE) then
          call gtcir(nclkey,buf)
      endif

c     working system
      if (lwrk .or. ifl(264).eq.1) then    
        if (lwrk) call transf(buf,invwrk,nwds,ietype)
        if (ifl(264).eq.1) call transf(buf,umx,nwds,ietype)
      endif

c     check for refsys
700   if (ifl(72) .eq. 1 .and. trflg) then 
          call transf (buf, sc(68), nwds, ietype)
      endif

      return
      end
