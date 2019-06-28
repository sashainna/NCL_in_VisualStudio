C*********************************************************************
C*    NAME         :  ptentt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ptentt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:30
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptentt (ietype, buf, nclkey, ranwd)
C*       Given the entity type (IETYPE) and entity data (BUF),
C*       transform the entity data if REFSYS is on, store the 
C*       data in UNIBASE, and return the UNIBASE key of the stored
C*       data
C*    PARAMETERS   
C*       INPUT  : 
C*          ietype            entity type
C*          buf               buffer for returned data
C*       OUTPUT :  
C*          nclkey            UNIBASE key
C*          ranwd             hash table word
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine ptentt (ietype, buf, nclkey, ranwd)

      include 'com4a.com'
c      implicit undefined (a-z)
c      include '../incf/com.com'

      real*8 buf(35), ranwd, ranfwd
      integer*2 ietype, iranlo(4), nwds
      integer*4 nclkey
      equivalence (ranfwd, iranlo), (iranlo(3), nwds)

      ranfwd = ranwd
c          if plane, increase nwds to account for display point 
      if (ietype .eq. 6) nwds = 7
c         transform if in refsys
      if (ifl(72) .eq. 1)  call transf (buf, sc(56), nwds, ietype)

      call ptgeom (ietype, buf(1), nclkey, nwds)
c         check if entity should be displayed and make attribute blanked if
c         it is not to be displayed
      if ((ietype .eq. POINT .and. dsplpt) .or.
     x    (ietype .eq. VECTOR .and. dsplve) .or.
     x    (ietype .eq. LINE .and. dsplln) .or.
     x    (ietype .eq. PLANE .and. dsplpl) .or.
     x    (ietype .eq. PNTVEC .and. ldsppv) .or.
     x    (ietype .eq. CIRCLE .and. dsplci) .or.
     x    (ietype .eq. CURVE .and. dsplcv) .or.
     x    (ietype .eq. SURF .and. ldspsf) .or.
     x    (ietype .eq. SHAPE .and. ldspsh) .or.
     x    (ietype .eq. MATRIX .and. dsplmx) .or.
     x    (ietype .eq. VANOTE .and. dsplan) .or.
     x    (ietype .eq. VSYMBOL .and. dsplsy) .or.
     x    (ietype .eq. VSOLID .and. dsplso) .or.
     x    (ietype .eq. PATERN .and. ldsppn)) then
      else
          call blkgeo (nclkey, 1)
      endif
      call ptdesc (nclkey, ietype, ranwd)

      return
      end
