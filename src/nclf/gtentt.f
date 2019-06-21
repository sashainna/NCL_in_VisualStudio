C*********************************************************************
C*    NAME         :  gtentt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       gtentt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:10
C********************************************************************
C*    E_SUBROUTINE     : subroutine gtentt (ranfwd, trflg,
C*                                          nclkey, ietype, buf)
C*       Given a descriptor (RANFWD), determine the UNIBASE key
C*       (NCLKEY) and entity type (IETYPE), and retrieve the data
C*       into a buffer (BUF). If the data is to be transformed
C*       (TRFLG is true and a REFSYS is defined), the retrieved
C*       data will be transformed prior to being returned.
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

      subroutine gtentt (ranfwd, trflg, nclkey, ietype, buf)

      include 'com4a.com'

      real*8 buf(35), ranfwd, ranflo
      logical trflg
      integer*4 nclkey
      integer*2 ietype, iranlo(4), nwds
      equivalence (ranflo, iranlo), (iranlo(3), nwds)

      if (debug) then
          write (cout, 9010) ietype
9010      format ('gtentt entry: ietype=',i4)
          call putmsg (cout, 80, 16, 0)
      endif

      ranflo = ranfwd
      call gtgeom (ranfwd, buf(1),nclkey,nwds,ietype)

c         check for refsys
700   if (ifl(72) .eq. 1 .and. trflg) then 
          call transf (buf, sc(68), nwds, ietype)
      endif

      return
      end
