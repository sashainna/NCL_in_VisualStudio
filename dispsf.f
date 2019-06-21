C*********************************************************************
C*    NAME         :  dispsf.f
C*       CONTAINS:
C*     Displays a SURFace
C* 
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       dispsf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:56
C*********************************************************************
C*
C***********************************************************************
C*   E_SUBROUTINE     : subroutine dispsf
C*     Display SURFace
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          srfasw : Associated word of SURFace to display
C*          uval   : Number of U points to draw
C*          vval   : Number of V points to draw
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C***********************************************************************
 
      subroutine dispsf (srfasw, kuvs, sftok)
c
      include 'com4a.com'
c
      real*8 srfasw
      integer*4 kuvs(4) 
      character*64 sftok
c

      integer*4 nclkey
      integer*2 ietype,nwds,isftyp
      logical trflg

      trflg = .true.
      token2 = sftok

      call gtdesc (srfasw, nclkey, nwds, ietype)
      call sftype (nclkey, isftyp)
               
c            If the kind of SURFace is not a standard CAM ruled, sculptured
c            or MESH SURFace just make the SURFace visible.
c      25 is a quilt surface
c
      if (isftyp .eq. 25) then 
          call blankg(2, nclkey)
          if (kuvs(2) .ne. 0 .or. kuvs(4) .ne. 0) call error(-371)
          goto 999
      endif

c
c...Update the Surface header with the new U and V values
c...supplied in the DISPLY command, if none redisply only.  
c
      if (isftyp .ne. 25 .and. kuvs(2) .gt. 0 .and. 
     -                         kuvs(4) .gt. 0) then
          call upsurf (nclkey, kuvs)
      endif
c
c...Update the blank attribute in case the surface has
c...previously been erased.
c
      call blkgeo (nclkey, 0)
c
c...This call forces the SURF to be redrawn using the new
c...U & V values (if they were changed)
c
      call ncl_displst_delete (nclkey)
      call dspent (nclkey, ietype)
      call upsrst
 
999   return
      end
