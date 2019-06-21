C*********************************************************************
C*    NAME         :  dispmx.f
C*       CONTAINS:
C*     Displays a Matrix. 
C* 
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*    MDULE NAME AND RELEASE LEVEL 
C*        dispmx.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:56
C********************************************************************/
C*
C***********************************************************************
C*   E_SUBROUTINE     : subroutine dispmx
C*     Display MX when display command is used to change MX display
C*     parameters.
C*    PARAMETERS   
C*       INPUT  : 
C*          mxasw  : Associated word of Matrix to display
C*          box    : Four R*4 parameters of MX
C*          mxtok  : Matrix label
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C***********************************************************************
 
      subroutine dispmx (mxasw, box, mxtok)
c
      include 'com4a.com'
c
      real*8 mxasw
      real*4 box(4) 
      character*64 mxtok
c
      integer*4 nclkey 
      integer*2 ietype,nwds
      real*8 axis(4),u

      token2 = mxtok

      call vstchk
      call gtdesc (mxasw, nclkey, nwds, ietype)
c
c...Update the MX data with the new axis length & box size values
c...supplied in the DISPLY command, if none, redisply only.  
c
      u    = 1.d0
      if (ifl(264) .eq. 1) u = 25.4d0
      if (ietype .eq. matrix) then
         if (box(1) .gt. 0. .and. box(2) .gt. 0.) then
            axis(1) = box(1) / u
            axis(2) = box(2) / u
            axis(3) = box(3) / u
            axis(4) = box(4) / u
            call upmatr (nclkey, axis)
         end if
      endif
c
c...Update the blank attribute in case the Matrix has
c...previously been erased.
c
      call blkgeo (nclkey, 0)
c
c...This call forces the Matrix to be redrawn using the new
c...display values (if they were changed)
c
      call dspent (nclkey, ietype)
c
999   return
      end
