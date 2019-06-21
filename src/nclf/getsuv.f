C*********************************************************************
C*    NAME         :  getsuv.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       getsuv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:08
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getsuv
C*       Return start u & v value for a curve or surface
C*    PARAMETERS   
C*       INPUT  : 
C*          asw    - Pointer to surface
c*          isf    - index to surface tables
C*       OUTPUT :  
C*          u      - start u value
c*          v      - start v value
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getsuv(asw,isf,u,v)

      include 'com8a.com'
      include 'suvcom.com'

      real*8 asw
      real*4 u,v
      integer*2 isf

      integer*4 nclkey
      integer*2 i,nwds,ietype

      if (isf.eq.1 .and. psuv .or.
     x    isf.eq.2 .and. dsuv .or.
     x    isf.eq.3 .and. csuv) then
        u = uvsv(isf*2-1)
        v = uvsv(isf*2)
      else
        u=.5
        v=.5
        if (autouv) then
          call gtdesc(asw,nclkey,nwds,ietype)
          do 10 i=1,3
            if (kuv(i) .eq. nclkey) then
              u = hu(i)
              v = hv(i)
              goto 99999
            endif
10        continue
        endif
      endif
 
99999 return
      end
