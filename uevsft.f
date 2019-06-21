C*********************************************************************
C*    NAME         :  uevsft.f
C*       CONTAINS:
C*    COPYRIGHT 1992 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       uevsft.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:50
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine uevsff
C*      Evaluate surface & transform pt & vectors thru working system
C*      matrix.
C*    PARAMETERS   
C*       INPUT  : 
C*          u         - U parameter.
C*          v         - V parameter.
C*          isf       - Index into surface tables.
C*       OUTPUT :  
C*          sv        - Point & vectors.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine uevsff (u, v, isf, sv)

      include 'com.com'
      include 'wrksys.com'

      real*8 u, v, sv(9)
      integer*2 isf

      integer*2 i
      real*8 uu,vv,delu,delv,a,b

      if (ifl(270) .eq. isf) then
        a = sc(199)
        b = (a - 1.)/2
        uu = a*u - b
        vv = a*v - b
        delu = 0
        delv = 0
        if (uu .gt. 1) then
          delu = uu - 1
          uu = 1
        else if (uu .lt. 0) then
          delu = uu
          uu = 0
        endif
        if (vv .gt. 1) then
          delv = vv - 1
          vv = 1
        else if (vv .lt. 0) then
          delv = vv
          vv = 0
        endif
        call uevsrf(uu,vv,isf,sv)
        do i = 1,3
          sv(i) = sv(i) + sv(i+3)*delu + sv(i+6)*delv
        enddo
      else
        call uevsrf (u, v, isf, sv)
      endif

      if (lwrk) then
        call conent(sv,invwrk,3)
        call conent(sv(4),invwrk,4)
        call conent(sv(7),invwrk,4)
      endif

      if (ifl(264) .eq. 1) then
        do 100 i=1,9
100     sv(i) = sv(i)*25.4d0
      endif

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine evsf_ext_set (isf, fac)
C*      Set extension paramters for surface evaluation
C*    PARAMETERS   
C*       INPUT  : 
C*          isf       - Index into surface tables.
C*          fac       - extension factor
C*       OUTPUT :  none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine evsf_ext_set (isf, fac)

      include 'com.com'

      real*8 fac
      integer*2 isf

      ifl(270) = isf
      sc(199) = fac

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine evsf_ext_set (isf, fac)
C*      Set extension paramters for surface evaluation
C*    PARAMETERS   
C*       INPUT  : 
C*          isf       - Index into surface tables.
C*          fac       - extension factor
C*       OUTPUT :  none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine evsf_ext_rst()

      include 'com.com'

      ifl(270) = 0
      sc(199) = 0.

      return
      end
