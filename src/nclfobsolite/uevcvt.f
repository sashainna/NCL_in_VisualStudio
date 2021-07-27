c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       uevcvt.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:50
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1993 NCCS
C**
C*********************************************************************
C*    E_SUBROUTINE     : subroutine uevcvv
C*      Calculate point & slope on a wf curve & transform for wrksys,
C*      metric & refsys as appropiate.
C*    PARAMETERS   
C*       INPUT  : 
C*          u         - U parameter of point on curve.
C*          isrf      - Index into work tables.
C*          irs       - =1, apply refsys, =0 do not.
C*       OUTPUT :  
C*          pt        - Point on curve at parameter u.
C*          ve        - Slope of curve at parameter u.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine uevcvv (u, isrf, irs, pt, ve)

      include 'com.com'
      include 'wrksys.com'

      real*8 u, pt(3), ve(3)
      integer*2 isrf, irs

      integer*2 i2v3, i2v4
      data i2v3 /3/, i2v4 /4/

      call uevcrv(u,isrf,pt,ve)

      if (lwrk) then
        call conent(pt,invwrk,i2v3)
        call conent(ve,invwrk,i2v4)
      endif

      if (ifl(264).eq.1) then
        pt(1) = pt(1)*25.4d0
        pt(2) = pt(2)*25.4d0
        pt(3) = pt(3)*25.4d0
        ve(1) = ve(1)*25.4d0
        ve(2) = ve(2)*25.4d0
        ve(3) = ve(3)*25.4d0
      endif
 
      if (irs .eq. 1) then 
        call conent(pt,sc(68),i2v3)
        call conent(ve,sc(68),i2v4)
      endif

999   return
      end
