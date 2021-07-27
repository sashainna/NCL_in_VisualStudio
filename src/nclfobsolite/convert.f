C*********************************************************************
C*    NAME         :  convert.f
C*       CONTAINS:
c*
c*                 to_unibase
c*                 from_unibase
c*
C*    COPYRIGHT 1998 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       convert.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:43
C*********************************************************************
C
C*********************************************************************
C*    SUBROUTINE     : subroutine to_unibase (pt,pt0,type)
c       Convert point from NCL to Unibase system
C*    PARAMETERS   
C*       INPUT  : 
C*          pt     - point/vector to convert
C*       OUTPUT :  
C*          pt0    - converted point/vector
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
c
      subroutine to_unibase (pt,pt0,type)

      include 'com8a.com'
      include 'const.com'
      include 'wrksys.com'
     
      real*8 pt(3),pt0(3)
      integer*2 type

      call vctovc (pt,pt0)

      if (ifl(72) .eq. 1) call conent (pt0,sc(56),type)
      if (ifl(264).eq.1 .and. type.eq.POINT) 
     *       call vctmsc (pt0,pt0,ONE/INCH)
      if (lwrk) call conent (pt0,wrkmx,type)

      return
      end
C*********************************************************************
C*    SUBROUTINE     : subroutine from_unibase (pt0,pt,type)
c       Convert point/vector from NCL to Unibase system
C*    PARAMETERS   
C*       INPUT  : 
C*          pt0    - point/vector to convert
C*          type   - POINT/VECTOR 
C*       OUTPUT :  
C*          pt     - converted point/vector
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
c
      subroutine from_unibase (pt0,pt,type)

      include 'com8a.com'
      include 'const.com'
      include 'wrksys.com'

      real*8 pt0(3),pt(3)
      integer*2 type

      call vctovc (pt0,pt)

      if (lwrk) call conent (pt,invwrk,type)
      if (ifl(264).eq.1 .and. type.eq.POINT) call vctmsc (pt,pt,INCH)
      if (ifl(72) .eq. 1) call conent (pt,sc(68),type)

      return
      end


C*********************************************************************
C*    SUBROUTINE     : subroutine to_unbs (pt0,pt,type)
c       Convert point/vector from NCL to Unibase system, disregarding
c                                                        REFSYS
C*    PARAMETERS   
C*       INPUT  : 
C*          pt0    - point/vector to convert
C*          type   - POINT/VECTOR 
C*       OUTPUT :  
C*          pt     - converted point/vector
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
      subroutine to_unbs (pt,pt0,type)

      include 'com8a.com'
      include 'wrksys.com'
     
      real*8 pt(3),pt0(3),FCIN
      integer*2 type
   
      FCIN = 0.039370079

      call vctovc (pt,pt0)

      if (ifl(264).eq.1 .and. type.eq.POINT) 
     *       call vctmsc (pt0,pt0,FCIN)
      if (lwrk) call conent (pt0,wrkmx,type)

      return
      end

C*********************************************************************
C*    SUBROUTINE     : subroutine fr_unbs (pt0,pt,type)
c       Convert point/vector from NCL to Unibase system, disregarding
c                                                        REFSYS
C*    PARAMETERS   
C*       INPUT  : 
C*          pt0    - point/vector to convert
C*          type   - POINT/VECTOR 
C*       OUTPUT :  
C*          pt     - converted point/vector
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
c
      subroutine fr_unbs (pt0,pt,type)

      include 'com8a.com'
      include 'wrksys.com'

      real*8 pt0(3),pt(3),FCOUT
      integer*2 type

      FCOUT = 25.4

      call vctovc (pt0,pt)

      if (lwrk) call conent (pt,invwrk,type)
      if (ifl(264).eq.1 .and. type.eq.POINT) call vctmsc (pt,pt,FCOUT)

      return
      end
