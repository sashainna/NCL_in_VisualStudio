C*********************************************************************
C*    NAME         :  ipvfor.f
C*       CONTAINS:
C*          mcswcs  wcsmcs  
C*    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       ipvfor.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:12
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mcswcs
C*      Transform modeling coordinate system to world coordinate sys.
C*    PARAMETERS   
C*       INPUT  : 
C*          iopt      - 0 = point, 1 = vector.
C*          buf       - Coordinates to convert.
C*       OUTPUT :  
C*          buf       - Converted coordinates.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine mcswcs (iopt, buf)

      include 'com.com'
      include 'wrksys.com'

      integer*2 iopt
      real*8 buf(3)


      integer*2 ietype

      if (lwrk) then
        ietype = 3
        if (iopt.eq.1) ietype = 4
        call conent (buf, wrkmx, ietype)
      endif

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine wcsmcs
C*      Transform world coordinate system to modeling coordinate sys.
C*    PARAMETERS   
C*       INPUT  : 
C*          iopt      - 0 = point, 1 = vector.
C*          buf       - Coordinates to convert.
C*       OUTPUT :  
C*          buf       - Converted coordinates.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine wcsmcs (iopt, buf)

      include 'com.com'
      include 'wrksys.com'

      integer*2 iopt
      real*8 buf(3)

      integer*2 ietype

      if (lwrk) then
        ietype = 3
        if (iopt.eq.1) ietype = 4
        call conent (buf, invwrk, ietype)
      endif

999   return
      end
