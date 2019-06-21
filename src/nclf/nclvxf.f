C*********************************************************************
C*    NAME         :  nclvxf.f
C*       CONTAINS:
C*         vxnam
C*         stdfwf
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       nclvxf.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:20
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vxnam (nam, isub)
C*       Set the current entity name & subscript.
C*    PARAMETERS
C*       INPUT  :
C*          nam       - Entity name
C*          isub      - Entity subscript.
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine vxnam (nam, isub)
 
      include 'com.com'
 
      character*64 nam
      integer*4 isub
 

      savid2 = nam
      isvsub = isub

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine stdfwf (iflg)
C*       Set the defwf (defining wireframe geometry) flag.
C*    PARAMETERS
C*       INPUT  :
C*          iflg      - =0 ,set defwf false.
C*                    - =1 ,set defwf true.
C*       OUTPUT :
C*          iflg      - 0 if iflg=1 and not defwf else unchanged.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine stdfwf (iflg)
 
      include 'com.com'
 
      integer*2 iflg
 
      if (iflg.eq.1) then
        if (.not.defwf) iflg = 0
        defwf = .true.
      else
        defwf = .false.
      endif

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine runvx
C*     Load and run NCL in conjunction with Varimetrix.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine runvx()
 
      include 'com4a.com'
 
      integer*2  jquit
c
c... initialization of NCL variables
c
   10 call nclini
      call clinit
      ifl(35)=2
      ifl(268) = 1
      ifl(322) = 1
c
c...Call NCL
c
      call driver(jquit)
c
c...Return Apt source file only to VX
c
      ifl(69) = 0
      ifl(88) = 1
      call nclfin
      if (jquit .eq. 2) goto 10
c
c...End of routine
c
 8000 return
      end
