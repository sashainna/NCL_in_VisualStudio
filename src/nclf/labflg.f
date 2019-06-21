
C*********************************************************************
C*    NAME         :  labflg.f
C*       CONTAINS:
C*                 savlab
C*                 reslab
C*                 setlbf
C*                 setlbt
C*                 savldr
C*                 resldr
C*                 setldf
C*                 setldt
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       labflg.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:13
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine savlab()
C*      Save current label flags
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine savlab
      include 'com8a.com'

      svlbpt=lablpt
      svlbve=lablve
      svlbln=lablln
      svlbpl=lablpl
      svlbci=lablci
      svlbcv=lablcv
      svlbsf=lablsf
      svlbsh=lablsh
      svlbpn=lablpn
      svlbpv=lablpv
      svlbmx=lablmx
      svlban=lablan
      svlbsy=lablsy
      svlbso=lablso

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine reslab()
C*      Restore current label flags
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine reslab
      include 'com8a.com'

      lablpt=svlbpt
      lablve=svlbve
      lablln=svlbln
      lablpl=svlbpl
      lablci=svlbci
      lablcv=svlbcv
      lablsf=svlbsf
      lablsh=svlbsh
      lablpn=svlbpn
      lablpv=svlbpv
      lablmx=svlbmx
      lablan=svlban
      lablsy=svlbsy
      lablso=svlbso

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setlbt()
C*      Set all current label flags to TRUE
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine setlbt
      include 'com8a.com'

      lablpt=.true.
      lablve=.true.
      lablln=.true.
      lablpl=.true.
      lablci=.true.
      lablcv=.true.
      lablsf=.true.
      lablsh=.true.
      lablpv=.true.
      lablmx=.true.
      lablan=.true.
      lablsy=.true.
      lablso=.true.

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setlbf()
C*      Set all current label flags to FALSE
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine setlbf
      include 'com8a.com'

      lablpt=.false.
      lablve=.false.
      lablln=.false.
      lablpl=.false.
      lablci=.false.
      lablcv=.false.
      lablsf=.false.
      lablsh=.false.
      lablpv=.false.
      lablmx=.false.
      lablan=.false.
      lablsy=.false.
      lablso=.false.

      return
      end

C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine savldr()
C*      Save current leader line  flags
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine savldr
      include 'com8a.com'

      svldpt=lbldpt
      svldve=lbldve
      svldln=lbldln
      svldpl=lbldpl
      svldci=lbldci
      svldcv=lbldcv
      svldsf=lbldsf
      svldsh=lbldsh
      svldpn=lbldpn
      svldpv=lbldpv
      svldmx=lbldmx
      svldan=lbldan
      svldsy=lbldsy

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine resldr()
C*      Restore current leader line flags
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine resldr
      include 'com8a.com'

      lbldpt=svldpt
      lbldve=svldve
      lbldln=svldln
      lbldpl=svldpl
      lbldci=svldci
      lbldcv=svldcv
      lbldsf=svldsf
      lbldsh=svldsh
      lbldpn=svldpn
      lbldpv=svldpv
      lbldmx=svldmx
      lbldan=svldan
      lbldsy=svldsy

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setldt()
C*      Set all current leader line flags to TRUE
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine setldt
      include 'com8a.com'

      lbldpt=.true.
      lbldve=.true.
      lbldln=.true.
      lbldpl=.true.
      lbldci=.true.
      lbldcv=.true.
      lbldsf=.true.
      lbldsh=.true.
      lbldpv=.true.
      lbldmx=.true.
      lbldan=.true.
      lbldsy=.true.

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setldf()
C*      Set all current leader line flags to FALSE
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine setldf
      include 'com8a.com'

      lbldpt=.false.
      lbldve=.false.
      lbldln=.false.
      lbldpl=.false.
      lbldci=.false.
      lbldcv=.false.
      lbldsf=.false.
      lbldsh=.false.
      lbldpv=.false.
      lbldmx=.false.
      lbldan=.false.
      lbldsy=.false.

      return
      end
