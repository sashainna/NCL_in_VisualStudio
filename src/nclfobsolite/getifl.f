C*********************************************************************
C*    NAME         :  getifl.f
C*       CONTAINS:
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       getifl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:07
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getifl
c*          C callable routine to get an ifl
C*       INPUT  : 
C*          idx     -  index of ifl to return
C*       OUTPUT :  
C*          ival    -  value if ifl(idx)
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine getifl(idx,ival)

      include 'com4a.com'

      integer*2 idx,ival

      ival=ifl(idx)

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getjfl
c*          C callable routine to get an i*4 ifl
C*       INPUT  : 
C*          idx     -  index of ifl to return
C*       OUTPUT :  
C*          ival    -  value if ifl(idx)
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getjfl(idx,ival)

      include 'com4a.com'

      integer*2 idx
      integer*4 ival
c
      ival = ifl4(idx)

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getlfl
c*          C callable routine to get an lfl (logical flag).
C*       INPUT  : 
C*          idx     -  index of lfl to return
C*       OUTPUT :  
C*          ival    -  value if lfl(idx)
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getlfl(idx,ival)

      include 'com4a.com'

      integer*2 idx
      logical*2 ival

      ival=lfl(idx)

      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getsc
c*          C callable routine to get an sc
C*       INPUT  :
C*          idx     -  index of sc to return
C*       OUTPUT :
C*          sval    -  value of sc(idx)
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getsc (idx,sval)

      include 'com4a.com'

      integer*2 idx
      real*8 sval

      sval=sc(idx)

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtmsmm (mdsys, mm)
c*          C callable routine to get and reset the modsys and units flags.
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          mdsys   - Modsys flag
C*          mm      - Units flag
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine gtmsmm (mdsys, mm)

      include 'com4a.com'

      integer*2 mdsys, mm

      mdsys = 0
      if (lwrk) mdsys = 1
      mm = ifl(264)
      lwrk = .false.
      ifl(264) = 0

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine stmsmm (mdsys, mm)
c*          C callable routine to restore the modsys and units flags.
C*       INPUT  : 
C*          mdsys   - Modsys flag
C*          mm      - Units flag
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine stmsmm (mdsys, mm)

      include 'com4a.com'

      integer*2 mdsys, mm

      lwrk = .false.
      if (mdsys.eq.1) lwrk = .true.
      ifl(264) = mm

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtmsfl (mdsys, mm)
c*          C callable routine to get (without resetting) the modsys and 
c*          units flags.
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          mdsys   - Modsys flag
C*          mm      - Units flag
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine gtmsfl (mdsys, mm)

      include 'com.com'

      integer*2 mdsys, mm

      mdsys = 0
      if (lwrk) mdsys = 1
      mm = ifl(264)

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine getist (val)
c*          C callable routine to get 'ist'  value.
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          val   - ist value to be get
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getist (val)

      include 'com4a.com'

      integer*2 val

      val = ist

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine getityp (val)
c*          C callable routine to get 'ityp'  value.
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          val   - ist value to be get
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine getityp (val)

      include 'com4a.com'

      integer*2 val

      val = ityp

      return
      end


C*********************************************************************
C*    E_SUBROUTINE     : subroutine setist (val)
c*          C callable routine to set 'ist'  value.
C*       INPUT  : 
C*          val   - ist value to be set
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setist (val)

      include 'com4a.com'

      integer*2 val

      ist = val

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine setityp (val)
c*          C callable routine to set 'ityp'  value.
C*       INPUT  : 
C*          val   - ist value to be set
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine setityp (val)

      include 'com4a.com'

      integer*2 val

      ityp = val

      return
      end

