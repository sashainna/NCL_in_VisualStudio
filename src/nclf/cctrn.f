C*********************************************************************
C*    NAME         :  cctrn.f
C*       CONTAINS:
C*          mcstf  stmdax  stwpax  conpt  conlen
C*          conref  wcsact
C*    COPYRIGHT 1992 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       cctrn.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:39
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mcstf
C*      Return MCS matrix.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          iflg      - = 1 iff modeling coord sys is in effect.
C*          buf       - Model coord sys matrix.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine mcsmx (iflg, buf)

      include 'com.com'
      include 'wrksys.com'

      integer*2 iflg
      real*8 buf(12)

      integer*2 i

      if (lwrk) then
        iflg = 1
        do 10 i=1,12
10      buf(i) = wrkmx(i)
      else
        iflg = 0
        do 20 i=1,12
20      buf(i) = 0.0d0
        buf(1) = 1.0d0
        buf(6) = 1.0d0
        buf(11) = 1.0d0
      endif

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine stmdax
C*      Set modeling coordinate system.
C*    PARAMETERS   
C*       INPUT  : 
C*          iflg      - 0 = reset, 1 = set to matrix
C*          buf       - Modeling matrix.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine stmdax (iflg, buf)

      include 'com.com'
      include 'wrksys.com'

      integer*2 iflg
      real*8 buf(12)

      integer*2 i
      real*8 rsetmx (12)
      data rsetmx /1.0d0, 0.0d0, 0.0d0, 0.0d0,
     y             0.0d0, 1.0d0, 0.0d0, 0.0d0,
     z             0.0d0, 0.0d0, 1.0d0, 0.0d0/

      if (iflg.eq.1) then
c   ------   FIX   ------
c...    Fix for units/mm
c   ------ END FIX ------
        lwrk = .true.
        do 10 i=1,12
10      wrkmx(i) = buf(i)
        call invmx (buf, invwrk)
        wrkscl = dsqrt(buf(1)**2+buf(2)**2+buf(3)**2)
        call stmdmx (buf)
      else
        lwrk = .false.
        call stmdmx (rsetmx)
      endif
c
c... Reset REFSYS
c
      ifl(72) = 0
      rsname = ' '
      call stwpmx (rsetmx)

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine stwpax
C*      Set working coordinate system.
C*    PARAMETERS   
C*       INPUT  : 
C*          iflg      - 0 = reset, 1 = set to matrix
C*          buf       - Working sys matrix.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine stwpax (iflg, buf)

      include 'com.com'
      include 'wrksys.com'

      integer*2 iflg
      real*8 buf(12)

      integer*2 i
      real*8 rsetmx (12)
      data rsetmx /1.0d0, 0.0d0, 0.0d0, 0.0d0,
     y             0.0d0, 1.0d0, 0.0d0, 0.0d0,
     z             0.0d0, 0.0d0, 1.0d0, 0.0d0/

      if (iflg.eq.1) then
c   ------   FIX   ------
c...    Fix for units/mm & mod sys in effect
c   ------ END FIX ------
        ifl(72) = 1
        do 10 i=1,12
10      sc(55+i) = buf(i)
        call invmx (buf, sc(68))
        rsname = '@MX1'
        call stwpmx (buf)
      else
        ifl(72) = 0
        rsname = ' '
        call stwpmx (rsetmx)
      endif

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine conpt(buf)
C*      Tranform a point or vector for modsys, units and refsys.
C*    PARAMETERS   
C*       INPUT  : 
C*          buf       - Entity to transform.
C*          ietype    - Entity type.
C*       OUTPUT :  
C*          buf       - Transformed entity.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine conpt(buf, ietype)

      include 'com.com'
      include 'wrksys.com'

      real*8 buf(3)
      integer*2 ietype

      if (lwrk) call conent(buf,invwrk,ietype)
      if (ifl(264).eq.1) call vctmsc(buf,buf,25.4d0)
      if (ifl(72).eq.1) call conent(buf,sc(68),ietype)

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine conpt(buf)
C*      Transform a length for modsys, units and refsys.
C*    PARAMETERS   
C*       INPUT  : 
C*          dlen      - length to transform.
C*       OUTPUT :  
C*          dlen      - Transformed length.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine conlen(dlen)

      include 'com.com'
      include 'wrksys.com'

      real*8 dlen

      real*8 f_mag

      if (lwrk .and. wrkscl.ne.1.0d0) dlen = dlen*wrkscl
      if (ifl(264).eq.1) dlen = dlen*25.4d0
      if (ifl(72).eq.1) dlen = dlen*f_mag(sc(68))

999   return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine conref(pti,pto)
C*      Transform a point or vector for refsys.
C*    PARAMETERS   
C*       INPUT  : 
C*          buf       - Entity to transform.
C*          ietype    - Entity type.
C*       OUTPUT :  
C*          buf       - Transformed entity.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine conref(buf, ietype)

      include 'com.com'
      include 'wrksys.com'

      real*8 buf(3)
      integer*2 ietype

      if (ifl(72).eq.1) call conent(buf,sc(56),ietype)

999   return
      end

C
C*********************************************************************
C*    E_SUBROUTINE     : function wcsact()
C*      Returns whether MODSYS is active or not.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : True if MODSYS is active, False otherwise.
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      integer*4 function wcsact ()
c
      include 'com.com'
c
      wcsact = 0
		if (lwrk) wcsact = 1
      end
