C*********************************************************************
C*    NAME         :  parsuv.f
C*    COPYRIGHT 1991 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       parsuv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:24
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine parsuv
C*       Parse [U,V] syntax.
C*    PARAMETERS   
C*       INPUT  : 
C*          iret   - = 8 for curve, u only allowed
C*                 - = 9 for surface
C*       OUTPUT :  
C*          iret   - error code,  0 No error
C*                                7 Scalar expected
C*                              462 ']' (end of u or u,v) expected 
C*                              463 u,v range should be between 0.0 and 1.0
C*          lflg   - True if u or v found
C*          u      - u value
C*          v      - v value
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine parsuv(iret,lflg,u,v)

c      implicit undefined (a-z)
c      include '../incf/com.com'
      include 'com8a.com'

      integer*2 iret
      logical lflg
      real*4 u,v

      real*4 ut, vt
      integer*2 ietype, ifl44

      ietype = iret
      iret = 0
      lflg = .false.

C  If not '[' on entry, Not a parse u,v command 
      if (nextyp.ne.15) go to 99999
      ifl44 = ifl(44)
      ifl(44) = 0
      ut = u
      vt = v

C  Move past '['
      call parsit

C  Get 'u' value
      call parsit
      if (scalar) then
         lflg = .true.
         ut = tv
c
c..... qar 97142 - initialize vt, will be overwritten if a surface
c
         vt = 0.
         call parsit
      endif

C  Do we need a 'v' value
      if (ietype.eq.9) then

C  Is char a ','
         if (ityp.eq.5 .and. ist.eq.9) then
            call parsit
            if (scalar) then
               lflg = .true.
               vt = tv
               call parsit
            endif
         else
C  Error - Scalar expected
            if (.not. lflg) then
               iret = 7
               go to 9999
            endif
         endif
      endif

C  Is character a ']'
      if (ityp.eq.5 .and. ist.eq.16) then
          if (lflg) then

C  Check range for u,v values
     
              if (ut .ge. -.0001 .and. ut .lt. 0.) ut = 0.
              if (ut .le. 1.0001 .and. ut .gt. 1.) ut = 1.
              if (vt .ge. -.0001 .and. vt .lt. 0.) vt = 0.
              if (vt .le. 1.0001 .and. vt .gt. 1.) vt = 1.
              if ((ut.ge.0.0 .and. ut.le.1.0) .and.
     1            (vt.ge.0.0 .and. vt.le.1.0)) then
                   u = ut
                   v = vt
                   if (nextyp.eq.9 .and. ifl44.ne.0) call parsit
                   go to 9999

C  Error - u,v range should be between 0.0 and 1.0
              else
                  iret = 463
                  lflg = .false.
                  go to 9999
              endif

C  Error - Scalar expected
          else
              iret = 7
              go to 9999
          endif

C  Error - ']' (end of u or u,v) expected
      else
          iret = 462
          lflg = .false.
          go to 9999
      endif

9999  ifl(44) = ifl44

99999 return
      end
