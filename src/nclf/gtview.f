C*********************************************************************
C*    NAME         :  gtview.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
c*    MODULE NAME AND RELEASE LEVEL 
C*       gtview.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:11
c*********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtview
C*    PARAMETERS   
C*       INPUT  : 
C*          option: 1 = return vectors based on REFSYS matrix
C*                  2 = return vectors based on TRACUT matrix
C*                  3 = return vectors based on current tool position.
C*       OUTPUT :  
C*          rsorig: X, Y & Z offset coordinates from origin
C*          rszvec: I, J & K directions coordinates for viewing vector
C*          rxyvec: I, J & K directions coordiantes for up vector
C*          status: 0 = origin and vectors set from REFSYS or TRACUT 
C*                       MATRIX
C*                 -1 = No REFSYS or TRACUT was in effect
C*    RETURNS      : see "status" above
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine gtview (rsorig, rszvec, rsyvec, option, status)

      include 'com8a.com'
      include 'wrksys.com'

      real*8 rsorig(3), rszvec(3), rsyvec(3)
      integer*2 option, status

      real*8 scmm
      integer*2 i2v3, i2v4

      status = 0
c 
c         refsys
      if (option .eq. 1 .and. ifl(72).eq.1) then
          rsorig(1) = sc(59)
          rsorig(2) = sc(63)
          rsorig(3) = sc(67)

          rszvec(1) = sc(58)
          rszvec(2) = sc(62)
          rszvec(3) = sc(66)

          rsyvec(1) = sc(57)
          rsyvec(2) = sc(61)
          rsyvec(3) = sc(65)
c 
c         tracut
      else if (option .eq. 2 .and. ifl(73).eq.1) then
          rsorig(1) = sc(44)
          rsorig(2) = sc(48)
          rsorig(3) = sc(52)

          rszvec(1) = sc(43)
          rszvec(2) = sc(47)
          rszvec(3) = sc(51)

          rsyvec(1) = sc(42)
          rsyvec(2) = sc(46)
          rsyvec(3) = sc(50)
c
c... Tool position.
c
      else if (option .eq. 3) then
          rsorig(1) = sc(1)
          rsorig(2) = sc(2)
          rsorig(3) = sc(3)

          rszvec(1) = sc(4)
          rszvec(2) = sc(5)
          rszvec(3) = sc(6)

          rsyvec(1) = sc(7)
          rsyvec(2) = sc(8)
          rsyvec(3) = sc(9)
c
c... error condition.
c
      else
          status = -1
      endif
c
c...Transform for units mm and modsys
c
      if (status.eq.0) then
        if (ifl(264).eq.1) then
          scmm = 1.0d0/25.4d0
          call vctmsc(rsorig,rsorig,scmm)
        endif
        if (lwrk) then
          call conent(rsorig,wrkmx,POINT)
          call conent(rszvec,wrkmx,VECTOR)
          call conent(rsyvec,wrkmx,VECTOR)
          call unitizevc(rszvec)
          call unitizevc(rsyvec)
        endif
      endif

      return
      end
