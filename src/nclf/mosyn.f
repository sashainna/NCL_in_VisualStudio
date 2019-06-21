
C*********************************************************************
C*    NAME         :  mosyn.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       mosyn.f , 25.4
C*    DATE AND TIME OF LAST  MODIFICATION
C*       01/20/17 , 11:17:05
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mosyn
c*       branch to mosyn1 or mosyn2 for handling  
c*       the motion generation statement type syntax checking         
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
      subroutine mosyn

      include 'com8a.com'
c      include 'mocom.com'

c
c...Nops
c
      if (ist .eq. 726) then
        ifl(276) = 1
        fautops = .false.
        if (nextyp .eq. 11) then
            sc(10)=0
            go to 99999
        endif
        if (nextyp .ne. 9) then
          call error (57)
          go to 99999
        endif
        call parsit
        call parsit
      endif
c
c...Tlaxis
c
       if (ist.eq.721) then
           call tlaxis
c
c...Pocket, Scrub, Fmill, Smill or Pmill
c
       else if (ist.eq.738.or.ist.eq.739.or.ist.eq.748.or.
     x          ist.eq.756.or.ist.eq.1051) then
           call mosyn2
       else if (ist .eq. 761) then
           call profsy
       else if (ist .eq. 765) then
           call vmpksy
       else if (ist .eq. 702 .or. ist .eq. 726) then
           call gosyn
       else
           call mosyn1
       endif

99999  return
       end

