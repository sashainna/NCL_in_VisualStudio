C*********************************************************************
C*    NAME         :  lblchk.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       lblchk.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:14
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine lblchk (itype,lblst)
C*                checks to see if the label flag is set.
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
      subroutine lblchk (itype,lblst)
c

      include 'com8a.com'

      integer*2 itype,lblst

c
c
c    find out what type geometry it is and if the label flag is set for it. 
c
      lblst = 0
      if (itype .eq. 3) then
        if (lablpt) then
          if (lbldpt) then
            lblst = 5
          else 
           lblst = 1
          endif
        else 
          if (lbldpt) lblst = 4
        endif 
      else if (itype .eq. 5) then
      if (lablln) then
        if (lbldln) then
          lblst = 5
        else 
          lblst = 1
        endif
      else 
        if (lbldln) lblst = 4
      endif 
      else if (itype .eq. 6) then
        if (lablpl) then
          if (lbldpl) then
            lblst = 5
          else 
            lblst = 1
          endif
        else 
          if (lbldpl) lblst = 4
        endif
      else if (itype .eq. 7) then
         if (lablci) then
           if (lbldci) then
             lblst = 5
           else 
             lblst = 1
           endif
         else 
           if (lbldci) lblst = 4
         endif
      else if (itype .eq. 8) then
         if (lablcv) then
           if (lbldcv) then
             lblst = 5
           else 
             lblst = 1
           endif
         else 
           if (lbldcv) lblst = 4
         endif
      else if (itype .eq. 9) then
         if (lablsf) then
           if (lbldsf) then
             lblst = 5
           else 
             lblst = 1
           endif
         else 
           if (lbldsf) lblst = 4
         endif
      else if (itype .eq. 10) then
        if (lablmx) then
          if (lbldmx) then
            lblst = 5
          else 
            lblst = 1
          endif
        else 
          if (lbldmx) lblst = 4
        endif
      else if (itype .eq. 18) then
        if (lablsh) then
          if (lbldsh) then
            lblst = 5
          else 
            lblst = 1
          endif
        else 
          if (lbldsh) lblst = 4
        endif
      else if (itype .eq. 20) then
        if (lablpn) then
          if (lbldpn) then
            lblst = 5
          else 
            lblst = 1
          endif
        else 
          if (lbldpn) lblst = 4
        endif
      else if (itype .eq. 21) then
        if (lablpv) then
          if (lbldpv) then
            lblst = 5
          else 
            lblst = 1
          endif
        else 
          if (lbldpv) lblst = 4
        endif
c
c...ANOTE
c
      else if (itype .eq. VANOTE) then
        if (lablan) then
          if (lbldan) then
            lblst = 5
          else 
            lblst = 1
          endif
        else 
          if (lbldan) lblst = 4
        endif
c
c...SYMBOL
c
      else if (itype .eq. VPLACE) then
        if (lablsy) then
          if (lbldsy) then
            lblst = 5
          else 
            lblst = 1
          endif
        else 
          if (lbldsy) lblst = 4
        endif
c
c...SOLID
c
      else if (itype .eq. VSOLID) then
        if (lablso) then
          if (lbldso) then
            lblst = 5
          else 
            lblst = 1
          endif
        else 
          if (lbldso) lblst = 4
        endif
      endif
c
      return
      end
