C*********************************************************************
C*    NAME         :  decclr.f
C*       CONTAINS:
C*    COPYRIGHT 2010 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       decclr.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:52
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine decclr
C*      this subroutine parses the color declaration statement            *
C*      the valid syntax is:                                              *
C*        (name)=color/r,g,b                                              *
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
      subroutine decclr

      include 'com8a.com'
      include 'suvcom.com'


      integer*4 nc, value(3),enum, strlen1
 
       ifl(44)=9
       call parsit
c************************************** must be a r,g,b
       do 460 i=1 , 3
           if (ityp.ne.3.and.ityp.ne.4) go to 430
c                                   ***   integer or real
           value(i)=tv
           if (nextyp.eq.11.or.(nextyp.eq.7.and.ifl(111).eq.1))
     1         go to 480
           go to 460
430        if (ityp.ne.2) go to 450
           if (ist.ne.2) go to 440
c                                   ***  scaler
                value(i)=tv
                if (nextyp.eq.11.or.(nextyp.eq.7.and.ifl(111).eq.1))
     1              go to 480
                go to 460
440        if (ist.eq.1) call error(9)
           if (ist.ne.1) call error(12)
           go to 99999
450        call error(7)
           go to 99999
460    if (i.lt.3) call parsit
       call error(4)
       go to 99999
480    if (i.lt.3) call error(7)
88889  if (ifl(111).eq.0) then
           if (nextyp.ne.11) then
4              call error(4)
               go to 99999
           endif
       endif
       nc = strlen1(savid2)
       call ncl_store_clr(savid2, nc, isvsub, 
     1      value(1), value(2), value(3), enum)
       if (enum.ne.0) call error(enum)
99999  return
       end
