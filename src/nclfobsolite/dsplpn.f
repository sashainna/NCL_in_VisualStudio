C*********************************************************************
C*    NAME         :  dsplpn.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dsplpn.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:59
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dsplpn(nclkey)
C*      draw patern 
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
      subroutine dsplpn(nclkey, tfmat)

c  dtype   type of display is 20

      include 'com8a.com'

      integer*4 nclkey
      real*8 tfmat(12)
      integer*4 npts,ntyp
      integer*2 i
      real*8 dbuf(6)
      real*8 p(3)
      real*8 pout(3)
c
c...draw patern
c
c
c...Added tranformation
c...Yurong 9/30/97
c
      call drwpn (nclkey,npts,ntyp, tfmat)
c
c...draw labels if patern of points
c
c     if (ntyp .eq. 1) then
         do 100 i=1,npts
            call gtpnpt (dbuf,ntyp,nclkey,i)
            if (ntyp .eq. 2) call medpot (dbuf(1),dbuf(4),dbuf)
c
c...Added tranformation
c...Yurong 9/30/97
c
            p(1) = dbuf(1)
            p(2) = dbuf(2)
            p(3) = dbuf(3)
            call cctmtf(p, tfmat, pout)            
            call drwlab(pout(1),pout(2),pout(3), nclkey)
100      continue
c     end if

99999 if (debug) then
         write(cout, 9010) dbuf(1),dbuf(2)
9010     format('dsplge ',2f7.2)
         call putmsg(cout,80,23,0)
      endif

      return
      end
