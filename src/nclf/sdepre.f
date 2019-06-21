C*********************************************************************
C*    NAME         :  sdepre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sdepre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:39
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sdepre
C*       Define a four curve surface.
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
      subroutine sdepre

      include 'com8a.com'
c      implicit undefined (a-z)
c      include '../incf/com.com'

      common/pblok/p
      real*8 p(500)
      integer*4 ids(1000)
      equivalence (p,ids)

      common/keycom/keyold
      integer*4 keyold

      integer*2 i, nwds, ietype
      integer*4 nclkey
      real*8 asn
      integer*2 ksn(4)
      equivalence (asn,ksn)

      asn=sc(10)
c
c... Load keys of entities that will define surf.
c
      do 20 i = 1,4
        call gtdesc (sc(11+i),ids(i),nwds,ietype)
20    continue
c
c... Pass in key of surface being redefined so attributes will be retained
c
      nclkey = keyold
c
c... Set defwf so surface will be labelled correctly
c
      defwf = .true.
      i = sc(11)
      call sdedef(ids, i, nclkey)
      if (nclkey .eq. 0) then
        ifl(2) = 51
        err = .true.
        goto 99
      endif
      ietype = 9
      call ptdesc (nclkey, ietype, tv)
      call srfcls (nclkey)
      rest = tv

99    return
      end
