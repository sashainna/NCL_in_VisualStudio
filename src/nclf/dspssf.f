c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dspssf.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:00
c**
c*****************************************************
c**
c** COPYRIGHT (C) 1987 MILLS DATA SYSTEMS CORPORATION
c**
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dspssf (nclkey)
C*       display net surface type geometry.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey    - key of surface to display
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine dspssf (nclkey)

      include 'com4a.com'
      include 'mocom.com'

      integer*4 nclkey

      real*8 ssfhed(80),srfhed,asn,dbuf(3)
      integer*4 i4sshd(160),ikey
      integer*2 isshed(320),isfhed(4),ksn(4),isc11(4),isf,nsf
      equivalence (ssfhed,i4sshd,isshed),(d(51),srfhed,isfhed)
      equivalence (asn,ksn)
      equivalence (sc(11),isc11)
c                                 get header
      call gtgeo(nclkey,ssfhed)
      nsf=isshed(2)
c                                 display each surface
      do 100 isf=1,nsf
      ikey = i4sshd(isf+2)
      call sftype(ikey,isftyp)
      if (isftyp.eq.26) then
        call dspmsh (ikey,dbuf)
      else if (isftyp.eq.91) then
        call dsplsf (ikey,dbuf)
      else 
        call dspnwf (ikey, dbuf)
      endif
c                              check for operator abort
c RAH: added call to set intr flag - effective on SGI ONLY
      call ckintr(ifl(86),ifl(35))
      if (ifl(86).eq.1) then
        ifl(2)=-212
        go to 99999
      endif
100   continue

      call drwlab(dbuf(1), dbuf(2), dbuf(3), nclkey)

99999 return
      end
