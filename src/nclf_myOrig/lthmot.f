C*********************************************************************
C*    NAME         :  lthmot.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       lthmot.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:16
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     :
c*       called by rufcut to output motion during lathe/rough
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine lthmot

      include 'com4a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 e,f,a,b,asn,dbuf(6)
      real*4 ad(300)
      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40)
      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,ksn)
c
      equivalence  (idfr,kd(1)),(nds,kd(3)), (idx,kd(5)),  (icx,kd(6)),
     1            (ibx,kd(7)),(iax,kd(8)), (idpp,kd(9)), (icpp,kd(10)),
     2             (ibpp,kd(11)),(iapp,kd(12))
c        data for lthmot in a-tbl
      equivalence (ka(1),kcl),(ka(2),kpl)
c
      do 10 i=1,3
      dbuf(i) = a(i+1)
10    dbuf(i+3) = sc(i+3)
cc      if(kpl.eq.1) call plotm(dbuf,.false.)
      if(kcl.eq.1) call putcl(5000,5,1,dbuf)
      return
      end
