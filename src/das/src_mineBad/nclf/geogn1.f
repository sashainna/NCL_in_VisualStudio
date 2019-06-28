C*********************************************************************
C*    NAME         :  geogn1.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       geogn1.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:05
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine geogn1
C*       call geogna,b, or c
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
      subroutine geogn1

      include 'com8a.com'

      integer*2 isc(100),ktv(4),nw,pg,el,peq(4),ksn(4)
      real*8 a(35),pgelnw,w(12)
      equivalence (a,jb),(sc(10),isc),(tv,ktv),(peq(1),pgelnw)
      equivalence (pg,peq(1)),(el,peq(2)),(nw,peq(3)),(asn,ksn)
      logical nored

      nored=.false.
      ktv(4)=idst
      isub=isc(2)

c          pt/7  and  ln/10    call geognd     (also pt/10  2-may-83)
      if(idst.eq.3.and.(isub.eq.7.or.isub.eq.10)) goto 6
      if(idst.eq.3.and.isub.eq.16) goto 7
      if(idst.eq.5.and.isub.eq.10) goto 6
      goto 8
6     call geognd
      goto 99
c              pt/16 (pt/cv1,dis) goes to geogne  26-aug-86
7     call geogne (w)
      go to 99

8     continue
c

c          branch to geo type or mx def routines
      if(idst.gt.4) goto 10
c       **************************  pt 0r vec
      call geogna
      goto 99
10    if(idst.gt.6) goto 20
c       **************************  ln or pl
      call geognc
      goto 99
c       **************************  mx
20    call geognb

99    return
      end
