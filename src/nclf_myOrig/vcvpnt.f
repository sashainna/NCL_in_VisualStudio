
C*********************************************************************
C*    NAME         :  vcvpnt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       vcvpnt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:53
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vcvpnt(sa,sb,ivx,icv)
c*       a vecv is in vv-tbl.  solve vec per same and add to
c*       basecv for pts 2,6,10,14  or  3,7,11,15
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
      subroutine vcvpnt(sa,sb,ivx,icv)

      include 'com4a.com'

      real*8 x,y,z
      common/vblok/vv(120)
      common/blok/x(16),y(16),z(16)

c              assume basecv 1
      ibx=-3
      idx=1
      if(icv.eq.2)goto 10
c              basecv pts are 4,8,12,16
      ibx=0
      idx=-1
10    ds=sb-sa
      do 20 i=1,4
      ibx=ibx+4
      s=sa
      if(i.eq.2)s=sa+.3345*ds
      if(i.eq.3)s=sb-.3345*ds
      if(i.eq.4)s=sb
      c1=(1.-s)**3
      c2=3.*s*(1.-s)**2
      c3=3.*s**2*(1.-s)
      c4=s**3
      a=c1*vv(ivx+1)+c2*vv(ivx+4)+c3*vv(ivx+7)+c4*vv(ivx+10)
      b=c1*vv(ivx+2)+c2*vv(ivx+5)+c3*vv(ivx+8)+c4*vv(ivx+11)
      c=c1*vv(ivx+3)+c2*vv(ivx+6)+c3*vv(ivx+9)+c4*vv(ivx+12)
c              unitize abc
      sec=sqrt(a**2+b**2+c**2)
      if(sec.gt.0.)goto 15
c              something phony
      ifl(2)=5
      goto 99
15    a=a/sec
      b=b/sec
      c=c/sec
      x(ibx+idx)=x(ibx)+a
      y(ibx+idx)=y(ibx)+b
20    z(ibx+idx)=z(ibx)+c
99    return
      end
