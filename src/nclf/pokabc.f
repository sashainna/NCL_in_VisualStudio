
C*********************************************************************
C*    NAME         :  pokabc.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       pokabc.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:26
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokabc
c*       init abc for this point set
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
      subroutine pokabc

      include 'com4a.com'
      include 'mocom.com'

      common/pokom/tdat,bu,x(20),y(20),a(20),b(20),c(20)
     1,hx(20),hy(20),rgt,amt,finamt,dist,salmin,psk,dnat
     2,npts,inpts,npold,ibux,stepno, first

      real*8 asn,tdat(3),bj(35)
      real*4 ad(300),bu(900)
      integer*2 ksn(4),kd(600),stepno
      logical first
      equivalence(asn,bsn,ksn),(jb,bj),(d,ad,kd)

      do 20 i=1,npts
      j=i+1
      if(j.gt.npts) j=1
      dx=x(j)-x(i)
      dy=y(j)-y(i)
      tl=sqrt(dx**2+dy**2)
      if(tl.lt..001)goto 98
      a(i)=dy*rgt/tl
      b(i)=-dx*rgt/tl
20    c(i)=a(i)*x(i)+b(i)*y(i)
      goto 99
98    ifl(2)=48
99    return
      end
