C*********************************************************************
C*    NAME         :  displq.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       displq.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:55
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine displq (nclkey, dtype)
C*       display surface type geometry on plotter
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
      subroutine displq (nclkey,dtype)

      include 'com4a.com'
      include 'mocom.com'

      integer*4 nclkey
      integer*2 dtype

c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)

      real*8 asn,buf(3)
      real*4 ad(300),bsn(2),x(25),y(25),z(25)
      integer*2 kd51(4),ksn(4),ival(4),jval(4),kval(4),lval(4),mval(4)
      integer*4 nup,nvp,nupt,nvpt 
      equivalence (d,ad),(d(51),kd51),(asn,bsn,ksn)
      equivalence (ad(204),x),(ad(229),y),(ad(254),z)

      data ival/1,21,25,5/,jval/6,22,20,4/,kval/11,23,15,3/
      data lval/16,24,10,2/,mval/21,25,5,1/

c                    init
      asn=sc(11)
      ipg=ksn(1)
      iel=ksn(2)
      call gtsfdp (nup,nupt,nvp,nvpt)
      mnum=nup
      nnum=nvp
      call gtgeo(nclkey,d(51))
      npats=kd51(2)
c                    plot sides all patches
      do 40 ipat=1,npats
c
c                  get patch(ipat) from ranfil
      call gtqpat (nclkey,ipat,d(100))
      xo=ad(201)
      yo=ad(202)
      zo=ad(203)
c                          move to start point
      buf(1)=xo+x(1)
      buf(2)=yo+y(1)
      buf(3)=zo+z(1)
      call gmova3(buf(1),buf(2),buf(3))

c...
C      write(cout,9010) buf(1),buf(2),buf(3)
C9010  format(' after gmova3 buf('3f12.4')')
C      call putmsg(cout,80,1,0)
c...
      do 39 isid=1,4
      i=ival(isid)
      j=jval(isid)
      k=kval(isid)
      l=lval(isid)
      m=mval(isid)
c                    go along this cv 10 spcs
      u=0.
      do 30 ip=1,10
      u=u+.1
      um=1.-u
      c1=um**4
      c2=4.*u*um**3
      c3=6.*u**2*um**2
      c4=4.*u**3*um
      c5=u**4

      buf(1)=c1*x(i)+c2*x(j)+c3*x(k)+c4*x(l)+c5*x(m)+xo
      buf(2)=c1*y(i)+c2*y(j)+c3*y(k)+c4*y(l)+c5*y(m)+yo
      buf(3)=c1*z(i)+c2*z(j)+c3*z(k)+c4*z(l)+c5*z(m)+zo
      call glina3(buf(1),buf(2),buf(3))
c...
C      write(cout,9020) buf(1),buf(2),buf(3)
C9020  format(' after glina3 buf('3f12.4')')
C      call putmsg(cout,80,1,0)
c...
30    continue
39    continue
      call gdraw
40    continue
c                 finup work   (fut)

      return
      end
