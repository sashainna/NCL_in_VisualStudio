
C*********************************************************************
C*    NAME         :  fininv.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       fininv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:04
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fininv
C*       called by finpre to reverse shape order in f-tbl
C*       (pp commands are dropped in this step)
C*
C*       build invers shape in g-tbl, copy back to f-tbl and
C*       continue as is.
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
      subroutine fininv

      include 'com4a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 asn,e,f,a,b,g(1000)
      real*4 ad(300),bsn(2)
      integer*2 ksc(500),kd(600),ksn(4),ke(4000),kf(4000)
      equivalence (sc,ksc),(d,ad,kd),(asn,bsn,ksn),(e,ke),(f,kf)
c
      nf=kf(3)
c               last ln from f to first into g(11-13)
      nst=nf-2
      j=10
      do 22 i=nst,nf
      j=j+1
22    g(j)=f(i)
c               sch from f-end back for ln/ci entries. add to g-tbl
      igx=13
      ifx=nf-1
c               basic sch loop
c
30    ifx=ifx-1
32    if(ifx.lt.14) goto 50
      asn=f(ifx)
      ks4=ksn(4)
      ks3=ksn(3)
      if(ks4.ne.5.or.ks3.ne.2)goto 40
c               line found.  look ahead for next ndpt, add line to g-tbl
      jfx=ifx
      do 34 i=1,20
      jfx=jfx-1
      if(jfx.lt.11)goto 36
      asn=f(jfx)
      if(ksn(4).eq.5.and.ksn(3).eq.2) goto 38
      if(ksn(4).eq.7.and.ksn(3).eq.6) goto 38
34    continue
c               no entity found.  assume f-stpt
36    ifx=11
      jfx=11
      goto 50
c               use prev ent ndpt
38    g(igx+1)=f(ifx)
      g(igx+2)=f(jfx+1)
      g(igx+3)=f(jfx+2)
      igx=igx+3
      if(jfx.lt.14)goto 50
      goto 30
40    if(ks4.ne.7.or.ks3.ne.6)goto 30
c               circle found.
      asn=f(ifx)
      ksn(2)=-ksn(2)
      g(igx+1)=asn
      oldx=f(ifx+1)
      oldy=f(ifx+2)
      g(igx+4)=f(ifx+3)
      g(igx+5)=f(ifx+4)
      g(igx+6)=f(ifx+5)
c               switch angst, angnd
      asn=f(ifx+6)
      angnd=bsn(1)
      angst=bsn(2)
      bsn(1)=angst
      bsn(2)=angnd
      g(igx+7)=asn
c               calc new xnd, ynd per angnd
      g(igx+2)=f(ifx+3)+f(ifx+5)*cos(angnd)
      g(igx+3)=f(ifx+4)+f(ifx+5)*sin(angnd)
      igx=igx+7
      goto 30
50    continue
c                add last line if not already in g-tbl
      if(g(igx-1).eq.f(12).and.g(igx).eq.f(13))goto 55
      g(igx+1)=f(11)
      g(igx+2)=f(12)
      g(igx+3)=f(13)
      igx=igx+3
c                fix f(1) for new nwds and copy g to f
55    kf(3)=igx
      do 60 i=2,igx
60    f(i)=g(i)
99    return
      end
