C*********************************************************************
C*    NAME         :  patgen.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       patgen.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:24
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine patgen
c*       purpose of subroutine: this routine builds a set of patches for 
c*       4 curves in the 'w' array.                                    
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
      subroutine patgen

      include 'com8a.com'

      integer*2 maxpt, maxwd
      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

      common/wblok/w(4*(maxwd+20))
      common/pblok/p(400)
      real*8 w,p,asn
 
      common/twblok/ktwist(2*maxpt)
      integer*2 ktwist

      real*4 aw(8*(maxwd+20))
      real*4 s(maxpt),ap(800),sa,sb,ds
      integer*2 i, ksn(4)

      equivalence (asn,ksn),(w,aw),(p,ap)

      asn=sc(10)
      numpan=(ksn(3)-2)/2
c
c..... start at s=0, and generate s-list along cv1, cv3
c
      i=1
      s(1)=0.
10    sa=s(i)
c
c..... do cv1 and cv3 sb values
c
      call sbsolv(sa,sb,1,ktwist(1))
      sb1=sb
      call sbsolv(sa,sb,3,ktwist(3))
      sb3=sb
      i=i+1
      if(i.gt.maxpt) goto 997
      s(i)=sb3
      if(sb1.lt.sb3) s(i)=sb1

      ds=s(i)-s(i-1)
c
c..... limit ds to .3333 in the multi-panel case   6/14/82
c
      if(ds.gt..3333 .and. numpan.gt.1) s(i)=s(i-1)+.3333

      if(s(i) .lt. .9999) goto 10
      npat=i-1
c
c..... init the p-storage array word no. 1
c
      ksn(1)=2+(npat+1)/2+(npat-1)*20+28
      ksn(2)=npat
      ksn(3)=ifl(7)
      ksn(4)=ifl(8)
      p(1)=asn
c
c..... add s-list to p-storage array
c
      do 20 i=1,npat
20    ap(i+4)=s(i)
c
c..... build patches per this s-list
c
      do 40 i=1,npat
      ipat=i
      sa=s(i)
      sb=s(i+1)
      call patpre(sa,sb,ipat)
40    continue

      goto 999

997   ifl(2)=156
      err=.true.

999   return
      end
