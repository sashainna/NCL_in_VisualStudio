C*********************************************************************
C*    NAME         :  vvcvpt.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       vvcvpt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:54
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vvcvpt(sa,sb,icv)
c*       12 vecs are in w-tbl this curve.
c*
c*        icv=2:   calc pt2, vxyz(1)  and  pt14, vxyz(2)
c*           =4:    "     3,   "       "     15    "
c*
c*        polfin for interior pts on ret to patpre
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
      subroutine vvcvpt(sa,sb,icv)

      include 'com4a.com'

      parameter (maxpt = 50)
      parameter (maxwd=(maxpt*6+(maxpt+1)/2))

      common/wblok/w(600)
      common/blok/x(16),y(16),z(16),dum(21),vx(2),vy(2),vz(2)

      real*4 aw(1200)
      real*8 w,x,y,z,dum,vx,vy,vz
      equivalence (w,aw)
c

      sdel=3./11.
      sran=sb-sa
      jwx=(2*(maxwd+20))*(icv-1)
c              assume basecv 1
      iapt=1
      k=1
      sig=1.

      if(icv.ne.4) goto 10
      iapt=4
      k=-1
      sig=-1.

10    ibpt=iapt+4
      s=sa

c              a vvcv is in w-tbl.  4pt interp for a,b,c    27-jul-84
20    ipa=11.*s
      if(ipa.lt.1)ipa=1
      if(ipa.gt.9)ipa=9
      apa=ipa
      sst=(apa-1.)/11.
      ds=-.01
      dx=x(ibpt)-x(iapt)
      dy=y(ibpt)-y(iapt)
      dz=z(ibpt)-z(iapt)

30    sn=s+ds

      ro=(sn-sst)/sdel
      c1=1.+ro*(-5.5+ro*(9.-4.5*ro))
      c2=ro*(9.+ro*(13.5*ro-22.5))
      c3=ro*(-4.5+ro*(18.-13.5*ro))
      c4=ro*(1.+ro*(-4.5+4.5*ro))
      j=jwx+3*ipa-2
      a=c1*aw(j)+c2*aw(j+3)+c3*aw(j+6)+c4*aw(j+9)
      j=j+1
      b=c1*aw(j)+c2*aw(j+3)+c3*aw(j+6)+c4*aw(j+9)
      j=j+1
      c=c1*aw(j)+c2*aw(j+3)+c3*aw(j+6)+c4*aw(j+9)
c              unitize abc
      sec=sqrt(a**2+b**2+c**2)*sig
      if(abs(sec).gt.0.)goto 35
c              something phony
33    ifl(2)=5
      goto 99
35    a=a/sec
      b=b/sec
      c=c/sec
c              if ds neg, save this v-set and go do another set.
      if(ds.gt.0.) goto 40
      ds=.01
      aa=a
      bb=b
      cc=c
      goto 30

40    x(iapt+k)=x(iapt)+(aa+a)/2.
      y(iapt+k)=y(iapt)+(bb+b)/2.
      z(iapt+k)=z(iapt)+(cc+c)/2.
      rho=.02/sran/.3345
      if(s.eq.sb)rho=-rho
      dxa=rho*dx
      dya=rho*dy
      dza=rho*dz
      l=1
      if(s.eq.sb) l=2
      tvx=dxa+a-aa
      tvy=dya+b-bb
      tvz=dza+c-cc
      sec=sqrt(tvx**2+tvy**2+tvz**2)
      if(sec.eq.0.)goto 33
      vx(l)=tvx/sec
      vy(l)=tvy/sec
      vz(l)=tvz/sec
      if(s.eq.sb)goto 99
c            do the same at sb
      s=sb
      iapt=iapt+12
      ibpt=ibpt+4
      goto 20

99    return
      end
