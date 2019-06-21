C*********************************************************************
C*    NAME         :  errpol.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       errpol.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:01
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine errpol (ix,emax,jtsk)
C*          this routine uses the data in w(ix) work area,
C*          builds the polgon thru pva,pvb and solves err to pt.5
C*
C*          pq,r,j deltas are in w on return to patgen
C*
C*          jtsk=1   solve polgon and p.5 err
C*              =0   solve polgon only.
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
      subroutine errpol (ix,emax,jtsk)

      include 'com4a.com'

      common/wblok/w(600)

      real*8 w
      real*4 aw(1200)
      equivalence (w,aw)

cuni     write(16,1000) ix, emax, jtsk
cuni1000 format('errpol: ix=',i6,' emax='f12.6,' jtsk=',i6)
cuni     write(16,1010) (w(i), i=ix+1, ix+6)
cuni1010 format(' w(ix)',6f12.6)
c          pt j  deltas
      xj=w(ix+4)-w(ix+1)
      yj=w(ix+5)-w(ix+2)
      zj=w(ix+6)-w(ix+3)
c          add to aw now.
      jx=2*ix
      aw(jx+25)=xj
      aw(jx+26)=yj
      aw(jx+27)=zj
      chd=sqrt(xj**2+yj**2+zj**2)
      cal=(aw(jx+13)*xj+aw(jx+14)*yj+aw(jx+15)*zj)/chd
      cbe=(aw(jx+16)*xj+aw(jx+17)*yj+aw(jx+18)*zj)/chd
      adis=.3345*chd
      bdis=adis
c          do a rgt sense (if alfa zero, use beta)
      kx=12+jx
      if(cal.gt..999999)kx=15+jx
      aw(jx+37)=aw(kx+3)*yj-aw(kx+2)*zj
      aw(jx+38)=aw(kx+1)*zj-aw(kx+3)*xj
      aw(jx+39)=aw(kx+2)*xj-aw(kx+1)*yj
c          if rgt sense all zeros, this is st ln. create an
c          artificial vec to avoid upcheck failure in errcal.
      if(aw(jx+37)**2+aw(jx+38)**2+aw(jx+39)**2.gt.1.e-6)goto 20
      aw(jx+37)=1.
      aw(jx+38)=1.0101
      aw(jx+39)=1.0202
c
20    ad=adis
      bd=bdis
c          pt q deltas
      aw(jx+19)=ad*aw(jx+13)
      aw(jx+20)=ad*aw(jx+14)
      aw(jx+21)=ad*aw(jx+15)
c          pt r deltas
      aw(jx+22)=xj-bd*aw(jx+16)
      aw(jx+23)=yj-bd*aw(jx+17)
      aw(jx+24)=zj-bd*aw(jx+18)
c          if jtsk=0, polgon is done.
      if(jtsk.eq.0)goto 99
c          point to p.5 and get dev from present bezcub
      jex=jx+30
      call errcal(jx,jex,perr)
      emax=abs(perr)

99    return
      end
