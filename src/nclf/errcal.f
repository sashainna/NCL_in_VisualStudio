C*********************************************************************
C*    NAME         :  errcal.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       errcal.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:01
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine errcal(jx,jex,perr)
C*       calculates the directed error between the
C*       in aw(jx) work area and the point in aw(jex)
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
      subroutine errcal(jx,jex,perr)

      include 'com4a.com'

      common/wblok/w(600)

      real*8 w
      real*4 aw(1200)
      equivalence (w,aw)

c          expt is in aw(jex)
      ix=jx/2
      xp=aw(jex+1)
      yp=aw(jex+2)
      zp=aw(jex+3)
      idebug=0
8     u=.5
      du=.01
      itim=0
      ctan=.5
10    ca=(1.-u)**2
      cb=2.*u*(1.-u)
      cc=u**2
      x1=cb*aw(jx+19)+cc*aw(jx+22)
      y1=cb*aw(jx+20)+cc*aw(jx+23)
      z1=cb*aw(jx+21)+cc*aw(jx+24)
      dx=ca*aw(jx+19)+cb*aw(jx+22)+cc*aw(jx+25) - x1
      dy=ca*aw(jx+20)+cb*aw(jx+23)+cc*aw(jx+26) - y1
      dz=ca*aw(jx+21)+cb*aw(jx+24)+cc*aw(jx+27) - z1
      uerr=(dx*(xp-x1)+dy*(yp-y1)+dz*(zp-z1))/
     1 (dx**2+dy**2+dz**2) -u
      if(abs(uerr).lt.1.e-5)goto 30
      if(itim.eq.0)goto 20
c          if no real uerr chg, use old e/u slope
      if(abs(oerr-uerr).lt.1.e-5)goto 12
      ctan=du/(oerr-uerr)
12    du=uerr*ctan
      if(u+du.gt.1.)du=1.-u
      if(u+du.lt.0.)du=-u
20    itim=itim+1
c      if(idebug.gt.0)write(6,5)jx,xp,yp,zp
c5     format(' cubic jx =',i5,9x,'xyz=',3f12.6)
c      if(idebug.gt.0)write(6,6)itim,u,uerr
c6     format(' errcal data ',i6,3x,2f14.7)
      u=u+du
      oerr=uerr
      if(itim.lt.100)goto 10
c          too many iters.  Set error large & exit. IJD 22-FEB-1991
28    continue
      perr=999.
      goto 99
c28   if(idebug.gt.0) goto 99
c     ifl(2)=5
c     idebug=1
c     goto 8

c          found u may be o/b by .01 max
30    if(u.lt.-.01.or.u.gt.1.01)goto 28
c          do 'up' per rgt and dxyz
      xup=aw(jx+38)*dz-aw(jx+39)*dy
      yup=aw(jx+39)*dx-aw(jx+37)*dz
      zup=aw(jx+37)*dy-aw(jx+38)*dx
      sec=sqrt(xup**2+yup**2+zup**2)
      perr=(xup*(xp-x1)+yup*(yp-y1)+zup*(zp-z1))/sec

99    return
      end
