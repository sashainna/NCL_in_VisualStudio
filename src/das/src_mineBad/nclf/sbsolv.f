C*********************************************************************
C*    NAME         :  sbsolv.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       sbsolv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:39
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sbsolv(sa,sb,icrv)
c*       calculates the s-values along a curve  
c*       to break a surface panel up into patches.                     
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
      subroutine sbsolv(sa,sb,icrv,itwist)
       
      include 'com8a.com'

      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

      common/wblok/w((maxwd+20)*4)
      real*8 w

      integer*2 itwist

      real*4 aw((maxwd+20)*8)
      real*4 s,dxs,emax,esm,pm,xs,sa,sb
      integer*2 lsc(100)
      equivalence (w,aw),(sc,lsc)
      logical noerase

      noerase=.false.
      if(sc(27).le.0.) sc(27)=.001
      esm=sc(27)/3.
      if(esm.lt..00005) esm=.00005

      iwx=(maxwd+20)*(icrv-1)
      jwx=2*iwx
      ix1=iwx+maxwd
      ix2=iwx+maxwd*2+20
      jx1=ix1*2
      jx2=ix2*2
c          phase 1: increase s until real err found at mid pt of segment.
c          phase 2: if phase 1 exceeded toler, decrease s until error intol.
c          phase 3 & 4: check that .25 & .75 pts are in tol. If not, repeat phase 2.
      iphase=1
c
c..... Calculate point-vector at s=sa
c
      s=sa
      pm=1.0
      if (itwist.eq.1 .and. ifl(346).eq.0) then
         s=1.0-sa
         pm=-1.0
      endif

      call crvpnt(s,iwx,0,1)
      w(ix2+1)=w(ix1+12)
      w(ix2+2)=w(ix1+13)
      w(ix2+3)=w(ix1+14)
      aw(jx2+13)=pm*aw(jx1+29)
      aw(jx2+14)=pm*aw(jx1+30)
      aw(jx2+15)=pm*aw(jx1+31)
      xs=.05
      dxs=.056
      iknt=0
c
c..... if icrv=3, begin with s=sb from crv1
c
      if(icrv.eq.1) goto 20
      iphase=2
      xs=sb-sa
c
c..... guard against the unique rld case of ent1=pt,ent2=symm crv
c
      if(lsc(39).eq.2.and.lsc(48).eq.8.and.xs.gt..99) xs=.9
c
c..... guard against infinite loop
c
20    if(sa+xs.gt.1.) xs=1.-sa
      iknt=iknt+1
      if(iknt.lt.100) goto 21
c
c..... breakup into patches failed - exit
c
      ifl(2)=131
      goto 99

21    s=sa+xs
      sb=s
      pm=1.0
      if (itwist.eq.1 .and. ifl(346).eq.0) then
         s=1.0-s
         pm=-1.0
      endif

      call crvpnt(s,iwx,0,1)
      w(ix2+4)=w(ix1+12)
      w(ix2+5)=w(ix1+13)
      w(ix2+6)=w(ix1+14)
      aw(jx2+16)=pm*aw(jx1+29)
      aw(jx2+17)=pm*aw(jx1+30)
      aw(jx2+18)=pm*aw(jx1+31)
c
c..... p.5
c
      s=sa+xs*.5
      if (itwist.eq.1 .and. ifl(346).eq.0) s=1.0-s
30    call crvpnt (s,iwx,0,0)
      aw(jx2+31)=w(ix1+12)-w(ix2+1)
      aw(jx2+32)=w(ix1+13)-w(ix2+2)
      aw(jx2+33)=w(ix1+14)-w(ix2+3)
c
c..... calc err this sb
c
      jtsk=1
      call errpol(ix2,emax,jtsk)
c
c..... if err in ok range, exit
c
      if(emax.lt.esm) goto 40
c
c..... phase 2
c
      iphase=2
      xs=xs*.9
      goto 20

40    if(sa+xs.gt..999.or.iphase.gt.1) goto 50

      dxs=dxs+.02
      xs=xs+dxs
      goto 20

50    if (iphase.eq.4) goto 90

      iphase=iphase+1
      if (iphase.eq.3) s=sa+xs*.25
      if (iphase.eq.4) s=sa+xs*.75
      if (itwist.eq.1 .and. ifl(346).eq.0) s=1.0-s
      goto 30

90    sb=sa+xs
99    return
      end
