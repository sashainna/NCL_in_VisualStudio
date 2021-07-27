
C*********************************************************************
C*    NAME         :  shafix.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       shafix.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:42
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine shafix(itsk)
c*       fix the shape inplace in f-tbl per rtool and add xdel,ydel
c*       for lth rough operation.  (the lathe input list is in d(1))
c*
c*       only lns-circs are used.  pp's are not used in roughing and
c*       modifiers have already been acted on.
C*    PARAMETERS   
C*       INPUT  : 
c*          itsk=1   matl shape     xdel=cldist,ydel=0
c*               2   part shape     xdel=xstk, ydel=ystk
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine shafix(itsk)
      include 'com8a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)
      common/lthdum/dum(80)

      real*8 e,f,a,b,dum

      real*8 asn
      real*4 ad(300),ang(2)
      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40),kf(4000)
      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,ksn),(f,kf)
      equivalence (pang,ang)

      real*8 tol,den,sec,pang


      tol = sc(27)

      if (itsk.eq.1) then
c
c..... this is the matl shape
c
        xdel=d(2)
        ydel=0.
        asn=d(1)
      else 
c
c..... this is the part shape
c
        xdel=d(4)
        ydel=d(5)
        asn=d(3)
      endif

      rtool=sc(28)/2.
      numf=ksn(3)
c
c..... init the read-fix process.  put lim1 in b-tbl
c
      iexit=0
      irx=10
      ilx=11
c
c.....       b(1,2,3,4,5,6) from f(8,9,10,2,3,4)
c
      do 22 i=1,6
      j=i+7
      if (i.gt.3) j=i-2
22    b(i)=f(j)

c
c ****************  basic read-fix loop start ***************
c
c..... move b to a, then find next entity, load in b
c
30     call conv8_8 (b,a,10)

33    irx=irx+1
      if (irx.ge.numf) then    
        if (iexit.eq.1) goto 80
c            put lim2 in b and do final pt (build ln ident in a(1))
        iexit=1
        b(4)=f(5)
        b(5)=f(6)
        b(6)=f(7)
        goto 43
      endif

      asn=f(irx)
      if (ksn(3).ne.2.or.ksn(4).ne.5) goto 34
c
c.....            line.  add to b
c
      b(1)=f(irx)
      b(2)=f(irx+1)
      b(3)=f(irx+2)
      goto 40

34    if (ksn(3).ne.6.or.ksn(4).ne.7) goto 33
c
c.....            circle.  add to b
c
      j=irx-1
      do 36 i=1,7
36    b(i)=f(j+i)
      goto 60

c
c..... fix b-ln per rtool
c
40    vx=b(2)-a(2)
      vy=b(3)-a(3)
      sec=sqrt(vx**2+vy**2)

      if (sec.lt.tol) then
c..... error.  line has no length.
        ifl(2)=31
        goto 99
      endif

      b(4)=vy/sec
      b(5)=-vx/sec
      b(6)=b(4)*b(2)+b(5)*b(3)+rtool
c
c..... a-entity: ln or ci
c
43    if (ka(4).eq.5) then    
c
c..... a is ln.  intersect 2 lns
c
        den=a(4)*b(5)-a(5)*b(4)
        if (dabs(den).lt.tol) then
c..... error.  lines are parlel.
          ifl(2)=167
          goto 99
        endif

        xi=(b(5)*a(6)-a(5)*b(6))/den
        yi=(a(4)*b(6)-b(4)*a(6))/den
c
c..... add modified a-ln to b/u list
c
        f(ilx)=a(1)
        f(ilx+1)=xi+xdel
        f(ilx+2)=yi+ydel
c
c..... hold endpt in case next ent is circle (4 plcs.)
c
        dum(79)=xi
        dum(80)=yi
        ilx=ilx+3
        irx=irx+2
      else if (ka(4).eq.7) then    
c
c..... a is circle. intersect a-ci and b-line
c
        call acibln
c
c..... add a-ci to b/u list
c
        ka2 = ka(2)
        if (ka2 .eq. -2) ka(2) = -1
        f(ilx)=a(1)
        f(ilx+1)=a(8)+xdel
        f(ilx+2)=a(9)+ydel
        f(ilx+3)=a(4)+xdel
        f(ilx+4)=a(5)+ydel
        asig=ka(2)
        f(ilx+5)=a(6)+asig*rtool
        call angcal
        pang=a(10)
        f(ilx+6)=a(10)
        dum(79)=a(8)
        dum(80)=a(9)

        if (ka2.eq.-2 .and. itsk.eq.2) then
          ka(2) = 1
          a2=a(2)
          a3=a(3)
          a(2) = b(2) + rtool*b(4)
          a(3) = b(3) + rtool*b(4)
          a(4) = a2
          a(5) = a3
          a(6) = rtool
          sc(28)=0.

          call acibln

          sc(28) = 2. * rtool
          ilx=ilx+7
          f(ilx)=a(1)
          f(ilx+1)=a(8)+xdel
          f(ilx+2)=a(9)+ydel
          f(ilx+3)=a(4)+xdel
          f(ilx+4)=a(5)+ydel
          f(ilx+5)=rtool
          call angcal
          pang=a(10)
          f(ilx+6)=a(10)
          dum(79)=a(8)
          dum(80)=a(9)
        endif

        ilx=ilx+7
        irx=irx+2
      endif

      goto 30
c
c..... b is circle.   if concave case and rtool too big, skip it.
c
60    if (kb(2).lt.0.and.rtool.ge.b(6)) goto 33
c
c..... bra on a-ln or a-ci
c
      if (ka(4).eq.5) then
c
c..... intersect a-ln b-ci
c
        call alnbci
c
c..... add a-ln to b/u list
c
        f(ilx)=a(1)
        f(ilx+1)=a(7)+xdel
        f(ilx+2)=a(8)+ydel
        dum(79)=a(7)
        dum(80)=a(8)
        ilx=ilx+3
        irx=irx+6
      else if (ka(4).eq.7) then
c
c..... a-ci b-ci
c
        call acibci
        if (ifl(2).gt.0) goto 99
c
c..... add a-ci to b/u
c
        f(ilx)=a(1)
        f(ilx+1)=a(8)+xdel
        f(ilx+2)=a(9)+ydel
        f(ilx+3)=a(4)+xdel
        f(ilx+4)=a(5)+ydel
        asig=ka(2)
        f(ilx+5)=a(6)+asig*rtool
        call angcal
        pang=a(10)
        f(ilx+6)=a(10)
        dum(79)=a(8)
        dum(80)=a(9)
        ilx=ilx+7
        irx=irx+6
      endif

      goto 30
c
c..... update f(1)
c
80    kf(3)=ilx-1

99    return
      end
