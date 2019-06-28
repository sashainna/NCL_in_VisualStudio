C*********************************************************************
C*    NAME         :  sfptdo.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       sfptdo.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:42
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sfptdo (w)
C*       description
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          w     R*8  D3  - vector components 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine sfptdo (w)
c
      include 'com4a.com'
      include 'mocom.com'
      include 'drvcom.com'
      include 'suvcom.com'
c
      real*8 w(3)

c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)

      real*8 e(6),c
      real*4 ad(300),ac(2)
      integer*2 kd(600),kc(4),ktv(4)
      equivalence (d,ad,kd),(ifl(54),isrf),(c,ac,kc),(tv,ktv)
      integer*4 nclkey
      integer*2 nwds,ietype, ifl72

c++ 1-jul-85          new dist and vec defs per pt1,sf1   1-jul-85
c                     get pt1 in e, sf1 in d(51) if nec.

      call gtgeom(sc(11),e,nclkey,nwds,ietype)
c                     set isrf to ds
      isrf=2
      call gtdesc (sc(12),nclkey,nwds,ietype)
      call getsuv(sc(12),isrf,u,v)
      call sfinit(sc(12),isrf,u,v)
      ifl(332) = 0
c                     load pt1 in e-tbl
      do 42 i=1,3
42    srf(i+7,isrf)=e(i)
c
      call surfpn(u,v,1)

      if(ifl(2).gt.0)goto 99

      if (auvset) then
        hu(2) = u
        hv(2) = v
        kuv(2) = nclkey
      endif
      dsuv = .false.

      c=sc(10)
      if(kc(2).gt.1) goto 60
c                  calc dist, add to sc(13), and return
      sc(13)=srf(1,2)*e(1)+srf(2,2)*e(2)+srf(3,2)*e(3)-srf(4,2)
      if(sc(13).lt.0.)sc(13)=-sc(13)
      goto 99

c                  vec/pt1,sf1.    move srfnrn to e and call putent
60    e(1)=srf(1,2)
      e(2)=srf(2,2)
      e(3)=srf(3,2)
      dis=e(1)*(srf(5,2)-srf(8,2))+e(2)*(srf(6,2)-srf(9,2))
     x    +e(3)*(srf(7,2)-srf(10,2))
      if(dis.ge.-1.e-6) goto 62
      e(1)=-e(1)
      e(2)=-e(2)
      e(3)=-e(3)
62    continue
      if (kc(2) .eq. 13) then
          w(1) = e(1)
          w(2) = e(2)
          w(3) = e(3)
      else
          ktv(3)=3
          ktv(4)=4
          ietype = ktv(4)
          ifl72 = ifl(72)
          ifl(72) = 0
          call ptentt(ietype, e, nclkey, tv)
          ifl(72) = ifl72
          rest=tv
      end if

99    return
      end
