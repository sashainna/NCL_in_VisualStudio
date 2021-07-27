C*********************************************************************
C*    NAME         :  cvofst.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cvofst.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:48
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvofst
C*       this routine handles curve/cv1,x,y,z and 
C*                           spline/cv1,x,y,z
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
      subroutine cvofst

      include 'com8a.com'
      include 'wrksys.com'

      common/wblok/w(600)

      real*4 aw(1200)
      integer*2 ksn(4),ktv(4)
      integer*4 nclkey
      integer*2 nwds, ietype, isf, iwf, ksn2
      logical trflg,lv90

      equivalence (asn,ksn),(w,aw),(tv,ktv)
      real*8 sc10,delx,dely,delz,tbuf(3),dtol,dx,dy,dz,delxyz(3)
      equivalence (delxyz(1),delx),(delxyz(2),dely),(delxyz(3),delz)

      lv90 = sc(169).lt.9.049d0

      asn = sc(10)
      ksn2 = ksn(2)

      if (ksn2.ne.3 .and. ksn2.ne.16) goto 990

      sc10=sc(10)
      asn=sc(11)
      ktv(3)=ksn(3)

      call gtdesc (asn, nclkey, nwds, ietype)
      call cvtype1(nclkey,nss)
      if (ietype .eq. curve) then
        if (nss .eq. 89 .or. nss. eq. 90) then
          iwf = 1
        else
          call isitwf (nclkey, iwf)
        endif
      else if (ietype .eq. circle .or. ietype.eq.line) then
        iwf = 1
      else
        goto 990
      endif

      if (ksn2 .eq. 16) goto 50

c*****************************   cv/cv1,<xval>,<yval>,<zval>     -- 3 --
      if (iwf.eq.1) then
        call cvofwf (nclkey)
        sc(10)=sc10
        if (ifl(2).gt.0) goto 990
        ietype = CURVE
c       if (lwrk .and. .not.lv84) call transf(w,invwrk,nwds,ietype)
        if (lwrk .and. .not.lv90) then
c
c...CVOFWF returns the curve in the current units (inch/mm)
c...but, the modsys matrix is always in inches
c...so do a conversion here
c...Bobby  -  01/09/01
c
            if (ifl(264) .eq. 1) then
                dx = invwrk(4)
                dy = invwrk(8)
                dz = invwrk(12)
                invwrk(4) = invwrk(4) * 25.4
                invwrk(8) = invwrk(8) * 25.4
                invwrk(12) = invwrk(12) * 25.4
                call transf(w,invwrk,nwds,ietype)
                invwrk(4) = dx
                invwrk(8) = dy
                invwrk(12) = dz
            else
                call transf(w,invwrk,nwds,ietype)
            endif
        endif
        if (ifl(72).eq.1) call transf (w, sc(68), nwds, ietype)
      else
        trflg = .true.
        call gtentt(sc(11), trflg, nclkey, ietype, w(1))
        if (ietype .eq. 8) call ncl_get_tpar (nclkey,tbuf)
      endif

      if (isc10(3).eq.0) goto 34
      delx=sc(12)
      dely=sc(13)
      delz=sc(14)
      
      nseg=aw(1)
      do 32 i=1,nseg
        ix=(nseg+1)/2+6*i-5
        w(ix)=w(ix)+delx
        if (isc10(3).gt.1) w(ix+1)=w(ix+1)+dely
        if (isc10(3).gt.2) w(ix+2)=w(ix+2)+delz
32    continue

34    continue
      ktv(4)=8
      ietype = 8
      call ptentt(ietype, w, nclkey, rest)
      if (ietype .eq. 8 .and. iwf .eq. 0) 
     -      call ncl_put_tpar (nclkey,tbuf)
      goto 999

c*****************************   sp/cv1,<xval>,<yval>,<zval>     -- 16 --
50    defwf = .true.

      delx = 0.
      dely = 0.
      delz = 0. 
      if (isc10(3).gt.0) delx = sc(12)
      if (isc10(3).gt.1) dely = sc(13)
      if (isc10(3).gt.2) delz = sc(14)
      
      if (ifl(72).eq.1) call conent(delxyz,sc(56),VECTOR)

      dtol = sc(27)
      if (ifl(264) .eq. 1) then
         dtol = dtol/25.4d0
         delx = delx/25.4d0
         dely = dely/25.4d0
         delz = delz/25.4d0
      endif
      if (lwrk) call conent(delxyz,wrkmx,VECTOR)
c
c..... If the curve is a B-spline (nss=7) or an NCL curve (iwf=0), copy
c..... the curve structure directly to a spline structure, then translate
c..... the control points. Otherwise, evolve the curve, translate the
c..... evolved points, and fit a spline.
c
      if (nss .eq. 7) then
        call ofswf (nclkey,delx,dely,delz,ifl(2))
      else if (nss.eq.CURVE .and. iwf .eq. 0) then
        call ncvsp (nclkey,delx,dely,delz,ifl(2))
      else
        isf = 3
        call evstup (nclkey, isf)       
        call ncvofs (nclkey,delx,dely,delz,dtol,ifl(2))
      endif

      if (nclkey.eq.0) goto 990

      call ptdesc (nclkey,ietype,asn)
      rest = asn

      goto 999
c
c..... error exit.
c
990   if (ifl(2).lt.1) ifl(2)=5
      err=.true.
 
999   if (ifl(2).lt.1) call crvcls (nclkey)
      return
      end
