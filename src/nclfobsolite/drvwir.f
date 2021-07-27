C*********************************************************************
C*    NAME         :  drvwir.f
C*       CONTAINS:
C*     subroutine drvwir
C*    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       drvwir.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:58
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine drvwir
C*      Drive a curve as both the part and drive surface.
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
      subroutine drvwir (nclkey)

      include 'com.com'
      include 'mocom.com'
      include 'wrksys.com'

      integer*4 nclkey

c          motion common equivalences
      integer*2 kd(600)
      equivalence (d,kd)

      real*4 asc(100)
      equivalence (sc,asc)
      integer*2 ntk, iptk
      equivalence (ntk,ifl(79)), (iptk,ifl(50))

      real*8 buf(640), pt(21), ve(3), pmod(3), vr(3), ds(3)
      real*8 ti,tj,tk, xu,yu,zu, xr,yr,zr, sec, psthk
      real*8 h, hcr, dst, d1, cr, sbe, cbe, tol, fct
      integer*2 i, n, lnk, jptk, iclass, isubcl, mupt
      integer*2 izro
      data izro  /0/
      logical LFLSE,lv92
      data LFLSE /.false./
c
c...   Initialize variables
c
      lnk = 1
      jptk = 0
      lv92 = sc(169) .lt. 9.249d0
cc      if (lexpcl) then
      if (.not. lv92) then
        mupt = 21
        iclass = 5200
      else
        mupt = 3 * (ifl(82) + 1)
        iclass = 5000
      endif
      isubcl = 5
      ntk = 0
      iptk = 0
      fct = 1.0
      if (ifl(264).eq.1) fct = 25.4
      psthk = sc(23)
      ti = sc(4)
      tj = sc(5)
      tk = sc(6)
      pt(4) = ti
      pt(5) = tj
      pt(6) = tk
      do 10 i=10,21
10    pt(i) = 0.0d0
c
c...   Set initial right from current forward * tool axis
c
      vr(1) = sc(8)*tk-sc(9)*tj
      vr(2) = sc(8)*ti-sc(7)*tk
      vr(3) = sc(7)*tj-sc(8)*ti
c
c...   Set up vector distance of ps thick up tool axis
c
      xu = ti*psthk
      yu = tj*psthk
      zu = tk*psthk
c
c...   Calculate right offset for this height up tool axis
c
      dst = tool(6)
      if (psthk.lt.0.0d0) then
        sbe = asc(63)
        cbe = asc(64)
        if (cbe.eq.0.0d0) cbe = 1.0
        cr = sc(29)
        hcr = cr*(1.0-sbe)
        h = -psthk
        if (h.gt.hcr) then
          dst = dst + (h-hcr)*sbe/cbe
          h = hcr
        endif
        d1 = cr**2-(cr-h)**2
        if (d1.gt.0.0d0) dst = dst+dsqrt(d1)
      endif
      dst = dst*ifl(21)
c
c...   Initialize curve points
c
      tol = sc(27) / fct
      call cptint (nclkey, tol, izro, n)
      if (n.lt.1) goto 9028
c
c...   Loop through points, offset if tllft, tlrgt or thick & output to cl
c...   plot etc.
c
      do 100 i=1,n
        call cptnxt (pt,ve)
        if (lwrk) then
          call conent(pt,invwrk,POINT)
          call conent(ve,invwrk,VECTOR)
        endif
        xr = ve(2)*tk-ve(3)*tj
        yr = ve(3)*ti-ve(1)*tk
        zr = ve(1)*tj-ve(2)*ti
        sec = dsqrt(xr**2+yr**2+zr**2)
c
c...   If right vector not real, (ta & cv fwd parlel) drop through with old.
c
        if (sec.gt..01) then
          vr(1) = xr/sec
          vr(2) = yr/sec
          vr(3) = zr/sec
        endif

        pt(1) = pt(1)*fct + xu + dst*vr(1)
        pt(2) = pt(2)*fct + yu + dst*vr(2)
        pt(3) = pt(3)*fct + zu + dst*vr(3)
        sec = dsqrt(ve(1)**2+ve(2)**2+ve(3)**2)
        if (sec.eq.0.0d0) sec = 1.0d0
        pt(7) = ve(1)/sec
        pt(8) = ve(2)/sec
        pt(9) = ve(3)/sec

        ds(1) = pt(19)
        ds(2) = pt(20)
        ds(3) = pt(21)
        call outmot (buf, pt, ds, lnk, jptk, iclass, isubcl)

        if (i.eq.n) ifl(130) = 0
cc        if (ifl(42).eq.0) then
cc          if(ifl(104).eq.0)then
cc             call plotm(pt,LFLSE)
cc          else
cc             call modfy (pmod, pt)
cc             call plotm (pmod, LFLSE)
cc          endif
cc        endif
        if(ifl(246).gt.0 .and. (lv92 .or. lcmm)) call savmot(pt,ve)
100   continue
c
c...   Deallocate curve point & vector list.
c
      call cptend
c
c...   Output any buffered points to the CL file.
c
      if (ntk.gt.0) then
        n = ntk/mupt
        if (ifl(42) .eq. 0) then
            call putcl5 (iclass,isubcl,n,buf)
        else
            call putcl5 (iclass+100,isubcl,n,buf)
        endif
      endif
c
c...   DIsplay last point if PRINT/SMALL.
c
      if (ifl(154).eq.0) then
        if (ifl(104).eq.0) then 
          call tdsply(lnk,pt,jptk)
        else
          call tdsply(lnk,pmod,jptk)
        endif
      endif

C      if (ifl(246).gt.0.and.ifl(250).gt.0) call strmot
c
c...   Save last point & tool vector.
c
      sc(1) = pt(1)
      sc(2) = pt(2)
      sc(3) = pt(3)

      sc(7) = ve(1)
      sc(8) = ve(2)
      sc(9) = ve(3)
c
c...   Reset NOPS flag.
c
      ifl(276) = 0

999   return
C                                  Invalid drive surface
9028  ifl(2) = 28
      return
      end
